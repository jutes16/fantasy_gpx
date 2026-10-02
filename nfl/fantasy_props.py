"""
Betting-market fantasy projections vs Sleeper's.

    python3 fantasy_props.py 4                 # fetch week-4 props (~6 credits/game), compare
    python3 fantasy_props.py 4 --cached        # re-use saved props, no credits
    python3 fantasy_props.py 4 --ppr 1         # full PPR (default 0.5)
    python3 fantasy_props.py 4 --games PHI,BUF # only these home teams' games

Sportsbook player props (The Odds API, via ../nfl_bets/odds_api.py; needs
ODDS_API_KEY) are turned into an implied stat projection per player, then into
fantasy points, and compared with Sleeper's projection on the SAME stats:

  prop                 -> implied mean
  pass / rush / rec yds  line ~ median, nudged by the de-vigged over price;
                         mean = median x (mean/median measured from game logs:
                         pass 0.99, rush 1.05 (QB rush 1.18), rec 1.10)
  receptions, pass TDs   Poisson mean matching P(over the line)
  anytime TD             P(scores) -> expected TDs = -ln(1 - P)

Only stats the market prices are compared (e.g. no QB interceptions), so the
"covered points" columns are apples to apples. Big gaps are where the betting
market and Sleeper disagree -- start/sit and DFS leads, not certainties: the
market's lines are sharper on average, but player props carry more vig and
less liquidity than game lines.

Credits: 1 per market per game (6 markets x ~15 games = ~90 a week of the
500 free per month). Raw responses are saved under data/props/ and re-used.
"""

import json
import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd
import requests
from scipy import optimize, stats

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "nfl_bets"))
import odds_api                        # noqa: E402  (shared client; reads ODDS_API_KEY)
from common import SEASON, schedule   # noqa: E402
from weekly_stats import _normalize_name   # noqa: E402

PROPS_DIR = os.path.join(HERE, "data", "props")
MARKETS = ["player_pass_yds", "player_pass_tds", "player_rush_yds",
           "player_reception_yds", "player_receptions", "player_anytime_td"]
# Yardage shape, measured from nflverse weekly game logs (2023-24, players with
# 8+ games and a starter's volume): game-to-game spread (sd / mean) and how far
# the mean sits above the median (a prop line is a median; Sleeper projects means)
YARD_FIT = {                    # market: (cv, mean / median)
    "player_pass_yds": (0.33, 0.99),
    "player_rush_yds": (0.53, 1.05),
    "player_reception_yds": (0.62, 1.10),
}
QB_RUSH_FIT = (0.83, 1.18)      # QB rushing is far burstier (scrambles): measured separately
STAT = {"player_pass_yds": "pass_yd", "player_pass_tds": "pass_td", "player_rush_yds": "rush_yd",
        "player_reception_yds": "rec_yd", "player_receptions": "rec", "player_anytime_td": "td"}
YES_ONLY_VIG = 0.06                    # overround assumed when a book posts only "Yes"


# ------------------------------------------------------------------ fetch
def fetch_props(week, season=SEASON, homes=None, cached=False):
    """Props for the week's games that haven't kicked off; saved per game."""
    d = os.path.join(PROPS_DIR, f"{season}_wk{week:02d}")
    os.makedirs(d, exist_ok=True)
    sched = schedule(season, week)
    games = {(r.away, r.home): r.kickoff for r in sched.itertuples()}
    if cached:
        return [json.load(open(os.path.join(d, f))) for f in sorted(os.listdir(d)) if f.endswith(".json")]
    events, _, left = odds_api.get(f"/sports/{odds_api.SPORT}/events")
    now = datetime.now(timezone.utc)
    out, used = [], 0
    for e in events:
        away, home = odds_api._code(e["away_team"]), odds_api._code(e["home_team"])
        if (away, home) not in games or (homes and home not in homes):
            continue
        path = os.path.join(d, f"{away}_{home}.json")
        if os.path.exists(path):                       # already have it: free
            out.append(json.load(open(path)))
            continue
        if pd.Timestamp(e["commence_time"]) <= now:
            continue                                   # no pre-game props once started
        data, last, left = odds_api.get(f"/sports/{odds_api.SPORT}/events/{e['id']}/odds",
                                        dict(regions="us", markets=",".join(MARKETS),
                                             oddsFormat="american"))
        data["_fetched_at"] = now.isoformat(timespec="seconds")
        json.dump(data, open(path, "w"))
        used += int(last or 0)
        out.append(data)
    print(f"props: {len(out)} games ({used} credits used now, {left} left this month)")
    return out


# ------------------------------------------------------------------ market -> stats
def _p(price):
    return 1 / odds_api_decimal(price)


def odds_api_decimal(a):
    return 1 + (a / 100 if a > 0 else 100 / -a)


def consensus(events):
    """Per player x market: median line and median de-vigged P(over / yes)."""
    rows = []
    for ev in events:
        home, away = odds_api._code(ev["home_team"]), odds_api._code(ev["away_team"])
        for bk in ev.get("bookmakers", []):
            for mk in bk.get("markets", []):
                by_player = {}
                for o in mk["outcomes"]:
                    by_player.setdefault((o.get("description"), o.get("point")), {})[o["name"]] = o["price"]
                for (player, point), px in by_player.items():
                    if mk["key"] == "player_anytime_td":
                        if "Yes" not in px:
                            continue
                        py = _p(px["Yes"])
                        p = py / (py + _p(px["No"])) if "No" in px else py / (1 + YES_ONLY_VIG)
                    else:
                        if "Over" not in px or "Under" not in px or point is None:
                            continue
                        po, pu = _p(px["Over"]), _p(px["Under"])
                        p = po / (po + pu)
                    rows.append(dict(player=player, game=f"{away} @ {home}", market=mk["key"],
                                     book=bk["title"], line=point, p=p))
    d = pd.DataFrame(rows)
    if d.empty:
        return d
    return (d.groupby(["player", "game", "market"])
             .agg(line=("line", "median"), p=("p", "median"), books=("book", "nunique"))
             .reset_index())


def implied_mean(market, line, p, qb=False):
    p = float(np.clip(p, 0.02, 0.98))
    if market == "player_anytime_td":
        return -np.log(1 - p)                          # Poisson: P(>=1) = 1 - e^-lambda
    if market in ("player_receptions", "player_pass_tds"):
        k = np.floor(line)                             # over x.5 = at least k+1
        f = lambda lam: (1 - stats.poisson.cdf(k, lam)) - p
        return optimize.brentq(f, 1e-4, 60)
    cv, ratio = QB_RUSH_FIT if (qb and market == "player_rush_yds") else YARD_FIT[market]
    # the market's median: the line, nudged by how lopsided the de-vigged price is
    median = line + cv * line * stats.norm.ppf(p)
    return median * ratio                              # measured mean / median


# ------------------------------------------------------------------ sleeper
def sleeper_projections(week, season=SEASON):
    url = (f"https://api.sleeper.app/projections/nfl/{season}/{week}?season_type=regular"
           "&position%5B%5D=QB&position%5B%5D=RB&position%5B%5D=WR&position%5B%5D=TE")
    r = requests.get(url, timeout=30)
    r.raise_for_status()
    rows = []
    for x in r.json():
        p, s = x.get("player") or {}, x.get("stats") or {}
        name = f"{p.get('first_name', '')} {p.get('last_name', '')}".strip()
        rows.append(dict(name=name, key=_normalize_name(name), pos=p.get("position"),
                         team=x.get("team"), injury=p.get("injury_status") or "", **{k: s.get(k) or 0.0 for k in
                         ("pass_yd", "pass_td", "pass_int", "rush_yd", "rush_td",
                          "rec", "rec_yd", "rec_td", "pts_half_ppr", "pts_ppr", "pts_std")}))
    return pd.DataFrame(rows)


# ------------------------------------------------------------------ compare
def compare(week, ppr=0.5, homes=None, cached=False, season=SEASON):
    cons = consensus(fetch_props(week, season, homes, cached))
    if cons.empty:
        raise SystemExit("no props found -- check ODDS_API_KEY or try --cached")
    sl = sleeper_projections(week, season)
    qbs = set(sl.key[sl.pos == "QB"])
    cons["mean"] = [implied_mean(m, l, p, qb=_normalize_name(n) in qbs)
                    for m, l, p, n in zip(cons.market, cons.line, cons.p, cons.player)]
    cons["stat"] = cons.market.map(STAT)
    mk = cons.pivot_table(index=["player", "game"], columns="stat", values="mean").reset_index()
    mk["key"] = mk.player.map(_normalize_name)
    sl["td"] = sl.rush_td + sl.rec_td
    m = mk.merge(sl, on="key", how="inner", suffixes=("_mkt", "_slp"))

    pts = {"pass_yd": 0.04, "pass_td": 4, "rush_yd": 0.1, "rec_yd": 0.1, "rec": ppr, "td": 6}
    m["covered"] = ""
    m["pts_mkt"] = 0.0
    m["pts_slp"] = 0.0
    for stat, w in pts.items():
        col = f"{stat}_mkt" if f"{stat}_mkt" in m else stat
        if col not in m:
            continue
        have = m[col].notna()
        m.loc[have, "pts_mkt"] += m.loc[have, col] * w
        m.loc[have, "pts_slp"] += m.loc[have, f"{stat}_slp" if f"{stat}_slp" in m else stat] * w
        m.loc[have, "covered"] += stat + " "
    m["diff"] = m.pts_mkt - m.pts_slp
    m["slp_total"] = m.pts_ppr * ppr + m.pts_std * (1 - ppr)    # Sleeper full projection
    return m, cons


def main():
    a = sys.argv[1:]
    if not a:
        raise SystemExit(__doc__)
    week = int(a[0])
    ppr = float(a[a.index("--ppr") + 1]) if "--ppr" in a else 0.5
    homes = set(a[a.index("--games") + 1].upper().split(",")) if "--games" in a else None
    m, cons = compare(week, ppr, homes, cached="--cached" in a)

    out = os.path.join(PROPS_DIR, f"{SEASON}_wk{week:02d}_vs_sleeper.csv")
    m.to_csv(out, index=False)
    label = {0: "standard", 0.5: "half-PPR", 1: "PPR"}.get(ppr, f"{ppr} PPR")
    print("=" * 96)
    print(f"BETTING MARKET vs SLEEPER  week {week}, {label}  ({len(m)} players matched; "
          f"points on the stats the market prices)")
    print("=" * 96)
    stat_cols = [("pass_yd", "PaYd"), ("pass_td", "PaTD"), ("rush_yd", "RuYd"),
                 ("rec", "Rec"), ("rec_yd", "ReYd"), ("td", "TD")]
    for pos in ("QB", "RB", "WR", "TE"):
        d = m[(m.pos == pos)].copy()
        if d.empty:
            continue
        d = d.reindex(d["diff"].abs().sort_values(ascending=False).index).head(8)
        print(f"\n{pos}   (biggest disagreements; + = market higher than Sleeper)")
        hdr = f"  {'player':<22}{'team':<5}{'inj':<5}{'market':>8}{'Sleeper':>8}{'diff':>7}   " + \
              "  ".join(f"{h:>11}" for _, h in stat_cols)
        print(hdr)
        for r in d.itertuples():
            cells = []
            for s, _ in stat_cols:
                mv = getattr(r, f"{s}_mkt", np.nan) if f"{s}_mkt" in m else getattr(r, s, np.nan)
                sv = getattr(r, f"{s}_slp", np.nan) if f"{s}_slp" in m else np.nan
                cells.append("-".rjust(11) if pd.isna(mv) else f"{mv:.1f}/{sv:.1f}".rjust(11))
            inj = {"Out": "OUT", "Questionable": "Q", "Doubtful": "D", "IR": "IR"}.get(r.injury, r.injury[:3])
            print(f"  {r.name:<22}{str(r.team):<5}{inj:<5}{r.pts_mkt:>8.1f}{r.pts_slp:>8.1f}{r.diff:>+7.1f}   "
                  + "  ".join(cells))
    print(f"\n  stat cells: market / Sleeper.  Saved: {os.path.relpath(out)}")


if __name__ == "__main__":
    main()
