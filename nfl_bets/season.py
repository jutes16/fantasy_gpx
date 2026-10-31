"""
ELWAY's season-long numbers vs Kalshi's season markets -- a paper test, separate
from the weekly game bets.

    python3 season.py sheet 8            pull ELWAY's season feeds and Kalshi's season
                                         markets, print the sheet and log each team's
                                         best bet as a new batch (refused if ELWAY
                                         hasn't updated since the last batch)
    python3 season.py sheet 8 --no-log   ...print only
    python3 season.py grade              after the regular season (wins) and once the
                                         playoff field is set (playoffs)
    python3 season.py report             record and P&L by batch and market

Options for sheet: --min-ev 0.10 (time-adjusted EV to recommend a bet),
--rate 0.035 (yearly rate the money could earn elsewhere; see below).

ELWAY (Silver Bulletin's embed data, public feed; saved to the git-ignored data/elway/):
    elway-win-totals     each team's simulated season win distribution (0-17)
    elway-conditional    each team's playoff odds now, with a win and with a loss
                         this week
Kalshi (public prices, no key):
    KXNFLWINS            "<team> wins at least N games" (regular season)
    KXNFLPLAYOFF         "<team> makes the playoffs"

Every market is priced both ways, at what you could execute: YES at the ask, NO at
1 - bid, each plus Kalshi's fee. EV is per $1 staked. Season bets tie money up until
the market settles, so EV is also discounted to today at --rate a year:
    ev_adj = (1 + ev) / (1 + rate) ** years_to_settle - 1
(i.e. the bet has to beat leaving the money in an account earning `rate`).

One bet per team: the same opinion ("LAR is worse than the market thinks") shows up
in every rung of its wins ladder and in its playoff market, so only the team's best
side is logged. Only prices 20-80c are considered: a half-win disagreement turns a
5c long shot into a huge but unreliable EV. Flat 1-unit stakes.
"""

import io
import json
import os
import re
import sys
import time
import urllib.request
from datetime import datetime, timezone

import numpy as np
import pandas as pd

from common import DATA, SEASON, canon, kalshi_fee, nflverse
import kalshi

LOG = os.path.join(DATA, "season_log.csv")
FEED = "https://silver-bulletin.github.io/elway-embed-data/"
SERIES = {"wins": "KXNFLWINS", "playoffs": "KXNFLPLAYOFF"}
SETTLE = {"wins": "2027-01-18", "playoffs": "2027-01-11"}   # Kalshi's expected expiration
MIN_EV, RATE = 0.10, 0.035
PRICE_RANGE = (0.20, 0.80)


# ------------------------------------------------------------------ ELWAY's feed
def _fetch(url):
    req = urllib.request.Request(url, headers={"User-Agent": "fantasy_gpx-nfl_bets"})
    with urllib.request.urlopen(req, timeout=30) as r:
        return r.read().decode()


def elway_feed(week, season=SEASON, save=True):
    """(win distributions, conditional playoff odds, updated_at, versions) from ELWAY's latest feed."""
    man = json.loads(_fetch(FEED + "data/latest/manifest.json?_=" + str(int(time.time()))))
    files = man["files"]
    get = lambda k: _fetch(FEED + files[k]["path"] + "?release=" + files[k]["sha256"][:16])
    raw = {k: get(k) for k in ("elway-win-totals", "elway-win-totals-meta", "elway-conditional", "elway-conditional-meta")}
    meta = {k: dict(l.split(",", 1) for l in raw[k].strip().splitlines()[1:]) for k in raw if k.endswith("meta")}
    if save:
        d = os.path.join(DATA, "elway")
        os.makedirs(d, exist_ok=True)
        for k, text in raw.items():
            open(os.path.join(d, f"season_{season}_wk{week:02d}_{k}.csv"), "w").write(text)
    wins = pd.read_csv(io.StringIO(raw["elway-win-totals"]))
    wins["team"] = wins.home_sb_fran_id.map(canon)
    cond = pd.read_csv(io.StringIO(raw["elway-conditional"]), na_values=["."])
    cond["team"] = cond.nickname.map(dict(zip(wins.nickname, wins.team)))
    updated = meta["elway-win-totals-meta"]["updated_at"]
    version = meta["elway-win-totals-meta"]["data_version"] + "|" + meta["elway-conditional-meta"]["data_version"]
    return wins, cond, updated, version


def win_dist(wins):
    """{team: P(season wins = k), k = 0..17} from ELWAY's win-totals feed."""
    out = {}
    for team, g in wins.groupby("team"):
        p = np.zeros(18)
        p[g.season_total.astype(int)] = g.wins_pct.astype(float) / 100
        out[team] = p / p.sum()
    return out


# ------------------------------------------------------------------ Kalshi
def season_markets():
    rows = []
    for kind, series in SERIES.items():
        for m in kalshi.markets(series):
            t = m["ticker"]
            if kind == "wins":
                x = re.match(r"KXNFLWINS-\d+([A-Z]+)-(\d+)$", t)
                if not x:
                    continue
                team, n = canon(x[1]), int(x[2])
            else:
                team, n = canon(t.rsplit("-", 1)[1]), None
            rows.append(dict(kind=kind, ticker=t, team=team, n=n, bid=kalshi._f(m.get("yes_bid_dollars")),
                             ask=kalshi._f(m.get("yes_ask_dollars")), volume=kalshi._f(m.get("volume_fp"))))
    return pd.DataFrame(rows)


# ------------------------------------------------------------------ pricing
def years_to(kind, now):
    return max((pd.Timestamp(SETTLE[kind], tz="UTC") - now).days, 0) / 365.25


def price_sides(mk, dist, cond, now, rate=RATE):
    """Both sides of every market: ELWAY's probability, all-in cost, EV and time-adjusted EV."""
    po = dict(zip(cond.team, cond.made_playoffs_unconditional / 100))
    rows = []
    for x in mk.itertuples():
        if x.team not in dist:
            continue
        p_yes = dist[x.team][int(x.n):].sum() if x.kind == "wins" else po.get(x.team, np.nan)
        what = f"{int(x.n)}+ wins" if x.kind == "wins" else "makes the playoffs"
        t = years_to(x.kind, now)
        for side, price, p, label in (("yes", x.ask, p_yes, f"{x.team} {what}"),
                                      ("no", 1 - x.bid, 1 - p_yes, f"{x.team} not {what}" if x.kind == "wins" else f"{x.team} misses the playoffs")):
            if not (0 < price < 1) or np.isnan(p):
                continue
            cost = price + kalshi_fee(price)
            ev = p / cost - 1
            rows.append(dict(kind=x.kind, ticker=x.ticker, team=x.team, n=x.n, side=side, bet=label,
                             price=round(price, 2), cost=round(cost, 4), p=round(p, 4),
                             mid=round((x.bid + x.ask) / 2 if side == "yes" else 1 - (x.bid + x.ask) / 2, 3),
                             ev=round(ev, 4), years=round(t, 3), ev_adj=round((1 + ev) / (1 + rate) ** t - 1, 4),
                             volume=x.volume))
    return pd.DataFrame(rows)


def best_per_team(sides, min_ev=MIN_EV):
    ok = sides[sides.price.between(*PRICE_RANGE)]
    best = ok.sort_values("ev_adj", ascending=False).groupby("team").head(1).copy()
    best["recommended"] = best.ev_adj >= min_ev
    return best.sort_values("ev_adj", ascending=False)


def expected_wins(dist, mk):
    """ELWAY's expected wins vs the wins ladder's implied expectation (sum of P(W >= n), mids)."""
    rows = []
    for team, p in dist.items():
        lad = mk[(mk.kind == "wins") & (mk.team == team)].set_index("n")
        mid = ((lad.bid + lad.ask) / 2).to_dict()
        surv, last = [], 1.0
        for n in range(1, 18):
            if n in mid:
                last = min(last, mid[n])
            elif mid and n > max(mid):
                last = 0.0
            surv.append(last)
        rows.append(dict(team=team, elway_wins=round(float((p * np.arange(18)).sum()), 1),
                         kalshi_wins=round(sum(surv), 1) if mid else None))
    return pd.DataFrame(rows)


# ------------------------------------------------------------------ sheet / log
def sheet(week, log=True, min_ev=MIN_EV, rate=RATE, season=SEASON):
    wins, cond, updated, version = elway_feed(week, season)
    dist = win_dist(wins)
    mk = season_markets()
    now = pd.Timestamp.now(tz="UTC")
    sides = price_sides(mk, dist, cond, now, rate)
    best = best_per_team(sides, min_ev)
    ew = expected_wins(dist, mk).set_index("team")
    po = cond.set_index("team")

    print(f"ELWAY season feed updated {updated}; Kalshi prices {now:%Y-%m-%d %H:%M} UTC; "
          f"{len(mk)} markets, money discounted at {rate:.1%} a year")
    print(f"\n{'team':5}{'ELWAY wins':>11}{'Kalshi':>8}{'ELWAY po':>10}{'Kalshi':>8}   best bet (20-80c){'':>12}{'price':>6}{'ELWAY':>7}{'EV':>7}{'EV adj':>8}")
    for r in best.itertuples():
        kp = mk[(mk.kind == "playoffs") & (mk.team == r.team)]
        kpo = f"{(kp.bid.iloc[0] + kp.ask.iloc[0]) / 2:.0%}" if len(kp) else "—"
        print(f"{'*' if r.recommended else ' '}{r.team:4}{ew.elway_wins[r.team]:>11.1f}{ew.kalshi_wins[r.team] or float('nan'):>8.1f}"
              f"{po.made_playoffs_unconditional.get(r.team, float('nan')) / 100:>10.0%}{kpo:>8}   {r.bet:28}{r.price * 100:>5.0f}c{r.p:>7.0%}{r.ev:>+7.0%}{r.ev_adj:>+8.0%}")
    print(f"\n* = recommended (time-adjusted EV >= {min_ev:.0%}): {int(best.recommended.sum())} bets, 1 unit each")

    if not log:
        return best
    old = pd.read_csv(LOG) if os.path.exists(LOG) else pd.DataFrame()
    if len(old) and (old.elway_version == version).any():
        print(f"not logged: ELWAY's season feed hasn't changed since batch {int(old[old.elway_version == version].batch.max())}")
        return best
    batch = int(old.batch.max()) + 1 if len(old) else 1
    sw = po[["made_playoffs_loss", "made_playoffs_unconditional", "made_playoffs_win"]]
    out = best.assign(season=season, week=week, batch=batch, logged_at=now.strftime("%Y-%m-%dT%H:%M:%SZ"),
                      elway_updated=updated, elway_version=version, rate=rate,
                      elway_wins=best.team.map(ew.elway_wins), kalshi_wins=best.team.map(ew.kalshi_wins),
                      po_loss=best.team.map(sw.made_playoffs_loss), po_now=best.team.map(sw.made_playoffs_unconditional),
                      po_win=best.team.map(sw.made_playoffs_win),
                      stake=1.0, result=None, pnl=None)
    cols = ["season", "week", "batch", "logged_at", "kind", "team", "n", "side", "bet", "ticker", "price", "cost", "mid", "p",
            "ev", "years", "rate", "ev_adj", "recommended", "volume", "elway_wins", "kalshi_wins", "po_loss", "po_now", "po_win",
            "elway_updated", "elway_version", "stake", "result", "pnl"]
    pd.concat([old, out[cols]], ignore_index=True).to_csv(LOG, index=False)
    print(f"logged batch {batch}: {len(out)} teams' best bets ({int(out.recommended.sum())} recommended) -> {os.path.relpath(LOG)}")
    return best


# ------------------------------------------------------------------ grading
def final_wins(season=SEASON):
    """{team: regular-season wins}, only for teams whose 17 games are all final."""
    g = nflverse()
    g = g[(g.season == season) & (g.game_type == "REG")].copy()
    g["away"], g["home"] = g.away_team.map(canon), g.home_team.map(canon)
    out = {}
    for t in set(g.away) | set(g.home):
        mine = g[(g.away == t) | (g.home == t)]
        if len(mine) != 17 or mine.result.isna().any():
            continue
        out[t] = int(((mine.home == t) & (mine.result > 0)).sum() + ((mine.away == t) & (mine.result < 0)).sum())
    return out


def playoff_field(season=SEASON):
    """The 14 playoff teams once the wild-card and divisional games are on the schedule, else None."""
    g = nflverse()
    g = g[(g.season == season) & g.game_type.isin(["WC", "DIV"])]
    teams = set(g.away_team.map(canon)) | set(g.home_team.map(canon))
    return teams if len(teams) == 14 else None


def grade(season=SEASON):
    log = pd.read_csv(LOG)
    log["result"] = log.result.astype(object)               # all blank until the first grade
    wins, field = final_wins(season), playoff_field(season)
    n = 0
    for i, x in log.iterrows():
        if isinstance(x.result, str) and x.result in ("W", "L"):
            continue
        if x.kind == "wins" and x.team in wins:
            yes = wins[x.team] >= int(x.n)
        elif x.kind == "playoffs" and field is not None:
            yes = x.team in field
        else:
            continue
        won = yes == (x.side == "yes")
        log.loc[i, "result"] = "W" if won else "L"
        log.loc[i, "pnl"] = round(x.stake * (1 / x.cost - 1) if won else -x.stake, 4)
        n += 1
    log.to_csv(LOG, index=False)
    print(f"graded {n} bets; {int(log.result.isin(['W', 'L']).sum())} of {len(log)} settled")


def report():
    log = pd.read_csv(LOG)
    log["recommended"] = log.recommended.astype(str).str.lower() == "true"
    for name, d in (("recommended", log[log.recommended]), ("every team's best side", log)):
        done = d[d.result.isin(["W", "L"])]
        print(f"\n{name}: {len(d)} bets logged, {len(done)} settled")
        g = d.groupby(["batch", "week"]).agg(bets=("team", "size"), avg_ev=("ev", "mean"), avg_ev_adj=("ev_adj", "mean"),
                                             expected=("ev", "sum"), wins=("result", lambda r: (r == "W").sum()),
                                             losses=("result", lambda r: (r == "L").sum()), pnl=("pnl", "sum"))
        print(g.round(3).to_string())
        if len(done):
            print(f"settled: {(done.result == 'W').sum()}-{(done.result == 'L').sum()}, {done.pnl.sum():+.2f}u "
                  f"(ELWAY expected {done.ev.sum():+.2f}u)")


if __name__ == "__main__":
    a = sys.argv[1:]
    opt = lambda k, d: float(a[a.index(k) + 1]) if k in a else d
    if a and a[0] == "sheet":
        sheet(int(a[1]), log="--no-log" not in a, min_ev=opt("--min-ev", MIN_EV), rate=opt("--rate", RATE))
    elif a and a[0] == "grade":
        grade()
    elif a and a[0] == "report":
        report()
    else:
        print(__doc__)
