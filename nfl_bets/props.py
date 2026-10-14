"""
Player props: who goes over or under, priced against the market, logged
before kickoff and graded afterwards. A paper test, like bets.py: nothing is bet.

    python3 props.py sheet 5              # props + Sleeper projections -> sheet, logged
    python3 props.py sheet 5 --cached     # re-use saved props (no credits)
    python3 props.py sheet 5 --no-log     # just print
    python3 props.py grade 4              # results from Sleeper's box scores
    python3 props.py report
    python3 props.py dashboard            # the dashboard's data -> data/dashboard/*.json

Props come from The Odds API through ../nfl_fantasy/fantasy_props.py (same
saved files, so a week fetched there costs nothing here). For every player x
market, each book's line and de-vigged price is turned into an implied mean
(fantasy_props.implied_mean), and the market's fair mean is the median across
books. Two signals, each evaluated at the best offer across books:

  SHOP  the best offer is worth >= SHOP_EV against the OTHER books' fair price
        (an outlier line or a stale price; the classic line-shopping edge)
  PROJ  Sleeper's projection gives the side >= PROJ_EDGE more probability than
        the market's fair price (does Sleeper know something the market doesn't?)

Probabilities use the same shapes as the implied means (../nfl_fantasy/shapes.py,
measured from game logs by prop_shapes.py), so a Sleeper mean equal to the
market's gives exactly the market's probability:
  yards: the measured game / expected distribution for the line's volume band
  receptions, passing TDs: counts with the measured dispersion;
  anytime TD: P = 1 - exp(-expected TDs)

Every side in scope is logged (data/prop_log.csv), not just the flagged ones,
so the report can show how the signals do against the rest. In scope: prices
from -250 to +250, yardage lines of 15+ (150+ passing), and no SHOP on anytime
TD (no book quotes "No", so its fair price rests on an assumed vig). Rules match the
books': a player who doesn't play is void; a whole-number line can push.
"""

import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd
import requests

from common import DATA, SEASON, american_to_decimal

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "nfl_fantasy"))
import fantasy_props as fp                  # noqa: E402
import shapes                               # noqa: E402
import history                              # noqa: E402
from weekly_stats import _normalize_name    # noqa: E402

LOG = os.path.join(DATA, "prop_log.csv")
SHOP_EV = 0.03          # best offer beats the other books' fair price by 3%+
PROJ_EDGE = 0.08        # Sleeper's P(side) is 8+ points above the market's fair P
MIN_BOOKS = 3           # books quoting the player x market (SHOP needs 2+ others)
# Scope: only lines and prices the probability shapes are good for. The yardage
# shapes were measured on starters' volume, so tiny lines (a QB's 0.5 rushing
# yards, a backup's 6.5 receiving) are out; long shots are out because nobody
# can de-vig them; anytime TD is "Yes" only at every book, so its fair price
# rests on an assumed vig: PROJ only, never SHOP.
MIN_LINE = {"player_pass_yds": 150, "player_rush_yds": 15, "player_reception_yds": 15}
MAX_PRICE, MIN_PRICE = 250, -250
POISSON = {"player_receptions", "player_pass_tds"}
SLEEPER_STAT = {"player_pass_yds": "pass_yd", "player_pass_tds": "pass_td",
                "player_rush_yds": "rush_yd", "player_reception_yds": "rec_yd",
                "player_receptions": "rec", "player_anytime_td": "td"}
SHORT = {"player_pass_yds": "pass yds", "player_pass_tds": "pass TDs",
         "player_rush_yds": "rush yds", "player_reception_yds": "rec yds",
         "player_receptions": "receptions", "player_anytime_td": "anytime TD"}


# ------------------------------------------------------------------ probabilities
def p_side(market, mean, line, side, qb=False):
    """(P(win), P(push)) for a side ("over"/"under"; "yes"/"no" for anytime TD)."""
    if market == "player_anytime_td" or market in shapes.YARD_MARKETS:
        over = shapes.p_over(market, mean, line, qb)       # no pushes: x.5 lines, or yes/no
        return (over if side in ("over", "yes") else 1 - over), 0.0
    phi = shapes.load()["counts"][market]
    k = np.floor(line)
    below = shapes.count_cdf(k - 1, mean, phi)            # P(X < k)
    at_or_below = shapes.count_cdf(k, mean, phi)
    if line == k:                                         # whole number: can push
        over, under, push = 1 - at_or_below, below, at_or_below - below
    else:
        over, under, push = 1 - at_or_below, at_or_below, 0.0
    return (over if side == "over" else under), push


def ev(p_win, p_push, price):
    """Expected profit per 1 unit staked at American odds."""
    return p_win * (american_to_decimal(price) - 1) - (1 - p_win - p_push)


# ------------------------------------------------------------------ offers
def offers(events):
    """One row per book x player x market x line: both prices and the de-vigged P."""
    rows = []
    for e in events:
        home, away = fp.odds_api._code(e["home_team"]), fp.odds_api._code(e["away_team"])
        for bk in e.get("bookmakers", []):
            for mk in bk.get("markets", []):
                if mk["key"] not in SLEEPER_STAT:
                    continue
                by = {}
                for o in mk["outcomes"]:
                    by.setdefault((o.get("description"), o.get("point")), {})[o["name"].lower()] = o["price"]
                for (player, line), px in by.items():
                    if mk["key"] == "player_anytime_td":
                        if "yes" not in px:
                            continue
                        py = 1 / american_to_decimal(px["yes"])
                        p = py / (py + 1 / american_to_decimal(px["no"])) if "no" in px \
                            else py / (1 + fp.YES_ONLY_VIG)
                    else:
                        if "over" not in px or "under" not in px or line is None:
                            continue
                        po, pu = (1 / american_to_decimal(px[s]) for s in ("over", "under"))
                        p = po / (po + pu)
                    rows.append(dict(player=player, away=away, home=home, kickoff=e["commence_time"],
                                     fetched_at=e.get("_fetched_at", ""), market=mk["key"],
                                     book=bk["title"], line=line if line is not None else 0.5,
                                     p=p, prices=px))
    return pd.DataFrame(rows)


SLP_COLS = ["key", "name", "pos", "team", "injury", "pass_yd", "pass_td", "rush_yd", "rec", "rec_yd", "td"]


def snapshot_path(week, season=SEASON):
    """Sleeper's projections as they were when this week's sheet was logged."""
    return os.path.join(fp.PROPS_DIR, f"{season}_wk{week:02d}_sleeper_at_log.csv")


def sleeper_means(week, season=SEASON):
    """Sleeper's current projected stats per player (key = normalized name)."""
    sl = fp.sleeper_projections(week, season)
    sl["td"] = sl.rush_td + sl.rec_td
    return sl[SLP_COLS].drop_duplicates("key")


def save_snapshot(slp, keys, week, season=SEASON):
    """Keep what Sleeper projects now for these players; earlier rows for others
    (players whose games have started) are left as they were."""
    path = snapshot_path(week, season)
    new = slp[slp.key.isin(keys)]
    if os.path.exists(path):
        old = pd.read_csv(path)
        new = pd.concat([old[~old.key.isin(new.key)], new], ignore_index=True)
    new.to_csv(path, index=False)


# ------------------------------------------------------------------ sheet
def build_sheet(off, slp):
    """Best offer per player x market x side, with fair and Sleeper probabilities."""
    off = off.copy()
    off["key"] = off.player.map(_normalize_name)
    slp = slp.set_index("key")
    qbs = set(slp.index[slp.pos == "QB"])
    off["qb"] = off.key.isin(qbs)
    off["mean"] = [fp.implied_mean(m, l, p, qb=q) for m, l, p, q in
                   zip(off.market, off.line, off.p, off.qb)]
    rows = []
    for (player, market), g in off.groupby(["player", "market"]):
        books = g.book.nunique()
        if books < MIN_BOOKS:
            continue
        r0 = g.iloc[0]
        fair = float(g["mean"].median())
        s = slp.loc[r0.key] if r0.key in slp.index else None
        smean = None if s is None else s[SLEEPER_STAT[market]]
        if smean is not None and (pd.isna(smean) or smean <= 0):
            smean = None                                   # Sleeper doesn't project this stat
        smean = None if smean is None else float(smean)
        best = {}
        for o in g.itertuples():
            others = g[g.book != o.book]["mean"]
            loo = float(others.median())                   # fair without this book
            for side, price in o.prices.items():
                pw, pp = p_side(market, loo, o.line, side, o.qb)
                e = ev(pw, pp, price)
                if side not in best or e > best[side]["ev_shop"]:
                    best[side] = dict(line=o.line, book=o.book, price=price, ev_shop=e)
        for side, b in best.items():
            if not (MIN_PRICE <= b["price"] <= MAX_PRICE) or b["line"] < MIN_LINE.get(market, 0):
                continue
            pf, ppf = p_side(market, fair, b["line"], side, r0.qb)
            row = dict(player=player, key=r0.key, game=f"{r0.away} @ {r0.home}", kickoff=r0.kickoff,
                       fetched_at=r0.fetched_at, market=market, side=side, books=books,
                       fair_mean=round(fair, 2), p_fair=round(pf, 4), **b,
                       pos=None if s is None else s.pos, team=None if s is None else s.team,
                       injury="" if s is None else s.injury,
                       slp_mean=None, p_slp=None, edge=None, ev_slp=None)
            if smean is not None:
                ps, pps = p_side(market, smean, b["line"], side, r0.qb)
                row.update(slp_mean=round(smean, 2), p_slp=round(ps, 4), edge=round(ps - pf, 4),
                           ev_slp=round(ev(ps, pps, b["price"]), 4))
            rows.append(row)
    d = pd.DataFrame(rows)
    if d.empty:
        return d
    d["ev_shop"] = d.ev_shop.round(4)
    d["shop"] = (d.ev_shop >= SHOP_EV) & (d.market != "player_anytime_td")
    d["proj"] = d.edge.fillna(0) >= PROJ_EDGE
    return d


def _fmt_price(a):
    return f"{int(a):+d}"


def print_sheet(d, week, top=15):
    def line(r):
        side = r.side.upper() if r.market != "player_anytime_td" else ("YES" if r.side == "yes" else "NO")
        ln = "" if r.market == "player_anytime_td" else f" {r.line:g}"
        slp = "      -" if pd.isna(r.p_slp) else f"{r.slp_mean:>7.1f}"
        ps = "    -" if pd.isna(r.p_slp) else f"{r.p_slp:>5.0%}"
        inj = {"Out": "OUT", "Questionable": "Q", "Doubtful": "D", "IR": "IR"}.get(r.injury, str(r.injury)[:3])
        return (f"  {r.player:<22}{str(r.team or ''):<5}{inj:<4}{SHORT[r.market]:<11}{side + ln:<11}"
                f"{_fmt_price(r.price):>6} {r.book:<13}{r.fair_mean:>7.1f}{r.p_fair:>6.0%}"
                f"{slp}{ps}{r.ev_shop:>+7.1%}{(r.ev_slp if pd.notna(r.ev_slp) else np.nan):>+7.1%}"
                f"{'' if pd.isna(r.hit_rate) else f'{r.hit_rate:.0%}':>7}"
                f"{'' if pd.isna(r.opp_factor) else f'{r.opp} {r.opp_factor:.2f}':>9}")
    hdr = (f"  {'player':<22}{'team':<5}{'inj':<4}{'market':<11}{'bet':<11}{'price':>6} {'book':<13}"
           f"{'mktavg':>7}{'P mkt':>6}{'Sleepr':>7}{'P slp':>5}{'EV mkt':>7}{'EV slp':>7}{'L8 hit':>7}{'opp':>9}")
    print("=" * 128)
    print(f"PLAYER PROPS  week {week}   {d.player.nunique()} players, {len(d)} priced sides, "
          f"best offer across books")
    print("=" * 128)
    print(f"\nPROJ: Sleeper's P(side) {PROJ_EDGE:.0%}+ above the market's fair P  (overs and unders)")
    print(hdr)
    for r in d[d.proj].sort_values("edge", ascending=False).head(top).itertuples():
        print(line(r))
    print(f"\nSHOP: best offer worth {SHOP_EV:.0%}+ against the other books' fair price")
    print(hdr)
    for r in d[d.shop].sort_values("ev_shop", ascending=False).head(top).itertuples():
        print(line(r))
    print(f"\n  {int(d.proj.sum())} PROJ and {int(d.shop.sum())} SHOP flags "
          f"({int((d.proj & d.shop).sum())} both).  mktavg / Sleepr: projected stat; "
          "EV per 1 unit at the price shown.")
    print("  L8 hit: share of his last 8 games that would have won this bet at this line. opp: the")
    print("  opponent's allowed / league average for his position and stat (>1 = soft for overs).")
    if d.injury.isin(["Out", "Doubtful", "IR"]).any():
        print("  Check injuries: a player who doesn't play is void, but the line may already be stale.")


# ------------------------------------------------------------------ log
COLS = ["week", "logged_at", "player", "key", "pos", "team", "game", "kickoff", "market", "side",
        "line", "book", "price", "books", "fair_mean", "p_fair", "slp_mean", "p_slp", "edge",
        "ev_shop", "ev_slp", "shop", "proj", "injury", "hist_n", "hist_avg", "l8_avg", "hit_rate",
        "opp", "opp_factor", "actual", "result", "units"]


def write_log(d, week):
    """Replace this week's rows for these games (latest pre-kickoff view wins)."""
    d = d.copy()
    d["week"] = week
    d["logged_at"] = d.fetched_at.where(d.fetched_at.astype(bool),
                                        datetime.now(timezone.utc).isoformat(timespec="seconds"))
    for c in ("actual", "result", "units"):
        d[c] = None
    old = pd.read_csv(LOG) if os.path.exists(LOG) else pd.DataFrame(columns=COLS)
    keep = ~((old.week == week) & old.game.isin(d.game))
    out = pd.concat([old[keep], d[COLS]], ignore_index=True)
    os.makedirs(DATA, exist_ok=True)
    out.to_csv(LOG, index=False)
    print(f"\nlogged {len(d)} sides for week {week} -> {os.path.relpath(LOG)}")


def sheet(week, cached=False, log=True, season=SEASON):
    events = fp.fetch_props(week, season, cached=cached)
    now = pd.Timestamp.now(tz="UTC")
    if not cached:
        events = [e for e in events if pd.Timestamp(e["commence_time"]) > now]
    off = offers(events)
    if off.empty:
        raise SystemExit("no props -- check ODDS_API_KEY, or fetch with fantasy_props.py first")
    snap = snapshot_path(week, season)
    if cached and pd.Timestamp(off.kickoff.min()) < now:
        # rebuilding a week that has started: use Sleeper as it was when logged, never today's
        if not os.path.exists(snap):
            raise SystemExit(f"no Sleeper snapshot from before kickoff ({os.path.relpath(snap)})")
        slp = pd.read_csv(snap)
        slp["injury"] = slp.injury.fillna("")
        print(f"Sleeper projections as logged before kickoff ({os.path.relpath(snap)})")
    else:
        slp = sleeper_means(week, season)
        if log:
            save_snapshot(slp, set(off.player.map(_normalize_name)), week, season)
    d = build_sheet(off, slp)
    d = history.features(d, season, week)      # player history and matchup, games before this week only
    print_sheet(d, week)
    if log:
        write_log(d, week)
    return d


# ------------------------------------------------------------------ grade
def box_scores(week, season=SEASON):
    url = (f"https://api.sleeper.app/stats/nfl/{season}/{week}?season_type=regular"
           + "".join(f"&position%5B%5D={p}" for p in ("QB", "RB", "WR", "TE")))
    r = requests.get(url, timeout=60)
    r.raise_for_status()
    rows = []
    for x in r.json():
        p, s = x.get("player") or {}, x.get("stats") or {}
        name = f"{p.get('first_name', '')} {p.get('last_name', '')}".strip()
        played = (s.get("gp") or 0) > 0 or (s.get("off_snp") or 0) > 0
        rows.append(dict(key=_normalize_name(name), team=x.get("team"), played=played,
                         pass_yd=s.get("pass_yd") or 0, pass_td=s.get("pass_td") or 0,
                         rush_yd=s.get("rush_yd") or 0, rec=s.get("rec") or 0,
                         rec_yd=s.get("rec_yd") or 0,
                         td=(s.get("rush_td") or 0) + (s.get("rec_td") or 0)))
    return pd.DataFrame(rows)


def settle(market, side, line, actual):
    if market == "player_anytime_td":
        won = actual >= 1
        return "win" if won == (side == "yes") else "loss"
    if actual == line:
        return "push"
    return "win" if (actual > line) == (side == "over") else "loss"


def grade(week, season=SEASON):
    if not os.path.exists(LOG):
        raise SystemExit("no prop log yet")
    log = pd.read_csv(LOG)
    log["result"] = log.result.astype(object)
    wk = log.week == week
    if not wk.any():
        raise SystemExit(f"nothing logged for week {week}")
    box = box_scores(week, season)
    teams = box.set_index(["key", "team"])
    by_key = box.drop_duplicates("key", keep=False).set_index("key")   # unambiguous names
    for i in log.index[wk]:
        r = log.loc[i]
        b = teams.loc[(r.key, r.team)] if (r.key, r.team) in teams.index else \
            (by_key.loc[r.key] if r.key in by_key.index else None)
        if isinstance(b, pd.DataFrame):
            b = b.iloc[0]
        if b is None or not b.played:
            log.loc[i, ["actual", "result", "units"]] = [None, "void", 0.0]
            continue
        a = float(b[SLEEPER_STAT[r.market]])
        res = settle(r.market, r.side, r.line, a)
        u = {"win": american_to_decimal(r.price) - 1, "loss": -1.0, "push": 0.0}[res]
        log.loc[i, ["actual", "result", "units"]] = [a, res, round(u, 4)]
    log.to_csv(LOG, index=False)
    g = log[wk]
    print(f"week {week}: graded {(g.result != 'void').sum()} sides, {(g.result == 'void').sum()} void")
    report(log[wk], title=f"WEEK {week}")


# ------------------------------------------------------------------ report
def _row(label, g):
    g = g[g.result.isin(["win", "loss", "push"])]
    if g.empty:
        return f"  {label:<34}{'-':>5}"
    w, l, p = (g.result == s for s in ("win", "loss", "push"))
    n = len(g)
    exp = g.p_fair.mean()
    rate = w.sum() / max(w.sum() + l.sum(), 1)
    return (f"  {label:<34}{n:>5}  {w.sum():>4}-{l.sum()}-{p.sum():<4}{rate:>7.1%}{exp:>8.1%}"
            f"{g.units.sum():>+9.1f}{g.units.sum() / n:>+8.1%}")


FORM_BUCKETS = [("under 40%", 0, 0.4), ("40% to 60%", 0.4, 0.6), ("60% and up", 0.6, 1.01)]
MATCHUP_BUCKETS = [("tough (10%+ below average)", 0, 1 / 1.1), ("neutral", 1 / 1.1, 1.1),
                   ("soft (10%+ above average)", 1.1, 99)]


def report(log=None, title="ALL WEEKS"):
    if log is None:
        if not os.path.exists(LOG):
            raise SystemExit("no prop log yet")
        log = pd.read_csv(LOG)
    log = log[log.result.notna()]
    if log.empty:
        print("nothing graded yet")
        return
    for c in ("shop", "proj"):
        log[c] = log[c].astype(str).str.lower() == "true"
    print("=" * 84)
    print(f"PLAYER PROPS -- {title}   (1 unit per side at the logged best price)")
    print("=" * 84)
    print(f"  {'':<34}{'n':>5}  {'W-L-P':<9}{'win%':>7}{'mkt P':>8}{'units':>9}{'ROI':>8}")
    print(_row("every priced side", log))
    print(_row("  overs / yes", log[log.side.isin(["over", "yes"])]))
    print(_row("  unders / no", log[log.side.isin(["under", "no"])]))
    print(_row(f"SHOP (EV vs other books >= {SHOP_EV:.0%})", log[log.shop]))
    print(_row(f"PROJ (Sleeper edge >= {PROJ_EDGE:.0%})", log[log.proj]))
    print(_row("  PROJ overs / yes", log[log.proj & log.side.isin(["over", "yes"])]))
    print(_row("  PROJ unders / no", log[log.proj & log.side.isin(["under", "no"])]))
    print(_row("SHOP and PROJ", log[log.shop & log.proj]))
    # each prop once (over / yes side): the under's edge is the over's, negated
    print("\n  Sleeper edge on the over / yes (P slp - P mkt), each prop once:")
    e = log[log.edge.notna() & log.side.isin(["over", "yes"])]
    for lo, hi in [(-1, -0.08), (-0.08, -0.03), (-0.03, 0.03), (0.03, 0.08), (0.08, 1)]:
        print(_row(f"    {lo:+.0%} to {hi:+.0%}" if abs(lo) < 1 and abs(hi) < 1 else
                   (f"    below {hi:+.0%}" if lo == -1 else f"    {lo:+.0%} and up"),
                   e[(e.edge >= lo) & (e.edge < hi)]))
    if "hit_rate" in log:
        # one row per prop (the over / yes side): both sides are logged, and an over that hit
        # in 7 of 8 games is an under that hit in 1, so counting both would mirror every result
        ov = log[log.side.isin(["over", "yes"])]
        h = ov[ov.hit_rate.notna()]
        print("\n  Recent form (overs / yes, each prop once): share of his last 8 games over this line:")
        for lab, lo, hi in FORM_BUCKETS:
            print(_row(f"    {lab}", h[(h.hit_rate >= lo) & (h.hit_rate < hi)]))
        m = ov[ov.opp_factor.notna()]
        print("\n  Matchup (overs / yes): what the defense allows his position vs league average:")
        for lab, lo, hi in MATCHUP_BUCKETS:
            print(_row(f"    {lab}", m[(m.opp_factor >= lo) & (m.opp_factor < hi)]))
    print("\n  PROJ by market:")
    for m, g in log[log.proj].groupby("market"):
        print(_row(f"    {SHORT[m]}", g))
    print("\n  win% vs mkt P: how often the side won vs how often the market's fair price said it")
    print("  would. ROI includes the vig. Props carry ~5-10% vig, so a flat bettor starts behind.")


# ------------------------------------------------------------------ dashboard
DASH_DIR = os.path.join(DATA, "dashboard")
SHAPES_CHECK = os.path.join(HERE, "..", "nfl_fantasy", "data", "prop_shapes_check.csv")


def _stats(g):
    g = g[g.result.isin(["win", "loss", "push"])]
    w, l, p = (int((g.result == s).sum()) for s in ("win", "loss", "push"))
    n = len(g)
    return dict(n=n, wins=w, losses=l, pushes=p,
                win_pct=round(w / (w + l), 4) if w + l else None,
                mkt_p=round(float(g.p_fair.mean()), 4) if n else None,
                units=round(float(g.units.sum()), 2), roi=round(float(g.units.sum()) / n, 4) if n else None)


def dashboard():
    """Aggregates for the props dashboard, one JSON file per dataset."""
    import json
    log = pd.read_csv(LOG)
    for c in ("shop", "proj"):
        log[c] = log[c].astype(str).str.lower() == "true"
    g = log[log.result.notna()]
    over = g.side.isin(["over", "yes"])
    groups = [("all", "Every side", g), ("overs", "Overs", g[over]), ("unders", "Unders", g[~over]),
              ("proj", "PROJ picks", g[g.proj]), ("proj_overs", "PROJ overs", g[g.proj & over]),
              ("proj_unders", "PROJ unders", g[g.proj & ~over]), ("shop", "SHOP picks", g[g.shop])]
    out = {"summary": [dict(group=k, label=lab, weeks=int(g.week.nunique()), **_stats(x)) for k, lab, x in groups]}

    weekly = []
    for sig, mask in (("all", g.index == g.index), ("proj", g.proj), ("shop", g.shop)):
        cum = 0.0
        for wk, x in g[mask].groupby("week"):
            st = _stats(x)
            cum += st["units"]
            weekly.append(dict(week=int(wk), signal=sig, **st, cum_units=round(cum, 2)))
    out["weekly"] = weekly

    e = g[g.edge.notna() & over]                 # each prop once: the under's edge mirrors the over's
    cuts = [(-1, -0.08, "below −8"), (-0.08, -0.03, "−8 to −3"), (-0.03, 0.03, "−3 to +3"),
            (0.03, 0.08, "+3 to +8"), (0.08, 1, "+8 and up")]
    out["edge_buckets"] = [dict(bucket=lab, order=i, **_stats(e[(e.edge >= lo) & (e.edge < hi)]))
                           for i, (lo, hi, lab) in enumerate(cuts)]

    ov = g[g.side.isin(["over", "yes"])]
    out["context_buckets"] = (
        [dict(id=f"form {lab}", feature="Recent form", bucket=lab, order=i,
              **_stats(ov[ov.hit_rate.notna() & (ov.hit_rate >= lo) & (ov.hit_rate < hi)]))
         for i, (lab, lo, hi) in enumerate(FORM_BUCKETS)] +
        [dict(id=f"matchup {lab}", feature="Matchup", bucket=lab, order=i,
              **_stats(ov[ov.opp_factor.notna() & (ov.opp_factor >= lo) & (ov.opp_factor < hi)]))
         for i, (lab, lo, hi) in enumerate(MATCHUP_BUCKETS)])

    pj = g[g.proj]
    out["by_market"] = [dict(market=SHORT[m], **_stats(x),
                             overs=int((x.side.isin(["over", "yes"]) & (x.result != "void")).sum()),
                             unders=int((x.side.isin(["under", "no"]) & (x.result != "void")).sum()))
                        for m, x in pj.groupby("market")]

    wk = int(log.week.max())
    f = log[(log.week == wk) & (log.proj | log.shop)].copy()
    f["bet"] = [("YES" if s == "yes" else "NO") if m == "player_anytime_td" else f"{s.upper()} {l:g}"
                for m, s, l in zip(f.market, f.side, f.line)]
    f["signal"] = np.where(f.proj & f.shop, "PROJ + SHOP", np.where(f.proj, "PROJ", "SHOP"))
    out["picks"] = [dict(id=f"{r.key}|{r.market}|{r.side}", week=wk, player=r.player,
                         team=None if pd.isna(r.team) else r.team, game=r.game, market=SHORT[r.market],
                         bet=r.bet, price=int(r.price), book=r.book, p_mkt=r.p_fair,
                         p_slp=None if pd.isna(r.p_slp) else r.p_slp,
                         edge=None if pd.isna(r.edge) else r.edge, ev_shop=r.ev_shop, signal=r.signal,
                         hit_rate=None if pd.isna(r.hit_rate) else float(r.hit_rate),
                         l8_avg=None if pd.isna(r.l8_avg) else float(r.l8_avg),
                         hist_avg=None if pd.isna(r.hist_avg) else float(r.hist_avg),
                         hist_n=None if pd.isna(r.hist_n) else int(r.hist_n),
                         opp=None if pd.isna(r.opp) else r.opp,
                         opp_factor=None if pd.isna(r.opp_factor) else float(r.opp_factor),
                         result="open" if pd.isna(r.result) else r.result,
                         actual=None if pd.isna(r.actual) else float(r.actual))
                    for r in f.sort_values("edge", ascending=False).itertuples()]

    if os.path.exists(SHAPES_CHECK):
        sc = pd.read_csv(SHAPES_CHECK)
        names = {"player_pass_yds": "Passing yds", "player_rush_yds": "Rushing yds",
                 "qb_rush_yds": "QB rushing yds", "player_reception_yds": "Receiving yds"}
        sc["id"] = sc.market + " " + sc.band
        sc["market"] = sc.market.map(names)
        out["shape_check"] = sc.to_dict("records")

    os.makedirs(DASH_DIR, exist_ok=True)
    for k, rows in out.items():
        json.dump(rows, open(os.path.join(DASH_DIR, f"{k}.json"), "w"), indent=0)
    print(f"dashboard data: {', '.join(f'{k} ({len(v)})' for k, v in out.items())} -> "
          f"{os.path.relpath(DASH_DIR)}")


def main():
    a = sys.argv[1:]
    if not a or a[0] not in ("sheet", "grade", "report", "dashboard"):
        raise SystemExit(__doc__)
    if a[0] == "report":
        return report()
    if a[0] == "dashboard":
        return dashboard()
    week = int(a[1])
    if a[0] == "grade":
        return grade(week)
    sheet(week, cached="--cached" in a, log="--no-log" not in a)


if __name__ == "__main__":
    main()
