"""
Pool tracker: weekly 5-pick record, season standings, CLV, calibration.

    python3 pool_tracker.py log      <- append this week's card from pending.csv
    python3 pool_tracker.py report   <- full report
    python3 pool_tracker.py seed     <- load the Week 3 2026 card

Log columns (data/pool_log.csv):
    season, week, slot, game, pick, my_line, mkt_line, closing_line,
    pts_value, tier, model_win_rate, result(W/L/P), margin
"""

import os
import sys
import numpy as np
import pandas as pd
from scipy import stats

LOG = "data/pool_log.csv"
PICKS_PER_WEEK = 5
BASELINE = 0.50  # no vig in a pool


def wilson(w, n, conf=0.95):
    if n == 0:
        return (np.nan, np.nan, np.nan)
    z = stats.norm.ppf(1 - (1 - conf) / 2)
    p = w / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return (p, c - h, c + h)


def load():
    if os.path.exists(LOG):
        return pd.read_csv(LOG)
    return pd.DataFrame()


def report(df):
    print("=" * 70)
    print("POOL TRACKER")
    print("=" * 70)
    if df.empty:
        print("nothing logged yet")
        return

    g = df[df.result.isin(["W", "L", "P"])].copy()
    if g.empty:
        print(f"{len(df)} picks logged, none graded yet")
        return

    w = int((g.result == "W").sum())
    l = int((g.result == "L").sum())
    p = int((g.result == "P").sum())
    n = w + l
    rate, lo, hi = wilson(w, n)
    weeks = g.week.nunique()
    base_wins = weeks * PICKS_PER_WEEK * BASELINE

    print(f"\nweeks: {weeks}   picks graded: {len(g)}")
    print(f"record: {w}-{l}" + (f"-{p}" if p else ""))
    print(f"win rate: {rate*100:.1f}%   95% CI: [{lo*100:.1f}%, {hi*100:.1f}%]")
    print(f"wins: {w}   coin-flip baseline: {base_wins:.0f}   ({w-base_wins:+.0f})")
    if lo > BASELINE:
        print("  -> CI clears 50%. Ahead of a coin flip on this sample.")
    elif hi < BASELINE:
        print("  -> CI entirely below 50%. Behind a coin flip.")
    else:
        print("  -> CI straddles 50%. Not yet distinguishable from luck.")

    # ---- week by week ----
    print("\nWEEK BY WEEK")
    wk = (
        g.assign(win=(g.result == "W").astype(int))
        .groupby(["season", "week"])
        .agg(picks=("win", "size"), wins=("win", "sum"))
        .reset_index()
    )
    wk["vs_base"] = wk.wins - wk.picks * BASELINE
    for _, r in wk.iterrows():
        bar = "#" * int(r.wins)
        print(
            f"  {int(r.season)} wk{int(r.week):<3} {int(r.wins)}/{int(r.picks)}"
            f"  {r.vs_base:+.1f}   {bar}"
        )
    print(f"  {'TOTAL':<10} {w}/{len(g)}  {w-base_wins:+.1f}")

    # ---- CLV ----
    clv = g.dropna(subset=["closing_line", "my_line"]).copy()
    if not clv.empty:
        home = clv.game.str.split("@").str[1].str.strip()
        pteam = clv.pick.str.rsplit(" ", n=1).str[0].str.strip()
        is_home = pteam == home
        clv["clv"] = np.where(
            is_home, clv.my_line - clv.closing_line, clv.closing_line - clv.my_line
        ).round(2)
        print(f"\nCLOSING LINE VALUE  (n={len(clv)})")
        print(
            f"  beat {int((clv.clv>0).sum())} | tied {int((clv.clv==0).sum())} "
            f"| worse {int((clv.clv<0).sum())}"
        )
        print(f"  mean CLV: {clv.clv.mean():+.2f} pts")
        print("  (this converges far faster than W-L. Watch it, not the record.)")

    # ---- by value tier ----
    t = g[g.result.isin(["W", "L"])]
    if "tier" in t.columns and len(t) >= 5:
        print("\nBY VALUE TIER")
        for tier, sub in t.groupby("tier"):
            ww = int((sub.result == "W").sum())
            print(f"  {tier:<11} {ww}-{len(sub)-ww}   {ww/len(sub)*100:5.1f}%")

    if len(g) < 90:
        print(
            f"\nNOTE: {len(g)} graded picks. One season is ~90. Power to detect a "
            "3.7pt edge over one season is about 10%, so treat the record as "
            "provisional and judge on CLV."
        )


def seed():
    """Week 3 2026: the 5 the pool engine would have submitted."""
    rows = [
        # slot, game, pick, my_line(home persp), mkt, close, val, tier, rate, result, margin
        (1, "TEN @ NYG", "TEN +3.5", -3.5, -2.5, -2.5, 1.0, "EDGE", 0.561, "L", -5),
        (2, "BAL @ DAL", "BAL -2.5",  2.5,  3.5,  3.5, 1.0, "EDGE", 0.545, "W", 3),
        (3, "NE @ JAX",  "NE +3.5",  -3.5, -3.0, -3.0, 0.5, "slim", 0.534, "L", -29),
        (4, "LV @ NO",   "LV +3.5",  -3.5, -3.0, -3.0, 0.5, "slim", 0.534, "W", 8),
        (5, "KC @ MIA",  "MIA +11.5",11.5, 10.5, 10.5, 1.0, "EDGE", 0.533, "L", -14),
    ]
    return pd.DataFrame([
        dict(season=2026, week=3, slot=s, game=gm, pick=pk, my_line=my,
             mkt_line=mk, closing_line=cl, pts_value=v, tier=ti,
             model_win_rate=r, result=res, margin=mg)
        for s, gm, pk, my, mk, cl, v, ti, r, res, mg in rows
    ])


if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else "report"
    df = load()
    if cmd == "seed":
        os.makedirs("data", exist_ok=True)
        df = pd.concat([df, seed()], ignore_index=True) if not df.empty else seed()
        df.to_csv(LOG, index=False)
        print(f"seeded {len(seed())} picks\n")
    report(df)
