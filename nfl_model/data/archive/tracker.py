"""
Results tracker: record, ROI, closing line value, and calibration.

CLV is the primary metric. It is measurable in tens of bets; ATS record
needs thousands. If you consistently beat the closing number you are
getting the best of it whether or not this season's record shows it.

    python3 tracker.py add    <- append picks from picks_pending.csv
    python3 tracker.py report <- full report
"""

import os
import sys
import numpy as np
import pandas as pd
from scipy import stats

LOG = "data/archive/bet_log.csv"
BREAKEVEN = 0.5238

COLUMNS = [
    "season", "week", "game", "pick", "my_line", "mkt_line_at_bet",
    "closing_line", "pts_value", "model_win_rate", "verdict",
    "result", "margin", "notes",
]


def wilson(w, n, conf=0.95):
    if n == 0:
        return (np.nan, np.nan, np.nan)
    z = stats.norm.ppf(1 - (1 - conf) / 2)
    p = w / n
    den = 1 + z**2 / n
    ctr = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return (p, ctr - h, ctr + h)


def load():
    if os.path.exists(LOG):
        return pd.read_csv(LOG)
    return pd.DataFrame(columns=COLUMNS)


def save(df):
    os.makedirs("data", exist_ok=True)
    df.to_csv(LOG, index=False)


def report(df):
    print("=" * 74)
    print("BET LOG REPORT")
    print("=" * 74)

    if df.empty:
        print("no bets logged yet")
        return

    graded = df[df.result.isin(["W", "L", "P"])].copy()
    print(f"\nlogged: {len(df)}   graded: {len(graded)}")

    if graded.empty:
        return

    w = int((graded.result == "W").sum())
    l = int((graded.result == "L").sum())
    p = int((graded.result == "P").sum())
    n = w + l
    rate, lo, hi = wilson(w, n)
    units = w * (100 / 110) - l

    print(f"\nrecord: {w}-{l}-{p}")
    print(f"win rate: {rate*100:.1f}%   95% CI: [{lo*100:.1f}%, {hi*100:.1f}%]")
    print(f"units: {units:+.2f}   ROI: {units/len(graded)*100:+.1f}%")
    print(f"breakeven: {BREAKEVEN*100:.2f}%")
    if lo > BREAKEVEN:
        print("  -> CI clears breakeven. Real edge, on this sample.")
    elif hi < BREAKEVEN:
        print("  -> CI entirely below breakeven. Losing.")
    else:
        print("  -> CI straddles breakeven. Indistinguishable from noise.")

    # ---- CLV: the metric that actually converges ----
    clv = graded.dropna(subset=["closing_line", "my_line"]).copy()
    if not clv.empty:
        # Lines are stored HOME-perspective. Work out which side was bet
        # by matching the pick's team to the home team in "AWAY @ HOME",
        # then measure my_line against the close from that side.
        home_team = clv.game.str.split("@").str[1].str.strip()
        pick_team = clv.pick.str.rsplit(" ", n=1).str[0].str.strip()
        is_home = pick_team == home_team
        clv["clv_pts"] = np.where(
            is_home,
            clv.my_line - clv.closing_line,
            clv.closing_line - clv.my_line,
        ).round(2)
        beat = int((clv.clv_pts > 0).sum())
        tied = int((clv.clv_pts == 0).sum())
        lost = int((clv.clv_pts < 0).sum())
        print(f"\nCLOSING LINE VALUE  (n={len(clv)})")
        print(f"  beat close: {beat}   tied: {tied}   worse: {lost}")
        print(f"  mean CLV: {clv.clv_pts.mean():+.2f} pts")
        print(f"  beat-rate: {beat/len(clv)*100:.1f}%")
        if clv.clv_pts.mean() > 0.3:
            print("  -> positive CLV. Getting the best of the number.")
        elif clv.clv_pts.mean() < -0.1:
            print("  -> negative CLV. You are on the wrong side of the move.")

    # ---- calibration: does a higher model win rate actually win more? ----
    cal = graded[graded.result.isin(["W", "L"])].dropna(subset=["model_win_rate"])
    if len(cal) >= 8:
        print("\nCALIBRATION  (model win rate vs actual)")
        cal = cal.copy()
        cal["bucket"] = pd.cut(
            cal.model_win_rate,
            [0, 0.524, 0.545, 0.565, 1.0],
            labels=["<52.4 (pass)", "52.4-54.5", "54.5-56.5", "56.5+"],
        )
        for b, sub in cal.groupby("bucket", observed=True):
            ww = int((sub.result == "W").sum())
            nn = len(sub)
            print(
                f"  {str(b):<14} n={nn:<4} predicted={sub.model_win_rate.mean()*100:5.1f}%"
                f"   actual={ww/nn*100:5.1f}%"
            )
        if len(cal) < 100:
            print("  (under 100 bets: these buckets are not yet meaningful)")

    # ---- by points of value held ----
    pv = graded[graded.result.isin(["W", "L"])].dropna(subset=["pts_value"])
    if len(pv) >= 8:
        print("\nBY POINTS OF LINE VALUE HELD")
        pv = pv.copy()
        pv["vb"] = pd.cut(
            pv.pts_value, [-99, 0.01, 0.99, 1.99, 99],
            labels=["0 (no value)", "0.5", "1-1.5", "2+"],
        )
        for b, sub in pv.groupby("vb", observed=True):
            ww = int((sub.result == "W").sum())
            print(f"  {str(b):<14} {ww}-{len(sub)-ww}   {ww/len(sub)*100:5.1f}%")


def seed_week3():
    """Week 3 2026 card as actually given, with results."""
    rows = [
        # game, pick, my_line(home persp), mkt_at_bet, close, pts_val, conf->rate, result
        ("KC @ MIA",  "MIA +11.5", 11.5,  10.5, 10.5, 1.0, "PLAY", "L", -14),
        ("BAL @ DAL", "BAL -2.5",  2.5,   3.5,  3.5,  1.0, "PLAY", "W", 3),
        ("CIN @ PIT", "PIT +3.5",  3.5,   3.5,  3.5,  0.0, "PASS", "W", 3),
        ("TEN @ NYG", "TEN +3.5", -3.5,  -2.5, -2.5,  1.0, "PLAY", "L", -5),
        ("LAR @ DEN", "DEN +2.5",  2.5,   2.5,  2.5,  0.0, "PASS", "W", 4),
        ("SEA @ WAS", "WAS +7.5",  7.5,   7.5,  7.5,  0.0, "PASS", "W", 2),
        ("HOU @ IND", "IND +2.5",  2.5,   1.5,  1.5,  1.0, "PLAY", "W", 2),
        ("CAR @ CLE", "CLE +2.5",  2.5,   2.5,  2.5,  0.0, "PASS", "W", 3),
        ("NE @ JAX",  "NE +3.5",  -3.5,  -3.0, -3.0,  0.5, "lean", "L", -29),
        ("ARI @ SF",  "SF -8.5",  -8.5,  -8.5, -8.5,  0.0, "PASS", "L", 6),
        ("LV @ NO",   "NO -3.5",  -3.5,  -3.0, -3.0, -0.5, "PASS", "L", -8),
        ("NYJ @ DET", "DET -6.5", -6.5,  -6.5, -6.5,  0.0, "PASS", "W", 7),
        ("LAC @ BUF", "LAC +7.5", -7.5,  -7.0, -7.0,  0.5, "lean", "L", -8),
        ("MIN @ TB",  "MIN -1.5",  1.5,   1.5,  1.5,  0.0, "PASS", "W", 7),
    ]
    rate_map = {"PLAY": 0.545, "lean": 0.525, "PASS": 0.500}
    out = []
    for g, pick, my, mkt, close, pv, verdict, res, marg in rows:
        out.append(dict(
            season=2026, week=3, game=g, pick=pick, my_line=my,
            mkt_line_at_bet=mkt, closing_line=close, pts_value=pv,
            model_win_rate=rate_map[verdict], verdict=verdict,
            result=res, margin=marg, notes="seeded from chat card",
        ))
    return pd.DataFrame(out)


if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else "report"
    df = load()
    if cmd == "seed":
        df = pd.concat([df, seed_week3()], ignore_index=True)
        save(df)
        print(f"seeded {len(seed_week3())} bets")
    report(df)
