"""
Score a week from data/weekly_lines.xlsx. Lines are HOME-perspective
(negative = home favored).

The workbook tracks the whole season: columns week, away, home, my_line, mkt_line.
Optional market-signal columns (fill before submitting; blank is fine):

    open_line       opening market line, home-perspective
    home_bets_pct   % of tickets on the HOME side (0-100)
    home_money_pct  % of handle on the HOME side (0-100)
    sharp_side      "home" / "away" where sharp action was reported
    sharp_type      "book" (a sportsbook reported sharp action) or
                    "pro" (one pro bettor's pick / one large bet)
    signal_notes    free text (source, injuries, etc.)
    elway_line, elway_total, elway_home_wp
                    ELWAY projection -- filled by import_elway.py, not by hand

log_week.py submit snapshots them into data/pool_picks_log.csv, and
pool_tracker.py report grades each signal once results are in.

Append each week's games to it, then run:

    python3 sample_week.py          # latest week in the file
    python3 sample_week.py 3        # a specific week
"""

import os
import sys

import pandas as pd

from model import score_week, fmt

LINES_FILE = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "data", "weekly_lines.xlsx")
SIGNAL_COLS = ["open_line", "home_bets_pct", "home_money_pct",
               "sharp_side", "sharp_type", "signal_notes",
               "elway_line", "elway_total", "elway_home_wp"]   # import_elway.py


def load_games(week=None, path=LINES_FILE):
    """Games for `week` (default: latest week in the file) as score_week dicts."""
    df = pd.read_excel(path)
    if week is None:
        week = int(df["week"].max())
    wk = df[df["week"] == week]
    df = wk[wk["my_line"].notna()]  # skip games with no line of mine
    if df.empty:
        why = "none have my_line filled in" if len(wk) else "week not in file"
        raise ValueError(f"no games to score for week {week} ({why}): {path}")
    games = []
    for r in df.to_dict("records"):
        g = dict(away=r["away"], home=r["home"], my_line=float(r["my_line"]),
                 mkt_line=float(r["mkt_line"]))
        g.update({c: r[c] for c in SIGNAL_COLS if c in r and pd.notna(r[c])})
        games.append(g)
    return games


if __name__ == "__main__":
    wk = int(sys.argv[1]) if len(sys.argv) > 1 else None
    rows = score_week(load_games(wk))
    print(fmt(rows))
    plays = [r for r in rows if r["verdict"] == "PLAY"]
    leans = [r for r in rows if r["verdict"] == "lean"]
    print(
        f"\nPLAY: {len(plays)}   lean: {len(leans)}   "
        f"PASS: {len(rows)-len(plays)-len(leans)}"
    )
