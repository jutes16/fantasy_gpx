"""
Score a week from data/weekly_lines.xlsx. Lines are HOME-perspective
(negative = home favored).

The workbook tracks the whole season: columns week, away, home, my_line, mkt_line.
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
    return [
        dict(away=r.away, home=r.home, my_line=float(r.my_line),
             mkt_line=float(r.mkt_line))
        for r in df.itertuples()
    ]


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
