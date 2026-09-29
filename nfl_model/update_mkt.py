"""
Refresh mkt_line in data/weekly_lines.xlsx from nflverse (my_line is never touched).

    python3 update_mkt.py 4            # refresh week 4 rows already in the workbook
    python3 update_mkt.py 4 --add      # also append any week-4 games not yet in
                                       # the workbook (my_line left blank)
    python3 update_mkt.py 4 --dry-run  # show changes without writing

nflverse spread_line is POSITIVE = home favored; the workbook is home-perspective
(negative = home favored), so it is negated here.
"""

import sys

import pandas as pd

URL = "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"
FILE = "data/weekly_lines.xlsx"
SEASON = 2026


def main():
    week = int(sys.argv[1])
    add, dry = "--add" in sys.argv, "--dry-run" in sys.argv

    g = pd.read_csv(URL)
    g = g[(g.season == SEASON) & (g.game_type == "REG") & (g.week == week)].copy()
    if g.empty:
        raise SystemExit(f"no {SEASON} week {week} games on nflverse")
    fix = lambda t: "LAR" if t == "LA" else t
    g["away"], g["home"] = g.away_team.map(fix), g.home_team.map(fix)
    g["nfl"] = -g.spread_line
    nfl = {(r.away, r.home): r.nfl for r in g.itertuples() if pd.notna(r.nfl)}

    df = pd.read_excel(FILE)
    have = set()
    for i, r in df[df.week == week].iterrows():
        k = (r.away, r.home)
        have.add(k)
        if k in nfl and nfl[k] != r.mkt_line:
            print(f"{r.away} @ {r.home}: {r.mkt_line} -> {nfl[k]}")
            df.at[i, "mkt_line"] = nfl[k]
    new = [dict(week=week, away=a, home=h, my_line=None, mkt_line=v)
           for (a, h), v in nfl.items() if (a, h) not in have]
    if new:
        print(f"{len(new)} games not in the workbook" + (" -- adding" if add else " (use --add)"))
        if add:
            df = pd.concat([df, pd.DataFrame(new)], ignore_index=True)
    if not dry:
        df = df.sort_values("week", kind="stable")
        df.to_excel(FILE, index=False)


if __name__ == "__main__":
    main()
