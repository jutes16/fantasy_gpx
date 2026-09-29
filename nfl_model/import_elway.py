"""
Import an ELWAY projection table (pasted from the ELWAY page) into
data/weekly_lines.xlsx, and into data/pool_picks_log.csv if that week was
already submitted.

Copy the table on the ELWAY page, then:

    pbpaste | python3 import_elway.py 4        <- straight from the clipboard
    python3 import_elway.py 4                  <- re-import the saved paste

Every paste is saved to data/elway/<season>_wk<NN>.txt first, so it can be
re-parsed later. ELWAY columns written (HOME-perspective, like everything
else here):

    elway_line      projected home spread (negative = home favored)
    elway_total     projected total
    elway_home_wp   home win probability (0-1)

The expected paste looks like the ELWAY table: for each game the week, an
optional "N" (neutral site), home team, avg pts, win %, away team, avg pts,
win %, home spread, total. Whitespace and line breaks don't matter.
"""

import os
import re
import sys

import numpy as np
import pandas as pd

from fetch_splits import canon

SEASON = 2026
WORKBOOK = "data/weekly_lines.xlsx"
LOG = "data/pool_picks_log.csv"
RAW_DIR = "data/elway"
ELWAY_COLS = ["elway_line", "elway_total", "elway_home_wp"]

NUM = r"[+-]?\d+(?:\.\d+)?"
ROW = re.compile(
    rf"(?P<wk>\d{{1,2}})\s+(?:N\s+)?"
    rf"(?P<home>[A-Z]{{2,3}})\s+(?P<hpts>{NUM})\s+(?P<hwp>{NUM})%\s+"
    rf"(?P<away>[A-Z]{{2,3}})\s+(?P<apts>{NUM})\s+(?P<awp>{NUM})%\s+"
    rf"(?P<spread>{NUM}|PK|pk|Pick|EVEN)\s+(?P<total>{NUM})"
)


def parse(text):
    """Pasted ELWAY table -> DataFrame, one row per game."""
    flat = " ".join(text.split())
    rows = []
    for m in ROW.finditer(flat):
        sp = m["spread"]
        rows.append(dict(
            week=int(m["wk"]), home=canon(m["home"]), away=canon(m["away"]),
            elway_line=0.0 if not re.match(NUM, sp) else float(sp),
            elway_total=float(m["total"]),
            elway_home_wp=round(float(m["hwp"]) / 100, 3),
            elway_home_pts=float(m["hpts"]), elway_away_pts=float(m["apts"]),
        ))
    return pd.DataFrame(rows)


def write_into(target, df, has_season):
    """Overwrite the ELWAY columns for matching games. Returns (matched, missing)."""
    for c in ELWAY_COLS:
        if c not in target.columns:
            target[c] = np.nan
    look = {(r.week, r.away, r.home): r for r in df.itertuples()}
    hit = set()
    for i, r in target.iterrows():
        if has_season and int(r.season) != SEASON:
            continue
        k = (int(r.week), canon(r.away), canon(r.home))
        if k in look:
            e = look[k]
            for c in ELWAY_COLS:
                target.at[i, c] = getattr(e, c)
            hit.add(k)
    return hit, [k for k in look if k not in hit]


def main():
    if len(sys.argv) < 2:
        raise SystemExit(__doc__)
    week = int(sys.argv[1])
    path = os.path.join(RAW_DIR, f"{SEASON}_wk{week:02d}.txt")

    piped = "" if sys.stdin.isatty() else sys.stdin.read()
    if parse(piped).shape[0]:            # only a paste with games replaces the file
        os.makedirs(RAW_DIR, exist_ok=True)
        with open(path, "w") as f:
            f.write(piped)
        text = piped
    elif piped.strip():
        raise SystemExit("couldn't find any games in the paste -- check the format "
                         f"(nothing saved; {path} left as it was)")
    elif os.path.exists(path):
        text = open(path).read()
    else:
        raise SystemExit(f"nothing piped in and no saved paste at {path}\n"
                         f"copy the ELWAY table, then: pbpaste | python3 import_elway.py {week}")

    df = parse(text)
    if df.empty:
        raise SystemExit("couldn't find any games in the paste -- check the format")
    other = sorted(set(df.week) - {week})
    if other:
        print(f"WARNING: paste also has week(s) {other}; importing week {week} only")
    df = df[df.week == week]
    print(f"parsed {len(df)} ELWAY games for week {week}")

    wb = pd.read_excel(WORKBOOK)
    hit, missing = write_into(wb, df, has_season=False)
    wb.to_excel(WORKBOOK, index=False)
    print(f"  workbook: {len(hit)} games updated")
    if missing:
        print(f"  not in the workbook (add the week first?): "
              f"{', '.join(f'{a} @ {h}' for _, a, h in missing)}")
    no_elway = wb[(wb.week == week) & wb.elway_line.isna()]
    if len(no_elway):
        print(f"  workbook games with no ELWAY line: "
              f"{', '.join(f'{a} @ {h}' for a, h in zip(no_elway.away, no_elway.home))}")

    if os.path.exists(LOG):
        log = pd.read_csv(LOG)
        if ((log.season == SEASON) & (log.week == week)).any():
            hit, _ = write_into(log, df, has_season=True)
            log.to_csv(LOG, index=False)
            print(f"  {LOG}: {len(hit)} games updated (week already submitted)")


if __name__ == "__main__":
    main()
