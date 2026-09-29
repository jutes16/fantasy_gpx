"""
Pull Action Network consensus betting splits via the Apify actor
zen-studio/action-network-odds and store them in data/action_splits.csv.

Needs:  pip install apify-client   and   export APIFY_TOKEN=...  (never in the repo)

    python3 fetch_splits.py 2025 1-18          # backfill a season (weeks 1..18)
    python3 fetch_splits.py 2026 4             # one week
    python3 fetch_splits.py 2026 4 --to-workbook
        <- also fill EMPTY open_line / home_bets_pct / home_money_pct cells in
           data/weekly_lines.xlsx, so log_week.py submit snapshots them.
           Run this right before you submit.
    python3 fetch_splits.py 2025 3 --from-raw  <- re-parse a saved response, no API call
    python3 fetch_splits.py import 2026        <- copy already-fetched 2026 splits into
    python3 fetch_splits.py import 2026 1-3       weekly_lines.xlsx and pool_picks_log.csv
                                                  (no API call; empty cells only)
    python3 fetch_splits.py import 2026 --overwrite
                                               <- Action Network replaces hand-entered
                                                  splits/open lines (old values kept
                                                  in signal_notes)

Every response is saved to data/raw/action_network/ first, so a parsing bug
never costs a second paid call. Lines are HOME-perspective (negative = home
favored), same as the rest of the project.

Cost: about $4 per 1,000 games plus $4 per 1,000 for line movement, so a full
season is roughly $2 (inside Apify's free credits).

Caveat: splits for completed games are Action Network's final numbers, not
what you would have seen at submit time. Treat backfilled weeks accordingly.
"""

import json
import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd

ACTOR = "zen-studio/action-network-odds"
OUT = "data/action_splits.csv"
RAW_DIR = "data/raw/action_network"
WORKBOOK = "data/weekly_lines.xlsx"
LOG = "data/pool_picks_log.csv"
KEYS = ["season", "week", "away", "home"]

# one spelling per team, so Action Network, nflverse ("LA") and the workbook
# ("LAR") all match
CANON = {"LA": "LAR", "WSH": "WAS", "JAC": "JAX", "LVR": "LV", "OAK": "LV",
         "SD": "LAC", "STL": "LAR"}


def canon(team):
    t = str(team).strip().upper()
    return CANON.get(t, t)


def parse_weeks(arg):
    if "-" in arg:
        a, b = arg.split("-")
        return list(range(int(a), int(b) + 1))
    return [int(w) for w in arg.split(",")]


def raw_path(season, week):
    return os.path.join(RAW_DIR, f"{season}_wk{week:02d}.json")


def fetch(season, week):
    """Call the actor for one week, save the raw items, return them."""
    token = os.environ.get("APIFY_TOKEN")
    if not token:
        raise SystemExit("APIFY_TOKEN is not set -- see the docstring")
    try:
        from apify_client import ApifyClient
    except ImportError:
        raise SystemExit("pip install apify-client")

    client = ApifyClient(token)
    run = client.actor(ACTOR).call(run_input=dict(
        leagues=["nfl"], season=season, week=week, seasonType="reg",
        includeLineMovement=True, includeExpertPicks=False, includeProps=False,
        includeStandings=False, includeInjuries=False,
    ))
    if run is None:
        raise SystemExit(f"actor run failed for {season} wk{week}")
    ds = run["defaultDatasetId"] if isinstance(run, dict) else run.default_dataset_id
    items = list(client.dataset(ds).iterate_items())

    os.makedirs(RAW_DIR, exist_ok=True)
    with open(raw_path(season, week), "w") as f:
        json.dump(items, f, indent=1, default=str)
    return items


def _home_side(sides):
    for s in sides or []:
        if str(s.get("side", "")).lower() == "home":
            return s
    return {}


def parse(items, season, week):
    """Actor items -> one row per game in this project's columns."""
    stamp = datetime.now(timezone.utc).isoformat(timespec="seconds")
    rows = []
    for g in items:
        away = (g.get("awayTeam") or {}).get("abbreviation")
        home = (g.get("homeTeam") or {}).get("abbreviation")
        if not away or not home:
            continue
        spread = ((g.get("consensus") or {}).get("spread") or {}).get("sides")
        h = _home_side(spread)
        move = _home_side((g.get("lineMovement") or {}).get("spread"))
        rows.append(dict(
            season=season, week=week, away=canon(away), home=canon(home),
            an_line=h.get("line", np.nan),
            an_open_line=move.get("openingLine", np.nan),
            an_home_bets_pct=h.get("ticketPercent", np.nan),
            an_home_money_pct=h.get("moneyPercent", np.nan),
            an_status=g.get("status", ""),
            an_start=g.get("startTime", ""),
            fetched_at=stamp,
        ))
    return pd.DataFrame(rows)


def upsert(new):
    """Replace this season/week's rows in data/action_splits.csv."""
    if os.path.exists(OUT):
        old = pd.read_csv(OUT)
        done = set(zip(new.season, new.week))
        old = old[[(s, w) not in done for s, w in zip(old.season, old.week)]]
        new = pd.concat([old, new], ignore_index=True)
    new = new.sort_values(["season", "week", "home"]).reset_index(drop=True)
    new.to_csv(OUT, index=False)


def fill_signals(target, an, season=None, overwrite=False):
    """Fill open_line / bets % / money % cells in `target` from `an`
    (action_splits rows). Returns the number of games touched.

    Default: empty cells only. overwrite=True makes Action Network the source
    for every game it has a number for (consistent across weeks); the values
    it replaces are kept in signal_notes.

    Bets % and money % are written only as a pair, so one game's split never
    mixes Action Network with a number you typed in from another book.
    `season` restricts matching when `target` has no season column (the workbook).
    """
    if overwrite:
        return _overwrite_signals(target, an, season)
    for c in ("open_line", "home_bets_pct", "home_money_pct", "signal_notes"):
        if c not in target.columns:
            target[c] = np.nan
    target["signal_notes"] = target["signal_notes"].astype("object")
    lookup = {(int(r.season), int(r.week), r.away, r.home): r for r in an.itertuples()}
    filled = 0
    for i, r in target.iterrows():
        s = int(r.season) if "season" in target.columns else season
        a = lookup.get((s, int(r.week), canon(r.away), canon(r.home)))
        if a is None:
            continue
        touched = False
        if pd.isna(r.open_line) and pd.notna(a.an_open_line):
            target.at[i, "open_line"] = a.an_open_line
            touched = True
        if (pd.isna(r.home_bets_pct) and pd.isna(r.home_money_pct)
                and pd.notna(a.an_home_bets_pct) and pd.notna(a.an_home_money_pct)):
            target.at[i, "home_bets_pct"] = a.an_home_bets_pct
            target.at[i, "home_money_pct"] = a.an_home_money_pct
            touched = True
        if touched:
            note = f"Action Network splits {str(a.fetched_at)[:16]}"
            old = target.at[i, "signal_notes"]
            target.at[i, "signal_notes"] = f"{old}; {note}" if pd.notna(old) else note
            filled += 1
    return filled


def _fmt(v):
    return "-" if pd.isna(v) else f"{v:g}"


def _overwrite_signals(target, an, season):
    for c in ("open_line", "home_bets_pct", "home_money_pct", "signal_notes"):
        if c not in target.columns:
            target[c] = np.nan
    target["signal_notes"] = target["signal_notes"].astype("object")
    lookup = {(int(r.season), int(r.week), r.away, r.home): r for r in an.itertuples()}
    changed = 0
    for i, r in target.iterrows():
        s = int(r.season) if "season" in target.columns else season
        a = lookup.get((s, int(r.week), canon(r.away), canon(r.home)))
        if a is None:
            continue
        note = str(r.signal_notes) if pd.notna(r.signal_notes) else ""
        prior_was = ""
        if "Action Network" in note:
            old_tag = note[note.index("Action Network"):]
            if "(was " in old_tag:   # keep the original values from an earlier run
                prior_was = old_tag[old_tag.index("(was "):].split(")")[0] + ")"
            note = note[:note.index("Action Network")].rstrip("; ")
        replaced = []
        if pd.notna(a.an_open_line):
            if pd.notna(r.open_line) and r.open_line != a.an_open_line:
                replaced.append(f"open {_fmt(r.open_line)}")
            target.at[i, "open_line"] = a.an_open_line
        if pd.notna(a.an_home_bets_pct) and pd.notna(a.an_home_money_pct):
            if ((pd.notna(r.home_bets_pct) or pd.notna(r.home_money_pct))
                    and (r.home_bets_pct, r.home_money_pct)
                    != (a.an_home_bets_pct, a.an_home_money_pct)):
                replaced.append(f"bets {_fmt(r.home_bets_pct)}/money {_fmt(r.home_money_pct)}")
            target.at[i, "home_bets_pct"] = a.an_home_bets_pct
            target.at[i, "home_money_pct"] = a.an_home_money_pct
        tag = f"Action Network {str(a.fetched_at)[:16]}"
        if replaced:
            tag += f" (was {', '.join(replaced)})"
        elif prior_was:
            tag += f" {prior_was}"
        target.at[i, "signal_notes"] = f"{note}; {tag}" if note else tag
        changed += 1
    return changed


def to_workbook(df, week, season):
    """Fill one freshly fetched week into weekly_lines.xlsx."""
    wb = pd.read_excel(WORKBOOK)
    n = fill_signals(wb, df[df.week == week], season=season)
    wb.to_excel(WORKBOOK, index=False)
    print(f"  workbook: filled {n} week-{week} games (existing cells untouched)")


def import_saved(season, weeks=None, overwrite=False):
    """Copy already-fetched splits (data/action_splits.csv) into the workbook
    and the pool picks log. No API call, no cost.

    The workbook only holds the current season, so `season` picks which rows
    of action_splits.csv apply to it. The log is filled too so that
    pool_tracker.py report sees weeks you already submitted.
    """
    if not os.path.exists(OUT):
        raise SystemExit(f"{OUT} not found -- fetch first")
    an = pd.read_csv(OUT)
    an = an[an.season == season]
    if weeks:
        an = an[an.week.isin(weeks)]
    if an.empty:
        raise SystemExit(f"nothing saved for {season}" + (f" weeks {weeks}" if weeks else ""))

    wb = pd.read_excel(WORKBOOK)
    n = fill_signals(wb, an, season=season, overwrite=overwrite)
    wb.to_excel(WORKBOOK, index=False)
    print(f"workbook: {n} games from {OUT}")

    if os.path.exists(LOG):
        log = pd.read_csv(LOG)
        m = log.season == season
        part = log[m].copy()
        n = fill_signals(part, an, overwrite=overwrite)
        log = pd.concat([log[~m], part]).sort_index()
        log.to_csv(LOG, index=False)
        print(f"{LOG}: {n} games")
    print("Action Network overwrote splits/open lines it had; replaced values are in signal_notes"
          if overwrite else "existing cells were left alone; notes mark every Action Network fill")


def main():
    if len(sys.argv) < 3:
        raise SystemExit(__doc__)
    if sys.argv[1] == "import":
        args = [a for a in sys.argv[3:] if not a.startswith("--")]
        weeks = parse_weeks(args[0]) if args else None
        import_saved(int(sys.argv[2]), weeks, overwrite="--overwrite" in sys.argv)
        return
    season, weeks = int(sys.argv[1]), parse_weeks(sys.argv[2])
    from_raw = "--from-raw" in sys.argv

    for wk in weeks:
        if from_raw:
            with open(raw_path(season, wk)) as f:
                items = json.load(f)
        else:
            items = fetch(season, wk)
        df = parse(items, season, wk)
        if df.empty:
            print(f"{season} wk{wk}: no games returned")
            continue
        upsert(df)
        have = df.an_home_bets_pct.notna().sum()
        print(f"{season} wk{wk}: {len(df)} games, {have} with splits -> {OUT}")
        if "--to-workbook" in sys.argv:
            to_workbook(df, wk, season)


if __name__ == "__main__":
    main()
