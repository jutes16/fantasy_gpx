"""
Pre-kickoff snapshots that can't wait for a laptop to be awake, run by GitHub
Actions in the private data repo (jutes16/fantasy_gpx-data, snapshots.yml) with
this repo checked out beside it. Three modes:

  --full     Thursday 4 pm ET: buy player props for every game this week that
             hasn't started (The Odds API, ~6 credits a game, ~84 a week) and
             save Sleeper's projections for those players
  --recheck  Sunday 10 am ET: fresh prices on just this week's PICKs (only the
             picks' markets, only games not started: ~10-20 credits), saved to
             props/<season>_wk<NN>_recheck/ so the dashboard shows Sunday's line
  (none)     every 30 min on game days: a Kalshi snapshot when a game kicks off
             within WINDOW hours (the last one before kickoff is the close); and,
             as a safety net, props for any of those games Thursday missed
             (books post some late), with Sleeper's projections for their players

About 100 credits a week of the free tier's 500 a month. Nothing is logged or
graded here: locally, `python3 sync_data.py` copies the files in.

    python3 snapshot.py --full | --recheck | (nothing)
"""

import json
import os
import sys

import pandas as pd
import requests

import kalshi
import props
from common import SEASON, schedule
from props import fp
from weekly_stats import _normalize_name

WINDOW = 2.5        # hours: with runs every 30 min, each game is bought 2-2.5 h before kickoff


def current_week():
    s = requests.get("https://api.sleeper.app/v1/state/nfl", timeout=30).json()
    return int(s["week"]) if s.get("season_type") == "regular" else None


def kicking_off(sched, now, window):
    """The games that start after `now` and within `window` hours of it."""
    ko = pd.to_datetime(sched.kickoff, utc=True)
    return sched[(ko > now) & (ko <= now + pd.Timedelta(hours=window))]


def _game(e):
    return f"{fp.odds_api._code(e['away_team'])} @ {fp.odds_api._code(e['home_team'])}"


def _save_sleeper(week, events):
    off = props.offers(events)
    if len(off):
        keys = set(off.player.map(_normalize_name))
        props.save_snapshot(props.sleeper_means(week), keys, week)
        print(f"Sleeper projections saved for {len(keys)} players")


def full(week):
    """Thursday: props for every game this week that hasn't started."""
    _save_sleeper(week, fp.fetch_props(week))


def recheck(week):
    """Sunday morning: fresh prices for this week's picks, picks' markets only."""
    saved = os.path.join(fp.PROPS_DIR, f"{SEASON}_wk{week:02d}")
    if not os.path.isdir(saved) or not any(f.endswith(".json") for f in os.listdir(saved)):
        print(f"week {week}: no props saved yet (Thursday's pull), so no picks to re-check")
        return
    d = props.sheet(week, cached=True, log=False)           # picks from the saved props + Sleeper snapshot
    now = pd.Timestamp.now(tz="UTC")
    picks = d[d.pick & (pd.to_datetime(d.kickoff, utc=True) > now)]
    if picks.empty:
        print("no upcoming picks to re-check")
        return
    want = picks.groupby("game").market.apply(lambda m: sorted(set(m))).to_dict()
    out = os.path.join(fp.PROPS_DIR, f"{SEASON}_wk{week:02d}_recheck")
    os.makedirs(out, exist_ok=True)
    events, _, left = fp.odds_api.get(f"/sports/{fp.odds_api.SPORT}/events")
    used = 0
    for e in events:
        g = _game(e)
        path = os.path.join(out, g.replace(" @ ", "_") + ".json")
        if g not in want or os.path.exists(path) or pd.Timestamp(e["commence_time"]) <= now:
            continue
        data, last, left = fp.odds_api.get(f"/sports/{fp.odds_api.SPORT}/events/{e['id']}/odds",
                                           dict(regions="us", markets=",".join(want[g]), oddsFormat="american"))
        data["_fetched_at"] = now.isoformat(timespec="seconds")
        json.dump(data, open(path, "w"))
        used += int(last or 0)
    print(f"re-checked {len(picks)} picks in {len(want)} games ({used} credits, {left} left this month)")


def main(window=WINDOW):
    week = current_week()
    if week is None:
        print("not the regular season: nothing to snapshot")
        return
    now = pd.Timestamp.now(tz="UTC")
    soon = kicking_off(schedule(SEASON, week), now, window)
    if soon.empty:
        print(f"week {week}: no game kicks off in the next {window} h")
        return
    games = set(soon.away + " @ " + soon.home)
    print(f"week {week}: {len(games)} game(s) within {window} h: {', '.join(sorted(games))}")

    # safety net: buys only games Thursday's pull didn't get; refreshes their players' Sleeper snapshot
    _save_sleeper(week, [e for e in fp.fetch_props(week, window=window) if _game(e) in games])
    kalshi.snapshot(week)


if __name__ == "__main__":
    a = sys.argv[1:]
    if "--full" in a or "--recheck" in a:
        wk = current_week()
        if wk is None:
            raise SystemExit("not the regular season")
        full(wk) if "--full" in a else recheck(wk)
    else:
        main(float(a[a.index("--window") + 1]) if "--window" in a else WINDOW)
