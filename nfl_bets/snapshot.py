"""
Pre-kickoff snapshots that can't wait for a laptop to be awake. Runs every half
hour on game days in GitHub Actions, in the private data repo
(jutes16/fantasy_gpx-data, workflow snapshots.yml), with this repo checked out
beside it. Each run, for the games kicking off within WINDOW hours:

  props   fetch their player props (The Odds API, about 6 credits a game; a
          saved game is never bought again) and save Sleeper's projections for
          their players: the pre-kickoff snapshot props.py grades against
  kalshi  a snapshot of the week's Kalshi markets; the last one before each
          kickoff is that game's close for closing-line value

Nothing is logged or graded here. Locally, `python3 sync_data.py` copies the
files in, then `props.py sheet <wk> --cached`, `bets.py grade` and `hq.py` work
as before.

    python3 snapshot.py              # what the scheduled run does
    python3 snapshot.py --window 3   # hours ahead (default 2.5)
"""

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

    events = [e for e in fp.fetch_props(week, window=window)
              if f"{fp.odds_api._code(e['away_team'])} @ {fp.odds_api._code(e['home_team'])}" in games]
    off = props.offers(events)
    if len(off):
        keys = set(off.player.map(_normalize_name))
        props.save_snapshot(props.sleeper_means(week), keys, week)
        print(f"Sleeper projections saved for {len(keys)} players")
    else:
        print("no props posted for these games yet")

    kalshi.snapshot(week)


if __name__ == "__main__":
    a = sys.argv[1:]
    main(float(a[a.index("--window") + 1]) if "--window" in a else WINDOW)
