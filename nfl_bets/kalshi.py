"""
Kalshi NFL market data (public API, no key needed).

    python3 kalshi.py 4            # snapshot every week-4 win / spread / total market
    python3 kalshi.py 4 --show     # ...and print the win + main spread markets

Each run saves a timestamped snapshot to data/kalshi/<season>_wk<NN>_<UTC time>.csv,
so you can take one when you build the bet sheet and another just before kickoff
(that later one is the "close" for closing-line value).

Series used:
    KXNFLGAME    "<team> wins"                       (one market per team)
    KXNFLSPREAD  "<team> wins by over <strike>"      (a ladder of strikes per team)
    KXNFLTOTAL   "over <strike> points scored"

Prices are dollars (0-1). Fees are NOT included here; see bets.py.
"""

import json
import os
import sys
import time
import urllib.parse
import urllib.request
from datetime import datetime, timezone

import pandas as pd

from common import SEASON, canon, schedule

API = "https://api.elections.kalshi.com/trade-api/v2"
SERIES = {"win": "KXNFLGAME", "spread": "KXNFLSPREAD", "total": "KXNFLTOTAL"}
SNAP_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data", "kalshi")


def _get(path, params=None, tries=3):
    url = f"{API}{path}" + (f"?{urllib.parse.urlencode(params)}" if params else "")
    for i in range(tries):
        try:
            req = urllib.request.Request(url, headers={"Accept": "application/json",
                                                       "User-Agent": "fantasy_gpx-nfl_bets"})
            with urllib.request.urlopen(req, timeout=20) as r:
                return json.load(r)
        except Exception as e:                       # network blip / rate limit
            if i == tries - 1:
                raise RuntimeError(f"Kalshi API failed: {url}\n{e}")
            time.sleep(1.5 * (i + 1))


def markets(series_ticker, status="open"):
    """Every market in a series, following the pagination cursor."""
    out, cursor = [], None
    while True:
        p = {"series_ticker": series_ticker, "status": status, "limit": 1000}
        if cursor:
            p["cursor"] = cursor
        d = _get("/markets", p)
        out += d.get("markets", [])
        cursor = d.get("cursor")
        if not cursor:
            return out


def _f(x):
    try:
        return float(x)
    except (TypeError, ValueError):
        return float("nan")


def _event_teams(event_ticker, sched):
    """'KXNFLGAME-26OCT04JACCIN' -> (gameday, away, home) matched to the schedule."""
    code = event_ticker.split("-", 1)[1]              # 26OCT04JACCIN
    date = datetime.strptime(code[:7], "%y%b%d").date().isoformat()
    teams = code[7:]
    for r in sched[sched.gameday == date].itertuples():
        for a in {r.away, r.away_k}:
            for h in {r.home, r.home_k}:
                if teams == a + h:
                    return date, r.away, r.home
    return date, None, None


def snapshot(week, season=SEASON, save=True):
    """All open week-`week` Kalshi NFL markets as one tidy DataFrame."""
    sched = schedule(season, week)
    # Kalshi's own abbreviations, where they differ from ours
    to_k = {"JAX": "JAC", "LAR": "LAR", "WAS": "WAS", "LV": "LV"}
    sched["away_k"] = sched.away.map(lambda t: to_k.get(t, t))
    sched["home_k"] = sched.home.map(lambda t: to_k.get(t, t))
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    rows, unmatched = [], set()
    for kind, series in SERIES.items():
        for m in markets(series):
            date, away, home = _event_teams(m["event_ticker"], sched)
            if away is None:
                unmatched.add(m["event_ticker"])
                continue
            suffix = m["ticker"].rsplit("-", 1)[1]            # CIN / NO8 / 55
            team = canon(suffix.rstrip("0123456789")) if kind != "total" else ""
            rows.append(dict(
                fetched_at=stamp, season=season, week=week, gameday=date,
                away=away, home=home, kind=kind, team=team,
                strike=_f(m.get("floor_strike")) if kind != "win" else float("nan"),
                yes_bid=_f(m.get("yes_bid_dollars")), yes_ask=_f(m.get("yes_ask_dollars")),
                last=_f(m.get("last_price_dollars")),
                volume=_f(m.get("volume_fp") or m.get("volume")),
                open_interest=_f(m.get("open_interest_fp") or m.get("open_interest")),
                ticker=m["ticker"], close_time=m.get("close_time"),
            ))
    df = pd.DataFrame(rows)
    if save and len(df):
        os.makedirs(SNAP_DIR, exist_ok=True)
        path = os.path.join(SNAP_DIR, f"{season}_wk{week:02d}_{stamp.replace(':', '')}.csv")
        df.to_csv(path, index=False)
        print(f"saved {len(df)} markets for {df[['away','home']].drop_duplicates().shape[0]}"
              f" games -> {os.path.relpath(path)}")
    missing = sched[~sched.set_index(["away", "home"]).index.isin(
        df.set_index(["away", "home"]).index if len(df) else [])]
    if len(missing):
        print("  no Kalshi markets found for: "
              + ", ".join(f"{a} @ {h}" for a, h in zip(missing.away, missing.home)))
    return df


def snapshots(week, season=SEASON):
    """Paths of every saved snapshot for a week, oldest first."""
    if not os.path.isdir(SNAP_DIR):
        return []
    pre = f"{season}_wk{week:02d}_"
    return sorted(os.path.join(SNAP_DIR, f) for f in os.listdir(SNAP_DIR) if f.startswith(pre))


def latest(week, season=SEASON):
    s = snapshots(week, season)
    return pd.read_csv(s[-1]) if s else None


if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise SystemExit(__doc__)
    wk = int(sys.argv[1])
    df = snapshot(wk)
    if "--show" in sys.argv and len(df):
        w = df[df.kind == "win"].copy()
        w["mid"] = (w.yes_bid + w.yes_ask) / 2
        print(w[["away", "home", "team", "yes_bid", "yes_ask", "mid"]].to_string(index=False))
