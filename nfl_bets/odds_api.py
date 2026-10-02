"""
The Odds API: every US sportsbook's NFL prices, for best-price betting and
player props.

Needs ODDS_API_KEY in your environment (e.g. in ~/.zshrc). Never in the repo.

    python3 odds_api.py 4                 # snapshot week-4 game odds from every book
    python3 odds_api.py 4 --show          # ...and print the best price per side

Credits (free tier: 500 a month): game odds cost 1 per market, so a snapshot
(moneyline + spread + total) is 3. Player props cost 1 per market per game
(see ../nfl/fantasy_props.py). Every response is saved under data/odds/, and
each call prints the credits used and remaining.

Lines are converted to this project's home-perspective convention
(negative = home favoured).
"""

import json
import os
import sys
import time
import urllib.parse
import urllib.request
from datetime import datetime, timezone

import pandas as pd

from common import DATA, SEASON, canon, schedule

API = "https://api.the-odds-api.com/v4"
SPORT = "americanfootball_nfl"
ODDS_DIR = os.path.join(DATA, "odds")

TEAM_CODES = {
    "Arizona Cardinals": "ARI", "Atlanta Falcons": "ATL", "Baltimore Ravens": "BAL",
    "Buffalo Bills": "BUF", "Carolina Panthers": "CAR", "Chicago Bears": "CHI",
    "Cincinnati Bengals": "CIN", "Cleveland Browns": "CLE", "Dallas Cowboys": "DAL",
    "Denver Broncos": "DEN", "Detroit Lions": "DET", "Green Bay Packers": "GB",
    "Houston Texans": "HOU", "Indianapolis Colts": "IND", "Jacksonville Jaguars": "JAX",
    "Kansas City Chiefs": "KC", "Las Vegas Raiders": "LV", "Los Angeles Chargers": "LAC",
    "Los Angeles Rams": "LAR", "Miami Dolphins": "MIA", "Minnesota Vikings": "MIN",
    "New England Patriots": "NE", "New Orleans Saints": "NO", "New York Giants": "NYG",
    "New York Jets": "NYJ", "Philadelphia Eagles": "PHI", "Pittsburgh Steelers": "PIT",
    "San Francisco 49ers": "SF", "Seattle Seahawks": "SEA", "Tampa Bay Buccaneers": "TB",
    "Tennessee Titans": "TEN", "Washington Commanders": "WAS",
}


def _key():
    k = os.environ.get("ODDS_API_KEY")
    if not k:
        raise SystemExit("ODDS_API_KEY is not set (add it to ~/.zshrc, never to the repo)")
    return k


def get(path, params=None, tries=3):
    """GET an Odds API endpoint. Returns (json, credits_used_by_call, remaining)."""
    q = dict(params or {}, apiKey=_key())
    url = f"{API}{path}?{urllib.parse.urlencode(q)}"
    for i in range(tries):
        try:
            with urllib.request.urlopen(urllib.request.Request(url), timeout=30) as r:
                last = r.headers.get("x-requests-last")
                left = r.headers.get("x-requests-remaining")
                return json.load(r), last, left
        except urllib.error.HTTPError as e:
            if e.code in (401, 422, 429) or i == tries - 1:
                raise SystemExit(f"The Odds API {e.code}: {e.read().decode()[:300]}")
        except Exception:
            if i == tries - 1:
                raise
        time.sleep(2 * (i + 1))


def _code(name):
    return canon(TEAM_CODES.get(name, name))


def snapshot(week, markets=("h2h", "spreads", "totals"), season=SEASON):
    """Every book's current odds for the week's games -> tidy DataFrame, saved."""
    data, last, left = get(f"/sports/{SPORT}/odds", dict(
        regions="us", markets=",".join(markets), oddsFormat="american"))
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    os.makedirs(ODDS_DIR, exist_ok=True)
    raw = os.path.join(ODDS_DIR, f"{season}_wk{week:02d}_{stamp.replace(':', '')}.json")
    json.dump(data, open(raw, "w"))
    df = tidy(data, week, season, stamp)
    print(f"odds snapshot: {df[['away', 'home']].drop_duplicates().shape[0]} week-{week} games, "
          f"{df.book.nunique()} books -> {os.path.relpath(raw)}   (credits used {last}, left {left})")
    return df


def tidy(data, week, season=SEASON, stamp=""):
    """Raw /odds response -> one row per game x book x market x side, week only."""
    sched = schedule(season, week, refresh=False)
    games = set(zip(sched.away, sched.home))
    rows = []
    for ev in data:
        home, away = _code(ev["home_team"]), _code(ev["away_team"])
        if (away, home) not in games:
            continue
        for bk in ev.get("bookmakers", []):
            for mk in bk.get("markets", []):
                for o in mk.get("outcomes", []):
                    side = ("home" if _code(o["name"]) == home else "away") \
                        if mk["key"] in ("h2h", "spreads") else o["name"].lower()
                    point = o.get("point")
                    if mk["key"] == "spreads" and point is not None and side == "away":
                        point = -point                     # store as the home line
                    rows.append(dict(season=season, week=week, away=away, home=home,
                                     book=bk["title"], market=mk["key"], side=side,
                                     point=point, price=o["price"], fetched_at=stamp,
                                     book_updated=bk.get("last_update")))
    return pd.DataFrame(rows)


def latest(week, season=SEASON):
    """The most recent saved snapshot for a week, or None."""
    if not os.path.isdir(ODDS_DIR):
        return None
    pre = f"{season}_wk{week:02d}_"
    files = sorted(f for f in os.listdir(ODDS_DIR) if f.startswith(pre) and f.endswith(".json"))
    if not files:
        return None
    stamp = files[-1][len(pre):-5]
    stamp = f"{stamp[:13]}:{stamp[13:15]}:{stamp[15:]}"
    return tidy(json.load(open(os.path.join(ODDS_DIR, files[-1]))), week, season, stamp)


def best_moneylines(df):
    """Best (highest-paying) moneyline per game and side, with the book."""
    ml = df[df.market == "h2h"]
    i = ml.groupby(["away", "home", "side"]).price.idxmax()
    return ml.loc[i, ["away", "home", "side", "book", "price"]].reset_index(drop=True)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise SystemExit(__doc__)
    wk = int(sys.argv[1])
    d = snapshot(wk)
    if "--show" in sys.argv and len(d):
        b = best_moneylines(d)
        print(b.to_string(index=False))
