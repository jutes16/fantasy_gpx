"""
ELWAY's weekly projections from its public feed (the Silver Bulletin embed's
data), saved where bets.py and import_elway.py read them. No browser, no paste.

    python3 elway_pull.py           # the week the feed is on
    python3 elway_pull.py 9         # a given week (refused if the feed isn't on it)

Writes, for week NN:
  data/elway/<season>_wkNN.txt              the ELWAY table (bets.py)
  data/elway/dist_<season>_wkNN.csv         simulated home-margin distribution per game
  ../nfl_model/data/elway/<season>_wkNN.txt the same table (python3 import_elway.py NN)

Mid-week updates drop the games already played; their rows from the earlier pull
are kept. The files depend only on the feed's contents (no pull time), so two
pulls of the same feed version give identical files and the same ELWAY
fingerprint in bets.py. Prints "unchanged" when nothing changed.

    python3 elway_pull.py --cloud <dir>   # the cloud job (elway.yml in the data repo): when
                                          # ELWAY changed, also price the bet sheet now and
                                          # save it to <dir>; sync_data.py logs it on the laptop
"""

import io
import os
import sys
import time
import urllib.request

import pandas as pd

from common import DATA, SEASON, canon

FEED = "https://silver-bulletin.github.io/elway-embed-data/data/latest/"
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(DATA, "elway")
POOL_OUT = os.path.join(HERE, "..", "nfl_model", "data", "elway")


def _get(name):
    req = urllib.request.Request(f"{FEED}{name}?_={int(time.time())}", headers={"User-Agent": "fantasy_gpx"})
    return urllib.request.urlopen(req, timeout=30).read()


def _meta(name):
    m = pd.read_csv(io.BytesIO(_get(f"{name}-meta.csv")))
    return dict(zip(m.key, m.value))


def fetch():
    """(games, margins, updated_at) straight from the feed."""
    games = pd.read_csv(io.BytesIO(_get("elway-margin-games.csv")))
    margins = pd.read_csv(io.BytesIO(_get("elway-margins.csv")))
    return games, margins, _meta("elway-margins")["updated_at"]


def build(games, margins, week=None):
    """The week's table rows and distribution from the feed's two files.
    week=None: the week whose games the margins cover."""
    games = games.assign(home=games.home_sb_fran_id.map(canon), away=games.away_sb_fran_id.map(canon))
    m = margins.assign(away=margins.matchup.str.split(" @ ").str[0].map(canon),
                       home=margins.matchup.str.split(" @ ").str[1].map(canon))
    pairs = set(zip(m.away, m.home))
    on = games[[(a, h) in pairs for a, h in zip(games.away, games.home)]]
    if on.empty:
        raise SystemExit("the feed's margins match no scheduled game")
    feed_week = int(on.week.min())
    if week is not None and week != feed_week:
        raise SystemExit(f"the feed is on week {feed_week}, not week {week}")
    g = on[on.week == feed_week].copy()
    m = m[[(a, h) in set(zip(g.away, g.home)) for a, h in zip(m.away, m.home)]]
    pct = m.home_prob.fillna(m.away_prob).fillna(m.tie_prob)      # each margin's share sits in one column
    dist = pd.DataFrame(dict(season=SEASON, week=feed_week, away=m.away, home=m.home,
                             home_margin=m.margin.astype(int), prob=(pct / 100).round(10)))
    dist = dist[dist.prob > 0].sort_values(["away", "home", "home_margin"])
    return feed_week, g, dist


def _spread(x):
    return "PK" if x == 0 else f"{x:+g}"


def table_lines(g):
    return [f"{int(r.week)} {'N ' if str(r.neutral).lower() in ('yes', 'true', '1') else ''}"
            f"{r.home} {r.pf_home:.1f} {r.win_home:.2f}% {r.away} {r.pf_away:.1f} {r.win_away:.2f}% "
            f"{_spread(float(r.point_spread))} {float(r.total):g}" for r in g.itertuples()]


def _key(line):
    p = line.split()
    p = [x for x in p if x != "N"]
    return p[1], p[4]                                   # home, away


def pull(week=None, out=OUT, pool_out=POOL_OUT):
    """Save the week's files; returns (week, changed)."""
    games, margins, updated = fetch()
    wk, g, dist = build(games, margins, week)
    txt_path = os.path.join(out, f"{SEASON}_wk{wk:02d}.txt")
    dist_path = os.path.join(out, f"dist_{SEASON}_wk{wk:02d}.csv")
    lines = table_lines(g)
    if os.path.exists(txt_path):                         # keep games the feed has dropped (already played)
        new = {_key(x) for x in lines}
        old = [x for x in open(txt_path).read().splitlines() if x and not x.startswith("#")]
        lines = [x for x in old if _key(x) not in new] + lines
    if os.path.exists(dist_path):
        old = pd.read_csv(dist_path)
        keep = ~pd.Series(list(zip(old.away, old.home))).isin(set(zip(dist.away, dist.home))).values
        dist = pd.concat([old[keep], dist]).sort_values(["away", "home", "home_margin"])
    text = "\n".join([f"# ELWAY margin-of-victory feed, updated {updated}"] + lines) + "\n"
    body = dist.to_csv(index=False, float_format="%.10g")
    changed = not (os.path.exists(txt_path) and open(txt_path).read() == text
                   and os.path.exists(dist_path) and open(dist_path).read() == body)
    if changed:
        for d in (out, pool_out):
            os.makedirs(d, exist_ok=True)
        open(txt_path, "w").write(text)
        open(dist_path, "w").write(body)
        open(os.path.join(pool_out, f"{SEASON}_wk{wk:02d}.txt"), "w").write(text)
    print(f"ELWAY week {wk} (feed updated {updated}): {len(g)} games in the feed, "
          f"{len(lines)} in the file -- {'saved' if changed else 'unchanged'}")
    return wk, changed


if __name__ == "__main__":
    a = sys.argv[1:]
    cloud = a[a.index("--cloud") + 1] if "--cloud" in a else None
    nums = [x for x in a if x.isdigit()]
    wk, changed = pull(int(nums[0]) if nums else None)
    if cloud and changed:
        import bets
        from datetime import datetime, timezone
        ts = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H%M%SZ")
        bets.sheet(wk, export=os.path.join(cloud, f"bets_{SEASON}_wk{wk:02d}_{ts}.csv"))
