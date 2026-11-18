"""
Injury check for ELWAY's game bets.

ELWAY posts its numbers on Tuesday, before the week's injury reports. By
kickoff the market has priced the injuries ELWAY hasn't, so a side that is
missing starters can look like an edge when it isn't. Through week 9 of 2026,
recommended bets on a side missing 2+ more starters than its opponent went
1-8 (ELWAY expected 4.3 wins), and the closing line sat about 1.5 points off
ELWAY's per starting QB out and 0.3 per other starter.

    python3 injuries.py 10        this week's flags, team by team

A side is FLAGGED when, on nflverse's injury report for that week:
  - its starting QB is Out or Doubtful (and the opponent's isn't), or
  - it has STARTER_GAP or more starters Out/Doubtful than its opponent.
A starter is a player who averaged STARTER_SHARE+ of his side's snaps
(offense or defense) in the team's earlier games this season. Questionable
players don't count. The report fills in Wednesday through Friday, so the
check means most from Friday on: hq.py reruns it every time it builds the
dashboard.
"""

import os
import sys
import time

import numpy as np
import pandas as pd

from common import DATA, SEASON, canon

RELEASES = "https://github.com/nflverse/nflverse-data/releases/download"
CACHE = os.path.join(DATA, "injuries")
MAX_AGE_HOURS = 3              # re-download the report when the cache is older
STARTER_SHARE = 0.6            # avg snap share that makes a player a starter
STARTER_GAP = 2                # this many more starters out than the opponent -> flag
OUT = ("Out", "Doubtful")


def _get(name, url, refresh):
    """Download to the cache when missing or stale (refresh=True), else read the cache."""
    path = os.path.join(CACHE, name)
    stale = not os.path.exists(path) or time.time() - os.path.getmtime(path) > MAX_AGE_HOURS * 3600
    if refresh and stale:
        os.makedirs(CACHE, exist_ok=True)
        df = pd.read_csv(url, low_memory=False)
        df.to_csv(path, index=False)
    return pd.read_csv(path, low_memory=False)


def load(season=SEASON, refresh=True):
    """nflverse's injury reports, snap counts and player ids for a season."""
    inj = _get(f"injuries_{season}.csv", f"{RELEASES}/injuries/injuries_{season}.csv", refresh)
    snaps = _get(f"snap_counts_{season}.csv", f"{RELEASES}/snap_counts/snap_counts_{season}.csv", refresh)
    ids = _get("players.csv", f"{RELEASES}/players/players.csv", refresh)[["gsis_id", "pfr_id"]].dropna()
    return inj, snaps, ids


def team_report(week, season=SEASON, data=None, refresh=True):
    """One row per team on that week's report: starters Out/Doubtful (non-QB) and
    whether the starting QB is out, with names."""
    inj, snaps, ids = data if data is not None else load(season, refresh)
    r = inj[(inj.season == season) & (inj.week == week) & inj.report_status.isin(OUT)].copy()
    cols = ["team", "qb_out", "qb", "starters_out", "starters"]
    if r.empty:
        return pd.DataFrame(columns=cols)
    r["team"] = r.team.map(canon)
    r = r.merge(ids, on="gsis_id", how="left")
    s = snaps[(snaps.season == season) & (snaps.week < week)].copy()
    s["share"] = s[["offense_pct", "defense_pct"]].max(axis=1)
    share = s.groupby("pfr_player_id").share.mean()
    r["share"] = r.pfr_id.map(share).fillna(0.0)
    r["starter"] = r.share >= STARTER_SHARE
    rows = []
    for team, d in r[r.starter].groupby("team"):
        qb, other = d[d.position == "QB"], d[d.position != "QB"]
        rows.append(dict(team=team, qb_out=len(qb) > 0, qb=", ".join(qb.full_name),
                         starters_out=len(other),
                         starters=", ".join(f"{n} ({p})" for n, p in zip(other.full_name, other.position))))
    return pd.DataFrame(rows, columns=cols)


def check(team, opp, rep):
    """(flagged, note) for a bet backing `team` against `opp`, from team_report()."""
    get = lambda t: rep[rep.team == t].iloc[0] if (rep.team == t).any() else None
    a, b = get(team), get(opp)
    qa, qb = bool(a is not None and a.qb_out), bool(b is not None and b.qb_out)
    na, nb = (int(a.starters_out) if a is not None else 0), (int(b.starters_out) if b is not None else 0)
    reasons = []
    if qa and not qb:
        reasons.append(f"{team} QB out ({a.qb})")
    if na - nb >= STARTER_GAP:
        reasons.append(f"{team} missing {na} starters to {opp}'s {nb}: {a.starters}")
    return bool(reasons), "; ".join(reasons)


def backs(b):
    """The team a logged bet wins with (a Kalshi spread NO backs the opponent)."""
    if b["instrument"] == "kalshi_spread" and b["side"] == "no":
        return b["away"] if b["team"] == b["home"] else b["home"]
    return b["team"]


def flag_bets(bets, week, season=SEASON, data=None, refresh=True):
    """Add inj_flag / inj_note to a frame of bets (needs team, side, instrument, home, away)."""
    rep = team_report(week, season, data, refresh)
    out = bets.copy()
    res = []
    for _, b in out.iterrows():
        if b["instrument"] == "pair":
            res.append((False, ""))
            continue
        t = backs(b)
        res.append(check(t, b["away"] if t == b["home"] else b["home"], rep))
    out["inj_flag"] = [f for f, _ in res]
    out["inj_note"] = [n for _, n in res]
    return out


def main(week):
    rep = team_report(week)
    if rep.empty:
        print(f"week {week}: no starters Out/Doubtful on nflverse's report yet "
              "(it fills in Wednesday through Friday)")
        return
    print(f"week {week}: starters Out/Doubtful (averaged {STARTER_SHARE:.0%}+ of snaps), from nflverse's report")
    for r in rep.sort_values(["qb_out", "starters_out"], ascending=False).itertuples():
        qb = f"QB {r.qb}; " if r.qb_out else ""
        print(f"  {r.team:<4}{qb}{r.starters_out} other: {r.starters or '-'}")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise SystemExit(__doc__)
    main(int(sys.argv[1]))
