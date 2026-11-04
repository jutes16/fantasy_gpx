"""
Play-by-play -> one row per team per game, on a points-per-DRIVE basis.

This is the input layer for elway_lite.py. It follows ELWAY's 2026 accounting:
offense, defense and special teams are kept apart, so a pick-six counts
against the offense that threw it and for the defense that returned it, never
as an offensive score.

Data: nflverse play-by-play, one file per season (2001 onward, when drive
data becomes reliable). Fetch once, then the current season each week:
    python3 drives.py fetch 2001-2026     <- play-by-play (+ FTN charting 2022 on)
    python3 drives.py fetch 2026          <- refresh this season
    python3 drives.py build               <- writes data/drives.csv

Scoring events (a touchdown counts 7, ignoring the conversion, which is noise):
    off   TD by the team with the ball on a scrimmage play; field goals
    def   interception/fumble return TDs; safeties (2 to the defense)
    st    kickoff and punt return TDs, blocked kick returns

Per drive, two adjustments are recorded (both fitted league-wide):
    script_adj   points a drive "should" lose or gain from the score and
                 clock, e.g. a team up 14 in the 4th quarter runs, not scores
    ep_start     expected points from the starting field position

Rump drives (starting with under RUMP_SECS left in a half) and kneel-only
drives are flagged and left out of the efficiency counts, but still count
toward pace.
"""

import os
import subprocess
import sys

import numpy as np
import pandas as pd

PBP_DIR = "data/pbp"
OUT = "data/drives.csv"
RELEASES = "https://github.com/nflverse/nflverse-data/releases/download"
URL = RELEASES + "/pbp/play_by_play_{y}.csv.gz"
FTN_URL = RELEASES + "/ftn_charting/ftn_charting_{y}.csv"   # drops, sneaks: 2022 on

RUMP_SECS = 120      # drives starting with < 2:00 in the half are "rump"
TD_PTS = 7
SCRIMMAGE = {"pass", "run", "qb_kneel", "qb_spike", "no_play",
             "punt", "field_goal"}

COLS = ["game_id", "season", "season_type", "week", "game_date", "home_team",
        "away_team", "posteam", "defteam", "fixed_drive", "play_type", "qtr",
        "half_seconds_remaining", "game_seconds_remaining", "yardline_100",
        "score_differential", "yards_gained", "first_down", "sack",
        "pass_attempt", "rush_attempt", "qb_kneel", "touchdown", "td_team",
        "field_goal_result", "safety", "interception", "fumble_lost"]


def fetch(seasons):
    os.makedirs(PBP_DIR, exist_ok=True)
    for y in seasons:
        path = os.path.join(PBP_DIR, f"play_by_play_{y}.csv.gz")
        subprocess.run(["curl", "-sSLf", "-o", path, URL.format(y=y)], check=True)
        if y >= 2022:
            path = os.path.join(PBP_DIR, f"ftn_charting_{y}.csv")
            subprocess.run(["curl", "-sSLf", "-o", path, FTN_URL.format(y=y)],
                           check=True)
        print(f"fetched {y}")


def scoring_events(p):
    """Every score in the game: (row index, team, category, points)."""
    ev = []
    td = p[p.touchdown == 1]
    st = td.play_type.isin(["kickoff", "punt", "field_goal", "extra_point"])
    cat = np.where(st, "st", np.where(td.td_team == td.posteam, "off", "def"))
    ev.append(pd.DataFrame({"i": td.index, "team": td.td_team, "cat": cat,
                            "pts": TD_PTS}))
    fg = p[p.field_goal_result == "made"]
    ev.append(pd.DataFrame({"i": fg.index, "team": fg.posteam, "cat": "off",
                            "pts": 3}))
    sf = p[p.safety == 1]
    ev.append(pd.DataFrame({"i": sf.index, "team": sf.defteam, "cat": "def",
                            "pts": 2}))
    return pd.concat(ev)


def season_drives(y):
    p = pd.read_csv(os.path.join(PBP_DIR, f"play_by_play_{y}.csv.gz"),
                    usecols=COLS, low_memory=False)
    p = p[p.season_type.isin(["REG", "POST"]) & p.posteam.notna()]
    p = p.reset_index(drop=True)

    ev = scoring_events(p)
    p["off_pts"] = 0.0
    off = ev[ev.cat == "off"]
    p.loc[off.i, "off_pts"] = off.pts.values

    scrim = p[p.play_type.isin(SCRIMMAGE)]
    first = scrim.groupby(["game_id", "fixed_drive"]).first()
    play = p[(p.pass_attempt == 1) | (p.rush_attempt == 1)]

    g = p.groupby(["game_id", "fixed_drive"])
    d = pd.DataFrame({
        "posteam": g.posteam.agg(lambda s: s.mode().iat[0]),
        "off_pts": g.off_pts.sum(),
        "fd": g.first_down.sum(),
        "turnover": g.interception.sum() + g.fumble_lost.sum(),
    })
    pg = play.groupby(["game_id", "fixed_drive"])
    d["plays"] = pg.size()
    d["yds"] = pg.yards_gained.sum()
    d["sacks"] = pg.sack.sum()
    d[["plays", "yds", "sacks"]] = d[["plays", "yds", "sacks"]].fillna(0)
    d["kneel_only"] = (g.qb_kneel.max() == 1) & (d.plays == g.qb_kneel.sum())
    d["start_secs"] = first.half_seconds_remaining
    d["start_qtr"] = first.qtr
    d["start_yl"] = first.yardline_100
    d["start_diff"] = first.score_differential
    d["fg"] = (d.off_pts % TD_PTS == 3) & (d.off_pts > 0)
    d["td"] = d.off_pts >= TD_PTS
    d = d.dropna(subset=["start_secs", "start_yl"]).reset_index()

    meta = p.groupby("game_id")[["season", "season_type", "week", "game_date",
                                 "home_team", "away_team"]].first()
    d = d.join(meta, on="game_id")
    d["defteam"] = np.where(d.posteam == d.home_team, d.away_team, d.home_team)
    d["rump"] = (d.start_secs < RUMP_SECS) & (d.start_qtr != 5)

    # opportunities and non-offensive scores, per team-game
    kicks = p[p.play_type.isin(["kickoff", "punt"])]
    rec = np.where(kicks.play_type == "kickoff", kicks.posteam, kicks.defteam)
    st_opp = pd.Series(1, index=pd.MultiIndex.from_arrays(
        [kicks.game_id, rec])).groupby(level=[0, 1]).sum()
    ev = ev.join(p[["game_id"]], on="i")
    other = ev[ev.cat != "off"].groupby(["game_id", "team", "cat"]).pts.sum()
    return d, st_opp, other


def script_and_field(d):
    """League-wide drive adjustments for score/clock and starting field position."""
    eff = d[~d.rump & ~d.kneel_only]
    # starting field position: expected points by 5-yard bin of yardline_100
    yl = (d.start_yl.clip(1, 99) // 5) * 5
    ep = eff.groupby((eff.start_yl.clip(1, 99) // 5) * 5).off_pts.mean()
    d["ep_start"] = yl.map(ep).fillna(eff.off_pts.mean())
    # score/clock: 2nd half only; lead buckets of a score or more
    late = (d.start_qtr >= 3)
    lead = pd.cut(d.start_diff, [-99, -17, -9, -4, 3, 8, 16, 99],
                  labels=["-17+", "-9..-16", "-4..-8", "even", "+4..+8",
                          "+9..+16", "+17+"])
    q4 = d.start_qtr >= 4
    bucket = np.where(late, lead.astype(str) + np.where(q4, "|q4", "|q3"), "1st")
    resid = d.off_pts - d.ep_start
    adj = resid[~d.rump & ~d.kneel_only].groupby(
        bucket[~d.rump & ~d.kneel_only]).mean()
    adj = adj - adj.get("1st", 0.0)
    d["script_adj"] = pd.Series(bucket, index=d.index).map(adj).fillna(0.0)
    return d, adj


def team_games(d, st_opp, other):
    """Drive rows -> one row per team per game."""
    d = d.assign(eff=~d.rump & ~d.kneel_only)
    e = d[d.eff]
    k = ["game_id", "posteam"]
    t = pd.DataFrame({
        "drives_all": d.groupby(k).size(),
        "drives": e.groupby(k).size(),
        "pts": e.groupby(k).off_pts.sum(),
        "pts_script": (e.off_pts - e.script_adj).groupby(
            [e.game_id, e.posteam]).sum(),
        "ep_start": e.groupby(k).ep_start.sum(),
        "fd": e.groupby(k).fd.sum(),
        "plays": e.groupby(k).plays.sum(),
        "yds": e.groupby(k).yds.sum(),
        "sacks": e.groupby(k).sacks.sum(),
        "turnovers": d.groupby(k).turnover.sum(),
        "fgs": e.groupby(k).fg.sum(),
        "tds": e.groupby(k).td.sum(),
        "off_pts_all": d.groupby(k).off_pts.sum(),
    }).fillna(0).reset_index().rename(columns={"posteam": "team"})
    meta = d.groupby("game_id")[["season", "season_type", "week", "game_date",
                                 "home_team", "away_team"]].first()
    t = t.join(meta, on="game_id")
    t["opp"] = np.where(t.team == t.home_team, t.away_team, t.home_team)
    t["home"] = (t.team == t.home_team).astype(int)
    t["st_opps"] = [st_opp.get((g, tm), 0) for g, tm in zip(t.game_id, t.team)]
    for cat in ["def", "st"]:
        t[f"{cat}_pts"] = [other.get((g, tm, cat), 0)
                           for g, tm in zip(t.game_id, t.team)]
    # defensive points the offense gave up (pick-sixes, fumble returns, safeties)
    gave = t.set_index(["game_id", "team"]).def_pts
    t["gave_def_pts"] = [gave.get((g, o), 0) for g, o in zip(t.game_id, t.opp)]
    return t.drop(columns=["home_team", "away_team"])


def build(seasons):
    drives, opps, others = [], [], []
    for y in seasons:
        d, s, o = season_drives(y)
        drives.append(d)
        opps.append(s)
        others.append(o)
        print(f"{y}: {d.game_id.nunique()} games, {len(d)} drives")
    d = pd.concat(drives, ignore_index=True)
    d, adj = script_and_field(d)
    print("\nscore/clock adjustment (pts per drive vs a 1st-half drive):")
    print(adj.round(2).to_string())
    t = team_games(d, pd.concat(opps), pd.concat(others))
    t.to_csv(OUT, index=False)
    print(f"\nwrote {OUT}: {len(t)} team-games")
    return t


def parse_years(s):
    a, _, b = s.partition("-")
    return list(range(int(a), int(b or a) + 1))


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] not in ("fetch", "build"):
        raise SystemExit(__doc__)
    if sys.argv[1] == "fetch":
        fetch(parse_years(sys.argv[2]))
    else:
        have = sorted(int(f[13:17]) for f in os.listdir(PBP_DIR)
                      if f.startswith("play_by_play_"))
        build(have)
