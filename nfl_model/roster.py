"""
AV-style roster ratings: what each team's Week 1 roster is worth, from what
its players produced the last three seasons, wherever they produced it.

Approximate Value, rebuilt from free data the way Pro-Football-Reference
builds it: a unit's value is split among its positions, then among the
players at each position by snaps.

    unit value     points above replacement over the season, from
                   drives.py: offense = (ppd - replacement ppd) x drives,
                   defense = (replacement ppd allowed - ppd allowed) x
                   opponent drives. Replacement is the REPLACEMENT_PCT
                   percentile team-season. Floored at zero.
    QB share       QB_SHARE of the offense goes to the QB and is left out
                   here: the QB layer (qbert.py) handles quarterbacks.
    positions      POS_WEIGHT within offense and within defense
    players        a position's value split by snaps (nflverse snap counts,
                   2012 on), then divided by games: AV per game

Projection to a new season, per position group, fitted on EVEN seasons:
    veterans   AV/game ~ recent AV (weights 3/2/1 over the last three
               seasons) x (1 + aging terms around the group's peak age)
    rookies    AV/game ~ log(draft pick), separately for offense/defense;
               undrafted rookies get the undrafted mean

Roster rating = sum of projected AV/game over the Week 1 active roster,
with the top SLOTS[group] players at full weight and the rest at DEPTH.
Offense and defense are kept separate, in points per game.

    python3 roster.py fetch 2012-2026   snap counts, weekly rosters, players
    python3 roster.py build         writes data/roster_ratings.csv
    python3 roster.py show 2026     a season's ratings
"""

import os
import subprocess
import sys

import numpy as np
import pandas as pd

from fetch_splits import canon

PBP_DIR = "data/pbp"
DRIVES = "data/drives.csv"
OUT = "data/roster_ratings.csv"
PLAYER_OUT = "data/player_av.csv"

REPLACEMENT_PCT = 20
QB_SHARE = 0.5
GROUP = {"T": "OL", "G": "OL", "C": "OL", "OL": "OL", "WR": "WR", "TE": "TE",
         "RB": "RB", "HB": "RB", "FB": "FB",
         "DE": "DL", "DT": "DL", "NT": "DL", "DL": "DL", "LB": "LB",
         "ILB": "LB", "OLB": "LB", "MLB": "LB", "CB": "DB", "S": "DB",
         "SS": "DB", "FS": "DB", "DB": "DB", "SAF": "DB"}
SIDE = {"OL": "off", "WR": "off", "TE": "off", "RB": "off", "FB": "off",
        "DL": "def", "LB": "def", "DB": "def"}
POS_WEIGHT = {"OL": 0.36, "WR": 0.30, "TE": 0.14, "RB": 0.17, "FB": 0.03,
              "DL": 0.40, "LB": 0.22, "DB": 0.38}
PEAK_AGE = {"OL": 28, "WR": 27, "TE": 27, "RB": 25, "FB": 26,
            "DL": 27, "LB": 26, "DB": 26}
SLOTS = {"OL": 5, "WR": 3, "TE": 2, "RB": 2, "FB": 1, "DL": 4, "LB": 3, "DB": 5}
DEPTH = 0.2
YEAR_WEIGHTS = [3, 2, 1]
# legacy codes in older nflverse roster files
OLD_CODES = {"ARZ": "ARI", "BLT": "BAL", "CLV": "CLE", "HST": "HOU", "SL": "LAR"}


def fetch(seasons):
    from drives import RELEASES
    files = [("players/players.csv", "players.csv")]
    for y in seasons:
        files += [(f"snap_counts/snap_counts_{y}.csv", f"snap_counts_{y}.csv"),
                  (f"weekly_rosters/roster_weekly_{y}.csv", f"roster_weekly_{y}.csv")]
    for src, dst in files:
        subprocess.run(["curl", "-sSLf", "-o", os.path.join(PBP_DIR, dst),
                        f"{RELEASES}/{src}"], check=True)
        print(f"fetched {dst}")


def fit_years(seasons):
    return (seasons % 2 == 0) & (seasons <= 2024)


def unit_values():
    """Points above replacement per team-season, offense and defense."""
    t = pd.read_csv(DRIVES)
    t = t[t.season_type == "REG"]
    t["team"] = t.team.map(canon)
    t["opp"] = t.opp.map(canon)
    off = t.groupby(["season", "team"]).agg(pts=("pts_script", "sum"),
                                            drives=("drives", "sum"),
                                            games=("game_id", "nunique"))
    dfn = t.groupby(["season", "opp"]).agg(pts=("pts_script", "sum"),
                                           drives=("drives", "sum"))
    dfn.index.names = ["season", "team"]
    off["ppd"] = off.pts / off.drives
    dfn["ppd"] = dfn.pts / dfn.drives
    rep_o = off.groupby(level=0).ppd.transform(
        lambda s: np.percentile(s, REPLACEMENT_PCT))
    rep_d = dfn.groupby(level=0).ppd.transform(
        lambda s: np.percentile(s, 100 - REPLACEMENT_PCT))
    v = pd.DataFrame({
        "off": ((off.ppd - rep_o) * off.drives).clip(lower=0) * (1 - QB_SHARE),
        "def": ((rep_d - dfn.ppd) * dfn.drives).clip(lower=0),
        "games": off.games,
    })
    return v


def ids():
    pl = pd.read_csv(os.path.join(PBP_DIR, "players.csv"),
                     usecols=["gsis_id", "pfr_id", "birth_date", "draft_pick"],
                     low_memory=False)
    return pl


def player_av():
    """AV per game for every player-team-season with snap counts."""
    v = unit_values()
    parts = []
    for y in range(2012, 2027):
        path = os.path.join(PBP_DIR, f"snap_counts_{y}.csv")
        if os.path.exists(path):
            parts.append(pd.read_csv(path))
    s = pd.concat(parts)
    s = s[s.game_type == "REG"]
    s["team"] = s.team.map(canon)
    s["group"] = s.position.map(GROUP)
    s = s[s.group.notna()]
    s["snaps"] = np.where(s.group.map(SIDE) == "off", s.offense_snaps,
                          s.defense_snaps).astype(float)
    p = s.groupby(["season", "team", "pfr_player_id", "group"]).snaps.sum().reset_index()
    p["group_snaps"] = p.groupby(["season", "team", "group"]).snaps.transform("sum")
    p = p[p.group_snaps > 0]
    p = p.join(v, on=["season", "team"])
    side_val = np.where(p.group.map(SIDE) == "off", p["off"], p["def"])
    p["av"] = side_val * p.group.map(POS_WEIGHT) * p.snaps / p.group_snaps
    p["av_pg"] = p.av / p.games
    # a player traded mid-season: sum his teams, keep his main group
    a = p.sort_values("snaps").groupby(["season", "pfr_player_id"]).agg(
        av_pg=("av_pg", "sum"), snaps=("snaps", "sum"), group=("group", "last"))
    return a.reset_index()


def week1_rosters():
    pl = ids().dropna(subset=["gsis_id"]).set_index("gsis_id")
    parts = []
    for y in range(2013, 2027):
        r = pd.read_csv(os.path.join(PBP_DIR, f"roster_weekly_{y}.csv"),
                        low_memory=False)
        r = r[(r.game_type == "REG") & r.status.isin(["ACT", "INA"])]
        r = r[r.week == r.groupby("team").week.transform("min")]   # 2017: Irma
        parts.append(r[["season", "team", "gsis_id", "pfr_id", "position",
                        "birth_date", "draft_number", "rookie_year",
                        "entry_year"]])
    r = pd.concat(parts, ignore_index=True)
    r["team"] = r.team.replace(OLD_CODES).map(canon)
    r["pfr_id"] = r.pfr_id.fillna(r.gsis_id.map(pl.pfr_id))
    r["birth_date"] = r.birth_date.fillna(r.gsis_id.map(pl.birth_date))
    r["draft_number"] = r.draft_number.fillna(r.gsis_id.map(pl.draft_pick))
    r["group"] = r.position.map(GROUP)
    return r[r.group.notna()]


def history(r, av):
    """Weighted recent AV/game for each rostered player, plus his age."""
    a = av.set_index(["pfr_player_id", "season"]).av_pg
    num = np.zeros(len(r))
    den = np.zeros(len(r))
    for k, w in enumerate(YEAR_WEIGHTS, start=1):
        x = np.array([a.get((pid, s - k), np.nan)
                      for pid, s in zip(r.pfr_id, r.season)])
        num += np.where(np.isnan(x), 0, w * np.nan_to_num(x))
        den += np.where(np.isnan(x), 0, w)
    r = r.copy()
    r["base"] = np.where(den > 0, num / np.where(den > 0, den, 1), np.nan)
    r["years"] = den
    born = pd.to_datetime(r.birth_date, errors="coerce")
    r["age"] = r.season - born.dt.year - (born.dt.month > 8)
    r["age"] = r.age.fillna(r.groupby("group").age.transform("median"))
    r["rookie"] = (r.rookie_year == r.season) | (r.entry_year == r.season)
    actual = av.set_index(["pfr_player_id", "season"]).av_pg
    r["actual"] = [actual.get((pid, s), 0.0) for pid, s in zip(r.pfr_id, r.season)]
    return r


def fit_projection(r):
    """Per group: veterans on recent AV and age; rookies on draft slot."""
    models = {}
    fit = r[fit_years(r.season)]
    for grp in POS_WEIGHT:
        v = fit[(fit.group == grp) & fit.base.notna()]
        peak = PEAK_AGE[grp]
        X = np.column_stack([v.base, v.base * (v.age - peak).clip(lower=0),
                             v.base * (peak - v.age).clip(lower=0),
                             np.ones(len(v))])
        models[(grp, "vet")] = np.linalg.lstsq(X, v.actual, rcond=None)[0]
        rk = fit[(fit.group == grp) & fit.rookie & fit.draft_number.notna()]
        X = np.column_stack([np.log(rk.draft_number), np.ones(len(rk))])
        models[(grp, "rookie")] = np.linalg.lstsq(X, rk.actual, rcond=None)[0]
        ud = fit[(fit.group == grp) & fit.base.isna() & ~(fit.rookie & fit.draft_number.notna())]
        models[(grp, "other")] = ud.actual.mean() if len(ud) else 0.0
    return models


def project(r, models):
    proj = np.zeros(len(r))
    for i, x in enumerate(r.itertuples()):
        g = x.group
        if not np.isnan(x.base):
            b = models[(g, "vet")]
            peak = PEAK_AGE[g]
            proj[i] = (b[0] * x.base + b[1] * x.base * max(x.age - peak, 0)
                       + b[2] * x.base * max(peak - x.age, 0) + b[3])
        elif x.rookie and not np.isnan(x.draft_number):
            b = models[(g, "rookie")]
            proj[i] = b[0] * np.log(x.draft_number) + b[1]
        else:
            proj[i] = models[(g, "other")]
    return np.maximum(proj, 0)


def team_ratings(r):
    r = r.sort_values("proj", ascending=False)
    r["rank"] = r.groupby(["season", "team", "group"]).cumcount()
    r["w"] = np.where(r["rank"] < r.group.map(SLOTS), 1.0, DEPTH)
    r["val"] = r.w * r.proj
    r["side"] = r.group.map(SIDE)
    t = r.pivot_table(index=["season", "team"], columns="side", values="val",
                      aggfunc="sum").rename(columns={"off": "roster_off",
                                                     "def": "roster_def"})
    return t.reset_index()


def build():
    av = player_av()
    av.to_csv(PLAYER_OUT, index=False)
    r = history(week1_rosters(), av)
    models = fit_projection(r)
    r["proj"] = project(r, models)
    t = team_ratings(r)
    t.to_csv(OUT, index=False)
    ok = r[r.base.notna()]
    print(f"{len(av)} player-seasons of AV; {len(r)} Week 1 roster spots, "
          f"{len(ok) / len(r):.0%} with history")
    for k, b in models.items():
        if k[1] == "vet":
            print(f"  {k[0]:3s} next AV/g = {b[0]:.2f} x recent "
                  f"{b[1]:+.3f}/yr past peak {b[2]:+.3f}/yr before")
    test = r[~fit_years(r.season) & (r.season <= 2025)]
    print(f"odd-season check, projected vs actual AV/g per player: corr "
          f"{np.corrcoef(test.proj, test.actual)[0, 1]:.3f}")
    print(f"wrote {OUT}, {PLAYER_OUT}")


if __name__ == "__main__":
    if sys.argv[1:2] == ["build"]:
        build()
    elif sys.argv[1:2] == ["fetch"]:
        from drives import parse_years
        fetch(parse_years(sys.argv[2]))
    elif sys.argv[1:2] == ["show"]:
        t = pd.read_csv(OUT)
        s = t[t.season == int(sys.argv[2])].copy()
        s["total"] = s.roster_off + s.roster_def
        print(s.sort_values("total", ascending=False).round(2).to_string(index=False))
    else:
        raise SystemExit(__doc__)
