"""
Player history and matchup for a prop, from nflverse game logs, using only
games played BEFORE the prop's week (no peeking).

  hit rate   share of the player's last 8 games (this season, then last
             season's latest) that would have won the bet at today's line
  avg        his average this season so far, and over those last 8 games
  matchup    what the opponent has allowed per game this season to the
             player's position for that stat, relative to the league
             average, pulled toward 1 by MATCHUP_PRIOR games' worth of
             average defense (three or four games is mostly noise)

These are context and logged features, not part of any signal: ../nfl_bets/
props.py report shows whether they predict anything beyond the market price.
"""

import os
import sys
import time

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "nfl_bets"))
import prop_shapes                          # noqa: E402  (game_logs: downloads + caches)
from common import canon                    # noqa: E402
from weekly_stats import _normalize_name    # noqa: E402

LAST_N = 8
MATCHUP_PRIOR = 4               # games of league-average defense blended into each team's record
STAT = {"player_pass_yds": "passing_yards", "player_pass_tds": "passing_tds",
        "player_rush_yds": "rushing_yards", "player_reception_yds": "receiving_yards",
        "player_receptions": "receptions", "player_anytime_td": "td"}
POS_GROUP = {"QB": "QB", "RB": "RB", "FB": "RB", "WR": "WR", "TE": "TE"}


def logs(season):
    """This season's and last season's regular-season game logs (current season re-fetched twice a day)."""
    cur = os.path.join(prop_shapes.NFLVERSE, f"stats_player_week_{season}.csv")
    if os.path.exists(cur) and time.time() - os.path.getmtime(cur) > 12 * 3600:
        os.remove(cur)
    d = prop_shapes.game_logs((season - 1, season))
    d = d[d.player_display_name.notna()].copy()
    d["key"] = d.player_display_name.map(_normalize_name)
    d["team"] = d.team.map(canon)
    d["opponent_team"] = d.opponent_team.map(canon)
    d["grp"] = d.position.map(POS_GROUP)
    return d


def defense_factors(d, season, week):
    """(opponent, position group, market) -> allowed per game / league average, shrunk."""
    cur = d[(d.season == season) & (d.week < week) & d.grp.notna()]
    out = {}
    for mk, col in STAT.items():
        per_game = cur.groupby(["opponent_team", "grp", "week"])[col].sum().reset_index()
        league = per_game.groupby("grp")[col].mean()
        team = per_game.groupby(["opponent_team", "grp"])[col].agg(["mean", "size"]).reset_index()
        for r in team.itertuples():
            lg = league.get(r.grp, np.nan)
            if not lg or np.isnan(lg):
                continue
            f = r.mean / lg
            out[(r.opponent_team, r.grp, mk)] = (r.size * f + MATCHUP_PRIOR) / (r.size + MATCHUP_PRIOR)
    return out


def won(side, line, x):
    if side == "yes":
        return x >= 1
    if side == "no":
        return x < 1
    if x == line:
        return np.nan                                  # a push: not a hit or a miss
    return (x > line) if side == "over" else (x < line)


def features(sheet, season, week, d=None):
    """Adds hist_n, hist_avg, l8_avg, hit_rate, opp, opp_factor to a props sheet."""
    d = logs(season) if d is None else d
    before = d[(d.season < season) | (d.week < week)]
    dfac = defense_factors(d, season, week)
    games = {k: g.sort_values(["season", "week"]) for k, g in before.groupby("key")}
    rows = []
    for r in sheet.itertuples():
        g = games.get(r.key)
        if g is not None and isinstance(r.team, str):
            on_team = g[g.team == r.team]
            g = on_team if len(on_team) else g                     # same name, other team: rare
        f = dict(hist_n=0, hist_avg=None, l8_avg=None, hit_rate=None, opp=None, opp_factor=None)
        away, home = r.game.split(" @ ")
        team = r.team if isinstance(r.team, str) else None
        if g is not None and len(g):
            team = team or g.team.iloc[-1]
            vals = g[STAT[r.market]].to_numpy(dtype=float)
            cur = g[g.season == season][STAT[r.market]]
            last = vals[-LAST_N:]
            hits = [won(r.side, r.line, x) for x in last]
            hits = [h for h in hits if not (isinstance(h, float) and np.isnan(h))]
            f.update(hist_n=int(len(cur)), hist_avg=round(float(cur.mean()), 2) if len(cur) else None,
                     l8_avg=round(float(last.mean()), 2),
                     hit_rate=round(float(np.mean(hits)), 3) if hits else None)
            grp = POS_GROUP.get(g.position.iloc[-1])
        else:
            grp = POS_GROUP.get(r.pos) if isinstance(r.pos, str) else None
        if team in (away, home):
            opp = home if team == away else away
            f["opp"] = opp
            fac = dfac.get((opp, grp, r.market))
            f["opp_factor"] = None if fac is None else round(float(fac), 3)
        rows.append(f)
    return pd.concat([sheet.reset_index(drop=True), pd.DataFrame(rows)], axis=1)
