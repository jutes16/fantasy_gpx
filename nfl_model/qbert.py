"""
QBERT-lite: quarterback ratings from play-by-play, following the 2026
QBERT changes where free data allows.

Unit: EPA per QB play (dropbacks, scrambles and designed QB runs).
    drops         a dropped pass is credited as an average completion at
                  that depth (FTN charting, 2022 on only)
    DPI           defensive pass interference counts as a QB play, credited
                  to the team's most recent passer, worth its EPA
    grounding     already a dropback, worth its (negative) EPA
    sneaks        left out entirely (FTN flag from 2022; before that a QB
                  run with 2 or fewer yards to go up the middle, which
                  matches the FTN flag ~80% of the time)
    interceptions an INT play is scored as an incompletion, and the QB's
                  INT rate enters separately, regressed toward the league
                  rate with INT_PRIOR dropbacks of league average
    touchdowns    EPA values a TD at its points, not QBR's ~45-yard bonus,
                  so no extra discount is applied
    game script   none (QBERT 2026 dropped it)

Ratings are a weighted ridge on QB-games, opponent-defense adjusted:
    value/play = prior[qb] + qb[qb] + def[opp]
where the prior is set by draft slot for QBs with little history.

    python3 qbert.py build        writes data/qb_games.csv
    python3 qbert.py top          current ratings
"""

import os
import sys

import numpy as np
import pandas as pd

from fetch_splits import canon

PBP_DIR = "data/pbp"
OUT = "data/qb_games.csv"

QB_HALF_LIFE = 80.0   # weeks; QB skill is more persistent than team form
QB_LOOKBACK = 8       # seasons
QB_RIDGE = 6.0        # games of shrinkage toward the draft-slot prior
DEF_RIDGE = 8.0
INT_PRIOR = 1000.0    # dropbacks of league-average INT rate
# draft-slot priors are fitted on even seasons only (see elway_lite.is_fit)

COLS = ["game_id", "play_id", "season", "season_type", "week", "posteam",
        "defteam", "play_type", "qb_dropback", "qb_scramble", "qb_kneel",
        "qb_spike", "passer_player_id", "passer_player_name",
        "rusher_player_id", "rusher_player_name", "epa", "penalty_type",
        "interception", "complete_pass", "incomplete_pass", "ydstogo",
        "run_location", "air_yards"]


def season_plays(y):
    p = pd.read_csv(os.path.join(PBP_DIR, f"play_by_play_{y}.csv.gz"),
                    usecols=COLS, low_memory=False)
    p = p[p.season_type.isin(["REG", "POST"]) & p.posteam.notna()]
    p = p.sort_values(["game_id", "play_id"]).reset_index(drop=True)

    p["qb"] = p.passer_player_id.where(p.passer_player_id.notna(),
                                       p.rusher_player_id.where(p.qb_dropback == 1))
    p["qb_name"] = p.passer_player_name.where(p.passer_player_name.notna(),
                                              p.rusher_player_name.where(p.qb_dropback == 1))
    # DPI: no passer recorded -> the team's most recent passer in the game
    last = p[["game_id", "posteam", "qb", "qb_name"]].copy()
    last[["qb", "qb_name"]] = last.groupby(["game_id", "posteam"])[
        ["qb", "qb_name"]].ffill()
    dpi = p.penalty_type == "Defensive Pass Interference"
    p.loc[dpi, "qb"] = last.qb[dpi]
    p.loc[dpi, "qb_name"] = last.qb_name[dpi]

    qbs = set(p.loc[p.qb_dropback == 1, "passer_player_id"].dropna())
    run = (p.play_type == "run") & (p.qb_scramble != 1) & (p.qb_kneel != 1) \
        & p.rusher_player_id.isin(qbs)
    ftn_path = os.path.join(PBP_DIR, f"ftn_charting_{y}.csv")
    p["drop"] = False
    if os.path.exists(ftn_path):
        f = pd.read_csv(ftn_path, usecols=["nflverse_game_id", "nflverse_play_id",
                                           "is_drop", "is_qb_sneak"])
        f = f.rename(columns={"nflverse_game_id": "game_id",
                              "nflverse_play_id": "play_id"})
        p = p.merge(f, on=["game_id", "play_id"], how="left")
        sneak = p.is_qb_sneak.fillna(False).astype(bool)
        p["drop"] = p.is_drop.fillna(False).astype(bool)
    else:
        sneak = run & (p.ydstogo <= 2) & (p.run_location == "middle")
    p.loc[run & ~sneak, "qb"] = p.rusher_player_id
    p.loc[run & ~sneak, "qb_name"] = p.rusher_player_name

    use = ((p.qb_dropback == 1) | dpi | (run & ~sneak)) & (p.qb_spike != 1) \
        & p.qb.notna() & p.epa.notna()
    q = p[use].copy()

    # drops -> an average completion at that depth; INTs -> an incompletion
    depth = pd.cut(q.air_yards, [-99, 0, 5, 10, 15, 20, 30, 99])
    comp = q[q.complete_pass == 1].groupby(depth[q.complete_pass == 1],
                                           observed=True).epa.mean()
    inc_epa = q.loc[q.incomplete_pass == 1, "epa"].mean()
    is_drop = q["drop"] & (q.incomplete_pass == 1)
    q.loc[is_drop, "epa"] = depth[is_drop].map(comp).astype(float).fillna(inc_epa)
    q["int"] = (q.interception == 1).astype(int)
    int_cost = q.loc[q.int == 1, "epa"].mean() - inc_epa
    q.loc[q.int == 1, "epa"] = inc_epa
    q["pass_play"] = ((q.qb_dropback == 1) | dpi[q.index]).astype(int)

    g = q.groupby(["game_id", "posteam", "defteam", "qb"])
    out = pd.DataFrame({
        "qb_name": g.qb_name.agg(lambda s: s.mode().iat[0]),
        "plays": g.size(), "epa": g.epa.sum(), "ints": g.int.sum(),
        "dropbacks": g.pass_play.sum(), "drops": g["drop"].sum(),
    }).reset_index()
    meta = p.groupby("game_id")[["season", "week"]].first()
    out = out.join(meta, on="game_id")
    out["int_cost"] = int_cost
    return out


def build():
    have = sorted(int(f[13:17]) for f in os.listdir(PBP_DIR)
                  if f.startswith("play_by_play_"))
    parts = []
    for y in have:
        parts.append(season_plays(y))
        print(f"{y}: {len(parts[-1])} QB-games, INT cost {parts[-1].int_cost.iat[0]:.2f} EPA")
    q = pd.concat(parts, ignore_index=True).rename(
        columns={"posteam": "team", "defteam": "opp"})
    q["team"] = q.team.map(canon)
    q["opp"] = q.opp.map(canon)
    q.to_csv(OUT, index=False)
    print(f"wrote {OUT}: {len(q)} QB-games")


# --- ratings ----------------------------------------------------------------

def draft_info():
    pl = pd.read_csv(os.path.join(PBP_DIR, "players.csv"),
                     usecols=["gsis_id", "draft_pick", "draft_year",
                              "rookie_season", "birth_date"], low_memory=False)
    return pl.set_index("gsis_id")


def draft_bucket(pick):
    if pd.isna(pick):
        return "undrafted"
    return "top10" if pick <= 10 else "rd1" if pick <= 32 else \
        "rd2-3" if pick <= 100 else "day3"


def fit_priors(q, info):
    """Per-play value of QBs in their first two seasons, by draft slot."""
    q = q.join(info[["draft_pick", "rookie_season"]], on="qb")
    fit = (q.season % 2 == 0) & (q.season <= 2024)
    early = q[fit & (q.season - q.rookie_season <= 1)]
    early = early.assign(b=early.draft_pick.map(draft_bucket))
    g = early.groupby("b")
    pri = (g.epa.sum() / g.plays.sum())
    return pri


class QBRatings:
    """QB ratings refitted at any point in time from QB-games before it."""

    def __init__(self, clock_fn):
        self.q = pd.read_csv(OUT)
        self.q["clock"] = clock_fn(self.q.season, self.q.week)
        self.info = draft_info()
        self.priors = fit_priors(self.q, self.info)
        self.bucket = self.info.draft_pick.map(draft_bucket)
        self.names = self.q.groupby("qb").qb_name.last()

    def prior(self, qb):
        b = self.bucket.get(qb, "undrafted")
        return self.priors.get(b, self.priors["undrafted"])

    def fit(self, now, span, half_life=None):
        half_life = half_life or QB_HALF_LIFE
        q = self.q[(self.q.clock < now) & (self.q.clock >= now - QB_LOOKBACK * span)]
        qbs = {x: i for i, x in enumerate(sorted(q.qb.unique()))}
        defs = {x: i for i, x in enumerate(sorted(q.opp.unique()))}
        nq, nd = len(qbs), len(defs)
        w = q.plays.values / 35.0 * np.exp(-(now - q.clock.values) / half_life)
        pri = q.qb.map(lambda x: self.prior(x)).values
        y = q.epa.values / q.plays.values - pri
        X = np.zeros((len(q), nq + nd + 1))
        X[np.arange(len(q)), q.qb.map(qbs).values] = 1
        X[np.arange(len(q)), nq + q.opp.map(defs).values] = 1
        X[:, -1] = 1.0
        pen = np.r_[np.full(nq, QB_RIDGE), np.full(nd, DEF_RIDGE), 0.0]
        A = (X * w[:, None]).T @ X + np.diag(pen)
        beta = np.linalg.solve(A, (X * w[:, None]).T @ y)
        # INT rate, regressed toward the league rate
        wd = q.dropbacks.values * np.exp(-(now - q.clock.values) / half_life)
        wi = q.ints.values * np.exp(-(now - q.clock.values) / half_life)
        lg = q.ints.sum() / max(q.dropbacks.sum(), 1)
        ints = pd.DataFrame({"qb": q.qb, "wd": wd, "wi": wi}).groupby("qb").sum()
        rate = (ints.wi + INT_PRIOR * lg) / (ints.wd + INT_PRIOR)
        cost = q.int_cost.mean()
        share = (q.dropbacks.sum() / q.plays.sum())
        r = {x: self.prior(x) + beta[i] + (rate[x] - lg) * cost * share
             for x, i in qbs.items()}
        self.rating, self.lg_int, self.cost, self.share = r, lg, cost, share
        return r

    def get(self, qb):
        if pd.isna(qb):
            return self.priors["undrafted"]
        return self.rating.get(qb, self.prior(qb))

    def team_baseline(self, team, now, half_life, span):
        """Plays-weighted rating of the QBs the team has actually used lately."""
        q = self.q[(self.q.team == team) & (self.q.clock < now)
                   & (self.q.clock >= now - 2 * span)]
        if q.empty:
            return np.nan
        w = q.plays.values * np.exp(-(now - q.clock.values) / half_life)
        return float(np.sum(w * q.qb.map(self.get).values) / w.sum())


if __name__ == "__main__":
    if sys.argv[1:2] == ["build"]:
        build()
    elif sys.argv[1:2] == ["top"]:
        import elway_lite as E
        R = QBRatings(E.clock)
        g = E.load_games()
        now = g[g.result.isna()].clock.min()
        R.fit(now, E.SPAN)
        recent = R.q[R.q.clock >= now - E.SPAN].groupby("qb").plays.sum()
        recent = recent[recent >= 150].index
        top = pd.Series({x: R.rating[x] for x in recent}).sort_values(ascending=False)
        for x, v in top.items():
            print(f"{R.names.get(x, x):22s} {v:+.3f} EPA/play")
    else:
        raise SystemExit(__doc__)
