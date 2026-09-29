"""
Is it worth building a team-quality / projection model?

Decisive test: build power ratings from ACTUAL GAME RESULTS (far better
information than preseason fantasy projections), walk them forward with
no look-ahead, predict every spread, and see whether the model's side
covers when it disagrees with the market.

If a model fed real results cannot beat the closing line, one fed
preseason fantasy projections certainly cannot.

Method
  ratings solved by ridge regression on:
      margin = rating[home] - rating[away] + HFA
  fitted only on games played BEFORE the game being predicted
  (within-season, with prior-season ratings shrunk in as a preseason prior)
"""

import numpy as np
import pandas as pd
from scipy import stats
from sklearn.linear_model import Ridge

MIN_WEEK = 4        # need some games before ratings mean anything
RIDGE_ALPHA = 12.0  # shrinkage toward average; tuned loosely, not fitted to ATS
CARRYOVER = 0.45    # weight on prior-season rating as a preseason prior


def wilson(w, n):
    if n == 0:
        return (np.nan, np.nan, np.nan)
    z = 1.959963985
    p = w / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return (p, c - h, c + h)


def fit_ratings(games, teams, prior=None):
    """Ridge-solve team ratings + home field from a set of played games."""
    if len(games) == 0:
        return {t: 0.0 for t in teams}, 2.0
    idx = {t: i for i, t in enumerate(teams)}
    X = np.zeros((len(games), len(teams) + 1))
    y = games.result.values.astype(float)
    for r, (_, g) in enumerate(games.iterrows()):
        X[r, idx[g.home_team]] = 1.0
        X[r, idx[g.away_team]] = -1.0
        X[r, -1] = 1.0                      # home field
    m = Ridge(alpha=RIDGE_ALPHA, fit_intercept=False)
    m.fit(X, y)
    rat = {t: m.coef_[idx[t]] for t in teams}
    hfa = m.coef_[-1]
    if prior:
        rat = {t: (1 - CARRYOVER) * rat[t] + CARRYOVER * prior.get(t, 0.0)
               for t in teams}
    return rat, hfa


d = pd.read_csv("data/games_clean.csv", low_memory=False)
d = d.sort_values(["season", "week"]).reset_index(drop=True)
teams = sorted(set(d.home_team) | set(d.away_team))

rows = []
prior_ratings = None
for season, sdf in d.groupby("season"):
    season_prior = prior_ratings
    for week in sorted(sdf.week.unique()):
        if week < MIN_WEEK:
            continue
        played = sdf[sdf.week < week]
        rat, hfa = fit_ratings(played, teams, prior=season_prior)
        cur = sdf[sdf.week == week]
        for _, g in cur.iterrows():
            pred = rat[g.home_team] - rat[g.away_team] + hfa
            rows.append(dict(
                season=season, week=week,
                home=g.home_team, away=g.away_team,
                actual=g.result, mkt=g.spread_line, model=pred,
            ))
    # end of season: full-season ratings become next season's prior
    prior_ratings, _ = fit_ratings(sdf, teams)

p = pd.DataFrame(rows)
p["disagree"] = p.model - p.mkt          # + means model likes home more
p["abs_dis"] = p.disagree.abs()
p["home_cov"] = p.actual - p.mkt
p = p[p.home_cov != 0]                    # drop pushes

# model's side: home if it thinks home is undervalued
p["model_side_won"] = np.where(p.disagree > 0, p.home_cov > 0, p.home_cov < 0)

print("=" * 74)
print("CAN A TEAM-QUALITY MODEL BEAT THE CLOSING LINE?")
print(f"walk-forward power ratings, {p.season.min()}-{p.season.max()}, "
      f"{len(p)} games (week {MIN_WEEK}+)")
print("=" * 74)

mae_model = (p.model - p.actual).abs().mean()
mae_mkt = (p.mkt - p.actual).abs().mean()
print(f"\nmean absolute error predicting actual margin:")
print(f"  market closing line : {mae_mkt:.3f} pts")
print(f"  power-rating model  : {mae_model:.3f} pts")
print(f"  -> model is {mae_model-mae_mkt:+.3f} pts {'WORSE' if mae_model>mae_mkt else 'BETTER'}")

print(f"\nmean |disagreement| with market: {p.abs_dis.mean():.2f} pts")

print("\nATS record of the MODEL'S side, by how much it disagrees:")
out = []
for lo, hi in [(0, 1), (1, 2), (2, 3), (3, 5), (5, 99)]:
    sub = p[(p.abs_dis >= lo) & (p.abs_dis < hi)]
    if len(sub) < 30:
        continue
    w = int(sub.model_side_won.sum())
    r, cl, ch = wilson(w, len(sub))
    out.append(dict(disagreement=f"{lo}-{hi}pt", games=len(sub), wins=w,
                    win_rate=r, ci_lo=cl, ci_hi=ch))
w = int(p.model_side_won.sum())
r, cl, ch = wilson(w, len(p))
out.append(dict(disagreement="ALL", games=len(p), wins=w,
                win_rate=r, ci_lo=cl, ci_hi=ch))
print(pd.DataFrame(out).to_string(index=False, float_format=lambda x: f"{x:.4f}"))

print("\n" + "=" * 74)
print("WHAT WOULD IT TAKE TO BE USEFUL?")
print("=" * 74)
print("A projection model only helps if it is MORE accurate than the market.")
print(f"The market's MAE is {mae_mkt:.2f} pts. To generate 1 point of genuine")
print("edge you would need to beat that by roughly a point, consistently.")
print("\nFor reference, the line-value signal needs no forecasting at all:")
print("it compares two numbers that both already exist.")

p.to_csv("data/power_rating_test.csv", index=False)
print("\nwrote data/power_rating_test.csv")
