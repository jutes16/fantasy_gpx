"""
Do betting splits predict covers, modelled continuously?

    python3 splits_model.py

One row per game, from the home team's side: its sharp gap (money % minus
tickets %) and its ticket %, predicting whether it covered the CLOSING spread
(pushes dropped). Data: every game with Action Network splits (2023 on).

Three models, scored OUT OF SAMPLE (leave-one-season-out: each season is
predicted by a model fit on the other seasons only):
  0. no splits      -- intercept only (home cover rate)
  1. linear         -- logistic, straight-line in gap and ticket %
  2. non-linear     -- logistic on natural cubic splines (4 knots each), with
                       an L2 penalty whose strength is chosen by inner CV, so
                       the curve only bends where the data insist
A model "has information" only if it beats model 0 on games it never saw.
Then the fitted curves are shown with bootstrap 90% bands.

Splits are Action Network's final (kickoff) numbers, graded at the close.
"""

import warnings

import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression, LogisticRegressionCV
from sklearn.metrics import brier_score_loss, log_loss
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import SplineTransformer, StandardScaler

from fetch_splits import canon

warnings.filterwarnings("ignore")
NFLVERSE = "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"
FEATURES = ["gap", "tix"]


def load():
    an = pd.read_csv("data/action_splits.csv").dropna(subset=["an_home_bets_pct", "an_home_money_pct"])
    g = pd.read_csv(NFLVERSE, low_memory=False)
    g = g[(g.game_type == "REG") & g.result.notna() & g.spread_line.notna()].copy()
    g["away"], g["home"] = g.away_team.map(canon), g.home_team.map(canon)
    d = an.merge(g[["season", "week", "away", "home", "result", "spread_line"]],
                 on=["season", "week", "away", "home"])
    d["gap"] = d.an_home_money_pct - d.an_home_bets_pct
    d["tix"] = d.an_home_bets_pct
    margin = d.result - d.spread_line
    d = d[margin != 0].copy()                      # drop pushes
    d["cover"] = (margin > 0).astype(int)
    return d.reset_index(drop=True)


def models():
    return {
        "0. no splits": None,
        "1. linear": make_pipeline(StandardScaler(), LogisticRegression(C=1e6, max_iter=1000)),
        "2. non-linear (spline)": make_pipeline(
            SplineTransformer(n_knots=4, degree=3, extrapolation="linear"),
            LogisticRegressionCV(Cs=np.logspace(-3, 2, 12), cv=5, scoring="neg_log_loss",
                                 max_iter=2000)),
    }


def loso(d):
    """Leave-one-season-out predictions for every model."""
    preds = {k: np.zeros(len(d)) for k in models()}
    for s in sorted(d.season.unique()):
        tr, te = d.season != s, d.season == s
        for name, m in models().items():
            if m is None:
                preds[name][te] = d.cover[tr].mean()
            else:
                m.fit(d.loc[tr, FEATURES], d.cover[tr])
                preds[name][te] = m.predict_proba(d.loc[te, FEATURES])[:, 1]
    return preds


def boot_curves(d, var, grid, n_boot=300, seed=0):
    """Fitted cover prob along `var` (other feature at its median), with a
    bootstrap 90% band, for the linear and spline models."""
    rng = np.random.default_rng(seed)
    other = [f for f in FEATURES if f != var][0]
    X = pd.DataFrame({var: grid, other: np.median(d[other])})[FEATURES]
    out = {}
    for name in ("1. linear", "2. non-linear (spline)"):
        fits = []
        for _ in range(n_boot):
            i = rng.integers(0, len(d), len(d))
            m = models()[name]
            m.fit(d.loc[i, FEATURES], d.cover.iloc[i])
            fits.append(m.predict_proba(X)[:, 1])
        f = np.array(fits)
        full = models()[name].fit(d[FEATURES], d.cover).predict_proba(X)[:, 1]
        out[name] = (full, np.percentile(f, 5, axis=0), np.percentile(f, 95, axis=0))
    return out


def main():
    d = load()
    print("=" * 84)
    print(f"SPLITS, MODELLED CONTINUOUSLY   {len(d)} games with splits, "
          f"{d.season.min()}-{d.season.max()}, graded at the close (pushes dropped)")
    print("=" * 84)

    # ---- out-of-sample comparison ----
    preds = loso(d)
    base_ll = log_loss(d.cover, preds["0. no splits"])
    print("\nOUT OF SAMPLE (leave-one-season-out)       log loss   Brier    vs no-splits")
    for name, p in preds.items():
        ll, br = log_loss(d.cover, p), brier_score_loss(d.cover, p)
        print(f"  {name:<40}{ll:>8.4f}{br:>9.4f}{(base_ll - ll) * 1000:>+10.2f}  (x1000, + = better)")
    # is the gain distinguishable from zero? paired bootstrap of the per-game log-loss gain
    rng = np.random.default_rng(1)
    ll_i = lambda p: -(d.cover * np.log(p) + (1 - d.cover) * np.log(1 - p))
    for name in ("1. linear", "2. non-linear (spline)"):
        gain = (ll_i(preds["0. no splits"]) - ll_i(preds[name])).to_numpy()
        bs = [gain[rng.integers(0, len(gain), len(gain))].mean() for _ in range(2000)]
        print(f"  {name:<40} gain 90% CI [{np.percentile(bs, 5)*1000:+.2f}, {np.percentile(bs, 95)*1000:+.2f}]"
              f"  P(gain > 0) = {np.mean(np.array(bs) > 0):.0%}")

    # ---- in-sample linear coefficients, for interpretation ----
    import statsmodels.api as sm
    X = sm.add_constant(d[FEATURES] / 10)                 # per 10 points
    fit = sm.Logit(d.cover, X).fit(disp=0)
    print("\nLINEAR FIT, all games (per 10 points; odds ratio 1.00 = no effect)")
    for k in FEATURES:
        lo, hi = np.exp(fit.conf_int().loc[k])
        print(f"  {k:<5} odds ratio {np.exp(fit.params[k]):.3f}  95% CI [{lo:.3f}, {hi:.3f}]  p = {fit.pvalues[k]:.2f}")

    # ---- fitted curves ----
    for var, grid, label in (("gap", np.array([-30, -20, -10, 0, 10, 20, 30]), "sharp gap (money - tickets, home side)"),
                             ("tix", np.array([20, 30, 40, 50, 60, 70, 80]), "ticket % (home side)")):
        cur = boot_curves(d, var, grid)
        print(f"\nFITTED COVER PROBABILITY by {label}; other signal at its median; 90% bootstrap band")
        print(f"  {var:<12}" + "".join(f"{v:>15}" for v in grid))
        for name, (fit_, lo, hi) in cur.items():
            short = "linear" if name.startswith("1") else "spline"
            print(f"  {short:<12}" + "".join(f"{f*100:5.1f}% [{l*100:.0f}-{h*100:.0f}]".rjust(15)
                                               for f, l, h in zip(fit_, lo, hi)))
    print("\n  in-sample games per gap range:", pd.cut(d.gap, [-100, -20, -10, 0, 10, 20, 100]).value_counts(sort=False).to_dict())


if __name__ == "__main__":
    main()
