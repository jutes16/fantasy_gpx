"""
Game-to-game shapes of player stats, measured from nflverse game logs, for
turning a prop line (a median) into a projected mean and back.

    python3 prop_shapes.py          # fit on 2022-24, check on 2025, write data/prop_shapes.json

What it measures, for every game of a player with 6+ other games that season,
around his average over those OTHER games (so the game itself can't leak in):

  yards (passing, rushing, QB rushing, receiving): the full distribution of
      game / expected, in volume bands. The shape changes a lot with volume: a
      20-yard receiver's mean sits ~1.4x his median (plenty of zeros and
      blow-ups), a 70-yard receiver's ~1.1x. Each band is rescaled to mean 1
      and narrowed for the noise in the 'expected' (an average of ~13 games).
  counts (receptions, passing TDs): dispersion = variance / mean, the same
      noise correction. 1 = Poisson; receptions run wider (1.19), passing TDs
      narrower (0.79), and both predict 2025's zero games better than Poisson.
  anytime TD stays Poisson: TDs measure narrower (0.81) yet zero-TD games are
      MORE common than Poisson says (zero-heavy, rarely two), so the dispersion
      fit predicts 2025's P(0) worse than Poisson does (62.8% vs 65.6%; actual 68.7%).

The check fits on 2022-24 and scores 2025: for each 2025 game, the line the
shape implies at the 25th / 50th / 75th percentile, and how often the player
actually went over it (should be 75% / 50% / 25%). The old fixed ratios
(one mean/median per market, normal spread) are scored the same way.
"""

import json
import os
import urllib.request

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
NFLVERSE = os.path.join(HERE, "data", "nflverse")
OUT = os.path.join(HERE, "data", "prop_shapes.json")
CHECK_OUT = os.path.join(HERE, "data", "prop_shapes_check.csv")   # the 2025 check, as a table
URL = "https://github.com/nflverse/nflverse-data/releases/download/stats_player/stats_player_week_{}.csv"
FIT, CHECK = (2022, 2023, 2024), (2025,)
MIN_OTHER = 6
QS = np.linspace(0, 1, 201)

# stat column, which players, volume bands on the expected value
YARDS = {
    "player_pass_yds": ("passing_yards", "qb", [120, 210, 250, 450]),
    "player_rush_yds": ("rushing_yards", "skill", [10, 20, 30, 45, 65, 200]),
    "qb_rush_yds": ("rushing_yards", "qb", [5, 15, 25, 100]),
    "player_reception_yds": ("receiving_yards", "skill", [10, 20, 30, 45, 65, 200]),
}
COUNTS = {                          # stat, players, minimum expected (prop-level volume)
    "player_receptions": ("receptions", "skill", 1.5),
    "player_pass_tds": ("passing_tds", "qb", 0.5),
    "player_anytime_td": ("td", "skill", 0.2),
}
OLD = {"player_pass_yds": (0.33, 0.99), "player_rush_yds": (0.53, 1.05),   # (cv, mean/median)
       "qb_rush_yds": (0.83, 1.18), "player_reception_yds": (0.62, 1.10)}


def game_logs(seasons):
    os.makedirs(NFLVERSE, exist_ok=True)
    frames = []
    for y in seasons:
        path = os.path.join(NFLVERSE, f"stats_player_week_{y}.csv")
        if not os.path.exists(path):
            urllib.request.urlretrieve(URL.format(y), path)
        frames.append(pd.read_csv(path, low_memory=False))
    d = pd.concat(frames)
    d = d[d.season_type == "REG"].copy()
    num = ["attempts", "passing_yards", "passing_tds", "rushing_yards", "rushing_tds",
           "receptions", "receiving_yards", "receiving_tds"]
    d[num] = d[num].fillna(0)
    d["td"] = d.rushing_tds + d.receiving_tds
    return d


def around_expected(d, stat, who):
    """Each game with the player's average over his other games that season."""
    keep = (d.position == "QB") & (d.attempts >= 15) if who == "qb" else d.position.isin(["RB", "WR", "TE"])
    x = d[keep].copy()
    g = x.groupby(["player_id", "season"])[stat]
    total, n = g.transform("sum"), g.transform("size")
    x["m"] = (total - x[stat]) / (n - 1)
    x["n_other"] = n - 1
    x["x"] = x[stat]
    return x[(x.n_other >= MIN_OTHER) & (x.m > 0)]


def fit_yards(x, edges):
    bands = []
    for lo, hi in zip(edges[:-1], edges[1:]):
        b = x[(x.m > lo) & (x.m <= hi)]
        z = b.x / b.m
        z = z / z.mean()                                   # mean exactly 1
        z = 1 + (z - 1) * np.sqrt(1 - 1 / b.n_other.mean())  # remove the noise in 'expected'
        q = np.quantile(z, QS)
        bands.append(dict(lo=lo, hi=hi, n=int(len(b)), median=float(np.quantile(z, 0.5)),
                          quantiles=[round(float(v), 4) for v in q]))
    return bands


def fit_count(x):
    nb = x.n_other.mean()
    return float(((x.x - x.m) ** 2).mean() / x.m.mean() / (1 + 1 / nb))


def check(shapes, d):
    """2025: over-rate at the lines a shape puts at its 25/50/75th percentile."""
    import shapes as sh
    print(f"\nCHECK on {CHECK[0]} (fit on {FIT[0]}-{FIT[-1]}): how often the player went over the "
          "line the shape puts at its\n25th / 50th / 75th percentile (target 75% / 50% / 25%)\n")
    print(f"  {'market':<22}{'band (expected)':<16}{'n':>6}   {'new: 25/50/75':<20}{'old fixed ratio':<20}")
    rows = []
    for mk, (stat, who, edges) in YARDS.items():
        x = around_expected(d, stat, who)
        for lo, hi in zip(edges[:-1], edges[1:]):
            b = x[(x.m > lo) & (x.m <= hi)]
            if b.empty:
                continue
            new, old = [], []
            for p in (0.25, 0.5, 0.75):
                band = sh.band_for_mean(shapes, mk, b.m.to_numpy())
                lines = np.array([sh.quantile(bd, p) for bd in band]) * b.m.to_numpy()
                new.append((b.x.to_numpy() > lines).mean())
                cv, ratio = OLD[mk]
                med = b.m / ratio
                from scipy import stats
                old.append((b.x > med * (1 + cv * stats.norm.ppf(p))).mean())
            rows.append(dict(market=mk, band=f"{lo}-{hi}", n=len(b),
                             **{f"new_{t}": round(v, 4) for t, v in zip((25, 50, 75), new)},
                             **{f"old_{t}": round(v, 4) for t, v in zip((25, 50, 75), old)}))
            print(f"  {mk:<22}{f'{lo}-{hi}':<16}{len(b):>6}   "
                  f"{'/'.join(f'{v:.0%}' for v in new):<20}{'/'.join(f'{v:.0%}' for v in old):<20}")
    pd.DataFrame(rows).to_csv(CHECK_OUT, index=False)
    print("\n  counts: dispersion fit vs check, and P(0) predicted vs actual on the check season")
    for mk, (stat, who, lo) in COUNTS.items():
        x = around_expected(d, stat, who)
        x = x[x.m >= lo]
        phi = fit_count(x)
        fitted = shapes["measured_anytime_td"] if mk == "player_anytime_td" else shapes["counts"][mk]
        p0 = sh.count_pmf(0, x.m.to_numpy(), fitted).mean()
        p0_pois = np.exp(-x.m).mean()
        print(f"  {mk:<22} dispersion fit {fitted:.2f}, {CHECK[0]} {phi:.2f}   "
              f"P(0): actual {(x.x == 0).mean():.1%}, shape {p0:.1%}, Poisson {p0_pois:.1%}")
    print("  anytime TD uses Poisson (dispersion 1): the fitted shape misses P(0) by more")


def main():
    fit = game_logs(FIT)
    shapes = {"fit_seasons": list(FIT), "yards": {}, "counts": {}}
    for mk, (stat, who, edges) in YARDS.items():
        shapes["yards"][mk] = fit_yards(around_expected(fit, stat, who), edges)
    for mk, (stat, who, lo) in COUNTS.items():
        x = around_expected(fit, stat, who)
        shapes["counts"][mk] = round(fit_count(x[x.m >= lo]), 3)
    shapes["measured_anytime_td"] = shapes["counts"]["player_anytime_td"]
    shapes["counts"]["player_anytime_td"] = 1.0          # Poisson kept: see the docstring
    json.dump(shapes, open(OUT, "w"), indent=1)
    print(f"wrote {os.path.relpath(OUT)}")
    print("\n  mean / median by band (old fixed ratio in brackets):")
    for mk, bands in shapes["yards"].items():
        print(f"  {mk:<22}" + "  ".join(f"{b['lo']}-{b['hi']}: {1 / b['median']:.2f}" for b in bands)
              + f"   [{OLD[mk][1]}]")
    print("  dispersion (1 = Poisson): " + ", ".join(f"{k} {v}" for k, v in shapes["counts"].items()))
    check(shapes, game_logs(CHECK))


if __name__ == "__main__":
    main()
