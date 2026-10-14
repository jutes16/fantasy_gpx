"""
Prop line <-> projected mean, using the shapes measured by prop_shapes.py
(data/prop_shapes.json). Used by fantasy_props.py and ../nfl_bets/props.py.

  yards: game = mean x Z, with Z's measured distribution for the line's
         volume band, so P(over line) = P(Z > line / mean), and the mean
         implied by a price p is line / Z's (1 - p) quantile. Exact inverses.
  counts: Poisson widened (negative binomial) or narrowed (binomial) to the
          measured dispersion.
"""

import json
import os
from functools import lru_cache

import numpy as np
from scipy import optimize, stats

HERE = os.path.dirname(os.path.abspath(__file__))
PATH = os.path.join(HERE, "data", "prop_shapes.json")
YARD_MARKETS = {"player_pass_yds", "player_rush_yds", "player_reception_yds"}
COUNT_MARKETS = {"player_receptions", "player_pass_tds", "player_anytime_td"}


@lru_cache(maxsize=1)
def load():
    return json.load(open(PATH))


def _yard_key(market, qb):
    return "qb_rush_yds" if (qb and market == "player_rush_yds") else market


# ------------------------------------------------------------------ yards
def band_for_mean(shapes, key, m):
    """The band for each expected value (used when fitting / checking)."""
    bands = shapes["yards"][key]
    return [next((b for b in bands if b["lo"] < v <= b["hi"]),
                 bands[0] if v <= bands[0]["lo"] else bands[-1]) for v in np.atleast_1d(m)]


def band_for_line(shapes, key, line):
    """The band a line belongs to: the one whose typical line (mean x median) covers it."""
    bands = shapes["yards"][key]
    for b in bands:
        if b["lo"] < line / b["median"] <= b["hi"]:
            return b
    return min(bands, key=lambda b: abs(np.log(line / b["median"] / np.sqrt(b["lo"] * b["hi"]))))


def _q(band):
    q = np.asarray(band["quantiles"])
    return q + np.arange(len(q)) * 1e-9            # strictly increasing for interpolation


def quantile(band, p):
    q = _q(band)
    return float(np.interp(p, np.linspace(0, 1, len(q)), q))


def yards_p_over(market, mean, line, qb=False):
    if mean <= 0:
        return 0.0
    b = band_for_line(load(), _yard_key(market, qb), line)
    q = _q(b)
    return float(1 - np.interp(line / mean, q, np.linspace(0, 1, len(q))))


def yards_implied_mean(market, line, p, qb=False):
    b = band_for_line(load(), _yard_key(market, qb), line)
    return line / quantile(b, 1 - p)


# ------------------------------------------------------------------ counts
def _binom_mix(m, phi):
    """Two binomials with mean m whose mixture has variance phi x m (phi < 1)."""
    n1 = max(np.floor(m / (1 - phi)), np.ceil(m), 1)
    n2 = n1 + 1
    v1, v2 = 1 - m / n1, 1 - m / n2                 # variance / mean of each
    w = float(np.clip((phi - v2) / (v1 - v2), 0, 1)) if v1 != v2 else 1.0
    return [(w, int(n1), m / n1), (1 - w, int(n2), m / n2)]


def count_pmf(k, m, phi):
    m = np.asarray(m, dtype=float)
    if abs(phi - 1) < 1e-6:
        return stats.poisson.pmf(k, m)
    if phi > 1:
        return stats.nbinom.pmf(k, m / (phi - 1), 1 / phi)
    out = [sum(w * stats.binom.pmf(k, n, p) for w, n, p in _binom_mix(v, phi)) for v in np.atleast_1d(m)]
    return np.array(out) if m.ndim else out[0]


def count_cdf(k, m, phi):
    """P(X <= k)."""
    if k < 0:
        return 0.0
    if abs(phi - 1) < 1e-6:
        return float(stats.poisson.cdf(k, m))
    if phi > 1:
        return float(stats.nbinom.cdf(k, m / (phi - 1), 1 / phi))
    return float(sum(w * stats.binom.cdf(k, n, p) for w, n, p in _binom_mix(m, phi)))


def count_p_over(market, mean, line):
    """P(over line); for anytime TD, P(1+ TDs)."""
    if mean <= 0:
        return 0.0
    phi = load()["counts"][market]
    k = 0 if market == "player_anytime_td" else np.floor(line)   # over 4.5 / over 4 = 5+
    return 1 - count_cdf(k, mean, phi)


def count_implied_mean(market, line, p):
    f = lambda m: count_p_over(market, m, line) - p
    return optimize.brentq(f, 1e-4, 60)


# ------------------------------------------------------------------ either
def p_over(market, mean, line, qb=False):
    if market in YARD_MARKETS:
        return yards_p_over(market, mean, line, qb)
    return count_p_over(market, mean, line)


def implied_mean(market, line, p, qb=False):
    p = float(np.clip(p, 0.02, 0.98))
    if market in YARD_MARKETS:
        return yards_implied_mean(market, line, p, qb)
    return count_implied_mean(market, line, p)
