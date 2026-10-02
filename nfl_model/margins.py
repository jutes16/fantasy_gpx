"""
How NFL games actually finish around a closing line, key numbers included.

The home team's final margin, given the market's closing line, built from real
results (data/games_clean.csv, nflverse): historical margins are used UNSHIFTED
(and mirrored, so each game counts from both teams' side), weighted by how
close each game's closing spread was to this one (Gaussian kernel, 1.25 pts),
then exponentially tilted so the mean sits exactly on the close. That keeps
the spikes at 3, 7, 10 that a normal curve -- or sliding mass between
integers -- smears out. The same method is used in ../nfl_bets/common.py, where it matched ELWAY's
simulated distributions to 1.2 probability points.

Lines here are this model's convention: HOME-perspective, negative = home
favoured (so the expected home margin is -close).
"""

import os

import numpy as np
import pandas as pd

GAMES = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data", "games_clean.csv")
KS = np.arange(-80, 81)
_CACHE = {}


def _history(exclude_season=None):
    key = exclude_season
    if key not in _CACHE:
        g = pd.read_csv(GAMES, low_memory=False)
        g = g[(g.game_type == "REG") & g.result.notna() & g.spread_line.notna()]
        if exclude_season is not None:
            g = g[g.season != exclude_season]
        # nflverse: spread_line > 0 = home favoured = expected home margin.
        # Mirror every game (the other team's view: -spread, -margin): doubles
        # the sample and stops home/away noise from skewing one side.
        S = g.spread_line.to_numpy(float)
        R = np.clip(g.result.to_numpy(int), KS[0], KS[-1])
        _CACHE[key] = (np.concatenate([S, -S]), np.concatenate([R, -R]))
    return _CACHE[key]


def _tilt(p, target):
    """Reweight p_k by exp(theta * k) so the mean is `target`. Unlike sliding
    mass to a neighbouring integer, this keeps every key-number spike in
    proportion to its neighbours."""
    lo, hi = -1.0, 1.0
    for _ in range(60):
        th = (lo + hi) / 2
        q = p * np.exp(th * (KS - KS.mean()) / 10)
        q = q / q.sum()
        if (KS * q).sum() < target:
            lo = th
        else:
            hi = th
    return q


def home_margin_dist(close, bw=1.25, exclude_season=None):
    """P(home margin = k) for k in KS, for a game closing at `close`
    (home-perspective, negative = home favoured). `exclude_season` leaves a
    season out, for out-of-sample tests."""
    ck = (round(float(close) * 2) / 2, bw, exclude_season)
    if ck in _CACHE:
        return _CACHE[ck]
    center = -float(close)                      # expected home margin
    S, R = _history(exclude_season)
    w = np.exp(-0.5 * ((S - center) / bw) ** 2)
    p = np.bincount(R - KS[0], weights=w, minlength=len(KS))
    p = _tilt(p / p.sum(), center)              # mean exactly on the close
    _CACHE[ck] = p
    return p


def cover_prob(line, side, close, exclude_season=None):
    """Expected pool score of taking `side` ("home"/"away") at `line`
    (home-perspective) when the market closed at `close`: P(cover) + 0.5 P(push).

    Home covers when margin + line > 0; away covers when margin + line < 0.
    """
    p = home_margin_dist(close, exclude_season=exclude_season)
    x = KS + float(line)
    win = p[x > 0].sum() if side == "home" else p[x < 0].sum()
    return float(win + 0.5 * p[x == 0].sum())


def clv_prob(line, side, close, exclude_season=None):
    """Closing-line value in cover probability: your line vs the closing line,
    both scored on the distribution around the close. +3.5 vs a +3 close is
    worth far more than +4.5 vs +4."""
    return (cover_prob(line, side, close, exclude_season)
            - cover_prob(close, side, close, exclude_season))
