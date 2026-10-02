"""Shared pieces for nfl_bets: team codes, the nflverse schedule, ELWAY pastes, the
margin model, and Kalshi fees. Kept separate from ../nfl_model on purpose."""

import os
import re
from datetime import datetime, timezone
from zoneinfo import ZoneInfo

import numpy as np
import pandas as pd
from scipy import stats

SEASON = 2026
HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, "data")
NFLVERSE_URL = "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"
NFLVERSE_CACHE = os.path.join(DATA, "nflverse_games.csv")
ET = ZoneInfo("America/New_York")

KALSHI_FEE = 0.07            # taker fee = 0.07 * P * (1 - P) per $1 contract
MARGIN_SD = 13.5             # fallback sd of the margin around the projection

CANON = {"LA": "LAR", "WSH": "WAS", "JAC": "JAX", "LVR": "LV", "OAK": "LV",
         "SD": "LAC", "STL": "LAR"}


def canon(t):
    t = str(t).strip().upper()
    return CANON.get(t, t)


# ------------------------------------------------------------------ nflverse
def nflverse(refresh=True):
    """nflverse games.csv; re-downloaded each run (lines move), cached locally."""
    if refresh or not os.path.exists(NFLVERSE_CACHE):
        g = pd.read_csv(NFLVERSE_URL, low_memory=False)
        os.makedirs(DATA, exist_ok=True)
        g.to_csv(NFLVERSE_CACHE, index=False)
    return pd.read_csv(NFLVERSE_CACHE, low_memory=False)


def schedule(season, week, refresh=True):
    """One week: teams, kickoff (UTC), current lines/odds, and results if final."""
    g = nflverse(refresh)
    g = g[(g.season == season) & (g.week == week) & (g.game_type == "REG")].copy()
    g["away"], g["home"] = g.away_team.map(canon), g.home_team.map(canon)
    g["kickoff"] = [
        datetime.strptime(f"{d} {t}", "%Y-%m-%d %H:%M").replace(tzinfo=ET).astimezone(timezone.utc)
        for d, t in zip(g.gameday, g.gametime)]
    return g.reset_index(drop=True)


# ------------------------------------------------------------------ ELWAY
NUM = r"[+-]?\d+(?:\.\d+)?"
ROW = re.compile(
    rf"(?P<wk>\d{{1,2}})\s+(?:N\s+)?"
    rf"(?P<home>[A-Z]{{2,3}})\s+(?P<hpts>{NUM})\s+(?P<hwp>{NUM})%\s+"
    rf"(?P<away>[A-Z]{{2,3}})\s+(?P<apts>{NUM})\s+(?P<awp>{NUM})%\s+"
    rf"(?P<spread>{NUM}|PK|pk|Pick|EVEN)\s+(?P<total>{NUM})")
ELWAY_DIRS = [os.path.join(DATA, "elway"),
              os.path.join(HERE, "..", "nfl_model", "data", "elway")]   # pool's pastes


def parse_elway(text):
    rows = []
    for m in ROW.finditer(" ".join(text.split())):
        sp = m["spread"]
        rows.append(dict(week=int(m["wk"]), home=canon(m["home"]), away=canon(m["away"]),
                         elway_line=float(sp) if re.match(NUM, sp) else 0.0,
                         elway_total=float(m["total"]),
                         elway_home_wp=float(m["hwp"]) / 100))
    return pd.DataFrame(rows)


def elway_path(week, season=SEASON):
    for d in ELWAY_DIRS:
        p = os.path.join(d, f"{season}_wk{week:02d}.txt")
        if os.path.exists(p):
            return p
    return None


def load_elway(week, season=SEASON):
    p = elway_path(week, season)
    if p is None:
        return None
    e = parse_elway(open(p).read())
    return e[e.week == week]


# ------------------------------------------------------------------ margin model
class Margin:
    """Home margin ~ Normal(mu, sd), fitted to BOTH numbers ELWAY publishes:
    mu = -elway_line (projected home margin) and sd chosen so P(margin > 0)
    equals ELWAY's home win probability. sd is clamped to [10, 17]; if the
    projection is a pick'em the fallback 13.5 is used. Key numbers (3, 7) are
    ignored -- that is the main approximation."""

    def __init__(self, elway_line, home_wp):
        self.mu = -float(elway_line)
        z = stats.norm.ppf(np.clip(home_wp, 0.01, 0.99))
        sd = self.mu / z if abs(z) > 0.02 and self.mu * z > 0 else MARGIN_SD
        self.sd = float(np.clip(sd, 10.0, 17.0))
        self.home_wp = float(home_wp)

    def p_home_by_over(self, k):
        """P(home margin > k); k may be negative (home loses by less than |k|)."""
        return float(1 - stats.norm.cdf(k, self.mu, self.sd))

    def p_team_by_over(self, team_is_home, k):
        return self.p_home_by_over(k) if team_is_home else 1 - self.p_home_by_over(-k)

    def p_team_wins(self, team_is_home):
        return self.home_wp if team_is_home else 1 - self.home_wp


class EmpiricalMargin:
    """ELWAY's own simulated margin distribution, when we have it.

    `dist` maps home margin (integer points, home minus away) -> probability.
    Same interface as Margin, so bets.py doesn't care which it gets. Key
    numbers (3, 7, ...) carry whatever mass ELWAY's simulations give them.
    """

    def __init__(self, dist, home_wp=None):
        m = pd.Series(dist, dtype=float).groupby(level=0).sum().sort_index()
        self.dist = m / m.sum()
        self.mu = float((self.dist.index * self.dist).sum())
        self.sd = float(np.sqrt(((self.dist.index - self.mu) ** 2 * self.dist).sum()))
        # outright wins only: a tie settles NO on Kalshi's win contract (and
        # pushes a sportsbook moneyline), and ELWAY publishes win% the same way
        self.home_wp = float(self.dist[self.dist.index > 0].sum())
        self.away_wp = float(self.dist[self.dist.index < 0].sum())
        self.p_tie = float(self.dist.get(0, 0.0))
        self.published_wp = home_wp

    def p_home_by_over(self, k):
        return float(self.dist[self.dist.index > k].sum())

    def p_team_by_over(self, team_is_home, k):
        if team_is_home:
            return self.p_home_by_over(k)
        return float(self.dist[self.dist.index < -k].sum())     # away margin > k

    def p_team_wins(self, team_is_home):
        return self.home_wp if team_is_home else self.away_wp


_HIST = {}


def _history(first=2015, last=SEASON - 1):
    """(closing spreads, home margins) of every completed regular-season game,
    nflverse convention (spread > 0 = home favoured), each game mirrored
    (-spread, -margin) to double the sample and cancel home/away noise.
    Cached per process."""
    if (first, last) not in _HIST:
        g = nflverse(refresh=False)
        h = g[(g.game_type == "REG") & g.result.notna() & g.spread_line.notna()
              & g.season.between(first, last)]
        S, R = h.spread_line.to_numpy(float), h.result.to_numpy().astype(int)
        _HIST[(first, last)] = (np.concatenate([S, -S]), np.concatenate([R, -R]))
    return _HIST[(first, last)]


def _tilt(ks, p, target):
    """Reweight p_k by exp(theta * k) so the mean is `target`; keeps every
    key-number spike in proportion to its neighbours."""
    lo, hi = -1.0, 1.0
    for _ in range(60):
        th = (lo + hi) / 2
        q = p * np.exp(th * (ks - ks.mean()) / 10)
        q = q / q.sum()
        if (ks * q).sum() < target:
            lo = th
        else:
            hi = th
    return q


def historical_dist(center, bw=1.25):
    """Home-margin distribution for a game projected at `center` points.

    Historical margins are used UNSHIFTED (and mirrored), weighted by a
    Gaussian kernel on how close each game's closing spread was to `center`
    (bandwidth `bw` points), so the spikes at 3, 7, 10 survive. The mean is
    then put exactly on `center` by exponential tilting -- sliding mass to a
    neighbouring integer instead would move the spike at 3 onto 4.
    """
    S, R = _history()
    ks = np.arange(-80, 81)
    w = np.exp(-0.5 * ((S - center) / bw) ** 2)
    p = np.bincount(np.clip(R, ks[0], ks[-1]) - ks[0], weights=w, minlength=len(ks))
    p = _tilt(ks, p / p.sum(), center)
    return dict(zip(ks.tolist(), p))


def HistoricalMargin(elway_line, home_wp):
    """Fallback when ELWAY's own distribution wasn't pulled: the historical
    shape, centred so its home win probability matches ELWAY's published one.

    Checked against ELWAY's actual 2026 week-4 distributions (16 games, strikes
    -10.5..+10.5): mean error 1.2 prob. points, max 5.5 -- vs 2.5 / 10.4 for
    the normal curve. It also keeps the value of crossing 3 (6.4 pts vs
    ELWAY's 6.8; the normal gives 2.9).
    """
    lo, hi = -float(elway_line) - 8, -float(elway_line) + 8
    for _ in range(40):                      # bisection on the centre
        mid = (lo + hi) / 2
        if EmpiricalMargin(historical_dist(mid)).home_wp < home_wp:
            lo = mid
        else:
            hi = mid
    m = EmpiricalMargin(historical_dist((lo + hi) / 2), home_wp)
    m.kind = "hist"
    return m


def dist_path(week, season=SEASON):
    for d in ELWAY_DIRS:
        p = os.path.join(d, f"dist_{season}_wk{week:02d}.csv")
        if os.path.exists(p):
            return p
    return None


def load_dists(week, season=SEASON):
    """{(away, home): {home_margin: prob}} from data/elway/dist_<season>_wk<NN>.csv
    (columns: away, home, home_margin, prob). Empty dict if not collected."""
    p = dist_path(week, season)
    if p is None:
        return {}
    d = pd.read_csv(p)
    d["away"], d["home"] = d.away.map(canon), d.home.map(canon)
    return {(a, h): dict(zip(g.home_margin, g.prob)) for (a, h), g in d.groupby(["away", "home"])}


# ------------------------------------------------------------------ prices
def kalshi_fee(p):
    return KALSHI_FEE * p * (1 - p)


def buy_yes(ask):
    """All-in cost of one $1 YES contract bought at the ask."""
    return ask + kalshi_fee(ask)


def buy_no(bid):
    """All-in cost of one $1 NO contract (i.e. selling YES at the bid)."""
    p = 1 - bid
    return p + kalshi_fee(p)


def american_to_decimal(a):
    return 1 + (a / 100 if a > 0 else 100 / -a)
