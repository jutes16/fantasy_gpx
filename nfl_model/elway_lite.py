"""
ELWAY-lite: drive-based team ratings, walked forward with no look-ahead.

A scaled-down rebuild of the team-rating layer of Silver Bulletin's ELWAY
(2026 methodology), from free nflverse data. Three layers, each scored on
its own so you can see what it adds:
    team     drive-based team ratings (drives.py), below
    QB       projected starter's QBERT-lite rating minus the rating of the
             QBs the team's ratings were earned with (qbert.py)
    roster   AV-style Week 1 roster ratings (roster.py), a fixed term plus
             one that fades over the first weeks, from 2013 on
No injuries, weather, travel or coaching yet.

Ratings (each opponent-adjusted, each team gets an offense and a defense
number, refitted every week from games already played):
    ppd     offensive points per drive, adjusted for score/clock (drives.py)
    fd      first downs per drive        (process: predicts future scoring)
    ypp     yards per play
    pace    drives per game
    defst   defensive points scored per opponent drive, against the
            offense's tendency to give them up (pick-sixes etc.), plus
            return TDs per kick/punt received; shrunk hard toward zero

Each rating is a weighted ridge regression on team-games:
    value = league + off[team] + def[opp] + home
weighted by sample size x exp(-age / HALF_LIFE), where age counts weeks
played, plus OFFSEASON weeks for every summer in between. RIDGE_GAMES is
the shrinkage toward league average, in games: a rating behaves as if it
also contained that many games of a league-average team.

Ratings are turned into a margin and a total by a linear calibration. The
calibration and every hyperparameter are fitted on EVEN seasons (2004-24)
only, on margin accuracy, never on ATS results; ODD seasons (2003-25) are
the out-of-sample test. Alternating years keeps every era of the league on
both sides of the split. Ratings themselves always come from games played
before the one being predicted.

    python3 elway_lite.py            backtest + 2026 vs ELWAY + this week
    python3 elway_lite.py tune       hyperparameter grid (even seasons)
"""

import glob
import itertools
import os
import sys

import numpy as np
import pandas as pd

import qbert
from fetch_splits import canon
from import_elway import parse as parse_elway

DRIVES = "data/drives.csv"
GAMES = "data/pbp/games.csv"
GAMES_URL = "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"
OUT = "data/elway_lite_preds.csv"
ROSTER = "data/roster_ratings.csv"   # roster.py

HALF_LIFE = 18.0     # weeks
OFFSEASON = 10.0     # extra weeks of age per summer
RIDGE_GAMES = 4.0    # shrinkage, in games of league average
NOISY_GAMES = 40.0   # shrinkage for the defensive/return scoring terms
LOOKBACK = 3         # seasons of history fed to each fit
FIRST = 2003         # first season predicted (2001-02 are burn-in)
LAST_DONE = 2025     # last completed season


def is_fit(season):
    """Even completed seasons fit; odd seasons test."""
    return (season % 2 == 0) & (season <= LAST_DONE)


def is_test(season):
    return (season % 2 == 1) & (season <= LAST_DONE)

WEEKS_PER_SEASON = 22
SPAN = WEEKS_PER_SEASON + OFFSEASON     # clock units per season
QB_PLAYS = 40.0      # QB plays per game, to turn EPA/play into points
ROSTER_FROM = 2013   # first season with roster ratings (snap counts start 2012)
ROSTER_FADE = 4.0    # weeks: the early-season roster term decays like exp(-(wk-1)/4)


def clock(season, week):
    """Weeks played, with OFFSEASON extra weeks for every summer."""
    return season * SPAN + week

CHANNELS = {
    # name: (numerator, denominator, shrinkage)
    "ppd": ("pts_script", "drives", RIDGE_GAMES),
    "fd": ("fd", "drives", RIDGE_GAMES),
    "ypp": ("yds", "plays", RIDGE_GAMES),
    "defst": ("def_st_pts", "def_st_opps", NOISY_GAMES),
}


def load():
    t = pd.read_csv(DRIVES)
    for c in ["team", "opp"]:
        t[c] = t[c].map(canon)
    # defensive + return points as one noisy channel, per opportunity
    opp_drives = t.set_index(["game_id", "team"]).drives_all
    t["opp_drives"] = [opp_drives.get((g, o), np.nan)
                       for g, o in zip(t.game_id, t.opp)]
    t["def_st_pts"] = t.def_pts + t.st_pts
    t["def_st_opps"] = t.opp_drives + t.st_opps
    t["clock"] = clock(t.season, t.week)
    return t


def load_games(refresh=False):
    if refresh or not os.path.exists(GAMES):
        os.makedirs(os.path.dirname(GAMES), exist_ok=True)
        pd.read_csv(GAMES_URL).to_csv(GAMES, index=False)
    g = pd.read_csv(GAMES)
    g = g[g.season >= 2001].copy()
    g["home_team"] = g.home_team.map(canon)
    g["away_team"] = g.away_team.map(canon)
    g["neutral"] = (g.location == "Neutral").astype(int)
    g["clock"] = clock(g.season, g.week)
    if os.path.exists(ROSTER):
        r = pd.read_csv(ROSTER).set_index(["season", "team"])
        for side, col in [("h", "home_team"), ("a", "away_team")]:
            for c in ["roster_off", "roster_def"]:
                g[f"{side}_{c}"] = [r[c].get((s, tm), np.nan)
                                    for s, tm in zip(g.season, g[col])]
    return g


def fit(rows, y, n, opp_side, teams, shrink, half_life, now):
    """Weighted ridge: y = mu + a[team] + b[opp] + h*home. Returns (mu, a, b)."""
    k = len(teams)
    X = np.zeros((len(rows), 2 * k + 2))
    X[np.arange(len(rows)), rows.team.map(teams).values] = 1
    X[np.arange(len(rows)), k + rows.opp.map(teams).values] = 1 if opp_side else 0
    X[:, 2 * k] = rows.home.values
    X[:, -1] = 1.0
    w = n / np.nanmean(n) * np.exp(-(now - rows.clock.values) / half_life)
    pen = np.full(2 * k + 2, shrink)
    pen[2 * k:] = 0.0                      # home and league mean unpenalized
    A = (X * w[:, None]).T @ X + np.diag(pen)
    beta = np.linalg.solve(A, (X * w[:, None]).T @ y)
    return beta[-1], beta[:k], beta[k:2 * k]


def ratings(t, now, half_life=HALF_LIFE, ridge=RIDGE_GAMES):
    """Every channel's ratings from team-games strictly before `now`."""
    past = t[(t.clock < now) & (t.clock >= now - (LOOKBACK + 1) * SPAN)]
    teams = {tm: i for i, tm in enumerate(sorted(past.team.unique()))}
    r = {"teams": teams}
    for name, (num, den, shrink) in CHANNELS.items():
        ok = past[den] > 0
        p = past[ok]
        s = ridge if shrink == RIDGE_GAMES else shrink
        r[name] = fit(p, (p[num] / p[den]).values, p[den].values, True,
                      teams, s, half_life, now)
    # pace: drives in the game depend on both teams, so one rating per team
    # enters on both sides (team and opp share the same coefficient)
    p = past.copy()
    mu, a, b = fit(p, p.drives_all.values, np.ones(len(p)), True, teams,
                   ridge, half_life, now)
    r["pace"] = (mu, (a + b) / 2, (a + b) / 2)
    r["drives_eff"] = (past.drives.sum() / past.drives_all.sum())
    return r


def features(r, home, away):
    """Neutral-site expectations for one game, home-minus-away and sums."""
    ti = r["teams"]
    h, a = ti.get(home), ti.get(away)

    def exp(ch, off, dfn):
        mu, o, d = r[ch]
        return mu + (o[off] if off is not None else 0) + (d[dfn] if dfn is not None else 0)

    mu_p, pa, _ = r["pace"]
    drives_all = mu_p + (pa[h] if h is not None else 0) + (pa[a] if a is not None else 0)
    nd = drives_all * r["drives_eff"]
    f = {"drives": drives_all}
    for ch in ["ppd", "fd", "ypp"]:
        eh, ea = exp(ch, h, a), exp(ch, a, h)
        f[f"d_{ch}"] = (eh - ea) * (nd if ch != "ypp" else 1)
        f[f"s_{ch}"] = (eh + ea) * (nd if ch != "ypp" else 1)
    eh, ea = exp("defst", h, a), exp("defst", a, h)
    f["d_defst"] = (eh - ea) * drives_all
    f["s_defst"] = (eh + ea) * drives_all
    return f


def qb_features(qb, now, half_life, x):
    """Projected starter vs the QBs each team's ratings were earned with."""
    f = {}
    for side, team, qid, name in [("h", x.home_team, x.home_qb_id, x.home_qb_name),
                                  ("a", x.away_team, x.away_qb_id, x.away_qb_name)]:
        base = qb.team_baseline(team, now, half_life, SPAN)
        r = qb.get(qid)
        f[f"{side}_qb"] = name
        f[f"{side}_qb_rating"] = r
        f[f"{side}_qb_adj"] = 0.0 if np.isnan(base) else (r - base) * QB_PLAYS
    f["d_qb"] = f["h_qb_adj"] - f["a_qb_adj"]
    f["s_qb"] = f["h_qb_adj"] + f["a_qb_adj"]
    return f


def roster_features(x):
    """Week 1 roster ratings (roster.py): a fixed term and a fading one."""
    home = x.h_roster_off + x.h_roster_def
    away = x.a_roster_off + x.a_roster_def
    fade = np.exp(-(min(x.week, 18) - 1) / ROSTER_FADE)
    d = home - away
    s = (x.h_roster_off + x.a_roster_off) - (x.h_roster_def + x.a_roster_def)
    return {"d_roster": d, "d_roster_early": d * fade, "s_roster_early": s * fade}


def walk_forward(t, g, half_life=HALF_LIFE, ridge=RIDGE_GAMES, last=None,
                 qb=None):
    """Features for every game from FIRST on, each from prior games only."""
    out = []
    g = g[g.season >= FIRST]
    if last:
        g = g[g.season <= last]
    for now, wk in g.groupby("clock"):
        r = ratings(t, now, half_life, ridge)
        if qb is not None:
            qb.fit(now, SPAN)
        for x in wk.itertuples():
            f = features(r, x.home_team, x.away_team)
            f.update(game_id=x.game_id, season=x.season, week=x.week,
                     game_type=x.game_type, home=x.home_team, away=x.away_team,
                     neutral=x.neutral, result=x.result, total=x.total,
                     spread_line=x.spread_line, total_line=x.total_line)
            if qb is not None:
                f.update(qb_features(qb, now, half_life, x))
            if hasattr(x, "h_roster_off"):
                f.update(roster_features(x))
            out.append(f)
    return pd.DataFrame(out)


MARGIN_X = ["d_ppd", "d_fd", "d_ypp", "d_defst"]
TOTAL_X = ["s_ppd", "s_fd", "s_ypp", "s_defst", "drives"]
QB_MARGIN_X = ["d_qb"]
QB_TOTAL_X = ["s_qb"]
ROSTER_MARGIN_X = ["d_roster", "d_roster_early"]
ROSTER_TOTAL_X = ["s_roster_early"]


def ols(X, y):
    X1 = np.column_stack([X, np.ones(len(X))])
    return np.linalg.lstsq(X1, y, rcond=None)[0]


def calibrate(f, margin_x=MARGIN_X, total_x=TOTAL_X, fit_from=FIRST):
    """Margin and total for every game from one fit on the even seasons."""
    f = f.copy()
    f["homefield"] = 1 - f.neutral
    tr = f[is_fit(f.season) & (f.season >= fit_from) & f.result.notna()]
    bm = np.linalg.lstsq(tr[margin_x + ["homefield"]].values,
                         tr.result.values, rcond=None)[0]
    f["model_margin"] = f[margin_x + ["homefield"]].values @ bm
    bt = ols(tr[total_x].values, tr.total.values)
    f["model_total"] = np.column_stack([f[total_x].values,
                                        np.ones(len(f))]) @ bt
    return f, dict(zip(margin_x + ["hfa"], bm))


def wilson(w, n):
    if n == 0:
        return (np.nan, np.nan, np.nan)
    z = 1.959963985
    p = w / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return (p, c - h, c + h)


def report(f, label):
    d = f.dropna(subset=["model_margin", "result", "spread_line"])
    mae_m = (d.result - d.model_margin).abs().mean()
    mae_c = (d.result - d.spread_line).abs().mean()
    dt = d.dropna(subset=["total_line"])
    tm = (dt.total - dt.model_total).abs().mean()
    tc = (dt.total - dt.total_line).abs().mean()
    print(f"\n{label}: {len(d)} games")
    print(f"  margin MAE   model {mae_m:6.3f}   close {mae_c:6.3f}   "
          f"gap {mae_m - mae_c:+.3f}")
    print(f"  total  MAE   model {tm:6.3f}   close {tc:6.3f}   gap {tm - tc:+.3f}")
    return d


def ats_table(d):
    dis = d.model_margin - d.spread_line          # >0: model likes home more
    cov = d.result - d.spread_line
    keep = cov != 0
    won = np.sign(dis) == np.sign(cov)
    bands = pd.cut(dis.abs(), [0, 1, 2, 3, 5, 99],
                   labels=["0-1pt", "1-2pt", "2-3pt", "3-5pt", "5+pt"])
    rows = []
    for b in list(bands.cat.categories) + ["ALL"]:
        m = keep & ((bands == b) if b != "ALL" else True)
        p, lo, hi = wilson(won[m].sum(), m.sum())
        rows.append(dict(disagree=b, games=int(m.sum()), wins=int(won[m].sum()),
                         win_rate=round(p, 4), ci_lo=round(lo, 4), ci_hi=round(hi, 4)))
    print("\n  ATS of the model's side, by disagreement with the close:")
    print(pd.DataFrame(rows).to_string(index=False))


def blend(d):
    """Does the model add anything to the closing line?"""
    X = np.column_stack([d.spread_line, d.model_margin - d.spread_line])
    b = np.linalg.lstsq(np.column_stack([X, np.ones(len(d))]), d.result,
                        rcond=None)[0]
    res = d.result - np.column_stack([X, np.ones(len(d))]) @ b
    se = np.sqrt(res.var() * np.linalg.inv(
        np.column_stack([X, np.ones(len(d))]).T @
        np.column_stack([X, np.ones(len(d))])).diagonal())
    print(f"\n  result ~ close + w*(model - close):  w = {b[1]:+.3f} "
          f"(se {se[1]:.3f}, t {b[1] / se[1]:+.2f})")
    for w in [0.1, 0.2, 0.3]:
        mae = (d.result - (d.spread_line + w * (d.model_margin - d.spread_line))).abs().mean()
        print(f"    close + {w:.1f} x gap: MAE {mae:.3f}")


def vs_power_ratings(f, path="data/power_rating_test.csv"):
    """Same games as power_rating_test.py (margin-only ridge ratings)."""
    if not os.path.exists(path):
        return
    p = pd.read_csv(path)
    p["home"] = p.home.map(canon)
    p["away"] = p.away.map(canon)
    m = p.merge(f, on=["season", "week", "home", "away"])
    m = m[is_test(m.season)]
    close = (m.actual - m.mkt).abs().mean()
    old = (m.actual - m.model).abs().mean()
    new = (m.result - m.model_margin).abs().mean()
    print(f"\nvs power_rating_test.py, the {len(m)} odd-season games both cover:")
    print(f"  margin MAE   close {close:6.3f}   margin-only ratings {old:6.3f} "
          f"({old - close:+.3f})   drive ratings {new:6.3f} ({new - close:+.3f})")


def elway_compare(f):
    """2026: this model vs ELWAY vs the close, on games ELWAY projected."""
    rows = []
    for path in sorted(glob.glob("data/elway/2026_wk*.txt")):
        e = parse_elway(open(path).read())
        rows.append(e)
    if not rows:
        return
    e = pd.concat(rows)
    e["home"] = e.home.map(canon)
    e["away"] = e.away.map(canon)
    m = f[f.season == 2026].merge(e, on=["week", "home", "away"])
    m["elway_margin"] = -m.elway_line
    done = m.dropna(subset=["result"])
    print(f"\n2026 vs ELWAY: {len(m)} games ELWAY projected, {len(done)} played")
    if len(done):
        for c, lab in [("model_margin", "this model"), ("elway_margin", "ELWAY"),
                       ("spread_line", "close")]:
            print(f"  margin MAE  {lab:11s} {(done.result - done[c]).abs().mean():6.2f}")
    print(f"  corr(model, ELWAY) margin {np.corrcoef(m.model_margin, m.elway_margin)[0, 1]:.3f}"
          f"   mean |model - ELWAY| {(m.model_margin - m.elway_margin).abs().mean():.2f}")
    print(f"  corr(model, close) margin {np.corrcoef(m.model_margin, m.spread_line)[0, 1]:.3f}"
          f"   corr(ELWAY, close) {np.corrcoef(m.elway_margin, m.spread_line)[0, 1]:.3f}")


def upcoming(f):
    """Next unplayed week: projected home line and total."""
    u = f[(f.season == f.season.max()) & f.result.isna()]
    if u.empty:
        return
    wk = u.week.min()
    u = u[u.week == wk].copy()
    u["model_line"] = (-u.model_margin).round(1)
    u["mkt_line"] = -u.spread_line
    u["model_total"] = u.model_total.round(1)
    u["qb_adj"] = u.d_qb.round(1)
    print(f"\n{int(u.season.iat[0])} week {wk} (home line, negative = home favored; "
          "QB = projected starters from nflverse; no injuries or weather):")
    print(u[["away", "a_qb", "home", "h_qb", "qb_adj", "model_line", "mkt_line",
             "model_total", "total_line"]].to_string(index=False))


def fit_mae(f, margin_x=MARGIN_X, total_x=TOTAL_X):
    c, _ = calibrate(f, margin_x, total_x)
    d = c[is_fit(c.season) & (c.game_type == "REG")].dropna(subset=["result"])
    return round((d.result - d.model_margin).abs().mean(), 3)


def tune():
    """Team half-life and shrinkage first, then the QB layer's, even seasons only."""
    t, g = load(), load_games()
    rows = []
    for hl, rg in itertools.product([9, 12, 18, 26], [2, 4, 8]):
        rows.append(dict(half_life=hl, ridge_games=rg,
                         mae=fit_mae(walk_forward(t, g, hl, rg))))
        print(rows[-1], flush=True)
    print(pd.DataFrame(rows).sort_values("mae").to_string(index=False))
    qb = qbert.QBRatings(clock)
    rows = []
    for hl, rg in itertools.product([20, 40, 80], [3, 6, 12]):
        qbert.QB_HALF_LIFE, qbert.QB_RIDGE = hl, rg
        f = walk_forward(t, g, qb=qb)
        rows.append(dict(qb_half_life=hl, qb_ridge=rg,
                         mae=fit_mae(f, MARGIN_X + QB_MARGIN_X,
                                     TOTAL_X + QB_TOTAL_X)))
        print(rows[-1], flush=True)
    print(pd.DataFrame(rows).sort_values("mae").to_string(index=False))


LAYERS = {
    "team": (MARGIN_X, TOTAL_X),
    "team+QB": (MARGIN_X + QB_MARGIN_X, TOTAL_X + QB_TOTAL_X),
    "team+QB+roster": (MARGIN_X + QB_MARGIN_X + ROSTER_MARGIN_X,
                       TOTAL_X + QB_TOTAL_X + ROSTER_TOTAL_X),
}


def layer_table(raw, fit_from, label):
    """Each layer calibrated on the same even seasons, scored on odd ones."""
    print(f"\n{label}")
    print(f"  {'layers':16s} {'margin':>7s} {'wk 1-4':>7s} {'total':>7s}")
    for name, (mx, tx) in LAYERS.items():
        if "roster" in name and fit_from < ROSTER_FROM:
            continue
        c, _ = calibrate(raw[raw.season >= fit_from - 1], mx, tx, fit_from)
        d = c[is_test(c.season) & (c.game_type == "REG")].dropna(
            subset=["result", "spread_line"])
        e = d[d.week <= 4]
        print(f"  {name:16s} {(d.result - d.model_margin).abs().mean():7.3f} "
              f"{(e.result - e.model_margin).abs().mean():7.3f} "
              f"{(d.total - d.model_total).abs().mean():7.3f}")
    print(f"  {'closing line':16s} {(d.result - d.spread_line).abs().mean():7.3f} "
          f"{(e.result - e.spread_line).abs().mean():7.3f} "
          f"{(d.total - d.total_line).abs().mean():7.3f}")


def main():
    t, g = load(), load_games(refresh="--refresh" in sys.argv)
    raw = walk_forward(t, g, qb=qbert.QBRatings(clock))
    layer_table(raw, FIRST + 1, "margin/total MAE, odd seasons 2003-25 "
                "(calibrated on even 2004-24)")
    layer_table(raw, ROSTER_FROM + 1, "margin/total MAE, odd seasons 2013-25 "
                "(calibrated on even 2014-24; roster ratings start 2013)")

    # the full model: every layer, calibrated where all of them exist
    mx, tx = LAYERS["team+QB+roster"]
    f, coefs = calibrate(raw[raw.season >= ROSTER_FROM], mx, tx, ROSTER_FROM + 1)
    f.to_csv(OUT, index=False)
    print("\nfull model, margin calibration (even seasons 2014-24):")
    print({k: round(float(v), 3) for k, v in coefs.items()})

    reg = f[f.game_type == "REG"]
    d = report(reg[is_test(reg.season)], "full model, OUT OF SAMPLE: odd seasons 2013-2025")
    ats_table(d)
    blend(d)
    vs_power_ratings(f)
    report(reg[reg.season == 2026], "2026 so far")
    elway_compare(f)
    upcoming(f)
    print(f"\nwrote {OUT}")


if __name__ == "__main__":
    if sys.argv[1:2] == ["tune"]:
        tune()
    else:
        main()
