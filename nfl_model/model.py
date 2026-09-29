"""
Weekly pick engine, built on backtested coefficients only.

Every number in COEF comes from backtest.py / keynumbers.py over
2015-2025 (2,895 games, 5,790 bet sides). Nothing here is asserted
without a measurement behind it.

Usage:
    from model import score_week
    picks = score_week(games)   # list of dicts, see sample_week.py

Input per game:
    away, home        team codes
    my_line           YOUR number, stated for the HOME team
                      (negative = home favored, e.g. -2.5)
    mkt_line          current market number, same convention
    proj_margin       optional: projected home margin (home - away).
                      Only used if a weighted source is supplied.
    proj_weight       optional 0-1 credibility weight for that projection
"""

import numpy as np

BREAKEVEN = 0.5238  # -110

# --- backtested: ATS win rate by points of line value held vs close ---
# from backtest.py table [1], both sides of every game, 2015-2025
VALUE_CURVE = {
    0.0: 0.5000,
    0.5: 0.5248,
    1.0: 0.5451,
    1.5: 0.5629,
    2.0: 0.5793,
    2.5: 0.5948,
    3.0: 0.6101,
}

# --- backtested: multiplier on line value by where the spread sits ---
# from backtest.py table [2]: 1pt of value is worth much less in the
# 0-2.5 band and most near 3 and 7.
def band_multiplier(abs_line: float) -> float:
    a = abs(abs_line)
    if a < 2.5:
        return 0.40   # 51.8% at 1pt -> barely above coin flip
    if a < 3.5:
        return 1.45   # 56.5% at 1pt -> best band
    if a < 6.5:
        return 1.00   # 54.3%
    if a < 7.5:
        return 1.35   # 56.1%
    if a < 10.5:
        return 0.82   # 53.7%
    return 0.70       # 53.1%


# --- backtested: half-point gain by what it crosses (keynumbers.py) ---
# Note 7 is far weaker than folklore claims: push rate on 3 is 10.5%,
# on 7 only 6.1%, and on 10 it is 8.3%.
PUSH_RATE = {3: 0.1053, 7: 0.0613, 10: 0.0833, 6: 0.0441, 14: 0.0488, 4: 0.0157}


def _interp_value(pts: float) -> float:
    """Win rate for `pts` of line value, interpolated off the backtest curve."""
    xs = sorted(VALUE_CURVE)
    ys = [VALUE_CURVE[x] for x in xs]
    p = abs(pts)
    rate = float(np.interp(p, xs, ys))
    if pts < 0:  # holding a WORSE number than market
        rate = 1.0 - rate
    return rate


def line_value_edge(my_line: float, mkt_line: float, side: str) -> float:
    """
    Points of value the chosen side holds versus the market.

    Lines are home-perspective. Taking the home side, a more positive
    my_line is better (you lay less / get more). Taking the away side,
    the reverse.
    """
    diff = my_line - mkt_line
    return diff if side == "home" else -diff


def score_game(g: dict) -> dict:
    my, mkt = float(g["my_line"]), float(g["mkt_line"])

    # value available on each side; pick the better one
    v_home = line_value_edge(my, mkt, "home")
    v_away = line_value_edge(my, mkt, "away")
    side = "home" if v_home >= v_away else "away"
    raw_pts = max(v_home, v_away)

    mult = band_multiplier(mkt)
    eff_pts = raw_pts * mult
    base = _interp_value(eff_pts)

    # optional projection tilt, scaled by that source's credibility.
    # capped hard: projections are NOT validated by this backtest.
    proj_adj = 0.0
    pm, pw = g.get("proj_margin"), g.get("proj_weight", 0.0)
    if pm is not None and pw:
        # projected cover margin for the chosen side vs MY number
        pcov = (pm + my) if side == "home" else (-pm - my)
        proj_adj = float(np.clip(pcov * 0.004 * pw, -0.02, 0.02))

    win_rate = float(np.clip(base + proj_adj, 0.30, 0.70))
    team = g["home"] if side == "home" else g["away"]
    num = my if side == "home" else -my

    ev = win_rate * (100 / 110) - (1 - win_rate)

    if raw_pts >= 1.0 and win_rate > BREAKEVEN:
        verdict = "PLAY"
    elif win_rate > BREAKEVEN:
        verdict = "lean"
    else:
        verdict = "PASS"

    return dict(
        game=f"{g['away']} @ {g['home']}",
        pick=f"{team} {num:+g}",
        side=side,
        my_line=my,
        mkt_line=mkt,
        pts_value=round(raw_pts, 2),
        band_mult=mult,
        eff_pts=round(eff_pts, 2),
        win_rate=round(win_rate, 4),
        ev_per_unit=round(ev, 4),
        verdict=verdict,
    )


def score_week(games: list) -> list:
    out = [score_game(g) for g in games]
    return sorted(out, key=lambda r: -r["win_rate"])


def fmt(rows: list) -> str:
    hdr = f"{'GAME':<14}{'PICK':<14}{'VAL':>6}{'EFF':>6}{'WIN%':>8}{'EV':>8}  VERDICT"
    lines = [hdr, "-" * len(hdr)]
    for r in rows:
        lines.append(
            f"{r['game']:<14}{r['pick']:<14}{r['pts_value']:>6.1f}"
            f"{r['eff_pts']:>6.1f}{r['win_rate']*100:>7.1f}%"
            f"{r['ev_per_unit']:>+8.3f}  {r['verdict']}"
        )
    return "\n".join(lines)
