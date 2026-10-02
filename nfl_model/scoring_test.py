"""
Should picks be scored by the exact cover probability of each line (margins.py,
key numbers included) instead of the band multiplier in pool.py?

    python3 scoring_test.py

TEST 1 -- 2025's real pool sheet, 5 picks a week (same harness as
          rule_compare.py), ranked by each rule. The distribution is built
          WITHOUT 2025 (out of sample).
TEST 2 -- calibration, 2015-2025: every game, both sides, given 0.5 to 2 points
          of value vs the close. How well does each method predict the actual
          cover rate? Each season is predicted from the other seasons only.
          (The band multiplier was fitted on these same seasons, so this test
          favours it.)
"""

import numpy as np
import pandas as pd

from margins import cover_prob, home_margin_dist
from pool import _interp, band_multiplier

PICKS = 5


def wilson(w, n, z=1.959963985):
    p = w / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return p, c - h, c + h


def crosses_key(close, extra, side):
    """Does moving from the close to the line cross 3 or 7 for this side?"""
    # the side's own number: home gets -close (as a spread), away gets +close
    own = close if side == "home" else -close          # side's line, + = getting points
    a, b = sorted((own, own + extra))
    return any(a < k < b or a < -k < b for k in (3, 7)) or any(
        abs(x) in (3, 7) for x in (own, own + extra))


# ------------------------------------------------------------------ test 1
def test_2025_sheet():
    m = pd.read_csv("data/pool_2025_merged.csv", low_memory=False)
    m = m[~m.push].copy()
    m["won"] = m.value_side_won.astype(float)
    # pool_2025_merged is nflverse-signed (+ = home favoured); flip to ours
    m["pool_ours"], m["close_ours"] = -m.pool_line, -m.mkt_line
    m["band_rate"] = (m.abs_value * m.mkt_line.abs().map(band_multiplier)).map(_interp)
    m["dist_rate"] = [cover_prob(p, s, c, exclude_season=2025)
                      for p, s, c in zip(m.pool_ours, m.value_side, m.close_ours)]
    m["near3"] = m.mkt_line.abs().between(2.5, 3.5).astype(int)

    rules = {
        "model (band-weighted)  [current]": lambda g: g.sort_values("band_rate", ascending=False),
        "cover probability       [new]": lambda g: g.sort_values("dist_rate", ascending=False),
        "raw line value only": lambda g: g.sort_values("abs_value", ascending=False),
        "value, tiebreak near 3": lambda g: g.sort_values(["abs_value", "near3"], ascending=False),
    }
    print("=" * 74)
    print("TEST 1  2025 POOL SHEET: 5 picks a week, 18 weeks (pushes excluded)")
    print("=" * 74)
    print(f"{'rule':<34}{'record':>9}{'win %':>8}{'95% CI':>16}{'predicted':>11}")
    cards = {}
    for name, rank in rules.items():
        card = pd.concat(rank(g).head(PICKS) for _, g in m.groupby("wk"))
        cards[name] = card
        w, n = int(card.won.sum()), len(card)
        r, lo, hi = wilson(w, n)
        pred = card.dist_rate.sum()
        print(f"{name:<34}{f'{w}-{n - w}':>9}{r * 100:>7.1f}%"
              f"{f'[{lo * 100:.0f}, {hi * 100:.0f}]':>16}{pred:>10.1f}w")
    a = set(cards["model (band-weighted)  [current]"].index)
    b = set(cards["cover probability       [new]"].index)
    print(f"\n  picks that differ between current and new: {len(a - b)} of {len(a)}")
    swap_out = m.loc[sorted(a - b)]
    swap_in = m.loc[sorted(b - a)]
    print(f"  dropped by the new rule: {int(swap_out.won.sum())}-{len(swap_out) - int(swap_out.won.sum())}"
          f"   added: {int(swap_in.won.sum())}-{len(swap_in) - int(swap_in.won.sum())}")
    print("  ('predicted' = expected wins under the new distribution, same for every row)")


# ------------------------------------------------------------------ test 2
def test_calibration():
    g = pd.read_csv("data/games_clean.csv", low_memory=False)
    g = g[(g.game_type == "REG") & g.result.notna() & g.spread_line.notna()]
    rows = []
    for r in g.itertuples():
        close = -r.spread_line                       # ours: - = home favoured
        for side in ("home", "away"):
            for v in (0.5, 1.0, 1.5, 2.0):
                line = close + v if side == "home" else close - v
                cover = r.result + line              # > 0 home covers
                score = 0.5 if cover == 0 else float((cover > 0) == (side == "home"))
                rows.append(dict(
                    season=r.season, v=v, actual=score,
                    band=_interp(v * band_multiplier(close)),
                    dist=cover_prob(line, side, close, exclude_season=r.season),
                    key=crosses_key(close, v, side)))
    d = pd.DataFrame(rows)
    print("\n" + "=" * 74)
    print(f"TEST 2  CALIBRATION 2015-2025 ({len(d):,} game-sides x value sizes;"
          " leave-one-season-out)")
    print("=" * 74)
    brier = lambda c: ((d[c] - d.actual) ** 2).mean()
    print(f"  Brier score (lower = better): band {brier('band'):.5f}   distribution {brier('dist'):.5f}")
    print(f"\n  {'value':<6}{'key?':<6}{'n':>7}{'actual':>9}{'band':>8}{'dist':>8}")
    for (v, k), s in d.groupby(["v", "key"]):
        print(f"  {v:<6}{'yes' if k else 'no':<6}{len(s):>7}{s.actual.mean() * 100:>8.1f}%"
              f"{s.band.mean() * 100:>7.1f}%{s.dist.mean() * 100:>7.1f}%")
    # where the two disagree most: does the actual rate side with either?
    d["gap"] = d.dist - d.band
    q = pd.qcut(d.gap, 5, labels=["dist << band", "dist < band", "~equal", "dist > band", "dist >> band"])
    print(f"\n  {'where they disagree':<16}{'n':>8}{'actual':>9}{'band':>8}{'dist':>8}")
    for lbl, s in d.groupby(q, observed=True):
        print(f"  {lbl:<16}{len(s):>8}{s.actual.mean() * 100:>8.1f}%"
              f"{s.band.mean() * 100:>7.1f}%{s.dist.mean() * 100:>7.1f}%")


if __name__ == "__main__":
    test_2025_sheet()
    test_calibration()
