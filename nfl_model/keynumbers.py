"""
Supplementary test: is a HALF point worth more when it crosses a key number?

This is the specific claim leaned on for NE +3.5 (market +3) and
LAC +7.5 (market +7) in Week 3 2026. Test it rather than assume it.

Method: for every game, compare holding the closing number against
holding closing + 0.5, split by whether that half point crosses 3 or 7.
"""

import numpy as np
import pandas as pd
from scipy import stats

BREAKEVEN = 0.5238


def wilson(w, n, conf=0.95):
    if n == 0:
        return (np.nan, np.nan, np.nan)
    z = stats.norm.ppf(1 - (1 - conf) / 2)
    p = w / n
    den = 1 + z**2 / n
    ctr = (p + z**2 / (2 * n)) / den
    half = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return (p, ctr - half, ctr + half)


def units(w, l):
    return w * (100 / 110) - l


def grade(m, shift):
    a = m + shift
    return int((a > 0).sum()), int((a < 0).sum()), int((a == 0).sum())


d = pd.read_csv("data/games_clean.csv", low_memory=False)

# Build one row per BET SIDE: the number that side is getting, and its
# margin against that number.
rows = []
for _, g in d.iterrows():
    cm = g["cover_margin"]
    sl = g["spread_line"]
    # home side gets -sl (if sl>0 home lays sl); the dog side gets +|sl|
    rows.append(dict(side_number=-sl, margin=cm))
    rows.append(dict(side_number=sl, margin=-cm))
b = pd.DataFrame(rows)

print("=" * 74)
print("HALF-POINT VALUE: DOES CROSSING A KEY NUMBER MATTER?")
print(f"bet-sides: {len(b)}   breakeven: {BREAKEVEN:.4f}")
print("=" * 74)

# A +0.5 improvement moves the side's number from X to X+0.5.
# It "crosses" key number K if the side is a dog at exactly +K
# (going +3 -> +3.5) or a favorite at -(K+0.5) (going -3.5 -> -3).
out = []
for K in (3, 7):
    # dog sitting exactly on +K, buying the half to +K.5
    dog = b[np.isclose(b.side_number, K)]
    # favorite sitting on -(K+0.5), buying the half to -K
    fav = b[np.isclose(b.side_number, -(K + 0.5))]
    for label, sub in (
        (f"dog +{K} -> +{K}.5", dog),
        (f"fav -{K}.5 -> -{K}", fav),
    ):
        if len(sub) < 25:
            continue
        m = sub.margin.values
        w0, l0, p0 = grade(m, 0.0)
        w1, l1, p1 = grade(m, 0.5)
        r0, _, _ = wilson(w0, w0 + l0)
        r1, lo1, hi1 = wilson(w1, w1 + l1)
        out.append(
            dict(
                move=label,
                bets=len(sub),
                rate_at_close=r0,
                rate_with_half=r1,
                gain_pp=(r1 - r0) * 100,
                ci_lo=lo1,
                ci_hi=hi1,
                units=units(w1, l1),
            )
        )

# Control: the same half point bought where it crosses nothing.
ctrl = b[
    (~b.side_number.abs().isin([3, 3.5, 7, 7.5]))
    & (b.side_number.abs() >= 1)
    & (b.side_number.abs() <= 14)
]
m = ctrl.margin.values
w0, l0, _ = grade(m, 0.0)
w1, l1, _ = grade(m, 0.5)
r0, _, _ = wilson(w0, w0 + l0)
r1, lo1, hi1 = wilson(w1, w1 + l1)
out.append(
    dict(
        move="CONTROL: crosses nothing",
        bets=len(ctrl),
        rate_at_close=r0,
        rate_with_half=r1,
        gain_pp=(r1 - r0) * 100,
        ci_lo=lo1,
        ci_hi=hi1,
        units=units(w1, l1),
    )
)

res = pd.DataFrame(out)
print(res.to_string(index=False, float_format=lambda x: f"{x:.4f}"))

print("\n" + "=" * 74)
print("PUSH RATE BY NUMBER (how often a whole number lands exactly)")
print("=" * 74)
pr = []
for n in range(1, 15):
    sub = b[np.isclose(b.side_number.abs(), n)]
    if len(sub) < 25:
        continue
    _, _, p = grade(sub.margin.values, 0.0)
    pr.append(dict(number=n, bet_sides=len(sub), pushes=p, push_pct=p / len(sub) * 100))
print(
    pd.DataFrame(pr).to_string(index=False, float_format=lambda x: f"{x:.2f}")
)

res.to_csv("data/out_halfpoint.csv", index=False)
print("\nwrote data/out_halfpoint.csv")
