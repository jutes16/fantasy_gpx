"""
Which selection rule should pick the 5? Tested on 2025's real pool sheet.

Compares the model's band-weighted ranking against simpler alternatives,
so the extra machinery has to earn its place.
"""

import numpy as np
import pandas as pd
from scipy import stats
from pool import band_multiplier, _interp

PICKS = 5
m = pd.read_csv("data/pool_2025_merged.csv", low_memory=False)
m = m[~m.push].copy()
m["won"] = m.value_side_won.astype(float)


def wilson(w, n):
    if n == 0:
        return (np.nan, np.nan, np.nan)
    z = 1.959963985
    p = w / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return (p, c - h, c + h)


m["band_mult"] = m.mkt_line.abs().map(band_multiplier)
m["eff_pts"] = m.abs_value * m.band_mult
m["model_rate"] = m.eff_pts.map(_interp)
m["near3"] = (m.mkt_line.abs().between(2.5, 3.5)).astype(int)

RULES = {
    "model (band-weighted)": "model_rate",
    "raw line value only":   "abs_value",
    "value, tiebreak near 3": None,   # handled below
}

print("=" * 70)
print("SELECTION RULES ON THE 2025 POOL SHEET")
print(f"{PICKS} picks/week, {m.wk.nunique()} weeks")
print("=" * 70)

rows = []
for name, col in RULES.items():
    wins = picks = 0
    for _, g in m.groupby("wk"):
        if col is None:
            g = g.sort_values(["abs_value", "near3"], ascending=[False, False])
        else:
            g = g.sort_values(col, ascending=False)
        card = g.head(PICKS)
        wins += int(card.won.sum())
        picks += len(card)
    r, lo, hi = wilson(wins, picks)
    rows.append(dict(rule=name, wins=wins, picks=picks, rate=r, ci_lo=lo, ci_hi=hi,
                     vs_base=wins - picks * 0.5))

# reference: take every game with >= 1pt, unlimited
big = m[m.abs_value >= 1.0]
r, lo, hi = wilson(int(big.won.sum()), len(big))
rows.append(dict(rule="ALL >=1pt (unlimited)", wins=int(big.won.sum()), picks=len(big),
                 rate=r, ci_lo=lo, ci_hi=hi, vs_base=big.won.sum() - len(big) * 0.5))

print(pd.DataFrame(rows).to_string(index=False, float_format=lambda x: f"{x:.4f}"))

print("\n" + "=" * 70)
print("WIN RATE BY SPREAD BAND (does the band multiplier hold up in 2025?)")
print("=" * 70)
bands = [(0, 2.5), (2.5, 3.5), (3.5, 6.5), (6.5, 7.5), (7.5, 10.5), (10.5, 99)]
out = []
for lo_b, hi_b in bands:
    sub = m[(m.mkt_line.abs() >= lo_b) & (m.mkt_line.abs() < hi_b) & (m.abs_value >= 0.5)]
    if len(sub) < 8:
        continue
    w = int(sub.won.sum())
    r, cl, ch = wilson(w, len(sub))
    out.append(dict(band=f"{lo_b}-{hi_b}", n=len(sub), wins=w, rate=r,
                    ci_lo=cl, ci_hi=ch, mult_used=band_multiplier(lo_b + 0.01)))
print(pd.DataFrame(out).to_string(index=False, float_format=lambda x: f"{x:.4f}"))

print("\n" + "=" * 70)
print("SENSITIVITY: what if part of the gap only appeared after you submitted?")
print("=" * 70)
print("The backtest measures your sheet against the CLOSING line, which you")
print("cannot see when picks are due. If only a fraction of each gap existed")
print("at submission time, the usable edge shrinks:\n")
for frac in (1.0, 0.75, 0.5, 0.25):
    sub = m.copy()
    sub["eff"] = sub.abs_value * frac * sub.band_mult
    wins = picks = 0
    for _, g in sub.groupby("wk"):
        card = g.sort_values("eff", ascending=False).head(PICKS)
        wins += int(card.won.sum())
        picks += len(card)
    exp = 0.0
    for _, g in sub.groupby("wk"):
        exp += sum(_interp(v) for v in g.nlargest(PICKS, "eff").eff)
    print(f"  {frac*100:>3.0f}% of gap visible -> ranking picks {wins}/{picks} "
          f"= {wins/picks*100:.1f}%   expected {exp:.1f} wins")
print("\n  (the ranking barely changes; what changes is how much of the")
print("   measured edge you could actually have acted on)")
