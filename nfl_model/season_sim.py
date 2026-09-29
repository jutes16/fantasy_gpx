"""
How much does the line-value edge actually buy over a pool season?

Simulates 18 weeks of 5 picks using the backtested win rates, against
a field of opponents picking at various skill levels. Reports the
distribution, not a point estimate, because one season is mostly noise.

Value availability is modelled on the observed Week 3 2026 sheet:
14 games, of which 3 held 1.0 pt, 3 held 0.5 pt, 1 held -0.5, 7 held 0.
That is one observation, so the sim also runs optimistic and pessimistic
variants to bracket it.
"""

import numpy as np

rng = np.random.default_rng(20260928)

WEEKS = 18
PICKS = 5
SIMS = 40_000

# win rates by points of value held, from backtest.py
RATE = {0.0: 0.5000, 0.5: 0.5248, 1.0: 0.5451, 1.5: 0.5629, 2.0: 0.5793}


def weekly_card(profile):
    """
    profile: list of (pts_value, count) describing what the sheet offers.
    Returns the win rates of the best PICKS slots.
    """
    pool = []
    for pts, cnt in profile:
        pool += [RATE[pts]] * cnt
    pool.sort(reverse=True)
    card = pool[:PICKS]
    while len(card) < PICKS:      # thin week: fill with coin flips
        card.append(0.50)
    return np.array(card)


PROFILES = {
    "observed (wk3-like)": [(1.0, 3), (0.5, 3), (0.0, 8)],
    "pessimistic": [(1.0, 1), (0.5, 2), (0.0, 11)],
    "optimistic": [(1.5, 1), (1.0, 3), (0.5, 4), (0.0, 6)],
}


def simulate(profile):
    rates = weekly_card(profile)
    draws = rng.random((SIMS, WEEKS, PICKS)) < rates
    return draws.sum(axis=(1, 2))


print("=" * 72)
print(f"POOL SEASON SIM   {WEEKS} weeks x {PICKS} picks = {WEEKS*PICKS} picks")
print(f"{SIMS:,} simulated seasons   |   coin-flip baseline = {WEEKS*PICKS*0.5:.0f} wins")
print("=" * 72)

base = WEEKS * PICKS * 0.5

for name, prof in PROFILES.items():
    rates = weekly_card(prof)
    tot = simulate(prof)
    exp = rates.sum() * WEEKS
    print(f"\n{name}")
    print(f"  card win rates: {', '.join(f'{r:.3f}' for r in rates)}")
    print(f"  expected wins/week: {rates.sum():.2f}  (baseline 2.50)")
    print(f"  expected season:    {exp:.1f} wins   ({exp-base:+.1f} vs baseline)")
    print(f"  median:             {np.median(tot):.0f}")
    print(f"  5th-95th pct:       {np.percentile(tot,5):.0f} - {np.percentile(tot,95):.0f}")
    print(f"  P(beat baseline):   {(tot > base).mean()*100:.1f}%")

# --- head to head against opponents of varying quality ---
print("\n" + "=" * 72)
print("HEAD TO HEAD: chance you finish above a given opponent over a season")
print("=" * 72)
me = simulate(PROFILES["observed (wk3-like)"])
for opp_rate in (0.500, 0.510, 0.520, 0.530, 0.545):
    opp = (rng.random((SIMS, WEEKS * PICKS)) < opp_rate).sum(axis=1)
    win = (me > opp).mean()
    tie = (me == opp).mean()
    print(
        f"  opponent at {opp_rate*100:.1f}%  ->  you finish ahead "
        f"{win*100:.1f}%  (tie {tie*100:.1f}%)"
    )

print("\n" + "=" * 72)
print("HOW MANY SEASONS TO PROVE THE EDGE IS REAL")
print("=" * 72)
p = weekly_card(PROFILES["observed (wk3-like)"]).mean()
print(f"  model win rate: {p:.4f}   vs coin flip 0.5000")
for seasons in (1, 3, 5, 10, 20):
    n = seasons * WEEKS * PICKS
    se = np.sqrt(0.25 / n)
    z = (p - 0.5) / se
    from scipy import stats as st
    power = 1 - st.norm.cdf(1.96 - z)
    print(f"  {seasons:>2} season(s) = {n:>4} picks   power to detect: {power*100:4.1f}%")
