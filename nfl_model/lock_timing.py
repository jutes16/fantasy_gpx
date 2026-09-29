"""
What does the pool's lock rule cost?

Rule: picks lock at the first game you select. Take a Thursday game and
ALL FIVE lock Thursday night. Otherwise everything locks Sunday 1pm ET.

That makes Thursday games expensive in a way that has nothing to do with
the Thursday game itself: it degrades the market line you see for the
other four picks by roughly three days.

Method: take 2025's real pool sheet, corrupt the observed market line
with noise of size sigma (standing in for how much the line still had
left to move when you locked), re-rank, take the top 5, and grade against
what actually happened. Averaged over many draws.

sigma = 0     -> you saw the closing line (Sunday 1pm, 1pm games)
sigma ~ 0.3   -> a few hours of drift (Sunday 1pm lock, late games)
sigma ~ 0.7   -> three days of drift (Thursday lock)
"""

import numpy as np
import pandas as pd
from pool import band_multiplier, _interp

PICKS = 5
N_DRAWS = 3000
rng = np.random.default_rng(11)

m = pd.read_csv("data/pool_2025_merged.csv", low_memory=False)
m = m[~m.push].copy()
m["won"] = m.value_side_won.astype(float)
m = m.dropna(subset=["won"])

weeks = [g for _, g in m.groupby("wk")]

# Precompute per-game truth: which side wins, and the true market line.
print("=" * 70)
print("COST OF LOCKING EARLY")
print(f"2025 pool sheet, {len(m)} gradeable games, {len(weeks)} weeks, "
      f"{N_DRAWS} draws")
print("=" * 70)
print(f"\n{'sigma':<8}{'meaning':<34}{'wins/90':>9}{'win rate':>10}")
print("-" * 61)

SCENARIOS = [
    (0.0, "saw the close exactly"),
    (0.25, "Sunday 1pm lock, 1pm games"),
    (0.5, "Sunday 1pm lock, late games"),
    (0.75, "Thursday lock (~3 days drift)"),
    (1.0, "Thursday lock, volatile week"),
    (1.5, "heavy movement"),
]

results = {}
for sigma, label in SCENARIOS:
    totals = np.zeros(N_DRAWS)
    for g in weeks:
        true_mkt = g.mkt_line.values
        pool_line = g.pool_line.values
        won = g.won.values
        n = len(g)
        # observed market line when you locked
        noise = rng.normal(0, sigma, size=(N_DRAWS, n)) if sigma > 0 else np.zeros((N_DRAWS, n))
        obs_mkt = true_mkt[None, :] + noise
        # Many games tie on exactly 0.5 or 1.0 of value. Without a random
        # tiebreak, argsort resolves them by row order, which is arbitrary
        # and biases the sigma=0 row. Jitter far below any real difference.
        tiebreak = rng.normal(0, 1e-6, size=(N_DRAWS, n))
        # value you THINK you have, on the side you'd choose
        obs_val = np.abs(obs_mkt - pool_line[None, :])
        side_home = (obs_mkt - pool_line[None, :]) > 0
        # true side that had value
        true_side_home = (true_mkt - pool_line) > 0
        # you win if the side you picked is the one that covered
        picked_right_side = side_home == true_side_home[None, :]
        outcome = np.where(picked_right_side, won[None, :], 1 - won[None, :])

        mult = np.array([band_multiplier(x) for x in np.abs(true_mkt)])
        score = obs_val * mult[None, :] + tiebreak
        k = min(PICKS, n)
        idx = np.argsort(-score, axis=1)[:, :k]
        totals += np.take_along_axis(outcome, idx, axis=1).sum(axis=1)

    tot_picks = sum(min(PICKS, len(g)) for g in weeks)
    mean_w = totals.mean()
    results[sigma] = mean_w
    print(f"{sigma:<8.2f}{label:<34}{mean_w:>9.1f}{mean_w/tot_picks*100:>9.1f}%")

print("-" * 61)
base = results[0.0]
thu = results[0.75]
print(f"\ncost of a Thursday lock vs seeing the close: "
      f"{thu - base:+.1f} wins over a season")
print(f"per week: {(thu - base)/len(weeks):+.2f} wins")

print("\n" + "=" * 70)
print("THE THURSDAY DECISION RULE")
print("=" * 70)
print(f"Taking a Thursday game costs roughly {abs(thu-base)/len(weeks):.2f} wins per week")
print("on the OTHER four picks, by locking them ~3 days early.")
print()
print("So a Thursday game has to be better than your 5th-best Sunday")
print("option by at least that much to be worth taking. In win-rate terms")
print(f"it needs roughly {abs(thu-base)/len(weeks)*100:.0f} percentage points of edge over the")
print("game it displaces, on top of simply being a positive-value pick.")
