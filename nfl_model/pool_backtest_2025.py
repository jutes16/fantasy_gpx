"""
REAL out-of-sample test: the 2025 pool sheet vs the market closing line.

This is the test that matters. Everything before it was either synthetic
or measured on generic line value. Here we have the actual numbers the
pool published, so we can ask directly:

  1. How stale does the pool sheet actually get?
  2. Does taking the side with line value beat 50% in THIS pool?
  3. What would the 5-pick model have scored over a full season?

Conventions:
  pool home_spread : NEGATIVE = home favored   (opposite of nflverse)
  nflverse spread_line : POSITIVE = home favored
  so pool_line (nflverse terms) = -home_spread

  home covers when result > line
  home_value = market_line - pool_line   (lower line = easier for home)
  away_value = -home_value
"""

import re
import numpy as np
import pandas as pd
from scipy import stats

POOL_CSV = "/root/.claude/uploads/d5b25099-7409-524c-a6bb-1f660b8540ed/de624b71-google_sheet_pull_GAMES_20260125_182400.csv"
PICKS_PER_WEEK = 5

TEAM_FIX = {"LAR": "LA"}          # pool spells the Rams LAR, nflverse LA
WEEK_FIX = {"WC_PO": 19, "DV_PO": 20, "Champ_PO": 21, "SB_PO": 22}


def wilson(w, n, conf=0.95):
    if n == 0:
        return (np.nan, np.nan, np.nan)
    z = stats.norm.ppf(1 - (1 - conf) / 2)
    p = w / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return (p, c - h, c + h)


def parse_week(w):
    w = str(w).replace("Week_", "")
    if w in WEEK_FIX:
        return WEEK_FIX[w]
    m = re.match(r"^(\d+)$", w)
    return int(m.group(1)) if m else np.nan


# ---------- load ----------
pool = pd.read_csv(POOL_CSV)
pool["wk"] = pool.week.map(parse_week)
pool["away"] = pool.away.replace(TEAM_FIX)
pool["home"] = pool.home.replace(TEAM_FIX)
pool["spread_winner"] = pool.spread_winner.replace(TEAM_FIX)   # LAR -> LA
pool["pool_line"] = -pool.home_spread          # into nflverse convention
pool = pool.dropna(subset=["wk", "score_home", "score_away"])
pool["wk"] = pool.wk.astype(int)
pool = pool.drop(columns=["week"])

nfl = pd.read_csv("data/games_clean.csv", low_memory=False)
nfl = nfl[nfl.season == 2025][
    ["season", "week", "away_team", "home_team", "result", "spread_line", "abs_spread"]
].rename(columns={"away_team": "away", "home_team": "home",
                  "spread_line": "mkt_line", "week": "wk"})

m = pool.merge(nfl, on=["season", "wk", "away", "home"], how="inner")

print("=" * 74)
print("2025 POOL SHEET vs MARKET CLOSE")
print("=" * 74)
print(f"pool rows with scores: {len(pool)}   matched to market close: {len(m)}")

# sanity: our grading must reproduce the sheet's own spread_winner
m["pool_margin"] = m.score_home - m.score_away
m["home_cov"] = m.pool_margin - m.pool_line
m["graded_winner"] = np.where(m.home_cov > 0, m.home, np.where(m.home_cov < 0, m.away, "PUSH"))
agree = (m.graded_winner == m.spread_winner).mean()
print(f"grading agrees with sheet's spread_winner: {agree*100:.1f}%")
if agree < 0.99:
    bad = m[m.graded_winner != m.spread_winner]
    print(f"  !! {len(bad)} mismatches, inspect before trusting results")
    print(bad[["wk", "away", "home", "pool_line", "pool_margin",
               "graded_winner", "spread_winner"]].head(8).to_string(index=False))

# ---------- how stale is the sheet? ----------
m["home_value"] = m.mkt_line - m.pool_line
m["abs_value"] = m.home_value.abs()

print("\n" + "=" * 74)
print("HOW STALE IS THE POOL SHEET?")
print("=" * 74)
print(f"mean |gap| vs close: {m.abs_value.mean():.2f} pts")
print(f"median |gap|:        {m.abs_value.median():.2f} pts")
dist = m.abs_value.value_counts().sort_index()
print("\ngap size distribution:")
for v, c in dist.items():
    if c >= 3:
        print(f"  {v:>4.1f} pts : {c:>3} games ({c/len(m)*100:4.1f}%)")
print(f"\ngames with >= 1.0 pt of value: {(m.abs_value >= 1).sum()} "
      f"({(m.abs_value >= 1).mean()*100:.1f}%)")
print(f"games with >= 0.5 pt of value: {(m.abs_value >= 0.5).sum()} "
      f"({(m.abs_value >= 0.5).mean()*100:.1f}%)")
print(f"per week, >= 1.0 pt available: {(m.abs_value >= 1).sum()/m.wk.nunique():.1f} games")

# ---------- does the value side win? ----------
m["value_side"] = np.where(m.home_value > 0, "home",
                    np.where(m.home_value < 0, "away", "none"))
m["value_side_won"] = np.where(
    m.value_side == "home", m.home_cov > 0,
    np.where(m.value_side == "away", m.home_cov < 0, np.nan))
m["push"] = m.home_cov == 0

print("\n" + "=" * 74)
print("DOES THE LINE-VALUE SIDE ACTUALLY WIN?  (2025, out of sample)")
print("=" * 74)

buckets = [(0.5, 0.5), (1.0, 1.0), (1.5, 1.5), (2.0, 99)]
rows = []
for lo, hi in buckets:
    sub = m[(m.abs_value >= lo) & (m.abs_value <= hi) & (~m.push)]
    if len(sub) < 5:
        continue
    w = int(sub.value_side_won.sum())
    n = len(sub)
    r, cl, ch = wilson(w, n)
    label = f"{lo}" if lo == hi else f"{lo}+"
    rows.append(dict(value=label, games=n, wins=w, losses=n - w,
                     win_rate=r, ci_lo=cl, ci_hi=ch))

allv = m[(m.abs_value > 0) & (~m.push)]
w = int(allv.value_side_won.sum())
r, cl, ch = wilson(w, len(allv))
rows.append(dict(value="ALL >0", games=len(allv), wins=w, losses=len(allv) - w,
                 win_rate=r, ci_lo=cl, ci_hi=ch))
res = pd.DataFrame(rows)
print(res.to_string(index=False, float_format=lambda x: f"{x:.4f}"))

print("\nbacktest (2015-2025 generic) predicted: 0.5 pt -> 52.5%, 1.0 pt -> 54.5%")

# ---------- what would the 5-pick model have scored? ----------
print("\n" + "=" * 74)
print("THE 5-PICK MODEL OVER 2025")
print("=" * 74)

import sys
sys.path.insert(0, ".")
from pool import band_multiplier, _interp

m["band_mult"] = m.mkt_line.abs().map(band_multiplier)
m["eff_pts"] = m.abs_value * m.band_mult
m["model_rate"] = m.eff_pts.map(_interp)

weekly = []
for wk, g in m.groupby("wk"):
    g = g.sort_values("model_rate", ascending=False)
    card = g.head(PICKS_PER_WEEK)
    wins = int(card.value_side_won.sum())
    pushes = int(card.push.sum())
    weekly.append(dict(week=wk, games=len(g), picks=len(card), wins=wins,
                       pushes=pushes, exp=card.model_rate.sum(),
                       edges=int((card.abs_value >= 1).sum())))
wk = pd.DataFrame(weekly)

tot_w = int(wk.wins.sum())
tot_p = int(wk.picks.sum())
base = tot_p * 0.5
r, cl, ch = wilson(tot_w, tot_p)

print(wk.to_string(index=False, float_format=lambda x: f"{x:.2f}"))
print(f"\nseason: {tot_w}/{tot_p} = {r*100:.1f}%   95% CI [{cl*100:.1f}%, {ch*100:.1f}%]")
print(f"coin-flip baseline: {base:.0f} wins   actual: {tot_w}   ({tot_w-base:+.0f})")
print(f"model expected wins: {wk.exp.sum():.1f}")
print(f"mean edges (>=1pt) per card: {wk.edges.mean():.1f} of {PICKS_PER_WEEK}")
if cl > 0.5:
    print("  -> CI clears 50%. Beat a coin flip in 2025.")
elif ch < 0.5:
    print("  -> CI below 50%. Lost to a coin flip in 2025.")
else:
    print("  -> CI straddles 50%. One season cannot separate this from luck.")

# ---------- comparison: picking at random from the same weeks ----------
rng = np.random.default_rng(7)
N_SIM = 50_000
# A pool rival handicapping games lands near 50% per pick, so their
# season is Binomial(tot_p, 0.5). That is the honest comparison.
sims = rng.binomial(tot_p, 0.5, N_SIM)
print(f"\nrival picking at 50%: median {np.median(sims):.0f} wins, "
      f"5-95pct {np.percentile(sims,5):.0f}-{np.percentile(sims,95):.0f}")
print(f"model finished at {tot_w} -> beats such a rival "
      f"{(sims < tot_w).mean()*100:.1f}% of seasons")

# Was 2025 in line with what the generic backtest predicted, or lucky?
exp_w = wk.exp.sum()
se = np.sqrt(0.25 / tot_p)
z = (tot_w / tot_p - exp_w / tot_p) / se
print(f"\nmodel predicted {exp_w:.1f} wins, actual {tot_w} "
      f"({z:+.2f} standard errors)")
if abs(z) < 2:
    print("  -> inside 2 SE. 2025 is consistent with the model, on the lucky side.")
else:
    print("  -> outside 2 SE. The season does not match the model's own forecast.")

m.to_csv("data/pool_2025_merged.csv", index=False)
wk.to_csv("data/pool_2025_weekly.csv", index=False)
print("\nwrote data/pool_2025_merged.csv, data/pool_2025_weekly.csv")
