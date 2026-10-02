"""
Do weather, referees or rest/travel beat the closing line?

    python3 situational_factors.py

A short list of hypotheses fixed BEFORE looking, each with a reason it could
matter, graded against the closing spread or total for every regular-season
game since 2015 (nflverse). Media "trends" are found by slicing the data until
something looks good; with enough slices something always will. So: few
tests, a mechanism behind each, and a multiple-testing allowance.

Spread bets need 52.4% at -110 (50% in a no-vig pool); totals the same.
"""

import numpy as np
import pandas as pd

NFLVERSE = "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"
PACIFIC = {"SEA", "SF", "LA", "LAR", "LAC", "LV", "OAK", "ARI"}   # ARI: no DST, plays on Pacific time in-season
EASTERN = {"NE", "NYJ", "NYG", "BUF", "MIA", "PHI", "PIT", "BAL", "WAS", "CLE", "CIN",
           "DET", "ATL", "CAR", "TB", "JAX", "IND"}
N_TESTS = 8                     # for the multiple-testing (Bonferroni) allowance


def wilson(w, n, z=1.959963985):
    p = w / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return p, c - h, c + h


def rate(hit, n_miss):
    """hit/miss counts -> (n, rate, lo, hi, p-value vs 50%)."""
    from scipy import stats
    w, l = int(hit), int(n_miss)
    n = w + l
    if n == 0:
        return n, np.nan, np.nan, np.nan, np.nan
    r, lo, hi = wilson(w, n)
    p = stats.binomtest(w, n, 0.5).pvalue
    return n, r, lo, hi, p


def line(name, n, r, lo, hi, p, unit):
    flag = "  <-- survives multiple-testing" if p < 0.05 / N_TESTS else ("  (nominal p<.05)" if p < 0.05 else "")
    print(f"  {name:<46}{n:>6}  {r*100:5.1f}% {unit:<6}[{lo*100:.0f}, {hi*100:.0f}]  p={p:.3f}{flag}")


def main():
    g = pd.read_csv(NFLVERSE, low_memory=False)
    g = g[(g.game_type == "REG") & (g.season >= 2015) & g.result.notna() & g.spread_line.notna()].copy()
    g["home_cover"] = np.sign(g.result - g.spread_line)        # +1 home covers, 0 push, -1 away covers
    g["over"] = np.sign(g.total - g.total_line)                # +1 over, 0 push, -1 under
    out = g[g.roof.isin(["outdoors", "open"])]

    print("=" * 96)
    print(f"SITUATIONAL FACTORS vs THE CLOSING LINE   regular season 2015-{g.season.max()}, {len(g):,} games")
    print(f"8 hypotheses fixed in advance; 'survives' = p < {0.05/N_TESTS:.4f} (0.05 / {N_TESTS})")
    print("=" * 96)

    # ---- WEATHER (totals) ----
    print("\nWEATHER  (bet the UNDER; outdoor games with weather recorded)")
    w = out.dropna(subset=["wind"])
    print(f"  weather recorded for seasons {int(w.season.min())}-{int(w.season.max())} ({len(w):,} outdoor games)")
    for name, d in (("1. wind >= 15 mph", w[w.wind >= 15]),
                    ("   wind >= 20 mph (stronger dose)", w[w.wind >= 20]),
                    ("   wind < 10 mph (control)", w[w.wind < 10])):
        line(name, *rate((d.over < 0).sum(), (d.over > 0).sum()), "under")
    t = out.dropna(subset=["temp"])
    for name, d in (("2. temp <= 32F", t[t.temp <= 32]), ("   temp <= 20F (stronger dose)", t[t.temp <= 20])):
        line(name, *rate((d.over < 0).sum(), (d.over > 0).sum()), "under")
    # has the market caught up? wind effect early vs late
    for lbl, d in (("   wind >= 15, 2015-2019", w[(w.wind >= 15) & (w.season <= 2019)]),
                   ("   wind >= 15, 2020+", w[(w.wind >= 15) & (w.season >= 2020)])):
        line(lbl, *rate((d.over < 0).sum(), (d.over > 0).sum()), "under")

    # ---- REFEREES (out of sample) ----
    print("\nREFEREES  (tendency in 2015-2020 -> does it predict 2021+?)")
    train, test = g[g.season <= 2020], g[g.season >= 2021]
    ref_over = train.groupby("referee").over.agg(lambda s: (s > 0).sum() / max((s != 0).sum(), 1))
    ref_n = train.groupby("referee").size()
    ref_over = ref_over[ref_n >= 50]                         # refs with a real sample
    hi_refs = ref_over[ref_over >= ref_over.quantile(2 / 3)].index
    lo_refs = ref_over[ref_over <= ref_over.quantile(1 / 3)].index
    d = test[test.referee.isin(hi_refs)]
    line("3. 'over' refs (top third 2015-20), bet over", *rate((d.over > 0).sum(), (d.over < 0).sum()), "over")
    d = test[test.referee.isin(lo_refs)]
    line("   'under' refs (bottom third), bet under", *rate((d.over < 0).sum(), (d.over > 0).sum()), "under")
    ref_home = train.groupby("referee").home_cover.agg(lambda s: (s > 0).sum() / max((s != 0).sum(), 1))
    ref_home = ref_home[ref_n >= 50]
    hi_h = ref_home[ref_home >= ref_home.quantile(2 / 3)].index
    d = test[test.referee.isin(hi_h)]
    line("4. 'home-friendly' refs (top third), bet home", *rate((d.home_cover > 0).sum(), (d.home_cover < 0).sum()), "home")
    print(f"   ({len(hi_refs)} over-refs, {len(lo_refs)} under-refs, {len(hi_h)} home-refs judged on 2015-20,"
          f" tested on {len(test):,} games 2021+)")
    # how persistent are ref tendencies at all? correlation of ref over% train vs test
    tr = train.groupby("referee").over.agg(lambda s: (s > 0).sum() / max((s != 0).sum(), 1))
    te = test.groupby("referee").over.agg(lambda s: (s > 0).sum() / max((s != 0).sum(), 1))
    both = pd.concat([tr, te], axis=1, keys=["train", "test"]).dropna()
    both = both[both.index.isin(ref_n[ref_n >= 50].index)]
    print(f"   persistence: correlation of a ref's over% 2015-20 vs 2021+ = {both.train.corr(both.test):+.2f}"
          f" ({len(both)} refs)")

    # ---- REST / TRAVEL (spread) ----
    print("\nREST & TRAVEL  (spread)")
    h = g[g.location == "Home"]
    adv = h.home_rest - h.away_rest
    d1, d2 = h[adv >= 3], h[adv <= -3]                       # bet the more-rested side
    hit = (d1.home_cover > 0).sum() + (d2.home_cover < 0).sum()
    miss = (d1.home_cover < 0).sum() + (d2.home_cover > 0).sum()
    line("5. team with 3+ more days rest", *rate(hit, miss), "cover")
    early = h[(h.gametime == "13:00") & h.away_team.isin(PACIFIC) & h.home_team.isin(EASTERN)]
    line("6. Pacific team at Eastern home, 1pm ET: bet home", *rate((early.home_cover > 0).sum(), (early.home_cover < 0).sum()), "cover")

    # ---- MEDIA-STYLE TRENDS (controls) ----
    print("\nMEDIA-STYLE TRENDS (controls)")
    div = g[g.div_game == 1]
    dog_home = div[div.spread_line < 0]                      # home is the underdog
    dog_away = div[div.spread_line > 0]
    hit = (dog_home.home_cover > 0).sum() + (dog_away.home_cover < 0).sum()
    miss = (dog_home.home_cover < 0).sum() + (dog_away.home_cover > 0).sum()
    line("7. divisional underdogs", *rate(hit, miss), "cover")
    thu = h[h.weekday == "Thursday"]
    line("8. Thursday home teams", *rate((thu.home_cover > 0).sum(), (thu.home_cover < 0).sum()), "cover")

    print("\n  Breakeven: 52.4% at -110; 50% in a no-vig pool. CI = 95% range.")


if __name__ == "__main__":
    main()
