"""
NFL ATS backtest, 2015-2025 regular season.

Answers the question the whole pick model rests on:
  how much ATS win rate does one point of line value actually buy,
  and does it depend on where the spread sits?

Data: nflverse games.csv (closing spread, closing total, final scores).
      spread_line is stated from the HOME team's perspective:
      positive = home favored by that many.
      result = home_score - away_score.

Limitation: only the CLOSING line is archived. There is no opening-line
history here, so open-to-close movement cannot be tested. Betting splits
(ticket/handle %) are not archived anywhere free, so those signals are
forward-trackable only.
"""

import numpy as np
import pandas as pd
from scipy import stats

RAW = "data/games_raw.csv"  # re-fetch: curl -sSL -o data/games_raw.csv https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv
SEASONS = range(2015, 2026)
VIG_ODDS = -110


def load():
    df = pd.read_csv(RAW, low_memory=False)
    d = df[
        (df.season.isin(SEASONS))
        & (df.game_type == "REG")
        & df.spread_line.notna()
        & df.result.notna()
    ].copy()

    # Home-perspective margin against the closing spread.
    # cover_margin > 0  -> home covered
    # cover_margin == 0 -> push
    d["cover_margin"] = d["result"] - d["spread_line"]
    d["home_margin"] = d["result"]
    d["abs_spread"] = d["spread_line"].abs()
    d["total_pts"] = d["home_score"] + d["away_score"]
    return d.reset_index(drop=True)


def wilson(wins, n, conf=0.95):
    """Wilson score interval. Pushes must already be excluded from n."""
    if n == 0:
        return (np.nan, np.nan, np.nan)
    z = stats.norm.ppf(1 - (1 - conf) / 2)
    p = wins / n
    den = 1 + z**2 / n
    ctr = (p + z**2 / (2 * n)) / den
    half = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return (p, ctr - half, ctr + half)


def roi_units(wins, losses, odds=VIG_ODDS):
    """Units won risking 1 unit per bet at American odds."""
    payout = 100 / abs(odds) if odds < 0 else odds / 100
    return wins * payout - losses


def grade(margins, line_shift):
    """
    Grade a set of bets where the bettor's number is `line_shift` points
    BETTER than the closing line.

    margins: array of (actual margin - closing spread) from the bettor's side.
    A bet on a side with +line_shift of value wins when
    margin + line_shift > 0, pushes at 0, loses below.
    """
    adj = margins + line_shift
    w = int((adj > 0).sum())
    l = int((adj < 0).sum())
    p = int((adj == 0).sum())
    return w, l, p


def value_of_points(d, shifts=(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0)):
    """
    Core result: ATS win rate as a function of how many points of line
    value you hold versus the closing number.

    Uses BOTH sides of every game so the sample is side-agnostic:
    the home cover margin and its negation (the away side).
    """
    m = np.concatenate([d.cover_margin.values, -d.cover_margin.values])
    rows = []
    for s in shifts:
        w, l, p = grade(m, s)
        n = w + l
        rate, lo, hi = wilson(w, n)
        rows.append(
            dict(
                pts_of_value=s,
                bets=len(m),
                wins=w,
                losses=l,
                pushes=p,
                win_rate=rate,
                ci_lo=lo,
                ci_hi=hi,
                units=roi_units(w, l),
                roi=roi_units(w, l) / len(m),
            )
        )
    return pd.DataFrame(rows)


def value_by_spread_band(d, shift=1.0):
    """Does a point of value buy more near key numbers than elsewhere?"""
    bands = [(0, 2.5), (2.5, 3.5), (3.5, 6.5), (6.5, 7.5), (7.5, 10.5), (10.5, 99)]
    rows = []
    for lo_b, hi_b in bands:
        sub = d[(d.abs_spread >= lo_b) & (d.abs_spread < hi_b)]
        if len(sub) < 30:
            continue
        m = np.concatenate([sub.cover_margin.values, -sub.cover_margin.values])
        w, l, p = grade(m, shift)
        n = w + l
        rate, lo, hi = wilson(w, n)
        rows.append(
            dict(
                spread_band=f"{lo_b}-{hi_b}",
                games=len(sub),
                bets=len(m),
                win_rate=rate,
                ci_lo=lo,
                ci_hi=hi,
                pushes=p,
                units=roi_units(w, l),
            )
        )
    return pd.DataFrame(rows)


def margin_frequency(d, top=14):
    """Which final margins actually occur. Drives key-number logic."""
    marg = d.home_margin.abs()
    vc = marg.value_counts().sort_values(ascending=False).head(top)
    out = pd.DataFrame(
        dict(margin=vc.index.astype(int), games=vc.values, pct=vc.values / len(d) * 100)
    )
    return out.reset_index(drop=True)


def naive_strategies(d):
    """
    Flat strategies against the closing line. If the market is efficient
    these should all sit at ~50% and lose to the vig. Included so the
    model is not credited for edges that are actually noise.
    """
    tests = {}
    cm = d.cover_margin.values

    tests["always home"] = cm
    tests["always away"] = -cm

    fav_home = d.spread_line > 0
    tests["always favorite"] = np.where(fav_home, cm, -cm)
    tests["always underdog"] = np.where(fav_home, -cm, cm)

    hd = d.spread_line < 0  # home is the dog
    tests["home underdog"] = cm[hd]
    big = d.abs_spread >= 7
    tests["dog +7 or more"] = np.where(fav_home[big], -cm[big], cm[big])
    tests["favorite -7 or more"] = np.where(fav_home[big], cm[big], -cm[big])

    div = d.div_game == 1
    tests["divisional underdog"] = np.where(fav_home[div], -cm[div], cm[div])

    rows = []
    for name, m in tests.items():
        m = np.asarray(m, dtype=float)
        w, l, p = grade(m, 0.0)
        n = w + l
        rate, lo, hi = wilson(w, n)
        rows.append(
            dict(
                strategy=name,
                bets=len(m),
                win_rate=rate,
                ci_lo=lo,
                ci_hi=hi,
                pushes=p,
                units=roi_units(w, l),
                roi=roi_units(w, l) / len(m),
                beats_vig=bool(lo > 0.5238) if not np.isnan(lo) else False,
            )
        )
    return pd.DataFrame(rows).sort_values("win_rate", ascending=False)


def breakeven_value_needed():
    """How many points of value you need for the bet to be +EV at -110."""
    return 0.5238


def main():
    d = load()
    print("=" * 74)
    print(f"NFL ATS BACKTEST  |  {d.season.min()}-{d.season.max()} regular season")
    print(f"games: {len(d)}   breakeven at -110: 52.38%")
    print("=" * 74)

    print("\n[1] VALUE OF LINE VALUE  (both sides of every game)")
    vp = value_of_points(d)
    print(vp.to_string(index=False, float_format=lambda x: f"{x:.4f}"))

    print("\n[2] ONE POINT OF VALUE, BY WHERE THE SPREAD SITS")
    vb = value_by_spread_band(d, shift=1.0)
    print(vb.to_string(index=False, float_format=lambda x: f"{x:.4f}"))

    print("\n[3] MOST COMMON FINAL MARGINS")
    mf = margin_frequency(d)
    print(mf.to_string(index=False, float_format=lambda x: f"{x:.2f}"))

    print("\n[4] NAIVE STRATEGIES vs THE CLOSING LINE (efficiency check)")
    ns = naive_strategies(d)
    print(ns.to_string(index=False, float_format=lambda x: f"{x:.4f}"))

    d.to_csv("data/games_clean.csv", index=False)
    vp.to_csv("data/out_value_of_points.csv", index=False)
    vb.to_csv("data/out_value_by_band.csv", index=False)
    mf.to_csv("data/out_margin_freq.csv", index=False)
    ns.to_csv("data/out_naive.csv", index=False)
    print("\nwrote data/games_clean.csv and 4 result tables")


if __name__ == "__main__":
    main()
