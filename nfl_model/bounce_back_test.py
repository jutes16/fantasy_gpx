"""
Is there an edge betting teams coming off a loss (straight-up or against the
spread)?

    python3 bounce_back_test.py

Every regular-season game since 2015 (nflverse, closing lines), seen from
each team's side. A team "qualifies" when its previous game that season was
a loss (SU), a non-cover (ATS), or both. The bet is that team against the
CLOSING spread (and, separately, on the closing moneyline).

The pattern was noticed in 2025, so 2025 is reported apart from the other
seasons: a pattern spotted in one season always looks good in that season.
The honest test is 2015-2024 plus 2026 to date.

Breakeven against the spread at -110 is 52.38%; a pool with no vig needs 50%.
"""

import numpy as np
import pandas as pd

NFLVERSE = "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"
DISCOVERY = 2025
BREAKEVEN = 0.5238


def wilson(w, n, z=1.959963985):
    if n == 0:
        return np.nan, np.nan, np.nan
    p = w / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return p, c - h, c + h


def ml_payout(a):
    """Profit on a $1 moneyline bet that wins."""
    return a / 100 if a > 0 else 100 / -a


def team_games():
    g = pd.read_csv(NFLVERSE, low_memory=False)
    g = g[(g.game_type == "REG") & (g.season >= 2015) & g.result.notna() & g.spread_line.notna()]
    # nflverse: result = home margin; spread_line > 0 = home favoured by that much
    home = pd.DataFrame(dict(season=g.season, week=g.week, game_id=g.game_id, team=g.home_team,
                             opp=g.away_team, home=True, margin=g.result,
                             line=-g.spread_line, ml=g.home_moneyline))
    away = pd.DataFrame(dict(season=g.season, week=g.week, game_id=g.game_id, team=g.away_team,
                             opp=g.home_team, home=False, margin=-g.result,
                             line=g.spread_line, ml=g.away_moneyline))
    t = pd.concat([home, away]).sort_values(["team", "season", "week"]).reset_index(drop=True)
    t["ats"] = t.margin + t.line                 # > 0 covered, 0 push, < 0 lost ATS
    t["fav"] = t.line < 0
    # previous game for the same team in the same season (byes skipped)
    grp = t.groupby(["team", "season"])
    t["prev_margin"] = grp.margin.shift()
    t["prev_ats"] = grp.ats.shift()
    t["prev_week"] = grp.week.shift()
    return t[t.prev_margin.notna()].copy()


def record(d):
    w, l, p = (d.ats > 0).sum(), (d.ats < 0).sum(), (d.ats == 0).sum()
    rate, lo, hi = wilson(w, w + l)
    roi = (w * (100 / 110) - l) / (w + l) if w + l else np.nan     # flat bets at -110
    ml = d.dropna(subset=["ml"])
    ml_roi = np.where(ml.margin > 0, ml.ml.map(ml_payout), np.where(ml.margin < 0, -1.0, 0.0)).mean() \
        if len(ml) else np.nan
    return dict(n=w + l, W=w, L=l, P=p, cover=rate, lo=lo, hi=hi, roi_110=roi, ml_roi=ml_roi)


def show(title, rows):
    print(f"\n{title}")
    print(f"  {'':<34}{'bets':>6}{'W-L':>11}{'cover':>8}{'95% CI':>14}{'ROI -110':>10}{'ML ROI':>8}")
    for name, r in rows:
        print(f"  {name:<34}{r['n']:>6}{f'{r[chr(87)]}-{r[chr(76)]}':>11}{r['cover']*100:>7.1f}%"
              f"{f'[{r[chr(108)+chr(111)]*100:.0f}, {r[chr(104)+chr(105)]*100:.0f}]':>14}"
              f"{r['roi_110']*100:>+9.1f}%{r['ml_roi']*100:>+7.1f}%")


def main():
    t = team_games()
    # one side per game only: drop games where BOTH teams qualify for a signal
    # (you can't bet both sides), handled per signal below
    signals = {
        "off SU loss": t.prev_margin < 0,
        "off ATS loss": t.prev_ats < 0,
        "off SU AND ATS loss": (t.prev_margin < 0) & (t.prev_ats < 0),
        "off SU loss but covered": (t.prev_margin < 0) & (t.prev_ats > 0),
        "off SU win but no cover": (t.prev_margin > 0) & (t.prev_ats < 0),
        "off loss by 14+": t.prev_margin <= -14,
        "off ATS loss by 10+": t.prev_ats <= -10,
        "(control) off SU win": t.prev_margin > 0,
    }

    def one_side(mask):
        q = t[mask]
        both = q.game_id.duplicated(keep=False)        # both teams qualify
        return q[~both]

    holdout = lambda d: d[d.season != DISCOVERY]
    disc = lambda d: d[d.season == DISCOVERY]

    print("=" * 92)
    print("BOUNCE-BACK TEST: bet the team coming off a loss, at the closing line")
    print(f"regular season 2015-2026 to date, {len(t):,} team-games (week 1 excluded: no prior game)")
    print("games where both teams qualify are skipped (you can't bet both sides)")
    print("=" * 92)
    show(f"HOLDOUT: every season except {DISCOVERY} (the honest test)",
         [(k, record(holdout(one_side(m)))) for k, m in signals.items()])
    show(f"DISCOVERY SEASON {DISCOVERY} (where the pattern was noticed)",
         [(k, record(disc(one_side(m)))) for k, m in signals.items()])

    # season by season for the two headline signals
    for k in ("off SU loss", "off ATS loss"):
        d = one_side(signals[k])
        print(f"\n  {k}, by season:")
        line = []
        for s, x in d.groupby("season"):
            r = record(x)
            line.append(f"{s}: {r['cover']*100:.0f}% ({r['n']})")
        print("   " + "   ".join(line[:6]) + "\n   " + "   ".join(line[6:]))
        rates = [record(x)["cover"] for _, x in d.groupby("season")]
        print(f"   seasons above 50%: {sum(r > .5 for r in rates)} of {len(rates)};"
              f" above 52.4% (beats -110): {sum(r > BREAKEVEN for r in rates)} of {len(rates)}")

    # is it just a dog / home effect in disguise?
    d = holdout(one_side(signals["off SU loss"]))
    show("HOLDOUT, off SU loss, split by role this week",
         [("as underdog", record(d[~d.fav])), ("as favourite", record(d[d.fav])),
          ("at home", record(d[d.home])), ("on the road", record(d[~d.home]))])
    base = holdout(t)
    show("HOLDOUT baseline: ALL teams in those roles (for comparison)",
         [("all underdogs", record(base[~base.fav])), ("all favourites", record(base[base.fav])),
          ("all home", record(base[base.home])), ("all road", record(base[~base.home]))])


if __name__ == "__main__":
    main()
