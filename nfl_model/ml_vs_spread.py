"""
Can ELWAY make money, and in which instrument: spread or moneyline?

    python3 ml_vs_spread.py            # both parts
    python3 ml_vs_spread.py ledger     # part 1 only
    python3 ml_vs_spread.py straight   # part 2 only

PART 1 -- the registered Kalshi test (data/elway/kalshi_test_<season>_wk<NN>.csv).
    Each row pairs a team's WIN contract with its SPREAD contract at one strike:
    buy $1 of the one ELWAY likes more relative to price, sell $1 of the other,
    zero net outlay. Executable prices are rebuilt from the Kalshi mids: cross
    half the bid-ask (1c tick: half = 0.5c on an x.5 mid, else 1c) and pay the
    Kalshi fee 0.07 * P * (1 - P) per contract on each leg. The rebuilt expected
    values are checked against the published ones, then every pair is settled
    on the actual score. The question is which forecast the realized total
    lands nearer: ELWAY's (sum of EV_ELWAY) or the market's (pure frictions).

PART 2 -- straight bets at sportsbook closing prices (nflverse), for every week
    with ELWAY probabilities:
      moneyline: bet the side where ELWAY's win probability beats the price
      spread:    bet the side where ELWAY's cover probability beats the price
    ELWAY publishes a win probability and a projected spread but not its full
    margin distribution, so cover probabilities use a normal margin model
    (sd 13.5, centred on ELWAY's projected margin). That ignores key numbers
    (3, 7) and is the main approximation here.

All of it is a handful of games. Treat the output as a running ledger, not a
verdict: the point is to see which forecast, ELWAY's or the market's, the
realized P&L tracks as weeks accumulate.
"""

import glob
import os
import sys

import numpy as np
import pandas as pd
from scipy import stats

from fetch_splits import canon

NFLVERSE = "data/games_raw.csv"          # refresh: see README "Refreshing the data"
NFLVERSE_URL = "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"
SEASON = 2026
MARGIN_SD = 13.5
KALSHI_FEE = 0.07


# ------------------------------------------------------------------ results
def load_games(season=SEASON):
    """Final scores + closing lines/odds, keyed by (week, team) for either side."""
    g = pd.read_csv(NFLVERSE, low_memory=False) if os.path.exists(NFLVERSE) else None
    if g is None or not (g.season == season).any():      # local copy is stale
        g = pd.read_csv(NFLVERSE_URL, low_memory=False)
    g = g[(g.season == season) & (g.game_type == "REG") & g.result.notna()].copy()
    g["away"], g["home"] = g.away_team.map(canon), g.home_team.map(canon)
    return g


def team_margin(games, week, team):
    """(margin from `team`'s side, the game row) or (None, None)."""
    r = games[(games.week == week) & ((games.home == team) | (games.away == team))]
    if r.empty:
        return None, None
    r = r.iloc[0]
    return (r.result if r.home == team else -r.result), r


# ------------------------------------------------------------------ part 1
def fee(p):
    return KALSHI_FEE * p * (1 - p)


def half_spread(mid_pct):
    return 0.005 if round(mid_pct * 2) % 2 else 0.01


def buy_cost(mid_pct):
    p = mid_pct / 100 + half_spread(mid_pct)
    return p + fee(p)


def sell_net(mid_pct):
    p = mid_pct / 100 - half_spread(mid_pct)
    return p - fee(p)


def ledger(path):
    t = pd.read_csv(path)
    games = load_games()
    rows = []
    for r in t.itertuples():
        k = float(str(r.strike).replace("+", ""))
        m, _ = team_margin(games, r.week, canon(r.team))
        if m is None:
            print(f"  no final score for {r.team} wk{r.week}")
            continue
        wins = m > 0
        covers = m > -k        # +6.5: lose by 6 or less (or win); -3.5: win by 4+
        pw_k, ps_k = r.kalshi_win / 100, r.kalshi_spread / 100
        pw_e, ps_e = r.elway_win / 100, r.elway_spread / 100
        bet = str(r.bet)
        if bet.startswith("buy win"):
            n_buy, n_sell = 1 / buy_cost(r.kalshi_win), 1 / sell_net(r.kalshi_spread)
            ev_e = pw_e * n_buy - ps_e * n_sell
            ev_k = pw_k * n_buy - ps_k * n_sell
            pnl = n_buy * wins - n_sell * covers
        elif bet.startswith("buy spread"):
            n_buy, n_sell = 1 / buy_cost(r.kalshi_spread), 1 / sell_net(r.kalshi_win)
            ev_e = ps_e * n_buy - pw_e * n_sell
            ev_k = ps_k * n_buy - pw_k * n_sell
            pnl = n_buy * covers - n_sell * wins
        else:
            ev_e = ev_k = pnl = np.nan
        rows.append(dict(team=r.team, opp=r.opp, strike=r.strike, bet=bet,
                         ev_elway_pub=r.ev_elway, ev_elway=ev_e,
                         ev_kalshi_pub=r.ev_kalshi, ev_kalshi=ev_k,
                         margin=m, won=wins, covered=covers, pnl=pnl))
    return pd.DataFrame(rows)


def print_ledger():
    for path in sorted(glob.glob("data/elway/kalshi_test_*.csv")):
        d = ledger(path)
        print("=" * 78)
        print(f"PART 1  registered Kalshi pairs: {os.path.basename(path)}")
        print("=" * 78)
        print(f"{'pair':<12}{'strike':>7}  {'bet':<20}{'EV elway':>9}{'EV mkt':>8}"
              f"{'margin':>8}{'win':>5}{'cov':>5}{'P&L':>8}")
        for r in d.itertuples():
            if r.bet == "no bet":
                continue
            print(f"{r.team + ' v ' + r.opp:<12}{r.strike:>7}  {r.bet:<20}"
                  f"{r.ev_elway:>+9.2f}{r.ev_kalshi:>+8.2f}{r.margin:>+8.0f}"
                  f"{'Y' if r.won else 'n':>5}{'Y' if r.covered else 'n':>5}{r.pnl:>+8.2f}")
        b = d[d.bet != "no bet"]
        dev = max((b.ev_elway - b.ev_elway_pub).abs().max(),
                  (b.ev_kalshi - b.ev_kalshi_pub).abs().max())
        print(f"\n  price rebuild check: max |rebuilt EV - published EV| = {dev:.3f}"
              + ("  (matches to rounding)" if dev <= 0.015 else "  (CHECK: prices differ)"))
        print(f"  {len(b)} pairs, $1 bought + $1 sold each, zero net outlay")
        print(f"    ELWAY forecast : {b.ev_elway.sum():+.2f}   (published {b.ev_elway_pub.sum():+.2f})")
        print(f"    market forecast: {b.ev_kalshi.sum():+.2f}   (published {b.ev_kalshi_pub.sum():+.2f}; pure frictions)")
        print(f"    REALIZED       : {b.pnl.sum():+.2f}")
        # how noisy is one week of this? sd of the ledger under ELWAY's own probabilities
        sd = np.sqrt(sum(_pair_var(r) for r in b.itertuples()))
        print(f"    one-week noise : sd about {sd:.2f} under ELWAY's probabilities, so a single"
              f" week\n                     can't separate {b.ev_elway.sum():+.2f} from"
              f" {b.ev_kalshi.sum():+.2f}")
        nb = d[d.bet == "no bet"]
        if len(nb):
            print(f"  no-bet games (for the record): " + ", ".join(
                f"{r.team} {r.strike} ({'won' if r.won else 'lost'}, "
                f"{'covered' if r.covered else 'no cover'})" for r in nb.itertuples()))
        print()


def _pair_var(r):
    """Variance of one pair's P&L under ELWAY's probabilities (3 outcomes)."""
    t = pd.read_csv(glob.glob("data/elway/kalshi_test_*.csv")[0])
    x = t[(t.team == r.team) & (t.strike == r.strike)].iloc[0]
    pw, ps = x.elway_win / 100, x.elway_spread / 100
    if r.bet.startswith("buy win"):
        nb, ns = 1 / buy_cost(x.kalshi_win), 1 / sell_net(x.kalshi_spread)
        outs = [(pw, nb - ns), (ps - pw, -ns), (1 - ps, 0.0)]
    else:
        nb, ns = 1 / buy_cost(x.kalshi_spread), 1 / sell_net(x.kalshi_win)
        outs = [(ps, nb - ns), (pw - ps, -ns), (1 - pw, 0.0)]
    mu = sum(p * v for p, v in outs)
    return sum(p * (v - mu) ** 2 for p, v in outs)


# ------------------------------------------------------------------ part 2
def american_to_decimal(a):
    return 1 + (a / 100 if a > 0 else 100 / -a)


def elway_probs():
    """ELWAY home win prob and projected home margin per game, every week we have.

    Week 3 onward: import_elway.py columns in the picks log.
    Registered Kalshi weeks: the ledger's win probability; margin backed out
    of it with the same normal model.
    """
    out = []
    log = "data/pool_picks_log.csv"
    if os.path.exists(log):
        l = pd.read_csv(log)
        l = l[(l.season == SEASON)]
        if "elway_line" in l:
            l = l.dropna(subset=["elway_line", "elway_home_wp"])
            for r in l.itertuples():
                out.append(dict(week=r.week, home=canon(r.home), away=canon(r.away),
                                home_wp=r.elway_home_wp, home_mu=-r.elway_line,
                                src="ELWAY table"))
    for path in glob.glob("data/elway/kalshi_test_*.csv"):
        t = pd.read_csv(path)
        for r in t.itertuples():
            wp = r.elway_win / 100
            home_wp = wp if r.team_is_home else 1 - wp
            out.append(dict(week=r.week,
                            home=canon(r.team if r.team_is_home else r.opp),
                            away=canon(r.opp if r.team_is_home else r.team),
                            home_wp=home_wp,
                            home_mu=MARGIN_SD * stats.norm.ppf(home_wp),
                            src="Kalshi test (margin from win prob)"))
    d = pd.DataFrame(out)
    return d.drop_duplicates(["week", "home", "away"]) if len(d) else d


def straight_bets():
    games = load_games()
    e = elway_probs()
    if e.empty:
        print("no ELWAY probabilities on file")
        return
    g = games.merge(e, on=["week", "home", "away"], how="inner")
    rows = []
    for r in g.itertuples():
        # nflverse: spread_line > 0 = home favored by that much; result = home margin
        # ---- moneyline
        best = None
        for side, p, a in (("home", r.home_wp, r.home_moneyline),
                           ("away", 1 - r.home_wp, r.away_moneyline)):
            if pd.isna(a):
                continue
            dec = american_to_decimal(a)
            ev = p * dec - 1
            if best is None or ev > best[1]:
                best = (side, ev, dec, p)
        if best and best[1] > 0:
            side, ev, dec, p = best
            won = (r.result > 0) if side == "home" else (r.result < 0)
            push = r.result == 0
            rows.append(dict(week=r.week, game=f"{r.away} @ {r.home}", kind="moneyline",
                             pick=f"{r.home if side == 'home' else r.away} ML",
                             price=r.home_moneyline if side == "home" else r.away_moneyline,
                             p=p, ev=ev, pnl=0.0 if push else (dec - 1 if won else -1.0)))
        # ---- spread (normal margin model around ELWAY's projected margin)
        cov_home = 1 - stats.norm.cdf(r.spread_line, loc=r.home_mu, scale=MARGIN_SD)
        best = None
        for side, p, a in (("home", cov_home, r.home_spread_odds),
                           ("away", 1 - cov_home, r.away_spread_odds)):
            if pd.isna(a):
                continue
            dec = american_to_decimal(a)
            ev = p * dec - 1
            if best is None or ev > best[1]:
                best = (side, ev, dec, p)
        if best and best[1] > 0:
            side, ev, dec, p = best
            cover = r.result - r.spread_line          # >0 home covers
            won = cover > 0 if side == "home" else cover < 0
            num = -r.spread_line if side == "home" else r.spread_line
            rows.append(dict(week=r.week, game=f"{r.away} @ {r.home}", kind="spread",
                             pick=f"{r.home if side == 'home' else r.away} {num:+g}",
                             price=r.home_spread_odds if side == "home" else r.away_spread_odds,
                             p=p, ev=ev, pnl=0.0 if cover == 0 else (dec - 1 if won else -1.0)))
    b = pd.DataFrame(rows)

    print("=" * 78)
    print("PART 2  straight bets on ELWAY at sportsbook closing prices ($1 per bet)")
    print("=" * 78)
    print(f"  weeks with ELWAY probabilities: {sorted(e.week.unique())}   games: {len(g)}")
    for kind in ("moneyline", "spread"):
        k = b[b.kind == kind]
        print(f"\n  {kind.upper()}: {len(k)} bets (every game where ELWAY's EV > 0 at the close)")
        for r in k.sort_values(["week", "ev"], ascending=[True, False]).itertuples():
            print(f"    wk{r.week} {r.game:<11} {r.pick:<10} {r.price:>+5.0f}  "
                  f"ELWAY {r.p:5.1%}  EV {r.ev:+.2f}  P&L {r.pnl:+.2f}")
        if len(k):
            w = (k.pnl > 0).sum()
            print(f"    record {w}-{(k.pnl < 0).sum()}   ELWAY expected {k.ev.sum():+.2f}"
                  f"   REALIZED {k.pnl.sum():+.2f}   ROI {k.pnl.sum() / len(k):+.1%}")
            for cut in (0.05, 0.10):
                c = k[k.ev >= cut]
                if len(c):
                    print(f"      EV >= {cut:.2f}: {len(c)} bets, realized {c.pnl.sum():+.2f}"
                          f"  (ROI {c.pnl.sum() / len(c):+.1%})")
    print("\n  Note: ELWAY's probabilities vs the CLOSE are the strict test; you would")
    print("  bet earlier, at different prices. Spread cover probabilities are a normal")
    print("  approximation (no key numbers), so moneyline is the cleaner of the two.")


if __name__ == "__main__":
    part = sys.argv[1] if len(sys.argv) > 1 else "all"
    if part in ("all", "ledger"):
        print_ledger()
    if part in ("all", "straight"):
        straight_bets()
