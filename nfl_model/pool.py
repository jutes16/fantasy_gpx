"""
Pool pick engine: pick the best 5 against a fixed weekly sheet.

Pool assumptions (set from how your pool actually works):
  - straight wins, all 5 picks equal
  - the sheet is fixed and identical for everyone
  - season-long standings

Two consequences that change the math versus real betting:

  1. NO VIG. Breakeven is 50%, not 52.38%. A half point of line value
     is worth playing here; at -110 it is not. The engine is therefore
     more permissive than model.py.

  2. YOU MUST SUBMIT 5. There is no PASS. Some weeks only two or three
     games carry real value, so slots 4 and 5 are coin flips. The engine
     labels them honestly rather than dressing them up.

Because the sheet is shared and fixed, the edge is not "handicap the
game better than the field". It is "spot which of the shared numbers
has gone stale versus the live market". Different question, and the
only one with a measured edge behind it.

    python3 pool.py          -> scores the latest week in data/weekly_lines.xlsx
    python3 pool.py 3        -> a specific week
"""

import numpy as np

POOL_BREAKEVEN = 0.50   # no vig in a pool
COINFLIP_BAND = 0.25    # |value| below this is not a real edge

# Backtested ATS win rate by points of line value held vs the closing
# number. From backtest.py, 5,790 bet sides, 2015-2025.
VALUE_CURVE = {
    0.0: 0.5000, 0.5: 0.5248, 1.0: 0.5451,
    1.5: 0.5629, 2.0: 0.5793, 2.5: 0.5948, 3.0: 0.6101,
}


def band_multiplier(abs_line: float) -> float:
    """
    How much a point of value is worth given where the spread sits.
    From backtest.py table [2].
    """
    a = abs(abs_line)
    if a < 2.5:
        return 0.40   # 51.8% at 1pt: a point buys almost nothing here
    if a < 3.5:
        return 1.45   # 56.5%: best band, the 3 is doing the work
    if a < 6.5:
        return 1.00   # 54.3%
    if a < 7.5:
        return 1.35   # 56.1%
    if a < 10.5:
        return 0.82   # 53.7%
    return 0.70       # 53.1%


def _interp(pts: float) -> float:
    xs = sorted(VALUE_CURVE)
    ys = [VALUE_CURVE[x] for x in xs]
    r = float(np.interp(abs(pts), xs, ys))
    return 1.0 - r if pts < 0 else r


def score_game(g: dict) -> dict:
    """Score one game. Lines are HOME-perspective (negative = home favored)."""
    my, mkt = float(g["my_line"]), float(g["mkt_line"])

    v_home = my - mkt          # value if you take the home side
    v_away = mkt - my          # value if you take the away side
    side = "home" if v_home >= v_away else "away"
    raw = max(v_home, v_away)

    mult = band_multiplier(mkt)
    eff = raw * mult
    rate = _interp(eff)
    base_rate = rate

    # Optional projection tilt. Capped hard: projections are NOT
    # validated by the backtest, and pass a credibility weight in
    # proj_weight (a source running 44% ATS should get ~0).
    pm, pw = g.get("proj_margin"), g.get("proj_weight", 0.0)
    if pm is not None and pw:
        pcov = (pm + my) if side == "home" else (-pm - my)
        rate += float(np.clip(pcov * 0.004 * pw, -0.02, 0.02))

    proj_adj = rate - base_rate
    rate = float(np.clip(rate, 0.30, 0.70))
    team = g["home"] if side == "home" else g["away"]
    num = my if side == "home" else -my

    if raw >= 1.0:
        tier = "EDGE"
    elif raw >= 0.5:
        tier = "slim"
    elif raw > -COINFLIP_BAND:
        tier = "coin flip"
    else:
        tier = "AVOID"      # your number is worse than market

    return dict(
        game=f"{g['away']} @ {g['home']}",
        pick=f"{team} {num:+g}",
        side=side,
        my_line=my, mkt_line=mkt,
        pts_value=round(raw, 2),
        band_mult=mult,
        eff_pts=round(eff, 2),
        win_rate=round(rate, 4),
        base_rate=round(base_rate, 4),
        proj_adj=round(proj_adj, 4),
        tier=tier,
        is_thursday=bool(g.get("is_thursday", False)),
    )


def rank_alternatives(games: list, n: int = 5) -> dict:
    """
    Three defensible ways to pick the 5. Reported side by side because
    on 2025's real pool sheet they disagreed and the "best" one was
    almost certainly noise:

        model (band-weighted)   56/90  62.2%
        raw line value only     58/90  64.4%
        value, tiebreak near 3  61/90  67.8%

    All three CIs overlap heavily. The band-weighted rule is kept as
    the default because it has 2,895 games behind it, not 90. Log all
    three weekly and revisit after a few seasons of real data.
    """
    scored = [score_game(g) for g in games]
    out = {}
    out["model"] = sorted(scored, key=lambda r: -r["win_rate"])[:n]
    out["raw_value"] = sorted(scored, key=lambda r: -r["pts_value"])[:n]
    out["value_near3"] = sorted(
        scored,
        key=lambda r: (-r["pts_value"], -(2.5 <= abs(r["mkt_line"]) <= 3.5)),
    )[:n]
    return out


def best_five(games: list, n: int = 5) -> dict:
    """
    Rank every game and take the top n. Objective is expected wins,
    which for straight-wins scoring is just the sum of win rates.
    """
    scored = sorted((score_game(g) for g in games), key=lambda r: -r["win_rate"])
    card = scored[:n]
    exp_wins = sum(r["win_rate"] for r in card)
    baseline = 0.5 * n
    return dict(
        card=card,
        bench=scored[n:],
        expected_wins=round(exp_wins, 3),
        baseline=baseline,
        edge_wins=round(exp_wins - baseline, 3),
        real_edges=sum(1 for r in card if r["tier"] == "EDGE"),
        coin_flips=sum(1 for r in card if r["tier"] in ("coin flip", "AVOID")),
    )


# Cost of locking all five picks early, measured on 2025's sheet
# (lock_timing.py): ~1.2 wins a season, ~0.07 wins a week, because the
# other four picks get made against a market line ~3 days stale.
THURSDAY_LOCK_COST = 0.07


def thursday_check(res: dict) -> str | None:
    """
    This pool locks every pick at the first game selected. Take a Thursday
    game and the other four lock three days early, against a staler market
    line. Worth it only if the Thursday game clears the pick it displaces
    by more than the lock costs.
    """
    card = res["card"]
    thu = [r for r in card if r.get("is_thursday")]
    if not thu:
        return None

    bench = res["bench"]
    best_thu = max(thu, key=lambda r: r["win_rate"])
    # what you would play instead: worst non-Thursday pick on the card,
    # promoted from the best non-Thursday game on the bench
    non_thu = [r for r in card if not r.get("is_thursday")]
    repl = next((r for r in bench if not r.get("is_thursday")), None)
    if repl is None or not non_thu:
        return None
    displaced = min(non_thu, key=lambda r: r["win_rate"])

    gain = best_thu["win_rate"] - repl["win_rate"]
    net = gain - THURSDAY_LOCK_COST

    L = ["", "THURSDAY LOCK WARNING"]
    L.append(f"  {best_thu['pick']} is a Thursday game. Taking it locks all")
    L.append(f"  five picks ~3 days early.")
    L.append(f"    edge over next bench option ({repl['pick']}): {gain*100:+.1f} pp")
    L.append(f"    cost of locking the other four:              {-THURSDAY_LOCK_COST*100:.1f} pp")
    L.append(f"    net:                                         {net*100:+.1f} pp")
    if net > 0:
        L.append("  -> worth taking.")
    else:
        L.append(f"  -> NOT worth it. Drop it, promote {repl['pick']},")
        L.append("     and lock Sunday 1pm instead.")
    return "\n".join(L)


def fmt(res: dict, show_bench: bool = True) -> str:
    L = []
    L.append("=" * 68)
    L.append("POOL CARD — best 5")
    L.append("=" * 68)
    hdr = f"{'#':<3}{'GAME':<14}{'PICK':<13}{'VAL':>6}{'WIN%':>8}   TIER"
    L.append(hdr)
    L.append("-" * len(hdr))
    for i, r in enumerate(res["card"], 1):
        L.append(
            f"{i:<3}{r['game']:<14}{r['pick']:<13}{r['pts_value']:>+6.1f}"
            f"{r['win_rate']*100:>7.1f}%   {r['tier']}"
        )
    L.append("")
    L.append(f"expected wins: {res['expected_wins']:.2f} of 5")
    L.append(f"coin-flip baseline: {res['baseline']:.2f}")
    L.append(f"edge: {res['edge_wins']:+.2f} wins this week")
    L.append(f"real edges on the card: {res['real_edges']}   coin flips: {res['coin_flips']}")

    if show_bench and res["bench"]:
        L.append("")
        L.append("not on the card:")
        for r in res["bench"]:
            L.append(
                f"    {r['game']:<14}{r['pick']:<13}{r['pts_value']:>+6.1f}"
                f"{r['win_rate']*100:>7.1f}%   {r['tier']}"
            )
    return "\n".join(L)


def _signals(g, side):
    """Context signals for one pick, from the pick's side. None when not recorded.

    move  : points the market moved toward your team since the open
    tix/$ : % of tickets / money on your team
    elway : points ELWAY's projection favours your team over your line
    """
    home = side == "home"
    num = lambda k: (None if g.get(k) is None or (isinstance(g.get(k), float) and np.isnan(g[k]))
                     else float(g[k]))
    out = {}
    o, m = num("open_line"), num("mkt_line")
    out["move"] = None if o is None else ((o - m) if home else (m - o))
    b, d = num("home_bets_pct"), num("home_money_pct")
    out["tix"] = None if b is None else (b if home else 100 - b)
    out["money"] = None if d is None else (d if home else 100 - d)
    sh = str(g.get("sharp_side") or "").strip().lower()
    out["sharp"] = None if sh not in ("home", "away") else ("with" if sh == side else "AGAINST")
    st = str(g.get("sharp_type") or "").strip().lower()
    out["sharp_type"] = st if st in ("book", "pro") else ""
    e = num("elway_line")
    out["elway"] = None if e is None else ((float(g["my_line"]) - e) if home else (e - float(g["my_line"])))
    return out


def factors_view(games, res):
    """What goes into each pick's win %, and what else is known about it."""
    try:
        from margins import clv_prob          # key-number value of your line vs the market
    except Exception:                         # games_clean.csv missing: skip that column
        clv_prob = None
    by_game = {f"{g['away']} @ {g['home']}": g for g in games}
    rows = [(r, True) for r in res["card"]] + [(r, False) for r in res["bench"]]

    f = lambda v, spec, dash="-": dash if v is None else format(v, spec)
    L = ["", "FACTORS  (card first, then the bench)",
         "  sets WIN%:  VAL = your line vs market (pts) x BAND (spread-band weight) -> BASE%,",
         "              + ELWAY = projection tilt (capped +/-2 pts)",
         "  context only (tested, no edge at the close; not in WIN%):",
         "              KEY% = VAL as cover probability (key numbers counted), MOVE = market",
         "              move toward your team since the open, TIX/$ = % of tickets / money on",
         "              your team, SHARP = reported sharp side, ELWAY GAP = pts ELWAY likes your",
         "              team more than your line",
         f"{'':2}{'GAME':<12}{'PICK':<11}{'VAL':>5}{'BAND':>6}{'BASE%':>7}{'ELWAY':>7}{'WIN%':>7}"
         f" |{'KEY%':>6}{'MOVE':>6}{'TIX/$':>9}{'SHARP':>12}{'ELWAY GAP':>10}"]
    for r, on_card in rows:
        g = by_game[r["game"]]
        s = _signals(g, r["side"])
        key = clv_prob(r["my_line"], r["side"], r["mkt_line"]) * 100 if clv_prob else None
        split = "-" if s["tix"] is None else f"{s['tix']:.0f}/{f(s['money'], '.0f')}"
        sharp = "-" if s["sharp"] is None else s["sharp"] + (f" ({s['sharp_type']})" if s["sharp_type"] else "")
        L.append(
            f"{'*' if on_card else ' ':<2}{r['game']:<12}{r['pick']:<11}{r['pts_value']:>+5.1f}"
            f"{r['band_mult']:>5.2f}x{r['base_rate']*100:>6.1f}%{r['proj_adj']*100:>+6.1f}"
            f"{r['win_rate']*100:>6.1f}% |{f(key, '+5.1f'):>6}{f(s['move'], '+5.1f'):>6}"
            f"{split:>9}{sharp:>12}{f(s['elway'], '+5.1f'):>10}")
    L.append("  * = on the card.  ELWAY column is in win-% points; KEY% in cover-% points.")
    return "\n".join(L)


# ELWAY (import_elway.py) feeds the projection tilt in score_game. The tilt is
# capped at +/-2 pp whatever the weight, so line value still drives the card.
# 0 = show ELWAY but ignore it; 1 = full tilt. Raise it only if the ELWAY
# section of pool_tracker.py report earns it (gap 1+ row, many weeks).
ELWAY_WEIGHT = 0.5


def elway_view(games, res):
    """ELWAY's lean on every game vs your line, flagged against the card."""
    rows = [g for g in games if g.get("elway_line") is not None]
    if not rows:
        return ""
    card = {r["game"]: r["side"] for r in res["card"]}
    L = [f"\nELWAY view (weight {ELWAY_WEIGHT:g}; projection vs your line)",
         f"{'GAME':<13} {'YOUR LINE':>9} {'ELWAY':>7} {'GAP':>5}  {'ELWAY SIDE':<12} CARD"]
    for g in sorted(rows, key=lambda g: -abs(g["elway_line"] - g["my_line"])):
        lbl = f"{g['away']} @ {g['home']}"
        gap = g["elway_line"] - g["my_line"]
        if gap == 0:
            lean, note = "no lean", ""
        else:
            side = "home" if gap < 0 else "away"
            team = g["home"] if side == "home" else g["away"]
            num = g["my_line"] if side == "home" else -g["my_line"]
            lean = f"{team} {num:+g}"
            note = ("agrees" if card.get(lbl) == side else
                    "DISAGREES" if lbl in card else "")
        L.append(f"{lbl:<13} {g['my_line']:>+9g} {g['elway_line']:>+7g} "
                 f"{abs(gap):>5g}  {lean:<12} {note}")
    return "\n".join(L)


if __name__ == "__main__":
    import sys
    from sample_week import load_games
    games = load_games(int(sys.argv[1]) if len(sys.argv) > 1 else None)
    for g in games:
        if g.get("elway_line") is not None:
            g["proj_margin"] = -float(g["elway_line"])   # home margin
            g["proj_weight"] = ELWAY_WEIGHT
    res = best_five(games)
    print(fmt(res))
    print(factors_view(games, res))
    print(elway_view(games, res))

    warn = thursday_check(res)
    if warn:
        print(warn)

    alts = rank_alternatives(games)
    base = {r["pick"] for r in alts["model"]}
    disagree = {k: [r["pick"] for r in v if r["pick"] not in base]
                for k, v in alts.items() if k != "model"}
    if any(disagree.values()):
        print("\nalternate rules would swap in:")
        for k, picks in disagree.items():
            if picks:
                print(f"  {k:<13} {', '.join(picks)}")
        print("  (all three rules are within noise of each other on 90 picks;")
        print("   the default is the one with the most games behind it)")
