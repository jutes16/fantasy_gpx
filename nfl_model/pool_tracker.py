"""
Pool tracker: weekly 5-pick record, season standings, CLV, calibration.

    python3 pool_tracker.py report   <- your submitted picks vs the model's card,
                                        read from data/pool_picks_log.csv
                                        (written by log_week.py submit/grade)
    python3 pool_tracker.py claude 4 MIN,CLE,NYJ,TEN,IND
                                     <- record Claude's 5 for a week (team codes);
                                        graded like the others once you run
                                        log_week.py grade
                                     report also grades market signals (sharp
                                     side, money vs tickets, line movement)
                                     from the columns in weekly_lines.xlsx
    python3 pool_tracker.py legacy   <- old report from data/archive/pool_log.csv
    python3 pool_tracker.py seed     <- load the Week 3 2026 card into pool_log.csv

Log columns (data/archive/pool_log.csv):
    season, week, slot, game, pick, my_line, mkt_line, closing_line,
    pts_value, tier, model_win_rate, result(W/L/P), margin
"""

import os
import sys
import numpy as np
import pandas as pd
from scipy import stats

LOG = "data/archive/pool_log.csv"
SUBMISSIONS = "data/pool_picks_log.csv"
CLAUDE_PICKS = "data/claude_picks.csv"
# past seasons: data/signals_<season>.csv (hand-collected sharp sides/splits) and,
# if that year's pool sheet exists, data/pool_<season>_merged.csv
CURRENT_SEASON = 2026
NFLVERSE_GAMES = "data/games_clean.csv"
ACTION_SPLITS = "data/action_splits.csv"  # fetch_splits.py (Action Network via Apify)
PICKS_PER_WEEK = 5
BASELINE = 0.50  # no vig in a pool


def wilson(w, n, conf=0.95):
    if n == 0:
        return (np.nan, np.nan, np.nan)
    z = stats.norm.ppf(1 - (1 - conf) / 2)
    p = w / n
    den = 1 + z**2 / n
    c = (p + z**2 / (2 * n)) / den
    h = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / den
    return (p, c - h, c + h)


def load():
    if os.path.exists(LOG):
        return pd.read_csv(LOG)
    return pd.DataFrame()


def report(df):
    print("=" * 70)
    print("POOL TRACKER")
    print("=" * 70)
    if df.empty:
        print("nothing logged yet")
        return

    g = df[df.result.isin(["W", "L", "P"])].copy()
    if g.empty:
        print(f"{len(df)} picks logged, none graded yet")
        return

    w = int((g.result == "W").sum())
    l = int((g.result == "L").sum())
    p = int((g.result == "P").sum())
    n = w + l
    rate, lo, hi = wilson(w, n)
    weeks = g.week.nunique()
    base_wins = weeks * PICKS_PER_WEEK * BASELINE

    print(f"\nweeks: {weeks}   picks graded: {len(g)}")
    print(f"record: {w}-{l}" + (f"-{p}" if p else ""))
    print(f"win rate: {rate*100:.1f}%   95% CI: [{lo*100:.1f}%, {hi*100:.1f}%]")
    print(f"wins: {w}   coin-flip baseline: {base_wins:.0f}   ({w-base_wins:+.0f})")
    if lo > BASELINE:
        print("  -> CI clears 50%. Ahead of a coin flip on this sample.")
    elif hi < BASELINE:
        print("  -> CI entirely below 50%. Behind a coin flip.")
    else:
        print("  -> CI straddles 50%. Not yet distinguishable from luck.")

    # ---- week by week ----
    print("\nWEEK BY WEEK")
    wk = (
        g.assign(win=(g.result == "W").astype(int))
        .groupby(["season", "week"])
        .agg(picks=("win", "size"), wins=("win", "sum"))
        .reset_index()
    )
    wk["vs_base"] = wk.wins - wk.picks * BASELINE
    for _, r in wk.iterrows():
        bar = "#" * int(r.wins)
        print(
            f"  {int(r.season)} wk{int(r.week):<3} {int(r.wins)}/{int(r.picks)}"
            f"  {r.vs_base:+.1f}   {bar}"
        )
    print(f"  {'TOTAL':<10} {w}/{len(g)}  {w-base_wins:+.1f}")

    # ---- CLV ----
    clv = g.dropna(subset=["closing_line", "my_line"]).copy()
    if not clv.empty:
        home = clv.game.str.split("@").str[1].str.strip()
        pteam = clv.pick.str.rsplit(" ", n=1).str[0].str.strip()
        is_home = pteam == home
        clv["clv"] = np.where(
            is_home, clv.my_line - clv.closing_line, clv.closing_line - clv.my_line
        ).round(2)
        print(f"\nCLOSING LINE VALUE  (n={len(clv)})")
        print(
            f"  beat {int((clv.clv>0).sum())} | tied {int((clv.clv==0).sum())} "
            f"| worse {int((clv.clv<0).sum())}"
        )
        print(f"  mean CLV: {clv.clv.mean():+.2f} pts")
        print("  (this converges far faster than W-L. Watch it, not the record.)")

    # ---- by value tier ----
    t = g[g.result.isin(["W", "L"])]
    if "tier" in t.columns and len(t) >= 5:
        print("\nBY VALUE TIER")
        for tier, sub in t.groupby("tier"):
            ww = int((sub.result == "W").sum())
            print(f"  {tier:<11} {ww}-{len(sub)-ww}   {ww/len(sub)*100:5.1f}%")

    if len(g) < 90:
        print(
            f"\nNOTE: {len(g)} graded picks. One season is ~90. Power to detect a "
            "3.7pt edge over one season is about 10%, so treat the record as "
            "provisional and judge on CLV."
        )

def _pool_rows(sub, mask, pick, result, value, rate, label):
    """Reshape archive rows into the pool_log layout so report() can score them."""
    d = sub[mask].copy()
    d["slot"] = d.groupby(["season", "week"]).cumcount() + 1
    out = pd.DataFrame(dict(
        season=d.season, week=d.week, slot=d.slot, game=d.game,
        pick=pick(d), my_line=d.pool_line, mkt_line=d.mkt_line_at_submit,
        closing_line=d.closing_line, pts_value=value(d), tier=d.tier,
        model_win_rate=rate(d), result=result(d), margin=d.margin,
    ))
    out.attrs["label"] = label
    return out


def from_submissions():
    """(your submitted picks, the model's card), both in pool_log layout."""
    if not os.path.exists(SUBMISSIONS):
        return pd.DataFrame(), pd.DataFrame()
    sub = pd.read_csv(SUBMISSIONS)
    sub["submitted"] = sub.submitted.astype(str).str.lower() == "true"
    sub["on_model_card"] = sub.on_model_card.astype(str).str.lower() == "true"
    mine = _pool_rows(
        sub, sub.submitted,
        pick=lambda d: d.sub_pick, result=lambda d: d.result,
        value=lambda d: d.sub_pts_value,
        # win_rate is the model's number for ITS side; blank it if you flipped
        rate=lambda d: d.win_rate.where(d.side == d.model_side),
        label="YOUR SUBMITTED PICKS",
    )
    model = _pool_rows(
        sub, sub.on_model_card,
        pick=lambda d: d.model_pick, result=lambda d: d.model_result,
        value=lambda d: d.pts_value_at_submit, rate=lambda d: d.win_rate,
        label="MODEL CARD",
    )
    return mine, model


def claude_picks(sub):
    """Claude's picks (data/claude_picks.csv) in pool_log layout.

    Only the team is recorded. Game, side, pool line and result come from the
    archive, so Claude's picks are graded against the same pool number, margin
    and closing line as yours and the model's.
    """
    if not os.path.exists(CLAUDE_PICKS):
        return pd.DataFrame()
    cp = pd.read_csv(CLAUDE_PICKS)
    rows = []
    for c in cp.itertuples():
        wk = sub[(sub.season == c.season) & (sub.week == c.week)]
        hit = wk[(wk.away == c.team) | (wk.home == c.team)]
        if hit.empty:
            print(f"WARNING: {c.team} not on the {c.season} wk{c.week} board "
                  f"in {SUBMISSIONS}")
            continue
        r = hit.iloc[0].copy()
        home = r.home == c.team
        line = r.pool_line if home else -r.pool_line
        if pd.notna(c.quoted_line) and abs(line - c.quoted_line) > 1e-9:
            print(f"NOTE: {c.team} wk{c.week}: Claude quoted {c.quoted_line:+g}, "
                  f"pool line is {line:+g}. Graded at the pool line.")
        res = ""
        if pd.notna(r.margin):
            cover = r.margin + r.pool_line   # >0 home covers, <0 away covers
            res = "P" if cover == 0 else ("W" if (cover > 0) == home else "L")
        rows.append(dict(
            season=r.season, week=r.week, game=r.game, pick=f"{c.team} {line:+g}",
            my_line=r.pool_line, mkt_line=r.mkt_line_at_submit,
            closing_line=r.closing_line, pts_value=np.nan, tier=r.tier,
            model_win_rate=np.nan, result=res, margin=r.margin,
        ))
    out = pd.DataFrame(rows)
    if out.empty:
        return out
    out["slot"] = out.groupby(["season", "week"]).cumcount() + 1
    out.attrs["label"] = "CLAUDE PICKS"
    return out


def add_claude(week, teams, season=2026):
    """Append Claude's picks for a week, replacing any already logged for it."""
    new = pd.DataFrame(dict(season=season, week=week, team=teams,
                            quoted_line=np.nan))
    if os.path.exists(CLAUDE_PICKS):
        old = pd.read_csv(CLAUDE_PICKS)
        old = old[~((old.season == season) & (old.week == week))]
        new = pd.concat([old, new], ignore_index=True)
    new.to_csv(CLAUDE_PICKS, index=False)
    print(f"logged {len(teams)} Claude picks for {season} wk{week}: {', '.join(teams)}")


def compare(sets):
    """Week-by-week wins for each pick set; totals over weeks graded for all."""
    def wins(d):
        g = d[d.result.isin(["W", "L", "P"])]
        return g.groupby(["season", "week"]).agg(
            w=("result", lambda r: int((r == "W").sum())), n=("result", "size"))
    tbl = {name: wins(d) for name, d in sets.items() if not d.empty}
    if len(tbl) < 2:
        return
    weeks = sorted(set().union(*[t.index for t in tbl.values()]))
    print("\n" + "=" * 70)
    print("COMPARISON  (wins / graded picks)")
    print("=" * 70)
    print(f"  {'':<11}" + "".join(f"{n:>10}" for n in tbl))
    for wk in weeks:
        cells = [f"{int(t.at[wk,'w'])}/{int(t.at[wk,'n'])}" if wk in t.index else "-"
                 for t in tbl.values()]
        print(f"  {int(wk[0])} wk{int(wk[1]):<3}  " + "".join(f"{c:>10}" for c in cells))
    common = [wk for wk in weeks if all(wk in t.index for t in tbl.values())]
    if common:
        tot = [f"{int(sum(t.at[wk,'w'] for wk in common))}/"
               f"{int(sum(t.at[wk,'n'] for wk in common))}" for t in tbl.values()]
        print(f"  {'COMMON':<11}" + "".join(f"{c:>10}" for c in tot))
        print(f"  (COMMON = the {len(common)} week(s) graded for every set above. "
              "Tiny sample: noise for now.)")


SPLIT_GAP = 10   # money% minus bets% (pts) that counts as "money > tickets"
PUBLIC_MAX = 35  # a side with at most this % of tickets is "the unpopular side"


def past_seasons():
    """Completed seasons with any signal data (hand-collected or Action Network)."""
    found = set()
    for f in os.listdir("data"):
        if f.startswith("signals_") and f.endswith(".csv"):
            found.add(int(f[8:12]))
    if os.path.exists(ACTION_SPLITS):
        found |= set(pd.read_csv(ACTION_SPLITS).season.astype(int))
    return sorted(s for s in found if s < CURRENT_SEASON)


def load_season_signals(season, against="pool"):
    """One past season's games with signals, in the layout signals() expects.

    against="pool":   grade at that season's pool line when
                      data/pool_<season>_merged.csv exists, else the market close.
    against="market": always grade at the nflverse closing line.

    `pool_line` is the grading line; `mkt_line_at_submit` is the closing line
    (so "line moved toward" measures open -> close). Returns (frame, source).

    Action Network (data/action_splits.csv) is the primary source for splits
    and opening lines, so every week uses the same book. Hand-collected values
    (data/signals_<season>.csv) fill in only where Action Network has nothing.
    Bets % and money % are always taken as a pair. Sharp sides come only from
    the hand-collected file, since Action Network has none.
    """
    from fetch_splits import canon
    keys = ["season", "week", "away", "home"]

    # nflverse: spread_line POSITIVE = home favored, result = home margin.
    # Flip to this model's convention (negative = home favored).
    nfl = pd.read_csv(NFLVERSE_GAMES, low_memory=False)
    nfl = nfl[(nfl.season == season) & (nfl.game_type == "REG")
              & nfl.result.notna() & nfl.spread_line.notna()]
    p = pd.DataFrame(dict(season=nfl.season, week=nfl.week,
                          away=nfl.away_team.map(canon), home=nfl.home_team.map(canon),
                          close=-nfl.spread_line, margin=nfl.result))
    p["pool_line"] = p.close
    p["mkt_line_at_submit"] = p.close
    source = "market close"

    pool_file = f"data/pool_{season}_merged.csv"
    if against == "pool" and os.path.exists(pool_file):
        pl = pd.read_csv(pool_file).rename(columns={"wk": "week"})
        pl = pd.DataFrame(dict(season=pl.season, week=pl.week, away=pl.away.map(canon),
                               home=pl.home.map(canon), _pool=-pl.pool_line))
        p = p.merge(pl, on=keys, how="inner")
        p["pool_line"] = p._pool
        source = "pool line"
    if p.empty:
        return p, source

    sig_file = f"data/signals_{season}.csv"
    s = pd.read_csv(sig_file) if os.path.exists(sig_file) else pd.DataFrame(columns=keys)
    s["away"], s["home"] = s.away.map(canon), s.home.map(canon)
    g = p.merge(s, on=keys, how="left")
    for c in ("open_line", "home_bets_pct", "home_money_pct"):
        if c not in g.columns:
            g[c] = np.nan

    if os.path.exists(ACTION_SPLITS):
        an = pd.read_csv(ACTION_SPLITS)
        g = g.merge(an[keys + ["an_open_line", "an_home_bets_pct", "an_home_money_pct"]],
                    on=keys, how="left")
        g["open_line"] = g.an_open_line.fillna(g.open_line)
        pair = g.an_home_bets_pct.notna() & g.an_home_money_pct.notna()
        g.loc[pair, "home_bets_pct"] = g.loc[pair, "an_home_bets_pct"]
        g.loc[pair, "home_money_pct"] = g.loc[pair, "an_home_money_pct"]
    return g, source


def signals(sub, title="MARKET SIGNALS"):
    """ATS record of each market signal, graded at the pool line, all games.

    Only games where the signal was recorded count, so early weeks with no
    splits simply drop out rather than dilute the numbers.
    """
    need = {"margin", "pool_line"}
    if not need <= set(sub.columns):
        return
    g = sub.dropna(subset=["margin", "pool_line"]).copy()
    if g.empty:
        return
    cover = g.margin + g.pool_line          # >0 home covers, <0 away covers
    home_res = np.where(cover > 0, "W", np.where(cover < 0, "L", "P"))
    flip = {"W": "L", "L": "W", "P": "P"}

    def col(c):
        return g[c] if c in g.columns else pd.Series(np.nan, index=g.index)

    bets, money = col("home_bets_pct"), col("home_money_pct")
    sharp = col("sharp_side").astype(str).str.strip().str.lower()
    move = g.mkt_line_at_submit - col("open_line")   # <0 = moved toward home

    sharp = sharp.where(sharp.isin(["home", "away"]))
    stype = col("sharp_type").astype(str).str.strip().str.lower()
    sides = {
        "sharp side": sharp,
        # book = a sportsbook reported sharp action; pro = one pro bettor's
        # pick or a single large bet (only past seasons carry this column)
        "  reported by books": sharp.where(stype == "book"),
        "  one pro / one big bet": sharp.where(stype == "pro"),
        f"money > tickets by {SPLIT_GAP}+": pd.Series(np.select(
            [money - bets >= SPLIT_GAP, (100 - money) - (100 - bets) >= SPLIT_GAP],
            ["home", "away"], None), index=g.index),
        f"<= {PUBLIC_MAX}% of tickets": pd.Series(np.select(
            [bets <= PUBLIC_MAX, 100 - bets <= PUBLIC_MAX],
            ["home", "away"], None), index=g.index),
        "line moved toward": pd.Series(np.select(
            [move < 0, move > 0], ["home", "away"], None), index=g.index),
    }

    rows = []
    for name, side in sides.items():
        side = side.dropna()
        if side.empty:
            continue
        res = [home_res[g.index.get_loc(i)] if s == "home"
               else flip[home_res[g.index.get_loc(i)]] for i, s in side.items()]
        w, l = res.count("W"), res.count("L")
        rows.append((name, w, l, res.count("P")))
    if not rows:
        return

    print("\n" + "=" * 70)
    print(title + ("" if "(vs " in title else "  (vs pool line)"))
    print("every graded game with the signal on record")
    print("=" * 70)
    for name, w, l, p in rows:
        rate, lo, hi = wilson(w, w + l)
        print(f"  {name:<24} {w}-{l}" + (f"-{p}" if p else "") +
              f"   {rate*100:5.1f}%   CI [{lo*100:.0f}%, {hi*100:.0f}%]")

    # did YOUR picks do better with the sharps or against them?
    you = g[(g.get("submitted").astype(str).str.lower() == "true")
            & sharp.isin(["home", "away"])] if "submitted" in g else g.iloc[:0]
    if not you.empty:
        print("\n  your picks vs the sharp side:")
        for lbl, m in (("with sharps", you.side == sharp[you.index]),
                       ("against sharps", you.side != sharp[you.index])):
            r = you[m].result
            print(f"    {lbl:<15} {int((r=='W').sum())}-{int((r=='L').sum())}")
    print("  (signals only count from the first week you recorded them.)")


ELWAY_BUCKETS = [0.01, 0.5, 1.0, 2.0, 3.0, 99]
ELWAY_LABELS = ["0.5", "1", "1.5-2", "2.5-3", "3.5+"]


def elway_picks(sub, line_col):
    """One ELWAY 'pick' per game where its projection differs from `line_col`:
    the side the projection says that line undervalues. Graded at that line."""
    need = {"elway_line", "margin", line_col}
    if not need <= set(sub.columns):
        return pd.DataFrame()
    g = sub.dropna(subset=list(need)).copy()
    g["gap"] = (g.elway_line - g[line_col]).abs()
    g = g[g.gap > 0]
    if g.empty:
        return g
    side = np.where(g.elway_line < g[line_col], "home", "away")
    cover = g.margin + g[line_col]
    if "result" in g:                   # the log's own column = YOUR result
        g["result_sub"] = g["result"]
    g["result"] = np.where(cover == 0, "P",
                           np.where((cover > 0) == (side == "home"), "W", "L"))
    g["elway_side"] = side
    g["bucket"] = pd.cut(g.gap, ELWAY_BUCKETS, labels=ELWAY_LABELS)
    return g


def elway_report(sub):
    """ELWAY's record when it disagrees with your line (and with the close),
    by size of disagreement, plus how your picks did with/without it."""
    mine = elway_picks(sub, "pool_line")
    if mine.empty:
        return
    print("\n" + "=" * 70)
    print("ELWAY  (takes the side its projection says the line undervalues)")
    print("=" * 70)

    def table(g, label):
        print(f"\n  vs {label}")
        print(f"    {'gap':<7}{'record':>10}{'win %':>9}")
        for b in ELWAY_LABELS:
            r = g[g.bucket == b].result
            w, l, p = (r == "W").sum(), (r == "L").sum(), (r == "P").sum()
            if w + l + p:
                print(f"    {b:<7}{f'{w}-{l}' + (f'-{p}' if p else ''):>10}"
                      f"{w / (w + l) * 100 if w + l else 0:>8.0f}%")
        w, l = (g.result == "W").sum(), (g.result == "L").sum()
        rate, lo, hi = wilson(w, w + l)
        print(f"    {'ALL':<7}{f'{w}-{l}':>10}{rate * 100:>8.1f}%   CI [{lo*100:.0f}%, {hi*100:.0f}%]")
        big = g[g.gap >= 1.0]
        bw, bl = (big.result == "W").sum(), (big.result == "L").sum()
        if bw + bl:
            print(f"    {'gap 1+':<7}{f'{bw}-{bl}':>10}{bw / (bw + bl) * 100:>8.1f}%")

    table(mine, "your line (pool)")
    close = elway_picks(sub, "closing_line")
    if not close.empty:
        table(close, "closing line (the harder test)")

    print("\n  by week (vs your line):")
    for (s, wk), g in mine.groupby(["season", "week"]):
        w, l = (g.result == "W").sum(), (g.result == "L").sum()
        print(f"    {int(s)} wk{int(wk):<3} {w}-{l}")

    # your submitted picks: with ELWAY or against it
    you = mine[mine.get("submitted").astype(str).str.lower() == "true"] \
        if "submitted" in mine else mine.iloc[:0]
    if not you.empty:
        print("\n  your picks, where ELWAY had a side:")
        for lbl, m in (("with ELWAY", you.side == you.elway_side),
                       ("against ELWAY", you.side != you.elway_side)):
            r = you[m].result_sub
            print(f"    {lbl:<14} {int((r == 'W').sum())}-{int((r == 'L').sum())}")
    print("  (small samples: judge ELWAY on the gap 1+ row over many weeks)")


def seed():
    """Week 3 2026: the 5 the pool engine would have submitted."""
    rows = [
        # slot, game, pick, my_line(home persp), mkt, close, val, tier, rate, result, margin
        (1, "TEN @ NYG", "TEN +3.5", -3.5, -2.5, -2.5, 1.0, "EDGE", 0.561, "L", -5),
        (2, "BAL @ DAL", "BAL -2.5",  2.5,  3.5,  3.5, 1.0, "EDGE", 0.545, "W", 3),
        (3, "NE @ JAX",  "NE +3.5",  -3.5, -3.0, -3.0, 0.5, "slim", 0.534, "L", -29),
        (4, "LV @ NO",   "LV +3.5",  -3.5, -3.0, -3.0, 0.5, "slim", 0.534, "W", 8),
        (5, "KC @ MIA",  "MIA +11.5",11.5, 10.5, 10.5, 1.0, "EDGE", 0.533, "L", -14),
    ]
    return pd.DataFrame([
        dict(season=2026, week=3, slot=s, game=gm, pick=pk, my_line=my,
             mkt_line=mk, closing_line=cl, pts_value=v, tier=ti,
             model_win_rate=r, result=res, margin=mg)
        for s, gm, pk, my, mk, cl, v, ti, r, res, mg in rows
    ])


if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else "report"
    if cmd == "report":
        mine, model = from_submissions()
        if mine.empty and model.empty:
            raise SystemExit("no data/pool_picks_log.csv yet -- run log_week.py submit/grade")
        sets = {"you": mine, "model": model}
        if os.path.exists(SUBMISSIONS):
            cl = claude_picks(pd.read_csv(SUBMISSIONS))
            if not cl.empty:
                sets["claude"] = cl
        for d in sets.values():
            if d.empty:
                continue
            print(f"\n########  {d.attrs['label']}  ########")
            report(d)
        compare(sets)
        if os.path.exists(SUBMISSIONS):
            elway_report(pd.read_csv(SUBMISSIONS))
            signals(pd.read_csv(SUBMISSIONS), "MARKET SIGNALS 2026")
        # each past season at its own grading line (pool sheet if we have one)
        for season in past_seasons():
            g, src = load_season_signals(season)
            signals(g, f"MARKET SIGNALS {season} (vs {src})")
        # and all past seasons on one common footing: the market close
        frames = [load_season_signals(s, against="market")[0] for s in past_seasons()]
        frames = [f for f in frames if not f.empty]
        if len(frames) > 1:
            yrs = f"{past_seasons()[0]}-{past_seasons()[-1]}"
            signals(pd.concat(frames, ignore_index=True),
                    f"MARKET SIGNALS {yrs} COMBINED (vs market close)")
        raise SystemExit
    if cmd == "claude":
        add_claude(int(sys.argv[2]), [t.strip().upper() for t in sys.argv[3].split(",")])
        raise SystemExit
    df = load()
    if cmd == "seed":
        os.makedirs("data", exist_ok=True)
        df = pd.concat([df, seed()], ignore_index=True) if not df.empty else seed()
        df.to_csv(LOG, index=False)
        print(f"seeded {len(seed())} picks\n")
    report(df)
