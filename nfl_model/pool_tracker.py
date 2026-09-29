"""
Pool tracker: weekly 5-pick record, season standings, CLV, calibration.

    python3 pool_tracker.py report   <- your submitted picks vs the model's card,
                                        read from data/pool_picks_log.csv
                                        (written by log_week.py submit/grade)
    python3 pool_tracker.py claude 4 MIN,CLE,NYJ,TEN,IND
                                     <- record Claude's 5 for a week (team codes);
                                        graded like the others once you run
                                        log_week.py grade
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
