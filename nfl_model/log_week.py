"""
Archive the week's inputs at submission time, then grade them later.

The point: `mkt_line` is the model's only real input, and it is a moving
target. Saving it with a timestamp turns "how much of the gap was
actually available to me?" from an unanswerable question into a measured
one, at the cost of nothing extra -- these are the numbers you already
type in to make picks.

    python3 log_week.py submit 4                     <- take the model card
    python3 log_week.py submit 4 --picks TEN,BAL,MIA,PIT,DEN
                                     <- submit your own 5 instead; name the
                                        TEAM you picked, which fixes the side
    python3 log_week.py submit 4 --picks ... --note "why"
    python3 log_week.py grade 4       <- fill in closing lines + results
    python3 log_week.py clv           <- how well did your submission-time
                                         lines track the close?
    python3 log_week.py overrides     <- did your overrides beat the model?

Closing lines come from nflverse for free after the fact, so you never
have to record those by hand.
"""

import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd

from margins import clv_prob
from pool import best_five, score_game
from sample_week import SIGNAL_COLS

ARCHIVE = "data/pool_picks_log.csv"
NFLVERSE = (
    "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"
)
TEAM_FIX = {"LAR": "LA", "WSH": "WAS"}


def submit(games, season, week, note="", picks=None):
    """
    Archive every game's inputs, what the model wanted, and what you
    actually submitted.

    picks: list of team codes you really sent, e.g. ["TEN","BAL","MIA",
           "PIT","DEN"]. Naming a team fixes both the game and the side,
           so you can take the opposite side from the model. Omit it and
           your submission is recorded as the model's card.

    Both are stored. That keeps the record honest and, after enough
    weeks, answers whether your overrides actually beat the model.
    """
    res = best_five(games)
    model_card = {r["game"] for r in res["card"]}
    stamp = datetime.now(timezone.utc).isoformat(timespec="seconds")

    # map a team code -> (game label, which side that team is)
    team_to_game = {}
    for g in games:
        lbl = f"{g['away']} @ {g['home']}"
        team_to_game[g["away"].upper()] = (lbl, "away")
        team_to_game[g["home"].upper()] = (lbl, "home")

    sub_side = {}          # game label -> side you submitted
    if picks:
        unknown = [p for p in picks if p.upper() not in team_to_game]
        if unknown:
            raise SystemExit(
                f"not on this week's board: {', '.join(unknown)}\n"
                f"teams available: {', '.join(sorted(team_to_game))}"
            )
        for p in picks:
            lbl, side = team_to_game[p.upper()]
            if lbl in sub_side:
                raise SystemExit(f"two picks in the same game: {lbl}")
            sub_side[lbl] = side
        if len(sub_side) != 5:
            print(f"WARNING: {len(sub_side)} picks submitted, not 5")
    else:
        sub_side = {r["game"]: r["side"] for r in res["card"]}

    rows = []
    for g in games:
        s = score_game(g)
        lbl = s["game"]
        submitted = lbl in sub_side
        my_side = sub_side.get(lbl, "")
        # the number you get on the side YOU took
        if my_side:
            team = g["home"] if my_side == "home" else g["away"]
            num = s["my_line"] if my_side == "home" else -s["my_line"]
            sub_pick = f"{team} {num:+g}"
            sub_val = (s["my_line"] - s["mkt_line"]) if my_side == "home" \
                else (s["mkt_line"] - s["my_line"])
        else:
            sub_pick, sub_val = "", np.nan

        rows.append(dict(
            season=season, week=week,
            submitted_at=stamp,
            game=lbl, away=g["away"], home=g["home"],
            pool_line=s["my_line"],
            mkt_line_at_submit=s["mkt_line"],     # <- the number that matters
            closing_line=np.nan,                  # filled by grade()
            # what the model wanted
            model_pick=s["pick"], model_side=s["side"],
            on_model_card=lbl in model_card,
            pts_value_at_submit=s["pts_value"],
            win_rate=s["win_rate"], tier=s["tier"],
            # what you actually sent
            submitted=submitted, sub_pick=sub_pick, side=my_side,
            sub_pts_value=round(sub_val, 2) if my_side else np.nan,
            result="", model_result="", margin=np.nan, note=note,
            # market signals at submit time (blank if not entered)
            **{c: g.get(c, np.nan) for c in SIGNAL_COLS},
        ))
    df = pd.DataFrame(rows)

    os.makedirs("data", exist_ok=True)
    if os.path.exists(ARCHIVE):
        old = pd.read_csv(ARCHIVE)
        old = old[~((old.season == season) & (old.week == week))]
        df = pd.concat([old, df], ignore_index=True)
    df.to_csv(ARCHIVE, index=False)

    print(f"archived {len(rows)} games for {season} wk{week} at {stamp}")
    print(f"model card: {', '.join(r['pick'] for r in res['card'])}")

    sub_rows = [r for r in rows if r["submitted"]]
    if picks:
        print(f"you submitted: {', '.join(r['sub_pick'] for r in sub_rows)}")
        added = [r for r in sub_rows if not r["on_model_card"]]
        dropped = [r for r in rows if r["on_model_card"] and not r["submitted"]]
        flipped = [r for r in sub_rows
                   if r["on_model_card"] and r["side"] != r["model_side"]]
        if added or dropped or flipped:
            print("\noverrides:")
            for r in dropped:
                print(f"  dropped  {r['model_pick']:<13} ({r['win_rate']*100:.1f}%)")
            for r in added:
                print(f"  added    {r['sub_pick']:<13} "
                      f"(model had this at {r['pts_value_at_submit']:+.1f} pts of value)")
            for r in flipped:
                print(f"  flipped  {r['model_pick']} -> {r['sub_pick']}")
            # EV cost of the override, in the model's own terms
            model_exp = res["expected_wins"]
            sub_exp = 0.0
            for r in sub_rows:
                wr = r["win_rate"] if r["side"] == r["model_side"] else 1 - r["win_rate"]
                sub_exp += wr
            print(f"\n  model expected wins : {model_exp:.2f}")
            print(f"  your expected wins  : {sub_exp:.2f}   ({sub_exp-model_exp:+.2f})")
            if sub_exp < model_exp - 0.15:
                print("  -> the model rates your card meaningfully lower.")
                print("     Fine if you know something it doesn't; both are logged.")
        else:
            print("(identical to the model card)")
    else:
        print(f"expected wins {res['expected_wins']:.2f} of 5")
    return res


def grade(season, week):
    """Pull closing lines and results from nflverse, fill them in."""
    if not os.path.exists(ARCHIVE):
        print("nothing archived yet")
        return
    df = pd.read_csv(ARCHIVE)
    mask = (df.season == season) & (df.week == week)
    if not mask.any():
        print(f"no archive for {season} wk{week}")
        return

    # An all-empty 'result' column reloads from CSV as float64. pandas 3.0
    # no longer silently upcasts, so writing "W"/"L" into it raises.
    # Force the text columns to object before filling them.
    for col in ("result", "model_result", "note"):
        if col in df.columns:
            df[col] = df[col].astype("object")

    nfl = pd.read_csv(NFLVERSE, low_memory=False)
    nfl = nfl[(nfl.season == season) & (nfl.week == week)]
    nfl = nfl.dropna(subset=["result"])
    if nfl.empty:
        print(f"{season} wk{week} has no results posted yet")
        return

    key = {}
    for _, g in nfl.iterrows():
        a = TEAM_FIX.get(g.away_team, g.away_team)
        h = TEAM_FIX.get(g.home_team, g.home_team)
        key[(a, h)] = (g.spread_line, g.result)

    filled = 0
    for i in df[mask].index:
        a = TEAM_FIX.get(df.at[i, "away"], df.at[i, "away"])
        h = TEAM_FIX.get(df.at[i, "home"], df.at[i, "home"])
        if (a, h) not in key:
            continue
        close_nfl, result = key[(a, h)]
        # nflverse: spread_line POSITIVE = home favored.
        # this model:            NEGATIVE = home favored.
        # store the close in THIS model's convention so every line in the
        # archive is directly comparable.
        df.at[i, "closing_line"] = -close_nfl

        # grade against the POOL line, since that is what the pool settles on.
        # home covers when result > (-pool_line) in this convention.
        pool_line = df.at[i, "pool_line"]
        cover = result + pool_line
        df.at[i, "margin"] = result

        def grade_side(side):
            if not side:
                return ""
            if cover == 0:
                return "P"
            won = (cover > 0) if side == "home" else (cover < 0)
            return "W" if won else "L"

        df.at[i, "result"] = grade_side(df.at[i, "side"])
        df.at[i, "model_result"] = grade_side(df.at[i, "model_side"])
        filled += 1

    df.to_csv(ARCHIVE, index=False)

    sub = df[mask & (df.submitted == True)]                    # noqa: E712
    mod = df[mask & (df.on_model_card == True)]                # noqa: E712
    sw, sl = int((sub.result == "W").sum()), int((sub.result == "L").sum())
    mw, ml = int((mod.model_result == "W").sum()), int((mod.model_result == "L").sum())
    print(f"graded {filled} games.")
    print(f"  your card  : {sw}-{sl}")
    if (sw, sl) != (mw, ml) or not sub.index.equals(mod.index):
        print(f"  model card : {mw}-{ml}")


def clv():
    """Did your submission-time line track the close, or drift from it?"""
    if not os.path.exists(ARCHIVE):
        print("nothing archived yet")
        return
    df = pd.read_csv(ARCHIVE).dropna(subset=["closing_line"])
    if "submitted" in df.columns:
        df = df[df.submitted == True]                          # noqa: E712
    if df.empty:
        print("no graded weeks yet")
        return

    # how far the market moved after you locked
    df["move"] = df.closing_line - df.mkt_line_at_submit
    # value you thought you had vs what you actually had at the close
    # lines are home-perspective: a higher number = more points for home, so
    # home value = pool - close and away value = close - pool
    df["val_at_close"] = np.where(
        df.side == "home",
        df.pool_line - df.closing_line,
        df.closing_line - df.pool_line,
    )
    df["val_lost"] = df.val_at_close - df.pts_value_at_submit
    # the same in cover probability, so a half point through 3 or 7 counts
    # for what it's worth (margins.py)
    df["prob_at_submit"] = [clv_prob(l, s, m) for l, s, m in
                            zip(df.pool_line, df.side, df.mkt_line_at_submit)]
    df["prob_at_close"] = [clv_prob(l, s, c) for l, s, c in
                           zip(df.pool_line, df.side, df.closing_line)]

    print("=" * 66)
    print("SUBMISSION-TIME LINE vs CLOSE")
    print("=" * 66)
    print(f"games: {len(df)}   weeks: {df.week.nunique()}")
    print(f"\nmarket moved after you locked:")
    print(f"  mean |move|      : {df.move.abs().mean():.2f} pts")
    print(f"  moved not at all : {(df.move == 0).mean()*100:.1f}%")
    print(f"  moved >= 1 pt    : {(df.move.abs() >= 1).mean()*100:.1f}%")
    print(f"\nvalue you held, submit vs close:")
    print(f"  at submit : {df.pts_value_at_submit.mean():+.2f} pts")
    print(f"  at close  : {df.val_at_close.mean():+.2f} pts")
    print(f"  drift     : {df.val_lost.mean():+.2f} pts")
    print(f"\n  in cover probability (key numbers counted):")
    print(f"  at submit : {df.prob_at_submit.mean()*100:+.1f}%")
    print(f"  at close  : {df.prob_at_close.mean()*100:+.1f}%")
    print(f"  drift     : {(df.prob_at_close - df.prob_at_submit).mean()*100:+.1f}%")
    if df.val_lost.mean() > 0.1:
        print("  -> the market moved TOWARD your sheet. Submitting earlier")
        print("     would have shown less value than you actually got.")
    elif df.val_lost.mean() < -0.1:
        print("  -> the market moved AWAY from your sheet. Some of the value")
        print("     you saw at submission evaporated before kickoff.")
    else:
        print("  -> essentially no drift. Submission timing is not costing you.")

    print(f"\n(submitted picks only, n={len(df)})")


def overrides():
    """Do your overrides beat the model, or just feel better?"""
    if not os.path.exists(ARCHIVE):
        print("nothing archived yet")
        return
    df = pd.read_csv(ARCHIVE)
    df = df[df.result.isin(["W", "L"]) | df.model_result.isin(["W", "L"])]
    if df.empty:
        print("no graded weeks yet")
        return

    sub = df[df.submitted == True]                             # noqa: E712
    mod = df[df.on_model_card == True]                         # noqa: E712

    def rec(d, col):
        w = int((d[col] == "W").sum())
        l = int((d[col] == "L").sum())
        return w, l, (w / (w + l) if w + l else float("nan"))

    sw, sl, sr = rec(sub, "result")
    mw, ml, mr = rec(mod, "model_result")

    print("=" * 62)
    print("YOUR CARD vs THE MODEL'S CARD")
    print("=" * 62)
    print(f"weeks graded: {df.week.nunique()}")
    print(f"  your picks  : {sw}-{sl}   {sr*100:.1f}%")
    print(f"  model picks : {mw}-{ml}   {mr*100:.1f}%")
    print(f"  difference  : {sw-mw:+d} wins")

    # only the picks where the two actually differ
    diff_add = sub[sub.on_model_card == False]                 # noqa: E712
    diff_drop = mod[mod.submitted == False]                    # noqa: E712
    flip = sub[(sub.on_model_card == True) & (sub.side != sub.model_side)]

    n_diff = len(diff_add) + len(flip)
    if n_diff == 0:
        print("\nno overrides logged yet.")
        return

    aw, al, _ = rec(diff_add, "result")
    dw, dl, _ = rec(diff_drop, "model_result")
    fw, fl, _ = rec(flip, "result")

    print(f"\nwhere you differed ({n_diff} picks):")
    print(f"  games you ADDED     : {aw}-{al}")
    print(f"  games you DROPPED   : {dw}-{dl}  (what the model would have got)")
    if len(flip):
        print(f"  sides you FLIPPED   : {fw}-{fl}")
    net = (aw + fw) - (dw + fw if len(flip) else dw)
    print(f"  net from overriding : {(aw+fw)-(dw):+d} wins")

    total_ov = aw + al + fw + fl
    if total_ov < 25:
        print(f"\n{total_ov} override picks so far. Needs ~100 before this")
        print("says anything. Until then it is a log, not a verdict.")


if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else "clv"
    if cmd == "submit":
        from sample_week import load_games
        wk = int(sys.argv[2]) if len(sys.argv) > 2 else 0
        games = load_games(wk or None)
        picks = None
        if "--picks" in sys.argv:
            raw = sys.argv[sys.argv.index("--picks") + 1]
            picks = [t.strip() for t in raw.replace(" ", ",").split(",") if t.strip()]
        note = ""
        if "--note" in sys.argv:
            note = sys.argv[sys.argv.index("--note") + 1]
        submit(games, 2026, wk, note=note, picks=picks)
    elif cmd == "grade":
        grade(2026, int(sys.argv[2]))
    elif cmd in ("overrides", "override"):
        overrides()
    else:
        clv()
