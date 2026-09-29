"""
Archive the week's inputs at submission time, then grade them later.

The point: `mkt_line` is the model's only real input, and it is a moving
target. Saving it with a timestamp turns "how much of the gap was
actually available to me?" from an unanswerable question into a measured
one, at the cost of nothing extra -- these are the numbers you already
type in to make picks.

    python3 log_week.py submit 4      <- archive this week's card + inputs
    python3 log_week.py grade 4       <- fill in closing lines + results
    python3 log_week.py clv           <- how well did your submission-time
                                         lines track the close?

Closing lines come from nflverse for free after the fact, so you never
have to record those by hand.
"""

import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd

from pool import best_five, score_game

ARCHIVE = "data/submissions.csv"
NFLVERSE = (
    "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"
)
TEAM_FIX = {"LAR": "LA", "WSH": "WAS"}


def submit(games, season, week, note=""):
    """Archive every game's inputs plus which 5 were picked."""
    res = best_five(games)
    picked = {r["game"] for r in res["card"]}
    stamp = datetime.now(timezone.utc).isoformat(timespec="seconds")

    rows = []
    for g in games:
        s = score_game(g)
        rows.append(dict(
            season=season, week=week,
            submitted_at=stamp,
            game=s["game"], away=g["away"], home=g["home"],
            pool_line=s["my_line"],
            mkt_line_at_submit=s["mkt_line"],     # <- the number that matters
            closing_line=np.nan,                  # filled by grade()
            pick=s["pick"], side=s["side"],
            pts_value_at_submit=s["pts_value"],
            win_rate=s["win_rate"], tier=s["tier"],
            on_card=s["game"] in picked,
            result="", margin=np.nan, note=note,
        ))
    df = pd.DataFrame(rows)

    os.makedirs("data", exist_ok=True)
    if os.path.exists(ARCHIVE):
        old = pd.read_csv(ARCHIVE)
        old = old[~((old.season == season) & (old.week == week))]
        df = pd.concat([old, df], ignore_index=True)
    df.to_csv(ARCHIVE, index=False)

    print(f"archived {len(rows)} games for {season} wk{week} at {stamp}")
    print(f"card: {', '.join(r['pick'] for r in res['card'])}")
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
    for col in ("result", "note"):
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
        side = df.at[i, "side"]
        won = (cover > 0) if side == "home" else (cover < 0)
        df.at[i, "result"] = "P" if cover == 0 else ("W" if won else "L")
        df.at[i, "margin"] = result
        filled += 1

    df.to_csv(ARCHIVE, index=False)
    card = df[mask & df.on_card]
    w = int((card.result == "W").sum())
    l = int((card.result == "L").sum())
    print(f"graded {filled} games. card went {w}-{l}")


def clv():
    """Did your submission-time line track the close, or drift from it?"""
    if not os.path.exists(ARCHIVE):
        print("nothing archived yet")
        return
    df = pd.read_csv(ARCHIVE).dropna(subset=["closing_line"])
    if df.empty:
        print("no graded weeks yet")
        return

    # how far the market moved after you locked
    df["move"] = df.closing_line - df.mkt_line_at_submit
    # value you thought you had vs what you actually had at the close
    df["val_at_close"] = np.where(
        df.side == "home",
        df.closing_line - df.pool_line,
        df.pool_line - df.closing_line,
    )
    df["val_lost"] = df.val_at_close - df.pts_value_at_submit

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
    if df.val_lost.mean() > 0.1:
        print("  -> the market moved TOWARD your sheet. Submitting earlier")
        print("     would have shown less value than you actually got.")
    elif df.val_lost.mean() < -0.1:
        print("  -> the market moved AWAY from your sheet. Some of the value")
        print("     you saw at submission evaporated before kickoff.")
    else:
        print("  -> essentially no drift. Submission timing is not costing you.")

    card = df[df.on_card]
    if len(card) >= 5:
        print(f"\non-card only (n={len(card)}): drift {card.val_lost.mean():+.2f} pts")


if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else "clv"
    if cmd == "submit":
        from sample_week import games
        wk = int(sys.argv[2]) if len(sys.argv) > 2 else 0
        submit(games, 2026, wk)
    elif cmd == "grade":
        grade(2026, int(sys.argv[2]))
    else:
        clv()
