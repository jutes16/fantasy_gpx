"""
Pool field: every entrant's 5 picks a week, graded against the pool's lines.

    python3 pool_field.py              <- season standings, weekly points, the
                                          field's consensus picks, and any week
                                          where the graded points differ from
                                          the sheet's
    python3 pool_field.py add 8 "<name on the sheet>" CLE,CIN,ATL,NYG,LV
                                       <- add (or replace) one entrant's week

Picks live in data/pool_field_picks.csv (git-ignored: other people's names):
    week, player, picks (A-B-C-D-E), sheet_points, picks_left
sheet_points / picks_left are what the pool's sheet showed when the picks were
copied (picks_left > 0 means a game hadn't finished, so the sheet was partial).
Grading uses pool_line and margin from data/pool_picks_log.csv (log_week.py
grade), the same way as your own picks: cover = side * (margin + pool_line).
"""

import os
import sys
import pandas as pd

FIELD = "data/pool_field_picks.csv"
LOG = "data/pool_picks_log.csv"
SEASON = 2026
ME = "John Gilbert"


def games(season=SEASON):
    log = pd.read_csv(LOG)
    log = log[log.season == season]
    out = {}
    for r in log.itertuples():
        for team, s in ((r.home, 1), (r.away, -1)):
            out[(r.week, team)] = (r.game, s, r.pool_line, r.margin)
    return out


def graded(field=None):
    """One row per entrant x week x pick, with W/L/P (or open)."""
    field = pd.read_csv(FIELD) if field is None else field
    g, rows = games(), []
    for r in field.itertuples():
        for t in r.picks.split("-"):
            game, s, line, m = g.get((r.week, t), (None, None, None, None))
            if game is None:
                res = "?"
            elif pd.isna(m):
                res = "open"
            else:
                c = s * (m + line)
                res = "W" if c > 0 else ("P" if c == 0 else "L")
            rows.append(dict(week=r.week, player=r.player, team=t, game=game, pool_line=line, result=res))
    return pd.DataFrame(rows)


def points(res):
    return res.map({"W": 1.0, "P": 0.5}).fillna(0.0)


def report():
    field = pd.read_csv(FIELD)
    p = graded(field)
    unknown = p[p.result == "?"]
    if len(unknown):
        print("Picks not on the pool board (check the team code):")
        print(unknown[["week", "player", "team"]].to_string(index=False), "\n")
    p["pts"] = points(p.result)
    done = p[p.result.isin(["W", "L", "P"])]

    wk = p.pivot_table(index="player", columns="week", values="pts", aggfunc="sum").fillna(0)
    rec = done.groupby("player").result.value_counts().unstack(fill_value=0)
    for c in "WLP":
        rec[c] = rec.get(c, 0)
    s = pd.DataFrame({"pts": wk.sum(axis=1), "W": rec.W, "L": rec.L, "P": rec.P})
    s["cover%"] = (s.W + 0.5 * s.P) / (s.W + s.L + s.P) * 100
    s["best wk"] = wk.max(axis=1)
    s["4+ wks"] = (wk >= 4).sum(axis=1)
    s = s.sort_values(["pts", "W"], ascending=False)
    s.insert(0, "rank", s.pts.rank(ascending=False, method="min").astype(int))
    s = s.join(wk.rename(columns=lambda w: f"wk{w}"))
    print(f"Pool field, {SEASON}: {s.shape[0]} entrants, weeks {wk.columns.min()}-{wk.columns.max()} "
          f"(graded from the pool's lines; * = you)")
    show = s.copy()
    show.index = [f"{n} *" if n == ME else n for n in show.index]
    print(show.to_string(float_format=lambda x: f"{x:.0f}" if x == int(x) else f"{x:.1f}"))

    n = s.shape[0]
    print(f"\nField overall: {int(s.W.sum())}-{int(s.L.sum())}-{int(s.P.sum())} "
          f"({s.W.sum() / (s.W.sum() + s.L.sum()) * 100:.1f}%), avg {s.pts.mean():.1f} pts, "
          f"median {s.pts.median():.0f}")

    # Consensus: the teams most of the field took, and how they did
    c = done.groupby(["week", "team"]).agg(n=("player", "size"), result=("result", "first")).reset_index()
    pop = c[c.n >= n / 3]
    print(f"\nConsensus picks (taken by a third of the field or more): "
          f"{(pop.result == 'W').sum()}-{(pop.result == 'L').sum()}-{(pop.result == 'P').sum()}")
    print(pop.sort_values(["week", "n"], ascending=[True, False])
             .assign(share=lambda d: (d.n / n * 100).round().astype(int).astype(str) + "%")
             [["week", "team", "share", "result"]].to_string(index=False))

    # Where the graded points differ from what the sheet showed
    calc = p.groupby(["week", "player"]).pts.sum().rename("graded")
    chk = field.set_index(["week", "player"]).join(calc)
    off = chk[(chk.graded != chk.sheet_points) & (chk.picks_left == 0)]
    if len(off):
        print("\nGraded points differ from the sheet (sheet was final): check the picks or the pool's scoring")
        print(off[["picks", "sheet_points", "graded"]].to_string())


def add(week, player, picks):
    field = pd.read_csv(FIELD) if os.path.exists(FIELD) else pd.DataFrame(
        columns=["week", "player", "picks", "sheet_points", "picks_left"])
    picks = "-".join(t.strip().upper() for t in picks.replace("-", ",").split(","))
    if len(picks.split("-")) != 5:
        raise SystemExit("need 5 team codes")
    field = field[~((field.week == week) & (field.player == player))]
    field = pd.concat([field, pd.DataFrame([dict(week=week, player=player, picks=picks,
                                                 sheet_points=None, picks_left=None)])])
    field.sort_values(["week"], kind="stable").to_csv(FIELD, index=False)
    print(f"week {week}: {player} {picks}")


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "add":
        add(int(sys.argv[2]), sys.argv[3], sys.argv[4])
    else:
        report()
