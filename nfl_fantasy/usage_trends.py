"""
Target and touch trends for the season, from nflverse box scores: every
player's targets, carries and receptions each game, and snap counts.

  targets    a player's targets / his team's pass attempts that game
  touches    a running back's carries + receptions / his team's offensive snaps
  snaps      his offensive snaps / his team's (the most any of its players played)

Each player gets a recency-weighted share: weighted targets / weighted team
volume, with a week's weight halving every HALF_LIFE weeks back from the
latest week played (dividing sums, not averaging percentages, so a 50-play
game counts more than a 30-play one). Also:

  season     plain share over the games he played
  last3      share over his last three games
  trend      ewm share minus season share (rising usage is positive)
  slope      least-squares change in weekly share per week
  games      games he played (an offensive snap or a touch) / his team's games
  weekly     his count each week: "." his team played without him,
             "-" his (current) team didn't play (bye, or he was traded)

Games he missed are left out of his shares, so an injury doesn't drag them
down; a game he played with 0 targets counts as 0.

  python3 usage_trends.py                # leaders, risers and fallers
  python3 usage_trends.py --half-life 2  # weight recent weeks harder
  python3 usage_trends.py --top 40

Box scores and snap counts are cached under data/nflverse/ and re-fetched
after 12 hours. All games go to one long table, data/usage/<season>_weekly.csv
(the dashboard reads it), and the trend tables to
data/usage/<season>_targets_trends.csv and _touches_trends.csv.
"""

import argparse
import os
import re
import sys
import time
import unicodedata
import urllib.request

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "nfl_bets"))
import prop_shapes                          # noqa: E402  (game_logs: downloads + caches)
from common import canon                    # noqa: E402
from weekly_stats import _normalize_name    # noqa: E402

SEASON = 2026
OUT = os.path.join(HERE, "data", "usage")
SNAPS_URL = "https://github.com/nflverse/nflverse-data/releases/download/snap_counts/snap_counts_{}.csv"
STALE = 12 * 3600               # seconds before the current season is re-fetched
HALF_LIFE = 3                   # weeks
HALF_PPR = 0.5                  # both Sleeper leagues score half-PPR
LAST_N = 3
SKILL = {"RB": "RB", "HB": "RB", "FB": "RB", "WR": "WR", "TE": "TE"}
SUFFIX = re.compile(r"\s+(jr|sr|ii|iii|iv|v)\.?$", re.I)


def key(name):
    """Name key that matches box scores to snap counts ("Kenneth Walker III" = "Kenneth Walker")."""
    name = unicodedata.normalize("NFKD", str(name)).encode("ascii", "ignore").decode()
    return _normalize_name(SUFFIX.sub("", name.strip()))


def match_nicknames(box, sn):
    """Box-score names the snap counts spell differently (Bam / Zonovan Knight): same team,
    week, position and last name, when exactly one unmatched snap row fits; the alias then
    holds for all his weeks on that team. Rewrites sn.k."""
    j = ["team", "week", "pos", "last"]
    b = box[~box.set_index(["k", "team", "week"]).index.isin(sn.set_index(["k", "team", "week"]).index)].copy()
    o = sn[~sn.set_index(["k", "team", "week"]).index.isin(box.set_index(["k", "team", "week"]).index)].copy()
    b["last"] = b.player_display_name.map(lambda n: key(SUFFIX.sub("", n).split()[-1]))
    o["last"] = o.player.map(lambda n: key(SUFFIX.sub("", n).split()[-1]))
    o = o[~o.duplicated(j, keep=False)]
    m = o.reset_index().merge(b[j + ["k"]].drop_duplicates(j), on=j, suffixes=("", "_box"))
    alias = {(r.team, r.k): r.k_box for r in m.itertuples()}          # every week, once seen in one
    sn["k"] = [alias.get((t, k), k) for t, k in zip(sn.team, sn.k)]
    left = b[~b.set_index(j).index.isin(m.set_index(j).index) & ((b.targets > 0) | (b.carries > 0))]
    if len(left):
        print("no snap count for: " + ", ".join(sorted(set(left.player_display_name + " (" + left.team + ")"))), file=sys.stderr)
    return sn


def fresh(path, url):
    if os.path.exists(path) and time.time() - os.path.getmtime(path) > STALE:
        os.remove(path)
    if not os.path.exists(path):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        urllib.request.urlretrieve(url, path)
    return path


def load(season=SEASON):
    """Long table: kind, week, player, team, pos, n (targets/touches), team_n (attempts/snaps), snaps, team_snaps."""
    fresh(os.path.join(prop_shapes.NFLVERSE, f"stats_player_week_{season}.csv"), prop_shapes.URL.format(season))
    box = prop_shapes.game_logs((season,))
    box = box[box.player_display_name.notna()].copy()
    box["team"] = box.team.map(canon)
    for c in ("targets", "carries", "receptions", "attempts", "fantasy_points"):
        box[c] = box[c].fillna(0)
    box["fp"] = box.fantasy_points + HALF_PPR * box.receptions        # nflverse's is standard scoring
    pass_att = box.groupby(["team", "week"]).attempts.sum().rename("pass_att")

    sn = pd.read_csv(fresh(os.path.join(prop_shapes.NFLVERSE, f"snap_counts_{season}.csv"), SNAPS_URL.format(season)))
    sn = sn[sn.game_type == "REG"].copy()
    sn["team"] = sn.team.map(canon)
    team_snaps = sn.groupby(["team", "week"]).offense_snaps.max().rename("team_snaps")
    sn = sn[sn.position.isin(SKILL) & (sn.offense_snaps > 0)]

    box = box[box.position.isin(SKILL)]
    box = box.assign(k=box.player_display_name.map(key), pos=box.position.map(SKILL))
    sn = sn.assign(k=sn.player.map(key), pos=sn.position.map(SKILL))
    sn = match_nicknames(box, sn)
    d = box[["k", "player_display_name", "pos", "team", "week", "targets", "carries", "receptions", "fp"]].merge(
        sn[["k", "player", "pos", "team", "week", "offense_snaps"]], on=["k", "team", "week"], how="outer",
        suffixes=("", "_sn"))
    d["player"] = d.player_display_name.fillna(d.player)
    d["pos"] = d.pos.fillna(d.pos_sn)
    d = d.fillna({"targets": 0, "carries": 0, "receptions": 0, "fp": 0, "offense_snaps": 0})
    d = d.join(pass_att, on=["team", "week"]).join(team_snaps, on=["team", "week"])
    d["touches"] = d.carries + d.receptions
    d["opps"] = d.targets + np.where(d.pos == "RB", d.carries, 0)    # what points per opportunity is per
    d = d[(d.offense_snaps > 0) | (d.targets > 0) | (d.touches > 0)]
    # one name per player, his latest (box-score) spelling
    d["player"] = d.groupby("k").player.transform(lambda x: x.loc[d.loc[x.index, "week"].idxmax()])

    d["tgts"] = d.targets
    base = ["week", "player", "team", "pos", "offense_snaps", "team_snaps", "fp", "opps", "tgts"]
    tg = d[base + ["targets", "pass_att"]].rename(columns={"targets": "n", "pass_att": "team_n"})
    tc = d[d.pos == "RB"][base + ["touches", "team_snaps"]].copy()
    tc.columns = base + ["n", "team_n"]
    out = pd.concat([tg.assign(kind="targets"), tc.assign(kind="touches")], ignore_index=True)
    out = out.rename(columns={"offense_snaps": "snaps"})
    out = out[out.team_n > 0]
    return out[["kind", "week", "player", "team", "pos", "n", "team_n", "snaps", "team_snaps", "fp", "opps", "tgts"]].astype(
        {"n": int, "team_n": int, "snaps": int, "team_snaps": "Int64", "opps": int, "tgts": int})


def slope(weeks, shares):
    if len(weeks) < 3:
        return np.nan
    return np.polyfit(weeks, shares, 1)[0]


def trends(d, half_life=HALF_LIFE):
    """One row per player: recency-weighted share and the trend columns."""
    latest = d.week.max()
    team_weeks = d.groupby("team").week.agg(set)          # weeks each team played
    weeks = range(d.week.min(), latest + 1)
    out = []
    for player, g in d.sort_values("week").groupby("player"):
        w = 0.5 ** ((latest - g.week) / half_life)
        last, before = g.tail(LAST_N), g.iloc[:-LAST_N]
        team = g.team.iloc[-1]
        snap = lambda x: x.snaps.sum() / x.team_snaps.sum() if len(x) and x.team_snaps.sum() else np.nan
        share = g.n / g.team_n
        by_wk = dict(zip(g.week, g.n))
        out.append({
            "player": player, "pos": g.pos.iloc[-1], "team": team,
            "games": f"{len(g)}/{len(team_weeks.get(team, g.week))}",
            "avg": g.n.mean(), "ewm_avg": (w * g.n).sum() / w.sum(),
            "fp_avg": g.fp.mean(), "fp_last3": last.fp.mean(), "fp_ewm": (w * g.fp).sum() / w.sum(),
            "opps_ewm": (w * g.opps).sum() / w.sum(), "tgt_ewm": (w * g.tgts).sum() / w.sum(),
            "season": g.n.sum() / g.team_n.sum(),
            "last3": last.n.sum() / last.team_n.sum(),
            "before": before.n.sum() / before.team_n.sum() if len(before) else np.nan,
            "ewm": (w * g.n).sum() / (w * g.team_n).sum(),
            "snap_ewm": (w * g.snaps).sum() / (w * g.team_snaps.fillna(0)).sum() if g.team_snaps.notna().any() else np.nan,
            "snap_last3": snap(last), "snap_before": snap(before),
            "slope": slope(g.week.values, share.values),
            "last_wk": int(g.week.iloc[-1]),
            "weekly": " ".join(str(int(by_wk[k])) if k in by_wk
                               else ("." if k in team_weeks.get(team, ()) else "-") for k in weeks),
        })
    t = pd.DataFrame(out)
    t["trend"] = t["ewm"] - t["season"]
    return t.sort_values("ewm", ascending=False).reset_index(drop=True)


# ------------------------------------------------------------------ signals
MOVE = {"targets": 0.06, "touches": 0.08}    # last-3 share vs before: a real change in role
MIN_SHARE = 0.10                             # ...for a player whose role matters
OPENING = 0.07                               # share gained in the games a teammate missed
SIDELINED = {"Out", "IR", "PUP", "Sus", "Doubtful"}


def pct(x):
    return f"{100 * x:.0f}%"


def signals(d, t, kind, injury=None):
    """Role changes worth acting on, one row each: player, team, kind, signal, size, detail.

    rising / falling  share over his last 3 games vs his games before (4+ games)
    snaps_up          last-3 snap share 55%+, up 20+ points (an RB's under touches only)
    opening           a top-2 teammate is out now (Sleeper status, or missed his team's
                      latest game); his share in the games that teammate missed vs with him
    """
    injury = injury or {}
    out = []
    games = t.games.str.split("/").str[0].astype(int)
    for r in t[games >= LAST_N + 1].itertuples():
        delta = r.last3 - r.before
        if delta >= MOVE[kind] and r.last3 >= MIN_SHARE:
            out.append((r.player, r.team, kind, "rising", delta, f"{pct(r.before)} → {pct(r.last3)} over his last 3"))
        elif delta <= -MOVE[kind] and r.before >= MIN_SHARE + 0.02:
            out.append((r.player, r.team, kind, "falling", delta, f"{pct(r.before)} → {pct(r.last3)} over his last 3"))
        if (kind == "touches" or r.pos != "RB") and r.snap_last3 >= 0.55 and r.snap_last3 - r.snap_before >= 0.20:
            out.append((r.player, r.team, kind, "snaps_up", r.snap_last3 - r.snap_before,
                        f"snaps {pct(r.snap_before)} → {pct(r.snap_last3)}"))
    latest = d.groupby("team").week.max()
    for team, g in d.groupby("team"):
        tw = set(g.week)
        stars = t[(t.team == team) & (games >= 2)].nlargest(2, "season").player
        for star in stars:
            played = set(g[g.player == star].week)
            missed = tw - played
            out_now = injury.get((key(star), team)) in SIDELINED or latest[team] in missed
            if not missed or not out_now:
                continue
            for mate, m in g[g.player != star].groupby("player"):
                wo, w = m[m.week.isin(missed)], m[m.week.isin(played)]
                if not len(wo) or not len(w):
                    continue
                gain = wo.n.sum() / wo.team_n.sum() - w.n.sum() / w.team_n.sum()
                if gain >= OPENING:
                    wks = ", ".join(str(x) for x in sorted(wo.week))
                    out.append((mate, team, kind, "opening", gain,
                                f"{pct(w.n.sum() / w.team_n.sum())} with {star}, "
                                f"{pct(wo.n.sum() / wo.team_n.sum())} without him (wk {wks})", star))
    out = [r if len(r) == 7 else r + (None,) for r in out]
    cols = ["player", "team", "kind", "signal", "size", "detail", "because"]
    out = pd.DataFrame(out, columns=cols).sort_values("size", key=abs, ascending=False)
    return out.drop_duplicates(["player", "kind", "signal"])      # two teammates out: keep the bigger gain


def expected_points(d, t):
    """Adds x_fp (half-PPR points per game his usage is worth: his weighted targets times his
    position's points per target this season; an RB's targets and carries each at their own
    fitted rate) and fp_gap (actual minus that) to t."""
    games = d[d.kind == "targets"]
    g = games.groupby("pos")[["fp", "opps"]].sum()
    per_opp = (g.fp / g.opps).to_dict()
    rb = games[games.pos == "RB"]                  # a back's target is worth about twice a carry: fit both
    carries = rb.opps - rb.n
    per_opp["RB target"], per_opp["RB carry"] = np.linalg.lstsq(np.c_[rb.n, carries], rb.fp, rcond=None)[0]
    is_rb = t.pos == "RB"
    t["x_fp"] = np.where(is_rb, t.tgt_ewm * per_opp["RB target"] + (t.opps_ewm - t.tgt_ewm) * per_opp["RB carry"],
                         t.opps_ewm * t.pos.map(per_opp))
    t["fp_gap"] = t.fp_ewm - t.x_fp
    return {k: v for k, v in per_opp.items() if k != "RB"}


def point_signals(t):
    """buy_low   usage worth 8+ points a game, scoring at least 3 (and 25%) under it
    sell_high scoring 10+ a game, at least 4 (and 30%) more than his usage is worth
    3+ games. Size is the gap in points per game; mostly touchdowns, which follow usage."""
    games = t.games.str.split("/").str[0].astype(int)
    out = []
    for r in t[games >= 3].itertuples():
        detail = f"{r.fp_ewm:.1f} pts/g on usage worth {r.x_fp:.1f}"
        if r.x_fp >= 8 and r.fp_gap <= -max(3, 0.25 * r.x_fp):
            out.append((r.player, r.team, r.kind, "buy_low", r.fp_gap, detail))
        elif r.fp_ewm >= 10 and r.fp_gap >= max(4, 0.30 * r.x_fp):
            out.append((r.player, r.team, r.kind, "sell_high", r.fp_gap, detail))
    return pd.DataFrame(out, columns=["player", "team", "kind", "signal", "size", "detail"]).assign(because=None)


# ------------------------------------------------------------------ injuries
INJURY_URL = "https://github.com/nflverse/nflverse-data/releases/download/injuries/injuries_{}.csv"
PRACTICE = {"Did Not Participate In Practice": "DNP", "Limited Participation in Practice": "limited",
            "Full Participation in Practice": "full"}


def injury_reports(season=SEASON):
    """{(name key, team, week): (game status, injury, practice)} from the weekly NFL injury reports."""
    r = pd.read_csv(fresh(os.path.join(prop_shapes.NFLVERSE, f"injuries_{season}.csv"), INJURY_URL.format(season)))
    r = r[r.season_type == "REG"]
    out = {}
    for x in r.itertuples():
        hurt = x.report_primary_injury if isinstance(x.report_primary_injury, str) else x.practice_primary_injury
        hurt = "" if not isinstance(hurt, str) else "rest" if "resting" in hurt else "personal" if "personal" in hurt.lower() else hurt.lower()
        out[(key(x.full_name), canon(x.team), int(x.week))] = (
            x.report_status if isinstance(x.report_status, str) else "",
            hurt, PRACTICE.get(x.practice_status, ""))
    return out


def weeks_text(weeks):
    """[4, 5, 6, 9] -> "4-6, 9"."""
    runs, ws = [], sorted(weeks)
    for w in ws:
        if runs and w == runs[-1][1] + 1:
            runs[-1][1] = w
        else:
            runs.append([w, w])
    return ", ".join(f"{a}" if a == b else f"{a}-{b}" for a, b in runs)


def injury_columns(t, d, reports, injury, part):
    """Adds to t: injury (Sleeper's status now), injury_part, practice (this week's report so far),
    missed (games his team played without him, with the report's reason) and hurt (games he
    played while listed Questionable or Doubtful)."""
    team_weeks = d.groupby("team").week.agg(set)
    played = d.groupby("player").week.agg(set)
    nxt = d.week.max() + 1
    cols = {c: [] for c in ("injury", "injury_part", "practice", "missed", "hurt")}
    for r in t.itertuples():
        k = key(r.player)
        rep_ = lambda w: reports.get((k, r.team, w), ("", "", ""))
        cols["injury"].append(injury.get((k, r.team), ""))
        cols["injury_part"].append(part.get((k, r.team)) or rep_(nxt)[1] or "")
        cols["practice"].append(rep_(nxt)[2])
        missed = sorted(team_weeks.get(r.team, set()) - played.get(r.player, set()))
        why = {}
        for w in missed:
            why.setdefault(rep_(w)[1] or "inactive", []).append(w)
        cols["missed"].append("; ".join(f"wk {weeks_text(ws)} ({h})" for h, ws in why.items()))
        hurt = [w for w in sorted(played.get(r.player, ())) if rep_(w)[0] in ("Questionable", "Doubtful")]
        cols["hurt"].append(", ".join(f"wk {w} {rep_(w)[0][0]} ({rep_(w)[1]})" for w in hurt))
    for c, v in cols.items():
        t[c] = v
    return t


def injury_context(sig, d, tables, reports):
    """One line of injury context per signal: a teammate who returned (falling) or went out
    (rising, snaps up) during his last 3 games, injuries he played through, and his status now."""
    out = []
    for s in sig.itertuples():
        t = tables[s.kind]
        g = d[(d.kind == s.kind) & (d.team == s.team)]
        mine = sorted(g[g.player == s.player].week)
        recent, early = set(mine[-LAST_N:]), set(mine[:-LAST_N])
        mates = t[(t.team == s.team) & (t.player != s.player)].nlargest(3, "season")
        parts = []
        for m in mates.itertuples():
            mw = set(g[g.player == m.player].week)
            k = key(m.player)
            why = lambda ws: next((reports.get((k, s.team, w), ("", "", ""))[1] for w in sorted(ws)
                                   if reports.get((k, s.team, w), ("", "", ""))[1]), "")
            if s.signal == "falling" and (early - mw) and (recent & mw):
                back = min(recent & mw)
                parts.append(f"{m.player} back from {why(early - mw) or 'absence'} wk {back}")
            if s.signal in ("rising", "snaps_up", "buy_low") and (recent - mw) and (early & mw):
                gone = recent - mw
                back = sorted(w for w in mw if w > max(gone))
                parts.append(f"{m.player} out wk {weeks_text(gone)}" + (f" ({why(gone)})" if why(gone) else "")
                             + (f", back wk {back[0]}" if back else f", still {m.injury}" if m.injury else ""))
        if isinstance(s.because, str):
            b = t[t.player == s.because]
            if len(b) and b.injury.iloc[0]:
                parts.append(f"{s.because}: {b.injury.iloc[0]}" + (f" ({b.injury_part.iloc[0].lower()})" if b.injury_part.iloc[0] else ""))
        me = t[t.player == s.player]
        if len(me):
            me = me.iloc[0]
            if s.signal in ("falling", "buy_low") and me.hurt:
                parts.append(f"played hurt {me.hurt}")
            if me.injury:
                parts.append(f"he's {me.injury}" + (f" ({me.injury_part.lower()})" if me.injury_part else "")
                             + (f", {me.practice} in practice" if me.practice else ""))
        out.append("; ".join(parts))
    sig["context"] = out
    return sig


# ------------------------------------------------------------------ sleeper
def sleeper(username, season=SEASON):
    """(injury status and body part by (name key, team), roster table: league, player key, team,
    status mine/taken). Sleeper's status is the current one (this week's report once it's out)."""
    import sleeper_lineup as sl
    info = sl.players()
    hurt = [p for p in info.values() if p.get("team") and p.get("injury_status")]
    injury = {(key(p.get("full_name") or ""), canon(p.get("team"))): p.get("injury_status") for p in hurt}
    part = {(key(p.get("full_name") or ""), canon(p.get("team"))): p.get("injury_body_part") or "" for p in hurt}
    if not username:
        return injury, part, None
    user = sl.get(f"/user/{username}")
    if not user:
        raise SystemExit(f"no Sleeper user named {username!r}")
    rows = []
    for lg in sl.get(f"/user/{user['user_id']}/leagues/nfl/{season}") or []:
        for ro in sl.get(f"/league/{lg['league_id']}/rosters"):
            mine = ro.get("owner_id") == user["user_id"] or user["user_id"] in (ro.get("co_owners") or [])
            for pid in ro.get("players") or []:
                p = info.get(pid, {})
                if p.get("position") in SKILL:
                    rows.append((lg["name"], key(p.get("full_name") or ""), canon(p.get("team")),
                                 "mine" if mine else "taken"))
    return injury, part, pd.DataFrame(rows, columns=["league", "k", "team", "status"])


def show(t, kind, top):
    unit = "tgt" if kind == "targets" else "touch"
    fmt = t.copy()
    for c in ("season", "last3", "ewm", "trend", "slope"):
        fmt[c] = (fmt[c] * 100).round(1)
    fmt["avg"] = fmt.avg.round(1)
    fmt["ewm_avg"] = fmt.ewm_avg.round(1)
    fmt["snap_ewm"] = (fmt.snap_ewm * 100).round(0)
    fmt["fp_ewm"] = fmt.fp_ewm.round(1)
    cols = ["player", "pos", "team", "games", "avg", "ewm_avg", "season", "last3", "ewm", "trend", "slope", "snap_ewm", "fp_ewm", "weekly"]
    regulars = fmt[fmt.games.str.split("/").str[0].astype(int) >= 2]
    print(f"\n=== {kind}: recency-weighted share leaders (%, {unit}s/game) ===")
    print(fmt[cols].head(top).to_string(index=False))
    print(f"\n=== {kind}: risers (ewm share vs season, 2+ games) ===")
    print(regulars.sort_values("trend", ascending=False)[cols].head(top // 2).to_string(index=False))
    print(f"\n=== {kind}: fallers ===")
    print(regulars.sort_values("trend")[cols].head(top // 2).to_string(index=False))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--half-life", type=float, default=HALF_LIFE, help="weeks for a week's weight to halve")
    ap.add_argument("--top", type=int, default=30)
    ap.add_argument("--sleeper", metavar="USERNAME", help="tag players by your Sleeper leagues (yours / taken / free agent)")
    a = ap.parse_args()
    os.makedirs(OUT, exist_ok=True)
    d = load()
    d.assign(share=d.n / d.team_n).to_csv(os.path.join(OUT, f"{SEASON}_weekly.csv"), index=False, float_format="%.4f")
    pd.set_option("display.width", 200)
    injury, part, rosters = sleeper(a.sleeper)
    reports = injury_reports()
    sigs, names, main, tables = [], {}, [], {}
    for kind in ("targets", "touches"):
        dk = d[d.kind == kind]
        t = trends(dk, a.half_life)
        per_opp = expected_points(d, t)
        t = injury_columns(t, dk, reports, injury, part)
        tables[kind] = t
        path = os.path.join(OUT, f"{SEASON}_{kind}_trends.csv")
        t.to_csv(path, index=False, float_format="%.4f")
        print(f"\n{kind}: weeks {dk.week.min()}-{dk.week.max()}, {len(t)} players -> {os.path.relpath(path, HERE)}")
        show(t, kind, a.top)
        sigs.append(signals(dk, t, kind, injury))
        names.update({(key(p), tm): p for p, tm in zip(t.player, t.team)})
        main.append(t[(t.pos == "RB") == (kind == "touches")].assign(kind=kind))   # an RB under touches, the rest targets
    print("\nhalf-PPR points per opportunity: " + ", ".join(f"{k} {v:.2f}" for k, v in per_opp.items()))
    sigs.append(point_signals(pd.concat(main, ignore_index=True)))
    sig = injury_context(pd.concat(sigs, ignore_index=True), d, tables, reports)
    sig.to_csv(os.path.join(OUT, f"{SEASON}_signals.csv"), index=False, float_format="%.4f")
    print(f"\n=== signals ({len(sig)}; size: share points, or fantasy points a game for buy_low / sell_high) ===")
    pts = sig.signal.isin(["buy_low", "sell_high"])
    print(sig.drop(columns="because").assign(size=np.where(pts, sig["size"], 100 * sig["size"])).to_string(
        index=False, formatters={"size": lambda x: f"{x:+.1f}"}))
    if rosters is not None:
        rosters["player"] = [names.get((k, tm)) for k, tm in zip(rosters.k, rosters.team)]
        rosters = rosters[rosters.player.notna()][["league", "player", "team", "status"]]
        rosters.to_csv(os.path.join(OUT, f"{SEASON}_rosters.csv"), index=False)
        print(f"\nSleeper: {rosters.league.nunique()} league(s), {len(rosters)} rostered RB/WR/TE with usage")


if __name__ == "__main__":
    main()
