"""
Optimal starting lineup and free-agent upgrades for your Sleeper leagues.

    python3 sleeper_lineup.py <sleeper username>
    python3 sleeper_lineup.py <username> --week 5          # a specific week
    python3 sleeper_lineup.py <username> --league "Dynasty" # leagues whose name contains this

For every league you're in this season:
  1. Scores every player's Sleeper projection with THAT league's scoring
     settings (reception points, bonuses, kicker and defense tiers...).
  2. Finds the optimal starting lineup for the league's roster slots (FLEX,
     SUPER_FLEX, REC_FLEX... solved exactly as an assignment problem), skipping
     players who are Out / IR / suspended or on bye, and shows the changes from
     the lineup you have set now.
     Each starter shows Sleeper's projection AND the betting market's (player
     props saved by fantasy_props.py, market stats swapped into Sleeper's stat
     line, scored with the league's settings); Sleeper picks the lineup, and any
     swaps the market would make are listed, not applied.
  3. Free agents, two ways:
       this week  -- adding the player raises your optimal lineup's points
       rest of season -- the player projects for more points from this week
                         through week 18 than your weakest player at his position
Uses Sleeper's public API (no login). Projections and the player list are
cached under data/sleeper/ for the day.
"""

import json
import os
import sys
from datetime import date

import numpy as np
import pandas as pd
import requests
from scipy.optimize import linear_sum_assignment

HERE = os.path.dirname(os.path.abspath(__file__))
CACHE = os.path.join(HERE, "data", "sleeper")
API = "https://api.sleeper.app/v1"
LAST_WEEK = 18
NOT_PLAYING = {"Out", "IR", "PUP", "Sus", "NA", "DNR", "COV"}
SLOT_ELIGIBLE = {
    "QB": {"QB"}, "RB": {"RB"}, "WR": {"WR"}, "TE": {"TE"}, "K": {"K"}, "DEF": {"DEF"},
    "FLEX": {"RB", "WR", "TE"}, "WRRB_FLEX": {"WR", "RB"}, "REC_FLEX": {"WR", "TE"},
    "SUPER_FLEX": {"QB", "RB", "WR", "TE"},
    "DL": {"DL"}, "LB": {"LB"}, "DB": {"DB"}, "IDP_FLEX": {"DL", "LB", "DB"},
}
NON_STARTING = {"BN", "IR", "TAXI"}


# ------------------------------------------------------------------ sleeper API
def get(path):
    r = requests.get(f"{API}{path}", timeout=30)
    r.raise_for_status()
    return r.json()


def _cached(name, fetch):
    os.makedirs(CACHE, exist_ok=True)
    path = os.path.join(CACHE, f"{name}_{date.today().isoformat()}.json")
    if os.path.exists(path):
        return json.load(open(path))
    data = fetch()
    json.dump(data, open(path, "w"))
    for f in os.listdir(CACHE):                    # drop older days' copies
        if f.startswith(name + "_") and f != os.path.basename(path):
            os.remove(os.path.join(CACHE, f))
    return data


def players():
    return _cached("players", lambda: get("/players/nfl"))


def projections(season, week):
    def fetch():
        url = (f"https://api.sleeper.app/projections/nfl/{season}/{week}?season_type=regular"
               + "".join(f"&position%5B%5D={p}" for p in ("QB", "RB", "WR", "TE", "K", "DEF")))
        r = requests.get(url, timeout=60)
        r.raise_for_status()
        return {x["player_id"]: x.get("stats") or {} for x in r.json()}
    return _cached(f"proj_{season}_wk{week:02d}", fetch)


# ------------------------------------------------------------------ scoring
def score(stats, scoring):
    """Projected fantasy points under a league's scoring settings."""
    return float(sum(v * scoring[k] for k, v in stats.items()
                     if k in scoring and isinstance(v, (int, float))))


def player_table(pids, info, week_proj, ros_proj, scoring, market=None):
    rows = []
    market = market or {}
    for pid in pids:
        p = info.get(pid, {})
        pos = p.get("position") or ("DEF" if pid.isalpha() else None)
        name = (p.get("full_name") or f"{p.get('first_name', '')} {p.get('last_name', '')}").strip() \
            or (f"{p.get('team') or pid} D/ST" if pos == "DEF" else pid)
        status = p.get("injury_status") or ""
        wk = score(week_proj.get(pid, {}), scoring)
        ros = sum(score(w.get(pid, {}), scoring) for w in ros_proj)
        mk = market.get(_key(name))
        mkt = score(market_stats(week_proj.get(pid, {}), mk, pos), scoring) if mk else np.nan
        rows.append(dict(pid=pid, name=name, pos=pos, team=p.get("team") or "",
                         status=status, week=wk, ros=ros, mkt=mkt,
                         playing=status not in NOT_PLAYING and pid in week_proj and wk > 0))
    return pd.DataFrame(rows, columns=["pid", "name", "pos", "team", "status", "week", "ros", "mkt", "playing"])


def _key(name):
    return "".join(c for c in str(name).lower() if c.isalnum())


# ------------------------------------------------------------------ betting market
def market_means(week, season, info):
    """{normalized name: {stat: implied mean}} from player props already saved
    by fantasy_props.py (no credits spent here). Empty if none saved."""
    try:
        import fantasy_props as fp
    except Exception:
        return {}
    d = os.path.join(fp.PROPS_DIR, f"{season}_wk{week:02d}")
    if not os.path.isdir(d) or not any(f.endswith(".json") for f in os.listdir(d)):
        return {}
    cons = fp.consensus(fp.fetch_props(week, season, cached=True))
    if cons.empty:
        return {}
    qbs = {fp._normalize_name(p.get("full_name") or "") for p in info.values() if p.get("position") == "QB"}
    out = {}
    for r in cons.itertuples():
        key = fp._normalize_name(r.player)
        out.setdefault(key, {})[fp.STAT[r.market]] = fp.implied_mean(r.market, r.line, r.p, qb=key in qbs)
    return out


def market_stats(stats, mk, pos):
    """Sleeper's projected stat line with the market's means swapped in where a
    prop exists. The market prices total TDs; split them like Sleeper does."""
    st = dict(stats)
    for k in ("pass_yd", "pass_td", "rush_yd", "rec_yd", "rec"):
        if k in mk:
            if k == "rec" and "bonus_rec_te" in st:      # TE reception bonus follows receptions
                st["bonus_rec_te"] = mk["rec"]
            st[k] = mk[k]
    if "td" in mk:
        rush, rec = st.get("rush_td", 0) or 0, st.get("rec_td", 0) or 0
        if rush + rec > 0:
            st["rush_td"], st["rec_td"] = mk["td"] * rush / (rush + rec), mk["td"] * rec / (rush + rec)
        elif pos in ("QB", "RB"):
            st["rush_td"] = mk["td"]
        else:
            st["rec_td"] = mk["td"]
    return st


# ------------------------------------------------------------------ lineup
def optimal_lineup(df, slots):
    """Exact best assignment of available players to starting slots.
    Returns {slot index: pid}."""
    pool = df[df.playing].reset_index(drop=True)
    if pool.empty or not slots:
        return {}
    big = 1e6
    cost = np.full((len(pool), len(slots)), big)
    for i, r in pool.iterrows():
        for j, s in enumerate(slots):
            if r.pos in SLOT_ELIGIBLE.get(s, {s}):
                cost[i, j] = -r.week
    rows, cols = linear_sum_assignment(cost)
    return {j: pool.pid[i] for i, j in zip(rows, cols) if cost[i, j] < big}


def lineup_points(df, assignment):
    pts = df.set_index("pid").week
    return float(sum(pts[p] for p in assignment.values()))


# ------------------------------------------------------------------ report
def report_league(lg, user_id, info, week, week_proj, ros_proj, ros_weeks, market=None):
    scoring = lg.get("scoring_settings") or {}
    slots = [s for s in lg["roster_positions"] if s not in NON_STARTING]
    rosters = get(f"/league/{lg['league_id']}/rosters") or []
    rostered = {p for r in rosters for p in (r.get("players") or [])}
    mine = next((r for r in rosters if r.get("owner_id") == user_id
                 or user_id in (r.get("co_owners") or [])), None)
    # leagues kept only for their history have no rosters / players: skip them
    if not rostered or not slots:
        print(f"\n{lg['name']}: no rosters this season -- skipped")
        return
    if mine is None or not (mine.get("players") or []):
        print(f"\n{lg['name']}: you have no players on a roster here -- skipped")
        return
    my = player_table(mine.get("players") or [], info, week_proj, ros_proj, scoring, market)
    rec = scoring.get("rec", 0)
    fmt = {0: "standard", 0.5: "half-PPR", 1: "PPR"}.get(rec, f"{rec} PPR")
    print("\n" + "=" * 88)
    print(f"{lg['name']}  ({lg.get('total_rosters')} teams, {fmt}; week {week})")
    print("=" * 88)

    best = optimal_lineup(my, slots)
    current = [p for p in (mine.get("starters") or []) if p and p != "0"]
    pts = my.set_index("pid")
    cur_pts = float(sum(pts.week.get(p, 0) for p in current))
    best_pts = lineup_points(my, best)
    print(f"OPTIMAL LINEUP  {best_pts:.1f} projected by Sleeper  (your current lineup: "
          f"{cur_pts:.1f}, {best_pts - cur_pts:+.1f})")
    print(f"  {'slot':<11} {'player':<30} {'pos':<4}{'team':<5}{'Sleeper':>8}{'market':>8}")
    for j, s in enumerate(slots):
        pid = best.get(j)
        if pid is None:
            print(f"  {s:<11} -- empty: no eligible player available --")
            continue
        r = pts.loc[pid]
        flag = "" if pid in current else "   <- change"
        st = f" ({r.status})" if r.status else ""
        mk = "-" if pd.isna(r.mkt) else f"{r.mkt:.1f}"
        gap = "  !" if pd.notna(r.mkt) and abs(r.mkt - r.week) >= 3 else ""
        print(f"  {s:<11} {r['name'] + st:<30} {r.pos:<4}{r.team:<5}{r.week:>8.1f}{mk:>8}{gap}{flag}")
    if my.mkt.notna().any():
        # where the market would start someone else (market number where there
        # is one, Sleeper's otherwise) -- shown, not applied
        alt_tab = my.assign(week=my.mkt.fillna(my.week))
        alt = optimal_lineup(alt_tab, slots)
        swaps = sorted(set(alt.values()) - set(best.values()))
        outs = sorted(set(best.values()) - set(alt.values()))
        if swaps:
            nm = lambda p: f"{pts.loc[p, 'name']} ({alt_tab.set_index('pid').week[p]:.1f} market)"
            print("  market would start: " + ", ".join(nm(p) for p in swaps)
                  + "  instead of: " + ", ".join(nm(p) for p in outs))
        else:
            print("  market agrees with this lineup")
        print("  ! = Sleeper and the market differ by 3+ points")
    else:
        print(f"  (no saved props for week {week}: run `python3 fantasy_props.py {week}` "
              "for market projections, ~90 credits)")
    benched = [p for p in current if p not in set(best.values())]
    if benched:
        print("  out of the lineup: " + ", ".join(
            f"{pts.loc[p, 'name']} ({pts.loc[p, 'week']:.1f}"
            + (f", {pts.loc[p, 'status']}" if pts.loc[p, 'status'] else "") + ")"
            for p in benched if p in pts.index))
    hurt = my[(~my.playing) & my.pid.isin(current)]
    for r in hurt.itertuples():
        why = r.status or ("bye" if r.pid not in week_proj else "no projection")
        print(f"  WARNING: {r.name} is in your current lineup but won't play ({why})")

    # ---- free agents
    fa_ids = [pid for pid, p in info.items()
              if pid not in rostered and p.get("active") is not False
              and (p.get("position") in {q for s in slots for q in SLOT_ELIGIBLE.get(s, {s})})
              and (pid in week_proj or any(pid in w for w in ros_proj))]
    fa = player_table(fa_ids, info, week_proj, ros_proj, scoring, market)
    if fa.empty:
        return
    # this week: re-solve the lineup with each top free agent added
    cands = fa[fa.playing].sort_values("week", ascending=False).groupby("pos").head(8)
    gains = []
    for r in cands.itertuples():
        trial = pd.concat([my, pd.DataFrame([r._asdict()]).drop(columns="Index")], ignore_index=True)
        g = lineup_points(trial, optimal_lineup(trial, slots)) - best_pts
        if g > 0.05:
            gains.append((g, r))
    print("\nFREE AGENTS -- THIS WEEK (adding him raises your optimal lineup)")
    if not gains:
        print("  none: no free agent beats your current starters this week")
    for g, r in sorted(gains, key=lambda x: -x[0])[:6]:
        st = f" ({r.status})" if r.status else ""
        mk = "" if pd.isna(r.mkt) else f"  (market {r.mkt:.1f})"
        print(f"  {r.name + st:<30} {r.pos:<4}{r.team:<5}{r.week:>6.1f}   +{g:.1f} to your lineup{mk}")

    # rest of season: free agent vs your weakest player at the same position
    print(f"\nFREE AGENTS -- REST OF SEASON (weeks {ros_weeks[0]}-{ros_weeks[-1]}, "
          "vs your weakest player at his position)")
    lines = []
    for pos in sorted(set(fa.pos.dropna())):
        mine_pos = my[my.pos == pos]
        if mine_pos.empty:
            continue
        weakest = mine_pos.sort_values("ros").iloc[0]
        better = fa[(fa.pos == pos) & (fa.ros > weakest.ros + 5)].sort_values("ros", ascending=False).head(3)
        for r in better.itertuples():
            lines.append((r.ros - weakest.ros, f"  {r.name:<26}{pos:<4}{r.team:<5}{r.ros:>7.1f}"
                          f"   vs your {weakest['name']} {weakest.ros:.1f}  (+{r.ros - weakest.ros:.1f})"))
    if not lines:
        print("  none: no free agent projects meaningfully above your weakest player at any position")
    for _, l in sorted(lines, key=lambda x: -x[0])[:8]:
        print(l)


def main():
    a = sys.argv[1:]
    if not a or a[0].startswith("--"):
        raise SystemExit(__doc__)
    username = a[0]
    state = get("/state/nfl")
    season = int(state["league_season"])
    week = int(a[a.index("--week") + 1]) if "--week" in a else int(state["display_week"]) or 1
    only = a[a.index("--league") + 1].lower() if "--league" in a else None

    user = get(f"/user/{username}")
    if not user:
        raise SystemExit(f"no Sleeper user named {username!r}")
    leagues = get(f"/user/{user['user_id']}/leagues/nfl/{season}") or []
    if only:
        leagues = [l for l in leagues if only in l["name"].lower()]
    if not leagues:
        raise SystemExit(f"no {season} NFL leagues found for {username}")

    info = players()
    week_proj = projections(season, week)
    ros_weeks = list(range(week, LAST_WEEK + 1))
    ros_proj = [projections(season, w) for w in ros_weeks]
    market = market_means(week, season, info)
    print(f"Sleeper: {username}, {season} week {week}, {len(leagues)} league(s); "
          + (f"market projections for {len(market)} players (saved props)" if market
             else "no saved market props this week"))
    for lg in leagues:
        report_league(lg, user["user_id"], info, week, week_proj, ros_proj, ros_weeks, market)


if __name__ == "__main__":
    main()
