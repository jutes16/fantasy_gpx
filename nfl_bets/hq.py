"""
Data for the combined betting dashboard (Dashboard / Bets / Analysis tabs):
the bets you'd make, in one ledger: ELWAY's recommended game bet per game and
the prop PICKS (props.py: the PICK_MAX a week where Sleeper, recent form and the
matchup agree), plus the upcoming week's recommendations. The wider prop flags
(every Sleeper or line-shop flag) stay in props.py report, not in the ledger.

    python3 hq.py                    # -> data/dashboard_hq/*.json
    python3 hq.py --ticks <folder>   # first import the dashboard's "Bet" ticks (exported by Claude)

Reads data/bet_log.csv (bets.py) and data/prop_log.csv (props.py). One bet per
row, 1 unit flat; EV-scaled stakes as in bets.py (EV / 0.15, capped 0.5-2) for
ELWAY's bets. Props stay at 1 unit in both views: their EV is Sleeper's, which
hasn't shown it means anything yet (see props.py report).
"""

import json
import os

import numpy as np
import pandas as pd

import bets
import props
from common import DATA, SEASON, american_to_decimal, buy_yes

OUT = os.path.join(DATA, "dashboard_hq")
TYPE_NAMES = {"kalshi_win": "Kalshi win", "book_ml": "Book moneyline", "kalshi_spread": "Kalshi spread",
              "book_spread": "Book spread", "pair": "Win vs spread pair",
              "prop_pick": "Prop pick",
              "mine_kalshi": "Kalshi account", "mine_book": "Sportsbooks", "mine_prop": "Props"}
RECOMMENDED = ["kalshi_win", "book_ml", "kalshi_spread", "book_spread", "pair", "prop_pick"]
MINE = ["mine_kalshi", "mine_book", "mine_prop"]          # your own bets, by where you placed them
FILTERS = {"all": RECOMMENDED, "elway": RECOMMENDED[:5], "mine": MINE, **{k: [k] for k in TYPE_NAMES}}
FILTER_NAMES = {"all": "All recommended", "elway": "All ELWAY game bets", "mine": "My bets", **TYPE_NAMES}
MY_BETS = os.path.join(DATA, "my_bets.json")
KALSHI_MINE = os.path.join(DATA, "my_kalshi.json")    # your real Kalshi bets, from kalshi_mine.py (git-ignored)   # the dashboard's "Bet" ticks, exported by Claude (git-ignored)
STAT_LABEL = {"player_pass_yds": "Pass Yds", "player_pass_tds": "Pass TDs", "player_rush_yds": "Rush Yds",
              "player_reception_yds": "Rec Yds", "player_receptions": "Receptions",
              "player_anytime_td": "Anytime TD"}
RESULT = {"W": "W", "L": "L", "P": "P", "win": "W", "loss": "L", "push": "P", "void": "void"}


def _price_label(price):
    if pd.isna(price):
        return ""
    if 0 < price < 1:                     # Kalshi: dollars per contract
        return f"{round(price * 100)}¢"
    return f"+{int(price)}" if price > 0 else f"{int(price)}"      # American odds


def _num(v, nd=3):
    return None if v is None or pd.isna(v) else round(float(v), nd)


def elway_bets():
    log = pd.read_csv(bets.LOG)
    log = log[log.season == SEASON]
    first, _ = bets.views(log)
    r = first[first.recommended.astype(str).str.lower() == "true"]
    rows = []
    for x in r.itertuples():
        k = float(bets.scaled_stake(x.ev))
        res = RESULT.get(x.result, "open") if isinstance(x.result, str) else "open"
        strike = "" if pd.isna(x.strike) else f" {x.strike:g}"
        what = {"win": "to win", "yes": "to win" if x.instrument == "kalshi_win" else f"by over{strike}",
                "no": f"not by over{strike}"}.get(x.side, x.side)
        if x.instrument in ("book_spread", "kalshi_spread") and x.side not in ("yes", "no"):
            what = f"{x.side}{strike}"
        rows.append(dict(id=f"E|{int(x.week)}|{x.game}", source="ELWAY", type=x.instrument,
                         type_name=TYPE_NAMES.get(x.instrument, x.instrument), week=int(x.week), game=x.game,
                         bet=f"{x.team} {what}", price=_price_label(x.price), ev=_num(x.ev),
                         stake=1.0, stake_scaled=round(k, 2), result=res,
                         pnl=_num(x.pnl, 2), scaled_pnl=None if pd.isna(x.pnl) else round(float(x.pnl) * k, 2),
                         clv_cents=None if pd.isna(x.clv) else round(float(x.clv) * 100, 2),
                         phi="PHI" in (x.away, x.home)))
    return rows


def prop_bets():
    log = pd.read_csv(props.LOG)
    f = log[log.pick.astype(str).str.lower() == "true"]
    rows = []
    for x in f.itertuples():
        t = "prop_pick"
        side = {"over": "Over", "under": "Under", "yes": "Yes", "no": "No"}[x.side]
        line = "" if x.market == "player_anytime_td" else f" {x.line:g}"
        res = RESULT.get(x.result, "open") if isinstance(x.result, str) else "open"
        ev = x.ev_slp
        rows.append(dict(id=f"P|{int(x.week)}|{x.key}|{x.market}|{x.side}", source="Props", type=t,
                         type_name=TYPE_NAMES[t], week=int(x.week), game=x.game,
                         bet=f"{x.player} {side}{line} {STAT_LABEL[x.market]}", price=_price_label(x.price),
                         ev=_num(ev), stake=1.0, stake_scaled=1.0, result=res,
                         pnl=_num(x.units, 2), scaled_pnl=_num(x.units, 2), clv_cents=None,
                         phi="PHI" in x.game.split(" @ ")))
    return rows


def _game_bet_id(x):
    return f"{x.game}|{x.instrument}|{x.team}|{x.side}|{'' if pd.isna(x.strike) else x.strike}"


def win_per_unit(price, kalshi):
    """Profit on a 1-unit win at a price you entered: Kalshi in cents or dollars
    ('38', '38¢', '0.38', fee added as the ledger does), else American odds."""
    t = str(price).strip().replace("¢", "").replace("c", "")
    if not t:
        return None
    v = float(t)
    if kalshi:
        p = v / 100 if v >= 1 else v
        return 1 / buy_yes(p) - 1
    return american_to_decimal(v) - 1


def import_ticks(folder):
    """The dashboard's mybets documents (saved as JSON files, one per doc, by
    Claude's ArtifactData list) -> data/my_bets.json."""
    import glob
    docs = [json.load(open(f)) for f in glob.glob(os.path.join(folder, "*.json"))]
    keep = ("id", "kind", "week", "stake", "price_taken", "label", "price", "book", "game", "at")
    ticks = [{k: d.get("data", d).get(k) for k in keep} for d in docs]
    json.dump(sorted(ticks, key=lambda t: t["at"] or ""), open(MY_BETS, "w"), indent=1)
    print(f"{len(ticks)} ticks -> {os.path.relpath(MY_BETS)}")


def my_bets():
    """The bets you ticked on the dashboard's Analysis tab, graded from the logs at your stake."""
    if not os.path.exists(MY_BETS) and not os.path.exists(KALSHI_MINE):
        return []
    picks = json.load(open(MY_BETS)) if os.path.exists(MY_BETS) else []
    imported = os.path.exists(KALSHI_MINE)          # your real Kalshi bets (kalshi_mine.py) replace Kalshi ticks
    if imported:
        picks = [m for m in picks if not (m["kind"] == "game" and str(m.get("book") or "").startswith("KX"))]
    plog = pd.read_csv(props.LOG)
    plog["bet_id"] = plog.key + "|" + plog.market + "|" + plog.side
    blog = pd.read_csv(bets.LOG)
    blog = blog[blog.season == SEASON]
    rows = []
    for m in picks:
        wk, stake = int(m["week"]), float(m.get("stake") or 1)
        if m["kind"] == "prop":
            hit = plog[(plog.week == wk) & (plog.bet_id == m["id"])]
            if hit.empty:
                continue
            x = hit.iloc[-1]
            side = {"over": "Over", "under": "Under", "yes": "Yes", "no": "No"}[x.side]
            line = "" if x.market == "player_anytime_td" else f" {x.line:g}"
            label, units, ev = f"{x.player} {side}{line} {STAT_LABEL[x.market]}", x.units, x.ev_slp
            res, price, game = x.result, x.price, x.game
        else:
            _, latest = bets.views(blog[blog.week == wk])
            hit = latest[latest.apply(_game_bet_id, axis=1) == m["id"]] if len(latest) else latest
            if hit.empty:
                continue
            x = hit.iloc[-1]
            label, units, ev, res, price, game = m.get("label") or f"{x.team} {x.instrument}", x.pnl, x.ev, x.result, x.price, x.game
        res = RESULT.get(res, "open") if isinstance(res, str) else "open"
        taken = (m.get("price_taken") or "").strip()
        kalshi = m["kind"] == "game" and str(m.get("book") or "").startswith("KX")
        if taken:                                   # your price, not the sheet's
            try:
                w = win_per_unit(taken, kalshi)
                units = {"W": w, "L": -1.0, "P": 0.0, "void": 0.0}.get(res, np.nan)
            except ValueError:
                pass
        pnl = None if pd.isna(units) else round(float(units) * stake, 2)
        mt = "mine_prop" if m["kind"] == "prop" else ("mine_kalshi" if kalshi else "mine_book")
        rows.append(dict(id=f"M|{m['kind']}|{wk}|{m['id']}", source="Mine", type=mt, type_name=TYPE_NAMES[mt],
                         week=wk, game=game, bet=label,
                         price=(f"{taken}¢" if kalshi and taken and not taken.endswith("¢") and float(taken.replace("¢", "")) >= 1
                                else taken) if taken else _price_label(price), ev=_num(ev), stake=stake,
                         stake_scaled=stake, result=res, pnl=pnl, scaled_pnl=pnl, clv_cents=None,
                         phi="PHI" in game.split(" @ ")))
    if imported:
        for k in json.load(open(KALSHI_MINE)):
            if not k.get("week"):
                continue                                # not an NFL game this season
            rows.append(dict(id=f"K|{k['ticker']}", source="Mine", type="mine_kalshi", type_name=TYPE_NAMES["mine_kalshi"],
                             week=int(k["week"]), game=k["game"], bet=k["bet"],
                             price="" if k.get("price_cents") is None else f"{k['price_cents']}¢", ev=None,
                             stake=k["stake"], stake_scaled=k["stake"], result=k["result"], pnl=k["pnl"],
                             scaled_pnl=k["pnl"], clv_cents=None, phi="PHI" in k["game"].split(" @ ")))
    return rows


def performance(all_bets):
    """Per filter x view x stake: weekly and cumulative results, plus window summaries."""
    d = pd.DataFrame(all_bets)
    g = d[d.result.isin(["W", "L", "P"])].copy()
    weeks = sorted(g.week.unique())
    timeline, summary = [], []
    for fk, types in FILTERS.items():
        for view in ("all", "without_phi"):
            x = g[g.type.isin(types)]
            if view == "without_phi":
                x = x[~x.phi]
            if x.empty:
                continue
            for stake in ("flat", "scaled"):
                st = x.stake if stake == "flat" else x.stake_scaled
                pnl = x.pnl if stake == "flat" else x.scaled_pnl
                c = dict(bets=0, wins=0, losses=0, pushes=0, wagers=0.0, profit=0.0)
                for wk in weeks:
                    m = x.week == wk
                    wkd = dict(bets=int(m.sum()), wins=int((x.result[m] == "W").sum()),
                               losses=int((x.result[m] == "L").sum()), pushes=int((x.result[m] == "P").sum()),
                               wagers=float(st[m].sum()), profit=float(pnl[m].sum()))
                    for k in c:
                        c[k] += wkd[k]
                    timeline.append(dict(id=f"{fk}|{view}|{stake}|{wk}", filter=fk, view=view, stake=stake,
                                         week=int(wk), profit=round(wkd["profit"], 2),
                                         cum_profit=round(c["profit"], 2), cum_wagers=round(c["wagers"], 2),
                                         cum_roi=round(c["profit"] / c["wagers"], 4) if c["wagers"] else None,
                                         cum_bets=c["bets"]))
                for win, wks in (("season", weeks), ("last4", weeks[-4:]), ("last1", weeks[-1:])):
                    m = x.week.isin(wks)
                    w, l, p = ((x.result[m] == s).sum() for s in ("W", "L", "P"))
                    wag, pro = float(st[m].sum()), float(pnl[m].sum())
                    summary.append(dict(id=f"{fk}|{view}|{stake}|{win}", filter=fk, filter_name=FILTER_NAMES[fk],
                                        view=view, stake=stake, window=win, weeks=len(wks),
                                        first_week=int(wks[0]), last_week=int(wks[-1]), bets=int(m.sum()),
                                        wins=int(w), losses=int(l), pushes=int(p), wagers=round(wag, 2),
                                        profit=round(pro, 2), win_pct=round(w / (w + l), 4) if w + l else None,
                                        roi=round(pro / wag, 4) if wag else None,
                                        per_week=round(pro / len(wks), 2)))
    return timeline, summary


def recheck_prices(week):
    """Sunday-morning prices for the week's picks (snapshot.py --recheck): {(key, market, side): ...}."""
    d = os.path.join(props.fp.PROPS_DIR, f"{SEASON}_wk{week:02d}_recheck")
    if not os.path.isdir(d):
        return {}
    events = [json.load(open(os.path.join(d, f))) for f in sorted(os.listdir(d)) if f.endswith(".json")]
    off = props.offers(events)
    if off.empty:
        return {}
    off["key"] = off.player.map(props._normalize_name)
    out = {}
    for (key, market), g in off.groupby(["key", "market"]):
        for side in ("over", "under", "yes", "no"):
            q = [(r.line, r.prices[side], r.book) for r in g.itertuples() if side in r.prices]
            if q:
                out[(key, market, side)] = dict(q=q, at=events[0].get("_fetched_at"))
    return out


def _sunday(rc, line, side):
    """Best Sunday price at the same line, or the line it moved to."""
    if not rc:
        return None, None
    same = [(p, b) for l, p, b in rc["q"] if l == line]
    if same:
        p, b = max(same, key=lambda t: american_to_decimal(t[0]))
        return f"{_price_label(p)} {b}", rc["at"]
    l, p, b = min(rc["q"], key=lambda t: (abs(t[0] - line), -american_to_decimal(t[1])))
    return f"moved to {l:g}: {_price_label(p)} {b}", rc["at"]


def props_week(week):
    log = pd.read_csv(props.LOG)
    for c in ("shop", "proj", "pick"):
        log[c] = log[c].astype(str).str.lower() == "true"
    w = log[(log.week == week) & (log.proj | log.shop)]
    rc = recheck_prices(week)
    rows = []
    for x in w.itertuples():
        diff = None if pd.isna(x.slp_mean) else x.slp_mean - (x.line if x.market != "player_anytime_td" else 0.5)
        edge_pts = 0 if pd.isna(x.edge) else x.edge * 100
        stars = int(np.digitize(edge_pts, [8, 12, 16, 22, 30])) if x.proj else 0     # size of Sleeper's disagreement
        away, home = x.game.split(" @ ")
        rows.append(dict(id=f"{x.key}|{x.market}|{x.side}", week=int(week), as_of=x.logged_at, player=x.player, pos=None if pd.isna(x.pos) else x.pos,
                         team=None if pd.isna(x.team) else x.team, opp=None if pd.isna(x.opp) else x.opp,
                         game=x.game, kickoff=x.kickoff, injury="" if pd.isna(x.injury) else x.injury,
                         prop=("" if x.market == "player_anytime_td" else f"{x.line:g} ") + STAT_LABEL[x.market],
                         pick={"over": "Over", "under": "Under", "yes": "Yes", "no": "No"}[x.side],
                         price=_price_label(x.price), book=x.book,
                         mkt_proj=_num(x.fair_mean, 2), slp_proj=_num(x.slp_mean, 2), proj_diff=_num(diff, 2),
                         rating=stars, p_mkt=_num(x.p_fair), p_slp=_num(x.p_slp), ev_slp=_num(x.ev_slp),
                         ev_shop=_num(x.ev_shop), signal="PROJ + SHOP" if x.proj and x.shop else ("PROJ" if x.proj else "SHOP"),
                         sunday=_sunday(rc.get((x.key, x.market, x.side)), x.line, x.side)[0],
                         sunday_at=_sunday(rc.get((x.key, x.market, x.side)), x.line, x.side)[1],
                         is_pick=bool(x.pick), fav=_num(x.opp_factor if x.side in ("over", "yes") else
                                                     (1 / x.opp_factor if x.opp_factor else None)),
                         opp_rank=None if pd.isna(x.opp_rank) else int(x.opp_rank),
                         opp_factor=_num(x.opp_factor), hit_l5=_num(x.hit_l5), hit_l10=_num(x.hit_l10),
                         hit_season=_num(x.hit_season), result=RESULT.get(x.result, "open") if isinstance(x.result, str) else "open"))
    return rows


def games_week(week):
    log = pd.read_csv(bets.LOG)
    log = log[(log.season == SEASON) & (log.week == week)]
    _, latest = bets.views(log)
    latest = latest if len(latest) else log
    rows = []
    for x in latest.itertuples():
        strike = "" if pd.isna(x.strike) else f" {x.strike:g}"
        if x.instrument == "kalshi_win":
            what = "to win"
        elif x.instrument == "kalshi_spread":
            what = f"wins by over{strike}" if x.side == "yes" else f"doesn't win by over{strike}"
        elif x.instrument == "book_spread":
            what = f"spread{strike}"
        elif x.instrument == "pair":
            what = f"pair at{strike}"
        else:
            what = "moneyline"
        rows.append(dict(id=f"{x.game}|{x.instrument}|{x.team}|{x.side}|{'' if pd.isna(x.strike) else x.strike}",
                         week=int(week), as_of=x.logged_at, game=x.game, kickoff=x.kickoff, type=x.instrument, type_name=TYPE_NAMES.get(x.instrument, x.instrument),
                         bet=f"{x.team} {what}", price=_price_label(x.price), where=None if pd.isna(x.ticker) else str(x.ticker),
                         elway_p=_num(x.p), cost=_num(x.cost), ev=_num(x.ev),
                         stake_scaled=round(float(bets.scaled_stake(x.ev)), 2),
                         recommended=str(x.recommended).lower() == "true", phi="PHI" in (x.away, x.home)))
    return rows


def main():
    e, p, mine = elway_bets(), prop_bets(), my_bets()
    allb = e + p + mine
    timeline, summary = performance(allb)
    # the Analysis tab shows the latest week either log has reached (its sheets, graded or not)
    week = max(int(pd.read_csv(props.LOG).week.max()), int(pd.read_csv(bets.LOG).week.max()))
    out = {"bets": sorted(allb, key=lambda b: (-b["week"], b["source"], b["game"])), "timeline": timeline,
           "summary": summary, "props_week": props_week(week), "games_week": games_week(week)}
    os.makedirs(OUT, exist_ok=True)
    for k, rows in out.items():
        json.dump(rows, open(os.path.join(OUT, f"{k}.json"), "w"), default=str)
    print(f"week {week}: " + ", ".join(f"{k} ({len(v)})" for k, v in out.items()) + f" -> {os.path.relpath(OUT)}")


if __name__ == "__main__":
    import sys
    if "--ticks" in sys.argv:                       # python3 hq.py --ticks <folder of mybets docs>
        import_ticks(sys.argv[sys.argv.index("--ticks") + 1])
    main()
