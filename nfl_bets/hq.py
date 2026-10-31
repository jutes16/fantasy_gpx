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
import season
from common import DATA, SEASON, american_to_decimal, buy_yes, canon

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


def _kalshi_cents(american):
    """A Kalshi prop logged as American odds after the fee -> the contract price to place, e.g. '38¢'."""
    cost = 1 / american_to_decimal(american)
    p = (1.07 - np.sqrt(1.07 ** 2 - 0.28 * cost)) / 0.14          # p + 0.07 p (1 - p) = cost
    return f"{round(p * 100)}¢"


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
    picks = [m for m in picks if m["kind"] != "season"]   # season ticks are a note to self: kalshi_mine.py logs the real ones
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
                             scaled_pnl=k["pnl"], clv_cents=None, phi="PHI" in k["game"].split(" @ "),
                             stake_dollars=k.get("cost_dollars"), pnl_dollars=k.get("pnl_dollars"),
                             market_p=k.get("market_p"), value_dollars=k.get("value_dollars"),
                             unrealized_dollars=k.get("unrealized_dollars"), marked_at=k.get("marked_at"),
                             elway_p=elway_season_p(k)))
    return rows


_SEASON_FEED = {}


def elway_season_p(k):
    """ELWAY's latest probability for an open Kalshi season bet (season.py's saved feed), else None."""
    import glob
    import re
    if k.get("result") != "open" or not str(k.get("ticker", "")).startswith(("KXNFLWINS", "KXNFLPLAYOFF")):
        return None
    if not _SEASON_FEED:
        files = sorted(glob.glob(os.path.join(DATA, "elway", f"season_{SEASON}_wk*_elway-win-totals.csv")))
        if not files:
            return None
        wins = pd.read_csv(files[-1])
        wins["team"] = wins.home_sb_fran_id.map(canon)
        cond = pd.read_csv(files[-1].replace("win-totals", "conditional"), na_values=["."])
        cond["team"] = cond.nickname.map(dict(zip(wins.nickname, wins.team)))
        _SEASON_FEED.update(dist=season.win_dist(wins), po=dict(zip(cond.team, cond.made_playoffs_unconditional / 100)))
    t = k["ticker"]
    m = re.match(r"KXNFLWINS-\d+([A-Z]+)-(\d+)$", t)
    if m:
        team = canon(m[1])
        p = float(_SEASON_FEED["dist"][team][int(m[2]):].sum()) if team in _SEASON_FEED["dist"] else None
    else:
        p = _SEASON_FEED["po"].get(canon(t.rsplit("-", 1)[1]))
    return None if p is None else round(p if k["side"] == "yes" else 1 - p, 3)


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
                         price=_kalshi_cents(x.price) if x.book == "Kalshi" else _price_label(x.price), book=x.book,
                         mkt_proj=_num(x.fair_mean, 2), slp_proj=_num(x.slp_mean, 2), proj_diff=_num(diff, 2),
                         rating=stars, p_mkt=_num(x.p_fair), p_slp=_num(x.p_slp), ev_slp=_num(x.ev_slp),
                         ev_shop=_num(x.ev_shop), stake_scaled=round(float(bets.scaled_stake(x.ev_slp)), 2) if not pd.isna(x.ev_slp) else None,
                         signal="PROJ + SHOP" if x.proj and x.shop else ("PROJ" if x.proj else "SHOP"),
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
                         venue="Kalshi" if str(x.instrument).startswith("kalshi") or x.instrument == "pair" else "Sportsbook",
                         elway_p=_num(x.p), cost=_num(x.cost), ev=_num(x.ev),
                         stake_scaled=round(float(bets.scaled_stake(x.ev)), 2),
                         recommended=str(x.recommended).lower() == "true", phi="PHI" in (x.away, x.home)))
    return rows


def season_week():
    """The latest season-bets batch (season.py): each team's best bet vs Kalshi, with this week's playoff swing."""
    if not os.path.exists(season.LOG):
        return []
    log = pd.read_csv(season.LOG)
    log = log[(log.season == SEASON) & (log.batch == log.batch.max())]
    return [dict(id=f"{x.team}|{x.ticker}|{x.side}", week=int(x.week), as_of=x.logged_at, elway_as_of=x.elway_updated,
                 team=x.team, kind=x.kind, bet=x.bet, ticker=x.ticker, venue="Kalshi", price=round(float(x.price) * 100),
                 elway_p=_num(x.p), kalshi_p=_num(x.mid), ev=_num(x.ev), ev_adj=_num(x.ev_adj),
                 elway_wins=_num(x.elway_wins, 1), kalshi_wins=_num(x.kalshi_wins, 1),
                 po_loss=_num(x.po_loss, 1), po_now=_num(x.po_now, 1), po_win=_num(x.po_win, 1),
                 volume=None if pd.isna(x.volume) else int(x.volume),
                 stake_scaled=round(float(bets.scaled_stake(x.ev_adj)), 2),
                 recommended=str(x.recommended).lower() == "true") for x in log.itertuples()]


# ------------------------------------------------------------------ the pool
NFL_MODEL = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "nfl_model")
CARDS = (("You", "your submitted picks"), ("Model", "pool.py's card"), ("Claude", "Claude's picks"))


def pool_data():
    """The pool's three cards (pool_tracker.py's grading) and each team against the spread."""
    import contextlib
    import sys
    sys.path.insert(0, NFL_MODEL)
    import pool_tracker as pt

    @contextlib.contextmanager
    def in_model():
        here = os.getcwd()
        os.chdir(NFL_MODEL)
        try:
            yield
        finally:
            os.chdir(here)

    with in_model():
        mine, model = pt.from_submissions()
        sub = pd.read_csv(pt.SUBMISSIONS)
        claude = pt.claude_picks(sub)
        import pool_field as pf
        fp = pf.graded() if os.path.exists(pf.FIELD) else pd.DataFrame(columns=["week", "player", "team", "result"])
    picks, weekly = [], []
    for (card, _), d in zip(CARDS, (mine, model, claude)):
        if d is None or d.empty:
            continue
        for x in d.itertuples():
            res = x.result if isinstance(x.result, str) and x.result in ("W", "L", "P") else "open"
            picks.append(dict(id=f"{card}|{int(x.week)}|{int(x.slot)}", card=card, week=int(x.week), game=x.game,
                              pick=x.pick, value=_num(x.pts_value, 1), result=res, margin=_num(x.margin, 0),
                              closing_line=_num(x.closing_line, 1)))
        cw = cl = cp = 0
        for wk, g in d.groupby("week"):
            w, l, pu = ((g.result == r).sum() for r in ("W", "L", "P"))
            graded = int(w + l + pu)
            if not graded:
                continue
            cw, cl, cp = cw + int(w), cl + int(l), cp + int(pu)
            weekly.append(dict(id=f"{card}|{int(wk)}", card=card, week=int(wk), wins=int(w), losses=int(l), pushes=int(pu),
                               cum_wins=cw, cum_losses=cl, cum_pushes=cp,
                               cum_win_pct=round(cw / (cw + cl), 4) if cw + cl else None))
    # every team against the spread, at the pool line and at the closing line, from the board's results
    b = sub[sub.margin.notna()]
    rows = []
    for x in b.itertuples():
        for team, home in ((x.home, True), (x.away, False)):
            s_ = 1 if home else -1
            pool_cover = s_ * (x.margin + x.pool_line)        # lines home-perspective; margin = home margin
            close_cover = None if pd.isna(x.closing_line) else s_ * (x.margin + x.closing_line)
            rows.append(dict(team=team, home=home, pool_cover=pool_cover, close_cover=close_cover,
                             won=s_ * x.margin > 0, tie=x.margin == 0))
    t = pd.DataFrame(rows)
    fp = fp[fp.result.isin(["W", "L", "P"])]
    teams = []
    for team, g in t.groupby("team"):
        f = fp[fp.team == team]
        rec = lambda c: (int((c > 0).sum()), int((c < 0).sum()), int((c == 0).sum()))
        pw, pl, pp = rec(g.pool_cover)
        cc = g.close_cover.dropna()
        cw_, cl_, cp_ = rec(cc)
        teams.append(dict(team=team, games=len(g), su_wins=int(g.won.sum()), su_losses=int((~g.won & ~g.tie).sum()),
                          ats_wins=pw, ats_losses=pl, ats_pushes=pp,
                          ats_pct=round(pw / (pw + pl), 4) if pw + pl else None,
                          avg_cover=round(float(g.pool_cover.mean()), 1),
                          close_wins=cw_, close_losses=cl_, close_pushes=cp_,
                          home_ats=f"{int((g[g.home].pool_cover > 0).sum())}-{int((g[g.home].pool_cover < 0).sum())}",
                          away_ats=f"{int((g[~g.home].pool_cover > 0).sum())}-{int((g[~g.home].pool_cover < 0).sum())}",
                          field_picks=len(f), field_wins=int((f.result == "W").sum()), field_losses=int((f.result == "L").sum()),
                          field_net=int((f.result == "W").sum() - (f.result == "L").sum())))
    return picks, weekly, teams, field_weekly(fp, pf.ME)


def field_weekly(fp, me):
    """Every other entrant's running record by week, unnamed (entrant 1 = the leader), for the grey lines."""
    if fp.empty:
        return []
    wk = fp.assign(w=fp.result == "W", l=fp.result == "L", p=fp.result == "P") \
           .groupby(["player", "week"])[["w", "l", "p"]].sum().reset_index().sort_values("week")
    wk[["cw", "cl", "cp"]] = wk.groupby("player")[["w", "l", "p"]].cumsum()
    last = wk[wk.week == wk.week.max()].assign(pts=lambda d: d.cw + 0.5 * d.cp).sort_values("pts", ascending=False)
    order = {name: i + 1 for i, name in enumerate(last.player)}
    out = []
    for x in wk[wk.player != me].itertuples():
        n = order.get(x.player, 99)
        out.append(dict(id=f"{n}|{int(x.week)}", entrant=n, leader=n == 1, week=int(x.week), wins=int(x.w), losses=int(x.l),
                        pushes=int(x.p), cum_wins=int(x.cw), cum_losses=int(x.cl), cum_pushes=int(x.cp)))
    return out


def main():
    e, p, mine = elway_bets(), prop_bets(), my_bets()
    allb = e + p + mine
    timeline, summary = performance(allb)
    # the Analysis tab shows the latest week either log has reached (its sheets, graded or not)
    week = max(int(pd.read_csv(props.LOG).week.max()), int(pd.read_csv(bets.LOG).week.max()))
    out = {"bets": sorted(allb, key=lambda b: (-b["week"], b["source"], b["game"])), "timeline": timeline,
           "summary": summary, "props_week": props_week(week), "games_week": games_week(week),
           "season_week": season_week()}
    out["pool_picks"], out["pool_weekly"], out["pool_teams"], out["pool_field"] = pool_data()
    stand = os.path.join(DATA, "pool_standings.json")      # your place in the pool, from its sheet (git-ignored)
    out["pool_standings"] = json.load(open(stand)) if os.path.exists(stand) else []
    os.makedirs(OUT, exist_ok=True)
    for k, rows in out.items():
        json.dump(rows, open(os.path.join(OUT, f"{k}.json"), "w"), default=str)
    print(f"week {week}: " + ", ".join(f"{k} ({len(v)})" for k, v in out.items()) + f" -> {os.path.relpath(OUT)}")


if __name__ == "__main__":
    import sys
    if "--ticks" in sys.argv:                       # python3 hq.py --ticks <folder of mybets docs>
        import_ticks(sys.argv[sys.argv.index("--ticks") + 1])
    main()
