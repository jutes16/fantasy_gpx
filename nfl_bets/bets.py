"""
ELWAY bet sheet, bet log and grading -- separate from the picks pool.

Weekly:
    pbpaste | python3 bets.py elway 4     save the pasted ELWAY table (or reuse the
                                          one already imported for the pool)
    python3 bets.py sheet 4               pull Kalshi + sportsbook prices, price
                                          every bet against ELWAY, print the sheet
    python3 bets.py sheet 4 --log         ...and log the bets as a new batch
                                          (pre-kickoff only; refused if ELWAY
                                          hasn't changed since the last batch)
    python3 kalshi.py 4                   another snapshot right before kickoff
                                          (becomes the "close" for CLV)
    python3 bets.py grade 4               after the games
    python3 bets.py report                season ledger by instrument

Options for sheet: --min-ev 0.05 (threshold to flag a bet), --no-fetch (reuse
the latest Kalshi snapshot instead of pulling a new one), --books (price
sportsbook bets at the best offer across every book via odds_api.py, 3
credits; with --no-fetch it reuses the latest saved book snapshot).

Instruments, all priced at what you could actually execute:
    kalshi_win      buy YES on "<team> wins" at the ask, plus the Kalshi fee
    kalshi_spread   buy YES or NO on "<team> wins by over k" (strikes within
                    3.5 pts of the sportsbook line, where the liquidity is)
    book_ml         sportsbook moneyline (nflverse's current consensus price)
    book_spread     sportsbook spread at its posted odds
    pair            Kalshi win vs spread at the strike matching the book line:
                    buy $1 of one, sell $1 of the other, zero net outlay
EV is per $1 staked (for pairs, per $1 bought + $1 sold). Stakes are flat 1
unit; sizing is your call. This is a test ledger, not advice -- ELWAY's edge
over the market is unproven, and the margin model is an approximation.
"""

import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd

import kalshi
from common import (DATA, SEASON, EmpiricalMargin, HistoricalMargin, american_to_decimal,
                    buy_no, buy_yes, elway_path, kalshi_fee, load_dists, load_elway,
                    parse_elway, schedule)

LOG = os.path.join(DATA, "bet_log.csv")
STRIKE_WINDOW = 3.5
MAX_KALSHI_SPREAD = 0.04       # skip Kalshi quotes wider than 4c
MIN_PRICE = 0.10               # skip contracts under 10c: the normal model's tails
                               # are least reliable exactly where they look richest


def sell_net(bid):
    return bid - kalshi_fee(bid)


# ------------------------------------------------------------------ pricing
def price_week(week, fetch=True, books=None):
    sched = schedule(SEASON, week)
    el = load_elway(week)
    if el is None or el.empty:
        raise SystemExit(f"no ELWAY table for week {week}.\n"
                         f"copy it from the ELWAY page, then: pbpaste | python3 bets.py elway {week}")
    snap = kalshi.snapshot(week) if fetch else kalshi.latest(week)
    if snap is None or snap.empty:
        raise SystemExit("no Kalshi snapshot -- run without --no-fetch")
    snap_time = snap.fetched_at.iloc[0]

    g = sched.merge(el[["away", "home", "elway_line", "elway_home_wp"]], on=["away", "home"], how="left")
    miss = g[g.elway_line.isna()]
    if len(miss):
        print("  no ELWAY projection for: " + ", ".join(f"{a} @ {h}" for a, h in zip(miss.away, miss.home)))
    g = g.dropna(subset=["elway_line"])

    dists = load_dists(week)
    if dists:
        print(f"  using ELWAY's simulated margin distribution for {len(dists)} games"
              f" (historical-shape fallback for the rest)")
    else:
        print("  no ELWAY distribution pulled: using the historical-shape fallback"
              " (centred on ELWAY's win %)")
    bets, games = [], []
    for r in g.itertuples():
        if (r.away, r.home) in dists:
            M = EmpiricalMargin(dists[(r.away, r.home)], r.elway_home_wp)
            if abs(M.home_wp - r.elway_home_wp) > 0.02:
                print(f"  WARNING {r.away} @ {r.home}: distribution gives home win "
                      f"{M.home_wp:.1%}, ELWAY table says {r.elway_home_wp:.1%}")
        else:
            M = HistoricalMargin(r.elway_line, r.elway_home_wp)
        label = f"{r.away} @ {r.home}"
        base = dict(season=SEASON, week=week, game=label, away=r.away, home=r.home,
                    kickoff=r.kickoff.isoformat(), kalshi_snapshot=snap_time)
        k = snap[(snap.away == r.away) & (snap.home == r.home)]
        kw = k[k.kind == "win"].set_index("team")

        # ---- Kalshi win contracts
        for team, is_home in ((r.home, True), (r.away, False)):
            if team in kw.index:
                ask = kw.at[team, "yes_ask"]
                if MIN_PRICE <= ask < 0.99:
                    p = M.p_team_wins(is_home)
                    c = buy_yes(ask)
                    bets.append(dict(base, instrument="kalshi_win", team=team, side="yes",
                                     strike=np.nan, price=ask, cost=c, p=p, ev=p / c - 1,
                                     ticker=kw.at[team, "ticker"]))

        # ---- Kalshi spread ladder: the FAVOURITE's "wins by over k" markets near
        # the book line (YES = favourite covers k, NO = underdog +k). The
        # underdog's own "wins by over" ladder is a different bet entirely.
        book_abs = abs(r.spread_line) if pd.notna(r.spread_line) else 3.0
        fav = r.home if (pd.isna(r.spread_line) or r.spread_line >= 0) else r.away
        ks = k[(k.kind == "spread") & (k.team == fav)
               & ((k.yes_ask - k.yes_bid) <= MAX_KALSHI_SPREAD)
               & ((k.strike - book_abs).abs() <= STRIKE_WINDOW)]
        for s in ks.itertuples():
            p = M.p_team_by_over(s.team == r.home, s.strike)
            if MIN_PRICE <= s.yes_ask < 0.99:
                c = buy_yes(s.yes_ask)
                bets.append(dict(base, instrument="kalshi_spread", team=s.team, side="yes",
                                 strike=s.strike, price=s.yes_ask, cost=c, p=p, ev=p / c - 1,
                                 ticker=s.ticker))
            if 0.01 < s.yes_bid <= 1 - MIN_PRICE:
                c = buy_no(s.yes_bid)
                bets.append(dict(base, instrument="kalshi_spread", team=s.team, side="no",
                                 strike=s.strike, price=1 - s.yes_bid, cost=c, p=1 - p,
                                 ev=(1 - p) / c - 1, ticker=s.ticker))

        # ---- sportsbook moneyline and spread: every book's offer when a
        # best-price snapshot is loaded (odds_api.py), else nflverse consensus.
        # For each side, keep the offer with the highest EV under ELWAY -- for
        # spreads that weighs the line and the price together.
        offers_ml, offers_sp = _book_offers(r, books)
        for team, is_home in ((r.home, True), (r.away, False)):
            side = "home" if is_home else "away"
            p = M.p_team_wins(is_home)
            best = None
            for book, ml in offers_ml.get(side, []):
                d = american_to_decimal(ml)
                if best is None or p * d - 1 > best["ev"]:
                    best = dict(price=ml, cost=1 / d, ev=p * d - 1, ticker=book)
            if best:
                bets.append(dict(base, instrument="book_ml", team=team, side="win",
                                 strike=np.nan, p=p, **best))
            best = None
            for book, line, odds in offers_sp.get(side, []):     # line = team's number
                pc = M.p_team_by_over(is_home, -line)
                d = american_to_decimal(odds)
                if best is None or pc * d - 1 > best["ev"]:
                    best = dict(strike=line, price=odds, cost=1 / d, p=pc, ev=pc * d - 1, ticker=book)
            if best:
                bets.append(dict(base, instrument="book_spread", team=team, side="cover", **best))

        # ---- the win-vs-spread pair at the strike matching the book line
        pair = _pair(r, M, k, kw)
        if pair:
            bets.append(dict(base, **pair))

        games.append(dict(game=label, kickoff=r.kickoff.astimezone(timezone.utc),
                          elway_line=r.elway_line, elway_home=M.home_wp,
                          kalshi_home=_mid(kw, r.home), book_home=_novig(r.home_moneyline, r.away_moneyline),
                          book_line=-r.spread_line if pd.notna(r.spread_line) else np.nan,
                          sd=M.sd, model=getattr(M, "kind", "dist")))
    return pd.DataFrame(bets), pd.DataFrame(games), sched


def _book_offers(r, books):
    """(moneyline offers, spread offers) per side for one game:
    {'home': [(book, price)], ...}, {'home': [(book, team_line, price)], ...}.
    From a best-price snapshot when given, else nflverse's consensus."""
    ml, sp = {"home": [], "away": []}, {"home": [], "away": []}
    if books is not None and len(books):
        g = books[(books.away == r.away) & (books.home == r.home)]
        for x in g[g.market == "h2h"].itertuples():
            ml[x.side].append((x.book, x.price))
        for x in g[(g.market == "spreads") & g.point.notna()].itertuples():
            team_line = x.point if x.side == "home" else -x.point   # stored as the home line
            sp[x.side].append((x.book, team_line, x.price))
        if any(ml.values()) or any(sp.values()):
            return ml, sp
    if pd.notna(r.home_moneyline):
        ml["home"].append(("consensus", r.home_moneyline))
    if pd.notna(r.away_moneyline):
        ml["away"].append(("consensus", r.away_moneyline))
    if pd.notna(r.spread_line):           # nflverse: > 0 = home favoured
        if pd.notna(r.home_spread_odds):
            sp["home"].append(("consensus", -r.spread_line, r.home_spread_odds))
        if pd.notna(r.away_spread_odds):
            sp["away"].append(("consensus", r.spread_line, r.away_spread_odds))
    return ml, sp


def _mid(kw, team):
    if team not in kw.index:
        return np.nan
    return (kw.at[team, "yes_bid"] + kw.at[team, "yes_ask"]) / 2


def _novig(h, a):
    if pd.isna(h) or pd.isna(a):
        return np.nan
    ih, ia = 1 / american_to_decimal(h), 1 / american_to_decimal(a)
    return ih / (ih + ia)


def _pair(r, M, k, kw):
    """Buy $1 of one contract, sell $1 of the other, on the favourite's strike
    nearest the book line. Two structures, as in the ELWAY/Kalshi writeup:
      dog:  buy D wins,          sell D+k  (= NO on 'F by over k')
      fav:  buy 'F by over k',   sell F wins
    """
    if pd.isna(r.spread_line) or r.spread_line == 0:
        return None
    fav, dog = (r.home, r.away) if r.spread_line > 0 else (r.away, r.home)
    fs = k[(k.kind == "spread") & (k.team == fav)]
    if fs.empty or fav not in kw.index or dog not in kw.index:
        return None
    s = fs.iloc[(fs.strike - abs(r.spread_line)).abs().argsort()].iloc[0]
    kk = s.strike
    fav_home = fav == r.home
    p_fav_over = M.p_team_by_over(fav_home, kk)
    p_fav_win = M.p_team_wins(fav_home)
    p_dog_win = 1 - p_fav_win
    p_dog_cover = 1 - p_fav_over
    out = []
    # dog: buy D win at its ask; sell D+k at its bid (= 1 - ask of F-over-k)
    dog_ask, dk_bid = kw.at[dog, "yes_ask"], 1 - s.yes_ask
    if 0 < dog_ask < 0.99 and 0.01 < dk_bid < 1:
        nb, ns = 1 / buy_yes(dog_ask), 1 / sell_net(dk_bid)
        out.append(dict(instrument="pair", team=dog, side="buy win, sell spread", strike=kk,
                        price=dog_ask, cost=dk_bid, p=p_dog_win,
                        ev=p_dog_win * nb - p_dog_cover * ns, n_buy=nb, n_sell=ns,
                        ticker=f"{kw.at[dog, 'ticker']} / {s.ticker}"))
    # fav: buy F-over-k at its ask; sell F win at its bid
    fk_ask, fav_bid = s.yes_ask, kw.at[fav, "yes_bid"]
    if 0 < fk_ask < 0.99 and 0.01 < fav_bid < 1:
        nb, ns = 1 / buy_yes(fk_ask), 1 / sell_net(fav_bid)
        out.append(dict(instrument="pair", team=fav, side="buy spread, sell win", strike=kk,
                        price=fk_ask, cost=fav_bid, p=p_fav_over,
                        ev=p_fav_over * nb - p_fav_win * ns, n_buy=nb, n_sell=ns,
                        ticker=f"{s.ticker} / {kw.at[fav, 'ticker']}"))
    return max(out, key=lambda x: x["ev"]) if out else None


# ------------------------------------------------------------------ sheet
def describe(b):
    if b.instrument == "kalshi_win":
        return f"{b.team} wins  YES @ {b.price:.2f}"
    if b.instrument == "kalshi_spread":
        return f"{b.team} by over {b.strike:g}  {b.side.upper()} @ {b.price:.2f}"
    at = f" @{b.ticker}" if isinstance(b.ticker, str) and b.ticker not in ("", "consensus") else ""
    if b.instrument == "book_ml":
        return f"{b.team} ML {b.price:+.0f}{at}"
    if b.instrument == "book_spread":
        return f"{b.team} {b.strike:+g} ({b.price:+.0f}){at}"
    if b.instrument == "pair":
        return f"{b.team} {b.side} (k={b.strike:g})"
    return ""


def sheet(week, min_ev=0.05, fetch=True, log=False, force=False, books=False):
    book_df = None
    if books:                       # best price across sportsbooks (odds_api.py)
        import odds_api
        book_df = odds_api.snapshot(week) if fetch else odds_api.latest(week)
        if book_df is None or book_df.empty:
            print("  no sportsbook snapshot -- using nflverse consensus")
            book_df = None
    bets, games, sched = price_week(week, fetch, books=book_df)
    now = datetime.now(timezone.utc)
    print("=" * 92)
    print(f"ELWAY BET SHEET  {SEASON} week {week}   (prices as of {bets.kalshi_snapshot.iloc[0]}; "
          f"EV per $1; flag at EV >= {min_ev:.2f})")
    print("=" * 92)
    print(f"{'game':<12}{'kickoff (ET)':<14}{'ELWAY':>7}{'book':>6}  "
          f"{'home win%: ELWAY':>16}{'Kalshi':>8}{'book':>7}{'sd':>6}  model")
    for r in games.sort_values("kickoff").itertuples():
        ko = r.kickoff.tz_convert("America/New_York").strftime("%a %H:%M")
        print(f"{r.game:<12}{ko:<14}{r.elway_line:>+7g}{r.book_line:>+6g}  "
              f"{r.elway_home:>16.1%}{r.kalshi_home:>8.1%}{r.book_home:>7.1%}{r.sd:>6.1f}  {r.model}")

    # best bet per game per instrument
    best = (bets.sort_values("ev", ascending=False)
            .groupby(["game", "instrument"], as_index=False).head(1))
    started = pd.to_datetime(best.kickoff) <= now
    print(f"\n{'BEST BET PER GAME AND INSTRUMENT':<40}  (* = EV >= {min_ev:.2f})")
    for inst in ("kalshi_win", "book_ml", "kalshi_spread", "book_spread", "pair"):
        sub = best[best.instrument == inst].sort_values("ev", ascending=False)
        n = (sub.ev >= min_ev).sum()
        print(f"\n  {inst}  ({n} flagged)")
        for b in sub.itertuples():
            flag = "*" if b.ev >= min_ev else " "
            print(f"   {flag} {b.game:<12}{describe(b):<34} ELWAY {b.p:5.1%}  EV {b.ev:+.3f}")

    # one recommendation per game: the best EV across instruments (pairs excluded:
    # they are a relative-value test, not a directional bet)
    direct = best[best.instrument != "pair"]
    rec = direct.sort_values("ev", ascending=False).groupby("game").head(1)
    rec = rec[rec.ev >= min_ev].sort_values("ev", ascending=False)
    print("\n" + "-" * 92)
    print(f"RECOMMENDED (best instrument per game, EV >= {min_ev:.2f}): {len(rec)} bets, "
          f"ELWAY expects {rec.ev.sum():+.2f} units")
    for b in rec.itertuples():
        print(f"   {b.game:<12}{b.instrument:<14}{describe(b):<34} EV {b.ev:+.3f}")
    print("   Caveat: ELWAY vs the market is unproven; stakes are 1 unit, flat.")

    if log:
        flagged = best[best.ev >= min_ev].copy()
        flagged["recommended"] = flagged.index.isin(rec.index)
        write_log(flagged, week, now, force=force)


def elway_fingerprint(week):
    """Hash of the ELWAY inputs for a week (table + distribution), so a new
    log batch can tell whether ELWAY itself changed."""
    import hashlib
    from common import dist_path
    h = hashlib.sha1()
    for p in (elway_path(week), dist_path(week)):
        if p and os.path.exists(p):
            h.update(open(p, "rb").read())
    return h.hexdigest()[:10]


def write_log(flagged, week, now, force=False):
    """Append this run as a new batch; earlier batches are never touched.

    Only games that haven't kicked off are logged. A new batch is refused when
    ELWAY's inputs are unchanged since the week's last batch (re-logging at
    new prices alone would just move the test's entry point) -- pass --force
    to log anyway."""
    flagged = flagged[pd.to_datetime(flagged.kickoff) > now].copy()
    fp = elway_fingerprint(week)
    old = pd.read_csv(LOG) if os.path.exists(LOG) else pd.DataFrame()
    wk = old[(old.season == SEASON) & (old.week == week)] if len(old) else old
    batch = 1
    if len(wk):
        if "batch" not in wk:
            wk = wk.assign(batch=1)
        batch = int(wk.batch.max()) + 1
        last_fp = wk[wk.batch == wk.batch.max()].get("elway_fp")
        if (not force and last_fp is not None and last_fp.notna().any()
                and last_fp.iloc[0] == fp):
            print(f"\nNOT logged: ELWAY's week-{week} inputs are unchanged since batch "
                  f"{batch - 1} ({wk.logged_at.max()}). Re-log only when ELWAY updates"
                  f" (or pass --force).")
            return
    flagged["batch"] = batch
    flagged["elway_fp"] = fp
    flagged["logged_at"] = now.strftime("%Y-%m-%dT%H:%M:%SZ")
    flagged["stake"] = 1.0
    for c in ("result", "pnl", "close_price", "clv"):
        flagged[c] = np.nan
    out = pd.concat([old, flagged], ignore_index=True)
    os.makedirs(DATA, exist_ok=True)
    out.to_csv(LOG, index=False)
    print(f"\nlogged batch {batch}: {len(flagged)} bets for week {week} -> "
          f"{os.path.relpath(LOG)}  ({flagged.recommended.sum()} recommended)")


def views(log):
    """Two readings of a log with several batches per week:
      first  -- each game as first logged (what you'd have bet on day one)
      latest -- each game as last logged before its kickoff
    Returns (first, latest) DataFrames."""
    if log.empty:
        return log, log
    if "batch" not in log:
        log = log.assign(batch=1)
    log = log.assign(batch=log.batch.fillna(1).astype(int))
    key = ["season", "week", "game"]
    b = log.groupby(key).batch
    first = log[log.batch == b.transform("min")]
    pre = log[pd.to_datetime(log.logged_at) < pd.to_datetime(log.kickoff)]
    latest = pre[pre.batch == pre.groupby(key).batch.transform("max")]
    return first, latest


# ------------------------------------------------------------------ grading
def grade(week):
    if not os.path.exists(LOG):
        raise SystemExit("no bet log yet")
    log = pd.read_csv(LOG)
    m = (log.season == SEASON) & (log.week == week)
    if not m.any():
        raise SystemExit(f"no logged bets for week {week}")
    sched = schedule(SEASON, week).set_index(["away", "home"])
    snaps = [pd.read_csv(p) for p in kalshi.snapshots(week)]
    for c in ("result", "close_price"):
        log[c] = log[c].astype("object")
    done = 0
    for i in log[m].index:
        b = log.loc[i]
        g = sched.loc[(b.away, b.home)]
        if pd.isna(g.result):
            continue
        tm = g.result if b.team == b.home else -g.result          # team's margin
        pnl, res = _settle(b, tm)
        log.at[i, "pnl"], log.at[i, "result"] = pnl, res
        log.at[i, "close_price"], log.at[i, "clv"] = _clv(b, g, snaps)
        done += 1
    log.to_csv(LOG, index=False)
    print(f"graded {done} of {m.sum()} week-{week} bets")
    report(weeks=[week])


def _settle(b, tm):
    inst = b.instrument
    if inst in ("kalshi_win", "book_ml"):
        if tm == 0:
            return (0.0, "P") if inst == "book_ml" else (-1.0, "L")   # Kalshi tie: NO
        won = tm > 0
    elif inst == "kalshi_spread":
        over = tm > b.strike
        won = over if b.side == "yes" else not over
    elif inst == "book_spread":
        c = tm + b.strike
        if c == 0:
            return 0.0, "P"
        won = c > 0
    elif inst == "pair":
        if b.side.startswith("buy win"):          # team = dog; covers = dog margin > -k
            pnl = b.n_buy * (tm > 0) - b.n_sell * (tm > -b.strike)
        else:                                     # team = fav
            pnl = b.n_buy * (tm > b.strike) - b.n_sell * (tm > 0)
        return float(pnl), "W" if pnl > 0 else ("L" if pnl < 0 else "0")
    else:
        return np.nan, ""
    pay = 1 / b.cost - 1
    return (pay if won else -1.0), ("W" if won else "L")


def _clv(b, g, snaps):
    """(closing price, CLV in probability points; + = you beat the close)."""
    ko = pd.Timestamp(b.kickoff)
    if b.instrument in ("kalshi_win", "kalshi_spread"):
        pre = [s for s in snaps if pd.Timestamp(s.fetched_at.iloc[0]) < ko
               and pd.Timestamp(s.fetched_at.iloc[0]) > pd.Timestamp(b.logged_at)]
        if not pre:
            return np.nan, np.nan
        row = pre[-1][pre[-1].ticker == b.ticker]
        if row.empty:
            return np.nan, np.nan
        mid = float((row.yes_bid.iloc[0] + row.yes_ask.iloc[0]) / 2)
        close = mid if b.side == "yes" else 1 - mid
        return close, close - b.price
    if b.instrument == "book_ml":
        ml = g.home_moneyline if b.team == b.home else g.away_moneyline
        if pd.isna(ml):
            return np.nan, np.nan
        return ml, 1 / american_to_decimal(ml) - b.cost
    return np.nan, np.nan


# ------------------------------------------------------------------ report
def report(weeks=None):
    if not os.path.exists(LOG):
        raise SystemExit("no bet log yet")
    log = pd.read_csv(LOG)
    log = log[log.season == SEASON]
    if weeks:
        log = log[log.week.isin(weeks)]
    title = f"week {weeks[0]}" if weeks and len(weeks) == 1 else "season to date"
    first, latest = views(log)
    nb = int(log.groupby(["season", "week"]).batch.nunique().sum()) if "batch" in log else 1
    print("=" * 84)
    print(f"ELWAY BET LEDGER  {SEASON} {title}   ({log.pnl.notna().sum()} graded of "
          f"{len(log)} logged, {nb} batch(es))")
    print("=" * 84)

    def line(name, d):
        d = d.dropna(subset=["pnl"])
        if d.empty:
            return
        w, l = (d.result == "W").sum(), (d.result == "L").sum()
        clv = d.clv.mean() * 100 if d.clv.notna().any() else np.nan
        roi = d.pnl.sum() / d.stake.sum()
        print(f"{name:<15}{len(d):>5}{f'{w}-{l}':>8}{d.ev.sum():>+11.2f}{d.pnl.sum():>+8.2f}"
              f"{roi:>+8.1%}" + (f"{clv:>+7.1f}c" if pd.notna(clv) else f"{'-':>8}"))

    rec = lambda d: d[d.recommended.astype(str).str.lower() == "true"]
    print("FIRST LOG (each game as first logged)")
    print(f"{'instrument':<15}{'bets':>5}{'W-L':>8}{'ELWAY exp':>11}{'P&L':>8}{'ROI':>8}{'CLV':>8}")
    for inst in ("kalshi_win", "book_ml", "kalshi_spread", "book_spread", "pair"):
        line(inst, first[first.instrument == inst])
    print("-" * 84)
    line("RECOMMENDED", rec(first))
    if len(latest) and not latest.index.equals(first.index):
        print("\nLATEST LOG (each game as last logged before kickoff)")
        line("RECOMMENDED", rec(latest))
        sig = lambda d: rec(d).set_index(["season", "week", "game"])[["instrument", "team", "side", "strike"]]
        a, b = sig(first), sig(latest)
        both = a.index.intersection(b.index)
        changed = int((a.loc[both].fillna("-") != b.loc[both].fillna("-")).any(axis=1).sum()
                      + len(a.index.symmetric_difference(b.index)))
        print(f"  ({changed} game(s) where an ELWAY update changed the recommended bet; the gap between the two"
              f"\n   RECOMMENDED rows is what ELWAY's updates were worth)")
    print("\nCLV = closing price minus your price, in cents of probability (Kalshi: the")
    print("last snapshot before kickoff; book: nflverse's closing moneyline). Positive")
    print("CLV over many bets is the earliest honest sign of an edge; P&L lags far behind.")


# ------------------------------------------------------------------ ELWAY paste
def save_elway(week):
    text = "" if sys.stdin.isatty() else sys.stdin.read()
    if parse_elway(text).empty:
        p = elway_path(week)
        if p:
            print(f"no games in the paste; using the saved table {os.path.relpath(p)}")
            return
        raise SystemExit("couldn't find any games in the paste -- copy the ELWAY table and pipe it in")
    d = os.path.join(DATA, "elway")
    os.makedirs(d, exist_ok=True)
    p = os.path.join(d, f"{SEASON}_wk{week:02d}.txt")
    open(p, "w").write(text)
    e = parse_elway(text)
    print(f"saved {len(e[e.week == week])} ELWAY games for week {week} -> {os.path.relpath(p)}")


if __name__ == "__main__":
    a = sys.argv[1:]
    if not a:
        raise SystemExit(__doc__)
    cmd = a[0]
    opt = lambda k, d: float(a[a.index(k) + 1]) if k in a else d
    if cmd == "sheet":
        sheet(int(a[1]), min_ev=opt("--min-ev", 0.05), fetch="--no-fetch" not in a, log="--log" in a,
              force="--force" in a, books="--books" in a)
    elif cmd == "grade":
        grade(int(a[1]))
    elif cmd == "report":
        report()
    elif cmd == "elway":
        save_elway(int(a[1]))
    else:
        raise SystemExit(__doc__)
