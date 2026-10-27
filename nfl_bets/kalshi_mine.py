"""
Your real Kalshi NFL bets, read from your account (read-only), for "My bets"
on the Bet Tracker dashboard.

    python3 kalshi_mine.py        # -> data/my_kalshi.json (git-ignored); then python3 hq.py

Setup (once; never put the key in the repo):
  1. kalshi.com -> Account Settings -> API Keys -> Create New API Key. Save the
     private key file it downloads (it can't be downloaded again) and note the
     Key ID.
  2. mkdir -p ~/.kalshi && mv ~/Downloads/<the key file> ~/.kalshi/kalshi_key.pem
     && chmod 600 ~/.kalshi/kalshi_key.pem
  3. In ~/.zshrc:
       export KALSHI_API_KEY_ID="<Key ID>"
       export KALSHI_PRIVATE_KEY_PATH="$HOME/.kalshi/kalshi_key.pem"
       export MY_UNIT_DOLLARS="<what 1 unit is for you, e.g. 10>"

This only ever sends GET requests (fills and settlements): it never places,
changes or cancels an order. Each NFL market you traded becomes one bet:
  stake  = what you paid, fees included, in units (MY_UNIT_DOLLARS)
  price  = your average price on the side you hold, in cents
  result = from Kalshi's settlement (Kalshi's own cost and payout), else open
Positions are netted per market the way Kalshi does: buying NO and selling YES
are the same exposure. When this file exists, hq.py takes your Kalshi bets from
it and ignores Kalshi rows ticked on the Analysis tab (no double counting).
"""

import base64
import json
import os
import sys
import time
import urllib.parse
import urllib.request
from collections import defaultdict
from datetime import datetime

import pandas as pd

from common import DATA, SEASON, canon, nflverse

API = os.environ.get("KALSHI_API_BASE", "https://api.elections.kalshi.com/trade-api/v2")
OUT = os.path.join(DATA, "my_kalshi.json")
NFL = ("KXNFLGAME", "KXNFLSPREAD", "KXNFLTOTAL")
TO_KALSHI = {"JAX": "JAC"}                        # Kalshi's abbreviations, where they differ


# ------------------------------------------------------------------ signed GETs
def _key():
    kid = os.environ.get("KALSHI_API_KEY_ID")
    path = os.path.expanduser(os.environ.get("KALSHI_PRIVATE_KEY_PATH", "~/.kalshi/kalshi_key.pem"))
    if not kid or not os.path.exists(path):
        raise SystemExit("set KALSHI_API_KEY_ID and KALSHI_PRIVATE_KEY_PATH in ~/.zshrc (see the top of this file)")
    from cryptography.hazmat.primitives import serialization
    return kid, serialization.load_pem_private_key(open(path, "rb").read(), password=None)


def _sign(pk, message):
    from cryptography.hazmat.primitives import hashes
    from cryptography.hazmat.primitives.asymmetric import ed25519, padding
    if isinstance(pk, ed25519.Ed25519PrivateKey):            # Kalshi's default key type
        return pk.sign(message)
    return pk.sign(message, padding.PSS(mgf=padding.MGF1(hashes.SHA256()),
                                        salt_length=padding.PSS.DIGEST_LENGTH), hashes.SHA256())


def get(path, params=None, key=None):
    kid, pk = key or _key()
    ts = str(int(time.time() * 1000))
    signed_path = urllib.parse.urlparse(API).path + path      # /trade-api/v2/..., no query string
    sig = base64.b64encode(_sign(pk, (ts + "GET" + signed_path).encode())).decode()
    url = API + path + (f"?{urllib.parse.urlencode(params)}" if params else "")
    req = urllib.request.Request(url, headers={"KALSHI-ACCESS-KEY": kid, "KALSHI-ACCESS-TIMESTAMP": ts,
                                               "KALSHI-ACCESS-SIGNATURE": sig, "Accept": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return json.load(r)
    except urllib.error.HTTPError as e:
        raise SystemExit(f"Kalshi {e.code} on {path}: {e.read().decode()[:300]}")


def every(path, field, key, **params):
    out, cursor = [], ""
    while True:
        page = get(path, dict(params, limit=1000, **({"cursor": cursor} if cursor else {})), key)
        out += page.get(field) or []
        cursor = page.get("cursor") or ""
        if not cursor or not page.get(field):
            return out


# ------------------------------------------------------------------ fills -> bets
def _f(x, default=0.0):
    try:
        return float(x)
    except (TypeError, ValueError):
        return default


def describe(ticker):
    """'KXNFLSPREAD-26OCT25GBDET-DET7' -> (event code, kind, bet text for a YES holder, for a NO holder)."""
    series, code, suffix = ticker.split("-", 2)
    num = "".join(c for c in suffix if c.isdigit())
    team = canon(suffix.rstrip("0123456789"))
    if series == "KXNFLGAME":
        return code, "Kalshi win", f"{team} to win", f"{team} not to win"
    if series == "KXNFLSPREAD":
        k = int(num) - 0.5
        return code, "Kalshi spread", f"{team} wins by over {k:g}", f"{team} doesn't win by over {k:g}"
    k = int(num) - 0.5
    return code, "Kalshi total", f"over {k:g} points", f"under {k:g} points"


def season_schedule(season=SEASON):
    g = nflverse(refresh=False)
    g = g[(g.season == season) & (g.game_type == "REG")].copy()
    g["away"], g["home"] = g.away_team.map(canon), g.home_team.map(canon)
    return g


def find_game(code, sched):
    """'26OCT25GBDET' -> (week, 'GB @ DET') from the schedule, else (None, None)."""
    date = datetime.strptime(code[:7], "%y%b%d").date().isoformat()
    teams = code[7:]
    for r in sched[sched.gameday == date].itertuples():
        for a in {r.away, TO_KALSHI.get(r.away, r.away)}:
            for h in {r.home, TO_KALSHI.get(r.home, r.home)}:
                if teams == a + h:
                    return int(r.week), f"{r.away} @ {r.home}"
    return None, None


def positions(fills, settlements, sched, unit):
    """One bet per NFL market: stake, price, result and P&L (dollars and units)."""
    held = defaultdict(lambda: dict(yes=0.0, no=0.0, cost_yes=0.0, cost_no=0.0, fees=0.0, first=None))
    for f in fills:
        t = f.get("ticker") or f.get("market_ticker")
        if not t or not t.startswith(NFL):
            continue
        side = f.get("outcome_side") or f.get("side")
        n = _f(f.get("count_fp") or f.get("count"))
        price = _f(f.get(f"{side}_price_dollars"))
        p = held[t]
        p[side] += n
        p[f"cost_{side}"] += n * price
        p["fees"] += _f(f.get("fee_cost"))
        when = f.get("created_time") or ""
        p["first"] = min(p["first"] or when, when)
    settled = {s["ticker"]: s for s in settlements if s.get("ticker", "").startswith(NFL)}

    rows = []
    for t, p in held.items():
        code, kind, yes_text, no_text = describe(t)
        week, game = find_game(code, sched)
        side = "yes" if p["yes"] >= p["no"] else "no"
        count = p[side] - p["no" if side == "yes" else "yes"]          # net exposure
        s = settled.get(t)
        if s:                                                           # Kalshi's own numbers
            cost = _f(s.get("yes_total_cost_dollars")) + _f(s.get("no_total_cost_dollars")) + _f(s.get("fee_cost"))
            payout = _f(s.get("revenue")) / 100
            pnl = payout - cost
            result = "P" if abs(pnl) < 0.005 else ("W" if pnl > 0 else "L")
        else:
            cost = p["cost_yes"] + p["cost_no"] + p["fees"]
            pnl, result = None, "open"
        avg = p[f"cost_{side}"] / p[side] if p[side] else None
        rows.append(dict(ticker=t, week=week, game=game, kind=kind, side=side,
                         bet=f"{game}: {yes_text if side == 'yes' else no_text} ({kind})" if game else t,
                         contracts=round(count, 2), price_cents=None if avg is None else round(avg * 100),
                         cost_dollars=round(cost, 2), pnl_dollars=None if pnl is None else round(pnl, 2),
                         stake=round(cost / unit, 2), pnl=None if pnl is None else round(pnl / unit, 2),
                         result=result, first_fill=p["first"]))
    return sorted(rows, key=lambda r: r["first_fill"] or "")


def main():
    unit = _f(os.environ.get("MY_UNIT_DOLLARS"), 0)
    if unit <= 0:
        raise SystemExit("set MY_UNIT_DOLLARS in ~/.zshrc: what 1 unit is for you, in dollars")
    key = _key()
    fills = every("/portfolio/fills", "fills", key)
    settlements = every("/portfolio/settlements", "settlements", key)
    rows = positions(fills, settlements, season_schedule(), unit)
    json.dump(rows, open(OUT, "w"), indent=1)
    nfl = [r for r in rows if r["week"]]
    done = [r for r in nfl if r["result"] != "open"]
    print(f"{len(fills)} fills, {len(settlements)} settlements -> {len(nfl)} NFL bets "
          f"({len(done)} settled, {sum(r['pnl_dollars'] for r in done):+.2f} dollars) -> {os.path.relpath(OUT)}")


if __name__ == "__main__":
    main()
