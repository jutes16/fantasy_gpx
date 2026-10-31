"""Offline tests for kalshi_mine.py: request signing, tickers, and netting fills into bets."""

import base64

import pandas as pd
import pytest

import kalshi_mine as km


@pytest.mark.parametrize("kind", ["ed25519", "rsa"])
def test_signature_verifies(kind):
    from cryptography.hazmat.primitives import hashes
    from cryptography.hazmat.primitives.asymmetric import ed25519, padding, rsa
    pk = ed25519.Ed25519PrivateKey.generate() if kind == "ed25519" else rsa.generate_private_key(65537, 2048)
    msg = b"1700000000000GET/trade-api/v2/portfolio/fills"
    sig = km._sign(pk, msg)
    if kind == "ed25519":
        pk.public_key().verify(sig, msg)
    else:
        pk.public_key().verify(sig, msg, padding.PSS(mgf=padding.MGF1(hashes.SHA256()),
                                                     salt_length=padding.PSS.DIGEST_LENGTH), hashes.SHA256())
    assert base64.b64encode(sig)


def test_describe_tickers():
    assert km.describe("KXNFLSPREAD-26OCT25GBDET-DET7")[1:] == ("Kalshi spread", "DET wins by over 6.5",
                                                                 "DET doesn't win by over 6.5")
    assert km.describe("KXNFLGAME-26OCT25MIANYJ-MIA")[2] == "MIA to win"
    assert km.describe("KXNFLTOTAL-26OCT18CARPHI-43")[2:] == ("over 42.5 points", "under 42.5 points")


def test_positions_net_and_settle():
    sched = pd.DataFrame(dict(gameday=["2026-10-25"], week=[7], away=["GB"], home=["DET"]))
    fills = [  # 10 YES at 38c, then sell 4 back (= 4 NO at 60c): net 6 YES
        dict(ticker="KXNFLSPREAD-26OCT25GBDET-DET7", outcome_side="yes", count_fp="10.00",
             yes_price_dollars="0.3800", no_price_dollars="0.6200", fee_cost="0.17", created_time="2026-10-23T15:00:00Z"),
        dict(ticker="KXNFLSPREAD-26OCT25GBDET-DET7", outcome_side="no", count_fp="4.00",
             yes_price_dollars="0.4000", no_price_dollars="0.6000", fee_cost="0.07", created_time="2026-10-24T15:00:00Z"),
        dict(ticker="KXNBAGAME-26OCT25X-Y", outcome_side="yes", count_fp="1.00", yes_price_dollars="0.5"),  # not NFL
    ]
    open_ = km.positions(fills, [], sched, unit=10)
    assert len(open_) == 1 and open_[0]["result"] == "open"
    b = open_[0]
    assert (b["week"], b["game"], b["side"], b["contracts"], b["price_cents"]) == (7, "GB @ DET", "yes", 6.0, 38)
    assert b["cost_dollars"] == pytest.approx(3.80 + 2.40 + 0.24)
    # settled YES: Kalshi's cost basis and revenue decide P&L
    sett = [dict(ticker="KXNFLSPREAD-26OCT25GBDET-DET7", market_result="yes", yes_total_cost_dollars="3.8000",
                 no_total_cost_dollars="2.4000", fee_cost="0.2400", revenue=1000)]
    done = km.positions(fills, sett, sched, unit=10)[0]
    assert done["result"] == "W" and done["pnl_dollars"] == pytest.approx(10 - 6.44)
    assert done["pnl"] == pytest.approx((10 - 6.44) / 10, abs=0.01)


def test_season_markets_filed_under_the_week_bought():
    assert km.describe("KXNFLWINS-27LAR-8")[1:] == ("Kalshi season wins", "LAR 8+ wins", "LAR under 8 wins")
    assert km.describe("KXNFLWINS-27JAC-10")[2] == "JAX 10+ wins"
    assert km.describe("KXNFLPLAYOFF-27-CHI")[1:] == ("Kalshi playoffs", "CHI makes the playoffs", "CHI misses the playoffs")
    sched = pd.DataFrame(dict(gameday=["2026-10-26", "2026-11-02"], week=[7, 8], away=["GB", "CHI"], home=["DET", "SEA"]))
    fills = [
        dict(ticker="KXNFLWINS-27LAR-8", outcome_side="no", count_fp="50.00", yes_price_dollars="0.8000",
             no_price_dollars="0.2000", fee_cost="0.56", created_time="2026-10-28T21:00:00Z"),
        dict(ticker="KXNFLPLAYOFF-27-CHI", outcome_side="no", count_fp="10.00", yes_price_dollars="0.7200",
             no_price_dollars="0.2800", fee_cost="0.15", created_time="2026-10-29T12:00:00Z"),
        dict(ticker="KXNFLWINS-ANY-27-17", outcome_side="yes", count_fp="1.00", yes_price_dollars="0.02"),   # league-wide: skipped
    ]
    rows = {r["ticker"]: r for r in km.positions(fills, [], sched, unit=10)}
    assert set(rows) == {"KXNFLWINS-27LAR-8", "KXNFLPLAYOFF-27-CHI"}
    lar = rows["KXNFLWINS-27LAR-8"]
    assert (lar["week"], lar["game"], lar["side"], lar["result"]) == (8, "LAR (season)", "no", "open")
    assert lar["bet"] == "LAR (season): LAR under 8 wins (Kalshi season wins)"
    assert lar["cost_dollars"] == pytest.approx(10.56)
