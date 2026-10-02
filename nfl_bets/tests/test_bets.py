"""Offline tests for nfl_bets: prices, settlement, margin models.

No network and no local-only data: history is injected synthetically."""

import numpy as np
import pandas as pd
import pytest

import bets
import common
from common import (EmpiricalMargin, HistoricalMargin, Margin, buy_no, buy_yes,
                    canon, kalshi_fee, parse_elway)


# ------------------------------------------------------------------ prices
def test_kalshi_fee_and_costs():
    assert kalshi_fee(0.5) == pytest.approx(0.0175)
    assert buy_yes(0.19) == pytest.approx(0.19 + 0.07 * 0.19 * 0.81)
    assert buy_no(0.57) == pytest.approx(0.43 + 0.07 * 0.43 * 0.57)
    assert bets.sell_net(0.37) == pytest.approx(0.37 - 0.07 * 0.37 * 0.63)


def test_pair_reproduces_published_ari_example():
    """ELWAY/Kalshi week-1 writeup: ARI +6.5, buy win at 19c, sell spread at 37c."""
    nb, ns = 1 / buy_yes(0.19), 1 / bets.sell_net(1 - 0.63)
    assert 0.36 * nb - 0.582 * ns == pytest.approx(0.15, abs=0.005)     # EV under ELWAY
    assert 0.185 * nb - 0.375 * ns == pytest.approx(-0.14, abs=0.005)   # EV under market
    assert nb - ns == pytest.approx(2.17, abs=0.03)                     # ARI wins
    assert -ns == pytest.approx(-2.83, abs=0.01)                        # loses by 1-6


# ------------------------------------------------------------------ settlement
B = lambda **k: pd.Series(k)
NB, NS = 1 / buy_yes(0.19), 1 / bets.sell_net(0.37)


@pytest.mark.parametrize("bet, margin, pnl, result", [
    (B(instrument="kalshi_win", cost=0.5), 3, 1.0, "W"),
    (B(instrument="kalshi_win", cost=0.5), 0, -1.0, "L"),          # tie settles NO
    (B(instrument="book_ml", cost=0.4), 0, 0.0, "P"),              # tie pushes
    (B(instrument="book_ml", cost=0.4), -7, -1.0, "L"),
    (B(instrument="book_spread", strike=3.5, cost=0.5), -3, 1.0, "W"),
    (B(instrument="book_spread", strike=-3.0, cost=0.5), 3, 0.0, "P"),
    (B(instrument="kalshi_spread", side="yes", strike=6.5, cost=0.5), 7, 1.0, "W"),
    (B(instrument="kalshi_spread", side="no", strike=6.5, cost=0.5), 6, 1.0, "W"),
    (B(instrument="pair", side="buy win, sell spread", strike=6.5, n_buy=NB, n_sell=NS), 12, NB - NS, "W"),
    (B(instrument="pair", side="buy win, sell spread", strike=6.5, n_buy=NB, n_sell=NS), -3, -NS, "L"),
    (B(instrument="pair", side="buy win, sell spread", strike=6.5, n_buy=NB, n_sell=NS), -10, 0.0, "0"),
    (B(instrument="pair", side="buy spread, sell win", strike=3.5, n_buy=NB, n_sell=NS), 2, -NS, "L"),
])
def test_settle(bet, margin, pnl, result):
    got = bets._settle(bet, margin)
    assert got[0] == pytest.approx(pnl)
    assert got[1] == result


# ------------------------------------------------------------------ margin models
def test_empirical_margin_probabilities():
    d = {-3: 0.2, 0: 0.1, 3: 0.4, 7: 0.3}         # home margins
    m = EmpiricalMargin(d)
    assert m.home_wp == pytest.approx(0.7)        # ties are not wins
    assert m.away_wp == pytest.approx(0.2)
    assert m.p_home_by_over(2.5) == pytest.approx(0.7)
    assert m.p_home_by_over(3.5) == pytest.approx(0.3)
    assert m.p_team_by_over(False, 2.5) == pytest.approx(0.2)   # away wins by 3+
    assert m.p_team_wins(False) == pytest.approx(0.2)


def test_normal_margin_matches_win_prob():
    m = Margin(-3.0, 0.60)
    assert m.p_team_wins(True) == pytest.approx(0.60)
    assert m.p_home_by_over(-100) == pytest.approx(1.0)


@pytest.fixture
def fake_history(monkeypatch):
    """Synthetic history: margins ~ spread + noise, with extra mass on +/-3 and +/-7."""
    rng = np.random.default_rng(0)
    spreads = rng.choice(np.arange(-14, 14.5, 0.5), 20000)
    noise = rng.normal(0, 13, spreads.size)
    margins = np.round(spreads + noise).astype(int)
    key = rng.random(spreads.size) < 0.15
    margins[key] = np.sign(margins[key] + 0.1) * rng.choice([3, 7], key.sum())
    monkeypatch.setitem(common._HIST, (2015, common.SEASON - 1), (spreads, margins))


def test_historical_margin_centred_on_win_prob(fake_history):
    for line, wp in ((-3.0, 0.60), (7.0, 0.30), (-10.5, 0.80)):
        m = HistoricalMargin(line, wp)
        assert m.home_wp == pytest.approx(wp, abs=0.002)
        assert m.kind == "hist"


def test_historical_margin_keeps_key_numbers(fake_history):
    m = HistoricalMargin(-3.0, 0.60)
    crossing_3 = m.p_home_by_over(2.5) - m.p_home_by_over(3.5)
    crossing_4 = m.p_home_by_over(3.5) - m.p_home_by_over(4.5)
    assert crossing_3 > 1.5 * crossing_4          # the spike at 3 survives


# ------------------------------------------------------------------ parsing
def test_parse_elway_and_team_codes():
    text = """Wk Home Avg. pts. Win prob. Away Avg. pts. Win prob. Home spread Total
    4 CLE 20.1 50.63% PIT 19.9 48.89% -0.5 40
    4 N WAS 22.3 43.77% IND 24.2 55.63% +2.5 46.5
    4 JAC 21.0 40.0% LA 24.0 59.0% +3 45"""
    e = parse_elway(text)
    assert len(e) == 3
    assert e.iloc[0][["home", "away", "elway_line"]].tolist() == ["CLE", "PIT", -0.5]
    assert e.iloc[1].elway_home_wp == pytest.approx(0.4377)
    assert e.iloc[2][["home", "away"]].tolist() == ["JAX", "LAR"]
    assert canon("wsh") == "WAS"


# ------------------------------------------------------------------ best price (odds_api)
def test_odds_api_tidy_stores_spreads_as_the_home_line(monkeypatch):
    import odds_api
    sched = pd.DataFrame(dict(away=["LAR"], home=["PHI"]))
    monkeypatch.setattr(odds_api, "schedule", lambda *a, **k: sched)
    raw = [{"home_team": "Philadelphia Eagles", "away_team": "Los Angeles Rams", "bookmakers": [
        {"title": "BookA", "markets": [
            {"key": "spreads", "outcomes": [{"name": "Philadelphia Eagles", "price": -104, "point": 3.5},
                                            {"name": "Los Angeles Rams", "price": -115, "point": -3.5}]},
            {"key": "h2h", "outcomes": [{"name": "Philadelphia Eagles", "price": 165},
                                        {"name": "Los Angeles Rams", "price": -190}]}]}]}]
    d = odds_api.tidy(raw, 4)
    sp = d[d.market == "spreads"].set_index("side")
    assert sp.at["home", "point"] == 3.5 and sp.at["away", "point"] == 3.5   # both as PHI's line
    assert set(d[d.market == "h2h"].side) == {"home", "away"}


def test_book_offers_pick_from_every_book():
    books = pd.DataFrame([
        dict(away="LAR", home="PHI", book="A", market="h2h", side="home", point=None, price=150),
        dict(away="LAR", home="PHI", book="B", market="h2h", side="home", point=None, price=165),
        dict(away="LAR", home="PHI", book="A", market="spreads", side="away", point=3.0, price=-110),
        dict(away="LAR", home="PHI", book="B", market="spreads", side="away", point=2.5, price=-105),
    ])
    r = pd.Series(dict(away="LAR", home="PHI"))
    ml, sp = bets._book_offers(r, books)
    assert sorted(ml["home"]) == [("A", 150), ("B", 165)]
    # away offers are converted to the away team's own number
    assert sorted(sp["away"]) == [("A", -3.0, -110), ("B", -2.5, -105)]
