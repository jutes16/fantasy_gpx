"""Offline tests for the pool model's pure functions (no data files needed)."""

import pytest

from fetch_splits import canon, parse as parse_splits
from import_elway import parse as parse_elway
from pool import best_five, score_game


def test_score_game_takes_the_side_with_value():
    # pool sheet gives the home team +3.5, market has them +2.5 -> home has 1 pt of value
    s = score_game(dict(away="KC", home="MIA", my_line=3.5, mkt_line=2.5))
    assert s["side"] == "home"
    assert s["pts_value"] == pytest.approx(1.0)
    assert 0.5 < s["win_rate"] < 0.7


def test_score_game_no_value_is_a_coin_flip():
    s = score_game(dict(away="KC", home="MIA", my_line=-3.0, mkt_line=-3.0))
    assert s["pts_value"] == pytest.approx(0.0)
    assert s["win_rate"] == pytest.approx(0.5, abs=0.01)


def test_best_five_returns_five_picks():
    games = [dict(away=f"A{i}", home=f"H{i}", my_line=float(i % 4) - 1.5,
                  mkt_line=float(i % 3) - 1.0) for i in range(8)]
    res = best_five(games)
    assert len(res["card"]) == 5


def test_parse_elway_paste():
    text = """3
    N
    DAL
    24.6	39.7%
    BAL
    28.0	59.8%	+3	52
    3
    NO
    23.9	60.8%
    LV
    20.5	38.8%	-3	44"""
    d = parse_elway(text)
    assert len(d) == 2
    assert d.iloc[0][["home", "away", "elway_line", "elway_total"]].tolist() == ["DAL", "BAL", 3.0, 52.0]
    assert d.iloc[1].elway_home_wp == pytest.approx(0.608)


def test_parse_action_network_item():
    items = [{"awayTeam": {"abbreviation": "DAL"}, "homeTeam": {"abbreviation": "PHI"},
              "consensus": {"spread": {"sides": [
                  {"side": "away", "line": 7.5, "ticketPercent": 40, "moneyPercent": 20},
                  {"side": "home", "line": -7.5, "ticketPercent": 60, "moneyPercent": 80}]}},
              "lineMovement": {"spread": [{"side": "home", "openingLine": -7.0}]}},
             {"bad": 1}]
    d = parse_splits(items, 2025, 1)
    assert len(d) == 1
    r = d.iloc[0]
    assert (r.an_line, r.an_open_line, r.an_home_bets_pct, r.an_home_money_pct) == (-7.5, -7.0, 60, 80)
    assert canon("LA") == "LAR"
