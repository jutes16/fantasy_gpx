"""Offline tests for the pool model's pure functions (no data files needed)."""

import numpy as np
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
    assert (r.an_line, r.an_home_bets_pct, r.an_home_money_pct) == (-7.5, 60, 80)
    # openingLine is the lookahead; with no line history there's no game-week open
    assert r.an_open_lookahead == -7.0 and np.isnan(r.an_open_line)
    assert canon("LA") == "LAR"


def test_factor_signals_are_from_the_picks_side():
    from pool import _signals
    g = dict(home="DEN", away="LAR", my_line=2.5, mkt_line=-1.5, open_line=3.0,
             home_bets_pct=41.0, home_money_pct=61.0, sharp_side="home", sharp_type="book",
             elway_line=-4.5)
    h = _signals(g, "home")          # picking DEN (home)
    assert h["move"] == pytest.approx(4.5)        # market moved 4.5 toward DEN
    assert (h["tix"], h["money"]) == (41.0, 61.0)
    assert h["sharp"] == "with" and h["sharp_type"] == "book"
    assert h["elway"] == pytest.approx(7.0)       # ELWAY likes DEN 7 pts more than your line
    a = _signals(g, "away")          # same game, picking LAR
    assert a["move"] == pytest.approx(-4.5)
    assert (a["tix"], a["money"]) == (59.0, 39.0)
    assert a["sharp"] == "AGAINST"
    assert a["elway"] == pytest.approx(-7.0)
    assert _signals(dict(home="A", away="B", my_line=1.0, mkt_line=1.0), "home")["sharp"] is None


def test_score_game_reports_its_components():
    s = score_game(dict(away="LAR", home="DEN", my_line=2.5, mkt_line=-1.5,
                        proj_margin=4.5, proj_weight=0.5))
    assert s["win_rate"] == pytest.approx(s["base_rate"] + s["proj_adj"], abs=1e-4)
    assert s["proj_adj"] > 0


def test_game_week_open_uses_line_history_not_lookahead():
    from fetch_splits import _week_open, parse
    g = {"awayTeam": {"abbreviation": "PHI"}, "homeTeam": {"abbreviation": "CHI"},
         "startTime": "2026-09-29T00:15:00Z",
         "consensus": {"spread": {"sides": [{"side": "home", "line": 3.5}]}},
         "lineMovement": {"spread": [{"side": "home", "openingLine": -1.5}]},   # July lookahead
         "lineMovementHistory": [{"market": "spread", "side": "home", "bookName": "Consensus",
                                  "history": [{"recordedAt": "2026-07-06T11:00:00Z", "line": -1.5},
                                              {"recordedAt": "2026-09-21T12:00:00Z", "line": 3.0},
                                              {"recordedAt": "2026-09-25T12:00:00Z", "line": 3.5}]}]}
    assert _week_open(g) == pytest.approx(3.0)      # standing line 7 days before kickoff
    r = parse([g], 2026, 3).iloc[0]
    assert r.an_open_line == pytest.approx(3.0) and r.an_open_lookahead == pytest.approx(-1.5)
    g.pop("lineMovementHistory")
    assert np.isnan(parse([g], 2026, 3).iloc[0].an_open_line)   # no history -> no open, not the lookahead
