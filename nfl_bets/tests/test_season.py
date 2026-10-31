"""Offline tests for season.py: pricing both sides, the time discount, one bet per team, grading."""

import numpy as np
import pandas as pd
import pytest

import season
from common import kalshi_fee


def _dist(mean_wins):
    p = np.zeros(18)
    p[mean_wins] = 1.0
    return p


def test_prices_both_sides_with_fee_and_discount():
    dist = {"LAR": np.r_[np.zeros(8), [0.5, 0.5], np.zeros(8)]}           # 8 or 9 wins, 50/50
    mk = pd.DataFrame([dict(kind="wins", ticker="KXNFLWINS-27LA-9", team="LAR", n=9, bid=0.40, ask=0.42, volume=100)])
    cond = pd.DataFrame(dict(team=["LAR"], made_playoffs_unconditional=[30.0]))
    now = pd.Timestamp("2026-10-28", tz="UTC")
    s = season.price_sides(mk, dist, cond, now, rate=0.035).set_index("side")
    assert s.loc["yes", "p"] == pytest.approx(0.5)
    assert s.loc["yes", "cost"] == pytest.approx(0.42 + kalshi_fee(0.42), abs=1e-4)
    assert s.loc["no", "price"] == pytest.approx(0.60)
    assert s.loc["yes", "ev"] == pytest.approx(0.5 / (0.42 + kalshi_fee(0.42)) - 1, abs=1e-4)
    t = s.loc["yes", "years"]
    assert 0.2 < t < 0.25                                                    # ~11.5 weeks to Jan 18
    assert s.loc["yes", "ev_adj"] == pytest.approx((1 + s.loc["yes", "ev"]) / 1.035 ** t - 1, abs=1e-3)
    assert s.loc["yes", "ev_adj"] < s.loc["yes", "ev"]


def test_playoff_market_uses_elway_playoff_odds():
    mk = pd.DataFrame([dict(kind="playoffs", ticker="KXNFLPLAYOFF-27-CHI", team="CHI", n=None, bid=0.70, ask=0.72, volume=1)])
    cond = pd.DataFrame(dict(team=["CHI"], made_playoffs_unconditional=[61.3]))
    s = season.price_sides(mk, {"CHI": _dist(10)}, cond, pd.Timestamp("2026-10-28", tz="UTC")).set_index("side")
    assert s.loc["yes", "p"] == pytest.approx(0.613)
    assert s.loc["no", "p"] == pytest.approx(0.387)
    assert s.loc["no", "bet"] == "CHI misses the playoffs"


def test_one_bet_per_team_in_price_range():
    sides = pd.DataFrame([
        dict(team="LAR", price=0.05, ev_adj=0.9, bet="long shot"),               # out of range
        dict(team="LAR", price=0.52, ev_adj=0.30, bet="LAR misses the playoffs"),
        dict(team="LAR", price=0.20, ev_adj=0.50, bet="LAR not 8+ wins"),
        dict(team="KC", price=0.50, ev_adj=0.05, bet="KC 12+ wins"),
    ])
    b = season.best_per_team(sides, min_ev=0.10).set_index("team")
    assert len(b) == 2
    assert b.loc["LAR", "bet"] == "LAR not 8+ wins" and b.loc["LAR", "recommended"]
    assert not b.loc["KC", "recommended"]


def test_grade_wins_and_playoffs(tmp_path, monkeypatch):
    log = pd.DataFrame([
        dict(kind="wins", team="LAR", n=8, side="no", cost=0.21, stake=1.0, result=None, pnl=None),
        dict(kind="wins", team="MIA", n=5, side="yes", cost=0.26, stake=1.0, result=None, pnl=None),
        dict(kind="playoffs", team="CHI", n=None, side="no", cost=0.29, stake=1.0, result=None, pnl=None),
        dict(kind="playoffs", team="NYG", n=None, side="yes", cost=0.45, stake=1.0, result=None, pnl=None),
    ])
    path = tmp_path / "season_log.csv"
    log.to_csv(path, index=False)
    monkeypatch.setattr(season, "LOG", str(path))
    monkeypatch.setattr(season, "final_wins", lambda season=None: {"LAR": 7, "MIA": 4})
    monkeypatch.setattr(season, "playoff_field", lambda season=None: {"CHI"} | {f"T{i}" for i in range(13)})
    season.grade()
    g = pd.read_csv(path).set_index("team")
    assert g.loc["LAR", "result"] == "W" and g.loc["LAR", "pnl"] == pytest.approx(1 / 0.21 - 1, abs=1e-3)
    assert g.loc["MIA", "result"] == "L" and g.loc["MIA", "pnl"] == -1
    assert g.loc["CHI", "result"] == "L"
    assert g.loc["NYG", "result"] == "L"


def test_grade_waits_for_the_season(tmp_path, monkeypatch):
    path = tmp_path / "season_log.csv"
    pd.DataFrame([dict(kind="wins", team="LAR", n=8, side="no", cost=0.21, stake=1.0, result=None, pnl=None),
                  dict(kind="playoffs", team="CHI", n=None, side="no", cost=0.29, stake=1.0, result=None, pnl=None)]
                 ).to_csv(path, index=False)
    monkeypatch.setattr(season, "LOG", str(path))
    monkeypatch.setattr(season, "final_wins", lambda season=None: {})
    monkeypatch.setattr(season, "playoff_field", lambda season=None: None)
    season.grade()
    assert pd.read_csv(path).result.isna().all()
