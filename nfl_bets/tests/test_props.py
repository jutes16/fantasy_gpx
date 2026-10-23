"""Offline tests for props.py: probabilities invert the implied means, settlement."""

import pytest

import props
from props import fp       # ../nfl_fantasy/fantasy_props.py, path set by props


@pytest.mark.parametrize("market,line,p,qb", [
    ("player_pass_yds", 229.5, 0.55, False),
    ("player_rush_yds", 54.5, 0.45, False),
    ("player_rush_yds", 24.5, 0.60, True),
    ("player_reception_yds", 41.5, 0.52, False),
    ("player_receptions", 4.5, 0.48, False),
    ("player_pass_tds", 1.5, 0.40, False),
    ("player_anytime_td", 0.5, 0.35, False),
])
def test_p_side_inverts_implied_mean(market, line, p, qb):
    """A projection equal to the market's mean gives back the market's probability."""
    mean = fp.implied_mean(market, line, p, qb=qb)
    over = "yes" if market == "player_anytime_td" else "over"
    assert props.p_side(market, mean, line, over, qb)[0] == pytest.approx(p, abs=1e-6)

def test_counts_use_measured_dispersion():
    """Receptions are wider than Poisson (more zeros), passing TDs narrower."""
    from scipy import stats
    phi = props.shapes.load()["counts"]
    assert phi["player_receptions"] > 1 > phi["player_pass_tds"]
    assert props.shapes.count_cdf(0, 3.0, phi["player_receptions"]) > stats.poisson.cdf(0, 3.0)
    assert props.shapes.count_cdf(0, 1.5, phi["player_pass_tds"]) < stats.poisson.cdf(0, 1.5)
    for m in (0.3, 1.5, 4.0):          # the narrowed (binomial) count keeps its mean
        k = range(25)
        assert sum(i * props.shapes.count_pmf(i, m, 0.79) for i in k) == pytest.approx(m, rel=1e-6)


def test_low_volume_yards_are_skewed():
    """A 15.5 receiving line implies a mean well above it; a 75.5 line barely."""
    lo = fp.implied_mean("player_reception_yds", 15.5, 0.5) / 15.5
    hi = fp.implied_mean("player_reception_yds", 75.5, 0.5) / 75.5
    assert lo > 1.2 and 1.0 < hi < 1.15


def test_whole_number_line_pushes():
    w, push = props.p_side("player_receptions", 5.0, 5.0, "over")
    l, _ = props.p_side("player_receptions", 5.0, 5.0, "under")
    assert push > 0 and w + l + push == pytest.approx(1)

def test_ev_and_settle():
    assert props.ev(0.5, 0, -110) == pytest.approx(0.5 * 100 / 110 - 0.5)
    assert props.ev(0.5, 0, +100) == pytest.approx(0)
    assert props.settle("player_rush_yds", "over", 54.5, 60) == "win"
    assert props.settle("player_rush_yds", "under", 54.5, 60) == "loss"
    assert props.settle("player_receptions", "over", 5.0, 5) == "push"
    assert props.settle("player_anytime_td", "yes", 0.5, 1) == "win"
    assert props.settle("player_anytime_td", "yes", 0.5, 0) == "loss"


def test_snapshot_window():
    """Only games after now and within the window are snapshotted."""
    import pandas as pd
    import snapshot
    now = pd.Timestamp("2026-10-25T15:00:00Z")
    sched = pd.DataFrame(dict(away=["A", "B", "C", "D"], home=["E", "F", "G", "H"],
                              kickoff=pd.to_datetime(["2026-10-25T13:30:00Z", "2026-10-25T17:00:00Z",
                                                      "2026-10-25T17:25:00Z", "2026-10-26T00:20:00Z"])))
    assert list(snapshot.kicking_off(sched, now, 2.5).away) == ["B", "C"]
