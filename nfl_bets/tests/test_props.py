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


def test_kalshi_quotes_become_offers_at_the_books_lines():
    import pandas as pd
    from common import kalshi_fee
    off = pd.DataFrame([dict(player="Jake Ferguson", away="DAL", home="ARI", kickoff="2026-11-01T17:00:00Z", fetched_at="",
                             market="player_receptions", book="FanDuel", line=1.5, p=0.63, prices={"over": -188, "under": 150})])
    q = pd.DataFrame([
        dict(fetched_at="t", ticker="A", market="player_receptions", player="Jake Ferguson", key="jakeferguson", line=1.5, bid=0.62, ask=0.65, volume=0),
        dict(fetched_at="t", ticker="B", market="player_receptions", player="Jake Ferguson", key="jakeferguson", line=2.5, bid=0.35, ask=0.37, volume=0),  # no book quotes 2.5
        dict(fetched_at="t", ticker="C", market="player_receptions", player="Jake Ferguson", key="jakeferguson", line=1.5, bid=0.10, ask=0.90, volume=0),  # one-sided
    ])
    k = props.kalshi_offers(q, off)
    assert len(k) == 1 and k.iloc[0].book == "Kalshi" and k.iloc[0].line == 1.5
    over, under = k.iloc[0].prices["over"], k.iloc[0].prices["under"]
    assert 1 / props.american_to_decimal(over) == pytest.approx(0.65 + kalshi_fee(0.65), abs=0.003)
    assert 1 / props.american_to_decimal(under) == pytest.approx(0.38 + kalshi_fee(0.38), abs=0.003)


def test_kalshi_left_out_of_the_fair_price():
    import pandas as pd
    rows = [dict(player="P", away="A", home="H", kickoff="k", fetched_at="", market="player_receptions", book=b,
                 line=4.5, p=0.5, prices={"over": -110, "under": -110}) for b in ("B1", "B2", "B3")]
    rows.append(dict(player="P", away="A", home="H", kickoff="k", fetched_at="", market="player_receptions", book="Kalshi",
                     line=4.5, p=0.9, prices={"over": 100, "under": -400}))
    slp = pd.DataFrame([dict(key="p", name="P", pos="WR", team="H", injury="", pass_yd=0, pass_td=0, rush_yd=0, rec=4.6, rec_yd=50, td=0.3)])
    d = props.build_sheet(pd.DataFrame(rows), slp).set_index("side")
    assert d.loc["over", "book"] == "Kalshi" and d.loc["over", "price"] == 100       # best price wins
    assert d.loc["over", "books"] == 3                                               # sportsbooks only
    assert d.loc["over", "p_fair"] == pytest.approx(0.5, abs=0.01)                   # fair unaffected by Kalshi's 0.9
