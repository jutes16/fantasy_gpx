"""Offline tests for elway_pull.py (the feed -> bets.py's files) and bets.import_sheets (the cloud's sheets -> the log)."""

import pandas as pd
import pytest

import bets
import elway_pull
from common import SEASON, load_dists, parse_elway


def _feed(games=(("CAR", "GB"), ("ARI", "DAL")), week=8):
    g = pd.DataFrame([dict(week=week, home_sb_fran_id=h, pf_home=27.56, win_home=69.4, away_sb_fran_id=a, pf_away=20.96,
                           win_away=30.13, point_spread=-6, total=48, neutral="No", extra=0) for a, h in games]
                     + [dict(week=week + 1, home_sb_fran_id="KC", pf_home=28.2, win_home=83.5, away_sb_fran_id="NYJ",
                             pf_away=15.8, win_away=16.2, point_spread=-12, total=43.5, neutral="No", extra=0)])
    rows = []
    for a, h in games:
        rows += [dict(matchup=f"{a} @ {h}", margin=-3, away_prob=30.0, home_prob=None, tie_prob=None),
                 dict(matchup=f"{a} @ {h}", margin=0, away_prob=None, home_prob=None, tie_prob=0.5),
                 dict(matchup=f"{a} @ {h}", margin=7, away_prob=None, home_prob=69.5, tie_prob=None)]
    return g, pd.DataFrame(rows)


def test_build_finds_the_week_and_reads_each_margin_column():
    wk, g, dist = elway_pull.build(*_feed())
    assert wk == 8 and len(g) == 2
    d = dist[(dist.away == "ARI") & (dist.home == "DAL")].set_index("home_margin").prob
    assert d[-3] == pytest.approx(0.30) and d[0] == pytest.approx(0.005) and d[7] == pytest.approx(0.695)
    e = parse_elway("\n".join(elway_pull.table_lines(g)))
    assert set(e.home) == {"GB", "DAL"} and (e.elway_line == -6).all() and e.elway_home_wp.iloc[0] == pytest.approx(0.694)


def test_build_refuses_a_week_the_feed_is_not_on():
    with pytest.raises(SystemExit):
        elway_pull.build(*_feed(), week=9)


def test_pull_keeps_played_games_and_is_unchanged_on_a_repeat(tmp_path, monkeypatch):
    out, pool = tmp_path / "elway", tmp_path / "pool"
    monkeypatch.setattr(elway_pull, "fetch", lambda: (*_feed(), "2026-10-27T17:43:11"))
    assert elway_pull.pull(out=str(out), pool_out=str(pool)) == (8, True)
    assert elway_pull.pull(out=str(out), pool_out=str(pool)) == (8, False)
    monkeypatch.setattr(elway_pull, "fetch", lambda: (*_feed(games=(("ARI", "DAL"),)), "2026-10-30T20:27:21"))
    assert elway_pull.pull(out=str(out), pool_out=str(pool)) == (8, True)     # TNF dropped from the feed
    txt = open(out / f"{SEASON}_wk08.txt").read()
    assert "2026-10-30" in txt and len(parse_elway(txt)) == 2                  # its row kept
    assert open(pool / f"{SEASON}_wk08.txt").read() == txt
    monkeypatch.setattr("common.ELWAY_DIRS", [str(out)])
    assert set(load_dists(8)) == {("CAR", "GB"), ("ARI", "DAL")}


def test_import_sheets_logs_each_cloud_sheet_once(tmp_path, monkeypatch):
    log = tmp_path / "bet_log.csv"
    monkeypatch.setattr(bets, "LOG", str(log))
    sheets = tmp_path / "sheets"
    sheets.mkdir()
    row = dict(season=SEASON, week=9, game="NYJ @ KC", kickoff="2026-11-08T18:00:00Z", instrument="kalshi_win",
               team="KC", side="yes", ev=0.08, recommended=True)
    pd.DataFrame([dict(row, elway_fp="aaa", logged_at="2026-11-04T15:00:00Z")]).to_csv(sheets / f"bets_{SEASON}_wk09_1.csv", index=False)
    pd.DataFrame([dict(row, elway_fp="aaa", logged_at="2026-11-05T15:00:00Z")]).to_csv(sheets / f"bets_{SEASON}_wk09_2.csv", index=False)
    pd.DataFrame([dict(row, elway_fp="bbb", logged_at="2026-11-06T15:00:00Z")]).to_csv(sheets / f"bets_{SEASON}_wk09_3.csv", index=False)
    bets.import_sheets(str(sheets))
    bets.import_sheets(str(sheets))                                           # a second sync adds nothing
    g = pd.read_csv(log)
    assert list(g.batch) == [1, 2]                                            # the unchanged-ELWAY sheet is skipped
    assert list(g.logged_at) == ["2026-11-04T15:00:00Z", "2026-11-06T15:00:00Z"]
    assert list(g.elway_fp) == ["aaa", "bbb"]
