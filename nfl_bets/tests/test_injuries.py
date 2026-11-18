"""Offline tests for the injury check: synthetic reports and snap counts, no network."""

import pandas as pd

import injuries


def _data():
    inj = pd.DataFrame([
        # week 3 report: BAL's starting QB and a backup out; DEN missing three starters, one a part-timer
        dict(season=2026, week=3, team="BAL", gsis_id="qb1", position="QB", full_name="Starter QB", report_status="Out"),
        dict(season=2026, week=3, team="BAL", gsis_id="qb2", position="QB", full_name="Backup QB", report_status="Out"),
        dict(season=2026, week=3, team="DEN", gsis_id="d1", position="CB", full_name="Corner", report_status="Out"),
        dict(season=2026, week=3, team="DEN", gsis_id="d2", position="T", full_name="Tackle", report_status="Doubtful"),
        dict(season=2026, week=3, team="DEN", gsis_id="d3", position="LB", full_name="Linebacker", report_status="Out"),
        dict(season=2026, week=3, team="DEN", gsis_id="d4", position="WR", full_name="Part-timer", report_status="Out"),
        dict(season=2026, week=3, team="KC", gsis_id="k1", position="WR", full_name="Questionable WR", report_status="Questionable"),
    ])
    snaps = pd.DataFrame([
        dict(season=2026, week=w, pfr_player_id=p, offense_pct=o, defense_pct=d)
        for w in (1, 2) for p, o, d in [("QB1", 1.0, 0), ("QB2", 0.05, 0), ("D1", 0, 0.9), ("D2", 1.0, 0),
                                        ("D3", 0, 0.7), ("D4", 0.3, 0), ("K1", 0.9, 0)]])
    ids = pd.DataFrame(dict(gsis_id=["qb1", "qb2", "d1", "d2", "d3", "d4", "k1"],
                            pfr_id=["QB1", "QB2", "D1", "D2", "D3", "D4", "K1"]))
    return inj, snaps, ids


def test_team_report_counts_starters_only():
    rep = injuries.team_report(3, data=_data()).set_index("team")
    assert rep.loc["BAL", "qb_out"] and rep.loc["BAL", "qb"] == "Starter QB"   # the backup isn't a starter
    assert rep.loc["DEN", "starters_out"] == 3                                  # part-timer and QB-less
    assert not rep.loc["DEN", "qb_out"]
    assert "KC" not in rep.index                                                # Questionable doesn't count


def test_check_flags_qb_and_starter_gap():
    rep = injuries.team_report(3, data=_data())
    assert injuries.check("BAL", "KC", rep)[0]                 # its QB is out
    assert not injuries.check("KC", "BAL", rep)[0]              # backing the healthy side
    flagged, note = injuries.check("DEN", "LV", rep)
    assert flagged and "missing 3 starters" in note
    assert injuries.check("DEN", "BAL", rep)[0]


def test_backs_and_flag_bets():
    bets = pd.DataFrame([
        dict(instrument="kalshi_spread", team="BAL", side="no", home="BAL", away="KC"),   # backs KC
        dict(instrument="kalshi_win", team="BAL", side="yes", home="BAL", away="KC"),     # backs BAL
        dict(instrument="pair", team="BAL", side="win", home="BAL", away="KC"),           # never flagged
    ])
    out = injuries.flag_bets(bets, 3, data=_data())
    assert list(out.inj_flag) == [False, True, False]
    assert "QB out" in out.inj_note.iloc[1]


def test_empty_report():
    assert injuries.team_report(9, data=_data()).empty
    assert injuries.check("BAL", "KC", injuries.team_report(9, data=_data())) == (False, "")
