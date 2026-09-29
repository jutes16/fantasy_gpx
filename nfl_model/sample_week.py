"""
Example: score a week. Lines are HOME-perspective (negative = home favored).

Replace `games` each week with your sheet's numbers (my_line) and the
current market (mkt_line), then run:  python3 sample_week.py
"""

from model import score_week, fmt

# Week 3 2026, as a check against what was actually played.
games = [
    dict(away="KC",  home="MIA", my_line=11.5, mkt_line=10.5),
    dict(away="BAL", home="DAL", my_line=2.5,  mkt_line=3.5),
    dict(away="CIN", home="PIT", my_line=3.5,  mkt_line=3.5),
    dict(away="TEN", home="NYG", my_line=-3.5, mkt_line=-2.5),
    dict(away="LAR", home="DEN", my_line=2.5,  mkt_line=2.5),
    dict(away="SEA", home="WAS", my_line=7.5,  mkt_line=7.5),
    dict(away="HOU", home="IND", my_line=2.5,  mkt_line=1.5),
    dict(away="CAR", home="CLE", my_line=2.5,  mkt_line=2.5),
    dict(away="NE",  home="JAX", my_line=-3.5, mkt_line=-3.0),
    dict(away="ARI", home="SF",  my_line=-8.5, mkt_line=-8.5),
    dict(away="LV",  home="NO",  my_line=-3.5, mkt_line=-3.0),
    dict(away="NYJ", home="DET", my_line=-6.5, mkt_line=-6.5),
    dict(away="LAC", home="BUF", my_line=-7.5, mkt_line=-7.0),
    dict(away="MIN", home="TB",  my_line=1.5,  mkt_line=1.5),
]

if __name__ == "__main__":
    rows = score_week(games)
    print(fmt(rows))
    plays = [r for r in rows if r["verdict"] == "PLAY"]
    leans = [r for r in rows if r["verdict"] == "lean"]
    print(
        f"\nPLAY: {len(plays)}   lean: {len(leans)}   "
        f"PASS: {len(rows)-len(plays)-len(leans)}"
    )
