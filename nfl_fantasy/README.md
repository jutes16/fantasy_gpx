# nfl/ — fantasy tools

| file | what it does |
|---|---|
| `fantasy_props.py` | Betting-market fantasy projections vs Sleeper's, from sportsbook player props |
| `sleeper_lineup.py` | Optimal starting lineup and free-agent upgrades for each of your Sleeper leagues |
| `weekly_stats.py` | Sleeper weekly stats and a lineup points calculator (`python3 weekly_stats.py` runs the examples) |

## Betting market vs Sleeper

```
python3 fantasy_props.py 4             # fetch week-4 player props, compare with Sleeper
python3 fantasy_props.py 4 --cached    # re-use saved props (no credits)
python3 fantasy_props.py 4 --ppr 1     # full PPR (default half-PPR; 0 = standard)
python3 fantasy_props.py 4 --games PHI,BUF   # only games with these home teams
```

Sportsbook player props (passing / rushing / receiving yards, receptions,
passing TDs, anytime TD) from The Odds API are turned into an implied stat
projection per player, then fantasy points, and compared with Sleeper's
projection **on the same stats** (e.g. QB interceptions aren't priced, so they're
left out of both). Output: the biggest disagreements per position, with each
stat as market / Sleeper, and Sleeper's injury status; full table saved to
`data/props/<season>_wk<NN>_vs_sleeper.csv`.

How a prop becomes a projection:
- **Yards:** a prop line is a median; Sleeper projects means. The line is nudged
  by the de-vigged over price, then converted with the mean / median ratio
  measured from nflverse game logs (2023-24): passing 0.99, rushing 1.05,
  **QB rushing 1.18** (scrambles make it burstier), receiving 1.10.
- **Receptions, passing TDs:** Poisson mean matching P(over).
- **Anytime TD:** expected TDs = −ln(1 − P(scores)).

Check: across week 4 the market and Sleeper agree on average at every position
(mean gap within ±0.1 pts), so the gaps it flags are player-specific, not a
built-in tilt. They are start/sit and DFS leads, not certainties: props carry
more vig and less liquidity than game lines.

**Credits:** needs `ODDS_API_KEY` in your environment (`~/.zshrc`; never in the
repo). Props cost 1 credit per market per game: 6 markets × ~15 games ≈ 90 a
week of the 500 free per month. Each game is saved under `data/props/` (git-
ignored) and never paid for twice; games that have kicked off are skipped.
The client is shared with `../nfl_bets/odds_api.py`.

## Your Sleeper leagues: optimal lineup and free agents

```
python3 sleeper_lineup.py <sleeper username>
python3 sleeper_lineup.py <username> --week 5
python3 sleeper_lineup.py <username> --league "Dynasty"   # leagues whose name contains this
```

For every league you're in this season (Sleeper's public API, no login):
- **Scoring:** every projection is scored with that league's own scoring
  settings (reception points, bonuses, kicker and defense tiers). Checked: with
  half-PPR settings it matches Sleeper's half-PPR projections to within 0.06 pts.
- **Optimal lineup:** for the league's actual roster slots (FLEX, SUPER_FLEX,
  REC_FLEX, ...), solved exactly; players who are Out / IR / suspended or on bye
  are skipped. Shows the changes from your current lineup and warns if a
  current starter won't play.
- **Free agents, this week:** players whose addition raises your optimal
  lineup's projected points.
- **Free agents, rest of season:** players projecting 5+ more points from this
  week through week 18 than your weakest player at their position.

Projections and the player list are cached for the day under `data/sleeper/`
(git-ignored).
