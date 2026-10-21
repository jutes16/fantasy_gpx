# nfl_fantasy/ — fantasy tools

| file | what it does |
|---|---|
| `fantasy_props.py` | Betting-market fantasy projections vs Sleeper's, from sportsbook player props |
| `sleeper_lineup.py` | Optimal starting lineup and free-agent upgrades for each of your Sleeper leagues |
| `prop_shapes.py` | Measures how player stats vary game to game (nflverse logs), for converting prop lines to means; writes `data/prop_shapes.json` |
| `shapes.py` | Prop line ↔ mean conversions using those shapes (shared with `../nfl_bets/props.py`) |
| `history.py` | A prop's player history (last-8 hit rate at the line, averages) and matchup (what the opponent allows his position), games before the prop's week only |
| `usage_trends.py` | Season target-share, RB touch-share and snap-share trends (recency-weighted) from nflverse box scores |
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

How a prop becomes a projection (`shapes.py`, measured by `prop_shapes.py`):
- **Yards:** a prop line is a median; Sleeper projects means. The gap depends
  on volume, so `prop_shapes.py` measures the whole game-to-game distribution
  (game ÷ the player's average over his other games that season) in volume
  bands, from nflverse game logs 2022-24. A 15.5 receiving line implies a mean
  ~1.25× the line, a 65.5 line ~1.1×; low-volume rushing is burstier still.
  The mean is the one that puts the de-vigged P(over) above the line.
- **Receptions, passing TDs:** count distributions with the measured
  dispersion: receptions vary more than Poisson (1.19), passing TDs less (0.79).
- **Anytime TD:** expected TDs = −ln(1 − P(scores)) (Poisson; the measured
  dispersion predicted zero-TD games worse than Poisson did).

Checked on 2025 (not used in the fit): at the lines each shape puts at its
25th / 50th / 75th percentile, players went over 75 / 54 / 32% (receiving,
10-20 yd band; target 75 / 50 / 25) where the old single ratios gave 60 / 45 /
36. Re-run `python3 prop_shapes.py` to refit and see the full check (downloads
the game logs to `data/nflverse/`, git-ignored).

Check: in week 4, after conversion, Sleeper's average sat within 1-3% of the
market's for yardage (before, receiving yards ran ~10% apart from the
mean/median mismatch alone). Receptions are the exception: Sleeper projected
7% below the market, and players beat both. The gaps it flags are start/sit and
DFS leads, not certainties: props carry more vig and less liquidity than game
lines.

**Credits:** needs `ODDS_API_KEY` in your environment (`~/.zshrc`; never in the
repo). Props cost 1 credit per market per game: 6 markets × ~15 games ≈ 90 a
week of the 500 free per month. Each game is saved under `data/props/` (git-
ignored) and never paid for twice; games that have kicked off are skipped.
The client is shared with `../nfl_bets/odds_api.py`.

## Target and touch trends

```
python3 usage_trends.py                # leaders / risers / fallers
python3 usage_trends.py --half-life 2  # weight recent weeks harder (default 3)
python3 usage_trends.py --sleeper jg337 # also tag players by your Sleeper rosters (for the dashboard)
```

From nflverse box scores (every player's targets, carries and receptions) and
snap counts. Share = targets / team pass attempts, or an RB's carries +
receptions / team offensive snaps, summed with each week's weight halving every
`--half-life` weeks back; beside it the plain season share, the last three
games, the slope and the weighted snap share. Games a player missed are left
out; games he played with no targets count as 0. Box-score and snap-count
names are matched by team and week (nicknames like Bam / Zonovan Knight by
last name); any left unmatched are printed. It also flags role changes (`<season>_signals.csv`): gaining or losing work
(last 3 games vs before), a starter out (share in the games a top-2 teammate
missed, when he's out now per Sleeper), and snap jumps. Half-PPR points
(nflverse) sit beside each share, against what the player's usage is worth:
his weighted targets (an RB's targets and carries at their own fitted rates)
times his position's points per opportunity this season. 3+ games scoring well
under that is flagged `buy_low`, well over it `sell_high` (mostly touchdowns;
elite players sit above the line every year). Injuries come from Sleeper (status and
body part now) and the nflverse weekly injury reports (practice participation,
the reason for each missed game, games played while Questionable); each flagged
change gets a line of context, such as a teammate who went out or came back
during the player's last 3 games. Tables go to
`data/usage/<season>_{weekly,targets_trends,touches_trends,signals,rosters}.csv`;
they feed the [usage dashboard](https://claude.ai/artifact/2Yq3mXQwvqhfjdq95Bjf56).

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
