# Weekly NFL schedule

Every step across `nfl_model/` (pool), `nfl_bets/` (ELWAY paper bets) and
`nfl_fantasy/` (Sleeper). Times are ET. `<wk>` is the coming week unless
it says "last week". ⏰ = a scheduled task in the Claude desktop app runs it or
reminds you (see the bottom of this file).

## Tuesday: grade last week, waivers

| when | folder | step |
|---|---|---|
| 9:11 am ⏰ | `nfl_model` | `python3 log_week.py grade <last wk>` then `python3 pool_tracker.py report` |
| 9:11 am ⏰ | `nfl_bets` | `python3 bets.py grade <last wk>` then `python3 bets.py report` |
| 9:11 am ⏰ | `nfl_bets` | `python3 props.py grade <last wk>` then `python3 props.py report` |
| after grading | `nfl_bets` | `python3 props.py dashboard`, then ask Claude to upload it to the [Prop Bet Tracker](https://claude.ai/artifact/DFMxAvuerHEa3eiSxMfJqZ) |
| 7:02 pm ⏰ | `nfl_fantasy` | `python3 sleeper_lineup.py jg337` (rest-of-season free agents; Sleeper waivers clear overnight Tue → Wed) |

## Wednesday: ELWAY and the pool sheet

| when | folder | step |
|---|---|---|
| 10:06 am ⏰ | `nfl_bets` | Open ELWAY's page in the built-in browser and ask Claude to pull the week's margin distribution |
| after that | `nfl_bets` | `python3 bets.py sheet <wk>` (logs by default) |
| after that | `nfl_model` | `cat ../nfl_bets/data/elway/2026_wk<NN>.txt \| python3 import_elway.py <wk>` |
| when the pool sheet is out | `nfl_model` | `python3 update_mkt.py <wk> --add`, then enter `my_line` in `data/weekly_lines.xlsx` |
| if ELWAY updates later | `nfl_bets`, `nfl_model` | Repeat the pull, `sheet` and `import_elway` (only if you'd have acted on the update) |

## Thursday: TNF

| when | folder | step |
|---|---|---|
| 7:52 pm ⏰ | `nfl_bets` | `python3 kalshi.py <wk>` (closing snapshot for the Thursday game) |

## Sunday: lock at 1 pm

| when | folder | step |
|---|---|---|
| 10:06 am ⏰ | `nfl_model` | `python3 update_mkt.py <wk>`, then `python3 fetch_splits.py 2026 <wk> --to-workbook` (Apify), then `python3 pool.py <wk>` |
| before 1:00 pm | `nfl_model` | Submit, then `python3 log_week.py submit <wk> --picks A,B,C,D,E` |
| before 1:00 pm | `nfl_model` | Claude's picks, made in a **separate conversation**, then `python3 pool_tracker.py claude <wk> A,B,C,D,E` |
| 10:31 am ⏰ | `nfl_fantasy` | `python3 fantasy_props.py <wk>` (Odds API, ~90 credits), then `python3 sleeper_lineup.py jg337` |
| 10:31 am ⏰ | `nfl_bets` | `python3 props.py sheet <wk> --cached` (the props just fetched; no credits) |
| 12:47 pm ⏰ | `nfl_bets` | `python3 kalshi.py <wk>` (closing snapshot for the Sunday games) |

Close `weekly_lines.xlsx` in Excel before the Sunday 10:06 run: it writes to
the workbook.

## Scheduled tasks

They live in the Claude desktop app (sidebar → Scheduled) and run only while
the app is open. A task that was due while the app was closed runs at the next
launch. Each run gets the week from Sleeper's NFL state (the grade task uses
the last week completed in the nflverse schedule), and does nothing outside
the regular season.

| task | when | does |
|---|---|---|
| `nfl-tue-grade` | Tue 9:11 am | Grades the pool, ELWAY bets and props, then summarizes them |
| `nfl-tue-waivers` | Tue 7:02 pm | Runs `sleeper_lineup.py` and lists waiver targets |
| `nfl-wed-elway` | Wed 10:06 am | Reminder only (ELWAY pull, bet sheet, pool sheet) |
| `nfl-thu-tnf-close` | Thu 7:52 pm | Runs `kalshi.py` |
| `nfl-sun-pool` | Sun 10:06 am | Refreshes lines and splits, runs `pool.py`, reminds you to submit and log |
| `nfl-sun-lineup` | Sun 10:31 am | Runs props, lineups and the props sheet, lists lineup changes and prop flags |
| `nfl-sun-close` | Sun 12:47 pm | Runs `kalshi.py` |

Nothing scheduled submits picks, places bets or changes a Sleeper lineup:
those stay with you.

## Cloud snapshots (no laptop needed)

The steps that must happen before kickoff and need no one there run in GitHub
Actions, in the private repo [fantasy_gpx-data](https://github.com/jutes16/fantasy_gpx-data),
every 30 minutes on game days: for each game kicking off within 2.5 hours,
`nfl_bets/snapshot.py` buys its player props once, saves Sleeper's projections for
those players and takes a Kalshi snapshot (the last before kickoff is the close).
They're saved to the private repo; before grading or the props sheet, run
`python3 sync_data.py` from `nfl_bets/` to copy them in, then
`python3 props.py sheet <wk> --cached`.

Locally, don't fetch props (`fantasy_props.py <wk>` without `--cached`): a game
bought locally isn't in the cloud's store and would be bought twice.

