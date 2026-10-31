# Weekly NFL schedule

Every step across `nfl_model/` (pool), `nfl_bets/` (ELWAY paper bets) and
`nfl_fantasy/` (Sleeper). Times are ET. `<wk>` is the coming week unless
it says "last week". ⏰ = a scheduled task in the Claude desktop app runs it or
reminds you (see the bottom of this file).

## Tuesday: grade last week, waivers (by hand)

| when | folder | step |
|---|---|---|
| any time | `nfl_model` | `python3 log_week.py grade <last wk>` then `python3 pool_tracker.py report`; send Claude a screenshot of the pool board for the field's picks, then `python3 pool_field.py` |
| any time | `nfl_bets` | `python3 sync_data.py`, then `python3 bets.py grade <last wk>` then `python3 bets.py report` |
| any time | `nfl_bets` | `python3 props.py grade <last wk>` then `python3 props.py report` |
| after grading | `nfl_bets` | `python3 hq.py`, then ask Claude to upload it to the [Bet Tracker](https://claude.ai/artifact/VUFMER8ut3xa9naR877HJS) |
| before Tue night | `nfl_fantasy` | `python3 sleeper_lineup.py jg337` (rest-of-season free agents; Sleeper waivers clear overnight Tue → Wed) |

## Wednesday: ELWAY and the pool sheet

| when | folder | step |
|---|---|---|
| every 4 h ☁ | cloud | ELWAY's public feed (`elway_pull.py --cloud`): when it changes, the week's files and the bet sheet priced right then |
| 10:06 am ⏰ | `nfl_bets` | `python3 sync_data.py` (logs the cloud's sheets), then `python3 elway_pull.py`; if it saved anything new, `python3 bets.py sheet <wk>` |
| after that | `nfl_bets` | `python3 season.py sheet <wk>` (ELWAY's season wins and playoff odds vs Kalshi's season markets; a paper test, logs by default) |
| after that | `nfl_model` | `python3 import_elway.py <wk>` (re-imports the saved table) |
| after that | `nfl_bets` | `python3 hq.py`, then ask Claude to upload it |
| when the pool sheet is out | `nfl_model` | `python3 update_mkt.py <wk> --add`, then enter `my_line` in `data/weekly_lines.xlsx` |
| ELWAY updates later | cloud, then you | The cloud logs the new sheet; `sync_data.py` (Sunday's task runs it) brings it in. Re-run `import_elway.py <wk>` before the pool card |

## Thursday: TNF

| when | folder | step |
|---|---|---|
| 4 pm ☁ | cloud | the week's player props and Sleeper projections (`snapshot.py --full`) |
| evening | `nfl_bets` | `python3 sync_data.py`, `python3 props.py sheet <wk> --cached` (marks the PICKs), `python3 hq.py`, then ask Claude to upload: the Analysis tab shows the week |
| from ~2.5 h before kickoff ☁ | cloud | Kalshi snapshots (the last one before kickoff is the close) |

## Sunday: lock at 1 pm

| when | folder | step |
|---|---|---|
| 10 am ☁ | cloud | fresh prices on the week's picks (`snapshot.py --recheck`) |
| 10:36 am ⏰ | `nfl_bets` | `python3 sync_data.py` (the cloud's props, Sleeper and Kalshi snapshots) |
| 10:36 am ⏰ | `nfl_model` | `python3 import_elway.py <wk>` (ELWAY's latest), `python3 update_mkt.py <wk>`, then `python3 fetch_splits.py 2026 <wk> --to-workbook` (Apify), then `python3 pool.py <wk>` |
| before 1:00 pm | `nfl_model` | Submit, then `python3 log_week.py submit <wk> --picks A,B,C,D,E` |
| before 1:00 pm | `nfl_model` | Claude's picks, made in a **separate conversation**, then `python3 pool_tracker.py claude <wk> A,B,C,D,E` |
| 10:36 am ⏰ | `nfl_bets` | `python3 props.py sheet <wk> --cached` (marks the week's PICKs) |
| 10:36 am ⏰ | `nfl_fantasy` | `python3 fantasy_props.py <wk> --cached`, then `python3 sleeper_lineup.py jg337` |
| after that | `nfl_bets` | `python3 hq.py`, then ask Claude to upload it (the [Bet Tracker](https://claude.ai/artifact/VUFMER8ut3xa9naR877HJS)'s Analysis tab: this week's props and game bets) |
| from ~2.5 h before each kickoff ☁ | cloud | Kalshi snapshots (the closes); props for any game Thursday missed |

Close `weekly_lines.xlsx` in Excel before the Sunday 10:36 run: it writes to
the workbook.

## Scheduled tasks

They live in the Claude desktop app (sidebar → Scheduled) and run only while
the app is open. A task that was due while the app was closed runs at the next
launch. Each run gets the week from Sleeper's NFL state (the grade task uses
the last week completed in the nflverse schedule), and does nothing outside
the regular season.

| task | when | does |
|---|---|---|
| `nfl-wed-elway` | Wed 10:06 am | Syncs the cloud, pulls ELWAY, logs the bet and season sheets, imports ELWAY for the pool, runs `hq.py`; reminds you to enter the pool sheet |
| `nfl-sun-pool` | Sun 10:36 am | Syncs the cloud's data, refreshes lines and splits, runs `pool.py`, lineups and the props sheet; lists lineup changes, the pool card and the week's prop picks, and reminds you to submit and log |

The Kalshi closing snapshots are the cloud's alone (no local task: one on a
sleeping laptop fires hours late).

Nothing scheduled submits picks, places bets or changes a Sleeper lineup:
those stay with you.

## Cloud snapshots (no laptop needed)

The steps that must happen before kickoff and need no one there run in GitHub
Actions, in the private repo [fantasy_gpx-data](https://github.com/jutes16/fantasy_gpx-data),
Thursday 4 pm ET (every game's player props and Sleeper's projections), Sunday
10 am ET (fresh prices on the week's picks) and every 30 minutes on game days
(Kalshi snapshots; props for any game Thursday missed), plus ELWAY's feed every 4
hours (the bet sheet priced whenever ELWAY changes).
They're saved to the private repo; before grading or the props sheet, run
`python3 sync_data.py` from `nfl_bets/` to copy them in, then
`python3 props.py sheet <wk> --cached`.

Locally, don't fetch props (`fantasy_props.py <wk>` without `--cached`): a game
bought locally isn't in the cloud's store and would be bought twice.

