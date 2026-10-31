# nfl_bets — can ELWAY make money?

A **paper-betting test** of the ELWAY model, separate from the picks pool
(`../nfl_model`). Every week it prices Kalshi and sportsbook bets against
ELWAY's simulated margin distribution, logs the ones ELWAY likes **before
kickoff**, and grades them afterwards. No bets are placed; the log is the test.

The question it answers: does ELWAY beat the market, and in which instrument
(win contract, moneyline, spread, or win-vs-spread pair)?

## Weekly process

Run everything from `nfl_bets/`.

**1. Get ELWAY's numbers (Tue/Wed, once ELWAY publishes the week).**
Open the ELWAY projections page (Silver Bulletin, paid) in the Claude desktop
app's built-in browser and ask Claude to pull the week's margin distribution.
It reads the "Simulated margin of victory" embed and writes, into the
git-ignored `data/elway/`:

| file | contents |
|---|---|
| `dist_<season>_wk<NN>_raw.txt` | the embed's data as read (audit copy) |
| `dist_<season>_wk<NN>.csv` | `away, home, home_margin, prob` — one row per game and margin |
| `<season>_wk<NN>.txt` | ELWAY's table (spread, win %, total) in the paste format |

Sanity checks that must pass before the files are written: each game's
winning bins add up to ELWAY's published win % (to 0.05 pts). Note the embed's
sign convention: a **negative** display margin means the **home** team wins;
`home_margin` in the CSV is already flipped.

No distribution? `pbpaste | python3 bets.py elway <week>` saves a pasted ELWAY
table instead, and the sheet uses the historical-shape fallback (see below).

To use the same numbers in the pool: from `../nfl_model`,
`cat ../nfl_bets/data/elway/<season>_wk<NN>.txt | python3 import_elway.py <week>`.

**2. Price and log.**
```
python3 bets.py sheet 4
```
Pulls fresh Kalshi prices (public API, no key) and nflverse sportsbook prices,
prints the sheet, and appends every flagged bet to `data/bet_log.csv` as a new
**batch**, with the best one per game marked `recommended`. Games that have
kicked off are skipped. `python3 bets.py sheet 4 --no-log` just prints.

**When ELWAY updates, repeat steps 1 and 2** — but only for an update you would
have acted on. Each logged run adds a batch; earlier batches are never changed.
A new batch is refused if ELWAY's inputs are identical to the last batch's
(re-logging at new prices alone would just move the test's entry point);
`--force` overrides that.

`report` reads the batches two ways:
- **FIRST LOG** — each game as first logged: what you'd have bet on day one.
- **LATEST LOG** — each game as last logged before its kickoff.

The gap between their RECOMMENDED rows is what ELWAY's mid-week updates were
worth, and it says how many games an update actually changed the bet on.

**3. Closing snapshot(s), right before kickoff.**
```
python3 kalshi.py 4
```
Thursday evening for the TNF game, Sunday morning for the rest. The last
snapshot before each kickoff is the "close" used for closing-line value.

**4. Grade, after the last game.**
```
python3 bets.py grade 4
python3 bets.py report
python3 bets.py report --without PHI    # same ledger leaving out PHI's games
```

`report` shows every instrument two ways: flat (1 unit a bet) and **EV-scaled**,
where the stake is the bet's EV / 0.15 units, capped at 0.5 to 2 (0.15 is about
the median EV of logged bets; it's fixed, so each stake is known when the bet is
made). The cap matters: uncapped, one 13¢ Kalshi long shot (ARI at LAR, week 6,
EV 0.78) would have been a 4.2-unit bet and three quarters of the season's
profit. Through week 6, recommended bets without PHI's games: flat +13.17 units
(+41%), EV-scaled +24.10 (+66%); without the ARI win, +6.92 vs +11.60. Lower-EV
bets did worse than higher-EV ones, but CLV doesn't rise with EV yet, so this is
a comparison to track, not a reason to size up.

`--without` is analysis only: the log and the recommendations are unchanged.
It exists because ELWAY has rated PHI 3.5-6 pts above the market every week
since week 4 (the most of any team) and those bets went 0-3.

## Player props: over or under?

## Season bets (season.py): ELWAY's season vs Kalshi's season markets

A paper test of ELWAY's season-long numbers: its simulated win distribution and
playoff odds (Silver Bulletin's public embed feed) against Kalshi's "wins at
least N" (KXNFLWINS) and "makes the playoffs" (KXNFLPLAYOFF) markets.

```
python3 season.py sheet 8      # price, print, log a batch (Wednesday, after ELWAY updates)
python3 season.py grade        # January: wins once the regular season ends, playoffs once the field is set
python3 season.py report
```

Both sides of every market are priced at what you could execute (YES at the
ask, NO at 1 - bid, plus Kalshi's fee). One bet per team, its best side priced
20-80c, because every rung of a team's ladder is the same opinion. Money is tied
up until January, so EV is discounted at 3.5% a year (`--rate`):
`ev_adj = (1 + ev) / (1 + rate) ** years - 1`; recommended = `ev_adj` >= 10%
(`--min-ev`). The log (`data/season_log.csv`) keeps every team's best side with
ELWAY's playoff odds this week with a loss / now / with a win. The Bet Tracker's
Analysis tab shows the latest batch under "Season bets (ELWAY)".

A second paper test, separate from ELWAY: do Sleeper's projections, or simply
shopping six books, beat the player-prop market?

```
python3 props.py sheet 5            # Sunday morning: props + Sleeper -> sheet, logged
python3 props.py sheet 5 --cached   # re-use props already saved by fantasy_props.py (no credits)
python3 props.py grade 5            # Tuesday: results from Sleeper's box scores
python3 props.py report
```

Props (passing / rushing / receiving yards, receptions, passing TDs, anytime
TD) come from The Odds API through `../nfl_fantasy/fantasy_props.py`, sharing
its saved files, so the Sunday fetch is paid for once (~90 credits). Each
book's line and de-vigged price becomes an implied mean; the market's fair mean
is the median across books. Every side is priced at the **best offer across
books**, with two signals:

- **SHOP**: the best offer is worth 3%+ against the *other* books' fair price
  (an outlier line or a stale price).
- **PROJ**: Sleeper's projection gives the side 8+ points more probability than
  the market's fair price. These are the "who goes over / under" picks.
- **PICK**: the props you'd actually bet, at most 15 a week. From the PROJ
  flags, keep those where the player's last 8 games beat this line on this side
  at least half the time and the matchup favours the side (the opponent allows
  at least the league average to an over, at most to an under), drop anytime
  TDs, then take the 15 biggest Sleeper edges in the week.

**How PICK was chosen (weeks 4-5, the only graded weeks):** seven rules were
listed first and each tested by week. Injuries barely mattered (3 of 248 PROJ
flags carried a tag), and the biggest Sleeper edges did fine (7-3). Two held in
both weeks: matchup agreeing (58.0% vs the market's 51.1%) and recent form
agreeing (54.1% vs 51.1%). With both, no anytime TDs and the top 15 a week:
20-10, 66.7% vs 52.2%, +23.9% (+4.0% and +25.0% by week), against all 248 PROJ
flags at 52.4% vs 50.4%, +1.7%. The rules were picked on those same two weeks
(z = 1.6), so this is a lead the coming weeks will test. Line shop (SHOP) stays
logged but isn't a pick: its profit was one week of long shots (-23.9% in
week 4, +10.6% in week 5).

Probabilities use the shapes measured from nflverse game logs
(`../nfl_fantasy/prop_shapes.py`): yards follow the measured game-to-game
distribution for the line's volume band (a 15.5 receiving line implies a mean
well above it, a 65.5 line barely); receptions and passing TDs use the measured
dispersion; anytime TD is Poisson. Lines and Sleeper's means go through the
same shapes, so a Sleeper mean equal to the market's reproduces the market's
probability exactly (tested), and Sleeper's average vs the line's median gap
is accounted for.

**Scope**, fixed before any results: prices −250 to +250; yardage lines 15+
(passing 150+), since a QB's 0.5 rushing yards or a backup's 6.5 receiving
sit below the measured volume bands; no SHOP on anytime TD (no book quotes "No",
so its fair price rests on an assumed vig). Every side in scope is logged, not
only the flagged ones, so the report compares the signals with the rest. A
player who doesn't play is void; a whole-number line can push.

**Player history and matchup** (`../nfl_fantasy/history.py`, nflverse game
logs, games before the prop's week only) are logged with every side as context:

- `hit_rate`: share of the player's last 8 games (this season, then last
  season's latest) that would have won this bet at this line; `l8_avg` and
  `hist_avg` (2026 so far) beside it.
- `opp_factor`: what the opponent has allowed per game this season to his
  position for that stat, over the league average, pulled toward 1 by 4 games
  of average defense (three or four games is mostly noise). Above 1 is soft for
  an over.

They feed no signal. The report tests them on the over / yes side only, each
prop once: both sides are logged, and an over that hit in 7 of 8 games is an
under that hit in 1, so counting both would mirror every result (that error
made week 4 look like 55% vs 52%). The Sleeper-edge buckets count each prop
once for the same reason (the under's edge is the over's, negated).

`report` shows every side, overs vs unders, SHOP, PROJ, Sleeper's edge in
buckets, recent form and matchup buckets, and PROJ by market, each as W-L, win % vs the market's fair
probability, units and ROI (vig included). Read `win%` against `mkt P`;
`EV slp` is only as good as Sleeper's projection.

The sheet saves Sleeper's projections at log time
(`../nfl_fantasy/data/props/<season>_wk<NN>_sleeper_at_log.csv`); `sheet --cached`
on a week that has started uses that file, never today's projections, which
Sleeper revises after the games (a player ruled out drops to zero).

The [Bet Tracker](#bet-tracker-dashboard) dashboard shows these results and each
week's flagged props.

**Weeks 4-5** (both backfilled from props and Sleeper projections saved before
kickoff: week 4 on Oct 2, week 5 at 12:58 on Oct 11, so week 5 has no Thursday
game): 1,529 graded sides went 49.6% against a fair 49.7%, so the market's
prices are calibrated. Best price across books lost 2.1% (vs ~4.5% vig at one
book). PROJ is 130-118 (52.4% vs 50.4% fair, +1.7%); SHOP 38-46 (45.2% vs 42.3%,
+20.9% on 84 bets, mostly plus-money). One row per prop, priced against the
market, clustered by game (28 games):

| feature | z | p |
|---|---|---|
| Sleeper edge | +0.76 | 0.45 |
| recent form (last-8 hit rate) | +0.77 | 0.44 |
| matchup (opponent allowance) | +2.19 | 0.029 |

Only the matchup shows anything, and it misses the three-test threshold
(p < 0.017). It pointed the same way both weeks, and week 5 is out of sample
(the feature was written on Oct 10, before those games). Watch it; don't act on
it.

## Reading the sheet

```
  * LAR @ PHI   LAR by over 1.5  NO @ 0.43         ELWAY 60.2%  EV +0.347
```
- `LAR by over 1.5  NO` — Kalshi's "Rams win by over 1.5" market, NO side:
  pays $1 if the Eagles win, it's a tie, or the Rams win by exactly 1
  (effectively Eagles +1.5).
- `@ 0.43` — the price (1 − YES bid). All-in with Kalshi's fee
  (0.07 · P · (1−P)) it costs about 0.447.
- `ELWAY 60.2%` — ELWAY's probability the contract pays, summed from its
  simulated margins (PHI wins 56.5% + tie 0.7% + LAR by exactly 1 3.0%).
- `EV +0.347` — expected profit per $1 staked under ELWAY: 0.602 / 0.447 − 1.
- `*` — EV at or above the flag threshold (`--min-ev`, default 0.05).

A very large EV usually means ELWAY and a liquid market strongly disagree.
Check injury / QB news before trusting it: the market may know something the
model hasn't absorbed.

## Instruments

| instrument | what | priced at |
|---|---|---|
| `kalshi_win` | "team wins" YES | ask + fee |
| `kalshi_spread` | favourite "wins by over k": YES, or NO (= underdog +k) | ask + fee / (1 − bid) + fee |
| `book_ml` | sportsbook moneyline | best price across books with `--books`, else nflverse consensus |
| `book_spread` | sportsbook spread | best line + price (highest EV) across books with `--books`, else nflverse |
| `pair` | buy $1 of the win contract, sell $1 of the spread contract (or the reverse) at the strike matching the book line, zero net outlay | both legs at executable prices |

Kalshi spread strikes: the favourite's ladder only, within 3.5 points of the
book line, quotes no wider than 4¢, contracts priced 10¢ or more.
`RECOMMENDED` = the best directional bet per game (pairs excluded) with EV at
or above the threshold. Stakes are 1 unit, flat.

## The model

Every probability comes from a distribution of the home team's final margin:

1. **ELWAY's own simulated distribution** (`dist_<season>_wk<NN>.csv`, pulled
   from the chart) — used whenever it's there. Key numbers (3, 7) carry their
   real weight and ties are explicit (a tie settles a Kalshi win contract NO
   and pushes a sportsbook moneyline). `model` column: `dist`.
2. **Historical-shape fallback** (`common.HistoricalMargin`) — when only
   ELWAY's table (spread, win %) is available. Real NFL margins from
   2015 through last season, unshifted, weighted toward games whose closing
   spread was near the projection, and centred so the home win % matches
   ELWAY's published one. `model` column: `hist`.

The fallback was checked against ELWAY's actual week-4 distributions:

| | mean error | worst | value of crossing 3 |
|---|---|---|---|
| ELWAY's distribution | – | – | 6.8 pts |
| historical fallback | 1.2 pts | 5.5 | 6.4 pts |
| normal curve (old fallback, `common.Margin`) | 2.5 pts | 10.4 | 2.9 pts |

(errors: P(home wins by over k) for k = −10.5 … +10.5, in probability points.)
On the week-4 sheet the fallback flagged the same 12 games on the same side as
the chart; moneyline EVs matched within about 0.01, spread and pair EVs within
about 0.03 on average, sometimes preferring a neighbouring strike. So the chart
matters most for choosing the exact spread strike.

Neither method uses ELWAY's team ratings directly: the centre of each game is
ELWAY's projection, which already turns the ratings, home field / neutral site
and its other adjustments into a game-level number.

The pair math reproduces the published ELWAY/Kalshi week-1 example (ARI +6.5:
EV +0.15 under ELWAY, −0.14 under the market).

## Grading and what to watch

`grade` settles each bet on the final score and records CLV (Kalshi: last
snapshot before kickoff taken after you logged; book: nflverse's closing
moneyline). `report` shows each instrument and the RECOMMENDED set: bets,
W-L, ELWAY's expected P&L, realized P&L, ROI, CLV.

- **CLV is the early signal.** If logged prices consistently beat the close,
  ELWAY is seeing something before the market does.
- **P&L is mostly noise** for a long time: a season of ~12 bets a week is
  still a small sample.
- **Watch the underdog lean.** ELWAY has favoured underdogs most weeks; if
  it only wins in underdog-friendly seasons, it's a dog bet, not an edge.

## Bet Tracker dashboard

[Bet Tracker](https://claude.ai/artifact/VUFMER8ut3xa9naR877HJS) is a private claude.ai dashboard over the bets you'd make:
ELWAY's recommended game bet per game and the prop PICKs. Two tabs:

- **Dashboard:** profit, wagers, win rate and ROI, the record, running profit
  or ROI by week, and the bets themselves (filterable by week and result, with
  search). Filter by bet type (each ELWAY instrument, prop picks), all games or
  without PHI, flat or EV-scaled stakes, and last week / 4 weeks / season.
- **Analysis:** the upcoming week's recommendations: the props sheet (pick,
  price, Sleeper vs market projection, rating, EV, the opponent's rank for the
  stat, injury tag, last-5 / last-10 / season hit rates) and ELWAY's game bets
  (recommended bet per game, price, ELWAY vs market probability, EV, stake).
  Props open on the week's PICKs, each with the best price and the book
  offering it; every flag is a click away.

It reads uploaded copies of `data/dashboard_hq/*.json` (git-ignored), so it
doesn't update itself: after a sheet or grading, run `python3 hq.py` and ask
Claude to upload the files to the dashboard.

## Cloud snapshots

`snapshot.py` runs in GitHub Actions in the private repo
[fantasy_gpx-data](https://github.com/jutes16/fantasy_gpx-data), so the
pre-kickoff data doesn't depend on a laptop being awake:

- **Thursday 4 pm ET** (`--full`): player props for every game this week and
  Sleeper's projections for those players (~84 credits). The week's picks and the
  dashboard's Analysis tab can be built from Thursday evening.
- **Sunday 10 am ET** (`--recheck`): fresh prices on just the week's picks
  (~10-20 credits), shown as "Sun 10 am" on the Analysis tab.
- **Every 30 minutes on game days**: a Kalshi snapshot when a game kicks off
  within 2.5 hours (the last one before kickoff is the close), plus props for
  any game Thursday's pull missed.

About 100 credits a week of the free tier's 500 a month. `python3 sync_data.py`
copies everything into this checkout (cloning the data repo next to this one
the first time); then `props.py sheet <wk> --cached`, `bets.py grade` and
`hq.py` work as before. The data repo is private because the props are
per-book Odds API prices.

## My bets

The Bet Tracker's Dashboard tab switches between **My bets** and **All
recommended**. My bets come from two places:

- **Ticks:** tick **Bet** on any row of the Analysis tab, with your stake in
  units and, if different, your price. Claude exports the ticks
  (`python3 hq.py --ticks <folder>` imports them to `data/my_bets.json`) and
  `hq.py` grades them from the logs at your stake and price.
- **Your Kalshi account:** `python3 kalshi_mine.py` reads your NFL fills and
  settlements (read-only, signed with your API key; setup at the top of the
  file) into `data/my_kalshi.json`. Stakes are what you paid in units of
  `MY_UNIT_DOLLARS`; results are Kalshi's own settlements. When it exists, it
  replaces any Kalshi ticks.

## Files

| file | contents |
|---|---|
| `bets.py` | sheet, log, grade, report, ELWAY paste |
| `kalshi.py` | Kalshi public API client; `python3 kalshi.py <week>` saves a snapshot |
| `odds_api.py` | The Odds API client (needs `ODDS_API_KEY`): every US book's moneyline / spread / total; `python3 odds_api.py <week> --show` |
| `props.py` | player props: sheet (best price, SHOP and PROJ flags, history and matchup), log, grade, report |
| `hq.py` | the Bet Tracker dashboard's data: both ledgers, results by filter, the upcoming week's sheets |
| `snapshot.py` | the cloud's pre-kickoff snapshots: props, Sleeper projections, Kalshi (run by GitHub Actions in fantasy_gpx-data) |
| `kalshi_mine.py` | your real Kalshi NFL bets (read-only API) for My bets |
| `elway_pull.py` | ELWAY's week (table + margin distributions) from its public feed; `--cloud` also prices the bet sheet (elway.yml in the data repo) |
| `kalshi_mcp.py` | local MCP helper behind the dashboard's "Refresh my Kalshi bets" button (runs kalshi_mine.py + hq.py on your Mac) |
| `sync_data.py` | copies the cloud snapshots from the private data repo into this checkout |
| `common.py` | team codes, nflverse schedule, ELWAY parser, margin models (ELWAY distribution, historical fallback, normal), fees |
| `data/bet_log.csv` | every logged paper bet and its result (`batch`, `elway_fp` = which ELWAY forecast it came from) |
| `data/prop_log.csv` | every logged prop side and its result (git-ignored: per-book Odds API prices) |
| `data/dashboard_hq/` | the Bet Tracker dashboard's data, from `hq.py` (git-ignored) |
| `data/kalshi/` | timestamped Kalshi snapshots |
| `data/odds/` | sportsbook odds snapshots (git-ignored) |
| `data/elway/` | ELWAY tables and distributions (git-ignored: paywalled) |
| `data/nflverse_games.csv` | cached nflverse schedule/odds (git-ignored) |

Week 1 and 3 results at closing prices (before this ledger existed) are in
`../nfl_model/ml_vs_spread.py`; this ledger only counts bets logged before
kickoff.

## Status

- 2026 week 4: distribution pulled and 42 bets logged (12 recommended)
  on 2026-09-30.
