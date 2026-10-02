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
python3 bets.py sheet 4 --log
```
Pulls fresh Kalshi prices (public API, no key) and nflverse sportsbook prices,
prints the sheet, and appends every flagged bet to `data/bet_log.csv` as a new
**batch**, with the best one per game marked `recommended`. Games that have
kicked off are skipped. `python3 bets.py sheet 4` (without `--log`) just prints.

**When ELWAY updates, repeat steps 1 and 2** — but only for an update you would
have acted on. Each `--log` adds a batch; earlier batches are never changed.
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
```

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

## Files

| file | contents |
|---|---|
| `bets.py` | sheet, log, grade, report, ELWAY paste |
| `kalshi.py` | Kalshi public API client; `python3 kalshi.py <week>` saves a snapshot |
| `odds_api.py` | The Odds API client (needs `ODDS_API_KEY`): every US book's moneyline / spread / total; `python3 odds_api.py <week> --show` |
| `common.py` | team codes, nflverse schedule, ELWAY parser, margin models (ELWAY distribution, historical fallback, normal), fees |
| `data/bet_log.csv` | every logged paper bet and its result (`batch`, `elway_fp` = which ELWAY forecast it came from) |
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
