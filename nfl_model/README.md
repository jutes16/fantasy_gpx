# NFL pool model — best 5 picks weekly

Built for a straight-wins pool against a **fixed sheet shared by every
entrant**, scored on season-long standings.

Every coefficient traces to a measurement in `backtest.py` or
`keynumbers.py`, run over 2,895 regular-season games (2015-2025,
nflverse). Nothing is asserted without a number behind it.

## The strategy in one paragraph

Everyone in the pool gets the same numbers, and the sheet goes stale
between publication and kickoff. Your competitors are answering "who
wins this game?" You answer "which of these shared numbers has drifted
off the live market?" Only the second question has a measurable edge
behind it, and it does not require being a better football analyst than
anyone else in the pool.

## Setup

Run everything from `nfl_model/`, in the conda environment that has the
requirements installed (`pip install -r requirements.txt`).

Betting splits come from Action Network through the Apify actor
`zen-studio/action-network-odds` (about $2 a season, inside Apify's free
credits). Put your Apify token in `~/.zshrc`, never in the repo:

```
export APIFY_TOKEN="..."
```

## Weekly workflow

All lines are **home-perspective** (negative = home favored).

1. `python3 update_mkt.py <week> --add` — adds the week's games to
   `data/weekly_lines.xlsx` with the current market number (`mkt_line`)
   from nflverse. Fill in the pool sheet's number (`my_line`) in Excel.
   Re-run without `--add` to refresh `mkt_line`.
2. `python3 fetch_splits.py 2026 <week> --to-workbook` — right before you
   submit. Fills the opening line and Action Network bets % / money % into
   the workbook. This is the only step that costs API credits.
   Add `sharp_side` (`home`/`away`) by hand if a sharp report names one, and
   `sharp_type`: `book` if a sportsbook reported it, `pro` if it is one
   bettor's pick or one large bet.
3. Copy the ELWAY projection table, then `pbpaste | python3 import_elway.py <week>`.
   Saves the paste to `data/elway/` and fills `elway_line` / `elway_total` /
   `elway_home_wp`. (`python3 import_elway.py <week>` re-imports the saved paste.)
4. `python3 pool.py <week>` → ranked card, top 5 plus the bench, then the
   ELWAY view: its lean on every game vs your line, flagged `agrees` /
   `DISAGREES` against the card.
   (`python3 sample_week.py <week>` shows the plain PLAY/lean/PASS view.)
5. Submit, then log it: `python3 log_week.py submit <week> --picks A,B,C,D,E`.
   This snapshots every game's lines, signals and ELWAY numbers into
   `data/pool_picks_log.csv`, so the log shows what you knew when you picked.
6. Optional: `python3 pool_tracker.py claude <week> A,B,C,D,E` logs Claude's
   picks as a third card.
7. After results: `python3 log_week.py grade <week>`, then
   `python3 pool_tracker.py report`.

**Editing the workbook in Excel:** click **Done**, never **Move to Trash**,
if macOS says it "could not verify" the file. Excel's sandbox tags saved
files with a quarantine flag, and that button deletes the file.

## The report

`python3 pool_tracker.py report` prints, in order:

1. Your submitted picks, the model's card and Claude's picks, each with
   record, win-rate CI, week by week, CLV and value tiers.
2. A week-by-week comparison of the three cards.
3. **ELWAY** — its record wherever its projection differs from your line
   (and from the close), by size of gap, by week, and your picks with vs.
   against it.
4. **Market signals 2026** — every graded game, not just your picks — plus
   how your picks did with vs. against the sharp side.
5. **Market signals** for each past season (vs its pool line if one exists,
   otherwise vs the market close), then all past seasons combined vs the
   market close.

Just the signal tables:
`python3 pool_tracker.py report | sed -n '/MARKET SIGNALS/,$p'`

## Files

| file | what it does |
|---|---|
| `pool.py` | **Main engine.** Ranks every game, returns the best 5, reports expected wins. |
| `sample_week.py` | Loads a week from `weekly_lines.xlsx` (`load_games`) and scores it. |
| `update_mkt.py` | Adds a week's games / refreshes `mkt_line` from nflverse. Never touches `my_line`. |
| `margins.py` | How games finish around a closing line (historical, key numbers included); `clv_prob` turns a line edge into cover probability. |
| `scoring_test.py` | Tests scoring picks by exact cover probability vs the band multiplier (it didn't beat it; see below). |
| `bounce_back_test.py` | Tests betting teams coming off a loss (SU or ATS) at the closing line, 2015-present (no edge; see below). |
| `ml_vs_spread.py` | Can ELWAY make money? Grades registered Kalshi win-vs-spread pairs (`data/elway/kalshi_test_*.csv`) and straight moneyline vs spread bets at sportsbook closing prices. |
| `import_elway.py` | Parses a pasted ELWAY projection table into the workbook (and the log, for submitted weeks). |
| `fetch_splits.py` | Action Network splits via Apify; `import` copies saved splits in without an API call. |
| `log_week.py` | Archive submission-time lines and signals; auto-grade from nflverse. |
| `pool_tracker.py` | Your card vs the model's vs Claude's, CLV, tier calibration, market-signal records. |
| `backtest.py` | The backtest the coefficients come from. |
| `keynumbers.py` | Half-point value by what it crosses; push rates by number. |
| `season_sim.py` | What the edge is worth over a season, with distributions. |
| `pool_backtest_2025.py` | Real out-of-sample test on last year’s pool sheet. |
| `rule_compare.py` | Selection-rule comparison and submission-time sensitivity. |
| `lock_timing.py` | What the pool's lock rule costs. |
| `power_rating_test.py` | Why a projection model is not worth building. |
| `model.py` | Original −110 betting version (has a vig hurdle; `pool.py` does not). |

### Data files

| file | contents |
|---|---|
| `data/weekly_lines.xlsx` | **The file you edit.** One row per 2026 game: `week, away, home, my_line, mkt_line`, plus signals `open_line, home_bets_pct, home_money_pct, sharp_side, sharp_type, signal_notes`. |
| `data/pool_picks_log.csv` | Every game each week at submit time: your pick, the model's pick, signals, closing line, result. |
| `data/claude_picks.csv` | Claude's picks (team only; graded from the log). |
| `data/elway/` | Every ELWAY paste as received (`<season>_wk<NN>.txt`). |
| `data/action_splits.csv` | Every Action Network pull (`an_*` columns); raw responses in `data/raw/action_network/`. |
| `data/signals_<season>.csv` | Hand-collected sharp sides (`sharp_side`, `sharp_type`) and splits for a past season, sources in `signal_notes`. 2023–24: Fox Sports; 2025: VSiN, Yahoo, Fox, SBD. Optional. |
| `data/pool_<season>_merged.csv` | A past season's pool sheet merged with results (2025 only). Seasons without one are graded at the market close. |
| `data/archive/` | Retired: `tracker.py` (−110 bet tracker), `bet_log.csv`, `pool_log.csv`. |

## Two things a pool changes

**No vig.** Breakeven is 50%, not 52.38%. A half point of line value
wins 52.48% — not enough to beat the juice in a real book, but a
genuine edge in a pool. The pool engine is correspondingly more
permissive than `model.py`.

**You must submit 5.** There is no PASS. Some weeks only two or three
games carry real value, so slots 4 and 5 are coin flips. The engine
labels them `coin flip` rather than inventing a reason.



## Overriding the model

`pool.py` proposes; you submit. To record picks that differ from the card,
name the TEAMS you actually took (naming a team fixes the game *and* the
side, so you can take the opposite side from the model):

```
python3 log_week.py submit 4 --picks TEN,BAL,MIA,PIT,DEN
python3 log_week.py submit 4 --picks NYG,BAL,MIA,PIT,DEN --note "why"
```

It prints what you dropped, added or flipped, and what the override costs
in the model's own terms:

```
  dropped  NE +3.5       (53.4%)
  added    PIT +3.5      (model had this at +0.0 pts of value)
  model expected wins : 2.71
  your expected wins  : 2.64   (-0.07)
```

Both cards are stored and graded separately, so `log_week.py overrides`
can later answer whether your judgement actually beats the model. That
needs ~100 override picks to mean anything; until then it is a log, not a
verdict. Recording the override honestly is what keeps the calibration
data usable, so never log a pick you did not send.

## The lock rule (this pool)

Picks lock at the **first game you select**. Take a Thursday game and all
five lock Thursday night; otherwise everything locks Sunday 1pm ET.

**This mostly works in your favour.** Locking Sunday 1pm means the 1pm
games lock at their own kickoff, so your market line for those *is* the
closing line. Roughly 60% of a typical card is 1pm games. The 4:05/4:25
and SNF games have hours of drift left, MNF about a day. So the backtest
measuring against closing lines is close to exact for most of your picks.

**The Thursday option is the one real cost.** Measured on 2025's sheet
(`lock_timing.py`, ranking degraded by noise standing in for how much the
line still had left to move):

| line still to move | wins/90 | win rate |
|---|---|---|
| 0.00 (saw the close) | 55.0 | 61.1% |
| 0.25 (Sun 1pm, 1pm games) | 54.5 | 60.6% |
| 0.50 (Sun 1pm, late games) | 54.3 | 60.3% |
| 0.75 (Thursday lock) | 53.8 | 59.8% |
| 1.00 (volatile week) | 53.5 | 59.4% |
| 1.50 (heavy movement) | 52.5 | 58.3% |

A Thursday lock costs about **1.2 wins a season, 0.07 a week**, spread
across the other four picks. So a Thursday game must beat the Sunday
option it displaces by ~7 percentage points to be worth taking. A typical
card only spans ~3 points from best to fifth pick, so that bar is rarely
cleared. `pool.py` prints the arithmetic when a Thursday game lands on
the card; pass `is_thursday=True` on those games.

**Default: skip Thursday, lock Sunday 1pm.**

## What the backtest found

ATS win rate by points of line value held vs the closing number
(5,790 bet sides):

| value | win rate | 95% CI |
|---|---|---|
| 0.0 | 50.00% | [48.7, 51.3] |
| 0.5 | 52.48% | [51.2, 53.8] |
| 1.0 | 54.51% | [53.2, 55.8] |
| 1.5 | 56.29% | [55.0, 57.6] |
| 2.0 | 57.93% | [56.7, 59.2] |

At the closing line you win exactly 50.00%. The market is efficient;
that check is what validates the rest.

**A point of value is worth more in some places than others:**

| spread band | win rate at 1pt |
|---|---|
| 0–2.5 | 51.8% (a point buys almost nothing) |
| **2.5–3.5** | **56.5%** (best band) |
| 3.5–6.5 | 54.3% |
| **6.5–7.5** | **56.1%** |
| 7.5–10.5 | 53.7% |
| 10.5+ | 53.1% |

**Half-point value depends entirely on what it crosses:**

| move | gain |
|---|---|
| dog +3 → +3.5 | +5.2 pp |
| fav −3.5 → −3 | +5.2 pp |
| dog +7 → +7.5 | +3.2 pp |
| fav −7.5 → −7 | +3.7 pp |
| crosses nothing | +1.6 pp |

Push rates: **3 = 10.5%**, 10 = 8.3%, 7 = 6.1%, 14 = 4.9%, 6 = 4.4%.
Ten pushes more often than seven. The 3 is in a class of its own.

**Every naive system failed.** Home, away, favorites, dogs, home dogs,
+7 dogs, divisional dogs: all 48.6%–52.3%, every CI straddling 50%. If
a "system" doesn't involve getting a better number than the market, it
isn't an edge.

## 2025: the real out-of-sample test

Last year's actual pool sheet (272 games) merged against nflverse
closing lines. Grading reproduces the sheet's own `spread_winner`
column at 100%, so the merge is sound.

**The sheet is genuinely stale.** Mean gap versus the closing line is
0.86 points. Only 37.5% of games sat on the market number.

| gap vs close | games | share |
|---|---|---|
| 0.0 | 102 | 37.5% |
| 0.5 | 54 | 19.9% |
| 1.0 | 69 | 25.4% |
| 1.5 | 8 | 2.9% |
| 2.0 | 11 | 4.0% |
| 3.0 | 14 | 5.1% |
| 4.0 | 5 | 1.8% |

About 6.4 games a week carried ≥1 point of value, which is more than
enough to fill a 5-pick card. The 2026 Week 3 sheet was unusually thin
by comparison.

**The value side won.**

| value held | record | win rate | 95% CI |
|---|---|---|---|
| 0.5 pt | 27-27 | 50.0% | [37.1, 62.9] |
| 1.0 pt | 41-28 | 59.4% | [47.6, 70.2] |
| 2.0+ pt | 27-12 | 69.2% | [53.6, 81.4] |
| any | 99-71 | 58.2% | [50.7, 65.4] |

**The 5-pick model scored 56/90 (62.2%)**, CI [51.9%, 71.5%], versus a
45-win coin-flip baseline. It predicted 50.5 wins and got 56, which is
+1.16 standard errors — consistent with the model and on the lucky side.
A rival picking at 50% beats that only 1.3% of seasons.

### Three caveats that matter more than the headline

**1. The backtest uses closing lines you cannot see when you submit.**
This is the big one. The 62.2% is measured against where the market
*closed*, but your picks lock earlier. Any part of the gap that opened
after you submitted was never available to you. The ranking barely
changes under this assumption, but the usable edge shrinks:

| fraction of gap visible at submission | expected wins |
|---|---|
| 100% | 50.5 |
| 75% | 49.5 |
| 50% | 48.4 |
| 25% | 46.8 |

**Record the market line at the moment you submit each week.** That one
column turns this from an unknown into a measured quantity.

**2. The band multiplier underperformed in 2025.** The 2.5-3.5 band,
which carries the highest multiplier (1.45x), went 18-20 (47.4%). The
10.5+ band, with the lowest multiplier, went 15-5 (75%). Both CIs still
contain the backtested values, so 90 picks cannot overturn 2,895 games.
The multiplier stays, flagged for review.

**3. The rule that "won" is the one to distrust.** Three selection rules
tested on the same 90 picks:

| rule | record | win rate |
|---|---|---|
| model (band-weighted) | 56-34 | 62.2% |
| raw line value only | 58-32 | 64.4% |
| value, tiebreak near 3 | 61-29 | 67.8% |

The best rule beats the default by 5 games, chosen after seeing the
data, from three candidates. That is what overfitting looks like. All
three CIs overlap heavily. `pool.py` prints where the rules disagree so
all three can be tracked honestly over real seasons.

## What the edge is worth (40,000 simulated seasons)

18 weeks × 5 picks = 90 picks. Coin-flip baseline = 45 wins.

| scenario | expected wins | vs baseline | P(beat baseline) |
|---|---|---|---|
| pessimistic | 46.7 | +1.7 | 60.1% |
| observed | 48.3 | +3.3 | 72.5% |
| optimistic | 49.0 | +4.0 | 77.3% |

Head to head over a season:

| opponent skill | you finish ahead |
|---|---|
| 50.0% (coin flip) | 66.3% |
| 51.0% | 61.6% |
| 52.0% | 56.0% |
| 53.0% | 50.6% |
| 54.5% | 42.8% |

**This is an edge, not a lock.** Roughly +3 wins a season and a
two-thirds chance of beating an average opponent. In a pool of a dozen
people that is the difference between contending and mid-pack. It will
not win every year, and a sharp opponent at 53%+ is a coin flip.

## Closing-line value, in points and in probability

CLV is reported both ways. Points treat every half point alike; probability
counts what each half point is actually worth, using the historical
distribution of margins around the closing line (`margins.py`):

| half point | CLV in cover probability | measured in `keynumbers.py` |
|---|---|---|
| through 3 (+3 → +3.5, −3 → −2.5) | +4.8 pts | +5.2 pp |
| through 7 (+7 → +7.5, −7 → −6.5) | +3.1 pts | +3.2 / +3.7 pp |
| through nothing (+4 → +4.5) | +1.4 pts | +1.6 pp |

`pool_tracker.py report` shows mean CLV in both units plus the expected wins
it adds; `log_week.py clv` shows value held at submit vs close in both.

**Scoring picks the same way did not help.** `scoring_test.py` ranked picks by
the exact cover probability of each line instead of the band multiplier. On
2025's pool sheet it went 51-39 vs 57-33 for the current model; over
2015-2025 (leave-one-season-out) the two are equally calibrated (Brier
0.24229 vs 0.24217). So the distribution is used to *measure* CLV, and the
band multiplier still *picks*.

## Bounce-back: teams coming off a loss (no edge)

`bounce_back_test.py` bets every team whose previous game was a loss, at the
closing spread, 2015 to date. 2025, where the pattern was noticed, is kept
apart; the honest test is every other season:

| signal (all seasons but 2025) | bets | cover | 95% CI |
|---|---|---|---|
| off a straight-up loss | 1,182 | 49.0% | 46-52% |
| off an ATS loss | 1,190 | 50.3% | 47-53% |
| off an ATS loss by 10+ | 800 | 52.0% | 49-55% |
| (control) off a straight-up win | 1,174 | 50.9% | 48-54% |

2025 itself was 54.1% off an ATS loss, but on 135 bets (CI 46-62%), and only
5 of 12 seasons clear 52.4%. That is noise around 50%, not an edge, even for
a no-vig pool. Re-run it as the season goes.

## Why the record will lie to you

Power to detect a 3.7-point edge over one season (90 picks) is **10%**.
Ten seasons gets you to 60%. The W-L column simply cannot tell you
whether this works.

**CLV is the honest scoreboard.** Week 3 2026 is the illustration: the
pool card went 2-3, but all five picks beat the closing number by an
average of 0.8 points. Bad week, correct process. Judge the process.

## ELWAY projections

ELWAY publishes a projected home spread for each game. Wherever it differs
from your line, it implies a side: the one your line undervalues. The
report grades that side at your line and at the close, bucketed by the size
of the gap. Started 2026 Week 3: **9-4 vs your lines, 8-4-1 vs the close**
(CI 42–87%, one week).

`pool.py` feeds ELWAY into the card through the existing projection tilt,
at `ELWAY_WEIGHT = 0.5` (top of the `__main__` block). The tilt is capped at
±2 percentage points, so it can reorder close calls but line value still
decides the card. Set it to 0 to show ELWAY without letting it move picks;
raise it only if the report's `gap 1+` row holds up over many weeks.

A projection model's disagreements tend to be underdogs (10 of ELWAY's 13
in Week 3), so watch whether it simply tracks underdog years.

## Market signals (splits and sharp money)

Each signal is graded on **every** game where it was recorded, not just on
the five picks. A season is graded at its pool line when that year's sheet
exists (`data/pool_<season>_merged.csv`: 2025, and 2026 via the log),
otherwise at the nflverse closing line. The report also grades all past
seasons together at the market close, the one common footing. As of 2026
Week 3:

At the market close, by season (2026 is at the pool line, weeks 1–3):

| signal | 2023 | 2024 | 2025 | 2023–25 pooled | 2026 |
|---|---|---|---|---|---|
| sharp side | 2-6 | 25-34, 42% | 55-42, 57%* | 81-82, 50% [42, 57] | 17-8, 68% |
| … reported by books | 1-1 | 4-10 | 53-41, 56%* | 57-52, 52% [43, 61] | 17-8 |
| … one pro / one big bet | 1-5 | 21-24, 47% | 2-1 | 24-30, 44% [32, 58] | – |
| money % ≥ 10 pts above tickets % | 55-58, 49% | 30-41, 42% | 62-39, 61% | 147-138, 52% [46, 57] | 13-15, 46% |
| side with ≤ 35% of tickets | 33-39, 46% | 61-88, 41% | 82-51, 62% | 176-178, 50% [45, 55] | 14-7, 67% |
| side the line moved toward | – | 119-129, 48% | 69-60, 53% | 188-189, 50% [45, 55] | 19-19, 50% |

**Pooled over three seasons, the split signals are worth nothing (50–52%).**
2025 was the outlier, not 2023–24. The seasons differ more than chance
allows (chi-square p = 0.002 for the ≤35% signal), which is the signature
of a regime-dependent pattern, not a stable edge:

- The unpopular side is the underdog ~2/3 of the time, so it partly tracks
  whether dogs had a good year. Dogs covered 53–56% every season 2019–22,
  then only 47% in 2023 and 2024, and 52% in 2025.
- Dog years don't explain all of it: the unpopular side as a *favorite*
  also swung, from 16-29 in 2024 to 28-18 in 2025.
- The 2025 split edge is the same at the market close as at the pool line,
  so it was not the pool sheet's staleness in disguise.

\* 2025's column is at the pool line; the pooled column is at the close.

**The sharp side fails the same test.** Its 2025 result (56%) did not repeat:
pooled over three seasons it is 81-82. Reports where a sportsbook said sharp
money came in are 52% on 109 games; single pro-bettor picks (mostly Randy
McKay via Fox) are 44%. Sharp sides are tagged in `sharp_type` (`book` /
`pro`) so the two can be tracked separately going forward.

Caveats: 2023 has only 8 sharp sides and 2024's are mostly one bettor's
picks, because free sportsbook sharp reports for those years are largely
gone (VSiN's pages 404; Yahoo's URLs now serve newer seasons). And every
sharp side is second-hand and published near kickoff, after the line had
already moved toward it — which is exactly why it should be worth ~50% at
the close.

**Conclusion: none of the market signals is an edge once graded against the
market.** Line value against the pool sheet is. Keep recording the signals
(cheap), but don't let them override line value.

How the data was built, and why it is flattering:

- **Splits and opening lines are Action Network for every game**, so every
  week uses the same source. Where hand-collected values were replaced, the
  old numbers are kept in `signal_notes` as `(was ...)`.
- **Backfilled splits are final numbers**, not what you could see at submit
  time. Only weeks fetched with `--to-workbook` before submitting are a
  clean test.
- **Sharp sides are second-hand** (Yahoo, VSiN, Fox) and cover 83 of 2025's
  games and about half of each 2026 week. Where sources disagreed, the
  sharp side is left blank.
- **Action Network's opening lines** are sometimes the lookahead line, 2+
  points from the game-week open (e.g. 2026 PHI @ CHI). That mostly affects
  the line-movement signal.

The thresholds are `SPLIT_GAP = 10` and `PUBLIC_MAX = 35` at the top of the
signals section in `pool_tracker.py`.

## What could not be tested

- **Splits before 2023.** Action Network's archive thins out going back:
  2023 has splits for 208 of 272 games and no opening lines.
  To add a season: `python3 fetch_splits.py <season> 1-18` for splits
  (2024+), optionally hand-collect sharp sides into
  `data/signals_<season>.csv` (same columns as 2025's), and refresh
  `data/games_clean.csv` if the season is newer than it. The report picks
  up any season it finds; no code changes.
- **Open-to-close movement before 2024.** nflverse archives only the close.
- **Third-party projections.** The engine caps their influence at ±2
  percentage points and requires a credibility weight you supply. The
  source behind the MIA +11.5 pick was running 44% ATS on the season.

## Refreshing the data

```
curl -sSL -o data/games_raw.csv \
  https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv
python3 backtest.py && python3 keynumbers.py
```

Re-apply saved Action Network splits without an API call (empty cells
only; `--overwrite` makes Action Network replace hand-entered values):

```
python3 fetch_splits.py import 2026
python3 fetch_splits.py 2025 3 --from-raw    # re-parse a saved response
```
