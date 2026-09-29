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

## Weekly workflow

1. Put the week's games in `sample_week.py`: the pool sheet's number
   (`my_line`) and the current market number (`mkt_line`), both
   **home-perspective** (negative = home favored).
2. `python3 pool.py` → ranked card, top 5 plus the bench.
3. Submit. Log the 5 in `data/pool_log.csv`.
4. After results: `python3 pool_tracker.py report`.

## Files

| file | what it does |
|---|---|
| `pool.py` | **Main engine.** Ranks every game, returns the best 5, reports expected wins. |
| `pool_tracker.py` | Weekly record, season standings vs baseline, CLV, tier calibration. |
| `backtest.py` | The backtest the coefficients come from. |
| `keynumbers.py` | Half-point value by what it crosses; push rates by number. |
| `season_sim.py` | What the edge is worth over a season, with distributions. |
| `pool_backtest_2025.py` | Real out-of-sample test on last year’s pool sheet. |
| `rule_compare.py` | Selection-rule comparison and submission-time sensitivity. |
| `lock_timing.py` | What the pool's lock rule costs. |
| `log_week.py` | Archive submission-time lines; auto-grade from nflverse. |
| `power_rating_test.py` | Why a projection model is not worth building. |
| `model.py` | Original −110 betting version (has a vig hurdle; `pool.py` does not). |

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

## Why the record will lie to you

Power to detect a 3.7-point edge over one season (90 picks) is **10%**.
Ten seasons gets you to 60%. The W-L column simply cannot tell you
whether this works.

**CLV is the honest scoreboard.** Week 3 2026 is the illustration: the
pool card went 2-3, but all five picks beat the closing number by an
average of 0.8 points. Bad week, correct process. Judge the process.

## What could not be tested

- **Betting splits (ticket % / handle %).** No free historical archive.
  Forward-trackable only, never validated.
- **Reported sharp entry points.** Same problem, and second-hand.
- **Open-to-close movement.** nflverse archives only the close.
- **Third-party projections.** The engine caps their influence at ±2
  percentage points and requires a credibility weight you supply. The
  source behind the MIA +11.5 pick was running 44% ATS on the season.

## Refreshing the data

```
curl -sSL -o data/games_raw.csv \
  https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv
python3 backtest.py && python3 keynumbers.py
```
