# V2 decisions

One row per model step (M), parameterisation step (P) and model change of V2, with its verdict
in the step log's own words and the commit that recorded it. An M-step's verdict is the stop
rule's (a sampler check, not a forecast score); a P-step's is the adoption rule's. The full
narrative, with every table and command, is `harness/archive/steps.md`.

| Step | What it changed | Verdict (the log's words) | Commit |
|---|---|---|---|
| `v2s-m0` | The core: the walk, national and kjördæmi polls, the houses, a static δ, bounded concentrations | GO on all four fits | 3d14e10 |
| `v2s-p1` | The walk anchored at the closed elections' dates (`walk_form = 1`) | KEEP v2s-m0 (fails condition 3: 10 divergences on 2021 f180) | dca33d1 |
| `v2s-m1` | The epoch bias: the industry bias and each house's deviations step at each epoch | GO on both fits | 6cbbb8a |
| `v2s-m2` | The newcomer pool: an estimated mean and scale of a newcomer's entry bias | GO on both fits | 6fa21ce |
| `v2s-m3` | The election-day shock, one per closed election, shared by its kjördæmi | GO on both fits | 436aa8a |
| `v2s-m4` | The δ walk: kjördæmi deviations walk across elections | GO on all four fits | 98bcba6 |
| `v2s-p2` | The δ walk held centred at the closed elections (`delta_form = 1`) | KEEP v2s-m4 (fails condition 3: 8 divergences on 2021 f180) | e647723 |
| `v2s-m5` | The fundamentals pseudo-observation of the latent national shares at E | GO on both fits | b2556b3 |
| `v2` (candidate) | `v2s-m5`'s config under the candidate role, with five one-switch ablations | M5 stands, so every component is built; neither P-step was adopted, so both forms are 0 | b2556b3 |
| Post-review change | A newcomer's house level enters against the existing parties' mean, under `epoch_bias = 1` | One short fit of `v2` on 2021 f180: the stop rule gives GO | cbc53d3, b0c1e17 |
| `v2s-p3` | The anchored walk on V2 at `adapt_delta` 0.95 (lean, one seed) | KEEP V2, on one seed | d86d9ac |
| `v2s-p4` | The anchored walk on V2 at CmdStan's default `adapt_delta`, 0.8 (lean, one seed) | KEEP V2, on one seed (fails conditions 2 and 3) | 1e7eed8 |
| `v2s-m6` | The post-election multiplier: walk steps within 90 days after an election scaled by 1 + τ_post | Closes about a fifth of V2's 2024-eve MAE gap to R0 and none of the 2021 gap | 6e66f92, 4b7223a |
| `v2s-m7` | The calendar drift of the bias on half-yearly and yearly knots, in place of the epoch walk | GO on both runs; by the lean reading m7 does not improve on m6 | 5127f5b, e585274 |
| 8 Nov search | Exploratory variants of `v2s-m6`, fitted outside the repository; x-nn sets `newcomer_pool = 0` | The newcomer pool misleads; x-nn not yet made an arm or a committed run list | a1d2364 |
| Simple core | The file cut to one configuration, with the campaign multiplier on the walk step leaving an election | The cut changed nothing else: log density and gradient equal those of the tagged file at that configuration with the rule added, at 28 points on each of two cells. The rule itself is tested against the R reference (`tests/harness/test-v2-stan.R`) | 1c3a42b |

Two rows have two commits: the first built the step, the second recorded its fits or scores.
The likelihood's rewrite in matrix form (258d3c8) changed computation only and has no row.

**The working model.** On 10 November 2026 the owner chose the simple core as the working
model: a constant poll bias, static constituency deviations, the fundamentals
pseudo-observation, and the campaign multiplier on the walk step leaving an election. It has no
epoch walk, newcomer pool, shock, δ walk, post window or calendar drift. The cut is done:
`Stan/forecast_v2.stan` is that one configuration, the switched file and its harness are at the
tag `v2-switched-final`, and `harness/v2/scoring.md` holds the core's scores against R0.

**What the scores say.** CRPS10 in pp, lower is better (`harness/v2/scoring.md`, written from the
run list `harness/runs/test-elections.txt`):

| Cell | R0 | The core (arm `v2`) | Core − R0 | Outcome noise |
|---|---|---|---|---|
| 2016 at 180 days | 2.433 | 2.418 | −0.015 | 0.813 |
| 2016 at 47 days | 2.333 | 1.880 | −0.453 | 0.613 |
| 2016 at 14 days | 1.569 | 1.829 | 0.260 | 0.420 |
| 2016 eve | 1.453 | 1.317 | −0.137 | 0.410 |
| 2021 at 180 days | 1.507 | 1.351 | −0.157 | 0.200 |
| 2021 eve | 1.080 | 0.779 | −0.300 | 0.222 |
| 2024 eve | 0.848 | 0.850 | 0.002 | 0.060 |

By the election-level reading the core leads R0 by 0.086 pp on 2016 (outcome noise 0.564), by
0.229 pp on 2021 (0.211) and by nothing on 2024. Over the three elections with equal weight the
lead is 0.104 pp against an outcome noise of 0.202 pp, under twice the noise, so the two are
indistinguishable. Over 2021 and 2024 alone, before 2016 was added, it was 0.109 pp against
0.111 pp (the report as it stood at 4e01e64). 2016 made the comparison noisier, not sharper: its
outcome noise is nearly three times that of 2021.

The nine-switch V2 scored 0.891 on 2024 eve and 1.562 on 2021 at 180 days against R0's
1000 + 1000 fits of those cells (`harness/archive/v2/scoring.md`). The core's first three fits
were, draw for draw, those of the 8 November search's variant x-r0w (checked on 10 November 2026;
the search's fits are not in the repository), so they are that trial and add none to the count
below. The 2021 and 2024 runs behind this report are all at 400 + 400. The core's three are
refits with `Stan/forecast_v2.stan` as b507364 left it, and they reproduce the earlier draws
exactly. R0's on 2024 eve and on 2021 at 180 days are refits that replace fits at 1000 + 1000,
which moved its CRPS10 by 0.007 and 0.006 pp; R0's 2021 eve run is the one the earlier report
used.

**2016 as a test election (11 November 2026).** The core was chosen on 10 November on its 2021 and
2024 scores, before the harness could score 2016. The 2016 cells are four forecast days for
E = 2016-10-29 (180, 47 and 14 days and the eve) in a window that opens on 1 July 2012, so the
2013 election is a closed election inside it, with polls on both sides
(`harness/truth/anchor_2013.csv`, `harness/older_polls.csv`). A party with no admitted poll on the
forecast day is scored inside Annað, and the seat scores are marked not comparable, because Annað
holds four official seats in 2016 that no draw can give it (`harness/README.md`).

The cells, the window and the plan to read 2016 twice were fixed before any 2016 fit was scored:
first on the data's own polls with the window opening at the 2013 election (fits at f0fbbe4; the
report is kept as `harness/v2/scoring-2016-first-reading.md`), then with the older polls (fits at
698e0a9; the report is `harness/v2/scoring.md` as it stood at 6f65055). A first set of fits at
400 + 400 did not converge and was discarded on its diagnostics, before any score was computed.
The expectation was written in the working log of 11 November before the first scores; that log
is not in the repository, so the order rests on the author's word. It was that the two arms would
be indistinguishable on 2016 and within 0.15 pp of each other at the eve. Both readings bear it
out. A third reading, which was not planned, followed a ruling on how one kind of poll is read
(the fourth point below; fits at e7a60aa, the report above). CRPS10, pp:

| 2016 cell | National polls: first, later readings | R0, first | Core, first | R0, second | Core, second | R0, third | Core, third |
|---|---|---|---|---|---|---|---|
| 180 days | 67, 140 | 2.488 | 2.425 | 2.422 | 2.364 | 2.433 | 2.418 |
| 47 days | 76, 149 | 1.924 | 1.799 | 2.116 | 1.778 | 2.333 | 1.880 |
| 14 days | 87, 160 | 1.689 | 2.073 | 1.562 | 1.906 | 1.569 | 1.829 |
| Eve | 93, 166 | 1.443 | 1.544 | 1.453 | 1.386 | 1.453 | 1.317 |
| Mean | | 1.886 | 1.960 | 1.888 | 1.859 | 1.947 | 1.861 |

The older polls moved the core's mean by 0.10 pp and R0's by nothing, and the third reading moved
R0's by 0.06 pp and the core's by nothing. Every change is well inside the outcome noise of 0.41
to 0.81 pp per cell, so none changed the verdict.

What 2016 shows of both models, on the third reading: at the eve both arms put
Sjálfstæðisflokkurinn about six points under its result (medians of 22.87 for R0 and 23.41 for
the core against 29.00). The core's medians are closer to the result on every 2016 cell (the MAE
over the named parties is lower by 0.20 to 1.11 pp), but its joint distribution scores worse than
R0's at 180 and 14 days (mDSS) and ties at 47 days and the eve, and it puts Annað at 12.26 on the
eve against a result of 9.36, where R0 has 8.06. Both pass the eve calibration gate (p = 0.139
for the core, 0.123 for R0).

Four things to hold against the 2016 numbers. First, R0's fits have divergent transitions on
every 2016 cell (17 to 57 of 4,000 draws), and its fits at 47 and 14 days have a largest R-hat of
1.09 and 1.06 (the run table of `harness/v2/scoring.md`). The runs' own diagnostics, which are
not in the repository, say more: that R-hat belongs to one constituency-deviation scale
(`sigma_delta[3]`), so R0's kjördæmi scores on those cells are the least reliable in the report,
and every scored national share has an R-hat of at most 1.006 in all eight fits. In the second
reading's fits the same scale mixed badly on the eve instead (R-hat 1.30).

Second, R0 scales its walk in the 47 days before, and on the step leaving, each election on
production's own list (2016, 2017, 2021 and 2024, `R/stan_data.R`), which 2013 is not on. The
core marks the 47 days before every election in its window, 2013 included
(`tests/harness/test-arms-2016.R` checks the marks), and scales the step leaving each. On 2016
the arms differ in that rule too, and R0 is left as production wrote it.

Third, the design of the 2016 cells (the forecast days, the window, the fold) was made knowing
the 2016 result, though before any 2016 score.

Fourth, how a poll with a blank party is read. In thirteen of the national polls a 2016 cell
reads, a party that already had a reported share has none, and the poll's Annað, which is the
remainder, holds its support: Flokkur fólksins in ten of the twenty polls from 13 August 2016,
Viðreisn in two polls of April 2016 and Píratar in one of January 2013
(`harness/sources/README.md`, Known data discrepancies). In the first two readings both arms read
the missing party as not reported and the remainder as Annað alone, so those polls overstated
Annað. On 12 November 2026 the owner ruled that a 2016 cell reads Annað as not reported in such a
poll (e7a60aa: `blank_party_polls()` in `R/harness/info_set.R`; the cells of the other elections
read the same rows as the data hold them), and the eight 2016 runs were refitted. The same commit
keeps polls from 2016 out of the assumed sample sizes of the older polls (eight MMR polls moved,
by at most 10.5 respondents) and takes a 2016 cell's previous shares from the official 2013
result, which reaches no Stan field. The rule was chosen after the second reading's scores were
seen, and after a diagnostic refit of both arms on the eve, with the rule alone, had sized it (the
core's CRPS10 1.310 and R0's 1.449; its fits are not in the repository, and the third reading's
eve, 1.317 and 1.453, agrees with it within Monte Carlo error), so the third reading is
exploratory.

What the rule did. The refit carries the rule and the eight moved sample sizes together, so a
cell's change is theirs jointly. The core's mean over the four cells stayed where it was, its
gains at 14 days and on the eve (0.077 and 0.069 pp) offsetting its losses at 180 and 47 days
(0.054 and 0.102), and R0's mean got worse, by 0.217 pp at 47 days and by 0.011 or less elsewhere.
So the core's larger mean lead on 2016 (0.030 pp, now 0.086) is R0's loss at 47 days and not a net
gain of the core's. On the eve and at 14 days, where the polls that reported Flokkur fólksins from
29 September on had it at 1.2 to 3.8%, the rule did what it was meant to: the core's median for
Annað fell from 12.95 to 12.26 and from 15.40 to 14.74, against a result of 9.36. At 47 days it
cost both arms. That cell's three latest polls are Gallup's of 13 August, the first to report
Flokkur fólksins, at 0.10% (3 of 3,164 respondents), MMR's of 26 August and Fréttablaðið's of 6
September, which leave the party blank and have an Annað of 12.0% and 4.7% (the snapshot's rows).
The rule reads those two as not reporting Annað, which leaves Gallup's 3.9% as the only late
reading, and MMR's Annað ran well above Gallup's that summer (8.2 and 10.3% in MMR's two July
polls against 4.4 and 5.2% in Gallup's of June and July, with every party reported by both). Both
models then put Annað about 1.5 points lower (R0's median from 5.61 to 4.08, the core's from 8.08
to 6.40), further from the result; the cell's change sits on Annað and, as it falls, on Píratar.
At 180 days the rule changes two polls (MMR's of January 2013 and of 4 April 2016) and also moved
Annað away from its result there, 12.90 with Flokkur fólksins folded in: R0's median from 5.57 to
5.37 and the core's from 9.33 to 9.00, with the core's CRPS10 0.054 pp worse, about twice its
Monte Carlo error. The rule's trigger is any earlier positive share, however small.

The core's excess on Annað at the eve is still 2.9 points. Part of it is the constant poll bias:
with one closed election in the window it is what the polls' miss in 2013 taught, and in the
core's eve fit it scales Annað up by about 13% against the polls (a posterior median of −0.13 on
the log scale, 90% interval −0.29 to 0.04, from the run's draws, which are not in the repository).
The rest is the polls: every national poll from 3 October puts Annað between 9.5 and 15.2%, above
the result, whether or not it reports Flokkur fólksins (the snapshot's rows). The core follows
them, and R0's median of 8.06 sits below every one of them.

No model is adopted or changed on the strength of 2016. `Stan/forecast_v2.stan` changed once for it
(b507364: a guard for a cell with no kjördæmi poll; `harness/v2/lp-equivalence-simple-core.md`
records the file before the guard, and three refits of cells that have kjördæmi polls reproduced
the earlier draws exactly).

**How many variants stand behind that choice.** `harness/trials.csv` has a row for every arm with
draws; 17 of them were conceived after scores were seen, so the choice among them was exploratory
on 2021 and 2024. 2016 is the first election the core was scored on after it was chosen, and it
neither confirms nor overturns the choice: the core's lead there is about a seventh of the
outcome noise. Score differences between two arms are read by the election-level reading of
`R/harness/score_report.R` (`election_reading()` and `outcome_noise()` in `R/harness/scores.R`),
not by the per-cell tie rule.

**Where the narrative is.** `harness/archive/steps.md` is the full step log, frozen on
10 November 2026. The diagnostics tables of each step are in `harness/archive/v2/steps/`, and the
build plans are recorded by pull requests #4 (Phase 0), #7 (Phase 1) and #21 (Phase 2).
