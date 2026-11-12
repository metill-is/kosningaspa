# Scores: v2 against r0 (the first reading of 2016, kept as a record)

> This is the first reading of 2016, kept as it was written on 11 November 2026 and superseded by
> `harness/v2/scoring.md`. Its 2016 fits were made at commit f0fbbe4, when a 2016 cell read the
> data's own polls only (67, 76, 87 and 93 national polls on the four forecast days) in a window
> that opened at the 2013 election; the report was written by `R/harness/score_report.R` at commit
> ae0b808. It cannot be regenerated at a later commit, where the 2016 cells also read
> `harness/older_polls.csv`, and its wording is that of the script at the time. Two lines are
> edited by hand: this title and the run-list line. `harness/v2/decisions.md` sets the
> readings side by side.

Written by `R/harness/score_report.R` from `score_cell()` on each run, against the official results.
The lean reading (`harness/README.md`, Checking a model change): one run per arm per cell. SE_diff is
the two runs' batch-means SEs alone, with no seed-to-seed spread, and the spec's adoption rule (section
5.7), written for two decision cells and two seeds per election, is read on each cell alone. A
difference under 2 × SE_diff is a tie, and ties satisfy criteria 2 and 4 but are not "lower".
SE_diff is Monte Carlo error only; the election-level reading below sets each difference against the noise
of the election's outcome, and is the reading to rank the two arms by.

- Run list: the seven cells of `harness/runs/test-elections.txt` (this line is edited by hand: the list that was read named the same cells and was outside the repository)
- Margins: CRPS10 0.05 pp, kjördæmi CRPS 0.1 pp, seat CRPS 0.05 seats per party; calibration gate p > 0.01

| Cell (E, F) | Arm | Run | Iterations (warmup + sampling) | Seed | Commit | Diagnostics flags |
| --- | --- | --- | --- | --- | --- | --- |
| 2016-10-29, 2016-05-02 | r0 | `r0_2016-10-29_2016-05-02_s2701` | 1000 + 1000 | 2701 | f0fbbe450d0d954980d3b9584f0ca353541b9c57 | flags: max R-hat 1.0187 > 1.01; divergent 60 > 0 |
| 2016-10-29, 2016-05-02 | v2 | `v2_2016-10-29_2016-05-02_s2701` | 1000 + 1000 | 2701 | f0fbbe450d0d954980d3b9584f0ca353541b9c57 | flags: max R-hat 1.0128 > 1.01; divergent 22 > 0 |
| 2016-10-29, 2016-09-12 | r0 | `r0_2016-10-29_2016-09-12_s2701` | 1000 + 1000 | 2701 | f0fbbe450d0d954980d3b9584f0ca353541b9c57 | flags: max R-hat 1.0385 > 1.01; divergent 72 > 0 |
| 2016-10-29, 2016-09-12 | v2 | `v2_2016-10-29_2016-09-12_s2701` | 1000 + 1000 | 2701 | f0fbbe450d0d954980d3b9584f0ca353541b9c57 | flags: max R-hat 1.0115 > 1.01; divergent 13 > 0 |
| 2016-10-29, 2016-10-15 | r0 | `r0_2016-10-29_2016-10-15_s2701` | 1000 + 1000 | 2701 | f0fbbe450d0d954980d3b9584f0ca353541b9c57 | flags: max R-hat 1.0569 > 1.01; divergent 109 > 0 |
| 2016-10-29, 2016-10-15 | v2 | `v2_2016-10-29_2016-10-15_s2701` | 1000 + 1000 | 2701 | f0fbbe450d0d954980d3b9584f0ca353541b9c57 | flags: divergent 3 > 0 |
| 2016-10-29, 2016-10-28 | r0 | `r0_2016-10-29_2016-10-28_s2701` | 1000 + 1000 | 2701 | f0fbbe450d0d954980d3b9584f0ca353541b9c57 | flags: max R-hat 1.0106 > 1.01; divergent 80 > 0 |
| 2016-10-29, 2016-10-28 | v2 | `v2_2016-10-29_2016-10-28_s2701` | 1000 + 1000 | 2701 | f0fbbe450d0d954980d3b9584f0ca353541b9c57 | flags: divergent 1 > 0 |
| 2021-09-25, 2021-03-29 | r0 | `r0_2021-09-25_2021-03-29_s2701` | 400 + 400 | 2701 | b507364835250094f7da852328b97c163640824c | flags: max R-hat 1.0329 > 1.01; divergent 1 > 0; min scored bulk ESS 294 < 400 |
| 2021-09-25, 2021-03-29 | v2 | `v2_2021-09-25_2021-03-29_s2701` | 400 + 400 | 2701 | b507364835250094f7da852328b97c163640824c | flags: max R-hat 1.0281 > 1.01 |
| 2021-09-25, 2021-09-24 | r0 | `r0_2021-09-25_2021-09-24_s2701` | 400 + 400 | 2701 | b5cd2b741ec63f4813cdef981960073c27667582 | flags: max R-hat 1.0553 > 1.01; min scored bulk ESS 386 < 400 |
| 2021-09-25, 2021-09-24 | v2 | `v2_2021-09-25_2021-09-24_s2701` | 400 + 400 | 2701 | b507364835250094f7da852328b97c163640824c | flags: max R-hat 1.0199 > 1.01 |
| 2024-11-30, 2024-11-29 | r0 | `r0_2024-11-30_2024-11-29_s2701` | 400 + 400 | 2701 | b507364835250094f7da852328b97c163640824c | flags: max R-hat 1.0233 > 1.01 |
| 2024-11-30, 2024-11-29 | v2 | `v2_2024-11-30_2024-11-29_s2701` | 400 + 400 | 2701 | b507364835250094f7da852328b97c163640824c | flags: max R-hat 1.0250 > 1.01 |

## Election-level reading

CRPS10 in pp, v2 − r0; lower is better. Rank the two arms by this section. The outcome noise of a
cell is the standard deviation of that difference over the election results each forecast itself holds
possible (`outcome_noise()` in `R/harness/scores.R`): how far it would move with the election's outcome
alone. SE_diff says only how precisely the two runs compute their scores. The horizons of one election
share its outcome, so an election's noise is the mean over its cells, and each election has equal weight.
The noise is an estimate, not a bound: it holds both forecasts fixed and takes one of them to be right.

| Cell | v2 − r0 | SE_diff (Monte Carlo) | Outcome noise | Expected if r0 is right | Expected if v2 is right |
| --- | --- | --- | --- | --- | --- |
| 2016 f180 | -0.063 | 0.026 | 0.957 | 1.033 | -1.032 |
| 2016 f47 | -0.125 | 0.020 | 0.444 | 0.495 | -0.508 |
| 2016 f14 | 0.384 | 0.020 | 0.485 | 0.384 | -0.384 |
| 2016 eve | 0.101 | 0.015 | 0.501 | 0.547 | -0.528 |
| 2021 f180 | -0.157 | 0.021 | 0.200 | 0.125 | -0.139 |
| 2021 eve | -0.300 | 0.016 | 0.222 | 0.351 | -0.352 |
| 2024 eve | 0.002 | 0.009 | 0.060 | 0.034 | -0.025 |

| Election | Cells | Mean v2 − r0 | Outcome noise |
| --- | --- | --- | --- |
| 2016-10-29 | 4 | 0.074 | 0.597 |
| 2021-09-25 | 2 | -0.229 | 0.211 |
| 2024-11-30 | 1 | 0.002 | 0.060 |

Over the 3 elections: mean -0.051 pp, outcome noise 0.212 pp, ratio -0.24: indistinguishable (a mean difference under twice its noise).

## MAE

The mean absolute error of each model's median vote share against the result, in points. MAE8 is over the
eight named parties (Annað and Sósíalistaflokkurinn left out, as in the project's earlier backtests), with
the Monte Carlo SE of the medians; MAE over all parties includes them. Kjördæmi MAE is over the named
parties' kjördæmi cells; seat error is the sum over parties of |median seats − seats won|. MAE is
reported, not part of the adoption rule (spec section 5.6).

| Cell | r0: MAE8, pp (SE) | v2: MAE8, pp (SE) | v2 − r0 | Reading | r0: MAE, all parties | v2: MAE, all parties | r0: kjördæmi MAE | v2: kjördæmi MAE | r0: seat error | v2: seat error |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2016 f180 | 3.48 (0.03) | 3.05 (0.03) | -0.43 | better | 3.92 | 3.05 | 3.47 | 2.99 | 19 (not comparable) | 19 (not comparable) |
| 2016 f47 | 2.86 (0.03) | 2.48 (0.02) | -0.38 | better | 3.07 | 2.40 | 2.78 | 2.49 | 17 (not comparable) | 14 (not comparable) |
| 2016 f14 | 2.28 (0.03) | 2.02 (0.02) | -0.25 | better | 2.27 | 2.60 | 2.51 | 2.70 | 15 (not comparable) | 14 (not comparable) |
| 2016 eve | 1.99 (0.02) | 1.80 (0.02) | -0.19 | better | 1.74 | 2.10 | 2.37 | 2.29 | 18 (not comparable) | 13 (not comparable) |
| 2021 f180 | 2.08 (0.05) | 2.11 (0.02) | 0.03 | tie | 2.00 | 1.71 | 2.66 | 2.39 | 12 | 13 |
| 2021 eve | 1.53 (0.01) | 1.28 (0.01) | -0.25 | better | 1.63 | 1.19 | 1.81 | 1.34 | 9 | 11 |
| 2024 eve | 1.18 (0.01) | 1.33 (0.01) | 0.15 | worse | 1.02 | 1.15 | 1.59 | 1.51 | 7 | 8 |

## 2016, f180 (E 2016-10-29, F 2016-05-02)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 2.488 (0.021) | 2.425 (0.015) | -0.063 | 0.026 | better |
| mDSS (co-primary) | -3.47 (0.29) | 5.29 (0.73) | 8.76 | 0.78 | worse |
| d² (p: 0.234, 0.005) | 8.06 (0.33) | 18.38 (0.77) | 10.33 | 0.83 | worse |
| MAE8, pp (the eight named parties) | 3.477 (0.032) | 3.049 (0.030) | -0.427 | 0.044 | better |
| MAE over all parties, pp | 3.923 | 3.055 | -0.868 |  | reported |
| IS90 | 13.72 (0.11) | 24.51 (0.78) | 10.79 | 0.79 | worse |
| National 50% coverage (higher is better) | 2 of 7 (0.06) | 3 of 7 (0.06) | 1 | 0.09 | reported |
| National 90% coverage (higher is better) | 7 of 7 (0.14) | 5 of 7 (0.00) | -2 | 0.14 | reported |
| Kjördæmi CRPS, pp | 2.510 (0.019) | 2.346 (0.013) | -0.164 | 0.023 | better |
| Kjördæmi 90% coverage (higher is better) | 40 of 42 (0.51) | 34 of 42 (0.33) | -6 | 0.61 | reported |
| Kjördæmi MAE over the named cells, pp | 3.475 | 2.986 | -0.489 |  | reported |
| Seat CRPS per party (not comparable) | 2.006 (0.012) | 2.429 (0.015) | 0.423 | 0.019 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 19 | 19 | 0 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 6 of 7 (0.00) | 4 of 7 (0.09) | -2 | 0.09 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.023 (0.002) | 0.328 (0.010) | 0.305 | 0.010 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.022 | 0.009 | -0.014 |  | not comparable |

National parties: 7 (r0), 7 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.568 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.005 against χ² with 6 degrees of freedom, above 0.01: no, reported only: the gate is an eve criterion (r0's p = 0.234).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.063 pp (SE_diff 0.026, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = 8.76 (SE_diff 0.78, worse), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS -0.164 pp (better), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: yes.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 24.49 | 29.18 | 4.51 | 0.18 | 2.922 | 0.749 |
| Vinstri Græn | 15.91 | 13.92 | 14.08 | 1.99 | 1.82 | 1.268 | 1.167 |
| Píratar | 14.48 | 21.03 | 15.15 | 6.55 | 0.67 | 4.042 | 1.393 |
| Annað | 12.90 | 6.30 | 9.81 | 6.60 | 3.09 | 4.279 | 1.841 |
| Framsóknarflokkurinn | 11.49 | 14.35 | 14.50 | 2.86 | 3.01 | 1.772 | 1.816 |
| Viðreisn | 10.48 | 7.88 | 3.41 | 2.60 | 7.07 | 1.588 | 5.597 |
| Samfylkingin | 5.74 | 8.09 | 11.29 | 2.35 | 5.54 | 1.544 | 4.412 |

Of the 7 parties both arms forecast, v2's median is closer to the result on 4 and its CRPS lower on 4 (no MC error per party; consistency evidence only).

## 2016, f47 (E 2016-10-29, F 2016-09-12)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.924 (0.019) | 1.799 (0.006) | -0.125 | 0.020 | better |
| mDSS (co-primary) | -3.21 (0.33) | 7.63 (0.76) | 10.83 | 0.83 | worse |
| d² (p: 0.191, 0.004) | 9.96 (0.39) | 20.98 (0.80) | 11.01 | 0.89 | worse |
| MAE8, pp (the eight named parties) | 2.861 (0.033) | 2.481 (0.022) | -0.380 | 0.039 | better |
| MAE over all parties, pp | 3.073 | 2.403 | -0.670 |  | reported |
| IS90 | 11.63 (0.10) | 20.56 (0.24) | 8.93 | 0.26 | worse |
| National 50% coverage (higher is better) | 3 of 8 (0.15) | 4 of 8 (0.19) | 1 | 0.24 | reported |
| National 90% coverage (higher is better) | 7 of 8 (0.12) | 6 of 8 (0.06) | -1 | 0.14 | reported |
| Kjördæmi CRPS, pp | 1.977 (0.016) | 1.908 (0.007) | -0.070 | 0.017 | better |
| Kjördæmi 90% coverage (higher is better) | 44 of 48 (0.23) | 38 of 48 (0.09) | -6 | 0.25 | reported |
| Kjördæmi MAE over the named cells, pp | 2.777 | 2.491 | -0.285 |  | reported |
| Seat CRPS per party (not comparable) | 1.667 (0.012) | 1.422 (0.005) | -0.245 | 0.013 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 17 | 14 | -3 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 6 of 8 (0.12) | 6 of 8 (0.06) | 0 | 0.14 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.030 (0.003) | 0.090 (0.005) | 0.059 | 0.006 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.044 | 0.105 | 0.061 |  | not comparable |

National parties: 8 (r0), 8 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.630 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.004 against χ² with 7 degrees of freedom, above 0.01: no, reported only: the gate is an eve criterion (r0's p = 0.191).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.125 pp (SE_diff 0.020, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = 10.83 (SE_diff 0.83, worse), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS -0.070 pp (better), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: yes.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 27.14 | 28.62 | 1.86 | 0.38 | 1.153 | 0.825 |
| Vinstri Græn | 15.91 | 13.53 | 12.92 | 2.38 | 2.98 | 1.422 | 1.829 |
| Píratar | 14.48 | 20.60 | 17.32 | 6.11 | 2.84 | 3.797 | 1.910 |
| Framsóknarflokkurinn | 11.49 | 13.03 | 13.11 | 1.54 | 1.62 | 0.987 | 1.044 |
| Viðreisn | 10.48 | 5.74 | 7.89 | 4.74 | 2.59 | 2.651 | 1.812 |
| Annað | 9.36 | 4.80 | 7.50 | 4.56 | 1.86 | 2.938 | 1.075 |
| Samfylkingin | 5.74 | 8.29 | 9.30 | 2.54 | 3.56 | 1.776 | 2.633 |
| Flokkur Fólksins | 3.54 | 2.69 | 0.14 | 0.85 | 3.40 | 0.669 | 3.263 |

Of the 8 parties both arms forecast, v2's median is closer to the result on 4 and its CRPS lower on 4 (no MC error per party; consistency evidence only).

## 2016, f14 (E 2016-10-29, F 2016-10-15)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.689 (0.014) | 2.073 (0.014) | 0.384 | 0.020 | worse |
| mDSS (co-primary) | -3.74 (0.40) | -3.60 (0.33) | 0.14 | 0.51 | tie |
| d² (p: 0.047, 0.103) | 14.26 (0.38) | 11.92 (0.37) | -2.34 | 0.53 | better |
| MAE8, pp (the eight named parties) | 2.277 (0.029) | 2.025 (0.023) | -0.252 | 0.037 | better |
| MAE over all parties, pp | 2.266 | 2.598 | 0.332 |  | reported |
| IS90 | 16.83 (0.24) | 14.85 (0.25) | -1.98 | 0.35 | better |
| National 50% coverage (higher is better) | 5 of 8 (0.15) | 3 of 8 (0.06) | -2 | 0.17 | reported |
| National 90% coverage (higher is better) | 6 of 8 (0.11) | 6 of 8 (0.18) | 0 | 0.21 | reported |
| Kjördæmi CRPS, pp | 1.913 (0.011) | 2.300 (0.012) | 0.388 | 0.016 | worse |
| Kjördæmi 90% coverage (higher is better) | 37 of 48 (0.42) | 37 of 48 (0.29) | 0 | 0.51 | reported |
| Kjördæmi MAE over the named cells, pp | 2.510 | 2.700 | 0.190 |  | reported |
| Seat CRPS per party (not comparable) | 1.603 (0.010) | 1.366 (0.006) | -0.238 | 0.012 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 15 | 14 | -1 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 6 of 8 (0.09) | 6 of 8 (0.00) | 0 | 0.09 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.001 (0.000) | 0.010 (0.001) | 0.009 | 0.001 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.061 | 0.091 | 0.030 |  | not comparable |

National parties: 8 (r0), 8 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.561 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.103 against χ² with 7 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.047).
2. No loss of accuracy: CRPS10 v2 − r0 = 0.384 pp (SE_diff 0.020, worse), at most 0.05 pp worse or a tie: no.
3. Better distribution: mDSS v2 − r0 = 0.14 (SE_diff 0.51, tie), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS 0.388 pp (worse), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: no.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 23.21 | 23.09 | 5.78 | 5.91 | 4.642 | 4.339 |
| Vinstri Græn | 15.91 | 13.23 | 12.20 | 2.67 | 3.71 | 1.753 | 2.419 |
| Píratar | 14.48 | 16.04 | 13.82 | 1.56 | 0.67 | 1.080 | 1.055 |
| Framsóknarflokkurinn | 11.49 | 11.84 | 11.50 | 0.35 | 0.01 | 0.411 | 0.517 |
| Viðreisn | 10.48 | 7.85 | 10.39 | 2.63 | 0.08 | 1.813 | 1.095 |
| Annað | 9.36 | 11.55 | 15.97 | 2.19 | 6.61 | 1.421 | 4.737 |
| Samfylkingin | 5.74 | 7.67 | 7.86 | 1.92 | 2.11 | 1.360 | 1.468 |
| Flokkur Fólksins | 3.54 | 4.55 | 1.85 | 1.02 | 1.69 | 1.030 | 0.952 |

Of the 8 parties both arms forecast, v2's median is closer to the result on 3 and its CRPS lower on 4 (no MC error per party; consistency evidence only).

## 2016, eve (E 2016-10-29, F 2016-10-28)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.443 (0.010) | 1.544 (0.011) | 0.101 | 0.015 | worse |
| mDSS (co-primary) | -5.25 (0.47) | -7.98 (0.38) | -2.72 | 0.61 | better |
| d² (p: 0.030, 0.150) | 15.47 (0.54) | 10.75 (0.43) | -4.71 | 0.69 | better |
| MAE8, pp (the eight named parties) | 1.986 (0.017) | 1.798 (0.020) | -0.188 | 0.026 | better |
| MAE over all parties, pp | 1.741 | 2.103 | 0.362 |  | reported |
| IS90 | 16.99 (0.29) | 11.89 (0.25) | -5.09 | 0.38 | better |
| National 50% coverage (higher is better) | 4 of 8 (0.13) | 3 of 8 (0.06) | -1 | 0.14 | reported |
| National 90% coverage (higher is better) | 7 of 8 (0.12) | 7 of 8 (0.11) | 0 | 0.17 | reported |
| Kjördæmi CRPS, pp | 1.739 (0.009) | 1.843 (0.010) | 0.103 | 0.013 | worse |
| Kjördæmi 90% coverage (higher is better) | 37 of 48 (0.41) | 40 of 48 (0.45) | 3 | 0.61 | reported |
| Kjördæmi MAE over the named cells, pp | 2.366 | 2.291 | -0.075 |  | reported |
| Seat CRPS per party (not comparable) | 1.623 (0.007) | 1.311 (0.006) | -0.312 | 0.009 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 18 | 13 | -4 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 5 of 8 (0.12) | 6 of 8 (0.09) | 1 | 0.15 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.000 (0.000) | 0.003 (0.000) | 0.003 | 0.000 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.068 | 0.041 | -0.027 |  | not comparable |

National parties: 8 (r0), 8 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.594 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.150 against χ² with 7 degrees of freedom, above 0.01: yes (r0's p = 0.030).
2. No loss of accuracy: CRPS10 v2 − r0 = 0.101 pp (SE_diff 0.015, worse), at most 0.05 pp worse or a tie: no.
3. Better distribution: mDSS v2 − r0 = -2.72 (SE_diff 0.61, better), lower beyond a tie or r0 failing the gate: yes.
4. No regional loss: kjördæmi CRPS 0.103 pp (worse), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: no.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 23.07 | 23.47 | 5.93 | 5.52 | 4.937 | 4.141 |
| Vinstri Græn | 15.91 | 14.81 | 13.85 | 1.10 | 2.06 | 0.676 | 1.216 |
| Píratar | 14.48 | 16.40 | 14.15 | 1.92 | 0.34 | 1.145 | 0.854 |
| Framsóknarflokkurinn | 11.49 | 11.89 | 12.21 | 0.39 | 0.72 | 0.363 | 0.573 |
| Viðreisn | 10.48 | 8.81 | 11.49 | 1.67 | 1.02 | 1.635 | 0.915 |
| Annað | 9.36 | 9.34 | 13.60 | 0.02 | 4.23 | 0.600 | 2.865 |
| Samfylkingin | 5.74 | 7.17 | 7.27 | 1.43 | 1.53 | 0.962 | 0.979 |
| Flokkur Fólksins | 3.54 | 5.00 | 2.14 | 1.46 | 1.40 | 1.229 | 0.808 |

Of the 8 parties both arms forecast, v2's median is closer to the result on 4 and its CRPS lower on 4 (no MC error per party; consistency evidence only).

## 2021, f180 (E 2021-09-25, F 2021-03-29)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.507 (0.018) | 1.351 (0.010) | -0.157 | 0.021 | better |
| mDSS (co-primary) | -6.82 (0.37) | -9.44 (0.19) | -2.62 | 0.42 | better |
| d² (p: 0.855, 0.913) | 4.75 (0.41) | 3.98 (0.20) | -0.77 | 0.46 | tie |
| MAE8, pp (the eight named parties) | 2.080 (0.048) | 2.109 (0.019) | 0.029 | 0.052 | tie |
| MAE over all parties, pp | 2.004 | 1.715 | -0.289 |  | reported |
| IS90 | 13.00 (0.31) | 12.25 (0.37) | -0.75 | 0.49 | tie |
| National 50% coverage (higher is better) | 6 of 10 (0.27) | 8 of 10 (0.26) | 2 | 0.37 | reported |
| National 90% coverage (higher is better) | 9 of 10 (0.11) | 9 of 10 (0.11) | 0 | 0.16 | reported |
| Kjördæmi CRPS, pp | 1.714 (0.017) | 1.489 (0.009) | -0.225 | 0.019 | better |
| Kjördæmi 90% coverage (higher is better) | 57 of 60 (0.27) | 57 of 60 (0.20) | 0 | 0.34 | reported |
| Kjördæmi MAE over the named cells, pp | 2.657 | 2.390 | -0.267 |  | reported |
| Seat CRPS per party | 1.115 (0.012) | 1.124 (0.010) | 0.009 | 0.016 | tie |
| Seat absolute error, summed over parties | 12 | 13 | 0 |  | reported |
| Seat 90% coverage (higher is better) | 10 of 10 (0.12) | 9 of 10 (0.13) | -1 | 0.18 | reported |
| Brier score of the incumbents' majority | 0.183 (0.010) | 0.244 (0.010) | 0.060 | 0.014 | worse |
| P(majority for the coalition that formed), report only (higher is better) | 0.572 | 0.506 | -0.066 |  | reported |

National parties: 10 (r0), 10 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.783 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.913 against χ² with 9 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.855).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.157 pp (SE_diff 0.021, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = -2.62 (SE_diff 0.42, better), lower beyond a tie or r0 failing the gate: yes.
4. No regional loss: kjördæmi CRPS -0.225 pp (better), at most 0.1 worse or a tie; seat CRPS per party 0.009 (tie), at most 0.05 worse or a tie: yes.

All four on this cell: yes.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 24.39 | 22.44 | 23.65 | 1.95 | 0.74 | 1.202 | 0.815 |
| Framsóknarflokkurinn | 17.27 | 11.29 | 11.31 | 5.99 | 5.97 | 4.199 | 4.184 |
| Vinstri Græn | 12.57 | 12.43 | 10.84 | 0.15 | 1.73 | 1.008 | 1.186 |
| Samfylkingin | 9.93 | 10.77 | 11.61 | 0.85 | 1.68 | 0.760 | 1.064 |
| Flokkur Fólksins | 8.85 | 5.21 | 5.66 | 3.64 | 3.19 | 2.045 | 1.766 |
| Píratar | 8.63 | 9.85 | 9.64 | 1.22 | 1.02 | 0.997 | 0.885 |
| Viðreisn | 8.33 | 8.78 | 8.26 | 0.45 | 0.07 | 1.127 | 0.987 |
| Miðflokkurinn | 5.45 | 7.85 | 7.92 | 2.40 | 2.47 | 1.655 | 1.630 |
| Sósíalistaflokkurinn | 4.10 | 1.70 | 4.07 | 2.40 | 0.02 | 1.341 | 0.806 |
| Annað | 0.50 | 1.50 | 0.75 | 1.01 | 0.26 | 0.739 | 0.185 |

Of the 10 parties both arms forecast, v2's median is closer to the result on 7 and its CRPS lower on 8 (no MC error per party; consistency evidence only).

## 2021, eve (E 2021-09-25, F 2021-09-24)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.080 (0.015) | 0.779 (0.005) | -0.300 | 0.016 | better |
| mDSS (co-primary) | -4.51 (1.27) | -21.68 (0.40) | -17.18 | 1.33 | better |
| d² (p: 0.002, 0.535) | 26.27 (1.33) | 7.99 (0.46) | -18.28 | 1.40 | better |
| MAE8, pp (the eight named parties) | 1.534 (0.014) | 1.284 (0.014) | -0.250 | 0.020 | better |
| MAE over all parties, pp | 1.632 | 1.187 | -0.445 |  | reported |
| IS90 | 6.93 (0.16) | 5.08 (0.10) | -1.86 | 0.19 | better |
| National 50% coverage (higher is better) | 0 of 10 (0.19) | 3 of 10 (0.15) | 3 | 0.25 | reported |
| National 90% coverage (higher is better) | 7 of 10 (0.14) | 9 of 10 (0.06) | 2 | 0.16 | reported |
| Kjördæmi CRPS, pp | 1.360 (0.013) | 0.873 (0.004) | -0.488 | 0.014 | better |
| Kjördæmi 90% coverage (higher is better) | 38 of 60 (0.79) | 56 of 60 (0.24) | 18 | 0.83 | reported |
| Kjördæmi MAE over the named cells, pp | 1.809 | 1.344 | -0.465 |  | reported |
| Seat CRPS per party | 0.583 (0.007) | 0.658 (0.006) | 0.074 | 0.009 | worse |
| Seat absolute error, summed over parties | 9 | 11 | 2 |  | reported |
| Seat 90% coverage (higher is better) | 9 of 10 (0.06) | 10 of 10 (0.00) | 1 | 0.06 | reported |
| Brier score of the incumbents' majority | 0.006 (0.001) | 0.015 (0.002) | 0.009 | 0.003 | worse |
| P(majority for the coalition that formed), report only (higher is better) | 0.926 | 0.879 | -0.046 |  | reported |

National parties: 10 (r0), 10 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.864 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.535 against χ² with 9 degrees of freedom, above 0.01: yes (r0's p = 0.002).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.300 pp (SE_diff 0.016, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = -17.18 (SE_diff 1.33, better), lower beyond a tie or r0 failing the gate: yes.
4. No regional loss: kjördæmi CRPS -0.488 pp (better), at most 0.1 worse or a tie; seat CRPS per party 0.074 (worse), at most 0.05 worse or a tie: no.

All four on this cell: no.

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 24.39 | 23.28 | 23.55 | 1.11 | 0.84 | 0.682 | 0.535 |
| Framsóknarflokkurinn | 17.27 | 15.82 | 15.85 | 1.46 | 1.42 | 0.887 | 0.868 |
| Vinstri Græn | 12.57 | 10.55 | 10.05 | 2.02 | 2.53 | 1.370 | 1.813 |
| Samfylkingin | 9.93 | 10.64 | 10.30 | 0.72 | 0.37 | 0.425 | 0.332 |
| Flokkur Fólksins | 8.85 | 7.60 | 7.29 | 1.25 | 1.56 | 0.725 | 0.904 |
| Píratar | 8.63 | 9.87 | 9.06 | 1.24 | 0.43 | 0.747 | 0.335 |
| Viðreisn | 8.33 | 12.04 | 10.49 | 3.71 | 2.16 | 2.824 | 1.447 |
| Miðflokkurinn | 5.45 | 6.21 | 6.41 | 0.76 | 0.96 | 0.471 | 0.577 |
| Sósíalistaflokkurinn | 4.10 | 1.17 | 5.47 | 2.93 | 1.38 | 1.821 | 0.844 |
| Annað | 0.50 | 1.61 | 0.72 | 1.12 | 0.22 | 0.848 | 0.138 |

Of the 10 parties both arms forecast, v2's median is closer to the result on 7 and its CRPS lower on 7 (no MC error per party; consistency evidence only).

## 2024, eve (E 2024-11-30, F 2024-11-29)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 0.848 (0.007) | 0.850 (0.005) | 0.002 | 0.009 | tie |
| mDSS (co-primary) | 8.74 (2.74) | -7.33 (0.85) | -16.07 | 2.86 | better |
| d² (p: 0.000, 0.005) | 42.19 (2.76) | 23.78 (0.86) | -18.40 | 2.89 | better |
| MAE8, pp (the eight named parties) | 1.180 (0.013) | 1.326 (0.009) | 0.146 | 0.016 | worse |
| MAE over all parties, pp | 1.021 | 1.149 | 0.128 |  | reported |
| IS90 | 7.56 (0.19) | 6.89 (0.17) | -0.67 | 0.25 | better |
| National 50% coverage (higher is better) | 4 of 10 (0.16) | 4 of 10 (0.12) | 0 | 0.20 | reported |
| National 90% coverage (higher is better) | 6 of 10 (0.13) | 7 of 10 (0.10) | 1 | 0.16 | reported |
| Kjördæmi CRPS, pp | 1.034 (0.005) | 0.938 (0.005) | -0.097 | 0.007 | better |
| Kjördæmi 90% coverage (higher is better) | 39 of 60 (0.36) | 53 of 60 (0.33) | 14 | 0.49 | reported |
| Kjördæmi MAE over the named cells, pp | 1.592 | 1.511 | -0.081 |  | reported |
| Seat CRPS per party | 0.552 (0.009) | 0.596 (0.006) | 0.044 | 0.011 | worse |
| Seat absolute error, summed over parties | 7 | 8 | 1 |  | reported |
| Seat 90% coverage (higher is better) | 10 of 10 (0.09) | 10 of 10 (0.10) | 0 | 0.13 | reported |
| Brier score of the incumbents' majority | 0.000 (0.000) | 0.000 (0.000) | 0.000 | 0.000 | tie |
| P(majority for the coalition that formed), report only (higher is better) | 0.807 | 0.668 | -0.139 |  | reported |

National parties: 10 (r0), 10 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.283 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.005 against χ² with 9 degrees of freedom, above 0.01: no (r0's p = 0.000).
2. No loss of accuracy: CRPS10 v2 − r0 = 0.002 pp (SE_diff 0.009, tie), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = -16.07 (SE_diff 2.86, better), lower beyond a tie or r0 failing the gate: yes.
4. No regional loss: kjördæmi CRPS -0.097 pp (better), at most 0.1 worse or a tie; seat CRPS per party 0.044 (worse), at most 0.05 worse or a tie: yes.

All four on this cell: no.

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Samfylkingin | 20.75 | 17.81 | 17.36 | 2.94 | 3.39 | 2.044 | 2.376 |
| Sjálfstæðisflokkurinn | 19.36 | 17.32 | 17.75 | 2.04 | 1.62 | 1.409 | 1.008 |
| Viðreisn | 15.82 | 15.72 | 15.13 | 0.10 | 0.69 | 0.455 | 0.580 |
| Flokkur Fólksins | 13.78 | 13.51 | 13.52 | 0.27 | 0.26 | 0.485 | 0.545 |
| Miðflokkurinn | 12.10 | 12.03 | 12.78 | 0.07 | 0.69 | 0.398 | 0.538 |
| Framsóknarflokkurinn | 7.80 | 8.45 | 8.66 | 0.65 | 0.86 | 0.394 | 0.521 |
| Sósíalistaflokkurinn | 3.96 | 4.63 | 4.70 | 0.66 | 0.74 | 0.410 | 0.447 |
| Píratar | 3.02 | 4.93 | 4.74 | 1.91 | 1.72 | 1.585 | 1.334 |
| Vinstri Græn | 2.34 | 3.80 | 3.72 | 1.46 | 1.38 | 1.206 | 1.055 |
| Annað | 1.06 | 1.17 | 0.92 | 0.11 | 0.14 | 0.096 | 0.092 |

Of the 10 parties both arms forecast, v2's median is closer to the result on 4 and its CRPS lower on 4 (no MC error per party; consistency evidence only).

