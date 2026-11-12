# Scores: v2 against r0

Written by `R/harness/score_report.R` from `score_cell()` on each run, against the official results.
The lean reading (`harness/README.md`, Tests: the lean check for a model change): one run per arm per
cell. SE_diff is the two runs' batch-means SEs alone, with no seed-to-seed spread, and the spec's adoption
rule (section 5.7), written for two decision cells and two seeds per election, is read on each cell alone. A
difference under 2 × SE_diff is a tie, and ties satisfy criteria 2 and 4 but are not "lower".
SE_diff is Monte Carlo error only; the election-level reading below sets each difference against the noise
of the election's outcome, and is the reading to rank the two arms by.

- Run list: `harness/runs/test-elections.txt`
- Margins: CRPS10 0.05 pp, kjördæmi CRPS 0.1 pp, seat CRPS 0.05 seats per party; calibration gate p > 0.01

| Cell (E, F) | Arm | Run | Iterations (warmup + sampling) | Seed | Commit | Diagnostics flags |
| --- | --- | --- | --- | --- | --- | --- |
| 2016-10-29, 2016-05-02 | r0 | `r0_2016-10-29_2016-05-02_s2701` | 1000 + 1000 | 2701 | 698e0a902b69f960f4c32451feeac40257425b05 | flags: max R-hat 1.0400 > 1.01; divergent 5 > 0; min scored bulk ESS 301 < 400 |
| 2016-10-29, 2016-05-02 | v2 | `v2_2016-10-29_2016-05-02_s2701` | 1000 + 1000 | 2701 | 698e0a902b69f960f4c32451feeac40257425b05 | flags: max R-hat 1.0215 > 1.01; divergent 1 > 0 |
| 2016-10-29, 2016-09-12 | r0 | `r0_2016-10-29_2016-09-12_s2701` | 1000 + 1000 | 2701 | 698e0a902b69f960f4c32451feeac40257425b05 | flags: max R-hat 1.0152 > 1.01; divergent 57 > 0 |
| 2016-10-29, 2016-09-12 | v2 | `v2_2016-10-29_2016-09-12_s2701` | 1000 + 1000 | 2701 | 698e0a902b69f960f4c32451feeac40257425b05 | flags: max R-hat 1.0268 > 1.01 |
| 2016-10-29, 2016-10-15 | r0 | `r0_2016-10-29_2016-10-15_s2701` | 1000 + 1000 | 2701 | 698e0a902b69f960f4c32451feeac40257425b05 | flags: max R-hat 1.0219 > 1.01; divergent 55 > 0 |
| 2016-10-29, 2016-10-15 | v2 | `v2_2016-10-29_2016-10-15_s2701` | 1000 + 1000 | 2701 | 698e0a902b69f960f4c32451feeac40257425b05 | flags: max R-hat 1.0138 > 1.01 |
| 2016-10-29, 2016-10-28 | r0 | `r0_2016-10-29_2016-10-28_s2701` | 1000 + 1000 | 2701 | 698e0a902b69f960f4c32451feeac40257425b05 | flags: max R-hat 1.2986 > 1.01; divergent 55 > 0 |
| 2016-10-29, 2016-10-28 | v2 | `v2_2016-10-29_2016-10-28_s2701` | 1000 + 1000 | 2701 | 698e0a902b69f960f4c32451feeac40257425b05 | flags: max R-hat 1.0220 > 1.01; divergent 1 > 0 |
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
| 2016 f180 | -0.058 | 0.023 | 0.804 | 0.901 | -0.842 |
| 2016 f47 | -0.338 | 0.022 | 0.620 | 0.458 | -0.486 |
| 2016 f14 | 0.344 | 0.015 | 0.435 | 0.326 | -0.362 |
| 2016 eve | -0.067 | 0.013 | 0.437 | 0.571 | -0.554 |
| 2021 f180 | -0.157 | 0.021 | 0.200 | 0.125 | -0.139 |
| 2021 eve | -0.300 | 0.016 | 0.222 | 0.351 | -0.352 |
| 2024 eve | 0.002 | 0.009 | 0.060 | 0.034 | -0.025 |

| Election | Cells | Mean v2 − r0 | Outcome noise |
| --- | --- | --- | --- |
| 2016-10-29 | 4 | -0.030 | 0.574 |
| 2021-09-25 | 2 | -0.229 | 0.211 |
| 2024-11-30 | 1 | 0.002 | 0.060 |

Over the 3 elections: mean -0.086 pp, outcome noise 0.205 pp, ratio -0.42: indistinguishable (a mean difference under twice its noise).

## MAE

The mean absolute error of each model's median vote share against the result, in points. MAE8 is over
those of the eight named parties (Annað and Sósíalistaflokkurinn left out, as in the project's earlier
backtests) that the cell scores, with the Monte Carlo SE of the medians. A named party with no list at the
election, or one scored inside Annað, is left out, so the number of parties can differ from cell to cell;
the MAE8 row of each cell's section gives it. MAE over all parties is over every party the cell scores. Kjördæmi MAE
is over the named parties' kjördæmi cells; seat error is the sum over parties of |median seats
− seats won|, a whole number or a half. MAE is reported, not part of the adoption rule (spec section 5.6).

| Cell | r0: MAE8, pp (SE) | v2: MAE8, pp (SE) | v2 − r0 | Reading | r0: MAE, all parties | v2: MAE, all parties | r0: kjördæmi MAE | v2: kjördæmi MAE | r0: seat error | v2: seat error |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2016 f180 | 2.93 (0.04) | 2.41 (0.03) | -0.52 | better | 3.56 | 2.58 | 3.08 | 2.86 | 17 (not comparable) | 19 (not comparable) |
| 2016 f47 | 3.33 (0.03) | 2.26 (0.03) | -1.07 | better | 3.39 | 2.14 | 3.29 | 2.55 | 22 (not comparable) | 14 (not comparable) |
| 2016 f14 | 2.13 (0.02) | 1.98 (0.02) | -0.15 | better | 2.04 | 2.49 | 2.52 | 2.67 | 15 (not comparable) | 14 (not comparable) |
| 2016 eve | 1.96 (0.03) | 1.56 (0.02) | -0.40 | better | 1.81 | 1.81 | 2.45 | 2.30 | 20 (not comparable) | 13 (not comparable) |
| 2021 f180 | 2.08 (0.05) | 2.11 (0.02) | 0.03 | tie | 2.00 | 1.71 | 2.66 | 2.39 | 12.5 | 13 |
| 2021 eve | 1.53 (0.01) | 1.28 (0.01) | -0.25 | better | 1.63 | 1.19 | 1.81 | 1.34 | 9 | 11 |
| 2024 eve | 1.18 (0.01) | 1.33 (0.01) | 0.15 | worse | 1.02 | 1.15 | 1.59 | 1.51 | 7 | 8 |

## 2016, f180 (E 2016-10-29, F 2016-05-02)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 2.422 (0.017) | 2.364 (0.015) | -0.058 | 0.023 | better |
| mDSS (co-primary) | -1.21 (0.31) | 4.19 (0.60) | 5.40 | 0.67 | worse |
| d² (p: 0.126, 0.019) | 9.96 (0.28) | 15.11 (0.63) | 5.15 | 0.69 | worse |
| MAE8, pp (over the 6 named parties scored) | 2.934 (0.043) | 2.411 (0.029) | -0.523 | 0.052 | better |
| MAE over all parties, pp | 3.563 | 2.577 | -0.986 |  | reported |
| IS90 | 16.76 (0.43) | 23.89 (0.65) | 7.13 | 0.78 | worse |
| National 50% coverage (higher is better) | 3 of 7 (0.16) | 4 of 7 (0.00) | 1 | 0.16 | reported |
| National 90% coverage (higher is better) | 6 of 7 (0.13) | 5 of 7 (0.06) | -1 | 0.14 | reported |
| Kjördæmi CRPS, pp | 2.475 (0.016) | 2.337 (0.014) | -0.138 | 0.021 | better |
| Kjördæmi 90% coverage (higher is better) | 37 of 42 (0.41) | 35 of 42 (0.14) | -2 | 0.43 | reported |
| Kjördæmi MAE over the named cells, pp | 3.079 | 2.857 | -0.221 |  | reported |
| Seat CRPS per party (not comparable) | 1.872 (0.014) | 2.419 (0.015) | 0.547 | 0.021 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 17 | 19 | 2 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 6 of 7 (0.06) | 4 of 7 (0.10) | -2 | 0.12 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.022 (0.002) | 0.129 (0.005) | 0.107 | 0.006 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.025 | 0.024 | -0.002 |  | not comparable |

National parties: 7 (r0), 7 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.598 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.019 against χ² with 6 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.126).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.058 pp (SE_diff 0.023, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = 5.40 (SE_diff 0.67, worse), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS -0.138 pp (better), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: yes.

On this cell Flokkur Fólksins is scored inside Annað, having had no admitted poll by F, so Annað's result includes its share.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 24.94 | 28.80 | 4.06 | 0.20 | 2.400 | 1.084 |
| Vinstri Græn | 15.91 | 15.40 | 15.86 | 0.50 | 0.05 | 1.020 | 1.013 |
| Píratar | 14.48 | 20.36 | 16.52 | 5.87 | 2.04 | 3.710 | 2.034 |
| Annað | 12.90 | 5.57 | 9.33 | 7.33 | 3.57 | 5.296 | 2.088 |
| Framsóknarflokkurinn | 11.49 | 13.82 | 11.64 | 2.33 | 0.15 | 1.519 | 1.036 |
| Viðreisn | 10.48 | 8.03 | 3.41 | 2.45 | 7.07 | 1.443 | 5.549 |
| Samfylkingin | 5.74 | 8.13 | 10.71 | 2.39 | 4.96 | 1.564 | 3.744 |

Of the 7 parties both arms forecast, v2's median is closer to the result on 5 and its CRPS lower on 5 (no MC error per party; consistency evidence only).

## 2016, f47 (E 2016-10-29, F 2016-09-12)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 2.116 (0.018) | 1.778 (0.013) | -0.338 | 0.022 | better |
| mDSS (co-primary) | 1.46 (0.32) | 5.34 (0.49) | 3.88 | 0.59 | worse |
| d² (p: 0.055, 0.025) | 13.81 (0.37) | 16.03 (0.53) | 2.21 | 0.65 | worse |
| MAE8, pp (over the 7 named parties scored) | 3.334 (0.028) | 2.263 (0.029) | -1.071 | 0.040 | better |
| MAE over all parties, pp | 3.387 | 2.141 | -1.246 |  | reported |
| IS90 | 12.31 (0.09) | 22.30 (0.25) | 10.00 | 0.27 | worse |
| National 50% coverage (higher is better) | 2 of 8 (0.12) | 6 of 8 (0.00) | 4 | 0.12 | reported |
| National 90% coverage (higher is better) | 7 of 8 (0.12) | 7 of 8 (0.09) | 0 | 0.15 | reported |
| Kjördæmi CRPS, pp | 2.150 (0.014) | 1.953 (0.012) | -0.197 | 0.018 | better |
| Kjördæmi 90% coverage (higher is better) | 45 of 48 (0.26) | 41 of 48 (0.21) | -4 | 0.33 | reported |
| Kjördæmi MAE over the named cells, pp | 3.291 | 2.555 | -0.737 |  | reported |
| Seat CRPS per party (not comparable) | 1.874 (0.013) | 1.494 (0.009) | -0.380 | 0.016 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 22 | 14 | -8 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 6 of 8 (0.12) | 7 of 8 (0.06) | 1 | 0.14 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.032 (0.002) | 0.040 (0.003) | 0.009 | 0.003 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.033 | 0.192 | 0.159 |  | not comparable |

National parties: 8 (r0), 8 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.581 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.025 against χ² with 7 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.055).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.338 pp (SE_diff 0.022, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = 3.88 (SE_diff 0.59, worse), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS -0.197 pp (better), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: yes.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 26.15 | 27.12 | 2.85 | 1.88 | 1.719 | 1.446 |
| Vinstri Græn | 15.91 | 14.43 | 14.09 | 1.48 | 1.82 | 0.981 | 1.212 |
| Píratar | 14.48 | 21.80 | 17.05 | 7.31 | 2.57 | 4.675 | 2.018 |
| Framsóknarflokkurinn | 11.49 | 12.84 | 10.81 | 1.35 | 0.68 | 1.001 | 0.979 |
| Viðreisn | 10.48 | 4.58 | 8.18 | 5.90 | 2.30 | 3.359 | 2.259 |
| Annað | 9.36 | 5.61 | 8.08 | 3.75 | 1.28 | 2.332 | 0.915 |
| Samfylkingin | 5.74 | 8.38 | 8.95 | 2.64 | 3.21 | 1.845 | 2.157 |
| Flokkur Fólksins | 3.54 | 1.72 | 0.14 | 1.81 | 3.39 | 1.015 | 3.238 |

Of the 8 parties both arms forecast, v2's median is closer to the result on 5 and its CRPS lower on 5 (no MC error per party; consistency evidence only).

## 2016, f14 (E 2016-10-29, F 2016-10-15)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.562 (0.011) | 1.906 (0.009) | 0.344 | 0.015 | worse |
| mDSS (co-primary) | -6.33 (0.28) | -3.13 (0.43) | 3.19 | 0.51 | worse |
| d² (p: 0.181, 0.111) | 10.14 (0.32) | 11.69 (0.47) | 1.55 | 0.57 | worse |
| MAE8, pp (over the 7 named parties scored) | 2.130 (0.022) | 1.981 (0.024) | -0.149 | 0.033 | better |
| MAE over all parties, pp | 2.040 | 2.488 | 0.448 |  | reported |
| IS90 | 15.96 (0.40) | 14.89 (0.32) | -1.08 | 0.51 | better |
| National 50% coverage (higher is better) | 6 of 8 (0.00) | 4 of 8 (0.18) | -2 | 0.18 | reported |
| National 90% coverage (higher is better) | 7 of 8 (0.09) | 6 of 8 (0.10) | -1 | 0.13 | reported |
| Kjördæmi CRPS, pp | 1.855 (0.010) | 2.199 (0.009) | 0.344 | 0.013 | worse |
| Kjördæmi 90% coverage (higher is better) | 40 of 48 (0.31) | 38 of 48 (0.35) | -2 | 0.47 | reported |
| Kjördæmi MAE over the named cells, pp | 2.519 | 2.671 | 0.152 |  | reported |
| Seat CRPS per party (not comparable) | 1.664 (0.009) | 1.413 (0.007) | -0.251 | 0.012 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 15 | 14 | -1 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 6 of 8 (0.14) | 6 of 8 (0.11) | 0 | 0.18 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.000 (0.000) | 0.001 (0.000) | 0.001 | 0.000 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.091 | 0.097 | 0.006 |  | not comparable |

National parties: 8 (r0), 8 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.528 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.111 against χ² with 7 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.181).
2. No loss of accuracy: CRPS10 v2 − r0 = 0.344 pp (SE_diff 0.015, worse), at most 0.05 pp worse or a tie: no.
3. Better distribution: mDSS v2 − r0 = 3.19 (SE_diff 0.51, worse), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS 0.344 pp (worse), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: no.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 22.45 | 22.65 | 6.55 | 6.35 | 4.928 | 4.682 |
| Vinstri Græn | 15.91 | 14.85 | 14.26 | 1.06 | 1.65 | 0.724 | 1.011 |
| Píratar | 14.48 | 15.95 | 15.02 | 1.47 | 0.53 | 1.137 | 1.112 |
| Framsóknarflokkurinn | 11.49 | 10.61 | 9.77 | 0.88 | 1.72 | 0.617 | 1.021 |
| Viðreisn | 10.48 | 8.53 | 10.21 | 1.94 | 0.27 | 1.763 | 1.250 |
| Annað | 9.36 | 10.77 | 15.40 | 1.41 | 6.03 | 1.066 | 4.172 |
| Samfylkingin | 5.74 | 7.41 | 7.38 | 1.67 | 1.64 | 1.112 | 1.033 |
| Flokkur Fólksins | 3.54 | 4.88 | 1.83 | 1.35 | 1.71 | 1.153 | 0.969 |

Of the 8 parties both arms forecast, v2's median is closer to the result on 4 and its CRPS lower on 5 (no MC error per party; consistency evidence only).

## 2016, eve (E 2016-10-29, F 2016-10-28)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.453 (0.009) | 1.386 (0.009) | -0.067 | 0.013 | better |
| mDSS (co-primary) | -8.23 (0.45) | -8.18 (0.20) | 0.06 | 0.50 | tie |
| d² (p: 0.071, 0.079) | 13.04 (0.49) | 12.72 (0.20) | -0.32 | 0.53 | tie |
| MAE8, pp (over the 7 named parties scored) | 1.957 (0.030) | 1.561 (0.019) | -0.396 | 0.036 | better |
| MAE over all parties, pp | 1.813 | 1.815 | 0.002 |  | reported |
| IS90 | 16.81 (0.18) | 13.04 (0.19) | -3.77 | 0.27 | better |
| National 50% coverage (higher is better) | 4 of 8 (0.16) | 4 of 8 (0.19) | 0 | 0.25 | reported |
| National 90% coverage (higher is better) | 7 of 8 (0.00) | 6 of 8 (0.00) | -1 | 0.00 | reported |
| Kjördæmi CRPS, pp | 1.808 (0.006) | 1.794 (0.008) | -0.014 | 0.010 | tie |
| Kjördæmi 90% coverage (higher is better) | 36 of 48 (0.19) | 37 of 48 (0.18) | 1 | 0.26 | reported |
| Kjördæmi MAE over the named cells, pp | 2.447 | 2.301 | -0.147 |  | reported |
| Seat CRPS per party (not comparable) | 1.744 (0.006) | 1.394 (0.007) | -0.350 | 0.009 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 20 | 13 | -7 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 6 of 8 (0.00) | 7 of 8 (0.13) | 1 | 0.13 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.000 (0.000) | 0.000 (0.000) | 0.000 | 0.000 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.086 | 0.025 | -0.061 |  | not comparable |

National parties: 8 (r0), 8 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.599 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.079 against χ² with 7 degrees of freedom, above 0.01: yes (r0's p = 0.071).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.067 pp (SE_diff 0.013, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = 0.06 (SE_diff 0.50, tie), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS -0.014 pp (tie), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: yes.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 22.75 | 23.29 | 6.25 | 5.71 | 5.126 | 4.485 |
| Vinstri Græn | 15.91 | 16.51 | 16.14 | 0.60 | 0.23 | 0.483 | 0.465 |
| Píratar | 14.48 | 16.47 | 16.13 | 1.99 | 1.65 | 1.181 | 1.058 |
| Framsóknarflokkurinn | 11.49 | 10.55 | 10.52 | 0.94 | 0.97 | 0.570 | 0.601 |
| Viðreisn | 10.48 | 9.47 | 10.89 | 1.01 | 0.42 | 1.668 | 0.768 |
| Annað | 9.36 | 8.56 | 12.95 | 0.80 | 3.59 | 0.589 | 2.557 |
| Samfylkingin | 5.74 | 6.49 | 6.42 | 0.75 | 0.68 | 0.443 | 0.415 |
| Flokkur Fólksins | 3.54 | 5.69 | 2.27 | 2.15 | 1.27 | 1.566 | 0.740 |

Of the 8 parties both arms forecast, v2's median is closer to the result on 6 and its CRPS lower on 6 (no MC error per party; consistency evidence only).

## 2021, f180 (E 2021-09-25, F 2021-03-29)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.507 (0.018) | 1.351 (0.010) | -0.157 | 0.021 | better |
| mDSS (co-primary) | -6.82 (0.37) | -9.44 (0.19) | -2.62 | 0.42 | better |
| d² (p: 0.855, 0.913) | 4.75 (0.41) | 3.98 (0.20) | -0.77 | 0.46 | tie |
| MAE8, pp (over the 8 named parties scored) | 2.080 (0.048) | 2.109 (0.019) | 0.029 | 0.052 | tie |
| MAE over all parties, pp | 2.004 | 1.715 | -0.289 |  | reported |
| IS90 | 13.00 (0.31) | 12.25 (0.37) | -0.75 | 0.49 | tie |
| National 50% coverage (higher is better) | 6 of 10 (0.27) | 8 of 10 (0.26) | 2 | 0.37 | reported |
| National 90% coverage (higher is better) | 9 of 10 (0.11) | 9 of 10 (0.11) | 0 | 0.16 | reported |
| Kjördæmi CRPS, pp | 1.714 (0.017) | 1.489 (0.009) | -0.225 | 0.019 | better |
| Kjördæmi 90% coverage (higher is better) | 57 of 60 (0.27) | 57 of 60 (0.20) | 0 | 0.34 | reported |
| Kjördæmi MAE over the named cells, pp | 2.657 | 2.390 | -0.267 |  | reported |
| Seat CRPS per party | 1.115 (0.012) | 1.124 (0.010) | 0.009 | 0.016 | tie |
| Seat absolute error, summed over parties | 12.5 | 13 | 0.5 |  | reported |
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
| MAE8, pp (over the 8 named parties scored) | 1.534 (0.014) | 1.284 (0.014) | -0.250 | 0.020 | better |
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
| MAE8, pp (over the 8 named parties scored) | 1.180 (0.013) | 1.326 (0.009) | 0.146 | 0.016 | worse |
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

