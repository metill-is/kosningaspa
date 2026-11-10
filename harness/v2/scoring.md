# Scores: v2 against r0

Written by `R/harness/score_report.R` from `score_cell()` on each run, against the official results.
The lean reading (`harness/README.md`, Checking a model change): one run per arm per cell. SE_diff is
the two runs' batch-means SEs alone, with no seed-to-seed spread, and the spec's adoption rule (section
5.7), written for two decision cells and two seeds per election, is read on each cell alone. A
difference under 2 × SE_diff is a tie, and ties satisfy criteria 2 and 4 but are not "lower".
SE_diff is Monte Carlo error only; the election-level reading below sets each difference against the noise
of the election's outcome, and is the reading to rank the two arms by.

- Run list: `harness/runs/v2.txt`
- Margins: CRPS10 0.05 pp, kjördæmi CRPS 0.1 pp, seat CRPS 0.05 seats per party; calibration gate p > 0.01

| Cell (E, F) | Arm | Run | Iterations (warmup + sampling) | Seed | Commit | Diagnostics flags |
| --- | --- | --- | --- | --- | --- | --- |
| 2024-11-30, 2024-11-29 | r0 | `r0_2024-11-30_2024-11-29_s2701` | 1000 + 1000 | 2701 | 1e7eed8fb1531ba753bcbf6f79dac6069b3fbc11 | pass |
| 2024-11-30, 2024-11-29 | v2 | `v2_2024-11-30_2024-11-29_s2701` | 400 + 400 | 2701 | b5cd2b741ec63f4813cdef981960073c27667582 | flags: max R-hat 1.0250 > 1.01 |
| 2021-09-25, 2021-09-24 | r0 | `r0_2021-09-25_2021-09-24_s2701` | 400 + 400 | 2701 | b5cd2b741ec63f4813cdef981960073c27667582 | flags: max R-hat 1.0553 > 1.01; min scored bulk ESS 386 < 400 |
| 2021-09-25, 2021-09-24 | v2 | `v2_2021-09-25_2021-09-24_s2701` | 400 + 400 | 2701 | b5cd2b741ec63f4813cdef981960073c27667582 | flags: max R-hat 1.0199 > 1.01 |
| 2021-09-25, 2021-03-29 | r0 | `r0_2021-09-25_2021-03-29_s2701` | 1000 + 1000 | 2701 | 1e7eed8fb1531ba753bcbf6f79dac6069b3fbc11 | flags: max R-hat 1.0117 > 1.01; divergent 2 > 0 |
| 2021-09-25, 2021-03-29 | v2 | `v2_2021-09-25_2021-03-29_s2701` | 400 + 400 | 2701 | b5cd2b741ec63f4813cdef981960073c27667582 | flags: max R-hat 1.0281 > 1.01 |

## Election-level reading

CRPS10 in pp, v2 − r0; lower is better. Rank the two arms by this section. The outcome noise of a
cell is the standard deviation of that difference over the election results each forecast itself holds
possible (`outcome_noise()` in `R/harness/scores.R`): how far it would move with the election's outcome
alone. SE_diff says only how precisely the two runs compute their scores. The horizons of one election
share its outcome, so an election's noise is the mean over its cells, and each election has equal weight.
The noise is an estimate, not a bound: it holds both forecasts fixed and takes one of them to be right.

| Cell | v2 − r0 | SE_diff (Monte Carlo) | Outcome noise | Expected if r0 is right | Expected if v2 is right |
| --- | --- | --- | --- | --- | --- |
| 2024 eve | 0.008 | 0.006 | 0.062 | 0.027 | -0.025 |
| 2021 eve | -0.300 | 0.016 | 0.222 | 0.351 | -0.352 |
| 2021 f180 | -0.150 | 0.017 | 0.205 | 0.131 | -0.149 |

| Election | Cells | Mean v2 − r0 | Outcome noise |
| --- | --- | --- | --- |
| 2024-11-30 | 1 | 0.008 | 0.062 |
| 2021-09-25 | 2 | -0.225 | 0.213 |

Over the 2 elections: mean -0.109 pp, outcome noise 0.111 pp, ratio -0.98: indistinguishable (a mean difference under twice its noise).

## MAE

The mean absolute error of each model's median vote share against the result, in points. MAE8 is over the
eight named parties (Annað and Sósíalistaflokkurinn left out, as in the project's earlier backtests), with
the Monte Carlo SE of the medians; MAE over all parties includes them. Kjördæmi MAE is over the named
parties' kjördæmi cells; seat error is the sum over parties of |median seats − seats won|. MAE is
reported, not part of the adoption rule (spec section 5.6).

| Cell | r0: MAE8, pp (SE) | v2: MAE8, pp (SE) | v2 − r0 | Reading | r0: MAE, all parties | v2: MAE, all parties | r0: kjördæmi MAE | v2: kjördæmi MAE | r0: seat error | v2: seat error |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve | 1.20 (0.01) | 1.33 (0.01) | 0.13 | worse | 1.03 | 1.15 | 1.60 | 1.51 | 7 | 8 |
| 2021 eve | 1.53 (0.01) | 1.28 (0.01) | -0.25 | better | 1.63 | 1.19 | 1.81 | 1.34 | 9 | 11 |
| 2021 f180 | 2.08 (0.03) | 2.11 (0.02) | 0.03 | tie | 2.01 | 1.71 | 2.63 | 2.39 | 13 | 13 |

## 2024, eve (E 2024-11-30, F 2024-11-29)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 0.842 (0.004) | 0.850 (0.005) | 0.008 | 0.006 | tie |
| mDSS (co-primary) | 8.82 (1.52) | -7.33 (0.85) | -16.16 | 1.74 | better |
| d² (p: 0.000, 0.005) | 42.24 (1.59) | 23.78 (0.86) | -18.45 | 1.81 | better |
| MAE8, pp (the eight named parties) | 1.197 (0.006) | 1.326 (0.009) | 0.129 | 0.011 | worse |
| MAE over all parties, pp | 1.031 | 1.149 | 0.117 |  | reported |
| IS90 | 7.46 (0.12) | 6.89 (0.17) | -0.58 | 0.20 | better |
| National 50% coverage (higher is better) | 4 of 10 (0.12) | 4 of 10 (0.12) | 0 | 0.18 | reported |
| National 90% coverage (higher is better) | 6 of 10 (0.19) | 7 of 10 (0.10) | 1 | 0.22 | reported |
| Kjördæmi CRPS, pp | 1.032 (0.003) | 0.938 (0.005) | -0.094 | 0.006 | better |
| Kjördæmi 90% coverage (higher is better) | 38 of 60 (0.29) | 53 of 60 (0.33) | 15 | 0.44 | reported |
| Kjördæmi MAE over the named cells, pp | 1.602 | 1.511 | -0.091 |  | reported |
| Seat CRPS per party | 0.559 (0.006) | 0.596 (0.006) | 0.037 | 0.009 | worse |
| Seat absolute error, summed over parties | 7 | 8 | 1 |  | reported |
| Seat 90% coverage (higher is better) | 10 of 10 (0.00) | 10 of 10 (0.10) | 0 | 0.10 | reported |
| Brier score of the incumbents' majority | 0.000 (0.000) | 0.000 (0.000) | 0.000 | 0.000 | tie |
| P(majority for the coalition that formed), report only (higher is better) | 0.805 | 0.668 | -0.137 |  | reported |

National parties: 10 (r0), 10 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.280 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.005 against χ² with 9 degrees of freedom, above 0.01: no (r0's p = 0.000).
2. No loss of accuracy: CRPS10 v2 − r0 = 0.008 pp (SE_diff 0.006, tie), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = -16.16 (SE_diff 1.74, better), lower beyond a tie or r0 failing the gate: yes.
4. No regional loss: kjördæmi CRPS -0.094 pp (better), at most 0.1 worse or a tie; seat CRPS per party 0.037 (worse), at most 0.05 worse or a tie: yes.

All four on this cell: no.

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Samfylkingin | 20.75 | 17.88 | 17.36 | 2.87 | 3.39 | 1.956 | 2.376 |
| Sjálfstæðisflokkurinn | 19.36 | 17.32 | 17.75 | 2.05 | 1.62 | 1.409 | 1.008 |
| Viðreisn | 15.82 | 15.63 | 15.13 | 0.19 | 0.69 | 0.473 | 0.580 |
| Flokkur Fólksins | 13.78 | 13.41 | 13.52 | 0.37 | 0.26 | 0.498 | 0.545 |
| Miðflokkurinn | 12.10 | 12.11 | 12.78 | 0.01 | 0.69 | 0.380 | 0.538 |
| Framsóknarflokkurinn | 7.80 | 8.49 | 8.66 | 0.69 | 0.86 | 0.416 | 0.521 |
| Sósíalistaflokkurinn | 3.96 | 4.61 | 4.70 | 0.65 | 0.74 | 0.400 | 0.447 |
| Píratar | 3.02 | 4.96 | 4.74 | 1.94 | 1.72 | 1.602 | 1.334 |
| Vinstri Græn | 2.34 | 3.80 | 3.72 | 1.46 | 1.38 | 1.190 | 1.055 |
| Annað | 1.06 | 1.15 | 0.92 | 0.09 | 0.14 | 0.093 | 0.092 |

Of the 10 parties both arms forecast, v2's median is closer to the result on 4 and its CRPS lower on 4 (no MC error per party; consistency evidence only).

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

## 2021, f180 (E 2021-09-25, F 2021-03-29)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.501 (0.014) | 1.351 (0.010) | -0.150 | 0.017 | better |
| mDSS (co-primary) | -7.00 (0.21) | -9.44 (0.19) | -2.44 | 0.28 | better |
| d² (p: 0.864, 0.913) | 4.65 (0.19) | 3.98 (0.20) | -0.67 | 0.27 | better |
| MAE8, pp (the eight named parties) | 2.079 (0.029) | 2.109 (0.019) | 0.029 | 0.035 | tie |
| MAE over all parties, pp | 2.014 | 1.715 | -0.299 |  | reported |
| IS90 | 12.29 (0.29) | 12.25 (0.37) | -0.04 | 0.47 | tie |
| National 50% coverage (higher is better) | 6 of 10 (0.21) | 8 of 10 (0.26) | 2 | 0.33 | reported |
| National 90% coverage (higher is better) | 10 of 10 (0.13) | 9 of 10 (0.11) | -1 | 0.17 | reported |
| Kjördæmi CRPS, pp | 1.701 (0.014) | 1.489 (0.009) | -0.211 | 0.016 | better |
| Kjördæmi 90% coverage (higher is better) | 57 of 60 (0.09) | 57 of 60 (0.20) | 0 | 0.22 | reported |
| Kjördæmi MAE over the named cells, pp | 2.629 | 2.390 | -0.238 |  | reported |
| Seat CRPS per party | 1.094 (0.010) | 1.124 (0.010) | 0.030 | 0.015 | worse |
| Seat absolute error, summed over parties | 13 | 13 | 0 |  | reported |
| Seat 90% coverage (higher is better) | 10 of 10 (0.12) | 9 of 10 (0.13) | -1 | 0.18 | reported |
| Brier score of the incumbents' majority | 0.167 (0.008) | 0.244 (0.010) | 0.077 | 0.013 | worse |
| P(majority for the coalition that formed), report only (higher is better) | 0.592 | 0.506 | -0.085 |  | reported |

National parties: 10 (r0), 10 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.770 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.913 against χ² with 9 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.864).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.150 pp (SE_diff 0.017, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = -2.44 (SE_diff 0.28, better), lower beyond a tie or r0 failing the gate: yes.
4. No regional loss: kjördæmi CRPS -0.211 pp (better), at most 0.1 worse or a tie; seat CRPS per party 0.030 (worse), at most 0.05 worse or a tie: yes.

All four on this cell: yes.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 24.39 | 22.52 | 23.65 | 1.86 | 0.74 | 1.164 | 0.815 |
| Framsóknarflokkurinn | 17.27 | 11.39 | 11.31 | 5.88 | 5.97 | 4.045 | 4.184 |
| Vinstri Græn | 12.57 | 12.50 | 10.84 | 0.08 | 1.73 | 1.031 | 1.186 |
| Samfylkingin | 9.93 | 10.91 | 11.61 | 0.99 | 1.68 | 0.801 | 1.064 |
| Flokkur Fólksins | 8.85 | 5.30 | 5.66 | 3.55 | 3.19 | 2.007 | 1.766 |
| Píratar | 8.63 | 10.02 | 9.64 | 1.39 | 1.02 | 1.014 | 0.885 |
| Viðreisn | 8.33 | 8.84 | 8.26 | 0.52 | 0.07 | 1.120 | 0.987 |
| Miðflokkurinn | 5.45 | 7.82 | 7.92 | 2.37 | 2.47 | 1.654 | 1.630 |
| Sósíalistaflokkurinn | 4.10 | 1.54 | 4.07 | 2.56 | 0.02 | 1.433 | 0.806 |
| Annað | 0.50 | 1.45 | 0.75 | 0.95 | 0.26 | 0.743 | 0.185 |

Of the 10 parties both arms forecast, v2's median is closer to the result on 6 and its CRPS lower on 7 (no MC error per party; consistency evidence only).

