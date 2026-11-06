# Scores: v2 against r0

Written by `R/harness/score_report.R` from `score_cell()` on each run, against the official results.
The lean reading (`harness/README.md`, Checking a model change): one run per arm per cell. SE_diff is
the two runs' batch-means SEs alone, with no seed-to-seed spread, and the spec's adoption rule (section
5.7), written for two decision cells and two seeds per election, is read on each cell alone. A
difference under 2 × SE_diff is a tie, and ties satisfy criteria 2 and 4 but are not "lower".

- Run list: `harness/runs/score-v2-r0.txt`
- Margins: CRPS10 0.05 pp, kjördæmi CRPS 0.1 pp, seat CRPS 0.05 seats per party; calibration gate p > 0.01

| Cell (E, F) | Arm | Run | Iterations (warmup + sampling) | Seed | Commit | Gate (section 5.7) |
| --- | --- | --- | --- | --- | --- | --- |
| 2024-11-30, 2024-11-29 | r0 | `r0_2024-11-30_2024-11-29_s2701` | 1000 + 1000 | 2701 | 1e7eed8fb1531ba753bcbf6f79dac6069b3fbc11 | pass |
| 2024-11-30, 2024-11-29 | v2 | `v2_2024-11-30_2024-11-29_s2701` | 400 + 400 | 2701 | 1234e5921d8debfa81c17b7941e29e13d7a42c5d | FAIL: max R-hat 1.0165 > 1.01 |
| 2021-09-25, 2021-03-29 | r0 | `r0_2021-09-25_2021-03-29_s2701` | 1000 + 1000 | 2701 | 1e7eed8fb1531ba753bcbf6f79dac6069b3fbc11 | FAIL: max R-hat 1.0117 > 1.01; divergent 2 > 0 |
| 2021-09-25, 2021-03-29 | v2 | `v2_2021-09-25_2021-03-29_s2701` | 400 + 400 | 2701 | 1234e5921d8debfa81c17b7941e29e13d7a42c5d | FAIL: max R-hat 1.0182 > 1.01 |

## 2024, eve (E 2024-11-30, F 2024-11-29)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 0.842 (0.004) | 0.891 (0.008) | 0.049 | 0.009 | worse |
| mDSS (co-primary) | 8.82 (1.52) | -8.60 (0.78) | -17.43 | 1.71 | better |
| d² (p: 0.000, 0.018) | 42.24 (1.59) | 19.91 (0.78) | -22.32 | 1.77 | better |
| MAE8, pp | 1.197 (0.006) | 1.498 (0.016) | 0.301 | 0.017 | worse |
| IS90 | 7.46 (0.12) | 6.58 (0.08) | -0.89 | 0.14 | better |
| National 50% coverage (higher is better) | 4 of 10 (0.12) | 2 of 10 (0.24) | -2 | 0.27 | reported |
| National 90% coverage (higher is better) | 6 of 10 (0.19) | 8 of 10 (0.06) | 2 | 0.20 | reported |
| Kjördæmi CRPS, pp | 1.032 (0.003) | 1.008 (0.007) | -0.024 | 0.008 | better |
| Kjördæmi 90% coverage (higher is better) | 38 of 60 (0.29) | 52 of 60 (0.41) | 14 | 0.50 | reported |
| Kjördæmi MAE over the named cells, pp | 1.602 | 1.666 | 0.064 |  | reported |
| Seat CRPS per party | 0.559 (0.006) | 0.674 (0.010) | 0.115 | 0.012 | worse |
| Seat absolute error, summed over parties | 7 | 12 | 5 |  | reported |
| Seat 90% coverage (higher is better) | 10 of 10 (0.00) | 10 of 10 (0.00) | 0 | 0.00 | reported |
| Brier score of the incumbents' majority | 0.000 (0.000) | 0.000 (0.000) | 0.000 | 0.000 | tie |
| P(majority for the coalition that formed), report only (higher is better) | 0.805 | 0.556 | -0.249 |  | reported |

National parties: 10 (r0), 10 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.280 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.018 against χ² with 9 degrees of freedom, above 0.01: yes (r0's p = 0.000).
2. No loss of accuracy: CRPS10 v2 − r0 = 0.049 pp (SE_diff 0.009, worse), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = -17.43 (SE_diff 1.71, better), lower beyond a tie or r0 failing the gate: yes.
4. No regional loss: kjördæmi CRPS -0.024 pp (better), at most 0.1 worse or a tie; seat CRPS per party 0.115 (worse), at most 0.05 worse or a tie: no.

All four on this cell: no.

| Party | r0: CRPS, pp | v2: CRPS, pp | v2 − r0 |
| --- | --- | --- | --- |
| Samfylkingin | 1.956 | 1.276 | -0.679 |
| Píratar | 1.602 | 1.085 | -0.517 |
| Sjálfstæðisflokkurinn | 1.409 | 1.157 | -0.252 |
| Vinstri Græn | 1.190 | 1.034 | -0.156 |
| Miðflokkurinn | 0.380 | 1.182 | 0.801 |
| Flokkur Fólksins | 0.498 | 1.148 | 0.651 |
| Sósíalistaflokkurinn | 0.400 | 0.860 | 0.459 |
| Viðreisn | 0.473 | 0.759 | 0.286 |
| Framsóknarflokkurinn | 0.416 | 0.279 | -0.137 |
| Annað | 0.093 | 0.126 | 0.032 |

v2's national CRPS is lower on 5 of the 10 parties both arms forecast (no MC error per party; consistency evidence only).

## 2021, f180 (E 2021-09-25, F 2021-03-29)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.501 (0.014) | 1.562 (0.015) | 0.061 | 0.021 | worse |
| mDSS (co-primary) | -7.00 (0.21) | -8.13 (0.18) | -1.12 | 0.27 | better |
| d² (p: 0.864, 0.937) | 4.65 (0.19) | 3.57 (0.19) | -1.08 | 0.26 | better |
| MAE8, pp | 2.079 (0.029) | 2.416 (0.032) | 0.336 | 0.043 | worse |
| IS90 | 12.29 (0.29) | 14.04 (0.38) | 1.74 | 0.48 | worse |
| National 50% coverage (higher is better) | 6 of 10 (0.21) | 7 of 10 (0.24) | 1 | 0.32 | reported |
| National 90% coverage (higher is better) | 10 of 10 (0.13) | 9 of 10 (0.10) | -1 | 0.16 | reported |
| Kjördæmi CRPS, pp | 1.701 (0.014) | 1.702 (0.014) | 0.001 | 0.019 | tie |
| Kjördæmi 90% coverage (higher is better) | 57 of 60 (0.09) | 56 of 60 (0.24) | -1 | 0.25 | reported |
| Kjördæmi MAE over the named cells, pp | 2.629 | 2.808 | 0.180 |  | reported |
| Seat CRPS per party | 1.094 (0.010) | 1.294 (0.011) | 0.199 | 0.015 | worse |
| Seat absolute error, summed over parties | 13 | 15 | 2 |  | reported |
| Seat 90% coverage (higher is better) | 10 of 10 (0.12) | 9 of 10 (0.12) | -1 | 0.17 | reported |
| Brier score of the incumbents' majority | 0.167 (0.008) | 0.334 (0.010) | 0.167 | 0.013 | worse |
| P(majority for the coalition that formed), report only (higher is better) | 0.592 | 0.422 | -0.170 |  | reported |

National parties: 10 (r0), 10 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.770 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.937 against χ² with 9 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.864).
2. No loss of accuracy: CRPS10 v2 − r0 = 0.061 pp (SE_diff 0.021, worse), at most 0.05 pp worse or a tie: no.
3. Better distribution: mDSS v2 − r0 = -1.12 (SE_diff 0.27, better), lower beyond a tie or r0 failing the gate: yes.
4. No regional loss: kjördæmi CRPS 0.001 pp (tie), at most 0.1 worse or a tie; seat CRPS per party 0.199 (worse), at most 0.05 worse or a tie: no.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | r0: CRPS, pp | v2: CRPS, pp | v2 − r0 |
| --- | --- | --- | --- |
| Framsóknarflokkurinn | 4.045 | 4.701 | 0.657 |
| Flokkur Fólksins | 2.007 | 2.110 | 0.104 |
| Miðflokkurinn | 1.654 | 1.950 | 0.297 |
| Sósíalistaflokkurinn | 1.433 | 1.105 | -0.328 |
| Sjálfstæðisflokkurinn | 1.164 | 1.279 | 0.115 |
| Vinstri Græn | 1.031 | 1.241 | 0.209 |
| Viðreisn | 1.120 | 1.110 | -0.010 |
| Píratar | 1.014 | 1.011 | -0.003 |
| Samfylkingin | 0.801 | 0.884 | 0.083 |
| Annað | 0.743 | 0.226 | -0.516 |

v2's national CRPS is lower on 4 of the 10 parties both arms forecast (no MC error per party; consistency evidence only).

