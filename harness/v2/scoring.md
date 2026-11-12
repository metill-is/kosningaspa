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
| 2016-10-29, 2016-05-02 | r0 | `r0_2016-10-29_2016-05-02_s2701` | 1000 + 1000 | 2701 | e7a60aa880f806a50cf5f7341650a077f93d3e28 | flags: max R-hat 1.0112 > 1.01; divergent 17 > 0 |
| 2016-10-29, 2016-05-02 | v2 | `v2_2016-10-29_2016-05-02_s2701` | 1000 + 1000 | 2701 | e7a60aa880f806a50cf5f7341650a077f93d3e28 | flags: max R-hat 1.0221 > 1.01 |
| 2016-10-29, 2016-09-12 | r0 | `r0_2016-10-29_2016-09-12_s2701` | 1000 + 1000 | 2701 | e7a60aa880f806a50cf5f7341650a077f93d3e28 | flags: max R-hat 1.0862 > 1.01; divergent 54 > 0 |
| 2016-10-29, 2016-09-12 | v2 | `v2_2016-10-29_2016-09-12_s2701` | 1000 + 1000 | 2701 | e7a60aa880f806a50cf5f7341650a077f93d3e28 | flags: max R-hat 1.0122 > 1.01; divergent 1 > 0 |
| 2016-10-29, 2016-10-15 | r0 | `r0_2016-10-29_2016-10-15_s2701` | 1000 + 1000 | 2701 | e7a60aa880f806a50cf5f7341650a077f93d3e28 | flags: max R-hat 1.0590 > 1.01; divergent 57 > 0 |
| 2016-10-29, 2016-10-15 | v2 | `v2_2016-10-29_2016-10-15_s2701` | 1000 + 1000 | 2701 | e7a60aa880f806a50cf5f7341650a077f93d3e28 | flags: max R-hat 1.0108 > 1.01 |
| 2016-10-29, 2016-10-28 | r0 | `r0_2016-10-29_2016-10-28_s2701` | 1000 + 1000 | 2701 | e7a60aa880f806a50cf5f7341650a077f93d3e28 | flags: max R-hat 1.0152 > 1.01; divergent 55 > 0 |
| 2016-10-29, 2016-10-28 | v2 | `v2_2016-10-29_2016-10-28_s2701` | 1000 + 1000 | 2701 | e7a60aa880f806a50cf5f7341650a077f93d3e28 | flags: max R-hat 1.0215 > 1.01; divergent 1 > 0 |
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
| 2016 f180 | -0.015 | 0.031 | 0.813 | 0.785 | -0.707 |
| 2016 f47 | -0.453 | 0.016 | 0.613 | 0.489 | -0.529 |
| 2016 f14 | 0.260 | 0.020 | 0.420 | 0.318 | -0.318 |
| 2016 eve | -0.137 | 0.013 | 0.410 | 0.532 | -0.524 |
| 2021 f180 | -0.157 | 0.021 | 0.200 | 0.125 | -0.139 |
| 2021 eve | -0.300 | 0.016 | 0.222 | 0.351 | -0.352 |
| 2024 eve | 0.002 | 0.009 | 0.060 | 0.034 | -0.025 |

| Election | Cells | Mean v2 − r0 | Outcome noise |
| --- | --- | --- | --- |
| 2016-10-29 | 4 | -0.086 | 0.564 |
| 2021-09-25 | 2 | -0.229 | 0.211 |
| 2024-11-30 | 1 | 0.002 | 0.060 |

Over the 3 elections: mean -0.104 pp, outcome noise 0.202 pp, ratio -0.52: indistinguishable (a mean difference under twice its noise).

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
| 2016 f180 | 2.93 (0.04) | 2.42 (0.03) | -0.51 | better | 3.58 | 2.63 | 3.01 | 2.88 | 17 (not comparable) | 19 (not comparable) |
| 2016 f47 | 3.31 (0.03) | 2.20 (0.03) | -1.11 | better | 3.56 | 2.30 | 3.30 | 2.54 | 22 (not comparable) | 14 (not comparable) |
| 2016 f14 | 2.13 (0.03) | 1.93 (0.02) | -0.20 | better | 2.01 | 2.36 | 2.53 | 2.63 | 15 (not comparable) | 13 (not comparable) |
| 2016 eve | 1.87 (0.02) | 1.61 (0.02) | -0.26 | better | 1.80 | 1.77 | 2.41 | 2.32 | 18 (not comparable) | 13 (not comparable) |
| 2021 f180 | 2.08 (0.05) | 2.11 (0.02) | 0.03 | tie | 2.00 | 1.71 | 2.66 | 2.39 | 12.5 | 13 |
| 2021 eve | 1.53 (0.01) | 1.28 (0.01) | -0.25 | better | 1.63 | 1.19 | 1.81 | 1.34 | 9 | 11 |
| 2024 eve | 1.18 (0.01) | 1.33 (0.01) | 0.15 | worse | 1.02 | 1.15 | 1.59 | 1.51 | 7 | 8 |

## 2016, f180 (E 2016-10-29, F 2016-05-02)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 2.433 (0.024) | 2.418 (0.019) | -0.015 | 0.031 | tie |
| mDSS (co-primary) | -1.28 (0.42) | 4.68 (0.68) | 5.96 | 0.80 | worse |
| d² (p: 0.132, 0.016) | 9.83 (0.46) | 15.60 (0.81) | 5.77 | 0.93 | worse |
| MAE8, pp (over the 6 named parties scored) | 2.927 (0.042) | 2.419 (0.026) | -0.508 | 0.049 | better |
| MAE over all parties, pp | 3.584 | 2.630 | -0.954 |  | reported |
| IS90 | 17.71 (0.56) | 23.78 (1.03) | 6.07 | 1.17 | worse |
| National 50% coverage (higher is better) | 3 of 7 (0.12) | 4 of 7 (0.00) | 1 | 0.12 | reported |
| National 90% coverage (higher is better) | 6 of 7 (0.11) | 5 of 7 (0.10) | -1 | 0.15 | reported |
| Kjördæmi CRPS, pp | 2.483 (0.023) | 2.386 (0.016) | -0.097 | 0.028 | better |
| Kjördæmi 90% coverage (higher is better) | 35 of 42 (0.26) | 35 of 42 (0.28) | 0 | 0.38 | reported |
| Kjördæmi MAE over the named cells, pp | 3.011 | 2.878 | -0.133 |  | reported |
| Seat CRPS per party (not comparable) | 1.871 (0.013) | 2.438 (0.022) | 0.567 | 0.025 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 17 | 19 | 2 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 6 of 7 (0.06) | 4 of 7 (0.11) | -2 | 0.13 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.025 (0.002) | 0.129 (0.003) | 0.103 | 0.004 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.025 | 0.021 | -0.004 |  | not comparable |

National parties: 7 (r0), 7 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.578 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.016 against χ² with 6 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.132).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.015 pp (SE_diff 0.031, tie), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = 5.96 (SE_diff 0.80, worse), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS -0.097 pp (better), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: yes.

On this cell Flokkur Fólksins is scored inside Annað, having had no admitted poll by F, so Annað's result includes its share.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 25.13 | 28.85 | 3.87 | 0.15 | 2.286 | 1.089 |
| Vinstri Græn | 15.91 | 15.40 | 15.99 | 0.51 | 0.08 | 1.045 | 1.028 |
| Píratar | 14.48 | 20.34 | 16.48 | 5.85 | 2.00 | 3.691 | 2.049 |
| Annað | 12.90 | 5.37 | 9.00 | 7.53 | 3.90 | 5.426 | 2.293 |
| Framsóknarflokkurinn | 11.49 | 13.69 | 11.62 | 2.20 | 0.13 | 1.459 | 1.058 |
| Viðreisn | 10.48 | 7.79 | 3.34 | 2.69 | 7.14 | 1.517 | 5.613 |
| Samfylkingin | 5.74 | 8.19 | 10.76 | 2.45 | 5.01 | 1.607 | 3.798 |

Of the 7 parties both arms forecast, v2's median is closer to the result on 5 and its CRPS lower on 5 (no MC error per party; consistency evidence only).

## 2016, f47 (E 2016-10-29, F 2016-09-12)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 2.333 (0.014) | 1.880 (0.008) | -0.453 | 0.016 | better |
| mDSS (co-primary) | 5.27 (0.49) | 6.16 (0.58) | 0.89 | 0.76 | tie |
| d² (p: 0.014, 0.020) | 17.50 (0.46) | 16.61 (0.63) | -0.89 | 0.78 | tie |
| MAE8, pp (over the 7 named parties scored) | 3.314 (0.033) | 2.203 (0.033) | -1.111 | 0.047 | better |
| MAE over all parties, pp | 3.561 | 2.299 | -1.262 |  | reported |
| IS90 | 14.37 (0.35) | 22.35 (0.15) | 7.98 | 0.38 | worse |
| National 50% coverage (higher is better) | 3 of 8 (0.15) | 5 of 8 (0.00) | 2 | 0.15 | reported |
| National 90% coverage (higher is better) | 7 of 8 (0.18) | 7 of 8 (0.09) | 0 | 0.20 | reported |
| Kjördæmi CRPS, pp | 2.346 (0.013) | 2.015 (0.007) | -0.331 | 0.015 | better |
| Kjördæmi 90% coverage (higher is better) | 42 of 48 (0.52) | 40 of 48 (0.24) | -2 | 0.57 | reported |
| Kjördæmi MAE over the named cells, pp | 3.298 | 2.544 | -0.754 |  | reported |
| Seat CRPS per party (not comparable) | 1.871 (0.012) | 1.493 (0.008) | -0.378 | 0.015 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 22 | 14 | -8 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 7 of 8 (0.11) | 7 of 8 (0.10) | 0 | 0.15 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.024 (0.002) | 0.036 (0.002) | 0.012 | 0.003 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.040 | 0.184 | 0.144 |  | not comparable |

National parties: 8 (r0), 8 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.610 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.020 against χ² with 7 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.014).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.453 pp (SE_diff 0.016, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = 0.89 (SE_diff 0.76, tie), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS -0.331 pp (better), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: yes.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 26.51 | 27.47 | 2.48 | 1.52 | 1.556 | 1.354 |
| Vinstri Græn | 15.91 | 14.83 | 14.39 | 1.08 | 1.52 | 0.891 | 1.127 |
| Píratar | 14.48 | 22.58 | 17.68 | 8.10 | 3.19 | 5.219 | 2.304 |
| Framsóknarflokkurinn | 11.49 | 12.93 | 11.07 | 1.43 | 0.43 | 1.051 | 0.957 |
| Viðreisn | 10.48 | 4.70 | 8.22 | 5.77 | 2.25 | 3.284 | 2.208 |
| Annað | 9.36 | 4.08 | 6.40 | 5.29 | 2.97 | 3.915 | 1.765 |
| Samfylkingin | 5.74 | 8.24 | 8.85 | 2.50 | 3.11 | 1.711 | 2.080 |
| Flokkur Fólksins | 3.54 | 1.70 | 0.14 | 1.84 | 3.40 | 1.036 | 3.243 |

Of the 8 parties both arms forecast, v2's median is closer to the result on 5 and its CRPS lower on 5 (no MC error per party; consistency evidence only).

## 2016, f14 (E 2016-10-29, F 2016-10-15)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.569 (0.017) | 1.829 (0.012) | 0.260 | 0.020 | worse |
| mDSS (co-primary) | -7.48 (0.34) | -4.10 (0.32) | 3.38 | 0.47 | worse |
| d² (p: 0.337, 0.156) | 7.95 (0.38) | 10.61 (0.33) | 2.67 | 0.51 | worse |
| MAE8, pp (over the 7 named parties scored) | 2.128 (0.027) | 1.933 (0.017) | -0.195 | 0.032 | better |
| MAE over all parties, pp | 2.007 | 2.364 | 0.357 |  | reported |
| IS90 | 16.34 (0.38) | 14.49 (0.34) | -1.86 | 0.51 | better |
| National 50% coverage (higher is better) | 6 of 8 (0.00) | 4 of 8 (0.19) | -2 | 0.19 | reported |
| National 90% coverage (higher is better) | 7 of 8 (0.06) | 6 of 8 (0.10) | -1 | 0.12 | reported |
| Kjördæmi CRPS, pp | 1.855 (0.014) | 2.126 (0.011) | 0.271 | 0.017 | worse |
| Kjördæmi 90% coverage (higher is better) | 41 of 48 (0.26) | 38 of 48 (0.31) | -3 | 0.41 | reported |
| Kjördæmi MAE over the named cells, pp | 2.526 | 2.627 | 0.101 |  | reported |
| Seat CRPS per party (not comparable) | 1.672 (0.011) | 1.412 (0.008) | -0.260 | 0.014 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 15 | 13 | -2 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 7 of 8 (0.18) | 6 of 8 (0.13) | -1 | 0.22 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.000 (0.000) | 0.001 (0.000) | 0.001 | 0.000 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.088 | 0.112 | 0.024 |  | not comparable |

National parties: 8 (r0), 8 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.548 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.156 against χ² with 7 degrees of freedom, above 0.01: yes, reported only: the gate is an eve criterion (r0's p = 0.337).
2. No loss of accuracy: CRPS10 v2 − r0 = 0.260 pp (SE_diff 0.020, worse), at most 0.05 pp worse or a tie: no.
3. Better distribution: mDSS v2 − r0 = 3.38 (SE_diff 0.47, worse), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS 0.271 pp (worse), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: no.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.
(Criterion 1 left out: the cell is not an eve.)

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 22.42 | 22.80 | 6.58 | 6.20 | 4.955 | 4.511 |
| Vinstri Græn | 15.91 | 14.89 | 14.30 | 1.02 | 1.60 | 0.745 | 0.988 |
| Píratar | 14.48 | 16.00 | 15.00 | 1.51 | 0.51 | 1.225 | 1.117 |
| Framsóknarflokkurinn | 11.49 | 10.61 | 9.82 | 0.88 | 1.67 | 0.653 | 1.004 |
| Viðreisn | 10.48 | 8.53 | 10.32 | 1.95 | 0.15 | 1.736 | 1.290 |
| Annað | 9.36 | 10.52 | 14.74 | 1.16 | 5.38 | 1.002 | 3.682 |
| Samfylkingin | 5.74 | 7.33 | 7.43 | 1.59 | 1.68 | 1.028 | 1.077 |
| Flokkur Fólksins | 3.54 | 4.90 | 1.83 | 1.36 | 1.71 | 1.207 | 0.962 |

Of the 8 parties both arms forecast, v2's median is closer to the result on 3 and its CRPS lower on 4 (no MC error per party; consistency evidence only).

## 2016, eve (E 2016-10-29, F 2016-10-28)

| Score (lower is better unless marked) | r0 (SE) | v2 (SE) | v2 − r0 | SE_diff | Reading (Monte Carlo error only) |
| --- | --- | --- | --- | --- | --- |
| CRPS10, pp (co-primary) | 1.453 (0.012) | 1.317 (0.006) | -0.137 | 0.013 | better |
| mDSS (co-primary) | -9.25 (0.29) | -9.77 (0.37) | -0.52 | 0.47 | tie |
| d² (p: 0.123, 0.139) | 11.37 (0.31) | 11.00 (0.40) | -0.37 | 0.51 | tie |
| MAE8, pp (over the 7 named parties scored) | 1.867 (0.021) | 1.611 (0.018) | -0.256 | 0.028 | better |
| MAE over all parties, pp | 1.796 | 1.772 | -0.025 |  | reported |
| IS90 | 16.52 (0.29) | 12.04 (0.15) | -4.48 | 0.32 | better |
| National 50% coverage (higher is better) | 5 of 8 (0.24) | 3 of 8 (0.12) | -2 | 0.27 | reported |
| National 90% coverage (higher is better) | 7 of 8 (0.00) | 7 of 8 (0.10) | 0 | 0.10 | reported |
| Kjördæmi CRPS, pp | 1.791 (0.010) | 1.727 (0.006) | -0.064 | 0.012 | better |
| Kjördæmi 90% coverage (higher is better) | 37 of 48 (0.27) | 39 of 48 (0.32) | 2 | 0.42 | reported |
| Kjördæmi MAE over the named cells, pp | 2.414 | 2.321 | -0.093 |  | reported |
| Seat CRPS per party (not comparable) | 1.712 (0.009) | 1.404 (0.004) | -0.308 | 0.010 | not comparable |
| Seat absolute error, summed over parties (not comparable) | 18 | 13 | -5 |  | not comparable |
| Seat 90% coverage (higher is better) (not comparable) | 6 of 8 (0.00) | 7 of 8 (0.18) | 1 | 0.18 | not comparable |
| Brier score of the incumbents' majority (not comparable) | 0.000 (0.000) | 0.000 (0.000) | 0.000 | 0.000 | not comparable |
| P(majority for the coalition that formed), report only (higher is better) (not comparable) | 0.101 | 0.025 | -0.076 |  | not comparable |

National parties: 8 (r0), 8 (v2). The gap between each run's two national readings (the kjördæmi aggregate scored here and the model's own) is at most 0.572 pp (r0) and 0.000 pp (v2).

1. Calibration gate: v2's d² has p = 0.139 against χ² with 7 degrees of freedom, above 0.01: yes (r0's p = 0.123).
2. No loss of accuracy: CRPS10 v2 − r0 = -0.137 pp (SE_diff 0.013, better), at most 0.05 pp worse or a tie: yes.
3. Better distribution: mDSS v2 − r0 = -0.52 (SE_diff 0.47, tie), lower beyond a tie or r0 failing the gate: no.
4. No regional loss: kjördæmi CRPS -0.064 pp (better), at most 0.1 worse or a tie; seat CRPS per party left out, not comparable: yes.

Seat scores not comparable on this cell: Annað holds 4 official seats at E, which no draw gives it, since every draw allocates the seats with Annað ineligible. They are reported, marked, and left out of criterion 4.

All four on this cell: no.

| Party | Result, % | r0: median, % | v2: median, % | r0: absolute error, pp | v2: absolute error, pp | r0: CRPS, pp | v2: CRPS, pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Sjálfstæðisflokkurinn | 29.00 | 22.87 | 23.41 | 6.12 | 5.59 | 4.924 | 4.375 |
| Vinstri Græn | 15.91 | 16.68 | 16.26 | 0.77 | 0.35 | 0.548 | 0.493 |
| Píratar | 14.48 | 16.49 | 16.34 | 2.00 | 1.85 | 1.201 | 1.167 |
| Framsóknarflokkurinn | 11.49 | 10.56 | 10.60 | 0.93 | 0.90 | 0.568 | 0.579 |
| Viðreisn | 10.48 | 9.80 | 11.03 | 0.68 | 0.55 | 1.733 | 0.828 |
| Annað | 9.36 | 8.06 | 12.26 | 1.30 | 2.89 | 0.790 | 1.903 |
| Samfylkingin | 5.74 | 6.37 | 6.44 | 0.63 | 0.70 | 0.376 | 0.416 |
| Flokkur Fólksins | 3.54 | 5.48 | 2.20 | 1.94 | 1.34 | 1.488 | 0.774 |

Of the 8 parties both arms forecast, v2's median is closer to the result on 6 and its CRPS lower on 5 (no MC error per party; consistency evidence only).

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

