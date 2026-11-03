# US midterms: feasibility experiment

Can this repo's model forecast the US midterm elections? This folder holds the experiment
that answers it (started 2 November 2026, the day before the 2026 midterms). Nothing here
is used by the Icelandic forecast or Fylgisvakt, and nothing outside `usa/` was changed.

## Files

| File | What it is |
|------|------------|
| `stan/us_midterm_v0.stan` | Prototype: `Stan/polling_watch_v5_epoch.stan` (anchored walk, pollster bias that random-walks across the epochs between elections) unchanged, plus an election-day step with a fundamentals factor, a race layer (each Senate state and House district a deviation from the national walk, with a regression prior learnt from past results) and seat counts. The header explains each addition |
| `stan/us_midterm_v1.stan` | v0 with tighter priors centred on US-plausible values, to get a first working fit (its header lists each change); expand from here once it fits |
| `stan/us_midterm_v2.stan` | v1 with the walk's step covariance exactly identified: P - 1 scales and a (P - 1) x (P - 1) correlation in the walk's own coordinates, where v1 inherits v5's P scales and P x P correlation centred onto a P - 1 dimensional subspace |
| `stan/us_midterm_v3.stan` | v2 with the walk integrated out by a Kalman filter: each poll's log-ratios Gaussian (delta-method multinomial covariance times the house's Dirichlet-multinomial factor), the walk drawn afterwards by forward filtering, backward sampling. Written for three parties |
| `stan/us_midterm_v4.stan` | v3 with each house's log phi partially centred by its number of polls, as the forecast model centres its constituency effects; v2's centred log phi is a funnel with ~160 mostly sparse pollsters |
| `stan/us_midterm_v5.stan` | v4 with per-epoch offsets of registered-voter and adult polls against likely voters. It failed the test set before its backtest (mean absolute error 2.82 against v4's 2.46, 5 of 8 90% intervals covered against 6), so its 2026 forecast is not recorded |
| `stan/us_midterm_v6.stan` | v5 with two switches in the data: `bias_walk` (1 = v5's industry bias that random-walks across epochs, 0 = stationary around a mean) and `phi_info` (the information per poll in log phi's centring weight; 0 = non-centred, which sparse final-weeks polls need). The header has the case for each |
| `stan/us_midterm_v8.stan` | v6 for two parties (P = 2): a scalar branch of the Kalman filter, every bias and race deviation a zero-sum pair. On P = 3 data it is v6 exactly. Other is dropped because races cannot share one isotropic scale across D–R (residual SD 0.20) and the major parties against Other (0.86); v7, which gave Other its own scale, went with it |
| `stan/us_midterm_v9.stan` | v8 with a polling error shared by each race's polls (`sigma_race_poll`, data; 0 = v8), integrated out: the model samples what the polls see, and generated quantities draws election day's race deviation from its conditional |
| `stan/us_midterm_v10.stan` | v9 with `bias_walk = 2`: the industry bias AR(1) around its long-run mean, so a cycle's bias can lean towards the last one's (coefficient estimated, prior centred on v9's stationary bias). With `bias_walk` 0 or 1 it is v9 |
| `stan/us_midterm_v11.stan` | v10 with two data switches on the industry bias, for the review's ablation: `fix_mu_bar` = 1 centres every epoch's bias on zero instead of a learnt long-run mean, and `bias_tail_nu` > 2 makes each epoch's shock multivariate t with that df and the same covariance. With both 0 it is v10: the same draws, bit for bit, on the 1998 history data |
| `stan/us_midterm_v12.stan` | v11 with extra walk variance on both sides of each election (`election_vol`, `vol_scale_days` L): each step's variance gains a_pre and a_post times the sums of exp(−d/L) over its days, d the days to the next election or since the last, so the walk, not the industry bias, can take a move near an election. With `election_vol` = 0 it is v11, bit for bit on the 1998 history data |
| `R/simulate_us.R` | Simulates US-scale data in that model's format; the national fields also run `polling_watch_v5_epoch.stan` unchanged. Options in its header |
| `R/build_generic_ballot.R` | Real data for `polling_watch_v5_epoch.stan`, and for v1–v9: the generic congressional ballot polls of the 2018–2026 cycles, anchored at the national House popular vote of 2018–2024. `target=` builds an out-of-sample backtest of an earlier election, `races=senate` adds that year's Senate races, `two_party=1` drops Other; the header lists every option |
| `R/senate_races.R` | The Senate race layer `build_generic_ballot.R races=senate` sources: each race's D and R slots, covariates (presidential lean, incumbency, an independent in the D slot), its polls, the past results the race regression learns from, the seats not up, the race polling error `race_poll_sd=auto` reads, and `race_type_error_sd()`, the error all of a cycle's race polls share beyond the national polls' (s_eta). Its header says which candidate fills the D slot and which races are left out |
| `R/build_generic_history.R` | Generic-ballot data for 1998–2022 from FiveThirtyEight's pollster-ratings `raw_polls.csv`: each election's polls of its final 60 days, anchored at the House popular vote since 1996, for backtests over 13 elections |
| `R/check_recovery.R` | Compares a fit on simulated data with the values that generated it: race coefficients, election-day vote, seat counts, each with its 90% interval |
| `R/summarise_generic.R` | Election-day forecast, industry bias per epoch and the walk's margin by date, from a generic-ballot fit of v3 or v4 |
| `R/score_backtest.R` | Scores backtest fits against the popular vote of their target election, beside two poll baselines |
| `R/score_races.R` | Scores the races of Senate backtest fits against their results: margin error, 90% coverage, Brier score, winners called, expected against actual Democratic seats, beside the mean of each race's polls of the final 28 days |
| `R/score_year_pit.R` | Year-level calibration of Senate or presidential backtest fits: where the national margin, the mean race margin, their difference and the D seats or electoral votes fall in each fit's forecast distribution (mid-PIT), since a year's races share their errors |
| `R/build_president.R` | v9 data for a presidential election of 2004–2024: the national and state polls of each cycle's final 60 days, anchored at the national popular vote since 1996, each state and DC a race with a regression prior on its leans at the two elections before |
| `R/score_president.R` | Scores presidential backtest fits: national margin, each state's margin, 90% coverage, Brier score and winner, and the electoral votes (each state's to its statewide winner), beside the mean of each state's polls of the final 28 days |
| `us-midterms-feasibility.html` | The write-up: data, model changes, run times, out-of-sample backtests of 2018–2024 and the 2026 national forecast |
| `us-model-review.html` | A critique of v9/v10 (2 November 2026) against 538, Silver Bulletin, the Economist and the polling-error and midterm literature: seven themes ranked, with measured evidence (the forecast cycle's industry bias against the realised poll miss; the Senate race regression's residual SD), fixes, tests and a suggested order of work |
| `us-midterms-improvement-plan.html` | Next steps (2 November 2026): the national-layer fixes, the race-layer data and model, the decisions they wait on, and the data held and missing |
| `us-house-layer-plan.html` | Plan for a House district layer (3 November 2026): the data held (district results 2006–2024, CC0) and missing (district lean on each map), the model sketch, six steps with checks, and the decisions they wait on |
| `house_maps.csv` | The congressional map in force in each state for each cycle 2016–2026 (`state`, `cycle`, `map`, `changed`, `how`), for the House layer: the 2016, 2018 and 2020 court and legislature redraws, the 2022 maps, the five 2024 redraws and the nine mid-decade 2026 ones (Missouri's and Virginia's were blocked) |
| `R/house_races.R` | House district history 2006–2024 from the CKS file (CC0): each district's leading Democrat against its leading Republican, runoffs replacing first rounds, contested flag, margin, winner, incumbency, freshmen, and the previous margin where the map did not change (`LC_ALL=en_US.UTF-8 Rscript usa/R/house_races.R raw=<raw> tables=<tables> out=<csv>`) |
| `R/house_lean.R` | District lean for the House layer: MEDSL precinct presidential votes summed onto each map's districts through the precincts' US House rows, matched by name across a map change and otherwise shared out within the county by each party's House votes; Maine and Nebraska from their official district results (`LC_ALL=en_US.UTF-8 Rscript usa/R/house_lean.R csv=<unzipped precinct CSVs> raw=<raw> out=<dir>`) |
| `R/house_lean_2026.R` | District lean on the 2026 maps: the own-map 2024 lean where the map did not change, the redistricting articles' 2024 results on the new Texas, Florida and Tennessee lines, and for North Carolina, Ohio, Utah, California, Louisiana and Alabama the 2026 Cook PVI listed on Wikipedia mapped to a 2024 margin by a line fitted on the unchanged states (residual SD 2.4; misses the three new-map tables by 1.3 points on average) |
| `R/house_layer.R` | The House race layer `build_generic_ballot.R races=house house=<dir>` sources: each contested district (leading Democrat against leading Republican), covariates (presidential lean on the map in force, incumbency), its polls (2026: Wikipedia; 2018–2024: 538), the 2018–2024 district results the race regression learns from, and `unopposed_d`, the seats only a Democrat contests |
| `data_freeze_2026-11-02.sha256` | sha256 of every poll table and raw file gathered on 2 November 2026, committed before the election, so a later 2026 fit can show it used pre-election data only |
| `data_freeze_2026-11-02_open.sha256` | The same for the open-licence files gathered later that day (paths under the study folder's `raw/`; `shasum -a 256 -c` from that folder) |
| `data_freeze_2026-11-03_medsl_precinct.sha256` | sha256 of the 107 MEDSL precinct-return zips downloaded on 3 November 2026 for the House layer's district lean (paths under the study folder's `raw/medsl-precinct/`) |
| `data_freeze_2026-11-03_wiki_2026_maps.sha256` | sha256 of the Wikipedia pages (fetched through metill-platform's polite client and the Action API, 3 November 2026) and the tables drawn from them for the 2026 district leans |
| `R/bench_fit.R` | Fits a Stan file on a saved data JSON, times it and prints one `DIAG` line (wall time, ms per gradient, tree depth, divergences, R-hat, ESS) |

The poll tables these scripts read were gathered on 2 November 2026 from Wikipedia, the
Wayback Machine's copy of FiveThirtyEight's poll CSVs, VoteHub and the Clerk of the House.
They live outside the repo in
`/Users/brynjolfurjonsson/.claude-reviews/usa-midterms-feasibility-2026-11-02/tables/`,
since this repo is public and the tables are third-party data.

The race layer and the history backtests read open-licence files only (the plan's
decision D4), under that folder's `raw/`:

| Data | Source | Licence |
|---|---|---|
| House, Senate and governor results 2006–2024, with incumbency | Cha, Kuriwaki and Snyder, Candidates in American General Elections, Harvard Dataverse doi:10.7910/DVN/DGDRDT (`dataverse-DGDRDT/`) | CC0 |
| Precinct returns 2016–2024: president and US House by precinct, with each precinct's House district | MIT Election Data and Science Lab: 2016 from github.com/MEDSL/official-precinct-returns (MIT licence); 2018 House and 2020 president and House from the last commits of github.com/MEDSL/2018-elections-official (738eaba) and 2020-elections-official (ad22f37) before the move to Dataverse, where the same data are CC0; 2022 and 2024 per-state files from github.com/MEDSL/2022- and 2024-elections-official (`medsl-precinct/`) | CC0 (MIT for 2016) |
| State presidential vote 1976–2024 | MIT Election Data and Science Lab, doi:10.7910/DVN/42MVDX (`medsl/president.csv`) | CC0 |
| Senate, House and governor polls of the 2018–2024 cycles | FiveThirtyEight `polls` repository, `*_polls_historical.csv` (`538-polls-historical/`) | CC BY 4.0 |
| Final-60-day polls with results, 1998–2022 | FiveThirtyEight pollster ratings, `raw_polls.csv` (`gb-538-github/`) | CC BY 4.0 |
| Sitting senators and their classes | unitedstates/congress-legislators (`congress-legislators/`) | CC0 |
| 2026 Senate races and polls | Wikipedia (`tables/senate_*`) | CC BY-SA 4.0 |
| 2024 presidential polls | FiveThirtyEight `president_polls.csv`, Wayback copy of 5 November 2024 (`president/538-president-polls-20241105/`) | CC BY 4.0 |
| Electoral votes by state, 2008–2024 | National Archives (`president/archives-gov-ev/`, `president/derived/ev_by_state.csv`) | Public domain |

MEDSL flags Biden's and Trump's 2020 votes in DC as write-ins, so `presidential_votes()` in
`R/senate_races.R` sums the major-party rows without filtering on that flag.

No open source has full-cycle generic-ballot polls before the 2018 cycle (HuffPost
Pollster's 2012–2016 archive is CC BY-NC-SA).

## Running

```bash
LC_ALL=en_US.UTF-8 Rscript usa/R/simulate_us.R out=<dir> anchor_n=2e5
LC_ALL=en_US.UTF-8 Rscript usa/R/bench_fit.R stan=usa/stan/us_midterm_v0.stan data=<dir>/stan_data.json out=<fit dir>
```

`build_generic_ballot.R top_houses=K` keeps the K pollsters with the most polls, and
`simulate_us.R n_senate= n_house= houses= state_houses=` sets the sizes, for a small first fit.
`bench_fit.R init_buffer= window= term_buffer=` set CmdStan's warmup windows: from `init=smart`,
`init_buffer=15` reaches the first metric estimate after 15 iterations rather than 75.

Both data scripts take `grid=week`, which moves each poll to the Tuesday on or before its
date. Election days are Tuesdays, so the results stay where they are, and the walk has a
fifth as many points. `anchor_n=` sets each national result's total votes: the real
~10^8 pins the walk at each election with an SD ~25 times smaller than Iceland's ~2×10^5 does.
`bench_fit.R init=smart` starts every chain near plausible US values (walk volatility
0.004 per day, log phi 7, small biases) rather than at 0, where the walk's volatility is 1
per day against a posterior near 0.003; `init=pathfinder` starts from Pathfinder draws,
which on the full simulated data took about 600,000 log-density evaluations per path.

`bench_fit.R` compiles without CmdStan's precompiled header, since that header goes stale
whenever Apple clang updates.

### Backtests

`build_generic_ballot.R target=2022-11-08` keeps the polls that end before that election
day and the results of the elections before it, and sets the forecast step to run to the
target; `asof=` moves the poll cutoff earlier (the forecast still targets election day).
A 2018 target has no earlier result in the window, and v4 needs one: `anchor_2016=1` adds
the 2016 result on the window's first date.
Build with the options of the 2026 fit, fit, then score every fit in the folder:

```bash
LC_ALL=en_US.UTF-8 Rscript usa/R/build_generic_ballot.R tables=<tables> out=<bt>/data_eve_2022 anchor_n=2e5 grid=week tau_scale=0.05 target=2022-11-08
LC_ALL=en_US.UTF-8 Rscript usa/R/bench_fit.R stan=usa/stan/us_midterm_v4.stan data=<bt>/data_eve_2022/stan_data.json out=<bt>/fit_eve_2022 init=smart init_buffer=15 window=25
LC_ALL=en_US.UTF-8 Rscript usa/R/score_backtest.R dir=<bt> tables=<tables>
```

### History backtests, 1998–2022

`build_generic_history.R` gives the generic-ballot model 13 more elections, each with the
polls of its final 60 days only. On those, v6 with a stationary industry bias
(`bias_walk=0`) beat v5's walk (`bias_walk=1`), both P = 3 with `phi_info=0`:

| Bias across epochs | Mean absolute error | 90% intervals covering | CRPS |
|---|---|---|---|
| Random walk (v5) | 2.34 | 11 of 13 | 1.85 |
| Stationary (v6) | 2.11 | 11 of 13 | 1.60 |
| Mean of the final-60-day polls | 3.14 | | |

Errors are in points of two-party margin. Both miss 2008 and 2018 (the polls understated
the Democrats). Two-party (v8, `two_party=1`), the same 13 elections give the same answer:
mean absolute error 2.46 for the walk against 2.09 stationary, CRPS 1.93 against 1.64, 10 of
13 intervals covering for each; the stationary fits of the smallest years (1998–2004) had
6–15 divergent transitions of 4,000. v10's AR(1) bias (`bias_walk=2`), which lets each
cycle's bias lean towards the last one's, falls between them: mean absolute error 2.18, CRPS
1.68, 10 of 13 covering, with up to 17 divergent transitions in the small fits. Its
coefficient stays close to its prior (median +0.04 to +0.42 across the 12 fits that have
one; every 90% interval covers −0.25 to +0.67), and carrying 2018's accurate polls forward made
its 2020 forecast worse. The Senate and presidential fits use the stationary bias.

```bash
LC_ALL=en_US.UTF-8 Rscript usa/R/build_generic_history.R raw=<raw>/gb-538-github/raw_polls.csv tables=<tables> out=<bt>/data_eve_2010 target=2010-11-02 anchor_n=2e5 grid=week tau_scale=0.05 bias_walk=0
```

### Senate backtests

`build_generic_ballot.R races=senate` adds the target year's Senate races. Each race is a
deviation from the national House vote, with a regression prior learnt from the Senate
results before it. Two-party (`two_party=1`, v8 and v9), with each race's shared polling
error set from the cycles before the target (`race_poll_sd=auto`, 0.064 to 0.068 per
direction):

```bash
LC_ALL=en_US.UTF-8 Rscript usa/R/build_generic_ballot.R tables=<tables> raw=<raw> races=senate out=<bt>/data_eve_2022 anchor_n=2e5 grid=week tau_scale=0.05 population=off bias_walk=0 phi_info=0.125 two_party=1 race_poll_sd=auto target=2022-11-08
LC_ALL=en_US.UTF-8 Rscript usa/R/bench_fit.R stan=usa/stan/us_midterm_v9.stan data=<bt>/data_eve_2022/stan_data.json out=<bt>/fit_eve_2022 init=smart init_buffer=15 window=25
LC_ALL=en_US.UTF-8 Rscript usa/R/score_races.R dir=<bt>
```

The D slot holds the Democrat, or a Democratic-aligned independent who ran ahead of every
Democrat; a race that is not a contest of the two slots is left out (`senate_races.R`,
header). That rule, applied on 2 November 2026, dropped seven races the first fits had
miscoded, among them Indiana 2006 (a Libertarian in the D slot), Arkansas 2020 (the same) and
Alaska 2022 (Murkowski against Tshibaka): 336 history races where there were 343, and 34
races in 2020 and in 2022. The race residual SD fell from 14.0 to 12.1 margin points.

On the eve of each election (`eve`) and four weeks before it (`w4`, `asof=`), v9's error on
the races with polls was below that of the mean of each race's polls of the final 28 days in
all eight fits (two-party margin points; fits in the study folder's `senate_dslot/`):

| Fit | Error, polled races | Polls' error | Winners right | 90% intervals covering | Expected / actual D seats |
|---|---|---|---|---|---|
| eve 2018 | 3.52 | 3.63 | 32 of 34 | 31 | 21.8 / 23 |
| w4 2018 | 4.01 | 5.98 | 32 of 34 | 32 | 23.2 / 23 |
| eve 2020 | 6.79 | 7.34 | 30 of 34 | 23 | 16.1 / 15 |
| w4 2020 | 6.64 | 7.63 | 30 of 34 | 28 | 16.5 / 15 |
| eve 2022 | 4.01 | 4.76 | 34 of 34 | 30 | 14.4 / 15 |
| w4 2022 | 5.55 | 5.84 | 34 of 34 | 28 | 16.0 / 15 |
| eve 2024 | 2.80 | 4.88 | 33 of 34 | 31 | 18.1 / 19 |
| w4 2024 | 2.95 | 5.22 | 32 of 34 | 32 | 19.3 / 19 |

On the races both codings share, the fix moved the errors by less than 0.1 point and the
coverage by one race either way. The exception is Maine, where King had no polls in the final
28 days. The first coding had flagged the dropped races as Democratic-aligned independents,
and their large negative residuals held the independent coefficient down; without them it
roughly doubled (0.066 to 0.108 per direction in the 2026 fit), and King's forecast moved 6–8
points toward his result. The intervals narrowed by 0.5–1 point.

2020, when the polls overstated the Democrats almost everywhere, is the one cycle whose
intervals cover too rarely: each race's polling error is independent of the others', so the
model has no error shared by a region or by all races beyond the national vote's. That shared
error is real. `race_type_error_sd()` puts the SD of a cycle's mean Senate poll miss, beyond
the same cycle's generic-ballot miss, at 3.1 margin points over 1998–2022 and 2.0 since 2008.
State presidential polls against national ones give 0.5.

`score_year_pit.R` scores each fit as one forecast of its year:

```bash
LC_ALL=en_US.UTF-8 Rscript usa/R/score_year_pit.R dir=<bt> house=<tables>/house_popular_vote_1946_2024.csv
LC_ALL=en_US.UTF-8 Rscript usa/R/score_year_pit.R dir=<presidential bt> ev=<raw>/president/derived/ev_by_state.csv
```

The Senate seat counts are calibrated (PITs 0.24–0.72), but the national House margin is
not: eve PITs of 0.97, 0.001, 0.19 and 0.94 for 2018–2024, with 2020's mean race margin also
at 0.001. That is the overconfident industry bias of `us-model-review.html`, theme 1. The
mean race margin minus the national one sits at 0.025 (eve) and 0.010 (w4) in 2018 and in
range since.

### Presidential backtests, 2008–2024

`build_president.R` makes each state and DC a race on the national popular vote, so the state
calls can be set beside the published forecasts' (their final state forecasts, gathered with
their sources in the study folder's `raw/benchmarks/`). Margins are two-party; 538's and Silver
Bulletin's electoral votes include the Maine and Nebraska districts, v9's give each state's
votes to its statewide winner. 538 is Silver's model to 2020 (polls-only in 2016) and Morris's
in 2024.

| Year | Forecast | States called | Brier | State error | P(D wins) | D electoral votes (actual) |
|---|---|---|---|---|---|---|
| 2008 | v9 | 49 (IN, MO missed) | 0.026 | 3.46 | 0.96 | 367 (365) |
| | 538 | 50 (IN) | | | 0.99 | 349 mean |
| 2012 | v9 | 51 | 0.013 | 3.48 | 0.85 | 318 (332) |
| | 538 | 51 | 0.009 | 3.27 | 0.91 | 313 mean |
| 2016 | v9 | 45 (FL, MI, NC, OH, PA, WI) | 0.082 | 5.76 | 0.91 | 323 (232) |
| | 538 | 46 (FL, MI, NC, PA, WI) | 0.067 | 6.08 | 0.71 | 302 mean |
| 2020 | v9 | 49 (FL, NC) | 0.039 | 5.46 | 0.97 | 356 (306) |
| | 538 | 49 (FL, NC) | 0.036 | 4.69 | 0.89 | 348 mean |
| | Economist | 49 (FL, NC) | 0.037 | 4.44 | 0.97 | 356 |
| 2024 | v9 | 49 (MI, WI) | 0.026 | 2.66 | 0.41 | 258 (226) |
| | 538 | 47 (MI, NV, PA, WI) | 0.034 | 2.85 | 0.50 | 270 |
| | Silver Bulletin | 49 (MI, WI) | 0.034 | | 0.50 | 271 mean |
| | Economist | 47 (MI, NV, PA, WI) | 0.037 | 3.39 | 0.56 | 276 |

v9's electoral votes are posterior medians. Its 90% intervals covered 47, 48, 32, 32 and 50
of the 51 states: like the Senate, it has no error shared by a region, which 2016 and 2020
needed.

```bash
LC_ALL=en_US.UTF-8 Rscript usa/R/build_president.R raw=<raw> out=<bt>/data_eve_2016 target=2016-11-08
LC_ALL=en_US.UTF-8 Rscript usa/R/bench_fit.R stan=usa/stan/us_midterm_v9.stan data=<bt>/data_eve_2016/stan_data.json out=<bt>/fit_eve_2016 init=smart init_buffer=15 window=25
LC_ALL=en_US.UTF-8 Rscript usa/R/score_president.R dir=<bt> ev=<raw>/president/derived/ev_by_state.csv
```

Year-level PITs (`score_year_pit.R`) put the electoral votes at 0.03 in 2016 and in range in
the other four years. The unweighted mean state margin minus the national margin sits at 0.014
in 2012 and 0.000 in 2016, against a 90% interval only about ±0.9 points wide: the forecasts
are too moderate in lopsided states (the state error falls by 0.8–1.4 points per 10 points of
forecast margin in 2008–2016, 0.1 in 2024), which moves that mean a lot and the electoral
votes little.

### Ablating the long-run industry bias

Step 2 of `us-model-review.html` fixed the long-run industry bias `mu_bar` at zero
(`us_midterm_v11.stan`, `fix_mu_bar=1`), with Gaussian shocks or multivariate t5 ones
(`bias_tail_nu=5`), on the baselines' own data: the two-party history fits (stationary bias),
the presidential fits and the Senate fits above. Each arm's data are the baseline's
`stan_data.json` with the two switches added (the builders' `mu_bar=zero bias_tail=5`); fits
in the study folder's `ablation/`.

| Backtests | Measure | Learnt `mu_bar` | `mu_bar` = 0 | `mu_bar` = 0, t5 |
|---|---|---|---|---|
| History 1998–2022 (13) | CRPS | 1.64 | 1.81 | 1.82 |
| | Mean absolute error | 2.09 | 2.47 | 2.52 |
| | 90% intervals covering | 10 | 12 | 12 |
| Presidential national 2008–2024 (5) | CRPS | 1.80 | 1.74 | 1.75 |
| | Mean absolute error | 2.72 | 2.55 | 2.55 |
| | Error 2016 / 2020 | 2.6 / 4.7 | 1.7 / 4.3 | 1.6 / 4.3 |
| Senate 2018–2024 (8 fits) | Error, polled races | 4.53 | 4.58 | 4.59 |
| | 90% intervals covering, of 34 | 29.4 | 29.3 | 29.8 |
| | Brier | 0.045 | 0.045 | 0.045 |

On the history the paired per-cycle CRPS difference is +0.16 (SE 0.38) for `mu_bar` = 0,
better in 3 of 13 cycles; the wider intervals (12 points against 9.3) buy the two extra
covers. The review's test called the ablation refuted if the history CRPS rose by more than
0.1 without cutting the 2016 and 2020 presidential national errors; it rose by 0.16 and cut
them by 0.9 and 0.4 points, so it is not refuted, but neither difference is clear of noise
over 13 and 5 cycles. The learnt mean stays the default: the generic-ballot polls' lean
towards the Democrats over 1998–2022 is a real mean that the history fits pay for losing,
the presidential national polls have little of one, and the Senate fits' windows (2016 on)
learn too little of it for the switch to matter. The t5 shocks changed nothing measurable,
with either mean: with `mu_bar` learnt they moved the history CRPS by +0.01 (SE 0.01), the
presidential national CRPS from 1.80 to 1.79 and the Senate polled-race error from 4.53 to 4.53.

Neither arm touches the cycle the review flagged: the 2020 eve national PIT stays at 0.001 in
every arm, and the 2026 forecast cycle's bias barely widens (D+2.0, 90% R+0.3 to D+4.3,
with `mu_bar` learnt; D+1.2, R+1.4 to D+4.0, with zero). That bias is read off the polls
after the last result, which is what the election-window volatility of the review's step 3
targets. With `mu_bar` = 0 the 2026 Senate forecast would be P(Democrats 51 or more) 0.81
(0.83 with t5), median 53 seats, House margin D+8.6.

Two arm fits needed a second look. The presidential 2008 fit with `mu_bar` = 0 had one chain
divergent on every draw (R-hat 1.54) and was refitted with `seed=4101` (no divergences; that
fit is the one scored). The history 2018 fit with `mu_bar` = 0 had 98 divergent transitions
(R-hat 1.016); a clean refit with `seed=4101` agreed within 0.13 points of margin.

### Election-window volatility

Step 3 of `us-model-review.html` lets the national walk move more on both sides of each
election, so that support can shift without the industry bias absorbing the shift
(`us_midterm_v12.stan`, the builders' `election_vol=1 vol_days=<L>`). Each step's variance
becomes v + a_pre·K_pre + a_post·K_post, in units of the walk's daily variance: K_pre sums
e^(−d/L) over the step's days with d the days to the next election, K_post the same with d the
days since the last result, and a_pre, a_post ~ half-normal(0, 2). The bias is constant
within an epoch, so poll movement inside the windows identifies both; the history and
presidential data hold only each cycle's final 60 days, so they see a_pre alone. The arms
reuse the baselines' data with the switch added; fits in the study folder's `volatility/`.

| Backtests | Measure | No windows | Windows, L = 30 | Windows, L = 60 |
|---|---|---|---|---|
| History 1998–2022 (13) | CRPS | 1.64 | 1.62 | – |
| | Mean absolute error | 2.09 | 2.05 | – |
| | 90% intervals covering | 10 | 10 | – |
| Presidential national 2008–2024 (5) | CRPS | 1.80 | 1.75 | – |
| | Mean absolute error | 2.72 | 2.67 | – |
| Senate 2018–2024 (8 fits) | Error, polled races | 4.53 | 4.52 | 4.53 |
| | 90% intervals covering, of 34 | 29.4 | 30.1 | 30.0 |
| | Brier | 0.045 | 0.045 | 0.046 |
| | National margin, 90% intervals covering | 5 of 8 | 7 of 8 | 7 of 8 |
| | 2020 eve national PIT | 0.001 | 0.006 | 0.004 |
| | 2020 eve forecast-cycle bias, z of the realised miss | +5.1 | +4.7 | +4.7 |

The windows pass neither half of the review's test (forecast-cycle z-scores within about ±2
and the 2020 eve interval covering), and the data do not ask for them. Across the nine Senate
fits a_post's posterior median runs 0.82–1.36 and a_pre's 0.52–1.49, against a prior median of
1.35, and their 95th percentiles 2.2–4.1 against the prior's 3.9: where the polls say
anything, they pull the windows down (the 2020 eve fit: 0.52 and 0.85). The history and
presidential fits leave a_post at its prior and move a_pre's median by at most 0.21. With L =
60 (Senate fits only) every median falls below the prior's (a_pre 0.44–1.05, a_post
0.45–1.18); a 60-day window holds twice the kernel mass, so the extra variance is about what L
= 30 gave, and so are the scores. The extra spread therefore comes from the prior and lands on
the walk, not on the bias: the 2020 eve fit's forecast-cycle bias SD goes from 1.0 to 1.1
points, and the two extra national covers (2018 eve, 2022 four weeks out) come from intervals
1.5 points wider on average. On the history the paired per-cycle CRPS difference is −0.02 (SE
0.03), better in 7 of 13 cycles. The 2026 Senate forecast barely moves: P(Democrats 51 or
more) 0.720 against 0.719, median 52 seats, House margin D+7.8 (90% D+4.9 to D+10.8, against
D+5.2 to D+10.4).

Why the windows cannot reach the 2020 miss: the walk is pinned at the 2018 result (D+8.7),
and the 2020 cycle's final-60-day generic-ballot polls averaged about D+8.4 (the D+3.1
result plus the +5.3 miss). Polls that end near the last result pin the cycle's bias near
zero, and nothing in the cycle's own polls shows the miss; it shows only at the next result.
Covering it takes a term the polls cannot pin: a zero-centred forecast-cycle term whose spread
comes from past cycles, or a within-cycle drift whose spread the closed epochs teach. The
windows stay switched off by default (`election_vol=0`).

## Forecast recorded before the 2026 election

Recorded on 2 November 2026 from `us_midterm_v4.stan` fitted to the generic ballot alone
(no races, flat fundamentals): 4,201 polls ending from 9 November 2016 to 30 October 2026,
built with `anchor_n=2e5 grid=week tau_scale=0.05` (`stan_data.json` sha256
`87cad1df43ff17438d708f048daad344a8a31374561093ecf3a7bbbf2b10b44d`; the poll tables are in
`data_freeze_2026-11-02.sha256`). 4 chains × 1,000 draws, no divergences, max R-hat 1.008.

| Quantity, 3 November 2026 | Median | 90% interval |
|---|---|---|
| Democratic share of the House vote | 51.9% | 50.3–53.5 |
| Republican share | 45.6% | 43.9–46.9 |
| Other | 2.5% | 1.7–3.8 |
| Two-party Democratic margin | D+6.5 | D+3.7 to D+9.6 |

3,998 of the 4,000 draws give the Democrats more of the two-party vote. Polls of the final four weeks
average D+8.7 two-party; the model subtracts an industry bias it carries over from the
2018–2024 cycles.

### Senate, v9

Recorded on 2 November 2026 from `us_midterm_v9.stan` on the generic ballot and the 35
Senate races (the regular class 2 seats and the Ohio and Florida specials), two-party, with
595 race polls ending by 1 November, built with the Senate backtests' options and
`target=2026-11-03` (`stan_data.json` sha256
`62e8f38adbedbaca321247d2d6b4206496bad3a3b02412d253d3b61c725dd151`). 4 chains × 1,000
draws, no divergences, max R-hat 1.004. 34 Democratic seats are not up.

| Quantity | Median | 90% interval |
|---|---|---|
| Two-party Democratic margin, House vote | D+7.8 | D+5.2 to D+10.4 |
| Democratic seats (an independent in the Democratic slot counted as one) | 52 | 48–55 |
| P(Democrats 51 or more) | 0.72 | 0.69 with no independent counted |

| Race | NYT/Siena, 1 November | Polls of the final 28 days | v9 margin | 90% interval | P(D) |
|---|---|---|---|---|---|
| Michigan | D+11.6 | D+4.5 (18) | D+2.8 | R+4.7 to D+10.5 | 0.74 |
| Maine | D+6.3 | D+4.4 (12) | D+2.2 | R+5.2 to D+9.7 | 0.69 |
| Texas | D+7.5 | D+3.3 (30) | D+1.9 | R+5.8 to D+9.0 | 0.66 |
| Ohio (special) | D+5.4 | D+3.0 (18) | D+1.3 | R+6.3 to D+8.9 | 0.62 |
| Alaska | D+2.1 | D+2.7 (6) | D+0.7 | R+6.9 to D+8.5 | 0.56 |
| Iowa | D+1.1 | D+0.0 (12) | even | R+7.6 to D+7.5 | 0.50 |
| Kansas | D+5.4 | D+2.8 (8) | R+1.0 | R+8.3 to D+6.5 | 0.42 |

Every race's file is `senate/forecast_2026_races.csv` in the study folder. The industry bias
the model takes off the 2024–26 polls is D+2.0 (90% interval R+0.3 to D+4.3).

Refitted on 2 November 2026 after the D-slot fix (`senate_dslot/fit_2026` in the study
folder; not the record above): median 52 seats (48–55), P(Democrats 51 or more) 0.72, 0.68
with no independent counted, House margin D+7.7. No race's P(D) moved by more than 0.03.
