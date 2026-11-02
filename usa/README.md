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
| `R/simulate_us.R` | Simulates US-scale data in that model's format; the national fields also run `polling_watch_v5_epoch.stan` unchanged. Options in its header |
| `R/build_generic_ballot.R` | Real data for `polling_watch_v5_epoch.stan`, and for v1–v4 with no races: the generic congressional ballot polls of the 2018–2026 cycles, anchored at the national House popular vote of 2018–2024. `target=` builds an out-of-sample backtest of an earlier election |
| `R/check_recovery.R` | Compares a fit on simulated data with the values that generated it: race coefficients, election-day vote, seat counts, each with its 90% interval |
| `R/summarise_generic.R` | Election-day forecast, industry bias per epoch and the walk's margin by date, from a generic-ballot fit of v3 or v4 |
| `R/score_backtest.R` | Scores backtest fits against the popular vote of their target election, beside two poll baselines |
| `us-midterms-feasibility.html` | The write-up: data, model changes, run times, out-of-sample backtests of 2018–2024 and the 2026 national forecast |
| `data_freeze_2026-11-02.sha256` | sha256 of every poll table and raw file gathered on 2 November 2026, committed before the election, so a later 2026 fit can show it used pre-election data only |
| `R/bench_fit.R` | Fits a Stan file on a saved data JSON, times it and prints one `DIAG` line (wall time, ms per gradient, tree depth, divergences, R-hat, ESS) |

The poll tables these scripts read were gathered on 2 November 2026 from Wikipedia, the
Wayback Machine's copy of FiveThirtyEight's poll CSVs, VoteHub and the Clerk of the House.
They live outside the repo in
`/Users/brynjolfurjonsson/.claude-reviews/usa-midterms-feasibility-2026-11-02/tables/`,
since this repo is public and the tables are third-party data.

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
