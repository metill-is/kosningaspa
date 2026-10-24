# R Scripts

## Module System

All R modules use `box::use()` for imports with `#' @export` roxygen tags for exported functions. The working directory must be the project root for `R/...` paths to resolve.

```r
box::use(
  R/data[read_polling_data, read_fundamentals_data],
  R/stan_data[prepare_stan_data]
)
```

Interactive fitting scripts (e.g., `fit_*.R`) also use `library()` for heavy dependencies like `tidyverse` and `cmdstanr`.

## Script Categories

### Foundation Modules (importable via box::use)

| Module | Exports | Used By |
|--------|---------|---------|
| `party_utils.R` | `party_tibble()` — party names, abbreviations (bokstafur), colors | data.R, scrape_polls.R, election_utils.R, visualization scripts |
| `data.R` | `read_polling_data()`, `read_fundamentals_data()`, `read_constituency_data()`, `update_*()`, `get_sheet_data()`, `get_election_data()` | All fitting scripts, modeling_utils, post_analysis |
| `stan_data.R` | `prepare_stan_data()`, `prepare_polling_data()`, `prepare_fundamentals_data()`, `prepare_polling_watch_data()` | Each respective fitting script |
| `election_utils.R` | `dhondt()`, `jofnunarsaeti()`, `seats_tibble()` (2024 layout), `seats_layout_2021()` (2013–2021 layout), `layout_for()` (the layout in force at a harness election), `calculate_seats()` (its `ineligible` lists win no seat) | The harness: `harness/info_set.R` (`layout_for()`), `tests/harness/test-seats.R` and `tests/harness/test-truth.R`. No live forecast script calls it; `archive/predict_seats.R` (which wrote the 2024 `seats_draws.parquet` that process_seats_draws.R reads) has its own copy |
| `modeling_utils.R` | `fit_model_at_date()` | Historical backtesting scripts |
| `flow_prior.R` | `flow_prior_target()`, `flow_exchange()`, `share_cov_target()`, `model_coords_target()`, `result_shares()`, `model_parties()`, `to_node()`, `read_switching()` — flow-informed RW-correlation prior target (Laplacian of ÍSKOS gross flows + non-voter reservoir → forecast-model clr coordinates → Omega_0 for a centred LKJ) | build_iskos_flow_targets.R |

### Harness (v2)

Box modules in `R/harness/`, imported as `R/harness/<module>`, with tests in `tests/harness/` (`LC_ALL=en_US.UTF-8 Rscript tests/run.R`). They read only the snapshot and `harness/sources/` (`harness/README.md`).

| Module | Exports | Role |
|--------|---------|------|
| `constants.R` | `project_root()`, `harness_path()`, `WINDOW_START`, `ELECTION_HOUSE`, `ELECTIONS`, `previous_election()`, `TARGETS`, `POLL_PARTIES`, `LETTER_PARTY`, `party_of_letter()`, `FUNDAMENTALS_RENAMED`, `KJORDAEMI` | The window, its elections, the 10 forecast cells, party and kjördæmi labels, paths |
| `inputs.R` | `SNAPSHOT_FILES`, `snapshot_inputs()`, `verify_snapshot()`, `snapshot_file()` | The sha256-checked snapshot of the live data in `harness/snapshot/` |
| `truth.R` | `parse_gild_atkvaedi()`, `parse_thingsaeti()`, `parse_jsonstat_results()`, `parse_kos02018()`, `read_seats_2021()`, `build_truth()`, `write_truth()`, `fold_truth()`, `national_shares()`, `prev_national_shares()` | Official kjördæmi votes and seats 2016–2024 → `harness/truth/elections_official.csv` |
| `availability.R` | `read_polling()`, `read_constituency()`, `parse_lokadagur()`, `poll_list()`, `match_fieldwork()`, `house_max_fieldwork()`, `poll_availability()`, `availability_counts()`, `excluded_near()`, `fieldwork_overlaps()`, `write_availability()` | Each poll's first available day (spec §5.3 rule 1) → `harness/availability.csv`, `harness/availability_report.md` |
| `tables.R` | `epoch_of()`, `read_houses()`, `entry_table()`, `write_entry_table()`, `read_coalitions()`, `cabinet_at()`, `coalition_event()`, `incumbents_at()`, `pm_at()`, `valid_votes()`, `expected_valid_votes()` | Houses, party entry (→ `harness/entry_table.csv`), epochs, cabinets and the Brier coalition, kjördæmi valid votes |
| `fundamentals.R` | `fundamentals_party()`, `fundamentals_inputs()` | The stage-1 training rows and E's target rows as known at F |
| `info_set.R` | `read_n_corrections()`, `read_fundamentals_corrections()`, `read_economy_release()`, `apply_n_corrections()`, `apply_fundamentals_corrections()`, `read_inputs()`, `cell_polls()`, `info_set()`, `validate_inputs()` | One cell's information set, the single gate to an arm's inputs, and its leak check |
| `build_phase0.R` | none: a plain script, not a module | Rewrites the generated files committed under `harness/` that build offline (all but the Wikipedia fieldwork tables and `snapshot_manifest.csv`) and prints the availability counts and each cell's admitted polls: `LC_ALL=en_US.UTF-8 Rscript R/harness/build_phase0.R` |

### Model Fitting Scripts (interactive, run line-by-line)

| Script | Stan Model | Output |
|--------|-----------|--------|
| `fit_polling_and_fundamentals_kjordaemi_model.R` | `polling_and_fundamentals_kjordaemi.stan` | `y_rep_draws_constituency.parquet`, `constituency_predictions.parquet` (no seat draws; see `election_utils.R`) |
| `fit_polling_and_fundamentals_model.R` | `polling_and_fundamentals.stan` | National-level draws |
| `fit_fundamentals_model.R` | `fundamentals.stan` | Fundamentals-only draws |

### Sampler checks for the production model (headless)

| Script | Purpose |
|--------|---------|
| `sampler_check/fit_headless.R` | Fits `polling_and_fundamentals_kjordaemi.stan` (or any `--stan=` file) on the interactive script's `stan_data`, rebuilt field for field, optionally with the polls after a date dropped (`--cutoff=`, for sampler checks only: the election date and fundamentals stay as in the fit script); writes CmdStan CSVs, `timing.rds` and per-chain `diagnostics.csv` to `--out=` |
| `sampler_check/equivalence.R` | Checks the centred δ / log σ reparameterisation (`delta_raw` → `delta_free`, `log_sigma_raw` → `log_sigma`) point by point: at posterior and off-posterior points, new log density minus (old log density minus log-Jacobian) must be constant, with each Jacobian term left out as a negative control; and both models' generated quantities, run at the posterior points with one seed, must agree |
| `sampler_check/compare_fits.R` | Old vs new fit: `treedepth__` distribution, max R-hat, min bulk/tail ESS, ESS per minute and per 1,000 gradients, election-day share quantiles compared within MCSE |
| `sampler_check/RESULTS.md` | Results of the centred δ / log σ + `max_treedepth = 12` change (24 Oct 2026), with the commands that reproduce them |

### Polling-watch fit (Fylgisvakt; headless, run by hand)

| Script | Stan Model | Output |
|--------|-----------|--------|
| `fit_polling_watch.R` | `polling_watch_v4.stan` (production; `polling_watch_v5_epoch.stan` is experimental and only fitted by `fit_polling_watch_epoch.R`) | `data/<YYYY-MM-DD>/` (gitignored): `polling_watch_draws.parquet` (pi_smooth), `polling_watch_omega.parquet` (Omega), `polling_watch_gamma.parquet` (house effects), `polling_watch_mu_gamma.parquet` (industry bias), `polling_watch_fit.rds` (full fit, re-queryable) |

Run `Rscript R/fit_polling_watch.R` by hand after a poll ingest (`.claude/skills/scrape-polls`); it takes about 10 minutes. Nothing schedules it, and the scrape-polls skill hands it off rather than running it. It is stage 2 of the Fylgisvakt publish chain: `/Users/brynjolfurjonsson/metill-platform/scripts/regenerate_kosningaspa.py` then exports the newest dated fit to the JSON in `data/fylgisvakt/` there, and metill.is/fylgisvakt changes only after that JSON is committed, pushed and deployed.

### Visualization Scripts (read parquet outputs, produce PNGs)

| Script | Reads | Produces |
|--------|-------|----------|
| `make_new_prediction_plots.R` | `y_rep_draws_constituency.parquet` | Vote share forecast plots |
| `make_new_prediction_plots_seats.R` | `seats_draws.parquet` | Seat forecast plots |
| `make_meirihlutar_plots.R` | `seats_draws.parquet` | Coalition majority probability plots |
| `make_polling_watch_plots.R` | `polling_watch_draws.parquet` + raw polling data | Time series + snapshot plots |
| `make_correlation_plots.R` | `polling_watch_omega.parquet` | Clustered marginal + partial (precision) correlation heatmap of the latent RW innovations |
| `make_rw_innovation_cov_plots.R` | `polling_watch_fit.rds` (sigma, Omega, pi_smooth) | RW-innovation covariance/correlation and pseudo-inverse precision/partial correlation on the *identified* clr (C·Sz·C) and share (J·Sz·J') scales, per 30 days (`rw_innovation_{covariance,precision}.png`). `make_correlation_plots.R` plots the pre-centring Omega, which v4 does not identify |
| `make_house_effects_plot.R` | `polling_watch_gamma.parquet` + `polling_watch_mu_gamma.parquet` + `polling_watch_draws.parquet`; 2024 election result as a pp baseline | Three house/industry-bias forest plots: logit scale, pp vs last election, pp vs current fylgisvakt (`polling_watch_house_effects{,_pp,_pp_current}.png`) |
| `plot_model_results.R` | Model fit object + polling data | Diagnostic plots |
| `plot_fundamentals_weight.R` | Model parameters | Fundamentals weight curve |
| `make_historical_prediction_plots.R` | Historical backtesting output | Backtesting comparison plots |

### Data Collection & Analysis

| Script | Purpose |
|--------|---------|
| `scrape_polls.R` | Scrapes polls, maintains hardcoded post-election polls → `data/post_election_polls.csv` |
| `prepare_economy_data.R` | Fetches from hagstofa (Statistics Iceland) and eurostat → `data/economy_data.csv` |
| `download_iskos.R` | Downloads the ÍSKOS (Icelandic National Election Study) voter surveys 1983–2021 and the open campaign panels from the GAGNÍS Dataverse → `data-raw/iskos/` (original `.sav` with value labels + codebooks, MD5-checked, `manifest.csv`); idempotent, skips restricted files. Each wave has current (`prtvoteYY`) and recalled previous vote (`prtfvoteYY`). 1983–2017 fall under the GAGNÍS non-commercial user terms; 2021 and the campaign panels are CC0 |
| `build_iskos_switching.R` | Builds election-to-election switching tables (recalled previous vote × current vote) from the 12 ÍSKOS voter surveys in `data-raw/iskos/` → gitignored `data/iskos/`: `switching_long.{parquet,csv}` (wave × weight scheme × cell; specific party + model group, which is the party if the fundamentals data names it for that election, else Annað), `validation.csv` (survey vs official shares), `label_map.csv` (crosswalk audit), `diagnostics.csv` (birth-year ineligibility). Parties are matched by value-label text, never code or letter; non-party answers via the turnout questions; eligibility at the previous election from year of birth. Derived from GAGNÍS-licensed data, so outputs stay out of git |
| `build_iskos_flow_targets.R` | Builds Omega_0 targets (via `flow_prior.R`) for the 2016/2017/2021/2024/2028 forecasts, each from the previous cycle's ÍSKOS table, plus gamma = 2 and no-reservoir variants → gitignored `data/iskos/flow_targets.rds`; prints the 2024 Omega_0 and cycle-to-cycle stability |
| `fit_polling_watch_epoch.R` | Runnable (not interactive) fit of `polling_watch_v4` or `polling_watch_v5_epoch` on a chosen window, optionally a leave-future-out backtest (`--cutoff=<election>`); writes `results/epoch_bias/<tag>/`. Options documented in the header |
| `epoch_bias_experiments.sh` | Driver for the epoch-bias experiment grid (full window, backtests, simulation, sensitivity); logs to `results/epoch_bias/logs/driver.log`. The `bt17` stage is excluded from the default grid and refuses to run until `harness/DECISION.md` exists: 2017 is the v2 harness's held-out confirmatory target |
| `epoch_bias_descriptive.R` | Model-free poll-vs-result error per election (centred log-ratio, pp) → `results/epoch_bias/descriptive_*.csv` |
| `epoch_bias_simulate.R` | Simulates polls with known epoch-varying bias on a fitted design, for recovery checks |
| `epoch_bias_evaluate.R` | Scores the epoch-bias fits: diagnostics, backtests (error, coverage, CRPS), bias of active pollsters per epoch, M−B contrast, simulation recovery → `results/epoch_bias/eval_*.csv`, `Figures/epoch_bias_*.png` |
| `post_analysis.R` | Post-election error analysis against actual results |
| `compare_error.R` | Forecast error comparison across models |

## Dependency Flow

```
party_utils (no deps)
    ↓
data (imports party_utils)
    ↓
stan_data (no R module deps)
    ↓
fit_* scripts (import data + stan_data)
    ↓
[parquet files in data/{date}/]
    ↓
visualization scripts (read parquet only, no box::use of fit_* scripts)
```

Visualization scripts are decoupled from fitting scripts — they communicate only through parquet files on disk.
