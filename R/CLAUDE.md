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
| `stan_data.R` | `prepare_stan_data()`, `prepare_polling_data()`, `prepare_fundamentals_data()`, `prepare_polling_watch_data()` | Each respective fitting script; `harness/r0.R` runs `prepare_stan_data()` for arm R0 |
| `election_utils.R` | `dhondt()`, `jofnunarsaeti()`, `seats_tibble()` (2024 layout), `seats_layout_2021()` (2013–2021 layout), `layout_for()` (the layout in force at a harness election), `calculate_seats()` (its `ineligible` lists win no seat) | The harness: `harness/info_set.R` (`layout_for()`), `harness/scores.R` (`calculate_seats()`), `tests/harness/test-seats.R` and `tests/harness/test-truth.R`. No live forecast script calls it; `archive/predict_seats.R` (which wrote the 2024 `seats_draws.parquet` that process_seats_draws.R reads) has its own copy |
| `modeling_utils.R` | `fit_model_at_date()` | Historical backtesting scripts |
| `flow_prior.R` | `flow_prior_target()`, `flow_exchange()`, `share_cov_target()`, `model_coords_target()`, `result_shares()`, `model_parties()`, `to_node()`, `read_switching()` — flow-informed RW-correlation prior target (Laplacian of ÍSKOS gross flows + non-voter reservoir → forecast-model clr coordinates → Omega_0 for a centred LKJ) | build_iskos_flow_targets.R |

### Harness (v2)

Box modules in `R/harness/`, imported as `R/harness/<module>`, with tests in `tests/harness/` (`LC_ALL=en_US.UTF-8 Rscript tests/run.R`). They read only the snapshot and `harness/sources/` (`harness/README.md`). Their code, and `election_utils.R`'s, writes non-ASCII characters as `\uXXXX` escapes and non-ASCII names with `stats::setNames()`, so a module loaded under the C locale reads the same; comments keep their Icelandic. `tests/harness/test-escapes.R` enforces this (`harness/README.md`, Tests).

| Module | Exports | Role |
|--------|---------|------|
| `constants.R` | `project_root()`, `harness_path()`, `WINDOW_START`, `ELECTION_HOUSE`, `ELECTIONS`, `previous_election()`, `TARGETS`, `POLL_PARTIES`, `LETTER_PARTY`, `party_of_letter()`, `FUNDAMENTALS_RENAMED`, `KJORDAEMI` | The window, its elections, the 10 forecast cells, party and kjördæmi labels, paths |
| `inputs.R` | `SNAPSHOT_FILES`, `snapshot_inputs()`, `verify_snapshot()`, `snapshot_file()` | The sha256-checked snapshot of the live data in `harness/snapshot/`; `snapshot_inputs(files = )` copies any named file set the same way, as `selftest.R` does |
| `truth.R` | `parse_gild_atkvaedi()`, `parse_thingsaeti()`, `parse_jsonstat_results()`, `parse_kos02018()`, `read_seats_2021()`, `build_truth()`, `write_truth()`, `read_truth()`, `fold_truth()`, `cell_truth()`, `national_shares()`, `prev_national_shares()` | Official kjördæmi votes and seats 2016–2024 → `harness/truth/elections_official.csv`, which `read_truth()` reads back with its column types; `cell_truth(E, parties)` is a cell's scoring truth (national shares, kjördæmi votes and shares, seats) over the run's parties, and an error if a party outside them has votes at E |
| `availability.R` | `read_polling()`, `read_constituency()`, `parse_lokadagur()`, `poll_list()`, `match_fieldwork()`, `house_max_fieldwork()`, `poll_availability()`, `availability_counts()`, `excluded_near()`, `fieldwork_overlaps()`, `write_availability()` | Each poll's first available day (spec §5.3 rule 1) → `harness/availability.csv`, `harness/availability_report.md` |
| `tables.R` | `epoch_of()`, `read_houses()`, `entry_table()`, `write_entry_table()`, `read_coalitions()`, `cabinet_at()`, `coalition_event()`, `formed_coalition()`, `incumbents_at()`, `pm_at()`, `valid_votes()`, `expected_valid_votes()` | Houses, party entry (→ `harness/entry_table.csv`), epochs, cabinets, the Brier coalition and the coalition that formed after E (hindsight, read only by `score_cell()`), kjördæmi valid votes |
| `fundamentals.R` | `fundamentals_party()`, `fundamentals_inputs()` | The stage-1 training rows and E's target rows as known at F |
| `info_set.R` | `read_n_corrections()`, `read_fundamentals_corrections()`, `read_economy_release()`, `apply_n_corrections()`, `apply_fundamentals_corrections()`, `read_inputs()`, `cell_polls()`, `info_set()`, `validate_inputs()` | One cell's information set, the single gate to an arm's inputs, and its leak check |
| `scores.R` | `NAMED8`, `crps_draws()`, `wide_draws()`, `party_scores()`, `alr_dss()`, `energy_score()`, `national_scores()`, `mae_se()`, `batch_of()`, `batch_se()`, `kd_to_nat()`, `kd_scores()`, `seat_draws()`, `majority_prob()`, `seat_scores()` | Pure scores of a forecast's draws against a result: national (CRPS10, DSS, alr d² and mDSS, IS90, MAE8, energy score) with their batch-means and MCSE standard errors; kjördæmi (CRPS, MAE over the NAMED8 cells, 90% width and coverage); seats (`seat_draws()` runs each draw through `election_utils.R`'s `calculate_seats()`, `Annað` ineligible by default; seat CRPS, abs error, type-1 90% width and coverage, joint exactness) and the coalition-majority Brier score. The formulas are the overdisp study's, ported line for line |
| `selftest.R` | `SELFTEST_FILES`, `selftest_inputs()`, `selftest_file()`, `selftest_present()` | The scorer self-test's inputs: two draw files of a run an earlier study scored, copied by the snapshot machinery into `harness/selftest/` (gitignored) and sha256-checked against `harness/selftest_manifest.csv`. `tests/harness/test-selftest.R` re-scores them with `scores.R` and reproduces that study's scores to 1e-9 (`harness/README.md`, The scorer self-test) |
| `freeze.R` | `read_sha_list()`, `verify_sha_list()`, `is_committed()`, `target_cell()`, `freeze_guard()` | The freeze guard (spec §5.7): a target cell may be scored only once `harness/FREEZE.md` and `harness/FREEZE.sha256` exist, the list covers FREEZE.md, every listed sha256 matches and both files are committed; a 2017 cell also needs `harness/DECISION.md` committed with its sha256, and never once `harness/CONFIRM.md` exists (`harness/README.md`, The freeze and the 2017 locks) |
| `scorer.R` | `SE_SCORES`, `read_run_meta()`, `score_cell()` | The only function that scores a cell against its result: reads the run's `meta.json`, calls `freeze_guard()`, refuses 2017 (scored by the Tier C runbook), and only then reads the result and the draws; returns the national scores of the kjördæmi aggregate (scored) and the eday national reading (reported) with batch SEs, per-party, kjördæmi and seat scores |
| `runs.R` | `EXTRACT_VERSION`, `SAMPLER`, `results_root()`, `run_id()`, `run_dir()`, `sha256_file()`, `stan_data_sha()`, `write_frozen_inputs()`, `expected_meta()`, `meta_mismatch()`, `retire_run()`, `summarise_diagnostics()`, `diagnostics_gate()`, `diag_line()` | Run records: ids and directories under `results/harness/` (`HARNESS_RESULTS` overrides), the frozen read-only Stan data (written atomically; `stan_data_sha()` hashes freshly built data for the sha-skip), the sha-skip keys and their comparison, retiring a stale run to `_stale/`, the diagnostics summary over parameters and scored quantities, the §5.7 gate (a scored quantity with an NA R-hat or ESS fails it) and the DIAG line (`harness/README.md`, Runs) |
| `r0.R` | `R0_KJORDAEMI`, `read_r0_config()`, `r0_production_inputs()`, `r0_stan_data()`, `R0_PARAM_VARIABLES`, `R0_EXTRACT_VARIABLES`, `r0_extract()` | Arm R0's inputs (spec §5.4): a cell's `info_set()` in the format production's fit script gives `prepare_stan_data()`, that function run as it is, then the fixed overrides (campaign days, expected valid votes and kjördæmi weights by name, the hedge scale `mu_gamma_scale`, the active parties per date, the newcomer prediction column) and the "R0 input:" guards; settings in `harness/configs/r0.json` (`harness/README.md`, R0's inputs). Its extraction: `r0_extract()` writes a fit's draws as the §5.5 parquets (`eday_kjordaemi`, `eday_national`, `latent_national`, `param_draws`), labelling kjördæmi and parties by the Stan indices in the element names, never by column position (`harness/README.md`, Runs) |
| `arms.R` | `ARMS`, `arm()` | The arm registry: each arm's Stan file, config and config reader, builder (`info_set()` to Stan data and meta), extractor and the variables it reads; Phase 1 has `r0` |
| `build_phase0.R` | none: a plain script, not a module | Rewrites the generated files committed under `harness/` that build offline (all but the Wikipedia fieldwork tables and `snapshot_manifest.csv`) and prints the availability counts and each cell's admitted polls: `LC_ALL=en_US.UTF-8 Rscript R/harness/build_phase0.R` |
| `fit_cell.R` | none: a plain script, not a module | The cell runner: fits one arm on one cell (`LC_ALL=en_US.UTF-8 Rscript R/harness/fit_cell.R <arm> <E> <F> <seed> <iter_warmup> <iter_sampling>`): builds and validates the inputs, skips a run whose `meta.json` matches (SKIP) or retires one that does not (STALE), freezes the inputs, fits with `SAMPLER`, extracts the parquets, writes `key_diagnostics.csv`, applies the gate, writes `meta.json` last and prints the DIAG line (`harness/README.md`, Runs) |
| `queue.sh` | none: a zsh script | The two-lane queue (`zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>`): refuses to start beside a running `fit_cell.R` unless `FORCE=1`, runs the full slow suite as a preflight, then lanes A and B of the run list (`harness/runs/`) side by side, logging to `${HARNESS_RESULTS:-results/harness}/logs/`; `DRY_RUN=1` only lists the runs |
| `tier0_report.R` | none: a plain script, not a module | The Tier 0 report (`LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R <arm> <run_list> [out]`): reads only each run's `meta.json` and writes one row of sampler diagnostics per run to `harness/tier0/<arm>.md` (minutes, divergences, treedepth hits, E-BFMI, R-hat and ESS over the gate's scope (parameters block + scored) and over the scored quantities, ESS per minute and its linear projection to 2000 sampling iterations, the informational gate), with the CmdStan version and `hw.ncpu`; no forecast quantity (`harness/README.md`, Tier 0) |

### Model Fitting Scripts (interactive, run line-by-line)

| Script | Stan Model | Output |
|--------|-----------|--------|
| `fit_polling_and_fundamentals_kjordaemi_model.R` | `polling_and_fundamentals_kjordaemi.stan` | `y_rep_draws_constituency.parquet`, `constituency_predictions.parquet` (no seat draws; see `election_utils.R`) |
| `fit_polling_and_fundamentals_model.R` | `polling_and_fundamentals.stan` | National-level draws |
| `fit_fundamentals_model.R` | `fundamentals.stan` | Fundamentals-only draws |

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
