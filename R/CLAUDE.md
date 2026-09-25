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
| `election_utils.R` | `dhondt()`, `jofnunarsaeti()`, `seats_tibble()`, `calculate_seats()` | predict_seats.R, process_seats_draws.R |
| `modeling_utils.R` | `fit_model_at_date()` | Historical backtesting scripts |

### Model Fitting Scripts (interactive, run line-by-line)

| Script | Stan Model | Output |
|--------|-----------|--------|
| `fit_polling_and_fundamentals_kjordaemi_model.R` | `polling_and_fundamentals_kjordaemi.stan` | `y_rep_draws_constituency.parquet`, `seats_draws.parquet` |
| `fit_polling_and_fundamentals_model.R` | `polling_and_fundamentals.stan` | National-level draws |
| `fit_fundamentals_model.R` | `fundamentals.stan` | Fundamentals-only draws |
| `fit_polling_watch.R` | `polling_watch_v4.stan` | `polling_watch_draws.parquet` (pi_smooth), `polling_watch_omega.parquet` (Omega), `polling_watch_gamma.parquet` (house effects), `polling_watch_mu_gamma.parquet` (industry bias), `polling_watch_fit.rds` (full fit, re-queryable) |

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
