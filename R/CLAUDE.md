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
| `fit_polling_watch.R` | `polling_watch.stan` | `polling_watch_draws.parquet` |

### Visualization Scripts (read parquet outputs, produce PNGs)

| Script | Reads | Produces |
|--------|-------|----------|
| `make_new_prediction_plots.R` | `y_rep_draws_constituency.parquet` | Vote share forecast plots |
| `make_new_prediction_plots_seats.R` | `seats_draws.parquet` | Seat forecast plots |
| `make_meirihlutar_plots.R` | `seats_draws.parquet` | Coalition majority probability plots |
| `make_polling_watch_plots.R` | `polling_watch_draws.parquet` + raw polling data | Time series + snapshot plots |
| `plot_model_results.R` | Model fit object + polling data | Diagnostic plots |
| `plot_fundamentals_weight.R` | Model parameters | Fundamentals weight curve |
| `make_historical_prediction_plots.R` | Historical backtesting output | Backtesting comparison plots |

### Data Collection & Analysis

| Script | Purpose |
|--------|---------|
| `scrape_polls.R` | Scrapes polls, maintains hardcoded post-election polls → `data/post_election_polls.csv` |
| `prepare_economy_data.R` | Fetches from hagstofa (Statistics Iceland) and eurostat → `data/economy_data.csv` |
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
