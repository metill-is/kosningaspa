# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Kosningaspa** is a Bayesian election forecasting system for Icelandic parliamentary elections (Alþingi). It combines polling data, economic fundamentals, and constituency-level effects using Stan models to predict both vote shares and seat allocations across Iceland's 6 electoral districts.

Authors: Brynjólfur Gauti Guðrúnar Jónsson, Rafael Daniel Vias, Hafsteinn Einarsson, Agnar Freyr Helgason.

## Key Commands

### Render the Quarto manuscript
```bash
quarto render
```
Output goes to `docs/`. The project uses `execute: freeze: true` so R code isn't re-run unless explicitly unfrozen.

### Run the full model fitting pipeline
The main entry point is `R/fit_polling_and_fundamentals_kjordaemi_model.R`. **This is an interactive script** — run it line-by-line or in sections in R/RStudio, not via `Rscript` or `source()`.

### Compile a Stan model
```r
library(cmdstanr)
model <- cmdstan_model("Stan/polling_and_fundamentals_kjordaemi.stan")
```

### Update data from Google Sheets
Requires `GOOGLE_MAIL`, `POLLING_SHEET_URL`, and `FUNDAMENTALS_SHEET_URL` environment variables.
```r
box::use(R/data[update_polling_data, update_fundamentals_data, update_constituency_data])
update_polling_data()
update_fundamentals_data()
update_constituency_data()
```

### Testing
No formal test suite. Validation is done interactively via model diagnostics, posterior predictive checks, and comparing predictions against known results (`R/compare_error.R`, `R/post_analysis.R`).

## Architecture

### Module System
R scripts use `box::use()` for imports with `#' @export` roxygen tags. See `R/CLAUDE.md` for the full module dependency graph.

### Data Pipeline
```
Google Sheets → data.R (update_*) → CSV files in data/
                                          ↓
                          data.R (read_*) → stan_data.R (prepare_stan_data)
                                                    ↓
                                    Stan model fitting (cmdstanr)
                                                    ↓
                          process_seats_draws.R → parquet files in data/{date}/
                                                    ↓
                          make_new_prediction_plots.R → Figures/
```

### Seat Allocation
`R/election_utils.R` implements Iceland's electoral system:
- `dhondt()` — D'Hondt method for regional seats
- `jofnunarsaeti()` — Adjustment seat allocation (9 seats, 5% threshold)
- `calculate_seats(draw)` — End-to-end: applies D'Hondt per constituency, then adjustment seats
- 63 total seats: Reykjavík Suður (9), Reykjavík Norður (9), Suðvestur (11), Suður (9), Norðaustur (9), Norðvestur (7), plus 9 adjustment seats

## Language & Naming Conventions

The codebase mixes English (code structure, documentation) and Icelandic (variable names, data labels):
- `flokkur` = party, `kjordaemi` = constituency, `fyrirtaeki` = polling firm
- `bokstafur` = party letter abbreviation (D, B, S, V, C, P, M, F, J)
- `saeti` = seats, `fylgi` = support, `meirihlutar` = majorities
- `jofnunarsaeti` = adjustment seats, `stjornarslit` = government collapse

## Dependencies

Core R packages: `tidyverse`, `cmdstanr`, `posterior`, `bayesplot`, `arrow`, `googlesheets4`, `here`, `clock`, `box`, `metill` (custom theme package from Metill providing `theme_metill()`), `gt`, `gtExtras`, `ggiraph`, `scales`.

Requires a working CmdStan installation for `cmdstanr`. No `renv.lock` — packages are installed manually.

## Important Notes

- The `data/` directory and all `.parquet` files are gitignored. Model outputs live locally only.
- Compiled Stan binaries (files without `.stan` extension in `Stan/`) are gitignored.
- The working directory must be the project root for `box::use(R/...)` imports to resolve.
- `R/archive/` and `Stan/archive/` contain experimental/superseded code — not used in production.
