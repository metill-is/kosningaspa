# R scripts

Modules load with `box::use(R/<module>[...])` from the project root; each exports with `#' @export` and documents its exports in roxygen headers. Run scripts as `LC_ALL=en_US.UTF-8 Rscript R/<script>.R`.

## Entry points

- Fylgisvakt: `R/fit_polling_watch.R`, headless, run by hand after a poll ingest; writes data/<date>/, which metill-platform's scripts/regenerate_kosningaspa.py exports.
- 2024 forecast: `R/fit_polling_and_fundamentals_kjordaemi_model.R`, interactive (line by line, never `Rscript`).
- Seats: `R/predict_seats.R`, built on `R/election_utils.R`.

## Foundation modules

- `R/party_utils.R`: party names, letters and colours.
- `R/data.R`: reads and updates the polling, fundamentals and constituency data.
- `R/stan_data.R`: builds the Stan data lists.
- `R/election_utils.R`: D'Hondt, adjustment seats, seat layouts, `calculate_seats()`.

## Harness

`R/harness/` is the v2 backtest harness: inputs, arms R0 and V2, fits, scoring and the freeze guard. Its operator page is `harness/README.md`; its tests are in `tests/harness/`. Harness code and `R/election_utils.R` write non-ASCII characters as `\uXXXX` escapes so a module reads the same under the C locale; `tests/harness/test-escapes.R` enforces it.

## Sampler checks

`R/sampler_check/`: headless fits, equivalence checks and fit comparisons for sampler changes; the record is `R/sampler_check/RESULTS.md`.

## Plot and data scripts

- Plots: `make_new_prediction_plots.R`, `make_new_prediction_plots_seats.R`, `make_meirihlutar_plots.R`, `make_polling_watch_plots.R`, `make_house_effects_plot.R`.
- Data: `scrape_polls.R` (post-election polls), `prepare_economy_data.R` (economy data), `process_seats_draws.R` (seat draws to tables).
- Analysis: `post_analysis.R`, `compare_error.R`; `fit_polling_watch_epoch.R` (Fylgisvakt fits on a chosen window, backtests).

`R/archive/` holds superseded scripts; nothing live calls them.

## Election anchors

`R/correct_election_anchors.R` corrected the valid-vote totals (`n`) of the `Kosning` rows in `data/polling_data.csv`. Back the CSV up before running it. The first refit after it reads new 2016, 2017 and 2021 anchors (party shares move by up to 0.36 points), and `prepare_stan_data()`'s `n_pred` changes too.
