# CLAUDE.md

## Project Overview

**Kosningaspa** is a Bayesian election forecasting system for Icelandic parliamentary elections (Alþingi). It combines polling data, economic fundamentals, and constituency-level effects using Stan models to predict both vote shares and seat allocations across Iceland's 6 electoral districts.

Authors: Brynjólfur Gauti Guðrúnar Jónsson, Rafael Daniel Vias, Hafsteinn Einarsson, Agnar Freyr Helgason.

## Repository Layout

The repo is code-first. Modelling code lives at the top level; written outputs are nested under `writing/`, each as a self-contained Quarto project rendered from inside its own directory (manuscript: `writing/manuscript/CLAUDE.md`; adding an output: `.claude/rules/quarto-manuscript.md`).

`harness/` holds the v2 backtest harness's committed inputs and generated tables, and its gitignored, sha256-checked snapshot of the live data (`harness/README.md`). Its code is in `R/harness/` and its tests in `tests/harness/`; `LC_ALL=en_US.UTF-8 Rscript R/harness/build_phase0.R` rewrites the generated tables.

## Key Commands

### Run the full model fitting pipeline
The main entry point is `R/fit_polling_and_fundamentals_kjordaemi_model.R`. **This is an interactive script** — run it line-by-line or in sections in R/RStudio, not via `Rscript` or `source()`.

### Update data from Google Sheets
Requires `GOOGLE_MAIL`, `POLLING_SHEET_URL`, and `FUNDAMENTALS_SHEET_URL` environment variables.
```r
box::use(R/data[update_polling_data, update_fundamentals_data, update_constituency_data])
update_polling_data()
update_fundamentals_data()
update_constituency_data()
```

### Testing
The v2 harness has a testthat suite in `tests/harness/`. Run it from the project root with `LC_ALL=en_US.UTF-8 Rscript tests/run.R [filter]`; its real-data tests read `harness/snapshot/` and skip without it. Model validation stays interactive: model diagnostics, posterior predictive checks, and comparing predictions against known results (`R/compare_error.R`, `R/post_analysis.R`).

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
                  constituency_predictions.parquet in data/{date}/
                                                    ↓
          calculate_seats() per draw → seats_draws.parquet (no live script;
          the 2024 forecast used R/archive/predict_seats.R)
                                                    ↓
          process_seats_draws.R, make_new_prediction_plots*.R → Figures/
```

**Publishing to metill.is (Fylgisvakt).** This repo does not publish anything by
itself. After a refit writes `data/<YYYY-MM-DD>/polling_watch_draws.parquet`, run
`uv run --extra data python scripts/regenerate_kosningaspa.py` from
`~/metill-platform`: it reads the newest dated folder here plus
`data/post_election_polls.csv` and writes `data/fylgisvakt/{current,meta,timeseries}.json`
there. Inspect the diff and ship it through a PR in that repo. The platform-side
runbook and checks live in `~/metill-platform/.claude/rules/fylgisvakt.md`.

### Seat Allocation
`R/election_utils.R` implements Iceland's electoral system:
- `dhondt()` — D'Hondt method for regional seats
- `jofnunarsaeti()` — Adjustment seat allocation (9 seats, 5% threshold)
- `calculate_seats(draw, layout = seats_tibble(), ineligible = character(0))` — End-to-end: D'Hondt per constituency, then the adjustment seats by national D'Hondt over lists with ≥ 5%, each placed where the list's next quotient is the largest share of that constituency's votes; lists in `ineligible` (a forecast passes `"Annað"`) win no seat, but their votes still count in the 5% threshold and in those shares; returns `seats` and `kjordaemi_seats`
- 63 total seats in each layout (constituency + adjustment seats). 2024 (`seats_tibble()`): Reykjavík Suður (9 + 2), Reykjavík Norður (9 + 2), Suðvestur (12 + 2), Suður (9 + 1), Norðaustur (9 + 1), Norðvestur (6 + 1). 2013–2021 (`seats_layout_2021()`): Suðvestur (11 + 2) and Norðvestur (7 + 1), the rest as in 2024. `layout_for(election)` returns the layout in force for 2016-10-29, 2017-10-28, 2021-09-25 or 2024-11-30
- No live script turns `constituency_predictions.parquet` into `seats_draws.parquet`: the 2024 forecast used `R/archive/predict_seats.R`, which carries its own copy of the allocator

## Language & Naming Conventions

The codebase mixes English (code structure, documentation) and Icelandic (variable names, data labels):
- `flokkur` = party, `kjordaemi` = constituency, `fyrirtaeki` = polling firm
- `bokstafur` = party letter abbreviation (D, B, S, V, C, P, M, F, J)
- `saeti` = seats, `fylgi` = support, `meirihlutar` = majorities
- `jofnunarsaeti` = adjustment seats, `stjornarslit` = government collapse

## Dependencies

Packages are installed manually (no `renv.lock` or `DESCRIPTION`), so this is the install list: `tidyverse`, `cmdstanr`, `posterior`, `bayesplot`, `arrow`, `googlesheets4`, `here`, `clock`, `box`, `metill`, `gt`, `gtExtras`, `ggiraph`, `scales`. `cmdstanr` requires a working CmdStan installation. `metill` is the custom theme package from Metill providing `theme_metill()`.

## Obsidian Output

Vault: `Metill` (MCP) / `~/Obsidian/Metill/`. Start with `load_topic` (vault `Metill`, query `kosningar`): it opens the strand hub `Kosningar/_MOC.md`, which covers this repo's forecast model plus the Fylgisvakt and Kosningakerfi pages built in `~/metill-platform`.

- **Handoff:** `Kosningar/Kosningar Handoff.md` (current state, overwritten; `/done` routes here).
- **Session notes:** vault-root `Sessions/` with `project: kosningar`.
- **Model knowledge:** `Kosningar/Forecast model/` (incl. `model-lineage-and-known-issues.md`, `reference-invariance-fix.md`); the publish chain: `Kosningar/Fylgisvakt/fylgisvakt-publish-chain.md`.
- **Data sources:** `Knowledge/Data Sources/Polls.md` and `Knowledge/Data Sources/ÍSKOS.md` (this repo's `data_sources.md` stays the per-poll provenance record).

**Where knowledge goes:** what must be true at HEAD → `.claude/rules/` or `docs/` here; decisions, strategy, data-source facts, case histories and domain reference → the hub's folders in the vault; preferences and tooling gotchas → auto-memory, as one-line pointers. Never wikilink memory or rule files from the vault; cite them by absolute path.

## Important Notes

- The `data/` directory (date-stamped parquet snapshots), `data-raw/` (raw CSV from Google Sheets), `results/` (local model output cache) and all `.parquet` files are gitignored. Model outputs live locally only. `Figures/` holds generated plots shared across writing outputs.
- The working directory must be the project root for `box::use(R/...)` imports to resolve.
- `R/archive/` and `Stan/archive/` contain experimental/superseded code — not used in production.
