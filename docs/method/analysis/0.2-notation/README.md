# Analysis behind chapter 0.2 (4 Nov 2026)

The script that read the party and house order in `start/notation.html`. No model is fitted. Run it from the repository root with `LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/0.2-notation/d_order.R`.

| Script | Reads | Gives |
|---|---|---|
| `d_order.R` | the live `data/` (git-ignored), through `read_polling_data()` and `read_polling_watch_data(start = 2016-01-01)` in `R/data.R` | each model's party order (with letters from `party_tibble()`) and house order, with the rows per level from 1 Jan 2016, and each party's first date with a national count above 0 |

The house order after `Kosning` (and, in the forecast model, `Félagsvísindastofnun`) follows the order the readers meet the houses in the data, so it can change when the data files are rewritten.
