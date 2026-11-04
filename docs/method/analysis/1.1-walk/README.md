# Analysis behind chapter 1.1 (4 Nov 2026)

Scripts that computed the measured numbers in `forecast/walk.html`. No model is fitted. Run them from the repository root with `LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/1.1-walk/<script>.R`.

| Script | Reads | Gives |
|---|---|---|
| `d_grid.R` | the live `data/` (git-ignored), through `R/sampler_check/fit_headless.R --data-only`, which builds the fit script's 2024-eve Stan data and quits | P, D, the national and kjördæmi rows, T, the gaps between dates, the dates in the 47-day windows, the anchors (votes as entered, lists reported and absent), each list's first date with a national count above 0, the kjördæmi rows by house and date, and the pre-election factor as the grid applies it: scaled days and extra variance-time per stretch at τ's prior median, and the first window date before each election |
| `d_priors.R` | nothing | the LKJ(10) marginal correlation, the pre-election variance factor under Exponential(1), the prior predictive of σ, the days and bridge midpoint sds of the 2024 forecast's stretches, and a multinomial share's sampling sd at the valid-vote totals |

`d_grid.R` was run on 4 Nov 2026, after `R/correct_election_anchors.R` had corrected the live `data/polling_data.csv`. On an uncorrected file the anchor totals differ, and on data changed since then the counts may too.
