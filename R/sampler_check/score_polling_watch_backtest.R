# Scores leave-future-out Fylgisvakt fits written by
# R/sampler_check/fit_polling_watch_headless.R with --cutoff=<E>: the smoothed shares
# (pi_smooth) at each fit's last date against the official national result at E
# (harness/truth/elections_official.csv), in percentage points: MAE and RMSE of the
# posterior medians, mean CRPS, and the coverage and mean width of the 90% intervals, then
# each party's median error.
#
# Usage, from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/score_polling_watch_backtest.R <E> \
#     <fit dir> [<fit dir> ...]
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(width = 160, warn = 1)
suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(dplyr)
  library(tidyr)
})
box::use(R / harness / scores[crps_draws])

args <- commandArgs(trailingOnly = TRUE)
election <- as.Date(args[1])
# WHY: 2017 is the v2 harness's held-out confirmatory target (R/epoch_bias_experiments.sh)
if (election == as.Date("2017-10-28") && !file.exists("harness/DECISION.md")) {
  stop("2017 is held out until harness/DECISION.md exists")
}
truth <- read.csv("harness/truth/elections_official.csv", fileEncoding = "UTF-8") |>
  filter(as.Date(election) == .env$election) |>
  summarise(votes = sum(votes), .by = flokkur) |>
  mutate(share = 100 * votes / sum(votes), key = tolower(flokkur))
stopifnot("no official result at that date" = nrow(truth) > 0)

score <- function(dir) {
  meta <- readRDS(file.path(dir, "meta.rds"))
  D <- nrow(meta$date_mapping)
  stopifnot("the fit saw the election or a later date" = meta$date_mapping$date[D] < election)
  files <- list.files(file.path(dir, "cmdstan"), pattern = "\\.csv$", full.names = TRUE)
  draws <- as_draws_matrix(read_cmdstan_csv(files, variables = "pi_smooth")$post_warmup_draws)
  m <- 100 * unclass(draws[, paste0("pi_smooth[", D, ",", seq_along(meta$party_names), "]")])
  key <- tolower(meta$party_names)
  y <- truth$share[match(key, truth$key)]
  stopifnot(
    "a fitted party is missing from the result" = !anyNA(y),
    "the result has votes outside the fitted parties" = all(truth$key %in% key)
  )
  q <- apply(m, 2, quantile, c(0.05, 0.5, 0.95))
  tibble(
    fit = basename(dir), last_date = meta$date_mapping$date[D], party = meta$party_names,
    result = y, err = q[2, ] - y,
    crps = vapply(seq_along(y), \(j) crps_draws(m[, j], y[j]), numeric(1)),
    covered = y >= q[1, ] & y <= q[3, ], width = q[3, ] - q[1, ]
  )
}

per <- bind_rows(lapply(args[-1], score))
cat("== election", format(election), "\n")
per |>
  summarise(
    last_date = first(last_date), mae_pp = mean(abs(err)), rmse_pp = sqrt(mean(err^2)),
    crps_pp = mean(crps), coverage90 = mean(covered), width90_pp = mean(width), .by = fit
  ) |>
  print()
cat("\n== per party: median - result (pp)\n")
per |>
  select(fit, party, result, err) |>
  pivot_wider(names_from = fit, values_from = err) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  print()
