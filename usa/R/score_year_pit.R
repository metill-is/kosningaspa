# Year-level calibration of Senate or presidential backtest fits of usa/stan/us_midterm_v9.stan
# (or v10): where each year's outcome falls in its forecast distribution. Race-level coverage
# counts each race as an independent trial, but the races of one year share their errors, so a
# year whose races all miss the same way shows up here as a PIT near 0 or 1.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/score_year_pit.R dir=<backtest dir> house=<csv> [ev=<csv>]
#
# For each fit, four statistics are computed in every draw and set against the result:
#   national          the national two-party margin (the House vote for a Senate fit, from
#                     house=, the year,dem_votes,rep_votes table; meta$national for a
#                     presidential fit)
#   race_mean         the mean two-party margin of the fit's races
#   race_vs_national  race_mean minus national, which a race-type common error moves
#   d_seats           the races the D slot wins (Senate), or its electoral votes, each state's
#                     going to its statewide winner (presidential, with ev= a CSV of year,
#                     state_po, ev)
# The PIT is the mid-PIT, P(X < y) + P(X = y) / 2, which treats the discrete seat count fairly.
# Writes <dir>/year_pit.csv and prints it. Skips fits with no results.

library(posterior)
options(width = 160)

opt <- list(dir = NULL, house = NULL, ev = NULL)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$dir), !is.null(opt$house) || !is.null(opt$ev))
house <- if (!is.null(opt$house)) read.csv(opt$house)
ev_table <- if (!is.null(opt$ev)) read.csv(opt$ev)
two_party <- function(d, r) 100 * (d - r) / (d + r)
mid_pit <- function(x, y) mean(x < y) + mean(x == y) / 2

fits <- list.files(opt$dir, "^fit_", full.names = TRUE)
fits <- fits[file.exists(file.path(fits, "fit.rds"))]
rows <- lapply(fits, function(f) {
  name <- sub("^fit_", "", basename(f))
  meta <- readRDS(file.path(opt$dir, paste0("data_", name), "meta.rds"))
  races <- meta$races
  if (is.null(races) || all(is.na(races$d_share))) {
    return(NULL)
  }
  year <- as.integer(format(meta$target, "%Y"))
  fit <- readRDS(file.path(f, "fit.rds"))
  be <- as_draws_matrix(fit$draws("beta_elec"))
  de <- as_draws_matrix(fit$draws("delta"))
  R <- nrow(races)
  margin <- vapply(seq_len(R), function(r) {
    100 * tanh((be[, "beta_elec[1]"] + de[, sprintf("delta[%d,1]", r)] -
      be[, "beta_elec[2]"] - de[, sprintf("delta[%d,2]", r)]) / 2)
  }, numeric(nrow(be)))
  actual <- two_party(races$d_share, races$r_share)

  if (!is.null(meta$national)) {
    nat_actual <- two_party(meta$national$d, meta$national$r)
  } else {
    h <- house[house$year == year, ]
    stopifnot("no House vote for the target year" = nrow(h) == 1)
    nat_actual <- two_party(h$dem_votes, h$rep_votes)
  }
  if (!is.null(ev_table)) {
    w <- ev_table$ev[match(paste(year, races$state), paste(ev_table$year, ev_table$state_po))]
    stopifnot("a state has no electoral votes" = !anyNA(w))
  } else {
    w <- rep(1, R)
  }

  national <- 100 * tanh((be[, "beta_elec[1]"] - be[, "beta_elec[2]"]) / 2)
  draws <- list(
    national = national,
    race_mean = rowMeans(margin),
    race_vs_national = rowMeans(margin) - national,
    d_seats = as.vector((margin > 0) %*% w)
  )
  observed <- list(
    national = nat_actual,
    race_mean = mean(actual),
    race_vs_national = mean(actual) - nat_actual,
    d_seats = sum(w[actual > 0])
  )
  do.call(rbind, lapply(names(draws), function(s) {
    q <- quantile(draws[[s]], c(0.05, 0.5, 0.95), names = FALSE)
    data.frame(
      fit = name, year = year, races = R, stat = s, actual = observed[[s]],
      median = q[2], q5 = q[1], q95 = q[3], pit = mid_pit(draws[[s]], observed[[s]])
    )
  }))
})
out <- do.call(rbind, rows)
write.csv(out, file.path(opt$dir, "year_pit.csv"), row.names = FALSE)
num <- vapply(out, is.numeric, logical(1))
out[num] <- lapply(out[num], round, 3)
print(out[order(out$stat, out$fit), ], row.names = FALSE)
