# Scores out-of-sample generic-ballot backtests: fits of usa/stan/us_midterm_v4.stan on data
# built by usa/R/build_generic_ballot.R with target= (and asof=), against the national House
# popular vote of the target election.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/score_backtest.R dir=<backtest dir> tables=<dir>
#
# Reads every <dir>/fit_<name> with a fit.rds and its data from <dir>/data_<name>, skipping
# fits whose target election has no result in the table yet (a 2026 fit). Fits of
# usa/stan/us_midterm_v5.stan score the same way; their bias is that of likely-voter polls. Margins
# are two-party, in points: 100 * (D - R) / (D + R). Two point baselines use the same polls
# as the fit: the mean margin of the polls that end in the 28 days before asof, and that
# mean minus the same average's miss at the previous election (none when the data hold no
# polls before it, as for 2018, whose only earlier result is 2016's). CRPS
# of a point forecast is its absolute error. Writes <dir>/backtest.csv and prints it.

library(posterior)
options(width = 160)

opt <- list(dir = NULL, tables = NULL)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$dir), !is.null(opt$tables))

pv <- read.csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"))
two_party <- function(d, r) 100 * (d - r) / (d + r)
actual_margin <- function(date) {
  row <- pv[pv$year == as.integer(format(date, "%Y")), ]
  two_party(row$dem_votes, row$rep_votes)
}
poll_mean <- function(polls, end) {
  p <- polls[polls$end_date >= end - 28 & polls$end_date < end, ]
  c(mean = mean(two_party(p$y_d, p$y_r)), n = nrow(p))
}
crps <- function(x, y) mean(abs(x - y)) - 0.5 * mean(abs(x - sample(x)))

fits <- list.files(opt$dir, "^fit_", full.names = TRUE)
fits <- fits[file.exists(file.path(fits, "fit.rds"))]
rows <- lapply(fits, function(f) {
  name <- sub("^fit_", "", basename(f))
  meta <- readRDS(file.path(opt$dir, paste0("data_", name), "meta.rds"))
  if (!as.integer(format(meta$target, "%Y")) %in% pv$year) {
    return(NULL)
  }
  fit <- readRDS(file.path(f, "fit.rds"))
  diag <- jsonlite::read_json(file.path(f, "results.json"))
  set.seed(1)

  pe <- as_draws_matrix(fit$draws("pi_elec"))
  m <- two_party(pe[, 1], pe[, 2])
  y <- actual_margin(meta$target)
  q <- quantile(m, c(0.05, 0.5, 0.95), names = FALSE)

  mg <- as_draws_matrix(fit$draws("mu_gamma"))
  E <- fit$metadata()$stan_variable_sizes$mu_gamma[1]
  bias <- quantile(50 * (mg[, sprintf("mu_gamma[%d,1]", E)] - mg[, sprintf("mu_gamma[%d,2]", E)]),
    c(0.05, 0.5, 0.95),
    names = FALSE
  )

  polls_now <- poll_mean(meta$polls, meta$asof)
  prev <- tail(meta$elections, 1)
  prev_polls <- if (length(prev)) poll_mean(meta$polls, prev) else c(mean = NA_real_, n = 0)
  prev_miss <- if (prev_polls[["n"]] > 0) prev_polls[["mean"]] - actual_margin(prev) else NA_real_
  corrected <- polls_now[["mean"]] - prev_miss

  data.frame(
    name = name, target = format(meta$target), asof = format(meta$asof), anchors = length(meta$elections),
    actual = y, model_median = q[2], model_q5 = q[1], model_q95 = q[3],
    model_error = q[2] - y, covered_90 = y >= q[1] & y <= q[3], model_crps = crps(m, y),
    p_d_two_party = mean(m > 0),
    bias_median = bias[2], bias_q5 = bias[1], bias_q95 = bias[3],
    polls_fitted = nrow(meta$polls), pollsters = length(meta$houses),
    polls_n = polls_now[["n"]], polls_mean = polls_now[["mean"]], polls_error = polls_now[["mean"]] - y,
    prev_poll_miss = prev_miss, corrected = corrected, corrected_error = corrected - y,
    wall_min = diag$wall_min, max_rhat = diag$max_rhat, min_ess_bulk = diag$min_ess_bulk,
    divergent = diag$divergent
  )
})
out <- do.call(rbind, rows)
out <- out[order(out$asof), ]
write.csv(out, file.path(opt$dir, "backtest.csv"), row.names = FALSE)
num <- vapply(out, is.numeric, logical(1))
out[num] <- lapply(out[num], round, 2)
print(out, row.names = FALSE)
