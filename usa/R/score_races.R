# Scores the races of out-of-sample Senate backtests: fits of usa/stan/us_midterm_v9.stan
# (or v1-v8) on data built by usa/R/build_generic_ballot.R races=senate target=, against
# each race's result.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/score_races.R dir=<backtest dir>
#
# Reads every <dir>/fit_<name> with a fit.rds and its data from <dir>/data_<name>, skipping
# fits with no races or no results (a 2026 fit). A race's margin is that of its D and R
# slots, 100 * (D - R) / (D + R), from softmax(beta_elec + delta[r]) in each draw; the
# baseline is the mean margin of the race's polls that end in the 28 days before asof (none
# for a race without such polls). Brier scores the D slot's win probability. Writes
# <dir>/races.csv (one row per race) and prints a summary per fit.

library(posterior)
options(width = 160)

opt <- list(dir = NULL)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$dir))

two_party <- function(d, r) 100 * (d - r) / (d + r)

fits <- list.files(opt$dir, "^fit_", full.names = TRUE)
fits <- fits[file.exists(file.path(fits, "fit.rds"))]
rows <- lapply(fits, function(f) {
  name <- sub("^fit_", "", basename(f))
  meta <- readRDS(file.path(opt$dir, paste0("data_", name), "meta.rds"))
  races <- meta$races
  if (is.null(races) || all(is.na(races$d_share))) {
    return(NULL)
  }
  fit <- readRDS(file.path(f, "fit.rds"))
  be <- as_draws_matrix(fit$draws("beta_elec"))
  de <- as_draws_matrix(fit$draws("delta"))
  R <- nrow(races)
  margin <- vapply(seq_len(R), function(r) {
    a <- be[, "beta_elec[1]"] + de[, sprintf("delta[%d,1]", r)]
    b <- be[, "beta_elec[2]"] + de[, sprintf("delta[%d,2]", r)]
    100 * tanh((a - b) / 2)
  }, numeric(nrow(be)))
  rp <- meta$race_polls
  late <- rp[rp$end_date >= meta$asof - 28 & rp$end_date < meta$asof, ]
  poll_mean <- vapply(seq_len(R), function(r) {
    p <- late[late$race == r, ]
    if (nrow(p)) mean(two_party(p$d, p$r)) else NA_real_
  }, numeric(1))
  actual <- two_party(races$d_share, races$r_share)
  q <- apply(margin, 2, quantile, c(0.05, 0.5, 0.95))
  p_d <- colMeans(margin > 0)
  data.frame(
    name = name, target = format(meta$target), asof = format(meta$asof), state = races$state,
    cls = if (!is.null(races$cls)) races$cls else NA, indep_d = races$indep_d,
    polls_total = tabulate(rp$race, R), polls_28d = tabulate(late$race, R),
    actual = actual, model_median = q[2, ], model_q5 = q[1, ], model_q95 = q[3, ],
    model_error = q[2, ] - actual, covered_90 = actual >= q[1, ] & actual <= q[3, ],
    p_d_win = p_d, d_won = actual > 0, brier = (p_d - (actual > 0))^2,
    poll_mean = poll_mean, poll_error = poll_mean - actual
  )
})
out <- do.call(rbind, rows)
write.csv(out, file.path(opt$dir, "races.csv"), row.names = FALSE)
summ <- do.call(rbind, lapply(split(out, out$name), function(x) {
  polled <- !is.na(x$poll_mean)
  data.frame(
    fit = x$name[1], races = nrow(x), mae = mean(abs(x$model_error)),
    mae_polled = mean(abs(x$model_error[polled])), poll_mae = mean(abs(x$poll_error[polled])),
    polled = sum(polled), covered = sum(x$covered_90), brier = mean(x$brier),
    winners_right = sum((x$p_d_win > 0.5) == x$d_won),
    d_seats_expected = sum(x$p_d_win), d_seats_actual = sum(x$d_won)
  )
}))
num <- vapply(summ, is.numeric, logical(1))
summ[num] <- lapply(summ[num], round, 2)
print(summ, row.names = FALSE)
