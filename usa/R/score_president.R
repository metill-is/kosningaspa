# Scores presidential backtest fits of usa/stan/us_midterm_v9.stan, on data from
# usa/R/build_president.R, against the results: the national two-party margin, each
# state's margin (error, 90% coverage, the Democratic win probability's Brier score and
# whether the favourite won) and the electoral votes, each state's going to its statewide
# winner. Maine's and Nebraska's district votes are not modelled, so the actual count
# beside the forecast also gives each of those states' votes to its statewide winner.
# The baseline is the mean margin of each state's polls ending in the 28 days before asof.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/score_president.R dir=<backtest dir> ev=<csv>
#
# ev= is a CSV of year, state_po, ev. Writes <dir>/president_states.csv (one row per
# state) and <dir>/president_summary.csv (one row per fit) and prints the summary.

library(posterior)
options(width = 160)

opt <- list(dir = NULL, ev = NULL)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$dir), !is.null(opt$ev))
ev_table <- read.csv(opt$ev)
two_party <- function(d, r) 100 * (d - r) / (d + r)
crps <- function(x, y) mean(abs(x - y)) - 0.5 * mean(abs(x - sample(x)))

fits <- list.files(opt$dir, "^fit_", full.names = TRUE)
fits <- fits[file.exists(file.path(fits, "fit.rds"))]
scored <- lapply(fits, function(f) {
  name <- sub("^fit_", "", basename(f))
  meta <- readRDS(file.path(opt$dir, paste0("data_", name), "meta.rds"))
  races <- meta$races
  if (all(is.na(races$d_share))) {
    return(NULL)
  }
  set.seed(1)
  year <- as.integer(format(meta$target, "%Y"))
  ev <- ev_table$ev[match(paste(year, races$state), paste(ev_table$year, ev_table$state_po))]
  stopifnot("a state has no electoral votes" = !anyNA(ev), sum(ev) == 538)

  fit <- readRDS(file.path(f, "fit.rds"))
  be <- as_draws_matrix(fit$draws("beta_elec"))
  de <- as_draws_matrix(fit$draws("delta"))
  R <- nrow(races)
  margin <- vapply(seq_len(R), function(r) {
    100 * tanh((be[, "beta_elec[1]"] + de[, sprintf("delta[%d,1]", r)] -
      be[, "beta_elec[2]"] - de[, sprintf("delta[%d,2]", r)]) / 2)
  }, numeric(nrow(be)))
  national <- 100 * tanh((be[, "beta_elec[1]"] - be[, "beta_elec[2]"]) / 2)
  ev_d <- as.vector((margin > 0) %*% ev)

  actual <- two_party(races$d_share, races$r_share)
  rp <- meta$race_polls
  late <- rp[rp$end_date >= meta$asof - 28 & rp$end_date < meta$asof, ]
  poll_mean <- vapply(seq_len(R), function(r) {
    p <- late[late$race == r, ]
    if (nrow(p)) mean(two_party(p$d, p$r)) else NA_real_
  }, numeric(1))
  q <- apply(margin, 2, quantile, c(0.05, 0.5, 0.95))
  p_d <- colMeans(margin > 0)
  states <- data.frame(
    name = name, year = year, state = races$state, ev = ev, polls_28d = tabulate(late$race, R),
    actual = actual, model_median = q[2, ], model_q5 = q[1, ], model_q95 = q[3, ],
    model_error = q[2, ] - actual, covered_90 = actual >= q[1, ] & actual <= q[3, ],
    p_d_win = p_d, d_won = actual > 0, brier = (p_d - (actual > 0))^2,
    called = (p_d > 0.5) == (actual > 0), poll_mean = poll_mean, poll_error = poll_mean - actual,
    poll_called = ifelse(is.na(poll_mean), NA, (poll_mean > 0) == (actual > 0))
  )

  nat_actual <- two_party(meta$national$d, meta$national$r)
  nat_q <- quantile(national, c(0.05, 0.5, 0.95), names = FALSE)
  ev_q <- quantile(ev_d, c(0.05, 0.5, 0.95), names = FALSE)
  ev_actual <- sum(ev[actual > 0])
  polled <- !is.na(poll_mean)
  summary <- data.frame(
    fit = name, year = year,
    national_actual = nat_actual, national_median = nat_q[2], national_q5 = nat_q[1],
    national_q95 = nat_q[3], national_crps = crps(national, nat_actual),
    p_d_president = mean(ev_d >= 270),
    ev_median = ev_q[2], ev_q5 = ev_q[1], ev_q95 = ev_q[3], ev_actual_statewide = ev_actual,
    states_called = sum(states$called), states = R, brier = mean(states$brier),
    state_mae = mean(abs(states$model_error)), covered_90 = sum(states$covered_90),
    polled_states = sum(polled), poll_called = sum(states$poll_called, na.rm = TRUE),
    poll_mae = mean(abs(states$poll_error[polled])), model_mae_polled = mean(abs(states$model_error[polled])),
    missed = paste(states$state[!states$called], collapse = " ")
  )
  list(states = states, summary = summary)
})
scored <- Filter(Negate(is.null), scored)
states <- do.call(rbind, lapply(scored, `[[`, "states"))
summ <- do.call(rbind, lapply(scored, `[[`, "summary"))
write.csv(states, file.path(opt$dir, "president_states.csv"), row.names = FALSE)
write.csv(summ, file.path(opt$dir, "president_summary.csv"), row.names = FALSE)
num <- vapply(summ, is.numeric, logical(1))
summ[num] <- lapply(summ[num], round, 2)
print(summ, row.names = FALSE)
