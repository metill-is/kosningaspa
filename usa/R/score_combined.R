# Scores combined backtest fits (usa/stan/us_midterm_v13.stan or v14 on races=both data from
# usa/R/build_generic_ballot.R) against the results and against the polls, for the live
# page's historical comparison (usa/accuracy_export.py).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/score_combined.R dir=<backtest dir> tables=<tables dir> [model=<label>]
#
# Per fit: the national two-party House margin (model median, the plain average of the
# generic-ballot polls ending in the final 28 days, the result); House seats (median and 90%
# interval against the result); House districts (margin MAE, 90% coverage, Brier score,
# winners called), with uniform swing from the presidential lean, moved by the model's own
# national median, as the baseline; and Senate races with polls (model margin MAE against the
# 28-day poll average's, coverage, Brier, winners called, the polls' winners called). Margins
# are D minus R as a share of the two-party vote, in points. Writes <dir>/scores.csv.

library(posterior)
options(width = 200)

opt <- list(dir = NULL, tables = NULL, model = "")
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
ht <- read.csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"))
tp <- function(d, r) 100 * (d - r) / (d + r)
pit <- function(x, y) mean(x < y) + mean(x == y) / 2

rows <- lapply(list.files(opt$dir, "^fit_", full.names = TRUE), function(f) {
  if (!file.exists(file.path(f, "fit.rds"))) {
    return(NULL)
  }
  n <- sub("^fit_", "", basename(f))
  m <- readRDS(file.path(opt$dir, paste0("data_", n), "meta.rds"))
  r <- m$races
  y <- as.integer(format(m$target, "%Y"))
  fit <- readRDS(file.path(f, "fit.rds"))
  be <- as_draws_matrix(fit$draws("beta_elec"))
  de <- as_draws_matrix(fit$draws("delta"))
  mg <- sapply(seq_len(nrow(r)), function(i) 100 * tanh((be[, 1] + de[, sprintf("delta[%d,1]", i)] - be[, 2] - de[, sprintf("delta[%d,2]", i)]) / 2))
  nat <- 100 * tanh((be[, 1] - be[, 2]) / 2)
  act <- tp(r$d_share, r$r_share)
  h <- r$type == 2
  hrow <- ht[ht$year == y, ]
  nat_act <- tp(hrow$dem_votes, hrow$rep_votes)
  gp <- m$polls[m$polls$end_date >= m$asof - 28 & m$polls$end_date < m$asof, ]
  nat_polls <- tp(sum(gp$y_d), sum(gp$y_r))
  seats <- m$unopposed_d + rowSums(mg[, h, drop = FALSE] > 0)
  q5 <- apply(mg, 2, quantile, 0.05)
  q95 <- apply(mg, 2, quantile, 0.95)
  pd <- colMeans(mg > 0)
  med <- apply(mg, 2, median)
  win <- act > 0
  base <- r$lean_margin - c(`2016` = 2.2, `2020` = 4.5, `2024` = -1.6)[as.character(r$pres_year)] + median(nat)
  rp <- m$race_polls[m$race_polls$end_date >= m$asof - 28, ]
  poll_mean <- tapply(tp(rp$y_d, rp$y_r), rp$race, mean)[as.character(seq_len(nrow(r)))]
  sp <- !h & !is.na(act) & !is.na(poll_mean)
  data.frame(
    model = opt$model, fit = n, year = y,
    national_actual = round(nat_act, 2), national_model = round(median(nat), 2), national_polls = round(nat_polls, 2),
    national_q5 = round(quantile(nat, 0.05, names = FALSE), 2), national_q95 = round(quantile(nat, 0.95, names = FALSE), 2),
    national_pit = round(pit(nat, nat_act), 3),
    house_seats_actual = hrow$dem_seats, house_seats_model = median(seats),
    house_seats_q5 = quantile(seats, 0.05, names = FALSE), house_seats_q95 = quantile(seats, 0.95, names = FALSE),
    house_seats_pit = round(pit(seats, hrow$dem_seats), 3),
    house_mae = round(mean(abs(med - act)[h]), 2), house_cov90 = round(mean((act >= q5 & act <= q95)[h]), 3),
    house_brier = round(mean(((pd - win)^2)[h]), 4), house_called = round(mean(((pd > 0.5) == win)[h]), 3),
    house_base_mae = round(mean(abs(base - act)[h]), 2), house_base_brier = round(mean((((base > 0) - win)^2)[h]), 4),
    senate_polled = sum(sp), senate_mae = round(mean(abs(med - act)[sp]), 2), senate_polls_mae = round(mean(abs(poll_mean - act)[sp]), 2),
    senate_cov90 = round(mean((act >= q5 & act <= q95)[!h & !is.na(act)]), 3), senate_brier = round(mean(((pd - win)^2)[!h & !is.na(act)]), 4),
    senate_called = sum(((pd > 0.5) == win)[!h & !is.na(act)]), senate_n = sum(!h & !is.na(act)),
    senate_polls_called = sum(((poll_mean > 0) == win)[sp]),
    row.names = NULL
  )
})
out <- do.call(rbind, rows)
write.csv(out, file.path(opt$dir, "scores.csv"), row.names = FALSE)
print(out, row.names = FALSE)
