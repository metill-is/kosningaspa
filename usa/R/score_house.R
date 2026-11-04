# Scores House backtest fits of usa/stan/us_midterm_v9.stan on races=house data
# (usa/R/build_generic_ballot.R) against the results, for the House layer's success bar
# (usa/us-house-layer-plan.html): the Democratic seat count's 90% interval and mid-PIT, and,
# over the contested districts, the margin error, 90% coverage and the Brier score of the
# Democratic win probability.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/score_house.R dir=<backtest dir> tables=<tables dir> raw=<raw dir>
#
# The baseline is uniform swing from the presidential lean: each district's presidential
# margin moved by the national House margin minus the national presidential margin of the
# lean's year, with the national House margin taken from the model's own median, so the
# comparison isolates the district layer. Its win probability is 1 or 0 (Brier as such).
# Writes <dir>/house_scores.csv (one row per fit) and <dir>/house_districts.csv, and prints them.

library(posterior)
options(width = 170)

opt <- list(dir = NULL, tables = NULL, raw = NULL)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
house_tab <- read.csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"))
two_party <- function(d, r) 100 * (d - r) / (d + r)
mid_pit <- function(x, y) mean(x < y) + mean(x == y) / 2
pres <- read.csv(file.path(opt$raw, "medsl", "president.csv"))
pres <- pres[pres$party_simplified %in% c("DEMOCRAT", "REPUBLICAN"), ]
nat_pres <- sapply(split(pres, pres$year), function(p) two_party(sum(p$candidatevotes[p$party_simplified == "DEMOCRAT"]), sum(p$candidatevotes[p$party_simplified == "REPUBLICAN"])))

fits <- list.files(opt$dir, "^fit_", full.names = TRUE)
fits <- fits[file.exists(file.path(fits, "fit.rds"))]
dist_rows <- list()
rows <- lapply(fits, function(f) {
  name <- sub("^fit_", "", basename(f))
  meta <- readRDS(file.path(opt$dir, paste0("data_", name), "meta.rds"))
  races <- meta$races
  year <- as.integer(format(meta$target, "%Y"))
  fit <- readRDS(file.path(f, "fit.rds"))
  be <- as_draws_matrix(fit$draws("beta_elec"))
  de <- as_draws_matrix(fit$draws("delta"))
  R <- nrow(races)
  margin <- vapply(seq_len(R), function(r) {
    100 * tanh((be[, "beta_elec[1]"] + de[, sprintf("delta[%d,1]", r)] - be[, "beta_elec[2]"] - de[, sprintf("delta[%d,2]", r)]) / 2)
  }, numeric(nrow(be)))
  national <- 100 * tanh((be[, "beta_elec[1]"] - be[, "beta_elec[2]"]) / 2)
  actual <- two_party(races$d_share, races$r_share)
  seats <- meta$unopposed_d + rowSums(margin > 0)
  h <- house_tab[house_tab$year == year, ]
  actual_seats <- h$dem_seats
  nat_actual <- two_party(h$dem_votes, h$rep_votes)

  p_d <- colMeans(margin > 0)
  q5 <- apply(margin, 2, quantile, 0.05)
  q95 <- apply(margin, 2, quantile, 0.95)
  med <- apply(margin, 2, median)
  pres_nat <- nat_pres[as.character(races$pres_year)]
  base <- races$lean_margin + (median(national) - pres_nat)
  dist_rows[[name]] <<- data.frame(
    fit = name, state = races$state, district = races$dist, actual = actual, median = med,
    q5 = q5, q95 = q95, p_d = p_d, baseline = base
  )
  data.frame(
    fit = name, year = year, contested = R, unopposed_d = meta$unopposed_d,
    seats_actual = actual_seats, seats_median = median(seats), seats_q5 = quantile(seats, 0.05, names = FALSE),
    seats_q95 = quantile(seats, 0.95, names = FALSE), seats_pit = mid_pit(seats, actual_seats),
    seats_covered = actual_seats >= quantile(seats, 0.05) & actual_seats <= quantile(seats, 0.95),
    national_actual = nat_actual, national_median = median(national), national_pit = mid_pit(national, nat_actual),
    mae = mean(abs(med - actual)), coverage = mean(actual >= q5 & actual <= q95), brier = mean((p_d - (actual > 0))^2),
    called = mean((p_d > 0.5) == (actual > 0)),
    base_mae = mean(abs(base - actual)), base_brier = mean(((base > 0) - (actual > 0))^2), base_called = mean((base > 0) == (actual > 0))
  )
})
out <- do.call(rbind, rows)
write.csv(out, file.path(opt$dir, "house_scores.csv"), row.names = FALSE)
write.csv(do.call(rbind, dist_rows), file.path(opt$dir, "house_districts.csv"), row.names = FALSE)
num <- vapply(out, is.numeric, logical(1))
out[num] <- lapply(out[num], round, 3)
print(out, row.names = FALSE)
