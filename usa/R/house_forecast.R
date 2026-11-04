# Summarises a House fit of usa/stan/us_midterm_v9.stan on races=house data
# (usa/R/build_generic_ballot.R): the Democratic seat distribution, P(Democrats win 218 or
# more), the national two-party margin and every contested district.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/house_forecast.R data=<data dir> fit=<fit dir> out=<dir>
#
# A draw's seats are the districts whose Democratic two-party margin is above zero plus
# meta$unopposed_d, the seats only a Democrat contests; the model's own seats_house_d counts
# the first part, and the two are checked against each other. Margins are D minus R as a
# share of the two-party vote, in points. Writes <out>/house_seat_dist.csv (seats, p),
# house_outcomes.csv (outcome, value) and house_races.csv (state, district, lean, inc,
# p_d, margin_mean, margin_q05, margin_q50, margin_q95, polls).

library(posterior)

opt <- list(data = NULL, fit = NULL, out = NULL)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
dir.create(opt$out, showWarnings = FALSE, recursive = TRUE)

meta <- readRDS(file.path(opt$data, "meta.rds"))
fit <- readRDS(file.path(opt$fit, "fit.rds"))
races <- meta$races
R <- nrow(races)
be <- as_draws_matrix(fit$draws("beta_elec"))
de <- as_draws_matrix(fit$draws("delta"))
margin <- vapply(seq_len(R), function(r) {
  100 * tanh((be[, "beta_elec[1]"] + de[, sprintf("delta[%d,1]", r)] - be[, "beta_elec[2]"] - de[, sprintf("delta[%d,2]", r)]) / 2)
}, numeric(nrow(be)))
national <- 100 * tanh((be[, "beta_elec[1]"] - be[, "beta_elec[2]"]) / 2)
seats <- meta$unopposed_d + rowSums(margin > 0)
model_seats <- as.vector(as_draws_matrix(fit$draws("seats_house_d")))
stopifnot("seat counts disagree with seats_house_d" = all(seats == meta$unopposed_d + model_seats))

dist <- as.data.frame(table(seats) / length(seats))
names(dist) <- c("seats", "p")
write.csv(dist, file.path(opt$out, "house_seat_dist.csv"), row.names = FALSE)
q <- function(x, p) unname(quantile(x, p))
out <- data.frame(
  outcome = c(
    "p_dem_control", "seats_mean", "seats_q05", "seats_q50", "seats_q95", "national_margin_q05", "national_margin_q50",
    "national_margin_q95", "unopposed_d", "contested", "draws"
  ),
  value = c(
    mean(seats >= 218), mean(seats), q(seats, 0.05), q(seats, 0.5), q(seats, 0.95), q(national, 0.05), q(national, 0.5),
    q(national, 0.95), meta$unopposed_d, R, length(seats)
  )
)
write.csv(out, file.path(opt$out, "house_outcomes.csv"), row.names = FALSE)
npolls <- tabulate(meta$race_polls$race, nbins = R)
rt <- data.frame(
  state = races$state, district = races$dist, lean = races$lean_margin, inc = races$inc,
  p_d = colMeans(margin > 0), margin_mean = colMeans(margin), margin_q05 = apply(margin, 2, q, 0.05),
  margin_q50 = apply(margin, 2, q, 0.5), margin_q95 = apply(margin, 2, q, 0.95), polls = npolls
)
write.csv(rt, file.path(opt$out, "house_races.csv"), row.names = FALSE)
cat(sprintf(
  "P(D >= 218) %.3f | D seats median %d (90%% %d-%d) | national D%+.1f (90%% %+.1f to %+.1f) | %d contested, %d unopposed D\n",
  mean(seats >= 218), as.integer(q(seats, 0.5)), as.integer(q(seats, 0.05)), as.integer(q(seats, 0.95)),
  q(national, 0.5), q(national, 0.05), q(national, 0.95), R, meta$unopposed_d
))
close <- rt[order(abs(rt$p_d - 0.5)), ][1:20, ]
close[, -(1:2)] <- lapply(close[, -(1:2)], function(x) round(x, 2))
print(close, row.names = FALSE)
