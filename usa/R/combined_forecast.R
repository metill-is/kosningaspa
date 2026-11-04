# Summarises a combined fit of usa/stan/us_midterm_v13.stan on races=both data
# (usa/R/build_generic_ballot.R) into the overview page's user_model JSON and CSVs: both
# chambers' seat distributions and control probabilities, their joint outcomes, the national
# two-party House margin and every race.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/combined_forecast.R data=<data dir> fit=<fit dir> out=<dir>
#
# Senate seats are the model's seats_senate_d (seats not up included); Democrats control
# the Senate with 51 or more, since the vice-president breaks a 50-50 tie for the
# Republicans. House seats are seats_house_d plus meta$unopposed_d, the seats only a
# Democrat contests; control is 218 or more. Margins are D minus R as a share of the
# two-party vote, in points. Writes <out>/user_model.json, outcomes.csv, senate_seat_dist.csv,
# house_seat_dist.csv and races.csv.

library(posterior)
library(jsonlite)

opt <- list(data = NULL, fit = NULL, out = NULL, as_of = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
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
senate <- as.vector(as_draws_matrix(fit$draws("seats_senate_d")))
house <- as.vector(as_draws_matrix(fit$draws("seats_house_d"))) + meta$unopposed_d
sen <- races$type == 1L
stopifnot(
  "Senate seats disagree with the race margins" = all(senate == meta$holdover + rowSums(margin[, sen, drop = FALSE] > 0)),
  "House seats disagree with the race margins" = all(house == meta$unopposed_d + rowSums(margin[, !sen, drop = FALSE] > 0))
)

q <- function(x, p) unname(quantile(x, p))
dist <- function(x) {
  t <- table(x) / length(x)
  data.frame(seats = as.integer(names(t)), p = as.numeric(t))
}
sd_s <- dist(senate)
sd_h <- dist(house)
write.csv(sd_s, file.path(opt$out, "senate_seat_dist.csv"), row.names = FALSE)
write.csv(sd_h, file.path(opt$out, "house_seat_dist.csv"), row.names = FALSE)
ds <- senate >= 51
dh <- house >= 218
outcomes <- data.frame(
  outcome = c(
    "p_dem_senate", "p_dem_house", "p_dem_both", "p_rep_both", "p_split_dem_house_rep_senate", "p_split_dem_senate_rep_house",
    "senate_q05", "senate_q50", "senate_q95", "house_q05", "house_q50", "house_q95", "national_q05", "national_q50", "national_q95", "draws"
  ),
  value = c(
    mean(ds), mean(dh), mean(ds & dh), mean(!ds & !dh), mean(dh & !ds), mean(ds & !dh),
    q(senate, 0.05), q(senate, 0.5), q(senate, 0.95), q(house, 0.05), q(house, 0.5), q(house, 0.95),
    q(national, 0.05), q(national, 0.5), q(national, 0.95), length(senate)
  )
)
write.csv(outcomes, file.path(opt$out, "outcomes.csv"), row.names = FALSE)

id <- ifelse(sen, paste0(races$state, "-SEN"), sprintf("%s-%02d", races$state, as.integer(races$dist)))
rt <- data.frame(
  id = id, chamber = ifelse(sen, "senate", "house"), state = races$state, p_dem = colMeans(margin > 0),
  margin_mean = colMeans(margin), margin_q05 = apply(margin, 2, q, 0.05), margin_q50 = apply(margin, 2, q, 0.5),
  margin_q95 = apply(margin, 2, q, 0.95), polls = tabulate(meta$race_polls$race, nbins = R)
)
write.csv(rt, file.path(opt$out, "races.csv"), row.names = FALSE)

seat_block <- function(x, d, control) {
  list(
    p_dem_control = round(mean(control), 4), dem_seats = list(mean = round(mean(x), 2), q05 = q(x, 0.05), q50 = q(x, 0.5), q95 = q(x, 0.95)),
    seat_dist = lapply(seq_len(nrow(d)), function(i) c(d$seats[i], round(d$p[i], 5)))
  )
}
um <- list(
  source = "Kosningaspá (US)", as_of = opt$as_of,
  notes = paste(
    "One model for both chambers (v13): national walk and polling-industry bias shared; each chamber's races regressed on",
    "presidential lean and incumbency, with a state effect shared by a state's Senate race and House districts and a",
    "cycle-wide effect per chamber. Margins are two-party (D minus R share of D+R). Senate control needs 51 seats; House 218."
  ),
  senate = seat_block(senate, sd_s, ds), house = seat_block(house, sd_h, dh),
  joint = list(
    p_dem_both = round(mean(ds & dh), 4), p_rep_both = round(mean(!ds & !dh), 4),
    p_dem_house_rep_senate = round(mean(dh & !ds), 4), p_dem_senate_rep_house = round(mean(ds & !dh), 4)
  ),
  generic_ballot = list(
    dem = round(50 + median(national) / 2, 2), rep = round(50 - median(national) / 2, 2),
    q05_margin = round(q(national, 0.05), 2), q95_margin = round(q(national, 0.95), 2)
  ),
  races = lapply(seq_len(R), function(i) {
    list(
      id = rt$id[i], p_dem = round(rt$p_dem[i], 4), margin_mean = round(rt$margin_mean[i], 2),
      margin_q05 = round(rt$margin_q05[i], 2), margin_q95 = round(rt$margin_q95[i], 2)
    )
  })
)
write_json(um, file.path(opt$out, "user_model.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
cat(sprintf(
  "Senate P(D >= 51) %.3f, median %.0f (90%% %.0f-%.0f) | House P(D >= 218) %.3f, median %.0f (90%% %.0f-%.0f) | both D %.3f, both R %.3f | national D%+.1f (%+.1f to %+.1f)\n",
  mean(ds), q(senate, 0.5), q(senate, 0.05), q(senate, 0.95), mean(dh), q(house, 0.5), q(house, 0.05), q(house, 0.95),
  mean(ds & dh), mean(!ds & !dh), median(national), q(national, 0.05), q(national, 0.95)
))
