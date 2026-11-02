# Builds out-of-sample Stan data for usa/stan/us_midterm_v4.stan (and v5, v6) from the
# generic-ballot polls in FiveThirtyEight's pollster-ratings raw_polls.csv: the polls of the
# final 60 days before each House election of 1998-2022, anchored at the national House
# popular vote of each election since 1996 (usa/README.md has where the files come from).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/build_generic_history.R raw=<raw_polls.csv> tables=<dir>
#     out=<dir> target=2014-11-04 [asof=] [anchor_n=] [grid=day|week] [tau_scale=0.1]
#     [bias_walk=0] [phi_info=0] [two_party=0]
#
# The window opens at the 1996 result. target= is one of the elections of 1998-2022: the
# data hold the polls that end before asof= (default: the target's election day) and the
# results of the elections before it, with pred_days running to the target. Each cycle's
# polls fill only its last 60 days, so every epoch's bias is learnt from the polls of
# those weeks against the result that ends it, and the target epoch's from the epochs
# before. A pollster is its 538 pollster rating id, so a house keeps its identity across
# cycles and names. raw_polls has no voter population: pop_n is 0 for every poll, and v5's
# offsets fall back on their priors. Parties, counts, grid=, anchor_n= and the race fields
# are those of usa/R/build_generic_ballot.R. bias_walk and phi_info are v6's switches
# (usa/stan/us_midterm_v6.stan, header): these polls need phi_info = 0, the default.
# two_party=1 drops Other, as in usa/R/build_generic_ballot.R.
# Writes <out>/stan_data.json and <out>/meta.rds.

library(dplyr)
library(readr)
library(cmdstanr)

opt <- list(
  raw = NULL, tables = NULL, out = NULL, target = NULL, asof = "", anchor_n = "", grid = "day",
  tau_scale = "0.1", bias_walk = "0", phi_info = "0", two_party = "0"
)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$raw), !is.null(opt$tables), !is.null(opt$out), !is.null(opt$target))
P <- if (opt$two_party == "1") 2L else 3L
dir.create(opt$out, recursive = TRUE, showWarnings = FALSE)

raw <- read_csv(opt$raw, show_col_types = FALSE, na = c("", "NA")) |>
  filter(type_simple == "House-G-US", location == "US")
stopifnot(all(raw$cand1_party == "DEM"), all(raw$cand2_party == "REP"))
elections <- sort(unique(c(as.Date("1996-11-05"), as.Date(raw$electiondate))))
target <- as.Date(opt$target)
asof <- if (nzchar(opt$asof)) as.Date(opt$asof) else target
stopifnot(target %in% elections[-1], asof <= target)
anchors <- elections[elections < asof]

polls <- raw |>
  transmute(
    house = as.character(pollster_rating_id), pollster, partisan,
    end_date = as.Date(polldate), sample_size = as.numeric(samplesize),
    dem = as.numeric(cand1_pct), rep = as.numeric(cand2_pct)
  ) |>
  filter(end_date < asof, !is.na(dem), !is.na(rep), !is.na(sample_size), sample_size > 0) |>
  arrange(house, end_date, desc(sample_size)) |>
  distinct(house, end_date, .keep_all = TRUE) |>
  mutate(
    y_d = as.integer(round(sample_size * dem / 100)),
    y_r = as.integer(round(sample_size * rep / 100)),
    date = if (opt$grid == "week") end_date - (as.integer(format(end_date, "%u")) - 2L) %% 7L else end_date
  )

pv <- read_csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"), show_col_types = FALSE) |>
  filter(year %in% as.integer(format(anchors, "%Y"))) |>
  arrange(year)
stopifnot(nrow(pv) == length(anchors))
results <- cbind(pv$dem_votes, pv$rep_votes, pv$other_votes)[, seq_len(P), drop = FALSE]
if (nzchar(opt$anchor_n)) {
  results <- round(results / rowSums(results) * as.numeric(opt$anchor_n))
}

houses <- names(sort(table(polls$house), decreasing = TRUE))
house_names <- vapply(houses, function(h) names(which.max(table(polls$pollster[polls$house == h]))), "")
all_dates <- sort(unique(c(polls$date, anchors)))
epoch_of <- function(d) findInterval(as.numeric(d), as.numeric(elections), left.open = TRUE)

y_n <- rbind(
  matrix(as.integer(results), ncol = P),
  cbind(polls$y_d, polls$y_r, 0L)[, seq_len(P), drop = FALSE]
)
house_n <- c(rep(1L, length(anchors)), 1L + match(polls$house, houses))
date_n <- match(c(anchors, polls$date), all_dates)
epoch_raw <- c(rep(1L, length(anchors)), epoch_of(polls$end_date))
used <- sort(unique(epoch_raw[house_n > 1]))
stopifnot("an epoch between two elections has no polls" = all(diff(used) == 1))
epoch_n <- ifelse(house_n > 1, match(epoch_raw, used), 1L)
n_parties_n <- as.integer(rowSums(y_n != 0))
party_index_n <- t(apply(y_n, 1, function(y) {
  nz <- which(y != 0)
  c(nz, rep(0L, P - length(nz)))
}))

stan_data <- list(
  D = length(all_dates), P = P, H = 1L + length(houses), N = nrow(y_n), E = length(used),
  y_n = y_n, house_n = as.integer(house_n), date_n = as.integer(date_n),
  epoch_n = as.integer(epoch_n), n_parties_n = n_parties_n, party_index_n = party_index_n,
  time_diff = as.numeric(diff(all_dates)), n_pred = 1000L,
  estimate_tau = 1L, tau_mu_fixed = 0, tau_house_fixed = 0,
  tau_prior_scale = as.numeric(opt$tau_scale),
  pred_days = as.integer(target - max(all_dates)),
  mu_fund = rep(0, P), sd_fund = 10,
  R = 0L, Q = 1L, X_race = matrix(0, 0, 1), N_hist = 0L, X_hist = matrix(0, 0, 1),
  d_hist = matrix(0, 0, P), N_r = 0L, y_r = matrix(0L, 0, P), house_r = integer(0),
  date_r = integer(0), epoch_r = integer(0), race_r = integer(0), n_parties_r = integer(0),
  party_index_r = matrix(0L, 0, P), is_senate = integer(0), senate_holdover_d = 0L,
  pop_n = rep(0L, nrow(y_n)), pop_r = integer(0),
  bias_walk = as.integer(opt$bias_walk), phi_info = as.numeric(opt$phi_info)
)
write_stan_json(stan_data, file.path(opt$out, "stan_data.json"))
saveRDS(
  list(
    dates = all_dates, houses = unname(house_names), polls = polls, elections = anchors,
    results = results, target = target, asof = asof
  ),
  file.path(opt$out, "meta.rds")
)
cat(sprintf(
  "BUILT D=%d N=%d polls=%d H=%d E=%d first=%s last=%s anchors=%d target=%s pred_days=%d\n",
  stan_data$D, stan_data$N, nrow(polls), stan_data$H, stan_data$E,
  format(min(all_dates)), format(max(all_dates)), length(anchors), format(target), stan_data$pred_days
))
print(table(epoch = epoch_n[house_n > 1]))
