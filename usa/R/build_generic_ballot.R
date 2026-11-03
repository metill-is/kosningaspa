# Builds Stan data for Stan/polling_watch_v5_epoch.stan from the US generic congressional
# ballot polls of the 2018-2026 cycles, anchored at the national House popular vote of
# 2018, 2020, 2022 and 2024 (usa/README.md has where the tables come from).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/build_generic_ballot.R tables=<dir> out=<dir> [anchor_n=]
#     [grid=day|week] [tau_scale=0.1] [min_polls=1] [top_houses=] [target=2026-11-03] [asof=]
#     [anchor_2016=0] [population=0] [bias_walk=] [phi_info=] [races=senate|house raw=<dir> [house=<dir>]] [two_party=0]
#     [race_poll_sd=auto|<sd>] [mu_bar=free|zero] [bias_tail=<nu>]
#     [election_vol=0|1 vol_days=<L>]
#
# Parties: 1 Democratic, 2 Republican, 3 Other. A poll gives Democratic and Republican
# counts (its sample size times each share; undecided and other answers are dropped, as
# most polls do not separate them), a result gives all three. One version per poll: the
# likely-voter one if there is one, then registered voters, then adults. anchor_n sets
# each result's total votes (default: the real totals, about 10^8). grid=week moves each
# poll to the Tuesday on or before its end date (its epoch still comes from the end date).
# min_polls pools the pollsters with fewer polls than that in the window into one house.
# top_houses=K keeps only the K pollsters with the most polls in the window and drops the
# rest, for a small first fit.
# target= forecasts an earlier election (2018-11-06, 2020-11-03, 2022-11-08 or 2024-11-05)
# out of sample: only the polls that end before asof= (default: the target's election day)
# and the results of the elections before it, with pred_days running to the target.
# anchor_2016=1 adds the 2016 result on the window's first date: a 2018 backtest has no
# other, and usa/stan/us_midterm_v4.stan needs at least one.
# population=1 adds each poll's voter population for usa/stan/us_midterm_v5.stan (pop_n,
# pop_r: 0 likely voters, V or a result, 1 registered voters, 2 adults); the default leaves
# the data as v4's fits saw them, and population=off writes every poll as likely voters (v5's
# offsets switched off; v5 to v8 need the fields). bias_walk= and phi_info=, when given, add v6's switches
# (usa/stan/us_midterm_v6.stan, header; v5's walk and centring are bias_walk=1 phi_info=0.125).
# races=senate adds the target year's Senate races, their polls and the past Senate results
# that teach the race regression, from the raw files under raw= (usa/R/senate_races.R);
# their pollsters join the national ones, and their dates the walk's. two_party=1 drops
# Other: P = 2, results of the Democratic and Republican votes alone (usa/stan/us_midterm_v8.stan).
# races=house house=<dir> adds the target year's House districts instead (usa/R/house_layer.R;
# house= holds house_lean_best.csv and house_lean_2026.csv), every race with is_senate = 0;
# seat totals add meta$unopposed_d, the seats only a Democrat contests, to seats_house_d.
# race_poll_sd=, when given, adds v9's sigma_race_poll (usa/stan/us_midterm_v9.stan): a
# number, or auto for the race polling error of the cycles before the target
# (usa/R/senate_races.R, race_poll_error_sd). mu_bar= and bias_tail=, when given, add v11's
# switches (usa/stan/us_midterm_v11.stan, header): fix_mu_bar (zero = 1) and bias_tail_nu.
# election_vol= and vol_days=, when given, add v12's election-window walk variance
# (usa/stan/us_midterm_v12.stan, header): election_vol and vol_scale_days.
# The data also carry the fields of usa/stan/us_midterm_v1.stan to v4 with no races and a
# flat fundamentals factor (sd_fund 10), so they fit the national walk alone. Writes
# <out>/stan_data.json and <out>/meta.rds (dates, houses, epochs).

library(dplyr)
library(readr)
library(cmdstanr)

opt <- list(
  tables = NULL, out = NULL, anchor_n = "", grid = "day", tau_scale = "0.1", min_polls = "1",
  top_houses = "", target = "2026-11-03", asof = "", anchor_2016 = "0", population = "0",
  bias_walk = "", phi_info = "", races = "", raw = "", house = "", two_party = "0", race_poll_sd = "", mu_bar = "", bias_tail = "", election_vol = "", vol_days = "30"
)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$tables), !is.null(opt$out))
P <- if (opt$two_party == "1") 2L else 3L
dir.create(opt$out, recursive = TRUE, showWarnings = FALSE)

elections <- as.Date(c("2018-11-06", "2020-11-03", "2022-11-08", "2024-11-05"))
window_start <- as.Date("2016-11-09")
target <- as.Date(opt$target)
asof <- if (nzchar(opt$asof)) as.Date(opt$asof) else target
stopifnot(target %in% c(elections, as.Date("2026-11-03")), asof <= target)
anchors <- elections[elections < asof]
if (opt$anchor_2016 == "1") anchors <- c(as.Date("2016-11-08"), anchors)

normalise_house <- function(x) {
  x |>
    tolower() |>
    gsub(pattern = "\\((d|r|i|l)\\)", replacement = "") |>
    sub(pattern = "/.*$", replacement = "") |>
    gsub(pattern = "[^a-z0-9 &]", replacement = " ") |>
    gsub(pattern = "\\b(inc|llc|research|polling|poll|polls|group|strategies|associates|analytics|insights|the)\\b", replacement = " ") |>
    gsub(pattern = "\\s+", replacement = " ") |>
    trimws()
}

to_grid <- function(end_date) {
  if (opt$grid == "week") end_date - (as.integer(format(end_date, "%u")) - 2L) %% 7L else end_date
}

polls <- lapply(c(2018, 2020, 2022, 2024, 2026), function(y) {
  read_csv(file.path(opt$tables, sprintf("generic_ballot_%d.csv", y)), show_col_types = FALSE) |>
    transmute(
      pollster,
      end_date = as.Date(end_date), sample_size = as.numeric(sample_size),
      population = toupper(as.character(population)), dem = as.numeric(dem), rep = as.numeric(rep)
    )
}) |>
  bind_rows() |>
  mutate(
    house = normalise_house(pollster),
    pop_rank = match(population, c("LV", "RV", "V", "A"), nomatch = 5L)
  ) |>
  filter(
    end_date >= window_start, end_date < asof, !is.na(dem), !is.na(rep), !is.na(sample_size), sample_size > 0
  ) |>
  arrange(house, end_date, pop_rank, desc(sample_size)) |>
  distinct(house, end_date, .keep_all = TRUE) |>
  mutate(
    y_d = as.integer(round(sample_size * dem / 100)),
    y_r = as.integer(round(sample_size * rep / 100)),
    date = to_grid(end_date)
  )

pv <- read_csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"), show_col_types = FALSE) |>
  filter(year %in% as.integer(format(anchors, "%Y"))) |>
  arrange(year)
results <- cbind(pv$dem_votes, pv$rep_votes, pv$other_votes)[, seq_len(P), drop = FALSE]
if (nzchar(opt$anchor_n)) {
  results <- round(results / rowSums(results) * as.numeric(opt$anchor_n))
}

if (nzchar(opt$top_houses)) {
  top <- names(sort(table(polls$house), decreasing = TRUE))[seq_len(as.integer(opt$top_houses))]
  polls <- polls[polls$house %in% top, ]
}
small <- names(which(table(polls$house) < as.integer(opt$min_polls)))
polls$house[polls$house %in% small] <- "other pollsters"
race_polls <- tibble(house = character(0), date = as.Date(character(0)))
if (opt$races == "senate") {
  source("usa/R/senate_races.R")
  pv_all <- read_csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"), show_col_types = FALSE)
  sen <- senate_layer(target, asof, window_start, opt$raw, opt$tables, pv_all, normalise_house, P)
  race_polls <- sen$polls |> mutate(date = to_grid(end_date))
}
if (opt$races == "house") {
  source("usa/R/house_layer.R")
  pv_all <- read_csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"), show_col_types = FALSE)
  sen <- house_layer(target, asof, window_start, opt$raw, opt$tables, opt$house, pv_all, normalise_house, P)
  sen$holdover <- 0L
  race_polls <- sen$polls |> mutate(date = to_grid(end_date))
}
houses <- names(sort(table(c(polls$house, race_polls$house)), decreasing = TRUE))
all_dates <- sort(unique(c(polls$date, anchors, race_polls$date)))
epoch_of <- function(d) 1L + findInterval(as.numeric(d), as.numeric(elections), left.open = TRUE)

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
  party_index_r = matrix(0L, 0, P), is_senate = integer(0), senate_holdover_d = 0L
)
if (opt$population == "1") {
  stopifnot(all(polls$population %in% c("LV", "V", "RV", "A")))
  stan_data$pop_n <- c(rep(0L, length(anchors)), match(polls$population, c("LV", "RV", "A"), nomatch = 1L) - 1L)
  stan_data$pop_r <- integer(0)
}
if (opt$population == "off") {
  stan_data$pop_n <- rep(0L, nrow(y_n))
  stan_data$pop_r <- integer(0)
}
if (opt$races %in% c("senate", "house")) {
  epoch_rp <- match(epoch_of(race_polls$end_date), used)
  stopifnot("a race poll falls in an epoch with no national polls" = !anyNA(epoch_rp))
  stan_data[c(
    "R", "Q", "X_race", "N_hist", "X_hist", "d_hist", "N_r", "y_r", "house_r", "date_r", "epoch_r",
    "race_r", "n_parties_r", "party_index_r", "is_senate", "senate_holdover_d"
  )] <- list(
    nrow(sen$races), ncol(sen$races$X), sen$races$X, sen$hist$n, sen$hist$X, sen$hist$d,
    nrow(race_polls), cbind(race_polls$y_d, race_polls$y_r, 0L)[, seq_len(P), drop = FALSE],
    as.integer(1L + match(race_polls$house, houses)), match(race_polls$date, all_dates), as.integer(epoch_rp),
    as.integer(race_polls$race), rep(2L, nrow(race_polls)), cbind(rep(1L, nrow(race_polls)), 2L, 0L)[, seq_len(P), drop = FALSE],
    rep(as.integer(opt$races == "senate"), nrow(sen$races)), as.integer(sen$holdover)
  )
  if (opt$population == "1") {
    stan_data$pop_r <- match(race_polls$population, c("LV", "RV", "A"), nomatch = 1L) - 1L
  }
  if (opt$population == "off") {
    stan_data$pop_r <- rep(0L, nrow(race_polls))
  }
}
if (nzchar(opt$bias_walk)) stan_data$bias_walk <- as.integer(opt$bias_walk)
if (nzchar(opt$phi_info)) stan_data$phi_info <- as.numeric(opt$phi_info)
if (nzchar(opt$mu_bar)) stan_data$fix_mu_bar <- as.integer(opt$mu_bar == "zero")
if (nzchar(opt$bias_tail)) stan_data$bias_tail_nu <- as.numeric(opt$bias_tail)
if (nzchar(opt$election_vol)) {
  stan_data$election_vol <- as.integer(opt$election_vol)
  stan_data$vol_scale_days <- as.numeric(opt$vol_days)
}
if (nzchar(opt$race_poll_sd)) {
  stan_data$sigma_race_poll <- if (opt$race_poll_sd == "auto") {
    race_poll_error_sd(file.path(opt$raw, "gb-538-github", "raw_polls.csv"), as.integer(format(target, "%Y")),
      type = if (opt$races == "house") "House-G" else "Sen-G")
  } else {
    as.numeric(opt$race_poll_sd)
  }
}
write_stan_json(stan_data, file.path(opt$out, "stan_data.json"))
saveRDS(
  c(
    list(
      dates = all_dates, houses = houses, polls = polls, elections = anchors, results = results,
      target = target, asof = asof
    ),
    if (opt$races == "senate") list(races = sen$races, race_polls = race_polls, holdover = sen$holdover),
    if (opt$races == "house") list(races = sen$races, race_polls = race_polls, unopposed_d = sen$unopposed_d)
  ),
  file.path(opt$out, "meta.rds")
)
cat(sprintf(
  "BUILT D=%d N=%d polls=%d H=%d E=%d first=%s last=%s anchors=%d target=%s pred_days=%d\n",
  stan_data$D, stan_data$N, nrow(polls), stan_data$H, stan_data$E,
  format(min(all_dates)), format(max(all_dates)), length(anchors), format(target), stan_data$pred_days
))
print(table(epoch = epoch_n[house_n > 1]))
