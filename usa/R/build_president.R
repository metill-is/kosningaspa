# Builds out-of-sample Stan data for usa/stan/us_midterm_v9.stan (P = 2) for a US
# presidential election, from FiveThirtyEight's pollster-ratings raw_polls.csv (CC BY 4.0):
# the national and state polls of the final 60 days of each cycle 2000-2020, anchored at
# the national presidential popular vote of each election since 1996 (MEDSL, CC0). Each
# state and DC is a race; its deviation from the national vote has a regression prior
# learnt from the state results since 1984, on its leans at the two elections before (a
# state's lean is its log(D / R) minus the nation's). usa/README.md has where the files
# come from.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/build_president.R raw=<dir> out=<dir> target=2016-11-08
#     [asof=] [anchor_n=2e5] [grid=week] [tau_scale=0.05] [phi_info=0] [race_poll_sd=auto]
#     [mu_bar=free] [bias_tail=0] [election_vol=0] [vol_days=30]
#
# target= is one of the elections of 2004-2024. 2024 adds the 2024 cycle's final 60 days
# from FiveThirtyEight's president_polls.csv (CC BY 4.0; its Wayback copy of 5 November
# 2024, raw/president/538-president-polls-20241105/): the general-election questions that
# are not hypothetical and not ranked-choice reallocations, with a Democrat and a
# Republican, one per pollster, area and end date (likely voters first, then the
# head-to-head, then the largest sample). The data hold the polls that end before
# asof= (default: election day) and the results before it, with pred_days running to the
# target. A state poll is a poll of the target's race only in the target's cycle; the
# earlier cycles' national polls stay, for the pollsters' biases. Each epoch is a cycle's
# final 60 days and the industry bias is stationary
# (bias_walk = 0), as in usa/R/build_generic_history.R. Maine's and Nebraska's district
# polls are left out: each state is one race, all its electoral votes going to the
# statewide winner (usa/R/score_president.R). race_poll_sd=auto sets sigma_race_poll from
# the state polls of the cycles before the target (usa/R/senate_races.R,
# race_poll_error_sd). mu_bar=zero and bias_tail=<nu> set v11's switches
# (usa/stan/us_midterm_v11.stan, header), fix_mu_bar and bias_tail_nu, by default free (0)
# and Gaussian (0); older models ignore them. election_vol=1 and vol_days=<L> set v12's
# election-window walk variance (usa/stan/us_midterm_v12.stan, header), off by default.
# Writes <out>/stan_data.json and <out>/meta.rds.

library(dplyr)
library(readr)
library(cmdstanr)
source("usa/R/senate_races.R")

opt <- list(
  raw = NULL, out = NULL, target = NULL, asof = "", anchor_n = "2e5", grid = "week",
  tau_scale = "0.05", phi_info = "0", race_poll_sd = "auto", mu_bar = "free", bias_tail = "0",
  election_vol = "0", vol_days = "30", eve_sd = "0"
)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$raw), !is.null(opt$out), !is.null(opt$target))
P <- 2L
dir.create(opt$out, recursive = TRUE, showWarnings = FALSE)
raw_polls <- file.path(opt$raw, "gb-538-github", "raw_polls.csv")

raw <- read_csv(raw_polls, show_col_types = FALSE) |>
  filter(type_simple == "Pres-G", location %in% c("US", state.abb, "DC"))
stopifnot(all(raw$cand1_party == "DEM"), all(raw$cand2_party == "REP"))
election_2024 <- as.Date("2024-11-05")
elections <- sort(unique(c(as.Date("1996-11-05"), as.Date(raw$electiondate), election_2024)))
target <- as.Date(opt$target)
asof <- if (nzchar(opt$asof)) as.Date(opt$asof) else target
stopifnot(target %in% elections[-(1:2)], asof <= target)
anchors <- elections[elections < asof]
ty <- as.integer(format(target, "%Y"))
to_grid <- function(d) if (opt$grid == "week") d - (as.integer(format(d, "%u")) - 2L) %% 7L else d

polls_538_2024 <- function(file) {
  read_csv(file, col_types = cols(.default = "c")) |>
    filter(stage == "general", hypothetical == "false", ranked_choice_reallocated != "true") |>
    mutate(end_date = as.Date(end_date, "%m/%d/%y"), pct = as.numeric(pct)) |>
    filter(end_date >= election_2024 - 60) |>
    group_by(question_id) |>
    summarise(
      location = if (is.na(first(state))) "US" else c(state.abb, "DC")[match(first(state), c(state.name, "District of Columbia"))],
      house = coalesce(first(pollster_rating_id), paste("538", first(pollster))),
      pollster = first(pollster), end_date = first(end_date), sample_size = as.numeric(first(sample_size)),
      pop_rank = match(first(population), c("lv", "rv", "v", "a"), nomatch = 5L),
      n_answers = n(), d = sum(pct[party == "DEM"]), r = sum(pct[party == "REP"]), .groups = "drop"
    ) |>
    filter(!is.na(location), d > 0, r > 0) |>
    arrange(location, house, end_date, pop_rank, n_answers, desc(sample_size)) |>
    distinct(location, house, end_date, .keep_all = TRUE) |>
    select(location, house, pollster, end_date, sample_size, d, r)
}

all_polls <- raw |>
  transmute(
    location,
    house = as.character(pollster_rating_id), pollster, end_date = as.Date(polldate),
    sample_size = as.numeric(samplesize), d = as.numeric(cand1_pct), r = as.numeric(cand2_pct)
  ) |>
  bind_rows(if (ty >= 2024) {
    polls_538_2024(file.path(opt$raw, "president", "538-president-polls-20241105", "president_polls.csv"))
  }) |>
  filter(end_date < asof, !is.na(d), !is.na(r), !is.na(sample_size), sample_size > 0) |>
  arrange(location, house, end_date, desc(sample_size)) |>
  distinct(location, house, end_date, .keep_all = TRUE) |>
  mutate(
    y_d = as.integer(round(sample_size * d / 100)),
    y_r = as.integer(round(sample_size * r / 100)),
    date = to_grid(end_date)
  )
polls <- filter(all_polls, location == "US")

pres <- rename(presidential_votes(file.path(opt$raw, "medsl", "president.csv")), state = state_po)
nat <- pres |>
  group_by(year) |>
  summarise(d = sum(d), r = sum(r))
lean <- pres |>
  left_join(transmute(nat, year, nat = log(d / r)), by = "year") |>
  mutate(lean = log(d / r) - nat)
lean_at <- function(y, st) lean$lean[match(paste(y, st), paste(lean$year, lean$state))]

anchor_votes <- nat[match(as.integer(format(anchors, "%Y")), nat$year), ]
stopifnot(!anyNA(anchor_votes$d))
results <- round(cbind(anchor_votes$d, anchor_votes$r) / (anchor_votes$d + anchor_votes$r) *
  as.numeric(opt$anchor_n))

past <- lean |>
  filter(year >= 1984, year < ty) |>
  mutate(l1 = lean_at(year - 4, state), l2 = lean_at(year - 8, state)) |>
  filter(!is.na(l1), !is.na(l2))
hist <- list(X = cbind(1, past$l1, past$l2), d = cbind(past$lean, -past$lean) / 2, n = nrow(past))

outcome <- filter(pres, year == ty)
races <- tibble(state = sort(lean$state[lean$year == ty - 4])) |>
  mutate(
    l1 = lean_at(ty - 4, state), l2 = lean_at(ty - 8, state), cls = NA_integer_, indep_d = FALSE,
    d_share = outcome$d[match(state, outcome$state)] / (outcome$d + outcome$r)[match(state, outcome$state)],
    r_share = 1 - d_share
  )
stopifnot("a state has no lean" = !anyNA(races$l1) && !anyNA(races$l2))
races$X <- cbind(1, races$l1, races$l2)
race_polls <- all_polls |>
  filter(location != "US", end_date > max(elections[elections < target])) |>
  mutate(race = match(location, races$state)) |>
  filter(!is.na(race))

houses <- names(sort(table(c(polls$house, race_polls$house)), decreasing = TRUE))
house_names <- vapply(houses, function(h) {
  names(which.max(table(c(polls$pollster[polls$house == h], race_polls$pollster[race_polls$house == h]))))
}, "")
all_dates <- sort(unique(c(polls$date, anchors, race_polls$date)))
epoch_of <- function(d) findInterval(as.numeric(d), as.numeric(elections), left.open = TRUE)

y_n <- rbind(matrix(as.integer(results), ncol = P), cbind(polls$y_d, polls$y_r))
house_n <- c(rep(1L, length(anchors)), 1L + match(polls$house, houses))
date_n <- match(c(anchors, polls$date), all_dates)
epoch_raw <- c(rep(1L, length(anchors)), epoch_of(polls$end_date))
used <- sort(unique(epoch_raw[house_n > 1]))
stopifnot("an epoch between two elections has no polls" = all(diff(used) == 1))
epoch_n <- ifelse(house_n > 1, match(epoch_raw, used), 1L)
epoch_rp <- match(epoch_of(race_polls$end_date), used)
stopifnot("a state poll falls in an epoch with no national polls" = !anyNA(epoch_rp))

stan_data <- list(
  D = length(all_dates), P = P, H = 1L + length(houses), N = nrow(y_n), E = length(used),
  y_n = y_n, house_n = as.integer(house_n), date_n = as.integer(date_n),
  epoch_n = as.integer(epoch_n), n_parties_n = rep(2L, nrow(y_n)),
  party_index_n = matrix(rep(1:2, each = nrow(y_n)), ncol = 2),
  time_diff = as.numeric(diff(all_dates)), n_pred = 1000L,
  estimate_tau = 1L, tau_mu_fixed = 0, tau_house_fixed = 0,
  tau_prior_scale = as.numeric(opt$tau_scale),
  pred_days = as.integer(target - max(all_dates)),
  mu_fund = rep(0, P), sd_fund = 10,
  R = nrow(races), Q = ncol(races$X), X_race = races$X, N_hist = hist$n, X_hist = hist$X,
  d_hist = hist$d, N_r = nrow(race_polls), y_r = cbind(race_polls$y_d, race_polls$y_r),
  house_r = as.integer(1L + match(race_polls$house, houses)),
  date_r = match(race_polls$date, all_dates), epoch_r = as.integer(epoch_rp),
  race_r = as.integer(race_polls$race), n_parties_r = rep(2L, nrow(race_polls)),
  party_index_r = matrix(rep(1:2, each = nrow(race_polls)), ncol = 2),
  is_senate = rep(1L, nrow(races)), senate_holdover_d = 0L,
  pop_n = rep(0L, nrow(y_n)), pop_r = rep(0L, nrow(race_polls)),
  bias_walk = 0L, phi_info = as.numeric(opt$phi_info),
  sigma_race_poll = if (opt$race_poll_sd == "auto") race_poll_error_sd(raw_polls, ty, "Pres-G") else as.numeric(opt$race_poll_sd),
  fix_mu_bar = as.integer(opt$mu_bar == "zero"), bias_tail_nu = as.numeric(opt$bias_tail),
  election_vol = as.integer(opt$election_vol), vol_scale_days = as.numeric(opt$vol_days)
)
# v13/v14 fields (ignored by v9-v12): every state is a race of type 1; each past result is
# its own state-year, and its cycle carries the cycle-wide effect shared by all states (the
# error shared across states that 2016 and 2020 needed); eve_sd= is v14's election-day
# national shock in margin points.
stan_data$type_r <- rep(1L, stan_data$R)
stan_data$type_hist <- rep(1L, stan_data$N_hist)
stan_data$S <- stan_data$R
stan_data$state_r <- seq_len(stan_data$R)
stan_data$G_hist <- stan_data$N_hist
stan_data$grp_hist <- seq_len(stan_data$N_hist)
stan_data$Y_hist <- length(unique(past$year))
stan_data$year_hist <- match(past$year, sort(unique(past$year)))
stan_data$sigma_race_poll2 <- c(stan_data$sigma_race_poll, stan_data$sigma_race_poll)
stan_data$s_eve <- as.numeric(opt$eve_sd) / 100
write_stan_json(stan_data, file.path(opt$out, "stan_data.json"))
saveRDS(
  list(
    dates = all_dates, houses = unname(house_names), polls = polls, elections = anchors,
    results = results, target = target, asof = asof, races = races, race_polls = race_polls,
    national = filter(nat, year == ty)
  ),
  file.path(opt$out, "meta.rds")
)
cat(sprintf(
  "BUILT D=%d N=%d polls=%d H=%d E=%d R=%d N_r=%d N_hist=%d sigma_race_poll=%.4f first=%s last=%s anchors=%d target=%s pred_days=%d\n",
  stan_data$D, stan_data$N, nrow(polls), stan_data$H, stan_data$E, stan_data$R, stan_data$N_r,
  stan_data$N_hist, stan_data$sigma_race_poll, format(min(all_dates)), format(max(all_dates)),
  length(anchors), format(target), stan_data$pred_days
))
