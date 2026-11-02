# Simulates US-scale data for usa/stan/us_midterm_v0.stan (and, through its national
# fields, Stan/polling_watch_v5_epoch.stan, since CmdStan ignores fields a model does not
# declare). For the runtime benchmark of the feasibility study (usa/README.md): the sizes
# come from the data survey, the generating values are plausible, not estimated.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/simulate_us.R out=<dir> [seed=1] [first=2018]
#     [polls=400,500,500,600] [houses=150] [state_houses=60] [senate_polls=400]
#     [house_polls=250] [races=1] [n_senate=35] [n_house=435] [anchor_n=] [grid=day|week]
#     [tau_scale=0.1]
#
# polls: national generic-ballot polls per epoch, oldest first (one per election stretch
# from `first`); races=0 leaves the race layer empty; n_senate and n_house set how many
# 2026 races it holds (a small first fit can take a few states and no districts); anchor_n
# sets every national result's total votes (default: the real House popular vote, about
# 10^8); grid=week moves each poll to the Tuesday on or before its date (election days are
# Tuesdays, so results stay put); tau_scale is the prior scale of the epoch shock SDs.
# Writes <out>/stan_data.json and <out>/truth.rds (the generating values).

library(cmdstanr)

opt <- list(
  out = NULL, seed = "1", first = "2018", polls = "400,500,500,600", houses = "150",
  state_houses = "60", senate_polls = "400", house_polls = "250", races = "1", n_senate = "35",
  n_house = "435", anchor_n = "", grid = "day", tau_scale = "0.1"
)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot("out= is required" = !is.null(opt$out))
set.seed(as.integer(opt$seed))
dir.create(opt$out, recursive = TRUE, showWarnings = FALSE)

centre <- function(x) x - mean(x)
softmax <- function(x) exp(x - max(x)) / sum(exp(x - max(x)))
rdirmult <- function(n, alpha) {
  g <- rgamma(length(alpha), alpha)
  as.integer(rmultinom(1, n, g / sum(g)))
}

P <- 3 # 1 Democratic, 2 Republican, 3 Other
all_elections <- as.Date(c("2018-11-06", "2020-11-03", "2022-11-08", "2024-11-05"))
all_votes <- c(1.14e8, 1.47e8, 1.07e8, 1.50e8) # national House popular vote, order of magnitude
keep <- as.integer(format(all_elections, "%Y")) >= as.integer(opt$first)
elections <- all_elections[keep]
votes <- all_votes[keep]
if (nzchar(opt$anchor_n)) votes <- rep(as.numeric(opt$anchor_n), length(votes))
m <- length(elections)
last_day <- as.Date("2026-11-02")
election_day <- as.Date("2026-11-03")
polls_per_epoch <- as.integer(strsplit(opt$polls, ",")[[1]])
stopifnot("polls= needs one count per epoch" = length(polls_per_epoch) == m)

# True national walk, daily, centred logits
days <- seq(elections[1], election_day, by = "day")
sigma_true <- c(0.003, 0.003, 0.008)
beta_true <- matrix(0, length(days), P)
beta_true[1, ] <- centre(log(c(0.53, 0.45, 0.02)))
for (t in 2:length(days)) {
  beta_true[t, ] <- beta_true[t - 1, ] + centre(rnorm(P, 0, sigma_true))
}
day_index <- function(d) as.integer(d - days[1]) + 1L

# Pollster bias: industry walk over epochs and house deviations (zero-sum)
H_nat <- as.integer(opt$houses)
H_state <- as.integer(opt$state_houses)
H <- 1L + H_nat + H_state # house 1 = election result
E <- m
mu_gamma <- matrix(0, E, P)
mu_gamma[1, ] <- centre(rnorm(P, 0, 0.03))
dev <- t(replicate(H - 1, centre(rnorm(P, 0, 0.04))))
gamma <- array(0, c(E, H, P))
for (e in 1:E) {
  if (e > 1) {
    mu_gamma[e, ] <- mu_gamma[e - 1, ] + centre(rnorm(P, 0, 0.02))
    dev <- dev + t(replicate(H - 1, centre(rnorm(P, 0, 0.02))))
  }
  for (h in 2:H) gamma[e, h, ] <- mu_gamma[e, ] + dev[h - 1, ]
}
phi_true <- exp(rnorm(H - 1, log(800), 0.6))

rows <- list()
add_row <- function(date, house, epoch, y, race = NA_integer_) {
  rows[[length(rows) + 1]] <<- list(date = date, house = house, epoch = epoch, y = y, race = race)
}

# Election results (house 1)
for (j in 1:m) {
  pi_e <- softmax(beta_true[day_index(elections[j]), ])
  add_row(elections[j], 1L, 1L, as.integer(round(votes[j] * pi_e)))
}

# National polls: each epoch runs from the day after its election to the next, denser
# towards the closing election; national pollsters follow a Zipf law, and the 20 largest
# poll in every epoch while each other one polls in an epoch with probability 0.6
nat_weight <- 1 / seq_len(H_nat)^1.1
ends <- c(elections[-1], last_day)
for (e in 1:E) {
  span <- seq(elections[e] + 1, ends[e], by = "day")
  w_day <- exp(-as.numeric(ends[e] - span) / 250)
  n_core <- min(20L, H_nat)
  active <- c(rep(TRUE, n_core), runif(H_nat - n_core) < 0.6)
  for (i in seq_len(polls_per_epoch[e])) {
    d <- span[sample.int(length(span), 1, prob = w_day)]
    h <- 1L + sample.int(H_nat, 1, prob = nat_weight * active)
    n <- as.integer(min(max(round(exp(rnorm(1, log(1100), 0.5))), 300), 15000))
    pi_p <- softmax(beta_true[day_index(d), ] + gamma[e, h, ])
    cols <- if (runif(1) < 0.5) 1:3 else 1:2
    y <- integer(P)
    y[cols] <- rdirmult(n, phi_true[h - 1] * pi_p[cols] / sum(pi_p[cols]))
    add_row(d, h, e, y)
  }
}

# Race layer: by default 35 Senate races and 435 House districts in 2026
races <- as.integer(opt$races) == 1
R_sen <- if (races) as.integer(opt$n_senate) else 0L
R_house <- if (races) as.integer(opt$n_house) else 0L
R <- R_sen + R_house
Q <- 4L # presidential lean, lagged own deviation, incumbency (+1 D, -1 R, 0 open), Senate
theta_true <- rbind(
  c(0.5, -0.5, 0),
  c(0.1, -0.1, 0),
  c(0.03, -0.03, 0),
  centre(c(0, 0, 0.3))
)
sigma_race_true <- 0.06
race_covariates <- function(n, senate) {
  lean <- rnorm(n, 0, if (senate) 0.3 else 0.45)
  lag <- 0.9 * lean + rnorm(n, 0, 0.1)
  inc <- ifelse(runif(n) < 0.15, 0, ifelse(runif(n) < plogis(3 * lean), 1, -1))
  cbind(lean, lag, inc, rep(as.numeric(senate), n))
}
race_deviation <- function(X) {
  X %*% theta_true + t(replicate(nrow(X), centre(rnorm(P, 0, sigma_race_true))))
}
X_race <- matrix(0, 0, Q)
delta_true <- matrix(0, 0, P)
X_hist <- matrix(0, 0, Q)
d_hist <- matrix(0, 0, P)
is_senate <- integer(0)
if (races) {
  X_race <- rbind(race_covariates(R_sen, TRUE), race_covariates(R_house, FALSE))
  delta_true <- race_deviation(X_race)
  is_senate <- c(rep(1L, R_sen), rep(0L, R_house))
  for (j in 1:m) {
    Xh <- rbind(race_covariates(33, TRUE), race_covariates(400, FALSE))
    X_hist <- rbind(X_hist, Xh)
    d_hist <- rbind(d_hist, race_deviation(Xh))
  }
  # Race polls in the open epoch: the most competitive 25 Senate races and 80 districts,
  # from February 2026; 40% by national pollsters, the rest by state pollsters
  closeness <- abs(delta_true[, 1] - delta_true[, 2] + beta_true[day_index(election_day), 1] -
    beta_true[day_index(election_day), 2])
  polled_sen <- order(closeness[seq_len(R_sen)])[seq_len(min(25L, R_sen))]
  polled_house <- R_sen + order(closeness[R_sen + seq_len(R_house)])[seq_len(min(80L, R_house))]
  span <- seq(as.Date("2026-02-01"), last_day, by = "day")
  w_day <- exp(-as.numeric(last_day - span) / 60)
  # WHY: index sampling, since sample(x) on a length-1 x draws from 1:x
  pick <- function(x, k) if (length(x) && k > 0) x[sample.int(length(x), k, replace = TRUE)] else integer(0)
  race_polls <- c(
    pick(polled_sen, as.integer(opt$senate_polls)),
    pick(polled_house, as.integer(opt$house_polls))
  )
  for (r in race_polls) {
    d <- span[sample.int(length(span), 1, prob = w_day)]
    h <- if (runif(1) < 0.4) 1L + sample.int(H_nat, 1, prob = nat_weight) else 1L + H_nat + sample.int(H_state, 1)
    n <- as.integer(min(max(round(exp(rnorm(1, log(600), 0.4))), 200), 5000))
    pi_p <- softmax(beta_true[day_index(d), ] + delta_true[r, ] + gamma[E, h, ])
    cols <- if (runif(1) < 0.4) 1:3 else 1:2
    y <- integer(P)
    y[cols] <- rdirmult(n, phi_true[h - 1] * pi_p[cols] / sum(pi_p[cols]))
    add_row(d, h, E, y, r)
  }
}

tuesday_on_or_before <- function(d) d - (as.integer(format(d, "%u")) - 2L) %% 7L
if (opt$grid == "week") {
  rows <- lapply(rows, function(r) {
    r$date <- tuesday_on_or_before(r$date)
    r
  })
}

# Date levels: every date with a poll or a result
all_dates <- sort(unique(do.call(c, lapply(rows, `[[`, "date"))))
date_level <- function(d) match(d, all_dates)
party_index <- function(y) {
  nz <- which(y != 0)
  c(nz, rep(0L, P - length(nz)))
}
is_race <- vapply(rows, function(r) !is.na(r$race), logical(1))
nat <- rows[!is_race]
rac <- rows[is_race]
mat <- function(rs, f) do.call(rbind, lapply(rs, f))

stan_data <- list(
  D = length(all_dates), P = P, H = H, N = length(nat), E = E,
  y_n = mat(nat, function(r) r$y),
  house_n = vapply(nat, `[[`, integer(1), "house"),
  date_n = vapply(nat, function(r) date_level(r$date), integer(1)),
  epoch_n = vapply(nat, `[[`, integer(1), "epoch"),
  n_parties_n = vapply(nat, function(r) sum(r$y != 0), integer(1)),
  party_index_n = mat(nat, function(r) party_index(r$y)),
  time_diff = as.numeric(diff(all_dates)),
  n_pred = 1000L,
  estimate_tau = 1L, tau_mu_fixed = 0, tau_house_fixed = 0, tau_prior_scale = as.numeric(opt$tau_scale),
  pred_days = as.integer(election_day - max(all_dates)),
  mu_fund = centre(beta_true[day_index(election_day), ] + centre(rnorm(P, 0, 0.08))),
  sd_fund = 0.1,
  R = R, Q = Q, X_race = X_race, N_hist = nrow(X_hist), X_hist = X_hist, d_hist = d_hist,
  N_r = length(rac),
  y_r = if (length(rac)) mat(rac, function(r) r$y) else matrix(0L, 0, P),
  house_r = vapply(rac, `[[`, integer(1), "house"),
  date_r = vapply(rac, function(r) date_level(r$date), integer(1)),
  epoch_r = vapply(rac, `[[`, integer(1), "epoch"),
  race_r = vapply(rac, `[[`, integer(1), "race"),
  n_parties_r = vapply(rac, function(r) sum(r$y != 0), integer(1)),
  party_index_r = if (length(rac)) mat(rac, function(r) party_index(r$y)) else matrix(0L, 0, P),
  is_senate = is_senate,
  senate_holdover_d = 34L # 47 Democratic-caucus seats, 13 of them up in 2026
)
write_stan_json(stan_data, file.path(opt$out, "stan_data.json"))
saveRDS(
  list(
    beta_true = beta_true, days = days, mu_gamma = mu_gamma, gamma = gamma, phi = phi_true,
    theta = theta_true, sigma_race = sigma_race_true, delta = delta_true, dates = all_dates
  ),
  file.path(opt$out, "truth.rds")
)
cat(sprintf(
  "SIM D=%d N=%d H=%d E=%d R=%d N_r=%d N_hist=%d first=%s pred_days=%d\n",
  stan_data$D, stan_data$N, H, E, R, stan_data$N_r, stan_data$N_hist,
  format(min(all_dates)), stan_data$pred_days
))
