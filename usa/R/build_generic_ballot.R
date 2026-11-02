# Builds Stan data for Stan/polling_watch_v5_epoch.stan from the US generic congressional
# ballot polls of the 2018-2026 cycles, anchored at the national House popular vote of
# 2018, 2020, 2022 and 2024 (usa/README.md has where the tables come from).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/build_generic_ballot.R tables=<dir> out=<dir> [anchor_n=]
#     [grid=day|week] [tau_scale=0.1] [min_polls=1] [top_houses=] [target=2026-11-03] [asof=]
#     [anchor_2016=0]
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
# The data also carry the fields of usa/stan/us_midterm_v1.stan to v4 with no races and a
# flat fundamentals factor (sd_fund 10), so they fit the national walk alone. Writes
# <out>/stan_data.json and <out>/meta.rds (dates, houses, epochs).

library(dplyr)
library(readr)
library(cmdstanr)

opt <- list(
  tables = NULL, out = NULL, anchor_n = "", grid = "day", tau_scale = "0.1", min_polls = "1",
  top_houses = "", target = "2026-11-03", asof = "", anchor_2016 = "0"
)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$tables), !is.null(opt$out))
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
    date = if (opt$grid == "week") end_date - (as.integer(format(end_date, "%u")) - 2L) %% 7L else end_date
  )

pv <- read_csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"), show_col_types = FALSE) |>
  filter(year %in% as.integer(format(anchors, "%Y"))) |>
  arrange(year)
results <- cbind(pv$dem_votes, pv$rep_votes, pv$other_votes)
if (nzchar(opt$anchor_n)) {
  results <- round(results / rowSums(results) * as.numeric(opt$anchor_n))
}

if (nzchar(opt$top_houses)) {
  top <- names(sort(table(polls$house), decreasing = TRUE))[seq_len(as.integer(opt$top_houses))]
  polls <- polls[polls$house %in% top, ]
}
small <- names(which(table(polls$house) < as.integer(opt$min_polls)))
polls$house[polls$house %in% small] <- "other pollsters"
houses <- names(sort(table(polls$house), decreasing = TRUE))
all_dates <- sort(unique(c(polls$date, anchors)))
epoch_of <- function(d) 1L + findInterval(as.numeric(d), as.numeric(elections), left.open = TRUE)

y_n <- rbind(
  matrix(as.integer(results), ncol = 3),
  cbind(polls$y_d, polls$y_r, 0L)
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
  c(nz, rep(0L, 3 - length(nz)))
}))

stan_data <- list(
  D = length(all_dates), P = 3L, H = 1L + length(houses), N = nrow(y_n), E = length(used),
  y_n = y_n, house_n = as.integer(house_n), date_n = as.integer(date_n),
  epoch_n = as.integer(epoch_n), n_parties_n = n_parties_n, party_index_n = party_index_n,
  time_diff = as.numeric(diff(all_dates)), n_pred = 1000L,
  estimate_tau = 1L, tau_mu_fixed = 0, tau_house_fixed = 0,
  tau_prior_scale = as.numeric(opt$tau_scale),
  pred_days = as.integer(target - max(all_dates)),
  mu_fund = c(0, 0, 0), sd_fund = 10,
  R = 0L, Q = 1L, X_race = matrix(0, 0, 1), N_hist = 0L, X_hist = matrix(0, 0, 1),
  d_hist = matrix(0, 0, 3), N_r = 0L, y_r = matrix(0L, 0, 3), house_r = integer(0),
  date_r = integer(0), epoch_r = integer(0), race_r = integer(0), n_parties_r = integer(0),
  party_index_r = matrix(0L, 0, 3), is_senate = integer(0), senate_holdover_d = 0L
)
write_stan_json(stan_data, file.path(opt$out, "stan_data.json"))
saveRDS(
  list(
    dates = all_dates, houses = houses, polls = polls, elections = anchors, results = results,
    target = target, asof = asof
  ),
  file.path(opt$out, "meta.rds")
)
cat(sprintf(
  "BUILT D=%d N=%d polls=%d H=%d E=%d first=%s last=%s anchors=%d target=%s pred_days=%d\n",
  stan_data$D, stan_data$N, nrow(polls), stan_data$H, stan_data$E,
  format(min(all_dates)), format(max(all_dates)), length(anchors), format(target), stan_data$pred_days
))
print(table(epoch = epoch_n[house_n > 1]))
