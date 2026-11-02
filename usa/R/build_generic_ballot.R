# Builds Stan data for Stan/polling_watch_v5_epoch.stan from the US generic congressional
# ballot polls of the 2018-2026 cycles, anchored at the national House popular vote of
# 2018, 2020, 2022 and 2024 (usa/README.md has where the tables come from).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/build_generic_ballot.R tables=<dir> out=<dir> [anchor_n=]
#
# Parties: 1 Democratic, 2 Republican, 3 Other. A poll gives Democratic and Republican
# counts (its sample size times each share; undecided and other answers are dropped, as
# most polls do not separate them), a result gives all three. One version per poll: the
# likely-voter one if there is one, then registered voters, then adults. anchor_n sets
# each result's total votes (default: the real totals, about 10^8). Writes
# <out>/stan_data.json and <out>/meta.rds (dates, houses, epochs).

library(dplyr)
library(readr)
library(cmdstanr)

opt <- list(tables = NULL, out = NULL, anchor_n = "")
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
stopifnot(!is.null(opt$tables), !is.null(opt$out))
dir.create(opt$out, recursive = TRUE, showWarnings = FALSE)

elections <- as.Date(c("2018-11-06", "2020-11-03", "2022-11-08", "2024-11-05"))
window_start <- as.Date("2016-11-09")

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
    end_date >= window_start, !is.na(dem), !is.na(rep), !is.na(sample_size), sample_size > 0
  ) |>
  arrange(house, end_date, pop_rank, desc(sample_size)) |>
  distinct(house, end_date, .keep_all = TRUE) |>
  mutate(
    y_d = as.integer(round(sample_size * dem / 100)),
    y_r = as.integer(round(sample_size * rep / 100))
  )

pv <- read_csv(file.path(opt$tables, "house_popular_vote_1946_2024.csv"), show_col_types = FALSE) |>
  filter(year %in% c(2018, 2020, 2022, 2024)) |>
  arrange(year)
results <- cbind(pv$dem_votes, pv$rep_votes, pv$other_votes)
if (nzchar(opt$anchor_n)) {
  results <- round(results / rowSums(results) * as.numeric(opt$anchor_n))
}

houses <- names(sort(table(polls$house), decreasing = TRUE))
all_dates <- sort(unique(c(polls$end_date, elections)))
epoch_of <- function(d) 1L + findInterval(as.numeric(d), as.numeric(elections), left.open = TRUE)

y_n <- rbind(
  matrix(as.integer(results), ncol = 3),
  cbind(polls$y_d, polls$y_r, 0L)
)
house_n <- c(rep(1L, length(elections)), 1L + match(polls$house, houses))
date_n <- match(c(elections, polls$end_date), all_dates)
epoch_raw <- c(rep(1L, length(elections)), epoch_of(polls$end_date))
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
  estimate_tau = 1L, tau_mu_fixed = 0, tau_house_fixed = 0, tau_prior_scale = 0.1
)
write_stan_json(stan_data, file.path(opt$out, "stan_data.json"))
saveRDS(
  list(dates = all_dates, houses = houses, polls = polls, elections = elections, results = results),
  file.path(opt$out, "meta.rds")
)
cat(sprintf(
  "BUILT D=%d N=%d polls=%d H=%d E=%d first=%s last=%s anchor_total=%s\n",
  stan_data$D, stan_data$N, nrow(polls), stan_data$H, stan_data$E,
  format(min(all_dates)), format(max(all_dates)), format(sum(results[4, ]), big.mark = ",")
))
print(table(epoch = epoch_n[house_n > 1]))
