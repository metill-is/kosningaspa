#!/usr/bin/env Rscript
# Reykjavíkurspá 2026 — export posterior draws as parquets for the metill.is
# platform's Python builder.
#
# Reads fit_state.rds (the saved Stan posterior + metadata) and writes:
#   exports/draws_pi.parquet          — per-draw vote-share (8000 × 10)
#   exports/draws_seats.parquet       — per-draw D'Hondt seat allocation (8000 × 10)
#   exports/draws_trajectory.parquet  — 500-draw subsample of per-(date, party) shares
#   exports/traj_summary.parquet      — per-(date, party) bands (q025, q05, q25, q50, q75, q95, q975)
#   exports/polls.parquet             — observed polls (date, pollster, n, per-party fractions)
#   exports/parties.csv               — canonical party metadata (bokstafur, flokkur, litur)
#   exports/meta.json                 — fit_date, forecast_date, last_poll_date, etc.
#
# House effects (gamma) are intentionally omitted from the platform export.

suppressMessages({
  library(tidyverse)
  library(arrow)
  library(jsonlite)
})

Sys.setlocale("LC_ALL", "is_IS.UTF-8")
options(width = 120)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 1) setwd(dirname(sub("^--file=", "", script_arg)))

N_SEATS <- 23
LATEST_CYCLE <- "2026"
N_TRAJECTORY_SUBSAMPLE <- 500 # how many draws to keep for coalition trajectories

state <- readRDS("fit_state.rds")
draws <- state$draws
parties_canonical <- state$parties_canonical |>
  # Editorial rename — the platform displays "Vinstrið" (not "Vinstri græn / Vinstrið")
  # so the chip strip stays readable at narrow viewports.
  mutate(flokkur = if_else(bokstafur == "V_or_A", "Vinstrið", flokkur))
cycle_levels <- state$cycle_levels
time_grid <- state$time_grid
all_obs <- state$all_obs

stopifnot(nrow(parties_canonical) == 10)

dir.create("exports", showWarnings = FALSE)

# ---- 1. Vote-share draws (per-draw, per-party) ----------------------------
draws_pi <- draws$pi_election |>
  as_tibble() |>
  select(starts_with("pi_election[")) |>
  set_names(parties_canonical$bokstafur) |>
  mutate(.draw = row_number()) |>
  pivot_longer(-.draw, names_to = "bokstafur", values_to = "pi")

write_parquet(draws_pi, "exports/draws_pi.parquet")
cat(sprintf("draws_pi: %d rows × 3 cols\n", nrow(draws_pi)))

# ---- 2. D'Hondt seat allocation per draw ----------------------------------
dhondt <- function(votes, n_seats) {
  party_seats <- numeric(length(votes))
  temp <- votes
  while (sum(party_seats) < n_seats) {
    i <- which.max(temp)
    party_seats[i] <- party_seats[i] + 1
    temp[i] <- votes[i] / (party_seats[i] + 1)
  }
  party_seats
}

pi_wide <- draws_pi |>
  select(.draw, bokstafur, pi) |>
  pivot_wider(names_from = bokstafur, values_from = pi)

seat_mat <- pi_wide[, parties_canonical$bokstafur] |>
  as.matrix() |>
  apply(1, dhondt, n_seats = N_SEATS) |>
  t()
colnames(seat_mat) <- parties_canonical$bokstafur

draws_seats <- as_tibble(seat_mat) |>
  mutate(.draw = pi_wide$.draw) |>
  pivot_longer(-.draw, names_to = "bokstafur", values_to = "seats")

write_parquet(draws_seats, "exports/draws_seats.parquet")
cat(sprintf("draws_seats: %d rows × 3 cols\n", nrow(draws_seats)))

# ---- 3. Trajectory summary bands (per-(date, party)) ---------------------
pi_traj_long <- draws$pi_trajectory |>
  as_tibble() |>
  select(starts_with("pi_trajectory[")) |>
  mutate(.draw = row_number()) |>
  pivot_longer(-.draw, names_to = "var", values_to = "pi") |>
  mutate(
    indices = str_extract(var, "(?<=\\[).+(?=\\])"),
    p_idx = as.integer(str_split_i(indices, ",", 1)),
    t_idx = as.integer(str_split_i(indices, ",", 2)),
    bokstafur = parties_canonical$bokstafur[p_idx]
  ) |>
  left_join(
    time_grid |> select(t_global, date, cycle),
    by = c("t_idx" = "t_global")
  ) |>
  filter(cycle == LATEST_CYCLE)

traj_summary <- pi_traj_long |>
  group_by(date, bokstafur) |>
  summarise(
    mean = mean(pi),
    q025 = quantile(pi, 0.025),
    q05 = quantile(pi, 0.05),
    q25 = quantile(pi, 0.25),
    q50 = median(pi),
    q75 = quantile(pi, 0.75),
    q95 = quantile(pi, 0.95),
    q975 = quantile(pi, 0.975),
    .groups = "drop"
  ) |>
  arrange(bokstafur, date)

write_parquet(traj_summary, "exports/traj_summary.parquet")
cat(sprintf(
  "traj_summary: %d rows (%d dates × %d parties)\n",
  nrow(traj_summary),
  n_distinct(traj_summary$date),
  n_distinct(traj_summary$bokstafur)
))

# ---- 4. Subsample of per-draw trajectory for coalition aggregation -------
set.seed(2026)
n_draws_total <- max(pi_traj_long$.draw)
sample_draws <- sort(sample.int(n_draws_total, N_TRAJECTORY_SUBSAMPLE))

traj_draws_subsample <- pi_traj_long |>
  filter(.draw %in% sample_draws) |>
  mutate(.draw = match(.draw, sample_draws)) |> # remap to 1..N
  select(.draw, date, bokstafur, pi) |>
  arrange(.draw, bokstafur, date)

write_parquet(traj_draws_subsample, "exports/draws_trajectory.parquet")
cat(sprintf(
  "draws_trajectory (subsampled): %d rows = %d draws × %d dates × %d parties\n",
  nrow(traj_draws_subsample),
  N_TRAJECTORY_SUBSAMPLE,
  n_distinct(traj_draws_subsample$date),
  n_distinct(traj_draws_subsample$bokstafur)
))

# ---- 5. Polls (for trajectory dots) --------------------------------------
polls_2026 <- all_obs |>
  filter(cycle == LATEST_CYCLE) |>
  rowwise() |>
  mutate(total = sum(c_across(all_of(parties_canonical$bokstafur)))) |>
  ungroup() |>
  mutate(across(all_of(parties_canonical$bokstafur), ~ .x / total)) |>
  select(date, fyrirtaeki, n, all_of(parties_canonical$bokstafur))

write_parquet(polls_2026, "exports/polls.parquet")
cat(sprintf("polls_2026: %d rows\n", nrow(polls_2026)))

# ---- 6. Canonical metadata + meta JSON ----------------------------------
write_csv(parties_canonical, "exports/parties.csv")

forecast_date <- state$forecast_date_2026
non_election_polls <- polls_2026 |> filter(fyrirtaeki != "Kosning")
last_poll <- if (nrow(non_election_polls) > 0) {
  as.character(max(non_election_polls$date))
} else {
  NA_character_
}

meta <- list(
  fit_date = as.character(Sys.Date()),
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  forecast_date = as.character(forecast_date),
  election_date = as.character(forecast_date),
  last_poll_date = last_poll,
  n_seats = N_SEATS,
  threshold_seats = 12L,
  n_draws_full = n_draws_total,
  n_draws_trajectory = N_TRAJECTORY_SUBSAMPLE,
  n_polls_2026 = sum(polls_2026$fyrirtaeki != "Kosning"),
  n_polls_total = nrow(all_obs),
  cycles = cycle_levels,
  pollsters = setdiff(unique(polls_2026$fyrirtaeki), "Kosning")
)

write_json(meta, "exports/meta.json", auto_unbox = TRUE, pretty = TRUE)

cat("\nDone. Wrote 7 files to exports/.\n")
cat(sprintf("Fit date:       %s\n", meta$fit_date))
cat(sprintf("Forecast date:  %s\n", meta$forecast_date))
cat(sprintf("Last poll:      %s\n", meta$last_poll_date))
cat(sprintf("Pollsters:      %s\n", paste(meta$pollsters, collapse = ", ")))
