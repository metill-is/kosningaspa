suppressMessages({
  library(tidyverse)
  library(cmdstanr)
})

Sys.setlocale("LC_ALL", "is_IS.UTF-8")
options(width = 120)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 1) setwd(dirname(sub("^--file=", "", script_arg)))

FORECAST_DATE_2026 <- as.Date("2026-05-16")

parties_canonical <- tribble(
  ~bokstafur, ~flokkur, ~litur,
  "D", "Sjálfstæðisflokkur", "#377eb8",
  "S", "Samfylkingin", "#e41a1c",
  "B", "Framsóknarflokkur", "#41ab5d",
  "C", "Viðreisn", "#ff7d14",
  "M", "Miðflokkur", "#08306b",
  "F", "Flokkur fólksins", "#FBB829",
  "P", "Píratar", "#984ea3",
  "J", "Sósíalistaflokkur", "#67000d",
  "V_or_A", "Vinstri græn / Vinstrið", "#183F38",
  "Other", "Annað", "#888888"
)
P_CANON <- nrow(parties_canonical)

polls_2026 <- read_csv(file("polls.csv", encoding = "UTF-8"), show_col_types = FALSE) |>
  mutate(date = as.Date(date)) |>
  rowwise() |>
  mutate(V_or_A = A + G, Other = O) |> # G (VG residual) joins V_or_A so 2026 Maskína matches Gallup canonicalisation (audit C4)
  ungroup() |>
  select(date, fyrirtaeki, n, D, S, B, C, M, F, P, J, V_or_A, Other) |>
  mutate(cycle = "2026")

to_canonical <- function(party_letter) {
  case_when(
    party_letter %in% c("D", "S", "B", "C", "M", "F", "P", "J") ~ party_letter,
    party_letter == "V" ~ "V_or_A",
    TRUE ~ "Other"
  )
}

historical_long <- read_csv(file("historical_polls.csv", encoding = "UTF-8"),
  show_col_types = FALSE
) |>
  filter(!is.na(pct), election_year >= 2018)

historical_polls <- historical_long |>
  mutate(canonical = to_canonical(party_letter)) |>
  group_by(election_year, date, pollster, canonical) |>
  summarise(pct = sum(pct), n_poll = dplyr::first(n), .groups = "drop") |>
  pivot_wider(names_from = canonical, values_from = pct, values_fill = 0) |>
  mutate(
    date = as.Date(date),
    n = coalesce(as.numeric(n_poll), 700),
    fyrirtaeki = pollster,
    cycle = as.character(election_year)
  ) |>
  select(date, fyrirtaeki, n, all_of(parties_canonical$bokstafur), cycle) |>
  filter(rowSums(across(all_of(parties_canonical$bokstafur))) >= 95)

election_results <- read_csv(file("historical_results.csv", encoding = "UTF-8"),
  show_col_types = FALSE
) |>
  mutate(canonical = to_canonical(party_letter)) |>
  group_by(election_date, canonical) |>
  summarise(votes = sum(votes), .groups = "drop") |>
  pivot_wider(names_from = canonical, values_from = votes, values_fill = 0)

elections_wide <- election_results |>
  filter(format(as.Date(election_date), "%Y") %in% c("2018", "2022")) |>
  rowwise() |>
  mutate(
    n = sum(c_across(all_of(parties_canonical$bokstafur))),
    cycle = format(as.Date(election_date), "%Y"),
    date = as.Date(election_date),
    fyrirtaeki = "Kosning"
  ) |>
  ungroup() |>
  select(date, fyrirtaeki, n, all_of(parties_canonical$bokstafur), cycle)

all_obs <- bind_rows(
  elections_wide,
  historical_polls |>
    pivot_longer(all_of(parties_canonical$bokstafur), names_to = "p", values_to = "pct") |>
    mutate(count = round(pct / 100 * n)) |>
    select(-pct) |>
    pivot_wider(names_from = p, values_from = count),
  polls_2026 |>
    pivot_longer(all_of(parties_canonical$bokstafur), names_to = "p", values_to = "pct") |>
    mutate(count = round(pct / 100 * n)) |>
    select(-pct) |>
    pivot_wider(names_from = p, values_from = count)
) |>
  arrange(cycle, date, fyrirtaeki)

cat("Observations going into the model:\n")
print(all_obs |> select(date, fyrirtaeki, n, cycle))

cycle_levels <- c("2018", "2022", "2026")
house_levels <- c("Kosning", sort(setdiff(unique(all_obs$fyrirtaeki), "Kosning")))

obs_dates <- all_obs |> distinct(cycle, date)
forecast_row <- tibble(cycle = "2026", date = FORECAST_DATE_2026)
time_grid <- bind_rows(obs_dates, forecast_row) |>
  distinct(cycle, date) |>
  arrange(cycle, date) |>
  group_by(cycle) |>
  mutate(
    t_within = row_number(),
    time_diff_days = c(0, as.numeric(diff(date)))
  ) |>
  ungroup() |>
  mutate(t_global = row_number())

forecast_time_idx <- time_grid |>
  filter(cycle == "2026", date == FORECAST_DATE_2026) |>
  pull(t_global)

all_obs <- all_obs |>
  left_join(time_grid |> select(cycle, date, t_global), by = c("cycle", "date"))

T_c <- time_grid |>
  count(cycle) |>
  arrange(match(cycle, cycle_levels)) |>
  pull(n)

stan_data <- list(
  P = P_CANON,
  H = length(house_levels),
  C = length(cycle_levels),
  T_total = nrow(time_grid),
  T_c = T_c,
  time_diff_days = time_grid$time_diff_days,
  N = nrow(all_obs),
  y = as.matrix(all_obs[, parties_canonical$bokstafur]),
  house_idx = match(all_obs$fyrirtaeki, house_levels),
  time_idx = all_obs$t_global,
  forecast_time_idx = forecast_time_idx,
  n_pred = 70000
)

cat(sprintf(
  "\nT_total = %d, T_c = (%s), forecast_time_idx = %d, N = %d\n",
  stan_data$T_total, paste(T_c, collapse = ", "),
  forecast_time_idx, stan_data$N
))

model <- cmdstan_model("model.stan")

make_s2z <- function(P, sd = 0.3) {
  v <- rnorm(P, 0, sd)
  v - mean(v)
}

init_fn <- function() {
  list(
    z_beta0 = lapply(seq_along(cycle_levels), function(c) make_s2z(P_CANON, 0.3)),
    z_beta_raw = lapply(seq_len(stan_data$T_total), function(t) make_s2z(P_CANON, 0.3)),
    z_log_sigma = rnorm(P_CANON, 0, 0.2),
    mu_log_sigma = -3.0,
    tau_log_sigma = 0.3,
    gamma_z = lapply(seq_len(length(house_levels) - 1), function(h) make_s2z(P_CANON, 0.3)),
    sigma_gamma = rep(0.05, P_CANON),
    log_phi = rep(4.0, length(house_levels) - 1)
  )
}

fit <- model$sample(
  data = stan_data,
  seed = 16052026,
  chains = 4, parallel_chains = 4,
  iter_warmup = 1500, iter_sampling = 2000,
  adapt_delta = 0.97, max_treedepth = 12,
  refresh = 0, show_messages = FALSE,
  init = init_fn
)

fit$cmdstan_diagnose()

draws <- list(
  pi_election = posterior::as_draws_df(fit$draws("pi_election")),
  pi_trajectory = posterior::as_draws_df(fit$draws("pi_trajectory")),
  y_rep_polls = posterior::as_draws_df(fit$draws("y_rep_polls")),
  gamma = posterior::as_draws_df(fit$draws("gamma")),
  sigma_p = posterior::as_draws_df(fit$draws("sigma_p")),
  mu_log_sigma = posterior::as_draws_df(fit$draws("mu_log_sigma")),
  tau_log_sigma = posterior::as_draws_df(fit$draws("tau_log_sigma")),
  sigma_gamma = posterior::as_draws_df(fit$draws("sigma_gamma")),
  phi = posterior::as_draws_df(fit$draws("phi"))
)

saveRDS(
  list(
    draws = draws,
    diagnostic_summary = fit$diagnostic_summary(),
    stan_data = stan_data,
    parties_canonical = parties_canonical,
    house_levels = house_levels,
    cycle_levels = cycle_levels,
    time_grid = time_grid,
    all_obs = all_obs,
    forecast_date_2026 = FORECAST_DATE_2026
  ),
  "fit_state.rds"
)

cat("\nWrote fit_state.rds\n")
