# WHY: under a C locale (headless Rscript) string matching against the Icelandic
# party names fails, so the fct_relevel() below silently no-ops. Pin a UTF-8 locale
# so the size-descending party order takes effect. polling_watch_v4 is
# reference-invariant, so party order does NOT change results -- but ordering parties
# largest->smallest (the tiny "Annað" residual LAST) keeps the sum-to-zero geometry
# well-conditioned: 0% max-treedepth, vs ~60-100% when the boundary party sits early.
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(here)
library(cmdstanr)
library(posterior)
library(arrow)
library(clock)

box::use(
  R / data[read_polling_data],
  R / stan_data[prepare_polling_watch_data]
)

# Read pre-election data (includes 2021 election + polls up to Nov 2024)
pre_election <- read_polling_data() |>
  filter(
    date >= date_build(2021, 9, 25)
  ) |>
  select(-lokadagur, -p)

# Read post-election data (2024 election + polls from Dec 2024 onward)
post_election_path <- here("data", "post_election_polls.csv")
if (file.exists(post_election_path)) {
  post_election <- read_csv(post_election_path, show_col_types = FALSE) |>
    mutate(
      fyrirtaeki = factor(fyrirtaeki),
      flokkur = factor(flokkur)
    ) |>
    select(-lokadagur, -p)
} else {
  post_election <- tibble()
}

# Combine and harmonize factor levels
polling_data <- bind_rows(pre_election, post_election) |>
  mutate(
    fyrirtaeki = fct_relevel(
      as_factor(fyrirtaeki),
      "Kosning"
    ),
    # Size-descending order (largest first, tiny "Annað" residual last) for sampling
    # efficiency. Reference-invariant, so this does not affect results.
    flokkur = fct_relevel(
      as_factor(flokkur),
      "Samfylkingin",
      "Sjálfstæðisflokkurinn",
      "Miðflokkurinn",
      "Viðreisn",
      "Framsóknarflokkurinn",
      "Flokkur Fólksins",
      "Vinstri Græn",
      "Sósíalistaflokkurinn",
      "Píratar",
      "Annað"
    )
  ) |>
  arrange(date, fyrirtaeki, flokkur)

# Guard: size-descending order must have taken (largest first, "Annað" last) so the
# sum-to-zero geometry stays well-conditioned. If this fails the locale fix above did
# not take; the order is invariant for results but matters for sampling speed.
stopifnot(
  "party order not size-descending — fct_relevel no-opped (check LC_ALL locale)" =
    levels(polling_data$flokkur)[1] == "Samfylkingin" &&
      tail(levels(polling_data$flokkur), 1) == "Annað"
)

unique(polling_data$flokkur)
cat(
  "Date range:", as.character(min(polling_data$date)),
  "to", as.character(max(polling_data$date)), "\n"
)
cat("Polls:", polling_data |> distinct(date, fyrirtaeki) |> nrow(), "\n")

prepared <- prepare_polling_watch_data(polling_data)
stan_data <- prepared$stan_data
date_mapping <- prepared$date_mapping
party_names <- prepared$party_names
house_names <- prepared$house_names

str(stan_data)

model <- cmdstan_model(
  here("Stan", "polling_watch_v4.stan")
)

fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  init = 0,
  iter_warmup = 500,
  iter_sampling = 1000
)

fit$summary("sigma")
fit$summary("phi")
fit$summary("gamma")
fit$summary("Omega")

# Extract pi_smooth draws → long tibble
pi_draws <- fit$draws("pi_smooth") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw),
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    d = str_match(variable, "pi_smooth\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "pi_smooth\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = party_names[p]
  ) |>
  inner_join(
    date_mapping,
    by = c("d" = "index")
  ) |>
  rename(dags = date) |>
  select(.chain, .iteration, .draw, dags, flokkur, value)

# Create output directory and save
output_dir <- here("data", as.character(today()))
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_parquet(pi_draws, here(output_dir, "polling_watch_draws.parquet"))

# Persist the full fit so the entire posterior can be re-queried (any parameter,
# any draw) without re-fitting: readRDS(...)$draws("gamma"), etc.
fit$save_object(here(output_dir, "polling_watch_fit.rds"))

# Party-space innovation correlation (Omega) draws, labelled by party, for the
# correlation/precision analysis. polling_watch_v4 gives a reference-invariant P x P Omega.
omega_draws <- fit$draws("Omega") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.iteration, -.draw), names_to = "variable", values_to = "value") |>
  mutate(
    i = str_match(variable, "Omega\\[(.*),.*\\]")[, 2] |> parse_number(),
    j = str_match(variable, "Omega\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur_i = party_names[i],
    flokkur_j = party_names[j]
  ) |>
  select(.chain, .iteration, .draw, flokkur_i, flokkur_j, value)
write_parquet(omega_draws, here(output_dir, "polling_watch_omega.parquet"))

# House effects (gamma) draws, labelled by firm + party. gamma[1] is the election
# anchor (pinned to 0), so keep only the polling houses (h > 1). On the softmax
# log-odds scale: positive => the house systematically over-states that party
# relative to the election-anchored latent trend. Zero-sum across parties per house.
gamma_draws <- fit$draws("gamma") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.iteration, -.draw), names_to = "variable", values_to = "value") |>
  mutate(
    h = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
    fyrirtaeki = house_names[h],
    flokkur = party_names[p]
  ) |>
  filter(h > 1) |>
  select(.chain, .iteration, .draw, fyrirtaeki, flokkur, value)
write_parquet(gamma_draws, here(output_dir, "polling_watch_gamma.parquet"))

# Shared industry bias (mu_gamma): the lean common to every polling house, per party.
mu_gamma_draws <- fit$draws("mu_gamma") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.iteration, -.draw), names_to = "variable", values_to = "value") |>
  mutate(
    p = str_match(variable, "mu_gamma\\[(.*)\\]")[, 2] |> parse_number(),
    flokkur = party_names[p]
  ) |>
  select(.chain, .iteration, .draw, flokkur, value)
write_parquet(mu_gamma_draws, here(output_dir, "polling_watch_mu_gamma.parquet"))

# Quick summary check
pi_draws |>
  filter(dags == max(dags)) |>
  summarise(
    median = median(value),
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .by = flokkur
  ) |>
  arrange(desc(median))
