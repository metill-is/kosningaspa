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
    flokkur = fct_relevel(
      as_factor(flokkur),
      "Annað",
      "Sjálfstæðisflokkurinn",
      "Framsóknarflokkurinn",
      "Samfylkingin",
      "Vinstri Græn",
      "Píratar",
      "Viðreisn",
      "Flokkur Fólksins",
      "Miðflokkurinn",
      "Sósíalistaflokkurinn"
    )
  ) |>
  arrange(date, fyrirtaeki, flokkur)

unique(polling_data$flokkur)
cat("Date range:", as.character(min(polling_data$date)),
    "to", as.character(max(polling_data$date)), "\n")
cat("Polls:", polling_data |> distinct(date, fyrirtaeki) |> nrow(), "\n")

prepared <- prepare_polling_watch_data(polling_data)
stan_data <- prepared$stan_data
date_mapping <- prepared$date_mapping
party_names <- prepared$party_names

str(stan_data)

model <- cmdstan_model(
  here("Stan", "polling_watch.stan")
)

fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  init = 0,
  iter_warmup = 500,
  iter_sampling = 500
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
