library(tidyverse)
library(here)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(gt)
library(gtExtras)
library(arrow)
library(scales)
library(clock)
library(metill)
theme_set(theme_metill())

box::use(
  R / data[
    read_fundamentals_data
  ],
  R / stan_data[
    prepare_fundamentals_data
  ]
)

election_date <- date_build(2024, 11, 30)

# Read fundamentals data
fundamentals_data <- read_fundamentals_data()

# Read and join economic data
econ_data <- read_csv(here("data", "economy_data.csv"))

fundamentals_data <- fundamentals_data |>
  inner_join(
    econ_data,
    by = "date"
  )

# Prepare Stan data using only the fundamentals component
stan_data <- prepare_fundamentals_data(fundamentals_data)
stan_data$n_votes <- rep(1e4, ncol(stan_data$y_f) + 1)

# Initialize model
model <- cmdstan_model(
  here("Stan", "fundamentals.stan")
)

# Fit model
fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  init = 0,
  iter_warmup = 1000,
  iter_sampling = 1000
)

# Examine fundamentals parameters
fit$summary("alpha_f")
fit$summary("beta_lag_f")
fit$summary("beta_inc_years_f")
fit$summary(c("beta_vnv_f", "beta_growth_f"))

# Calculate probability of negative effects
fit$draws(c("beta_vnv_f", "beta_growth_f")) |>
  summarise_draws(
    "perc_less_than_0" = ~ mean(.x < 0)
  )

fit$summary("tau_f")
fit$summary("phi_f_inv")

# Get combined parameter summary
fit$summary(c("beta_lag_f", "beta_inc_years_f", "beta_vnv_f", "beta_growth_f"))

# Create plot data
plot_dat <- fit$draws("y_rep") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.draw, -.chain, -.iteration)) |>
  mutate(
    p = str_match(name, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    t = str_match(name, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = rownames(stan_data$y_f)[p],
    year = colnames(stan_data$y_f)[t] |> as.numeric()
  ) |>
  mutate(
    value = value / sum(value),
    .by = c(year, .draw, .iteration, .chain)
  ) |>
  inner_join(
    fundamentals_data,
    by = c("year", "flokkur")
  ) |>
  mutate(
    error = value - voteshare / 100
  )

# Create visualization
plot_dat |>
  summarise(
    mean = mean(error),
    q5 = quantile(error, 0.05),
    q95 = quantile(error, 0.95),
    .by = c(year, flokkur, voteshare)
  ) |>
  select(year, flokkur, mean, voteshare, q5, q95) |>
  ggplot(aes(year, mean)) +
  geom_hline(yintercept = 0, lty = 2) +
  # geom_linerange(aes(ymin = q5, ymax = q95), alpha = 0.5) +
  geom_point() +
  facet_wrap(
    ~flokkur,
    scales = "free_y",
    ncol = 3
  ) +
  coord_cartesian(ylim = c(-0.15, 0.15))


plot_dat |>
  summarise(
    median = median(value),
    error = median(error),
    .by = c(year, flokkur, voteshare)
  ) |>
  select(year, flokkur, median, voteshare, error) |>
  filter(year == 2021) |>
  filter(flokkur != "Annað") |>
  mutate(
    abs_error = abs(error)
  ) |>
  arrange(desc(abs_error))
