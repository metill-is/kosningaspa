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
  R / prepare_data[
    read_polling_data,
    prepare_stan_data,
    read_fundamentals_data
  ]
)

election_date <- date_build(2024, 11, 30)

polling_data <- read_polling_data() |>
  filter(
    date >= date_build(2016, 1, 1)
  )

unique(polling_data$flokkur)
max(polling_data$date)

fundamentals_data <- read_fundamentals_data()

econ_data <- read_csv(here("data", "economy_data.csv"))

fundamentals_data <- fundamentals_data |>
  inner_join(
    econ_data,
    by = "date"
  )

n_votes <- fundamentals_data |>
  summarise(
    n_votes = unique(total_valid_votes) |>
      coalesce(200000),
    .by = year
  ) |>
  pull(n_votes)

stan_data <- prepare_stan_data(polling_data, fundamentals_data)
stan_data$n_votes <- n_votes[-(1:2)]
str(stan_data)

model <- cmdstan_model(
  here("Stan", "fundamentals.stan")
)


fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  init = 0
)



# Fundamentals Parameters
fit$summary("alpha_f")
fit$summary("beta_lag_f")


fit$summary("beta_inc_years_f")

fit$summary(c("beta_vnv_f", "beta_growth_f"))

fit$draws(c("beta_vnv_f", "beta_growth_f")) |>
  summarise_draws(
    "perc_less_than_0" = ~ mean(.x < 0)
  )

fit$summary("tau_f")

fit$summary("phi_inv") |>
  mutate(
    house = levels(polling_data$fyrirtaeki)
  ) |>
  select(
    house, mean, q5, q95
  )

fit$summary("phi_f_inv")


fit$summary(c("beta_lag_f", "beta_inc_years_f", "beta_vnv_f", "beta_growth_f"))

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
    value = value - voteshare / 100
  )




plot_dat |>
  summarise(
    mean = mean(value),
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .by = c(year, flokkur, voteshare)
  ) |>
  select(year, flokkur, mean, voteshare, q5, q95) |>
  ggplot(aes(year, mean)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_linerange(aes(ymin = q5, ymax = q95), alpha = 0.5) +
  geom_point() +
  facet_wrap(
    ~flokkur,
    scales = "free_y",
    ncol = 3
  ) +
  coord_cartesian(ylim = c(-0.25, 0.25))

plot_dat |>
  summarise(
    mean = mean(value),
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .by = year
  ) |>
  select(year, mean, q5, q95) |>
  ggplot(aes(year, mean)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_linerange(aes(ymin = q5, ymax = q95), alpha = 0.5) +
  geom_point()
