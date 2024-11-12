library(tidyverse)
library(glue)
library(arrow)
library(here)
library(clock)
box::use(
  R / modeling_utils[fit_model_at_date_2021],
  R / data[read_polling_data]
)

polling_data <- read_polling_data()

fit_dates <- polling_data |>
  filter(date < date_build(2021, 9, 25)) |>
  arrange(desc(date)) |>
  pull(date) |>
  unique() |>
  head(25)

fit_dates <- fit_dates[-(seq_along(results))]

results <- list()

for (i in seq_along(fit_dates)) {
  fit_date <- fit_dates[i]

  results[[i]] <- fit_model_at_date_2021(
    cutoff_date = fit_date
  ) |>
    mutate(
      fit_date = fit_date
    )
}

library(ggplot2)
results |>
  bind_rows() |>
  inner_join(
    polling_data |>
      filter(
        fyrirtaeki == "Kosning",
        year(date) == 2021
      ) |>
      mutate(
        p = n / sum(n)
      ),
    by = "flokkur"
  ) |>
  mutate(
    lower = lower - p,
    upper = upper - p
  ) |>
  ggplot(aes(x = fit_date, y = median)) +
  geom_hline(
    yintercept = 0,
    linewidth = 2
  ) +
  geom_segment(
    aes(
      xend = fit_date,
      y = lower,
      yend = upper,
      alpha = -coverage,
      group = coverage
    ),
    linewidth = 3
  ) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  facet_wrap(vars(flokkur)) +
  theme_bw()
