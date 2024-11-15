library(tidyverse)
library(glue)
library(arrow)
library(here)
box::use(
  R / modeling_utils[fit_model_at_date],
  R / data[read_polling_data]
)

polling_data <- read_polling_data()

fit_dates <- polling_data |>
  arrange(desc(date)) |>
  pull(date) |>
  unique() |>
  head(1)

results <- list()

for (i in seq_along(fit_dates)) {
  fit_date <- fit_dates[i]

  results[[i]] <- fit_model_at_date(
    cutoff_date = fit_date
  ) |>
    mutate(
      fit_date = fit_date
    )

  dir.create(
    here("data", "historical_results", fit_date),
    recursive = TRUE,
    showWarnings = FALSE
  )
  write_csv(
    results[[i]],
    here("data", "historical_results", fit_date, "results.csv")
  )
}


library(ggplot2)
results |>
  bind_rows() |>
  write_csv(here("data/historical_results.csv"))


results |>
  bind_rows() |>
  ggplot(aes(x = fit_date, y = median)) +
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
  geom_segment(
    aes(
      x = fit_date - 0.4,
      xend = fit_date + 0.4,
      y = median,
      yend = median,
    )
  ) +
  facet_wrap(vars(flokkur)) +
  theme_bw()
