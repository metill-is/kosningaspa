library(tidyverse)
library(arrow)
library(here)
library(clock)

box::use(
  R / party_utils[party_tibble],
  R / prepare_data[read_polling_data]
)

polling_data <- read_polling_data()

election_date <- date_build(2024, 10, 31)

dates <- c(
  unique(polling_data$date),
  seq.Date(max(polling_data$date), election_date, by = "day")
)

d <- here("results", "fit_historical") |>
  open_dataset() |>
  to_duckdb()


d |>
  filter(
    str_detect(variable, "sigma")
  )



curve(dgamma(x, 10, 0.8), from = 0, to = 30)

d |>
  filter(variable == "beta_stjornarslit") |>
  collect() |>
  mutate(fit_date = ymd(fit_date)) |>
  ggplot(aes(fit_date, mean)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.2) +
  geom_line()

d |>
  filter(
    str_detect(variable, "y_rep")
  ) |>
  collect() |>
  mutate(
    fit_date = ymd(fit_date),
    t = str_match(variable, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = levels(polling_data$flokkur)[p],
    dags = dates[t]
  ) |>
  filter(
    t == max(t),
    .by = fit_date
  ) |>
  select(flokkur, dags, fit_date, mean, q5, q95) |>
  filter(
    flokkur != "Annað"
  ) |>
  mutate_at(
    vars(mean, q5, q95),
    ~ . / 196302
  ) |>
  ggplot(aes(fit_date, mean, color = flokkur)) +
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = flokkur), alpha = 0.2) +
  geom_line() +
  geom_point(
    data = polling_data |>
      mutate(
        flokkur = as.character(flokkur)
      ) |>
      filter(
        date >= date_build(2024, 1, 1),
        flokkur != "Annað"
      ) |>
      mutate(
        value = n / sum(n),
        .by = date
      ),
    aes(date, value, color = flokkur),
    alpha = 0.5
  ) +
  facet_wrap(~flokkur)
