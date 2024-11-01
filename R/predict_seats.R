library(tidyverse)
library(arrow)
library(here)
library(googlesheets4)
library(purrr)

box::use(
  R / election_utils[dhondt, jofnunarsaeti]
)

seats_tibble <- tribble(
  ~kjordaemi, ~n_seats, ~n_jofnun,
  "Reykjavík Suður", 9, 2,
  "Reykjavík Norður", 9, 2,
  "Suðvestur", 11, 2,
  "Suður", 9, 1,
  "Norðaustur", 9, 1,
  "Norðvestur", 7, 1
)

electorate_byagearea <- read_csv(here("data", "electorate_byagearea.csv"))

maskina_aldur <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4",
  sheet = "maskina_aldur"
) |>
  select(-Ár, -Mánuður) |>
  pivot_longer(
    cols = -Flokkur,
    names_to = "aldur",
    values_to = "p_aldur"
  ) |>
  janitor::clean_names() |>
  mutate(
    p_aldur = p_aldur / sum(p_aldur),
    .by = flokkur
  )

maskina_kjordaemi <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4",
  sheet = "maskina_kjordaemi"
) |>
  select(-Ár, -Mánuður) |>
  pivot_longer(
    cols = -Flokkur,
    names_to = "kjordaemi",
    values_to = "p_kjordaemi"
  ) |>
  janitor::clean_names() |>
  mutate(
    p_kjordaemi = p_kjordaemi / sum(p_kjordaemi),
    .by = flokkur
  )

draws <- read_parquet(here("data", "2024-10-31", "y_rep_draws.parquet")) |>
  filter(
    dags == max(dags)
  ) |>
  select(.draw, flokkur, value) |>
  inner_join(
    maskina_kjordaemi,
    by = join_by(flokkur)
  ) |>
  inner_join(
    maskina_aldur,
    by = join_by(flokkur)
  ) |>
  inner_join(
    electorate_byagearea,
    by = join_by(kjordaemi, aldur)
  ) |>
  mutate(
    value = value * p_aldur * p_kjordaemi * n_kjosendur
  ) |>
  summarise(
    votes = sum(value),
    .by = c(.draw, flokkur, kjordaemi)
  ) |>
  arrange(kjordaemi, flokkur)



d <- draws |>
  group_by(.draw) |>
  group_modify(~ calculate_seats(.x)) |>
  ungroup()

d |>
  write_parquet(here("data", "seats_draws.parquet"))
