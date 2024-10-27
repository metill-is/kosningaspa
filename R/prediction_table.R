library(tidyverse)
library(gt)
library(gtExtras)
library(here)

colors <- tribble(
  ~flokkur, ~litur,
  "Sjálfstæðisflokkurinn", "#377eb8",
  "Framsóknarflokkurinn", "#41ab5d",
  "Samfylkingin", "#e41a1c",
  "Vinstri Græn", "#00441b",
  "Viðreisn", "#ff7d14",
  "Píratar", "#984ea3",
  "Miðflokkurinn", "#08306b",
  "Flokkur Fólksins", "#FBB829",
  "Sósíalistaflokkurinn", "#67000d",
  "Annað", "grey50"
)

seats_draws <- read_parquet(here("data", "seats_draws.parquet"))
y_rep <- read_parquet(here("data", "y_rep_draws.parquet"))


y_rep |>
  filter(
    dags == max(dags)
  ) |>
  summarise(
    median_votes = median(value),
    lower_votes = quantile(value, 0.05),
    upper_votes = quantile(value, 0.95),
    .by = c(flokkur)
  ) |>
  inner_join(
    seats_draws |>
      summarise(
        seats = sum(seats),
        .by = c(flokkur, .draw)
      ) |>
      summarise(
        median_seats = median(seats),
        lower_seats = quantile(seats, 0.05),
        upper_seats = quantile(seats, 0.95),
        .by = flokkur
      )
  ) |>
  select(flokkur, median_votes, lower_votes, upper_votes, median_seats, lower_seats, upper_seats) |>
  arrange(desc(median_votes)) |>
  gt() |>
  cols_label(
    flokkur = "Flokkur",
    starts_with("median") ~ "Miðgildi",
    starts_with("lower") ~ "95% Vikmörk",
    starts_with("upper") ~ "Efri"
  ) |>
  tab_spanner(
    label = "Atkvæði",
    columns = median_votes:upper_votes
  ) |>
  tab_spanner(
    label = "Þingsæti",
    columns = median_seats:upper_seats
  ) |>
  fmt_percent(
    columns = median_votes:upper_votes,
    decimals = 0
  ) |>
  fmt_number(
    columns = median_seats:upper_seats,
    decimals = 0
  ) |>
  cols_merge(
    columns = c(lower_votes, upper_votes),
    pattern = "{1} - {2}"
  ) |>
  cols_merge(
    columns = c(lower_seats, upper_seats),
    pattern = "{1} - {2}"
  ) |>
  cols_align(
    columns = -flokkur,
    align = "center"
  )
