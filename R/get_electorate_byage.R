library(tidyverse)
library(hagstofa)

url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/kosningar/althingi/althkjosendur/KOS02101a.px"

d <- hg_data(url) |>
  collect() |>
  janitor::clean_names() |>
  rename(n = 6) |>
  filter(
    eining == "Fjöldi",
    kjordaemi != "Allt landið",
    aldur != "Alls",
    kyn == "Alls",
    ar == "2021"
  ) |>
  janitor::remove_constant() |>
  arrange(kjordaemi) |>
  filter(
    aldur %in% c(
      "18-29",
      "30-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75 - 79",
      "80+"
    )
  ) |>
  mutate(
    aldur = case_when(
      aldur == "18-29" ~ aldur,
      aldur == "30-39" ~ aldur,
      aldur == "40-44" ~ "40-49",
      aldur == "45-49" ~ "40-49",
      aldur == "50-54" ~ "50-59",
      aldur == "55-59" ~ "50-59",
      TRUE ~ "60+"
    ),
    kjordaemi = str_replace(kjordaemi, "kjördæmi", "") |>
      str_replace("Reykjavíkur", "Reykjavík") |>
      str_to_title()
  ) |>
  count(aldur, kjordaemi, name = "n_kjosendur", wt = n)

d |>
  write_csv("data/electorate_byagearea.csv")
