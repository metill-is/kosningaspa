library(tidyverse)
library(here)

out <- read_csv(
  here("data", "gallup_party_support.csv")
) |>
  pivot_wider(names_from = type, values_from = support) |>
  mutate(
    n = round(count * prop),
    fyrirtaeki = "Gallup"
  ) |>
  select(
    date,
    fyrirtaeki,
    flokkur = party,
    n
  )
out |>
  write_csv(here("data", "gallup_data.csv"))
