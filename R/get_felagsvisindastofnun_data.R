library(tidyverse)
library(googlesheets4)
library(here)

# read data
gs4_auth()
felagsvisindastofnun_data <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4/edit?gid=0#gid=0",
  sheet = "felagsvisindastofnun"
)

out <- felagsvisindastofnun_data |>
  janitor::clean_names() |>
  mutate(
    total_hlutfall = sum(hlutfall),
    total_not_annad = sum(hlutfall * (flokkur != "Annað")),
    hlutfall = if_else(
      flokkur == "Annað",
      1 - total_not_annad,
      hlutfall
    ),
    .by = c(ar, manudur, dagur)
  ) |>
  mutate(
    dagur = coalesce(dagur, 15),
    date = clock::date_build(ar, manudur, dagur),
    n = hlutfall * fjoldi_alls
  ) |>
  select(
    date,
    fyrirtaeki,
    flokkur,
    n
  )

out |>
  write_csv(here("data", "felagsvisindastofnun_data.csv"))
