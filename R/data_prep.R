# Load required libraries
require(tidyverse)
require(lubridate)
require(janitor)
library(here)


# Function to convert string to numeric, replacing comma with dot
convert_to_numeric <- function(x) {
  converted <- suppressWarnings(
    as.numeric(
      stringr::str_replace_all(
        x,
        c("," = "", "\\." = "")
      )
    )
  )
  ifelse(is.na(converted), x, converted)
}

# Function to read and clean the party support data
clean_gallup_party_support <- function(file_path) {
  # import data
  data <- read_csv(
    file_path,
    col_types = cols(.default = col_character()),
    skip = 2,
    col_select = -1
  )
  data <- data[, 1:(ncol(data) - 2)]
  info <- read_csv(file_path, col_select = -1)[1:2, 1:ncol(data)]

  # clean data (column names and variable types)
  name_vec <- c(
    "Sjálf" = "Sjálfstæðisflokkurinn",
    "Frams" = "Framsóknarflokkurinn",
    "Viðr" = "Viðreisn",
    "Flokks" = "Flokkur Fólksins",
    "Sósíalista" = "Sósíalistaflokkurinn",
    "Miðflokk" = "Miðflokkurinn",
    "Pírata" = "Píratar",
    "Samfylk" = "Samfylkingin",
    "Vinstri" = "Vinstri Græn",
    "annarra" = "Annað"
  )
  party_vec <- rep("date", ncol(data))
  type_vec <- rep("", ncol(data))
  for (i in names(name_vec)) {
    idx <- grepl(i, info[1, ])
    party_vec[idx] <- name_vec[i][[1]]
    type_vec[idx] <- unlist(info[2, idx])
  }
  type_vec[grepl("Count", type_vec)] <- "_count"
  type_vec[grepl("Average", type_vec)] <- "_prop"
  colnames(data) <- paste0(party_vec, type_vec)

  # finalize before output
  data |>
    transform(
      date = ymd(paste(date, "-01"))
    ) |>
    filter(date >= "2021-09-01") |>
    mutate(across(
      contains("_count"),
      ~ as.numeric(str_replace(., "\\.", "")) * 10
    )) |>
    mutate(across(
      contains("_prop"),
      ~ as.numeric(str_replace(., ",", "\\.")) / 100
    )) |>
    pivot_longer(
      cols = -date,
      names_to = "party",
      values_to = "support"
    ) |>
    mutate(type = if_else(grepl("count", party), "count", "prop")) |>
    transform(party = str_extract(party, "^[^_]+"))
}


# Read and clean the datasets
gallup_party_support <- clean_gallup_party_support(here("data-raw", "gallup", "fylgi_flokka_eftir_manudum.csv"))


# Save the processed data
write_csv(gallup_party_support, here("data", "gallup_party_support.csv"))
