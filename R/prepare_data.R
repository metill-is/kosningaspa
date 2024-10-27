#' @export
combine_datasets <- function() {
  box::use(
    here[here],
    readr[read_csv],
    dplyr[
      bind_rows,
      mutate,
      filter,
      arrange,
      if_else
    ],
    forcats[fct_relevel, as_factor],
    clock[date_build]
  )
  gallup_data <- read_csv(here("data", "gallup_data.csv"))
  maskina_data <- read_csv(here("data", "maskina_data.csv"))
  prosent_data <- read_csv(here("data", "prosent_data.csv"))
  election_data <- read_csv(here("data", "election_data.csv"))
  # combine data
  data <- bind_rows(
    maskina_data,
    prosent_data,
    gallup_data,
    election_data
  ) |>
    mutate(
      flokkur = if_else(flokkur == "Lýðræðisflokkurinn", "Annað", flokkur),
      fyrirtaeki = fct_relevel(
        as_factor(fyrirtaeki),
        "Kosning"
      )
    ) |>
    filter(
      date >= date_build(2021, 1, 1),
      flokkur != "Annað"
    ) |>
    arrange(date, fyrirtaeki, flokkur)

  data
}

#' @export
prepare_stan_data <- function(data) {
  box::use(
    dplyr[
      distinct,
      select,
      mutate,
      pull,
      arrange,
      filter
    ],
    tidyr[pivot_wider, drop_na],
    clock[date_build]
  )
  D <- length(unique(data$date))
  P <- length(unique(data$flokkur))
  H <- length(unique(data$fyrirtaeki))
  N <- data |>
    distinct(fyrirtaeki, date) |>
    nrow()


  y <- data |>
    select(date, fyrirtaeki, flokkur, n) |>
    mutate(
      n = as.integer(n)
    ) |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    select(-date, -fyrirtaeki) |>
    as.matrix()

  house <- data |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    mutate(
      house = as.numeric(factor(fyrirtaeki))
    ) |>
    pull(house)

  date <- data |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    mutate(
      date = as.numeric(factor(date))
    ) |>
    pull(date)

  time_diff <- data |>
    distinct(date) |>
    arrange(date) |>
    mutate(
      time_diff = c(NA, diff(date))
    ) |>
    drop_na() |>
    pull(time_diff) |>
    as.numeric()

  max_date <- max(data$date)
  election_date <- clock::date_build(2024, 11, 30)
  pred_y_time_diff <- as.numeric(election_date - max_date)
  stjornarslit <- data |>
    distinct(date) |>
    mutate(
      stjornarslit = 1 * (date >= clock::date_build(2024, 10, 14))
    ) |>
    pull(stjornarslit)

  n_election <- data |>
    filter(
      fyrirtaeki == "Kosning",
      date == date_build(2021, 09, 25)
    ) |>
    pull(n) |>
    sum()

  stan_data <- list(
    D = D,
    P = P,
    H = H,
    N = N,
    y = y,
    house = house,
    date = date,
    time_diff = time_diff,
    pred_y_time_diff = pred_y_time_diff,
    stjornarslit = stjornarslit,
    n_pred = as.integer(n_election)
  )

  stan_data
}
