#' @export
prepare_stan_data <- function(
    polling_data,
    fundamentals_data,
    constituency_data,
    election_date = date_build(2024, 11, 30)) {
  box::use(
    dplyr[
      distinct,
      select,
      mutate,
      pull,
      arrange,
      filter,
      lag,
      summarise,
      rename,
      row_number,
      if_else,
      anti_join
    ],
    tibble[column_to_rownames],
    tidyr[pivot_wider, drop_na, complete, fill],
    clock[date_build],
    forcats[fct_relevel, as_factor]
  )


  #### Polling data ####


  polling_data <- prepare_polling_data(polling_data, constituency_data, election_date)


  #### Fundamentals data ####

  fundamentals_data <- prepare_fundamentals_data(fundamentals_data)

  stan_data <- list()

  for (i in seq_along(polling_data)) {
    stan_data[[names(polling_data)[i]]] <- polling_data[[i]]
  }

  for (i in seq_along(fundamentals_data)) {
    stan_data[[names(fundamentals_data)[i]]] <- fundamentals_data[[i]]
  }

  stan_data
}


#' @export
prepare_polling_data <- function(polling_data, constituency_data, election_date) {
  box::use(
    dplyr[
      distinct,
      select,
      mutate,
      pull,
      arrange,
      filter,
      lag,
      summarise,
      rename,
      row_number,
      if_else,
      anti_join,
      left_join,
      case_when,
      count
    ],
    tibble[column_to_rownames],
    tidyr[pivot_wider, drop_na, complete, fill],
    clock[date_build],
    forcats[fct_relevel, as_factor]
  )

  election_date <- date_build(2024, 11, 30)

  election_dates <- date_build(
    c(2016, 2017, 2021, 2024),
    c(10, 10, 09, 11),
    c(29, 28, 25, 30)
  )

  dates <- unique(c(polling_data$date, constituency_data$date))

  time_before_election <- case_when(
    dates <= election_dates[1] ~ election_dates[1] - dates,
    dates <= election_dates[2] ~ election_dates[2] - dates,
    dates <= election_dates[3] ~ election_dates[3] - dates,
    dates <= election_dates[4] ~ election_dates[4] - dates
  )

  month_before_election <- 1 * (time_before_election <= 47)




  stjornarslit_dags <- date_build(2024, 10, 14)
  days_between_stjornarslit_and_election <- as.numeric(election_date - stjornarslit_dags)


  date_factor <- factor(dates)
  stjornarslit <- cumsum(dates > stjornarslit_dags)
  stjornarslit <- 1 * (stjornarslit == 1)
  post_stjornarslit <- 1 * (dates > stjornarslit_dags)

  time_diff <- c(NA, diff(dates))[-1]

  max_date <- max(dates)

  pred_y_time_diff <- as.numeric(election_date - max_date)

  #### National level ####
  D <- length(date_factor)
  P <- length(unique(polling_data$flokkur))
  H <- length(unique(polling_data$fyrirtaeki))
  N <- polling_data |>
    distinct(fyrirtaeki, date) |>
    nrow()

  y_n <- polling_data |>
    select(date, fyrirtaeki, flokkur, n) |>
    # anti_join(
    #  constituency_data,
    #  by = c("date", "fyrirtaeki")
    # ) |>
    mutate(
      n = as.integer(n)
    ) |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    select(-date, -fyrirtaeki) |>
    as.matrix()

  house_n <- polling_data |>
    # anti_join(
    #   constituency_data,
    #   by = c("date", "fyrirtaeki")
    # ) |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    mutate(
      house = as.numeric(fyrirtaeki)
    ) |>
    pull(house)

  date_n <- polling_data |>
    # anti_join(
    #   constituency_data,
    #   by = c("date", "fyrirtaeki")
    # ) |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    mutate(
      date = as.numeric(factor(date, levels = levels(date_factor)))
    ) |>
    pull(date)


  n_election <- polling_data |>
    filter(
      fyrirtaeki == "Kosning"
    ) |>
    filter(
      date == max(date)
    ) |>
    pull(n) |>
    sum()

  n_parties <- polling_data |>
    # anti_join(
    #   constituency_data,
    #   by = c("date", "fyrirtaeki")
    # ) |>
    summarise(
      n_parties = sum(n != 0),
      .by = c(date, fyrirtaeki)
    ) |>
    arrange(date) |>
    pull(n_parties)

  n_parties_n_rep <- polling_data |>
    filter(n > 0) |>
    summarise(
      date = min(date),
      .by = flokkur
    ) |>
    count(date) |>
    mutate(
      n = cumsum(n)
    )

  n_parties_n_rep <- polling_data |>
    # anti_join(
    #   constituency_data,
    #   by = c("date", "fyrirtaeki")
    # ) |>
    summarise(
      n_parties = sum(n != 0),
      .by = c(date, fyrirtaeki)
    ) |>
    arrange(date) |>
    left_join(n_parties_n_rep, by = "date") |>
    fill(n, .direction = "down") |>
    pull(n)





  #### Kjördæmi level ####

  K <- length(levels(constituency_data$kjordaemi))
  N_k <- constituency_data |>
    distinct(date, kjordaemi, fyrirtaeki) |>
    nrow()

  y_k <- constituency_data |>
    select(date, fyrirtaeki, kjordaemi, flokkur, n) |>
    mutate(
      n = as.integer(n)
    ) |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    select(-date, -fyrirtaeki, -kjordaemi) |>
    as.matrix()

  house_k <- constituency_data |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    mutate(
      house = as.numeric(fyrirtaeki)
    ) |>
    pull(house)

  date_k <- constituency_data |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    mutate(
      date = as.numeric(factor(date, levels = levels(date_factor)))
    ) |>
    pull(date)

  n_parties_k <- constituency_data |>
    summarise(
      n_parties = sum(n != 0),
      .by = c(date, kjordaemi, fyrirtaeki)
    ) |>
    arrange(date) |>
    pull(n_parties)


  n_parties_k_rep <- constituency_data |>
    filter(n > 0) |>
    summarise(
      date = min(date),
      .by = flokkur
    ) |>
    count(date) |>
    mutate(
      n = cumsum(n)
    )


  n_parties_k_rep <- constituency_data |>
    summarise(
      n_parties = sum(n != 0),
      .by = c(date, kjordaemi, fyrirtaeki)
    ) |>
    arrange(date) |>
    left_join(n_parties_k_rep, by = "date") |>
    fill(n, .direction = "down") |>
    pull(n)

  constituency_k <- constituency_data |>
    pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
    mutate(
      constituency = as.numeric(factor(kjordaemi))
    ) |>
    pull(constituency)

  n_pred_k <- c(35118, 35504, 58608, 17251, 23536, 29713)

  list(
    # National level
    D = D,
    P = P,
    H = H,
    N = N,
    y_n = y_n,
    N_n = nrow(y_n),
    days_between_stjornarslit_and_election = days_between_stjornarslit_and_election,
    house_n = house_n,
    date_n = date_n,
    n_parties_n = n_parties,
    n_parties_n_rep = n_parties_n_rep,
    time_diff = time_diff,
    month_before_election = month_before_election,
    pred_y_time_diff = pred_y_time_diff,
    stjornarslit = stjornarslit,
    post_stjornarslit = post_stjornarslit,
    n_election = n_election,
    n_pred = as.integer(n_election),
    # Kjördæmi level
    K = K,
    N_k = N_k,
    y_k = y_k,
    house_k = house_k,
    date_k = date_k,
    n_parties_k = n_parties_k,
    n_parties_k_rep = n_parties_k_rep,
    constituency_k = constituency_k,
    n_pred_k = n_pred_k
  )
}


#' @export
prepare_fundamentals_data <- function(fundamentals_data) {
  box::use(
    dplyr[
      distinct,
      select,
      mutate,
      pull,
      arrange,
      filter,
      lag,
      summarise,
      rename,
      row_number,
      if_else
    ],
    tibble[column_to_rownames],
    tidyr[pivot_wider, drop_na, complete],
    clock[date_build],
    forcats[fct_relevel, as_factor]
  )

  logit_votes_f <- fundamentals_data |>
    drop_na(voteshare_prior) |>
    arrange(year) |>
    mutate(
      p = voteshare / sum(voteshare),
      .by = year
    ) |>
    filter(
      (p > 0)
    ) |>
    mutate(
      logit_p = log(p) - log(1 - p),
      logit_p = logit_p - mean(logit_p, na.rm = TRUE),
      .by = year
    ) |>
    select(year, flokkur, logit_p) |>
    arrange(flokkur, year) |>
    pivot_wider(names_from = year, values_from = logit_p, values_fill = 0) |>
    column_to_rownames("flokkur") |>
    as.matrix()


  votes_f <- fundamentals_data |>
    drop_na(voteshare_prior) |>
    arrange(date) |>
    mutate(
      p = voteshare / sum(voteshare),
      .by = date
    ) |>
    mutate(
      votes = round(p * total_valid_votes)
    ) |>
    filter(
      (p > 0)
    ) |>
    select(year, flokkur, votes) |>
    arrange(flokkur, year) |>
    pivot_wider(names_from = year, values_from = votes, values_fill = 0) |>
    column_to_rownames("flokkur") |>
    as.matrix()


  y_f <- votes_f[, -1]
  x_f <- logit_votes_f

  incumbent_f <- fundamentals_data |>
    drop_na(voteshare_prior) |>
    select(year, flokkur, incumbent) |>
    arrange(flokkur, year) |>
    pivot_wider(names_from = year, values_from = incumbent, values_fill = 0) |>
    column_to_rownames("flokkur") |>
    as.matrix()

  incumbent_f <- incumbent_f[, -1]

  incumbent_years <- fundamentals_data |>
    drop_na(voteshare_prior) |>
    select(year, flokkur, incumbent, incumbent_years) |>
    arrange(flokkur, year) |>
    mutate(
      incumbent_years = if_else(incumbent_years > 0, log(incumbent_years), 0),
      .by = year
    ) |>
    select(year, flokkur, incumbent_years) |>
    pivot_wider(
      names_from = year,
      values_from = incumbent_years,
      values_fill = 0
    ) |>
    column_to_rownames("flokkur") |>
    as.matrix()

  incumbent_years <- incumbent_years[, -1]

  vnv <- fundamentals_data |>
    drop_na(voteshare_prior) |>
    mutate(
      vnv = log(1 + vnv) * incumbent,
      .by = year
    ) |>
    select(year, flokkur, vnv) |>
    arrange(flokkur, year) |>
    pivot_wider(names_from = year, values_from = vnv, values_fill = 0) |>
    column_to_rownames("flokkur") |>
    as.matrix()

  vnv <- vnv[, -1]

  growth <- fundamentals_data |>
    drop_na(voteshare_prior) |>
    arrange(flokkur, year) |>
    mutate(
      growth = log(1 + growth / 100) * incumbent,
      .by = year
    ) |>
    select(year, flokkur, growth) |>
    pivot_wider(names_from = year, values_from = growth, values_fill = 0) |>
    column_to_rownames("flokkur") |>
    as.matrix()

  growth <- growth[, -1]

  n_parties_fundamentals <- fundamentals_data |>
    drop_na(voteshare_prior) |>
    arrange(flokkur, year) |>
    rename(p = voteshare) |>
    mutate(
      in_election = 1 * (p > 0)
    ) |>
    filter(in_election > 0) |>
    summarise(
      n_parties = sum(in_election),
      .by = year
    ) |>
    pull(n_parties)

  index_f <- fundamentals_data |>
    drop_na(voteshare_prior) |>
    arrange(flokkur, year) |>
    rename(p = voteshare) |>
    mutate(
      in_election = 1 * (p > 0)
    ) |>
    select(flokkur, year, in_election) |>
    complete(flokkur, year, fill = list(in_election = 0)) |>
    arrange(year, flokkur) |>
    mutate(
      index = row_number(),
      .by = year
    ) |>
    filter(in_election == 1) |>
    mutate(
      index_nr = row_number(),
      .by = year
    ) |>
    select(year, index, index_nr) |>
    pivot_wider(names_from = year, values_from = index, values_fill = 0) |>
    select(-index_nr) |>
    as.matrix()



  time_diff_f <- fundamentals_data |>
    drop_na(voteshare_prior) |>
    distinct(year) |>
    pull(year) |>
    unique() |>
    diff() |>
    as.numeric()

  list(
    D_f = ncol(y_f),
    P_f = nrow(y_f),
    y_f = y_f,
    x_f = x_f,
    incumbent_f = incumbent_f,
    incumbent_years = incumbent_years,
    vnv = vnv,
    growth = growth,
    index_f = index_f,
    n_parties_f = n_parties_fundamentals,
    max_n_parties_f = max(n_parties_fundamentals),
    time_diff_f = time_diff_f
  )
}
