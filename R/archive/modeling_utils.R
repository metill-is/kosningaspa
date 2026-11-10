#' Fit fundamentals and polling model using data up to a specified date
#'
#' @param cutoff_date Date up to which to use polling data
#' @param election_date Date of the upcoming election
#' @return A cmdstanr fit object
#' @export
fit_model_at_date <- function(
    cutoff_date,
    election_date = clock::date_build(2024, 11, 30)) {
  box::use(
    cmdstanr[cmdstan_model],
    dplyr[filter, inner_join, select, mutate, reframe],
    tidyr[drop_na, pivot_longer],
    here[here],
    readr[read_csv],
    clock[date_build],
    tibble[as_tibble],
    readr[parse_number],
    stats[quantile, median],
    posterior[as_draws_df]
  )

  box::use(
    R / data[
      read_polling_data,
      read_fundamentals_data,
      read_constituency_data
    ],
    R / stan_data[
      prepare_stan_data
    ]
  )

  # Read and filter polling data
  polling_data <- read_polling_data() |>
    filter(
      date >= date_build(2016, 1, 1),
      date <= cutoff_date
    )

  # Read fundamentals data
  fundamentals_data <- read_fundamentals_data()

  # Read and join economic data
  econ_data <- read_csv(here("data", "economy_data.csv"))

  fundamentals_data <- fundamentals_data |>
    inner_join(
      econ_data,
      by = "date"
    )

  # Read constituency data
  constituency_data <- read_constituency_data() |>
    mutate(var = NA) |>
    drop_na() |>
    select(-var)

  # Prepare Stan data
  stan_data <- prepare_stan_data(
    polling_data,
    fundamentals_data,
    constituency_data
  )

  # Set additional parameters
  stan_data$desired_weight <- 0.15
  stan_data$weight_time <- 180
  stan_data$last_poll_days <- 27
  stan_data$last_poll_house <- 6
  stan_data$n_last_poll <- 1000

  # Initialize model
  model <- cmdstan_model(
    here("Stan", "polling_and_fundamentals_historic.stan")
  )

  # Fit model
  fit <- model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    refresh = 100,
    init = 0,
    iter_warmup = 1000,
    iter_sampling = 2000
  )

  draws <- fit$draws("election_prediction") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(
      c(-.chain, -.iteration, -.draw)
    ) |>
    mutate(
      p = parse_number(name),
      flokkur = colnames(stan_data$y_n)[p]
    ) |>
    select(
      .chain,
      .iteration,
      .draw,
      flokkur,
      value
    ) |>
    mutate(
      value = value / stan_data$n_pred
    ) |>
    reframe(
      median = median(value),
      coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(flokkur)
    )

  return(draws)
}

#' @export
fit_model_at_date_2021 <- function(
    cutoff_date,
    weight_time = 180,
    weight_desired = 0.33,
    election_date = clock::date_build(2021, 9, 25)) {
  box::use(
    cmdstanr[cmdstan_model],
    dplyr[
      filter,
      inner_join,
      select,
      mutate,
      reframe,
      if_else,
      count
    ],
    tidyr[drop_na, pivot_longer],
    here[here],
    readr[read_csv],
    clock[date_build],
    tibble[as_tibble],
    readr[parse_number],
    stats[quantile, median],
    posterior[as_draws_df]
  )

  box::use(
    R / data[
      read_polling_data,
      read_fundamentals_data,
      read_constituency_data
    ],
    R / stan_data_2021[
      prepare_stan_data
    ]
  )

  # Read and filter polling data
  polling_data <- read_polling_data() |>
    filter(
      date >= date_build(2016, 1, 1),
      date <= cutoff_date
    ) |>
    mutate(
      flokkur = fct_recode(flokkur, "Annað" = "Sósíalistaflokkurinn"),
      fyrirtaeki = droplevels(fyrirtaeki)
    ) |>
    count(flokkur, date, fyrirtaeki, wt = n, name = "n")

  # Read fundamentals data
  fundamentals_data <- read_fundamentals_data() |>
    mutate(
      voteshare = if_else(
        date > cutoff_date,
        NA_real_,
        voteshare
      ),
      voteshare_change = if_else(
        date > cutoff_date,
        NA_real_,
        voteshare_change
      ),
      total_valid_votes = if_else(
        date > cutoff_date,
        NA_real_,
        total_valid_votes
      )
    ) |>
    filter(
      date < max(date),
      flokkur != "Sósíalistaflokkurinn"
    ) |>
    mutate(
      flokkur = droplevels(flokkur)
    )

  # Read and join economic data
  econ_data <- read_csv(here("data", "economy_data.csv"))

  fundamentals_data <- fundamentals_data |>
    inner_join(
      econ_data,
      by = "date"
    )

  # Read constituency data
  constituency_data <- read_constituency_data() |>
    mutate(var = NA) |>
    drop_na() |>
    select(-var)

  # Prepare Stan data
  stan_data <- prepare_stan_data(
    polling_data,
    fundamentals_data,
    constituency_data
  )

  # Set additional parameters
  stan_data$desired_weight <- weight_desired
  stan_data$weight_time <- weight_time

  # Initialize model
  model <- cmdstan_model(
    here("Stan", "polling_and_fundamentals_2021.stan")
  )

  # Fit model
  fit <- model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    refresh = 100,
    init = 0,
    iter_warmup = 1000,
    iter_sampling = 1000
  )

  # Process draws
  draws <- fit$draws("election_prediction") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(
      c(-.chain, -.iteration, -.draw)
    ) |>
    mutate(
      p = parse_number(name),
      flokkur = colnames(stan_data$y_n)[p]
    ) |>
    select(
      .chain,
      .iteration,
      .draw,
      flokkur,
      value
    ) |>
    mutate(
      value = value / stan_data$n_pred
    ) |>
    reframe(
      median = median(value),
      coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(flokkur)
    )

  return(draws)
}
