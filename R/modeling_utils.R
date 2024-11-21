#' Fit fundamentals and polling model using data up to a specified date
#'
#' @param cutoff_date Date up to which to use polling data
#' @param election_date Date of the upcoming election
#' @return A cmdstanr fit object
#' @export
fit_model_at_date <- function(
    cutoff_date,
    tau_f = 9.5,
    election_date = clock::date_build(2024, 11, 30)) {
  box::use(
    cmdstanr[cmdstan_model],
    dplyr[filter, inner_join],
    here[here],
    readr[read_csv],
    clock[date_build]
  )

  box::use(
    R / prepare_data[
      read_polling_data,
      prepare_stan_data,
      read_fundamentals_data
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

  # Prepare Stan data
  stan_data <- prepare_stan_data(polling_data, fundamentals_data)
  stan_data$tau_f <- tau_f

  # Initialize model
  model <- cmdstan_model(
    here("Stan", "polling_and_fundamentals.stan")
  )

  # Fit model
  fit <- model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    refresh = 100,
    init = 0
  )

  return(fit)
}
