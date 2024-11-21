library(tidyverse)
library(glue)
library(arrow)
library(here)
box::use(
  R / modeling_utils[fit_model_at_date]
)

fit_dates <- seq.Date(
  from = clock::date_build(2024, 5, 30),
  to = clock::date_build(2024, 11, 1),
  by = "2 weeks"
)


for (i in seq(3, 12)) {
  fit_date <- fit_dates[i]

  fit <- fit_model_at_date(
    cutoff_date = fit_date,
    tau_f = 9.5
  )

  draws <- fit$summary(
    c(
      "tau_stjornarslit",
      "alpha_f",
      "beta0",
      "beta_lag_f",
      "beta_inc_years_f",
      "beta_vnv_f",
      "beta_growth_f",
      "beta_stjornarslit",
      "phi_inv",
      "phi_f_inv",
      "gamma",
      "sigma",
      "y_rep"
    )
  ) |>
    as_tibble()

  dir.create(
    here("results", "fit_historical", glue("fit_date={fit_date}")),
    showWarnings = FALSE
  )

  write_parquet(
    draws,
    here("results", "fit_historical", glue("fit_date={fit_date}"), "part-0.parquet")
  )

  rm(draws, fit)
  gc()
}
