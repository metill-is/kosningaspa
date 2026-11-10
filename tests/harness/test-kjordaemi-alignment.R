box::use(R / stan_data[prepare_polling_data, prepare_stan_data, prepare_kjordaemi_stan_data, n_pred_k_2024])

HOUSES <- c("Kosning", "Gallup", "Maskina")
PARTIES <- c("Other", "A", "B")
KD <- c("North", "South")
ELECTION <- as.Date("2024-03-13")

# National rows: the 2021 result and two Gallup polls, on 10 January and 10 March 2024.
stub_national <- function() {
  tibble::tibble(
    date = as.Date(rep(c("2021-09-25", "2024-01-10", "2024-03-10"), each = 3)),
    fyrirtaeki = factor(rep(c("Kosning", "Gallup", "Gallup"), each = 3), levels = HOUSES),
    flokkur = factor(rep(PARTIES, 3), levels = PARTIES),
    n = c(200, 300, 500, 20, 30, 50, 25, 30, 45)
  )
}

# Kjördæmi rows for `polls` (date, fyrirtaeki, kjordaemi), one per party, in the scripts' order.
stub_kjordaemi <- function(polls, kd = KD) {
  tidyr::expand_grid(polls, flokkur = PARTIES) |>
    dplyr::mutate(
      date = as.Date(date),
      fyrirtaeki = factor(fyrirtaeki, levels = HOUSES),
      kjordaemi = factor(kjordaemi, levels = kd),
      flokkur = factor(flokkur, levels = PARTIES),
      n = 10 + seq_along(date)
    ) |>
    dplyr::arrange(flokkur, date, kjordaemi)
}

# Three elections' fundamentals for the stub's parties, joined to the economy data.
stub_fundamentals <- function() {
  tibble::tibble(
    year = rep(c(2016, 2017, 2021), each = 3),
    date = as.Date(rep(c("2016-10-29", "2017-10-28", "2021-09-25"), each = 3)),
    flokkur = rep(PARTIES, 3),
    incumbent = c(0, 1, 0, 0, 0, 1, 0, 1, 1),
    incumbent_years = c(0, 3, 0, 0, 0, 1, 0, 4, 4),
    voteshare = c(20, 30, 50, 25, 35, 40, 15, 45, 40),
    voteshare_prior = c(25, 30, 45, 20, 30, 50, 25, 35, 40),
    total_valid_votes = rep(c(1000, 1100, 1200), each = 3),
    vnv = rep(c(0.02, 0.03, 0.04), each = 3),
    growth = rep(c(2.1, 4.0, 4.3), each = 3)
  )
}

# 10 February is a kjördæmi-only date, between the national polls.
GOOD <- tibble::tribble(
  ~date, ~fyrirtaeki, ~kjordaemi,
  "2024-01-10", "Gallup", "North",
  "2024-01-10", "Gallup", "South",
  "2024-02-10", "Gallup", "North",
  "2024-02-10", "Gallup", "South"
)
MISALIGNED <- list(
  "a date missing a kjördæmi" = GOOD[-2, ],
  "two houses on one date" = dplyr::bind_rows(GOOD, dplyr::mutate(GOOD[1:2, ], fyrirtaeki = "Maskina")),
  "two houses splitting one date's kjördæmi" = dplyr::mutate(GOOD, fyrirtaeki = c("Gallup", "Maskina", "Gallup", "Gallup"))
)
CALENDAR <- as.Date(c("2021-09-25", "2024-01-10", "2024-02-10", "2024-03-10"))

test_that("prepare_polling_data() puts a kjördæmi-only date in calendar order", {
  out <- prepare_polling_data(stub_national(), stub_kjordaemi(GOOD), ELECTION)
  expect_equal(out$D, 4)
  expect_equal(out$date_k, c(2, 2, 3, 3))
  expect_equal(out$date_n, c(1, 2, 4))
  expect_equal(out$time_diff, as.numeric(diff(CALENDAR)))
})

test_that("the fit script labels the draws' dates in calendar order", {
  env <- run_block(
    FIT_SCRIPTS[["fit_script"]],
    from = "dates", to = "dates",
    polling_data = stub_national(), constituency_data = stub_kjordaemi(GOOD), election_date = ELECTION
  )
  expect_identical(env$dates, c(CALENDAR, as.Date(c("2024-03-11", "2024-03-12", "2024-03-13"))))
})

test_that("prepare_kjordaemi_stan_data() adds the constituency weights, obs_k, n_pred_k and the settings", {
  constituency_data <- stub_kjordaemi(GOOD)
  out <- prepare_kjordaemi_stan_data(
    stub_national(), stub_fundamentals(), constituency_data, ELECTION,
    n_pred_k = c(100L, 200L)
  )
  base <- prepare_stan_data(stub_national(), stub_fundamentals(), constituency_data, ELECTION)
  shared <- setdiff(names(base), "n_pred_k")
  expect_identical(out[shared], base[shared])
  shares <- prop.table(xtabs(n ~ date + kjordaemi, constituency_data), 1)
  expect_identical(rownames(out$constituency_weights), c("2024-01-10", "2024-02-10"))
  expect_identical(colnames(out$constituency_weights), KD)
  expect_equal(as.matrix(out$constituency_weights), unclass(shares), ignore_attr = TRUE)
  expect_equal(out$constituency_weights_pred, as.numeric(shares[1, ]))
  expect_identical(out$obs_k, c(1L, 1L, 2L, 2L))
  expect_identical(out$N_obs_k, 2L)
  expect_identical(out$n_pred_k, c(100L, 200L))
  expect_identical(out$desired_weight, 0.22)
  expect_identical(out$weight_time, 180)
})

for (case in names(MISALIGNED)) {
  test_that(paste("prepare_kjordaemi_stan_data() stops on", case), {
    expect_error(
      prepare_kjordaemi_stan_data(
        stub_national(), stub_fundamentals(), stub_kjordaemi(MISALIGNED[[case]]), ELECTION,
        n_pred_k = c(100L, 200L)
      ),
      "kjordaemi_obs_k"
    )
  })
}

test_that("prepare_kjordaemi_stan_data()'s default n_pred_k follows the constituency levels by name", {
  kd <- c("Su\u00F0ur", "Nor\u00F0vestur", "Reykjav\u00EDk Su\u00F0ur", "Su\u00F0vestur", "Nor\u00F0austur", "Reykjav\u00EDk Nor\u00F0ur")
  polls <- tibble::tibble(date = "2024-01-10", fyrirtaeki = "Gallup", kjordaemi = kd)
  out <- prepare_kjordaemi_stan_data(stub_national(), stub_fundamentals(), stub_kjordaemi(polls, kd), ELECTION)
  expect_identical(out$n_pred_k, n_pred_k_2024(kd))
})

test_that("kjordaemi_obs_k() stops when the weights' columns are not the kjördæmi levels", {
  box::use(R / stan_data[kjordaemi_obs_k])
  constituency_data <- stub_kjordaemi(GOOD)
  stan_data <- prepare_polling_data(stub_national(), constituency_data, ELECTION)
  weights <- data.frame(North = c(0.4, 0.5), South = c(0.6, 0.5), row.names = c("2024-01-10", "2024-02-10"))
  expect_identical(kjordaemi_obs_k(stan_data, weights, stub_national(), constituency_data), c(1L, 1L, 2L, 2L))
  expect_error(
    kjordaemi_obs_k(stan_data, weights[, 2:1], stub_national(), constituency_data),
    "kjordaemi_obs_k: constituency_weights' columns"
  )
})
