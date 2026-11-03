box::use(R / stan_data[prepare_polling_data])

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
stub_kjordaemi <- function(polls) {
  tidyr::expand_grid(polls, flokkur = PARTIES) |>
    dplyr::mutate(
      date = as.Date(date),
      fyrirtaeki = factor(fyrirtaeki, levels = HOUSES),
      kjordaemi = factor(kjordaemi, levels = KD),
      flokkur = factor(flokkur, levels = PARTIES),
      n = 10 + seq_along(date)
    ) |>
    dplyr::arrange(flokkur, date, kjordaemi)
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

# A script's obs_k for the kjördæmi rows of `polls`.
obs_k_for <- function(path, polls) {
  constituency_data <- stub_kjordaemi(polls)
  env <- run_block(
    path,
    from = "constituency_weights", to = "obs_k",
    polling_data = stub_national(), constituency_data = constituency_data,
    stan_data = prepare_polling_data(stub_national(), constituency_data, ELECTION)
  )
  env$obs_k
}

for (script in names(FIT_SCRIPTS)) {
  path <- FIT_SCRIPTS[[script]]

  test_that(paste(script, "maps each kjördæmi poll's K rows of y_k to its date's weights"), {
    expect_identical(obs_k_for(path, GOOD), c(1L, 1L, 2L, 2L))
  })

  for (case in names(MISALIGNED)) {
    test_that(paste(script, "stops on", case), {
      expect_error(obs_k_for(path, MISALIGNED[[case]]), "kjordaemi_obs_k")
    })
  }
}

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
