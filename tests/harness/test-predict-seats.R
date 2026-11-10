box::use(R / predict_seats[predict_seats])
box::use(R / election_utils[calculate_seats, seats_tibble])

OTHER <- "Anna\u00f0"

# Made-up vote draws: four lists and the residual category in every kjordaemi of the 2024 layout.
made_up_draws <- function(n_draws) {
  parties <- c("A", "B", "C", "D", OTHER)
  d <- expand.grid(flokkur = parties, kjordaemi = seats_tibble()$kjordaemi, .draw = seq_len(n_draws), stringsAsFactors = FALSE)
  d$value <- as.integer(round(stats::runif(nrow(d), 500, 20000)))
  # WHY: a large residual vote shows it still wins no seat.
  d$value[d$flokkur == OTHER] <- 30000L
  tibble::as_tibble(d)
}

test_that("predict_seats gives 63 seats a draw, none to the residual category, as calculate_seats does draw by draw", {
  set.seed(20261110)
  draws <- made_up_draws(5)
  seats <- predict_seats(draws)
  expect_named(seats, c(".draw", "flokkur", "kjordaemi", "seats", "kjordaemi_seats"))
  expect_equal(as.vector(tapply(seats$seats, seats$.draw, sum)), rep(63, 5))
  expect_true(all(seats$seats[seats$flokkur == OTHER] == 0))
  for (d in 1:5) {
    one <- draws[draws$.draw == d, c("flokkur", "kjordaemi", "value")]
    names(one)[3] <- "votes"
    expected <- calculate_seats(one, seats_tibble(), OTHER)
    got <- seats[seats$.draw == d, names(expected)]
    expect_equal(got, expected, ignore_attr = TRUE)
  }
})
