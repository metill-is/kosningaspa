box::use(R / election_utils[calculate_seats, seats_layout_2021, seats_tibble, layout_for])

kd <- seats_layout_2021()$kjordaemi
draw_with <- function(shares) {
  tidyr::expand_grid(kjordaemi = kd, flokkur = names(shares)) |>
    dplyr::mutate(votes = 10000 * shares[flokkur])
}

test_that("layouts hold 63 seats and match the elections", {
  expect_equal(sum(seats_layout_2021()$n_seats + seats_layout_2021()$n_jofnun), 63)
  expect_identical(layout_for("2017-10-28"), seats_layout_2021())
  expect_identical(layout_for(as.Date("2024-11-30")), seats_tibble())
  expect_error(layout_for("2013-04-27"), "no seat layout for 2013-04-27")
})

test_that("an ineligible list wins no seat and the rest still fill 63", {
  d <- draw_with(c(Annað = 0.30, D = 0.30, S = 0.25, B = 0.15))
  s <- calculate_seats(d, seats_layout_2021(), ineligible = "Annað")
  expect_equal(sum(s$seats[s$flokkur == "Annað"]), 0)
  expect_equal(sum(s$seats), 63)
  s0 <- calculate_seats(d, seats_layout_2021())
  expect_gt(sum(s0$seats[s0$flokkur == "Annað"]), 0)
})

test_that("the 5% threshold counts the ineligible list's votes", {
  # X has 4.8% of all valid votes but 5.3% of the votes outside Annað
  d <- draw_with(c(Annað = 0.094, X = 0.048, D = 0.458, S = 0.40))
  s <- calculate_seats(d, seats_layout_2021(), ineligible = "Annað")
  x <- s[s$flokkur == "X", ]
  expect_equal(sum(x$seats - x$kjordaemi_seats), 0)
})

test_that("adjustment seats are placed by shares that count the ineligible list's votes", {
  # More Annað votes in Reykjavík Suður shrink X's share of the votes cast there, so X's
  # adjustment seat moves elsewhere although no eligible list's votes change
  x_adjustment_in_rs <- function(d) {
    s <- calculate_seats(d, seats_layout_2021(), ineligible = "Annað")
    x <- s[s$flokkur == "X" & s$kjordaemi == "Reykjavík Suður", ]
    x$seats - x$kjordaemi_seats
  }
  d <- draw_with(c(Annað = 0.10, D = 0.40, S = 0.32, X = 0.18))
  expect_equal(x_adjustment_in_rs(d), 1)
  more_annad <- d$flokkur == "Annað" & d$kjordaemi == "Reykjavík Suður"
  d$votes[more_annad] <- 5 * d$votes[more_annad]
  expect_equal(x_adjustment_in_rs(d), 0)
})
