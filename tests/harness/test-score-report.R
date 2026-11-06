# score_report.R's rule, on made-up scores: the tie rule (read_diff()), the d2 p-value and
# the adoption rule's four criteria (adoption_criteria()). The script part, which scores real
# runs, is not run here.
box::use(R / harness / score_report[read_diff, d2_p, adoption_criteria, mae_summary, MARGINS, GATE_P])

test_that("read_diff calls a difference under 2 x SE_diff a tie, and otherwise better or worse (lower is better)", {
  d <- read_diff(1.00, 1.05, 0.03, 0.04)
  expect_equal(d$diff, 0.05)
  expect_equal(d$se_diff, 0.05)
  expect_identical(d$reading, "tie")
  expect_identical(read_diff(1.00, 0.85, 0.03, 0.04)$reading, "better")
  expect_identical(read_diff(1.00, 1.15, 0.03, 0.04)$reading, "worse")
  # WHY: exactly 2 x SE_diff is not under it, so it is not a tie.
  expect_identical(read_diff(0, 0.1, 0.03, 0.04)$reading, "worse")
  expect_identical(read_diff(1, NA, 0.03, 0.04)$reading, NA_character_)
})

test_that("read_diff calls equal values a tie even with no Monte Carlo error, and any other difference real", {
  # WHY: two runs that both put a probability at exactly 0 have a Brier score of 0 and an SE of 0.
  expect_identical(read_diff(0, 0, 0, 0)$reading, "tie")
  expect_identical(read_diff(0, 0.001, 0, 0)$reading, "worse")
  expect_identical(read_diff(0.001, 0, 0, 0)$reading, "better")
})

test_that("d2_p is the upper tail of chi-squared with n_parties - 1 degrees of freedom", {
  expect_equal(d2_p(16.919, 10), 0.05, tolerance = 1e-4)
  expect_equal(d2_p(3.57, 10), stats::pchisq(3.57, 9, lower.tail = FALSE))
})

base <- list(
  crps10 = 1.0, se_crps10 = 0.01, mdss = -8, se_mdss = 0.1, d2 = 8, se_d2 = 0.2, n_parties = 10,
  kd_crps = 1.5, se_kd_crps = 0.01, seat_crps_pp = 1.0, se_seat_crps_pp = 0.01
)
with_scores <- function(...) utils::modifyList(base, list(...))

test_that("adoption_criteria passes all four when new is better on every score", {
  new <- with_scores(crps10 = 0.9, mdss = -9, kd_crps = 1.4, seat_crps_pp = 0.9)
  expect_identical(adoption_criteria(base, new), c(gate = TRUE, accuracy = TRUE, distribution = TRUE, regional = TRUE))
})

test_that("adoption_criteria's gate fails when new's d2 has p at or below GATE_P", {
  expect_true(GATE_P == 0.01)
  new <- with_scores(d2 = stats::qchisq(GATE_P / 2, 9, lower.tail = FALSE), mdss = -9)
  expect_false(adoption_criteria(base, new)[["gate"]])
})

test_that("accuracy and regional allow a loss within the margin, and a tie of any size", {
  expect_identical(unname(MARGINS), c(0.05, 0.10, 0.05))
  within <- with_scores(crps10 = 1.04, kd_crps = 1.59, seat_crps_pp = 1.04, mdss = -9)
  expect_true(all(adoption_criteria(base, within)[c("accuracy", "regional")]))
  beyond <- with_scores(crps10 = 1.06, kd_crps = 1.5, mdss = -9)
  expect_false(adoption_criteria(base, beyond)[["accuracy"]])
  noisy <- with_scores(crps10 = 1.06, se_crps10 = 0.1, mdss = -9)
  expect_true(adoption_criteria(base, noisy)[["accuracy"]])
  kd_loss <- with_scores(kd_crps = 1.61, mdss = -9)
  expect_false(adoption_criteria(base, kd_loss)[["regional"]])
  seat_loss <- with_scores(seat_crps_pp = 1.06, mdss = -9)
  expect_false(adoption_criteria(base, seat_loss)[["regional"]])
})

test_that("distribution needs mDSS lower beyond a tie, unless ref fails the gate", {
  expect_false(adoption_criteria(base, with_scores(mdss = -8.1))[["distribution"]])
  expect_true(adoption_criteria(base, with_scores(mdss = -8.5))[["distribution"]])
  expect_false(adoption_criteria(base, with_scores(mdss = -7))[["distribution"]])
  ref_uncalibrated <- with_scores(d2 = 40)
  expect_true(adoption_criteria(ref_uncalibrated, with_scores(mdss = -7))[["distribution"]])
})

test_that("mae_summary takes each run's MAEs from the scored reading and reads MAE8 by the tie rule", {
  fake <- function(mae8, se, mae_all, kd, seat) {
    list(
      national = tibble::tibble(
        reading = c("eday_national", "kd_aggregate"), mae8 = c(99, mae8), se_mae8 = c(9, se), mae_all = c(99, mae_all)
      ),
      kjordaemi = tibble::tibble(kd_mae_named = kd),
      seats = tibble::tibble(seat_abs_err = seat)
    )
  }
  m <- mae_summary(fake(1.2, 0.01, 1.1, 1.6, 7), fake(1.5, 0.02, 1.4, 1.7, 12))
  expect_equal(m$mae8, c(ref = 1.2, new = 1.5))
  expect_equal(m$se_mae8, c(ref = 0.01, new = 0.02))
  expect_equal(m$mae8_diff$diff, 0.3)
  expect_identical(m$mae8_diff$reading, "worse")
  expect_equal(m$mae_all, c(ref = 1.1, new = 1.4))
  expect_equal(m$kd_mae, c(ref = 1.6, new = 1.7))
  expect_equal(m$seat_err, c(ref = 7, new = 12))
  expect_identical(mae_summary(fake(1.2, 0.1, 1, 1, 1), fake(1.25, 0.1, 1, 1, 1))$mae8_diff$reading, "tie")
})
