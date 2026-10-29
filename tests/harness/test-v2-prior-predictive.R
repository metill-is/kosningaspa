# The prior predictive check's band logic and row table (R/harness/v2_prior_predictive.R,
# loaded as a module, so its script part does not run): verdicts and lines on synthetic
# quantiles, the core's rows and bands, and the 20% party's composition. No prior draw is
# taken and nothing reads the snapshot.
box::use(
  R / harness / v2_prior_predictive[
    held_at, in_band, poll_sd_pp, ROWS, row_verdict, same_as, share_change, SUMMARY_FUNS, verdict_line
  ]
)

q <- function(q05, q95, q50 = (q05 + q95) / 2) c(q05 = q05, q50 = q50, q95 = q95)

test_that("an interval band holds on the 5% and the 95% quantile", {
  expect_true(in_band(q(-9, 9), "interval", -10, 10))
  expect_true(in_band(q(-10, 10), "interval", -10, 10))
  expect_false(in_band(q(-10.01, 9), "interval", -10, 10))
  expect_false(in_band(q(-9, 10.01), "interval", -10, 10))
  expect_false(in_band(q(-20, 24), "interval", -10, 10))
})

test_that("a q95 band holds on the 95% quantile alone", {
  expect_true(in_band(q(-100, 5), "q95", 2, 30))
  expect_false(in_band(q(-1, 1.9), "q95", 2, 30))
  expect_false(in_band(q(-1, 30.1), "q95", 2, 30))
  expect_error(in_band(q(0, 1), "median", 0, 1), "band type")
})

test_that("only a config-value FAIL stops, and a bias row held at section 6.10's value is WIDE", {
  expect_equal(row_verdict(TRUE, "config", held = FALSE), list(verdict = "PASS", stops = FALSE))
  expect_equal(row_verdict(TRUE, "config", held = TRUE), list(verdict = "PASS", stops = FALSE))
  expect_equal(row_verdict(FALSE, "config", held = TRUE), list(verdict = "WIDE", stops = FALSE))
  expect_equal(row_verdict(FALSE, "config", held = FALSE), list(verdict = "FAIL", stops = TRUE))
  expect_equal(row_verdict(TRUE, "comparison", held = FALSE), list(verdict = "PASS", stops = FALSE))
  expect_equal(row_verdict(FALSE, "comparison", held = FALSE), list(verdict = "FAIL", stops = FALSE))
  expect_equal(row_verdict(FALSE, "comparison", held = TRUE), list(verdict = "FAIL", stops = FALSE))
  expect_error(row_verdict(FALSE, "other", held = FALSE), "kind")
})

test_that("the lines print the summary, and the quantiles unless it passes", {
  expect_identical(verdict_line("PASS", "house_d/2024-eve/config", q(-1.234, 5.678)), "PRIOR PASS house_d/2024-eve/config")
  expect_identical(verdict_line("FAIL", "poll_sd/2021-f180/comparison", q(12.346, 38.004)), "PRIOR FAIL poll_sd/2021-f180/comparison 12.35 38.00")
  expect_identical(verdict_line("WIDE", "house_d/2024-eve/config", q(-20.5, 24.25)), "PRIOR WIDE house_d/2024-eve/config -20.50 24.25")
})

test_that("held_at() and same_as() compare the data's prior fields", {
  data <- list(prior_sd_mu1 = 1, prior_mu_phi = 6.907755278982137)
  expect_true(held_at(c(prior_sd_mu1 = 1), data))
  expect_false(held_at(c(prior_sd_mu1 = 1), list(prior_sd_mu1 = 0.3)))
  expect_false(held_at(NULL, data))
  expect_true(same_as(list(prior_mu_phi = log(1000)), data))
  expect_false(same_as(list(prior_mu_phi = 0), data))
  expect_error(held_at(c(prior_rate_sigma_gamma = 1), data), "prior_rate_sigma_gamma")
  expect_error(same_as(list(prior_rate_sigma_gamma = 5), data), "prior_rate_sigma_gamma")
})

test_that("the core's rows carry the bands written before any V2 fit", {
  core <- ROWS[ROWS$step == "v2s-m0", ]
  expect_true(all(is.na(core$switch)))
  key <- paste(core$summary, core$step, core$kind, vapply(core$set, \(s) paste(names(s), unlist(s), collapse = ","), ""))
  expect_false(anyDuplicated(key) > 0)
  expect_true(all(ROWS$summary %in% names(SUMMARY_FUNS)))
  cfg <- core[core$kind == "config", ]
  expect_setequal(cfg$summary, c("house_d", "industry_mu1", "poll_sd", "kd_delta", "walk_180"))
  band <- function(s) unlist(cfg[cfg$summary == s, c("band", "lo", "hi")], use.names = FALSE)
  expect_equal(band("house_d"), c("interval", "-10", "10"))
  expect_equal(band("industry_mu1"), c("interval", "-12", "12"))
  expect_equal(band("poll_sd"), c("interval", "0.8", "6"))
  expect_equal(band("kd_delta"), c("interval", "-25", "25"))
  expect_equal(band("walk_180"), c("q95", "2", "30"))
  held <- stats::setNames(cfg$held, cfg$summary)
  expect_equal(held$house_d, c(prior_rate_sigma_gamma = 1))
  expect_equal(held$industry_mu1, c(prior_sd_mu1 = 1))
  expect_true(all(lengths(held[c("poll_sd", "kd_delta", "walk_180")]) == 0))
  cmp <- core[core$kind == "comparison", ]
  expect_true(all(lengths(cmp$held) == 0))
  values <- function(s) sort(unlist(cmp$set[cmp$summary == s]))
  expect_equal(values("house_d"), c(prior_rate_sigma_gamma = 1, prior_rate_sigma_gamma = 5))
  expect_equal(values("industry_mu1"), c(prior_sd_mu1 = 0.3, prior_sd_mu1 = 1))
  expect_equal(values("poll_sd"), c(prior_mu_phi = 0, prior_mu_phi = log(1000)))
  expect_setequal(cmp$summary, c("house_d", "industry_mu1", "poll_sd"))
})

test_that("the 20% party's change is read off the composition beside P - 1 equal parties", {
  expect_equal(share_change(rep(0, 10)), 0)
  expect_equal(share_change(c(log(2), rep(0, 9))), 100 * (0.4 / 1.2 - 0.2))
  x <- c(0.3, -0.2, 0.5, rep(-0.6 / 7, 7))
  expect_equal(share_change(x + 4), share_change(x))
  expect_equal(share_change(c(log(2), rep(0, 4)), share = 0.1), 100 * (0.2 / 1.1 - 0.1))
})

test_that("a poll's sd is the Dirichlet-multinomial sd at n = 1000 and s = 0.2", {
  expect_equal(poll_sd_pp(1e15), 100 * sqrt(0.2 * 0.8 / 1000), tolerance = 1e-9)
  expect_equal(poll_sd_pp(0), 100 * sqrt(0.2 * 0.8), tolerance = 1e-12)
  expect_equal(poll_sd_pp(c(999, 1999)), 100 * sqrt(0.16 / 1000 * c(1999 / 1000, 2999 / 2000)))
})
