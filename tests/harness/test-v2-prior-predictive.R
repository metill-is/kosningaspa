# The prior predictive check (R/harness/v2_prior_predictive.R). Its band logic, row table,
# reference forms and summaries are tested with the file loaded as a module, so its script
# part does not run: synthetic quantiles and hand-built draws with known answers, no prior
# draw and no snapshot. The script itself runs as a subprocess at 20 draws, into a temporary
# report, never the committed one.
box::use(
  R / harness / v2_prior_predictive[
    band_exact, band_inside, band_median_ratio, band_q95, band_report, check_rows, fund_weight_against, held_at, in_band,
    poll_sd_pp, reference_forms, ROWS, row_verdict, same_as, share_change, shock_sd, SUMMARY_FUNS, SUMMARY_POOLED,
    verdict_line
  ]
)

q <- function(q05, q95, q50 = (q05 + q95) / 2) c(q05 = q05, q50 = q50, q95 = q95)

# The 20% party's change in pp beside P - 1 equal parties, for effect x, written out.
change_pp <- function(x) {
  P <- length(x)
  w <- c(0.2 * exp(x[1]), (0.8 / (P - 1)) * exp(x[-1]))
  100 * (w[1] / sum(w) - 0.2)
}

test_that("an inside band holds on the 5% and the 95% quantile", {
  b <- band_inside(-10, 10)
  expect_true(in_band(b, q(-9, 9), 0))
  expect_true(in_band(b, q(-10, 10), 0))
  expect_false(in_band(b, q(-10.01, 9), 0))
  expect_false(in_band(b, q(-9, 10.01), 0))
  expect_false(in_band(b, q(-20, 24), 0))
})

test_that("a q95 band holds on the 95% quantile alone", {
  b <- band_q95(2, 30)
  expect_true(in_band(b, q(-100, 5), 0))
  expect_false(in_band(b, q(-1, 1.9), 0))
  expect_false(in_band(b, q(-1, 30.1), 0))
})

test_that("a median_ratio band holds on the median within [lo, hi] times the target", {
  b <- band_median_ratio(1.6)
  expect_true(in_band(b, q(-50, 50, q50 = 1.6), 0))
  expect_true(in_band(b, q(0, 9, q50 = 0.8), 0))
  expect_true(in_band(b, q(0, 9, q50 = 3.2), 0))
  expect_false(in_band(b, q(0, 9, q50 = 0.79), 0))
  expect_false(in_band(b, q(0, 9, q50 = 3.21), 0))
  expect_false(in_band(band_median_ratio(0.95, 0.9, 1.1), q(0, 9, q50 = 1.6), 0))
  expect_error(band_median_ratio(-1), "positive")
})

test_that("an exact band holds when every draw is within tol, whatever the quantiles", {
  b <- band_exact(0.22, 1e-8)
  expect_true(in_band(b, q(0, 1), c(0.22, 0.22 + 5e-9, 0.22 - 1e-8)))
  expect_false(in_band(b, q(0.22, 0.22), c(0.22, 0.22, 0.22 + 2e-8)))
  expect_error(band_exact(0.22, -1), "tol")
})

test_that("a report band gives no verdict, and unknown or malformed bands are refused", {
  expect_true(is.na(in_band(band_report(), q(0, 1), 0)))
  expect_error(in_band(list(type = "median"), q(0, 1), 0), "band type")
  expect_error(band_inside(1, -1), "lo above hi")
  expect_error(band_inside(NA_real_, 1), "finite")
})

test_that("only a config-value FAIL stops; a held bias row is WIDE; a report row is REPORT", {
  expect_equal(row_verdict(TRUE, "config", held = FALSE), list(verdict = "PASS", stops = FALSE))
  expect_equal(row_verdict(TRUE, "config", held = TRUE), list(verdict = "PASS", stops = FALSE))
  expect_equal(row_verdict(FALSE, "config", held = TRUE), list(verdict = "WIDE", stops = FALSE))
  expect_equal(row_verdict(FALSE, "config", held = FALSE), list(verdict = "FAIL", stops = TRUE))
  expect_equal(row_verdict(TRUE, "comparison", held = FALSE), list(verdict = "PASS", stops = FALSE))
  expect_equal(row_verdict(FALSE, "comparison", held = FALSE), list(verdict = "FAIL", stops = FALSE))
  expect_equal(row_verdict(FALSE, "comparison", held = TRUE), list(verdict = "FAIL", stops = FALSE))
  expect_equal(row_verdict(NA, "config", held = FALSE), list(verdict = "REPORT", stops = FALSE))
  expect_equal(row_verdict(NA, "comparison", held = TRUE), list(verdict = "REPORT", stops = FALSE))
  expect_error(row_verdict(FALSE, "other", held = FALSE), "kind")
})

test_that("the lines print the summary, and the quantiles unless it passes", {
  expect_identical(verdict_line("PASS", "house_d/2024-eve/config", q(-1.234, 5.678)), "PRIOR PASS house_d/2024-eve/config")
  expect_identical(verdict_line("FAIL", "poll_sd/2021-f180/comparison", q(12.346, 38.004)), "PRIOR FAIL poll_sd/2021-f180/comparison 12.35 38.00")
  expect_identical(verdict_line("WIDE", "house_d/2024-eve/config", q(-20.5, 24.25)), "PRIOR WIDE house_d/2024-eve/config -20.50 24.25")
  expect_identical(verdict_line("REPORT", "w/2024-eve/config", q(0.2, 0.3, q50 = 0.25)), "PRIOR REPORT w/2024-eve/config 0.20 0.25 0.30")
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

test_that("the reference forms set walk_form and delta_form to 0 and nothing else", {
  data <- list(P = 10L, walk_form = 1L, y = matrix(1:6, 2), delta_form = 1L, prior_mu_phi = 6.9)
  out <- reference_forms(data)
  expect_identical(names(out), names(data))
  expect_identical(out$walk_form, 0L)
  expect_identical(out$delta_form, 0L)
  others <- setdiff(names(data), c("walk_form", "delta_form"))
  expect_identical(out[others], data[others])
  expect_error(reference_forms(list(P = 10L)), "walk_form")
})

test_that("the whole table checks, and the core's rows carry the bands written before any V2 fit", {
  expect_identical(check_rows(ROWS), ROWS)
  config <- ROWS$kind == "config"
  expect_false(anyDuplicated(paste(ROWS$summary, ROWS$step)[config]) > 0)
  key <- paste(ROWS$summary, ROWS$step, ROWS$kind, vapply(ROWS$set, \(s) paste(names(s), unlist(s), collapse = ","), ""))
  expect_false(anyDuplicated(key) > 0)
  core <- ROWS[ROWS$step == "v2s-m0", ]
  expect_true(all(is.na(core$switch)))
  cfg <- core[core$kind == "config", ]
  expect_setequal(cfg$summary, c("house_d", "industry_mu1", "poll_sd", "kd_delta", "walk_180"))
  band <- stats::setNames(cfg$band, cfg$summary)
  expect_identical(band$house_d, band_inside(-10, 10))
  expect_identical(band$industry_mu1, band_inside(-12, 12))
  expect_identical(band$poll_sd, band_inside(0.8, 6))
  expect_identical(band$kd_delta, band_inside(-25, 25))
  expect_identical(band$walk_180, band_q95(2, 30))
  held <- stats::setNames(cfg$held, cfg$summary)
  expect_equal(held$house_d, c(prior_rate_sigma_gamma = 1))
  expect_equal(held$industry_mu1, c(prior_sd_mu1 = 1))
  expect_true(all(lengths(held[c("poll_sd", "kd_delta", "walk_180")]) == 0))
  cmp <- core[core$kind == "comparison", ]
  values <- function(s) sort(unlist(cmp$set[cmp$summary == s]))
  expect_equal(values("house_d"), c(prior_rate_sigma_gamma = 1, prior_rate_sigma_gamma = 5))
  expect_equal(values("industry_mu1"), c(prior_sd_mu1 = 0.3, prior_sd_mu1 = 1))
  expect_equal(values("poll_sd"), c(prior_mu_phi = 0, prior_mu_phi = log(1000)))
  expect_setequal(cmp$summary, c("house_d", "industry_mu1", "poll_sd"))
  expect_true(all(vapply(seq_len(nrow(cmp)), \(i) identical(cmp$band[[i]], band[[cmp$summary[i]]]), logical(1))))
})

test_that("check_rows() refuses a bad switch, summary or band, and repeated rows anywhere in the table", {
  later <- ROWS[ROWS$summary == "walk_180", ]
  later$step <- "v2s-m1"
  expect_identical(nrow(check_rows(dplyr::bind_rows(ROWS, later))), nrow(ROWS) + 1L)
  bad_switch <- later
  bad_switch$switch <- "epoch_biass"
  expect_error(check_rows(dplyr::bind_rows(ROWS, bad_switch)), "epoch_biass")
  ok_switch <- later
  ok_switch$switch <- "epoch_bias"
  expect_identical(nrow(check_rows(dplyr::bind_rows(ROWS, ok_switch))), nrow(ROWS) + 1L)
  expect_error(check_rows(dplyr::bind_rows(ROWS, ROWS[ROWS$summary == "walk_180", ])), "two config rows")
  expect_error(check_rows(dplyr::bind_rows(ROWS, ROWS[ROWS$summary == "poll_sd" & ROWS$kind == "comparison", ][1, ])), "repeated comparison")
  no_fun <- later
  no_fun$summary <- "no_such_summary"
  expect_error(check_rows(dplyr::bind_rows(ROWS, no_fun)), "no_such_summary")
  bad_band <- later
  bad_band$band <- list(list(type = "median"))
  expect_error(check_rows(dplyr::bind_rows(ROWS, bad_band)), "band type")
  held_cmp <- ROWS[ROWS$summary == "poll_sd" & ROWS$kind == "comparison", ][1, ]
  held_cmp$held <- list(c(prior_mu_phi = 0))
  held_cmp$set <- list(list(prior_mu_phi = 3))
  expect_error(check_rows(dplyr::bind_rows(ROWS, held_cmp)), "held on a comparison")
})

test_that("house_d reads coordinate 1 of every house's sigma_gamma C eps_h", {
  sim <- list(pars = list(sigma_gamma = 0.5, house_raw = rbind(c(1, 0, -1), c(0, 2, 1))))
  got <- SUMMARY_FUNS$house_d(sim, list(H = 2L, P = 3L), 1L)
  # d_1 = 0.5 (1, 0, -1) and d_2 = 0.5 (-1, 1, 0), each already centred.
  want <- c(
    100 * (0.2 * exp(0.5) / (0.2 * exp(0.5) + 0.4 + 0.4 * exp(-0.5)) - 0.2),
    100 * (0.2 * exp(-0.5) / (0.2 * exp(-0.5) + 0.4 * exp(0.5) + 0.4) - 0.2)
  )
  expect_equal(got, want, tolerance = 1e-12)
})

test_that("industry_mu1 reads coordinate 1 of mu1", {
  sim <- list(pars = list(mu1 = c(0.6, -0.2, -0.2, -0.2)))
  want <- 100 * (0.2 * exp(0.6) / (0.2 * exp(0.6) + 0.8 * exp(-0.2)) - 0.2)
  expect_equal(SUMMARY_FUNS$industry_mu1(sim, list(P = 4L), 1L), want, tolerance = 1e-12)
})

test_that("poll_sd is the Dirichlet-multinomial sd at each house's phi", {
  sim <- list(truth = list(phi = c(1000, 4000)))
  want <- 100 * sqrt(0.2 * 0.8 / 1000 * c(2000 / 1001, 5000 / 4001))
  expect_equal(SUMMARY_FUNS$poll_sd(sim, list(H = 2L), 1L), want, tolerance = 1e-12)
})

test_that("kd_delta reads coordinate 1 of every kjordaemi's delta in the open epoch", {
  w <- c(0.25, 0.75)
  X <- rbind(c(0.4, 0, -0.1), c(-0.2, 0.3, 0))
  # M_e by hand: subtract each party's w-weighted mean (-0.05, 0.225, -0.025), then centre each row.
  Y <- rbind(c(0.45, -0.225, -0.075), c(-0.15, 0.075, 0.025))
  delta_open <- Y - rowMeans(Y)
  expect_equal(drop(w %*% delta_open), rep(0, 3), tolerance = 1e-15)
  expect_equal(rowSums(delta_open), rep(0, 2), tolerance = 1e-15)
  delta <- array(99, c(2, 2, 3))
  delta[2, , ] <- delta_open
  sim <- list(truth = list(delta = delta))
  want <- c(
    100 * (0.2 * exp(0.4) / (0.2 * exp(0.4) + 0.4 * exp(-0.275) + 0.4 * exp(-0.125)) - 0.2),
    100 * (0.2 * exp(-0.15 + 0.05 / 3) / (0.2 * exp(-0.15 + 0.05 / 3) + 0.4 * exp(0.075 + 0.05 / 3) + 0.4 * exp(0.025 + 0.05 / 3)) - 0.2)
  )
  expect_equal(SUMMARY_FUNS$kd_delta(sim, list(n_epoch = 2L, K = 2L, P = 3L), 1L), want, tolerance = 1e-12)
  # The label says which delta it reads: the open epoch's, static under delta_walk = 0 and the walk's under 1.
  label <- unique(ROWS$label[ROWS$summary == "kd_delta"])
  expect_length(label, 1)
  expect_true(all(vapply(c("open epoch", "`delta_walk` = 0", "the walk's under 1"), \(x) grepl(x, label, fixed = TRUE), logical(1))))
})

test_that("walk_180 maps z through sqrt(180) C (sigma * L_Omega z)", {
  sigma <- c(0.05, 0.01, 0.03)
  L <- rbind(c(1, 0, 0), c(0.5, sqrt(0.75), 0), c(0, 0, 1))
  sim <- list(pars = list(sigma = sigma, L_Omega = L))
  z <- withr::with_seed(7000001L, stats::rnorm(3))
  v <- c(0.05 * z[1], 0.01 * (0.5 * z[1] + sqrt(0.75) * z[2]), 0.03 * z[3])
  x <- sqrt(180) * (v - sum(v) / 3)
  want <- 100 * (0.2 * exp(x[1]) / (0.2 * exp(x[1]) + 0.4 * exp(x[2]) + 0.4 * exp(x[3])) - 0.2)
  got <- SUMMARY_FUNS$walk_180(sim, list(P = 3L), 7000001L)
  expect_equal(got, want, tolerance = 1e-12)
  expect_gt(abs(got), 0.1)
})

test_that("industry_step reads coordinate 1 of every epoch step C(tau * xi_e), Annad's element at its own scale", {
  xi <- matrix(c(1, 0, -1, 0.5, 0.5, 2), 3, 2)
  sim <- list(pars = list(mu_step_raw = xi, tau_mu = array(0.1, 1), tau_mu_A = array(0.3, 1)))
  data <- list(P = 3L, n_epoch = 3L, annad_idx = 3L)
  # The steps (0.1, 0, -0.3) and (0.05, 0.05, 0.6), the third party's at 0.3.
  want <- c(change_pp(c(0.1, 0, -0.3)), change_pp(c(0.05, 0.05, 0.6)))
  expect_equal(SUMMARY_FUNS$industry_step(sim, data, 1L), want, tolerance = 1e-12)
  # With no Annad every party's step scales by tau_mu.
  expect_equal(SUMMARY_FUNS$industry_step(sim, utils::modifyList(data, list(annad_idx = 0L)), 1L)[2], change_pp(c(0.05, 0.05, 0.2)), tolerance = 1e-12)
  expect_error(SUMMARY_FUNS$industry_step(sim, utils::modifyList(data, list(annad_idx = 1L)), 1L), "coordinate party")
})

test_that("the epoch bias's row runs under its switch, with the band written before any fit of its step", {
  row <- ROWS[ROWS$step == "v2s-m1", ]
  expect_identical(row$summary, "industry_step")
  expect_identical(row$kind, "config")
  expect_identical(row$switch, "epoch_bias")
  expect_identical(row$band[[1]], band_inside(-6, 6))
  expect_identical(row$keys[[1]], "prior_tau_mu")
  expect_length(row$held[[1]], 0)
})

test_that("the 20% party's change is read off the composition beside P - 1 equal parties", {
  expect_equal(share_change(rep(0, 10)), 0)
  expect_equal(share_change(c(log(2), rep(0, 9))), 100 * (0.4 / 1.2 - 0.2))
  x <- c(0.3, -0.2, 0.5, rep(-0.6 / 7, 7))
  expect_equal(share_change(x), change_pp(x))
  expect_equal(share_change(x + 4), share_change(x))
  expect_equal(share_change(c(log(2), rep(0, 4)), share = 0.1), 100 * (0.2 / 1.1 - 0.1))
})

test_that("a poll's sd is the Dirichlet-multinomial sd at n = 1000 and s = 0.2", {
  expect_equal(poll_sd_pp(1e15), 100 * sqrt(0.2 * 0.8 / 1000), tolerance = 1e-9)
  expect_equal(poll_sd_pp(0), 100 * sqrt(0.2 * 0.8), tolerance = 1e-12)
  expect_equal(poll_sd_pp(c(999, 1999)), 100 * sqrt(0.16 / 1000 * c(1999 / 1000, 2999 / 2000)))
})

# v2_prior_predictive.R run from the project root with `args`: its output, with the exit
# status as attribute "status" when it is not 0.
run_script <- function(args) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/v2_prior_predictive.R", args),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
}

# The sha256 of the committed report, or NA if there is none.
committed_sha <- function() {
  path <- root("harness", "v2", "prior_predictive.md")
  if (file.exists(path)) digest::digest(file = path, algo = "sha256") else NA_character_
}

test_that("the script prints its lines and writes its report to out, leaving the committed one alone", {
  skip_without_snapshot()
  before <- committed_sha()
  out <- withr::local_tempfile(fileext = ".md")
  res <- run_script(c(file.path("harness", "configs", "steps", "v2s-m0.json"), "20", out))
  expect_null(attr(res, "status"), label = paste(res, collapse = "\n"))
  lines <- grep("^PRIOR ", res, value = TRUE)
  expect_length(lines, 16)
  expect_true("PRIOR PASS poll_sd/2024-eve/config" %in% lines)
  expect_true(any(startsWith(lines, "PRIOR WIDE house_d/2024-eve/config ")))
  expect_true(any(startsWith(lines, "PRIOR WIDE industry_mu1/2021-f180/config ")))
  expect_true(any(startsWith(lines, "PRIOR FAIL poll_sd/2021-f180/comparison:prior_mu_phi=0 ")))
  report <- readLines(out)
  expect_identical(report[1], "# V2 prior predictive check")
  expect_true(all(lines %in% report))
  expect_true(any(grepl("Prior draws: 20 per cell and row", report, fixed = TRUE)))
  # An adopted P-step form reparameterises the same model: the run goes through, draw for draw.
  forms <- run_script(c(config_with(walk_form = 1L, delta_form = 1L), "20", withr::local_tempfile(fileext = ".md")))
  expect_null(attr(forms, "status"), label = paste(forms, collapse = "\n"))
  expect_identical(grep("^PRIOR ", forms, value = TRUE), lines)
  expect_identical(committed_sha(), before)
})

test_that("a config row out of its band prints FAIL, still writes the report, and exits non-zero", {
  skip_without_snapshot()
  before <- committed_sha()
  cfg <- config_with(prior_mu_phi = 0)
  out <- withr::local_tempfile(fileext = ".md")
  res <- run_script(c(cfg, "20", out))
  expect_false(is.null(attr(res, "status")))
  lines <- grep("^PRIOR ", res, value = TRUE)
  expect_true(any(startsWith(lines, "PRIOR FAIL poll_sd/2024-eve/config ")))
  expect_true(any(startsWith(lines, "PRIOR FAIL poll_sd/2021-f180/config ")))
  expect_true("PRIOR PASS poll_sd/2024-eve/comparison:prior_mu_phi=6.90776" %in% lines)
  expect_true(any(grepl("config row(s) left their band", res, fixed = TRUE)))
  expect_true(file.exists(out))
  expect_true(any(grepl("| FAIL (stops) |", readLines(out), fixed = TRUE)))
  expect_identical(committed_sha(), before)
})

test_that("newcomer_entry reads m_new + s_new eta of every newcomer on a 10% party beside P - 1 equal parties", {
  sim <- list(pars = list(m_new = array(0.2, 1), s_new = array(0.4, 1), new_raw = c(1, -2, 0)))
  # The contrasts 0.6, -0.6 and 0.2 against the other three parties' common bias.
  want <- vapply(c(0.6, -0.6, 0.2), \(x) 100 * (0.1 * exp(x) / (0.1 * exp(x) + 0.9) - 0.1), numeric(1))
  expect_equal(SUMMARY_FUNS$newcomer_entry(sim, list(P = 4L), 1L), want, tolerance = 1e-12)
  expect_gt(want[1], 0)
  expect_lt(want[2], 0)
  # With newcomer_pool = 0 the point has no m_new or s_new to read.
  off <- list(pars = list(m_new = array(numeric(0), 0), s_new = array(numeric(0), 0), new_raw = 1))
  expect_error(SUMMARY_FUNS$newcomer_entry(off, list(P = 4L), 1L), "newcomer_pool = 1")
})

test_that("the newcomer pool's row runs under its switch, with the band written before any fit of its step", {
  row <- ROWS[ROWS$step == "v2s-m2", ]
  expect_identical(row$summary, "newcomer_entry")
  expect_identical(row$kind, "config")
  expect_identical(row$switch, "newcomer_pool")
  expect_identical(row$band[[1]], band_inside(-10, 10))
  expect_length(row$keys[[1]], 0)
  expect_length(row$held[[1]], 0)
})

test_that("shock_sd is the sd over 200 z draws of the first party's change under C(s d z), d the builder's formula", {
  # P = 3 at a 20% party: the others stand at 40%, so d = (1, sqrt(0.5), sqrt(0.5)).
  z <- withr::with_seed(11L, matrix(stats::rnorm(3 * 200), 3, 200))
  x <- 0.3 * c(1, sqrt(0.5), sqrt(0.5)) * z
  want <- stats::sd(apply(x, 2, \(v) 100 * (0.2 * exp(v[1]) / (0.2 * exp(v[1]) + 0.4 * exp(v[2]) + 0.4 * exp(v[3])) - 0.2)))
  expect_equal(shock_sd(0.3, 3L, 0.2, 11L), want, tolerance = 1e-12)
  # At a 5% party beside nine of 95% / 9 its d is 2 and theirs sqrt(0.2 / (0.95 / 9)).
  d <- c(2, rep(sqrt(0.2 / (0.95 / 9)), 9))
  z <- withr::with_seed(12L, matrix(stats::rnorm(10 * 200), 10, 200))
  want <- stats::sd(apply(d * 0.15 * z, 2, \(v) {
    w <- c(0.05, rep(0.95 / 9, 9)) * exp(v)
    100 * (w[1] / sum(w) - 0.05)
  }))
  expect_equal(shock_sd(0.15, 10L, 0.05, 12L), want, tolerance = 1e-12)
  # The cap at 10 binds below 2%: a 1% party's d is sqrt(10), not sqrt(20). The centring
  # cancels in the softmax, so the uncentred x gives the same change.
  z <- withr::with_seed(13L, matrix(stats::rnorm(2 * 200), 2, 200))
  want <- stats::sd(apply(c(sqrt(10), sqrt(0.2 / 0.99)) * 0.1 * z, 2, \(v) {
    w <- c(0.01, 0.99) * exp(v)
    100 * (w[1] / sum(w) - 0.01)
  }))
  expect_equal(shock_sd(0.1, 2L, 0.01, 13L), want, tolerance = 1e-12)
  expect_equal(shock_sd(0, 10L, 0.2, 1L), 0)
  expect_equal(shock_sd(0.2, 10L, 0.2, 1L), 2 * shock_sd(0.1, 10L, 0.2, 1L), tolerance = 0.05)
})

test_that("shock_sd refuses a negative scale, fewer than 2 parties and a share outside (0, 1)", {
  expect_error(shock_sd(-0.1, 10L, 0.2, 1L), "prior predictive: shock_sd() needs s_bar_E", fixed = TRUE)
  expect_error(shock_sd(c(0.1, 0.2), 10L, 0.2, 1L), "prior predictive: shock_sd() needs s_bar_E", fixed = TRUE)
  expect_error(shock_sd(0.1, 1L, 0.2, 1L), "prior predictive: shock_sd() needs P", fixed = TRUE)
  expect_error(shock_sd(0.1, 2.5, 0.2, 1L), "prior predictive: shock_sd() needs P", fixed = TRUE)
  expect_error(shock_sd(0.1, 10L, 1, 1L), "prior predictive: shock_sd() needs share", fixed = TRUE)
  expect_error(shock_sd(0.1, 10L, 0, 1L), "prior predictive: shock_sd() needs share", fixed = TRUE)
})

test_that("the shock rows read the draw's s_bar_E, and refuse a draw without one", {
  sim <- list(pars = list(s_bar_E = array(0.12, 1)))
  expect_equal(SUMMARY_FUNS$shock_sd_20(sim, list(P = 10L), 5L), shock_sd(0.12, 10L, 0.2, 5L), tolerance = 1e-12)
  expect_equal(SUMMARY_FUNS$shock_sd_05(sim, list(P = 10L), 5L), shock_sd(0.12, 10L, 0.05, 5L), tolerance = 1e-12)
  # With shock = 0 the point has no s_bar_E to read.
  off <- list(pars = list(s_bar_E = array(numeric(0), 0)))
  expect_error(SUMMARY_FUNS$shock_sd_20(off, list(P = 10L), 5L), "shock = 1")
  expect_error(SUMMARY_FUNS$shock_sd_05(off, list(P = 10L), 5L), "shock = 1")
})

test_that("the shock's rows run under its switch, with the bands written before any fit of its step", {
  rows <- ROWS[ROWS$step == "v2s-m3", ]
  expect_identical(rows$summary, c("shock_sd_20", "shock_sd_05"))
  expect_identical(rows$kind, c("config", "config"))
  expect_identical(rows$switch, c("shock", "shock"))
  expect_identical(rows$band[[1]], band_median_ratio(1.6))
  expect_identical(rows$band[[2]], band_median_ratio(0.95))
  expect_identical(rows$keys, list("prior_s_bar_E", "prior_s_bar_E"))
  expect_true(all(lengths(rows$held) == 0))
  expect_true(all(grepl("200 draws of z", rows$label, fixed = TRUE)))
  expect_true(grepl("on a 20% party", rows$label[1], fixed = TRUE))
  expect_true(grepl("on a 5% party", rows$label[2], fixed = TRUE))
})

test_that("delta_step_4y reads M_open of a 4-year step s_bar_zeta s_zeta_raw * zeta, kjordaemi by kjordaemi, on a 20% party", {
  # K = 2 kjordaemi and P = 3 parties, the open epoch's weights (0.25, 0.75); at delta_years_bar
  # 4 a 4-year gap scales the step by sqrt(4 / 4) = 1.
  sim <- list(pars = list(s_bar_zeta = array(0.5, 1), s_zeta_raw = c(0.4, 1, 2)))
  data <- list(K = 2L, P = 3L, n_epoch = 2L, w = rbind(c(0.5, 0.5), c(0.25, 0.75)), delta_years_bar = 4)
  zeta <- withr::with_seed(21L, matrix(stats::rnorm(6), 2, 3))
  X <- zeta %*% diag(0.5 * c(0.4, 1, 2))
  Y <- X - matrix(drop(c(0.25, 0.75) %*% X), 2, 3, byrow = TRUE)
  step <- Y - rowMeans(Y)
  expect_equal(drop(c(0.25, 0.75) %*% step), rep(0, 3), tolerance = 1e-15)
  want <- apply(step, 1, change_pp)
  expect_equal(SUMMARY_FUNS$delta_step_4y(sim, data, 21L), want, tolerance = 1e-12)
  # At delta_years_bar 1 the same gap scales it by sqrt(4).
  want_1 <- apply(2 * step, 1, change_pp)
  expect_equal(SUMMARY_FUNS$delta_step_4y(sim, utils::modifyList(data, list(delta_years_bar = 1)), 21L), want_1, tolerance = 1e-12)
  # The coordinate party's own scale is s_zeta_raw[1]: at 0 its column is the weighted mean's
  # and the other parties' move alone.
  flat <- sim
  flat$pars$s_zeta_raw[1] <- 0
  X0 <- zeta %*% diag(0.5 * c(0, 1, 2))
  Y0 <- X0 - matrix(drop(c(0.25, 0.75) %*% X0), 2, 3, byrow = TRUE)
  expect_equal(SUMMARY_FUNS$delta_step_4y(flat, data, 21L), apply(Y0, 1, change_pp), tolerance = 1e-12)
  # With delta_walk = 0 the point has no s_bar_zeta or s_zeta_raw to read.
  off <- list(pars = list(s_bar_zeta = array(numeric(0), 0), s_zeta_raw = numeric(0)))
  expect_error(SUMMARY_FUNS$delta_step_4y(off, data, 21L), "delta_walk = 1")
})

test_that("the delta walk's row runs under its switch, with the band written before any fit of its step", {
  row <- ROWS[ROWS$step == "v2s-m4", ]
  expect_identical(row$summary, "delta_step_4y")
  expect_identical(row$kind, "config")
  expect_identical(row$switch, "delta_walk")
  expect_identical(row$band[[1]], band_inside(-15, 15))
  expect_identical(row$keys[[1]], "prior_s_bar_zeta")
  expect_length(row$held[[1]], 0)
  # The label states the construction: a 4-year step, the open epoch's weights, and the term
  # it leaves out, the weight change; "re-centring" names the walk's own -c 1' term.
  left_out <- "leaving out M_e(\u03b4_{e\u22121}) \u2212 \u03b4_{e\u22121}, the change of kj\u00f6rd\u00e6mi weights"
  expect_true(all(vapply(c("4-year", "open epoch", left_out), \(x) grepl(x, row$label, fixed = TRUE), logical(1))))
  expect_false(grepl("re-centring", row$label, fixed = TRUE))
})

# A hand-built design for the fundamentals rows: P = 3 parties, S_f the first and third, three
# epochs, and the spec's weight, days and campaign.
FUND_DATA <- list(
  P = 3L, n_epoch = 3L, fundamentals = 1L, sf_idx = c(1L, 3L), weight_time = 180, campaign_days = 47L, desired_weight = 0.22
)

# A draw for the fundamentals rows: independent walk scales sigma (L_Omega = I), tau_camp, and
# the industry bias mu by epoch (3 x 3).
fund_sim <- function(sigma, tau_camp, mu) list(pars = list(sigma = sigma, L_Omega = diag(3), tau_camp = tau_camp), truth = list(mu = mu))

test_that("fund_weight is the pseudo-observation's weight against V_w for every party of S_f, w by construction", {
  sigma <- c(0.01, 0.02, 0.015)
  C <- diag(3) - 1 / 3
  V_w <- diag(C %*% diag(sigma^2) %*% C)[c(1, 3)] * (133 + 47 * 1.5^2)
  tau_f <- sqrt(V_w * 0.78 / 0.22)
  sim <- fund_sim(sigma, 0.5, matrix(0, 3, 3))
  expect_equal(SUMMARY_FUNS$fund_weight(sim, FUND_DATA, 1L), rep(0.22, 2), tolerance = 1e-12)
  expect_equal(fund_weight_against(tau_f, V_w), rep(0.22, 2), tolerance = 1e-12)
  # Against twice V_w the weight is 2w / (2w + 1 - w).
  expect_equal(fund_weight_against(tau_f, 2 * V_w), rep(0.44 / 1.22, 2), tolerance = 1e-12)
  expect_error(SUMMARY_FUNS$fund_weight(sim, utils::modifyList(FUND_DATA, list(fundamentals = 0L)), 1L), "fundamentals = 1")
  expect_error(SUMMARY_FUNS$fund_weight(sim, utils::modifyList(FUND_DATA, list(sf_idx = 1L)), 1L), "two parties of S_f")
})

test_that("fund_weight_bias adds the variance over the draws of the open-epoch step over S_f, centred over S_f", {
  sigma <- c(0.01, 0.02, 0.015)
  C <- diag(3) - 1 / 3
  V_w <- diag(C %*% diag(sigma^2) %*% C)[c(1, 3)] * (133 + 47 * 1.5^2)
  tau_f <- sqrt(V_w * 0.78 / 0.22)
  # The open epoch is 3: each draw's step mu[3, ] - mu[2, ] over parties 1 and 3, centred, is
  # (-a, a) with a = 0.1, -0.2, 0.4; a common shift (party 2's value, 5) cancels.
  steps <- c(0.1, -0.2, 0.4)
  mus <- lapply(steps, \(a) rbind(c(0, 0, 0), c(0.3, 1, -0.3), c(0.3 - a + 5, 6, -0.3 + a + 5)))
  draws <- lapply(seq_along(mus), \(i) SUMMARY_FUNS$fund_weight_bias(fund_sim(sigma, 0.5, mus[[i]]), FUND_DATA, i))
  expect_equal(draws[[2]]$step, c(0.2, -0.2), tolerance = 1e-12)
  V_bias <- rep(stats::var(steps), 2)
  want <- c(t(rbind(fund_weight_against(tau_f, V_w + V_bias), fund_weight_against(tau_f, V_w + V_bias), fund_weight_against(tau_f, V_w + V_bias))))
  expect_equal(SUMMARY_POOLED$fund_weight_bias(draws, FUND_DATA), want, tolerance = 1e-12)
  expect_true(all(want > 0.22))
  # With no step, or a step shared by S_f, nothing is added: the weight is w.
  flat <- lapply(1:3, \(i) SUMMARY_FUNS$fund_weight_bias(fund_sim(sigma, 0.5, rbind(0, 0, c(i, 7, i))), FUND_DATA, i))
  expect_equal(SUMMARY_POOLED$fund_weight_bias(flat, FUND_DATA), rep(0.22, 6), tolerance = 1e-12)
})

test_that("the fundamentals rows run under its switch: the construction checked to 1e-8, the bias weight reported with no band", {
  rows <- ROWS[ROWS$step == "v2s-m5", ]
  expect_identical(rows$summary, c("fund_weight", "fund_weight_bias"))
  expect_identical(rows$kind, c("config", "config"))
  expect_identical(rows$switch, c("fundamentals", "fundamentals"))
  expect_identical(rows$band[[1]], band_exact(0.22, 1e-8))
  expect_identical(rows$band[[2]], band_report())
  expect_identical(rows$keys[[1]], c("desired_weight", "weight_time", "campaign_days"))
  expect_identical(rows$keys[[2]], "prior_tau_mu")
  expect_identical(names(SUMMARY_POOLED), "fund_weight_bias")
  # The bias row's label says what 6.5's target leaves out.
  expect_true(grepl("0.22 target leaves out", rows$label[2], fixed = TRUE))
})

test_that("on v2s-m5 the script checks the weight to 1e-8 and reports the bias weight, every other line as on v2s-m4", {
  skip_without_snapshot()
  before <- committed_sha()
  m5 <- run_script(c(file.path("harness", "configs", "steps", "v2s-m5.json"), "20", withr::local_tempfile(fileext = ".md")))
  expect_null(attr(m5, "status"), label = paste(m5, collapse = "\n"))
  m4 <- run_script(c(file.path("harness", "configs", "steps", "v2s-m4.json"), "20", withr::local_tempfile(fileext = ".md")))
  lines <- grep("^PRIOR ", m5, value = TRUE)
  fund <- grepl(" fund_weight", lines, fixed = TRUE)
  expect_identical(lines[!fund], grep("^PRIOR ", m4, value = TRUE))
  expect_true(all(c("PRIOR PASS fund_weight/2024-eve/config", "PRIOR PASS fund_weight/2021-f180/config") %in% lines))
  expect_identical(sum(startsWith(lines, "PRIOR REPORT fund_weight_bias/")), 2L)
  expect_identical(committed_sha(), before)
})
