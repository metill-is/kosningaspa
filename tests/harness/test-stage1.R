box::use(
  R / harness / info_set[info_set],
  R / harness / stage1[stage1_data, stage1_mu_f]
)

test_that("the target row cannot reach stage 1", {
  skip_without_snapshot()
  info <- info_set(as.Date("2021-09-25"), as.Date("2021-03-29"))
  a <- stage1_data(info)
  info2 <- info
  info2$fundamentals$target[c("voteshare", "voteshare_change", "total_valid_votes")] <- 0.5
  expect_identical(stage1_data(info2), a)
  expect_true(all(attr(a, "election_dates") < as.Date("2021-09-25")))
})
test_that("stage 1's density is invariant to the order of party labels", {
  skip_unless_slow()
  d <- stage1_data(info_set(as.Date("2021-09-25"), as.Date("2021-03-29")))
  d_rev <- reverse_labels(d)
  m <- v2_methods("Stan/fundamentals_v2.stan", d)
  m_rev <- v2_methods("Stan/fundamentals_v2.stan", d_rev)
  set.seed(1)
  for (i in 1:5) {
    u <- rnorm(n_unc(m))
    pars <- m$constrain_variables(u)
    pars$alpha <- rev(pars$alpha)
    expect_equal(m_rev$log_prob(m_rev$unconstrain_variables(pars), jacobian = FALSE), m$log_prob(u, jacobian = FALSE), tolerance = 1e-10)
  }
})
test_that("stage 1 is cached by its inputs", {
  skip_unless_slow()
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  info <- info_set(as.Date("2021-09-25"), as.Date("2021-03-29"))
  cfg <- cfg_m0()
  a <- stage1_mu_f(info, cfg)
  b <- stage1_mu_f(info, cfg)
  expect_identical(a, b)
  expect_named(a$mu_f, info$fundamentals$S_f)
  expect_lt(abs(sum(a$mu_f)), 1e-8)
  expect_true(file.exists(file.path(Sys.getenv("HARNESS_RESULTS"), "stage1", a$sha256, "mu_f.json")))
  out <- capture.output(again <- stage1_mu_f(info, cfg))
  expect_false(any(startsWith(out, "STAGE1")))
  expect_identical(again, a)
  cfg$stage1$seed <- cfg$stage1$seed + 1L
  expect_false(identical(stage1_mu_f(info, cfg)$sha256, a$sha256))
})
test_that("stage 1 refuses a 2017 cell before the freeze", {
  skip_without_snapshot()
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  # WHY: a refusal moved after the compile would then fail with "compiled", not pass with the right message.
  local_mocked_bindings(cmdstan_model = function(...) stop("compiled"), .package = "cmdstanr")
  out <- capture.output(expect_error(
    stage1_mu_f(info_set(as.Date("2017-10-28"), as.Date("2017-10-27")), cfg_m0(), root = withr::local_tempdir()),
    "stage 1: 2017 is not fitted before the freeze"
  ))
  expect_false(any(startsWith(out, "STAGE1")))
  expect_false(file.exists(file.path(Sys.getenv("HARNESS_RESULTS"), "stage1")))
})
test_that("stage 1's log density and eta_E are the R reference's at arbitrary points", {
  skip_unless_slow()
  skip_without_snapshot()
  d <- stage1_data(info_set(as.Date("2021-09-25"), as.Date("2021-03-29"), v2_inputs()))
  m <- v2_methods("Stan/fundamentals_v2.stan", d)
  set.seed(2)
  lp <- matrix(NA_real_, 5, 2, dimnames = list(NULL, c("stan", "ref")))
  for (i in 1:5) {
    u <- rnorm(n_unc(m))
    pars <- m$constrain_variables(u)
    lp[i, ] <- c(m$log_prob(u, jacobian = FALSE), stage1_lp_ref(d, pars))
    expect_equal(c(pars$eta_E), stage1_eta_E_ref(d, pars), tolerance = 1e-12)
  }
  # WHY: Stan drops the constant terms (the multinomial coefficient, the normals' constants), so the gap is constant, not 0.
  gap <- lp[, "stan"] - lp[, "ref"]
  expect_lt(diff(range(gap)), 1e-9 * max(1, abs(lp)))
})
test_that("a stage-1 fit that fails its check stops and writes no cache", {
  skip_unless_slow()
  skip_without_snapshot()
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  info <- info_set(as.Date("2021-09-25"), as.Date("2021-03-29"), v2_inputs())
  cfg <- cfg_m0()
  cfg$stage1[c("iter_warmup", "iter_sampling")] <- list(10L, 10L)
  expect_error(capture.output(stage1_mu_f(info, cfg)), "stage 1: the fit fails its check")
  expect_false(dir.exists(file.path(results, "stage1")))
  expect_length(list.files(results, pattern = "mu_f", recursive = TRUE, all.files = TRUE), 0)
})

test_that("stage 1's rows are S_f(d)'s lists that stood at each election before E, with production's counts and the recorded lag", {
  skip_without_snapshot()
  for (cell in list(c("2021-09-25", "2021-03-29"), c("2024-11-30", "2024-11-29"))) {
    info <- info_set(as.Date(cell[1]), as.Date(cell[2]), v2_inputs())
    d <- stage1_data(info)
    tr <- info$fundamentals$train
    kept <- tr[tr$in_sf & tr$voteshare > 0, ]
    per_date <- table(kept$date)
    dates <- as.Date(names(per_date)[per_date >= 2])
    expect_identical(attr(d, "election_dates"), dates)
    expect_identical(c(d$n_rows), as.integer(per_date[format(dates)]))
    expect_identical(c(d$D, d$N), c(length(dates), sum(c(d$n_rows))))
    rows <- kept[kept$date %in% dates, ]
    labels <- attr(d, "labels")
    expect_identical(d$L, length(labels))
    expect_false(anyDuplicated(labels) > 0)
    # every training row appears once, as its (election, label) pair
    got <- paste(rep(dates, c(d$n_rows)), labels[c(d$label)])
    expect_setequal(got, paste(rows$date, rows$party))
    expect_false(anyDuplicated(got) > 0)
    i <- match(got, paste(rows$date, rows$party))
    total <- tapply(tr$voteshare[!is.na(tr$voteshare_prior)], tr$date[!is.na(tr$voteshare_prior)], sum)
    expect_identical(c(d$y), as.integer(round(rows$voteshare[i] / total[format(rows$date[i])] * rows$total_valid_votes[i])))
    expect_equal(c(d$x), log(rows$voteshare_prior[i] / 100) - log(1 - rows$voteshare_prior[i] / 100))
    expect_equal(c(d$inc_years), ifelse(rows$incumbent_years[i] > 0, log(rows$incumbent_years[i]), 0))
    expect_equal(c(d$vnv), log(1 + rows$vnv[i]) * rows$incumbent[i])
    expect_equal(c(d$growth), log(1 + rows$growth[i] / 100) * rows$incumbent[i])
    S_f <- info$fundamentals$S_f
    tg <- info$fundamentals$target
    expect_identical(labels[c(d$label_E)], S_f)
    expect_identical(d$N_E, length(S_f))
    expect_equal(c(d$x_E), log(tg$voteshare_prior / 100) - log(1 - tg$voteshare_prior / 100))
    expect_equal(c(d$vnv_E), log(1 + tg$vnv) * tg$incumbent)
    expect_equal(c(d$growth_E), log(1 + tg$growth / 100) * tg$incumbent)
    vectors <- c("n_rows", "label", "y", "x", "inc_years", "vnv", "growth", "label_E", "x_E", "inc_years_E", "vnv_E", "growth_E")
    for (f in vectors) expect_identical(length(dim(d[[f]])), 1L, label = f)
    for (f in c("D", "N", "L", "N_E", "n_rows", "label", "y", "label_E")) expect_true(is.integer(d[[f]]), label = f)
  }
})
