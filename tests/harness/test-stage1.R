box::use(
  R / harness / compare_runs[NEAR_CAP],
  R / harness / constants[project_root],
  R / harness / info_set[info_set],
  R / harness / runs[sha256_file],
  R / harness / stage1[
    STAGE1_STAN, stage1_current, stage1_data, stage1_key, stage1_mu_f, stage1_near_cap, stage1_near_cap_of
  ],
  R / harness / v2_reference[V2_CAP_MARGIN, V2_STAGE1_LOG_CAP]
)

# The near-cap counts of a stage-1 record over 4 x 1000 draws.
NEAR_RECORD <- list(cap = 20, margin = 2, near_cap = 0.5, n_draws = 4000L, within_margin = 6L, within_near_cap = 2L)

# A stage-1 cache record as stage1_mu_f() writes it (iter_sampling 1000, so 4 x 1000 draws), at
# key `key`, its log phi_f near-cap counts `near` (none: a record written before they were).
stage1_record <- function(key = strrep("e5", 32), near = NEAR_RECORD, parties = c("p1", "p2")) {
  diagnostics <- list(
    max_rhat = 1.002, min_ess_bulk = 900, min_ess_tail = 800, num_divergent = 0L, num_max_treedepth = 0L,
    min_ebfmi = 0.9, minutes = 0.2
  )
  if (!is.null(near)) diagnostics$log_phi_f_near_cap <- near
  list(
    sha256 = key, E = "2021-09-25", F = "2021-03-29", parties = I(parties), mu_f = I(rep(0, length(parties))),
    diagnostics = diagnostics, seed = 20261028L, iter_warmup = 1000L, iter_sampling = 1000L, cmdstan_version = "2.38.0"
  )
}

# `rec` written to `path` as stage1_mu_f() writes mu_f.json; returns `path`.
round_trip <- function(rec, path = withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())) {
  jsonlite::write_json(rec, path, auto_unbox = TRUE, pretty = TRUE, digits = I(17))
  path
}

test_that("stage 1's cap and the two margins are Decision 4's, the stop rule's and the Stan file's", {
  # WHY: written out here, so that a change to any of them shows.
  expect_identical(V2_STAGE1_LOG_CAP, c(log_phi_f = 20))
  expect_identical(V2_CAP_MARGIN, 2)
  expect_identical(NEAR_CAP, 0.5)
  line <- grep("phi_f_inv;", readLines(root(STAGE1_STAN)), value = TRUE)
  expect_length(line, 1)
  expect_match(line, "real<lower = exp(-20)> phi_f_inv;", fixed = TRUE)
})

test_that("stage 1 counts the draws of log phi_f within 2 and within 0.5 of its cap, strictly", {
  # WHY: log phi_f = -log(phi_f_inv); 18 and 19.5 sit exactly on the margins and round-trip exp() and log() exactly.
  log_phi_f <- c(rep(18.5, 3), 18, rep(19.75, 2), 19.5, rep(10, 5))
  near <- stage1_near_cap(matrix(exp(-log_phi_f), ncol = 1))
  expect_identical(near, list(cap = 20, margin = 2, near_cap = 0.5, n_draws = 12L, within_margin = 6L, within_near_cap = 2L))
  expect_identical(stage1_near_cap(exp(-rep(5, 8)))[c("within_margin", "within_near_cap")], list(within_margin = 0L, within_near_cap = 0L))
  expect_error(stage1_near_cap(numeric(0)), "stage 1: phi_f_inv has no draws")
  expect_error(stage1_near_cap(c(0.1, NA)), "stage 1: phi_f_inv has no draws, or a missing or non-positive one")
  expect_error(stage1_near_cap(c(0.1, 0)), "non-positive")
})

test_that("a stage-1 record holds its near-cap counts only when they are the current code's", {
  read <- function(rec) stage1_near_cap_of(jsonlite::read_json(round_trip(rec)))
  expect_identical(read(stage1_record()), list(n_draws = 4000L, within_margin = 6L, within_near_cap = 2L))
  # WHY: a record written before the counts were recorded is stale.
  expect_null(read(stage1_record(near = NULL)))
  near <- NEAR_RECORD
  stale <- list(
    list(margin = 1.5), list(near_cap = 0.25), list(cap = 22), list(n_draws = 400L), list(within_margin = 1L),
    list(within_near_cap = -1L), list(within_margin = 6.5), list(within_margin = 4001L, n_draws = 4000L)
  )
  for (edit in stale) expect_null(read(stage1_record(near = utils::modifyList(near, edit))), label = paste(names(edit), unlist(edit)))
  expect_null(read(stage1_record(near = near[names(near) != "within_near_cap"])))
  expect_null(stage1_near_cap_of(list()))
  expect_null(stage1_near_cap_of("not a record"))

  dir <- withr::local_tempdir()
  path <- file.path(dir, "mu_f.json")
  expect_false(stage1_current(path))
  round_trip(stage1_record(near = NULL), path)
  expect_false(stage1_current(path))
  round_trip(stage1_record(), path)
  expect_true(stage1_current(path))
})

test_that("a stale stage-1 record is refitted, or with fit = FALSE read as a cold cache, and a current one read without a compile", {
  skip_without_snapshot()
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  # WHY: a fit would then fail with "compiled", so a read that returns shows no fit ran.
  local_mocked_bindings(cmdstan_model = function(...) stop("compiled"), .package = "cmdstanr")
  info <- info_set(as.Date("2021-09-25"), as.Date("2021-03-29"))
  cfg <- cfg_m0()
  # The key, as stage1_mu_f() computes it.
  data <- stage1_data(info)
  attributes(data) <- list(names = names(data))
  json <- withr::local_tempfile(fileext = ".json")
  cmdstanr::write_stan_json(data, json)
  S_f <- info$fundamentals$S_f
  key <- stage1_key(sha256_file(file.path(project_root(), STAGE1_STAN)), sha256_file(json), cfg$stage1, S_f)
  path <- file.path(results, "stage1", key, "mu_f.json")
  dir.create(dirname(path), recursive = TRUE)

  round_trip(stage1_record(key, near = NULL, parties = S_f), path)
  expect_error(
    stage1_mu_f(info, cfg, fit = FALSE),
    "the cached record has no current log phi_f near-cap counts, so it is stale", fixed = TRUE
  )
  expect_error(stage1_mu_f(info, cfg, fit = FALSE), "stage 1: no cached fit for 2021-09-25 F 2021-03-29", fixed = TRUE)
  expect_error(stage1_mu_f(info, cfg), "compiled", fixed = TRUE)
  round_trip(stage1_record(key, parties = S_f), path)
  s1 <- stage1_mu_f(info, cfg, fit = FALSE)
  expect_identical(s1$sha256, key)
  expect_identical(stage1_mu_f(info, cfg), s1)
  expect_identical(s1$diagnostics$log_phi_f_near_cap$within_margin, 6L)
})


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
  path <- file.path(Sys.getenv("HARNESS_RESULTS"), "stage1", a$sha256, "mu_f.json")
  expect_true(file.exists(path))
  # WHY: the fit records Decision 4's near-cap counts of log phi_f over its chains x iter_sampling draws.
  near <- stage1_near_cap_of(jsonlite::read_json(path))
  expect_identical(near$n_draws, 4L * cfg$stage1$iter_sampling)
  expect_true(near$within_near_cap <= near$within_margin && near$within_margin <= near$n_draws)
  expect_true(stage1_current(path))
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
test_that("stage 1 read from its cache only stops on a cold cache, compiling, fitting and writing nothing", {
  skip_without_snapshot()
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  # WHY: a cache-only read that fell through to the fit would then fail with "compiled", not pass with the right message.
  local_mocked_bindings(cmdstan_model = function(...) stop("compiled"), .package = "cmdstanr")
  out <- capture.output(expect_error(
    stage1_mu_f(info_set(as.Date("2021-09-25"), as.Date("2021-03-29")), cfg_m0(), fit = FALSE),
    "stage 1: no cached fit for 2021-09-25 F 2021-03-29 under results_root()/stage1/",
    fixed = TRUE
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
