# Simulation-based recovery for arm V2 (R/harness/recovery.R). Its redraw rule, ranks, coverage
# checks, folder states and report are tested with the file loaded as a module, so its script
# part does not run, on made-up data and replicate folders. One replicate runs end to end as a
# subprocess into a temporary results root, never the real one: slow.
box::use(
  R / harness / recovery[
    attempt_seed, redraw_reasons, real_rule_text, rank_elements, group_checks, incomplete_reasons, recovery_verdict, replicate_state,
    take_lock, truth_check, GROUPS, DESIGNS, SCALAR_TRUTHS
  ],
  R / harness / freeze[is_frozen],
  R / harness / runs[sha256_file, stan_data_sha, results_root]
)

# Decision 7's pre-registered seeds, in DESIGNS's order.
design_seeds <- function() unlist(lapply(seq_len(nrow(DESIGNS)), \(i) DESIGNS$first_seed[i] + seq_len(DESIGNS$n[i]) - 1L))

# The module's namespace, for run_report().
rec <- environment(attempt_seed)

# The script's output lines, with attribute `status` when it exits non-zero, under results root
# `results`.
run_recovery <- function(args, results) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/recovery.R", args),
    stdout = TRUE, stderr = TRUE, env = c("LC_ALL=en_US.UTF-8", paste0("HARNESS_RESULTS=", results))
  )))
}

shown <- function(out) paste(out, collapse = "\n")

# A data list with what the redraw rule reads: one closed election in two kjordaemi and four
# national polls in two epochs, three parties; party 3 is reported nowhere but in the last poll.
rule_data <- function() {
  y_res <- array(0L, c(1, 2, 3))
  y_res[1, 1, ] <- c(500L, 500L, 0L)
  y_res[1, 2, ] <- c(600L, 400L, 0L)
  list(
    n_closed = 1L, K = 2L, P = 3L, idx_res = matrix(c(1L, 2L, 0L), 1), n_rep_res = 2L, y_res = y_res,
    grid_epoch = c(1L, 1L, 2L, 2L), grid_nat = 1:4,
    y_nat = rbind(c(50L, 50L, 0L), c(60L, 40L, 0L), c(70L, 30L, 0L), c(50L, 49L, 1L)),
    idx_nat = rbind(c(1L, 2L, 0L), c(1L, 2L, 0L), c(1L, 2L, 0L), c(1L, 2L, 3L)), n_rep_nat = c(2L, 2L, 2L, 3L)
  )
}

# The same data with the closed election's result reporting party 3 as well.
reported3 <- function(d) {
  d$idx_res[1, ] <- 1:3
  d$n_rep_res[1] <- 3L
  d
}

test_that("each attempt of each replicate draws with its own seed, within R's integers", {
  expect_identical(attempt_seed(3001, 1), 300100001L)
  expect_identical(attempt_seed(3110, 99999), 311099999L)
  expect_lt(attempt_seed(3001, 99999), attempt_seed(3002, 1))
  expect_error(attempt_seed(3001, 100000), "an attempt must be")
  expect_error(attempt_seed(0, 1), "seed must be")
  expect_error(attempt_seed(21474, 1), "seed must be")
  expect_lte(as.numeric(attempt_seed(21473, 99999)), .Machine$integer.max)
})

test_that("the redraw rule reads each closed election's national result and each epoch's mean poll share of the reported parties", {
  d <- rule_data()
  expect_identical(redraw_reasons(d), character(0))
  # Below 0.5% (here 0 votes) in one kjordaemi but 45% nationally: kept, since a single kjordaemi's share is not read.
  kd_low <- d
  kd_low$y_res[1, 1, ] <- c(100L, 900L, 0L)
  kd_low$y_res[1, 2, ] <- c(1000L, 0L, 0L)
  expect_identical(redraw_reasons(kd_low), character(0))
  # Above 70% in one kjordaemi but 60% nationally: kept.
  kd_high <- d
  kd_high$y_res[1, 1, ] <- c(800L, 200L, 0L)
  kd_high$y_res[1, 2, ] <- c(400L, 600L, 0L)
  expect_identical(redraw_reasons(kd_high), character(0))
  # The national share is the kjordaemi counts summed. With party 3 reported too, its 9 of 2,000 votes (0.45%)
  # is below 0.5% though no other party leaves the range; 10 of 2,000 is at it.
  low <- reported3(d)
  low$y_res[1, 1, ] <- c(500L, 495L, 5L)
  low$y_res[1, 2, ] <- c(600L, 396L, 4L)
  expect_identical(redraw_reasons(low), "result 1")
  low$y_res[1, 2, ] <- c(600L, 395L, 5L)
  expect_identical(redraw_reasons(low), character(0))
  # 1,400 of 2,000 is at 70%; 1,401 is above it.
  high <- d
  high$y_res[1, 1, ] <- c(900L, 100L, 0L)
  high$y_res[1, 2, ] <- c(500L, 500L, 0L)
  expect_identical(redraw_reasons(high), character(0))
  high$y_res[1, 1, ] <- c(901L, 99L, 0L)
  expect_identical(redraw_reasons(high), "result 1")
  # Both halves of the rule at once, results first.
  both <- low
  both$y_res[1, 2, ] <- c(600L, 396L, 4L)
  both$y_nat[4, ] <- c(50L, 50L, 0L)
  expect_identical(redraw_reasons(both), c("result 1", "national polls epoch 2"))
  # Party 3's mean over the one poll reporting it is 1%: kept. At 0 votes in that poll it is below the range.
  zero <- d
  zero$y_nat[4, ] <- c(50L, 50L, 0L)
  expect_identical(redraw_reasons(zero), "national polls epoch 2")
  # One poll at 72% averages with one at 60% to 66% in epoch 1: kept; both above 70%, not.
  ok <- d
  ok$y_nat[1, ] <- c(72L, 28L, 0L)
  expect_identical(redraw_reasons(ok), character(0))
  ok$y_nat[2, ] <- c(71L, 29L, 0L)
  expect_identical(redraw_reasons(ok), "national polls epoch 1")
  # An unreported party's count is not read: the rows report parties 1 and 2 only.
  hidden <- d
  hidden$y_res[1, 1, ] <- c(499L, 499L, 2L)
  expect_identical(redraw_reasons(hidden), character(0))
})

test_that("the real cell's reasons give each failing result's election, party and national share", {
  meta <- list(party_order = c("A", "B", "C"), closed_elections = "2021-09-25")
  d <- reported3(rule_data())
  d$y_res[1, 1, ] <- c(500L, 495L, 5L)
  d$y_res[1, 2, ] <- c(600L, 395L, 5L)
  expect_identical(real_rule_text(d, meta), character(0))
  d$y_res[1, 2, ] <- c(600L, 396L, 4L)
  d$y_nat[4, ] <- c(50L, 50L, 0L)
  expect_identical(real_rule_text(d, meta), c("result 1 (2021-09-25): C 0.450%", "national polls epoch 2"))
  # Every party outside the range is named: two reported parties at 99.55% and 0.45%.
  two <- rule_data()
  two$y_res[1, 1, ] <- c(995L, 5L, 0L)
  two$y_res[1, 2, ] <- c(1000L, 4L, 0L)
  expect_identical(real_rule_text(two, meta), "result 1 (2021-09-25): A 99.551%, B 0.449%")
})

test_that("the redraw rule gathers each row's reported parties by identity, not by position", {
  # Party 2 is reported in no result and no poll here, an interior party dropped: a positional
  # gather (the first n_rep columns) would read its 0 votes as parties 1 and 2's and redraw.
  d <- rule_data()
  d$idx_res[1, ] <- c(1L, 3L, 0L)
  d$y_res[1, 1, ] <- c(500L, 0L, 500L)
  d$y_res[1, 2, ] <- c(400L, 0L, 600L)
  d$idx_nat <- rbind(c(1L, 3L, 0L), c(1L, 3L, 0L), c(3L, 1L, 0L), c(1L, 3L, 0L))
  d$y_nat <- rbind(c(50L, 0L, 50L), c(60L, 0L, 40L), c(30L, 0L, 70L), c(45L, 0L, 55L))
  d$n_rep_nat <- rep(2L, 4)
  expect_identical(redraw_reasons(d), character(0))
  # By identity the rule still sees party 3: at 9 of 2,000 votes (0.45%) its result is redrawn,
  # and the party named is C, not the B a positional gather would name.
  low <- d
  low$y_res[1, 1, ] <- c(995L, 0L, 5L)
  low$y_res[1, 2, ] <- c(996L, 0L, 4L)
  meta <- list(party_order = c("A", "B", "C"), closed_elections = "2021-09-25")
  expect_identical(real_rule_text(low, meta), "result 1 (2021-09-25): A 99.550%, C 0.450%")
  # The polls' row order (3, 1 in poll 3) does not matter either: party 3 at 0 in poll 4 alone
  # leaves epoch 2's mean at (70 + 0) / 2 = 35%, kept; at 0 in both of epoch 2's polls it is
  # below the range.
  zero <- d
  zero$y_nat[4, ] <- c(100L, 0L, 0L)
  expect_identical(redraw_reasons(zero), character(0))
  zero$y_nat[3, ] <- c(100L, 0L, 0L)
  expect_identical(redraw_reasons(zero), "national polls epoch 2")
})

test_that("the truth check pairs each truth with the Stan value of its name and names an element that differs", {
  elements <- data.frame(
    group = c("tau_mu", "s_bar_E", "mu_open", "delta_closed", "delta_open"),
    variable = c("tau_mu[1]", "s_bar_E[1]", "mu[2,1]", "delta[1,2,3]", "delta[2,1,1]"), truth = c(0.3, 0.8, -0.5, 0.75, -2.5)
  )
  got <- stats::setNames(elements$truth, elements$variable)[c(5, 3, 1, 4, 2)]
  expect_identical(truth_check(got, elements), 0)
  # Within 1e-8 of the truth relative to max(1, |truth|): kept.
  near <- got
  near["delta[2,1,1]"] <- near["delta[2,1,1]"] * (1 + 1e-9)
  expect_lt(truth_check(near, elements), 1e-8)
  # One delta element off by 1e-6: stopped, that element named.
  off <- got
  off["delta[1,2,3]"] <- off["delta[1,2,3]"] + 1e-6
  expect_error(truth_check(off, elements, "rep"), "rep: the Stan model's quantities at the true parameters differ from the truth by more than 1e-08: delta\\[1,2,3\\] by 1e-06")
  # Two off: both named, the larger first; the others are not.
  off["mu[2,1]"] <- off["mu[2,1]"] + 1e-4
  err <- tryCatch(truth_check(off, elements), error = conditionMessage)
  expect_match(err, ": mu\\[2,1\\] by 1e-04, delta\\[1,2,3\\] by 1e-06$")
  expect_no_match(err, "tau_mu|s_bar_E|delta\\[2,1,1\\]")
  # A missing element and a non-finite value are errors, never a pass.
  expect_error(truth_check(got[-1], elements), "lacks delta\\[2,1,1\\]")
  bad <- got
  bad["tau_mu[1]"] <- NaN
  expect_error(truth_check(bad, elements), "non-finite")
})

test_that("a truth is inside the 90% interval for q in [0.05, 0.95] and the 50% for q in [0.25, 0.75]", {
  draws <- matrix(rep(1:20, 2), 20, 2, dimnames = list(NULL, c("a[1]", "b[1]")))
  truths <- c(1, 1.5, 5.5, 15.5, 16.5, 19.5, 20.5)
  for (i in seq_along(truths)) {
    r <- rank_elements(draws, data.frame(group = "tau_mu", variable = "a[1]", truth = truths[i]))
    expect_equal(r$q, mean(1:20 < truths[i]))
  }
  q <- function(t) rank_elements(draws, data.frame(group = "tau_mu", variable = "a[1]", truth = t))
  expect_identical(unlist(q(1)[c("in50", "in90")]), c(in50 = FALSE, in90 = FALSE))
  expect_identical(unlist(q(1.5)[c("in50", "in90")]), c(in50 = FALSE, in90 = TRUE))
  expect_identical(unlist(q(5.5)[c("in50", "in90")]), c(in50 = TRUE, in90 = TRUE))
  expect_identical(unlist(q(15.5)[c("in50", "in90")]), c(in50 = TRUE, in90 = TRUE))
  expect_identical(unlist(q(16.5)[c("in50", "in90")]), c(in50 = FALSE, in90 = TRUE))
  expect_identical(unlist(q(19.5)[c("in50", "in90")]), c(in50 = FALSE, in90 = TRUE))
  expect_identical(unlist(q(20.5)[c("in50", "in90")]), c(in50 = FALSE, in90 = FALSE))
  expect_error(rank_elements(draws, data.frame(group = "x", variable = "c[1]", truth = 1)), "the draws lack c\\[1\\]")
})

test_that("each truth is ranked against the draws column of its own name, whatever the order of columns or elements", {
  set.seed(7)
  draws <- cbind(`a[1]` = rnorm(400), `b[2,1]` = rnorm(400, 5), `c[1,1,1]` = rnorm(400, -5))
  elements <- data.frame(group = "x", variable = c("c[1,1,1]", "a[1]", "b[2,1]"), truth = c(-5.2, 0.4, 6))
  r <- rank_elements(draws, elements)
  expect_equal(r$q, c(mean(draws[, 3] < -5.2), mean(draws[, 1] < 0.4), mean(draws[, 2] < 6)))
  # The draws' columns shuffled: every q unchanged.
  expect_identical(rank_elements(draws[, c(2, 3, 1)], elements)$q, r$q)
  # The elements in another order: each keeps its q.
  shuffled <- rank_elements(draws, elements[c(3, 1, 2), ])
  expect_identical(shuffled$q[match(r$variable, shuffled$variable)], r$q)
})

# Ranks of `n` replicates: in each scalar group, the first `in90` replicates inside the 90%
# interval and the first `in50` inside the 50%; in each vector group, replicate i covers
# `share90[i]` and `share50[i]` of its 10 elements.
made_ranks <- function(n = 30, in90 = 27, in50 = 15, share90 = rep(0.9, n), share50 = rep(0.5, n)) {
  rows <- list()
  for (i in seq_len(n)) {
    id <- paste0("r", i)
    for (g in GROUPS$group[GROUPS$kind == "scalar"]) {
      rows[[length(rows) + 1]] <- data.frame(replicate = id, group = g, in50 = i <= in50, in90 = i <= in90)
    }
    for (g in GROUPS$group[GROUPS$kind == "vector"]) {
      rows[[length(rows) + 1]] <- data.frame(
        replicate = id, group = g, in50 = seq_len(10) <= round(10 * share50[i]), in90 = seq_len(10) <= round(10 * share90[i])
      )
    }
  }
  do.call(rbind, rows)
}

check_of <- function(checks, group, interval) checks[checks$group == group & checks$interval == interval, ]

test_that("a scalar group passes with 22 or more of 30 truths inside the 90% interval and 8 to 22 inside the 50%", {
  expect_identical(SCALAR_TRUTHS, 30L)
  for (case in list(c(22, 8, TRUE, TRUE), c(21, 8, FALSE, TRUE), c(30, 22, TRUE, TRUE), c(30, 23, TRUE, FALSE), c(25, 7, TRUE, FALSE))) {
    ch <- group_checks(made_ranks(in90 = case[1], in50 = case[2]))
    expect_identical(check_of(ch, "tau_mu", "90")$value, case[1])
    expect_identical(check_of(ch, "tau_mu", "90")$pass, as.logical(case[3]))
    expect_identical(check_of(ch, "s_bar_E", "50")$pass, as.logical(case[4]))
  }
  # The band is Decision 7's for 30 truths: another count of replicates has no verdict.
  ch <- group_checks(made_ranks(n = 29, in90 = 29, in50 = 15))
  expect_identical(check_of(ch, "tau_mu", "90")$n, 29L)
  expect_true(is.na(check_of(ch, "tau_mu", "90")$pass))
})

test_that("a vector group passes when its mean share over replicates lies within 3 standard errors of 0.9 and of 0.5", {
  share90 <- rep(c(0.8, 1), 15)
  share50 <- rep(c(0.4, 0.6), 15)
  ch <- group_checks(made_ranks(share90 = share90, share50 = share50))
  r <- check_of(ch, "delta_open", "90")
  expect_equal(r$value, 0.9)
  expect_equal(r$se, stats::sd(share90) / sqrt(30))
  expect_equal(c(r$lo, r$hi), 0.9 + c(-3, 3) * r$se)
  expect_true(r$pass)
  # Shifted down by 4 standard errors, the mean fails.
  low <- share90 - 0.1
  ch <- group_checks(made_ranks(share90 = low, share50 = share50))
  r <- check_of(ch, "mu_open", "90")
  expect_equal(r$value, 0.8)
  expect_false(r$pass)
  expect_true(check_of(ch, "mu_open", "50")$pass)
  # One replicate has no standard error, so no verdict.
  expect_true(is.na(check_of(group_checks(made_ranks(n = 1)), "delta_closed", "90")$pass))
})

test_that("a vector group's coverage is the mean over replicates of each one's share, not the share of all its elements", {
  # Two designs with different element counts: two replicates of 10 elements, all inside the
  # 90% interval, and two of 2, none inside. Per replicate: (1 + 1 + 0 + 0) / 4 = 0.5; pooled
  # over the elements it would be 20 / 24.
  ranks <- do.call(rbind, lapply(c("a1", "a2", "b1", "b2"), \(id) {
    m <- if (startsWith(id, "a")) 10 else 2
    data.frame(replicate = id, group = "mu_open", in50 = FALSE, in90 = startsWith(id, "a") & seq_len(m) > 0)
  }))
  r <- check_of(group_checks(ranks), "mu_open", "90")
  expect_identical(r$n, 4L)
  expect_equal(r$value, 0.5)
  expect_equal(r$se, stats::sd(c(1, 1, 0, 0)) / 2)
})

test_that("a vector group's verdict turns at 3 standard errors", {
  # Ten replicates of 10 elements. Inside: mean 0.77, 3 SE 0.1345 (0.13 off 0.9, 0.97 of 3 SE);
  # outside: mean 0.75, 3 SE 0.1432 (0.15 off 0.9, 1.05 of 3 SE).
  inside <- c(5, 6, 7, 8, 8, 8, 8, 8, 9, 10) / 10
  outside <- c(5, 6, 7, 7, 7, 7, 8, 9, 9, 10) / 10
  r <- check_of(group_checks(made_ranks(n = 10, share90 = inside, share50 = rep(0.5, 10))), "delta_closed", "90")
  expect_equal(r$value, 0.77)
  expect_true(abs(r$value - 0.9) < 3 * r$se && abs(r$value - 0.9) > 0.95 * 3 * r$se)
  expect_true(r$pass)
  r <- check_of(group_checks(made_ranks(n = 10, share90 = outside, share50 = rep(0.5, 10))), "delta_closed", "90")
  expect_equal(r$value, 0.75)
  expect_true(abs(r$value - 0.9) > 3 * r$se && abs(r$value - 0.9) < 1.05 * 3 * r$se)
  expect_false(r$pass)
})

test_that("only Decision 7's set of replicates, its pre-registered seeds at 400 + 400, gets a PASS or a FAIL", {
  expect_identical(DESIGNS$first_seed, c(3001L, 3101L))
  set <- data.frame(cell = rep(DESIGNS$cell, DESIGNS$n), seed = design_seeds(), iter_warmup = 400L, iter_sampling = 400L)
  expect_identical(set$seed, c(3001:3020, 3101:3110))
  expect_identical(incomplete_reasons(set), character(0))
  checks <- group_checks(made_ranks())
  expect_identical(recovery_verdict(checks, incomplete_reasons(set)), "PASS")
  checks$pass[3] <- FALSE
  expect_identical(recovery_verdict(checks, incomplete_reasons(set)), "FAIL")
  expect_identical(incomplete_reasons(set[-1, ]), c("2024 eve: 19 of 20 replicates", "2024 eve: seed(s) 3001 missing"))
  # A substituted seed keeps the count but not the set.
  swapped <- set
  swapped$seed[20] <- 3021L
  expect_identical(incomplete_reasons(swapped), c("2024 eve: seed(s) 3020 missing", "2024 eve: seed(s) 3021 outside 3001\u20133020"))
  expect_identical(recovery_verdict(checks, incomplete_reasons(swapped)), "INCOMPLETE")
  # Extra and missing seeds are given as runs.
  gaps <- set[!set$seed %in% c(3005:3007, 3010), ]
  expect_match(incomplete_reasons(gaps), "2024 eve: seed(s) 3005\u20133007, 3010 missing", fixed = TRUE, all = FALSE)
  short <- set
  short$iter_sampling[30] <- 20L
  expect_identical(incomplete_reasons(short), "2021 f180: 1 replicates not at 400 + 400")
  other <- rbind(set, data.frame(cell = "2017-10-28_2017-10-27", seed = 3001L, iter_warmup = 400L, iter_sampling = 400L))
  expect_identical(incomplete_reasons(other), "replicates on another cell: 2017-10-28_2017-10-27")
  expect_identical(recovery_verdict(checks, incomplete_reasons(short)), "INCOMPLETE")
})

test_that("a replicate's folder is reused when its meta.json matches, retired when it does not or has none, and left alone while another process holds it", {
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  cell <- file.path(results, "recovery", "2021-09-25_2021-03-29")
  expected <- list(stan_sha256 = "a", seed = 3101L, iter_sampling = 20L)
  dir <- file.path(cell, "3101")

  s <- replicate_state(dir, expected, "recovery_x_s3101")
  expect_identical(s$state, "new")
  expect_identical(Sys.readlink(s$owner), as.character(Sys.getpid()))
  dir.create(dir)
  jsonlite::write_json(expected, file.path(dir, "meta.json"), auto_unbox = TRUE)
  rec$release_lock(s$owner)
  expect_true(is.na(Sys.readlink(s$owner)))

  expect_identical(replicate_state(dir, expected, "recovery_x_s3101")$state, "skip")
  expect_true(is.na(Sys.readlink(file.path(cell, ".3101.owner"))))

  s <- replicate_state(dir, utils::modifyList(expected, list(iter_sampling = 400L)), "recovery_x_s3101")
  expect_identical(s$state, "stale")
  expect_identical(s$keys, "iter_sampling")
  expect_false(dir.exists(dir))
  expect_true(file.exists(file.path(s$to, "meta.json")))
  expect_match(basename(s$to), "^recovery_x_s3101_[0-9]{14}$")
  expect_identical(dirname(s$to), file.path(results, "_stale"))
  rec$release_lock(s$owner)

  dir.create(dir)
  writeLines("x", file.path(dir, "truth.json"))
  s <- replicate_state(dir, expected, "recovery_y_s3101")
  expect_identical(s$state, "partial")
  expect_null(s$error)
  expect_false(dir.exists(dir))
  rec$release_lock(s$owner)

  # A meta.json that does not parse is neither finished nor absent: retired as partial, its
  # parser's message kept for the log.
  dir.create(dir)
  writeLines('{"stan_sha256": "a", "seed": 31', file.path(dir, "meta.json"))
  s <- replicate_state(dir, expected, "recovery_w_s3101")
  expect_identical(s$state, "partial")
  expect_true(is.character(s$error) && nzchar(s$error))
  expect_false(dir.exists(dir))
  expect_true(file.exists(file.path(s$to, "meta.json")))
  rec$release_lock(s$owner)

  # A claim whose process has ended is taken over; one whose process runs is not.
  file.symlink("999999", file.path(cell, ".3101.owner"))
  s <- replicate_state(dir, expected, "recovery_z_s3101")
  expect_identical(s$state, "new")
  rec$release_lock(s$owner)
  p <- processx::process$new("sleep", "30")
  withr::defer(p$kill())
  file.symlink(as.character(p$get_pid()), file.path(cell, ".3101.owner"))
  s <- replicate_state(dir, expected, "recovery_z_s3101")
  expect_identical(s$state, "busy")
  expect_identical(s$pid, p$get_pid())
  expect_identical(take_lock(file.path(cell, ".3101.owner")), p$get_pid())
})

# A finished replicate folder under `base` with made-up outputs: its meta.json (the keys the
# report and the stop rule read), ranks.csv from the rows of `ranks` whose replicate is `replicate`, a
# key_diagnostics.csv with an lp__ row and a param_draws.parquet of 4 x 400 draws far from the caps;
# `real` is the real cell's redraw reasons it records, `version` its recovery_version.
fake_replicate <- function(base, cell, seed, ranks, stan_sha = "s", replicate = paste0("r", seed), real = character(0),
                           version = rec$RECOVERY_VERSION) {
  id <- sprintf("recovery_%s_s%d", cell, seed)
  dir <- file.path(base, "recovery", cell, seed)
  dir.create(dir, recursive = TRUE)
  meta <- list(
    id = id, seed = seed, iter_warmup = 400L, iter_sampling = 400L, chains = 4L, minutes = 1.5, attempts = 3L,
    stan_sha256 = stan_sha, config_sha256 = "c", design_sha256 = paste0("d", cell), recovery_version = version,
    git_head = "abc", git_dirty = FALSE, git_head_end = "abc", git_dirty_end = FALSE,
    cmdstan_version = "2.37.0", real_cell_redraw = I(real),
    diagnostics = list(divergent = 0L, max_rhat = 1.001, treedepth_hits = 0L, min_ess_bulk = 500, n_leapfrog = 1e5)
  )
  jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, digits = NA)
  mine <- ranks[ranks$replicate == replicate, ]
  readr::write_csv(
    data.frame(
      group = mine$group, variable = paste0(mine$group, "[", seq_len(nrow(mine)), "]"), epoch = NA_integer_, kjordaemi = NA,
      flokkur = NA, truth = 0, q = 0.5, in50 = mine$in50, in90 = mine$in90
    ),
    file.path(dir, "ranks.csv")
  )
  readr::write_csv(data.frame(variable = c("tau_mu[1]", "lp__"), rhat = 1.001, ess_bulk = 500, ess_tail = 500), file.path(dir, "key_diagnostics.csv"))
  arrow::write_parquet(
    data.frame(.chain = rep(1:4, each = 400), .iteration = rep(1:400, 4), .draw = 1:1600, `log_phi[1]` = 7, log_phi_K = 7, log_kappa = 0, check.names = FALSE),
    file.path(dir, "param_draws.parquet")
  )
  dir
}

test_that("the report gives Decision 7's set its verdict, counts a replicate that trips the stop rule, and refuses a replicate of another Stan file or recovery version", {
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  ranks <- made_ranks(share90 = rep(c(0.8, 1), 15), share50 = rep(c(0.4, 0.6), 15))
  seeds <- design_seeds()
  ranks$replicate <- sub("^r", "", ranks$replicate)
  ranks$replicate <- paste0("r", seeds[as.integer(ranks$replicate)])
  real <- "result 3 (2021-09-25): X 0.495%"
  for (i in 1:20) fake_replicate(results, DESIGNS$cell[1], seeds[i], ranks, real = real)
  for (i in 21:30) fake_replicate(results, DESIGNS$cell[2], seeds[i], ranks)
  # Replicate 3's lp__ R-hat trips the stop rule; it stays in every count.
  tripped <- file.path(results, "recovery", DESIGNS$cell[1], "3003", "key_diagnostics.csv")
  readr::write_csv(data.frame(variable = c("tau_mu[1]", "lp__"), rhat = c(1.001, 1.5), ess_bulk = 500, ess_tail = 500), tripped)
  out <- withr::local_tempfile(fileext = ".md")
  printed <- utils::capture.output(status <- rec$run_report(out))
  expect_identical(status, 0L)
  expect_identical(utils::tail(printed, 2)[1], "RECOVERY PASS")
  expect_true("COVERAGE tau_mu 90% 27/30 band [22, 30] PASS" %in% printed, label = shown(printed))
  md <- readLines(out, encoding = "UTF-8")
  expect_identical(utils::tail(md, 1), "RECOVERY PASS")
  expect_true(any(startsWith(md, "STOP recovery_2024-11-30_2024-11-29_s3003 ")))
  expect_identical(sum(startsWith(md, "GO recovery_")), 29L)
  expect_true("| 2024 eve | 20 | 3001\u20133020 | 400 + 400 | 60 | 40 | 0.6667 | 1.5, 1.5 | 1 | redrawn: result 3 (2021-09-25): X 0.495% |" %in% md, label = shown(md))
  expect_true("| 2021 f180 | 10 | 3101\u20133110 | 400 + 400 | 30 | 20 | 0.6667 | 1.5, 1.5 | 0 | kept |" %in% md, label = shown(md))
  expect_true("- Commit(s) at lane start: `abc`" %in% md, label = shown(md))
  expect_true("- Replicates fitted from a tree with changes (at lane start): 0" %in% md, label = shown(md))
  expect_true("- Replicates whose commit or tree changed while their lane ran: none" %in% md, label = shown(md))
  expect_true("REAL 2024 eve under the redraw rule: redrawn: result 3 (2021-09-25): X 0.495%" %in% printed, label = shown(printed))
  expect_true("REAL 2021 f180 under the redraw rule: kept" %in% printed, label = shown(printed))
  # Every table row is one physical line.
  expect_true(all(endsWith(md[startsWith(md, "|")], "|")))

  # A replicate whose commit changed while its lane ran, and one fitted from a tree with changes,
  # are listed; neither changes the verdict.
  meta_of <- function(seed) file.path(results, "recovery", DESIGNS$cell[1], seed, "meta.json")
  m <- jsonlite::read_json(meta_of(3004), simplifyVector = TRUE)
  m$git_head_end <- "def"
  jsonlite::write_json(m, meta_of(3004), auto_unbox = TRUE, digits = NA)
  m <- jsonlite::read_json(meta_of(3005), simplifyVector = TRUE)
  m$git_dirty <- TRUE
  m$git_dirty_end <- TRUE
  jsonlite::write_json(m, meta_of(3005), auto_unbox = TRUE, digits = NA)
  printed <- utils::capture.output(rec$run_report(out))
  expect_true("RECOVERY PASS" %in% printed)
  md <- readLines(out, encoding = "UTF-8")
  expect_true("- Replicates fitted from a tree with changes (at lane start): 1: recovery_2024-11-30_2024-11-29_s3005" %in% md, label = shown(md))
  expect_true(
    "- Replicates whose commit or tree changed while their lane ran: 1: recovery_2024-11-30_2024-11-29_s3004 (git_head abc to def, git_dirty FALSE to FALSE)" %in% md,
    label = shown(md)
  )

  # A coverage failure in one group is a FAIL.
  for (seed in 3001:3010) {
    path <- file.path(results, "recovery", DESIGNS$cell[1], seed, "ranks.csv")
    r <- readr::read_csv(path, show_col_types = FALSE)
    r$in90[r$group == "tau_mu"] <- FALSE
    readr::write_csv(r, path)
  }
  printed <- utils::capture.output(rec$run_report(out))
  expect_true("RECOVERY FAIL" %in% printed)
  expect_true("COVERAGE tau_mu 90% 17/30 band [22, 30] FAIL" %in% printed, label = shown(printed))

  # Another Stan file's replicate is refused by name, and so is one of another recovery version.
  odd <- fake_replicate(results, DESIGNS$cell[2], 200L, ranks, stan_sha = "t", replicate = "r3101")
  expect_error(rec$run_report(out), "recovery_2021-09-25_2021-03-29_s200 have another Stan file sha256")
  unlink(odd, recursive = TRUE)
  odd <- fake_replicate(results, DESIGNS$cell[2], 200L, ranks, replicate = "r3101", version = rec$RECOVERY_VERSION - 1L)
  expect_error(
    rec$run_report(out),
    paste0("recovery_2021-09-25_2021-03-29_s200 were drawn under recovery_version ", rec$RECOVERY_VERSION - 1L, ", not the current ", rec$RECOVERY_VERSION),
    fixed = TRUE
  )
  unlink(odd, recursive = TRUE)
  # A meta.json that does not parse stops the report, naming its folder.
  odd <- fake_replicate(results, DESIGNS$cell[2], 200L, ranks, replicate = "r3101")
  writeLines("{", file.path(odd, "meta.json"))
  expect_error(rec$run_report(out), "2021-09-25_2021-03-29/200 does not parse", fixed = TRUE)
  unlink(odd, recursive = TRUE)
  # A set short of Decision 7's is INCOMPLETE, never a PASS, and names the seed missing.
  unlink(file.path(results, "recovery", DESIGNS$cell[1], "3020"), recursive = TRUE)
  printed <- utils::capture.output(rec$run_report(out))
  expect_true("RECOVERY INCOMPLETE" %in% printed)
  expect_true("incomplete: 2024 eve: 19 of 20 replicates; 2024 eve: seed(s) 3020 missing" %in% printed, label = shown(printed))
  # A design with no replicate yet still gets its row and its real-cell line.
  for (seed in 3001:3019) unlink(file.path(results, "recovery", DESIGNS$cell[1], seed), recursive = TRUE)
  printed <- utils::capture.output(rec$run_report(out))
  md <- readLines(out, encoding = "UTF-8")
  expect_true("REAL 2024 eve under the redraw rule: not recorded (no replicate yet)" %in% printed, label = shown(printed))
  expect_true("REAL 2021 f180 under the redraw rule: kept" %in% printed, label = shown(printed))
  expect_true("| 2024 eve | 0 | \u2013 | \u2013 | 0 | 0 | \u2013 | \u2013 | 0 | not recorded (no replicate yet) |" %in% md, label = shown(md))
  expect_true("| Group | 2021 f180 |" %in% md, label = shown(md))
  expect_true(all(endsWith(md[startsWith(md, "|")], "|")))
  expect_true("RECOVERY INCOMPLETE" %in% printed)
})

test_that("the script refuses a bad command line and a 2017 cell before the freeze, before anything is written", {
  skip_if(is_frozen(), "the freeze is sealed")
  # WHY: with no CmdStan to find, a refusal that regressed past the 2017 check stops at its first CmdStan call and can fit nothing.
  local_no_cmdstan()
  results <- withr::local_tempdir()
  out <- run_recovery(c("2021-09-25", "2021-03-29", "3101", "1", "20"), results)
  expect_identical(attr(out, "status"), 1L)
  expect_match(out, "recovery: usage:", fixed = TRUE, all = FALSE)
  out <- run_recovery(c("2021-09-25", "2021-03-29", "3101", "1", "20", "20", "--lane=C"), results)
  expect_identical(attr(out, "status"), 1L)
  out <- run_recovery(c("2017-10-28", "2017-10-27", "3101", "1", "20", "20"), results)
  expect_identical(attr(out, "status"), 1L)
  expect_match(out, "recovery: 2017 is not fitted before the freeze", fixed = TRUE, all = FALSE)
  expect_length(list.files(results, recursive = TRUE, all.files = TRUE), 0)
})

test_that("one replicate at 20 + 20 on 2021 f180 runs end to end: its counts and mu_f simulated, its design the real cell's, one rank per truth", {
  skip_unless_slow()
  skip_without_snapshot()
  box::use(R / harness / v2[read_v2_config, v2_stan_data], R / harness / stage1[stage1_mu_f])
  # The real cell's stage 1 is read from the real results root's cache, never fitted or written
  # there: on a cold cache the test is skipped. Its cache entry is copied into the temporary
  # results root, so the replicate's run reads the same mu_f and fits no stage 1 either.
  info <- info_set(as.Date("2021-09-25"), as.Date("2021-03-29"), v2_inputs())
  cfg <- read_v2_config(root("harness", "configs", "v2.json"))
  real_stage1 <- file.path(results_root(), "stage1")
  before <- list.files(real_stage1, recursive = TRUE, all.files = TRUE)
  s1 <- tryCatch(stage1_mu_f(info, cfg, fit = FALSE), error = function(e) {
    if (!grepl("stage 1: no cached fit", conditionMessage(e), fixed = TRUE)) stop(e)
    NULL
  })
  if (is.null(s1)) {
    skip(paste(
      "stage 1's cache for 2021 f180 is cold under results_root()/stage1/: warm it with a v2 fit of the cell",
      "(R/harness/fit_cell.R v2 2021-09-25 2021-03-29 ...) or the slow v2s-m5 tests, then re-run"
    ))
  }
  results <- withr::local_tempdir("recovery-results-")
  dir.create(file.path(results, "stage1"))
  expect_true(file.copy(file.path(real_stage1, s1$sha256), file.path(results, "stage1"), recursive = TRUE))
  args <- c("2021-09-25", "2021-03-29", "3101", "1", "20", "20", "--lane=B")
  out <- run_recovery(args, results)
  expect_null(attr(out, "status"), label = shown(out))
  expect_match(out, "^TRUTH recovery_2021-09-25_2021-03-29_s3101 seed 3101[0-9]{5} after [0-9]+ attempt", all = FALSE)
  expect_match(out, "^(GO|STOP) recovery_2021-09-25_2021-03-29_s3101 lp_rhat=", all = FALSE)
  expect_match(out, "^DONE ", all = FALSE)
  # The real 2021-f180 data pass the rule: 2016's and 2017's national results and the polls.
  expect_true("REAL 2021-09-25_2021-03-29 under the redraw rule: kept" %in% out, label = shown(out))
  rep <- file.path(results, "recovery", "2021-09-25_2021-03-29", "3101")
  for (f in c("meta.json", "truth.json", "ranks.csv", "key_diagnostics.csv", "param_draws.parquet", "inputs/stan_data.json")) {
    expect_true(file.exists(file.path(rep, f)), label = f)
  }
  expect_false(dir.exists(file.path(rep, "cmdstan")))

  # The replicate's run fitted no stage 1: it read the copied cache.
  expect_false(any(startsWith(out, "STAGE1")), label = shown(out))

  # The real cell, its mu_f the cached stage 1's, through the same JSON writer as the replicate's data.
  real <- v2_stan_data(info, cfg, stage1 = function(info, cfg, root) s1)$data
  real$draw_forecast <- 0L
  real_json <- withr::local_tempfile(fileext = ".json")
  cmdstanr::write_stan_json(real, real_json)
  real_j <- jsonlite::read_json(real_json, simplifyVector = TRUE)
  sim <- jsonlite::read_json(file.path(rep, "inputs", "stan_data.json"), simplifyVector = TRUE)
  expect_identical(sort(names(sim)), sort(names(real_j)))
  for (f in c("y_nat", "y_kd", "y_res", "mu_f")) {
    expect_false(identical(digest::digest(sim[[f]], algo = "sha256"), digest::digest(real_j[[f]], algo = "sha256")), label = paste(f, "is the real cell's"))
  }
  design <- c(grep("^(grid_|house_)", names(real_j), value = TRUE), "N_open", "w")
  expect_true(all(c("grid_nat", "grid_kd", "grid_res", "grid_epoch", "house_nat", "house_kd", "house_first_epoch") %in% design))
  for (f in c(design, "idx_nat", "idx_kd", "idx_res", "n_rep_nat", "n_rep_kd", "n_rep_res", "delta_days", "exists", "entry_epoch")) {
    expect_identical(sim[[f]], real_j[[f]], label = f)
  }
  # Each row keeps its total: the sample sizes and valid votes are the real ones.
  expect_identical(rowSums(sim$y_nat), rowSums(real_j$y_nat))
  expect_identical(apply(sim$y_res, c(1, 2), sum), apply(real_j$y_res, c(1, 2), sum))
  expect_identical(sim$draw_forecast, 0L)

  ranks <- readr::read_csv(file.path(rep, "ranks.csv"), show_col_types = FALSE)
  expect_identical(nrow(ranks), as.integer(2 + real$P + real$n_epoch * real$K * real$P))
  expect_false(anyDuplicated(ranks$variable) > 0)
  expect_identical(as.vector(table(factor(ranks$group, GROUPS$group))), as.integer(c(1, 1, real$P, real$n_closed * real$K * real$P, real$K * real$P)))
  expect_true(all(ranks$q >= 0 & ranks$q <= 1))
  expect_identical(ranks$in90, ranks$q >= 0.05 & ranks$q <= 0.95)
  truth <- jsonlite::read_json(file.path(rep, "truth.json"), simplifyVector = TRUE)
  expect_equal(ranks$truth[ranks$variable == "tau_mu[1]"], truth$pars$tau_mu)
  expect_equal(ranks$truth[ranks$variable == "s_bar_E[1]"], truth$pars$s_bar_E)
  expect_equal(ranks$truth[ranks$group == "mu_open"], truth$natural$mu[real$n_epoch, ])
  expect_false(any(c("theta", "pi_nat") %in% names(truth$natural)))

  meta <- jsonlite::read_json(file.path(rep, "meta.json"), simplifyVector = TRUE)
  expect_identical(meta$stan_sha256, sha256_file(root("Stan", "forecast_v2.stan")))
  expect_identical(meta$config_sha256, sha256_file(root("harness", "configs", "v2.json")))
  expect_identical(meta$data_sha256, sha256_file(file.path(rep, "inputs", "stan_data.json")))
  expect_identical(c(meta$seed, meta$iter_warmup, meta$iter_sampling, meta$chains), c(3101L, 20L, 20L, 4L))
  expect_identical(meta$sim_seed, attempt_seed(3101, meta$attempts))
  expect_identical(meta$rejected, meta$attempts - 1L)
  expect_lt(meta$truth_check_error, 1e-8)
  expect_true(all(c("git_head", "git_dirty", "minutes", "cmdstan_version") %in% names(meta)))
  # The design is the real cell's, mu_f included: the replicate's design sha256 is the real data's.
  expect_identical(meta$design_sha256, stan_data_sha(real))
  # The provenance is the lane's, read when it started: the LANE line's, inputs.json's; read again
  # at the end, unchanged here.
  lane_line <- grep("^LANE B 2021-09-25_2021-03-29 seeds 3101..3101 at 20 \\+ 20 ", out, value = TRUE)
  expect_length(lane_line, 1)
  expect_true(endsWith(lane_line, paste0("git_head ", meta$git_head, ", git_dirty ", meta$git_dirty)), label = lane_line)
  inputs <- jsonlite::read_json(file.path(rep, "inputs", "inputs.json"), simplifyVector = TRUE)
  expect_identical(inputs[c("git_head", "git_dirty", "snapshot_manifest_sha256")], meta[c("git_head", "git_dirty", "snapshot_manifest_sha256")])
  expect_identical(c(meta$git_head_end, meta$git_dirty_end), c(meta$git_head, meta$git_dirty))
  expect_false(any(startsWith(out, "PROVENANCE ")))
  expect_identical(meta$recovery_version, rec$RECOVERY_VERSION)
  expect_true(all(c("divergent", "max_rhat", "treedepth_hits", "min_ess_bulk", "n_leapfrog") %in% names(meta$diagnostics)))

  # A second run skips the finished replicate; the report on it is not Decision 7's set.
  again <- run_recovery(args, results)
  expect_null(attr(again, "status"), label = shown(again))
  expect_true("SKIP recovery_2021-09-25_2021-03-29_s3101 (meta.json matches)" %in% again, label = shown(again))
  expect_false(any(startsWith(again, "START ")))
  md <- file.path(results, "recovery.md")
  report <- run_recovery(c("report", md), results)
  expect_null(attr(report, "status"), label = shown(report))
  expect_true("RECOVERY INCOMPLETE" %in% report, label = shown(report))
  text <- readLines(md, encoding = "UTF-8")
  expect_identical(utils::tail(text, 1), "RECOVERY INCOMPLETE")
  expect_true(any(startsWith(text, "| 2021 f180 | 1 | 3101\u20133101 | 20 + 20 |") & endsWith(text, "| kept |")), label = shown(text))
  expect_true("REAL 2021 f180 under the redraw rule: kept" %in% report, label = shown(report))
  expect_true(file.exists(file.path(results, "recovery", "logs", "lane-B.log")))
  # Nothing was written to the real results root's stage-1 cache.
  expect_identical(list.files(real_stage1, recursive = TRUE, all.files = TRUE), before)
})
