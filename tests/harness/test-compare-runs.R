# compare_runs.R (the Phase 2 plan's Decisions 5, 6 and 8), run as a script on synthetic run
# directories: made-up meta.json, key_diagnostics.csv and parquets, never a fit's draws.
box::use(
  R / harness / runs[run_id],
  R / harness / freeze[is_frozen],
  R / harness / v2[read_v2_config, v2_variables, V2_PARAM_SWITCH],
  R / harness / v2_reference[V2_LOG_CAPS]
)

EVE <- c("2024-11-30", "2024-11-29")
F180 <- c("2021-09-25", "2021-03-29")
CHAINS <- 4L
ITERS <- 400L
DRAWS <- CHAINS * ITERS
# WHY: Decision 4's caps, written out here, so that a change to V2_LOG_CAPS shows.
CAPS <- c(log_phi = 20, log_phi_K = 20, log_kappa = 2)

# compare_runs.R run from the project root; its output, with the exit status as attribute
# "status" when it is not 0.
compare <- function(args) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/compare_runs.R", args),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
}

shown <- function(out) paste(out, collapse = "\n")

ids <- function(arm, cell, seeds) vapply(seeds, \(s) run_id(arm, cell[1], cell[2], s), character(1))

# A run list line per seed: "<lane> <arm> <E> <F> <seed>".
run_lines <- function(lane, arm, cell, seeds) sprintf("%s %s %s %s %d", lane, arm, cell[1], cell[2], seeds)

DIAG <- list(
  max_rhat = 1.02, max_rhat_scored = 1.01, min_ess_bulk = 512, min_ess_bulk_scored = 600,
  min_ess_tail = 400, min_ess_tail_scored = 450, na_scored = 0L, divergent = 0L,
  treedepth_hits = 3L, max_depth = 9L, min_ebfmi = 0.7, n_leapfrog = 1e6
)

# The meta.json of run (arm, cell, seed) in `dir`, with `diag` as its diagnostics.
write_meta <- function(dir, arm, cell, seed, diag = DIAG) {
  meta <- list(
    arm = arm, role = "dev", E = cell[1], F = cell[2], id = basename(dir), seed = seed, iter_warmup = ITERS,
    iter_sampling = ITERS, chains = CHAINS, max_treedepth = 12L, minutes = 14.2, diagnostics = diag
  )
  jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "string")
}

# A finished run of `arm` on `cell` with `seed`, 4 chains of 400 sampling draws, under
# `results`: its meta.json, `diag` merged into DIAG (NULL drops a field); its
# key_diagnostics.csv, lp__'s R-hat `lp_rhat` (no lp__ row when NULL); and its
# param_draws.parquet, whose bounded log concentrations sit 3 below their caps except that
# each element `name = list(draws, value)` of `set` puts column `name` at `value` on those
# draws, with the columns in `drop` left out.
fake_run <- function(results, arm, cell, seed, diag = list(), lp_rhat = 1.003, set = list(), drop = character(0)) {
  dir <- file.path(results, run_id(arm, cell[1], cell[2], seed))
  dir.create(dir, recursive = TRUE)
  write_meta(dir, arm, cell, seed, utils::modifyList(DIAG, diag))
  key <- tibble::tibble(
    variable = c("scored_nat[Flokkur A]", "sigma[1]"), rhat = c(1.004, 1.002), ess_bulk = c(800, 700),
    ess_tail = c(750, 650)
  )
  if (!is.null(lp_rhat)) key <- rbind(key, tibble::tibble(variable = "lp__", rhat = lp_rhat, ess_bulk = 300, ess_tail = 350))
  readr::write_csv(key, file.path(dir, "key_diagnostics.csv"))
  below <- function(cap) rep(CAPS[[cap]] - 3, DRAWS)
  pd <- list(
    .chain = rep(seq_len(CHAINS), each = ITERS), .iteration = rep(seq_len(ITERS), CHAINS), .draw = seq_len(DRAWS),
    `sigma[1]` = rep(0.01, DRAWS), `log_phi[1]` = below("log_phi"), `log_phi[2]` = below("log_phi"),
    log_phi_K = below("log_phi_K"), log_kappa = below("log_kappa"), sigma_gamma = rep(0.1, DRAWS)
  )
  for (name in names(set)) pd[[name]][set[[name]][[1]]] <- set[[name]][[2]]
  arrow::write_parquet(tibble::as_tibble(pd[setdiff(names(pd), drop)]), file.path(dir, "param_draws.parquet"))
  invisible(dir)
}

test_that("the caps the stop rule reads are Decision 4's and the Stan file's, and v2s-m0's param_draws holds each", {
  expect_identical(V2_LOG_CAPS, CAPS)
  stan <- readLines(root("Stan", "forecast_v2.stan"))
  cap <- function(name) {
    line <- grep(paste0("^ *real ", name, " = "), stan, value = TRUE)
    expect_length(line, 1)
    as.numeric(sub("^.*= *([0-9.]+);.*$", "\\1", line))
  }
  expect_identical(c(log_phi = cap("phi_cap"), log_phi_K = cap("phi_K_cap"), log_kappa = cap("kappa_cap")), CAPS)
  # WHY: v2_extract() writes every variable of v2_variables() that no switch removes into param_draws.parquet.
  vars <- v2_variables(read_v2_config(root("harness", "configs", "steps", "v2s-m0.json")))
  expect_true(all(names(CAPS) %in% vars))
  expect_false(any(names(CAPS) %in% names(V2_PARAM_SWITCH)))
})

test_that("the step run list holds v2s-m0's four sampler-check fits, and the stop rule reads it by default", {
  lines <- trimws(readLines(root("harness", "runs", "v2-steps.txt")))
  runs <- lines[nzchar(lines) & !startsWith(lines, "#")]
  expect_identical(runs[grepl("^[AB] v2s-m0 ", runs)], c(
    run_lines("A", "v2s-m0", EVE, c(2701L, 2702L)), run_lines("B", "v2s-m0", F180, c(2701L, 2702L))
  ))
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  out <- compare(c("stop", "v2s-m0"))
  expect_identical(attr(out, "status"), 1L)
  expect_match(out, "no meta.json in .*/v2s-m0_2024-11-30_2024-11-29_s2701", all = FALSE)
})

test_that("the stop rule gives GO with no signal, STOP on each signal alone, and GO at each threshold", {
  results <- withr::local_tempdir()
  lists <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  arm <- "v2s-t0"
  fake_run(results, arm, EVE, 1)
  fake_run(results, arm, EVE, 2, lp_rhat = 1.1001)
  fake_run(results, arm, EVE, 3, diag = list(divergent = 17L))
  fake_run(results, arm, EVE, 4, set = list(`log_phi[1]` = list(1:9, 19.5), log_kappa = list(10:17, 1.5)))
  fake_run(results, arm, F180, 5, diag = list(max_rhat = 1.1001))
  # WHY: each signal at its threshold, which is not above it: log_phi[2] and log_phi_K are within 0.5 of their caps on the same 16 draws, which count once, and log_phi[1] sits 0.51 below its cap, which is not within 0.5.
  fake_run(results, arm, F180, 6,
    lp_rhat = 1.1, diag = list(divergent = 16L, max_rhat = 1.1),
    set = list(`log_phi[2]` = list(1:16, 19.5), log_phi_K = list(1:16, 19.5), `log_phi[1]` = list(17:400, 19.49))
  )
  # WHY: no n_leapfrog, as in a run fitted before it was recorded: a recorded value, never a stop.
  fake_run(results, arm, F180, 7, diag = list(n_leapfrog = NULL))
  runs <- file.path(lists, "runs.txt")
  writeLines(c(
    "# lane arm E F seed", run_lines("A", arm, EVE, 1:4), "A v2s-other 2024-11-30 2024-11-29 1",
    run_lines("B", arm, F180, 5:7)
  ), runs)

  out <- compare(c("stop", arm, runs))
  expect_identical(attr(out, "status"), 1L, info = shown(out))
  lines <- grep("^(GO|STOP) ", out, value = TRUE)
  expect_identical(sub(" .*$", "", lines), c("GO", "STOP", "STOP", "STOP", "STOP", "GO", "GO"))
  expect_identical(sub("^[A-Z]+ ([^ ]+) .*$", "\\1", lines), c(ids(arm, EVE, 1:4), ids(arm, F180, 5:7)))
  expect_identical(lines[1], paste(
    "GO v2s-t0_2024-11-30_2024-11-29_s1 lp_rhat=1.0030 divergent=0/1600 near_cap=0/1600 max_rhat=1.0200;",
    "recorded: minutes=14.2 treedepth_hits=3 ess_bulk_per_1k_grad=0.512"
  ))
  expect_match(lines[2], "lp_rhat=1.1001 .*; stop: R-hat of lp__ 1.1001 > 1.1$")
  expect_match(lines[3], "divergent=17/1600 .*; stop: 17 of 1600 transitions divergent > 1%$")
  expect_match(lines[4], "near_cap=17/1600 .*; stop: 17 of 1600 draws within 0.5 of a cap > 1%$")
  expect_match(lines[5], "max_rhat=1.1001; .*; stop: max R-hat \\(gate scope\\) 1.1001 > 1.1$")
  expect_false(any(grepl("stop: .*;", lines)))
  expect_match(lines[6], "^GO .* lp_rhat=1.1000 divergent=16/1600 near_cap=16/1600 max_rhat=1.1000; recorded: ")
  expect_match(lines[7], "^GO .*ess_bulk_per_1k_grad=NA$")
  expect_false(any(grepl("v2s-other", out, fixed = TRUE)))

  writeLines(run_lines("A", arm, F180, 6:7), runs)
  out <- compare(c("stop", arm, runs))
  expect_null(attr(out, "status"), info = shown(out))
  expect_identical(sub(" .*$", "", grep("^(GO|STOP) ", out, value = TRUE)), c("GO", "GO"))
})

test_that("the adoption rule adopts only when all four of its conditions hold", {
  results <- withr::local_tempdir()
  lists <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  # WHY: with n_leapfrog 1e6, min bulk ESS / 1000 is the min bulk ESS per 1,000 gradients.
  fits <- function(arm, ess, divergent, lp_rhat = rep(1.003, 4)) {
    cells <- list(EVE, EVE, F180, F180)
    seeds <- c(2701L, 2702L, 2701L, 2702L)
    for (i in 1:4) {
      fake_run(results, arm, cells[[i]], seeds[i], diag = list(min_ess_bulk = ess[i], divergent = divergent[i]), lp_rhat = lp_rhat[i])
    }
    c(run_lines("A", arm, EVE, seeds[1:2]), run_lines("B", arm, F180, seeds[3:4]))
  }
  runs <- file.path(lists, "runs.txt")
  writeLines(c(
    "# lane arm E F seed",
    # WHY: medians 0.5 (eve) and 0.25 (f180), one divergence.
    fits("v2s-r", c(400, 600, 200, 300), c(1L, 0L, 0L, 0L)),
    # WHY: 1.26x and 1.32x the reference, and 5 = 1 + 4 divergences: every condition holds.
    fits("v2s-na", c(620, 640, 320, 340), c(2L, 1L, 1L, 1L)),
    # WHY: 1.22x on f180.
    fits("v2s-nb", c(620, 640, 300, 310), c(0L, 0L, 0L, 0L)),
    # WHY: 6 > 1 + 4 divergences.
    fits("v2s-nc", c(620, 640, 320, 340), c(2L, 2L, 1L, 1L)),
    # WHY: one fit's lp__ R-hat trips the stop rule.
    fits("v2s-nd", c(620, 640, 320, 340), c(0L, 0L, 0L, 0L), lp_rhat = c(1.003, 1.003, 1.2, 1.003))
  ), runs)
  rows <- tibble::tibble(
    E = c(EVE[1], F180[1], "2017-10-28", EVE[1]), F = c(EVE[2], F180[2], "2017-10-27", EVE[2]),
    order = c("default", "default", "default", "reversed"), verdict = "PASS", n_points = c(45L, 45L, 20L, 45L)
  )
  equivalence <- function(name, x) {
    path <- file.path(lists, name)
    readr::write_csv(x, path)
    path
  }
  good <- equivalence("good.csv", rows)
  failed <- equivalence("failed.csv", dplyr::mutate(rows, verdict = c("PASS", "PASS", "PASS", "FAIL")))
  partial <- equivalence("partial.csv", rows[-2, ])
  adopt <- function(new, csv) {
    out <- compare(c("adopt", "v2s-r", new, csv, runs))
    expect_null(attr(out, "status"), info = shown(out))
    out
  }

  out <- adopt("v2s-na", good)
  expect_identical(out[length(out)], "ADOPT v2s-na")
  expect_true("| 2024-11-30, 2024-11-29, eve | 0.500 | 0.630 | 1.26 | yes |" %in% out, info = shown(out))
  expect_true("| 2021-09-25, 2021-03-29, f180 | 0.250 | 0.330 | 1.32 | yes |" %in% out, info = shown(out))
  expect_true("| v2s-r_2024-11-30_2024-11-29_s2701 | 0.400 | 1 | GO |" %in% out, info = shown(out))
  expect_length(grep("^\\| v2s-(r|na)_", out), 8)
  expect_false(any(grepl("v2s-n[bcd]", out)))
  expect_match(out, "^1\\. .*: yes", all = FALSE)
  expect_match(out, "^3\\. .* 5, .* 1, plus 4: yes$", all = FALSE)

  expect_identical(utils::tail(adopt("v2s-nb", good), 1), "KEEP v2s-r: v2s-nb fails condition 2")
  expect_identical(utils::tail(adopt("v2s-nc", good), 1), "KEEP v2s-r: v2s-nc fails condition 3")
  expect_identical(utils::tail(adopt("v2s-nd", good), 1), "KEEP v2s-r: v2s-nd fails condition 4")
  out <- adopt("v2s-na", failed)
  expect_identical(out[length(out)], "KEEP v2s-r: v2s-na fails condition 1")
  expect_match(out, "^1\\. .*: no \\(.*FAIL.*2024-11-30, 2024-11-29, reversed", all = FALSE)
  out <- adopt("v2s-na", partial)
  expect_identical(out[length(out)], "KEEP v2s-r: v2s-na fails condition 1")
  expect_match(out, "^1\\. .*: no \\(.*no default-order row on 2021-09-25, 2021-03-29", all = FALSE)
})

PARTIES <- c("Flokkur A", "Mi\u00f0flokkurinn", "Flokkur C", "Flokkur D")
KJORDAEMI <- paste("Kj\u00f6rd\u00e6mi", 1:6)
S0 <- c(0.45, 0.3, 0.17, 0.08)
PERM_CHAINS <- 4L
PERM_ITERS <- ITERS

softmax <- function(x) exp(x - max(x)) / sum(exp(x - max(x)))

# Made-up eday draws of one run: PERM_CHAINS chains of PERM_ITERS independent draws around
# S0, `nat` (draws x parties) national shares and `kd` (draws x kjördæmi x parties) votes,
# 20000 in each kjördæmi.
eday_draws <- function(seed) {
  withr::with_seed(seed, {
    n <- PERM_CHAINS * PERM_ITERS
    K <- length(KJORDAEMI)
    P <- length(PARTIES)
    effect <- matrix(seq(-0.3, 0.3, length.out = K * P), K, P)
    nat <- t(replicate(n, softmax(log(S0) + stats::rnorm(P, 0, 0.05))))
    kd <- array(0L, c(n, K, P))
    for (i in seq_len(n)) {
      for (k in seq_len(K)) kd[i, k, ] <- stats::rmultinom(1, 20000, softmax(log(S0) + effect[k, ] + stats::rnorm(P, 0, 0.05)))
    }
    list(nat = nat, kd = kd)
  })
}

# The MCSE of the mean of party p's national share in `d`.
mcse_nat <- function(d, p) posterior::mcse_mean(matrix(d$nat[, p], ncol = PERM_CHAINS))

# Write `d` as the eday_national and eday_kjordaemi parquets of run (arm, cell, seed), with
# its meta.json, rows in the reversed party and kjördæmi order when `reverse`, as v2-perm
# writes them.
write_eday <- function(results, arm, cell, seed, d, reverse = FALSE) {
  dir <- file.path(results, run_id(arm, cell[1], cell[2], seed))
  dir.create(dir, recursive = TRUE)
  write_meta(dir, arm, cell, seed)
  n <- nrow(d$nat)
  pp <- if (reverse) rev(seq_along(PARTIES)) else seq_along(PARTIES)
  kk <- if (reverse) rev(seq_along(KJORDAEMI)) else seq_along(KJORDAEMI)
  draw <- seq_len(n)
  chain <- (draw - 1L) %/% PERM_ITERS + 1L
  iteration <- (draw - 1L) %% PERM_ITERS + 1L
  nat <- tibble::tibble(
    .chain = rep(chain, each = length(pp)), .iteration = rep(iteration, each = length(pp)),
    .draw = rep(draw, each = length(pp)), flokkur = rep(PARTIES[pp], n), share = c(t(d$nat[, pp]))
  )
  cells <- expand.grid(p = pp, k = kk)
  kd <- tibble::tibble(
    .chain = rep(chain, each = nrow(cells)), .iteration = rep(iteration, each = nrow(cells)),
    .draw = rep(draw, each = nrow(cells)), kjordaemi = rep(KJORDAEMI[cells$k], n), flokkur = rep(PARTIES[cells$p], n),
    votes = as.integer(unlist(lapply(draw, \(i) d$kd[cbind(i, cells$k, cells$p)])))
  )
  arrow::write_parquet(nat, file.path(dir, "eday_national.parquet"))
  arrow::write_parquet(kd, file.path(dir, "eday_kjordaemi.parquet"))
  invisible(dir)
}

PERM_LINE <- paste0(
  "^PERM (PASS|FAIL) [^ ]+ vs [^ ]+: [0-9]+ z's, [0-9]+ with \\|z\\| > 3, max \\|z\\| ([0-9]+\\.[0-9]{2}|Inf|NA), ",
  "sd of the mean z's ([0-9]+\\.[0-9]{2}|NA)$"
)
NAME <- "eday_(national\\[[^],]+\\]|kjordaemi\\[[^],]+,[^],]+\\]) (mean|q05|q50|q95)"
NAMES_LINE <- paste0("^  \\|z\\| > 3: ", NAME, "(, ", NAME, ")*$")

test_that("the permutation rule passes two runs of one distribution, matched by name, and fails a 10-MCSE shift", {
  results <- withr::local_tempdir()
  lists <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  write_eday(results, "v2s-pa", EVE, 1201, eday_draws(13))
  # WHY: the rows in the reversed orders, so a match by position rather than by name would fail.
  write_eday(results, "v2s-pb", EVE, 1201, eday_draws(14), reverse = TRUE)
  a <- eday_draws(11)
  b <- eday_draws(12)
  b$nat[, 1] <- b$nat[, 1] + 10 * sqrt(mcse_nat(a, 1)^2 + mcse_nat(b, 1)^2)
  write_eday(results, "v2s-pa", F180, 1202, a)
  write_eday(results, "v2s-pb", F180, 1202, b, reverse = TRUE)
  runs <- file.path(lists, "runs.txt")
  writeLines(c(
    run_lines("A", "v2s-pa", EVE, 1201L), run_lines("B", "v2s-pa", F180, 1202L),
    run_lines("A", "v2s-pb", EVE, 1201L), run_lines("B", "v2s-pb", F180, 1202L)
  ), runs)

  out <- compare(c("perm", "v2s-pa", "v2s-pb", runs))
  expect_identical(attr(out, "status"), 1L, info = shown(out))
  verdicts <- grep("^PERM ", out, value = TRUE)
  expect_length(verdicts, 2)
  n_z <- 4 * (length(PARTIES) + length(KJORDAEMI) * length(PARTIES))
  expect_match(verdicts[1], paste0(
    "^PERM PASS ", ids("v2s-pa", EVE, 1201), " vs ", ids("v2s-pb", EVE, 1201), ": ", n_z, " z's, [0-3] with "
  ))
  expect_match(verdicts[2], paste0("^PERM FAIL ", ids("v2s-pa", F180, 1202), " vs ", ids("v2s-pb", F180, 1202), ": "))
  expect_true(any(grepl(paste0("^  \\|z\\| > 3: .*eday_national\\[", PARTIES[1], "\\] mean"), out)), info = shown(out))
  # WHY: every line is a verdict with its statistics or a list of names, so no share value is printed.
  expect_true(all(grepl(PERM_LINE, out) | grepl(NAMES_LINE, out)), info = shown(out))

  writeLines(c(run_lines("A", "v2s-pa", EVE, 1201L), run_lines("A", "v2s-pb", EVE, 1201L)), runs)
  out <- compare(c("perm", "v2s-pa", "v2s-pb", runs))
  expect_null(attr(out, "status"), info = shown(out))
  expect_match(out, "^PERM PASS ", all = FALSE)
  expect_true(all(grepl(PERM_LINE, out) | grepl(NAMES_LINE, out)), info = shown(out))
})

test_that("compare_runs.R stops on a command line, run or table it cannot read", {
  results <- withr::local_tempdir()
  lists <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  runs <- file.path(lists, "runs.txt")
  fails_with <- function(args, message) {
    out <- compare(args)
    expect_identical(attr(out, "status"), 1L, info = shown(out))
    expect_match(out, message, fixed = TRUE, all = FALSE)
  }

  fails_with("check", "usage: Rscript R/harness/compare_runs.R stop <arm> [run_list]")
  writeLines(run_lines("A", "v2s-t9", EVE, 1L), runs)
  fails_with(c("stop", "v2s-t1", runs), "no run of arm v2s-t1 in")
  fake_run(results, "v2s-t1", EVE, 1, lp_rhat = NULL)
  writeLines(run_lines("A", "v2s-t1", EVE, 1L), runs)
  fails_with(c("stop", "v2s-t1", runs), "key_diagnostics.csv has no lp__ row")
  fake_run(results, "v2s-t1", EVE, 2, drop = "log_kappa")
  writeLines(run_lines("A", "v2s-t1", EVE, 2L), runs)
  fails_with(c("stop", "v2s-t1", runs), "param_draws.parquet has no column of log_kappa")
  dir <- fake_run(results, "v2s-t1", EVE, 3)
  meta <- jsonlite::read_json(file.path(dir, "meta.json"))
  meta$seed <- 4L
  jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, digits = NA)
  writeLines(run_lines("A", "v2s-t1", EVE, 3L), runs)
  fails_with(c("stop", "v2s-t1", runs), "is not of the run list's run v2s-t1_2024-11-30_2024-11-29_s3")

  # WHY: the new arm has a cell the reference form lacks.
  fake_run(results, "v2s-t1", F180, 5)
  writeLines(c(run_lines("A", "v2s-t1", EVE, 1L), run_lines("A", "v2s-t2", F180, 5L)), runs)
  csv <- file.path(lists, "equivalence.csv")
  readr::write_csv(tibble::tibble(E = EVE[1], F = EVE[2], order = "default", verdict = "PASS"), csv)
  fails_with(c("adopt", "v2s-t1", "v2s-t2", csv, runs), "v2s-t1 and v2s-t2 have runs on different cells")
  readr::write_csv(tibble::tibble(E = EVE[1], F = EVE[2], verdict = "PASS"), csv)
  writeLines(c(run_lines("A", "v2s-t1", F180, 5L), run_lines("A", "v2s-t2", F180, 5L)), runs)
  fake_run(results, "v2s-t2", F180, 5)
  fails_with(c("adopt", "v2s-t1", "v2s-t2", csv, runs), "lacks the column order")

  write_eday(results, "v2s-pa", EVE, 1, eday_draws(21))
  # WHY: run b is run a with one party fewer.
  dir <- file.path(results, run_id("v2s-pb", EVE[1], EVE[2], 1))
  dir.create(dir)
  write_meta(dir, "v2s-pb", EVE, 1)
  nat <- arrow::read_parquet(file.path(results, run_id("v2s-pa", EVE[1], EVE[2], 1), "eday_national.parquet"))
  kd <- arrow::read_parquet(file.path(results, run_id("v2s-pa", EVE[1], EVE[2], 1), "eday_kjordaemi.parquet"))
  arrow::write_parquet(nat[nat$flokkur != PARTIES[4], ], file.path(dir, "eday_national.parquet"))
  arrow::write_parquet(kd[kd$flokkur != PARTIES[4], ], file.path(dir, "eday_kjordaemi.parquet"))
  writeLines(c(run_lines("A", "v2s-pa", EVE, 1L), run_lines("A", "v2s-pb", EVE, 1L)), runs)
  fails_with(c("perm", "v2s-pa", "v2s-pb", runs), "the runs' variables differ")
})

test_that("compare_runs.R refuses a 2017 cell before the freeze, before it reads the run", {
  # WHY: this reads the repository's real freeze state; Plan 4 replaces the test once the freeze is sealed.
  skip_if(is_frozen(), "the freeze is sealed")
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  runs <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("A v2s-pa 2017-10-28 2017-10-27 1", "A v2s-pb 2017-10-28 2017-10-27 1"), runs)
  for (args in list(c("stop", "v2s-pa", runs), c("perm", "v2s-pa", "v2s-pb", runs))) {
    out <- compare(args)
    expect_identical(attr(out, "status"), 1L, info = shown(out))
    expect_match(out, "compare_runs: 2017 is not compared before the freeze (spec \u00a75.7)", fixed = TRUE, all = FALSE)
    expect_false(any(grepl("meta.json|parquet", out)))
  }
})
