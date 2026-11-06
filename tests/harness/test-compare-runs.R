# compare_runs.R (the Phase 2 plan's Decisions 5, 6 and 8), run as a script on synthetic run
# directories: made-up meta.json, key_diagnostics.csv and parquets, never a fit's draws. Its z
# and pass rule, perm_z() and perm_verdict(), are also tested as functions, on made-up draws and
# z-statistics, and so is relative(), which keeps absolute paths out of its messages.
box::use(
  R / harness / runs[run_id],
  R / harness / freeze[is_frozen],
  R / harness / v2[read_v2_config, v2_variables, V2_PARAM_SWITCH],
  R / harness / v2_reference[V2_LOG_CAPS],
  R / harness / compare_runs[perm_z, perm_verdict, relative, EQUIVALENCE_REQUIRED]
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

# Whether `out` prints the absolute path of `results` (as given or resolved) anywhere.
prints_path <- function(out, results) {
  any(grepl(results, out, fixed = TRUE) | grepl(normalizePath(results), out, fixed = TRUE))
}

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
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  out <- compare(c("stop", "v2s-m0"))
  expect_identical(attr(out, "status"), 1L)
  # WHY: a run directory is printed under $HARNESS_RESULTS, never as an absolute path a step log could pick up.
  expect_match(out, "compare_runs: no meta.json in $HARNESS_RESULTS/v2s-m0_2024-11-30_2024-11-29_s2701:", fixed = TRUE, all = FALSE)
  expect_false(prints_path(out, results), info = shown(out))
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

test_that("the stop rule STOPs on an NA or absent signal, and on a near-cap value in any house's log_phi column", {
  results <- withr::local_tempdir()
  lists <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  arm <- "v2s-t8"
  fake_run(results, arm, EVE, 1, lp_rhat = NA)
  fake_run(results, arm, EVE, 2, diag = list(divergent = NA))
  fake_run(results, arm, EVE, 3, diag = list(divergent = NULL))
  fake_run(results, arm, EVE, 4, diag = list(max_rhat = NA))
  fake_run(results, arm, EVE, 5, diag = list(max_rhat = NULL))
  # WHY: only the second house's column is near its cap, so a check of the first column alone would give GO.
  fake_run(results, arm, EVE, 6, set = list(`log_phi[2]` = list(1:17, 19.6)))
  runs <- file.path(lists, "runs.txt")
  writeLines(run_lines("A", arm, EVE, 1:6), runs)

  out <- compare(c("stop", arm, runs))
  expect_identical(attr(out, "status"), 1L, info = shown(out))
  lines <- grep("^(GO|STOP) ", out, value = TRUE)
  expect_identical(sub(" .*$", "", lines), rep("STOP", 6), info = shown(out))
  expect_match(lines[1], "lp_rhat=NA .*; stop: R-hat of lp__ NA > 1.1$")
  for (i in 2:3) expect_match(lines[i], "divergent=NA/1600 .*; stop: NA of 1600 transitions divergent > 1%$")
  for (i in 4:5) expect_match(lines[i], "max_rhat=NA; .*; stop: max R-hat \\(gate scope\\) NA > 1.1$")
  expect_match(lines[6], "near_cap=17/1600 .*; stop: 17 of 1600 draws within 0.5 of a cap > 1%$")
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
    # WHY: v2s-r with one fit's lp__ R-hat over its limit: a STOP on the reference form alone.
    fits("v2s-rs", c(400, 600, 200, 300), c(1L, 0L, 0L, 0L), lp_rhat = c(1.003, 1.2, 1.003, 1.003)),
    # WHY: 1.26x and 1.32x the reference, and 5 = 1 + 4 divergences: every condition holds.
    fits("v2s-na", c(620, 640, 320, 340), c(2L, 1L, 1L, 1L)),
    # WHY: 1.22x on f180.
    fits("v2s-nb", c(620, 640, 300, 310), c(0L, 0L, 0L, 0L)),
    # WHY: 6 > 1 + 4 divergences.
    fits("v2s-nc", c(620, 640, 320, 340), c(2L, 2L, 1L, 1L)),
    # WHY: one fit's lp__ R-hat trips the stop rule.
    fits("v2s-nd", c(620, 640, 320, 340), c(0L, 0L, 0L, 0L), lp_rhat = c(1.003, 1.003, 1.2, 1.003)),
    # WHY: exactly 1.25x on both cells, 0.625 = 1.25 x 0.5 and 0.3125 = 1.25 x 0.25, each exact in binary.
    fits("v2s-ne", c(625, 625, 312.5, 312.5), c(0L, 0L, 0L, 0L))
  ), runs)
  rows <- tibble::tibble(
    E = c(EVE[1], F180[1], "2017-10-28", EVE[1]), F = c(EVE[2], F180[2], "2017-10-27", EVE[2]),
    order = c("default", "default", "default", "reversed"), verdict = "PASS", n_points = c(45L, 45L, 20L, 45L)
  )
  # WHY: the checks condition 1 requires are the four rows the P1 step's equivalence check writes.
  expect_identical(do.call(paste, EQUIVALENCE_REQUIRED), do.call(paste, rows[c("E", "F", "order")]))
  equivalence <- function(name, x) {
    path <- file.path(lists, name)
    readr::write_csv(x, path)
    path
  }
  good <- equivalence("good.csv", rows)
  failed <- equivalence("failed.csv", dplyr::mutate(rows, verdict = c("PASS", "PASS", "PASS", "FAIL")))
  partial <- equivalence("partial.csv", rows[-2, ])
  no_2017 <- equivalence("no_2017.csv", rows[-3, ])
  no_reversed <- equivalence("no_reversed.csv", rows[-4, ])
  extra_fail <- equivalence("extra_fail.csv", rbind(rows, tibble::tibble(
    E = F180[1], F = F180[2], order = "reversed", verdict = "FAIL", n_points = 45L
  )))
  adopt <- function(new, csv, ref = "v2s-r") {
    out <- compare(c("adopt", ref, new, csv, runs))
    expect_null(attr(out, "status"), info = shown(out))
    out
  }

  # WHY: good.csv holds the 2017-eve row, a verdict and not a run, which adopt reads before the freeze.
  out <- adopt("v2s-na", good)
  expect_identical(out[length(out)], "ADOPT v2s-na")
  expect_true("| 2024-11-30, 2024-11-29, eve | 0.500 | 0.630 | 1.26 | yes |" %in% out, info = shown(out))
  expect_true("| 2021-09-25, 2021-03-29, f180 | 0.250 | 0.330 | 1.32 | yes |" %in% out, info = shown(out))
  expect_true("| v2s-r_2024-11-30_2024-11-29_s2701 | 0.400 | 1 | GO |" %in% out, info = shown(out))
  expect_length(grep("^\\| v2s-(r|na)_", out), 8)
  expect_false(any(grepl("v2s-n[bcde]|v2s-rs", out)))
  expect_match(out, "^1\\. .*: yes \\(4 rows, none FAIL, a PASS row of each of the 4 required checks\\)$", all = FALSE)
  expect_match(out, "^3\\. .* 5, .* 1, plus 4: yes$", all = FALSE)

  out <- adopt("v2s-ne", good)
  expect_identical(out[length(out)], "ADOPT v2s-ne", info = shown(out))
  expect_true("| 2024-11-30, 2024-11-29, eve | 0.500 | 0.625 | 1.25 | yes |" %in% out, info = shown(out))
  expect_true("| 2021-09-25, 2021-03-29, f180 | 0.250 | 0.312 | 1.25 | yes |" %in% out, info = shown(out))

  expect_identical(utils::tail(adopt("v2s-nb", good), 1), "KEEP v2s-r: v2s-nb fails condition 2")
  expect_identical(utils::tail(adopt("v2s-nc", good), 1), "KEEP v2s-r: v2s-nc fails condition 3")
  expect_identical(utils::tail(adopt("v2s-nd", good), 1), "KEEP v2s-r: v2s-nd fails condition 4")
  # WHY: condition 4 reads both arms' fits, so a STOP on a reference fit alone keeps the reference form.
  out <- adopt("v2s-na", good, ref = "v2s-rs")
  expect_identical(out[length(out)], "KEEP v2s-rs: v2s-na fails condition 4")
  expect_true("| v2s-rs_2024-11-30_2024-11-29_s2702 | 0.600 | 0 | STOP: R-hat of lp__ 1.2000 > 1.1 |" %in% out, info = shown(out))

  condition_1 <- function(csv, detail) {
    out <- adopt("v2s-na", csv)
    expect_identical(out[length(out)], "KEEP v2s-r: v2s-na fails condition 1")
    expect_match(out, paste0("^1\\. .*: no \\(", detail, "\\)$"), all = FALSE)
  }
  condition_1(failed, "a FAIL row on 2024-11-30, 2024-11-29, reversed; no PASS row on 2024-11-30, 2024-11-29, reversed")
  condition_1(partial, "no PASS row on 2021-09-25, 2021-03-29, default")
  condition_1(no_2017, "no PASS row on 2017-10-28, 2017-10-27, default")
  condition_1(no_reversed, "no PASS row on 2024-11-30, 2024-11-29, reversed")
  # WHY: a FAIL row outside the required checks fails condition 1 too.
  condition_1(extra_fail, "a FAIL row on 2021-09-25, 2021-03-29, reversed")
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

test_that("perm's z is the difference over the root sum of the squared MCSEs, with posterior's MCSEs", {
  probs <- c(0.05, 0.5, 0.95)
  draws <- function(seed, sd) withr::with_seed(seed, matrix(stats::rnorm(DRAWS, 0, sd), ncol = CHAINS))
  a0 <- draws(31, 1)
  b0 <- draws(32, 2)
  m_a <- posterior::mcse_mean(a0)
  m_b <- posterior::mcse_mean(b0)
  # WHY: a shift between 3 sqrt(m_a^2 + m_b^2) and 3 (m_a + m_b), so the root-sum z is above 3 and delta / (m_a + m_b) below it.
  shift <- 1.5 * (sqrt(m_a^2 + m_b^2) + m_a + m_b)
  a <- a0 - mean(a0)
  b <- b0 - mean(b0) + shift
  mcse <- function(m) c(posterior::mcse_mean(m), unname(posterior::mcse_quantile(m, probs = probs)))
  delta <- c(mean(a) - mean(b), stats::quantile(c(a), probs, names = FALSE) - stats::quantile(c(b), probs, names = FALSE))

  z <- perm_z(a, b)
  expect_identical(names(z), c("mean", "q05", "q50", "q95"))
  expect_equal(unname(z), delta / sqrt(mcse(a)^2 + mcse(b)^2))
  expect_gt(abs(z[["mean"]]), 3)
  expect_lt(abs(delta[1]) / (mcse(a)[1] + mcse(b)[1]), 3)
  zero <- c(mean = 0, q05 = 0, q50 = 0, q95 = 0)
  expect_identical(perm_z(a, a), zero)
  # WHY: a constant has no MCSE, so its z is 0 against itself and missing against another value.
  flat <- matrix(0.3, ITERS, CHAINS)
  expect_identical(perm_z(flat, flat), zero)
  missing <- c(mean = NA_real_, q05 = NA_real_, q50 = NA_real_, q95 = NA_real_)
  expect_identical(perm_z(flat, flat + 0.01), missing)
  a[1] <- NaN
  expect_identical(perm_z(a, b), missing)
})

# z-statistics named as perm_pair() names them: 28 variables, their mean z's `mean` and their
# quantile z's `q`.
z_set <- function(mean = rep(0, 28), q = rep(0, 3 * 28)) {
  vars <- paste0("eday_kjordaemi[K,v", seq_len(28), "]")
  c(stats::setNames(mean, paste(vars, "mean")), stats::setNames(q, paste(rep(vars, each = 3), c("q05", "q50", "q95"))))
}

test_that("perm's pass rule trips on each of its three conditions alone, at the bounds Decision 8 states", {
  verdict <- function(...) perm_verdict(z_set(...))
  q <- rep(0, 84)
  expect_true(verdict()$pass)

  # WHY: every |z| is 2, so none is above 3 and the max is within 5, but the mean z's have sd 2.04 > 1.5.
  spread <- verdict(mean = rep(c(2, -2), 14))
  expect_false(spread$pass)
  expect_identical(sum(spread$over), 0L)
  expect_identical(spread$biggest, 2)
  expect_gt(spread$spread, 1.5)
  # WHY: the quantile z's spread as much, so the sd of all z's is 1.74 > 1.5, but only the mean z's count.
  expect_gt(stats::sd(z_set(q = rep(c(2, -2), 42))), 1.5)
  expect_true(verdict(q = rep(c(2, -2), 42))$pass)

  four <- verdict(q = replace(q, 1:4, c(4, -4.9, 3.1, -3.5)))
  expect_false(four$pass)
  expect_identical(sum(four$over), 4L)
  expect_lte(four$biggest, 5)
  three <- verdict(q = replace(q, 1:3, c(4, -4.9, 3.1)))
  expect_true(three$pass)
  expect_identical(sum(three$over), 3L)
  # WHY: a |z| of 3 is not above 3.
  at_three <- verdict(q = replace(q, 1:4, c(3, -3, 3, -3)))
  expect_true(at_three$pass)
  expect_identical(sum(at_three$over), 0L)

  # WHY: the sd bound itself: mean z's whose squares sum to 60.75, so their variance is 60.75 / 27 = 2.25 and their sd 1.5, each step exact in binary, pass; a hair above fails.
  at_sd <- c(rep(c(2.5, -2.5), 4), 2, -2, 1, -1, 0.5, -0.5, 0.25, -0.25, 0.25, -0.25, rep(0, 10))
  expect_identical(stats::sd(at_sd), 1.5)
  expect_true(verdict(mean = at_sd)$pass)
  expect_false(verdict(mean = at_sd * (1 + 1e-12))$pass)

  big <- verdict(q = replace(q, 1, 5.5))
  expect_false(big$pass)
  expect_identical(sum(big$over), 1L)
  expect_true(verdict(q = replace(q, 1, -5))$pass)

  # WHY: a missing z exceeds both thresholds: it is counted, and the max is Inf, though it is the only one.
  missing <- verdict(q = replace(q, 1, NA))
  expect_false(missing$pass)
  expect_identical(sum(missing$over), 1L)
  expect_identical(missing$biggest, Inf)
  missing_mean <- verdict(mean = replace(rep(0, 28), 1, NA))
  expect_false(missing_mean$pass)
  expect_identical(sum(missing_mean$over), 1L)
})

test_that("relative() writes a results-root path as $HARNESS_RESULTS/..., resolved or as given, and a project path relative to it", {
  # WHY: without the doubled slash a trailing-slash TMPDIR leaves (".../T//Rtmp..."), the given form is a substring of the resolved one.
  results <- gsub("/+", "/", withr::local_tempdir())
  withr::local_envvar(HARNESS_RESULTS = results)
  expect_identical(relative(paste("in", file.path(results, "v2s-x"))), "in $HARNESS_RESULTS/v2s-x")
  expect_identical(relative(file.path(root(), "harness", "runs", "x.txt")), "harness/runs/x.txt")
  resolved <- normalizePath(results)
  # WHY: a temporary directory on macOS resolves to a longer path that holds the given one (/private/var/... for /var/...), so replacing the given form first would leave "/private" in front.
  skip_if(identical(resolved, results), "the temporary directory is already resolved")
  expect_identical(relative(paste("in", file.path(resolved, "v2s-x"))), "in $HARNESS_RESULTS/v2s-x")
})

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

test_that("compare_runs.R stops on a command line, run or table it cannot read, with its prefix and no absolute run directory", {
  results <- withr::local_tempdir()
  lists <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  runs <- file.path(lists, "runs.txt")
  fails_with <- function(args, message) {
    out <- compare(args)
    expect_identical(attr(out, "status"), 1L, info = shown(out))
    expect_match(out, message, fixed = TRUE, all = FALSE)
    errors <- grep("^Error", out, value = TRUE)
    expect_true(length(errors) > 0 && all(startsWith(errors, "Error: compare_runs: ")), info = shown(out))
    expect_false(prints_path(out, results), info = shown(out))
  }

  fails_with("check", "usage: Rscript R/harness/compare_runs.R stop <arm> [run_list]")
  writeLines(run_lines("A", "v2s-t9", EVE, 1L), runs)
  fails_with(c("stop", "v2s-t1", runs), "no run of arm v2s-t1 in")
  fake_run(results, "v2s-t1", EVE, 1, lp_rhat = NULL)
  writeLines(run_lines("A", "v2s-t1", EVE, 1L), runs)
  fails_with(c("stop", "v2s-t1", runs), "key_diagnostics.csv has no lp__ row ($HARNESS_RESULTS/v2s-t1_2024-11-30_2024-11-29_s1)")
  fake_run(results, "v2s-t1", EVE, 2, drop = "log_kappa")
  writeLines(run_lines("A", "v2s-t1", EVE, 2L), runs)
  fails_with(c("stop", "v2s-t1", runs), "param_draws.parquet has no column of log_kappa")
  dir <- fake_run(results, "v2s-t1", EVE, 3)
  meta <- jsonlite::read_json(file.path(dir, "meta.json"))
  meta$seed <- 4L
  jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, digits = NA)
  writeLines(run_lines("A", "v2s-t1", EVE, 3L), runs)
  fails_with(c("stop", "v2s-t1", runs), "is not of the run list's run v2s-t1_2024-11-30_2024-11-29_s3")
  # WHY: each field of the ownership guard alone, with a valid value that is another run's.
  for (case in list(list(7L, arm = "v2s-t9"), list(8L, E = F180[1]), list(9L, F = "2024-11-16"))) {
    dir <- fake_run(results, "v2s-t1", EVE, case[[1]])
    meta <- utils::modifyList(jsonlite::read_json(file.path(dir, "meta.json")), case[-1])
    jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, digits = NA)
    writeLines(run_lines("A", "v2s-t1", EVE, case[[1]]), runs)
    fails_with(c("stop", "v2s-t1", runs), paste0("is not of the run list's run v2s-t1_2024-11-30_2024-11-29_s", case[[1]]))
  }
  # WHY: a file arrow cannot read, so the error comes from a package, not from compare_runs.R's own checks.
  dir <- fake_run(results, "v2s-t1", EVE, 6)
  writeLines("not a parquet file", file.path(dir, "param_draws.parquet"))
  writeLines(run_lines("A", "v2s-t1", EVE, 6L), runs)
  fails_with(c("stop", "v2s-t1", runs), "compare_runs: ")

  csv <- file.path(lists, "equivalence.csv")
  readr::write_csv(dplyr::mutate(EQUIVALENCE_REQUIRED, verdict = "PASS"), csv)
  writeLines(run_lines("A", "v2s-u1", EVE, 1:2), runs)
  fails_with(c("perm", "v2s-u1", "v2s-u1", runs), "compare_runs: perm compares two different arms, not v2s-u1 with itself")
  fails_with(c("adopt", "v2s-u1", "v2s-u1", csv, runs), "compare_runs: adopt compares two different arms, not v2s-u1 with itself")

  # WHY: the new arm has a cell the reference form lacks.
  writeLines(c(run_lines("A", "v2s-t1", EVE, 1:2), run_lines("A", "v2s-t2", F180, 5:6)), runs)
  fails_with(c("adopt", "v2s-t1", "v2s-t2", csv, runs), "v2s-t1 and v2s-t2 have runs on different cells")
  # WHY: on each cell each arm has two fits of different seeds, the other arm's; checked from the run list alone.
  writeLines(c(
    run_lines("A", "v2s-u1", EVE, 1:2), run_lines("A", "v2s-u2", EVE, 1:3), run_lines("A", "v2s-u3", EVE, 1L),
    run_lines("A", "v2s-u4", EVE, c(1L, 3L)), run_lines("A", "v2s-u5", EVE, c(1L, 1L)),
    run_lines("A", "v2s-u6", EVE, 1:2), run_lines("B", "v2s-u6", F180, 1:2),
    run_lines("A", "v2s-u7", EVE, 1:2), run_lines("B", "v2s-u7", F180, 1:3)
  ), runs)
  three <- "v2s-u2 has 3 fits on 2024-11-30, 2024-11-29 (seeds 1, 2, 3), not 2 of different seeds"
  fails_with(c("adopt", "v2s-u1", "v2s-u2", csv, runs), three)
  fails_with(c("adopt", "v2s-u2", "v2s-u1", csv, runs), three)
  fails_with(c("adopt", "v2s-u1", "v2s-u3", csv, runs), "v2s-u3 has 1 fit on 2024-11-30, 2024-11-29 (seed 1), not 2 of different seeds")
  fails_with(c("adopt", "v2s-u1", "v2s-u4", csv, runs), "the seeds of v2s-u4 on 2024-11-30, 2024-11-29, 1 and 3, are not v2s-u1's, 1 and 2")
  fails_with(c("adopt", "v2s-u1", "v2s-u5", csv, runs), "v2s-u5 has 2 fits on 2024-11-30, 2024-11-29 (seeds 1, 1), not 2 of different seeds")
  fails_with(c("adopt", "v2s-u6", "v2s-u7", csv, runs), "v2s-u7 has 3 fits on 2021-09-25, 2021-03-29 (seeds 1, 2, 3), not 2 of different seeds")
  # WHY: the cells must be Decision 6's two, also checked from the run list alone.
  cells_are <- "adopt runs on Decision 6's two cells, 2024-11-30, 2024-11-29 and 2021-09-25, 2021-03-29: "
  writeLines(c(run_lines("A", "v2s-u1", EVE, 1:2), run_lines("A", "v2s-u8", EVE, 1:2)), runs)
  fails_with(c("adopt", "v2s-u1", "v2s-u8", csv, runs), paste0(cells_are, "no runs on 2021-09-25, 2021-03-29"))
  writeLines(c(
    run_lines("A", "v2s-u1", EVE, 1:2), run_lines("B", "v2s-u1", c("2021-09-25", "2021-09-24"), 1:2),
    run_lines("A", "v2s-u8", EVE, 1:2), run_lines("B", "v2s-u8", c("2021-09-25", "2021-09-24"), 1:2)
  ), runs)
  fails_with(
    c("adopt", "v2s-u1", "v2s-u8", csv, runs),
    paste0(cells_are, "no runs on 2021-09-25, 2021-03-29, and runs on 2021-09-25, 2021-09-24")
  )
  # WHY: every fit of both arms has the same iter_warmup and iter_sampling, read from meta.json
  # before the stop rule, which would otherwise stop on the draw count first.
  for (arm in c("v2s-i1", "v2s-i2", "v2s-i3")) for (cell in list(EVE, F180)) for (seed in 1:2) fake_run(results, arm, cell, seed)
  for (case in list(list("v2s-i2", iter_sampling = 200L), list("v2s-i3", iter_warmup = 300L))) {
    dir <- file.path(results, run_id(case[[1]], F180[1], F180[2], 2))
    meta <- utils::modifyList(jsonlite::read_json(file.path(dir, "meta.json")), case[-1])
    jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE, digits = NA)
  }
  writeLines(unlist(lapply(c("v2s-i1", "v2s-i2", "v2s-i3"), \(a) c(run_lines("A", a, EVE, 1:2), run_lines("B", a, F180, 1:2)))), runs)
  others <- function(arm) paste(c(ids("v2s-i1", EVE, 1:2), ids("v2s-i1", F180, 1:2), ids(arm, EVE, 1:2), ids(arm, F180, 1)), collapse = ", ")
  fails_with(
    c("adopt", "v2s-i1", "v2s-i2", csv, runs),
    paste0(
      "adopt compares fits of equal iterations, but their iter_sampling differ: 400 in ", others("v2s-i2"),
      "; 200 in v2s-i2_2021-09-25_2021-03-29_s2"
    )
  )
  fails_with(
    c("adopt", "v2s-i1", "v2s-i3", csv, runs),
    paste0(
      "adopt compares fits of equal iterations, but their iter_warmup differ: 400 in ", others("v2s-i3"),
      "; 300 in v2s-i3_2021-09-25_2021-03-29_s2"
    )
  )

  readr::write_csv(tibble::tibble(E = EVE[1], F = EVE[2], verdict = "PASS"), csv)
  writeLines(c(
    run_lines("A", "v2s-t1", EVE, 5:6), run_lines("B", "v2s-t1", F180, 5:6),
    run_lines("A", "v2s-t2", EVE, 5:6), run_lines("B", "v2s-t2", F180, 5:6)
  ), runs)
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
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("A v2s-pa 2017-10-28 2017-10-27 1", "A v2s-pb 2017-10-28 2017-10-27 1"), runs)
  readr::write_csv(dplyr::mutate(EQUIVALENCE_REQUIRED, verdict = "PASS"), csv)
  for (args in list(c("stop", "v2s-pa", runs), c("adopt", "v2s-pa", "v2s-pb", csv, runs), c("perm", "v2s-pa", "v2s-pb", runs))) {
    out <- compare(args)
    expect_identical(attr(out, "status"), 1L, info = shown(out))
    expect_match(out, "compare_runs: 2017 is not compared before the freeze (spec \u00a75.7)", fixed = TRUE, all = FALSE)
    expect_false(any(grepl("meta.json|parquet", out)))
  }
})

test_that("compare_runs.R refuses a 2017 cell on any line of the run list, a third arm's included", {
  # WHY: this reads the repository's real freeze state; Plan 4 replaces the test once the freeze is sealed.
  skip_if(is_frozen(), "the freeze is sealed")
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  runs <- withr::local_tempfile(fileext = ".txt")
  csv <- withr::local_tempfile(fileext = ".csv")
  # WHY: the named arms' runs are on Decision 6's cells and only a third arm's line is on 2017,
  # so a refusal limited to the named arms' lines would go on to read their (absent) runs.
  writeLines(c(
    run_lines("A", "v2s-pa", EVE, 1:2), run_lines("B", "v2s-pa", F180, 1:2),
    run_lines("A", "v2s-pb", EVE, 1:2), run_lines("B", "v2s-pb", F180, 1:2),
    "B v2s-px 2017-10-28 2017-10-27 1"
  ), runs)
  readr::write_csv(dplyr::mutate(EQUIVALENCE_REQUIRED, verdict = "PASS"), csv)
  for (args in list(c("stop", "v2s-pa", runs), c("adopt", "v2s-pa", "v2s-pb", csv, runs), c("perm", "v2s-pa", "v2s-pb", runs))) {
    out <- compare(args)
    expect_identical(attr(out, "status"), 1L, info = shown(out))
    expect_match(out, "compare_runs: 2017 is not compared before the freeze (spec \u00a75.7)", fixed = TRUE, all = FALSE)
    expect_false(any(grepl("meta.json|parquet", out)))
  }
})
