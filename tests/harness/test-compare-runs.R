# compare_runs.R's stop rule (the Phase 2 plan's Decision 5), run as a script on synthetic run
# directories: made-up meta.json, key_diagnostics.csv and parquets, never a fit's draws; and
# relative() (constants.R), which keeps absolute paths out of its messages.
box::use(
  R / harness / runs[run_id],
  R / harness / freeze[is_frozen],
  R / harness / v2[V2_PARAM_VARIABLES],
  R / harness / v2_reference[V2_LOG_CAPS],
  R / harness / constants[relative]
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
    arm = arm, E = cell[1], F = cell[2], id = basename(dir), seed = seed, iter_warmup = ITERS,
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

test_that("the caps the stop rule reads are Decision 4's and the Stan file's, and V2's param_draws holds each", {
  expect_identical(V2_LOG_CAPS, CAPS)
  stan <- readLines(root("Stan", "forecast_v2.stan"))
  cap <- function(name) {
    line <- grep(paste0("^ *real ", name, " = "), stan, value = TRUE)
    expect_length(line, 1)
    as.numeric(sub("^.*= *([0-9.]+);.*$", "\\1", line))
  }
  expect_identical(c(log_phi = cap("phi_cap"), log_phi_K = cap("phi_K_cap"), log_kappa = cap("kappa_cap")), CAPS)
  # WHY: v2_extract() writes every variable of V2_PARAM_VARIABLES into param_draws.parquet.
  expect_true(all(names(CAPS) %in% V2_PARAM_VARIABLES))
})

test_that("the run list holds V2 and R0 on the three cells at seed 2701, and the stop rule reads it by default", {
  lines <- trimws(readLines(root("harness", "runs", "v2.txt")))
  runs <- lines[nzchar(lines) & !startsWith(lines, "#")]
  eve21 <- c("2021-09-25", "2021-09-24")
  expect_identical(runs, c(
    run_lines("A", "v2", EVE, 2701L), run_lines("A", "r0", EVE, 2701L),
    run_lines("B", "v2", eve21, 2701L), run_lines("B", "r0", eve21, 2701L),
    run_lines("B", "v2", F180, 2701L), run_lines("B", "r0", F180, 2701L)
  ))
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  out <- compare(c("stop", "v2"))
  expect_identical(attr(out, "status"), 1L)
  # WHY: a run directory is printed under $HARNESS_RESULTS, never as an absolute path a step log could pick up.
  expect_match(out, "compare_runs: no meta.json in $HARNESS_RESULTS/v2_2024-11-30_2024-11-29_s2701:", fixed = TRUE, all = FALSE)
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
  # WHY: the adopt and perm commands are archived, so their command lines are usage errors.
  fails_with(c("perm", "v2s-pa", "v2s-pb"), "usage: Rscript R/harness/compare_runs.R stop <arm> [run_list]")
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
})

test_that("compare_runs.R refuses a 2017 cell before the freeze, before it reads the run", {
  # WHY: this reads the repository's real freeze state; Plan 4 replaces the test once the freeze is sealed.
  skip_if(is_frozen(), "the freeze is sealed")
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  runs <- withr::local_tempfile(fileext = ".txt")
  writeLines("A v2s-pa 2017-10-28 2017-10-27 1", runs)
  out <- compare(c("stop", "v2s-pa", runs))
  expect_identical(attr(out, "status"), 1L, info = shown(out))
  expect_match(out, "compare_runs: 2017 is not compared before the freeze (spec \u00a75.7)", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("meta.json|parquet", out)))
})

test_that("compare_runs.R refuses a 2017 cell on any line of the run list, another arm's included", {
  # WHY: this reads the repository's real freeze state; Plan 4 replaces the test once the freeze is sealed.
  skip_if(is_frozen(), "the freeze is sealed")
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  runs <- withr::local_tempfile(fileext = ".txt")
  # WHY: only another arm's line is on 2017, so a refusal limited to the named arm's lines would
  # go on to read its (absent) runs.
  writeLines(c(run_lines("A", "v2s-pa", EVE, 1:2), run_lines("B", "v2s-pa", F180, 1:2), "B v2s-px 2017-10-28 2017-10-27 1"), runs)
  out <- compare(c("stop", "v2s-pa", runs))
  expect_identical(attr(out, "status"), 1L, info = shown(out))
  expect_match(out, "compare_runs: 2017 is not compared before the freeze (spec \u00a75.7)", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("meta.json|parquet", out)))
})
