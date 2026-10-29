box::use(
  R / harness / runs[expected_meta, meta_mismatch, run_id, stan_data_sha],
  R / harness / r0[r0_stan_data, R0_EXTRACT_VARIABLES, R0_KJORDAEMI],
  R / harness / info_set[info_set, read_inputs],
  R / harness / scorer[read_run_meta, score_cell],
  R / harness / arms[arm],
  R / harness / freeze[is_frozen],
  R / harness / v2[v2_stan_data]
)

TIER0 <- c("would run A r0 2024-11-30 2024-11-29 1101", "would run B r0 2021-09-25 2021-03-29 1102")
PARQUETS <- paste0(c("eday_kjordaemi", "eday_national", "latent_national", "param_draws"), ".parquet")
REFUSED_2017 <- "fit_cell: 2017 is not fitted before the freeze (spec \u00a75.7)"
GUARD <- "--file=R/harness/fit_cell.R"

# queue.sh run from the project root; its output, with the exit status as attribute "status"
# when it is not 0.
queue <- function(args) {
  withr::with_dir(root(), suppressWarnings(system2("zsh", c("R/harness/queue.sh", args), stdout = TRUE, stderr = TRUE)))
}

# An executable `name` in `dir` running `lines` under sh.
stub <- function(dir, name, lines) {
  writeLines(c("#!/bin/sh", lines), file.path(dir, name))
  Sys.chmod(file.path(dir, name), "0755")
}

# Polls `ok()` every 0.1 s for up to 10 s; an error naming `what` if it never holds.
wait_for <- function(ok, what) {
  for (i in 1:100) {
    if (isTRUE(ok())) {
      return(invisible(TRUE))
    }
    Sys.sleep(0.1)
  }
  stop("timed out waiting for ", what)
}

ABORTED <- "^QUEUE ABORTED rc=[0-9]+ .* \\(pid [0-9]+\\)$"

# A process whose command line holds the queue guard's pattern, as a running fit_cell.R's does.
fake_fit <- function(env = parent.frame()) {
  p <- processx::process$new("sh", c("-c", "read x", GUARD), stdin = "|")
  withr::defer(p$kill(), envir = env)
  # WHY: read the process's own command line; a pgrep run through system2() would match its parent sh.
  shown <- function() paste(suppressWarnings(system2("ps", c("-o", "command=", "-p", p$get_pid()), stdout = TRUE)), collapse = "")
  for (i in 1:50) {
    if (grepl(GUARD, shown(), fixed = TRUE)) break
    Sys.sleep(0.1)
  }
  if (!grepl(GUARD, shown(), fixed = TRUE)) stop("the fake fit did not start")
  p
}

test_that("the queue's dry run prints the Tier 0 runs and writes nothing", {
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results, DRY_RUN = "1", FORCE = NA)
  out <- queue(c("harness/runs/tier0.txt", "200", "200"))
  expect_null(attr(out, "status"))
  expect_identical(as.vector(out), TIER0)
  expect_length(list.files(results, recursive = TRUE, all.files = TRUE, include.dirs = TRUE), 0)
})

test_that("the queue refuses a bad command line or run list", {
  lists <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir(), DRY_RUN = "1")
  out <- queue(c("harness/runs/tier0.txt", "200"))
  expect_identical(attr(out, "status"), 2L)
  expect_match(out, "usage: zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>", fixed = TRUE, all = FALSE)
  bad <- file.path(lists, "bad.txt")
  writeLines(c("# lane arm E F seed", "C r0 2024-11-30 2024-11-29 1101"), bad)
  out <- queue(c(bad, "200", "200"))
  expect_identical(attr(out, "status"), 2L)
  expect_match(out, "lane must be A or B", fixed = TRUE, all = FALSE)
  writeLines("A r0 2024-11-30 2024-11-29", bad)
  out <- queue(c(bad, "200", "200"))
  expect_identical(attr(out, "status"), 2L)
  expect_match(out, "5 fields", fixed = TRUE, all = FALSE)
  out <- queue(c(file.path(lists, "absent.txt"), "200", "200"))
  expect_identical(attr(out, "status"), 2L)
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results, DRY_RUN = NA)
  out <- queue(c(bad, "200", "200"))
  expect_identical(attr(out, "status"), 2L)
  log <- readLines(file.path(results, "logs", "queue.log"))
  expect_length(log, 1)
  expect_match(log, "^QUEUE ABORTED rc=2 .*5 fields .* \\(pid [0-9]+\\)$")
})

test_that("the queue will not start beside a running fit_cell.R unless FORCE=1, and stops on a failed preflight", {
  results <- withr::local_tempdir()
  bin <- withr::local_tempdir()
  stub(bin, "Rscript", "exit 3")
  withr::local_envvar(HARNESS_RESULTS = results, DRY_RUN = NA, FORCE = NA, PATH = paste(bin, Sys.getenv("PATH"), sep = ":"))
  logs <- file.path(results, "logs")
  fake_fit()
  out <- queue(c("harness/runs/tier0.txt", "200", "200"))
  expect_identical(attr(out, "status"), 1L)
  expect_match(out, "a fit_cell.R is running", fixed = TRUE, all = FALSE)
  log <- readLines(file.path(logs, "queue.log"))
  expect_length(log, 1)
  expect_match(log, ABORTED)
  expect_match(log, "^QUEUE ABORTED rc=1 a fit_cell.R is running ")
  expect_false(dir.exists(file.path(logs, "queue.lock")))
  expect_false(file.exists(file.path(logs, "preflight.log")))

  withr::local_envvar(FORCE = "1")
  out <- queue(c("harness/runs/tier0.txt", "200", "200"))
  expect_identical(attr(out, "status"), 1L)
  log <- readLines(file.path(logs, "queue.log"))[-1]
  expect_match(log[1], "^QUEUE START .* \\(pid [0-9]+\\)$")
  expect_match(log[2], "^preflight FAIL rc=3 ")
  expect_length(log, 3)
  expect_match(log[3], "^QUEUE ABORTED rc=1 preflight failed \\(rc=3\\)")
  expect_match(log[3], ABORTED)
  expect_false(dir.exists(file.path(logs, "queue.lock")))
  expect_true(file.exists(file.path(logs, "preflight.log")))
})

test_that("the queue's guard refuses to start when pgrep fails", {
  results <- withr::local_tempdir()
  bin <- withr::local_tempdir()
  stub(bin, "Rscript", "exit 3")
  withr::local_envvar(HARNESS_RESULTS = results, DRY_RUN = NA, FORCE = NA, PATH = paste(bin, Sys.getenv("PATH"), sep = ":"))
  for (rc in c(2L, 3L, 127L)) {
    stub(bin, "pgrep", paste("exit", rc))
    out <- queue(c("harness/runs/tier0.txt", "200", "200"))
    expect_identical(attr(out, "status"), 1L)
    expect_match(out, paste0("the guard could not look for a running fit_cell.R (pgrep exit ", rc, ")"), fixed = TRUE, all = FALSE)
    log <- readLines(file.path(results, "logs", "queue.log"))
    expect_match(log[length(log)], paste0("^QUEUE ABORTED rc=1 the guard could not look .*pgrep exit ", rc))
    expect_false(any(startsWith(log, "QUEUE START")))
  }
})

test_that("a second queue is refused while the first holds the lock, and a killed queue logs QUEUE ABORTED", {
  results <- withr::local_tempdir()
  bin <- withr::local_tempdir()
  go <- file.path(bin, "go")
  calls <- file.path(bin, "calls.txt")
  # WHY: the preflight waits for a file that never comes, so the first queue holds the lock in its preflight.
  stub(bin, "Rscript", c('echo "$*" >> "$CALLS"', 'while [ ! -f "$GO" ]; do sleep 0.1; done', "exit 0"))
  withr::local_envvar(
    HARNESS_RESULTS = results, DRY_RUN = NA, FORCE = NA, GO = go, CALLS = calls,
    PATH = paste(bin, Sys.getenv("PATH"), sep = ":")
  )
  lock <- file.path(results, "logs", "queue.lock")
  first <- processx::process$new("zsh", c("R/harness/queue.sh", "harness/runs/tier0.txt", "200", "200"), wd = root())
  withr::defer(first$kill())
  wait_for(\() file.exists(calls) && any(grepl("tests/run.R", readLines(calls))), "the first queue's preflight")
  expect_identical(readLines(file.path(lock, "pid")), as.character(first$get_pid()))

  out <- queue(c("harness/runs/tier0.txt", "200", "200"))
  expect_identical(attr(out, "status"), 1L)
  expect_match(out, paste0("another queue holds ", lock, " (pid ", first$get_pid(), ")"), fixed = TRUE, all = FALSE)
  expect_true(first$is_alive())
  expect_true(dir.exists(lock))

  first$signal(tools::SIGTERM)
  first$wait(10000)
  expect_false(first$is_alive())
  expect_identical(first$get_exit_status(), 143L)
  log <- readLines(file.path(results, "logs", "queue.log"))
  expect_match(log[1], paste0("^QUEUE START .* \\(pid ", first$get_pid(), "\\)$"))
  expect_match(log, paste0("^QUEUE ABORTED rc=1 another queue holds .* \\(pid [0-9]+\\)$"), all = FALSE)
  expect_identical(log[length(log)], paste0("QUEUE ABORTED rc=143 killed (TERM) (pid ", first$get_pid(), ")"))
  expect_false(dir.exists(lock))
  expect_false(any(grepl("^(preflight|start|end) ", log)))
})

test_that("the queue runs each lane's runs in order, the lanes side by side, and logs how each ended", {
  results <- withr::local_tempdir()
  stub <- withr::local_tempdir()
  calls <- file.path(stub, "calls.txt")
  writeLines(c(
    "#!/bin/sh",
    'echo "${HARNESS_SLOW:-} $LC_ALL $*" >> "$CALLS"',
    'if [ "$1" = "tests/run.R" ]; then exit 0; fi',
    "echo noise",
    'echo "DIAG run=$2_$3_$4_s$5 stub"',
    'if [ "$5" = "13" ]; then exit 4; fi'
  ), file.path(stub, "Rscript"))
  Sys.chmod(file.path(stub, "Rscript"), "0755")
  list <- file.path(stub, "runs.txt")
  writeLines(c(
    "# lane arm E F seed", "A r0 2024-11-30 2024-11-29 11", "", "B r0 2021-09-25 2021-03-29 12",
    "  A   r0 2024-11-30 2024-11-29 13"
  ), list)
  withr::local_envvar(
    HARNESS_RESULTS = results, DRY_RUN = NA, FORCE = "1", HARNESS_SLOW = NA, CALLS = calls,
    PATH = paste(stub, Sys.getenv("PATH"), sep = ":")
  )
  out <- queue(c(list, "200", "300"))
  expect_identical(attr(out, "status"), 1L)
  made <- readLines(calls)
  expect_identical(made[1], "1 en_US.UTF-8 tests/run.R")
  fits <- made[-1]
  expect_setequal(fits, paste(" en_US.UTF-8 R/harness/fit_cell.R r0", c(
    "2024-11-30 2024-11-29 11", "2021-09-25 2021-03-29 12", "2024-11-30 2024-11-29 13"
  ), "200 300"))
  expect_lt(grep(" 11 200 300$", fits), grep(" 13 200 300$", fits))
  log <- readLines(file.path(results, "logs", "queue.log"))
  expect_match(log[1], "^QUEUE START ")
  expect_identical(log[2], "preflight PASS")
  expect_identical(log[length(log)], "QUEUE FINISHED")
  expect_false(dir.exists(file.path(results, "logs", "queue.lock")))
  ends <- sort(grep("^end ", log, value = TRUE))
  expect_identical(ends, c(
    "end r0_2021-09-25_2021-03-29_s12 rc=0 DIAG run=r0_2021-09-25_2021-03-29_s12 stub",
    "end r0_2024-11-30_2024-11-29_s11 rc=0 DIAG run=r0_2024-11-30_2024-11-29_s11 stub",
    "end r0_2024-11-30_2024-11-29_s13 rc=4 DIAG run=r0_2024-11-30_2024-11-29_s13 stub"
  ))
  expect_length(grep("^start [AB] r0_", log), 3)
  expect_identical(readLines(file.path(results, "logs", "r0_2024-11-30_2024-11-29_s11.log"))[1], "noise")
})

test_that("every variable the R0 extraction reads is declared in the R0 Stan file", {
  skip_unless_slow()
  v <- cmdstanr::cmdstan_model(root(arm("r0")$stan_file), compile = FALSE)$variables()
  declared <- c(names(v$parameters), names(v$transformed_parameters), names(v$generated_quantities))
  expect_true(all(R0_EXTRACT_VARIABLES %in% declared), info = paste(setdiff(R0_EXTRACT_VARIABLES, declared), collapse = ", "))
})

test_that("fit_cell.R fits a cell, skips it while meta.json matches, and retires it when not", {
  skip_unless_slow()
  skip_without_snapshot()
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  E <- "2024-11-30"
  F <- "2024-11-29"
  id <- run_id("r0", E, F, 1)
  dir <- file.path(results, id)
  fit <- function() {
    withr::with_dir(root(), suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"), c("R/harness/fit_cell.R", "r0", E, F, "1", "10", "10"),
      stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
    )))
  }
  shown <- function(out) paste(utils::tail(out, 30), collapse = "\n")
  a <- arm("r0")
  built <- r0_stan_data(info_set(as.Date(E), as.Date(F), read_inputs()), a$read_config(root(a$config)))
  expected <- expected_meta("r0", 1, 10, 10, root(a$stan_file), root(a$config), stan_data_sha(built$data))

  out <- fit()
  expect_null(attr(out, "status"), info = shown(out))
  diag <- grep("^DIAG run=", out, value = TRUE)
  expect_length(diag, 1)
  expect_true(startsWith(diag, paste0("DIAG run=", id, " minutes=")))
  expect_true(paste("DONE", dir) %in% out)
  expect_true(all(file.exists(file.path(dir, c(PARQUETS, "key_diagnostics.csv", "meta.json")))))
  inputs <- file.path(dir, "inputs", c("stan_data.json", "inputs.json"))
  expect_identical(unname(file.access(inputs, 2)), c(-1L, -1L))
  expect_identical(list.files(dir, pattern = "\\.csv$", recursive = TRUE), "key_diagnostics.csv")

  meta <- jsonlite::read_json(file.path(dir, "meta.json"))
  expect_identical(meta_mismatch(meta, expected), character(0))
  expect_true(all(c(
    names(expected), "arm", "E", "F", names(built$meta), "minutes", "diagnostics", "gate", "git_head",
    "snapshot_manifest_sha256", "cmdstan_version", "cmdstanr_version"
  ) %in% names(meta)))
  expect_identical(c(meta$arm, meta$E, meta$F), c("r0", E, F))
  expect_named(meta$gate, c("pass", "reasons"))
  inputs_meta <- jsonlite::read_json(inputs[2])
  expect_identical(meta_mismatch(inputs_meta, expected), character(0))
  expect_true(all(c("arm", "E", "F", names(built$meta), "git_head", "snapshot_manifest_sha256") %in% names(inputs_meta)))

  run_meta <- read_run_meta(dir)
  expect_identical(run_meta$party_order, built$meta$party_order)
  expect_identical(run_meta$n_pred_k, stats::setNames(as.numeric(built$meta$n_pred_k), R0_KJORDAEMI))
  expect_identical(run_meta$coalition_event$parties, built$meta$coalition_event$parties)
  calls <- 0L
  reader <- function(...) {
    calls <<- calls + 1L
    stop("truth read")
  }
  expect_error(score_cell(dir, truth = reader), "no freeze")
  expect_identical(calls, 0L)

  kd <- arrow::read_parquet(file.path(dir, "eday_kjordaemi.parquet"))
  expect_identical(sort(unique(kd$.draw)), seq_len(40L))
  expect_setequal(kd$kjordaemi, R0_KJORDAEMI)
  expect_setequal(kd$flokkur, built$meta$party_order)
  totals <- tapply(kd$votes, list(kd$.draw, kd$kjordaemi), sum)[, R0_KJORDAEMI]
  # WHY: each kjördæmi's draws sum to its own n_pred_k, so a kjördæmi label off by one Stan index shows.
  expect_true(all(totals == matrix(built$meta$n_pred_k, 40, 6, byrow = TRUE)))
  pd <- arrow::read_parquet(file.path(dir, "param_draws.parquet"))
  keyd <- utils::read.csv(file.path(dir, "key_diagnostics.csv"), check.names = FALSE, encoding = "UTF-8")
  expect_named(keyd, c("variable", "rhat", "ess_bulk", "ess_tail"))
  P <- length(built$meta$party_order)
  expect_equal(sum(startsWith(keyd$variable, "scored_nat[")), P)
  expect_equal(sum(startsWith(keyd$variable, "scored_kd[")), 6 * P)
  expect_identical(sum(keyd$variable == "lp__"), 1L)
  expect_setequal(setdiff(keyd$variable, c(keyd$variable[grepl("^scored_(nat|kd)\\[", keyd$variable)], "lp__")), setdiff(names(pd), c(".chain", ".iteration", ".draw")))

  out <- fit()
  expect_null(attr(out, "status"), info = shown(out))
  expect_true(paste0("SKIP ", id, " (meta.json matches)") %in% out)
  expect_false(any(grepl("^(DIAG|STALE)", out)))

  # WHY: the sha-skip hashes freshly built data; one that hashed the frozen file would call this run STALE.
  Sys.chmod(inputs[1], "0644")
  cat("\n", file = inputs[1], append = TRUE)
  Sys.chmod(inputs[1], "0444")
  expect_false(identical(digest::digest(file = inputs[1], algo = "sha256"), expected$data_sha256))
  out <- fit()
  expect_null(attr(out, "status"), info = shown(out))
  expect_true(paste0("SKIP ", id, " (meta.json matches)") %in% out)
  expect_false(any(grepl("^(DIAG|STALE)", out)))

  path <- file.path(dir, "meta.json")
  Sys.chmod(path, "0644")
  text <- readLines(path, encoding = "UTF-8")
  edited <- sub('^(\\s*"seed": )1(,?)$', "\\12\\2", text)
  expect_equal(sum(edited != text), 1)
  writeLines(edited, path)
  out <- fit()
  expect_null(attr(out, "status"), info = shown(out))
  expect_true(paste("STALE", id, "seed") %in% out)
  stale <- list.files(file.path(results, "_stale"), full.names = TRUE)
  expect_length(stale, 1)
  expect_match(basename(stale), paste0("^", id, "_[0-9]{14}$"))
  expect_identical(jsonlite::read_json(file.path(stale, "meta.json"))$seed, 2L)
  expect_length(grep("^DIAG run=", out), 1)
  expect_identical(meta_mismatch(jsonlite::read_json(path), expected), character(0))
})

# fit_cell.R run from the project root under LC_ALL=en_US.UTF-8; its output, with the exit
# status as attribute "status" when it is not 0.
fit_cell <- function(args) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/fit_cell.R", args),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
}

test_that("fit_cell.R refuses a 2017 cell before the freeze, before it builds or writes anything", {
  # WHY: this reads the repository's real freeze state; Plan 4 replaces the test once the freeze is sealed.
  skip_if(is_frozen(), "the freeze is sealed")
  results <- withr::local_tempdir()
  # WHY: with CMDSTAN naming an empty directory cmdstanr finds no CmdStan, so a regressed refusal stops at the first cmdstanr call, before any compile or sample.
  withr::local_envvar(HARNESS_RESULTS = results)
  local_no_cmdstan()
  out <- fit_cell(c("v2s-m0", "2017-10-28", "2017-10-27", "1", "10", "10"))
  expect_identical(attr(out, "status"), 1L)
  expect_match(out, REFUSED_2017, fixed = TRUE, all = FALSE)
  expect_false(any(grepl("^(DIAG|DONE|SKIP|STALE)", out)))
  expect_false(any(grepl("Running MCMC", out, fixed = TRUE)))
  expect_length(list.files(results, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE), 0)
  expect_false(dir.exists(root("results", "harness", "v2s-m0_2017-10-28_2017-10-27_s1")))
})

test_that("fit_cell.R fits v2s-m0 on 2024 eve: the five parquets, the role, n_leapfrog, the scorer's meta fields and an lp__ row", {
  skip_unless_slow()
  skip_without_snapshot()
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results, HARNESS_KEEP_CMDSTAN = "1")
  E <- "2024-11-30"
  F <- "2024-11-29"
  id <- run_id("v2s-m0", E, F, 1)
  dir <- file.path(results, id)
  a <- arm("v2s-m0")
  built <- v2_stan_data(info_set(as.Date(E), as.Date(F), read_inputs()), a$read_config(root(a$config)))
  expected <- expected_meta("v2s-m0", 1, 10, 10, root(a$stan_file), root(a$config), stan_data_sha(built$data))

  out <- fit_cell(c("v2s-m0", E, F, "1", "10", "10"))
  expect_null(attr(out, "status"), info = paste(utils::tail(out, 30), collapse = "\n"))
  expect_true(paste("DONE", dir) %in% out)
  parquets <- c(PARQUETS, "ppc_results.parquet")
  expect_true(all(file.exists(file.path(dir, c(parquets, "key_diagnostics.csv", "meta.json")))))
  expect_identical(sort(list.files(dir, pattern = "\\.parquet$")), sort(parquets))

  meta <- jsonlite::read_json(file.path(dir, "meta.json"))
  expect_identical(meta_mismatch(meta, expected), character(0))
  expect_true(all(c("arm", "E", "F", "role", "diagnostics", "gate", names(built$meta)) %in% names(meta)))
  expect_identical(meta$role, "dev")
  leapfrog <- meta$diagnostics$n_leapfrog
  expect_true(is.numeric(leapfrog) && length(leapfrog) == 1 && leapfrog >= 40 && leapfrog == round(leapfrog))
  # WHY: only the sampler's n_leapfrog__ column is read from the kept CSVs (variables = ""), no forecast value.
  csvs <- list.files(file.path(dir, "cmdstan"), pattern = "\\.csv$", full.names = TRUE)
  expect_length(csvs, 4)
  steps <- cmdstanr::read_cmdstan_csv(csvs, variables = "", sampler_diagnostics = "n_leapfrog__")
  per_chain <- apply(unclass(steps$post_warmup_sampler_diagnostics), 2, sum)
  expect_length(per_chain, 4)
  expect_equal(leapfrog, sum(per_chain))
  expect_true(leapfrog > max(per_chain))
  # WHY: v2s-m0 has no stage 1, so its stage1_sha256 is NULL, which must read back as a null, not as {}.
  expect_true("stage1_sha256" %in% names(meta))
  expect_null(meta$stage1_sha256)
  expect_true(any(grepl('^\\s*"stage1_sha256": null,?$', readLines(file.path(dir, "meta.json")))))
  box::use(R / harness / scorer[META_FIELDS])
  run_meta <- read_run_meta(dir)
  expect_true(all(META_FIELDS %in% names(run_meta)))
  expect_true("stage1_sha256" %in% names(run_meta))
  expect_null(run_meta$stage1_sha256)
  expect_identical(run_meta$party_order, built$meta$party_order)
  expect_identical(names(run_meta$n_pred_k), built$meta$kjordaemi_levels)
  expect_error(score_cell(dir, truth = function(...) stop("truth read")), "has role dev", fixed = TRUE)

  # WHY: only names, types and row counts are read: no forecast value is looked at.
  schema <- function(f) {
    x <- arrow::read_parquet(file.path(dir, f), as_data_frame = FALSE)
    list(names = names(x), types = vapply(x$schema$fields, \(fl) fl$type$ToString(), character(1)), rows = x$num_rows)
  }
  K <- length(built$meta$kjordaemi_levels)
  P <- length(built$meta$party_order)
  ids <- c(".chain", ".iteration", ".draw")
  kd <- schema("eday_kjordaemi.parquet")
  expect_identical(kd$names, c(ids, "kjordaemi", "flokkur", "votes"))
  expect_identical(kd$types, c("int32", "int32", "int32", "string", "string", "int32"))
  expect_equal(kd$rows, 40 * K * P)
  for (f in c("eday_national.parquet", "latent_national.parquet")) {
    s <- schema(f)
    expect_identical(s$names, c(ids, "flokkur", "share"), label = f)
    expect_identical(s$types, c("int32", "int32", "int32", "string", "double"), label = f)
    expect_equal(s$rows, 40 * P, label = f)
  }
  ppc <- schema("ppc_results.parquet")
  expect_identical(ppc$names, c(ids, "election", "kjordaemi", "flokkur", "votes"))
  expect_identical(ppc$types, c("int32", "int32", "int32", "date32[day]", "string", "string", "int32"))
  expect_equal(ppc$rows, 40 * built$meta$n_closed * K * P)
  pd <- schema("param_draws.parquet")
  expect_identical(pd$names[1:3], ids)
  kept <- setdiff(a$variables(a$read_config(root(a$config))), c("votes_kd", "pi_nat_eday", "pi_latent_E", "y_rep_res"))
  expect_identical(unique(sub("\\[.*$", "", pd$names[-(1:3)])), kept)
  expect_equal(pd$rows, 40)
  keyd <- utils::read.csv(file.path(dir, "key_diagnostics.csv"), check.names = FALSE, encoding = "UTF-8")
  expect_named(keyd, c("variable", "rhat", "ess_bulk", "ess_tail"))
  # WHY: the stop rule reads lp__'s R-hat from this row (R/harness/compare_runs.R).
  lp <- keyd[keyd$variable == "lp__", ]
  expect_identical(nrow(lp), 1L)
  expect_true(is.finite(lp$rhat))
})
