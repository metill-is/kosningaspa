box::use(
  R / harness / runs[expected_meta, meta_mismatch, results_root, run_id, stan_data_sha],
  R / harness / r0[r0_stan_data, R0_EXTRACT_VARIABLES, R0_KJORDAEMI],
  R / harness / info_set[info_set, read_inputs],
  R / harness / scorer[read_run_meta, score_cell],
  R / harness / arms[arm],
  R / harness / freeze[is_frozen],
  R / harness / v2[v2_stan_data, V2_PARAM_VARIABLES]
)

TIER0 <- c("would run A r0 2024-11-30 2024-11-29 1101", "would run B r0 2021-09-25 2021-03-29 1102")
PARQUETS <- paste0(c("eday_kjordaemi", "eday_national", "latent_national", "param_draws"), ".parquet")
REFUSED_2017 <- "fit_cell: 2017 is not fitted before the freeze (spec \u00a75.7)"

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

test_that("the queue's dry run prints the Tier 0 runs and writes nothing", {
  results <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results, DRY_RUN = "1")
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

test_that("the queue runs each lane's runs in order, the lanes side by side, and logs how each ended", {
  results <- withr::local_tempdir()
  stub <- withr::local_tempdir()
  calls <- file.path(stub, "calls.txt")
  writeLines(c(
    "#!/bin/sh",
    'echo "${HARNESS_SLOW:-} $LC_ALL $*" >> "$CALLS"',
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
    HARNESS_RESULTS = results, DRY_RUN = NA, HARNESS_SLOW = NA, CALLS = calls,
    PATH = paste(stub, Sys.getenv("PATH"), sep = ":")
  )
  out <- queue(c(list, "200", "300"))
  expect_identical(attr(out, "status"), 1L)
  fits <- readLines(calls)
  expect_setequal(fits, paste(" en_US.UTF-8 R/harness/fit_cell.R r0", c(
    "2024-11-30 2024-11-29 11", "2021-09-25 2021-03-29 12", "2024-11-30 2024-11-29 13"
  ), "200 300"))
  expect_lt(grep(" 11 200 300$", fits), grep(" 13 200 300$", fits))
  log <- readLines(file.path(results, "logs", "queue.log"))
  expect_match(log[1], "^QUEUE START ")
  expect_identical(log[length(log)], "QUEUE FINISHED")
  ends <- sort(grep("^end ", log, value = TRUE))
  expect_identical(ends, c(
    "end r0_2021-09-25_2021-03-29_s12 rc=0 DIAG run=r0_2021-09-25_2021-03-29_s12 stub",
    "end r0_2024-11-30_2024-11-29_s11 rc=0 DIAG run=r0_2024-11-30_2024-11-29_s11 stub",
    "end r0_2024-11-30_2024-11-29_s13 rc=4 DIAG run=r0_2024-11-30_2024-11-29_s13 stub"
  ))
  expect_length(grep("^start [AB] r0_", log), 3)
  expect_identical(readLines(file.path(results, "logs", "r0_2024-11-30_2024-11-29_s11.log"))[1], "noise")
})

test_that("a queue killed by SIGTERM mid-fit exits 143 and logs QUEUE ABORTED", {
  results <- withr::local_tempdir()
  bin <- withr::local_tempdir()
  go <- file.path(bin, "go")
  calls <- file.path(bin, "calls.txt")
  # WHY: each fit waits for a file that comes only after the kill, so the queue is killed mid-fit.
  # A fit also stops once that file's folder is gone, so none outlives the test's temporary folder.
  stub(bin, "Rscript", c('echo "$*" >> "$CALLS"', 'while [ ! -f "$GO" ] && [ -d "${GO%/*}" ]; do sleep 0.1; done', "exit 0"))
  withr::local_envvar(
    HARNESS_RESULTS = results, DRY_RUN = NA, GO = go, CALLS = calls,
    PATH = paste(bin, Sys.getenv("PATH"), sep = ":")
  )
  q <- processx::process$new("zsh", c("R/harness/queue.sh", "harness/runs/tier0.txt", "200", "200"), wd = root())
  withr::defer(q$kill())
  withr::defer(file.create(go))
  wait_for(\() file.exists(calls) && length(readLines(calls)) == 2, "both lanes' first fits")
  q$signal(tools::SIGTERM)
  q$wait(10000)
  expect_false(q$is_alive())
  expect_identical(q$get_exit_status(), 143L)
  log <- readLines(file.path(results, "logs", "queue.log"))
  expect_match(log[1], paste0("^QUEUE START .* \\(pid ", q$get_pid(), "\\)$"))
  expect_identical(log[length(log)], paste0("QUEUE ABORTED rc=143 killed (TERM) (pid ", q$get_pid(), ")"))
  expect_false(any(grepl("^(QUEUE FINISHED|end )", log)))
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
  expected <- expected_meta(
    "r0", 1, 10, 10, root(a$stan_file), root(a$config), stan_data_sha(built$data),
    config = a$read_config(root(a$config))
  )

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
  # WHY: a 2024 cell is scored with no freeze, so the scorer reaches the result reader.
  expect_error(score_cell(dir, truth = reader), "truth read", fixed = TRUE)
  expect_identical(calls, 1L)

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
  out <- fit_cell(c("v2", "2017-10-28", "2017-10-27", "1", "10", "10"))
  expect_identical(attr(out, "status"), 1L)
  expect_match(out, REFUSED_2017, fixed = TRUE, all = FALSE)
  expect_false(any(grepl("^(DIAG|DONE|SKIP|STALE)", out)))
  expect_false(any(grepl("Running MCMC", out, fixed = TRUE)))
  expect_length(list.files(results, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE), 0)
  expect_false(dir.exists(root("results", "harness", "v2_2017-10-28_2017-10-27_s1")))
})

test_that("fit_cell.R fits v2 on 2024 eve: the five parquets, n_leapfrog, the scorer's meta fields and an lp__ row", {
  skip_unless_slow()
  skip_without_snapshot()
  results <- withr::local_tempdir()
  E <- "2024-11-30"
  F <- "2024-11-29"
  id <- run_id("v2", E, F, 1)
  dir <- file.path(results, id)
  a <- arm("v2")
  # WHY: stage 1 is fitted once into the project's results cache, then copied to where the run's fit_cell.R looks for it.
  built <- v2_stan_data(info_set(as.Date(E), as.Date(F), read_inputs()), a$read_config(root(a$config)))
  expect_true(file.copy(file.path(results_root(), "stage1"), results, recursive = TRUE))
  withr::local_envvar(HARNESS_RESULTS = results, HARNESS_KEEP_CMDSTAN = "1")
  expected <- expected_meta(
    "v2", 1, 10, 10, root(a$stan_file), root(a$config), stan_data_sha(built$data),
    config = a$read_config(root(a$config))
  )

  out <- fit_cell(c("v2", E, F, "1", "10", "10"))
  expect_null(attr(out, "status"), info = paste(utils::tail(out, 30), collapse = "\n"))
  expect_true(paste("DONE", dir) %in% out)
  parquets <- c(PARQUETS, "ppc_results.parquet")
  expect_true(all(file.exists(file.path(dir, c(parquets, "key_diagnostics.csv", "meta.json")))))
  expect_identical(sort(list.files(dir, pattern = "\\.parquet$")), sort(parquets))

  meta <- jsonlite::read_json(file.path(dir, "meta.json"))
  expect_identical(meta_mismatch(meta, expected), character(0))
  expect_true(all(c("arm", "E", "F", "diagnostics", "gate", names(built$meta)) %in% names(meta)))
  expect_false("role" %in% names(meta))
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
  # WHY: fit_cell.R writes a character value of the builder's meta as a JSON array, so it reads back as a list.
  expect_identical(unlist(meta$stage1_sha256), built$meta$stage1_sha256)
  expect_match(built$meta$stage1_sha256, "^[0-9a-f]{64}$")
  box::use(R / harness / scorer[META_FIELDS])
  run_meta <- read_run_meta(dir)
  expect_true(all(META_FIELDS %in% names(run_meta)))
  expect_identical(run_meta$stage1_sha256, built$meta$stage1_sha256)
  expect_identical(run_meta$party_order, built$meta$party_order)
  expect_identical(names(run_meta$n_pred_k), built$meta$kjordaemi_levels)
  # WHY: the 2024 cell passes the freeze guard, so the scorer reaches the result reader.
  expect_error(score_cell(dir, truth = function(...) stop("truth read")), "truth read", fixed = TRUE)

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
  expect_identical(unique(sub("\\[.*$", "", pd$names[-(1:3)])), V2_PARAM_VARIABLES)
  expect_equal(pd$rows, 40)
  keyd <- utils::read.csv(file.path(dir, "key_diagnostics.csv"), check.names = FALSE, encoding = "UTF-8")
  expect_named(keyd, c("variable", "rhat", "ess_bulk", "ess_tail"))
  # WHY: the stop rule reads lp__'s R-hat from this row (R/harness/compare_runs.R).
  lp <- keyd[keyd$variable == "lp__", ]
  expect_identical(nrow(lp), 1L)
  expect_true(is.finite(lp$rhat))
})
