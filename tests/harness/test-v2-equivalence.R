# The P-step equivalence check (R/harness/v2_equivalence.R). Its CSV writer is tested with the
# file loaded as a module, so its script part does not run, and its refusals by running the
# script on command lines it stops on before anything is compiled. The check itself runs as a
# subprocess, into a temporary CSV, never the committed one: slow, and named by the step whose
# form it checks.
box::use(
  R / harness / constants[POLL_PARTIES],
  R / harness / v2_equivalence[EQUIVALENCE_SUMMARY, provisional_row, write_equivalence_row]
)

P1_CONFIG <- "harness/configs/steps/v2s-p1.json"

# The script's output lines, with attribute `status` when it exits non-zero.
run_equivalence <- function(args) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/v2_equivalence.R", args),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
}

shown <- function(out) paste(out, collapse = "\n")

# Points CMDSTAN at an empty directory for the caller's frame, after checking that cmdstanr
# then finds no CmdStan: the script then stops at its first CmdStan call, before any compile.
local_no_cmdstan <- function(env = parent.frame()) {
  withr::local_envvar(CMDSTAN = withr::local_tempdir(.local_envir = env), .local_envir = env)
  probe <- withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("-e", shQuote("cmdstanr::cmdstan_version()")),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
  if (is.null(attr(probe, "status"))) stop("cmdstanr finds CmdStan with CMDSTAN set to an empty directory: not running the script")
}

# A summary row with every EQUIVALENCE_SUMMARY column, these values set.
summary_row <- function(...) {
  row <- as.list(stats::setNames(rep("x", length(EQUIVALENCE_SUMMARY)), EQUIVALENCE_SUMMARY))
  utils::modifyList(row, list(...))
}

test_that("the CSV writer creates the file, replaces a row of the same E, F and order in place, and appends others", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "FAIL"))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(names(x), EQUIVALENCE_SUMMARY)
  expect_identical(x$verdict, "FAIL")
  write_equivalence_row(path, summary_row(E = "2021-09-25", F = "2021-03-29", order = "default", verdict = "PASS"))
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "reversed", verdict = "PASS"))
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS", reasons = "rerun"))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(x$E, c("2024-11-30", "2021-09-25", "2024-11-30"))
  expect_identical(x$order, c("default", "default", "reversed"))
  expect_identical(x$verdict, c("PASS", "PASS", "PASS"))
  expect_identical(x$reasons, c("rerun", "x", "x"))
  expect_length(list.files(dirname(path), pattern = "^equivalence-"), 0)
})

test_that("the CSV writer keeps the file's columns first and fills a column a row lacks", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_equivalence_row(path, list(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS", a = 1))
  write_equivalence_row(path, list(E = "2021-09-25", F = "2021-03-29", order = "default", verdict = "FAIL", b = 2))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(names(x), c("E", "F", "order", "verdict", "a", "b"))
  expect_identical(x$a, c("1", ""))
  expect_identical(x$b, c("", "2"))
  expect_error(write_equivalence_row(path, list(E = "2024-11-30", verdict = "PASS")), "E, F, order and verdict")
})

test_that("a provisional row replaces an earlier verdict of its cell and order, and the check's own row replaces it", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS"))
  write_equivalence_row(path, summary_row(E = "2021-09-25", F = "2021-03-29", order = "default", verdict = "PASS"))
  write_equivalence_row(path, provisional_row("2024-11-30", "2024-11-29", "default", "walk", P1_CONFIG, "sha"))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(names(x), EQUIVALENCE_SUMMARY)
  expect_identical(x$verdict, c("FAIL", "PASS"))
  expect_identical(x$reasons, c("incomplete", "x"))
  expect_identical(x$stan_sha256, c("sha", "x"))
  expect_identical(x$n_posterior, c("", "x"))
  write_equivalence_row(path, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS", reasons = ""))
  x <- utils::read.csv(path, colClasses = "character")
  expect_identical(x$E, c("2024-11-30", "2021-09-25"))
  expect_identical(x$verdict, c("PASS", "PASS"))
  expect_identical(x$reasons, c("", "x"))
})

test_that("the script refuses a bad command line before it compiles anything, and writes no CSV", {
  local_no_cmdstan()
  out <- withr::local_tempfile(fileext = ".csv")
  missing_dir <- file.path(withr::local_tempdir(), "no-such-dir", "out.csv")
  cell <- "--cell=2024-11-30,2024-11-29"
  cases <- list(
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", cell), "--out is required"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", cell, paste0("--out=", out), "--n=ten"), "--n must be a whole number"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", cell, paste0("--out=", out), "--bogus=1"), "unknown argument --bogus=1"),
    list(c("--config=configs/v2s-p1.json", "--form=walk", cell, paste0("--out=", out)), "--config must be a file under harness/configs/"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2024-11-30", paste0("--out=", out)), "--cell must be <E>,<F>"),
    list(c(paste0("--config=", P1_CONFIG), "--form=centred", cell, paste0("--out=", out)), "--form must be walk, delta or both"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", cell, paste0("--out=", missing_dir)), "--out must be a file in an existing directory"),
    list(c(paste0("--config=", P1_CONFIG), "--form=delta", cell, paste0("--out=", out)), "only the walk form's map is built: --form=walk (the delta map needs the plan's Task 13)"),
    list(c(paste0("--config=", P1_CONFIG), "--form=both", cell, paste0("--out=", out)), "only the walk form's map is built: --form=walk (the delta map needs the plan's Task 13)"),
    list(c("--config=harness/configs/steps/v2s-m0.json", "--form=walk", cell, paste0("--out=", out)), "the config's walk_form is 0"),
    list(c(paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2017-10-28,2017-10-27", paste0("--out=", out), "--n-off=5"), "a 2017 cell takes prior points only")
  )
  for (case in cases) {
    res <- run_equivalence(case[[1]])
    expect_identical(attr(res, "status"), 1L, label = shown(case[[1]]))
    expect_match(shown(res), paste0("v2_equivalence: ", case[[2]]), fixed = TRUE)
  }
  expect_false(file.exists(out))
  expect_false(file.exists(missing_dir))
})

test_that("a check that stops after its command line is accepted leaves a provisional FAIL row, not an earlier verdict", {
  skip_without_snapshot()
  local_no_cmdstan()
  out <- withr::local_tempfile(fileext = ".csv")
  write_equivalence_row(out, summary_row(E = "2024-11-30", F = "2024-11-29", order = "default", verdict = "PASS"))
  write_equivalence_row(out, summary_row(E = "2021-09-25", F = "2021-03-29", order = "default", verdict = "PASS"))
  # WHY: with no CmdStan the check stops at its first CmdStan call, the compile, after the provisional row and before any point.
  res <- run_equivalence(c(paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2024-11-30,2024-11-29", paste0("--out=", out)))
  expect_identical(attr(res, "status"), 1L)
  expect_false(any(grepl("^EQUIV ", res)), label = shown(res))
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(x$E, c("2024-11-30", "2021-09-25"))
  expect_identical(x$verdict, c("FAIL", "PASS"))
  expect_identical(x$reasons, c("incomplete", "x"))
  expect_identical(x$stan_sha256[1], digest::digest(file = root("Stan", "forecast_v2.stan"), algo = "sha256"))
})

test_that("v2s-p1: the anchored walk is the reference walk on 2021 f180", {
  skip_unless_slow()
  skip_unless_step("v2s-p1")
  skip_without_snapshot()
  out <- withr::local_tempfile(fileext = ".csv")
  res <- run_equivalence(c(
    paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2021-09-25,2021-03-29", "--n=10", "--n-off=5", paste0("--out=", out)
  ))
  expect_null(attr(res, "status"), label = shown(res))
  expect_match(res, "^EQUIV PASS", all = FALSE)
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(nrow(x), 1L)
  expect_identical(unlist(x[1, c("E", "F", "order", "verdict")], use.names = FALSE), c("2021-09-25", "2021-03-29", "default", "PASS"))
  expect_identical(unlist(x[1, c("n_posterior", "n_off", "n_prior")], use.names = FALSE), c("10", "5", "0"))
  # WHY: each log-Jacobian term must be exercised on this cell, so that its negative control is decisive.
  expect_identical(unlist(x[1, paste0("control_", c("ref_walk", "new_anchor", "new_bridge", "new_forward"))], use.names = FALSE), rep("fails", 4))
})

test_that("v2s-p1: the check fails when the bridge's conditional scale is the step's own", {
  skip_unless_slow()
  skip_unless_step("v2s-p1")
  skip_without_snapshot()
  # The bridge's conditional sd sqrt(v_t (cv_b - cv_t) / R) replaced by sqrt(v_t): a valid model,
  # but not the reference walk, so the constant moves with the walk scales between points.
  stan <- readLines(root("Stan", "forecast_v2.stan"))
  bridge <- "sqrt(v[t] * (cv[a] - cv[t]) / R)"
  hit <- grepl(bridge, stan, fixed = TRUE)
  expect_identical(sum(hit), 1L)
  mutant <- file.path(withr::local_tempdir("v2-equivalence-mutant-"), "forecast_v2.stan")
  writeLines(sub(bridge, "sqrt(v[t])", stan, fixed = TRUE), mutant)
  out <- withr::local_tempfile(fileext = ".csv")
  res <- run_equivalence(c(
    paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2021-09-25,2021-03-29", "--n=10", "--n-off=5",
    paste0("--out=", out), paste0("--stan=", mutant)
  ))
  expect_identical(attr(res, "status"), 1L, label = shown(res))
  expect_match(res, "^EQUIV FAIL", all = FALSE)
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(x$verdict, "FAIL")
  # WHY: the FAIL must come from the density check alone: the path, the generated quantities and the points are as asked.
  expect_match(x$reasons, "^constant range [^;]* is not below tol [^;]*$")
  expect_lt(as.numeric(x$path_err), 1e-8)
  expect_identical(unlist(x[1, c("n_posterior", "n_off", "n_rejected", "n_outside", "n_one_sided")], use.names = FALSE), c("10", "5", "0", "0", "0"))
})

test_that("v2s-p1: the anchored walk is the reference walk on 2024 eve under the reversed orders", {
  skip_unless_slow()
  skip_unless_step("v2s-p1")
  skip_without_snapshot()
  out <- withr::local_tempfile(fileext = ".csv")
  res <- run_equivalence(c(
    paste0("--config=", P1_CONFIG), "--form=walk", "--cell=2024-11-30,2024-11-29", "--n=10", "--n-off=5", "--reverse",
    paste0("--out=", out)
  ))
  expect_null(attr(res, "status"), label = shown(res))
  expect_match(res, "^EQUIV PASS", all = FALSE)
  # WHY: under the reversed orders the last poll party (Annad) is the coordinate party, not the first, so every present and absent set changes.
  expect_match(res, "order reversed", all = FALSE)
  expect_true(paste("coordinate party:", POLL_PARTIES[length(POLL_PARTIES)]) %in% res, label = shown(res))
  expect_false(paste("coordinate party:", POLL_PARTIES[1]) %in% res)
  x <- utils::read.csv(out, colClasses = "character")
  expect_identical(unlist(x[1, c("E", "F", "order", "verdict")], use.names = FALSE), c("2024-11-30", "2024-11-29", "reversed", "PASS"))
})
