box::use(R / harness / runs[run_id])

# tier0_report.R run from the project root; its output, with the exit status as attribute
# "status" when it is not 0.
report <- function(args) {
  withr::with_dir(root(), suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("R/harness/tier0_report.R", args),
    stdout = TRUE, stderr = TRUE, env = "LC_ALL=en_US.UTF-8"
  )))
}

PASSING <- list(
  max_rhat = 1.0087, max_rhat_scored = 1.00731, min_ess_bulk = 321.9, min_ess_bulk_scored = 456.7,
  min_ess_tail = 250.5, min_ess_tail_scored = 250.5, na_scored = 0L, divergent = 0L,
  treedepth_hits = 40L, max_depth = 11L, min_ebfmi = 0.6789
)

# A finished run's meta.json as fit_cell.R writes it (non-finite numbers as strings), passed
# through `edit` first, with its modification time set to `written`.
fake_run <- function(results, E, F, seed, iters = c(200L, 200L), minutes = 12.5, diagnostics = PASSING,
                     gate = list(pass = TRUE, reasons = character(0)), written = "2026-10-24 12:34:56",
                     edit = identity) {
  dir <- file.path(results, run_id("r0", E, F, seed))
  dir.create(dir, recursive = TRUE)
  meta <- list(
    arm = "r0", E = E, F = F, id = basename(dir), seed = seed, iter_warmup = iters[1],
    iter_sampling = iters[2], chains = 4L, max_treedepth = 12L,
    # WHY: a forecast-like field, which the report must not print.
    decoy_median_share = 0.3141,
    minutes = minutes, diagnostics = diagnostics, gate = list(pass = gate$pass, reasons = I(gate$reasons)),
    cmdstan_version = "2.36.0", cmdstanr_version = "0.8.1"
  )
  path <- file.path(dir, "meta.json")
  jsonlite::write_json(edit(meta), path, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "string")
  Sys.setFileTime(path, as.POSIXct(written, tz = "UTC"))
  dir
}

cells <- function(row) trimws(strsplit(sub("^\\|(.*)\\|$", "\\1", row), "|", fixed = TRUE)[[1]])

test_that("the Tier 0 report writes one row of diagnostics per run, non-finite values as recorded", {
  results <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  fake_run(results, "2024-11-30", "2024-11-29", 1101L)
  fake_run(results, "2021-09-25", "2021-03-29", 1102L, c(10L, 10L), 0.04,
    diagnostics = list(
      max_rhat = Inf, max_rhat_scored = Inf, min_ess_bulk = NA_real_, min_ess_bulk_scored = NA_real_,
      min_ess_tail = NA_real_, min_ess_tail_scored = NaN, na_scored = 2L, divergent = 40L,
      treedepth_hits = 0L, max_depth = 0L, min_ebfmi = NaN
    ),
    gate = list(pass = FALSE, reasons = c(
      "max R-hat Inf > 1.01", "divergent 40 > 0", "min scored bulk ESS NA < 400",
      "scored quantities with NA R-hat or ESS: 2"
    )),
    written = "2026-10-24 13:05:00"
  )
  runs <- file.path(out_dir, "runs.txt")
  writeLines(c(
    "# lane arm E F seed",
    "A r0 2024-11-30 2024-11-29 1101",
    "A v2 2024-11-30 2024-11-29 1101",
    "B r0 2021-09-25 2021-03-29 1102"
  ), runs)
  out <- file.path(out_dir, "r0.md")
  before <- list.files(root("results"), recursive = TRUE, all.files = TRUE, include.dirs = TRUE)

  shown <- report(c("r0", runs, out))
  expect_null(attr(shown, "status"), info = paste(shown, collapse = "\n"))
  text <- readLines(out, encoding = "UTF-8")
  rows <- grep("^\\| 20", text, value = TRUE)
  expect_length(rows, 2)
  expect_identical(cells(rows[1]), c(
    "2024-11-30, 2024-11-29, eve", "1101", "200 + 200", "12.5", "0", "40 (5.0%)", "11", "0.678",
    "1.0087; 1.0074", "321; 456", "250; 250", "25.7; 36.5", "3219; 4567", "PASS", "2026-10-24 12:34 UTC"
  ))
  expect_identical(cells(rows[2]), c(
    "2021-09-25, 2021-03-29, f180", "1102", "10 + 10", "0.0", "40", "0 (0.0%)", "0", "NaN",
    "Inf; Inf", "NA; NA", "NA; NaN", "NA; NA", "NA; NA",
    "FAIL: max R-hat Inf > 1.01; divergent 40 > 0; min scored bulk ESS NA < 400; scored quantities with NA R-hat or ESS: 2",
    "2026-10-24 13:05 UTC"
  ))
  header <- cells(text[which(text == rows[1]) - 2L])
  expect_length(header, 15)
  expect_true(all(grepl("(gate scope: parameters block + scored; scored)", header[9:13], fixed = TRUE)))
  expect_true(any(grepl("CmdStan 2.36.0", text, fixed = TRUE)))
  ncpu <- system2("sysctl", c("-n", "hw.ncpu"), stdout = TRUE)
  expect_true(any(grepl(paste0("hw.ncpu: ", ncpu), text, fixed = TRUE)))
  expect_true(any(grepl("linear projection", text, fixed = TRUE)))
  # WHY: the temporary run list's path is not the report's own text, and its random letters
  # could spell one of the words below.
  own <- gsub(out_dir, "", text, fixed = TRUE)
  # WHY: no column heading holds one of these words, so the whole report is checked.
  expect_false(any(grepl("share|mean|median|crps|mae", own, ignore.case = TRUE)))
  # WHY: the decoy's value, 0.3141, in any rounding or as a percentage.
  expect_false(any(grepl("314", own, fixed = TRUE)))
  expect_identical(list.files(root("results"), recursive = TRUE, all.files = TRUE, include.dirs = TRUE), before)
})

test_that("the Tier 0 report stops, writing nothing, on a run it cannot read", {
  results <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  withr::local_envvar(HARNESS_RESULTS = results)
  runs <- file.path(out_dir, "runs.txt")
  out <- file.path(out_dir, "r0.md")
  # WHY: a readable run comes first, so a stop on the second shows that nothing is written in part.
  fake_run(results, "2021-09-25", "2021-03-29", 1102L)
  writeLines(c("A r0 2021-09-25 2021-03-29 1102", "A r0 2024-11-30 2024-11-29 1101"), runs)
  fails_with <- function(message) {
    unlink(out)
    shown <- report(c("r0", runs, out))
    expect_identical(attr(shown, "status"), 1L)
    expect_match(shown, message, fixed = TRUE, all = FALSE)
    expect_false(file.exists(out))
  }
  second <- file.path(results, run_id("r0", "2024-11-30", "2024-11-29", 1101))

  fails_with("no meta.json")
  fake_run(results, "2024-11-30", "2024-11-29", 1101L, edit = \(m) utils::modifyList(m, list(seed = 1199L)))
  fails_with("is not of the run list's run r0_2024-11-30_2024-11-29_s1101")
  unlink(second, recursive = TRUE)
  fake_run(results, "2024-11-30", "2024-11-29", 1101L, edit = \(m) m[names(m) != "minutes"])
  fails_with("meta.json: minutes is not a number")
  unlink(second, recursive = TRUE)
  fake_run(results, "2024-11-30", "2024-11-29", 1101L, edit = \(m) {
    m$diagnostics$max_rhat <- "high"
    m
  })
  fails_with("meta.json: diagnostics$max_rhat is not a number")

  writeLines("A v2 2024-11-30 2024-11-29 1101", runs)
  fails_with("no run of arm r0")
})
