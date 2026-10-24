# The generated files committed under harness/ that build offline, against a fresh build:
# each writer writes to a temp file, which must hold the committed file's bytes.
# R/harness/build_phase0.R rewrites the committed files.
box::use(
  R / harness / availability[write_availability],
  R / harness / tables[write_entry_table],
  R / harness / truth[write_truth]
)
committed <- function(...) root("harness", ...)

expect_same_file <- function(fresh, path) {
  what <- sub(paste0(root(), "/"), "", path, fixed = TRUE)
  expect_true(file.exists(fresh), label = paste("a fresh build of", what))
  if (!file.exists(fresh)) {
    return(invisible())
  }
  expect_identical(
    readLines(fresh, encoding = "UTF-8"), readLines(path, encoding = "UTF-8"),
    label = paste("a fresh build of", what), expected.label = paste("the committed", what)
  )
  expect_identical(
    unname(tools::md5sum(fresh)), unname(tools::md5sum(path)),
    label = paste("the md5 of a fresh build of", what), expected.label = paste("the committed", what)
  )
}

test_that("the committed truth table is a fresh build", {
  skip_without_snapshot()
  fresh <- withr::local_tempfile(fileext = ".csv")
  write_truth(fresh)
  expect_same_file(fresh, committed("truth", "elections_official.csv"))
})

test_that("the committed availability table and report are a fresh build", {
  skip_without_snapshot()
  csv <- withr::local_tempfile(fileext = ".csv")
  report <- withr::local_tempfile(fileext = ".md")
  write_availability(csv, report)
  expect_same_file(csv, committed("availability.csv"))
  expect_same_file(report, committed("availability_report.md"))
})

test_that("the committed entry table is a fresh build", {
  skip_without_snapshot()
  fresh <- withr::local_tempfile(fileext = ".csv")
  write_entry_table(fresh)
  expect_same_file(fresh, committed("entry_table.csv"))
})

test_that("the committed 2021 seats are a fresh count of the committed Althingi page", {
  fresh <- withr::local_tempfile(fileext = ".csv")
  log <- withr::local_tempfile(fileext = ".log")
  script <- file.path("harness", "sources", "althingi", "seats_2021_official.R")
  # WHY: the script resolves its box imports from the working directory.
  status <- withr::with_dir(root(), system2(
    file.path(R.home("bin"), "Rscript"), shQuote(c(script, fresh)),
    stdout = log, stderr = log
  ))
  expect_equal(status, 0, info = paste(readLines(log), collapse = "\n"))
  expect_same_file(fresh, committed("sources", "seats_2021_official.csv"))
})
