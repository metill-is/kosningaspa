# The environment the suite and the harness entry points run in (R52).
test_that("the suite runs under testthat edition 3", {
  expect_identical(testthat::edition_get(), 3L)
  # WHY: edition 2's all.equal-based expect_equal() drops `ignore_attr` silently and fails here.
  expect_equal(c(a = 1), c(b = 1), ignore_attr = TRUE)
})

# Runs `args` with Rscript under LC_ALL=C from a scratch directory; returns its output
# lines with the exit status as attribute "status" (0 when it succeeded).
rscript_c_locale <- function(args) {
  dir <- withr::local_tempdir()
  out <- withr::with_dir(dir, suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), args, stdout = TRUE, stderr = TRUE, env = "LC_ALL=C"
  )))
  if (is.null(attr(out, "status"))) attr(out, "status") <- 0L
  out
}
test_that("read_inputs() stops under a non-UTF-8 locale", {
  skip_without_snapshot()
  code <- sprintf(
    "options(box.path = '%s'); box::use(R/harness/info_set[read_inputs]); invisible(read_inputs())", root()
  )
  out <- rscript_c_locale(c("-e", shQuote(code)))
  expect_false(identical(attr(out, "status"), 0L))
  expect_match(paste(out, collapse = "\n"), "read_inputs() needs a UTF-8 locale", fixed = TRUE)
})
test_that("build_phase0.R stops under a non-UTF-8 locale before anything else", {
  out <- rscript_c_locale(shQuote(root("R", "harness", "build_phase0.R")))
  expect_false(identical(attr(out, "status"), 0L))
  expect_match(paste(out, collapse = "\n"), "build_phase0.R needs a UTF-8 locale", fixed = TRUE)
})
