# Runs the harness tests from the project root: LC_ALL=en_US.UTF-8 Rscript tests/run.R [filter]
stopifnot("run under LC_ALL=en_US.UTF-8" = isTRUE(l10n_info()[["UTF-8"]]))
options(box.path = normalizePath("."))
# WHY: test_dir() takes the edition from TESTTHAT_EDITION or a DESCRIPTION, and the repo has
# no DESCRIPTION, so without this it runs under edition 2.
Sys.setenv(TESTTHAT_EDITION = "3")
# WHY: attaching testthat after a profile-loaded devtools prints a masking notice.
suppressPackageStartupMessages(library(testthat))
args <- commandArgs(trailingOnly = TRUE)
results <- testthat::test_dir("tests/harness",
  filter = if (length(args)) args[1] else NULL,
  stop_on_failure = TRUE
)
# WHY: test_dir() stops on a failure before it returns, so the table follows a run that passed
# and the exit status is test_dir()'s.
per_test <- as.data.frame(results)
if (nrow(per_test)) {
  per_file <- stats::aggregate(real ~ file, data = per_test, FUN = sum)
  per_file <- per_file[order(-per_file$real), ]
  cat("\nReal seconds per test file, slowest first:\n")
  cat(sprintf("%9.1f  %s", per_file$real, per_file$file), sep = "\n")
}
