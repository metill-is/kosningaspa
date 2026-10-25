# The scorer self-test's inputs (spec §5.6): two draw files of a run that an earlier study
# scored, copied once into harness/selftest/ (gitignored) and sha256-checked on every read
# against harness/selftest_manifest.csv (committed), through the snapshot machinery.
# tests/harness/test-selftest.R re-scores them and must reproduce that study's scores.

#' Self-test source files, relative to the earlier study's folder (spec §5.6 names it).
#' Each is stored in `harness/selftest/` under its basename.
#' @export
SELFTEST_FILES <- c(
  eday_national = "runs/base_eve/eday_national.parquet",
  constituency_predictions = "runs/base_eve/constituency_predictions.parquet"
)

#' Copy the `SELFTEST_FILES` into `harness/selftest/` and write
#' `harness/selftest_manifest.csv`.
#' @param src_root the earlier study's folder
#' @return the manifest, as `snapshot_inputs()` returns it
#' @export
selftest_inputs <- function(src_root) {
  box::use(R / harness / constants[harness_path], R / harness / inputs[snapshot_inputs])
  snapshot_inputs(src_root, harness_path("selftest"), harness_path("selftest_manifest.csv"), files = SELFTEST_FILES)
}

#' Absolute path of one self-test file, after checking its sha256.
#' @param name a `SELFTEST_FILES` name, such as "eday_national"
#' @export
selftest_file <- function(name) {
  box::use(R / harness / constants[harness_path], R / harness / inputs[snapshot_file])
  snapshot_file(name, harness_path("selftest"), harness_path("selftest_manifest.csv"))
}

#' Whether the self-test inputs have been copied.
#' @return logical(1): TRUE when `harness/selftest_manifest.csv` and `harness/selftest/`
#'   both exist; a file missing or changed there is then an error in `selftest_file()`,
#'   not a skip
#' @export
selftest_present <- function() {
  box::use(R / harness / constants[harness_path])
  file.exists(harness_path("selftest_manifest.csv")) && dir.exists(harness_path("selftest"))
}
