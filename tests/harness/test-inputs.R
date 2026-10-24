box::use(
  R / harness / inputs[snapshot_inputs, verify_snapshot, SNAPSHOT_FILES],
  R / harness / constants[TARGETS, previous_election]
)

fake_src <- function() {
  src <- withr::local_tempdir(.local_envir = parent.frame())
  for (p in SNAPSHOT_FILES) {
    dir.create(dirname(file.path(src, p)), recursive = TRUE, showWarnings = FALSE)
    writeLines(p, file.path(src, p))
  }
  src
}

test_that("snapshot_inputs copies and hashes every file", {
  src <- fake_src()
  out <- withr::local_tempdir()
  m <- snapshot_inputs(src, file.path(out, "snap"), file.path(out, "manifest.csv"))
  expect_equal(nrow(m), length(SNAPSHOT_FILES))
  expect_equal(m$sha256[1], digest::digest(file = file.path(src, SNAPSHOT_FILES[[1]]), algo = "sha256"))
  expect_true(verify_snapshot(file.path(out, "snap"), file.path(out, "manifest.csv")))
})

test_that("verify_snapshot fails on a modified file", {
  src <- fake_src()
  out <- withr::local_tempdir()
  snapshot_inputs(src, file.path(out, "snap"), file.path(out, "manifest.csv"))
  cat("x", file = file.path(out, "snap", "polling_data.csv"), append = TRUE)
  expect_error(
    verify_snapshot(file.path(out, "snap"), file.path(out, "manifest.csv")),
    "sha256 mismatch: polling_data.csv"
  )
})

test_that("snapshot_inputs refuses an existing destination", {
  src <- fake_src()
  out <- withr::local_tempdir()
  dir.create(file.path(out, "snap"))
  expect_error(snapshot_inputs(src, file.path(out, "snap"), file.path(out, "m.csv")), "exists")
})

test_that("cells and previous elections are the design's", {
  expect_equal(nrow(TARGETS), 10)
  expect_equal(
    as.character(TARGETS$F[TARGETS$E == as.Date("2024-11-30")]),
    c("2024-06-03", "2024-10-14", "2024-11-16", "2024-11-29")
  )
  expect_equal(as.character(TARGETS$F[TARGETS$confirmatory]), c("2017-10-14", "2017-10-27"))
  expect_equal(previous_election(as.Date("2016-10-29")), as.Date("2013-04-27"))
  expect_equal(previous_election(as.Date("2024-11-30")), as.Date("2021-09-25"))
  expect_error(previous_election(as.Date("2020-01-01")))
})
