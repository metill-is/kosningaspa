box::use(R / harness / freeze[read_sha_list, verify_sha_list, is_committed, freeze_guard, target_cell])

E24 <- "2024-11-30"
F24 <- "2024-11-29"
E17 <- "2017-10-28"
F17 <- "2017-10-27"
NO_FREEZE <- "no freeze: harness/FREEZE.md and harness/FREEZE.sha256 must exist before a target cell is scored"
NOT_COMMITTED <- "the freeze is not committed"
LOCKED_2017 <- "2017 is locked until harness/DECISION.md is committed with its sha256"
CONFIRMED_2017 <- "2017 has been scored (harness/CONFIRM.md): no second look"

test_that("read_sha_list reads shasum lines, skips comments and blank lines, and refuses any other line", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "list.sha256")
  a <- strrep("a", 64)
  b <- paste0(strrep("0", 63), "f")
  writeLines(c("# frozen files", "", paste0(a, "  harness/FREEZE.md"), "   ", paste0(b, "  harness/a b.csv")), path)
  expected <- tibble::tibble(sha256 = c(a, b), path = c("harness/FREEZE.md", "harness/a b.csv"))
  expect_identical(read_sha_list(path), expected)
  bad <- c(
    paste0(toupper(a), "  x"), paste0(a, " x"), paste0(a, " *x"), paste0(substr(a, 1, 63), "  x"),
    paste0(a, "  "), paste0(a, "0  x"), "  # indented", "x",
    paste0(a, "  ../x"), paste0(a, "  harness/../../x"), paste0(a, "  harness/.."), paste0(a, "  /etc/hosts")
  )
  for (line in bad) {
    writeLines(c("# frozen", line), path)
    expect_error(read_sha_list(path), paste0("malformed sha256 line 2 in ", path), fixed = TRUE, label = line)
  }
  writeLines("# nothing", path)
  expect_identical(read_sha_list(path), tibble::tibble(sha256 = character(0), path = character(0)))
  writeLines(paste0(a, "  harness/a..b/..c.csv"), path)
  expect_identical(read_sha_list(path)$path, "harness/a..b/..c.csv")
})

test_that("read_sha_list reads the lines shasum -a 256 writes", {
  skip_if(!nzchar(Sys.which("shasum")), "no shasum")
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "harness"))
  writeLines("frozen", file.path(dir, "harness", "FREEZE.md"))
  out <- withr::with_dir(dir, system2("shasum", c("-a", "256", "harness/FREEZE.md"), stdout = TRUE))
  writeLines(out, file.path(dir, "FREEZE.sha256"))
  expect_identical(read_sha_list(file.path(dir, "FREEZE.sha256"))$path, "harness/FREEZE.md")
  expect_identical(out, sha256_line(dir, "harness/FREEZE.md"))
  expect_true(verify_sha_list(file.path(dir, "FREEZE.sha256"), dir))
})

test_that("verify_sha_list checks every listed file, relative to root, and stops at the first failure", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "harness"))
  writeLines("one", file.path(dir, "harness", "a.txt"))
  list <- file.path(dir, "list.sha256")
  writeLines(sha256_line(dir, "harness/a.txt"), list)
  expect_invisible(verify_sha_list(list, dir))
  expect_true(verify_sha_list(list, dir))
  writeLines(c(sha256_line(dir, "harness/a.txt"), sub("a.txt", "b.txt", sha256_line(dir, "harness/a.txt"))), list)
  expect_error(verify_sha_list(list, dir), "missing: harness/b.txt", fixed = TRUE)
  dir.create(file.path(dir, "harness", "b.txt"))
  expect_error(verify_sha_list(list, dir), "missing: harness/b.txt", fixed = TRUE)
  unlink(file.path(dir, "harness", "b.txt"), recursive = TRUE)
  writeLines("two", file.path(dir, "harness", "b.txt"))
  expect_error(verify_sha_list(list, dir), "sha256 mismatch: harness/b.txt", fixed = TRUE)
  writeLines("# nothing listed", list)
  expect_error(verify_sha_list(list, dir), paste0("empty sha256 list: ", list), fixed = TRUE)
})

test_that("is_committed needs every path tracked and unchanged against HEAD", {
  fr <- frozen_root()
  files <- c("harness/FREEZE.md", "harness/FREEZE.sha256")
  expect_true(is_committed(files, fr))
  writeLines("new", file.path(fr, "harness", "new.md"))
  expect_false(is_committed(c(files, "harness/new.md"), fr))
  git_in(fr, "add", "-A")
  expect_false(is_committed("harness/new.md", fr))
  commit_all(fr, "new")
  expect_true(is_committed(c(files, "harness/new.md"), fr))
  cat("changed\n", file = file.path(fr, "harness", "new.md"), append = TRUE)
  expect_false(is_committed("harness/new.md", fr))
  expect_true(is_committed(files, fr))
  file.remove(file.path(fr, "harness", "FREEZE.md"))
  expect_false(is_committed(files, fr))
  expect_false(is_committed(files, seal(withr::local_tempdir(), "FREEZE")))
})

test_that("is_committed sees an edit that the index is told to ignore", {
  for (flag in c("--assume-unchanged", "--skip-worktree")) {
    fr <- frozen_root()
    git_in(fr, "update-index", flag, "harness/FREEZE.md")
    cat("edited\n", file = file.path(fr, "harness", "FREEZE.md"), append = TRUE)
    expect_false(is_committed("harness/FREEZE.md", fr), label = flag)
    expect_true(is_committed("harness/FREEZE.sha256", fr), label = flag)
  }
})

test_that("target_cell returns the TARGETS row of a forecast cell", {
  expect_identical(target_cell(E24, F24), TARGETS[TARGETS$E == as.Date(E24) & TARGETS$F == as.Date(F24), ])
  expect_true(target_cell(as.Date(E17), as.Date(F17))$confirmatory)
})

test_that("the guard passes every non-confirmatory target cell under a committed freeze", {
  fr <- frozen_root()
  expect_invisible(freeze_guard(E24, F24, fr))
  for (i in which(!TARGETS$confirmatory)) {
    expect_true(freeze_guard(TARGETS$E[i], TARGETS$F[i], fr), label = paste(TARGETS$E[i], TARGETS$F[i]))
  }
})

test_that("1: a cell outside TARGETS is refused before any freeze file is looked at", {
  empty <- withr::local_tempdir()
  expect_error(freeze_guard(E24, "2024-09-01", empty), "not a forecast cell: E = 2024-11-30, F = 2024-09-01",
    fixed = TRUE
  )
  fr <- frozen_root()
  expect_error(freeze_guard("2016-10-29", "2016-10-28", fr), "not a forecast cell: E = 2016-10-29, F = 2016-10-28",
    fixed = TRUE
  )
  expect_error(freeze_guard(F24, E24, fr), "not a forecast cell")
  expect_error(freeze_guard(c(E24, E24), c(F24, F24), fr), "not a forecast cell")
  expect_error(freeze_guard(NA, F24, fr), "not a forecast cell")
  expect_error(freeze_guard("not a date", F24, fr), "not a forecast cell")
})

test_that("2: without FREEZE.md or FREEZE.sha256 there is no freeze", {
  expect_error(freeze_guard(E24, F24, withr::local_tempdir()), NO_FREEZE, fixed = TRUE)
  for (f in c("FREEZE.md", "FREEZE.sha256")) {
    fr <- frozen_root()
    file.remove(file.path(fr, "harness", f))
    expect_error(freeze_guard(E24, F24, fr), NO_FREEZE, fixed = TRUE, label = f)
  }
  fr <- frozen_root()
  file.remove(file.path(fr, "harness", "FREEZE.md"))
  commit_all(seal(fr, "DECISION"), "a decision without a freeze")
  expect_error(freeze_guard(E17, F17, fr), NO_FREEZE, fixed = TRUE)
})

test_that("3: a sha256 list that does not name FREEZE.md does not cover it", {
  fr <- frozen_root()
  writeLines("other", file.path(fr, "harness", "other.txt"))
  writeLines(sha256_line(fr, "harness/other.txt"), file.path(fr, "harness", "FREEZE.sha256"))
  commit_all(fr, "list another file")
  expect_error(freeze_guard(E24, F24, fr), "harness/FREEZE.sha256 does not cover harness/FREEZE.md", fixed = TRUE)
  writeLines("# nothing listed", file.path(fr, "harness", "FREEZE.sha256"))
  commit_all(fr, "list nothing")
  expect_error(freeze_guard(E24, F24, fr), "harness/FREEZE.sha256 does not cover harness/FREEZE.md", fixed = TRUE)
})

test_that("4: an edited FREEZE.md, or a listed file gone, fails the sha256 check", {
  fr <- frozen_root()
  cat("edited\n", file = file.path(fr, "harness", "FREEZE.md"), append = TRUE)
  expect_error(freeze_guard(E24, F24, fr), "sha256 mismatch: harness/FREEZE.md", fixed = TRUE)
  fr <- frozen_root()
  write(paste0(strrep("0", 64), "  harness/gone.csv"), file.path(fr, "harness", "FREEZE.sha256"), append = TRUE)
  commit_all(fr, "list a file that does not exist")
  expect_error(freeze_guard(E24, F24, fr), "missing: harness/gone.csv", fixed = TRUE)
  fr <- frozen_root()
  write("not a sha256 line", file.path(fr, "harness", "FREEZE.sha256"), append = TRUE)
  commit_all(fr, "a malformed line")
  expect_error(freeze_guard(E24, F24, fr), "malformed sha256 line 2", fixed = TRUE)
})

test_that("5: a freeze never committed, only staged or changed since is not committed", {
  fr <- withr::local_tempdir()
  git_in(fr, "init", "-q")
  seal(fr, "FREEZE")
  expect_error(freeze_guard(E24, F24, fr), NOT_COMMITTED, fixed = TRUE)
  fr <- withr::local_tempdir()
  git_in(fr, "init", "-q")
  writeLines("readme", file.path(fr, "README"))
  commit_all(fr, "first")
  seal(fr, "FREEZE")
  expect_error(freeze_guard(E24, F24, fr), NOT_COMMITTED, fixed = TRUE)
  git_in(fr, "add", "-A")
  expect_error(freeze_guard(E24, F24, fr), NOT_COMMITTED, fixed = TRUE)
  fr <- frozen_root()
  write("# a comment keeps the list valid", file.path(fr, "harness", "FREEZE.sha256"), append = TRUE)
  expect_error(freeze_guard(E24, F24, fr), NOT_COMMITTED, fixed = TRUE)
  expect_error(freeze_guard(E24, F24, seal(withr::local_tempdir(), "FREEZE")), NOT_COMMITTED, fixed = TRUE)
})

test_that("6: 2017 is locked until DECISION.md and its sha256 list pass the freeze's checks, the failed check named", {
  locked <- function(why) paste0(LOCKED_2017, " (", why, ")")
  no_decision <- "no harness/DECISION.md and harness/DECISION.sha256"
  fr <- frozen_root()
  for (F in c("2017-10-14", "2017-10-27")) {
    expect_error(freeze_guard(E17, F, fr), locked(no_decision), fixed = TRUE, label = F)
  }
  seal(fr, "DECISION")
  expect_error(freeze_guard(E17, F17, fr), locked("the decision is not committed"), fixed = TRUE)
  commit_all(fr, "decision")
  for (F in c("2017-10-14", "2017-10-27")) expect_true(freeze_guard(E17, F, fr), label = F)
  cat("edited\n", file = file.path(fr, "harness", "DECISION.md"), append = TRUE)
  expect_error(freeze_guard(E17, F17, fr), locked("sha256 mismatch: harness/DECISION.md"), fixed = TRUE)
  expect_true(freeze_guard(E24, F24, fr))
  fr <- frozen_root(decision = TRUE)
  file.remove(file.path(fr, "harness", "DECISION.sha256"))
  expect_error(freeze_guard(E17, F17, fr), locked(no_decision), fixed = TRUE)
  fr <- frozen_root(decision = TRUE)
  writeLines("# nothing listed", file.path(fr, "harness", "DECISION.sha256"))
  commit_all(fr, "a decision list that names nothing")
  expect_error(freeze_guard(E17, F17, fr), locked("harness/DECISION.sha256 does not cover harness/DECISION.md"), fixed = TRUE)
})

test_that("7: once CONFIRM.md exists, 2017 is never scored again", {
  fr <- frozen_root(decision = TRUE)
  expect_true(freeze_guard(E17, F17, fr))
  writeLines("scores", file.path(fr, "harness", "CONFIRM.md"))
  for (F in c("2017-10-14", "2017-10-27")) {
    expect_error(freeze_guard(E17, F, fr), CONFIRMED_2017, fixed = TRUE, label = F)
  }
  commit_all(fr, "confirm")
  expect_error(freeze_guard(E17, F17, fr), CONFIRMED_2017, fixed = TRUE)
  expect_true(freeze_guard(E24, F24, fr))
})
