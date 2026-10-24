# The freeze guard (spec §5.7): no target cell is scored before harness/FREEZE.md is
# committed with its sha256 list, no 2017 cell before harness/DECISION.md is too, and no
# 2017 cell once harness/CONFIRM.md records its one scoring.

SHA_LINE <- "^([0-9a-f]{64})  (.+)$"

#' Read a sha256 list as `shasum -a 256` writes it.
#' @param path the list
#' @return a tibble (sha256, path), one row per `<64 lowercase hex><two spaces><path>`
#'   line, in file order. Lines that start with `#` and blank lines are skipped; any other
#'   line is an error, "malformed sha256 line <n> in <path>"
#' @export
read_sha_list <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  skip <- startsWith(lines, "#") | grepl("^[[:space:]]*$", lines)
  m <- regmatches(lines, regexec(SHA_LINE, lines))
  bad <- which(!skip & lengths(m) == 0)
  if (length(bad)) stop("malformed sha256 line ", bad[1], " in ", path)
  tibble::tibble(
    sha256 = vapply(m[!skip], `[`, "", 2),
    path = vapply(m[!skip], `[`, "", 3)
  )
}

#' Check every file of a sha256 list against its sha256.
#' @param path the list (`read_sha_list()`)
#' @param root the directory the listed paths are relative to
#' @return `invisible(TRUE)`; otherwise an error at the first failure: "empty sha256 list:
#'   <path>", "missing: <listed path>" (no such regular file) or "sha256 mismatch: <listed
#'   path>"
#' @export
verify_sha_list <- function(path, root) {
  listed <- read_sha_list(path)
  if (!nrow(listed)) stop("empty sha256 list: ", path)
  for (i in seq_len(nrow(listed))) {
    file <- file.path(root, listed$path[i])
    if (!utils::file_test("-f", file)) stop("missing: ", listed$path[i])
    if (!identical(digest::digest(file = file, algo = "sha256"), listed$sha256[i])) {
      stop("sha256 mismatch: ", listed$path[i])
    }
  }
  invisible(TRUE)
}

#' Whether files are committed: tracked by git and unchanged against HEAD.
#' @param paths paths relative to `root`
#' @param root a directory in a git working tree
#' @return logical(1): TRUE when `git -C root ls-files --error-unmatch` and
#'   `git -C root diff --quiet HEAD --` both succeed for `paths`; FALSE outside a
#'   repository or before its first commit
#' @export
is_committed <- function(paths, root) {
  stopifnot(is.character(paths), length(paths) > 0, !anyNA(paths))
  # WHY: GIT_DIR and its kin override -C, and `root` alone must name the repository.
  withr::local_envvar(c(GIT_DIR = NA, GIT_WORK_TREE = NA, GIT_INDEX_FILE = NA))
  git <- function(...) {
    system2("git", c("-C", shQuote(root), ..., "--", shQuote(paths)), stdout = FALSE, stderr = FALSE)
  }
  git("ls-files", "--error-unmatch") == 0 && git("diff", "--quiet", "HEAD") == 0
}

#' The `TARGETS` row of a forecast cell.
#' @param E,F the election and forecast date (Date or ISO string), one each
#' @return a one-row tibble; an error, "not a forecast cell: E = …, F = …", unless (E, F)
#'   is a `TARGETS` row
#' @export
target_cell <- function(E, F) {
  box::use(R / harness / constants[TARGETS])
  as_date <- function(x) tryCatch(as.Date(x), error = function(e) as.Date(NA))
  e <- as_date(E)
  f <- as_date(F)
  hit <- if (length(e) == 1 && length(f) == 1) which(TARGETS$E == e & TARGETS$F == f) else integer(0)
  if (length(hit) != 1) {
    stop("not a forecast cell: E = ", paste(format(E), collapse = ", "), ", F = ", paste(format(F), collapse = ", "))
  }
  TARGETS[hit, ]
}

#' Checks 2–5 of `freeze_guard()` for `harness/<name>.md` and `harness/<name>.sha256`.
check_sealed <- function(name, root, none, not_committed) {
  md <- paste0("harness/", name, ".md")
  list <- paste0("harness/", name, ".sha256")
  if (!all(file.exists(file.path(root, c(md, list))))) stop(none)
  if (!md %in% read_sha_list(file.path(root, list))$path) stop(list, " does not cover ", md)
  verify_sha_list(file.path(root, list), root)
  if (!is_committed(c(md, list), root)) stop(not_committed)
  invisible(TRUE)
}

#' The freeze guard: whether forecast cell (E, F) may be scored under `root`.
#' @param E,F the cell's election and forecast date (Date or ISO string)
#' @param root the project root whose `harness/` holds the freeze files
#' @return `invisible(TRUE)`; otherwise an error at the first failure, in this order:
#'   1. "not a forecast cell: E = …, F = …" if (E, F) is not a `TARGETS` row;
#'   2. "no freeze: harness/FREEZE.md and harness/FREEZE.sha256 must exist before a
#'   target cell is scored";
#'   3. "harness/FREEZE.sha256 does not cover harness/FREEZE.md";
#'   4. `verify_sha_list()`'s error for `harness/FREEZE.sha256`;
#'   5. "the freeze is not committed" unless `is_committed()` holds for both files;
#'   6. for a confirmatory cell, "2017 is locked until harness/DECISION.md is committed
#'   with its sha256" unless `harness/DECISION.md` and `harness/DECISION.sha256` pass
#'   checks 2–5;
#'   7. for a confirmatory cell, "2017 has been scored (harness/CONFIRM.md): no second
#'   look" if `harness/CONFIRM.md` exists
#' @export
freeze_guard <- function(E, F, root = project_root()) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(R / harness / constants[project_root])
  cell <- target_cell(E, F)
  check_sealed(
    "FREEZE", root,
    none = "no freeze: harness/FREEZE.md and harness/FREEZE.sha256 must exist before a target cell is scored",
    not_committed = "the freeze is not committed"
  )
  if (cell$confirmatory) {
    decided <- tryCatch(check_sealed("DECISION", root, "", ""), error = function(e) FALSE)
    if (!isTRUE(decided)) stop("2017 is locked until harness/DECISION.md is committed with its sha256")
    if (file.exists(file.path(root, "harness", "CONFIRM.md"))) {
      stop("2017 has been scored (harness/CONFIRM.md): no second look")
    }
  }
  invisible(TRUE)
}
