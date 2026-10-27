box::use(
  R / harness / constants[project_root, TARGETS, POLL_PARTIES, ELECTIONS, WINDOW_START],
  R / harness / inputs[snapshot_file],
  R / election_utils[seats_layout_2021]
)

root <- function(...) file.path(project_root(), ...)

# WHY: R0 is production's model at this commit plus the hooks: centred delta and log sigma (Decision 5), replications gathered by party identity.
R0_PARENT_COMMIT <- "884560f9390721bca43acccbdf8f3d41b7382562"

snapshot_present <- function() {
  file.exists(root("harness", "snapshot_manifest.csv")) && dir.exists(root("harness", "snapshot"))
}

skip_without_snapshot <- function() {
  if (!snapshot_present()) testthat::skip("no harness snapshot: run snapshot_inputs()")
}

skip_unless_slow <- function() {
  if (!identical(Sys.getenv("HARNESS_SLOW"), "1")) testthat::skip("slow test: set HARNESS_SLOW=1")
}

# One draw of kjördæmi votes: 10000 votes in each kjördæmi of the 2021 layout, split by `shares`.
draw_with <- function(shares) {
  tidyr::expand_grid(kjordaemi = seats_layout_2021()$kjordaemi, flokkur = names(shares)) |>
    dplyr::mutate(votes = 10000 * shares[flokkur])
}

# WHY: GIT_DIR and its kin override -C, so a test's git command could otherwise reach this checkout.
git_in <- function(dir, ...) {
  withr::local_envvar(c(GIT_DIR = NA, GIT_WORK_TREE = NA, GIT_INDEX_FILE = NA))
  status <- system2("git", c("-C", shQuote(dir), ...), stdout = FALSE, stderr = FALSE)
  if (status != 0) stop("git ", paste(c(...), collapse = " "), " failed in ", dir)
  invisible(dir)
}

commit_all <- function(dir, message) {
  git_in(dir, "add", "-A")
  git_in(dir, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", shQuote(message))
}

sha256_line <- function(dir, path) {
  paste0(digest::digest(file = file.path(dir, path), algo = "sha256"), "  ", path)
}

seal <- function(dir, name, text = paste("#", name)) {
  md <- file.path("harness", paste0(name, ".md"))
  dir.create(file.path(dir, "harness"), showWarnings = FALSE)
  writeLines(text, file.path(dir, md))
  writeLines(sha256_line(dir, md), file.path(dir, "harness", paste0(name, ".sha256")))
  invisible(dir)
}

frozen_root <- function(decision = FALSE, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  git_in(dir, "init", "-q")
  commit_all(seal(dir, "FREEZE"), "freeze")
  if (decision) commit_all(seal(dir, "DECISION"), "decision")
  dir
}
