box::use(
  R / harness / constants[project_root, TARGETS, POLL_PARTIES, ELECTIONS, WINDOW_START],
  R / harness / inputs[snapshot_file],
  R / election_utils[seats_layout_2021]
)

root <- function(...) file.path(project_root(), ...)

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
