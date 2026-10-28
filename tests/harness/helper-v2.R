# Test fixtures for arm V2: its step configs, the 2024-eve cell and its Stan data, and
# helpers that each plant one fault in one table of an info_set().
# The V2 modules are imported inside the fixtures, so that a broken module fails the tests
# that use it, not the sourcing of every test file.
box::use(
  R / harness / constants[project_root],
  R / harness / info_set[info_set, read_inputs]
)

V2_M0_CONFIG <- file.path(project_root(), "harness", "configs", "steps", "v2s-m0.json")

# WHY: read_inputs() and v2_stan_data() take seconds, and the 2024-eve fixtures serve many tests.
v2_cache <- new.env(parent = emptyenv())
cached <- function(key, value) {
  if (is.null(v2_cache[[key]])) v2_cache[[key]] <- value
  v2_cache[[key]]
}

cfg_m0 <- function() {
  box::use(R / harness / v2[read_v2_config])
  read_v2_config(V2_M0_CONFIG)
}

# A temporary copy of the m0 config with the named keys changed (NULL drops a key, a new
# name adds one), deleted when the caller's frame exits; returns its path.
config_with <- function(..., env = parent.frame()) {
  cfg <- utils::modifyList(jsonlite::read_json(V2_M0_CONFIG), list(...))
  path <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, pretty = TRUE, digits = I(17))
  path
}

v2_inputs <- function() {
  skip_without_snapshot()
  cached("inputs", read_inputs())
}

info_2024eve <- function() {
  skip_without_snapshot()
  cached("info_2024eve", info_set(as.Date("2024-11-30"), as.Date("2024-11-29"), v2_inputs()))
}

d_2024eve <- function() {
  box::use(R / harness / v2[v2_stan_data])
  skip_without_snapshot()
  cached("d_2024eve", v2_stan_data(info_2024eve(), cfg_m0())$data)
}

# The cell's party order with Sósíalistaflokkurinn first: it has no votes at 2016 or 2017.
j_first <- function(info) {
  j <- "S\u00f3s\u00edalistaflokkurinn"
  c(j, setdiff(info$parties, j))
}

# Samfylkingin gets 0 votes in one kjördæmi of the first closed election, where it has votes.
zero_votes_in_one_kd <- function(info) {
  r <- info$results
  i <- which(r$election == min(r$election) & r$flokkur == "Samfylkingin")[1]
  stopifnot(r$votes[i] > 0)
  r$votes[i] <- 0L
  info$results <- r
  info
}

duplicate_kd_row <- function(info) {
  info$polls_kd <- rbind(info$polls_kd, info$polls_kd[1, ])
  info
}

no_national_polls <- function(info) {
  info$polls_nat <- info$polls_nat[0, ]
  info
}

# The last national poll date moves to F + 1.
poll_after_F <- function(info) {
  last <- which(info$polls_nat$date == max(info$polls_nat$date))
  info$polls_nat$date[last] <- info$F + 1
  info
}
