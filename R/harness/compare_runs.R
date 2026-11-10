# Sampler checks between runs: the stop rule of the Phase 2 plan (its Decision 5; the plan is
# pull request #21's record). Its adoption and permutation
# rules (Decisions 6 and 8) ran during the P-steps and are kept, not run, in
# R/harness/archive/compare_runs_adopt_perm.R.
# A plain script whose definitions also load as a box module; the script part runs only when
# box::name() is NULL. Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop <arm> [run_list]
#
# The run list is the queue's (harness/runs/; "lane arm E F seed" per line, # for comments),
# harness/runs/v2.txt by default. Each run of an arm in it is read from
# results_root()/<run_id>, once its meta.json exists and names that run. A 2017 cell of the run
# list is refused before the freeze (forecast_free(); spec section 5.7), before any run is read.
#
# stop <arm>: the M-step stop rule (Decision 5). One line per run of <arm>, in run list order:
#   GO|STOP <id> lp_rhat=<x> divergent=<n>/<N> near_cap=<n>/<N> max_rhat=<x>; recorded:
#   minutes=<x> treedepth_hits=<n> ess_bulk_per_1k_grad=<x>[; stop: <reason>[; <reason>]]
# where N = chains x iter_sampling. A run STOPs on any of:
#   - lp_rhat, the R-hat of lp__ (its row in key_diagnostics.csv), above 1.1: a stuck chain;
#   - more than 1% of its N transitions divergent (more than 16 of 1,600);
#   - near_cap, the draws in which a bounded log concentration is within 0.5 of its cap
#     (cap - x <= 0.5), more than 1% of N. The concentrations are the param_draws.parquet
#     columns of log_phi (one per house), log_phi_K and log_kappa, their caps V2_LOG_CAPS
#     (R/harness/v2_reference.R); a draw counts once however many of them are near;
#   - max_rhat, the gate-scope max R-hat of meta.json's diagnostics, above 1.10.
# A signal that is NA, or absent (the R-hat of the lp__ row, meta.json's diagnostics$divergent
# or $max_rhat), is a STOP. R-hat is printed rounded up to four decimals, so a value printed at
# or below its limit is at or below it. Recorded, never a stop: the minutes, the treedepth hits
# and the gate-scope min bulk ESS per 1,000 gradients, 1000 * min_ess_bulk / n_leapfrog (NA for
# a meta.json without n_leapfrog). Exits 1 on any STOP.
#
# Any other failure (a bad command line, a run list without a run of the arm, a missing or
# foreign meta.json, a missing lp__ row or parquet column, an error from a package) stops with
# "compare_runs: ...", with each path under the project root printed relative to it and each
# under the results root as $HARNESS_RESULTS/..., never an absolute run directory.
if (is.null(box::name())) {
  if (!isTRUE(l10n_info()[["UTF-8"]])) stop("compare_runs: needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
  # WHY: the box path below is the working directory, so check it is the project root first.
  if (!file.exists("R/harness/constants.R")) stop("compare_runs: run from the project root (no R/harness/constants.R here)", call. = FALSE)
  options(box.path = normalizePath("."), width = 120, warn = 1)
}
box::use(
  R / harness / constants[NEAR_CAP, relative],
  R / harness / freeze[forecast_free],
  R / harness / runs[run_id, run_dir],
  R / harness / v2_reference[V2_LOG_CAPS]
)

DEFAULT_RUN_LIST <- "harness/runs/v2.txt"
PREFIX <- "compare_runs: "
USAGE <- "usage: Rscript R/harness/compare_runs.R stop <arm> [run_list]"

# Decision 5.
LP_RHAT_MAX <- 1.1
DIVERGENT_MAX <- 0.01
NEAR_CAP_MAX <- 0.01
RHAT_MAX <- 1.10

fail <- function(...) stop(PREFIX, ..., call. = FALSE)

#' A number of meta.json, where a non-finite one is the string "Inf", "-Inf", "NaN" or "NA".
#' @return a double; an error naming `field` for anything else, a missing field included
number <- function(x, field) {
  if (is.numeric(x) && length(x) == 1) {
    return(as.double(x))
  }
  words <- c("Inf" = Inf, "-Inf" = -Inf, "NaN" = NaN, "NA" = NA_real_)
  if (is.character(x) && length(x) == 1 && x %in% names(words)) {
    return(unname(words[x]))
  }
  fail("meta.json: ", field, " is not a number")
}

#' A stop signal of meta.json: as number(), but NA when the field is absent, so that it STOPs.
signal <- function(x, field) if (is.null(x)) NA_real_ else number(x, field)

#' `x` to `digits` decimals, rounded up or down; Inf, NaN and NA print as such.
shown <- function(x, digits, up = FALSE) {
  # WHY: x * 10^digits carries float error (1.0042 * 1e4 is 10042.000000000002), which ceiling() would round up.
  y <- round(x * 10^digits, 6)
  sprintf("%.*f", digits, (if (up) ceiling(y) else floor(y)) / 10^digits)
}

#' An R-hat as printed: rounded up to four decimals.
rhat_shown <- function(x) shown(x, 4, up = TRUE)


#' The runs of a run list, each a character vector "lane arm E F seed"; an error for a 2017
#' cell before the freeze on any line, whichever arm it names, before any run is read.
read_run_list <- function(path) {
  if (!file.exists(path)) fail("no run list: ", path)
  lines <- trimws(readLines(path, warn = FALSE))
  fields <- strsplit(lines[nzchar(lines) & !startsWith(lines, "#")], "[[:space:]]+")
  if (any(lengths(fields) != 5)) fail(path, ": a run has 5 fields (lane arm E F seed)")
  for (f in fields) if (forecast_free(f[3])) fail("2017 is not compared before the freeze (spec \u00a75.7)")
  fields
}

#' The runs of `arm` in a run list, in its order, each `list(arm, E, F, seed, id, cell)`; an
#' error for an arm without a run.
arm_runs <- function(fields, arm, path) {
  mine <- Filter(\(f) identical(f[2], arm), fields)
  if (!length(mine)) fail("no run of arm ", arm, " in ", path)
  lapply(mine, \(f) {
    seed <- suppressWarnings(as.numeric(f[5]))
    E <- as.Date(f[3])
    F <- as.Date(f[4])
    list(arm = arm, E = E, F = F, seed = seed, id = run_id(arm, E, F, seed), cell = paste(format(E), format(F), sep = ", "))
  })
}

#' A run's meta.json, after checking that it is the run's own.
read_meta <- function(run) {
  dir <- run_dir(run$id)
  path <- file.path(dir, "meta.json")
  if (!file.exists(path)) fail("no meta.json in ", dir, ": the run has not finished")
  meta <- jsonlite::read_json(path, simplifyVector = TRUE)
  as_day <- function(x) if (is.character(x) && length(x) == 1) tryCatch(as.Date(x), error = function(e) as.Date(NA)) else as.Date(NA)
  seed <- meta[["seed"]]
  own <- identical(meta[["arm"]], run$arm) && identical(as_day(meta[["E"]]), run$E) && identical(as_day(meta[["F"]]), run$F) &&
    is.numeric(seed) && length(seed) == 1 && isTRUE(seed == run$seed)
  if (!own) fail("the meta.json in ", dir, " is not of the run list's run ", run$id)
  meta
}

#' The R-hat of lp__ in a run's key_diagnostics.csv; NA for an empty or NA value.
lp_rhat <- function(dir) {
  path <- file.path(dir, "key_diagnostics.csv")
  if (!file.exists(path)) fail("no key_diagnostics.csv in ", dir)
  # WHY: readr reads "Inf" and "NaN" as NA; as.numeric() keeps them.
  key <- readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
  if (!all(c("variable", "rhat") %in% names(key))) fail("key_diagnostics.csv lacks its variable or rhat column (", dir, ")")
  rows <- which(key[["variable"]] == "lp__")
  if (!length(rows)) {
    fail("key_diagnostics.csv has no lp__ row (", dir, "): the run was fitted before it was written; retire it and refit")
  }
  if (length(rows) > 1) fail("key_diagnostics.csv has ", length(rows), " lp__ rows (", dir, ")")
  suppressWarnings(as.numeric(key[["rhat"]][rows]))
}

#' The number of a run's `n` draws in which a bounded log concentration is within NEAR_CAP of
#' its cap.
near_cap <- function(dir, n) {
  path <- file.path(dir, "param_draws.parquet")
  if (!file.exists(path)) fail("no param_draws.parquet in ", dir)
  pd <- arrow::read_parquet(path)
  if (nrow(pd) != n) fail("param_draws.parquet holds ", nrow(pd), " draws, not chains x iter_sampling = ", n, " (", dir, ")")
  base <- sub("\\[.*$", "", names(pd))
  near <- logical(nrow(pd))
  for (v in names(V2_LOG_CAPS)) {
    cols <- names(pd)[base == v]
    if (!length(cols)) fail("param_draws.parquet has no column of ", v, " (", dir, ")")
    x <- as.matrix(pd[cols])
    if (!is.numeric(x) || anyNA(x)) fail("param_draws.parquet holds a missing value of ", v, " (", dir, ")")
    near <- near | rowSums(V2_LOG_CAPS[[v]] - x <= NEAR_CAP) > 0
  }
  sum(near)
}

#' A reason, or NULL when `value` is at most `limit`.
above <- function(value, limit, reason) if (isTRUE(value <= limit)) NULL else reason

#' One run's stop rule (Decision 5), read from the directory that holds its outputs. A
#' harness run reaches it through `stop_rule()`; a directory that is not a `run_dir()` can be
#' passed directly.
#' @param dir the directory: its `key_diagnostics.csv` (the `lp__` row) and
#'   `param_draws.parquet` (the bounded log concentrations' columns) are read
#' @param meta its `meta.json`, read with `simplifyVector = TRUE`: `chains`, `iter_sampling`,
#'   `minutes` and `diagnostics` (`divergent`, `max_rhat`, `treedepth_hits`, `min_ess_bulk`,
#'   `n_leapfrog`) are read
#' @param id the name the printed line gives the run
#' @param cell the run's cell, as `arm_runs()` writes it, or NA
#' @return a list: the signals, the recorded values, `per_gradient` (NA without n_leapfrog),
#'   `has_leapfrog`, `reasons` (the failed signals) and the printed `line`
#' @export
stop_rule_at <- function(dir, meta, id, cell = NA_character_) {
  d <- meta[["diagnostics"]]
  if (!is.list(d)) fail("the meta.json in ", dir, ": diagnostics is not an object")
  n <- number(meta[["chains"]], "chains") * number(meta[["iter_sampling"]], "iter_sampling")
  lp <- lp_rhat(dir)
  divergent <- signal(d[["divergent"]], "diagnostics$divergent")
  near <- near_cap(dir, n)
  rhat <- signal(d[["max_rhat"]], "diagnostics$max_rhat")
  has_leapfrog <- !is.null(d[["n_leapfrog"]])
  per_gradient <- if (has_leapfrog) {
    1000 * number(d[["min_ess_bulk"]], "diagnostics$min_ess_bulk") / number(d[["n_leapfrog"]], "diagnostics$n_leapfrog")
  } else {
    NA_real_
  }
  limit <- function(share) share * n
  reasons <- c(
    above(lp, LP_RHAT_MAX, sprintf("R-hat of lp__ %s > %s", rhat_shown(lp), format(LP_RHAT_MAX))),
    above(divergent, limit(DIVERGENT_MAX), sprintf("%.0f of %.0f transitions divergent > 1%%", divergent, n)),
    above(near, limit(NEAR_CAP_MAX), sprintf("%d of %.0f draws within %s of a cap > 1%%", near, n, format(NEAR_CAP))),
    above(rhat, RHAT_MAX, sprintf("max R-hat (gate scope) %s > %s", rhat_shown(rhat), format(RHAT_MAX)))
  )
  verdict <- if (length(reasons)) "STOP" else "GO"
  line <- sprintf(
    "%s %s lp_rhat=%s divergent=%.0f/%.0f near_cap=%d/%.0f max_rhat=%s; recorded: minutes=%.1f treedepth_hits=%.0f ess_bulk_per_1k_grad=%s%s",
    verdict, id, rhat_shown(lp), divergent, n, near, n, rhat_shown(rhat), number(meta[["minutes"]], "minutes"),
    number(d[["treedepth_hits"]], "diagnostics$treedepth_hits"), shown(per_gradient, 3),
    if (length(reasons)) paste0("; stop: ", paste(reasons, collapse = "; ")) else ""
  )
  list(
    id = id, cell = cell, divergent = divergent, per_gradient = per_gradient, has_leapfrog = has_leapfrog,
    reasons = reasons, line = line
  )
}

#' One run's stop rule (Decision 5): `stop_rule_at()` on its `run_dir()` and its own
#' `meta.json` (`read_meta()`).
stop_rule <- function(run) stop_rule_at(run_dir(run$id), read_meta(run), run$id, run$cell)

run_stop <- function(arm, path) {
  rules <- lapply(arm_runs(read_run_list(path), arm, path), stop_rule)
  cat(vapply(rules, \(r) r$line, character(1)), sep = "\n")
  if (any(vapply(rules, \(r) length(r$reasons) > 0, logical(1)))) 1L else 0L
}

#' The script: its exit status, or an error "compare_runs: ..." with no absolute run directory.
main <- function(args) {
  tryCatch(
    {
      if (!length(args) %in% 2:3 || !identical(args[1], "stop")) fail(USAGE)
      run_stop(args[2], if (length(args) == 3) args[3] else DEFAULT_RUN_LIST)
    },
    error = function(e) {
      message <- relative(conditionMessage(e))
      stop(if (startsWith(message, PREFIX)) "" else PREFIX, message, call. = FALSE)
    }
  )
}

if (is.null(box::name())) quit(save = "no", status = main(commandArgs(trailingOnly = TRUE)))
