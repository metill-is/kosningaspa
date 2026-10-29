# Sampler checks between runs: the stop rule, the adoption rule and the permutation rule of the
# Phase 2 plan (its Decisions 5, 6 and 8; docs/superpowers/plans/2026-10-28-v2-harness-phase2-v2-model.md).
# A plain script whose definitions also load as a box module, so that its test can import
# perm_z(), perm_verdict(), relative() and EQUIVALENCE_REQUIRED; the script part runs only when
# box::name() is NULL. Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop <arm> [run_list]
#   LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R adopt <ref_arm> <new_arm> <equivalence_csv> [run_list]
#   LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R perm <arm_a> <arm_b> [run_list]
#
# The run list is the queue's (harness/runs/; "lane arm E F seed" per line, # for comments),
# harness/runs/v2-steps.txt by default. Each run of an arm in it is read from
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
# adopt <ref_arm> <new_arm> <equivalence_csv>: the P-step adoption rule (Decision 6). The two
# arms must differ and have runs on the same cells, and on each cell each arm has exactly two
# fits, of two different seeds that are the other arm's (Decision 6 runs seeds 2701 and 2702,
# but any two seeds the arms share pass); this is checked before any run or the equivalence
# CSV is read. It prints a table of the fits
# (min bulk ESS per 1,000 gradients, divergences and the stop rule's verdict), a table of the
# cells (each arm's median over its fits of min bulk ESS per 1,000 gradients, and their ratio),
# the four conditions with yes or no, and a last line "ADOPT <new_arm>" if all four hold, else
# "KEEP <ref_arm>: <new_arm> fails condition(s) <i>, ...":
#   1. the equivalence check passes: equivalence_csv has no FAIL row, and a PASS row for each
#      check of EQUIVALENCE_REQUIRED, the four the step's check writes (the plan's Task 8):
#      2024 eve, 2021 f180 and 2017 eve (prior points only) in the default order, and 2024 eve
#      in the reversed order. A 2017 row is only a verdict, so the 2017 refusal, which is about
#      the runs read, does not apply to it;
#   2. on each cell, new_arm's median min bulk ESS per 1,000 gradients is at least 1.25 times
#      ref_arm's;
#   3. new_arm's divergences, summed over its fits, are at most ref_arm's plus 4;
#   4. no fit of either arm trips the stop rule.
# Both forms have the same posterior, so no forecast quantity is read. A fit's meta.json must
# hold n_leapfrog. Exits 0 on ADOPT and on KEEP.
#
# The equivalence CSV, one row per check that R/harness/v2_equivalence.R ran (Task 8 writes
# it), has at least these columns; any others are ignored:
#   E, F     the cell, ISO dates
#   order    "default" or "reversed", the party and kjördæmi order of both sides
#   verdict  "PASS" or "FAIL": the check's EQUIV verdict, its negative controls included
#
# perm <arm_a> <arm_b>: the permutation rule (Decision 8). The two arms must differ. Their runs
# are paired by cell, in run list order, and each arm must have as many runs on each cell. For
# each pair it reads only eday_national.parquet and eday_kjordaemi.parquet, and matches by name
# each national share (eday_national[<flokkur>]) and each kjördæmi share, votes over the
# kjördæmi's votes in the draw (eday_kjordaemi[<kjordaemi>,<flokkur>]); the two runs must have
# the same names. For the mean and the 5%, 50% and 95% quantiles of each, z = (a - b) /
# sqrt(MCSE_a^2 + MCSE_b^2) (posterior's mcse_mean() and mcse_quantile(); 0 when a = b; perm_z()).
# A pair passes when at most 3 |z| exceed 3, none exceeds 5 and the sd of the mean z's is at
# most 1.5 (perm_verdict()). A missing z (from a missing draw or an undefined MCSE) exceeds both:
# it is counted above 3, and the max |z| is then Inf. It prints only these statistics and the
# names of the variables whose |z| exceeds 3, never a share:
#   PERM PASS|FAIL <id_a> vs <id_b>: <n> z's, <k> with |z| > 3, max |z| <x>, sd of the mean z's <x>
#     |z| > 3: <variable> <mean|q05|q50|q95>, ...
# with |z| and the sd rounded up to two decimals. Exits 1 on any PERM FAIL.
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
  R / harness / constants[project_root],
  R / harness / freeze[forecast_free, target_cell],
  R / harness / runs[results_root, run_id, run_dir],
  R / harness / v2_reference[V2_LOG_CAPS]
)

DEFAULT_RUN_LIST <- "harness/runs/v2-steps.txt"
PREFIX <- "compare_runs: "
USAGE <- paste(
  "usage: Rscript R/harness/compare_runs.R stop <arm> [run_list]",
  "       Rscript R/harness/compare_runs.R adopt <ref_arm> <new_arm> <equivalence_csv> [run_list]",
  "       Rscript R/harness/compare_runs.R perm <arm_a> <arm_b> [run_list]",
  sep = "\n"
)

# Decision 5.
LP_RHAT_MAX <- 1.1
DIVERGENT_MAX <- 0.01
NEAR_CAP <- 0.5
NEAR_CAP_MAX <- 0.01
RHAT_MAX <- 1.10
# Decision 6.
ESS_RATIO_MIN <- 1.25
DIVERGENT_MARGIN <- 4
ADOPT_FITS <- 2L
EQUIVALENCE_COLUMNS <- c("E", "F", "order", "verdict")

#' The checks a P-step's equivalence CSV must hold a PASS row of (Decision 6's condition 1):
#' 2024 eve, 2021 f180 and 2017 eve (prior points only) in the default order, and 2024 eve in
#' the reversed order.
#' @export
EQUIVALENCE_REQUIRED <- data.frame(
  E = c("2024-11-30", "2021-09-25", "2017-10-28", "2024-11-30"),
  F = c("2024-11-29", "2021-03-29", "2017-10-27", "2024-11-29"),
  order = c("default", "default", "default", "reversed")
)

# Decision 8.
PERM_PROBS <- c(q05 = 0.05, q50 = 0.5, q95 = 0.95)
PERM_STATS <- c("mean", names(PERM_PROBS))
PERM_Z <- 3
PERM_Z_COUNT <- 3
PERM_Z_MAX <- 5
PERM_SD_MAX <- 1.5

fail <- function(...) stop(PREFIX, ..., call. = FALSE)

#' `text` with every path under the project root relative to it, and every path under the
#' results root (when outside the project) written as $HARNESS_RESULTS/..., so that no message
#' prints an absolute run directory.
#' @param text a message
#' @return `text` with those paths replaced
#' @export
relative <- function(text) {
  bases <- list(project_root(), results_root())
  labels <- c("", "$HARNESS_RESULTS/")
  for (i in seq_along(bases)) {
    forms <- unique(sub("/+$", "", c(bases[[i]], normalizePath(bases[[i]], mustWork = FALSE))))
    # WHY: the longer form first, since a resolved path can hold the given one (/private/var/... holds /var/...).
    for (base in forms[order(-nchar(forms))]) text <- gsub(paste0(base, "/"), labels[i], text, fixed = TRUE)
  }
  text
}

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

#' A z-statistic as printed: rounded up to two decimals, NA for a missing or undefined one.
z_shown <- function(x) if (is.na(x)) "NA" else shown(x, 2, up = TRUE)

#' The runs of a run list, each a character vector "lane arm E F seed".
read_run_list <- function(path) {
  if (!file.exists(path)) fail("no run list: ", path)
  lines <- trimws(readLines(path, warn = FALSE))
  fields <- strsplit(lines[nzchar(lines) & !startsWith(lines, "#")], "[[:space:]]+")
  if (any(lengths(fields) != 5)) fail(path, ": a run has 5 fields (lane arm E F seed)")
  fields
}

#' The runs of `arm` in a run list, in its order, each `list(arm, E, F, seed, id, cell)`; an
#' error for an arm without a run, and for a 2017 cell before the freeze.
arm_runs <- function(fields, arm, path) {
  mine <- Filter(\(f) identical(f[2], arm), fields)
  if (!length(mine)) fail("no run of arm ", arm, " in ", path)
  for (f in mine) if (forecast_free(f[3])) fail("2017 is not compared before the freeze (spec \u00a75.7)")
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

#' One run's stop rule (Decision 5).
#' @return a list: the signals, the recorded values, `per_gradient` (NA without n_leapfrog),
#'   `has_leapfrog`, `reasons` (the failed signals) and the printed `line`
stop_rule <- function(run) {
  meta <- read_meta(run)
  dir <- run_dir(run$id)
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
    verdict, run$id, rhat_shown(lp), divergent, n, near, n, rhat_shown(rhat), number(meta[["minutes"]], "minutes"),
    number(d[["treedepth_hits"]], "diagnostics$treedepth_hits"), shown(per_gradient, 3),
    if (length(reasons)) paste0("; stop: ", paste(reasons, collapse = "; ")) else ""
  )
  list(
    id = run$id, cell = run$cell, divergent = divergent, per_gradient = per_gradient, has_leapfrog = has_leapfrog,
    reasons = reasons, line = line
  )
}

run_stop <- function(arm, path) {
  rules <- lapply(arm_runs(read_run_list(path), arm, path), stop_rule)
  cat(vapply(rules, \(r) r$line, character(1)), sep = "\n")
  if (any(vapply(rules, \(r) length(r$reasons) > 0, logical(1)))) 1L else 0L
}

#' The equivalence CSV, validated: EQUIVALENCE_COLUMNS as text, E and F as dates, with `check`,
#' "<E>, <F>, <order>", added.
read_equivalence <- function(path) {
  if (!file.exists(path)) fail("no equivalence table: ", path)
  x <- readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
  lacks <- setdiff(EQUIVALENCE_COLUMNS, names(x))
  if (length(lacks)) fail(path, " lacks the column", if (length(lacks) > 1) "s", " ", paste(lacks, collapse = ", "))
  if (!nrow(x)) fail(path, " has no rows")
  dates <- lapply(x[c("E", "F")], \(d) as.Date(d, format = "%Y-%m-%d"))
  if (anyNA(unlist(dates))) fail(path, ": E and F must be ISO dates")
  if (!all(x[["order"]] %in% c("default", "reversed"))) fail(path, ": order must be default or reversed")
  if (!all(x[["verdict"]] %in% c("PASS", "FAIL"))) fail(path, ": verdict must be PASS or FAIL")
  x$check <- paste(format(dates$E), format(dates$F), x[["order"]], sep = ", ")
  x
}

#' An error unless, on each cell, each arm has exactly ADOPT_FITS fits of different seeds, the
#' same seeds as the other arm's.
check_fit_sets <- function(ref, new, ref_runs, new_runs, cells) {
  seeds_on <- function(runs, cell) vapply(Filter(\(r) identical(r$cell, cell), runs), \(r) r$seed, numeric(1))
  for (cell in cells) {
    for (side in list(list(ref, ref_runs), list(new, new_runs))) {
      s <- seeds_on(side[[2]], cell)
      if (length(s) != ADOPT_FITS || anyNA(s) || anyDuplicated(s)) {
        plural <- if (length(s) != 1) "s" else ""
        fail(
          side[[1]], " has ", length(s), " fit", plural, " on ", cell, " (seed", plural, " ", paste(s, collapse = ", "),
          "), not ", ADOPT_FITS, " of different seeds"
        )
      }
    }
    a <- seeds_on(ref_runs, cell)
    b <- seeds_on(new_runs, cell)
    if (!setequal(a, b)) {
      fail("the seeds of ", new, " on ", cell, ", ", paste(b, collapse = " and "), ", are not ", ref, "'s, ", paste(a, collapse = " and "))
    }
  }
}

run_adopt <- function(ref, new, csv, path) {
  if (identical(ref, new)) fail("adopt compares two different arms, not ", ref, " with itself")
  fields <- read_run_list(path)
  ref_runs <- arm_runs(fields, ref, path)
  new_runs <- arm_runs(fields, new, path)
  cells <- unique(vapply(ref_runs, \(r) r$cell, character(1)))
  new_cells <- unique(vapply(new_runs, \(r) r$cell, character(1)))
  if (!setequal(cells, new_cells)) {
    fail(
      ref, " and ", new, " have runs on different cells: ", paste(cells, collapse = "; "), " and ",
      paste(new_cells, collapse = "; ")
    )
  }
  check_fit_sets(ref, new, ref_runs, new_runs, cells)
  equivalence <- read_equivalence(csv)
  ref_rules <- lapply(ref_runs, stop_rule)
  new_rules <- lapply(new_runs, stop_rule)
  rules <- c(ref_rules, new_rules)
  for (r in rules) if (!r$has_leapfrog) fail("the meta.json of ", r$id, " has no diagnostics$n_leapfrog, which the adoption rule reads")

  table_row <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")
  fit_rows <- vapply(rules, \(r) table_row(c(
    r$id, shown(r$per_gradient, 3), sprintf("%.0f", r$divergent),
    if (length(r$reasons)) paste("STOP:", paste(r$reasons, collapse = "; ")) else "GO"
  )), character(1))
  centre <- function(rs, cell) stats::median(vapply(Filter(\(r) identical(r$cell, cell), rs), \(r) r$per_gradient, numeric(1)))
  by_cell <- lapply(cells, \(cell) {
    a <- centre(ref_rules, cell)
    b <- centre(new_rules, cell)
    parts <- strsplit(cell, ", ", fixed = TRUE)[[1]]
    tag <- tryCatch(target_cell(parts[1], parts[2])$tag, error = function(e) fail(cell, ": ", conditionMessage(e)))
    list(
      pass = isTRUE(b >= ESS_RATIO_MIN * a),
      row = c(paste(cell, tag, sep = ", "), shown(a, 3), shown(b, 3), shown(b / a, 2))
    )
  })
  cell_rows <- vapply(by_cell, \(x) table_row(c(x$row, if (x$pass) "yes" else "no")), character(1))

  required <- paste(EQUIVALENCE_REQUIRED$E, EQUIVALENCE_REQUIRED$F, EQUIVALENCE_REQUIRED$order, sep = ", ")
  failed <- unique(equivalence$check[equivalence[["verdict"]] == "FAIL"])
  missing <- setdiff(required, equivalence$check[equivalence[["verdict"]] == "PASS"])
  ref_divergent <- sum(vapply(ref_rules, \(r) r$divergent, numeric(1)))
  new_divergent <- sum(vapply(new_rules, \(r) r$divergent, numeric(1)))
  pass <- c(
    !length(failed) && !length(missing),
    all(vapply(by_cell, \(x) x$pass, logical(1))),
    isTRUE(new_divergent <= ref_divergent + DIVERGENT_MARGIN),
    !any(vapply(rules, \(r) length(r$reasons) > 0, logical(1)))
  )
  equivalence_detail <- if (pass[1]) {
    sprintf("%d rows, none FAIL, a PASS row of each of the %d required checks", nrow(equivalence), length(required))
  } else {
    paste(c(
      if (length(failed)) paste("a FAIL row on", paste(failed, collapse = "; ")),
      if (length(missing)) paste("no PASS row on", paste(missing, collapse = "; "))
    ), collapse = "; ")
  }
  yes <- function(ok) if (ok) "yes" else "no"
  conditions <- c(
    sprintf("1. The equivalence check passes (%s): %s (%s)", relative(csv), yes(pass[1]), equivalence_detail),
    sprintf(
      "2. On each cell, %s's median min bulk ESS per 1,000 gradients is at least %s \u00d7 %s's: %s",
      new, format(ESS_RATIO_MIN), ref, yes(pass[2])
    ),
    sprintf(
      "3. %s's divergences over its %d fits, %.0f, are at most %s's, %.0f, plus %d: %s",
      new, length(new_rules), new_divergent, ref, ref_divergent, DIVERGENT_MARGIN, yes(pass[3])
    ),
    sprintf("4. No fit of %s or %s trips the stop rule: %s", ref, new, yes(pass[4]))
  )
  verdict <- if (all(pass)) {
    paste("ADOPT", new)
  } else {
    sprintf("KEEP %s: %s fails condition%s %s", ref, new, if (sum(!pass) > 1) "s" else "", paste(which(!pass), collapse = ", "))
  }
  cat(c(
    sprintf("Adoption rule (Decision 6): %s against the reference form %s; run list %s", new, ref, relative(path)),
    "",
    table_row(c("Fit", "Min bulk ESS per 1,000 gradients (gate scope)", "Divergences", "Stop rule")),
    table_row(rep("---", 4)),
    fit_rows,
    "",
    table_row(c(
      "Cell (E, F, tag)", paste0(ref, ": median min bulk ESS per 1,000 gradients"),
      paste0(new, ": median min bulk ESS per 1,000 gradients"), "Ratio",
      sprintf("At least %s \u00d7 %s's", format(ESS_RATIO_MIN), ref)
    )),
    table_row(rep("---", 5)),
    cell_rows,
    "",
    conditions,
    verdict
  ), sep = "\n")
  0L
}

#' A run's eday shares, by variable name, each an iterations x chains matrix: the national
#' shares and each kjördæmi cell's share of its kjördæmi's votes.
eday_matrices <- function(run) {
  read_meta(run)
  dir <- run_dir(run$id)
  paths <- file.path(dir, c("eday_national.parquet", "eday_kjordaemi.parquet"))
  for (p in paths) if (!file.exists(p)) fail("no ", basename(p), " in ", dir)
  nat <- arrow::read_parquet(paths[1], col_select = c(".chain", ".iteration", "flokkur", "share"))
  kd <- arrow::read_parquet(paths[2], col_select = c(".chain", ".iteration", ".draw", "kjordaemi", "flokkur", "votes"))
  votes <- as.numeric(kd$votes)
  long <- rbind(
    data.frame(variable = paste0("eday_national[", nat$flokkur, "]"), .chain = nat$.chain, .iteration = nat$.iteration, value = nat$share),
    data.frame(
      variable = paste0("eday_kjordaemi[", kd$kjordaemi, ",", kd$flokkur, "]"), .chain = kd$.chain,
      .iteration = kd$.iteration, value = votes / stats::ave(votes, kd$.draw, kd$kjordaemi, FUN = sum)
    )
  )
  long <- long[order(long$variable, long$.chain, long$.iteration, method = "radix"), ]
  chains <- length(unique(long$.chain))
  lapply(split(long, long$variable), \(v) {
    per_chain <- tabulate(match(v$.chain, unique(v$.chain)))
    if (length(per_chain) != chains || length(unique(per_chain)) != 1 || anyDuplicated(v[c(".chain", ".iteration")])) {
      fail("the draws of ", v$variable[1], " in ", dir, " are not chains of equal length")
    }
    matrix(v$value, ncol = chains)
  })
}

#' The estimates and their MCSEs of one variable's draws: the mean and the PERM_PROBS quantiles,
#' all NA when a draw is missing.
estimates <- function(m) {
  if (anyNA(m)) {
    none <- stats::setNames(rep(NA_real_, length(PERM_STATS)), PERM_STATS)
    return(list(value = none, mcse = none))
  }
  list(
    value = stats::setNames(c(mean(m), stats::quantile(c(m), PERM_PROBS, names = FALSE)), PERM_STATS),
    mcse = stats::setNames(c(posterior::mcse_mean(m), unname(posterior::mcse_quantile(m, probs = PERM_PROBS))), PERM_STATS)
  )
}

#' Decision 8's z-statistics of one variable between two runs.
#' @param a,b the variable's draws in the two runs, iterations x chains matrices
#' @return a double vector named mean, q05, q50, q95: for each estimate, (a - b) /
#'   sqrt(MCSE_a^2 + MCSE_b^2) with posterior's mcse_mean() and mcse_quantile(); 0 when a = b,
#'   NA when a draw is missing or an MCSE undefined and a != b
#' @export
perm_z <- function(a, b) {
  x <- estimates(a)
  y <- estimates(b)
  delta <- x$value - y$value
  z <- delta / sqrt(x$mcse^2 + y$mcse^2)
  z[!is.na(delta) & delta == 0] <- 0
  stats::setNames(z, PERM_STATS)
}

#' Decision 8's verdict on one pair's z-statistics.
#' @param z the pair's z-statistics, named "<variable> <mean|q05|q50|q95>"
#' @return a list: `over` (a logical vector, |z| > PERM_Z, a missing z included), `biggest` (the
#'   max |z|, Inf when a z is missing), `spread` (the sd of the mean z's) and `pass` (at most
#'   PERM_Z_COUNT over, `biggest` at most PERM_Z_MAX and `spread` at most PERM_SD_MAX)
#' @export
perm_verdict <- function(z) {
  size <- abs(unname(z))
  size[is.na(size)] <- Inf
  over <- size > PERM_Z
  biggest <- max(size)
  spread <- stats::sd(z[endsWith(names(z), " mean")])
  list(
    over = over, biggest = biggest, spread = spread,
    pass = sum(over) <= PERM_Z_COUNT && biggest <= PERM_Z_MAX && isTRUE(spread <= PERM_SD_MAX)
  )
}

#' One pair's permutation statistics and its printed lines.
perm_pair <- function(run_a, run_b) {
  a <- eday_matrices(run_a)
  b <- eday_matrices(run_b)
  only_a <- setdiff(names(a), names(b))
  only_b <- setdiff(names(b), names(a))
  if (length(only_a) || length(only_b)) {
    fail(
      "the runs' variables differ: ", run_a$id, " alone has ", paste(c(only_a, "none")[1:max(1, length(only_a))], collapse = ", "),
      "; ", run_b$id, " alone has ", paste(c(only_b, "none")[1:max(1, length(only_b))], collapse = ", ")
    )
  }
  z <- unlist(lapply(names(a), \(v) stats::setNames(perm_z(a[[v]], b[[v]]), paste(v, PERM_STATS))))
  verdict <- perm_verdict(z)
  lines <- sprintf(
    "PERM %s %s vs %s: %d z's, %d with |z| > %s, max |z| %s, sd of the mean z's %s",
    if (verdict$pass) "PASS" else "FAIL", run_a$id, run_b$id, length(z), sum(verdict$over), format(PERM_Z),
    z_shown(verdict$biggest), z_shown(verdict$spread)
  )
  if (any(verdict$over)) lines <- c(lines, paste0("  |z| > ", format(PERM_Z), ": ", paste(names(z)[verdict$over], collapse = ", ")))
  list(pass = verdict$pass, lines = lines)
}

run_perm <- function(arm_a, arm_b, path) {
  if (identical(arm_a, arm_b)) fail("perm compares two different arms, not ", arm_a, " with itself")
  fields <- read_run_list(path)
  runs_a <- arm_runs(fields, arm_a, path)
  runs_b <- arm_runs(fields, arm_b, path)
  cell_a <- vapply(runs_a, \(r) r$cell, character(1))
  cell_b <- vapply(runs_b, \(r) r$cell, character(1))
  for (cell in union(cell_a, cell_b)) {
    if (sum(cell_a == cell) != sum(cell_b == cell)) fail(arm_a, " and ", arm_b, " have different numbers of runs on ", cell)
  }
  pairs <- unlist(lapply(unique(cell_a), \(cell) {
    Map(\(x, y) list(a = x, b = y), runs_a[cell_a == cell], runs_b[cell_b == cell])
  }), recursive = FALSE)
  results <- lapply(pairs, \(p) perm_pair(p$a, p$b))
  cat(unlist(lapply(results, \(r) r$lines)), sep = "\n")
  if (all(vapply(results, \(r) r$pass, logical(1)))) 0L else 1L
}

#' The script: its exit status, or an error "compare_runs: ..." with no absolute run directory.
main <- function(args) {
  run_list <- function(i) if (length(args) >= i) args[i] else DEFAULT_RUN_LIST
  command <- if (length(args)) args[1] else ""
  counts <- list(stop = 2:3, adopt = 4:5, perm = 3:4)
  tryCatch(
    {
      if (!command %in% names(counts) || !length(args) %in% counts[[command]]) fail(USAGE)
      switch(command,
        stop = run_stop(args[2], run_list(3)),
        adopt = run_adopt(args[2], args[3], args[4], run_list(5)),
        perm = run_perm(args[2], args[3], run_list(4))
      )
    },
    error = function(e) {
      message <- relative(conditionMessage(e))
      stop(if (startsWith(message, PREFIX)) "" else PREFIX, message, call. = FALSE)
    }
  )
}

if (is.null(box::name())) quit(save = "no", status = main(commandArgs(trailingOnly = TRUE)))
