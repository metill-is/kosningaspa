# The Tier 0 report (spec section 5.8): the sampler diagnostics of an arm's Tier 0 runs, and
# nothing of their forecasts. A plain script, not a box module. Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R <arm> <run_list> [out]
#
# The run list is the queue's (harness/runs/; "lane arm E F seed" per line, # for comments).
# For each run of <arm> in it, in order, the script reads only the run's meta.json, in
# results_root()/<run_id>, and writes one row of a markdown table to out (default
# harness/tier0/<arm>.md):
#   the cell (E, F, tag), the seed and the iterations; the minutes, divergences, treedepth hits
#   (count, and percentage of the chains x sampling draws), the deepest tree and the min
#   E-BFMI; the max R-hat and the min bulk and tail ESS over the gate's scope (every
#   parameters-block variable and the scored quantities) and over the scored quantities alone;
#   both min bulk ESS per minute and projected linearly to 2000 sampling iterations; the gate's
#   verdict and reasons; and the modification time of meta.json, the measurement date.
# Above the table it records the run list, the CmdStan and cmdstanr versions and the
# max_treedepth of meta.json, and `sysctl -n hw.ncpu` read when it runs. It stops, writing
# nothing, on a run list with no run of <arm>, a run with no meta.json, a meta.json of another
# run, or a field that is missing or does not parse.
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("tier0_report.R needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
# WHY: the box path below is the working directory, so check it is the project root first.
stopifnot("run from the project root (no R/harness/constants.R here)" = file.exists("R/harness/constants.R"))
options(box.path = normalizePath("."), width = 120, warn = 1)
box::use(
  R / harness / constants[harness_path],
  R / harness / freeze[target_cell],
  R / harness / runs[run_id, run_dir]
)

SCOPE <- "(gate scope: parameters block + scored; scored)"

HEADINGS <- c(
  "Cell (E, F, tag)", "Seed", "Iterations (warmup + sampling)", "Minutes", "Divergences",
  "Treedepth hits (count; % of chains \u00d7 sampling)", "Deepest tree", "Min E-BFMI",
  paste("Max R-hat", SCOPE), paste("Min bulk ESS", SCOPE), paste("Min tail ESS", SCOPE),
  paste("Bulk ESS/min", SCOPE),
  paste("Bulk ESS at 2000 sampling iterations, linear projection", SCOPE),
  "Gate (\u00a75.7, informational)", "meta.json written"
)

DIAGNOSTICS <- c(
  "max_rhat", "max_rhat_scored", "min_ess_bulk", "min_ess_bulk_scored", "min_ess_tail",
  "min_ess_tail_scored", "divergent", "treedepth_hits", "max_depth", "min_ebfmi"
)

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
  stop("meta.json: ", field, " is not a number", call. = FALSE)
}

#' A text field of meta.json; an error naming `field` if it is missing or not one string.
text <- function(x, field) {
  if (!is.character(x) || length(x) != 1) stop("meta.json: ", field, " is not one string", call. = FALSE)
  x
}

#' A date field of meta.json; an error naming `field` if it is missing or not one ISO date.
day <- function(x, field) {
  d <- tryCatch(as.Date(text(x, field)), error = function(e) as.Date(NA))
  if (is.na(d)) stop("meta.json: ", field, " is not a date", call. = FALSE)
  d
}

#' `x` to `digits` decimals, rounded up or down; Inf, NaN and NA print as such.
shown <- function(x, digits, up = FALSE) {
  # WHY: x * 10^digits carries float error (1.0042 * 1e4 is 10042.000000000002), which ceiling() would round up.
  y <- round(x * 10^digits, 6)
  sprintf("%.*f", digits, (if (up) ceiling(y) else floor(y)) / 10^digits)
}

#' One run's row of the table, the run named by a run list line's `arm`, `E`, `F` and `seed`.
#' @return a list: the run's `id`; `cells`, the row's cells in the order of HEADINGS, each `|`
#'   escaped for markdown; and `setup`, the run's CmdStan and cmdstanr versions and max_treedepth
run_row <- function(arm, E, F, seed) {
  id <- run_id(arm, E, F, as.numeric(seed))
  dir <- run_dir(id)
  meta_path <- file.path(dir, "meta.json")
  if (!file.exists(meta_path)) stop("no meta.json in ", dir, ": the run has not finished", call. = FALSE)
  meta <- jsonlite::read_json(meta_path, simplifyVector = TRUE)
  meta_E <- day(meta$E, "E")
  meta_F <- day(meta$F, "F")
  if (!identical(text(meta$arm, "arm"), arm) || meta_E != as.Date(E) || meta_F != as.Date(F) ||
    !isTRUE(number(meta$seed, "seed") == as.numeric(seed))) {
    stop("the meta.json in ", dir, " is not of the run list's run ", id, call. = FALSE)
  }
  d <- meta$diagnostics
  if (!is.list(d)) stop("meta.json: diagnostics is not an object", call. = FALSE)
  v <- lapply(stats::setNames(DIAGNOSTICS, DIAGNOSTICS), \(f) number(d[[f]], paste0("diagnostics$", f)))
  minutes <- number(meta$minutes, "minutes")
  warmup <- number(meta$iter_warmup, "iter_warmup")
  sampling <- number(meta$iter_sampling, "iter_sampling")
  draws <- number(meta$chains, "chains") * sampling
  rhat <- c(v$max_rhat, v$max_rhat_scored)
  bulk <- c(v$min_ess_bulk, v$min_ess_bulk_scored)
  tail <- c(v$min_ess_tail, v$min_ess_tail_scored)

  pass <- meta$gate$pass
  reasons <- as.character(unlist(meta$gate$reasons))
  gate <- if (isTRUE(pass)) {
    "PASS"
  } else if (isFALSE(pass)) {
    paste(c("FAIL", if (length(reasons)) paste0(": ", paste(reasons, collapse = "; "))), collapse = "")
  } else {
    "NA"
  }
  pair <- function(x) paste(x, collapse = "; ")
  cells <- c(
    sprintf("%s, %s, %s", format(meta_E), format(meta_F), target_cell(meta_E, meta_F)$tag),
    sprintf("%.0f", number(meta$seed, "seed")),
    sprintf("%.0f + %.0f", warmup, sampling),
    sprintf("%.1f", minutes),
    sprintf("%.0f", v$divergent),
    sprintf("%.0f (%.1f%%)", v$treedepth_hits, 100 * v$treedepth_hits / draws),
    sprintf("%.0f", v$max_depth),
    shown(v$min_ebfmi, 3),
    pair(shown(rhat, 4, up = TRUE)),
    pair(shown(bulk, 0)),
    pair(shown(tail, 0)),
    pair(shown(bulk / minutes, 1)),
    pair(shown(bulk * 2000 / sampling, 0)),
    gate,
    format(file.mtime(meta_path), "%Y-%m-%d %H:%M UTC", tz = "UTC")
  )
  setup <- sprintf(
    "CmdStan %s and cmdstanr %s, `max_treedepth` %.0f",
    text(meta$cmdstan_version, "cmdstan_version"), text(meta$cmdstanr_version, "cmdstanr_version"),
    number(meta$max_treedepth, "max_treedepth")
  )
  list(id = id, cells = gsub("|", "\\|", cells, fixed = TRUE), setup = setup)
}

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 2:3) stop("usage: Rscript R/harness/tier0_report.R <arm> <run_list> [out]", call. = FALSE)
arm <- args[1]
run_list <- args[2]
out <- if (length(args) == 3) args[3] else harness_path("tier0", paste0(arm, ".md"))
if (!file.exists(run_list)) stop("no run list: ", run_list, call. = FALSE)

lines <- trimws(readLines(run_list, warn = FALSE))
fields <- strsplit(lines[nzchar(lines) & !startsWith(lines, "#")], "[[:space:]]+")
if (any(lengths(fields) != 5)) stop(run_list, ": a run has 5 fields (lane arm E F seed)", call. = FALSE)
fields <- Filter(\(f) identical(f[2], arm), fields)
if (!length(fields)) stop("no run of arm ", arm, " in ", run_list, call. = FALSE)
rows <- lapply(fields, \(f) run_row(f[2], f[3], f[4], f[5]))

ncpu <- tryCatch(
  suppressWarnings(system2("sysctl", c("-n", "hw.ncpu"), stdout = TRUE, stderr = FALSE)),
  error = function(e) character(0)
)
if (length(ncpu) != 1 || !nzchar(ncpu)) ncpu <- "unavailable"
setups <- vapply(rows, \(r) r$setup, character(1))
setup <- if (length(unique(setups)) == 1) {
  sprintf("- %s (`meta.json`)", setups[1])
} else {
  sprintf("- %s: %s (`meta.json`)", vapply(rows, \(r) r$id, character(1)), setups)
}
table_row <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")

report <- c(
  paste("# Tier 0 diagnostics: arm", arm),
  "",
  "Written by `R/harness/tier0_report.R` from each run's `meta.json`.",
  "Sampler diagnostics only. Tier 0 is not gated: the gate column is informational.",
  "",
  sprintf("- Run list: `%s`", run_list),
  setup,
  sprintf("- hw.ncpu: %s (`sysctl -n hw.ncpu`, read when this report was written)", ncpu),
  "",
  table_row(HEADINGS),
  table_row(rep("---", length(HEADINGS))),
  vapply(rows, \(r) table_row(r$cells), character(1)),
  "",
  "The columns:",
  "",
  paste(
    "- **Gate scope: parameters block + scored**, the first value of each pair, is `meta.json`'s",
    "`max_rhat`, `min_ess_bulk` and `min_ess_tail`: the extremes over every `parameters`-block",
    "variable and the scored quantities together, the scope of the gate's R-hat rule (the plan's",
    "Decision 3); when it equals the scored value, the scored set is the binding one. **Scored**,",
    "the second, is the scored quantities alone (`max_rhat_scored`, `min_ess_bulk_scored`,",
    "`min_ess_tail_scored`). A parameter with no R-hat or ESS (a constant) is left out of the gate",
    "scope; a scored quantity with none makes both values NA."
  ),
  paste(
    "- The gate checks the gate-scope max R-hat and the scored min bulk ESS; a failing gate's",
    "reasons print the value that failed."
  ),
  paste(
    "- R-hat is rounded up, and ESS, ESS/min and E-BFMI down, to the digits shown. `Inf`, `NaN`",
    "and `NA` are printed as the run recorded them."
  ),
  paste(
    "- **Treedepth hits** are transitions that reached `max_treedepth`, as a percentage of the",
    "chains \u00d7 sampling draws; the **deepest tree** is the deepest any transition reached."
  ),
  "- **Bulk ESS/min** is each min bulk ESS \u00f7 the minutes, the fit's wall time (warmup and sampling).",
  paste(
    "- **Bulk ESS at 2000 sampling iterations** scales each min bulk ESS by 2000 / `iter_sampling`.",
    "It is a linear projection that informs Decision 5, not the gate."
  ),
  "- **meta.json written** is the modification time of the run's `meta.json`: the date of the measurement."
)
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
writeLines(report, out)
cat(sprintf("wrote %s (%d runs)\n", out, length(rows)))
