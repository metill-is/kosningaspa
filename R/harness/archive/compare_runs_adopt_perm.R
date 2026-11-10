# Archived: the adoption rule (Decision 6) and the permutation rule (Decision 8) of the Phase 2
# plan, once the adopt and perm commands of R/harness/compare_runs.R. They ran during the V2
# P-steps and are kept as a record; this file is not run or tested and does not load on its own
# (it used compare_runs.R's fail(), number(), shown(), read_run_list(), arm_runs(), read_meta(),
# stop_rule_at() and relative() as they were). R/harness/archive/README.md names the commit to
# check out to run them.
#   LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R adopt <ref_arm> <new_arm> <equivalence_csv> [run_list]
#   LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R perm <arm_a> <arm_b> [run_list]
#
# adopt <ref_arm> <new_arm> <equivalence_csv>: the P-step adoption rule (Decision 6). The two
# arms must differ and have runs on the same cells, and on each cell each arm has exactly two
# fits, of two different seeds that are the other arm's (Decision 6 runs seeds 2701 and 2702,
# but any two seeds the arms share pass), and the cells are exactly Decision 6's two, 2024 eve
# (2024-11-30, 2024-11-29) and 2021 f180 (2021-09-25, 2021-03-29); this is checked before any
# run or the equivalence CSV is read. Every fit of both arms must then have the same
# iter_warmup and the same iter_sampling in its meta.json, whatever their values, since
# condition 3 compares divergence counts. It prints a table of the fits (min bulk ESS per 1,000 gradients, divergences
# and the stop rule's verdict), a table of the cells (each arm's median over its fits of min
# bulk ESS per 1,000 gradients, and their ratio), the four conditions with yes or no, and a
# last line "ADOPT <new_arm>" if all four hold, else
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
# Decision 6.
ESS_RATIO_MIN <- 1.25
DIVERGENT_MARGIN <- 4
ADOPT_FITS <- 2L
# WHY: condition 3 compares divergence counts summed over each arm's fits, so the design is
# pinned: these two cells (2024 eve and 2021 f180, "<E>, <F>" as arm_runs() writes a cell), and
# equal iterations in every fit.
ADOPT_CELLS <- c("2024-11-30, 2024-11-29", "2021-09-25, 2021-03-29")
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

#' A z-statistic as printed: rounded up to two decimals, NA for a missing or undefined one.
z_shown <- function(x) if (is.na(x)) "NA" else shown(x, 2, up = TRUE)

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

#' An error unless the arms' cells are exactly ADOPT_CELLS.
check_adopt_cells <- function(cells) {
  lacks <- setdiff(ADOPT_CELLS, cells)
  extra <- setdiff(cells, ADOPT_CELLS)
  if (length(lacks) || length(extra)) {
    fail(
      "adopt runs on Decision 6's two cells, ", paste(ADOPT_CELLS, collapse = " and "), ": ",
      paste(c(
        if (length(lacks)) paste("no runs on", paste(lacks, collapse = "; ")),
        if (length(extra)) paste("runs on", paste(extra, collapse = "; "))
      ), collapse = ", and ")
    )
  }
}

#' An error unless every fit's meta.json has the same iter_warmup, and the same iter_sampling.
#' @param runs both arms' runs
#' @param metas their meta.json, in the same order
check_adopt_iterations <- function(runs, metas) {
  for (field in c("iter_warmup", "iter_sampling")) {
    x <- vapply(metas, \(m) number(m[[field]], field), numeric(1))
    if (anyNA(x) || length(unique(x)) != 1) {
      shown_x <- ifelse(is.na(x), "NA", format(x, scientific = FALSE, trim = TRUE))
      groups <- vapply(unique(shown_x), \(v) {
        paste0(v, " in ", paste(vapply(runs[shown_x == v], \(r) r$id, character(1)), collapse = ", "))
      }, character(1))
      fail("adopt compares fits of equal iterations, but their ", field, " differ: ", paste(groups, collapse = "; "))
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
  check_adopt_cells(cells)
  equivalence <- read_equivalence(csv)
  runs <- c(ref_runs, new_runs)
  metas <- lapply(runs, read_meta)
  check_adopt_iterations(runs, metas)
  rules <- Map(\(r, meta) stop_rule_at(run_dir(r$id), meta, r$id, r$cell), runs, metas)
  ref_rules <- rules[seq_along(ref_runs)]
  new_rules <- rules[length(ref_runs) + seq_along(new_runs)]
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
