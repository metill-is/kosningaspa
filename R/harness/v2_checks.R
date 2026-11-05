# The V2 Tier 0 checks (spec section 6.9; the Phase 2 plan's Task 16): the extras beside the Tier 0
# report (tier0_report.R), and the posterior predictive check of the past elections. A plain
# script, not a box module. Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/v2_checks.R <run_list> [out]
#
# The run list is the queue's (harness/runs/; "lane arm E F seed" per line, # for comments). For
# each run in it, in order, the script reads only the run's meta.json, param_draws.parquet and
# ppc_results.parquet, in results_root()/<run_id>, and the closed elections' official counts in
# harness/truth/elections_official.csv (read_truth(), folded to the poll parties by fold_truth()).
# It writes two markdown tables to out (default harness/tier0/v2_checks.md):
#   - one row per run: the minutes, and the minutes projected to 1000 + 2000 iterations,
#     minutes x 3000 / (iter_warmup + iter_sampling), flagged above 120 (spec section 6.9); the
#     posterior correlation of log_phi_K and s_bar_zeta[1], the chains pooled; for each bounded
#     log concentration, the draws within 2 of its cap on the model's log scale (log c > cap - 2,
#     the caps V2_LOG_CAPS), for log_phi (one column per house) the largest count over the houses
#     and the house that has it; and the (sigma, Omega) flat directions, computed per draw from
#     sigma and L_Omega: the min bulk ESS over the chains of each distinct entry of C Sigma C', and
#     of each off-diagonal entry of Omega, where Sigma = diag(sigma) Omega diag(sigma),
#     Omega = L_Omega L_Omega' and C = I - 11'/P over the run's P parties;
#   - one row per run and closed election: the count of kjördæmi x party cells whose mid-PIT
#     under the in-sample replicates y_rep_res, P(y_rep < y) + P(y_rep = y) / 2, lies outside
#     [0.025, 0.975], and the number of cells, the official count y matched by election,
#     kjördæmi and party name, never by position. A party with no official count at an election
#     (no list there) is left out, and its replicates there must all be 0.
# It prints "FLAG <id>: projected to <x> minutes at 1000 + 2000, over 120 (...)" for each run
# projected above 120 minutes, then "wrote <out> (<n> runs)". It reads no forecast quantity (no
# eday_* or latent_* parquet) and writes none: counts, a correlation, ESS and minutes only.
# It stops with "v2_checks: ...", writing nothing, on a bad command line or run list, a 2017
# cell before the freeze (forecast_free(); spec section 5.7), a run with no meta.json or with one
# of another run, a cell that is not a forecast cell, a meta.json field it reads that is missing
# or does not parse, a parquet that is missing, lacks a column it reads or holds a missing value
# there, a param_draws.parquet that does not hold chains x iter_sampling draws (one per chain and
# iteration) or holds elements of sigma, L_Omega or log_phi beyond meta.json's parties and houses,
# a ppc_results.parquet whose elections are not meta.json's closed elections, which names a
# kjördæmi or party outside meta.json's levels, or does not hold chains x iter_sampling draws of
# each cell, a party outside the run with an official count at a closed election, or a nonzero
# replicate of a party with none.
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("v2_checks.R needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
# WHY: the box path below is the working directory, so check it is the project root first.
stopifnot("run from the project root (no R/harness/constants.R here)" = file.exists("R/harness/constants.R"))
options(box.path = normalizePath("."), width = 120, warn = 1)
box::use(
  R / harness / constants[harness_path],
  R / harness / freeze[forecast_free, target_cell],
  R / harness / runs[run_id, run_dir],
  R / harness / truth[fold_truth, read_truth],
  R / harness / v2_reference[V2_LOG_CAPS]
)

PREFIX <- "v2_checks: "
USAGE <- "usage: Rscript R/harness/v2_checks.R <run_list> [out]"
#' The mid-PIT interval outside which a past-election cell counts.
PIT_LOW <- 0.025
PIT_HIGH <- 0.975
#' A draw is near a cap when its log concentration is more than cap - NEAR.
NEAR <- 2
#' Spec section 6.9's 1000 + 2000 iterations, and the projected minutes above which a fit is flagged.
SPEC_ITERATIONS <- 3000
FLAG_MINUTES <- 120

fail <- function(...) stop(PREFIX, ..., call. = FALSE)

#' The values of `x` joined for a message.
listed <- function(x) paste(unique(x), collapse = "; ")

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

#' A positive whole number of meta.json; an error naming `field` for anything else.
count_of <- function(x, field) {
  n <- number(x, field)
  if (!is.finite(n) || n < 1 || n != round(n)) fail("meta.json: ", field, " is not a positive whole number")
  n
}

#' A date field of meta.json; an error naming `field` if it is missing or not one ISO date.
day <- function(x, field) {
  d <- if (is.character(x) && length(x) == 1) tryCatch(as.Date(x), error = function(e) as.Date(NA)) else as.Date(NA)
  if (is.na(d)) fail("meta.json: ", field, " is not a date")
  d
}

#' Labels of meta.json (party_order, kjordaemi_levels, house_levels, closed_elections); an error
#' naming `field` unless they are one or more distinct strings.
labels_of <- function(x, field) {
  if (!is.character(x) || !length(x) || anyNA(x) || anyDuplicated(x)) {
    fail("meta.json: ", field, " is not one or more distinct strings")
  }
  x
}

#' An ESS as printed: rounded down, NA for an undefined one.
ess_shown <- function(x) {
  # WHY: x carries float error (38 may be 37.99999999999), which floor() would round down.
  if (is.na(x)) "NA" else sprintf("%.0f", floor(round(x, 6)))
}

#' A count of draws as printed: "<k> of <n> (<percent>%)".
draws_shown <- function(k, n) sprintf("%.0f of %.0f (%.1f%%)", k, n, 100 * k / n)

#' The runs of a run list, each a character vector "lane arm E F seed"; an error for a malformed
#' or empty list, and for a 2017 cell before the freeze, before any run is read.
read_run_list <- function(path) {
  if (!file.exists(path)) fail("no run list: ", path)
  lines <- trimws(readLines(path, warn = FALSE))
  fields <- strsplit(lines[nzchar(lines) & !startsWith(lines, "#")], "[[:space:]]+")
  if (!length(fields)) fail("no run in ", path)
  if (any(lengths(fields) != 5)) fail(path, ": a run has 5 fields (lane arm E F seed)")
  for (f in fields) if (forecast_free(f[3])) fail("2017 is not checked before the freeze (spec \u00a75.7)")
  fields
}

#' The columns `cols` of a parquet table, after checking that each is present, numeric where
#' `numeric`, and has no missing value.
need <- function(table, name, cols, id, numeric = TRUE) {
  lacks <- setdiff(cols, names(table))
  if (length(lacks)) fail(name, " lacks ", listed(lacks), " (run ", id, ")")
  bad <- cols[vapply(cols, \(col) anyNA(table[[col]]) || (numeric && !is.numeric(table[[col]])), logical(1))]
  if (length(bad)) fail(name, " holds a missing or non-numeric value of ", listed(bad), " (run ", id, ")")
  table[cols]
}

#' An error if `table` holds an element of variable `base` other than the columns `want`.
only <- function(table, name, base, want, id) {
  extra <- setdiff(names(table)[sub("\\[.*$", "", names(table)) == base], want)
  if (length(extra)) fail(name, " holds elements of ", base, " beyond meta.json's levels: ", listed(extra), " (run ", id, ")")
}

#' The row order that sorts `table` by chain, then iteration, after checking that its .chain and
#' .iteration hold each of the chains x iterations once.
draw_order <- function(table, name, chains, iters, id) {
  n <- chains * iters
  if (nrow(table) != n) fail(name, " holds ", nrow(table), " draws, not chains x iter_sampling = ", n, " (run ", id, ")")
  ids <- need(table, name, c(".chain", ".iteration"), id)
  key <- (ids$.chain - 1) * iters + ids$.iteration
  if (any(ids$.iteration < 1 | ids$.iteration > iters) || !setequal(key, seq_len(n)) || anyDuplicated(key)) {
    fail(name, "'s .chain and .iteration do not hold each of the ", chains, " x ", iters, " draws once (run ", id, ")")
  }
  order(ids$.chain, ids$.iteration)
}

#' The min bulk ESS over the distinct entries of C Sigma C', and over the off-diagonal entries of
#' Omega, computed per draw.
#' @param sigma the draws x P matrix of sigma
#' @param L the draws x P^2 matrix of L_Omega, each row column-major
#' @param chains,iters the draws are chain by chain, iteration by iteration
#' @return `c(sigma = , omega = )`, NA where an entry's ESS is undefined
flat_ess <- function(sigma, L, chains, iters) {
  P <- ncol(sigma)
  distinct <- which(upper.tri(diag(P), diag = TRUE))
  off <- which(upper.tri(diag(P)))
  csc <- matrix(NA_real_, nrow(sigma), length(distinct))
  omega <- matrix(NA_real_, nrow(sigma), length(off))
  for (d in seq_len(nrow(sigma))) {
    Ld <- matrix(L[d, ], P, P)
    # WHY: diag(sigma) L, then C = I - 11'/P subtracts each column's mean; C Sigma C' = (C diag(sigma) L)(C diag(sigma) L)'.
    M <- sigma[d, ] * Ld
    CM <- M - rep(colMeans(M), each = P)
    csc[d, ] <- tcrossprod(CM)[distinct]
    omega[d, ] <- tcrossprod(Ld)[off]
  }
  ess <- function(m) apply(m, 2, \(x) posterior::ess_bulk(matrix(x, iters, chains)))
  c(sigma = min(ess(csc)), omega = min(ess(omega)))
}

#' One run's past-election cells: for each closed election, the cells of the parties with an
#' official count there whose mid-PIT lies outside [PIT_LOW, PIT_HIGH], and the number of cells.
#' @return a tibble (election, outside, cells) in meta.json's closed-election order
ppc_counts <- function(ppc, meta, closed, n, truth, id) {
  name <- "ppc_results.parquet"
  need(ppc, name, c(".chain", ".iteration", "votes"), id)
  need(ppc, name, c("election", "kjordaemi", "flokkur"), id, numeric = FALSE)
  elections <- format(as.Date(ppc$election))
  if (!setequal(elections, format(closed))) fail(name, "'s elections are not meta.json's closed elections (run ", id, ")")
  outside <- c(setdiff(ppc$kjordaemi, meta$kjordaemi_levels), setdiff(ppc$flokkur, meta$party_order))
  if (length(outside)) fail(name, " names a kj\u00f6rd\u00e6mi or party outside meta.json's levels: ", listed(outside), " (run ", id, ")")
  key <- paste(elections, ppc$kjordaemi, ppc$flokkur, sep = "\r")
  per_cell <- table(key)
  if (length(per_cell) != length(closed) * length(meta$kjordaemi_levels) * length(meta$party_order) ||
    any(per_cell != n) || anyDuplicated(paste(key, ppc$.chain, ppc$.iteration, sep = "\r"))) {
    fail(name, " does not hold ", n, " draws of each cell (closed election, kj\u00f6rd\u00e6mi, party) (run ", id, ")")
  }

  folded <- fold_truth(truth[truth$election %in% closed, ])
  national <- dplyr::summarise(folded, total = sum(votes), .by = c(election, flokkur))
  listed_at <- national[national$total > 0, ]
  foreign <- listed_at$flokkur[!listed_at$flokkur %in% meta$party_order]
  if (length(foreign)) fail("parties outside run ", id, " have official counts at its closed elections: ", listed(foreign))
  y <- folded$votes[match(key, paste(format(folded$election), folded$kjordaemi, folded$flokkur, sep = "\r"))]
  if (anyNA(y)) fail("no official count for ", listed(sub("\r", ", ", key[is.na(y)], fixed = TRUE)), " (run ", id, ")")
  has_list <- paste(elections, ppc$flokkur) %in% paste(format(listed_at$election), listed_at$flokkur)
  stray <- !has_list & ppc$votes != 0
  if (any(stray)) {
    fail(
      name, " holds a replicate count for a party with no official count at that election: ",
      listed(paste(elections[stray], ppc$flokkur[stray])), " (run ", id, ")"
    )
  }

  rows <- tibble::tibble(key = key, election = elections, below = ppc$votes < y, tie = ppc$votes == y)[has_list, ]
  # WHY: (below + tie / 2) / n, not mean(below) + mean(tie) / 2, so that a PIT on the interval's edge (10 / 400) is that double exactly.
  pit <- dplyr::summarise(rows, pit = (sum(below) + sum(tie) / 2) / dplyr::n(), .by = c(key, election))
  out <- dplyr::summarise(pit, outside = sum(pit < PIT_LOW | pit > PIT_HIGH), cells = dplyr::n(), .by = election)
  out[match(format(closed), out$election), ]
}

#' One run's checks, the run named by a run list line's `arm`, `E`, `F` and `seed`.
#' @return a list: the run's `id`; `cells`, its row of the main table, in the order of HEADINGS;
#'   `ppc`, its rows of the past-election table; and `projected`, the projected minutes
run_checks <- function(arm, E, F, seed, truth) {
  id <- run_id(arm, E, F, as.numeric(seed))
  dir <- run_dir(id)
  meta_path <- file.path(dir, "meta.json")
  if (!file.exists(meta_path)) fail("no meta.json for run ", id, ": the run has not finished")
  meta <- jsonlite::read_json(meta_path, simplifyVector = TRUE)
  meta_E <- day(meta$E, "E")
  meta_F <- day(meta$F, "F")
  if (!identical(meta$arm, arm) || meta_E != as.Date(E) || meta_F != as.Date(F) ||
    !isTRUE(number(meta$seed, "seed") == as.numeric(seed))) {
    fail("the meta.json in directory ", id, " is not of the run list's run ", id)
  }
  tag <- tryCatch(target_cell(meta_E, meta_F)$tag, error = function(e) fail("run ", id, ": ", conditionMessage(e)))
  minutes <- number(meta$minutes, "minutes")
  if (!is.finite(minutes) || minutes < 0) fail("meta.json: minutes is not a finite number at least 0")
  chains <- count_of(meta$chains, "chains")
  warmup <- count_of(meta$iter_warmup, "iter_warmup")
  iters <- count_of(meta$iter_sampling, "iter_sampling")
  parties <- labels_of(meta$party_order, "party_order")
  houses <- labels_of(meta$house_levels, "house_levels")
  labels_of(meta$kjordaemi_levels, "kjordaemi_levels")
  closed <- tryCatch(as.Date(labels_of(meta$closed_elections, "closed_elections")), error = function(e) as.Date(NA))
  if (anyNA(closed)) fail("meta.json: closed_elections are not dates")
  P <- length(parties)
  n <- chains * iters

  name <- "param_draws.parquet"
  path <- file.path(dir, name)
  if (!file.exists(path)) fail("no ", name, " for run ", id)
  pd <- arrow::read_parquet(path)
  sigma_cols <- sprintf("sigma[%d]", seq_len(P))
  L_cols <- sprintf("L_Omega[%d,%d]", rep(seq_len(P), times = P), rep(seq_len(P), each = P))
  phi_cols <- sprintf("log_phi[%d]", seq_along(houses))
  zeta <- "s_bar_zeta[1]"
  need(pd, name, c(sigma_cols, L_cols, phi_cols, "log_phi_K", "log_kappa", zeta), id)
  only(pd, name, "sigma", sigma_cols, id)
  only(pd, name, "L_Omega", L_cols, id)
  only(pd, name, "log_phi", phi_cols, id)
  pd <- pd[draw_order(pd, name, chains, iters, id), ]

  corr <- suppressWarnings(stats::cor(pd$log_phi_K, pd[[zeta]]))
  near <- function(cols, cap) colSums(as.matrix(pd[cols]) > V2_LOG_CAPS[[cap]] - NEAR)
  phi <- near(phi_cols, "log_phi")
  h <- which.max(phi)
  phi_cell <- draws_shown(phi[h], n)
  if (phi[h] > 0) phi_cell <- sprintf("%s in %s (%s)", phi_cell, phi_cols[h], houses[h])
  flat <- flat_ess(as.matrix(pd[sigma_cols]), as.matrix(pd[L_cols]), chains, iters)
  projected <- minutes * SPEC_ITERATIONS / (warmup + iters)

  name <- "ppc_results.parquet"
  path <- file.path(dir, name)
  if (!file.exists(path)) fail("no ", name, " for run ", id)
  counts <- ppc_counts(arrow::read_parquet(path), meta, closed, n, truth, id)

  cell <- sprintf("%s, %s, %s", format(meta_E), format(meta_F), tag)
  seed_text <- sprintf("%.0f", number(meta$seed, "seed"))
  cells <- c(
    arm, cell, seed_text, sprintf("%.0f + %.0f", warmup, iters), sprintf("%.1f", minutes), sprintf("%.1f", projected),
    if (projected > FLAG_MINUTES) "yes" else "no",
    if (is.finite(corr)) sprintf("%.3f", corr) else "NA",
    phi_cell, draws_shown(near("log_phi_K", "log_phi_K"), n), draws_shown(near("log_kappa", "log_kappa"), n),
    ess_shown(flat[["sigma"]]), ess_shown(flat[["omega"]])
  )
  ppc <- lapply(seq_len(nrow(counts)), \(i) {
    c(arm, cell, seed_text, counts$election[i], sprintf("%d", counts$outside[i]), sprintf("%d", counts$cells[i]))
  })
  list(id = id, cells = cells, ppc = ppc, projected = projected)
}

#' The cap of each bounded log concentration, as a heading prints it.
cap_text <- function(name) {
  cap <- V2_LOG_CAPS[[name]]
  sprintf("> %s (cap %s \u2212 %s)", format(cap - NEAR), format(cap), format(NEAR))
}

HEADINGS <- c(
  "Arm", "Cell (E, F, tag)", "Seed", "Iterations (warmup + sampling)", "Minutes", "Minutes projected to 1000 + 2000",
  "Over 120 projected minutes (spec \u00a76.9)", "Corr(log \u03c6_K, s\u0304_\u03b6)",
  sprintf("Draws with log \u03c6_h %s, max over houses", cap_text("log_phi")),
  paste("Draws with log \u03c6_K", cap_text("log_phi_K")), paste("Draws with log \u03ba", cap_text("log_kappa")),
  "Min bulk ESS over the entries of C\u03a3C\u1d40", "Min bulk ESS over the off-diagonals of \u03a9 = L_Omega L_Omega\u1d40"
)
PPC_HEADINGS <- c("Arm", "Cell (E, F, tag)", "Seed", "Closed election", "Cells with PIT outside [0.025, 0.975]", "Cells")

table_lines <- function(headings, rows) {
  line <- function(cells) paste0("| ", paste(gsub("|", "\\|", cells, fixed = TRUE), collapse = " | "), " |")
  c(line(headings), line(rep("---", length(headings))), vapply(rows, line, character(1)))
}

main <- function(args) {
  if (!length(args) %in% 1:2) fail(USAGE)
  run_list <- args[1]
  out <- if (length(args) == 2) args[2] else harness_path("tier0", "v2_checks.md")
  fields <- read_run_list(run_list)
  truth <- read_truth()
  runs <- lapply(fields, \(f) run_checks(f[2], f[3], f[4], f[5], truth))

  report <- c(
    "# V2 Tier 0 checks",
    "",
    paste(
      "Written by `R/harness/v2_checks.R` from each run's `meta.json`, `param_draws.parquet` and",
      "`ppc_results.parquet`, and the closed elections' official counts in",
      "`harness/truth/elections_official.csv`: the extras of spec \u00a76.9 beside the Tier 0 report",
      "(`harness/tier0/<arm>.md`). No forecast quantity is read or printed."
    ),
    "",
    sprintf("- Run list: `%s`", run_list),
    "",
    "## Parameters and compute",
    "",
    table_lines(HEADINGS, lapply(runs, \(r) r$cells)),
    "",
    "## Past elections: in-sample PIT under `y_rep_res`",
    "",
    table_lines(PPC_HEADINGS, unlist(lapply(runs, \(r) r$ppc), recursive = FALSE)),
    "",
    "The columns:",
    "",
    paste(
      "- **Minutes projected to 1000 + 2000** is `meta.json`'s `minutes` \u00d7 3000 \u00f7 (`iter_warmup` +",
      "`iter_sampling`): the fit's wall time scaled linearly to spec \u00a76.9's 4 \u00d7 (1000 + 2000).",
      "**Over 120 projected minutes** is `yes` above 120: spec \u00a76.9 asks for a report before Tier 1",
      "when a V2 fit takes over 2 h, and iterations are never cut for one arm alone."
    ),
    paste(
      "- **Corr(log \u03c6_K, s\u0304_\u03b6)** is the Pearson correlation of `log_phi_K` and `s_bar_zeta[1]` over",
      "all draws, the chains pooled. \u03b4 is close to saturated per cell, so the cell-level",
      "concentration and the \u03b4 walk's scale may trade off (spec \u00a76.6)."
    ),
    paste(
      "- **Draws with log c > cap \u2212 2** count the chains \u00d7 sampling draws in which a bounded log",
      "concentration lies within 2 of its cap, on the log scale the model uses; the caps are",
      "`V2_LOG_CAPS` (`R/harness/v2_reference.R`). log \u03c6_h has one column per house: the cell gives",
      "the largest count over the houses, and the house that has it (`meta.json`'s `house_levels`)",
      "when that count is not 0."
    ),
    paste(
      "- **Min bulk ESS over the entries of C\u03a3C\u1d40**: per draw, \u03a3 = diag(\u03c3) \u03a9 diag(\u03c3) with",
      "\u03a9 = `L_Omega` `L_Omega`\u1d40 and C = I \u2212 11\u1d40/P over the run's P parties, the walk's",
      "identified innovation covariance (spec \u00a76.2); then `posterior::ess_bulk()` over the chains",
      "of each of its P(P + 1)/2 distinct entries, and the min. **Min bulk ESS over the off-diagonals",
      "of \u03a9** is the same over \u03a9's P(P \u2212 1)/2 off-diagonal entries. Only C\u03a3C\u1d40 is identified, so a",
      "low ESS in \u03a9 alone is a flat direction of the likelihood. Both are rounded down."
    ),
    paste(
      "- **Cells with PIT outside [0.025, 0.975]**: for each kj\u00f6rd\u00e6mi \u00d7 party cell of a closed",
      "election, the mid-PIT of the official count y under the in-sample replicates `y_rep_res`",
      "(`ppc_results.parquet`), P(y_rep < y) + \u00bd P(y_rep = y) over the draws, y matched by election,",
      "kj\u00f6rd\u00e6mi and party name (`harness/truth/elections_official.csv`, folded to the poll",
      "parties). A PIT of exactly 0.025 or 0.975 is inside. **Cells** counts the cells of the parties",
      "with an official count at that election; a party without a list there, whose replicates are",
      "0, is left out. Counts only: no replicate or official count is printed."
    )
  )
  for (r in runs) {
    if (r$projected > FLAG_MINUTES) {
      cat(sprintf(
        "FLAG %s: projected to %.1f minutes at 1000 + 2000, over %s (spec \u00a76.9: report before Tier 1)\n",
        r$id, r$projected, format(FLAG_MINUTES)
      ))
    }
  }
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeLines(report, out)
  cat(sprintf("wrote %s (%d runs)\n", out, length(runs)))
}

tryCatch(main(commandArgs(trailingOnly = TRUE)), error = function(e) {
  message <- conditionMessage(e)
  stop(if (startsWith(message, PREFIX)) "" else PREFIX, message, call. = FALSE)
})
