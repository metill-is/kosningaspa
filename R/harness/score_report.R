# The lean scoring report (harness/README.md, Checking a model change): two arms' runs on the
# same cells, each scored against the result by score_cell(), and the spec's adoption rule
# (section 5.7) read on each cell. A plain script; loaded as a module (as its test does), the
# script part does not run. Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/score_report.R <ref_arm> <new_arm> <run_list> [out]
#
# The run list is the queue's (harness/runs/; "lane arm E F seed" per line, # for comments).
# Every cell that holds a run of either arm must hold exactly one run of each, and each run a
# meta.json; lines of other arms are ignored. out defaults to harness/v2/scoring.md.
#
# The rule is written for two decision cells (eve and 14 days) and two seeds per election.
# This report reads it on each cell alone, with one run per arm, so SE_diff is the two runs'
# batch-means SEs alone, sqrt(SE_ref^2 + SE_new^2), with no seed-to-seed spread, and a cell
# that is not an eve reports the calibration gate without gating on it. For each cell:
#   - a table of the scores that score_cell() returns with an SE, and the rest it reports: the
#     scored national reading (kd_aggregate), the kjordaemi and the seat scores, each with both
#     arms' values, new - ref, SE_diff and the tie rule's reading (read_diff());
#   - the four criteria (adoption_criteria()): the calibration gate (new's eve d2 has
#     p > GATE_P against chi-squared with P - 1 degrees of freedom), no loss of accuracy
#     (CRPS10 at most MARGINS["crps10"] pp worse, or a tie), a better distribution (mDSS lower
#     beyond a tie, or ref failing the gate) and no regional loss (kjordaemi CRPS at most
#     MARGINS["kd_crps"] pp worse and seat CRPS per party at most MARGINS["seat_crps_pp"]
#     worse, each or a tie);
#   - each party's result, both arms' medians, absolute errors and CRPS, and the number of
#     parties on which new's median is closer and its CRPS lower.
# Above the cells, a table of the MAEs (mae_summary()): MAE8 with its SE and the tie rule's
# reading, the MAE over all parties, the kjordaemi MAE and the summed seat error.
# It reads each run's meta.json for its iterations, seed and gate, and stops, writing nothing,
# on a run list with no run of an arm, a cell missing one arm's run or holding two, a run with
# no meta.json, and on whatever score_cell() refuses (a 2017 cell, a dev or check arm).
if (is.null(box::name())) {
  if (!isTRUE(l10n_info()[["UTF-8"]])) stop("score_report: needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
  # WHY: the box path below is the working directory, so check it is the project root first.
  if (!file.exists("R/harness/constants.R")) stop("score_report: run from the project root (no R/harness/constants.R here)", call. = FALSE)
  options(box.path = normalizePath("."), width = 120, warn = 1)
}
box::use(
  R / harness / runs[run_id, run_dir],
  R / harness / scorer[score_cell],
  R / harness / compare_runs[relative]
)

#' The margins of the adoption rule's criteria 2 and 4 (spec section 5.7): CRPS10 in pp,
#' kjordaemi CRPS in pp, seat CRPS in seats per party.
#' @export
MARGINS <- c(crps10 = 0.05, kd_crps = 0.10, seat_crps_pp = 0.05)

#' The calibration gate's level: new's eve d2 must have a p-value above it.
#' @export
GATE_P <- 0.01

DEFAULT_OUT <- "harness/v2/scoring.md"
PREFIX <- "score_report: "
fail <- function(...) stop(PREFIX, ..., call. = FALSE)

#' The tie rule (spec section 5.7) on one score where lower is better.
#' @param ref,new the two arms' values
#' @param se_ref,se_new their batch-means SEs
#' @return a list: `diff`, new - ref; `se_diff`, sqrt(se_ref^2 + se_new^2); `reading`,
#'   `"tie"` when |diff| < 2 se_diff or diff is 0 (two runs with no Monte Carlo error that agree), else `"better"` (diff < 0) or `"worse"`, and NA when
#'   any input is NA
#' @export
read_diff <- function(ref, new, se_ref, se_new) {
  diff <- new - ref
  se_diff <- sqrt(se_ref^2 + se_new^2)
  reading <- if (anyNA(c(diff, se_diff))) {
    NA_character_
  } else if (diff == 0 || abs(diff) < 2 * se_diff) {
    "tie"
  } else if (diff < 0) {
    "better"
  } else {
    "worse"
  }
  list(diff = diff, se_diff = se_diff, reading = reading)
}

#' The p-value of a Mahalanobis d2 against chi-squared with n_parties - 1 degrees of freedom.
#' @export
d2_p <- function(d2, n_parties) stats::pchisq(d2, n_parties - 1, lower.tail = FALSE)

#' One run's scores as the criteria read them, from score_cell()'s value.
#' @param s score_cell()'s value
#' @return a list: the national `kd_aggregate` row's `crps10`, `mdss`, `d2`, `n_parties` and
#'   their SEs, the kjordaemi CRPS and its SE, and seat CRPS per party (the per-party sum
#'   over the parties of `seats_per_party`) and its SE
#' @export
cell_scores <- function(s) {
  nat <- s$national[s$national$reading == "kd_aggregate", ]
  if (nrow(nat) != 1) fail("score_cell() returned ", nrow(nat), " kd_aggregate rows, not 1")
  p_seats <- nrow(s$seats_per_party)
  list(
    crps10 = nat$crps10, se_crps10 = nat$se_crps10, mdss = nat$mdss, se_mdss = nat$se_mdss,
    d2 = nat$d2, se_d2 = nat$se_d2, n_parties = nat$n_parties,
    kd_crps = s$kjordaemi$kd_crps, se_kd_crps = s$kjordaemi$se_kd_crps,
    seat_crps_pp = s$seats$seat_crps / p_seats, se_seat_crps_pp = s$seats$se_seat_crps / p_seats
  )
}

#' The adoption rule's four criteria on one cell.
#' @param ref,new `cell_scores()` of the two runs
#' @return a named logical vector: `gate` (new's d2 has p > GATE_P), `accuracy` (CRPS10 at
#'   most `MARGINS["crps10"]` worse, or a tie), `distribution` (mDSS lower beyond a tie, or
#'   ref's d2 has p <= GATE_P) and `regional` (kjordaemi CRPS and seat CRPS per party each at
#'   most their margin worse, or a tie)
#' @export
adoption_criteria <- function(ref, new) {
  within <- function(d, margin) identical(d$reading, "tie") || isTRUE(d$diff <= margin)
  crps <- read_diff(ref$crps10, new$crps10, ref$se_crps10, new$se_crps10)
  dss <- read_diff(ref$mdss, new$mdss, ref$se_mdss, new$se_mdss)
  kd <- read_diff(ref$kd_crps, new$kd_crps, ref$se_kd_crps, new$se_kd_crps)
  seat <- read_diff(ref$seat_crps_pp, new$seat_crps_pp, ref$se_seat_crps_pp, new$se_seat_crps_pp)
  c(
    gate = isTRUE(d2_p(new$d2, new$n_parties) > GATE_P),
    accuracy = within(crps, MARGINS[["crps10"]]),
    distribution = identical(dss$reading, "better") || !isTRUE(d2_p(ref$d2, ref$n_parties) > GATE_P),
    regional = within(kd, MARGINS[["kd_crps"]]) && within(seat, MARGINS[["seat_crps_pp"]])
  )
}

#' The MAEs of two runs on one cell, the headline the report opens with.
#' @param s_ref,s_new score_cell()'s values for the two runs
#' @return a list: `mae8` (100 x the mean |median - result| over the eight named parties,
#'   `NAMED8`) of each run with its MCSE and `read_diff()` of the two; `mae_all` (the same
#'   over every party) of each; `kd_mae` (over the named parties' kj\u00f6rd\u00e6mi cells) of each;
#'   `seat_err` (the sum over parties of |median seats - seats won|) of each. Only `mae8`
#'   has a Monte Carlo SE.
#' @export
mae_summary <- function(s_ref, s_new) {
  nat <- function(s) s$national[s$national$reading == "kd_aggregate", ]
  a <- nat(s_ref)
  b <- nat(s_new)
  list(
    mae8 = c(ref = a$mae8, new = b$mae8), se_mae8 = c(ref = a$se_mae8, new = b$se_mae8),
    mae8_diff = read_diff(a$mae8, b$mae8, a$se_mae8, b$se_mae8),
    mae_all = c(ref = a$mae_all, new = b$mae_all),
    kd_mae = c(ref = s_ref$kjordaemi$kd_mae_named, new = s_new$kjordaemi$kd_mae_named),
    seat_err = c(ref = s_ref$seats$seat_abs_err, new = s_new$seats$seat_abs_err)
  )
}

read_run_list <- function(path) {
  if (!file.exists(path)) fail("no run list ", relative(path))
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  fields <- strsplit(lines, "[[:space:]]+")
  bad <- lengths(fields) != 5
  if (any(bad)) fail(relative(path), ": a line is not \"lane arm E F seed\": ", lines[bad][1])
  tibble::tibble(
    arm = vapply(fields, `[`, "", 2), E = vapply(fields, `[`, "", 3), F = vapply(fields, `[`, "", 4),
    seed = as.integer(vapply(fields, `[`, "", 5))
  )
}

num <- function(x, digits) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
yes <- function(ok) if (isTRUE(ok)) "yes" else "no"
row <- function(...) paste0("| ", paste(c(...), collapse = " | "), " |")

#' The markdown section of one cell.
cell_section <- function(cell, ref, new, s_ref, s_new) {
  a <- cell_scores(s_ref)
  b <- cell_scores(s_new)
  nat_a <- s_ref$national[s_ref$national$reading == "kd_aggregate", ]
  nat_b <- s_new$national[s_new$national$reading == "kd_aggregate", ]
  scored <- function(label, x, y, se_x, se_y, digits) {
    d <- read_diff(x, y, se_x, se_y)
    row(
      label, paste0(num(x, digits), " (", num(se_x, digits), ")"), paste0(num(y, digits), " (", num(se_y, digits), ")"),
      num(d$diff, digits), num(d$se_diff, digits), d$reading
    )
  }
  reported <- function(label, x, y, digits) row(label, num(x, digits), num(y, digits), num(y - x, digits), "", "reported")
  count <- function(label, x, y, of_x, of_y, se_x, se_y) {
    row(
      label, paste0(x, " of ", of_x, " (", num(se_x, 2), ")"), paste0(y, " of ", of_y, " (", num(se_y, 2), ")"),
      num(y - x, 0), num(sqrt(se_x^2 + se_y^2), 2), "reported"
    )
  }
  p_a <- d2_p(a$d2, a$n_parties)
  p_b <- d2_p(b$d2, b$n_parties)
  table <- c(
    row("Score (lower is better unless marked)", paste0(ref, " (SE)"), paste0(new, " (SE)"), paste0(new, " @MINUS@ ", ref), "SE_diff", "Reading"),
    row(rep("---", 6)),
    scored("CRPS10, pp (co-primary)", a$crps10, b$crps10, a$se_crps10, b$se_crps10, 3),
    scored("mDSS (co-primary)", a$mdss, b$mdss, a$se_mdss, b$se_mdss, 2),
    scored(sprintf("d@SUP2@ (p: %s, %s)", num(p_a, 3), num(p_b, 3)), a$d2, b$d2, a$se_d2, b$se_d2, 2),
    scored("MAE8, pp (the eight named parties)", nat_a$mae8, nat_b$mae8, nat_a$se_mae8, nat_b$se_mae8, 3),
    reported("MAE over all parties, pp", nat_a$mae_all, nat_b$mae_all, 3),
    scored("IS90", nat_a$is90, nat_b$is90, nat_a$se_is90, nat_b$se_is90, 2),
    count("National 50% coverage (higher is better)", nat_a$cov50, nat_b$cov50, nat_a$n_parties, nat_b$n_parties, nat_a$se_cov50, nat_b$se_cov50),
    count("National 90% coverage (higher is better)", nat_a$cov90, nat_b$cov90, nat_a$n_parties, nat_b$n_parties, nat_a$se_cov90, nat_b$se_cov90),
    scored("Kj\u00f6rd\u00e6mi CRPS, pp", a$kd_crps, b$kd_crps, a$se_kd_crps, b$se_kd_crps, 3),
    count(
      "Kj\u00f6rd\u00e6mi 90% coverage (higher is better)", s_ref$kjordaemi$kd_cov90, s_new$kjordaemi$kd_cov90,
      s_ref$kjordaemi$kd_cells, s_new$kjordaemi$kd_cells, s_ref$kjordaemi$se_kd_cov90, s_new$kjordaemi$se_kd_cov90
    ),
    reported("Kj\u00f6rd\u00e6mi MAE over the named cells, pp", s_ref$kjordaemi$kd_mae_named, s_new$kjordaemi$kd_mae_named, 3),
    scored("Seat CRPS per party", a$seat_crps_pp, b$seat_crps_pp, a$se_seat_crps_pp, b$se_seat_crps_pp, 3),
    reported("Seat absolute error, summed over parties", s_ref$seats$seat_abs_err, s_new$seats$seat_abs_err, 0),
    count(
      "Seat 90% coverage (higher is better)", s_ref$seats$seat_cov90, s_new$seats$seat_cov90,
      nrow(s_ref$seats_per_party), nrow(s_new$seats_per_party), s_ref$seats$se_seat_cov90, s_new$seats$se_seat_cov90
    ),
    scored("Brier score of the incumbents' majority", s_ref$seats$brier, s_new$seats$brier, s_ref$seats$se_brier, s_new$seats$se_brier, 3),
    reported("P(majority for the coalition that formed), report only (higher is better)", s_ref$seats$p_formed, s_new$seats$p_formed, 3)
  )
  crit <- adoption_criteria(a, b)
  eve <- identical(cell$tag, "eve")
  d <- function(x, y, sx, sy) read_diff(x, y, sx, sy)
  crps <- d(a$crps10, b$crps10, a$se_crps10, b$se_crps10)
  dss <- d(a$mdss, b$mdss, a$se_mdss, b$se_mdss)
  kd <- d(a$kd_crps, b$kd_crps, a$se_kd_crps, b$se_kd_crps)
  seat <- d(a$seat_crps_pp, b$seat_crps_pp, a$se_seat_crps_pp, b$se_seat_crps_pp)
  criteria <- c(
    sprintf(
      "1. Calibration gate: %s's d@SUP2@ has p = %s against @CHI@@SUP2@ with %d degrees of freedom, above %s: %s%s (%s's p = %s).",
      new, num(p_b, 3), b$n_parties - 1L, format(GATE_P), yes(crit[["gate"]]),
      if (eve) "" else ", reported only: the gate is an eve criterion", ref, num(p_a, 3)
    ),
    sprintf(
      "2. No loss of accuracy: CRPS10 %s @MINUS@ %s = %s pp (SE_diff %s, %s), at most %s pp worse or a tie: %s.",
      new, ref, num(crps$diff, 3), num(crps$se_diff, 3), crps$reading, format(MARGINS[["crps10"]]), yes(crit[["accuracy"]])
    ),
    sprintf(
      "3. Better distribution: mDSS %s @MINUS@ %s = %s (SE_diff %s, %s), lower beyond a tie or %s failing the gate: %s.",
      new, ref, num(dss$diff, 2), num(dss$se_diff, 2), dss$reading, ref, yes(crit[["distribution"]])
    ),
    sprintf(
      "4. No regional loss: kj\u00f6rd\u00e6mi CRPS %s pp (%s), at most %s worse or a tie; seat CRPS per party %s (%s), at most %s worse or a tie: %s.",
      num(kd$diff, 3), kd$reading, format(MARGINS[["kd_crps"]]), num(seat$diff, 3), seat$reading,
      format(MARGINS[["seat_crps_pp"]]), yes(crit[["regional"]])
    )
  )
  cols <- c("flokkur", "actual", "median", "abs_err", "crps")
  pp <- merge(s_ref$per_party[, cols], s_new$per_party[, cols], by = "flokkur", all = TRUE, suffixes = c("_ref", "_new"), sort = FALSE)
  pp$actual <- ifelse(is.na(pp$actual_ref), pp$actual_new, pp$actual_ref)
  pp <- pp[order(-pp$actual), ]
  both <- !is.na(pp$crps_ref) & !is.na(pp$crps_new)
  pct <- function(x) num(100 * x, 2)
  parties <- c(
    row(
      "Party", "Result, %", paste0(ref, ": median, %"), paste0(new, ": median, %"), paste0(ref, ": absolute error, pp"),
      paste0(new, ": absolute error, pp"), paste0(ref, ": CRPS, pp"), paste0(new, ": CRPS, pp")
    ),
    row(rep("---", 8)),
    vapply(seq_len(nrow(pp)), \(i) row(
      pp$flokkur[i], pct(pp$actual[i]), pct(pp$median_ref[i]), pct(pp$median_new[i]), pct(pp$abs_err_ref[i]),
      pct(pp$abs_err_new[i]), num(100 * pp$crps_ref[i], 3), num(100 * pp$crps_new[i], 3)
    ), ""),
    "",
    sprintf(
      "Of the %d parties both arms forecast, %s's median is closer to the result on %d and its CRPS lower on %d (no MC error per party; consistency evidence only).%s",
      sum(both), new, sum(pp$abs_err_new[both] < pp$abs_err_ref[both]), sum(pp$crps_new[both] < pp$crps_ref[both]),
      if (all(both)) "" else paste0(" Forecast by one arm only: ", paste(pp$flokkur[!both], collapse = ", "), ".")
    )
  )
  c(
    sprintf("## %s, %s (E %s, F %s)", format(cell$E, "%Y"), cell$tag, cell$E, cell$F), "",
    table, "",
    sprintf(
      "National parties: %d (%s), %d (%s). The gap between each run's two national readings (the kj\u00f6rd\u00e6mi aggregate scored here and the model's own) is at most %s pp (%s) and %s pp (%s).",
      a$n_parties, ref, b$n_parties, new, num(s_ref$gap_pp, 3), ref, num(s_new$gap_pp, 3), new
    ), "",
    criteria, "",
    sprintf("All four on this cell: %s.", yes(all(crit[if (eve) names(crit) else setdiff(names(crit), "gate")]))),
    if (eve) character(0) else "(Criterion 1 left out: the cell is not an eve.)",
    "",
    parties, ""
  )
}

main <- function(args) {
  if (!length(args) %in% 3:4) fail("usage: Rscript R/harness/score_report.R <ref_arm> <new_arm> <run_list> [out]")
  ref <- args[1]
  new <- args[2]
  path <- args[3]
  out <- if (length(args) == 4) args[4] else DEFAULT_OUT
  if (identical(ref, new)) fail("compare two different arms, not ", ref, " with itself")
  runs <- read_run_list(path)
  for (a in c(ref, new)) if (!any(runs$arm == a)) fail(relative(path), " has no run of ", a)
  runs <- runs[runs$arm %in% c(ref, new), ]
  cells <- unique(runs[, c("E", "F")])
  sections <- character(0)
  run_rows <- character(0)
  mae_rows <- character(0)
  for (i in seq_len(nrow(cells))) {
    on <- runs[runs$E == cells$E[i] & runs$F == cells$F[i], ]
    for (a in c(ref, new)) {
      k <- sum(on$arm == a)
      if (k != 1) fail(cells$E[i], ", ", cells$F[i], ": ", k, " runs of ", a, ", not 1")
    }
    ids <- vapply(c(ref, new), \(a) run_id(a, cells$E[i], cells$F[i], on$seed[on$arm == a]), "")
    for (id in ids) {
      if (!file.exists(file.path(run_dir(id), "meta.json"))) fail("no meta.json in ", relative(run_dir(id)))
      m <- jsonlite::read_json(file.path(run_dir(id), "meta.json"))
      run_rows <- c(run_rows, row(
        paste(cells$E[i], cells$F[i], sep = ", "), m$arm, paste0("`", id, "`"),
        paste(m$iter_warmup, "+", m$iter_sampling), m$seed, m$git_head %||% "NA",
        if (isTRUE(m$gate$pass)) "pass" else paste("FAIL:", paste(unlist(m$gate$reasons), collapse = "; "))
      ))
    }
    s_ref <- score_cell(run_dir(ids[[1]]))
    s_new <- score_cell(run_dir(ids[[2]]))
    sections <- c(sections, cell_section(s_new$cell, ref, new, s_ref, s_new))
    m <- mae_summary(s_ref, s_new)
    mae_rows <- c(mae_rows, row(
      paste(format(s_new$cell$E, "%Y"), s_new$cell$tag),
      paste0(num(m$mae8[["ref"]], 2), " (", num(m$se_mae8[["ref"]], 2), ")"),
      paste0(num(m$mae8[["new"]], 2), " (", num(m$se_mae8[["new"]], 2), ")"),
      num(m$mae8_diff$diff, 2), m$mae8_diff$reading, num(m$mae_all[["ref"]], 2), num(m$mae_all[["new"]], 2),
      num(m$kd_mae[["ref"]], 2), num(m$kd_mae[["new"]], 2), num(m$seat_err[["ref"]], 0), num(m$seat_err[["new"]], 0)
    ))
  }
  text <- c(
    sprintf("# Scores: %s against %s", new, ref), "",
    "Written by `R/harness/score_report.R` from `score_cell()` on each run, against the official results.",
    "The lean reading (`harness/README.md`, Checking a model change): one run per arm per cell. SE_diff is",
    "the two runs' batch-means SEs alone, with no seed-to-seed spread, and the spec's adoption rule (section",
    "5.7), written for two decision cells and two seeds per election, is read on each cell alone. A",
    "difference under 2 @TIMES@ SE_diff is a tie, and ties satisfy criteria 2 and 4 but are not \"lower\".", "",
    sprintf("- Run list: `%s`", relative(path)),
    sprintf(
      "- Margins: CRPS10 %s pp, kj\u00f6rd\u00e6mi CRPS %s pp, seat CRPS %s seats per party; calibration gate p > %s",
      format(MARGINS[["crps10"]]), format(MARGINS[["kd_crps"]]), format(MARGINS[["seat_crps_pp"]]), format(GATE_P)
    ), "",
    row("Cell (E, F)", "Arm", "Run", "Iterations (warmup + sampling)", "Seed", "Commit", "Gate (section 5.7)"),
    row(rep("---", 7)),
    run_rows, "",
    "## MAE",
    "",
    "The mean absolute error of each model's median vote share against the result, in points. MAE8 is over the",
    "eight named parties (Anna@ETH@ and S@OACUTE@s@IACUTE@alistaflokkurinn left out, as in the project's earlier backtests), with",
    "the Monte Carlo SE of the medians; MAE over all parties includes them. Kj\u00f6rd\u00e6mi MAE is over the named",
    "parties' kj\u00f6rd\u00e6mi cells; seat error is the sum over parties of |median seats @MINUS@ seats won|. MAE is",
    "reported, not part of the adoption rule (spec section 5.6).",
    "",
    row(
      "Cell", paste0(ref, ": MAE8, pp (SE)"), paste0(new, ": MAE8, pp (SE)"), paste0(new, " @MINUS@ ", ref), "Reading",
      paste0(ref, ": MAE, all parties"), paste0(new, ": MAE, all parties"), paste0(ref, ": kj\u00f6rd\u00e6mi MAE"),
      paste0(new, ": kj\u00f6rd\u00e6mi MAE"), paste0(ref, ": seat error"), paste0(new, ": seat error")
    ),
    row(rep("---", 11)),
    mae_rows, "",
    sections
  )
  text <- gsub("@MINUS@", "\u2212", text, fixed = TRUE)
  text <- gsub("@TIMES@", "\u00d7", text, fixed = TRUE)
  text <- gsub("@SUP2@", "\u00b2", text, fixed = TRUE)
  text <- gsub("@ETH@", "\u00f0", text, fixed = TRUE)
  text <- gsub("@OACUTE@", "\u00f3", text, fixed = TRUE)
  text <- gsub("@IACUTE@", "\u00ed", text, fixed = TRUE)
  text <- gsub("@CHI@", "\u03c7", text, fixed = TRUE)
  writeLines(text, out)
  cat("wrote ", relative(out), " (", nrow(cells), " cells)\n", sep = "")
  0L
}

if (is.null(box::name())) quit(save = "no", status = main(commandArgs(trailingOnly = TRUE)))
