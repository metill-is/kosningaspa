# The prior predictive check of arm V2 (spec §6.10; the plan's Task 6): summaries of prior
# draws of V2's model on the designs of two cells, each checked against a band written before
# any V2 fit. A plain script whose definitions also load as a box module, so that its test can
# import ROWS and the band logic; the script part runs only when box::name() is NULL. Run from
# the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/v2_prior_predictive.R <config> [n_draws=1000]
#
# It builds the config's Stan data on the 2024-eve and 2021-f180 cells (v2_stan_data(); stage 1
# is never fitted, since μ_f is a likelihood term's data and no prior draw reads it; no 2017
# cell is ever built), and on each takes n_draws prior points with
# v2_simulate(data, seed = SEED + i, observe = FALSE): parameters only, no counts, no fit and
# no forecast. Each row of ROWS reads one summary off every draw, in pp, and is checked on the
# 5% and 95% quantiles over the draws (pooled over houses or kjördæmi where said below):
#   - a config row runs on the config's data;
#   - a comparison row runs on the same data with its `set` fields changed and nothing else,
#     and the same seeds (common random numbers). A comparison whose values the config
#     already holds is skipped.
#
# The 20% party. Each effect x, a P-vector on the log-ratio scale, moves the composition
# s0 = (0.2, 0.8 / (P − 1), ..., 0.8 / (P − 1)); the summary is the first party's change,
# 100 (softmax(log s0 + x)_1 − 0.2) pp, with every party's element of x in the softmax. So
# the 20% party is coordinate 1, the coordinate party: the first party of the order, present
# at the window's start (checked here) and with votes at every closed election (checked by
# v2_stan_data()). None of its effects is therefore replaced by a newcomer's construction
# (§6.4), and in the default order it is Sjálfstæðisflokkurinn, a party of about that size.
# Every prior read below is exchangeable over parties, so coordinate 1 stands for any party
# present from the start. The effects:
#   - house_d: d_h = σ_γ C ε_h (pars sigma_gamma and house_raw), coordinate 1 of every house,
#     the houses pooled (they are exchangeable).
#   - industry_mu1: μ_1, the parameter mu1 (normal(0, prior_sd_mu1) on every element,
#     sum-to-zero), coordinate 1. The newcomers' elements enter as drawn, before §6.4 replaces
#     them at entry, so the row reads the scale prior_sd_mu1 alone.
#   - poll_sd: no composition. The Dirichlet-multinomial sd of a share s = 0.2 in a poll of
#     n = 1000, sqrt(s (1 − s) / n · (n + φ) / (1 + φ)), at each house's φ_h (v2_natural()'s
#     phi), the houses pooled.
#   - kd_delta: δ_open,k, v2_natural()'s δ of the open epoch (the core's static δ, with
#     proposal 2's hyperprior log s_δ0 = μ_sd + τ_sd r), coordinate 1 of every kjördæmi, the
#     kjördæmi pooled. Σ_k w_k δ_k = 0 makes log s0 the national log-ratio to first order, so
#     the change is the kjördæmi's deviation from the party's national share to that order.
#   - walk_180: x = √180 · C(σ ⊙ L_Ω z), with z ~ N(0, I_P) from seed SEED_AUX + i and the
#     draw's σ and L_Ω: covariance 180 CΣCᵀ, the walk's own innovation covariance (§6.2) over
#     180 days off the campaign (s = 1). The campaign multiplier τ_camp has a prior of its own,
#     which this row leaves out. The change is signed, so its band reads the upper tail.
#
# Verdicts and lines, one per row and cell:
#   PRIOR PASS <summary>               the quantiles are in the band
#   PRIOR FAIL <summary> <q05> <q95>   they are not
#   PRIOR WIDE <summary> <q05> <q95>   they are not, on a config row of a bias whose prior the
#                                      config holds at §6.10's value (the row's `held`)
# <summary> is <summary>/<cell>/<kind>, a comparison's kind followed by its fields. A
# comparison row prints PASS or FAIL and never stops the script. The script writes
# harness/v2/prior_predictive.md, prints the lines, and then stops with an error if a config
# row FAILs. A band catches a broken prior (a wrong scale, unit or sign), not a mistuned one,
# and is no tuning target.
#
# Later steps append their rows to ROWS (keyed by summary and step; a row with a switch runs
# only when the config turns it on), their summaries to SUMMARY_FUNS and any new band type to
# in_band().
if (is.null(box::name())) {
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    stop("v2_prior_predictive.R needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
  }
  # WHY: the box path below is the working directory, so check it is the project root first.
  stopifnot("run from the project root (no R/harness/constants.R here)" = file.exists("R/harness/constants.R"))
  options(box.path = normalizePath("."), width = 120, warn = 1)
}
box::use(
  R / harness / constants[harness_path, project_root, TARGETS],
  R / harness / info_set[info_set, read_inputs],
  R / harness / v2[read_v2_config, stage1_placeholder, v2_stan_data],
  R / harness / v2_reference[v2_simulate]
)

#' The share of the party whose change the composition summaries read, the poll size of
#' the poll-sd row, and the days of the walk row.
SHARE <- 0.2
POLL_N <- 1000
WALK_DAYS <- 180

#' Prior draw i takes seed SEED + i; a summary that needs more randomness takes SEED_AUX + i.
SEED <- 6000L
SEED_AUX <- 7000000L

#' The fewest draws whose 5% and 95% quantiles mean anything.
MIN_DRAWS <- 20L

#' The cells whose designs the draws use. No 2017 cell.
CELLS <- tibble::tibble(
  cell = c("2024-eve", "2021-f180"),
  E = as.Date(c("2024-11-30", "2021-09-25")),
  F = as.Date(c("2024-11-29", "2021-03-29"))
)

#' The 20% party's change in pp when the composition (share, (1 − share) / (P − 1), ...)
#' moves by `x` on the log-ratio scale: 100 (softmax(log s0 + x)_1 − share).
#' @param x a numeric P-vector, P at least 2
#' @param share the first party's share
#' @return numeric(1)
#' @export
share_change <- function(x, share = SHARE) {
  P <- length(x)
  eta <- log(c(share, rep((1 - share) / (P - 1), P - 1))) + x
  e <- exp(eta - max(eta))
  100 * (e[1] / sum(e) - share)
}

#' The Dirichlet-multinomial sd of a share `s` in a poll of `n` at concentration `phi`, in pp:
#' 100 sqrt(s (1 − s) / n · (n + φ) / (1 + φ)).
#' @param phi concentrations, non-negative
#' @return a numeric vector as long as `phi`
#' @export
poll_sd_pp <- function(phi, s = SHARE, n = POLL_N) 100 * sqrt(s * (1 - s) / n * (n + phi) / (1 + phi))

#' The summaries, each a function of a `v2_simulate()` draw `sim`, its `data` and a `seed`
#' for any further randomness, returning the draw's values in pp (one per house or
#' kjördæmi for a pooled summary). The header comment says which coordinate each reads.
#' @export
SUMMARY_FUNS <- list(
  house_d = function(sim, data, seed) {
    eps <- matrix(sim$pars$house_raw, data$H, data$P)
    apply(sim$pars$sigma_gamma * (eps - rowMeans(eps)), 1, share_change)
  },
  industry_mu1 = function(sim, data, seed) share_change(sim$pars$mu1),
  poll_sd = function(sim, data, seed) poll_sd_pp(sim$truth$phi),
  kd_delta = function(sim, data, seed) {
    apply(matrix(sim$truth$delta[data$n_epoch, , ], data$K, data$P), 1, share_change)
  },
  walk_180 = function(sim, data, seed) {
    z <- withr::with_seed(seed, stats::rnorm(data$P))
    v <- sim$pars$sigma * drop(matrix(sim$pars$L_Omega, data$P, data$P) %*% z)
    share_change(sqrt(WALK_DAYS) * (v - mean(v)))
  }
)

#' The rows of one summary: its config row, then one comparison row per entry of `compare`.
#' @param summary its name in SUMMARY_FUNS
#' @param step the step that adds it
#' @param switch NA, or the data switch its component needs
#' @param label what it is, for the report
#' @param keys the data's prior fields its prior reads, for the report
#' @param band "interval" (the 5% quantile at least `lo`, the 95% at most `hi`) or "q95" (the
#'   95% quantile in [lo, hi])
#' @param held NULL, or for a bias row its prior field at §6.10's value, a named number: a
#'   config row whose data hold it prints WIDE out of its band
#' @param compare a list of comparison rows, each `list(set, source)`: the data fields it
#'   changes (a named list) and where their values come from
#' @return a tibble: summary, step, switch, label, keys, kind, set, source, held, band, lo, hi
summary_rows <- function(summary, step, switch, label, keys, band, lo, hi, held = NULL, compare = list()) {
  tibble::tibble(
    summary = summary, step = step, switch = switch, label = label, keys = list(keys),
    kind = c("config", rep("comparison", length(compare))),
    set = c(list(NULL), lapply(compare, `[[`, "set")),
    source = c("config", vapply(compare, `[[`, "", "source")),
    held = c(list(held), rep(list(NULL), length(compare))),
    band = band, lo = lo, hi = hi
  )
}

#' Every row of the check, keyed by summary and step. The core's (step v2s-m0): the bands
#' were written before any V2 fit; the first three rows each have the other values of the
#' plan's Decision 1 as comparisons (§6.10's, and proposals 1 and 4).
#' @export
ROWS <- dplyr::bind_rows(
  summary_rows(
    "house_d", "v2s-m0", NA_character_,
    "A house's deviation d from the industry, on a 20% party (d_h = \u03c3_\u03b3 C \u03b5_h, \u03c3_\u03b3 ~ exponential(`prior_rate_sigma_gamma`))",
    "prior_rate_sigma_gamma", "interval", -10, 10,
    held = c(prior_rate_sigma_gamma = 1),
    compare = list(
      list(set = list(prior_rate_sigma_gamma = 1), source = "\u00a76.10"),
      list(set = list(prior_rate_sigma_gamma = 5), source = "proposal 4")
    )
  ),
  summary_rows(
    "industry_mu1", "v2s-m0", NA_character_,
    "Industry bias \u03bc_1 on a 20% party (\u03bc_1 sum-to-zero, normal(0, `prior_sd_mu1`) on every element)",
    "prior_sd_mu1", "interval", -12, 12,
    held = c(prior_sd_mu1 = 1),
    compare = list(
      list(set = list(prior_sd_mu1 = 1), source = "\u00a76.10"),
      list(set = list(prior_sd_mu1 = 0.3), source = "proposal 4")
    )
  ),
  summary_rows(
    "poll_sd", "v2s-m0", NA_character_,
    "A poll's sd around its mean, n = 1,000, 20% party, at the prior \u03c6_h (\u03bc_\u03c6 ~ N(`prior_mu_phi`, 1))",
    "prior_mu_phi", "interval", 0.8, 6,
    compare = list(
      list(set = list(prior_mu_phi = 0), source = "\u00a76.10"),
      list(set = list(prior_mu_phi = log(1000)), source = "proposal 1")
    )
  ),
  summary_rows(
    "kd_delta", "v2s-m0", NA_character_,
    "A party's kj\u00f6rd\u00e6mi deviation from its national share, 20% party (static \u03b4, log s_\u03b40 = \u03bc_sd + \u03c4_sd r)",
    character(0), "interval", -25, 25
  ),
  summary_rows(
    "walk_180", "v2s-m0", NA_character_,
    "A 20% party's 180-day walk change, off the campaign (covariance 180 C\u03a3C\u1d40)",
    character(0), "q95", 2, 30
  )
)

#' Whether the quantiles `q` (named q05, q50, q95) lie in a band.
#' @param band "interval": q05 at least `lo` and q95 at most `hi`; "q95": q95 in [lo, hi]
#' @return TRUE or FALSE; an error for another band type
#' @export
in_band <- function(q, band, lo, hi) {
  switch(band,
    interval = q[["q05"]] >= lo && q[["q95"]] <= hi,
    q95 = q[["q95"]] >= lo && q[["q95"]] <= hi,
    stop("prior predictive: unknown band type ", band, call. = FALSE)
  )
}

#' Whether `data` hold every field of `set` at its value, to a relative 1e-12 (a config
#' file's decimal for log 1000 included).
#' @param set a named list or vector of data fields
#' @return TRUE or FALSE (TRUE for an empty `set`); an error naming a field `data` lacks
#' @export
same_as <- function(set, data) {
  missing <- setdiff(names(set), names(data))
  if (length(missing)) stop("prior predictive: the data have no field ", paste(missing, collapse = ", "), call. = FALSE)
  all(vapply(names(set), \(k) isTRUE(all.equal(as.numeric(data[[k]]), as.numeric(set[[k]]), tolerance = 1e-12)), logical(1)))
}

#' Whether a bias row is held at §6.10's value: `held` is not empty and `data` hold it.
#' @export
held_at <- function(held, data) length(held) > 0 && same_as(as.list(held), data)

#' A row's verdict on one cell.
#' @param ok whether its quantiles are in its band (`in_band()`)
#' @param kind "config" or "comparison"
#' @param held whether it is a bias row held at §6.10's value (`held_at()`)
#' @return `list(verdict, stops)`: "PASS" in band; out of band "WIDE" for a held config row,
#'   else "FAIL"; `stops` is TRUE for a config row's FAIL only
#' @export
row_verdict <- function(ok, kind, held) {
  if (!kind %in% c("config", "comparison")) stop("prior predictive: unknown row kind ", kind, call. = FALSE)
  verdict <- if (ok) "PASS" else if (kind == "config" && held) "WIDE" else "FAIL"
  list(verdict = verdict, stops = kind == "config" && verdict == "FAIL")
}

#' The printed line of a verdict: "PRIOR PASS <id>", or "PRIOR <verdict> <id> <q05> <q95>"
#' with the quantiles to 2 decimals.
#' @export
verdict_line <- function(verdict, id, q) {
  if (verdict == "PASS") {
    return(paste("PRIOR PASS", id))
  }
  sprintf("PRIOR %s %s %.2f %.2f", verdict, id, q[["q05"]], q[["q95"]])
}

#' `x` to 6 significant digits.
num <- function(x) trimws(formatC(x, digits = 6, format = "g"))

#' A set of data fields as "k=v,k=v", or "" for none.
set_text <- function(set) if (length(set)) paste(names(set), num(unlist(set)), sep = "=", collapse = ",") else ""

#' `path` relative to the project root when it lies under it, else its base name: a
#' committed report names no absolute path.
shown_path <- function(path) {
  full <- normalizePath(path, mustWork = TRUE)
  root <- normalizePath(project_root())
  if (startsWith(full, paste0(root, "/"))) substring(full, nchar(root) + 2) else basename(full)
}

#' The values of each row of `rows` over `n_draws` prior draws on `data`: a list, one numeric
#' vector per row. Rows with the same data share their draws: the config rows the config's
#' data, a comparison row its own `set`; every variant takes seeds SEED + 1, ..., SEED + n.
cell_values <- function(rows, data, n_draws, cell) {
  variant <- ifelse(rows$kind == "config", "config", vapply(rows$set, set_text, ""))
  out <- vector("list", nrow(rows))
  for (v in unique(variant)) {
    j <- which(variant == v)
    d <- data
    if (v != "config") d[names(rows$set[[j[1]]])] <- rows$set[[j[1]]]
    start <- proc.time()[["elapsed"]]
    vals <- lapply(j, \(r) vector("list", n_draws))
    for (i in seq_len(n_draws)) {
      sim <- v2_simulate(d, seed = SEED + i, observe = FALSE)
      for (m in seq_along(j)) vals[[m]][[i]] <- SUMMARY_FUNS[[rows$summary[j[m]]]](sim, d, SEED_AUX + i)
    }
    out[j] <- lapply(vals, unlist)
    cat(sprintf("%s, %s: %d prior draws in %.1f s\n", cell, v, n_draws, proc.time()[["elapsed"]] - start))
  }
  out
}

#' The report's prior column: the fields and their values in `data`, or "literals" when the
#' summary's prior has no data field (its scales are literals of the model).
prior_text <- function(keys, data) {
  if (!length(keys)) {
    return("literals")
  }
  paste(sprintf("`%s` = %s", keys, num(unlist(data[keys]))), collapse = ", ")
}

#' A band as the report prints it.
band_text <- function(band, lo, hi) {
  switch(band,
    interval = sprintf("[5%%, 95%%] inside [%s, %s]", num(lo), num(hi)),
    q95 = sprintf("95%% quantile in [%s, %s]", num(lo), num(hi))
  )
}

#' One markdown table row.
table_row <- function(cells) paste0("| ", paste(gsub("|", "\\|", cells, fixed = TRUE), collapse = " | "), " |")

#' The script: the check of `args` = c(config, [n_draws]).
main <- function(args) {
  if (!length(args) %in% 1:2) {
    stop("usage: Rscript R/harness/v2_prior_predictive.R <config> [n_draws=1000]", call. = FALSE)
  }
  n_draws <- if (length(args) == 2) suppressWarnings(as.numeric(args[2])) else 1000
  if (!(isTRUE(is.finite(n_draws)) && n_draws == round(n_draws) && n_draws >= MIN_DRAWS)) {
    stop("n_draws must be a whole number, at least ", MIN_DRAWS, call. = FALSE)
  }
  n_draws <- as.integer(n_draws)
  cfg <- read_v2_config(args[1])
  config <- shown_path(args[1])
  cells <- dplyr::left_join(CELLS, TARGETS, by = c("E", "F"))
  if (anyNA(cells$tag) || any(cells$confirmatory)) stop("CELLS must be non-confirmatory forecast cells", call. = FALSE)
  on <- vapply(ROWS$switch, \(s) is.na(s) || identical(cfg[[s]], 1L), logical(1))
  rows <- ROWS[on, ]

  inputs <- read_inputs()
  # WHY: v2_stan_data() fits stage 1 when the config turns fundamentals on; a prior draw never reads mu_f.
  no_stage1 <- function(info, cfg, root) stage1_placeholder(info)
  results <- list()
  skipped <- character(0)
  designs <- character(0)
  for (ci in seq_len(nrow(cells))) {
    cell <- cells$cell[ci]
    data <- v2_stan_data(info_set(cells$E[ci], cells$F[ci], inputs), cfg, stage1 = no_stage1)$data
    if (c(data$entry_epoch)[1] != 0L) {
      stop(cell, ": the coordinate party enters after the window's start, so it cannot be the 20% party", call. = FALSE)
    }
    designs <- c(designs, sprintf(
      "%s (E %s, F %s): P %d parties, H %d houses, K %d kj\u00f6rd\u00e6mi, J %d kj\u00f6rd\u00e6mi polls, %d epochs",
      cell, format(cells$E[ci]), format(cells$F[ci]), data$P, data$H, data$K, data$J, data$n_epoch
    ))
    held_by_config <- rows$kind == "comparison" & vapply(rows$set, same_as, logical(1), data = data)
    if (any(held_by_config)) {
      skipped <- union(skipped, sprintf("%s comparison %s (%s)", rows$summary[held_by_config], vapply(rows$set[held_by_config], set_text, ""), rows$source[held_by_config]))
    }
    run <- rows[!held_by_config, ]
    values <- cell_values(run, data, n_draws, cell)
    for (j in seq_len(nrow(run))) {
      v <- values[[j]]
      if (!length(v) || !all(is.finite(v))) stop(run$summary[j], " on ", cell, ": a value is not finite", call. = FALSE)
      q <- stats::setNames(stats::quantile(v, c(0.05, 0.5, 0.95), names = FALSE), c("q05", "q50", "q95"))
      comparison <- run$kind[j] == "comparison"
      vd <- row_verdict(in_band(q, run$band[j], run$lo[j], run$hi[j]), run$kind[j], !comparison && held_at(run$held[[j]], data))
      id <- paste(run$summary[j], cell, if (comparison) paste0("comparison:", set_text(run$set[[j]])) else "config", sep = "/")
      prior <- if (comparison) {
        sprintf("%s (%s)", prior_text(names(run$set[[j]]), run$set[[j]]), run$source[j])
      } else {
        prior_text(run$keys[[j]], data)
      }
      results[[length(results) + 1]] <- tibble::tibble(
        summary = run$summary[j], step = run$step[j], kind = run$kind[j], prior = prior, cell = cell,
        n = length(v), q05 = q[["q05"]], q50 = q[["q50"]], q95 = q[["q95"]],
        band = band_text(run$band[j], run$lo[j], run$hi[j]), verdict = vd$verdict, stops = vd$stops,
        line = verdict_line(vd$verdict, id, q)
      )
    }
  }
  res <- dplyr::bind_rows(results)
  res <- res[order(match(res$summary, ROWS$summary), match(res$kind, c("config", "comparison")), res$prior, match(res$cell, cells$cell)), ]

  labels <- unique(rows[c("summary", "step", "switch", "label", "band", "lo", "hi")])
  shown_verdict <- ifelse(res$stops, "FAIL (stops)", ifelse(res$kind == "comparison" & res$verdict == "FAIL", "FAIL (comparison)", res$verdict))
  report <- c(
    "# V2 prior predictive check",
    "",
    paste(
      "Written by `LC_ALL=en_US.UTF-8 Rscript R/harness/v2_prior_predictive.R <config> [n_draws]`.",
      "Prior draws only (`v2_simulate(observe = FALSE)`): nothing is fitted and no forecast is computed."
    ),
    "",
    sprintf("- Config: `%s` (sha256 `%s`)", config, digest::digest(file = args[1], algo = "sha256")),
    sprintf(
      "- Prior draws: %d per cell and row, seeds %d to %d (the walk row's z: seeds %d to %d)",
      n_draws, SEED + 1L, SEED + n_draws, SEED_AUX + 1L, SEED_AUX + n_draws
    ),
    paste("- Cell", designs),
    paste(
      "- The 20% party is the coordinate party (the first of the order), in a composition of 20% beside",
      "P \u2212 1 equal parties sharing 80%; each effect moves it on the log-ratio scale, and the summary",
      "is its change in pp. The script's header says which coordinate each effect reads."
    ),
    "",
    "## Summaries",
    "",
    sprintf(
      "- **%s** (step %s%s): %s. Band: %s.", labels$summary, labels$step,
      ifelse(is.na(labels$switch), "", paste0(", switch `", labels$switch, "`")), labels$label,
      mapply(band_text, labels$band, labels$lo, labels$hi)
    ),
    "",
    "## Results",
    "",
    "Quantiles in pp over the prior draws (`Values`: draws \u00d7 houses or kj\u00f6rd\u00e6mi for a pooled summary).",
    "",
    table_row(c("Summary", "Step", "Kind", "Prior", "Cell", "Values", "q05", "q50", "q95", "Band", "Verdict")),
    table_row(rep("---", 11)),
    vapply(seq_len(nrow(res)), \(i) table_row(c(
      res$summary[i], res$step[i], res$kind[i], res$prior[i], res$cell[i], res$n[i],
      sprintf("%.2f", res$q05[i]), sprintf("%.2f", res$q50[i]), sprintf("%.2f", res$q95[i]), res$band[i], shown_verdict[i]
    )), ""),
    "",
    if (length(skipped)) c(paste0("Skipped, as the config holds their values: ", paste(skipped, collapse = "; "), "."), ""),
    "## Lines",
    "",
    "```",
    res$line,
    "```",
    "",
    "## Reading the verdicts",
    "",
    "- **PASS**: the quantiles are in the band.",
    paste(
      "- **WIDE**: a config row of a bias whose prior the config holds at \u00a76.10's value, out of its band.",
      "\u00a76.10 fixes that prior, so the row goes to Brynj\u00f3lfur as information and does not stop the script."
    ),
    "- **FAIL (stops)**: a config row out of its band. The script stops after writing this report.",
    "- **FAIL (comparison)**: a comparison row out of its band, reported as information; it never stops the script.",
    paste(
      "- A band catches a broken prior (a wrong scale, unit or sign), not a mistuned one. It is wide on purpose,",
      "and no band is a tuning target: the quantiles themselves are the result."
    )
  )
  out <- harness_path("v2", "prior_predictive.md")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeLines(report, out)
  cat(sprintf("wrote %s (%d rows)\n", shown_path(out), nrow(res)))
  writeLines(res$line)
  if (any(res$stops)) {
    stop("prior predictive: ", sum(res$stops), " config row(s) left their band: ", paste(res$line[res$stops], collapse = "; "), call. = FALSE)
  }
  invisible(res)
}

if (is.null(box::name())) main(commandArgs(trailingOnly = TRUE))
