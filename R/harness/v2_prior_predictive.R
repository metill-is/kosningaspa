# The prior predictive check of arm V2 (spec §6.10; the plan's Task 6): summaries of prior
# draws of V2's model on the designs of two cells, each checked against a band written before
# any V2 fit. A plain script whose definitions also load as a box module, so that its test can
# import ROWS and the band logic; the script part runs only when box::name() is NULL. Run from
# the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/v2_prior_predictive.R <config> [n_draws=1000] [out]
# where out defaults to harness/v2/prior_predictive.md.
#
# It builds the config's Stan data on the 2024-eve and 2021-f180 cells (v2_stan_data(); stage 1
# is never fitted, since μ_f is a likelihood term's data and no prior draw reads it; no 2017
# cell is ever built), sets its walk and δ forms to the reference forms (reference_forms()),
# and on each takes n_draws prior points with
# v2_simulate(data, seed = SEED + i, observe = FALSE): parameters only, no counts, no fit and
# no forecast. Each row of ROWS reads one summary off every draw, in pp, and is checked
# against its band (by default on the 5% and 95% quantiles over the draws, pooled over houses
# or kjördæmi where said below):
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
# present from the start, except Annað's industry-bias steps (industry_step), whose scale is
# its own. The effects:
#   - house_d: d_h = σ_γ C ε_h (pars sigma_gamma and house_raw), coordinate 1 of every house,
#     the houses pooled (they are exchangeable).
#   - industry_mu1: μ_1, the parameter mu1 (normal(0, prior_sd_mu1) on every element,
#     sum-to-zero), coordinate 1. The newcomers' elements enter as drawn, before §6.4 replaces
#     them at entry, so the row reads the scale prior_sd_mu1 alone.
#   - poll_sd: no composition. The Dirichlet-multinomial sd of a share s = 0.2 in a poll of
#     n = 1000, sqrt(s (1 − s) / n · (n + φ) / (1 + φ)), at each house's φ_h (v2_natural()'s
#     phi), the houses pooled.
#   - kd_delta: δ_open,k, v2_natural()'s δ in the open epoch, coordinate 1 of every
#     kjördæmi, the kjördæmi pooled: the core's static δ under delta_walk = 0 (proposal 2's
#     hyperprior log s_δ0 = μ_sd + τ_sd r), and under 1 the walk's δ (§6.7), which has taken
#     every step to the open epoch, so it is wider. Σ_k w_k δ_k = 0 makes log s0 the national
#     log-ratio to first order, so the change is the kjördæmi's deviation from the party's
#     national share to that order.
#   - walk_180: x = √180 · C(σ ⊙ L_Ω z), with z ~ N(0, I_P) from seed SEED_AUX + i and the
#     draw's σ and L_Ω: covariance 180 CΣCᵀ, the walk's own innovation covariance (§6.2) over
#     180 days off the campaign (s = 1). The campaign multiplier τ_camp has a prior of its own,
#     which this row leaves out. The change is signed, so its band reads the upper tail.
#   - industry_step (step v2s-m1, switch epoch_bias): x = C(τ ⊙ ξ_e), an epoch's step of the
#     industry bias (§6.4; pars mu_step_raw, tau_mu and tau_mu_A), τ_p = τ_μ but τ_μ,A for
#     Annað (the data's annad_idx), coordinate 1 of every step, the steps pooled (they are
#     exchangeable). It stops if the coordinate party is Annað.
#   - newcomer_entry (step v2s-m2, switch newcomer_pool): x = (m_new + s_new η_p, 0, ..., 0),
#     a newcomer's entry bias against the mean bias of the parties existing before its entry
#     epoch (§6.4; pars m_new, s_new and new_raw), read on a 10% party (NEWCOMER_SHARE) beside
#     P − 1 equal parties, the newcomers pooled (they are exchangeable). The re-centring of μ
#     after the entry is a common shift, which cancels in the softmax. It reads no
#     party-indexed parameter, so coordinate 1 here is the newcomer, not the coordinate party.
#   - shock_sd_20 and shock_sd_05 (step v2s-m3, switch shock): the sd of the election-day
#     shock's share change (§6.6) given the draw's s̄_E (par s_bar_E), on a 20% and on a 5%
#     party: over SHOCK_INNER draws of z ~ N(0, I_P) from seed SEED_AUX + i, the sd of
#     share_change(C(s̄_E d ⊙ z), share) on the composition (share, (1 − share) / (P − 1), ...),
#     each element's d = √(min(0.2 / π_p, 10)) the builder's formula (shock_scaling()) on that
#     composition. §6.10 puts the median at about 1.6 pp for a 20% party and 0.95 pp for a 5%
#     one: the party-alone linearisation π(1 − π) s̄ d at s̄_E's median, 0.15 · qnorm(0.75).
#     This construction also shocks the other P − 1 parties (the centring is a common shift,
#     which cancels in the softmax), so its population medians are about 1.80 and 1.00 pp, and
#     the ratios to §6.10's targets are expected near 1.13 and 1.05. They read no
#     party-indexed parameter, so coordinate 1 is the 20% or 5% party.
#   - delta_step_4y (step v2s-m4, switch delta_walk): the change in a party's kjördæmi
#     deviation over a 4-year gap (§6.7), a step in an epoch where no party enters: row k of
#     M_e(√(4 / Δ̄) s_ζ ⊙ ζ), the walk's step on the open epoch's weights w, with ζ ~ N(0, 1)
#     on every kjördæmi and party from seed SEED_AUX + i and s_ζ = s̄_ζ s_zeta_raw (pars
#     s_bar_zeta and s_zeta_raw), coordinate 1 of every kjördæmi, the kjördæmi pooled. In such
#     an epoch M_e removes the walk's −c_{e−1} 1ᵀ, so a change δ_e − δ_{e−1} is this step plus
#     M_e(δ_{e−1}) − δ_{e−1}, the change of kjördæmi weights, which this row leaves out.
#     Coordinate 1's own scale is s_zeta_raw[1].
#
# The bands, each built by its constructor and checked by in_band():
#   band_inside(lo, hi)                      the 5% quantile at least lo, the 95% at most hi
#   band_q95(lo, hi)                         the 95% quantile in [lo, hi]
#   band_median_ratio(target, lo = 0.5, hi = 2)   the median in [lo target, hi target]
#   band_exact(value, tol)                   every draw within tol of value
#   band_report()                            no band: the row is reported, never judged
#
# Verdicts and lines, one per row and cell:
#   PRIOR PASS <summary>                       the row is in its band
#   PRIOR FAIL <summary> <q05> <q95>           it is not
#   PRIOR WIDE <summary> <q05> <q95>           it is not, on a config row of a bias whose prior
#                                              the config holds at §6.10's value (`held`)
#   PRIOR REPORT <summary> <q05> <q50> <q95>   a band_report() row
# <summary> is <summary>/<cell>/<kind>, a comparison's kind followed by its fields. A
# comparison row prints PASS or FAIL, and a report row REPORT; neither ever stops the script.
# The script writes its report (out), prints the lines, and then stops with an error if a
# config row FAILs. A band catches a broken prior (a wrong scale, unit or sign), not a
# mistuned one, and is no tuning target.
#
# Later steps append their rows to ROWS (keyed by summary and step; a row with a switch, one
# of V2_SWITCHES, runs only when the config turns it on) and their summaries to SUMMARY_FUNS.
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
  R / harness / v2[read_v2_config, shock_scaling, stage1_placeholder, V2_SWITCHES, v2_stan_data],
  R / harness / v2_reference[M_e, v2_simulate]
)

#' The share of the party whose change the composition summaries read, the poll size of
#' the poll-sd row, and the days of the walk row.
SHARE <- 0.2
POLL_N <- 1000
WALK_DAYS <- 180

#' The share of the newcomer whose entry bias the newcomer row reads.
NEWCOMER_SHARE <- 0.1

#' The gap in years of the δ-walk row's step.
DELTA_GAP_YEARS <- 4

#' The shares of the parties whose shock sd the shock rows read, and the draws of z each
#' value's sd is taken over.
SHOCK_SHARES <- c(shock_sd_20 = 0.2, shock_sd_05 = 0.05)
SHOCK_INNER <- 200L

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

#' The first party's change in pp, at share `share` (20% by default), when the composition
#' (share, (1 − share) / (P − 1), ...) moves by `x` on the log-ratio scale:
#' 100 (softmax(log s0 + x)_1 − share).
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

#' The sd in pp of the first party's change under the election-day shock (§6.6) at scale
#' `s_bar_E`: over `SHOCK_INNER` draws of z ~ N(0, I_P) from `seed`, the sd of
#' `share_change(C(s_bar_E d ⊙ z), share)` on the composition (share, (1 − share) / (P − 1),
#' ...), each element's d `shock_scaling()` of its share in that composition.
#' @param s_bar_E the shock's scale, one non-negative number
#' @param P the parties, a whole number of at least 2
#' @param share the first party's share, in (0, 1)
#' @param seed the seed of the z draws
#' @return numeric(1)
#' @export
shock_sd <- function(s_bar_E, P, share, seed) {
  one <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  if (!(one(s_bar_E) && s_bar_E >= 0)) stop("prior predictive: shock_sd() needs s_bar_E, one non-negative number", call. = FALSE)
  if (!(one(P) && P == round(P) && P >= 2)) stop("prior predictive: shock_sd() needs P, a whole number of at least 2", call. = FALSE)
  if (!(one(share) && share > 0 && share < 1)) stop("prior predictive: shock_sd() needs share in (0, 1)", call. = FALSE)
  d <- shock_scaling(c(share, rep((1 - share) / (P - 1), P - 1)))
  z <- withr::with_seed(seed, matrix(stats::rnorm(P * SHOCK_INNER), P, SHOCK_INNER))
  x <- s_bar_E * d * z
  x <- x - rep(colMeans(x), each = P)
  stats::sd(apply(x, 2, share_change, share = share))
}

#' A shock row's value for one prior draw `sim`: `shock_sd()` at the draw's s̄_E.
shock_row <- function(sim, data, seed, share) {
  if (length(sim$pars$s_bar_E) != 1) {
    stop("prior predictive: the shock rows need s_bar_E, which shock = 1 puts in the model", call. = FALSE)
  }
  shock_sd(as.numeric(sim$pars$s_bar_E), data$P, share, seed)
}

#' The Dirichlet-multinomial sd of a share `s` in a poll of `n` at concentration `phi`, in pp:
#' 100 sqrt(s (1 − s) / n · (n + φ) / (1 + φ)).
#' @param phi concentrations, non-negative
#' @return a numeric vector as long as `phi`
#' @export
poll_sd_pp <- function(phi, s = SHARE, n = POLL_N) 100 * sqrt(s * (1 - s) / n * (n + phi) / (1 + phi))

#' The summaries, each a function of a `v2_simulate()` draw `sim`, its `data` and a `seed`
#' for any further randomness, returning the draw's values in pp (one per house,
#' kjördæmi, epoch step or newcomer for a pooled summary). The header comment says which coordinate each reads.
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
  },
  industry_step = function(sim, data, seed) {
    if (identical(as.integer(data$annad_idx), 1L)) {
      stop("prior predictive: the coordinate party is Anna\u00f0, whose industry-bias steps have their own scale", call. = FALSE)
    }
    xi <- matrix(sim$pars$mu_step_raw, data$P, data$n_epoch - 1L)
    tau <- replace(rep(as.numeric(sim$pars$tau_mu), data$P), data$annad_idx, as.numeric(sim$pars$tau_mu_A))
    apply(tau * xi, 2, \(x) share_change(x - mean(x)))
  },
  newcomer_entry = function(sim, data, seed) {
    if (length(sim$pars$m_new) != 1 || length(sim$pars$s_new) != 1) {
      stop("prior predictive: newcomer_entry needs m_new and s_new, which newcomer_pool = 1 puts in the model", call. = FALSE)
    }
    x <- as.numeric(sim$pars$m_new) + as.numeric(sim$pars$s_new) * as.numeric(sim$pars$new_raw)
    vapply(x, \(v) share_change(c(v, rep(0, data$P - 1)), share = NEWCOMER_SHARE), numeric(1))
  },
  shock_sd_20 = function(sim, data, seed) shock_row(sim, data, seed, SHOCK_SHARES[["shock_sd_20"]]),
  shock_sd_05 = function(sim, data, seed) shock_row(sim, data, seed, SHOCK_SHARES[["shock_sd_05"]]),
  delta_step_4y = function(sim, data, seed) {
    if (length(sim$pars$s_bar_zeta) != 1 || length(sim$pars$s_zeta_raw) != data$P) {
      stop(
        "prior predictive: delta_step_4y needs s_bar_zeta and s_zeta_raw, which delta_walk = 1 puts in the model",
        call. = FALSE
      )
    }
    zeta <- withr::with_seed(seed, matrix(stats::rnorm(data$K * data$P), data$K, data$P))
    s_zeta <- as.numeric(sim$pars$s_bar_zeta) * as.numeric(sim$pars$s_zeta_raw)
    x <- sqrt(DELTA_GAP_YEARS / data$delta_years_bar) * zeta * matrix(s_zeta, data$K, data$P, byrow = TRUE)
    apply(M_e(x, data$w[data$n_epoch, ]), 1, share_change)
  }
)

#' `x` to 6 significant digits.
num <- function(x) trimws(formatC(x, digits = 6, format = "g"))

#' A set of data fields as "k=v,k=v", or "" for none.
set_text <- function(set) if (length(set)) paste(names(set), num(unlist(set)), sep = "=", collapse = ",") else ""

#' A band: the check a row's values must pass, built by one of these constructors and checked
#' by `in_band()`. `band_inside(lo, hi)`: the 5% quantile at least `lo` and the 95% at most
#' `hi`. `band_q95(lo, hi)`: the 95% quantile in [lo, hi]. `band_median_ratio(target, lo, hi)`:
#' the median in [lo target, hi target], `target` positive. `band_exact(value, tol)`: every
#' draw within `tol` of `value`. `band_report()`: no band; the row is reported, never judged.
#' @return a list with its `type` and parameters
#' @export
band_inside <- function(lo, hi) band_of("inside", lo = lo, hi = hi)

#' @rdname band_inside
#' @export
band_q95 <- function(lo, hi) band_of("q95", lo = lo, hi = hi)

#' @rdname band_inside
#' @export
band_median_ratio <- function(target, lo = 0.5, hi = 2) {
  if (!(is.numeric(target) && length(target) == 1 && target > 0)) stop("prior predictive: a median_ratio target must be positive", call. = FALSE)
  band_of("median_ratio", target = target, lo = lo, hi = hi)
}

#' @rdname band_inside
#' @export
band_exact <- function(value, tol) {
  if (!(is.numeric(tol) && length(tol) == 1 && tol >= 0)) stop("prior predictive: an exact band's tol must be at least 0", call. = FALSE)
  band_of("exact", value = value, tol = tol)
}

#' @rdname band_inside
#' @export
band_report <- function() list(type = "report")

#' A band of `type` with the numbers `...`, each one finite number, `lo` at most `hi`.
band_of <- function(type, ...) {
  b <- list(...)
  ok <- vapply(b, \(x) is.numeric(x) && length(x) == 1 && is.finite(x), logical(1))
  if (!all(ok)) stop("prior predictive: a ", type, " band takes one finite number per parameter", call. = FALSE)
  if (!is.null(b$lo) && b$lo > b$hi) stop("prior predictive: a ", type, " band has lo above hi", call. = FALSE)
  c(list(type = type), b)
}

#' Whether a row's `values` pass its band.
#' @param band a band (`band_inside()` and the other constructors)
#' @param q the quantiles of `values`, named q05, q50, q95
#' @param values every draw's values
#' @return TRUE or FALSE; NA for `band_report()`; an error for an unknown band type
#' @export
in_band <- function(band, q, values) {
  switch(band$type,
    inside = q[["q05"]] >= band$lo && q[["q95"]] <= band$hi,
    q95 = q[["q95"]] >= band$lo && q[["q95"]] <= band$hi,
    median_ratio = q[["q50"]] >= band$lo * band$target && q[["q50"]] <= band$hi * band$target,
    exact = all(abs(values - band$value) <= band$tol),
    report = NA,
    stop("prior predictive: unknown band type ", band$type, call. = FALSE)
  )
}

#' A band as the report prints it.
band_text <- function(band) {
  switch(band$type,
    inside = sprintf("[5%%, 95%%] inside [%s, %s]", num(band$lo), num(band$hi)),
    q95 = sprintf("95%% quantile in [%s, %s]", num(band$lo), num(band$hi)),
    median_ratio = sprintf("median in [%s, %s] \u00d7 %s", num(band$lo), num(band$hi), num(band$target)),
    exact = sprintf("every draw within %s of %s", num(band$tol), num(band$value)),
    report = "none (reported)"
  )
}

#' The rows of one summary: its config row, then one comparison row per entry of `compare`.
#' @param summary its name in SUMMARY_FUNS
#' @param step the step that adds it
#' @param switch NA, or the data switch (one of `V2_SWITCHES`) its component needs
#' @param label what it is, for the report
#' @param keys the data's prior fields its prior reads, for the report
#' @param band its band (`band_inside()` and the other constructors)
#' @param held NULL, or for a bias row its prior field at §6.10's value, a named number: a
#'   config row whose data hold it prints WIDE out of its band
#' @param compare a list of comparison rows, each `list(set, source)`: the data fields it
#'   changes (a named list) and where their values come from
#' @return a tibble: summary, step, switch, label, keys, kind, set, source, held, band
summary_rows <- function(summary, step, switch, label, keys, band, held = NULL, compare = list()) {
  tibble::tibble(
    summary = summary, step = step, switch = switch, label = label, keys = list(keys),
    kind = c("config", rep("comparison", length(compare))),
    set = c(list(NULL), lapply(compare, `[[`, "set")),
    source = c("config", vapply(compare, `[[`, "", "source")),
    held = c(list(held), rep(list(NULL), length(compare))),
    band = list(band)
  )
}

#' `rows` after checking the table: every switch NA or one of `V2_SWITCHES`, every summary
#' in SUMMARY_FUNS, every band of a known type (`in_band()`), one config row per (summary,
#' step), every comparison row with fields to change and none repeated within its
#' (summary, step), and `held` only on config rows.
#' @param rows a table of `summary_rows()`
#' @return `rows`; an error naming the first fault
#' @export
check_rows <- function(rows) {
  bad <- function(...) stop("prior predictive: ROWS: ", ..., call. = FALSE)
  sw <- rows$switch[!is.na(rows$switch)]
  if (!all(sw %in% V2_SWITCHES)) bad("switch not in V2_SWITCHES: ", paste(setdiff(sw, V2_SWITCHES), collapse = ", "))
  if (!all(rows$summary %in% names(SUMMARY_FUNS))) bad("summary not in SUMMARY_FUNS: ", paste(setdiff(rows$summary, names(SUMMARY_FUNS)), collapse = ", "))
  types <- vapply(rows$band, \(b) if (is.list(b) && is.character(b$type)) b$type else "", "")
  known <- c("inside", "q95", "median_ratio", "exact", "report")
  if (!all(types %in% known)) bad("unknown band type: ", paste(setdiff(types, known), collapse = ", "))
  config <- rows$kind == "config"
  if (!all(rows$kind %in% c("config", "comparison"))) bad("kind not config or comparison")
  if (anyDuplicated(paste(rows$summary, rows$step)[config])) bad("two config rows for one (summary, step)")
  if (!all(paste(rows$summary, rows$step)[!config] %in% paste(rows$summary, rows$step)[config])) bad("a comparison row with no config row")
  if (any(lengths(rows$set[!config]) == 0)) bad("a comparison row with no fields to change")
  if (any(lengths(rows$held[!config]) > 0)) bad("held on a comparison row")
  key <- paste(rows$summary, rows$step, rows$kind, vapply(rows$set, set_text, ""))
  if (anyDuplicated(key)) bad("a repeated comparison: ", key[duplicated(key)][1])
  rows
}

#' Every row of the check, keyed by summary and step. The core's (step v2s-m0): the bands
#' were written before any V2 fit; the first three rows each have the other values of the
#' plan's Decision 1 as comparisons (§6.10's, and proposals 1 and 4). The epoch bias's (step
#' v2s-m1, switch `epoch_bias`): an epoch's step of the industry bias, its band written before
#' any fit of that step. The newcomer pool's (step v2s-m2, switch `newcomer_pool`): a
#' newcomer's entry bias on a 10% party, its band written before any fit of that step. The
#' shock's (step v2s-m3, switch `shock`): its sd on a 20% and on a 5% party, each median
#' within [0.5×, 2×] of §6.10's 1.6 and 0.95 pp, written before any fit of that step. The δ
#' walk's (step v2s-m4, switch `delta_walk`): the change in a 20% party's kjördæmi deviation
#' over a 4-year gap, its band written before any fit of that step.
#' @export
ROWS <- check_rows(dplyr::bind_rows(
  summary_rows(
    "house_d", "v2s-m0", NA_character_,
    "A house's deviation d from the industry, on a 20% party (d_h = \u03c3_\u03b3 C \u03b5_h, \u03c3_\u03b3 ~ exponential(`prior_rate_sigma_gamma`))",
    "prior_rate_sigma_gamma", band_inside(-10, 10),
    held = c(prior_rate_sigma_gamma = 1),
    compare = list(
      list(set = list(prior_rate_sigma_gamma = 1), source = "\u00a76.10"),
      list(set = list(prior_rate_sigma_gamma = 5), source = "proposal 4")
    )
  ),
  summary_rows(
    "industry_mu1", "v2s-m0", NA_character_,
    "Industry bias \u03bc_1 on a 20% party (\u03bc_1 sum-to-zero, normal(0, `prior_sd_mu1`) on every element)",
    "prior_sd_mu1", band_inside(-12, 12),
    held = c(prior_sd_mu1 = 1),
    compare = list(
      list(set = list(prior_sd_mu1 = 1), source = "\u00a76.10"),
      list(set = list(prior_sd_mu1 = 0.3), source = "proposal 4")
    )
  ),
  summary_rows(
    "poll_sd", "v2s-m0", NA_character_,
    "A poll's sd around its mean, n = 1,000, 20% party, at the prior \u03c6_h (\u03bc_\u03c6 ~ N(`prior_mu_phi`, 1))",
    "prior_mu_phi", band_inside(0.8, 6),
    compare = list(
      list(set = list(prior_mu_phi = 0), source = "\u00a76.10"),
      list(set = list(prior_mu_phi = log(1000)), source = "proposal 1")
    )
  ),
  summary_rows(
    "kd_delta", "v2s-m0", NA_character_,
    "\u03b4 in the open epoch: a party's kj\u00f6rd\u00e6mi deviation from its national share, 20% party (static under `delta_walk` = 0, log s_\u03b40 = \u03bc_sd + \u03c4_sd r; the walk's under 1)",
    character(0), band_inside(-25, 25)
  ),
  summary_rows(
    "walk_180", "v2s-m0", NA_character_,
    "A 20% party's 180-day walk change, off the campaign (covariance 180 C\u03a3C\u1d40)",
    character(0), band_q95(2, 30)
  ),
  summary_rows(
    "industry_step", "v2s-m1", "epoch_bias",
    paste0(
      "An epoch's step in the industry bias on a 20% party (C(\u03c4 \u2299 \u03be_e), \u03c4_\u03bc ~ half-N(0, `prior_tau_mu`), ",
      "\u03c4_\u03bc,A ~ half-N(0, 0.3) for Anna\u00f0)"
    ),
    "prior_tau_mu", band_inside(-6, 6)
  ),
  summary_rows(
    "newcomer_entry", "v2s-m2", "newcomer_pool",
    paste0(
      "A newcomer's entry bias against the existing parties' mean, on a 10% party (m_new + s_new \u03b7, ",
      "m_new ~ N(0, 0.3), s_new ~ half-N(0, 0.3))"
    ),
    character(0), band_inside(-10, 10)
  ),
  summary_rows(
    "shock_sd_20", "v2s-m3", "shock",
    paste0(
      "The election-day shock's sd on a 20% party: given the draw's s\u0304_E, the sd of its change ",
      "over ", SHOCK_INNER, " draws of z, on the composition (20%, 80% / (P \u2212 1), \u2026), with \u03b5 = ",
      "C(s\u0304_E d \u2299 z) and each d = \u221a(min(0.2 / \u03c0, 10)) of that composition ",
      "(s\u0304_E ~ half-N(0, `prior_s_bar_E`))"
    ),
    "prior_s_bar_E", band_median_ratio(1.6)
  ),
  summary_rows(
    "shock_sd_05", "v2s-m3", "shock",
    paste0(
      "The election-day shock's sd on a 5% party: given the draw's s\u0304_E, the sd of its change ",
      "over ", SHOCK_INNER, " draws of z, on the composition (5%, 95% / (P \u2212 1), \u2026), with \u03b5 = ",
      "C(s\u0304_E d \u2299 z) and each d = \u221a(min(0.2 / \u03c0, 10)) of that composition ",
      "(s\u0304_E ~ half-N(0, `prior_s_bar_E`))"
    ),
    "prior_s_bar_E", band_median_ratio(0.95)
  ),
  summary_rows(
    "delta_step_4y", "v2s-m4", "delta_walk",
    paste0(
      "A 20% party's change in its kj\u00f6rd\u00e6mi deviation over a 4-year gap: the walk's step ",
      "M_e(\u221a(4 / \u0394\u0304) s_\u03b6 \u2299 \u03b6) on the open epoch's weights, ",
      "s_\u03b6 = s\u0304_\u03b6 s_zeta_raw (s\u0304_\u03b6 ~ half-N(0, `prior_s_bar_zeta`), s_zeta_raw ~ half-N(0, 1)), ",
      "leaving out M_e(\u03b4_{e\u22121}) \u2212 \u03b4_{e\u22121}, the change of kj\u00f6rd\u00e6mi weights"
    ),
    "prior_s_bar_zeta", band_inside(-15, 15)
  )
))

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
#' @param ok whether it passes its band (`in_band()`): TRUE, FALSE, or NA for a report row
#' @param kind "config" or "comparison"
#' @param held whether it is a bias row held at §6.10's value (`held_at()`)
#' @return `list(verdict, stops)`: "REPORT" for a report row; "PASS" in band; out of band
#'   "WIDE" for a held config row, else "FAIL"; `stops` is TRUE for a config row's FAIL only
#' @export
row_verdict <- function(ok, kind, held) {
  if (!kind %in% c("config", "comparison")) stop("prior predictive: unknown row kind ", kind, call. = FALSE)
  verdict <- if (is.na(ok)) "REPORT" else if (ok) "PASS" else if (kind == "config" && held) "WIDE" else "FAIL"
  list(verdict = verdict, stops = kind == "config" && verdict == "FAIL")
}

#' The printed line of a verdict: "PRIOR PASS <id>", "PRIOR REPORT <id> <q05> <q50> <q95>",
#' or "PRIOR <verdict> <id> <q05> <q95>", the quantiles to 2 decimals.
#' @export
verdict_line <- function(verdict, id, q) {
  if (verdict == "PASS") {
    return(paste("PRIOR PASS", id))
  }
  if (verdict == "REPORT") {
    return(sprintf("PRIOR REPORT %s %.2f %.2f %.2f", id, q[["q05"]], q[["q50"]], q[["q95"]]))
  }
  sprintf("PRIOR %s %s %.2f %.2f", verdict, id, q[["q05"]], q[["q95"]])
}

#' `data` with `walk_form` and `delta_form` at 0, the reference forms, and every other field
#' as it was.
#' @param data a `v2_stan_data()` data list
#' @return the data list; an error if it lacks either field
#' @export
reference_forms <- function(data) {
  if (!all(c("walk_form", "delta_form") %in% names(data))) stop("prior predictive: the data have no walk_form or delta_form", call. = FALSE)
  data$walk_form <- 0L
  data$delta_form <- 0L
  data
}

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

#' One markdown table row.
table_row <- function(cells) paste0("| ", paste(gsub("|", "\\|", cells, fixed = TRUE), collapse = " | "), " |")

#' The script: the check of `args` = c(config, [n_draws], [out]).
main <- function(args) {
  if (!length(args) %in% 1:3) {
    stop("usage: Rscript R/harness/v2_prior_predictive.R <config> [n_draws=1000] [out]", call. = FALSE)
  }
  n_draws <- if (length(args) >= 2) suppressWarnings(as.numeric(args[2])) else 1000
  out <- if (length(args) == 3) args[3] else harness_path("v2", "prior_predictive.md")
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
    # WHY: a P-step form (walk_form, delta_form) reparameterises the same model, so the prior draws do not depend on it, and v2_simulate() takes the reference forms only.
    data <- reference_forms(v2_stan_data(info_set(cells$E[ci], cells$F[ci], inputs), cfg, stage1 = no_stage1)$data)
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
      vd <- row_verdict(in_band(run$band[[j]], q, v), run$kind[j], !comparison && held_at(run$held[[j]], data))
      id <- paste(run$summary[j], cell, if (comparison) paste0("comparison:", set_text(run$set[[j]])) else "config", sep = "/")
      prior <- if (comparison) {
        sprintf("%s (%s)", prior_text(names(run$set[[j]]), run$set[[j]]), run$source[j])
      } else {
        prior_text(run$keys[[j]], data)
      }
      results[[length(results) + 1]] <- tibble::tibble(
        summary = run$summary[j], step = run$step[j], kind = run$kind[j], prior = prior, cell = cell,
        n = length(v), q05 = q[["q05"]], q50 = q[["q50"]], q95 = q[["q95"]],
        band = band_text(run$band[[j]]), verdict = vd$verdict, stops = vd$stops,
        line = verdict_line(vd$verdict, id, q)
      )
    }
  }
  res <- dplyr::bind_rows(results)
  res <- res[order(match(res$summary, ROWS$summary), match(res$kind, c("config", "comparison")), res$prior, match(res$cell, cells$cell)), ]

  labels <- rows[rows$kind == "config", ]
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
      "- Prior draws: %d per cell and row, seeds %d to %d (the walk, shock and \u03b4-step rows' z: seeds %d to %d)",
      n_draws, SEED + 1L, SEED + n_draws, SEED_AUX + 1L, SEED_AUX + n_draws
    ),
    paste("- Cell", designs),
    paste(
      "- The 20% party is the coordinate party (the first of the order), in a composition of 20% beside",
      "P \u2212 1 equal parties sharing 80%; each effect moves it on the log-ratio scale, and the summary",
      "is its change in pp. The script's header says which coordinate each effect reads."
    ),
    if ("newcomer_entry" %in% rows$summary) {
      sprintf(
        paste(
          "- `newcomer_entry` reads no coordinate party: its first party is the newcomer, at %g%% beside",
          "P \u2212 1 equal parties sharing %g%%, moved by its entry bias against the existing parties' mean."
        ),
        100 * NEWCOMER_SHARE, 100 * (1 - NEWCOMER_SHARE)
      )
    },
    if (any(c("shock_sd_20", "shock_sd_05") %in% rows$summary)) {
      sprintf(
        paste(
          "- `shock_sd_20` and `shock_sd_05` read no coordinate party: their first party is a %g%% or a %g%% party beside",
          "P \u2212 1 equal parties, and each value is the sd of its change over %d draws of z given the draw's s\u0304_E."
        ),
        100 * SHOCK_SHARES[["shock_sd_20"]], 100 * SHOCK_SHARES[["shock_sd_05"]], SHOCK_INNER
      )
    },
    "",
    "## Summaries",
    "",
    sprintf(
      "- **%s** (step %s%s): %s. Band: %s.", labels$summary, labels$step,
      ifelse(is.na(labels$switch), "", paste0(", switch `", labels$switch, "`")), labels$label,
      vapply(labels$band, band_text, "")
    ),
    "",
    "## Results",
    "",
    "Quantiles in pp over the prior draws (`Values`: draws \u00d7 houses, kj\u00f6rd\u00e6mi, epoch steps or newcomers for a pooled summary).",
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
    if (any(res$verdict == "REPORT")) "- **REPORT**: a row with no band, reported as information; it never stops the script.",
    paste(
      "- A band catches a broken prior (a wrong scale, unit or sign), not a mistuned one. It is wide on purpose,",
      "and no band is a tuning target: the quantiles themselves are the result."
    )
  )
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
