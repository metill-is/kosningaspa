# Simulation-based recovery for arm V2 (spec section 8, the Phase 2 plan's Decision 7): does a
# fit of V2 cover the parameters that simulated its data? A plain script whose definitions also
# load as a box module, so that its test can import the redraw rule, the coverage checks and the
# folder states; the script part runs only when box::name() is NULL. Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/recovery.R <E> <F> <first_seed> <n> <iter_warmup> <iter_sampling> [--lane=A|B]
#   LC_ALL=en_US.UTF-8 Rscript R/harness/recovery.R report [out]
#
# The first form fits one replicate for each seed of first_seed .. first_seed + n - 1 on the cell
# (E, F), lane A unless --lane says B:
#   1. the real design: arm v2's config (harness/configs/v2.json) and v2_stan_data() of the cell,
#      after validate_inputs(), with draw_forecast set to 0, since recovery reads no forecast
#      quantity. A 2017 cell is refused before the freeze (forecast_free()), before anything is
#      built or written;
#   2. the truth and its data: v2_simulate() at seed SEED_STRIDE * seed + a for attempt a = 1,
#      2, ...: the parameters one draw of V2's prior, every count (y_nat, y_kd, y_res) redrawn
#      from the model given them, each row keeping its total and its reported parties, then mu_f
#      from its pseudo-observation's normal. The grid, the poll schedule, sample sizes, houses,
#      entry and valid votes stay the real cell's, and so does d_shock, the shock scaling taken
#      from the previous election's real national shares: it is design, not a count, so no real
#      count survives, but real past shares still scale the shocks. An attempt is rejected, and
#      the next drawn, when a reported party's share of a closed election's national result
#      (its kjordaemi counts summed per party), or its mean share over an epoch's national
#      polls (the polls reporting it), leaves [0.5%, 70%] (redraw_reasons()). The rule reads
#      the simulated data alone, so the posterior given the kept data is the model's and the
#      ranks stay uniform. It reads national results, not each kjordaemi's: Decision 7's floor
#      is a national-support notion, and a kjordaemi reading would also select on delta, a
#      recovery target;
#   3. a check that each truth element is the Stan model's quantity of that name: a
#      one-iteration fixed_param run at the true parameters must give every one to 1e-8
#      relative, paired by name (truth_check(), which names the elements that differ);
#   4. the fit: SAMPLER's chains, init and max_treedepth, the replicate's seed;
#   5. the ranks: for each truth element (truth_elements()), q = mean(draws < truth), inside the
#      90% interval when 0.05 <= q <= 0.95 and the 50% when 0.25 <= q <= 0.75.
# The groups (truth_elements()): tau_mu[1] and s_bar_E[1], one truth per replicate; the industry
# bias at the open epoch, mu[n_epoch, p] for every party; delta at the closed epochs,
# delta[e, k, p] for e <= n_closed, and at the open epoch, delta[n_epoch, k, p], every kjordaemi
# and party, a newcomer's pre-entry columns included (a prior draw no data sees, which a
# calibrated fit covers all the same). Annad's own industry-bias scale tau_mu_A is no group.
#
# Each replicate is written to results_root()/recovery/<E>_<F>/<seed>/ (HARNESS_RESULTS
# overrides results/harness), every file read-only, meta.json last:
#   inputs/stan_data.json   the simulated data the fit read; inputs/inputs.json, its seeds
#   truth.json              the seeds, the attempts, the true parameters (V2_CORE_PARS) and
#                           their natural quantities (mu, gamma, delta, eps, the concentrations,
#                           s_delta0, s_zeta)
#   ranks.csv               one row per truth element: group, variable, epoch, kjordaemi,
#                           flokkur, truth, q, in50, in90
#   param_draws.parquet     the V2_PARAM_VARIABLES columns of a harness fit
#   key_diagnostics.csv     R-hat and ESS of the truth elements, the param_draws columns, lp__
#   meta.json               the provenance of the code the replicate ran (run_provenance(),
#                           read once when the lane starts, after its modules are loaded:
#                           git_head, git_dirty, the snapshot manifest's sha256, CmdStan's and
#                           cmdstanr's versions; inputs/inputs.json holds the same), git_head_end
#                           and git_dirty_end (read again when the replicate ends; a change is
#                           logged as PROVENANCE), the Stan file's, the config's and the real
#                           design's sha256, the seeds and attempts, the sampler settings, the
#                           minutes, the truth check's error, each group's element count, the
#                           diagnostics and real_cell_redraw (real_rule_text() of the real
#                           cell's own data)
# A replicate's scored quantities, for summarise_diagnostics() and so for the stop rule's
# gate-scope R-hat, are its truth elements. A replicate whose meta.json matches the run's
# settings (expected_keys()) is skipped (SKIP); one whose meta.json does not, that has none (a
# fit that stopped) or whose meta.json does not parse, is moved to results_root()/_stale/
# (STALE) and refitted, never mixed.
# Each replicate is claimed by a symlink <E>_<F>/.<seed>.owner naming the process, so a seed
# that another live process is fitting is left alone (BUSY). Each lane has its own log
# (recovery/logs/lane-<L>.log), lock and compiled copy of the Stan file
# (recovery/_lanes/<L>/), and two lanes share no file; a lane already running refuses a second
# process. Every replicate ends with the stop rule's line (R/harness/compare_runs.R's
# stop_rule_at(), Decision 5) and "DONE <dir>". The CmdStan CSVs are deleted unless
# HARNESS_KEEP_CMDSTAN is 1.
#
# report reads every replicate under results_root()/recovery/ whose meta.json exists and writes
# out (harness/v2/recovery.md by default): the provenance (the commits at lane start, the
# replicates fitted from a tree with changes, and those whose commit or tree changed while
# their lane ran), each design's replicates, attempts, rejection rate (rejected draws over all
# draws), whether the real cell's own data pass the redraw rule (and its reasons if not; real
# past results are public, no forecast quantity; "not recorded" for a design with no
# replicate yet) and element counts, each group's 50% and 90% coverage against Decision 7's
# band (a count of 30 truths for the scalar groups, the mean over replicates of each
# replicate's share of the group's elements covered, within 3 standard errors, the sd over
# replicates over sqrt(n), of 0.5 and 0.9 for the vector groups; group_checks()), the stop
# rule's line for every replicate, a replicate that trips it counted all the same, and a last
# line RECOVERY PASS or RECOVERY FAIL. Those need Decision 7's set, the pre-registered seeds
# of DESIGNS (3001-3020 on 2024 eve and 3101-3110 on 2021 f180), each at 400 + 400, and no
# other; with any other set the line is RECOVERY INCOMPLETE and its reasons, naming the seeds
# missing or extra. It refuses replicates of another RECOVERY_VERSION, and replicates whose
# Stan file or config sha256 differs from the others', or whose real design differs within a
# cell, naming them. It prints the coverage lines and the verdict, never a forecast quantity.
#
# Every error starts "recovery: " and prints no absolute results directory (relative()).
if (is.null(box::name())) {
  if (!isTRUE(l10n_info()[["UTF-8"]])) stop("recovery: needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
  # WHY: the box path below is the working directory, so check it is the project root first.
  if (!file.exists("R/harness/constants.R")) stop("recovery: run from the project root (no R/harness/constants.R here)", call. = FALSE)
  options(box.path = normalizePath("."), width = 120, warn = 1)
}
box::use(
  R / harness / arms[arm],
  R / harness / compare_runs[stop_rule_at],
  R / harness / constants[project_root, relative],
  R / harness / freeze[forecast_free],
  R / harness / runs[
    SAMPLER, results_root, sha256_file, stan_data_sha, write_frozen_inputs, meta_mismatch, retire_run, measure_draws,
    run_provenance, chain_sampler, summarise_diagnostics
  ],
  R / harness / v2[V2_PARAM_VARIABLES]
)

PREFIX <- "recovery: "
USAGE <- paste(
  "usage: Rscript R/harness/recovery.R <E> <F> <first_seed> <n> <iter_warmup> <iter_sampling> [--lane=A|B]",
  "       Rscript R/harness/recovery.R report [out]",
  sep = "\n"
)

#' The arm whose recovery this is: its Stan file and config.
RECOVERY_ARM <- "v2"

#' The version of a replicate's outputs; a change makes every finished replicate stale. 2: the
#' redraw rule reads each closed election's national result, so a seed keeps other data than in 1.
RECOVERY_VERSION <- 2L

#' A present party's share, in a closed election's national result or as its mean over an
#' epoch's national polls, must lie in this range, or the replicate is redrawn (Decision 7).
SHARE_RANGE <- c(0.005, 0.70)

#' Attempt a of the replicate of seed s draws with seed SEED_STRIDE * s + a, a <= MAX_ATTEMPTS,
#' so no two attempts of any two replicates share a seed; the seed must stay within R's integers.
SEED_STRIDE <- 100000L
MAX_ATTEMPTS <- SEED_STRIDE - 1L
MAX_SEED <- (.Machine$integer.max - MAX_ATTEMPTS) %/% SEED_STRIDE

#' The truth check's tolerance, relative to max(1, |truth|).
TRUTH_TOL <- 1e-8

#' The columns of a replicate's ranks.csv and their types.
RANKS_COLUMNS <- readr::cols_only(
  group = "c", variable = "c", epoch = "i", kjordaemi = "c", flokkur = "c", truth = "d", q = "d", in50 = "l", in90 = "l"
)

#' The groups of Decision 7, in report order: `kind` scalar (one truth per replicate) or vector.
#' @export
GROUPS <- data.frame(
  group = c("tau_mu", "s_bar_E", "mu_open", "delta_closed", "delta_open"),
  kind = c("scalar", "scalar", "vector", "vector", "vector"),
  label = c(
    "\u03c4_\u03bc (tau_mu[1])", "s\u0304_E (s_bar_E[1])", "open-epoch \u03bc (mu[n_epoch, p])",
    "closed \u03b4 (delta[e, k, p], e <= n_closed)", "open \u03b4 (delta[n_epoch, k, p])"
  )
)

#' The intervals: each one's posterior quantiles and its nominal coverage.
#' @export
INTERVALS <- list(
  `50` = list(lo = 0.25, hi = 0.75, target = 0.5),
  `90` = list(lo = 0.05, hi = 0.95, target = 0.9)
)

#' Decision 7's set of replicates, pre-registered: seeds 3001-3020 on the 2024-eve design and
#' 3101-3110 on the 2021-f180 design (`first_seed` to `first_seed + n - 1`), each at 400 + 400.
#' @export
DESIGNS <- data.frame(
  cell = c("2024-11-30_2024-11-29", "2021-09-25_2021-03-29"),
  label = c("2024 eve", "2021 f180"),
  n = c(20L, 10L),
  first_seed = c(3001L, 3101L)
)
DESIGN_ITER <- c(iter_warmup = 400L, iter_sampling = 400L)

#' Decision 7's pass rule. Scalar groups, 30 truths each: at least 22 inside the 90% interval
#' and 8 to 22 inside the 50%. Vector groups: the mean over replicates of each replicate's share
#' of the group's elements covered within VECTOR_SE standard errors of the nominal coverage.
#' @export
SCALAR_TRUTHS <- 30L
SCALAR_BAND <- list(`50` = c(8L, 22L), `90` = c(22L, 30L))
VECTOR_SE <- 3

fail <- function(...) stop(PREFIX, ..., call. = FALSE)

#' The seed of attempt `attempt` of the replicate of seed `seed`.
#' @param seed the replicate's seed, a whole number in [1, MAX_SEED]
#' @param attempt the attempt, a whole number in [1, MAX_ATTEMPTS]
#' @return SEED_STRIDE * seed + attempt, an integer; an error outside those ranges
#' @export
attempt_seed <- function(seed, attempt) {
  ok <- function(x, hi) is.numeric(x) && length(x) == 1 && is.finite(x) && x == round(x) && x >= 1 && x <= hi
  if (!ok(seed, MAX_SEED)) fail("a replicate's seed must be a whole number in [1, ", MAX_SEED, "], not ", format(seed))
  if (!ok(attempt, MAX_ATTEMPTS)) fail("an attempt must be a whole number in [1, ", MAX_ATTEMPTS, "], not ", format(attempt))
  as.integer(SEED_STRIDE * seed + attempt)
}

#' A closed election's national result: its kjordaemi counts summed per party.
#' @param data a `v2_stan_data()` data list
#' @param e the closed election's index
#' @return a numeric vector of P counts, in party order
national_result <- function(data, e) colSums(matrix(data$y_res[e, , ], data$K, data$P))

#' The redraw rule of Decision 7 on simulated data: the reasons to redraw, none to keep it. A
#' simulated result is a closed election's national result (`national_result()`), and its
#' present parties are that election's reported parties (`idx_res`, the real cell's); a poll's
#' present parties are its row's (`idx_nat`). Whatever their simulated counts, so a reported
#' party drawing 0 votes is below the range. A party's share in a single kjordaemi is not read.
#' @param data a `v2_stan_data()` data list (simulated)
#' @param range the share range, SHARE_RANGE
#' @return a character vector, one element per closed election whose national result gives a
#'   present party a share outside `range` ("result <e>") and per epoch where a party's mean
#'   share over the national polls reporting it is outside `range` ("national polls epoch
#'   <e>"); `character(0)` when the data are kept
#' @export
redraw_reasons <- function(data, range = SHARE_RANGE) {
  out <- character(0)
  outside <- function(s) any(s < range[1] | s > range[2])
  for (e in seq_len(data$n_closed)) {
    S <- data$idx_res[e, seq_len(data$n_rep_res[e])]
    y <- national_result(data, e)
    if (outside(y[S] / sum(y))) out <- c(out, sprintf("result %d", e))
  }
  epoch <- data$grid_epoch[data$grid_nat]
  for (e in sort(unique(epoch))) {
    rows <- which(epoch == e)
    present <- matrix(FALSE, length(rows), data$P)
    for (i in seq_along(rows)) present[i, data$idx_nat[rows[i], seq_len(data$n_rep_nat[rows[i]])]] <- TRUE
    y <- data$y_nat[rows, , drop = FALSE]
    share <- y / rowSums(y)
    polls <- colSums(present)
    mean_share <- colSums(share * present)[polls > 0] / polls[polls > 0]
    if (outside(mean_share)) out <- c(out, sprintf("national polls epoch %d", e))
  }
  out
}

#' The redraw rule on the real cell's own data, as the report prints it: `redraw_reasons()`,
#' each "result <e>" given its election's date and the party and national share outside the
#' range (a public past result; no forecast quantity is read).
#' @param data the real cell's data (`v2_stan_data()`)
#' @param meta its meta: `party_order` and `closed_elections` are read
#' @param range the share range, SHARE_RANGE
#' @return a character vector, `character(0)` when the real data pass the rule
#' @export
real_rule_text <- function(data, meta, range = SHARE_RANGE) {
  vapply(redraw_reasons(data, range), \(r) {
    if (!startsWith(r, "result ")) {
      return(r)
    }
    e <- as.integer(sub("^result ", "", r))
    y <- national_result(data, e)
    S <- data$idx_res[e, seq_len(data$n_rep_res[e])]
    share <- y[S] / sum(y)
    off <- share < range[1] | share > range[2]
    paste0(r, " (", meta$closed_elections[e], "): ", paste(sprintf("%s %.3f%%", meta$party_order[S][off], 100 * share[off]), collapse = ", "))
  }, character(1), USE.NAMES = FALSE)
}

#' A replicate's truth and its data: v2_simulate() at attempt_seed(seed, a) for a = 1, 2, ...,
#' until redraw_reasons() keeps the data.
#' @param data the real cell's data (`v2_stan_data()`, draw_forecast 0)
#' @param seed the replicate's seed
#' @return `v2_simulate()`'s list plus `attempts` (the draws made, the kept one included) and
#'   `sim_seed` (the kept draw's seed); an error after MAX_ATTEMPTS rejected draws
#' @export
draw_replicate <- function(data, seed) {
  box::use(R / harness / v2_reference[v2_simulate])
  for (a in seq_len(MAX_ATTEMPTS)) {
    s <- attempt_seed(seed, a)
    sim <- v2_simulate(data, s)
    if (!length(redraw_reasons(sim$data))) {
      return(c(sim, list(attempts = a, sim_seed = s)))
    }
  }
  fail("seed ", seed, ": every one of ", MAX_ATTEMPTS, " draws was rejected by the redraw rule")
}

#' The truth elements of a replicate, in report order: tau_mu[1], s_bar_E[1], mu[n_epoch, p] for
#' each party, then delta[e, k, p] for each epoch, kjordaemi and party (closed epochs first).
#' @param sim a `draw_replicate()` (or `v2_simulate()`) list: `pars$tau_mu`, `pars$s_bar_E`,
#'   `truth$mu` (n_epoch x P) and `truth$delta` (n_epoch x K x P) are read
#' @param data the cell's data: `P`, `K`, `n_epoch` and `n_closed`
#' @param meta the cell's meta: `party_order` and `kjordaemi_levels`, the labels
#' @return a data frame: `group` (GROUPS$group), `variable` (the Stan element, e.g.
#'   "delta[1,2,3]"), `epoch`, `kjordaemi`, `flokkur` (NA where they do not apply) and `truth`
#' @export
truth_elements <- function(sim, data, meta) {
  P <- data$P
  K <- data$K
  n_epoch <- data$n_epoch
  scalar <- function(group, name, value) {
    data.frame(group = group, variable = paste0(name, "[1]"), epoch = NA_integer_, kjordaemi = NA_character_, flokkur = NA_character_, truth = value)
  }
  mu <- data.frame(
    group = "mu_open", variable = sprintf("mu[%d,%d]", n_epoch, seq_len(P)), epoch = n_epoch, kjordaemi = NA_character_,
    flokkur = meta$party_order, truth = as.numeric(sim$truth$mu[n_epoch, ])
  )
  cells <- expand.grid(p = seq_len(P), k = seq_len(K), e = seq_len(n_epoch))
  delta <- data.frame(
    group = ifelse(cells$e <= data$n_closed, "delta_closed", "delta_open"),
    variable = sprintf("delta[%d,%d,%d]", cells$e, cells$k, cells$p), epoch = cells$e,
    kjordaemi = meta$kjordaemi_levels[cells$k], flokkur = meta$party_order[cells$p],
    truth = sim$truth$delta[cbind(cells$e, cells$k, cells$p)]
  )
  out <- rbind(scalar("tau_mu", "tau_mu", sim$pars$tau_mu[1]), scalar("s_bar_E", "s_bar_E", sim$pars$s_bar_E[1]), mu, delta)
  if (anyNA(out$truth) || anyDuplicated(out$variable)) fail("a truth element is missing or named twice")
  out
}

#' The posterior quantile of each truth and whether its intervals cover it.
#' @param draws a matrix of draws, one column per element of `elements$variable` (named so)
#' @param elements a `truth_elements()` data frame
#' @return `elements` with `q` = mean(draws < truth) and `in50`, `in90` (INTERVALS, the
#'   bounds inside)
#' @export
rank_elements <- function(draws, elements) {
  lacks <- setdiff(elements$variable, colnames(draws))
  if (length(lacks)) fail("the draws lack ", paste(utils::head(lacks, 5), collapse = ", "))
  x <- draws[, elements$variable, drop = FALSE]
  elements$q <- colMeans(x < matrix(elements$truth, nrow(x), ncol(x), byrow = TRUE))
  for (i in names(INTERVALS)) {
    b <- INTERVALS[[i]]
    elements[[paste0("in", i)]] <- elements$q >= b$lo & elements$q <= b$hi
  }
  elements
}

#' Decision 7's coverage checks over a set of replicates.
#' @param ranks a data frame of the replicates' ranks: `replicate` (an id), `group`, `in50`,
#'   `in90`; a scalar group has one row per replicate
#' @return one row per group of GROUPS and interval ("50", "90"): `group`, `kind`, `interval`,
#'   `n` (replicates), `value` (a scalar group's count inside, a vector group's mean share),
#'   `se` (a vector group's sd over replicates over sqrt(n); NA for a scalar), `lo`, `hi` (the
#'   band) and `pass`: a scalar group's count within SCALAR_BAND when n is SCALAR_TRUTHS, NA
#'   for another n; a vector group's |value - target| <= VECTOR_SE * se, NA below 2 replicates
#' @export
group_checks <- function(ranks) {
  rows <- list()
  for (g in seq_len(nrow(GROUPS))) {
    r <- ranks[ranks$group == GROUPS$group[g], ]
    reps <- unique(r$replicate)
    n <- length(reps)
    for (i in names(INTERVALS)) {
      inside <- r[[paste0("in", i)]]
      if (GROUPS$kind[g] == "scalar") {
        if (anyDuplicated(r$replicate)) fail("scalar group ", GROUPS$group[g], " has more than one truth in a replicate")
        band <- SCALAR_BAND[[i]]
        value <- sum(inside)
        rows[[length(rows) + 1]] <- data.frame(
          group = GROUPS$group[g], kind = "scalar", interval = i, n = n, value = value, se = NA_real_, lo = band[1],
          hi = band[2], pass = if (n == SCALAR_TRUTHS) value >= band[1] && value <= band[2] else NA
        )
      } else {
        share <- vapply(reps, \(id) mean(inside[r$replicate == id]), numeric(1))
        target <- INTERVALS[[i]]$target
        value <- if (n) mean(share) else NA_real_
        se <- if (n >= 2) stats::sd(share) / sqrt(n) else NA_real_
        rows[[length(rows) + 1]] <- data.frame(
          group = GROUPS$group[g], kind = "vector", interval = i, n = n, value = value, se = se,
          lo = target - VECTOR_SE * se, hi = target + VECTOR_SE * se,
          pass = if (n >= 2) abs(value - target) <= VECTOR_SE * se else NA
        )
      }
    }
  }
  do.call(rbind, rows)
}

#' A design's pre-registered seeds.
#' @param i the design's row of DESIGNS
design_seeds <- function(i) DESIGNS$first_seed[i] + seq_len(DESIGNS$n[i]) - 1L

#' Whole numbers as runs: "3001\u20133003, 3005".
seed_runs <- function(x) {
  x <- sort(unique(as.integer(x)))
  starts <- x[c(TRUE, diff(x) != 1L)]
  ends <- x[c(diff(x) != 1L, TRUE)]
  paste(ifelse(starts == ends, starts, paste0(starts, "\u2013", ends)), collapse = ", ")
}

#' The reasons a set of replicates is not Decision 7's (DESIGNS's seeds at DESIGN_ITER, nothing
#' else).
#' @param reps a data frame, one row per replicate: `cell` ("<E>_<F>"), `seed`, `iter_warmup`,
#'   `iter_sampling`
#' @return a character vector, empty for Decision 7's set: per design, a count other than its
#'   `n`, its seeds missing and the seeds outside its pre-registered range, and replicates at
#'   other iterations; replicates on another cell
#' @export
incomplete_reasons <- function(reps) {
  out <- character(0)
  for (i in seq_len(nrow(DESIGNS))) {
    mine <- reps[reps$cell == DESIGNS$cell[i], , drop = FALSE]
    seeds <- design_seeds(i)
    if (nrow(mine) != DESIGNS$n[i]) out <- c(out, sprintf("%s: %d of %d replicates", DESIGNS$label[i], nrow(mine), DESIGNS$n[i]))
    missing <- setdiff(seeds, mine$seed)
    if (length(missing)) out <- c(out, sprintf("%s: seed(s) %s missing", DESIGNS$label[i], seed_runs(missing)))
    extra <- setdiff(mine$seed, seeds)
    if (length(extra)) out <- c(out, sprintf("%s: seed(s) %s outside %s", DESIGNS$label[i], seed_runs(extra), seed_runs(seeds)))
    off <- mine$iter_warmup != DESIGN_ITER[["iter_warmup"]] | mine$iter_sampling != DESIGN_ITER[["iter_sampling"]]
    if (any(off)) {
      out <- c(out, sprintf(
        "%s: %d replicates not at %d + %d", DESIGNS$label[i], sum(off), DESIGN_ITER[["iter_warmup"]], DESIGN_ITER[["iter_sampling"]]
      ))
    }
  }
  other <- setdiff(unique(reps$cell), DESIGNS$cell)
  if (length(other)) out <- c(out, paste("replicates on another cell:", paste(other, collapse = ", ")))
  out
}

#' The verdict on a set of replicates.
#' @param checks `group_checks()`
#' @param incomplete `incomplete_reasons()`
#' @return "PASS" or "FAIL" for Decision 7's set (FAIL when any check fails), "INCOMPLETE"
#'   otherwise
#' @export
recovery_verdict <- function(checks, incomplete) {
  if (length(incomplete)) {
    return("INCOMPLETE")
  }
  if (all(checks$pass %in% TRUE)) "PASS" else "FAIL"
}

#' Take the lock `path`, a symlink naming this process's pid. A lock whose process is no
#' longer running is taken over.
#' @return TRUE, or the pid holding it (an integer) when that process is running
#' @export
take_lock <- function(path) {
  for (i in 1:2) {
    if (suppressWarnings(file.symlink(as.character(Sys.getpid()), path))) {
      return(TRUE)
    }
    holder <- suppressWarnings(as.integer(Sys.readlink(path)))
    if (!is.na(holder) && holder != Sys.getpid() && isTRUE(tools::pskill(holder, 0L))) {
      return(holder)
    }
    unlink(path)
  }
  fail("could not take the lock ", path)
}

#' Release the lock `path` if this process holds it.
#' @export
release_lock <- function(path) {
  if (identical(Sys.readlink(path), as.character(Sys.getpid()))) unlink(path)
  invisible(NULL)
}

#' The keys of a replicate's meta.json that must match for it to be reused.
#' @param stan_file,config_file the arm's Stan file and config, hashed here
#' @param design_sha the sha256 of the real cell's data (`stan_data_sha()`, draw_forecast 0)
#' @param seed,iter_warmup,iter_sampling whole numbers
#' @return a named list: `stan_sha256`, `config_sha256`, `design_sha256`, `seed`,
#'   `iter_warmup`, `iter_sampling`, `chains`, `max_treedepth`, `init` (SAMPLER) and
#'   `recovery_version`
#' @export
expected_keys <- function(stan_file, config_file, design_sha, seed, iter_warmup, iter_sampling) {
  list(
    stan_sha256 = sha256_file(stan_file), config_sha256 = sha256_file(config_file), design_sha256 = design_sha,
    seed = as.integer(seed), iter_warmup = as.integer(iter_warmup), iter_sampling = as.integer(iter_sampling),
    chains = SAMPLER$chains, max_treedepth = SAMPLER$max_treedepth, init = SAMPLER$init, recovery_version = RECOVERY_VERSION
  )
}

#' The state of a replicate's folder, before its fit: claimed by `.<seed>.owner` beside it,
#' then reused, retired or new.
#' @param dir the replicate's folder, results_root()/recovery/<E>_<F>/<seed>
#' @param expected `expected_keys()`
#' @param label the name a retired folder is kept under in results_root()/_stale/
#' @return a list: `state` "skip" (meta.json matches `expected`; the claim is released), "busy"
#'   (another running process holds the claim: `pid`), "stale" (meta.json does not match:
#'   `keys`, the mismatched keys, and `to`, where it went), "partial" (no meta.json, a fit that
#'   stopped, or one that does not parse, its parser's message in `error`; `to`) or "new"; with
#'   `owner`, the claim's path, held for "stale", "partial" and "new"
#' @export
replicate_state <- function(dir, expected, label) {
  dir.create(dirname(dir), recursive = TRUE, showWarnings = FALSE)
  owner <- file.path(dirname(dir), paste0(".", basename(dir), ".owner"))
  held <- take_lock(owner)
  if (!isTRUE(held)) {
    return(list(state = "busy", pid = held))
  }
  meta_path <- file.path(dir, "meta.json")
  if (file.exists(meta_path)) {
    finished <- tryCatch(jsonlite::read_json(meta_path), error = function(e) e)
    if (inherits(finished, "error")) {
      return(list(state = "partial", error = conditionMessage(finished), to = retire_run(dir, label), owner = owner))
    }
    keys <- meta_mismatch(finished, expected)
    if (!length(keys)) {
      release_lock(owner)
      return(list(state = "skip"))
    }
    return(list(state = "stale", keys = keys, to = retire_run(dir, label), owner = owner))
  }
  if (dir.exists(dir)) {
    return(list(state = "partial", to = retire_run(dir, label), owner = owner))
  }
  list(state = "new", owner = owner)
}

#' The model of `stan_file`, compiled from a copy in the lane's folder: each lane compiles its
#' own, so two lanes never write one executable, and a lane reuses its copy across invocations
#' while the Stan file's sha256 is unchanged.
lane_model <- function(lane_dir, stan_file) {
  sha <- sha256_file(stan_file)
  dir <- file.path(lane_dir, paste0("model-", substr(sha, 1, 16)))
  copy <- file.path(dir, basename(stan_file))
  if (!file.exists(copy)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(stan_file, copy)) fail("could not copy ", stan_file, " to ", copy)
  }
  if (!identical(sha256_file(copy), sha)) fail("the lane's copy of ", basename(stan_file), " does not hash as the file: delete ", dir)
  cmdstanr::cmdstan_model(copy)
}

#' The Stan model's quantities of the truth elements' names at the true parameters: a
#' one-iteration fixed_param run of `model` started at `pars`, read back at 18 significant
#' figures, in `dir` (deleted after); a named vector, for truth_check().
truth_stan_values <- function(model, data_json, pars, elements, dir) {
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # WHY: CmdStan's init reader refuses a zero-size parameter written as [], and there is nothing to initialise in it.
  withr::local_options(cmdstanr_warn_inits = FALSE)
  fit <- model$sample(
    data = data_json, fixed_param = TRUE, chains = 1, iter_warmup = 0, iter_sampling = 1, adapt_engaged = FALSE, seed = 1,
    init = list(pars[lengths(pars) > 0]), sig_figs = 18, output_dir = dir, refresh = 0, show_messages = FALSE
  )
  if (!all(fit$return_codes() == 0)) fail("the truth check's fixed_param run failed")
  x <- unclass(posterior::as_draws_matrix(fit$draws(variables = unique(sub("\\[.*$", "", elements$variable)))))
  x[1, ]
}

#' The truth check: each truth element against the Stan model's quantity of the same name.
#' @param got the Stan model's values at the true parameters, named by Stan element
#'   (`truth_stan_values()`); paired with `elements` by name, in any order
#' @param elements a `truth_elements()` data frame
#' @param id the replicate's id, the error's prefix
#' @return the largest error, relative to max(1, |truth|); an error naming the elements (up to
#'   five, the largest first) whose error exceeds TRUTH_TOL, and one if an element is missing or
#'   a value is not finite
#' @export
truth_check <- function(got, elements, id = "the truth check") {
  lacks <- setdiff(elements$variable, names(got))
  if (length(lacks)) fail(id, ": the fixed_param run lacks ", paste(utils::head(lacks, 5), collapse = ", "))
  err <- abs(got[elements$variable] - elements$truth) / pmax(1, abs(elements$truth))
  if (!all(is.finite(err))) fail(id, ": the truth check read a non-finite value")
  bad <- order(err, decreasing = TRUE)[seq_len(min(5L, sum(err > TRUTH_TOL)))]
  if (length(bad)) {
    fail(
      id, ": the Stan model's quantities at the true parameters differ from the truth by more than ", TRUTH_TOL, ": ",
      paste(sprintf("%s by %s", elements$variable[bad], format(err[bad], digits = 3)), collapse = ", ")
    )
  }
  max(err)
}

#' Write `x` as JSON in `path`, read-only, as a harness run writes its meta.json.
write_json_file <- function(x, path) {
  # WHY: jsonlite writes a NULL as {} by default; null = "null" keeps it NULL. na = "string" keeps a non-finite number apart from null.
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "string", null = "null")
}

#' The lane's log line: printed, and appended to the lane's log with the time.
say <- function(log, ...) {
  line <- paste0(...)
  cat(line, "\n", sep = "")
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S "), line, "\n", sep = "", file = log, append = TRUE)
}

#' Fit one replicate (steps 2-5 of the header) into `dir`, claimed by `state`.
fit_replicate <- function(cell, seed, iters, expected, model, log) {
  id <- sprintf("recovery_%s_%s_s%d", cell$E, cell$F, seed)
  dir <- file.path(results_root(), "recovery", paste0(cell$E, "_", cell$F), seed)
  state <- replicate_state(dir, expected, id)
  switch(state$state,
    skip = {
      say(log, "SKIP ", id, " (meta.json matches)")
      return(invisible("skip"))
    },
    busy = {
      say(log, "BUSY ", id, " (pid ", state$pid, " holds it)")
      return(invisible("busy"))
    },
    stale = say(log, "STALE ", id, " ", paste(state$keys, collapse = ","), "; retired to ", relative(state$to)),
    partial = say(
      log, "STALE ", id, if (is.null(state$error)) " (no meta.json)" else paste0(" (meta.json does not parse: ", state$error, ")"),
      "; retired to ", relative(state$to)
    )
  )
  on.exit(release_lock(state$owner), add = TRUE)
  say(log, "START ", id)
  if (!dir.create(dir)) fail("could not create ", dir)

  sim <- draw_replicate(cell$data, seed)
  elements <- truth_elements(sim, cell$data, cell$meta)
  inputs_meta <- c(
    list(
      id = id, E = cell$E, F = cell$F, seed = seed, sim_seed = sim$sim_seed, attempts = sim$attempts,
      design_sha256 = expected$design_sha256, party_order = I(cell$meta$party_order),
      kjordaemi_levels = I(cell$meta$kjordaemi_levels), closed_elections = I(cell$meta$closed_elections)
    ),
    cell$provenance
  )
  data_sha <- write_frozen_inputs(dir, sim$data, inputs_meta)
  data_json <- file.path(dir, "inputs", "stan_data.json")
  say(log, sprintf("TRUTH %s seed %d after %d attempt(s), %d rejected", id, sim$sim_seed, sim$attempts, sim$attempts - 1L))
  # WHY: theta and pi_nat are left out of the truth file: they hold the simulated latent shares, and no forecast-like quantity is written.
  natural <- sim$truth[c("mu", "gamma", "delta", "eps", "log_phi", "log_phi_K", "log_kappa", "s_delta0", "s_zeta")]
  truth_path <- file.path(dir, "truth.json")
  write_json_file(
    list(id = id, seed = seed, sim_seed = sim$sim_seed, attempts = sim$attempts, pars = sim$pars, natural = natural),
    truth_path
  )

  check_err <- truth_check(truth_stan_values(model, data_json, sim$pars, elements, file.path(dir, "check")), elements, id)

  cmdstan_dir <- file.path(dir, "cmdstan")
  dir.create(cmdstan_dir)
  start <- Sys.time()
  fit <- model$sample(
    data = data_json, seed = seed, chains = SAMPLER$chains, parallel_chains = SAMPLER$parallel_chains, init = SAMPLER$init,
    max_treedepth = SAMPLER$max_treedepth, iter_warmup = iters[["iter_warmup"]], iter_sampling = iters[["iter_sampling"]],
    refresh = 50, output_dir = cmdstan_dir
  )
  minutes <- as.numeric(difftime(Sys.time(), start, units = "mins"))
  lp <- fit$draws("lp__")
  if (!all(fit$return_codes() == 0) || posterior::nchains(lp) != SAMPLER$chains ||
    posterior::ndraws(lp) != SAMPLER$chains * iters[["iter_sampling"]]) {
    fail(id, ": the fit does not hold ", SAMPLER$chains, " chains of ", iters[["iter_sampling"]], " draws; no meta.json written")
  }
  if (!identical(sha256_file(cell$stan_file), expected$stan_sha256)) fail(id, ": the Stan file changed during the fit; no meta.json written")
  if (!identical(sha256_file(data_json), data_sha)) fail(id, ": the simulated data changed during the fit; no meta.json written")

  scored <- posterior::subset_draws(fit$draws(variables = c("tau_mu", "s_bar_E", "mu", "delta")), variable = elements$variable)
  ranks <- rank_elements(unclass(posterior::as_draws_matrix(scored)), elements)
  # WHY: a zero-size parameter (V2's with its switch off) has no column in the CSVs, and fit$draws() refuses it.
  params <- fit$draws(variables = intersect(names(model$variables()$parameters), fit$metadata()$stan_variables))
  sampler <- fit$diagnostic_summary(quiet = TRUE)
  d <- summarise_diagnostics(params, scored, c(
    list(num_divergent = sampler$num_divergent, num_max_treedepth = sampler$num_max_treedepth, ebfmi = sampler$ebfmi),
    chain_sampler(fit$sampler_diagnostics())
  ))
  pd <- tibble::as_tibble(posterior::as_draws_df(fit$draws(variables = intersect(cell$param_variables, fit$metadata()$stan_variables))))
  pd <- pd[c(".chain", ".iteration", ".draw", setdiff(names(pd), c(".chain", ".iteration", ".draw")))]
  key <- rbind(measure_draws(scored), measure_draws(posterior::as_draws_df(pd)))
  key <- rbind(key[!duplicated(key$variable), ], measure_draws(lp))

  end <- run_provenance()[c("git_head", "git_dirty")]
  if (!identical(end$git_head, cell$provenance$git_head) || !identical(end$git_dirty, cell$provenance$git_dirty)) {
    say(
      log, "PROVENANCE ", id, " changed while the lane ran: git_head ", paste(cell$provenance$git_head, collapse = ""), " at lane start, ",
      paste(end$git_head, collapse = ""), " now; git_dirty ", cell$provenance$git_dirty, " at lane start, ", end$git_dirty, " now"
    )
  }

  paths <- file.path(dir, c("ranks.csv", "param_draws.parquet", "key_diagnostics.csv"))
  readr::write_csv(ranks, paths[1])
  arrow::write_parquet(pd, paths[2])
  readr::write_csv(key[c("variable", "rhat", "ess_bulk", "ess_tail")], paths[3])
  meta <- c(
    list(id = id, arm = RECOVERY_ARM, role = "recovery", E = cell$E, F = cell$F, lane = cell$lane),
    expected,
    list(
      data_sha256 = data_sha, sim_seed = sim$sim_seed, attempts = sim$attempts, rejected = sim$attempts - 1L,
      truth_check_error = check_err, elements = as.list(table(factor(elements$group, GROUPS$group))), minutes = minutes,
      diagnostics = d, real_cell_redraw = I(cell$real_redraw)
    ),
    cell$provenance,
    list(git_head_end = end$git_head, git_dirty_end = end$git_dirty)
  )
  tmp <- tempfile("meta-", tmpdir = dir, fileext = ".json")
  write_json_file(meta, tmp)
  if (!all(Sys.chmod(c(paths, truth_path, tmp), mode = "0444"))) fail("could not make ", id, "'s outputs read-only")
  if (!file.rename(tmp, file.path(dir, "meta.json"))) fail("could not move ", tmp, " to meta.json")
  if (!identical(Sys.getenv("HARNESS_KEEP_CMDSTAN"), "1")) unlink(cmdstan_dir, recursive = TRUE)

  rule <- stop_rule_at(dir, jsonlite::read_json(file.path(dir, "meta.json"), simplifyVector = TRUE), id)
  say(log, rule$line)
  say(log, "DONE ", relative(dir))
  invisible("done")
}

#' The replicates of one invocation: `n` seeds from `first_seed` on the cell (E, F), in `lane`.
run_replicates <- function(E, F, first_seed, n, iter_warmup, iter_sampling, lane) {
  box::use(R / harness / info_set[info_set, validate_inputs])
  if (forecast_free(E)) fail("2017 is not fitted before the freeze (spec \u00a75.7)")
  base <- file.path(results_root(), "recovery")
  lane_dir <- file.path(base, "_lanes", lane)
  dir.create(lane_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base, "logs"), showWarnings = FALSE)
  log <- file.path(base, "logs", paste0("lane-", lane, ".log"))
  lock <- file.path(lane_dir, "lock")
  held <- take_lock(lock)
  if (!isTRUE(held)) fail("lane ", lane, " is running (pid ", held, "): give this one --lane=", setdiff(c("A", "B"), lane))
  on.exit(release_lock(lock), add = TRUE)

  # WHY: the code a replicate runs is loaded by now (v2_reference and stage1 would otherwise load at their first call), so this is its provenance.
  box::use(R / harness / v2_reference[v2_simulate], R / harness / stage1[stage1_mu_f])
  provenance <- run_provenance()
  a <- arm(RECOVERY_ARM)
  stan_file <- file.path(project_root(), a$stan_file)
  config_file <- file.path(project_root(), a$config)
  info <- info_set(E, F)
  validate_inputs(info)
  cfg <- a$read_config(config_file)
  built <- a$build(info, cfg)
  data <- built$data
  data$draw_forecast <- 0L
  design_sha <- stan_data_sha(data)
  cell <- list(
    E = format(E), F = format(F), lane = lane, data = data, meta = built$meta, stan_file = stan_file,
    param_variables = intersect(V2_PARAM_VARIABLES, a$variables(cfg)), real_redraw = real_rule_text(data, built$meta),
    provenance = provenance
  )
  say(log, sprintf(
    "LANE %s %s_%s seeds %d..%d at %d + %d (pid %d); git_head %s, git_dirty %s", lane, cell$E, cell$F, first_seed, first_seed + n - 1,
    iter_warmup, iter_sampling, Sys.getpid(), paste(provenance$git_head, collapse = ""), provenance$git_dirty
  ))
  say(log, "REAL ", cell$E, "_", cell$F, " under the redraw rule: ", real_text(cell$real_redraw))
  model <- lane_model(lane_dir, stan_file)
  iters <- c(iter_warmup = iter_warmup, iter_sampling = iter_sampling)
  for (seed in seq(first_seed, length.out = n)) {
    attempt_seed(seed, 1)
    fit_replicate(cell, seed, iters, expected_keys(stan_file, config_file, design_sha, seed, iter_warmup, iter_sampling), model, log)
  }
  say(log, "LANE ", lane, " FINISHED")
  0L
}

#' Every finished replicate under `base` (results_root()/recovery): a list of
#' `list(id, cell, dir, meta, ranks)`, and the number of folders without a meta.json.
read_replicates <- function(base) {
  cells <- list.files(base, pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  reps <- list()
  partial <- 0L
  for (cell in cells) {
    seeds <- list.files(file.path(base, cell), pattern = "^[0-9]+$")
    for (seed in seeds[order(as.integer(seeds))]) {
      dir <- file.path(base, cell, seed)
      if (!file.exists(file.path(dir, "meta.json"))) {
        partial <- partial + 1L
        next
      }
      meta <- tryCatch(jsonlite::read_json(file.path(dir, "meta.json"), simplifyVector = TRUE), error = function(e) {
        fail("the meta.json in ", dir, " does not parse (", conditionMessage(e), "): re-run its lane to retire it")
      })
      ranks <- as.data.frame(readr::read_csv(file.path(dir, "ranks.csv"), col_types = RANKS_COLUMNS, progress = FALSE))
      if (!identical(as.character(meta$id), sprintf("recovery_%s_s%s", cell, seed))) fail("the meta.json in ", dir, " is not of replicate ", cell, " ", seed)
      reps[[length(reps) + 1]] <- list(id = meta$id, cell = cell, dir = dir, meta = meta, ranks = ranks)
    }
  }
  list(reps = reps, partial = partial)
}

#' Stop, naming them, if any replicates' `key` differs from the most common value among `reps`.
same_key <- function(reps, key, what) {
  values <- vapply(reps, \(r) as.character(r$meta[[key]]), character(1))
  common <- names(sort(table(values), decreasing = TRUE))[1]
  odd <- values != common
  if (any(odd)) {
    fail(
      "replicate(s) ", paste(vapply(reps[odd], \(r) r$id, character(1)), collapse = ", "), " have another ", what,
      " sha256 than the rest (", substr(common, 1, 12), "...): retire them or refit"
    )
  }
  common
}

shown <- function(x, digits = 3) if (is.na(x)) "\u2013" else formatC(x, format = "f", digits = digits)

#' `real_rule_text()`'s reasons as one cell of text: "kept", or "redrawn: " and the reasons.
real_text <- function(reasons) {
  reasons <- unlist(reasons)
  if (length(reasons)) paste0("redrawn: ", paste(reasons, collapse = "; ")) else "kept"
}
table_row <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")

#' Write the report (header) to `out`; returns the exit status.
run_report <- function(out) {
  base <- file.path(results_root(), "recovery")
  read <- read_replicates(base)
  reps <- read$reps
  if (!length(reps)) fail("no finished replicate under ", relative(base))
  ids <- vapply(reps, \(r) r$id, character(1))
  version <- vapply(reps, \(r) if (is.null(r$meta$recovery_version)) NA_integer_ else as.integer(r$meta$recovery_version), integer(1))
  old <- !(version %in% RECOVERY_VERSION)
  if (any(old)) {
    fail(
      "replicate(s) ", paste(ids[old], collapse = ", "), " were drawn under recovery_version ", paste(unique(version[old]), collapse = ", "),
      ", not the current ", RECOVERY_VERSION, ": retire them or refit"
    )
  }
  stan_sha <- same_key(reps, "stan_sha256", "Stan file")
  config_sha <- same_key(reps, "config_sha256", "config")
  for (cell in unique(vapply(reps, \(r) r$cell, character(1)))) {
    same_key(Filter(\(r) r$cell == cell, reps), "design_sha256", paste0("real design (", cell, ")"))
  }
  rules <- lapply(reps, \(r) stop_rule_at(r$dir, r$meta, r$id, r$cell))
  ranks <- do.call(rbind, lapply(reps, \(r) cbind(replicate = r$id, cell = r$cell, r$ranks)))
  table_of <- data.frame(
    cell = vapply(reps, \(r) r$cell, character(1)),
    seed = vapply(reps, \(r) as.integer(r$meta$seed), integer(1)),
    iter_warmup = vapply(reps, \(r) as.integer(r$meta$iter_warmup), integer(1)),
    iter_sampling = vapply(reps, \(r) as.integer(r$meta$iter_sampling), integer(1)),
    attempts = vapply(reps, \(r) as.integer(r$meta$attempts), integer(1)),
    minutes = vapply(reps, \(r) as.numeric(r$meta$minutes), numeric(1)),
    stop = vapply(rules, \(r) length(r$reasons) > 0, logical(1))
  )
  checks <- group_checks(ranks)
  incomplete <- incomplete_reasons(table_of)
  verdict <- recovery_verdict(checks, incomplete)
  label <- function(cell) {
    i <- match(cell, DESIGNS$cell)
    if (is.na(i)) sub("_", " F ", cell) else DESIGNS$label[i]
  }
  rep_cells <- c(intersect(DESIGNS$cell, table_of$cell), setdiff(unique(table_of$cell), DESIGNS$cell))
  cells <- c(DESIGNS$cell, setdiff(rep_cells, DESIGNS$cell))
  real <- vapply(cells, \(cell) {
    mine <- Filter(\(r) r$cell == cell, reps)
    if (!length(mine)) {
      "not recorded (no replicate yet)"
    } else if (is.null(mine[[1]]$meta$real_cell_redraw)) {
      "not recorded"
    } else {
      real_text(mine[[1]]$meta$real_cell_redraw)
    }
  }, character(1))
  heads <- unique(unlist(lapply(reps, \(r) r$meta$git_head)))
  dirty <- ids[vapply(reps, \(r) isTRUE(r$meta$git_dirty), logical(1))]
  moved <- unlist(lapply(reps, \(r) {
    m <- r$meta
    if (is.null(m$git_head_end) || is.null(m$git_dirty_end)) {
      return(paste0(r$id, " (not recorded)"))
    }
    if (identical(m$git_head_end, m$git_head) && identical(m$git_dirty_end, m$git_dirty)) {
      return(NULL)
    }
    sprintf(
      "%s (git_head %s to %s, git_dirty %s to %s)", r$id, substr(paste(m$git_head, collapse = ""), 1, 12),
      substr(paste(m$git_head_end, collapse = ""), 1, 12), m$git_dirty, m$git_dirty_end
    )
  }))
  listed <- function(x, none) if (length(x)) paste0(length(x), ": ", paste(x, collapse = "; ")) else none
  pinned <- paste(vapply(seq_len(nrow(DESIGNS)), \(i) paste("seeds", seed_runs(design_seeds(i)), "on", DESIGNS$label[i]), character(1)), collapse = " and ")
  cmdstan <- unique(vapply(reps, \(r) as.character(r$meta$cmdstan_version), character(1)))
  a <- arm(RECOVERY_ARM)

  lines <- c(
    "# V2 recovery",
    "",
    paste0(
      "Written by `LC_ALL=en_US.UTF-8 Rscript R/harness/recovery.R report`, from the replicates of ",
      "`R/harness/recovery.R <E> <F> <first_seed> <n> <iter_warmup> <iter_sampling>` (the script's header gives the protocol). ",
      "Each replicate's truth is one draw of V2's prior and its data a draw of the model on the real cell's design; ",
      "no forecast quantity is computed."
    ),
    "",
    paste0("- Stan file `", a$stan_file, "` (sha256 `", stan_sha, "`), config `", a$config, "` (sha256 `", config_sha, "`)"),
    paste0("- Commit(s) at lane start: ", paste0("`", heads, "`", collapse = ", ")),
    paste0("- Replicates fitted from a tree with changes (at lane start): ", listed(dirty, "0")),
    paste0("- Replicates whose commit or tree changed while their lane ran: ", listed(moved, "none")),
    paste0("- CmdStan ", paste(cmdstan, collapse = ", "), "; ", SAMPLER$chains, " chains, max_treedepth ", SAMPLER$max_treedepth, ", init ", SAMPLER$init),
    paste0(
      "- Redraw rule: a replicate is redrawn when a reported party's share of a closed election's national result ",
      "(its kj\u00f6rd\u00e6mi counts summed), or its mean share over an epoch's national polls, leaves [",
      100 * SHARE_RANGE[1], "%, ", 100 * SHARE_RANGE[2], "%]; the rejection rate is the rejected draws over all draws. ",
      "\"Real cell\" says whether the real cell's own data pass the rule"
    ),
    paste0("- Folders without a meta.json (not counted): ", read$partial),
    "",
    "## Designs",
    "",
    table_row(c(
      "Design", "Replicates", "Seeds", "Iterations", "Draws", "Rejected", "Rejection rate", "Minutes (median, max)", "Stop rule tripped",
      "Real cell"
    )),
    table_row(rep("---", 10))
  )
  for (cell in cells) {
    t <- table_of[table_of$cell == cell, ]
    if (!nrow(t)) {
      lines <- c(lines, table_row(c(label(cell), 0, rep("\u2013", 2), 0, 0, rep("\u2013", 2), 0, real[[cell]])))
      next
    }
    iters <- unique(paste(t$iter_warmup, "+", t$iter_sampling))
    lines <- c(lines, table_row(c(
      label(cell), nrow(t), paste0(min(t$seed), "\u2013", max(t$seed)), paste(iters, collapse = ", "), sum(t$attempts),
      sum(t$attempts - 1L), shown(sum(t$attempts - 1L) / sum(t$attempts), 4),
      paste0(shown(stats::median(t$minutes), 1), ", ", shown(max(t$minutes), 1)), sum(t$stop), real[[cell]]
    )))
  }
  lines <- c(
    lines, "", "Truth elements per replicate:", "",
    table_row(c("Group", vapply(rep_cells, label, character(1)))),
    table_row(rep("---", 1 + length(rep_cells)))
  )
  for (g in seq_len(nrow(GROUPS))) {
    counts <- vapply(rep_cells, \(cell) {
      per <- table(ranks$replicate[ranks$cell == cell & ranks$group == GROUPS$group[g]])
      if (length(unique(per)) > 1) fail("the replicates of ", cell, " differ in their count of ", GROUPS$group[g])
      if (length(per)) as.integer(per[1]) else 0L
    }, integer(1))
    lines <- c(lines, table_row(c(GROUPS$label[g], counts)))
  }
  lines <- c(
    lines, "",
    "## Coverage",
    "",
    paste0(
      "A truth is inside the 90% interval when its posterior quantile q = mean(draws < truth) is in [0.05, 0.95], and inside ",
      "the 50% when q is in [0.25, 0.75]. Scalar groups: the count of truths inside, against Decision 7's band for ",
      SCALAR_TRUTHS, " truths (90%: at least ", SCALAR_BAND[["90"]][1], "; 50%: ", SCALAR_BAND[["50"]][1], " to ",
      SCALAR_BAND[["50"]][2], "). Vector groups: the mean over replicates of each replicate's share of the group's elements ",
      "inside, against the nominal coverage \u00b1 ", VECTOR_SE, " standard errors (the sd over replicates over \u221an)."
    ),
    "",
    table_row(c("Group", "Kind", "Replicates", "50% inside", "50% band", "50%", "90% inside", "90% band", "90%")),
    table_row(rep("---", 9))
  )
  mark <- function(p) if (is.na(p)) "\u2013" else if (p) "PASS" else "FAIL"
  for (g in seq_len(nrow(GROUPS))) {
    cs <- checks[checks$group == GROUPS$group[g], ]
    cell_text <- function(i) {
      r <- cs[cs$interval == i, ]
      if (r$kind == "scalar") {
        c(paste0(r$value, " of ", r$n), paste0("[", r$lo, ", ", r$hi, "]"), mark(r$pass))
      } else {
        c(paste0(shown(r$value), " (SE ", shown(r$se), ")"), paste0("[", shown(r$lo), ", ", shown(r$hi), "]"), mark(r$pass))
      }
    }
    lines <- c(lines, table_row(c(GROUPS$label[g], GROUPS$kind[g], cs$n[1], cell_text("50"), cell_text("90"))))
  }
  lines <- c(
    lines, "",
    "## Stop rule",
    "",
    paste0(
      "Decision 5's stop rule on each replicate (`R/harness/compare_runs.R`'s `stop_rule_at()`; its gate-scope R-hat over the ",
      "parameters and the truth elements). A replicate that trips it stays in every coverage count."
    ),
    "",
    "```",
    vapply(rules, \(r) r$line, character(1)),
    "```",
    "",
    "## Verdict",
    "",
    if (length(incomplete)) paste0("Not Decision 7's set (", pinned, ", each at 400 + 400): ", paste(incomplete, collapse = "; "), ".") else NULL,
    if (length(incomplete)) "" else NULL,
    paste0("RECOVERY ", verdict)
  )
  tmp <- tempfile("recovery-", tmpdir = dirname(out), fileext = ".md")
  writeLines(lines, tmp, useBytes = TRUE)
  if (!file.rename(tmp, out)) fail("could not write ", out)
  for (i in seq_len(nrow(checks))) {
    r <- checks[i, ]
    cat(sprintf(
      "COVERAGE %s %s%% %s band [%s, %s] %s\n", r$group, r$interval,
      if (r$kind == "scalar") paste0(r$value, "/", r$n) else shown(r$value),
      if (r$kind == "scalar") r$lo else shown(r$lo), if (r$kind == "scalar") r$hi else shown(r$hi), mark(r$pass)
    ))
  }
  for (cell in cells) cat("REAL ", label(cell), " under the redraw rule: ", real[[cell]], "\n", sep = "")
  if (length(incomplete)) cat("incomplete: ", paste(incomplete, collapse = "; "), "\n", sep = "")
  cat("RECOVERY ", verdict, "\n", sep = "")
  cat("wrote ", relative(normalizePath(out)), "\n", sep = "")
  0L
}

main <- function(args) {
  whole <- function(x, lo) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) != 1 || is.na(v) || v != round(v) || v < lo) fail(USAGE)
    as.integer(v)
  }
  tryCatch(
    {
      if (length(args) && args[1] == "report") {
        if (length(args) > 2) fail(USAGE)
        out <- if (length(args) == 2) args[2] else file.path(project_root(), "harness", "v2", "recovery.md")
        if (!dir.exists(dirname(out))) fail("no directory for ", out)
        return(run_report(out))
      }
      lane <- "A"
      flags <- startsWith(args, "--")
      for (f in args[flags]) {
        if (!f %in% c("--lane=A", "--lane=B")) fail(USAGE)
        lane <- sub("^--lane=", "", f)
      }
      pos <- args[!flags]
      if (length(pos) != 6) fail(USAGE)
      day <- function(x) {
        d <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
        if (is.na(d) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) fail(USAGE)
        d
      }
      E <- day(pos[1])
      F <- day(pos[2])
      first <- whole(pos[3], 1)
      n <- whole(pos[4], 1)
      if (first + n - 1 > MAX_SEED) fail("the last seed must be at most ", MAX_SEED)
      run_replicates(E, F, first, n, whole(pos[5], 1), whole(pos[6], 1), lane)
    },
    error = function(e) {
      message <- relative(conditionMessage(e))
      stop(if (startsWith(message, PREFIX)) "" else PREFIX, message, call. = FALSE)
    }
  )
}

if (is.null(box::name())) quit(save = "no", status = main(commandArgs(trailingOnly = TRUE)))
