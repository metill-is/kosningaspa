# The election-level reading of a Fylgisvakt backtest. For each election E it takes two
# leave-future-out fits written by R/sampler_check/fit_polling_watch_headless.R --cutoff=E, a
# reference model and a new one, and scores them as R/sampler_check/score_polling_watch_backtest.R
# does: the smoothed shares pi_smooth at each fit's last date against the official national
# result at E (harness/truth/elections_official.csv), CRPS10 in pp averaged over the fitted
# parties. Per election it gives the paired difference CRPS10(new) - CRPS10(ref) and its
# outcome noise (outcome_noise() in R/harness/scores.R), and reads it alone; over the
# elections, each weighted equally, election_reading() (R/harness/score_report.R) reads the
# mean difference as better, worse or indistinguishable against twice the combined noise.
#
# The two fits of an election must have finished (DONE), read the same data (timing.rds
# data_md5), end on the same date, after the previous official result and before E, and fit
# the same parties; a fit that built its data with a cutoff must have used E. The script
# stops on a missing file, on any of these mismatches, on pi_smooth draws whose shape does not
# match meta.rds, and on E = 2017-10-28 while harness/DECISION.md does not exist.
#
# Usage, from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/election_reading_polling_watch.R \
#     <E> <reference fit dir> <new fit dir> [<E> <reference fit dir> <new fit dir> ...]
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(width = 160, warn = 1)
suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
})
box::use(
  R / harness / scores[crps_draws, outcome_noise],
  R / harness / score_report[election_reading]
)

fail <- function(...) stop("election_reading_polling_watch: ", ..., call. = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0 || length(args) %% 3 != 0) {
  fail("give one or more triples <E> <reference fit dir> <new fit dir>")
}
spec <- data.frame(
  E = args[seq(1, length(args), 3)], ref = args[seq(2, length(args), 3)], new = args[seq(3, length(args), 3)]
)
E_dates <- as.Date(spec$E, optional = TRUE)
if (anyNA(E_dates)) fail("not a date: ", paste(spec$E[is.na(E_dates)], collapse = ", "))
if (anyDuplicated(E_dates)) fail("an election is given twice")
# WHY: 2017 is the v2 harness's held-out confirmatory target, as in score_polling_watch_backtest.R
if (any(E_dates == as.Date("2017-10-28")) && !file.exists("harness/DECISION.md")) {
  fail("2017 is held out until harness/DECISION.md exists")
}

official <- read.csv("harness/truth/elections_official.csv", fileEncoding = "UTF-8")
official$election <- as.Date(official$election)
truth_at <- function(E) {
  t <- official[official$election == E, ]
  if (!nrow(t)) fail("no official result at ", format(E))
  votes <- tapply(t$votes, t$flokkur, sum)
  setNames(as.numeric(votes) / sum(votes), tolower(names(votes)))
}

# One fit's shares at its last date: a draw x party matrix (shares 0-1, columns the fitted
# parties), with the fit's last date, data md5 and Stan file
read_fit <- function(dir, E) {
  need <- file.path(dir, c("DONE", "meta.rds", "timing.rds"))
  if (!all(file.exists(need))) fail(dir, ": missing ", paste(basename(need[!file.exists(need)]), collapse = ", "))
  files <- list.files(file.path(dir, "cmdstan"), pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) fail(dir, ": no CmdStan CSV in cmdstan/")
  meta <- readRDS(file.path(dir, "meta.rds"))
  timing <- readRDS(file.path(dir, "timing.rds"))
  if (is.null(meta$date_mapping) || is.null(meta$party_names)) fail(dir, ": meta.rds lacks date_mapping or party_names")
  D <- nrow(meta$date_mapping)
  last_date <- meta$date_mapping$date[D]
  if (!is.na(timing$cutoff) && as.Date(timing$cutoff) != E) {
    fail(dir, ": fitted with --cutoff=", timing$cutoff, ", not ", format(E))
  }
  if (last_date >= E) fail(dir, ": the fit saw ", format(E), " or a later date (last date ", format(last_date), ")")
  previous <- official$election[official$election < E]
  if (length(previous) && last_date <= max(previous)) {
    fail(dir, ": the fit ends on ", format(last_date), ", not after the previous result (", format(max(previous)), ")")
  }
  draws <- as_draws_matrix(read_cmdstan_csv(files, variables = "pi_smooth")$post_warmup_draws)
  idx <- do.call(rbind, lapply(strsplit(gsub("^pi_smooth\\[|\\]$", "", variables(draws)), ","), as.integer))
  P <- length(meta$party_names)
  if (max(idx[, 1]) != D || max(idx[, 2]) != P) {
    fail(dir, ": pi_smooth is ", max(idx[, 1]), " x ", max(idx[, 2]), " but meta.rds has ", D, " dates and ", P, " parties")
  }
  X <- unclass(draws[, paste0("pi_smooth[", D, ",", seq_len(P), "]")])
  colnames(X) <- meta$party_names
  if (anyNA(X) || nrow(X) < 2) fail(dir, ": fewer than two draws, or missing values, in pi_smooth[", D, ", ]")
  if (max(abs(rowSums(X) - 1)) > 1e-6) fail(dir, ": the shares at the last date do not sum to 1")
  list(X = X, last_date = last_date, md5 = timing$data_md5, stan = basename(timing$stan_file))
}

crps10 <- function(X, y) mean(vapply(seq_len(ncol(X)), \(j) crps_draws(100 * X[, j], 100 * y[j]), numeric(1)))

rows <- lapply(seq_len(nrow(spec)), \(i) {
  E <- E_dates[i]
  ref <- read_fit(spec$ref[i], E)
  new <- read_fit(spec$new[i], E)
  if (!identical(ref$md5, new$md5)) fail(format(E), ": the two fits read different data (timing.rds data_md5)")
  if (ref$last_date != new$last_date) fail(format(E), ": the two fits end on different dates")
  if (!setequal(colnames(ref$X), colnames(new$X)) || anyDuplicated(colnames(ref$X))) {
    fail(format(E), ": the two fits name different parties")
  }
  truth <- truth_at(E)
  key <- tolower(colnames(ref$X))
  if (anyNA(match(key, names(truth)))) fail(format(E), ": a fitted party is missing from the result")
  if (!all(names(truth) %in% key)) fail(format(E), ": the result has votes outside the fitted parties")
  y <- truth[key]
  X_new <- new$X[, colnames(ref$X), drop = FALSE]
  c_ref <- crps10(ref$X, y)
  c_new <- crps10(X_new, y)
  noise <- outcome_noise(ref$X, X_new)$sd
  one <- election_reading(format(E), c_new - c_ref, noise)
  data.frame(
    E = format(E), last_date = format(ref$last_date), ref = ref$stan, new = new$stan, parties = ncol(ref$X),
    draws = sprintf("%d/%d", nrow(ref$X), nrow(X_new)), crps10_ref = c_ref, crps10_new = c_new,
    diff = c_new - c_ref, noise = noise, z = one$z, reading = one$reading
  )
})
tab <- do.call(rbind, rows)
overall <- election_reading(tab$E, tab$diff, tab$noise)
cat("== CRPS10 (pp) of pi_smooth at each fit's last date; diff = new - ref; noise = outcome_noise()$sd\n")
print(tab, digits = 3, row.names = FALSE)
cat(sprintf(
  "\n== over %d election(s), each weighted equally: diff %.3f pp, noise %.3f pp, z %.2f: %s (indistinguishable when |diff| < 2 noise)\n",
  nrow(tab), overall$diff, overall$noise, overall$z, overall$reading
))
