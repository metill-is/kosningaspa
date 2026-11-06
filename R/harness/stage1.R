# Stage 1 of arm V2 (spec §6.5): the fundamentals regression on the elections before E, in
# Stan/fundamentals_v2.stan, and its output mu_f, the posterior mean of the centred
# predictor at E over S_f(E), cached under results_root()/stage1/ by the sha256 of its
# inputs.

#' The stage-1 Stan file, relative to the project root.
#' @export
STAGE1_STAN <- file.path("Stan", "fundamentals_v2.stan")

#' The largest R-hat a stage-1 fit may have.
STAGE1_MAX_RHAT <- 1.01

#' The significant digits of the stage-1 fit's CSV: the full double, so that the mean of the
#' centred draws of `eta_E` sums to 0 to rounding.
STAGE1_SIG_FIGS <- 18L

#' The data fields declared as a one-dimensional array or vector, stored as 1-d R arrays so
#' that `cmdstanr::write_stan_json()` writes a length-1 field as an array.
STAGE1_VECTOR_FIELDS <- c(
  "n_rows", "label", "y", "x", "inc_years", "vnv", "growth", "label_E", "x_E", "inc_years_E", "vnv_E", "growth_E"
)

#' Stop with "stage 1: " and `...`.
fail <- function(...) stop("stage 1: ", ..., call. = FALSE)

#' Stop with "stage 1: `what`" unless `ok` is TRUE.
check <- function(ok, what) if (!isTRUE(ok)) fail(what)

#' The predictors §6.5 names: the lag, logit(voteshare_prior / 100), the previous vote
#' share as recorded in the row; log incumbency years (0 for none); and log(1 + CPI
#' inflation) and log(1 + GDP growth / 100) for incumbents, 0 for the other parties. The
#' last three are coded as production's `prepare_fundamentals_data()` codes them. The lag
#' is not production's `x_f`, which renormalises the previous election's shares, gives 0
#' to a label with no row at the previous election and drops the first election as an
#' outcome.
covariates <- function(rows) {
  p <- rows$voteshare_prior / 100
  list(
    x = log(p) - log(1 - p),
    inc_years = ifelse(rows$incumbent_years > 0, log(rows$incumbent_years), 0),
    vnv = log(1 + rows$vnv) * rows$incumbent,
    growth = log(1 + rows$growth / 100) * rows$incumbent
  )
}

#' The Stan data of `Stan/fundamentals_v2.stan` for one cell.
#' @param info an `info_set()` list
#' @return a list, in this order, each one-dimensional field a 1-d array
#'   (`STAGE1_VECTOR_FIELDS`):
#'   - `D`, `N`, `L`: training elections, rows and party labels; `n_rows` (each election's
#'     rows) and, per row, sorted by election and then label, `label`, `y`, `x`,
#'     `inc_years`, `vnv` and `growth`;
#'   - `N_E` (the parties of `info$fundamentals$S_f`), and in S_f order `label_E`, `x_E`,
#'     `inc_years_E`, `vnv_E` and `growth_E`.
#'
#'   The rows are `fundamentals_rows()`'s (R0's table), with the party order
#'   `POLL_PARTIES` restricted to the cell. A training row is a row dated before E that is
#'   in S_f(d) (`info$fundamentals$train$in_sf`: a list with a previous result, not Annað,
#'   not new) and stood at d (voteshare > 0); an election enters with at least two such
#'   rows. Its count is production's, round(voteshare / Σ voteshare × total_valid_votes)
#'   with the sum over that election's rows with a previous share, restricted to S_f(d), so
#'   the shares are renormalised over S_f(d). Its predictors are `covariates()`'s, so the
#'   row set differs from R0's in two ways: 1967 enters as an outcome (its rows record
#'   1963's shares, and R0's table drops its first election as one), and a successor list
#'   with no row under the same label at the previous election gets its recorded previous
#'   share, where R0's lag is 0. E's rows
#'   are the target rows of the S_f parties, whose masked columns (voteshare,
#'   voteshare_change, total_valid_votes) are never read. The labels are the α labels
#'   (`party`) of those rows and E's, in the order of `fundamentals_rows()`'s levels.
#'
#'   Attributes: `election_dates`, the training elections' dates (increasing, all before
#'   E), and `labels`, the label of each index.
#'
#'   An error, "stage 1: ...", if a row of the table is not a training row of
#'   `info$fundamentals$train`, an S_f party has no single target row, no election has two
#'   training rows, or a field holds an NA or a non-finite value.
#' @export
stage1_data <- function(info) {
  box::use(
    R / harness / constants[POLL_PARTIES],
    R / harness / r0[fundamentals_rows]
  )
  E <- as.Date(info$E)
  S_f <- info$fundamentals$S_f
  rows <- fundamentals_rows(info, intersect(POLL_PARTIES, info$parties))
  label_order <- levels(rows$flokkur)
  rows$flokkur <- as.character(rows$flokkur)

  train <- rows[rows$date < E, ]
  tr <- info$fundamentals$train
  key <- function(date, party) paste(format(date), party, sep = "\r")
  at <- match(key(train$date, train$flokkur), key(tr$date, tr$party))
  check(!anyNA(at) && !anyDuplicated(at), "the training rows are not those of info$fundamentals$train")
  with_prior <- !is.na(train$voteshare_prior)
  total <- tapply(train$voteshare[with_prior], format(train$date[with_prior]), sum)
  train$votes <- round(train$voteshare / total[format(train$date)] * train$total_valid_votes)
  train <- train[tr$in_sf[at] & !is.na(train$voteshare) & train$voteshare > 0, ]
  sizes <- table(format(train$date))
  dates <- sort(as.Date(names(sizes)[sizes >= 2]))
  check(length(dates) >= 1, "no election before E has two parties of S_f")
  train <- train[train$date %in% dates, ]
  train <- train[order(train$date, match(train$flokkur, label_order), method = "radix"), ]

  target <- rows[rows$date == E & rows$flokkur %in% S_f, ]
  check(nrow(target) == length(S_f) && !anyDuplicated(target$flokkur), "an S_f party has no single target row at E")
  target <- target[match(S_f, target$flokkur), ]

  labels <- label_order[label_order %in% c(train$flokkur, target$flokkur)]
  cov <- covariates(train)
  cov_E <- covariates(target)
  data <- list(
    D = length(dates), N = nrow(train), L = length(labels),
    n_rows = as.integer(table(factor(format(train$date), levels = format(dates)))),
    label = match(train$flokkur, labels), y = as.integer(train$votes),
    x = cov$x, inc_years = cov$inc_years, vnv = cov$vnv, growth = cov$growth,
    N_E = length(S_f), label_E = match(target$flokkur, labels),
    x_E = cov_E$x, inc_years_E = cov_E$inc_years, vnv_E = cov_E$vnv, growth_E = cov_E$growth
  )
  bad <- names(data)[!vapply(data, \(v) all(is.finite(v)), logical(1))]
  if (length(bad)) fail("no field holds an NA or a non-finite value; it does in ", paste(bad, collapse = ", "))
  data[STAGE1_VECTOR_FIELDS] <- lapply(data[STAGE1_VECTOR_FIELDS], \(v) array(v, dim = length(v)))
  attr(data, "election_dates") <- dates
  attr(data, "labels") <- labels
  data
}

#' The cache key of a stage-1 fit.
#' @return the sha256 of the lines naming the Stan file's and the data JSON's sha256, the
#'   seed, the iterations, the CmdStan version, the other sampler settings (`SAMPLER`'s
#'   chains, init and max_treedepth, and `STAGE1_SIG_FIGS`) and the S_f parties
#' @export
stage1_key <- function(stan_sha, data_sha, s1, S_f) {
  box::use(R / harness / runs[SAMPLER])
  lines <- c(
    paste("stan", stan_sha), paste("data", data_sha), paste("seed", s1$seed),
    paste("iter_warmup", s1$iter_warmup), paste("iter_sampling", s1$iter_sampling),
    paste("cmdstan", cmdstanr::cmdstan_version()), paste("chains", SAMPLER$chains), paste("init", SAMPLER$init),
    paste("max_treedepth", SAMPLER$max_treedepth), paste("sig_figs", STAGE1_SIG_FIGS),
    paste("S_f", paste(enc2utf8(S_f), collapse = "\r"))
  )
  digest::digest(paste(lines, collapse = "\n"), algo = "sha256", serialize = FALSE)
}

#' The fields of a stage-1 record's `diagnostics$log_phi_f_near_cap`.
NEAR_CAP_FIELDS <- c("cap", "margin", "near_cap", "n_draws", "within_margin", "within_near_cap")

#' Stage 1's draws near its cap (Decision 4): log φ_f = -log(phi_f_inv) within
#' `V2_CAP_MARGIN` and within `NEAR_CAP` (the stop rule's margin) of `V2_STAGE1_LOG_CAP`, where
#' within m means log φ_f > cap - m.
#' @param phi_f_inv the fit's draws of `phi_f_inv`, all chains
#' @return `list(cap, margin, near_cap, n_draws, within_margin, within_near_cap)`, the last
#'   three whole numbers; an error, "stage 1: ...", for no draws or a missing or non-positive one
#' @export
stage1_near_cap <- function(phi_f_inv) {
  box::use(R / harness / compare_runs[NEAR_CAP], R / harness / v2_reference[V2_CAP_MARGIN, V2_STAGE1_LOG_CAP])
  x <- as.numeric(phi_f_inv)
  check(length(x) > 0 && !anyNA(x) && all(x > 0), "phi_f_inv has no draws, or a missing or non-positive one")
  log_phi_f <- -log(x)
  cap <- V2_STAGE1_LOG_CAP[["log_phi_f"]]
  list(
    cap = cap, margin = V2_CAP_MARGIN, near_cap = NEAR_CAP, n_draws = length(x),
    within_margin = sum(log_phi_f > cap - V2_CAP_MARGIN), within_near_cap = sum(log_phi_f > cap - NEAR_CAP)
  )
}

#' The log φ_f near-cap counts of a stage-1 cache record, when it holds them as the current
#' code would write them.
#' @param rec a record, as `jsonlite::read_json()` reads `mu_f.json`
#' @return `list(n_draws, within_margin, within_near_cap)`, or NULL when the record has no
#'   `diagnostics$log_phi_f_near_cap`, or has it with another cap or margin than
#'   `stage1_near_cap()` uses, with `n_draws` other than `SAMPLER`'s chains x the record's
#'   `iter_sampling`, or with counts that are not whole numbers, 0 <= `within_near_cap` <=
#'   `within_margin` <= `n_draws`. Such a record is stale: `stage1_mu_f()` refits it, or with
#'   `fit = FALSE` treats it as a cold cache
#' @export
stage1_near_cap_of <- function(rec) {
  box::use(
    R / harness / compare_runs[NEAR_CAP],
    R / harness / runs[SAMPLER],
    R / harness / v2_reference[V2_CAP_MARGIN, V2_STAGE1_LOG_CAP]
  )
  if (!is.list(rec) || !is.list(rec$diagnostics)) {
    return(NULL)
  }
  nc <- rec$diagnostics$log_phi_f_near_cap
  one <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  if (!is.list(nc) || !all(vapply(NEAR_CAP_FIELDS, \(f) one(nc[[f]]), logical(1))) || !one(rec$iter_sampling)) {
    return(NULL)
  }
  counts <- c(nc$within_near_cap, nc$within_margin, nc$n_draws)
  ok <- nc$cap == V2_STAGE1_LOG_CAP[["log_phi_f"]] && nc$margin == V2_CAP_MARGIN && nc$near_cap == NEAR_CAP &&
    nc$n_draws == SAMPLER$chains * rec$iter_sampling && all(counts == round(counts)) && counts[1] >= 0 &&
    !is.unsorted(counts)
  if (!ok) {
    return(NULL)
  }
  list(n_draws = nc$n_draws, within_margin = nc$within_margin, within_near_cap = nc$within_near_cap)
}

#' Whether `path` holds a stage-1 record with current near-cap counts (`stage1_near_cap_of()`):
#' FALSE for no file and for a stale record, which `stage1_mu_f()` then refits, or with
#' `fit = FALSE` treats as a cold cache.
#' @export
stage1_current <- function(path) file.exists(path) && !is.null(stage1_near_cap_of(jsonlite::read_json(path)))

#' A cached stage-1 fit, as `stage1_mu_f()` returns it.
read_stage1 <- function(path, key, S_f) {
  rec <- jsonlite::read_json(path)
  parties <- as.character(unlist(rec$parties))
  mu_f <- as.numeric(unlist(rec$mu_f))
  check(
    identical(rec$sha256, key) && identical(parties, S_f) && length(mu_f) == length(S_f) && all(is.finite(mu_f)),
    paste0("the cache ", path, " does not hold this fit")
  )
  list(mu_f = stats::setNames(mu_f, S_f), sha256 = key, diagnostics = rec$diagnostics)
}

#' Fit stage 1 and write its cache file atomically; stops, writing nothing, unless the fit
#' passes its check.
fit_stage1 <- function(stan_file, json, s1, key, info, path) {
  box::use(R / harness / runs[SAMPLER])
  model <- cmdstanr::cmdstan_model(stan_file, quiet = TRUE)
  out <- tempfile("stage1-")
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  start <- Sys.time()
  fit <- model$sample(
    data = json, seed = s1$seed, chains = SAMPLER$chains, parallel_chains = SAMPLER$parallel_chains,
    init = SAMPLER$init, max_treedepth = SAMPLER$max_treedepth, iter_warmup = s1$iter_warmup,
    iter_sampling = s1$iter_sampling, sig_figs = STAGE1_SIG_FIGS, output_dir = out, refresh = 0,
    show_messages = FALSE
  )
  minutes <- as.numeric(difftime(Sys.time(), start, units = "mins"))
  eta <- fit$draws("eta_E", format = "draws_matrix")
  check(
    all(fit$return_codes() == 0) && nrow(eta) == SAMPLER$chains * s1$iter_sampling,
    paste("the fit does not hold", SAMPLER$chains, "chains of", s1$iter_sampling, "draws")
  )
  params <- fit$draws(variables = c(names(model$variables()$parameters), "eta_E"))
  m <- posterior::summarise_draws(params, "rhat", "ess_bulk", "ess_tail")
  sampler <- fit$diagnostic_summary(quiet = TRUE)
  diagnostics <- list(
    max_rhat = max(m$rhat), min_ess_bulk = min(m$ess_bulk), min_ess_tail = min(m$ess_tail),
    num_divergent = as.integer(sum(sampler$num_divergent)), num_max_treedepth = as.integer(sum(sampler$num_max_treedepth)),
    min_ebfmi = min(sampler$ebfmi), minutes = minutes,
    log_phi_f_near_cap = stage1_near_cap(fit$draws("phi_f_inv", format = "draws_matrix"))
  )
  cat(sprintf(
    "STAGE1 E=%s F=%s %s max_rhat=%.4f min_ess_bulk=%.0f min_ess_tail=%.0f divergent=%d max_treedepth_hits=%d min_ebfmi=%.2f minutes=%.2f\n",
    format(info$E), format(info$F), substr(key, 1, 12), diagnostics$max_rhat, diagnostics$min_ess_bulk,
    diagnostics$min_ess_tail, diagnostics$num_divergent, diagnostics$num_max_treedepth, diagnostics$min_ebfmi, minutes
  ))
  if (!isTRUE(diagnostics$max_rhat <= STAGE1_MAX_RHAT) || diagnostics$num_divergent > 0) {
    fail(
      "the fit fails its check (max R-hat ", format(diagnostics$max_rhat), ", at most ", STAGE1_MAX_RHAT, "; ",
      diagnostics$num_divergent, " divergent transitions, none allowed); no cache written"
    )
  }
  rec <- list(
    sha256 = key, E = format(info$E), F = format(info$F), parties = I(info$fundamentals$S_f),
    mu_f = I(unname(colMeans(eta))), diagnostics = diagnostics, seed = s1$seed, iter_warmup = s1$iter_warmup,
    iter_sampling = s1$iter_sampling, cmdstan_version = cmdstanr::cmdstan_version()
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("mu_f-", tmpdir = dirname(path), fileext = ".json")
  # WHY: 17 significant digits round-trip a double exactly, so a cache hit returns the fit's own values.
  jsonlite::write_json(rec, tmp, auto_unbox = TRUE, pretty = TRUE, digits = I(17))
  if (!file.rename(tmp, path)) fail("could not move ", tmp, " to ", path)
  invisible(path)
}

#' Stage 1's fit for one cell, the fundamentals pseudo-observation's source (spec §6.5).
#' @param info an `info_set()` list
#' @param cfg a `read_v2_config()` list; its `stage1` gives the seed and the iterations
#' @param root the project root whose freeze files `forecast_free()` reads
#' @param fit FALSE to read the cache only: on a cold cache, an error ("stage 1: no cached
#'   fit ...") with nothing compiled, fitted or written, as a test that must not write
#'   outside its own results root needs
#' @return `list(mu_f, sha256, diagnostics)`: `mu_f` the posterior mean of `eta_E`, named
#'   by `info$fundamentals$S_f` in that order; `sha256` the cache key; `diagnostics` a list
#'   (`max_rhat` and `min_ess_bulk`, `min_ess_tail` over the parameters and `eta_E`,
#'   `num_divergent`, `num_max_treedepth`, `min_ebfmi`, `minutes`, and
#'   `log_phi_f_near_cap`, `stage1_near_cap()` of the fit's `phi_f_inv` draws).
#'
#'   The key is the sha256 over the Stan file's sha256, the sha256 of the data JSON as
#'   `cmdstanr::write_stan_json()` writes `stage1_data(info)`, the seed, the iterations,
#'   the CmdStan version, `SAMPLER`'s chains, init and max_treedepth, `STAGE1_SIG_FIGS`
#'   and the S_f parties.
#'   The fit's output lives in `results_root()/stage1/<key>/mu_f.json`, written atomically
#'   (a temporary file, then a rename), and a later call with the same key reads it
#'   instead of fitting, so both calls return identical values. A record without current
#'   near-cap counts (`stage1_near_cap_of()` NULL), as one written before they were recorded, is
#'   stale: it is refitted under the same key, or with `fit = FALSE` treated as a cold cache.
#'   The fit runs `SAMPLER`'s chains, init and max_treedepth with `cfg$stage1`'s seed and
#'   iterations, and also counts the draws of log φ_f near its cap (`stage1_near_cap()`); it writes its
#'   CSVs with `STAGE1_SIG_FIGS` digits, and prints a STAGE1 line of its diagnostics.
#'
#'   An error, "stage 1: ...", before any fit or compile when `forecast_free(info$E,
#'   root)` holds ("2017 is not fitted before the freeze"); when `cfg$stage1` is not a
#'   seed and two iteration counts; on any error of `stage1_data()`; and, writing no cache,
#'   unless every chain finished with `iter_sampling` draws, the max R-hat is at most 1.01
#'   and there is no divergent transition.
#' @export
stage1_mu_f <- function(info, cfg, root = project_root(), fit = TRUE) {
  # WHY: the default argument is a promise, forced after this import.
  box::use(
    R / harness / constants[project_root],
    R / harness / freeze[forecast_free],
    R / harness / runs[results_root, sha256_file]
  )
  if (forecast_free(info$E, root)) fail("2017 is not fitted before the freeze")
  s1 <- cfg$stage1
  whole <- function(x, lo) is.numeric(x) && length(x) == 1 && is.finite(x) && x == round(x) && x >= lo
  check(
    is.list(s1) && whole(s1$seed, 0) && whole(s1$iter_warmup, 1) && whole(s1$iter_sampling, 1),
    "cfg$stage1 must hold a seed and two iteration counts"
  )
  data <- stage1_data(info)
  attributes(data) <- list(names = names(data))
  stan_file <- file.path(project_root(), STAGE1_STAN)
  json <- tempfile("stage1-data-", fileext = ".json")
  on.exit(unlink(json), add = TRUE)
  cmdstanr::write_stan_json(data, json)
  S_f <- info$fundamentals$S_f
  key <- stage1_key(sha256_file(stan_file), sha256_file(json), s1, S_f)
  path <- file.path(results_root(), "stage1", key, "mu_f.json")
  # WHY: a record without current near-cap counts (none, as one written before they were recorded, or ones of another cap, margin or draw count) is stale: refitted, or with fit = FALSE a cold cache.
  if (!stage1_current(path)) {
    stale <- if (file.exists(path)) "; the cached record has no current log phi_f near-cap counts, so it is stale" else ""
    if (!isTRUE(fit)) {
      fail("no cached fit for ", format(info$E), " F ", format(info$F), " under results_root()/stage1/ (key ", substr(key, 1, 12), ")", stale)
    }
    fit_stage1(stan_file, json, s1, key, info, path)
  }
  read_stage1(path, key, S_f)
}
