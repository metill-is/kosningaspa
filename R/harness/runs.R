# Run records (spec §5.5, §5.7, §5.8): each fit's id and directory, its frozen read-only
# Stan data, the meta keys that decide whether a finished run is reused (sha-skip), the
# retirement of a stale run, the diagnostics summary, the §5.7 gate and the DIAG line.

#' The version of the draw extraction; a change makes every finished run stale.
#' @export
EXTRACT_VERSION <- 1L

#' The sampler settings of every harness fit (spec §5.8).
#' @export
SAMPLER <- list(chains = 4L, parallel_chains = 4L, init = 0, max_treedepth = 12L)

#' Directory that holds the run directories.
#' @return `HARNESS_RESULTS` when set and not empty, otherwise `results/harness` under the
#'   project root (gitignored)
#' @export
results_root <- function() {
  box::use(R / harness / constants[project_root])
  root <- Sys.getenv("HARNESS_RESULTS", file.path(project_root(), "results", "harness"))
  # WHY: an empty value would put runs at the filesystem root ("" joins as "/").
  if (nzchar(root)) root else file.path(project_root(), "results", "harness")
}

as_count <- function(x, what) {
  ok <- is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0 && x == round(x) && x <= .Machine$integer.max
  if (!ok) stop(what, " must be one whole number in [0, 2^31), not ", paste(format(x), collapse = ", "))
  as.integer(x)
}

as_arm <- function(arm) {
  if (!is.character(arm) || length(arm) != 1 || !grepl("^[A-Za-z0-9-]+$", arm)) {
    stop("arm must be one name of letters, digits and hyphens, not ", paste(arm, collapse = ", "))
  }
  arm
}

as_day <- function(x, what) {
  d <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (length(d) != 1 || is.na(d)) stop(what, " must be one date, not ", paste(format(x), collapse = ", "))
  d
}

#' The id of a run: `<arm>_<E>_<F>_s<seed>`.
#' @param arm the arm's name: letters, digits and hyphens
#' @param E,F the election and forecast date (Date or ISO string), one each
#' @param seed the sampler seed, a whole number
#' @return for example `"r0_2024-11-30_2024-11-29_s1101"`
#' @export
run_id <- function(arm, E, F, seed) {
  sprintf("%s_%s_%s_s%d", as_arm(arm), format(as_day(E, "E")), format(as_day(F, "F")), as_count(seed, "seed"))
}

#' The directory of a run.
#' @param id a `run_id()`
#' @return `file.path(results_root(), id)`
#' @export
run_dir <- function(id) {
  file.path(results_root(), id)
}

#' The sha256 of a file.
#' @param path the file
#' @return 64 lowercase hex characters
#' @export
sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

#' The sha256 of Stan data as `cmdstanr::write_stan_json()` writes it.
#' @param stan_data the Stan data list
#' @return 64 lowercase hex characters: the sha of freshly built data, to compare with a
#'   finished run's `data_sha256`
#' @export
stan_data_sha <- function(stan_data) {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  cmdstanr::write_stan_json(stan_data, path)
  sha256_file(path)
}

#' Write a run's frozen inputs, read-only.
#' @param dir the run directory (created if absent)
#' @param stan_data the Stan data list
#' @param inputs_meta a list recording how the data were built
#' @return `stan_data_sha(stan_data)`, which is the sha256 of the written
#'   `dir/inputs/stan_data.json` (`cmdstanr::write_stan_json()`), after writing it and
#'   `dir/inputs/inputs.json` (`jsonlite::write_json(auto_unbox = TRUE, pretty = TRUE,
#'   digits = NA, null = "null")`, so a NULL field is written as null) with mode 0444. Both
#'   are written into a temporary sibling directory that is then renamed to `inputs`, so a
#'   failed write leaves no `inputs/`. An error if `dir/inputs` exists
#' @export
write_frozen_inputs <- function(dir, stan_data, inputs_meta) {
  inputs <- file.path(dir, "inputs")
  if (file.exists(inputs)) stop("frozen inputs exist: ", inputs)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("inputs-", tmpdir = dir)
  if (!dir.create(tmp)) stop("could not create ", tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  data_json <- file.path(tmp, "stan_data.json")
  meta_json <- file.path(tmp, "inputs.json")
  cmdstanr::write_stan_json(stan_data, data_json)
  jsonlite::write_json(inputs_meta, meta_json, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null")
  if (!all(Sys.chmod(c(data_json, meta_json), mode = "0444"))) stop("could not make read-only: ", tmp)
  sha <- stan_data_sha(stan_data)
  if (!identical(sha, sha256_file(data_json))) stop("stan_data.json does not reproduce stan_data_sha()")
  if (!file.rename(tmp, inputs)) stop("could not move ", tmp, " to ", inputs)
  sha
}

#' The sha256 of a config as its reader returns it.
#' @param config the config list (an arm's `read_config()` result)
#' @return 64 lowercase hex characters: the sha256 of the config as JSON with every object's
#'   keys sorted, scalars unboxed and numbers at full precision, so that key order, whitespace
#'   and number formatting in the file do not change it, and an integer and a double of the
#'   same whole value hash alike
#' @export
config_sha <- function(config) {
  sorted <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    if (!is.null(names(x))) x <- x[order(names(x), method = "radix")]
    lapply(x, sorted)
  }
  text <- jsonlite::toJSON(sorted(config), auto_unbox = TRUE, digits = NA, null = "null", na = "string")
  digest::digest(as.character(text), algo = "sha256", serialize = FALSE)
}

#' The meta keys a finished run must match to be reused (sha-skip).
#' @param arm the arm's name; not a key, since the run id carries it
#' @param seed,iter_warmup,iter_sampling the sampler seed and iterations, whole numbers
#' @param stan_file the arm's Stan file, hashed here
#' @param config_file the arm's config file
#' @param data_sha the sha256 of the run's Stan data JSON (`write_frozen_inputs()`)
#' @param config the config as the arm's reader returns it; by default the file as
#'   `jsonlite::read_json()` parses it
#' @return a named list: `stan_sha256`, `config_sha256` (`config_sha(config)`), `data_sha256`,
#'   `seed`, `iter_warmup`, `iter_sampling` (integers), `chains`, `max_treedepth`, `init` (from
#'   `SAMPLER`) and `extract_version` (`EXTRACT_VERSION`). Its attribute `"also_accepted"`,
#'   `list(config_sha256 = <sha256 of the file's raw text>)`, is the hash runs recorded before
#'   `config_sha()`, which `meta_mismatch()` accepts too
#' @export
expected_meta <- function(arm, seed, iter_warmup, iter_sampling, stan_file, config_file, data_sha,
                          config = jsonlite::read_json(config_file)) {
  as_arm(arm)
  if (!is.character(data_sha) || length(data_sha) != 1 || !grepl("^[0-9a-f]{64}$", data_sha)) {
    stop("data_sha must be one sha256 (64 lowercase hex characters)")
  }
  e <- list(
    stan_sha256 = sha256_file(stan_file),
    config_sha256 = config_sha(config),
    data_sha256 = data_sha,
    seed = as_count(seed, "seed"),
    iter_warmup = as_count(iter_warmup, "iter_warmup"),
    iter_sampling = as_count(iter_sampling, "iter_sampling"),
    chains = SAMPLER$chains,
    max_treedepth = SAMPLER$max_treedepth,
    init = SAMPLER$init,
    extract_version = EXTRACT_VERSION
  )
  # WHY: a run finished before config_sha() recorded the raw file's sha; it stays fresh.
  attr(e, "also_accepted") <- list(config_sha256 = sha256_file(config_file))
  e
}

same_value <- function(a, b) {
  if (is.null(a)) {
    return(FALSE)
  }
  a <- unlist(a, use.names = FALSE)
  b <- unlist(b, use.names = FALSE)
  if (length(a) != length(b) || anyNA(a) || anyNA(b)) {
    return(FALSE)
  }
  # WHY: a JSON round trip turns 0 into 0L and 4L into 4, so numbers compare by value.
  if (is.numeric(a) && is.numeric(b)) all(a == b) else identical(a, b)
}

#' The keys of `expected` that a run's meta does not match.
#' @param meta a run's `meta.json` as parsed (`jsonlite::read_json()`, simplified or not);
#'   keys outside `expected` are ignored
#' @param expected `expected_meta()`; a key of its attribute `"also_accepted"` (a named list)
#'   matches either value
#' @return the names of the keys whose values differ from `expected` or are missing from
#'   `meta`, in the order of `expected`; `character(0)` when the run can be reused.
#'   Numbers compare by value whatever their type; any other value must be identical, so a
#'   number written as a string, a null or an NA is a mismatch
#' @export
meta_mismatch <- function(meta, expected) {
  keys <- names(expected)
  if (!is.list(expected) || is.null(keys) || !all(nzchar(keys))) stop("expected must be a named list")
  if (!is.list(meta)) meta <- list()
  also <- attr(expected, "also_accepted")
  ok <- function(k) same_value(meta[[k]], expected[[k]]) || (!is.null(also[[k]]) && same_value(meta[[k]], also[[k]]))
  keys[!vapply(keys, ok, logical(1))]
}

#' Move a run aside, for a refit.
#' @param dir the run directory
#' @param label the name it is kept under: the directory's name, the run id, by default; a
#'   directory not named by a run id can be given its own
#' @return the new path, `results_root()/_stale/<label>_<YYYYmmddHHMMSS>`; an error if `dir`
#'   is not a directory or the new path exists
#' @export
retire_run <- function(dir, label = basename(dir)) {
  if (!dir.exists(dir)) stop("no run to retire: ", dir)
  stale <- file.path(results_root(), "_stale")
  dir.create(stale, showWarnings = FALSE, recursive = TRUE)
  to <- file.path(stale, paste0(label, "_", format(Sys.time(), "%Y%m%d%H%M%S")))
  if (file.exists(to)) stop("already retired this second: ", to)
  if (!file.rename(dir, to)) stop("could not move ", dir, " to ", to)
  to
}

#' R-hat and the bulk and tail ESS of each variable of a draws object.
#' @param x a posterior draws object
#' @return `posterior::summarise_draws(x, "rhat", "ess_bulk", "ess_tail")`
#' @export
measure_draws <- function(x) posterior::summarise_draws(x, "rhat", "ess_bulk", "ess_tail")

#' What a fit records of where it ran: the commit and whether the tree had changes, the
#' snapshot manifest's sha256 and the CmdStan and cmdstanr versions.
#' @return `list(git_head, git_dirty, snapshot_manifest_sha256, cmdstan_version,
#'   cmdstanr_version)`; `git_head` and `git_dirty` from git run in the working directory,
#'   which a runner keeps at the project root
#' @export
run_provenance <- function() {
  box::use(R / harness / constants[harness_path])
  git <- function(...) suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE))
  list(
    git_head = git("rev-parse", "HEAD"),
    git_dirty = length(git("status", "--porcelain")) > 0,
    snapshot_manifest_sha256 = sha256_file(harness_path("snapshot_manifest.csv")),
    cmdstan_version = cmdstanr::cmdstan_version(),
    cmdstanr_version = as.character(utils::packageVersion("cmdstanr"))
  )
}

# WHY: a constant parameter (L_Omega's fixed entries) has no R-hat or ESS.
present <- function(x) x[!is.na(x)]

extreme <- function(f, x) {
  if (length(x)) f(x) else NA_real_
}

#' Each chain's deepest tree and leapfrog steps.
#' @param diagnostics a fit's post-warmup sampler diagnostics (`fit$sampler_diagnostics()`),
#'   a posterior draws object holding `treedepth__` and `n_leapfrog__`
#' @return `list(max_depth, n_leapfrog)`, one value per chain, as `summarise_diagnostics()`
#'   takes them: the chain's deepest `treedepth__` and the sum of its `n_leapfrog__` over
#'   its draws
#' @export
chain_sampler <- function(diagnostics) {
  x <- unclass(posterior::as_draws_array(diagnostics))
  lacks <- setdiff(c("treedepth__", "n_leapfrog__"), dimnames(x)[[3]])
  if (length(lacks)) stop("sampler diagnostics lack: ", paste(lacks, collapse = ", "))
  list(
    max_depth = apply(x[, , "treedepth__", drop = FALSE], 2, max),
    n_leapfrog = apply(x[, , "n_leapfrog__", drop = FALSE], 2, sum)
  )
}

#' The diagnostics of a fit.
#' @param params the draws of every `parameters`-block variable, a posterior draws object
#' @param scored the draws of the scored quantities, with the same chains
#' @param sampler `list(num_divergent, num_max_treedepth, ebfmi, max_depth, n_leapfrog)`, one
#'   value per chain: the first three from `fit$diagnostic_summary()`, `max_depth` the
#'   deepest `treedepth__` of the chain in `fit$sampler_diagnostics()` and `n_leapfrog` the
#'   sum of its post-warmup `n_leapfrog__` there (whole numbers), the last two as
#'   `chain_sampler()` returns them
#' @return a list: `max_rhat`, `min_ess_bulk` and `min_ess_tail` over `params` and
#'   `scored` together (Decision 3); `max_rhat_scored`, `min_ess_bulk_scored` and
#'   `min_ess_tail_scored` over `scored`; `na_scored`, the number of scored quantities
#'   whose R-hat, bulk ESS or tail ESS is NA; `divergent` and `treedepth_hits`, summed
#'   over chains; `max_depth` (integers), `min_ebfmi` and `n_leapfrog`, the leapfrog steps
#'   (gradients) of the post-warmup draws summed over chains. A parameter without an R-hat or
#'   ESS (a constant) is left out of the extremes; a scored quantity without one (a
#'   constant, or an NA draw) is kept, so the extremes it enters are NA
#' @export
summarise_diagnostics <- function(params, scored, sampler) {
  fields <- c("num_divergent", "num_max_treedepth", "ebfmi", "max_depth", "n_leapfrog")
  lacks <- setdiff(fields, names(sampler))
  if (length(lacks)) stop("sampler lacks: ", paste(lacks, collapse = ", "))
  chains <- posterior::nchains(params)
  if (posterior::nchains(scored) != chains) stop("params and scored have different chains")
  if (any(lengths(sampler[fields]) != chains)) stop("sampler needs one value per chain (", chains, ")")
  leapfrog <- sampler$n_leapfrog
  if (!is.numeric(leapfrog) || anyNA(leapfrog) || any(leapfrog < 0 | leapfrog != round(leapfrog))) {
    stop("sampler n_leapfrog must be whole numbers, at least 0")
  }
  p <- measure_draws(params)
  s <- measure_draws(scored)
  list(
    max_rhat = extreme(max, c(present(p$rhat), s$rhat)),
    max_rhat_scored = extreme(max, s$rhat),
    min_ess_bulk = extreme(min, c(present(p$ess_bulk), s$ess_bulk)),
    min_ess_tail = extreme(min, c(present(p$ess_tail), s$ess_tail)),
    min_ess_bulk_scored = extreme(min, s$ess_bulk),
    min_ess_tail_scored = extreme(min, s$ess_tail),
    na_scored = sum(is.na(s$rhat) | is.na(s$ess_bulk) | is.na(s$ess_tail)),
    divergent = as.integer(sum(sampler$num_divergent)),
    treedepth_hits = as.integer(sum(sampler$num_max_treedepth)),
    max_depth = as.integer(max(sampler$max_depth)),
    min_ebfmi = min(sampler$ebfmi),
    n_leapfrog = sum(as.numeric(leapfrog))
  )
}

diag_values <- function(d, fields) {
  lacks <- setdiff(fields, names(d))
  if (length(lacks)) stop("d lacks: ", paste(lacks, collapse = ", "))
  # WHY: a value read back from JSON can be null or a logical NA; either must fail, not vanish.
  lapply(stats::setNames(fields, fields), \(f) if (is.numeric(d[[f]]) && length(d[[f]]) == 1) d[[f]] else NA_real_)
}

# WHY: at four decimals 1.01004 prints as 1.0100, which would read as passing a 1.01 rule.
shown_above <- function(x, limit) {
  if (is.na(x)) {
    return("NA")
  }
  for (k in 4:17) {
    s <- sprintf("%.*f", k, x)
    if (as.numeric(s) > limit) {
      return(s)
    }
  }
  sprintf("%.17g", x)
}

#' The diagnostics gate (spec §5.7).
#' @param d `summarise_diagnostics()`
#' @param rhat_max,ess_min the R-hat ceiling and the scored bulk ESS floor
#' @return `list(pass, reasons)`: a cell passes if `max_rhat <= rhat_max`, `divergent == 0`,
#'   `min_ess_bulk_scored >= ess_min` and `na_scored == 0`; `reasons` holds one line per
#'   failed rule with its value ("max R-hat 1.0234 > 1.01", "divergent 2 > 0", "min scored
#'   bulk ESS 399 < 400", "scored quantities with NA R-hat or ESS: 2"). R-hat is printed
#'   with as many decimals (four or more) as it takes to show it above `rhat_max`, and ESS
#'   rounded down, so a reason never reads as passing its rule. A missing value (NA, or a
#'   null read back from JSON) fails its rule
#' @export
diagnostics_gate <- function(d, rhat_max = 1.01, ess_min = 400) {
  v <- diag_values(d, c("max_rhat", "divergent", "min_ess_bulk_scored", "na_scored"))
  rules <- c(
    isTRUE(v$max_rhat <= rhat_max),
    isTRUE(v$divergent == 0),
    isTRUE(v$min_ess_bulk_scored >= ess_min),
    isTRUE(v$na_scored == 0)
  )
  reasons <- c(
    sprintf("max R-hat %s > %s", shown_above(v$max_rhat, rhat_max), format(rhat_max)),
    sprintf("divergent %s > 0", format(v$divergent)),
    sprintf("min scored bulk ESS %.0f < %s", floor(v$min_ess_bulk_scored), format(ess_min)),
    sprintf("scored quantities with NA R-hat or ESS: %s", format(v$na_scored))
  )
  list(pass = all(rules), reasons = reasons[!rules])
}

#' The one-line DIAG log entry of a run.
#' @param id the `run_id()`
#' @param minutes the fit's wall time in minutes
#' @param d `summarise_diagnostics()`
#' @param gate `diagnostics_gate()`
#' @return `"DIAG run=<id> minutes=%.1f max_rhat=%.4f min_ess_bulk=%.0f min_ess_tail=%.0f
#'   divergent=%d treedepth_hits=%d max_depth=%d min_ebfmi=%.3f gate=<PASS|FAIL>"`
#' @export
diag_line <- function(id, minutes, d, gate) {
  v <- diag_values(d, c("max_rhat", "min_ess_bulk", "min_ess_tail", "divergent", "treedepth_hits", "max_depth", "min_ebfmi"))
  sprintf(
    "DIAG run=%s minutes=%.1f max_rhat=%.4f min_ess_bulk=%.0f min_ess_tail=%.0f divergent=%d treedepth_hits=%d max_depth=%d min_ebfmi=%.3f gate=%s",
    id, minutes, v$max_rhat, v$min_ess_bulk, v$min_ess_tail, as.integer(v$divergent),
    as.integer(v$treedepth_hits), as.integer(v$max_depth), v$min_ebfmi,
    if (isTRUE(gate$pass)) "PASS" else "FAIL"
  )
}
