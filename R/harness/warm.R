# Adaptation reuse. A standard harness fit leaves its sampler adaptation in warm_start/ in its
# run directory: per chain the last draw as a CmdStan init, and in adaptation.json the adapted
# step size and diagonal inverse metric. The arm `<base>-w<ref_seed>` (R/harness/arms.R) is a
# warm run of `<base>` on the same cell: it takes that record from the cell's seed-<ref_seed>
# run of `<base>`, which must be the same model on the same data, and samples after a fixed
# burn-in with adaptation off. R/harness/fit_cell.R writes the record and runs the warm fit.

#' The burn-in of a warm run: iterations sampled with adaptation off and discarded.
#' @export
WARM_BURN_IN <- 100L

#' The significant figures of every harness fit's CmdStan CSV. At the default, 8, a
#' sum-to-zero vector's last draw is off zero sum by more than CmdStan's tolerance of 1e-8,
#' so it could not be read back as an init.
#' @export
SIG_FIGS <- 18L

WARM_DIR <- "warm_start"
ADAPTATION <- "adaptation.json"
# WHY: CmdStan prints the step size and the inverse metric in the CSV header at six significant figures.
HEADER_TOL <- 1e-5

refuse <- function(...) stop("warm run: ", ..., call. = FALSE)

is_count <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0 && x == round(x)

positive <- function(x) is.numeric(x) && length(x) > 0 && !anyNA(x) && all(is.finite(x) & x > 0)

close_to <- function(a, b) length(a) == length(b) && all(abs(a - b) <= HEADER_TOL * pmax(abs(a), abs(b)))

#' Each chain's last draw of a fit as a CmdStan init.
#' @param draws the draws of every parameter of non-zero size, a posterior draws object
#' @param chain_ids the chain id of each chain of `draws`, in its order (`fit$metadata()$id`)
#' @param parameters the model's parameters (`model$variables()$parameters`): a named list
#'   whose elements hold `dimensions`, the number of dimensions (0 for a scalar)
#' @param sizes the dimensions of each parameter of non-zero size
#'   (`fit$metadata()$stan_variable_sizes`); a parameter not in it has size zero
#' @return a list named by chain id: for each chain, a named list of every parameter of
#'   non-zero size, in the order of `parameters`, holding its value at the chain's last
#'   iteration. A parameter declared with dimensions keeps them, so
#'   `cmdstanr::write_stan_json()` writes one with a single element as `[x]`. A parameter of
#'   size zero is left out: CmdStan refuses `[]` for one of two or more dimensions (dims
#'   (0, P) declared, (0) found), and starts a missing one, which has nothing to start, at
#'   random. An error if a column of `draws` is missing
#' @export
warm_inits <- function(draws, chain_ids, parameters, sizes) {
  x <- unclass(posterior::as_draws_array(draws))
  if (length(chain_ids) != dim(x)[2] || anyNA(chain_ids) || anyDuplicated(chain_ids)) {
    stop("chain_ids must name each of the ", dim(x)[2], " chains of draws once")
  }
  last <- dim(x)[1]
  one_chain <- function(j) {
    v <- stats::setNames(as.numeric(x[last, j, ]), dimnames(x)[[3]])
    out <- list()
    for (n in names(parameters)) {
      d <- sizes[[n]]
      if (is.null(d) || prod(d) == 0) next
      scalar <- parameters[[n]]$dimensions == 0
      cols <- if (scalar) n else paste0(n, "[", apply(as.matrix(expand.grid(lapply(d, seq_len))), 1, paste, collapse = ","), "]")
      if (!all(cols %in% names(v))) stop("draws lack ", n, " (", sum(!cols %in% names(v)), " of ", length(cols), " columns)")
      out[[n]] <- if (scalar) unname(v[[n]]) else array(unname(v[cols]), d)
    }
    out
  }
  stats::setNames(lapply(seq_along(chain_ids), one_chain), as.character(chain_ids))
}

#' Write a run's warm-start record, read-only, in `dir/warm_start/`: `init-<chain>.json` for
#' each chain (`cmdstanr::write_stan_json()`) and `adaptation.json`, which holds `chain_ids`,
#' `step_size`, `inv_metric` (one array per chain), `zero_size` (the parameters of size zero,
#' which the inits leave out), `init` (the init files' names) and `init_sha256` (their
#' sha256), so that its own sha256 covers the inits.
#' @param dir the run directory
#' @param inits `warm_inits()`, one init per chain, named by chain id
#' @param step_size,inv_metric each chain's adapted step size and diagonal inverse metric, in
#'   the order of `inits`
#' @param zero_size the names of the model's parameters of size zero
#' @return the paths written, `adaptation.json` last. The files are written into a temporary
#'   sibling directory that is then renamed, so a failed write leaves no `warm_start/`; an
#'   error if `dir/warm_start` exists or a step size or metric is not positive
#' @export
write_warm_record <- function(dir, inits, step_size, inv_metric, zero_size = character(0)) {
  box::use(R / harness / runs[sha256_file])
  out <- file.path(dir, WARM_DIR)
  if (file.exists(out)) stop("a warm-start record exists: ", out)
  ids <- suppressWarnings(as.integer(names(inits)))
  if (!length(ids) || anyNA(ids) || anyDuplicated(ids)) stop("inits must be named by chain id")
  if (length(step_size) != length(ids) || !positive(step_size)) stop("step_size needs one positive value per chain")
  if (length(inv_metric) != length(ids) || !all(vapply(inv_metric, positive, logical(1)))) {
    stop("inv_metric needs one positive vector per chain")
  }
  tmp <- tempfile("warm-", tmpdir = dir)
  if (!dir.create(tmp)) stop("could not create ", tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  files <- sprintf("init-%d.json", ids)
  for (k in seq_along(ids)) cmdstanr::write_stan_json(inits[[k]], file.path(tmp, files[k]))
  record <- list(
    chain_ids = ids, step_size = as.numeric(step_size), inv_metric = lapply(unname(inv_metric), as.numeric),
    zero_size = as.character(zero_size), init = files,
    init_sha256 = vapply(file.path(tmp, files), sha256_file, character(1), USE.NAMES = FALSE)
  )
  jsonlite::write_json(record, file.path(tmp, ADAPTATION), digits = NA)
  written <- file.path(tmp, c(files, ADAPTATION))
  if (!all(Sys.chmod(written, mode = "0444"))) stop("could not make read-only: ", tmp)
  if (!file.rename(tmp, out)) stop("could not move ", tmp, " to ", out)
  file.path(out, c(files, ADAPTATION))
}

#' Write the warm-start record of a finished standard fit (`write_warm_record()`): each
#' chain's last draw, adapted step size and inverse metric, paired by chain id.
#' @param fit the CmdStanMCMC fit, sampled with adaptation and at `SIG_FIGS`
#' @param model its CmdStanModel
#' @param draws the fit's draws of every parameter of non-zero size
#' @param dir the run directory
#' @return `write_warm_record()`'s paths
#' @export
write_warm_start <- function(fit, model, draws, dir) {
  meta <- fit$metadata()
  ids <- meta$id
  metric <- fit$inv_metric(matrix = FALSE)[as.character(ids)]
  if (any(vapply(metric, is.null, logical(1)))) stop("the fit's CSVs hold no inverse metric for every chain")
  parameters <- model$variables()$parameters
  sizes <- meta$stan_variable_sizes
  zero <- Filter(\(n) is.null(sizes[[n]]) || prod(sizes[[n]]) == 0, names(parameters))
  write_warm_record(dir, warm_inits(draws, ids, parameters, sizes), meta$step_size_adaptation, metric, zero)
}

#' The reference of a warm run, checked: the same cell's finished standard run of the base
#' arm at `ref_seed`, current under this run's code and data, with its warm-start record.
#' @param base the base arm's name
#' @param E,F the cell's election and forecast dates
#' @param ref_seed the reference run's seed
#' @param expected this run's expected meta without the warm keys (`expected_meta()` with no
#'   `warm`): its shas, seed and sampler settings
#' @param parameters the names of the model's parameters: each init must name every one
#'   that the record does not list as of size zero, and no other
#' @return `list(id, chain_ids, inits, step_size, inv_metric, sha256)`: the reference's run
#'   id, its chains (1 to `SAMPLER$chains`), its init files' absolute paths, step sizes and
#'   inverse metrics in chain order, and the sha256 of its `adaptation.json`. An error, each
#'   case with its own message, for: a 2017 cell before the freeze (before anything is read);
#'   `ref_seed` equal to this run's seed; a reference with no `meta.json`; a reference that is
#'   itself warm; a stale reference, whose meta does not match `expected` at the reference's
#'   own seed and iterations (the keys are named); a reference with no warm-start record; and
#'   a record whose chains, step sizes or metrics are malformed, whose init files do not hash
#'   as it records, or whose inits with its parameters of size zero miss a parameter or name
#'   one the model does not declare
#' @export
warm_reference <- function(base, E, F, ref_seed, expected, parameters) {
  box::use(
    R / harness / freeze[forecast_free],
    R / harness / runs[SAMPLER, run_id, run_dir, meta_mismatch, sha256_file]
  )
  if (forecast_free(as.Date(E))) refuse("2017 is not fitted before the freeze (spec section 5.7)")
  id <- run_id(base, E, F, ref_seed)
  if (as.numeric(ref_seed) == as.numeric(expected$seed)) {
    refuse("seed ", expected$seed, " is the seed of its reference ", id, "; give the warm run a seed of its own")
  }
  dir <- run_dir(id)
  meta_path <- file.path(dir, "meta.json")
  if (!file.exists(meta_path)) {
    refuse("reference ", id, " is not finished (no meta.json); fit it first, before this run in the same lane")
  }
  meta <- tryCatch(jsonlite::read_json(meta_path), error = function(e) list())
  if (!is.null(meta$warm_from)) refuse("reference ", id, " is itself a warm run (from ", meta$warm_from[[1]], "); warm from a standard run")
  ref_expected <- expected
  ref_expected$seed <- as.integer(ref_seed)
  for (k in c("iter_warmup", "iter_sampling")) ref_expected[[k]] <- if (is_count(meta[[k]])) meta[[k]] else NA
  stale <- meta_mismatch(meta, ref_expected)
  if (length(stale)) refuse("reference ", id, " is stale (", paste(stale, collapse = ","), "); refit it first")

  record_path <- file.path(dir, WARM_DIR, ADAPTATION)
  if (!file.exists(record_path)) refuse("reference ", id, " has no warm-start record; retire and refit it")
  bad <- function(...) refuse("reference ", id, "'s warm-start record ", ..., "; retire and refit it")
  rec <- tryCatch(jsonlite::read_json(record_path), error = function(e) list())
  chains <- seq_len(SAMPLER$chains)
  ids <- suppressWarnings(as.integer(unlist(rec$chain_ids)))
  if (!identical(ids, chains)) bad("holds chains ", paste(ids, collapse = ","), ", not ", paste(chains, collapse = ","))
  step_size <- unlist(rec$step_size)
  if (length(step_size) != length(chains) || !positive(step_size)) bad("does not hold a positive step size per chain")
  inv_metric <- lapply(rec$inv_metric, \(m) as.numeric(unlist(m)))
  if (length(inv_metric) != length(chains) || !all(vapply(inv_metric, positive, logical(1))) || length(unique(lengths(inv_metric))) != 1) {
    bad("does not hold a positive inverse metric of one length per chain")
  }
  zero <- as.character(unlist(rec$zero_size))
  files <- unlist(rec$init)
  shas <- unlist(rec$init_sha256)
  if (length(files) != length(chains) || length(shas) != length(chains) || !identical(basename(files), files)) {
    bad("does not name one init file per chain")
  }
  inits <- file.path(dir, WARM_DIR, files)
  for (k in chains) {
    if (!file.exists(inits[k]) || !identical(sha256_file(inits[k]), shas[k])) bad("does not match ", files[k])
    named <- c(names(jsonlite::read_json(inits[k])), zero)
    if (anyDuplicated(named)) bad(files[k], " names a parameter it lists as of size zero")
    lacks <- setdiff(parameters, named)
    if (length(lacks)) bad(files[k], " lacks ", paste(lacks, collapse = ", "))
    extra <- setdiff(named, parameters)
    if (length(extra)) bad(files[k], " names what the model does not declare: ", paste(extra, collapse = ", "))
  }
  list(
    id = id, chain_ids = ids, inits = normalizePath(inits, mustWork = TRUE), step_size = as.numeric(step_size),
    inv_metric = inv_metric, sha256 = sha256_file(record_path)
  )
}

#' What a warm fit did not take from its reference.
#' @param ref `warm_reference()`
#' @param chain_ids the fit's chain id of each chain, in the order of `diagnostics`
#'   (`fit$metadata()$id`)
#' @param diagnostics the fit's sampler diagnostics (`fit$sampler_diagnostics()`), a posterior
#'   draws object holding `stepsize__`
#' @param inv_metric the fit's inverse metrics, named by chain id (`fit$inv_metric(matrix =
#'   FALSE)`)
#' @return one line per chain whose step size (at any draw) or inverse metric differs from the
#'   reference's for the same chain id by more than the CSV header's six significant figures,
#'   or that the reference lacks; `character(0)` when the fit sampled with the reference's
#'   adaptation
#' @export
warm_mismatch <- function(ref, chain_ids, diagnostics, inv_metric) {
  x <- unclass(posterior::as_draws_array(diagnostics))
  if (!"stepsize__" %in% dimnames(x)[[3]]) stop("sampler diagnostics lack stepsize__")
  if (length(chain_ids) != dim(x)[2]) stop("chain_ids must name each chain of diagnostics")
  out <- character(0)
  for (j in seq_along(chain_ids)) {
    id <- chain_ids[j]
    k <- match(id, ref$chain_ids)
    if (is.na(k)) {
      out <- c(out, sprintf("chain %s is not a chain of the reference", id))
      next
    }
    steps <- as.numeric(x[, j, "stepsize__"])
    if (!close_to(steps, rep(ref$step_size[k], length(steps)))) {
      out <- c(out, sprintf("chain %s sampled at step size %s, not the reference's %s", id, format(steps[1], digits = 6), format(ref$step_size[k], digits = 6)))
    }
    m <- inv_metric[[as.character(id)]]
    if (is.null(m) || !close_to(as.numeric(m), ref$inv_metric[[k]])) {
      out <- c(out, sprintf("chain %s's inverse metric is not the reference's", id))
    }
  }
  out
}
