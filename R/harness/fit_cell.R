# The cell runner (spec sections 5.5, 5.7, 5.8). A plain script, not a box module.
# Run from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/fit_cell.R <arm> <E> <F> <seed> <iter_warmup> <iter_sampling>
#
# It fits one arm (R/harness/arms.R) on one cell, in results_root()/<run_id> (HARNESS_RESULTS
# overrides results/harness):
#   1. builds the cell's Stan data from info_set(E, F), after validate_inputs();
#   2. computes the expected meta, with the sha256 of the freshly built data;
#   3. prints "SKIP <id> (meta.json matches)" and exits 0 if a finished run matches it; a run
#      directory that does not, or has no meta.json, goes to _stale/ with
#      "STALE <id> <mismatched keys>" (or "STALE <id> (no meta.json)");
#   4. freezes the Stan data and the inputs meta, read-only, in inputs/;
#   5. fits with SAMPLER, the seed and the iterations, the CmdStan CSVs in cmdstan/;
#   6. stops, with no meta.json, unless the fit has 4 chains of iter_sampling draws and the
#      Stan file and the frozen data still hash as expected;
#   7. extracts the parquets, then summarises the diagnostics over the parameters and the
#      scored quantities, writes key_diagnostics.csv and applies the gate;
#   8. writes meta.json last, read-only with the other outputs, so that its presence means the
#      run is complete; prints the DIAG line and "DONE <dir>"; deletes the CmdStan CSVs.
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("fit_cell.R needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
# WHY: the box path below is the working directory, so check it is the project root first.
stopifnot("run from the project root (no R/harness/constants.R here)" = file.exists("R/harness/constants.R"))
options(box.path = normalizePath("."), width = 120, warn = 1)
box::use(
  R / harness / arms[arm],
  R / harness / constants[harness_path],
  R / harness / info_set[info_set, validate_inputs],
  R / harness / runs[
    SAMPLER, run_id, run_dir, sha256_file, stan_data_sha, write_frozen_inputs, expected_meta, meta_mismatch,
    retire_run, summarise_diagnostics, diagnostics_gate, diag_line
  ],
  R / harness / scores[kd_to_nat]
)

#' The builder's meta as JSON values: a Date as an ISO string, a named vector as an object,
#' and a character vector as an array even when it has one element.
json_ready <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x))
  }
  if (is.list(x)) {
    return(lapply(x, json_ready))
  }
  if (!is.null(names(x))) {
    return(as.list(x))
  }
  if (is.character(x)) I(x) else x
}

#' The scored quantities (spec section 5.7) as a draws_array: the national shares of
#' `kd_to_nat()`, named `scored_nat[<flokkur>]`, and each kjördæmi cell's share of its
#' kjördæmi's votes, named `scored_kd[<kjordaemi>,<flokkur>]`.
scored_draws <- function(eday_kd) {
  nat <- kd_to_nat(eday_kd)
  kd <- dplyr::mutate(eday_kd, share = votes / sum(votes), .by = c(.draw, kjordaemi))
  long <- rbind(
    tibble::tibble(nat[c(".chain", ".iteration", ".draw")], variable = paste0("scored_nat[", nat$flokkur, "]"), share = nat$share),
    tibble::tibble(
      kd[c(".chain", ".iteration", ".draw")],
      variable = paste0("scored_kd[", kd$kjordaemi, ",", kd$flokkur, "]"), share = kd$share
    )
  )
  wide <- tidyr::pivot_wider(long, names_from = "variable", values_from = "share")
  posterior::as_draws_array(posterior::as_draws_df(wide))
}

#' R-hat and the bulk and tail ESS of each variable of `x`.
measure <- function(x) posterior::summarise_draws(x, "rhat", "ess_bulk", "ess_tail")

git <- function(...) suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6) {
  stop("usage: Rscript R/harness/fit_cell.R <arm> <E> <F> <seed> <iter_warmup> <iter_sampling>", call. = FALSE)
}
name <- args[1]
numbers <- suppressWarnings(as.numeric(args[4:6]))
a <- arm(name)
id <- run_id(name, args[2], args[3], numbers[1])
E <- as.Date(args[2])
F <- as.Date(args[3])

info <- info_set(E, F)
validate_inputs(info)
built <- a$build(info, a$read_config(a$config))

data_sha <- stan_data_sha(built$data)
expected <- expected_meta(name, numbers[1], numbers[2], numbers[3], a$stan_file, a$config, data_sha)

dir <- run_dir(id)
meta_path <- file.path(dir, "meta.json")
if (file.exists(meta_path)) {
  finished <- tryCatch(jsonlite::read_json(meta_path), error = function(e) list())
  stale <- meta_mismatch(finished, expected)
  if (!length(stale)) {
    cat(sprintf("SKIP %s (meta.json matches)\n", id))
    quit(save = "no", status = 0)
  }
  to <- retire_run(dir)
  cat(sprintf("STALE %s %s\n", id, paste(stale, collapse = ",")))
  cat(sprintf("retired %s to %s\n", dir, to))
} else if (file.exists(dir)) {
  to <- retire_run(dir)
  cat(sprintf("STALE %s (no meta.json)\n", id))
  cat(sprintf("retired %s to %s\n", dir, to))
}

cell <- list(arm = name, E = format(E), F = format(F), id = id)
provenance <- list(
  git_head = git("rev-parse", "HEAD"),
  git_dirty = length(git("status", "--porcelain")) > 0,
  snapshot_manifest_sha256 = sha256_file(harness_path("snapshot_manifest.csv")),
  cmdstan_version = cmdstanr::cmdstan_version(),
  cmdstanr_version = as.character(utils::packageVersion("cmdstanr"))
)
built_meta <- json_ready(built$meta)
if (!identical(write_frozen_inputs(dir, built$data, c(cell, expected, built_meta, provenance)), data_sha)) {
  stop("the frozen Stan data do not hash as the built data")
}

model <- cmdstanr::cmdstan_model(a$stan_file)
cmdstan_dir <- file.path(dir, "cmdstan")
dir.create(cmdstan_dir)
data_json <- file.path(dir, "inputs", "stan_data.json")
start <- Sys.time()
fit <- model$sample(
  data = data_json, seed = expected$seed, chains = SAMPLER$chains, parallel_chains = SAMPLER$parallel_chains,
  init = SAMPLER$init, max_treedepth = SAMPLER$max_treedepth, iter_warmup = expected$iter_warmup,
  iter_sampling = expected$iter_sampling, refresh = 50, output_dir = cmdstan_dir
)
minutes <- as.numeric(difftime(Sys.time(), start, units = "mins"))

lp <- fit$draws("lp__")
if (!all(fit$return_codes() == 0) || posterior::nchains(lp) != SAMPLER$chains ||
  posterior::ndraws(lp) != SAMPLER$chains * expected$iter_sampling) {
  stop("the fit does not hold ", SAMPLER$chains, " chains of ", expected$iter_sampling, " draws; no meta.json written")
}
if (!identical(sha256_file(a$stan_file), expected$stan_sha256)) stop("the Stan file changed during the fit; no meta.json written")
if (!identical(sha256_file(data_json), expected$data_sha256)) stop("the frozen Stan data changed during the fit; no meta.json written")

paths <- a$extract(fit$draws(variables = a$variables, format = "draws_df"), built$meta, dir)
scored <- scored_draws(arrow::read_parquet(paths[["eday_kjordaemi"]]))
params <- fit$draws(variables = names(model$variables()$parameters))
sampler <- fit$diagnostic_summary(quiet = TRUE)
depth <- unclass(fit$sampler_diagnostics())[, , "treedepth__", drop = FALSE]
d <- summarise_diagnostics(params, scored, list(
  num_divergent = sampler$num_divergent, num_max_treedepth = sampler$num_max_treedepth,
  ebfmi = sampler$ebfmi, max_depth = apply(depth, 2, max)
))
key <- rbind(measure(scored), measure(posterior::as_draws_df(arrow::read_parquet(paths[["param_draws"]]))))
key_path <- file.path(dir, "key_diagnostics.csv")
readr::write_csv(key[c("variable", "rhat", "ess_bulk", "ess_tail")], key_path)
gate <- diagnostics_gate(d)

meta <- c(
  cell, expected, built_meta,
  list(minutes = minutes, diagnostics = d, gate = list(pass = gate$pass, reasons = I(gate$reasons))),
  provenance
)
tmp <- tempfile("meta-", tmpdir = dir, fileext = ".json")
# WHY: na = "string" keeps an Inf R-hat as "Inf"; "null" would merge it with a missing value.
jsonlite::write_json(meta, tmp, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "string")
if (!all(Sys.chmod(c(paths, key_path, tmp), mode = "0444"))) stop("could not make the outputs read-only")
if (!file.rename(tmp, meta_path)) stop("could not move ", tmp, " to ", meta_path)

cat(diag_line(id, minutes, d, gate), "\n", sep = "")
if (!gate$pass) cat(sprintf("gate: %s\n", paste(gate$reasons, collapse = "; ")))
cat(sprintf("DONE %s\n", dir))
unlink(cmdstan_dir, recursive = TRUE)
