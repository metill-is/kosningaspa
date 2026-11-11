# The harness arms (spec section 5.5): for each, its Stan file and config, the reader of that
# config, the builder from a cell's info_set() to Stan data and meta, the extractor of a fit's
# draws into the parquets, and the variables the extractor reads.
# R/harness/fit_cell.R runs one arm on one cell.

#' The entry of an arm of the V2 family, which runs `Stan/forecast_v2.stan` with its own
#' config.
#' @param config the arm's config, a path under `harness/configs/` relative to the project
#'   root
#' @return `list(stan_file = "Stan/forecast_v2.stan", config, read_config =
#'   read_v2_config, build = v2_stan_data, extract = v2_extract, variables = V2_VARIABLES)`;
#'   an error unless `config` is one path under `harness/configs/`
#' @export
v2_arm <- function(config) {
  box::use(R / harness / v2[read_v2_config, v2_stan_data, v2_extract, V2_VARIABLES])
  if (!is.character(config) || length(config) != 1 || !startsWith(config, "harness/configs/")) {
    stop("v2_arm: config must be one path under harness/configs/, not ", paste(config, collapse = ", "), call. = FALSE)
  }
  list(
    stan_file = "Stan/forecast_v2.stan", config = config, read_config = read_v2_config, build = v2_stan_data,
    extract = v2_extract, variables = V2_VARIABLES
  )
}

#' The arms, by name. Each is a list: `stan_file` and `config` (paths under the project
#' root), `read_config` (config path to config), `build` (`info_set()` and config to
#' `list(data, meta)`), `extract` (draws, meta and directory to the parquets) and
#' `variables` (the character vector of what `extract` reads from a fit). `r0` is the 2024
#' model and `v2` the forecast model of `Stan/forecast_v2.stan`.
#' @export
ARMS <- local({
  box::use(R / harness / r0[r0_stan_data, read_r0_config, r0_extract, R0_EXTRACT_VARIABLES])
  list(
    r0 = list(
      stan_file = "Stan/polling_and_fundamentals_kjordaemi_r0.stan",
      config = "harness/configs/r0.json",
      read_config = read_r0_config,
      build = r0_stan_data,
      extract = r0_extract,
      variables = R0_EXTRACT_VARIABLES
    ),
    v2 = v2_arm("harness/configs/v2.json")
  )
})

#' One arm, or a warm run of one.
#' @param name the arm's name: a name of `ARMS`, or `<base>-w<ref_seed>`, a warm run of the arm
#'   `<base>` from the same cell's seed-`<ref_seed>` run of `<base>` (R/harness/warm.R)
#' @return `ARMS[[name]]`; for a warm run, `ARMS[[base]]` with `warm = list(base, ref_seed)`
#'   added, `ref_seed` an integer; an error, "unknown arm: <name>", for any other name
#' @export
arm <- function(name) {
  one <- is.character(name) && length(name) == 1 && !is.na(name)
  warm <- one && grepl("^.+-w(0|[1-9][0-9]{0,9})$", name)
  base <- if (warm) sub("-w[0-9]+$", "", name) else name
  ref_seed <- if (warm) as.numeric(sub("^.*-w", "", name)) else NA
  if (!one || !base %in% names(ARMS) || isTRUE(ref_seed > .Machine$integer.max)) {
    stop("unknown arm: ", paste(name, collapse = ", "), "; the arms are ", paste(names(ARMS), collapse = ", "), call. = FALSE)
  }
  if (!warm) {
    return(ARMS[[name]])
  }
  c(ARMS[[base]], list(warm = list(base = base, ref_seed = as.integer(ref_seed))))
}
