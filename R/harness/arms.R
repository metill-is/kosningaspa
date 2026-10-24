# The harness arms (spec section 5.5): for each, its Stan file and config, the reader of that
# config, the builder from a cell's info_set() to Stan data and meta, the extractor of a fit's
# draws into the parquets, and the variables the extractor reads. R/harness/fit_cell.R runs
# one arm on one cell.

#' The arms, by name. Each is a list: `stan_file` and `config` (paths under the project
#' root), `read_config` (config path to config), `build` (`info_set()` and config to
#' `list(data, meta)`), `extract` (draws, meta and directory to the parquets) and
#' `variables` (what `extract` reads from a fit).
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
    )
  )
})

#' One arm.
#' @param name the arm's name
#' @return `ARMS[[name]]`; an error, "unknown arm: <name>", unless `name` is one name of
#'   `ARMS`
#' @export
arm <- function(name) {
  if (!is.character(name) || length(name) != 1 || !name %in% names(ARMS)) {
    stop("unknown arm: ", paste(name, collapse = ", "), "; the arms are ", paste(names(ARMS), collapse = ", "), call. = FALSE)
  }
  ARMS[[name]]
}
