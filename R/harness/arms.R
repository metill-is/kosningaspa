# The harness arms (spec section 5.5): for each, its Stan file and config, the reader of that
# config, the builder from a cell's info_set() to Stan data and meta, the extractor of a fit's
# draws into the parquets, the variables the extractor reads, and the arm's role.
# R/harness/fit_cell.R runs one arm on one cell.

#' The roles an arm may have: `reference` (R0), `candidate` (V2), `ablation` (V2 with one
#' component off), `check` (a refit that must reproduce another arm) and `dev` (a step of
#' V2's staircase). `score_cell()` never scores a `check` or `dev` arm. Plan 4 may add roles
#' for the arms it scores.
#' @export
ARM_ROLES <- c("reference", "candidate", "ablation", "check", "dev")

#' The entry of an arm of the V2 family, which runs `Stan/forecast_v2.stan` with its own
#' config.
#' @param config the arm's config, a path under `harness/configs/` relative to the project
#'   root
#' @param role one of `ARM_ROLES`
#' @return `list(stan_file = "Stan/forecast_v2.stan", config, read_config =
#'   read_v2_config, build = v2_stan_data, extract = v2_extract, variables = v2_variables,
#'   role)`, `variables` a function of the config; an error unless `config` is one path
#'   under `harness/configs/` and `role` one of `ARM_ROLES`
#' @export
v2_arm <- function(config, role) {
  box::use(R / harness / v2[read_v2_config, v2_stan_data, v2_extract, v2_variables])
  if (!is.character(config) || length(config) != 1 || !startsWith(config, "harness/configs/")) {
    stop("v2_arm: config must be one path under harness/configs/, not ", paste(config, collapse = ", "), call. = FALSE)
  }
  if (!is.character(role) || length(role) != 1 || !role %in% ARM_ROLES) {
    stop("v2_arm: role must be one of ", paste(ARM_ROLES, collapse = ", "), ", not ", paste(role, collapse = ", "), call. = FALSE)
  }
  list(
    stan_file = "Stan/forecast_v2.stan", config = config, read_config = read_v2_config, build = v2_stan_data,
    extract = v2_extract, variables = v2_variables, role = role
  )
}

#' The arms, by name. Each is a list: `stan_file` and `config` (paths under the project
#' root), `read_config` (config path to config), `build` (`info_set()` and config to
#' `list(data, meta)`), `extract` (draws, meta and directory to the parquets), `variables`
#' (what `extract` reads from a fit: a character vector, or a function of the config) and
#' `role` (one of `ARM_ROLES`). R0 is the reference; `v2s-m0`, the core of V2's staircase,
#' `v2s-p1`, its walk anchored at the closed elections, `v2s-m1`, the core with the
#' pollster biases drifting across epochs, `v2s-m2`, `v2s-m1` with the newcomer contrast's
#' mean and scale estimated, `v2s-m3`, `v2s-m2` with the election-day shock, `v2s-m4`,
#' `v2s-m3` with the kjördæmi deviations walking across elections, `v2s-p2`, `v2s-m4`
#' with those deviations held centred at the closed elections, and `v2s-m5`, `v2s-m4` with
#' the fundamentals pseudo-observation, are dev arms. `v2`, `v2s-m5`'s config with the
#' adopted forms (both 0: neither P-step's form was adopted), is the candidate, and
#' `v2-noepoch`, `v2-nonew`, `v2-noshock`, `v2-staticdelta` and `v2-nofund`, each `v2` with
#' one component switch off (`epoch_bias`, `newcomer_pool`, `shock`, `delta_walk` and
#' `fundamentals`), are its ablations. `v2-perm`, `v2`'s config with `order` = `"reversed"`
#' (both the party and the kjördæmi order), is the check that must reproduce `v2`.
#' `v2s-p3`, `v2`'s config with the anchored walk and `adapt_delta` 0.95, is a dev arm: P1
#' re-tried on V2 (`harness/v2/steps.md`). `v2s-p4`, `v2`'s config with the anchored walk at
#' CmdStan's default `adapt_delta` (0.8), is a dev arm too: P3 without the higher target.
#' `v2s-m6`, `v2`'s config with the post-election multiplier (`post_flex` = 1, §6.2), is a dev
#' arm: the first fix for V2's 2024 MAE (`harness/v2/steps.md`). `v2s-m6-noepoch`, `v2s-m6`'s
#' config with `epoch_bias` = 0, is a dev arm: a constant bias plus the shock, the comparison
#' that shows whether the bias needs to move at all. `v2s-m7`, `v2s-m6`'s config with the epoch
#' walk replaced by the calendar drift (`epoch_bias` = 0, `bias_drift` = 1, §6.4), is a dev arm:
#' the second fix.
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
      variables = R0_EXTRACT_VARIABLES,
      role = "reference"
    ),
    `v2s-m0` = v2_arm("harness/configs/steps/v2s-m0.json", "dev"),
    `v2s-p1` = v2_arm("harness/configs/steps/v2s-p1.json", "dev"),
    `v2s-m1` = v2_arm("harness/configs/steps/v2s-m1.json", "dev"),
    `v2s-m2` = v2_arm("harness/configs/steps/v2s-m2.json", "dev"),
    `v2s-m3` = v2_arm("harness/configs/steps/v2s-m3.json", "dev"),
    `v2s-m4` = v2_arm("harness/configs/steps/v2s-m4.json", "dev"),
    `v2s-p2` = v2_arm("harness/configs/steps/v2s-p2.json", "dev"),
    `v2s-m5` = v2_arm("harness/configs/steps/v2s-m5.json", "dev"),
    v2 = v2_arm("harness/configs/v2.json", "candidate"),
    `v2-noepoch` = v2_arm("harness/configs/v2-noepoch.json", "ablation"),
    `v2-nonew` = v2_arm("harness/configs/v2-nonew.json", "ablation"),
    `v2-noshock` = v2_arm("harness/configs/v2-noshock.json", "ablation"),
    `v2-staticdelta` = v2_arm("harness/configs/v2-staticdelta.json", "ablation"),
    `v2-nofund` = v2_arm("harness/configs/v2-nofund.json", "ablation"),
    `v2-perm` = v2_arm("harness/configs/v2-perm.json", "check"),
    `v2s-p3` = v2_arm("harness/configs/steps/v2s-p3.json", "dev"),
    `v2s-p4` = v2_arm("harness/configs/steps/v2s-p4.json", "dev"),
    `v2s-m6` = v2_arm("harness/configs/steps/v2s-m6.json", "dev"),
    `v2s-m6-noepoch` = v2_arm("harness/configs/steps/v2s-m6-noepoch.json", "dev"),
    `v2s-m7` = v2_arm("harness/configs/steps/v2s-m7.json", "dev")
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
