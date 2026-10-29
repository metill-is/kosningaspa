# The P-step equivalence check (the Phase 2 plan's Decision 6, condition 1): a P-step
# reparameterises Stan/forecast_v2.stan and must leave its posterior unchanged. The reference
# side is the config with the named form(s) set to 0; the new side is the config as written.
# Each point of one side maps to the other through the natural quantity the form changes (the
# walk theta for walk_form), every other parameter copied, and
#   (lp_new - ld_new) - (lp_ref - ld_ref) = constant,
# where lp is log_prob(jacobian = TRUE) and ld each side's log |d theta / d its walk
# parameters|, the sum of the log-Jacobians of its non-centred maps from standard normals to
# theta:
#   ref_walk     the reference walk, sum over its G - 1 forward steps of
#                log|L_walk| + (P - 1)/2 log v_t, v_t = Delta_t s_t^2;
#   new_bridge   each bridge step of the anchored walk, the same with v_t replaced by the
#                bridge's conditional variance v_t (cv_b - cv_t) / R, R = cv_b - cv_{t-1};
#   new_forward  each forward step after the last anchor, as in the reference walk;
#   new_anchor   each anchor's absent block, log|chol(Q_BB)| + |B_j|/2 log V_j.
# The present blocks contribute none: they are centred, and their density is on theta itself.
# Each negative control drops one term and must then fail; a term constant over the points
# cannot be checked so, and fails the check as not exercised. A point with a bounded log
# concentration at its cap is outside the claim: counted and excluded. A point whose log
# density is not finite on both sides is dropped and counted; one finite on one side only
# fails the check.
#
# Points. On a fitted cell: --n posterior and --n-off off-posterior points, distinct draws of
# a 4 x (100 + 40) fit of the new side at seed 5100 and max_treedepth 10 (draw_forecast = 0),
# the latter with each OFF_SCALES element multiplied by exp(N(0, 0.5)); they map from the new
# side to the reference. On a 2017 cell (forecast_free(), Decision 9): --n prior points,
# v2_simulate() draws of the reference side at seeds 5101, 5102, ..., mapped from the
# reference to the new side, and no fit. The points' generated quantities, run on both sides
# by CmdStan's standalone generate_quantities at seed 1 and sig_figs = 18 (the parameter
# columns flattened in declaration order, column-major, since it matches them by position),
# must agree; on a 2017 cell neither side may write votes_kd, pi_nat_eday or pi_latent_E.
#
# Pass: the constant's range over all kept points below tol = 1e-12 max(1, |lp_new|); under
# --form=walk every negative control exercised and above tol; the generated quantities within
# 1e-9 relative (|a - b| / max(1, |b|)); theta within 1e-8 on both sides; no one-sided point;
# no posterior point dropped; and at least 5 kept points of each kind. With fewer than 5 kept
# points of the main kind (posterior, or prior on a 2017 cell) no generated quantities are
# compared, and the check fails on that count. check_reasons() gives the failed conditions.
#
# Usage, from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/harness/v2_equivalence.R --config=<config> --form=walk|delta|both
#     --cell=<E>,<F> --out=<csv> [--n=30] [--n-off=15] [--stan=Stan/forecast_v2.stan] [--reverse]
# The config is a path under harness/configs/. --reverse sets `order` to "reversed" on both
# sides, so the coordinate party is the last of the default order. Both sides' data are
# v2_stan_data()'s under a temporary root (no freeze), so a 2017 cell is its pre-freeze form.
# Only the walk form's map is built: --form=delta and --form=both stop until the plan's Task 13
# builds the delta map. It prints the coordinate party, each point's kind, constant and path
# error, the bookkeeping, the negative controls, the generated quantities' largest relative
# difference (never a value of one) and a last line, EQUIV PASS or EQUIV FAIL, and exits 1 on
# a FAIL. --out, a file in an existing directory, gets one summary row per check (E, F,
# order, verdict, then the summary columns), replacing an earlier row of the same E, F and
# order; the file is created with a header if absent. Once the command line is accepted, a
# provisional FAIL row (reasons "incomplete") is written first, so a check that stops before
# its end leaves no earlier verdict of its cell and order standing.
#
# Stops. Before that row: a bad command line, --form=delta or both, a config whose form is 0,
# no Stan file, and point counts it cannot take (too few, more than the fit draws, or
# off-posterior points on a 2017 cell). After it, the row left standing: the two sides'
# coordinate parties differ, a 2017 cell's data draw a forecast or its generated quantities
# hold a forecast variable, the Stan file's parameters are not v2_par_dims()' in its order,
# the anchored map leaves a z_walk column unset, a parameter has the wrong number of values,
# or the standalone generate_quantities run fails.
#
# The definitions also load as a box module (its test imports EQUIVALENCE_SUMMARY,
# write_equivalence_row(), provisional_row(), control_verdicts() and check_reasons()); the
# script part runs only when box::name() is NULL. Every failure outside the check itself
# stops with "v2_equivalence: ...".
if (is.null(box::name())) {
  if (!isTRUE(l10n_info()[["UTF-8"]])) stop("v2_equivalence: needs a UTF-8 locale: run under LC_ALL=en_US.UTF-8", call. = FALSE)
  # WHY: the box path below is the working directory, so check it is the project root first.
  if (!file.exists("R/harness/constants.R")) stop("v2_equivalence: run from the project root (no R/harness/constants.R here)", call. = FALSE)
  options(box.path = normalizePath("."), width = 120, warn = 1)
}
box::use(
  R / harness / constants[project_root],
  R / harness / freeze[forecast_free],
  R / harness / v2_reference[chol_of_gram, walk_rows, walk_scale, V2_LOG_CAPS]
)

PREFIX <- "v2_equivalence: "
FORMS <- c(walk = "walk_form", delta = "delta_form")
# The points' fit (the plan's sampler table) and the prior points' first seed.
POINTS_FIT <- list(seed = 5100L, iter_warmup = 100L, iter_sampling = 40L, max_treedepth = 10L)
GQ_SEED <- 1L
TOL_REL <- 1e-12
GQ_TOL <- 1e-9
PATH_TOL <- 1e-8
MIN_KEPT <- 5L
FORECAST_GQ <- c("votes_kd", "pi_nat_eday", "pi_latent_E")
JACOBIAN_TERMS <- c("ref_walk", "new_anchor", "new_bridge", "new_forward")

#' The columns of a summary row of the equivalence CSV, in order: compare_runs.R's
#' EQUIVALENCE_COLUMNS (E, F, order, verdict) first.
#' @export
EQUIVALENCE_SUMMARY <- c(
  "E", "F", "order", "verdict", "form", "config", "stan_sha256", "n_posterior", "n_off", "n_prior", "n_rejected",
  "n_outside", "n_one_sided", "const_range", "tol", "path_err", "gq_points", "gq_max_rel",
  paste0("control_", JACOBIAN_TERMS), "reasons"
)

fail <- function(...) stop(PREFIX, ..., call. = FALSE)

#' Write one summary row to the equivalence CSV `path`: it replaces the row with the same E, F
#' and order where there is one and is appended otherwise; the file is created with a header if
#' it does not exist. Columns are the union of the file's and the row's, the file's first, a
#' missing value empty. Written atomically.
#' @param path the CSV
#' @param row a named list or one-row data frame of `EQUIVALENCE_SUMMARY`'s columns
#' @return `path`, invisibly
#' @export
write_equivalence_row <- function(path, row) {
  row <- as.data.frame(lapply(row, as.character), stringsAsFactors = FALSE)
  if (nrow(row) != 1 || !all(c("E", "F", "order", "verdict") %in% names(row))) {
    fail("a summary row needs one row with E, F, order and verdict")
  }
  old <- if (file.exists(path)) utils::read.csv(path, colClasses = "character", check.names = FALSE) else row[0, ]
  cols <- union(names(old), names(row))
  for (n in setdiff(cols, names(old))) old[[n]] <- rep(NA_character_, nrow(old))
  for (n in setdiff(cols, names(row))) row[[n]] <- NA_character_
  old <- old[cols]
  row <- row[cols]
  same <- which(old$E == row$E & old$F == row$F & old$order == row$order)
  if (length(same)) {
    old[same[1], ] <- row
    if (length(same) > 1) old <- old[-same[-1], , drop = FALSE]
  } else {
    old <- rbind(old, row)
  }
  tmp <- tempfile("equivalence-", tmpdir = dirname(path), fileext = ".csv")
  utils::write.csv(old, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, path)) fail("could not write ", path)
  invisible(path)
}

#' The provisional summary row of a check, written before anything is built or fitted: verdict
#' FAIL and reasons "incomplete", so a check that stops before its end leaves no earlier verdict
#' of the same E, F and order standing. The check's own row replaces it.
#' @param E,F the cell, ISO dates
#' @param order `"default"` or `"reversed"`
#' @param form the --form value
#' @param config the config, a path under the project root
#' @param stan_sha256 the Stan file's sha256
#' @return a named list of `EQUIVALENCE_SUMMARY`'s columns, the others NA
#' @export
provisional_row <- function(E, F, order, form, config, stan_sha256) {
  row <- as.list(stats::setNames(rep(NA_character_, length(EQUIVALENCE_SUMMARY)), EQUIVALENCE_SUMMARY))
  utils::modifyList(row, list(
    E = E, F = F, order = order, verdict = "FAIL", form = form, config = config, stan_sha256 = stan_sha256,
    reasons = "incomplete"
  ))
}

#' The negative controls' verdicts: "not exercised" for a term constant over the points (NA),
#' "fails" for a control whose constant range exceeds tol, as it must, and "PASSES" otherwise.
#' @param controls the constant's range with each term left out, named by `JACOBIAN_TERMS`
#' @param tol the tolerance
#' @return a character vector named as `controls`
#' @export
control_verdicts <- function(controls, tol) {
  stats::setNames(ifelse(is.na(controls), "not exercised", ifelse(controls > tol, "fails", "PASSES")), names(controls))
}

#' Why a check fails, in the order its EQUIV line and the CSV's `reasons` give them; none when
#' it passes. The generated quantities count only when compared: with fewer than `MIN_KEPT`
#' kept points of the main kind they are not, and the check fails on that count instead.
#' @param form the --form value; under "walk" every control must be exercised
#' @param const_range the constant's range over the kept points
#' @param tol the tolerance
#' @param controls the controls' constant ranges, named by `JACOBIAN_TERMS`, NA where the term
#'   is not exercised
#' @param main_kind "posterior", or "prior" on a 2017 cell
#' @param gq_max_rel the generated quantities' largest relative difference (NA if not compared;
#'   an NA or NaN where they are compared fails, as an NA constant range or path error does)
#' @param path_err the largest path error
#' @param one_sided the points finite on one side only
#' @param posterior_dropped whether a posterior point was dropped
#' @param kept_n the kept points of each kind the check draws, named by kind
#' @return a character vector
#' @export
check_reasons <- function(form, const_range, tol, controls, main_kind, gq_max_rel, path_err, one_sided,
                          posterior_dropped, kept_n) {
  verdict <- control_verdicts(controls, tol)
  gq_ran <- kept_n[[main_kind]] >= MIN_KEPT
  # WHY: isTRUE() so that an NA or NaN measure fails its condition instead of stopping the check.
  as.character(c(
    if (!isTRUE(const_range < tol)) sprintf("constant range %.3g is not below tol %.3g", const_range, tol),
    if (any(verdict == "PASSES")) paste("a negative control passes:", paste(names(verdict)[verdict == "PASSES"], collapse = ", ")),
    if (form == "walk" && any(verdict == "not exercised")) {
      paste("a negative control is not exercised:", paste(names(verdict)[verdict == "not exercised"], collapse = ", "))
    },
    if (gq_ran && !isTRUE(gq_max_rel < GQ_TOL)) sprintf("generated quantities differ by %.3g relative", gq_max_rel),
    if (!isTRUE(path_err < PATH_TOL)) sprintf("path error %.3g", path_err),
    if (one_sided > 0) sprintf("%d points finite on one side only", one_sided),
    if (posterior_dropped) "a posterior point was dropped",
    if (any(kept_n < MIN_KEPT)) sprintf("fewer than %d kept points of a kind", MIN_KEPT)
  ))
}

#' The command line as a list, or an error naming what is wrong.
parse_args <- function(args) {
  known <- c("config", "form", "cell", "out", "n", "n-off", "stan", "reverse")
  name <- sub("=.*$", "", sub("^--", "", args))
  bad <- args[!startsWith(args, "--") | !name %in% known]
  if (length(bad)) fail("unknown argument ", paste(bad, collapse = " "))
  if (anyDuplicated(name)) fail("an argument is given twice")
  value <- function(n, default = NULL) {
    hit <- args[name == n]
    if (!length(hit)) {
      return(default)
    }
    if (!grepl("=", hit)) fail("--", n, " needs a value")
    sub("^[^=]*=", "", hit)
  }
  count <- function(n, default) {
    x <- value(n, default)
    if (!grepl("^[0-9]+$", x)) fail("--", n, " must be a whole number, not ", x)
    as.integer(x)
  }
  for (n in c("config", "form", "cell", "out")) if (is.null(value(n))) fail("--", n, " is required")
  if ("reverse" %in% name && args[name == "reverse"] != "--reverse") fail("--reverse takes no value")
  cell <- strsplit(value("cell"), ",", fixed = TRUE)[[1]]
  dates <- as.Date(cell, format = "%Y-%m-%d")
  if (length(cell) != 2 || anyNA(dates)) fail("--cell must be <E>,<F> as ISO dates, not ", value("cell"))
  config <- value("config")
  if (!startsWith(config, "harness/configs/") || !file.exists(file.path(project_root(), config))) {
    fail("--config must be a file under harness/configs/, not ", config)
  }
  form <- value("form")
  if (!form %in% c(names(FORMS), "both")) fail("--form must be walk, delta or both, not ", form)
  if (!dir.exists(dirname(value("out")))) fail("--out must be a file in an existing directory, not ", value("out"))
  list(
    config = config, form = form, E = dates[1], F = dates[2], out = value("out"), n = count("n", "30"),
    n_off = count("n-off", "15"), n_off_given = "n-off" %in% name, stan = value("stan", "Stan/forecast_v2.stan"),
    reverse = "reverse" %in% name
  )
}

#' The anchored walk's layout on `data`, as Stan/forecast_v2.stan's transformed data builds
#' it: the anchors (grid_res), each anchor's present and absent coordinates (1..P - 1, party
#' c + 1; present means national votes, idx_res), w_off (n_closed x (P - 1): the result's
#' national log shares centred over its parties, 0 for an absent coordinate) and zcol (the
#' z_walk column of each date that is neither date 1 nor an anchor, else 0).
walk_layout <- function(data) {
  P <- data$P
  K <- data$K
  G <- data$G
  anchors <- as.integer(c(data$grid_res))
  idx <- matrix(as.integer(data$idx_res), data$n_closed, P)
  present <- absent <- vector("list", length(anchors))
  w_off <- matrix(0, length(anchors), P - 1)
  for (j in seq_along(anchors)) {
    s <- idx[j, seq_len(data$n_rep_res[j])]
    l <- log(colSums(matrix(data$y_res[j, , ], K, P))[s])
    l <- l - mean(l)
    w_off[j, s[s > 1] - 1] <- l[s > 1]
    present[[j]] <- sort(s[s > 1] - 1L)
    absent[[j]] <- setdiff(seq_len(P - 1), present[[j]])
  }
  zcol <- integer(G)
  free <- setdiff(2:G, anchors)
  zcol[free] <- seq_along(free)
  list(anchors = anchors, present = present, absent = absent, w_off = w_off, zcol = zcol)
}

#' The walk's parameters of the point `p` as plain numbers: sigma (P), L_Omega (P x P) and
#' tau_camp, whatever shapes constrain_variables() gave them.
walk_point <- function(p) {
  sigma <- as.numeric(p$sigma)
  list(sigma = sigma, L_Omega = matrix(as.numeric(p$L_Omega), length(sigma)), tau_camp = as.numeric(p$tau_camp))
}

#' The walk's variance multipliers on `data` at `tau_camp`: v[t] of the step from t - 1 to t
#' (v[1] = 0) and their running total cv.
walk_var <- function(tau_camp, data) {
  v <- c(0, walk_scale(tau_camp, data)^2)
  list(v = v, cv = cumsum(v))
}

#' The reference walk's z_walk ((P - 1) x (G - 1)) that reproduces `theta` (P x G) at the
#' point `p` (its sigma, L_Omega and tau_camp).
to_reference <- function(theta, p, data) {
  p <- walk_point(p)
  L <- chol_of_gram(walk_rows(p$sigma, p$L_Omega))
  X <- theta[-1, , drop = FALSE]
  step <- X[, -1, drop = FALSE] - X[, -ncol(X), drop = FALSE]
  forwardsolve(L, step / rep(walk_scale(p$tau_camp, data), each = nrow(X)))
}

#' The anchored walk's anchor_w (n_closed x (P - 1)) and z_walk ((P - 1) x (G - 1 - n_closed))
#' that reproduce `theta` at the point `p` on `data`, by `lay` (walk_layout()).
to_anchored <- function(theta, p, data, lay) {
  p <- walk_point(p)
  P1 <- data$P - 1L
  G <- data$G
  B <- walk_rows(p$sigma, p$L_Omega)
  L <- chol_of_gram(B)
  w <- walk_var(p$tau_camp, data)
  v <- w$v
  cv <- w$cv
  X <- theta[-1, , drop = FALSE]
  anchor_w <- matrix(0, length(lay$anchors), P1)
  z <- matrix(NA_real_, P1, G - 1L - length(lay$anchors))
  prev <- 1L
  for (j in seq_along(lay$anchors)) {
    a <- lay$anchors[j]
    pr <- lay$present[[j]]
    ab <- lay$absent[[j]]
    if (length(ab)) {
      anchor_w[j, ab] <- forwardsolve(chol_of_gram(B[ab, , drop = FALSE]), (X[ab, a] - X[ab, prev]) / sqrt(cv[a] - cv[prev]))
      anchor_w[j, pr] <- X[pr, a] + sum(X[ab, a]) / (length(pr) + 1) - lay$w_off[j, pr]
    } else {
      anchor_w[j, ] <- X[, a] - lay$w_off[j, ]
    }
    for (t in seq_len(max(0L, a - prev - 1L)) + prev) {
      R <- cv[a] - cv[t - 1]
      s <- sqrt(v[t] * (cv[a] - cv[t]) / R)
      z[, lay$zcol[t]] <- forwardsolve(L, (X[, t] - X[, t - 1] - (v[t] / R) * (X[, a] - X[, t - 1])) / s)
    }
    prev <- a
  }
  for (t in seq_len(G - prev) + prev) z[, lay$zcol[t]] <- forwardsolve(L, (X[, t] - X[, t - 1]) / sqrt(v[t]))
  if (anyNA(z)) fail("the anchored map left a z_walk column unset")
  list(anchor_w = anchor_w, z_walk = z)
}

#' The log-Jacobian terms of `JACOBIAN_TERMS` at the point `p` on `data`, by `lay`: they
#' depend only on sigma, L_Omega, tau_camp and the data, never on theta.
walk_ljac <- function(p, data, lay) {
  p <- walk_point(p)
  P1 <- data$P - 1L
  G <- data$G
  B <- walk_rows(p$sigma, p$L_Omega)
  ldL <- sum(log(diag(chol_of_gram(B))))
  w <- walk_var(p$tau_camp, data)
  v <- w$v
  cv <- w$cv
  out <- c(ref_walk = (G - 1) * ldL + P1 / 2 * sum(log(v[-1])), new_anchor = 0, new_bridge = 0, new_forward = 0)
  prev <- 1L
  for (j in seq_along(lay$anchors)) {
    a <- lay$anchors[j]
    ab <- lay$absent[[j]]
    if (length(ab)) {
      out[["new_anchor"]] <- out[["new_anchor"]] + length(ab) / 2 * log(cv[a] - cv[prev]) +
        sum(log(diag(chol_of_gram(B[ab, , drop = FALSE]))))
    }
    for (t in seq_len(max(0L, a - prev - 1L)) + prev) {
      out[["new_bridge"]] <- out[["new_bridge"]] + ldL + P1 / 2 * log(v[t] * (cv[a] - cv[t]) / (cv[a] - cv[t - 1]))
    }
    prev <- a
  }
  for (t in seq_len(G - prev) + prev) out[["new_forward"]] <- out[["new_forward"]] + ldL + P1 / 2 * log(v[t])
  out
}

#' The parameters `pars` (a constrain_variables() list) as one row of CmdStan columns, in
#' declaration order (`dims`, v2_par_dims()) and Stan's column-major element order, a scalar
#' (`scalar`) under its bare name; a size-0 parameter has no column.
flatten <- function(pars, dims, scalar) {
  unlist(lapply(names(dims), function(v) {
    d <- dims[[v]]
    if (prod(d) == 0) {
      return(NULL)
    }
    x <- as.numeric(pars[[v]])
    if (length(x) != prod(d)) fail(v, " has ", length(x), " values, not ", prod(d))
    nm <- if (scalar[[v]]) v else paste0(v, "[", apply(as.matrix(expand.grid(lapply(d, seq_len))), 1, paste, collapse = ","), "]")
    stats::setNames(x, nm)
  }))
}

#' The generated quantities of `stan` on `data` at the parameter rows `X` (flatten()), by
#' CmdStan's standalone generate_quantities on the executable alone, at seed GQ_SEED and
#' sig_figs = 18: a draws matrix with one row per row of X.
gq_rows <- function(stan, data, X) {
  box::use(R / harness / v2_methods[v2_model, data_json])
  dir <- tempfile("v2-equivalence-gq-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  plain <- cmdstanr::cmdstan_model(exe_file = v2_model(stan)$exe_file())
  gq <- plain$generate_quantities(
    fitted_params = posterior::as_draws_array(posterior::as_draws_matrix(X)), data = data_json(data, dir),
    seed = GQ_SEED, sig_figs = 18, output_dir = dir
  )
  if (!all(gq$return_codes() == 0)) fail("the standalone generate_quantities run failed")
  unclass(posterior::as_draws_matrix(gq$draws()))
}

#' A range, or Inf unless every value is finite.
spread <- function(x) if (length(x) && all(is.finite(x))) diff(range(x)) else Inf

main <- function(args) {
  box::use(
    R / harness / info_set[info_set, read_inputs],
    R / harness / v2[read_v2_config, v2_stan_data],
    R / harness / v2_methods[v2_methods, short_fit_draws, OFF_SCALES],
    R / harness / v2_reference[v2_par_dims, v2_simulate]
  )
  a <- parse_args(args)
  cfg <- read_v2_config(file.path(project_root(), a$config))
  if (a$reverse) cfg$order <- "reversed"
  forms <- if (a$form == "both") unname(FORMS) else FORMS[[a$form]]
  if (!identical(forms, "walk_form")) fail("only the walk form's map is built: --form=walk (the delta map needs the plan's Task 13)")
  off <- forms[vapply(forms, \(f) !identical(as.integer(cfg[[f]]), 1L), logical(1))]
  if (length(off)) fail("the config's ", paste(off, collapse = " and "), " is 0: both sides would be the same model")
  stan <- if (startsWith(a$stan, "/")) a$stan else file.path(project_root(), a$stan)
  if (!file.exists(stan)) fail("no Stan file ", a$stan)
  root <- tempfile("v2-equivalence-root-")
  dir.create(root)
  prior_only <- forecast_free(a$E, root)
  if (prior_only && a$n_off_given && a$n_off > 0) fail("a 2017 cell takes prior points only: --n-off must be 0 or left out")
  n_off <- if (prior_only) 0L else a$n_off
  if (a$n < MIN_KEPT) fail("--n must be at least ", MIN_KEPT)
  if (!prior_only && n_off < MIN_KEPT) fail("--n-off must be at least ", MIN_KEPT)
  if (!prior_only && a$n + n_off > 4L * POINTS_FIT$iter_sampling) fail("--n plus --n-off exceeds the fit's ", 4L * POINTS_FIT$iter_sampling, " draws")
  stan_sha256 <- digest::digest(file = stan, algo = "sha256")
  write_equivalence_row(a$out, provisional_row(format(a$E), format(a$F), cfg$order, a$form, a$config, stan_sha256))
  ref_cfg <- cfg
  ref_cfg[forms] <- 0L

  info <- info_set(a$E, a$F, read_inputs())
  b_ref <- v2_stan_data(info, ref_cfg, root = root)
  b_new <- v2_stan_data(info, cfg, root = root)
  if (!identical(b_ref$meta$coordinate_party, b_new$meta$coordinate_party)) fail("the two sides' coordinate parties differ")
  d_ref <- b_ref$data
  d_new <- b_new$data
  if (prior_only && !(d_ref$draw_forecast == 0L && d_new$draw_forecast == 0L)) fail("a 2017 cell's data must have draw_forecast = 0")
  P <- d_new$P
  G <- d_new$G
  lay <- walk_layout(d_new)
  cat(sprintf(
    "cell %s, %s, order %s, form %s: P = %d, G = %d, %d anchors (absent coordinates per anchor: %s)\n",
    format(a$E), format(a$F), cfg$order, a$form, P, G, length(lay$anchors), paste(lengths(lay$absent), collapse = ", ")
  ))
  cat(sprintf("coordinate party: %s\n", b_new$meta$coordinate_party))

  m_ref <- v2_methods(stan, d_ref)
  m_new <- v2_methods(stan, d_new)
  dims_ref <- v2_par_dims(d_ref)
  dims_new <- v2_par_dims(d_new)
  info_pars <- m_new$runset$args$model_variables$parameters
  if (!identical(names(dims_new), names(info_pars))) fail("the Stan file's parameters are not v2_par_dims()' in its order")
  scalar <- vapply(names(dims_new), \(v) info_pars[[v]]$dimensions == 0, logical(1))

  if (prior_only) {
    points <- lapply(seq_len(a$n), \(i) list(pars = v2_simulate(d_ref, seed = POINTS_FIT$seed + i, observe = FALSE)$pars, kind = "prior", from = "ref"))
  } else {
    draws <- short_fit_draws(m_new, d_new, POINTS_FIT$seed, POINTS_FIT$iter_warmup, POINTS_FIT$iter_sampling, POINTS_FIT$max_treedepth)
    points <- withr::with_seed(POINTS_FIT$seed, {
      pick <- sample(length(draws), a$n + n_off)
      lapply(seq_along(pick), function(i) {
        p <- draws[[pick[i]]]
        is_off <- i > a$n
        if (is_off) for (s in OFF_SCALES) p[[s]] <- p[[s]] * exp(stats::rnorm(length(p[[s]]), 0, 0.5))
        list(pars = p, kind = if (is_off) "off-posterior" else "posterior", from = "new")
      })
    })
  }

  constrained <- function(m, u) m$constrain_variables(u, transformed_parameters = TRUE, generated_quantities = FALSE)
  at_cap <- function(cp) any(cp$log_phi >= V2_LOG_CAPS[["log_phi"]]) || cp$log_phi_K >= V2_LOG_CAPS[["log_phi_K"]] || cp$log_kappa >= V2_LOG_CAPS[["log_kappa"]]
  walk_pars <- c("anchor_w", "z_walk")
  evaluate <- function(pt) {
    if (pt$from == "new") {
      u_new <- m_new$unconstrain_variables(pt$pars)
      c_new <- constrained(m_new, u_new)
      if (at_cap(c_new)) {
        return(list(kind = pt$kind, outside = TRUE))
      }
      theta <- matrix(c_new$theta, P, G)
      p_new <- c_new[names(dims_new)]
      p_ref <- utils::modifyList(p_new[setdiff(names(p_new), walk_pars)], list(
        anchor_w = array(numeric(0), dims_ref$anchor_w), z_walk = to_reference(theta, p_new, d_ref)
      ))[names(dims_ref)]
      u_ref <- m_ref$unconstrain_variables(p_ref)
      c_ref <- constrained(m_ref, u_ref)
    } else {
      u_ref <- m_ref$unconstrain_variables(pt$pars)
      c_ref <- constrained(m_ref, u_ref)
      if (at_cap(c_ref)) {
        return(list(kind = pt$kind, outside = TRUE))
      }
      theta <- matrix(c_ref$theta, P, G)
      p_ref <- c_ref[names(dims_ref)]
      p_new <- utils::modifyList(p_ref[setdiff(names(p_ref), walk_pars)], to_anchored(theta, p_ref, d_new, lay))[names(dims_new)]
      u_new <- m_new$unconstrain_variables(p_new)
      c_new <- constrained(m_new, u_new)
    }
    j <- walk_ljac(p_new, d_new, lay)
    lp_ref <- m_ref$log_prob(u_ref, jacobian = TRUE)
    lp_new <- m_new$log_prob(u_new, jacobian = TRUE)
    list(
      kind = pt$kind, outside = FALSE, lp_ref = lp_ref, lp_new = lp_new, jac = j,
      const = (lp_new - sum(j[startsWith(names(j), "new_")])) - (lp_ref - j[["ref_walk"]]),
      path_err = max(abs(matrix(c_ref$theta, P, G) - matrix(c_new$theta, P, G))),
      row_ref = flatten(c_ref, dims_ref, scalar), row_new = flatten(c_new, dims_new, scalar)
    )
  }
  res <- lapply(points, evaluate)

  kind <- vapply(res, \(r) r$kind, "")
  outside <- vapply(res, \(r) r$outside, logical(1))
  lp_ok <- function(side) vapply(res, \(r) !r$outside && is.finite(r[[side]]), logical(1))
  both <- lp_ok("lp_ref") & lp_ok("lp_new")
  one_sided <- sum(xor(lp_ok("lp_ref"), lp_ok("lp_new")))
  rejected <- !outside & !both
  for (i in seq_along(res)) {
    r <- res[[i]]
    cat(if (r$outside) {
      sprintf("point %d (%s): a bounded log concentration at its cap, excluded\n", i, r$kind)
    } else {
      sprintf(
        "point %d (%s): lp_ref %.10g, lp_new %.10g, constant %.12g, path error %.2e\n",
        i, r$kind, r$lp_ref, r$lp_new, r$const, r$path_err
      )
    })
  }
  kinds <- unique(kind)
  count <- function(k, x) sum(x & kind == k)
  cat(sprintf("points at a cap (excluded): %s\n", paste(sprintf("%s %d", kinds, vapply(kinds, count, 0L, outside)), collapse = ", ")))
  cat(sprintf(
    "points dropped (log density not finite on both sides): %s; finite on one side only: %d\n",
    paste(sprintf("%s %d", kinds, vapply(kinds, count, 0L, rejected)), collapse = ", "), one_sided
  ))
  kept <- res[both]
  kept_kind <- kind[both]
  const <- vapply(kept, \(r) r$const, 0)
  lp_new <- vapply(kept, \(r) r$lp_new, 0)
  path_err <- if (length(kept)) max(vapply(kept, \(r) r$path_err, 0)) else Inf
  tol <- TOL_REL * max(1, abs(lp_new))
  for (k in kinds) {
    x <- const[kept_kind == k]
    cat(sprintf("%s: n = %d, constant range %.3e, mean %.6g; lp_new spans %.4g\n", k, length(x), spread(x), mean(x), spread(lp_new[kept_kind == k])))
  }
  jac <- if (length(kept)) do.call(rbind, lapply(kept, \(r) r$jac)) else matrix(0, 0, length(JACOBIAN_TERMS), dimnames = list(NULL, JACOBIAN_TERMS))
  controls <- vapply(JACOBIAN_TERMS, function(t) {
    if (spread(jac[, t]) == 0) {
      return(NA_real_)
    }
    spread(const + if (startsWith(t, "new_")) jac[, t] else -jac[, t])
  }, 0)
  control_verdict <- control_verdicts(controls, tol)
  cat(sprintf(
    "negative control without_%s: %s\n", JACOBIAN_TERMS,
    ifelse(is.na(controls), "the term is constant over these points, so it is not exercised, which fails the check",
      sprintf("constant range %.3g (%s)", controls, ifelse(controls > tol, "fails, as it must", "PASSES: the check cannot see this term"))
    )
  ), sep = "")

  main_kind <- if (prior_only) "prior" else "posterior"
  gq_idx <- which(kept_kind == main_kind)
  # WHY: with too few kept points of the main kind the check fails on MIN_KEPT; comparing nothing would only stop it.
  gq_ran <- length(gq_idx) >= MIN_KEPT
  gq_max_rel <- NA_real_
  if (gq_ran) {
    gq_ref <- gq_rows(stan, d_ref, do.call(rbind, lapply(kept[gq_idx], \(r) r$row_ref)))
    gq_new <- gq_rows(stan, d_new, do.call(rbind, lapply(kept[gq_idx], \(r) r$row_new)))
    forecast_cols <- function(x) grep(paste0("^(", paste(FORECAST_GQ, collapse = "|"), ")(\\[|$)"), colnames(x), value = TRUE)
    if (prior_only && length(c(forecast_cols(gq_ref), forecast_cols(gq_new)))) {
      fail("a 2017 cell's generated quantities hold a forecast variable; stopped before comparing them")
    }
    gq_same_cols <- identical(colnames(gq_ref), colnames(gq_new))
    gq_max_rel <- if (gq_same_cols) max(abs(gq_new - gq_ref) / pmax(1, abs(gq_ref))) else Inf
    cat(sprintf(
      "generated quantities at %d %s points: %d columns on each side%s, max relative difference %.2e\n",
      length(gq_idx), main_kind, ncol(gq_ref), if (gq_same_cols) "" else " (the two sides' columns differ)", gq_max_rel
    ))
  } else {
    cat(sprintf("generated quantities not compared: %d kept %s points, fewer than %d\n", length(gq_idx), main_kind, MIN_KEPT))
  }

  kept_n <- vapply(c("posterior", "off-posterior", "prior"), \(k) sum(kept_kind == k), 0L)
  reasons <- check_reasons(
    a$form, spread(const), tol, controls, main_kind, gq_max_rel, path_err, one_sided,
    any(rejected & kind == "posterior"), kept_n[kinds]
  )
  pass <- !length(reasons)
  verdict <- if (pass) "PASS" else "FAIL"
  write_equivalence_row(a$out, list(
    E = format(a$E), F = format(a$F), order = cfg$order, verdict = verdict, form = a$form, config = a$config,
    stan_sha256 = stan_sha256, n_posterior = kept_n[["posterior"]],
    n_off = kept_n[["off-posterior"]], n_prior = kept_n[["prior"]], n_rejected = sum(rejected), n_outside = sum(outside),
    n_one_sided = one_sided, const_range = signif(spread(const), 4), tol = signif(tol, 4), path_err = signif(path_err, 4),
    gq_points = if (gq_ran) length(gq_idx) else 0L, gq_max_rel = signif(gq_max_rel, 4),
    control_ref_walk = control_verdict[["ref_walk"]], control_new_anchor = control_verdict[["new_anchor"]],
    control_new_bridge = control_verdict[["new_bridge"]], control_new_forward = control_verdict[["new_forward"]],
    reasons = paste(reasons, collapse = "; ")
  ))
  cat(sprintf(
    "EQUIV %s: constant range %.2e over all %d kept points (tolerance %.1e); max path error %.2e; generated quantities max relative difference %.2e%s\n",
    verdict, spread(const), length(kept), tol, path_err, gq_max_rel, if (pass) "" else paste0("; ", paste(reasons, collapse = "; "))
  ))
  if (!pass) quit(save = "no", status = 1)
  invisible(TRUE)
}

if (is.null(box::name())) main(commandArgs(trailingOnly = TRUE))
