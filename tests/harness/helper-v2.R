# Test fixtures for arm V2: its step configs, the 2024-eve cell and its Stan data, helpers
# that each plant one fault in one table of an info_set(), and model methods on V2's Stan
# files (v2_model(), v2_methods(), n_unc(), reverse_labels(), and stage 1's R reference),
# random_sigma() for the walk's covariance algebra, and the comparison of
# Stan/forecast_v2.stan with the R reference (skip_unless_step(), v2_test_cells(),
# lp_points(), stan_pars(), expect_lp_matches(), expect_lp_rejects(), mutant_exercised(),
# bounded_at()).
# The V2 modules are imported inside the fixtures, so that a broken module fails the tests
# that use it, not the sourcing of every test file.
box::use(
  R / harness / constants[project_root],
  R / harness / info_set[info_set, read_inputs]
)

V2_M0_CONFIG <- file.path(project_root(), "harness", "configs", "steps", "v2s-m0.json")

# WHY: read_inputs() and v2_stan_data() take seconds, and the 2024-eve fixtures serve many tests.
v2_cache <- new.env(parent = emptyenv())
cached <- function(key, value) {
  if (is.null(v2_cache[[key]])) v2_cache[[key]] <- value
  v2_cache[[key]]
}

cfg_m0 <- function() {
  box::use(R / harness / v2[read_v2_config])
  read_v2_config(V2_M0_CONFIG)
}

# A temporary copy of the m0 config with the named keys changed (NULL drops a key, a new
# name adds one), deleted when the caller's frame exits; returns its path.
config_with <- function(..., env = parent.frame()) {
  cfg <- utils::modifyList(jsonlite::read_json(V2_M0_CONFIG), list(...))
  path <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, pretty = TRUE, digits = I(17))
  path
}

v2_inputs <- function() {
  skip_without_snapshot()
  cached("inputs", read_inputs())
}

info_2024eve <- function() {
  skip_without_snapshot()
  cached("info_2024eve", info_set(as.Date("2024-11-30"), as.Date("2024-11-29"), v2_inputs()))
}

d_2024eve <- function() {
  box::use(R / harness / v2[v2_stan_data])
  skip_without_snapshot()
  cached("d_2024eve", v2_stan_data(info_2024eve(), cfg_m0())$data)
}

# The cell's party order with Sósíalistaflokkurinn first: it has no votes at 2016 or 2017.
j_first <- function(info) {
  j <- "S\u00f3s\u00edalistaflokkurinn"
  c(j, setdiff(info$parties, j))
}

# Samfylkingin gets 0 votes in one kjördæmi of the first closed election, where it has votes.
zero_votes_in_one_kd <- function(info) {
  r <- info$results
  i <- which(r$election == min(r$election) & r$flokkur == "Samfylkingin")[1]
  stopifnot(r$votes[i] > 0)
  r$votes[i] <- 0L
  info$results <- r
  info
}

duplicate_kd_row <- function(info) {
  info$polls_kd <- rbind(info$polls_kd, info$polls_kd[1, ])
  info
}

no_national_polls <- function(info) {
  info$polls_nat <- info$polls_nat[0, ]
  info
}

# The last national poll date moves to F + 1.
poll_after_F <- function(info) {
  last <- which(info$polls_nat$date == max(info$polls_nat$date))
  info$polls_nat$date[last] <- info$F + 1
  info
}

# A random walk-innovation covariance Sigma = diag(sigma) Omega diag(sigma), with
# sigma ~ U(0.01, 0.05) and Omega an LKJ(2) draw, from the global RNG.
random_sigma <- function(P) {
  box::use(R / harness / v2_reference[lkj_corr_cholesky_rng])
  s <- stats::runif(P, 0.01, 0.05)
  L <- lkj_corr_cholesky_rng(P, 2)
  diag(s) %*% tcrossprod(L) %*% diag(s)
}

# The compiled models of v2_methods(), one per Stan file's sha256 in this R session.
methods_models <- new.env(parent = emptyenv())

# The model of `stan_file` compiled with its methods: compiled from a copy of the file in a
# fresh temporary directory, never in place and never with force_recompile (which overwrites
# an executable that running chains may be using), once per R session per file sha256. A
# relative `stan_file` is read from the project root.
v2_model <- function(stan_file) {
  if (!startsWith(stan_file, "/")) stan_file <- file.path(project_root(), stan_file)
  sha <- digest::digest(file = stan_file, algo = "sha256")
  if (is.null(methods_models[[sha]])) {
    # WHY: cmdstanr builds the methods only when it compiles, never from an executable it finds up to date.
    dir <- tempfile("v2-methods-")
    dir.create(dir)
    copy <- file.path(dir, basename(stan_file))
    stopifnot(file.copy(stan_file, copy))
    methods_models[[sha]] <- cmdstanr::cmdstan_model(copy, compile_model_methods = TRUE, quiet = TRUE)
  }
  methods_models[[sha]]
}

# The v2_model() whose executable ran the fit `m`.
model_of <- function(m) {
  exe <- m$runset$exe_file()
  for (sha in ls(methods_models)) if (identical(methods_models[[sha]]$exe_file(), exe)) {
    return(methods_models[[sha]])
  }
  stop("no v2_model() in this session ran this fit")
}

# A fixed_param run of the executable of `stan_file` (v2_model()) on `data`: one chain of
# `iter` iterations from `init` (0, or a parameter list such as a v2_simulate() draw), seed
# 1, output in `dir`. The executable runs alone, without the model methods, so nothing of
# the model runs in this R process. A run that fails stops with CmdStan's exception text.
fixed_param_run <- function(stan_file, data, dir, init = 0, iter = 1, sig_figs = NULL) {
  plain <- cmdstanr::cmdstan_model(exe_file = v2_model(stan_file)$exe_file())
  if (is.list(init)) {
    # WHY: CmdStan's init reader refuses a zero-size parameter (nu with J = 0) written as [], and
    # there is nothing to initialise in it; cmdstanr would note that it was left out.
    withr::local_options(cmdstanr_warn_inits = FALSE)
    init <- list(init[lengths(init) > 0])
  }
  fit <- withCallingHandlers(
    plain$sample(
      data = data_json(data, dir), fixed_param = TRUE, chains = 1, iter_warmup = 0, iter_sampling = iter,
      adapt_engaged = FALSE, seed = 1, init = init, sig_figs = sig_figs, output_dir = dir, refresh = 0,
      show_messages = FALSE, show_exceptions = FALSE
    ),
    # WHY: cmdstanr's warning names no reason; the stop below gives CmdStan's.
    warning = function(w) {
      if (grepl("No chains finished successfully", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
    }
  )
  if (!all(fit$return_codes() == 0)) {
    out <- fit$runset$procs$proc_output(1)
    said <- grep("Exception|Error", out, value = TRUE)
    if (!length(said)) said <- utils::tail(out, 10)
    stop("the fixed_param run failed; CmdStan said:\n", paste(said, collapse = "\n"), call. = FALSE)
  }
  fit
}

# A one-iteration fit that carries the model methods (log_prob, unconstrain_variables, ...)
# of `stan_file` (v2_model()) on `data`. `data$draw_forecast` is set to 0 if the field
# exists. The methods are attached by a fixed_param run in a temporary directory, deleted
# unread. The data are first run by the executable alone (fixed_param_run()), so a reject
# stops with CmdStan's text before anything of the model runs in this R process.
v2_methods <- function(stan_file, data) {
  model <- v2_model(stan_file)
  if (!is.null(data$draw_forecast)) data$draw_forecast <- 0L
  run <- tempfile("v2-methods-run-")
  dir.create(run)
  on.exit(unlink(run, recursive = TRUE), add = TRUE)
  # WHY: a fit of a model compiled with its methods builds the model on its data in this R
  # process as soon as cmdstanr creates the fit, before any return code can be read. The
  # methods are built with g++-16 against a clang-built R, so a C++ exception there (a reject
  # in transformed data) corrupts R's heap even when caught; nothing here catches it. The
  # executable alone meets any reject first, in CmdStan's own process.
  check <- file.path(run, "check")
  dir.create(check)
  fixed_param_run(stan_file, data, check)
  fit <- model$sample(
    data = data_json(data, run), fixed_param = TRUE, chains = 1, iter_warmup = 0, iter_sampling = 1, adapt_engaged = FALSE,
    seed = 1, init = 0, output_dir = run, refresh = 0, show_messages = FALSE, show_exceptions = FALSE
  )
  stopifnot("the fixed_param run with the model methods failed" = all(fit$return_codes() == 0))
  # WHY: the model pointer reads the data file, which on.exit deletes.
  fit$init_model_methods()
  fit
}

# The length of the unconstrained parameter vector of a v2_methods() fit.
# WHY: cmdstanr keeps it only in the fit's private methods environment; reading it calls no model method.
n_unc <- function(m) m$.__enclos_env__$private$model_methods_env_$num_upars_

# Stage-1 data (stage1_data()) with the party labels in reverse order: label l becomes
# L + 1 - l, the attribute `labels` is reversed, and the rows of each election and the
# target rows are listed in reverse order too.
reverse_labels <- function(d) {
  first <- cumsum(c(0L, c(d$n_rows)[-d$D]))
  i <- unlist(lapply(seq_len(d$D), \(e) rev(first[e] + seq_len(d$n_rows[e]))))
  j <- rev(seq_len(d$N_E))
  out <- d
  for (f in c("y", "x", "inc_years", "vnv", "growth")) out[[f]] <- array(c(d[[f]])[i], dim = d$N)
  out$label <- array(d$L + 1L - c(d$label)[i], dim = d$N)
  for (f in c("x_E", "inc_years_E", "vnv_E", "growth_E")) out[[f]] <- array(c(d[[f]])[j], dim = d$N_E)
  out$label_E <- array(d$L + 1L - c(d$label_E)[j], dim = d$N_E)
  attr(out, "labels") <- rev(attr(d, "labels"))
  out
}

# The predictor of stage 1 (Stan/fundamentals_v2.stan) at parameters `p` for covariates
# `x`, `inc_years`, `vnv`, `growth` (suffix "" for the training rows, "_E" for E).
stage1_predictor <- function(d, p, suffix = "") {
  f <- function(name) c(d[[paste0(name, suffix)]])
  label <- if (suffix == "") c(d$label) else c(d$label_E)
  c(p$alpha)[label] + p$beta_lag * f("x") + p$beta_inc * f("inc_years") + p$beta_vnv * f("vnv") +
    p$beta_growth * f("growth")
}

# Stage 1's log density at constrained parameters `p` (a constrain_variables() list), with
# every normalising constant: the Dirichlet-multinomial log-pmf of each election's counts
# given phi_f * softmax(eta_d), plus the §6.10 priors, without the Jacobian.
stage1_lp_ref <- function(d, p) {
  eta <- stage1_predictor(d, p)
  phi <- 1 / p$phi_f_inv
  last <- cumsum(c(d$n_rows))
  lp <- 0
  for (e in seq_len(d$D)) {
    i <- (last[e] - d$n_rows[e] + 1):last[e]
    a <- phi * exp(eta[i] - max(eta[i])) / sum(exp(eta[i] - max(eta[i])))
    y <- c(d$y)[i]
    n <- sum(y)
    lp <- lp + lgamma(n + 1) - sum(lgamma(y + 1)) + lgamma(sum(a)) - lgamma(n + sum(a)) + sum(lgamma(y + a) - lgamma(a))
  }
  lp + sum(stats::dnorm(c(p$alpha), 0, 1, log = TRUE)) + stats::dnorm(p$beta_lag, 0, 1, log = TRUE) +
    stats::dnorm(p$beta_inc, 0, 1, log = TRUE) + stats::dnorm(p$beta_vnv, 0, 5, log = TRUE) +
    stats::dnorm(p$beta_growth, 0, 5, log = TRUE) + stats::dexp(p$phi_f_inv, 1, log = TRUE)
}

# Stage 1's eta_E at constrained parameters `p`: the predictor at E, centred over S_f(E).
stage1_eta_E_ref <- function(d, p) {
  e <- stage1_predictor(d, p, "_E")
  e - mean(e)
}
# Skip unless the env var V2_STEPS is unset or empty, or its comma-separated list names
# `step`, so that one step's tests of test-v2-stan.R run on their own.
skip_unless_step <- function(step) {
  steps <- trimws(strsplit(Sys.getenv("V2_STEPS"), ",", fixed = TRUE)[[1]])
  steps <- steps[nzchar(steps)]
  if (length(steps) && !step %in% steps) testthat::skip(paste0(step, " is not in V2_STEPS"))
}

# The three cells of the Stan lp tests, as list(data, n_post, n_off), built on first call in
# the R session for each config, never when the helper is sourced: `data` is
# v2_stan_data()'s for `cfg` with walk_form and delta_form at 0, under a temporary root with
# no freeze, so the 2017 cell is its pre-freeze form (draw_forecast = 0). The 2017 cell gets
# prior points only.
v2_test_cells <- function(cfg) {
  box::use(R / harness / v2[v2_stan_data])
  skip_without_snapshot()
  cfg$walk_form <- 0L
  cfg$delta_form <- 0L
  cached(paste("v2_test_cells", digest::digest(cfg)), {
    root <- withr::local_tempdir("v2-cells-root-")
    build <- function(E, F) v2_stan_data(info_set(as.Date(E), as.Date(F), v2_inputs()), cfg, root = root)$data
    list(
      eve2024 = list(data = build("2024-11-30", "2024-11-29"), n_post = 20L, n_off = 10L),
      f180_2021 = list(data = build("2021-09-25", "2021-03-29"), n_post = 20L, n_off = 10L),
      eve2017 = list(data = build("2017-10-28", "2017-10-27"), n_post = 0L, n_off = 0L)
    )
  })
}

# The core's parameters at unconstrained point `u` of the v2_methods() fit `m`, as
# constrain_variables() gives them: named and ordered as V2_CORE_PARS, a matrix as a
# matrix, and an array of more than two dimensions (`delta_raw`) flat in column-major
# order, since utils::relist() drops its dims.
stan_pars <- function(m, u) {
  box::use(R / harness / v2_reference[V2_CORE_PARS])
  pars <- m$constrain_variables(u, transformed_parameters = FALSE, generated_quantities = FALSE)
  stopifnot(identical(names(pars), V2_CORE_PARS$name))
  pars
}

# One draw `x` (a named numeric vector of CmdStan columns) as a list of the parameters of
# `dims`, each in cmdstanr's shape; `scalar` names the parameters declared as scalars.
draw_pars <- function(x, dims, scalar) {
  out <- list()
  for (n in names(dims)) {
    d <- dims[[n]]
    if (prod(d) == 0) {
      out[[n]] <- array(numeric(0), d)
    } else if (scalar[[n]]) {
      out[[n]] <- unname(x[[n]])
    } else {
      idx <- as.matrix(expand.grid(lapply(d, seq_len)))
      v <- unname(x[paste0(n, "[", apply(idx, 1, paste, collapse = ","), "]")])
      stopifnot(!anyNA(v))
      out[[n]] <- if (length(d) == 1) v else array(v, d)
    }
  }
  out
}

# A point read back from a CmdStan CSV, made safe for unconstrain_variables(), which throws a
# C++ exception (corrupting R's heap, since it is built with g++-16) on a sum-to-zero vector
# off by the CSV's rounding or a bound violated: every sum-to-zero group (theta1, mu1, each
# nu[j]) projected to zero sum, and every lower-bounded parameter checked strictly positive.
project_point <- function(p) {
  p$theta1 <- p$theta1 - mean(p$theta1)
  p$mu1 <- p$mu1 - mean(p$mu1)
  if (length(p$nu)) p$nu <- p$nu - rowMeans(p$nu)
  stopifnot(
    abs(sum(p$theta1)) < 1e-12, abs(sum(p$mu1)) < 1e-12, all(abs(rowSums(p$nu)) < 1e-12),
    all(unlist(p[OFF_SCALES]) > 0)
  )
  p
}

# The lower-bounded scale parameters that an off-posterior point multiplies by exp(N(0, 0.5)).
OFF_SCALES <- c("sigma", "tau_camp", "sigma_gamma", "sigma_phi", "tau_sd")

# The draws in `fit` (of `model`) of the variables named in `dims`, each in cmdstanr's
# shape with its dims there; a list of one named list per draw.
fit_values <- function(fit, model, dims) {
  info <- do.call(c, unname(model$variables()[c("parameters", "transformed_parameters", "generated_quantities")]))
  scalar <- vapply(names(dims), \(v) info[[v]]$dimensions == 0, logical(1))
  present <- names(dims)[vapply(dims, prod, numeric(1)) > 0]
  x <- unclass(posterior::as_draws_matrix(fit$draws(variables = present)))
  lapply(seq_len(nrow(x)), \(i) draw_pars(x[i, ], dims, scalar))
}

# Data written as CmdStan JSON in `dir`; returns the path.
data_json <- function(data, dir) {
  json <- file.path(dir, "data.json")
  attributes(data) <- list(names = names(data))
  cmdstanr::write_stan_json(data, json)
  json
}

# The parameter draws of a 4 x (50 + 20) fit of `m`'s model on `data` at max_treedepth 8,
# init 0 and `seed`, read at sig_figs = 18 and projected (project_point()): a list of 80
# parameter lists. The fit draws no forecast (draw_forecast = 0, as in v2_methods()): the
# points need only the parameters. It runs the executable of `m`'s model alone, without the
# model methods. Its output is in a temporary directory, deleted on
# return; only the parameters are read. Every transition reaching depth 8 is expected, so
# the sampler's diagnostic warnings are off.
short_fit_draws <- function(m, data, seed) {
  box::use(R / harness / v2_reference[v2_par_dims])
  model <- model_of(m)
  if (!is.null(data$draw_forecast)) data$draw_forecast <- 0L
  run <- withr::local_tempdir("v2-points-")
  # WHY: a fit of the methods-compiled model builds the model on its data in this R process (heap corruption under g++-16 on any C++ exception); the executable alone does not.
  plain <- cmdstanr::cmdstan_model(exe_file = model$exe_file())
  fit <- plain$sample(
    data = data_json(data, run), chains = 4, parallel_chains = 4, iter_warmup = 50, iter_sampling = 20,
    max_treedepth = 8, seed = seed, init = 0, sig_figs = 18, output_dir = run, refresh = 0, show_messages = FALSE,
    show_exceptions = FALSE, diagnostics = NULL
  )
  stopifnot("the short fit failed" = all(fit$return_codes() == 0))
  lapply(fit_values(fit, model, v2_par_dims(data)), project_point)
}

# The parameters and generated quantities of `stan_file` (v2_model()) on `data` at `pars` (a
# v2_simulate() draw): a one-draw fixed_param run started at `pars`, read back at sig_figs =
# 18, so `pars` is the point as the run holds it. Returns list(pars, gq), `gq` holding every
# generated quantity of non-zero size in its shape.
gq_at <- function(stan_file, data, pars) {
  box::use(R / harness / v2_reference[v2_par_dims])
  model <- v2_model(stan_file)
  run <- withr::local_tempdir("v2-gq-")
  fit <- fixed_param_run(stan_file, data, run, init = pars, sig_figs = 18)
  sizes <- fit$metadata()$stan_variable_sizes
  gq <- intersect(names(model$variables()$generated_quantities), names(sizes))
  gq <- gq[vapply(sizes[gq], prod, numeric(1)) > 0]
  list(pars = fit_values(fit, model, v2_par_dims(data))[[1]], gq = fit_values(fit, model, sizes[gq])[[1]])
}

# The draws of the generated quantity `variable` of `stan_file` on `data` in an
# `iter`-iteration fixed_param run (fixed_param_run()) at `pars` (a v2_simulate() draw), as
# an array [iter, dims of `variable`]: the draws are independent given the point. Read at
# CmdStan's default 6 significant figures, which is exact for a count below 1e6.
gq_draws <- function(stan_file, data, pars, variable, iter) {
  run <- withr::local_tempdir("v2-gq-draws-")
  fit <- fixed_param_run(stan_file, data, run, init = pars, iter = iter)
  posterior::draws_of(posterior::as_draws_rvars(fit$draws(variables = variable))[[variable]])
}

# The points of the Stan lp tests on `data`, for the v2_methods() fit `m`: a list of
# list(u, pars, kind), `u` unconstrained and `pars` constrain_variables()' parameters at `u`
# (stan_pars()). `n_post` posterior points and `n_off` off-posterior points are distinct
# draws of one short fit (short_fit_draws(), seed `seed`), the latter with each OFF_SCALES
# element multiplied by exp(N(0, 0.5)); `n_prior` prior points are v2_simulate() draws with
# seeds seed + 1, ..., seed + n_prior. With n_post = n_off = 0 nothing is fitted. Memoised in
# the R session by the model, the data and the counts, so a cell's fit runs once.
lp_points <- function(m, data, n_post = 20, n_off = 10, n_prior = 10, seed = 5100) {
  key <- paste("lp_points", digest::digest(list(m$runset$exe_file(), data)), n_post, n_off, n_prior, seed)
  cached(key, {
    box::use(R / harness / v2_reference[v2_simulate])
    point <- function(u, kind) list(u = u, pars = stan_pars(m, u), kind = kind)
    pts <- list()
    if (n_post + n_off > 0) {
      draws <- short_fit_draws(m, data, seed)
      withr::with_seed(seed, {
        pick <- sample(length(draws), n_post + n_off)
        for (i in seq_along(pick)) {
          p <- draws[[pick[i]]]
          off <- i > n_post
          if (off) for (n in OFF_SCALES) p[[n]] <- p[[n]] * exp(stats::rnorm(length(p[[n]]), 0, 0.5))
          pts[[length(pts) + 1]] <- point(m$unconstrain_variables(p), if (off) "off" else "post")
        }
      })
    }
    for (i in seq_len(n_prior)) {
      s <- v2_simulate(data, seed = seed + i, observe = FALSE)
      pts[[length(pts) + 1]] <- point(m$unconstrain_variables(s$pars), "prior")
    }
    pts
  })
}

# The range of `x`, or Inf if some element is not finite (a mutant whose density is -Inf
# at a point where Stan's is finite differs from it without bound).
lp_range <- function(x) if (all(is.finite(x))) diff(range(x)) else Inf

# The tolerance of the constant over points whose true reference log densities are `ref`.
lp_tol <- function(ref) 1e-9 * max(1, abs(ref))

# Stan's log density (log_prob, jacobian = FALSE) minus the reference's (`mutant`) at each
# point, the true reference's log density `ref`, each point's `kind`, and the tolerance
# lp_tol(ref) over all points.
lp_gap <- function(m, data, points, mutant = "none") {
  box::use(R / harness / v2_reference[v2_lp_ref])
  stan <- vapply(points, \(p) m$log_prob(p$u, jacobian = FALSE), numeric(1))
  ref <- vapply(points, \(p) v2_lp_ref(p$pars, data), numeric(1))
  mut <- if (mutant == "none") ref else vapply(points, \(p) v2_lp_ref(p$pars, data, mutant), numeric(1))
  list(gap = stan - mut, ref = ref, kind = vapply(points, \(p) p$kind, character(1)), tol = lp_tol(ref))
}

# Expect Stan's log density minus the reference's to be constant over `points`: within each
# kind of point (post, off, prior) its range below that kind's lp_tol(), and over all points
# its range below lp_tol() of all (one constant, not one per kind). Returns invisibly
# list(range, tol, kind), `kind` a list of c(range, tol) per kind of point.
expect_lp_matches <- function(m, data, points) {
  g <- lp_gap(m, data, points)
  kind <- list()
  for (k in unique(g$kind)) {
    i <- g$kind == k
    kind[[k]] <- c(range = lp_range(g$gap[i]), tol = lp_tol(g$ref[i]))
    expect_lt(kind[[k]][["range"]], kind[[k]][["tol"]], label = paste0("range of lp_stan - lp_ref over the ", k, " points"))
  }
  r <- lp_range(g$gap)
  expect_lt(r, g$tol, label = "range of lp_stan - lp_ref over all points")
  invisible(list(range = r, tol = g$tol, kind = kind))
}

# Expect the same range against the `mutant` reference to exceed 100 times the tolerance.
# Returns that range over 100 * tol invisibly.
expect_lp_rejects <- function(m, data, points, mutant) {
  g <- lp_gap(m, data, points, mutant)
  r <- lp_range(g$gap)
  expect_gt(r, 100 * g$tol, label = paste0("range of lp_stan - lp_ref(", mutant, ")"))
  invisible(r / (100 * g$tol))
}

# Whether `mutant`'s reference differs from the true reference by more than 1e-6 at some of
# `points`: a property of the reference and the data alone.
mutant_exercised <- function(data, points, mutant) {
  box::use(R / harness / v2_reference[v2_lp_ref])
  any(vapply(points, \(p) !isTRUE(abs(v2_lp_ref(p$pars, data, mutant) - v2_lp_ref(p$pars, data)) <= 1e-6), logical(1)))
}

# The transformed parameters of `m` at a prior draw (v2_simulate(), seed 1) with every
# bounded concentration's free parameter (log_phi_u, log_phi_K_u, log_kappa_u) set to `u`.
bounded_at <- function(m, data, u) {
  box::use(R / harness / v2_reference[v2_simulate])
  pars <- v2_simulate(data, seed = 1, observe = FALSE)$pars
  pars$log_phi_u <- rep(u, data$H)
  pars$log_phi_K_u <- u
  pars$log_kappa_u <- u
  m$constrain_variables(m$unconstrain_variables(pars), transformed_parameters = TRUE, generated_quantities = FALSE)
}
