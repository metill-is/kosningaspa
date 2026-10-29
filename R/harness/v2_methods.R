# The model-method machinery for Stan/forecast_v2.stan and the other V2 Stan files, shared by
# the tests (tests/harness/helper-v2.R imports it under the same names) and by
# R/harness/v2_equivalence.R, so that the script and the tests run one copy of the heap-safe
# code. cmdstanr's model methods are built with g++-16 against a
# clang-built R, so a C++ exception in any of them (a reject, a bound or a sum-to-zero vector
# violated) corrupts R's heap even when caught. Everything here therefore runs the data and
# every point through CmdStan's executable alone first, in its own process, and catches no
# model-method error: a model compiled with its methods is a copy in a temporary directory,
# memoised once per R session per Stan file sha256; every sampling fit and fixed_param run
# uses the executable alone; a point read back from a CSV is projected onto its sum-to-zero
# groups before it reaches unconstrain_variables().

box::use(
  R / harness / constants[project_root]
)

# The compiled models of v2_methods(), one per Stan file's sha256 in this R session.
methods_models <- new.env(parent = emptyenv())

#' The model of `stan_file` compiled with its methods: compiled from a copy of the file in a
#' fresh temporary directory, never in place and never with force_recompile (which overwrites
#' an executable that running chains may be using), once per R session per file sha256. A
#' relative `stan_file` is read from the project root.
#' @export
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

#' The v2_model() whose executable ran the fit `m`.
#' @export
model_of <- function(m) {
  exe <- m$runset$exe_file()
  for (sha in ls(methods_models)) if (identical(methods_models[[sha]]$exe_file(), exe)) {
    return(methods_models[[sha]])
  }
  stop("no v2_model() in this session ran this fit")
}

#' A fixed_param run of the executable of `stan_file` (v2_model()) on `data`: one chain of
#' `iter` iterations from `init` (0, or a parameter list such as a v2_simulate() draw), seed
#' 1, output in `dir`. The executable runs alone, without the model methods, so nothing of
#' the model runs in this R process. A run that fails stops with CmdStan's exception text.
#' @export
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

#' A one-iteration fit that carries the model methods (log_prob, unconstrain_variables, ...)
#' of `stan_file` (v2_model()) on `data`. `data$draw_forecast` is set to 0 if the field
#' exists. The methods are attached by a fixed_param run in a temporary directory, deleted
#' unread. The data are first run by the executable alone (fixed_param_run()), so a reject
#' stops with CmdStan's text before anything of the model runs in this R process.
#' @export
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

# WHY: cmdstanr keeps it only in the fit's private methods environment; reading it calls no model method.
#' The length of the unconstrained parameter vector of a v2_methods() fit.
#' @export
n_unc <- function(m) m$.__enclos_env__$private$model_methods_env_$num_upars_

#' One draw `x` (a named numeric vector of CmdStan columns) as a list of the parameters of
#' `dims`, each in cmdstanr's shape; `scalar` names the parameters declared as scalars, and
#' `keep_dim` those whose one-dimensional value keeps its dim when it has one element (the
#' `array[1] real` scales of the epoch bias, a `vector[1]`).
#' @export
draw_pars <- function(x, dims, scalar, keep_dim = character(0)) {
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
      # WHY: write_stan_json() writes a plain length-1 numeric as a scalar, and CmdStan's
      # init reader needs [x] for a parameter declared with one element.
      out[[n]] <- if (length(d) == 1 && !(d == 1 && n %in% keep_dim)) v else array(v, d)
    }
  }
  out
}

#' A point read back from a CmdStan CSV, made safe for unconstrain_variables(), which throws a
#' C++ exception (corrupting R's heap, since it is built with g++-16) on a sum-to-zero vector
#' off by the CSV's rounding or a bound violated: every sum-to-zero group (theta1, mu1, each
#' nu[j]) projected to zero sum, and every lower-bounded parameter checked strictly positive.
#' @export
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

#' The lower-bounded scale parameters that an off-posterior point multiplies by exp(N(0, 0.5)).
#' The epoch bias's scales come after the core's, then the newcomer pool's s_new, and the
#' shock's s_bar_E last: each is size 0 with its switch at 0 and draws nothing, so the other
#' scales' draws are as they were before it existed.
#' @export
OFF_SCALES <- c(
  "sigma", "tau_camp", "sigma_gamma", "sigma_phi", "tau_sd", "tau_mu", "tau_mu_A", "tau_house", "s_new", "s_bar_E"
)

#' The draws in `fit` (of `model`) of the variables named in `dims`, each in cmdstanr's
#' shape with its dims there; a list of one named list per draw. A parameter of
#' `V2_CORE_PARS` with one element keeps its dim (`draw_pars()`), so the point can be
#' written as a CmdStan init; a generated quantity's shape is as before.
#' @export
fit_values <- function(fit, model, dims) {
  box::use(R / harness / v2_reference[V2_CORE_PARS])
  info <- do.call(c, unname(model$variables()[c("parameters", "transformed_parameters", "generated_quantities")]))
  scalar <- vapply(names(dims), \(v) info[[v]]$dimensions == 0, logical(1))
  present <- names(dims)[vapply(dims, prod, numeric(1)) > 0]
  x <- unclass(posterior::as_draws_matrix(fit$draws(variables = present)))
  lapply(seq_len(nrow(x)), \(i) draw_pars(x[i, ], dims, scalar, keep_dim = V2_CORE_PARS$name))
}

#' Data written as CmdStan JSON in `dir`; returns the path.
#' @export
data_json <- function(data, dir) {
  json <- file.path(dir, "data.json")
  attributes(data) <- list(names = names(data))
  cmdstanr::write_stan_json(data, json)
  json
}

#' The parameter draws of a 4 x (`iter_warmup` + `iter_sampling`) fit of `m`'s model on
#' `data` at `max_treedepth`, init 0 and `seed`, read at sig_figs = 18 and projected
#' (project_point()): a list of 4 `iter_sampling` parameter lists (80 at the defaults, the
#' lp tests' 50 + 20 at depth 8). The fit draws no forecast (draw_forecast = 0, as in
#' v2_methods()): the points need only the parameters. It runs the executable of `m`'s model
#' alone, without the model methods. Its output is in a temporary directory, deleted on
#' return; only the parameters are read. Every transition reaching the depth cap is
#' expected in so short a fit, so the sampler's diagnostic warnings are off.
#' @export
short_fit_draws <- function(m, data, seed, iter_warmup = 50, iter_sampling = 20, max_treedepth = 8) {
  box::use(R / harness / v2_reference[v2_par_dims])
  model <- model_of(m)
  if (!is.null(data$draw_forecast)) data$draw_forecast <- 0L
  run <- withr::local_tempdir("v2-points-")
  # WHY: a fit of the methods-compiled model builds the model on its data in this R process (heap corruption under g++-16 on any C++ exception); the executable alone does not.
  plain <- cmdstanr::cmdstan_model(exe_file = model$exe_file())
  fit <- plain$sample(
    data = data_json(data, run), chains = 4, parallel_chains = 4, iter_warmup = iter_warmup,
    iter_sampling = iter_sampling, max_treedepth = max_treedepth, seed = seed, init = 0, sig_figs = 18,
    output_dir = run, refresh = 0, show_messages = FALSE, show_exceptions = FALSE, diagnostics = NULL
  )
  stopifnot("the short fit failed" = all(fit$return_codes() == 0))
  lapply(fit_values(fit, model, v2_par_dims(data)), project_point)
}

#' The parameters and generated quantities of `stan_file` (v2_model()) on `data` at `pars` (a
#' v2_simulate() draw): a one-draw fixed_param run started at `pars`, read back at sig_figs =
#' 18, so `pars` is the point as the run holds it. Returns list(pars, gq), `gq` holding every
#' generated quantity of non-zero size in its shape.
#' @export
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

#' The draws of the generated quantity `variable` of `stan_file` on `data` in an
#' `iter`-iteration fixed_param run (fixed_param_run()) at `pars` (a v2_simulate() draw), as
#' an array [iter, dims of `variable`]: the draws are independent given the point. Read at
#' CmdStan's default 6 significant figures, which is exact for a count below 1e6.
#' @export
gq_draws <- function(stan_file, data, pars, variable, iter) {
  run <- withr::local_tempdir("v2-gq-draws-")
  fit <- fixed_param_run(stan_file, data, run, init = pars, iter = iter)
  posterior::draws_of(posterior::as_draws_rvars(fit$draws(variables = variable))[[variable]])
}
