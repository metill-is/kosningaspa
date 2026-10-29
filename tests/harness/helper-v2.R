# Test fixtures for arm V2: its step configs, the 2024-eve cell and its Stan data, helpers
# that each plant one fault in one table of an info_set(), stage 1's data and R reference
# (reverse_labels() and the stage1_* functions), random_sigma() for the walk's covariance
# algebra, and the comparison of Stan/forecast_v2.stan with the R reference
# (skip_unless_step(), v2_test_cells(), lp_points(), stan_pars(), expect_lp_matches(),
# expect_lp_rejects(), mutant_exercised(), bounded_at(), stan_block_decls()). The model
# methods on V2's Stan files (v2_model(), v2_methods(), n_unc(), fixed_param_run(),
# short_fit_draws(), gq_at(), ...) are R/harness/v2_methods.R's, imported below under their
# own names.
# The other V2 modules are imported inside the fixtures, so that a broken module fails the
# tests that use it, not the sourcing of every test file.
box::use(
  R / harness / constants[project_root],
  R / harness / info_set[info_set, read_inputs],
  # WHY: the methods machinery is a module so that R/harness/v2_equivalence.R and the tests run one copy of the heap-safe code, under the same names.
  R / harness / v2_methods[
    v2_model, model_of, fixed_param_run, v2_methods, n_unc, draw_pars, project_point, OFF_SCALES, fit_values,
    data_json, short_fit_draws, gq_at, gq_draws
  ]
)

V2_M0_CONFIG <- file.path(project_root(), "harness", "configs", "steps", "v2s-m0.json")
V2_M1_CONFIG <- file.path(project_root(), "harness", "configs", "steps", "v2s-m1.json")
V2_M2_CONFIG <- file.path(project_root(), "harness", "configs", "steps", "v2s-m2.json")

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

# Step v2s-m1's config: the core with the epoch bias (§6.4).
cfg_m1 <- function() {
  box::use(R / harness / v2[read_v2_config])
  read_v2_config(V2_M1_CONFIG)
}

# Step v2s-m2's config: v2s-m1's with the newcomer contrast's mean and scale estimated (§6.4).
cfg_m2 <- function() {
  box::use(R / harness / v2[read_v2_config])
  read_v2_config(V2_M2_CONFIG)
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

# The declarations of block `block` ("parameters", say) of the Stan program `lines`: a data
# frame of `type` and `name`, one row per non-empty line between the line `<block> {` and the
# next line `}`, in the file's order. It strips `//` comments only, and splits each line at
# its last name before a final `;`, so a line holding two declarations parses as one row with
# a garbled type rather than erroring; a garbled row still fails an exact comparison of names
# and types. An error if the line `<block> {` is not found exactly once, if no line `}`
# follows it, or if a line does not end in a name and `;` (a trailing `/* */` comment
# included); an empty block errors too, naming its closing `}`.
stan_block_decls <- function(lines, block) {
  start <- which(lines == paste(block, "{"))
  if (length(start) != 1) stop("stan_block_decls: block ", block, " found ", length(start), " times", call. = FALSE)
  end <- start + which(lines[-seq_len(start)] == "}")[1]
  if (is.na(end)) stop("stan_block_decls: block ", block, " has no closing brace", call. = FALSE)
  body <- trimws(sub("//.*$", "", lines[seq(start + 1, end - 1)]))
  body <- body[nzchar(body)]
  m <- regmatches(body, regexec("^(.*\\S)\\s+([A-Za-z_][A-Za-z0-9_]*);$", body))
  bad <- lengths(m) != 3
  if (any(bad)) stop("stan_block_decls: not one declaration: ", body[bad][1], call. = FALSE)
  data.frame(type = vapply(m, `[`, "", 2), name = vapply(m, `[`, "", 3))
}
