# Test fixtures for arm V2: its step configs, the 2024-eve cell and its Stan data, helpers
# that each plant one fault in one table of an info_set(), and model methods on V2's Stan
# files (v2_methods(), n_unc(), reverse_labels(), and stage 1's R reference).
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

# The compiled models of v2_methods(), one per Stan file's sha256 in this R session.
methods_models <- new.env(parent = emptyenv())

# A one-iteration fit that carries the model methods (log_prob, unconstrain_variables, ...)
# of `stan_file` on `data`. Ruling P1: the model is compiled with its methods from a copy of
# the file in a fresh temporary directory, never in place and never with force_recompile,
# once per R session per file sha256. `data$draw_forecast` is set to 0 if the field exists.
# The methods are attached by a fixed_param run in a temporary directory, deleted unread.
# A relative `stan_file` is read from the project root.
v2_methods <- function(stan_file, data) {
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
  if (!is.null(data$draw_forecast)) data$draw_forecast <- 0L
  attributes(data) <- list(names = names(data))
  run <- tempfile("v2-methods-run-")
  dir.create(run)
  on.exit(unlink(run, recursive = TRUE), add = TRUE)
  json <- file.path(run, "data.json")
  cmdstanr::write_stan_json(data, json)
  fit <- methods_models[[sha]]$sample(
    data = json, fixed_param = TRUE, chains = 1, iter_warmup = 0, iter_sampling = 1, adapt_engaged = FALSE,
    seed = 1, init = 0, output_dir = run, refresh = 0, show_messages = FALSE, show_exceptions = FALSE
  )
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
