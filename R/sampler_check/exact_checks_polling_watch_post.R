# Exact check of Stan/polling_watch_v5_post.stan, the trial with a faster walk step leaving
# each election result, against the live Stan/polling_watch_v5_epoch.stan. It needs no fit:
# both log densities are evaluated with cmdstanr's model methods at random points drawn in
# the new model's unconstrained space.
#
#   The identity. With tau = tau_post, the new model's log density at (theta, tau), without
#   the Jacobian, minus exponential_lpdf(tau | 1) (that is, plus tau), equals the live
#   model's log density at theta on the same data with the day gap of each step leaving an
#   election result multiplied by (1 + tau)^2. The step leaving the result on grid date a is
#   the gap from a to the next grid date, time_diff[a]; the script takes it from the dates.
#   Both files declare time_diff as a real vector, so tau is drawn freely rather than kept to
#   values that leave the scaled gaps whole days. write_stan_json() passes the scaled gaps at
#   15 significant digits, which moves the live log density by far less than the tolerance.
#   The gradient with respect to theta must agree as well (relative to its largest entry).
#   The mutant. A copy of the new file, written to a temporary directory, with the multiplier
#   on the step INTO each result (leave[anchor[j]]) must fail the identity at every point.
#   The mistake it catches is the index shift between the v2 forecast, which flags a step by
#   its start date, and v5, whose v[t] is the step into date t.
#
# Three windows from --start, the data built as R/sampler_check/fit_polling_watch_headless.R
# builds them with its default tau options: all the data, and the polls before 2021-09-25
# and before 2024-11-30 (the backtest cutoffs). None of them is a forecast of 2017.
#
# Usage, from the project root (reads data/ through read_polling_watch_data()):
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/exact_checks_polling_watch_post.R \
#     [--stan=Stan/polling_watch_v5_post.stan] [--live=Stan/polling_watch_v5_epoch.stan] \
#     [--start=2016-01-01] [--n=20]
# Prints one block per window and EXACT PASS or EXACT FAIL; exits with status 1 on FAIL.
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(width = 160, warn = 1)
suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
stan_new <- arg("stan", "Stan/polling_watch_v5_post.stan")
stan_live <- arg("live", "Stan/polling_watch_v5_epoch.stan")
start <- as.Date(arg("start", "2016-01-01"))
n_pts <- as.integer(arg("n", "20"))
stopifnot("--n must be at least 20" = n_pts >= 20)
TOL <- 1e-8
MAP_TOL <- 1e-9
CUTOFFS <- c(all = NA_character_, cut2021 = "2021-09-25", cut2024 = "2024-11-30")
work <- tempfile("exact_checks_post_")
dir.create(work)

build_window <- function(cutoff) {
  box::use(
    R / data[read_polling_watch_data],
    R / stan_data[prepare_polling_watch_data, prepare_polling_watch_epochs]
  )
  polling_data <- read_polling_watch_data(start)
  if (!is.na(cutoff)) polling_data <- dplyr::filter(polling_data, date < as.Date(cutoff))
  prepare_polling_watch_epochs(
    prepare_polling_watch_data(polling_data),
    estimate_tau = TRUE, tau_prior_scale = 0.1, tau_mu_fixed = 0, tau_house_fixed = 0
  )
}

# The gap leaving each election result, read from the dates rather than from the Stan
# file's indexing: time_diff[i] runs from grid date i to i + 1, so the gap that starts on
# the result's grid date a is time_diff[a] (none when the result is the last date)
leaving_gaps <- function(prepared) {
  d <- prepared$stan_data
  dates <- prepared$date_mapping$date
  stopifnot("time_diff does not match the dates" = all(as.numeric(diff(dates)) == d$time_diff))
  anchors <- sort(unique(d$date_n[d$house_n == 1]))
  leaving <- anchors[anchors < d$D]
  list(idx = leaving, days = as.numeric(dates[leaving + 1] - dates[leaving]), results = dates[anchors])
}

src_new <- readLines(stan_new)
leave_line <- "if (anchor[j] < D) leave[anchor[j] + 1] = 1;"
hit <- which(trimws(src_new) == leave_line)
stopifnot("the leaving-step line is not in the new file exactly once" = length(hit) == 1)
src_mut <- src_new
src_mut[hit] <- sub(leave_line, "leave[anchor[j]] = 1;", src_mut[hit], fixed = TRUE)

compile_methods <- function(src, name) {
  # WHY: cmdstanr attaches model methods only when it compiles; a private copy in a temp
  # directory leaves the binaries beside the .stan files (and any fit using them) alone
  f <- file.path(work, paste0(name, ".stan"))
  writeLines(src, f)
  cmdstan_model(f, compile_model_methods = TRUE, force_recompile = TRUE, quiet = TRUE)
}
mods <- list(
  new = compile_methods(src_new, "v5_post"),
  live = compile_methods(readLines(stan_live), "v5_live"),
  mut = compile_methods(src_mut, "v5_post_mut_into")
)
methods_fit <- function(mod, data_file) {
  fit <- mod$sample(
    data = data_file, chains = 1, iter_warmup = 0, iter_sampling = 1, fixed_param = TRUE,
    init = 0, seed = 1, output_dir = work, refresh = 0, show_messages = FALSE
  )
  fit$init_model_methods()
  fit
}
write_data <- function(d, name) {
  f <- file.path(work, paste0(name, ".json"))
  write_stan_json(d, f)
  f
}

# WHY: constrain_variables() returns the 3-D z_house as a flat vector in Stan's column-major
# order, the order array() fills, and unconstrain_variables() needs it shaped
pars_of <- function(fit, u, d) {
  x <- fit$constrain_variables(u, transformed_parameters = FALSE, generated_quantities = FALSE)
  zh <- x[["z_house"]]
  if (length(zh) && is.null(dim(zh))) {
    x[["z_house"]] <- array(zh, c(d$E - 1, d$H - 1, d$P))
    stopifnot("z_house reshaped wrongly" = max(abs(apply(x[["z_house"]], c(1, 2), sum))) < 1e-10)
  }
  x
}

# A realistic base (elections at their results, sigma 0.01 per sqrt(day), sigma_gamma 0.1,
# tau_mu 0.1, tau_house 0.05, tau_post 1) plus N(0, 0.3^2) on every unconstrained coordinate
# but tau_post's, which is N(0, 1) so that tau ranges over about 0.1 to 7
random_points <- function(fit, d, n, seed) {
  p0 <- pars_of(fit, fit$unconstrain_draws(draws = fit$draws(), format = "draws_matrix")[1, ], d)
  p0$log_sigma[] <- log(0.01)
  p0$sigma_gamma <- 0.1
  p0$sigma_phi <- 0.5
  if (length(p0$tau_mu_est)) p0$tau_mu_est[] <- 0.1
  if (length(p0$tau_house_est)) p0$tau_house_est[] <- 0.05
  p0$tau_post <- 1
  u0 <- fit$unconstrain_variables(p0)
  K <- length(u0)
  probe <- fit$unconstrain_variables(modifyList(p0, list(tau_post = exp(1))))
  stopifnot(
    "tau_post is not the last unconstrained coordinate" =
      identical(which(probe != u0), K) && abs(probe[K] - 1) < 1e-12
  )
  set.seed(seed)
  lapply(seq_len(n), \(i) {
    u <- u0 + rnorm(K, 0, 0.3)
    u[K] <- rnorm(1)
    u
  })
}

check_window <- function(name, cutoff) {
  prepared <- build_window(cutoff)
  d <- prepared$stan_data
  lg <- leaving_gaps(prepared)
  data_file <- write_data(d, name)
  f_new <- methods_fit(mods$new, data_file)
  f_mut <- methods_fit(mods$mut, data_file)
  pts <- random_points(f_new, d, n_pts, seed = match(name, names(CUTOFFS)))
  K <- length(pts[[1]])
  rows <- lapply(seq_along(pts), \(i) {
    u <- pts[[i]]
    p <- pars_of(f_new, u, d)
    tau <- p$tau_post
    d_live <- d
    d_live$time_diff[lg$idx] <- d$time_diff[lg$idx] * (1 + tau)^2
    f_live <- methods_fit(mods$live, write_data(d_live, sprintf("%s_live_%02d", name, i)))
    # the live model's coordinates are the new model's without tau_post, the last one
    map_err <- max(abs(f_live$unconstrain_variables(p[setdiff(names(p), "tau_post")]) - u[-K]))
    g_new <- f_new$grad_log_prob(u, jacobian = FALSE)
    g_mut <- f_mut$grad_log_prob(u, jacobian = FALSE)
    g_live <- f_live$grad_log_prob(u[-K], jacobian = FALSE)
    lp_live <- attr(g_live, "log_prob")
    scale <- max(1, abs(g_live))
    data.frame(
      window = name, point = i, tau = tau, map_err = map_err, lp_live = lp_live,
      diff = attr(g_new, "log_prob") + tau - lp_live,
      diff_mut = attr(g_mut, "log_prob") + tau - lp_live,
      grad = max(abs(g_new[-K] - g_live)) / scale,
      grad_mut = max(abs(g_mut[-K] - g_live)) / scale
    )
  })
  res <- do.call(rbind, rows)
  cat(sprintf(
    "%s (cutoff %s): D=%d, N=%d, E=%d, %d unconstrained coordinates; results %s; leaving steps %s days; tau %.3f to %.3f; |log density| up to %.3g\n",
    name, if (is.na(cutoff)) "none" else cutoff, d$D, d$N, d$E, K, paste(lg$results, collapse = " "),
    paste(lg$days, collapse = " "), min(res$tau), max(res$tau), max(abs(res$lp_live))
  ))
  cat(sprintf(
    "  identity: max |difference| %.3g over %d points (tolerance %g); gradient max relative difference %.3g; coordinate map max error %.3g\n",
    max(abs(res$diff)), nrow(res), TOL, max(res$grad), max(res$map_err)
  ))
  cat(sprintf(
    "  mutant (multiplier on the step into each result): |difference| from %.3g to %.3g; gradient max relative difference %.3g\n",
    min(abs(res$diff_mut)), max(abs(res$diff_mut)), max(res$grad_mut)
  ))
  res
}

res <- do.call(rbind, lapply(names(CUTOFFS), \(w) check_window(w, CUTOFFS[[w]])))
ok <- c(
  identity = all(abs(res$diff) <= TOL),
  gradient = all(res$grad <= TOL),
  coordinate_map = all(res$map_err <= MAP_TOL),
  mutant_fails_everywhere = all(abs(res$diff_mut) > TOL)
)
print(ok)
pass <- all(ok)
cat(sprintf("EXACT %s: %d points in %d windows\n", if (pass) "PASS" else "FAIL", nrow(res), length(CUTOFFS)))
if (!pass) quit(status = 1)
