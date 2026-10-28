# Compares two fits of the production constituency model written by
# R/sampler_check/fit_headless.R (or any fit dir laid out the same way: CmdStan CSVs
# in <dir>/cmdstan/ and a timing.rds holding wall_min): sampler diagnostics, the treedepth__ distribution, max R-hat
# and min bulk/tail ESS, ESS per wall minute and per 1,000 sampling-phase
# gradients, and agreement of election-day vote-share quantiles within MCSE.
#
# pi_eday is computed here, not in Stan: softmax of (beta0 + mu_gamma_pred) with
# Annað's log-ratio as minus their sum, i.e. the shares y_rep_national draws its
# election-day counts from. Agreement z = (q_new - q_old) / sqrt(mcse_new^2 + mcse_old^2).
#
# Usage, from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/compare_fits.R --old=<fit dir> \
#     --new=<fit dir> --data-json=<file> --out=<dir> [--label=eve]
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(width = 160, warn = 1)
suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(dplyr)
  library(tidyr)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
label <- arg("label", "fit")
out <- arg("out")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dat <- jsonlite::read_json(arg("data-json"), simplifyVector = TRUE)
P <- dat$P
parties <- c("Annað", "D", "B", "S", "V", "P", "C", "F", "M", "J")
stopifnot(length(parties) == P)

KEY <- c(
  "beta", "beta0", "sigma", "Omega", "gamma", "phi", "delta_pred", "sigma_delta",
  "mu_log_sigma", "tau_log_sigma", "tau_stjornarslit", "mu_gamma", "sigma_gamma",
  "mu_phi", "sigma_phi", "alpha_f", "beta_lag_f", "beta_inc_years_f", "beta_vnv_f",
  "beta_growth_f", "phi_f", "mu_pred", "mu_gamma_pred"
)
SHARED_PARAMS <- c(
  "L_Omega", "z_beta0", "sigma_delta", "gamma_raw", "mu_gamma", "sigma_gamma",
  "mu_log_sigma", "tau_log_sigma", "tau_stjornarslit", "mu_phi", "sigma_phi",
  "alpha_f_raw", "beta_lag_f", "beta_inc_years_f", "beta_vnv_f", "beta_growth_f", "phi_f_inv"
)
# Each parameterisation's own parameters. WHY: the anchored model also writes delta_free,
# log_sigma and log_phi, but as transformed parameters, so a fit is anchored when it has
# anchor_w and takes the backward walk's names otherwise.
OWN_PARAMS <- list(
  anchored = c("anchor_w", "z_walk", "delta_t", "log_sigma_t", "log_phi_u"),
  backward = c("z_beta_raw", "log_phi", "delta_raw", "log_sigma_raw", "delta_free", "log_sigma")
)

read_fit <- function(dir) {
  files <- list.files(file.path(dir, "cmdstan"), pattern = "\\.csv$", full.names = TRUE)
  meta <- read_cmdstan_csv(files, variables = "", sampler_diagnostics = "")$metadata
  # WHY: cmdstanr renames CmdStan's max_depth to max_treedepth; the old name reads NULL,
  # and treedepth__ >= NULL counts no transition at the cap.
  stopifnot(is.numeric(meta$max_treedepth), length(meta$max_treedepth) == 1)
  own <- if ("anchor_w" %in% meta$stan_variables) OWN_PARAMS$anchored else intersect(OWN_PARAMS$backward, meta$stan_variables)
  csv <- read_cmdstan_csv(files, variables = unique(c(KEY, SHARED_PARAMS, own)))
  list(
    draws = csv$post_warmup_draws, sd = csv$post_warmup_sampler_diagnostics,
    time = csv$time$chains, max_treedepth = meta$max_treedepth, own = own,
    wall_min = readRDS(file.path(dir, "timing.rds"))$wall_min
  )
}

pi_eday <- function(draws) {
  a <- unclass(as_draws_array(subset_draws(draws, variable = c("beta0", "mu_gamma_pred"))))
  eta <- a[, , sprintf("beta0[%d]", 1:(P - 1)), drop = FALSE] +
    a[, , sprintf("mu_gamma_pred[%d]", 1:(P - 1)), drop = FALSE]
  full <- array(c(-apply(eta, c(1, 2), sum), eta), dim = c(dim(eta)[1:2], P))
  ex <- exp(sweep(full, c(1, 2), apply(full, c(1, 2), max)))
  pi <- sweep(ex, c(1, 2), apply(ex, c(1, 2), sum), "/")
  dimnames(pi) <- list(NULL, NULL, sprintf("pi_eday[%d]", 1:P))
  as_draws_array(pi)
}

conv <- function(draws) {
  summarise_draws(draws, rhat, ess_bulk, ess_tail) |>
    filter(is.finite(rhat)) |>
    mutate(block = sub("\\[.*$", "", variable))
}

fit_summary <- function(f, name) {
  sd <- as_tibble(as_draws_df(f$sd))
  key_draws <- bind_draws(subset_draws(f$draws, variable = KEY), pi_eday(f$draws), along = "variable")
  key <- conv(key_draws)
  params <- conv(subset_draws(f$draws, variable = c(SHARED_PARAMS, f$own)))
  ebfmi <- sd |>
    summarise(e = sum(diff(energy__)^2) / sum((energy__ - mean(energy__))^2), .by = .chain)
  n_grad <- sum(sd$n_leapfrog__)
  list(
    key = key, key_draws = key_draws,
    row = tibble(
      fit = name,
      wall_min = f$wall_min,
      max_treedepth = f$max_treedepth,
      stepsize = sprintf("%.4f-%.4f", min(sd$stepsize__), max(sd$stepsize__)),
      mean_leapfrog = mean(sd$n_leapfrog__),
      at_cap = sum(sd$treedepth__ >= f$max_treedepth),
      divergent = sum(sd$divergent__),
      min_ebfmi = min(ebfmi$e),
      ms_per_gradient = 1000 * sum(f$time$sampling) / n_grad,
      max_rhat_key = max(key$rhat),
      min_bulk_key = min(key$ess_bulk),
      min_tail_key = min(key$ess_tail),
      max_rhat_params = max(params$rhat),
      min_bulk_params = min(params$ess_bulk),
      min_tail_params = min(params$ess_tail),
      bulk_per_min = min_bulk_key / wall_min,
      tail_per_min = min_tail_key / wall_min,
      bulk_per_1k_grad = 1000 * min_bulk_key / n_grad,
      n_key = nrow(key),
      n_params = nrow(params)
    ),
    treedepth = count(sd, treedepth__) |> mutate(fit = name)
  )
}

old <- fit_summary(read_fit(arg("old")), "old")
new <- fit_summary(read_fit(arg("new")), "new")

diag <- bind_rows(old$row, new$row)
treedepth <- bind_rows(old$treedepth, new$treedepth) |>
  pivot_wider(names_from = fit, values_from = n, values_fill = 0) |>
  arrange(treedepth__)
blocks <- bind_rows(mutate(old$key, fit = "old"), mutate(new$key, fit = "new")) |>
  summarise(min_bulk = min(ess_bulk), min_tail = min(ess_tail), max_rhat = max(rhat), .by = c(fit, block)) |>
  pivot_wider(names_from = fit, values_from = c(min_bulk, min_tail, max_rhat))

probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
quantiles <- function(draws, name) {
  bind_rows(lapply(seq_len(P), function(p) {
    x <- extract_variable_matrix(draws, sprintf("pi_eday[%d]", p))
    tibble(
      party = parties[p], prob = probs, q = quantile(c(x), probs, names = FALSE),
      mcse = mcse_quantile(x, probs)
    )
  })) |>
    mutate(fit = name)
}
agree <- bind_rows(quantiles(old$key_draws, "old"), quantiles(new$key_draws, "new")) |>
  pivot_wider(names_from = fit, values_from = c(q, mcse)) |>
  mutate(z = (q_new - q_old) / sqrt(mcse_new^2 + mcse_old^2))

means <- function(draws, name) {
  summarise_draws(draws, mean, mcse_mean) |>
    filter(is.finite(mcse_mean), mcse_mean > 0) |>
    mutate(fit = name)
}
mean_z <- bind_rows(means(old$key_draws, "old"), means(new$key_draws, "new")) |>
  pivot_wider(names_from = fit, values_from = c(mean, mcse_mean)) |>
  drop_na() |>
  mutate(z = (mean_new - mean_old) / sqrt(mcse_mean_new^2 + mcse_mean_old^2))

write_csv(diag, file.path(out, paste0(label, "_diagnostics.csv")))
write_csv(treedepth, file.path(out, paste0(label, "_treedepth.csv")))
write_csv(blocks, file.path(out, paste0(label, "_blocks.csv")))
write_csv(agree, file.path(out, paste0(label, "_pi_eday_quantiles.csv")))
write_csv(mean_z, file.path(out, paste0(label, "_mean_z.csv")))

cat("\n## ", label, "\n\n", sep = "")
print(as.data.frame(t(diag)))
cat("\ntreedepth__ (post-warmup transitions):\n")
print(as.data.frame(treedepth))
cat(sprintf(
  "\nmin bulk ESS / min: old %.1f, new %.1f (x%.2f); per 1k gradients: old %.4f, new %.4f (x%.2f)\n",
  old$row$bulk_per_min, new$row$bulk_per_min, new$row$bulk_per_min / old$row$bulk_per_min,
  old$row$bulk_per_1k_grad, new$row$bulk_per_1k_grad, new$row$bulk_per_1k_grad / old$row$bulk_per_1k_grad
))
cat("\nmin bulk ESS by block:\n")
print(as.data.frame(select(blocks, block, min_bulk_old, min_bulk_new, max_rhat_old, max_rhat_new)))
cat("\npi_eday quantiles, z = difference / combined MCSE:\n")
print(as.data.frame(mutate(agree, across(c(q_old, q_new), ~ round(100 * .x, 2)), z = round(z, 2)) |>
  select(party, prob, q_old, q_new, z)))
cat(sprintf(
  "\npi_eday quantiles: %d of %d with |z| > 2, max |z| %.2f\n",
  sum(abs(agree$z) > 2), nrow(agree), max(abs(agree$z))
))
cat(sprintf(
  "posterior means of %d key quantities: |z| > 2 for %.1f%%, |z| > 3 for %.2f%%, sd(z) %.2f\n",
  nrow(mean_z), 100 * mean(abs(mean_z$z) > 2), 100 * mean(abs(mean_z$z) > 3), sd(mean_z$z)
))
