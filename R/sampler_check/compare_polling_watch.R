# Compares two fits of the Fylgisvakt model written by
# R/sampler_check/fit_polling_watch_headless.R to the same data (CmdStan CSVs in
# <dir>/cmdstan/ and a timing.rds holding wall_min): sampler diagnostics, the
# treedepth__ distribution, max R-hat and min bulk/tail ESS over the parameters and
# every smoothed share (pi_smooth), ESS per wall minute and per 1,000 sampling-phase
# gradients, and agreement of the last date's smoothed-share quantiles within MCSE:
# z = (q_new - q_old) / sqrt(mcse_new^2 + mcse_old^2).
#
# Usage, from the project root:
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/compare_polling_watch.R --old=<fit dir> \
#     --new=<fit dir> --out=<dir> [--label=fit]
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

KEY <- c(
  "sigma", "Omega", "phi", "mu_phi", "sigma_phi", "gamma", "mu_gamma", "sigma_gamma", "tau_mu", "tau_house",
  "pi_smooth"
)

read_fit <- function(dir) {
  files <- list.files(file.path(dir, "cmdstan"), pattern = "\\.csv$", full.names = TRUE)
  # WHY: v4 has no tau_mu or tau_house, and read_cmdstan_csv() stops on a variable the model lacks
  present <- read_cmdstan_csv(files[1], variables = "lp__", sampler_diagnostics = "")$metadata$stan_variables
  csv <- read_cmdstan_csv(files, variables = intersect(KEY, present))
  timing <- readRDS(file.path(dir, "timing.rds"))
  list(
    dir = dir, draws = csv$post_warmup_draws, diag = as_draws_df(csv$post_warmup_sampler_diagnostics),
    timing = timing, data_md5 = timing$data_md5, max_treedepth = timing$max_treedepth
  )
}

fit_summary <- function(f, which) {
  s <- summarise_draws(f$draws, "rhat", "ess_bulk", "ess_tail", sd = sd)
  # WHY: constant columns (Omega's diagonal, the election house's gamma) have no R-hat or ESS
  s <- filter(s, sd > 0)
  diag <- f$diag
  td <- table(factor(diag$treedepth__, levels = 0:f$max_treedepth))
  per_chain <- diag |>
    summarise(
      stepsize = first(stepsize__), mean_leapfrog = mean(n_leapfrog__),
      at_cap = sum(treedepth__ >= f$max_treedepth), divergent = sum(divergent__), .by = .chain
    )
  gradients <- sum(diag$n_leapfrog__)
  sampling_min <- sum(f$timing$time$chains$sampling) / 60 / nrow(per_chain)
  worst <- s |> slice_min(ess_bulk, n = 5, with_ties = FALSE)
  row <- tibble(
    fit = which, dir = f$dir, wall_min = f$timing$wall_min, sampling_min = sampling_min,
    max_treedepth = f$max_treedepth, at_cap = sum(per_chain$at_cap), divergent = sum(per_chain$divergent),
    stepsize_min = min(per_chain$stepsize), stepsize_max = max(per_chain$stepsize),
    mean_leapfrog = mean(diag$n_leapfrog__),
    max_rhat = max(s$rhat, na.rm = TRUE), n_rhat_na = sum(is.na(s$rhat)),
    min_ess_bulk = min(s$ess_bulk, na.rm = TRUE), min_ess_tail = min(s$ess_tail, na.rm = TRUE),
    min_ess_bulk_pi = min(s$ess_bulk[startsWith(s$variable, "pi_smooth")], na.rm = TRUE),
    ess_per_wall_min = min(s$ess_bulk, na.rm = TRUE) / f$timing$wall_min,
    ess_per_1k_grad = 1000 * min(s$ess_bulk, na.rm = TRUE) / gradients
  )
  list(row = row, td = td, per_chain = per_chain, worst = worst, draws = f$draws)
}

old <- fit_summary(read_fit(arg("old")), "old")
new <- fit_summary(read_fit(arg("new")), "new")
summ <- bind_rows(old$row, new$row)
write_csv(summ, file.path(out, paste0(label, "_summary.csv")))
cat("\nper chain:\n")
print(bind_rows(mutate(old$per_chain, fit = "old"), mutate(new$per_chain, fit = "new")))
cat("\ntreedepth__ (sampling iterations):\n")
print(rbind(old = old$td[seq_len(min(length(old$td), length(new$td)))], new = new$td[seq_len(min(length(old$td), length(new$td)))]))
cat("\nslowest five by bulk ESS:\n")
print(bind_rows(mutate(old$worst, fit = "old"), mutate(new$worst, fit = "new")))
cat("\n")
print(as.data.frame(summ[, setdiff(names(summ), "dir")]), digits = 4)

same_data <- identical(
  readRDS(file.path(arg("old"), "timing.rds"))$data_md5,
  readRDS(file.path(arg("new"), "timing.rds"))$data_md5
)
if (!same_data) {
  cat("\nThe two fits read different data, so their smoothed shares are not compared.\n")
  quit(save = "no")
}
last <- max(as.integer(sub("^pi_smooth\\[(\\d+),.*$", "\\1", grep("^pi_smooth\\[", variables(old$draws), value = TRUE))))
vars <- grep(sprintf("^pi_smooth\\[%d,", last), variables(old$draws), value = TRUE)
probs <- c(0.05, 0.5, 0.95)
agree <- bind_rows(lapply(vars, function(v) {
  bind_rows(lapply(probs, function(p) {
    xo <- extract_variable_matrix(old$draws, v)
    xn <- extract_variable_matrix(new$draws, v)
    qo <- quantile(xo, p)
    qn <- quantile(xn, p)
    se <- sqrt(mcse_quantile(xo, p)^2 + mcse_quantile(xn, p)^2)
    tibble(variable = v, prob = p, old = unname(qo), new = unname(qn), z = unname((qn - qo) / se))
  }))
}))
write_csv(agree, file.path(out, paste0(label, "_pi_last_quantiles.csv")))
cat(sprintf("\nlast date's pi_smooth quantiles, z = difference / combined MCSE:\n"))
print(as.data.frame(agree), digits = 4)
cat(sprintf(
  "\npi_smooth[%d, ] quantiles: %d of %d with |z| > 2, max |z| %.2f; max |difference| %.2f pp\n",
  last, sum(abs(agree$z) > 2), nrow(agree), max(abs(agree$z)), 100 * max(abs(agree$new - agree$old))
))
