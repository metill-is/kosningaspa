# Headless fit of the production constituency model, for sampler checks.
# Builds stan_data exactly as R/fit_polling_and_fundamentals_kjordaemi_model.R does
# (its data section, up to str(stan_data)), optionally with every poll after a
# forecast date dropped, then samples 4 chains with init 0. The cutoff serves sampler
# checks only, not leave-future-out forecasts: the election date, the stjornarslit
# window, the fundamentals and n_pred_k stay as the fit script sets them.
#
# Usage, from the project root (here() must find data/):
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/fit_headless.R --out=<dir>
#     [--stan=Stan/polling_and_fundamentals_kjordaemi.stan] [--data-json=<file>]
#     [--cutoff=YYYY-MM-DD] [--max-treedepth=12] [--seed=4100]
#     [--warmup=1000] [--sampling=1000] [--data-only]
# --data-json is read when it exists and written otherwise, so two models can be
# fitted to identical data; --cutoff with an existing --data-json is an error. Writes <out>/cmdstan/*.csv, <out>/timing.rds,
# <out>/diagnostics.csv and <out>/DONE.
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(width = 160, warn = 1)
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(cmdstanr)
  library(posterior)
  library(clock)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
out <- arg("out")
stopifnot(!is.null(out))
stan_file <- arg("stan", here("Stan", "polling_and_fundamentals_kjordaemi.stan"))
data_json <- arg("data-json", file.path(out, "stan_data.json"))
cutoff <- arg("cutoff")
max_treedepth <- as.integer(arg("max-treedepth", "12"))
seed <- as.integer(arg("seed", "4100"))
iter_warmup <- as.integer(arg("warmup", "1000"))
iter_sampling <- as.integer(arg("sampling", "1000"))
if (!is.null(cutoff) && file.exists(data_json)) {
  stop("--cutoff is only applied when building --data-json, and ", data_json, " exists")
}
dir.create(file.path(out, "cmdstan"), recursive = TRUE, showWarnings = FALSE)

build_stan_data <- function(cutoff) {
  box::use(
    R / data[read_polling_data, read_fundamentals_data, read_constituency_data],
    R / stan_data[prepare_stan_data]
  )
  keep_date <- function(date) if (is.null(cutoff)) rep(TRUE, length(date)) else date <= as.Date(cutoff)

  polling_data <- read_polling_data() |>
    filter(
      date >= date_build(2016, 1, 1),
      (fyrirtaeki != "Gallup") | (date <= date_build(2024, 11, 10)),
      keep_date(date)
    ) |>
    select(-lokadagur, -p)
  # WHY: under a C locale fct_relevel() silently skips the Icelandic names and the
  # softmax reference stops being Annað, which changes the model.
  stopifnot(
    levels(polling_data$flokkur)[1] == "Annað",
    levels(polling_data$fyrirtaeki)[1] == "Kosning"
  )

  fundamentals_data <- read_fundamentals_data() |>
    inner_join(read_csv(here("data", "economy_data.csv"), show_col_types = FALSE), by = "date")

  constituency_data <- read_constituency_data() |>
    filter(
      date >= date_build(2021, 9, 25),
      date != date_build(2024, 11, 15),
      (fyrirtaeki != "Gallup") | (date <= date_build(2024, 11, 10)),
      keep_date(date)
    ) |>
    drop_na() |>
    mutate(
      fyrirtaeki = factor(fyrirtaeki, levels = levels(polling_data$fyrirtaeki)),
      flokkur = factor(flokkur, levels = levels(polling_data$flokkur)),
      kjordaemi = as_factor(kjordaemi)
    ) |>
    arrange(flokkur, date, kjordaemi)

  stan_data <- prepare_stan_data(polling_data, fundamentals_data, constituency_data)

  constituency_weights <- constituency_data |>
    summarise(n = sum(n), .by = c(kjordaemi, date)) |>
    mutate(p = n / sum(n), .by = date) |>
    select(-n) |>
    pivot_wider(names_from = kjordaemi, values_from = p, values_fill = 0) |>
    column_to_rownames("date")
  N_obs_k <- nrow(constituency_weights)
  obs_k <- rep(1:N_obs_k, each = ncol(constituency_weights))
  stopifnot(length(obs_k) == stan_data$N_k)

  kjorsokn <- c(82.0, 81.0, 78.8, 81.2, 79.2, 78.8) / 100
  kjorskra <- c(22348, 31039, 40994, 79052, 47503, 47486)
  stan_data$n_pred_k <- as.integer((kjorskra * kjorsokn)[c(5, 6, 4, 1, 2, 3)])

  stan_data$constituency_weights <- as.matrix(constituency_weights)
  stan_data$constituency_weights_pred <- constituency_weights[1, ] |> as.numeric()
  stan_data$desired_weight <- 0.22
  stan_data$weight_time <- 180
  stan_data$N_obs_k <- N_obs_k
  stan_data$obs_k <- obs_k
  stan_data
}

data_built_here <- !file.exists(data_json)
if (data_built_here) {
  stan_data <- build_stan_data(cutoff)
  write_stan_json(stan_data, data_json)
  cat(sprintf(
    "DATA %s: D=%d N_n=%d N_k=%d N_obs_k=%d pred_y_time_diff=%d\n",
    data_json, stan_data$D, stan_data$N_n, stan_data$N_k, stan_data$N_obs_k, stan_data$pred_y_time_diff
  ))
}
if ("--data-only" %in% args) quit(save = "no")

mod <- cmdstan_model(stan_file)
load_before <- system("sysctl -n vm.loadavg", intern = TRUE)
t0 <- Sys.time()
fit <- mod$sample(
  data = data_json, chains = 4, parallel_chains = 4, init = 0, seed = seed,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling, max_treedepth = max_treedepth,
  output_dir = file.path(out, "cmdstan"), refresh = 250
)
wall_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
saveRDS(
  list(
    time = fit$time(), wall_min = wall_min, stan_file = stan_file, data_json = data_json,
    data_md5 = unname(tools::md5sum(data_json)), data_built_here = data_built_here,
    data_cutoff = if (!data_built_here) NA_character_ else if (is.null(cutoff)) "none" else cutoff,
    max_treedepth = max_treedepth, seed = seed,
    load_before = load_before, load_after = system("sysctl -n vm.loadavg", intern = TRUE)
  ),
  file.path(out, "timing.rds")
)

sd <- as_draws_df(fit$sampler_diagnostics())
diag <- tibble(
  chain = sd$.chain, treedepth = sd$treedepth__, n_leapfrog = sd$n_leapfrog__,
  divergent = sd$divergent__, stepsize = sd$stepsize__
) |>
  summarise(
    stepsize = first(stepsize),
    mean_leapfrog = mean(n_leapfrog),
    max_treedepth_seen = max(treedepth),
    at_cap = sum(treedepth >= max_treedepth),
    divergent = sum(divergent),
    .by = chain
  ) |>
  mutate(ebfmi = as.numeric(fit$diagnostic_summary(quiet = TRUE)$ebfmi))
write_csv(diag, file.path(out, "diagnostics.csv"))
print(diag)
cat(sprintf(
  "DIAG %s: wall %.1f min, at cap %d of %d, divergent %d, min E-BFMI %.3f\n",
  out, wall_min, sum(diag$at_cap), nrow(sd), sum(diag$divergent), min(diag$ebfmi)
))
file.create(file.path(out, "DONE"))
