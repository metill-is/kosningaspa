# Headless fit of a Fylgisvakt model for sampler checks: by default
# Stan/polling_watch_v4.stan, the comparison model; production fits polling_watch_v5_epoch.stan.
# Builds stan_data as R/fit_polling_watch.R does (read_polling_watch_data(), then
# prepare_polling_watch_data()), from --start on or from the first poll and, with --cutoff,
# only polls dated before it (a leave-future-out fit), and samples with init 0. A model
# whose data declare epoch_n (polling_watch_v5_epoch.stan) also gets the bias epochs of
# prepare_polling_watch_epochs() and the tau options. It writes nothing under data/.
#
# Usage, from the project root (here() must find data/):
#   LC_ALL=en_US.UTF-8 Rscript R/sampler_check/fit_polling_watch_headless.R --out=<dir>
#     [--stan=Stan/polling_watch_v4.stan] [--data-json=<file>] [--start=YYYY-MM-DD]
#     [--cutoff=YYYY-MM-DD]
#     [--max-treedepth=12] [--seed=4100] [--warmup=1000] [--sampling=1000] [--chains=4]
#     [--estimate-tau=1] [--tau-prior=0.1] [--tau-mu=0] [--tau-house=0]
#     [--data-only]
# --data-json is read when it exists and written otherwise, so two models can be fitted
# to identical data; --start, --cutoff or a tau option with an existing --data-json is an
# error. Writes <out>/cmdstan/*.csv, <out>/timing.rds, <out>/diagnostics.csv and <out>/DONE,
# and, when it builds the data, <out>/meta.rds (party, house and date labels, and the epoch
# table for v5).
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(width = 160, warn = 1)
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(cmdstanr)
  library(posterior)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
out <- arg("out")
stopifnot("--out is required" = !is.null(out))
dir.create(file.path(out, "cmdstan"), recursive = TRUE, showWarnings = FALSE)
stan_file <- arg("stan", here("Stan", "polling_watch_v4.stan"))
data_json <- arg("data-json", file.path(out, "stan_data.json"))
start <- arg("start")
cutoff <- arg("cutoff")
max_treedepth <- as.integer(arg("max-treedepth", "12"))
seed <- as.integer(arg("seed", "4100"))
iter_warmup <- as.integer(arg("warmup", "1000"))
iter_sampling <- as.integer(arg("sampling", "1000"))
chains <- as.integer(arg("chains", "4"))

mod <- cmdstan_model(stan_file, compile = FALSE)
epochs <- "epoch_n" %in% names(mod$variables()$data)

data_built_here <- !file.exists(data_json)
if (data_built_here) {
  box::use(
    R / data[read_polling_watch_data],
    R / stan_data[prepare_polling_watch_data, prepare_polling_watch_epochs]
  )
  polling_data <- read_polling_watch_data(if (is.null(start)) NULL else as.Date(start))
  if (!is.null(cutoff)) polling_data <- filter(polling_data, date < as.Date(cutoff))
  prepared <- prepare_polling_watch_data(polling_data)
  if (epochs) {
    prepared <- prepare_polling_watch_epochs(
      prepared,
      estimate_tau = arg("estimate-tau", "1") == "1",
      tau_prior_scale = as.numeric(arg("tau-prior", "0.1")),
      tau_mu_fixed = as.numeric(arg("tau-mu", "0")),
      tau_house_fixed = as.numeric(arg("tau-house", "0"))
    )
    print(prepared$epoch_table)
  }
  write_stan_json(prepared$stan_data, data_json)
  saveRDS(
    prepared[intersect(c("date_mapping", "party_names", "house_names", "epoch_table"), names(prepared))],
    file.path(out, "meta.rds")
  )
  sd_ <- prepared$stan_data
  cat(sprintf(
    "DATA %s: D=%d N=%d H=%d P=%d, %s to %s, elections %s\n", data_json, sd_$D, sd_$N, sd_$H, sd_$P,
    min(prepared$date_mapping$date), max(prepared$date_mapping$date),
    paste(prepared$date_mapping$date[unique(sd_$date_n[sd_$house_n == 1])], collapse = " ")
  ))
  cat("parties:", prepared$party_names, "\nhouses:", prepared$house_names, "\n")
} else {
  # WHY: the data file holds the epochs and tau settings too, so an option it overrides
  # would be ignored without a word
  tau_opts <- grep("^--(estimate-tau|tau-prior|tau-mu|tau-house)=", args, value = TRUE)
  stopifnot(
    "--start, --cutoff and the tau options need a new --data-json" =
      is.null(start) && is.null(cutoff) && length(tau_opts) == 0
  )
}
if ("--data-only" %in% args) quit(save = "no")

mod$compile()
load_before <- system("sysctl -n vm.loadavg", intern = TRUE)
t0 <- Sys.time()
fit <- mod$sample(
  data = data_json, chains = chains, parallel_chains = chains, init = 0, seed = seed,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling, max_treedepth = max_treedepth,
  output_dir = file.path(out, "cmdstan"), refresh = 250
)
wall_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
saveRDS(
  list(
    time = fit$time(), wall_min = wall_min, stan_file = stan_file, data_json = data_json,
    data_md5 = unname(tools::md5sum(data_json)), data_built_here = data_built_here,
    start = if (!data_built_here) NA_character_ else if (is.null(start)) "all" else start,
    cutoff = if (is.null(cutoff)) NA_character_ else cutoff,
    max_treedepth = max_treedepth, seed = seed, chains = chains,
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
