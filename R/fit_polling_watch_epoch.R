# Fit polling_watch_v4 (constant pollster bias) or polling_watch_v5_epoch (bias
# that random-walks across election epochs) on a chosen window, optionally as a
# leave-future-out backtest that stops the day before a target election.
#
# Unlike the interactive fit_* scripts this one is run whole, from the project
# root, one fit per call:
#
#   Rscript R/fit_polling_watch_epoch.R --model=v5 --start=2016-01-01 --tag=v5_full
#   Rscript R/fit_polling_watch_epoch.R --model=v4 --cutoff=2024-11-30 --tag=v4_bt2024
#
# Options (defaults in brackets):
#   --model=v4|v5            [v5]
#   --start=YYYY-MM-DD       first poll date kept [2016-01-01]
#   --cutoff=YYYY-MM-DD      backtest target election: drop everything on or
#                            after this date, including its result [none]
#   --estimate_tau=0|1       v5 only: sample the shock SDs [1]
#   --tau_prior=x            v5 only: half-normal scale for sampled taus [0.1]
#   --tau_mu=x --tau_house=x v5 only: fixed shock SDs when estimate_tau=0 [0, 0]
#   --warmup=n --sampling=n  [500, 1000]
#   --chains=n               [4]
#   --seed=n                 [20260930]
#   --tag=name               output folder under results/epoch_bias/ [required]
#   --dry_run=1              stop after printing the epoch table [0]
#   --data_rds=path          fit a saved meta.rds (e.g. simulated data from
#                            R/epoch_bias_simulate.R) instead of building the
#                            data; window/epoch options are then ignored [none]
#
# Writes results/epoch_bias/<tag>/: fit.rds, meta.rds (party/house/date/epoch
# maps), pi_last.parquet (pi_smooth draws on the last date), summary.csv
# (bias, tau, sigma parameters), diag.txt. Prints one "DIAG ..." line.

Sys.setlocale("LC_ALL", "en_US.UTF-8")

suppressMessages({
  library(tidyverse)
  library(here)
  library(cmdstanr)
  library(posterior)
  library(arrow)
  library(clock)
})

box::use(
  R / data[read_polling_data],
  R / stan_data[prepare_polling_watch_data]
)

# ---- options ----------------------------------------------------------------
opt <- list(
  model = "v5", start = "2016-01-01", cutoff = NA_character_,
  estimate_tau = "1", tau_prior = "0.1", tau_mu = "0", tau_house = "0",
  warmup = "500", sampling = "1000", chains = "4", seed = "20260930", tag = NA_character_,
  dry_run = "0", data_rds = NA_character_
)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- str_match(a, "^--([a-z_]+)=(.*)$")
  if (is.na(kv[1, 1]) || !kv[1, 2] %in% names(opt)) stop("Bad argument: ", a)
  opt[[kv[1, 2]]] <- kv[1, 3]
}
stopifnot("--tag is required" = !is.na(opt$tag), opt$model %in% c("v4", "v5"))
start_date <- as.Date(opt$start)
cutoff <- as.Date(opt$cutoff)
out_dir <- here("results", "epoch_bias", opt$tag)
if (opt$dry_run != "1") dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (!is.na(opt$data_rds)) {
  meta <- readRDS(opt$data_rds)
  meta$opt <- opt
  stan_data <- meta$stan_data
  stan_data$estimate_tau <- as.integer(opt$estimate_tau)
  stan_data$tau_prior_scale <- as.numeric(opt$tau_prior)
  stan_data$tau_mu_fixed <- as.numeric(opt$tau_mu)
  stan_data$tau_house_fixed <- as.numeric(opt$tau_house)
  meta$stan_data <- stan_data
  prepared <- list(party_names = meta$party_names)
  date_mapping <- meta$date_mapping
  E <- stan_data$E
  saveRDS(meta, here(out_dir, "meta.rds"))
} else {
# ---- data (same assembly as fit_polling_watch.R) ------------------------------
pre_election <- suppressMessages(read_polling_data()) |>
  filter(date >= start_date) |>
  select(-lokadagur, -p)
post_election <- read_csv(here("data", "post_election_polls.csv"), show_col_types = FALSE) |>
  filter(date >= start_date) |>
  mutate(fyrirtaeki = factor(fyrirtaeki), flokkur = factor(flokkur)) |>
  select(-lokadagur, -p)

polling_data <- bind_rows(pre_election, post_election) |>
  # A zero count is "not reported": dropping it changes no likelihood term (only
  # nonzero columns enter via party_index_n) but lets fct_drop remove a party
  # never polled inside a short backtest window.
  filter(n > 0) |>
  mutate(
    fyrirtaeki = fct_relevel(as_factor(fyrirtaeki), "Kosning"),
    flokkur = fct_relevel(
      as_factor(flokkur),
      "Samfylkingin", "Sjálfstæðisflokkurinn", "Miðflokkurinn", "Viðreisn",
      "Framsóknarflokkurinn", "Flokkur Fólksins", "Vinstri Græn",
      "Sósíalistaflokkurinn", "Píratar", "Annað"
    )
  )
if (!is.na(cutoff)) {
  polling_data <- filter(polling_data, date < cutoff)
}
polling_data <- polling_data |>
  mutate(fyrirtaeki = fct_drop(fyrirtaeki), flokkur = fct_drop(flokkur)) |>
  arrange(date, fyrirtaeki, flokkur)
stopifnot(
  "party order not applied (check LC_ALL locale)" =
    levels(polling_data$flokkur)[1] == "Samfylkingin" &&
      tail(levels(polling_data$flokkur), 1) == "Annað",
  "house 1 must be the election anchor" = levels(polling_data$fyrirtaeki)[1] == "Kosning"
)

prepared <- prepare_polling_watch_data(polling_data)
stan_data <- prepared$stan_data
date_mapping <- prepared$date_mapping

# ---- bias epochs ---------------------------------------------------------------
# A poll dated after the k-th election in the window is in raw epoch k + 1. Raw
# epochs holding no polls (e.g. before an election that opens the window) are
# dropped by renumbering; a gap in the MIDDLE would silently merge two epochs, so
# it is an error.
election_dates <- polling_data |>
  filter(fyrirtaeki == "Kosning") |>
  distinct(date) |>
  pull(date) |>
  sort()
poll_dates <- date_mapping$date[stan_data$date_n]
raw_epoch <- 1L + map_int(poll_dates, \(d) sum(election_dates < d))
is_poll <- stan_data$house_n > 1
used <- sort(unique(raw_epoch[is_poll]))
stopifnot("an epoch between two elections has no polls" = all(diff(used) == 1))
epoch_n <- ifelse(is_poll, match(raw_epoch, used), 1L)
E <- length(used)
epoch_table <- tibble(
  epoch = seq_len(E),
  first_poll = map(used, \(u) min(poll_dates[is_poll & raw_epoch == u])) |> list_c(),
  last_poll = map(used, \(u) max(poll_dates[is_poll & raw_epoch == u])) |> list_c(),
  n_polls = map_int(used, \(u) sum(is_poll & raw_epoch == u))
)
cat("Elections in window:", format(election_dates), "\n")
print(epoch_table)

stan_data$E <- E
stan_data$epoch_n <- as.integer(epoch_n)
stan_data$estimate_tau <- as.integer(opt$estimate_tau)
stan_data$tau_prior_scale <- as.numeric(opt$tau_prior)
stan_data$tau_mu_fixed <- as.numeric(opt$tau_mu)
stan_data$tau_house_fixed <- as.numeric(opt$tau_house)

meta <- list(
  opt = opt,
  party_names = prepared$party_names,
  house_names = prepared$house_names,
  date_mapping = date_mapping,
  election_dates = election_dates,
  epoch_table = epoch_table,
  stan_data = stan_data
)
cat(
  "P =", stan_data$P, " H =", stan_data$H, " D =", stan_data$D,
  " N =", stan_data$N, " E =", E, "\nparties:", prepared$party_names,
  "\nhouses:", prepared$house_names, "\n"
)
if (opt$dry_run == "1") quit(save = "no")
saveRDS(meta, here(out_dir, "meta.rds"))
}

# ---- fit -------------------------------------------------------------------------
stan_file <- if (opt$model == "v4") "polling_watch_v4.stan" else "polling_watch_v5_epoch.stan"
model <- cmdstan_model(here("Stan", stan_file))

fit <- model$sample(
  data = stan_data,
  chains = as.integer(opt$chains),
  parallel_chains = as.integer(opt$chains),
  refresh = 250,
  init = 0,
  seed = as.integer(opt$seed),
  iter_warmup = as.integer(opt$warmup),
  iter_sampling = as.integer(opt$sampling),
  max_treedepth = 11 # as production; see fit_polling_watch.R
)
fit$save_object(here(out_dir, "fit.rds"))

# ---- outputs ---------------------------------------------------------------------
D <- stan_data$D
P <- stan_data$P
pi_last <- fit$draws(sprintf("pi_smooth[%d,%d]", D, seq_len(P)), format = "draws_df") |>
  as_tibble() |>
  pivot_longer(starts_with("pi_smooth"), names_to = "variable", values_to = "value") |>
  mutate(
    p = str_match(variable, ",(\\d+)\\]")[, 2] |> as.integer(),
    flokkur = prepared$party_names[p],
    date = date_mapping$date[D]
  ) |>
  select(.chain, .iteration, .draw, date, flokkur, value)
write_parquet(pi_last, here(out_dir, "pi_last.parquet"))

key_vars <- intersect(
  c("mu_gamma", "gamma", "sigma_gamma", "tau_mu", "tau_house", "sigma", "phi", "beta0"),
  fit$metadata()$stan_variables
)
summ <- fit$summary(key_vars)
write_csv(summ, here(out_dir, "summary.csv"))

all_summ <- fit$summary(c(key_vars, "pi_smooth"), "rhat", "ess_bulk", "ess_tail")
diag <- fit$diagnostic_summary(quiet = TRUE)
sampler <- fit$sampler_diagnostics(format = "draws_df")
diag_line <- sprintf(
  "DIAG tag=%s model=%s E=%d D=%d N=%d max_rhat=%.4f min_ess_bulk=%.0f min_ess_tail=%.0f divergences=%d max_treedepth_hits=%d min_ebfmi=%.3f mean_leapfrog=%.0f wall_s=%.0f",
  opt$tag, opt$model, E, D, stan_data$N,
  max(all_summ$rhat, na.rm = TRUE),
  min(all_summ$ess_bulk, na.rm = TRUE),
  min(all_summ$ess_tail, na.rm = TRUE),
  sum(diag$num_divergent), sum(diag$num_max_treedepth), min(diag$ebfmi),
  mean(sampler$n_leapfrog__), fit$time()$total
)
writeLines(diag_line, here(out_dir, "diag.txt"))
cat(diag_line, "\n")
