# Fits the Fylgisvakt smoother (Stan/polling_watch_v5_epoch.stan) to the polls and
# election results since 1 January 2016 and writes data/<YYYY-MM-DD>/ for the platform
# export. Fylgisvakt is the election forecast's poll model without its fundamentals, a
# summary of where the polls stand between elections, so it reads the forecast's window
# (R/fit_polling_and_fundamentals_kjordaemi_model.R). The pollsters' bias may change at
# each election: an industry bias and house effects per epoch between results, a random
# walk over the epochs with sampled shock scales (prepare_polling_watch_epochs()). Until
# 28 October 2026 the fit used polling_watch_v4.stan, whose bias is constant over the
# window.
#
# Until 28 October 2026 the fit started at the 2021 election, because the model's
# earlier parameterisation could not sample a longer window: it drew the walk forward
# from its first date, and each election result then pinned a sum of every earlier step.
# From 2013 every transition reached the depth-11 cap at step size 0.002. The walk is now
# anchored at the election results (the model's PARAMETERISATION note): v4 samples the
# series from 2013 at depth 8-9 and v5 this window at depth 8, with no divergent
# transition; R/sampler_check/RESULTS.md (Fylgisvakt) has the comparisons.
#
# WHY: under a C locale (headless Rscript) string matching against the Icelandic party
# names fails, so read_polling_watch_data()'s party order would not apply (its stopifnot
# catches that). The order is a determinism convention, not a tuning knob: reordering
# moved posterior medians by <= 0.01 pp on 2026-08-19, less than a reseed.
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(here)
library(cmdstanr)
library(posterior)
library(arrow)

box::use(
  R / data[read_polling_watch_data],
  R / stan_data[prepare_polling_watch_data, prepare_polling_watch_epochs]
)

# The 2016, 2017, 2021 and 2024 results anchor the walk (read_polling_watch_data()
# documents the files and the fixed party order, and merges same-day polls by one house).
polling_data <- read_polling_watch_data(start = as.Date("2016-01-01"))

# The post-election polls this fit saw, written beside its draws below: the platform
# export reads its poll points from that copy, since the live CSV may gain polls first.
polls_csv <- read_file_raw(here("data", "post_election_polls.csv"))

cat(
  "Date range:", as.character(min(polling_data$date)),
  "to", as.character(max(polling_data$date)), "\n"
)
cat("Polls:", polling_data |> distinct(date, fyrirtaeki) |> nrow(), "\n")

# The bias epochs, with prepare_polling_watch_epochs()' tau settings: shock scales sampled,
# half-normal prior scale 0.1
prepared <- prepare_polling_watch_data(polling_data) |>
  prepare_polling_watch_epochs()
print(prepared$epoch_table)
stan_data <- prepared$stan_data
date_mapping <- prepared$date_mapping
party_names <- prepared$party_names
house_names <- prepared$house_names

str(stan_data)

model <- cmdstan_model(
  here("Stan", "polling_watch_v5_epoch.stan")
)

# WHY: transitions stop at depth 8 (255 leapfrog steps) on this window; the default cap
# of 10 would leave one doubling of headroom over a slower chain at depth 9, the margin
# that truncated the earlier model's slower chains. A transition at the cap fails the
# publish gate.
max_treedepth <- 12

fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  init = 0,
  iter_warmup = 1000,
  iter_sampling = 1000,
  max_treedepth = max_treedepth
)

fit$summary("sigma")
fit$summary("phi")
fit$summary("gamma")
fit$summary(c("tau_mu", "tau_house"))
fit$summary("Omega")

# Extract pi_smooth draws → long tibble
pi_draws <- fit$draws("pi_smooth") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw),
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    d = str_match(variable, "pi_smooth\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "pi_smooth\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = party_names[p]
  ) |>
  inner_join(
    date_mapping,
    by = c("d" = "index")
  ) |>
  rename(dags = date) |>
  select(.chain, .iteration, .draw, dags, flokkur, value)

output_dir <- here("data", as.character(today()))
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
# WHY: a second fit on the same day would otherwise write its polls and diagnostics beside
# the first fit's draws, and a failure before its own draws left a folder that looks complete
unlink(here(output_dir, "polling_watch_draws.parquet"))
write_file(polls_csv, here(output_dir, "post_election_polls.csv"))

# Convergence summary beside the draws (written before them: the platform takes the
# draws parquet as the sign of a finished fit), for the publish gate in
# ~/metill-platform/scripts/regenerate_kosningaspa.py (it refuses a fit with a divergent
# transition, a transition at the depth cap, R-hat above 1.01 or bulk ESS below 400).
# R-hat and ESS run over the parameters and every smoothed share; constant columns
# (Omega's diagonal, the election house's gamma) have none and are left out.
key_summary <- fit$summary(
  c(
    "sigma", "Omega", "phi", "mu_phi", "sigma_phi", "gamma", "mu_gamma", "sigma_gamma",
    "tau_mu", "tau_house", "pi_smooth"
  ),
  "rhat", "ess_bulk",
  sd = sd
) |>
  filter(sd > 0)
sampler <- fit$sampler_diagnostics(format = "df")
diagnostics <- list(
  num_divergent = sum(sampler$divergent__),
  max_treedepth_hits = sum(sampler$treedepth__ >= max_treedepth),
  max_rhat = max(key_summary$rhat),
  min_ess_bulk = min(key_summary$ess_bulk),
  max_treedepth = max_treedepth,
  stan_model = basename(model$stan_file()),
  chains = fit$num_chains(),
  iter_sampling = nrow(sampler) / fit$num_chains(),
  first_date = as.character(min(date_mapping$date)),
  last_date = as.character(max(date_mapping$date))
)
# WHY: na = "null" writes an NA or infinite R-hat or ESS as null, which the gate refuses;
# jsonlite's default writes the string "NA"
jsonlite::write_json(
  diagnostics, here(output_dir, "diagnostics.json"),
  auto_unbox = TRUE, digits = NA, na = "null", pretty = TRUE
)
str(diagnostics)

write_parquet(pi_draws, here(output_dir, "polling_watch_draws.parquet"))

# Persist the full fit so the entire posterior can be re-queried (any parameter,
# any draw) without re-fitting: readRDS(...)$draws("gamma"), etc.
fit$save_object(here(output_dir, "polling_watch_fit.rds"))

# Party-space innovation correlation (Omega) draws, labelled by party, for the
# correlation/precision analysis. The model gives a reference-invariant P x P Omega.
omega_draws <- fit$draws("Omega") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.iteration, -.draw), names_to = "variable", values_to = "value") |>
  mutate(
    i = str_match(variable, "Omega\\[(.*),.*\\]")[, 2] |> parse_number(),
    j = str_match(variable, "Omega\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur_i = party_names[i],
    flokkur_j = party_names[j]
  ) |>
  select(.chain, .iteration, .draw, flokkur_i, flokkur_j, value)
write_parquet(omega_draws, here(output_dir, "polling_watch_omega.parquet"))

# House effects (gamma[e, h, p]) draws, labelled by bias epoch (polling_watch_epochs.csv
# has their dates), firm and party. House 1 is the election anchor (pinned to 0), and a
# house's effect in an epoch it published no poll in is only its walk from the epochs
# around it, so each house keeps the epochs it polled in. On the softmax log-odds scale:
# positive => the house over-states that party relative to the election-anchored latent
# trend. Zero-sum across parties per house.
polled <- tibble(epoch = stan_data$epoch_n, h = stan_data$house_n) |>
  filter(h > 1) |>
  distinct()
gamma_draws <- fit$draws("gamma") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.iteration, -.draw), names_to = "variable", values_to = "value") |>
  mutate(
    epoch = str_match(variable, "gamma\\[(\\d+),")[, 2] |> parse_number(),
    h = str_match(variable, "gamma\\[\\d+,(\\d+),")[, 2] |> parse_number(),
    p = str_match(variable, ",(\\d+)\\]")[, 2] |> parse_number(),
    fyrirtaeki = house_names[h],
    flokkur = party_names[p]
  ) |>
  semi_join(polled, by = c("epoch", "h")) |>
  select(.chain, .iteration, .draw, epoch, fyrirtaeki, flokkur, value)
write_parquet(gamma_draws, here(output_dir, "polling_watch_gamma.parquet"))

# Shared industry bias (mu_gamma[e, p]): the lean common to every polling house, per
# epoch and party.
mu_gamma_draws <- fit$draws("mu_gamma") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.iteration, -.draw), names_to = "variable", values_to = "value") |>
  mutate(
    epoch = str_match(variable, "mu_gamma\\[(\\d+),")[, 2] |> parse_number(),
    p = str_match(variable, ",(\\d+)\\]")[, 2] |> parse_number(),
    flokkur = party_names[p]
  ) |>
  select(.chain, .iteration, .draw, epoch, flokkur, value)
write_parquet(mu_gamma_draws, here(output_dir, "polling_watch_mu_gamma.parquet"))
write_csv(prepared$epoch_table, here(output_dir, "polling_watch_epochs.csv"))

# Quick summary check
pi_draws |>
  filter(dags == max(dags)) |>
  summarise(
    median = median(value),
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .by = flokkur
  ) |>
  arrange(desc(median))
