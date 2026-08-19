# WHY: under a C locale (headless Rscript) string matching against the Icelandic
# party names fails, so the fct_relevel() below silently no-ops. Pin a UTF-8 locale
# so the party order comes out the same headless as interactive.
#
# The order itself is only a determinism convention -- NOT a tuning knob. Measured
# 2026-08-19, three fits on current data at matched settings:
#   * Results are invariant. Reordering moves posterior medians by <=0.01pp, LESS
#     than a reseed of the same order does (<=0.02pp). polling_watch_v4's
#     reference invariance holds comfortably.
#   * Sampling is unaffected too. An earlier version of this comment claimed
#     0% max-treedepth here vs ~60-100% with the tiny "Annað" residual placed
#     early; that does not reproduce. Annað-FIRST scored 1.0%, in between two
#     runs of the order below (9.0% and 0.0%).
#   * Max-treedepth warnings come from step-size adaptation variance, not from
#     ordering and not from initial values. This posterior needs a fixed distance
#     per trajectory, which at the adapted step size costs almost exactly 511
#     leapfrog steps = 2^9 -- i.e. it sits ON a power-of-two boundary, with only
#     one doubling of headroom under the default cap of 10. Any chain that adapts
#     a slightly smaller epsilon spills over. Measured on the 2026-08-19 fit:
#         chain 1  eps 0.00718   0.0% at cap   511 leapfrog
#         chain 2  eps 0.00752   0.0% at cap   511 leapfrog
#         chain 3  eps 0.00650  25.7% at cap   742 leapfrog   <- 12% smaller eps
#         chain 4  eps 0.00743   0.0% at cap   511 leapfrog
#     No divergences, rhat 1.00, posterior unaffected -- the cap is binding, not
#     the geometry misbehaving.
#
#     Better inits (e.g. Pathfinder) will NOT help: init = 0 starts all chains at
#     the SAME point, so the epsilon spread is pure adaptation RNG, with no
#     starting-point variation to remove. Drawing a separate Pathfinder init per
#     chain would add between-chain variation that does not currently exist.
#     If you want to attack the spread directly the knob is term_buffer (default
#     50 iterations is the whole sample behind the final epsilon). We instead take
#     the simpler route: max_treedepth = 11 is set on the sample() call below, so
#     the occasional conservative chain completes its trajectory rather than being
#     truncated. Do not reorder parties chasing this.
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(here)
library(cmdstanr)
library(posterior)
library(arrow)
library(clock)

box::use(
  R / data[read_polling_data],
  R / stan_data[prepare_polling_watch_data]
)

# Read pre-election data (includes 2021 election + polls up to Nov 2024)
pre_election <- read_polling_data() |>
  filter(
    date >= date_build(2021, 9, 25)
  ) |>
  select(-lokadagur, -p)

# Read post-election data (2024 election + polls from Dec 2024 onward)
post_election_path <- here("data", "post_election_polls.csv")
if (file.exists(post_election_path)) {
  post_election <- read_csv(post_election_path, show_col_types = FALSE) |>
    mutate(
      fyrirtaeki = factor(fyrirtaeki),
      flokkur = factor(flokkur)
    ) |>
    select(-lokadagur, -p)
} else {
  post_election <- tibble()
}

# Combine and harmonize factor levels
polling_data <- bind_rows(pre_election, post_election) |>
  mutate(
    fyrirtaeki = fct_relevel(
      as_factor(fyrirtaeki),
      "Kosning"
    ),
    # Fixed party order. Immaterial to both results and sampling speed (see the
    # measurement note at the top of this file) -- pinned purely so the fit is
    # reproducible and locale-independent. Sorting this by party size would be a
    # no-op: in the RAW polls Samfylkingin still leads Sjalfstaedisflokkurinn at
    # every recent window; D leads only in the house-effect-corrected latent.
    flokkur = fct_relevel(
      as_factor(flokkur),
      "Samfylkingin",
      "Sjálfstæðisflokkurinn",
      "Miðflokkurinn",
      "Viðreisn",
      "Framsóknarflokkurinn",
      "Flokkur Fólksins",
      "Vinstri Græn",
      "Sósíalistaflokkurinn",
      "Píratar",
      "Annað"
    )
  ) |>
  arrange(date, fyrirtaeki, flokkur)

# Guard: the relevel must actually have taken. Party order does not change results,
# so a no-op here is not a correctness bug -- but it means the locale pin above
# failed, which is worth catching loudly before it breaks something that does care.
stopifnot(
  "party order not applied — fct_relevel no-opped (check LC_ALL locale)" =
    levels(polling_data$flokkur)[1] == "Samfylkingin" &&
      tail(levels(polling_data$flokkur), 1) == "Annað"
)

unique(polling_data$flokkur)
cat(
  "Date range:", as.character(min(polling_data$date)),
  "to", as.character(max(polling_data$date)), "\n"
)
cat("Polls:", polling_data |> distinct(date, fyrirtaeki) |> nrow(), "\n")

prepared <- prepare_polling_watch_data(polling_data)
stan_data <- prepared$stan_data
date_mapping <- prepared$date_mapping
party_names <- prepared$party_names
house_names <- prepared$house_names

str(stan_data)

model <- cmdstan_model(
  here("Stan", "polling_watch_v4.stan")
)

fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  init = 0,
  iter_warmup = 500,
  iter_sampling = 1000,
  # This posterior legitimately needs ~511 leapfrog steps = 2^9 per trajectory, so
  # the default cap of 10 leaves a single doubling of headroom and any chain that
  # adapts a slightly small step size gets truncated. See the note at the top of
  # this file.
  #
  # Verified 2026-08-19 on seed 20260819 (a seed that truncates under the default):
  # nothing reaches depth 11, so the warning is gone -- but the chain that used to
  # be cut off (eps 0.00625) genuinely wants depth 10 on 81% of its transitions and
  # now runs 960 leapfrog steps instead of being stopped. That is the point: a
  # truncated NUTS trajectory is a trajectory stopped before its U-turn, which
  # biases exploration. It is NOT free, though -- the well-adapted chains are
  # unchanged at 511 steps, but the slow chain's sampling goes 204s -> 399s, so
  # wall-clock rose 458s -> 586s (+28%). Bought with that: min ESS 3665 -> 4324
  # (+18%), still 0 divergences, rhat 1.000. Worth it for a monthly job.
  max_treedepth = 11
)

fit$summary("sigma")
fit$summary("phi")
fit$summary("gamma")
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

# Create output directory and save
output_dir <- here("data", as.character(today()))
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_parquet(pi_draws, here(output_dir, "polling_watch_draws.parquet"))

# Persist the full fit so the entire posterior can be re-queried (any parameter,
# any draw) without re-fitting: readRDS(...)$draws("gamma"), etc.
fit$save_object(here(output_dir, "polling_watch_fit.rds"))

# Party-space innovation correlation (Omega) draws, labelled by party, for the
# correlation/precision analysis. polling_watch_v4 gives a reference-invariant P x P Omega.
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

# House effects (gamma) draws, labelled by firm + party. gamma[1] is the election
# anchor (pinned to 0), so keep only the polling houses (h > 1). On the softmax
# log-odds scale: positive => the house systematically over-states that party
# relative to the election-anchored latent trend. Zero-sum across parties per house.
gamma_draws <- fit$draws("gamma") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.iteration, -.draw), names_to = "variable", values_to = "value") |>
  mutate(
    h = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
    fyrirtaeki = house_names[h],
    flokkur = party_names[p]
  ) |>
  filter(h > 1) |>
  select(.chain, .iteration, .draw, fyrirtaeki, flokkur, value)
write_parquet(gamma_draws, here(output_dir, "polling_watch_gamma.parquet"))

# Shared industry bias (mu_gamma): the lean common to every polling house, per party.
mu_gamma_draws <- fit$draws("mu_gamma") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.iteration, -.draw), names_to = "variable", values_to = "value") |>
  mutate(
    p = str_match(variable, "mu_gamma\\[(.*)\\]")[, 2] |> parse_number(),
    flokkur = party_names[p]
  ) |>
  select(.chain, .iteration, .draw, flokkur, value)
write_parquet(mu_gamma_draws, here(output_dir, "polling_watch_mu_gamma.parquet"))

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
