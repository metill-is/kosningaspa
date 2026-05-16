suppressMessages({
  library(tidyverse)
  library(posterior)
  library(ggridges)
  library(scales)
  library(ragg)
})

Sys.setlocale("LC_ALL", "is_IS.UTF-8")
options(width = 120)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 1) setwd(dirname(sub("^--file=", "", script_arg)))

N_SEATS <- 23

state <- readRDS("fit_state.rds")
draws <- state$draws
parties_canonical <- state$parties_canonical
house_levels <- state$house_levels
cycle_levels <- state$cycle_levels
time_grid <- state$time_grid
all_obs <- state$all_obs
P_CANON <- nrow(parties_canonical)
LATEST_CYCLE <- last(cycle_levels)

base_theme <- metill::theme_metill() +
  theme(legend.position = "none", plot.title.position = "plot")

# ----- Posterior pi at forecast date -----

draws_pi <- draws$pi_election |>
  as_tibble() |>
  select(starts_with("pi_election")) |>
  set_names(parties_canonical$bokstafur) |>
  mutate(.draw = row_number()) |>
  pivot_longer(-.draw, names_to = "bokstafur", values_to = "pi") |>
  left_join(parties_canonical, by = "bokstafur")

vote_share_summary <- draws_pi |>
  group_by(flokkur, bokstafur, litur) |>
  summarise(
    mean = mean(pi), q05 = quantile(pi, 0.05),
    q50 = median(pi), q95 = quantile(pi, 0.95),
    .groups = "drop"
  ) |>
  arrange(desc(mean))

party_order <- vote_share_summary$flokkur
party_colors <- setNames(vote_share_summary$litur, vote_share_summary$flokkur)

# ----- Plot 1: vote share densities -----

p_votes <- draws_pi |>
  mutate(flokkur = factor(flokkur, levels = rev(party_order))) |>
  ggplot(aes(x = pi, y = flokkur, fill = flokkur)) +
  geom_density_ridges(scale = 1.3, alpha = 0.85, color = "white", linewidth = 0.3) +
  scale_x_continuous(
    labels = metill::label_hlutf(accuracy = 1),
    breaks = seq(0, 0.4, 0.05),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_fill_manual(values = party_colors) +
  labs(
    title = "Reykjavíkurkosningar 2026 — spáð fylgi",
    subtitle = sprintf(
      "Tímabreytilegt líkan — þjálfað á %d athugunum í %d kjörtímabilum",
      nrow(all_obs), length(cycle_levels)
    ),
    x = "Fylgi", y = NULL,
    caption = sprintf(
      "Heimildir: Gallup, Maskína, Fréttablaðið/Zenter + úrslit %s",
      paste(setdiff(cycle_levels, "2026"), collapse = ", ")
    )
  ) +
  base_theme

agg_png("figures/01_fylgi.png", width = 1400, height = 900, res = 140)
print(p_votes)
dev.off()

# ----- D'Hondt seat allocation -----

dhondt <- function(votes, n_seats) {
  party_seats <- numeric(length(votes))
  temp <- votes
  while (sum(party_seats) < n_seats) {
    i <- which.max(temp)
    party_seats[i] <- party_seats[i] + 1
    temp[i] <- votes[i] / (party_seats[i] + 1)
  }
  party_seats
}

pi_wide <- draws_pi |>
  select(.draw, bokstafur, pi) |>
  pivot_wider(names_from = bokstafur, values_from = pi)

seat_mat <- pi_wide[, parties_canonical$bokstafur] |>
  as.matrix() |>
  apply(1, dhondt, n_seats = N_SEATS) |>
  t()
colnames(seat_mat) <- parties_canonical$bokstafur

seats_long <- as_tibble(seat_mat) |>
  mutate(.draw = pi_wide$.draw) |>
  pivot_longer(-.draw, names_to = "bokstafur", values_to = "seats") |>
  left_join(parties_canonical, by = "bokstafur")

# ----- Plot 2: seat heatmap -----

p_seats <- seats_long |>
  mutate(flokkur = factor(flokkur, levels = rev(party_order))) |>
  count(flokkur, seats, litur) |>
  group_by(flokkur) |>
  mutate(prob = n / sum(n)) |>
  ungroup() |>
  filter(prob > 0.005) |>
  ggplot(aes(x = seats, y = flokkur, fill = litur, alpha = prob)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(prob > 0.04, metill::hlutf(prob, accuracy = 1), "")),
    color = "white", size = 3.4, fontface = "bold"
  ) +
  scale_fill_identity() +
  scale_alpha_continuous(range = c(0.2, 1), guide = "none") +
  scale_x_continuous(breaks = 0:12, expand = expansion(add = 0.5)) +
  labs(
    title = sprintf("Spáður fjöldi sæta — %d sæti alls, D’Hondt", N_SEATS),
    subtitle = "Líkur á tilteknum sætafjölda fyrir hvert framboð",
    x = "Sæti", y = NULL
  ) +
  base_theme +
  theme(panel.grid.major.y = element_blank())

agg_png("figures/02_saeti.png", width = 1400, height = 900, res = 140)
print(p_seats)
dev.off()

# ----- Coalitions (minimal winners, respecting declared incompatibilities) -----

active_letters <- setdiff(parties_canonical$bokstafur, "Other")

forbidden_pairs <- list(
  c("C", "M"),
  c("S", "M"),
  c("J", "M"),
  c("V_or_A", "M"),
  c("J", "D"),
  c("V_or_A", "D"),
  c("P", "D"),
  c("P", "M")
)
has_forbidden_pair <- function(combo) any(map_lgl(forbidden_pairs, ~ all(.x %in% combo)))
coalition_prob <- function(combo) mean(rowSums(seat_mat[, combo, drop = FALSE]) >= 12)

coalitions_2 <- combn(active_letters, 2, simplify = FALSE)
coalitions_3 <- combn(active_letters, 3, simplify = FALSE)
coalitions_4 <- combn(active_letters, 4, simplify = FALSE)
coalitions_5 <- combn(active_letters, 5, simplify = FALSE)
all_combos <- c(coalitions_2, coalitions_3, coalitions_4, coalitions_5)

is_minimal_winner <- function(combo, threshold = 0.5) {
  if (length(combo) <= 2) {
    return(TRUE)
  }
  subsets <- list()
  for (k in 2:(length(combo) - 1)) {
    subsets <- c(subsets, combn(combo, k, simplify = FALSE))
  }
  subsets <- subsets[!map_lgl(subsets, has_forbidden_pair)]
  !any(map_lgl(subsets, ~ coalition_prob(.x) >= threshold))
}

coalitions <- tibble(
  size = map_int(all_combos, length),
  combo = all_combos,
  prob = map_dbl(all_combos, coalition_prob)
) |>
  filter(!map_lgl(combo, has_forbidden_pair)) |>
  filter(map_lgl(combo, is_minimal_winner)) |>
  mutate(combo_str = map_chr(combo, ~ paste(.x, collapse = " + "))) |>
  filter(prob > 0.10) |>
  arrange(desc(prob)) |>
  head(15)

p_coalitions <- coalitions |>
  mutate(combo_str = fct_reorder(combo_str, prob)) |>
  ggplot(aes(x = prob, y = combo_str, fill = factor(size))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = metill::hlutf(prob, accuracy = 1)),
    hjust = -0.15, size = 3.6
  ) +
  scale_x_continuous(
    labels = metill::label_hlutf(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_fill_manual(
    values = c("2" = "#41569D", "3" = "#9CCE82", "4" = "#FBB829", "5" = "#984ea3"),
    name = "Stærð", labels = c("2", "3", "4", "5")
  ) +
  labs(
    title = "Lágmarks-meirihlutar (≥12 sæti)",
    subtitle = "Útilokað: C+M, S+M, J+M, V/A+M (hard) + J+D, V/A+D, P+D, P+M (lýstu yfir)",
    x = "Líkur", y = NULL,
    caption = "Aðeins samsetningar þar sem enginn minni hluti nær líka meirihluta"
  ) +
  base_theme +
  theme(legend.position = "top")

agg_png("figures/03_meirihlutar.png", width = 1400, height = 900, res = 140)
print(p_coalitions)
dev.off()

# ----- Plot 4: house effects -----

draws_gamma <- draws$gamma |>
  as_tibble() |>
  select(starts_with("gamma[")) |>
  mutate(.draw = row_number()) |>
  pivot_longer(-.draw, names_to = "var", values_to = "gamma") |>
  mutate(
    indices = str_extract(var, "(?<=\\[).+(?=\\])"),
    p_idx = as.integer(str_split_i(indices, ",", 1)),
    h_idx = as.integer(str_split_i(indices, ",", 2)),
    bokstafur = parties_canonical$bokstafur[p_idx],
    fyrirtaeki = house_levels[h_idx]
  ) |>
  filter(fyrirtaeki != "Kosning") |>
  left_join(parties_canonical, by = "bokstafur")

# Stan now exports gamma as the full P-vector per house (P = 10), including D.
# D no longer needs a reconstruction step — its bias is gamma[1, h] directly
# from the symmetric sum-to-zero parameterisation. (audit C1)

p_gamma <- draws_gamma |>
  mutate(
    flokkur = factor(flokkur, levels = rev(party_order)),
    fyrirtaeki = factor(fyrirtaeki, levels = intersect(
      c("Gallup", "Maskína", "Fréttablaðið"),
      unique(fyrirtaeki)
    ))
  ) |>
  ggplot(aes(x = gamma, y = flokkur, fill = flokkur)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_density_ridges(scale = 1.1, alpha = 0.8, color = "white", linewidth = 0.3) +
  facet_wrap(~fyrirtaeki, nrow = 1) +
  scale_fill_manual(values = party_colors) +
  labs(
    title = "Kannanaáhrif (gamma) eftir flokk og fyrirtæki",
    subtitle = "Frávik frá Kosning-viðmiði á log-hlutfalls kvarða; jákvætt = ofmat",
    x = "Bjögun (log-hlutfall)", y = NULL,
    caption = sprintf("Þjálfað á %d athugunum yfir %d kjörtímabil", nrow(all_obs), length(cycle_levels))
  ) +
  base_theme

agg_png("figures/04_kannanaahrif.png", width = 1600, height = 900, res = 140)
print(p_gamma)
dev.off()

# ----- Plot 5: trajectory — most recent cycle only -----

pi_traj_summary <- draws$pi_trajectory |>
  as_tibble() |>
  select(starts_with("pi_trajectory[")) |>
  pivot_longer(everything(), names_to = "var", values_to = "pi") |>
  mutate(
    indices = str_extract(var, "(?<=\\[).+(?=\\])"),
    p_idx = as.integer(str_split_i(indices, ",", 1)),
    t_idx = as.integer(str_split_i(indices, ",", 2)),
    bokstafur = parties_canonical$bokstafur[p_idx]
  ) |>
  group_by(t_idx, bokstafur) |>
  summarise(
    mean = mean(pi), q05 = quantile(pi, 0.05),
    q25 = quantile(pi, 0.25), q75 = quantile(pi, 0.75),
    q95 = quantile(pi, 0.95),
    .groups = "drop"
  ) |>
  left_join(time_grid |> select(t_global, date, cycle), by = c("t_idx" = "t_global")) |>
  left_join(parties_canonical, by = "bokstafur") |>
  mutate(flokkur = factor(flokkur, levels = party_order)) |>
  filter(cycle == LATEST_CYCLE)

obs_dots <- all_obs |>
  filter(cycle == LATEST_CYCLE, fyrirtaeki != "Kosning") |>
  pivot_longer(all_of(parties_canonical$bokstafur), names_to = "bokstafur", values_to = "count") |>
  group_by(date, fyrirtaeki, cycle) |>
  mutate(p_obs = count / sum(count)) |>
  ungroup() |>
  left_join(parties_canonical, by = "bokstafur") |>
  mutate(flokkur = factor(flokkur, levels = party_order)) |>
  filter(bokstafur != "Other")

p_traj <- pi_traj_summary |>
  filter(bokstafur != "Other") |>
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = litur), alpha = 0.18) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = litur), alpha = 0.35) +
  geom_line(aes(color = litur), linewidth = 0.7) +
  geom_point(
    data = obs_dots,
    aes(x = date, y = p_obs, shape = fyrirtaeki),
    color = "grey25", size = 1.8, alpha = 0.75
  ) +
  facet_wrap(~flokkur, scales = "free_y", ncol = 3) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_shape_manual(values = c(Gallup = 16, Maskína = 17, Fréttablaðið = 15), name = NULL) +
  scale_y_continuous(labels = metill::label_hlutf(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = sprintf("Þróun fylgis — kjörtímabilið %s", LATEST_CYCLE),
    subtitle = "Punktar = einstakar kannanir; band = 50% og 90% posterior",
    x = NULL, y = NULL
  ) +
  metill::theme_metill() +
  theme(
    legend.position = "top", plot.title.position = "plot",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0)
  )

agg_png("figures/05_throun.png", width = 1600, height = 1100, res = 140)
print(p_traj)
dev.off()

cat("\nWrote:\n")
cat("  figures/01_fylgi.png\n")
cat("  figures/02_saeti.png\n")
cat("  figures/03_meirihlutar.png\n")
cat("  figures/04_kannanaahrif.png\n")
cat("  figures/05_throun.png\n")

cat("\nVote share summary:\n")
print(vote_share_summary |>
  mutate(across(c(mean, q05, q50, q95), ~ metill::hlutf(.x, accuracy = 0.1))))

cat("\nParty-specific sigma_p (per sqrt(day)):\n")
sigma_p_long <- draws$sigma_p |>
  posterior::as_draws_df() |>
  posterior::subset_draws(variable = "sigma_p") |>
  posterior::summarise_draws("mean", ~ quantile(.x, 0.05), ~ quantile(.x, 0.95))
sigma_p_long$party <- parties_canonical$bokstafur[seq_len(nrow(sigma_p_long))]
sigma_p_long$party_name <- parties_canonical$flokkur[seq_len(nrow(sigma_p_long))]
print(sigma_p_long |>
  select(party, party_name, mean, `5%`, `95%`) |>
  mutate(across(where(is.numeric), ~ sprintf("%.4f", .x))))

cat("\nHyperprior (log scale):\n")
mu_summary <- posterior::summarise_draws(draws$mu_log_sigma, "mean", ~ quantile(.x, 0.05), ~ quantile(.x, 0.95)) |>
  filter(variable == "mu_log_sigma")
tau_summary <- posterior::summarise_draws(draws$tau_log_sigma, "mean", ~ quantile(.x, 0.05), ~ quantile(.x, 0.95)) |>
  filter(variable == "tau_log_sigma")
print(bind_rows(mu_summary, tau_summary) |>
  mutate(across(where(is.numeric), ~ sprintf("%.4f", .x))))
