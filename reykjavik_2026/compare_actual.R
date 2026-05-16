suppressMessages({
  library(tidyverse)
  library(ggridges)
  library(ragg)
})

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 1) setwd(dirname(sub("^--file=", "", script_arg)))

actual_raw <- tribble(
  ~letter, ~pct,
  "D", NA,
  "S", NA,
  "B", NA,
  "C", NA,
  "M", NA,
  "F", NA,
  "P", NA,
  "J", NA,
  "A", NA, # Vinstrið
  "G", NA, # Góðan daginn
  "O", NA # Okkar borg
)

if (any(is.na(actual_raw$pct))) {
  stop("Settu inn raunveruleg úrslit (% atkvæða) í `actual_raw` tibble áður en þú keyrir.")
}

actual <- actual_raw |>
  mutate(bokstafur = case_when(
    letter == "A" ~ "V_or_A",
    letter %in% c("G", "O") ~ "Other",
    TRUE ~ letter
  )) |>
  group_by(bokstafur) |>
  summarise(pct = sum(pct), .groups = "drop")

state <- readRDS("fit_state.rds")
parties <- state$parties
draws_pi <- state$draws_pi
party_order <- state$party_order
party_colors <- state$party_colors
vote_share_summary <- state$vote_share_summary

comparison <- vote_share_summary |>
  left_join(actual, by = "bokstafur") |>
  mutate(
    forecast = mean,
    actual = pct / 100,
    diff_pp = (actual - forecast) * 100,
    in_90 = actual >= q05 & actual <= q95
  )

cat("\nSpá vs úrslit:\n")
print(comparison |>
  transmute(
    Flokkur = flokkur,
    `Spáð` = metill::hlutf(forecast, accuracy = 0.1),
    `Úrslit` = metill::hlutf(actual, accuracy = 0.1),
    `Skekkja (pp)` = sprintf("%+.1f", diff_pp),
    `Í 90% bili?` = ifelse(in_90, "✓", "✗")
  ))

mae <- mean(abs(comparison$diff_pp))
cat(sprintf("\nMAE: %.2f prósentustig\n", mae))

p <- draws_pi |>
  mutate(flokkur = factor(flokkur, levels = rev(party_order))) |>
  ggplot(aes(x = pi, y = flokkur)) +
  geom_density_ridges(aes(fill = flokkur), scale = 1.3, alpha = 0.75, color = "white", linewidth = 0.3) +
  geom_point(
    data = comparison |> mutate(flokkur = factor(flokkur, levels = rev(party_order))),
    aes(x = actual, y = flokkur),
    color = "black", fill = "yellow", shape = 21, size = 4
  ) +
  scale_x_continuous(
    labels = metill::label_hlutf(accuracy = 1),
    breaks = seq(0, 0.4, 0.05),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_fill_manual(values = party_colors) +
  labs(
    title = "Reykjavíkurkosningar 2026 — spá vs úrslit",
    subtitle = "Posterior dreifing; gul prik = raunveruleg úrslit",
    x = "Fylgi", y = NULL,
    caption = sprintf(
      "MAE = %.2f pp; %d af %d flokkum í 90%% bili",
      mae, sum(comparison$in_90), nrow(comparison)
    )
  ) +
  metill::theme_metill() +
  theme(legend.position = "none", plot.title.position = "plot")

agg_png("figures/04_spa_vs_urslit.png", width = 1400, height = 900, res = 140)
print(p)
dev.off()

cat("\nWrote figures/04_spa_vs_urslit.png\n")
