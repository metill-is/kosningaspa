library(tidyverse)
library(ggh4x)
library(scales)
library(metill)
library(here)
theme_set(theme_metill(type = "blog"))

current_poll <- here("data", "poll_predictions", "prosent8nov.csv")

p <- read_csv(current_poll) |>
  mutate(
    flokkur = str_to_sentence(flokkur) |>
      fct_reorder(true)
  ) |>
  ggplot(aes(true, flokkur)) +
  geom_linerange(aes(xmin = q5, xmax = q95, col = "Spá")) +
  geom_point(
    aes(x = mean, col = "Spá", shape = "Spá"),
    size = 4
  ) +
  geom_point(
    aes(col = "Rétt gildi", shape = "Rétt gildi"),
    size = 4
  ) +
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_colour_manual(
    values = c("Rétt gildi" = "black", "Spá" = "gray70")
  ) +
  scale_shape_manual(
    values = c("Rétt gildi" = 16, "Spá" = 15)
  ) +
  theme(
    legend.position = c(0.12, 0.87),
    legend.box.background = element_rect(linewidth = 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Spá um niðurstöður könnunar Prósents 8. nóvember",
    subtitle = str_c(
      "Gráir kassar eru miðgildi og línur eru 90% óvissubil fyrir spár | ",
      "Svartir punktar eru rétt gildi"
    ),
    shape = NULL,
    col = NULL
  )

p


ggsave(
  here("Figures", "poll_predictions", "prosent8nov_background.png"),
  width = 8,
  height = 0.5 * 8,
  scale = 1.4,
  bg = "#fdfcfc"
)

ggsave(
  here("Figures", "poll_predictions", "prosent8nov_transparent.png"),
  width = 8,
  height = 0.5 * 8,
  scale = 1.4
)
