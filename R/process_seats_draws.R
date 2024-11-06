library(tidyverse)
library(arrow)
library(here)
library(metill)
library(ggh4x)
library(scales)
library(gt)
library(gtExtras)
library(patchwork)
theme_set(theme_metill())
colors <- tribble(
  ~flokkur, ~litur,
  "Sjálfstæðisflokkurinn", "#377eb8",
  "Framsóknarflokkurinn", "#41ab5d",
  "Samfylkingin", "#e41a1c",
  "Vinstri Græn", "#00441b",
  "Viðreisn", "#ff7d14",
  "Píratar", "#984ea3",
  "Miðflokkurinn", "#08306b",
  "Flokkur Fólksins", "#FBB829",
  "Sósíalistaflokkurinn", "#67000d",
  "Annað", "grey50"
)

seats_draws <- read_parquet(here("data", today(), "seats_draws.parquet"))

seats_draws |>
  summarise(
    seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  count(seats, flokkur) |>
  mutate(
    prop = n / sum(n),
    .by = flokkur
  ) |>
  left_join(colors, by = "flokkur") |>
  mutate(
    flokkur2 = flokkur
  ) |>
  group_by(flokkur2) |>
  group_map(
    ~ .x |>
      ggplot(aes(seats, prop, fill = litur)) +
      geom_col() +
      scale_x_continuous(
        expand = expansion(),
        breaks = breaks_width(1),
        guide = guide_axis_truncated()
      ) +
      scale_y_continuous(
        expand = expansion(),
        labels = label_percent(),
        limits = c(0, 1),
        guide = guide_axis_truncated()
      ) +
      facet_wrap(~flokkur) +
      scale_fill_identity() +
      labs(
        x = NULL,
        y = NULL
      )
  ) |>
  wrap_plots() +
  plot_annotation(
    title = "Eftirádreifing yfir fjölda sæta"
  )


seats_draws |>
  crossing(
    n_seats = 1:6
  ) |>
  summarise(
    prop = mean(seats >= n_seats),
    .by = c(flokkur, kjordaemi, n_seats)
  ) |>
  filter(
    (prop >= 0.005) | (n_seats == 1)
  ) |>
  arrange(n_seats, kjordaemi) |>
  pivot_wider(names_from = n_seats, values_from = prop) |>
  group_by(flokkur) |>
  gt() |>
  cols_label(
    kjordaemi = ""
  ) |>
  sub_missing(
    columns = -c(kjordaemi, flokkur),
    missing_text = ""
  ) |>
  fmt_percent(
    columns = -c(kjordaemi, flokkur),
    decimals = 0
  ) |>
  tab_spanner(
    label = "Fjöldi sæta",
    columns = -c(kjordaemi, flokkur)
  ) |>
  tab_style(
    location = cells_row_groups(),
    style = cell_text(
      weight = "bold",
      size = px(20)
    )
  ) |>
  gtsave(here("Figures", "seats_by_n_seats.png"))
