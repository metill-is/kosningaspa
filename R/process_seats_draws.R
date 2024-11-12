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


seats_draws |>
  filter(
    flokkur %in% c(
      "Samfylkingin",
      "Viðreisn",
      "Framsóknarflokkurinn",
      "Píratar"
    )
  ) |>
  count(.draw, wt = seats, name = "seats") |>
  count(seats) |>
  mutate(
    prop = n / sum(n),
    col = if_else(seats >= 32, "Meirihluti", "Minnihluti")
  ) |>
  ggplot(aes(seats, prop, fill = col)) +
  geom_col() +
  geom_text(
    data = ~ summarise(
      .x,
      perc = mean(seats >= 32) |> round(2),
      min_seats = min(seats),
      max_y = max(prop),
      label = paste0(perc * 100, "% líkur á meirihluta")
    ),
    aes(x = min_seats, y = 0.8 * max_y, label = label),
    vjust = -1,
    inherit.aes = FALSE,
    hjust = 0,
    size = 5
  ) +
  scale_x_continuous(
    expand = expansion(),
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    expand = expansion(),
    labels = label_percent(),
    limits = c(0, NA),
    guide = guide_axis_truncated()
  ) +
  labs(
    title = "BCPS"
  )


seats_draws |>
  filter(
    flokkur %in% c(
      "Viðreisn",
      "Sjálfstæðisflokkurinn",
      "Miðflokkurinn"
    )
  ) |>
  count(.draw, wt = seats, name = "seats") |>
  count(seats) |>
  mutate(
    prop = n / sum(n),
    col = if_else(seats >= 32, "Meirihluti", "Minnihluti")
  ) |>
  ggplot(aes(seats, prop, fill = col)) +
  geom_col() +
  geom_text(
    data = ~ summarise(
      .x,
      perc = mean(seats >= 32) |> round(2),
      min_seats = min(seats),
      max_y = max(prop),
      label = paste0(perc * 100, "% líkur á meirihluta")
    ),
    aes(x = min_seats, y = 0.8 * max_y, label = label),
    vjust = -1,
    inherit.aes = FALSE,
    hjust = 0,
    size = 5
  ) +
  scale_x_continuous(
    expand = expansion(),
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    expand = expansion(),
    labels = label_percent(),
    limits = c(0, NA),
    guide = guide_axis_truncated()
  ) +
  labs(
    title = "CDM"
  )


seats_draws |>
  filter(
    flokkur %in% c(
      "Flokkur Fólksins",
      "Sjálfstæðisflokkurinn",
      "Miðflokkurinn"
    )
  ) |>
  count(.draw, wt = seats, name = "seats") |>
  count(seats) |>
  mutate(
    prop = n / sum(n),
    col = if_else(seats >= 32, "Meirihluti", "Minnihluti")
  ) |>
  ggplot(aes(seats, prop, fill = col)) +
  geom_col() +
  geom_text(
    data = ~ summarise(
      .x,
      perc = mean(seats >= 32) |> round(2),
      min_seats = min(seats),
      max_y = max(prop),
      label = paste0(perc * 100, "% líkur á meirihluta")
    ),
    aes(x = min_seats, y = 0.8 * max_y, label = label),
    vjust = -1,
    inherit.aes = FALSE,
    hjust = 0,
    size = 5
  ) +
  scale_x_continuous(
    expand = expansion(),
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    expand = expansion(),
    labels = label_percent(),
    limits = c(0, NA),
    guide = guide_axis_truncated()
  ) +
  labs(
    title = "DFM"
  )


seats_draws |>
  filter(
    flokkur %in% c(
      "Viðreisn",
      "Sjálfstæðisflokkurinn",
      "Samfylkingin"
    )
  ) |>
  count(.draw, wt = seats, name = "seats") |>
  count(seats) |>
  mutate(
    prop = n / sum(n),
    col = if_else(seats >= 32, "Meirihluti", "Minnihluti")
  ) |>
  ggplot(aes(seats, prop, fill = col)) +
  geom_col() +
  geom_text(
    data = ~ summarise(
      .x,
      perc = mean(seats >= 32) |> round(1),
      min_seats = min(seats),
      max_y = max(prop),
      label = paste0(perc * 100, "% líkur á meirihluta")
    ),
    aes(x = min_seats, y = 0.8 * max_y, label = label),
    vjust = -1,
    inherit.aes = FALSE,
    hjust = 0,
    size = 5
  ) +
  labs(
    title = "CDS"
  )





seats_draws |>
  summarise(
    seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  pivot_wider(
    names_from = flokkur,
    values_from = seats,
    values_fill = 0
  ) |>
  mutate(
    case = case_when(
      (Píratar > 0) & (`Vinstri Græn` > 0) & (Sósíalistaflokkurinn > 0) ~ "Píratar, Vinstri Græn og Sósíalistaflokkurinn",
      (Píratar > 0) & (`Vinstri Græn` > 0) & (Sósíalistaflokkurinn == 0) ~ "Píratar og Vinstri Græn",
      (Píratar > 0) & (`Vinstri Græn` == 0) & (Sósíalistaflokkurinn > 0) ~ "Píratar og Sósíalistaflokkurinn",
      (Píratar == 0) & (`Vinstri Græn` > 0) & (Sósíalistaflokkurinn > 0) ~ "Vinstri Græn og Sósíalistaflokkurinn",
      (Píratar == 0) & (`Vinstri Græn` > 0) & (Sósíalistaflokkurinn == 0) ~ "Vinstri Græn",
      (Píratar > 0) & (`Vinstri Græn` == 0) & (Sósíalistaflokkurinn == 0) ~ "Píratar",
      (Píratar == 0) & (`Vinstri Græn` == 0) & (Sósíalistaflokkurinn > 0) ~ "Sósíalistaflokkurinn",
      TRUE ~ "Enginn"
    )
  ) |>
  pivot_longer(
    cols = -c(.draw, case),
    names_to = "flokkur",
    values_to = "seats"
  ) |>
  count(case, flokkur, seats) |>
  filter(
    flokkur %in% c("Píratar", "Vinstri Græn", "Sósíalistaflokkurinn"),
    str_detect(case, flokkur)
  ) |>
  mutate(
    p = n / sum(n),
    .by = c(case, flokkur)
  ) |>
  ggplot(aes(seats, p, fill = flokkur)) +
  geom_col() +
  scale_x_continuous(
    expand = expansion(),
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    expand = expansion(),
    labels = label_percent(),
    limits = c(0, NA),
    guide = guide_axis_truncated()
  ) +
  facet_wrap(vars(flokkur, case), scales = "free_x")


seats_draws |>
  summarise(
    seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  pivot_wider(
    names_from = flokkur,
    values_from = seats,
    values_fill = 0
  ) |>
  mutate(
    n_small_parties = (Píratar > 0) + (`Vinstri Græn` > 0) + (Sósíalistaflokkurinn > 0)
  ) |>
  pivot_longer(
    cols = -c(.draw, n_small_parties),
    names_to = "flokkur",
    values_to = "seats"
  ) |>
  count(n_small_parties, flokkur, seats) |>
  filter(
    flokkur %in% c("Píratar", "Vinstri Græn", "Sósíalistaflokkurinn"),
    seats > 0
  ) |>
  mutate(
    p = n / sum(n),
    .by = c(n_small_parties, flokkur)
  ) |>
  ggplot(aes(seats, p, fill = flokkur)) +
  geom_col() +
  scale_x_continuous(
    expand = expansion(),
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    expand = expansion(),
    labels = label_percent(),
    limits = c(0, NA),
    guide = guide_axis_truncated()
  ) +
  facet_wrap(vars(flokkur, n_small_parties), scales = "free_x")


seats_draws |>
  filter(
    flokkur %in% c(
      "Píratar",
      "Vinstri Græn",
      "Sósíalistaflokkurinn"
    )
  ) |>
  summarise(
    seats = sum(seats),
    kjordaemi_seats = sum(kjordaemi_seats),
    jofnunar_seats = seats - kjordaemi_seats,
    .by = c(.draw, flokkur)
  ) |>
  mutate(
    n_small_parties = sum(seats > 0),
    .by = .draw
  ) |>
  pivot_longer(
    cols = c(contains("seats")),
    names_to = "type",
    values_to = "seats"
  ) |>
  count(n_small_parties, flokkur, type, seats) |>
  mutate(
    p = n / sum(n),
    .by = c(n_small_parties, flokkur, type)
  ) |>
  filter(n_small_parties > 0) |>
  mutate(
    n_small_parties = factor(n_small_parties)
  ) |>
  ggplot(aes(seats, p, fill = n_small_parties, group = n_small_parties)) +
  geom_col(position = "dodge") +
  scale_x_continuous(
    expand = expansion(),
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    expand = expansion(),
    labels = label_percent(),
    limits = c(0, NA),
    guide = guide_axis_truncated()
  ) +
  scale_fill_brewer(
    palette = "Set1"
  ) +
  facet_wrap(vars(flokkur, type), scales = "free_x")
