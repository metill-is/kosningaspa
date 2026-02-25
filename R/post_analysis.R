library(tidyverse)
library(metill)
library(gt)
library(gtExtras)
library(patchwork)
library(ggh4x)
library(arrow)
library(here)
library(clock)
theme_set(theme_metill(type = "blog"))

box::use(
  R / data[get_sheet_data_raw]
)

gallup <- get_sheet_data_raw("gallup")
prosent <- get_sheet_data_raw("prosent")
maskina <- get_sheet_data_raw("maskina")
felo <- get_sheet_data_raw("felagsvisindastofnun")
kosningaspa <- get_sheet_data_raw("kosningaspa")
metill <- read_parquet(
  here("data", "2024-11-30", "y_rep_draws_constituency.parquet")
) |>
  filter(
    dags == max(dags)
  ) |>
  summarise(
    p = median(value) |> round(digits = 3),
    .by = c(flokkur, dags)
  ) |> 
  mutate(
    fyrirtaeki = "Metill",
    date = dags
  ) |> 
  select(date, fyrirtaeki, flokkur, p) |> 
  mutate(
    flokkur = if_else(
      flokkur == "Annað",
      "Lýðræðisflokkurinn",
      flokkur
    )
  )

kosning <- tribble(
  ~flokkur, ~p_true,
  "Samfylkingin", 0.208,
  "Sjálfstæðisflokkurinn", 0.194,
  "Viðreisn", 0.158,
  "Flokkur Fólksins", 0.138,
  "Miðflokkurinn", 0.121,
  "Framsóknarflokkurinn", 0.078,
  "Sósíalistaflokkurinn", 0.04,
  "Píratar", 0.03,
  "Vinstri Græn", 0.023,
  "Lýðræðisflokkurinn", 0.01
)


#### Only Last Prediction ####
d <- bind_rows(
  gallup, 
  prosent,
  maskina,
  kosningaspa,
  felo
) |> 
  mutate(
    flokkur = if_else(
      flokkur == "Annað",
      "Lýðræðisflokkurinn",
      flokkur
    )
  ) |> 
  filter(
    date == max(date),
    .by = fyrirtaeki
  ) |> 
  mutate(
    date = as_date(lokadagur)
  ) |> 
  select(
    date, fyrirtaeki, flokkur, p
  ) |> 
  bind_rows(
    metill
  ) |> 
  inner_join(
    kosning
  ) |> 
  mutate(
    flokkur = str_to_sentence(flokkur)
  )


d |> 
  mutate(
    err = p - p_true
  ) |> 
  summarise(
    mae = mean(abs(err)),
    .by = fyrirtaeki
  ) |> 
  arrange(mae)

p <- d |> 
  mutate(
    err = p - p_true
  ) |> 
  summarise(
    mae = mean(abs(err)),
    .by = fyrirtaeki
  ) |> 
  arrange(mae) |> 
  mutate(
    fyrirtaeki = fct_reorder(fyrirtaeki, mae)
  ) |> 
  ggplot(aes(mae, fyrirtaeki)) +
  annotate(
    geom = "segment",
    y = 0,
    yend = 6,
    x = seq(0.005, 0.02, by = 0.005),
    xend = seq(0.005, 0.02, by = 0.005),
    linewidth = 0.1,
    alpha = 0.4,
    lty = 5
  ) +
  geom_segment(
    aes(
      yend = fyrirtaeki,
      xend = 0
    ),
    linewidth = 0.4
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_continuous(
    guide = guide_axis_truncated(),
    labels = label_percent(
      suffix = "%-stig",
      decimal.mark = ",",
      big.mark = "."
    ),
    limits = c(0, 0.02),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_discrete(
    guide = guide_axis_truncated()
  ) +
  coord_cartesian(
    ylim = c(1, 6)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Meðalfrávik í síðustu könnunum/spám"
  )

ggsave(
  plot = p,
  filename = "Figures/post_analysis/final_mae_transparent.png",
  width = 8, height = 0.4 * 8,
  scale = 1
)

ggsave(
  plot = p,
  filename = "Figures/post_analysis/final_mae.png",
  width = 8, height = 0.4 * 8,
  scale = 1,
  bg = "#fdfcfc"
)

p <- d |> 
  mutate(
    err = p - p_true,
    fyrirtaeki = if_else(
      fyrirtaeki == "Félagsvísindastofnun",
      "Félagsv.",
      fyrirtaeki
    ),
    group = fyrirtaeki
  ) |> 
  mutate(
    group = fct_reorder(group, err, .fun = \(x) mean(abs(x))),
    dir = factor(1 * (err > 0))
  ) |> 
  ggplot(aes(err, group, col = dir)) +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  geom_segment(
    aes(
      yend = group,
      xend = 0
    ),
    linewidth = 0.4
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_continuous(
    guide = guide_axis_truncated(),
    labels = label_percent(
      suffix = "%-stig",
      decimal.mark = ",",
      big.mark = "."
    )
  ) +
  scale_y_discrete(
    guide = guide_axis_truncated(),
    labels = function(x) {
      str_replace(x, "_.*", "")
    }
  ) +
  scale_colour_manual(
    values = c(
      "#e41a1c",
      "#4daf4a"
    ),
    guide = "none"
  ) +
  facet_wrap(
    vars(flokkur),
    scales = "free_y",
    ncol = 5
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15),
    axis.text.x = element_text(size = 8)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Frávik eftir flokkum"
  )


ggsave(
  plot = p,
  filename = "Figures/post_analysis/fravik_flokkar_transparent.png",
  width = 8, 
  height = 0.4 * 8,
  scale = 1.7
)

ggsave(
  plot = p,
  filename = "Figures/post_analysis/fravik_flokkar.png",
  width = 8, 
  height = 0.4 * 8,
  scale = 1.7,
  bg = "#fdfcfc"
)



p <- d |> 
  mutate(
    err = p - p_true,
    fyrirtaeki = if_else(
      fyrirtaeki == "Félagsvísindastofnun",
      "Félagsv.",
      fyrirtaeki
    ),
    flokkur = fct_reorder(flokkur, -p_true)
  ) |> 
  mutate(
    fyrirtaeki = fct_reorder(fyrirtaeki, err, .fun = \(x) mean(abs(unique(x)))),
    dir = factor(1 * (err > 0))
  ) |> 
  ggplot(aes(err, fyrirtaeki, col = dir)) +
  annotate(
    geom = "segment",
    y = 0,
    yend = 6,
    x = c(-0.05, -0.025, 0.025),
    xend = c(-0.05, -0.025, 0.025),
    linewidth = 0.1,
    alpha = 0.4,
    lty = 5
  ) +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  geom_segment(
    aes(
      yend = fyrirtaeki,
      xend = 0
    ),
    linewidth = 0.4
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_continuous(
    guide = guide_axis_truncated(),
    labels = label_percent(
      suffix = "%-stig",
      decimal.mark = ",",
      big.mark = "."
    )
  ) +
  scale_y_discrete(
    guide = guide_axis_truncated(),
    labels = function(x) {
      str_replace(x, "_.*", "")
    }
  ) +
  scale_colour_manual(
    values = c(
      "#e41a1c",
      "#4daf4a"
    ),
    guide = "none"
  ) +
  facet_wrap(
    vars(flokkur),
    scales = "free_y",
    ncol = 5
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15),
    axis.text.x = element_text(size = 8)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Frávik eftir flokkum"
  )



ggsave(
  plot = p,
  filename = "Figures/post_analysis/fravik_flokkar2_transparent.png",
  width = 8, 
  height = 0.4 * 8,
  scale = 1.8
)

ggsave(
  plot = p,
  filename = "Figures/post_analysis/fravik_flokkar2.png",
  width = 8, 
  height = 0.4* 8,
  scale = 1.8,
  bg = "#fdfcfc"
)




d |> 
  mutate(
  err = abs(p - p_true)
) |> 
group_by(
  flokkur2 = flokkur
) |> 
group_map(
  function(x, ...) {
    x |> 
      gt() |> 
      tab_header(
        title = unique(x$flokkur)
      ) |> 
      cols_hide(c(flokkur, date)) |> 
      fmt_percent(decimals = 1) |> 
      gtExtras::gt_color_rows(
        err,
        palette = "Greys",
        domain = c(0, 0.05)
      ) |> 
      wrap_table()
  }
) |> 
wrap_plots()


d

#### Time Series ####

metill <- tribble(
  ~date, ~flokkur, ~p,
  date_build(2024, 11, 7), "Samfylkingin", 0.18,
  date_build(2024, 11, 7), "Sjálfstæðisflokkurinn", 0.17,
  date_build(2024, 11, 7), "Viðreisn", 0.14,
  date_build(2024, 11, 7), "Flokkur Fólksins", 0.12,
  date_build(2024, 11, 7), "Miðflokkurinn", 0.15,
  date_build(2024, 11, 7), "Framsóknarflokkurinn", 0.09,
  date_build(2024, 11, 7), "Sósíalistaflokkurinn", 0.03,
  date_build(2024, 11, 7), "Píratar", 0.05,
  date_build(2024, 11, 7), "Vinstri Græn", 0.04,
  date_build(2024, 11, 7), "Lýðræðisflokkurinn", 0.01,
  date_build(2024, 11, 8), "Samfylkingin", 0.19,
  date_build(2024, 11, 8), "Sjálfstæðisflokkurinn", 0.17,
  date_build(2024, 11, 8), "Viðreisn", 0.15,
  date_build(2024, 11, 8), "Flokkur Fólksins", 0.12,
  date_build(2024, 11, 8), "Miðflokkurinn", 0.15,
  date_build(2024, 11, 8), "Framsóknarflokkurinn", 0.09,
  date_build(2024, 11, 8), "Sósíalistaflokkurinn", 0.03,
  date_build(2024, 11, 8), "Píratar", 0.05,
  date_build(2024, 11, 8), "Vinstri Græn", 0.04,
  date_build(2024, 11, 8), "Lýðræðisflokkurinn", 0.01,
  date_build(2024, 11, 16), "Samfylkingin", 0.17,
  date_build(2024, 11, 16), "Sjálfstæðisflokkurinn", 0.17,
  date_build(2024, 11, 16), "Viðreisn", 0.16,
  date_build(2024, 11, 16), "Flokkur Fólksins", 0.12,
  date_build(2024, 11, 16), "Miðflokkurinn", 0.14,
  date_build(2024, 11, 16), "Framsóknarflokkurinn", 0.09,
  date_build(2024, 11, 16), "Sósíalistaflokkurinn", 0.04,
  date_build(2024, 11, 16), "Píratar", 0.04,
  date_build(2024, 11, 16), "Vinstri Græn", 0.03,
  date_build(2024, 11, 16), "Lýðræðisflokkurinn", 0.01,
  date_build(2024, 11, 23), "Samfylkingin", 0.17,
  date_build(2024, 11, 23), "Sjálfstæðisflokkurinn", 0.17,
  date_build(2024, 11, 23), "Viðreisn", 0.17,
  date_build(2024, 11, 23), "Flokkur Fólksins", 0.13,
  date_build(2024, 11, 23), "Miðflokkurinn", 0.12,
  date_build(2024, 11, 23), "Framsóknarflokkurinn", 0.08,
  date_build(2024, 11, 23), "Sósíalistaflokkurinn", 0.04,
  date_build(2024, 11, 23), "Píratar", 0.04,
  date_build(2024, 11, 23), "Vinstri Græn", 0.03,
  date_build(2024, 11, 23), "Lýðræðisflokkurinn", 0.01,
  date_build(2024, 11, 28), "Samfylkingin", 0.18,
  date_build(2024, 11, 28), "Sjálfstæðisflokkurinn", 0.18,
  date_build(2024, 11, 28), "Viðreisn", 0.16,
  date_build(2024, 11, 28), "Flokkur Fólksins", 0.14,
  date_build(2024, 11, 28), "Miðflokkurinn", 0.12,
  date_build(2024, 11, 28), "Framsóknarflokkurinn", 0.09,
  date_build(2024, 11, 28), "Sósíalistaflokkurinn", 0.04,
  date_build(2024, 11, 28), "Píratar", 0.05,
  date_build(2024, 11, 28), "Vinstri Græn", 0.04,
  date_build(2024, 11, 28), "Lýðræðisflokkurinn", 0.01,
  date_build(2024, 11, 29), "Samfylkingin", 0.184,
  date_build(2024, 11, 29), "Sjálfstæðisflokkurinn", 0.193,
  date_build(2024, 11, 29), "Viðreisn", 0.149,
  date_build(2024, 11, 29), "Flokkur Fólksins", 0.132,
  date_build(2024, 11, 29), "Miðflokkurinn", 0.11,
  date_build(2024, 11, 29), "Framsóknarflokkurinn", 0.091,
  date_build(2024, 11, 29), "Sósíalistaflokkurinn", 0.042,
  date_build(2024, 11, 29), "Píratar", 0.047,
  date_build(2024, 11, 29), "Vinstri Græn", 0.036,
  date_build(2024, 11, 29), "Lýðræðisflokkurinn", 0.011
) |> 
  mutate(fyrirtaeki = "Metill")

p <- bind_rows(
  gallup, 
  prosent,
  maskina,
  felo,
  kosningaspa
) |> 
  mutate(
    flokkur = if_else(
      flokkur == "Annað",
      "Lýðræðisflokkurinn",
      flokkur
    )
  ) |> 
  mutate(
    date = as_date(lokadagur)
  ) |> 
  select(
    date, fyrirtaeki, flokkur, p
  ) |> 
  filter(
    date >= date_build(2024, 10, 25)
  ) |> 
  bind_rows(
    metill
  ) |> 
  inner_join(
    kosning
  ) |> 
  mutate(
    err = p - p_true
  ) |> 
  summarise(
    mae = mean(abs(err)),
    .by = c(date, fyrirtaeki)
  ) |> 
  ggplot(aes(date, mae, col = fyrirtaeki, shape = fyrirtaeki)) +
  geom_hline(
    yintercept = c(0.01, 0.02, 0.03, 0.04),
    lty = 5,
    linewidth = 0.2,
    alpha = 0.15
  ) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_line(
    data = ~ filter(.x, fyrirtaeki != "Félagsvísindastofnun"),
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_date(
    breaks = date_build(2024, 11, c(1, 8, 15, 22, 30)),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = breaks_width(0.01),
    labels = label_percent(suffix = "%-stig", accuracy = 1),
    guide = guide_axis_truncated(),
    limits = c(0, 0.04)
  ) +
  scale_colour_manual(
    values = c(
      Félagsvísindastofnun = "#10099F",
      Gallup = "#e41a1c",
      Kosningaspa.is = "black",
      Maskína = "#4daf4a",
      Metill = "#984ea3",
      Prósent = "#377eb8"
    )
  ) +
  scale_shape_manual(
    values = c(
      Félagsvísindastofnun = 17,
      Gallup = 21,
      Kosningaspa.is = 25,
      Maskína = 22,
      Metill = 16,
      Prósent = 23
    )
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = c(0.5, 0.85),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = NA)
  ) +
  labs(
    x = NULL,
    y = NULL,
    shape = NULL,
    col = NULL,
    title = "Meðalfrávik eftir dagsetningu birtingar"
  ) 

ggsave(
  plot = p,
  filename = "Figures/post_analysis/time_series_transparent.png",
  width = 8, 
  height = 0.621 * 8,
  scale = 1
)

ggsave(
  plot = p,
  filename = "Figures/post_analysis/time_series.png",
  width = 8, 
  height = 0.621 * 8,
  scale = 1,
  bg = "#fdfcfc"
)




bind_rows(
  gallup, 
  prosent,
  maskina,
  felo,
  kosningaspa
) |> 
  mutate(
    flokkur = if_else(
      flokkur == "Annað",
      "Lýðræðisflokkurinn",
      flokkur
    )
  ) |> 
  mutate(
    date = as_date(lokadagur)
  ) |> 
  select(
    date, fyrirtaeki, flokkur, p
  ) |> 
  filter(
    date >= date_build(2024, 10, 25)
  ) |> 
  bind_rows(
    metill
  ) |> 
  inner_join(
    kosning
  ) |> 
  mutate(
    err = p - p_true
  ) |> 
  summarise(
    mae = mean(abs(err)),
    .by = c(date, fyrirtaeki)
  ) |> 
  fill(date, fyrirtaeki) |> 
  pivot_wider(names_from = fyrirtaeki, values_from = mae) |> 
  arrange(date)


bind_rows(
  gallup, 
  prosent,
  maskina,
  felo,
  kosningaspa
) |> 
  mutate(
    flokkur = if_else(
      flokkur == "Annað",
      "Lýðræðisflokkurinn",
      flokkur
    )
  ) |> 
  mutate(
    date = as_date(lokadagur)
  ) |> 
  select(
    date, fyrirtaeki, flokkur, p
  ) |> 
  filter(
    date >= date_build(2024, 10, 25)
  ) |> 
  bind_rows(
    metill
  ) |> 
  inner_join(
    kosning
  ) |> 
  mutate(
    err = p - p_true
  ) |> 
  summarise(
    mae = mean(abs(err)),
    .by = c(date, fyrirtaeki)
  ) |> 
  mutate(
    fyrirtaeki = if_else(
      fyrirtaeki %in% c("Metill", "Kosningaspa.is"),
      fyrirtaeki,
      "Könnunaraðilar"
    )
  ) |> 
  complete(date, fyrirtaeki) |> 
  arrange(date) |> 
  group_by(fyrirtaeki) |> 
  fill(mae, .direction = "down") |> 
  ungroup() |> 
  summarise(
    mae = mean(mae),
    .by = c(date, fyrirtaeki)
  ) |> 
  pivot_wider(names_from = fyrirtaeki, values_from = mae) |> 
  arrange(date)
