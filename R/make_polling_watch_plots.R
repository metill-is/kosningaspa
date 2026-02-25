library(tidyverse)
library(metill)
library(patchwork)
library(here)
library(arrow)
library(glue)
library(ggtext)
library(gt)
library(scales)
library(ggrepel)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

theme_set(theme_metill(type = "blog"))

box::use(
  R / data[read_polling_data],
  R / party_utils[party_tibble]
)

format_date <- function(date) {
  clock::date_format(date, format = "%d. %B", locale = clock::clock_locale("is")) |>
    str_replace("^0", "")
}

caption <- str_c(
  "Mynd frá kannanavaktinni á www.metill.is", "\n",
  "Unnið af Brynjólfi Gauta Guðrúnar Jónssyni, ásamt Agnari Frey Helgasyni, Hafsteini Einarssyni og Rafael Daniel Vias", "\n",
  "Frekari greiningar og útskýringar á aðferðafræði má nálgast á www.metill.is"
)

colors <- party_tibble() |>
  select(flokkur, litur) |>
  mutate(flokkur = str_to_sentence(flokkur))

# Load draws
output_date <- as.character(Sys.Date())
draws_path <- here("data", output_date, "polling_watch_draws.parquet")
if (!file.exists(draws_path)) {
  # Fall back to most recent available
  available <- list.dirs(here("data"), recursive = FALSE, full.names = FALSE)
  available <- sort(available[str_detect(available, "^\\d{4}-\\d{2}-\\d{2}$")], decreasing = TRUE)
  for (d in available) {
    p <- here("data", d, "polling_watch_draws.parquet")
    if (file.exists(p)) {
      draws_path <- p
      output_date <- d
      break
    }
  }
}

pi_draws <- read_parquet(draws_path)

# Load raw polling data for overlay (combine pre- and post-election)
pre_election <- read_polling_data() |>
  filter(date >= clock::date_build(2021, 9, 25))

post_election_path <- here("data", "post_election_polls.csv")
if (file.exists(post_election_path)) {
  post_election <- read_csv(post_election_path, show_col_types = FALSE)
} else {
  post_election <- tibble()
}

polling_data <- bind_rows(pre_election, post_election) |>
  filter(fyrirtaeki != "Kosning") |>
  mutate(
    p = n / sum(n),
    .by = c(date, fyrirtaeki)
  )


#### Plot 1: Time Series ####

plot_summary <- pi_draws |>
  reframe(
    median = median(value),
    coverage = c(0.5, 0.8, 0.95),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = c(dags, flokkur)
  )

# Get last date median for label ordering
label_data <- plot_summary |>
  filter(
    dags == max(dags),
    coverage == 0.5
  ) |>
  mutate(flokkur = str_to_sentence(flokkur)) |>
  inner_join(colors, by = "flokkur")

p_timeseries <- plot_summary |>
  mutate(flokkur = str_to_sentence(flokkur)) |>
  inner_join(colors, by = "flokkur") |>
  ggplot(aes(dags, group = paste(flokkur, coverage))) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = litur, alpha = -coverage)
  ) +
  geom_line(
    data = ~ filter(.x, coverage == 0.5),
    aes(y = median, col = litur),
    linewidth = 0.8
  ) +
  geom_point(
    data = polling_data |>
      mutate(flokkur = str_to_sentence(flokkur)) |>
      inner_join(colors, by = "flokkur"),
    aes(x = date, y = p, col = litur),
    size = 1.5,
    alpha = 0.4,
    inherit.aes = FALSE
  ) +
  geom_text_repel(
    data = label_data,
    aes(x = dags, y = median, label = flokkur, col = litur),
    hjust = 0,
    size = 3.5,
    fontface = "bold",
    direction = "y",
    nudge_x = 5,
    segment.size = 0.3,
    segment.alpha = 0.5,
    xlim = c(max(plot_summary$dags) + 3, NA),
    inherit.aes = FALSE
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_continuous(range = c(0.05, 0.3)) +
  scale_y_continuous(
    labels = label_percent(),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion(mult = c(0.02, 0.15))
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Kannanavaktin",
    subtitle = glue("Þróun fylgis frá {format_date(min(pi_draws$dags))} til {format_date(max(pi_draws$dags))}"),
    caption = caption
  )

ggsave(
  plot = p_timeseries,
  filename = here("Figures", "polling_watch_timeseries.png"),
  width = 8,
  height = 0.621 * 8,
  scale = 1.4,
  bg = "#fdfcfc"
)


#### Plot 1b: Post-election Time Series (zoomed in) ####

election_2024 <- as.Date("2024-11-30")

post_election_summary <- plot_summary |>
  filter(dags >= election_2024)

post_election_polls <- polling_data |>
  filter(date >= election_2024)

post_label_data <- post_election_summary |>
  filter(
    dags == max(dags),
    coverage == 0.5
  ) |>
  mutate(flokkur = str_to_sentence(flokkur)) |>
  inner_join(colors, by = "flokkur")

p_post_election <- post_election_summary |>
  mutate(flokkur = str_to_sentence(flokkur)) |>
  inner_join(colors, by = "flokkur") |>
  ggplot(aes(dags, group = paste(flokkur, coverage))) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = litur, alpha = -coverage)
  ) +
  geom_line(
    data = ~ filter(.x, coverage == 0.5),
    aes(y = median, col = litur),
    linewidth = 0.8
  ) +
  geom_point(
    data = post_election_polls |>
      mutate(flokkur = str_to_sentence(flokkur)) |>
      inner_join(colors, by = "flokkur"),
    aes(x = date, y = p, col = litur),
    size = 2,
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text_repel(
    data = post_label_data,
    aes(x = dags, y = median, label = flokkur, col = litur),
    hjust = 0,
    size = 3.5,
    fontface = "bold",
    direction = "y",
    nudge_x = 5,
    segment.size = 0.3,
    segment.alpha = 0.5,
    xlim = c(max(post_election_summary$dags) + 3, NA),
    inherit.aes = FALSE
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_continuous(range = c(0.05, 0.3)) +
  scale_y_continuous(
    labels = label_percent(),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion(mult = c(0.02, 0.15))
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Kannanavaktin",
    subtitle = glue("Þróun fylgis frá kosningum {format_date(election_2024)} til {format_date(max(pi_draws$dags))}"),
    caption = caption
  )

ggsave(
  plot = p_post_election,
  filename = here("Figures", "polling_watch_post_election.png"),
  width = 8,
  height = 0.621 * 8,
  scale = 1.4,
  bg = "#fdfcfc"
)


#### Plot 2: Snapshot ####

latest_date <- max(pi_draws$dags)

d_snapshot <- pi_draws |>
  filter(dags == latest_date) |>
  mutate(flokkur = str_to_sentence(flokkur)) |>
  reframe(
    median = median(value),
    coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = flokkur
  )

p_snapshot <- d_snapshot |>
  inner_join(colors, by = "flokkur") |>
  mutate(
    flokkur_ordered = glue("<b style='color:{litur}'>{flokkur}</b>"),
    flokkur_ordered = fct_reorder(flokkur_ordered, median)
  ) |>
  ggplot(aes(
    y = flokkur_ordered,
    color = litur,
    group = paste(flokkur, coverage)
  )) +
  annotate(
    geom = "segment",
    x = seq(0.1, 0.3, 0.05),
    xend = seq(0.1, 0.3, 0.05),
    y = 0.5,
    yend = length(unique(d_snapshot$flokkur)) + 0.255,
    alpha = 0.2,
    linewidth = 0.2
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      alpha = -coverage
    ),
    linewidth = 5
  ) +
  geom_segment(
    aes(
      x = median,
      xend = median,
      y = as.integer(flokkur_ordered) - 0.4,
      yend = as.integer(flokkur_ordered) + 0.4
    ),
    linewidth = 1
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = lower,
      y = as.integer(flokkur_ordered) - 0.15,
      yend = as.integer(flokkur_ordered) + 0.15,
      alpha = -coverage
    ),
    linewidth = 0.2
  ) +
  geom_segment(
    aes(
      x = upper,
      xend = upper,
      y = as.integer(flokkur_ordered) - 0.15,
      yend = as.integer(flokkur_ordered) + 0.15,
      alpha = -coverage
    ),
    linewidth = 0.2
  ) +
  geom_vline(
    xintercept = 0.05,
    linetype = "dashed",
    alpha = 0.4
  ) +
  scale_color_identity() +
  scale_x_continuous(
    breaks = c(0.05, seq(0, 0.3, 0.05)),
    labels = label_percent(),
    limits = c(0, 0.3),
    guide = ggh4x::guide_axis_truncated(
      trunc_lower = 0,
      trunc_upper = 0.3
    ),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion()
  ) +
  scale_alpha_continuous(
    range = c(0, 0.3)
  ) +
  coord_cartesian(
    xlim = c(0, 0.3),
    ylim = c(0.5, length(unique(d_snapshot$flokkur)) + 0.5),
    clip = "off"
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(size = 18),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  labs(
    x = NULL,
    y = NULL
  )

table <- d_snapshot |>
  filter(coverage == 0.9) |>
  select(flokkur, median, lower, upper) |>
  arrange(desc(median)) |>
  gt(process_md = TRUE) |>
  fmt_percent(
    columns = median:upper,
    decimals = 1,
    drop_trailing_zeros = TRUE,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  cols_label(
    flokkur = "",
    median = "Miðgildi",
    lower = "Neðri",
    upper = "Efri"
  ) |>
  cols_align("center") |>
  cols_hide(flokkur) |>
  tab_spanner(
    label = md("90% Óvissubil"),
    columns = lower:upper
  ) |>
  tab_options(
    table.background.color = "transparent",
    column_labels.hidden = FALSE,
    table.border.top.style = "0px",
    table.border.bottom.style = "0px",
    table.font.size = px(20),
    table_body.border.bottom.style = "0px",
    table_body.border.top.style = "0px"
  ) |>
  opt_table_font(
    font = google_font("Lato"),
    weight = "bold"
  ) |>
  opt_horizontal_padding(0) |>
  opt_vertical_padding(1.6)

for (row in seq_len(nrow(colors))) {
  table <- table |>
    tab_style(
      style = cell_text(
        color = colors$litur[row],
        weight = "bold",
        font = google_font("Lato")
      ),
      locations = cells_body(
        rows = flokkur == colors$flokkur[row]
      )
    )
}

p_tab <- p_snapshot + wrap_table(table, space = "fixed") +
  plot_annotation(
    title = "Kannanavaktin",
    subtitle = glue("Miðja síðustu kannanar var {format_date(latest_date)}"),
    caption = caption,
    theme = theme(
      plot.title = element_text(
        size = 30,
        margin = margin(5, 5, 5, 5)
      ),
      plot.subtitle = element_text(
        size = 20,
        margin = margin(0, 5, -20, 5)
      )
    )
  )

ggsave(
  plot = p_tab,
  filename = here("Figures", "polling_watch_snapshot.png"),
  width = 8,
  height = 0.55 * 8,
  scale = 1.4,
  bg = "#fdfcfc"
)
