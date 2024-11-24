library(ggridges)
fit_date <- clock::date_build(2024, 11, 18)

caption <- str_c(
  "¹Líkur á að flokkur hljóti meiri en 5% atkvæða og eigi þannig rétt á jöfnunarþingsætum", "\n",
  "Nánast öruggt: >99%, Mjög líklegt: 80-99%, Líklegra en ekki: 60-80%, Helmingslíkur: 40-60%, Nokkrar líkur: 20-40%, Ólíklegt: <20%", "\n",
  "Unnið af Brynjólfi Gauta Guðrúnar Jónssyni, ásamt Agnari Frey Helgasyni, Hafsteini Einarssyni og Rafael Daniel Vias", "\n",
  "Frekari greiningar og útskýringar á aðferðafræði má nálgast á www.metill.is"
)

d <- read_parquet(
  here("data", "2024-11-23", "seats_draws.parquet")
)

read_parquet(
  here("data", "2024-11-23", "y_rep_draws_constituency.parquet")
) |>
  filter(dags == max(dags)) |> 
  filter(
    value == max(value),
    .by = .draw
  ) |> 
  count(flokkur) |> 
  mutate(p = n / sum(n)) |> 
  arrange(desc(p))

support <- read_parquet(
  here("data", "2024-11-23", "y_rep_draws_constituency.parquet")
) |>
  filter(dags == max(dags)) |> 
  summarise(
    p_in = mean(value > 0.05),
    .by = flokkur
  ) |> 
  mutate(
    flokkur = if_else(
      flokkur == "Annað",
      "Lýðræðisflokkurinn",
      flokkur) |> 
      str_to_sentence()
  )

plot_dat <- d |> 
  count(.draw, flokkur, wt = seats, name = "seats") |> 
  count(flokkur, seats) |> 
  mutate(
    p = n / sum(n),
    .by = flokkur
  ) |> 
  mutate(
    flokkur = if_else(
      flokkur == "Annað",
      "Lýðræðisflokkurinn",
      flokkur) |> 
      str_to_sentence()
  ) |> 
  inner_join(
    colors,
    by = "flokkur"
  ) |> 
  mutate(
    flokkur_ordered = glue("<b style='color:{litur}'>{flokkur}</b>"),
    flokkur_ordered = fct_reorder(flokkur_ordered, seats * p, sum)
  )

#### Sharing ####

p <- plot_dat |> 
  filter(p > 0.005) |> 
  mutate(
    p = if_else(
      seats == 0, 
      p * 0.75, 
      p
    ),
    p = if_else(
      (seats == 0) & (flokkur == "Vinstri græn"),
      p * 0.25,
      p
    ),
    p1 = p / max(p),
    .by = flokkur
  ) |> 
  ggplot(aes(seats, flokkur_ordered, fill = litur)) +
  annotate(
    geom = "segment",
    x = seq(0, 20, by = 2),
    xend = seq(0, 20, by = 2),
    y = 0.5,
    yend = 11,
    linewidth = 0.2,
    alpha = 0.1
  ) +
  geom_rect(
    aes(
      xmin = seats - 0.5,
      xmax = seats + 0.5,
      ymin = as.integer(flokkur_ordered),
      ymax = as.integer(flokkur_ordered) + p1 * 0.95
    ),
    col = "#fdfcfc",
    linewidth = 0.4
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = breaks_width(2),
    labels = label_number(accuracy = 1),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion()
  ) +
  coord_cartesian(
    xlim = c(0, 20),
    ylim = c(0.5, 11),
    clip = "on"
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

table_dat <- plot_dat |> 
  summarise(
    mean = sum(seats * p),
    median = min(seats[cumsum(p) > 0.5]),
    .by = flokkur
  ) |> 
  select(
    flokkur, median, mean
  ) |> 
  inner_join(
    support
  ) |> 
  arrange(desc(mean)) |> 
  mutate(
    p_in = case_when(
      p_in >= 0.992 ~ "Nánast öruggt",
      p_in >= 0.8 ~ "Mjög líklegt",
      p_in >= 0.6 ~ "Líklegri en ekki",
      p_in >= 0.4 ~ "Helmingslíkur",
      p_in >= 0.2 ~ "Nokkrar líkur",
      TRUE ~ "Ólíklegt"
    )
    # median = if_else(
    #   median <= 1,
    #   "0-1",
    #   as.character(round(median))
    # )
  )

table <- table_dat |> 
  gt(process_md = TRUE) |>
  cols_hide(mean) |> 
  cols_label(
    flokkur = "",
    median = "Miðgildi",
    p_in = "Fylgi>5%¹"
  ) |>
  cols_align("center") |>
  cols_hide(flokkur) |>
  tab_options(
    table.background.color = "transparent",
    column_labels.hidden = FALSE,
    table.border.top.style = "0px",
    table.border.bottom.style = "0px",
    table.font.size = px(20),
    # table_body.hlines.style = "0px",
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
        rows = flokkur == str_to_sentence(colors$flokkur[row])
      )
    )
}



p_tab <- p + wrap_table(table, space = "fixed")

p_tab_sharing <- p_tab +
  plot_annotation(
    title = "Þingsætaspá Metils",
    caption = caption,
    theme = theme(
      plot.title = element_text(
        size = 30,
        margin = margin(5, 5, -20, 5)
      )
    )
  )

ggsave(
  plot = p_tab_sharing,
  filename = here("Figures", "Historical", "Sharing", glue("{fit_date}_seats.png")),
  width = 8,
  height = 0.55 * 8,
  scale = 1.4,
  bg = "#fdfcfc"
)

#### No Title/Subtitle

table <- table_dat |> 
  gt(process_md = TRUE) |>
  cols_hide(mean) |> 
  fmt_number(
    columns = median,
    decimals = 0
  ) |>
  cols_label(
    flokkur = "",
    median = "Miðgildi",
    p_in = "Fylgi>5%¹"
  ) |>
  cols_align("center") |>
  cols_hide(flokkur) |>
  tab_options(
    table.background.color = "transparent",
    column_labels.hidden = FALSE,
    table.border.top.style = "0px",
    table.border.bottom.style = "0px",
    table.font.size = px(20),
    # table_body.hlines.style = "0px",
    table_body.border.bottom.style = "0px",
    table_body.border.top.style = "0px"
  ) |>
  opt_table_font(
    font = google_font("Lato"),
    weight = "bold"
  ) |>
  opt_horizontal_padding(0) |>
  opt_vertical_padding(1.9)



for (row in seq_len(nrow(colors))) {
  table <- table |>
    tab_style(
      style = cell_text(
        color = colors$litur[row],
        weight = "bold",
        font = google_font("Lato")
      ),
      locations = cells_body(
        rows = flokkur == str_to_sentence(colors$flokkur[row])
      )
    )
}



p_tab <- p + wrap_table(table, space = "fixed") +
  plot_annotation(
    caption = str_c(
      "¹Líkur á að flokkur hljóti meiri en 5% atkvæða og eigi þannig rétt á jöfnunarþingsætum.", "\n",
      "Nánast öruggt: >99%, Mjög líklegt: 80-99%, Líklegra en ekki: 60-80%, Helmingslíkur: 40-60%, Nokkrar líkur: 20-40%, Ólíklegt: <20%"
    )
  )





ggsave(
  plot = p_tab,
  filename = here("Figures", "Historical", "Background", glue("{fit_date}_seats_background.png")),
  width = 8,
  height = 0.55 * 8,
  scale = 1.4,
  bg = "#fdfcfc"
)

ggsave(
  plot = p_tab,
  filename = here("Figures", "Historical", "Transparent", glue("{fit_date}_seats_transparent.png")),
  width = 8,
  height = 0.55 * 8,
  scale = 1.4,
  bg = "transparent"
)

