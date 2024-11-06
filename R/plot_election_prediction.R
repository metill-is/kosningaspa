library(tidyverse)
library(ggiraph)
library(metill)
library(patchwork)
library(here)
library(arrow)
library(glue)
library(ggtext)
library(geomtextpath)
library(gt)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

theme_set(theme_metill())

today_date <- Sys.Date()
vote_date <- clock::date_build(2024, 11, 30)
days_until_vote <- as.numeric(vote_date - today_date)

caption <- str_c(
  "Samantektin byggir á fylgiskönnunum Félagsvísindastofnunar, Gallup, Maskínu og Prósents ",
  "ásamt niðurstöðum kosninga og kjörsókn 2021", "\n",
  "Unnið af Agnari Frey Helgasyni, Brynjólfi Gauta Guðrúnar Jónssyni, Hafsteini Einarssyni og Rafael Daniel Vias"
)

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
  "Annað", "grey30",
  "Lýðræðisflokkurinn", "grey30"
)

coverage_data <- read_parquet(here("data", today(), "y_rep_draws.parquet")) |>
  filter(dags == max(dags)) |> 
  reframe(
    mean = median(value),
    coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = c(dags, flokkur)
  ) |>
  inner_join(
    colors
  ) |> 
  mutate(
    flokkur = if_else(
      flokkur == "Annað",
      "Lýðræðisflokkurinn",
      flokkur
    ),
    flokkur_ordered = glue("<b style='color:{litur}'>{flokkur}</b>"),
    flokkur_ordered = fct_reorder(flokkur_ordered, mean)
  ) 

write_parquet(
  coverage_data,
  here(
    "results",
    "Predictions",
    glue("coverage_data_{today_date}.parquet")
  )
)


p <- coverage_data |>
  filter(
    dags == max(dags)
  ) |>
  ggplot(aes(
    y = flokkur_ordered,
    color = litur,
    group = paste(flokkur, coverage)
  )) +
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
      x = mean,
      xend = mean,
      y = as.integer(flokkur_ordered) - 0.4,
      yend = as.integer(flokkur_ordered) + 0.4
    ),
    linewidth = 1
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = lower,
      y = as.integer(flokkur_ordered) - 0.2,
      yend = as.integer(flokkur_ordered) + 0.2,
      alpha = -coverage
    ),
    linewidth = 0.2
  ) +
  geom_segment(
    aes(
      x = upper,
      xend = upper,
      y = as.integer(flokkur_ordered) - 0.2,
      yend = as.integer(flokkur_ordered) + 0.2,
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
    breaks = c(0.05, seq(0, 0.25, 0.05)),
    labels = label_percent(),
    limits = c(0, 0.25),
    guide = ggh4x::guide_axis_truncated(
      trunc_lower = 0,
      trunc_upper = 0.25
    )
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion()
  ) +
  scale_alpha_continuous(
    range = c(0, 0.3)
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(size = 12),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  labs(
    x = NULL,
    y = NULL
  )

ggsave(
  here("Figures", "Preds", glue("election_prediction_no_polling_bias_{today_date}.png")),
  width = 8,
  height = 0.4 * 8,
  scale = 1.4
)


table <- coverage_data |>
  filter(
    dags == max(dags),
    coverage == 0.9
  ) |>
  select(flokkur, mean, lower, upper) |>
  arrange(desc(mean)) |>
  gt(process_md = TRUE) |>
  fmt_percent(
    columns = mean:upper,
    decimals = 0
  ) |>
  cols_label(
    flokkur = "",
    mean = "Spá",
    lower = "Neðri",
    upper = "Efri"
  ) |>
  tab_spanner(
    label = md("Óvissubil¹"),
    columns = lower:upper
  ) |>
  tab_options(
    table.background.color = "transparent",
    column_labels.hidden = FALSE, 
    table.border.top.style = "0px",
    table.border.bottom.style = "0px",
    table.font.size = "14px",
    #table_body.hlines.style = "0px",
    table_body.border.bottom.style = "0px",
    table_body.border.top.style = "0px"
  ) |> 
  opt_table_font(
    font = google_font("Lato"),
    weight = "bold"
  )

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



p_tab <- p + wrap_table(table, space = "fixed") +
  plot_annotation(
    title = glue("Fylgisspá þegar {days_until_vote} dagar eru til kosninga"),
    subtitle = "Spáð fylgi stjórnmálaflokkanna 30. nóvember",
    caption = str_c(
      caption, "\n",
      "1: 90% óvissubil úr eftirádreifingu líkansins"
    )
  )

ggsave(
  here("Figures", "Preds", glue("election_prediction_table_{today_date}.png")),
  width = 8,
  height = 0.4 * 8,
  scale = 1.4
)
