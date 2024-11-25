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

theme_set(theme_metill(type = "blog"))

vote_date <- clock::date_build(2024, 11, 30)

format_date <- function(date) {
  clock::date_format(date, format = "%d. %B", locale = clock::clock_locale("is")) |>
    str_replace("^0", "")
}

format_date(vote_date)

caption <- str_c(
  "¹Líkur á að flokkur hljóti meiri en 5% atkvæða og eigi þannig rétt á jöfnunarþingsætum", "\n",
  "Mynd frá kosningavaktinni á www.metill.is", "\n",
  "Unnið af Brynjólfi Gauta Guðrúnar Jónssyni, ásamt Agnari Frey Helgasyni, Hafsteini Einarssyni og Rafael Daniel Vias", "\n",
  "Frekari greiningar og útskýringar á aðferðafræði má nálgast á www.metill.is"
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
) |>
  mutate(
    flokkur = str_to_sentence(flokkur)
  )

library(ggridges)
fit_date <- clock::date_build(2024, 11, 18)

caption <- str_c(
  "¹Nánast öruggt: >99%, Mjög líklegt: 80-99%, Líklegra en ekki: 60-80%, Helmingslíkur: 40-60%, Nokkrar líkur: 20-40%, Ólíklegt: <20%", "\n",
  "Unnið af Brynjólfi Gauta Guðrúnar Jónssyni, ásamt Agnari Frey Helgasyni, Hafsteini Einarssyni og Rafael Daniel Vias", "\n",
  "Frekari greiningar og útskýringar á aðferðafræði má nálgast á www.metill.is"
)

box::use(
  R / party_utils[party_tibble]
)

draws <- d |> 
  count(.draw, flokkur, wt = seats, name = "seats") |> 
  inner_join(
    party_tibble() |> 
      mutate(
        bokstafur = if_else(
          flokkur == "Annað",
          "L",
          bokstafur
        )
      )
  ) |> 
  select(.draw, bokstafur, seats) |> 
  pivot_wider(
    names_from = bokstafur, values_from = seats
  )  |> 
  mutate(
    BCJS = B + C + J + S,
    BCPS = B + C + P + S,
    BCS = B + C + S,
    BCSV = B + C + S + V,
    BDM = B + D + M,
    CD = C + D,
    CDM = C + D + M,
    CDS = C + D + S,
    CFS = C + F + S,
    CMS = C + M + S,
    CS = C + S,
    DFM = D + F + M,
    DFS = D + M + S,
    DM = D + M,
    DS = D + S
  ) |> 
  select(-(L:C)) |> 
  pivot_longer(c(-.draw), names_to = "flokkur", values_to = "seats")

plot_dat <- draws |> 
  count(flokkur, seats) |> 
  mutate(p = n / sum(n), .by = flokkur) |> 
  mutate(
    flokkur_ordered = fct_reorder(flokkur, seats * p, sum),
    col = factor(1 * (seats >= 32))
  )



colors <- party_tibble()$litur
names(colors) <- party_tibble()$bokstafur

add_colours <- function(flokkur) {
  str_split(flokkur, pattern = "")[[1]] |> 
    map(
      function(x) {
        glue(
          "<b style='color:{colors[x]};'>{x}</b>"
        )
      }
    ) |> 
    str_c(collapse = "")
}

n_fl <- length(unique(plot_dat$flokkur))

p <- plot_dat |> 
  mutate(
    p1 = p / max(p),
    .by = flokkur
  ) |> 
  mutate(
    flokkur_ordered = map_chr(flokkur, add_colours) |> 
      fct_reorder(seats * p, sum)
  ) |> 
  ggplot(aes(seats, flokkur_ordered)) +
  annotate(
    geom = "segment",
    x = 32,
    xend = 32,
    y = 0.5,
    yend = n_fl + 1.5,
    linewidth = 0.7,
    alpha = 0.4,
    lty = 2
  ) +
  geom_rect(
    aes(
      xmin = seats - 0.5,
      xmax = seats + 0.5,
      ymin = as.integer(flokkur_ordered),
      ymax = as.integer(flokkur_ordered) + p1 * 0.95,
      fill = col
    ),
    col = "#fdfcfc",
    linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c(
      "grey60",
      "grey20"
    )
  ) +
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
    xlim = c(14, 46),
    ylim = c(0.5, n_fl + 1),
    clip = "on"
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(size = 22),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  labs(
    x = NULL,
    y = NULL
  )



table_dat <- draws |> 
  summarise(
    median = median(seats),
    mean = mean(seats),
    p_in = mean(seats >= 32),
    .by = flokkur
  )|> 
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
    p_in = "≥32¹"
  ) |>
  tab_spanner(
    label = "Þingsæti",
    columns = 2:4
  ) |> 
  cols_align("center") |>
  cols_hide(flokkur) |>
  tab_options(
    table.background.color = "transparent",
    column_labels.hidden = FALSE,
    table.border.top.style = "0px",
    table.border.bottom.style = "0px",
    table.font.size = px(22),
    # table_body.hlines.style = "0px",
    table_body.border.bottom.style = "0px",
    table_body.border.top.style = "0px"
  ) |>
  opt_table_font(
    font = google_font("Lato"),
    weight = "bold"
  ) |>
  opt_horizontal_padding(0) |>
  opt_vertical_padding(1)


p_tab <- p + wrap_table(table, space = "fixed")

p_tab_sharing <- p_tab +
  plot_annotation(
    title = "Samanlögð þingsætaspá Metils",
    subtitle = "Myndin sýnir dreifingu sætafjölda, en ekki líkur á að flokkar vilji vinna saman",
    caption = caption,
    theme = theme(
      plot.title = element_text(
        size = 30,
        margin = margin(5, 5, 5, 5)
      ),
      plot.subtitle = element_text(
        margin = margin(0, 5, -5, 5)
      )
    )
  )

p_tab_sharing

ggsave(
  plot = p_tab_sharing,
  filename = here("Figures", "Historical", "Sharing", glue("{fit_date}_majority_seats.png")),
  width = 8,
  height = 0.621 * 8,
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
  tab_spanner(
    label = "Þingsæti",
    columns = 2:4
  ) |> 
  cols_label(
    flokkur = "",
    median = "Miðgildi",
    p_in = "≥32¹"
  ) |>
  cols_align("center") |>
  cols_hide(flokkur) |>
  tab_options(
    table.background.color = "transparent",
    column_labels.hidden = FALSE,
    table.border.top.style = "0px",
    table.border.bottom.style = "0px",
    table.font.size = px(22),
    # table_body.hlines.style = "0px",
    table_body.border.bottom.style = "0px",
    table_body.border.top.style = "0px"
  ) |>
  opt_table_font(
    font = google_font("Lato"),
    weight = "bold"
  ) |>
  opt_horizontal_padding(0) |>
  opt_vertical_padding(1.3)




p_tab <- p + wrap_table(table, space = "fixed") +
  plot_annotation(
    caption = str_c(
      "¹Nánast öruggt: >99%, Mjög líklegt: 80-99%, Líklegra en ekki: 60-80%, Helmingslíkur: 40-60%, Nokkrar líkur: 20-40%, Ólíklegt: <20%"
    )
  )





ggsave(
  plot = p_tab,
  filename = here("Figures", "Historical", "Background", glue("{fit_date}_majority_seats_background.png")),
  width = 8,
  height = 0.621 * 8,
  scale = 1.4,
  bg = "#fdfcfc"
)

ggsave(
  plot = p_tab,
  filename = here("Figures", "Historical", "Transparent", glue("{fit_date}_majority_seats_transparent.png")),
  width = 8,
  height = 0.621 * 8,
  scale = 1.4,
  bg = "transparent"
)

