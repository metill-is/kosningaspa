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

d <- list.files(
  here("data", "historical_results"),
  full.names = TRUE
) |>
  paste0("/results.csv") |>
  map(read_csv) |>
  bind_rows()

digits <- 1


#### Plots with title and subtitle ####
plots <- d |>
  mutate(
    flokkur = if_else(flokkur == "Annað", "Lýðræðisflokkurinn", flokkur) |>
      str_to_sentence(),
    fit_date2 = fit_date
  ) |>
  inner_join(
    colors,
    by = "flokkur"
  ) |>
  group_by(fit_date2) |>
  group_map(
    function(data, ...) {
      fit_date <- unique(data$fit_date)
      days_to_vote <- as.numeric(vote_date - fit_date)
      xl <- if_else(fit_date >= ymd("2024-09-29"), "Lýðræðisflokkurinn", "Annað")

      p <- data |>
        mutate(
          flokkur = if_else(flokkur == "Lýðræðisflokkurinn", xl, flokkur) |>
            str_to_sentence(),
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
          yend = 10.255,
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
          ylim = c(0.5, 10.5),
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

      table <- data |>
        filter(
          coverage == 0.9
        ) |>
        select(flokkur, median, lower, upper) |>
        arrange(desc(median)) |>
        gt(process_md = TRUE) |>
        fmt_percent(
          columns = median:upper,
          decimals = digits
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



      p_tab <- p + wrap_table(table, space = "fixed") +
        plot_annotation(
          title = "Kosningaspá Metils",
          subtitle = glue("Miðja síðustu kannanar í spánni var {format_date(fit_date)}"),
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
        filename = here("Figures", "Historical", "Sharing", glue("{fit_date}.png")),
        width = 8,
        height = 0.55 * 8,
        scale = 1.4,
        bg = "#fdfcfc"
      )


      p_tab
    }
  )

#### Plots without title and subtitle ####
plots <- d |>
  mutate(
    flokkur = if_else(flokkur == "Annað", "Lýðræðisflokkurinn", flokkur) |>
      str_to_sentence(),
    fit_date2 = fit_date
  ) |>
  inner_join(
    colors,
    by = "flokkur"
  ) |>
  group_by(fit_date2) |>
  group_map(
    function(data, ...) {
      fit_date <- unique(data$fit_date)
      days_to_vote <- as.numeric(vote_date - fit_date)
      xl <- if_else(fit_date >= ymd("2024-09-29"), "Lýðræðisflokkurinn", "Annað")

      p <- data |>
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
          x = seq(0.1, 0.25, 0.05),
          xend = seq(0.1, 0.25, 0.05),
          y = 0.5,
          yend = 10.255,
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
          breaks = c(0.05, seq(0, 0.25, 0.05)),
          labels = label_percent(),
          limits = c(0, 0.25),
          guide = ggh4x::guide_axis_truncated(
            trunc_lower = 0,
            trunc_upper = 0.25
          ),
          expand = expansion(mult = c(0, 0.01))
        ) +
        scale_y_discrete(
          guide = ggh4x::guide_axis_truncated(),
          expand = expansion()
        ) +
        scale_alpha_continuous(
          range = c(0, 0.25)
        ) +
        coord_cartesian(
          xlim = c(0, 0.25),
          ylim = c(0.5, 10.5),
          clip = "off"
        ) +
        theme(
          legend.position = "none",
          axis.text.y = element_markdown(size = 20),
          plot.margin = margin(0, 0, 0, 0)
        ) +
        labs(
          x = NULL,
          y = NULL
        )


      table <- data |>
        filter(
          coverage == 0.9
        ) |>
        select(flokkur, median, lower, upper) |>
        arrange(desc(median)) |>
        gt(process_md = TRUE) |>
        fmt_percent(
          columns = median:upper,
          decimals = digits
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
          table.font.size = px(24),
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



      p_tab <- p + wrap_table(table, space = "fixed")

      ggsave(
        plot = p_tab,
        filename = here("Figures", "Historical", "Background", glue("{fit_date}_background.png")),
        width = 8,
        height = 0.55 * 8,
        scale = 1.4,
        bg = "#fdfcfc"
      )

      ggsave(
        plot = p_tab,
        filename = here("Figures", "Historical", "Transparent", glue("{fit_date}_transparent.png")),
        width = 8,
        height = 0.55 * 8,
        scale = 1.4,
        bg = "transparent"
      )
    }
  )
