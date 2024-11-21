make_plot <- function() {
  library(tidyverse)
  library(ggiraph)
  library(metill)
  library(patchwork)
  library(here)
  library(arrow)
  Sys.setlocale("LC_ALL", "is_IS.UTF-8")

  box::use(
    R / prepare_data[
      read_polling_data
    ]
  )

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

  point_shapes <- c(
    "Gallup" = 21,
    "Maskína" = 22,
    "Prósent" = 23,
    "Kosning" = 4,
    "MMR" = 1,
    "Fréttablaðið, Stöð 2 og Vísir" = 3,
    "Félagsvísindastofnun" = 24
  )

  # read data
  poll_data <- read_polling_data() |>
    mutate(
      p = n / sum(n),
      .by = c(date, fyrirtaeki)
    ) |>
    select(
      dags = date,
      fyrirtaeki,
      flokkur,
      p_poll = p
    ) |>
    mutate(
      fyrirtaeki = fct_relevel(fyrirtaeki, "Kosning")
    ) |> 
    filter( dags <= date_build(2024, 11, 3)
    )

  poll_data |>
    group_by(fyrirtaeki) |>
    summarise(
      min_dags = min(dags),
      max_dags = max(dags)
    )

  d <- read_parquet(here("data", "2024-11-03", "y_rep_draws.parquet")) |>    # recreate the 7th of november prediction
    summarise(
      mean = median(value),
      q5 = quantile(value, 0.05),
      q95 = quantile(value, 0.95),
      .by = c(dags, flokkur)
    ) |>
    inner_join(
      colors
    ) |>
    left_join(
      poll_data
    ) |>
    inner_join(
      poll_data |>
        filter(p_poll > 0) |>
        summarise(
          start_dags = min(dags),
          .by = flokkur
        )
    ) |>
    filter(dags >= start_dags)



  coverage_data <- read_parquet(here("data", "2024-11-03", "y_rep_draws.parquet")) |>  # recreate the 7th of november prediction
    reframe(
      coverage = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(dags, flokkur)
    ) |>
    inner_join(
      colors
    ) |>
    inner_join(
      poll_data |>
        filter(p_poll > 0) |>
        summarise(
          start_dags = min(dags),
          .by = flokkur
        )
    ) |>
    filter(dags >= start_dags)



  p1 <- d |>
    filter(dags == max(dags)) |>
    distinct(flokkur, mean, litur) |>
    mutate(
      flokkur_ordered = fct_reorder(flokkur, mean)
    ) |>
    ggplot(aes(mean, flokkur_ordered, color = litur, data_id = flokkur)) +
    geom_text_interactive(
      aes(label = flokkur, x = 0),
      hjust = 1,
      nudge_x = -0.005,
      size = 5
    ) +
    geom_segment_interactive(
      aes(xend = 0, yend = flokkur_ordered),
      alpha = 0.3,
      linewidth = 0.6
    ) +
    geom_point_interactive(
      size = 2.5
    ) +
    scale_x_continuous(
      breaks = seq(0, 0.25, by = 0.05),
      guide = ggh4x::guide_axis_truncated(
        trunc_upper = 0.25
      ),
      labels = label_percent()
    ) +
    scale_colour_identity() +
    coord_cartesian(clip = "off", xlim = c(-0.12, 0.25)) +
    theme(
      plot.margin = margin(5, 15, 5, 15),
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Staðan í nýjustu könnunum"
    )

  p2 <- d |>
    ggplot(aes(dags, mean, colour = litur, data_id = flokkur)) +
    geom_vline(
      xintercept = clock::date_build(2024, 11, 30),
      alpha = 0.4
    ) +
    annotate(
      geom = "label",
      label = "Kosningar 30. nóvember",
      x = clock::date_build(2024, 11, 30),
      y = 0.18,
      hjust = 0.5,
      vjust = 1,
      angle = 90,
      fill = "#faf9f9"
    ) +
    geom_ribbon_interactive(
      data = coverage_data,
      aes(
        x = dags,
        ymin = lower,
        ymax = upper,
        fill = litur,
        alpha = -coverage,
        group = paste(flokkur, coverage),
        data_id = flokkur
      ),
      col = NA,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    geom_line_interactive(
      data = ~ filter(.x, dags != max(dags))
    ) +
    geom_point_interactive(
      aes(y = p_poll, shape = fyrirtaeki, fill = litur),
      alpha = 0.3
    ) +
    scale_x_date(
      guide = ggh4x::guide_axis_truncated(
        trunc_upper = clock::date_build(2024, 11, 30)
      ),
      limits = c(NA_Date_, clock::date_build(2024, 11, 30)),
      labels = label_date_short(),
      breaks = clock::date_build(
        2024,
        c(
          8, 8, 8, 8,
          9, 9, 9, 9,
          10, 10, 10, 10,
          11, 11, 11, 11, 11
        ),
        c(
          1, 8, 15, 22,
          1, 8, 15, 22,
          1, 8, 15, 22,
          1, 8, 15, 22, 30
        )
      )
    ) +
    scale_y_continuous(
      breaks = seq(0, 0.3, by = 0.05),
      limits = c(0, 0.3),
      guide = ggh4x::guide_axis_truncated(),
      labels = label_percent()
    ) +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_alpha_continuous(
      range = c(0, 0.1)
    ) +
    scale_shape_manual(
      values = point_shapes,
      name = "Könnunarfyrirtæki:"
    ) +
    coord_cartesian(
      xlim = clock::date_build(2024, c(8, 11), c(1, 30))
    ) +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Kapphlaupið"
    )

  p3 <- d |>
    filter(dags >= clock::date_build(2021, 1, 1)) |>
    ggplot(aes(dags, mean, colour = litur, data_id = flokkur)) +
    geom_vline(
      xintercept = clock::date_build(2024, 11, 30),
      alpha = 0.4
    ) +
    annotate(
      geom = "label",
      label = "Kosningar 30. nóvember",
      x = clock::date_build(2024, 11, 30),
      y = 0.18,
      hjust = 0.5,
      vjust = 1,
      angle = 90,
      fill = "#faf9f9"
    ) +
    geom_line_interactive(
      linewidth = 1
    ) +
    geom_point_interactive(
      data = ~ filter(.x, fyrirtaeki != "Kosning"),
      aes(
        y = p_poll,
        shape = fyrirtaeki,
        color = litur,
        fill = litur
      ),
      alpha = 0.2
    ) +
    geom_point_interactive(
      data = ~ filter(.x, fyrirtaeki == "Kosning"),
      aes(
        y = p_poll,
        shape = fyrirtaeki,
        color = litur,
        fill = litur
      ),
      alpha = 0.8,
      size = 5
    ) +
    scale_shape_manual(
      values = point_shapes,
      name = "Könnunarfyrirtæki:",
      na.translate = FALSE
    ) +
    scale_color_identity() +
    scale_fill_identity() +
    guides(shape = guide_legend(override.aes = list(alpha = 0.8))) +
    scale_x_date(
      guide = ggh4x::guide_axis_truncated(
        trunc_upper = clock::date_build(2024, 11, 30)
      ),
      limits = c(NA_Date_, clock::date_build(2024, 11, 30)),
      labels = label_date_short(),
      breaks = seq.Date(
        from = clock::date_build(2016, 1),
        to = clock::date_build(2024, 11, 30),
        by = "3 month"
      )
    ) +
    scale_y_continuous(
      breaks = seq(0, 0.3, by = 0.05),
      guide = ggh4x::guide_axis_truncated(),
      labels = label_percent()
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Fylgisþróun"
    ) +
    theme(
      legend.position = "bottom",
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.box.margin = margin(6, 6, 6, 6)
    )

  design <- "
AABB
CCCC
"

  p <- wrap_plots(
    p1, p2, p3,
    design = design,
    heights = c(0.6, 1)
  ) +
    plot_annotation(
      title = "Samantekt á fylgi stjórnmálaflokka",
      subtitle = "Niðustöður mismunandi kannana vegnar saman"
    )

  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 1 * 11,
    bg = "transparent",
    options = list(
      opts_tooltip(
        opacity = 0.8,
        use_fill = TRUE,
        use_stroke = FALSE,
        css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:white"
      ),
      opts_hover(css = ""),
      opts_hover_inv(css = "opacity:0.05"),
      opts_toolbar(saveaspng = TRUE),
      opts_zoom(max = 1)
    )
  )
}
