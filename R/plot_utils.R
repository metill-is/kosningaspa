#' @export
plot_gamma <- function(fit) {
  box::use(
    dplyr[mutate, select, bind_rows],
    stringr[str_match],
    ggplot2[
      ggplot,
      aes,
      geom_point,
      geom_segment,
      scale_x_continuous,
      scale_y_discrete,
      scale_colour_manual,
      facet_wrap,
      theme,
      labs,
      theme_set
    ],
    ggh4x[guide_axis_truncated],
    metill[theme_metill]
  )

  theme_set(theme_metill())
  fit$summary("gamma") |>
    select(variable, mean, q5, q95) |>
    mutate(
      p = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
      h = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
      flokkur = colnames(stan_data$y)[p],
      fyrirtaeki = levels(data$fyrirtaeki)[h]
    ) |>
    filter(h != 1) |>
    bind_rows(
      fit$summary("mu_gamma") |>
        mutate(
          flokkur = colnames(stan_data$y) |>
            as_factor() |>
            fct_reorder(mean),
          fyrirtaeki = "Samtals",
          .before = variable
        )
    ) |>
    ggplot(aes(0, fyrirtaeki, col = fyrirtaeki)) +
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(
      aes(x = mean),
      size = 3
    ) +
    geom_segment(
      aes(x = q5, xend = q95, y = fyrirtaeki, yend = fyrirtaeki),
      alpha = 0.5
    ) +
    scale_x_continuous(
      breaks = breaks_width(width = 0.1),
      guide = ggh4x::guide_axis_truncated()
    ) +
    scale_y_discrete(
      guide = ggh4x::guide_axis_truncated()
    ) +
    scale_colour_manual(
      values = c(
        "Samtals" = "black",
        "Prósent" = "#1f78b4",
        "Maskína" = "#1b9e77",
        "Gallup" = "#e41a1c"
      )
    ) +
    facet_wrap(
      vars(flokkur),
      ncol = 1,
      scales = "free_y"
    ) +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Bjagi mismunandi fyrirtækja á fylgi flokka"
    )
}
