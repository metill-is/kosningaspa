library(tidyverse)
library(glue)
library(arrow)
library(here)
library(clock)
box::use(
  R / modeling_utils[fit_model_at_date_2021],
  R / data[read_polling_data]
)

polling_data <- read_polling_data()

fit_dates <- polling_data |>
  filter(
    date <= date_build(2021, 9, 25),
    !((fyrirtaeki == "Kosning") & (date == date_build(2021, 9, 25)))
  ) |>
  arrange(desc(date)) |>
  pull(date) |>
  unique() |>
  head(1)


results <- list()

for (i in seq_along(fit_dates)) {
  fit_date <- fit_dates[i]

  results[[i]] <- fit_model_at_date_2021(
    cutoff_date = fit_date,
    weight_time = 180,
    weight_desired = 0.50
  ) |>
    mutate(
      fit_date = fit_date
    )
}

library(ggplot2)
# results |>
#  bind_rows() |>
#  write_csv(here("data", "model_results_2021.csv"))

p <- results |>
  bind_rows() |>
  inner_join(
    polling_data |>
      filter(
        fyrirtaeki == "Kosning",
        year(date) == 2021
      ) |>
      mutate(
        p = n / sum(n)
      ),
    by = "flokkur"
  ) |>
  mutate(
    lower = lower - p,
    upper = upper - p
  ) |>
  ggplot(aes(x = fit_date, y = median)) +
  geom_hline(
    yintercept = 0,
    linewidth = 2
  ) +
  geom_segment(
    aes(
      xend = fit_date,
      y = lower,
      yend = upper,
      alpha = -coverage,
      group = coverage
    ),
    linewidth = 3
  ) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  facet_wrap(vars(flokkur)) +
  theme_bw()

ggsave(
  plot = p,
  filename = here("Figures", "model_results_2021_1.png"),
  width = 8,
  height = 8,
  scale = 1.3
)


p <- results |>
  bind_rows() |>
  distinct(flokkur, median, fit_date) |>
  filter(fit_date == max(fit_date)) |>
  select(-fit_date) |>
  inner_join(
    polling_data |>
      filter(
        fyrirtaeki == "Kosning",
        year(date) == 2021
      ) |>
      mutate(
        kosning = n / sum(n)
      ) |>
      select(flokkur, kosning)
  ) |>
  inner_join(
    polling_data |>
      filter(
        date < date_build(2021, 09, 25),
        year(date) == 2021
      ) |>
      filter(date == max(date), .by = fyrirtaeki) |>
      mutate(
        p = n / sum(n),
        .by = fyrirtaeki
      ) |>
      select(fyrirtaeki, flokkur, p) |>
      pivot_wider(names_from = fyrirtaeki, values_from = p) |>
      janitor::clean_names()
  ) |>
  rename(metill = median) |>
  pivot_longer(c(-flokkur, -kosning)) |>
  mutate(
    err = value - kosning
  ) |>
  ggplot(aes(err, name)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_segment(
    aes(
      xend = 0,
      color = name
    ),
    alpha = 0.4,
    linewidth = 0.4
  ) +
  geom_point(
    size = 4,
    aes(color = name)
  ) +
  facet_wrap("flokkur") +
  theme_bw() +
  theme(
    legend.position = "none"
  )

p

ggsave(
  plot = p,
  filename = here("Figures", "model_results_2021.png"),
  width = 8,
  height = 8,
  scale = 1.3
)


library(gt)
library(gtExtras)
library(patchwork)

results |>
  bind_rows() |>
  distinct(flokkur, median, fit_date) |>
  filter(fit_date == max(fit_date)) |>
  select(-fit_date) |>
  inner_join(
    polling_data |>
      filter(
        fyrirtaeki == "Kosning",
        year(date) == 2021
      ) |>
      mutate(
        kosning = n / sum(n)
      ) |>
      select(flokkur, kosning)
  ) |>
  inner_join(
    polling_data |>
      filter(
        date < date_build(2021, 09, 25),
        year(date) == 2021
      ) |>
      filter(date == max(date), .by = fyrirtaeki) |>
      mutate(
        p = n / sum(n),
        .by = fyrirtaeki
      ) |>
      select(fyrirtaeki, flokkur, p) |>
      pivot_wider(names_from = fyrirtaeki, values_from = p) |>
      janitor::clean_names()
  ) |>
  rename(metill = median) |>
  pivot_longer(c(-flokkur, -kosning)) |>
  mutate(
    err = value - kosning
  ) |>
  select(flokkur, name, kosning, value, err) |>
  mutate(
    flokkur2 = flokkur,
    err = abs(err)
  ) |>
  group_by(flokkur2) |>
  group_map(
    function(data, ...) {
      data |>
        select(-flokkur) |>
        gt() |>
        tab_header(
          title = unique(data$flokkur)
        ) |>
        gt_color_rows(err, palette = "Greys") |>
        fmt_percent() |>
        wrap_table(space = "free")
    }
  ) |>
  wrap_plots()



results |>
  bind_rows() |>
  distinct(flokkur, median, fit_date) |>
  filter(fit_date == max(fit_date)) |>
  select(-fit_date) |>
  inner_join(
    polling_data |>
      filter(
        fyrirtaeki == "Kosning",
        year(date) == 2021
      ) |>
      mutate(
        kosning = n / sum(n)
      ) |>
      select(flokkur, kosning)
  ) |>
  inner_join(
    polling_data |>
      filter(
        date < date_build(2021, 09, 25),
        year(date) == 2021
      ) |>
      filter(date == max(date), .by = fyrirtaeki) |>
      mutate(
        p = n / sum(n),
        .by = fyrirtaeki
      ) |>
      select(fyrirtaeki, flokkur, p) |>
      pivot_wider(names_from = fyrirtaeki, values_from = p) |>
      janitor::clean_names()
  ) |>
  rename(metill = median) |>
  pivot_longer(c(-flokkur, -kosning)) |>
  mutate(
    err = value - kosning
  ) |>
  select(flokkur, name, kosning, value, err) |>
  mutate(
    err = abs(err)
  ) |>
  arrange(desc(kosning)) |>
  group_by(name) |>
  top_n(9, kosning) |>
  ungroup() |>
  summarise(
    mean_mae = mean(err),
    median_mae = median(err),
    .by = name
  )
