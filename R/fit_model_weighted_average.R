library(tidyverse)
library(here)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(gt)
library(gtExtras)
library(arrow)
library(scales)
library(clock)
library(metill)
theme_set(theme_metill())

box::use(
  R / data[
    read_polling_data,
    read_fundamentals_data,
    read_constituency_data
  ],
  R / stan_data[
    prepare_stan_data
  ]
)

election_date <- date_build(2024, 11, 30)

polling_data <- read_polling_data() |>
  filter(
    date >= date_build(2016, 1, 1)
  )

unique(polling_data$flokkur)
max(polling_data$date)

fundamentals_data <- read_fundamentals_data()

econ_data <- read_csv(here("data", "economy_data.csv"))

fundamentals_data <- fundamentals_data |>
  inner_join(
    econ_data,
    by = "date"
  )

constituency_data <- read_constituency_data() |>
  mutate(var = NA) |>
  drop_na() |>
  select(-var)

polling_data$fyrirtaeki |> levels()

stan_data <- prepare_stan_data(
  polling_data,
  fundamentals_data,
  constituency_data
)

stan_data$desired_weight <- 0.33
stan_data$weight_time <- 180
stan_data$last_poll_days <- 27
stan_data$last_poll_house <- 7
stan_data$n_last_poll <- 1000


str(stan_data)

model <- cmdstan_model(
  here("Stan", "base_model_no_polling_bias.stan")
)


fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 100
)

fit$summary("tau_stjornarslit")


fit$summary("sigma") |>
  mutate(
    flokkur = colnames(y),
    .before = variable
  )

fit$summary("phi_inv")




dates <- c(
  unique(polling_data$date),
  seq.Date(max(polling_data$date), election_date, by = "day")
)


fit$summary("y_rep", mean, ~ quantile(.x, c(0.05, 0.95))) |>
  rename(q5 = `5%`, q95 = `95%`) |>
  mutate(
    t = str_match(variable, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y_n)[p],
    dags = dates[t]
  ) |>
  group_by(dags) |>
  mutate_at(
    vars(mean, q5, q95),
    ~ .x / stan_data$n_pred
  ) |>
  ungroup() |>
  left_join(
    polling_data |>
      mutate(p = n / sum(n), .by = c(date, fyrirtaeki)) |>
      select(dags = date, fyrirtaeki, flokkur, konnun = p),
    by = join_by(dags, flokkur),
    relationship = "many-to-many"
  ) |>
  filter(dags >= date_build(2024, 1, 1)) |>
  ggplot(aes(dags, mean)) +
  geom_ribbon(
    aes(ymin = q5, ymax = q95, fill = flokkur),
    alpha = 0.2
  ) +
  geom_line(aes(col = flokkur), linewidth = 1) +
  geom_point(aes(y = konnun, col = flokkur))



fit$draws("y_rep") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw)
  ) |>
  mutate(
    t = str_match(name, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(name, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y_n)[p],
    dags = dates[t]
  ) |>
  filter(dags == max(dags)) |>
  mutate(
    value = value / stan_data$n_pred
  ) |>
  summarise(
    mean = mean(value),
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .by = flokkur
  ) |>
  select(flokkur, mean, q5, q95) |>
  arrange(desc(mean)) |>
  mutate(
    plot_col = mean,
    .before = mean
  ) |>
  gt() |>
  cols_label(
    flokkur = "Flokkur",
    plot_col = "",
    mean = "Væntigildi",
    q5 = "Neðri",
    q95 = "Efri"
  ) |>
  tab_spanner(
    label = "95% Öryggisbil",
    columns = c(q5, q95)
  ) |>
  cols_align(
    align = "left",
    columns = 1
  ) |>
  fmt_percent() |>
  gt_color_rows(
    columns = c(mean, q5, q95),
    palette = "Greys",
    domain = c(0, 0.5)
  ) |>
  gt_plt_conf_int(
    column = plot_col,
    ci_columns = c(q5, q95),
    ref_line = 0.05,
    text_size = 0,
    width = 30
  ) |>
  tab_header(
    title = "Spáð fylgi stjórnmálaflokka"
  )


y_rep_draws <- fit$draws("y_rep") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw)
  ) |>
  mutate(
    t = str_match(name, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(name, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y_n)[p],
    dags = dates[t]
  ) |>
  select(
    .chain,
    .iteration,
    .draw,
    dags,
    flokkur,
    value
  ) |>
  mutate(
    value = value / stan_data$n_pred
  )


last_poll_date <- max(polling_data$date)
write_parquet(y_rep_draws, here("data", as.character(last_poll_date), "y_rep_draws_no_polling_bias.parquet"))

theme_set(metill::theme_metill())

fit$summary("gamma") |>
  select(variable, mean, q5, q95) |>
  mutate(
    p = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
    h = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y_n)[p],
    fyrirtaeki = levels(polling_data$fyrirtaeki)[h]
  ) |>
  filter(h != 1) |>
  ggplot(aes(0, fyrirtaeki, col = fyrirtaeki)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(aes(x = mean), size = 3) +
  geom_segment(
    aes(x = q5, xend = q95, y = fyrirtaeki, yend = fyrirtaeki),
    alpha = 0.5
  ) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  coord_cartesian(
    xlim = c(-0.4, 0.4)
  ) +
  facet_wrap(
    vars(flokkur),
    scales = "free_y"
  )

fit$summary("industry_bias") |>
  mutate(
    flokkur = colnames(y) |>
      as_factor() |>
      fct_reorder(mean),
    .before = variable
  ) |>
  ggplot(aes(mean, flokkur)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(
    size = 3
  ) +
  geom_segment(
    aes(x = q5, xend = q95, y = flokkur, yend = flokkur),
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = breaks_width(width = 0.1),
    guide = ggh4x::guide_axis_truncated(
      trunc_lower = -0.4,
      trunc_upper = 0.4
    )
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Heildarbjagi í mati á fylgi flokka",
    subtitle = "Sýnt á log-odds kvarða"
  )


fit$summary("Omega") |>
  mutate(
    p = str_match(variable, "Omega\\[(.*),.*\\]")[, 2] |> parse_number(),
    q = str_match(variable, "Omega\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur1 = colnames(stan_data$y)[-1][p],
    flokkur2 = colnames(stan_data$y)[-1][q]
  ) |>
  select(flokkur1, flokkur2, mean) |>
  pivot_wider(
    names_from = flokkur2,
    values_from = mean
  ) |>
  column_to_rownames("flokkur1") |>
  as.matrix() |>
  corrplot::corrplot(
    method = "color",
    order = "hclust",
    tl.col = "black",
    addCoef.col = "black"
  )
