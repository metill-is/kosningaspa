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



stan_data$desired_weight <- 0.15
stan_data$weight_time <- 180
stan_data$last_poll_days <- 20
stan_data$last_poll_house <- 7
stan_data$n_last_poll <- 1436


str(stan_data)

rownames(stan_data$y_f)
levels(polling_data$flokkur)

model <- cmdstan_model(
  here("Stan", "polling_and_fundamentals.stan")
)


fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  init = 0,
  iter_warmup = 1000,
  iter_sampling = 2000
)


# Polling Parameters
fit$summary("beta0") |>
  mutate(
    flokkur = colnames(stan_data$y_n)[-1]
  ) |>
  select(flokkur, mean, q5, q95) |>
  arrange(desc(mean))

fit$summary("mu_pred") |>
  mutate(
    flokkur = colnames(stan_data$y_n)[-1]
  ) |>
  select(flokkur, mean, q5, q95) |>
  arrange(desc(mean))

fit$summary("sigma") |>
  mutate(
    flokkur = colnames(stan_data$y_n)[-1]
  ) |>
  select(flokkur, mean, q5, q95) |>
  arrange(desc(mean))
fit$summary("mu_log_sigma")

fit$summary("tau_log_sigma")

fit$summary("tau_stjornarslit")
fit$summary("tau_f")

fit$summary("sigma_gamma")
fit$summary("mu_gamma")


# Fundamentals Parameters
fit$summary("alpha_f") |>
  mutate(
    flokkur = rownames(stan_data$y_f)
  ) |>
  select(flokkur, mean, q5, q95) |>
  arrange(desc(mean)) |>
  semi_join(
    polling_data,
    by = join_by(flokkur)
  )



fit$summary("beta_lag_f")


fit$summary("beta_inc_years_f")

fit$summary(c("beta_vnv_f", "beta_growth_f"))

fit$draws(c("beta_vnv_f", "beta_growth_f")) |>
  summarise_draws(
    "perc_less_than_0" = ~ mean(.x < 0)
  )

fit$summary("tau_f")

fit$summary("phi_inv") |>
  mutate(
    house = levels(polling_data$fyrirtaeki)
  ) |>
  select(
    house, mean, q5, q95, rhat
  )

fit$summary("phi_f_inv")



fit$summary("Omega") |>
  mutate(
    p = str_match(variable, "Omega\\[(.*),.*\\]")[, 2] |> parse_number(),
    q = str_match(variable, "Omega\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur1 = colnames(stan_data$y_n)[-1][p],
    flokkur2 = colnames(stan_data$y_n)[-1][q]
  ) |>
  select(flokkur1, flokkur2, mean) |>
  pivot_wider(
    names_from = flokkur2,
    values_from = mean
  ) |>
  column_to_rownames("flokkur1") |>
  as.matrix() |>
  solve() |>
  cov2cor() |>
  corrplot::corrplot(
    method = "color",
    order = "hclust",
    tl.col = "black",
    addCoef.col = "black",
    is.corr = TRUE
  )

fit$summary("Omega") |>
  mutate(
    p = str_match(variable, "Omega\\[(.*),.*\\]")[, 2] |> parse_number(),
    q = str_match(variable, "Omega\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur1 = colnames(stan_data$y_n)[-1][p],
    flokkur2 = colnames(stan_data$y_n)[-1][q]
  ) |>
  select(flokkur1, flokkur2, mean, q5, q95) |>
  filter(flokkur1 != flokkur2) |>
  ggplot(aes(mean, flokkur2)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  geom_linerange(aes(xmin = q5, xmax = q95)) +
  facet_wrap(
    vars(flokkur1),
    scales = "free_y"
  )


dates <- c(
  unique(polling_data$date),
  seq.Date(max(polling_data$date) + 1, election_date, by = "day")
)

d_yrep <- fit$summary("y_rep")

d_yrep |>
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
  filter(dags >= clock::date_build(2024, 1, 14)) |>
  ggplot(aes(dags, mean)) +
  geom_ribbon(
    aes(ymin = q5, ymax = q95, fill = flokkur),
    alpha = 0.2
  ) +
  geom_line(aes(col = flokkur), linewidth = 1) +
  geom_point(aes(y = konnun, col = flokkur))


d_yrep |>
  mutate(
    t = str_match(variable, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y_n)[p],
    dags = dates[t]
  ) |>
  filter(
    dags %in% c(max(dags), today())
  ) |>
  mutate(
    mean = median / stan_data$n_pred,
    q5 = q5 / stan_data$n_pred,
    q95 = q95 / stan_data$n_pred,
    interval_size = q95 - q5
  ) |>
  select(dags, flokkur, interval_size) |>
  pivot_wider(
    names_from = dags,
    values_from = interval_size
  )

d_yrep |>
  mutate(
    t = str_match(variable, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y_n)[p],
    dags = dates[t]
  ) |>
  filter(
    dags == max(dags)
  ) |>
  mutate(
    mean = median / stan_data$n_pred,
    q5 = q5 / stan_data$n_pred,
    q95 = q95 / stan_data$n_pred
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
    label = "90% Óvissubil",
    columns = c(q5, q95)
  ) |>
  cols_align(
    align = "left",
    columns = 1
  ) |>
  fmt_percent(
    decimals = 0
  ) |>
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

last_poll_date <- today()
dir.create(here("data", as.character(last_poll_date)), showWarnings = FALSE)
write_parquet(y_rep_draws, here("data", as.character(last_poll_date), "y_rep_draws.parquet"))

#### Predicting The Newest Poll ####

theme_set(theme_metill(type = "blog"))

d <- fit$draws("y_rep_newest_poll")

plot_dat <- d |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw)
  ) |>
  mutate(
    value = value / sum(value),
    .by = .draw
  ) |>
  summarise(
    mean = mean(value),
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .by = c(name)
  ) |>
  mutate(
    flokkur = colnames(stan_data$y_n)
  ) |>
  select(flokkur, mean, q5, q95) |>
  mutate(
    true = c(
      0.017, 0.134, 0.201, 0.073, 0.034, 0.051, 0.199, 0.092, 0.126, 0.063
    )
  ) |>
  mutate(
    flokkur = if_else(
      flokkur == "Annað",
      "Lýðræðisflokkurinn",
      flokkur
    ) |>
      str_to_sentence(),
    flokkur = fct_reorder(flokkur, true)
  )

plot_dat |>
  write_csv(
    here("data", "poll_predictions", "maskina14nov.csv")
  )

p <- plot_dat |>
  ggplot(aes(true, flokkur)) +
  geom_linerange(aes(xmin = q5, xmax = q95, col = "Spá")) +
  geom_point(
    aes(x = mean, col = "Spá", shape = "Spá"),
    size = 4
  ) +
  geom_point(
    aes(col = "Rétt gildi", shape = "Rétt gildi"),
    size = 4
  ) +
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_colour_manual(
    values = c("Rétt gildi" = "black", "Spá" = "gray70")
  ) +
  scale_shape_manual(
    values = c("Rétt gildi" = 16, "Spá" = 15)
  ) +
  theme(
    legend.position = "top"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Spáð fylgi flokka í könnun Maskínu 8. nóvember",
    subtitle = str_c(
      "Gráir kassar eru miðgildi og línur eru 90% öryggisbil fyrir spár | ",
      "Svartir punktar eru rétt gildi"
    ),
    shape = NULL,
    col = NULL
  )
p

ggsave(
  here("Figures", "newest_poll_predictions.png"),
  p,
  width = 8,
  height = 0.5 * 8,
  scale = 1.3,
  bg = "#fdfcfc"
)

ggsave(
  here("Figures", "newest_poll_predictions_transparent.png"),
  p,
  width = 8,
  height = 0.5 * 8,
  scale = 1.3,
  bg = "transparent"
)

#### Gamma / House Effects ####

plot_dat <- fit$summary("gamma") |>
  select(variable, mean, q5, q95) |>
  mutate(
    p = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
    h = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y_n)[-1][p],
    fyrirtaeki = levels(polling_data$fyrirtaeki)[h]
  ) |>
  filter(h != 1)

plot_dat |>
  print(n = 100)

plot_dat |>
  write_csv(
    here("data", "gamma_effects.csv")
  )

p <- plot_dat |>
  bind_rows(
    fit$summary("mu_gamma") |>
      mutate(
        flokkur = colnames(stan_data$y_n)[-c(1, 10)] |>
          as_factor() |>
          fct_reorder(mean),
        fyrirtaeki = "Samtals",
        .before = variable
      )
  ) |>
  filter(fyrirtaeki != "Samtals") |>
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
  coord_cartesian(
    xlim = c(-0.7, 0.7)
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
    title = "Frávik mismunandi fyrirtækja frá metnu fylgi flokka samkvæmt líkani"
  )

p

ggsave(
  here("Figures", "gamma2.png"),
  p,
  width = 8,
  height = 1.2 * 8,
  scale = 1.3
)

p <- fit$draws(c("beta_inc_years_f", "beta_vnv_f", "beta_growth_f")) |>
  as_draws_df() |>
  crossing(
    years = 1:15,
    vnv = c(0, 0.02, 0.05, 0.1),
    growth = c(-0.1, 0, 0.05, 0.1)
  ) |>
  mutate(
    value = beta_inc_years_f * log(years) +
      beta_vnv_f * log(1 + vnv) +
      beta_growth_f * log(1 + growth)
  ) |>
  group_by(years, vnv, growth) |>
  reframe(
    coverage = seq(0.05, 0.95, by = 0.1),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2)
  ) |>
  mutate_at(vars(lower, upper), exp) |>
  mutate(
    Inflation = percent(vnv, accuracy = 0.1) |>
      fct_reorder(vnv),
    Growth = percent(growth, accuracy = 0.1) |>
      fct_reorder(growth)
  ) |>
  arrange(coverage) |>
  ggplot(aes(years, ymin = lower, ymax = upper, fill = coverage, group = coverage)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_ribbon(aes(alpha = -coverage)) +
  scale_x_continuous(
    labels = \(x) number(x, accuracy = 1),
    breaks = breaks_width(width = 2, offset = 1),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = \(x) number(x, accuracy = 0.1),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_alpha_continuous(
    range = c(0.1, 0.4)
  ) +
  facet_grid(
    rows = vars(Growth),
    cols = vars(Inflation),
    labeller = label_both
  ) +
  labs(
    x = "Years as Incumbent",
    y = "Effect on Odds Ratio Scale",
    title = "Effect of Incumbent Years, Inflation and Growth on Vote Share",
    subtitle = "Deeper blues indicate lower posterior coverage"
  ) +
  theme(
    legend.position = "none"
  )

p

ggsave(
  here("Figures", "fundamentals_effect.png"),
  p,
  width = 8,
  height = 0.8 * 8,
  scale = 1.3
)
