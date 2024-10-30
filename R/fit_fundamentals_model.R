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
  R / prepare_data[
    read_polling_data,
    prepare_stan_data,
    read_fundamentals_data
  ]
)

election_date <- date_build(2024, 11, 30)

polling_data <- read_polling_data() |>
  filter(
    date >= clock::date_build(2016, 1, 1)
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

stan_data <- prepare_stan_data(polling_data, fundamentals_data)


str(stan_data)

model <- cmdstan_model(
  here("Stan", "polling_and_fundamentals.stan")
)

init <- list(
  sigma = rep(1, stan_data$P - 1),
  beta_0 = rep(0, stan_data$P - 1),
  z_beta = matrix(0, stan_data$P - 1, stan_data$D + stan_data$pred_y_time_diff),
  mu_gamma = rep(0, stan_data$P - 1),
  sigma_gamma = rep(1, stan_data$P - 1),
  gamma_raw = matrix(0, stan_data$P - 1, stan_data$H - 1),
  phi_inv = 1,
  inv_phi_f = 1,
  alpha_f_raw = rep(0, stan_data$P_f - 1),
  sigma_f = 10
)

fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  init = rep(list(init), 4),
  refresh = 100
)

p <- fit$draws(c("beta_inc_years_f", "beta_inc_f", "beta_vnv_f", "beta_growth_f")) |>
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

# Polling Parameters
fit$summary("beta0")
fit$summary("sigma")

# Fundamentals Parameters
fit$summary("alpha_f")
fit$summary("beta_lag_f")


fit$summary("beta_inc_f")
fit$summary("beta_inc_f") |>
  select(variable, mean, q5, q95) |>
  mutate_at(vars(-variable), exp)

fit$summary("beta_inc_years_f")

fit$summary(c("beta_vnv_f", "beta_growth_f"))

fit$draws(c("beta_vnv_f", "beta_growth_f")) |>
  summarise_draws(
    "perc_less_than_0" = ~ mean(.x < 0)
  )

fit$summary("sigma_f")

fit$summary("alpha_f") |>
  mutate(
    flokkur = rownames(stan_data$y_f)
  ) |>
  select(flokkur, mean) |>
  mutate(
    voteshare = exp(mean) / sum(exp(mean))
  ) |>
  arrange(desc(voteshare))

fit$summary("phi_inv")
fit$summary("phi_f_inv")


tibble(
  y = as.numeric(stan_data$y_f[-1, ]),
  prev = as.numeric(stan_data$x_f[-1, -17]),
  incumbent = as.numeric(stan_data$incumbent_f[-1, -17]),
  vnv = as.numeric(stan_data$vnv[-1, -17])
) |>
  filter(y != 0) |>
  reframe(
    lm(y ~ prev + incumbent + vnv) |>
      broom::tidy(conf.int = TRUE)
  )


dates <- c(
  unique(polling_data$date),
  seq.Date(max(polling_data$date), election_date, by = "day")
)


fit$summary("y_rep", mean, ~ quantile(.x, c(0.05, 0.95))) |>
  rename(q5 = `5%`, q95 = `95%`) |>
  mutate(
    t = str_match(variable, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y)[p],
    dags = dates[t]
  ) |>
  # filter(flokkur != "Annað") |>
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
    flokkur = colnames(stan_data$y)[p],
    dags = dates[t]
  ) |>
  filter(
    dags == max(dags),
    flokkur != "Annað"
  ) |>
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
    flokkur = colnames(stan_data$y)[p],
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
dir.create(here("data", as.character(last_poll_date)), showWarnings = FALSE)
write_parquet(y_rep_draws, here("data", as.character(last_poll_date), "y_rep_draws.parquet"))

theme_set(metill::theme_metill())

p <- fit$summary("gamma") |>
  select(variable, mean, q5, q95) |>
  mutate(
    p = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
    h = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y)[-1][p],
    fyrirtaeki = levels(polling_data$fyrirtaeki)[h]
  ) |>
  filter(h != 1) |>
  bind_rows(
    fit$summary("mu_gamma") |>
      mutate(
        flokkur = colnames(stan_data$y)[-1] |>
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

p

ggsave(
  here("Figures", "gamma.png"),
  p,
  width = 8,
  height = 1.2 * 8,
  scale = 1.3
)
