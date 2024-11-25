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
    read_constituency_data,
    update_constituency_data
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
# update_constituency_data()
constituency_data <- read_constituency_data() |>
  filter(
    date >= date_build(2021, 9, 25)
    # date < date_build(2024, 10, 1)
  ) |>
  drop_na() |>
  mutate(
    fyrirtaeki = factor(fyrirtaeki, levels = levels(polling_data$fyrirtaeki)),
    flokkur = factor(flokkur, levels = levels(polling_data$flokkur)),
    kjordaemi = as_factor(kjordaemi)
  ) |>
  arrange(flokkur, date, kjordaemi)

constituency_data |>
  distinct(date, fyrirtaeki) |>
  anti_join(
    polling_data |>
      distinct(date, fyrirtaeki)
  )

stan_data <- prepare_stan_data(
  polling_data,
  fundamentals_data,
  constituency_data
)

constituency_weights <- constituency_data |>
  summarise(
    n = sum(n),
    .by = c(kjordaemi, date)
  ) |>
  mutate(
    p = n / sum(n),
    .by = date
  ) |>
  select(-n) |>
  pivot_wider(
    names_from = kjordaemi,
    values_from = p,
    values_fill = 0
  ) |>
  column_to_rownames("date")

N_obs_k <- nrow(constituency_weights)
obs_k <- rep(1:N_obs_k, each = ncol(constituency_weights))


stan_data$constituency_weights <- constituency_weights
stan_data$constituency_weights_pred <- constituency_weights[3, ] |> as.numeric()
stan_data$desired_weight <- 0.22
stan_data$weight_time <- 180
stan_data$N_obs_k <- N_obs_k
stan_data$obs_k <- obs_k

str(stan_data)

model <- cmdstan_model(
  here("Stan", "polling_and_fundamentals_kjordaemi.stan")
)

fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  init = 0,
  iter_warmup = 500,
  iter_sampling = 500
)


#### Polling Parameters ####
#### National Level ####
fit$summary("beta0")
fit$summary("sigma")
fit$summary("tau_stjornarslit")
fit$summary("tau_f")
fit$summary("phi") |>
  mutate(
    fyrirtaeki = levels(polling_data$fyrirtaeki)[-1]
  ) |>
  select(fyrirtaeki, median, q5, q95) |>
  arrange(desc(median))
fit$summary("mu_phi")
fit$summary("sigma_phi")

fit$summary("sigma")

#### Constituency Parameters ####
fit$summary("sigma_delta") |>
  mutate(
    flokkur = levels(constituency_data$flokkur)[-1]
  ) |>
  select(flokkur, mean, q5, q95)

deltas <- fit$summary("delta_raw") |>
  mutate(
    p = str_match(variable, "delta_raw\\[(.*),.*\\]")[, 2] |> parse_number(),
    k = str_match(variable, "delta_raw\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = levels(constituency_data$flokkur)[p + 1],
    kjordaemi = levels(constituency_data$kjordaemi)[k]
  )

deltas |>
  ggplot(aes(mean, flokkur)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  geom_linerange(aes(xmin = q5, xmax = q95)) +
  facet_wrap(
    vars(kjordaemi)
  )

fit$summary("delta_raw") |>
  mutate(
    p = str_match(variable, "delta_raw\\[(.*),.*\\]")[, 2] |> parse_number(),
    k = str_match(variable, "delta_raw\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = levels(constituency_data$flokkur)[p + 1],
    kjordaemi = levels(constituency_data$kjordaemi)[k]
  ) |>
  select(flokkur, kjordaemi, median, q5, q95) |>
  pivot_longer(c(median, q5, q95)) |>
  pivot_wider(
    names_from = kjordaemi,
    values_from = value
  ) |>
  mutate(
    Suður = -rowSums(across(-c(flokkur, name)))
  ) |>
  pivot_longer(
    c(-flokkur, -name),
    names_to = "kjordaemi",
  ) |>
  pivot_wider(
    names_from = name,
    values_from = value
  ) |>
  ggplot(aes(median, kjordaemi)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  geom_linerange(aes(xmin = q5, xmax = q95)) +
  facet_wrap(
    vars(flokkur)
  )


# Fundamentals Parameters
fit$summary("alpha_f")
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
    house, mean, q5, q95
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
  corrplot::corrplot(
    method = "color",
    order = "hclust",
    tl.col = "black",
    addCoef.col = "black"
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
  unique(c(polling_data$date, constituency_data$date)),
  seq.Date(
    max(c(polling_data$date, constituency_data$date)) + 1,
    election_date,
    by = "day"
  )
)

d_yrep_national <- fit$summary("y_rep_national")

d_yrep_national |>
  mutate(
    t = str_match(variable, "y_rep_national\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep_national\\[.*,(.*)\\]")[, 2] |> parse_number(),
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
  filter(dags >= clock::date_build(2024, 8)) |>
  ggplot(aes(dags, mean)) +
  geom_ribbon(
    aes(ymin = q5, ymax = q95, fill = flokkur),
    alpha = 0.2
  ) +
  geom_line(aes(col = flokkur), linewidth = 1) +
  geom_point(aes(y = konnun, col = flokkur))



d_yrep_national |>
  mutate(
    t = str_match(variable, "y_rep_national\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep_national\\[.*,(.*)\\]")[, 2] |> parse_number(),
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
  mutate_at(
    vars(mean, q5, q95),
    ~ round(.x * 200) / 200
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
  fmt_percent(
    decimals = 1,
    drop_trailing_zeros = TRUE
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

y_rep_draws <- fit$draws("y_rep_national") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw)
  ) |>
  mutate(
    t = str_match(name, "y_rep_national\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(name, "y_rep_national\\[.*,(.*)\\]")[, 2] |> parse_number(),
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



write_parquet(y_rep_draws, here("data", as.character(today()), "y_rep_draws_constituency.parquet"))


#### Constituency Y-Rep Draws ####

d <- fit$summary("y_rep_k") |>
  mutate(
    n_k = str_match(variable, "y_rep_k\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep_k\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y_k)[p],
    kjordaemi = levels(constituency_data$kjordaemi)[stan_data$constituency_k[n_k]],
    date = dates[stan_data$date_k[n_k]],
    fyrirtaeki = levels(polling_data$fyrirtaeki)[stan_data$house_k[n_k]]
  ) |>
  select(flokkur, kjordaemi, date, fyrirtaeki, mean, q5, q95)

obs <- d |>
  distinct(date, fyrirtaeki)

for (i in seq_len(nrow(obs))) {
  p <- d |>
    filter(
      fyrirtaeki == obs$fyrirtaeki[i],
      date == obs$date[i]
    ) |>
    inner_join(
      constituency_data,
      by = join_by(flokkur, kjordaemi, date, fyrirtaeki)
    ) |>
    mutate(
      mean = mean / sum(n),
      q5 = q5 / sum(n),
      q95 = q95 / sum(n),
      n = n / sum(n),
      .by = c(kjordaemi)
    ) |>
    ggplot(aes(mean, kjordaemi)) +
    geom_point(
      aes(col = "Predicted", shape = "Predicted"),
      size = 3
    ) +
    geom_point(
      aes(x = n, col = "Observed", shape = "Observed"),
      size = 3
    ) +
    geom_linerange(
      aes(xmin = q5, xmax = q95, col = "Predicted"),
      alpha = 0.5
    ) +
    scale_x_continuous(
      labels = \(x) percent(x, accuracy = 0.1)
    ) +
    facet_wrap(
      vars(flokkur),
      scales = "free_x"
    ) +
    labs(
      shape = "Type",
      col = "Type",
      title = paste(obs$fyrirtaeki[i], obs$date[i])
    )


  p

  ggsave(
    here("Figures", "constituency_predictions", paste0("constituency_predictions_", obs$fyrirtaeki[i], "_", obs$date[i], ".png")),
    p,
    width = 8,
    height = 0.621 * 8,
    scale = 1.5
  )
}

#### Constituency Predictions ####

d_const_draws <- fit$draws("y_pred_constituency") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw)
  ) |>
  mutate(
    k = str_match(name, "y_pred_constituency\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(name, "y_pred_constituency\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = levels(constituency_data$flokkur)[p],
    kjordaemi = levels(constituency_data$kjordaemi)[k]
  ) |>
  select(
    .chain,
    .iteration,
    .draw,
    kjordaemi,
    flokkur,
    value
  )

write_parquet(d_const_draws, here("data", as.character(today()), "constituency_predictions.parquet"))

d_const <- d_const_draws |>
  mutate(
    value = value / sum(value),
    .by = c(.draw, kjordaemi)
  ) |>
  summarise(
    median = median(value),
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .by = c(kjordaemi, flokkur)
  ) |>
  select(flokkur, kjordaemi, median, q5, q95)

p <- d_const |>
  group_by(kjordaemi2 = kjordaemi) |>
  arrange(desc(median)) |>
  group_map(
    \(data, ...)  {
      data |>
        select(-kjordaemi) |>
        gt() |>
        fmt_percent(decimals = 0) |>
        tab_header(
          title = unique(data$kjordaemi)
        ) |>
        patchwork::wrap_table(space = "free")
    }
  ) |>
  patchwork::wrap_plots()

ggsave(
  here("Figures", "constituency_predictions.png"),
  p,
  width = 8,
  height = 0.7 * 8,
  scale = 1.4
)

#### Gamma ####
theme_set(metill::theme_metill())

p <- fit$summary("gamma") |>
  select(variable, mean, q5, q95) |>
  mutate(
    p = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
    h = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y_n)[-1][p],
    fyrirtaeki = levels(polling_data$fyrirtaeki)[h]
  ) |>
  filter(h != 1) |>
  bind_rows(
    fit$summary("mu_gamma") |>
      mutate(
        flokkur = colnames(stan_data$y_n)[-1] |>
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
  here("Figures", "gamma_kjordaemi.png"),
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


constituency_data |>
  filter(
    fyrirtaeki == "Gallup",
    year(date) == 2024
  ) |>
  mutate(
    p = n / sum(n),
    .by = kjordaemi
  ) |>
  mutate(
    weight = constituency_weights[7, ] |> as.numeric(),
    .by = flokkur
  ) |>
  summarise(
    national_p = sum(p * weight),
    .by = flokkur
  )

polling_data |>
  filter(
    fyrirtaeki == "Gallup",
    date == ymd("2024-09-15")
  ) |>
  mutate(
    p = n / sum(n)
  )
