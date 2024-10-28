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
    prepare_stan_data
  ]
)

election_date <- date_build(2024, 11, 30)

data <- read_polling_data() |>
  filter(
    date >= clock::date_build(2016, 1, 1)
  ) |>
  mutate(
    fyrirtaeki = fct_relevel(
      as_factor(fyrirtaeki),
      "Kosning",
      "Félagsvísindastofnun"
    ),
    flokkur = fct_relevel(
      as_factor(flokkur),
      "Annað",
      "Sjálfstæðisflokkurinn",
      "Samfylkingin",
      "Framsóknarflokkurinn",
      "Vinstri Græn",
      "Píratar",
      "Viðreisn",
      "Flokkur Fólksins",
      "Miðflokkurinn",
      "Sósíalistaflokkurinn"
    )
  ) |>
  arrange(date, fyrirtaeki, flokkur)

stan_data <- prepare_stan_data(data)

n_parties <- data |>
  summarise(
    n_parties = sum(n != 0),
    .by = c(date, fyrirtaeki)
  ) |>
  arrange(date) |>
  pull(n_parties)

stan_data$n_parties <- n_parties

model <- cmdstan_model(
  here("Stan", "base_model.stan")
)

init <- list(
  sigma = rep(1, stan_data$P - 1),
  beta_0 = rep(0, stan_data$P - 1),
  z_beta = matrix(0, stan_data$P - 1, stan_data$D + stan_data$pred_y_time_diff),
  mu_gamma = rep(0, stan_data$P - 1),
  sigma_gamma = rep(0.4, stan_data$P - 1),
  gamma_raw = matrix(0, stan_data$P - 1, stan_data$H - 1),
  phi_inv = 1
)

fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  init = rep(list(init), 4),
  refresh = 100
)


fit$summary("mu_gamma")
fit$summary("sigma_gamma")

mcmc_trace(fit$draws("mu_gamma"))
mcmc_trace(fit$draws("sigma_gamma"))

fit$summary("sigma") |>
  mutate(
    flokkur = colnames(stan_data$y),
    .before = variable
  )

fit$summary("phi_inv")




dates <- c(
  unique(data$date),
  seq.Date(max(data$date), election_date, by = "day")
)


fit$summary("y_rep", mean, ~ quantile(.x, c(0.05, 0.95))) |>
  rename(q5 = `5%`, q95 = `95%`) |>
  mutate(
    t = str_match(variable, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y)[p],
    dags = dates[t]
  ) |>
  filter(flokkur != "Annað") |>
  group_by(dags) |>
  mutate_at(
    vars(mean, q5, q95),
    ~ .x / stan_data$n_pred
  ) |>
  ungroup() |>
  left_join(
    data |>
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
    title = "Spáð fylgi stjórnmálaflokka miðað við nýjustu kannanir"
  ) |>
  tab_footnote(
    md(
      str_c(
        "Matið styðst við kannanir Félagsvísindastofnunar, Gallup, Maskínu og Prósents\n",
        "auk niðurstaðna kosninga frá 2017 og 2021."
      )
    )
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

write_parquet(y_rep_draws, here("data", "y_rep_draws.parquet"))

theme_set(metill::theme_metill())

p <- fit$summary("gamma") |>
  select(variable, mean, q5, q95) |>
  mutate(
    p = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
    h = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(stan_data$y)[-1][p],
    fyrirtaeki = levels(data$fyrirtaeki)[h]
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
