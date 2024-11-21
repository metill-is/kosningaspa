library(tidyverse)
library(here)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(gt)
library(gtExtras)
library(arrow)
library(scales)
# read data
gallup_data <- read_csv(here("data", "gallup_data.csv"))
maskina_data <- read_csv(here("data", "maskina_data.csv"))
prosent_data <- read_csv(here("data", "prosent_data.csv"))
election_data <- read_csv(here("data", "election_data.csv"))
# combine data
data <- bind_rows(
  maskina_data,
  prosent_data,
  gallup_data,
  election_data
) |>
  mutate(
    flokkur = if_else(flokkur == "Lýðræðisflokkurinn", "Annað", flokkur),
    fyrirtaeki = fct_relevel(
      as_factor(fyrirtaeki),
      "Kosning"
    )
  ) |>
  filter(
    date >= clock::date_build(2021, 1, 1),
    flokkur != "Annað"
  ) |>
  arrange(date, fyrirtaeki, flokkur)

election_date <- clock::date_build(2024, 11, 30)

D <- as.numeric(election_date - min(data$date))
P <- length(unique(data$flokkur))
H <- length(unique(data$fyrirtaeki))
N <- data |>
  distinct(fyrirtaeki, date) |>
  nrow()


y <- data |>
  select(date, fyrirtaeki, flokkur, n) |>
  mutate(
    n = as.integer(n)
  ) |>
  pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
  select(-date, -fyrirtaeki) |>
  as.matrix()

house <- data |>
  pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
  mutate(
    house = as.numeric(factor(fyrirtaeki))
  ) |>
  pull(house)

date <- data |>
  pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
  mutate(
    factor_date = factor(
      date,
      levels = seq.Date(
        from = min(date),
        to = election_date,
        by = "day"
      )
    ),
    date = as.numeric(factor_date)
  ) |>
  pull(date)

time_diff <- data |>
  distinct(date) |>
  arrange(date) |>
  mutate(
    time_diff = c(NA, diff(date))
  ) |>
  drop_na() |>
  pull(time_diff) |>
  as.numeric()

max_date <- max(data$date)

pred_y_time_diff <- as.numeric(election_date - max_date)
stjornarslit <- data |>
  pivot_wider(names_from = flokkur, values_from = n, values_fill = 0) |>
  mutate(
    stjornarslit = 1 * (date >= clock::date_build(2024, 10, 14))
  ) |>
  pull(stjornarslit)

stan_data <- list(
  D = D,
  P = P,
  H = H,
  N = N,
  y = y,
  house = house,
  date = date,
  time_diff = time_diff,
  pred_y_time_diff = pred_y_time_diff,
  stjornarslit = stjornarslit,
  n_pred = as.integer(sum(election_data$n)),
  sigma_house = 0.01,
  trend_damping = 0.8
)

model <- cmdstan_model(
  here("Stan", "base_model_rw2.stan")
)

init <- list(
  sigma_beta = 0.005,
  beta_0 = rep(0, P),
  beta_1 = rep(0, P),
  z_beta = matrix(0, P, D - 2),
  gamma_raw = matrix(0, P, H - 2),
  phi_inv = 1
)

fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  init = rep(list(init), 4),
  refresh = 10,
  iter_warmup = 500,
  iter_sampling = 500
)

fit$summary("beta_0") |>
  mutate(
    flokkur = colnames(y),
    .before = variable
  )

mcmc_trace(
  fit$draws("beta_0")
)

fit$summary("beta_1") |>
  mutate(
    flokkur = colnames(y),
    .before = variable
  )

fit$summary("sigma_beta") |>
  mutate(
    flokkur = colnames(y),
    .before = variable
  )

mcmc_trace(
  fit$draws("sigma_beta"),
  pars = "sigma_beta"
)

fit$summary("sigma_eps_beta") |>
  mutate(
    flokkur = colnames(y),
    .before = variable
  )

mcmc_trace(
  fit$draws("sigma_beta"),
  pars = "sigma_beta[1]"
)



dates <- c(
  seq.Date(from = min(data$date), to = election_date, by = "day")
)

plot_dat <- fit$summary("y_rep", mean, ~ quantile(.x, c(0.05, 0.95))) |>
  rename(q5 = `5%`, q95 = `95%`) |>
  mutate(
    t = str_match(variable, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(variable, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(y)[p],
    dags = dates[t]
  )


p <- plot_dat |>
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
  geom_line(aes(col = flokkur), linewidth = 1) +
  geom_point(aes(y = konnun, col = flokkur))
p

ggsave(
  here("Figures", "y_rep.png"),
  p,
  width = 8,
  height = 0.621 * 8,
  scale = 1.5
)


fit$draws("y_rep") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    c(-.chain, -.iteration, -.draw)
  ) |>
  mutate(
    t = str_match(name, "y_rep\\[(.*),.*\\]")[, 2] |> parse_number(),
    p = str_match(name, "y_rep\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(y)[p],
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
    ref_line = 0,
    text_size = 0,
    width = 30
  ) |>
  tab_header(
    title = "Spáð fylgi stjórnmálaflokka á kosningadag"
  ) |>
  tab_footnote(
    md(
      str_c(
        "Matið styðst við kannanir Félagsvísindastofnunar, Gallup, Maskínu og Prósents\n",
        "auk niðurstaðna kosninga frá 2021."
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
    flokkur = colnames(y)[p],
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

p <- mcmc_trace(
  fit$draws("gamma"),
  pars = "gamma[2,2]"
)

ggsave(
  here("Figures", "gamma_trace.png"),
  p,
  width = 8,
  height = 0.621 * 8,
  scale = 1.5
)

p <- fit$summary("gamma") |>
  select(variable, mean, q5, q95, rhat) |>
  mutate(
    p = str_match(variable, "gamma\\[(.*),.*\\]")[, 2] |> parse_number(),
    h = str_match(variable, "gamma\\[.*,(.*)\\]")[, 2] |> parse_number(),
    flokkur = colnames(y)[p],
    fyrirtaeki = levels(data$fyrirtaeki)[h]
  ) |>
  filter(h != 1) |>
  ggplot(aes(0, flokkur, col = fyrirtaeki)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(aes(x = mean, y = flokkur)) +
  geom_segment(
    aes(x = q5, xend = q95, y = flokkur, yend = flokkur),
    alpha = 0.5
  ) +
  scale_colour_brewer(
    palette = "Set1"
  )

p

ggsave(
  here("Figures", "gamma.png"),
  p,
  width = 8,
  height = 0.621 * 8,
  scale = 1.5
)
