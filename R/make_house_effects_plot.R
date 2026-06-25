# House- and industry-bias plots from the newest polling_watch_v4 fit.
# Three views of the same gamma / mu_gamma draws written by fit_polling_watch.R:
#   1. logit scale (model-native)                -> Figures/polling_watch_house_effects.png
#   2. pp vs 2024 election result (fixed p0)      -> Figures/polling_watch_house_effects_pp.png
#   3. pp vs current fylgisvakt, PER DRAW         -> Figures/polling_watch_house_effects_pp_current.png
# pp views (Agnar: use a real baseline, not equal support). Per draw:
#   p_biased = softmax(log(p0) + bias);  diff = p_biased - p0.
# Version 3 pairs each draw's own latest pi_smooth with that draw's bias, i.e.
#   softmax(log pi_smooth + gamma) = softmax(beta + gamma) = the model's expected
#   poll for the house minus the latent truth -> baseline & bias uncertainty stay joint.
library(tidyverse)
library(metill)
library(here)
library(arrow)
library(glue)
library(ggtext)
library(scales)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

theme_set(theme_metill(type = "blog"))

box::use(
  R / party_utils[party_tibble],
  R / data[read_polling_data]
)

format_date <- function(date) {
  clock::date_format(date, format = "%d. %B %Y", locale = clock::clock_locale("is")) |>
    str_replace("^0", "")
}

industry_label <- "Sameiginleg skekkja (öll fyrirtæki)"

caption <- str_c(
  "Mynd frá kannanavaktinni á www.metill.is", "\n",
  "Húsáhrif (gamma) og sameiginleg skekkja (mu_gamma) úr polling_watch_v4 líkaninu"
)

# Locate the most recent fit that persisted house-effect draws.
available <- list.dirs(here("data"), recursive = FALSE, full.names = FALSE)
available <- sort(available[str_detect(available, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")], decreasing = TRUE)
output_date <- NULL
for (d in available) {
  if (file.exists(here("data", d, "polling_watch_gamma.parquet"))) {
    output_date <- d
    break
  }
}
stopifnot(
  "No polling_watch_gamma.parquet found - run fit_polling_watch.R first." = !is.null(output_date)
)

colors <- party_tibble() |>
  select(flokkur, litur)

gamma_draws <- read_parquet(here("data", output_date, "polling_watch_gamma.parquet"))
mu_draws <- read_parquet(here("data", output_date, "polling_watch_mu_gamma.parquet"))
pi_draws <- read_parquet(here("data", output_date, "polling_watch_draws.parquet"))

# --- Baselines for the percentage-point views ---
# (a) most recent election result: a single fixed simplex.
election_rows <- {
  pre_election <- read_polling_data() |>
    filter(date >= clock::date_build(2021, 9, 25))
  post_election_path <- here("data", "post_election_polls.csv")
  post_election <- if (file.exists(post_election_path)) {
    read_csv(post_election_path, show_col_types = FALSE)
  } else {
    tibble()
  }
  bind_rows(pre_election, post_election) |>
    filter(fyrirtaeki == "Kosning") |>
    filter(date == max(date))
}
election_date <- unique(election_rows$date)
baseline_election <- election_rows |>
  transmute(flokkur, p0 = n / sum(n))

# (b) current fylgisvakt: the latest latent support in *each* posterior draw.
current_date <- max(pi_draws$dags)
baseline_current_draws <- pi_draws |>
  filter(dags == current_date) |>
  transmute(.draw, flokkur, p0 = value)

# --- logit-scale summaries (model-native) ---
logit_summary <- function(draws, by) {
  draws |>
    summarise(median = median(value), q5 = quantile(value, 0.05), q95 = quantile(value, 0.95), .by = {{ by }})
}
gamma_sum <- logit_summary(gamma_draws, c(fyrirtaeki, flokkur))
mu_sum <- logit_summary(mu_draws, flokkur) |>
  mutate(fyrirtaeki = industry_label)

# --- percentage-point summaries: apply the bias to a baseline, per draw ---
# `join_by` = "flokkur" for a fixed baseline; c(".draw","flokkur") to pair each
# draw's bias with that same draw's baseline support.
pp_summary <- function(base, join_by) {
  softmax_diff <- function(df, grp) {
    df |>
      inner_join(base, by = join_by) |>
      mutate(num = p0 * exp(value)) |>
      mutate(p_biased = num / sum(num), .by = all_of(grp)) |>
      mutate(diff = p_biased - p0) |>
      summarise(median = median(diff), q5 = quantile(diff, 0.05), q95 = quantile(diff, 0.95),
                .by = all_of(setdiff(c(grp, "flokkur"), ".draw")))
  }
  list(
    gamma = softmax_diff(gamma_draws, c(".draw", "fyrirtaeki")),
    mu = softmax_diff(mu_draws, ".draw") |> mutate(fyrirtaeki = industry_label)
  )
}
pp_elec <- pp_summary(baseline_election, "flokkur")
pp_cur <- pp_summary(baseline_current_draws, c(".draw", "flokkur"))

facet_levels <- c(industry_label, sort(setdiff(unique(gamma_sum$fyrirtaeki), industry_label)))

party_order_of <- function(mu_tbl) {
  mu_tbl |> filter(flokkur != "Annað") |> arrange(median) |> pull(flokkur)
}

make_forest <- function(gamma_tbl, mu_tbl, party_order, x_scale, title, subtitle, xlab) {
  bind_rows(gamma_tbl, mu_tbl) |>
    filter(flokkur %in% party_order) |>
    inner_join(colors, by = "flokkur") |>
    mutate(
      flokkur = factor(flokkur, levels = party_order),
      flokkur_lab = glue("<b style='color:{litur}'>{flokkur}</b>"),
      flokkur_lab = fct_reorder(flokkur_lab, as.integer(flokkur)),
      fyrirtaeki = factor(fyrirtaeki, levels = facet_levels)
    ) |>
    ggplot(aes(x = median, y = flokkur_lab, colour = litur)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
    geom_linerange(aes(xmin = q5, xmax = q95), linewidth = 1) +
    geom_point(size = 2.4) +
    scale_colour_identity() +
    x_scale +
    facet_wrap(~fyrirtaeki, ncol = 3) +
    theme(
      legend.position = "none",
      axis.text.y = element_markdown(size = 11),
      panel.spacing.x = unit(1.4, "lines"),
      panel.spacing.y = unit(1, "lines"),
      strip.text = element_text(face = "bold")
    ) +
    labs(x = xlab, y = NULL, title = title, subtitle = subtitle, caption = caption)
}

n_rows <- ceiling(length(facet_levels) / 3)
fig_h <- 1.8 + 1.9 * n_rows
pp_scale <- function() {
  scale_x_continuous(labels = scales::label_number(scale = 100, accuracy = 1),
                     guide = ggh4x::guide_axis_truncated())
}
save_fig <- function(p, file) {
  ggsave(p, filename = here("Figures", file), width = 12, height = fig_h, bg = "#fdfcfc")
}

p_logit <- make_forest(
  gamma_sum, mu_sum, party_order_of(mu_sum),
  scale_x_continuous(guide = ggh4x::guide_axis_truncated()),
  "Skekkja kannanafyrirtækja eftir flokkum",
  glue("Húsáhrif úr kannanavaktinni (polling_watch_v4), keyrt {format_date(as.Date(output_date))}.\n",
       "Fyrsti reitur: sameiginleg skekkja allra fyrirtækja. Strik = 90% óvissubil."),
  "Skekkja á logit-kvarða  (0 = engin kerfisbundin skekkja; jákvætt = ofmetur flokkinn)"
)
save_fig(p_logit, "polling_watch_house_effects.png")

p_pp_elec <- make_forest(
  pp_elec$gamma, pp_elec$mu, party_order_of(pp_elec$mu),
  pp_scale(),
  "Skekkja kannanafyrirtækja í prósentustigum",
  glue("Húsáhrifum beitt á kosningaúrslitin {format_date(election_date)} sem grunnlínu.\n",
       "Frávik = skekkjuð könnun - raunúrslit (prósentustig). Strik = 90% óvissubil."),
  "Frávik frá raunúrslitum (prósentustig); jákvætt = fyrirtækið ofmetur flokkinn"
)
save_fig(p_pp_elec, "polling_watch_house_effects_pp.png")

p_pp_cur <- make_forest(
  pp_cur$gamma, pp_cur$mu, party_order_of(pp_cur$mu),
  pp_scale(),
  "Skekkja kannanafyrirtækja í prósentustigum",
  glue("Húsáhrifum beitt á fylgi fylgisvaktarinnar ({format_date(current_date)}) í hverjum pósteríordrætti.\n",
       "Frávik = vænt könnun - rétt fylgi (prósentustig). Strik = 90% óvissubil."),
  "Frávik frá réttu fylgi (prósentustig); jákvætt = fyrirtækið ofmetur flokkinn"
)
save_fig(p_pp_cur, "polling_watch_house_effects_pp_current.png")

cat("Wrote logit + pp(election) + pp(current, per-draw) plots from data/", output_date, "\n", sep = "")
