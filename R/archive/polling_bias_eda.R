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
    read_polling_data
  ]
)

election_date <- date_build(2024, 11, 30)

data <- read_polling_data() |>
  filter(
    date >= clock::date_build(2016, 1, 1)
  )

p <- data |>
  mutate(
    p = n / sum(n),
    .by = c(date, fyrirtaeki)
  ) |>
  filter(
    flokkur == "Vinstri Græn"
  ) |>
  select(-n) |>
  mutate(
    which_election = cumsum(fyrirtaeki == "Kosning"),
    which_election = which_election - (fyrirtaeki == "Kosning")
  ) |>
  filter(which_election <= 2) |>
  mutate(
    election_date = date[fyrirtaeki == "Kosning"],
    p_true = p[fyrirtaeki == "Kosning"],
    .by = which_election
  ) |>
  mutate(
    time_to_election = election_date - date,
    error = p - p_true,
    which_election = c(2016, 2017, 2021)[which_election + 1]
  ) |>
  filter(
    fyrirtaeki != "Kosning",
    time_to_election <= 50
  ) |>
  ggplot(aes(-time_to_election, error)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_line(
    data = ~ rename(.x, fr = fyrirtaeki),
    alpha = 0.2,
    aes(group = fr)
  ) +
  geom_point(
    data = ~ rename(.x, fr = fyrirtaeki),
    alpha = 0.1,
    aes(group = fr)
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    labels = function(x) number(abs(x)),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1, suffix = "%-stig"),
    guide = ggh4x::guide_axis_truncated()
  ) +
  facet_grid(
    rows = vars(which_election),
    cols = vars(fyrirtaeki)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Villa í könnunum eftir því hversu langt var til kosninga",
    subtitle = "Vinstri Græn"
  )

p

ggsave(
  here("Figures", "xv_error.png"),
  p,
  width = 8,
  height = 0.621 * 8,
  scale = 1.5
)
