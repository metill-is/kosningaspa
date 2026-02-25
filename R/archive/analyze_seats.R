library(tidyverse)
library(arrow)
library(here)
library(metill)
library(ggh4x)
theme_set(theme_metill())
d <- read_parquet(here("data", "seats_draws.parquet"))

d |>
  mutate(
    flokkur = if_else(
      flokkur %in% c(
        "Framsóknarflokkurinn",
        "Samfylkingin",
        "Viðreisn"
      ),
      "B+C+S",
      "Annað"
    )
  ) |>
  summarise(
    seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  filter(flokkur != "Annað") |>
  count(seats) |>
  mutate(
    p = n / sum(n),
    tegund = if_else(
      seats >= 32,
      "Meirihluti",
      "Minnihluti"
    )
  ) |>
  ggplot(aes(seats, p, fill = tegund)) +
  geom_col() +
  scale_x_continuous(
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = "Fjöldi sæta",
    y = "Líkur",
    title = "B + C + S"
  )

d |>
  mutate(
    flokkur = if_else(
      flokkur %in% c(
        "Framsóknarflokkurinn",
        "Samfylkingin",
        "Viðreisn",
        "Píratar"
      ),
      "B+C+S+P",
      "Annað"
    )
  ) |>
  summarise(
    seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  filter(flokkur != "Annað") |>
  count(seats) |>
  mutate(
    p = n / sum(n),
    tegund = if_else(
      seats >= 32,
      "Meirihluti",
      "Minnihluti"
    )
  ) |>
  ggplot(aes(seats, p, fill = tegund)) +
  geom_col() +
  scale_x_continuous(
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = "Fjöldi sæta",
    y = "Líkur",
    title = "B + C + S + P"
  )


d |>
  mutate(
    flokkur = if_else(
      flokkur %in% c(
        "Sjálfstæðisflokkurinn",
        "Miðflokkurinn",
        "Viðreisn"
      ),
      "C+D+M",
      "Annað"
    )
  ) |>
  summarise(
    seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  filter(flokkur != "Annað") |>
  count(seats) |>
  mutate(
    p = n / sum(n),
    tegund = if_else(
      seats >= 32,
      "Meirihluti",
      "Minnihluti"
    )
  ) |>
  ggplot(aes(seats, p, fill = tegund)) +
  geom_col() +
  scale_x_continuous(
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = "Fjöldi sæta",
    y = "Líkur",
    title = "C+D+M"
  )


d |>
  mutate(
    flokkur = if_else(
      flokkur %in% c(
        "Samfylkingin",
        "Viðreisn",
        "Flokkur Fólksins"
      ),
      "B+C+S+P",
      "Annað"
    )
  ) |>
  summarise(
    seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  filter(flokkur != "Annað") |>
  count(seats) |>
  mutate(
    p = n / sum(n),
    tegund = if_else(
      seats >= 32,
      "Meirihluti",
      "Minnihluti"
    )
  ) |>
  ggplot(aes(seats, p, fill = tegund)) +
  geom_col() +
  scale_x_continuous(
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = "Fjöldi sæta",
    y = "Líkur",
    title = "C + F + S"
  )


d |>
  mutate(
    flokkur = if_else(
      flokkur %in% c(
        "Sjálfstæðisflokkurinn",
        "Miðflokkurinn",
        "Flokkur Fólksins"
      ),
      "D+F+M",
      "Annað"
    )
  ) |>
  summarise(
    seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  filter(flokkur != "Annað") |>
  count(seats) |>
  mutate(
    p = n / sum(n),
    tegund = if_else(
      seats >= 32,
      "Meirihluti",
      "Minnihluti"
    )
  ) |>
  ggplot(aes(seats, p, fill = tegund)) +
  geom_col() +
  scale_x_continuous(
    breaks = breaks_width(1),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = "Fjöldi sæta",
    y = "Líkur",
    title = "D+F+M"
  )
