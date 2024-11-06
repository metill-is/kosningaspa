library(tidyverse)
library(arrow)
library(here)
library(googlesheets4)
library(purrr)
library(scales)

box::use(
  R / election_utils[calculate_seats, seats_tibble, dhondt, jofnunarsaeti]
)

seats_tibble <- tribble(
  ~kjordaemi, ~n_seats, ~n_jofnun,
  "Reykjavík Suður", 9, 2,
  "Reykjavík Norður", 9, 2,
  "Suðvestur", 11, 2,
  "Suður", 9, 1,
  "Norðaustur", 9, 1,
  "Norðvestur", 7, 1
)

calculate_seats <- function(draw) {
  box::use(
    dplyr[arrange, pull],
    tidyr[pivot_wider],
    tibble[as_tibble, column_to_rownames]
  )

  votes_matrix <- draw |>
    pivot_wider(names_from = kjordaemi, values_from = votes) |>
    column_to_rownames("flokkur") |>
    as.matrix()

  available_seats <- seats_tibble() |>
    arrange(kjordaemi) |>
    pull(n_seats)

  seats_matrix <- matrix(
    0,
    nrow = nrow(votes_matrix),
    ncol = ncol(votes_matrix)
  )

  colnames(seats_matrix) <- colnames(votes_matrix)
  rownames(seats_matrix) <- rownames(votes_matrix)

  for (i in seq_along(available_seats)) {
    n_seats <- available_seats[i]
    party_seats <- numeric(nrow(votes_matrix))
    temp_votes <- votes_matrix[, i]

    while (sum(party_seats) < n_seats) {
      which_max <- which.max(temp_votes)
      party_seats[which_max] <- party_seats[which_max] + 1
      temp_votes[which_max] <- votes_matrix[which_max, i] / (party_seats[which_max] + 1)
    }

    seats_matrix[, i] <- party_seats
  }

  kjordaemi_seats <- seats_matrix

  available_jofnun <- seats_tibble() |>
    arrange(kjordaemi) |>
    pull(n_jofnun)

  total_votes <- rowSums(votes_matrix)
  percent_votes <- total_votes / sum(total_votes)
  percent_votes[percent_votes < 0.05] <- 0

  total_seats <- rowSums(seats_matrix)
  seat_entitlement <- percent_votes * 63

  adjustment_seats <- numeric(9)
  n_adjustment_seats <- numeric(9)
  where_to_assign <- numeric(9)

  for (i in seq_along(adjustment_seats)) {
    seat_resid <- seat_entitlement - total_seats - n_adjustment_seats

    which_max <- which.max((total_votes * (percent_votes >= 0.05)) / (total_seats + n_adjustment_seats + 1))

    adjustment_seats[i] <- which_max
    n_adjustment_seats[which_max] <- n_adjustment_seats[which_max] + 1

    resid_votes_matrix <- apply(
      votes_matrix / (seats_matrix + 1),
      MARGIN = 2,
      FUN = function(x) x / sum(x)
    )

    seat_placement <- resid_votes_matrix[which_max, ] |>
      order(decreasing = TRUE)

    j <- 1
    keep_going <- TRUE
    while (keep_going) {
      place <- seat_placement[j]
      if (available_jofnun[place] > 0) {
        seats_matrix[which_max, place] <- seats_matrix[which_max, place] + 1
        available_jofnun[place] <- available_jofnun[place] - 1
        keep_going <- FALSE
        where_to_assign[i] <- place
      }
      j <- j + 1
    }
  }


  seats_matrix |>
    as_tibble(rownames = "flokkur") |>
    pivot_longer(
      cols = -flokkur,
      names_to = "kjordaemi",
      values_to = "seats"
    ) |>
    mutate(
      kjordaemi_seats = kjordaemi_seats |>
        as_tibble(rownames = "flokkur") |>
        pivot_longer(
          cols = -flokkur,
          names_to = "kjordaemi",
          values_to = "value"
        ) |>
        pull(value)
    )
}

electorate_byagearea <- read_csv(here("data", "electorate_byagearea.csv")) |>
  count(kjordaemi, wt = n_kjosendur, name = "n_kjosendur")

gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))

kjordaemi <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4",
  sheet = "kjordaemi"
) |>
  filter(Fyrirtæki == "Kosning") |>
  select(-Ár, -Mánuður, -Dagur, -Fyrirtæki) |>
  pivot_longer(
    cols = -Flokkur,
    names_to = "kjordaemi",
    values_to = "p_kjordaemi"
  ) |>
  janitor::clean_names() |>
  mutate(
    p_kjordaemi = p_kjordaemi / sum(p_kjordaemi),
    .by = flokkur
  )

draws <- read_parquet(here("data", today() - 1, "y_rep_draws.parquet")) |>
  filter(
    dags == max(dags)
  ) |>
  select(.draw, flokkur, value) |>
  inner_join(
    kjordaemi,
    by = join_by(flokkur)
  ) |>
  inner_join(
    electorate_byagearea,
    by = join_by(kjordaemi)
  ) |>
  mutate(
    value = value * p_kjordaemi * n_kjosendur
  ) |>
  summarise(
    votes = sum(value),
    .by = c(.draw, flokkur, kjordaemi)
  ) |>
  arrange(kjordaemi, flokkur)

d <- draws |>
  group_by(.draw) |>
  group_modify(~ calculate_seats(.x)) |>
  ungroup()

p <- d |>
  summarise(
    total_seats = sum(seats),
    kjordaemi_seats = sum(kjordaemi_seats),
    jofnunar_seats = total_seats - kjordaemi_seats,
    .by = c(flokkur, .draw)
  ) |>
  pivot_longer(contains("seats")) |>
  count(flokkur, name, value) |>
  mutate(
    p = n / sum(n),
    .by = c(flokkur, name)
  ) |>
  ggplot(aes(value, p)) +
  geom_col() +
  scale_x_continuous(
    breaks = breaks_width(1)
  ) +
  facet_wrap(
    vars(flokkur, name),
    scales = "free",
    ncol = 3
  )

ggsave(
  plot = p,
  filename = "Figures/seats_dist.png",
  width = 8,
  height = 1.5 * 8,
  scale = 1.3
)

dir.create(here("data", today()))

d |>
  write_parquet(here("data", today(), "seats_draws.parquet"))



d <- read_parquet(here("data", today() - 1, "y_rep_draws.parquet")) |>
  filter(
    dags == max(dags)
  ) |>
  select(.draw, flokkur, value) |>
  inner_join(
    kjordaemi,
    by = join_by(flokkur)
  )


d |>
  mutate(
    kjor_fylgi = value * p_kjordaemi
  ) |>
  mutate(
    kjor_fylgi = kjor_fylgi / sum(kjor_fylgi),
    .by = c(.draw, kjordaemi)
  ) |>
  summarise(
    mean = mean(kjor_fylgi),
    lower = quantile(kjor_fylgi, 0.05),
    upper = quantile(kjor_fylgi, 0.95),
    .by = c(flokkur, kjordaemi)
  ) |>
  group_by(kjordaemi2 = kjordaemi) |>
  arrange(desc(mean)) |>
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
