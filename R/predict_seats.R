library(tidyverse)
library(arrow)
library(here)
library(googlesheets4)
library(purrr)
library(scales)

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

  votes_matrix <- draw |>
    pivot_wider(names_from = kjordaemi, values_from = votes) |>
    column_to_rownames("flokkur") |>
    as.matrix()

  available_seats <- seats_tibble |>
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

  available_jofnun <- seats_tibble |>
    arrange(kjordaemi) |>
    pull(n_jofnun)

  total_votes <- rowSums(votes_matrix)
  percent_votes <- total_votes / sum(total_votes)

  total_seats <- rowSums(seats_matrix)

  adjustment_seats <- numeric(9)
  n_adjustment_seats <- numeric(nrow(votes_matrix))
  where_to_assign <- numeric(nrow(votes_matrix))

  for (i in seq_along(adjustment_seats)) {
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

# Breyta í RDS til að deila
read_parquet(here("data", today(), "constituency_predictions.parquet")) |>
  select(.draw, flokkur, kjordaemi, votes = value) |>
  write_rds(here("data", today(), "constituency_predictions.rds"))

draws <- read_parquet(here("data", today(), "constituency_predictions.parquet")) |>
  select(.draw, flokkur, kjordaemi, votes = value) |>
  arrange(kjordaemi, flokkur)

# Til að geta þæginlega labbað í gegnum fallið að ofan
draw <- draws |>
  filter(.draw == 1) |>
  select(-.draw)



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

# dir.create(here("data", today()))

d |>
  write_parquet(here("data", today(), "seats_draws.parquet"))
