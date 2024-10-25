library(tidyverse)
library(googlesheets4)
library(arrow)

dhondt <- function(votes, n_seats = 63, return_seats = TRUE) {
  n_seats <- unique(n_seats)
  party_seats <- numeric(length(votes))
  temp_votes <- votes

  while (sum(party_seats) < n_seats) {
    which_max <- which.max(temp_votes)
    party_seats[which_max] <- party_seats[which_max] + 1
    temp_votes[which_max] <- votes[which_max] / (party_seats[which_max] + 1)
  }

  if (return_seats) {
    return(party_seats)
  }
  temp_votes
}

jofnunarsaeti <- function(seats, votes, n_seats = 9) {
  n_seats <- 9
  assigned_seats <- 0
  jofnun_seats <- numeric(length(seats))
  perc_votes <- votes / sum(votes)
  too_low <- perc_votes < 0.05
  votes[too_low] <- 0
  while (assigned_seats < n_seats) {
    diff <- votes / seats
    which_max <- which.max(diff)
    seats[which_max] <- seats[which_max] + 1
    jofnun_seats[which_max] <- jofnun_seats[which_max] + 1
    assigned_seats <- assigned_seats + 1
  }

  jofnun_seats
}

seats_tibble <- tribble(
  ~kjordaemi, ~n_seats, ~n_jofnun,
  "Reykjavík Suður", 9, 2,
  "Reykjavík Norður", 9, 2,
  "Suðvestur", 11, 2,
  "Suður", 9, 1,
  "Norðaustur", 9, 1,
  "Norðvestur", 7, 1
)

gs4_auth()

electorate_byagearea <- read_csv("data/electorate_byagearea.csv")
maskina_aldur <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4",
  sheet = "maskina_aldur"
) |>
  select(-Ár, -Mánuður) |>
  pivot_longer(
    cols = -Flokkur,
    names_to = "aldur",
    values_to = "p_aldur"
  ) |>
  janitor::clean_names() |>
  mutate(
    p_aldur = p_aldur / sum(p_aldur),
    .by = flokkur
  )

maskina_kjordaemi <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yEn5feIiltc4kWC61q57sGg_CpMCkxVFLIK9ySYxWQ4",
  sheet = "maskina_kjordaemi"
) |>
  select(-Ár, -Mánuður) |>
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

y_rep <- read_parquet("data/y_rep_draws.parquet") |>
  filter(dags == max(dags))


d <- y_rep |>
  filter(.draw <= 1) |>
  mutate(
    value = value / sum(value),
    .by = c(.draw, .iteration, .chain)
  ) |>
  inner_join(
    maskina_kjordaemi,
    by = join_by(flokkur)
  ) |>
  inner_join(
    maskina_aldur,
    by = join_by(flokkur)
  ) |>
  inner_join(
    electorate_byagearea,
    by = join_by(kjordaemi, aldur)
  ) |>
  mutate(
    value = value * p_aldur * p_kjordaemi * n_kjosendur
  )

d |>
  summarise(
    votes = sum(value),
    .by = c(.draw, flokkur, kjordaemi)
  ) |>
  nest(data = c(flokkur, votes)) |>
  inner_join(seats_tibble) |>
  mutate(
    data = pmap(
      list(data, n_seats),
      function(data, n_seats, n_jofnun) {
        votes <- data$votes
        party_seats <- numeric(length(votes))
        temp_votes <- votes

        while (sum(party_seats) < n_seats) {
          which_max <- which.max(temp_votes)
          party_seats[which_max] <- party_seats[which_max] + 1
          temp_votes[which_max] <- votes[which_max] / (party_seats[which_max] + 1)
        }

        data <- data |>
          mutate(
            assigned_seats = party_seats,
            resid_votes = temp_votes
          )
      }
    )
  ) |>
  select(-n_seats) |>
  unnest(data) |>
  mutate(
    country_votes = sum(votes),
    country_seats = sum(assigned_seats),
    .by = c(.draw, flokkur)
  ) |>
  nest(
    data = c(-.draw, -flokkur, -country_votes, -country_seats)
  ) |>
  mutate(
    assigned_jofnun_total = jofnunarsaeti(country_seats, country_votes),
    .by = .draw
  ) |>
  unnest(data) |>
  mutate(
    resid_perc_votes = resid_votes / sum(resid_votes),
    .by = c(kjordaemi)
  )
nest(data = c(-.draw)) |>
  mutate(
    data = map(
      data,
      function(data) {
        assigned_jofnun_total <- data$assigned_jofnun_total
        while (any(assigned_jofnun_total > 0)) {
          data |>
            mutate(
              resid_perc_votes = resid_votes / sum(resid_votes),
              .by = c(kjordaemi)
            ) |>
            mutate(
              assigned_jofnun = 1 * (assigned_jofnun_total > 0) * (resid_perc_votes == max(resid_perc_votes)),
              .by = flokkur
            )
        }
      }
    )
  )
mutate(
  resid_perc_votes = resid_votes / sum(resid_votes),
  .by = c(.draw, kjordaemi)
) |>
  mutate(
    assigned_jofnun = 1 * (assigned_jofnun_total > 0) * (resid_perc_votes == max(resid_perc_votes)),
    .by = c(.draw, flokkur)
  ) |>
  View()


plot_dat <- d |>
  summarise(
    value = sum(value),
    .by = c(flokkur, .draw, kjordaemi)
  ) |>
  inner_join(
    seats_tibble,
    by = join_by(kjordaemi)
  ) |>
  mutate(
    seats = dhondt(value, n_seats),
    .by = c(.draw, kjordaemi)
  ) |>
  summarise(
    total_votes = sum(value),
    total_seats = sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  mutate(
    jofnun_seats = jofnunarsaeti(total_seats, total_votes),
    seats = total_seats + jofnun_seats,
    .by = c(.draw)
  ) |>
  summarise(
    mean_seats = mean(seats),
    mean_raw_seats = mean(total_seats),
    mean_votes = mean(total_votes),
    .by = flokkur
  ) |>
  mutate(
    p_votes = mean_votes / sum(mean_votes)
  ) |>
  mutate(
    flokkur = fct_reorder(flokkur, p_votes)
  )

plot_dat |>
  ggplot(aes(p_votes, flokkur)) +
  geom_point(
    aes(
      x = p_votes,
      col = "Eftir vigtun",
      shape = "Eftir vigtun"
    ),
    size = 2.5
  ) +
  geom_point(
    aes(
      x = p_raw_votes,
      col = "Fyrir vigtun"
    ),
    size = 2.5
  ) +
  geom_point(
    aes(
      x = p_kjosendur_votes,
      col = "Kjósendur",
      shape = "Kjósendur"
    ),
    size = 2.5,
    position = position_jitter(height = 0.3, seed = 1)
  )


plot_dat |>
  mutate(
    flokkur = fct_reorder(flokkur, mean_seats)
  ) |>
  ggplot(aes(mean_seats, flokkur)) +
  geom_point(
    aes(
      x = mean_seats,
      col = "Eftir jöfnun"
    ),
    size = 3
  ) +
  geom_point(
    aes(
      x = mean_raw_seats,
      col = "Fyrir jöfnun"
    ),
    size = 3
  )


d |>
  summarise(
    value = sum(value),
    .by = c(flokkur, .draw, kjordaemi)
  ) |>
  inner_join(
    seats_tibble,
    by = join_by(kjordaemi)
  ) |>
  mutate(
    seats = dhondt(value, n_seats),
    .by = c(.draw, kjordaemi)
  ) |>
  mutate(
    p_seats = seats / sum(seats),
    .by = c(.draw, flokkur)
  ) |>
  summarise(
    seats = mean(seats),
    p_seats = mean(p_seats),
    .by = c(flokkur, kjordaemi)
  ) |>
  mutate(
    flokkur = fct_reorder(flokkur, seats, sum)
  ) |>
  ggplot(aes(seats, flokkur, fill = kjordaemi)) +
  geom_col()
