#' @export
dhondt <- function(data, ...) {
  n_seats <- unique(data$n_seats)
  party_seats <- numeric(length(data$votes))
  temp_votes <- data$votes

  while (sum(party_seats) < n_seats) {
    which_max <- which.max(temp_votes)
    party_seats[which_max] <- party_seats[which_max] + 1
    temp_votes[which_max] <- data$votes[which_max] / (party_seats[which_max] + 1)
  }

  data$assigned_seats <- party_seats
  data$resid_votes <- temp_votes

  data
}

#' @export
jofnunarsaeti <- function(data, ...) {
  box::use(
    dplyr[summarise, inner_join, select, join_by]
  )

  country_data <- data |>
    summarise(
      votes = sum(votes),
      assigned_seats = sum(assigned_seats),
      .by = flokkur
    )

  votes <- country_data$votes
  seats <- country_data$assigned_seats
  n_seats <- 9
  assigned_seats <- 0
  jofnun_seats <- numeric(length(seats))
  perc_votes <- votes / sum(votes)
  too_low <- perc_votes < 0.05
  votes[too_low] <- 0

  while (assigned_seats < n_seats) {
    diff <- votes / (seats + 1)
    which_max <- which.max(diff)
    seats[which_max] <- seats[which_max] + 1
    jofnun_seats[which_max] <- jofnun_seats[which_max] + 1
    assigned_seats <- assigned_seats + 1
  }

  country_data$jofnun_seats <- jofnun_seats

  data |>
    inner_join(
      country_data |> select(flokkur, jofnun_seats),
      by = join_by(flokkur)
    )
}

#' Seats per kjördæmi in the 2024 layout (Suðvestur gained a seat from Norðvestur)
#' @export
seats_tibble <- function() {
  box::use(
    dplyr[tribble]
  )
  tribble(
    ~kjordaemi, ~n_seats, ~n_jofnun,
    "Reykjavík Suður", 9, 2,
    "Reykjavík Norður", 9, 2,
    "Suðvestur", 12, 2,
    "Suður", 9, 1,
    "Norðaustur", 9, 1,
    "Norðvestur", 6, 1
  )
}

#' Seats per kjördæmi in the layout in force for the 2013–2021 elections
#'
#' With it, `calculate_seats()` reproduces every official seat of the 2016, 2017 and
#' 2021 elections (`tests/harness/test-truth.R`).
#' @export
seats_layout_2021 <- function() {
  box::use(
    dplyr[tribble]
  )
  tribble(
    ~kjordaemi, ~n_seats, ~n_jofnun,
    "Reykjavík Suður", 9, 2,
    "Reykjavík Norður", 9, 2,
    "Suðvestur", 11, 2,
    "Suður", 9, 1,
    "Norðaustur", 9, 1,
    "Norðvestur", 7, 1
  )
}

#' The seat layout in force at a harness election
#'
#' @param election The election day, as a Date or an ISO `YYYY-MM-DD` string.
#' @return `seats_layout_2021()` for 2016-10-29, 2017-10-28 and 2021-09-25,
#'   `seats_tibble()` for 2024-11-30; any other date is an error.
#' @export
layout_for <- function(election) {
  election <- as.Date(election)
  stopifnot(length(election) == 1, !is.na(election))
  key <- format(election)
  switch(key,
    "2016-10-29" = ,
    "2017-10-28" = ,
    "2021-09-25" = seats_layout_2021(),
    "2024-11-30" = seats_tibble(),
    stop("no seat layout for ", key, call. = FALSE)
  )
}

#' Allocate the 63 seats for one draw of kjördæmi votes
#'
#' D'Hondt per kjördæmi, then the adjustment seats nationally by D'Hondt over the
#' lists with at least 5% of the national vote, each placed in the kjördæmi where
#' that list's next quotient is the largest share of the valid votes cast there.
#'
#' @param draw A tibble with columns `flokkur`, `kjordaemi` and `votes`; a list
#'   with no row in a kjördæmi gets 0 votes there.
#' @param layout A tibble with columns `kjordaemi`, `n_seats` and `n_jofnun`.
#' @param ineligible Lists that can win no seat (a forecast passes `"Annað"`, its
#'   residual category). They take part in neither D'Hondt, but their votes still
#'   count as valid votes in the 5% threshold and in the shares that place the
#'   adjustment seats.
#' @return A tibble with one row per list and kjördæmi: `seats` (all seats) and
#'   `kjordaemi_seats` (seats won before adjustment).
#' @export
calculate_seats <- function(draw, layout = seats_tibble(), ineligible = character(0)) {
  box::use(
    tidyr[pivot_wider, pivot_longer],
    tibble[as_tibble, column_to_rownames],
    dplyr[arrange, left_join]
  )

  # WHY: integer vote draws can tie; which.max() breaks ties by row order, and
  # sorting here keeps the result independent of input order (as in 2024 production).
  votes_matrix <- draw |>
    arrange(kjordaemi, flokkur) |>
    pivot_wider(names_from = kjordaemi, values_from = votes, values_fill = 0) |>
    column_to_rownames("flokkur") |>
    as.matrix()

  stopifnot(
    setequal(colnames(votes_matrix), layout$kjordaemi),
    !anyNA(votes_matrix)
  )
  layout <- layout[match(colnames(votes_matrix), layout$kjordaemi), ]
  can_win <- !rownames(votes_matrix) %in% ineligible

  seats_matrix <- matrix(
    0,
    nrow = nrow(votes_matrix),
    ncol = ncol(votes_matrix),
    dimnames = dimnames(votes_matrix)
  )

  for (i in seq_len(ncol(votes_matrix))) {
    party_seats <- numeric(nrow(votes_matrix))
    temp_votes <- votes_matrix[, i] * can_win

    while (sum(party_seats) < layout$n_seats[i]) {
      which_max <- which.max(temp_votes)
      party_seats[which_max] <- party_seats[which_max] + 1
      temp_votes[which_max] <- votes_matrix[which_max, i] / (party_seats[which_max] + 1)
    }

    seats_matrix[, i] <- party_seats
  }

  kjordaemi_seats <- seats_matrix

  available_jofnun <- layout$n_jofnun
  total_votes <- rowSums(votes_matrix)
  percent_votes <- total_votes / sum(total_votes)
  total_seats <- rowSums(seats_matrix)
  n_adjustment_seats <- numeric(nrow(votes_matrix))

  for (k in seq_len(sum(layout$n_jofnun))) {
    which_max <- which.max(
      (total_votes * (percent_votes >= 0.05) * can_win) / (total_seats + n_adjustment_seats + 1)
    )
    n_adjustment_seats[which_max] <- n_adjustment_seats[which_max] + 1

    resid_votes_matrix <- votes_matrix / (seats_matrix + 1)
    resid_votes_matrix <- t(t(resid_votes_matrix) / colSums(votes_matrix))

    for (place in order(resid_votes_matrix[which_max, ], decreasing = TRUE)) {
      if (available_jofnun[place] > 0) {
        seats_matrix[which_max, place] <- seats_matrix[which_max, place] + 1
        available_jofnun[place] <- available_jofnun[place] - 1
        break
      }
    }
  }

  seats_matrix |>
    as_tibble(rownames = "flokkur") |>
    pivot_longer(
      cols = -flokkur,
      names_to = "kjordaemi",
      values_to = "seats"
    ) |>
    left_join(
      kjordaemi_seats |>
        as_tibble(rownames = "flokkur") |>
        pivot_longer(
          cols = -flokkur,
          names_to = "kjordaemi",
          values_to = "kjordaemi_seats"
        ),
      by = c("flokkur", "kjordaemi")
    )
}
