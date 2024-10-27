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
    diff <- votes / seats
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


#' @export
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


  available_jofnun <- seats_tibble |>
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

    which_max <- which.max(seat_resid)

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
    )
}
