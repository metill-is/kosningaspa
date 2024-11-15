library(ggplot2)
library(dplyr)
library(lubridate)

# Function to calculate V(t)
calc_V <- function(t, tau_stjornarslit) {
  if (t <= 47) {
    return(t * (1 + tau_stjornarslit)^2)
  } else {
    return((t - 47) + 47 * (1 + tau_stjornarslit)^2)
  }
}

# Function to calculate tau_f for our chosen parameters
calc_tau_f <- function(tau_stjornarslit, w = 0.3, t = 180) {
  V_t <- calc_V(t, tau_stjornarslit)
  return(sqrt(V_t * (1 - w) / w))
}

# Function to calculate fundamentals weight at any time given tau_f
calc_weight <- function(t, tau_f, tau_stjornarslit) {
  V_t <- calc_V(t, tau_stjornarslit)
  # Now returning fundamentals weight instead of polling weight
  return(1 / (tau_f^2) / (1 / (tau_f^2) + 1 / V_t))
}

# Create plot data
create_weight_plot <- function(tau_stjornarslit = 0.24) {
  tau_f <- calc_tau_f(tau_stjornarslit)

  # Calculate days until election
  election_date <- clock::date_build(2024, 11, 30)
  current_date <- today()
  days_until_election <- as.numeric(election_date - current_date)

  # Calculate current fundamentals weight
  current_weight <- calc_weight(days_until_election, tau_f, tau_stjornarslit)

  # Calculate weights for different times
  times <- 1:180
  weights <- sapply(times, function(t) calc_weight(t, tau_f, tau_stjornarslit))

  df <- data.frame(
    days_to_election = times,
    weight = weights
  )

  # Create plot
  ggplot(df, aes(x = days_to_election, y = weight)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0.3, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 47, linetype = "dashed", color = "blue") +
    # Add point and label for current date
    annotate(
      geom = "point",
      x = days_until_election,
      y = current_weight,
      color = "darkred",
      size = 3
    ) +
    annotate(
      geom = "text",
      x = days_until_election,
      y = current_weight,
      label = sprintf("Today: %.1f%%", current_weight * 100),
      hjust = 1.2,
      vjust = 1
    ) +
    # Add point and label for day before election
    annotate(
      geom = "point",
      x = 1,
      y = calc_weight(1, tau_f, tau_stjornarslit),
      color = "darkblue",
      size = 3
    ) +
    annotate(
      geom = "text",
      x = 1,
      y = calc_weight(1, tau_f, tau_stjornarslit),
      label = sprintf("Election Eve: %.1f%%", calc_weight(1, tau_f, tau_stjornarslit) * 100),
      hjust = 1.2,
      vjust = 1
    ) +
    scale_x_reverse() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "Days Until Election",
      y = "Weight of Fundamentals Model",
      title = "Weight of Fundamentals Model Over Time",
      subtitle = paste(
        "τ_stjornarslit =", tau_stjornarslit,
        ", Target weight = 30% at t=180"
      )
    ) +
    theme_minimal()
}

# Generate plot
create_weight_plot(tau_stjornarslit = 0.9)
