# Chapter 1.1: the 2024-eve date grid, anchors and party entry, from the Stan data the
# production fit script builds. No model is fitted: fit_headless.R --data-only builds the
# data and quits. Run from the repository root:
#   LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/1.1-walk/d_grid.R
options(width = 120)
box::use(
  jsonlite[read_json],
  R / data[read_polling_data, get_election_data]
)

out <- tempfile("walk_grid_")
status <- system2("Rscript", c("R/sampler_check/fit_headless.R", paste0("--out=", out), "--data-only"))
stopifnot(status == 0)
sd <- read_json(file.path(out, "stan_data.json"), simplifyVector = TRUE)

parties <- levels(read_polling_data()$flokkur)
stopifnot(length(parties) == sd$P, parties[1] == "Annað")
first <- min(read_polling_data()$date[read_polling_data()$date >= as.Date("2016-01-01")])
dates <- first + cumsum(c(0, sd$time_diff))
stopifnot(length(dates) == sd$D)

cat("P =", sd$P, " D =", sd$D, " N_n =", sd$N_n, " N_k =", sd$N_k, " H =", sd$H, "\n")
cat("first date", format(dates[1]), " last date", format(dates[sd$D]),
    " pred_y_time_diff", sd$pred_y_time_diff, " T =", sd$D + sd$pred_y_time_diff, "\n")
cat("days_between_stjornarslit_and_election", sd$days_between_stjornarslit_and_election, "\n")
cat("gaps between dates (days): median", median(sd$time_diff), " mean", round(mean(sd$time_diff), 2),
    " max", max(sd$time_diff), " share of 1-day gaps", round(mean(sd$time_diff == 1), 3), "\n")
cat("dates with month_before_election = 1:", sum(sd$month_before_election), "\n")
mbe_dates <- dates[sd$month_before_election == 1]
print(table(format(mbe_dates, "%Y")))

anchor_rows <- which(sd$house_n == 1)
y_n <- sd$y_n
anchors <- data.frame(
  date = format(dates[sd$date_n[anchor_rows]]),
  votes = rowSums(y_n[anchor_rows, , drop = FALSE]),
  reported = sapply(anchor_rows, function(r) paste(parties[y_n[r, ] > 0], collapse = ", ")),
  absent = sapply(anchor_rows, function(r) paste(parties[y_n[r, ] == 0], collapse = ", "))
)
print(anchors, right = FALSE)

ge <- get_election_data()
tot <- tapply(ge$n, as.character(ge$date), sum)
cat("get_election_data() totals:\n"); print(round(tot))

entry <- sapply(seq_len(sd$P), function(p) {
  hit <- which(apply(sd$active_index_d, 1, function(r) p %in% r))
  if (length(hit)) format(dates[min(hit)]) else NA
})
cat("first date each party is active (n_active_d / active_index_d):\n")
print(data.frame(party = parties, first_active = entry), right = FALSE)
cat("n_active_d over dates:\n"); print(table(sd$n_active_d))

kd_dates <- format(dates[sd$date_k])
cat("kjordaemi rows by house and date (house 1 = a result):\n")
print(table(house = sd$house_k, date = kd_dates))

# The pre-election factor as the model applies it: v[t] = time_diff[t-1] * (1 + tau * m[t-1])^2
# for grid steps, (1 + tau)^2 for the last days_between_stjornarslit_and_election future days.
tau <- log(2)
m <- sd$month_before_election
v <- c(0, sd$time_diff * (1 + tau * m[-sd$D])^2)
fut <- sd$pred_y_time_diff
v_fut <- ifelse(seq(fut, 1) <= sd$days_between_stjornarslit_and_election, (1 + tau)^2, 1)
scaled_days <- c(0, sd$time_diff * m[-sd$D])
a <- sd$date_n[anchor_rows]
bounds <- c(1, a, sd$D)
lab <- c("first date -> 2016", "2016 -> 2017", "2017 -> 2021", "2021 -> last date (+ future days)")
for (i in seq_along(lab)) {
  idx <- (bounds[i] + 1):bounds[i + 1]
  days <- sum(sd$time_diff[idx - 1]) + if (i == 4) fut else 0
  sd_days <- sum(scaled_days[idx]) + if (i == 4) fut else 0
  vt <- sum(v[idx]) + if (i == 4) sum(v_fut) else 0
  cat(sprintf("%-36s days %4d  scaled days %3d  extra variance-time at tau = ln 2: %5.1f\n",
              lab[i], days, sd_days, vt - days))
}
cat("window dates nearest each election (days before):\n")
ed <- as.Date(c("2016-10-29", "2017-10-28", "2021-09-25", "2024-11-30"))
for (e in seq_along(ed)) {
  w <- dates[m == 1 & dates <= ed[e] & dates > ed[e] - 60]
  cat(format(ed[e]), " earliest window date", as.numeric(ed[e] - min(w)), "days before; result date on grid has m =",
      if (ed[e] %in% dates) m[dates == ed[e]] else NA, "\n")
}
