options(width = 140)
# Run from the repository root: LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/3.1-bias/<script>.R
suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(tidyr)
})
options(box.path = getwd())
box::use(R / data[read_polling_watch_data])
pd <- read_polling_watch_data(start = as.Date("2016-01-01"))
D <- "Sjálfstæðisflokkurinn"
shares <- pd |>
  group_by(date, fyrirtaeki) |>
  mutate(p = n / sum(n)) |>
  ungroup() |>
  filter(flokkur == D)
cat("Results:\n")
print(shares |> filter(fyrirtaeki == "Kosning") |> select(date, p))
cat("\nD polls 2024-09-01 .. 2025-06-30:\n")
print(shares |> filter(date >= as.Date("2024-09-01"), date <= as.Date("2025-06-30")) |> select(date, fyrirtaeki, p) |> mutate(p = round(100 * p, 1)), n = 60)
dr <- read_parquet("data/2026-10-29/polling_watch_draws.parquet")
lat <- dr |>
  filter(flokkur == D) |>
  group_by(dags) |>
  summarise(med = median(value), q5 = quantile(value, .05), q95 = quantile(value, .95))
cat("\nLatent D (pp) at selected dates:\n")
sel <- as.Date(c("2024-09-01", "2024-10-15", "2024-11-01", "2024-11-15", "2024-11-30", "2024-12-15", "2025-01-01", "2025-01-15", "2025-02-01", "2025-03-01", "2025-04-01", "2025-06-01", "2025-09-01", "2026-01-01", "2026-09-27"))
print(lat |> filter(dags %in% sel) |> mutate(across(c(med, q5, q95), ~ round(100 * .x, 1))))
cat("\nE5 polls, D mean by house and half-year:\n")
print(shares |> filter(date > as.Date("2024-11-30"), fyrirtaeki != "Kosning") |> mutate(per = paste0(format(date, "%Y"), ifelse(as.integer(format(date, "%m")) <= 6, "H1", "H2"))) |> summarise(n = n(), D = round(100 * mean(p), 1), .by = c(fyrirtaeki, per)) |> arrange(per, fyrirtaeki))
cat("\nE4 polls in the last 8 weeks, D mean by house:\n")
print(shares |> filter(date > as.Date("2024-10-05"), date < as.Date("2024-11-30")) |> summarise(n = n(), D = round(100 * mean(p), 1), .by = fyrirtaeki))
cat("\nD across E4, by year and house:\n")
print(shares |> filter(date > as.Date("2021-09-25"), date < as.Date("2024-11-30")) |> mutate(yr = format(date, "%Y")) |> summarise(n = n(), D = round(100 * mean(p), 1), .by = c(yr, fyrirtaeki)) |> arrange(yr, fyrirtaeki))
