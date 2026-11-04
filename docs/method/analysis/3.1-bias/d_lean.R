## D's poll lean against v5's latent support: poll share minus the median latent
## share on the poll date, both over the parties that poll reports (pp).
options(width = 140)
# Run from the repository root: LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/3.1-bias/<script>.R
suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(tidyr)
})
options(box.path = getwd())
box::use(R / data[read_polling_watch_data])
pd <- read_polling_watch_data(start = as.Date("2016-01-01")) |>
  filter(fyrirtaeki != "Kosning") |>
  group_by(date, fyrirtaeki) |>
  mutate(p = n / sum(n)) |>
  ungroup()
dr <- read_parquet("data/2026-10-29/polling_watch_draws.parquet")
lat <- dr |>
  summarise(lat = median(value), .by = c(dags, flokkur))
D <- "Sjálfstæðisflokkurinn"
x <- pd |>
  inner_join(lat, by = c(date = "dags", "flokkur")) |>
  group_by(date, fyrirtaeki) |>
  mutate(lat_r = lat / sum(lat)) |>
  ungroup() |>
  filter(flokkur == D) |>
  mutate(
    lean = 100 * (p - lat_r),
    lean_raw = 100 * (p - lat),
    per = case_when(
      date <= as.Date("2024-11-30") & date > as.Date("2021-09-25") ~ paste0("E4 ", format(date, "%Y")),
      date > as.Date("2024-11-30") ~ paste0("E5 ", format(date, "%Y")),
      TRUE ~ "earlier"
    )
  )
cat("Polls matched:", nrow(x), " (D rows in polls:", sum(pd$flokkur == D), ")\n\n")
for (h in c("Gallup", "Maskína")) {
  cat("==", h, "by year within epoch ==\n")
  print(x |> filter(fyrirtaeki == h, per != "earlier") |>
    summarise(n = n(), lean = round(mean(lean), 2), lean_raw = round(mean(lean_raw), 2), .by = per) |> arrange(per))
  cat("\n", h, "E4 Oct-Nov 2024:\n")
  print(x |> filter(fyrirtaeki == h, date >= as.Date("2024-10-01"), date <= as.Date("2024-11-30")) |>
    summarise(n = n(), lean = round(mean(lean), 2)))
  cat("\n", h, "E4 and E5 overall:\n")
  print(x |> filter(fyrirtaeki == h, per != "earlier") |> mutate(ep = substr(per, 1, 2)) |>
    summarise(n = n(), lean = round(mean(lean), 2), .by = ep))
  cat("\n", h, "by quarter in E4:\n")
  print(x |> filter(fyrirtaeki == h, substr(per, 1, 2) == "E4") |>
    mutate(q = paste0(format(date, "%Y"), "Q", (as.integer(format(date, "%m")) - 1) %/% 3 + 1)) |>
    summarise(n = n(), lean = round(mean(lean), 2), .by = q) |> arrange(q), n = 20)
  cat("\n")
}
