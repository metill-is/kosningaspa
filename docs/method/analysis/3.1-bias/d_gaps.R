options(width = 140)
# Run from the repository root: LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/3.1-bias/<script>.R
suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
})
options(box.path = getwd())
box::use(R / data[read_polling_watch_data])
pd <- read_polling_watch_data(start = as.Date("2016-01-01"))
d_name <- "Sjálfstæðisflokkurinn"
shares <- pd |>
  group_by(date, fyrirtaeki) |>
  mutate(p = n / sum(n)) |>
  ungroup() |>
  filter(flokkur == d_name)
res <- shares |>
  filter(fyrirtaeki == "Kosning") |>
  select(e = date, result = p)
polls <- shares |> filter(fyrirtaeki != "Kosning")
gap <- function(lo, hi, label) {
  res |>
    rowwise() |>
    mutate(
      n = sum(polls$date > e + lo & polls$date < e + hi),
      poll_mean = mean(polls$p[polls$date > e + lo & polls$date < e + hi]),
      window = label
    ) |>
    ungroup() |>
    mutate(gap_pp = round(100 * (poll_mean - result), 1), result = round(100 * result, 1), poll_mean = round(100 * poll_mean, 1))
}
cat("D: poll mean minus result, pooled houses\n")
print(bind_rows(
  gap(-90, 0, "90 days before"),
  gap(-30, 0, "30 days before"),
  gap(0, 60, "60 days after"),
  gap(0, 120, "120 days after")
) |> arrange(e, window), n = 40)

fit <- readRDS("data/2026-10-29/polling_watch_fit.rds")
s <- fit$summary(c("sigma", "tau_mu", "tau_house", "sigma_gamma"), median = ~ median(.x), q5 = ~ quantile(.x, 0.05), q95 = ~ quantile(.x, 0.95))
print(s, n = 20)
sd_mu <- fit$draws("mu_gamma", format = "draws_df")
cat("\nmu_gamma column names (head):\n")
print(head(grep("mu_gamma", names(sd_mu), value = TRUE), 12))
dm <- fit$metadata()$stan_variable_sizes
print(dm[c("mu_gamma", "gamma", "beta", "sigma")])
