options(width = 140)
# Run from the repository root: LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/3.1-bias/<script>.R
suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(posterior)
})
d_name <- "Sjálfstæðisflokkurinn"
fit <- readRDS("data/2026-10-29/polling_watch_fit.rds")
P <- 10
sig <- as_draws_matrix(fit$draws("sigma"))
lom <- as_draws_matrix(fit$draws("L_Omega"))
res <- read.csv("data/2026-10-29/post_election_polls.csv") |> filter(fyrirtaeki == "Kosning")
dr <- read_parquet("data/2026-10-29/polling_watch_draws.parquet")
options(box.path = getwd())
box::use(R/data[read_polling_watch_data])
p_names <- levels(read_polling_watch_data(start = as.Date("2016-01-01"))$flokkur)
cat("party order in draws:", p_names, "\n")
p0 <- dr |>
  filter(dags == as.Date("2024-11-30")) |>
  summarise(p = median(value), .by = flokkur)
p0 <- p0$p[match(p_names, as.character(p0$flokkur))]
stopifnot(!anyNA(p0), abs(sum(p0) - 1) < 0.01)
p0 <- p0 / sum(p0)
k <- which(p_names == d_name)
cmat <- diag(P) - matrix(1 / P, P, P)
jac <- (diag(p0) - p0 %*% t(p0))[k, ]
sd_day <- vapply(seq_len(nrow(sig)), function(i) {
  l <- matrix(lom[i, ], P, P)
  m <- diag(as.numeric(sig[i, ])) %*% l
  s <- cmat %*% m %*% t(m) %*% cmat
  sqrt(drop(t(jac) %*% s %*% jac))
}, numeric(1))
cat("\nD walk: 1-sd change in pp after n days, from the 2024 result (median over draws)\n")
for (n in c(12, 24, 42, 48, 78, 365)) cat(sprintf("%4d days: %.2f pp\n", n, 100 * median(sd_day) * sqrt(n)))
cat("\nLatent D around the election (pp):\n")
print(dr |> filter(flokkur == d_name, dags >= as.Date("2024-11-01"), dags <= as.Date("2025-03-20")) |>
  summarise(med = round(100 * median(value), 1), q5 = round(100 * quantile(value, .05), 1), q95 = round(100 * quantile(value, .95), 1), .by = dags), n = 30)
mu <- as_draws_matrix(fit$draws("mu_gamma"))
d_e4 <- mu[, sprintf("mu_gamma[4,%d]", k)]
d_e5 <- mu[, sprintf("mu_gamma[5,%d]", k)]
tm <- as.numeric(as_draws_matrix(fit$draws("tau_mu")))
z <- as.numeric(d_e5 - d_e4) / tm
cat(sprintf(
  "\nD industry shock E4->E5: logit %.3f (median), in tau_mu units %.2f (median), P(shock > 0) = %.3f\n",
  median(d_e5 - d_e4), median(z), mean(d_e5 > d_e4)
))
for (e in 1:4) {
  zz <- as.numeric(mu[, sprintf("mu_gamma[%d,%d]", e + 1, k)] - mu[, sprintf("mu_gamma[%d,%d]", e, k)]) / tm
  cat(sprintf("E%d->E%d D shock in tau_mu units: median %.2f\n", e, e + 1, median(zz)))
}
cat(sprintf(
  "\nhalf-normal(0, 0.1) prior: median %.3f, 90%% %.3f, 95%% %.3f; posterior tau_mu median at prior quantile %.2f\n",
  0.1 * qnorm(0.75), 0.1 * qnorm(0.95), 0.1 * qnorm(0.975), 2 * pnorm(median(tm) / 0.1) - 1
))
