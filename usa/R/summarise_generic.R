# Summarises a generic-ballot fit of usa/stan/us_midterm_v3.stan or v4: the election-day
# forecast, the industry bias of each epoch, and the walk's two-party margin on every date
# beside the polls, for the feasibility report (usa/README.md).
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/summarise_generic.R fit=<fit dir> data=<builder out dir>
#
# Margins are two-party, in points: 100 * (D - R) / (D + R). A bias of b in the D-R
# log-odds moves a two-party margin near 50-50 by about 50 * b points. Writes
# <fit>/walk_margin.csv (date, median, 5%, 95%) and <fit>/poll_margin.csv (date, house,
# margin), and prints SUMMARY lines.

library(posterior)
options(width = 120)

opt <- list(fit = NULL, data = NULL)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
fit <- readRDS(file.path(opt$fit, "fit.rds"))
meta <- readRDS(file.path(opt$data, "meta.rds"))

q <- function(x) quantile(x, c(0.05, 0.5, 0.95), names = FALSE)
fmt <- function(x, d = 1) sprintf(paste0("%.", d, "f [%.", d, "f, %.", d, "f]"), x[2], x[1], x[3])

pe <- as_draws_matrix(fit$draws("pi_elec"))
two_party_d <- 100 * pe[, 1] / (pe[, 1] + pe[, 2])
margin_elec <- 2 * two_party_d - 100
cat(sprintf(
  "SUMMARY election day: D %s, R %s, Other %s; two-party D %s; two-party margin D%s\n",
  fmt(100 * q(pe[, 1])), fmt(100 * q(pe[, 2])), fmt(100 * q(pe[, 3])),
  fmt(q(two_party_d)), fmt(q(margin_elec))
))
cat(sprintf("SUMMARY P(D wins the two-party vote) = %.3f\n", mean(margin_elec > 0)))

mg <- as_draws_matrix(fit$draws("mu_gamma"))
E <- fit$metadata()$stan_variable_sizes$mu_gamma[1]
for (e in seq_len(E)) {
  b <- 50 * (mg[, sprintf("mu_gamma[%d,1]", e)] - mg[, sprintf("mu_gamma[%d,2]", e)])
  cat(sprintf("SUMMARY industry bias, epoch %d (polls minus vote, two-party margin, D+): %s\n", e, fmt(q(b))))
}

pw <- as_draws_matrix(fit$draws("pi_walk"))
D <- length(meta$dates)
walk <- do.call(rbind, lapply(seq_len(D), function(t) {
  d <- pw[, sprintf("pi_walk[%d,1]", t)]
  r <- pw[, sprintf("pi_walk[%d,2]", t)]
  x <- q(100 * (d - r) / (d + r))
  data.frame(date = meta$dates[t], q5 = x[1], median = x[2], q95 = x[3])
}))
write.csv(walk, file.path(opt$fit, "walk_margin.csv"), row.names = FALSE)
polls <- data.frame(
  date = meta$polls$end_date, house = meta$polls$house,
  margin = 100 * (meta$polls$y_d - meta$polls$y_r) / (meta$polls$y_d + meta$polls$y_r)
)
write.csv(polls, file.path(opt$fit, "poll_margin.csv"), row.names = FALSE)
last <- polls[polls$date >= max(polls$date) - 28, ]
cat(sprintf(
  "SUMMARY polls in the last 4 weeks: %d, mean two-party margin D%+.1f, median D%+.1f\n",
  nrow(last), mean(last$margin), median(last$margin)
))
cat(sprintf("SUMMARY walk on %s (last poll date): D%s\n", format(meta$dates[D]), fmt(unlist(walk[D, c("q5", "median", "q95")]))))
