# Compares a us_midterm_v0 fit on simulated data with the values that generated it.
#
#   LC_ALL=en_US.UTF-8 Rscript usa/R/check_recovery.R fit=<fit dir> sim=<sim dir>
#
# Prints each quantity's truth beside its posterior median and 90% interval, and whether
# the interval covers the truth. Seats are the true winners on election day (the true
# national walk plus each race's true deviation) against the posterior seat counts.

library(cmdstanr)
library(posterior)

opt <- list(fit = NULL, sim = NULL)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1]]
  opt[[kv[1]]] <- kv[2]
}
fit <- readRDS(file.path(opt$fit, "fit.rds"))
truth <- readRDS(file.path(opt$sim, "truth.rds"))
stan_data <- jsonlite::read_json(file.path(opt$sim, "stan_data.json"), simplifyVector = TRUE)

softmax <- function(x) exp(x - max(x)) / sum(exp(x - max(x)))
beta_elec <- truth$beta_true[nrow(truth$beta_true), ]
pi_race <- t(apply(truth$delta, 1, function(d) softmax(beta_elec + d)))
d_wins <- apply(pi_race, 1, which.max) == 1
true_values <- c(
  setNames(as.vector(t(truth$theta)), sprintf("theta[%d][%d]", rep(1:4, each = 3), rep(1:3, 4))),
  sigma_race = truth$sigma_race,
  setNames(softmax(beta_elec), sprintf("pi_elec[%d]", 1:3)),
  seats_house_d = sum(d_wins[stan_data$is_senate == 0]),
  seats_senate_d = sum(d_wins[stan_data$is_senate == 1]) + stan_data$senate_holdover_d
)

draws <- fit$draws(c("theta", "sigma_race", "pi_elec", "seats_house_d", "seats_senate_d"),
  format = "draws_matrix"
)
names_draws <- sub("^theta\\[(\\d+),(\\d+)\\]$", "theta[\\1][\\2]", colnames(draws))
colnames(draws) <- names_draws
rows <- lapply(names(true_values), function(v) {
  x <- as.numeric(draws[, v])
  q <- quantile(x, c(0.05, 0.5, 0.95))
  data.frame(
    variable = v, truth = true_values[[v]], median = q[[2]], q5 = q[[1]], q95 = q[[3]],
    covered = true_values[[v]] >= q[[1]] & true_values[[v]] <= q[[3]]
  )
})
res <- do.call(rbind, rows)
options(width = 120)
print(res, digits = 3, row.names = FALSE)
cat(sprintf("RECOVERY covered %d of %d at 90%%\n", sum(res$covered), nrow(res)))
