# Chapter 1.1: what the walk's priors imply, analytically or by prior simulation, and the
# days between the 2024 forecast's anchors. No data and no fit. Run from the
# repository root:
#   LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/1.1-walk/d_priors.R
options(width = 120)
set.seed(20261104)

# LKJ(eta) on a d x d correlation matrix: each correlation is 2 Beta(a, a) - 1, a = eta - 1 + d / 2
eta <- 10; d <- 9; a <- eta - 1 + d / 2
cat("LKJ(10), d = 9: a =", a, " sd(r) =", round(1 / sqrt(2 * a + 1), 3),
    " 5-95%:", round(2 * qbeta(c(0.05, 0.95), a, a) - 1, 3), "\n")
cat("LKJ(2), d = 9 for comparison: sd(r) =", round(1 / sqrt(2 * (2 - 1 + d / 2) + 1), 3), "\n")

# tau_stjornarslit ~ exponential(1); the step variance in the 47-day windows is (1 + tau)^2 times the rest
q <- c(0.05, 0.5, 0.95)
cat("(1 + tau)^2 under exponential(1): 5/50/95% =", round((1 + qexp(q))^2, 2), "\n")

# log sigma ~ N(mu, tau), mu ~ N(-2, 2), tau ~ exponential(1): prior predictive of one party's sigma
n <- 1e6
s <- exp(rnorm(n, rnorm(n, -2, 2), rexp(n, 1)))
cat("sigma per sqrt(day), prior 5/50/95%:", signif(quantile(s, q), 3), "\n")
cat("  as sd over 365 days:", signif(quantile(s * sqrt(365), q), 3), "\n")

# Days between the 2024 forecast's anchors, and the bridge midpoint sd without the pre-election
# factor. The factor as the date grid applies it is in d_grid.R: it depends on where the polls fall.
edays <- as.Date(c("2016-01-16", "2016-10-29", "2017-10-28", "2021-09-25", "2024-11-30"))
gaps <- as.numeric(diff(edays))
lab <- c("first poll -> 2016", "2016 -> 2017", "2017 -> 2021", "2021 -> election day 2024")
print(data.frame(stretch = lab, days = gaps,
                 bridge_mid_sd_tau0 = round(sqrt(gaps / 4), 1)), right = FALSE)
cat("bridge conditional variance at variance-time s of R: s (R - s) / R, largest R / 4 at s = R / 2\n")

# A result is a multinomial with the election's valid votes: the sampling sd of one share
votes <- c(`2016` = 189648, `2017` = 196259, `2021` = 199730)
cat("multinomial share sd at p = 0.5 (pp):", round(100 * sqrt(0.25 / votes), 3), "\n")
cat("  at p = 0.25 (pp):", round(100 * sqrt(0.25 * 0.75 / votes), 3), "\n")
