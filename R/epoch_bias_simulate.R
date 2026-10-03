# Simulate polls with KNOWN epoch-varying pollster bias on the real design, to
# check that polling_watch_v5_epoch recovers the shock SDs and the per-epoch
# biases (and to see what a constant-bias fit gets wrong when bias does drift).
#
# The design (dates, houses, epochs, sample sizes, which parties each poll
# reports) is copied from a fitted window. The truth is built from that fit's
# posterior: one draw of the latent path beta, overdispersion phi, epoch-1
# industry bias and house deviations. Epoch shocks are then drawn with the SDs given here.
#
#   Rscript R/epoch_bias_simulate.R --from=v4_full --tau_mu=0.12 --tau_house=0.05 --seed=1 --tag=sim1
#
# Writes results/epoch_bias/<tag>_data/meta.rds (a meta list that
# fit_polling_watch_epoch.R --data_rds= accepts) and truth.rds.

Sys.setlocale("LC_ALL", "en_US.UTF-8")

suppressMessages({
  library(tidyverse)
  library(here)
  library(posterior)
})

opt <- list(from = NA_character_, tau_mu = "0.12", tau_house = "0.05", seed = "1", tag = NA_character_)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- str_match(a, "^--([a-z_]+)=(.*)$")
  if (is.na(kv[1, 1]) || !kv[1, 2] %in% names(opt)) stop("Bad argument: ", a)
  opt[[kv[1, 2]]] <- kv[1, 3]
}
stopifnot(!is.na(opt$from), !is.na(opt$tag))
set.seed(as.integer(opt$seed))

src <- here("results", "epoch_bias", opt$from)
meta <- readRDS(file.path(src, "meta.rds"))
fit <- readRDS(file.path(src, "fit.rds"))
sd0 <- meta$stan_data
P <- sd0$P
H <- sd0$H
E <- sd0$E
D <- sd0$D

# The truth is ONE posterior draw, not the posterior mean: a posterior-mean
# random-walk path is too smooth, which would make a bias jump at an election
# artificially easy to tell apart from genuine opinion movement.
if ("mu_gamma0" %in% fit$metadata()$stan_variables) stop("expected a v4 source fit")
dm <- fit$draws(c("beta", "phi", "mu_gamma", "gamma"), format = "draws_matrix")
draw <- sample.int(nrow(dm), 1)
pick <- function(v) {
  x <- dm[draw, startsWith(colnames(dm), paste0(v, "["))]
  ij <- str_match(names(x), "\\[(\\d+)(?:,(\\d+))?\\]")
  if (all(is.na(ij[, 3]))) return(as.numeric(x)[order(as.integer(ij[, 2]))])
  m <- matrix(NA_real_, max(as.integer(ij[, 2])), max(as.integer(ij[, 3])))
  m[cbind(as.integer(ij[, 2]), as.integer(ij[, 3]))] <- as.numeric(x)
  m
}
beta <- pick("beta") # [D, P]
phi <- pick("phi")
mu1 <- pick("mu_gamma")
g1 <- pick("gamma") # [H, P]
stopifnot(dim(beta) == c(D, P), dim(g1) == c(H, P), length(mu1) == P)
dev1 <- sweep(g1[-1, , drop = FALSE], 2, mu1) # house deviations, epoch 1

# zero-sum shocks: draw iid normal, centre (as the model's shocks live on the subspace)
zs <- function(sd) {
  x <- rnorm(P, 0, sd)
  x - mean(x)
}
tau_mu <- as.numeric(opt$tau_mu)
tau_house <- as.numeric(opt$tau_house)
mu <- matrix(0, E, P)
mu[1, ] <- mu1 - mean(mu1)
dev <- array(0, c(E, H - 1, P))
dev[1, , ] <- dev1
for (e in seq_len(E)[-1]) {
  mu[e, ] <- mu[e - 1, ] + zs(tau_mu)
  for (h in seq_len(H - 1)) dev[e, h, ] <- dev[e - 1, h, ] + zs(tau_house)
}
gamma_true <- function(e, h) if (h == 1) rep(0, P) else mu[e, ] + dev[e, h - 1, ]

rdirmult <- function(alpha, size) {
  g <- rgamma(length(alpha), alpha)
  as.vector(rmultinom(1, size, g / sum(g)))
}

y <- sd0$y_n
for (n in seq_len(sd0$N)) {
  k <- sd0$n_parties_n[n]
  cols <- sd0$party_index_n[n, seq_len(k)]
  size <- sum(sd0$y_n[n, ])
  eta <- beta[sd0$date_n[n], ] + gamma_true(sd0$epoch_n[n], sd0$house_n[n])
  pr <- exp(eta[cols] - max(eta[cols]))
  pr <- pr / sum(pr)
  y[n, ] <- 0L
  y[n, cols] <- if (sd0$house_n[n] == 1) {
    as.vector(rmultinom(1, size, pr))
  } else {
    rdirmult(pr * phi[sd0$house_n[n] - 1], size)
  }
  # keep the design exact: a reported party must stay reported (nonzero)
  zero <- cols[y[n, cols] == 0]
  y[n, zero] <- 1L
}
storage.mode(y) <- "integer"

sim_meta <- meta
sim_meta$stan_data$y_n <- y
sim_meta$simulated <- list(from = opt$from, draw = draw, tau_mu = tau_mu, tau_house = tau_house, seed = opt$seed)

out <- here("results", "epoch_bias", paste0(opt$tag, "_data"))
dir.create(out, showWarnings = FALSE, recursive = TRUE)
saveRDS(sim_meta, file.path(out, "meta.rds"))
truth <- list(
  mu = mu, dev = dev, beta = beta, phi = phi, tau_mu = tau_mu, tau_house = tau_house,
  pi_last = {
    b <- beta[D, ]
    exp(b) / sum(exp(b))
  },
  party_names = meta$party_names, epoch_table = meta$epoch_table
)
saveRDS(truth, file.path(out, "truth.rds"))
cat("Simulated", sd0$N, "polls over", E, "epochs. True industry bias by epoch:\n")
print(round(`dimnames<-`(mu, list(paste0("e", seq_len(E)), meta$party_names)), 2))
