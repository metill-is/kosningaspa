# Arm V2's R reference for the core (spec §6.2–§6.7 with every component switch off, in the
# reference forms), written from the spec and never from Stan/forecast_v2.stan, so that the
# Stan file is tested against an independent oracle: the log density that
# log_prob(u, jacobian = FALSE) evaluates, a prior simulator, the kjördæmi map M_e, the
# failure modes the tests use as mutants, and the map of a point and its data to other party
# and kjördæmi orders.

box::use(
  R / harness / v2[V2_SWITCHES]
)

#' The core's parameters (Ruling T3a), in the declaration order of Stan/forecast_v2.stan,
#' with their Stan types: the names and order of the list that
#' `constrain_variables(..., transformed_parameters = FALSE)` returns and `v2_lp_ref()`
#' reads. `N_new` is the number of parties with `entry_epoch` > 0, and `new_raw` holds one
#' element per such party, in party order. `delta_raw` is one ζ, mapped through every
#' epoch's M_e (static δ). `s_delta0_raw` is r, with log s_δ0 = μ_sd + τ_sd r.
#' @export
V2_CORE_PARS <- tibble::tibble(
  name = c(
    "theta1", "z_walk", "sigma", "L_Omega", "tau_camp", "mu1", "house_raw", "sigma_gamma", "new_raw",
    "log_phi_u", "mu_phi", "sigma_phi", "log_phi_K_u", "log_kappa_u", "delta_raw", "s_delta0_raw", "mu_sd",
    "tau_sd", "nu"
  ),
  type = c(
    "sum_to_zero_vector[P]", "matrix[P - 1, G - 1]", "vector<lower=0>[P]", "cholesky_factor_corr[P]",
    "real<lower=0>", "sum_to_zero_vector[P]", "matrix[H, P]", "real<lower=0>", "vector[N_new]", "vector[H]",
    "real", "real<lower=0>", "real", "real", "array[1] matrix[K, P]", "vector[P]", "real", "real<lower=0>",
    "array[J] sum_to_zero_vector[P]"
  )
)

#' One row per failure mode the tests use as a mutant of `v2_lp_ref()`: `mutant`, `switch`
#' (NA for the core's; else the data switch that enables the step it belongs to) and
#' `min_closed` (the fewest closed elections at which it can change the density). The
#' core's:
#' - `positional_gather`: each likelihood row's counts `y[1:n_rep]` against the first
#'   `n_rep` parties, at all three levels;
#' - `previous_w`: closed epochs from the second on use the previous election's w;
#' - `next_epoch_delta`: the results and kjördæmi polls of closed epoch e use δ_{e+1};
#' - `close_last_kjordaemi`: δ closes on the last kjördæmi instead of through M_e;
#' - `conditional_walk`: the walk's innovation covariance is the conditional
#'   Σ − Σ11ᵀΣ / 1ᵀΣ1 of a normal placed directly on a sum-to-zero innovation, not CΣC.
#' @export
V2_MUTANTS <- tibble::tibble(
  mutant = c("positional_gather", "previous_w", "next_epoch_delta", "close_last_kjordaemi", "conditional_walk"),
  switch = NA_character_,
  min_closed = c(1L, 2L, 2L, 1L, 1L)
)

#' The switches of the components the reference does not have yet (the M-steps' tasks add
#' them), and the parameterisation switches whose reference form is 0.
COMPONENT_SWITCHES <- setdiff(V2_SWITCHES, c("walk_form", "delta_form"))
FORM_SWITCHES <- c("walk_form", "delta_form")

# The §6.10 priors and the bounded transforms (Decision 4) that are literals, not data.
THETA1_SD <- 2
SIGMA_DF <- 3
SIGMA_SCALE <- 0.02
LKJ_ETA <- 10
TAU_CAMP_RATE <- 1
NU_SD <- 2
#' The newcomer contrast with `newcomer_pool` = 0: m_new = 0 and s_new = 0.2023, the median
#' of half-N(0, 0.3) to four digits.
M_NEW <- 0
S_NEW <- 0.2023
LOG_PHI_CAP <- 20
MU_PHI_SD <- 1
SIGMA_PHI_SD <- 1
LOG_PHI_K_CAP <- 20
LOG_PHI_K_INIT <- log(1000)
LOG_PHI_K_MEAN <- log(1000)
LOG_PHI_K_SD <- 1
LOG_KAPPA_CAP <- 2
LOG_KAPPA_INIT <- 0
LOG_KAPPA_MEAN <- 0
MU_SD_MEAN <- log(0.3)
MU_SD_SD <- 0.5
TAU_SD_SD <- 0.5

#' Stop with "v2 reference: " and `...`.
fail <- function(...) stop("v2 reference: ", ..., call. = FALSE)

#' Stop with "v2 reference: `what`" unless `ok` is TRUE.
check <- function(ok, what) if (!isTRUE(ok)) fail(what)

#' The values of `x` joined for a message, or "none".
listed <- function(x) if (length(x)) paste(unique(x), collapse = "; ") else "none"

#' `x` minus its mean.
centre <- function(x) x - mean(x)

#' The softmax of `x`.
softmax <- function(x) {
  e <- exp(x - max(x))
  e / sum(e)
}

#' Neumaier's compensated sum of `x`: within about one rounding of the exact sum, whatever
#' the order of `x`.
neumaier <- function(x) {
  s <- 0
  comp <- 0
  for (v in x) {
    t <- s + v
    comp <- comp + if (abs(s) >= abs(v)) (s - t) + v else (v - t) + s
    s <- t
  }
  s + comp
}

#' The running sums along each row of `x` (a matrix), started at `start`, with Neumaier's
#' compensation: column t is start + the sum of the first t - 1 columns of `x`, each within
#' about one rounding of the exact value.
cumsum_rows <- function(start, x) {
  out <- matrix(0, nrow(x), ncol(x) + 1)
  s <- start
  comp <- 0 * start
  out[, 1] <- s
  for (i in seq_len(ncol(x))) {
    v <- x[, i]
    t <- s + v
    comp <- comp + ifelse(abs(s) >= abs(v), (s - t) + v, (v - t) + s)
    s <- t
    out[, i + 1] <- s + comp
  }
  out
}

#' The column sums of `x` with Neumaier's compensation.
colsums_exact <- function(x) {
  s <- x[1, ]
  comp <- 0 * s
  for (i in seq_len(nrow(x))[-1]) {
    v <- x[i, ]
    t <- s + v
    comp <- comp + ifelse(abs(s) >= abs(v), (s - t) + v, (v - t) + s)
    s <- t
  }
  s + comp
}

#' The lower-triangular L with a positive diagonal and LLᵀ = BBᵀ, from the QR decomposition
#' of Bᵀ, so the Gram matrix BBᵀ is never formed: its rounding would square the condition
#' number. Mathematically chol(BBᵀ).
chol_of_gram <- function(B) {
  q <- qr(t(B), tol = 0)
  check(identical(q$pivot, seq_len(ncol(t(B)))), "chol_of_gram: the QR decomposition pivoted")
  R <- qr.R(q)
  t(R * sign(diag(R)))
}

#' The walk's steps in coordinates 2:P, sqrt(Δ_i) s_i L_walk z_i, one column per grid step.
walk_steps <- function(L_walk, z, scale) (L_walk %*% z) * rep(scale, each = nrow(L_walk))

#' The walk's per-step scale sqrt(Δ_i) s_i, with s_i = 1 + τ_camp c_i (§6.2).
walk_scale <- function(tau_camp, data) sqrt(c(data$delta_days)) * (1 + tau_camp * c(data$campaign))

#' The factor L_walk = chol((CΣC)[2:P, 2:P]) of Σ = diag(σ) Ω diag(σ), Ω = LLᵀ, computed as
#' the Cholesky factor of the Gram matrix of the rows 2:P of C diag(σ) L.
walk_factor <- function(sigma, L_Omega) {
  M <- sigma * L_Omega
  chol_of_gram((M - matrix(colMeans(M), nrow(M), ncol(M), byrow = TRUE))[-1, , drop = FALSE])
}

#' The map M_e of §6.7: (I − 1wᵀ) X C. It subtracts each party's w-weighted mean over the
#' kjördæmi, then centres each kjördæmi over the parties, so the result's rows sum to 0 and
#' its w-weighted column sums are 0.
#' @param X a K × P matrix
#' @param w the K kjördæmi weights, summing to 1
#' @return a K × P matrix
#' @export
M_e <- function(X, w) {
  X <- as.matrix(X)
  Y <- X - matrix(drop(as.numeric(w) %*% X), nrow(X), ncol(X), byrow = TRUE)
  Y - rowMeans(Y)
}

#' The `close_last_kjordaemi` mutant of M_e: centre each kjördæmi over the parties, then
#' set the last kjördæmi to the value that zeroes the w-weighted column sums.
close_on_last <- function(X, w) {
  Y <- X - rowMeans(X)
  K <- nrow(Y)
  Y[K, ] <- -colSums(w[-K] * Y[-K, , drop = FALSE]) / w[K]
  Y
}

#' The Dirichlet-multinomial log-pmf of counts `y` given concentrations `alpha`, with its
#' normalising constant, the multinomial coefficient included:
#' lgamma(n + 1) + lgamma(A) − lgamma(n + A) + Σ_p [lgamma(y_p + α_p) − lgamma(α_p) −
#' lgamma(y_p + 1)], with n = Σ y and A = Σ α. It is evaluated as the same sum written with
#' lbeta, log n + lbeta(A, n) − Σ_{y_p > 0} [log y_p + lbeta(α_p, y_p)], which leaves out the
#' cancelling terms of order n log n that cost digits at an election's counts. A category
#' with `alpha` 0 and count 0 adds nothing (its limit); one with `alpha` 0 and a positive
#' count gives -Inf.
#' @param y non-negative counts
#' @param alpha non-negative concentrations, as long as `y`
#' @return numeric(1)
#' @export
dirmult_lpmf <- function(y, alpha) {
  y <- as.numeric(y)
  alpha <- as.numeric(alpha)
  check(length(y) == length(alpha) && all(y >= 0) && all(alpha >= 0), "dirmult_lpmf needs counts and concentrations >= 0")
  zero <- alpha == 0
  if (any(y[zero] > 0)) {
    return(-Inf)
  }
  y <- y[!zero]
  alpha <- alpha[!zero]
  n <- sum(y)
  if (n == 0) {
    return(0)
  }
  pos <- y > 0
  (log(n) + lbeta(sum(alpha), n)) - sum(log(y[pos]) + lbeta(alpha[pos], y[pos]))
}

#' One draw of `n` counts from the Dirichlet-multinomial with concentrations `alpha`, from
#' the global RNG. The Dirichlet draw is taken on the log scale, log G_a = log G_{a+1} +
#' log(U) / a, so a small concentration cannot underflow to an all-zero draw.
#' @param n the total count
#' @param alpha positive concentrations
#' @return an integer vector as long as `alpha`, summing to `n`
#' @export
dirmult_rng <- function(n, alpha) {
  check(length(alpha) >= 1 && all(alpha > 0), "dirmult_rng needs positive concentrations")
  g <- log(stats::rgamma(length(alpha), alpha + 1)) + log(stats::runif(length(alpha))) / alpha
  as.integer(stats::rmultinom(1, n, softmax(g)))
}

#' The log density Stan's `lkj_corr_cholesky_lpdf(L | eta)` gives the Cholesky factor of a
#' correlation matrix: the LKJ(eta) density of Ω = LLᵀ times the Jacobian of Ω in L's free
#' elements, sum_{k=2}^{P} (P − k + 2 eta − 2) log L_kk, plus the normalising constant of
#' Lewandowski, Kurowicka and Joe (2009), so it integrates to 1 over L's free elements.
#' @param L a P × P lower-triangular Cholesky factor of a correlation matrix
#' @param eta the shape, positive
#' @return numeric(1)
#' @export
lkj_corr_cholesky_lpdf <- function(L, eta) {
  P <- nrow(L)
  if (P < 2) {
    return(0)
  }
  i <- seq_len(P - 1)
  b <- eta + (i - 1) / 2
  log_c <- -sum((2 * eta - 2 + i) * i * log(2) + i * lbeta(b, b))
  k <- 2:P
  log_c + sum((P - k + 2 * eta - 2) * log(diag(L)[k]))
}

#' One draw of the Cholesky factor of an LKJ(eta) correlation matrix of size P, by the
#' C-vine, from the global RNG: the canonical partial correlations of column j are
#' independent 2 Beta(b_j, b_j) − 1 with b_j = eta + (P − 1 − j) / 2, and row i of L is
#' L_ij = w_ij sqrt(1 − sum_{k<j} L_ik²), L_ii = sqrt(1 − sum_{k<i} L_ik²).
#' @param P the size
#' @param eta the shape, positive
#' @return a P × P lower-triangular matrix with unit-norm rows
#' @export
lkj_corr_cholesky_rng <- function(P, eta) {
  w <- matrix(0, P, P)
  for (j in seq_len(P - 1)) {
    b <- eta + (P - 1 - j) / 2
    w[(j + 1):P, j] <- 2 * stats::rbeta(P - j, b, b) - 1
  }
  L <- matrix(0, P, P)
  L[1, 1] <- 1
  for (i in seq_len(P)[-1]) {
    used <- 0
    for (j in seq_len(i - 1)) {
      L[i, j] <- w[i, j] * sqrt(1 - used)
      used <- used + L[i, j]^2
    }
    L[i, i] <- sqrt(1 - used)
  }
  L
}

#' The dimensions of the core's parameters on `data`, as cmdstanr's `stan_variable_sizes`
#' gives them (1 for a scalar), named and ordered as `V2_CORE_PARS`.
#' @param data a `v2_stan_data()` data list
#' @return a named list of integer vectors
#' @export
v2_par_dims <- function(data) {
  P <- data$P
  list(
    theta1 = P, z_walk = c(P - 1L, data$G - 1L), sigma = P, L_Omega = c(P, P), tau_camp = 1L, mu1 = P,
    house_raw = c(data$H, P), sigma_gamma = 1L, new_raw = sum(c(data$entry_epoch) > 0), log_phi_u = data$H,
    mu_phi = 1L, sigma_phi = 1L, log_phi_K_u = 1L, log_kappa_u = 1L, delta_raw = c(1L, data$K, P),
    s_delta0_raw = P, mu_sd = 1L, tau_sd = 1L, nu = c(data$J, P)
  )
}

#' Stop unless `data` is the core in the reference forms, and `mutant` is "none" or one of
#' `v2_mutants(data)`.
check_core <- function(data, mutant = "none") {
  forms <- vapply(FORM_SWITCHES, \(s) as.integer(data[[s]]), integer(1))
  check(all(forms == 0L), "the reference forms only: walk_form and delta_form must be 0")
  on <- COMPONENT_SWITCHES[vapply(COMPONENT_SWITCHES, \(s) !identical(as.integer(data[[s]]), 0L), logical(1))]
  if (length(on)) fail("switch(es) not in the reference yet: ", listed(on))
  check(
    is.character(mutant) && length(mutant) == 1 && mutant %in% c("none", v2_mutants(data)),
    paste0("mutant ", mutant, " is not one of none, ", paste(v2_mutants(data), collapse = ", "))
  )
}

#' `pars` as plain numeric vectors and matrices, after checking its names, lengths, bounds
#' and constraints: each `V2_CORE_PARS` name present with `v2_par_dims()`' length (in
#' cmdstanr's column-major order), nothing else of non-zero length, the sum-to-zero vectors
#' summing to 0, the lower-bounded scalars positive, `L_Omega` lower triangular with a
#' positive diagonal and unit-norm rows.
core_pars <- function(pars, data) {
  dims <- v2_par_dims(data)
  missing <- setdiff(names(dims), names(pars))
  if (length(missing)) fail("pars lacks ", listed(missing))
  extra <- setdiff(names(pars), names(dims))
  extra <- extra[lengths(pars[extra]) > 0]
  if (length(extra)) fail("pars has parameters outside the core: ", listed(extra))
  for (n in names(dims)) {
    if (length(pars[[n]]) != prod(dims[[n]])) fail(n, " has length ", length(pars[[n]]), ", not ", prod(dims[[n]]))
    if (!all(is.finite(as.numeric(pars[[n]])))) fail(n, " is not finite")
  }
  P <- data$P
  K <- data$K
  as_matrix <- function(n) matrix(as.numeric(pars[[n]]), dims[[n]][length(dims[[n]]) - 1], dims[[n]][length(dims[[n]])])
  p <- lapply(pars[names(dims)], as.numeric)
  for (n in c("z_walk", "L_Omega", "house_raw", "nu")) p[[n]] <- as_matrix(n)
  p$delta_raw <- matrix(as.numeric(pars$delta_raw), K, P)
  zero_sum <- function(x) abs(sum(x)) <= 1e-8 * max(1, abs(x))
  check(zero_sum(p$theta1), "theta1 does not sum to 0")
  check(zero_sum(p$mu1), "mu1 does not sum to 0")
  check(all(apply(p$nu, 1, zero_sum)), "a row of nu does not sum to 0")
  for (n in c("sigma", "tau_camp", "sigma_gamma", "sigma_phi", "tau_sd")) check(all(p[[n]] > 0), paste(n, "must be positive"))
  L <- p$L_Omega
  check(
    all(L[upper.tri(L)] == 0) && all(diag(L) > 0) && all(abs(rowSums(L^2) - 1) <= 1e-8),
    "L_Omega is not the Cholesky factor of a correlation matrix"
  )
  p
}

#' The natural quantities of a checked point `p` (`core_pars()`) on `data`: the walk θ
#' (P × G) and its innovation factor, μ_e, γ_{e,h}, δ_e, the bounded log concentrations,
#' `phi_init`, s_δ0 and π_nat at each national poll's date. `conditional` and `close_last`
#' switch to the `conditional_walk` and `close_last_kjordaemi` mutants.
natural <- function(p, data, conditional = FALSE, close_last = FALSE) {
  P <- data$P
  G <- data$G
  H <- data$H
  K <- data$K
  n_epoch <- data$n_epoch
  entry <- c(data$entry_epoch)

  # §6.2: coordinates 2:P step by sqrt(Δ_i) s_i L_walk z_i, L_walk = chol((CΣC)[2:P, 2:P]),
  # and party 1 by minus their sum, so every θ_t sums to 0 and each step's covariance is
  # Δ_i s_i² CΣC.
  # The sums are compensated, so θ does not depend on the party order beyond a rounding.
  L_walk <- if (conditional) {
    Sigma <- tcrossprod(p$sigma * p$L_Omega)
    V <- Sigma - Sigma %*% matrix(1, P, P) %*% Sigma / sum(Sigma)
    t(chol(V[-1, -1, drop = FALSE]))
  } else {
    walk_factor(p$sigma, p$L_Omega)
  }
  step <- walk_steps(L_walk, p$z_walk, walk_scale(p$tau_camp, data))
  step <- rbind(-colsums_exact(step), step)
  theta <- cumsum_rows(p$theta1, step)

  # §6.4 with μ constant over epochs: from its entry epoch on, a newcomer's bias is the mean
  # of the biases of the parties existing before that epoch (present at the window's start
  # or entering earlier), plus m_new + s_new η; then μ_e is re-centred.
  new <- which(entry > 0)
  mu <- matrix(0, n_epoch, P)
  m <- p$mu1
  for (e in seq_len(n_epoch)) {
    entering <- which(entry == e)
    if (length(entering)) m[entering] <- mean(m[entry < e]) + M_NEW + S_NEW * p$new_raw[match(entering, new)]
    m <- centre(m)
    mu[e, ] <- m
  }
  # d_h = C(σ_γ ε_h), the same in every epoch: with no drift, a newcomer's slot already holds
  # its fresh σ_γ ε draw (up to the house's common shift, which cancels in every softmax).
  d <- p$sigma_gamma * p$house_raw
  d <- d - rowMeans(d)
  gamma <- array(0, c(n_epoch, H, P))
  for (e in seq_len(n_epoch)) gamma[e, , ] <- d + matrix(mu[e, ], H, P, byrow = TRUE)

  # §6.7, static: δ_e = M_e(s_δ0 ⊙ ζ), one ζ for every epoch.
  s_delta0 <- exp(p$mu_sd + p$tau_sd * p$s_delta0_raw)
  X <- p$delta_raw * matrix(s_delta0, K, P, byrow = TRUE)
  delta <- array(0, c(n_epoch, K, P))
  for (e in seq_len(n_epoch)) delta[e, , ] <- if (close_last) close_on_last(X, data$w[e, ]) else M_e(X, data$w[e, ])

  # Decision 4: log c = cap − (cap − init) exp(u).
  phi_init <- mean(log(rowSums(data$y_nat)))
  log_phi <- LOG_PHI_CAP - (LOG_PHI_CAP - phi_init) * exp(p$log_phi_u)
  log_phi_K <- LOG_PHI_K_CAP - (LOG_PHI_K_CAP - LOG_PHI_K_INIT) * exp(p$log_phi_K_u)
  log_kappa <- LOG_KAPPA_CAP - (LOG_KAPPA_CAP - LOG_KAPPA_INIT) * exp(p$log_kappa_u)

  # §6.3: π_nat,t = Σ_k w_{e,k} softmax over A_t of (θ_t + δ_{e,k}), 0 outside A_t.
  pi_nat <- matrix(0, data$N_nat, P)
  for (n in seq_len(data$N_nat)) {
    t <- data$grid_nat[n]
    e <- data$grid_epoch[t]
    A <- which(data$exists[t, ] == 1L)
    for (k in seq_len(K)) pi_nat[n, A] <- pi_nat[n, A] + data$w[e, k] * softmax(theta[A, t] + delta[e, k, A])
  }

  list(
    theta = theta, step = step, L_walk = L_walk, mu = mu, gamma = gamma, delta = delta, phi_init = phi_init,
    log_phi = log_phi, log_phi_K = log_phi_K, log_kappa = log_kappa, s_delta0 = s_delta0, pi_nat = pi_nat
  )
}

#' The natural quantities of a point: what Stan's transformed parameters of the same names
#' hold, and what a simulation's truth records.
#' @param pars a named list as `constrain_variables(..., transformed_parameters = FALSE)`
#'   returns it (`V2_CORE_PARS`)
#' @param data a `v2_stan_data()` data list of the core in the reference forms
#' @return a list: `theta` (P × G), `pi_nat` (N_nat × P: π_nat at each national poll's
#'   date), `mu` (n_epoch × P, each row summing to 0), `gamma` (n_epoch × H × P), `delta`
#'   (n_epoch × K × P), `eps` (n_closed × P: the election shock, 0 in the core), `log_phi`
#'   and `phi` (H), `log_phi_K` and `phi_K`, `log_kappa` and `kappa`, and `s_delta0` (P)
#' @export
v2_natural <- function(pars, data) {
  check_core(data)
  nat <- natural(core_pars(pars, data), data)
  list(
    theta = nat$theta, pi_nat = nat$pi_nat, mu = nat$mu, gamma = nat$gamma, delta = nat$delta,
    eps = matrix(0, data$n_closed, data$P), log_phi = nat$log_phi, phi = exp(nat$log_phi),
    log_phi_K = nat$log_phi_K, phi_K = exp(nat$log_phi_K), log_kappa = nat$log_kappa, kappa = exp(nat$log_kappa),
    s_delta0 = nat$s_delta0
  )
}

#' Every likelihood row of the core, each `list(field, at, S, alpha)`: the count field and
#' its row (`at`; (e, k) for a result), the parties S its counts are gathered by and its
#' Dirichlet-multinomial concentrations over S. National poll n (§6.3):
#' φ_h normalise(π_nat[S] ⊙ exp(γ_{e,h}[S])). Kjördæmi poll row r (§6.7):
#' κ φ_h softmax over S of (ν_j + δ_{e,k}). Result (e, k) (§6.6):
#' φ_K softmax over S of (θ_{t_e} + δ_{e,k}). S is the row's reported parties by identity,
#' and e the epoch of the row's date. `positional` and `next_delta` switch to the
#' `positional_gather` and `next_epoch_delta` mutants.
dm_rows <- function(p, nat, data, positional = FALSE, next_delta = FALSE) {
  parties <- function(idx, n_rep, i) if (positional) seq_len(n_rep[i]) else idx[i, seq_len(n_rep[i])]
  delta_epoch <- function(e) if (next_delta && e <= data$n_closed) e + 1L else e
  phi <- exp(nat$log_phi)
  rows <- vector("list", data$N_nat + data$N_kd + data$n_closed * data$K)
  i <- 0L
  for (n in seq_len(data$N_nat)) {
    e <- data$grid_epoch[data$grid_nat[n]]
    h <- data$house_nat[n]
    S <- parties(data$idx_nat, data$n_rep_nat, n)
    a <- nat$pi_nat[n, S] * exp(nat$gamma[e, h, S])
    i <- i + 1L
    rows[[i]] <- list(field = "y_nat", at = n, S = S, alpha = phi[h] * a / sum(a))
  }
  kappa <- exp(nat$log_kappa)
  for (r in seq_len(data$N_kd)) {
    e <- delta_epoch(data$grid_epoch[data$grid_kd[r]])
    S <- parties(data$idx_kd, data$n_rep_kd, r)
    eta <- p$nu[data$poll_kd[r], S] + nat$delta[e, data$kd_kd[r], S]
    i <- i + 1L
    rows[[i]] <- list(field = "y_kd", at = r, S = S, alpha = kappa * phi[data$house_kd[r]] * softmax(eta))
  }
  phi_K <- exp(nat$log_phi_K)
  for (e in seq_len(data$n_closed)) {
    t <- data$grid_res[e]
    de <- delta_epoch(data$grid_epoch[t])
    S <- parties(data$idx_res, data$n_rep_res, e)
    for (k in seq_len(data$K)) {
      i <- i + 1L
      rows[[i]] <- list(field = "y_res", at = c(e, k), S = S, alpha = phi_K * softmax(nat$theta[S, t] + nat$delta[de, k, S]))
    }
  }
  rows
}

#' The counts of likelihood row `r` (`dm_rows()`), gathered by its S.
row_counts <- function(data, r) {
  switch(r$field,
    y_nat = data$y_nat[r$at, r$S],
    y_kd = data$y_kd[r$at, r$S],
    y_res = data$y_res[r$at[1], r$at[2], r$S]
  )
}

#' The prior terms of a checked point, one per block, with every normalising constant: the
#' §6.10 priors on the constrained values, the half-t and half-normals normalised over the
#' positive half-line, `lkj_corr_cholesky_lpdf()` on L itself, and the bounded transforms'
#' log-Jacobians log(cap − init) + u, which the model block adds by hand.
prior_terms <- function(p, nat, data) {
  ld <- function(x, mean = 0, sd = 1) sum(stats::dnorm(x, mean, sd, log = TRUE))
  c(
    theta1 = ld(p$theta1, 0, THETA1_SD),
    z_walk = ld(p$z_walk),
    sigma = sum(log(2) + stats::dt(p$sigma / SIGMA_SCALE, SIGMA_DF, log = TRUE) - log(SIGMA_SCALE)),
    L_Omega = lkj_corr_cholesky_lpdf(p$L_Omega, LKJ_ETA),
    tau_camp = stats::dexp(p$tau_camp, TAU_CAMP_RATE, log = TRUE),
    mu1 = ld(p$mu1, 0, data$prior_sd_mu1),
    house_raw = ld(p$house_raw),
    sigma_gamma = stats::dexp(p$sigma_gamma, data$prior_rate_sigma_gamma, log = TRUE),
    new_raw = ld(p$new_raw),
    log_phi = ld(nat$log_phi, p$mu_phi, p$sigma_phi),
    log_phi_jacobian = sum(log(LOG_PHI_CAP - nat$phi_init) + p$log_phi_u),
    mu_phi = ld(p$mu_phi, data$prior_mu_phi, MU_PHI_SD),
    sigma_phi = log(2) + ld(p$sigma_phi, 0, SIGMA_PHI_SD),
    log_phi_K = ld(nat$log_phi_K, LOG_PHI_K_MEAN, LOG_PHI_K_SD),
    log_phi_K_jacobian = log(LOG_PHI_K_CAP - LOG_PHI_K_INIT) + p$log_phi_K_u,
    log_kappa = ld(nat$log_kappa, LOG_KAPPA_MEAN, data$prior_sd_log_kappa),
    log_kappa_jacobian = log(LOG_KAPPA_CAP - LOG_KAPPA_INIT) + p$log_kappa_u,
    delta_raw = ld(p$delta_raw),
    s_delta0_raw = ld(p$s_delta0_raw),
    mu_sd = ld(p$mu_sd, MU_SD_MEAN, MU_SD_SD),
    tau_sd = log(2) + ld(p$tau_sd, 0, TAU_SD_SD),
    nu = ld(p$nu, 0, NU_SD)
  )
}

#' The reference log density of the V2 core (spec §6.2–§6.7, every component switch off):
#' what Stan's `log_prob(u, jacobian = FALSE)` evaluates in the reference forms, up to a
#' constant. It is the density of the parameters on their constrained values (the §6.10
#' priors, `lkj_corr_cholesky(10)` on L itself, the normals on every element of each
#' sum-to-zero vector) plus the log-Jacobians log(cap − init) + u of the three bounded
#' concentration transforms, which the model block adds by hand, plus the national-poll,
#' kjördæmi-poll and result likelihoods (`dm_rows()`), each a Dirichlet-multinomial gathered
#' by party identity; every normalising constant is included. It leaves out the automatic
#' Jacobians of Stan's own constraints (lower = 0, cholesky_factor_corr, sum_to_zero), which
#' `jacobian = FALSE` excludes. Terms are summed with a compensated sum, so the result does
#' not depend on the party or kjördæmi order beyond a rounding.
#' @param pars a named list as `constrain_variables(..., transformed_parameters = FALSE)`
#'   returns it: the `V2_CORE_PARS`, each in cmdstanr's column-major order
#' @param data a `v2_stan_data()` data list with `walk_form`, `delta_form` and the five
#'   component switches 0
#' @param mutant "none", or a mutant of `v2_mutants(data)` (`V2_MUTANTS`)
#' @return numeric(1). An error, "v2 reference: ...", for another form or switch, another
#'   mutant, or a point with a missing, misshapen or out-of-bounds parameter
#' @export
v2_lp_ref <- function(pars, data, mutant = "none") {
  check_core(data, mutant)
  p <- core_pars(pars, data)
  if (mutant == "previous_w") data$w[2:data$n_closed, ] <- data$w[seq_len(data$n_closed - 1), , drop = FALSE]
  nat <- natural(p, data, conditional = mutant == "conditional_walk", close_last = mutant == "close_last_kjordaemi")
  rows <- dm_rows(p, nat, data, positional = mutant == "positional_gather", next_delta = mutant == "next_epoch_delta")
  lik <- vapply(rows, \(r) dirmult_lpmf(row_counts(data, r), r$alpha), numeric(1))
  neumaier(c(prior_terms(p, nat, data), lik))
}

#' The mutants of `V2_MUTANTS` that can change the density on `data`: those whose switch is
#' NA (the core's) or 1 in `data`, and whose `min_closed` is at most `data$n_closed`.
#' @param data a `v2_stan_data()` data list
#' @return a character vector
#' @export
v2_mutants <- function(data) {
  on <- vapply(V2_MUTANTS$switch, \(s) is.na(s) || identical(as.integer(data[[s]]), 1L), logical(1))
  V2_MUTANTS$mutant[on & V2_MUTANTS$min_closed <= data$n_closed]
}

#' A sum-to-zero vector of length n with normal(0, sd) on every element: sd · C z, which is
#' isotropic on the sum-to-zero subspace.
rstz <- function(n, sd) centre(stats::rnorm(n, 0, sd))

#' A prior draw of the core's parameters on `data`, in `V2_CORE_PARS` order and cmdstanr's
#' shapes, from the global RNG. Untruncated, except that each bounded concentration's block
#' is redrawn whole until it lies below its cap: the model's density is the untruncated one
#' restricted to below the cap, so the rejection draws it exactly.
draw_prior <- function(data) {
  P <- data$P
  G <- data$G
  H <- data$H
  K <- data$K
  J <- data$J
  phi_init <- mean(log(rowSums(data$y_nat)))
  pars <- list(
    theta1 = rstz(P, THETA1_SD),
    z_walk = matrix(stats::rnorm((P - 1) * (G - 1)), P - 1, G - 1),
    sigma = abs(stats::rt(P, SIGMA_DF)) * SIGMA_SCALE,
    L_Omega = lkj_corr_cholesky_rng(P, LKJ_ETA),
    tau_camp = stats::rexp(1, TAU_CAMP_RATE),
    mu1 = rstz(P, data$prior_sd_mu1),
    house_raw = matrix(stats::rnorm(H * P), H, P),
    sigma_gamma = stats::rexp(1, data$prior_rate_sigma_gamma),
    new_raw = stats::rnorm(sum(c(data$entry_epoch) > 0))
  )
  repeat {
    mu_phi <- stats::rnorm(1, data$prior_mu_phi, MU_PHI_SD)
    sigma_phi <- abs(stats::rnorm(1, 0, SIGMA_PHI_SD))
    log_phi <- stats::rnorm(H, mu_phi, sigma_phi)
    if (all(log_phi < LOG_PHI_CAP)) break
  }
  repeat {
    log_phi_K <- stats::rnorm(1, LOG_PHI_K_MEAN, LOG_PHI_K_SD)
    if (log_phi_K < LOG_PHI_K_CAP) break
  }
  repeat {
    log_kappa <- stats::rnorm(1, LOG_KAPPA_MEAN, data$prior_sd_log_kappa)
    if (log_kappa < LOG_KAPPA_CAP) break
  }
  c(pars, list(
    log_phi_u = log((LOG_PHI_CAP - log_phi) / (LOG_PHI_CAP - phi_init)),
    mu_phi = mu_phi,
    sigma_phi = sigma_phi,
    log_phi_K_u = log((LOG_PHI_K_CAP - log_phi_K) / (LOG_PHI_K_CAP - LOG_PHI_K_INIT)),
    log_kappa_u = log((LOG_KAPPA_CAP - log_kappa) / (LOG_KAPPA_CAP - LOG_KAPPA_INIT)),
    delta_raw = array(stats::rnorm(K * P), c(1L, K, P)),
    s_delta0_raw = stats::rnorm(P),
    mu_sd = stats::rnorm(1, MU_SD_MEAN, MU_SD_SD),
    tau_sd = abs(stats::rnorm(1, 0, TAU_SD_SD)),
    nu = array(t(vapply(seq_len(J), \(j) rstz(P, NU_SD), numeric(P))), c(J, P))
  ))
}

#' A draw from V2's core: parameters from the prior, and, when `observe`, every count from
#' the model given them.
#' @param data a `v2_stan_data()` data list of the core in the reference forms
#' @param seed the seed; the draw uses its own RNG stream (`withr::with_seed()`), leaving the
#'   caller's untouched
#' @param observe whether to replace the counts
#' @return `list(pars, truth, data)`:
#'   - `pars`, a prior draw of `V2_CORE_PARS` in cmdstanr's shapes (scalars and vectors
#'     plain, `delta_raw` 1 × K × P, `nu` J × P), untruncated: σ ~ half-t(3, 0, 0.02),
#'     Ω ~ LKJ(10) by the C-vine, and each bounded concentration from its normal below its
#'     cap (the model's prior);
#'   - `truth`, `v2_natural(pars, data)`;
#'   - `data`, the input with `y_nat`, `y_kd` and `y_res` replaced (when `observe`): each
#'     row keeps its total and its reported parties (`idx_*`, `n_rep_*`), its other cells
#'     stay 0, and its reported counts are one Dirichlet-multinomial draw with the row's
#'     concentrations, as the likelihood has them. A reported party can draw 0 and stays
#'     reported. Every other field is unchanged
#' @export
v2_simulate <- function(data, seed, observe = TRUE) {
  check_core(data)
  withr::with_seed(seed, {
    pars <- draw_prior(data)
    p <- core_pars(pars, data)
    nat <- natural(p, data)
    sim <- data
    if (observe) {
      for (r in dm_rows(p, nat, data)) {
        if (r$field == "y_res") {
          n <- sum(data$y_res[r$at[1], r$at[2], ])
          sim$y_res[r$at[1], r$at[2], ] <- 0L
          sim$y_res[r$at[1], r$at[2], r$S] <- dirmult_rng(n, r$alpha)
        } else {
          n <- sum(data[[r$field]][r$at, ])
          sim[[r$field]][r$at, ] <- 0L
          sim[[r$field]][r$at, r$S] <- dirmult_rng(n, r$alpha)
        }
      }
    }
  })
  list(pars = pars, truth = v2_natural(pars, data), data = sim)
}

#' `new` stored as `old` was: a 1-d array if `old` is one.
same_shape <- function(new, old) if (length(dim(old)) == 1) array(new, dim = length(new)) else new

#' The reported-party lists `idx` (rows padded with 0) mapped to the party order `pp`: each
#' row's parties by identity, in the new order, padded with 0.
remap_idx <- function(idx, n_rep, pp) {
  out <- matrix(0L, nrow(idx), ncol(idx))
  for (i in seq_len(nrow(idx))) {
    s <- sort(match(idx[i, seq_len(n_rep[i])], pp))
    out[i, seq_along(s)] <- s
  }
  out
}

#' `data` in party order `pp` and kjördæmi order `kp`, keeping the builder's contract.
permute_data <- function(data, pp, kp) {
  out <- data
  out$exists <- data$exists[, pp, drop = FALSE]
  out$w <- data$w[, kp, drop = FALSE]
  out$y_nat <- data$y_nat[, pp, drop = FALSE]
  out$idx_nat <- remap_idx(data$idx_nat, c(data$n_rep_nat), pp)
  k_new <- match(c(data$kd_kd), kp)
  o <- order(c(data$poll_kd), k_new)
  out$poll_kd <- same_shape(c(data$poll_kd)[o], data$poll_kd)
  out$kd_kd <- same_shape(k_new[o], data$kd_kd)
  out$grid_kd <- same_shape(c(data$grid_kd)[o], data$grid_kd)
  out$house_kd <- same_shape(c(data$house_kd)[o], data$house_kd)
  out$y_kd <- data$y_kd[o, pp, drop = FALSE]
  out$n_rep_kd <- same_shape(c(data$n_rep_kd)[o], data$n_rep_kd)
  out$idx_kd <- remap_idx(data$idx_kd[o, , drop = FALSE], c(data$n_rep_kd)[o], pp)
  out$y_res <- data$y_res[, kp, pp, drop = FALSE]
  out$idx_res <- remap_idx(data$idx_res, c(data$n_rep_res), pp)
  out$entry_epoch <- same_shape(c(data$entry_epoch)[pp], data$entry_epoch)
  out$d_shock <- data$d_shock[, pp, drop = FALSE]
  out$N_open <- same_shape(c(data$N_open)[kp], data$N_open)
  out$sf_idx <- same_shape(match(c(data$sf_idx), pp), data$sf_idx)
  out
}

#' A point and its data in another party and kjördæmi order, mapped through the natural
#' quantities, for the reference forms. The permuted data's party i is the input's party
#' `party_perm[i]`, and its kjördæmi k the input's `kd_perm[k]`. θ_1, μ_1, the houses' ε,
#' ν, ζ, r and the newcomers' η are permuted; `L_Omega` is re-derived as chol(Ω[pp, pp]),
#' and `z_walk` from the permuted θ increments in the new coordinates 2:P. The data keep the
#' builder's contract: counts, `exists`, `entry_epoch`, `d_shock`, `w` columns, `y_res`'s
#' kjördæmi and party dimensions and `N_open` permuted; reported-party lists re-derived by
#' identity; kjördæmi poll rows re-sorted by poll and new kjördæmi; `sf_idx` re-indexed.
#' @param pars a named list of `V2_CORE_PARS`
#' @param data a `v2_stan_data()` data list of the core in the reference forms
#' @param party_perm a permutation of 1:P
#' @param kd_perm a permutation of 1:K
#' @return `list(pars, data)` with attribute `ljac_L_Omega`: the change in
#'   log|∂Ω/∂L| = Σ_{k=2}^{P} (P − k) log L_kk, new minus old, which is the change in
#'   `v2_lp_ref()`
#' @export
permute_v2 <- function(pars, data, party_perm, kd_perm) {
  check_core(data)
  P <- data$P
  K <- data$K
  pp <- as.integer(party_perm)
  kp <- as.integer(kd_perm)
  check(length(pp) == P && setequal(pp, seq_len(P)), "party_perm is not a permutation of 1:P")
  check(length(kp) == K && setequal(kp, seq_len(K)), "kd_perm is not a permutation of 1:K")
  p <- core_pars(pars, data)
  nat <- natural(p, data)
  out_data <- permute_data(data, pp, kp)

  L <- chol_of_gram(p$L_Omega[pp, , drop = FALSE])
  sigma <- p$sigma[pp]
  # WHY: z_walk is solved against the factor and steps the reference computes from the new
  # point, then refined, so that the reference's walk reproduces the permuted steps to a
  # rounding; a separately computed factor would leave errors of order eps cond(L_walk).
  L_walk <- walk_factor(sigma, L)
  scale <- walk_scale(p$tau_camp, data)
  target <- nat$step[pp, , drop = FALSE][-1, , drop = FALSE]
  z_walk <- forwardsolve(L_walk, target / rep(scale, each = P - 1))
  for (i in 1:3) z_walk <- z_walk - forwardsolve(L_walk, (walk_steps(L_walk, z_walk, scale) - target) / rep(scale, each = P - 1))

  entry <- c(data$entry_epoch)
  new_old <- which(entry > 0)
  new_new <- which(entry[pp] > 0)
  out_pars <- list(
    theta1 = p$theta1[pp], z_walk = z_walk, sigma = sigma, L_Omega = L, tau_camp = p$tau_camp,
    mu1 = p$mu1[pp], house_raw = p$house_raw[, pp, drop = FALSE], sigma_gamma = p$sigma_gamma,
    new_raw = p$new_raw[match(pp[new_new], new_old)], log_phi_u = p$log_phi_u, mu_phi = p$mu_phi,
    sigma_phi = p$sigma_phi, log_phi_K_u = p$log_phi_K_u, log_kappa_u = p$log_kappa_u,
    delta_raw = array(p$delta_raw[kp, pp, drop = FALSE], c(1L, K, P)), s_delta0_raw = p$s_delta0_raw[pp],
    mu_sd = p$mu_sd, tau_sd = p$tau_sd, nu = p$nu[, pp, drop = FALSE]
  )
  ljac <- function(L) sum((P - 2:P) * log(diag(L)[2:P]))
  structure(list(pars = out_pars, data = out_data), ljac_L_Omega = ljac(L) - ljac(p$L_Omega))
}
