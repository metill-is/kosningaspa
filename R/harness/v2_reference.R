# Arm V2's R reference of Stan/forecast_v2.stan's one configuration (spec §6.2–§6.7): the
# sum-to-zero walk with the campaign multiplier, the national polls on the vote-weighted share
# times exp(bias) with a constant industry bias and constant house deviations, the static
# kjördæmi deviation through each epoch's M_e, the kjördæmi polls, the closed elections'
# results, the fundamentals pseudo-observation (§6.5) and the bounded concentrations. It is
# written from the spec and never from the Stan file, so that the Stan file is tested against
# an independent oracle: the log density that log_prob(u, jacobian = FALSE) evaluates, a prior
# simulator, the kjördæmi map M_e, the failure modes the tests use as mutants, and the map of a
# point and its data to other party and kjördæmi orders.

#' The model's parameters, in the declaration order of Stan/forecast_v2.stan, with their Stan
#' types: the Stan file's parameters block declares the same names in the same order with the
#' same types, which the declaration test in tests/harness/test-v2-reference.R pins.
#' `constrain_variables(..., transformed_parameters = FALSE)` returns a list with these names
#' in this order, and `v2_lp_ref()` reads one. `z_walk` holds the walk's innovations in
#' coordinates 2:P, one column per grid step. `N_new` is the number of parties with
#' `entry_epoch` > 0, and `new_raw` holds one element per such party, in party order.
#' `delta_raw` holds the one ζ of the static kjördæmi deviation, mapped through every epoch's
#' M_e. `s_delta0_raw` is r, with log s_δ0 = μ_sd + τ_sd r.
#' @export
V2_CORE_PARS <- tibble::tibble(
  name = c(
    "theta1", "z_walk", "sigma", "L_Omega", "tau_camp", "mu1", "house_raw", "sigma_gamma", "new_raw", "log_phi_u",
    "mu_phi", "sigma_phi", "log_phi_K_u", "log_kappa_u", "delta_raw", "s_delta0_raw", "mu_sd", "tau_sd", "nu"
  ),
  type = c(
    "sum_to_zero_vector[P]", "matrix[P - 1, G - 1]", "vector<lower=0>[P]", "cholesky_factor_corr[P]", "real<lower=0>",
    "sum_to_zero_vector[P]", "matrix[H, P]", "real<lower=0>", "vector[N_new]", "vector[H]", "real", "real<lower=0>",
    "real", "real", "array[1] matrix[K, P]", "vector[P]", "real", "real<lower=0>", "array[J] sum_to_zero_vector[P]"
  )
)

#' One row per failure mode the tests use as a mutant of `v2_lp_ref()`: `mutant`, `mistake`
#' (what a Stan file matching it would have got wrong) and `min_closed` (the fewest closed
#' elections at which it can change the density):
#' - `positional_gather`: each likelihood row's counts `y[1:n_rep]` read against the first
#'   `n_rep` parties, at all three levels, instead of the row's reported parties by identity;
#' - `previous_w`: closed epochs from the second on use the previous election's kjördæmi
#'   weights w;
#' - `next_epoch_delta`: the results and kjördæmi polls of closed epoch e use δ_{e+1};
#' - `close_last_kjordaemi`: δ closes on the last kjördæmi instead of through M_e;
#' - `conditional_walk`: the walk's innovation covariance is the conditional
#'   Σ − Σ11ᵀΣ / 1ᵀΣ1 of a normal placed directly on a sum-to-zero innovation, not CΣC;
#' - `no_leaving_step`: the step leaving each closed election's date is not scaled by the
#'   campaign multiplier, only the steps inside a campaign are. Every closed election in the
#'   window has a step leaving it, so every cell exercises it;
#' - `newcomer_mean_of_mu1`: a newcomer entering in epoch e_p > 1 takes the mean over μ_1 of
#'   the parties existing before e_p, instead of over the current bias, where an earlier
#'   newcomer's entry value and the re-centring after it have moved them. It needs a newcomer
#'   entering after another one entered in an epoch after the first;
#' - `newcomer_mean_over_all`: a newcomer's entry value takes the mean of the bias over every
#'   party, the newcomer and the parties entering later included, instead of over the parties
#'   existing before e_p. The bias sums to zero going into every entry epoch, so that mean is 0
#'   to rounding: the mutant drops the base, and the entry value is s_new η alone. Any newcomer
#'   entering in the window exercises it;
#' - `fund_uncentred`: the fundamentals pseudo-observation's mean is log π_nat,E[S_f] itself,
#'   not centred over S_f. The mean of the log of shares below 1 is negative, so every cell
#'   exercises it.
#' @export
V2_MUTANTS <- tibble::tibble(
  mutant = c(
    "positional_gather", "previous_w", "next_epoch_delta", "close_last_kjordaemi", "conditional_walk",
    "no_leaving_step", "newcomer_mean_of_mu1", "newcomer_mean_over_all", "fund_uncentred"
  ),
  mistake = c(
    "counts gathered by position, not by the reported parties",
    "a closed epoch weighted by the previous election's w",
    "a closed epoch's results and kjordaemi polls read the next epoch's delta",
    "delta closed on the last kjordaemi, not through M_e",
    "the walk's step covariance the conditional normal's, not C Sigma C",
    "the step leaving a closed election not scaled by the campaign multiplier",
    "a newcomer's entry mean taken over mu_1, not the current bias",
    "a newcomer's entry mean taken over every party, not those existing before it",
    "the fundamentals mean not centred over S_f"
  ),
  min_closed = c(1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L)
)

# The §6.10 priors and the bounded transforms (Decision 4) that are literals, not data.
THETA1_SD <- 2
SIGMA_DF <- 3
SIGMA_SCALE <- 0.02
LKJ_ETA <- 10
TAU_CAMP_RATE <- 1
NU_SD <- 2
#' A newcomer's bias contrast has mean 0 and scale S_NEW, the median of half-N(0, S_NEW_SD) to
#' four digits (§6.4); `tests/harness/test-v2-reference.R` ties the two.
S_NEW <- 0.2023
S_NEW_SD <- 0.3
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

#' The caps of the bounded log concentrations (Decision 4), named as their columns in
#' `param_draws.parquet`: log φ_h (one column per house), log φ_K and log κ. The one place R
#' reads them from; `R/harness/compare_runs.R`'s stop rule counts the draws within 0.5 of
#' them.
#' @export
V2_LOG_CAPS <- c(log_phi = LOG_PHI_CAP, log_phi_K = LOG_PHI_K_CAP, log_kappa = LOG_KAPPA_CAP)

#' The cap of stage 1's bounded log concentration (Decision 4), log φ_f < 20, which
#' `Stan/fundamentals_v2.stan` sets as `phi_f_inv > exp(-20)`. Stage 1 is not V2's model, so
#' it is kept apart from `V2_LOG_CAPS`, whose names are `param_draws.parquet` columns;
#' `R/harness/stage1.R` counts its draws near it.
#' @export
V2_STAGE1_LOG_CAP <- c(log_phi_f = 20)

#' Decision 4's reporting margin: the Tier 0 checks (`R/harness/archive/v2_checks.R`) report the draws
#' of each bounded log concentration within this of its cap, log c > cap − 2, beside the stop
#' rule's 0.5 (`NEAR_CAP`, `R/harness/compare_runs.R`).
#' @export
V2_CAP_MARGIN <- 2

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
#' @param B a matrix with full row rank
#' @return the lower-triangular factor, `nrow(B)` square
#' @export
chol_of_gram <- function(B) {
  q <- qr(t(B), tol = 0)
  check(identical(q$pivot, seq_len(ncol(t(B)))), "chol_of_gram: the QR decomposition pivoted")
  R <- qr.R(q)
  t(R * sign(diag(R)))
}

#' The walk's steps in coordinates 2:P, sqrt(Δ_i) s_i L_walk z_i, one column per grid step.
walk_steps <- function(L_walk, z, scale) (L_walk %*% z) * rep(scale, each = nrow(L_walk))

#' Which grid steps the campaign multiplier scales (§6.2): those inside an election's
#' campaign (`data$campaign`), and the step leaving each closed election, the one that starts
#' on its grid date `data$grid_res[j]`.
#' @param data a `v2_stan_data()` data list: `G`, `campaign` and `grid_res` are read
#' @return the G − 1 flags, 0 or 1, one per grid step
#' @export
campaign_steps <- function(data) {
  # WHY: R0 marks a step by the date it starts on, and an election's date is inside its own
  # campaign, so the step that leaves a closed election moves at the campaign's pace too.
  leaves_election <- seq_len(data$G - 1L) %in% c(data$grid_res)
  as.integer(c(data$campaign) == 1L | leaves_election)
}

#' The walk's per-step scale sqrt(Δ_i) s_i, with s_i = 1 + τ_camp c_i and c_i the step's flag
#' of `campaign_steps()` (§6.2).
#' @param tau_camp the campaign multiplier τ_camp
#' @param data a `v2_stan_data()` data list
#' @param flags the steps' flags; the default is the model's, and the `no_leaving_step` mutant
#'   passes `data$campaign` alone
#' @return the G − 1 scales, one per grid step
#' @export
walk_scale <- function(tau_camp, data, flags = campaign_steps(data)) {
  sqrt(c(data$delta_days)) * (1 + tau_camp * c(flags))
}

#' The rows 2:P of C diag(σ) L, C = I − 11ᵀ/P: their Gram matrix is (CΣC)[2:P, 2:P] for
#' Σ = diag(σ) Ω diag(σ), Ω = LLᵀ.
#' @param sigma the P walk scales σ
#' @param L_Omega the Cholesky factor of Ω, P × P
#' @return a (P − 1) × P matrix
#' @export
walk_rows <- function(sigma, L_Omega) {
  M <- sigma * L_Omega
  (M - matrix(colMeans(M), nrow(M), ncol(M), byrow = TRUE))[-1, , drop = FALSE]
}

#' The factor L_walk = chol((CΣC)[2:P, 2:P]) of Σ = diag(σ) Ω diag(σ), Ω = LLᵀ, computed as
#' the Cholesky factor of the Gram matrix of the rows 2:P of C diag(σ) L.
walk_factor <- function(sigma, L_Omega) chol_of_gram(walk_rows(sigma, L_Omega))

#' The scales of the fundamentals pseudo-observation (§6.5) at the walk's σ, L_Ω and τ_camp,
#' for the parties of S_f (`data$sf_idx`, in that order): v_p = diag(CΣCᵀ)_p, each party's
#' variance of a walk step per unit Δ s² (§6.2), with Σ = diag(σ) Ω diag(σ), Ω = L_Ω L_Ωᵀ and
#' C = I − 11ᵀ/P over all P parties, formed here as the matrix product; the walk's variance
#' over `weight_time` days whose last `campaign_days` are in the campaign, V_w,p =
#' v_p [(weight_time − campaign_days) + campaign_days (1 + τ_camp)²]; and
#' τ_f,p = √(V_w,p (1 − w) / w), w = `desired_weight`, the sd that gives the pseudo-observation
#' the weight w against V_w: (1/τ_f²) / (1/τ_f² + 1/V_w) = w. The window ends at E, so no step
#' leaving a closed election falls in it.
#' @param sigma the P walk scales σ
#' @param L_Omega the Cholesky factor of Ω (P × P, or its P² elements column-major)
#' @param tau_camp the campaign multiplier τ_camp
#' @param data a `v2_stan_data()` data list: `P`, `sf_idx`, `weight_time`, `campaign_days`
#'   and `desired_weight` are read
#' @return `list(v, V_w, tau_f)`, each as long as `data$sf_idx`
#' @export
fund_scales <- function(sigma, L_Omega, tau_camp, data) {
  P <- data$P
  Sigma <- tcrossprod(as.numeric(sigma) * matrix(as.numeric(L_Omega), P, P))
  C <- diag(P) - 1 / P
  v <- diag(C %*% Sigma %*% C)[as.integer(c(data$sf_idx))]
  V_w <- v * ((data$weight_time - data$campaign_days) + data$campaign_days * (1 + as.numeric(tau_camp))^2)
  w <- data$desired_weight
  list(v = v, V_w = V_w, tau_f = sqrt(V_w * (1 - w) / w))
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

#' The dimensions of the model's parameters on `data`, as cmdstanr's `stan_variable_sizes`
#' gives them (1 for a scalar), named and ordered as `V2_CORE_PARS`.
#' @param data a `v2_stan_data()` data list
#' @return a named list of integer vectors
#' @export
v2_par_dims <- function(data) {
  P <- as.integer(data$P)
  list(
    theta1 = P, z_walk = c(P - 1L, as.integer(data$G) - 1L), sigma = P, L_Omega = c(P, P), tau_camp = 1L, mu1 = P,
    house_raw = c(as.integer(data$H), P), sigma_gamma = 1L, new_raw = sum(c(data$entry_epoch) > 0),
    log_phi_u = as.integer(data$H), mu_phi = 1L, sigma_phi = 1L, log_phi_K_u = 1L, log_kappa_u = 1L,
    delta_raw = c(1L, as.integer(data$K), P), s_delta0_raw = P, mu_sd = 1L, tau_sd = 1L, nu = c(as.integer(data$J), P)
  )
}

#' Stop unless `data` holds what the reference reads beyond the counts (the closed elections'
#' grid dates before E, and the fundamentals' parties, values and weights), and `mutant` is
#' "none" or one of `v2_mutants(data)`.
check_core <- function(data, mutant = "none") {
  gr <- as.integer(c(data$grid_res))
  check(
    length(gr) == data$n_closed && all(gr %in% seq_len(data$G - 1L)),
    "data$grid_res must be n_closed grid dates before E"
  )
  sf <- as.integer(c(data$sf_idx))
  check(
    identical(as.integer(data$n_sf), length(sf)) && length(sf) >= 2 && all(sf %in% seq_len(data$P)) && !anyDuplicated(sf),
    "the fundamentals need at least two distinct parties of S_f (n_sf and sf_idx)"
  )
  check(all(data$exists[data$G, sf] == 1L), "the fundamentals need every party of S_f (sf_idx) to exist at E")
  check(length(data$mu_f) == length(sf) && all(is.finite(c(data$mu_f))), "data$mu_f must be n_sf finite numbers")
  w <- data$desired_weight
  check(length(w) == 1 && is.finite(w) && w > 0 && w < 1, "data$desired_weight must be one number in (0, 1)")
  check(
    length(data$weight_time) == 1 && length(data$campaign_days) == 1 && is.finite(data$weight_time) &&
      data$weight_time >= data$campaign_days && data$campaign_days >= 1,
    "data$weight_time must be at least data$campaign_days, itself at least 1"
  )
  check(
    is.character(mutant) && length(mutant) == 1 && mutant %in% c("none", v2_mutants(data)),
    paste0("mutant ", mutant, " is not one of none, ", paste(v2_mutants(data), collapse = ", "))
  )
}

#' `pars` as plain numeric vectors and matrices, after checking its names, lengths, bounds
#' and constraints: each `V2_CORE_PARS` name present with `v2_par_dims()`' length (in
#' cmdstanr's column-major order), nothing else of non-zero length, the sum-to-zero vectors
#' summing to 0, the lower-bounded scalars positive, `L_Omega` lower triangular with a
#' positive diagonal and unit-norm rows. `delta_raw` becomes a 1 × K × P array, which
#' constrain_variables() gives flat in column-major order.
core_pars <- function(pars, data) {
  dims <- v2_par_dims(data)
  missing <- setdiff(names(dims), names(pars))
  if (length(missing)) fail("pars lacks ", listed(missing))
  extra <- setdiff(names(pars), names(dims))
  extra <- extra[lengths(pars[extra]) > 0]
  if (length(extra)) fail("pars has parameters outside the model: ", listed(extra))
  for (n in names(dims)) {
    if (length(pars[[n]]) != prod(dims[[n]])) fail(n, " has length ", length(pars[[n]]), ", not ", prod(dims[[n]]))
    if (!all(is.finite(as.numeric(pars[[n]])))) fail(n, " is not finite")
  }
  as_matrix <- function(n) matrix(as.numeric(pars[[n]]), dims[[n]][1], dims[[n]][2])
  p <- lapply(pars[names(dims)], as.numeric)
  for (n in c("z_walk", "L_Omega", "house_raw", "nu")) p[[n]] <- as_matrix(n)
  p$delta_raw <- array(as.numeric(pars$delta_raw), dims$delta_raw)
  zero_sum <- function(x) abs(sum(x)) <= 1e-8 * max(1, abs(x))
  check(zero_sum(p$theta1), "theta1 does not sum to 0")
  check(zero_sum(p$mu1), "mu1 does not sum to 0")
  check(all(apply(p$nu, 1, zero_sum)), "a row of nu does not sum to 0")
  for (n in c("sigma", "tau_camp", "sigma_gamma", "sigma_phi", "tau_sd")) {
    check(all(p[[n]] > 0), paste(n, "must be positive"))
  }
  L <- p$L_Omega
  check(
    all(L[upper.tri(L)] == 0) && all(diag(L) > 0) && all(abs(rowSums(L^2) - 1) <= 1e-8),
    "L_Omega is not the Cholesky factor of a correlation matrix"
  )
  p
}

#' The natural quantities of a checked point `p` (`core_pars()`) on `data`: the walk θ
#' (P × G), its steps and innovation factor, μ_e, γ_{e,h}, δ_e, the bounded log
#' concentrations, `phi_init`, s_δ0 and π_nat at each national poll's date. `conditional`,
#' `no_leaving`, `close_last`, `entry_mean_mu1` and `entry_mean_all` switch to the
#' `conditional_walk`, `no_leaving_step`, `close_last_kjordaemi`, `newcomer_mean_of_mu1` and
#' `newcomer_mean_over_all` mutants.
natural <- function(p, data, conditional = FALSE, no_leaving = FALSE, close_last = FALSE, entry_mean_mu1 = FALSE,
                    entry_mean_all = FALSE) {
  P <- data$P
  H <- data$H
  K <- data$K
  n_epoch <- data$n_epoch
  entry <- c(data$entry_epoch)

  # §6.2: coordinates 2:P step by sqrt(Δ_i) s_i L_walk z_i, L_walk = chol((CΣC)[2:P, 2:P]),
  # and party 1 by minus their sum, so every θ_t sums to 0 and each step's covariance is
  # Δ_i s_i² CΣC; s_i = 1 + τ_camp c_i (walk_scale()). The sums are compensated, so θ does
  # not depend on the party order beyond a rounding.
  L_walk <- if (conditional) {
    Sigma <- tcrossprod(p$sigma * p$L_Omega)
    V <- Sigma - Sigma %*% matrix(1, P, P) %*% Sigma / sum(Sigma)
    t(chol(V[-1, -1, drop = FALSE]))
  } else {
    walk_factor(p$sigma, p$L_Omega)
  }
  scale <- if (no_leaving) walk_scale(p$tau_camp, data, data$campaign) else walk_scale(p$tau_camp, data)
  step <- walk_steps(L_walk, p$z_walk, scale)
  step <- rbind(-colsums_exact(step), step)
  theta <- cumsum_rows(p$theta1, step)

  # §6.4, the industry bias: μ_1, with no step between epochs. From its entry epoch e_p on, a
  # newcomer's bias is the mean of the current bias over the parties existing before e_p
  # (present at the window's start or entering earlier), plus s_new η with s_new = 0.2023;
  # then the bias is re-centred.
  new <- which(entry > 0)
  mu <- matrix(0, n_epoch, P)
  m <- p$mu1
  for (e in seq_len(n_epoch)) {
    entering <- which(entry == e)
    if (length(entering)) {
      base <- if (entry_mean_mu1 && e > 1) mu[1, ] else m
      existing <- if (entry_mean_all) seq_len(P) else which(entry < e)
      m[entering] <- mean(base[existing]) + S_NEW * p$new_raw[match(entering, new)]
    }
    m <- centre(m)
    mu[e, ] <- m
  }
  # §6.4, the houses: γ_{e,h} = μ_e + d_h, d_h = C(σ_γ ε_h) in every epoch; a newcomer's slot
  # holds its deviation from its entry on.
  gamma <- array(0, c(n_epoch, H, P))
  for (h in seq_len(H)) {
    d_h <- centre(p$sigma_gamma * p$house_raw[h, ])
    for (e in seq_len(n_epoch)) gamma[e, h, ] <- d_h + mu[e, ]
  }

  # §6.7: δ_e = M_e(s_δ0 ⊙ ζ), one ζ for every epoch.
  s_delta0 <- exp(p$mu_sd + p$tau_sd * p$s_delta0_raw)
  X <- matrix(p$delta_raw[1, , ], K, P) * matrix(s_delta0, K, P, byrow = TRUE)
  map <- if (close_last) close_on_last else M_e
  delta <- array(0, c(n_epoch, K, P))
  for (e in seq_len(n_epoch)) delta[e, , ] <- map(X, data$w[e, ])

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
#' @param data a `v2_stan_data()` data list
#' @return a list: `theta` (P × G), `pi_nat` (N_nat × P: π_nat at each national poll's
#'   date), `mu` (n_epoch × P, each row summing to 0), `gamma` (n_epoch × H × P), `delta`
#'   (n_epoch × K × P), `log_phi` and `phi` (H), `log_phi_K` and `phi_K`, `log_kappa` and
#'   `kappa`, and `s_delta0` (P)
#' @export
v2_natural <- function(pars, data) {
  check_core(data)
  nat <- natural(core_pars(pars, data), data)
  list(
    theta = nat$theta, pi_nat = nat$pi_nat, mu = nat$mu, gamma = nat$gamma, delta = nat$delta,
    log_phi = nat$log_phi, phi = exp(nat$log_phi), log_phi_K = nat$log_phi_K, phi_K = exp(nat$log_phi_K),
    log_kappa = nat$log_kappa, kappa = exp(nat$log_kappa), s_delta0 = nat$s_delta0
  )
}

#' Every likelihood row, each `list(field, at, S, alpha)`: the count field and its row (`at`;
#' (e, k) for a result), the parties S its counts are gathered by and its
#' Dirichlet-multinomial concentrations over S. National poll n (§6.3):
#' φ_h normalise(π_nat[S] ⊙ exp(γ_{e,h}[S])). Kjördæmi poll row r (§6.7):
#' κ φ_h softmax over S of (ν_j + δ_{e,k}). Result (e, k) (§6.6):
#' φ_K softmax over S of (θ_{t_e} + δ_{e,k}), with no election-day shock. S is the row's
#' reported parties by identity, and e the epoch of the row's date. `positional` and
#' `next_delta` switch to the `positional_gather` and `next_epoch_delta` mutants.
dm_rows <- function(p, nat, data, positional = FALSE, next_delta = FALSE) {
  parties <- function(idx, n_rep, i) if (positional) seq_len(n_rep[i]) else idx[i, seq_len(n_rep[i])]
  delta_epoch <- function(e) if (next_delta && e <= data$n_closed) e + 1L else e
  phi <- exp(nat$log_phi)
  rows <- vector("list", data$N_nat + data$N_kd + data$n_closed * data$K)
  i <- 0L
  for (n in seq_len(data$N_nat)) {
    h <- data$house_nat[n]
    S <- parties(data$idx_nat, data$n_rep_nat, n)
    a <- nat$pi_nat[n, S] * exp(nat$gamma[data$grid_epoch[data$grid_nat[n]], h, S])
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
      eta <- nat$theta[S, t] + nat$delta[de, k, S]
      rows[[i]] <- list(field = "y_res", at = c(e, k), S = S, alpha = phi_K * softmax(eta))
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

#' The fundamentals pseudo-observation's normal (§6.5) at a checked point `p` with its natural
#' quantities `nat`: μ_f ~ N(x_c, τ_f) over S_f (`data$sf_idx`, in that order), where
#' x = log π_nat,E[S_f], π_nat,E = Σ_k w_open,k softmax over A_E of (θ_E + δ_open,k) (θ_E the
#' walk at grid date G, which is E; δ_open and w_open the open epoch's; A_E the parties
#' existing at E), x_c = x − mean(x), and τ_f is `fund_scales()`'. `uncentred` switches to
#' the `fund_uncentred` mutant, x in place of x_c.
#' @return `list(mean, tau_f)`, each as long as `data$sf_idx`
fund_normal <- function(p, nat, data, uncentred = FALSE) {
  G <- data$G
  e <- data$n_epoch
  A <- which(data$exists[G, ] == 1L)
  share <- numeric(data$P)
  for (k in seq_len(data$K)) share[A] <- share[A] + data$w[e, k] * softmax(nat$theta[A, G] + nat$delta[e, k, A])
  x <- log(share[as.integer(c(data$sf_idx))])
  list(mean = if (uncentred) x else x - mean(x), tau_f = fund_scales(p$sigma, p$L_Omega, p$tau_camp, data)$tau_f)
}

#' The fundamentals pseudo-observation's log density, Σ_p log N(μ_f,p | x_c,p, τ_f,p) over S_f
#' with its normalising constant (`fund_normal()`). It is a likelihood term on data, so it has
#' no Jacobian; τ_f depends on σ, Ω and τ_camp, so its −log τ_f terms count.
fund_lp <- function(p, nat, data, uncentred = FALSE) {
  f <- fund_normal(p, nat, data, uncentred)
  sum(stats::dnorm(c(data$mu_f), f$mean, f$tau_f, log = TRUE))
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

#' The reference log density of V2's model (spec §6.2–§6.7): what Stan's
#' `log_prob(u, jacobian = FALSE)` evaluates, up to a constant. It is the density of the
#' parameters on their constrained values (the §6.10 priors, `lkj_corr_cholesky(10)` on L
#' itself, the normals on every element of each sum-to-zero vector) plus the log-Jacobians
#' log(cap − init) + u of the three bounded concentration transforms, which the model block
#' adds by hand, plus the national-poll, kjördæmi-poll and result likelihoods (`dm_rows()`),
#' each a Dirichlet-multinomial gathered by party identity, plus the fundamentals
#' pseudo-observation μ_f ~ N(x_c, τ_f) over S_f (§6.5, `fund_normal()` and `fund_scales()`);
#' every normalising constant is included. It leaves out the automatic Jacobians of Stan's own
#' constraints (lower = 0, cholesky_factor_corr, sum_to_zero), which `jacobian = FALSE`
#' excludes. Terms are summed with a compensated sum, so the result does not depend on the
#' party or kjördæmi order beyond a rounding.
#' @param pars a named list as `constrain_variables(..., transformed_parameters = FALSE)`
#'   returns it: the `V2_CORE_PARS`, each in cmdstanr's column-major order
#' @param data a `v2_stan_data()` data list, with at least two distinct parties in `sf_idx`,
#'   each existing at E, and `mu_f` as long, finite
#' @param mutant "none", or a mutant of `v2_mutants(data)` (`V2_MUTANTS`)
#' @return numeric(1). An error, "v2 reference: ...", for another mutant, fundamentals data it
#'   cannot read, or a point with a missing, misshapen or out-of-bounds parameter
#' @export
v2_lp_ref <- function(pars, data, mutant = "none") {
  check_core(data, mutant)
  p <- core_pars(pars, data)
  if (mutant == "previous_w") data$w[2:data$n_closed, ] <- data$w[seq_len(data$n_closed - 1), , drop = FALSE]
  nat <- natural(
    p, data,
    conditional = mutant == "conditional_walk", no_leaving = mutant == "no_leaving_step",
    close_last = mutant == "close_last_kjordaemi", entry_mean_mu1 = mutant == "newcomer_mean_of_mu1",
    entry_mean_all = mutant == "newcomer_mean_over_all"
  )
  rows <- dm_rows(p, nat, data, positional = mutant == "positional_gather", next_delta = mutant == "next_epoch_delta")
  lik <- vapply(rows, \(r) dirmult_lpmf(row_counts(data, r), r$alpha), numeric(1))
  fund <- fund_lp(p, nat, data, uncentred = mutant == "fund_uncentred")
  neumaier(c(prior_terms(p, nat, data), lik, fundamentals = fund))
}

#' The mutants of `V2_MUTANTS` that can change the density on `data`: those whose
#' `min_closed` is at most `data$n_closed`.
#' @param data a `v2_stan_data()` data list
#' @return a character vector
#' @export
v2_mutants <- function(data) V2_MUTANTS$mutant[V2_MUTANTS$min_closed <= data$n_closed]

#' A sum-to-zero vector of length n with normal(0, sd) on every element: sd · C z, which is
#' isotropic on the sum-to-zero subspace.
rstz <- function(n, sd) centre(stats::rnorm(n, 0, sd))

#' A prior draw of the model's parameters on `data`, in `V2_CORE_PARS` order and cmdstanr's
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

#' A draw from V2's model: parameters from the prior, and, when `observe`, every count and
#' `mu_f` from the model given them.
#' @param data a `v2_stan_data()` data list
#' @param seed the seed; the draw uses its own RNG stream (`withr::with_seed()`), leaving the
#'   caller's untouched
#' @param observe whether to replace the counts and `mu_f`
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
#'     reported. `mu_f` is replaced too, after every count, by one draw of N(x_c, τ_f)
#'     (`fund_normal()`). Every other field is unchanged
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
      f <- fund_normal(p, nat, data)
      sim$mu_f <- same_shape(stats::rnorm(length(f$mean), f$mean, f$tau_f), data$mu_f)
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
  out$N_open <- same_shape(c(data$N_open)[kp], data$N_open)
  out$sf_idx <- same_shape(match(c(data$sf_idx), pp), data$sf_idx)
  out
}

#' A point and its data in another party and kjördæmi order, mapped through the natural
#' quantities. The permuted data's party i is the input's party `party_perm[i]`, and its
#' kjördæmi k the input's `kd_perm[k]`. θ_1, μ_1, the houses' ε, ν, ζ, r and the newcomers' η
#' are permuted (the scales are not party-indexed); `L_Omega` is re-derived as
#' chol(Ω[pp, pp]), and `z_walk` from the permuted θ increments in the new coordinates 2:P.
#' The data keep the builder's contract: counts, `exists`, `entry_epoch`, `w` columns,
#' `y_res`'s kjördæmi and party dimensions and `N_open` permuted; reported-party lists
#' re-derived by identity; kjördæmi poll rows re-sorted by poll and new kjördæmi; `sf_idx`
#' re-indexed.
#' @param pars a named list of `V2_CORE_PARS`
#' @param data a `v2_stan_data()` data list
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
    delta_raw = p$delta_raw[, kp, pp, drop = FALSE], s_delta0_raw = p$s_delta0_raw[pp],
    mu_sd = p$mu_sd, tau_sd = p$tau_sd, nu = p$nu[, pp, drop = FALSE]
  )
  ljac <- function(L) sum((P - 2:P) * log(diag(L)[2:P]))
  structure(list(pars = out_pars, data = out_data), ljac_L_Omega = ljac(L) - ljac(p$L_Omega))
}
