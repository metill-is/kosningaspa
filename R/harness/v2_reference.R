# Arm V2's R reference for the core (spec §6.2–§6.7 in the reference forms) and the components
# built so far (step v2s-m1's epoch bias, §6.4, switch epoch_bias; step v2s-m2's newcomer
# pooling, §6.4, switch newcomer_pool; step v2s-m3's election-day shock, §6.6, switch shock;
# step v2s-m4's δ walk across elections, §6.7, switch delta_walk; step v2s-m5's fundamentals
# pseudo-observation, §6.5, switch fundamentals; step v2s-m6's post-election multiplier, §6.2,
# switch post_flex; step v2s-m7's calendar drift of the bias, §6.4, switch bias_drift), written
# from the spec and
# never from Stan/forecast_v2.stan, so that the Stan file is tested against an independent
# oracle: the log density that log_prob(u, jacobian = FALSE) evaluates, a prior simulator, the
# kjördæmi map M_e, the failure modes the tests use as mutants, and the map of a point and its
# data to other party and kjördæmi orders.

box::use(
  R / harness / v2[V2_SWITCHES]
)

#' The core's parameters, in the declaration order of Stan/forecast_v2.stan, with their Stan
#' types: the Stan file's parameters block declares the same names in the same order with the
#' same types, which the declaration test in tests/harness/test-v2-reference.R pins.
#' `constrain_variables(..., transformed_parameters = FALSE)` returns a list with these names
#' in this order, and `v2_lp_ref()` reads one. `N_new` is the number of parties with
#' `entry_epoch` > 0, and `new_raw` holds one element per such party, in party order.
#' `delta_raw` holds δ's ζ: with `delta_walk` = 0 one, mapped through every epoch's M_e
#' (static δ); with 1 one per epoch, ζ_e of the walk (§6.7); with `delta_form` = 1 as well,
#' only the open epoch's. `s_delta0_raw` is r,
#' with log s_δ0 = μ_sd + τ_sd r. `anchor_w` is the anchored walk's (walk_form = 1, §6.9):
#' size 0 in the reference forms, where `z_walk` has its G − 1 columns. The epoch bias's
#' (§6.4, `epoch_bias` = 1) come next, each size 0 with `epoch_bias` = 0: `mu_step_raw` holds
#' ξ_e of the industry bias's steps, column e − 1 for e = 2..n_epoch; `house_step_raw` each
#' house's ζ_{e,h}, in the same columns; `tau_mu`, `tau_mu_A` (Annað's) and `tau_house` are
#' their scales, each an array of one. The newcomer pool's (§6.4, `newcomer_pool` = 1)
#' follow, each size 0 with `newcomer_pool` = 0: `m_new` and `s_new`, the mean and scale of a
#' newcomer's bias contrast m_new + s_new η, each an array of one. The shock's (§6.6,
#' `shock` = 1) come next, each size 0 with `shock` = 0: `z_shock`, P × n_closed, column e
#' holding closed election e's z_e, and `s_bar_E`, the shock's scale s̄_E, an array of one.
#' The δ walk's (§6.7, `delta_walk` = 1) come next, each size 0 with `delta_walk` = 0:
#' `s_zeta_raw`, the walk's per-party scales over s̄_ζ (s_ζ = s̄_ζ `s_zeta_raw`), and
#' `s_bar_zeta`, s̄_ζ, an array of one. `delta_coord` comes next, the δ walk held centred at the
#' closed elections (`delta_form` = 1, §6.7): n_closed × (K − 1) × (P − 1), each closed
#' election's coordinates of δ_e[2:K, 2:P]; size 0 in the reference forms. `tau_post` comes
#' next, the post-election multiplier τ_post (§6.2, `post_flex` = 1), an array of one, size 0
#' with `post_flex` = 0. The calendar drift's (§6.4, `bias_drift` = 1) are last, each size 0
#' with `bias_drift` = 0: `mu_drift_raw`, P × (n_bk − 1), column k − 1 holding ξ_k of the
#' industry bias's step into knot k; `house_drift_raw`, each house's ζ_{k,h} on the house
#' knots, H × P × (n_hk − 1); and `tau_drift`, `tau_drift_A` (Annað's) and `tau_house_drift`,
#' their scales per √year, each an array of one.
#' @export
V2_CORE_PARS <- tibble::tibble(
  name = c(
    "theta1", "anchor_w", "z_walk", "sigma", "L_Omega", "tau_camp", "mu1", "house_raw", "sigma_gamma", "new_raw",
    "log_phi_u", "mu_phi", "sigma_phi", "log_phi_K_u", "log_kappa_u", "delta_raw", "s_delta0_raw", "mu_sd",
    "tau_sd", "nu", "mu_step_raw", "house_step_raw", "tau_mu", "tau_mu_A", "tau_house", "m_new", "s_new", "z_shock",
    "s_bar_E", "s_zeta_raw", "s_bar_zeta", "delta_coord", "tau_post", "mu_drift_raw", "house_drift_raw", "tau_drift",
    "tau_drift_A", "tau_house_drift"
  ),
  type = c(
    "sum_to_zero_vector[P]", "array[walk_form * n_closed] vector[P - 1]", "matrix[P - 1, G - 1 - walk_form * n_closed]",
    "vector<lower=0>[P]", "cholesky_factor_corr[P]",
    "real<lower=0>", "sum_to_zero_vector[P]", "matrix[H, P]", "real<lower=0>", "vector[N_new]", "vector[H]",
    "real", "real<lower=0>", "real", "real", "array[1 + delta_walk * (n_epoch - 1) - delta_form * n_closed] matrix[K, P]",
    "vector[P]",
    "real", "real<lower=0>",
    "array[J] sum_to_zero_vector[P]", "matrix[P, epoch_bias * (n_epoch - 1)]",
    "array[H] matrix[P, epoch_bias * (n_epoch - 1)]", "array[epoch_bias] real<lower=0>",
    "array[epoch_bias] real<lower=0>", "array[epoch_bias] real<lower=0>", "array[newcomer_pool] real",
    "array[newcomer_pool] real<lower=0>", "matrix[P, shock * n_closed]", "array[shock] real<lower=0>",
    "vector<lower=0>[delta_walk * P]", "array[delta_walk] real<lower=0>", "array[delta_form * n_closed] matrix[K - 1, P - 1]",
    "array[post_flex] real<lower=0>", "matrix[P, bias_drift * (n_bk - 1)]", "array[H] matrix[P, bias_drift * (n_hk - 1)]",
    "array[bias_drift] real<lower=0>", "array[bias_drift] real<lower=0>", "array[bias_drift] real<lower=0>"
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
#'
#' The epoch bias's (`epoch_bias`, §6.4):
#' - `house_walk_from_first_poll`: every house's deviations walk from epoch 1, the window's
#'   start, instead of from its first epoch;
#' - `newcomer_mean_of_mu1`: a newcomer entering in epoch e_p > 1 takes the mean over μ_1
#'   of the parties existing before e_p, instead of over μ_{e_p};
#' - `no_mu_restart`: a newcomer's industry bias at its entry epoch e_p > 1 keeps the
#'   prior-only drift its slot gathered before e_p (its own elements of the centred steps
#'   of epochs 2..e_p) on top of its entry value, instead of restarting at the entry value;
#' - `no_house_restart`: a newcomer's level in each house walking before e_p keeps that
#'   house's prior-only drift, instead of restarting at σ_γ ε_{h,p} plus the mean drift of
#'   the parties existing before e_p;
#' - `house_restart_absolute`: a newcomer's level in each house walking before e_p restarts
#'   at σ_γ ε_{h,p} alone, the form `v2s-m1` built, so a drift common to the existing
#'   parties' levels before e_p, flat in every likelihood row before it, moves each of the
#'   newcomer's contrasts after it (`harness/archive/steps.md`, the post-review change).
#'
#' The newcomer pool's (`newcomer_pool`, §6.4):
#' - `newcomer_mean_over_all`: a newcomer's entry value takes the mean of μ_{e_p} over every
#'   party, the newcomer and the parties entering later included, instead of over the parties
#'   existing before e_p. μ sums to zero going into every entry epoch (μ_1 is sum-to-zero, the
#'   steps are centred and μ is re-centred after each epoch), so that mean is 0 to rounding:
#'   the mutant drops the base, and the entry value is m_new + s_new η alone. It does not test
#'   where the set of existing parties ends: the spec-valued test "a newcomer enters at the
#'   existing parties' mean plus the point's m_new + s_new eta", in
#'   `tests/harness/test-v2-reference.R`, pins the reference's set, entry_epoch < e_p, and the
#'   lp tests pin Stan to the reference.
#'   Any newcomer entering in the window exercises the mutant, the 2017 cells' (one closed
#'   election) included. With `bias_drift` = 1 it acts at each entry knot instead, where μ
#'   likewise sums to zero.
#'
#' The shock's (`shock`, §6.6):
#' - `shock_unscaled`: d ≡ 1, the shock C(s̄_E z_e) without the √(min(0.2 / π̄, 10)) scaling.
#'   Every closed election exercises it, the 2017 cells' one included, since no party's d is 1
#'   unless its π̄ is exactly 0.2. A party with no previous result reaches `shock_scaling()`
#'   (`R/harness/v2.R`) as NA, which its floor sets to π̄ = 0.02. Read as 0 instead, its d is
#'   √10 all the same (0.2 / 0 is capped at 10, the floor's 0.2 / 0.02), so that slip is no
#'   mutant; a dropped floor leaves an NA in d_shock, which the builder refuses.
#'
#' The δ walk's (`delta_walk`, §6.7):
#' - `delta_years_unscaled`: every step's scale is √1, not √(Δyears_e / Δ̄). Any step whose
#'   gap is not Δ̄ = 4 years exercises it, which every cell has (the 2017 cells' open epoch
#'   runs one year);
#' - `no_delta_restart`: a newcomer's column at its entry epoch e_p > 1 keeps the prior-only
#'   walk its slot gathered before e_p plus a step, instead of restarting at s_δ0,p ζ_{e_p,·,p}.
#'   Any newcomer entering after the first epoch exercises it, the 2017 cells' included;
#' - `delta_entry_leak`: each step starts from δ_{e−1} itself, not re-centred over the
#'   previous election's parties, so at an entry epoch the newcomer's earlier column and an
#'   offset shared by those parties reach the contrasts (`delta_epochs()`). Any newcomer
#'   entering after the first epoch exercises it, the 2017 cells' included.
#'
#' The fundamentals pseudo-observation's (`fundamentals`, §6.5):
#' - `fund_uncentred`: μ_f's mean is log π_nat,E[S_f] itself, not centred over S_f. The mean
#'   of the log of shares below 1 is negative, so every cell exercises it, the 2017 cells'
#'   (whose μ_f is the placeholder's zeros) included.
#'
#' The post-election multiplier's (`post_flex`, §6.2):
#' - `post_unscaled`: the walk's steps in a post window scale by √Δ (1 + τ_camp c) alone, τ_post
#'   left out of the walk while its prior stays. Any closed election whose post window holds a
#'   grid step exercises it, and the builder puts the window's end on the grid, so every cell
#'   with a closed election in its window does, the 2017 cells' included.
#'
#' The calendar drift's (`bias_drift`, §6.4):
#' - `drift_unscaled`: every knot step, the industry bias's and the houses', scales by √1, not
#'   √(years between knots). The knots are half a year and a year apart, so the industry
#'   bias's steps exercise it on every cell;
#' - `drift_left_knot`: a poll's bias is the left knots' alone, not interpolated towards the
#'   right ones. Any poll off a knot exercises it, which every cell has;
#' - `drift_house_from_start`: every house's deviations drift from knot 1 instead of from its
#'   first knot. Any house whose first national poll comes after the first house knot's
#'   interval exercises it;
#' - `drift_no_mu_restart`: a newcomer's industry bias at its entry knot keeps the prior-only
#'   drift its slot gathered before it (its own elements of the centred steps into knots
#'   2..k_p) on top of its entry value. Any newcomer entering after the first knot exercises it.
#' @export
V2_MUTANTS <- tibble::tibble(
  mutant = c(
    "positional_gather", "previous_w", "next_epoch_delta", "close_last_kjordaemi", "conditional_walk",
    "house_walk_from_first_poll", "newcomer_mean_of_mu1", "no_mu_restart", "no_house_restart",
    "house_restart_absolute", "newcomer_mean_over_all", "shock_unscaled", "delta_years_unscaled", "no_delta_restart",
    "delta_entry_leak", "fund_uncentred", "post_unscaled", "drift_unscaled", "drift_left_knot", "drift_house_from_start",
    "drift_no_mu_restart"
  ),
  switch = c(
    rep(NA_character_, 5), rep("epoch_bias", 5), "newcomer_pool", "shock", rep("delta_walk", 3), "fundamentals",
    "post_flex", rep("bias_drift", 4)
  ),
  min_closed = c(1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
)

#' The component switches the reference has (each 0 or 1), the switches of the components it
#' does not have yet (none: step v2s-m7 built the last), and the parameterisation switches
#' whose reference form is 0.
BUILT_SWITCHES <- c("epoch_bias", "newcomer_pool", "shock", "delta_walk", "fundamentals", "post_flex", "bias_drift")
FORM_SWITCHES <- c("walk_form", "delta_form")
COMPONENT_SWITCHES <- setdiff(V2_SWITCHES, c(FORM_SWITCHES, BUILT_SWITCHES))

# The §6.10 priors and the bounded transforms (Decision 4) that are literals, not data.
THETA1_SD <- 2
SIGMA_DF <- 3
SIGMA_SCALE <- 0.02
LKJ_ETA <- 10
TAU_CAMP_RATE <- 1
TAU_POST_RATE <- 1
NU_SD <- 2
#' The newcomer contrast with `newcomer_pool` = 0: m_new = 0 and s_new = 0.2023, the median
#' of half-N(0, 0.3) to four digits; with `newcomer_pool` = 1, m_new ~ N(0, 0.3) and
#' s_new ~ half-N(0, 0.3) (§6.10). `tests/harness/test-v2-reference.R` ties S_NEW to S_NEW_SD.
M_NEW <- 0
S_NEW <- 0.2023
M_NEW_SD <- 0.3
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
#' The epoch bias's literal scales (§6.10): τ_μ,A ~ half-N(0, 0.3) and τ_house ~ half-N(0, 0.1).
TAU_MU_A_SD <- 0.3
TAU_HOUSE_SD <- 0.1
#' The calendar drift's literal scales per √year (§6.10): τ_drift ~ half-N(0, 0.07), τ_drift,A
#' ~ half-N(0, 0.15) and τ_house,d ~ half-N(0, 0.05).
TAU_DRIFT_SD <- 0.07
TAU_DRIFT_A_SD <- 0.15
TAU_HOUSE_DRIFT_SD <- 0.05

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

#' The walk's per-step scale sqrt(Δ_i) s_i, with s_i = 1 + τ_camp c_i, plus τ_post q_i with
#' `post_flex` = 1, q_i the step's post-window flag `data$post` (§6.2).
#' @param tau_camp the campaign multiplier τ_camp
#' @param data a `v2_stan_data()` data list
#' @param tau_post the post-election multiplier τ_post, read only with `data$post_flex` = 1
#' @return the G − 1 scales, one per grid step
#' @export
walk_scale <- function(tau_camp, data, tau_post = 0) {
  s <- 1 + tau_camp * c(data$campaign)
  if (identical(as.integer(data$post_flex), 1L)) s <- s + as.numeric(tau_post) * c(data$post)
  sqrt(c(data$delta_days)) * s
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
#' v_p [(weight_time − campaign_days) + campaign_days (1 + τ_camp)²], and with `post_flex` = 1
#' v_p [(weight_time − campaign_days − d) + d (1 + τ_post)² + campaign_days (1 + τ_camp)²], d =
#' `weight_post_days`, the window's days in a post window; and
#' τ_f,p = √(V_w,p (1 − w) / w), w = `desired_weight`, the sd that gives the pseudo-observation
#' the weight w against V_w: (1/τ_f²) / (1/τ_f² + 1/V_w) = w.
#' @param sigma the P walk scales σ
#' @param L_Omega the Cholesky factor of Ω (P × P, or its P² elements column-major)
#' @param tau_camp the campaign multiplier τ_camp
#' @param data a `v2_stan_data()` data list: `P`, `sf_idx`, `weight_time`, `campaign_days`,
#'   `desired_weight`, `post_flex` and, with `post_flex` = 1, `weight_post_days` are read
#' @param tau_post the post-election multiplier τ_post, read only with `data$post_flex` = 1
#' @return `list(v, V_w, tau_f)`, each as long as `data$sf_idx`
#' @export
fund_scales <- function(sigma, L_Omega, tau_camp, data, tau_post = 0) {
  P <- data$P
  Sigma <- tcrossprod(as.numeric(sigma) * matrix(as.numeric(L_Omega), P, P))
  C <- diag(P) - 1 / P
  v <- diag(C %*% Sigma %*% C)[as.integer(c(data$sf_idx))]
  V_w <- if (identical(as.integer(data$post_flex), 1L)) {
    d <- data$weight_post_days
    v * ((data$weight_time - data$campaign_days - d) + d * (1 + as.numeric(tau_post))^2 +
      data$campaign_days * (1 + as.numeric(tau_camp))^2)
  } else {
    v * ((data$weight_time - data$campaign_days) + data$campaign_days * (1 + as.numeric(tau_camp))^2)
  }
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

#' The dimensions of the core's parameters on `data`, as cmdstanr's `stan_variable_sizes`
#' gives them (1 for a scalar), named and ordered as `V2_CORE_PARS`: with `walk_form` = 1,
#' `anchor_w` is n_closed × (P − 1) and `z_walk` (P − 1) × (G − 1 − n_closed); with 0,
#' `anchor_w` is 0 × (P − 1) and `z_walk` (P − 1) × (G − 1). With `epoch_bias` = 1,
#' `mu_step_raw` is P × (n_epoch − 1), `house_step_raw` H × P × (n_epoch − 1) and `tau_mu`,
#' `tau_mu_A` and `tau_house` 1 each; with 0, their last dimension is 0. With `newcomer_pool` =
#' 1, `m_new` and `s_new` are 1 each; with 0, 0. With `shock` = 1, `z_shock` is P × n_closed
#' and `s_bar_E` 1; with 0, `z_shock` is P × 0 and `s_bar_E` 0. With `delta_walk` = 1,
#' `delta_raw` is n_epoch × K × P, `s_zeta_raw` P and `s_bar_zeta` 1; with 0, `delta_raw` is
#' 1 × K × P and the other two 0. With `delta_form` = 1 (which needs `delta_walk` = 1),
#' `delta_coord` is n_closed × (K − 1) × (P − 1) and `delta_raw` 1 × K × P, the open epoch's ζ;
#' with 0, `delta_coord` is 0 × (K − 1) × (P − 1). With `post_flex` = 1, `tau_post` is 1; with
#' 0, 0. With `bias_drift` = 1, `mu_drift_raw` is P × (n_bk − 1), `house_drift_raw`
#' H × P × (n_hk − 1) and `tau_drift`, `tau_drift_A` and `tau_house_drift` 1 each; with 0,
#' their last dimension is 0.
#' @param data a `v2_stan_data()` data list
#' @return a named list of integer vectors
#' @export
v2_par_dims <- function(data) {
  P <- data$P
  wf <- as.integer(data$walk_form)
  check(length(wf) == 1 && wf %in% 0:1, "data$walk_form must be 0 or 1")
  n_anchor <- wf * as.integer(data$n_closed)
  eb <- as.integer(data$epoch_bias)
  check(length(eb) == 1 && eb %in% 0:1, "data$epoch_bias must be 0 or 1")
  n_step <- eb * (as.integer(data$n_epoch) - 1L)
  np <- as.integer(data$newcomer_pool)
  check(length(np) == 1 && np %in% 0:1, "data$newcomer_pool must be 0 or 1")
  sh <- as.integer(data$shock)
  check(length(sh) == 1 && sh %in% 0:1, "data$shock must be 0 or 1")
  dw <- as.integer(data$delta_walk)
  check(length(dw) == 1 && dw %in% 0:1, "data$delta_walk must be 0 or 1")
  df <- as.integer(data$delta_form)
  check(length(df) == 1 && df %in% 0:1, "data$delta_form must be 0 or 1")
  check(df <= dw, "data$delta_form = 1 needs delta_walk = 1")
  pf <- as.integer(data$post_flex)
  check(length(pf) == 1 && pf %in% 0:1, "data$post_flex must be 0 or 1")
  bd <- as.integer(data$bias_drift)
  check(length(bd) == 1 && bd %in% 0:1, "data$bias_drift must be 0 or 1")
  list(
    theta1 = P, anchor_w = c(n_anchor, P - 1L), z_walk = c(P - 1L, data$G - 1L - n_anchor), sigma = P,
    L_Omega = c(P, P), tau_camp = 1L, mu1 = P,
    house_raw = c(data$H, P), sigma_gamma = 1L, new_raw = sum(c(data$entry_epoch) > 0), log_phi_u = data$H,
    mu_phi = 1L, sigma_phi = 1L, log_phi_K_u = 1L, log_kappa_u = 1L,
    delta_raw = c(1L + dw * (as.integer(data$n_epoch) - 1L) - df * as.integer(data$n_closed), data$K, P),
    s_delta0_raw = P, mu_sd = 1L, tau_sd = 1L, nu = c(data$J, P), mu_step_raw = c(P, n_step),
    house_step_raw = c(data$H, P, n_step), tau_mu = eb, tau_mu_A = eb, tau_house = eb, m_new = np, s_new = np,
    z_shock = c(P, sh * as.integer(data$n_closed)), s_bar_E = sh, s_zeta_raw = dw * P, s_bar_zeta = dw,
    delta_coord = c(df * as.integer(data$n_closed), data$K - 1L, P - 1L), tau_post = pf,
    mu_drift_raw = c(P, bd * (as.integer(data$n_bk) - 1L)), house_drift_raw = c(data$H, P, bd * (as.integer(data$n_hk) - 1L)),
    tau_drift = bd, tau_drift_A = bd, tau_house_drift = bd
  )
}

#' Stop unless `data` is the core in the reference forms, with only the built component
#' switches (`BUILT_SWITCHES`) on, and `mutant` is "none" or one of `v2_mutants(data)`.
check_core <- function(data, mutant = "none") {
  forms <- vapply(FORM_SWITCHES, \(s) as.integer(data[[s]]), integer(1))
  check(all(forms == 0L), "the reference forms only: walk_form and delta_form must be 0")
  # WHY: no component switch is left unbuilt since v2s-m5; this refuses one added to V2_SWITCHES before the reference builds it.
  on <- COMPONENT_SWITCHES[vapply(COMPONENT_SWITCHES, \(s) !identical(as.integer(data[[s]]), 0L), logical(1))]
  if (length(on)) fail("switch(es) not in the reference yet: ", listed(on))
  built <- vapply(BUILT_SWITCHES, \(s) length(data[[s]]) == 1 && as.integer(data[[s]]) %in% 0:1, logical(1))
  if (!all(built)) fail("switch(es) neither 0 nor 1: ", listed(BUILT_SWITCHES[!built]))
  if (identical(as.integer(data$epoch_bias), 1L) || identical(as.integer(data$bias_drift), 1L)) {
    a <- data$annad_idx
    check(length(a) == 1 && a %in% 0:data$P, "data$annad_idx must be 0 or a party index")
  }
  if (identical(as.integer(data$bias_drift), 1L)) {
    check(identical(as.integer(data$epoch_bias), 0L), "data$bias_drift = 1 replaces the epoch walk, so it needs epoch_bias = 0")
    new <- c(data$entry_epoch) > 0
    knots <- function(n, lo, a, years, entry, what) {
      check(length(n) == 1 && n >= 2, paste0("data$n_", what, " must be at least 2"))
      check(length(lo) == data$G && all(lo %in% seq_len(n - 1)), paste0("data$", what, "_lo must be G knots in 1..n_", what, " - 1"))
      check(length(a) == data$G && all(is.finite(a) & a >= 0 & a <= 1), paste0("data$", what, "_a must be G weights in [0, 1]"))
      check(length(years) == 1 && is.finite(years) && years > 0, paste0("data$", what, "_step_years must be one positive number"))
      check(
        length(entry) == data$P && all(entry %in% 0:n) && identical(entry > 0, new),
        paste0("data$entry_", substr(what, 1, 2), " must be P knots, positive exactly for the newcomers")
      )
    }
    knots(data$n_bk, c(data$bk_lo), c(data$bk_a), data$bk_step_years, c(data$entry_bk), "bk")
    knots(data$n_hk, c(data$hk_lo), c(data$hk_a), data$hk_step_years, c(data$entry_hk), "hk")
    f <- c(data$house_first_hk)
    check(length(f) == data$H && all(f %in% seq_len(data$n_hk)), "data$house_first_hk must be H knots in 1..n_hk")
  }
  if (identical(as.integer(data$delta_walk), 1L)) {
    y <- c(data$delta_years)
    check(length(y) == data$n_epoch && all(is.finite(y)) && all(y >= 0), "data$delta_years must be n_epoch non-negative numbers")
    b <- data$delta_years_bar
    check(length(b) == 1 && is.finite(b) && b > 0, "data$delta_years_bar must be one positive number")
  }
  if (identical(as.integer(data$fundamentals), 1L)) {
    sf <- as.integer(c(data$sf_idx))
    check(
      identical(as.integer(data$n_sf), length(sf)) && length(sf) >= 2 && all(sf %in% seq_len(data$P)) && !anyDuplicated(sf),
      "data$fundamentals = 1 needs at least two distinct parties of S_f (n_sf and sf_idx)"
    )
    check(all(data$exists[data$G, sf] == 1L), "data$fundamentals = 1 needs every party of S_f (sf_idx) to exist at E")
    check(length(data$mu_f) == length(sf) && all(is.finite(c(data$mu_f))), "data$mu_f must be n_sf finite numbers")
    w <- data$desired_weight
    check(length(w) == 1 && is.finite(w) && w > 0 && w < 1, "data$desired_weight must be one number in (0, 1)")
    check(
      length(data$weight_time) == 1 && length(data$campaign_days) == 1 && is.finite(data$weight_time) &&
        data$weight_time >= data$campaign_days && data$campaign_days >= 1,
      "data$weight_time must be at least data$campaign_days, itself at least 1"
    )
  }
  if (identical(as.integer(data$post_flex), 1L)) {
    q <- c(data$post)
    check(length(q) == data$G - 1L && all(q %in% 0:1), "data$post must be G - 1 flags, each 0 or 1")
    check(!any(q == 1L & c(data$campaign) == 1L), "no step may be in both a campaign and a post window (data$post, data$campaign)")
    if (identical(as.integer(data$fundamentals), 1L)) {
      d <- data$weight_post_days
      check(
        length(d) == 1 && is.finite(d) && d >= 0 && d <= data$weight_time - data$campaign_days,
        "data$weight_post_days must be one number in [0, weight_time - campaign_days]"
      )
    }
  }
  check(
    is.character(mutant) && length(mutant) == 1 && mutant %in% c("none", v2_mutants(data)),
    paste0("mutant ", mutant, " is not one of none, ", paste(v2_mutants(data), collapse = ", "))
  )
}

#' `pars` as plain numeric vectors and matrices, after checking its names, lengths, bounds
#' and constraints: each `V2_CORE_PARS` name present with `v2_par_dims()`' length (in
#' cmdstanr's column-major order), nothing else of non-zero length, the sum-to-zero vectors
#' summing to 0, the lower-bounded scalars positive, `L_Omega` lower triangular with a
#' positive diagonal and unit-norm rows. `house_step_raw` becomes an H × P × (n_epoch − 1)
#' array and `delta_raw` a (1 or n_epoch) × K × P one, which constrain_variables() gives flat
#' in column-major order.
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
  for (n in c("z_walk", "L_Omega", "house_raw", "nu", "mu_step_raw", "z_shock")) p[[n]] <- as_matrix(n)
  p$delta_raw <- array(as.numeric(pars$delta_raw), dims$delta_raw)
  p$house_step_raw <- array(as.numeric(pars$house_step_raw), dims$house_step_raw)
  p$mu_drift_raw <- as_matrix("mu_drift_raw")
  p$house_drift_raw <- array(as.numeric(pars$house_drift_raw), dims$house_drift_raw)
  zero_sum <- function(x) abs(sum(x)) <= 1e-8 * max(1, abs(x))
  check(zero_sum(p$theta1), "theta1 does not sum to 0")
  check(zero_sum(p$mu1), "mu1 does not sum to 0")
  check(all(apply(p$nu, 1, zero_sum)), "a row of nu does not sum to 0")
  positive <- c(
    "sigma", "tau_camp", "sigma_gamma", "sigma_phi", "tau_sd", "tau_mu", "tau_mu_A", "tau_house", "s_new", "s_bar_E",
    "s_zeta_raw", "s_bar_zeta", "tau_post", "tau_drift", "tau_drift_A", "tau_house_drift"
  )
  for (n in positive) {
    check(all(p[[n]] > 0), paste(n, "must be positive"))
  }
  L <- p$L_Omega
  check(
    all(L[upper.tri(L)] == 0) && all(diag(L) > 0) && all(abs(rowSums(L^2) - 1) <= 1e-8),
    "L_Omega is not the Cholesky factor of a correlation matrix"
  )
  p
}

#' δ of every epoch (n_epoch × K × P, §6.7) from `zeta`, the point's `delta_raw` ((1 or
#' n_epoch) × K × P), the scales `s_delta0` and `s_zeta` (P each; `s_zeta` unread with
#' `delta_walk` = 0) and `data`'s `delta_walk`, `w`, `entry_epoch`, `delta_years`,
#' `delta_years_bar`, `idx_res` and `n_rep_res`. With `delta_walk` = 0, static,
#' δ_e = M_e(s_δ0 ⊙ ζ), one ζ for every epoch. With 1, the walk: δ_1 = M_1(s_δ0 ⊙ ζ_1) and
#' δ_e = M_e(δ_{e−1} − c_{e−1} 1ᵀ + √(Δyears_e / Δ̄) s_ζ ⊙ ζ_e), where c_{e−1,k} is the
#' unweighted mean of δ_{e−1}[k, S] over the parties S with results at election e − 1, except
#' that a newcomer's column in its entry epoch e_p > 1 is s_δ0,p ζ_{e_p,·,p}. Before e_p the
#' newcomer's column walks on the prior alone.
#'
#' The re-centring is §6.7's walk wherever no party enters, since M_e removes any constant
#' within a kjördæmi. At an entry it keeps out what the likelihood cannot see before the entry:
#' an offset within a kjördæmi shared by the parties S, which cancels in every softmax over S
#' but would set the newcomer's entry level against theirs. The newcomer's own earlier column
#' is one source of such an offset, through M_{e−1}'s centring over all P parties. So the
#' newcomer's entry column is a fresh s_δ0 ζ draw against the existing parties' mean, as its
#' industry bias is against theirs (§6.4).
#'
#' `close_last`, `years_unscaled`, `delta_carry` and `entry_leak` switch to the
#' `close_last_kjordaemi`, `delta_years_unscaled`, `no_delta_restart` and `delta_entry_leak`
#' mutants.
delta_epochs <- function(zeta, s_delta0, s_zeta, data, close_last = FALSE, years_unscaled = FALSE, delta_carry = FALSE,
                         entry_leak = FALSE) {
  K <- data$K
  P <- data$P
  n_epoch <- data$n_epoch
  entry <- c(data$entry_epoch)
  z <- function(e) matrix(zeta[e, , ], K, P)
  map <- function(X, e) if (close_last) close_on_last(X, data$w[e, ]) else M_e(X, data$w[e, ])
  X <- z(1) * matrix(s_delta0, K, P, byrow = TRUE)
  delta <- array(0, c(n_epoch, K, P))
  if (!identical(as.integer(data$delta_walk), 1L)) {
    for (e in seq_len(n_epoch)) delta[e, , ] <- map(X, e)
    return(delta)
  }
  delta[1, , ] <- map(X, 1L)
  for (e in seq_len(n_epoch)[-1]) {
    scale <- if (years_unscaled) 1 else sqrt(data$delta_years[e] / data$delta_years_bar)
    prev <- matrix(delta[e - 1, , ], K, P)
    if (!entry_leak) {
      S <- data$idx_res[e - 1, seq_len(data$n_rep_res[e - 1])]
      prev <- prev - rowMeans(prev[, S, drop = FALSE])
    }
    X <- prev + scale * z(e) * matrix(s_zeta, K, P, byrow = TRUE)
    entering <- which(entry == e)
    if (!delta_carry) {
      X[, entering] <- z(e)[, entering, drop = FALSE] * matrix(s_delta0[entering], K, length(entering), byrow = TRUE)
    }
    delta[e, , ] <- map(X, e)
  }
  delta
}

#' The natural quantities of a checked point `p` (`core_pars()`) on `data`: the walk θ
#' (P × G) and its innovation factor, μ_e, γ_{e,h}, δ_e, the bounded log concentrations,
#' `phi_init`, s_δ0, s_ζ, π_nat at each national poll's date and the closed elections' shocks
#' ε. `conditional`, `close_last`, `house_from_start`, `entry_mean_mu1`, `mu_carry`,
#' `house_carry`, `house_absolute`, `entry_mean_all`, `unscaled`, `years_unscaled`,
#' `delta_carry`, `entry_leak`, `post_unscaled`, `drift_unscaled`, `drift_left`,
#' `drift_house_start` and `drift_mu_carry` switch to the `conditional_walk`,
#' `close_last_kjordaemi`, `house_walk_from_first_poll`, `newcomer_mean_of_mu1`,
#' `no_mu_restart`, `no_house_restart`, `house_restart_absolute`, `newcomer_mean_over_all`,
#' `shock_unscaled`, `delta_years_unscaled`, `no_delta_restart`, `delta_entry_leak` and
#' `post_unscaled`, `drift_unscaled`, `drift_left_knot`, `drift_house_from_start` and
#' `drift_no_mu_restart` mutants. Besides those quantities it gives `gamma_nat` (N_nat × P),
#' the bias of each national poll's house at its date, which the likelihood reads, and with
#' `bias_drift` = 1 `mu_knot` (n_bk × P, the industry bias at each knot) and `dev_knot`
#' (H × n_hk × P, each house's centred deviations at each house knot), NULL with 0.
natural <- function(p, data, conditional = FALSE, close_last = FALSE, house_from_start = FALSE, entry_mean_mu1 = FALSE,
                    mu_carry = FALSE, house_carry = FALSE, house_absolute = FALSE, entry_mean_all = FALSE,
                    unscaled = FALSE, years_unscaled = FALSE, delta_carry = FALSE, entry_leak = FALSE,
                    post_unscaled = FALSE, drift_unscaled = FALSE, drift_left = FALSE, drift_house_start = FALSE,
                    drift_mu_carry = FALSE) {
  P <- data$P
  G <- data$G
  H <- data$H
  K <- data$K
  n_epoch <- data$n_epoch
  entry <- c(data$entry_epoch)

  # §6.2: coordinates 2:P step by sqrt(Δ_i) s_i L_walk z_i, L_walk = chol((CΣC)[2:P, 2:P]),
  # and party 1 by minus their sum, so every θ_t sums to 0 and each step's covariance is
  # Δ_i s_i² CΣC; s_i = 1 + τ_camp c_i, plus τ_post q_i with post_flex = 1.
  # The sums are compensated, so θ does not depend on the party order beyond a rounding.
  L_walk <- if (conditional) {
    Sigma <- tcrossprod(p$sigma * p$L_Omega)
    V <- Sigma - Sigma %*% matrix(1, P, P) %*% Sigma / sum(Sigma)
    t(chol(V[-1, -1, drop = FALSE]))
  } else {
    walk_factor(p$sigma, p$L_Omega)
  }
  step <- walk_steps(L_walk, p$z_walk, walk_scale(p$tau_camp, data, if (post_unscaled) 0 else p$tau_post))
  step <- rbind(-colsums_exact(step), step)
  theta <- cumsum_rows(p$theta1, step)

  # §6.4, the industry bias: μ_1, then with epoch_bias = 1 μ_e = μ_{e−1} + C(τ ⊙ ξ_e), τ_p = τ_μ
  # but τ_μ,A for Annað; with epoch_bias = 0 no step, so μ is constant over epochs. From its
  # entry epoch e_p on, a newcomer's bias is the mean of μ_{e_p} over the parties existing
  # before e_p (present at the window's start or entering earlier), plus m_new + s_new η; then
  # μ_{e_p} is re-centred, and after e_p the newcomer walks with the rest. With newcomer_pool = 0,
  # m_new = 0 and s_new = 0.2023; with 1, both are the point's.
  eb <- identical(as.integer(data$epoch_bias), 1L)
  pool <- identical(as.integer(data$newcomer_pool), 1L)
  m_new <- if (pool) p$m_new else M_NEW
  s_new <- if (pool) p$s_new else S_NEW
  tau <- if (eb) replace(rep(p$tau_mu, P), data$annad_idx, p$tau_mu_A) else numeric(P)
  new <- which(entry > 0)
  mu <- matrix(0, n_epoch, P)
  m <- p$mu1
  drift <- numeric(P)
  for (e in seq_len(n_epoch)) {
    if (eb && e > 1) {
      s <- centre(tau * p$mu_step_raw[, e - 1])
      m <- m + s
      drift <- drift + s
    }
    entering <- which(entry == e)
    if (length(entering)) {
      base <- if (entry_mean_mu1 && e > 1) mu[1, ] else m
      existing <- if (entry_mean_all) seq_len(P) else which(entry < e)
      m[entering] <- mean(base[existing]) + m_new + s_new * p$new_raw[match(entering, new)] +
        if (mu_carry) drift[entering] else 0
    }
    m <- centre(m)
    mu[e, ] <- m
  }
  # §6.4, the houses: γ_{e,h} = μ_e + d_{e,h}, with d_{e,h} = C x_{e,h} for the house's levels
  # x. They start at σ_γ ε_h and stay there up to the house's first epoch f_h; with
  # epoch_bias = 1 they walk after it, x_{e,h} = x_{e−1,h} + τ_house ζ_{e,h}, so that
  # d_{e,h} = d_{e−1,h} + C(τ_house ζ_{e,h}). At the entry epoch e_p > f_h of a newcomer p,
  # after that epoch's step, its level restarts at σ_γ ε_{h,p} plus the mean drift
  # x_{e_p,h,q} − σ_γ ε_{h,q} of the parties q existing before e_p. With epoch_bias = 0,
  # d_{e,h} = C(σ_γ ε_h) in every epoch, and a newcomer's slot already holds its σ_γ ε draw. A
  # common shift of a house's levels cancels in every softmax.
  gamma <- array(0, c(n_epoch, H, P))
  for (h in seq_len(H)) {
    start <- if (house_from_start) 1L else data$house_first_epoch[h]
    x <- p$sigma_gamma * p$house_raw[h, ]
    for (e in seq_len(n_epoch)) {
      if (eb && e > start) {
        x <- x + p$tau_house * p$house_step_raw[h, , e - 1]
        entering <- which(entry == e)
        # WHY: a shift common to the existing parties' levels is flat in every likelihood row
        # before e_p, so it must stay out of the newcomer's contrasts after e_p. Entering at its
        # core draw plus their mean drift (each q's drift includes what q inherited at its own
        # entry), the newcomer moves with such a shift, as its μ enters at their mean; its
        # contrast with q is σ_γ (ε_p − ε_q) less q's drift from that mean. Its own pre-entry
        # steps are overwritten, and τ_house = 0 makes the drift exactly 0, the core. The
        # absolute restart at σ_γ ε (house_restart_absolute) moved every contrast by the shift.
        if (length(entering) && !house_carry) {
          existing <- which(entry < e)
          drift <- if (house_absolute) 0 else mean(x[existing] - p$sigma_gamma * p$house_raw[h, existing])
          x[entering] <- p$sigma_gamma * p$house_raw[h, entering] + drift
        }
      }
      gamma[e, h, ] <- centre(x) + mu[e, ]
    }
  }

  # §6.4, the calendar drift (bias_drift = 1), in place of the epoch walk (epoch_bias = 0, so
  # the loops above left μ and γ constant over epochs): the industry bias walks on its knots,
  # μ at knot 1 = μ_1 and μ_k = μ_{k−1} + √(bk_step_years) C(τ ⊙ ξ_k), τ_p = τ_drift but
  # τ_drift,A for Annað; a newcomer enters at its entry knot k_p at the mean of μ_{k_p} over the
  # parties existing before it (entry_bk < k_p) plus m_new + s_new η, and each knot is
  # re-centred. Each house's levels start at σ_γ ε_h and drift after its first knot by
  # √(hk_step_years) τ_house,d ζ_{k,h}, a newcomer restarting at its entry house knot at σ_γ ε
  # plus the mean drift of the parties existing before it. A poll's bias interpolates each part
  # linearly between the knots around its date; μ_e and γ_{e,h} are the bias at epoch e's last
  # date (closed election e, or E).
  drift_on <- identical(as.integer(data$bias_drift), 1L)
  bias_at <- NULL
  if (drift_on) {
    rb <- if (drift_unscaled) 1 else sqrt(data$bk_step_years)
    rh <- if (drift_unscaled) 1 else sqrt(data$hk_step_years)
    tau_d <- replace(rep(p$tau_drift, P), data$annad_idx, p$tau_drift_A)
    entry_b <- c(data$entry_bk)
    entry_h <- c(data$entry_hk)
    mu_knot <- matrix(0, data$n_bk, P)
    m <- p$mu1
    carried <- numeric(P)
    for (k in seq_len(data$n_bk)) {
      if (k > 1) {
        s <- centre(rb * tau_d * p$mu_drift_raw[, k - 1])
        m <- m + s
        carried <- carried + s
      }
      entering <- which(entry_b == k)
      if (length(entering)) {
        existing <- if (entry_mean_all) seq_len(P) else which(entry_b < k)
        m[entering] <- mean(m[existing]) + m_new + s_new * p$new_raw[match(entering, new)] +
          if (drift_mu_carry) carried[entering] else 0
      }
      m <- centre(m)
      mu_knot[k, ] <- m
    }
    dev_knot <- array(0, c(H, data$n_hk, P))
    for (h in seq_len(H)) {
      start <- if (drift_house_start) 1L else data$house_first_hk[h]
      x <- p$sigma_gamma * p$house_raw[h, ]
      for (k in seq_len(data$n_hk)) {
        if (k > start) {
          x <- x + rh * p$tau_house_drift * p$house_drift_raw[h, , k - 1]
          entering <- which(entry_h == k)
          if (length(entering)) {
            existing <- which(entry_h < k)
            x[entering] <- p$sigma_gamma * p$house_raw[h, entering] +
              mean(x[existing] - p$sigma_gamma * p$house_raw[h, existing])
          }
        }
        dev_knot[h, k, ] <- centre(x)
      }
    }
    industry_at <- function(t) {
      a <- if (drift_left) 0 else data$bk_a[t]
      (1 - a) * mu_knot[data$bk_lo[t], ] + a * mu_knot[data$bk_lo[t] + 1L, ]
    }
    bias_at <- function(t, h) {
      a <- if (drift_left) 0 else data$hk_a[t]
      industry_at(t) + (1 - a) * dev_knot[h, data$hk_lo[t], ] + a * dev_knot[h, data$hk_lo[t] + 1L, ]
    }
    ends <- c(c(data$grid_res), G)
    for (e in seq_len(n_epoch)) {
      mu[e, ] <- industry_at(ends[e])
      for (h in seq_len(H)) gamma[e, h, ] <- bias_at(ends[e], h)
    }
  }
  gamma_nat <- matrix(0, data$N_nat, P)
  for (n in seq_len(data$N_nat)) {
    t <- data$grid_nat[n]
    h <- data$house_nat[n]
    gamma_nat[n, ] <- if (drift_on) bias_at(t, h) else gamma[data$grid_epoch[t], h, ]
  }

  # §6.7: δ, static with delta_walk = 0 and the walk across elections with 1, s_ζ = s̄_ζ
  # s_zeta_raw (delta_epochs()).
  s_delta0 <- exp(p$mu_sd + p$tau_sd * p$s_delta0_raw)
  s_zeta <- p$s_bar_zeta * p$s_zeta_raw
  delta <- delta_epochs(
    p$delta_raw, s_delta0, s_zeta, data,
    close_last = close_last, years_unscaled = years_unscaled, delta_carry = delta_carry, entry_leak = entry_leak
  )

  # §6.6, the election-day shock: with shock = 1, ε_e = C(s̄_E d_e ⊙ z_e) at closed election e,
  # shared by its kjördæmi, d_e = √(min(0.2 / π̄, 10)) the row of d_shock of the epoch e closes
  # (row e: closed election e closes epoch e), and z_e column e of z_shock; with shock = 0,
  # ε ≡ 0.
  eps <- matrix(0, data$n_closed, P)
  if (identical(as.integer(data$shock), 1L)) {
    for (e in seq_len(data$n_closed)) {
      d <- if (unscaled) rep(1, P) else data$d_shock[e, ]
      eps[e, ] <- centre(p$s_bar_E * d * p$z_shock[, e])
    }
  }

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
    theta = theta, step = step, L_walk = L_walk, mu = mu, gamma = gamma, gamma_nat = gamma_nat,
    mu_knot = if (drift_on) mu_knot, dev_knot = if (drift_on) dev_knot, delta = delta, phi_init = phi_init,
    log_phi = log_phi, log_phi_K = log_phi_K, log_kappa = log_kappa, s_delta0 = s_delta0, s_zeta = s_zeta,
    pi_nat = pi_nat, eps = eps
  )
}

#' The natural quantities of a point: what Stan's transformed parameters of the same names
#' hold, and what a simulation's truth records.
#' @param pars a named list as `constrain_variables(..., transformed_parameters = FALSE)`
#'   returns it (`V2_CORE_PARS`)
#' @param data a `v2_stan_data()` data list of the core in the reference forms, `epoch_bias`,
#'   `newcomer_pool`, `shock`, `delta_walk` and `fundamentals` 0 or 1
#' @return a list: `theta` (P × G), `pi_nat` (N_nat × P: π_nat at each national poll's
#'   date), `mu` (n_epoch × P, each row summing to 0), `gamma` (n_epoch × H × P), `delta`
#'   (n_epoch × K × P), `eps` (n_closed × P: the closed elections' shocks ε_e, each row
#'   summing to 0; 0 with `shock` = 0), `log_phi`
#'   and `phi` (H), `log_phi_K` and `phi_K`, `log_kappa` and `kappa`, `s_delta0` (P) and
#'   `s_zeta` (P with `delta_walk` = 1, else length 0)
#' @export
v2_natural <- function(pars, data) {
  check_core(data)
  nat <- natural(core_pars(pars, data), data)
  list(
    theta = nat$theta, pi_nat = nat$pi_nat, mu = nat$mu, gamma = nat$gamma, delta = nat$delta,
    eps = nat$eps, log_phi = nat$log_phi, phi = exp(nat$log_phi),
    log_phi_K = nat$log_phi_K, phi_K = exp(nat$log_phi_K), log_kappa = nat$log_kappa, kappa = exp(nat$log_kappa),
    s_delta0 = nat$s_delta0, s_zeta = nat$s_zeta
  )
}

#' Every likelihood row of the core, each `list(field, at, S, alpha)`: the count field and
#' its row (`at`; (e, k) for a result), the parties S its counts are gathered by and its
#' Dirichlet-multinomial concentrations over S. National poll n (§6.3):
#' φ_h normalise(π_nat[S] ⊙ exp(γ[S])), γ its house's bias at its date (`natural()`'s
#' `gamma_nat`: γ_{e,h}, or with `bias_drift` = 1 the drift interpolated at the date). Kjördæmi poll row r (§6.7):
#' κ φ_h softmax over S of (ν_j + δ_{e,k}). Result (e, k) (§6.6):
#' φ_K softmax over S of (θ_{t_e} + ε_e + δ_{e,k}), ε_e the election's shock (`natural()`'s
#' `eps`, 0 with `shock` = 0). S is the row's reported parties by identity,
#' and e the epoch of the row's date. `positional` and `next_delta` switch to the
#' `positional_gather` and `next_epoch_delta` mutants.
dm_rows <- function(p, nat, data, positional = FALSE, next_delta = FALSE) {
  parties <- function(idx, n_rep, i) if (positional) seq_len(n_rep[i]) else idx[i, seq_len(n_rep[i])]
  delta_epoch <- function(e) if (next_delta && e <= data$n_closed) e + 1L else e
  phi <- exp(nat$log_phi)
  rows <- vector("list", data$N_nat + data$N_kd + data$n_closed * data$K)
  i <- 0L
  for (n in seq_len(data$N_nat)) {
    h <- data$house_nat[n]
    S <- parties(data$idx_nat, data$n_rep_nat, n)
    a <- nat$pi_nat[n, S] * exp(nat$gamma_nat[n, S])
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
      eta <- nat$theta[S, t] + nat$eps[e, S] + nat$delta[de, k, S]
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
#' x = log π_nat,E[S_f], π_nat,E = Σ_k w_open,k softmax over A_E of (θ_E + δ_open,k) with no
#' shock (θ_E the walk at grid date G, which is E; δ_open and w_open the open epoch's; A_E the
#' parties existing at E), x_c = x − mean(x), and τ_f is `fund_scales()`'. `uncentred`
#' switches to the `fund_uncentred` mutant, x in place of x_c.
#' @return `list(mean, tau_f)`, each as long as `data$sf_idx`
fund_normal <- function(p, nat, data, uncentred = FALSE) {
  G <- data$G
  e <- data$n_epoch
  A <- which(data$exists[G, ] == 1L)
  share <- numeric(data$P)
  for (k in seq_len(data$K)) share[A] <- share[A] + data$w[e, k] * softmax(nat$theta[A, G] + nat$delta[e, k, A])
  x <- log(share[as.integer(c(data$sf_idx))])
  list(mean = if (uncentred) x else x - mean(x), tau_f = fund_scales(p$sigma, p$L_Omega, p$tau_camp, data, p$tau_post)$tau_f)
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
  out <- c(
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
    nu = ld(p$nu, 0, NU_SD),
    mu_step_raw = ld(p$mu_step_raw),
    house_step_raw = ld(p$house_step_raw),
    tau_mu = sum(log(2) + stats::dnorm(p$tau_mu, 0, data$prior_tau_mu, log = TRUE)),
    tau_mu_A = sum(log(2) + stats::dnorm(p$tau_mu_A, 0, TAU_MU_A_SD, log = TRUE)),
    tau_house = sum(log(2) + stats::dnorm(p$tau_house, 0, TAU_HOUSE_SD, log = TRUE)),
    m_new = ld(p$m_new, 0, M_NEW_SD),
    s_new = sum(log(2) + stats::dnorm(p$s_new, 0, S_NEW_SD, log = TRUE)),
    z_shock = ld(p$z_shock),
    s_bar_E = sum(log(2) + stats::dnorm(p$s_bar_E, 0, data$prior_s_bar_E, log = TRUE)),
    s_zeta_raw = sum(log(2) + stats::dnorm(p$s_zeta_raw, 0, 1, log = TRUE)),
    s_bar_zeta = sum(log(2) + stats::dnorm(p$s_bar_zeta, 0, data$prior_s_bar_zeta, log = TRUE))
  )
  # WHY: with post_flex = 0 the term is left out, so every earlier step sums the same terms as before.
  if (identical(as.integer(data$post_flex), 1L)) out <- c(out, tau_post = stats::dexp(p$tau_post, TAU_POST_RATE, log = TRUE))
  # WHY: with bias_drift = 0 the terms are left out, so every earlier step sums the same terms as before.
  if (identical(as.integer(data$bias_drift), 1L)) {
    out <- c(out,
      mu_drift_raw = ld(p$mu_drift_raw), house_drift_raw = ld(p$house_drift_raw),
      tau_drift = log(2) + ld(p$tau_drift, 0, TAU_DRIFT_SD), tau_drift_A = log(2) + ld(p$tau_drift_A, 0, TAU_DRIFT_A_SD),
      tau_house_drift = log(2) + ld(p$tau_house_drift, 0, TAU_HOUSE_DRIFT_SD)
    )
  }
  out
}

#' The reference log density of the V2 core (spec §6.2–§6.7) and its built components (the
#' epoch bias, §6.4, with `epoch_bias` = 1; the newcomer pool, §6.4, with `newcomer_pool` = 1:
#' m_new and s_new estimated; the election-day shock, §6.6, with `shock` = 1: ε_e in each
#' closed election's kjördæmi softmax, z_shock ~ N(0, 1) and s̄_E ~ half-N(0,
#' `prior_s_bar_E`); the δ walk, §6.7, with `delta_walk` = 1: each epoch's ζ ~ N(0, 1),
#' `s_zeta_raw` ~ half-N(0, 1) and s̄_ζ ~ half-N(0, `prior_s_bar_zeta`); the fundamentals
#' pseudo-observation, §6.5, with `fundamentals` = 1: μ_f ~ N(x_c, τ_f) over S_f, x_c the
#' centred log π_nat,E[S_f], a likelihood term on data with no new parameter, `fund_normal()`
#' and `fund_scales()`; the post-election multiplier, §6.2, with `post_flex` = 1: each step in
#' a post window scaled by 1 + τ_camp c + τ_post, τ_post ~ exponential(1); the calendar drift,
#' §6.4, with `bias_drift` = 1: the knots' steps ~ N(0, 1), τ_drift, τ_drift,A and τ_house,d
#' half-normal, and each national poll's bias interpolated at its date): what Stan's
#' `log_prob(u, jacobian = FALSE)`
#' evaluates in the reference forms, up to a constant. It is the density of the parameters
#' on their constrained values (the §6.10
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
#' @param data a `v2_stan_data()` data list with `walk_form` and `delta_form` 0 and each
#'   component switch (`epoch_bias`, `newcomer_pool`, `shock`, `delta_walk`, `fundamentals`,
#'   `post_flex`) 0 or 1; with `fundamentals` = 1, at least two distinct parties in `sf_idx`, each existing
#'   at E, and `mu_f` as long, finite
#' @param mutant "none", or a mutant of `v2_mutants(data)` (`V2_MUTANTS`)
#' @return numeric(1). An error, "v2 reference: ...", for another form or switch, another
#'   mutant, fundamentals data it cannot read, or a point with a missing, misshapen or
#'   out-of-bounds parameter
#' @export
v2_lp_ref <- function(pars, data, mutant = "none") {
  check_core(data, mutant)
  p <- core_pars(pars, data)
  if (mutant == "previous_w") data$w[2:data$n_closed, ] <- data$w[seq_len(data$n_closed - 1), , drop = FALSE]
  nat <- natural(
    p, data,
    conditional = mutant == "conditional_walk", close_last = mutant == "close_last_kjordaemi",
    house_from_start = mutant == "house_walk_from_first_poll", entry_mean_mu1 = mutant == "newcomer_mean_of_mu1",
    mu_carry = mutant == "no_mu_restart", house_carry = mutant == "no_house_restart",
    house_absolute = mutant == "house_restart_absolute", entry_mean_all = mutant == "newcomer_mean_over_all", unscaled = mutant == "shock_unscaled",
    years_unscaled = mutant == "delta_years_unscaled", delta_carry = mutant == "no_delta_restart",
    entry_leak = mutant == "delta_entry_leak", post_unscaled = mutant == "post_unscaled",
    drift_unscaled = mutant == "drift_unscaled", drift_left = mutant == "drift_left_knot",
    drift_house_start = mutant == "drift_house_from_start", drift_mu_carry = mutant == "drift_no_mu_restart"
  )
  rows <- dm_rows(p, nat, data, positional = mutant == "positional_gather", next_delta = mutant == "next_epoch_delta")
  lik <- vapply(rows, \(r) dirmult_lpmf(row_counts(data, r), r$alpha), numeric(1))
  terms <- c(prior_terms(p, nat, data), lik)
  # WHY: with fundamentals = 0 the term is left out, so every earlier step sums the same terms as before.
  if (identical(as.integer(data$fundamentals), 1L)) {
    terms <- c(terms, fundamentals = fund_lp(p, nat, data, uncentred = mutant == "fund_uncentred"))
  }
  neumaier(terms)
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
  eb <- as.integer(data$epoch_bias)
  n_step <- eb * (data$n_epoch - 1L)
  np <- as.integer(data$newcomer_pool)
  n_shock <- as.integer(data$shock) * as.integer(data$n_closed)
  dw <- as.integer(data$delta_walk)
  n_walk <- dw * (as.integer(data$n_epoch) - 1L)
  phi_init <- mean(log(rowSums(data$y_nat)))
  pars <- list(
    theta1 = rstz(P, THETA1_SD),
    anchor_w = array(numeric(0), c(0L, P - 1L)),
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
  out <- c(pars, list(
    log_phi_u = log((LOG_PHI_CAP - log_phi) / (LOG_PHI_CAP - phi_init)),
    mu_phi = mu_phi,
    sigma_phi = sigma_phi,
    log_phi_K_u = log((LOG_PHI_K_CAP - log_phi_K) / (LOG_PHI_K_CAP - LOG_PHI_K_INIT)),
    log_kappa_u = log((LOG_KAPPA_CAP - log_kappa) / (LOG_KAPPA_CAP - LOG_KAPPA_INIT)),
    delta_raw = array(stats::rnorm(K * P), c(1L, K, P)),
    s_delta0_raw = stats::rnorm(P),
    mu_sd = stats::rnorm(1, MU_SD_MEAN, MU_SD_SD),
    tau_sd = abs(stats::rnorm(1, 0, TAU_SD_SD)),
    nu = array(t(vapply(seq_len(J), \(j) rstz(P, NU_SD), numeric(P))), c(J, P)),
    # WHY: drawn last, so that with epoch_bias = 0 (nothing drawn) every other parameter's
    # draw is the same as before the epoch bias existed; the scales are 1-d arrays, which
    # write_stan_json() writes as [x], the form CmdStan's init reader needs for an array[1].
    mu_step_raw = matrix(stats::rnorm(P * n_step), P, n_step),
    house_step_raw = array(stats::rnorm(H * P * n_step), c(H, P, n_step)),
    tau_mu = array(abs(stats::rnorm(eb, 0, data$prior_tau_mu)), eb),
    tau_mu_A = array(abs(stats::rnorm(eb, 0, TAU_MU_A_SD)), eb),
    tau_house = array(abs(stats::rnorm(eb, 0, TAU_HOUSE_SD)), eb),
    # WHY: drawn after the epoch bias, for the same reason: with newcomer_pool = 0 nothing is
    # drawn, and with 1 every other parameter's draw is the one newcomer_pool = 0 gives.
    m_new = array(stats::rnorm(np, 0, M_NEW_SD), np),
    s_new = array(abs(stats::rnorm(np, 0, S_NEW_SD)), np),
    # WHY: drawn after the newcomer pool, for the same reason: with shock = 0 nothing is drawn,
    # and with 1 every other parameter's draw is the one shock = 0 gives.
    z_shock = matrix(stats::rnorm(P * n_shock), P, n_shock),
    s_bar_E = array(abs(stats::rnorm(as.integer(data$shock), 0, data$prior_s_bar_E)), as.integer(data$shock))
  ))
  # WHY: the δ walk's draws come after the shock's, for the same reason: with delta_walk = 0
  # nothing is drawn, and with 1 every other parameter's draw, δ's first ζ included, is the one
  # delta_walk = 0 gives. The walk's ζ_2..ζ_n_epoch join that first ζ in delta_raw.
  steps <- array(stats::rnorm(n_walk * K * P), c(n_walk, K, P))
  if (n_walk > 0) {
    walk <- array(0, c(1L + n_walk, K, P))
    walk[1, , ] <- out$delta_raw[1, , ]
    walk[-1, , ] <- steps
    out$delta_raw <- walk
  }
  out$s_zeta_raw <- abs(stats::rnorm(dw * P))
  out$s_bar_zeta <- array(abs(stats::rnorm(dw, 0, data$prior_s_bar_zeta)), dw)
  # WHY: size 0 in the reference forms, the only ones drawn here; it draws nothing.
  out$delta_coord <- array(numeric(0), c(0L, K - 1L, P - 1L))
  # WHY: drawn last, so that with post_flex = 0 (nothing drawn) every other parameter's draw is
  # the same as before the post-election multiplier existed.
  pf <- as.integer(data$post_flex)
  out$tau_post <- array(stats::rexp(pf, TAU_POST_RATE), pf)
  # WHY: drawn last, so that with bias_drift = 0 (nothing drawn) every other parameter's draw
  # is the same as before the calendar drift existed.
  bd <- as.integer(data$bias_drift)
  nb <- bd * (as.integer(data$n_bk) - 1L)
  nh <- bd * (as.integer(data$n_hk) - 1L)
  out$mu_drift_raw <- matrix(stats::rnorm(P * nb), P, nb)
  out$house_drift_raw <- array(stats::rnorm(H * P * nh), c(H, P, nh))
  out$tau_drift <- array(abs(stats::rnorm(bd, 0, TAU_DRIFT_SD)), bd)
  out$tau_drift_A <- array(abs(stats::rnorm(bd, 0, TAU_DRIFT_A_SD)), bd)
  out$tau_house_drift <- array(abs(stats::rnorm(bd, 0, TAU_HOUSE_DRIFT_SD)), bd)
  out
}

#' A draw from V2's core: parameters from the prior, and, when `observe`, every count from
#' the model given them.
#' @param data a `v2_stan_data()` data list of the core in the reference forms, `epoch_bias`,
#'   `newcomer_pool`, `shock`, `delta_walk` and `fundamentals` 0 or 1
#' @param seed the seed; the draw uses its own RNG stream (`withr::with_seed()`), leaving the
#'   caller's untouched
#' @param observe whether to replace the counts
#' @return `list(pars, truth, data)`:
#'   - `pars`, a prior draw of `V2_CORE_PARS` in cmdstanr's shapes (scalars and vectors
#'     plain, `delta_raw` (1 or n_epoch) × K × P, `nu` J × P), untruncated: σ ~ half-t(3, 0, 0.02),
#'     Ω ~ LKJ(10) by the C-vine, and each bounded concentration from its normal below its
#'     cap (the model's prior), the epoch bias's τ_μ, τ_μ,A and τ_house half-normal, the
#'     newcomer pool's m_new normal and s_new half-normal, the shock's z_shock standard
#'     normal and s̄_E half-normal, the δ walk's ζ standard normal and `s_zeta_raw` and
#'     s̄_ζ half-normal, the post-election multiplier's τ_post exponential(1), and the
#'     calendar drift's steps standard normal and τ_drift, τ_drift,A and τ_house,d half-normal;
#'   - `truth`, `v2_natural(pars, data)`;
#'   - `data`, the input with `y_nat`, `y_kd` and `y_res` replaced (when `observe`): each
#'     row keeps its total and its reported parties (`idx_*`, `n_rep_*`), its other cells
#'     stay 0, and its reported counts are one Dirichlet-multinomial draw with the row's
#'     concentrations, as the likelihood has them. A reported party can draw 0 and stays
#'     reported. With `fundamentals` = 1, `mu_f` is replaced too, after every count, by one
#'     draw of N(x_c, τ_f) (`fund_normal()`). Every other field is unchanged
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
      # WHY: drawn after every count, so that with fundamentals = 0 nothing more is drawn and with 1 every count is the draw fundamentals = 0 gives.
      if (identical(as.integer(data$fundamentals), 1L)) {
        f <- fund_normal(p, nat, data)
        sim$mu_f <- same_shape(stats::rnorm(length(f$mean), f$mean, f$tau_f), data$mu_f)
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
  out$entry_bk <- same_shape(c(data$entry_bk)[pp], data$entry_bk)
  out$entry_hk <- same_shape(c(data$entry_hk)[pp], data$entry_hk)
  out$annad_idx <- if (data$annad_idx > 0L) match(data$annad_idx, pp) else 0L
  out$d_shock <- data$d_shock[, pp, drop = FALSE]
  out$N_open <- same_shape(c(data$N_open)[kp], data$N_open)
  out$sf_idx <- same_shape(match(c(data$sf_idx), pp), data$sf_idx)
  out
}

#' A point and its data in another party and kjördæmi order, mapped through the natural
#' quantities, for the reference forms. The permuted data's party i is the input's party
#' `party_perm[i]`, and its kjördæmi k the input's `kd_perm[k]`. θ_1, μ_1, the houses' ε,
#' ν, every epoch's ζ, r, the newcomers' η, the epoch bias's steps ξ and ζ, the calendar
#' drift's steps, the shock's z and the δ walk's `s_zeta_raw` are permuted, and Annað's index re-derived (the other scales,
#' m_new and s_new are not party-indexed); `L_Omega` is re-derived as chol(Ω[pp, pp]),
#' and `z_walk` from the permuted θ increments in the new coordinates 2:P. The data keep the
#' builder's contract: counts, `exists`, `entry_epoch`, `entry_bk`, `entry_hk`, `d_shock`, `w` columns, `y_res`'s
#' kjördæmi and party dimensions and `N_open` permuted; reported-party lists re-derived by
#' identity; kjördæmi poll rows re-sorted by poll and new kjördæmi; `sf_idx` re-indexed.
#' @param pars a named list of `V2_CORE_PARS`
#' @param data a `v2_stan_data()` data list of the core in the reference forms, `epoch_bias`,
#'   `newcomer_pool`, `shock`, `delta_walk` and `fundamentals` 0 or 1
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
  scale <- walk_scale(p$tau_camp, data, p$tau_post)
  target <- nat$step[pp, , drop = FALSE][-1, , drop = FALSE]
  z_walk <- forwardsolve(L_walk, target / rep(scale, each = P - 1))
  for (i in 1:3) z_walk <- z_walk - forwardsolve(L_walk, (walk_steps(L_walk, z_walk, scale) - target) / rep(scale, each = P - 1))

  entry <- c(data$entry_epoch)
  new_old <- which(entry > 0)
  new_new <- which(entry[pp] > 0)
  out_pars <- list(
    theta1 = p$theta1[pp], anchor_w = array(numeric(0), c(0L, P - 1L)), z_walk = z_walk, sigma = sigma,
    L_Omega = L, tau_camp = p$tau_camp,
    mu1 = p$mu1[pp], house_raw = p$house_raw[, pp, drop = FALSE], sigma_gamma = p$sigma_gamma,
    new_raw = p$new_raw[match(pp[new_new], new_old)], log_phi_u = p$log_phi_u, mu_phi = p$mu_phi,
    sigma_phi = p$sigma_phi, log_phi_K_u = p$log_phi_K_u, log_kappa_u = p$log_kappa_u,
    delta_raw = p$delta_raw[, kp, pp, drop = FALSE], s_delta0_raw = p$s_delta0_raw[pp],
    mu_sd = p$mu_sd, tau_sd = p$tau_sd, nu = p$nu[, pp, drop = FALSE],
    mu_step_raw = p$mu_step_raw[pp, , drop = FALSE], house_step_raw = p$house_step_raw[, pp, , drop = FALSE],
    tau_mu = pars$tau_mu, tau_mu_A = pars$tau_mu_A, tau_house = pars$tau_house, m_new = pars$m_new, s_new = pars$s_new,
    z_shock = p$z_shock[pp, , drop = FALSE], s_bar_E = pars$s_bar_E,
    # WHY: numeric(0)[pp] would be P NAs.
    s_zeta_raw = if (length(p$s_zeta_raw)) p$s_zeta_raw[pp] else p$s_zeta_raw, s_bar_zeta = pars$s_bar_zeta,
    delta_coord = array(numeric(0), c(0L, K - 1L, P - 1L)), tau_post = pars$tau_post,
    mu_drift_raw = p$mu_drift_raw[pp, , drop = FALSE], house_drift_raw = p$house_drift_raw[, pp, , drop = FALSE],
    tau_drift = pars$tau_drift, tau_drift_A = pars$tau_drift_A, tau_house_drift = pars$tau_house_drift
  )
  ljac <- function(L) sum((P - 2:P) * log(diag(L)[2:P]))
  structure(list(pars = out_pars, data = out_data), ljac_L_Omega = ljac(L) - ljac(p$L_Omega))
}
