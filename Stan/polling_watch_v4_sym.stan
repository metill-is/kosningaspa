// v4_sym: as v4 but with a SYMMETRIC (centred) beta0 so geometry does not depend
// on party ordering (beta0's Other-in-pivot cell was the min-ESS bottleneck).
//
// Reference-invariant polling smoother — per-party RW volatility WITHOUT the
// hyper-scale funnel, full cross-party correlation retained (variant v4).
//
// PROBLEM (v2): the per-party innovation scales were drawn from a log-normal
// HIERARCHY,
//     sigma[p] = exp(mu_log_sigma + tau_log_sigma * log_sigma_raw[p]).
// On P=10 data the empirical per-party sigmas span only ~1.4x (0.0126-0.018),
// so the cross-party spread tau_log_sigma is very weakly identified: its
// posterior has substantial mass near 0. As tau_log_sigma -> 0 the directions
// log_sigma_raw[p] become unconstrained by the prior product
// tau_log_sigma * log_sigma_raw[p], producing a Neal funnel between the
// hyper-scale and its raw deviations. This funnel is intrinsic to a weakly
// identified hyper-scale and is NOT cured by the non-centred form: the
// non-centred prior is std_normal on log_sigma_raw, but the LIKELIHOOD couples
// log_sigma_raw[p] to the data only through tau_log_sigma * log_sigma_raw[p],
// so when tau is tiny the step size that works at the funnel mouth is far too
// large in the neck. The diagnosed decomposition confirms this is the dominant
// pathology: removing the sigma hierarchy (shared scalar sigma) drops treedepth
// saturation from 97.7% to 25-32%.
//
// FIX: drop the mu/tau/raw hierarchy entirely and give each party its own
// scale directly,
//     sigma[p] ~ half-Student-t(3, 0, scale_sigma),   independent over p.
// There is now NO hyper-scale to funnel against. Each sigma[p] is a single
// parameter with a FIXED-width prior, identified directly by party p's own
// random-walk innovations. We parameterise sigma on the log scale with a
// std_normal raw and a Jacobian-correct half-Student-t target so the sampler
// sees an unconstrained, roughly-unit-scaled coordinate per party. The
// heavy (df=3) tail lets a genuinely more volatile party escape upward without
// a shared mean dragging it back, while the common fixed scale_sigma supplies
// the mild pooling that a 10-party panel needs — but as a PRIOR constant, not a
// sampled hyper-parameter, so it cannot create a funnel.
//
// KEEPS: (a) per-party RW volatility — 10 free sigma[p], no shared-scalar
// collapse; (b) the FULL P x P cross-party innovation correlation L_Omega
// (unchanged from v2). Innovations are diag_pre_multiply(sigma, L_Omega)-scaled
// and projected onto the sum-to-zero subspace by centring, exactly as v2.
// Reference-invariance is preserved: beta lives on the zero-sum subspace and
// pi_smooth = softmax(beta) is independent of party ordering.

data {
  int<lower = 1> D;                                // Number of time points
  int<lower = 2> P;                                // Number of parties
  int<lower = 1> H;                                // Number of houses (house 1 = election anchor)
  int<lower = 1> N;                                // Number of polls

  array[N, P] int<lower = 0> y_n;                  // Polling counts per party [N x P]
  array[N] int<lower = 1, upper = H> house_n;      // House indicator per poll [N]
  array[N] int<lower = 1, upper = D> date_n;       // Date indicator per poll [N]
  array[N] int<lower = 1, upper = P> n_parties_n;  // Parties reported per poll [N]

  vector[D - 1] time_diff;                         // Gaps between consecutive dates [D-1]
  int<lower = 1> n_pred;                           // Sample size for posterior predictions
}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);      // sqrt-time RW scaling
  real nu_sigma = 3.0;                             // half-Student-t df for per-party scales
  real scale_sigma = 0.02;                         // fixed prior scale (NOT sampled => no funnel)
}

parameters {
  // Latent support: random walk on the sum-to-zero (simplex-logit) subspace.
  // SYMMETRIC beta0: plain vector centred in transformed params (no ordered ILR pivot).
  vector[P] beta0_raw;

  cholesky_factor_corr[P] L_Omega;                 // full P x P cross-party innovation correlation
  matrix[P, D - 1] z_step_raw;                     // raw innovations in full P-space [P x D-1]

  // Per-party RW volatility — INDEPENDENT per party, NO shared hyper-scale.
  // log_sigma is unconstrained; the half-Student-t prior is imposed in `model`
  // with the change-of-variables Jacobian for the exp() transform.
  vector[P] log_sigma;

  // Per-house overdispersion (hierarchical, non-centred).
  vector[H - 1] log_phi;
  real mu_phi;
  real<lower = 0> sigma_phi;

  // House effects, zero-sum over parties per house. House 1 (election) pinned to 0.
  array[H - 1] sum_to_zero_vector[P] gamma_free;
  sum_to_zero_vector[P] mu_gamma;                  // shared industry bias (zero-sum)
  real<lower = 0> sigma_gamma;                     // single scale (label-symmetric)
}

transformed parameters {
  vector<lower = 0>[P] sigma = exp(log_sigma);     // per-party innovation scales
  vector<lower = 0>[H - 1] phi = exp(mu_phi + sigma_phi * log_phi);

  vector[P] beta0 = beta0_raw - mean(beta0_raw);   // centred => sum-to-zero, label-symmetric

  array[H] vector[P] gamma;
  gamma[1] = rep_vector(0.0, P);
  for (h in 2:H) {
    gamma[h] = mu_gamma + sigma_gamma * gamma_free[h - 1];
  }

  // Correlated, per-party-scaled innovations, projected onto the zero-sum
  // subspace by centring. Centring a label-symmetric draw stays label-symmetric.
  matrix[P, D - 1] z_step = diag_pre_multiply(sigma, L_Omega) * z_step_raw;

  array[D] vector[P] beta;
  beta[1] = beta0;
  for (t in 2:D) {
    vector[P] innov = z_step[, t - 1];
    innov -= mean(innov);
    beta[t] = beta[t - 1] + time_scale[t - 1] * innov;
  }
}

model {
  beta0_raw ~ normal(0, 2);

  L_Omega ~ lkj_corr_cholesky(10);
  to_vector(z_step_raw) ~ std_normal();

  // Independent half-Student-t(nu, 0, scale) on each per-party sigma, sampled on
  // the log scale. student_t_lpdf gives the half-t density on sigma >= 0 (the
  // truncation constant is the same for every draw, so it can be dropped); the
  // `+ log_sigma` term is the Jacobian for sigma = exp(log_sigma).
  target += student_t_lpdf(sigma | nu_sigma, 0, scale_sigma) + sum(log_sigma);

  log_phi ~ std_normal();
  mu_phi ~ normal(0, 1);
  sigma_phi ~ normal(0, 1);

  for (h in 1:(H - 1)) {
    gamma_free[h] ~ std_normal();
  }
  mu_gamma ~ std_normal();
  sigma_gamma ~ exponential(1);

  for (n in 1:N) {
    int k = n_parties_n[n];
    vector[P] eta = beta[date_n[n]] + gamma[house_n[n]];
    if (house_n[n] > 1) {
      vector[k] pi_n = softmax(eta[1:k]);
      y_n[n, 1:k] ~ dirichlet_multinomial(pi_n * phi[house_n[n] - 1]);
    } else {
      y_n[n, 1:k] ~ multinomial_logit(eta[1:k]);
    }
  }
}

generated quantities {
  // Party-space (reference-invariant) correlation of the RW innovations.
  corr_matrix[P] Omega = multiply_lower_tri_self_transpose(L_Omega);

  // Smoothed support: softmax of a zero-sum latent vector — invariant to party ordering.
  array[D, P] real pi_smooth;
  for (d in 1:D) {
    pi_smooth[d] = to_array_1d(softmax(beta[d]));
  }

  array[P] int<lower = 0> y_rep = multinomial_logit_rng(beta[D], n_pred);
}
