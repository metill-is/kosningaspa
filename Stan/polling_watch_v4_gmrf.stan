// polling_watch_v4 — state-level (GMRF) reparameterisation of the
// reference-invariant between-election polling smoother.
//
// GOAL: remove the bad geometry of polling_watch_v2 (max_treedepth=10 on ~98%
// of transitions) while KEEPING (a) per-party random-walk volatility and (b) a
// full P x P cross-party correlation of the innovations. Stays reference-
// invariant (latent state on the sum-to-zero subspace; pi_smooth = softmax of a
// zero-sum latent vector), and keeps the Dirichlet-multinomial likelihood, the
// exact multinomial election anchor (house 1), and the hierarchical house
// effects.
//
// ---------------------------------------------------------------------------
// WHY v2's GEOMETRY IS BAD
// ---------------------------------------------------------------------------
// v2 builds the latent path from innovations:
//     beta[t] = beta[t-1] + time_scale[t-1] * (diag(sigma) * L_Omega * z[,t-1])
// with the per-party scale hierarchy
//     sigma = exp(mu_log_sigma + tau_log_sigma * log_sigma_raw).
// The diagnosed DOMINANT funnel is the multiplicative scale hierarchy: the
// per-party sigmas span only ~1.4x, so tau_log_sigma is weakly identified and
// heavy-tailed (a Neal funnel between tau_log_sigma and the (P-1)-dim
// log_sigma_raw). Worse, `sigma` sits INSIDE the state recursion, so the
// tau funnel does not stay local — through `sigma` it rescales every one of the
// (D-1)*P innovation coordinates. It is a tau x {sigma direction} x {whole path}
// triple funnel, which is why NUTS lengthens trajectories to max_treedepth.
// The full correlation then couples the per-party scales into the joint
// innovation covariance diag(sigma) Omega diag(sigma): a step size tuned for one
// party's scale is wrong for another, adding ~25pp treedepth on top.
//
// ---------------------------------------------------------------------------
// WHAT THIS REPARAMETERISATION DOES (3 moves)
// ---------------------------------------------------------------------------
// 1. GMRF / standardized-increment view. A first-order RW is a GMRF whose
//    increments Delta_t = beta[t]-beta[t-1] are INDEPENDENT given their
//    covariance. We keep the increments as the sampling basis (already the
//    well-conditioned direction in t) but pull EVERY global scale OUT of the
//    recursion so the path is built from UNIT-scale standardized increments and
//    multiplied by scales that live outside the loop. The state-level geometry
//    is then a fixed, well-conditioned tridiagonal-precision GMRF; the only
//    free scales are a single shared sigma_bar and a small zero-sum per-party
//    shape vector.
//
// 2. Break the dominant funnel by splitting location from shape and
//    orthogonalising. The overall log-scale (v2's mu_log_sigma) becomes a single
//    shared scalar sigma_bar that multiplies the ALREADY-unit-scaled latent path
//    once, at state level — its funnel neck is one scalar times a standardized
//    path, not a scale buried in (D-1)*P increments. The per-party deviations
//    s_party = exp(tau_sigma * w) live on a zero-sum unit basis w ~ std_normal()
//    with a TIGHT half-normal tau_sigma (matched to the observed ~1.4x =>
//    log-spread ~0.34, so tau_sigma ~ 0.1-0.3): this shrinks the funnel neck so
//    the heavy tau tail is gone. sigma_bar and the per-party shape no longer
//    multiply each other inside the loop, so the tau x mu x path interaction
//    that drove the treedepth is removed.
//
// 3. Decouple scale from correlation. Correlation is applied to standardized
//    innovations (L_Omega * z), the per-party shape multiplies row-wise, and the
//    single shared sigma_bar and sqrt-time gaps multiply at the end. The metric
//    can adapt one shared scale + a near-identity set of per-party shapes,
//    instead of a product diag(sigma) L_Omega whose per-coordinate scale is a
//    tangle of sigma_p and L_Omega row-norms.
//
// KEEPS: per-party volatility (full, via s_party) and the FULL P x P innovation
// correlation (L_Omega, exact — not approximated). Reference-invariant: the
// per-party shape lives on the sum-to-zero subspace, innovations are centred
// onto it, and pi_smooth = softmax(zero-sum beta).
//
// REMOVES: the multiplicative per-party-sigma funnel (tau_log_sigma vs
// log_sigma_raw) AND its propagation through the whole state path; i.e. the
// dominant funnel responsible for the ~98% treedepth saturation.

data {
  int<lower = 1> D;                                // Number of time points
  int<lower = 2> P;                                // Number of parties
  int<lower = 1> H;                                // Number of houses (house 1 = election anchor)
  int<lower = 1> N;                                // Number of polls

  array[N, P] int<lower = 0> y_n;                  // Polling counts per party
  array[N] int<lower = 1, upper = H> house_n;      // House indicator per poll
  array[N] int<lower = 1, upper = D> date_n;       // Date indicator per poll
  array[N] int<lower = 1, upper = P> n_parties_n;  // Number of parties reported per poll
  array[N, P] int<lower = 0, upper = P> party_index_n; // Column ids of reported parties per poll (0-padded tail)

  vector[D - 1] time_diff;                         // Gaps between consecutive dates
  int<lower = 1> n_pred;                           // Sample size for posterior predictions
}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);      // sqrt-time RW scaling
}

parameters {
  // --- Latent state on the sum-to-zero subspace ---
  sum_to_zero_vector[P] beta0;                     // initial level, zero-sum over parties

  // Standardized, correlated innovations in full P-space (UNIT scale).
  cholesky_factor_corr[P] L_Omega;                 // FULL cross-party innovation correlation
  matrix[P, D - 1] z_step_raw;                     // raw standard-normal innovations

  // --- Scale hierarchy, refactored to kill the funnel ---
  // Single shared log-scale (replaces v2's mu_log_sigma): pulled OUT of the
  // recursion, multiplies the standardized path once at state level.
  real log_sigma_bar;
  // Per-party shape: zero-sum deviations on a unit basis with a TIGHT scale.
  real<lower = 0> tau_sigma;                       // cross-party log-scale spread (tight)
  sum_to_zero_vector[P] w_sigma;                   // unit zero-sum shape direction

  // --- Per-house overdispersion (hierarchical, non-centred) ---
  vector[H - 1] log_phi;
  real mu_phi;
  real<lower = 0> sigma_phi;

  // --- House effects (zero-sum per party; house 1 pinned to 0) ---
  array[H - 1] sum_to_zero_vector[P] gamma_free;
  sum_to_zero_vector[P] mu_gamma;                  // shared industry bias (zero-sum)
  real<lower = 0> sigma_gamma;                     // single scale (label-symmetric)
}

transformed parameters {
  // Shared scale (scalar) and per-party shape (zero-sum, near-1 multiplier).
  real sigma_bar = exp(log_sigma_bar);
  // Per-party relative volatility: exp of a zero-sum vector => geometric-mean 1.
  // Centred in log-space so it carries shape only; sigma_bar carries level.
  vector<lower = 0>[P] s_party = exp(tau_sigma * w_sigma);

  vector<lower = 0>[H - 1] phi = exp(mu_phi + sigma_phi * log_phi);

  array[H] vector[P] gamma;
  gamma[1] = rep_vector(0.0, P);
  for (h in 2:H) {
    gamma[h] = mu_gamma + sigma_gamma * gamma_free[h - 1];
  }

  // --- Build the path from UNIT-scale standardized increments (GMRF view) ---
  // Step 1: correlate the standardized innovations (full P x P), apply per-party
  //         SHAPE only (s_party), and project onto the zero-sum subspace.
  // Step 2: accumulate with sqrt-time gaps to get a STANDARDIZED latent path.
  // Step 3: multiply the whole path by the single shared scalar sigma_bar.
  // sigma_bar therefore multiplies an already-well-scaled path exactly once,
  // instead of being buried inside every increment.
  matrix[P, D - 1] u_step = diag_pre_multiply(s_party, L_Omega) * z_step_raw;

  array[D] vector[P] beta;
  beta[1] = beta0;                                 // beta0 carries its own scale (prior below)
  {
    // Standardized random-walk part (excludes beta0 level), scaled by sigma_bar.
    vector[P] state = rep_vector(0.0, P);
    for (t in 2:D) {
      vector[P] innov = u_step[, t - 1];
      innov -= mean(innov);                        // project increment onto zero-sum subspace
      state += time_scale[t - 1] * innov;          // unit-scale accumulation
      beta[t] = beta0 + sigma_bar * state;         // single shared scale, applied once
    }
  }
}

model {
  // Initial level.
  beta0 ~ normal(0, 2);

  // Innovations: standardized + full correlation.
  L_Omega ~ lkj_corr_cholesky(10);
  to_vector(z_step_raw) ~ std_normal();

  // Scale hierarchy (funnel-free):
  //  - log_sigma_bar around v2's exp(mu_log_sigma) ~ exp(-2); normal(-2, 1).
  //  - tau_sigma TIGHT: observed cross-party log-spread ~0.34 => half-normal(0,0.3)
  //    keeps the neck narrow so no heavy tau tail, while still allowing the
  //    genuine ~1.4x per-party spread.
  log_sigma_bar ~ normal(-2, 1);
  tau_sigma ~ normal(0, 0.3);                      // half-normal (tau_sigma >= 0)
  w_sigma ~ std_normal();

  // House overdispersion.
  log_phi ~ std_normal();
  mu_phi ~ normal(0, 1);
  sigma_phi ~ normal(0, 1);

  // House effects.
  for (h in 1:(H - 1)) {
    gamma_free[h] ~ std_normal();
  }
  mu_gamma ~ std_normal();
  sigma_gamma ~ exponential(1);

  // Likelihood: DM for polls (house > 1), exact multinomial for the election
  // anchor (house 1). eta is zero-sum (beta zero-sum, gamma zero-sum).
  for (n in 1:N) {
    int k = n_parties_n[n];
    vector[P] eta = beta[date_n[n]] + gamma[house_n[n]];
    // Gather reported parties by identity (party_index_n), not the first-k slice.
    array[k] int cols = party_index_n[n, 1:k];
    array[k] int y_obs = y_n[n, cols];
    if (house_n[n] > 1) {
      vector[k] pi_n = softmax(eta[cols]);
      y_obs ~ dirichlet_multinomial(pi_n * phi[house_n[n] - 1]);
    } else {
      y_obs ~ multinomial_logit(eta[cols]);
    }
  }
}

generated quantities {
  // Party-space (reference-invariant) correlation of the RW innovations.
  corr_matrix[P] Omega = multiply_lower_tri_self_transpose(L_Omega);

  // Per-party total RW volatility on v2's scale, for continuity of reporting:
  // v2's sigma_p == sigma_bar * s_party (shared level x per-party shape).
  vector<lower = 0>[P] sigma = sigma_bar * s_party;

  // Smoothed support: softmax of a zero-sum latent vector — invariant to ordering.
  array[D, P] real pi_smooth;
  for (d in 1:D) {
    pi_smooth[d] = to_array_1d(softmax(beta[d]));
  }

  array[P] int<lower = 0> y_rep = multinomial_logit_rng(beta[D], n_pred);
}
