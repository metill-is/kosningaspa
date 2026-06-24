// Reference-invariant reparameterisation of the between-election polling smoother.
//
// The original polling_watch.stan used a drop-one (eta[1] = -sum(eta[2:P]))
// parameterisation. softmax of that is reference-invariant in the LIKELIHOOD but
// not in the PRIORS: the residual party got an asymmetric initial prior
// (SD ~ 2*sqrt(P-1) vs 2), an asymmetric RW innovation covariance, and an
// asymmetric house-effect zero-sum. That made pi_smooth depend on which party
// was the reference (a ~2pp swing on the deployed fit).
//
// Here the latent state, the house effects, and the industry mean all live on
// the sum-to-zero subspace symmetrically (no party is the residual), so
// pi_smooth = softmax(beta) is invariant to party ordering. Per-party RW
// volatility (sigma) and the full P x P cross-party correlation (Omega) are
// retained; the correlated per-party-scaled innovations are projected onto the
// sum-to-zero subspace by centring. Semantics (DM likelihood for polls, exact
// multinomial for the election anchor, hierarchical house effects, sqrt-time
// random walk) are otherwise identical to polling_watch.stan.

data {
  int<lower = 1> D;                                // Number of time points
  int<lower = 2> P;                                // Number of parties
  int<lower = 1> H;                                // Number of houses (house 1 = election anchor)
  int<lower = 1> N;                                // Number of polls

  array[N, P] int<lower = 0> y_n;                  // Polling counts per party
  array[N] int<lower = 1, upper = H> house_n;      // House indicator per poll
  array[N] int<lower = 1, upper = D> date_n;       // Date indicator per poll
  array[N] int<lower = 1, upper = P> n_parties_n;  // Number of parties reported per poll

  vector[D - 1] time_diff;                         // Gaps between consecutive dates
  int<lower = 1> n_pred;                           // Sample size for posterior predictions
}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);
}

parameters {
  // Latent support: random walk on the sum-to-zero (simplex-logit) subspace.
  sum_to_zero_vector[P] beta0;                      // initial level, zero-sum over parties

  cholesky_factor_corr[P] L_Omega;                 // cross-party innovation correlation (all P)
  matrix[P, D - 1] z_step_raw;                     // raw innovations in full P-space

  // Per-party RW volatility (hierarchical, log scale; zero-sum deviations => symmetric).
  real mu_log_sigma;
  real<lower = 0> tau_log_sigma;
  sum_to_zero_vector[P] log_sigma_raw;

  // Per-house overdispersion (hierarchical).
  vector[H - 1] log_phi;
  real mu_phi;
  real<lower = 0> sigma_phi;

  // House effects, zero-sum over parties per house. House 1 (election) pinned to 0.
  array[H - 1] sum_to_zero_vector[P] gamma_free;
  sum_to_zero_vector[P] mu_gamma;                  // shared industry bias (zero-sum)
  real<lower = 0> sigma_gamma;                     // single scale (label-symmetric)
}

transformed parameters {
  vector<lower = 0>[P] sigma = exp(mu_log_sigma + tau_log_sigma * log_sigma_raw);
  vector<lower = 0>[H - 1] phi = exp(mu_phi + sigma_phi * log_phi);

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
  beta0 ~ normal(0, 2);

  L_Omega ~ lkj_corr_cholesky(10);
  to_vector(z_step_raw) ~ std_normal();

  mu_log_sigma ~ normal(-2, 2);
  tau_log_sigma ~ exponential(1);
  log_sigma_raw ~ std_normal();

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
