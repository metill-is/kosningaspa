// Reference-invariant polling smoother — shared innovation variance + cross-party
// correlation (variant a). Same as polling_watch_v2.stan but the per-party RW
// volatility HIERARCHY (mu_log_sigma, tau_log_sigma, log_sigma_raw) is replaced by
// a single shared scale `sigma`. That removes a hierarchical-scale funnel (the main
// remaining bad geometry in v2) at the cost of differential per-party volatility,
// which was modest (~1.4x spread). The cross-party correlation (Omega) is retained,
// so the innovations are still projected onto the sum-to-zero subspace by centring
// (a party-space correlation does not preserve sum-to-zero on its own).

data {
  int<lower = 1> D;
  int<lower = 2> P;
  int<lower = 1> H;
  int<lower = 1> N;

  array[N, P] int<lower = 0> y_n;
  array[N] int<lower = 1, upper = H> house_n;
  array[N] int<lower = 1, upper = D> date_n;
  array[N] int<lower = 1, upper = P> n_parties_n;
  array[N, P] int<lower = 0, upper = P> party_index_n; // Column ids of reported parties per poll (0-padded tail)

  vector[D - 1] time_diff;
  int<lower = 1> n_pred;
}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);
}

parameters {
  sum_to_zero_vector[P] beta0;

  cholesky_factor_corr[P] L_Omega;            // cross-party innovation correlation
  matrix[P, D - 1] z_step_raw;
  real mu_log_sigma;                          // single shared innovation log-scale

  vector[H - 1] log_phi;
  real mu_phi;
  real<lower = 0> sigma_phi;

  array[H - 1] sum_to_zero_vector[P] gamma_free;
  sum_to_zero_vector[P] mu_gamma;
  real<lower = 0> sigma_gamma;
}

transformed parameters {
  real<lower = 0> sigma = exp(mu_log_sigma);  // shared across parties
  vector<lower = 0>[H - 1] phi = exp(mu_phi + sigma_phi * log_phi);

  array[H] vector[P] gamma;
  gamma[1] = rep_vector(0.0, P);
  for (h in 2:H) {
    gamma[h] = mu_gamma + sigma_gamma * gamma_free[h - 1];
  }

  // Correlated innovations with a single shared scale, projected to sum-to-zero.
  matrix[P, D - 1] z_step = sigma * (L_Omega * z_step_raw);

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
  corr_matrix[P] Omega = multiply_lower_tri_self_transpose(L_Omega);
  array[D, P] real pi_smooth;
  for (d in 1:D) {
    pi_smooth[d] = to_array_1d(softmax(beta[d]));
  }
  array[P] int<lower = 0> y_rep = multinomial_logit_rng(beta[D], n_pred);
}
