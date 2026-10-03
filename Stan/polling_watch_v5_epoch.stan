// polling_watch_v4 with pollster bias that may change at each election (v5_epoch).
//
// v4 holds the industry bias mu_gamma and every house effect gamma[h] fixed over
// the whole window. Here the window is cut into bias EPOCHS at each anchoring
// election: epoch 1 runs up to the first election in the data, epoch 2 from the
// day after it to the next election, and so on, the last epoch being the open
// post-election period. Each epoch has its own bias, a random walk over epochs:
//
//     mu_gamma[1]  = mu_gamma0                       (v4's mu_gamma)
//     mu_gamma[e]  = mu_gamma[e-1] + tau_mu * z_mu[e-1]
//     dev[1, h]    = sigma_gamma * gamma_free[h]     (v4's house deviation)
//     dev[e, h]    = dev[e-1, h] + tau_house * z_house[e-1, h]
//     gamma[e, h]  = mu_gamma[e] + dev[e, h],        gamma[e, 1] = 0 (election)
//
// Shocks are zero-sum over parties (sum_to_zero_vector, std_normal), exactly
// like v4's mu_gamma and gamma_free, so the model stays reference-invariant.
//
// tau_mu and tau_house are either sampled (estimate_tau = 1, half-normal prior
// with scale tau_prior_scale) or fixed from data (estimate_tau = 0). With both
// fixed at 0 every z is prior-only and the model is v4, which is the
// implementation check.
//
// Identification. An epoch between two elections is pinned at both ends: the
// latent state is continuous and equals the result on each election day, so
// polls just after the opening election and just before the closing one both
// measure that epoch's bias. The last (open) epoch has only its opening anchor,
// so its bias is learnt from how the first post-election polls sat against the
// result, shrunk towards the previous epoch by tau. Poll bias and genuine
// post-election opinion movement are separated only by tau versus the RW
// volatility sigma.

data {
  int<lower = 1> D;                                // Number of time points
  int<lower = 2> P;                                // Number of parties
  int<lower = 1> H;                                // Number of houses (house 1 = election anchor)
  int<lower = 1> N;                                // Number of polls
  int<lower = 1> E;                                // Number of bias epochs

  array[N, P] int<lower = 0> y_n;                  // Polling counts per party [N x P]
  array[N] int<lower = 1, upper = H> house_n;      // House indicator per poll [N]
  array[N] int<lower = 1, upper = D> date_n;       // Date indicator per poll [N]
  array[N] int<lower = 1, upper = E> epoch_n;      // Bias epoch per poll [N] (ignored for house 1)
  array[N] int<lower = 1, upper = P> n_parties_n;  // Parties reported per poll [N]
  array[N, P] int<lower = 0, upper = P> party_index_n; // Column ids of reported parties per poll (0-padded tail)

  vector[D - 1] time_diff;                         // Gaps between consecutive dates [D-1]
  int<lower = 1> n_pred;                           // Sample size for posterior predictions

  int<lower = 0, upper = 1> estimate_tau;          // 1 = sample tau_mu, tau_house; 0 = use the fixed values
  real<lower = 0> tau_mu_fixed;                    // industry shock SD when estimate_tau = 0
  real<lower = 0> tau_house_fixed;                 // house shock SD when estimate_tau = 0
  real<lower = 0> tau_prior_scale;                 // half-normal scale for sampled taus
}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);      // sqrt-time RW scaling
  real nu_sigma = 3.0;                             // half-Student-t df for per-party scales
  real scale_sigma = 0.02;                         // fixed prior scale (NOT sampled => no funnel)
}

parameters {
  sum_to_zero_vector[P] beta0;                     // initial level, zero-sum over parties

  cholesky_factor_corr[P] L_Omega;                 // full P x P cross-party innovation correlation
  matrix[P, D - 1] z_step_raw;                     // raw innovations in full P-space [P x D-1]
  vector[P] log_sigma;                             // per-party RW volatility (log scale)

  vector[H - 1] log_phi;                           // per-house overdispersion (non-centred)
  real mu_phi;
  real<lower = 0> sigma_phi;

  // Epoch-1 bias, as in v4.
  sum_to_zero_vector[P] mu_gamma0;                 // industry bias in epoch 1
  array[H - 1] sum_to_zero_vector[P] gamma_free;   // standardised house deviations in epoch 1
  real<lower = 0> sigma_gamma;                     // cross-house spread in epoch 1

  // Between-election shocks (non-centred).
  array[E - 1] sum_to_zero_vector[P] z_mu;             // industry shocks
  array[E - 1, H - 1] sum_to_zero_vector[P] z_house;   // house shocks
  array[estimate_tau] real<lower = 0> tau_mu_est;
  array[estimate_tau] real<lower = 0> tau_house_est;
}

transformed parameters {
  vector<lower = 0>[P] sigma = exp(log_sigma);
  vector<lower = 0>[H - 1] phi = exp(mu_phi + sigma_phi * log_phi);

  real<lower = 0> tau_mu = estimate_tau ? tau_mu_est[1] : tau_mu_fixed;
  real<lower = 0> tau_house = estimate_tau ? tau_house_est[1] : tau_house_fixed;

  array[E] vector[P] mu_gamma;
  array[E, H] vector[P] gamma;
  {
    array[H - 1] vector[P] dev;
    for (h in 1:(H - 1)) {
      dev[h] = sigma_gamma * gamma_free[h];
    }
    mu_gamma[1] = mu_gamma0;
    for (e in 1:E) {
      if (e > 1) {
        mu_gamma[e] = mu_gamma[e - 1] + tau_mu * z_mu[e - 1];
        for (h in 1:(H - 1)) {
          dev[h] += tau_house * z_house[e - 1, h];
        }
      }
      gamma[e, 1] = rep_vector(0.0, P);
      for (h in 2:H) {
        gamma[e, h] = mu_gamma[e] + dev[h - 1];
      }
    }
  }

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
  target += student_t_lpdf(sigma | nu_sigma, 0, scale_sigma) + sum(log_sigma);

  log_phi ~ std_normal();
  mu_phi ~ normal(0, 1);
  sigma_phi ~ normal(0, 1);

  mu_gamma0 ~ std_normal();
  for (h in 1:(H - 1)) {
    gamma_free[h] ~ std_normal();
  }
  sigma_gamma ~ exponential(1);

  for (e in 1:(E - 1)) {
    z_mu[e] ~ std_normal();
    for (h in 1:(H - 1)) {
      z_house[e, h] ~ std_normal();
    }
  }
  tau_mu_est ~ normal(0, tau_prior_scale);
  tau_house_est ~ normal(0, tau_prior_scale);

  for (n in 1:N) {
    int k = n_parties_n[n];
    vector[P] eta = beta[date_n[n]] + gamma[epoch_n[n], house_n[n]];
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
