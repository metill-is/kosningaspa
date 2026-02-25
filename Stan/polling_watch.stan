data {
  int<lower = 1> D;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of polls

  array[N, P] int<lower = 0> y_n;          // Polling data (counts per party)
  array[N] int<lower = 1, upper = H> house_n;  // House indicator for each poll
  array[N] int<lower = 1, upper = D> date_n;   // Date indicator for each poll
  array[N] int<lower = 1, upper = P> n_parties_n;  // Number of parties in each poll

  vector[D - 1] time_diff;                 // Time differences between consecutive dates
  int<lower = 1> n_pred;                   // Sample size for posterior predictions
}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);
  vector[N] n_sample;

  for (n in 1:N) {
    n_sample[n] = sum(y_n[n, ]);
  }
}

parameters {
  matrix[P - 1, D] z_beta_raw;             // Raw random walk innovations
  cholesky_factor_corr[P - 1] L_Omega;     // Cholesky factor of correlation matrix
  vector[P - 1] beta0;                     // Initial support levels

  // Variance parameters
  real mu_log_sigma;
  real<lower = 0> tau_log_sigma;
  vector[P - 1] log_sigma_raw;

  // Per-firm overdispersion (hierarchical)
  vector[H - 1] log_phi;
  real mu_phi;
  real<lower = 0> sigma_phi;

  // House effects (systematic polling bias)
  matrix[P - 2, H - 1] gamma_raw;
  vector[P - 2] mu_gamma;
  vector<lower = 0>[P - 2] sigma_gamma;
}

transformed parameters {
  vector<lower = 0>[P - 1] sigma = exp(mu_log_sigma + tau_log_sigma * log_sigma_raw);
  vector<lower = 0>[H - 1] phi = exp(mu_phi + sigma_phi * log_phi);

  // House effects: reference house (Kosning) = 0, last party by zero-sum
  matrix[P - 1, H] gamma;
  for (p in 1:(P - 2)) {
    gamma[p, 1] = 0;
    gamma[p, 2:H] = mu_gamma[p] + sigma_gamma[p] * gamma_raw[p, ];
  }
  gamma[P - 1, 1] = 0;
  for (h in 2:H) {
    gamma[P - 1, h] = -sum(gamma[1:(P - 2), h]);
  }

  matrix[P - 1, D] z_beta = diag_pre_multiply(sigma, L_Omega) * z_beta_raw;
  matrix[P - 1, D] beta;

  // Initialize first time point
  beta[, 1] = beta0;

  // Forward random walk evolution
  for (t in 2:D) {
    beta[, t] = beta[, t - 1] + time_scale[t - 1] * z_beta[, t];
  }
}

model {
  // Priors
  to_vector(z_beta_raw) ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(10);
  beta0 ~ normal(0, 2);
  mu_log_sigma ~ normal(-2, 2);
  tau_log_sigma ~ exponential(1);
  log_sigma_raw ~ std_normal();

  log_phi ~ std_normal();
  mu_phi ~ normal(0, 1);
  sigma_phi ~ normal(0, 1);

  to_vector(gamma_raw) ~ std_normal();
  mu_gamma ~ std_normal();
  sigma_gamma ~ exponential(1);

  // Likelihood
  for (n in 1:N) {
    vector[P] eta;
    eta[2:P] = beta[, date_n[n]] + gamma[, house_n[n]];
    eta[1] = -sum(eta[2:P]);

    if (house_n[n] > 1) {
      // Dirichlet-multinomial for polls (overdispersed)
      vector[n_parties_n[n]] pi_n = softmax(eta[1:n_parties_n[n]]);
      y_n[n, 1:n_parties_n[n]] ~ dirichlet_multinomial(pi_n * phi[house_n[n] - 1]);
    } else {
      // Multinomial for election results (no overdispersion)
      y_n[n, 1:n_parties_n[n]] ~ multinomial_logit(eta[1:n_parties_n[n]]);
    }
  }
}

generated quantities {
  corr_matrix[P - 1] Omega = L_Omega * L_Omega';

  // Smoothed probabilities at each date
  array[D, P] real pi_smooth;
  for (d in 1:D) {
    vector[P] eta_d;
    eta_d[2:P] = beta[, d];
    eta_d[1] = -sum(eta_d[2:P]);
    pi_smooth[d] = to_array_1d(softmax(eta_d));
  }

  // Predicted counts at last date
  {
    vector[P] eta_last;
    eta_last[2:P] = beta[, D];
    eta_last[1] = -sum(eta_last[2:P]);
  }
  array[P] int<lower = 0> y_rep = multinomial_logit_rng(
    append_row(-sum(beta[, D]), beta[, D]),
    n_pred
  );
}
