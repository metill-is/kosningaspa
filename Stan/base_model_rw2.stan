data {
  int<lower = 1> D;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of date x house observations

  array[N, P] int<lower = 0> y;           // Polling data (counts per party)

  array[N] int<lower = 1, upper = H> house; // House indicator for each poll
  array[N] int<lower = 1, upper = D> date;  // Date indicator for each poll
  vector[D - 1] time_diff;
  real pred_y_time_diff;

  real<lower = 0> sigma_house_sum;
  real<lower = 0> sigma_house;
  int<lower = 1> n_pred;
}

parameters {
  vector[P] beta_0;                      // Party-specific initial effect
  matrix[P, D] z_beta;                   // Standardized random walk innovations
  matrix[P, H - 1] gamma_raw;            // House effects (constant over time for each house)
  vector<lower = 0>[P] sigma;            // Party-specific random walk scale
  real<lower = 0> phi_inv;
}

transformed parameters {
  real<lower = 0> phi = pow(phi_inv, -1);
  matrix[P, H] gamma;
  matrix[P, D + 1] beta;  // Dynamic party effects over time
  
  // Polling house effects
  for (p in 1:P) {
    gamma[p, 1] = 0;                  // Fix the first house effect to zero
    gamma[p, 2:H] = gamma_raw[p];     // Free parameters for other houses
  }

  // Initialize the first two time points
  for (p in 1:P) {
    beta[p, 1] = beta_0[p];
    beta[p, 2] = beta[p, 1] + z_beta[p, 1] * sigma[p] * sqrt(time_diff[1]);
  }

  // Second-order random walk
  for (p in 1:P) {
    for (t in 3:D) {
      beta[p, t] = 2 * beta[p, t - 1] - beta[p, t - 2] + z_beta[p, t - 1] * sigma[p] * sqrt(time_diff[t - 1]);
    }
    // Prediction for time D + 1
    beta[p, D + 1] = 2 * beta[p, D] - beta[p, D - 1] + z_beta[p, D] * sigma[p] * sqrt(pred_y_time_diff);
  }
}

model {
  // Priors for beta (dynamic main effects for each party)
  for (p in 1:P) {
    gamma_raw[p, ] ~ normal(0, sigma_house);      // House effects prior
  }

  beta_0 ~ normal(0, 1);
  to_vector(z_beta) ~ std_normal();
  sigma ~ exponential(1);                 // Random walk scale prior
  phi_inv ~ exponential(1);
  // Likelihood (Multinomial observation model)
  for (n in 1:N) {
    vector[P] eta_n = beta[ , date[n]] + gamma[ , house[n]];  // Linear predictor for softmax
    vector[P] pi_n = softmax(eta_n);
    y[n, ] ~ dirichlet_multinomial(pi_n * phi);                // Polling data likelihood
  }
}

generated quantities {
  array[D + 1, P] int<lower = 0> y_rep;
  vector[P] industry_bias;
  for (d in 1:(D + 1)) {
    vector[P] pi_d = softmax(beta[ , d]);
    y_rep[d, ] = dirichlet_multinomial_rng(pi_d * phi, n_pred);
  }

  for (p in 1:P) {
    industry_bias[p] = mean(gamma_raw[p, ]);
  }
}
