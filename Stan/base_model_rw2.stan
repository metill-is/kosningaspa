data {
  int<lower = 1> D;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of date x house observations

  array[N, P] int<lower = 0> y;           // Polling data (counts per party)

  array[N] int<lower = 1, upper = H> house; // House indicator for each poll
  array[N] int<lower = 1, upper = D> date;  // Date indicator for each poll

  real<lower = 0> sigma_house_sum;
  real<lower = 0> sigma_house;
  int<lower = 1> n_pred;
  real<lower = 0> trend_damping;
}

parameters {
  real<lower = 0> phi_inv;
  vector[P] beta_0;                      // Party-specific initial effect
  vector[P] beta_1;
  matrix[P, D - 2] z_beta;                   // Second order innovations
  matrix[P, H - 1] gamma_raw;            // House effects (constant over time for each house)
  vector<lower = 0>[P] sigma_beta;            // Party-specific random walk scale
}

transformed parameters {
  real<lower = 0> phi = pow(phi_inv, -1);
  matrix[P, H] gamma;
  matrix[P, D] beta;                     // Dynamic party effects over time
  
  for (p in 1:P) {
    gamma[p, 1] = 0;                   // Fix the house effect of the election to zero
    gamma[p, 2:H] = gamma_raw[p];      // Free parameters for other houses
  }

  for (p in 1:P) {
    beta[p, 1] = beta_0[p];
    beta[p, 2] = beta_1[p];
    for (t in 3:D) {
      beta[p, t] = 2 * beta[p, t - 1] - beta[p, t - 2] + sigma_beta[p] * z_beta[p, t - 2];
    }
  }
}

model {
  // Priors for beta (dynamic main effects for each party)
  for (p in 1:P) {
    gamma_raw[p, ] ~ normal(0, sigma_house);      // House effects prior
  }

  beta_0 ~ std_normal();
  beta_1 ~ std_normal();
  to_vector(z_beta) ~ std_normal();
  sigma_beta ~ exponential(5);                 // First order innovation scale
  phi_inv ~ exponential(1);
  // Likelihood (Multinomial observation model)
  for (n in 1:N) {
    vector[P] eta_n = beta[ , date[n]] + gamma[ , house[n]];  // Linear predictor for softmax
    vector[P] pi_n = softmax(eta_n);
    y[n, ] ~ dirichlet_multinomial(pi_n * phi);                // Polling data likelihood
  }
}

generated quantities {
  array[D, P] int<lower = 0> y_rep;
  vector[P] industry_bias;
  for (d in 1:D) {
    vector[P] pi_n = softmax(beta[ , d]);
    y_rep[d, ] = dirichlet_multinomial_rng(pi_n * phi, n_pred);
  }

  for (p in 1:P) {
    industry_bias[p] = mean(gamma_raw[p, ]);
  }
}
