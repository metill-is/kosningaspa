data {
  int<lower = 1> D;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of date x house observations

  array[N, P] int<lower = 0> y;           // Polling data (counts per party)

  array[N] int<lower = 1, upper = H> house; // House indicator for each poll
  array[N] int<lower = 1, upper = D> date;  // Date indicator for each poll
  vector[D - 1] time_diff;
  int<lower = 1> pred_y_time_diff;
  
  int<lower = 1> n_pred;
}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);
}

parameters {
  vector[P - 1] beta_0;                         // Party-specific initial effect
  matrix[P - 1, D + pred_y_time_diff] z_beta;   // Standardized random walk innovations
  matrix[P - 1, H - 1] gamma_raw;               // House effects (constant over time for each house)
  vector[P - 1] mu_gamma;
  vector<lower = 0>[P - 1] sigma_gamma;
  vector<lower = 0>[P - 1] sigma;               // Party-specific random walk scale
  real<lower = 0> phi_inv;
}

transformed parameters {
  real<lower = 0> phi = pow(phi_inv, -1);
  matrix[P - 1, H] gamma;                     
  matrix[P - 1, D + pred_y_time_diff] beta;   
  for (p in 1:(P - 1)) {
    // Fix the first house effect of the election to zero
    gamma[p, 1] = 0;                      
    // Free parameters for other houses
    gamma[p, 2:H] = mu_gamma[p] + sigma_gamma[p] * gamma_raw[p, ];         
  }
  
  beta[ , 1] = beta_0;

  for (t in 2:D) {
    beta[ , t] = beta[ , t - 1] + z_beta[, t - 1] .* sigma * time_scale[t - 1];
  }

  for (t in 1:pred_y_time_diff) {
    beta[ , D + t] = beta[ , D + t - 1] + z_beta[, D + t - 1] .* sigma * time_scale[D - 1];
  }
}

model {
  // Priors for house effects
  to_vector(gamma_raw) ~ std_normal();
  mu_gamma ~ std_normal();
  sum(mu_gamma) ~ std_normal();
  sigma_gamma ~ exponential(1);
  for (h in 2:(H - 1)) {
    sum(gamma_raw[ , h]) ~ std_normal();
  }

  // Priors for true party support
  beta_0 ~ std_normal();
  to_vector(z_beta) ~ std_normal();
  sigma ~ exponential(1);                 

  // Prior for the Dirichlet-multinomial scale parameter
  phi_inv ~ exponential(1);

  // Likelihood (Dirichlet-multinomial observation model)
  for (n in 1:N) {
    vector[P] eta_n;
    eta_n[2:P] = beta[, date[n]] + gamma[ , house[n]];  // Linear predictor for softmax
    eta_n[1] = -sum(eta_n[2:P]);
    vector[P] pi_n = softmax(eta_n);
    y[n, ] ~ dirichlet_multinomial(pi_n * phi);                // Polling data likelihood
  }
}

generated quantities {
  array[D + pred_y_time_diff, P] int<lower = 0> y_rep;;
  for (d in 1:(D + pred_y_time_diff)) {
    vector[P] eta_d;
    eta_d[2:P] = beta[, d];
    eta_d[1] = -sum(eta_d[2:P]);
    vector[P] pi_d = softmax(eta_d);
    y_rep[d, ] = dirichlet_multinomial_rng(pi_d * phi, n_pred);
  }

}
