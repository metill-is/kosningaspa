data {
  // Polling
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


  // Fundamentals
  int<lower = 1> D_fundamentals;
  int<lower = 1> P_fundamentals;
  int<lower = 1> xdim_fundamentals;

  array[D_fundamentals, P_fundamentals] int<lower = 0> y_fundamentals;
  matrix[D_fundamentals, xdim_fundamentals] x_fundamentals;
}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);
}

parameters {
  vector[P] beta_0;                         // Party-specific initial effect
  matrix[P, D + pred_y_time_diff] z_beta;   // Standardized random walk innovations
  matrix[P, H - 1] gamma_raw;               // House effects (constant over time for each house)
  vector[P] mu_gamma;
  vector<lower = 0>[P] sigma_gamma;
  vector<lower = 0>[P] sigma;               // Party-specific random walk scale
  real<lower = 0> phi_inv;
}

transformed parameters {
  real<lower = 0> phi = pow(phi_inv, -1);
  matrix[P, H] gamma;                     
  matrix[P, D + pred_y_time_diff] beta;   
  for (p in 1:P) {
    // Fix the first house effect of the election to zero
    gamma[p, 1] = 0;                      
    // Free parameters for other houses
    gamma[p, 2:H] = mu_gamma[p] + sigma_gamma[p] * gamma_raw[p, ];         
  }

  // True party support over time
  for (p in 1:P) {
    beta[p, 1] = beta_0[p];
    for (t in 2:D) {
      beta[p, t] = beta[p, t - 1] + z_beta[p, t - 1] * sigma[p] * time_scale[t - 1];
    }

    // Daily predictions up until the last date
    for (t in 1:pred_y_time_diff) {
      beta[p, D + t] = beta[p, D + t - 1] + z_beta[p, D + t - 1] * sigma[p]; // Daily predictions, so no time scale
    }
  }
}

model {
  // Priors for house effects
  to_vector(gamma_raw) ~ std_normal();
  mu_gamma ~ normal(0, 0.1);
  sum(mu_gamma) ~ normal(0, 0.1);
  sigma_gamma ~ exponential(1);
  for (h in 2:(H - 1)) {
    sum(gamma_raw[ , h]) ~ normal(0, 0.1);
  }

  // Priors for true party support
  beta_0 ~ std_normal();
  to_vector(z_beta) ~ std_normal();
  sigma ~ exponential(1);                 

  // Prior for the Dirichlet-multinomial scale parameter
  phi_inv ~ exponential(1);

  // Likelihood (Dirichlet-multinomial observation model)
  for (n in 1:N) {
    vector[P] eta_n = beta[ , date[n]] + gamma[ , house[n]];  // Linear predictor for softmax
    vector[P] pi_n = softmax(eta_n);
    y[n, ] ~ dirichlet_multinomial(pi_n * phi);                // Polling data likelihood
  }
}

generated quantities {
  array[D + pred_y_time_diff, P] int<lower = 0> y_rep;
  vector[P] industry_bias;
  for (d in 1:(D + pred_y_time_diff)) {
    vector[P] pi_d = softmax(beta[ , d]);
    y_rep[d, ] = dirichlet_multinomial_rng(pi_d * phi, n_pred);
  }

  for (p in 1:P) {
    industry_bias[p] = mean(gamma_raw[p, ]);
  }
}
