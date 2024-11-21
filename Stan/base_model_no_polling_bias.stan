data {
  // Polling Data
  int<lower = 1> D;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of date x house observations
  int<lower = 1> days_between_stjornarslit_and_election;

  array[N, P] int<lower = 0> y;           // Polling data (counts per party)

  array[N] int<lower = 1, upper = H> house; // House indicator for each poll
  array[N] int<lower = 1, upper = D> date;  // Date indicator for each poll
  array[N] int<lower = 1, upper = P> n_parties; // Number of parties in each poll
  vector[D] stjornarslit;
  vector[D] post_stjornarslit;
  
  vector[D - 1] time_diff;
  int<lower = 1> pred_y_time_diff;
  
  int<lower = 1> n_pred;

  // Fundamentals
  int<lower = 1> D_f;
  int<lower = 1> P_f;
  array[P_f, D_f] int<lower = 0> y_f;
  matrix[P_f, D_f + 1] x_f;
  matrix[P_f, D_f + 1] incumbent_f;
  matrix[P_f, D_f + 1] incumbent_years;
  matrix[P_f, D_f + 1] vnv;
  matrix[P_f, D_f + 1] growth;
  int<lower = 1> max_n_parties_f;
  array[max_n_parties_f, D_f + 1] int<lower = 0> index_f;
  array[D_f + 1] int<lower = 0, upper = P_f> n_parties_f;

  real<lower = 0, upper = 1> desired_weight;  // Desired weight for fundamentals at t=180
  int<lower = 0> weight_time;                 // Time point for desired weight (e.g., 180)

}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);
}

parameters {
  // Polling Data
  matrix[P - 1, D + pred_y_time_diff] z_beta_raw;   // Standardized random walk innovations
  vector[P - 1] beta0;
  cholesky_factor_corr[P - 1] L_Omega;
  vector<lower = 0>[P - 1] sigma;               // Party-specific random walk scale

  matrix[P - 1, H - 2] gamma_raw;               // House effects (constant over time for each house)

  vector<lower = 0>[H] phi_inv;
}

transformed parameters {
  vector<lower = 0>[H] phi = pow(phi_inv, -1);
  matrix[P - 1, D + pred_y_time_diff] z_beta = L_Omega * z_beta_raw;
  matrix[P - 1, H] gamma;                     
  matrix[P - 1, D + pred_y_time_diff] beta;   

  for (p in 1:(P - 1)) {
    gamma[p, 1] = 0;                   // Fix the first house effect of the election to zero
    gamma[p, 2] = -sum(gamma_raw[p, ]);
    gamma[p, 3:H] = gamma_raw[p, ];      // Free parameters for other houses
  }     
  
  
  beta[ , 1] = beta0;

  for (t in 2:D) {
    beta[, t] = beta[ , t - 1] + time_scale[t - 1] * z_beta[, t - 1] .* sigma;
  }

  for (t in 1:pred_y_time_diff) {
    beta[ , D + t] = beta[ , D + t - 1] + z_beta[, D + t - 1] .* sigma;
  }

  

  
}

model {

  /* Polling Data */
  // Priors for house effects
  to_vector(gamma_raw) ~ std_normal();
  for (h in 3:H) {
    sum(gamma[ , h]) ~ normal(0, 0.1);
  }

  // Priors for true party support
  to_vector(z_beta_raw) ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(2);
  sigma ~ exponential(1);                 
  beta0 ~ std_normal();
  // Prior for the Dirichlet-multinomial scale parameter
  phi_inv ~ exponential(1);

  // Likelihood (Dirichlet-multinomial observation model)
  for (n in 1:N) {
    vector[P] eta_n;
    eta_n[2:P] = beta[, date[n]] + gamma[ , house[n]];  // Linear predictor for softmax
    eta_n[1] = -sum(eta_n[2:P]);
    vector[P] pi_n = softmax(eta_n);
    y[n, 1:n_parties[n]] ~ dirichlet_multinomial(pi_n[1:n_parties[n]] * phi[house[n]]);                // Polling data likelihood
  }

}

generated quantities {
  array[D + pred_y_time_diff, P] int<lower = 0> y_rep;
  corr_matrix[P - 1] Omega = L_Omega * L_Omega';
  for (d in 1:(D + pred_y_time_diff)) {
    vector[P] eta_d;
    eta_d[2:P] = beta[, d];
    eta_d[1] = -sum(eta_d[2:P]);
    vector[P] pi_d = softmax(eta_d);
    y_rep[d, ] = dirichlet_multinomial_rng(pi_d * phi[1], n_pred);
  }

}




