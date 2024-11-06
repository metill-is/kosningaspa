data {
  /* Polling Data */
  // National Level
  int<lower = 1> D;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of date x house observations
  int<lower = 1> N_n;
  int<lower = 1> days_between_stjornarslit_and_election;

  array[N_n, P] int<lower = 0> y_n;           // Polling data (counts per party)

  array[N_n] int<lower = 1, upper = H> house_n; // House indicator for each poll
  array[N_n] int<lower = 1, upper = D> date_n;  // Date indicator for each poll
  array[N_n] int<lower = 1, upper = P> n_parties_n; // Number of parties in each poll

  vector[D] stjornarslit;
  vector[D] post_stjornarslit;
  
  vector[D - 1] time_diff;
  int<lower = 1> pred_y_time_diff;
  
  int<lower = 1> n_pred;

  // Constituency Level
  int<lower = 1> K;                        // Number of constituencies
  int<lower = 1> N_k;                      // Number of constituency polls
  array[N_k, P] int<lower = 0> y_k;        // Constituency polling data
  array[N_k] int<lower = 1, upper = H> house_k;
  array[N_k] int<lower = 1, upper = D> date_k;
  array[N_k] int<lower = 1, upper = P> n_parties_k;
  array[N_k] int<lower = 1, upper = K> constituency_k;

  /* Fundamentals Data*/
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
  /* Polling */
  // Party-specific random ralks
  matrix[P - 1, D + pred_y_time_diff] z_beta_raw;
  cholesky_factor_corr[P - 1] L_Omega;
  vector[P - 1] beta0;
  vector<lower = 0>[P - 1] sigma;        
  real<lower = 0> tau_stjornarslit;

  // Constituency effects
  matrix[P - 1, K - 1] delta_raw;
  vector<lower = 0>[P - 1] sigma_delta;

  
  // House effects
  matrix[P - 1, H - 1] gamma_raw;    
  vector[P - 1] mu_gamma;
  vector<lower = 0>[P - 1] sigma_gamma;
  vector<lower = 0>[H] phi_inv;

  /* Fundamentals */
  vector[P_f - 1] alpha_f_raw;
  real beta_lag_f;
  real beta_inc_years_f;
  real beta_vnv_f;
  real beta_growth_f;
  real<lower = 0> phi_f_inv;
}

transformed parameters {
  vector<lower = 0>[H] phi = pow(phi_inv, -1);
  real<lower = 0> phi_f = pow(phi_f_inv, -1);
  matrix[P - 1, D + pred_y_time_diff] z_beta = L_Omega * z_beta_raw;
  matrix[P - 1, H] gamma;                     
  matrix[P - 1, D + pred_y_time_diff] beta;   
  vector[P_f] alpha_f;
  alpha_f[2:P_f] = alpha_f_raw;
  alpha_f[1] = -sum(alpha_f[2:P_f]);
  real<lower = 0> V_t;
  real<lower = 0> tau_f;
  matrix[P - 1, K] delta;
  
  // Calculate V(t) based on weight_time and tau_stjornarslit
  if (weight_time <= 47) {
    V_t = weight_time * square(1 + tau_stjornarslit);
  } else {
    V_t = (weight_time - 47) + 47 * square(1 + tau_stjornarslit);
  }
  
  // Calculate tau_f based on desired weight and V_t
  tau_f = sqrt(V_t * (1 - desired_weight) / desired_weight);
  
  for (p in 1:(P - 1)) {
    // Fix the first house effect of the election to zero
    gamma[p, 1] = 0;                      
    // Free parameters for other houses
    gamma[p, 2:H] = mu_gamma[p] + sigma_gamma[p] * gamma_raw[p, ];    
    
    // Each party's constituency effects sum to zero
    delta[p, 1:(K-1)] = sigma_delta[p] * delta_raw[p, ];
    delta[p, K] = -sum(delta[p, 1:(K-1)]);
  }
  
  beta[ , D + pred_y_time_diff] = beta0;

  for (t in 1:pred_y_time_diff) {
    real scale;
    if (t <= days_between_stjornarslit_and_election) {
      scale = 1 + tau_stjornarslit;
    } else {
      scale = 1;
    }
    beta[ , D + pred_y_time_diff - t] = beta[ , D + pred_y_time_diff - t + 1] + 
      z_beta[, D + pred_y_time_diff - t + 1] .* sigma * scale;
  }

  for (t in 1:(D - 1)) {
    beta[ , D - t] = beta[ , D - t + 1] + 
      time_scale[D - t] * z_beta[, D - t + 1] .* sigma * (1 + tau_stjornarslit * post_stjornarslit[D - t]);
  }

  
}

model {
  vector[P - 1] mu_pred = alpha_f[index_f[2:n_parties_f[D_f + 1], D_f + 1]] + 
    beta_lag_f * x_f[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] + 
    beta_inc_years_f * incumbent_years[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_vnv_f * vnv[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_growth_f * growth[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1];

  
  /* Polling Data */
  // Priors for house effects
  to_vector(gamma_raw) ~ std_normal();
  mu_gamma ~ std_normal();
  sum(mu_gamma) ~ std_normal();
  sigma_gamma ~ exponential(1);
  phi_inv ~ exponential(1);
  for (h in 2:(H - 1)) {
    sum(gamma_raw[ , h]) ~ std_normal();
  }

  // Priors for latent party support
  to_vector(z_beta_raw) ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(2);
  sigma ~ exponential(1);                 
  beta0 ~ normal(mu_pred, tau_f * sigma);
  tau_stjornarslit ~ normal(0, 0.01);

  

  // National-level likelihood (Dirichlet-multinomial observation model)
  for (n in 1:N_n) {
    vector[P] eta_n;
    eta_n[2:P] = beta[, date_n[n]] + gamma[ , house_n[n]];  // Linear predictor for softmax
    eta_n[1] = -sum(eta_n[2:P]);
    vector[P] pi_n = softmax(eta_n);
    y_n[n, 1:n_parties_n[n]] ~ dirichlet_multinomial(pi_n[1:n_parties_n[n]] * phi[house_n[n]]);                // Polling data likelihood
  }

  // Constituency priors
  to_vector(delta_raw) ~ std_normal();
  sigma_delta ~ exponential(1);

  // Constituency-level likelihood
  for (n in 1:N_k) {
    vector[P] eta_k;
    eta_k[2:P] = beta[, date_k[n]] + gamma[, house_k[n]] + delta[, constituency_k[n]];
    eta_k[1] = -sum(eta_k[2:P]);
    vector[P] pi_k = softmax(eta_k);
    y_k[n, 1:n_parties_k[n]] ~ dirichlet_multinomial(pi_k[1:n_parties_k[n]] * phi[house_k[n]]);
  }

  /* Fundamentals */
  // Priors
  beta_lag_f ~ std_normal();
  beta_inc_years_f ~ std_normal();
  alpha_f_raw ~ std_normal();
  phi_f_inv ~ exponential(1);

  for (d in 1:D_f) { 
    vector[n_parties_f[d]] mu_d;
    mu_d[1:n_parties_f[d]] = alpha_f[index_f[1:n_parties_f[d], d]] + 
      beta_lag_f * x_f[index_f[1:n_parties_f[d], d], d] + 
      beta_inc_years_f * incumbent_years[index_f[1:n_parties_f[d], d], d] +
      beta_vnv_f * vnv[index_f[1:n_parties_f[d], d], d] +
      beta_growth_f * growth[index_f[1:n_parties_f[d], d], d];
    vector[n_parties_f[d]] pi_d = softmax(mu_d);
    y_f[index_f[1:n_parties_f[d], d], d] ~ dirichlet_multinomial(pi_d * phi_f);
  }
}

generated quantities {
  array[D + pred_y_time_diff, P] int<lower = 0> y_rep_national;
  array[P, K] int<lower = 0> y_rep_constituency;
  corr_matrix[P - 1] Omega = L_Omega * L_Omega';
  for (d in 1:(D + pred_y_time_diff)) {
    vector[P] eta_d;
    eta_d[2:P] = beta[, d];
    eta_d[1] = -sum(eta_d[2:P]);
    vector[P] pi_d = softmax(eta_d);
    y_rep_national[d, ] = dirichlet_multinomial_rng(pi_d * phi[1], n_pred);
  }

  for (k in 1:K) {
    vector[P] eta_k;
    eta_k[2:P] = beta[, D + pred_y_time_diff] + delta[, k];
    eta_k[1] = -sum(eta_k[2:P]);
    vector[P] pi_k = softmax(eta_k);
    y_rep_constituency[, k] = dirichlet_multinomial_rng(pi_k * phi[1], n_pred);
  }

}




