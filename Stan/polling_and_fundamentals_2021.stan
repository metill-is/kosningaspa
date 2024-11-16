data {
  // Polling Data
  int<lower = 1> D;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N;                        // Number of date x house observations

  array[N, P] int<lower = 0> y_n;           // Polling data (counts per party)

  array[N] int<lower = 1, upper = H> house_n; // House indicator for each poll
  array[N] int<lower = 1, upper = D> date_n;  // Date indicator for each poll
  array[N] int<lower = 1, upper = P> n_parties_n; // Number of parties in each poll
  vector[D] month_before_election;
  
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
  vector[D] time_to_election;

  // Calculate time to election for each date
  time_to_election[D] = pred_y_time_diff;
  for (d in 1:(D - 1)) {
    time_to_election[D - d] = time_to_election[D - d + 1] + time_diff[D - d];
  }
}

parameters {
  // Polling Data
  matrix[P - 1, D + pred_y_time_diff] z_beta_raw;   // Standardized random walk innovations
  cholesky_factor_corr[P - 1] L_Omega;
  vector[P - 1] z_beta0;
  
  
  // House Effects Parameters
  matrix[P - 1, H - 1] gamma_raw;                   // Raw house effects
  vector[P - 2] mu_gamma;                           // Mean house effects
  vector<lower = 0>[P - 2] sigma_gamma;             // Scale of house effects

  // Variance Parameters
  real mu_log_sigma;                    // Population mean of log volatility
  real<lower=0> tau_log_sigma;          // Population SD of log volatility
  vector[P-1] log_sigma_raw;            // Raw party-specific parameters
  vector<lower = 0>[H] phi_inv;
  real<lower = 0> tau_stjornarslit;


  // Fundamentals
  vector[P_f - 1] alpha_f_raw;
  real beta_lag_f;
  real beta_inc_years_f;
  real beta_vnv_f;
  real beta_growth_f;
  real<lower = 0> phi_f_inv;
}

transformed parameters {
  vector<lower=0>[P-1] sigma = exp(mu_log_sigma + tau_log_sigma * log_sigma_raw);           // Party-specific volatilities
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
  vector[P - 1] beta0;
  vector[P - 1] mu_pred = alpha_f[index_f[2:n_parties_f[D_f + 1], D_f + 1]] + 
    beta_lag_f * x_f[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] + 
    beta_inc_years_f * incumbent_years[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_vnv_f * vnv[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_growth_f * growth[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1];
  
  // Calculate V(t) based on weight_time and tau_stjornarslit
  V_t = weight_time;
  
  // Calculate V(t) based on weight_time and tau_stjornarslit
  if (weight_time <= 47) {
    V_t = weight_time * square(1 + tau_stjornarslit);
  } else {
    V_t = (weight_time - 47) + 47 * square(1 + tau_stjornarslit);
  }
  
  // Calculate tau_f based on desired weight and V_t
  tau_f = sqrt(V_t * (1 - desired_weight) / desired_weight);


  // Set up house effects
  for (p in 1:(P - 2)) {
    gamma[p, 1] = 0;                          // Reference house effect set to 0
    gamma[p, 2:H] = mu_gamma[p] + sigma_gamma[p] * gamma_raw[p, ];  // Other house effects
  }

  gamma[P - 1, 1] = 0;

  for (h in 2:H) {
    gamma[P - 1, h] = -sum(gamma[1:(P - 2), h]);
  }


  beta0 = mu_pred + tau_f * sigma .* z_beta0;
  beta[ , D + pred_y_time_diff] = beta0;

  for (t in 1:pred_y_time_diff) {
    real scale;
    if (t <= 47) {
      scale = 1 + tau_stjornarslit;
    } else {
      scale = 1;
    }
    beta[ , D + pred_y_time_diff - t] = beta[ , D + pred_y_time_diff - t + 1] + 
      z_beta[, D + pred_y_time_diff - t + 1] .* sigma * scale;
  }

  for (t in 1:(D - 1)) {
    beta[ , D - t] = beta[ , D - t + 1] + 
      time_scale[D - t] * z_beta[, D - t + 1] .* sigma * (1 + tau_stjornarslit * month_before_election[D - t]);
  }

  
}

model {
  
  /* Polling Data */
  // Priors for house effects
  to_vector(gamma_raw) ~ std_normal();        // Raw house effects
  mu_gamma ~ std_normal();                    // Mean house effects
  sigma_gamma ~ exponential(1);               // House effect scales

  // Priors for true party support
  to_vector(z_beta_raw) ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(2);
  mu_log_sigma ~ normal(-4, 1);         // Prior centered on small volatility
  tau_log_sigma ~ exponential(2);       // Weakly informative prior on variance
  log_sigma_raw ~ std_normal();         // Unit normal for non-centered param    
  z_beta0 ~ std_normal();
  tau_stjornarslit ~ exponential(1);
  // Prior for the Dirichlet-multinomial scale parameter
  phi_inv ~ exponential(1);

  // Likelihood (Dirichlet-multinomial observation model)
  for (n in 1:N) {
    vector[P] eta_n;
    eta_n[2:P] = beta[, date_n[n]] + gamma[ , house_n[n]];  // Linear predictor for softmax
    eta_n[1] = -sum(eta_n[2:P]);
    vector[P] pi_n = softmax(eta_n);
    y_n[n, 1:n_parties_n[n]] ~ dirichlet_multinomial(pi_n[1:n_parties_n[n]] * phi[house_n[n]]);                // Polling data likelihood
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
  array[P] int<lower = 0> election_prediction;
  
  vector[P] eta_d;
  eta_d[2:P] = beta[, D + pred_y_time_diff];
  eta_d[1] = -sum(eta_d[2:P]);
  vector[P] pi_d = softmax(eta_d);
  election_prediction = dirichlet_multinomial_rng(pi_d * phi[1], n_pred);
}




