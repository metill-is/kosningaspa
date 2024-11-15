data {
  // Polling Data Section
  int<lower = 1> D;                        // Number of time points in polling data
  int<lower = 1> P;                        // Number of political parties
  int<lower = 1> H;                        // Number of polling houses/companies
  int<lower = 1> N;                        // Total number of polls
  int<lower = 1> days_between_stjornarslit_and_election;  // Days between government collapse and election
  
  // Core polling data arrays
  array[N, P] int<lower = 0> y_n;          // Poll results: counts for each party in each poll
  array[N] int<lower = 1, upper = H> house_n;  // Which polling house conducted each poll
  array[N] int<lower = 1, upper = D> date_n;   // When each poll was conducted
  array[N] int<lower = 1, upper = P> n_parties_n;  // Number of parties included in each poll
  
  // Time-related vectors
  vector[D] stjornarslit;                  // Indicator for government collapse period
  vector[D] post_stjornarslit;             // Indicator for post-collapse period
  vector[D] month_before_election;          // Indicator for month before election
  vector[D - 1] time_diff;                 // Time differences between consecutive polls
  
  // Prediction-related parameters
  int<lower = 1> pred_y_time_diff;         // How far into future to predict
  int<lower = 1> n_pred;                   // Number of predictions to generate
  
  // Latest poll information
  int<lower = 1> last_poll_days;           // Days since last poll
  int<lower = 1> last_poll_house;          // House that conducted last poll
  int<lower = 1> n_last_poll;              // Sample size of last poll

  // Fundamentals Data Section
  int<lower = 1> D_f;                      // Number of past elections
  int<lower = 1> P_f;                      // Number of parties in fundamentals data
  array[P_f, D_f] int<lower = 0> y_f;      // Historical election results
  
  // Predictor matrices for fundamentals model
  matrix[P_f, D_f + 1] x_f;                // Previous vote share
  matrix[P_f, D_f + 1] incumbent_f;        // Incumbent status
  matrix[P_f, D_f + 1] incumbent_years;    // Years as incumbent
  matrix[P_f, D_f + 1] vnv;                // Vote/No Vote status
  matrix[P_f, D_f + 1] growth;             // Economic growth
  
  // Indexing arrays for fundamentals
  int<lower = 1> max_n_parties_f;          // Maximum number of parties in any election
  array[max_n_parties_f, D_f + 1] int<lower = 0> index_f;  // Party index mapping
  array[D_f + 1] int<lower = 0, upper = P_f> n_parties_f;  // Parties per election
  
  // Model weighting parameters
  real<lower = 0, upper = 1> desired_weight;  // Desired weight for fundamentals at specified time
  int<lower = 0> weight_time;                 // Time point for desired weight
}

transformed data {
  // Scale time differences by square root to moderate the impact of longer gaps
  vector[D - 1] time_scale = sqrt(time_diff);
}

parameters {
  // Polling Model Parameters
  matrix[P - 1, D + pred_y_time_diff] z_beta_raw;   // Raw random walk innovations
  cholesky_factor_corr[P - 1] L_Omega;              // Cholesky factor of correlation matrix
  vector[P - 1] beta0;                              // Initial party support levels
  
  // House Effects Parameters
  matrix[P - 1, H - 1] gamma_raw;                   // Raw house effects
  vector[P - 2] mu_gamma;                           // Mean house effects
  vector<lower = 0>[P - 2] sigma_gamma;             // Scale of house effects
  
  // Variance Parameters
  real mu_log_sigma;                    // Population mean of log volatility
  real<lower=0> tau_log_sigma;          // Population SD of log volatility
  vector[P-1] log_sigma_raw;            // Raw party-specific parameters
  vector<lower = 0>[H] phi_inv;                     // Inverse dispersion parameters
  real<lower = 0> tau_stjornarslit;                // Scale factor for government collapse period
  
  // Fundamentals Model Parameters
  vector[P_f - 1] alpha_f_raw;                      // Party-specific intercepts
  real beta_lag_f;                                  // Effect of previous vote share
  real beta_inc_years_f;                            // Effect of incumbent years
  real beta_vnv_f;                                  // Effect of vote/no vote status
  real beta_growth_f;                               // Effect of economic growth
  real<lower = 0> phi_f_inv;                        // Inverse dispersion for fundamentals
}

transformed parameters {
  vector<lower=0>[P-1] sigma = exp(mu_log_sigma + tau_log_sigma * log_sigma_raw);           // Party-specific volatilities

  // Convert inverse dispersion parameters to dispersion parameters
  vector<lower = 0>[H] phi = pow(phi_inv, -1);        // Polling dispersion
  real<lower = 0> phi_f = pow(phi_f_inv, -1);         // Fundamentals dispersion
  
  // Transform raw parameters into model parameters
  matrix[P - 1, D + pred_y_time_diff] z_beta = L_Omega * z_beta_raw;  // Correlated random walk innovations
  matrix[P - 1, H] gamma;                     // House effects matrix
  matrix[P - 1, D + pred_y_time_diff] beta;   // Party support trajectories
  vector[P_f] alpha_f;                        // Party-specific intercepts for fundamentals
  
  // Sum-to-zero constraint for party intercepts
  alpha_f[2:P_f] = alpha_f_raw;
  alpha_f[1] = -sum(alpha_f[2:P_f]);
  
  // Variables for combining polling and fundamentals
  real<lower = 0> V_t;                        // Total variance at weight_time
  real<lower = 0> tau_f;                      // Scale parameter for fundamentals
  
  // Calculate variance based on time period
  if (weight_time <= 47) {
    // Higher variance during government collapse period
    V_t = weight_time * square(1 + tau_stjornarslit);
  } else {
    // Normal variance after collapse period
    V_t = (weight_time - 47) + 47 * square(1 + tau_stjornarslit);
  }
  
  // Calculate fundamentals weight parameter
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
  
  // Initialize end state
  beta[ , D + pred_y_time_diff] = beta0;
  
  // Forward prediction period
  for (t in 1:pred_y_time_diff) {
    real scale;
    // Adjust volatility during government collapse period
    if (t <= days_between_stjornarslit_and_election) {
      scale = 1 + tau_stjornarslit;
    } else {
      scale = 1;
    }
    // Random walk evolution
    beta[ , D + pred_y_time_diff - t] = beta[ , D + pred_y_time_diff - t + 1] + 
      z_beta[, D + pred_y_time_diff - t + 1] .* sigma * scale;
  }
  
  // Backward evolution through historical period
  for (t in 1:(D - 1)) {
    beta[ , D - t] = beta[ , D - t + 1] + 
      time_scale[D - t] * z_beta[, D - t + 1] .* sigma * 
      (1 + tau_stjornarslit * month_before_election[D - t]);
  }
}

model {
  // Calculate predicted values from fundamentals model
  vector[P - 1] mu_pred = alpha_f[index_f[2:n_parties_f[D_f + 1], D_f + 1]] + 
    beta_lag_f * x_f[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] + 
    beta_inc_years_f * incumbent_years[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_vnv_f * vnv[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_growth_f * growth[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1];

  /* Polling Data Priors */
  // House effects priors
  to_vector(gamma_raw) ~ std_normal();        // Raw house effects
  mu_gamma ~ std_normal();                    // Mean house effects
  sigma_gamma ~ exponential(1);               // House effect scales

  // Party support priors
  to_vector(z_beta_raw) ~ std_normal();       // Random walk innovations
  L_Omega ~ lkj_corr_cholesky(2);            // Prior for correlation matrix
  mu_log_sigma ~ normal(-4, 1);         // Prior centered on small volatility
  tau_log_sigma ~ exponential(2);       // Weakly informative prior on variance
  log_sigma_raw ~ std_normal();         // Unit normal for non-centered param
  beta0 ~ normal(mu_pred, tau_f * sigma);     // Initial state (combines with fundamentals)
  tau_stjornarslit ~ exponential(1);         // Prior for collapse period scale
  phi_inv[2:H] ~ exponential(1);                   // Dispersion parameter
  phi_inv[1] ~ exponential(1);                   // Dispersion parameter for elections should be lower

  /* Polling Data Likelihood */
  for (n in 1:N) {
    vector[P] eta_n;
    // Linear predictor combining latent support and house effects
    eta_n[2:P] = beta[, date_n[n]] + gamma[ , house_n[n]];
    eta_n[1] = -sum(eta_n[2:P]);             // Sum-to-zero constraint
    vector[P] pi_n = softmax(eta_n);          // Convert to probabilities
    // Dirichlet-multinomial likelihood for poll counts
    y_n[n, 1:n_parties_n[n]] ~ dirichlet_multinomial(pi_n[1:n_parties_n[n]] * phi[house_n[n]]);
  }

  /* Fundamentals Priors and Likelihood */
  beta_lag_f ~ std_normal();                  // Previous vote share effect
  beta_inc_years_f ~ std_normal();            // Incumbency duration effect
  alpha_f_raw ~ std_normal();                 // Party-specific intercepts
  phi_f_inv ~ exponential(1);                 // Fundamentals dispersion

  // Likelihood for historical election results
  for (d in 1:D_f) { 
    vector[n_parties_f[d]] mu_d;
    // Linear predictor for fundamentals
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




