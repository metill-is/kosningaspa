data {
  /* Polling Data */
  // National Level Parameters
  int<lower = 1> D;                        // Number of time points
  int<lower = 1> P;                        // Number of parties
  int<lower = 1> H;                        // Number of polling houses
  int<lower = 1> N_n;                      // Number of national polls
  int<lower = 1> days_between_stjornarslit_and_election;  // Days between government collapse and election

  // National Level Data Arrays
  array[N_n, P] int<lower = 0> y_n;           // National polling data (counts per party)
  array[N_n] int<lower = 1, upper = H> house_n;  // House indicator for each national poll
  array[N_n] int<lower = 1, upper = D> date_n;   // Date indicator for each national poll
  array[N_n] int<lower = 1, upper = P> n_parties_n;     // Number of parties in each national poll
  array[N_n] int<lower = 1, upper = P> n_parties_n_rep; // Number of parties for replications

  // Time-related vectors
  vector[D] stjornarslit;                  // Indicator for government collapse period
  vector[D] post_stjornarslit;             // Indicator for post-collapse period
  vector[D] month_before_election;          // Indicator for month before election
  vector[D - 1] time_diff;                 // Time differences between consecutive polls
  
  // Prediction parameters
  int<lower = 1> pred_y_time_diff;         // How far into future to predict
  int<lower = 1> n_pred;                   // Number of predictions to generate

  // Constituency Level Parameters
  int<lower = 1> K;                        // Number of constituencies
  int<lower = 1> N_k;                      // Number of constituency polls
  int<lower = 1> N_obs_k;                  // Number of unique constituency observations
  
  // Constituency Level Data Arrays
  array[N_k, P] int<lower = 0> y_k;        // Constituency polling data
  array[N_k] int<lower = 1, upper = H> house_k;  // House indicator for constituency polls
  array[N_k] int<lower = 1, upper = D> date_k;   // Date indicator for constituency polls
  array[N_k] int<lower = 1, upper = P> n_parties_k;     // Number of parties in constituency polls
  array[N_k] int<lower = 1, upper = P> n_parties_k_rep; // Number of parties for replications
  array[N_k] int<lower = 1, upper = K> constituency_k;  // Constituency indicator
  array[N_k] int<lower = 1, upper = N_obs_k> obs_k;     // Observation indicator
  array[K] int<lower = 1> n_pred_k;                     // Predictions per constituency
  
  // Constituency weights matrices
  matrix[N_obs_k, K] constituency_weights;      // Historical constituency weights
  vector[K] constituency_weights_pred;          // Prediction weights

  /* Fundamentals Data */
  int<lower = 1> D_f;                      // Number of past elections
  int<lower = 1> P_f;                      // Number of parties in fundamentals
  array[P_f, D_f] int<lower = 0> y_f;      // Historical election results
  
  // Predictor matrices for fundamentals
  matrix[P_f, D_f + 1] x_f;                // Previous vote shares
  matrix[P_f, D_f + 1] incumbent_f;        // Incumbent status
  matrix[P_f, D_f + 1] incumbent_years;    // Years as incumbent
  matrix[P_f, D_f + 1] vnv;                // Vote/No Vote status
  matrix[P_f, D_f + 1] growth;             // Economic growth
  
  // Fundamentals indexing
  int<lower = 1> max_n_parties_f;          // Maximum parties in any election
  array[max_n_parties_f, D_f + 1] int<lower = 0> index_f;  // Party index mapping
  array[D_f + 1] int<lower = 0, upper = P_f> n_parties_f;  // Parties per election

  // Model weighting parameters
  real<lower = 0, upper = 1> desired_weight;  // Desired weight for fundamentals
  int<lower = 0> weight_time;                 // Time point for desired weight
}

transformed data {
  // Scale time differences to moderate impact of longer gaps
  vector[D - 1] time_scale = sqrt(time_diff);
}

parameters {
  /* Polling Parameters */
  // National level parameters
  matrix[P - 1, D + pred_y_time_diff] z_beta_raw;  // Raw random walk innovations
  cholesky_factor_corr[P - 1] L_Omega;             // Cholesky factor of correlation matrix
  vector[P - 1] beta0;                             // Initial support levels
  vector<lower = 0>[P - 1] sigma;                  // Random walk scales
  real<lower = 0> tau_stjornarslit;                // Scale for government collapse period

  // Constituency level parameters
  matrix[P - 1, K - 1] delta_raw;                  // Raw constituency effects
  vector<lower = 0>[P - 1] sigma_delta;            // Constituency effect scales

  // House effects parameters
  matrix[P - 1, H - 1] gamma_raw;                  // Raw house effects
  vector[P - 1] mu_gamma;                          // Mean house effects
  vector<lower = 0>[P - 1] sigma_gamma;            // House effect scales
  vector<lower = 0>[H] phi_inv;                    // Inverse dispersion parameters

  /* Fundamentals Parameters */
  vector[P_f - 1] alpha_f_raw;                     // Party-specific intercepts
  real beta_lag_f;                                 // Effect of previous vote share
  real beta_inc_years_f;                           // Effect of incumbent years
  real beta_vnv_f;                                 // Effect of vote/no vote status
  real beta_growth_f;                              // Effect of economic growth
  real<lower = 0> phi_f_inv;                       // Inverse dispersion for fundamentals
}

transformed parameters {
  // Convert inverse dispersions to dispersions
  vector<lower = 0>[H] phi = pow(phi_inv, -1);
  real<lower = 0> phi_f = pow(phi_f_inv, -1);
  
  // Transform raw parameters into model parameters
  matrix[P - 1, D + pred_y_time_diff] z_beta = L_Omega * z_beta_raw;
  matrix[P - 1, H] gamma;                     
  matrix[P - 1, D + pred_y_time_diff] beta;   
  vector[P_f] alpha_f;
  
  // Sum-to-zero constraint for fundamentals intercepts
  alpha_f[2:P_f] = alpha_f_raw;
  alpha_f[1] = -sum(alpha_f[2:P_f]);
  
  // Calculate variance parameters for combining models
  real<lower = 0> V_t;
  real<lower = 0> tau_f;
  
  // Calculate total variance based on time period
  if (weight_time <= 47) {
    V_t = weight_time * square(1 + tau_stjornarslit);
  } else {
    V_t = (weight_time - 47) + 47 * square(1 + tau_stjornarslit);
  }
  
  // Calculate fundamentals weight parameter
  tau_f = sqrt(V_t * (1 - desired_weight) / desired_weight);
  
  // Set up house effects
  for (p in 1:(P - 1)) {
    gamma[p, 1] = 0;                      // Reference house effect set to 0
    gamma[p, 2:H] = mu_gamma[p] + sigma_gamma[p] * gamma_raw[p, ];    
  }
  
  // Initialize end state and evolve support backwards
  beta[ , D + pred_y_time_diff] = beta0;

  // Forward prediction period
  for (t in 1:pred_y_time_diff) {
    real scale = (t <= days_between_stjornarslit_and_election) ? (1 + tau_stjornarslit) : 1;
    beta[ , D + pred_y_time_diff - t] = beta[ , D + pred_y_time_diff - t + 1] + 
      z_beta[, D + pred_y_time_diff - t + 1] .* sigma * scale;
  }

  // Historical period evolution
  for (t in 1:(D - 1)) {
    beta[ , D - t] = beta[ , D - t + 1] + 
      time_scale[D - t] * z_beta[, D - t + 1] .* sigma * 
      (1 + tau_stjornarslit * month_before_election[D - t]);
  }
}

model {
  // Calculate fundamentals predictions
  vector[P - 1] mu_pred = alpha_f[index_f[2:n_parties_f[D_f + 1], D_f + 1]] + 
    beta_lag_f * x_f[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] + 
    beta_inc_years_f * incumbent_years[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_vnv_f * vnv[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_growth_f * growth[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1];

  /* Prior Distributions */
  // House effects priors
  to_vector(gamma_raw) ~ std_normal();
  mu_gamma ~ std_normal();
  sum(mu_gamma) ~ std_normal();
  sigma_gamma ~ exponential(1);
  phi_inv ~ exponential(1);
  for (h in 2:(H - 1)) {
    sum(gamma_raw[ , h]) ~ std_normal();
  }

  // Support evolution priors
  to_vector(z_beta_raw) ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(2);
  sigma ~ exponential(1);                 
  beta0 ~ normal(mu_pred, tau_f * sigma);
  tau_stjornarslit ~ normal(0, 0.3);

  // Constituency effects priors
  to_vector(delta_raw) ~ std_normal();
  sigma_delta ~ exponential(1);

  /* Likelihood Functions */
  // National polling likelihood
  for (n in 1:N_n) {
    vector[n_parties_n[n]] eta_n;
    eta_n[2:n_parties_n[n]] = beta[1:(n_parties_n[n] - 1), date_n[n]] + 
      gamma[1:(n_parties_n[n] - 1), house_n[n]];
    eta_n[1] = -sum(eta_n[2:n_parties_n[n]]);
    vector[n_parties_n[n]] pi_n = softmax(eta_n);
    y_n[n, 1:n_parties_n[n]] ~ dirichlet_multinomial(pi_n[1:n_parties_n[n]] * phi[house_n[n]]);
  }

  // Constituency polling likelihood
  for (n in 1:N_k) {
    matrix[P - 1, K] delta;
    vector[n_parties_k[n]] eta_k;

    // Calculate constituency effects with sum-to-zero constraint
    for (p in 1:(P - 1)) {
      delta[p, 1:(K - 1)] = sigma_delta[p] * delta_raw[p, ];
      delta[p, K] = -sum(delta[p, 1:(K - 1)] .* constituency_weights[obs_k[n], 1:(K - 1)]) / 
            constituency_weights[obs_k[n] , K];
    }
    
    // Combine national trends, house effects, and constituency effects
    eta_k[2:n_parties_k[n]] = beta[1:(n_parties_k[n] - 1), date_k[n]] + 
      gamma[1:(n_parties_k[n] - 1), house_k[n]] + 
      delta[1:(n_parties_k[n] - 1), constituency_k[n]];
    eta_k[1] = -sum(eta_k[2:n_parties_k[n]]);
    
    vector[n_parties_k[n]] pi_k = softmax(eta_k);
    y_k[n, 1:n_parties_k[n]] ~ dirichlet_multinomial(pi_k * phi[house_k[n]]);
  }

  /* Fundamentals Likelihood */
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
  // Calculate correlation matrix from Cholesky factor
  corr_matrix[P - 1] Omega = L_Omega * L_Omega';
  
  // Arrays for predictions and replications
  array[D + pred_y_time_diff, P] int<lower = 0> y_rep_national;  // National predictions
  array[N_k, P] int<lower = 0> y_rep_k;                         // Constituency replications
  array[K, P] int<lower = 0> y_pred_constituency;               // Constituency predictions
  matrix[P - 1, K] delta_pred;                                  // Predicted constituency effects

  // Initialize arrays to 0
  for (d in 1:(D + pred_y_time_diff)) {
    for (p in 1:P) {
      y_rep_national[d, p] = 0;
    }
  }
  
  for (n in 1:N_k) {
    for (p in 1:P) {
      y_rep_k[n, p] = 0;
    }
  }
  
  for (k in 1:K) {
    for (p in 1:P) {
      y_pred_constituency[k, p] = 0;
    }
  }

  // Calculate constituency effects for predictions
  for (p in 1:(P - 1)) {
    delta_pred[p, 1:(K - 1)] = sigma_delta[p] * delta_raw[p, ];
    delta_pred[p, K] = -sum(to_vector(delta_pred[p, 1:(K - 1)]) .* constituency_weights_pred[1:(K - 1)]) / 
          constituency_weights_pred[K];
  }

  // Generate national-level predictions
  for (d in 1:D) {
    vector[n_parties_n_rep[d]] eta_d;
    vector[n_parties_n_rep[d]] pi_d;
    
    eta_d[2:n_parties_n_rep[d]] = beta[1:(n_parties_n_rep[d] - 1), d];
    eta_d[1] = -sum(eta_d[2:n_parties_n_rep[d]]);
    pi_d = softmax(eta_d);
    y_rep_national[d, 1:n_parties_n_rep[d]] = 
      dirichlet_multinomial_rng(pi_d * phi[1], n_pred);
  }

  // Generate future predictions
  for (d in 1:pred_y_time_diff) {
    vector[P] eta_d;
    vector[P] pi_d;
    eta_d[2:P] = beta[, D + d];
    eta_d[1] = -sum(eta_d[2:P]);
    pi_d = softmax(eta_d);
    y_rep_national[D + d, ] = dirichlet_multinomial_rng(pi_d * phi[1], n_pred);
  }

  // Generate constituency-level predictions
  for (k in 1:K) {
    vector[P] eta_k;
    vector[P] pi_k;
    
    eta_k[2:P] = beta[, D + pred_y_time_diff] + delta_pred[, k];
    eta_k[1] = -sum(eta_k[2:P]);
    pi_k = softmax(eta_k);
    y_pred_constituency[k, ] = dirichlet_multinomial_rng(pi_k * phi[1], n_pred_k[k]);
  }

  // Generate constituency-level replications
  for (n in 1:N_k) {
    matrix[P - 1, K] delta;
    vector[n_parties_k_rep[n]] eta_k;
    vector[n_parties_k_rep[n]] pi_k;

    for (p in 1:(P - 1)) {
      delta[p, 1:(K - 1)] = sigma_delta[p] * delta_raw[p, ];
      delta[p, K] = -sum(delta[p, 1:(K - 1)] .* constituency_weights[obs_k[n], 1:(K - 1)]) / 
            constituency_weights[obs_k[n] , K];
    }

    eta_k[2:n_parties_k_rep[n]] = beta[1:(n_parties_k_rep[n] - 1), date_k[n]] + 
        gamma[1:(n_parties_k_rep[n] - 1), house_k[n]] + 
        delta[1:(n_parties_k_rep[n] - 1), constituency_k[n]];

    eta_k[1] = -sum(eta_k[2:n_parties_k_rep[n]]);
    pi_k = softmax(eta_k);
    y_rep_k[n, 1:n_parties_k_rep[n]] = 
      dirichlet_multinomial_rng(pi_k * phi[house_k[n]], sum(y_k[n, ]));
  }
}