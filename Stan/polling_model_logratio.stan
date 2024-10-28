functions {
  // Additive log-ratio transformation
  vector alr(vector pi) {
    int K = num_elements(pi);
    vector[K-1] result;
    
    for (k in 1:(K-1)) {
      result[k] = log(pi[k]) - log(pi[K]);
    }
    return result;
  }
  
  // Inverse additive log-ratio transformation
  vector inverse_alr(vector x) {
    int K = num_elements(x) + 1;
    vector[K] log_pi;
    vector[K] pi;
    
    // First compute log probabilities
    for (k in 1:(K-1)) {
      log_pi[k] = x[k];
    }
    log_pi[K] = 0;  // log(1) for baseline
    
    // Use log_sum_exp for numerical stability
    real log_sum = log_sum_exp(log_pi);
    
    // Transform back and normalize
    for (k in 1:K) {
      pi[k] = exp(log_pi[k] - log_sum);
    }
    
    return pi;
  }
}

data {
  int<lower = 1> D;                        // Number of time points
  int<lower = 2> P;                        // Number of parties
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
  vector[P - 1] beta_0;                           // Party-specific initial effect (ALR scale)
  matrix[P - 1, D + pred_y_time_diff] z_beta;     // Standardized random walk innovations
  matrix[P - 1, H - 1] gamma_raw;                 // House effects (ALR scale)
  vector[P - 1] mu_gamma;
  vector<lower = 0>[P - 1] sigma_gamma;
  vector<lower = 0>[P - 1] sigma;                 // Party-specific random walk scale
  real<lower = 0> phi_inv;
}

transformed parameters {
  real<lower = 0> phi = pow(phi_inv, -1);
  matrix[P-1, H] gamma;                     
  matrix[P-1, D + pred_y_time_diff] beta;   // On ALR scale
  matrix[P, D + pred_y_time_diff] pi;       // On probability scale
  
  // House effects (on ALR scale)
  for (p in 1:(P-1)) {
    gamma[p, 1] = 0;                      // Fix first house effect to zero
    gamma[p, 2:H] = mu_gamma[p] + sigma_gamma[p] * gamma_raw[p, ];         
  }

  // True party support over time (on ALR scale)
  for (p in 1:(P-1)) {
    beta[p, 1] = beta_0[p];
    for (t in 2:D) {
      beta[p, t] = beta[p, t - 1] + z_beta[p, t - 1] * sigma[p] * time_scale[t - 1];
    }

    // Daily predictions up until the last date
    for (t in 1:pred_y_time_diff) {
      beta[p, D + t] = beta[p, D + t - 1] + z_beta[p, D + t - 1] * sigma[p];
    }
  }
  
  // Transform to probability scale for likelihood
  for (d in 1:(D + pred_y_time_diff)) {
    pi[,d] = inverse_alr(beta[,d]);
  }
}

model {
  // Priors for house effects
  to_vector(gamma_raw) ~ std_normal();
  mu_gamma ~ normal(0, 0.1);
  sum(mu_gamma) ~ normal(0, 0.1);
  sigma_gamma ~ exponential(1);
  for (h in 2:(H - 1)) {
    sum(gamma_raw[,h]) ~ normal(0, 0.1);
  }

  // Priors for true party support
  beta_0 ~ std_normal();
  to_vector(z_beta) ~ std_normal();
  sigma ~ exponential(1);                 

  // Prior for the Dirichlet-multinomial scale parameter
  phi_inv ~ exponential(1);

  // Likelihood
  for (n in 1:N) {
    vector[P-1] eta_n = beta[,date[n]] + gamma[,house[n]];  // Linear predictor on ALR scale
    vector[P] pi_n = inverse_alr(eta_n);                    // Transform to probability scale
    y[n,] ~ dirichlet_multinomial(pi_n * phi);
  }
}

generated quantities {
  array[D + pred_y_time_diff, P] int<lower = 0> y_rep;
  vector[P-1] industry_bias;
  
  for (d in 1:(D + pred_y_time_diff)) {
    y_rep[d,] = dirichlet_multinomial_rng(pi[,d] * phi, n_pred);
  }

  for (p in 1:(P-1)) {
    industry_bias[p] = mean(gamma_raw[p,]);
  }
}
