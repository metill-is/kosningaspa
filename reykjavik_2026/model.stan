// Forward random walk per cycle with hierarchical party-specific sigma.
// Fully symmetric across parties: sum_to_zero_vector used for cycle anchors,
// innovations, and house effects so that no party plays a special role.
// Identification is via the sum-to-zero constraint, applied at the parameter
// level by Stan's built-in constrained type (introduced in Stan 2.34).

data {
  int<lower = 2> P;                                     // number of parties (canonical, e.g. 10)
  int<lower = 1> H;                                     // number of houses incl. Kosning anchor at h=1
  int<lower = 1> C;                                     // number of election cycles
  int<lower = 1> T_total;                               // total unique (cycle, date) time points
  array[C] int<lower = 1> T_c;                          // per-cycle time-grid lengths; sum(T_c) == T_total
  vector<lower = 0>[T_total] time_diff_days;            // days between t-1 and t within a cycle; 0 at each cycle start

  int<lower = 1> N;                                     // observations (elections + polls)
  array[N, P] int<lower = 0> y;                         // [N, P] integer counts (votes or rounded poll shares)
  array[N] int<lower = 1, upper = H> house_idx;         // [N] which house, 1 = Kosning (election results)
  array[N] int<lower = 1, upper = T_total> time_idx;    // [N] index into the global time grid

  int<lower = 1, upper = T_total> forecast_time_idx;    // global index of the forecast date (election day)
  int<lower = 1> n_pred;                                // synthetic vote count for y_rep_election
}

transformed data {
  vector[T_total] time_scale = sqrt(time_diff_days);
  array[C] int cycle_start_idx;
  {
    int pos = 0;
    for (c in 1:C) {
      cycle_start_idx[c] = pos + 1;
      pos += T_c[c];
    }
  }
}

parameters {
  array[T_total] sum_to_zero_vector[P] z_beta_raw;      // sum-to-zero innovations per time point
  array[C] sum_to_zero_vector[P] z_beta0;               // sum-to-zero cycle-start state per cycle
  vector[P] z_log_sigma;                                // non-centred party-specific walk scale (all P parties)
  real mu_log_sigma;                                    // hyper-mean of log walk scale
  real<lower = 0> tau_log_sigma;                        // hyper-sd of log walk scale across parties

  array[H - 1] sum_to_zero_vector[P] gamma_z;           // sum-to-zero house effects per non-Kosning house
  vector<lower = 0>[P] sigma_gamma;                     // per-party sd of house effects
  vector[H - 1] log_phi;                                // log Dirichlet-Multinomial concentration per pollster
}

transformed parameters {
  // beta[, t] = sum-to-zero P-vector of latent log-odds at time t. No reference
  // party; identification is via the sum-to-zero constraint at every time index.
  // FORWARD walk per cycle: anchored at cycle start by z_beta0[c], walks forward
  // with Gaussian increments scaled by party-specific sigma_p and sqrt(days). For
  // the 2026 cycle the LAST time index is the forecast date, so pi_election =
  // softmax(beta) at that index.
  matrix[P, T_total] beta;
  matrix[P, H] gamma;
  vector<lower = 0>[P] sigma_p = exp(mu_log_sigma + tau_log_sigma * z_log_sigma);   // party-specific RW scale
  vector<lower = 0>[H - 1] phi = exp(log_phi);

  gamma[ , 1] = rep_vector(0, P);                       // Kosning anchor: no house bias by definition
  for (h in 2:H) {
    // sigma_gamma .* gamma_z destroys the sum-to-zero property because sigma_gamma
    // is per-party; recentre by subtracting the mean so gamma values can be read
    // as honest "logit bias relative to Kosning" rather than carrying an unknown
    // per-house offset that gets eaten by softmax (audit C2, agent A finding M1).
    vector[P] raw_eff = sigma_gamma .* gamma_z[h - 1];
    gamma[ , h] = raw_eff - mean(raw_eff);
  }

  for (c in 1:C) {
    int start_g = cycle_start_idx[c];
    beta[ , start_g] = z_beta0[c];
    for (t_fwd in 1:(T_c[c] - 1)) {
      int g = start_g + t_fwd;
      beta[ , g] = beta[ , g - 1] + time_scale[g] * (sigma_p .* z_beta_raw[g]);
    }
  }
}

model {
  // Sum-to-zero vectors with std_normal() give marginal variance (1 - 1/P) per
  // element. For P = 10 that's 0.9 -- close enough to unit variance that the
  // downstream sigma_p / sigma_gamma scaling reads as approximate sd.
  for (g in 1:T_total) z_beta_raw[g] ~ std_normal();
  for (c in 1:C)       z_beta0[c]    ~ std_normal();

  // Hierarchical party-specific walk scale on log-normal hierarchy:
  //   sigma_p[p] = exp(mu_log_sigma + tau_log_sigma * z_log_sigma[p])
  // mu_log_sigma centred on log(0.05) ~= -3.0.
  z_log_sigma     ~ std_normal();
  mu_log_sigma    ~ normal(-3.0, 0.5);
  tau_log_sigma   ~ exponential(2);                     // prior mean 0.5 in log space

  for (h in 1:(H - 1)) gamma_z[h] ~ std_normal();
  sigma_gamma     ~ exponential(20);                    // prior mean 0.05 logit (~1pp absolute bias); aggressive tightening — forces house effects toward zero unless data overwhelmingly demands a correction
  log_phi         ~ normal(4, 1);                       // exp(4) ~ 55 ESS-ish

  for (n in 1:N) {
    vector[P] eta = beta[ , time_idx[n]] + gamma[ , house_idx[n]];   // sum-to-zero + sum-to-zero = sum-to-zero
    if (house_idx[n] == 1) {
      y[n, ] ~ multinomial_logit(eta);
    } else {
      // Floor on alpha avoids the underflow class where softmax(eta) drops a tiny
      // entry below double precision and dirichlet_multinomial sees alpha == 0.
      vector[P] alpha = softmax(eta) * phi[house_idx[n] - 1] + 1e-9;
      y[n, ] ~ dirichlet_multinomial(alpha);
    }
  }
}

generated quantities {
  vector[P] pi_election = softmax(beta[ , forecast_time_idx]);
  array[P] int<lower = 0> y_rep_election = multinomial_rng(pi_election, n_pred);

  // Per-observation posterior predictive draws for PPC. Matches the likelihood
  // used during fitting: multinomial_logit for Kosning (h=1), Dirichlet-Multinomial
  // otherwise. Total count = sum(y[n,]) to mirror the observed sample size.
  array[N, P] int<lower = 0> y_rep_polls;
  for (n in 1:N) {
    vector[P] eta = beta[ , time_idx[n]] + gamma[ , house_idx[n]];
    int n_obs = sum(y[n, ]);
    if (house_idx[n] == 1) {
      y_rep_polls[n, ] = multinomial_logit_rng(eta, n_obs);
    } else {
      vector[P] alpha = softmax(eta) * phi[house_idx[n] - 1] + 1e-9;
      vector[P] theta = dirichlet_rng(alpha);
      y_rep_polls[n, ] = multinomial_rng(theta, n_obs);
    }
  }

  matrix[P, T_total] pi_trajectory;
  for (t in 1:T_total) {
    pi_trajectory[ , t] = softmax(beta[ , t]);
  }
}
