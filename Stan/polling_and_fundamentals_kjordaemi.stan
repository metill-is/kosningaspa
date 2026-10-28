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
  array[D] int<lower = 1, upper = P> n_active_d;        // Parties active at each date level [D]
  array[D, P] int<lower = 0, upper = P> active_index_d; // Their columns in party order, 0-padded [D, P]
  array[N_n, P] int<lower = 0, upper = P> party_index_n; // Column ids of reported parties per national poll (0-padded tail)

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
  array[N_k, P] int<lower = 0, upper = P> party_index_k; // Column ids of reported parties per constituency poll (0-padded tail)
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
  matrix[P_f, D_f + 1] vnv;                // log(1 + CPI inflation), incumbents only
  matrix[P_f, D_f + 1] growth;             // log(1 + GDP growth), incumbents only
  
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
  vector[N_n] n_sample_n;
  vector[N_k] n_sample_k;

  for (n in 1:N_n) {
    n_sample_n[n] = sum(y_n[n, ]);
  }

  for (n in 1:N_k) {
    n_sample_k[n] = sum(y_k[n, ]);
  }

  // Anchored walk. Dates with a national election result (house 1) are anchors: their
  // levels are centred parameters, since the result pins them and the backward non-centred
  // walk turned each pin into a dense stiff direction. Between anchors (and from the last
  // anchor to the end date) the path is a non-centred Brownian bridge; before the first
  // anchor it is a non-centred backward walk. The joint prior over the path is unchanged.
  int T = D + pred_y_time_diff;
  array[D] int is_anchor = rep_array(0, D);
  array[D, P - 1] int reported = rep_array(0, D, P - 1);
  for (n in 1:N_n) {
    if (house_n[n] == 1) {
      is_anchor[date_n[n]] = 1;
      for (j in 1:n_parties_n[n]) {
        if (party_index_n[n, j] > 1) reported[date_n[n], party_index_n[n, j] - 1] = 1;
      }
    }
  }
  int m = sum(is_anchor);
  array[m] int anchor;
  // Per anchor: parties the result reports (present) and the rest (absent). Present
  // coordinates are stored shifted so that the result's likelihood does not depend on the
  // absent ones: beta_present = w_present - sum(beta_absent) / (n_present + 1). The shift
  // is unit triangular, so no Jacobian term.
  array[m] int n_pres;
  array[m, P - 1] int pres_idx = rep_array(0, m, P - 1);
  array[m] int n_abs;
  array[m, P - 1] int abs_idx = rep_array(0, m, P - 1);
  {
    int j = 0;
    for (t in 1:D) {
      if (is_anchor[t] == 1) {
        j += 1;
        anchor[j] = t;
        n_pres[j] = 0;
        n_abs[j] = 0;
        for (p in 1:(P - 1)) {
          if (reported[t, p] == 1) {
            n_pres[j] += 1;
            pres_idx[j, n_pres[j]] = p;
          } else {
            n_abs[j] += 1;
            abs_idx[j, n_abs[j]] = p;
          }
        }
      }
    }
  }
  // Non-centred innovation column for every date in 1..T-1 that is not an anchor
  int nz = T - 1 - m;
  array[T] int zcol = rep_array(0, T);
  {
    int c = 0;
    for (t in 1:(T - 1)) {
      if (t > D || is_anchor[t] == 0) {
        c += 1;
        zcol[t] = c;
      }
    }
  }

  // Each anchor's parameter is stored relative to the levels that reproduce its election
  // result exactly (w_off), so the default init of 0 starts every election at its result
  // rather than at equal shares, whose gradients against a result of ~200,000 votes wreck
  // early step-size adaptation. A constant shift: exact, no Jacobian. The result's logits
  // against party 1 are w_i + sum(w) in the stored present-party levels w, so
  // w_i = r_i - sum(r) / (n_present + 1) reproduces r_i = log(y_i / y_1). Absent parties'
  // coordinates are non-centred and need no offset.
  array[m] vector[P - 1] w_off = rep_array(rep_vector(0, P - 1), m);
  for (n in 1:N_n) {
    if (house_n[n] == 1) {
      int j = 0;
      for (i in 1:m) {
        if (anchor[i] == date_n[n]) j = i;
      }
      real r_sum = 0;
      int n_r = 0;
      for (k in 1:n_parties_n[n]) {
        int c = party_index_n[n, k];
        if (c > 1) {
          w_off[j][c - 1] = log((y_n[n, c] + 0.5) / (y_n[n, 1] + 0.5));
          r_sum += w_off[j][c - 1];
          n_r += 1;
        }
      }
      for (k in 1:n_parties_n[n]) {
        int c = party_index_n[n, k];
        if (c > 1) w_off[j][c - 1] -= r_sum / (n_r + 1);
      }
    }
  }

  // Overdispersion bound: log phi_h <= phi_cap. Beyond it the Dirichlet-multinomial is a
  // multinomial for these polls, and Stan's gradient of it (CmdStan 2.38) carries an error
  // that grows with phi, since its digamma differences cancel: negligible at log phi 20,
  // O(1) and more from 30, enough to stall a chain that warmup throws there. The bound
  // conditions the posterior on log phi_h < phi_cap. The houses' posteriors centre on
  // log phi 6-8; one with a single national poll reached 17.8 in a 2021 fit without the
  // bound, and its estimated mass above phi_cap is of order 1e-5. The bound's
  // unconstrained value is shifted so that init 0 is phi_init, the polls' geometric-mean
  // size.
  real phi_cap = 20;
  real phi_init = 0;
  {
    int n_polls = 0;
    for (n in 1:N_n) {
      if (house_n[n] > 1) {
        phi_init += log(n_sample_n[n]);
        n_polls += 1;
      }
    }
    phi_init /= n_polls;
  }

  // Partially centred hierarchical effects (Gorinova et al. 2020, sec. 5.2). An effect
  // theta ~ N(mu, s) is sampled as theta_t ~ N(lambda mu, s^lambda) and mapped to
  // theta = mu + s^(1 - lambda) (theta_t - lambda mu): lambda = 1 is centred, 0 non-centred,
  // and the map is exact for any lambda in [0, 1], one that depends on parameters included.
  // The weight is lambda = s^2 I / (1 + s^2 I), the prior's share of the effect's variance
  // given data that carry information I about it. I is a data-only proxy: each poll that
  // reports the party adds the binomial information 0.5 n pi (1 - pi) at its own share pi
  // and half its size (a Dirichlet-multinomial with phi near n about halves it). A cell no
  // poll informs gets I = 0, so lambda = 0 (non-centred) and no funnel. House effects stay
  // non-centred: the walk and mu_gamma absorb most of what a poll says about one, so the
  // proxy would overstate it and centre cells whose scale sits near 0.
  // Information proxy for each party's log volatility: 0.5 per date with a national poll
  // that reports the party (an exactly observed walk step carries information 2 about it;
  // poll noise about the size of a week's step leaves roughly a quarter).
  vector[P - 1] info_sigma = rep_vector(0, P - 1);
  {
    array[D, P - 1] int seen = rep_array(0, D, P - 1);
    for (n in 1:N_n) {
      for (j in 1:n_parties_n[n]) {
        int c = party_index_n[n, j];
        if (c > 1 && seen[date_n[n], c - 1] == 0) {
          seen[date_n[n], c - 1] = 1;
          info_sigma[c - 1] += 0.5;
        }
      }
    }
  }
  matrix[P - 1, K - 1] info_delta = rep_matrix(0, P - 1, K - 1);
  for (n in 1:N_k) {
    for (j in 1:n_parties_k[n]) {
      int c = party_index_k[n, j];
      if (c > 1) {
        real s = y_k[n, c] / n_sample_k[n];
        real info = 0.5 * n_sample_k[n] * s * (1 - s);
        if (constituency_k[n] < K) {
          info_delta[c - 1, constituency_k[n]] += info;
        } else {
          // The K-th effect is -sum(w_k delta_k) / w_K, so its rows inform every free effect
          for (k in 1:(K - 1)) {
            info_delta[c - 1, k] += square(constituency_weights[obs_k[n], k] / constituency_weights[obs_k[n], K]) * info;
          }
        }
      }
    }
  }
}

parameters {
  /* Polling Parameters */
  // National level parameters
  array[m] vector[P - 1] anchor_w;                  // Election-date levels (present parties shifted)
  matrix[P - 1, nz] z_walk;                         // Bridge and pre-anchor innovations
  cholesky_factor_corr[P - 1] L_Omega;             // Cholesky factor of correlation matrix
  vector[P - 1] z_beta0;                             // Initial support levels
  
  // Constituency level parameters
  // Parameterisation (Stan/CLAUDE.md): the walk is anchored at election dates (anchor_w:
  // centred, stored relative to each election's result; a party absent from an election
  // non-centred) with non-centred Brownian bridges between them (z_walk). The
  // constituency effects and each party's log volatility are partially centred cell by
  // cell (delta_t, log_sigma_t; transformed data explains the weights); the house effects
  // stay non-centred (gamma_raw) and log_phi is centred. Together they are an
  // exact reparameterisation of the earlier backward non-centred walk.
  // One change of model: log_phi is bounded above by phi_cap (transformed data explains
  // why).
  matrix[P - 1, K - 1] delta_t;                    // Partially centred constituency effects
  vector<lower = 0>[P - 1] sigma_delta;             // Constituency effect scales
  
  // House Effects Parameters
  matrix[P - 2, H - 1] gamma_raw;                   // Raw house effects (non-centred)
  vector[P - 2] mu_gamma;                           // Mean house effects
  vector<lower = 0>[P - 2] sigma_gamma;             // Scale of house effects
  
  
  // Variance Parameters
  real mu_log_sigma;                    // Population mean of log volatility
  real<lower=0> tau_log_sigma;          // Population SD of log volatility
  vector[P - 1] log_sigma_t;              // Partially centred log volatilities
  real<lower = 0> tau_stjornarslit;                // Scale for government collapse period

  // vector<lower = 0>[H - 1] phi;
  vector[H - 1] log_phi_u;                // Log overdispersion per house (bounded, shifted)
  real mu_phi;
  real<lower = 0> sigma_phi;

  /* Fundamentals Parameters */
  vector[P_f - 1] alpha_f_raw;                     // Party-specific intercepts
  real beta_lag_f;                                 // Effect of previous vote share
  real beta_inc_years_f;                           // Effect of incumbent years
  real beta_vnv_f;                                 // Effect of CPI inflation (incumbents)
  real beta_growth_f;                              // Effect of GDP growth (incumbents)
  real<lower = 0> phi_f_inv;                       // Inverse dispersion for fundamentals
}

transformed parameters {
  vector[P - 1] lambda_sigma = 1 - inv(1 + square(tau_log_sigma) * info_sigma);  // Partial-centring weights
  vector[P - 1] log_sigma = mu_log_sigma
    + exp((1 - lambda_sigma) * log(tau_log_sigma)) .* (log_sigma_t - lambda_sigma * mu_log_sigma);
  vector<lower=0>[P - 1] sigma = exp(log_sigma);           // Party-specific volatilities
  // Convert inverse dispersion parameters to dispersion parameters
  
  // Centred log overdispersion per house, bounded above by phi_cap: Stan's upper-bound
  // transform with its unconstrained value shifted by log(phi_cap - phi_init)
  vector[H - 1] log_phi = phi_cap - (phi_cap - phi_init) * exp(log_phi_u);
  vector<lower = 0>[H - 1] phi = exp(log_phi);
  matrix[P - 1, K - 1] delta_free;
  matrix[P - 1, K - 1] lambda_delta;                // Partial-centring weights (info_delta is data)
  for (p in 1:(P - 1)) {
    lambda_delta[p] = 1 - inv(1 + square(sigma_delta[p]) * info_delta[p]);
    delta_free[p] = exp((1 - lambda_delta[p]) * log(sigma_delta[p])) .* delta_t[p];
  }

  real<lower = 0> phi_f = pow(phi_f_inv, -1);
  
  // Transform raw parameters into model parameters
  matrix[P - 1, P - 1] L_walk = diag_pre_multiply(sigma, L_Omega);
  matrix[P - 1, H] gamma;                     
  matrix[P - 1, D + pred_y_time_diff] beta;   
  vector[P_f] alpha_f;
  
  // Reference-zero intercept for party 1 (never used: its mu is minus the sum of the others)
  alpha_f[2:P_f] = alpha_f_raw;
  alpha_f[1] = 0;
  
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
  for (p in 1:(P - 2)) {
    gamma[p, 1] = 0;                          // Reference house effect set to 0
    gamma[p, 2:H] = mu_gamma[p] + sigma_gamma[p] * gamma_raw[p, ];  // Other house effects
  }
  
  gamma[P - 1, 1] = 0;
  for (h in 2:H) {
    gamma[P - 1, h] = -sum(gamma[1:(P - 2), h]);
  }
  // Calculate fundamentals predictions (same linear predictor as the fundamentals likelihood below)
  vector[P - 1] mu_pred = alpha_f[index_f[2:n_parties_f[D_f + 1], D_f + 1]] + 
    beta_lag_f * x_f[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] + 
    beta_inc_years_f * incumbent_years[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_vnv_f * vnv[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1] +
    beta_growth_f * growth[index_f[2:n_parties_f[D_f + 1], D_f + 1], D_f + 1];

    vector[P - 1] beta0 = mu_pred + tau_f * z_beta0 .* sigma;
  
  // Variance multiplier v[t] of the step between dates t - 1 and t, as the backward walk
  // scales it (z_beta[, t] * time_scale * (1 + tau * mbe), or * (1 + tau) in the last
  // days_between_stjornarslit_and_election future days); cv is its running total.
  vector[T] v;
  v[1] = 0;
  for (t in 2:D) {
    v[t] = time_diff[t - 1] * square(1 + tau_stjornarslit * month_before_election[t - 1]);
  }
  for (t in (D + 1):T) {
    v[t] = T - t + 1 <= days_between_stjornarslit_and_election ? square(1 + tau_stjornarslit) : 1;
  }
  vector[T] cv = cumulative_sum(v);
  beta[, T] = beta0;
  // Anchors from the last back, since each one's prior is centred on the next. A party the
  // result does not report has no data at the anchor: its level is non-centred,
  // beta_abs = beta_next + sqrt(dcv) L_aa u (u = anchor_w[j][ab]), L_aa the Cholesky factor of
  // its block of the step covariance.
  for (i in 1:m) {
    int j = m + 1 - i;
    int b = j < m ? anchor[j + 1] : T;
    if (n_abs[j] > 0) {
      array[n_abs[j]] int ab = abs_idx[j, 1:n_abs[j]];
      array[n_pres[j]] int pr = pres_idx[j, 1:n_pres[j]];
      matrix[n_abs[j], n_abs[j]] L_aa = cholesky_decompose(multiply_lower_tri_self_transpose(L_walk)[ab, ab]);
      beta[ab, anchor[j]] = beta[ab, b] + sqrt(cv[b] - cv[anchor[j]]) * L_aa * anchor_w[j][ab];
      beta[pr, anchor[j]] = anchor_w[j][pr] + w_off[j][pr] - sum(beta[ab, anchor[j]]) / (n_pres[j] + 1);
    } else {
      beta[, anchor[j]] = anchor_w[j] + w_off[j];
    }
  }
  {
    matrix[P - 1, nz] Lz = L_walk * z_walk;
    // Bridges, left to right within each gap: the exact conditional of the walk given both ends
    for (j in 1:m) {
      int a = anchor[j];
      int b = j < m ? anchor[j + 1] : T;
      for (t in (a + 1):(b - 1)) {
        real R = cv[b] - cv[t - 1];
        beta[, t] = beta[, t - 1] + (v[t] / R) * (beta[, b] - beta[, t - 1])
          + sqrt(v[t] * (cv[b] - cv[t]) / R) * Lz[, zcol[t]];
      }
    }
    // Before the first anchor (or everywhere, with no anchor): backward non-centred walk
    int a1 = m > 0 ? anchor[1] : T;
    for (i in 1:(a1 - 1)) {
      int t = a1 - i;
      beta[, t] = beta[, t + 1] + sqrt(v[t + 1]) * Lz[, zcol[t]];
    }
  }
}

model {
  /* Prior Distributions */
  // House effects priors
  to_vector(gamma_raw) ~ std_normal();
  mu_gamma ~ std_normal();
  sigma_gamma ~ exponential(1);
  
  
  // Support evolution priors
  to_vector(z_walk) ~ std_normal();
  // Anchor priors: beta_a ~ N(beta_next, dcv L L'), absent parties non-centred and present
  // ones given them, from the Cholesky factor with the absent parties first
  for (j in 1:m) {
    int b = j < m ? anchor[j + 1] : T;
    real sc = sqrt(cv[b] - cv[anchor[j]]);
    if (n_abs[j] > 0) {
      int na = n_abs[j];
      array[na] int ab = abs_idx[j, 1:na];
      array[n_pres[j]] int pr = pres_idx[j, 1:n_pres[j]];
      array[P - 1] int perm = append_array(ab, pr);
      matrix[P - 1, P - 1] L_perm = cholesky_decompose(multiply_lower_tri_self_transpose(L_walk)[perm, perm]);
      anchor_w[j][ab] ~ std_normal();
      beta[pr, anchor[j]] ~ multi_normal_cholesky(beta[pr, b] + sc * L_perm[(na + 1):(P - 1), 1:na] * anchor_w[j][ab],
                                                   sc * L_perm[(na + 1):(P - 1), (na + 1):(P - 1)]);
    } else {
      beta[, anchor[j]] ~ multi_normal_cholesky(beta[, b], sc * L_walk);
    }
  }
  L_Omega ~ lkj_corr_cholesky(10);
  mu_log_sigma ~ normal(-2, 2);         // Prior centered on small volatility
  tau_log_sigma ~ exponential(1);       // Weakly informative prior on variance
  log_sigma_t ~ normal(lambda_sigma * mu_log_sigma, exp(lambda_sigma * log(tau_log_sigma)));
  z_beta0 ~ std_normal();
  tau_stjornarslit ~ exponential(1);

  log_phi ~ normal(mu_phi, sigma_phi);
  target += sum(log_phi_u);                     // Jacobian of the bound (constant dropped)
  mu_phi ~ normal(0, 1);
  sigma_phi ~ normal(0, 1);
  // phi ~ exponential(1);

  // Constituency effects priors
  for (p in 1:(P - 1)) {
    delta_t[p] ~ normal(0, exp(lambda_delta[p] * log(sigma_delta[p])));
  }
  sigma_delta ~ exponential(1);

  /* Likelihood Functions */
  // National polling likelihood
  for (n in 1:N_n) {
    vector[P] eta_n;
    eta_n[2:P] = beta[, date_n[n]] + gamma[, house_n[n]];
    eta_n[1] = -sum(eta_n[2:P]);
    // Gather the reported parties by identity (party_index_n), not by the first
    // n_parties_n columns. eta_n is the full sum-to-zero P-vector (position 1 =
    // reference), so gathering after reconstruction handles an interior drop —
    // including a dropped reference party — correctly.
    int k = n_parties_n[n];
    array[k] int cols = party_index_n[n, 1:k];
    vector[k] eta_obs = eta_n[cols];
    array[k] int y_obs = y_n[n, cols];
    if (house_n[n] > 1) {
      // Dirichlet-multinomial likelihood for poll counts
      vector[k] pi_n = softmax(eta_obs);                                       // Convert to probabilities
      y_obs ~ dirichlet_multinomial(pi_n * phi[house_n[n] - 1]);
    } else {
      y_obs ~ multinomial_logit(eta_obs);
    }
  }

  // Constituency polling likelihood
  // The K-th constituency's effect depends only on the observation's weights: build it once
  // per observation (N_obs_k) rather than once per row (N_k).
  matrix[P - 1, N_obs_k] delta_K = -(delta_free * constituency_weights[, 1:(K - 1)]')
    ./ rep_matrix(constituency_weights[, K]', P - 1);
  for (n in 1:N_k) {
    vector[P] eta_k;
    eta_k[2:P] = beta[1:(P - 1), date_k[n]] + gamma[1:(P - 1), house_k[n]]
      + (constituency_k[n] < K ? delta_free[, constituency_k[n]] : delta_K[, obs_k[n]]);
    eta_k[1] = -sum(eta_k[2:P]);

    // Gather reported parties by identity (party_index_k). Constituency polls are
    // the ones that actually drop an interior party (small parties / a dropped
    // reference in small kjordaemi), so this branch is where the positional slice
    // corrupted the likelihood.
    int k = n_parties_k[n];
    array[k] int cols = party_index_k[n, 1:k];
    vector[k] eta_obs = eta_k[cols];
    array[k] int y_obs = y_k[n, cols];
    if (house_k[n] > 1) {
      vector[k] pi_k = softmax(eta_obs);
      y_obs ~ dirichlet_multinomial(pi_k * phi[house_k[n] - 1]);
    } else {
      y_obs ~ multinomial_logit(eta_obs);
    }
  }

  /* Fundamentals Likelihood */
  beta_lag_f ~ std_normal();
  beta_inc_years_f ~ std_normal();
  beta_vnv_f ~ normal(0, 5);
  beta_growth_f ~ normal(0, 5);
  alpha_f_raw ~ std_normal();
  phi_f_inv ~ exponential(1);

  for (d in 1:D_f) { 
    vector[n_parties_f[d]] mu_d;
    mu_d[2:n_parties_f[d]] = alpha_f[index_f[2:n_parties_f[d], d]] + 
      beta_lag_f * x_f[index_f[2:n_parties_f[d], d], d] + 
      beta_inc_years_f * incumbent_years[index_f[2:n_parties_f[d], d], d] +
      beta_vnv_f * vnv[index_f[2:n_parties_f[d], d], d] +
      beta_growth_f * growth[index_f[2:n_parties_f[d], d], d];
    mu_d[1] = -sum(mu_d[2:n_parties_f[d]]);
    vector[n_parties_f[d]] pi_d = softmax(mu_d);
    y_f[index_f[1:n_parties_f[d], d], d] ~ dirichlet_multinomial(pi_d * phi_f);
  }
}

generated quantities {
  real<lower = 0, upper = 1> mu_gamma_weight;
  mu_gamma_weight = beta_rng(40, 60);
  vector[P - 1] mu_gamma_pred;
  mu_gamma_pred[1:(P - 2)] = mu_gamma;   
  mu_gamma_pred[P - 1] = - sum(mu_gamma);
  mu_gamma_pred[1:(P - 3)] /= sqrt(3.0);
  mu_gamma_pred[P - 2] /= sqrt(2.0);
  mu_gamma_pred *= mu_gamma_weight;
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
    delta_pred[p, 1:(K - 1)] = delta_free[p, ];
    delta_pred[p, K] = -sum(to_vector(delta_pred[p, 1:(K - 1)]) .* constituency_weights_pred[1:(K - 1)]) / 
          constituency_weights_pred[K];
  }

  // Generate national-level predictions. Like the likelihood, build the full
  // sum-to-zero P-vector, then gather the parties active at date d by identity.
  for (d in 1:D) {
    vector[P] eta_d;
    eta_d[2:P] = beta[, d] + mu_gamma_pred;
    eta_d[1] = -sum(eta_d[2:P]);
    array[n_active_d[d]] int cols = active_index_d[d, 1:n_active_d[d]];
    y_rep_national[d, cols] = multinomial_logit_rng(eta_d[cols], n_pred);
  }

  // Generate future predictions
  for (d in 1:pred_y_time_diff) {
    vector[P] eta_d;
    vector[P] pi_d;
    eta_d[2:P] = beta[, D + d] + mu_gamma_pred;
    eta_d[1] = -sum(eta_d[2:P]);
    pi_d = softmax(eta_d);
    y_rep_national[D + d, ] = multinomial_logit_rng(eta_d[1:P], n_pred);
  }

  // Generate constituency-level predictions
  for (k in 1:K) {
    vector[P] eta_k;
    vector[P] pi_k;
    
    eta_k[2:P] = beta[, D + pred_y_time_diff] + delta_pred[, k] + mu_gamma_pred;
    eta_k[1] = -sum(eta_k[2:P]);
    pi_k = softmax(eta_k);
    y_pred_constituency[k, ] = multinomial_logit_rng(eta_k[1:P], n_pred_k[k]);
  }

  // Generate constituency-level replications
  for (n in 1:N_k) {
    matrix[P - 1, K] delta;
    vector[P] eta_k;

    for (p in 1:(P - 1)) {
      delta[p, 1:(K - 1)] = delta_free[p, ];
      delta[p, K] = -sum(delta[p, 1:(K - 1)] .* constituency_weights[obs_k[n], 1:(K - 1)]) / 
            constituency_weights[obs_k[n] , K];
    }

    eta_k[2:P] = beta[, date_k[n]] + gamma[, house_k[n]] +
      delta[, constituency_k[n]] + mu_gamma_pred;
    eta_k[1] = -sum(eta_k[2:P]);

    // Gather the poll's reported parties by identity, as the likelihood does
    int k = n_parties_k[n];
    array[k] int cols = party_index_k[n, 1:k];
    if (house_k[n] > 1) {
      y_rep_k[n, cols] =
        dirichlet_multinomial_rng(softmax(eta_k[cols]) * phi[house_k[n] - 1], sum(y_k[n, ]));
    } else {
      y_rep_k[n, cols] = multinomial_logit_rng(eta_k[cols], sum(y_k[n, ]));
    }
  }
}

