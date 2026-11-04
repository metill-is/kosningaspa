// US midterm prototype (v1): us_midterm_v0.stan with tighter priors, to get a first working fit.
//
// v1 changes only priors, each centred on US-plausible values (usa/README.md, v1):
//   log phi:      mu_phi ~ normal(7, 1), sigma_phi ~ half-normal(0, 0.5) (v0: normal(0, 1) both;
//                 a poll of ~1,000 with a design effect near 2 has phi near 1,000, log 7)
//   mu_gamma0:    normal(0, 0.1) per direction (v0: std_normal), industry bias of a few points
//   sigma_gamma:  half-normal(0, 0.1) (v0: exponential(1)), house spread of a few points
//   the epoch shock scales keep tau_prior_scale, which the data now set to 0.05
// v0's header follows.
//
// US midterm prototype (v0): polling_watch_v5_epoch.stan plus a race layer.
//
// Feasibility experiment (usa/README.md), not a production model. Everything from the
// data block's first section through the national poll likelihood is v5_epoch's code
// unchanged: the national walk (generic congressional ballot) anchored at the national
// House popular vote of each past election, with non-centred Brownian bridges between
// them, and pollster bias (industry mu_gamma and house deviations) that random-walks
// across the epochs between elections. What v0 adds:
//
//   1. Election day. beta_elec is one forward step of pred_days from the last date D, and
//      a fundamentals factor N(mu_fund, sd_fund) on it (the forecast model's fundamentals
//      prior on the walk's end, as a product of experts).
//   2. Races. Each race r of the target election (a Senate state or a House district) is
//      a centred-logit deviation from the national walk,
//          delta[r] = sum_q X_race[r, q] theta[q] + sigma_race * z_race[r],
//      zero-sum over parties. theta and sigma_race are learnt from past contested
//      results, whose deviations from that election's national result are DATA (d_hist):
//      a district's result has 10^5 votes or more, which pins its deviation far tighter
//      than sigma_race, so modelling it as a parameter would only add stiff directions.
//      A race poll sees beta[date] + delta[race] + gamma[epoch, house].
//   3. Seats. Each race's winner on election day (argmax of softmax(beta_elec + delta)),
//      counted per chamber; the Senate count adds the seats not up this year.
//
// Parties: 1 = Democratic, 2 = Republican, 3 = Other (when P = 3).

data {
  int<lower = 1> D;                                // Number of time points
  int<lower = 2> P;                                // Number of parties
  int<lower = 1> H;                                // Number of houses (house 1 = election anchor)
  int<lower = 1> N;                                // Number of national polls and results
  int<lower = 1> E;                                // Number of bias epochs

  array[N, P] int<lower = 0> y_n;                  // Polling counts per party [N x P]
  array[N] int<lower = 1, upper = H> house_n;      // House indicator per poll [N]
  array[N] int<lower = 1, upper = D> date_n;       // Date indicator per poll [N]
  array[N] int<lower = 1, upper = E> epoch_n;      // Bias epoch per poll [N] (ignored for house 1)
  array[N] int<lower = 1, upper = P> n_parties_n;  // Parties reported per poll [N]
  array[N, P] int<lower = 0, upper = P> party_index_n; // Column ids of reported parties per poll (0-padded tail)

  vector[D - 1] time_diff;                         // Gaps between consecutive dates [D-1]
  int<lower = 1> n_pred;                           // Sample size for posterior predictions

  int<lower = 0, upper = 1> estimate_tau;          // 1 = sample tau_mu, tau_house; 0 = use the fixed values
  real<lower = 0> tau_mu_fixed;                    // industry shock SD when estimate_tau = 0
  real<lower = 0> tau_house_fixed;                 // house shock SD when estimate_tau = 0
  real<lower = 0> tau_prior_scale;                 // half-normal scale for sampled taus

  // Election day
  int<lower = 1> pred_days;                        // Days from date D to election day
  vector[P] mu_fund;                               // Fundamentals' election-day centred logits (zero-sum) [P]
  real<lower = 0> sd_fund;                         // Their SD per direction

  // Races of the target election
  int<lower = 0> R;                                // Number of races
  int<lower = 1> Q;                                // Number of race covariates
  matrix[R, Q] X_race;                             // Covariates of the target races [R x Q]
  int<lower = 0> N_hist;                           // Past contested race results
  matrix[N_hist, Q] X_hist;                        // Their covariates [N_hist x Q]
  matrix[N_hist, P] d_hist;                        // Their centred-logit deviations from the national result, rows zero-sum [N_hist x P]
  int<lower = 0> N_r;                              // Number of race polls
  array[N_r, P] int<lower = 0> y_r;                // Race poll counts [N_r x P]
  array[N_r] int<lower = 2, upper = H> house_r;    // House per race poll [N_r]
  array[N_r] int<lower = 1, upper = D> date_r;     // Date per race poll [N_r]
  array[N_r] int<lower = 1, upper = E> epoch_r;    // Bias epoch per race poll [N_r]
  array[N_r] int<lower = 1, upper = R> race_r;     // Race per race poll [N_r]
  array[N_r] int<lower = 1, upper = P> n_parties_r; // Parties reported per race poll [N_r]
  array[N_r, P] int<lower = 0, upper = P> party_index_r; // Their column ids (0-padded tail) [N_r x P]
  array[R] int<lower = 0, upper = 1> is_senate;    // 1 = Senate race, 0 = House district [R]
  int<lower = 0> senate_holdover_d;                // Democratic Senate seats not up this year
}

transformed data {
  real nu_sigma = 3.0;                             // half-Student-t df for per-party scales
  real scale_sigma = 0.02;                         // fixed prior scale (NOT sampled => no funnel)

  // v[t]: days in the step into date t (the step's variance multiplier); cv: running total
  vector[D] v = rep_vector(0, D);
  for (t in 2:D) {
    v[t] = time_diff[t - 1];
  }
  vector[D] cv = cumulative_sum(v);

  // Anchors: dates with an election result (house 1), and the parties each result reports
  array[D] int is_anchor = rep_array(0, D);
  array[D, P] int reported = rep_array(0, D, P);
  for (n in 1:N) {
    if (house_n[n] == 1) {
      is_anchor[date_n[n]] = 1;
      for (j in 1:n_parties_n[n]) {
        reported[date_n[n], party_index_n[n, j]] = 1;
      }
    }
  }
  int m = sum(is_anchor);
  if (m == 0) {
    reject("us_midterm_v1: the data hold no election result (house 1)");
  }
  array[m] int anchor;
  {
    int j = 0;
    for (t in 1:D) {
      if (is_anchor[t] == 1) {
        j += 1;
        anchor[j] = t;
      }
    }
  }
  // Reference party: the last party that every result reports. The walk's coordinates
  // are the other parties' levels (oth); the reference's level is minus their sum.
  int ref_party = 0;
  for (p in 1:P) {
    int all_report = 1;
    for (j in 1:m) {
      if (reported[anchor[j], p] == 0) all_report = 0;
    }
    if (all_report == 1) ref_party = p;
  }
  if (ref_party == 0) {
    reject("us_midterm_v1: no party is reported by every election result");
  }
  array[P - 1] int oth;
  {
    int c = 0;
    for (p in 1:P) {
      if (p != ref_party) {
        c += 1;
        oth[c] = p;
      }
    }
  }
  array[m] int n_pres;
  array[m, P - 1] int pres_idx = rep_array(0, m, P - 1);
  array[m] int n_abs;
  array[m, P - 1] int abs_idx = rep_array(0, m, P - 1);
  for (j in 1:m) {
    n_pres[j] = 0;
    n_abs[j] = 0;
    for (i in 1:(P - 1)) {
      if (reported[anchor[j], oth[i]] == 1) {
        n_pres[j] += 1;
        pres_idx[j, n_pres[j]] = i;
      } else {
        n_abs[j] += 1;
        abs_idx[j, n_abs[j]] = i;
      }
    }
  }
  int nz = D - m;
  array[D] int zcol = rep_array(0, D);
  {
    int c = 0;
    for (t in 1:D) {
      if (is_anchor[t] == 0) {
        c += 1;
        zcol[t] = c;
      }
    }
  }
  array[m] vector[P - 1] w_off = rep_array(rep_vector(0, P - 1), m);
  for (n in 1:N) {
    if (house_n[n] == 1) {
      int j = 0;
      for (i in 1:m) {
        if (anchor[i] == date_n[n]) j = i;
      }
      real r_sum = 0;
      int n_r = 0;
      for (k in 1:n_parties_n[n]) {
        int c = party_index_n[n, k];
        if (c != ref_party) {
          int i = c < ref_party ? c : c - 1;
          w_off[j][i] = log((y_n[n, c] + 0.5) / (y_n[n, ref_party] + 0.5));
          r_sum += w_off[j][i];
          n_r += 1;
        }
      }
      for (k in 1:n_parties_n[n]) {
        int c = party_index_n[n, k];
        if (c != ref_party) {
          int i = c < ref_party ? c : c - 1;
          w_off[j][i] -= r_sum / (n_r + 1);
        }
      }
    }
  }

  real phi_cap = 20;
  real phi_init = 0;
  {
    int n_polls = 0;
    for (n in 1:N) {
      if (house_n[n] > 1) {
        phi_init += log(sum(y_n[n, ]));
        n_polls += 1;
      }
    }
    if (n_polls > 0) phi_init /= n_polls;
  }
}

parameters {
  cholesky_factor_corr[P] L_Omega;                 // full P x P cross-party innovation correlation
  array[m] vector[P - 1] anchor_w;
  matrix[P - 1, nz] z_walk;
  vector[P] log_sigma;

  vector[H - 1] log_phi_u;
  real mu_phi;
  real<lower = 0> sigma_phi;

  sum_to_zero_vector[P] mu_gamma0;                 // industry bias in epoch 1
  array[H - 1] sum_to_zero_vector[P] gamma_free;   // standardised house deviations in epoch 1
  real<lower = 0> sigma_gamma;                     // cross-house spread in epoch 1
  array[E - 1] sum_to_zero_vector[P] z_mu;             // industry shocks
  array[E - 1, H - 1] sum_to_zero_vector[P] z_house;   // house shocks
  array[estimate_tau] real<lower = 0> tau_mu_est;
  array[estimate_tau] real<lower = 0> tau_house_est;

  // v0 additions
  vector[P - 1] z_elec;                            // Election-day step from date D (non-centred)
  array[Q] sum_to_zero_vector[P] theta;            // Race regression coefficients
  real<lower = 0> sigma_race;                      // Residual SD of a race's deviation, per direction
  array[R] sum_to_zero_vector[P] z_race;           // Standardised race deviations (non-centred)
}

transformed parameters {
  vector<lower = 0>[P] sigma = exp(log_sigma);
  vector[H - 1] log_phi = phi_cap - (phi_cap - phi_init) * exp(log_phi_u);
  vector<lower = 0>[H - 1] phi = exp(log_phi);

  real<lower = 0> tau_mu = estimate_tau ? tau_mu_est[1] : tau_mu_fixed;
  real<lower = 0> tau_house = estimate_tau ? tau_house_est[1] : tau_house_fixed;

  array[E] vector[P] mu_gamma;
  array[E, H] vector[P] gamma;
  {
    array[H - 1] vector[P] dev;
    for (h in 1:(H - 1)) {
      dev[h] = sigma_gamma * gamma_free[h];
    }
    mu_gamma[1] = mu_gamma0;
    for (e in 1:E) {
      if (e > 1) {
        mu_gamma[e] = mu_gamma[e - 1] + tau_mu * z_mu[e - 1];
        for (h in 1:(H - 1)) {
          dev[h] += tau_house * z_house[e - 1, h];
        }
      }
      gamma[e, 1] = rep_vector(0.0, P);
      for (h in 2:H) {
        gamma[e, h] = mu_gamma[e] + dev[h - 1];
      }
    }
  }

  matrix[P - 1, P - 1] L_walk;
  {
    matrix[P, P] M = diag_pre_multiply(sigma, L_Omega);
    matrix[P, P] CM = M - rep_matrix(rep_row_vector(1.0 / P, P) * M, P);
    L_walk = cholesky_decompose(tcrossprod(CM[oth, ]));
  }

  array[D] vector[P] beta;
  vector[P] beta_elec;
  {
    matrix[P - 1, D] b;
    for (i in 1:m) {
      int j = m + 1 - i;
      if (n_abs[j] > 0) {
        array[n_abs[j]] int ab = abs_idx[j, 1:n_abs[j]];
        array[n_pres[j]] int pr = pres_idx[j, 1:n_pres[j]];
        if (j < m) {
          matrix[n_abs[j], n_abs[j]] L_aa = cholesky_decompose(multiply_lower_tri_self_transpose(L_walk)[ab, ab]);
          b[ab, anchor[j]] = b[ab, anchor[j + 1]]
            + sqrt(cv[anchor[j + 1]] - cv[anchor[j]]) * L_aa * anchor_w[j][ab];
        } else {
          b[ab, anchor[j]] = anchor_w[j][ab];
        }
        b[pr, anchor[j]] = anchor_w[j][pr] + w_off[j][pr] - sum(b[ab, anchor[j]]) / (n_pres[j] + 1);
      } else {
        b[, anchor[j]] = anchor_w[j] + w_off[j];
      }
    }
    matrix[P - 1, nz] Lz = L_walk * z_walk;
    for (j in 1:(m - 1)) {
      int a = anchor[j];
      int nxt = anchor[j + 1];
      for (t in (a + 1):(nxt - 1)) {
        real R_ = cv[nxt] - cv[t - 1];
        b[, t] = b[, t - 1] + (v[t] / R_) * (b[, nxt] - b[, t - 1])
          + sqrt(v[t] * (cv[nxt] - cv[t]) / R_) * Lz[, zcol[t]];
      }
    }
    for (i in 1:(anchor[1] - 1)) {
      int t = anchor[1] - i;
      b[, t] = b[, t + 1] + sqrt(v[t + 1]) * Lz[, zcol[t]];
    }
    for (t in (anchor[m] + 1):D) {
      b[, t] = b[, t - 1] + sqrt(v[t]) * Lz[, zcol[t]];
    }
    for (t in 1:D) {
      beta[t][oth] = b[, t];
      beta[t][ref_party] = -sum(b[, t]);
    }
    vector[P - 1] b_elec = b[, D] + sqrt(pred_days) * L_walk * z_elec;
    beta_elec[oth] = b_elec;
    beta_elec[ref_party] = -sum(b_elec);
  }

  matrix[Q, P] theta_mat;
  for (q in 1:Q) {
    theta_mat[q] = theta[q]';
  }
  array[R] vector[P] delta;
  {
    matrix[R, P] delta_mean = X_race * theta_mat;
    for (r in 1:R) {
      delta[r] = delta_mean[r]' + sigma_race * z_race[r];
    }
  }
}

model {
  L_Omega ~ lkj_corr_cholesky(10);

  beta[1] ~ normal(0, 2);
  to_vector(z_walk) ~ std_normal();
  for (j in 1:(m - 1)) {
    real sc = sqrt(cv[anchor[j + 1]] - cv[anchor[j]]);
    vector[P - 1] b_here = beta[anchor[j]][oth];
    vector[P - 1] b_next = beta[anchor[j + 1]][oth];
    if (n_abs[j] > 0) {
      int na = n_abs[j];
      array[na] int ab = abs_idx[j, 1:na];
      array[n_pres[j]] int pr = pres_idx[j, 1:n_pres[j]];
      array[P - 1] int perm = append_array(ab, pr);
      matrix[P - 1, P - 1] L_perm = cholesky_decompose(multiply_lower_tri_self_transpose(L_walk)[perm, perm]);
      anchor_w[j][ab] ~ std_normal();
      b_here[pr] ~ multi_normal_cholesky(b_next[pr] + sc * L_perm[(na + 1):(P - 1), 1:na] * anchor_w[j][ab],
                                         sc * L_perm[(na + 1):(P - 1), (na + 1):(P - 1)]);
    } else {
      b_here ~ multi_normal_cholesky(b_next, sc * L_walk);
    }
  }

  target += student_t_lpdf(sigma | nu_sigma, 0, scale_sigma) + sum(log_sigma);

  log_phi ~ normal(mu_phi, sigma_phi);
  target += sum(log_phi_u);
  mu_phi ~ normal(7, 1);
  sigma_phi ~ normal(0, 0.5);

  mu_gamma0 ~ normal(0, 0.1);
  for (h in 1:(H - 1)) {
    gamma_free[h] ~ std_normal();
  }
  sigma_gamma ~ normal(0, 0.1);

  for (e in 1:(E - 1)) {
    z_mu[e] ~ std_normal();
    for (h in 1:(H - 1)) {
      z_house[e, h] ~ std_normal();
    }
  }
  if (estimate_tau) {
    tau_mu_est ~ normal(0, tau_prior_scale);
    tau_house_est ~ normal(0, tau_prior_scale);
  }

  for (n in 1:N) {
    int k = n_parties_n[n];
    vector[P] eta = beta[date_n[n]] + gamma[epoch_n[n], house_n[n]];
    array[k] int cols = party_index_n[n, 1:k];
    array[k] int y_obs = y_n[n, cols];
    if (house_n[n] > 1) {
      vector[k] pi_n = softmax(eta[cols]);
      y_obs ~ dirichlet_multinomial(pi_n * phi[house_n[n] - 1]);
    } else {
      y_obs ~ multinomial_logit(eta[cols]);
    }
  }

  // v0: election day
  z_elec ~ std_normal();
  // Fundamentals factor on the zero-sum election-day logits: isotropic on the P - 1
  // dimensional subspace (mu_fund is zero-sum, so the residual is too)
  target += -0.5 * dot_self(beta_elec - mu_fund) / square(sd_fund);

  // v0: races. The past deviations' residuals are zero-sum rows, isotropic normal with
  // SD sigma_race per direction on the P - 1 dimensional subspace.
  for (q in 1:Q) {
    theta[q] ~ normal(0, 1);
  }
  sigma_race ~ normal(0, 0.5);
  if (N_hist > 0) {
    matrix[N_hist, P] res = d_hist - X_hist * theta_mat;
    target += -0.5 * dot_self(to_vector(res)) / square(sigma_race)
      - N_hist * (P - 1) * log(sigma_race);
  }
  for (r in 1:R) {
    z_race[r] ~ std_normal();
  }
  for (n in 1:N_r) {
    int k = n_parties_r[n];
    vector[P] eta = beta[date_r[n]] + delta[race_r[n]] + gamma[epoch_r[n], house_r[n]];
    array[k] int cols = party_index_r[n, 1:k];
    array[k] int y_obs = y_r[n, cols];
    vector[k] pi_r = softmax(eta[cols]);
    y_obs ~ dirichlet_multinomial(pi_r * phi[house_r[n] - 1]);
  }
}

generated quantities {
  vector[P] pi_elec = softmax(beta_elec);          // National vote on election day
  array[R] real pi_race_d;                         // Democratic share per race on election day
  int seats_house_d = 0;
  int seats_senate_d = senate_holdover_d;
  for (r in 1:R) {
    vector[P] pr = softmax(beta_elec + delta[r]);
    pi_race_d[r] = pr[1];
    if (pr[1] == max(pr)) {
      if (is_senate[r] == 1) {
        seats_senate_d += 1;
      } else {
        seats_house_d += 1;
      }
    }
  }
}
