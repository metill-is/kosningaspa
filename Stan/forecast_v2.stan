// Arm V2's forecast model (spec §6), in one configuration. The latent national state is a
// forward sum-to-zero walk with per-party volatility, an LKJ correlation and a campaign
// multiplier, which also scales the step leaving each closed election (§6.2). National polls
// read the vote-weighted national share times exp(bias) (§6.3); the industry bias and the house
// deviations are constant over epochs, and a newcomer's bias is a contrast with mean 0 and scale
// 0.2023 (§6.4). One static kjordaemi deviation is mapped through each epoch's M_e (§6.7) and
// read by the kjordaemi polls and by the closed elections' kjordaemi results, which have no
// election-day shock (§6.6). Stage 1's fundamentals enter as a pseudo-observation (§6.5), and
// the concentrations are bounded (Decision 4). Every likelihood gathers its parties by identity
// through the data's idx_* lists, never by position. The data come from v2_stan_data() in
// R/harness/v2.R, and tests/harness/test-v2-stan.R tests the log density against the R
// reference R/harness/v2_reference.R. The file with the switched components (the epoch walk of
// the bias, the newcomer pool, the shock, the delta walk, the post-election multiplier, the
// calendar drift and the anchored and centred forms) is at the tag v2-switched-final.
functions {
  // The walk's per-step scale sqrt(Delta_i) s_i (§6.2), s_i = 1 + tau_camp c_i.
  vector step_scale(vector sqrt_delta, real tau_camp, vector campaign_v) {
    return sqrt_delta .* (1 + tau_camp * campaign_v);
  }

  // The map M_e of §6.7 on a K x P matrix: subtract each party's w-weighted mean over the
  // kjordaemi, then centre each kjordaemi over the parties. The result's rows sum to 0 and
  // its w-weighted column sums are 0, and it closes on no kjordaemi and no party.
  matrix kjordaemi_map(matrix X, row_vector w) {
    int K = rows(X);
    matrix[K, cols(X)] Y = X - rep_matrix(w * X, K);
    for (k in 1:K) {
      Y[k] -= mean(Y[k]);
    }
    return Y;
  }

  // The national share pi_nat at one grid date (§6.3): sum_k w_k softmax over the existing
  // parties A of (theta_t + delta_k), 0 for a party outside A.
  vector national_share(vector theta_t, matrix delta_e, row_vector w_e, array[] int A) {
    vector[size(A)] s = rep_vector(0, size(A));
    vector[rows(theta_t)] pi = rep_vector(0, rows(theta_t));
    for (k in 1:rows(delta_e)) {
      s += w_e[k] * softmax(theta_t[A] + delta_e[k, A]');
    }
    pi[A] = s;
    return pi;
  }

  // A national poll's Dirichlet-multinomial concentrations over its reported parties S
  // (§6.3): phi normalise(pi_nat[S] .* exp(gamma[S])).
  vector national_alpha(vector pi, vector gamma_eh, real phi, array[] int S) {
    vector[size(S)] a = pi[S] .* exp(gamma_eh[S]);
    return phi * a / sum(a);
  }

  // The rows 2:P of C diag(sigma) L_Omega, C = I - 11'/P: their Gram matrix is the walk's
  // step covariance (C Sigma C)[2:P, 2:P] per unit variance.
  matrix walk_rows(vector sigma, matrix L_Omega) {
    int P = rows(sigma);
    matrix[P, P] CM = diag_pre_multiply(sigma, L_Omega);
    CM -= rep_matrix(rep_row_vector(1, P) * CM / P, P);
    return CM[2:P];
  }

  // diag(C Sigma C'), C = I - 11'/P and Sigma = diag(sigma) Omega diag(sigma): each party's
  // variance of a walk step per unit Delta s^2 (§6.2), the row sums of squares of
  // C diag(sigma) L_Omega. Each of its columns sums to 0, so its row 1 is minus the sum of
  // walk_rows()' rows 2:P.
  vector walk_var(vector sigma, matrix L_Omega) {
    int P = rows(sigma);
    matrix[P - 1, P] R = walk_rows(sigma, L_Omega);
    return append_row(dot_self(rep_row_vector(1, P - 1) * R), rows_dot_self(R));
  }
}

data {
  // Sizes and the grid (§6.2)
  int<lower=2> P;                                     // parties; party 1 is the walk's coordinate party
  int<lower=1> K;                                     // kjordaemi
  int<lower=1> H;                                     // houses (pollsters), by first admitted poll
  int<lower=2> G;                                     // grid dates; date G is E
  int<lower=2> n_epoch;                               // epochs; epoch n_epoch is open and holds F
  vector<lower=0>[G - 1] delta_days;                  // [G - 1] days of each grid step
  array[G - 1] int<lower=0, upper=1> campaign;        // [G - 1] 1 on a step inside an election's campaign
  array[G] int<lower=1, upper=n_epoch> grid_epoch;    // [G] each date's epoch; an election day closes its epoch
  array[G, P] int<lower=0, upper=1> exists;           // [G, P] 1 from a party's first non-zero poll on
  matrix<lower=0>[n_epoch, K] w;                      // [n_epoch, K] kjordaemi vote weights; the open epoch's are the previous election's

  // National polls (§6.3)
  int<lower=1> N_nat;                                 // national polls
  array[N_nat] int<lower=1, upper=G> grid_nat;        // [N_nat] grid date
  array[N_nat] int<lower=1, upper=H> house_nat;       // [N_nat] house
  array[N_nat, P] int<lower=0> y_nat;                 // [N_nat, P] counts, 0 for an unreported party
  array[N_nat] int<lower=2, upper=P> n_rep_nat;       // [N_nat] reported parties
  array[N_nat, P] int<lower=0, upper=P> idx_nat;      // [N_nat, P] reported parties by index, 0-padded

  // Kjordaemi polls (§6.7)
  int<lower=0> J;                                     // kjordaemi polls (a breakdown of one national sample)
  int<lower=0> N_kd;                                  // kjordaemi poll rows, one per poll and kjordaemi
  array[N_kd] int<lower=1, upper=J> poll_kd;          // [N_kd] poll
  array[N_kd] int<lower=1, upper=K> kd_kd;            // [N_kd] kjordaemi
  array[N_kd] int<lower=1, upper=G> grid_kd;          // [N_kd] grid date
  array[N_kd] int<lower=1, upper=H> house_kd;         // [N_kd] house
  array[N_kd, P] int<lower=0> y_kd;                   // [N_kd, P] counts
  array[N_kd] int<lower=2, upper=P> n_rep_kd;         // [N_kd] reported parties
  array[N_kd, P] int<lower=0, upper=P> idx_kd;        // [N_kd, P] reported parties by index, 0-padded

  // Results of the closed elections (§6.6)
  int<lower=1> n_closed;                              // closed elections in the window, n_epoch - 1
  array[n_closed] int<lower=1, upper=G> grid_res;     // [n_closed] election date
  array[n_closed, K, P] int<lower=0> y_res;           // [n_closed, K, P] official kjordaemi votes
  array[n_closed] int<lower=2, upper=P> n_rep_res;    // [n_closed] parties with national votes
  array[n_closed, P] int<lower=0, upper=P> idx_res;   // [n_closed, P] those parties by index, 0-padded

  // Parties (§6.4)
  array[P] int<lower=0, upper=n_epoch> entry_epoch;   // [P] 0 for a party present at the window's start

  // The forecast and the outputs (§6.8)
  array[K] int<lower=1> N_open;                       // [K] valid votes of the previous election
  int<lower=0, upper=1> draw_forecast;                // 1: write votes_kd, pi_nat_eday and pi_latent_E
  int<lower=0, upper=1> latent_path;                  // 1: write pi_latent on the whole grid

  // Fundamentals (§6.5)
  int<lower=0> n_sf;                                  // parties of S_f
  array[n_sf] int<lower=1, upper=P> sf_idx;           // [n_sf] their indices
  vector[n_sf] mu_f;                                  // [n_sf] stage 1's centred predictor
  real<lower=0, upper=1> desired_weight;              // w
  real<lower=0> weight_time;                          // days
  int<lower=1> campaign_days;                         // days

  // Prior locations and scales (Tier 3b's and Decision 1's)
  real<lower=0> prior_sd_log_kappa;
  real prior_mu_phi;
  real<lower=0> prior_sd_mu1;
  real<lower=0> prior_rate_sigma_gamma;
}

transformed data {
  // The fundamentals pseudo-observation (§6.5) centres log pi_nat,E over the parties of S_f, so
  // it needs two of them at least, distinct, and each existing at E: outside A_E pi_nat,E is 0.
  // Its scale tau_f needs w in (0, 1) and a stretch off the campaign of weight_time -
  // campaign_days >= 0 days; the data block holds campaign_days >= 1.
  if (n_sf < 2) {
    reject("forecast_v2: the fundamentals need at least two parties of S_f, since mu_f's mean is centred over them, but n_sf = ",
           n_sf);
  }
  for (i in 1:(n_sf - 1)) {
    for (j in (i + 1):n_sf) {
      if (sf_idx[i] == sf_idx[j]) {
        reject("forecast_v2: the fundamentals need the parties of S_f to be distinct, but sf_idx[", i, "] = sf_idx[", j,
               "] = ", sf_idx[i]);
      }
    }
  }
  for (i in 1:n_sf) {
    if (exists[G, sf_idx[i]] == 0) {
      reject("forecast_v2: the fundamentals need every party of S_f to exist at E (exists[G, sf_idx[i]] = 1), but sf_idx[",
             i, "] = ", sf_idx[i], " does not");
    }
  }
  if (desired_weight <= 0 || desired_weight >= 1) {
    reject("forecast_v2: the fundamentals need desired_weight in (0, 1), since tau_f = sqrt(V_w (1 - w) / w), but desired_weight = ",
           desired_weight);
  }
  if (weight_time < campaign_days) {
    reject("forecast_v2: the fundamentals need weight_time to be at least campaign_days, since V_w counts weight_time - campaign_days days off the campaign, but weight_time = ",
           weight_time, " and campaign_days = ", campaign_days);
  }

  // Decision 4: log c = cap - (cap - init) exp(u), so u = 0 is the init and c stays below the cap.
  real phi_cap = 20;
  real phi_init;                                      // the mean log national poll size
  real phi_K_cap = 20;
  real phi_K_init = log(1000);
  real kappa_cap = 2;
  real kappa_init = 0;
  {
    vector[N_nat] log_n;
    for (n in 1:N_nat) {
      log_n[n] = log(sum(y_nat[n]));
    }
    phi_init = mean(log_n);
  }

  // A newcomer's bias contrast (§6.4) has mean 0 and scale s_new, the median of half-N(0, 0.3)
  // to four digits.
  real s_new_fixed = 0.2023;

  vector[G - 1] sqrt_delta = sqrt(delta_days);
  vector[G - 1] campaign_v = to_vector(campaign);
  // WHY: R0 flags a step by the date it starts on, and an election date is 0 days before
  // itself, so the step leaving each closed election scales by the campaign multiplier too.
  for (j in 1:n_closed) campaign_v[grid_res[j]] = 1;
  real n_pred = sum(N_open);

  // The parties existing at each grid date, by index.
  array[G] int n_exist;
  array[G, P] int exist_idx = rep_array(0, G, P);
  for (t in 1:G) {
    n_exist[t] = 0;
    for (p in 1:P) {
      if (exists[t, p]) {
        n_exist[t] += 1;
        exist_idx[t, n_exist[t]] = p;
      }
    }
  }

  // Newcomers: each one's slot in new_raw, in party order; the parties entering in each
  // epoch, and those existing before it (present at the window's start or entering earlier).
  int N_new = 0;
  array[P] int new_pos = rep_array(0, P);
  for (p in 1:P) {
    if (entry_epoch[p] > 0) {
      N_new += 1;
      new_pos[p] = N_new;
    }
  }
  array[n_epoch] int n_enter = rep_array(0, n_epoch);
  array[n_epoch, P] int enter_idx = rep_array(0, n_epoch, P);
  array[n_epoch] int n_before = rep_array(0, n_epoch);
  array[n_epoch, P] int before_idx = rep_array(0, n_epoch, P);
  for (e in 1:n_epoch) {
    for (p in 1:P) {
      if (entry_epoch[p] == e) {
        n_enter[e] += 1;
        enter_idx[e, n_enter[e]] = p;
      } else if (entry_epoch[p] < e) {
        n_before[e] += 1;
        before_idx[e, n_before[e]] = p;
      }
    }
  }

  // The likelihood is written in matrix form, which evaluates the same density as one
  // national_share() and one dirichlet_multinomial per row at about a quarter of the gradient's
  // cost (generated quantities keep the row-by-row form for log_lik). Its indices:
  // the grid dates the national likelihood reads (each national poll's, and E for the
  // fundamentals), in date order; ucol[t] is date t's column, 0 if none. The columns of one
  // epoch are contiguous because grid_epoch never decreases, which the rejects below check.
  int nU = 0;
  array[G] int ucol = rep_array(0, G);
  {
    array[G] int need = rep_array(0, G);
    for (n in 1:N_nat) need[grid_nat[n]] = 1;
    need[G] = 1;
    for (t in 1:G) {
      if (need[t]) {
        nU += 1;
        ucol[t] = nU;
      }
    }
  }
  for (t in 2:G) {
    if (grid_epoch[t] < grid_epoch[t - 1]) {
      reject("forecast_v2: the matrix likelihood needs grid_epoch non-decreasing, but grid_epoch[", t, "] = ",
             grid_epoch[t], " < grid_epoch[", t - 1, "] = ", grid_epoch[t - 1]);
    }
  }
  if (grid_epoch[G] != n_epoch) {
    reject("forecast_v2: the matrix likelihood needs date G in the open epoch, but grid_epoch[G] = ", grid_epoch[G],
           " and n_epoch = ", n_epoch);
  }
  array[nU] int u_date;
  for (t in 1:G) {
    if (ucol[t] > 0) u_date[ucol[t]] = t;
  }
  matrix[P, nU] u_mask;
  for (j in 1:nU) {
    for (p in 1:P) u_mask[p, j] = exists[u_date[j], p];
  }
  array[n_epoch] int u_lo = rep_array(0, n_epoch);
  array[n_epoch] int u_n = rep_array(0, n_epoch);
  for (j in 1:nU) {
    int e = grid_epoch[u_date[j]];
    if (u_n[e] == 0) u_lo[e] = j;
    u_n[e] += 1;
  }

  // The national likelihood flattened: the reported cells (n, p) of the P x N_nat count matrix,
  // column-major, and each poll's epoch-house column of the gamma matrix.
  int n_flat = sum(n_rep_nat);
  array[n_flat] int flat_idx;
  vector[n_flat] y_flat;
  matrix[P, N_nat] rep_mask = rep_matrix(0, P, N_nat);
  vector[N_nat] N_poll;
  array[N_nat] int eh_nat;
  array[N_nat] int ucol_nat;
  {
    int i = 0;
    for (n in 1:N_nat) {
      N_poll[n] = sum(y_nat[n]);
      eh_nat[n] = (grid_epoch[grid_nat[n]] - 1) * H + house_nat[n];
      ucol_nat[n] = ucol[grid_nat[n]];
      for (j in 1:n_rep_nat[n]) {
        int p = idx_nat[n, j];
        i += 1;
        flat_idx[i] = (n - 1) * P + p;
        y_flat[i] = y_nat[n, p];
        rep_mask[p, n] = 1;
      }
    }
  }

  // The kjordaemi likelihood flattened, as the national one.
  int n_flat_kd = sum(n_rep_kd);
  array[n_flat_kd] int flat_kd;
  vector[n_flat_kd] y_flat_kd;
  matrix[P, N_kd] kd_mask = rep_matrix(0, P, N_kd);
  vector[N_kd] N_row_kd;
  array[N_kd] int ek_kd;
  {
    int i = 0;
    for (r in 1:N_kd) {
      N_row_kd[r] = sum(y_kd[r]);
      ek_kd[r] = (grid_epoch[grid_kd[r]] - 1) * K + kd_kd[r];
      for (j in 1:n_rep_kd[r]) {
        int p = idx_kd[r, j];
        i += 1;
        flat_kd[i] = (r - 1) * P + p;
        y_flat_kd[i] = y_kd[r, p];
        kd_mask[p, r] = 1;
      }
    }
  }

  // The results likelihood flattened: column (e - 1) K + k is closed election e's kjordaemi k.
  int n_flat_res = K * sum(n_rep_res);
  array[n_flat_res] int flat_res;
  vector[n_flat_res] y_flat_res;
  matrix[P, n_closed * K] res_mask = rep_matrix(0, P, n_closed * K);
  vector[n_closed * K] N_res;
  {
    int i = 0;
    for (e in 1:n_closed) {
      for (k in 1:K) {
        int c = (e - 1) * K + k;
        N_res[c] = sum(y_res[e, k]);
        for (j in 1:n_rep_res[e]) {
          int p = idx_res[e, j];
          i += 1;
          flat_res[i] = (c - 1) * P + p;
          y_flat_res[i] = y_res[e, k, p];
          res_mask[p, c] = 1;
        }
      }
    }
  }
  // Each row's log density is summed before the rows are added, so that its large lgamma terms
  // cancel within the row, as in dirichlet_multinomial, and not across the whole likelihood,
  // which would cost about 25 times the rounding. A row's cells are contiguous in the flattened
  // vectors, so the row sums are one CSR product with a matrix of ones: u_* are the rows' first
  // cells (CSR row starts) and v_* the cells' own indices.
  array[N_nat + 1] int u_nat;
  array[N_kd + 1] int u_kd;
  array[n_closed * K + 1] int u_res;
  u_nat[1] = 1;
  for (n in 1:N_nat) u_nat[n + 1] = u_nat[n] + n_rep_nat[n];
  u_kd[1] = 1;
  for (r in 1:N_kd) u_kd[r + 1] = u_kd[r] + n_rep_kd[r];
  u_res[1] = 1;
  for (e in 1:n_closed) {
    for (k in 1:K) u_res[(e - 1) * K + k + 1] = u_res[(e - 1) * K + k] + n_rep_res[e];
  }
  array[n_flat] int v_nat = linspaced_int_array(n_flat, 1, n_flat);
  array[n_flat_kd] int v_kd = linspaced_int_array(n_flat_kd, 1, n_flat_kd);
  array[n_flat_res] int v_res = linspaced_int_array(n_flat_res, 1, n_flat_res);
  // The data terms that dirichlet_multinomial keeps inside its lbeta() calls even in a sampling
  // statement, lgamma(N) - sum of lgamma(y) over the nonzero counts, row by row. The flattened
  // likelihood adds them to each row, so that lp__ is the row-by-row form's.
  vector[N_nat] dm_nat = lgamma(N_poll);
  for (n in 1:N_nat) {
    for (j in 1:n_rep_nat[n]) {
      if (y_nat[n, idx_nat[n, j]] > 0) dm_nat[n] -= lgamma(y_nat[n, idx_nat[n, j]]);
    }
  }
  vector[N_kd] dm_kd = lgamma(N_row_kd);
  for (r in 1:N_kd) {
    for (j in 1:n_rep_kd[r]) {
      if (y_kd[r, idx_kd[r, j]] > 0) dm_kd[r] -= lgamma(y_kd[r, idx_kd[r, j]]);
    }
  }
  vector[n_closed * K] dm_res = lgamma(N_res);
  for (e in 1:n_closed) {
    for (k in 1:K) {
      for (j in 1:n_rep_res[e]) {
        if (y_res[e, k, idx_res[e, j]] > 0) dm_res[(e - 1) * K + k] -= lgamma(y_res[e, k, idx_res[e, j]]);
      }
    }
  }
}

parameters {
  // The names, types and order of V2_CORE_PARS in R/harness/v2_reference.R, so the tests can
  // move a point between the reference and Stan without renaming or reordering.
  sum_to_zero_vector[P] theta1;                       // the state at the first grid date
  matrix[P - 1, G - 1] z_walk;                        // the walk's innovations in coordinates 2:P
  vector<lower=0>[P] sigma;                           // per sqrt(day), log-ratio
  cholesky_factor_corr[P] L_Omega;
  real<lower=0> tau_camp;                             // campaign multiplier
  sum_to_zero_vector[P] mu1;                          // industry bias
  matrix[H, P] house_raw;                             // house deviations, before C and sigma_gamma
  real<lower=0> sigma_gamma;
  vector[N_new] new_raw;                              // newcomers' bias contrasts
  vector[H] log_phi_u;                                // bounded log phi_h
  real mu_phi;
  real<lower=0> sigma_phi;
  real log_phi_K_u;                                   // bounded log phi_K
  real log_kappa_u;                                   // bounded log kappa
  array[1] matrix[K, P] delta_raw;                    // zeta of the static kjordaemi deviation
  vector[P] s_delta0_raw;                             // log s_delta0 = mu_sd + tau_sd s_delta0_raw
  real mu_sd;
  real<lower=0> tau_sd;
  array[J] sum_to_zero_vector[P] nu;                  // kjordaemi polls' levels
}

transformed parameters {
  matrix[P, G] theta;                                 // the latent national log-ratio state
  array[n_epoch] vector[P] mu;                        // industry bias by epoch
  array[n_epoch, H] vector[P] gamma;                  // house bias by epoch
  array[n_epoch] matrix[K, P] delta;                  // kjordaemi deviations by epoch
  vector[H] log_phi = phi_cap - (phi_cap - phi_init) * exp(log_phi_u);
  real log_phi_K = phi_K_cap - (phi_K_cap - phi_K_init) * exp(log_phi_K_u);
  real log_kappa = kappa_cap - (kappa_cap - kappa_init) * exp(log_kappa_u);
  vector[P] s_delta0 = exp(mu_sd + tau_sd * s_delta0_raw);

  // §6.2: coordinates 2:P step by sqrt(Delta_i) s_i L_walk z_i and party 1 by minus their
  // sum, so every theta_t sums to 0 and each step's covariance is Delta_i s_i^2 C Sigma C.
  // L_walk is chol((C Sigma C)[2:P, 2:P]), taken as R' from the thin QR decomposition of
  // B' with B the rows 2:P of C diag(sigma) L_Omega, since B B' = (C Sigma C)[2:P, 2:P]:
  // forming B B' would square the condition number (proposal 3).
  {
    matrix[P - 1, P - 1] L_walk = qr_thin_R(walk_rows(sigma, L_Omega)')';
    matrix[P - 1, G - 1] step = diag_post_multiply(L_walk * z_walk, step_scale(sqrt_delta, tau_camp, campaign_v));
    matrix[P - 1, G] th = append_col(theta1[2:P], step);
    for (p in 1:(P - 1)) th[p] = cumulative_sum(th[p]);
    theta[2:P, : ] = th;
    theta[1, : ] = -rep_row_vector(1, P - 1) * th;
  }

  // §6.4, the industry bias: mu_1, constant over epochs, except that from its entry epoch on a
  // newcomer's bias is the mean of the current biases of the parties existing before that
  // epoch, plus s_new eta; then mu_e is re-centred.
  {
    vector[P] m = mu1;
    for (e in 1:n_epoch) {
      if (n_enter[e] > 0) {
        real base = mean(m[before_idx[e, 1:n_before[e]]]);
        for (i in 1:n_enter[e]) {
          int p = enter_idx[e, i];
          m[p] = base + s_new_fixed * new_raw[new_pos[p]];
        }
      }
      m -= mean(m);
      mu[e] = m;
    }
  }

  // gamma_{e,h} = mu_e + d_h, with d_h = C(sigma_gamma eps_h) the house's constant deviations;
  // a newcomer's slot is its deviation from its entry on.
  for (h in 1:H) {
    row_vector[P] x = sigma_gamma * house_raw[h];
    for (e in 1:n_epoch) {
      gamma[e, h] = (x - mean(x))' + mu[e];
    }
  }

  // §6.7: delta_e = M_e(s_delta0 .* zeta), one zeta for every epoch.
  {
    matrix[K, P] X = diag_post_multiply(delta_raw[1], s_delta0);
    for (e in 1:n_epoch) {
      delta[e] = kjordaemi_map(X, w[e]);
    }
  }
}

model {
  vector[H] phi = exp(log_phi);
  real kappa = exp(log_kappa);
  real phi_K = exp(log_phi_K);

  // pi_nat at every date the likelihood reads (§6.3): pi[p, j] = sum_k w_k exp(theta_p +
  // delta_kp) m_p / sum_q m_q exp(theta_q + delta_kq), m the date's existing parties, from two
  // matrix products per epoch: S = exp(delta_e) Et gives every kjordaemi's normaliser at every
  // date, and exp(delta_e)' (w_e ./ S) the weighted sum. Subtracting max(theta) before exp()
  // cancels in each ratio and keeps exp() from overflowing, as softmax() does.
  matrix[P, nU] Pi_u;
  {
    matrix[P, nU] Et = exp(theta[ : , u_date] - max(theta)) .* u_mask;
    for (e in 1:n_epoch) {
      if (u_n[e] > 0) {
        matrix[K, P] Ed = exp(delta[e]);
        matrix[P, u_n[e]] Eb = block(Et, 1, u_lo[e], P, u_n[e]);
        matrix[K, u_n[e]] C = rep_matrix(w[e]', u_n[e]) ./ (Ed * Eb);
        Pi_u[ : , u_lo[e]:(u_lo[e] + u_n[e] - 1)] = Eb .* (Ed' * C);
      }
    }
  }

  // National polls (§6.3): y_n ~ DirMult(phi_h normalise(pi_nat[S] .* exp(gamma_{e,h}[S]))).
  // Flattened: alpha_n sums to phi_h, so the density's lgamma(alpha_0) - lgamma(alpha_0 + N) is
  // lgamma(phi_h) - lgamma(phi_h + N), and the rest is a sum of lgamma(alpha + y) - lgamma(alpha)
  // over the reported cells; each row is summed first (transformed data's u_nat) and gets its
  // data terms (dm_nat).
  {
    matrix[P, n_epoch * H] Gm;
    for (e in 1:n_epoch) {
      for (h in 1:H) Gm[ : , (e - 1) * H + h] = gamma[e, h];
    }
    matrix[P, N_nat] A = Pi_u[ : , ucol_nat] .* exp(Gm)[ : , eh_nat] .* rep_mask;
    vector[N_nat] phin = phi[house_nat];
    matrix[P, N_nat] Al = diag_post_multiply(A, phin ./ (rep_row_vector(1, P) * A)');
    vector[n_flat] af = to_vector(Al)[flat_idx];
    vector[N_nat] cells = csr_matrix_times_vector(N_nat, n_flat, rep_vector(1, n_flat), v_nat, u_nat,
                                                  lgamma(af + y_flat) - lgamma(af));
    target += sum(cells + lgamma(phin) - lgamma(phin + N_poll) + dm_nat);
  }

  // Kjordaemi polls (§6.7): y_jk ~ DirMult(kappa phi_h softmax over S of (nu_j + delta_{e,k})),
  // flattened as the national polls, the softmax as exp(nu_j) .* exp(delta_ek) masked to S.
  {
    matrix[P, J] Enu;
    for (j in 1:J) Enu[ : , j] = nu[j];
    matrix[P, n_epoch * K] Dm;
    for (e in 1:n_epoch) Dm[ : , ((e - 1) * K + 1):(e * K)] = delta[e]';
    matrix[P, N_kd] A = exp(Enu)[ : , poll_kd] .* exp(Dm)[ : , ek_kd] .* kd_mask;
    vector[N_kd] cphi = kappa * phi[house_kd];
    matrix[P, N_kd] Al = diag_post_multiply(A, cphi ./ (rep_row_vector(1, P) * A)');
    vector[n_flat_kd] af = to_vector(Al)[flat_kd];
    vector[N_kd] cells = csr_matrix_times_vector(N_kd, n_flat_kd, rep_vector(1, n_flat_kd), v_kd, u_kd,
                                                 lgamma(af + y_flat_kd) - lgamma(af));
    target += sum(cells + lgamma(cphi) - lgamma(cphi + N_row_kd) + dm_kd);
  }

  // Results (§6.6): y_ek ~ DirMult(phi_K softmax over S of (theta_te + delta_{e,k})), flattened
  // as the national polls.
  {
    matrix[P, n_closed * K] Ar;
    for (e in 1:n_closed) {
      int t = grid_res[e];
      int de = grid_epoch[t];
      Ar[ : , ((e - 1) * K + 1):(e * K)] = rep_matrix(theta[ : , t], K) + delta[de]';
    }
    Ar = exp(Ar) .* res_mask;
    matrix[P, n_closed * K] Al = diag_post_multiply(Ar, phi_K ./ (rep_row_vector(1, P) * Ar)');
    vector[n_flat_res] af = to_vector(Al)[flat_res];
    vector[n_closed * K] cells = csr_matrix_times_vector(n_closed * K, n_flat_res, rep_vector(1, n_flat_res), v_res,
                                                         u_res, lgamma(af + y_flat_res) - lgamma(af));
    target += sum(cells + lgamma(phi_K) - lgamma(phi_K + N_res) + dm_res);
  }

  // The fundamentals pseudo-observation (§6.5): mu_f ~ N(x_c, tau_f), x = log pi_nat,E[S_f]
  // with pi_nat,E the latent national share at E (theta_E with the open epoch's delta and w,
  // over A_E), x_c = x - mean(x), tau_f = sqrt(V_w (1 - w) / w) and V_w = diag(C Sigma C')[S_f]
  // ((weight_time - campaign_days) + campaign_days (1 + tau_camp)^2), the walk's variance over
  // weight_time days, the last campaign_days in the campaign: the weight w at weight_time days
  // is relative to that variance alone. A likelihood term on data, so no Jacobian; tau_f depends
  // on sigma, Omega and tau_camp, so the sampling statement keeps its -log tau_f.
  {
    vector[P] pi_E = Pi_u[ : , ucol[G]];
    vector[n_sf] x = log(pi_E[sf_idx]);
    real days = (weight_time - campaign_days) + campaign_days * square(1 + tau_camp);
    vector[n_sf] V_w = walk_var(sigma, L_Omega)[sf_idx] * days;
    mu_f ~ normal(x - mean(x), sqrt(V_w * (1 - desired_weight) / desired_weight));
  }

  // Priors (§6.10, Decisions 1, 2 and 4). A normal on a sum_to_zero_vector is on every element.
  theta1 ~ normal(0, 2);
  to_vector(z_walk) ~ std_normal();
  sigma ~ student_t(3, 0, 0.02);
  L_Omega ~ lkj_corr_cholesky(10);
  tau_camp ~ exponential(1);
  mu1 ~ normal(0, prior_sd_mu1);
  to_vector(house_raw) ~ std_normal();
  sigma_gamma ~ exponential(prior_rate_sigma_gamma);
  new_raw ~ std_normal();
  log_phi ~ normal(mu_phi, sigma_phi);
  mu_phi ~ normal(prior_mu_phi, 1);
  sigma_phi ~ std_normal();
  log_phi_K ~ normal(log(1000), 1);
  log_kappa ~ normal(0, prior_sd_log_kappa);
  to_vector(delta_raw[1]) ~ std_normal();
  s_delta0_raw ~ std_normal();
  mu_sd ~ normal(log(0.3), 0.5);
  tau_sd ~ normal(0, 0.5);
  for (j in 1:J) {
    nu[j] ~ normal(0, 2);
  }
  // The bounded transforms' log-Jacobians, |d log c / d u| = (cap - init) exp(u).
  target += H * log(phi_cap - phi_init) + sum(log_phi_u);
  target += log(phi_K_cap - phi_K_init) + log_phi_K_u;
  target += log(kappa_cap - kappa_init) + log_kappa_u;
}

generated quantities {
  // The forecast (§6.8), with draw_forecast = 1: the kjordaemi counts at E, their
  // vote-weighted national aggregate, and the latent national share at E.
  array[draw_forecast ? K : 0, P] int votes_kd;
  vector[draw_forecast ? P : 0] pi_nat_eday;
  vector[draw_forecast ? P : 0] pi_latent_E;
  // The log-likelihood of each row: national polls, kjordaemi poll rows, then the results
  // election by election, kjordaemi by kjordaemi.
  vector[N_nat + N_kd + n_closed * K] log_lik;
  // A replication of each closed election's kjordaemi counts, 0 outside its parties.
  array[n_closed, K, P] int y_rep_res = rep_array(0, n_closed, K, P);
  // pi_nat on the whole grid, with latent_path = 1.
  matrix[latent_path ? P : 0, latent_path ? G : 0] pi_latent;
  {
    vector[H] phi = exp(log_phi);
    real kappa = exp(log_kappa);
    real phi_K = exp(log_phi_K);
    for (n in 1:N_nat) {
      int t = grid_nat[n];
      int e = grid_epoch[t];
      array[n_rep_nat[n]] int S = idx_nat[n, 1:n_rep_nat[n]];
      vector[P] pi = national_share(theta[ : , t], delta[e], w[e], exist_idx[t, 1:n_exist[t]]);
      log_lik[n] = dirichlet_multinomial_lpmf(y_nat[n, S] | national_alpha(pi, gamma[e, house_nat[n]], phi[house_nat[n]], S));
    }
    for (r in 1:N_kd) {
      int e = grid_epoch[grid_kd[r]];
      array[n_rep_kd[r]] int S = idx_kd[r, 1:n_rep_kd[r]];
      log_lik[N_nat + r] = dirichlet_multinomial_lpmf(y_kd[r, S] | kappa * phi[house_kd[r]] * softmax(nu[poll_kd[r]][S] + delta[e][kd_kd[r], S]'));
    }
    for (e in 1:n_closed) {
      int t = grid_res[e];
      int de = grid_epoch[t];
      array[n_rep_res[e]] int S = idx_res[e, 1:n_rep_res[e]];
      for (k in 1:K) {
        vector[n_rep_res[e]] alpha = phi_K * softmax(theta[S, t] + delta[de][k, S]');
        log_lik[N_nat + N_kd + (e - 1) * K + k] = dirichlet_multinomial_lpmf(y_res[e, k, S] | alpha);
        y_rep_res[e, k, S] = dirichlet_multinomial_rng(alpha, sum(y_res[e, k, S]));
      }
    }

    if (draw_forecast) {
      int nA = n_exist[G];
      array[nA] int A = exist_idx[G, 1:nA];
      vector[nA] latent = rep_vector(0, nA);
      for (k in 1:K) {
        vector[nA] s = softmax(theta[A, G] + delta[n_epoch][k, A]');
        votes_kd[k] = rep_array(0, P);
        votes_kd[k, A] = dirichlet_multinomial_rng(phi_K * s, N_open[k]);
        latent += w[n_epoch, k] * s;
      }
      pi_latent_E = rep_vector(0, P);
      pi_latent_E[A] = latent;
      for (p in 1:P) {
        pi_nat_eday[p] = sum(votes_kd[ : , p]) / n_pred;
      }
    }

    if (latent_path) {
      for (t in 1:G) {
        int e = grid_epoch[t];
        pi_latent[ : , t] = national_share(theta[ : , t], delta[e], w[e], exist_idx[t, 1:n_exist[t]]);
      }
    }
  }
}
