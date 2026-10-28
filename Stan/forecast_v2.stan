// Arm V2's forecast model (spec §6), its components switched by data. This file holds the
// core, step v2s-m0, in the reference forms: the forward sum-to-zero walk (§6.2), national
// polls through the vote-weighted national share (§6.3), industry and house bias constant
// over epochs with newcomers in contrast form (§6.4), one static kjordaemi deviation mapped
// through each epoch's M_e (§6.7), the kjordaemi polls and the closed elections' kjordaemi
// results (§6.6, §6.7), and the bounded concentrations (Decision 4). Every likelihood
// gathers its parties by identity through the data's idx_* lists, never by position. The
// data come from v2_stan_data() in R/harness/v2.R, and tests/harness/test-v2-stan.R tests
// the log density against the R reference R/harness/v2_reference.R. A component switch or
// a form other than 0 is refused until the step that builds it (Stan/CLAUDE.md, switches).
functions {
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
  vector<lower=0>[n_epoch] delta_years;               // [n_epoch] years from the previous election (0 for epoch 1)

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

  // Houses and parties (§6.4)
  array[H] int<lower=1, upper=n_epoch> house_first_epoch;  // [H] epoch of each house's first poll
  array[P] int<lower=0, upper=n_epoch> entry_epoch;   // [P] 0 for a party present at the window's start

  // Shock scaling, the forecast and the outputs (§6.6, §6.8)
  matrix<lower=0>[n_epoch, P] d_shock;                // [n_epoch, P] shock scaling
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
  real<lower=0> delta_years_bar;                      // years

  // Prior locations and scales (Tier 3b's and Decision 1's)
  real<lower=0> prior_tau_mu;
  real<lower=0> prior_s_bar_E;
  real<lower=0> prior_s_bar_zeta;
  real<lower=0> prior_sd_log_kappa;
  real prior_mu_phi;
  real<lower=0> prior_sd_mu1;
  real<lower=0> prior_rate_sigma_gamma;

  // Switches: five components, then two parameterisations (V2_SWITCHES)
  int<lower=0, upper=1> epoch_bias;
  int<lower=0, upper=1> newcomer_pool;
  int<lower=0, upper=1> shock;
  int<lower=0, upper=1> delta_walk;
  int<lower=0, upper=1> fundamentals;
  int<lower=0, upper=1> walk_form;
  int<lower=0, upper=1> delta_form;
}

transformed data {
  if (epoch_bias || newcomer_pool || shock || delta_walk || fundamentals || walk_form || delta_form) {
    reject("forecast_v2: only the core in the reference forms is built; every switch must be 0");
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

  // The newcomer contrast with newcomer_pool = 0 (§6.4): m_new = 0, and s_new the median of
  // half-N(0, 0.3) to four digits.
  real m_new = 0;
  real s_new = 0.2023;

  vector[G - 1] sqrt_delta = sqrt(delta_days);
  vector[G - 1] campaign_v = to_vector(campaign);
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
}

parameters {
  // Ruling T3a: the names, types and order of V2_CORE_PARS in R/harness/v2_reference.R.
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
  array[1] matrix[K, P] delta_raw;                    // static delta: one zeta for every epoch
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
    matrix[P, P] CM = diag_pre_multiply(sigma, L_Omega);
    CM -= rep_matrix(rep_row_vector(1, P) * CM / P, P);
    matrix[P - 1, P - 1] L_walk = qr_thin_R(CM[2:P]')';
    vector[G - 1] scale = sqrt_delta .* (1 + tau_camp * campaign_v);
    matrix[P - 1, G - 1] step = diag_post_multiply(L_walk * z_walk, scale);
    theta[ : , 1] = theta1;
    for (i in 1:(G - 1)) {
      theta[2:P, i + 1] = theta[2:P, i] + step[ : , i];
      theta[1, i + 1] = theta[1, i] - sum(step[ : , i]);
    }
  }

  // §6.4 with no drift over epochs: from its entry epoch on, a newcomer's bias is the mean of
  // the current biases of the parties existing before that epoch, plus m_new + s_new eta;
  // then mu_e is re-centred.
  {
    vector[P] m = mu1;
    for (e in 1:n_epoch) {
      if (n_enter[e] > 0) {
        real base = mean(m[before_idx[e, 1:n_before[e]]]);
        for (i in 1:n_enter[e]) {
          int p = enter_idx[e, i];
          m[p] = base + m_new + s_new * new_raw[new_pos[p]];
        }
      }
      m -= mean(m);
      mu[e] = m;
    }
  }

  // gamma_{e,h} = mu_e + d_h, d_h = C(sigma_gamma eps_h): one eps per house and the same d_h
  // in every epoch, so a newcomer's slot is its restarted deviation.
  for (h in 1:H) {
    row_vector[P] d = sigma_gamma * house_raw[h];
    d -= mean(d);
    for (e in 1:n_epoch) {
      gamma[e, h] = d' + mu[e];
    }
  }

  // §6.7, static: delta_e = M_e(s_delta0 .* zeta), one zeta for every epoch.
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

  // National polls (§6.3): y_n ~ DirMult(phi_h normalise(pi_nat[S] .* exp(gamma_{e,h}[S]))).
  for (n in 1:N_nat) {
    int t = grid_nat[n];
    int e = grid_epoch[t];
    array[n_rep_nat[n]] int S = idx_nat[n, 1:n_rep_nat[n]];
    vector[P] pi = national_share(theta[ : , t], delta[e], w[e], exist_idx[t, 1:n_exist[t]]);
    y_nat[n, S] ~ dirichlet_multinomial(national_alpha(pi, gamma[e, house_nat[n]], phi[house_nat[n]], S));
  }

  // Kjordaemi polls (§6.7): y_jk ~ DirMult(kappa phi_h softmax over S of (nu_j + delta_{e,k})).
  for (r in 1:N_kd) {
    int e = grid_epoch[grid_kd[r]];
    array[n_rep_kd[r]] int S = idx_kd[r, 1:n_rep_kd[r]];
    y_kd[r, S] ~ dirichlet_multinomial(kappa * phi[house_kd[r]] * softmax(nu[poll_kd[r]][S] + delta[e][kd_kd[r], S]'));
  }

  // Results (§6.6, no shock in the core): y_ek ~ DirMult(phi_K softmax over S of (theta_te + delta_{e,k})).
  for (e in 1:n_closed) {
    int t = grid_res[e];
    int de = grid_epoch[t];
    array[n_rep_res[e]] int S = idx_res[e, 1:n_rep_res[e]];
    for (k in 1:K) {
      y_res[e, k, S] ~ dirichlet_multinomial(phi_K * softmax(theta[S, t] + delta[de][k, S]'));
    }
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
