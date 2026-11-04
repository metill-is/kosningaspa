// Arm V2's forecast model (spec §6), its components switched by data. This file holds the
// core, step v2s-m0, in the reference forms: the forward sum-to-zero walk (§6.2), national
// polls through the vote-weighted national share (§6.3), industry and house bias constant
// over epochs with newcomers in contrast form (§6.4), one static kjordaemi deviation mapped
// through each epoch's M_e (§6.7), the kjordaemi polls and the closed elections' kjordaemi
// results (§6.6, §6.7), and the bounded concentrations (Decision 4). Every likelihood
// gathers its parties by identity through the data's idx_* lists, never by position. The
// data come from v2_stan_data() in R/harness/v2.R, and tests/harness/test-v2-stan.R tests
// the log density against the R reference R/harness/v2_reference.R. A component switch or
// a form other than 0 was refused until the step that built it (Stan/CLAUDE.md, switches).
// Step v2s-p1 builds walk_form = 1, the walk anchored at the closed elections (§6.9), the
// same model as the reference walk; R/harness/v2_equivalence.R checks that it is. Step v2s-m1
// builds epoch_bias = 1 (§6.4): the industry bias and each house's deviations walk across
// epochs, Annad's industry-bias steps with a scale of their own. Step v2s-m2 builds
// newcomer_pool = 1 (§6.4): the mean m_new and scale s_new of a newcomer's bias contrast are
// estimated, instead of fixed at 0 and 0.2023. Step v2s-m3 builds shock = 1 (§6.6): each
// closed election's results and the forecast take an election-day shock C(s_bar_E d .* z),
// one per election and shared by its kjordaemi, with d from the previous election's shares.
// Step v2s-m4 builds delta_walk = 1 (§6.7): the kjordaemi deviations walk across elections,
// delta_e = M_e(delta_{e-1} - c_{e-1} 1' + sqrt(delta_years_e / delta_years_bar) s_zeta .*
// zeta_e), with delta_{e-1} re-centred within each kjordaemi over the parties with results at
// election e - 1, and a newcomer's column fresh at its entry epoch. Step v2s-p2 builds
// delta_form = 1 (§6.7), the same delta walk held centred at the closed elections: each closed
// election's delta_e in its coordinates delta_e[2:K, 2:P], the parties with results there
// centred and stored relative to the result, the others non-centred, and the open epoch's
// step non-centred as before; R/harness/v2_equivalence.R checks that it is the same model.
// Step v2s-m5 builds fundamentals = 1 (§6.5): stage 1's mu_f enters as data, mu_f ~ N(x_c,
// tau_f) over S_f, x_c the log of the latent national share at E (no shock) centred over S_f,
// tau_f = sqrt(V_w (1 - w) / w) and V_w the walk's variance over weight_time days, the last
// campaign_days of them in the campaign. It adds no parameter.
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

  // The Cholesky factor of (B B')[rs, rs], with a positive diagonal, as R' from the thin QR
  // decomposition of B[rs]': forming B B' would square the condition number (proposal 3).
  matrix chol_gram_rows(matrix B, array[] int rs) {
    return qr_thin_R(B[rs]')';
  }

  // The prior mean of delta_e given delta_{e-1} in the delta walk (§6.7): delta_{e-1}
  // re-centred within each kjordaemi over the parties S with results at election e - 1, the
  // columns of the parties entering in epoch e (enter) 0, as a newcomer's column is fresh then.
  matrix delta_step_mean(matrix delta_prev, array[] int S, array[] int enter) {
    int K = rows(delta_prev);
    int P = cols(delta_prev);
    vector[K] c = rep_vector(0, K);
    for (i in 1:size(S)) {
      c += delta_prev[ : , S[i]];
    }
    matrix[K, P] m = delta_prev - rep_matrix(c / size(S), P);
    for (i in 1:size(enter)) {
      m[ : , enter[i]] = rep_vector(0, K);
    }
    return m;
  }

  // The column scales of the delta walk's step into epoch e: r s_zeta, r = sqrt(delta_years_e /
  // delta_years_bar), but s_delta0 for a party entering in epoch e (enter).
  vector delta_step_scale(real r, vector s_zeta, vector s_delta0, array[] int enter) {
    vector[rows(s_zeta)] s = r * s_zeta;
    for (i in 1:size(enter)) {
      s[enter[i]] = s_delta0[enter[i]];
    }
    return s;
  }

  // The Kronecker product of A and B: block (i, j) is A[i, j] B. The column-major vec of a
  // matrix X has covariance U kron V when X's columns have cross-covariances U[i, j] V, and the
  // product of two lower-triangular Cholesky factors is the Cholesky factor of U kron V.
  matrix kron(matrix A, matrix B) {
    int p = rows(B);
    int q = cols(B);
    matrix[rows(A) * p, cols(A) * q] C;
    for (i in 1:rows(A)) {
      for (j in 1:cols(A)) {
        C[((i - 1) * p + 1):(i * p), ((j - 1) * q + 1):(j * q)] = A[i, j] * B;
      }
    }
    return C;
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
  array[H] int<lower=1, upper=n_epoch> house_first_epoch;  // [H] epoch of each house's first national poll
  array[P] int<lower=0, upper=n_epoch> entry_epoch;   // [P] 0 for a party present at the window's start
  int<lower=0, upper=P> annad_idx;                    // Annad's index, 0 if none: its industry-bias steps scale by tau_mu_A

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
  // The fundamentals pseudo-observation (fundamentals = 1, §6.5) centres log pi_nat,E over the
  // parties of S_f, so it needs two of them at least, distinct, and each existing at E: outside
  // A_E pi_nat,E is 0. Its scale tau_f needs w in (0, 1) and a stretch off the campaign of
  // weight_time - campaign_days >= 0 days; the data block holds campaign_days >= 1.
  if (fundamentals) {
    if (n_sf < 2) {
      reject("forecast_v2: fundamentals = 1 needs at least two parties of S_f, since mu_f's mean is centred over them, but n_sf = ",
             n_sf);
    }
    for (i in 1:(n_sf - 1)) {
      for (j in (i + 1):n_sf) {
        if (sf_idx[i] == sf_idx[j]) {
          reject("forecast_v2: fundamentals = 1 needs the parties of S_f to be distinct, but sf_idx[", i, "] = sf_idx[", j,
                 "] = ", sf_idx[i]);
        }
      }
    }
    for (i in 1:n_sf) {
      if (exists[G, sf_idx[i]] == 0) {
        reject("forecast_v2: fundamentals = 1 needs every party of S_f to exist at E (exists[G, sf_idx[i]] = 1), but sf_idx[",
               i, "] = ", sf_idx[i], " does not");
      }
    }
    if (desired_weight <= 0 || desired_weight >= 1) {
      reject("forecast_v2: fundamentals = 1 needs desired_weight in (0, 1), since tau_f = sqrt(V_w (1 - w) / w), but desired_weight = ",
             desired_weight);
    }
    if (weight_time < campaign_days) {
      reject("forecast_v2: fundamentals = 1 needs weight_time to be at least campaign_days, since V_w counts weight_time - campaign_days days off the campaign, but weight_time = ",
             weight_time, " and campaign_days = ", campaign_days);
    }
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
  real m_new_fixed = 0;
  real s_new_fixed = 0.2023;

  vector[G - 1] sqrt_delta = sqrt(delta_days);
  vector[G - 1] campaign_v = to_vector(campaign);
  real n_pred = sum(N_open);

  // The anchored walk (walk_form = 1, §6.9): its anchors are the closed elections' dates. At
  // anchor j the present parties A_j are the result's (those with national votes, idx_res)
  // and the absent ones B_j the rest, both without the coordinate party and both as
  // coordinates c = 1..P - 1 of theta[2:P] (party c + 1). It needs every anchor in 2..G, in
  // date order, and the coordinate party in every result: z_walk's G - 1 - n_closed columns
  // and the shift by |A_j u {1}| below assume both.
  int n_anchor = walk_form * n_closed;
  for (j in 1:n_anchor) {
    if (grid_res[j] < 2 || grid_res[j] > G) {
      reject("forecast_v2: walk_form = 1 needs every grid_res in 2..G, but grid_res[", j, "] = ", grid_res[j]);
    }
    if (j > 1 && grid_res[j] <= grid_res[j - 1]) {
      reject("forecast_v2: walk_form = 1 needs grid_res strictly increasing, but grid_res[", j, "] = ", grid_res[j],
             " follows ", grid_res[j - 1]);
    }
    int coord = 0;
    for (i in 1:n_rep_res[j]) {
      if (idx_res[j, i] == 1) coord = 1;
    }
    if (coord == 0 || sum(y_res[j, : , 1]) == 0) {
      reject("forecast_v2: walk_form = 1 needs the coordinate party (index 1) to have national votes at every closed",
             " election, but it has none at closed election ", j);
    }
  }
  array[n_anchor] int n_pres = rep_array(0, n_anchor);
  array[n_anchor, P - 1] int pres_idx = rep_array(0, n_anchor, P - 1);
  array[n_anchor] int n_abs = rep_array(0, n_anchor);
  array[n_anchor, P - 1] int abs_idx = rep_array(0, n_anchor, P - 1);
  // Each anchor's present levels are stored relative to w_off, the result's national log
  // shares centred over its parties (A_j u {1}), so that anchor_w = 0 starts the walk at
  // each result rather than at equal shares. A constant shift: exact, no Jacobian.
  array[n_anchor] vector[P - 1] w_off = rep_array(rep_vector(0, P - 1), n_anchor);
  for (j in 1:n_anchor) {
    array[P] int present = rep_array(0, P);
    vector[n_rep_res[j]] l;
    for (i in 1:n_rep_res[j]) {
      present[idx_res[j, i]] = 1;
      l[i] = log(sum(y_res[j, : , idx_res[j, i]]));
    }
    l -= mean(l);
    for (i in 1:n_rep_res[j]) {
      if (idx_res[j, i] > 1) w_off[j][idx_res[j, i] - 1] = l[i];
    }
    for (c in 1:(P - 1)) {
      if (present[c + 1]) {
        n_pres[j] += 1;
        pres_idx[j, n_pres[j]] = c;
      } else {
        n_abs[j] += 1;
        abs_idx[j, n_abs[j]] = c;
      }
    }
  }
  // The column of z_walk for each date 2..G that is not an anchor, in date order; 0 at an
  // anchor and at date 1. With walk_form = 0 it is t - 1.
  array[G] int zcol = rep_array(0, G);
  {
    array[G] int is_anchor = rep_array(0, G);
    for (j in 1:n_anchor) {
      is_anchor[grid_res[j]] = 1;
    }
    int c = 0;
    for (t in 2:G) {
      if (is_anchor[t] == 0) {
        c += 1;
        zcol[t] = c;
      }
    }
  }

  // The delta walk held centred at the closed elections (delta_form = 1, §6.7). At closed
  // election e, delta_e is held in its coordinates D_e = delta_e[2:K, 2:P]: kjordaemi 1 and
  // party 1 follow from M_e's two constraints, delta_e 1 = 0 and w_e' delta_e = 0. The present
  // parties are the result's (idx_res) and the absent ones the rest, both without party 1 and
  // both as coordinates c = 1..P - 1 (party c + 1). The form needs the walk, since a static
  // delta has fewer free values than the closed elections' coordinates; party 1 in every
  // result, which the shift by |Pr| + 1 below assumes; w_e1 > 0, since kjordaemi 1 is solved
  // from the others; and each closed election's w summing to 1, which makes M_e a projection,
  // within 1e-9, since the data's sums are off by rounding.
  int n_dc = delta_form * n_closed;
  if (delta_form && delta_walk == 0) {
    reject("forecast_v2: delta_form = 1 needs delta_walk = 1: a static delta has no centred form");
  }
  for (e in 1:n_dc) {
    int coord = 0;
    for (i in 1:n_rep_res[e]) {
      if (idx_res[e, i] == 1) coord = 1;
    }
    if (coord == 0) {
      reject("forecast_v2: delta_form = 1 needs the coordinate party (index 1) among the parties with results at every",
             " closed election, but it is not at closed election ", e);
    }
    if (w[e, 1] <= 0) {
      reject("forecast_v2: delta_form = 1 needs w[e, 1] > 0 at every closed election e, but w[", e, ", 1] = ", w[e, 1]);
    }
    if (abs(sum(w[e]) - 1) >= 1e-9) {
      reject("forecast_v2: delta_form = 1 needs each closed election's w to sum to 1 within 1e-9, but row ", e,
             " of w sums to ", sum(w[e]));
    }
  }
  array[n_dc] int d_n_pres = rep_array(0, n_dc);
  array[n_dc, P - 1] int d_pres_idx = rep_array(0, n_dc, P - 1);
  array[n_dc] int d_n_abs = rep_array(0, n_dc);
  array[n_dc, P - 1] int d_abs_idx = rep_array(0, n_dc, P - 1);
  array[n_dc, P - 1] int d_order;                     // the absent coordinates, then the present ones
  // Each closed election's present coordinates are stored relative to d_off, the coordinates
  // of A_e log(y_res[e, , S_e] + 1/2) C over the result's parties S_e, its kjordaemi log shares
  // through M_e (the 1/2 guards a zero count), so that delta_coord = 0 starts each result's
  // rows at its kjordaemi deviations. A data constant: exact, no Jacobian.
  array[n_dc] matrix[K - 1, P - 1] d_off = rep_array(rep_matrix(0, K - 1, P - 1), n_dc);
  // L_Psi[e] = chol((A_e A_e')[2:K, 2:K]), A_e = I - 1 w_e': the kjordaemi factor of the
  // covariance of D_e given delta_{e-1}.
  array[n_dc] matrix[K - 1, K - 1] L_Psi;
  matrix[P, P] I_P = identity_matrix(P);
  for (e in 1:n_dc) {
    array[n_rep_res[e]] int S = idx_res[e, 1:n_rep_res[e]];
    array[P] int present = rep_array(0, P);
    matrix[K, n_rep_res[e]] l;
    for (i in 1:n_rep_res[e]) {
      present[S[i]] = 1;
      for (k in 1:K) {
        l[k, i] = log(y_res[e, k, S[i]] + 0.5);
      }
    }
    l = kjordaemi_map(l, w[e]);
    for (i in 1:n_rep_res[e]) {
      if (S[i] > 1) d_off[e][ : , S[i] - 1] = l[2:K, i];
    }
    for (c in 1:(P - 1)) {
      if (present[c + 1]) {
        d_n_pres[e] += 1;
        d_pres_idx[e, d_n_pres[e]] = c;
      } else {
        d_n_abs[e] += 1;
        d_abs_idx[e, d_n_abs[e]] = c;
      }
    }
    d_order[e] = append_array(d_abs_idx[e, 1:d_n_abs[e]], d_pres_idx[e, 1:d_n_pres[e]]);
    L_Psi[e] = chol_gram_rows(identity_matrix(K) - rep_matrix(w[e], K), linspaced_int_array(K - 1, 2, K));
  }

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
  // The names, types and order of V2_CORE_PARS in R/harness/v2_reference.R, so the tests can
  // move a point between the reference and Stan without renaming or reordering.
  sum_to_zero_vector[P] theta1;                       // the state at the first grid date
  array[walk_form * n_closed] vector[P - 1] anchor_w; // walk_form = 1: each anchor's levels in coordinates 2:P
  matrix[P - 1, G - 1 - walk_form * n_closed] z_walk; // the walk's innovations in coordinates 2:P
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
  array[1 + delta_walk * (n_epoch - 1) - delta_form * n_closed] matrix[K, P] delta_raw;  // zeta: one, one per epoch, or the open epoch's
  vector[P] s_delta0_raw;                             // log s_delta0 = mu_sd + tau_sd s_delta0_raw
  real mu_sd;
  real<lower=0> tau_sd;
  array[J] sum_to_zero_vector[P] nu;                  // kjordaemi polls' levels
  // The epoch bias (epoch_bias = 1, §6.4), each size 0 with epoch_bias = 0. Column e - 1 holds
  // the step into epoch e = 2..n_epoch.
  matrix[P, epoch_bias * (n_epoch - 1)] mu_step_raw;  // the industry bias's steps xi_e
  array[H] matrix[P, epoch_bias * (n_epoch - 1)] house_step_raw;  // each house's steps zeta_{e,h}
  array[epoch_bias] real<lower=0> tau_mu;             // the industry bias's step scale
  array[epoch_bias] real<lower=0> tau_mu_A;           // Annad's
  array[epoch_bias] real<lower=0> tau_house;          // the houses' step scale
  // The newcomer pool (newcomer_pool = 1, §6.4), each size 0 with newcomer_pool = 0.
  array[newcomer_pool] real m_new;                    // the mean of a newcomer's bias contrast
  array[newcomer_pool] real<lower=0> s_new;           // its scale
  // The election-day shock (shock = 1, §6.6), each size 0 with shock = 0. Column e holds
  // closed election e's z.
  matrix[P, shock * n_closed] z_shock;                // the shocks' standard normals z_e
  array[shock] real<lower=0> s_bar_E;                 // the shock's scale
  // The delta walk (delta_walk = 1, §6.7), each size 0 with delta_walk = 0; its steps'
  // zeta_2..zeta_n_epoch are delta_raw[2:n_epoch], or with delta_form = 1 the open epoch's
  // step's zeta delta_raw[1].
  vector<lower=0>[delta_walk * P] s_zeta_raw;         // s_zeta = s_bar_zeta s_zeta_raw
  array[delta_walk] real<lower=0> s_bar_zeta;         // the walk's scale
  // The delta walk held centred (delta_form = 1, §6.7), size 0 with delta_form = 0: each closed
  // election's coordinates, W_B in the absent parties' columns and R in the present ones'.
  array[delta_form * n_closed] matrix[K - 1, P - 1] delta_coord;
}

transformed parameters {
  matrix[P, G] theta;                                 // the latent national log-ratio state
  array[n_epoch] vector[P] mu;                        // industry bias by epoch
  array[n_epoch, H] vector[P] gamma;                  // house bias by epoch
  array[n_epoch] matrix[K, P] delta;                  // kjordaemi deviations by epoch
  matrix[n_closed, P] eps = rep_matrix(0, n_closed, P);  // the closed elections' shocks, 0 with shock = 0
  vector[H] log_phi = phi_cap - (phi_cap - phi_init) * exp(log_phi_u);
  real log_phi_K = phi_K_cap - (phi_K_cap - phi_K_init) * exp(log_phi_K_u);
  real log_kappa = kappa_cap - (kappa_cap - kappa_init) * exp(log_kappa_u);
  vector[P] s_delta0 = exp(mu_sd + tau_sd * s_delta0_raw);
  vector[delta_walk * P] s_zeta;                      // the walk's per-party step scale
  if (delta_walk) s_zeta = s_bar_zeta[1] * s_zeta_raw;

  // §6.2: coordinates 2:P step by sqrt(Delta_i) s_i L_walk z_i and party 1 by minus their
  // sum, so every theta_t sums to 0 and each step's covariance is Delta_i s_i^2 C Sigma C.
  // L_walk is chol((C Sigma C)[2:P, 2:P]), taken as R' from the thin QR decomposition of
  // B' with B the rows 2:P of C diag(sigma) L_Omega, since B B' = (C Sigma C)[2:P, 2:P]:
  // forming B B' would square the condition number (proposal 3).
  {
    matrix[P - 1, P] B = walk_rows(sigma, L_Omega);
    matrix[P - 1, P - 1] L_walk = qr_thin_R(B')';
    vector[G - 1] scale = sqrt_delta .* (1 + tau_camp * campaign_v);
    if (walk_form == 0) {
      matrix[P - 1, G - 1] step = diag_post_multiply(L_walk * z_walk, scale);
      theta[ : , 1] = theta1;
      for (i in 1:(G - 1)) {
        theta[2:P, i + 1] = theta[2:P, i] + step[ : , i];
        theta[1, i + 1] = theta[1, i] - sum(step[ : , i]);
      }
    } else {
      // §6.9, the same walk anchored at the closed elections, forward from theta1. With
      // v[t] = Delta s^2 of the step from t - 1 to t and cv its running total, the walk from
      // the previous anchor (or date 1) to anchor a has covariance V Q, V = cv[a] - cv[prev]
      // and Q = (C Sigma C)[2:P, 2:P]. At each anchor, the absent parties' coordinates are
      // non-centred from the previous anchor, theta_B = theta_prev,B + sqrt(V) chol(Q_BB) w_B,
      // and the present ones centred and shifted, theta_A = w_off_A + w_A - sum(theta_B) /
      // |A u {1}|, so that the result's softmax over A u {1} does not depend on theta_B; the
      // model block gives them their conditional normal. Between anchors (and from date 1 to
      // the first) the path is the exact conditional of the walk given both ends, a
      // non-centred Brownian bridge; after the last anchor it is the forward walk.
      vector[G] v = append_row(0, square(scale));
      vector[G] cv = cumulative_sum(v);
      matrix[P - 1, G - 1 - n_anchor] Lz = L_walk * z_walk;
      matrix[P - 1, G] th;
      th[ : , 1] = theta1[2:P];
      int prev = 1;
      for (j in 1:n_anchor) {
        int a = grid_res[j];
        array[n_pres[j]] int pr = pres_idx[j, 1:n_pres[j]];
        if (n_abs[j] > 0) {
          array[n_abs[j]] int ab = abs_idx[j, 1:n_abs[j]];
          th[ab, a] = th[ab, prev] + sqrt(cv[a] - cv[prev]) * chol_gram_rows(B, ab) * anchor_w[j][ab];
          th[pr, a] = w_off[j][pr] + anchor_w[j][pr] - sum(th[ab, a]) / (n_pres[j] + 1);
        } else {
          th[ : , a] = w_off[j] + anchor_w[j];
        }
        for (t in (prev + 1):(a - 1)) {
          real R = cv[a] - cv[t - 1];
          th[ : , t] = th[ : , t - 1] + (v[t] / R) * (th[ : , a] - th[ : , t - 1])
                       + sqrt(v[t] * (cv[a] - cv[t]) / R) * Lz[ : , zcol[t]];
        }
        prev = a;
      }
      for (t in (prev + 1):G) {
        th[ : , t] = th[ : , t - 1] + scale[t - 1] * Lz[ : , zcol[t]];
      }
      theta[2:P, : ] = th;
      theta[1, : ] = -rep_row_vector(1, P - 1) * th;
    }
  }

  // §6.4, the industry bias: mu_1, then with epoch_bias = 1 mu_e = mu_{e-1} + C(tau .* xi_e),
  // tau_p = tau_mu but tau_mu_A for Annad; with epoch_bias = 0 no step, so mu is constant over
  // epochs. From its entry epoch on, a newcomer's bias is the mean of the current biases of
  // the parties existing before that epoch, plus m_new + s_new eta; then mu_e is re-centred,
  // and after it the newcomer walks with the rest. With newcomer_pool = 1 m_new and s_new are
  // the parameters, which the newcomers' entries inform; with newcomer_pool = 0 they are fixed
  // at 0 and 0.2023.
  {
    vector[P] tau = rep_vector(0, P);
    if (epoch_bias) {
      tau = rep_vector(tau_mu[1], P);
      if (annad_idx > 0) tau[annad_idx] = tau_mu_A[1];
    }
    real mn = m_new_fixed;
    real sn = s_new_fixed;
    if (newcomer_pool) {
      mn = m_new[1];
      sn = s_new[1];
    }
    vector[P] m = mu1;
    for (e in 1:n_epoch) {
      if (epoch_bias && e > 1) {
        vector[P] s = tau .* mu_step_raw[ : , e - 1];
        m += s - mean(s);
      }
      if (n_enter[e] > 0) {
        real base = mean(m[before_idx[e, 1:n_before[e]]]);
        for (i in 1:n_enter[e]) {
          int p = enter_idx[e, i];
          m[p] = base + mn + sn * new_raw[new_pos[p]];
        }
      }
      m -= mean(m);
      mu[e] = m;
    }
  }

  // gamma_{e,h} = mu_e + d_{e,h}, with d_{e,h} = C x_{e,h} for the house's levels x. They start
  // at sigma_gamma eps_h and stay there up to the house's first epoch; with epoch_bias = 1 they
  // walk after it, x_{e,h} = x_{e-1,h} + tau_house zeta_{e,h}, so d_{e,h} = d_{e-1,h} +
  // C(tau_house zeta_{e,h}), and at a later entry epoch a newcomer's level restarts at
  // sigma_gamma eps_{h,p} instead of carrying prior-only drift. With epoch_bias = 0, d_h =
  // C(sigma_gamma eps_h) in every epoch, so a newcomer's slot is its restarted deviation.
  for (h in 1:H) {
    row_vector[P] x = sigma_gamma * house_raw[h];
    for (e in 1:n_epoch) {
      if (epoch_bias && e > house_first_epoch[h]) {
        x += tau_house[1] * house_step_raw[h][ : , e - 1]';
        for (i in 1:n_enter[e]) {
          int p = enter_idx[e, i];
          x[p] = sigma_gamma * house_raw[h, p];
        }
      }
      gamma[e, h] = (x - mean(x))' + mu[e];
    }
  }

  // §6.7: with delta_walk = 0, static, delta_e = M_e(s_delta0 .* zeta), one zeta for every
  // epoch. With 1, the walk: delta_1 = M_1(s_delta0 .* zeta_1) and delta_e = M_e(delta_{e-1} -
  // c_{e-1} 1' + sqrt(delta_years_e / delta_years_bar) s_zeta .* zeta_e), where c_{e-1,k} is the
  // unweighted mean of delta_{e-1}[k, S] over the parties S with results at election e - 1,
  // except that a newcomer's column in its entry epoch e > 1 is s_delta0,p zeta_{e,.,p}; before
  // it the column walks on the prior. M_e removes any constant within a kjordaemi, so the
  // re-centring is the plain walk wherever no party enters. At an entry it keeps the directions
  // the likelihood cannot see before then, the newcomer's earlier column and an offset shared by
  // the parties S, out of the contrasts: the newcomer enters against the existing parties' mean.
  // So delta_e = M_e(m_e + zeta_e diag(s_e)), with m_e = 0 and s_e = s_delta0 at e = 1.
  //
  // With delta_form = 1 each closed election's delta_e is built from its coordinates D_e =
  // delta_e[2:K, 2:P] instead: delta_e[k, 1] = -sum_p D_e[k, p] for k >= 2, and row 1 = -sum_k
  // w_ek delta_e[k, .] / w_e1, so delta_e lies in M_e's range. Given delta_{e-1}, vec(D_e) ~
  // N(vec(M), Phi kron Psi), M the coordinates of M_e(m_e), Phi = (C S_e C)[2:P, 2:P], S_e =
  // diag(s_e^2), and Psi = (A_e A_e')[2:K, 2:K]. With L the Cholesky factor of Phi, the absent
  // coordinates first, the absent ones are non-centred, D_B = M_B + L_Psi W_B L_BB', and the
  // present ones centred and shifted, D_Pr = d_off_Pr + R - rowSums(D_B) / (|Pr| + 1), so that
  // every row k >= 2 of delta_e over the result's parties moves by one constant with D_B and
  // the result's softmax does not depend on W_B; the model block gives D_Pr its conditional
  // normal. The open epoch's step stays non-centred.
  if (delta_walk == 0) {
    matrix[K, P] X = diag_post_multiply(delta_raw[1], s_delta0);
    for (e in 1:n_epoch) {
      delta[e] = kjordaemi_map(X, w[e]);
    }
  } else {
    for (e in 1:n_epoch) {
      matrix[K, P] m = rep_matrix(0, K, P);
      vector[P] s = s_delta0;
      if (e > 1) {
        m = delta_step_mean(delta[e - 1], idx_res[e - 1, 1:n_rep_res[e - 1]], enter_idx[e, 1:n_enter[e]]);
        s = delta_step_scale(sqrt(delta_years[e] / delta_years_bar), s_zeta, s_delta0, enter_idx[e, 1:n_enter[e]]);
      }
      if (e > n_dc) {
        delta[e] = kjordaemi_map(m + diag_post_multiply(delta_raw[e - n_dc], s), w[e]);
      } else {
        int na = d_n_abs[e];
        int np = d_n_pres[e];
        array[np] int pr = d_pres_idx[e, 1:np];
        matrix[K - 1, P - 1] D;
        if (na > 0) {
          array[na] int ab = d_abs_idx[e, 1:na];
          matrix[K - 1, P - 1] M = block(kjordaemi_map(m, w[e]), 2, 2, K - 1, P - 1);
          matrix[P - 1, P - 1] L = chol_gram_rows(walk_rows(s, I_P), d_order[e]);
          D[ : , ab] = M[ : , ab] + L_Psi[e] * delta_coord[e][ : , ab] * L[1:na, 1:na]';
          D[ : , pr] = d_off[e][ : , pr] + delta_coord[e][ : , pr] - rep_matrix(D[ : , ab] * rep_vector(1, na) / (np + 1), np);
        } else {
          D = d_off[e] + delta_coord[e];
        }
        delta[e, 2:K, 2:P] = D;
        delta[e, 2:K, 1] = -D * rep_vector(1, P - 1);
        delta[e, 1] = -(w[e, 2:K] * delta[e, 2:K]) / w[e, 1];
      }
    }
  }

  // §6.6: eps_e = C(s_bar_E d_e .* z_e), one shock per closed election, shared by its
  // kjordaemi; d_e is row e of d_shock, from the shares of the election before it.
  if (shock) {
    for (e in 1:n_closed) {
      row_vector[P] x = s_bar_E[1] * d_shock[e] .* z_shock[ : , e]';
      eps[e] = x - mean(x);
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

  // Results (§6.6): y_ek ~ DirMult(phi_K softmax over S of (theta_te + eps_e + delta_{e,k})),
  // eps_e = 0 with shock = 0.
  for (e in 1:n_closed) {
    int t = grid_res[e];
    int de = grid_epoch[t];
    array[n_rep_res[e]] int S = idx_res[e, 1:n_rep_res[e]];
    for (k in 1:K) {
      y_res[e, k, S] ~ dirichlet_multinomial(phi_K * softmax(theta[S, t] + eps[e, S]' + delta[de][k, S]'));
    }
  }

  // The fundamentals pseudo-observation (fundamentals = 1, §6.5): mu_f ~ N(x_c, tau_f), x = log
  // pi_nat,E[S_f] with pi_nat,E the latent national share at E (theta_E with the open epoch's
  // delta and w, over A_E, and no shock), x_c = x - mean(x), tau_f = sqrt(V_w (1 - w) / w) and
  // V_w = diag(C Sigma C')[S_f] ((weight_time - campaign_days) + campaign_days (1 + tau_camp)^2),
  // the walk's variance over weight_time days, the last campaign_days in the campaign: the
  // weight w at weight_time days is relative to that variance alone. A likelihood term on data,
  // so no Jacobian; tau_f depends on sigma, Omega and tau_camp, so the sampling statement keeps
  // its -log tau_f.
  if (fundamentals) {
    vector[P] pi_E = national_share(theta[ : , G], delta[n_epoch], w[n_epoch], exist_idx[G, 1:n_exist[G]]);
    vector[n_sf] x = log(pi_E[sf_idx]);
    vector[n_sf] V_w = walk_var(sigma, L_Omega)[sf_idx] * ((weight_time - campaign_days) + campaign_days * square(1 + tau_camp));
    mu_f ~ normal(x - mean(x), sqrt(V_w * (1 - desired_weight) / desired_weight));
  }

  // Priors (§6.10, Decisions 1, 2 and 4). A normal on a sum_to_zero_vector is on every element.
  theta1 ~ normal(0, 2);
  to_vector(z_walk) ~ std_normal();
  // The anchored walk (walk_form = 1): at each anchor, N(0, 1) on the absent parties' w_B and
  // the present parties' conditional normal given theta_B and the previous anchor (or date
  // 1), theta_A ~ N(theta_prev,A + sqrt(V) L_AB w_B, V L_AA L_AA'), from the Cholesky factor
  // L of Q with the absent coordinates first (L_BB = chol(Q_BB)). With every party present,
  // theta_a ~ N(theta_prev, V Q). The bridges and the forward steps are N(0, 1) on z_walk.
  if (walk_form) {
    matrix[P - 1, P] B = walk_rows(sigma, L_Omega);
    vector[G] cv = cumulative_sum(append_row(0, square(sqrt_delta .* (1 + tau_camp * campaign_v))));
    matrix[P - 1, G] th = theta[2:P, : ];
    int prev = 1;
    for (j in 1:n_anchor) {
      int a = grid_res[j];
      int na = n_abs[j];
      real sV = sqrt(cv[a] - cv[prev]);
      if (na > 0) {
        array[na] int ab = abs_idx[j, 1:na];
        array[n_pres[j]] int pr = pres_idx[j, 1:n_pres[j]];
        matrix[P - 1, P - 1] L = chol_gram_rows(B, append_array(ab, pr));
        anchor_w[j][ab] ~ std_normal();
        th[pr, a] ~ multi_normal_cholesky(th[pr, prev] + sV * L[(na + 1):(P - 1), 1:na] * anchor_w[j][ab],
                                          sV * L[(na + 1):(P - 1), (na + 1):(P - 1)]);
      } else {
        th[ : , a] ~ multi_normal_cholesky(th[ : , prev], sV * qr_thin_R(B')');
      }
      prev = a;
    }
  }
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
  // The epoch bias (epoch_bias = 1): N(0, 1) steps and half-normal scales, all size 0 with
  // epoch_bias = 0.
  to_vector(mu_step_raw) ~ std_normal();
  for (h in 1:H) {
    to_vector(house_step_raw[h]) ~ std_normal();
  }
  tau_mu ~ normal(0, prior_tau_mu);
  tau_mu_A ~ normal(0, 0.3);
  tau_house ~ normal(0, 0.1);
  // The newcomer pool (newcomer_pool = 1): m_new ~ N(0, 0.3) and s_new ~ half-N(0, 0.3), both
  // size 0 with newcomer_pool = 0.
  m_new ~ normal(0, 0.3);
  s_new ~ normal(0, 0.3);
  // The election-day shock (shock = 1): z ~ N(0, 1) and s_bar_E ~ half-N(0, prior_s_bar_E),
  // both size 0 with shock = 0.
  to_vector(z_shock) ~ std_normal();
  s_bar_E ~ normal(0, prior_s_bar_E);
  // The delta walk (delta_walk = 1): N(0, 1) zeta for epochs 2..n_epoch, s_zeta_raw ~
  // half-N(0, 1) and s_bar_zeta ~ half-N(0, prior_s_bar_zeta), all size 0 with delta_walk = 0.
  for (e in 2:size(delta_raw)) {
    to_vector(delta_raw[e]) ~ std_normal();
  }
  s_zeta_raw ~ std_normal();
  s_bar_zeta ~ normal(0, prior_s_bar_zeta);
  // The delta walk held centred (delta_form = 1): at each closed election, N(0, 1) on the absent
  // parties' W_B and the present parties' conditional normal given D_B and delta_{e-1},
  // vec(D_Pr) ~ N(vec(M_Pr + L_Psi W_B L_PrB'), (L_PrPr L_PrPr') kron Psi), from the Cholesky
  // factor L of Phi with the absent coordinates first; with every party present, vec(D_e) ~
  // N(vec(M), Phi kron Psi). Its normaliser's -(K - 1) sum log diag(L_PrPr) depends on the
  // scales and stays in the target. D_Pr is R shifted, so R has no Jacobian.
  for (e in 1:n_dc) {
    int na = d_n_abs[e];
    int np = d_n_pres[e];
    array[np] int pr = d_pres_idx[e, 1:np];
    matrix[K, P] m = rep_matrix(0, K, P);
    vector[P] s = s_delta0;
    if (e > 1) {
      m = delta_step_mean(delta[e - 1], idx_res[e - 1, 1:n_rep_res[e - 1]], enter_idx[e, 1:n_enter[e]]);
      s = delta_step_scale(sqrt(delta_years[e] / delta_years_bar), s_zeta, s_delta0, enter_idx[e, 1:n_enter[e]]);
    }
    matrix[K - 1, P - 1] M = block(kjordaemi_map(m, w[e]), 2, 2, K - 1, P - 1);
    matrix[K - 1, P - 1] D = block(delta[e], 2, 2, K - 1, P - 1);
    matrix[P - 1, P - 1] L = chol_gram_rows(walk_rows(s, I_P), d_order[e]);
    matrix[np, np] L_PrPr = L[(na + 1):(P - 1), (na + 1):(P - 1)];
    matrix[K - 1, np] mean_pr = M[ : , pr];
    if (na > 0) {
      array[na] int ab = d_abs_idx[e, 1:na];
      to_vector(delta_coord[e][ : , ab]) ~ std_normal();
      mean_pr += L_Psi[e] * delta_coord[e][ : , ab] * L[(na + 1):(P - 1), 1:na]';
    }
    to_vector(D[ : , pr]) ~ multi_normal_cholesky(to_vector(mean_pr), kron(L_PrPr, L_Psi[e]));
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
        vector[n_rep_res[e]] alpha = phi_K * softmax(theta[S, t] + eps[e, S]' + delta[de][k, S]');
        log_lik[N_nat + N_kd + (e - 1) * K + k] = dirichlet_multinomial_lpmf(y_res[e, k, S] | alpha);
        y_rep_res[e, k, S] = dirichlet_multinomial_rng(alpha, sum(y_res[e, k, S]));
      }
    }

    if (draw_forecast) {
      int nA = n_exist[G];
      array[nA] int A = exist_idx[G, 1:nA];
      vector[nA] latent = rep_vector(0, nA);
      // E's shock (§6.6), drawn after y_rep_res and only with shock = 1, so that shock = 0
      // draws what the core draws: eps_E = C(s_bar_E d_E .* z), d_E the open row of d_shock.
      // The latent share at E stays shock-free.
      vector[P] eps_E = rep_vector(0, P);
      if (shock) {
        vector[P] x = s_bar_E[1] * d_shock[n_epoch]' .* to_vector(normal_rng(rep_vector(0, P), 1));
        eps_E = x - mean(x);
      }
      for (k in 1:K) {
        vector[nA] s = softmax(theta[A, G] + delta[n_epoch][k, A]');
        votes_kd[k] = rep_array(0, P);
        votes_kd[k, A] = dirichlet_multinomial_rng(phi_K * softmax(theta[A, G] + eps_E[A] + delta[n_epoch][k, A]'), N_open[k]);
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
