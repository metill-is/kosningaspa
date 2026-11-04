// Reference-invariant polling smoother — per-party RW volatility WITHOUT the
// hyper-scale funnel, full cross-party correlation retained (variant v4).
//
// PARAMETERISATION (28 Oct 2026). What NUTS samples follows the election forecast's
// (Stan/CLAUDE.md, Parameterisation); the posterior is the one described below, apart
// from the bound on log phi. The earlier form drew the walk forward from its first date
// with non-centred innovations. Each election result (~200,000 votes) then pinned a sum
// of every earlier innovation, a dense stiff direction that a diagonal metric can only
// meet with a small step: from 2016 (four results) the step size fell to 0.0022 and every
// transition reached the depth-11 cap, which is why the live fit started at the 2021
// election. Now:
//   * The walk lives on the sum-to-zero subspace, so it is written in the levels of the
//     P - 1 parties other than a reference party that every election result reports
//     (ref_party, the last such party; its level is minus their sum). Its step covariance
//     per day is the reference-free block of C S C, S = diag(sigma) Omega diag(sigma), C
//     the centring matrix: v4's centred innovations, without the direction per step that
//     the centring discarded and nothing but the prior saw.
//   * Election dates are anchors: centred parameters, stored relative to the levels that
//     reproduce the result, so that init 0 starts there. The last anchor is the root;
//     each earlier anchor is centred on the next one, and a party its result does not
//     report is non-centred given the next anchor.
//   * Between anchors the path is a non-centred Brownian bridge. Before the first anchor
//     it is a backward non-centred walk, and its first date carries the start prior
//     (the former beta0 ~ normal(0, 2)); after the last anchor, a forward non-centred walk.
//   * log phi is centred and bounded above at phi_cap = 20, as in the election forecast.
// R/sampler_check/equivalence_polling_watch.R checks the posterior point by point.
//
// PROBLEM (v2): the per-party innovation scales were drawn from a log-normal
// HIERARCHY,
//     sigma[p] = exp(mu_log_sigma + tau_log_sigma * log_sigma_raw[p]).
// On P=10 data the empirical per-party sigmas span only ~1.4x (0.0126-0.018),
// so the cross-party spread tau_log_sigma is very weakly identified: its
// posterior has substantial mass near 0. As tau_log_sigma -> 0 the directions
// log_sigma_raw[p] become unconstrained by the prior product
// tau_log_sigma * log_sigma_raw[p], producing a Neal funnel between the
// hyper-scale and its raw deviations. This funnel is intrinsic to a weakly
// identified hyper-scale and is NOT cured by the non-centred form: the
// non-centred prior is std_normal on log_sigma_raw, but the LIKELIHOOD couples
// log_sigma_raw[p] to the data only through tau_log_sigma * log_sigma_raw[p],
// so when tau is tiny the step size that works at the funnel mouth is far too
// large in the neck. The diagnosed decomposition confirms this is the dominant
// pathology: removing the sigma hierarchy (shared scalar sigma) drops treedepth
// saturation from 97.7% to 25-32%.
//
// FIX: drop the mu/tau/raw hierarchy entirely and give each party its own
// scale directly,
//     sigma[p] ~ half-Student-t(3, 0, scale_sigma),   independent over p.
// There is now NO hyper-scale to funnel against. Each sigma[p] is a single
// parameter with a FIXED-width prior, identified directly by party p's own
// random-walk innovations. We parameterise sigma on the log scale with a
// std_normal raw and a Jacobian-correct half-Student-t target so the sampler
// sees an unconstrained, roughly-unit-scaled coordinate per party. The
// heavy (df=3) tail lets a genuinely more volatile party escape upward without
// a shared mean dragging it back, while the common fixed scale_sigma supplies
// the mild pooling that a 10-party panel needs — but as a PRIOR constant, not a
// sampled hyper-parameter, so it cannot create a funnel.
//
// KEEPS: (a) per-party RW volatility — 10 free sigma[p], no shared-scalar
// collapse; (b) the FULL P x P cross-party innovation correlation L_Omega
// (unchanged from v2). Innovations are diag_pre_multiply(sigma, L_Omega)-scaled
// and projected onto the sum-to-zero subspace by centring, exactly as v2.
// Reference-invariance is preserved: beta lives on the zero-sum subspace and
// pi_smooth = softmax(beta) is independent of party ordering.

data {
  int<lower = 1> D;                                // Number of time points
  int<lower = 2> P;                                // Number of parties
  int<lower = 1> H;                                // Number of houses (house 1 = election anchor)
  int<lower = 1> N;                                // Number of polls

  array[N, P] int<lower = 0> y_n;                  // Polling counts per party [N x P]
  array[N] int<lower = 1, upper = H> house_n;      // House indicator per poll [N]
  array[N] int<lower = 1, upper = D> date_n;       // Date indicator per poll [N]
  array[N] int<lower = 1, upper = P> n_parties_n;  // Parties reported per poll [N]
  array[N, P] int<lower = 0, upper = P> party_index_n; // Column ids of reported parties per poll (0-padded tail)

  vector[D - 1] time_diff;                         // Gaps between consecutive dates [D-1]
  int<lower = 1> n_pred;                           // Sample size for posterior predictions
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
    reject("polling_watch_v4: the data hold no election result (house 1)");
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
    reject("polling_watch_v4: no party is reported by every election result");
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
  // Per anchor, in the walk's coordinates: parties the result reports (present) and the
  // rest (absent). Present coordinates are stored shifted so that the result's likelihood
  // does not depend on the absent ones: b_present = w_present - sum(b_absent) / (n_present + 1).
  // The shift is unit triangular, so no Jacobian term.
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
  // Non-centred innovation column for every date that is not an anchor
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
  // Each anchor's parameter is stored relative to the levels that reproduce its result
  // exactly (w_off), so init 0 starts every election at its result rather than at equal
  // shares. A constant shift: exact, no Jacobian. The result's logits against the
  // reference are w_i + sum(w) in the stored present levels w, so
  // w_i = r_i - sum(r) / (n_present + 1) reproduces r_i = log(y_i / y_ref).
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

  // Overdispersion bound: log phi_h <= phi_cap. Beyond it the Dirichlet-multinomial is a
  // multinomial for these polls, and Stan's gradient of it (CmdStan 2.38) carries an error
  // that grows with phi: negligible at log phi 20, O(1) and more from 30, enough to stall
  // a chain that warmup throws there. The bound conditions the posterior on
  // log phi_h < phi_cap; the houses' posteriors centre on log phi 6-8. The unconstrained
  // value is shifted so that init 0 is phi_init, the polls' geometric-mean size.
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

  // The walk: election-date levels (anchor_w, present parties shifted) and the non-centred
  // innovations of the bridges and of the stretches before the first and after the last
  // anchor (z_walk). beta is built from them in transformed parameters.
  array[m] vector[P - 1] anchor_w;
  matrix[P - 1, nz] z_walk;

  // Per-party RW volatility — INDEPENDENT per party, NO shared hyper-scale.
  // log_sigma is unconstrained; the half-Student-t prior is imposed in `model`
  // with the change-of-variables Jacobian for the exp() transform.
  vector[P] log_sigma;

  // Per-house overdispersion (hierarchical, centred, bounded above at phi_cap).
  vector[H - 1] log_phi_u;
  real mu_phi;
  real<lower = 0> sigma_phi;

  // House effects, zero-sum over parties per house. House 1 (election) pinned to 0.
  array[H - 1] sum_to_zero_vector[P] gamma_free;
  sum_to_zero_vector[P] mu_gamma;                  // shared industry bias (zero-sum)
  real<lower = 0> sigma_gamma;                     // single scale (label-symmetric)
}

transformed parameters {
  vector<lower = 0>[P] sigma = exp(log_sigma);     // per-party innovation scales
  // Stan's upper-bound transform, its unconstrained value shifted by log(phi_cap - phi_init)
  vector[H - 1] log_phi = phi_cap - (phi_cap - phi_init) * exp(log_phi_u);
  vector<lower = 0>[H - 1] phi = exp(log_phi);

  array[H] vector[P] gamma;
  gamma[1] = rep_vector(0.0, P);
  for (h in 2:H) {
    gamma[h] = mu_gamma + sigma_gamma * gamma_free[h - 1];
  }

  // Cholesky factor of the walk's step covariance per day in the reference-free
  // coordinates: the oth block of C S C (each column of diag(sigma) L_Omega centred)
  matrix[P - 1, P - 1] L_walk;
  {
    matrix[P, P] M = diag_pre_multiply(sigma, L_Omega);
    matrix[P, P] CM = M - rep_matrix(rep_row_vector(1.0 / P, P) * M, P);
    L_walk = cholesky_decompose(tcrossprod(CM[oth, ]));
  }

  array[D] vector[P] beta;
  {
    matrix[P - 1, D] b;
    // Anchors from the root (the last) back, since each earlier one is centred on the
    // next. A party the result does not report has no data at the anchor: its level is
    // non-centred, b_abs = b_next + sqrt(dcv) L_aa u (u = anchor_w[j][ab]), L_aa the
    // Cholesky factor of its block of the step covariance.
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
    // Bridges, left to right within each gap: the exact conditional of the walk given both ends
    for (j in 1:(m - 1)) {
      int a = anchor[j];
      int nxt = anchor[j + 1];
      for (t in (a + 1):(nxt - 1)) {
        real R = cv[nxt] - cv[t - 1];
        b[, t] = b[, t - 1] + (v[t] / R) * (b[, nxt] - b[, t - 1])
          + sqrt(v[t] * (cv[nxt] - cv[t]) / R) * Lz[, zcol[t]];
      }
    }
    // Before the first anchor: backward non-centred walk
    for (i in 1:(anchor[1] - 1)) {
      int t = anchor[1] - i;
      b[, t] = b[, t + 1] + sqrt(v[t + 1]) * Lz[, zcol[t]];
    }
    // After the last anchor: forward non-centred walk
    for (t in (anchor[m] + 1):D) {
      b[, t] = b[, t - 1] + sqrt(v[t]) * Lz[, zcol[t]];
    }
    for (t in 1:D) {
      beta[t][oth] = b[, t];
      beta[t][ref_party] = -sum(b[, t]);
    }
  }
}

model {
  L_Omega ~ lkj_corr_cholesky(10);

  // The walk. Its start keeps the former beta0 prior; the root anchor has no prior of its
  // own, since the walk carries the start prior to it.
  beta[1] ~ normal(0, 2);
  to_vector(z_walk) ~ std_normal();
  // Earlier anchors: b_a ~ N(b_next, dcv L L'), absent parties non-centred and present
  // ones given them, from the Cholesky factor with the absent parties first
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

  // Independent half-Student-t(nu, 0, scale) on each per-party sigma, sampled on
  // the log scale. student_t_lpdf gives the half-t density on sigma >= 0 (the
  // truncation constant is the same for every draw, so it can be dropped); the
  // `+ log_sigma` term is the Jacobian for sigma = exp(log_sigma).
  target += student_t_lpdf(sigma | nu_sigma, 0, scale_sigma) + sum(log_sigma);

  log_phi ~ normal(mu_phi, sigma_phi);
  target += sum(log_phi_u);                        // Jacobian of the bound (constant dropped)
  mu_phi ~ normal(0, 1);
  sigma_phi ~ normal(0, 1);

  for (h in 1:(H - 1)) {
    gamma_free[h] ~ std_normal();
  }
  mu_gamma ~ std_normal();
  sigma_gamma ~ exponential(1);

  for (n in 1:N) {
    int k = n_parties_n[n];
    vector[P] eta = beta[date_n[n]] + gamma[house_n[n]];
    // Gather reported parties by identity (party_index_n), not the first-k slice.
    array[k] int cols = party_index_n[n, 1:k];
    array[k] int y_obs = y_n[n, cols];
    if (house_n[n] > 1) {
      vector[k] pi_n = softmax(eta[cols]);
      y_obs ~ dirichlet_multinomial(pi_n * phi[house_n[n] - 1]);
    } else {
      y_obs ~ multinomial_logit(eta[cols]);
    }
  }
}

generated quantities {
  // Party-space (reference-invariant) correlation of the RW innovations.
  corr_matrix[P] Omega = multiply_lower_tri_self_transpose(L_Omega);

  // Smoothed support: softmax of a zero-sum latent vector — invariant to party ordering.
  array[D, P] real pi_smooth;
  for (d in 1:D) {
    pi_smooth[d] = to_array_1d(softmax(beta[d]));
  }

  array[P] int<lower = 0> y_rep = multinomial_logit_rng(beta[D], n_pred);
}
