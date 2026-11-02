// US midterm prototype (v5): v4 with an offset for each poll's voter population.
//
// A US poll reports likely voters (LV), registered voters (RV) or adults (A). v4 keeps one
// version per release, preferring LV, and treats all three alike, so its pollster bias mixes
// them. The mix changes within a cycle: LV polls are 10-26% of a cycle's early polls and
// 39-81% of its final eight weeks (2018-2026), and where one pollster released both on the
// same date, the LV two-party margin ran 2.6 points more Republican than the RV one in 2018
// but 1.3 more Democratic in 2026. v5 adds, per bias epoch e, an offset for RV polls and one
// for A polls, against LV polls (and results, which take none):
//     poll offset = gamma[e, house] + lambda[e, s],   s = RV or A,
//     lambda[e, s] ~ normal(lambda_bar[s], 0.05),     lambda_bar[s] ~ normal(0, 0.05),
// per direction on the zero-sum subspace, so mu_gamma is now the bias of LV polls. Centred:
// the between-epoch scale is fixed, so there is no funnel. Each epoch holds 300-860 RV
// polls but 0-33 A polls (2016-2026), so the A offsets lean on their prior. 0.05 on the
// log-ratio is about 2.5 points of two-party margin. Data: pop_n and pop_r (0 = LV, a result or V; 1 = RV; 2 = A).
// v4's header follows.
//
// US midterm prototype (v4): v3 with each house's log phi partially centred.
//
// v2 and v3 centre log phi on its hierarchical prior, log_phi ~ normal(mu_phi, sigma_phi),
// through log_phi = phi_cap - (phi_cap - phi_init) * exp(log_phi_u). With Iceland's seven
// houses, each with many polls, centring suits. Most of the ~160 US pollsters have a few
// polls, which say little about their phi, so the centred prior is a funnel in sigma_phi
// (the joint mode of v2 sits at sigma_phi -> 0). v4 centres each house's log phi in
// proportion to what its polls say, as the forecast model does for its constituency
// effects (Stan/CLAUDE.md, Parameterisation):
//     log_phi[h] = (1 - c[h]) * mu_phi + sigma_phi^(1 - c[h]) * u[h],
//     c[h] = s^2 I[h] / (1 + s^2 I[h]),   s = 0.5,   I[h] = (polls by house h) / 8,
// where I is a data-only proxy for the Fisher information about log phi (a Gaussian
// variance observation gives 1/2 about log (n + phi) / (1 + phi), whose slope in log phi is
// about -1/2 at n = phi). c = 0 is non-centred, c = 1 centred. The phi_cap bound goes: the
// hierarchical prior bounds log phi. v3's header follows.
//
// US midterm prototype (v3): v2 with the walk integrated out by a Kalman filter.
//
// v2's posterior has ~1,000 walk coordinates beside ~1,800 bias parameters, and NUTS
// spends thousands of leapfrog steps per transition on it. v2's walk prior is a Gaussian
// random walk (its anchors and bridges reparameterise one), and its polls see the walk
// through a softmax, so with each poll's likelihood approximated as Gaussian on its
// log-ratios the walk is a linear Gaussian state space that a Kalman filter integrates out
// exactly. The sampler then sees only the bias, overdispersion, walk-scale and race
// parameters. What changes from v2:
//
//   1. Likelihood. A poll reporting k parties gives k - 1 log-ratios against its last
//      reported party, Gaussian with the delta-method multinomial covariance from its
//      counts times the house's Dirichlet-multinomial variance factor (n + phi) / (1 + phi);
//      an election result is the same with factor 1. The bias offsets (house, plus the
//      race's deviation for a race poll) enter as in v2.
//   2. The walk. Same coordinates (centred logits of the parties other than the
//      reference), same prior (v2's beta[1] ~ normal(0, 2), steps with covariance
//      days * Sigma), same fundamentals factor on election day, now a pseudo-observation.
//   3. Generated quantities draw the walk given the parameters (forward filter, backward
//      sample): pi_walk on every date and beta_elec, from which the seats follow as in v2.
//
// Everything else (data, priors, bias epochs, race layer) is v2's. v2's header follows.
//
// US midterm prototype (v2): v1 with an exactly identified walk covariance.
//
// v1 (and v5) build the walk's step covariance from P per-party scales and a P x P
// correlation, centred onto the zero-sum subspace: 2P + P(P-1)/2 - P numbers for the
// (P-1)P/2 entries the data can see, so the posterior has ridges that the priors alone
// bound. v2 parameterises the step covariance in the walk's own P - 1 coordinates (the
// parties other than the reference, which here is Other) with P - 1 scales and a
// (P - 1) x (P - 1) correlation: exactly identified. The price is reference
// invariance, which v5's construction keeps; with P = 3 and Other the reference it is a
// small price for a first fit. Scales keep v5's half-t(3, 0, 0.02); the correlation gets
// LKJ(2), since a Democratic-Republican swing makes their levels move in opposite
// directions and LKJ(10) would pull that towards 0. v1's header follows.
//
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

functions {
  // Kalman filter for the walk b (K coordinates, a Gaussian random walk with step
  // covariance v[t] * Sigma into date t) and the log-ratio observations of each date, then
  // a step of pred_days to election day and the fundamentals pseudo-observation there.
  // Returns the log marginal likelihood and, when store = 1, the filtered means and
  // covariances of dates 1..D and of election day (D + 1). Written for K = 2.
  tuple(real, array[] vector, array[] matrix) kalman(
      int D, vector v, matrix Sigma, vector m0, matrix P0,
      array[] int ord, array[] int o_start, array[] int o_count, array[] int o_k,
      array[] vector o_z, array[] matrix o_F, array[] matrix o_R, vector infl,
      array[] vector off, int pred_days, vector b_fund, matrix R_fund, int store) {
    // WHY: scalar state (K = 2), since autodiff of many tiny matrix operations costs far
    // more than their arithmetic; the few two-dimensional results use matrices
    real lp = 0;
    real m1 = m0[1];
    real m2 = m0[2];
    real p11 = P0[1, 1];
    real p12 = P0[1, 2];
    real p22 = P0[2, 2];
    array[store ? D + 1 : 0] vector[2] ms;
    array[store ? D + 1 : 0] matrix[2, 2] Ps;
    for (t in 1:D) {
      if (t > 1) {
        p11 += v[t] * Sigma[1, 1];
        p12 += v[t] * Sigma[1, 2];
        p22 += v[t] * Sigma[2, 2];
      }
      for (i in 1:o_count[t]) {
        int o = ord[o_start[t] + i - 1];
        int d = o_k[o] - 1;
        if (d == 1) {
          real f1 = o_F[o][1, 1];
          real f2 = o_F[o][1, 2];
          real q1 = p11 * f1 + p12 * f2;
          real q2 = p12 * f1 + p22 * f2;
          real s2 = f1 * q1 + f2 * q2 + infl[o] * o_R[o][1, 1];
          real resid = o_z[o][1] - f1 * m1 - f2 * m2 - off[o][1];
          real g = resid / s2;
          lp += -0.5 * (log(s2) + resid * g);
          m1 += q1 * g;
          m2 += q2 * g;
          p11 -= square(q1) / s2;
          p12 -= q1 * q2 / s2;
          p22 -= square(q2) / s2;
        } else if (d == 2) {
          matrix[2, 2] Pm = [[p11, p12], [p12, p22]];
          matrix[2, 2] F = o_F[o];
          matrix[2, 2] PF = Pm * F';
          matrix[2, 2] S = F * PF + infl[o] * o_R[o];
          S = 0.5 * (S + S');
          vector[2] resid = o_z[o] - F * [m1, m2]' - off[o];
          lp += multi_normal_lpdf(resid | rep_vector(0, 2), S);
          matrix[2, 2] Kg = mdivide_right_spd(PF, S);
          vector[2] dm = Kg * resid;
          matrix[2, 2] dP = Kg * PF';
          m1 += dm[1];
          m2 += dm[2];
          p11 -= dP[1, 1];
          p12 -= 0.5 * (dP[1, 2] + dP[2, 1]);
          p22 -= dP[2, 2];
        }
      }
      if (store) {
        ms[t] = [m1, m2]';
        Ps[t] = [[p11, p12], [p12, p22]];
      }
    }
    matrix[2, 2] Pm = [[p11, p12], [p12, p22]] + pred_days * Sigma;
    vector[2] m = [m1, m2]';
    {
      matrix[2, 2] S = Pm + R_fund;
      vector[2] resid = b_fund - m;
      lp += multi_normal_lpdf(resid | rep_vector(0, 2), S);
      matrix[2, 2] Kg = mdivide_right_spd(Pm, S);
      m += Kg * resid;
      Pm -= Kg * Pm;
      Pm = 0.5 * (Pm + Pm');
    }
    if (store) {
      ms[D + 1] = m;
      Ps[D + 1] = Pm;
    }
    return (lp, ms, Ps);
  }

  // Each observation's variance inflation (1 for a result) and its bias offsets: house
  // bias, plus (v5) its voter population's offset, plus, for a race poll, the race's
  // deviation, as log-ratio contrasts like o_z.
  tuple(vector, array[] vector) obs_terms(
      int K, array[] int o_house, array[] int o_epoch, array[] int o_race, array[] int o_pop,
      array[] int o_k, array[,] int o_cols, vector o_n, vector phi, array[,] vector gamma,
      array[] vector delta, array[,] vector lambda) {
    int No = size(o_house);
    int P = K + 1;
    vector[No] infl;
    array[No] vector[K] off;
    for (o in 1:No) {
      vector[P] g = rep_vector(0, P);
      infl[o] = 1;
      if (o_house[o] > 1) {
        real ph = phi[o_house[o] - 1];
        infl[o] = (o_n[o] + ph) / (1 + ph);
        g = gamma[o_epoch[o], o_house[o]];
        if (o_pop[o] > 0) {
          g += lambda[o_epoch[o], o_pop[o]];
        }
      }
      if (o_race[o] > 0) {
        g += delta[o_race[o]];
      }
      off[o] = rep_vector(0, K);
      for (j in 1:(o_k[o] - 1)) {
        off[o][j] = g[o_cols[o, j]] - g[o_cols[o, o_k[o]]];
      }
    }
    return (infl, off);
  }
}

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
  array[N] int<lower = 0, upper = 2> pop_n;        // v5: voter population per poll [N] (0 = LV or a result, 1 = RV, 2 = A)

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
  array[N_r] int<lower = 0, upper = 2> pop_r;      // v5: voter population per race poll [N_r]
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
    reject("us_midterm_v5: the data hold no election result (house 1)");
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
    reject("us_midterm_v5: no party is reported by every election result");
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

  // v3: the walk's coordinates b are v2's (centred logits of the parties other than the
  // reference); G maps them to all P centred logits, beta = G * b.
  int K = P - 1;
  if (K != 2) {
    reject("us_midterm_v5: the Kalman filter is written for three parties (P = 3)");
  }
  matrix[P, K] G = rep_matrix(0, P, K);
  for (i in 1:K) {
    G[oth[i], i] = 1;
  }
  G[ref_party] = rep_row_vector(-1, K);
  // v2's beta[1] ~ normal(0, 2) on all P centred logits, as a prior on b at date 1
  matrix[K, K] P0 = 4 * inverse_spd(crossprod(G));
  // v2's fundamentals factor on beta_elec, as a pseudo-observation of b on election day
  vector[K] b_fund = mu_fund[oth];
  matrix[K, K] R_fund = square(sd_fund) * inverse_spd(crossprod(G));

  // Observations: national results and polls (1..N), then race polls (N+1..N+N_r). Each
  // reported party but the last is a log-ratio against the last, Gaussian with the
  // delta-method multinomial covariance from the counts (o_R); a poll's is inflated by
  // its house's Dirichlet-multinomial factor (n + phi) / (1 + phi) in the model.
  int No = N + N_r;
  array[No] int o_date;
  array[No] int o_house;
  array[No] int o_epoch;
  array[No] int o_race;
  array[No] int o_pop;
  array[No] int o_k;
  array[No, P] int o_cols;
  vector[No] o_n;
  array[No] vector[K] o_z;
  array[No] matrix[K, K] o_F;
  array[No] matrix[K, K] o_R;
  for (o in 1:No) {
    int k;
    array[P] int cols;
    array[P] int y;
    if (o <= N) {
      o_date[o] = date_n[o];
      o_house[o] = house_n[o];
      o_epoch[o] = epoch_n[o];
      o_race[o] = 0;
      o_pop[o] = house_n[o] == 1 ? 0 : pop_n[o];
      k = n_parties_n[o];
      cols = party_index_n[o];
      y = y_n[o];
    } else {
      int r = o - N;
      o_date[o] = date_r[r];
      o_house[o] = house_r[r];
      o_epoch[o] = epoch_r[r];
      o_race[o] = race_r[r];
      o_pop[o] = pop_r[r];
      k = n_parties_r[r];
      cols = party_index_r[r];
      y = y_r[r];
    }
    o_k[o] = k;
    o_cols[o] = cols;
    o_n[o] = 0;
    for (j in 1:k) {
      o_n[o] += y[cols[j]];
    }
    o_z[o] = rep_vector(0, K);
    o_F[o] = rep_matrix(0, K, K);
    o_R[o] = rep_matrix(0, K, K);
    real inv_ref = 1.0 / (y[cols[k]] + 0.5);
    for (j in 1:(k - 1)) {
      o_z[o][j] = log((y[cols[j]] + 0.5) / (y[cols[k]] + 0.5));
      o_F[o][j] = G[cols[j]] - G[cols[k]];
      for (jj in 1:(k - 1)) {
        o_R[o][j, jj] = inv_ref + (j == jj ? 1.0 / (y[cols[j]] + 0.5) : 0);
      }
    }
  }
  array[No] int ord = sort_indices_asc(o_date);
  // v4: centring weight of each house's log phi, from its number of polls
  vector[H - 1] c_phi;
  {
    vector[H - 1] n_h = rep_vector(0, H - 1);
    for (o in 1:No) {
      if (o_house[o] > 1) n_h[o_house[o] - 1] += 1;
    }
    vector[H - 1] info = n_h / 8;
    c_phi = 0.25 * info ./ (1 + 0.25 * info);
  }
  array[D] int o_start = rep_array(1, D);
  array[D] int o_count = rep_array(0, D);
  for (o in 1:No) {
    o_count[o_date[o]] += 1;
  }
  for (t in 2:D) {
    o_start[t] = o_start[t - 1] + o_count[t - 1];
  }
}

parameters {
  cholesky_factor_corr[P - 1] L_corr;              // Step correlation in the walk's coordinates
  vector[P - 1] log_sigma;                         // Log step scales per day, walk coordinates

  real mu_phi;
  real<lower = 0> sigma_phi;
  vector<offset = (1 - c_phi) * mu_phi, multiplier = exp((1 - c_phi) * log(sigma_phi))>[H - 1] log_phi;

  sum_to_zero_vector[P] mu_gamma0;                 // industry bias in epoch 1
  array[H - 1] sum_to_zero_vector[P] gamma_free;   // standardised house deviations in epoch 1
  real<lower = 0> sigma_gamma;                     // cross-house spread in epoch 1
  array[E - 1] sum_to_zero_vector[P] z_mu;             // industry shocks
  array[E - 1, H - 1] sum_to_zero_vector[P] z_house;   // house shocks
  array[estimate_tau] real<lower = 0> tau_mu_est;
  array[estimate_tau] real<lower = 0> tau_house_est;

  // v0 additions
  array[Q] sum_to_zero_vector[P] theta;            // Race regression coefficients
  real<lower = 0> sigma_race;                      // Residual SD of a race's deviation, per direction
  array[R] sum_to_zero_vector[P] z_race;           // Standardised race deviations (non-centred)

  // v5 additions
  array[2] sum_to_zero_vector[P] lambda_bar;       // Mean offset of RV (1) and A (2) polls against LV
  array[E, 2] sum_to_zero_vector[P] lambda;        // Offset per epoch (centred)
}

transformed parameters {
  vector<lower = 0>[P - 1] sigma = exp(log_sigma);
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

  matrix[P - 1, P - 1] L_walk = diag_pre_multiply(sigma, L_corr);

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
  L_corr ~ lkj_corr_cholesky(2);

  target += student_t_lpdf(sigma | nu_sigma, 0, scale_sigma) + sum(log_sigma);

  log_phi ~ normal(mu_phi, sigma_phi);
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
  for (s in 1:2) {
    lambda_bar[s] ~ normal(0, 0.05);
    for (e in 1:E) {
      lambda[e, s] ~ normal(lambda_bar[s], 0.05);
    }
  }
  // v3: the walk integrated out by the Kalman filter
  {
    tuple(vector[No], array[No] vector[K]) ot = obs_terms(K, o_house, o_epoch, o_race, o_pop, o_k, o_cols, o_n, phi, gamma, delta, lambda);
    matrix[K, K] Sigma = multiply_lower_tri_self_transpose(L_walk);
    target += kalman(D, v, Sigma, rep_vector(0, K), P0, ord, o_start, o_count, o_k, o_z, o_F,
                     o_R, ot.1, ot.2, pred_days, b_fund, R_fund, 0).1;
  }
}

generated quantities {
  // The walk drawn given the parameters: forward filter, backward sample
  matrix[D, P] pi_walk;                            // Vote shares on each date
  vector[P] beta_elec;
  {
    tuple(vector[No], array[No] vector[K]) ot = obs_terms(K, o_house, o_epoch, o_race, o_pop, o_k, o_cols, o_n, phi, gamma, delta, lambda);
    matrix[K, K] Sigma = multiply_lower_tri_self_transpose(L_walk);
    tuple(real, array[D + 1] vector[K], array[D + 1] matrix[K, K]) kf = kalman(
      D, v, Sigma, rep_vector(0, K), P0, ord, o_start, o_count, o_k, o_z, o_F, o_R, ot.1, ot.2,
      pred_days, b_fund, R_fund, 1);
    vector[K] b_next = multi_normal_rng(kf.2[D + 1], kf.3[D + 1]);
    beta_elec = G * b_next;
    for (i in 1:D) {
      int t = D + 1 - i;
      real step = t == D ? pred_days : v[t + 1];
      matrix[K, K] P_pred = kf.3[t] + step * Sigma;
      matrix[K, K] J = mdivide_right_spd(kf.3[t], P_pred);
      matrix[K, K] C = kf.3[t] - J * P_pred * J';
      vector[K] b_t = multi_normal_rng(kf.2[t] + J * (b_next - kf.2[t]), 0.5 * (C + C'));
      pi_walk[t] = softmax(G * b_t)';
      b_next = b_t;
    }
  }
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
