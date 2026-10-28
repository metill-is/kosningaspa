// Stage 1 of arm V2 (spec §6.5): the fundamentals regression, fitted on the elections
// before E only, made reference-invariant. The predictor is computed for every party of
// S_f(d) and centred within each election, and the intercepts are a sum-to-zero vector over
// the party labels, so no label is a reference and the log density does not depend on the
// order of the labels or of the rows. The data come from stage1_data() in
// R/harness/stage1.R; the harness takes the posterior mean of eta_E as mu_f.
data {
  int<lower = 1> D;                              // training elections
  int<lower = 2> N;                              // training rows, sorted by election
  int<lower = 2> L;                              // party labels (the alpha labels)
  array[D] int<lower = 2> n_rows;                // [D] rows of each election, summing to N
  array[N] int<lower = 1, upper = L> label;      // [N] each row's label
  array[N] int<lower = 0> y;                     // [N] votes, over S_f(d) only
  vector[N] x;                                   // [N] logit of the previous vote share
  vector[N] inc_years;                           // [N] log incumbency years, 0 for none
  vector[N] vnv;                                 // [N] log(1 + CPI inflation), incumbents only
  vector[N] growth;                              // [N] log(1 + GDP growth), incumbents only
  int<lower = 2> N_E;                            // parties of S_f(E)
  array[N_E] int<lower = 1, upper = L> label_E;  // [N_E] each S_f(E) party's label
  vector[N_E] x_E;                               // [N_E] as x, at E
  vector[N_E] inc_years_E;                       // [N_E] as inc_years, at E
  vector[N_E] vnv_E;                             // [N_E] as vnv, at E
  vector[N_E] growth_E;                          // [N_E] as growth, at E
}

transformed data {
  if (sum(n_rows) != N) reject("n_rows must sum to N");
}

parameters {
  sum_to_zero_vector[L] alpha;
  real beta_lag;
  real beta_inc;
  real beta_vnv;
  real beta_growth;
  // The lower bound is log phi_f < 20 (Decision 4): CmdStan 2.38's dirichlet_multinomial
  // gradient is exact up to a log concentration of about 22.
  real<lower = exp(-20)> phi_f_inv;
}

model {
  vector[N] eta = alpha[label] + beta_lag * x + beta_inc * inc_years + beta_vnv * vnv + beta_growth * growth;
  real phi_f = inv(phi_f_inv);
  int pos = 1;
  for (d in 1:D) {
    vector[n_rows[d]] eta_d = segment(eta, pos, n_rows[d]);
    y[pos:(pos + n_rows[d] - 1)] ~ dirichlet_multinomial(phi_f * softmax(eta_d - mean(eta_d)));
    pos += n_rows[d];
  }

  alpha ~ normal(0, 1);
  beta_lag ~ std_normal();
  beta_inc ~ std_normal();
  beta_vnv ~ normal(0, 5);
  beta_growth ~ normal(0, 5);
  phi_f_inv ~ exponential(1);
}

generated quantities {
  // The centred predictor at E over S_f(E), in the order of label_E.
  vector[N_E] eta_E;
  {
    vector[N_E] e = alpha[label_E] + beta_lag * x_E + beta_inc * inc_years_E + beta_vnv * vnv_E
                    + beta_growth * growth_E;
    eta_E = e - mean(e);
  }
}
