data {
  // Fundamentals
  int<lower = 1> D_f;
  int<lower = 1> P_f;
  int<lower = 1> max_n_parties_f;

  array[P_f, D_f] int<lower = 0> y_f;

  matrix[P_f, D_f + 1] x_f;
  matrix[P_f, D_f + 1] incumbent_f;
  matrix[P_f, D_f + 1] incumbent_years;
  matrix[P_f, D_f + 1] vnv;
  matrix[P_f, D_f + 1] growth;

  array[max_n_parties_f, D_f + 1] int<lower = 0> index_f;
  array[D_f + 1] int<lower = 0, upper = P_f> n_parties_f;

  array[D_f + 1] int<lower = 0> n_votes;
}

parameters {
  // Fundamentals
  vector[P_f - 1] alpha_f_raw;
  real beta_lag_f;
  real beta_inc_years_f;
  real beta_vnv_f;
  real beta_growth_f;
  real<lower = 0> phi_f_inv;
}

transformed parameters {
  real<lower = 0> phi_f = pow(phi_f_inv, -1);
  vector[P_f] alpha_f;
  alpha_f[2:P_f] = alpha_f_raw;
  alpha_f[1] = -sum(alpha_f[2:P_f]);

  
}

model {
  /* Fundamentals */
  // Priors
  beta_lag_f ~ std_normal();
  beta_inc_years_f ~ std_normal();
  alpha_f_raw ~ std_normal();
  phi_f_inv ~ exponential(1);

  for (d in 1:D_f) { 
    vector[n_parties_f[d]] mu_d;
    mu_d[1:n_parties_f[d]] = alpha_f[index_f[1:n_parties_f[d], d]] + 
      beta_lag_f * x_f[index_f[1:n_parties_f[d], d], d] + 
      beta_inc_years_f * incumbent_years[index_f[1:n_parties_f[d], d], d] +
      beta_vnv_f * vnv[index_f[1:n_parties_f[d], d], d] +
      beta_growth_f * growth[index_f[1:n_parties_f[d], d], d];
    vector[n_parties_f[d]] pi_d = softmax(mu_d);
    y_f[index_f[1:n_parties_f[d], d], d] ~ dirichlet_multinomial(pi_d * phi_f);
  }
}

generated quantities {
  array[P_f, D_f] int<lower = 0> y_rep;
  for (p in 1:P_f) {
    for (d in 1:D_f) {
      y_rep[p, d] = 0;
    }
  }
  for (d in 1:D_f) {
    vector[n_parties_f[d]] mu_d;
    mu_d[1:n_parties_f[d]] = alpha_f[index_f[1:n_parties_f[d], d]] + 
      beta_lag_f * x_f[index_f[1:n_parties_f[d], d], d] + 
      beta_inc_years_f * incumbent_years[index_f[1:n_parties_f[d], d], d] +
      beta_vnv_f * vnv[index_f[1:n_parties_f[d], d], d] +
      beta_growth_f * growth[index_f[1:n_parties_f[d], d], d];
    vector[n_parties_f[d]] pi_d = softmax(mu_d);
    y_rep[index_f[1:n_parties_f[d], d], d] = dirichlet_multinomial_rng(pi_d * phi_f, n_votes[d]);
  }
}




