// Reference-invariant polling smoother — shared innovation variance, ISOTROPIC
// (variant b). The cleanest geometry: native sum_to_zero_vector innovations scaled
// by a single shared `sigma`, with NO per-timestep centring (the constraint is
// handled by the type) and NO cross-party correlation. This is the minimal,
// best-conditioned model; the trade-off is that bloc co-movement (the Omega
// structure) is dropped. Use as a speed/geometry reference, or in production if the
// correlation turns out to add little (it is heavily shrunk by lkj already).

data {
  int<lower = 1> D;
  int<lower = 2> P;
  int<lower = 1> H;
  int<lower = 1> N;

  array[N, P] int<lower = 0> y_n;
  array[N] int<lower = 1, upper = H> house_n;
  array[N] int<lower = 1, upper = D> date_n;
  array[N] int<lower = 1, upper = P> n_parties_n;

  vector[D - 1] time_diff;
  int<lower = 1> n_pred;
}

transformed data {
  vector[D - 1] time_scale = sqrt(time_diff);
}

parameters {
  sum_to_zero_vector[P] beta0;
  array[D - 1] sum_to_zero_vector[P] eps;     // isotropic sum-to-zero innovations (native)
  real mu_log_sigma;                          // single shared innovation log-scale

  vector[H - 1] log_phi;
  real mu_phi;
  real<lower = 0> sigma_phi;

  array[H - 1] sum_to_zero_vector[P] gamma_free;
  sum_to_zero_vector[P] mu_gamma;
  real<lower = 0> sigma_gamma;
}

transformed parameters {
  real<lower = 0> sigma = exp(mu_log_sigma);
  vector<lower = 0>[H - 1] phi = exp(mu_phi + sigma_phi * log_phi);

  array[H] vector[P] gamma;
  gamma[1] = rep_vector(0.0, P);
  for (h in 2:H) {
    gamma[h] = mu_gamma + sigma_gamma * gamma_free[h - 1];
  }

  // Random walk: each innovation is already sum-to-zero, so no centring needed.
  array[D] vector[P] beta;
  beta[1] = beta0;
  for (t in 2:D) {
    beta[t] = beta[t - 1] + (time_scale[t - 1] * sigma) * eps[t - 1];
  }
}

model {
  beta0 ~ normal(0, 2);
  for (t in 1:(D - 1)) eps[t] ~ std_normal();
  mu_log_sigma ~ normal(-2, 2);

  log_phi ~ std_normal();
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
    if (house_n[n] > 1) {
      vector[k] pi_n = softmax(eta[1:k]);
      y_n[n, 1:k] ~ dirichlet_multinomial(pi_n * phi[house_n[n] - 1]);
    } else {
      y_n[n, 1:k] ~ multinomial_logit(eta[1:k]);
    }
  }
}

generated quantities {
  array[D, P] real pi_smooth;
  for (d in 1:D) {
    pi_smooth[d] = to_array_1d(softmax(beta[d]));
  }
  array[P] int<lower = 0> y_rep = multinomial_logit_rng(beta[D], n_pred);
}
