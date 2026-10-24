# Sampler checks: centred δ and log σ, `max_treedepth = 12`

Results for the change that centres the constituency effects (`delta_raw` →
`delta_free ~ normal(0, sigma_delta[p])`) and the log volatilities (`log_sigma_raw` →
`log_sigma ~ normal(mu_log_sigma, tau_log_sigma)`) in
`Stan/polling_and_fundamentals_kjordaemi.stan`, and fits it with `max_treedepth = 12`.
Run on 24 October 2026.

- **Old model:** `Stan/polling_and_fundamentals_kjordaemi.stan` at master 5d22031
  (sha256 `f3c7f48d17024538548dbcc59a2514d82a89297680c8f849eb9d0273d3d687de`), fitted at the old default `max_treedepth = 10`.
- **New model:** the same file after this change (sha256 `5516fa513823bad80e4803041e3fa9cad93f70ca9c2f6aa34e1bb98905f9b6f3`), fitted at
  `max_treedepth = 12`.
- **Data:** the interactive fit script's `stan_data` for the eve of the 2024 election,
  built by `fit_headless.R` with no cutoff on 16 October 2026 (`stan_data_eve.json`, sha256
  `acafe475e6ca31a483b98316ed6fa1a70b4646fd271f761c34607199ff9a35bb`): 288 national polls
  on D = 268 dates, 6 constituency-level polls (36 poll-by-constituency rows), 10 parties,
  6 constituencies.
  `data/` has changed since (`fundamentals_data.csv` was refreshed on 23 October 2026),
  so rebuilding it today gives different data.
- **Sampler:** 4 chains, 1000 warmup + 1000 sampling iterations, `init = 0`, seed 4100.

## Equivalence

`equivalence.R` on 40 posterior draws of the old fit and 15 points pushed off the
posterior in every changed coordinate: **EQUIV PASS**.

- Log density: new minus (old minus the log-Jacobian) is constant to within 4.7e-10 over
  the posterior points and 7.0e-10 over all 55 points. It takes only the values −2, −1, 0
  and +1 times 2.3e-10, the spacing of doubles near |lp| = 1.6 million (the posterior
  points; the off-posterior points reach 3.8 million). The tolerance is 3.8e-6. Leaving out the
  `log tau_log_sigma` term, the `log sigma_delta` term or the whole Jacobian makes the
  constant vary by 31.5, 27.4 and 22.5 across the 55 points, so the check sees each term.
- Generated quantities: both models, run at the 40 posterior points with one seed, agree
  on all 130,600 values to a relative 1.4e-15. Every `y_rep` count is identical; the
  continuous quantities differ only by rounding. A mutant that keeps `sigma_delta *` in
  `delta_pred` passes the log-density part and fails this part (largest relative
  difference 8.0, on draws of a 150 + 50 old fit).

## Old against new

| | Old (depth 10) | New (depth 12) |
|---|---|---|
| Transitions at the depth cap | 4000 of 4000 | 0 of 4000 (all stop at depth 11) |
| Leapfrog steps per transition | 1023 | 2047 |
| Divergent transitions | 0 | 0 |
| Min E-BFMI | 0.69 | 0.69 |
| ms per gradient | 0.845 | 0.839 |
| Max R-hat, key quantities | 1.046 | 1.007 |
| Min bulk / tail ESS, key quantities | 139 / 158 | 1117 / 1014 |
| Min bulk ESS, all parameters | 138 | 942 |
| Min bulk ESS per 1,000 gradients | 0.034 | 0.136 (×4.0) |
| Wall time | 28.8 min | 59.4 min |
| Min bulk ESS per wall minute | 4.8 | 18.8 (×3.9) |

Key quantities are the 2,717 scalars of `beta`, `beta0`, `sigma`, `Omega`, `gamma`,
`phi`, `delta_pred`, the hierarchy and fundamentals parameters, `mu_pred`,
`mu_gamma_pred` and the election-day shares `pi_eday` (`compare_fits.R`, `KEY`).
At depth 10 the old model has not converged at 1000 + 1000: `sigma_delta` reaches
R-hat 1.046 with a minimum bulk ESS of 139. The largest gains are in the hierarchy
parameters of the centred blocks: minimum bulk ESS of `sigma_delta` rises from 139 to 4054,
of `mu_log_sigma` from 262 to 2991 and of `tau_log_sigma` from 387 to 2198. The table
compares the two changes together; it does not separate centring from the deeper cap.

The ESS per gradient is the like-for-like measure. The wall times were taken with other
jobs on the machine (load average 5–10 on 10 cores), but the cost per gradient was the
same for both fits.

The two fits agree within Monte Carlo error. Of the 50 election-day share quantiles
(5, 25, 50, 75 and 95 % for each party), 2 differ by more than 2 combined MCSEs
(largest |z| 2.20; about 2.3 are expected by chance). Of the 2,717 key posterior means,
4.6 % differ by more than 2 MCSEs and none by more than 3, with sd(z) = 1.02.

Not run: a second data set (such as the polls up to 31 October 2024) or other seeds.

## Reproducing

From the project root, with `<dir>` a scratch folder holding `stan_data_eve.json`:

```bash
git show 5d22031:Stan/polling_and_fundamentals_kjordaemi.stan > <dir>/kjordaemi_old.stan
cp Stan/polling_and_fundamentals_kjordaemi.stan <dir>/kjordaemi_new.stan
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/fit_headless.R --out=<dir>/eve_old \
  --stan=<dir>/kjordaemi_old.stan --data-json=<dir>/stan_data_eve.json --max-treedepth=10
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/equivalence.R --old-stan=<dir>/kjordaemi_old.stan \
  --new-stan=<dir>/kjordaemi_new.stan --data-json=<dir>/stan_data_eve.json \
  --draws-dir=<dir>/eve_old/cmdstan --out=<dir>/equivalence_eve.csv
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/fit_headless.R --out=<dir>/eve_new \
  --stan=<dir>/kjordaemi_new.stan --data-json=<dir>/stan_data_eve.json --max-treedepth=12
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/compare_fits.R --old=<dir>/eve_old \
  --new=<dir>/eve_new --data-json=<dir>/stan_data_eve.json --out=<dir>/compare --label=eve
```

`fit_headless.R` defaults to seed 4100 and 1000 + 1000 iterations. Without
`stan_data_eve.json`, its first call builds the JSON from the current `data/`.
