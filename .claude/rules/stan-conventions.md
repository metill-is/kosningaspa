---
paths:
  - "**/*.stan"
---

# Stan Model Conventions

- Use `cmdstanr` (not `rstan`) for all model compilation and sampling.
- Non-centred parameterisations (`_raw` suffix) for hierarchical parameters, unless the data pin them. The production model `polling_and_fundamentals_kjordaemi.stan` centres `delta_free` and `log_sigma`: non-centred, they made slow ridges with `sigma_delta` and `mu_log_sigma` (minimum bulk ESS of `sigma_delta` 139 non-centred at depth 10 against 4054 centred at depth 12, 2024-eve data, 24 Oct 2026, `R/sampler_check/RESULTS.md`; that comparison does not separate the two changes). It keeps its random walk `z_beta_raw` non-centred: before a party first appears in the polls only the prior pins its walk, which a non-centred walk absorbs (centred walks mixed far worse in tests not recorded in this repo). Check a change to either with `R/sampler_check/` (`equivalence.R`, then `compare_fits.R`).
- The production model is fitted with `max_treedepth = 12`: its trajectories stop at depth 11, so the default cap of 10 would truncate every transition (under cap 12 all 4000 transitions stop at depth 11; the earlier non-centred model had 4000 of 4000 at cap 10; 2024-eve data, 24 Oct 2026, commands in `R/sampler_check/RESULTS.md`). Check `treedepth__` on each fit and raise the cap if transitions reach it.
- Comment expected array dimensions in the `data` block.
- `generated quantities` block must produce `y_rep_*` posterior predictive draws.
- Test model changes by compiling with `cmdstan_model()` before running full MCMC.
- Default MCMC settings: 4 chains, 1000 warmup, 1000 sampling iterations.
- Election model uses Dirichlet-Multinomial likelihood with house effects summing to zero.
