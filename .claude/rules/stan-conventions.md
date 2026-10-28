---
paths:
  - "**/*.stan"
---

# Stan Model Conventions

- Use `cmdstanr` (not `rstan`) for all model compilation and sampling.
- Non-centred parameterisations (`_raw` suffix) for hierarchical parameters, unless the data pin them; where the data pin some groups and not others, centre each group partially. The production model `polling_and_fundamentals_kjordaemi.stan` anchors its walk at the election dates with non-centred Brownian bridges between them (`anchor_w`, `z_walk`), centres the constituency effects and log volatilities partially, cell by cell (`delta_t`, `log_sigma_t`, weight lambda = s^2 I / (1 + s^2 I) from a data-only information proxy I; I = 0 is non-centred), keeps the house effects non-centred (`gamma_raw`: the walk and `mu_gamma` absorb what a poll says about them, so the proxy overstates it) and bounds log phi above at 20 (`Stan/CLAUDE.md`, Parameterisation). Against the earlier backward walk with centred effects, minimum bulk ESS per 1,000 gradients rose 4.4x on the 2024-eve data and 8.0x on 2021 f180 (28 Oct 2026, `R/sampler_check/RESULTS.md`). Check a change with `R/sampler_check/` (`equivalence_anchored.R`, then `compare_fits.R`).
- The production model is fitted with `max_treedepth = 12`. Its transitions stop at depth 8 on the 2024-eve data and 9 on 2021 f180 (28 Oct 2026, `R/sampler_check/RESULTS.md`); the earlier model needed 11, which the default cap of 10 truncated. Check `treedepth__` on each fit and raise the cap if transitions reach it. A few divergent transitions on 2021 f180 (up to 3 of 4,000) are known (RESULTS.md, Divergences); a fit with more needs a look before it is used.
- Comment expected array dimensions in the `data` block.
- `generated quantities` block must produce `y_rep_*` posterior predictive draws.
- Test model changes by compiling with `cmdstan_model()` before running full MCMC.
- Default MCMC settings: 4 chains, 1000 warmup, 1000 sampling iterations.
- Election model uses Dirichlet-Multinomial likelihood with house effects summing to zero.
