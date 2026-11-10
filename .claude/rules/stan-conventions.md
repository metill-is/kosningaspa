---
paths:
  - "**/*.stan"
---

# Stan Model Conventions

- Use `cmdstanr` (not `rstan`) for all model compilation and sampling.
- Non-centred parameterisations (`_raw` suffix) for hierarchical parameters, unless the data pin them; where the data pin some groups and not others, centre each group partially. The production model's parameterisation (the walk anchored at the election dates, constituency effects and log volatilities partially centred, house effects non-centred, log phi bounded above at 20) is described in `Stan/CLAUDE.md`, and each change's measurements are in `R/sampler_check/RESULTS.md`. Check a change with `R/sampler_check/` (`equivalence_anchored.R`, then `compare_fits.R`).
- The production model is fitted with `max_treedepth = 12`. Check `treedepth__` on each fit and raise the cap if transitions reach it; the depths and the few known divergent transitions on record are in `R/sampler_check/RESULTS.md`.
- Comment expected array dimensions in the `data` block.
- `generated quantities` block must produce `y_rep_*` posterior predictive draws.
- Test model changes by compiling with `cmdstan_model()` before running full MCMC.
- Default MCMC settings: 4 chains, 1000 warmup, 1000 sampling iterations.
- Election model uses Dirichlet-Multinomial likelihood with house effects summing to zero.
