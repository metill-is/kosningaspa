---
paths:
  - "**/*.stan"
---

# Stan Model Conventions (Workspace-wide)

- Use `cmdstanr` (not `rstan`) for all model compilation and sampling.
- Non-centred parameterisations (`_raw` suffix) for hierarchical parameters.
- Comment expected array dimensions in the `data` block.
- `generated quantities` block must produce `y_rep_*` posterior predictive draws.
- Test model changes by compiling with `cmdstan_model()` before running full MCMC.
- Default MCMC settings: 4 chains, 1000 warmup, 1000 sampling iterations.
- Sports models use Student's t likelihood (basketball/handball) or bivariate Poisson (football).
- Election model uses Dirichlet-Multinomial likelihood with house effects summing to zero.
