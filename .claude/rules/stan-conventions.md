---
paths:
  - "Stan/**/*.stan"
---

# Stan Model Conventions

- The production model is `polling_and_fundamentals_kjordaemi.stan`. Changes here affect live forecasts.
- Use non-centered parameterizations (`_raw` suffix) for hierarchical parameters to improve sampling.
- All observation models use Dirichlet-Multinomial likelihood unless explicitly noted otherwise.
- House effects (`gamma`) must sum to zero within each polling firm — enforce via `sum_to_zero_vector` or manual subtraction.
- The `generated quantities` block must produce `y_rep_*` posterior predictive draws for model checking.
- Comment expected array dimensions in the `data` block (e.g., `int<lower=1> N; // number of polls`).
- Test model changes by compiling with `cmdstan_model()` before running full sampling.
