# Stan Models

## Production Model

**`polling_and_fundamentals_kjordaemi.stan`** is the production model used for election forecasting. All other models are either alternatives for specific use cases or experimental.

## Model Inventory

| Model | Purpose | Used By |
|-------|---------|---------|
| `polling_and_fundamentals_kjordaemi.stan` | Full model with constituency effects | `R/fit_polling_and_fundamentals_kjordaemi_model.R` |
| `polling_and_fundamentals.stan` | National-level model (no constituencies) | `R/fit_polling_and_fundamentals_model.R` |
| `polling_and_fundamentals_historic.stan` | Backtesting variant for past elections | `R/modeling_utils.R` |
| `fundamentals.stan` | Fundamentals-only (no polling data) | `R/fit_fundamentals_model.R` |
| `base_model_no_polling_bias.stan` | Weighted average baseline (no house effects) | Comparison/baseline |
| `polling_watch.stan` | Between-election polling smoother | `R/fit_polling_watch.R` |

## Model Architecture (production model)

Dirichlet-Multinomial state-space model with:
- **Random walk** for latent party support (`beta`) with party-specific volatilities (`sigma`) and correlation structure (`L_Omega`)
- **House effects** (`gamma`/`phi`) with zero-sum constraint per polling firm
- **Fundamentals component** weighted by `tau_f` that decreases as election day approaches
- **Constituency deviations** (`delta`) from national trends with `sigma_delta`
- **Government collapse** (`stjornarslit`) periods trigger increased random walk volatility

## Key Parameters

| Parameter | Description |
|-----------|-------------|
| `beta`, `mu_pred` | Latent party support (national, simplex via softmax) |
| `gamma`, `phi` | House effects per polling firm (zero-sum constrained) |
| `sigma` | Party-specific random walk volatilities |
| `L_Omega` | Cholesky factor of party correlation matrix |
| `alpha_f`, `beta_lag_f`, `beta_inc_years_f`, `beta_growth_f` | Fundamentals regression coefficients |
| `delta_raw`, `sigma_delta` | Constituency deviations from national trends |
| `tau_f` | Fundamentals weight decay parameter |
| `y_rep_national`, `y_rep_k` | Posterior predictive draws (national / constituency) |

## Compilation

Compiled binaries (files without `.stan` extension) are gitignored and recompiled automatically by `cmdstanr`:

```r
library(cmdstanr)
model <- cmdstan_model("Stan/polling_and_fundamentals_kjordaemi.stan")
```

## Conventions

- Data blocks should document expected dimensions in comments
- Use `_raw` suffix for unconstrained parameterizations (e.g., `delta_raw` with non-centered parameterization)
- Generated quantities block should produce `y_rep_*` posterior predictive draws
- Use `softmax()` to convert unconstrained parameters to simplex
