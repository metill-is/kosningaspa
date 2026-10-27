# Stan Models

## Production Model

**`polling_and_fundamentals_kjordaemi.stan`** is the production model used for election forecasting. All other models are either alternatives for specific use cases or experimental.

## Model Inventory

| Model | Purpose | Used By |
|-------|---------|---------|
| `polling_and_fundamentals_kjordaemi.stan` | Full model with constituency effects | `R/fit_polling_and_fundamentals_kjordaemi_model.R` |
| `polling_and_fundamentals_kjordaemi_r0.stan` | Harness arm R0: the production model as of commit 884560f (δ and log σ centred, replications gathered by party identity) with harness hooks only (H1 the hedge scale `mu_gamma_scale` as data, H2 `pi_eday`/`pi_kd`; H3, the identity gather of `y_rep_k`/`y_rep_national`, has been production's own since 5f09c5b); `transformed data` to `model` are byte-identical to that commit's `polling_and_fundamentals_kjordaemi.stan`, which `tests/harness/test-r0-stan.R` reads with `git show`, not from the working tree | `R/harness/fit_cell.R` |
| `polling_and_fundamentals.stan` | National-level model (no constituencies) | `R/fit_polling_and_fundamentals_model.R` |
| `polling_and_fundamentals_historic.stan` | Backtesting variant for past elections | `R/modeling_utils.R` |
| `fundamentals.stan` | Fundamentals-only (no polling data) | `R/fit_fundamentals_model.R` |
| `base_model_no_polling_bias.stan` | Weighted average baseline (no house effects) | Comparison/baseline |
| `polling_watch_v4.stan` | Between-election polling smoother (production; the other `polling_watch*.stan` files are superseded or comparison variants, see `polling_watch_variants.md`) | `R/fit_polling_watch.R` |
| `polling_watch_v5_epoch.stan` | Experimental: v4 with pollster bias (industry `mu_gamma` and house effects) that random-walks across election epochs (`tau_mu`, `tau_house`, sampled or fixed; both 0 reproduces v4) | `R/fit_polling_watch_epoch.R` |

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
| `log_sigma`, `sigma` | Party-specific random walk log volatilities (centred: `log_sigma ~ normal(mu_log_sigma, tau_log_sigma)`) and `sigma = exp(log_sigma)` |
| `L_Omega` | Cholesky factor of party correlation matrix |
| `alpha_f`, `beta_lag_f`, `beta_inc_years_f`, `beta_vnv_f`, `beta_growth_f` | Fundamentals regression coefficients (`beta_vnv_f`, `beta_growth_f`: CPI inflation and previous-year GDP growth for incumbents, `normal(0, 5)` priors) |
| `delta_free`, `sigma_delta` | Constituency deviations from national trends (`delta_free` is centred: `normal(0, sigma_delta[p])`) |
| `tau_f` | Fundamentals weight decay parameter |
| `y_rep_national`, `y_rep_k` | Posterior predictive draws (national / constituency). Like the likelihood, they gather parties by identity from the full sum-to-zero vector: up to date D, `y_rep_national` draws the parties active at each date (`n_active_d`, `active_index_d`), and `y_rep_k` the parties each constituency poll reported (`party_index_k`); the other columns are 0 |

## Compilation

Compiled binaries (files without `.stan` extension) are gitignored and recompiled automatically by `cmdstanr`:

```r
library(cmdstanr)
model <- cmdstan_model("Stan/polling_and_fundamentals_kjordaemi.stan")
```

## Conventions

- Use `softmax()` to convert unconstrained parameters to simplex
