# Stan Models

## Production Model

**`polling_and_fundamentals_kjordaemi.stan`** is the production model used for election forecasting. All other models are either alternatives for specific use cases or experimental.

## Model Inventory

| Model | Purpose | Used By |
|-------|---------|---------|
| `polling_and_fundamentals_kjordaemi.stan` | Full model with constituency effects | `R/fit_polling_and_fundamentals_kjordaemi_model.R` |
| `polling_and_fundamentals_kjordaemi_r0.stan` | Harness arm R0: the production model as of commit aa2ac1a (anchored walk, δ and log σ partially centred, log φ bounded above at 20, replications gathered by party identity) with harness hooks only (H1 the hedge scale `mu_gamma_scale` as data, H2 `pi_eday`/`pi_kd`; H3, the identity gather of `y_rep_k`/`y_rep_national`, has been production's own since 5f09c5b); `transformed data` to `model` are byte-identical to that commit's `polling_and_fundamentals_kjordaemi.stan`, which `tests/harness/test-r0-stan.R` reads with `git show`, not from the working tree | `R/harness/fit_cell.R` |
| `polling_and_fundamentals.stan` | National-level model (no constituencies) | `R/fit_polling_and_fundamentals_model.R` |
| `polling_and_fundamentals_historic.stan` | Backtesting variant for past elections | `R/modeling_utils.R` |
| `fundamentals.stan` | Fundamentals-only (no polling data) | `R/fit_fundamentals_model.R` |
| `base_model_no_polling_bias.stan` | Weighted average baseline (no house effects) | Comparison/baseline |
| `polling_watch_v5_epoch.stan` | Between-election polling smoother, Fylgisvakt's production model since 28 October 2026: v4 with pollster bias (industry `mu_gamma` and house effects) that random-walks across the epochs between election results (`tau_mu`, `tau_house`, sampled or fixed; both fixed at 0 reproduces v4). Its walk and log φ are v4's (below); the posterior is unchanged by that parameterisation apart from the bound on log φ (`polling_watch_variants.md`, §10) | `R/fit_polling_watch.R`, `R/fit_polling_watch_epoch.R` |
| `polling_watch_v4.stan` | The same smoother with one pollster bias over the whole window: Fylgisvakt's production model until 28 October 2026, kept as v5's comparison (the other `polling_watch*.stan` files are superseded or comparison variants, see `polling_watch_variants.md`). Since 28 October 2026 its walk is anchored at the election results with non-centred Brownian bridges between them, and log φ is bounded above at 20, as in the production model's Parameterisation below; the posterior is unchanged apart from that bound (`polling_watch_variants.md`, §9) | `R/fit_polling_watch_epoch.R` (`--model=v4`), `R/sampler_check/` |

## Model Architecture (production model)

Dirichlet-Multinomial state-space model with:
- **Random walk** for latent party support (`beta`) with party-specific volatilities (`sigma`) and correlation structure (`L_Omega`)
- **House effects** (`gamma`/`phi`) with zero-sum constraint per polling firm
- **Fundamentals component** weighted by `tau_f` that decreases as election day approaches
- **Constituency deviations** (`delta`) from national trends with `sigma_delta`
- **Government collapse** (`stjornarslit`) periods trigger increased random walk volatility

**Parameterisation** (what NUTS samples; the posterior is that of the backward non-centred walk with non-centred effects, apart from the bound on log φ): the walk is anchored at election dates (`anchor_w`) with non-centred Brownian bridges between them (`z_walk`); the constituency effects and log volatilities are partially centred cell by cell, with weights from data-only information proxies in `transformed data` (Gorinova, Moore & Hoffman 2020): centred where data inform a cell, non-centred where none do; the house effects are non-centred; log φ is centred and bounded above at `phi_cap` = 20. `R/sampler_check/RESULTS.md` has the equivalence check and the old-against-new fits.

## Key Parameters

| Parameter | Description |
|-----------|-------------|
| `beta`, `mu_pred` | Latent party support (national, simplex via softmax) |
| `anchor_w`, `z_walk` | The walk's sampled coordinates: each election date's levels (centred, stored relative to that election's result so that init 0 starts there; a party absent from that election non-centred) and the non-centred innovations of the Brownian bridges between election dates and of the stretch before the first. `beta` is built from them and `z_beta0` |
| `gamma`, `phi` | House effects per polling firm (zero-sum constrained); `gamma = mu_gamma + sigma_gamma * gamma_raw` (non-centred). `log_phi ~ normal(mu_phi, sigma_phi)` is centred and bounded above at `phi_cap` = 20 through `log_phi_u`: this conditions the posterior on log φ < 20, where the polls' likelihood is already multinomial, because CmdStan 2.38's `dirichlet_multinomial` gradient is wrong far above it and stalls chains that warmup throws there |
| `log_sigma`, `sigma` | Party-specific random walk log volatilities, `log_sigma ~ normal(mu_log_sigma, tau_log_sigma)`, partially centred (sampled as `log_sigma_t`, weight `lambda_sigma`), and `sigma = exp(log_sigma)` |
| `L_Omega` | Cholesky factor of party correlation matrix |
| `alpha_f`, `beta_lag_f`, `beta_inc_years_f`, `beta_vnv_f`, `beta_growth_f` | Fundamentals regression coefficients (`beta_vnv_f`, `beta_growth_f`: CPI inflation and previous-year GDP growth for incumbents, `normal(0, 5)` priors) |
| `delta_free`, `sigma_delta` | Constituency deviations from national trends, `delta_free ~ normal(0, sigma_delta[p])`, partially centred cell by cell (sampled as `delta_t`, weight `lambda_delta`): a party no kjördæmi row reports gets weight 0, non-centred, which removes the centred form's funnel |
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
