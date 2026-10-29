# Stan Models

## Production Model

**`polling_and_fundamentals_kjordaemi.stan`** is the production model used for election forecasting. All other models are either alternatives for specific use cases or experimental.

## Model Inventory

| Model | Purpose | Used By |
|-------|---------|---------|
| `polling_and_fundamentals_kjordaemi.stan` | Full model with constituency effects | `R/fit_polling_and_fundamentals_kjordaemi_model.R` |
| `polling_and_fundamentals_kjordaemi_r0.stan` | Harness arm R0: the production model as of commit aa2ac1a (anchored walk, δ and log σ partially centred, log φ bounded above at 20, replications gathered by party identity) with harness hooks only (H1 the hedge scale `mu_gamma_scale` as data, H2 `pi_eday`/`pi_kd`; H3, the identity gather of `y_rep_k`/`y_rep_national`, has been production's own since 5f09c5b); `transformed data` to `model` are byte-identical to that commit's `polling_and_fundamentals_kjordaemi.stan`, which `tests/harness/test-r0-stan.R` reads with `git show`, not from the working tree | `R/harness/fit_cell.R` |
| `fundamentals_v2.stan` | Harness arm V2's stage 1 (spec §6.5): the fundamentals regression on the elections before E, made reference-invariant. The predictor (α + β_lag · logit of the previous share as recorded + β_inc · log incumbency years + β_vnv · CPI + β_growth · growth, the last two for incumbents) is computed for every party of S_f(d) and centred within each election; α is a `sum_to_zero_vector` over the party labels; y_d ~ DirMult(φ_f softmax(η_d)), with φ_f⁻¹ ~ exponential(1) bounded below at exp(−20), so log φ_f < 20. Its one generated quantity, `eta_E`, is the centred predictor at E over S_f(E), whose posterior mean is μ_f. No `y_rep`: the generated quantities draw nothing, so model methods never reach an RNG | `R/harness/stage1.R` |
| `forecast_v2.stan` | Harness arm V2's forecast model (spec §6), its components switched by data (forecast_v2.stan's switches, below). The core, step `v2s-m0`, in the reference forms: a forward walk of the sum-to-zero log-ratio state θ whose innovation covariance is Δ s² CΣCᵀ, written in coordinates 2:P with `L_walk` = chol((CΣCᵀ)[2:P, 2:P]) taken from the thin QR of the rows 2:P of C diag(σ) L_Ω (forming the Gram matrix would square its condition number), so the density is invariant to the party order up to L_Ω's Jacobian; national polls DirMult(φ_h normalise(π_nat[S] ⊙ exp(γ[S]))), π_nat the w-weighted sum of the kjördæmi softmaxes over the parties existing at the poll's date; industry bias μ and house deviations C(σ_γ ε_h), constant over epochs in the core, a newcomer's bias the mean of the then-current μ over the parties existing before its entry epoch plus 0.2023 η, then re-centred; with `epoch_bias = 1` (step `v2s-m1`, §6.4) μ walks across epochs by centred steps C(τ ⊙ ξ_e), τ_p = τ_μ but Annað's own τ_μ,A (at `annad_idx`), and each house's deviations walk by C(τ_house ζ) after its first epoch (`house_first_epoch`, that of its first national poll; a house with no national poll gets n_epoch, so its deviations never walk), while a newcomer restarts at its entry epoch e_p: its μ at the entry mean and, in each house already walking, the newcomer's element of the uncentred level at σ_γ ε, before the centring, so no pre-entry drift reaches the likelihood and τ = 0 is the core; one static δ mapped through each epoch's M_e; kjördæmi polls DirMult(κ φ_h softmax(ν_j + δ)); the closed elections' kjördæmi results DirMult(φ_K softmax(θ + δ)); log φ_h, log φ_K and log κ bounded above at 20, 20 and 2 by log c = cap − (cap − init) exp(u), with their log-Jacobians added by hand. Every likelihood and replication gathers parties by identity through `idx_*`. Generated quantities: `log_lik` (national polls, kjördæmi poll rows, then results by election and kjördæmi), `y_rep_res`, and behind switches the forecast and `pi_latent`. Transformed data refuses each switch it does not build yet, naming the switch and its value. With `walk_form = 1` (step `v2s-p1`, the same posterior) the walk is anchored at the closed elections' dates (`grid_res`): at each, the parties with national votes are centred, stored relative to the result's centred log shares so that `anchor_w` = 0 starts the walk there, and shifted so that the result's likelihood does not depend on the absent parties, which are non-centred from the previous anchor; the dates between anchors are non-centred Brownian bridges and those after the last a forward walk. Transformed data refuses it unless every `grid_res` is in 2..G and strictly increasing and the coordinate party has national votes at every closed election. `R/harness/v2_equivalence.R` checks it against the reference walk, and all four of P1's checks pass; the adoption rule kept the reference walk all the same (`KEEP v2s-m0`, on 10 divergences against none; `harness/v2/steps.md`), so every later step's config sets `walk_form = 0` and the anchored form stays built behind its switch. `tests/harness/test-v2-stan.R` tests its log density, transformed parameters and generated quantities against the R reference `R/harness/v2_reference.R` | `tests/harness/test-v2-stan.R`, `R/harness/v2_equivalence.R`, `R/harness/arms.R` and `R/harness/fit_cell.R` (which fit it for the `v2s-*` arms) |
| `polling_and_fundamentals.stan` | National-level model (no constituencies) | `R/fit_polling_and_fundamentals_model.R` |
| `polling_and_fundamentals_historic.stan` | Backtesting variant for past elections | `R/modeling_utils.R` |
| `fundamentals.stan` | Fundamentals-only (no polling data) | `R/fit_fundamentals_model.R` |
| `base_model_no_polling_bias.stan` | Weighted average baseline (no house effects) | Comparison/baseline |
| `polling_watch_v4.stan` | Between-election polling smoother (production; the other `polling_watch*.stan` files are superseded or comparison variants, see `polling_watch_variants.md`) | `R/fit_polling_watch.R` |
| `polling_watch_v5_epoch.stan` | Experimental: v4 with pollster bias (industry `mu_gamma` and house effects) that random-walks across election epochs (`tau_mu`, `tau_house`, sampled or fixed; both 0 reproduces v4) | `R/fit_polling_watch_epoch.R` |

## forecast_v2.stan's switches

Each V2 step (`harness/configs/steps/v2s-*.json`) adds the row of the switch it builds. Until then, `transformed data` rejects that switch at any value but 0: in the core all seven `V2_SWITCHES` (`R/harness/v2.R`) are 0.

| Switch | Values | What it sizes | Step |
|--------|--------|---------------|------|
| `draw_forecast` | 1: draw the forecast; 0: none (a 2017 cell before the freeze) | `votes_kd` [K, P], `pi_nat_eday` [P], `pi_latent_E` [P]; size 0 at 0 | `v2s-m0` |
| `latent_path` | 1: write π_nat on the whole grid; 0 in every harness fit | `pi_latent` [P, G]; size 0 at 0 | `v2s-m0` |
| `walk_form` | 1: the walk anchored at the closed elections, built and checked equivalent; 0: the reference forward walk, which the adoption rule kept (`KEEP v2s-m0`, `harness/v2/steps.md`), so every later step sets 0 | `anchor_w` [n_closed] [P − 1] and `z_walk` [P − 1, G − 1 − n_closed]; at 0, `anchor_w` size 0 and `z_walk` [P − 1, G − 1] | `v2s-p1` |
| `epoch_bias` | 1: the industry bias and each house's deviations walk across epochs (§6.4), μ from epoch 2 with τ_μ (Annað's τ_μ,A), a house after its first epoch (that of its first national poll; a house with none never walks) with τ_house, a newcomer restarting at its entry epoch; 0: both constant over epochs, as in the core | `mu_step_raw` [P, n_epoch − 1], `house_step_raw` [H] [P, n_epoch − 1], `tau_mu`, `tau_mu_A` and `tau_house` [1]; size 0 at 0 | `v2s-m1` |

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
