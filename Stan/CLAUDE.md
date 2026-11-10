# Stan models

| File | Status | Fitted by |
|---|---|---|
| `polling_watch_v5_epoch.stan` | live (Fylgisvakt) | `R/fit_polling_watch.R` |
| `polling_and_fundamentals_kjordaemi.stan` | production forecast (2024) | `R/fit_polling_and_fundamentals_kjordaemi_model.R` |
| `polling_and_fundamentals_kjordaemi_r0.stan` | harness (arm R0) | `R/harness/fit_cell.R` via `R/harness/arms.R` |
| `forecast_v2.stan` | harness (arm V2) | `R/harness/fit_cell.R` via `R/harness/arms.R` |
| `fundamentals_v2.stan` | harness (V2 stage 1) | `R/harness/stage1.R` |
| `polling_watch_v4.stan` | comparison (for v5) | `R/fit_polling_watch_epoch.R` |
| `polling_and_fundamentals.stan` | no live caller | none |
| `fundamentals.stan` | no live caller | none |

The R0 file is the harness's pinned copy of the 2024 model; its differences from production are deliberate. Do not edit it: its sha256 keys the recorded R0 fits, and `tests/harness/test-r0-stan.R` compares it with production at a fixed commit.

## forecast_v2.stan's switches

Verdicts from `harness/v2/steps.md`.

| Switch | Turns on | Verdict |
|---|---|---|
| `epoch_bias` | pollster bias walking across epochs | GO (m1); on in V2 |
| `newcomer_pool` | estimated newcomer bias mean and scale | GO (m2); off beat m6 in the 8 Nov search |
| `shock` | election-day shock per closed election | GO (m3); on in V2 |
| `delta_walk` | kjördæmi deviations walking across elections | GO (m4); on in V2 |
| `fundamentals` | stage 1 as a pseudo-observation | GO (m5); on in V2 |
| `post_flex` | campaign-like multiplier after an election | m6: a fifth of the 2024 MAE gap, 2021 ties; off |
| `bias_drift` | calendar drift of the bias | m7: no better than m6; off |
| `walk_form` | walk anchored at elections | KEEP v2s-m0 (P1), KEEP V2 (P3, P4) |
| `delta_form` | delta walk held centred | KEEP v2s-m4 (P2) |

On 10 November 2026 the owner decided to cut the file to one configuration (only `fundamentals` on, plus the campaign multiplier on the walk step leaving an election); until that is done it holds all nine.

## Key Parameters

Production model, `polling_and_fundamentals_kjordaemi.stan`.

| Parameter | Description |
|---|---|
| `beta`, `mu_pred` | latent national support (softmax) |
| `anchor_w`, `z_walk` | walk levels at election dates and bridge innovations |
| `gamma`, `phi` | house effects (zero-sum, non-centred) and concentrations |
| `log_sigma`, `sigma` | party walk volatilities (partially centred) |
| `L_Omega` | Cholesky factor of the party correlation |
| `alpha_f`, `beta_lag_f`, `beta_inc_years_f`, `beta_vnv_f`, `beta_growth_f` | fundamentals coefficients |
| `delta_free`, `sigma_delta` | constituency deviations (partially centred) |
| `tau_f` | fundamentals weight decay |
| `y_rep_national`, `y_rep_k` | posterior predictive draws |

## Invariants

- log phi is bounded above at 20: Stan 2.38's `dirichlet_multinomial` gradient is wrong far above it.
- Every likelihood and replication gathers parties by identity, never by position.
- The production model is fitted with `max_treedepth = 12`.
- `Stan/polling_watch_variants.md` is the Fylgisvakt decision record; `R/sampler_check/RESULTS.md` the sampler record.
- Compiled binaries beside the `.stan` files are git-ignored; `Stan/archive/` holds superseded models.
