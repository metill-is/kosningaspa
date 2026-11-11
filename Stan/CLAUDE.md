# Stan models

| File | Status | Fitted by |
|---|---|---|
| `polling_watch_v5_epoch.stan` | live (Fylgisvakt) | `R/fit_polling_watch.R` |
| `polling_and_fundamentals_kjordaemi.stan` | production forecast (2024) | `R/fit_polling_and_fundamentals_kjordaemi_model.R` |
| `polling_and_fundamentals_kjordaemi_r0.stan` | harness (arm R0) | `R/harness/fit_cell.R` via `R/harness/arms.R` |
| `forecast_v2.stan` | harness (arm V2) | `R/harness/fit_cell.R` via `R/harness/arms.R` |
| `fundamentals_v2.stan` | harness (V2 stage 1) | `R/harness/stage1.R` |
| `polling_watch_v4.stan` | comparison (for v5) | `R/fit_polling_watch_epoch.R` |

The R0 file is the harness's pinned copy of the 2024 model; its differences from production are deliberate. Do not edit it: its sha256 keys the recorded R0 fits, and `tests/harness/test-r0-stan.R` compares it with production at a fixed commit.

## forecast_v2.stan

One configuration, the simple core: a sum-to-zero walk with LKJ correlation and the campaign multiplier (also on the step leaving an election), a constant poll bias with constant house deviations, a static kjördæmi deviation, the closed elections' results with no election-day shock, and the fundamentals pseudo-observation. The switched file and its harness are at the tag `v2-switched-final`; `harness/v2/lp-equivalence-simple-core.md` records that this file is that one at the simple core with the leaving-step rule added, and `harness/v2/decisions.md` holds the verdicts of the removed components.

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
