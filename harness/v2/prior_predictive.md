# V2 prior predictive check

Written by `LC_ALL=en_US.UTF-8 Rscript R/harness/v2_prior_predictive.R <config> [n_draws]`. Prior draws only (`v2_simulate(observe = FALSE)`): nothing is fitted and no forecast is computed.

- Config: `harness/configs/steps/v2s-m6.json` (sha256 `5746d9131bdecce975536db85d855c87d17c454b3a3570a8eea349189ff9dbbc`)
- Prior draws: 1000 per cell and row, seeds 6001 to 7000 (the walk, shock and δ-step rows' z: seeds 7000001 to 7001000)
- Cell 2024-eve (E 2024-11-30, F 2024-11-29): P 10 parties, H 6 houses, K 6 kjördæmi, J 22 kjördæmi polls, 4 epochs
- Cell 2021-f180 (E 2021-09-25, F 2021-03-29): P 10 parties, H 6 houses, K 6 kjördæmi, J 6 kjördæmi polls, 3 epochs
- The 20% party is the coordinate party (the first of the order), in a composition of 20% beside P − 1 equal parties sharing 80%; each effect moves it on the log-ratio scale, and the summary is its change in pp. The script's header says which coordinate each effect reads.
- `newcomer_entry` reads no coordinate party: its first party is the newcomer, at 10% beside P − 1 equal parties sharing 90%, moved by its entry bias against the existing parties' mean.
- `shock_sd_20` and `shock_sd_05` read no coordinate party: their first party is a 20% or a 5% party beside P − 1 equal parties, and each value is the sd of its change over 200 draws of z given the draw's s̄_E.

## Summaries

- **house_d** (step v2s-m0): A house's deviation d from the industry, on a 20% party (d_h = σ_γ C ε_h, σ_γ ~ exponential(`prior_rate_sigma_gamma`)). Band: [5%, 95%] inside [-10, 10].
- **industry_mu1** (step v2s-m0): Industry bias μ_1 on a 20% party (μ_1 sum-to-zero, normal(0, `prior_sd_mu1`) on every element). Band: [5%, 95%] inside [-12, 12].
- **poll_sd** (step v2s-m0): A poll's sd around its mean, n = 1,000, 20% party, at the prior φ_h (μ_φ ~ N(`prior_mu_phi`, 1)). Band: [5%, 95%] inside [0.8, 6].
- **kd_delta** (step v2s-m0): δ in the open epoch: a party's kjördæmi deviation from its national share, 20% party (static under `delta_walk` = 0, log s_δ0 = μ_sd + τ_sd r; the walk's under 1). Band: [5%, 95%] inside [-25, 25].
- **walk_180** (step v2s-m0): A 20% party's 180-day walk change, off the campaign (covariance 180 CΣCᵀ). Band: 95% quantile in [2, 30].
- **industry_step** (step v2s-m1, switch `epoch_bias`): An epoch's step in the industry bias on a 20% party (C(τ ⊙ ξ_e), τ_μ ~ half-N(0, `prior_tau_mu`), τ_μ,A ~ half-N(0, 0.3) for Annað). Band: [5%, 95%] inside [-6, 6].
- **newcomer_entry** (step v2s-m2, switch `newcomer_pool`): A newcomer's entry bias against the existing parties' mean, on a 10% party (m_new + s_new η, m_new ~ N(0, 0.3), s_new ~ half-N(0, 0.3)). Band: [5%, 95%] inside [-10, 10].
- **shock_sd_20** (step v2s-m3, switch `shock`): The election-day shock's sd on a 20% party: given the draw's s̄_E, the sd of its change over 200 draws of z, on the composition (20%, 80% / (P − 1), …), with ε = C(s̄_E d ⊙ z) and each d = √(min(0.2 / π, 10)) of that composition (s̄_E ~ half-N(0, `prior_s_bar_E`)). Band: median in [0.5, 2] × 1.6.
- **shock_sd_05** (step v2s-m3, switch `shock`): The election-day shock's sd on a 5% party: given the draw's s̄_E, the sd of its change over 200 draws of z, on the composition (5%, 95% / (P − 1), …), with ε = C(s̄_E d ⊙ z) and each d = √(min(0.2 / π, 10)) of that composition (s̄_E ~ half-N(0, `prior_s_bar_E`)). Band: median in [0.5, 2] × 0.95.
- **delta_step_4y** (step v2s-m4, switch `delta_walk`): A 20% party's change in its kjördæmi deviation over a 4-year gap: the walk's step M_e(√(4 / Δ̄) s_ζ ⊙ ζ) on the open epoch's weights, s_ζ = s̄_ζ s_zeta_raw (s̄_ζ ~ half-N(0, `prior_s_bar_zeta`), s_zeta_raw ~ half-N(0, 1)), leaving out M_e(δ_{e−1}) − δ_{e−1}, the change of kjördæmi weights. Band: [5%, 95%] inside [-15, 15].
- **fund_weight** (step v2s-m5, switch `fundamentals`): The fundamentals pseudo-observation's weight against the walk's 180-day variance, for every party of S_f (a weight, not pp): (1/τ_f²) / (1/τ_f² + 1/V_w), τ_f = √(V_w (1 − w) / w), V_w = diag(CΣCᵀ) [(weight_time − campaign_days) + campaign_days (1 + τ_camp)²]: w by construction, a check of the code. Band: every draw within 1e-08 of 0.22.
- **fund_weight_bias** (step v2s-m5, switch `fundamentals`): The same weight with V_w + V_bias in place of V_w, for every party of S_f (a weight, not pp): V_bias the variance over the prior draws of the industry bias's open-epoch step μ_open − μ_{open−1}, over S_f and centred over S_f, which §6.5's 0.22 target leaves out; V_w is centred over all P parties, as §6.5 writes it, and V_bias over S_f. Band: none (reported).
- **post_window** (step v2s-m6, switch `post_flex`): A 20% party's walk change over one post-election window (covariance D (1 + τ_post)² CΣCᵀ, D the window's days on the grid, `post_days` = 90 in V2; τ_post ~ exponential(1)). Band: 95% quantile in [2, 30].

## Results

Quantiles over the prior draws, in pp unless a row says otherwise (`Values`: draws × houses, kjördæmi, epoch steps, newcomers or parties of S_f for a pooled summary).

| Summary | Step | Kind | Prior | Cell | Values | q05 | q50 | q95 | Band | Verdict |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| house_d | v2s-m0 | config | `prior_rate_sigma_gamma` = 1 | 2024-eve | 6000 | -19.72 | -2.14 | 23.97 | [5%, 95%] inside [-10, 10] | WIDE |
| house_d | v2s-m0 | config | `prior_rate_sigma_gamma` = 1 | 2021-f180 | 6000 | -19.46 | -1.50 | 25.29 | [5%, 95%] inside [-10, 10] | WIDE |
| house_d | v2s-m0 | comparison | `prior_rate_sigma_gamma` = 5 (proposal 4) | 2024-eve | 6000 | -7.64 | -0.08 | 6.48 | [5%, 95%] inside [-10, 10] | PASS |
| house_d | v2s-m0 | comparison | `prior_rate_sigma_gamma` = 5 (proposal 4) | 2021-f180 | 6000 | -6.87 | -0.05 | 6.62 | [5%, 95%] inside [-10, 10] | PASS |
| industry_mu1 | v2s-m0 | config | `prior_sd_mu1` = 1 | 2024-eve | 1000 | -17.24 | -6.34 | 30.51 | [5%, 95%] inside [-12, 12] | WIDE |
| industry_mu1 | v2s-m0 | config | `prior_sd_mu1` = 1 | 2021-f180 | 1000 | -17.28 | -5.31 | 30.95 | [5%, 95%] inside [-12, 12] | WIDE |
| industry_mu1 | v2s-m0 | comparison | `prior_sd_mu1` = 0.3 (proposal 4) | 2024-eve | 1000 | -7.38 | -0.82 | 9.12 | [5%, 95%] inside [-12, 12] | PASS |
| industry_mu1 | v2s-m0 | comparison | `prior_sd_mu1` = 0.3 (proposal 4) | 2021-f180 | 1000 | -7.60 | -0.38 | 9.40 | [5%, 95%] inside [-12, 12] | PASS |
| poll_sd | v2s-m0 | config | `prior_mu_phi` = 6.90776 | 2024-eve | 6000 | 1.33 | 1.78 | 4.29 | [5%, 95%] inside [0.8, 6] | PASS |
| poll_sd | v2s-m0 | config | `prior_mu_phi` = 6.90776 | 2021-f180 | 6000 | 1.33 | 1.81 | 4.19 | [5%, 95%] inside [0.8, 6] | PASS |
| poll_sd | v2s-m0 | comparison | `prior_mu_phi` = 0 (§6.10) | 2024-eve | 6000 | 12.11 | 28.22 | 38.24 | [5%, 95%] inside [0.8, 6] | FAIL (comparison) |
| poll_sd | v2s-m0 | comparison | `prior_mu_phi` = 0 (§6.10) | 2021-f180 | 6000 | 12.12 | 28.60 | 38.16 | [5%, 95%] inside [0.8, 6] | FAIL (comparison) |
| kd_delta | v2s-m0 | config | literals | 2024-eve | 6000 | -12.61 | -1.08 | 12.78 | [5%, 95%] inside [-25, 25] | PASS |
| kd_delta | v2s-m0 | config | literals | 2021-f180 | 6000 | -11.56 | -1.04 | 12.52 | [5%, 95%] inside [-25, 25] | PASS |
| walk_180 | v2s-m0 | config | literals | 2024-eve | 1000 | -9.53 | -0.65 | 10.72 | 95% quantile in [2, 30] | PASS |
| walk_180 | v2s-m0 | config | literals | 2021-f180 | 1000 | -10.62 | -0.86 | 9.75 | 95% quantile in [2, 30] | PASS |
| industry_step | v2s-m1 | config | `prior_tau_mu` = 0.1 | 2024-eve | 3000 | -2.86 | -0.04 | 2.72 | [5%, 95%] inside [-6, 6] | PASS |
| industry_step | v2s-m1 | config | `prior_tau_mu` = 0.1 | 2021-f180 | 2000 | -3.01 | -0.06 | 2.71 | [5%, 95%] inside [-6, 6] | PASS |
| newcomer_entry | v2s-m2 | config | literals | 2024-eve | 4000 | -4.54 | 0.01 | 7.81 | [5%, 95%] inside [-10, 10] | PASS |
| newcomer_entry | v2s-m2 | config | literals | 2021-f180 | 4000 | -4.71 | 0.04 | 7.85 | [5%, 95%] inside [-10, 10] | PASS |
| shock_sd_20 | v2s-m3 | config | `prior_s_bar_E` = 0.15 | 2024-eve | 1000 | 0.18 | 1.81 | 4.91 | median in [0.5, 2] × 1.6 | PASS |
| shock_sd_20 | v2s-m3 | config | `prior_s_bar_E` = 0.15 | 2021-f180 | 1000 | 0.13 | 1.72 | 4.88 | median in [0.5, 2] × 1.6 | PASS |
| shock_sd_05 | v2s-m3 | config | `prior_s_bar_E` = 0.15 | 2024-eve | 1000 | 0.10 | 1.00 | 3.05 | median in [0.5, 2] × 0.95 | PASS |
| shock_sd_05 | v2s-m3 | config | `prior_s_bar_E` = 0.15 | 2021-f180 | 1000 | 0.07 | 0.95 | 3.11 | median in [0.5, 2] × 0.95 | PASS |
| delta_step_4y | v2s-m4 | config | `prior_s_bar_zeta` = 0.2 | 2024-eve | 6000 | -4.37 | -0.04 | 4.33 | [5%, 95%] inside [-15, 15] | PASS |
| delta_step_4y | v2s-m4 | config | `prior_s_bar_zeta` = 0.2 | 2021-f180 | 6000 | -4.52 | -0.03 | 4.30 | [5%, 95%] inside [-15, 15] | PASS |
| fund_weight | v2s-m5 | config | `desired_weight` = 0.22, `weight_time` = 180, `campaign_days` = 47 | 2024-eve | 9000 | 0.22 | 0.22 | 0.22 | every draw within 1e-08 of 0.22 | PASS |
| fund_weight | v2s-m5 | config | `desired_weight` = 0.22, `weight_time` = 180, `campaign_days` = 47 | 2021-f180 | 8000 | 0.22 | 0.22 | 0.22 | every draw within 1e-08 of 0.22 | PASS |
| fund_weight_bias | v2s-m5 | config | `prior_tau_mu` = 0.1 | 2024-eve | 9000 | 0.22 | 0.24 | 0.34 | none (reported) | REPORT |
| fund_weight_bias | v2s-m5 | config | `prior_tau_mu` = 0.1 | 2021-f180 | 8000 | 0.22 | 0.23 | 0.32 | none (reported) | REPORT |
| post_window | v2s-m6 | config | literals | 2024-eve | 1000 | -15.02 | -1.11 | 13.96 | 95% quantile in [2, 30] | PASS |
| post_window | v2s-m6 | config | literals | 2021-f180 | 1000 | -15.13 | -1.42 | 13.53 | 95% quantile in [2, 30] | PASS |

Skipped, as the config holds their values: house_d comparison prior_rate_sigma_gamma=1 (§6.10); industry_mu1 comparison prior_sd_mu1=1 (§6.10); poll_sd comparison prior_mu_phi=6.90776 (proposal 1).

## Lines

```
PRIOR WIDE house_d/2024-eve/config -19.72 23.97
PRIOR WIDE house_d/2021-f180/config -19.46 25.29
PRIOR PASS house_d/2024-eve/comparison:prior_rate_sigma_gamma=5
PRIOR PASS house_d/2021-f180/comparison:prior_rate_sigma_gamma=5
PRIOR WIDE industry_mu1/2024-eve/config -17.24 30.51
PRIOR WIDE industry_mu1/2021-f180/config -17.28 30.95
PRIOR PASS industry_mu1/2024-eve/comparison:prior_sd_mu1=0.3
PRIOR PASS industry_mu1/2021-f180/comparison:prior_sd_mu1=0.3
PRIOR PASS poll_sd/2024-eve/config
PRIOR PASS poll_sd/2021-f180/config
PRIOR FAIL poll_sd/2024-eve/comparison:prior_mu_phi=0 12.11 38.24
PRIOR FAIL poll_sd/2021-f180/comparison:prior_mu_phi=0 12.12 38.16
PRIOR PASS kd_delta/2024-eve/config
PRIOR PASS kd_delta/2021-f180/config
PRIOR PASS walk_180/2024-eve/config
PRIOR PASS walk_180/2021-f180/config
PRIOR PASS industry_step/2024-eve/config
PRIOR PASS industry_step/2021-f180/config
PRIOR PASS newcomer_entry/2024-eve/config
PRIOR PASS newcomer_entry/2021-f180/config
PRIOR PASS shock_sd_20/2024-eve/config
PRIOR PASS shock_sd_20/2021-f180/config
PRIOR PASS shock_sd_05/2024-eve/config
PRIOR PASS shock_sd_05/2021-f180/config
PRIOR PASS delta_step_4y/2024-eve/config
PRIOR PASS delta_step_4y/2021-f180/config
PRIOR PASS fund_weight/2024-eve/config
PRIOR PASS fund_weight/2021-f180/config
PRIOR REPORT fund_weight_bias/2024-eve/config 0.22 0.24 0.34
PRIOR REPORT fund_weight_bias/2021-f180/config 0.22 0.23 0.32
PRIOR PASS post_window/2024-eve/config
PRIOR PASS post_window/2021-f180/config
```

## Reading the verdicts

- **PASS**: the quantiles are in the band.
- **WIDE**: a config row of a bias whose prior the config holds at §6.10's value, out of its band. §6.10 fixes that prior, so the row goes to Brynjólfur as information and does not stop the script.
- **FAIL (stops)**: a config row out of its band. The script stops after writing this report.
- **FAIL (comparison)**: a comparison row out of its band, reported as information; it never stops the script.
- **REPORT**: a row with no band, reported as information; it never stops the script.
- A band catches a broken prior (a wrong scale, unit or sign), not a mistuned one. It is wide on purpose, and no band is a tuning target: the quantiles themselves are the result.
