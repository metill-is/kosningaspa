# V2 prior predictive check

Written by `LC_ALL=en_US.UTF-8 Rscript R/harness/v2_prior_predictive.R <config> [n_draws]`. Prior draws only (`v2_simulate(observe = FALSE)`): nothing is fitted and no forecast is computed.

- Config: `harness/configs/v2.json` (sha256 `3af32d723ef484f75090972ad4cfdaccbfa65f5a4457e58a2d3ba145df6e4dc7`)
- Prior draws: 1000 per cell and row, seeds 6001 to 7000 (the walk row's z: seeds 7000001 to 7001000)
- Cell 2024-eve (E 2024-11-30, F 2024-11-29): P 10 parties, H 6 houses, K 6 kjördæmi, J 22 kjördæmi polls, 4 epochs
- Cell 2021-f180 (E 2021-09-25, F 2021-03-29): P 10 parties, H 6 houses, K 6 kjördæmi, J 6 kjördæmi polls, 3 epochs
- The 20% party is the coordinate party (the first of the order), in a composition of 20% beside P − 1 equal parties sharing 80%; each effect moves it on the log-ratio scale, and the summary is its change in pp. The script's header says which coordinate each effect reads.

## Summaries

- **house_d** (step v2s-m0): A house's deviation d from the industry, on a 20% party (d_h = σ_γ C ε_h, σ_γ ~ exponential(`prior_rate_sigma_gamma`)). Band: [5%, 95%] inside [-10, 10].
- **industry_mu1** (step v2s-m0): Industry bias μ_1 on a 20% party (μ_1 sum-to-zero, normal(0, `prior_sd_mu1`) on every element). Band: [5%, 95%] inside [-12, 12].
- **poll_sd** (step v2s-m0): A poll's sd around its mean, n = 1,000, 20% party, at the prior φ_h (μ_φ ~ N(`prior_mu_phi`, 1)). Band: [5%, 95%] inside [0.8, 6].
- **kd_delta** (step v2s-m0): δ in the open epoch: a party's kjördæmi deviation from its national share, 20% party (static, log s_δ0 = μ_sd + τ_sd r). Band: [5%, 95%] inside [-25, 25].
- **walk_180** (step v2s-m0): A 20% party's 180-day walk change, off the campaign (covariance 180 CΣCᵀ). Band: 95% quantile in [2, 30].
- **fund_weight** (step v2s-m5): The fundamentals pseudo-observation's weight against the walk's 180-day variance, for every party of S_f (a weight, not pp): (1/τ_f²) / (1/τ_f² + 1/V_w), τ_f = √(V_w (1 − w) / w), V_w = diag(CΣCᵀ) [(weight_time − campaign_days) + campaign_days (1 + τ_camp)²]: w by construction, a check of the code. Band: every draw within 1e-08 of 0.22.

## Results

Quantiles over the prior draws, in pp unless a row says otherwise (`Values`: draws × houses, kjördæmi or parties of S_f for a pooled summary).

| Summary | Step | Kind | Prior | Cell | Values | q05 | q50 | q95 | Band | Verdict |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| house_d | v2s-m0 | config | `prior_rate_sigma_gamma` = 1 | 2024-eve | 6000 | -19.58 | -2.14 | 23.25 | [5%, 95%] inside [-10, 10] | WIDE |
| house_d | v2s-m0 | config | `prior_rate_sigma_gamma` = 1 | 2021-f180 | 6000 | -19.39 | -1.80 | 24.16 | [5%, 95%] inside [-10, 10] | WIDE |
| house_d | v2s-m0 | comparison | `prior_rate_sigma_gamma` = 5 (proposal 4) | 2024-eve | 6000 | -7.22 | -0.05 | 6.58 | [5%, 95%] inside [-10, 10] | PASS |
| house_d | v2s-m0 | comparison | `prior_rate_sigma_gamma` = 5 (proposal 4) | 2021-f180 | 6000 | -6.92 | -0.02 | 6.31 | [5%, 95%] inside [-10, 10] | PASS |
| industry_mu1 | v2s-m0 | config | `prior_sd_mu1` = 1 | 2024-eve | 1000 | -16.97 | -5.01 | 29.30 | [5%, 95%] inside [-12, 12] | WIDE |
| industry_mu1 | v2s-m0 | config | `prior_sd_mu1` = 1 | 2021-f180 | 1000 | -17.25 | -6.10 | 26.93 | [5%, 95%] inside [-12, 12] | WIDE |
| industry_mu1 | v2s-m0 | comparison | `prior_sd_mu1` = 0.3 (proposal 4) | 2024-eve | 1000 | -7.17 | -0.30 | 8.78 | [5%, 95%] inside [-12, 12] | PASS |
| industry_mu1 | v2s-m0 | comparison | `prior_sd_mu1` = 0.3 (proposal 4) | 2021-f180 | 1000 | -7.63 | -0.61 | 8.40 | [5%, 95%] inside [-12, 12] | PASS |
| poll_sd | v2s-m0 | config | `prior_mu_phi` = 6.90776 | 2024-eve | 6000 | 1.33 | 1.80 | 4.44 | [5%, 95%] inside [0.8, 6] | PASS |
| poll_sd | v2s-m0 | config | `prior_mu_phi` = 6.90776 | 2021-f180 | 6000 | 1.32 | 1.75 | 4.10 | [5%, 95%] inside [0.8, 6] | PASS |
| poll_sd | v2s-m0 | comparison | `prior_mu_phi` = 0 (§6.10) | 2024-eve | 6000 | 12.43 | 28.44 | 38.36 | [5%, 95%] inside [0.8, 6] | FAIL (comparison) |
| poll_sd | v2s-m0 | comparison | `prior_mu_phi` = 0 (§6.10) | 2021-f180 | 6000 | 11.90 | 27.65 | 38.07 | [5%, 95%] inside [0.8, 6] | FAIL (comparison) |
| kd_delta | v2s-m0 | config | literals | 2024-eve | 6000 | -10.90 | -0.71 | 9.72 | [5%, 95%] inside [-25, 25] | PASS |
| kd_delta | v2s-m0 | config | literals | 2021-f180 | 6000 | -10.95 | -0.75 | 10.11 | [5%, 95%] inside [-25, 25] | PASS |
| walk_180 | v2s-m0 | config | literals | 2024-eve | 1000 | -11.07 | -1.03 | 9.59 | 95% quantile in [2, 30] | PASS |
| walk_180 | v2s-m0 | config | literals | 2021-f180 | 1000 | -10.20 | -0.82 | 12.16 | 95% quantile in [2, 30] | PASS |
| fund_weight | v2s-m5 | config | `desired_weight` = 0.22, `weight_time` = 180, `campaign_days` = 47 | 2024-eve | 9000 | 0.22 | 0.22 | 0.22 | every draw within 1e-08 of 0.22 | PASS |
| fund_weight | v2s-m5 | config | `desired_weight` = 0.22, `weight_time` = 180, `campaign_days` = 47 | 2021-f180 | 8000 | 0.22 | 0.22 | 0.22 | every draw within 1e-08 of 0.22 | PASS |

Skipped, as the config holds their values: house_d comparison prior_rate_sigma_gamma=1 (§6.10); industry_mu1 comparison prior_sd_mu1=1 (§6.10); poll_sd comparison prior_mu_phi=6.90776 (proposal 1).

## Lines

```
PRIOR WIDE house_d/2024-eve/config -19.58 23.25
PRIOR WIDE house_d/2021-f180/config -19.39 24.16
PRIOR PASS house_d/2024-eve/comparison:prior_rate_sigma_gamma=5
PRIOR PASS house_d/2021-f180/comparison:prior_rate_sigma_gamma=5
PRIOR WIDE industry_mu1/2024-eve/config -16.97 29.30
PRIOR WIDE industry_mu1/2021-f180/config -17.25 26.93
PRIOR PASS industry_mu1/2024-eve/comparison:prior_sd_mu1=0.3
PRIOR PASS industry_mu1/2021-f180/comparison:prior_sd_mu1=0.3
PRIOR PASS poll_sd/2024-eve/config
PRIOR PASS poll_sd/2021-f180/config
PRIOR FAIL poll_sd/2024-eve/comparison:prior_mu_phi=0 12.43 38.36
PRIOR FAIL poll_sd/2021-f180/comparison:prior_mu_phi=0 11.90 38.07
PRIOR PASS kd_delta/2024-eve/config
PRIOR PASS kd_delta/2021-f180/config
PRIOR PASS walk_180/2024-eve/config
PRIOR PASS walk_180/2021-f180/config
PRIOR PASS fund_weight/2024-eve/config
PRIOR PASS fund_weight/2021-f180/config
```

## Reading the verdicts

- **PASS**: the quantiles are in the band.
- **WIDE**: a config row of a bias whose prior the config holds at §6.10's value, out of its band. §6.10 fixes that prior, so the row goes to Brynjólfur as information and does not stop the script.
- **FAIL (stops)**: a config row out of its band. The script stops after writing this report.
- **FAIL (comparison)**: a comparison row out of its band, reported as information; it never stops the script.
- A band catches a broken prior (a wrong scale, unit or sign), not a mistuned one. It is wide on purpose, and no band is a tuning target: the quantiles themselves are the result.
