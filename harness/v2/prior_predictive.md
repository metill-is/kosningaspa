# V2 prior predictive check

Written by `LC_ALL=en_US.UTF-8 Rscript R/harness/v2_prior_predictive.R <config> [n_draws]`. Prior draws only (`v2_simulate(observe = FALSE)`): nothing is fitted and no forecast is computed.

- Config: `harness/configs/steps/v2s-m2.json` (sha256 `d50a7cb3b6104310eabc88688abce38f2701facda9c5eaf1af673760417b1e4a`)
- Prior draws: 1000 per cell and row, seeds 6001 to 7000 (the walk row's z: seeds 7000001 to 7001000)
- Cell 2024-eve (E 2024-11-30, F 2024-11-29): P 10 parties, H 6 houses, K 6 kjördæmi, J 9 kjördæmi polls, 4 epochs
- Cell 2021-f180 (E 2021-09-25, F 2021-03-29): P 10 parties, H 5 houses, K 6 kjördæmi, J 1 kjördæmi polls, 3 epochs
- The 20% party is the coordinate party (the first of the order), in a composition of 20% beside P − 1 equal parties sharing 80%; each effect moves it on the log-ratio scale, and the summary is its change in pp. The script's header says which coordinate each effect reads. A summary whose label names another share reads a party of that share instead.

## Summaries

- **house_d** (step v2s-m0): A house's deviation d from the industry, on a 20% party (d_h = σ_γ C ε_h, σ_γ ~ exponential(`prior_rate_sigma_gamma`)). Band: [5%, 95%] inside [-10, 10].
- **industry_mu1** (step v2s-m0): Industry bias μ_1 on a 20% party (μ_1 sum-to-zero, normal(0, `prior_sd_mu1`) on every element). Band: [5%, 95%] inside [-12, 12].
- **poll_sd** (step v2s-m0): A poll's sd around its mean, n = 1,000, 20% party, at the prior φ_h (μ_φ ~ N(`prior_mu_phi`, 1)). Band: [5%, 95%] inside [0.8, 6].
- **kd_delta** (step v2s-m0): A party's kjördæmi deviation from its national share, 20% party (static δ, log s_δ0 = μ_sd + τ_sd r). Band: [5%, 95%] inside [-25, 25].
- **walk_180** (step v2s-m0): A 20% party's 180-day walk change, off the campaign (covariance 180 CΣCᵀ). Band: 95% quantile in [2, 30].
- **industry_step** (step v2s-m1, switch `epoch_bias`): An epoch's step in the industry bias on a 20% party (C(τ ⊙ ξ_e), τ_μ ~ half-N(0, `prior_tau_mu`), τ_μ,A ~ half-N(0, 0.3) for Annað). Band: [5%, 95%] inside [-6, 6].
- **newcomer_entry** (step v2s-m2, switch `newcomer_pool`): A newcomer's entry bias against the existing parties' mean, on a 10% party (m_new + s_new η, m_new ~ N(0, 0.3), s_new ~ half-N(0, 0.3)). Band: [5%, 95%] inside [-10, 10].

## Results

Quantiles in pp over the prior draws (`Values`: draws × houses, kjördæmi, epoch steps or newcomers for a pooled summary).

| Summary | Step | Kind | Prior | Cell | Values | q05 | q50 | q95 | Band | Verdict |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| house_d | v2s-m0 | config | `prior_rate_sigma_gamma` = 1 | 2024-eve | 6000 | -19.63 | -2.20 | 26.27 | [5%, 95%] inside [-10, 10] | WIDE |
| house_d | v2s-m0 | config | `prior_rate_sigma_gamma` = 1 | 2021-f180 | 5000 | -19.68 | -2.19 | 23.87 | [5%, 95%] inside [-10, 10] | WIDE |
| house_d | v2s-m0 | comparison | `prior_rate_sigma_gamma` = 5 (proposal 4) | 2024-eve | 6000 | -7.38 | -0.06 | 6.92 | [5%, 95%] inside [-10, 10] | PASS |
| house_d | v2s-m0 | comparison | `prior_rate_sigma_gamma` = 5 (proposal 4) | 2021-f180 | 5000 | -7.19 | -0.08 | 6.61 | [5%, 95%] inside [-10, 10] | PASS |
| industry_mu1 | v2s-m0 | config | `prior_sd_mu1` = 1 | 2024-eve | 1000 | -17.17 | -5.48 | 27.00 | [5%, 95%] inside [-12, 12] | WIDE |
| industry_mu1 | v2s-m0 | config | `prior_sd_mu1` = 1 | 2021-f180 | 1000 | -17.47 | -5.97 | 27.99 | [5%, 95%] inside [-12, 12] | WIDE |
| industry_mu1 | v2s-m0 | comparison | `prior_sd_mu1` = 0.3 (proposal 4) | 2024-eve | 1000 | -7.33 | -0.48 | 8.98 | [5%, 95%] inside [-12, 12] | PASS |
| industry_mu1 | v2s-m0 | comparison | `prior_sd_mu1` = 0.3 (proposal 4) | 2021-f180 | 1000 | -7.68 | -0.71 | 8.40 | [5%, 95%] inside [-12, 12] | PASS |
| poll_sd | v2s-m0 | config | `prior_mu_phi` = 6.90776 | 2024-eve | 6000 | 1.33 | 1.80 | 4.25 | [5%, 95%] inside [0.8, 6] | PASS |
| poll_sd | v2s-m0 | config | `prior_mu_phi` = 6.90776 | 2021-f180 | 5000 | 1.33 | 1.83 | 4.15 | [5%, 95%] inside [0.8, 6] | PASS |
| poll_sd | v2s-m0 | comparison | `prior_mu_phi` = 0 (§6.10) | 2024-eve | 6000 | 12.63 | 28.49 | 38.21 | [5%, 95%] inside [0.8, 6] | FAIL (comparison) |
| poll_sd | v2s-m0 | comparison | `prior_mu_phi` = 0 (§6.10) | 2021-f180 | 5000 | 12.13 | 28.97 | 38.11 | [5%, 95%] inside [0.8, 6] | FAIL (comparison) |
| kd_delta | v2s-m0 | config | literals | 2024-eve | 6000 | -10.93 | -0.80 | 10.40 | [5%, 95%] inside [-25, 25] | PASS |
| kd_delta | v2s-m0 | config | literals | 2021-f180 | 6000 | -11.44 | -0.77 | 11.58 | [5%, 95%] inside [-25, 25] | PASS |
| walk_180 | v2s-m0 | config | literals | 2024-eve | 1000 | -11.09 | -0.70 | 7.87 | 95% quantile in [2, 30] | PASS |
| walk_180 | v2s-m0 | config | literals | 2021-f180 | 1000 | -10.90 | -0.99 | 9.83 | 95% quantile in [2, 30] | PASS |
| industry_step | v2s-m1 | config | `prior_tau_mu` = 0.1 | 2024-eve | 3000 | -3.03 | -0.03 | 2.86 | [5%, 95%] inside [-6, 6] | PASS |
| industry_step | v2s-m1 | config | `prior_tau_mu` = 0.1 | 2021-f180 | 2000 | -2.72 | -0.04 | 2.63 | [5%, 95%] inside [-6, 6] | PASS |
| newcomer_entry | v2s-m2 | config | literals | 2024-eve | 4000 | -4.77 | 0.18 | 7.65 | [5%, 95%] inside [-10, 10] | PASS |
| newcomer_entry | v2s-m2 | config | literals | 2021-f180 | 4000 | -4.72 | 0.03 | 7.87 | [5%, 95%] inside [-10, 10] | PASS |

Skipped, as the config holds their values: house_d comparison prior_rate_sigma_gamma=1 (§6.10); industry_mu1 comparison prior_sd_mu1=1 (§6.10); poll_sd comparison prior_mu_phi=6.90776 (proposal 1).

## Lines

```
PRIOR WIDE house_d/2024-eve/config -19.63 26.27
PRIOR WIDE house_d/2021-f180/config -19.68 23.87
PRIOR PASS house_d/2024-eve/comparison:prior_rate_sigma_gamma=5
PRIOR PASS house_d/2021-f180/comparison:prior_rate_sigma_gamma=5
PRIOR WIDE industry_mu1/2024-eve/config -17.17 27.00
PRIOR WIDE industry_mu1/2021-f180/config -17.47 27.99
PRIOR PASS industry_mu1/2024-eve/comparison:prior_sd_mu1=0.3
PRIOR PASS industry_mu1/2021-f180/comparison:prior_sd_mu1=0.3
PRIOR PASS poll_sd/2024-eve/config
PRIOR PASS poll_sd/2021-f180/config
PRIOR FAIL poll_sd/2024-eve/comparison:prior_mu_phi=0 12.63 38.21
PRIOR FAIL poll_sd/2021-f180/comparison:prior_mu_phi=0 12.13 38.11
PRIOR PASS kd_delta/2024-eve/config
PRIOR PASS kd_delta/2021-f180/config
PRIOR PASS walk_180/2024-eve/config
PRIOR PASS walk_180/2021-f180/config
PRIOR PASS industry_step/2024-eve/config
PRIOR PASS industry_step/2021-f180/config
PRIOR PASS newcomer_entry/2024-eve/config
PRIOR PASS newcomer_entry/2021-f180/config
```

## Reading the verdicts

- **PASS**: the quantiles are in the band.
- **WIDE**: a config row of a bias whose prior the config holds at §6.10's value, out of its band. §6.10 fixes that prior, so the row goes to Brynjólfur as information and does not stop the script.
- **FAIL (stops)**: a config row out of its band. The script stops after writing this report.
- **FAIL (comparison)**: a comparison row out of its band, reported as information; it never stops the script.
- A band catches a broken prior (a wrong scale, unit or sign), not a mistuned one. It is wide on purpose, and no band is a tuning target: the quantiles themselves are the result.
