# V2 Tier 0 checks

Written by `R/harness/v2_checks.R` from each run's `meta.json`, `param_draws.parquet`, `ppc_results.parquet` and stage-1 record, and the closed elections' official counts in `harness/truth/elections_official.csv`: the extras of spec §6.9 beside the Tier 0 report (`harness/tier0/<arm>.md`). No forecast quantity is read or printed.

- Run list: `harness/runs/tier0-v2.txt`

## Parameters and compute

| Arm | Cell (E, F, tag) | Seed | Iterations (warmup + sampling) | Minutes | Minutes projected to 1000 + 2000 | Over 120 projected minutes (spec §6.9) | Corr(log φ_K, s̄_ζ) | Draws with log φ_h within 2; within 0.5 of cap 20, max over houses | Draws with log φ_K within 2; within 0.5 of cap 20 | Draws with log κ within 2; within 0.5 of cap 2 | Stage 1: draws with log φ_f within 2; within 0.5 of cap 20 | Min bulk ESS over the entries of CΣCᵀ | Min bulk ESS over the off-diagonals of Ω = L_Omega L_Omegaᵀ |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| v2 | 2024-11-30, 2024-11-29, eve | 1201 | 200 + 200 | 28.4 | 212.9 | yes | 0.261 | 0 of 800 (0.0%); 0 of 800 (0.0%) | 0 of 800 (0.0%); 0 of 800 (0.0%) | 100 of 800 (12.5%); 0 of 800 (0.0%) | 0 of 4000 (0.0%); 0 of 4000 (0.0%) | 224 | 322 |
| v2 | 2021-09-25, 2021-03-29, f180 | 1202 | 200 + 200 | 12.7 | 95.0 | no | 0.390 | 0 of 800 (0.0%); 0 of 800 (0.0%) | 0 of 800 (0.0%); 0 of 800 (0.0%) | 252 of 800 (31.5%); 0 of 800 (0.0%) | 0 of 4000 (0.0%); 0 of 4000 (0.0%) | 234 | 636 |
| v2-perm | 2024-11-30, 2024-11-29, eve | 1201 | 200 + 200 | 26.7 | 200.1 | yes | 0.303 | 0 of 800 (0.0%); 0 of 800 (0.0%) | 0 of 800 (0.0%); 0 of 800 (0.0%) | 99 of 800 (12.4%); 0 of 800 (0.0%) | 0 of 4000 (0.0%); 0 of 4000 (0.0%) | 231 | 235 |
| v2-perm | 2021-09-25, 2021-03-29, f180 | 1202 | 200 + 200 | 12.9 | 96.7 | no | 0.475 | 0 of 800 (0.0%); 0 of 800 (0.0%) | 0 of 800 (0.0%); 0 of 800 (0.0%) | 284 of 800 (35.5%); 0 of 800 (0.0%) | 0 of 4000 (0.0%); 0 of 4000 (0.0%) | 199 | 211 |

## Past elections: in-sample PIT under `y_rep_res`

| Arm | Cell (E, F, tag) | Seed | Closed election | Cells with PIT outside [0.025, 0.975] | Cells |
| --- | --- | --- | --- | --- | --- |
| v2 | 2024-11-30, 2024-11-29, eve | 1201 | 2016-10-29 | 0 | 48 |
| v2 | 2024-11-30, 2024-11-29, eve | 1201 | 2017-10-28 | 0 | 54 |
| v2 | 2024-11-30, 2024-11-29, eve | 1201 | 2021-09-25 | 0 | 60 |
| v2 | 2021-09-25, 2021-03-29, f180 | 1202 | 2016-10-29 | 0 | 48 |
| v2 | 2021-09-25, 2021-03-29, f180 | 1202 | 2017-10-28 | 0 | 54 |
| v2-perm | 2024-11-30, 2024-11-29, eve | 1201 | 2016-10-29 | 0 | 48 |
| v2-perm | 2024-11-30, 2024-11-29, eve | 1201 | 2017-10-28 | 0 | 54 |
| v2-perm | 2024-11-30, 2024-11-29, eve | 1201 | 2021-09-25 | 0 | 60 |
| v2-perm | 2021-09-25, 2021-03-29, f180 | 1202 | 2016-10-29 | 0 | 48 |
| v2-perm | 2021-09-25, 2021-03-29, f180 | 1202 | 2017-10-28 | 0 | 54 |

The columns:

- **Minutes projected to 1000 + 2000** is `meta.json`'s `minutes` × 3000 ÷ (`iter_warmup` + `iter_sampling`): the fit's wall time scaled linearly to spec §6.9's 4 × (1000 + 2000). **Over 120 projected minutes** is `yes` strictly above 120: spec §6.9 asks for a report before Tier 1 when a V2 fit takes over 2 h, and iterations are never cut for one arm alone.
- **Corr(log φ_K, s̄_ζ)** is the Pearson correlation of `log_phi_K` and `s_bar_zeta[1]` over all draws, the chains pooled. δ is close to saturated per cell, so the cell-level concentration and the δ walk's scale may trade off (spec §6.6).
- **Draws with log c within 2; within 0.5 of its cap** count the chains × sampling draws in which a bounded log concentration lies within 2 (Decision 4's margin, `V2_CAP_MARGIN`) and within 0.5 (the stop rule's, `NEAR_CAP` in `R/harness/compare_runs.R`) of its cap, on the log scale the model uses: log c > cap − 2 and log c > cap − 0.5. The stop rule counts cap − log c ≤ 0.5, which differs only for a draw exactly on the edge, and counts a draw once across every house and concentration, so its `near_cap` is at least the 0.5 count of each column but stage 1's and need not equal any of them. The caps are `V2_LOG_CAPS` and `V2_STAGE1_LOG_CAP` (`R/harness/v2_reference.R`). log φ_h has one column per house: for each margin the cell gives the largest count over the houses, and the house that has it (`meta.json`'s `house_levels`) when that count is not 0. **Stage 1**'s log φ_f is counted over stage 1's own draws when it is fitted, and read from its cached record (`results_root()/stage1/<stage1_sha256>/mu_f.json`); `not recorded` marks a record without current counts, as one written before they were, and `no stage 1` a run without a stage 1.
- **Min bulk ESS over the entries of CΣCᵀ**: per draw, Σ = diag(σ) Ω diag(σ) with Ω = `L_Omega` `L_Omega`ᵀ and C = I − 11ᵀ/P over the run's P parties, the walk's identified innovation covariance (spec §6.2); then `posterior::ess_bulk()` over the chains of each of its P(P + 1)/2 distinct entries, and the min. **Min bulk ESS over the off-diagonals of Ω** is the same over Ω's P(P − 1)/2 off-diagonal entries. Only CΣCᵀ is identified, so a low ESS in Ω alone is a flat direction of the likelihood. Both are rounded down.
- **Cells with PIT outside [0.025, 0.975]**: for each kjördæmi × party cell of a closed election, the mid-PIT of the official count y under the in-sample replicates `y_rep_res` (`ppc_results.parquet`), P(y_rep < y) + ½ P(y_rep = y) over the draws, y matched by election, kjördæmi and party name (`harness/truth/elections_official.csv`, folded to the poll parties). A PIT of exactly 0.025 or 0.975 is inside. **Cells** counts the cells of the parties with an official count at that election; a party without a list there, whose replicates are 0, is left out. Counts only: no replicate or official count is printed.

## Permutation check

The output of `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R perm v2 v2-perm harness/runs/tier0-v2.txt` (the Phase 2 plan's Decision 8), as it printed:

```
PERM PASS v2_2024-11-30_2024-11-29_s1201 vs v2-perm_2024-11-30_2024-11-29_s1201: 280 z's, 0 with |z| > 3, max |z| 2.72, sd of the mean z's 0.75
PERM PASS v2_2021-09-25_2021-03-29_s1202 vs v2-perm_2021-09-25_2021-03-29_s1202: 280 z's, 1 with |z| > 3, max |z| 4.90, sd of the mean z's 0.73
  |z| > 3: eday_kjordaemi[Reykjavík Suður,Vinstri Græn] q95
```
