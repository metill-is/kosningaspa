# Tier 0 diagnostics: arm v2s-m4

Written by `R/harness/tier0_report.R` from each run's `meta.json`.
Sampler diagnostics only. Tier 0 is not gated: the gate column is informational.

- Run list: `harness/runs/v2-steps.txt`
- CmdStan 2.38.0 and cmdstanr 0.9.0.9000, `max_treedepth` 12 (`meta.json`)
- hw.ncpu: 10 (`sysctl -n hw.ncpu`, read when this report was written)

| Cell (E, F, tag) | Seed | Iterations (warmup + sampling) | Minutes | Divergences | Treedepth hits (count; % of chains × sampling) | Deepest tree | Min E-BFMI | Max R-hat (gate scope: parameters block + scored; scored) | Min bulk ESS (gate scope: parameters block + scored; scored) | Min tail ESS (gate scope: parameters block + scored; scored) | Bulk ESS/min (gate scope: parameters block + scored; scored) | Bulk ESS at 2000 sampling iterations, linear projection (gate scope: parameters block + scored; scored) | Min bulk ESS per 1,000 gradients (gate scope) | Gate (§5.7, informational) | meta.json written |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024-11-30, 2024-11-29, eve | 2701 | 400 + 400 | 77.8 | 0 | 0 (0.0%) | 10 | 0.698 | 1.0624; 1.0056 | 50; 1406 | 225; 1033 | 0.6; 18.0 | 251; 7032 | 0.030 | FAIL: max R-hat 1.0624 > 1.01 | 2026-10-30 08:43 UTC |
| 2024-11-30, 2024-11-29, eve | 2702 | 400 + 400 | 45.1 | 0 | 0 (0.0%) | 10 | 0.607 | 1.0352; 1.0055 | 117; 1441 | 264; 947 | 2.6; 31.9 | 585; 7205 | 0.071 | FAIL: max R-hat 1.0352 > 1.01 | 2026-11-04 13:05 UTC |
| 2021-09-25, 2021-03-29, f180 | 2701 | 400 + 400 | 24.7 | 0 | 0 (0.0%) | 10 | 0.541 | 1.0372; 1.0067 | 105; 1397 | 148; 899 | 4.2; 56.4 | 528; 6987 | 0.064 | FAIL: max R-hat 1.0371 > 1.01 | 2026-10-30 07:50 UTC |
| 2021-09-25, 2021-03-29, f180 | 2702 | 400 + 400 | 50.2 | 0 | 0 (0.0%) | 10 | 0.508 | 1.0257; 1.0067 | 126; 1559 | 166; 921 | 2.5; 31.0 | 634; 7795 | 0.079 | FAIL: max R-hat 1.0256 > 1.01 | 2026-10-30 08:40 UTC |

The columns:

- **Gate scope: parameters block + scored**, the first value of each pair, is `meta.json`'s `max_rhat`, `min_ess_bulk` and `min_ess_tail`: the extremes over every `parameters`-block variable and the scored quantities together, the scope of the gate's R-hat rule (the plan's Decision 3); when it equals the scored value, the scored set is the binding one. **Scored**, the second, is the scored quantities alone (`max_rhat_scored`, `min_ess_bulk_scored`, `min_ess_tail_scored`). A parameter with no R-hat or ESS (a constant) is left out of the gate scope; a scored quantity with none makes both values NA.
- The gate checks the gate-scope max R-hat and the scored min bulk ESS; a failing gate's reasons print the value that failed.
- R-hat is rounded up, and ESS, ESS/min and E-BFMI down, to the digits shown. `Inf`, `NaN` and `NA` are printed as the run recorded them.
- **Treedepth hits** are transitions that reached `max_treedepth`, as a percentage of the chains × sampling draws; the **deepest tree** is the deepest any transition reached.
- **Bulk ESS/min** is each min bulk ESS ÷ the minutes, the fit's wall time (warmup and sampling).
- **Bulk ESS at 2000 sampling iterations** scales each min bulk ESS by 2000 / `iter_sampling`. It is a linear projection that informs Decision 5, not the gate.
- **Min bulk ESS per 1,000 gradients (gate scope)** is 1000 × the gate-scope min bulk ESS ÷ `n_leapfrog`, the fit's post-warmup leapfrog steps (gradients) summed over chains (`diagnostics$n_leapfrog` of `meta.json`), and `–` for a `meta.json` without it, as in a run fitted before it was recorded. Rounded down. The Phase 2 plan's adoption rule (its Decision 6) compares it between two forms of one model.
- **meta.json written** is the modification time of the run's `meta.json`: the date of the measurement.
