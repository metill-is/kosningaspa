# V2 step log

The staircase of arm V2 (the Phase 2 plan's step protocol), one section per step, in order. Each
step's sampler-check fits are lines of `harness/runs/v2-steps.txt`. Their diagnostics table is
`harness/v2/steps/<step>.md`, written by `R/harness/tier0_report.R` from each run's `meta.json`,
and their verdicts below come from `R/harness/compare_runs.R`: the stop rule (Decision 5) for an
M-step, and the adoption rule (Decision 6) for a P-step (`harness/README.md`, Step checks). No
forecast quantity is read or shown here.

## v2s-m0: the core (M-step)

The core of V2 (config `harness/configs/steps/v2s-m0.json`): the walk, the national and
kjördæmi polls, the houses, a static δ, the bounded concentrations, and the plan's proposals 1
and 2 (s_δ0), with proposal 4's scales as data. Its lp, mutant and permutation tests are in
`tests/harness/test-v2-stan.R`, and its prior predictive check is in
`harness/v2/prior_predictive.md`. This section is its sampler check.

**Fits.** Four fits at 400 + 400: 2024 eve (lane A) and 2021 f180 (lane B), each with seeds
2701 and 2702. Seed 2702 is there because the next step, P1, is a P-step. They ran under
`zsh R/harness/queue.sh harness/runs/v2-steps.txt 400 400` from 2026-10-29 03:18 to 04:59.
The queue's preflight (the full slow suite) passed, all four fits exited 0, and `queue.log`
ends in `QUEUE FINISHED`. CmdStan 2.38.0, `max_treedepth` 12, 10 cores (`hw.ncpu`). The
diagnostics table is `harness/v2/steps/v2s-m0.md`.

**Stop rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m0` (exit 0):

```
GO v2s-m0_2024-11-30_2024-11-29_s2701 lp_rhat=1.0118 divergent=0/1600 near_cap=0/1600 max_rhat=1.0211; recorded: minutes=44.2 treedepth_hits=0 ess_bulk_per_1k_grad=0.254
GO v2s-m0_2024-11-30_2024-11-29_s2702 lp_rhat=1.0233 divergent=0/1600 near_cap=0/1600 max_rhat=1.0162; recorded: minutes=41.2 treedepth_hits=0 ess_bulk_per_1k_grad=0.252
GO v2s-m0_2021-09-25_2021-03-29_s2701 lp_rhat=1.0108 divergent=0/1600 near_cap=0/1600 max_rhat=1.0206; recorded: minutes=21.1 treedepth_hits=0 ess_bulk_per_1k_grad=0.273
GO v2s-m0_2021-09-25_2021-03-29_s2702 lp_rhat=1.0147 divergent=0/1600 near_cap=0/1600 max_rhat=1.0165; recorded: minutes=22.0 treedepth_hits=0 ess_bulk_per_1k_grad=0.268
```

| Fit (cell, seed) | Verdict | R-hat of `lp__` (STOP above 1.1) | Divergences (STOP above 16) | Draws within 0.5 of a cap (STOP above 16) | Max R-hat, gate scope (STOP above 1.10) | Minutes | Treedepth hits | Min bulk ESS per 1,000 gradients (gate scope) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | GO | 1.0118 | 0 of 1,600 | 0 of 1,600 | 1.0211 | 44.2 | 0 | 0.254 |
| 2024 eve, 2702 | GO | 1.0233 | 0 of 1,600 | 0 of 1,600 | 1.0162 | 41.2 | 0 | 0.252 |
| 2021 f180, 2701 | GO | 1.0108 | 0 of 1,600 | 0 of 1,600 | 1.0206 | 21.1 | 0 | 0.273 |
| 2021 f180, 2702 | GO | 1.0147 | 0 of 1,600 | 0 of 1,600 | 1.0165 | 22.0 | 0 | 0.268 |

**Verdict: GO on all four fits.** The stop rule's signals are all well inside their limits: the
largest R-hat of `lp__` is 1.0233, no transition diverged, no draw came within 0.5 of a cap,
and the largest gate-scope R-hat is 1.0211. Minutes, treedepth hits and ESS per 1,000 gradients
are recorded, never a stop.

- **The §5.7 gate is not the stop rule.** Each fit's DIAG line in `queue.log` reads `gate=FAIL`,
  because its gate-scope max R-hat (1.0161 to 1.0211) is above the scoring gate's 1.01. That gate
  decides whether a run may be scored. The Decision 5 stop's R-hat signal is 1.10, and a `dev`
  arm such as `v2s-m0` is never scored.
- **The minutes are a little loaded.** From 03:53 to 04:09 a one-core test job, the
  `compare_runs.R` tests, ran beside the queue's 8 chains on the 10 cores. That overlaps the 2024
  eve seed 2701 fit and both 2021 f180 fits, so their minutes may read slightly high. The 2024
  eve seed 2702 fit started at 04:18, after the job ended.

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-m0 harness/runs/v2-steps.txt harness/v2/steps/v2s-m0.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m0
```

Next: P1 (`v2s-p1`), the anchored walk, which the adoption rule compares with `v2s-m0` on these
cells and seeds.
