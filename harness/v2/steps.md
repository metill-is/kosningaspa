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

**Provenance.** Each run's `meta.json` records the commit and the working tree's state at the
fit's start (`git_head`, `git_dirty`). The 2024 eve and 2021 f180 seed 2701 fits: 4bf2570,
clean. The 2021 f180 seed 2702 fit, started at 03:54: 4bf2570, dirty, probably with the edits
that fa8e826 committed at 04:09. The 2024 eve seed 2702 fit, started at 04:18: fa8e826,
clean. fa8e826 touched only `R/harness/compare_runs.R`, its test, `R/CLAUDE.md` and
`harness/README.md`, none of which a fit loads.

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
  because its gate-scope max R-hat (1.0162 to 1.0211) is above the scoring gate's 1.01. That gate
  decides whether a run may be scored. The Decision 5 stop's R-hat signal is 1.10, and a `dev`
  arm such as `v2s-m0` is never scored.
- **The minutes are a little loaded.** Light jobs ran beside the queue's 8 chains on the 10
  cores at times: from 03:53 to 04:09 a one-core test job, the `compare_runs.R` tests, and at
  other times read-only reviews and test runs. No fit ran unloaded, so every fit's minutes may
  read slightly high.

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-m0 harness/runs/v2-steps.txt harness/v2/steps/v2s-m0.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m0
```

Next: P1 (`v2s-p1`), the anchored walk, which the adoption rule compares with `v2s-m0` on these
cells and seeds.

## v2s-p1: the anchored walk (P-step)

`v2s-m0` with `walk_form = 1` (config `harness/configs/steps/v2s-p1.json`): the walk anchored
at the closed elections' dates, the parties with national votes centred at each anchor and the
absent ones non-centred from the previous anchor, Brownian bridges between the anchors and a
forward walk after the last (`Stan/CLAUDE.md`). It reparameterises the same model. Its log
density and generated quantities are tested against the R reference in
`tests/harness/test-v2-stan.R`, and its check against the reference walk is
`R/harness/v2_equivalence.R`, tested in `tests/harness/test-v2-equivalence.R`. This section is
that check's four results, the sampler check of its fits and the adoption rule's verdict.

**Equivalence.** The four checks of `EQUIVALENCE_REQUIRED`, as written to
`harness/v2/steps/v2s-p1-equivalence.csv` by `LC_ALL=en_US.UTF-8 Rscript
R/harness/v2_equivalence.R --config=harness/configs/steps/v2s-p1.json --form=walk
--cell=<E>,<F> --out=harness/v2/steps/v2s-p1-equivalence.csv` (2017 eve with `--n=20`, the
reversed row with `--reverse`). Every row: no point dropped, excluded at a cap or finite on one
side only, and each Jacobian term's negative control `fails`, as it must.

| Cell | Order | Kept points | Constant range | Tolerance | Max path error | Generated quantities: points; max relative difference | Controls (`ref_walk`, `new_anchor`, `new_bridge`, `new_forward`) | Verdict |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve | default | 30 posterior + 15 off-posterior | 6.003e-11 | 2.452e-08 | 5.773e-15 | 30; 9.076e-14 | fails, fails, fails, fails | PASS |
| 2021 f180 | default | 30 posterior + 15 off-posterior | 4.866e-11 | 7.501e-09 | 7.55e-15 | 30; 9.315e-14 | fails, fails, fails, fails | PASS |
| 2017 eve | default | 20 prior (no fit, no forecast) | 1.31e-10 | 1.306e-07 | 6.439e-15 | 20; 9.644e-15 | fails, fails, fails, fails | PASS |
| 2024 eve | reversed | 30 posterior + 15 off-posterior | 6.276e-11 | 1.267e-08 | 7.55e-15 | 30; 8.846e-14 | fails, fails, fails, fails | PASS |

**Fits.** Four fits at 400 + 400 on `v2s-m0`'s cells and seeds: 2024 eve (lane A) and 2021
f180 (lane B), each with seeds 2701 and 2702. They ran under `nohup zsh R/harness/queue.sh
results/harness/logs/v2s-p1-runs.txt 400 400`, a run list of the four `v2s-p1` lines of
`harness/runs/v2-steps.txt` (`harness/README.md`, Step checks). The queue started at
2026-10-29 07:13; its preflight (the full slow suite, the 2024-eve reversed equivalence test
among it) passed, the fits ran from 08:01 to 08:45, all four exited 0, and `queue.log` ends in
`QUEUE FINISHED`. CmdStan 2.38.0, `max_treedepth` 12, 10 cores (`hw.ncpu`). The diagnostics
table is `harness/v2/steps/v2s-p1.md`.

**Provenance.** All four fits ran at c981e7e: each `meta.json` records `git_head` c981e7e and
`git_dirty` true, probably because of the then untracked equivalence CSV. The CSV's four rows
came from f87e0b6's script. c981e7e changed that script's refusals, its provisional rows and its
control rule (a control that is not exercised now fails a walk check; all four rows' controls
are `fails`, so the rule does not change their verdicts), but none of the point check's
arithmetic, and left `Stan/forecast_v2.stan` byte-identical: every row's `stan_sha256` is the
file's at both commits, `0495827175f177b3f618a4f171b97625c28ffce0ebe958623782f952b89cddf6`.

**Stop rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-p1` (exit 0):

```
GO v2s-p1_2024-11-30_2024-11-29_s2701 lp_rhat=1.0033 divergent=0/1600 near_cap=0/1600 max_rhat=1.0210; recorded: minutes=23.9 treedepth_hits=0 ess_bulk_per_1k_grad=0.730
GO v2s-p1_2024-11-30_2024-11-29_s2702 lp_rhat=1.0081 divergent=0/1600 near_cap=0/1600 max_rhat=1.0230; recorded: minutes=19.2 treedepth_hits=0 ess_bulk_per_1k_grad=0.597
GO v2s-p1_2021-09-25_2021-03-29_s2701 lp_rhat=1.0041 divergent=9/1600 near_cap=1/1600 max_rhat=1.0202; recorded: minutes=17.3 treedepth_hits=0 ess_bulk_per_1k_grad=0.598
GO v2s-p1_2021-09-25_2021-03-29_s2702 lp_rhat=1.0124 divergent=1/1600 near_cap=0/1600 max_rhat=1.0205; recorded: minutes=14.4 treedepth_hits=0 ess_bulk_per_1k_grad=0.461
```

| Fit (cell, seed) | Verdict | R-hat of `lp__` (STOP above 1.1) | Divergences (STOP above 16) | Draws within 0.5 of a cap (STOP above 16) | Max R-hat, gate scope (STOP above 1.10) | Minutes | Treedepth hits | Min bulk ESS per 1,000 gradients (gate scope) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | GO | 1.0033 | 0 of 1,600 | 0 of 1,600 | 1.0210 | 23.9 | 0 | 0.730 |
| 2024 eve, 2702 | GO | 1.0081 | 0 of 1,600 | 0 of 1,600 | 1.0230 | 19.2 | 0 | 0.597 |
| 2021 f180, 2701 | GO | 1.0041 | 9 of 1,600 | 1 of 1,600 | 1.0202 | 17.3 | 0 | 0.598 |
| 2021 f180, 2702 | GO | 1.0124 | 1 of 1,600 | 0 of 1,600 | 1.0205 | 14.4 | 0 | 0.461 |

**Adoption rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R adopt v2s-m0 v2s-p1
harness/v2/steps/v2s-p1-equivalence.csv` (exit 0), its output verbatim:

Adoption rule (Decision 6): v2s-p1 against the reference form v2s-m0; run list harness/runs/v2-steps.txt

| Fit | Min bulk ESS per 1,000 gradients (gate scope) | Divergences | Stop rule |
| --- | --- | --- | --- |
| v2s-m0_2024-11-30_2024-11-29_s2701 | 0.254 | 0 | GO |
| v2s-m0_2024-11-30_2024-11-29_s2702 | 0.252 | 0 | GO |
| v2s-m0_2021-09-25_2021-03-29_s2701 | 0.273 | 0 | GO |
| v2s-m0_2021-09-25_2021-03-29_s2702 | 0.268 | 0 | GO |
| v2s-p1_2024-11-30_2024-11-29_s2701 | 0.730 | 0 | GO |
| v2s-p1_2024-11-30_2024-11-29_s2702 | 0.597 | 0 | GO |
| v2s-p1_2021-09-25_2021-03-29_s2701 | 0.598 | 9 | GO |
| v2s-p1_2021-09-25_2021-03-29_s2702 | 0.461 | 1 | GO |

| Cell (E, F, tag) | v2s-m0: median min bulk ESS per 1,000 gradients | v2s-p1: median min bulk ESS per 1,000 gradients | Ratio | At least 1.25 × v2s-m0's |
| --- | --- | --- | --- | --- |
| 2024-11-30, 2024-11-29, eve | 0.253 | 0.664 | 2.62 | yes |
| 2021-09-25, 2021-03-29, f180 | 0.270 | 0.530 | 1.95 | yes |

1. The equivalence check passes (harness/v2/steps/v2s-p1-equivalence.csv): yes (4 rows, none FAIL, a PASS row of each of the 4 required checks)
2. On each cell, v2s-p1's median min bulk ESS per 1,000 gradients is at least 1.25 × v2s-m0's: yes
3. v2s-p1's divergences over its 4 fits, 10, are at most v2s-m0's, 0, plus 4: no
4. No fit of v2s-m0 or v2s-p1 trips the stop rule: yes
KEEP v2s-m0: v2s-p1 fails condition 3

**Verdict: KEEP v2s-m0.** `v2s-p1` fails condition 3: its 10 divergences (9 and 1, both on 2021
f180) are more than `v2s-m0`'s 0 plus 4. Conditions 1, 2 (cell ratios 2.62 and 1.95) and 4
hold. The rule's verdict stands: every later step's config sets `walk_form = 0`, and the
anchored form stays built behind its switch, with its tests and its equivalence check.

- **Where the divergences are.** 9 of the 1,600 post-warmup transitions of 2021 f180 seed 2701
  and 1 of seed 2702's; none on 2024 eve. `meta.json` keeps only their sum and the CmdStan CSVs
  were deleted, so the chains are not known. The one draw within 0.5 of a cap, also on 2021
  f180 seed 2701, is `log_kappa`'s (cap 2) in chain 2, iteration 386. Not chased further.
- **The pre-entry check.** The absent parties' `anchor_w` coordinates are those of the parties
  with no national votes in an anchor's result (`idx_res`), on both cells: Miðflokkurinn
  (`anchor_w[1,7]`) and Sósíalistaflokkurinn (`anchor_w[1,8]`) at the 2016-10-29 anchor, and
  Sósíalistaflokkurinn (`anchor_w[2,8]`) at the 2017-10-28 anchor. Their bulk ESS, from each
  run's `key_diagnostics.csv` (ESS rounded down; every R-hat at most 1.0053, rounded up):

  | Fit (cell, seed) | `anchor_w[1,7]` | `anchor_w[1,8]` | `anchor_w[2,8]` | Min | Gate-scope min bulk ESS |
  | --- | --- | --- | --- | --- | --- |
  | 2024 eve, 2701 | 804.9 | 1327.9 | 1074.5 | 804.9 | 298.1 |
  | 2024 eve, 2702 | 867.7 | 1009.5 | 971.1 | 867.7 | 243.7 |
  | 2021 f180, 2701 | 1243.5 | 1602.5 | 1619.4 | 1243.5 | 487.6 |
  | 2021 f180, 2702 | 907.1 | 1525.6 | 1303.2 | 907.1 | 377.0 |

- **The ratio over equal scopes, which has no say in the verdict.** The gate scope holds
  `anchor_w` for `v2s-p1` only, so the adoption rule's ratio compares unequal scopes. With
  `anchor_w` left out nothing changes: each `v2s-p1` run's gate-scope min is another variable's
  bulk ESS (`tau_sd`'s, and `log_phi[2]`'s on 2021 f180 seed 2702), well below every
  `anchor_w` element's. Its min bulk ESS per 1,000 gradients stays 0.7308, 0.5975, 0.5986 and
  0.4613 (2024 eve 2701, 2702, 2021 f180 2701, 2702; rounded down) against `v2s-m0`'s 0.2547,
  0.2520, 0.2733 and 0.2683, and the cell ratios stay 2.62 and 1.95. Condition 3, not this
  ratio, decided the verdict.
- **The §5.7 gate is not the stop rule.** Each fit's DIAG line reads `gate=FAIL`: its gate-scope
  max R-hat (1.0202 to 1.0230) is above 1.01, and the 2021 f180 fits diverged. A `dev` arm is
  never scored.
- **The minutes are a little loaded.** Light jobs ran beside the queue at times (a read-only
  review, fast test runs). No fit ran unloaded, so every fit's minutes may read slightly high.
  The adoption rule reads ESS per gradient, not per minute.

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-p1 harness/runs/v2-steps.txt harness/v2/steps/v2s-p1.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-p1
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R adopt v2s-m0 v2s-p1 harness/v2/steps/v2s-p1-equivalence.csv
```

Next: M1 (`v2s-m1`), epoch bias, on `v2s-m0`'s reference forms (`walk_form = 0`).
