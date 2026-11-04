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
  other times read-only reviews and test runs. No fit is known to have run unloaded, so every
  fit's minutes may read slightly high.

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
forward walk after the last (`Stan/CLAUDE.md`). It reparameterises the same model. The R
reference `v2_lp_ref()` refuses `walk_form = 1`, so the three `v2s-p1` tests in
`tests/harness/test-v2-stan.R` cover what needs no reference density: transformed data's
rejects of an anchored walk it cannot build, the parameters' sizes and the reference walk's free
size, and each anchor at its result when the present levels are 0. Its log density and
generated quantities are checked against the reference walk by `R/harness/v2_equivalence.R`
(tested in `tests/harness/test-v2-equivalence.R`), whose reference side is the form that
`test-v2-stan.R` tests against `v2_lp_ref()`. This section is that check's four results, the
sampler check of its fits and the adoption rule's verdict.

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
came from f87e0b6's script. c981e7e changed that script's refusals, its provisional rows, its
control rule (a control that is not exercised now fails a walk check; all four rows' controls
are `fails`, so the rule does not change their verdicts), the coordinate-party stop and line,
and the `MIN_KEPT` gate on the generated-quantities comparison (every row compared 20 or 30
points, so the gate does not change them either), but none of the point check's arithmetic,
and left `Stan/forecast_v2.stan` byte-identical: every row's `stan_sha256` is the file's at
both commits, `0495827175f177b3f618a4f171b97625c28ffce0ebe958623782f952b89cddf6`. The
`v2s-m0` fits ran with the file at sha256 `430939e21f0d…` and the `v2s-p1` fits at
`0495827175f1…` (each `meta.json`'s `stan_sha256`); the `walk_form = 0` model is pinned across
that edit by the M-step lp tests, which passed at f87e0b6 (`HARNESS_SLOW=1 V2_STEPS=v2s-m0`).

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

```
1. The equivalence check passes (harness/v2/steps/v2s-p1-equivalence.csv): yes (4 rows, none FAIL, a PASS row of each of the 4 required checks)
2. On each cell, v2s-p1's median min bulk ESS per 1,000 gradients is at least 1.25 × v2s-m0's: yes
3. v2s-p1's divergences over its 4 fits, 10, are at most v2s-m0's, 0, plus 4: no
4. No fit of v2s-m0 or v2s-p1 trips the stop rule: yes
KEEP v2s-m0: v2s-p1 fails condition 3
```

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

- **The ratio with `anchor_w` left out, which has no say in the verdict.** The gate scope
  holds `anchor_w` for `v2s-p1` only, so the adoption rule's ratio compares unequal scopes; with
  `anchor_w` left out they are still not equal, because the two forms' `z_walk` differ in size.
  Leaving `anchor_w` out changes nothing: each `v2s-p1` run's gate-scope min is another
  parameter's bulk ESS (`tau_sd`'s, and on 2021 f180 seed 2702 `log_phi_u[2]`'s, which
  `key_diagnostics.csv` lists through its transform `log_phi[2]`), well below every
  `anchor_w` element's. Its min bulk ESS per 1,000 gradients stays 0.7308, 0.5975, 0.5986 and
  0.4613 (2024 eve 2701, 2702, 2021 f180 2701, 2702; rounded down) against `v2s-m0`'s 0.2547,
  0.2520, 0.2733 and 0.2683, and the cell ratios stay 2.62 and 1.95. Condition 3, not this
  ratio, decided the verdict.
- **The §5.7 gate is not the stop rule.** Each fit's DIAG line reads `gate=FAIL`: its gate-scope
  max R-hat (1.0202 to 1.0230) is above 1.01, and the 2021 f180 fits diverged. A `dev` arm is
  never scored.
- **The minutes are a little loaded.** Light jobs ran beside the queue at times (a read-only
  review, fast test runs). No fit is known to have run unloaded, so every fit's minutes may
  read slightly high. The adoption rule reads ESS per gradient, not per minute.

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-p1 harness/runs/v2-steps.txt harness/v2/steps/v2s-p1.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-p1
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R adopt v2s-m0 v2s-p1 harness/v2/steps/v2s-p1-equivalence.csv
```

Next: M1 (`v2s-m1`), epoch bias, on `v2s-m0`'s reference forms (`walk_form = 0`).

## v2s-m1: the epoch bias (M-step)

`v2s-m0` with `epoch_bias = 1` (config `harness/configs/steps/v2s-m1.json`, `walk_form = 0`
after P1's `KEEP v2s-m0`): the epoch bias of spec §6.4. The industry bias μ takes a centred
step C(τ ⊙ ξ_e) into each epoch from the second, with τ_p = τ_μ for every party but Annað,
whose own scale is τ_μ,A (the data's `annad_idx`). Each house's deviations walk by
C(τ_house ζ) after its first epoch, the epoch of its first national poll (`house_first_epoch`).
A newcomer restarts at its entry epoch e_p. Its lp, mutant and natural-quantity tests are in
`tests/harness/test-v2-stan.R`, its reference tests (the restart's properties among them) in
`tests/harness/test-v2-reference.R`, and its prior predictive row in
`harness/v2/prior_predictive.md`. This section is its sampler check.

**Fits.** Two fits at 400 + 400, seed 2701: 2024 eve (lane A) and 2021 f180 (lane B), the two
`v2s-m1` lines of `harness/runs/v2-steps.txt`. They ran under `R/harness/queue.sh` with the
run list `results/harness/logs/v2s-m1-runs.txt` (`harness/README.md`, Step checks). The queue
started at 2026-10-29 10:45:24, and its preflight (the full slow suite) passed. The fits ran
from 11:35 to 12:30, both exited 0, and `queue.log` ends in `QUEUE FINISHED`. CmdStan 2.38.0,
`max_treedepth` 12, 10 cores (`hw.ncpu`). The diagnostics table is
`harness/v2/steps/v2s-m1.md`.

**Provenance.** Both fits ran at 5a53130 on a clean tree (`meta.json`: `git_head` 5a53130,
`git_dirty` false), with `Stan/forecast_v2.stan` at sha256
`34b8fbc70743abfdeef25ec2fc89bc4296fd96345389227e242bde26009a5a2b`. The next commit, 3b9503a,
changed no Stan line; the file's sha256 is the same there and in this step's commit. 3b9503a
also changed how `v2_stan_data()` builds `house_first_epoch` (national polls only), which
leaves both cells' data byte-identical. Each run's `meta.json` `data_sha256` equals
`stan_data_sha()` (`R/harness/runs.R`) of the data that 3b9503a's builder gives for the same
cell through the arm, as `fit_cell.R` builds it, and equals the sha256 of the run's frozen
`inputs/stan_data.json`:

| Fit (cell, seed) | `git_head`, `git_dirty` | `data_sha256` (`meta.json`) | `stan_data_sha()` at 3b9503a |
| --- | --- | --- | --- |
| 2024 eve, 2701 | 5a53130, false | `0a8f392b5e19f2ea71ed4278f2d913adee1768fbfc16f580eddff1c4e94aaf06` | the same |
| 2021 f180, 2701 | 5a53130, false | `9616f525a8005c80e8b1e5de6f0931e87bcf689f204212bd5bb130c5d8655ca5` | the same |

To re-measure a cell's sha, from the project root, with the cell's E and F as the two
arguments (`2024-11-30 2024-11-29` for 2024 eve and `2021-09-25 2021-03-29` for 2021 f180, the
two lines below). It reproduces the recorded sha only on a tree whose `R/harness` builder and
`harness/configs/steps/v2s-m1.json` build the data as 3b9503a's do; after a later change to
either it prints another sha, which is not a provenance failure:

```
LC_ALL=en_US.UTF-8 Rscript -e 'x <- commandArgs(TRUE); options(box.path = getwd()); box::use(R/harness/arms[arm], R/harness/info_set[info_set], R/harness/runs[stan_data_sha]); a <- arm("v2s-m1"); cat(stan_data_sha(a$build(info_set(as.Date(x[1]), as.Date(x[2])), a$read_config(a$config))$data), "\n", sep = "")' 2024-11-30 2024-11-29
LC_ALL=en_US.UTF-8 Rscript -e 'x <- commandArgs(TRUE); options(box.path = getwd()); box::use(R/harness/arms[arm], R/harness/info_set[info_set], R/harness/runs[stan_data_sha]); a <- arm("v2s-m1"); cat(stan_data_sha(a$build(info_set(as.Date(x[1]), as.Date(x[2])), a$read_config(a$config))$data), "\n", sep = "")' 2021-09-25 2021-03-29
```

**Stop rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m1` (exit 0):

```
GO v2s-m1_2024-11-30_2024-11-29_s2701 lp_rhat=1.0099 divergent=0/1600 near_cap=0/1600 max_rhat=1.0177; recorded: minutes=54.6 treedepth_hits=0 ess_bulk_per_1k_grad=0.197
GO v2s-m1_2021-09-25_2021-03-29_s2701 lp_rhat=1.0042 divergent=0/1600 near_cap=0/1600 max_rhat=1.0198; recorded: minutes=29.5 treedepth_hits=0 ess_bulk_per_1k_grad=0.347
```

| Fit (cell, seed) | Verdict | R-hat of `lp__` (STOP above 1.1) | Divergences (STOP above 16) | Draws within 0.5 of a cap (STOP above 16) | Max R-hat, gate scope (STOP above 1.10) | Minutes | Treedepth hits | Min bulk ESS per 1,000 gradients (gate scope) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | GO | 1.0099 | 0 of 1,600 | 0 of 1,600 | 1.0177 | 54.6 | 0 | 0.197 |
| 2021 f180, 2701 | GO | 1.0042 | 0 of 1,600 | 0 of 1,600 | 1.0198 | 29.5 | 0 | 0.347 |

**Verdict: GO on both fits.** The stop rule's signals are all well inside their limits:
- the largest R-hat of `lp__` is 1.0099;
- no transition diverged;
- no draw came within 0.5 of a cap;
- the largest gate-scope R-hat is 1.0198.

Minutes, treedepth hits and ESS per 1,000 gradients are recorded, never a stop.

- **The §5.7 gate is not the stop rule.** Each fit's DIAG line in `queue.log` reads `gate=FAIL`,
  because its gate-scope max R-hat (1.0177 and 1.0197 as DIAG prints them; the stop rule
  rounds the second up to 1.0198) is above the scoring gate's 1.01. A `dev` arm is never
  scored.
- **The minutes read high.** Other work shared the 10 cores while the fits ran. 2021 f180
  finished at 12:05:02 and 2024 eve at 12:30:13 (each `meta.json`'s modification time). Beside
  both fits ran:
  - single-file test runs (the reference, extract, escapes, data, arms, prior predictive and
    run-comparison tests), from about 11:47 to 11:52 and 12:03 to 12:05;
  - a scratch script that compiled `Stan/forecast_v2.stan` and ran three one-draw
    fixed_param runs, from 11:52 to 12:00, with the load average about 25 at its start;
  - the prior predictive report's regeneration, from 12:00 to 12:03;
  - a process outside the harness, seen at about one core at 11:52 and 12:00.

  The whole fast suite started at about 12:05:06, after 2021 f180 had finished, so it ran
  beside 2024 eve only, until 12:08. No slow test ran beside either fit: the slow suite ran in
  the preflight, before the fits. No fit is known to have run unloaded, so both fits' minutes
  read high, by an unknown amount.

**The newcomer restart.** At a newcomer's entry epoch e_p, its μ restarts at the entry mean:
the mean over the parties existing before e_p of μ_{e_p}, plus m_new + s_new η (0 and 0.2023
here), and then μ_{e_p} is re-centred. In each house already walking, the restart sets the
newcomer's element of the uncentred level to σ_γ ε and then centres. Under this reading no
prior-only drift from before e_p reaches the likelihood, not even 1/P of it through the
centring, and τ_house = 0 gives the core. Restarting the centred deviation instead would shift
the newcomer's contrast with every party by the house's mean uncentred level. Fast property
tests in `tests/harness/test-v2-reference.R` pin the reading:
- moving a newcomer's pre-entry industry or house steps changes the density only by their
  own std_normal prior;
- a step after entry reaches the likelihood;
- the entry value is the mean above.

Two mutants plant the carried drift: `no_mu_restart` and `no_house_restart`.

**What the lp test covers.** `HARNESS_SLOW=1 V2_STEPS=v2s-m1 LC_ALL=en_US.UTF-8 Rscript
tests/run.R v2-stan` (at 3b9503a, `[ FAIL 0 | WARN 0 | SKIP 13 | PASS 110 ]`). On each cell,
Stan's log density minus the reference's (`v2_lp_ref()`) is constant over the cell's points
within one tolerance per cell, tol = 1e-9 · max(1, |lp_ref|) with |lp_ref| the
largest over the cell's points (`lp_tol()`, `tests/harness/helper-v2.R`), and the points of
each kind (posterior, off-posterior, prior) are also checked against their own `lp_tol()`
(`expect_lp_matches()`):
- on 2024 eve and 2021 f180, over 20 posterior and 10 off-posterior points of a short fit and
  10 prior points;
- on 2017 eve, over 10 prior points only (no fit, no forecast).

Each mutant a cell exercises is rejected above 100 × tol: the core's five, and the epoch
bias's `house_walk_from_first_poll`, `newcomer_mean_of_mu1`, `no_mu_restart` and
`no_house_restart`. Its limit: `house_walk_from_first_poll` is not exercised on 2017 eve,
where every house starts in epoch 1, though it is on the other two cells. The default order
puts Annað last at §6.10's `prior_tau_mu` = 0.1 on every cell, where scaling the last party
instead of `annad_idx`, or a literal 0.1 for the data scale, would pass. A fourth check
therefore runs 2024 eve in the reversed order (`annad_idx = 1`) at `prior_tau_mu = 0.37`, on
10 prior points, and shows that each of those two slips, planted in the reference's data,
fails there. The test's lines:

```
v2s-m1 eve2024: 40 points, lp constant range 1.57e-09 (tol 0.000665, ratio 2.36e-06; ratio by kind: post 3.3e-06, off 2.37e-06, prior 2.36e-06); weakest exercised mutant next_epoch_delta; range / (100 tol): positional_gather 1.56e+06, previous_w 3.96e+03, next_epoch_delta 3.7e+03, close_last_kjordaemi 3e+05, conditional_walk 8.23e+05, house_walk_from_first_poll 7.48e+04, newcomer_mean_of_mu1 8.41e+03, no_mu_restart 1.43e+05, no_house_restart 4.1e+04; not exercised: none
v2s-m1 f180_2021: 40 points, lp constant range 2.33e-10 (tol 0.000264, ratio 8.82e-07; ratio by kind: post 4.62e-06, off 1.83e-06, prior 8.82e-07); weakest exercised mutant next_epoch_delta; range / (100 tol): positional_gather 2.26e+06, previous_w 1.46e+03, next_epoch_delta 1e+03, close_last_kjordaemi 5.31e+05, conditional_walk 2.05e+06, house_walk_from_first_poll 1.89e+03, newcomer_mean_of_mu1 2.52e+04, no_mu_restart 1.39e+05, no_house_restart 5.07e+04; not exercised: none
v2s-m1 eve2017: 10 points, lp constant range 1.02e-10 (tol 0.000104, ratio 9.78e-07; ratio by kind: prior 9.78e-07); weakest exercised mutant newcomer_mean_of_mu1; range / (100 tol): positional_gather 1.01e+06, close_last_kjordaemi 1.2e+06, conditional_walk 1.31e+06, newcomer_mean_of_mu1 915, no_mu_restart 7.25e+03, no_house_restart 4.58e+03; not exercised: house_walk_from_first_poll
v2s-m1 reversed eve2024 at prior_tau_mu 0.37: 10 prior points, lp constant range 9.31e-10 (tol 0.000557); range / (100 tol): annad_last 1.74e+06, literal_0.1 322
```

**The prior predictive row.** `industry_step`: one epoch's step in a 20% party's industry
bias, in pp, at `prior_tau_mu` = 0.1 (`harness/v2/prior_predictive.md`):

| Cell | Values | q05 | q50 | q95 | Band | Verdict |
| --- | --- | --- | --- | --- | --- | --- |
| 2024 eve | 3000 | -3.03 | -0.03 | 2.86 | [5%, 95%] inside [-6, 6] | PASS |
| 2021 f180 | 2000 | -2.72 | -0.04 | 2.63 | [5%, 95%] inside [-6, 6] | PASS |

```
PRIOR PASS industry_step/2024-eve/config
PRIOR PASS industry_step/2021-f180/config
```

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-m1 harness/runs/v2-steps.txt harness/v2/steps/v2s-m1.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m1
```

Next: M2 (`v2s-m2`), newcomer pooling (§6.4), on `v2s-m1`'s config with `newcomer_pool = 1`.

## v2s-m2: the newcomer pool (M-step)

`v2s-m1` with `newcomer_pool = 1` (config `harness/configs/steps/v2s-m2.json`): the newcomer
pool of spec §6.4. A newcomer's entry value is the mean of μ_{e_p} over the parties existing
before its entry epoch e_p (`entry_epoch` < e_p), plus m_new + s_new η, and μ_{e_p} is then
re-centred. The step estimates the contrast's mean m_new ~ N(0, 0.3) and scale
s_new ~ half-N(0, 0.3), which `newcomer_pool = 0` fixes at 0 and 0.2023, the median of that
half-normal to four digits. A fast test ties those literals between `Stan/forecast_v2.stan`
and the reference, and another compares every parameter declaration in the Stan file with
`V2_CORE_PARS`' types, s_new's `<lower=0>` pinned by name. Its lp, size, natural-quantity and
JSON round-trip tests are in `tests/harness/test-v2-stan.R`, its reference tests in
`tests/harness/test-v2-reference.R`, and its prior predictive row in
`harness/v2/prior_predictive.md`. This section is its sampler check.

**Fits.** Two fits at 400 + 400, seed 2701: 2024 eve (lane A) and 2021 f180 (lane B), the two
`v2s-m2` lines of `harness/runs/v2-steps.txt`. They ran under `R/harness/queue.sh` with the
run list `results/harness/logs/v2s-m2-runs.txt` (`harness/README.md`, Step checks). The queue
started at 2026-10-29 13:52:11, and its preflight (the full slow suite) passed. Both fits
started at 14:33:25. 2021 f180 finished at 15:02:45 and 2024 eve at 15:27:18 (`queue.log`'s
start lines; each `meta.json`'s modification time), both exited 0, and `queue.log` ends in
`QUEUE FINISHED`. CmdStan 2.38.0, `max_treedepth` 12, 10 cores (`hw.ncpu`). The diagnostics
table is `harness/v2/steps/v2s-m2.md`.

**Provenance.** Both fits ran at b0e4a69 on a clean tree (`meta.json`: `git_head` b0e4a69,
`git_dirty` false), with `Stan/forecast_v2.stan` at sha256
`934281745c266c4df9a20ceb9ac86e14d53c9f290a5236401e4ca1ff7415b3fa`. The next commit, f390638,
changed no Stan line and no file the builder reads: in `R/harness` it changed only
`v2_reference.R`'s roxygen and the prior predictive script. The Stan file's sha256 is the same
there and in this step's commit. Each run's `meta.json` `data_sha256` equals `stan_data_sha()`
(`R/harness/runs.R`) of the data that f390638's builder gives for the same cell through the
arm, as `fit_cell.R` builds it, and equals the sha256 of the run's frozen
`inputs/stan_data.json`:

| Fit (cell, seed) | `git_head`, `git_dirty` | `data_sha256` (`meta.json`) | `stan_data_sha()` at f390638 |
| --- | --- | --- | --- |
| 2024 eve, 2701 | b0e4a69, false | `e39fd297d6a81c892eef4bba1bc3c2ebb9069e333d35548925321271c83b4103` | the same |
| 2021 f180, 2701 | b0e4a69, false | `a44004d6b4f26768213882d3d9b12146c55536b79f256fd04cd6a1d00d926b76` | the same |

To re-measure a cell's sha, from the project root, with the cell's E and F as the two
arguments (`2024-11-30 2024-11-29` for 2024 eve and `2021-09-25 2021-03-29` for 2021 f180, the
two lines below). It reproduces the recorded sha only on a tree whose `R/harness` builder and
`harness/configs/steps/v2s-m2.json` build the data as f390638's do; after a later change to
either it prints another sha, which is not a provenance failure:

```
LC_ALL=en_US.UTF-8 Rscript -e 'x <- commandArgs(TRUE); options(box.path = getwd()); box::use(R/harness/arms[arm], R/harness/info_set[info_set], R/harness/runs[stan_data_sha]); a <- arm("v2s-m2"); cat(stan_data_sha(a$build(info_set(as.Date(x[1]), as.Date(x[2])), a$read_config(a$config))$data), "\n", sep = "")' 2024-11-30 2024-11-29
LC_ALL=en_US.UTF-8 Rscript -e 'x <- commandArgs(TRUE); options(box.path = getwd()); box::use(R/harness/arms[arm], R/harness/info_set[info_set], R/harness/runs[stan_data_sha]); a <- arm("v2s-m2"); cat(stan_data_sha(a$build(info_set(as.Date(x[1]), as.Date(x[2])), a$read_config(a$config))$data), "\n", sep = "")' 2021-09-25 2021-03-29
```

**Stop rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m2` (exit 0):

```
GO v2s-m2_2024-11-30_2024-11-29_s2701 lp_rhat=1.0062 divergent=0/1600 near_cap=0/1600 max_rhat=1.0192; recorded: minutes=53.6 treedepth_hits=0 ess_bulk_per_1k_grad=0.181
GO v2s-m2_2021-09-25_2021-03-29_s2701 lp_rhat=1.0091 divergent=2/1600 near_cap=1/1600 max_rhat=1.0179; recorded: minutes=29.1 treedepth_hits=0 ess_bulk_per_1k_grad=0.271
```

| Fit (cell, seed) | Verdict | R-hat of `lp__` (STOP above 1.1) | Divergences (STOP above 16) | Draws within 0.5 of a cap (STOP above 16) | Max R-hat, gate scope (STOP above 1.10) | Minutes | Treedepth hits | Min bulk ESS per 1,000 gradients (gate scope) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | GO | 1.0062 | 0 of 1,600 | 0 of 1,600 | 1.0192 | 53.6 | 0 | 0.181 |
| 2021 f180, 2701 | GO | 1.0091 | 2 of 1,600 | 1 of 1,600 | 1.0179 | 29.1 | 0 | 0.271 |

**Verdict: GO on both fits.** The stop rule's signals are all well inside their limits:
- the largest R-hat of `lp__` is 1.0091;
- 2 of 1,600 transitions diverged on 2021 f180, none on 2024 eve, against the limit of 16;
- 1 of 1,600 draws on 2021 f180 came within 0.5 of a cap, none on 2024 eve, against the
  limit of 16;
- the largest gate-scope R-hat is 1.0192.

Minutes, treedepth hits and ESS per 1,000 gradients are recorded, never a stop.

- **The §5.7 gate is not the stop rule.** Each fit's DIAG line in `queue.log` reads `gate=FAIL`,
  because its gate-scope max R-hat (1.0191 and 1.0179 as DIAG prints them; the stop rule
  rounds the first up to 1.0192) is above the scoring gate's 1.01. On 2021 f180 its two
  divergences fail the gate as well. A `dev` arm is never scored.
- **The divergences and the cap.** `v2s-m1` had no divergence and no draw near a cap on
  either cell. On 2021 f180, `v2s-m2` has 2 divergent transitions of 1,600 and 1 draw of
  1,600 within 0.5 of a cap: log κ, in chain 4 (`param_draws.parquet`). The run does not keep
  the divergences per chain. `fit_cell.R` deleted the CmdStan CSVs (`HARNESS_KEEP_CMDSTAN`
  unset), `key_diagnostics.csv` holds R-hat and ESS only, `param_draws.parquet` has `.chain`
  but no `divergent__`, and the fit's log prints only the total. They were not chased further.
- **ESS per 1,000 gradients** is 0.181 on 2024 eve and 0.271 on 2021 f180, against
  `v2s-m1`'s 0.197 and 0.347: 8% and 22% lower. Recorded, not a stop.
- **The minutes read high.** They are 53.6 and 29.1, against `v2s-m1`'s 54.6 and 29.5. Other
  work shared the 10 cores while the fits ran. Beside both fits ran:
  - from about 14:35, the step's fix round: edits, two runs of the prior predictive script
    (14:39 to 14:42 and 14:42 to 14:45, one after the other), single-file fast tests and short
    scratch R scripts (14:41 to 14:46), the whole fast suite (14:46 to 14:51), then five
    single-file fast test runs (14:51 to 14:53);
  - a Monte Carlo check of M3's prior rows, one R process of about 4 seconds, from 14:36:08 to
    14:36:12;
  - a read-only re-review's fast probes, from about 14:55 to 15:02;
  - possibly, a session outside this step on the same machine: it was running pilot
    simulations when the queue started (13:52), and its load during the fits is unknown.

  2021 f180 finished at 15:02:45. No other work of this step is known to have run beside
  2024 eve after that, until it finished at 15:27:18. No slow test ran beside either fit:
  the slow suite ran in the preflight, before the fits. No fit is known to have run unloaded,
  so both fits' minutes read high, by an unknown amount.

**What the lp test covers.** `HARNESS_SLOW=1 V2_STEPS=v2s-m2 LC_ALL=en_US.UTF-8 Rscript
tests/run.R v2-stan` (at f390638, after `QUEUE FINISHED`, `[ FAIL 0 | WARN 0 | SKIP 19 | PASS 106 ]`).
On each cell, Stan's log density minus the reference's (`v2_lp_ref()`) is constant over the
cell's points within one tolerance per cell, tol = 1e-9 · max(1, |lp_ref|) with |lp_ref| the
largest over the cell's points (`lp_tol()`, `tests/harness/helper-v2.R`), and the points of
each kind (posterior, off-posterior, prior) are also checked against their own `lp_tol()`
(`expect_lp_matches()`):
- on 2024 eve and 2021 f180, over 20 posterior and 10 off-posterior points of a short fit and
  10 prior points;
- on 2017 eve, over 10 prior points only (no fit, no forecast).

Each mutant a cell exercises is rejected above 100 × tol: the core's five, the epoch bias's
four, and the newcomer pool's `newcomer_mean_over_all`. Its limits:
- `newcomer_mean_over_all` takes the entry mean over every party. μ sums to zero going into
  every entry epoch, so that mean is 0 to rounding: the mutant amounts to a base of 0, and
  does not test where the set of existing parties ends. That set, `entry_epoch` < e_p, is
  pinned for the reference by the spec-valued fast test ("a newcomer enters at the existing
  parties' mean plus the point's m_new + s_new eta", `tests/harness/test-v2-reference.R`), on
  every cell with the epoch bias on and off, and for Stan by the lp tests against the
  reference.
- `house_walk_from_first_poll` is not exercised on 2017 eve, where every house starts in
  epoch 1, as in M1.

A fourth check runs 2024 eve's prior points (10, no fit) with `newcomer_pool = 1` and
`epoch_bias = 0`, the combination Task 14's `v2-noepoch` ablation fits, where μ takes no step
and the epoch bias's parameters have size 0. Stan matches the reference there, and
`newcomer_mean_over_all` is rejected. The test's lines:

```
v2s-m2 eve2024: 40 points, lp constant range 1.28e-09 (tol 0.000667, ratio 1.92e-06; ratio by kind: post 4.88e-06, off 1.39e-06, prior 1.92e-06); weakest exercised mutant next_epoch_delta; range / (100 tol): positional_gather 1.56e+06, previous_w 4.05e+03, next_epoch_delta 3.69e+03, close_last_kjordaemi 2.26e+05, conditional_walk 7.4e+05, house_walk_from_first_poll 7.24e+04, newcomer_mean_of_mu1 7.39e+03, no_mu_restart 1.34e+05, no_house_restart 4.1e+04, newcomer_mean_over_all 3.45e+05; not exercised: none
v2s-m2 f180_2021: 40 points, lp constant range 5.09e-10 (tol 0.000265, ratio 1.92e-06; ratio by kind: post 8.1e-06, off 1.77e-06, prior 1.48e-06); weakest exercised mutant next_epoch_delta; range / (100 tol): positional_gather 2.14e+06, previous_w 1.56e+03, next_epoch_delta 999, close_last_kjordaemi 9.27e+05, conditional_walk 4.04e+06, house_walk_from_first_poll 1.87e+03, newcomer_mean_of_mu1 7.84e+03, no_mu_restart 4.54e+04, no_house_restart 8.5e+04, newcomer_mean_over_all 1.44e+06; not exercised: none
v2s-m2 eve2017: 10 points, lp constant range 5.82e-11 (tol 0.000103, ratio 5.67e-07; ratio by kind: prior 5.67e-07); weakest exercised mutant newcomer_mean_of_mu1; range / (100 tol): positional_gather 1.03e+06, close_last_kjordaemi 1.2e+06, conditional_walk 1.31e+06, newcomer_mean_of_mu1 594, no_mu_restart 4.51e+03, no_house_restart 4.66e+03, newcomer_mean_over_all 4.26e+05; not exercised: house_walk_from_first_poll
v2s-m2 eve2024 with epoch_bias = 0: 10 prior points, lp constant range 5.82e-10 (tol 0.00056); range / (100 tol): newcomer_mean_over_all 4.5e+05
```

**The prior predictive row.** `newcomer_entry`: a newcomer's entry bias against the existing
parties' mean, m_new + s_new η, read on a 10% newcomer beside P − 1 equal parties sharing 90%,
in pp, the newcomers pooled (`harness/v2/prior_predictive.md`):

| Cell | Values | q05 | q50 | q95 | Band | Verdict |
| --- | --- | --- | --- | --- | --- | --- |
| 2024 eve | 4000 | -4.77 | 0.18 | 7.65 | [5%, 95%] inside [-10, 10] | PASS |
| 2021 f180 | 4000 | -4.72 | 0.03 | 7.87 | [5%, 95%] inside [-10, 10] | PASS |

```
PRIOR PASS newcomer_entry/2024-eve/config
PRIOR PASS newcomer_entry/2021-f180/config
```

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-m2 harness/runs/v2-steps.txt harness/v2/steps/v2s-m2.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m2
```

Next: M3 (`v2s-m3`), the election-day shock (§6.6), on `v2s-m2`'s config with `shock = 1`.

## v2s-m3: the election-day shock (M-step)

`v2s-m2` with `shock = 1` (config `harness/configs/steps/v2s-m3.json`): the election-day shock
of spec §6.6. Each closed election e's kjördæmi results take one shock
ε_e = C(s̄_E d_e ⊙ z_e), shared by the election's kjördæmi, with z_e ~ N(0, I_P),
s̄_E ~ half-N(0, `prior_s_bar_E`) (0.15 in the config) and d_e = √(min(0.2 / π̄, 10)) from the
national shares π̄ of the election before e (row e of `d_shock`; a party with no previous
result reaches the builder's `shock_scaling()` as NA, is floored at π̄ = 0.02 and gets √10). The
forecast draws a fresh ε_E on the open row of `d_shock`, after the in-sample replicates and
only with `shock = 1`. It enters `votes_kd`, and through it `pi_nat_eday`, but never
`pi_latent_E`. The parameters are `z_shock` (P × n_closed) and `s_bar_E` (an array of one),
declared after `s_new`; ε is the transformed parameter `eps` [n_closed, P], all zeros with
`shock = 0`. Its lp, size, transformed-parameter and generated-quantity tests are in
`tests/harness/test-v2-stan.R`, its reference tests in `tests/harness/test-v2-reference.R`, the
floor case (2024 eve's Sósíalistaflokkurinn, present at 2021 with no 2017 result, so its 2021
row of `d_shock` is √10) in `tests/harness/test-v2-data.R`, and its prior predictive rows in
`harness/v2/prior_predictive.md`. This section is its sampler check.

**Fits.** Two fits at 400 + 400, seed 2701: 2024 eve (lane A) and 2021 f180 (lane B), the two
`v2s-m3` lines of `harness/runs/v2-steps.txt`. They ran under `R/harness/queue.sh` with the
run list `results/harness/logs/v2s-m3-runs.txt` (`harness/README.md`, Step checks). The queue
started at 2026-10-29 17:33:08, its preflight (the full slow suite) passed, and both fits
started at 18:21:20: the preflight took about 48 minutes, longer than before, with M3's new
slow tests in it and a peer's load beside it. 2021 f180 finished at 18:52:13 and 2024 eve at
19:15:58 (`queue.log`'s start lines; each `meta.json`'s modification time), both exited 0, and
`queue.log` ends in `QUEUE FINISHED`.
CmdStan 2.38.0, `max_treedepth` 12, 10 cores (`hw.ncpu`). The diagnostics table is
`harness/v2/steps/v2s-m3.md`. `queue.log` after its `QUEUE START` line (left out here: it names
the run list by its absolute path):

```
preflight PASS
start B v2s-m3_2021-09-25_2021-03-29_s2701 2026-10-29 18:21:20
start A v2s-m3_2024-11-30_2024-11-29_s2701 2026-10-29 18:21:20
end v2s-m3_2021-09-25_2021-03-29_s2701 rc=0 DIAG run=v2s-m3_2021-09-25_2021-03-29_s2701 minutes=30.7 max_rhat=1.0294 min_ess_bulk=479 min_ess_tail=651 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.636 gate=FAIL
end v2s-m3_2024-11-30_2024-11-29_s2701 rc=0 DIAG run=v2s-m3_2024-11-30_2024-11-29_s2701 minutes=54.3 max_rhat=1.0215 min_ess_bulk=289 min_ess_tail=461 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.679 gate=FAIL
QUEUE FINISHED
```

**Provenance.** Both fits ran at 68c53ac on a clean tree (`meta.json`: `git_head` 68c53ac,
`git_dirty` false), with `Stan/forecast_v2.stan` at sha256
`9149dbba616a972025d16e332adcd10585df861897e66eae6f49b796dc92c7ee`. The next two commits,
b878d39 and 1afba4d, changed no Stan line and no file the builder reads: in `R/harness` they
changed `v2_reference.R`'s roxygen, the prior predictive script (its header and `shock_sd()`'s
guard) and `queue.sh`, which the builder never reads. The Stan file's sha256 is the same there
and in this step's commit. Each run's `meta.json` `data_sha256` equals `stan_data_sha()`
(`R/harness/runs.R`) of the data that 1afba4d's builder gives for the same cell through the
arm, as `fit_cell.R` builds it, and equals the sha256 of the run's frozen
`inputs/stan_data.json`:

| Fit (cell, seed) | `git_head`, `git_dirty` | `stan_sha256` | `data_sha256` (`meta.json`) | `stan_data_sha()` at 1afba4d | sha256 of `inputs/stan_data.json` |
| --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | 68c53ac, false | `9149dbba616a972025d16e332adcd10585df861897e66eae6f49b796dc92c7ee` | `06aa215b114495ede3d8e44995eda30bccc5ff1885060b25a73451b2e7add347` | the same | the same |
| 2021 f180, 2701 | 68c53ac, false | `9149dbba616a972025d16e332adcd10585df861897e66eae6f49b796dc92c7ee` | `8b67379378fd8bec278f6976f81bfd2ec70c8561e6f187808dd4ec6098fff024` | the same | the same |

To re-measure a cell's sha, from the project root, with the cell's E and F as the two
arguments (`2024-11-30 2024-11-29` for 2024 eve and `2021-09-25 2021-03-29` for 2021 f180, the
two lines below). It reproduces the recorded sha only on a tree whose `R/harness` builder and
`harness/configs/steps/v2s-m3.json` build the data as 1afba4d's do; after a later change to
either it prints another sha, which is not a provenance failure:

```
LC_ALL=en_US.UTF-8 Rscript -e 'x <- commandArgs(TRUE); options(box.path = getwd()); box::use(R/harness/arms[arm], R/harness/info_set[info_set], R/harness/runs[stan_data_sha]); a <- arm("v2s-m3"); cat(stan_data_sha(a$build(info_set(as.Date(x[1]), as.Date(x[2])), a$read_config(a$config))$data), "\n", sep = "")' 2024-11-30 2024-11-29
LC_ALL=en_US.UTF-8 Rscript -e 'x <- commandArgs(TRUE); options(box.path = getwd()); box::use(R/harness/arms[arm], R/harness/info_set[info_set], R/harness/runs[stan_data_sha]); a <- arm("v2s-m3"); cat(stan_data_sha(a$build(info_set(as.Date(x[1]), as.Date(x[2])), a$read_config(a$config))$data), "\n", sep = "")' 2021-09-25 2021-03-29
```

**Stop rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m3` (exit 0):

```
GO v2s-m3_2024-11-30_2024-11-29_s2701 lp_rhat=1.0064 divergent=0/1600 near_cap=0/1600 max_rhat=1.0215; recorded: minutes=54.3 treedepth_hits=0 ess_bulk_per_1k_grad=0.176
GO v2s-m3_2021-09-25_2021-03-29_s2701 lp_rhat=1.0039 divergent=0/1600 near_cap=0/1600 max_rhat=1.0295; recorded: minutes=30.7 treedepth_hits=0 ess_bulk_per_1k_grad=0.292
```

| Fit (cell, seed) | Verdict | R-hat of `lp__` (STOP above 1.1) | Divergences (STOP above 16) | Draws within 0.5 of a cap (STOP above 16) | Max R-hat, gate scope (STOP above 1.10) | Minutes | Treedepth hits | Min bulk ESS per 1,000 gradients (gate scope) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | GO | 1.0064 | 0 of 1,600 | 0 of 1,600 | 1.0215 | 54.3 | 0 | 0.176 |
| 2021 f180, 2701 | GO | 1.0039 | 0 of 1,600 | 0 of 1,600 | 1.0295 | 30.7 | 0 | 0.292 |

**Verdict: GO on both fits.** The stop rule's signals are all well inside their limits:
- the largest R-hat of `lp__` is 1.0064;
- no transition diverged on either cell (0 of 1,600 each), against the limit of 16;
- no draw came within 0.5 of a cap on either cell, against the limit of 16;
- the largest gate-scope R-hat is 1.0295.

Minutes, treedepth hits and ESS per 1,000 gradients are recorded, never a stop.

- **The §5.7 gate is not the stop rule.** Each fit's DIAG line in `queue.log` reads `gate=FAIL`,
  because its gate-scope max R-hat (1.0215 and 1.0294 as DIAG prints them; the stop rule
  rounds the second up to 1.0295) is above the scoring gate's 1.01. A `dev` arm is never
  scored.
- **Divergences per chain.** None diverged on either fit (`meta.json` `divergent` 0), so none in
  any chain. The run keeps no per-chain counts: `fit_cell.R` deleted the CmdStan CSVs
  (`HARNESS_KEEP_CMDSTAN` unset), and `key_diagnostics.csv` holds R-hat and ESS only. `v2s-m2`
  had 2 divergent transitions and 1 draw near a cap on 2021 f180; `v2s-m3` has neither.
- **ESS per 1,000 gradients** is 0.176 on 2024 eve and 0.292 on 2021 f180, against `v2s-m2`'s
  0.181 and 0.271: 3% lower and 8% higher. Recorded, not a stop.
- **What sets the extremes.** M3 adds s̄_E, a scale read off only 3 closed elections on 2024 eve
  and 2 on 2021 f180 (§6.6). It sets neither extreme: its R-hat is 1.0066 and 1.0011 and its
  bulk ESS 447 and 808 (`key_diagnostics.csv`). The gate's min bulk ESS is `tau_sd`'s on 2024
  eve (288.8) and house 2's (Gallup) log φ on 2021 f180 (479.0; `key_diagnostics.csv` carries
  `log_phi`, a monotone map of the parameter `log_phi_u`, whose bulk ESS is the same). The
  gate's max R-hat, 1.0215 and 1.0295 (the stop table's), is above every variable
  `key_diagnostics.csv` holds (at most 1.0150, `tau_sd` on 2024 eve, and 1.0096, `s_delta0[7]`
  on 2021 f180; the scored quantities at most 1.0046 and 1.0077). So a parameters-block variable
  outside `param_draws.parquet` sets it, and the kept files cannot say which: the runs keep no
  per-variable diagnostics for those variables, and their CmdStan CSVs are deleted. `z_shock` is
  not ruled out. `v2s-m2`'s were 1.0192 and 1.0179.
- **The minutes read high.** They are 54.3 and 30.7, against `v2s-m2`'s 53.6 and 29.1. Other
  work shared the 10 cores while the fits ran. Beside both fits ran:
  - throughout, a peer session's untimed profiling run (`sc_profile.R`): at most 3
    single-threaded R processes, BLAS on 1 thread;
  - about 18:22 to 18:31, the step's fix round: edits and fast tests (escapes, v2-reference and
    v2-prior-predictive, the last of which runs the prior predictive script at 20 draws in a
    subprocess), then the whole fast suite (18:25:55 to 18:29:16), in which
    `tests/harness/test-fit-cell.R`'s stubbed queue was refused because these fits were running;
  - about 18:31 to 18:40, a read-only re-review's reads and fast probes.

  No other work of this step is known to have run beside either fit after 18:40: 2021 f180
  finished at 18:52:13 and 2024 eve at 19:15:58. No slow test ran beside either fit: the slow
  suite ran in the preflight, before the fits. No fit is known to have run unloaded, so both
  fits' minutes read high, by an unknown amount.

**What the lp tests cover.** `HARNESS_SLOW=1 V2_STEPS=v2s-m3 LC_ALL=en_US.UTF-8 Rscript
tests/run.R v2-stan` (on the tree committed as 1afba4d, after `QUEUE FINISHED`,
`[ FAIL 0 | WARN 0 | SKIP 24 | PASS 133 ]`). On each cell, Stan's log density minus the
reference's (`v2_lp_ref()`) is constant over the cell's points within one tolerance per cell,
tol = 1e-9 · max(1, |lp_ref|) with |lp_ref| the largest over the cell's points (`lp_tol()`,
`tests/harness/helper-v2.R`), and the points of each kind (posterior, off-posterior, prior)
are also checked against their own `lp_tol()` (`expect_lp_matches()`):
- on 2024 eve and 2021 f180, over 20 posterior and 10 off-posterior points of a short fit and
  10 prior points;
- on 2017 eve, over 10 prior points only (no fit, no forecast).

Each mutant a cell exercises is rejected above 100 × tol: the core's five, the epoch bias's
four, the newcomer pool's `newcomer_mean_over_all`, and the shock's `shock_unscaled` (d ≡ 1),
which every cell exercises, 2017 eve's one closed election included: its range is 6.94e+04,
6.97e+04 and 3.81e+04 times 100 × tol on 2024 eve, 2021 f180 and 2017 eve. A fourth check runs
2024 eve's prior points (10, no fit) at `prior_s_bar_E` = 0.37: Stan matches the reference
there, and three slips are rejected: the prior's scale written as the literal 0.15 (127 times
100 × tol), each closed election's shock read off the next row of `d_shock` (9.57e+04) and off
the open row (1.28e+05). Its limits:
- `house_walk_from_first_poll` is not exercised on 2017 eve, where every house starts in
  epoch 1, as in M1 and M2.
- The centring of ε is invisible to the likelihood and to the forecast: a shift common to the
  parties cancels in every softmax. Only the `eps` transformed-parameter test, eps equal to the
  reference's C(s̄_E d_e ⊙ z_e) to 1e-10, pins it.
- The forecast side is tested at fixed points: the results' rows carry their shocks,
  `pi_latent_E` is the shock-free latent share and `pi_nat_eday` aggregates `votes_kd`; and at a
  point whose walk and δ are 0 and whose closed shocks are far from 0, `votes_kd`'s log-ratio
  contrasts have the open row's variance s̄_E² (d_p² + d_q²), move together across kjördæmi,
  and stay centred at the shock-free value. A copy of the Stan file with the last closed
  election's ε planted in `votes_kd`'s softmax fails that centre check alone, its largest |z|
  200.3 against the threshold of 4.

The test's lines:

```
v2s-m3 eve2024: 40 points, lp constant range 1.16e-09 (tol 0.000667, ratio 1.75e-06; ratio by kind: post 4.68e-06, off 7.63e-07, prior 1.75e-06); weakest exercised mutant previous_w; range / (100 tol): positional_gather 1.46e+06, previous_w 3.16e+03, next_epoch_delta 3.5e+03, close_last_kjordaemi 2.3e+05, conditional_walk 7.39e+05, house_walk_from_first_poll 7.66e+04, newcomer_mean_of_mu1 1.47e+04, no_mu_restart 1.63e+05, no_house_restart 5.46e+04, newcomer_mean_over_all 3.45e+05, shock_unscaled 6.94e+04; not exercised: none
v2s-m3 f180_2021: 40 points, lp constant range 4.07e-10 (tol 0.000265, ratio 1.54e-06; ratio by kind: post 5.32e-06, off 1.25e-06, prior 1.54e-06); weakest exercised mutant next_epoch_delta; range / (100 tol): positional_gather 2.27e+06, previous_w 1.56e+03, next_epoch_delta 1.05e+03, close_last_kjordaemi 1.4e+06, conditional_walk 1.53e+06, house_walk_from_first_poll 2.07e+03, newcomer_mean_of_mu1 1.18e+04, no_mu_restart 1.18e+05, no_house_restart 1.34e+05, newcomer_mean_over_all 1.52e+06, shock_unscaled 6.97e+04; not exercised: none
v2s-m3 eve2017: 10 points, lp constant range 5.82e-11 (tol 0.000103, ratio 5.67e-07; ratio by kind: prior 5.67e-07); weakest exercised mutant newcomer_mean_of_mu1; range / (100 tol): positional_gather 1.04e+06, close_last_kjordaemi 1.2e+06, conditional_walk 1.3e+06, newcomer_mean_of_mu1 593, no_mu_restart 4.5e+03, no_house_restart 4.66e+03, newcomer_mean_over_all 4.26e+05, shock_unscaled 3.81e+04; not exercised: house_walk_from_first_poll
v2s-m3 eve2024 at prior_s_bar_E 0.37: 10 prior points, lp constant range 8.73e-10 (tol 0.000667); range / (100 tol): literal_0.15 127, next_row 9.57e+04, open_row 1.28e+05
```

**Shock off means off.** With `shock = 0`, ε ≡ 0 and the generated quantities make no extra
random draw. Checked once, after `QUEUE FINISHED`: plain executables of the committed
`Stan/forecast_v2.stan` (68c53ac's, unchanged since) and of 69b8978's (the step before), each
compiled from a copy in a temporary directory, ran as `fixed_param_run()` does (one chain, one
iteration, CmdStan seed 1, the point as its init) at one prior point,
`v2_simulate(seed = 5, observe = FALSE)`, of the `v2s-m2` config's data (`shock = 0`,
`draw_forecast = 1`) on 2024 eve and 2021 f180, built as the Stan tests' cells are
(`v2_test_cells()`, `tests/harness/helper-v2.R`). Every quantity is identical between the two
files; only TRUE or FALSE was printed. In the script's output below, the "new file" is the
committed file (68c53ac's) and the "old file" 69b8978's:

```
new file sha256 9149dbba616a972025d16e332adcd10585df861897e66eae6f49b796dc92c7ee
old file sha256 934281745c266c4df9a20ceb9ac86e14d53c9f290a5236401e4ca1ff7415b3fa
point: v2_simulate(seed = 5, observe = FALSE) on v2_test_cells(cfg_m2()) data; CmdStan seed 1
eve2024: all five present in both: TRUE
eve2024 log_lik identical: TRUE
eve2024 y_rep_res identical: TRUE
eve2024 votes_kd identical: TRUE
eve2024 pi_nat_eday identical: TRUE
eve2024 pi_latent_E identical: TRUE
f180_2021: all five present in both: TRUE
f180_2021 log_lik identical: TRUE
f180_2021 y_rep_res identical: TRUE
f180_2021 votes_kd identical: TRUE
f180_2021 pi_nat_eday identical: TRUE
f180_2021 pi_latent_E identical: TRUE
```

The standing test can see less. Its draw-order check, `y_rep_res` identical with
`draw_forecast` 1 and 0 at `shock = 1`, detects a forecast draw gated on `draw_forecast` and
placed before the replicates (a planted one fails it). It cannot see a draw guarded only by
`if (shock)` before the results loop, nor one outside `if (draw_forecast)`, since both of its
runs make such a draw. Runs at `shock = 1` and 0 cannot be compared draw for draw: a
fixed_param run's initialisation draws one uniform per unconstrained parameter even when every
init is given, and the two are P n_closed + 1 parameters apart (the newcomer pool's two
parameters show the same offset at an equal likelihood).

**The prior predictive rows.** `shock_sd_20` and `shock_sd_05`: the sd of a 20% and of a 5%
party's change under the shock, given the draw's s̄_E, over 200 draws of z on the composition
(share, (1 − share) / (P − 1), ...), in pp (`harness/v2/prior_predictive.md`):

| Row | Cell | Values | q05 | q50 | q95 | Band | Median ÷ target | Verdict |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `shock_sd_20` | 2024 eve | 1000 | 0.16 | 1.73 | 5.12 | median in [0.5, 2] × 1.6 | 1.08 | PASS |
| `shock_sd_20` | 2021 f180 | 1000 | 0.18 | 1.70 | 5.13 | median in [0.5, 2] × 1.6 | 1.06 | PASS |
| `shock_sd_05` | 2024 eve | 1000 | 0.09 | 0.96 | 3.34 | median in [0.5, 2] × 0.95 | 1.01 | PASS |
| `shock_sd_05` | 2021 f180 | 1000 | 0.10 | 0.94 | 3.27 | median in [0.5, 2] × 0.95 | 0.99 | PASS |

```
PRIOR PASS shock_sd_20/2024-eve/config
PRIOR PASS shock_sd_20/2021-f180/config
PRIOR PASS shock_sd_05/2024-eve/config
PRIOR PASS shock_sd_05/2021-f180/config
```

§6.10's 1.6 and 0.95 pp are the party-alone linearisation π(1 − π) s̄ d at s̄_E's median,
0.15 · qnorm(0.75) = 0.1012. The rows' construction also shocks the other P − 1 parties, so its
population medians are about 1.80 and 1.00 pp, and their ratios to §6.10's targets about 1.13
and 1.05. The rows sit lower because their 1,000-draw sample of s̄_E has a median below its
population median: 0.0982 on 2024 eve and 0.0936 on 2021 f180 (ratios 0.971 and 0.926; measured
with `v2_simulate()` at the report's seeds 6001 to 7000 on the v2s-m3 config, only s̄_E's median
printed). Every earlier row of the report is byte-identical to the one `v2s-m2`'s config wrote.

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-m3 harness/runs/v2-steps.txt harness/v2/steps/v2s-m3.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m3
```

Next: M4 (`v2s-m4`), the kjördæmi deviations' walk, on `v2s-m3`'s config with `delta_walk = 1`,
as long as M3 stands. M4 precedes P2, so its queue lines need seed 2702 as well as 2701.

## v2s-m4: the δ walk (M-step)

`v2s-m3` with `delta_walk = 1` (config `harness/configs/steps/v2s-m4.json`): the kjördæmi
deviations walk across elections (spec §6.7). δ_1 = M_1(s_δ0 ⊙ ζ_1), and each later epoch's
δ_e = M_e(δ_{e−1} − c_{e−1} 1ᵀ + √(Δyears_e / Δ̄) s_ζ ⊙ ζ_e), where c_{e−1} is, in each
kjördæmi, the unweighted mean of δ_{e−1} over S_{e−1}, the parties with results at election
e − 1. Here ζ_e ~ N(0, 1) on every kjördæmi and party, s_ζ = s̄_ζ s_zeta_raw with
s̄_ζ ~ half-N(0, `prior_s_bar_zeta`) (0.2 in the config) and s_zeta_raw ~ half-N(0, 1),
Δyears_e is the years since the previous election (`delta_years`, the open epoch's running to
E) and Δ̄ = `delta_years_bar` (4). A newcomer's column in its entry epoch e_p > 1 is
s_δ0,p ζ_{e_p,·,p} before M_e. The parameters are `delta_raw`, now 1 + (n_epoch − 1) matrices
[K, P] (ζ_1 and the steps' ζ), `s_zeta_raw` [P] and `s_bar_zeta` (an array of one), the last
two declared after `s_bar_E`; s_ζ is the transformed parameter `s_zeta` [P]. With
`delta_walk = 0`, δ stays the static M_e(s_δ0 ⊙ ζ) on one ζ. Its lp, size,
transformed-parameter and generated-quantity tests are in `tests/harness/test-v2-stan.R`, its
reference tests in `tests/harness/test-v2-reference.R`, and its prior predictive row in
`harness/v2/prior_predictive.md`. This section is its sampler check.

**The form, and how it departs from §6.7's literal text.** §6.7 writes each step from δ_{e−1}
itself; the model steps from δ_{e−1} re-centred within each kjördæmi over S_{e−1}. Two
directions are unseen by every likelihood term before a newcomer enters: the newcomer's own
earlier column (it is in no softmax before its entry epoch) and a per-kjördæmi offset shared
by the parties of S_{e−1} (it cancels in every softmax over them). Without the re-centring the
entering column restarts while the others keep δ_{e−1}, so both directions reach the entry
epoch's contrasts, and the newcomer enters against a level that nothing observed has set. In
the R reference with the literal step, moving one of a newcomer's pre-entry ζ by 1 on 2024 eve
changed the log density by 0.50 to 20.95 beyond that ζ's own N(0, 1) prior, against a
tolerance of 2.6e-4; on a toy input (4 kjördæmi, 5 parties, one entering at epoch 2) the entry
epoch's contrasts moved by 0.042 under the newcomer's earlier ζ and by 0.54 under an offset
shared by S_1. With the re-centring both directions are flat (below, What the lp tests cover).
The re-centred step equals the literal text wherever no party enters, because M_e removes any
constant within a kjördæmi and c_{e−1} 1ᵀ is one. The newcomer's column before M_e is still a
fresh draw s_δ0,p ζ_{e_p,·,p}, as in §6.7: the re-centring changes only what the existing
parties carry into the entry epoch.

**Fits.** Four fits at 400 + 400: 2024 eve (lane A) and 2021 f180 (lane B), each at seeds 2701
and 2702, because a P-step follows; they are the four `v2s-m4` lines of
`harness/runs/v2-steps.txt`. They ran under `R/harness/queue.sh` with the run list
`results/harness/logs/v2s-m4-runs.txt` (`harness/README.md`, Step checks):
- **The overnight stall.** The queue started at 2026-10-29 21:54:28. Its preflight (the full
  slow suite) was still running when the Mac, on battery at 1%, went into "Low Power Sleep" at
  22:38:27 and hibernated until 07:08:26 on 2026-10-30 (`pmset -g log`). The preflight was
  inside an R CMD SHLIB compile when it slept; it resumed on wake and passed at 07:25:13. From
  07:12, `caffeinate -i -w <queue pid>` held an idle-sleep assertion for the queue's lifetime,
  since the Mac's AC `sleep` setting is 1 minute.
- **The fits.** Both lanes started their seed-2701 fits at 07:25:13. 2021 f180 seed 2701
  finished at 07:50:10, and lane B ran seed 2702 from then until 08:40:37; 2024 eve seed 2701
  finished at 08:43:22 (`queue.log`'s start lines; each `meta.json`'s modification time), and
  lane A started seed 2702.
- **A second sleep.** At 08:03:06 the Mac went on battery, and at 08:03:14 into "Clamshell
  Sleep", which an idle-sleep assertion does not prevent. `pmset -g log` then shows seven sleeps
  (2 min 19 s, 5 min 14 s, 6 min 53 s, 8 min 30 s, 25 s, 3 s and 14 s: 23.6 minutes in all)
  between dark wakes (about 4.5 minutes in all, the longest 3 minutes, 08:26:35 to 08:29:35),
  and a full wake at 08:31:19, back on AC. The two fits running then, 2024 eve seed 2701 and
  2021 f180 seed 2702, count those 28 minutes, 23.6 of them asleep, in their minutes.
- **The pause and the refit.** At 09:04 the work was paused and the queue killed by TERM
  (`QUEUE ABORTED rc=143`), about 21 minutes into 2024 eve seed 2702, which left no `meta.json`.
  On 2026-11-04 that fit was refitted alone, through the one-line run list
  `results/harness/logs/v2s-m4-refit-runs.txt`. The first attempt (`QUEUE START` 11:14:01)
  failed its preflight: every failure was CmdStan's stale precompiled header after a clang
  update, so CmdStan was rebuilt (`rebuild_cmdstan()`), with Brynjólfur's agreement, twice:
  once for the clang change and once after a Command Line Tools update. The second attempt
  (`QUEUE START` 11:33:41) passed its preflight at 12:20:24
  (`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 6661 ]`). Its fit started at 12:20:24, retired the killed
  run's partial folder (no `meta.json`) to `_stale/`, finished at 13:05:44 and exited 0, and
  `queue.log` ends in `QUEUE FINISHED`. All four fits ran the same executable,
  `Stan/forecast_v2` (git-ignored), compiled at 2026-10-29 21:57 (its modification time), sha256
  `9a532c8cdb1f5777ab5f30a8cc8c41190c091cd90af5b1fee33e59315487224c`. cmdstanr reuses an
  executable newer than its .stan file (`Stan/forecast_v2.stan` last changed at 21:37 that
  day), so the refit did not recompile it, and the two rebuilds served only the preflight's
  compiles of copies in temporary directories.

CmdStan 2.38.0, `max_treedepth` 12, 10 cores (`hw.ncpu`). The diagnostics table is
`harness/v2/steps/v2s-m4.md`. `queue.log` after the first queue's `QUEUE START` line, and after
the refit's second `QUEUE START` line (the `QUEUE START` lines and the failed attempt's two
lines are left out here: they name the run list or the preflight log by its absolute path):

```
preflight PASS
start B v2s-m4_2021-09-25_2021-03-29_s2701 2026-10-30 07:25:13
start A v2s-m4_2024-11-30_2024-11-29_s2701 2026-10-30 07:25:13
end v2s-m4_2021-09-25_2021-03-29_s2701 rc=0 DIAG run=v2s-m4_2021-09-25_2021-03-29_s2701 minutes=24.7 max_rhat=1.0371 min_ess_bulk=106 min_ess_tail=149 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.541 gate=FAIL
start B v2s-m4_2021-09-25_2021-03-29_s2702 2026-10-30 07:50:10
end v2s-m4_2021-09-25_2021-03-29_s2702 rc=0 DIAG run=v2s-m4_2021-09-25_2021-03-29_s2702 minutes=50.2 max_rhat=1.0256 min_ess_bulk=127 min_ess_tail=167 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.509 gate=FAIL
end v2s-m4_2024-11-30_2024-11-29_s2701 rc=0 DIAG run=v2s-m4_2024-11-30_2024-11-29_s2701 minutes=77.8 max_rhat=1.0624 min_ess_bulk=50 min_ess_tail=225 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.698 gate=FAIL
start A v2s-m4_2024-11-30_2024-11-29_s2702 2026-10-30 08:43:22
QUEUE ABORTED rc=143 killed (TERM) (pid 44643)
```

```
preflight PASS
start A v2s-m4_2024-11-30_2024-11-29_s2702 2026-11-04 12:20:24
end v2s-m4_2024-11-30_2024-11-29_s2702 rc=0 STALE v2s-m4_2024-11-30_2024-11-29_s2702 (no meta.json)
QUEUE FINISHED
```

`queue.log`'s end line for the refit shows the run's `STALE` line, not its `DIAG` line, because
the queue logs the first `DIAG`, `SKIP` or `STALE` line of the run log and the retire came
first. The refit's run log, `results/harness/logs/v2s-m4_2024-11-30_2024-11-29_s2702.log`, its
first line and its `DIAG` line (the retire's second line, which names both folders by their
absolute paths, is left out):

```
STALE v2s-m4_2024-11-30_2024-11-29_s2702 (no meta.json)
DIAG run=v2s-m4_2024-11-30_2024-11-29_s2702 minutes=45.1 max_rhat=1.0352 min_ess_bulk=117 min_ess_tail=264 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.608 gate=FAIL
```

**Provenance.** All four fits ran at f7468df on a clean tree (`meta.json`: `git_head` f7468df,
`git_dirty` false; `fit_cell.R` records both before it fits), with `Stan/forecast_v2.stan` at
sha256 `8d34b6fe43778b5d073b80e7e41996ee8a66be5c907cda94ffa51880de2be05b` and the config at
`2a950600e6274b0201369e41aa9b7353d6ceca06c2c01f1f2f28a625b243b3e6`. The next commit, cac85df,
changed no Stan line and no file the builder reads: in `R/harness` it changed only the prior
predictive script's header and one row's label, and otherwise tests and the prior predictive
report. The Stan file's sha256 is the same there and in this step's commit. Each run's
`meta.json` `data_sha256` equals `stan_data_sha()` (`R/harness/runs.R`) of the data that
cac85df's builder gives for the same cell through the arm, as `fit_cell.R` builds it, and
equals the sha256 of the run's frozen `inputs/stan_data.json`:

| Fit (cell, seed) | `git_head`, `git_dirty` | `stan_sha256` | `data_sha256` (`meta.json`) | `stan_data_sha()` at cac85df | sha256 of `inputs/stan_data.json` |
| --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | f7468df, false | `8d34b6fe43778b5d073b80e7e41996ee8a66be5c907cda94ffa51880de2be05b` | `44395c14fb5a85e5a56908900754065c05c7932d336678bfa55a2db040155e89` | the same | the same |
| 2024 eve, 2702 | f7468df, false | `8d34b6fe43778b5d073b80e7e41996ee8a66be5c907cda94ffa51880de2be05b` | `44395c14fb5a85e5a56908900754065c05c7932d336678bfa55a2db040155e89` | the same | the same |
| 2021 f180, 2701 | f7468df, false | `8d34b6fe43778b5d073b80e7e41996ee8a66be5c907cda94ffa51880de2be05b` | `51e36d6557550f65f558fbc9e0aef7fc26c0ce8e598c2c8504fb314fc910b453` | the same | the same |
| 2021 f180, 2702 | f7468df, false | `8d34b6fe43778b5d073b80e7e41996ee8a66be5c907cda94ffa51880de2be05b` | `51e36d6557550f65f558fbc9e0aef7fc26c0ce8e598c2c8504fb314fc910b453` | the same | the same |

To re-measure a cell's sha, from the project root, with the cell's E and F as the two
arguments (`2024-11-30 2024-11-29` for 2024 eve and `2021-09-25 2021-03-29` for 2021 f180, the
two lines below). It reproduces the recorded sha only on a tree whose `R/harness` builder and
`harness/configs/steps/v2s-m4.json` build the data as cac85df's do; after a later change to
either it prints another sha, which is not a provenance failure:

```
LC_ALL=en_US.UTF-8 Rscript -e 'x <- commandArgs(TRUE); options(box.path = getwd()); box::use(R/harness/arms[arm], R/harness/info_set[info_set], R/harness/runs[stan_data_sha]); a <- arm("v2s-m4"); cat(stan_data_sha(a$build(info_set(as.Date(x[1]), as.Date(x[2])), a$read_config(a$config))$data), "\n", sep = "")' 2024-11-30 2024-11-29
LC_ALL=en_US.UTF-8 Rscript -e 'x <- commandArgs(TRUE); options(box.path = getwd()); box::use(R/harness/arms[arm], R/harness/info_set[info_set], R/harness/runs[stan_data_sha]); a <- arm("v2s-m4"); cat(stan_data_sha(a$build(info_set(as.Date(x[1]), as.Date(x[2])), a$read_config(a$config))$data), "\n", sep = "")' 2021-09-25 2021-03-29
```

**Stop rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m4` (exit 0):

```
GO v2s-m4_2024-11-30_2024-11-29_s2701 lp_rhat=1.0237 divergent=0/1600 near_cap=0/1600 max_rhat=1.0624; recorded: minutes=77.8 treedepth_hits=0 ess_bulk_per_1k_grad=0.030
GO v2s-m4_2024-11-30_2024-11-29_s2702 lp_rhat=1.0144 divergent=0/1600 near_cap=0/1600 max_rhat=1.0352; recorded: minutes=45.1 treedepth_hits=0 ess_bulk_per_1k_grad=0.071
GO v2s-m4_2021-09-25_2021-03-29_s2701 lp_rhat=1.0160 divergent=0/1600 near_cap=1/1600 max_rhat=1.0372; recorded: minutes=24.7 treedepth_hits=0 ess_bulk_per_1k_grad=0.064
GO v2s-m4_2021-09-25_2021-03-29_s2702 lp_rhat=1.0211 divergent=0/1600 near_cap=0/1600 max_rhat=1.0257; recorded: minutes=50.2 treedepth_hits=0 ess_bulk_per_1k_grad=0.079
```

| Fit (cell, seed) | Verdict | R-hat of `lp__` (STOP above 1.1) | Divergences (STOP above 16) | Draws within 0.5 of a cap (STOP above 16) | Max R-hat, gate scope (STOP above 1.10) | Minutes | Treedepth hits | Min bulk ESS per 1,000 gradients (gate scope) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | GO | 1.0237 | 0 of 1,600 | 0 of 1,600 | 1.0624 | 77.8 | 0 | 0.030 |
| 2024 eve, 2702 | GO | 1.0144 | 0 of 1,600 | 0 of 1,600 | 1.0352 | 45.1 | 0 | 0.071 |
| 2021 f180, 2701 | GO | 1.0160 | 0 of 1,600 | 1 of 1,600 | 1.0372 | 24.7 | 0 | 0.064 |
| 2021 f180, 2702 | GO | 1.0211 | 0 of 1,600 | 0 of 1,600 | 1.0257 | 50.2 | 0 | 0.079 |

**Verdict: GO on all four fits.** The stop rule's signals are all inside their limits:
- the largest R-hat of `lp__` is 1.0237;
- no transition diverged on any fit (0 of 1,600 each), against the limit of 16;
- one draw came within 0.5 of a cap, on 2021 f180 seed 2701, against the limit of 16;
- the largest gate-scope R-hat is 1.0624.

Minutes, treedepth hits and ESS per 1,000 gradients are recorded, never a stop.

- **The §5.7 gate is not the stop rule.** Each fit's `DIAG` line reads `gate=FAIL`, because its
  gate-scope max R-hat (1.0624, 1.0352, 1.0371 and 1.0256 as `DIAG` prints them; the stop rule
  rounds the last two up to 1.0372 and 1.0257) is above the scoring gate's 1.01. A `dev` arm is
  never scored.
- **Divergences per chain.** None diverged on any fit (`meta.json` `divergent` 0), so none in
  any chain. The runs keep no per-chain counts: `fit_cell.R` deleted the CmdStan CSVs
  (`HARNESS_KEEP_CMDSTAN` unset), and `key_diagnostics.csv` holds R-hat and ESS only. The one
  draw near a cap is a draw of log κ (cap 2) in `param_draws.parquet`; `v2s-m3` had none.
- **ESS per 1,000 gradients** is 0.030 and 0.071 on 2024 eve (seeds 2701 and 2702) and 0.064
  and 0.079 on 2021 f180, against `v2s-m3`'s 0.176 and 0.292: 2.5 to 5.8 times lower (from the
  unrounded `min_ess_bulk` ÷ `n_leapfrog` of each `meta.json`). The
  gradients barely differ: a post-warmup transition took 1,001 to 1,023 leapfrog steps on
  average (`n_leapfrog` ÷ 1,600; tree depth 10 is 1,023 steps), here as in `v2s-m3`, so the drop
  is the min bulk ESS's own. Recorded, not a stop.
- **What sets the extremes.** log φ_K, the concentration of the closed elections' kjördæmi
  results, sets the gate-scope min bulk ESS and max R-hat on all four fits: `meta.json`'s
  `min_ess_bulk` and `max_rhat`, the "gate scope: parameters block + scored" values of the
  diagnostics table (the §5.7 gate itself checks the gate-scope max R-hat and the scored min
  bulk ESS). The gate scope reads the parameter `log_phi_K_u`, which `key_diagnostics.csv` does
  not hold. It holds `log_phi_K`, a monotone map of it, whose bulk ESS (50.2, 117.2, 105.6 and
  126.8, in the stop table's order) and R-hat (1.0624, 1.0352, 1.0371 and 1.0256) equal
  `meta.json`'s `min_ess_bulk` and `max_rhat` to every stored digit. The gate-scope min tail
  ESS is log φ_K's on 2021 f180 seed 2701, τ_μ's on both 2024 eve fits and s̄_ζ's on 2021 f180
  seed 2702. Under `v2s-m3` log φ_K mixed well: bulk ESS 2310 and 665, R-hat 1.0001 and 1.0028.
  Its posterior median rises, from 6.31 to 7.15 on 2024 eve (both seeds) and from 7.02 to 7.89
  and 7.78 on 2021 f180 (prior median log 1000 = 6.91). §6.9 names the check: the posterior
  correlation of (log φ_K, s̄_ζ). Over each fit's 1,600 draws pooled across chains
  (`param_draws.parquet`, columns `log_phi_K` and `s_bar_zeta[1]`), it is 0.31 and 0.30 on 2024
  eve (seeds 2701 and 2702) and 0.44 and 0.45 on 2021 f180. Draws with larger walk steps have
  more concentrated results, as a trade-off between the walk's steps and the results'
  overdispersion around δ would give; the correlation is moderate, and it does not show that
  the trade-off is what slows log φ_K's mixing. In `key_diagnostics.csv` the next lowest bulk
  ESS is s_ζ's on Viðreisn on 2024 eve seed 2701 (67; R-hat 1.0478).
- **Where s̄_ζ sits.** It sets one extreme, the gate-scope min tail ESS on 2021 f180 seed 2702
  (166.7, `meta.json`'s `min_ess_tail`; s_ζ on Viðreisn has a lower tail ESS there, 162.6, but
  s_ζ is a transformed parameter, outside the gate scope), and no R-hat or bulk-ESS extreme.
  Its posterior medians sit above its prior median, 0.135 = 0.2 · qnorm(0.75): a little on
  2024 eve (0.16) and more on 2021 f180 (0.22 and 0.24), all below the prior's 95% point 0.39.
  The four chains agree on every fit (`key_diagnostics.csv` and `param_draws.parquet`, s̄_ζ
  only):

  | Fit (cell, seed) | R-hat | Bulk ESS | Posterior median | 5% and 95% | Chain medians |
  | --- | --- | --- | --- | --- | --- |
  | 2024 eve, 2701 | 1.0148 | 496 | 0.160 | 0.101, 0.252 | 0.161, 0.162, 0.156, 0.163 |
  | 2024 eve, 2702 | 1.0177 | 301 | 0.161 | 0.101, 0.266 | 0.156, 0.161, 0.162, 0.165 |
  | 2021 f180, 2701 | 1.0119 | 288 | 0.238 | 0.085, 0.400 | 0.237, 0.237, 0.248, 0.227 |
  | 2021 f180, 2702 | 1.0117 | 249 | 0.224 | 0.087, 0.397 | 0.222, 0.219, 0.235, 0.221 |

  Short fits made before the re-centring (4 chains of 50 + 20 at treedepth 8) had 2024 eve's
  s̄_ζ at 0.28 to 1.04, with chain medians from 0.31 to 0.82. The full fits do not reproduce
  that: their chains agree, near 0.16. The short fits differ from these in both the form and
  the warmup, so they cannot say which of the two moved s̄_ζ.
- **The minutes read high, and two include a sleep.** They are 77.8 and 45.1 on 2024 eve and
  24.7 and 50.2 on 2021 f180, against `v2s-m3`'s 54.3 and 30.7. What ran beside each fit:
  - 2021 f180, seed 2701 (07:25:13 to 07:50:10, awake, on AC): 2024 eve seed 2701 throughout;
    at 07:26 the post-wake system work (XProtect, installd, an updater), with a 1-minute load
    average of 69 and each of the 8 chains at about 95% CPU; and a peer session's simulation
    study, which at 21:56 ran 4 cores and about 10 GB and was due to end at 22:20 to 22:40, but
    the sleep paused it, so it ran on after the wake: one R process at about 72% CPU at 07:26.
  - 2021 f180, seed 2702 (07:50:10 to 08:40:37): 2024 eve seed 2701 throughout, and the peer's
    simulation for as long as it ran. About 23.6 of its 50.2 minutes were asleep, so it ran
    about 27 minutes not asleep, about 4.5 of them in dark wakes.
  - 2024 eve, seed 2701 (07:25:13 to 08:43:22): both 2021 f180 fits, the post-wake work and the
    peer's simulation, and the same 23.6 minutes asleep, so about 54 minutes not asleep.
  - 2024 eve, seed 2702 (2026-11-04 12:20:24 to 13:05:44, the only fit of its queue, awake
    throughout: `pmset -g log` records no sleep from 11:00:10 until after it finished): from
    about 12:21 to 12:33 this step's fix round, as single R processes: fast test files
    (escapes, v2-reference, v2-prior-predictive), short reference scripts, the prior predictive
    report's regeneration (1,000 draws, 1:45) and then the whole fast suite (about 12:29 to
    12:32), at a 1-minute load average of 6.1 to 8.2; and beside all of it a read-only audit
    job (a `claude -p` process, no R) and the controller's checks.

  A per-chain time ratio, wall minutes (warmup included) ÷ (post-warmup gradients ÷ 4), is
  6.6 ms for the refit and 3.6 ms for 2021 f180 seed 2701, against `v2s-m3`'s 8.0 and 4.5. It
  counts warmup time against post-warmup gradients only, so it is not a cost per gradient; it
  compares fits of the same iterations, as these are. No slow test ran beside any fit: the
  slow suite ran in each preflight, before the fits. No fit is known to have run unloaded, so
  every fit's minutes read high, by an unknown amount.

**What the lp tests cover.** The refit's preflight (the full slow suite on f7468df, 2026-11-04,
`[ FAIL 0 | WARN 0 | SKIP 0 | PASS 6661 ]`) ran the step's Stan tests. On each cell, Stan's log
density minus the reference's (`v2_lp_ref()`) is constant over the cell's points within one
tolerance per cell, tol = 1e-9 · max(1, |lp_ref|) with |lp_ref| the largest over the cell's
points (`lp_tol()`, `tests/harness/helper-v2.R`), and the points of each kind (posterior,
off-posterior, prior) are also checked against their own `lp_tol()` (`expect_lp_matches()`):
- on 2024 eve and 2021 f180, over 20 posterior and 10 off-posterior points of a short fit and
  10 prior points;
- on 2017 eve, over 10 prior points only (no fit, no forecast).

Each mutant a cell exercises is rejected above 100 × tol, the walk's three included:
`delta_years_unscaled` (every step on √1, not √(Δyears_e / Δ̄)) at 6.68e+04, 1.27e+05 and
2.14e+04 times 100 × tol on 2024 eve, 2021 f180 and 2017 eve; `no_delta_restart` (a
newcomer's column keeps its pre-entry walk) at 5.08e+04, 4.28e+04 and 1.31e+04; and
`delta_entry_leak` (the literal step, from δ_{e−1} itself) at 3.19e+03, 3.76e+03 and 370.
370 on 2017 eve is the narrowest margin of any mutant on any cell; only the slip test below
has a narrower margin, its planted slip `literal_0.2` at 43.3. 2017 eve has only prior
points, one closed election and no kjördæmi poll, and its newcomer (Miðflokkurinn) enters in
the open epoch. So the leak reaches the density only through the open epoch's national polls,
whose share Σ_k w_k softmax(θ + δ_k) sees δ only through the sum's curvature: M_e makes δ's
w-weighted kjördæmi mean zero for every party, so the sum's first-order term in δ vanishes.

A fourth check runs 2024 eve's prior points (10, no fit) at `delta_years_bar` = 2.5 and
`prior_s_bar_zeta` = 0.37: Stan matches the reference there, and three planted slips are
rejected: Δ̄ written as the literal 4 (9.57e+04 times 100 × tol), the prior's scale as the
literal 0.2 (43.3), and each step on the previous epoch's gap (2.03e+05). The two flatness
tests, in
`tests/harness/test-v2-reference.R`, pin the re-centring in the reference, which the lp test
ties Stan to:
- on real data, at seed 9 with s̄_ζ = 0.3, a newcomer's pre-entry ζ (2024 eve, 6 moves) and an
  offset a_k / s_δ0,q added to the epoch-1 ζ of every party of S_1 (2024 eve and 2021 f180)
  change the density by their N(0, 1) prior alone, to at most 2.5e-11, 7.9e-13 and 3.6e-12,
  each also under 1e-10 + 8 ε |lp|; the literal step (`delta_entry_leak`) misses by at least
  0.50, 43.2 and 12.0, at least 19 times 100 × tol;
- on the toy input above, neither move shifts epoch 1's contrasts or the entry epoch's by more
  than 1e-12, and under the literal step each shifts the entry epoch's by more than 1e-3.

Its limits:
- `house_walk_from_first_poll` is not exercised on 2017 eve, where every house starts in
  epoch 1, as in M1 to M3.
- The re-centring is invisible wherever no party enters, since M_e removes it there; every
  cell has a later entry (Miðflokkurinn, and on 2024 eve and 2021 f180
  Sósíalistaflokkurinn), so `delta_entry_leak` is exercised on each.

The test's lines:

```
v2s-m4 eve2024: 40 points, lp constant range 9.9e-10 (tol 0.000667, ratio 1.48e-06; ratio by kind: post 5.25e-06, off 9.09e-07, prior 1.48e-06); weakest exercised mutant previous_w; range / (100 tol): positional_gather 1.46e+06, previous_w 2.89e+03, next_epoch_delta 1.11e+04, close_last_kjordaemi 3.08e+05, conditional_walk 9.84e+05, house_walk_from_first_poll 7.24e+04, newcomer_mean_of_mu1 5.18e+03, no_mu_restart 1.47e+05, no_house_restart 5.94e+04, newcomer_mean_over_all 3.41e+05, shock_unscaled 6.92e+04, delta_years_unscaled 6.68e+04, no_delta_restart 5.08e+04, delta_entry_leak 3.19e+03; not exercised: none
v2s-m4 f180_2021: 40 points, lp constant range 4.37e-10 (tol 0.000265, ratio 1.65e-06; ratio by kind: post 6.8e-06, off 1.61e-06, prior 1.65e-06); weakest exercised mutant house_walk_from_first_poll; range / (100 tol): positional_gather 2.31e+06, previous_w 2.12e+03, next_epoch_delta 7.09e+04, close_last_kjordaemi 5.34e+05, conditional_walk 1.49e+06, house_walk_from_first_poll 2.04e+03, newcomer_mean_of_mu1 6.38e+03, no_mu_restart 3.64e+04, no_house_restart 3.08e+04, newcomer_mean_over_all 7.84e+05, shock_unscaled 5.45e+04, delta_years_unscaled 1.27e+05, no_delta_restart 4.28e+04, delta_entry_leak 3.76e+03; not exercised: none
v2s-m4 eve2017: 10 points, lp constant range 5.09e-11 (tol 0.000103, ratio 4.96e-07; ratio by kind: prior 4.96e-07); weakest exercised mutant delta_entry_leak; range / (100 tol): positional_gather 1.04e+06, close_last_kjordaemi 1.2e+06, conditional_walk 1.3e+06, newcomer_mean_of_mu1 587, no_mu_restart 4.45e+03, no_house_restart 4.79e+03, newcomer_mean_over_all 4.28e+05, shock_unscaled 3.8e+04, delta_years_unscaled 2.14e+04, no_delta_restart 1.31e+04, delta_entry_leak 370; not exercised: house_walk_from_first_poll
v2s-m4 eve2024 at delta_years_bar 2.5, prior_s_bar_zeta 0.37: 10 prior points, lp constant range 9.31e-10 (tol 0.000668); range / (100 tol): literal_bar_4 9.57e+04, literal_0.2 43.3, prev_years 2.03e+05
```

**The prior predictive rows.** `delta_step_4y`: a 20% party's change in its kjördæmi deviation
over a 4-year gap, a step in an epoch where no party enters, in pp
(`harness/v2/prior_predictive.md`):

| Row | Cell | Values | q05 | q50 | q95 | Band | Verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `delta_step_4y` | 2024 eve | 6000 | −4.69 | −0.05 | 4.18 | [5%, 95%] inside [−15, 15] | PASS |
| `delta_step_4y` | 2021 f180 | 6000 | −4.13 | −0.05 | 4.13 | [5%, 95%] inside [−15, 15] | PASS |

```
PRIOR PASS delta_step_4y/2024-eve/config
PRIOR PASS delta_step_4y/2021-f180/config
```

`kd_delta` now reads δ in the open epoch: its label became "δ in the open epoch: a party's
kjördæmi deviation from its national share, 20% party (static under `delta_walk` = 0,
log s_δ0 = μ_sd + τ_sd r; the walk's under 1)", and under the walk its quantiles widen, as the
open epoch's δ adds the steps to the first epoch's deviation:

| Row | Cell | q05, q50, q95 under `v2s-m3` (static δ) | q05, q50, q95 under `v2s-m4` (the walk) | Band | Verdict |
| --- | --- | --- | --- | --- | --- |
| `kd_delta` | 2024 eve | −10.93, −0.80, 10.40 | −12.49, −1.13, 12.98 | [5%, 95%] inside [−25, 25] | PASS |
| `kd_delta` | 2021 f180 | −11.44, −0.77, 11.58 | −12.24, −0.93, 12.92 | [5%, 95%] inside [−25, 25] | PASS |

Every other summary, table row and verdict line is byte-identical to the report `v2s-m3`'s
config wrote.

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-m4 harness/runs/v2-steps.txt harness/v2/steps/v2s-m4.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m4
```

Next: P2 (`v2s-p2`), the δ form, on `v2s-m4`'s config with `delta_form = 1`, as long as M4
stands; both seeds of `v2s-m4` are its comparison runs.

## v2s-p2: the δ walk held centred (P-step)

`v2s-m4` with `delta_form = 1` (config `harness/configs/steps/v2s-p2.json`, `v2s-m4.json` with
that one field changed): at each closed election δ_e is held in its coordinates
δ_e[2:K, 2:P], the parties without results there non-centred and those with results centred,
shifted per kjördæmi row and stored relative to the result's kjördæmi log-share deviations;
the open epoch's step stays non-centred (`Stan/CLAUDE.md`, the `delta_form` row). It
reparameterises the same model. The R reference `v2_lp_ref()` refuses `delta_form = 1`, so the
three `v2s-p2` tests in `tests/harness/test-v2-stan.R` cover what needs no reference density:
transformed data's refusals of a centred δ it cannot build, the parameters' sizes and the free
size (n_closed (K + P − 1) fewer than `v2s-m4`'s), and each closed election's δ at the result's
log-share deviations when the present coordinates are 0. Its log density and generated
quantities are checked against the non-centred walk by `R/harness/v2_equivalence.R
--form=delta`, whose reference side is the form that `test-v2-stan.R` tests against
`v2_lp_ref()`. `tests/harness/test-v2-equivalence.R` tests that check, with a control whose
present block has its Kronecker factors swapped, and the map's offset and inverse on a small
layout. This section is the check's four results, the sampler check of its fits and the
adoption rule's verdict.

**Equivalence.** The four checks of `EQUIVALENCE_REQUIRED`, as written to
`harness/v2/steps/v2s-p2-equivalence.csv` by `LC_ALL=en_US.UTF-8 Rscript
R/harness/v2_equivalence.R --config=harness/configs/steps/v2s-p2.json --form=delta
--cell=<E>,<F> --out=harness/v2/steps/v2s-p2-equivalence.csv` (`--n=30 --n-off=15` on the
fitted cells, 2017 eve with `--n=20`, the reversed row with `--reverse`), every row on
`Stan/forecast_v2.stan` at sha256 `774680c6…`. Every row: no point dropped, excluded at a cap
or finite on one side only, and each term's negative control `fails`, as it must.

| Cell | Order | Kept points | Constant range | Tolerance | Max path error (δ) | Generated quantities: points; max relative difference | Controls (`ref_delta`, `ref_unused`, `new_delta_absent`) | Verdict |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve | default | 30 posterior + 15 off-posterior | 4.657e-10 | 2.677e-07 | 1.021e-14 | 30; 1.063e-13 | fails, fails, fails | PASS |
| 2021 f180 | default | 30 posterior + 15 off-posterior | 6.548e-11 | 1.797e-07 | 1.976e-14 | 30; 1.915e-13 | fails, fails, fails | PASS |
| 2017 eve | default | 20 prior (no fit, no forecast) | 8.731e-11 | 1.361e-07 | 7.785e-15 | 20; 1.54e-14 | fails, fails, fails | PASS |
| 2024 eve | reversed | 30 posterior + 15 off-posterior | 1.164e-10 | 3.392e-07 | 7.161e-15 | 30; 8.027e-14 | fails, fails, fails | PASS |

On 2017 eve, with one closed election, all three terms vary over the points as well, so the
delta form's tolerance of a control that is not exercised did not come into play on any row.
What one closed election leaves untested is the δ step from the second closed election on
(`harness/README.md`, Step checks).

**Fits.** Four fits at 400 + 400 on `v2s-m4`'s cells and seeds: 2024 eve (lane A) and 2021
f180 (lane B), each with seeds 2701 and 2702, the four `v2s-p2` lines of
`harness/runs/v2-steps.txt`. They ran under `nohup zsh R/harness/queue.sh
results/harness/logs/v2s-p2-runs.txt 400 400` (`harness/README.md`, Step checks):
- The queue started at 2026-11-04 15:16:32. Its preflight (the full slow suite, the two slow
  `v2s-p2` equivalence tests among it) passed at 16:25, `[ FAIL 0 | WARN 0 | SKIP 1 | PASS
  6840 ]`, the skip being the one test that reads the live `data/`.
- Lane A ran 2024 eve seed 2701 from 16:25:34 to 17:25 and seed 2702 from 17:25:26 to 18:11;
  lane B ran 2021 f180 seed 2701 from 16:25:34 to 16:55 and seed 2702 from 16:55:13 to 17:24
  (`queue.log`'s start lines; each `meta.json`'s modification time). So 2024 eve seed 2702 had
  no other fit beside it. All four exited 0, and `queue.log` ends in `QUEUE FINISHED`.
- `Stan/forecast_v2` (git-ignored) was compiled at 15:19, during the preflight (its
  modification time, unchanged since), so all four fits ran that executable, sha256
  `dd739150da8c…`.

CmdStan 2.38.0, `max_treedepth` 12, 10 cores (`hw.ncpu`). The diagnostics table is
`harness/v2/steps/v2s-p2.md`. `queue.log` after the queue's `QUEUE START` line (left out: it
names the run list by its absolute path):

```
preflight PASS
start A v2s-p2_2024-11-30_2024-11-29_s2701 2026-11-04 16:25:34
start B v2s-p2_2021-09-25_2021-03-29_s2701 2026-11-04 16:25:34
end v2s-p2_2021-09-25_2021-03-29_s2701 rc=0 DIAG run=v2s-p2_2021-09-25_2021-03-29_s2701 minutes=29.4 max_rhat=1.0195 min_ess_bulk=180 min_ess_tail=134 divergent=2 treedepth_hits=0 max_depth=10 min_ebfmi=0.549 gate=FAIL
start B v2s-p2_2021-09-25_2021-03-29_s2702 2026-11-04 16:55:13
end v2s-p2_2021-09-25_2021-03-29_s2702 rc=0 DIAG run=v2s-p2_2021-09-25_2021-03-29_s2702 minutes=28.7 max_rhat=1.0228 min_ess_bulk=162 min_ess_tail=230 divergent=6 treedepth_hits=0 max_depth=10 min_ebfmi=0.619 gate=FAIL
end v2s-p2_2024-11-30_2024-11-29_s2701 rc=0 DIAG run=v2s-p2_2024-11-30_2024-11-29_s2701 minutes=59.6 max_rhat=1.0360 min_ess_bulk=133 min_ess_tail=97 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.674 gate=FAIL
start A v2s-p2_2024-11-30_2024-11-29_s2702 2026-11-04 17:25:26
end v2s-p2_2024-11-30_2024-11-29_s2702 rc=0 DIAG run=v2s-p2_2024-11-30_2024-11-29_s2702 minutes=45.5 max_rhat=1.0587 min_ess_bulk=98 min_ess_tail=110 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.663 gate=FAIL
QUEUE FINISHED
```

**Provenance.** All four fits ran at 875c633 on a clean tree (`meta.json`: `git_head` 875c633,
`git_dirty` false), with `Stan/forecast_v2.stan` at sha256
`774680c6adf1f992d7f4d5d15bbaec958c5d336a8c93e120b161f4dd84731cfe` and the config at
`860c3be00140137a5b9bd5c18a325b57034b76e3ea831792915ee1a14cc0de9b`. The commit that logs
this step is 875c633's child and changes no file the builder or the model reads. Each run's
`meta.json` `data_sha256` equals the sha256 of its frozen `inputs/stan_data.json`:
`0ca647464fbcb3d38fb7d8feadfab4be3e5619fd3f5cc1cfb6f70f5e0bcac3fa` on 2024 eve and
`d52dc091401f847c45a3ce9944f62900b3da7229a32240eeb2803c90f7f4c1f4` on 2021 f180. The
`v2s-m4` fits ran with the file at sha256 `8d34b6fe…`; the `delta_form = 0` model is pinned
across that edit by `v2s-m4`'s lp tests, which this queue's preflight ran at 875c633.

**Stop rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-p2` (exit 0):

```
GO v2s-p2_2024-11-30_2024-11-29_s2701 lp_rhat=1.0170 divergent=0/1600 near_cap=0/1600 max_rhat=1.0361; recorded: minutes=59.6 treedepth_hits=0 ess_bulk_per_1k_grad=0.081
GO v2s-p2_2024-11-30_2024-11-29_s2702 lp_rhat=1.0087 divergent=0/1600 near_cap=0/1600 max_rhat=1.0588; recorded: minutes=45.5 treedepth_hits=0 ess_bulk_per_1k_grad=0.060
GO v2s-p2_2021-09-25_2021-03-29_s2701 lp_rhat=1.0071 divergent=2/1600 near_cap=0/1600 max_rhat=1.0196; recorded: minutes=29.4 treedepth_hits=0 ess_bulk_per_1k_grad=0.126
GO v2s-p2_2021-09-25_2021-03-29_s2702 lp_rhat=1.0105 divergent=6/1600 near_cap=1/1600 max_rhat=1.0229; recorded: minutes=28.7 treedepth_hits=0 ess_bulk_per_1k_grad=0.106
```

| Fit (cell, seed) | Verdict | R-hat of `lp__` (STOP above 1.1) | Divergences (STOP above 16) | Draws within 0.5 of a cap (STOP above 16) | Max R-hat, gate scope (STOP above 1.10) | Minutes | Treedepth hits | Min bulk ESS per 1,000 gradients (gate scope) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | GO | 1.0170 | 0 of 1,600 | 0 of 1,600 | 1.0361 | 59.6 | 0 | 0.081 |
| 2024 eve, 2702 | GO | 1.0087 | 0 of 1,600 | 0 of 1,600 | 1.0588 | 45.5 | 0 | 0.060 |
| 2021 f180, 2701 | GO | 1.0071 | 2 of 1,600 | 0 of 1,600 | 1.0196 | 29.4 | 0 | 0.126 |
| 2021 f180, 2702 | GO | 1.0105 | 6 of 1,600 | 1 of 1,600 | 1.0229 | 28.7 | 0 | 0.106 |

**Adoption rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R adopt v2s-m4 v2s-p2
harness/v2/steps/v2s-p2-equivalence.csv` (exit 0), its output verbatim:

Adoption rule (Decision 6): v2s-p2 against the reference form v2s-m4; run list harness/runs/v2-steps.txt

| Fit | Min bulk ESS per 1,000 gradients (gate scope) | Divergences | Stop rule |
| --- | --- | --- | --- |
| v2s-m4_2024-11-30_2024-11-29_s2701 | 0.030 | 0 | GO |
| v2s-m4_2024-11-30_2024-11-29_s2702 | 0.071 | 0 | GO |
| v2s-m4_2021-09-25_2021-03-29_s2701 | 0.064 | 0 | GO |
| v2s-m4_2021-09-25_2021-03-29_s2702 | 0.079 | 0 | GO |
| v2s-p2_2024-11-30_2024-11-29_s2701 | 0.081 | 0 | GO |
| v2s-p2_2024-11-30_2024-11-29_s2702 | 0.060 | 0 | GO |
| v2s-p2_2021-09-25_2021-03-29_s2701 | 0.126 | 2 | GO |
| v2s-p2_2021-09-25_2021-03-29_s2702 | 0.106 | 6 | GO |

| Cell (E, F, tag) | v2s-m4: median min bulk ESS per 1,000 gradients | v2s-p2: median min bulk ESS per 1,000 gradients | Ratio | At least 1.25 × v2s-m4's |
| --- | --- | --- | --- | --- |
| 2024-11-30, 2024-11-29, eve | 0.051 | 0.070 | 1.38 | yes |
| 2021-09-25, 2021-03-29, f180 | 0.071 | 0.116 | 1.61 | yes |

```
1. The equivalence check passes (harness/v2/steps/v2s-p2-equivalence.csv): yes (4 rows, none FAIL, a PASS row of each of the 4 required checks)
2. On each cell, v2s-p2's median min bulk ESS per 1,000 gradients is at least 1.25 × v2s-m4's: yes
3. v2s-p2's divergences over its 4 fits, 8, are at most v2s-m4's, 0, plus 4: no
4. No fit of v2s-m4 or v2s-p2 trips the stop rule: yes
KEEP v2s-m4: v2s-p2 fails condition 3
```

**Verdict: KEEP v2s-m4.** `v2s-p2` fails condition 3: its 8 divergences (2 and 6, both on 2021
f180) are more than `v2s-m4`'s 0 plus 4. Conditions 1, 2 (cell ratios 1.38 and 1.61) and 4
hold. The rule's verdict stands: `delta_form = 0`, the non-centred walk, stays the V2 form, so
`v2s-m5`'s config and `v2.json` (the plan's Task 14) take it from `v2s-m4`'s config, and the
centred form stays built behind its switch, with its tests and its equivalence check.

- **Where the divergences are.** 2 of the 1,600 post-warmup transitions of 2021 f180 seed 2701
  and 6 of seed 2702's; none on 2024 eve. `meta.json` keeps only their sum and the run log only
  each fit's count (cmdstanr's warning and the `DIAG` line); `param_draws.parquet` has no
  `divergent__` column, and `fit_cell.R` deleted the CmdStan CSVs (`HARNESS_KEEP_CMDSTAN`
  unset). So neither their chains nor their iterations are known. The one draw within 0.5 of a
  cap, on 2021 f180 seed 2702, is log κ's (cap 2), 1.543 in chain 1, iteration 245
  (`param_draws.parquet`).
- **What sets the extremes.** `fit_cell.R` measures the gate scope over every
  `parameters`-block variable with the scored quantities, while `key_diagnostics.csv` and
  `param_draws.parquet` hold the `V2_PARAM_VARIABLES` columns, several of them transformed
  parameters. On 2021 f180 log φ_K sets the gate-scope min bulk ESS on both fits, through the
  parameter `log_phi_K_u`, of which `log_phi_K` is a monotone map (as under `v2s-m4`, above):
  its bulk ESS, 180.3 and 162.2, equals `meta.json`'s `min_ess_bulk`. On 2024 eve seed 2701 log
  φ_K sets it too, at 133.2, and its R-hat, 1.0360, is the gate-scope max; the lowest saved
  column there, s_ζ on Viðreisn (`s_zeta[6]`, 127.8), is a transformed parameter
  (s_ζ = s̄_ζ s_zeta_raw), outside the gate scope. On 2024 eve seed 2702 no saved column has
  `meta.json`'s `min_ess_bulk`, 98.4 (the lowest, `s_zeta[6]`, is 91.9). It is
  `s_zeta_raw[6]`'s, Viðreisn's raw step scale, which the files do not hold;
  `s_zeta[6]` ÷ `s_bar_zeta[1]` rebuilds it exactly from `param_draws.parquet`, and the
  rebuilt column's bulk ESS (98.3669), R-hat (1.05875) and tail ESS (110.2794) equal
  `meta.json`'s `min_ess_bulk`, `max_rhat` and `min_ess_tail` to every printed digit. The same
  rebuild finds seed 2701's gate-scope min tail ESS, 97.07, in `s_zeta_raw[10]` (Annað).
- **ESS like for like.** Per fit, `v2s-m4`'s value first (ESS from `key_diagnostics.csv`, the
  gate-scope minimum from `meta.json`, both rounded to one decimal; the correlation over each
  fit's 1,600 draws pooled across chains, `param_draws.parquet` columns `log_phi_K` and
  `s_bar_zeta[1]`):

  | Fit (cell, seed) | log φ_K bulk ESS | s̄_ζ bulk ESS | s̄_ζ tail ESS | corr(log φ_K, s̄_ζ) | s_ζ on Viðreisn, bulk ESS | Gate-scope min bulk ESS (owner) |
  | --- | --- | --- | --- | --- | --- | --- |
  | 2024 eve, 2701 | 50.2; 133.2 | 496.5; 389.4 | 1095.8; 807.5 | 0.313; 0.273 | 67.3; 127.8 | 50.2 (log φ_K); 133.2 (log φ_K) |
  | 2024 eve, 2702 | 117.2; 147.8 | 301.4; 453.0 | 820.7; 812.9 | 0.297; 0.263 | 279.7; 91.9 | 117.2 (log φ_K); 98.4 (`s_zeta_raw[6]`) |
  | 2021 f180, 2701 | 105.6; 180.3 | 289.0; 234.7 | 230.5; 134.2 | 0.440; 0.388 | 250.3; 242.8 | 105.6 (log φ_K); 180.3 (log φ_K) |
  | 2021 f180, 2702 | 126.8; 162.2 | 249.5; 372.8 | 166.7; 292.6 | 0.451; 0.370 | 237.3; 352.9 | 126.8 (log φ_K); 162.2 (log φ_K) |

  log φ_K mixes faster under the centred form on every fit, and the correlation with s̄_ζ is a
  little lower on every fit. On 2024 eve the minimum moves off log φ_K on seed 2702 only.
- **Leapfrog steps.** Every 2024 eve fit of both arms took 1,023 leapfrog steps per
  post-warmup transition (`n_leapfrog` ÷ 1,600 = 1,023.00; tree depth 10 is 1,023 steps). On
  2021 f180 `v2s-p2`'s fits took 894.06 and 955.17 on average, `v2s-m4`'s 1,023.00 and
  1,000.92. So on 2021 f180 part of `v2s-p2`'s higher ESS per 1,000 gradients is fewer
  gradients (12.6% and 4.6% fewer than `v2s-m4`'s fit of the same seed), and the rest its
  higher min bulk ESS.
- **The time.** The per-chain time ratio defined under `v2s-m4` (wall minutes, warmup
  included, ÷ (post-warmup gradients ÷ 4); not a cost per gradient) is 6.61 ms for
  `v2s-m4`'s 2024 eve seed 2702 and 6.68 ms for `v2s-p2`'s (`meta.json` minutes 45.07 and
  45.54, `n_leapfrog` 1,636,800 on both), 1.0% higher. Neither fit had another fit beside it,
  but neither is known to have run unloaded: `v2s-m4`'s had a fix round's test runs beside it
  from about 12:21 to 12:33 (above), and other sessions' load is unknown for both. If both
  warmups took about as many gradients, which is not recorded, the centred form costs about 1%
  more per gradient on that cell, more if that load slowed `v2s-m4`'s fit. How that splits
  between the present block's vec-form Kronecker factor and the model block's recomputation
  was not measured.
  The other three `v2s-p2` fits ran with the other lane's fit beside them (8.74 ms on 2024 eve
  seed 2701, 4.94 and 4.50 ms on 2021 f180), as `v2s-m4`'s did (11.41, 3.63 and 7.52 ms, two
  of them across the sleep), so they do not compare.
- **A reading, not a finding.** The divergences come only with the centred form: `v2s-m4` had
  none on any fit. That is consistent with a funnel between a present party's centred
  coordinates and its step scale, which the non-centred form avoids. s_ζ on Viðreisn is among
  the slowest saved columns under both forms (67.3 and 279.7 under `v2s-m4`, 127.8 and 91.9
  under `v2s-p2` on 2024 eve). Its raw scale, `s_zeta_raw[6]`, rebuilt as `s_zeta[6]` ÷
  `s_bar_zeta[1]` from each fit's `param_draws.parquet` and measured by `posterior::ess_bulk`
  over the four chains, has bulk ESS 196.8 and 515.0 under `v2s-m4` (seeds 2701 and 2702) and
  187.9 and 98.4 under `v2s-p2`. 98.4 is below log φ_K's bulk ESS on that seed under either
  form (117.2 under `v2s-m4`, 147.8 under `v2s-p2`), so seed 2702's minimum moved to the raw
  scale because the raw scale was slow on that seed, not because log φ_K mixed faster; on seed
  2701 it held level. Two seeds cannot say whether the centred form slows it. Where the
  divergences fell is not in the saved outputs, so no funnel is located.
- **The §5.7 gate is not the stop rule.** Each fit's `DIAG` line reads `gate=FAIL`: its
  gate-scope max R-hat (1.0196 to 1.0588) is above 1.01, and the 2021 f180 fits diverged. A
  `dev` arm is never scored.

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-p2 harness/runs/v2-steps.txt harness/v2/steps/v2s-p2.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-p2
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R adopt v2s-m4 v2s-p2 harness/v2/steps/v2s-p2-equivalence.csv
```

Next: M5 (`v2s-m5`), the fundamentals pseudo-observation, on `v2s-m4`'s config
(`delta_form = 0`).

## v2s-m5: the fundamentals pseudo-observation (M-step)

`v2s-m4` with `fundamentals = 1` (config `harness/configs/steps/v2s-m5.json`, `v2s-m4.json` with
that one field changed): stage 1's prediction enters the forecast as a pseudo-observation of
the latent national shares at E (spec §6.5). π_nat,E = Σ_k w_open,k softmax over A_E of
(θ_G + δ_open,k) is built from the grid's last θ and the open epoch's δ and kjördæmi weights,
without the shock ε. With x = log π_nat,E over S_f, the term is μ_f ~ N(x − mean(x), τ_f) over
S_f. Here τ_f = √(V_w (1 − w) / w), with w = `desired_weight` (0.22), and
V_w = diag(CΣCᵀ)[S_f] ((weight_time − campaign_days) + campaign_days (1 + τ_camp)²), where
C = I − 11ᵀ/P centres over all P parties: the walk's variance over the 180-day horizon, with
its last 47 days in the campaign. μ_f is data: stage 1's centred predictor (`stage1_mu_f()`,
`Stan/fundamentals_v2.stan`), gathered by party identity into `sf_idx` and centred over S_f (9
parties on 2024 eve, 8 on 2021 f180). The step adds no parameter, transformed parameter or
generated quantity. Transformed data refuses `fundamentals = 1` when S_f has fewer than 2
parties, repeats an `sf_idx` entry or holds a party absent at E, when w is outside (0, 1), or
when weight_time < campaign_days. Its lp, guard and size tests are in
`tests/harness/test-v2-stan.R`, its reference tests in `tests/harness/test-v2-reference.R`,
and its prior predictive rows in `harness/v2/prior_predictive.md`. This section is its
sampler check.

**Fits.** Two fits at 400 + 400, seed 2701: 2024 eve (lane A) and 2021 f180 (lane B), the two
`v2s-m5` lines of `harness/runs/v2-steps.txt`. They ran under `R/harness/queue.sh` with the run
list `results/harness/logs/v2s-m5-runs.txt` (`harness/README.md`, Step checks):
- The queue started at 2026-11-04 20:13:20 on ae18fbe, on AC, under
  `caffeinate -i -w <queue pid>`. Its preflight (the full slow suite) passed at 21:24:25,
  `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 7237 ]`, the skip being the one test that reads the live
  `data/`.
- Both fits started at 21:24:25. 2021 f180 finished at 21:45:49 and 2024 eve at 22:08:21
  (`queue.log`'s start lines; each `meta.json`'s modification time). Both exited 0, and
  `queue.log` ends in `QUEUE FINISHED`. 2024 eve ran beside 2021 f180 for its first 21.4
  minutes and alone for its last 22.5.
- Beside them: during the preflight, a read-only review agent (no R); during the fits, only
  the controller's `awk` and `grep` reads. Every peer session was idle at 21:3x. Other load is
  unknown.
- `Stan/forecast_v2` (git-ignored) was compiled at 20:16:27, during the preflight (its
  modification time), so both fits ran that executable, sha256 `77ab3d3b0579…`.
- Stage 1 was read from its cache, not fitted. Neither run log has a `STAGE1` line, which
  `stage1_mu_f()` prints only when it fits. The two cache files under `results/harness/stage1/`
  were written at 18:44 that day, before the queue. Each `meta.json`'s `stage1_sha256` is that
  cache's key: `ad28201cf68c…` on 2024 eve and `a4cb18491df8…` on 2021 f180.

CmdStan 2.38.0, `max_treedepth` 12, 10 cores (`hw.ncpu`). The diagnostics table is
`harness/v2/steps/v2s-m5.md`. Below is `queue.log` after the queue's `QUEUE START` line (left
out: it names the run list by its absolute path). Each fit's `DIAG` line in its run log,
`results/harness/logs/v2s-m5_<E>_<F>_s2701.log`, is the same as its `end` line here:

```
preflight PASS
start B v2s-m5_2021-09-25_2021-03-29_s2701 2026-11-04 21:24:25
start A v2s-m5_2024-11-30_2024-11-29_s2701 2026-11-04 21:24:25
end v2s-m5_2021-09-25_2021-03-29_s2701 rc=0 DIAG run=v2s-m5_2021-09-25_2021-03-29_s2701 minutes=21.2 max_rhat=1.0192 min_ess_bulk=152 min_ess_tail=262 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.477 gate=FAIL
end v2s-m5_2024-11-30_2024-11-29_s2701 rc=0 DIAG run=v2s-m5_2024-11-30_2024-11-29_s2701 minutes=43.7 max_rhat=1.0225 min_ess_bulk=229 min_ess_tail=175 divergent=0 treedepth_hits=0 max_depth=10 min_ebfmi=0.576 gate=FAIL
QUEUE FINISHED
```

**Provenance.** Both fits ran at ae18fbe on a clean tree (`meta.json`: `git_head` ae18fbe,
`git_dirty` false), with `Stan/forecast_v2.stan` at sha256
`d6d7b2cf128e98e3a504591096b53353ed23cffc463696ff7f93f40284aa2d71` and the config at
`f77b187d21a634c355cf888819405bfc1a385f52376259fcba988efbce65a0df`. The commit that logs this
step is ae18fbe's child. In `R/harness` it changes only the prior predictive script's header
comment and two of its functions' documentation, which no fit reads. Each run's `meta.json`
`data_sha256` equals the sha256 of its frozen `inputs/stan_data.json`:
`b7917c0c1f43af45b80d0bb41da75bcab9a27c2b7c6f77f41f7a8e4882952b0a` on 2024 eve and
`c04fbe993ae45c5a9864c363f80644ddce432b5cfe20ca36bfb63a98aaf61536` on 2021 f180. The `v2s-m4`
fits ran with the file at sha256 `8d34b6fe…`. The `fundamentals = 0` model is pinned across
the edits since then by every earlier step's lp tests, which this queue's preflight ran at
ae18fbe.

**What the lp tests cover.** The preflight ran the step's Stan tests. On each cell, Stan's log
density minus the reference's is constant within `lp_tol()`, as under `v2s-m4`. On 2024 eve
and 2021 f180 that holds over 20 posterior, 10 off-posterior and 10 prior points; on 2017 eve,
over 10 prior points only. μ_f is stage 1's on 2024 eve and 2021 f180, read from the cache. On
2017 eve it is the placeholder's zeros, since no 2017 forecast is computed before the freeze;
there the term tests the density's form against a zero observation, not stage 1.
- The mutant `fund_uncentred` (the mean x without its centring) is rejected at 2.27e+04,
  3.75e+04 and 5.15e+04 times 100 × tol on 2024 eve, 2021 f180 and 2017 eve. Every earlier
  mutant is rejected as under `v2s-m4`.
- At w = 0.3, weight_time = 150 and campaign_days = 40 on 2024 eve's prior points, Stan
  matches the reference. The literals 0.22, 180 and 47, written into Stan in their place, would
  be rejected at 9.47e+03, 2.61e+03 and 2.18e+03.
- The switch moves the log density by the reference's term plus one constant per cell:
  8.27045, 7.35151 and 6.43257. These are n_sf log(2π) / 2 for n_sf = 9, 8 and 7, which Stan's
  `~` drops. The switch adds no variable to the model.
- The guards refuse each condition above with its message, and the same data pass with the
  switch off. At a prior point the term's mean is the centred log of Stan's own `pi_latent_E`
  over S_f.
- Four ablation configs each combine switches that no step ran: `v2-noepoch`, `v2-nonew`,
  `v2-noshock` and `v2-staticdelta`. Each matches the reference at 10 prior points on 2024 eve
  and on 2021 f180: every range is below 1.2e-09, against a tolerance of at least 2.58e-04.
  `v2-nofund` is `v2s-m4`'s config, so that step's lp tests cover it.

The test's lines:

```
v2s-m5 eve2024: 40 points, lp constant range 1.05e-09 (tol 0.000667, ratio 1.57e-06; ratio by kind: post 6.76e-06, off 2.98e-06, prior 1.57e-06); weakest exercised mutant previous_w; range / (100 tol): positional_gather 1.46e+06, previous_w 2.89e+03, next_epoch_delta 1.98e+04, close_last_kjordaemi 6.26e+05, conditional_walk 7.69e+05, house_walk_from_first_poll 7.24e+04, newcomer_mean_of_mu1 9.41e+03, no_mu_restart 1.47e+05, no_house_restart 4.02e+04, newcomer_mean_over_all 3.41e+05, shock_unscaled 6.92e+04, delta_years_unscaled 6.03e+04, no_delta_restart 1.3e+05, delta_entry_leak 3.18e+03, fund_uncentred 2.27e+04; not exercised: none
v2s-m5 f180_2021: 40 points, lp constant range 3.78e-10 (tol 0.000265, ratio 1.43e-06; ratio by kind: post 6.2e-06, off 1.24e-06, prior 1.43e-06); weakest exercised mutant house_walk_from_first_poll; range / (100 tol): positional_gather 2.26e+06, previous_w 2.12e+03, next_epoch_delta 7.08e+04, close_last_kjordaemi 7.99e+05, conditional_walk 1.47e+06, house_walk_from_first_poll 1.88e+03, newcomer_mean_of_mu1 1.22e+04, no_mu_restart 4.17e+05, no_house_restart 2.42e+05, newcomer_mean_over_all 7.84e+05, shock_unscaled 8.41e+04, delta_years_unscaled 2.77e+05, no_delta_restart 9.41e+04, delta_entry_leak 9.88e+03, fund_uncentred 3.75e+04; not exercised: none
v2s-m5 eve2017: 10 points, lp constant range 5.82e-11 (tol 0.000103, ratio 5.66e-07; ratio by kind: prior 5.66e-07); weakest exercised mutant delta_entry_leak; range / (100 tol): positional_gather 1.04e+06, close_last_kjordaemi 1.2e+06, conditional_walk 1.3e+06, newcomer_mean_of_mu1 586, no_mu_restart 4.44e+03, no_house_restart 4.78e+03, newcomer_mean_over_all 4.27e+05, shock_unscaled 3.8e+04, delta_years_unscaled 2.14e+04, no_delta_restart 1.31e+04, delta_entry_leak 370, fund_uncentred 5.15e+04; not exercised: house_walk_from_first_poll
v2s-m5 eve2024 at desired_weight 0.3, weight_time 150, campaign_days 40: 10 prior points, lp constant range 9.31e-10 (tol 0.000667); range / (100 tol): literal_0.22 9.47e+03, literal_180 2.61e+03, literal_47 2.18e+03
v2s-m5 eve2024: the switch's lp change minus the term, range 2.24e-11 (tol 0.000448), constant 8.27045
v2s-m5 f180_2021: the switch's lp change minus the term, range 1.28e-11 (tol 0.000198), constant 7.35151
v2s-m5 eve2017: the switch's lp change minus the term, range 9.41e-12 (tol 0.000143), constant 6.43257
v2-noepoch eve2024: 10 prior points, lp constant range 9.9e-10 (tol 0.000559, ratio 1.77e-06)
v2-noepoch f180_2021: 10 prior points, lp constant range 1.46e-10 (tol 0.000258, ratio 5.63e-07)
v2-nonew eve2024: 10 prior points, lp constant range 9.9e-10 (tol 0.000665, ratio 1.49e-06)
v2-nonew f180_2021: 10 prior points, lp constant range 3.49e-10 (tol 0.000263, ratio 1.33e-06)
v2-noshock eve2024: 10 prior points, lp constant range 8.73e-10 (tol 0.000668, ratio 1.31e-06)
v2-noshock f180_2021: 10 prior points, lp constant range 4.37e-10 (tol 0.000265, ratio 1.65e-06)
v2-staticdelta eve2024: 10 prior points, lp constant range 1.16e-09 (tol 0.000667, ratio 1.75e-06)
v2-staticdelta f180_2021: 10 prior points, lp constant range 4.51e-10 (tol 0.000265, ratio 1.7e-06)
```

**The prior predictive rows.** Both rows read the term's weight for each party of S_f (a
weight, not pp), pooled over the parties (`harness/v2/prior_predictive.md`).
- `fund_weight` is the weight against V_w: 0.22 by construction, so it checks the code, not
  the model.
- `fund_weight_bias` adds to V_w the variance over the prior draws of the industry bias's
  open-epoch step, centred over S_f. It is reported, with no band.

| Row | Cell | Values | q05 | q50 | q95 | Band | Verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `fund_weight` | 2024 eve | 9000 | 0.22 | 0.22 | 0.22 | every draw within 1e-08 of 0.22 | PASS |
| `fund_weight` | 2021 f180 | 8000 | 0.22 | 0.22 | 0.22 | every draw within 1e-08 of 0.22 | PASS |
| `fund_weight_bias` | 2024 eve | 9000 | 0.22 | 0.24 | 0.33 | none (reported) | REPORT |
| `fund_weight_bias` | 2021 f180 | 8000 | 0.22 | 0.23 | 0.32 | none (reported) | REPORT |

```
PRIOR PASS fund_weight/2024-eve/config
PRIOR PASS fund_weight/2021-f180/config
PRIOR REPORT fund_weight_bias/2024-eve/config 0.22 0.24 0.33
PRIOR REPORT fund_weight_bias/2021-f180/config 0.22 0.23 0.32
```

V_w is centred over all P parties, as §6.5 writes it, while the term's mean and V_bias are
centred over S_f. So the 0.22 target holds against V_w, not against the variance of the
centred mean. That choice follows the spec as written and is left to Brynjólfur. Every other
summary, table row and verdict line is the same as in the report `v2s-m4`'s config wrote,
apart from the Config line and the Results header's wording.

**Stop rule.** `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m5` (exit 0):

```
GO v2s-m5_2024-11-30_2024-11-29_s2701 lp_rhat=1.0077 divergent=0/1600 near_cap=0/1600 max_rhat=1.0225; recorded: minutes=43.7 treedepth_hits=0 ess_bulk_per_1k_grad=0.139
GO v2s-m5_2021-09-25_2021-03-29_s2701 lp_rhat=1.0092 divergent=0/1600 near_cap=0/1600 max_rhat=1.0192; recorded: minutes=21.2 treedepth_hits=0 ess_bulk_per_1k_grad=0.092
```

| Fit (cell, seed) | Verdict | R-hat of `lp__` (STOP above 1.1) | Divergences (STOP above 16) | Draws within 0.5 of a cap (STOP above 16) | Max R-hat, gate scope (STOP above 1.10) | Minutes | Treedepth hits | Min bulk ESS per 1,000 gradients (gate scope) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve, 2701 | GO | 1.0077 | 0 of 1,600 | 0 of 1,600 | 1.0225 | 43.7 | 0 | 0.139 |
| 2021 f180, 2701 | GO | 1.0092 | 0 of 1,600 | 0 of 1,600 | 1.0192 | 21.2 | 0 | 0.092 |

**Verdict: GO on both fits.** The stop rule's signals are all inside their limits:
- the largest R-hat of `lp__` is 1.0092;
- no transition diverged;
- no draw came within 0.5 of a cap;
- the largest gate-scope R-hat is 1.0225.

The §5.7 gate is not the stop rule. Each fit's `DIAG` line reads `gate=FAIL`, because its
gate-scope max R-hat (1.0225 and 1.0192) is above the scoring gate's 1.01 at 400 + 400. Tier 0
is not gated, so the gate column is informational, and a `dev` arm is never scored.

**Like for like with `v2s-m4`.** Each `v2s-m5` fit is set beside `v2s-m4`'s fit of the same
cell and seed; on 2024 eve, `v2s-m4`'s refit (seed 2702) is shown too. Both seed-2701 pairs ran
two lanes at once. The values are from each `meta.json`. ESS is to one decimal, R-hat is
rounded up and E-BFMI and ESS per 1,000 gradients down, as the diagnostics tables print them:

| Fit (arm, cell, seed) | Minutes | Leapfrog steps per post-warmup transition | Min bulk ESS (gate scope) | Min tail ESS (gate scope) | Max R-hat (gate scope) | Divergences | Min E-BFMI | Min bulk ESS per 1,000 gradients | Per-chain time ratio |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `v2s-m4`, 2024 eve, 2701 | 77.8, 28 of them across the sleep (23.6 asleep) | 1,023.00 | 50.2 | 225.4 | 1.0624 | 0 | 0.698 | 0.030 | 11.41 ms |
| `v2s-m4`, 2024 eve, 2702 (the refit, alone) | 45.1 | 1,023.00 | 117.2 | 264.2 | 1.0352 | 0 | 0.607 | 0.071 | 6.61 ms |
| `v2s-m5`, 2024 eve, 2701 | 43.7 | 1,023.00 | 228.5 | 175.3 | 1.0225 | 0 | 0.576 | 0.139 | 6.41 ms |
| `v2s-m4`, 2021 f180, 2701 | 24.7 | 1,023.00 | 105.6 | 148.8 | 1.0372 | 0 | 0.541 | 0.064 | 3.63 ms |
| `v2s-m5`, 2021 f180, 2701 | 21.2 | 1,023.00 | 152.0 | 262.1 | 1.0192 | 0 | 0.477 | 0.092 | 3.11 ms |

- **Gradients.** Every fit took 1,023.00 leapfrog steps per post-warmup transition
  (`n_leapfrog` ÷ 1,600; tree depth 10 is 1,023 steps). So the ESS per 1,000 gradients moves
  with the min bulk ESS alone. Against `v2s-m4` it is 4.6 times seed 2701's and 2.0 times the
  refit's on 2024 eve, and 1.4 times on 2021 f180 (from the unrounded `min_ess_bulk` ÷
  `n_leapfrog`).
- **The time.** For 2024 eve the comparison is with the refit, seed 2702, since seed 2701's
  minutes include the sleep. The per-chain time ratio (defined under `v2s-m4`; not a cost per
  gradient) is 6.41 ms against 6.61 ms, 3.1% lower. On 2021 f180 it is 3.11 ms against
  3.63 ms, 14% lower. The loads differ:
  - On 2024 eve, `v2s-m5`'s fit had the other lane beside it for its first 21.4 minutes, while
    the refit ran alone but with a fix round's tests beside it for about 12 minutes.
  - On 2021 f180 both fits had the other lane beside them throughout. `v2s-m4`'s also had the
    post-wake system work and a peer's simulation.

  The term adds no parameter. Neither cell shows a time cost, but these loads cannot resolve a
  small one.
- **Which variable sets each extreme.** `posterior::ess_bulk`, `ess_tail` and `rhat` were run
  over each fit's four chains of 400 draws in `param_draws.parquet`: every saved column, plus
  each `s_zeta_raw[j]` rebuilt as `s_zeta[j]` ÷ `s_bar_zeta[1]`, as under `v2s-p2`. These are
  the five lowest bulk ESS:

  | Fit (arm, cell, seed) | The five lowest bulk ESS (saved and rebuilt columns) |
  | --- | --- |
  | `v2s-m4`, 2024 eve, 2701 | `log_phi_K` 50.2, `s_zeta[6]` 67.3, `s_zeta_raw[6]` (rebuilt) 196.8, `s_zeta[2]` 312.8, `s_bar_E[1]` 332.9 |
  | `v2s-m5`, 2024 eve, 2701 | `log_phi_K` 228.5, `s_zeta[6]` 244.6, `s_zeta[2]` 304.7, `s_zeta[4]` 329.8, `s_zeta_raw[6]` (rebuilt) 345.6 |
  | `v2s-m4`, 2021 f180, 2701 | `log_phi_K` 105.6, `s_zeta[3]` 226.0, `s_zeta[6]` 250.3, `s_bar_zeta[1]` 289.0, `s_zeta[2]` 338.6 |
  | `v2s-m5`, 2021 f180, 2701 | `log_phi_K` 152.0, `s_zeta[3]` 300.1, `s_zeta[6]` 323.4, `s_bar_zeta[1]` 355.1, `s_zeta[2]` 374.5 |

  - **Gate-scope min bulk ESS.** log φ_K sets it on all four fits. The gate scope reads the
    parameter `log_phi_K_u`, of which `log_phi_K` is a monotone map, so their bulk ESS agree.
    Under `v2s-m5` it is 228.5457 and 152.0474, equal to `meta.json`'s `min_ess_bulk` to every
    printed digit.
  - **Gate-scope max R-hat, 2024 eve.** Under `v2s-m5` τ_μ (`tau_mu[1]`, the industry bias's
    step scale) sets it, at 1.02250, equal to `meta.json`'s; log φ_K's is 1.01041.
  - **Gate-scope max R-hat, 2021 f180.** Under `v2s-m5`, `meta.json`'s 1.01917 matches no saved
    or rebuilt column. The largest is log φ_K's, 1.01615, and `s_delta0_raw`, rebuilt as
    (log `s_delta0[j]` − `mu_sd`) ÷ `tau_sd`, reaches 1.00591 at most. So 1.01917 belongs to a
    raw parameter the files do not hold, and it is not located.
  - **Max R-hat under `v2s-m4`.** log φ_K set it on both cells, at 1.06238 and 1.03712.
  - **Gate-scope min tail ESS.** Under `v2s-m5`:
    - on 2024 eve it is `s_zeta_raw[2]`'s, Framsóknarflokkurinn's raw step scale, rebuilt, at
      175.3141, equal to `meta.json`'s. `s_zeta[2]`'s tail ESS is lower, 174.0, but s_ζ is a
      transformed parameter, outside the gate scope;
    - on 2021 f180 it is s̄_ζ's, 262.0812.

    Under `v2s-m4` it was τ_μ's (225.4) and log φ_K's (148.8).
- **A reading of one seed, not a finding.** log φ_K stays the slowest saved column on both
  cells, as under every `v2s-m4` fit and `v2s-p2` on 2021 f180. On this seed it mixes faster:
  - bulk ESS 228.5 against 50.2 on 2024 eve, and 152.0 against 105.6 on 2021 f180;
  - R-hat 1.01041 against 1.06238, and 1.01615 against 1.03712.

  But `v2s-m4`'s two seeds on 2024 eve already gave 50.2 and 117.2. So one seed of `v2s-m5`
  cannot say whether the term speeds log φ_K. corr(log φ_K, s̄_ζ) over each fit's 1,600 draws
  is 0.294 on 2024 eve and 0.409 on 2021 f180; `v2s-m4` seed 2701 gave 0.313 and 0.440.

To rewrite the table and the verdicts from the finished runs under `results/harness/`, from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2s-m5 harness/runs/v2-steps.txt harness/v2/steps/v2s-m5.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2s-m5
```

M5 stands, so every component is built: `harness/configs/v2.json` holds `v2s-m5`'s config
under the candidate role, and the Phase 2 plan's Tasks 15 and 16 fit it.
