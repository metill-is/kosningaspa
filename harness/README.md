# v2 backtest harness

The harness builds frozen, leak-checked inputs for the v2 forecast backtests: official
results, poll availability, fundamentals target rows and seat layouts, for the forecast
cells of the 2021 and 2024 elections and the held-out 2017 confirmatory cells. Code lives
in `R/harness/` (box modules), tests in `tests/harness/`.

Spec: the note "2026-10-23 — v2 forecast model and two-election harness (design)" in the
Metill vault (Kosningar/Forecast model).

## The snapshot

The harness never reads the live `data/`. `snapshot_inputs(src_root)` copies the files in
`SNAPSHOT_FILES` (`R/harness/inputs.R`) from the main checkout into `harness/snapshot/`
(gitignored) and writes `harness/snapshot_manifest.csv` (name, file, source, bytes, sha256;
committed). Every read goes through `snapshot_file(name)`, which checks that file's sha256
against the manifest; `verify_snapshot()` checks them all. `snapshot_inputs()` refuses an
existing `harness/snapshot/`: to re-snapshot, delete it by hand, re-run, and commit the new
manifest.

## Tests

From the project root: `LC_ALL=en_US.UTF-8 Rscript tests/run.R [filter]`, which runs the
suite under testthat edition 3. Tests that need the snapshot call `skip_without_snapshot()`
and skip without it. `tests/harness/test-generated.R` fails if a generated committed file
that builds offline is not a fresh build. Slow tests (the sweep of every forecast day in
`tests/harness/test-sweep.R`, the self-test's kjördæmi and seat scores, the R0 Stan tests
and the cell runner's test fits of `r0` and `v2s-m0` on the 2024 eve, which compile with CmdStan) call `skip_unless_slow()` and
run only with `HARNESS_SLOW=1` in the environment:
`HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R`. The one test that reads the live
`data/` (the Kosning rows of `data/polling_data.csv`, in `tests/harness/test-election-data.R`)
calls `skip_unless_live_data()` and runs only with `KOSNINGASPA_LIVE_DATA=1`, so the queue's
preflight never depends on the live data:
`KOSNINGASPA_LIVE_DATA=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R election-data`. The tests of `Stan/forecast_v2.stan`
in `tests/harness/test-v2-stan.R` are slow and named by the V2 step they test (`v2s-m0: …`);
`V2_STEPS=<step>[,<step>…]` runs only those steps' tests (`skip_unless_step()`), and unset it
runs them all. `HARNESS_V2_STAN=<absolute path>` runs that file's tests on a scratch copy of
the Stan file instead, for a planted fault's RED: the file must exist and the path be
absolute, or the test file stops as it loads, and the tests announce the path and its
sha256, so every log records the switch; unset, they test `Stan/forecast_v2.stan` and print
nothing. `queue.sh` refuses to start while it is set, so the preflight never certifies
another file than the one the fits use. The slow `v2s-m5` tests, and so the queue's
preflight, run the real `stage1_mu_f()`, which fits stage 1 into `results_root()/stage1/` on
a cold cache or reads it there; that fit's warmup prints "Prior size parameter is 0"
notices, which are expected. The slow tests of the equivalence check in
`tests/harness/test-v2-equivalence.R` are named and gated the same way (`v2s-p1: …`): each
runs `R/harness/v2_equivalence.R` in a subprocess into a temporary CSV, one of them on a
copy of the Stan file with a planted fault, which must fail for the constant's range alone. Its
fast tests check the CSV writer, its provisional rows, and the command lines the script
refuses before it compiles anything: they run it with `CMDSTAN` naming an empty directory,
after checking that cmdstanr then finds no CmdStan, so a refusal that regressed past the
first CmdStan call fails its message check. The model
methods these tests use (`v2_model()`, `v2_methods()`, `short_fit_draws()`, …) are the
module `R/harness/v2_methods.R`, which `tests/harness/helper-v2.R` imports, so the
equivalence script runs the same heap-safe code. The fast test of the cell runner's 2017 refusal reads this repository's real
freeze state, under a temporary `HARNESS_RESULTS`, and skips once the freeze is sealed. It
runs the runner with `CMDSTAN` naming an empty directory, after checking that cmdstanr then
finds no CmdStan, so a regressed refusal stops at its first cmdstanr call and can neither
compile nor sample a 2017 cell.

The R code in `R/harness/` and `R/election_utils.R` writes every non-ASCII character as a
`\uXXXX` escape and gives a non-ASCII name with `stats::setNames()`, never as a name tag;
comments keep their Icelandic. R parses a module in the locale active when it is loaded.
Under C it translates every name tag to a native symbol, escaped or not, so
`c("Framsókn" = …)` became `Frams<U+00F3>kn` and Framsóknarflokkurinn silently dropped
out of the fundamentals' party set. A literal survives such a load only because box tells
the parser the file is UTF-8; `source()` or a bare `parse()` reads it natively.
`tests/harness/test-escapes.R` enforces both rules and loads the harness under `LC_ALL=C`
in a subprocess to check that it still reads the snapshot right. `read_inputs()` and
`build_phase0.R` still stop under a locale that is not UTF-8, but that check cannot stand
in for the escapes: it runs when they are called, after the modules were parsed, so a
session that loads the harness under C and then switches to UTF-8 passes it.

## The scorer self-test

Before it scores anything new, the scorer must reproduce a run that an earlier study
already scored (spec §5.6, which names the study's folder). `selftest_inputs()`
(`R/harness/selftest.R`) copies two of that run's draw files, `SELFTEST_FILES`, into
`harness/selftest/` (gitignored) and writes `harness/selftest_manifest.csv` (committed)
through the snapshot machinery, so every read checks the file's sha256. Once, from the
project root:

```r
options(box.path = normalizePath("."))
box::use(R/harness/selftest[selftest_inputs])
selftest_inputs("<path to the overdisp study>")
```

Like `snapshot_inputs()`, it refuses an existing `harness/selftest/`. Re-copied from the
same files, the manifest comes out unchanged.

`tests/harness/test-selftest.R` re-scores those draws against the committed 2024 result
(`cell_truth()`) with the functions of `R/harness/scores.R`, and must reproduce the study's
published scores to a tolerance of 1e-9: the national scores (CRPS10, IS90, DSS10, mDSS,
d², energy score, MAE8, the MAE over all ten parties, 90% width and coverage, zero cells
replaced) with their batch-means and MCSE standard errors, the kjördæmi scores, and the
seat scores with the seat-CRPS standard error and the majority probability of the
coalition that formed. The reference values are written in the test file and are never
changed: a miss means the scorer is wrong. The national test runs whenever
`harness/selftest/` exists; the kjördæmi and seat tests also need `HARNESS_SLOW=1`
(allocating seats for the run's 8000 draws took 107 s on 24 Oct 2026). Without the folder
they skip, but the test of the committed manifest still runs.

It is the phase's one scoring of a known result. No harness forecast is compared with its
result before the freeze (spec §8), and the self-test keeps that rule: it re-scores draws
the earlier study had already scored and published, not a harness arm's, so no harness arm
meets a result.

## The freeze and the 2017 locks

`score_cell(run_dir)` (`R/harness/scorer.R`) is the only function that scores a forecast
cell against its result (spec §5.6–§5.7). It reads the run's `meta.json`, refuses an arm
whose role is `check` or `dev` and a run whose `meta.json` records a role other than its
arm's (Runs, Arms), calls `freeze_guard(E, F)` (`R/harness/freeze.R`), and only once the guard passes reads the
result (`cell_truth()`) and then the draws. With the default result reader, which reads
this project's results, it refuses a `root` other than the project's, since the guard
would otherwise check another repository's freeze. The guard stops at the first failure:

1. (E, F) is not a cell of `TARGETS`;
2. `harness/FREEZE.md` or `harness/FREEZE.sha256` does not exist;
3. `FREEZE.sha256` does not list `harness/FREEZE.md`;
4. a file it lists is missing or its sha256 differs (`verify_sha_list()`);
5. the two files are not committed, that is tracked by git and unchanged against HEAD
   (`is_committed()`, which compares each file's `git hash-object` with its blob in HEAD,
   so an edit the index is told to ignore, by `--assume-unchanged` or `--skip-worktree`,
   still counts).

A 2017 cell, the confirmatory target, is locked until `harness/DECISION.md` and
`harness/DECISION.sha256` pass checks 2–5 as well (the refusal names the check that
failed), and locked for good once `harness/CONFIRM.md` exists. Past both locks, `score_cell()` still refuses it: 2017 is
scored once, by the Tier C runbook, which writes `CONFIRM.md`. Phase 1 has no such writer.

A sha256 list holds the lines `shasum -a 256` writes (`<64 lowercase hex>  <path>`), with
paths relative to the project root and inside it; lines that start with `#` and blank lines
are skipped, and any other line, an absolute path or a path with a `..` segment among them,
is an error. To freeze, from the project root:

```sh
shasum -a 256 harness/FREEZE.md <every other frozen file> > harness/FREEZE.sha256
```

and commit both files. The list cannot hold its own sha256; the commit check covers it.

What `score_cell()` returns, per cell:

- `national`: the scored reading, `kd_aggregate`, sums the kjördæmi vote draws to national
  shares (`kd_to_nat()`), with `zero_share = 0.5 / sum(n_pred_k)`. The `eday_national`
  reading, the model's own national shares with `zero_share = 0.5 / n_pred`, is reported
  beside it, and `gap_pp` is the largest gap between the two readings' posterior means,
  in points. Each row carries batch-means SEs over 16 blocks (`batch_of()`, four per
  chain) of CRPS10, IS90, DSS10, mDSS, d², MAE8 and the 50% and 90% coverage counts.
- `per_party` (with the PIT's batch SE), `kjordaemi` (`kd_scores()`, with the batch SEs
  `se_kd_crps` and `se_kd_cov90`), and `seats` and `seats_per_party`: the seats of each
  draw allocated with Annað ineligible, scored for the Brier coalition recorded in
  `meta.json`, with the majority probability of the coalition that formed after E
  (`formed_coalition()`, hindsight that only `score_cell()` reads). `seats` carries the
  batch SEs `se_seat_crps`, `se_seat_cov90` and `se_brier`.

Every batch SE is over the same 16 blocks: the score is recomputed on each block's draws,
and its SE is their standard deviation over √16. So the tie rule of spec §5.7 (a
difference under 2 × SE_diff) can be applied to every scored quantity, and coverage comes
with its Monte Carlo error.

Past the guard and the 2017 refusal, it recomputes the Brier event,
`coalition_event(F, read_coalitions())`, and stops with "coalition event changed since the
fit: refit <run>" unless its parties are those `meta.json` recorded: the sha-skip keys do
not cover the event, so a reused run could otherwise be scored against an outdated one. It
stops with "no list at E" if a party the run forecasts has a national result of 0, as a
party with no list would, since the alr scores would take the log of that zero. Until Phase
3 commits `FREEZE.md`, it refuses every target cell of this repository;
`tests/harness/test-scorer.R` checks that with a result reader that fails if it is called.
The tests that pass the guard run in temporary git repositories.

## Runs

`R/harness/runs.R` keeps the record of each fit (spec §5.5, §5.7, §5.8): its id and
directory, its frozen inputs, the keys that decide whether a finished run is reused, the
diagnostics summary, the gate and the DIAG line. It fits nothing; the cell runner builds on
it.

**Ids and directories.** `run_id(arm, E, F, seed)` is `<arm>_<E>_<F>_s<seed>`, for example
`r0_2024-11-30_2024-11-29_s1101`; an arm's name holds only letters, digits and hyphens.
`run_dir(id)` is `results_root()/<id>`, and `results_root()` is `results/harness/` under the
project root (gitignored), or `HARNESS_RESULTS` when that is set and not empty. The tests
set it to a temporary directory and never write a run under the project.

```
results/harness/
  <id>/
    inputs/stan_data.json   the Stan data (cmdstanr::write_stan_json()), mode 0444
    inputs/inputs.json      how the data were built, mode 0444
    eday_kjordaemi.parquet  the draws, as the arm's extractor writes them (spec §5.5)
    eday_national.parquet
    latent_national.parquet
    param_draws.parquet
    ppc_results.parquet     V2 only: the closed elections' in-sample replicates
    key_diagnostics.csv     R-hat and ESS of the scored quantities, the kept parameters and lp__
    meta.json               written last by the runner: present means finished
  _stale/<id>_<YYYYmmddHHMMSS>/   a retired run, kept as it was
  stage1/<sha256>/mu_f.json  V2's stage-1 output, cached by the sha256 of its inputs (R/harness/stage1.R)
  logs/                     the queue's logs: queue.log, preflight.log, <id>.log, and its lock, queue.lock/
```

The runner makes every file of a finished run read-only (mode 0444).

`write_frozen_inputs(dir, stan_data, inputs_meta)` writes the two input files into a
temporary sibling directory, makes them read-only there and renames that directory to
`inputs/`, so a failed write leaves no `inputs/` to block a retry. It returns
`stan_data_sha(stan_data)`, the sha256 of the data as `cmdstanr::write_stan_json()` writes
them, after checking that it equals the sha256 of the written `stan_data.json`. It refuses
a run whose `inputs/` exists, so frozen inputs are never rewritten in place. The runner adds the draws and
`meta.json` beside them. `meta.json` holds the `expected_meta()` keys and more besides (the
arm, E, F, the builder's meta, minutes, diagnostics, the gate and provenance).

**Sha-skip.** `expected_meta()` lists the keys a finished run must match to be reused: the
sha256 of the Stan file, the config and the Stan data (`stan_data_sha()` of the freshly
built data, never the hash of the run's own frozen file, which would compare the old data
with itself; the slow `test-fit-cell.R` edits a run's frozen file and expects SKIP), the
seed, the warmup and sampling
iterations, the sampler settings of `SAMPLER` (4 chains, `init = 0`, `max_treedepth = 12`)
and `EXTRACT_VERSION`. `meta_mismatch(meta, expected)` returns the keys of `expected` whose
values differ or are missing from `meta`, and ignores every other key of `meta`. Numbers
compare by value, since a JSON round trip turns `0` into `0L`; every other value must be
identical, so a number written as a string, a null or an NA is a mismatch. An empty result
means the run is reused. Otherwise `retire_run(dir)` moves the directory under `_stale/`,
never deleting it, and the cell is refitted. Bump `EXTRACT_VERSION` whenever the
extraction changes, so that every finished run is refitted.

**Diagnostics.** `summarise_diagnostics(params, scored, sampler)` takes the draws of every
`parameters`-block variable, the draws of the scored quantities, and the sampler's
per-chain divergences, treedepth hits, E-BFMI, deepest tree and post-warmup leapfrog steps
(the last two from `chain_sampler()` of `fit$sampler_diagnostics()`).
It reports the max R-hat
and the min bulk and tail ESS over parameters and scored quantities together (the plan's
Decision 3) and over the scored quantities alone, the divergences and treedepth hits
summed over chains, the deepest tree, the min E-BFMI and `n_leapfrog`, the post-warmup
leapfrog steps (gradients) summed over chains, which puts ESS per gradient within reach of
`meta.json` alone. A parameter with no R-hat or ESS,
such as a constant entry of a Cholesky factor, is left out of the extremes. A scored
quantity with none (a constant, or a draw that is NA) is not: it makes every extreme it
enters NA, and `na_scored` counts such quantities. The deepest tree shows whether
trajectories end by U-turn below `max_treedepth`.

**The gate (spec §5.7).** `diagnostics_gate(d)` passes a cell if the max R-hat over
parameters and scored quantities is at most 1.01, there are no divergences, the min bulk
ESS of the scored quantities is at least 400, and every scored quantity has an R-hat and
both ESS (`na_scored == 0`). Its `reasons` hold one line per failed rule with the value
that failed it, for example `max R-hat 1.0234 > 1.01`, `divergent 2 > 0`,
`min scored bulk ESS 399 < 400` or `scored quantities with NA R-hat or ESS: 2`; a missing
value fails its rule. R-hat is printed with as many decimals as it takes to show it above
the ceiling (`1.01004 > 1.01`), and ESS is rounded down, so no reason reads as passing. The
spec gives a failing cell one rerun at twice the sampling iterations and counts a second
failure against its arm; that rerun belongs to the scoring runbook, not to Phase 1.

**DIAG.** Each finished run prints one line, `diag_line(id, minutes, d, gate)`:

```
DIAG run=<id> minutes=%.1f max_rhat=%.4f min_ess_bulk=%.0f min_ess_tail=%.0f divergent=%d treedepth_hits=%d max_depth=%d min_ebfmi=%.3f gate=PASS|FAIL
```

with R-hat and ESS over parameters and scored quantities together. Its format is fixed,
so it leaves out `na_scored`: a scored NA shows as `gate=FAIL` (and as `NA` in the
extremes), and the gate's reasons are in `meta.json`.

**Arms.** `R/harness/arms.R` holds `ARMS`, one entry per arm, and `arm(name)` returns one
(an error for an unknown name). An entry names the arm's Stan file and config (paths under
the project root), the config's reader, the builder from a cell's `info_set()` and the
config to `list(data, meta)`, the extractor from a fit's draws to the parquets, the
variables the extractor reads (a vector, or a function of the config), and the arm's role,
one of `ARM_ROLES`:

| Role | Arms | Scored |
|---|---|---|
| `reference` | `r0` | yes, after the freeze |
| `candidate` | `v2` | yes, after the freeze |
| `ablation` | `v2-noepoch`, `v2-nonew`, `v2-noshock`, `v2-staticdelta`, `v2-nofund` | yes, after the freeze |
| `check` | `v2-perm`, V2 with both orders reversed, which must reproduce `v2` | never |
| `dev` | `v2s-m0` and the rest of V2's staircase, one component or form per step | never |

`score_cell()` refuses a `check` or `dev` arm before the freeze guard, and a run whose
`meta.json` records a role other than its arm's; a `meta.json` without `role`, as R0's
Phase 1 runs have, takes its arm's. The role is not a sha-skip key (`expected_meta()`), so
those runs still SKIP. If an arm's role changes in `ARMS`, its finished runs still SKIP.
`score_cell()` refuses each one that records the old role with "run role <old> differs from
arm <arm>'s role <new>"; retire such a run (`retire_run()`) and refit it. A run with no
recorded role takes its arm's current role and is not refused. Plan 4 may add roles for the arms it scores. Every arm
but `r0` runs `Stan/forecast_v2.stan` with its own config, an entry `v2_arm(config, role)`
builds. `ARMS` holds `r0`, `v2s-m0`, `v2s-p1`, `v2s-m1` (the epoch bias), `v2s-m2` (the
newcomer pool), `v2s-m3` (the election-day shock), `v2s-m4` (the δ walk), `v2s-p2` (the δ walk
held centred at the closed elections), `v2s-m5` (the fundamentals pseudo-observation), `v2`
(`harness/configs/v2.json`, `v2s-m5`'s config: every component on, both forms at 0, as neither
P-step's form was adopted) and its five ablations (`harness/configs/v2-*.json`, each `v2.json`
with one component switch at 0: `epoch_bias`, `newcomer_pool`, `shock`, `delta_walk` or
`fundamentals`; `tests/harness/test-arms.R` asserts each one's single differing field).
`v2-perm` is registered as its config lands.

**R0's extraction.** `r0_extract(draws, meta, dir)` (`R/harness/r0.R`) writes the four
parquets of spec §5.5 from a fit's draws: `eday_kjordaemi` (`.chain`, `.iteration`,
`.draw`, `kjordaemi`, `flokkur`, `votes`) from `y_pred_constituency[k, p]`;
`eday_national` (`.chain`, `.iteration`, `.draw`, `flokkur`, `share`) with
`y_rep_national[eday_index, p] / n_pred`; `latent_national`, the same columns with
`pi_eday[p]`; and `param_draws`, one column per element of `R0_PARAM_VARIABLES`. Kjördæmi
k and party p are `meta$kjordaemi_levels[k]` and `meta$party_order[p]`, read from the
indices in each element's name and never from a column's position, so a reordered draws
table cannot relabel a cell. It writes nothing unless all four tables build.

**V2's extraction.** `v2_variables(cfg)` (`R/harness/v2.R`) names what a V2 fit's draws must
hold: `votes_kd`, `pi_nat_eday`, `pi_latent_E`, `y_rep_res`, and the parameters and
transformed parameters of `V2_PARAM_VARIABLES` that the config's switches put in the model
(`V2_PARAM_SWITCH` names the switch of each that one adds; a parameter of size 0 has no
column in the CSVs). `v2_extract(draws, meta, dir)` writes the four parquets in R0's format,
`eday_kjordaemi` from `votes_kd[k, p]`, `eday_national` from `pi_nat_eday` (the
vote-weighted aggregate of `votes_kd`, so its gap to the scored kjördæmi aggregate is zero),
`latent_national` from `pi_latent_E` and `param_draws` from those parameters, plus
`ppc_results.parquet` (`.chain`, `.iteration`, `.draw`, `election`, `kjordaemi`, `flokkur`,
`votes`) from `y_rep_res[e, k, p]`, the in-sample replicates of each closed election's
kjördæmi counts (0 for a party with no national votes there), which only the past-election
checks read. The election label is the builder's `closed_elections[e]`, and every label
comes from the element names' indices, as in R0's extraction.

**The cell runner.** From the project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/fit_cell.R <arm> <E> <F> <seed> <iter_warmup> <iter_sampling>
```

`R/harness/fit_cell.R` is a plain script. It stops unless the locale is UTF-8 and the
working directory is the project root. Right after reading its arguments it stops on a 2017
cell while the freeze is not sealed (`forecast_free(E)`: "fit_cell: 2017 is not fitted before
the freeze (spec §5.7)"), for every arm, before it builds or writes anything. It builds the cell's data from `info_set(E, F)`
after `validate_inputs()`, and compares the expected meta (with `stan_data_sha()` of the
freshly built data) with the run's `meta.json`. On a match it prints
`SKIP <id> (meta.json matches)` and exits 0. A run directory that does not match, or has no
`meta.json`, is retired with `STALE <id> <mismatched keys>` (or `STALE <id> (no
meta.json)`) and refitted. It then freezes the inputs; `inputs.json` records the arm, E, F,
the expected keys, the builder's meta, the git HEAD (and whether the tree was dirty), the
sha256 of `snapshot_manifest.csv` and the CmdStan and cmdstanr versions. It fits with
`SAMPLER`, the seed and the iterations (`refresh = 50`), on the Stan executable alone (never
with the model methods compiled, which would build the model on the data inside R), and stops with no `meta.json`
unless the fit holds 4 chains of `iter_sampling` draws and the Stan file and the frozen
data still hash as expected. The extractor reads the arm's variables (for V2,
`v2_variables()` of the config). It then summarises the diagnostics over every
`parameters`-block variable present in the fit (a zero-size one has no draws) and the scored quantities, the national shares of
`kd_to_nat(eday_kjordaemi)` (`scored_nat[<flokkur>]`) and every kjördæmi cell's share
(`scored_kd[<kjordaemi>,<flokkur>]`), writes `key_diagnostics.csv` (`variable`, `rhat`,
`ess_bulk`, `ess_tail`) for those, the `param_draws` columns and `lp__`, and applies the gate.
The stop rule (Step checks, below) reads the `lp__` row; a run fitted before the row was
written has none, and `compare_runs.R` stops on it rather than guess.
`meta.json` comes last: the expected keys, `arm`, `role` (the arm's), `E`, `F`, `id`, the builder's meta
(`n_pred_k` and `n_e` as objects named by kjördæmi and party), `minutes` (the fit's wall
time, warmup and sampling), `diagnostics`, `gate` (`pass`, `reasons`) and the provenance of `inputs.json`.
A non-finite number in `meta.json` is written as a string (`"Inf"`, `"NA"`), so an infinite
R-hat stays distinct from a missing one; the gate fails on either. A NULL, such as V2's
`stage1_sha256` with no stage 1, is written as `null` in both `meta.json` and `inputs.json`,
and reads back as NULL. It prints the DIAG line
and `DONE <dir>`, then deletes the CmdStan CSVs, unless `HARNESS_KEEP_CMDSTAN` is `1`: the
slow V2 test keeps them to check `n_leapfrog` against their `n_leapfrog__` column.

**The queue.** From the project root:

```
zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>
```

A run list holds one run per line, `lane arm E F seed` separated by whitespace, with lane
A or B; lines starting with `#` are comments. `harness/runs/tier0.txt` is Tier 0's. The logs
directory is `${HARNESS_RESULTS:-results/harness}/logs`. Before any fit the queue takes the
lock `<logs>/queue.lock`, a directory holding its pid, and refuses to start while another
queue holds it, while a `fit_cell.R` runs (`pgrep -f -- '--file=R/harness/fit_cell.R'`), or
when `pgrep` fails and so cannot tell (an exit other than 0 or 1), unless `FORCE=1`; whatever
`FORCE`, it refuses to start while `HARNESS_V2_STAN` is set (Tests). It runs
the full slow suite as a preflight (`<logs>/preflight.log`) and stops if that fails, then
runs lanes A and B side by side, each lane's runs one after another, each run's output in
`<logs>/<id>.log`. `queue.log` there gets
`QUEUE START <time> <list> <warmup> <sampling> (pid <pid>)`, the preflight's verdict, a `start` and an
`end <id> rc=<rc> <first DIAG|SKIP|STALE line>` line per run, and one terminal line:
`QUEUE FINISHED` once both lanes are done, or `QUEUE ABORTED rc=<rc> <reason> (pid <pid>)` on
any other exit (a bad command line, a refusal, a failed preflight, SIGTERM or SIGINT). A
refused queue writes only its `QUEUE ABORTED` line. The log is appended to by every queue, so
a watcher reads only what follows the last `QUEUE START` and stops at `QUEUE FINISHED` or at a
`QUEUE ABORTED` carrying that start's pid (a refused second queue's line carries its own); and
a watcher whose own command line holds the guard's pattern trips the guard. On SIGTERM or
SIGINT the queue stops the preflight or both lanes, so no further run starts; a fit already
running finishes on its own (the guard's `pgrep` finds it). Every exit releases the lock but
SIGKILL's, which leaves the lock and no terminal line: remove `<logs>/queue.lock` once no queue
runs. `DRY_RUN=1` prints `would run <lane> <arm> <E> <F> <seed>` per run and does nothing
else: no lock, guard, preflight, log or fit. It exits 0 when every run exits 0, 1 when a run,
the lock, the guard or the preflight fails, 2 on a bad command line or run list, and 143 or
130 on SIGTERM or SIGINT.

## Tier 0

Tier 0 measures an arm's sampler on two cells before anything is scored (spec §5.8): the runs
of `harness/runs/tier0.txt` (R0 at the 2024 eve cell, seed 1101, in lane A, and at the 2021
f180 cell, seed 1102, in lane B), at 200 + 200 iterations. From the project root, in a normal
shell so that the queue outlives the session:

```
mkdir -p results/harness/logs
nohup zsh R/harness/queue.sh harness/runs/tier0.txt 200 200 > results/harness/logs/tier0_nohup.log 2>&1 &
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R r0 harness/runs/tier0.txt
```

Both runs are done once `results/harness/logs/queue.log` ends in `QUEUE FINISHED`; a
`QUEUE ABORTED` line with the queue's pid means it stopped first, and the line names why.
The last command, run once both runs are done, writes the report, `harness/tier0/<arm>.md`
(committed). `R/harness/tier0_report.R <arm> <run_list> [out]` is a plain script. For each run
of the arm in the run list it reads only the run's `meta.json`, and writes one table row:

- the cell (E, F, tag), the seed and the iterations;
- the minutes (the fit's wall time, warmup and sampling), divergences, treedepth hits (count,
  and percentage of the chains × sampling draws), the deepest tree and the min E-BFMI;
- the max R-hat and the min bulk and tail ESS, each as a pair: first over the gate's scope,
  every `parameters`-block variable and the scored quantities together (`max_rhat`,
  `min_ess_bulk`, `min_ess_tail`), then over the scored quantities alone (the `_scored`
  extremes). When the two are equal, the scored set is the binding one;
- both min bulk ESS per minute, and both scaled linearly to 2000 sampling iterations
  (× 2000 / `iter_sampling`). That projection informs the plan's Decision 5 (the centring),
  not the gate;
- the gate-scope min bulk ESS per 1,000 gradients, 1000 × `min_ess_bulk` ÷ `n_leapfrog` (the
  post-warmup leapfrog steps summed over chains, `diagnostics$n_leapfrog`), rounded down, or
  `–` for a `meta.json` without `n_leapfrog`, such as R0's Tier 0 runs, fitted before it was
  recorded. The Phase 2 plan's adoption rule (its Decision 6) compares it between two forms;
- the gate's verdict and reasons, which are informational: Tier 0 is not gated;
- the modification time of `meta.json`, the date of the measurement.

Above the table it records the run list, the CmdStan and cmdstanr versions and
`max_treedepth` of `meta.json`, and `sysctl -n hw.ncpu`, read when the report is written.
R-hat is rounded up and ESS and E-BFMI down, and a non-finite value prints as the run
recorded it (`Inf`, `NaN`, `NA`). The report holds sampler diagnostics only: no share, mean,
quantile or score of a forecast appears in it, and `tests/harness/test-tier0-report.R` checks
that none of the words share, mean, median, CRPS or MAE, nor the value of a forecast-like
decoy field in its fixture, does. It stops, writing nothing, on a run list with no run of the
arm, a run without `meta.json`, a `meta.json` of another run, a cell that is not a forecast
cell (naming the run), or a field it reads that is missing or does not parse, the gate's
among them: `pass` must be true or false and `reasons` a list of strings, possibly empty.

## Step checks

V2 is built as a staircase of steps (the Phase 2 plan's step protocol): an M-step adds a
component and so changes the posterior, and a P-step reparameterises the model and must not.
Each step's sampler-check fits are lines of `harness/runs/v2-steps.txt`, at 400 + 400 on 2024
eve (lane A) and 2021 f180 (lane B), seed 2701, and seed 2702 as well when the next step is a
P-step. Each step appends its lines to this record, and its queue runs a list of only those
lines, because a Stan edit makes the earlier steps' runs STALE: the runner would retire and
refit them. From the project root, in a normal shell so that the queue outlives the session:

```
mkdir -p results/harness/logs
grep -E '^[AB] <step> ' harness/runs/v2-steps.txt > results/harness/logs/<step>-runs.txt
nohup zsh R/harness/queue.sh results/harness/logs/<step>-runs.txt 400 400 > results/harness/logs/<step>_nohup.log 2>&1 &
LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R <step> harness/runs/v2-steps.txt harness/v2/steps/<step>.md
LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop <step>
```

The last two, run once `queue.log` ends in `QUEUE FINISHED`, write the step's diagnostics table
(committed) and print the stop rule's verdicts, which go into the step log,
`harness/v2/steps.md`, one section per step.

P1 (`v2s-p1`), the walk anchored at the closed elections, passed its four equivalence checks
and was not adopted: `adopt` printed `KEEP v2s-m0`, condition 3 failing on its 10 divergences
against `v2s-m0`'s none. The anchored form stays built behind `walk_form`, and every later
step's config sets `walk_form = 0`.

M1 (`v2s-m1`), the epoch bias (`epoch_bias = 1`), passed the stop rule: `GO` on both fits
(`harness/v2/steps.md`). Each step's config is the config of the last step whose form
stands (the previous M-step, or a P-step only when it is adopted) with the step's own switch
on. M1's is therefore `v2s-m0`'s with `epoch_bias = 1`, since P1 was kept, not adopted, and
every later step keeps `epoch_bias = 1`.

M2 (`v2s-m2`), the newcomer pool (`newcomer_pool = 1`), passed the stop rule as well: `GO` on
both fits (`harness/v2/steps.md`). Its config is `v2s-m1`'s with `newcomer_pool = 1`, as the
rule gives, and every later step keeps `newcomer_pool = 1`.

M3 (`v2s-m3`), the election-day shock (`shock = 1`), passed the stop rule too: `GO` on both
fits, with no divergence and no draw near a cap (`harness/v2/steps.md`). Its config is
`v2s-m2`'s with `shock = 1`, as the rule gives, and every later step keeps `shock = 1`.

M4 (`v2s-m4`), the δ walk (`delta_walk = 1`), passed the stop rule as well: `GO` on all four
fits, both seeds on each cell since P2 follows, with no divergence and one draw near a cap, 1
of 1,600 on 2021 f180 seed 2701 (`harness/v2/steps.md`). Its config is `v2s-m3`'s with
`delta_walk = 1`, as the rule gives, and every later step keeps `delta_walk = 1`.

P2 (`v2s-p2`), the δ walk held centred at the closed elections (`delta_form = 1`), passed its
four equivalence checks and was not adopted: `adopt` printed `KEEP v2s-m4`, condition 3
failing on its 8 divergences (all on 2021 f180) against `v2s-m4`'s none. The centred form
stays built behind `delta_form`, and every later step's config sets `delta_form = 0`; M5's is
therefore `v2s-m4`'s with its own switch on (`harness/v2/steps.md`).

A P-step's form is first checked against the reference form, point by point, by
`R/harness/v2_equivalence.R` (the plan's Decision 6, condition 1), once for each check of
`EQUIVALENCE_REQUIRED` (below), all into the step's equivalence CSV:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/v2_equivalence.R --config=harness/configs/steps/<step>.json --form=walk|delta --cell=<E>,<F> --out=harness/v2/steps/<step>-equivalence.csv [--n=30] [--n-off=15] [--stan=Stan/forecast_v2.stan] [--reverse]
```

The new side is the config as written and the reference side the same config with the form
at 0; a config whose form is 0 is refused. On a fitted cell the points are `--n` posterior and
`--n-off` off-posterior draws (each scale of `OFF_SCALES` multiplied by exp(N(0, 0.5))) of a
4 × (100 + 40) fit of the new side at seed 5100 and max_treedepth 10 that draws no forecast;
on a 2017 cell they are `--n` prior draws of the reference side (`v2_simulate()`, seeds 5101,
5102, …), nothing is fitted, and the check stops if either side's generated quantities hold
a forecast variable. Each point maps to the other side through the quantity the form
changes, θ for `walk_form` and δ for `delta_form`, every other parameter copied, and
(lp_new − J_new) − (lp_ref − J_ref) must be one constant within 1e-12 × max(1, |lp_new|), lp
being `log_prob(jacobian = TRUE)` and J each side's terms: its log-Jacobian from its standard
normals to θ or δ and, for `delta_form`, the reference's density of the K + P − 1 directions
of each closed election's ζ that M_e removes, which the centred form does not hold (the
script's header lists the terms). Under `--form=delta` a posterior point's ζ is the
minimum-norm preimage of its δ plus an N(0, I) draw in M_e's null space at seed 5200 + the
point's number, and a 2017 prior point's coordinates come from its δ. Each term left out
alone, a negative control, must break it; under `--form=walk` every control must be
exercised, so a term constant over the points fails the check as not exercised, while under
`--form=delta` such a term is tolerated. Of the delta form's terms only `new_delta_absent` can
be constant over the points: it is 0 on a cell where no closed election lacks a party. A cell
with one closed election (2017 eve) varies all three; what it leaves untested is the δ step
from the second closed election on (the walk's mean, an entrant's scale, which election's c
and w it reads). Both sides' generated quantities at the posterior (or prior) points, by
standalone `generate_quantities` at seed 1 and `sig_figs = 18`, must agree within 1e-9 relative, and θ
(or δ at every epoch) within 1e-8. A point
with a bounded log concentration at its cap is excluded and counted; one whose log density is
not finite on both sides is dropped and counted, and a dropped posterior point fails the
check, as does one finite on one side only; each kind needs at least 5 kept points; with
fewer of the main kind no generated quantities are compared and the check fails on that
count (`gq_points_compared()`, the one gate on the comparison). `check_reasons()` gives the
failed conditions and `control_verdicts()` the controls' verdicts. Besides its own stops (the
script's header lists them), the data can fail `v2_stan_data()`'s guards or the Stan file's
transformed-data rejects, which come back as "the fixed_param run failed; CmdStan said: ...".
It prints the coordinate party (`coordinate party: <label>`; it stops if the two
sides' differ), each point's constant and path error, the controls and the generated
quantities' largest relative difference, never a forecast value, then `EQUIV PASS` or
`EQUIV FAIL` (exit 1). `--out` must be a file in an existing directory. Once the command line
is accepted, the script first writes a provisional `FAIL` row for its cell and order (reasons
`incomplete`, the current `stan_sha256`), which its own row replaces at the end, so a check
that stops leaves no earlier verdict standing. `--form=both` stops: P1 kept `walk_form = 0`,
so no step config sets both forms. A check
of 10 + 5 points on 2021 f180 took 5 min 6 s (`time`, 29 Oct 2026); 2024 eve's grid has 274
dates to 2021 f180's 160.

`R/harness/compare_runs.R` is a plain script with three subcommands, whose definitions also
load as a box module (its test imports `perm_z()`, `perm_verdict()`, `relative()` and
`EQUIVALENCE_REQUIRED`). Each reads the runs of a run list, `harness/runs/v2-steps.txt` by
default, from `results_root()`: a run only once its `meta.json` exists and names it. A 2017
cell of the run list is refused before the freeze (`forecast_free()`), before any run is read.
Any other failure stops with `compare_runs: ...`, a path under the project root printed
relative to it and one under the results root as `$HARNESS_RESULTS/...`, never an absolute run
directory.

**`stop <arm> [run_list]`**, the M-step stop rule (the plan's Decision 5), prints one line per
run of the arm, in the run list's order:

```
GO|STOP <id> lp_rhat=<x> divergent=<n>/<N> near_cap=<n>/<N> max_rhat=<x>; recorded: minutes=<x> treedepth_hits=<n> ess_bulk_per_1k_grad=<x>[; stop: <reason>; ...]
```

with N = chains × `iter_sampling`. A run STOPs on any of these signals, and on one that is
NA or absent:

| Signal | Read from | STOP when |
|---|---|---|
| `lp_rhat`, the R-hat of `lp__` (a stuck chain) | `key_diagnostics.csv`'s `lp__` row | above 1.1 |
| `divergent`, the divergent transitions | `meta.json`'s diagnostics | more than 1% of N (more than 16 of 1,600) |
| `near_cap`, the draws in which a bounded log concentration is within 0.5 of its cap | `param_draws.parquet`'s `log_phi` (one column per house), `log_phi_K` and `log_kappa`, against `V2_LOG_CAPS` (`R/harness/v2_reference.R`: 20, 20 and 2) | more than 1% of N; a draw counts once however many are near |
| `max_rhat`, the gate-scope max R-hat | `meta.json`'s diagnostics | above 1.10 |

R-hat is printed rounded up to four decimals, so a value printed at or below its limit is at
or below it. The minutes, the treedepth hits and the gate-scope min bulk ESS per 1,000
gradients (1000 × `min_ess_bulk` ÷ `n_leapfrog`, NA without `n_leapfrog`) are recorded, never a
stop. It exits 1 on any STOP.

**`adopt <ref_arm> <new_arm> <equivalence_csv> [run_list]`**, the P-step adoption rule (the
plan's Decision 6), compares a P-step's form (`new_arm`) with the reference form (`ref_arm`,
the step before it). The two arms must differ and have runs on the same cells, and on each
cell each arm has exactly two fits, of two different seeds that are the other arm's (the plan
runs seeds 2701 and 2702, but any two seeds the arms share pass); this is checked from the run
list before anything is read. Every `meta.json`
must hold `n_leapfrog`. It prints a table of the fits (min bulk ESS per 1,000 gradients,
divergences, the stop rule's verdict), a table of the cells (each arm's median over its fits
of min bulk ESS per 1,000 gradients, and their ratio), the four conditions with yes or no, and
a last line, `ADOPT <new_arm>` when all four hold and otherwise
`KEEP <ref_arm>: <new_arm> fails condition(s) <i>, ...`:

1. the equivalence check passes: the equivalence CSV has no `FAIL` row, and a `PASS` row for
   each check of `EQUIVALENCE_REQUIRED`, the four that P1's check writes (the plan's Task 8):
   2024 eve, 2021 f180 and 2017 eve (prior points only) in the default order, and 2024 eve in
   the reversed order. A 2017 row is only a verdict, so the 2017 refusal, which is about the
   runs read, does not apply to it;
2. on each cell, `new_arm`'s median is at least 1.25 × `ref_arm`'s;
3. `new_arm`'s divergences, summed over its fits, are at most `ref_arm`'s plus 4;
4. no fit of either arm trips the stop rule.

Both forms have the same posterior, so no forecast quantity is read. It exits 0 on `ADOPT` and
on `KEEP`. The equivalence CSV (`harness/v2/steps/<step>-equivalence.csv`) has one row per
check `R/harness/v2_equivalence.R` ran, with at least these columns; any others are ignored:

| Column | Value |
|---|---|
| `E`, `F` | the cell, ISO dates |
| `order` | `default` or `reversed`: the party and kjördæmi order of both sides (`--reverse`) |
| `verdict` | `PASS` or `FAIL`: the check's `EQUIV` verdict, its negative controls included; a `FAIL` with reasons `incomplete` is the provisional row of a check that has not finished |

`v2_equivalence.R` writes these first, then `form`, `config`, `stan_sha256`, the kept points
by kind (`n_posterior`, `n_off`, `n_prior`), the dropped, excluded and one-sided counts
(`n_rejected`, `n_outside`, `n_one_sided`), `const_range`, `tol`, `path_err`, `gq_points`
(0 when no generated quantities were compared, `gq_max_rel` then empty), `gq_max_rel`, one
`control_<term>` per log-Jacobian term (`fails`, `PASSES` or `not exercised`) and `reasons`,
the failed conditions; reasons `incomplete` mark the provisional `FAIL` row of a check that
has not finished, one that stopped or is still running.

**`perm <arm_a> <arm_b> [run_list]`**, the permutation rule (the plan's Decision 8), compares two
different arms that must give the same forecast draws within Monte Carlo error, such as `v2` and
`v2-perm`. Their runs are paired by cell, in the run list's order, and each arm must have as
many runs on each cell. Of each pair it reads only `eday_national.parquet` and
`eday_kjordaemi.parquet`, and matches by name every national share (`eday_national[<flokkur>]`)
and every kjördæmi share, a cell's votes over its kjördæmi's votes in the draw
(`eday_kjordaemi[<kjordaemi>,<flokkur>]`); the two runs must have the same names. For the mean
and the 5%, 50% and 95% quantiles of each, z = (a − b) / √(MCSE_a² + MCSE_b²), with posterior's
`mcse_mean()` and `mcse_quantile()` (0 when a = b; `perm_z()`). A pair passes when at most
3 |z| exceed 3, none exceeds 5 and the sd of the mean z's is at most 1.5 (`perm_verdict()`).
A missing z, from a missing draw or an undefined MCSE, exceeds both: it is counted above 3
and makes the max |z| Inf. It prints only those statistics, rounded up to two decimals, and
the names of the variables whose |z| exceeds 3, never a share:

```
PERM PASS|FAIL <id_a> vs <id_b>: <n> z's, <k> with |z| > 3, max |z| <x>, sd of the mean z's <x>
  |z| > 3: <variable> <mean|q05|q50|q95>, ...
```

It exits 1 on any `PERM FAIL`. `tests/harness/test-compare-runs.R` runs all three on synthetic
run directories, made-up draws only, and checks that every line `perm` prints is a verdict or
a list of names. It also tests `perm_z()` against a hand computation with posterior's MCSEs,
and `perm_verdict()` on made-up z's that break each of its three conditions alone.

## R0's inputs

`R/harness/r0.R` builds the Stan data of arm R0, production mapped to E (spec §5.4).
`r0_production_inputs(info)` puts a cell's `info_set()` into the format production's fit
script hands to `prepare_stan_data()` (`R/stan_data.R`): the `Kosning` rows and admitted
national polls; the previous election's kjördæmi results and the admitted kjördæmi polls
dated after it; the training rows, E's target rows and an Annað target row. The party order
is Annað, then the cell's parties in `POLL_PARTIES` order; the houses are `Kosning`, then
each house by its first admitted poll; the kjördæmi follow `R0_KJORDAEMI`, production's 2024
order, so Suður closes δ in every cell. No house is filtered out.

`r0_stan_data(info)` runs production's `prepare_stan_data()` on those inputs and then applies
a fixed table of overrides, and only those (Task 8 of
`docs/superpowers/plans/2026-10-24-v2-harness-phase1-scorer-r0.md`): the campaign length;
`n_pred_k` and the prediction weights from the previous election's valid votes, by name; the
kjördæmi weights of each date, by name; the fundamentals weight and its day; the hedge scale
`mu_gamma_scale` (1/√n_e, 0 for a party new to the window); and the fundamentals prediction
column, rebuilt for the cell's party order, with a newcomer at the median debut share of earlier new parties. The
settings are in `harness/configs/r0.json` (`read_r0_config()`). A build whose data break a
shape the Stan file relies on stops with "R0 input: <check>". So does a newcomer, a party
with no fundamentals share at the previous election, that had votes there in the official
results, as a fundamentals label missing from `FUNDAMENTALS_RENAMED` would make one. `stjornarslit` and
`post_stjornarslit` keep production's hard-coded 2024-10-14; the model declares them but
never reads them.

R0's Stan file, `Stan/polling_and_fundamentals_kjordaemi_r0.stan`, is production's model as
of commit aa2ac1a: the centred model of the plan's Decision 5 (ruled on 2026-10-27),
reparameterised (anchored walk, δ and log σ partially centred, log φ bounded above at 20;
`Stan/CLAUDE.md`, `R/sampler_check/RESULTS.md`), plus two hooks: H1, the hedge scale as data, and H2, the E-day shares `pi_eday` and `pi_kd`.
It has that model's log density (`Stan/CLAUDE.md`). A third hook, H3, gathered the
replications by party identity; production took it over in 5f09c5b, with the active parties
(`n_active_d`, `active_index_d`) that `prepare_stan_data()` now computes.
`tests/harness/test-r0-stan.R` reads that parent with `git show` at `R0_PARENT_COMMIT`
(aa2ac1a's full sha), never from the working tree, into `results/harness/stan_parent/`
(gitignored, rewritten only when its bytes change), and compares R0 with it: `transformed
data` to `model` byte for byte, the data block and generated quantities outside the two
hooks with comments and whitespace stripped (H2 may hold no RNG call), and the log density
and its gradient. `tests/harness/test-r0-inputs.R` likewise holds R0's data to that
commit's `prepare_stan_data()`: its fields in its order, with its values but for the
overrides above.

## Rebuilding the generated files

From the project root: `LC_ALL=en_US.UTF-8 Rscript R/harness/build_phase0.R`. It verifies
the snapshot, then rewrites, each through its own writer,
`harness/sources/seats_2021_official.csv` (from the committed Alþingi page),
`harness/truth/elections_official.csv`, `harness/availability.csv`,
`harness/availability_report.md` and `harness/entry_table.csv`.
It prints the polls per availability rule and each forecast cell's admitted polls
(national and kjördæmi). The output is deterministic, so on unchanged inputs
`git status --porcelain harness/` stays empty. It does not rebuild the Wikipedia fieldwork
tables, whose parser needs page revisions fetched from the web (`harness/sources/README.md`),
or `snapshot_manifest.csv`.

## Committed versus generated

- Committed: this README, `snapshot_manifest.csv`, `selftest_manifest.csv`, the generated
  tables that `build_phase0.R` writes, and `harness/sources/`, which holds source files
  fetched once or entered by hand, with their provenance in a README: `harness/truth/README.md` for the
  results sources, `harness/sources/README.md` for the poll-fieldwork tables and the house
  map, the fieldwork corrections, the publication dates, the two correction tables
  (`poll_n_corrections.csv`, `fundamentals_corrections.csv`), the economy release dates,
  the house table and the coalition events. `harness/configs/` holds each arm's settings
  (`r0.json`, V2's candidate `v2.json` and its ablations `v2-*.json`, and V2's step configs in
  `steps/`), `harness/runs/` the queue's run lists (`tier0.txt`, `v2-steps.txt`), and
  `harness/tier0/` the Tier 0 reports, which `tier0_report.R` writes from local runs (no offline
  rebuild). `harness/v2/steps.md` is V2's step log and `harness/v2/steps/` each step's
  diagnostics table and equivalence CSV, from local runs too (Step checks).
  `harness/v2/prior_predictive.md` is V2's prior predictive check, from prior draws on the
  snapshot with no fit, which
  `LC_ALL=en_US.UTF-8 Rscript R/harness/v2_prior_predictive.R harness/configs/steps/<step>.json`
  rewrites deterministically.
- Generated and gitignored: `harness/snapshot/` and `harness/selftest/`.

## Target rows: the economy and the carried columns

`fundamentals_inputs()` (`R/harness/fundamentals.R`) builds E's target rows. Their `vnv`
and `growth` are the snapshot's `economy_data.csv` row at E, as `R/prepare_economy_data.R`
writes it: `vnv` is the 12-month change of the CPI (Hagstofa VIS01002) at July of E − 1,
and `growth` is the volume change of GDP per capita on the previous year (THJ01000, "Verg
landsframleiðsla á mann, magnbreyting frá fyrra ári") for E − 1. `info_set()` admits only
an F on or after the day Hagstofa first published both figures, from
`harness/sources/economy_release.csv` (sources and the checks: `harness/sources/README.md`).
The data was copied into the snapshot on 23 Oct 2026, so the values are that vintage, not
the first releases: growth for 2016 was first published as 5.5 and is 4.7 in the
snapshot, and growth for 2020 as −8.2 against −7.8. That is the revision leak spec §5.3
rule 5 accepts: the release dates bound when a figure was first public, not which value
it had then.

The carried columns, `year`, `new_party`, `party_split`, `voteshare_prior` and
`incumbent_years`, come from the party's row at E: known before E, not necessarily at F.
Framsókn's `party_split` = 1 in 2017 records the split of September 2017 (Miðflokkurinn
was founded on 24 September; English Wikipedia, "Centre Party (Iceland)", retrieved
24 Oct 2026). Both 2017 cells' F postdate it, but an F before it would see a split not
yet made. It is latent: the spec's stage-1 regression (§6.5) and the production model do
not use `party_split`.

## Notes for the freeze review

Brynjólfur decides on these before Phase 3. The Brier event of spec §5.6 is a majority for
the coalition in office at F: `coalition_event()` in `R/harness/tables.R`, from
`harness/sources/coalition_events.csv`.

- **2017-10-14 and 2017-10-27: the event is D + C, with A in Annað.** Björt framtíð left
  the coalition on 15 September 2017. The Prime Minister resigned on 16 September, and the
  cabinet sat on as a caretaker government, with Björt framtíð's ministers in office until
  30 November. So the composition in office at both dates is D + C + A, and the literal
  §5.6 event, with A in Annað, is D + C. The incumbents (`incumbents_at()`) are also
  D + C.
- **2024-11-16 and 2024-11-29: the event is D + B.** Vinstri græn left the government on
  17 October 2024, and Sjálfstæðisflokkurinn and Framsóknarflokkurinn sat as a caretaker
  government until 21 December. So the literal §5.6 rule gives D + B at both dates, while
  D + B + V, the cabinet's opening composition, still counts as the incumbents
  (`incumbents_at()`, as `fundamentals_data.csv` codes them).
- **2024-10-14: the event is D + B + V.** F is one day after Bjarni Benediktsson announced
  the end of the coalition on 13 October 2024. The government had not yet resigned (the
  President accepted the resignation on 15 October), and Vinstri græn's ministers held
  office until 17 October. So the composition in office, and the literal §5.6 event, is
  D + B + V. Spec §5.2 asks whether the 30 November election date had been announced by
  F. It had been announced, not yet fixed. At his press conference of 13 October the
  Prime Minister said elections would be held on 30 November, pending the President's
  consent to dissolve the Alþingi ([Reykjavík Grapevine, 13 Oct
  2024](https://grapevine.is/?p=10079846)). The President's statement of 14 October
  records his proposal of a dissolution and elections "í lok nóvember" and puts off her
  decision to later in the week ([Wayback capture](https://web.archive.org/web/20250805091546/https://www.forseti.is/media/14866/20241014-yfirl%C3%BDsing-forseta-vegna-tilloegu-um-%C3%BEingrof.pdf)).
  Her statement of 15 October accepts it: the dissolution would be announced at the
  Alþingi's sitting of 17 October, and the election held on 30 November ([Wayback
  capture](https://web.archive.org/web/20250802184631/https://www.forseti.is/media/14843/yfirl%C3%BDsing-forseta-vegna-%C3%BEingrofs-og-lausnarbei%C3%B0nar.pdf)).
  The two statements' forseti.is addresses returned 404 on 24 Oct 2026, when these were
  read, and althingi.is refuses scripted requests.
- **`fundamentals_data.csv` codes incumbency by three conventions.** `incumbent` and
  `incumbent_prime_minister` follow the cabinet formed after the previous election (the
  rule `incumbent` follows from 2003 on, `harness/sources/README.md`; before 2003, 1991 is
  an exception), the cabinet in office on election day, or, in 1983, the parties rather
  than the prime minister. The first two readings differ in four years, and 1983 follows
  neither:
  - The cabinet formed after the previous election, in 1979 and 2009. In 1979 the
    incumbents are Framsókn, Alþýðuflokkur and Alþýðubandalag and the prime minister is
    Framsókn's: Ólafur Jóhannesson's cabinet, appointed on 1 September 1978. Benedikt
    Gröndal (Alþýðuflokkur) was prime minister on election day; his term began on
    15 October 1979. In 2009 the prime minister is D's, of the coalition formed after the
    2007 election, although S held the premiership on election day.
  - The cabinet in office on election day, in 1991 for both columns and in 2024 for the
    prime minister only. In 1991 the incumbents are Framsókn, Alþýðuflokkur,
    Alþýðubandalag and Borgaraflokkur and the prime minister is Framsókn's: Steingrímur
    Hermannsson, prime minister from 28 September 1988. Þorsteinn Pálsson (D) headed the
    coalition formed after the 1987 election. In 2024 the prime minister is D's, as from
    9 April 2024, not V's, whose leader headed the coalition formed after the 2021
    election; the incumbents (B, D and V) are that coalition's.
  - The parties, not the prime minister, in 1983. Gunnar Thoroddsen, a D member, was
    prime minister from 8 February 1980 to 26 May 1983, so both readings give his
    cabinet. He formed it with Framsókn and Alþýðubandalag against his party's will
    (English Wikipedia, "Gunnar Thoroddsen", retrieved 24 Oct 2026). The data codes
    Framsókn and Alþýðubandalag as incumbents, D as not incumbent, and no party as the
    prime minister's, which neither reading gives.

  In every other year from 1963 to 2021 both readings give the same prime minister's
  party: D in 1963, 1967 and 1971, B in 1974, D in 1978, B in 1987, D from 1995 to 2007,
  S in 2013, B in 2016, D in 2017 and V in 2021. So 1979, 1991 and 2009 are the training
  rows on which the first two readings differ, and 1983 is the one that follows neither;
  2024 is a target row only. The harness codes the target row's prime minister as at F
  (`pm_at()`), which in every cell equals the data's coding at E.
  Prime ministers' terms: Icelandic Wikipedia, "Listi yfir forsætisráðherra Íslands"
  (retrieved 24 Oct 2026).
