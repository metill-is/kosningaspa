# v2 backtest harness

The harness fits forecast models on past election cells and scores them against the results.
It builds frozen, leak-checked inputs (official results, poll availability, fundamentals target
rows and seat layouts) for the cells of the 2016, 2021 and 2024 elections and the held-out 2017
cells, fits an arm on a cell, and scores two arms against each other. Code is in `R/harness/`
(box modules and plain scripts), tests in `tests/harness/`, committed inputs and reports here.

## The snapshot

The harness never reads the live `data/`. `snapshot_inputs(src_root)` (`R/harness/inputs.R`)
copies the files in `SNAPSHOT_FILES` from the main checkout into `harness/snapshot/`
(gitignored) and writes `harness/snapshot_manifest.csv` (committed). Every read goes through
`snapshot_file(name)`, which checks the file's sha256 against the manifest; `verify_snapshot()`
checks them all.

To refresh it, delete `harness/snapshot/` by hand (`snapshot_inputs()` refuses an existing
one), re-run it from the project root, and commit the new manifest:

```r
options(box.path = normalizePath("."))
box::use(R/harness/inputs[snapshot_inputs])
snapshot_inputs("<path to the main checkout>")
```

Then rebuild the generated tables, which reads the snapshot and rewrites
`harness/truth/elections_official.csv`, `harness/availability.csv`,
`harness/availability_report.md`, `harness/entry_table.csv` and
`harness/sources/seats_2021_official.csv`; on unchanged inputs `git status harness/` stays
clean:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/build_phase0.R
```

## Tests

From the project root, `LC_ALL=en_US.UTF-8 Rscript tests/run.R [filter]` runs the suite under
testthat edition 3, and a passing run prints each test file's seconds, slowest first.

- **The fast suite** is that command with no switches. Tests that need the snapshot call
  `skip_without_snapshot()`. `tests/harness/test-generated.R` fails if a generated committed
  file is not a fresh build, and `tests/harness/test-escapes.R` enforces that harness R code
  writes non-ASCII characters as `\uXXXX` escapes.
- **The slow suite** adds the tests that call `skip_unless_slow()`: the sweep of every forecast
  day, the self-test's kjördæmi and seat scores, the R0 and V2 Stan tests and the cell
  runner's test fits, which compile with CmdStan:
  `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R`. `HARNESS_V2_STAN=<absolute path>`
  runs the V2 Stan tests (`tests/harness/test-v2-stan.R`) on a scratch copy of the Stan file.
- **The live-data test** reads `data/polling_data.csv` and runs only with
  `KOSNINGASPA_LIVE_DATA=1`: `KOSNINGASPA_LIVE_DATA=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R election-data`.

**The lean check for a model change** (the owner's policy of 5 November 2026: a change gets the
checks its kind needs, and the wider suite waits for a published or frozen forecast):

- *Any change:* the fast suite, then the V2 Stan tests (Stan's log density against the R
  reference, the permutation lp test, the generated quantities):

  ```
  LC_ALL=en_US.UTF-8 Rscript tests/run.R
  HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R v2-stan
  ```

- *A change to what the model computes:* add one short fit on the cell the change matters for,
  through the queue on a run list of one line, then its diagnostics and the stop rule, written
  to a report of its own so that `harness/tier0/` keeps its records:

  ```
  zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>
  LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R v2 <run_list> <out>
  LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop v2 <run_list>
  ```

- *A change meant to improve forecasts:* fit the old and the new model once each on 2024 eve
  and 2021 f180 and score them (Scoring a comparison). The working model's run list is
  `harness/runs/v2.txt`.
- *Before a published forecast or the freeze:* the full slow suite, two seeds per arm and cell,
  and a recovery study, which has to be rebuilt for this model first (`R/harness/archive/README.md`).

## Running fits

**One cell.** From the project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/fit_cell.R <arm> <E> <F> <seed> <iter_warmup> <iter_sampling>
```

`R/harness/fit_cell.R` fits one arm of `R/harness/arms.R` on one cell, in
`results_root()/<arm>_<E>_<F>_s<seed>` (`results/harness/`, gitignored, or `HARNESS_RESULTS`).
It stops on a 2017 cell before the freeze, before it builds or writes anything. It builds the
cell's Stan data from `info_set(E, F)` and computes the expected meta: the sha256 of the Stan
file, of the freshly built data and of the config as its reader returns it, the seed, the
iterations and the sampler settings. A finished run that matches prints
`SKIP <id> (meta.json matches)`; one that does not, or has no `meta.json`, is moved to `_stale/`
with a `STALE` line and refitted. It freezes the inputs read-only in `inputs/`, fits (with the
config's `adapt_delta` when it sets one), checks the chains and hashes, extracts the parquets,
summarises the diagnostics, writes `key_diagnostics.csv` and applies the gate. `meta.json` comes
last, so its presence means the run is complete; then it prints the DIAG line and
`DONE <dir>` and deletes the CmdStan CSVs unless `HARNESS_KEEP_CMDSTAN=1`. The diagnostics
thresholds are for the long fits behind a published forecast: a short development fit that
misses them is flagged (`flags: <reasons>`), not failed, and the DIAG line still ends in
`gate=PASS|FAIL`.

**A run list.** Run the fast suite by hand first, then, from anywhere:

```
zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>
```

A run list (`harness/runs/`) holds one run per line, `lane arm E F seed`, the lane A or B;
lines starting with `#` are comments. `R/harness/queue.sh` runs each lane's runs one after
another and the two lanes side by side, each through `fit_cell.R` with its output in
`<logs>/<id>.log`, where `<logs>` is `${HARNESS_RESULTS:-results/harness}/logs`.
`<logs>/queue.log` gets a `QUEUE START` line, a `start` and an
`end <id> rc=<rc> <first DIAG|SKIP|STALE line>` per run, and one terminal line: `QUEUE FINISHED`,
or `QUEUE ABORTED rc=<rc> <reason> (pid <pid>)` on a bad command line, SIGTERM or SIGINT. On a
signal it stops both lanes; a fit already running finishes on its own. `DRY_RUN=1` prints
`would run <lane> <arm> <E> <F> <seed>` per run and does nothing else. Exit status: 0 when every
run exits 0, 1 when a run fails, 2 on a bad command line or run list, 143 or 130 on a signal.
Start a long queue with `nohup` from a normal shell so that it outlives the session.

## Scoring a comparison

`score_cell(run_dir)` (`R/harness/scorer.R`) scores one finished run against the result. To
compare two arms, fit both in this checkout on the same cells, one run each, and from the
project root:

```
LC_ALL=en_US.UTF-8 Rscript R/harness/score_report.R <ref_arm> <new_arm> <run_list> [out]
```

`R/harness/score_report.R` writes `harness/v2/scoring.md` by default and prints no score. It
opens with the election-level reading, the one to rank two arms by: CRPS10's difference on
each cell beside its outcome noise (`outcome_noise()` in `R/harness/scores.R`, the spread of
that difference over the results each forecast itself holds possible), averaged within each
election and then over the elections with equal weight, and read as indistinguishable when
the mean is under twice its noise (`election_reading()`). Then come the MAEs and, per cell, the
spec's adoption rule with the tie rule. The tie rule's SE_diff is Monte Carlo error alone, so a
per-cell "better" or "worse" says that a difference is computed precisely, not that one model
forecasts better.

A party with votes at E but no admitted poll at F, so that the cell has none of it (Flokkur
Fólksins at 2016-05-02), is scored as part of Annað on that cell: `fold_parties()` names it,
reading the snapshot, and `cell_truth(..., fold =)` adds its votes and seats to Annað's. A run
that leaves out a party the cell does have is still refused. Where Annað holds official seats
(2016: Björt framtíð's 4), no draw can give it them, so the report marks that cell's seat
scores "(not comparable)" and leaves them out of criterion 4; the election-level reading is
CRPS10 alone and does not change.

To compare two commits rather than two arms, fit the old model in a worktree of its own with a
copy of `harness/snapshot/` and its own `HARNESS_RESULTS`, passing the run list by absolute
path: two fits of the same arm, cell and seed in one results root would retire each other to
`_stale/`.

Every arm fitted on a scored cell gets a row in `harness/trials.csv`: the arm, the day it was
first fitted, its config, the cells it has draws for, how it was conceived (`planned` before
any forecast was scored, `after-scores`, or `sampler` for another form of the same posterior)
and what it changes. The number of rows is the number of comparisons behind any choice, and an
`after-scores` arm is exploratory on 2021 and 2024 until another election confirms it.

## The freeze and the 2017 locks

`score_cell(run_dir)` (`R/harness/scorer.R`) is the only function that scores a forecast
cell against its result (spec §5.6–§5.7). It reads the run's `meta.json`, calls
`freeze_guard(E, F)` (`R/harness/freeze.R`), refuses a 2017 cell, and only then reads the
result (`cell_truth()`) and the draws. With the default result reader, which reads
this project's results, it refuses a `root` other than the project's, since the guard
would otherwise check another repository's freeze.

The guard refuses a cell that is not in `TARGETS` (check 1). A 2016, 2021 or 2024 cell passes
it with no freeze: Brynjólfur decided on 5 November 2026 that the model is developed without
such locks, so no freeze file is read for those cells. A 2017 cell, the held-out confirmatory
target, stops at the first of these failures:

1. (E, F) is not a cell of `TARGETS`;
2. `harness/FREEZE.md` or `harness/FREEZE.sha256` does not exist;
3. `FREEZE.sha256` does not list `harness/FREEZE.md`;
4. a file it lists is missing or its sha256 differs (`verify_sha_list()`);
5. the two files are not committed, that is tracked by git and unchanged against HEAD
   (`is_committed()`, which compares each file's `git hash-object` with its blob in HEAD,
   so an edit the index is told to ignore, by `--assume-unchanged` or `--skip-worktree`,
   still counts).

A 2017 cell is also locked until `harness/DECISION.md` and
`harness/DECISION.sha256` pass checks 2–5 (the refusal names the check that
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
- `per_party` (with the PIT's batch SE, and each party's result and the absolute error of
  its median, the terms of the MAEs), `kjordaemi` (`kd_scores()`, with the batch SEs
  `se_kd_crps` and `se_kd_cov90`), and `seats` and `seats_per_party`: the seats of each
  draw allocated with Annað ineligible, scored for the Brier coalition recorded in
  `meta.json`, with the majority probability of the coalition that formed after E
  (`formed_coalition()`, hindsight that only `score_cell()` reads). `seats` carries the
  batch SEs `se_seat_crps`, `se_seat_cov90` and `se_brier`.
- `seats_not_comparable`, only for a cell whose result gives Annað official seats (2016):
  why its seat scores, Brier score and `p_formed`, returned as usual, are not comparable.

Every batch SE is over the same 16 blocks: the score is recomputed on each block's draws,
and its SE is their standard deviation over √16. So the tie rule of spec §5.7 (a
difference under 2 × SE_diff) can be applied to every scored quantity, and coverage comes
with its Monte Carlo error.

Past the guard and the 2017 refusal, it recomputes the Brier event,
`coalition_event(F, read_coalitions())`, and stops with "coalition event changed since the
fit: refit <run>" unless its parties are those `meta.json` recorded: the sha-skip keys do
not cover the event, so a reused run could otherwise be scored against an outdated one. It
stops with "no list at E" if a party the run forecasts has a national result of 0, as a
party with no list would, since the alr scores would take the log of that zero. With this
repository unfrozen, it reads the result for every 2016, 2021 and 2024 cell and refuses every
2017 one before any result is read; `tests/harness/test-scorer.R` checks both with a result
reader that fails if it is called, and scores a 2021 cell with no freeze. The tests of a
sealed freeze and decision run in temporary git repositories.

## Where the record is

- `harness/v2/decisions.md`: each V2 step's verdict and commit, and the working model. The
  switched file and its harness are at the tag `v2-switched-final`.
- `harness/archive/`: the frozen V2 step log (`steps.md`), this page as it stood before
  10 November 2026 (`README-2026-11-10.md`), the step and ablation configs (`configs/`), the
  old run lists (`runs/`) and the nine-switch model's reports (`v2/`: scoring, recovery and
  each step's diagnostics; `tier0/`: its Tier 0 sampler reports).
- `R/harness/archive/`: retired one-off studies and the recovery study, with their own README.
- Generated reports: `harness/v2/` (scoring, the simple core's log-density equivalence, and
  the prior predictive check, `R/harness/v2_prior_predictive.R harness/configs/v2.json`) and
  `harness/tier0/` (R0's Tier 0 sampler report).
