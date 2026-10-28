# v2 Harness Phase 1 (Scorer and R0) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the harness a scorer, a freeze guard and its reference arm. Phase 1 ends when three things hold. The scorer reproduces the overdisp study's `base_eve` scores from that study's saved draws (CRPS10 0,709; d² 36,5). The scorer refuses a target cell without a freeze. R0's Tier 0 diagnostics are committed.

**Architecture:**
- The pure score functions in `R/harness/scores.R` take draw matrices and true values. They know nothing about runs.
- `score_cell()` (`R/harness/scorer.R`) is the only code that puts an arm's draws next to a result. It calls `freeze_guard()` (`R/harness/freeze.R`) before it reads any result.
- An arm is an entry in `R/harness/arms.R`. It has a builder from `info_set()` to frozen Stan data, a Stan file, and an extractor that writes the §5.5 parquets.
- `R/harness/fit_cell.R` fits one cell headless. It uses the run records in `R/harness/runs.R`: ids, frozen inputs, `meta.json`, sha-skip, the DIAG line and the §5.7 gate. `R/harness/queue.sh` runs a run list on two lanes.
- R0's builder is production's `prepare_stan_data()` run on the cell's admitted inputs, plus a fixed list of overrides. R0's Stan file is production's with three hooks.
  - Since Decision 5 (ruled 2026-10-27), the parent is production at 884560f (centred), and since b57a015 at aa2ac1a (anchored walk, partially centred effects); R0 keeps two hooks, H1 and H2; H3 is the parent's own. Where the tasks below say 5d22031, three hooks, H3 or R0's own active parties, they describe the plan as executed before that ruling.

**Tech Stack:** R box modules, testthat 3, cmdstanr 0.9 (CmdStan 2.38; model methods for `log_prob`), posterior 1.6, arrow, jsonlite, digest, withr, zsh.

**Spec:** the design note in the Metill vault, `/Users/brynjolfurjonsson/Obsidian/Metill/Kosningar/Forecast model/2026-10-23 — v2 forecast model and two-election harness (design).md`. The relevant sections are:
- §5.4: the R0 row and the R0 config;
- §5.5: the adapter contract;
- §5.6: scores, MC error and the self-test;
- §5.7: the freeze, the 2017 locks and the diagnostics gate;
- §5.8: runs, Tier 0 and the reused queue;
- §5.9: where everything lives;
- §8: the Phase 1 row.

Read §5.4–§5.7 before Task 3.

**Scope:**
- Phase 1, plus two items carried over from Phase 0: the R54 escapes (Task 1) and the daily-F sweep (Task 2).
- V2 is Phase 2. The freeze itself is Phase 3. All target scoring is Phases 4–5. Each gets its own plan (see *Later plans*).

**Ported code.** Several tasks port code from the two earlier studies, which live in git-excluded folders of the main checkout. Spec §5.4–§5.8 name those folders. This plan, being committed, calls them *the 2021 study* and *the overdisp study* and gives file paths relative to each study's folder (`code/R/prep.R`).

## Decisions for Brynjólfur

Each item below is a choice the spec leaves open, or one where the data contradicts the spec's wording. The plan runs on the defaults given, and a task can change each one cheaply before the freeze (Phase 3).

**Ruled by Brynjólfur on 2026-10-24:** the defaults of Decisions 1–4 stand; Decision 5 as recorded below.

1. **R0's fundamentals prior for a newcomer** (Sósíalistaflokkurinn in 2021, Miðflokkurinn in 2017). Production's model needs a prior for every party, and the spec doesn't say what a newcomer's should be.
   - Default (Task 8): the median debut share of the fundamentals rows with `new_party == 1` dated before E.
   - That gives 7,2 % at 2021, the 2021 study's 0,072, and 7,15 % at 2017 (via the training rows of `info_set()`, 2026-10-24).
2. **The national `Kosning` rows R0 reads carry recycled valid-vote totals.**
   - `R/data.R:253` writes `n = p * c(199730, 196259, 189648)`, which recycles three totals row by row across the election rows, not one total per election.
   - The spec says R0 reads these rows "as production reads them", so the default keeps them.
   - The alternative is to fix them from the official totals, as §5.4 already fixes the RS/RN swap in expected valid votes.
   - They weight the election rows in the likelihood and set `n_pred`. `n_pred` scales `eday_national` only; the scored national forecast comes from the kjördæmi draws.
3. **Which quantities the R-hat ≤ 1.01 gate checks** (§5.7).
   - Default (Task 7): every variable in the `parameters` block, plus the scored quantities. The overdisp study's `max_rhat` used that scope.
   - Under it, that study's `base_eve` fit has max R-hat 1,0129 at 4 × (1000 + 2000) and `max_treedepth` 10, so it would fail the gate.
   - The alternative is the scored quantities only.
   - This interacts with Decision 5: R0's slowest quantities are hierarchical scales, which only the default scope gates.
4. **The R54 escapes also cover `R/election_utils.R`.** The brief names `R/harness/`, but the harness loads `layout_for()` and `calculate_seats()` from this file, which has 12 non-ASCII literals. Default: included (Task 1).
5. **Whether R0 includes the exact centring of δ and log σ.**
   - **Background.** R0 runs §5.8's sampler budget, as every arm does, not production's fit-script settings (see Global Constraints).
   - **What the sampler study measured** on the same posterior: 2024-eve data, 4 × (1000 + 1000), figures read from its report on 2026-10-24.

     | Setting | Max R-hat | Min bulk ESS | Min-ESS per minute |
     |---|---|---|---|
     | Production, depth 10 | 1.022–1.025 | 171–222 | 1× |
     | Depth 12 alone | 1.009–1.012 | 825–846 | 2.1× |
     | Centring plus depth 12 | 1.004–1.006 | 1,295–1,501 | 3.6× |

     - At depth 10, every transition is cut off at the cap. At depth 12, every transition ends at its own U-turn at depth 11.
     - The slowest quantities are σ_δ and μ_log σ.
   - **Where the centring is.** It is a work-in-progress port that Brynjólfur held on 16 Oct. It sits on the local branch `kjordaemi-centred-delta-td12` (`3c044bf`, not on master), and its validation is unfinished.
   - **Ruled by Brynjólfur on 2026-10-24: keep the default for now.** R0 is not centred. It stays master plus the three hooks (§5.4), sampled at depth 12, and the question is decided once Tier 0 reports.
   - **Why it matters.** §5.7 counts a cell that fails the gate twice as a failure for its arm, so a sampler artefact would count against R0 in the V2 decision. Under Decision 3's default scope, depth 12 alone sits on the 1.01 line at 1000 + 1000 (Tier 1 samples 2000), and the centring clears it. Under the scored-quantities scope, the slow scales are not gated.
   - **When to decide:** after Tier 0 (Task 11, which runs R0 uncentred at depth 12) and before the freeze (Phase 3). After the freeze, only bugs change an arm.
   - **If it is included,** the centring becomes a fourth change to R0's Stan file. Task 9's equality test then holds up to the log-Jacobian of the reparameterisation, which the test computes from the same draw.
   - **Ruled by Brynjólfur on 2026-10-27, after Tier 0: R0 is centred.** Uncentred, the 2021 f180 cell reached depth 12 on 75% of its transitions. R0 is re-based on production at 884560f, which has been centred since #5 and gathers replications by party identity since #8. The centring is therefore the parent's, not a fourth change, and the equality test needs no log-Jacobian. H3 is now the parent's own, so R0 keeps H1 and H2.

**Spec readings this plan takes** (no answer needed):
- **The hedge.** Sósíalistaflokkurinn gets no hedge correction in 2021 (n_e = 0, §5.4's rule). The 2021 study floored the closing party at 1.
- **The self-test** is the only place a known result is scored before Phase 4. It re-scores a forecast the overdisp study already scored, as §5.6 and §8 require.
- **R0's equivalence to master is shown on the posterior.**
  - It is not shown draw for draw. CmdStan's generated quantities draw from the sampler's own RNG stream.
  - The identity gathers change how many categories each replication draws, so with the same seed every later iteration differs.
  - Task 9 therefore shows that the log density and its gradient are identical.
- **2017 scoring.** Phase 1 refuses every 2017 cell, even once `DECISION.md` exists. The `CONFIRM.md` writer (the score-once lock) belongs with Tier C in the Phase 3–6 runbook.
- **Kjördæmi order.** R0 keeps production's 2024 order in every cell, so Suður closes δ everywhere, as it did in production.
- **"The branch model"** (§5.4's `c62b1b2`, `ecab561`) is master's `Stan/polling_and_fundamentals_kjordaemi.stan` and `R/stan_data.R`. Those commits reached master through the PR #4 squash and are not ancestors of HEAD.
- **Run directories add the seed** (`results/harness/<arm>_<E>_<F>_s<seed>/`, against §5.9's `<arm>_<E>_<F>/`), because Tier 1 runs two seeds per cell.

## Global Constraints

- **The repo is public.**
  - No committed text (code, comments, docs, commit messages, generated files) names a git-excluded folder. These are the top-level `/<name>/` entries of the main checkout's `$(git rev-parse --git-common-dir)/info/exclude`. Spec §5.4–§5.8 and the brief name them; this plan does not repeat the names, so that it obeys its own rule.
  - No committed text holds figures derived from the 1983–2017 ÍSKOS waves. Phase 1 uses no ÍSKOS input.
- **No forecast is compared with a result in this phase.**
  - No 2021 or 2024 forecast is scored against its result before Phase 4, and no 2017 forecast before `harness/DECISION.md` exists.
  - The self-test (Task 5) is the one exception. It re-scores the overdisp study's `base_eve` draws, which are not a harness arm's.
  - Nobody prints, plots or summarises the forecast quantities of a real harness run. The Tier 0 report carries diagnostics only. The gate reads the scored quantities' R-hat and ESS by code.
- **The harness never reads the live `data/` and never writes to the main checkout** (`/Users/brynjolfurjonsson/kosningaspa/`).
  - Inputs come from `harness/snapshot/`, `harness/selftest/` and committed files under `harness/`.
  - Run outputs go to `results/harness/` in the working checkout, which is gitignored.
- **Locale.**
  - Every R run is `LC_ALL=en_US.UTF-8 Rscript …`.
  - After Task 1, non-ASCII characters in R code under `R/harness/` and in `R/election_utils.R` are written as `\uXXXX` escapes, and name tags are ASCII (`stats::setNames()` for non-ASCII names). `tests/harness/test-escapes.R` enforces both.
  - Write JSON holding Icelandic text with `jsonlite::write_json()` (UTF-8) or Python's `json.dump(..., ensure_ascii = False)`, never by hand.
- **Sampler (§5.8).**
  - Every fit runs 4 chains with `parallel_chains = 4`, `init = 0` and `max_treedepth = 12`.
  - Tier 0 runs 200 + 200 iterations. From Tier 1 on, every arm runs 1000 + 2000; iterations are never cut for one arm alone (§6.9).
  - R0 does not inherit production's fit-script settings (4 × (200 + 200) at the default `max_treedepth` of 10, `R/fit_polling_and_fundamentals_kjordaemi_model.R:131-139`).
  - Seeds are fixed in the run list and recorded in `meta.json`.
- **Slow tests.**
  - Tests that compile Stan, sweep every day, or allocate seats for 8000 draws call `skip_unless_slow()` (Task 2). They run only with `HARNESS_SLOW=1`.
  - Each task's "full suite" step means `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R`.
  - The suite stood at 500 passing tests in 12 s (2026-10-24, before this plan).
- **CmdStan.** If a compile fails on a stale precompiled header (`.gch`), stop and ask Brynjólfur before calling `rebuild_cmdstan()`.
- **Code style.**
  - Box modules use `#' @export` roxygen and `box::use()` inside each function, as in `R/harness/`.
  - Use the base pipe.
  - Comments are roxygen blocks or one-line `# WHY:` notes.
  - Prose uses British spelling.
  - New modules go in `R/harness/`, tests in `tests/harness/test-*.R`, and every default path is built with `harness_path()` or `project_root()`.
  - Each task updates its rows in `R/CLAUDE.md` § Harness (v2) and its paragraph in `harness/README.md` in the same commit.
- **Commits.**
  - Before each commit, run `git fetch`, then `git branch --show-current` (expect the execution branch, recorded before Task 1), then `ListAgents`.
  - Every message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
  - Never use bare `git stash`.
  - Before a heavy local job (the slow suite, Tier 0), check `ListAgents` and ask any peer that may be timing a benchmark.

## Review Focus

1. **A 2017 cell has nine parties.** With Sósíalistaflokkurinn absent, Miðflokkurinn closes the house effects. Any rule written for ten parties silently misplaces the hedge or a column: the master's `/= sqrt(3.0)` on rows 1..P−3, or `y_n` columns taken by position. *Test:* Task 8 builds both 2017 cells and pins P = 9, the party order and `mu_gamma_scale` = (1 × 7, 0).
2. **A kjördæmi table in `KJORDAEMI` order** (Reykjavík Suður first) silently changes which kjördæmi closes δ, and it labels `n_pred_k` and the extracted draws wrongly. Production's own RS/RN swap was this bug. *Test:* Task 8 pins the levels and `n_pred_k` by name in every cell. Task 10 pins the labels by Stan index.
3. **The scorer must not touch a result before the guard passes,** not even to learn the party set. *Test:* Task 6 passes `score_cell()` a truth reader that records its calls, and asserts it is never called when the guard refuses.
4. **A result with votes for a party the run does not forecast** (a folded list, or a 9-party run) must stop the scorer, not drop silently. *Test:* Task 5's `cell_truth()` error test.
5. **The sha-skip must not reuse a stale run.** A finished run whose inputs or Stan file changed must be refitted, and the old output kept aside. *Test:* Task 7's `meta_mismatch()` tests and Task 10's runner test.

---

### Task 1: Escape the Icelandic literals in the harness's modules (R54)

**Why.** R parses a module in the locale active when it is loaded.
- Under a C locale, a literal such as `"Annað"` is read in the native encoding.
- A name tag such as `c("Framsókn" = x)` is translated to a native symbol, even when written `"Framsókn"`.
- A session that loads the harness under C and switches to UTF-8 before `read_inputs()` therefore passes `read_inputs()`'s locale check, but loses Framsóknarflokkurinn from S_f. This was verified RED on 2026-10-24 with the subprocess test below.

**Files:**
- Modify: `R/harness/availability.R`, `constants.R`, `fundamentals.R`, `info_set.R`, `tables.R`, `truth.R`. Together they hold 61 non-ASCII string literals and one non-ASCII name tag (via parse data, 2026-10-24).
- Modify: `R/election_utils.R`, which holds 12 (Decision 4).
- Create: `tests/harness/test-escapes.R`.
- Modify: `harness/README.md` (Tests paragraph): the escape rule, and why `read_inputs()`'s locale check is not enough.

**Interfaces:**
- Produces: no API change. Every exported object of every module listed is `identical()` before and after.

- [ ] **Step 1: Write the failing tests** in `tests/harness/test-escapes.R`.

```r
# WHY: R parses a module in the locale active when it is loaded; under C it reads a
# non-ASCII literal natively and translates every name tag, escaped or not (R54).
HARNESS_FILES <- c(Sys.glob(root("R", "harness", "*.R")), root("R", "election_utils.R"))
non_ascii <- function(x) grepl("[^\\x01-\\x7f]", x, perl = TRUE)
tokens <- function(path) {
  pd <- utils::getParseData(parse(path, keep.source = TRUE, encoding = "UTF-8"))
  pd <- pd[pd$terminal, ]
  pd[order(pd$line1, pd$col1), ]
}

test_that("no string literal or symbol in a harness module is non-ASCII", {
  for (f in HARNESS_FILES) {
    tk <- tokens(f)
    bad <- tk[tk$token %in% c("STR_CONST", "SYMBOL", "SYMBOL_SUB", "SYMBOL_FUNCTION_CALL") & non_ascii(tk$text), ]
    expect_equal(nrow(bad), 0, label = paste(basename(f), "lines", paste(bad$line1, collapse = ", ")))
  }
})

test_that("every name tag in a harness module is plain ASCII", {
  for (f in HARNESS_FILES) {
    tk <- tokens(f)
    tag <- which(tk$token == "EQ_SUB") - 1L
    tag <- tag[tk$token[tag] %in% c("STR_CONST", "SYMBOL_SUB")]
    bad <- tk[tag[non_ascii(tk$text[tag]) | grepl("\\u", tk$text[tag], fixed = TRUE)], ]
    expect_equal(nrow(bad), 0, label = paste(basename(f), "lines", paste(bad$line1, collapse = ", ")))
  }
})

test_that("a harness loaded under the C locale still reads the snapshot right", {
  skip_without_snapshot()
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    'options(box.path = normalizePath("."))',
    "box::use(R/harness/constants[FUNDAMENTALS_RENAMED, KJORDAEMI], R/election_utils[layout_for],",
    "         R/harness/info_set[info_set, read_inputs, validate_inputs])",
    'invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))',
    'info <- info_set("2024-11-30", "2024-11-29", read_inputs())',
    'stopifnot(identical(names(FUNDAMENTALS_RENAMED), "Frams\\u00f3kn"),',
    '          length(info$fundamentals$S_f) == 9, "Frams\\u00f3knarflokkurinn" %in% info$fundamentals$S_f,',
    '          all(layout_for("2024-11-30")$kjordaemi %in% KJORDAEMI$kjordaemi))',
    "validate_inputs(info)",
    'cat("R54 OK\\n")'
  ), script)
  out <- withr::with_dir(root(), system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                                         stdout = TRUE, stderr = TRUE, env = "LC_ALL=C"))
  expect_true("R54 OK" %in% out, info = paste(out, collapse = "\n"))
})
```

- [ ] **Step 2: Run them and confirm they fail.**
  - Run: `LC_ALL=en_US.UTF-8 Rscript tests/run.R escapes`
  - Expected: FAIL. The first test names every file holding non-ASCII literals. The second names `constants.R` (`FUNDAMENTALS_RENAMED`). The third prints no "R54 OK", because S_f lacks Framsóknarflokkurinn.
- [ ] **Step 3: Record the exports before the rewrite.** A scratch script (in the session scratchpad, not committed) `box::use()`s each listed module and `saveRDS()`s a named list of its exported objects. Functions are compared by `deparse()` of their bodies with escapes resolved, so record the values only: the constants and what `layout_for()`, `seats_tibble()` and `seats_layout_2021()` return.
- [ ] **Step 4: Rewrite the literals.**
  - Use a one-off script (scratchpad, not committed). It reads each file's parse data and rewrites every `STR_CONST` token holding non-ASCII characters, replacing each such character with `\u` plus four lowercase hex digits. Every Icelandic letter and `§` is in the Basic Multilingual Plane.
  - Comments and roxygen keep their Icelandic.
  - By hand: `FUNDAMENTALS_RENAMED <- stats::setNames("Framsóknarflokkurinn", "Framsókn")`.
  - Drop the WHY comment in `read_inputs()` about the translated name. Keep its locale check, which still protects data reads.
- [ ] **Step 5: Run the tests and the comparison.**
  - Run: `LC_ALL=en_US.UTF-8 Rscript tests/run.R escapes`, then the full suite, then the Step 3 script in compare mode.
  - Expected: PASS. All recorded values are `identical()`.
- [ ] **Step 6: Commit** with the message "harness: escape Icelandic literals so a C-locale load reads the snapshot right (R54)". State in the body the counts escaped per file.

---

### Task 2: The daily-F sweep

The ten design cells validate (Phase 0). This test runs `info_set()` and `validate_inputs()` on every day of the three target cycles, so a leak or a broken rule on any other day would show.

**Files:**
- Modify: `tests/harness/helper-root.R`, adding `skip_unless_slow()`.
- Create: `tests/harness/test-sweep.R`.

**Interfaces:**
- Produces (helper-root.R): `skip_unless_slow()`, which skips with "slow test: set HARNESS_SLOW=1" unless `Sys.getenv("HARNESS_SLOW") == "1"`.

**Measured on 2026-10-24** with a scratch sweep that ran `read_inputs()` once. The runtime was 2,951 days in 52 s. Every day from the first economy release to E − 1 validates. Every earlier day is refused by `info_set()` with its economy message ("… is before the first release of economy covariate(s) …").

| E | Days in the cycle | First F that validates | Days that validate |
|---|---|---|---|
| 2017-10-28 | 363 | 2017-03-09 | 233 |
| 2021-09-25 | 1,427 | 2021-02-26 | 211 |
| 2024-11-30 | 1,161 | 2024-02-29 | 275 |

- [ ] **Step 1: Write the test.** Read `inp <- read_inputs()` once, and the oracle `av <- readr::read_csv(root("harness", "availability.csv"))` (`level, date, fyrirtaeki, fieldwork_end, available_from, rule`). Then, for each E above, take every F from `previous_election(E) + 1` to `E − 1`. Keep from each day only the admitted keys and parties, so memory stays small.

```r
test_that("every day of the 2017, 2021 and 2024 cycles validates from the economy release on", {
  skip_without_snapshot()
  skip_unless_slow()
  for (i in seq_len(nrow(SWEEP))) {
    E <- as.Date(SWEEP$E[i])
    days <- seq(previous_election(E) + 1, E - 1, by = "day")
    res <- lapply(days, \(F) tryCatch({
      info <- info_set(E, F, inp)
      validate_inputs(info)
      list(nat = keys(info$polls_nat), kd = keys(info$polls_kd), parties = info$parties,
           fixed = list(info$kosning_nat, info$results, info$prev_shares),
           late = max(info$polls_nat$date) >= F || max(c(info$polls_nat$available_from, info$polls_kd$available_from)) > F)
    }, error = conditionMessage))
    ok <- vapply(res, is.list, logical(1))
    expect_equal(sum(ok), SWEEP$n_ok[i])
    expect_equal(min(days[ok]), as.Date(SWEEP$first_ok[i]))
    expect_true(all(ok[days >= as.Date(SWEEP$first_ok[i])]))
    expect_true(all(grepl("before the first release of economy covariate", unlist(res[!ok]), fixed = TRUE)))
    good <- res[ok]
    expect_false(any(vapply(good, `[[`, logical(1), "late")))
    expect_true(all(vapply(seq_along(good), \(j) identical(good[[j]]$nat, oracle_nat(days[ok][j])), logical(1))))
    expect_true(all(vapply(seq_along(good)[-1], \(j) subset_of(good[[j - 1]], good[[j]]), logical(1))))
    expect_true(all(vapply(good[-1], \(g) identical(g$fixed, good[[1]]$fixed), logical(1))))
  }
})
```

  The helpers this test needs:
  - `SWEEP`: the table above, as a tibble (E, first_ok, n_ok).
  - `keys(rows)`: the sorted distinct `paste(date, fyrirtaeki)` of a poll table.
  - `oracle_nat(F)`: the same keys from `av` rows with `level == "national"`, `fyrirtaeki != "Kosning"`, `date >= WINDOW_START` and `available_from <= F`.
  - `subset_of(a, b)`: true iff `a$nat ⊆ b$nat`, `a$kd ⊆ b$kd` and `a$parties ⊆ b$parties`. The admitted polls and parties never shrink as F grows.
- [ ] **Step 2: Run it.**
  - Run: `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R sweep`
  - Expected: PASS in about a minute.
  - This test checks behaviour that already holds, so check that it can fail: temporarily change one `n_ok` and confirm it fails, then restore it.
- [ ] **Step 3: Commit** with the message "harness: sweep every forecast day of the three target cycles through the leak gate".

---

### Task 3: National scores

**Files:**
- Create: `R/harness/scores.R`, `tests/harness/test-scores.R`.

**Interfaces** (all pure, no I/O; formulas follow the overdisp study's `code/R/evaluate_od.R`):
- `NAMED8`: `POLL_PARTIES` without Annað and Sósíalistaflokkurinn.
- `crps_draws(x, y)` → numeric(1). The sorted-draw V-statistic form of E|X − y| − ½E|X − X′|:
  ```r
  x <- sort(x); n <- length(x); mean(abs(x - y)) - sum((2 * seq_len(n) - n - 1) * x) / n^2
  ```
- `wide_draws(long, value)` → a numeric matrix with one row per draw, in `.chain`, `.iteration` order, and one column per `flokkur` in first-appearance order. It carries integer attributes `chain` and `iteration`. `long` has `.chain`, `.iteration`, `.draw`, `flokkur` and the column named by `value`. It is an error if a draw lacks a party or a (`.chain`, `.iteration`) pair repeats.
- `party_scores(X, y)` → a tibble with one row per column of X:
  - `flokkur`, `mean`, `sd` (n − 1), `median`, and `q05`, `q25`, `q75`, `q95` (all type 7);
  - `crps`;
  - `dss` = log((100·sd)²) + ((y − mean)/sd)²;
  - `pit` = mean(X < y) + ½·mean(X = y);
  - `in50` and `in90`.

  `y` is a named numeric vector whose names are set-equal to `colnames(X)`. It is reordered to the columns, and it is an error if the names differ.
- `alr_dss(X, y, zero_share)` → `c(mdss = , d2 = )`. The steps:
  - replace zeros: `X <- pmax(X, zero_share); X / rowSums(X)`;
  - take the alr against column 1: `Z <- log(X[, -1] / X[, 1])`, `z <- log(y[-1] / y[1])`;
  - `S <- cov(Z)`, `d <- z − colMeans(Z)`, `d2 = dᵀS⁻¹d` (via `solve(S, d)`);
  - `mdss = log det S + d2` (via `determinant(S, logarithm = TRUE)$modulus`).
- `energy_score(X, y)` → numeric(1): mean ‖Xᵢ − y‖ − ½ mean ‖Xᵢ − Xⱼ₍ᵢ₎‖, with the fixed pairing `j <- ((seq_len(n) - 1 + n %/% 2) %% n) + 1`.
- `national_scores(X, y, zero_share)` → a one-row tibble with these columns:

  | Column | Definition |
  |---|---|
  | `n_parties` | number of parties |
  | `crps10` | 100 × the mean CRPS (the spec's name, also for 9 parties) |
  | `is90` | 100 × the mean of `(hi − lo) + 20·max(lo − y, 0) + 20·max(y − hi, 0)`, with type-7 `q05`/`q95` |
  | `dss10` | the mean `dss` |
  | `mdss`, `d2` | from `alr_dss()` |
  | `es` | `energy_score(100 * X, 100 * y)` |
  | `mae8` | 100 × the mean of \|median − y\| over the `NAMED8` parties present |
  | `mae_all` | the same over every party |
  | `w90` | 100 × the mean of `q95 − q05` |
  | `cov50`, `cov90` | counts |
  | `zero_cells_replaced` | `sum(X <= 0)` |

- `mae_se(X, y, parties)` → numeric(1): `100 / length(parties) * sqrt(sum(mcse^2))`, where `mcse` is `posterior::mcse_median(matrix(X[, p], ncol = n_chains))` for each party, and X's rows are in chain order.
- `batch_of(chain, iteration, per_chain = 4L)` → integer block id `(chain − 1) × per_chain + ceiling(iteration / (max(iteration) / per_chain))`. With 4 chains this gives 16 blocks.
- `batch_se(f, X, block)` → a named numeric vector: `f` (X → named numeric) is applied to each block's rows, and the result is the sd across blocks divided by √(number of blocks).
- `kd_to_nat(eday_kd)` → a tibble (`.chain`, `.iteration`, `.draw`, `flokkur`, `votes`, `share`): votes summed over kjördæmi, then `share = votes / sum(votes)` by `.draw`.

- [ ] **Step 1: Write the failing tests:**
  - `crps_draws(c(0, 1), 0)` is 0.25, and `crps_draws(rep(3, 5), 5)` is 2. For `x <- c(0.1, 0.4, 0.35, 0.2, 0.9)` and `y = 0.3`, it equals `mean(abs(x - y)) - 0.5 * mean(abs(outer(x, x, "-")))` to 1e-14.
  - On a random 2000 × 4 Dirichlet-like matrix, permuting the columns (so a different party is the alr reference) leaves `d2` and `mdss` unchanged to 1e-8.
  - When `y` is the closed exponential of the alr means (`y ∝ c(1, exp(colMeans(Z)))`), `d2` is 0 to 1e-10 and `mdss` equals `log det S`.
  - A matrix with one zero cell gives a finite `mdss` and `zero_cells_replaced = 1`.
  - IS90 is exactly `hi − lo` for `y` inside the interval, and `hi − lo + 20·(lo − y)` below it (a hand-built 100-draw column).
  - `mae8` ignores Annað and Sósíalistaflokkurinn: a forecast whose medians are right except Annað's, which is off by 0.10, gives `mae8 = 0` and `mae_all = 100 × 0.10 / 10`.
  - The energy score on a 4 × 2 matrix equals the value computed by hand with pairs (1,3), (2,4), (3,1), (4,2).
  - `batch_of(rep(1:4, each = 8), rep(1:8, 4))` is `rep(1:16, each = 2)`.
  - `batch_se(colMeans-of-column-1, X, block)` equals `sd(block means) / 4` for 16 blocks.
  - `kd_to_nat()` on two kjördæmi and two parties sums votes before taking shares. With RN (A 10, B 30) and S (A 30, B 30), A's share is 40/100.
  - `national_scores()` refuses a `y` whose names differ from the columns ("truth and draws name different parties").
  - `wide_draws()` refuses a draw that lacks a party.
- [ ] **Step 2: Run them and confirm they fail.**
  - Run: `LC_ALL=en_US.UTF-8 Rscript tests/run.R scores`
  - Expected: FAIL; `R/harness/scores` is not found.
- [ ] **Step 3: Implement the interfaces above in `R/harness/scores.R`.**
- [ ] **Step 4: Run the tests and confirm they pass,** then run the full suite.
- [ ] **Step 5: Commit** with the message "harness: national scores (CRPS10, DSS, d², IS90, MAE, energy score, batch SEs)".

---

### Task 4: Kjördæmi and seat scores

**Files:**
- Modify: `R/harness/scores.R`, `tests/harness/test-scores.R`.

**Interfaces:**
- Consumes: `calculate_seats(draw, layout, ineligible)` from `R/election_utils.R`. Its `draw` is a tibble (`flokkur`, `kjordaemi`, `votes`), and it returns (`flokkur`, `kjordaemi`, `seats`, `kjordaemi_seats`).
- Produces:
  - `kd_scores(eday_kd, truth_kd)` → a one-row tibble. Shares are taken within each (`.draw`, `kjordaemi`). The columns:

    | Column | Definition |
    |---|---|
    | `kd_cells` | number of cells |
    | `kd_crps` | 100 × the mean CRPS over cells |
    | `kd_mae_named` | 100 × the MAE of medians over the `NAMED8` cells |
    | `kd_w90` | 100 × the mean of `q95 − q05` (type 7) |
    | `kd_cov90` | count |

    `truth_kd` is (`kjordaemi`, `flokkur`, `share`). An unmatched cell on either side is an error.
  - `seat_draws(eday_kd, layout, ineligible = "Annad")` → a tibble (`.chain`, `.iteration`, `.draw`, `flokkur`, `seats`). Each draw's rows, sorted by `kjordaemi` then `flokkur`, go through `calculate_seats()`, and the seats are summed per party. (Write `"Annað"` in code.)
  - `seat_scores(seats, actual, coalition, formed = NULL, majority = 32L)` → `list(summary, per_party)`.
    - `seats` comes from `seat_draws()`. `actual` is a named integer vector over the same parties. `coalition` and `formed` are character vectors of poll labels.
    - `summary` has these columns:

      | Column | Definition |
      |---|---|
      | `seat_crps` | the sum over parties of `crps_draws()` on seat counts |
      | `seat_abs_err` | the sum of \|median − actual\| |
      | `seat_w90` | the mean of `q95 − q05`, with type-1 quantiles |
      | `seat_cov90` | count |
      | `p_exact` | the share of draws in which every party's seats are exact |
      | `p_majority` | `majority_prob(seats, coalition, majority)` |
      | `majority_outcome` | `sum(actual[coalition]) >= majority` |
      | `brier` | `(p_majority − majority_outcome)²` |
      | `p_formed` | `majority_prob(seats, formed, majority)`, or NA when `formed` is NULL: the probability of the coalition that formed (§5.6, report only) |

    - `per_party` has `flokkur`, `crps`, `median`, `q05`, `q95` and `p_exact`.
  - `majority_prob(seats, parties, majority = 32L)` → the share of draws in which the parties' seats sum to at least `majority`.

- [ ] **Step 1: Write the failing tests:**
  - `kd_scores()` on draws that equal the truth in every cell gives `kd_crps = 0`, `kd_w90 = 0` and `kd_cov90 = kd_cells`.
  - `kd_scores()` refuses a truth missing one cell ("cells without a match").
  - `seat_draws()` on a one-draw table equals `calculate_seats()`'s per-party totals on that draw. With `ineligible = "Annað"`, Annað has 0 seats, and the 63 seats are still filled (reuse `draw_with()` from `test-seats.R`).
  - `seat_scores()` on 16 identical draws equal to `actual` gives `seat_crps = 0`, `p_exact = 1` and `brier = 0`. With `actual` changed so the coalition loses its majority, `brier = 1`. Without `formed`, `p_formed` is NA.
  - `majority_prob()` on a hand-built 4-draw table where the coalition holds 31, 32, 33 and 20 seats is 0.5.
- [ ] **Step 2: Run them and confirm they fail** (`tests/run.R scores`).
- [ ] **Step 3: Implement the interfaces above.**
- [ ] **Step 4: Run the tests and confirm they pass,** then run the full suite.
- [ ] **Step 5: Commit** with the message "harness: kjördæmi scores, seat draws, seat CRPS and the coalition Brier score".

---

### Task 5: The scorer self-test (exit criterion 1)

Spec §5.6: "before scoring anything new, the module must reproduce `base_eve`… from that study's saved draws". Two of that run's files are copied into a gitignored, sha-checked folder through the snapshot machinery of Phase 0. The test then re-scores them against the committed 2024 truth.

**Files:**
- Modify: `R/harness/inputs.R`. `snapshot_inputs(src_root, dest, manifest, files = SNAPSHOT_FILES)` gains `files`; the default keeps today's behaviour.
- Create: `R/harness/selftest.R`.
- Create: `tests/harness/test-selftest.R`.
- Modify: `R/harness/truth.R`, adding `read_truth()` and `cell_truth()`.
- Modify: `tests/harness/test-truth.R`.
- Modify: `.gitignore` (add `harness/selftest/`).
- Generate and commit: `harness/selftest_manifest.csv`.
- Modify: `harness/README.md`, adding a section "The scorer self-test" with the copy command and the reason the test is the phase's one scoring of a known result.

**Interfaces:**
- Produces (`selftest.R`):
  - `SELFTEST_FILES` = `c(eday_national = "runs/base_eve/eday_national.parquet", constituency_predictions = "runs/base_eve/constituency_predictions.parquet")`. The paths are relative to the overdisp study's folder.
  - `selftest_inputs(src_root)`, which wraps `snapshot_inputs(src_root, harness_path("selftest"), harness_path("selftest_manifest.csv"), files = SELFTEST_FILES)`.
  - `selftest_file(name)` → `snapshot_file(name, harness_path("selftest"), harness_path("selftest_manifest.csv"))`.
  - `selftest_present()` → logical(1).
- Produces (`truth.R`):
  - `read_truth(path = harness_path("truth", "elections_official.csv"))` → the committed table with its column types.
  - `cell_truth(E, parties, truth = read_truth())` → `list(nat, kd, seats)`, all from `fold_truth(truth)` at E:
    - `nat`: national vote shares, a named numeric over `parties`;
    - `kd`: within-kjördæmi shares, a tibble (`kjordaemi`, `flokkur`, `share`);
    - `seats`: official seats, a named integer over `parties`.

    It is an error if a poll party outside `parties` has votes at E: "truth has votes for parties the run does not forecast: …".

The reference values come from the overdisp study's `compare/summary.csv`, row `base_eve`, read on 2026-10-24. They go into the test file verbatim:

| Score | Value | SE |
|---|---|---|
| crps10 | 0.70877364221101147 | 0.0051017190249985113 |
| is90 | 6.352848993229478 | 0.073069598173392494 |
| dss10 | 2.4305189899030442 | 0.034319142943985859 |
| mdss | −0.87010115800367061 | 0.90728689419264363 |
| d2 | 36.537669694218991 | 0.94081239456516885 |
| es | 2.6166490644078086 | — |
| mae8 | 1.1389019529791662 | 0.0082496541290049616 (`mae_se`) |
| mae_all (study: mae10) | 0.93983520886613281 | 0.0067659892340684201 (`mae_se`) |
| w90 | 3.050931775110548 | — |
| cov90 | 6 | — |
| zero_cells_replaced | 0 | — |
| seat_crps | 4.0283096250000145 | 0.04525820426471517 |
| seat_abs_err | 5 | — |
| seat_w90 | 2.8 | — |
| seat_cov90 | 10 | — |
| p_majority for Samfylkingin + Viðreisn + Flokkur Fólksins (study: p_gov32) | 0.839875 | — |
| kd_mae_named (study: kd_mae48) | 1.3773025672446 | — |
| kd_crps (study: kd_crps60) | 0.9089015855884458 | — |
| kd_w90 | 3.1471739157676528 | — |
| kd_cov90 | 37 | — |

The study's settings:
- `zero_share` = 0.5 / 196292, where 196292 is that run's `n_pred`.
- Seats use `seats_tibble()` (the 2024 layout) with `ineligible = character(0)`, so every list was eligible.
- The 16 blocks are `batch_of(chain, iteration)` over 4 × 2000 draws.
- `eday_national.parquet` has `.chain`, `.iteration`, `.draw`, `flokkur` and `value` (rename `value` to `share`).
- `constituency_predictions.parquet` has `.chain`, `.iteration`, `.draw`, `kjordaemi`, `flokkur` and `votes` (integer); it is the `eday_kd` input.

- [ ] **Step 1: Copy the inputs.**
  - Run in R: `selftest_inputs("<the overdisp study's folder in the main checkout>")`. Spec §5.6 names the folder.
  - Expected: `harness/selftest/` holds the two parquets, and `harness/selftest_manifest.csv` has two rows.
  - Check that `git status` shows the manifest and not the folder.
- [ ] **Step 2: Write the failing tests.**
  - `test-truth.R`:
    - `cell_truth("2024-11-30", POLL_PARTIES)` has `nat` summing to 1, `seats` summing to 63, and kjördæmi votes summing to 212470;
    - `cell_truth("2024-11-30", setdiff(POLL_PARTIES, "Sósíalistaflokkurinn"))` errors ("truth has votes for parties the run does not forecast: Sósíalistaflokkurinn");
    - `cell_truth("2017-10-28", setdiff(POLL_PARTIES, "Sósíalistaflokkurinn"))` succeeds, because the party had no list in 2017.
  - `test-selftest.R`: the national scores are always run when the self-test inputs are present (`skip_if_not(selftest_present())`). The kjördæmi and seat scores also call `skip_unless_slow()`. All comparisons use `expect_equal(..., tolerance = 1e-9)`.

```r
# WHY: the phase's one scoring of a known result (spec §5.6, §8). It re-scores draws the
# overdisp study already scored, so no harness arm meets a result before the freeze.
ZERO_SHARE <- 0.5 / 196292
draws <- function() dplyr::rename(arrow::read_parquet(selftest_file("eday_national")), share = value)

test_that("the scorer reproduces base_eve's national scores and their SEs", {
  skip_if_not(selftest_present(), "no scorer self-test inputs: run selftest_inputs()")
  X <- wide_draws(draws(), "share")
  y <- cell_truth("2024-11-30", colnames(X))$nat
  s <- national_scores(X, y, ZERO_SHARE)
  for (k in names(NATIONAL_REF)) expect_equal(s[[k]], NATIONAL_REF[[k]], tolerance = 1e-9, label = k)
  block <- batch_of(attr(X, "chain"), attr(X, "iteration"))
  se <- batch_se(\(x) unlist(national_scores(x, y, ZERO_SHARE)[names(SE_REF)]), X, block)
  for (k in names(SE_REF)) expect_equal(se[[k]], SE_REF[[k]], tolerance = 1e-9, label = paste("se", k))
  expect_equal(mae_se(X, y, intersect(NAMED8, colnames(X))), 0.0082496541290049616, tolerance = 1e-9)
  expect_equal(mae_se(X, y, colnames(X)), 0.0067659892340684201, tolerance = 1e-9)
})
```

  Here `NATIONAL_REF` and `SE_REF` hold the table's values. Two more tests:
  - "the scorer reproduces base_eve's kjördæmi scores": `kd_scores()` on `constituency_predictions` against `cell_truth()$kd`.
  - "the scorer reproduces base_eve's seat scores": `seat_draws(cp, seats_tibble(), ineligible = character(0))`, then `seat_scores()` with `coalition = c("Samfylkingin", "Viðreisn", "Flokkur Fólksins")`, and the seat-CRPS SE through `batch_se()` on the wide seat matrix. The study's `p_gov32` is this call's `p_majority`, because the study scored the coalition that formed.
- [ ] **Step 3: Run the tests and confirm they fail.**
  - Run: `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R "selftest|truth"`
  - Expected: FAIL; `cell_truth` and `R/harness/selftest` are not found.
- [ ] **Step 4: Implement `read_truth()`, `cell_truth()` and `R/harness/selftest.R`.**
  - If a score misses its reference beyond the tolerance, the port of the formula is wrong. Compare it line by line with `code/R/evaluate_od.R` in the overdisp study.
  - Never change a reference value.
- [ ] **Step 5: Run the tests and confirm they pass,** with SKIP 0 under `HARNESS_SLOW=1`. Record the seat test's runtime. Then run the full suite.
- [ ] **Step 6: Commit** with the message "harness: scorer self-test reproduces the overdisp study's base_eve scores". The body names the scores reproduced and the tolerance.

---

### Task 6: The freeze guard and `score_cell()` (exit criterion 2)

**Files:**
- Create: `R/harness/freeze.R`, `R/harness/scorer.R`, `tests/harness/test-freeze.R`, `tests/harness/test-scorer.R`.
- Modify: `R/harness/tables.R`, adding `formed_coalition()`.
- Modify: `tests/harness/test-tables.R`.
- Modify: `harness/README.md`, adding a section "The freeze and the 2017 locks".

**Interfaces:**
- Consumes:
  - `TARGETS` (from `constants.R`);
  - `cell_truth()` (Task 5);
  - `national_scores()`, `party_scores()`, `batch_of()`, `batch_se()`, `kd_to_nat()`, `wide_draws()`, `kd_scores()`, `seat_draws()` and `seat_scores()` (Tasks 3–4);
  - `layout_for()`.
- Produces (`tables.R`): `formed_coalition(E, coalitions)` → the poll-party labels (via `named_parties()`) of the first cabinet with `caretaker == FALSE` and `from > E`. It is hindsight, so only `score_cell()` calls it, after the guard; `info_set()` never does.
- Produces (`freeze.R`):
  - `read_sha_list(path)` → a tibble (`sha256`, `path`).
    - Lines are `<64 lowercase hex><two spaces><path>`, as `shasum -a 256` writes them. `#` and blank lines are skipped.
    - Any other line is an error: "malformed sha256 line <n> in <path>".
  - `verify_sha_list(path, root)` → `invisible(TRUE)`. Paths are relative to `root`. Errors: "empty sha256 list: …", "missing: <path>", "sha256 mismatch: <path>".
  - `is_committed(paths, root)` → logical(1). Every path must be tracked (`git -C root ls-files --error-unmatch`) and unchanged against HEAD (`git -C root diff --quiet HEAD --`).
  - `freeze_guard(E, F, root = project_root())` → `invisible(TRUE)`. Otherwise it stops at the first failure, in this order:
    1. "not a forecast cell: E = …, F = …" if (E, F) is not a `TARGETS` row;
    2. "no freeze: harness/FREEZE.md and harness/FREEZE.sha256 must exist before a target cell is scored";
    3. "harness/FREEZE.sha256 does not cover harness/FREEZE.md";
    4. `verify_sha_list()`'s error for `FREEZE.sha256`;
    5. "the freeze is not committed" unless `is_committed()` holds for both files;
    6. for a confirmatory cell: "2017 is locked until harness/DECISION.md is committed with its sha256", unless `DECISION.md` and `DECISION.sha256` pass the checks of 2–5;
    7. for a confirmatory cell: "2017 has been scored (harness/CONFIRM.md): no second look", if `CONFIRM.md` exists.
- Produces (`scorer.R`):
  - `score_cell(run_dir, truth = cell_truth, root = project_root())` → a list with these elements:
    - `national`: two rows. `reading = "kd_aggregate"` is the scored row, `kd_to_nat(eday_kjordaemi)` with `zero_share = 0.5 / sum(n_pred_k)`. `reading = "eday_national"` is reported beside it, with `zero_share = 0.5 / n_pred`. Each row carries the batch SEs (`se_` prefix) of `crps10`, `is90`, `dss10`, `mdss`, `d2`, `mae8`, `cov50` and `cov90`, so coverage is reported with its MC error (§5.6).
    - `gap_pp`: 100 × the maximum absolute difference between the two readings' posterior means.
    - `per_party`: for the scored reading, with `pit_se` from `batch_se()`.
    - `kjordaemi` and `seats`: seats from `seat_draws(eday_kjordaemi, layout_for(E), ineligible = "Annað")` and `seat_scores(..., coalition = meta$coalition_event$parties, formed = formed_coalition(E, read_coalitions()))`.
    - `seats_per_party`.

    The order of operations:
    1. Read `run_dir/meta.json` (`arm`, `E`, `F`, `party_order`, `n_pred`, `n_pred_k`, `coalition_event`).
    2. Call `freeze_guard(E, F, root)`.
    3. Refuse a confirmatory cell with "2017 cells are scored by the Tier C runbook, which writes harness/CONFIRM.md".
    4. Only then call `truth(E, meta$party_order)`, and only then read the parquets.

- [ ] **Step 1: Write the failing tests.**
  - `test-freeze.R` uses a helper `frozen_root(decision = FALSE)`. It makes a `withr::local_tempdir()`, runs `git init -q`, writes `harness/FREEZE.md` and `harness/FREEZE.sha256` (computed with `digest`), and commits both with `git -c user.name=t -c user.email=t@t commit -qm freeze`. The tests:
    - the guard passes for (2024-11-30, 2024-11-29);
    - each of messages 1–5 is raised by its own broken root: a missing file, a list without FREEZE.md, an edited FREEZE.md, an uncommitted freeze;
    - 2017 raises 6 without a decision, passes with a committed decision, and raises 7 once `CONFIRM.md` exists;
    - `read_sha_list()` refuses a malformed line.
  - `test-scorer.R` uses a helper `fake_run(E, F)`. It writes a run directory with a `meta.json` and synthetic parquets: 4 chains × 4 iterations, 3 parties, 2 kjördæmi.

```r
test_that("score_cell refuses every target cell while the repo has no freeze, before reading any result", {
  expect_false(file.exists(root("harness", "FREEZE.md")))
  for (i in seq_len(nrow(TARGETS))) {
    calls <- 0L
    reader <- function(...) { calls <<- calls + 1L; stop("truth read") }
    expect_error(score_cell(fake_run(TARGETS$E[i], TARGETS$F[i]), truth = reader), "no freeze")
    expect_equal(calls, 0L)
  }
})
```

  More `test-scorer.R` tests:
  - A non-target cell (2024-11-30, 2024-09-01) errors with "not a forecast cell".
  - `score_cell()` on a `fake_run()` under `frozen_root()`, with a truth reader that returns the synthetic draws' own value, gives `crps10 = 0` and `kd_crps = 0`. The reader is called exactly once, with the run's E and `party_order`.
  - A 2017 cell under a frozen root with a committed decision errors with "Tier C runbook".
  - `test-tables.R`: `formed_coalition()` gives Samfylkingin, Viðreisn, Flokkur Fólksins at 2024-11-30. It gives Sjálfstæðisflokkurinn, Framsóknarflokkurinn, Vinstri Græn (`POLL_PARTIES` order) at 2017-10-28 and at 2021-09-25. Those are the cabinets Katrín Jakobsdóttir I (from 2017-11-30) and II (from 2021-11-28). At 2017-10-28 it skips the caretaker row of 2017-09-16.
- [ ] **Step 2: Run them and confirm they fail** (`tests/run.R "freeze|scorer"`).
- [ ] **Step 3: Implement `freeze.R` and `scorer.R` as specified.**
- [ ] **Step 4: Run the tests and confirm they pass,** then run the full suite.
- [ ] **Step 5: Commit** with the message "harness: freeze guard, 2017 locks and the guarded score_cell()".

---

### Task 7: Run records

**Files:**
- Create: `R/harness/runs.R`, `tests/harness/test-runs.R`.
- Modify: `harness/README.md`, adding a section "Runs" (ids, layout of a run directory, sha-skip, DIAG, the gate).

**Interfaces** (`runs.R`):
- `EXTRACT_VERSION`: `1L`.
- `SAMPLER`: `list(chains = 4L, parallel_chains = 4L, init = 0, max_treedepth = 12L)`.
- `results_root()` → `Sys.getenv("HARNESS_RESULTS", file.path(project_root(), "results", "harness"))`.
- `run_id(arm, E, F, seed)` → for example `"r0_2024-11-30_2024-11-29_s1101"`.
- `run_dir(id)` → `file.path(results_root(), id)`.
- `sha256_file(path)` → hex, via `digest::digest(file = path, algo = "sha256")`.
- `write_frozen_inputs(dir, stan_data, inputs_meta)`:
  - writes `dir/inputs/stan_data.json` (`cmdstanr::write_stan_json()`) and `dir/inputs/inputs.json` (`jsonlite::write_json(auto_unbox = TRUE, pretty = TRUE, digits = NA)`);
  - makes both read-only (`Sys.chmod(mode = "0444")`);
  - returns the data sha;
  - errors if `dir/inputs` exists.
- `expected_meta(arm, seed, iter_warmup, iter_sampling, stan_file, config_file, data_sha)` → a named list of the keys a reusable run must match: `stan_sha256`, `config_sha256`, `data_sha256`, `seed`, `iter_warmup`, `iter_sampling`, `chains`, `max_treedepth`, `init` and `extract_version`.
- `meta_mismatch(meta, expected)` → the names of the keys whose values differ, or are missing from `meta`. `character(0)` means the run can be reused.
- `retire_run(dir)` → moves `dir` to `results_root()/_stale/<id>_<YYYYmmddHHMMSS>` and returns the new path.
- `summarise_diagnostics(params, scored, sampler)`:
  - `params` and `scored` are posterior `draws_array`s.
  - `sampler` is `list(num_divergent, num_max_treedepth, ebfmi, max_depth)`, one value per chain. The first three come from `fit$diagnostic_summary()`. `max_depth` is the deepest `treedepth__` in `fit$sampler_diagnostics()`, which shows whether trajectories end by U-turn below the cap.
  - It returns `list(max_rhat, max_rhat_scored, min_ess_bulk, min_ess_tail, min_ess_bulk_scored, min_ess_tail_scored, divergent, treedepth_hits, max_depth, min_ebfmi)`.
  - `max_rhat` and `min_ess_*` are taken over `params` and `scored` together (Decision 3).
- `diagnostics_gate(d, rhat_max = 1.01, ess_min = 400)` → `list(pass, reasons)`.
  - The rules: `max_rhat ≤ rhat_max`, `divergent == 0` and `min_ess_bulk_scored ≥ ess_min`.
  - `reasons` names each failed rule with its value.
- `diag_line(id, minutes, d, gate)` → exactly `sprintf("DIAG run=%s minutes=%.1f max_rhat=%.4f min_ess_bulk=%.0f min_ess_tail=%.0f divergent=%d treedepth_hits=%d max_depth=%d min_ebfmi=%.3f gate=%s", …)`, with `gate` PASS or FAIL.

- [ ] **Step 1: Write the failing tests:**
  - `run_id("r0", as.Date("2024-11-30"), as.Date("2024-11-29"), 1101)` is `"r0_2024-11-30_2024-11-29_s1101"`.
  - `write_frozen_inputs()` writes both files read-only (`file.access(path, 2) == -1`) and refuses an existing `inputs/`.
  - `meta_mismatch()` returns `character(0)` for an equal list. It returns `"data_sha256"` when the data sha differs, and `"seed"` when `seed` is missing.
  - `retire_run()` moves a directory under `_stale/` and leaves nothing at the old path (with `HARNESS_RESULTS` set to a temp dir through `withr::local_envvar()`).
  - `summarise_diagnostics()` on a synthetic 4-chain `draws_array` in which one chain is shifted by 3 sd gives `max_rhat > 1.01`, and `diagnostics_gate()` fails with a reason naming R-hat. Unshifted, it passes. One divergence fails it. A scored ESS of 399 fails it.
  - `diag_line()` for a fixed input equals the expected string character for character.
- [ ] **Step 2: Run them and confirm they fail** (`tests/run.R runs`).
- [ ] **Step 3: Implement `runs.R`.**
- [ ] **Step 4: Run the tests and confirm they pass,** then run the full suite.
- [ ] **Step 5: Commit** with the message "harness: run ids, frozen inputs, meta and sha-skip, the DIAG line and the §5.7 gate".

---

### Task 8: The R0 input builder

R0 is production mapped to E (§5.4). The builder assembles the cell's admitted inputs in production's format and calls production's `prepare_stan_data()` (`R/stan_data.R`, which reads no files). It then applies the overrides below, and only these.

**Files:**
- Create: `R/harness/r0.R` (the builder half), `harness/configs/r0.json`, `tests/harness/test-r0-inputs.R`.

**Interfaces:**
- Consumes:
  - `info_set()`, which supplies `parties`, `polls_nat`, `polls_kd`, `kosning_nat`, `results`, `fundamentals`, `valid_votes_open`, `prev_shares`, `entry` and `coalition_event`;
  - `epoch_of()` (tables.R), `fundamentals_party()` (fundamentals.R), and `prepare_stan_data()` (`R/stan_data.R`).
- Produces:
  - `R0_KJORDAEMI`: Reykjavík Norður, Reykjavík Suður, Suðvestur, Norðvestur, Norðaustur, Suður. This is production's level order in the 2024 run, so the last level, Suður, closes δ.
  - `read_r0_config(path = harness_path("configs", "r0.json"))` → `list(campaign_days = 47L, desired_weight = 0.22, weight_time = 180L, debut = "median_new_party_before_E")`. A missing or extra key is an error.
  - `r0_production_inputs(info)` → `list(polling, constituency, fundamentals)` in `prepare_stan_data()`'s format:
    - **polling**: `kosning_nat` and `polls_nat` (`date`, `fyrirtaeki`, `flokkur`, `n`).
      - `flokkur` is a factor in the order Annað, then the other `info$parties` in `POLL_PARTIES` order.
      - `fyrirtaeki` is a factor with `Kosning` first, then the other houses present, by their first admitted poll date (ties by name, `method = "radix"`). There are no unused levels.
      - Rows are sorted by date, `fyrirtaeki` and `flokkur`.
    - **constituency**: `info$results` at `previous_election(E)` as `Kosning` rows (`votes` → `n`), plus the `polls_kd` rows dated after `previous_election(E)`.
      - `kjordaemi` is a factor in `R0_KJORDAEMI` order. `flokkur` and `fyrirtaeki` use polling's levels.
      - Rows are sorted by `flokkur`, date and `kjordaemi` (fit l.63).
      - It is an error if a dropped party has votes, a kjördæmi date is not a national date, or a kjördæmi date has more than one house.
    - **fundamentals**: `fundamentals$train` (with `flokkur` := `party`), `fundamentals$target`, and one Annað target row.
      - The Annað row has date E, the target rows' `year`, `vnv` and `growth`, `voteshare_prior` = 100 × Annað's `prev_shares` share at E, and `incumbent`, `incumbent_years`, `incumbent_prime_minister`, `new_party` and `party_split` all 0. Its masked columns are NA.
      - `flokkur` is a factor with the parties' order first, then every other label in first-appearance order (the rule in `R/data.R` l.39–52). Rows are sorted by `flokkur` and date.
  - `r0_stan_data(info, cfg = read_r0_config())` → `list(data, meta)`.
    - `data` is `prepare_stan_data(polling, fundamentals, constituency, election_date = E)` with the overrides below. Data frames become matrices, and the list fields that are not in the Stan data block (`N`, `xm`, `n_election`, `time_diff_f`) are dropped.
    - `meta` holds `party_order`, `kjordaemi_levels`, `house_levels`, `n_e` (named), `newcomers`, `debut_share`, `eday_index` (= D + `pred_y_time_diff`), `n_pred`, `n_pred_k` (named), `n_polls_nat`, `n_polls_kd`, `max_poll_date` and `coalition_event`.

**The overrides.** These and nothing else:

| Field | R0 | Production |
|---|---|---|
| `days_between_stjornarslit_and_election` | `campaign_days`, 47 | E − 2024-10-14 (47 only for 2024; negative for 2021) |
| `n_pred_k` | `valid_votes_open$votes`, matched by name to `R0_KJORDAEMI` | the fit script's kjörskrá × kjörsókn with RS/RN swapped (fit l.99–114) |
| `N_obs_k`, `obs_k`, `constituency_weights` | one row per kjördæmi date: each kjördæmi's share of that date's total n, columns by name; `obs_k` by date | fit l.78–96, by position |
| `constituency_weights_pred` | `valid_votes_open$w`, by name | the first weights row (the previous election), by position |
| `desired_weight`, `weight_time` | 0.22, 180 (config) | fit l.118–119, the same values |
| `mu_gamma_scale` (new) | for `party_order[-1]`: 1/√n_e, and 0 when n_e = 0; n_e = `epoch_of(E) − entry_epoch` | `/ sqrt(3.0)` on rows 1..P−3 and `/ sqrt(2.0)` on row P−2, inside the Stan file |
| Column D_f + 1 of `x_f`, `index_f` and `n_parties_f`, plus the newcomer rows of `y_f`, `x_f`, `incumbent_f`, `incumbent_years`, `vnv` and `growth` | rebuilt by the prediction-column rule below | the previous election's party set |

Decision 5 (2026-10-27) removed two rows from this table. They were R0's own `n_active_d`/`active_index_d`, and the positional `n_parties_n_rep`/`n_parties_k_rep` that `rep_counts()` restored. R0's parent became 884560f (aa2ac1a since b57a015, with the same `prepare_stan_data()`): its `prepare_stan_data()` computes the active parties itself, and its Stan file declares no rep counts.

The builder applies no house filter (§5.4: no Gallup filters, as in the final 2024 run).

`month_before_election` is not overridden. Production computes it from a hard-coded list of the four window elections, which is right for every window E, and a test pins it.

**The prediction-column rule** is a port of `prediction_column()` from the 2021 study's `code/R/prep.R`. Let `col = D_f + 1`.
1. Take each party's share at `previous_election(E)` from the training rows' `voteshare`. Map labels with `fundamentals_party()`, so lists outside the party order fold into Annað. Normalise over the parties with a share above 0.
2. The newcomers are the parties with no previous share. Append a named zero row for each to `y_f`, `x_f`, `incumbent_f`, `incumbent_years`, `vnv` and `growth`, and increase `P_f` by one for each.
3. Set `x_f[p, col] = logit(s_p) − c`, where c is the mean logit over the non-newcomers. A newcomer gets `logit(debut) − c`, where `debut` is the median of `voteshare / 100` over the training rows with `new_party == 1` and `voteshare > 0`. That is 0.0715 for E = 2017 and 0.072 for E = 2021 (Decision 1; via `info_set()`, 2026-10-24).
4. Set `index_f[1:P, col] = match(party_order, rownames(y_f))`, with the rest 0, and `n_parties_f[col] = P`.

**Guards.** Each failure is an error, "R0 input: <check>":
- the column names of `y_n` and `y_k` are identical to `party_order`, and `P == length(party_order)`;
- `house_n` and `house_k` lie in 1..H, and house 1 is `Kosning`;
- `mu_gamma_scale` has length P − 1 and every value is finite and in [0, 1];
- `x_f`, `incumbent_years`, `vnv` and `growth` have `rownames(y_f)` and D_f + 1 columns, as does `index_f`;
- `rownames(y_f)[index_f[1:P, col]]` is identical to `party_order` (the 2021 study's S/B-swap guard);
- `n_parties_f[col] == P`;
- K = 6; `n_pred_k` is a vector of positive integers; `obs_k` lies in 1..`N_obs_k`; every row of `constituency_weights` sums to 1 (to 1e-12);
- `pred_y_time_diff ≥ 1` and `days_between_stjornarslit_and_election ≥ 1`;
- no NA in any field;
- every national and kjördæmi date is on or before F.

- [ ] **Step 1: Write `harness/configs/r0.json`** with the four keys above (written with `jsonlite`).
- [ ] **Step 2: Write the failing tests.** Build every `TARGETS` cell once per file (`inp <- read_inputs()`); all tests call `skip_without_snapshot()`.

```r
r0 <- if (snapshot_present()) lapply(seq_len(nrow(TARGETS)), \(i) r0_stan_data(info_set(TARGETS$E[i], TARGETS$F[i], inp)))
at <- function(E) r0[which(TARGETS$E == as.Date(E))]

test_that("the hedge scale is 1/sqrt(n_e), with no correction for a party new to the window", {
  for (x in at("2024-11-30")) expect_equal(unname(x$data$mu_gamma_scale), c(rep(1 / sqrt(3), 7), 1 / sqrt(2), 1))
  for (x in at("2021-09-25")) expect_equal(unname(x$data$mu_gamma_scale), c(rep(1 / sqrt(2), 7), 1, 0))
  for (x in at("2017-10-28")) {
    expect_equal(unname(x$data$mu_gamma_scale), c(rep(1, 7), 0))
    expect_equal(x$meta$party_order, c("Annað", setdiff(POLL_PARTIES, c("Annað", "Sósíalistaflokkurinn"))))
  }
})

test_that("expected valid votes are the previous election's, by name", {
  nk <- function(E) at(E)[[1]]$meta$n_pred_k
  expect_equal(names(nk("2024-11-30")), R0_KJORDAEMI)
  expect_equal(unname(nk("2024-11-30")), c(35118, 35504, 58608, 17251, 23536, 29713))
  expect_equal(unname(nk("2021-09-25")), c(35878, 35721, 55740, 17252, 23611, 28057))
  expect_equal(unname(nk("2017-10-28")), c(34988, 34880, 53301, 16760, 22703, 27016))
})
```

  Further tests:
  - In every cell, `n_e` equals the number of `kosning_nat` rows with n > 0 for each party (the entry table and the election rows agree).
  - In every cell, `kjordaemi_levels` is identical to `R0_KJORDAEMI`, and `constituency_weights_pred` equals `n_pred_k / sum(n_pred_k)`.
  - In every cell, `days_between_stjornarslit_and_election` is 47. `month_before_election` is 1 exactly on the date levels within 47 days before the next election in `ELECTIONS[ELECTIONS <= E]` (an independent computation).
  - The newcomers are {Miðflokkurinn} in 2017 and {Sósíalistaflokkurinn} in 2021, with none in 2024. Each newcomer's `x_f[, col]` equals `qlogis(debut) − c`, with debut 0.0715 and 0.072 respectively.
  - The S/B-swap identity holds, and the non-newcomers' `x_f[, col]` has mean 0 (1e-12).
  - At the 2021 f180 cell, at both 2017 cells and at the 2024 f180 cell, the kjördæmi rows are only the previous election's six `Kosning` rows (N_k = 6). At the 2024 eve, N_k is 6 × (1 + the number of admitted kjördæmi polls dated after 2021-09-25).
  - `active_index_d` lists every party at the last date level. At the 2021 cells, Sósíalistaflokkurinn is absent from every date level before its first poll, 2018-10-16.
- [ ] **Step 3: Run them and confirm they fail** (`tests/run.R r0-inputs`).
- [ ] **Step 4: Implement the builder.**
- [ ] **Step 5: Run the tests and confirm they pass,** then run the full suite.
- [ ] **Step 6: A one-off fidelity check** (scratchpad script, not committed). It compares field by field:
  - `r0_stan_data()` at (2021-09-25, 2021-03-29) against the 2021 study's frozen p10 f180 input (`runs/inputs/stan_data_p10_f180.json`);
  - `r0_stan_data()` at (2024-11-30, 2024-11-29) against the overdisp study's frozen base-eve input (`runs/inputs/stan_data_base_eve.json`).

  Every difference must be explained by one of these:
  - the Phase 0 availability rule;
  - the poll-n and fundamentals corrections;
  - the economy vintage (the 2021 study's −7.8 growth override);
  - the n_pred_k or weights rule;
  - house or kjördæmi order;
  - the newcomer rule;
  - the n_e = 0 hedge;
  - the new fields.

  An unexplained difference is a bug: stop and report it. List the differing fields with their reasons in the commit body, naming neither study's folder.
- [ ] **Step 7: Commit** with the message "harness: R0 inputs, production's prepare_stan_data mapped to E".

---

### Task 9: The R0 Stan file and its hook tests

**Files:**
- Create: `Stan/polling_and_fundamentals_kjordaemi_r0.stan`, a copy of `Stan/polling_and_fundamentals_kjordaemi.stan` with three hooks.
- Create: `tests/harness/test-r0-stan.R`.
- Modify: `Stan/CLAUDE.md` (inventory row: "Harness arm R0: the production model with harness hooks only; used by `R/harness/fit_cell.R`").

**Interfaces:**
- Consumes: `r0_stan_data()` (Task 8).
- Produces: the Stan file. Its data block is master's plus `mu_gamma_scale`, `n_active_d` and `active_index_d`. Its generated quantities add `pi_eday` (`vector[P]`) and `pi_kd` (`array[K] vector[P]`). Every block from `transformed data` to `model` is byte-identical to master's.

**The hooks:**
- **H1, the hedge scale as data.** Append to the data block:
  ```stan
  vector<lower = 0, upper = 1>[P - 1] mu_gamma_scale;   // 1/sqrt(n_e); 0 for a party new to the window
  array[D] int<lower = 1, upper = P> n_active_d;        // parties active at each date level
  array[D, P] int<lower = 0, upper = P> active_index_d; // their columns, 0-padded
  ```
  In generated quantities, replace the two lines `mu_gamma_pred[1:(P - 3)] /= sqrt(3.0);` and `mu_gamma_pred[P - 2] /= sqrt(2.0);` with `mu_gamma_pred = mu_gamma_pred .* mu_gamma_scale;`. It must stay before `mu_gamma_pred *= mu_gamma_weight;`.
- **H2, the `pi_eday`/`pi_kd` outputs.** These are deterministic and use no RNG. Declare them after `delta_pred` is computed:
  ```stan
  vector[P] pi_eday;
  array[K] vector[P] pi_kd;
  {
    vector[P] eta_e;
    eta_e[2:P] = beta[, D + pred_y_time_diff] + mu_gamma_pred;
    eta_e[1] = -sum(eta_e[2:P]);
    pi_eday = softmax(eta_e);
    for (k in 1:K) {
      eta_e[2:P] = beta[, D + pred_y_time_diff] + delta_pred[, k] + mu_gamma_pred;
      eta_e[1] = -sum(eta_e[2:P]);
      pi_kd[k] = softmax(eta_e);
    }
  }
  ```
- **H3, gathering by party identity.** This uses the likelihood's own pattern: rebuild the full sum-to-zero P-vector, then gather.
  - In the `y_rep_national` loop over `d in 1:D`: `eta_d[2:P] = beta[, d] + mu_gamma_pred; eta_d[1] = -sum(eta_d[2:P]);`, then `cols = active_index_d[d, 1:n_active_d[d]]` and `y_rep_national[d, cols] = multinomial_logit_rng(eta_d[cols], n_pred);`.
  - In the `y_rep_k` loop: `eta_k[2:P] = beta[, date_k[n]] + gamma[, house_k[n]] + delta[, constituency_k[n]] + mu_gamma_pred;` and `eta_k[1] = -sum(eta_k[2:P]);`. Then `cols = party_index_k[n, 1:n_parties_k[n]]`, and either `dirichlet_multinomial_rng(softmax(eta_k[cols]) * phi[house_k[n] - 1], sum(y_k[n, ]))` or `multinomial_logit_rng(eta_k[cols], sum(y_k[n, ]))` into `y_rep_k[n, cols]`.
  - The future rows and `y_pred_constituency` are unchanged; they already use all P parties.

- [ ] **Step 1: Write the failing tests.**
  - The first is always on: "the r0 file differs from master only in the data block and generated quantities". The text from `transformed data {` up to `generated quantities {` is identical in both files. R0's data block is master's plus the three declarations, compared after stripping whitespace and comments.
  - The rest are slow (`skip_unless_slow()`, `skip_without_snapshot()`). They write R0's data at the 2024 eve to a temp JSON. Master reads the same JSON, because CmdStan ignores fields a model does not declare. Both models are compiled with `compile_model_methods = TRUE`.
  - **The log density is master's.**
    - Fit R0 for 1 chain at `iter_warmup = 20`, `iter_sampling = 10`, `seed = 1`, `init = 0` and `max_treedepth = 6`, which is enough to get 10 points.
    - Fit master for 1 chain at 1 + 1 iterations, only to attach its methods.
    - For each R0 draw, `u <- fit_r0$unconstrain_draws()`. Then `fit_master$log_prob(u)` equals `fit_r0$log_prob(u)`, and `grad_log_prob` agrees likewise, with `tolerance = 1e-10`.
  - **The hooks compute what they claim,** on R0's draws:
    - `mu_gamma_pred` equals `mu_gamma_weight * mu_gamma_scale * c(mu_gamma, -sum(mu_gamma))` (1e-12);
    - `pi_eday` equals `softmax(c(-sum(e), e))` with `e = beta[, D + pred_y_time_diff] + mu_gamma_pred`, and `pi_kd` equals the same with `delta_pred[, k]` added (1e-10);
    - every `y_rep_k[n, ]` is 0 outside `party_index_k[n, 1:n_parties_k[n]]` and sums to `sum(y_k[n, ])`;
    - every `y_rep_national[d, ]` with d ≤ D is 0 outside `active_index_d[d, 1:n_active_d[d]]`, and every row sums to `n_pred`;
    - every `y_pred_constituency[k, ]` sums to `n_pred_k[k]`.
  - **The gather check catches master.** The same `y_rep_k` and `y_rep_national` zero-pattern checks fail on master's draw. This works because admitted 2024 kjördæmi polls drop a party in the interior (Gallup's dropped Annað), and master slices by position. If master passes, stop and report: the check would prove nothing.
- [ ] **Step 2: Run them and confirm they fail** (`HARNESS_SLOW=1 … tests/run.R r0-stan`). The R0 file does not exist yet.
- [ ] **Step 3: Write the Stan file.** Copy master byte for byte, then apply H1–H3.
- [ ] **Step 4: Run the tests and confirm they pass,** then run the full suite. Note the compile and test time in the commit body.
- [ ] **Step 5: Commit** with the message "Stan: R0, the production model with the harness hooks only".

---

### Task 10: R0 extraction, the cell runner and the queue

**Files:**
- Modify: `R/harness/r0.R`, adding the extraction half.
- Create: `R/harness/arms.R`, `R/harness/fit_cell.R` (a plain script, like `build_phase0.R`), `R/harness/queue.sh`, `harness/runs/tier0.txt`, `tests/harness/test-r0-extract.R`, `tests/harness/test-fit-cell.R`.
- Modify: `harness/README.md` ("Runs": the runner and queue commands).

**Interfaces:**
- Produces (`r0.R`):
  - `R0_PARAM_VARIABLES`: `mu_gamma`, `sigma_gamma`, `sigma_delta`, `mu_log_sigma`, `tau_log_sigma`, `tau_stjornarslit`, `log_phi`, `mu_phi`, `sigma_phi`, `beta_lag_f`, `beta_inc_years_f`, `beta_vnv_f`, `beta_growth_f`, `phi_f_inv`, `L_Omega`, `sigma`, `tau_f`, `mu_pred`, `mu_gamma_weight`, `mu_gamma_pred`, `delta_pred`.
  - `r0_extract(draws, meta, dir)` → the written paths, invisibly. `draws` is a posterior `draws_df` holding `y_pred_constituency`, the row `y_rep_national[eday_index, ]`, `pi_eday` and `R0_PARAM_VARIABLES`. It writes four files:

    | File | Columns | Source |
    |---|---|---|
    | `eday_kjordaemi.parquet` | `.chain`, `.iteration`, `.draw` (int), `kjordaemi`, `flokkur` (chr), `votes` (int) | `y_pred_constituency[k, p]` → `meta$kjordaemi_levels[k]`, `meta$party_order[p]` |
    | `eday_national.parquet` | `.chain`, `.iteration`, `.draw`, `flokkur`, `share` | `y_rep_national[eday_index, p] / n_pred` |
    | `latent_national.parquet` | the same columns | `share` is `pi_eday[p]` |
    | `param_draws.parquet` | `.chain`, `.iteration`, `.draw`, then one column per element of `R0_PARAM_VARIABLES` | |

    Labels come from the indices parsed out of the variable names (`[k,p]`), never from column positions.
- Produces (`arms.R`):
  - `ARMS`: `list(r0 = list(stan_file = "Stan/polling_and_fundamentals_kjordaemi_r0.stan", config = "harness/configs/r0.json", build = <r0_stan_data>, extract = <r0_extract>, variables = <the variables r0_extract reads>))`.
  - `arm(name)` → that entry; an error for an unknown arm.
- Produces (`fit_cell.R`), run as `LC_ALL=en_US.UTF-8 Rscript R/harness/fit_cell.R <arm> <E> <F> <seed> <iter_warmup> <iter_sampling>`. It stops unless the locale is UTF-8 and the working directory is the project root, then:
  1. `info <- info_set(E, F)`, `validate_inputs(info)`, `built <- arm$build(info, read config)`.
  2. Compute the data sha of the canonical JSON (written to a temp file first) and `expected_meta()`.
  3. If `run_dir(id)/meta.json` exists and `meta_mismatch()` is empty, print `SKIP <id> (meta.json matches)` and exit 0. If the directory exists but does not match, or has no `meta.json`, call `retire_run()` and print `STALE <id> <mismatched keys>`.
  4. `write_frozen_inputs()`. The inputs meta records `built$meta`, the expected keys, E, F, the arm, `git rev-parse HEAD`, the sha256 of `harness/snapshot_manifest.csv`, and the CmdStan and cmdstanr versions.
  5. `cmdstan_model(stan_file)$sample()` on `dir/inputs/stan_data.json`, with `SAMPLER`, the seed and the iterations, `refresh = 50` and `output_dir = dir/cmdstan`.
  6. Check that the fit has 4 chains and 4 × `iter_sampling` draws, and re-hash the Stan file and the data. If any check fails, stop with no `meta.json`.
  7. `arm$extract()`. Then:
     - the scored quantities as a `draws_array`: the national shares from `kd_to_nat(eday_kjordaemi)` and every kjördæmi cell share;
     - `summarise_diagnostics()` over every `parameters`-block variable (`model$variables()$parameters`) plus the scored quantities;
     - `key_diagnostics.csv` (`variable`, `rhat`, `ess_bulk`, `ess_tail`) for the scored quantities and `R0_PARAM_VARIABLES`;
     - `diagnostics_gate()`.
  8. Write `meta.json` last, so its presence means the run is complete. Print `diag_line()`, then `DONE <dir>`, then delete the CmdStan CSVs.
- Produces (`queue.sh`), run as `zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>`:
  - **Run list.** Whitespace-separated `lane arm E F seed`; `#` lines are comments.
  - **Guard.** It refuses to start if `pgrep -f -- '--file=R/harness/fit_cell.R'` matches, unless `FORCE=1`.
  - **Preflight.** `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R > $LOGS/preflight.log 2>&1`. Its exit status is checked bare, with no pipe. On failure the queue logs and exits 1.
  - **Lanes.** Lanes A and B run in the background, then `wait`. Each line becomes `LC_ALL=en_US.UTF-8 Rscript R/harness/fit_cell.R … > $LOGS/<id>.log 2>&1`.
  - **Logging.** The status is captured as `rc=$?` straight after `Rscript`, before any other command. Then `end <id> rc=<rc> <first DIAG|SKIP|STALE line>` is appended to `$LOGS/queue.log`, and `QUEUE FINISHED` at the end.
  - `LOGS` is `results/harness/logs/`.
  - `DRY_RUN=1` skips the preflight and the fits, and prints `would run <lane> <arm> <E> <F> <seed>` per line.
- `harness/runs/tier0.txt`:
  ```
  # lane arm E F seed
  A r0 2024-11-30 2024-11-29 1101
  B r0 2021-09-25 2021-03-29 1102
  ```

- [ ] **Step 1: Write the failing tests.**
  - `test-r0-extract.R` uses a synthetic `posterior::draws_df`: 4 chains × 4 iterations, P = 3, K = 2. Each `y_pred_constituency[k,p]` is `1000 * k + p`. The `meta` carries `party_order = c("Annað", "X", "Y")` and `kjordaemi_levels = c("Suður", "Norðvestur")`. Assert:
    - the `votes` of (Norðvestur, Y) are 2003 in every draw;
    - the column names and types are exactly as listed;
    - `.draw` runs 1..16;
    - `eday_national$share` equals `y_rep_national[eday, p] / n_pred`.
  - `test-fit-cell.R` covers the queue and the runner:
    - The queue's dry run prints exactly the two Tier 0 lines (fast).
    - A slow test runs `fit_cell.R r0 2024-11-30 2024-11-29 1 10 10`, with `HARNESS_RESULTS` pointing at a temp dir. It expects exit 0, the four parquets, `key_diagnostics.csv`, a `meta.json` with every `expected_meta()` key, read-only inputs, and one `DIAG run=` line.
    - The same command a second time prints `SKIP`.
    - With the seed changed in that run's `meta.json` by hand (chmod first), it prints `STALE … seed` and leaves the old run under `_stale/`.
- [ ] **Step 2: Run them and confirm they fail** (`tests/run.R "r0-extract|fit-cell"`).
- [ ] **Step 3: Implement the extraction, `arms.R`, `fit_cell.R` and `queue.sh`.**
- [ ] **Step 4: Run the tests and confirm they pass,** then run the full suite.
- [ ] **Step 5: Commit** with the message "harness: R0 extraction to the §5.5 parquets, the cell runner and the two-lane queue".

---

### Task 11: Tier 0, the report and the exit check (exit criterion 3)

**Files:**
- Create: `R/harness/tier0_report.R` (a plain script).
- Generate and commit: `harness/tier0/r0.md`.
- Create: `tests/harness/test-tier0-report.R`.
- Modify: `harness/README.md` ("Tier 0"), `CLAUDE.md` (Testing: `HARNESS_SLOW=1` runs the slow tests and needs CmdStan and the self-test inputs), `R/CLAUDE.md` (check that every new module has its row).

**Interfaces:**
- Produces (`tier0_report.R`), run as `LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R <arm> <run_list> [out]`, with `out` defaulting to `harness/tier0/<arm>.md`. It reads only each run's `meta.json` and `key_diagnostics.csv`. It writes one table row per run with these columns:
  - cell (E, F, tag), seed, and the iterations;
  - minutes, divergences, treedepth hits (count, and share of 4 × sampling), and the deepest tree reached;
  - min E-BFMI;
  - max R-hat (parameters; scored quantities);
  - min bulk and tail ESS (parameters; scored quantities);
  - ESS/min (min scored bulk ESS ÷ minutes; min parameter bulk ESS ÷ minutes);
  - both minimum bulk ESS values scaled to 2000 sampling iterations (× 2000 / `iter_sampling`). The report labels these a linear projection; they inform Decision 5, not the gate;
  - the §5.7 gate verdict with its reasons. This is informational: Tier 0 is not gated.

  It also records the CmdStan version, `sysctl -n hw.ncpu`, and the measurement date. No share, mean, quantile or score appears in it.

- [ ] **Step 1: Write the failing test.** On a fixture run directory, with `meta.json` and `key_diagnostics.csv` written by the test, the report contains the row's values. It matches none of `share|mean|median|crps|mae`, case-insensitive, outside the column headings it defines.
- [ ] **Step 2: Implement `tier0_report.R`,** and run the test to confirm it passes.
- [ ] **Step 3: Get ready.**
  - Check `ListAgents`, and ask any peer that may be timing a benchmark: Tier 0's ESS/min is a timing.
  - The two lanes use 8 cores. If `sysctl -n hw.ncpu` is below 10, put both runs in lane A and say so in the report.
- [ ] **Step 4: Launch Tier 0** from a normal Bash call, so it survives the session: `nohup zsh R/harness/queue.sh harness/runs/tier0.txt 200 200 > results/harness/logs/tier0_nohup.log 2>&1 &`.
  - Wait with a Monitor until `results/harness/logs/queue.log` holds `QUEUE FINISHED`. Do not poll in a sleep loop.
  - If a run's log has no `DONE` line, read the log before rerunning anything.
- [ ] **Step 5: Write the report** with `LC_ALL=en_US.UTF-8 Rscript R/harness/tier0_report.R r0 harness/runs/tier0.txt`. Read it, and check that it holds two rows and no forecast quantity.
- [ ] **Step 6: Run the exit check:**
  - `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R` gives FAIL 0 and SKIP 0, with the snapshot and the self-test inputs present. Record the pass count.
  - Exit criterion 1: `tests/run.R selftest` passes with SKIP 0.
  - Exit criterion 2:
    - `tests/run.R "freeze|scorer"` passes;
    - `score_cell(run_dir("r0_2024-11-30_2024-11-29_s1101"), truth = function(...) stop("truth read"))` on the real Tier 0 run fails with "no freeze", not with "truth read".
  - Exit criterion 3: `harness/tier0/r0.md` is written from both runs' `meta.json`, and both logs end in `DONE`.
  - Committed text. `<base>` is the commit this plan's execution started from. Write the excluded names to a scratch file, then check that the branch's files and its commit messages hold none of them. Both greps print nothing:
    ```bash
    X=<scratchpad>/excluded.txt
    sed -nE 's:^/([^/]+)/$:\1:p' "$(git rev-parse --git-common-dir)/info/exclude" > "$X"
    git diff --name-only <base>..HEAD | xargs git grep -nFf "$X" --
    git log <base>..HEAD --format=%B | command grep -nFf "$X"
    ```
- [ ] **Step 7: Commit** with the message "harness: R0 Tier 0 diagnostics; Phase 1 exit criteria met". The body records:
  - the test count;
  - that the self-test passed;
  - for each Tier 0 run: minutes, max R-hat, min scored bulk ESS, divergences and treedepth hits.

  No forecast quantity goes in the body. Then record the Phase 1 state in the vault handoff (`/done`).

---

## Later plans (not part of this plan)

- **Plan 3 (Phase 2).** Covers:
  - the §6.2 grid checks in `validate_inputs()`;
  - `Stan/fundamentals_v2.stan` and `Stan/forecast_v2.stan`;
  - a V2 entry in `ARMS`, with its builder and extractor writing the same parquets;
  - simulation-based recovery;
  - the permuted-order refit;
  - a check on the walk's parameterisation before Tier 0:
    - §6.2 keeps parties that do not exist yet in the sum-to-zero state, while every softmax runs over A_t.
    - So shifting the present parties together, with the absent ones absorbing the shift, leaves the likelihood flat: a flat subspace of dimension |N(t)|.
    - This is harmless under a non-centred walk. A centred Q-basis walk (a §6.9 option) would meet the kind of ridge that made the sampler study's centred walks 7.6–17× slower.
    - §6.2's alternative avoids it: start a newcomer's component at entry.
  - V2 Tier 0, read against `harness/tier0/r0.md`.
- **Plan 4 (Phases 3–6, runbook).** Covers:
  - `FREEZE.md` and its sha list (the arms, the R0 config including this plan's Decisions 1–3 and 5, priors, dates, scores, margins, the per-cell poll lists, the newcomer, house and coalition tables);
  - Tier 1 and Tier 2 through the queue, scored by `score_cell()` and the seed-to-seed SE rule;
  - the gate's one rerun at twice the sampling iterations, and counting a second failure against the arm (§5.7);
  - `DECISION.md`;
  - the `CONFIRM.md` writer and Tier C, which lift Task 6's refusal of 2017 cells;
  - Tier 3 and 3b;
  - the vault decision memo.
