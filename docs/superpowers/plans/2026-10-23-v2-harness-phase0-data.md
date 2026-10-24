# v2 Harness Phase 0 (Data Work) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the frozen, leak-checked inputs that every harness arm reads. These are official results, poll availability, fundamentals target rows, layouts and the tables D1–D9. Phase 0 ends when official seats are reproduced for 2016–2024 and `validate_inputs()` rejects planted leaks.

**Architecture:** The live `data/`, `data-raw/` and backtest source files are copied once into a gitignored, sha256-checked snapshot (`harness/snapshot/`). Box modules under `R/harness/` read only that snapshot plus committed source tables in `harness/sources/`. They write committed, deterministic tables (`harness/truth/`, `harness/availability.csv`, `harness/entry_table.csv`). `info_set(E, F)` bundles one cell's admissible information, and `validate_inputs()` refuses anything outside it. A testthat 3 suite under `tests/harness/` pins every rule.

**Tech Stack:** R with box modules, testthat 3, readxl, jsonlite, digest, withr, dplyr/tidyr/readr.

**Spec:** the design note in the Metill vault, `/Users/brynjolfurjonsson/Obsidian/Metill/Kosningar/Forecast model/2026-10-23 — v2 forecast model and two-election harness (design).md`. The relevant sections are §3 (constraints), §5.2–§5.5 (cells, leakage rules, arms, adapter contract), §6.1, §6.4 and §6.6 (window, epochs, newcomers, π̄), §7 (D1–D9) and §8 (Phase 0 exit). Read §5.3 and §7 before Task 1.

**Scope:** Phase 0 only. Phases 1–6 get their own plans once this one lands, because each consumes this plan's interfaces and the measurements of the phase before it (see *Later plans* at the end).

## Global Constraints

- **The repo is public** (github.com/metill-is/kosningaspa).
  - Nothing derived from the 1983–2017 ÍSKOS waves goes into code, comments, commit messages or committed files. Phase 0 uses no ÍSKOS input.
  - No git-excluded folder of the main checkout is committed or named in committed text.
  - Hagstofa results are public and may be committed.
- **The harness never reads the live `data/`.** Every input comes from `harness/snapshot/`, verified against `harness/snapshot_manifest.csv`, or from committed files under `harness/`.
- **No forecast is compared with a result in this phase.** There is no 2021/2024 scoring before Phase 4 and no 2017 scoring before Phase 5. Building truth tables and 2017 info sets is allowed; scoring is not.
- **Availability order** (§5.3 rule 1; amended 23 Oct 2026): fieldwork correction end + 1 → `lokadagur + 1` → Wikipedia fieldwork end + 1 → recorded publication date → `date` + that house's longest observed fieldwork + 1; kjördæmi polls: correction → national twin → recorded publication date → house max. Floors: a recorded publication date and, for a kjördæmi poll, its national twin's date never let a poll in earlier (`available_from` is the latest of the order's date and the floors). Publication dates are recorded for every national poll a forecast admits with `available_from` in (F − 45, F]. **No midpoint fallback.** `date` is the fieldwork midpoint, so no non-election poll may get `available_from <= date`. A poll is admitted at F iff `available_from <= F`.
- **No house-specific cuts:** there are no Gallup filters for any arm.
- **Party set.**
  - The ten poll parties use the labels as in the data: Sjálfstæðisflokkurinn, Framsóknarflokkurinn, Samfylkingin, Vinstri Græn, Píratar, Viðreisn, Flokkur Fólksins, Miðflokkurinn, Sósíalistaflokkurinn, Annað.
  - Every other list is Annað, including Björt framtíð (letter A in 2016/17) and Lýðræðisflokkurinn (L, 2024).
  - The 2017 cells have nine parties (no Sósíalistaflokkurinn).
- **Annað wins no seat** in a forecast (`ineligible = "Annað"`). The official-seat reproduction runs on unfolded lists with no ineligible list, because Björt framtíð won 4 seats in 2016.
- **Join kjördæmi and parties by name, never by position.** Production's expected-valid-votes vector had Reykjavík Suður and Norður swapped by a positional reorder.
- **Fixed constants.**
  - Window start: 2016-01-01.
  - Elections in the window: 2016-10-29, 2017-10-28, 2021-09-25, 2024-11-30. The election before 2016 is 2013-04-27.
  - `campaign_days` = 47.
  - Forecast cells (E: F):
    - 2021-09-25: 2021-03-29, 2021-08-09, 2021-09-11, 2021-09-24;
    - 2024-11-30: 2024-06-03, 2024-10-14, 2024-11-16, 2024-11-29;
    - 2017-10-28 (confirmatory): 2017-10-14, 2017-10-27.
- **Locale.** Every R run uses `LC_ALL=en_US.UTF-8`. Under the C locale, R misreads literal Icelandic identifiers and the "Annað" match fails.
- **Code style.**
  - Box modules use `#' @export` roxygen tags and `box::use()` inside each function, as in `R/election_utils.R` and `R/flow_prior.R`.
  - New modules live in `R/harness/`, tests in `tests/harness/test-*.R`.
  - British spelling in prose.
- **Write CSVs with `readr::write_csv`** (UTF-8). Dates are ISO `YYYY-MM-DD`, rows are sorted deterministically, and a rebuild must produce a byte-identical file.
- **Commits.**
  - Before each one, run `git fetch`, then `git branch --show-current` (expect `claude/kosningaspa-model-improvements-213d5c`), then `ListAgents`.
  - Every message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
  - Never use bare `git stash`.
- **Source root.** The snapshot is copied from the main checkout, `/Users/brynjolfurjonsson/kosningaspa/`. The worktree has no `data/` or `data-raw/`.

## Review Focus

1. **A list that did not run in a kjördæmi** (`.` in the Hagstofa sheets) must get 0 votes and 0 seats there, not be dropped. A dropped row shifts D'Hondt quotients and only the seat test catches it. *Test:* Task 3's `ran` assertions and the per-kjördæmi seat test.
2. **`lokadagur` is an ISO datetime string** (`2024-11-13T00:00:00Z`), not a date. A naive parse yields NA, which silently pushes the poll down to a weaker rule, or a timezone shift moves it a day. *Test:* Task 5 `parse_lokadagur`.
3. **One Wikipedia row could match two polls,** for example two same-house polls a few days apart. Matching must be one-to-one, so a single fieldwork end can't admit two polls. *Test:* Task 5's one-to-one test.
4. **A poll available exactly on F** is admitted, and one available on F + 1 is not. An off-by-one here moves the decision cells. *Test:* Task 9's boundary test.
5. **A misdated poll can enter early through the fallback rule.** Prósent "2024-09-26" is probably a misdated duplicate of an 18 Oct poll, and on rule 4 alone it would be admitted at F = 2024-10-14. *Test:* Task 5's real-snapshot assertion after `fieldwork_corrections.csv`.

---

### Task 1: Test runner, constants and the frozen snapshot

**Files:**
- Create: `tests/run.R`, `tests/harness/helper-root.R`, `R/harness/constants.R`, `R/harness/inputs.R`, `tests/harness/test-inputs.R`, `harness/README.md`
- Generate and commit: `harness/snapshot_manifest.csv`
- Modify: `.gitignore` (add `harness/snapshot/`)

**Interfaces:**
- Produces (constants.R):
  - `project_root()`, computed once at module load from `box::file("..", "..")`, so it is right whatever the working directory (testthat runs tests from `tests/harness/`), and `harness_path(...)` = `file.path(project_root(), "harness", ...)`. **Every default path in `R/harness/` is built with `harness_path()`**, never relative to the working directory;
  - `WINDOW_START`, a Date, 2016-01-01;
  - `ELECTIONS`, a Date vector of the four window elections;
  - `previous_election(E)`, a Date: 2016-10-29 → 2013-04-27, and every later election → the one before it; any other date is an error;
  - `TARGETS`, a tibble (E, F, tag, confirmatory) with the 10 cells above; tag ∈ f180, f47, f14, eve, and confirmatory = TRUE for 2017;
  - `POLL_PARTIES`, a character vector of the 10 labels, in the order listed under Global Constraints;
  - `LETTER_PARTY`, a named character vector: B Framsóknarflokkurinn, C Viðreisn, D Sjálfstæðisflokkurinn, F Flokkur Fólksins, J Sósíalistaflokkurinn, M Miðflokkurinn, P Píratar, S Samfylkingin, V Vinstri Græn. Every other letter maps to Annað through `party_of_letter(letter)`;
  - `KJORDAEMI`, a tibble (kjordaemi, hagstofa, code). The three columns hold the poll labels (Reykjavík Suður, Reykjavík Norður, Suðvestur, Suður, Norðaustur, Norðvestur), the Hagstofa sheet labels (Reykjavíkurkjördæmi suður, Reykjavíkurkjördæmi norður, Suðvesturkjördæmi, Suðurkjördæmi, Norðausturkjördæmi, Norðvesturkjördæmi) and the KOS02018 codes (RS, RN, SV, SU, NA, NV).
- Produces (inputs.R):
  - `SNAPSHOT_FILES`, named source paths relative to the source root:
    - `polling_data` = `data/polling_data.csv`; likewise `constituency_data`, `fundamentals_data` and `economy_data` under `data/`;
    - `gild_2016`, `gild_2017`, `gild_2021` = `data-raw/hagstofa/althingiskosningar/gild_atkvaedi_<y>.xlsx`;
    - `thingsaeti_2016`, `thingsaeti_2017`, `thingsaeti_2021`, likewise;
    - `kos02018_2024` and `official_2024_kjordaemi`, the 2024 Hagstofa KOS02018 table and its fold to the poll parties. Superseded on 24 Oct 2026: both are committed under `harness/sources/hagstofa/` and are no longer snapshot files.
  - `snapshot_inputs(src_root, dest = harness_path("snapshot"), manifest = harness_path("snapshot_manifest.csv"))` returns a tibble (name, file, source, bytes, sha256). It errors if `dest` already exists, so re-snapshotting is a deliberate manual delete.
  - `verify_snapshot(dest, manifest)` returns `invisible(TRUE)` and errors on a missing file or a mismatched hash.
  - `snapshot_file(name, dest, manifest)` returns the absolute path after verifying that file.
- Produces (helper-root.R; testthat sources it into every test file):
  - `box::use(R/harness/constants[project_root, TARGETS, POLL_PARTIES, ELECTIONS, WINDOW_START], R/harness/inputs[snapshot_file])`, so every test sees these names;
  - `root(...)` returns `file.path(project_root(), ...)`;
  - `skip_without_snapshot()` skips with "no harness snapshot: run snapshot_inputs()" when `harness/snapshot_manifest.csv` or `harness/snapshot/` is absent.

- [ ] **Step 1: Write `tests/run.R`**

```r
# Runs the harness tests from the project root: LC_ALL=en_US.UTF-8 Rscript tests/run.R [filter]
stopifnot("run under LC_ALL=en_US.UTF-8" = isTRUE(l10n_info()[["UTF-8"]]))
options(box.path = normalizePath("."))
args <- commandArgs(trailingOnly = TRUE)
testthat::test_dir("tests/harness", filter = if (length(args)) args[1] else NULL,
                   stop_on_failure = TRUE)
```

- [ ] **Step 2: Write the failing tests** in `tests/harness/test-inputs.R`.

```r
box::use(R/harness/inputs[snapshot_inputs, verify_snapshot, SNAPSHOT_FILES],
         R/harness/constants[TARGETS, previous_election])

fake_src <- function() {
  src <- withr::local_tempdir(.local_envir = parent.frame())
  for (p in SNAPSHOT_FILES) { dir.create(dirname(file.path(src, p)), recursive = TRUE, showWarnings = FALSE)
    writeLines(p, file.path(src, p)) }
  src
}

test_that("snapshot_inputs copies and hashes every file", {
  src <- fake_src(); out <- withr::local_tempdir()
  m <- snapshot_inputs(src, file.path(out, "snap"), file.path(out, "manifest.csv"))
  expect_equal(nrow(m), length(SNAPSHOT_FILES))
  expect_equal(m$sha256[1], digest::digest(file = file.path(src, SNAPSHOT_FILES[[1]]), algo = "sha256"))
  expect_true(verify_snapshot(file.path(out, "snap"), file.path(out, "manifest.csv")))
})

test_that("verify_snapshot fails on a modified file", {
  src <- fake_src(); out <- withr::local_tempdir()
  snapshot_inputs(src, file.path(out, "snap"), file.path(out, "manifest.csv"))
  cat("x", file = file.path(out, "snap", "polling_data.csv"), append = TRUE)
  expect_error(verify_snapshot(file.path(out, "snap"), file.path(out, "manifest.csv")),
               "sha256 mismatch: polling_data.csv")
})

test_that("snapshot_inputs refuses an existing destination", {
  src <- fake_src(); out <- withr::local_tempdir(); dir.create(file.path(out, "snap"))
  expect_error(snapshot_inputs(src, file.path(out, "snap"), file.path(out, "m.csv")), "exists")
})

test_that("cells and previous elections are the design's", {
  expect_equal(nrow(TARGETS), 10)
  expect_equal(as.character(TARGETS$F[TARGETS$E == as.Date("2024-11-30")]),
               c("2024-06-03", "2024-10-14", "2024-11-16", "2024-11-29"))
  expect_equal(as.character(TARGETS$F[TARGETS$confirmatory]), c("2017-10-14", "2017-10-27"))
  expect_equal(previous_election(as.Date("2016-10-29")), as.Date("2013-04-27"))
  expect_equal(previous_election(as.Date("2024-11-30")), as.Date("2021-09-25"))
  expect_error(previous_election(as.Date("2020-01-01")))
})
```

- [ ] **Step 3: Run and confirm failure.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R inputs`. Expected: FAIL, because the module `R/harness/inputs` cannot be found.
- [ ] **Step 4: Implement `constants.R` and `inputs.R`** with the interfaces above. Snapshot files keep their basenames (`polling_data.csv`, `gild_atkvaedi_2016.xlsx`, …). Hash with `digest::digest(file =, algo = "sha256")`.
- [ ] **Step 5: Run and confirm pass.** Same command. Expected: 4 tests pass.
- [ ] **Step 6: Take the real snapshot.** Add `harness/snapshot/` to `.gitignore`. Run `LC_ALL=en_US.UTF-8 Rscript -e 'box::use(R/harness/inputs[snapshot_inputs]); print(snapshot_inputs("/Users/brynjolfurjonsson/kosningaspa"))'`. Expected: 14 rows and a committed-to-be `harness/snapshot_manifest.csv`.
- [ ] **Step 7: Write `harness/README.md`** in under 30 lines: what the harness is, a link to the spec path, the snapshot rule (gitignored, sha-checked, re-snapshot = delete by hand and re-run), the test command, and which files are committed versus generated.
- [ ] **Step 8: Commit** `tests/ R/harness/ harness/README.md harness/snapshot_manifest.csv .gitignore` with the message "harness: frozen input snapshot, constants and the test runner".

---

### Task 2: Seat layouts and seat-ineligible lists (D5)

**Files:**
- Modify: `R/election_utils.R`. Add `seats_layout_2021()` and `layout_for()`, and give `calculate_seats()` an `ineligible` argument (l.87 on).
- Create: `tests/harness/test-seats.R`.
- Modify: `R/CLAUDE.md` (the `election_utils.R` row) and `CLAUDE.md` (the Seat Allocation bullets).

**Interfaces:**
- `seats_layout_2021()` returns a tibble (kjordaemi, n_seats, n_jofnun): Reykjavík Suður 9+2, Reykjavík Norður 9+2, Suðvestur 11+2, Suður 9+1, Norðaustur 9+1, Norðvestur 7+1. The roxygen states it is the layout in force for the 2013–2021 elections.
- `layout_for(election)` takes a Date or an ISO string. It returns `seats_layout_2021()` for 2016-10-29, 2017-10-28 and 2021-09-25, and `seats_tibble()` for 2024-11-30. Any other date fails with "no seat layout for <date>".
- `calculate_seats(draw, layout = seats_tibble(), ineligible = character(0))`:
  - Lists named in `ineligible` take part in neither the kjördæmi D'Hondt nor the national adjustment D'Hondt.
  - Their votes still count in the 5% threshold's denominator and in the residual shares that place adjustment seats.
  - The default leaves today's behaviour unchanged. Use a `can_win` multiplier (0 for an ineligible list) on the quotients.

- [ ] **Step 1: Write the failing tests.**

```r
box::use(R/election_utils[calculate_seats, seats_layout_2021, seats_tibble, layout_for])

kd <- seats_layout_2021()$kjordaemi
draw_with <- function(shares) tidyr::expand_grid(kjordaemi = kd, flokkur = names(shares)) |>
  dplyr::mutate(votes = 10000 * shares[flokkur])

test_that("layouts hold 63 seats and match the elections", {
  expect_equal(sum(seats_layout_2021()$n_seats + seats_layout_2021()$n_jofnun), 63)
  expect_identical(layout_for("2017-10-28"), seats_layout_2021())
  expect_identical(layout_for(as.Date("2024-11-30")), seats_tibble())
  expect_error(layout_for("2013-04-27"), "no seat layout for 2013-04-27")
})

test_that("an ineligible list wins no seat and the rest still fill 63", {
  d <- draw_with(c(Annað = 0.30, D = 0.30, S = 0.25, B = 0.15))
  s <- calculate_seats(d, seats_layout_2021(), ineligible = "Annað")
  expect_equal(sum(s$seats[s$flokkur == "Annað"]), 0)
  expect_equal(sum(s$seats), 63)
  s0 <- calculate_seats(d, seats_layout_2021())
  expect_gt(sum(s0$seats[s0$flokkur == "Annað"]), 0)
})

test_that("the 5% threshold counts the ineligible list's votes", {
  # X has 4.8% of all valid votes but 5.3% of the votes outside Annað
  d <- draw_with(c(Annað = 0.094, X = 0.048, D = 0.458, S = 0.40))
  s <- calculate_seats(d, seats_layout_2021(), ineligible = "Annað")
  x <- s[s$flokkur == "X", ]
  expect_equal(sum(x$seats - x$kjordaemi_seats), 0)
})
```

- [ ] **Step 2: Run and confirm failure.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R seats`. Expected: FAIL (no `seats_layout_2021`).
- [ ] **Step 3: Implement** the three changes in `R/election_utils.R`.
- [ ] **Step 4: Run and confirm pass.** Same command. Expected: 3 tests pass.
- [ ] **Step 5: Update the docs.** In the `R/CLAUDE.md` row and in `CLAUDE.md` § Seat Allocation, list `seats_layout_2021()` and `layout_for()` and the `ineligible` argument. Keep each change to one bullet.
- [ ] **Step 6: Commit** with the message "seats: 2013–2021 layout, layout_for() and seat-ineligible lists".

---

### Task 3: Official results and the seat-reproduction test (D1)

**Files:**
- Create: `R/harness/truth.R`, `tests/harness/test-truth.R`, `harness/truth/README.md`.
- Generate and commit: `harness/truth/elections_official.csv`.

**Interfaces:**
- Consumes: `snapshot_file()`, `KJORDAEMI`, `party_of_letter()`, `previous_election()`, `layout_for()`, `calculate_seats()`.
- Produces:
  - `parse_gild_atkvaedi(path, election)` returns a tibble (election, kjordaemi, list_letter, list_name, votes, ran).
    - Sheet layout: row 3 holds the kjördæmi headers in columns 3–8; party rows run from row 4 to the first NA in column 2.
    - Column 2 is `Name  (Letter)`, and the letter comes from the parentheses.
    - A `.` cell means 0 votes with `ran = FALSE`. Kjördæmi labels map through `KJORDAEMI$hagstofa`.
  - `parse_thingsaeti(path, election)` returns a tibble (election, kjordaemi, list_letter, seats). It uses the same layout, and `.` means 0.
  - `parse_kos02018(path)` returns the same columns as `parse_gild_atkvaedi` plus `seats`, for election 2024-11-30.
    - The file is JSON-stat with dimensions Atriði (1 = votes, 3 = seats) × Flokkur × Kjördæmi, in row-major order, 168 values.
    - Drop the totals: Flokkur code `A` is "Alls", **not** Björt framtíð, and Kjördæmi `AL` is the national total.
    - Take the letter from the label's parentheses, never from the code. `ran = votes > 0` for 2024.
  - `build_truth()` returns a tibble (election, kjordaemi, list_letter, list_name, flokkur, votes, seats, ran, source).
    - Rows are one per election × kjördæmi × list, for 2016, 2017, 2021 and 2024.
    - `flokkur` comes from `party_of_letter(list_letter)`; `source` is the snapshot file name.
  - `write_truth(path = harness_path("truth", "elections_official.csv"))`.
  - `fold_truth(truth)` returns a tibble (election, kjordaemi, flokkur, votes, seats), summed over lists.
  - `national_shares(truth, election)` returns a named numeric over `POLL_PARTIES`. Each value is the party's share of the national valid vote, and NA for a party with no list at that election.
  - `prev_national_shares(E, truth, fundamentals)` gives π̄ for §6.6:
    - for E = 2016-10-29, the 2013 rows of `fundamentals_data.csv` mapped to poll parties (Framsókn → Framsóknarflokkurinn; Björt framtíð and Annað → Annað), divided by their sum, NA for parties absent in 2013;
    - otherwise `national_shares(truth, previous_election(E))`.

- [ ] **Step 1: Write the failing tests** (each calls `skip_without_snapshot()` first).

```r
box::use(R/harness/truth[build_truth, fold_truth, prev_national_shares],
         R/harness/inputs[snapshot_file], R/election_utils[calculate_seats, layout_for])
tr <- function() build_truth()

test_that("totals match the official valid votes and 63 seats", {
  skip_without_snapshot(); t <- tr()
  tot <- t |> dplyr::summarise(v = sum(votes), s = sum(seats), .by = election)
  expect_equal(tot$v, c(189648, 196259, 199730, 212470))
  expect_equal(tot$s, rep(63, 4))
})

test_that("Hagstofa sheets equal the constituency Kosning rows cell by cell", {
  skip_without_snapshot()
  kos <- readr::read_csv(snapshot_file("constituency_data"), show_col_types = FALSE) |>
    dplyr::filter(fyrirtaeki == "Kosning") |> dplyr::transmute(election = date, kjordaemi, flokkur, n)
  j <- dplyr::full_join(fold_truth(tr()) |> dplyr::filter(election < as.Date("2024-01-01")), kos,
                        by = c("election", "kjordaemi", "flokkur"))
  expect_equal(nrow(j), 180)
  expect_equal(j$votes, j$n)
})

test_that("2024 equals official_2024_kjordaemi.csv", {
  skip_without_snapshot()
  o <- readr::read_csv(snapshot_file("official_2024_kjordaemi"), show_col_types = FALSE)
  j <- dplyr::inner_join(fold_truth(tr()) |> dplyr::filter(election == as.Date("2024-11-30")), o,
                         by = c("kjordaemi", "flokkur"), suffix = c("", "_o"))
  expect_equal(nrow(j), 60); expect_equal(j$votes, j$votes_o); expect_equal(j$seats, j$seats_official)
})

test_that("the allocator reproduces every official seat, 2016-2024, on unfolded lists", {
  skip_without_snapshot(); t <- tr()
  for (e in unique(t$election)) {
    te <- t[t$election == e, ]
    s <- calculate_seats(dplyr::transmute(te, flokkur = list_letter, kjordaemi, votes), layout_for(e))
    got <- dplyr::inner_join(s, dplyr::transmute(te, flokkur = list_letter, kjordaemi, official = seats),
                             by = c("flokkur", "kjordaemi"))
    expect_equal(got$seats, got$official, label = paste("seats", e))
    tot <- dplyr::summarise(te, s = sum(seats), .by = kjordaemi) |> dplyr::arrange(kjordaemi)
    lay <- dplyr::mutate(layout_for(e), s = n_seats + n_jofnun) |> dplyr::arrange(kjordaemi)
    expect_equal(tot$s, lay$s, label = paste("kjördæmi totals", e))  # 2016/17 use the 2013–2021 layout
  }
  expect_equal(sum(t$seats[t$election == as.Date("2016-10-29") & t$list_letter == "A"]), 4)
})

test_that("lists that did not run keep a zero row", {
  skip_without_snapshot(); t <- tr()
  expect_true(all(t$votes[!t$ran] == 0)); expect_true(any(!t$ran))
  expect_equal(nrow(dplyr::count(t, election, list_letter) |> dplyr::filter(n != 6)), 0)
})

test_that("previous national shares feed the shock scaling", {
  skip_without_snapshot()
  f <- readr::read_csv(snapshot_file("fundamentals_data"), show_col_types = FALSE)
  p16 <- prev_national_shares(as.Date("2016-10-29"), tr(), f)
  expect_equal(unname(p16["Annað"]), (11.9 + 8.3) / 100.2)
  expect_equal(unname(p16["Sjálfstæðisflokkurinn"]), 26.7 / 100.2)
  expect_true(all(is.na(p16[c("Viðreisn", "Flokkur Fólksins", "Miðflokkurinn", "Sósíalistaflokkurinn")])))
  expect_false(is.na(prev_national_shares(as.Date("2024-11-30"), tr(), f)["Sósíalistaflokkurinn"]))
})
```

- [ ] **Step 2: Run and confirm failure.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R truth`. Expected: FAIL (no module `R/harness/truth`).
- [ ] **Step 3: Implement `truth.R`.** Read the xlsx with `readxl::read_excel(col_names = FALSE, .name_repair = "minimal")` and the JSON with `jsonlite::fromJSON(simplifyVector = TRUE)`.
- [ ] **Step 4: Run and confirm pass.** Same command. Expected: 6 tests pass.
  - **If the cell-by-cell or seat test fails, stop and report the differing cells.** That is a data finding for Brynjólfur, not something to code around.
  - Hagstofa is the authority; the constituency rows are what production read.
- [ ] **Step 5: Write `harness/truth/elections_official.csv`** with `write_truth()`. Then write `harness/truth/README.md` in under 15 lines: the sources (Hagstofa tables KOS02118b and friends from the xlsx footers, and KOS02018), the letter mapping, the reconciliation result, and the fact that it is public data.
- [ ] **Step 6: Commit** with the message "harness: official kjördæmi results 2016–2024 from Hagstofa, seats reproduced".

---

### Task 4: Fieldwork sources (data gathering)

**Files (all committed):**
- `harness/sources/wikipedia_2017_poll_fieldwork.csv`
- `harness/sources/wikipedia_2021_poll_fieldwork.csv`, a verbatim copy of an earlier fetch of the same Wikipedia table (`harness/sources/README.md`), extended if it lacks rows in the range below
- `harness/sources/wikipedia_2024_poll_fieldwork.csv`
- `harness/sources/wikipedia_house_map.csv` (wikipedia_house, fyrirtaeki; `fyrirtaeki` is NA for a house absent from the polls)
- `harness/sources/fieldwork_corrections.csv` (level, date, fyrirtaeki, end, source, note)
- `harness/sources/poll_publication.csv` (level, date, fyrirtaeki, published, source). Header only for now: no structured publication dates exist, and rule 2 reports 0 polls.
- `harness/sources/README.md`: the URL, revision `oldid` and retrieval date per table
- Test: `tests/harness/test-sources.R`

**Interfaces:**
- Produces files that Task 5 reads.
- All three Wikipedia tables have the columns of the 2021 file: `fieldwork, house, start, end, note, n, D, S, B, V, P, C, F, M, J`. Shares are in percent, and an empty cell means the party was not reported.
- **Required coverage:** every poll the page lists from 60 days before the cycle's earliest F through E. That is 2017-08-15 to 2017-10-28, 2021-01-28 to 2021-09-25 and 2024-04-04 to 2024-11-30. Earlier rows are welcome.

- [ ] **Step 1: Write the structural tests.**

```r
wk <- function(y) readr::read_csv(root("harness", "sources", sprintf("wikipedia_%d_poll_fieldwork.csv", y)),
                                  show_col_types = FALSE)
test_that("fieldwork tables share one shape and have ordered dates", {
  for (y in c(2017, 2021, 2024)) {
    w <- wk(y)
    expect_named(w, c("fieldwork", "house", "start", "end", "note", "n", "D", "S", "B", "V", "P", "C", "F", "M", "J"))
    expect_true(all(w$start <= w$end, na.rm = TRUE)); expect_false(anyNA(w$end))
  }
  expect_lte(min(wk(2017)$end), as.Date("2017-08-15")); expect_lte(min(wk(2021)$end), as.Date("2021-01-28"))
  expect_lte(min(wk(2024)$end), as.Date("2024-04-04"))
})
test_that("every Wikipedia house is mapped", {
  m <- readr::read_csv(root("harness", "sources", "wikipedia_house_map.csv"), show_col_types = FALSE)
  houses <- unique(unlist(lapply(c(2017, 2021, 2024), \(y) wk(y)$house)))
  expect_true(all(houses %in% m$wikipedia_house))
})
test_that("corrections name a fieldwork end after the midpoint", {
  cr <- readr::read_csv(root("harness", "sources", "fieldwork_corrections.csv"), show_col_types = FALSE)
  expect_true(all(cr$end >= cr$date)); expect_false(anyNA(cr$source))
})
```

- [ ] **Step 2: Run and confirm failure.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R sources`. Expected: FAIL (files missing).
- [ ] **Step 3: Build the 2017 and 2024 tables** from en.wikipedia "Opinion polling for the 2017 Icelandic parliamentary election" and the 2024 equivalent (or the polling section of the election article). Fetch with WebFetch.
  - Parse each fieldwork range to ISO `start`/`end`.
  - Copy the 2021 file across and extend it to 2021-01-28 if it starts later.
  - Record the URL and `oldid` per table in the sources README.
- [ ] **Step 4: Fill `wikipedia_house_map.csv`.** Map each Wikipedia pollster name to a `fyrirtaeki` from `polling_data.csv`: MMR, Gallup, "Fréttablaðið, Stöð 2 og Vísir", Félagsvísindastofnun, Prósent, Maskína. Check the 2016–17 newspaper polls' contractor (Zenter?) on the page before mapping.
- [ ] **Step 5: Fill `fieldwork_corrections.csv`.** Use a primary source per row (Gallup, RÚV, Vísir or the pollster's report). Treat earlier notes on these polls as leads, not sources.
  - Gallup 2024-11-15: end 2024-11-21, for both level `national` and level `kjordaemi`. The data's `lokadagur` says 2024-11-18.
  - Prósent 2024-09-15: fieldwork 18 Sep – 3 Oct 2024, so end 2024-10-03.
  - Prósent 2024-09-26: probably a misdated duplicate of an 18 Oct poll. Its end is that poll's end as the source gives it.
  - **If no source confirms a lead, stop and report it to Brynjólfur rather than guessing.** A confirmed duplicate stays in the data (production read it; there are no house-specific cuts), and the sources README notes it.
- [ ] **Step 6: Run and confirm pass.** Same command. Expected: 3 tests pass.
- [ ] **Step 7: Commit** with the message "harness: Wikipedia fieldwork tables, house map and fieldwork corrections".

---

### Task 5: Poll availability (D2)

**Files:**
- Create: `R/harness/availability.R`, `tests/harness/test-availability.R`.
- Generate and commit: `harness/availability.csv` and `harness/availability_report.md`.

**Interfaces:**
- Consumes: the snapshot, the Task 4 sources, `TARGETS`, `WINDOW_START`.
- Produces:
  - `parse_lokadagur(x)` returns a Date. It accepts `2024-11-13T00:00:00Z`, `2024-11-13` and `NA`/`"NA"`, and takes the UTC calendar date.
  - `poll_list(polling, constituency)` returns one row per poll: (level ∈ national/kjordaemi, date, fyrirtaeki, lokadagur, a `shares` list-column of named pp values). The key is (level, date, fyrirtaeki, kjordaemi-free), and it errors if one key carries two `lokadagur` values.
  - `match_fieldwork(polls, fieldwork, house_map, tol_pp = 0.25, max_days = 7)` returns (date, fyrirtaeki, start, end). A candidate needs the same house, a Wikipedia midpoint within `max_days` of `date`, and a mean absolute share difference ≤ `tol_pp` over the parties both report. Matching is one-to-one: greedy by smallest share difference, each Wikipedia row used at most once.
  - `house_max_fieldwork(spans)` returns a tibble (fyrirtaeki, max_days), where max_days = max(end − start) in days over known spans. Its attribute `overall` is the maximum over all houses; a house with no span uses it.
  - `poll_availability(polling = <snapshot polling_data>, constituency = <snapshot constituency_data>, sources_dir = harness_path("sources"))` returns a tibble (level, date, fyrirtaeki, fieldwork_end, available_from, rule), one row per poll in the window plus the national `Kosning` rows.
    - **National rule order** (amended 23 Oct 2026): `correction` (end + 1), `lokadagur` (+1), `wikipedia` (end + 1), `published` (that date), `house_max` (`date` + max_days + 1). Floor: `available_from` is never earlier than the poll's recorded publication date in `poll_publication.csv`, which covers every national poll a forecast admits with `available_from` in (F − 45, F]; `rule` names the rule whose date binds.
    - **Kjördæmi rule order** (amended 23 Oct 2026): `correction`, then `twin` (the national poll with the same date and house; inherits its `fieldwork_end` and `available_from`), then `published`, then `house_max` on the house's national spans. Floors: `available_from` is never earlier than the poll's recorded publication date or its national twin's `available_from`.
    - **Election rows:** `Kosning` gets `available_from = date + 1`, rule `election`.
  - `availability_counts(av)` returns (level, rule, n) for every rule, zeros included.
  - `excluded_near(av, F)` returns the rows with `date <= F < available_from`.
  - `fieldwork_overlaps(av)` returns pairs of same-house polls with overlapping known spans. It is a report, never a filter (§5.3 rule 2).
  - `write_availability()` writes the CSV and a report: counts per rule, and for each of the 10 cells the `excluded_near` rows with their rule, plus overlaps.

- [ ] **Step 1: Write the failing tests.** The synthetic ones need no snapshot.

```r
box::use(R/harness/availability[parse_lokadagur, match_fieldwork, house_max_fieldwork, poll_availability])

test_that("lokadagur datetimes parse to their calendar date", {
  expect_equal(parse_lokadagur(c("2024-11-13T00:00:00Z", "2024-11-13", "NA", NA)),
               as.Date(c("2024-11-13", "2024-11-13", NA, NA)))
})

wiki1 <- tibble::tibble(house = "Gallup", start = as.Date("2021-09-01"), end = as.Date("2021-09-07"),
                        D = 24.0, S = 10.0)
map1 <- tibble::tibble(wikipedia_house = "Gallup", fyrirtaeki = "Gallup")
poll <- function(date, d, s) tibble::tibble(date = as.Date(date), fyrirtaeki = "Gallup",
                                            shares = list(c(D = d, S = s)))
test_that("matching needs house, date and shares, and is one-to-one", {
  m <- match_fieldwork(dplyr::bind_rows(poll("2021-09-04", 24.1, 10.1), poll("2021-09-06", 24.2, 10.2)),
                       wiki1, map1)
  expect_equal(nrow(m), 1); expect_equal(m$date, as.Date("2021-09-04"))
  expect_equal(nrow(match_fieldwork(poll("2021-09-04", 24.26, 10.26), wiki1, map1)), 0)  # 0.26 pp
  expect_equal(nrow(match_fieldwork(poll("2021-09-12", 24.0, 10.0), wiki1, map1)), 0)    # 8 days
})

test_that("longest fieldwork per house, and the overall maximum for houses with none", {
  h <- house_max_fieldwork(tibble::tibble(fyrirtaeki = c("Gallup", "MMR"),
         start = as.Date(c("2021-08-01", "2021-08-01")), end = as.Date(c("2021-08-31", "2021-08-04"))))
  expect_equal(h$max_days[h$fyrirtaeki == "MMR"], 3); expect_equal(attr(h, "overall"), 30)
})

test_that("real snapshot: every poll has one row, none at its midpoint", {
  skip_without_snapshot(); av <- poll_availability()
  expect_equal(nrow(dplyr::count(av, level, date, fyrirtaeki) |> dplyr::filter(n > 1)), 0)
  nonel <- av[av$rule != "election", ]
  expect_true(all(nonel$available_from > nonel$date))
  g <- av[av$date == as.Date("2024-11-15") & av$fyrirtaeki == "Gallup", ]
  expect_equal(g$available_from, as.Date(c("2024-11-22", "2024-11-22")))   # national and kjördæmi
  p <- av[av$date == as.Date("2024-09-26") & av$fyrirtaeki == "Prósent", ]
  expect_gt(p$available_from, as.Date("2024-10-14"))
})
```

- [ ] **Step 2: Run and confirm failure.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R availability`. Expected: FAIL.
- [ ] **Step 3: Implement `availability.R`.** Shares for matching are `p × 100` per party, using the poll parties' letters against the Wikipedia columns (Annað has no column and is skipped).
- [ ] **Step 4: Run and confirm pass.** Same command. Expected: 4 tests pass. If the Prósent assertion fails, Task 4's correction row is missing or wrong; fix the source table, not the rule.
- [ ] **Step 5: Write the table and report.** Run `write_availability()`.
  - The generated report flags every `excluded_near` row decided by `house_max` within 14 days of an F: the polls the fallback alone keeps out.
  - Look each one up on its Wikipedia page. Add a fieldwork row to Task 4's table if one exists; otherwise list it under "Polls with no fieldwork source" in `harness/sources/README.md`. The report itself stays generated.
  - Paste the counts table into the commit message body.
- [ ] **Step 6: Commit** with the message "harness: poll availability by the §5.3 rule order, with per-rule counts".

---

### Task 6: Houses, entry and epochs (D7, D8)

**Files:**
- Create: `R/harness/tables.R`, `harness/sources/house_table.csv`, `tests/harness/test-tables.R`.
- Generate and commit: `harness/entry_table.csv`.

**Interfaces:**
- `epoch_of(date, elections = ELECTIONS)` returns an integer: 1 + the number of elections strictly before `date`. An election day belongs to the epoch it closes.
- `house_table.csv` has the columns (fyrirtaeki, house, lineage). `Kosning` is house `election`. Every other `fyrirtaeki` is its own house, with lineage = itself (§12 Q2 default: Prósent does not continue MMR).
- `read_houses()` returns that tibble.
- `entry_table(polling, window_start = WINDOW_START)` returns a tibble (flokkur, first_nonzero, entry_epoch, newcomer), with Kosning rows excluded. `newcomer` means the party has no `n > 0` poll dated before `window_start`.
- `write_entry_table()`.

- [ ] **Step 1: Write the failing tests.**

```r
box::use(R/harness/tables[epoch_of, entry_table, read_houses], R/harness/inputs[snapshot_file])
test_that("epochs close on election day", {
  d <- as.Date(c("2016-03-13", "2016-10-29", "2016-10-30", "2017-09-21", "2018-10-16", "2024-11-29", "2024-12-01"))
  expect_equal(epoch_of(d), c(1L, 1L, 2L, 2L, 3L, 4L, 5L))
})
test_that("the entry table is the design's", {
  skip_without_snapshot()
  e <- entry_table(readr::read_csv(snapshot_file("polling_data"), show_col_types = FALSE))
  nc <- e[e$newcomer, ] |> dplyr::arrange(first_nonzero)
  expect_equal(nc$flokkur, c("Viðreisn", "Flokkur Fólksins", "Miðflokkurinn", "Sósíalistaflokkurinn"))
  expect_equal(as.character(nc$first_nonzero), c("2016-03-13", "2016-08-13", "2017-09-21", "2018-10-16"))
  expect_equal(nc$entry_epoch, c(1L, 1L, 2L, 3L))
  expect_equal(sum(!e$newcomer), 6)
})
test_that("every pollster has a house", {
  skip_without_snapshot()
  f <- c(readr::read_csv(snapshot_file("polling_data"), show_col_types = FALSE)$fyrirtaeki,
         readr::read_csv(snapshot_file("constituency_data"), show_col_types = FALSE)$fyrirtaeki)
  expect_true(all(unique(f) %in% read_houses()$fyrirtaeki))
})
```

- [ ] **Step 2: Run and confirm failure.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R tables`. Expected: FAIL.
- [ ] **Step 3: Implement** the functions and write `house_table.csv` (7 rows: Kosning plus the six pollsters).
- [ ] **Step 4: Run and confirm pass.** Same command. Expected: 3 tests pass. Then run `write_entry_table()`.
- [ ] **Step 5: Commit** with the message "harness: house table, newcomer entry table and epochs".

---

### Task 7: Coalitions and incumbency at F (D9; incumbency for D3)

**Files:**
- Create: `harness/sources/coalition_events.csv`.
- Modify: `R/harness/tables.R`, `tests/harness/test-tables.R`.

**Interfaces:**
- `coalition_events.csv` has one row per cabinet composition: (cabinet, from, to, parties, pm, caretaker, source_url). `parties` and `pm` are list letters joined by `;`; `to` is NA for the cabinet still in office. Verify each date against its `source_url` before committing. The tests depend only on which row is in office at the 10 F dates. Expected rows:

  | cabinet | from | to | parties | pm | caretaker |
  |---|---|---|---|---|---|
  | Sigurður Ingi Jóhannsson | 2016-04-07 | 2017-01-11 | B;D | B | FALSE |
  | Bjarni Benediktsson (2017) | 2017-01-11 | 2017-11-30 | D;C;A | D | FALSE |
  | Katrín Jakobsdóttir I | 2017-11-30 | 2021-11-28 | V;D;B | V | FALSE |
  | Katrín Jakobsdóttir II | 2021-11-28 | 2024-04-09 | V;D;B | V | FALSE |
  | Bjarni Benediktsson II | 2024-04-09 | 2024-10-17 | D;B;V | D | FALSE |
  | Bjarni Benediktsson II | 2024-10-17 | 2024-12-21 | D;B | D | TRUE |
  | Kristrún Frostadóttir | 2024-12-21 | NA | S;C;F | S | FALSE |

  Björt framtíð (A) left the coalition on 15 Sep 2017; the cabinet stayed in office until 30 Nov. Vinstri græn refused the caretaker role after the 13 Oct 2024 collapse and left on 17 Oct.
- `read_coalitions()` returns that tibble.
- `cabinet_at(F, coalitions)` returns the row with `from <= F < to`, where NA counts as open.
- `coalition_event(F, coalitions)` returns `list(parties = poll-party labels, dropped = letters mapped to Annað)`. It is the Brier event's coalition: the composition in office at F.
- `incumbents_at(F, coalitions)` returns poll-party labels of the **opening composition** of the cabinet in office at F. The rows share the `cabinet` name, so take the earliest `from` among them. Letters that map to Annað (Björt framtíð) are dropped. This is how `fundamentals_data.csv` codes incumbency (Vinstri græn counts as incumbent in 2024). `pm_at(F, coalitions)` returns the PM's party label.

- [ ] **Step 1: Write the failing tests** (appended to test-tables.R).

```r
box::use(R/harness/tables[read_coalitions, coalition_event, incumbents_at, pm_at])
co <- read_coalitions()
ev <- function(F) sort(coalition_event(as.Date(F), co)$parties)
test_that("the Brier coalition is the composition in office at F", {
  expect_equal(ev("2017-10-14"), sort(c("Sjálfstæðisflokkurinn", "Viðreisn")))
  expect_equal(coalition_event(as.Date("2017-10-27"), co)$dropped, "A")
  for (F in c("2021-03-29", "2021-09-24", "2024-06-03", "2024-10-14"))
    expect_equal(ev(F), sort(c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn", "Vinstri Græn")), label = F)
  for (F in c("2024-11-16", "2024-11-29"))
    expect_equal(ev(F), sort(c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn")), label = F)
})
test_that("incumbency at F equals the fundamentals coding in every cell", {
  skip_without_snapshot()
  f <- readr::read_csv(snapshot_file("fundamentals_data"), show_col_types = FALSE)
  for (i in seq_len(nrow(TARGETS))) {
    E <- TARGETS$E[i]; F <- TARGETS$F[i]; fe <- f[f$date == E, ]
    coded <- fe$flokkur[fe$incumbent == 1] |> dplyr::recode(Framsókn = "Framsóknarflokkurinn")
    expect_equal(sort(incumbents_at(F, co)), sort(intersect(coded, POLL_PARTIES)), label = paste(E, F))
    pm <- dplyr::recode(fe$flokkur[fe$incumbent_prime_minister == 1], Framsókn = "Framsóknarflokkurinn")
    expect_equal(pm_at(F, co), pm, label = paste("pm", E, F))
  }
})
```

- [ ] **Step 2: Run and confirm failure.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R tables`. Expected: FAIL.
- [ ] **Step 3: Fill `coalition_events.csv`** with each row verified against its source (stjornarradid.is or the Wikipedia cabinet pages, which cite RÚV). Then implement the four functions.
- [ ] **Step 4: Run and confirm pass.** Same command. Expected: all tables tests pass. The 2024-11-16/29 Brier event being D + B is the literal §5.6 rule; record it in `harness/README.md` under "Notes for the freeze review" so Brynjólfur sees it before Phase 3.
- [ ] **Step 5: Commit** with the message "harness: coalition events, the Brier coalition and incumbency at F".

---

### Task 8: Fundamentals target rows (D3) and expected valid votes (D6)

**Files:**
- Create: `R/harness/fundamentals.R`, `tests/harness/test-fundamentals.R`.
- Modify: `R/harness/tables.R` (D6) and its tests.

**Interfaces:**
- Consumes: `incumbents_at`, `pm_at`, `read_coalitions`, `fold_truth`, `national_shares`, `previous_election`, the entry table, the snapshot.
- Produces:
  - `fundamentals_party(flokkur)` maps Framsókn → Framsóknarflokkurinn and keeps a label already in `POLL_PARTIES`; everything else → Annað.
  - `fundamentals_inputs(E, F, fund, econ, coalitions, parties)` returns `list(train, target, S_f)`:
    - `train` holds the rows with `date < E`, inner-joined to `econ` by `date`. It errors if any training election has no economy row. Its flag `in_sf` = `flokkur != "Annað" & voteshare_prior > 0 & new_party == 0`, applied in each training election's own labels.
    - `S_f` is `parties` (the cell's poll parties) that had a list at `previous_election(E)`, excluding Annað.
    - `target` has one row per `S_f` party. It takes `voteshare_prior` and `incumbent_years` from the `fund` row at E (mapped with `fundamentals_party`), `incumbent` and `incumbent_prime_minister` from `incumbents_at(F)` and `pm_at(F)`, and `vnv` and `growth` from `econ` at E. `voteshare` and `voteshare_change` are NA.
  - `valid_votes(truth, election)` returns a tibble (kjordaemi, votes, w), sorted by `kjordaemi`, with w = votes / sum. It is the weights w_{e,k} for a closed election.
  - `expected_valid_votes(truth, E)` = `valid_votes(truth, previous_election(E))`, the N_k and w_open for the forecast. It fails for E = 2016-10-29 (no 2013 kjördæmi truth).

- [ ] **Step 1: Write the failing tests.**

```r
box::use(R/harness/fundamentals[fundamentals_inputs], R/harness/tables[valid_votes, expected_valid_votes, read_coalitions],
         R/harness/truth[build_truth, national_shares])
fi <- function(E, F, parties = POLL_PARTIES) {
  f <- readr::read_csv(snapshot_file("fundamentals_data"), show_col_types = FALSE)
  x <- readr::read_csv(snapshot_file("economy_data"), show_col_types = FALSE)
  fundamentals_inputs(as.Date(E), as.Date(F), f, x, read_coalitions(), parties)
}
test_that("S_f is the parties with a previous result, without Annað", {
  skip_without_snapshot()
  s17 <- c("Sjálfstæðisflokkurinn", "Framsóknarflokkurinn", "Samfylkingin", "Vinstri Græn", "Píratar",
           "Viðreisn", "Flokkur Fólksins")
  expect_setequal(fi("2017-10-28", "2017-10-14", setdiff(POLL_PARTIES, "Sósíalistaflokkurinn"))$S_f, s17)
  expect_setequal(fi("2021-09-25", "2021-09-11")$S_f, c(s17, "Miðflokkurinn"))
  expect_setequal(fi("2024-11-30", "2024-11-16")$S_f, setdiff(POLL_PARTIES, "Annað"))
})
test_that("the target is masked and training stops before E", {
  skip_without_snapshot(); x <- fi("2017-10-28", "2017-10-27", setdiff(POLL_PARTIES, "Sósíalistaflokkurinn"))
  expect_true(all(is.na(x$target$voteshare)) && all(is.na(x$target$voteshare_change)))
  expect_equal(max(x$train$date), as.Date("2016-10-29"))
  expect_false(anyNA(x$target$vnv)); expect_false(anyNA(x$target$growth))
})
test_that("target priors agree with the official previous result to rounding", {
  skip_without_snapshot(); x <- fi("2021-09-25", "2021-09-24"); t <- build_truth()
  prev <- 100 * national_shares(t, as.Date("2017-10-28"))[x$target$flokkur]
  expect_true(all(abs(x$target$voteshare_prior - prev) <= 0.06))
})
test_that("expected valid votes are the previous election's, by name", {
  skip_without_snapshot(); t <- build_truth()
  expect_equal(sum(expected_valid_votes(t, as.Date("2024-11-30"))$votes), 199730)
  expect_equal(sum(expected_valid_votes(t, as.Date("2017-10-28"))$votes), 189648)
  withr::local_seed(1)
  expect_identical(valid_votes(t[sample(nrow(t)), ], as.Date("2021-09-25")), valid_votes(t, as.Date("2021-09-25")))
  expect_equal(sum(valid_votes(t, as.Date("2021-09-25"))$w), 1)
  expect_error(expected_valid_votes(t, as.Date("2016-10-29")))
})
```

- [ ] **Step 2: Run and confirm failure.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R fundamentals` (and `tables`). Expected: FAIL.
- [ ] **Step 3: Implement** `fundamentals.R` and the D6 functions.
- [ ] **Step 4: Run and confirm pass.** Expected: 3 new fundamentals tests and 1 new tables test pass.
- [ ] **Step 5: Commit** with the message "harness: fundamentals target rows with S_f and incumbency at F; previous-election valid votes".

---

### Task 9: `info_set()` and `validate_inputs()` with planted leaks (D4, §5.3)

**Files:**
- Create: `R/harness/info_set.R`, `tests/harness/test-info-set.R`.

**Interfaces:**
- Consumes: everything above.
- Produces:
  - `read_inputs()` returns a list: the snapshot tables, truth, availability, entry, houses and coalitions. It verifies the snapshot once.
  - `info_set(E, F, inp = read_inputs())` returns a list with these elements:
    - `E`, `F`;
    - `parties`: `POLL_PARTIES` restricted to parties with an admitted non-zero poll by F;
    - `polls_nat`: national polls with `date >= WINDOW_START`, not Kosning, `available_from <= F`; long format (date, fyrirtaeki, flokkur, n, available_from), restricted to `parties`;
    - `polls_kd`: D4, every kjördæmi poll (not Kosning) with `available_from <= F`, the same columns plus `kjordaemi`;
    - `kosning_nat`: national Kosning rows with `WINDOW_START <= date < E`, for R0;
    - `results`: `fold_truth()` rows with `election < E`;
    - `fundamentals`: from `fundamentals_inputs()`;
    - `coalitions`: rows with `from <= F`, with `to > F` censored to NA;
    - `coalition_event`, `layout` (`layout_for(E)`), `valid_votes_open` (`expected_valid_votes()`), `prev_shares` (`prev_national_shares()` for each window election ≤ E), `entry` and `houses`.
  - `validate_inputs(info)` returns `invisible(list(admitted = tibble(level, date, fyrirtaeki, available_from), overlaps))`. Each check stops with the message given:
    - "poll available after F": any `polls_nat`/`polls_kd` row with `available_from > F` or NA;
    - "poll dated on or after E";
    - "poll before the window";
    - "target election result": any `results$election >= E` or `kosning_nat$date >= E`;
    - "target vote share": a non-NA `voteshare`/`voteshare_change` in `fundamentals$target`, or any `train$date >= E`;
    - "event after F": any `coalitions$from > F`, or a non-NA `to > F`;
    - "party not yet available": a party in `parties` whose first admitted non-zero poll is after F.
  - The admitted list is what Phase 3 writes into `FREEZE.md` per cell.

- [ ] **Step 1: Write the failing tests.**

```r
box::use(R/harness/info_set[info_set, validate_inputs, read_inputs])
inp <- if (file.exists(root("harness", "snapshot_manifest.csv"))) read_inputs()
cell <- function(E, F) info_set(as.Date(E), as.Date(F), inp)

test_that("every design cell builds and validates", {
  skip_without_snapshot()
  for (i in seq_len(nrow(TARGETS))) expect_no_error(validate_inputs(info_set(TARGETS$E[i], TARGETS$F[i], inp)))
})
test_that("2017 has nine parties; Gallup 15 Nov is out on 16 Nov and in on the eve", {
  skip_without_snapshot()
  expect_false("Sósíalistaflokkurinn" %in% cell("2017-10-28", "2017-10-14")$parties)
  g <- function(x) any(x$date == as.Date("2024-11-15") & x$fyrirtaeki == "Gallup")
  a <- cell("2024-11-30", "2024-11-16"); b <- cell("2024-11-30", "2024-11-29")
  expect_false(g(a$polls_nat) || g(a$polls_kd)); expect_true(g(b$polls_nat) && g(b$polls_kd))
})
test_that("a poll available exactly on F is admitted", {
  skip_without_snapshot()
  F <- as.Date("2024-11-22")
  expect_true(any(cell("2024-11-30", F)$polls_kd$date == as.Date("2024-11-15")))
  expect_false(any(cell("2024-11-30", F - 1)$polls_kd$date == as.Date("2024-11-15")))
})
test_that("planted leaks are refused", {
  skip_without_snapshot(); ok <- cell("2024-11-30", "2024-11-16")
  x <- ok; x$polls_nat$available_from[1] <- x$F + 1
  expect_error(validate_inputs(x), "poll available after F")
  x <- ok; x$results <- dplyr::bind_rows(x$results, dplyr::mutate(x$results[1, ], election = x$E))
  expect_error(validate_inputs(x), "target election result")
  x <- ok; x$fundamentals$target$voteshare[1] <- 20
  expect_error(validate_inputs(x), "target vote share")
  x <- ok; x$coalitions <- dplyr::bind_rows(x$coalitions, dplyr::mutate(x$coalitions[1, ], from = x$F + 1))
  expect_error(validate_inputs(x), "event after F")
  x <- cell("2017-10-28", "2017-10-14"); x$parties <- c(x$parties, "Sósíalistaflokkurinn")
  expect_error(validate_inputs(x), "party not yet available")
  y <- cell("2021-09-25", "2021-09-24")
  y$kosning_nat <- dplyr::bind_rows(y$kosning_nat, dplyr::mutate(y$kosning_nat[1, ], date = y$E))
  expect_error(validate_inputs(y), "target election result")
  x <- ok; x$polls_kd$date[1] <- x$E
  expect_error(validate_inputs(x), "poll dated on or after E")
  x <- ok; x$polls_nat$date[1] <- as.Date("2015-12-31")
  expect_error(validate_inputs(x), "poll before the window")
})
```

- [ ] **Step 2: Run and confirm failure.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R info-set`. Expected: FAIL.
- [ ] **Step 3: Implement `info_set.R`.**
- [ ] **Step 4: Run and confirm pass.** Same command. Expected: 4 tests pass.
- [ ] **Step 5: Commit** with the message "harness: info_set() and validate_inputs(), with planted-leak tests".

---

### Task 10: The rebuild script, docs and the Phase 0 exit check

**Files:**
- Create: `R/harness/build_phase0.R`.
- Modify: `harness/README.md`, `R/CLAUDE.md` (a new "Harness (v2)" table), and `CLAUDE.md` (§ Testing: the suite and its command; `harness/` under Repository Layout).

**Interfaces:**
- `build_phase0.R` is a headless script run from the root with `LC_ALL=en_US.UTF-8 Rscript R/harness/build_phase0.R`. It verifies the snapshot, rewrites the four generated committed files (truth, availability CSV and report, entry table) and prints the availability counts plus each cell's admitted poll counts (national, kjördæmi).

- [ ] **Step 1: Write `build_phase0.R`.** Run it twice and check that `git status --porcelain harness/` is empty after the second run. Expected: deterministic output with no diff.
- [ ] **Step 2: Run the whole suite.** `LC_ALL=en_US.UTF-8 Rscript tests/run.R`. Expected: every test passes and none is skipped. A skip means the snapshot is missing.
- [ ] **Step 3: Check the Phase 0 exit criterion against the output, item by item.**
  - (a) The seat test passes for 2016-10-29, 2017-10-28, 2021-09-25 and 2024-11-30.
  - (b) The planted-leak test refuses a post-F poll, a target result, a target vote share and a post-F event.
  - (c) The availability counts sum to the number of polls, and the no-midpoint assertion (`available_from > date` for every non-election row) passes.
- [ ] **Step 4: Update the docs.**
  - Root `CLAUDE.md` § Testing: replace "No formal test suite" with the harness suite and its command. Model validation stays interactive.
  - `R/CLAUDE.md`: one table row per `R/harness/` module.
  - `harness/README.md`: the rebuild command.
- [ ] **Step 5: Commit** with the message "harness: Phase 0 rebuild script and docs; exit criteria met". Record in the body the test count, and the seat and leak results as run.

---

## Later plans (not part of this plan)

Each plan is written once the previous one has landed, because each depends on what the previous phase measured.

- **Plan 2 (Phase 1).** Covers:
  - `R/harness/` scoring: DSS, CRPS10, d², kjördæmi and seat scores;
  - the scorer self-test against the reference scores recorded in the Metill vault (Kosningar/Forecast model);
  - the freeze guard and the 2017 `DECISION.md`/`CONFIRM.md` locks;
  - the adapter contract (§5.5) and the R0 adapter from `info_set()` (production's kjördæmi filter mapped to E; the hedge with n_e from `entry`);
  - a reused fitting queue;
  - R0 Tier 0.
- **Plan 3 (Phase 2).** Covers:
  - the §6.2 grid checks added to `validate_inputs()` (sorted, every Δ > 0, nothing observed between F and E);
  - `Stan/fundamentals_v2.stan` (stage 1, consuming `fundamentals_inputs()`);
  - `Stan/forecast_v2.stan` with every §6 part;
  - simulation-based recovery;
  - the permuted-order refit;
  - V2 Tier 0, which picks the walk parameterisation by ESS/min and applies the §10 fallback order if a fit exceeds 2 h.
- **Plan 4 (Phases 3–6, runbook).** Covers:
  - `FREEZE.md` (writing `validate_inputs()`'s admitted lists, the entry and house tables, coalition events, the margins after Brynjólfur's review);
  - Tier 1/2, then `DECISION.md`;
  - Tier C (2017, once);
  - Tier 3/3b and the vault decision memo.
