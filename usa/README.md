# US midterms: feasibility experiment

Can this repo's model forecast the US midterm elections? This folder holds the experiment
that answers it (started 2 November 2026, the day before the 2026 midterms). Nothing here
is used by the Icelandic forecast or Fylgisvakt, and nothing outside `usa/` was changed.

## Files

| File | What it is |
|------|------------|
| `stan/us_midterm_v0.stan` | Prototype: `Stan/polling_watch_v5_epoch.stan` (anchored walk, pollster bias that random-walks across the epochs between elections) unchanged, plus an election-day step with a fundamentals factor, a race layer (each Senate state and House district a deviation from the national walk, with a regression prior learnt from past results) and seat counts. The header explains each addition |
| `R/simulate_us.R` | Simulates US-scale data in that model's format; the national fields also run `polling_watch_v5_epoch.stan` unchanged. Options in its header |
| `R/build_generic_ballot.R` | Real data for `polling_watch_v5_epoch.stan`: the generic congressional ballot polls of the 2018–2026 cycles, anchored at the national House popular vote of 2018–2024 |
| `R/bench_fit.R` | Fits a Stan file on a saved data JSON, times it and prints one `DIAG` line (wall time, ms per gradient, tree depth, divergences, R-hat, ESS) |

The poll tables these scripts read were gathered on 2 November 2026 from Wikipedia, the
Wayback Machine's copy of FiveThirtyEight's poll CSVs, VoteHub and the Clerk of the House.
They live outside the repo in
`/Users/brynjolfurjonsson/.claude-reviews/usa-midterms-feasibility-2026-11-02/tables/`,
since this repo is public and the tables are third-party data.

## Running

```bash
LC_ALL=en_US.UTF-8 Rscript usa/R/simulate_us.R out=<dir> anchor_n=2e5
LC_ALL=en_US.UTF-8 Rscript usa/R/bench_fit.R stan=usa/stan/us_midterm_v0.stan data=<dir>/stan_data.json out=<fit dir>
```

`bench_fit.R` compiles without CmdStan's precompiled header, since that header goes stale
whenever Apple clang updates.
