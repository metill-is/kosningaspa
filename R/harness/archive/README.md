# Archived harness studies

One-off V2 verification studies that ran, passed and changed no model or prior: the Tier 0 extras (`v2_checks.R`), the P-step equivalence check (`v2_equivalence.R`) and the adoption and permutation rules once in `compare_runs.R` (`compare_runs_adopt_perm.R`). Their tests are in `tests/harness/archive/`.

Nothing here is run or tested, and the files do not load from this folder. To run one as it was, check out commit `d6970a6`, where they sit in `R/harness/` and `tests/harness/`.

`v2_simple_core_lp_check.py` checked that the single-configuration `Stan/forecast_v2.stan` is the same model as the tag `v2-switched-final`'s file at the simple core; its report is `harness/v2/lp-equivalence-simple-core.md`, and it runs from the repository root as that report says.
