# Archived harness studies

One-off V2 verification studies that ran, passed and changed no model or prior: the Tier 0 extras (`v2_checks.R`), the P-step equivalence check (`v2_equivalence.R`) and the adoption and permutation rules once in `compare_runs.R` (`compare_runs_adopt_perm.R`). Their tests are in `tests/harness/archive/`.

Nothing here is run or tested, and the files do not load from this folder. To run one as it was, check out commit `d6970a6`, where they sit in `R/harness/` and `tests/harness/`.

`v2_simple_core_lp_check.py` checked that the single-configuration `Stan/forecast_v2.stan` is the same model as the tag `v2-switched-final`'s file at the simple core; its report is `harness/v2/lp-equivalence-simple-core.md`, and it runs from the repository root as that report says.

`recovery.R` was the simulation-based recovery study (spec §8, Decision 7): coverage of `tau_mu`, `s_bar_E`, the open-epoch industry bias and δ over 30 pre-registered replicates at 400 + 400; it ran against the nine-switch model at the tag `v2-switched-final`, where it, its test and its report (then `harness/v2/recovery.md`) can be read and run; the report is now `harness/archive/v2/recovery.md`.
A version for the simple core would need new target groups (no `tau_mu` or `s_bar_E`; the bias μ_1 and the static δ instead), truth elements and stored natural quantities without `eps` or `s_zeta`, and a newly pre-registered replicate set and pass rule before any fit.
