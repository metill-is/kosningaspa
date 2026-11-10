# forecast_v2: the simple core against its switched form

Generated 2026-11-10 by `R/harness/archive/v2_simple_core_lp_check.py`.

- New: `Stan/forecast_v2.stan`, sha256 `a93df9c731e6b33bab54f42e57f8866169b6cb963180c1ada2f88d79050d4560`.
- Reference: `Stan/forecast_v2.stan` at tag `v2-switched-final` with the campaign line for the step leaving each closed
  election, sha256 `70824606248367d4d123130daa145018fd1f2f4d61e9aa2a9483c263873b3206`.
- Points per cell: the first 8 warm-up draws and 20 draws of one reference chain
  (24 warm-up iterations, max_depth 8), constraints restored, evaluated with CmdStan's
  `log_prob` (jacobian = 1) on both models.
- Pass: lp_new - lp_reference constant within 1e-08 max(1, |lp|), and gradients within 1e-08 of the
  largest absolute gradient element.

| Cell | Points | Coordinates | lp offset | Largest lp deviation | Largest gradient deviation | Verdict |
|---|---|---|---|---|---|---|
| election 2024-11-30, forecast date 2024-11-29 | 28 | 2912 | 0 | 0 | 0 | PASS |
| election 2021-09-25, forecast date 2021-03-29 | 28 | 1724 | 0 | 0 | 0 | PASS |

Overall: PASS.

Regenerate, from the repository root, with the x-r0w configuration's recorded Stan data files:

```
python3 R/harness/archive/v2_simple_core_lp_check.py <2024-11-30 eve stan_data.json> <2021-09-25 at 180 days stan_data.json>
```
