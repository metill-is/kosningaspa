# Stan/archive — Archived Model Variants

Experimental Stan model variants kept for historical reference. These are **not used in production**.

Includes: Poisson observation model, no-Dirichlet variant, 2021-specific models, DLM observation variant, alternative parameterizations (xi), the superseded and rejected Fylgisvakt variants (`polling_watch.stan`, `_v2`, `_v3`, `_v3b`, `_v4_sym`, `_v4_gmrf`; their record is `Stan/polling_watch_variants.md`), the historical-backtest model (`polling_and_fundamentals_historic.stan`) and the baseline without house effects (`base_model_no_polling_bias.stan`).

Also here since 11 November 2026, when nothing live called them any more: the national model without constituencies (`polling_and_fundamentals.stan`) and the standalone fundamentals model (`fundamentals.stan`). Their fit scripts are `R/archive/fit_polling_and_fundamentals_model.R` and `R/archive/fit_fundamentals_model.R`.
