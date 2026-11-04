# Analysis behind chapter 3.1 (29 Oct 2026)

Scripts that computed the worked example's numbers in `bias/updating.html`. Each reads the 29 Oct 2026 v5 fit in `data/2026-10-29/` (git-ignored, so present only where that fit was run) and the polls via `R/data.R`. Run them from the repository root with `LC_ALL=en_US.UTF-8 Rscript docs/method/analysis/3.1-bias/<script>.R`.

| Script | Gives |
|---|---|
| `d_gaps.R` | D's mean poll minus result, 30 days before and 60 days after each election |
| `d_polls.R` | the early E5 polls, results, latent D at dates, house means by period |
| `d_shocks.R` | the shock table: each party's industry shock per epoch boundary in units of τ_μ, with the RMS |
| `d_walk.R` | the walk's sd of D's share after the 2024 result; latent D around the election; D's E4→E5 shock (logit, in τ_μ, P > 0) and earlier steps; the half-normal prior's quantiles and τ_μ's percentile |
| `d_lean.R` | Gallup's and Maskína's D lean against v5's latent, by year and quarter |

The chapter's linear-Gaussian E5 weights (94% on the polls to day 48) came from a checking agent's own calculation on 29 Oct 2026; no script was kept.

The per-epoch pp figures (Figure 2, the house biases) came from the comparison script of the 29 Oct v5 review, which is not in the repository; the method is the chapter's Δ formula averaged over each epoch's polls. If the fit folder is gone, these numbers can no longer be re-run; the chapter's text is the record.
