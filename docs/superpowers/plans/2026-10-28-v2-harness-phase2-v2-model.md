# v2 Harness Phase 2 (the V2 Model) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build arm V2 (spec §6) and its five ablations as harness arms on one Stan file, with each posterior-changing step checked against an independent R implementation of its log density, and each change of parameterisation checked for equivalence. Phase 2 ends when the spec's §8 exit criterion holds. That criterion has four parts: simulation-based recovery, a posterior predictive check of past elections, the permuted-order refit agreeing within MC error, and V2's Tier 0 report committed. Nothing is scored.

**Architecture:**
- **One Stan file.** `Stan/forecast_v2.stan` holds all of §6.
  - Each model component is a data switch: `epoch_bias`, `newcomer_pool`, `shock`, `delta_walk` and `fundamentals`. A component that is switched off has zero-size parameters, so the model is exactly the previous step's.
  - Each parameterisation is also a switch, `walk_form` and `delta_form`. The reference forms stay in the file permanently as a regression check.
- **A staircase.** V2 is built up from a core, which has all five switches off: the V2-noepoch, V2-nonew, V2-noshock, V2-staticδ and V2-nofund ablations at once.
  - An **M-step** turns on one component.
  - A **P-step** changes a parameterisation only.
- **An independent oracle.** `R/harness/v2_reference.R` holds the log density and a simulator, both written from the spec, not from the Stan file.
  - An M-step's test: Stan's log density minus the reference's must be constant across points, and a mutant reference for each failure mode must break that.
  - A P-step's test: the equivalence check of `R/sampler_check/equivalence_anchored.R`, ported to v2.
- **aa2ac1a's parameterisation carries over.** Plan 3 builds on the anchored, partially centred parameterisation of `Stan/polling_and_fundamentals_kjordaemi.stan` at `aa2ac1a`:
  - **P1, the walk.** Election-date levels are centred at the result, with offsets so that init 0 starts there. A party absent from an election is non-centred. Brownian bridges between the anchors are non-centred.
  - **P2, δ.** δ is centred at closed elections and non-centred in the open epoch, as spec §6.7 has it.
  - **Concentrations (core).** Every Dirichlet-multinomial concentration is centred and bounded above, log φ < 20, as in aa2ac1a.
- **Stage 1** (`Stan/fundamentals_v2.stan`, `R/harness/stage1.R`) is fitted inside V2's builder. It is cached by the sha256 of its inputs and enters `forecast_v2.stan` as data.
- **The arms are configs** over one builder and one extractor:
  - `v2` and its five ablations;
  - `v2-perm`, a check arm that is never scored;
  - the staircase's `v2s-*` arms, dev arms that are never scored.

**Tech Stack:** R box modules, testthat 3, cmdstanr 0.9 (CmdStan 2.38; model methods for `log_prob`, `unconstrain_variables`, `constrain_variables`), posterior 1.6, arrow, jsonlite, digest, withr, zsh.

**Spec:** the design note in the Metill vault, `Kosningar/Forecast model/2026-10-23 — v2 forecast model and two-election harness (design).md`. Its relevant sections:
- §6, in full: the model;
- §5.4, the V2 rows: the arms;
- §5.5: the adapter contract;
- §5.7: pre-freeze evidence and the gate;
- §5.8: Tier 0;
- §8: the Phase 2 row;
- §9: left to the builder;
- §10: the compute fallbacks.

Two inputs sit beside it:
- the 28 Oct record in the same vault folder, `2026-10-28 — Anchored, partially centred kjördæmi model (record).md`, whose section "v2 proposals" lists five proposals;
- `R/sampler_check/RESULTS.md`, 28 October section: the equivalence check and its tolerances.

Read §6 and the record before Task 1.

**Scope:**
- **In:** Phase 2.
- **Plan 4:** the freeze, Tiers 1–3b and C, R0-od, V2-φe, FV5, PA, CF, `DECISION.md` and `CONFIRM.md`.
- **Nothing in Plan 3 scores a cell** or reads a result beside a forecast.

## The five v2 proposals

The record's five proposals change the posterior, so each becomes part of V2 or an explicit ruling here. None becomes a change to R0.

| # | Proposal (28 Oct record) | Ruling | Where |
|---|---|---|---|
| 1 | House concentration prior μ_φ ~ N(log 1000, 1) | Adopted, replacing v4's N(0, 1) row of §6.10. A house's concentration measures poll dispersion, which the results barely inform, and its centre follows from poll sizes alone; it is the centre §6.10 already gives φ_K (Decision 1) | Task 4 (the centre is data); Task 6 (its band) |
| 2 | Pool the kjördæmi scales, log σ_δ,p ~ N(μ_sd, τ_sd) | Adopted for V2's initial spread s_δ0,p, which is the scale a newcomer's fresh δ uses (§6.7). The step scales s_ζ,p are already hierarchical in §6.10 and stay so (Decision 2) | Task 4 (s_δ0); Task 12 (s_ζ) |
| 3 | A P-dimensional zero-sum walk, which "needs its own design" | The core walk is P-dimensional and sum-to-zero, with innovation covariance exactly Δ s² CΣCᵀ (§6.2), written in P − 1 coordinates as `L_walk = cholesky((CΣCᵀ)[2:P, 2:P])`. Every (P − 1)-principal minor of CΣCᵀ is equal, because its null vector is **1**. So the log density is exactly invariant to the party order, up to the Jacobian of `L_Omega`, which Task 4 computes and tests. A normal placed directly on a `sum_to_zero_vector` innovation gives the conditional covariance instead (§6.2's warning). That mutant is as permutation-invariant as the true form, so the lp comparison against the reference catches it, not the permutation test | Task 3 (algebra test); Task 4 (lp test, exact permutation test); Task 16 (permuted refit) |
| 4 | Tighter house-effect hyperpriors, N(0, 0.3) and exponential(5) | Not adopted by default. House and industry bias are measured against the results, which is the case §6.10 refuses for τ_μ. §6.10's N(0, 1) and exponential(1) stay, as data, and Tier 3b tests 0.3 and 5 (Decision 1) | Task 4 (the scales are data); Task 6 (reports both); Plan 4 (Tier 3b) |
| 5 | A δ that drifts across elections, fed by earlier results and pre-F kjördæmi poll breakdowns | This is §6.7's δ walk, M4. It uses every closed election in the window (2016, 2017 and 2021, as the cell allows) and every kjördæmi poll available at F. The 2013 results are not added: a 2013 start is a §2 non-goal (§6.1, §10) | Task 12 |

## Decisions for Brynjólfur

Each item is a choice the spec leaves open, or one where this plan departs from it. The plan runs on the defaults given, and each one can be changed cheaply before the freeze (Plan 4). Every value below is a domain value or a rule written before any V2 fit. Decision 1 records where proposals 1 and 4 were first noticed, which was a posterior.

1. **Proposals 1 and 4: provenance, and what each prior is learned from.**
   - **Provenance.** Both were first noticed in the R0 Tier 0 posteriors (the 28 Oct record). Those fits saw the 2016 and 2017 results, and the 2024-eve fit saw 2021 too. Spec §5.3 rule 7 forbids priors that come from a fit that saw the target election, and §6.10 applies the rule to τ_μ: its posterior sat in the prior's tail, and the prior was not moved.
   - **Default: adopt proposal 1; keep §6.10's rows for proposal 4.**
     - *Proposal 1* is about poll dispersion around the latent state, which the election results barely inform. Its centre follows from poll sizes alone: n ≈ 800–2,000 with a design effect of 1–2 puts the concentration near 1,000 (log 6.9), the centre §6.10 already gives φ_K. So μ_φ ~ N(log 1000, 1) (`prior_mu_phi`), and Task 6 checks its band.
     - *Proposal 4* is about house and industry bias, which is measured against the election results, so it is §6.10's τ_μ case exactly. μ_1 ~ N(0, 1) and σ_γ ~ exponential(1) stay. Their scales are data (`prior_sd_mu1`, `prior_rate_sigma_gamma`), and Tier 3b (Plan 4) tests 0.3 and 5.
     - A simulation run while writing this plan (100,000 prior draws, a 20% party beside nine of 8.9%; no fit) gave these [5%, 95%] intervals. The industry bias: −17 to +29 pp under N(0, 1), −7.5 to +8.8 under N(0, 0.3). A house's deviation: −20 to +24 pp under exponential(1), −7.1 to +6.3 under exponential(5). A poll's sd: 12 to 38 pp under μ_φ ~ N(0, 1), 1.3 to 4.1 under N(log 1000, 1). So §6.10's bias rows are wide by domain standards. Task 6 reports both rows' intervals; a wide prior on a bias the results identify is not by itself a reason to move it.
     - `FREEZE.md` (Plan 4) records proposal 1's provenance, and the 2017 test cannot be read as confirming it.
   - **Alternatives:** adopt proposal 4 too (`prior_sd_mu1` 0.3, `prior_rate_sigma_gamma` 5, with the same provenance note); or keep §6.10's row for proposal 1 as well.
2. **Proposal 2's hyperprior.**
   - Default: log s_δ0,p = μ_sd + τ_sd r_p, with r_p ~ N(0, 1), μ_sd ~ N(log 0.3, 0.5) and τ_sd ~ half-N(0, 0.5).
   - The prior median of s_δ0 is then 0.30, a little below the median of §6.10's half-N(0, 0.5), 0.34.
   - Task 6's prior predictive reports the implied spread of kjördæmi deviations.
3. **Δ̄ in the δ walk.**
   - §6.7 says Δ̄ is "the mean of the gaps between elections". That mean changes with the cell, and for the 2024 cells it involves the target date.
   - Default: Δ̄ = 4 years, the statutory term.
4. **Bounds on every Dirichlet-multinomial concentration.**
   - **Why.** CmdStan 2.38's `dirichlet_multinomial` gradient is exact up to log concentration ≈ 22, drifts by 25 and is wrong from 30 (the 28 Oct record).
   - **Default:**

     | Concentration | Upper bound | Init |
     |---|---|---|
     | log φ_h | 20 | mean log national poll size, as aa2ac1a |
     | log φ_K | 20 | log 1000 |
     | log κ | 2 | 0 (so log κφ_h < 22) |
     | Stage 1's log φ_f | 20 | — |

   - Each bound conditions the posterior on what it excludes, as aa2ac1a's does.
   - Task 16 reports the share of draws within 2 of each bound.
5. **The M-step stop rule.** An M-step's two sampler-check fits (Task 7) run at 400 + 400, seed 2701, on 2024 eve and 2021 f180.
   - **Stop and report before the next step** if any fit shows any of:
     - R-hat of `lp__` above 1.1, a stuck chain;
     - more than 1% divergent transitions (16 of 1,600);
     - more than 1% of draws with a bounded log concentration within 0.5 of its cap;
     - a gate-scope max R-hat above 1.10.
   - **Recorded, never a stop:** minutes, treedepth hits, and min bulk ESS per 1,000 gradients.
6. **The P-step adoption rule.** Both forms run on both cells with seeds 2701 and 2702 at 400 + 400. The new form is adopted only if all four hold:
   1. the equivalence check passes;
   2. on each cell, the median over the two seeds of min bulk ESS per 1,000 gradients (gate scope) is at least 1.25× the reference form's;
   3. the new form's divergences over its four fits are at most the reference form's plus 4;
   4. no fit trips the stop rule.

   Otherwise the switch stays at the reference form, and the step log records the numbers. Both forms have the same posterior, so no forecast quantity is consulted.
7. **The recovery protocol** (§8: "coverage of μ, τ_μ, s̄_E, δ").
   - **Replicates.** 20 on the 2024-eve design and 10 on the 2021-f180 design, each at 400 + 400.
   - **The truth.** Each replicate's parameters are drawn from V2's prior, untruncated. A replicate is redrawn only on its simulated data: when a present party's share in a simulated result, or its mean share over an epoch's simulated national polls, leaves [0.5%, 70%]. Selection on the data alone leaves each posterior unchanged, so the ranks stay uniform. The rejection rate is reported.
   - **Pass.** A calibrated model fails one of these about 3% of the time in all.
     - **Scalar groups** (τ_μ, s̄_E; 30 truths each): at least 22 of 30 inside the 90% interval (a calibrated model falls below that 0.2% of the time), and 8 to 22 inside the 50% interval (0.5% outside).
     - **Vector groups** (open-epoch μ, closed δ, open δ): the unit is the replicate, because a replicate's elements are correlated. Each replicate's coverage is the share of the group's elements covered. The mean over the 30 replicates must lie within 3 standard errors (the sd over replicates ÷ √30) of 0.90, and of 0.50.
   - **A fail is a finding.** It goes to Brynjólfur and is not tuned away.
8. **The permutation refit's pass rule** (§8: "the same forecast draws within MC error").
   - **What is compared.** At each Tier 0 cell, V2 against `v2-perm` by z = Δ / √(MCSE₁² + MCSE₂²). The z's cover the mean and the 5%, 50% and 95% quantiles of each `eday_national` share and each kjördæmi share, matched by name.
   - **Pass:** at most 3 |z| > 3, none above 5, and the sd of the mean z's at most 1.5.
   - **Printed:** only these statistics and the names of the variables that exceed 3.
9. **No 2017 MCMC in Plan 3.**
   - The 2017 cells are built and tested, and their log density is evaluated at prior draws.
   - Until the freeze is sealed (`is_frozen()`, Task 1), a 2017 cell's builder writes `draw_forecast = 0` and takes μ_f from `stage1_placeholder()` (0 for every party of S_f), never from `stage1_mu_f()`. So no 2017 forecast quantity is computed, a fundamentals forecast included, and every method fit, GQ run and equivalence check on a 2017 cell is forecast-free.
   - `stage1_mu_f()` and `fit_cell.R` both refuse a 2017 cell until the freeze is sealed (Tasks 2 and 5).
   - 2017 is first fitted after the freeze (spec §5.7).

## Spec readings this plan takes (no answer needed)

**Parties and houses**
- **Reported parties.** A poll's or a kjördæmi poll row's party set S is the parties with n > 0, which is production's rule (`R/stan_data.R:173`).
- **Result cells.** A result cell's party set is the parties with national votes at that election.
  - The builder stops if such a party has 0 votes in some kjördæmi.
  - The official results satisfy this for 2016, 2017, 2021 and 2024. To check, sum `harness/truth/elections_official.csv` by (election, kjordaemi, flokkur) and look for a zero in a party that is present nationally; done 2026-10-28.
- **Houses are pollsters only.** `ELECTION_HOUSE` is dropped. Elections enter no national-poll term, so §6.1's "h = 1 is the election" has no index here.
- **The coordinate party** is the first party of the order. The builder stops unless it has votes at every closed election in the window.
  - The default order, `POLL_PARTIES`, starts with Sjálfstæðisflokkurinn.
  - `v2-perm` reverses both the party order and the kjördæmi order (config key `order` = `"reversed"`), so Annað is first.

**The walk**
- **Normals on `sum_to_zero_vector`s.** θ_1, ν_j and μ_1 put `normal(0, s)` on every element of a `sum_to_zero_vector`, as v4 does. That prior is isotropic, and for an isotropic prior the conditional and projected covariances agree (s²C). The walk's innovations are not isotropic, so they use the projected covariance (proposal 3).
- **σ keeps §6.10's half-t(3, 0, 0.02).** That prior has no hierarchy, so aa2ac1a's partially centred log σ has nothing to centre: σ is sampled directly.
- **No centred Q-basis walk is built.**
  - The sampler study measured centred walks 7.6–17× worse, because of the pre-entry flat direction.
  - §6.9's choice by ESS is therefore between the reference form (non-centred) and the anchored form (P1). Both are the same model, and both keep pre-entry components non-centred.
- **Newcomers stay in the state from the window's start**, which is §6.2's default. Their components enter no likelihood term before entry.

**Grid and epochs**
- **The grid** (§6.2) is built from the admitted poll dates, the closed elections in the window, the campaign starts, F and E.
  - The campaign starts are election − `campaign_days`, for the closed elections and E, and only those on or after the first poll date.
  - Its first date is the first admitted poll date, not 2016-01-01.
  - Every observation date is on or before F. In a 180-day cell the target's campaign start, E − 47, lies between F and E and is the only grid date there; in a 47-day cell it is F itself.
  - The grid checks run in `v2_stan_data()`, not in the shared `validate_inputs()`, which R0's runs also pass through.
  - **The campaign flag:** step i has c_i = 1 when t_i ≥ election − 47 and t_{i+1} ≤ election for some election, past or target.
- **Epochs.** An election day belongs to the epoch it closes (`epoch_of()`). Election e's result uses δ_e and w_e.
- **Weights.**
  - A closed epoch's polls use its closing election's w, years before that election included.
  - The open epoch uses the previous election's w (§6.1).
  - Δyears_e = days between elections ÷ 365.25.

**Outputs**
- **`eday_national`** is `pi_nat_eday`, §6.8's vote-weighted aggregate of `votes_kd`. It equals the scorer's `kd_to_nat()` reading draw by draw, so V2's §5.5 gap is zero by construction. `latent_national` is `pi_latent_E` = Σ_k w_open,k softmax(θ_E + δ_open,k), without ε_E and the cell noise.
  - `n_pred` = Σ_k N_k, and `n_pred_k` = N_k.
- **`pi_latent` on the whole grid** sits behind the data switch `latent_path`, which is 0 in every harness fit (it is about 300 MB of CSV per fit). `pi_latent_E` is always written.

**Stage 1**
- **Covariates.** Stage 1 takes R0's covariates from `fundamentals_rows()` in `R/harness/r0.R`, which Task 2 exports. Counts are restricted to S_f(d) per election, so the shares are renormalised over S_f.
- **Determinism.** Stage 1 is deterministic under a fixed seed, and the cache makes the builder's data sha stable. A lost cache refitted on a different CmdStan can change μ_f in its last digits. The sha-skip then treats the run as stale, which is safe.

**The ablations and switches**
- **The Tier 3b prior scales are data:** `prior_tau_mu` 0.1, `prior_s_bar_E` 0.15, `prior_s_bar_zeta` 0.2, `prior_sd_log_kappa` 0.5, and Decision 1's `prior_mu_phi` (log 1000), `prior_sd_mu1` (1) and `prior_rate_sigma_gamma` (1). Tier 3b then needs no new Stan file. Every other prior is a literal in the Stan file.
- **V2-staticδ uses `delta_form = 0`.** A static δ maps one draw through every epoch's M_e, so "centred at a closed election" is not defined. The posterior does not depend on the form.
- **V2-nonew** keeps the contrast form with m_new = 0 and s_new fixed at 0.2023, the median of half-N(0, 0.3).
- **V2-noepoch** sets τ_μ = τ_μ,A = τ_house = 0.

## Global Constraints

- **The repo is public.**
  - No committed text names a git-excluded path. The names are the last path component of each entry of `$(git rev-parse --git-common-dir)/info/exclude`, except `/data` and `.claude/worktrees/`, which committed docs already name. That covers code, comments, docs, commit messages and generated reports.
  - No committed text holds 1983–2017 ÍSKOS figures. Plan 3 uses no ÍSKOS input.
  - No committed text holds an absolute path under the macOS home folder. Reports print run lists and files as repo-relative paths. Task 16's check matches such paths with the pattern `/[U]sers/`, which does not match itself.
- **No forecast is compared with a result.**
  - No cell is scored. A target cell needs a committed `harness/FREEZE.md` before `score_cell()` runs, and that is Plan 4.
  - No 2017 cell is fitted (Decision 9).
- **No forecast is inspected.**
  - Reports carry sampler diagnostics, summaries of non-forecast parameters, recovery ranks of simulated truths, in-sample PITs of past elections aggregated per election, and z-statistics between two fits.
  - They never print, plot or summarise `eday_*`, `latent_*` or `votes_kd` values, or seats.
  - No step is adopted or rejected on a forecast quantity.
- **The harness never reads the live `data/`.** It writes only to `results/harness/` in the working checkout.
- **Locale.**
  - Every R run is `LC_ALL=en_US.UTF-8 Rscript …`.
  - No config JSON holds Icelandic text: party and kjördæmi orders are a key (`order`), never a list of names.
  - New modules under `R/harness/` write non-ASCII as `\uXXXX` escapes and non-ASCII names with `stats::setNames()`; `tests/harness/test-escapes.R` covers them.
  - The auto-format hook can undo `\u` escapes. Write such lines with Python (`chr(92)`), then run `tests/run.R escapes`.
- **The sampler.** Every fit runs 4 chains, `parallel_chains = 4`, `init = 0` and `max_treedepth = 12`, unless the table says otherwise.

  | Run | Iterations | Other settings |
  |---|---|---|
  | Tier 0 | 200 + 200 | — |
  | Step checks | 400 + 400 | — |
  | Short fits for lp-test points | 50 + 20 | `max_treedepth = 8`, seed 5100 |
  | Equivalence-check points | 100 + 40 | `max_treedepth = 10`, seed 5100 |
  | Stage 1 | 4 × (1000 + 1000) | seed 20261028 |

- **Slow tests.**
  - Tests that compile Stan or fit call `skip_unless_slow()`.
  - Each task's "full suite" is `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R`.
  - Before any slow test or fit, restore the git-ignored harness state (snapshot, self-test inputs, the anchored R0's Tier 0 runs) with `cp -Rp` from the copy named in the vault handoff `Kosningar/Kosningar Handoff.md`.
- **Machine sharing.** Before a queue run or the full slow suite, run `ListAgents`, and ask any peer that may be timing a benchmark.
- **Never edit a running job's script.** Rscript reads a script incrementally, so keep tunables in configs.
- **CmdStan.** If a compile fails on a stale precompiled header (`.gch`), stop and ask Brynjólfur before calling `rebuild_cmdstan()`.
- **Git.** Commit after each task on the session's branch. Ask before any push or PR.

## Review Focus

1. **A poll that drops an interior party.**
   - The case: Gallup drops its smallest party, often Píratar, which sits fifth in `POLL_PARTIES`. A kjördæmi row or a result cell can miss a party the same way.
   - Expected: every likelihood gathers parties by identity, at all three levels.
   - *Test:* Task 4, `test-v2-stan.R` "positional-gather mutant is rejected", on real 2024-eve data. There, every national poll before 2017-09-21 and the 2016 result omit Miðflokkurinn and Sósíalistaflokkurinn (positions 8 and 9) but report Annað (10). Task 1's "partial polls and results are gathered by identity" pins the builder's side.
2. **A party with no kjördæmi information in its epoch.**
   - The cases: Sósíalistaflokkurinn at 2021 f180, which has no kjördæmi poll; Miðflokkurinn in the 2017 cells. A party absent at an anchor is the same kind: Miðflokkurinn and Sósíalistaflokkurinn at 2016, Sósíalistaflokkurinn at 2017.
   - Expected: the builder keeps the party, its δ is prior-only, the log density is finite, and the anchored walk leaves its coordinates non-centred.
   - *Tests:*
     - Task 1, "2021 f180 keeps J with no kjördæmi row";
     - Task 8, the equivalence check on 2021 f180;
     - Task 4, prior-point lp on 2017 eve.
3. **Epoch boundaries and weights.**
   - Expected: an election day belongs to the epoch it closes; a closed epoch uses its own w and the open epoch the previous election's; a kjördæmi poll in epoch e uses δ_e.
   - *Tests:*
     - Task 1, "epochs and weights follow the closing election";
     - Task 4, the "previous-w" and "next-epoch δ" mutants.
4. **Concentrations near their bound.**
   - Expected:
     - the bounded transforms start at their init when u = 0, and stay below the cap for any u;
     - a point at the cap is excluded from the equivalence check, never failed;
     - the builder stops when no national poll is admitted, since `phi_init`, computed in transformed data from the national poll sizes, would be NaN.
   - *Tests:* Task 4, "bounded concentrations"; Task 1, "the guards fire".
5. **Stage-1 leakage.**
   - Expected: stage 1 sees only elections dated before E, and E's masked vote share reaches nothing. Changing E's target row's masked columns in `info` leaves stage 1's Stan data byte-identical.
   - *Test:* Task 2, "the target row cannot reach stage 1".

---

## File map

**Created**

| File | Role |
|---|---|
| `R/harness/v2.R` | `read_v2_config()`, `v2_stan_data()`, `stage1_placeholder()`, `v2_variables()`, `v2_extract()`, `V2_SWITCHES` |
| `R/harness/stage1.R` | `stage1_data()`, `stage1_mu_f()` (fit + cache) |
| `R/harness/v2_reference.R` | `v2_lp_ref()`, `v2_simulate()`, `M_e()`, `V2_MUTANTS`, `v2_mutants()`, `permute_v2()` |
| `Stan/forecast_v2.stan` | All of §6, switched |
| `Stan/fundamentals_v2.stan` | Stage 1 |
| `R/harness/v2_equivalence.R` | Plain script: P-step equivalence check |
| `R/harness/v2_prior_predictive.R` | Plain script → `harness/v2/prior_predictive.md` |
| `R/harness/compare_runs.R` | Plain script: stop rule, adoption rule, permutation z-statistics |
| `R/harness/recovery.R` | Plain script: recovery replicates → `harness/v2/recovery.md` |
| `R/harness/v2_checks.R` | Plain script: Tier 0 extras and past-election PPC → `harness/tier0/v2_checks.md` |
| `harness/configs/v2.json` and `v2-{noepoch,nonew,noshock,staticdelta,nofund,perm}.json` | Arm configs |
| `harness/configs/steps/v2s-{m0,p1,m1,m2,m3,m4,p2,m5}.json` | Staircase configs |
| `harness/runs/v2-steps.txt`, `harness/runs/tier0-v2.txt` | Run lists |
| `harness/v2/steps.md` | The step log: one section per step |
| `tests/harness/helper-v2.R` | Test-only helpers (Task 1 creates it; Tasks 2–4 extend it): data fixtures, planting helpers, model methods, points, lp comparison, `v2_test_cells()` |
| `tests/harness/test-v2-data.R` | Task 1: grid, Stan data, guards |
| `tests/harness/test-stage1.R` | Task 2 |
| `tests/harness/test-v2-reference.R` | Task 3: reference density, simulator, algebra, mutants |
| `tests/harness/test-v2-stan.R` | Tasks 4 and 9–14: Stan lp against the reference, per step |
| `tests/harness/test-v2-extract.R`, `tests/harness/test-arms.R` | Task 5 |
| `tests/harness/test-compare-runs.R` | Task 7 |
| `tests/harness/test-v2-equivalence.R` | Tasks 8 and 13 |
| `tests/harness/test-v2-recovery.R` | Task 15 |
| `harness/v2/steps/` | Per-step reports (`<step>.md`) and equivalence CSVs (`<step>-equivalence.csv`) |

Each run directory also gains `ppc_results.parquet` (Task 5), which only `v2_checks.R` reads.

**Modified**
- `R/harness/tables.R`: `grid_dates()`.
- `R/harness/freeze.R`: `is_frozen()`.
- `R/harness/r0.R`: exports `fundamentals_rows()`, unchanged otherwise.
- `R/harness/arms.R`: entries, `role`, `variables` as a function of the config.
- `R/harness/fit_cell.R`: reads variables from the config; drops zero-size parameters; writes `role` into `meta.json`; refuses a 2017 cell before the freeze.
- `R/harness/runs.R`: `summarise_diagnostics()` records `n_leapfrog`.
- `R/harness/scorer.R`: refuses roles `check` and `dev`, and a run whose `meta.json` role differs from its arm's.
- `R/harness/tier0_report.R`: adds an ESS-per-1,000-gradients column.
- `tests/harness/test-r0-extract.R`: the line asserting `expect_named(ARMS, "r0")`.
- `tests/harness/test-freeze.R`, `test-fit-cell.R`, `test-scorer.R`, `test-tier0-report.R`: the tests Tasks 1, 5 and 7 add.
- Docs, each in the task that adds what it documents: `harness/README.md`, `R/CLAUDE.md`, `Stan/CLAUDE.md`.

## Arms this plan adds

Every arm below but `r0` runs `Stan/forecast_v2.stan` with its own config. Roles are what `score_cell()` checks (Task 5): it never scores a `check` or `dev` arm, and Plan 4 may add roles for the arms it scores (fallback, exploratory, baseline). Nothing is scored in this plan.

| Arm | Role | Config | What it is | Task |
|---|---|---|---|---|
| `r0` | reference | (unchanged) | aa2ac1a, gains the role field only | 5 |
| `v2` | candidate | `harness/configs/v2.json` | `v2s-m5` with the adopted forms | 14 |
| `v2-noepoch`, `v2-nonew`, `v2-noshock`, `v2-staticdelta`, `v2-nofund` | ablation | `harness/configs/v2-*.json` | `v2` with one switch off (Task 14's table) | 14 |
| `v2-perm` | check | `harness/configs/v2-perm.json` | `v2` with both orders reversed (`order` = `"reversed"`) | 16 |
| `v2s-m0`, `v2s-p1`, `v2s-m1`, `v2s-m2`, `v2s-m3`, `v2s-m4`, `v2s-p2`, `v2s-m5` | dev | `harness/configs/steps/v2s-*.json` | The staircase, one component or form per step | 4, 8–14 |

## Checks per step

An M-step changes the posterior; a P-step must not. Each row's checks must pass before the next step starts.

| Step | Kind | Adds | Checks |
|---|---|---|---|
| `v2s-m0` | M | The core: walk, polls, houses, static δ, bounded concentrations, proposals 1 and 2 (s_δ0); proposal 4's scales as data | Reference lp on 3 cells; core mutants; exact permutation identity; prior predictive (Task 6); stop rule at 400 + 400 (Task 7) |
| `v2s-p1` | P | Anchored walk (`walk_form = 1`) | Equivalence on 3 cells plus negative controls, and on 2024 eve under the reversed orders; adoption rule, seeds 2701 and 2702 |
| `v2s-m1` | M | Epoch bias (§6.4) | Step protocol, mutants `house_walk_from_first_poll` and `newcomer_mean_of_mu1` |
| `v2s-m2` | M | Newcomer pooling (§6.4) | Step protocol, mutant `newcomer_mean_over_all` |
| `v2s-m3` | M | Election-day shock (§6.6) | Step protocol, mutant `shock_unscaled` |
| `v2s-m4` | M | δ walk (§6.7), proposal 5 | Step protocol, mutant `delta_years_unscaled`, the `delta_years` values test |
| `v2s-p2` | P | Centred δ (`delta_form = 1`) | Equivalence on 3 cells plus a swapped-Kronecker control; adoption rule |
| `v2s-m5` | M | Fundamentals pseudo-observation (§6.5) | Step protocol, mutant `fund_uncentred`, the 0.22 construction check |
| `v2` | final | — | Recovery (Task 15); Tier 0 on its forecast cells; `v2-perm` z-statistics; past-election PPC; equivalence re-run on the final config (Task 16) |

## The step protocol

Tasks 9–12 and 14 (the M-steps) each follow these steps, after Task 4 has done them for the core. Each such task names only its component, its parameters, its mutant and its prior-predictive summary. Below, `<step>` is the step's arm name, such as `v2s-m1`.

- [ ] **A. Config, arm and reference.**
  - Write `harness/configs/steps/<step>.json`: the previous step's config with the task's switch on.
  - Register `<step>` in `ARMS` with role `dev`.
  - Extend `v2_lp_ref()` and `v2_simulate()` with the component, from the spec section the task cites.
  - Add the task's mutants to `V2_MUTANTS`, each with the switch that enables it.
- [ ] **B. Failing test.** Write the step's lp test in `test-v2-stan.R`, as Task 4's first test with `v2_test_cells(<step config>)` and the mutants of `v2_mutants(data)`: the step config, with `walk_form` and `delta_form` set to 0, on 2024 eve and 2021 f180 (short-fit and off-posterior points), and on 2017 eve (prior points). The adopted forms are covered by the P-steps' equivalence checks, which Task 16 re-runs on the final config. Run `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R v2-stan`. Expected: FAIL, because the Stan file lacks the component.
- [ ] **C. Stan.** Add the component to `Stan/forecast_v2.stan` behind its switch, with zero-size parameters when the switch is off.
- [ ] **D. Pass.** Re-run the step's lp test. Expected: PASS, with:
  - the range of `lp_stan − lp_ref` below tol = 1e-9 · max(1, |lp_ref|);
  - each of the task's mutants above 100 × tol on every cell where it is exercised (`mutant_exercised()`), and exercised on at least one cell;
  - every earlier step's lp test still passing.
- [ ] **E. Prior predictive.** Add the task's summary to `R/harness/v2_prior_predictive.R` and re-run it (Task 6).
- [ ] **F. Sampler check.**
  - Add the step's lines to `harness/runs/v2-steps.txt` (2024 eve and 2021 f180, seed 2701; seed 2702 as well when the next task is a P-step).
  - Run `zsh R/harness/queue.sh harness/runs/v2-steps.txt 400 400` (the sha-skip passes over finished steps).
  - Then run `LC_ALL=en_US.UTF-8 Rscript R/harness/compare_runs.R stop <step>`.
  - Append its output to `harness/v2/steps.md`.
  - A stop (Decision 5) ends the task there, and the step log says why.
- [ ] **G. Docs and commit.** Add the switch's row to the switch table in `Stan/CLAUDE.md` (Task 4 creates it), and finish the step log. Commit with `feat(v2): <step> <component>`.

The compute this costs: two fits per M-step and four per P-step at 400 + 400. The anchored R0 took 14.5 and 16.2 minutes at 1000 + 1000 on 2024 eve and 2021 f180 (`R/sampler_check/RESULTS.md`, 28 Oct). The step log records each step's own minutes.

---

### Task 1: The grid and V2's Stan data

**Files:**
- Modify: `R/harness/tables.R` (add `grid_dates()`), `R/harness/freeze.R` (add `is_frozen()`), `R/CLAUDE.md`
- Create: `R/harness/v2.R` (`read_v2_config()`, `v2_stan_data()`, `stage1_placeholder()`, `V2_SWITCHES`), `harness/configs/steps/v2s-m0.json`, `tests/harness/helper-v2.R`
- Test: `tests/harness/test-v2-data.R`, `tests/harness/test-freeze.R`

**Interfaces:**
- Consumes:
  - `info_set(E, F)` (`R/harness/info_set.R`);
  - `check_sealed()` (`R/harness/freeze.R`, internal);
  - `epoch_of()`, `expected_valid_votes()` and `valid_votes()` (`R/harness/tables.R`);
  - `prev_national_shares()` (`R/harness/truth.R`);
  - `ELECTIONS`, `WINDOW_START`, `POLL_PARTIES` and `KJORDAEMI` (`R/harness/constants.R`).
- Produces:
  - **`grid_dates(info, campaign_days = 47L) -> Date`.** Sorted and strictly increasing, with E last. Every observation date is on or before F; the only dates after F are E − `campaign_days` (when it is after F) and E.
  - **`is_frozen(root = project_root()) -> logical(1)`** in `freeze.R`: TRUE when `harness/FREEZE.md` and `harness/FREEZE.sha256` pass checks 2–5 of `freeze_guard()` (`check_sealed()`, wrapped in `tryCatch`), else FALSE.
  - **`read_v2_config(path) -> list`.** The keys:
    - `campaign_days` 47, `desired_weight` 0.22, `weight_time` 180, `delta_years_bar` 4;
    - the switches in `V2_SWITCHES` = `epoch_bias`, `newcomer_pool`, `shock`, `delta_walk`, `fundamentals`, `walk_form`, `delta_form`, each 0 or 1;
    - `order`: `"default"` (`POLL_PARTIES` and `KJORDAEMI$kjordaemi`, restricted to the cell) or `"reversed"` (both reversed);
    - `prior_tau_mu`, `prior_s_bar_E`, `prior_s_bar_zeta`, `prior_sd_log_kappa`, `prior_mu_phi`, `prior_sd_mu1`, `prior_rate_sigma_gamma`;
    - `stage1` = {`seed`, `iter_warmup`, `iter_sampling`}.

    It stops on a missing or unknown key, a switch outside {0, 1}, or another `order`.
  - **`v2_stan_data(info, cfg, stage1 = stage1_mu_f, party_order = NULL, root = project_root()) -> list(data, meta)`.**
    - `data` holds the fields below in this order (`stan_data_sha()` depends on field order).
    - `meta` is a superset of the scorer's `META_FIELDS`: `party_order`, `kjordaemi_levels`, `house_levels`, `newcomers`, `eday_index` (= G), `n_pred`, `n_pred_k`, `n_polls_nat`, `n_polls_kd`, `max_poll_date`, `coalition_event`, `coordinate_party`, `n_grid`, `n_epoch`, `n_closed`, `n_kd_polls`, `switches` and `stage1_sha256` (null when `fundamentals = 0`).
    - Its guards stop with `"v2 input: <what>"`. They include the §6.2 grid checks on `grid_dates()`' output ("v2 input: observation after F", "v2 input: non-positive grid step") and "v2 input: party_order is not a permutation of the cell's parties".
    - `party_order` is for tests only: it overrides the config's `order`.
    - `stage1` is called only when `cfg$fundamentals == 1`. Until Task 2, the default is a stub that stops.
    - **On a 2017 cell** (`target_cell(E, F)$confirmatory`), unless `is_frozen(root)`: `draw_forecast = 0`, and μ_f comes from `stage1_placeholder(info)` instead of `stage1`. `stage1_placeholder(info) -> list(mu_f = 0 over S_f, sha256 = "placeholder", diagnostics = list())`.

The data fields (the contract between the builder, the Stan file and `v2_reference.R`):

**Sizes and the grid**

| Field | Type, dims | Meaning |
|---|---|---|
| `P`, `K`, `H`, `G`, `n_epoch` | int | Parties, kjördæmi, houses (pollsters), grid dates, epochs up to the open one |
| `delta_days` | vector[G−1], > 0 | Δ_i in days |
| `campaign` | array[G−1] int 0/1 | c_i |
| `grid_epoch` | array[G] int | Epoch of each grid date |
| `exists` | array[G, P] int 0/1 | A_t |
| `w` | matrix[n_epoch, K], rows sum to 1 | Kjördæmi weights per epoch |
| `delta_years` | vector[n_epoch] | Δyears_e; entry 1 is 0 and unused |

**National polls**

| Field | Type, dims | Meaning |
|---|---|---|
| `N_nat`, `grid_nat`, `house_nat` | int, array[N_nat] int | National polls, their grid index and house |
| `y_nat`, `n_rep_nat`, `idx_nat` | array[N_nat, P] int, array[N_nat] int, array[N_nat, P] int | Counts in party order; number reported; reported parties by identity, padded with 0 |

**Kjördæmi polls**

| Field | Type, dims | Meaning |
|---|---|---|
| `J`, `N_kd`, `poll_kd`, `kd_kd`, `grid_kd`, `house_kd` | int, arrays[N_kd] | Kjördæmi polls (a poll is one (date, fyrirtaeki)), their rows, kjördæmi, grid index and house |
| `y_kd`, `n_rep_kd`, `idx_kd` | as for national polls | Each row's counts and reported parties |

**Results**

| Field | Type, dims | Meaning |
|---|---|---|
| `n_closed`, `grid_res` | int, array[n_closed] int | Closed elections in the window, and their grid index |
| `y_res`, `n_rep_res`, `idx_res` | array[n_closed, K, P] int, array[n_closed] int, array[n_closed, P] int | Official kjördæmi votes; the parties present nationally |

**Houses and newcomers**

| Field | Type, dims | Meaning |
|---|---|---|
| `house_first_epoch` | array[H] int | Epoch of each house's first admitted poll |
| `entry_epoch` | array[P] int | 0 for a party present at the window's start, else its entry epoch (`harness/entry_table.csv`, restricted to the cell) |

**Shock and forecast**

| Field | Type, dims | Meaning |
|---|---|---|
| `d_shock` | matrix[n_epoch, P] | √(min(0.2 / π̄_{e,p}, 10)). π̄ is the previous election's national share, or 0.02 for a party with none. The row for the open epoch serves E |
| `N_open` | array[K] int | Previous election's valid votes (`info$valid_votes_open`) |
| `draw_forecast`, `latent_path` | int 0/1 | GQ switches. The builder writes 1 and 0 |

**Fundamentals and switches**

| Field | Type, dims | Meaning |
|---|---|---|
| `n_sf`, `sf_idx`, `mu_f` | int, array[n_sf] int, vector[n_sf] | Stage 1's output over S_f. n_sf = 0 when `fundamentals = 0` |
| `desired_weight`, `weight_time`, `campaign_days`, `delta_years_bar` | real, real, int, real | From the config |
| `prior_tau_mu`, `prior_s_bar_E`, `prior_s_bar_zeta`, `prior_sd_log_kappa`, `prior_mu_phi`, `prior_sd_mu1`, `prior_rate_sigma_gamma` | real | Tier 3b's prior scales and Decision 1's values |
| the seven switches | int 0/1 | `V2_SWITCHES` |

`phi_init` is not a data field. The Stan file computes it in transformed data as the mean of log(rowSums(`y_nat`)), as aa2ac1a does from the poll sizes, and `v2_lp_ref()` recomputes it the same way.

- [ ] **Step 1: Write the failing tests** in `tests/harness/test-v2-data.R` (each calls `skip_without_snapshot()`):

```r
test_that("the grid holds every observation date, F and E, strictly increasing", {
  info <- info_2024eve()
  g <- grid_dates(info)
  expect_true(all(diff(g) > 0))
  expect_equal(tail(g, 2), as.Date(c("2024-11-29", "2024-11-30")))
  expect_true(all(c(info$polls_nat$date, info$polls_kd$date, as.Date(c("2016-10-29", "2017-10-28", "2021-09-25"))) %in% g))
  expect_true((as.Date("2024-11-30") - 47) %in% g)
})
test_that("a 180-day grid ends at F, the campaign start and E; a 47-day grid has F as the campaign start", {
  skip_without_snapshot()
  expect_equal(tail(grid_dates(info_set(as.Date("2024-11-30"), as.Date("2024-06-03"))), 3), as.Date(c("2024-06-03", "2024-10-14", "2024-11-30")))
  expect_equal(tail(grid_dates(info_set(as.Date("2021-09-25"), as.Date("2021-08-09"))), 2), as.Date(c("2021-08-09", "2021-09-25")))
})
test_that("epochs and weights follow the closing election", {
  d <- d_2024eve()
  i21 <- d$grid_res[3]
  expect_equal(d$grid_epoch[i21], 3L)
  expect_equal(d$w[4, ], d$w[3, ])
  expect_equal(unname(d$w[3, ]), unname(valid_votes(read_truth(), as.Date("2021-09-25"))$w))
})
test_that("2021 f180 keeps J with no kjordaemi row, entering in the open epoch", {
  skip_without_snapshot()
  b <- v2_stan_data(info_set(as.Date("2021-09-25"), as.Date("2021-03-29")), cfg_m0())
  j <- match("S\u00f3s\u00edalistaflokkurinn", b$meta$party_order)
  expect_false(is.na(j))
  expect_false(any(b$data$y_kd[, j] > 0))
  expect_equal(b$data$entry_epoch[j], b$data$n_epoch)
})
test_that("the 2017 cells have nine parties, one closed epoch, M entering in the open one, and no forecast", {
  skip_without_snapshot()
  for (Fc in c("2017-10-14", "2017-10-27")) {
    b <- v2_stan_data(info_set(as.Date("2017-10-28"), as.Date(Fc)), modifyList(cfg_m0(), list(fundamentals = 1L)),
      stage1 = \(...) stop("stage 1 called"), root = withr::local_tempdir())
    expect_equal(b$data$P, 9L); expect_equal(b$data$n_closed, 1L); expect_equal(b$data$n_epoch, 2L)
    expect_equal(b$data$entry_epoch[match("Mi\u00f0flokkurinn", b$meta$party_order)], 2L)
    expect_equal(b$data$draw_forecast, 0L); expect_identical(b$meta$stage1_sha256, "placeholder")
  }
})
test_that("partial polls and results are gathered by identity", {
  # polls before 2017-09-21 omit M and J (positions 8, 9) but report Annað (10); so does the 2016 result
  d <- d_2024eve()
  for (i in seq_len(d$N_nat)) expect_equal(d$idx_nat[i, seq_len(d$n_rep_nat[i])], which(d$y_nat[i, ] > 0))
  expect_equal(d$idx_res[1, seq_len(d$n_rep_res[1])], which(colSums(d$y_res[1, , ]) > 0))
  expect_true(any(d$n_rep_nat < d$P & d$y_nat[, d$P] > 0))
})
test_that("the guards fire", {
  info <- info_2024eve(); cfg <- cfg_m0()
  expect_error(v2_stan_data(info, cfg, party_order = j_first(info)), "v2 input: coordinate party")
  expect_error(v2_stan_data(info, cfg, party_order = head(info$parties, -1)), "v2 input: party_order is not a permutation")
  expect_error(v2_stan_data(zero_votes_in_one_kd(info), cfg), "v2 input: .* no votes in")
  expect_error(v2_stan_data(duplicate_kd_row(info), cfg), "v2 input: kj.rd.mi poll .* twice")
  expect_error(v2_stan_data(no_national_polls(info), cfg), "v2 input: no national poll")
  expect_error(v2_stan_data(poll_after_F(info), cfg), "v2 input: observation after F")
  expect_error(read_v2_config(config_with(order = "backwards")), "order")
})
```

  `helper-v2.R` (this task creates it) holds the fixtures, each calling `skip_without_snapshot()` first where it reads the snapshot:
  - `cfg_m0()` reads `harness/configs/steps/v2s-m0.json`; `config_with(...)` writes a temporary copy of it with the named keys changed and returns its path;
  - `info_2024eve()` and `d_2024eve()` are the 2024-eve `info_set()` and `v2_stan_data(..., cfg_m0())$data`;
  - `j_first(info)` is the cell's party order with Sósíalistaflokkurinn first (absent at the 2016 and 2017 results);
  - the planting helpers `zero_votes_in_one_kd()`, `duplicate_kd_row()`, `no_national_polls()` and `poll_after_F()` each edit one table of `info`.

  Write the Icelandic strings with `\u` escapes (Global Constraints). In `test-freeze.R`, add "is_frozen is FALSE without a sealed freeze and TRUE with one", built in a temporary git repository as the file's existing `freeze_guard()` tests are.

- [ ] **Step 2: Run the tests and see them fail.** Run `LC_ALL=en_US.UTF-8 Rscript tests/run.R v2-data`. Expected: FAIL (`could not find function "grid_dates"`).
- [ ] **Step 3: Implement.** `grid_dates()` goes in `tables.R` and `is_frozen()` in `freeze.R`. `read_v2_config()`, `v2_stan_data()` and `stage1_placeholder()` go in `v2.R`. `v2s-m0.json` holds every switch at 0, `order` = `"default"`, and Decision 1's values (`prior_mu_phi` = log 1000, `prior_sd_mu1` = 1, `prior_rate_sigma_gamma` = 1). Add `v2.R` and `is_frozen()` to the harness table in `R/CLAUDE.md`.
  - Kjördæmi poll ids are dense integers in date order.
  - Houses are ordered by first admitted poll.
  - The party order is the config's, restricted to `info$parties`.
- [ ] **Step 4: Run the tests and see them pass.** Run `LC_ALL=en_US.UTF-8 Rscript tests/run.R v2-data` and `… freeze`. Expected: PASS.
- [ ] **Step 5: Run the full fast suite.** Run `LC_ALL=en_US.UTF-8 Rscript tests/run.R`. Expected: PASS, `escapes` included.
- [ ] **Step 6: Commit.** `git add R/harness/tables.R R/harness/freeze.R R/harness/v2.R R/CLAUDE.md harness/configs/steps/v2s-m0.json tests/harness/helper-v2.R tests/harness/test-v2-data.R tests/harness/test-freeze.R && git commit -m "feat(v2): V2 Stan data from info_set(), the §6.2 grid checks, and 2017 forecast-free before the freeze"`

### Task 2: Stage 1, the fundamentals pseudo-observation's source

**Files:**
- Create: `Stan/fundamentals_v2.stan`, `R/harness/stage1.R`
- Modify: `R/harness/v2.R` (the default `stage1` becomes `stage1_mu_f`), `R/harness/r0.R` (export `fundamentals_rows()`), `tests/harness/helper-v2.R` (`v2_methods()`, `n_unc()`), `R/CLAUDE.md`, `Stan/CLAUDE.md`
- Test: `tests/harness/test-stage1.R`

**Interfaces:**
- Consumes: `info$fundamentals` (train, target, S_f); `fundamentals_rows(info, party_order)` from `R/harness/r0.R`, which this task exports (add `#' @export`; its body is unchanged). Its rows are production's table (training rows, E's target rows and an Annað target row); `stage1_data()` keeps the columns §6.5 names and restricts each election's rows to S_f(d).
- Produces:
  - **`stage1_data(info) -> list`.** The Stan data of `fundamentals_v2.stan`, with the training elections' dates as the attribute `election_dates`. Training elections are those dated before E only, and each election's counts are restricted to S_f(d).
  - **`stage1_mu_f(info, cfg, root = project_root()) -> list(mu_f = named numeric over S_f(E), sha256 = chr, diagnostics = list)`.** It fits with `cfg$stage1` and caches in `results_root()/stage1/<sha256>/mu_f.json`. The sha256 covers the Stan file, the data JSON, the seed, the iterations and the CmdStan version. It stops unless the fit's max R-hat ≤ 1.01 and it has no divergences.
    - On a 2017 cell it stops before any fit, unless `is_frozen(root)`: "stage 1: 2017 is not fitted before the freeze".
  - **`helper-v2.R`** gains `v2_methods(stan_file, data)` and `n_unc(m)`. `v2_methods()` compiles with `compile_model_methods = TRUE`, sets `data$draw_forecast` to 0 if the field exists, and attaches methods through a one-iteration `fixed_param` run in a temporary directory, which it deletes unread. `n_unc(m)` is the length of the unconstrained parameter vector.

`fundamentals_v2.stan` is §6.5's stage 1:
- **Predictor.** η_{d,p} = α_p + β_lag x + β_inc inc_years + β_vnv vnv + β_growth growth, for each p ∈ S_f(d), centred within each election.
- **Intercepts.** α is a `sum_to_zero_vector` over the party labels.
- **Likelihood.** y_d ~ DirMult(φ_f softmax(η_d)).
- **Priors** (§6.10): β_lag, β_inc ~ N(0, 1); β_vnv, β_growth ~ N(0, 5); α ~ N(0, 1).
- **The bound.** φ_f⁻¹ ~ exponential(1), declared `<lower = exp(-20)>`: the log φ_f < 20 bound (Decision 4).
- **Output.** The generated quantity `eta_E` is the centred predictor at E over S_f(E). Its posterior mean is μ_f.

- [ ] **Step 1: Write the failing tests.**

```r
test_that("the target row cannot reach stage 1", {
  skip_without_snapshot()
  info <- info_set(as.Date("2021-09-25"), as.Date("2021-03-29"))
  a <- stage1_data(info)
  info2 <- info
  info2$fundamentals$target[c("voteshare", "voteshare_change", "total_valid_votes")] <- 0.5
  expect_identical(stage1_data(info2), a)
  expect_true(all(attr(a, "election_dates") < as.Date("2021-09-25")))
})
test_that("stage 1's density is invariant to the order of party labels", {
  skip_unless_slow()
  d <- stage1_data(info_set(as.Date("2021-09-25"), as.Date("2021-03-29")))
  d_rev <- reverse_labels(d)
  m <- v2_methods("Stan/fundamentals_v2.stan", d); m_rev <- v2_methods("Stan/fundamentals_v2.stan", d_rev)
  set.seed(1)
  for (i in 1:5) {
    u <- rnorm(n_unc(m))
    pars <- m$constrain_variables(u); pars$alpha <- rev(pars$alpha)
    expect_equal(m_rev$log_prob(m_rev$unconstrain_variables(pars), jacobian = FALSE), m$log_prob(u, jacobian = FALSE), tolerance = 1e-10)
  }
})
test_that("stage 1 is cached by its inputs", {
  skip_unless_slow()
  withr::local_envvar(HARNESS_RESULTS = withr::local_tempdir())
  info <- info_set(as.Date("2021-09-25"), as.Date("2021-03-29")); cfg <- cfg_m0()
  a <- stage1_mu_f(info, cfg); b <- stage1_mu_f(info, cfg)
  expect_identical(a, b); expect_named(a$mu_f, info$fundamentals$S_f)
})
test_that("stage 1 refuses a 2017 cell before the freeze", {
  skip_without_snapshot()
  expect_error(stage1_mu_f(info_set(as.Date("2017-10-28"), as.Date("2017-10-27")), cfg_m0(), root = withr::local_tempdir()),
    "stage 1: 2017 is not fitted before the freeze")
})
```

- [ ] **Step 2: Run them and see them fail.** Run `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R stage1`. Expected: FAIL.
- [ ] **Step 3: Implement** `Stan/fundamentals_v2.stan` and `R/harness/stage1.R`, and `reverse_labels(d)` in `helper-v2.R` (the stage-1 data with its party labels in reverse order). The cache write is atomic: write a temporary file, then rename. Add `stage1.R` and `fundamentals_rows()` to `R/CLAUDE.md`'s harness table, and `fundamentals_v2.stan` to `Stan/CLAUDE.md`'s inventory.
- [ ] **Step 4: Run them and see them pass.** Expected: PASS, with stage 1's diagnostics printed for 2021 f180.
- [ ] **Step 5: Commit.** `git commit -m "feat(v2): stage-1 fundamentals fit (reference-invariant, cached)"`

### Task 3: The reference log density and simulator (core)

**Files:**
- Create: `R/harness/v2_reference.R`
- Modify: `tests/harness/helper-v2.R` (`random_sigma()`), `R/CLAUDE.md`
- Test: `tests/harness/test-v2-reference.R`

**Interfaces:**
- Consumes: the Task 1 data contract.
- Produces:
  - **`v2_lp_ref(pars, data, mutant = "none") -> numeric(1)`.** The log density that Stan's `log_prob(u, jacobian = FALSE)` evaluates in the reference forms: the parameters' density on their constrained values, plus the bounded transforms' log-Jacobians that the model block adds by hand, full normalising constants included. It stops unless `data$walk_form` and `data$delta_form` are 0; the P-steps' forms are checked against the reference form by the equivalence check instead (Tasks 8 and 13).
    - `pars` is the named list that `constrain_variables(..., transformed_parameters = FALSE)` returns.
    - It is written from §6, with no reference to the Stan file.
  - **`v2_simulate(data, seed, observe = TRUE) -> list(pars, truth, data)`.**
    - `pars` is a prior draw in the reference form's parameters.
    - `truth` holds the natural quantities: θ path, π_nat at the poll dates, μ_e, γ, δ_e, ε_e, φ_h, φ_K and κ.
    - `data` is the input with every count field replaced by simulated draws (when `observe`).
  - **`M_e(X, w) -> matrix`**: (I − 1wᵀ) X C.
  - **`V2_MUTANTS`** is a tibble (`mutant`, `switch`, `min_closed`), one row per failure mode; `switch` is NA for the core's and names the switch that enables a later step's. **`v2_mutants(data) -> chr`** returns the mutants whose switch is NA or 1 in `data` and whose `min_closed` ≤ `data$n_closed`, so a test never asks a mutant to change a density it cannot reach. The core's are:
    - `"positional_gather"`: `y[1:n_rep]` against the first `n_rep` parties;
    - `"previous_w"`: closed epochs from the second on use the previous election's w (`min_closed` 2; the first has no earlier w in the data);
    - `"next_epoch_delta"`: results and kjördæmi polls of closed epoch e use δ_{e+1}, and the open epoch is unchanged (`min_closed` 2: with one closed epoch, the static δ_1 and δ_2 share w and are equal);
    - `"close_last_kjordaemi"`: δ closes on kjördæmi K instead of M_e;
    - `"conditional_walk"`: a normal on a `sum_to_zero_vector` innovation. It is as permutation-invariant as the true form, so only the comparison with Stan (Task 4) and the algebra test below tell them apart.
  - **`permute_v2(pars, data, party_perm, kd_perm) -> list(pars, data)`.** For the reference forms only. It maps a point and its data to the permuted order through the natural quantities, re-deriving `L_Omega`, `z_walk` and the δ raw draws. It returns `ljac_L_Omega`, the change in log|∂Ω/∂L| = Σ_{k=2}^{P} (P − k) log L_kk, as an attribute.

The core (§6.2–§6.7 with all five switches off). The walk:
- θ_1 ~ normal(0, 2) on `sum_to_zero_vector[P]`.
- Coordinates 2:P: θ_{i+1} = θ_i + √Δ_i s_i `L_walk` z_i, with z_i ~ N(0, I_{P−1}), s_i = 1 + τ_camp c_i and `L_walk` = chol((CΣCᵀ)[2:P, 2:P]). Party 1 is −Σ of the rest.
- Priors: σ ~ half-t(3, 0, 0.02), `L_Omega` ~ LKJ-Cholesky(10), τ_camp ~ exponential(1).

House bias, constant over epochs:
- γ_{e,h} = μ_1 + d_h, with μ_1 ~ normal(0, `prior_sd_mu1`) on `sum_to_zero_vector[P]`, d_h = C(σ_γ ε_h), ε_h ~ N(0, I_P) and σ_γ ~ exponential(`prior_rate_sigma_gamma`).
- **Newcomers** p entering in epoch e_p:
  - μ_{e,p} for e ≥ e_p is the mean of μ_1 over the parties existing before epoch e_p (present at the window's start, or entering in an earlier epoch), plus m_new + s_new η_p, with η_p ~ N(0, 1), m_new = 0 and s_new = 0.2023; then re-centred. (With μ constant over epochs this is §6.4's μ_{e_p}; Task 9 restates it for a walking μ.)
  - Each house's d_{h,p} restarts at σ_γ ε for epochs ≥ e_p.

Concentrations:
- log φ_h = 20 − (20 − `phi_init`)·exp(u_h), with log φ_h ~ N(μ_φ, σ_φ), μ_φ ~ N(`prior_mu_phi`, 1), σ_φ ~ half-N(0, 1), and `phi_init` the mean of log n over national polls.
- log φ_K = 20 − (20 − log 1000)·exp(u_K), with log φ_K ~ N(log 1000, 1).
- log κ = 2 − 2 exp(u_κ), with log κ ~ N(0, `prior_sd_log_kappa`).
- These are densities on the natural log concentrations. The sampled values are the u's, so the model block adds each bounded transform's log-Jacobian, log(cap − init) + u, as aa2ac1a does for `log_phi_u`, and the reference adds the same.

δ, static:
- δ_e = M_e(s_δ0 ⊙ ζ), with ζ ∈ ℝ^{K×P} iid N(0, 1).
- M_e(X) = (I − 1 w_eᵀ) X C.
- log s_δ0 = μ_sd + τ_sd r, with r ~ N(0, 1), μ_sd ~ N(log 0.3, 0.5) and τ_sd ~ half-N(0, 0.5).

Likelihoods:
- **National poll:** DirMult(φ_h · normalise(π_nat,d[S] ⊙ exp(γ[S]))), with π_nat,d = Σ_k w_{e,k} softmax_{A_d}(θ_d + δ_{e,k}).
- **Kjördæmi poll row:** DirMult(κ φ_h softmax_S(ν_j + δ_{e,k})), with ν_j ~ normal(0, 2) on `sum_to_zero_vector[P]`.
- **Result:** DirMult(φ_K softmax_A(θ_{t_e} + δ_{e,k})).

- [ ] **Step 1: Write the failing tests** (no Stan):

```r
test_that("the walk's covariance algebra holds", {
  set.seed(1); P <- 10; C <- diag(P) - 1 / P; J <- matrix(1, P, P)
  S <- random_sigma(P)
  Q <- C %*% S %*% C
  minors <- vapply(seq_len(P), \(k) determinant(Q[-k, -k])$modulus, numeric(1))
  expect_lt(diff(range(minors)), 1e-10)
  expect_gt(max(abs(S - S %*% J %*% S / sum(S) - Q)), 1e-6)
  iso <- 0.3^2 * diag(P)
  expect_equal(iso - iso %*% J %*% iso / sum(iso), C %*% iso %*% C)
})
test_that("M_e lands in both constraints and commutes with a kjordaemi permutation", {
  set.seed(2); K <- 6; P <- 10
  w <- prop.table(runif(K)); X <- matrix(rnorm(K * P), K)
  Y <- M_e(X, w)
  expect_equal(rowSums(Y), rep(0, K)); expect_equal(drop(w %*% Y), rep(0, P))
  o <- sample(K)
  expect_equal(M_e(X[o, ], w[o]), Y[o, ])
})
test_that("the reference density is exactly permutation-invariant up to the L_Omega Jacobian", {
  d <- d_2024eve()
  for (seed in 1:3) {
    s <- v2_simulate(d, seed = seed, observe = FALSE)
    p <- permute_v2(s$pars, d, rev(seq_len(d$P)), rev(seq_len(d$K)))
    expect_equal(v2_lp_ref(p$pars, p$data) - v2_lp_ref(s$pars, d), attr(p, "ljac_L_Omega"), tolerance = 1e-10)
  }
})
test_that("each core mutant changes the density on real data", {
  d <- d_2024eve(); pars <- v2_simulate(d, seed = 1, observe = FALSE)$pars
  for (m in v2_mutants(d)) expect_gt(abs(v2_lp_ref(pars, d, m) - v2_lp_ref(pars, d)), 1e-6)
})
test_that("simulated data keep the design and replace the counts", {
  d <- d_2024eve(); sim <- v2_simulate(d, seed = 1)$data
  design <- c("P", "K", "H", "G", "n_epoch", "delta_days", "campaign", "grid_epoch", "exists", "w",
    "grid_nat", "house_nat", "n_rep_nat", "idx_nat", "grid_kd", "house_kd", "kd_kd", "grid_res", "N_open")
  expect_identical(sim[design], d[design])
  expect_identical(rowSums(sim$y_nat), rowSums(d$y_nat))
  expect_identical(apply(sim$y_res, 1:2, sum), apply(d$y_res, 1:2, sum))
  expect_true(all(sim$y_nat[d$y_nat == 0] == 0))
  expect_false(identical(sim$y_nat, d$y_nat))
})
```

  `random_sigma(P)` goes in `helper-v2.R`: diag(σ) Ω diag(σ), with σ ~ U(0.01, 0.05) and Ω from an LKJ(2) draw.

- [ ] **Step 2: Run them and see them fail.** Expected: FAIL.
- [ ] **Step 3: Implement** `v2_reference.R`. The DM log-pmf is written out with `lgamma`, normalising constant included. Add its row to `R/CLAUDE.md`'s harness table.
- [ ] **Step 4: Run them and see them pass.** Expected: PASS.
- [ ] **Step 5: Commit.** `git commit -m "feat(v2): R reference density and simulator for the V2 core"`

### Task 4: The core in Stan, reference form (step `v2s-m0`)

**Files:**
- Create: `Stan/forecast_v2.stan`
- Modify: `tests/harness/helper-v2.R`, `Stan/CLAUDE.md`
- Test: `tests/harness/test-v2-stan.R`

**Interfaces:**
- Consumes: Task 1's data, Task 3's `v2_lp_ref()`, `v2_simulate()` and `permute_v2()`.
- Produces (names that later tasks, the extractor and the reports use):
  - **Parameters** in the core:
    - `theta1` (`sum_to_zero_vector[P]`), `z_walk` (matrix[P−1, G−1]), `sigma` (vector<lower=0>[P]), `L_Omega`, `tau_camp`;
    - `mu1` (`sum_to_zero_vector[P]`), `house_raw` (matrix[H, P]), `sigma_gamma`, `new_raw`;
    - `log_phi_u` (vector[H]), `mu_phi`, `sigma_phi`, `log_phi_K_u`, `log_kappa_u`;
    - `delta_raw` (array[n_epoch or 1] matrix[K, P]), `s_delta0_raw` (vector[P]), `mu_sd`, `tau_sd`;
    - `nu` (array[J] `sum_to_zero_vector[P]`).
    - Standard-normal parameters carry `_raw` (`.claude/rules/stan-conventions.md`); `z_walk`, and later `z_shock`, keep production's `z_` prefix.
  - **Transformed parameters:** `theta` (matrix[P, G]), `mu` (array[n_epoch] vector[P]), `gamma` (array[n_epoch, H] vector[P]), `delta` (array[n_epoch] matrix[K, P]), `log_phi`, `log_phi_K`, `log_kappa`, `s_delta0`.
  - **Generated quantities:**
    - with `draw_forecast = 1` (size 0 otherwise): `votes_kd` (array[K, P] int), `pi_nat_eday[P]` (the vote-weighted aggregate of `votes_kd`, §6.8) and `pi_latent_E[P]` (Σ_k w_open,k softmax(θ_E + δ_open,k));
    - `log_lik` (vector[N_nat + N_kd + n_closed·K]);
    - `y_rep_res` (array[n_closed, K, P] int);
    - with `latent_path = 1`: `pi_latent` (matrix[P, G]).
  - **`helper-v2.R`** gains:
    - `lp_points(m, data, n_post = 20, n_off = 10, n_prior = 10, seed = 5100)`: unconstrained points. Posterior points come from a 50 + 20 fit at depth 8, off-posterior points have the scale parameters multiplied by exp(N(0, 0.5)), and prior points come from `v2_simulate()`. With `n_post = n_off = 0`, it fits nothing.
    - `expect_lp_matches(m, data, points)`: `m$log_prob(u, jacobian = FALSE)` minus `v2_lp_ref()` at each point has a range below 1e-9 · max(1, |lp_ref|). `expect_lp_rejects(m, data, points, mutant)`: the same range against the mutant reference exceeds 100 × that tolerance. `mutant_exercised(data, points, mutant)`: TRUE when the mutant reference differs from the true reference by more than 1e-6 at some point, a property of the reference and the data alone.
    - `bounded_at(m, data, u)`: the transformed parameters at a prior draw with every bounded `*_u` parameter set to u.
    - `v2_test_cells(cfg)`: a named list (`eve2024`, `f180_2021`, `eve2017`) of `list(data, n_post, n_off)`, built when called, never when the helper is sourced, and skipping without the snapshot. `data` is `v2_stan_data(..., cfg with both forms at 0, root = a temporary directory)$data`, so the 2017 cell is always its pre-freeze form. `eve2024` and `f180_2021` have `n_post = 20, n_off = 10`; `eve2017` has `n_post = n_off = 0` (prior points only).

- [ ] **Step 1: Write the failing tests** in `test-v2-stan.R` (all slow):

```r
test_that("v2s-m0: Stan's log density matches the reference up to a constant", {
  skip_unless_slow()
  cells <- v2_test_cells(cfg_m0()); hit <- character(0)
  for (cell in cells) {                               # 2024 eve, 2021 f180: posterior + off; 2017 eve: prior only
    m <- v2_methods("Stan/forecast_v2.stan", cell$data)
    pts <- lp_points(m, cell$data, n_post = cell$n_post, n_off = cell$n_off)
    expect_lp_matches(m, cell$data, pts)
    for (mu in v2_mutants(cell$data)) if (mutant_exercised(cell$data, pts, mu)) {
      expect_lp_rejects(m, cell$data, pts, mu); hit <- c(hit, mu)
    }
  }
  expect_setequal(unique(hit), v2_mutants(cells$eve2024$data))   # every mutant is exercised on some cell
})
test_that("positional-gather mutant is rejected", {
  skip_unless_slow()
  # 2024 eve: every poll before 2017-09-21 and the 2016 result omit M and J but report Annað
  cell <- v2_test_cells(cfg_m0())$eve2024
  m <- v2_methods("Stan/forecast_v2.stan", cell$data)
  expect_lp_rejects(m, cell$data, lp_points(m, cell$data, cell$n_post, cell$n_off), "positional_gather")
})
test_that("Stan's log density is permutation-invariant up to the L_Omega Jacobian", {
  skip_unless_slow()
  d <- v2_test_cells(cfg_m0())$eve2024$data
  rp <- rev(seq_len(d$P)); rk <- rev(seq_len(d$K))
  m <- v2_methods("Stan/forecast_v2.stan", d)
  m_p <- v2_methods("Stan/forecast_v2.stan", permute_v2(v2_simulate(d, 1, FALSE)$pars, d, rp, rk)$data)
  for (seed in 1:10) {
    s <- v2_simulate(d, seed = seed, observe = FALSE)
    p <- permute_v2(s$pars, d, rp, rk)
    lp <- m$log_prob(m$unconstrain_variables(s$pars), jacobian = FALSE)
    lp_p <- m_p$log_prob(m_p$unconstrain_variables(p$pars), jacobian = FALSE)
    expect_equal(lp_p - lp, attr(p, "ljac_L_Omega"), tolerance = 1e-9)
  }
})
test_that("bounded concentrations start at their init and stay below their caps", {
  skip_unless_slow()
  d <- v2_test_cells(cfg_m0())$eve2024$data
  m <- v2_methods("Stan/forecast_v2.stan", d)
  b0 <- bounded_at(m, d, 0)
  expect_equal(b0$log_phi, rep(mean(log(rowSums(d$y_nat))), d$H))
  expect_equal(b0$log_phi_K, log(1000)); expect_equal(b0$log_kappa, 0)
  for (u in c(-30, 30)) {
    b <- bounded_at(m, d, u)
    expect_true(all(b$log_phi < 20) && b$log_phi_K < 20 && b$log_kappa < 2)
  }
})
test_that("v2_methods attaches methods to a 2017 cell without drawing a forecast", {
  skip_unless_slow()
  m <- v2_methods("Stan/forecast_v2.stan", v2_test_cells(cfg_m0())$eve2017$data)
  expect_false(any(c("votes_kd", "pi_nat_eday", "pi_latent_E") %in% m$metadata()$stan_variables))
})
```

- [ ] **Step 2: Run them and see them fail.** Run `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R v2-stan`. Expected: FAIL (no Stan file).
- [ ] **Step 3: Write the core** in `Stan/forecast_v2.stan`.
  - `~` statements are fine: the constants they drop cancel in the constant-difference test.
  - Comment the dims in the data block (`.claude/rules/stan-conventions.md`).
  - Gather every likelihood by `idx_*`, never by position.
  - The switch blocks for later steps are absent until their tasks.
  - Add `forecast_v2.stan` to `Stan/CLAUDE.md`'s inventory, with a switch table (switch, values, the parameters it sizes, the step that adds it) holding the core's rows; later steps add theirs.
- [ ] **Step 4: Run the tests and see them pass.** Expected: PASS.
  - The constant's range is below 1e-9·max(1, |lp|) on every cell.
  - Each mutant is above 100 × tol.
  - The permutation identity holds to 1e-9.
  - Record each cell's range and its weakest mutant in the test output.
- [ ] **Step 5: Commit.** (Task 5 registers `v2s-m0` in `ARMS`.) `git commit -m "feat(v2): forecast_v2.stan core (reference form), tested against the R reference"`

### Task 5: Arms, extraction and the runner

**Files:**
- Modify: `R/harness/arms.R`, `R/harness/fit_cell.R`, `R/harness/runs.R`, `R/harness/scorer.R`, `R/harness/v2.R` (`v2_variables()`, `v2_extract()`), `tests/harness/test-r0-extract.R`, `R/CLAUDE.md`, `harness/README.md`
- Test: `tests/harness/test-v2-extract.R`, `tests/harness/test-arms.R`, `tests/harness/test-fit-cell.R`, `tests/harness/test-scorer.R`

**Interfaces:**
- Consumes: Task 4's GQ names; `run_id()`, `summarise_diagnostics()` and `read_run_meta()`.
- Produces:
  - **Each `ARMS` entry gains `role`:** `"reference"` (r0), `"candidate"` (v2), `"ablation"`, `"check"` (v2-perm) or `"dev"` (v2s-*). Plan 4 may add roles for the arms it scores.
    - `variables` may be a function of the config.
    - `arm(name)` is unchanged.
    - `v2_arm(config, role)` builds an entry with `stan_file = "Stan/forecast_v2.stan"`, `config = config` (a path under `harness/configs/`), `read_config = read_v2_config`, `build = v2_stan_data`, `extract = v2_extract`, `variables = v2_variables` and `role = role`.
  - **`v2_variables(cfg) -> chr`.** `votes_kd`, `pi_nat_eday`, `pi_latent_E` and `y_rep_res`, plus the parameter names present under `cfg`'s switches. That is, of these: `sigma`, `L_Omega`, `tau_camp`, `mu_phi`, `sigma_phi`, `log_phi`, `log_phi_K`, `log_kappa`, `sigma_gamma`, `mu`, `tau_mu`, `tau_mu_A`, `tau_house`, `m_new`, `s_new`, `s_bar_E`, `mu_sd`, `tau_sd`, `s_delta0`, `s_bar_zeta`, `s_zeta`, and `anchor_w` when `walk_form = 1`, so that its per-coordinate ESS reaches `key_diagnostics.csv` (Task 8).
  - **`v2_extract(draws, meta, dir) -> named chr`.** It writes `eday_kjordaemi`, `eday_national` (from `pi_nat_eday`), `latent_national` (from `pi_latent_E`) and `param_draws` in the §5.5 format of `r0_extract()`. It also writes `ppc_results` (from `y_rep_res`: `.chain`, `.iteration`, `.draw`, `election`, `kjordaemi`, `flokkur`, `votes`), the in-sample replicates of the closed elections, which only `v2_checks.R` reads.
    - `.draw` is global, 1..N.
    - It labels by the Stan indices in element names, through `meta$kjordaemi_levels` and `meta$party_order`.
  - **`fit_cell.R`:**
    - `variables <- if (is.function(a$variables)) a$variables(cfg) else a$variables`;
    - the gate-scope parameters are `intersect(names(model$variables()$parameters), fit$metadata()$stan_variables)`, because zero-size parameters are not in the CSV;
    - `meta.json` gains `role` (the arm's);
    - right after parsing its arguments, it stops on a 2017 cell unless `is_frozen()`: "fit_cell: 2017 is not fitted before the freeze (spec §5.7)".
  - **`summarise_diagnostics()`** adds `n_leapfrog`, the sum of post-warmup `n_leapfrog__`.
  - **`score_cell()`**, right after `read_run_meta()` and before `freeze_guard()`, looks up the arm's role in `ARMS`. It stops with `"score_cell: arm <a> has role <r>; check and dev arms are never scored"` for those two roles, and with `"score_cell: run role <r1> differs from arm <a>'s role <r2>"` when `meta.json` holds a different role. A `meta.json` without `role` (R0's existing runs) uses the arm's.

- [ ] **Step 1: Write the failing tests.**
  - `test-v2-extract.R`: synthetic draws of `votes_kd[k,p]` with a permuted element order come out labelled right, and `.draw` runs 1..16. Mirror `test-r0-extract.R`.
  - `test-arms.R`:
    - every arm has a non-empty role, and the v2 family's are those of the arms table;
    - every v2-family config file parses with `read_v2_config()`;
    - `v2_variables()` drops `tau_mu` when `epoch_bias = 0`.
  - `test-scorer.R`: a `meta.json` with `arm = "v2s-m0"` is refused with the role message before `freeze_guard()` runs, and one with `arm = "r0"` and `role = "dev"` is refused with the mismatch message.
  - `test-fit-cell.R` (fast): `fit_cell.R v2s-m0 2017-10-28 2017-10-27 1 10 10` exits 1 with the 2017 message and creates no run directory. It reads the real freeze state, so it skips once `is_frozen()` holds, and Plan 4 replaces it.
  - `test-fit-cell.R`: a slow end-to-end run of `fit_cell.R v2s-m0 2024-11-30 2024-11-29 1 10 10` under a temporary `HARNESS_RESULTS`. It writes the five parquets (the four of §5.5 and `ppc_results`), and `meta.json` holds `n_leapfrog` and the `META_FIELDS`.
  - `test-r0-extract.R`: change the line asserting `expect_named(ARMS, "r0")` to assert that `"r0"` is in `names(ARMS)` with role `"reference"`.
- [ ] **Step 2: Run them and see them fail.** Expected: FAIL.
- [ ] **Step 3: Implement.** The `ARMS` entries are r0, `v2s-m0` (dev), and the rest as their configs land (Tasks 8–14, 16). Update `R/CLAUDE.md`'s `arms.R` row (it says Phase 1 has `r0`) and describe the roles in `harness/README.md`.
- [ ] **Step 4: Run the full suite.** Run `HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R`. Expected: PASS.
- [ ] **Step 5: Commit.** `git commit -m "feat(harness): v2 arms with roles, extraction, leapfrog counts; scorer refuses check and dev arms; no 2017 fit before the freeze"`

### Task 6: Prior predictive check

**Files:**
- Create: `R/harness/v2_prior_predictive.R`, `harness/v2/prior_predictive.md`
- Modify: `R/CLAUDE.md` (its row)

**Interfaces:**
- Consumes: `v2_simulate()`, on the 2024-eve and 2021-f180 designs.
- Produces: `LC_ALL=en_US.UTF-8 Rscript R/harness/v2_prior_predictive.R <config> [n_draws=1000]`. It writes the report and stops if any summary leaves its band below. Later M-steps append their own rows.
  - The first three rows run under the config's values and, for comparison, under Decision 1's other value (§6.10's or the proposal's). A comparison row prints `PRIOR PASS` or `PRIOR FAIL` and never stops the script.
  - A config-value row stops it on a FAIL, with one exception: a bias row held at §6.10's value, which §6.10 fixes, prints `PRIOR WIDE <summary> <q05> <q95>` and goes to Brynjólfur as information.

**The core's summaries and their bands.** Each band is written here, before any V2 fit, and checked on the 5% and 95% quantiles over prior draws. A band catches a broken prior (a wrong scale, unit or sign), not a mistuned one: it is wide on purpose, and Brynjólfur reads the quantiles themselves in the report. No band is a tuning target.

| Summary (pp unless stated) | Band for the [5%, 95%] interval |
|---|---|
| A house's deviation d from the industry, on a 20% party (σ_γ: Decision 1) | inside [−10, 10] |
| Industry bias μ_1 on a 20% party (μ_1: Decision 1) | inside [−12, 12] |
| A poll's sd around its mean, n = 1,000, 20% party, at the prior φ_h (μ_φ: Decision 1) | inside [0.8, 6] |
| A party's kjördæmi deviation from its national share, 20% party (proposal 2) | inside [−25, 25] |
| A 20% party's 180-day walk change | 95% quantile in [2, 30] |

- [ ] **Step 1: Write the script.** Its pass/fail lines print `PRIOR PASS <summary>` or `PRIOR FAIL <summary> <q05> <q95>`.
- [ ] **Step 2: Run it** on `harness/configs/steps/v2s-m0.json`. Expected, from Decision 1's simulation: under the config's values, `PRIOR PASS` for the poll-sd row and every later row, and `PRIOR WIDE` for the two bias rows; under the comparison values, `PRIOR FAIL` for §6.10's μ_φ and `PRIOR PASS` for proposal 4's two rows. A FAIL that stops the script goes to Brynjólfur with the row; no prior is changed without his ruling.
- [ ] **Step 3: Commit** the script, its `R/CLAUDE.md` row and the report.

### Task 7: Sampler-check machinery, and the core's check

**Files:**
- Create: `R/harness/compare_runs.R`, `harness/runs/v2-steps.txt`, `harness/v2/steps.md`
- Modify: `R/harness/tier0_report.R` (one column), `R/harness/fit_cell.R` (an `lp__` row in `key_diagnostics.csv`), `R/CLAUDE.md` (`compare_runs.R`'s row; `tier0_report.R`'s new column)
- Test: `tests/harness/test-tier0-report.R`, `tests/harness/test-compare-runs.R`, `tests/harness/test-fit-cell.R`

**Interfaces:**
- Consumes: `meta.json` (with `n_leapfrog`) and the runs' `param_draws.parquet` and `eday_*.parquet`.
- Produces:
  - **`tier0_report.R`** gains "Min bulk ESS per 1,000 gradients (gate scope)" = 1000 · `min_ess_bulk` / `n_leapfrog`. It prints "–" for a `meta.json` without `n_leapfrog`, such as R0's existing Tier 0 runs.
  - **`compare_runs.R stop <arm> [run_list]`** prints one `STOP` or `GO` line per fit with the Decision 5 signals, and exits 1 on any `STOP`.
    - R-hat of `lp__` comes from `key_diagnostics.csv`, which gains an `lp__` row in Step 3.
    - The share of draws near a cap comes from `param_draws.parquet`.
  - **`compare_runs.R adopt <ref_arm> <new_arm> <equivalence_csv>`** prints the Decision 6 table and `ADOPT` or `KEEP`.
  - **`compare_runs.R perm <arm_a> <arm_b> [run_list]`** prints the Decision 8 statistics and `PERM PASS` or `PERM FAIL`. It reads the eday parquets of both runs and prints no share value.

- [ ] **Step 1: Write the failing tests.**
  - `tier0_report`: the new column on a synthetic `meta.json`, with "–" when the field is absent.
  - `compare_runs`: synthetic run directories produce `STOP` for each signal alone and `GO` for none; `perm` gives `PERM FAIL` on two runs whose shares differ by 10 MCSE.
  - `test-fit-cell.R`: Task 5's slow end-to-end run's `key_diagnostics.csv` has an `lp__` row with a finite R-hat.
- [ ] **Step 2: Run them and see them fail.**
- [ ] **Step 3: Implement.** In `fit_cell.R`, add `lp__` to `key_diagnostics.csv`.
- [ ] **Step 4: Run them and see them pass.**
- [ ] **Step 5: The core's sampler check.**
  - Add `v2s-m0` to `harness/runs/v2-steps.txt` on 2024 eve and 2021 f180, seeds 2701 and 2702 (Task 8 is a P-step).
  - Run `zsh R/harness/queue.sh harness/runs/v2-steps.txt 400 400` in the background.
  - Then run `tier0_report.R v2s-m0 harness/runs/v2-steps.txt harness/v2/steps/v2s-m0.md` and `compare_runs.R stop v2s-m0`.
  - Expected: `GO` on all four. On a `STOP`, record it in `harness/v2/steps.md` and report.
- [ ] **Step 6: Commit** `compare_runs.R`, `tier0_report.R`, `fit_cell.R`, the tests, `R/CLAUDE.md`, the run list, the step report and the step log.

### Task 8: P1, the anchored walk (step `v2s-p1`)

**Files:**
- Modify: `Stan/forecast_v2.stan` (`walk_form = 1`), `R/harness/arms.R` (`v2s-p1`, role `dev`)
- Create: `R/harness/v2_equivalence.R`, `harness/configs/steps/v2s-p1.json`
- Test: `tests/harness/test-v2-equivalence.R`

**Interfaces:**
- Consumes: the reference form's `theta` (transformed parameter); `helper-v2.R`.
- Produces:
  - **Parameters with `walk_form = 1`:** `anchor_w` (array[n_closed] vector[P−1]) and `z_walk` (matrix[P−1, G−1−n_closed]); `theta1` stays.
  - **`LC_ALL=en_US.UTF-8 Rscript R/harness/v2_equivalence.R --config=<config> --form=walk|delta|both --cell=<E>,<F> --out=<csv> [--n=30] [--n-off=15] [--stan=Stan/forecast_v2.stan] [--reverse]`.** It compares the config with the named form(s) set to 0 (the reference side) against the config as written (the new side). It prints `EQUIV PASS` or `EQUIV FAIL` and exits 1 on a FAIL. The config is a path under `harness/configs/`.
    - `--reverse` sets `order` to `"reversed"` on both sides, so Annað is the coordinate party and every A_j and B_j changes.
    - Both sides' data come from `v2_stan_data(..., root = a temporary directory)`. On a 2017 cell that is the pre-freeze form (`draw_forecast = 0`, placeholder μ_f), and the script stops unless `votes_kd`, `pi_nat_eday` and `pi_latent_E` are absent from both sides' GQ output.

**The anchored form** (aa2ac1a's construction for a forward walk on the sum-to-zero subspace):

Construction:
- **Anchors.** The anchors are `grid_res`. At anchor j, A_j is the parties with national votes and B_j is the rest (both excluding the coordinate party).
- **The window's start.** θ_1 stays a parameter with its N(0, 2²) prior. Its scale is fixed, so a pre-entry coordinate there has no funnel.
- **Absent parties at anchor j: non-centred** from the previous anchor (or θ_1). θ_{a_j,B} = θ_{prev,B} + √V_j · chol(Q_BB) · `anchor_w[j][B]`, where V_j = Σ s_i² Δ_i over the segment and Q = (CΣCᵀ)[2:P, 2:P].
- **Present parties at anchor j: centred**, with the shift trick. θ_{a_j,A} = `w_off`_{j,A} + `anchor_w[j][A]` − (Σ_{b∈B_j} θ_{a_j,b}) / |A_j ∪ {1}|.
  - `w_off` is the result's national log shares centred over the present parties, computed in transformed data from `y_res`.
  - So `anchor_w` = 0 starts at the result, and the result's likelihood does not depend on θ_B.
- **Interior grid points: non-centred Brownian bridges** between consecutive anchors (and between θ_1 and the first anchor). This is aa2ac1a's bridge step, `theta[,t] = theta[,t-1] + (v[t]/R)*(theta[,b]-theta[,t-1]) + sqrt(v[t]*(cv[b]-cv[t])/R)*L_walk*z`, with v_t = s_t² Δ_t.
- **After the last anchor:** forward non-centred, as in the reference form.

The density:
- log N(θ_1);
- the conditional normal of each present block given θ_B and the previous anchor;
- N(0, 1) on every non-centred coordinate.

No extra Jacobian term is needed, because each non-centred piece is a whitened standard normal and the shift has unit Jacobian.

**The equivalence check** is a port of `equivalence_anchored.R`, keeping:
- `methods_fit`;
- `flatten`, in declaration order, column-major (standalone GQ matches columns by position);
- the constant-difference test with tol = 1e-12 · max(1, |lp|);
- one negative control per log-Jacobian term;
- the rejected, outside and one-sided bookkeeping;
- GQ agreement < 1e-9 relative, with seed 1 and `sig_figs = 18`;
- a path error < 1e-8.

Its new parts:
- Points come from a 100 + 40 fit of the `--config` form, seed 5100, depth 10. With `--cell` a 2017 cell, prior points come from `v2_simulate()` and no fit runs.
- The map runs through θ.
- The log-Jacobian terms (of each non-centred map from standard normals to θ):
  - the reference form: Σ_t (log|`L_walk`| + ½(P − 1) log v_t), over its forward steps;
  - the anchored form: the same per bridge step, with v_t replaced by the bridge's conditional variance v_t (cv_b − cv_t)/R, and per forward step after the last anchor; plus Σ_j (log|chol(Q_BB)| + ½|B_j| log V_j) over the anchors' absent blocks;
  - the present blocks contribute none, because they are centred and their density is on θ itself.

- [ ] **Step 1: Write the failing slow tests** in `test-v2-equivalence.R`.
  - `v2_equivalence.R --config=harness/configs/steps/v2s-p1.json --form=walk --cell=2021-09-25,2021-03-29 --n=10 --n-off=5` exits 0 and prints `EQUIV PASS`.
  - With `--stan=` a temporary copy of the Stan file in which the bridge's conditional scale √(v_t (cv_b − cv_t)/R) is replaced by √v_t, it must exit 1.
  - With `--reverse`, `--cell=2024-11-30,2024-11-29 --n=10 --n-off=5` also prints `EQUIV PASS`.
- [ ] **Step 2: Run it and see it fail.**
- [ ] **Step 3: Implement** the anchored walk and the script.
- [ ] **Step 4: Run the equivalence check on three cells.** Run it on 2024 eve and 2021 f180 (30 posterior + 15 off-posterior points), on 2017 eve (20 prior points), and with `--reverse` on 2024 eve. Expected: `EQUIV PASS` on each. Write the results to `harness/v2/steps/v2s-p1-equivalence.csv`.
- [ ] **Step 5: Sampler comparison and adoption.**
  - Add `v2s-p1` to `v2-steps.txt` with seeds 2701 and 2702, and run the queue at 400 + 400.
  - Run `compare_runs.R adopt v2s-m0 v2s-p1 harness/v2/steps/v2s-p1-equivalence.csv`.
  - Log `ADOPT` or `KEEP` and the table in `harness/v2/steps.md`.
  - The `walk_form` of every later step config follows the verdict.
  - Also log the min bulk ESS over the absent parties' `anchor_w` coordinates, read from each run's `key_diagnostics.csv`. This is the pre-entry check listed for Plan 3 in Phase 1's "Later plans".
- [ ] **Step 6: Docs and commit.** Add `walk_form` to `Stan/CLAUDE.md`'s switch table and `v2_equivalence.R` to `R/CLAUDE.md`. `git commit -m "feat(v2): P1 anchored walk, equivalence check and adoption verdict"`

### Task 9: M1, epoch bias (step `v2s-m1`)

Follow the step protocol.
- **Component.** §6.4:
  - μ_e = μ_{e−1} + C(τ ⊙ ξ_e), with τ_p = τ_μ, or τ_μ,A for Annað;
  - after a house's first epoch, d_{e,h} = d_{e−1,h} + C(τ_house ζ_{e,h});
  - **a newcomer p restarts at e_p only** (§6.4): μ_{e_p,p} = the mean over the parties existing before e_p of μ_{e_p,q}, plus m_new + s_new η_p, and then μ_{e_p} is re-centred. For e > e_p, μ_{e,p} follows the walk. Each house's d_{e_p,h,p} restarts at σ_γ ε (its `house_raw` column) and walks after e_p.
- **Parameters:** `mu_step_raw` (matrix[P, n_epoch − 1]), `house_step_raw` (array[H] matrix[P, n_epoch − 1]), `tau_mu`, `tau_mu_A` and `tau_house`. Each is size 0 when `epoch_bias = 0`, and `gamma` is then as in the core.
- **Priors:** τ_μ ~ half-N(0, `prior_tau_mu`), τ_house ~ half-N(0, 0.1), τ_μ,A ~ half-N(0, 0.3).
- **Mutants** (switch `epoch_bias`): `"house_walk_from_first_poll"`, a house's walk starts at the window's start instead of its first epoch; `"newcomer_mean_of_mu1"`, the entry mean is over μ_1 instead of μ_{e_p}.
- **Prior predictive row:** the change in a 20% party's industry bias over one epoch, [5%, 95%] inside [−6, 6] pp.
- **Commit:** `feat(v2): M1 epoch bias`.

### Task 10: M2, newcomer pooling (step `v2s-m2`)

Follow the step protocol.
- **Component.** §6.4's contrast prior with m_new and s_new estimated.
- **Parameters:** `m_new` and `s_new` (size 0 when `newcomer_pool = 0`, which uses 0 and 0.2023).
- **Priors:** m_new ~ N(0, 0.3), s_new ~ half-N(0, 0.3).
- **Mutant** (switch `newcomer_pool`): `"newcomer_mean_over_all"`: the mean runs over every party, the newcomer included.
- **Prior predictive row:** a newcomer's entry bias on a 10% party, [5%, 95%] inside [−10, 10] pp.
- **Commit:** `feat(v2): M2 newcomer bias pooling`.

### Task 11: M3, the election-day shock (step `v2s-m3`)

Follow the step protocol.
- **Component.** §6.6:
  - ε_e = C(s̄_E d_e ⊙ z_e) at each closed election, added inside each result's softmax;
  - in GQ, ε_E is drawn fresh with `d_shock`'s open row and enters `votes_kd` and `pi_nat_eday` but not `pi_latent_E`.
- **Parameters:** `z_shock` (matrix[P, n_closed]) and `s_bar_E`, each size 0 when `shock = 0`.
- **Prior:** s̄_E ~ half-N(0, `prior_s_bar_E`).
- **Anchoring.** With `walk_form = 1`, the anchors stay on θ_{t_e}, and ε_e stays non-centred (§6.7).
- **Mutant** (switch `shock`): `"shock_unscaled"`: d ≡ 1, without the √(0.2/π̄) scaling. (A missing floor is no mutant: the cap at 10 equals the floor's √(0.2/0.02).) The 2024-eve data exercise the floor itself: Sósíalistaflokkurinn has no 2017 result but is present at 2021, so its 2021 shock uses d = √10.
- **Prior predictive rows:** the shock's sd on a 20% and on a 5% party, near §6.10's 1.6 and 0.95 pp at the median, each inside [0.5×, 2×].
- **Commit:** `feat(v2): M3 election-day shock`.

### Task 12: M4, the δ walk (step `v2s-m4`)

Follow the step protocol. This is proposal 5.
- **Component.** §6.7:
  - δ_1 = M_1(s_δ0 ⊙ ζ_1);
  - δ_e = M_e(δ_{e−1} + √(Δyears_e / Δ̄) · s_ζ ⊙ ζ_e), with Δ̄ = `delta_years_bar` = 4;
  - in a newcomer's entry epoch, its column is s_δ0,p ζ_{e,·,p}, not a walk step;
  - the open epoch's step uses Δyears = (E − previous election) / 365.25.
- **Parameters:** `delta_raw` grows to n_epoch matrices; `s_zeta_raw` (vector<lower=0>[P]) gives s_ζ = s̄_ζ · `s_zeta_raw` with `s_zeta_raw` ~ half-N(0, 1), non-centred; and `s_bar_zeta`. Each is size 0 (or, for `delta_raw`, one matrix) when `delta_walk = 0`.
- **Prior:** s̄_ζ ~ half-N(0, `prior_s_bar_zeta`).
- **Mutant** (switch `delta_walk`): `"delta_years_unscaled"`: every step uses √1.
- **Extra builder test:** `delta_years` is 0, 1.0, 3.91 and 3.18 for the 2024 cells' epochs 1–4 (to 2 decimals; the open epoch's is to E).
- **Prior predictive row:** the change in a party's kjördæmi deviation over a 4-year gap, on a 20% party, [5%, 95%] inside [−15, 15] pp.
- **Commit:** `feat(v2): M4 delta walk across elections`.

### Task 13: P2, δ centred at closed elections (step `v2s-p2`)

**Files:**
- Modify: `Stan/forecast_v2.stan` (`delta_form = 1`), `R/harness/v2_equivalence.R` (a `--form=delta` map), `R/harness/arms.R` (`v2s-p2`, role `dev`)
- Create: `harness/configs/steps/v2s-p2.json`
- Test: `tests/harness/test-v2-equivalence.R`

**The centred form**, following the anchored walk's shift trick. It centres fully (λ = 1) where a result informs δ and not at all where none does, so it departs from aa2ac1a's partial centring (`.claude/rules/stan-conventions.md`). That rule would not give λ ≈ 1 here. A result's information about a cell's log share is bounded by its concentration, roughly I ≈ φ_K π(1 − π), about 160 for a 20% party at φ_K's prior centre of 1,000, so λ = s²I/(1 + s²I) is about 0.94 at s_δ0 = 0.3 and about 0.75 for a walk step at s_ζ = 0.135 (s̄_ζ's prior median), lower for smaller steps. Both forms are exact reparameterisations, so the equivalence check holds either way, and the adoption rule (Decision 6) decides P2 by ESS. The open epoch stays non-centred even where kjördæmi polls inform it, because §6.7 says so. A λ-weighted form is left for a later plan.

Closed epochs:
- **Coordinates.** At each closed epoch e, δ_e is held in the subspace coordinates δ_e[2:K, 2:P]. Kjördæmi 1 and the coordinate party are implicit through M_e's two constraints.
- **Absent parties** at election e (B_e) are non-centred.
- **Present parties: centred**, with the shift trick per kjördæmi row and an offset from the result's kjördæmi log-share deviations (M_e applied to the centred log shares), so that zero starts at the result.
- **The prior.** The prior of vec(δ_e[2:K, 2:P]) given δ_{e−1} is Kronecker: (C S_e C)[2:P, 2:P] ⊗ (A_e A_eᵀ)[2:K, 2:K].
  - A_e = I − 1 w_eᵀ.
  - S_e = diag(Δyears_e / Δ̄ · s_ζ²) for a walk step. For e = 1, and for a newcomer's entry column, S_e uses s_δ0,p² instead and the mean is 0 (Task 12's δ_1 = M_1(s_δ0 ⊙ ζ_1)).
  - The present block's density is the conditional given the absent columns.

Elsewhere:
- **The open epoch** stays non-centred (§6.7).
- **Gauge directions.** The reference form's ζ has K + P − 1 directions per epoch that M_e removes. They carry over as an unused term in the equivalence check, as `equivalence_anchored.R` carries the old model's unused walk column.

The steps:

- [ ] **Step 1: Write the failing slow test.** `v2_equivalence.R --config=harness/configs/steps/v2s-p2.json --form=delta --cell=2024-11-30,2024-11-29 --n=10 --n-off=5` must print `EQUIV PASS`. With `--stan=` a temporary copy whose Kronecker factors are swapped, it must exit 1.
- [ ] **Step 2: Run it and see it fail.**
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run the equivalence check** on 2024 eve, 2021 f180 and 2017 eve (prior points). Expected: `EQUIV PASS` on each.
- [ ] **Step 5: Sampler comparison and adoption.** Add `v2s-m4` seed 2702 if Task 12 did not run it. Run `compare_runs.R adopt v2s-m4 v2s-p2 …`, and log the verdict. `delta_form` in `v2s-m5` and `v2.json` follows it.
- [ ] **Step 6: Docs and commit.** Add `delta_form` to `Stan/CLAUDE.md`'s switch table, with the binary-centring note above. `git commit -m "feat(v2): P2 centred delta at closed elections, equivalence check and verdict"`

### Task 14: M5, the fundamentals pseudo-observation; V2 and its ablations (step `v2s-m5`)

Follow the step protocol, then register the arms.
- **Component.** §6.5:
  - μ_f ~ N(centre_{S_f}(log π_nat,E[S_f]), τ_f), where π_nat,E = Σ_k w_open,k softmax(θ_E + δ_open,k), without ε;
  - τ_f,p = √(V_w,p (1 − w)/w);
  - V_w,p = diag(CΣCᵀ)_p · [(weight_time − campaign_days) + campaign_days (1 + τ_camp)²], with w = 0.22, weight_time = 180 and campaign_days = 47.

  `fundamentals = 1` calls `stage1_mu_f()` in the builder (the placeholder on a 2017 cell before the freeze). There is no new parameter.
- **Mutant** (switch `fundamentals`): `"fund_uncentred"`: log π_nat,E[S_f] is not centred.
- **Extra test:** `v2_stan_data()` with `fundamentals = 0` never calls `stage1` (a stub that stops would fire).
- **Prior predictive rows:**
  - a construction check: at every prior draw, the pseudo-observation's precision divided by the sum of its precision and that of the walk's 180-day variance V_w equals 0.22 to 1e-8;
  - reported, not banded: the same weight with V_w,p replaced by V_w,p + V_bias,p, where V_bias,p is the prior variance, over the prior draws, of μ_open,p − μ_{open−1},p on the centred log-share scale. §6.5 says the 0.22 target leaves this out.
- **Arms:**
  - `harness/configs/v2.json` = `v2s-m5`'s config with the adopted forms.
  - Five ablation configs, each turning one switch off:

    | Config | Switch off |
    |---|---|
    | `v2-noepoch.json` | `epoch_bias` |
    | `v2-nonew.json` | `newcomer_pool` |
    | `v2-noshock.json` | `shock` |
    | `v2-staticdelta.json` | `delta_walk` (`delta_form = 0`) |
    | `v2-nofund.json` | `fundamentals` |

  - Register `v2` (candidate) and the five (ablation) in `ARMS`.
  - A test asserts that each ablation config differs from `v2.json` in exactly its named fields.
- **Commit:** `feat(v2): M5 fundamentals pseudo-observation; V2 and ablation arms`.

### Task 15: Simulation-based recovery

**Files:**
- Create: `R/harness/recovery.R`, `harness/v2/recovery.md`
- Modify: `R/CLAUDE.md` (its row)

**Interfaces:**
- Consumes: `v2_simulate()`, `v2_stan_data()` and `harness/configs/v2.json`.
- Produces: `LC_ALL=en_US.UTF-8 Rscript R/harness/recovery.R <E> <F> <first_seed> <n> <warmup> <sampling> [--lane=A|B]`.
  - For each replicate, it builds the real design and replaces every count with a simulated draw: the poll schedule, sample sizes, houses, entry and valid votes stay real; no real count survives. μ_f is also replaced, by a draw from its pseudo-observation density.
  - It fits V2 with seed = the replicate's and writes `results_root()/recovery/<E>_<F>/<seed>/` (truth JSON, posterior quantile of each truth, diagnostics).
  - `recovery.R report` writes `harness/v2/recovery.md`: each group's 50% and 90% coverage with Decision 7's band (binomial counts for the scalar groups, replicate means ± 3 SE for the vector groups), the rejection rate, the replicates that tripped the stop rule, and `RECOVERY PASS` or `RECOVERY FAIL` under Decision 7.
  - A replicate's truth is a prior draw, untruncated; the redraw rule looks only at its simulated data (Decision 7).

- [ ] **Step 1: Write the failing test.** `test-v2-recovery.R` (slow): one replicate at 20 + 20 runs end to end. The sha256 of its count fields (`y_nat`, `y_kd`, `y_res`) and of `mu_f` differs from the real cell's, the design fields (`grid_*`, `house_*`, `N_open`, `w`) are identical, and its rank file has one row per truth element.
- [ ] **Step 2: Run it and see it fail.**
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run the replicates** in the background on two lanes:
  - 2024 eve, seeds 3001–3020;
  - 2021 f180, seeds 3101–3110;
  - both at 400 + 400.

  Then run `recovery.R report`. Expected: `RECOVERY PASS`. A FAIL goes to Brynjólfur with the report.
- [ ] **Step 5: Commit** the script, the test, its `R/CLAUDE.md` row and the report.

### Task 16: V2 Tier 0, the permuted refit, the past-election PPC, and the exit

**Files:**
- Create: `harness/configs/v2-perm.json`, `harness/runs/tier0-v2.txt`, `R/harness/v2_checks.R`, `harness/tier0/v2.md`, `harness/tier0/v2-perm.md`, `harness/tier0/v2_checks.md`
- Modify: `harness/README.md`, `R/CLAUDE.md`, `Stan/CLAUDE.md`

**Interfaces:**
- Consumes: Tasks 5, 7 and 14.
- Produces:
  - **`v2-perm.json`** is `v2.json` with `order` = `"reversed"`. Register it in `ARMS` with role `"check"`. A test asserts that it differs from `v2.json` in that field only.
  - **`tier0-v2.txt`:** `A v2 2024-11-30 2024-11-29 1201`, `B v2 2021-09-25 2021-03-29 1202`, `A v2-perm 2024-11-30 2024-11-29 1201`, `B v2-perm 2021-09-25 2021-03-29 1202`.
  - **`v2_checks.R <run_list>`** reads only `param_draws.parquet`, `ppc_results.parquet` and `meta.json`, plus the closed elections' official counts from `harness/truth/` (results that are data in these cells). It writes `harness/tier0/v2_checks.md` with §6.9's extras:
    - the posterior correlation of (log φ_K, s̄_ζ);
    - the share of draws within 2 of each concentration's cap;
    - two columns for the (σ, Ω) flat directions: the min bulk ESS over the entries of CΣCᵀ, and over the off-diagonals of Ω = `L_Omega` `L_Omega`ᵀ, both computed per draw from the saved `sigma` and `L_Omega`;
    - each fit's minutes projected to 1000 + 2000 (× 3000 / 400), flagged above 120 (§6.9);
    - per closed election, the count of kjördæmi × party cells whose in-sample PIT under `y_rep_res` lies outside [0.025, 0.975] (the PPC of past elections; counts only).

- [ ] **Step 1: Write the failing test.** `v2_checks.R` on a synthetic run directory: the counts and correlation match hand-computed values, and its output holds no column named `share` or `votes`.
- [ ] **Step 2: Run it and see it fail.** Then implement, and run it again to see it pass.
- [ ] **Step 3: Run Tier 0.**
  - Restore the git-ignored state.
  - Run `ListAgents`.
  - Run `zsh R/harness/queue.sh harness/runs/tier0-v2.txt 200 200` in the background.
- [ ] **Step 4: Write the reports.**
  - `tier0_report.R v2 harness/runs/tier0-v2.txt`;
  - `tier0_report.R v2-perm harness/runs/tier0-v2.txt harness/tier0/v2-perm.md`;
  - `v2_checks.R harness/runs/tier0-v2.txt`, which writes `harness/tier0/v2_checks.md`;
  - then `compare_runs.R perm v2 v2-perm harness/runs/tier0-v2.txt`, whose output is appended to it.

  Expected: `PERM PASS` on both cells. A `PERM FAIL` is a bug until shown otherwise (§5.7): report it and stop.
- [ ] **Step 5: Docs.**
  - `harness/README.md`: a V2 section with the arms, roles, staircase, step log, checks and how to reproduce each report.
  - `R/CLAUDE.md`: the row for `v2_checks.R`.
  - `Stan/CLAUDE.md`: the adopted forms beside the switch table.
- [ ] **Step 6: Exit check.**
  - Re-run the adopted P-steps' equivalence checks on the final model: `v2_equivalence.R --config=harness/configs/v2.json --form=both`, on 2024 eve, 2021 f180 and 2017 eve (prior points). If neither P-step was adopted, skip this line and say so in the step log. Expected: `EQUIV PASS` on each. This covers the later components under the adopted forms, which the M-steps' lp tests ran only in the reference forms.
  - Run the full slow suite (`HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R`). Expected: PASS.
  - Then confirm each of these files is committed:
    - `harness/v2/steps.md` with every step's verdict;
    - `harness/v2/prior_predictive.md`;
    - `harness/v2/recovery.md` with `RECOVERY PASS`;
    - `harness/tier0/v2.md`, `harness/tier0/v2-perm.md` and `harness/tier0/v2_checks.md` with `PERM PASS`.
  - The public-text check. Build the pattern in the shell, never in a committed file: `/[U]sers/` plus, for each entry of `$(git rev-parse --git-common-dir)/info/exclude` except `/data` and `.claude/worktrees/`, its last path component as a whole path segment, `(^|[/ `"'(])<name>([/ `"').,:;]|$)`. Then both `git diff master...HEAD | grep -nE "$pattern"` and `git log master..HEAD --format=%B | grep -nE "$pattern"` must print nothing.
- [ ] **Step 7: Commit.** `git commit -m "harness: V2 Tier 0, permuted refit and past-election PPC"`

---

## Later plans

**Plan 4 (Phases 3–6, runbook)**
- **The freeze.** `FREEZE.md` and `FREEZE.sha256`. They list the arms, the R0 and V2 configs (this plan's Decisions included), the priors, dates, scores, margins and rule, the per-cell poll lists, and the newcomer, house and coalition tables.
  - The sha list covers `Stan/forecast_v2.stan`, `Stan/fundamentals_v2.stan`, `R/harness/*` and `R/stan_data.R`.
  - It also records §10's compute-fallback order.
- **Tiers 1 and 2** through the queue, scored by `score_cell()` with the seed-to-seed SE rule, and the gate's one rerun.
- **R0-od, V2-φe, FV5, PA and CF.**
- **The decision and the 2017 test.** `DECISION.md`, then the `CONFIRM.md` writer and Tier C, which is the first 2017 MCMC.
- **Tier 3 and 3b.** Tier 3b varies the `prior_*` scales by ×½ and ×2.
- **The vault decision memo.**

**Parked, still open:**
- `queue.sh`'s lock under a relative `HARNESS_RESULTS` (harness R1);
- PSIS-LOO over `log_lik` (needs the `loo` package);
- the `y_rep_national` all-rows read in `fit_cell.R` (R0 only);
- a λ-weighted partially centred δ, at closed elections and in the open epoch, if P2 is kept at the reference form or V2's δ ESS stays poor.
