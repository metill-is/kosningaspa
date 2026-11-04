# Methodology notes: plan for the thirteen planned chapters

**Date:** 4 Nov 2026. **Status:** plan only; no chapter is written here. **Base:** master `b67a29f` (PR #17). **For:** the author's review.

`docs/method/site.json` lists 14 chapters in six parts. Only 3.1 (`bias/updating.html`) is written. This plan gives, for each of the other 13: its scope and boundaries, its sources, the numbers it would need measured, its dependencies and risks, and its size. It then gives a writing order and a review process.

This file sits in the public repository, so it follows the notes' own citation rules (`docs/method/README.md`, Public). It names repository files and commits, dated fit folders under `data/`, and the author's private Obsidian vault `Metill` by note path, labelled private and never quoted. Local-only studies are described as local-only, without their folder names, paths or numbers.

## 1. Ground rules for every chapter

1. **Citable:**
   - code and commits on master, or on a named branch;
   - merged PR bodies on GitHub;
   - committed records: `R/sampler_check/RESULTS.md`, `Stan/polling_watch_variants.md`, `harness/README.md`, `harness/tier0/*.md`, and the v2 branch's step logs;
   - data the repository reads;
   - dated fits in the git-ignored `data/<date>/`.
2. **Not citable:**
   - local-only studies in git-excluded folders: their names, paths and numbers;
   - absolute or home-relative paths;
   - figures derived from the 1983–2017 ÍSKOS waves;
   - the US model in `usa/`: no mention, comparisons included.

   The vault may be named as a private source, never quoted.
3. **Re-measure or leave out.** A number from a local-only study enters a chapter only if a script kept in `docs/method/analysis/<id>-<slug>/` re-measures it from citable inputs; that folder has a README, as 3.1's does. Otherwise it stays out, and where the gap matters the chapter says "not yet measured in the repository". 3.1 has one number with no kept script (the linear-Gaussian weights, a checking agent's calculation). That is the case to avoid.
4. **No forecast quantities from the harness before the freeze.** `harness/README.md` (The scorer self-test; Tier 0) says no harness forecast is compared with its result before the freeze, and the Tier 0 report holds sampler diagnostics only. This plan goes one step further: before the freeze, no chapter shows a forecast quantity from a harness run under `results/harness/` at all, since a public page would set it beside the known result. Tier 0 tables and step logs are citable as diagnostics.
5. **No fit without the author's go-ahead.** §4 lists the optional fits. Before any heavy R job, check whether other sessions' CmdStan batches are running.
6. **Each chapter lands in one PR.** That PR holds the chapter's body, its analysis folder, `python3 tools/build.py` run from `docs/method/` (with `--check` clean) and the cross-reference updates in other chapters. Push and PR only on the author's yes.

## 2. Fixed points and boundaries

3.1 already owns the following, and other chapters link to it rather than repeat it:

- what "bias" means: γ = μ + d on the log-ratio scale, the zero sum, and the pp method;
- how the anchors identify the bias;
- the forecast model's hedge;
- v5's epochs;
- v2's M1.

3.1's hand-written cross-references are "chapter 1.1 (planned)" (the walk the bias is read against), "2.1 (planned)" (Fylgisvakt as a whole) and "4.2 (planned)" (v2's other components). The change that lands one of those chapters also turns its reference in 3.1 into a link.

| Topic | Owner | What other chapters do |
|---|---|---|
| Latent state, walk, volatility, correlation, anchors as data, bridges | 1.1 | 2.1: "as 1.1, over all ten parties"; 3.1 keeps only its identification angle |
| Walk's end on election day | 1.1 gives the mechanics, β_T = μ_pred + τ_f z ⊙ σ | 1.2 owns μ_pred and τ_f |
| Poll variance: φ, effective n, election rows with no φ | 3.2 | 1.1 and 2.1 state the likelihood in one line |
| Constituency effects δ | 1.3 | 4.2 owns v2's δ walk |
| Seat allocation | 1.4 | 4.1 covers seat *scoring* only |
| Fylgisvakt model / its publication | 2.1 / 2.2 | — |
| Harness machinery / v2's components | 4.1 / 4.2 | — |
| Open issues | each chapter's "What is open" | 5.2 lists them all, with links |
| Dated decisions | 5.1 | chapters cite the commit or PR |
| Symbols | defined where first used | 0.2 collects them |

## 3. The chapters

Section counts include the plain picture, "What is open" and "Sources", as 3.1's twelve do. For scale, 3.1 has 12 sections, 2 SVG figures and 4 tables.

### 0.1 Kosningaspá in one place · `start/overview.html`

- **Scope.** A map of the model families and how they fit together:
  - the forecast model (master);
  - Fylgisvakt: v4 is live, v5 is on master and not published;
  - harness arm R0, the forecast model as of `aa2ac1a` with harness hooks;
  - v2 (branch).

  It also follows the data from the Google Sheets through `R/data.R` and `R/stan_data.R` to the fits and the two outputs. One paragraph and a link per topic, with no mechanism explained.
- **Sources (repository):**
  - the top-level `CLAUDE.md` data pipeline;
  - `R/CLAUDE.md` and the `Stan/CLAUDE.md` inventory;
  - `R/data.R` `update_*()` and `read_*()`, and `R/stan_data.R` `prepare_*()`;
  - lineage: Stoetzer et al. (2019), *Political Analysis* 27(2), doi:10.1017/pan.2018.49, already cited in `writing/iskosbok/kafli.qmd`;
  - `writing/manuscript/` for the 2024 forecast as published.
- **Sources (private, not quoted):** `Kosningar/Forecast model/_MOC.md`; `Kosningar/Forecast model/model-lineage-and-known-issues.md`.
- **Keep out:** nothing beyond the ground rules.
- **New measurements:** counts (polls, houses, results in the window) from `read_polling_watch_data()`. No fit.
- **Dependencies and risks:** write it last, because it summarises the rest. It goes stale fastest, so it carries a dated snapshot and names the live state as master.
- **Size:** 6 sections (picture, the models, the data, the outputs, what is on a branch, sources), 1 figure (pipeline diagram), 1 table (model × where it runs × window × what it publishes).

### 0.2 Notation and glossary · `start/notation.html`

- **Scope.** One reference page with no derivations:
  - each symbol, its Stan name and the chapter that defines it;
  - the Icelandic terms;
  - party letters and the fixed party order;
  - houses, epochs, anchors, harness cells (E, F) and arms.

  It also settles clashes. For example, 3.1's γ, μ and d sit beside the forecast model's `mu_gamma` and `sigma_gamma`.
- **Sources (repository):**
  - the `Stan/CLAUDE.md` Key Parameters table;
  - the top-level `CLAUDE.md` naming conventions;
  - `R/party_utils.R` `party_tibble()`;
  - `R/harness/constants.R` (`POLL_PARTIES`, `LETTER_PARTY`, `KJORDAEMI`, `ELECTIONS`, `TARGETS`);
  - the symbols 3.1 uses.
- **Sources (private):** none needed.
- **New measurements:** none.
- **Dependencies and risks:** seed it from 3.1 and 1.1 once 1.1 is drafted. Each later chapter adds its rows in its own PR. Without that, symbols drift between chapters.
- **Size:** 4 sections (symbols, Stan names, Icelandic terms, parties and houses), 0 figures, 3 tables.

### 1.1 The latent support walk and its election anchors · `forecast/walk.html`

- **Scope.** The forecast model's state and the likelihood in outline:
  - β on the log-ratio scale, with Annað as the drop-one reference;
  - the backward walk from election day and its accumulated variance;
  - party volatilities (hierarchical log σ) and the correlation, `lkj_corr_cholesky(10)`;
  - the stjórnarslit volatility (`tau_stjornarslit`, `month_before_election`, `days_between_stjornarslit_and_election`);
  - the anchored walk with Brownian bridges (`aa2ac1a`);
  - parties before they enter the data;
  - partial polls and the identity gather (`9c1ad37`, `5f09c5b`);
  - election rows as anchors and their vote totals (PR #15);
  - the walk's end, β_T, as the fundamentals prior;
  - a short section on sampling (partial centring, `max_treedepth` 12).
- **Leaves to:** bias → 3.1; φ → 3.2; δ → 1.3; μ_pred and τ_f → 1.2.
- **Sources (repository):**
  - `Stan/polling_and_fundamentals_kjordaemi.stan`, block by block:
    - `transformed data`: anchor construction and the information proxies;
    - `parameters`: `anchor_w`, `z_walk`, `L_Omega`, `log_sigma_t`, `tau_stjornarslit`;
    - `transformed parameters`: the variance increments `v[t]`, the bridge build, `beta0`;
    - `model`: the priors and the poll and election likelihood.
  - `R/stan_data.R`:
    - `prepare_polling_data()`: the date grid, `n_active_d`, `active_index_d` and `party_index`;
    - `poll_dates()`.
  - `R/data.R` `get_election_data()`.
  - `R/sampler_check/RESULTS.md` (24 Oct and 28 Oct sections) and `R/sampler_check/equivalence_anchored.R`.
  - Commits and PRs:
    - `8146e76`: exact multinomial anchors (confirm with `git show` before citing);
    - `9381677`, `9c1ad37`;
    - `ebb3e8d` (PR #5), `5f09c5b` (PR #8);
    - `aa2ac1a` (PR #10, merge `4abc579`);
    - `f51c3c6` and `253d037` (PR #15).
  - `writing/manuscript/adferdir.qmd` (Statistical Framework, Polling Component) for the 2024 form, to say where master differs.
- **Sources (private):** in `Kosningar/Forecast model/`:
  - `2026-10-28 — Anchored, partially centred kjördæmi model (record).md`;
  - `2026-10-24 — Sampling efficiency and pre-entry parties.md`;
  - `reference-invariance-fix.md`;
  - `model-lineage-and-known-issues.md`.
- **Keep out:** the speed-ups and centred-walk comparisons measured in the local-only sampler study. Use `RESULTS.md`'s committed numbers instead (for example the ×4.4 and ×8.0 ESS per gradient of the anchored model).
- **New measurements:**
  - **No fit needed:**
    - anchor sizes and parties per election, from `get_election_data()`;
    - the variance accumulation and bridge variance, analytically;
    - the LKJ(10) prior's marginal correlation spread, analytically.
  - **Optional fit F1 (§4):** the posteriors of σ, Ω (on an identified scale) and `tau_stjornarslit`.
- **Dependencies and risks:**
  - It depends on no other chapter, and 3.1 already points to it.
  - The live `data/polling_data.csv` still holds recycled anchor totals on 17 of its 30 rows until `R/correct_election_anchors.R` runs (3.1, Known issues). Any data-derived number must say which totals it used.
  - The 2024 manuscript describes the pre-anchored, non-centred form.
- **Size:** 11 sections (picture; state and reference; the backward walk; volatility and correlation; stjórnarslit; anchors and bridges; parties before entry; the poll likelihood in outline; sampling; open; sources), 2 figures (variance accumulation with collapse inflation; bridge schematic between anchors), 2 tables (anchors: date, valid votes, parties; parameters and priors).

### 1.2 Fundamentals and their weight · `forecast/fundamentals.html`

- **Scope.**
  - The fundamentals regression inside the forecast model: α_p, β_lag on the last logit vote, β_inc_years, β_vnv (inflation for incumbents) and β_growth.
  - Its training rows and the economy series: `R/prepare_economy_data.R`, growth g(t−1), CPI as a 12-month change.
  - The prediction μ_pred.
  - How its strength is set: a target weight w = 0.22 at 180 days (`desired_weight`, `weight_time`), τ_f = √(V_t(1−w)/w), and V_t from the walk's collapse-inflated variance, with its 47-day literal.
  - The correctness fixes of 24 Oct: the prediction now uses α + β_lag·x as training does, growth lags a year, and `normal(0, 5)` priors.
  - The standalone `Stan/fundamentals.stan`.
  - v2's stage-1 pseudo-observation gets only a branch-box pointer to 4.2.
- **Sources (repository):**
  - `Stan/polling_and_fundamentals_kjordaemi.stan`:
    - `data`: `desired_weight`, `weight_time`, `x_f`, `index_f`, `incumbent_years`;
    - `transformed parameters`: `V_t`, `tau_f`, `mu_pred`, `beta0`;
    - `model`: the fundamentals likelihood and priors.
  - `R/stan_data.R` `prepare_fundamentals_data()`.
  - `R/data.R` `read_fundamentals_data()` and `update_fundamentals_data()`.
  - `R/prepare_economy_data.R`.
  - `R/fit_polling_and_fundamentals_kjordaemi_model.R`, which sets `desired_weight` and `weight_time`.
  - `Stan/fundamentals.stan`, `R/fit_fundamentals_model.R` and `R/plot_fundamentals_weight.R`.
  - Commits:
    - `df1caff`: the appendix on choosing τ_f;
    - `63fb201`;
    - `e1910f6` (PR #4, merge `5d22031`), which carries the formula, lag and prior fixes;
    - `9381677`: the 2024 form, for the before-and-after.
  - `writing/manuscript/adferdir.qmd`: Fundamentals Component, Choosing τ_f.
- **Sources (private):** in `Kosningar/Forecast model/`:
  - `fundamentals-weight-prior-dossier.md`;
  - `fundamentals-weight-prior-literature.md`;
  - `2026-10-23 — Model-improvement research state.md`.

  The novelty question for the weight target is the author's to state. The chapter describes the mechanism and cites only public references it has checked.
- **Keep out:**
  - numbers from local-only analyses of the 2024 forecasts;
  - the per-party size of the formula fix, which is recorded only privately.

  Re-measure them (F2) or leave them out.
- **New measurements:**
  - **Optional fit F1:** the realised posterior weight of the fundamentals on election day against the nominal 0.22 at 180 days (an open item in the private records), and the weight curve over the campaign.
  - **Optional fits F2:** leave-one-election-out for `fundamentals.stan`, to show which formula predicts better.
- **Dependencies and risks:**
  - Depends on 1.1, for V_t and the walk's end.
  - **Verify first.** Check in `prepare_fundamentals_data()` that `index_f` and `y_f` line up; a possible lag between them was raised in review and is not settled on master. If there is a lag, it is a known issue here and in 5.2, and possibly a fix PR first (the author decides).
  - The local `data/fundamentals_data.csv` can lag the sheet.
  - The manuscript still describes the pre-fix formula.
- **Size:** 9 sections (picture; the regression; training data and economy series; the prediction; the weight target and τ_f; how the weight decays; the 2024 form against master; open; sources), 2 figures (weight curve; V_t with collapse inflation), 2 tables (coefficients and priors; economy series definitions).

### 1.3 Constituency effects · `forecast/constituencies.html`

- **Scope.**
  - The static kjördæmi deviations δ per party and kjördæmi, the K-th closed by a weighted sum (`constituency_weights`).
  - σ_δ per party, and partial centring by the data-only information proxy (`lambda_delta`).
  - Kjördæmi polls since the 2021 election and kjördæmi results as data, with the house's national γ plus δ.
  - The kjördæmi forecast: `delta_pred`, `y_pred_constituency`, and `n_pred_k` from `n_pred_k_2024()`.
  - The name-matching fix of PR #15 (the Reykjavík N/S swap).
  - v2's δ walk gets only a pointer to 4.2.
- **Sources (repository):**
  - `Stan/polling_and_fundamentals_kjordaemi.stan`:
    - `data`: `K`, `constituency_weights`, `constituency_weights_pred`, `obs_k`, `n_pred_k`;
    - `transformed data`: `info_delta`;
    - `parameters`: `delta_t`, `sigma_delta`;
    - `transformed parameters`: `lambda_delta`, `delta_free`;
    - `model`: `delta_K`, the kjördæmi likelihood;
    - `generated quantities`: `delta_pred`, `y_pred_constituency`, `y_rep_k`.
  - `R/stan_data.R` `n_pred_k_2024()`, `kjordaemi_obs_k()`, `poll_dates()`.
  - `R/data.R` `read_constituency_data()`, `update_constituency_data()`.
  - `tests/harness/test-kjordaemi-alignment.R`, `test-fit-script-n-pred.R`.
  - Commits: `3c45310`, `63fb201`, `d14df9b`, `ebb3e8d`, `aa2ac1a`, PR #15 (merge `9cd12cb`).
- **Sources (private):** `Kosningar/Forecast model/2026-10-28 — Anchored, partially centred kjördæmi model (record).md` (the author's question on more kjördæmi data); the design note of 23 Oct, §6.7.
- **Keep out:** the kjördæmi calibration figures (interval coverage, and error against carrying the last election's deviations forward). They come from local-only backtests. They are the chapter's most important evidence, and the open way to get them is the harness's kjördæmi scores (`R/harness/scores.R` `kd_scores()`) after the freeze.
- **New measurements:**
  - **No fit needed:** kjördæmi poll counts by house and date.
  - **Optional fit F1:** σ_δ and δ by party and kjördæmi.
  - **After the freeze:** calibration.
- **Dependencies and risks:**
  - Depends on 1.1, and on 3.1 for the house term.
  - Without harness scores, the "how good is it" section is an open box (Decision D4).
- **Size:** 8 sections (picture; δ and its closing constraint; σ_δ and partial centring; the kjördæmi data; the kjördæmi forecast; the PR #15 fix; open; sources), 2 figures (weighted closing schematic; δ heat map from F1, if run), 2 tables.

### 1.4 From vote shares to seats · `forecast/seats.html`

- **Scope.** The allocation rules:
  - six kjördæmi; 54 constituency seats by D'Hondt;
  - 9 adjustment seats over lists with at least 5% nationally, by national D'Hondt, each placed where the list's next quotient is the largest share of that kjördæmi's votes;
  - the 2013–2021 and 2024 layouts;
  - lists in `ineligible` (Annað) win no seat but count in the threshold and the shares.

  Then the steps from draws to seat distributions and majorities, how the allocator is tested, and the pipeline gap: no live script writes `seats_draws.parquet`, and the 2024 forecast used `R/archive/predict_seats.R` with its own copy of the allocator. The Kosningakerfi simulator lives in a private repository; mention it in one line at most.
- **Sources (repository):**
  - `R/election_utils.R`: `dhondt()`, `jofnunarsaeti()`, `seats_tibble()`, `seats_layout_2021()`, `layout_for()`, `calculate_seats()`.
  - `R/archive/predict_seats.R`, `R/process_seats_draws.R`, `R/make_meirihlutar_plots.R`, `R/make_new_prediction_plots_seats.R`.
  - `R/harness/scores.R` `seat_draws()`, `seat_scores()`, `majority_prob()`.
  - `harness/truth/elections_official.csv` and `harness/sources/seats_2021_official.csv`.
  - `tests/harness/test-truth.R`, which reproduces every official seat 2016–2024 and the 2021 adjustment-seat placement, and `tests/harness/test-seats.R`.
  - Commits and PRs: `e1910f6` (the allocator made the 2024 production one), PR #16 (`aaf0aa1`, `bcd1ea6`).
  - The election acts: Lög um kosningar til Alþingis nr. 24/2000 (the 2016, 2017 and 2021 elections) and Kosningalög nr. 112/2021 (in force from 1 Jan 2022; the 2024 election). Check the article numbers before citing them.
- **Sources (private):** `Kosningar/Kosningakerfi/Kosningakerfi simulator.md`.
- **Keep out:** any claim about how the allocator compares with the 2024 forecast's saved seat draws. It could be re-measured from `data/2024-11-30/` with a kept script, but Decision D7 keeps this chapter to official results, so it stays out. The old allocator's errors can be shown from git (the pre-`e1910f6` `R/election_utils.R`) with a kept script.
- **New measurements, none needing a fit:**
  - a worked example, 2021 official votes to seats step by step, from `harness/truth/`;
  - the re-run against the 2024 draws (D7);
  - optionally, seat sensitivity near the 5% threshold.
- **Dependencies and risks:** independent of every other chapter except 1.3, for the input draws, and independent of v2. The pipeline gap may close (a live seat script); update the chapter if it does.
- **Size:** 8 sections (picture; the rules; the layouts; the allocator in code; from draws to seats and majorities; how it is tested; open; sources), 2 figures (worked adjustment-seat example; a seat distribution), 2 tables (layouts; the 2021 worked example).

### 2.1 The polling smoother, v4 to v5 · `fylgisvakt/smoother.html`

- **Scope.** Fylgisvakt as the forecast's poll model without fundamentals or kjördæmi:
  - v4's sum-to-zero state over all ten parties, and why: reference non-invariance, `6d93978`;
  - the walk (σ, LKJ(10)) and the problem that the saved Ω is not the identified co-movement, against the identified clr and share scales;
  - the likelihood in one line (detail → 3.2);
  - partial polls;
  - party ordering as the sampling lever (variants §5);
  - the anchored walk and the window moving from the 2021 result to 2016 (`fdf7590`);
  - outputs (`pi_smooth`);
  - the v4 → v5 switch in one section, with its bias mechanics left to 3.1;
  - the flow-informed correlation prior, as a method only (`R/flow_prior.R`, `6051b55`).
- **Leaves to:** 2.2 the publication; 3.1 everything about γ and μ.
- **Sources (repository):**
  - `Stan/polling_watch_v4.stan`, `Stan/polling_watch_v5_epoch.stan`, and the comparison variants `Stan/polling_watch*.stan`.
  - `Stan/polling_watch_variants.md` §1–§10.
  - `R/stan_data.R` `prepare_polling_watch_data()` and `prepare_polling_watch_epochs()`; `R/data.R` `read_polling_watch_data()`.
  - `R/fit_polling_watch.R` and `R/fit_polling_watch_epoch.R`.
  - In `R/sampler_check/`: `fit_polling_watch_headless.R`, `equivalence_polling_watch.R`, `compare_polling_watch.R`, `score_polling_watch_backtest.R`, and `RESULTS.md` (28 Oct Fylgisvakt sections).
  - `R/make_rw_innovation_cov_plots.R` and `R/make_correlation_plots.R`.
  - Commits and PRs: `7e0c032`, `51a1825`, `6d93978`, `7189aa0`, `e73e79b`, `9c1ad37`, `9137118`, `88b8663`, `fdf7590`, `fc29771`, PR #11 (`723b533`).
- **Sources (private):**
  - `Kosningar/Forecast model/reference-invariance-fix.md`;
  - `Kosningar/Forecast model/2026-10-28 — Fylgisvakt on the forecast's window with epoch-varying bias (record).md`;
  - `Kosningar/Forecast model/model-lineage-and-known-issues.md` (the Ω, LKJ and flow-prior sections);
  - `Knowledge/Data Sources/ÍSKOS.md`, for the licence boundary.
- **Keep out:**
  - every flow-prior target or agreement figure built from the 1983–2017 waves;
  - LKJ-sensitivity numbers that exist only in private records.

  A figure from the CC0 2021 wave may enter only with a kept script and the author's OK.
- **New measurements, no fit needed:** from the 29 Oct v5 fit (`data/2026-10-29/polling_watch_fit.rds`, re-queryable): σ by party, Ω on the identified clr scale, and the diagnostics in `diagnostics.json`.
  - **Optional fit F3:** LKJ sensitivity (two v5 fits). Low priority.
- **Dependencies and risks:**
  - Write it after 1.1, so it can say "as 1.1, over all ten parties" instead of re-deriving the walk.
  - The site still serves v4 (fit of 29 Sep). When v5 is published, 2.1, 2.2 and 3.1's table need a dated update.
  - `Stan/polling_watch_variants.md` still names v4 as production in its TL;DR, as 3.1 notes; fix it in a separate doc change.
  - A refit after the anchor correction moves the 2016–2021 anchors by up to 0.36 pp. Say which fit every number comes from.
- **Size:** 10 sections (picture; the model in one place; sum-to-zero and reference invariance; walk and correlation; partial polls and ordering; anchored walk and window; v4 to v5; sampling; open; sources), 3 figures (state schematic; v4 against v5 curves for D and S, if a matching v4 fit exists in `data/`, checked at writing time; identified correlation heat map), 2 tables (variant lineage v1–v5; sampling comparison from `RESULTS.md`).

### 2.2 From a fit to metill.is · `fylgisvakt/publish.html`

- **Scope.** The operational chain, manual end to end with no cron:
  1. **Ingest:** the scrape-polls skill, `data/post_election_polls.csv` and `data_sources.md` provenance, including the Gallup hazard of a missing smallest party (never enter 0.0).
  2. **Fit:** `R/fit_polling_watch.R`, its `data/<date>/` outputs and `diagnostics.json` as the publish gate.
  3. **Export:** by the metill-platform exporter into `data/fylgisvakt/*.json`.
  4. **Deploy.**

  Then what the page shows: the softmax(β) curve, raw poll dots, P(S ahead) and coalition bands from joint draws. Last, the failure points.
- **Sources (repository):**
  - `R/fit_polling_watch.R` and `R/scrape_polls.R`;
  - `.claude/skills/scrape-polls/SKILL.md` (tracked) and `data_sources.md`;
  - the top-level `CLAUDE.md`, "Publishing to metill.is";
  - `R/correct_election_anchors.R`;
  - `R/make_polling_watch_plots.R` and `R/make_house_effects_plot.R`;
  - the published page and its JSON on metill.is, which any reader can fetch.
- **Sources (private):**
  - the metill-platform repository (private on GitHub, checked 4 Nov 2026 with `gh repo view`): exporter, guards, coalition export;
  - `Kosningar/Fylgisvakt/fylgisvakt-publish-chain.md` and `Kosningar/Fylgisvakt/Fylgisvakt coalition explorer.md`;
  - `Knowledge/Data Sources/Polls.md`.
- **Keep out:** the exporter's code and internal guards beyond what the published JSON shows, unless the author decides otherwise (Decision D3).
- **New measurements:** the field lists of the published `meta.json` and `current.json`, from the live site, and the dated fit folders under `data/`. No fit.
- **Dependencies and risks:**
  - Half of the chain is in a private repository, so the chapter can describe it only by its contract.
  - Write it after the first v5 publish, so it describes the live chain once rather than two configurations.
- **Size:** 8 sections (picture; ingest; the fit and its outputs; the publish gate; export and deploy, contract only; reading the page; failure points; sources), 1 figure (chain diagram), 2 tables (outputs per file; guards).

### 3.2 Overdispersion and house precision · `bias/overdispersion.html`

- **Scope.** The variance side of the polls; 3.1 has the mean side.
  - Why polls are Dirichlet-multinomial: the per-house concentration φ_h, with log φ ~ N(μ_φ, σ_φ), the variance inflation (n + φ)/(1 + φ) and the effective sample size.
  - Results as house 1: a plain multinomial with no φ, hence exact anchors and what that does to forecast width.
  - The log φ cap at 20 and why: the CmdStan 2.38 gradient error that the Stan file's own `transformed data` comment records.
  - Poll sizes and their corrections (`harness/sources/poll_n_corrections.csv`).
  - φ for kjördæmi polls.
  - v2's answers in a branch box pointing to 4.2: bounded concentrations, proposal 1 (μ_φ ~ N(log 1000, 1)), φ_K for kjördæmi results, M3's election-day shock.
- **Sources (repository):**
  - `Stan/polling_and_fundamentals_kjordaemi.stan` (`phi_cap`, `phi_init`, `log_phi_u`, the `mu_phi`/`sigma_phi` priors, the Dirichlet-multinomial lines), and v4's and v5's φ.
  - `R/stan_data.R`, for poll n and house index; `R/harness/info_set.R` `apply_n_corrections()`.
  - `R/sampler_check/RESULTS.md` (28 Oct).
  - Commits: `8146e76`, `aa2ac1a`, `fdf7590`.
  - Branch: `Stan/forecast_v2.stan`; the Phase 2 plan's proposals table.
- **Sources (private):** the 28 Oct anchored-model record (the gradient finding); the research-state note (defect 6); the design note of 23 Oct, §6.6 and §6.10.
- **Keep out:** the whole local-only election-overdispersion study and its calibration and score figures. Where it is measured in the open is the harness, scoring R0 against v2 with M3 after the freeze.
- **New measurements:**
  - **No fit needed:** φ_h by house from the 29 Oct v5 fit, and the implied effective n against nominal n.
  - **Optional fit F1:** the forecast model's φ.
- **Dependencies and risks:**
  - Depends on 1.1 and 3.1, and reuses 2.1's fit.
  - Its strongest evidence (calibration) is not citable yet. Write the mechanism now with an open box, and add the calibration box when the harness scores (Decision D4).
- **Size:** 7 sections (picture; the Dirichlet-multinomial and effective n; results with no φ; the cap; poll sizes and corrections; v2 on the branch; open and sources), 2 figures (φ by house with effective n; the variance inflation curve), 1 table.

### 4.1 The two-election backtest harness · `v2/harness.html`

- **Scope.**
  - What the harness answers: leave-future-out forecasts of 2021 and 2024 from what was known at each forecast date F; 10 cells.
  - The information set and leakage rules: availability by fieldwork and publication date, `info_set()`, the leak check.
  - The sha256-checked snapshot, and the official truth.
  - The scores: national CRPS10, DSS, alr d², IS90, MAE8 and energy score; kjördæmi CRPS and coverage; seats; the coalition Brier score.
  - The freeze and the 2017 locks (2017 is the held-out confirmatory target).
  - Arms and runs (R0 = production as of `aa2ac1a` with hooks), Tier 0 diagnostics, and the queue.
  - Phase 2's step-check machinery (stop and adoption rules, `compare_runs.R`) in a branch box.
  - No scores: nothing is scored, and no freeze exists (`harness/FREEZE.md` and `harness/DECISION.md` absent on master, 4 Nov 2026).
- **A correction to the premise.** Most of this chapter is on master, not the branch: Phase 0 (PR #4, merge `5d22031`, 24 Oct), Phase 1 (PR #7, `884560f`, 25 Oct) and R0's re-base on `aa2ac1a` (PR #10). Only Phase 2's additions are unmerged: v2 arms, `R/harness/compare_runs.R`, `R/harness/stage1.R`, and `harness/README.md` § Step checks.
- **Sources (repository, master):**
  - `harness/README.md`, all sections;
  - in `R/harness/`: `constants`, `inputs`, `truth`, `availability`, `tables`, `fundamentals`, `info_set`, `scores`, `selftest`, `freeze`, `scorer`, `runs`, `r0`, `arms`, `fit_cell.R`, `queue.sh`, `tier0_report.R`, `build_phase0.R`;
  - the committed tables under `harness/` (`availability.csv`, `entry_table.csv`, `sources/`, `truth/`, `tier0/r0.md`, `configs/r0.json`, `runs/tier0.txt`);
  - `tests/harness/`;
  - the Phase 0 and Phase 1 plans in `docs/superpowers/plans/`;
  - PRs #4, #7, #10, and the Decision 5 pair `4ed77c0` and `1ff40cc`.
- **Sources (branch, unmerged):** `R/harness/compare_runs.R`, `R/harness/stage1.R`, the branch's `R/harness/arms.R`, `harness/README.md` § Step checks, `harness/runs/v2-steps.txt`, `harness/configs/steps/`.
- **Sources (private):** the design note of 23 Oct, §5; `Kosningar/Forecast model/2026-10-24 — v2 harness Phase 0 (record).md`.
- **Keep out:**
  - the name of the earlier local study whose scores the self-test reproduces; `R/CLAUDE.md` already describes it without naming it;
  - any forecast quantity from a harness run before the freeze.
- **New measurements, none needing a fit:**
  - polls admitted per cell and the availability counts, from `build_phase0.R`'s output;
  - Tier 0 diagnostics, already committed.
- **Dependencies and risks:** §5. Scores wait for the freeze and the scoring run, both author decisions.
- **Size:** 11 sections (picture; question and cells; information set and leakage; snapshot and truth; arms and runs; scores; freeze and the 2017 locks; Tier 0 and the queue; step checks (branch); open; sources), 2 figures (cells on a timeline; data flow snapshot → info set → arm → scorer), 3 tables (cells; scores; arms).

### 4.2 v2's components · `v2/components.html`

- **Scope.** The V2 model as designed and built, step by step:
  - **the core:** a P-dimensional sum-to-zero walk with innovation covariance CΣCᵀ, national and kjördæmi polls, houses, a static δ, bounded concentrations;
  - **P1:** the anchored walk;
  - **M1:** epoch bias, linking to 3.1;
  - **M2:** the newcomer pool;
  - **M3:** the election-day shock;
  - **M4:** the δ walk;
  - **P2:** δ centred at closed elections;
  - **M5:** the fundamentals pseudo-observation from stage 1 (`Stan/fundamentals_v2.stan`);
  - **V2 and its ablations.**

  For each component: what it changes against production, its status, and its sampler check. Then the step protocol (R reference density, mutants, permutation test, stop and adoption rules), the prior predictive check, recovery (Task 15), the five proposals and the rulings (Decisions 1–9). No scores.
- **Sources (branch only, read with `git show`, never checked out):**
  - `Stan/forecast_v2.stan` and `Stan/fundamentals_v2.stan`;
  - in `R/harness/`: `v2.R`, `v2_reference.R`, `v2_equivalence.R`, `v2_methods.R`, `v2_prior_predictive.R`, `stage1.R`;
  - `harness/v2/steps.md`, `harness/v2/steps/*.md` and `harness/v2/prior_predictive.md`;
  - `harness/configs/steps/*.json`;
  - `tests/harness/test-v2-*.R`;
  - `docs/superpowers/plans/2026-10-28-v2-harness-phase2-v2-model.md` (Tasks 1–16, the proposals, the Decisions).
- **Sources (private):** the design note of 23 Oct, §6.
- **Branch state (4 Nov 2026).** Tip `f7468df`: 34 commits on master's `4abc579`, the last on 29 Oct, not on origin (`git ls-remote --heads origin claude/epic-lichterman-e32cb6` is empty).
  - **Built:** the core (`v2s-m0`), P1, M1, M2, M3 and M4.
  - **Step-log sections:** up to `v2s-m3`. M4 (`dbbd70b`, `f7468df`) has none yet.
  - **Not built:** P2 (Task 13), M5 with V2 and its ablations (Task 14), recovery (Task 15), and V2 Tier 0, the permuted refit, the past-election PPC and the exit (Task 16).
- **New measurements:** none before scoring. The sampler diagnostics and prior predictive bands are already in the branch's committed logs.
- **Dependencies and risks:** §5. 3.1's v2 section, read at `18f4c68`, is already stale against the branch: it calls M3 unbuilt and the M4 δ walk not built, but both landed on 29 Oct (`436aa8a`; `dbbd70b`). Refresh it with 4.2, or sooner if the author wants.
- **Size:** 13 sections (picture; the staircase and its protocol; core; P1; M1; M2; M3; M4; P2; M5 and stage 1; V2, ablations and recovery; rulings, open; sources), 2 figures (the staircase; component dependencies), 2 tables (component × status × check; the Decisions).

### 5.1 Timeline and decisions · `history/timeline.html`

- **Scope.** A dated list of model changes and decisions from the 2024 forecast to now. Each entry gives its commit or PR and the chapter that explains it. Model-relevant changes only, not every commit. Decisions say that the author made them and cite the PR body, or name the private record.
- **Sources (repository):**
  - `git log` on master for `Stan/`, `R/stan_data.R`, `R/data.R`, `R/election_utils.R` and `harness/`;
  - merged PR bodies #4–#17 (`gh pr view`);
  - `Stan/polling_watch_variants.md` and `R/sampler_check/RESULTS.md`.
- **Sources (private):** `Kosningar/Kosningar Handoff.md` and the dated records in `Kosningar/Forecast model/`.
- **Keep out:** any decision whose only record is a local-only study or a private review.
- **New measurements:** none.
- **Dependencies and risks:** better after the model chapters, so entries can link to them. Low risk, but it needs a row with every model PR from then on.
- **Size:** 3 sections (picture; timeline; open decisions), 1 figure (timeline), 1 table of about 30–40 rows.

### 5.2 Known issues · `history/known-issues.html`

- **Scope.** The register of open model issues. Each entry gives the model, where it is in the code, its status, citable evidence, and the chapter that discusses it. A second table lists resolved issues with the commit that fixed each one. Every entry is re-checked against master when the chapter is written.
- **Candidates, from code and committed records:**
  - **From 3.1:** Annað has no house effect and J closes; the hedge's unrecorded intent, broken zero sum and 2024-only divisors; recycled anchor totals in old data files (PR #15).
  - **Forecast model, other:** exact anchors and forecast width (3.2); static δ (1.3); the 47-day literal in V_t (1.2); the `index_f`/`y_f` lag, if verified (1.2); no live seat script (1.4).
  - **Fylgisvakt:** the saved v4 Ω is not identified, the LKJ(10) choice is open, v5 is unpublished, and `polling_watch_variants.md`'s TL;DR is stale (2.1).
  - **Resolved:** reference non-invariance (`6d93978`); partial-poll slicing (`9c1ad37`); the national model's newest-poll replicate (PR #16, `867823b`).
- **Sources (private):** `Kosningar/Forecast model/model-lineage-and-known-issues.md` and the research-state note's defect list.
- **Keep out:** evidence numbers from local-only studies. The structural statement from the code stays; the measured size waits for the harness or a kept script.
- **New measurements:** none.
- **Dependencies and risks:** after Part I and 3.2, so entries can link to them. Each later chapter PR updates its rows.
- **Size:** 3 sections (picture; register; resolved), 0 figures, 2 tables.

## 4. Measurements and fits

No chapter needs a fit to be written: each is written from the code, committed records and existing fits. The fits below only add measured boxes, and each needs the author's go-ahead.

| Id | What | Feeds | Needs |
|---|---|---|---|
| X1 | The 29 Oct v5 fit, `data/2026-10-29/` (exists) | 2.1, 3.2 | nothing |
| X2 | The 2024 forecast's saved outputs, `data/2024-11-30/` (exist) | 1.4 | the author's OK to use them (D7) |
| X3 | Committed records: `RESULTS.md`, `variants.md`, `harness/tier0/r0.md`, step logs | 1.1, 2.1, 4.1, 4.2 | nothing |
| F1 | One production forecast-model fit on current data, headless via `R/sampler_check/fit_headless.R`, into a dated `data/<date>/` | 1.1, 1.2, 1.3, 3.2 | the author's go-ahead; which election date it targets (the interactive script's settings are the 2024 forecast's); run after `R/correct_election_anchors.R` has corrected the live CSV, or say it used the old totals |
| F2 | `Stan/fundamentals.stan` leave-one-election-out | 1.2 | go-ahead (small fits) |
| F3 | Two v5 fits for LKJ sensitivity | 2.1 | go-ahead; low priority |
| H | Harness scores: R0, later v2 | 1.3, 3.2, 4.1, 4.2 | the freeze and the scoring run; the author's decisions, not chapter work |

## 5. The v2 branch and chapters 4.1 and 4.2

`claude/epic-lichterman-e32cb6` is local and unpushed, so a reader cannot check it. The README allows a branch box that names the branch and says whether it is pushed. Keep such boxes few.

- **4.1: write now.** Write it from master (Phases 0–1 and R0), with one branch box for Phase 2's step-check machinery. The box names the branch, says it is not pushed, and gives the commit it was read at. When the v2 PR merges, replace the box with master paths and the merge commit. The scores section waits for the freeze.
- **4.2: write after the v2 PR merges.** Several components are unbuilt, the tip is moving, and branch SHAs die if the merge squashes. If the author wants it sooner, draft it on a branch with every section in a branch box carrying the commit read, and do not merge it to master before v2. Read the branch only with `git show <branch>:<path>` (in zsh, brace the variable, `"${B}:path"`, or the `:h` modifier eats it). Never check it out. If a question needs the Plan 3 session, send it one batched message.
- **3.1's v2 section** gets the same treatment as 4.2: refresh it at the merge.

## 6. Writing order

1. **1.1, the walk.** It is the fixed point 3.1 already points to, the algebra 1.2, 1.3, 2.1 and 3.2 build on, master-only and fit-free. **Recommended first chapter.**
2. **1.4, seats, in parallel with 1.1.** Its sources are disjoint from 1.1's, it needs no fit, and the tests give high certainty. Run it in its own session and worktree.
3. **0.2, notation.** Seed it from 3.1 and 1.1 as soon as 1.1 is drafted; every later chapter adds its rows.
4. **1.2, fundamentals,** after the `index_f`/`y_f` check.
5. **1.3, constituencies.**
6. **2.1, the smoother.** It reuses 1.1 and is measured from X1.
7. **3.2, overdispersion.** It reuses 1.1 and 2.1, with the calibration box open (D4).
8. **5.2, known issues,** once Parts I–III have open boxes to collect.
9. **4.1, the harness** (the master part). It can move earlier if the author wants the harness documented before scoring.
10. **2.2, publication.** It is gated on the first v5 publish, and moves up when that happens.
11. **4.2, v2's components.** Gated on the v2 merge; refresh 3.1's v2 section in the same PR.
12. **5.1, timeline.**
13. **0.1, overview,** last.

## 7. Review process per chapter

3.1 had a draft, two rounds of independent read-only fact checks against code and data, and a separate public-leak check before committing. Keep that for every model chapter (1.1–1.4, 2.1, 3.2, 4.1, 4.2). For 0.1, 0.2, 2.2, 5.1 and 5.2, which mostly point at other chapters, one fact-check round is enough, but they keep the leak check: 2.2 sits on the private-repository boundary.

**Per chapter:**

1. **Draft** in a Claude session in its own worktree off master. Every measured number gets a kept script in `docs/method/analysis/<id>-<slug>/` with a README. Then run `python3 tools/build.py` and `--check`.
2. **Round 1:** three read-only subagents in parallel, each given the chapter file and the repository, not the drafter's conclusions:
   - **(a) Code and commits:** every Stan block, function, SHA and branch claim, checked on master with `grep` and `git show`.
   - **(b) Numbers:** re-run each analysis script, and recompute each number independently from the fit or data.
   - **(c) Reading:** overlap with §2's fixed points, consistency with 0.2 and 3.1, every claim saying where it comes from, British spelling.

   Before accepting any number an agent recomputed, re-run it yourself.
3. **Round 2:** one fresh verifier on the corrected draft, given the list of round-1 corrections. Run a third round only if round 2 finds a substantive error.
4. **Leak check:** a separate agent given only the README's Public paragraph, §1 of this plan and the chapter, plus a mechanical grep (D6).
5. **Commit.** The review stamp says "Not reviewed by the author". Ask before push and PR.
6. **Author review.** The author reads the rendered page; their reading is the point of the notes. On their review, the `review` meta changes.

**The leak grep (D6).** Recommended: a small `docs/method/tools/leak_check.py`, written with chapter 1.1. It reads the local-only folder names at run time from the repository's git exclude file (`git rev-parse --git-common-dir`, then `info/exclude`), so the list itself is never committed. It matches each name as a path component anchored at the repository root, not as a bare word, since some names collide with ordinary words. It adds fixed patterns for absolute and home paths and for `usa/`. Checked by hand on this plan on 4 Nov 2026: no folder name matches as a path. ÍSKOS-year figures cannot be grepped reliably, so the leak agent covers them.

**Who does what:**

- **The author:** rulings (§8); go-aheads for fits, pushes and PRs; the final read and the review stamp; merges.
- **One Claude session per chapter** (two for the 1.1 ∥ 1.4 pair): sources, scripts, draft, build, dispatching the checks, fixes, commit.
- **Subagents:** the read-only checks only, three in round 1 and one each for round 2 and the leak check. Stagger them, to stay clear of rate limits.

**Parallel chapters.** Chapters have their own files, but each landing changes shared generated files. `build.py` rewrites every written chapter's pager (it links only to written chapters) and `index.html`, and a landing also turns "(planned)" references into links in other chapters. So merge one chapter at a time, and before each merge rebase on master and re-run `build.py`.

## 8. Decisions for the author

- **D1.** Approve the order, and 1.1 as the first chapter, with 1.4 in parallel.
- **D2.** F1: whether to run it, which election date it targets, and whether to run it after `R/correct_election_anchors.R` has corrected the live CSV.
- **D3.** 2.2: how much of the private exporter to describe. Recommended: the contract only, as the published JSON shows it.
- **D4.** 1.3 and 3.2: write them now with an open calibration box (recommended), or wait for harness scores.
- **D5.** 4.2: wait for the v2 merge (recommended), or draft it on a branch now.
- **D6.** Add `leak_check.py` with chapter 1.1 (recommended), or keep the grep manual.
- **D7.** Whether 1.4 may use the 2024 forecast's saved draws in `data/2024-11-30/`. Without them, 1.4 uses official results only.
