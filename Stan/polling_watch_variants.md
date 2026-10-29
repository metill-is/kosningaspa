# Polling-watch model: reference invariance & reparameterisation

Decision record for the between-election polling smoother (`polling_watch*.stan`,
fit by `R/fit_polling_watch.R`, deployed to metill.is/fylgisvakt). Written 2026-06-24.

**TL;DR.** The original `polling_watch.stan` used a drop-one softmax parameterisation
whose reported support depended on an *arbitrary* choice of reference party — a bug
that, combined with a locale issue, briefly published an artefactual "D overtakes S"
headline. The fix is a **sum-to-zero reparameterisation** that is invariant to party
ordering. After comparing several variants, the production model is
**`polling_watch_v4.stan` (per-party RW volatility + full cross-party correlation)**,
fit with parties ordered **largest → smallest** for sampling efficiency.

---

## 1. The bug: reference non-invariance

The original model parameterised the latent log-support with a *dropped* reference
category: `eta[2:P] = beta`, `eta[1] = -sum(eta[2:P])`. The softmax *likelihood* is
invariant to which party is dropped, but the *priors* are not:

- `beta0 ~ normal(0, 2)` gives each non-reference party prior SD 2, but the reference
  party (`eta[1] = -sum(beta0)`) gets SD ≈ 2·√(P−1) ≈ 6 — a 3× wider prior.
- The RW innovation covariance (`sigma`, `L_Omega`) lives on the P−1 free parties, so
  the reference party's volatility/correlation is the negative sum of the others.
- The `gamma` house-effect zero-sum and `mu_gamma` prior exclude the reference party.

Because the data don't fully overwhelm these, the **reference party's level is pulled
by ~2pp**. A matched-seed test (same seed, only the reference changed):

| reference party | smoothed S | smoothed D | P(D>S) |
|---|---|---|---|
| Samfylkingin (the headless-locale default) | 24.0% | 26.0% | 0.85 |
| Annað (the code's *intended* reference)    | 25.8% | 25.7% | 0.48 |

A locale bug compounded it: under a C locale (headless `Rscript`) the `fct_relevel()`
in the fit script silently no-opped, so the deployed fit used **Samfylkingin** as the
reference instead of the intended **Annað** — publishing the D-lead. The immediate fix
was pinning `LC_ALL=en_US.UTF-8`; this document covers the durable modelling fix.

---

## 2. The fix: symmetric sum-to-zero

Parameterise the latent state, house effects and industry mean on the **sum-to-zero
subspace** (no party singled out), so `pi_smooth = softmax(beta)` cannot depend on
ordering. `sum_to_zero_vector[P]` (Stan ≥ 2.34; we use cmdstan 2.38) gives each party
equal marginal prior variance. Per-party volatility and cross-party correlation are
*retained* — correlated, per-party-scaled innovations are projected onto the sum-to-zero
subspace by centring (`innov -= mean(innov)`).

Invariance is verified empirically: fitting under two very different party orderings,
endpoint `pi_smooth` agrees to **< 0.11pp** (vs ~2pp for the drop-one model).

---

## 3. Model variants (all reference-invariant; all reproduce the S≈D tie)

| file | RW volatility | cross-party correlation | notes |
|---|---|---|---|
| `polling_watch.stan` | per-party (hierarchy) | full | **original drop-one** — NOT invariant; locale-fixed but superseded |
| `polling_watch_v2.stan` | per-party (log-normal hierarchy) | full `lkj` | first invariant version; faithful to original |
| `polling_watch_v3.stan` | single shared σ | full `lkj` | parsimonious |
| `polling_watch_v3b.stan` | single shared σ | none (isotropic) | minimal; native `sum_to_zero_vector` innovations, no centring |
| **`polling_watch_v4.stan`** | **per-party (independent half-t, no hierarchy)** | **full `lkj`** | **PRODUCTION** |

`v4` keeps both modelling features of the original (differential per-party volatility
*and* full correlation) but drops the per-party σ *hierarchy* (`mu_log_sigma`,
`tau_log_sigma`) — a weakly-identified hyper-scale that was the dominant sampling funnel.

---

## 4. Sampling comparison

Headline metrics on the real P=10 data (4 chains, 500 warmup / 1000 sampling). The
honest cost-normalised metric is **ESS per 1000 leapfrog steps**; treedepth alone is
seed-volatile and misleading.

| variant (ORDER A = `Other` first) | divergences | max-treedepth % | min ESS | ESS / 1k leapfrogs |
|---|---|---|---|---|
| v2 (per-party σ hierarchy + corr) | 0 | 0–98% (seed-volatile) | ~1200–3100 | ~0.57 |
| v3 (shared σ + corr) | 0 | ~25% | ~1400–3100 | ~0.56 |
| v3b (isotropic) | 0 | 0% | ~1400–3700 | ~0.55 |
| v4 (per-party σ indep + corr) | 0 | ~59% | ~1800 | ~0.55 |

**Key findings:**

1. **ESS-per-leapfrog is ~equal (~0.55) across all σ-structures.** No reparameterisation
   improved intrinsic efficiency; they change how many leapfrogs NUTS takes per
   iteration, not the yield per gradient.
2. **Treedepth is seed-volatile.** v2 swung from 97.7% (seed 111) to 0% (seed 11) with
   no other change. Single-seed treedepth comparisons are unreliable.
3. **Invariance achieved** for v2/v3/v3b/v4 (max |Δpi_smooth| across orderings ≤ 0.11pp).

---

## 5. The real lever: party ordering (not σ structure)

`sum_to_zero_vector` uses an *ordered* ILR basis with a privileged pivot (first
position). The single worst-mixing cell in the whole model was the **tiny `Other`
(Annað, ~0.3%) party at the initial state `beta0`, when it sat in the pivot**. Moving it
out is the dominant efficiency lever (v4, seed 11):

| party ordering | pos1 / posP | treedepth | ESS / 1k lf | bottleneck |
|---|---|---|---|---|
| `other_first` | Other / J | 59.2% | 0.5 | Other |
| `other_last`  | J / Other | 2.6% | 1.3 | F |
| `big_ends` (D first, S last; Other middle) | D / S | 21.4% | 0.9 | Other |
| **`size_desc` (largest→smallest, Other last)** | S / Other | **0.0%** | **1.3** | B |

The rule: **order parties largest → smallest** so the near-boundary residual (`Annað`)
lands in the freest (last) coordinate and a well-conditioned large party holds the pivot.
With this ordering, ESS-per-leapfrog roughly doubles (~0.5 → ~1.3). Multi-seed: ORDER B
gave treedepth {0.1%, 0.0%, 33%} and ESS/lf {1.3, 1.2, 1.0} — robust efficiency, with
treedepth still seed-variable (a production fit saw 48%, 0 divergences, healthy ESS).

**Production order:** `S, D, M, C, B, F, V, J, P, Annað` (set in `read_polling_watch_data()`
in `R/data.R`, with a `stopifnot` guard). Under the anchored walk (§9) the order picks the
reference party, the last party that every election result reports (Annað), not a pivot
of the sampled coordinates.

---

## 6. Rejected approaches

- **`v4_sym` — symmetric (centred) `beta0`** instead of `sum_to_zero_vector`. Theory said
  it would remove the order-dependence; *tested*, it made things **worse** (size_desc went
  0% → 100% treedepth). The ordered ILR basis was doing useful conditioning; plain centring
  threw it away. Rejected. (File kept for the record.)
- **GMRF-state reparam.** A design panel candidate; on review it *reintroduced* the same
  `tau×raw` scale funnel (`s_party = exp(tau_sigma * w_sigma)`). Rejected.
- **Reduced-rank / factor covariance** (`ΛΛ' + diag(ψ²)`). Genuinely promising — best
  geometry ceiling, interpretable factors mapping onto the consolidation axis — but it only
  *approximates* the correlation (rank-k). Not chosen because the goal was to keep the
  *full* correlation; worth revisiting if a faster model is ever needed.
- **Bumping `max_treedepth`.** A band-aid, not a geometry fix; superseded by the ordering.

---

## 7. Correlation analysis (a v4 by-product)

`v4` exposes a reference-invariant **party-space P×P correlation `Omega`** of the RW
innovations (saved to `data/<date>/polling_watch_omega.parquet`). Marginal and partial
(precision-based, conditional) correlations reveal the bloc structure:

- **Trades (negative):** D↔M (right), S↔J / S↔B (left), P↔C (liberal) — the conditional
  view sharpens these.
- **Co-movement (positive):** F↔Other +0.35 (fringe bloc), D↔B (centre-right pair), and a
  D↔S "big-two consolidation vs fragmentation" axis that survives into the invariant basis.

Plotted by **`R/make_correlation_plots.R`** (reads the latest `polling_watch_omega.parquet`,
computes marginal + precision-based partial correlations, clusters parties by co-movement) →
`Figures/latent_rw_correlation_clustered.png` (clustered heatmap + dendrogram).

---

## 8. Files

| file | role |
|---|---|
| `Stan/polling_watch_v5_epoch.stan` | **production** model since 28 October 2026: v4 with pollster bias per epoch between election results (§10) |
| `Stan/polling_watch_v4.stan` | one pollster bias over the whole window; production until 28 October 2026, kept as v5's comparison |
| `Stan/polling_watch_v2.stan`, `_v3.stan`, `_v3b.stan` | invariant alternatives (kept as documentation) |
| `Stan/polling_watch_v4_sym.stan` | rejected (symmetric beta0); kept for the record |
| `Stan/polling_watch_v4_gmrf.stan` | rejected (GMRF-state; reintroduced a scale funnel); kept for the record |
| `Stan/polling_watch.stan` | original drop-one model (superseded; not invariant) |
| `R/fit_polling_watch.R` | fits v5 to `read_polling_watch_data()` from 1 January 2016, the election forecast's window (fixed party order + locale guard), with the epochs of `prepare_polling_watch_epochs()`; writes the `pi_smooth`, `Omega` and per-epoch house-effect parquets, the epoch table, the fit, `diagnostics.json` and the polls it saw |
| `R/sampler_check/fit_polling_watch_headless.R`, `equivalence_polling_watch.R`, `compare_polling_watch.R` | the §9 and §10 checks: headless fits (optionally leave-future-out), the point-by-point equivalence check, old against new |
| `R/make_correlation_plots.R` | renders the clustered marginal + partial (precision) correlation heatmap from `Omega` |

---

## 9. Anchored walk (28 October 2026)

The forward walk from `beta0` could not sample a window with several election results. Each
result (~200,000 votes) pins the sum of every innovation before it, a dense, stiff direction
that a diagonal metric meets only with a small step: on the full series (2013–2026, four
results) the step size fell to 0.002 and every transition reached the depth-11 cap, so the
live fit started at the 2021 election.

v4 now samples its walk as the election forecast does (`Stan/CLAUDE.md`, Parameterisation; the
model's PARAMETERISATION note). The walk is written in the levels of the P − 1 parties other
than the last party every result reports (Annað in the production order, §5). It is anchored
at the election dates, which are centred and stored relative to each result's log-ratios, with
non-centred Brownian bridges between them, a backward walk before the first and a forward walk
after the last. log φ is bounded above at 20. The posterior is the old one conditioned on
log φ < 20: `R/sampler_check/equivalence_polling_watch.R` passes on the full series and on the
window from 2021, and no posterior draw of either fit reaches the bound.

On the window from 2021, with the same data, the minimum bulk ESS per 1,000 gradients rises
from 0.43 to 2.57 (×5.9) and a transition takes 127 leapfrog steps instead of 511. On the full
series it rises from 0.029 to 0.615 (×20.9): the old model reached the depth-11 cap on every
transition, the new one never reaches its cap of 12 and has no divergent transition
(`R/sampler_check/RESULTS.md`, Fylgisvakt: anchored walk).

## 10. Epoch-varying pollster bias in production (28 October 2026)

Fylgisvakt is the election forecast's poll model without its fundamentals: a summary of where
the polls stand between elections, which metill.is replaces with the forecast as an election
nears. Since 28 October 2026 `R/fit_polling_watch.R` fits `polling_watch_v5_epoch.stan` on the
forecast's window, the polls and results since 1 January 2016, instead of v4 from the 2021
election.

v4 corrects every poll with one industry bias and one effect per house, learnt from all the
results in the window. v5 lets that bias move: each epoch, the stretch between two results,
has its own industry bias and house effects, and these random-walk from one epoch to the next
with the sampled scales `tau_mu` and `tau_house` (the model's header). The epoch since the
last result has no result at its end, so its bias rests on the first polls after that result,
read against it, and on the previous epoch's bias spread by the taus. Before §9, v5 kept the
forward walk and could not sample several results; it now samples v4's anchored walk, at
v4's cost (`R/sampler_check/RESULTS.md`, Fylgisvakt: epoch-varying pollster bias). On the
production window `tau_mu` has posterior median 0.15 and `tau_house` 0.05, on the logit scale
per epoch.

Leave-future-out fits on the production window, each to the polls dated before an election,
with the shares at the last poll date scored against the official national result
(`R/sampler_check/score_polling_watch_backtest.R`; percentage points over the ten parties):

| Election | Model | MAE | RMSE | CRPS | 90% coverage | 90% width |
|---|---|---|---|---|---|---|
| 2024 | v4 | 1.01 | 1.35 | 0.844 | 4 of 10 | 1.60 |
| 2024 | v5 | 0.73 | 0.85 | 0.542 | 7 of 10 | 2.86 |
| 2021 | v4 | 1.12 | 1.51 | 0.854 | 8 of 10 | 5.53 |
| 2021 | v5 | 1.29 | 1.50 | 0.865 | 10 of 10 | 6.33 |

In 2024 v4's intervals were too narrow and its largest error was Samfylkingin's (−3.2 points
against v5's −0.9): it applied to the 2021–2024 polls a correction learnt mostly from earlier
epochs. In 2021 the window holds only the 2016 and 2017 results, the bias is poorly pinned in
both models, and they tie on CRPS. Putting a fraction w of the industry bias back into the
shares (softmax(log π + w·mu_gamma), the hedge of the forecast) helps v4 in 2024, at best to an
MAE of 0.83 at w = 0.4, still worse than v5 without it, and hurts both models in 2021; v5 scores
best at w = 0 in both. Two elections are thin evidence: v5 wins one clearly and ties the other.
The case for it is that pollster bias does move between elections, which v4 cannot represent.

The switch moves the headline. On the polls to 27 September 2026, v4 put Samfylkingin at 25.8%
(90% interval 24.1–27.6) and Sjálfstæðisflokkurinn at 27.2% (25.5–28.9), Samfylkingin ahead
with probability 0.22; v5 puts them at 27.5% (24.8–30.2) and 24.8% (22.6–27.2), Samfylkingin
ahead with probability 0.86. v4's industry bias, learnt over 2016–2024, says the polls
understate Sjálfstæðisflokkurinn by 0.19 on the logit scale and overstate Samfylkingin by 0.05;
v5's bias for the epoch since the 2024 election is −0.11 and −0.04, so it lifts
Sjálfstæðisflokkurinn less and lifts Samfylkingin a little where v4 lowered it.
