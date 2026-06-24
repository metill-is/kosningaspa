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

**Production order:** `S, D, M, C, B, F, V, J, P, Annað` (set in `fit_polling_watch.R`
with a `stopifnot` guard).

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

Figures: `Figures/latent_rw_correlation_invariant.png` (marginal + partial) and
`Figures/latent_rw_correlation_clustered_v4.png` (hierarchically clustered + dendrogram).

---

## 8. Files

| file | role |
|---|---|
| `Stan/polling_watch_v4.stan` | **production** model |
| `Stan/polling_watch_v2.stan`, `_v3.stan`, `_v3b.stan` | invariant alternatives (kept as documentation) |
| `Stan/polling_watch_v4_sym.stan` | rejected (symmetric beta0); kept for the record |
| `Stan/polling_watch_v4_gmrf.stan` | rejected (GMRF-state; reintroduced a scale funnel); kept for the record |
| `Stan/polling_watch.stan` | original drop-one model (superseded; not invariant) |
| `R/fit_polling_watch.R` | fits v4 with size-descending order + locale guard; writes `pi_smooth` + `Omega` parquets |
