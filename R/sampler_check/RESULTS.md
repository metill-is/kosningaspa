# Sampler checks

Newest first. Each section names the old and new model files by commit and sha256, the data, and the commands that reproduce it.

## Anchored walk, partially centred δ and log σ, bounded log φ (28 October 2026)

Results for the reparameterisation described in `Stan/CLAUDE.md` (Parameterisation): the
walk is anchored at election dates with non-centred Brownian bridges between them, δ and
log σ are partially centred cell by cell, the house effects stay non-centred, and log φ is
centred and bounded above at 20. Apart from that bound the posterior is unchanged.

- **Old model:** `Stan/polling_and_fundamentals_kjordaemi.stan` at 884560f (sha256
  `542027f4b282ccdc2040c70bf3d9b5cd0a682c095bddb2ec1068bbcc2b7be4b4`): backward
  non-centred walk, centred δ and log σ.
- **New model:** the same file at aa2ac1a (sha256
  `a4c8fc0475ddeb7292065ee66dadf413ce56ca1a23d3d806c082b3e33ae71767`).
- **Data:** harness arm R0's Stan data for the two Tier 0 cells, as `R/harness/fit_cell.R`
  freezes them in `results/harness/r0_<E>_<F>_s<seed>/inputs/stan_data.json` (production
  ignores R0's extra field `mu_gamma_scale`):
  - **2024 eve** (E = 2024-11-30, F = 2024-11-29; sha256
    `72725f7bedd0c74cac23970f7ca5dd9762c05d0c5dc6784a39180de6facb9ef0`): 287 national
    rows on D = 268 dates, three election results, 42 kjördæmi rows (the 2021 result and
    six Gallup and Maskína kjördæmi polls).
  - **2021 f180** (E = 2021-09-25, F = 2021-03-29; sha256
    `38424116808bb4c2dd28c2394d27b4caa872db1d4884e636d31eec0e3976641a`): 161 national
    rows on D = 155 dates, two election results, and 6 kjördæmi rows, all from the 2017
    result. Sósíalistaflokkurinn has no kjördæmi row.
- **Sampler:** 4 chains, 1000 warmup + 1000 sampling iterations, `init = 0`, seed 4100,
  `max_treedepth = 12`, for both models.

### Equivalence

`equivalence_anchored.R` maps each old point to the new model's unconstrained space through
the natural quantities (the walk path, the effects, the scales, log φ) and requires new log
density minus its log-Jacobian minus (old log density minus the old model's unused walk
column and its log-Jacobian) to be constant.

- **2024 eve and 2021 f180**, on 40 draws of each old 1000 + 1000 fit below and 15 points
  pushed off the posterior: **EQUIV PASS** on both. The constant varies by 7.0e-10 over 55
  points on 2024 eve (tolerance 1.8e-6) and 1.2e-9 over 54 points on 2021 f180 (tolerance
  1.2e-6). One more off-posterior point of 2021 f180 has log φ ≥ 20, outside the new
  model's support, and is excluded; no posterior draw of either fit reaches the bound. Each
  of the ten negative controls (nine log-Jacobian terms, and the unused column; the walk
  start's term is the same on both sides) makes the
  constant vary by at least 1.46 on 2024 eve and 2.27 on 2021 f180. Both models' generated
  quantities, run at the 40 posterior points with one seed, agree on 133,800 and 153,400
  values to a relative 1.9e-15 and 4.3e-15.
- **The other eight harness cells** (2017 eve and f14; 2021 eve, f14 and f47; 2024 f14,
  f47 and f180), on 20 draws of a 30 + 20 old fit (depth 8, seed 5100) and 8 off-posterior
  points each: **EQUIV PASS** on every cell, the constant within 9.4e-10 and the generated
  quantities within a relative 2.3e-15. Every control fails on every cell; the weakest, the
  unused walk column, varies by 0.0047–0.0098, over 4,000 times the tolerance (so short a fit
  barely moves that prior-only column). On the 2017 cells only these checks ran: the
  generated quantities were compared between the two models, and no forecast quantity was
  looked at or scored.

### Old against new

| | 2024 eve, old | 2024 eve, new | 2021 f180, old | 2021 f180, new |
|---|---|---|---|---|
| Tree depth of the post-warmup transitions | 11 (all) | 8 (all) | 11 (all) | 9 (3,997 of 4,000) |
| Leapfrog steps per transition | 2,047 | 255 | 2,051 | 511 |
| Step size | 0.0020–0.0023 | 0.0130–0.0147 | 0.0016–0.0017 | 0.0081–0.0097 |
| Divergent transitions | 0 | 0 | 0 | 3 |
| Min E-BFMI | 0.68 | 0.65 | 0.70 | 0.77 |
| Max R-hat, key quantities | 1.006 | 1.011 | 1.028 | 1.014 |
| Min bulk / tail ESS, key quantities | 1062 / 1089 | 579 / 690 | 145 / 135 | 289 / 321 |
| Min bulk ESS per 1,000 gradients | 0.130 | 0.567 (×4.4) | 0.0177 | 0.141 (×8.0) |
| ms per gradient | 0.96 | 1.13 | 0.57 | 0.88 |
| Wall time | 71.3 min | 14.5 min | 45.4 min | 16.2 min |
| Min bulk ESS per wall minute | 14.9 | 40.0 (×2.7) | 3.2 | 17.8 (×5.6) |

Key quantities as in `compare_fits.R` (`KEY`): 2,735 scalars on 2024 eve and 3,490 on 2021
f180. On 2021 f180 the minimum is `sigma_delta[9]` in both fits, the scale of
Sósíalistaflokkurinn's effects, which no kjördæmi row reports (below). On 2024 eve the
minimum moves from `Omega` to `tau_log_sigma`, whose bulk ESS falls from 3,006 to 579; per
1,000 gradients it still rises, from 0.37 to 0.57. The wall times were taken with other fits
on the machine (load average 5–13 on 10 cores), so the cost per gradient is not comparable
between fits; the ESS per gradient is the like-for-like measure. The old fits stop at depth
11, so the cap of 12 binds neither.

The fits agree within Monte Carlo error. Of the 50 election-day share quantiles, 1 differs by
more than 2 combined MCSEs on 2024 eve (|z| 2.75) and 2 on 2021 f180 (largest |z| 2.50);
about 2.3 are expected by chance. Of the key posterior means, 4.1 % differ by more than 2
MCSEs on 2024 eve (3 of 2,735 by more than 3; sd(z) = 1.00), and 0.1 % on 2021 f180 (none
by more than 3; sd(z) = 0.52).

### Divergences

The new model is not divergence-free on 2021 f180. At 1000 + 1000, seed 4100 gives 3
divergent transitions of 4,000 (all in chain 4) and seed 4101 none; with `adapt_delta = 0.9`
seed 4100 gives 1, its step sizes almost unchanged (0.0087–0.0097) and still 511 leapfrog
steps per transition. At 400 + 400, seeds 2701–2703 give 0, 0 and 2. 2024 eve gives none;
the other cells are under Generality.
Two mechanisms show up:

- The 2 at 400 + 400 are one point, reached after a trajectory took the house-effect scale
  `sigma_gamma[1]` from 0.02 to 1.93 (posterior median 0.077). The house effects are
  non-centred and the polls pin them, so the likelihood's curvature in `gamma_raw` grows with
  `sigma_gamma` squared, about 630 times its typical value there. This is the old model's
  house-effect parameterisation, unchanged; the tail it reaches is the prior's
  (`sigma_gamma ~ exponential(1)`).
- At the 4 at 1000 + 1000 (and one more in a variant that differs only in how the absent
  party's effects are coded), `sigma_gamma[1]` is ordinary and `sigma_delta[2]` sits in the
  top 11 % of its posterior (quantiles 0.89–0.995). That pattern was picked out of 3,517
  coordinates after the fact, where about 0.1 such coordinates are expected by chance, so it
  is a lead, not a diagnosis.

The fit with 3 divergent transitions agrees with the old, divergence-free fit within Monte
Carlo error (above).

### Generality

The new model on the eight non-2017 cells, 400 + 400 iterations, seed 2701 (2024 eve and
2021 f180 also 2702 and 2703), `max_treedepth = 12`, as arm R0 at b57a015: the same model
with the harness's generated quantities. Max R-hat and min bulk ESS are over the natural
quantities (walk, effects, scales, φ, fundamentals) and the scored election-day kjördæmi
shares; the last column is over the scored shares alone. No transition reached the cap.

| Cell | Absent party | Mean tree depth | Divergent | Max R-hat | Min bulk ESS (all; scored) | Scored bulk ESS per 1,000 gradients |
|---|---|---|---|---|---|---|
| 2024 eve, 3 seeds | no | 8.0–8.2 | 0, 0, 0 | 1.018, 1.016, 1.019 | 458, 237, 270; 1189, 1119, 1125 | 2.33, 2.21, 2.76 |
| 2024 f14 | no | 8 | 1 | 1.013 | 251; 1401 | 3.44 |
| 2024 f47 | no | 8 | 0 | 1.017 | 349; 1097 | 2.69 |
| 2024 f180 | no | 8 | 0 | 1.016 | 318; 1336 | 3.28 |
| 2021 eve | no | 9 | 0 | 1.056 | 69; 271 | 0.33 |
| 2021 f14 | no | 9 | 0 | 1.030 | 152; 339 | 0.41 |
| 2021 f47 | yes | 9 | 1 | 1.034 | 130; 316 | 0.39 |
| 2021 f180, 3 seeds | yes | 9 | 0, 0, 2 | 1.068, 1.029, 1.016 | 43, 99, 142; 246, 269, 403 | 0.30, 0.33, 0.49 |

The 2024 cells sample well everywhere: R-hat at most 1.019 and at least 1,097 scored draws'
worth of bulk ESS from 1,600. The 2021 cells are an order of magnitude slower per gradient,
and their weakest parameter is usually `sigma_delta[9]`, Sósíalistaflokkurinn's kjördæmi
scale (worst in 2021 eve, f14 and two f180 seeds). The party had no previous result; two
kjördæmi polls inform its effects in 2021 eve and f14 (12 of 18 rows), and none in f47 and
f180, where its effects lie on the ridge below. At 400 + 400 that scale reaches R-hat
1.056–1.068; at 1000 + 1000 on 2021 f180 (above) its R-hat is 1.012 and bulk ESS 289. The
harness runs 1000 + 2000 from Tier 1 on.

### The absent party

A party that polls nationally but that no kjördæmi row reports enters the constituency
likelihood only through Annað's log-ratio, minus the sum of the others. Raising its effect in
a kjördæmi by x and lowering every reported party's effect there by x / (P − n_absent) leaves
each such row unchanged, so only the priors bound that direction. In 2021 f180 that party is
Sósíalistaflokkurinn (`sigma_delta[9]`). The ridge belongs to the model, not to either
parameterisation; the new one mixes it faster (bulk ESS 289 against 145). Sampling the
reported parties' effects net of that shift turns the ridge into the absent party's own
coordinate. In 400 + 400 trials on 2021 f180 (not recorded in this repo) that mixed `sigma_delta[9]` 2 to 29 times better
on two seeds of three, but on the third a warmup chain fell into a reported party's
scale funnel (max R-hat 1.13), so the model does not use it.

### Not covered

- **The bound.** The new posterior is the old one conditioned on log φ < 20; no posterior
  draw above reaches it, but data that pushed φ that high would be fitted differently.
- **Live data.** Every fit here uses arm R0's frozen data for its cell, never `data/`.
- **2017.** The 2017 cells were checked for equivalence only, not sampled.

### Reproducing

Each cell's data, from the project root with the harness snapshot in place
(`harness/README.md`), shown for 2024 eve (2021 f180: E = 2021-09-25, F = 2021-03-29):

```r
box::use(R/harness/arms[arm], R/harness/info_set[info_set, validate_inputs])
a <- arm("r0")
info <- info_set(as.Date("2024-11-30"), as.Date("2024-11-29"))
validate_inputs(info)
cmdstanr::write_stan_json(a$build(info, a$read_config(a$config))$data, "<dir>/eve2024.json")
```

This rebuilt both files with the sha256s above on 28 October 2026. Then, with `<dir>` a
scratch folder:

```bash
git show 884560f:Stan/polling_and_fundamentals_kjordaemi.stan > <dir>/kjordaemi_old.stan
git show aa2ac1a:Stan/polling_and_fundamentals_kjordaemi.stan > <dir>/kjordaemi_new.stan
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/fit_headless.R --out=<dir>/eve_old \
  --stan=<dir>/kjordaemi_old.stan --data-json=<dir>/eve2024.json
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/equivalence_anchored.R \
  --old-stan=<dir>/kjordaemi_old.stan --new-stan=<dir>/kjordaemi_new.stan \
  --data-json=<dir>/eve2024.json --draws-dir=<dir>/eve_old/cmdstan --out=<dir>/equivalence_eve.csv
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/fit_headless.R --out=<dir>/eve_new \
  --stan=<dir>/kjordaemi_new.stan --data-json=<dir>/eve2024.json
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/compare_fits.R --old=<dir>/eve_old \
  --new=<dir>/eve_new --data-json=<dir>/eve2024.json --out=<dir>/compare --label=eve
```

`fit_headless.R` defaults to seed 4100, 1000 + 1000 iterations and `max_treedepth = 12`;
`--seed=4101` gives the second seed. It has no `adapt_delta` option (the 0.9 fit passed it to
cmdstanr's `$sample()`). The other eight cells: the old fit with `--max-treedepth=8
--warmup=30 --sampling=20 --seed=5100`, then `equivalence_anchored.R` with `--n=20 --n-off=8`.


## Centred δ and log σ, `max_treedepth = 12` (24 October 2026)

Results for the change that centres the constituency effects (`delta_raw` →
`delta_free ~ normal(0, sigma_delta[p])`) and the log volatilities (`log_sigma_raw` →
`log_sigma ~ normal(mu_log_sigma, tau_log_sigma)`) in
`Stan/polling_and_fundamentals_kjordaemi.stan`, and fits it with `max_treedepth = 12`.
Run on 24 October 2026.

- **Old model:** `Stan/polling_and_fundamentals_kjordaemi.stan` at master 5d22031
  (sha256 `f3c7f48d17024538548dbcc59a2514d82a89297680c8f849eb9d0273d3d687de`), fitted at the old default `max_treedepth = 10`.
- **New model:** the same file at ebb3e8d, after this change (sha256 `5516fa513823bad80e4803041e3fa9cad93f70ca9c2f6aa34e1bb98905f9b6f3`), fitted at
  `max_treedepth = 12`.
- **Data:** the interactive fit script's `stan_data` for the eve of the 2024 election,
  built by `fit_headless.R` with no cutoff on 16 October 2026 (`stan_data_eve.json`, sha256
  `acafe475e6ca31a483b98316ed6fa1a70b4646fd271f761c34607199ff9a35bb`): 288 national polls
  on D = 268 dates, 6 constituency-level polls (36 poll-by-constituency rows), 10 parties,
  6 constituencies.
  `data/` has changed since (`fundamentals_data.csv` was refreshed on 23 October 2026),
  so rebuilding it today gives different data.
- **Sampler:** 4 chains, 1000 warmup + 1000 sampling iterations, `init = 0`, seed 4100.

### Equivalence

`equivalence.R` on 40 posterior draws of the old fit and 15 points pushed off the
posterior in every changed coordinate: **EQUIV PASS**.

- Log density: new minus (old minus the log-Jacobian) is constant to within 4.7e-10 over
  the posterior points and 7.0e-10 over all 55 points. It takes only the values −2, −1, 0
  and +1 times 2.3e-10, the spacing of doubles near |lp| = 1.6 million (the posterior
  points; the off-posterior points reach 3.8 million). The tolerance is 3.8e-6. Leaving out the
  `log tau_log_sigma` term, the `log sigma_delta` term or the whole Jacobian makes the
  constant vary by 31.5, 27.4 and 22.5 across the 55 points, so the check sees each term.
- Generated quantities: both models, run at the 40 posterior points with one seed, agree
  on all 130,600 values to a relative 1.4e-15. Every `y_rep` count is identical; the
  continuous quantities differ only by rounding. A mutant that keeps `sigma_delta *` in
  `delta_pred` passes the log-density part and fails this part (largest relative
  difference 8.0, on draws of a 150 + 50 old fit).

### Old against new

| | Old (depth 10) | New (depth 12) |
|---|---|---|
| Transitions at the depth cap | 4000 of 4000 | 0 of 4000 (all stop at depth 11) |
| Leapfrog steps per transition | 1023 | 2047 |
| Divergent transitions | 0 | 0 |
| Min E-BFMI | 0.69 | 0.69 |
| ms per gradient | 0.845 | 0.839 |
| Max R-hat, key quantities | 1.046 | 1.007 |
| Min bulk / tail ESS, key quantities | 139 / 158 | 1117 / 1014 |
| Min bulk ESS, all parameters | 138 | 942 |
| Min bulk ESS per 1,000 gradients | 0.034 | 0.136 (×4.0) |
| Wall time | 28.8 min | 59.4 min |
| Min bulk ESS per wall minute | 4.8 | 18.8 (×3.9) |

Key quantities are the 2,717 scalars of `beta`, `beta0`, `sigma`, `Omega`, `gamma`,
`phi`, `delta_pred`, the hierarchy and fundamentals parameters, `mu_pred`,
`mu_gamma_pred` and the election-day shares `pi_eday` (`compare_fits.R`, `KEY`).
At depth 10 the old model has not converged at 1000 + 1000: `sigma_delta` reaches
R-hat 1.046 with a minimum bulk ESS of 139. The largest gains are in the hierarchy
parameters of the centred blocks: minimum bulk ESS of `sigma_delta` rises from 139 to 4054,
of `mu_log_sigma` from 262 to 2991 and of `tau_log_sigma` from 387 to 2198. The table
compares the two changes together; it does not separate centring from the deeper cap.

The ESS per gradient is the like-for-like measure. The wall times were taken with other
jobs on the machine (load average 5–10 on 10 cores), but the cost per gradient was the
same for both fits.

The two fits agree within Monte Carlo error. Of the 50 election-day share quantiles
(5, 25, 50, 75 and 95 % for each party), 2 differ by more than 2 combined MCSEs
(largest |z| 2.20; about 2.3 are expected by chance). Of the 2,717 key posterior means,
4.6 % differ by more than 2 MCSEs and none by more than 3, with sd(z) = 1.02.

Not run: a second data set (such as the polls up to 31 October 2024) or other seeds.

### Reproducing

From the project root, with `<dir>` a scratch folder holding `stan_data_eve.json`:

```bash
git show 5d22031:Stan/polling_and_fundamentals_kjordaemi.stan > <dir>/kjordaemi_old.stan
git show ebb3e8d:Stan/polling_and_fundamentals_kjordaemi.stan > <dir>/kjordaemi_new.stan
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/fit_headless.R --out=<dir>/eve_old \
  --stan=<dir>/kjordaemi_old.stan --data-json=<dir>/stan_data_eve.json --max-treedepth=10
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/equivalence.R --old-stan=<dir>/kjordaemi_old.stan \
  --new-stan=<dir>/kjordaemi_new.stan --data-json=<dir>/stan_data_eve.json \
  --draws-dir=<dir>/eve_old/cmdstan --out=<dir>/equivalence_eve.csv
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/fit_headless.R --out=<dir>/eve_new \
  --stan=<dir>/kjordaemi_new.stan --data-json=<dir>/stan_data_eve.json --max-treedepth=12
LC_ALL=en_US.UTF-8 Rscript R/sampler_check/compare_fits.R --old=<dir>/eve_old \
  --new=<dir>/eve_new --data-json=<dir>/stan_data_eve.json --out=<dir>/compare --label=eve
```

`fit_headless.R` defaults to seed 4100 and 1000 + 1000 iterations.

ebb3e8d's model file has the new model's sha256 above; the replication draws gathered
parties by identity only later (5f09c5b). Both models read `n_parties_n_rep` and `n_parties_k_rep`, which
`prepare_stan_data()` no longer writes, so use the frozen `stan_data_eve.json`. The model
file from 5f09c5b to 884560f is a different model: its `y_rep_national` and `y_rep_k` gather the parties active
at each date and the parties each constituency poll reported, from the full sum-to-zero
vector, as the likelihood does, and it needs `n_active_d` and `active_index_d`, which
`stan_data_eve.json` lacks. On the eve data its replications change on dates 1–99 and on 9
of the 36 constituency rows. As that changes how many random numbers those draws use, every
random quantity generated after them can change too, so `equivalence.R`'s
generated-quantities check fails between the 5d22031 model and that file. The current file
(aa2ac1a on) is the anchored model of the 28 October section, whose parameters
`equivalence.R` cannot map; `equivalence_anchored.R` checks it against 884560f.
