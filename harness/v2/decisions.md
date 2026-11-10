# V2 decisions

One row per model step (M), parameterisation step (P) and model change of V2, with its verdict
in the step log's own words and the commit that recorded it. An M-step's verdict is the stop
rule's (a sampler check, not a forecast score); a P-step's is the adoption rule's. The full
narrative, with every table and command, is `harness/archive/steps.md`.

| Step | What it changed | Verdict (the log's words) | Commit |
|---|---|---|---|
| `v2s-m0` | The core: the walk, national and kjördæmi polls, the houses, a static δ, bounded concentrations | GO on all four fits | 3d14e10 |
| `v2s-p1` | The walk anchored at the closed elections' dates (`walk_form = 1`) | KEEP v2s-m0 (fails condition 3: 10 divergences on 2021 f180) | dca33d1 |
| `v2s-m1` | The epoch bias: the industry bias and each house's deviations step at each epoch | GO on both fits | 6cbbb8a |
| `v2s-m2` | The newcomer pool: an estimated mean and scale of a newcomer's entry bias | GO on both fits | 6fa21ce |
| `v2s-m3` | The election-day shock, one per closed election, shared by its kjördæmi | GO on both fits | 436aa8a |
| `v2s-m4` | The δ walk: kjördæmi deviations walk across elections | GO on all four fits | 98bcba6 |
| `v2s-p2` | The δ walk held centred at the closed elections (`delta_form = 1`) | KEEP v2s-m4 (fails condition 3: 8 divergences on 2021 f180) | e647723 |
| `v2s-m5` | The fundamentals pseudo-observation of the latent national shares at E | GO on both fits | b2556b3 |
| `v2` (candidate) | `v2s-m5`'s config under the candidate role, with five one-switch ablations | M5 stands, so every component is built; neither P-step was adopted, so both forms are 0 | b2556b3 |
| Post-review change | A newcomer's house level enters against the existing parties' mean, under `epoch_bias = 1` | One short fit of `v2` on 2021 f180: the stop rule gives GO | cbc53d3, b0c1e17 |
| `v2s-p3` | The anchored walk on V2 at `adapt_delta` 0.95 (lean, one seed) | KEEP V2, on one seed | d86d9ac |
| `v2s-p4` | The anchored walk on V2 at CmdStan's default `adapt_delta`, 0.8 (lean, one seed) | KEEP V2, on one seed (fails conditions 2 and 3) | 1e7eed8 |
| `v2s-m6` | The post-election multiplier: walk steps within 90 days after an election scaled by 1 + τ_post | Closes about a fifth of V2's 2024-eve MAE gap to R0 and none of the 2021 gap | 6e66f92, 4b7223a |
| `v2s-m7` | The calendar drift of the bias on half-yearly and yearly knots, in place of the epoch walk | GO on both runs; by the lean reading m7 does not improve on m6 | 5127f5b, e585274 |
| 8 Nov search | Exploratory variants of `v2s-m6`, fitted outside the repository; x-nn sets `newcomer_pool = 0` | The newcomer pool misleads; x-nn not yet made an arm or a committed run list | a1d2364 |
| Simple core | The file cut to one configuration, with the campaign multiplier on the walk step leaving an election | Same model as the tagged file at that configuration: log density and gradient equal at 28 points on each of two cells | 1c3a42b |

Two rows have two commits: the first built the step, the second recorded its fits or scores.
The likelihood's rewrite in matrix form (258d3c8) changed computation only and has no row.

**The working model.** On 10 November 2026 the owner chose the simple core as the working
model: a constant poll bias, static constituency deviations, the fundamentals
pseudo-observation, and the campaign multiplier on the walk step leaving an election. It has no
epoch walk, newcomer pool, shock, δ walk, post window or calendar drift. The cut is done:
`Stan/forecast_v2.stan` is that one configuration, the switched file and its harness are at the
tag `v2-switched-final`, and `harness/v2/scoring.md` holds the core's scores against R0.

**What the scores say.** On CRPS10 (pp, lower is better) the core scores 0.850 on 2024 eve, 0.779
on 2021 eve and 1.351 on 2021 at 180 days, against R0's 0.842, 1.080 and 1.501
(`harness/v2/scoring.md`). By the election-level reading its lead over R0, 0.109 pp over the two
elections, is under twice the outcome noise of 0.111 pp, so the two are indistinguishable. The
nine-switch V2 scored 0.891 on 2024 eve and 1.562 on 2021 at 180 days against the same R0 fits
(`harness/archive/v2/scoring.md`). The core's three fits are, draw for draw, those of the 8 November
search's variant x-r0w (checked on 10 November 2026; the search's fits are not in the repository),
so they are that trial and add none to the count below.

**How many variants stand behind that choice.** `harness/trials.csv` counts them: 22 arms with
draws, 17 of them conceived after scores were seen, so a choice among them is exploratory on
2021 and 2024 until another election confirms it. Score differences between two arms are read
by the election-level reading of `R/harness/score_report.R` (`election_reading()` and
`outcome_noise()` in `R/harness/scores.R`), not by the per-cell tie rule.

**Where the narrative is.** `harness/archive/steps.md` is the full step log, frozen on
10 November 2026. The diagnostics tables of each step are in `harness/archive/v2/steps/`, and the
build plans are recorded by pull requests #4 (Phase 0), #7 (Phase 1) and #21 (Phase 2).
