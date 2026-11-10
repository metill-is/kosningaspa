# V2 recovery

Written by `LC_ALL=en_US.UTF-8 Rscript R/harness/recovery.R report`, from the replicates of `R/harness/recovery.R <E> <F> <first_seed> <n> <iter_warmup> <iter_sampling>` (the script's header gives the protocol). Each replicate's truth is one draw of V2's prior and its data a draw of the model on the real cell's design; no forecast quantity is computed.

- Stan file `Stan/forecast_v2.stan` (sha256 `d6d7b2cf128e98e3a504591096b53353ed23cffc463696ff7f93f40284aa2d71`), config `harness/configs/v2.json` (sha256 `f77b187d21a634c355cf888819405bfc1a385f52376259fcba988efbce65a0df`)
- Commit(s) at lane start: `b352436c52fea80084cb97ca3a9e00de55b4a918`
- Replicates fitted from a tree with changes (at lane start): 0
- Replicates whose commit or tree changed while their lane ran: none
- CmdStan 2.38.0; 4 chains, max_treedepth 12, init 0
- Redraw rule: a replicate is redrawn when a reported party's share of a closed election's national result (its kjördæmi counts summed), or its mean share over an epoch's national polls, leaves [0.5%, 70%]; the rejection rate is the rejected draws over all draws. "Real cell" says whether the real cell's own data pass the rule
- Folders without a meta.json (not counted): 0

## Designs

| Design | Replicates | Seeds | Iterations | Draws | Rejected | Rejection rate | Minutes (median, max) | Stop rule tripped | Real cell |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2024 eve | 20 | 3001–3020 | 400 + 400 | 3681 | 3661 | 0.9946 | 47.9, 84.7 | 1 | redrawn: result 3 (2021-09-25): Annað 0.495% |
| 2021 f180 | 10 | 3101–3110 | 400 + 400 | 688 | 678 | 0.9855 | 21.7, 41.1 | 1 | kept |

Truth elements per replicate:

| Group | 2024 eve | 2021 f180 |
| --- | --- | --- |
| τ_μ (tau_mu[1]) | 1 | 1 |
| s̄_E (s_bar_E[1]) | 1 | 1 |
| open-epoch μ (mu[n_epoch, p]) | 10 | 10 |
| closed δ (delta[e, k, p], e <= n_closed) | 180 | 120 |
| open δ (delta[n_epoch, k, p]) | 60 | 60 |

## Coverage

A truth is inside the 90% interval when its posterior quantile q = mean(draws < truth) is in [0.05, 0.95], and inside the 50% when q is in [0.25, 0.75]. Scalar groups: the count of truths inside, against Decision 7's band for 30 truths (90%: at least 22; 50%: 8 to 22). Vector groups: the mean over replicates of each replicate's share of the group's elements inside, against the nominal coverage ± 3 standard errors (the sd over replicates over √n).

| Group | Kind | Replicates | 50% inside | 50% band | 50% | 90% inside | 90% band | 90% |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| τ_μ (tau_mu[1]) | scalar | 30 | 10 of 30 | [8, 22] | PASS | 27 of 30 | [22, 30] | PASS |
| s̄_E (s_bar_E[1]) | scalar | 30 | 14 of 30 | [8, 22] | PASS | 28 of 30 | [22, 30] | PASS |
| open-epoch μ (mu[n_epoch, p]) | vector | 30 | 0.503 (SE 0.040) | [0.379, 0.621] | PASS | 0.917 (SE 0.016) | [0.852, 0.948] | PASS |
| closed δ (delta[e, k, p], e <= n_closed) | vector | 30 | 0.500 (SE 0.014) | [0.457, 0.543] | PASS | 0.899 (SE 0.011) | [0.866, 0.934] | PASS |
| open δ (delta[n_epoch, k, p]) | vector | 30 | 0.472 (SE 0.017) | [0.449, 0.551] | PASS | 0.890 (SE 0.011) | [0.868, 0.932] | PASS |

## Stop rule

Decision 5's stop rule on each replicate (`R/harness/compare_runs.R`'s `stop_rule_at()`; its gate-scope R-hat over the parameters and the truth elements). A replicate that trips it stays in every coverage count.

```
GO recovery_2021-09-25_2021-03-29_s3101 lp_rhat=1.0114 divergent=0/1600 near_cap=1/1600 max_rhat=1.0181; recorded: minutes=23.2 treedepth_hits=0 ess_bulk_per_1k_grad=0.155
STOP recovery_2021-09-25_2021-03-29_s3102 lp_rhat=1.0128 divergent=51/1600 near_cap=5/1600 max_rhat=1.0765; recorded: minutes=10.7 treedepth_hits=0 ess_bulk_per_1k_grad=0.092; stop: 51 of 1600 transitions divergent > 1%
GO recovery_2021-09-25_2021-03-29_s3103 lp_rhat=1.0051 divergent=0/1600 near_cap=2/1600 max_rhat=1.0288; recorded: minutes=22.5 treedepth_hits=0 ess_bulk_per_1k_grad=0.110
GO recovery_2021-09-25_2021-03-29_s3104 lp_rhat=1.0023 divergent=0/1600 near_cap=0/1600 max_rhat=1.0313; recorded: minutes=14.4 treedepth_hits=0 ess_bulk_per_1k_grad=0.240
GO recovery_2021-09-25_2021-03-29_s3105 lp_rhat=1.0153 divergent=0/1600 near_cap=0/1600 max_rhat=1.0243; recorded: minutes=14.4 treedepth_hits=0 ess_bulk_per_1k_grad=0.178
GO recovery_2021-09-25_2021-03-29_s3106 lp_rhat=1.0018 divergent=0/1600 near_cap=1/1600 max_rhat=1.0216; recorded: minutes=41.1 treedepth_hits=0 ess_bulk_per_1k_grad=0.054
GO recovery_2021-09-25_2021-03-29_s3107 lp_rhat=1.0120 divergent=1/1600 near_cap=0/1600 max_rhat=1.0185; recorded: minutes=21.5 treedepth_hits=0 ess_bulk_per_1k_grad=0.153
GO recovery_2021-09-25_2021-03-29_s3108 lp_rhat=1.0014 divergent=1/1600 near_cap=0/1600 max_rhat=1.0531; recorded: minutes=23.6 treedepth_hits=0 ess_bulk_per_1k_grad=0.074
GO recovery_2021-09-25_2021-03-29_s3109 lp_rhat=1.0115 divergent=8/1600 near_cap=5/1600 max_rhat=1.0356; recorded: minutes=14.9 treedepth_hits=0 ess_bulk_per_1k_grad=0.098
GO recovery_2021-09-25_2021-03-29_s3110 lp_rhat=1.0082 divergent=0/1600 near_cap=1/1600 max_rhat=1.0170; recorded: minutes=21.9 treedepth_hits=0 ess_bulk_per_1k_grad=0.211
STOP recovery_2024-11-30_2024-11-29_s3001 lp_rhat=1.0063 divergent=0/1600 near_cap=44/1600 max_rhat=1.0243; recorded: minutes=39.6 treedepth_hits=0 ess_bulk_per_1k_grad=0.234; stop: 44 of 1600 draws within 0.5 of a cap > 1%
GO recovery_2024-11-30_2024-11-29_s3002 lp_rhat=1.0038 divergent=0/1600 near_cap=0/1600 max_rhat=1.0245; recorded: minutes=45.2 treedepth_hits=0 ess_bulk_per_1k_grad=0.149
GO recovery_2024-11-30_2024-11-29_s3003 lp_rhat=1.0057 divergent=0/1600 near_cap=0/1600 max_rhat=1.0199; recorded: minutes=41.1 treedepth_hits=0 ess_bulk_per_1k_grad=0.225
GO recovery_2024-11-30_2024-11-29_s3004 lp_rhat=1.0037 divergent=0/1600 near_cap=0/1600 max_rhat=1.0255; recorded: minutes=75.9 treedepth_hits=0 ess_bulk_per_1k_grad=0.074
GO recovery_2024-11-30_2024-11-29_s3005 lp_rhat=1.0059 divergent=0/1600 near_cap=6/1600 max_rhat=1.0256; recorded: minutes=47.6 treedepth_hits=0 ess_bulk_per_1k_grad=0.117
GO recovery_2024-11-30_2024-11-29_s3006 lp_rhat=1.0109 divergent=0/1600 near_cap=5/1600 max_rhat=1.0257; recorded: minutes=74.9 treedepth_hits=0 ess_bulk_per_1k_grad=0.035
GO recovery_2024-11-30_2024-11-29_s3007 lp_rhat=1.0037 divergent=0/1600 near_cap=0/1600 max_rhat=1.0226; recorded: minutes=36.5 treedepth_hits=0 ess_bulk_per_1k_grad=0.294
GO recovery_2024-11-30_2024-11-29_s3008 lp_rhat=1.0182 divergent=0/1600 near_cap=0/1600 max_rhat=1.0213; recorded: minutes=39.9 treedepth_hits=0 ess_bulk_per_1k_grad=0.303
GO recovery_2024-11-30_2024-11-29_s3009 lp_rhat=1.0033 divergent=0/1600 near_cap=0/1600 max_rhat=1.0202; recorded: minutes=21.8 treedepth_hits=0 ess_bulk_per_1k_grad=0.668
GO recovery_2024-11-30_2024-11-29_s3010 lp_rhat=1.0120 divergent=0/1600 near_cap=0/1600 max_rhat=1.0221; recorded: minutes=72.2 treedepth_hits=0 ess_bulk_per_1k_grad=0.109
GO recovery_2024-11-30_2024-11-29_s3011 lp_rhat=1.0434 divergent=0/1600 near_cap=1/1600 max_rhat=1.0308; recorded: minutes=78.1 treedepth_hits=0 ess_bulk_per_1k_grad=0.043
GO recovery_2024-11-30_2024-11-29_s3012 lp_rhat=1.0062 divergent=0/1600 near_cap=0/1600 max_rhat=1.0234; recorded: minutes=38.4 treedepth_hits=0 ess_bulk_per_1k_grad=0.145
GO recovery_2024-11-30_2024-11-29_s3013 lp_rhat=1.0113 divergent=0/1600 near_cap=0/1600 max_rhat=1.0364; recorded: minutes=72.6 treedepth_hits=0 ess_bulk_per_1k_grad=0.046
GO recovery_2024-11-30_2024-11-29_s3014 lp_rhat=1.0196 divergent=7/1600 near_cap=0/1600 max_rhat=1.0570; recorded: minutes=48.5 treedepth_hits=0 ess_bulk_per_1k_grad=0.037
GO recovery_2024-11-30_2024-11-29_s3015 lp_rhat=1.0091 divergent=0/1600 near_cap=0/1600 max_rhat=1.0202; recorded: minutes=42.8 treedepth_hits=0 ess_bulk_per_1k_grad=0.210
GO recovery_2024-11-30_2024-11-29_s3016 lp_rhat=1.0010 divergent=0/1600 near_cap=0/1600 max_rhat=1.0195; recorded: minutes=43.3 treedepth_hits=0 ess_bulk_per_1k_grad=0.122
GO recovery_2024-11-30_2024-11-29_s3017 lp_rhat=1.0077 divergent=0/1600 near_cap=0/1600 max_rhat=1.0323; recorded: minutes=52.4 treedepth_hits=0 ess_bulk_per_1k_grad=0.084
GO recovery_2024-11-30_2024-11-29_s3018 lp_rhat=1.0088 divergent=0/1600 near_cap=0/1600 max_rhat=1.0195; recorded: minutes=84.7 treedepth_hits=0 ess_bulk_per_1k_grad=0.123
GO recovery_2024-11-30_2024-11-29_s3019 lp_rhat=1.0052 divergent=0/1600 near_cap=0/1600 max_rhat=1.0204; recorded: minutes=48.2 treedepth_hits=0 ess_bulk_per_1k_grad=0.251
GO recovery_2024-11-30_2024-11-29_s3020 lp_rhat=1.0113 divergent=0/1600 near_cap=1/1600 max_rhat=1.0438; recorded: minutes=69.7 treedepth_hits=0 ess_bulk_per_1k_grad=0.029
```

## Verdict

RECOVERY PASS
