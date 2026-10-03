#!/usr/bin/env bash
# Runs the epoch-bias experiment grid, two fits at a time, re-running the
# evaluation after every stage. Each fit writes results/epoch_bias/<tag>/ and a
# log under results/epoch_bias/logs/; the DIAG lines collect in logs/driver.log.
#
#   bash R/epoch_bias_experiments.sh              # whole grid, in priority order
#   bash R/epoch_bias_experiments.sh full bt24    # only some stages
#
# Stages: full (2016+ window, v4 vs v5); bt24 / bt21 / bt17 (leave-future-out
# backtests of the 2024 / 2021 / 2017 elections); sim (simulate from v4_full
# with known drifting bias, fit v5 and v4); sens (v5 tau-prior sensitivity);
# fix (v5 with a fixed small shock, tau = 0.05).
#
# Model labels: v4 = constant bias over 2016+, v5 = epoch random walk over
# 2016+, v4w1 = v4 from the previous election only (bias learnt from the open
# epoch alone), v4w2 = v4 from the election before that (the production window
# rule: one closed cycle plus the open one), v5fix05 = v5 with tau fixed.
#
# Sampling: 3 chains x (400 warmup + 600 draws) per fit, two fits at a time, so
# the 6 chains stay on the M1 Pro's 8 performance cores (a chain pushed onto an
# efficiency core holds up its whole fit). A 2016+ fit ran at about 19
# draws/min/chain with 8 chains on the machine (measured 2026-09-30).
#
# No --no-init-file: box.path comes from ~/.Rprofile.
set -uo pipefail
cd "$(dirname "$0")/.."
export LC_ALL=en_US.UTF-8
LOGS=results/epoch_bias/logs
mkdir -p "$LOGS"
FIT=R/fit_polling_watch_epoch.R
SAMPLING_OPTS="--chains=3 --warmup=400 --sampling=600"

fit() { # tag, then options for the fit script
  local tag=$1
  shift
  Rscript "$FIT" --tag="$tag" $SAMPLING_OPTS "$@" >"$LOGS/$tag.log" 2>&1
  local rc=$?
  echo "$(date '+%F %T') rc=$rc $(grep -h '^DIAG' "$LOGS/$tag.log" || echo "DIAG tag=$tag MISSING")" >>"$LOGS/driver.log"
}

pair() { # runs two fit() calls concurrently: pair "tagA opts..." "tagB opts..."
  fit $1 &
  fit $2 &
  wait
}

stages=${*:-full bt24 bt21 bt17 sim sens fix}
for s in $stages; do
  echo "$(date '+%F %T') START stage=$s" >>"$LOGS/driver.log"
  case $s in
  full)
    pair "v4_full --model=v4" "v5_full --model=v5"
    ;;
  bt24)
    pair "v4_bt2024 --model=v4 --cutoff=2024-11-30" "v5_bt2024 --model=v5 --cutoff=2024-11-30"
    pair "v4w1_bt2024 --model=v4 --start=2021-09-25 --cutoff=2024-11-30" "v4w2_bt2024 --model=v4 --start=2017-10-28 --cutoff=2024-11-30"
    ;;
  bt21)
    pair "v4_bt2021 --model=v4 --cutoff=2021-09-25" "v5_bt2021 --model=v5 --cutoff=2021-09-25"
    pair "v4w1_bt2021 --model=v4 --start=2017-10-28 --cutoff=2021-09-25" "v4w2_bt2021 --model=v4 --start=2016-10-29 --cutoff=2021-09-25"
    ;;
  bt17)
    pair "v4_bt2017 --model=v4 --cutoff=2017-10-28" "v5_bt2017 --model=v5 --cutoff=2017-10-28"
    pair "v4w1_bt2017 --model=v4 --start=2016-10-29 --cutoff=2017-10-28" "v4w1_full --model=v4 --start=2024-11-30"
    ;;
  sim)
    Rscript R/epoch_bias_simulate.R --from=v4_full --tau_mu=0.12 --tau_house=0.05 --seed=1 --tag=sim1 >"$LOGS/sim1_generate.log" 2>&1
    pair "sim1_v5_full --model=v5 --data_rds=results/epoch_bias/sim1_data/meta.rds" \
      "sim1_v4_full --model=v4 --data_rds=results/epoch_bias/sim1_data/meta.rds"
    ;;
  sens)
    pair "v5_full_tp05 --model=v5 --tau_prior=0.05" "v5_full_tp25 --model=v5 --tau_prior=0.25"
    ;;
  fix)
    pair "v5fix05_full --model=v5 --estimate_tau=0 --tau_mu=0.05 --tau_house=0.05" \
      "v5fix05_bt2024 --model=v5 --estimate_tau=0 --tau_mu=0.05 --tau_house=0.05 --cutoff=2024-11-30"
    ;;
  esac
  Rscript R/epoch_bias_evaluate.R >"$LOGS/evaluate.log" 2>&1
  echo "$(date '+%F %T') END stage=$s evaluate_rc=$?" >>"$LOGS/driver.log"
done
echo "$(date '+%F %T') DONE stages=[$stages]" >>"$LOGS/driver.log"
