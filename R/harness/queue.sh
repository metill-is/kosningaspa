#!/usr/bin/env zsh
# The two-lane queue of harness fits. Run from anywhere:
#   zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>
#
# The run list holds one run per line, "lane arm E F seed" separated by whitespace, the lane
# A or B; lines starting with # are comments. The runs of each lane go one after another, the
# two lanes side by side, each run as
#   LC_ALL=en_US.UTF-8 Rscript R/harness/fit_cell.R <arm> <E> <F> <seed> <iter_warmup> <iter_sampling>
# from the project root, its output in $LOGS/<id>.log. LOGS is
# ${HARNESS_RESULTS:-<project root>/results/harness}/logs, as the runner's results_root().
#
# Before any fit it refuses to start while a fit_cell.R runs, unless FORCE=1, and runs the
# full slow test suite as a preflight ($LOGS/preflight.log), stopping if that fails.
# $LOGS/queue.log gets "QUEUE START ...", "preflight PASS" (or "preflight FAIL ..."), a
# "start <lane> <id> <time>" and an "end <id> rc=<rc> <first DIAG|SKIP|STALE line>" per run,
# and "QUEUE FINISHED". DRY_RUN=1 prints "would run <lane> <arm> <E> <F> <seed>" per run and
# does nothing else: no guard, preflight, log or fit.
#
# Exit status: 0 when every run exits 0; 1 when a run, the guard or the preflight fails; 2 on
# a bad command line or run list.
emulate -R zsh
setopt no_unset

if (( $# != 3 )); then
  print -u2 -r -- "usage: zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>"
  exit 2
fi
list=${1:A}
warmup=$2
sampling=$3
root=${0:A:h:h:h}
if [[ ! -f $list || ! -r $list ]]; then
  print -u2 -r -- "no run list: $1"
  exit 2
fi
if [[ $warmup != <-> || $sampling != <-> ]]; then
  print -u2 -r -- "iter_warmup and iter_sampling must be whole numbers: $warmup $sampling"
  exit 2
fi

typeset -a runs fields
n=0
while IFS= read -r line || [[ -n $line ]]; do
  (( n += 1 ))
  fields=(${=line})
  (( ${#fields} == 0 )) && continue
  [[ $fields[1] == \#* ]] && continue
  if (( ${#fields} != 5 )); then
    print -u2 -r -- "$list:$n: a run has 5 fields (lane arm E F seed): $line"
    exit 2
  fi
  if [[ $fields[1] != (A|B) ]]; then
    print -u2 -r -- "$list:$n: lane must be A or B: $line"
    exit 2
  fi
  if [[ $fields[5] != <-> ]]; then
    print -u2 -r -- "$list:$n: seed must be a whole number: $line"
    exit 2
  fi
  runs+=("${(j: :)fields}")
done < $list
if (( ${#runs} == 0 )); then
  print -u2 -r -- "no runs in $list"
  exit 2
fi

if [[ ${DRY_RUN:-0} == 1 ]]; then
  for r in $runs; do
    print -r -- "would run $r"
  done
  exit 0
fi

if [[ ${FORCE:-0} != 1 ]] && pgrep -f -- '--file=R/harness/fit_cell.R' > /dev/null; then
  print -u2 -r -- "a fit_cell.R is running (pgrep -f -- '--file=R/harness/fit_cell.R'); FORCE=1 starts anyway"
  exit 1
fi

cd $root || exit 1
LOGS=${HARNESS_RESULTS:-$root/results/harness}/logs
mkdir -p $LOGS || exit 1
qlog() { print -r -- "$*" >> $LOGS/queue.log }
now() { date '+%Y-%m-%d %H:%M:%S' }

qlog "QUEUE START $(now) $list $warmup $sampling"
HARNESS_SLOW=1 LC_ALL=en_US.UTF-8 Rscript tests/run.R > $LOGS/preflight.log 2>&1
rc=$?
if (( rc != 0 )); then
  qlog "preflight FAIL rc=$rc ($LOGS/preflight.log)"
  print -u2 -r -- "preflight failed (rc=$rc): see $LOGS/preflight.log"
  exit 1
fi
qlog "preflight PASS"

run_lane() {
  local lane=$1 failed=0 r id rc first
  local -a f
  for r in $runs; do
    f=(${=r})
    [[ $f[1] == $lane ]] || continue
    id="${f[2]}_${f[3]}_${f[4]}_s$(( f[5] ))"
    qlog "start $lane $id $(now)"
    LC_ALL=en_US.UTF-8 Rscript R/harness/fit_cell.R $f[2] $f[3] $f[4] $f[5] $warmup $sampling > $LOGS/$id.log 2>&1
    rc=$?
    first=$(command grep -m1 -E '^(DIAG|SKIP|STALE)' $LOGS/$id.log)
    qlog "end $id rc=$rc${first:+ $first}"
    (( rc == 0 )) || failed=1
  done
  return $failed
}

run_lane A &
lane_a=$!
run_lane B &
lane_b=$!
wait $lane_a
rc_a=$?
wait $lane_b
rc_b=$?
qlog "QUEUE FINISHED"
(( rc_a == 0 && rc_b == 0 ))
