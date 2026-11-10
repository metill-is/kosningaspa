#!/usr/bin/env zsh
# The two-lane queue of harness fits. Run from anywhere:
#   zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>
#
# Run the fast suite by hand before a queue: LC_ALL=en_US.UTF-8 Rscript tests/run.R
#
# The run list holds one run per line, "lane arm E F seed" separated by whitespace, the lane
# A or B; lines starting with # are comments. The runs of each lane go one after another, the
# two lanes side by side, each run as
#   LC_ALL=en_US.UTF-8 Rscript R/harness/fit_cell.R <arm> <E> <F> <seed> <iter_warmup> <iter_sampling>
# from the project root, its output in $LOGS/<id>.log. LOGS is
# ${HARNESS_RESULTS:-<project root>/results/harness}/logs, as the runner's results_root().
#
# $LOGS/queue.log gets "QUEUE START <time> <list> <warmup> <sampling> (pid <pid>)", a
# "start <lane> <id> <time>" and an "end <id> rc=<rc> <first DIAG|SKIP|STALE line>" per run,
# and one terminal line: "QUEUE FINISHED" once both lanes are done, or "QUEUE ABORTED rc=<rc>
# <reason> (pid <pid>)" on any other exit (a bad command line, SIGTERM or SIGINT). On SIGTERM
# or SIGINT it stops both lanes, so no further run starts; a fit already running finishes on
# its own. DRY_RUN=1 prints "would run <lane> <arm> <E> <F> <seed>" per run and does nothing
# else: no log and no fit.
#
# Exit status: 0 when every run exits 0; 1 when a run fails; 2 on a bad command line or run
# list; 143 or 130 on SIGTERM or SIGINT.
emulate -R zsh
setopt no_unset

root=${0:A:h:h:h}
LOGS=${HARNESS_RESULTS:-$root/results/harness}/logs
finished=0
reason="exited"
typeset -a children

qlog() { print -r -- "$*" >> $LOGS/queue.log }
now() { date '+%Y-%m-%d %H:%M:%S' }

# The exit trap: stop the lanes and write the terminal line unless the queue finished or is a
# dry run.
on_exit() {
  local rc=$?
  if (( ${#children} )); then
    kill -TERM $children 2> /dev/null
  fi
  if (( ! finished )) && [[ ${DRY_RUN:-0} != 1 ]] && mkdir -p $LOGS 2> /dev/null; then
    qlog "QUEUE ABORTED rc=$rc $reason (pid $$)"
  fi
}
trap on_exit EXIT
trap 'reason="killed (TERM)"; exit 143' TERM
trap 'reason="interrupted (INT)"; exit 130' INT

die() {
  local rc=$1
  shift
  reason="$*"
  print -u2 -r -- "$*"
  exit $rc
}

(( $# == 3 )) || die 2 "usage: zsh R/harness/queue.sh <run_list> <iter_warmup> <iter_sampling>"
list=${1:A}
warmup=$2
sampling=$3
[[ -f $list && -r $list ]] || die 2 "no run list: $1"
if [[ $warmup != <-> || $sampling != <-> ]]; then
  die 2 "iter_warmup and iter_sampling must be whole numbers: $warmup $sampling"
fi

typeset -a runs fields
n=0
while IFS= read -r line || [[ -n $line ]]; do
  (( n += 1 ))
  fields=(${=line})
  (( ${#fields} == 0 )) && continue
  [[ $fields[1] == \#* ]] && continue
  (( ${#fields} == 5 )) || die 2 "$list:$n: a run has 5 fields (lane arm E F seed): $line"
  [[ $fields[1] == (A|B) ]] || die 2 "$list:$n: lane must be A or B: $line"
  [[ $fields[5] == <-> ]] || die 2 "$list:$n: seed must be a whole number: $line"
  runs+=("${(j: :)fields}")
done < $list
(( ${#runs} > 0 )) || die 2 "no runs in $list"

if [[ ${DRY_RUN:-0} == 1 ]]; then
  for r in $runs; do
    print -r -- "would run $r"
  done
  exit 0
fi

mkdir -p $LOGS || die 1 "cannot make $LOGS"
cd $root || die 1 "cannot cd to $root"
qlog "QUEUE START $(now) $list $warmup $sampling (pid $$)"

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

# WHY: the lanes run in the background so that a SIGTERM's trap runs at once rather than
# after the running fits.
run_lane A &
lane_a=$!
run_lane B &
lane_b=$!
children=($lane_a $lane_b)
wait $lane_a
rc_a=$?
wait $lane_b
rc_b=$?
children=()
qlog "QUEUE FINISHED"
finished=1
(( rc_a == 0 && rc_b == 0 ))
