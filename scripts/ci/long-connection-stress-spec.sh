#!/usr/bin/env bash
set -euo pipefail

# Bounded concurrency regression for the synchronous RunJob path. The runner
# owns an isolated Unix socket and only terminates its direct periodicd child.
BACKEND="${BACKEND:-memory}"
JOBS="${JOBS:-1000}"
CLIENTS="${CLIENTS:-16}"
WORKERS="${WORKERS:-16}"
MAX_POOL_SIZE="${MAX_POOL_SIZE:-32}"
ASSIGN_WAIT="${ASSIGN_WAIT:-10}"

case "$BACKEND" in
  memory|sqlite) ;;
  *) echo "BACKEND must be memory or sqlite" >&2; exit 1 ;;
esac

output="$(BACKEND="$BACKEND" RUNS=1 JOBS="$JOBS" CLIENTS="$CLIENTS" \
  WORKERS="$WORKERS" MAX_POOL_SIZE="$MAX_POOL_SIZE" ASSIGN_WAIT="$ASSIGN_WAIT" \
  RUN_JOB_AGGRESSIVE=1 bash scripts/bench/e2e_long_connection.sh)"
printf '%s\n' "$output"

metric() {
  local key="$1"
  awk -F= -v key="$key" '$1 == key { value = $2 } END { print value }' <<<"$output"
}

[ "$(metric max_pool_size)" = "$MAX_POOL_SIZE" ] || {
  echo "max-pool-size was not applied" >&2
  exit 1
}
[ "$(metric succeeded)" = "$JOBS" ] || {
  echo "lost or mismatched RunJob responses" >&2
  exit 1
}
[ "$(metric failed)" = "0" ] || {
  echo "failed RunJob count is nonzero" >&2
  exit 1
}
