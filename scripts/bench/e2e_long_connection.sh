#!/usr/bin/env bash
set -euo pipefail

# Isolated long-lived-client benchmark.  Unlike e2e_throughput_latency.sh,
# this measures server scheduling rather than shell/process startup overhead.
RUNS="${RUNS:-3}"
JOBS="${JOBS:-10000}"
CLIENTS="${CLIENTS:-16}"
WORKERS="${WORKERS:-16}"
PAYLOAD_BYTES="${PAYLOAD_BYTES:-128}"
BACKEND="${BACKEND:-memory}" # memory or sqlite
RUN_JOB_AGGRESSIVE="${RUN_JOB_AGGRESSIVE:-1}" # 0 or 1
ASSIGN_WAIT="${ASSIGN_WAIT:-10}" # seconds
MAX_POOL_SIZE="${MAX_POOL_SIZE:-32}" # concurrent session handlers per connection

case "$RUN_JOB_AGGRESSIVE" in
  0|1) ;;
  *) echo "RUN_JOB_AGGRESSIVE must be 0 or 1" >&2; exit 1 ;;
esac

case "$ASSIGN_WAIT" in
  ''|*[!0-9]*|0) echo "ASSIGN_WAIT must be a positive integer" >&2; exit 1 ;;
esac

case "$MAX_POOL_SIZE" in
  ''|*[!0-9]*|0) echo "MAX_POOL_SIZE must be a positive integer" >&2; exit 1 ;;
esac

BIN_DIR="$(stack path --local-install-root)/bin"
PERIODICD_BIN="${BIN_DIR}/periodicd"
BENCH_BIN="${BIN_DIR}/periodic-bench"
PERIODIC_BIN="${BIN_DIR}/periodic"
for binary in "$PERIODICD_BIN" "$BENCH_BIN" "$PERIODIC_BIN"; do
  if [ ! -x "$binary" ]; then
    echo "missing binary: $binary" >&2
    echo "run: stack build periodic-server periodic-client-exe" >&2
    exit 1
  fi
done

workdir="$(mktemp -d /private/tmp/periodic-long-bench.XXXXXX)"
socket=""
server_pid=""

stop_server() {
  if [ -n "$server_pid" ]; then
    "$PERIODIC_BIN" --host "$socket" shutdown >/dev/null 2>&1 || true
    for _ in $(seq 1 20); do
      if ! kill -0 "$server_pid" 2>/dev/null; then
        break
      fi
      sleep 0.1
    done
    if kill -0 "$server_pid" 2>/dev/null; then
      kill -9 "$server_pid" 2>/dev/null || true
    fi
    wait "$server_pid" 2>/dev/null || true
    server_pid=""
  fi
}

cleanup() {
  stop_server
  rm -rf "$workdir"
}
trap cleanup EXIT

start_server() {
  local run="$1"
  local backend_uri
  socket="unix://${workdir}/run-${run}.sock"
  case "$BACKEND" in
    memory) backend_uri=":memory:" ;;
    sqlite) backend_uri="file://${workdir}/run-${run}.sqlite" ;;
    *) echo "BACKEND must be memory or sqlite" >&2; exit 1 ;;
  esac

  "$PERIODICD_BIN" --host "$socket" --path "$backend_uri" >"${workdir}/periodicd-${run}.log" 2>&1 &
  server_pid="$!"
  for _ in $(seq 1 100); do
    if "$PERIODIC_BIN" --host "$socket" ping >/dev/null 2>&1; then
      break
    fi
    sleep 0.05
  done
  if ! kill -0 "$server_pid" 2>/dev/null; then
    cat "${workdir}/periodicd-${run}.log" >&2
    exit 1
  fi

  "$PERIODIC_BIN" --host "$socket" config set run-job-aggressive "$RUN_JOB_AGGRESSIVE"
  configured_aggressive="$("$PERIODIC_BIN" --host "$socket" config get run-job-aggressive)"
  if [ "$configured_aggressive" != "$RUN_JOB_AGGRESSIVE" ]; then
    echo "failed to configure run-job-aggressive: expected ${RUN_JOB_AGGRESSIVE}, got ${configured_aggressive}" >&2
    exit 1
  fi
  "$PERIODIC_BIN" --host "$socket" config set assign-wait "$ASSIGN_WAIT"
  configured_assign_wait="$("$PERIODIC_BIN" --host "$socket" config get assign-wait)"
  if [ "$configured_assign_wait" != "$ASSIGN_WAIT" ]; then
    echo "failed to configure assign-wait: expected ${ASSIGN_WAIT}, got ${configured_assign_wait}" >&2
    exit 1
  fi
  "$PERIODIC_BIN" --host "$socket" config set max-pool-size "$MAX_POOL_SIZE"
  configured_max_pool_size="$("$PERIODIC_BIN" --host "$socket" config get max-pool-size)"
  if [ "$configured_max_pool_size" != "$MAX_POOL_SIZE" ]; then
    echo "failed to configure max-pool-size: expected ${MAX_POOL_SIZE}, got ${configured_max_pool_size}" >&2
    exit 1
  fi
}

echo "backend=${BACKEND}"
echo "jobs=${JOBS} clients=${CLIENTS} workers=${WORKERS} payload_bytes=${PAYLOAD_BYTES}"
echo "run_job_aggressive=${RUN_JOB_AGGRESSIVE}"
echo "assign_wait=${ASSIGN_WAIT}"
echo "max_pool_size=${MAX_POOL_SIZE}"
for run in $(seq 1 "$RUNS"); do
  echo "run=${run}"
  echo "backend=${BACKEND}"
  echo "max_pool_size=${MAX_POOL_SIZE}"
  start_server "$run"
  if ! "$BENCH_BIN" --host "$socket" --jobs "$JOBS" --clients "$CLIENTS" --workers "$WORKERS" --payload-bytes "$PAYLOAD_BYTES" 2>>"${workdir}/periodic-bench.log"; then
    cat "${workdir}/periodic-bench.log" >&2
    cat "${workdir}/periodicd-${run}.log" >&2
    exit 1
  fi
  stop_server
done
