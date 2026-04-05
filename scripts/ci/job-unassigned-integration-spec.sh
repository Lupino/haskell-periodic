#!/usr/bin/env bash
set -euo pipefail

PERIODIC_PORT="unix:///tmp/periodic.sock"
export PERIODIC_PORT
BIN_DIR="$(stack path --local-install-root)/bin"
PERIODIC_BIN="${BIN_DIR}/periodic"
PERIODICD_BIN="${BIN_DIR}/periodicd"
PERIODIC_RUN_BIN="${BIN_DIR}/periodic-run"

wait_for_ping() {
  local attempts="${1:-60}"
  for _ in $(seq 1 "$attempts"); do
    if "$PERIODIC_BIN" ping >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  "$PERIODIC_BIN" ping >/dev/null
}

stop_process() {
  local pid="$1"
  local grace_seconds="${2:-5}"
  if [ -z "${pid:-}" ]; then
    return 0
  fi

  kill "$pid" 2>/dev/null || true
  for _ in $(seq 1 "$grace_seconds"); do
    if ! kill -0 "$pid" 2>/dev/null; then
      break
    fi
    sleep 1
  done
  if kill -0 "$pid" 2>/dev/null; then
    kill -9 "$pid" 2>/dev/null || true
  fi
  wait "$pid" 2>/dev/null || true
}

wait_for_run_result_contains() {
  local func_name="$1"
  local job_name="$2"
  local expected="$3"
  local attempts="${4:-60}"
  for _ in $(seq 1 "$attempts"); do
    local output
    output="$("$PERIODIC_BIN" run "$func_name" "$job_name" 2>/dev/null || true)"
    if echo "$output" | grep -F "Result: ${expected}" >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  return 1
}

wait_for_log_contains() {
  local logfile="$1"
  local pattern="$2"
  local attempts="${3:-60}"
  for _ in $(seq 1 "$attempts"); do
    if grep -F "$pattern" "$logfile" >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  return 1
}

wait_for_process_exit() {
  local pid="$1"
  local attempts="${2:-15}"
  for _ in $(seq 1 "$attempts"); do
    if ! kill -0 "$pid" 2>/dev/null; then
      return 0
    fi
    sleep 1
  done
  return 1
}

assert_contains() {
  local output="$1"
  local expected="$2"
  if ! echo "$output" | grep -F "Result: ${expected}" >/dev/null 2>&1; then
    echo "Assertion failed: expected output to contain 'Result: ${expected}'"
    echo "Actual output:"
    echo "$output"
    return 1
  fi
}

assert_not_in_file() {
  local filepath="$1"
  local expected="$2"
  if grep -F "$expected" "$filepath" >/dev/null 2>&1; then
    echo "Assertion failed: expected file to not contain '${expected}'"
    echo "Actual file: ${filepath}"
    tail -n 200 "$filepath" || true
    return 1
  fi
}

main() {
  local tag="job-unassigned-$(date +%s)"
  local backend_uri="file://periodic-job-unassigned.sqlite"
  local server_log="periodicd-${tag}.log"
  local slow_log="periodic-run-slow-${tag}.log"
  local fast_log="periodic-run-fast-${tag}.log"
  local func_name="job-unassigned-func-${tag}"
  local warmup_job="warmup-${tag}"
  local blocking_job="blocking-${tag}"
  local overflow_job="overflow-${tag}"

  local server_pid=""
  local slow_worker_pid=""
  local fast_worker_pid=""
  local blocking_pid=""

  cleanup() {
    stop_process "${blocking_pid:-}" 1
    stop_process "${fast_worker_pid:-}" 3
    stop_process "${slow_worker_pid:-}" 3
    stop_process "${server_pid:-}" 5
    pkill -f "periodicd" 2>/dev/null || true
    pkill -f "periodic-run" 2>/dev/null || true
    rm -f /tmp/periodic.sock
  }
  trap cleanup EXIT

  rm -f /tmp/periodic.sock periodic-job-unassigned.sqlite
  PERIODIC_PATH="$backend_uri" "$PERIODICD_BIN" > "$server_log" 2>&1 &
  server_pid=$!
  wait_for_ping 60

  "$PERIODIC_RUN_BIN" --data --thread 1 "$func_name" bash -lc 'sleep 6; echo "slow:$1"' _ > "$slow_log" 2>&1 &
  slow_worker_pid=$!

  if ! wait_for_run_result_contains "$func_name" "$warmup_job" "slow:${warmup_job}" 60; then
    echo "Warmup on slow worker failed"
    tail -n 200 "$slow_log" || true
    tail -n 200 "$server_log" || true
    return 1
  fi

  "$PERIODIC_BIN" run "$func_name" "$blocking_job" > "blocking-${tag}.out" 2>&1 &
  blocking_pid=$!
  sleep 1

  # Request graceful shutdown while a blocking job is running.
  kill -TERM "$slow_worker_pid"
  sleep 1
  if ! kill -0 "$slow_worker_pid" 2>/dev/null; then
    echo "Slow worker exited before finishing in-flight blocking job"
    tail -n 200 "$slow_log" || true
    return 1
  fi

  "$PERIODIC_RUN_BIN" --data --thread 1 "$func_name" bash -lc 'echo "fast:$1"' _ > "$fast_log" 2>&1 &
  fast_worker_pid=$!

  local t0 t1 elapsed output
  t0="$(date +%s)"
  output="$("$PERIODIC_BIN" run "$func_name" "$overflow_job")"
  t1="$(date +%s)"
  elapsed=$((t1 - t0))

  echo "$output"
  assert_contains "$output" "fast:${overflow_job}"

  if [ "$elapsed" -ge 5 ]; then
    echo "Expected overflow job to be reassigned quickly, but took ${elapsed}s"
    tail -n 200 "$slow_log" || true
    tail -n 200 "$fast_log" || true
    tail -n 200 "$server_log" || true
    return 1
  fi

  wait "$blocking_pid"
  blocking_pid=""

  if ! wait_for_process_exit "$slow_worker_pid" 15; then
    echo "Slow worker did not exit after draining in-flight job"
    tail -n 200 "$slow_log" || true
    return 1
  fi
  slow_worker_pid=""

  if ! wait_for_log_contains "$slow_log" "Got shutdown signal." 20; then
    echo "Expected shutdown log not found in slow worker log"
    tail -n 200 "$slow_log" || true
    return 1
  fi

  assert_not_in_file "$slow_log" "Worker disconnected, retrying in 3s."

  echo "job-unassigned integration test passed in ${elapsed}s"

  trap - EXIT
  cleanup
}

main "$@"
