#!/usr/bin/env bash
set -euo pipefail

PERIODIC_PORT="unix:///tmp/periodic.sock"
export PERIODIC_PORT
BIN_DIR="$(stack path --local-install-root)/bin"
PERIODIC_BIN="${BIN_DIR}/periodic"
PERIODICD_BIN="${BIN_DIR}/periodicd"
PERIODIC_RUN_BIN="${BIN_DIR}/periodic-run"
PERIODIC_RUN_PIPE_BIN="${BIN_DIR}/periodic-run-pipe"

wait_for_ping() {
  local attempts="${1:-90}"
  for _ in $(seq 1 "$attempts"); do
    if "$PERIODIC_BIN" ping >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  "$PERIODIC_BIN" ping >/dev/null
}

wait_for_log() {
  local logfile="$1"
  local pattern="$2"
  local attempts="${3:-90}"
  for _ in $(seq 1 "$attempts"); do
    if grep -F "$pattern" "$logfile" >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  echo "Missing log pattern: $pattern in $logfile"
  tail -n 200 "$logfile" || true
  return 1
}

wait_for_run_result() {
  local func_name="$1"
  local input="$2"
  local attempts="${3:-60}"
  for _ in $(seq 1 "$attempts"); do
    local output
    output="$("$PERIODIC_BIN" run "$func_name" "$input" 2>/dev/null || true)"
    if echo "$output" | grep -F "Result: ${input}" >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  return 1
}

start_server() {
  local backend_uri="$1"
  local logfile="$2"
  rm -f /tmp/periodic.sock
  PERIODIC_PATH="$backend_uri" "$PERIODICD_BIN" > "$logfile" 2>&1 &
  echo $!
}

test_reconnect_run() {
  local backend_uri="$1"
  local tag="$2"
  local db_file="periodic-reconnect.sqlite"

  local server_log="periodicd-reconnect-run-${tag}.log"
  local worker_log="periodic-run-reconnect-${tag}.log"
  local func_name="reconnect-run-func-${tag}"
  local input="reconnect-run-input-${tag}"

  local server_pid
  local worker_pid=""

  cleanup() {
    if [ -n "$worker_pid" ]; then
      kill "$worker_pid" 2>/dev/null || true
      wait "$worker_pid" 2>/dev/null || true
      worker_pid=""
    fi
    kill "$server_pid" 2>/dev/null || true
    wait "$server_pid" 2>/dev/null || true
    rm -f /tmp/periodic.sock
  }
  trap cleanup RETURN

  rm -f "$db_file"
  server_pid="$(start_server "$backend_uri" "$server_log")"
  wait_for_ping 30

  "$PERIODIC_RUN_BIN" --data "$func_name" echo > "$worker_log" 2>&1 &
  worker_pid=$!
  if ! wait_for_run_result "$func_name" "warmup-${tag}" 90; then
    echo "-- periodic-run readiness failure diagnostics --"
    tail -n 200 "$worker_log" || true
    tail -n 200 "$server_log" || true
    return 1
  fi

  kill "$server_pid" 2>/dev/null || true
  wait "$server_pid" 2>/dev/null || true

  server_pid="$(start_server "$backend_uri" "$server_log")"
  wait_for_ping 90

  wait_for_log "$worker_log" "Worker disconnected, retrying in 3s." 90
  if ! wait_for_run_result "$func_name" "post-restart-warmup-${tag}" 90; then
    echo "-- periodic-run reconnect readiness failure diagnostics --"
    tail -n 200 "$worker_log" || true
    tail -n 200 "$server_log" || true
    return 1
  fi

  output="$("$PERIODIC_BIN" run "$func_name" "$input")"
  echo "$output"
  echo "$output" | grep -F "Result: ${input}"

  trap - RETURN
  cleanup
}

test_reconnect_pipe() {
  local backend_uri="$1"
  local tag="$2"
  local db_file="periodic-reconnect.sqlite"

  local server_log="periodicd-reconnect-pipe-${tag}.log"
  local worker_log="periodic-run-pipe-reconnect-${tag}.log"
  local pipe_script="reconnect-pipe-${tag}.sh"
  local func_name="reconnect-pipe-func-${tag}"
  local input="reconnect-pipe-input-${tag}"

  local server_pid
  local worker_pid=""

  cat > "$pipe_script" <<EOF
#!/usr/bin/env bash
echo ready
while read -r msgid arg; do
  echo "\${msgid} \${arg}"
done
EOF
  chmod +x "$pipe_script"

  cleanup() {
    if [ -n "$worker_pid" ]; then
      kill "$worker_pid" 2>/dev/null || true
      wait "$worker_pid" 2>/dev/null || true
      worker_pid=""
    fi
    kill "$server_pid" 2>/dev/null || true
    wait "$server_pid" 2>/dev/null || true
    rm -f /tmp/periodic.sock "$pipe_script"
  }
  trap cleanup RETURN

  rm -f "$db_file"
  server_pid="$(start_server "$backend_uri" "$server_log")"
  wait_for_ping 30

  "$PERIODIC_RUN_PIPE_BIN" "$func_name" "./$pipe_script" > "$worker_log" 2>&1 &
  worker_pid=$!
  if ! wait_for_run_result "$func_name" "warmup-${tag}" 90; then
    echo "-- periodic-run-pipe readiness failure diagnostics --"
    tail -n 200 "$worker_log" || true
    tail -n 200 "$server_log" || true
    return 1
  fi

  kill "$server_pid" 2>/dev/null || true
  wait "$server_pid" 2>/dev/null || true

  server_pid="$(start_server "$backend_uri" "$server_log")"
  wait_for_ping 90

  wait_for_log "$worker_log" "Worker disconnected, retrying in 3s." 90
  if ! wait_for_run_result "$func_name" "post-restart-warmup-${tag}" 90; then
    echo "-- periodic-run-pipe reconnect readiness failure diagnostics --"
    tail -n 200 "$worker_log" || true
    tail -n 200 "$server_log" || true
    return 1
  fi

  output="$("$PERIODIC_BIN" run "$func_name" "$input")"
  echo "$output"
  echo "$output" | grep -F "Result: ${input}"

  trap - RETURN
  cleanup
}

test_reconnect_run "file://periodic-reconnect.sqlite" "sqlite"
test_reconnect_pipe "file://periodic-reconnect.sqlite" "sqlite"
