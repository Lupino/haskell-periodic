#!/usr/bin/env bash
set -euo pipefail

wait_for_ping() {
  local attempts="${1:-30}"
  for _ in $(seq 1 "$attempts"); do
    if stack exec periodic -- ping >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  stack exec periodic -- ping >/dev/null
}

wait_for_log() {
  local logfile="$1"
  local pattern="$2"
  local attempts="${3:-30}"
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

start_server() {
  local backend_uri="$1"
  local logfile="$2"
  rm -f /tmp/periodic.sock
  PERIODIC_PATH="$backend_uri" stack exec periodicd > "$logfile" 2>&1 &
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

  stack exec periodic-run -- --data "$func_name" echo > "$worker_log" 2>&1 &
  worker_pid=$!
  wait_for_log "$worker_log" "Worker started." 30

  kill "$server_pid" 2>/dev/null || true
  wait "$server_pid" 2>/dev/null || true

  server_pid="$(start_server "$backend_uri" "$server_log")"
  wait_for_ping 30

  wait_for_log "$worker_log" "Worker disconnected, retrying in 3s." 30

  output="$(stack exec periodic -- run "$func_name" "$input")"
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

  stack exec periodic-run-pipe -- "$func_name" "./$pipe_script" > "$worker_log" 2>&1 &
  worker_pid=$!
  wait_for_log "$worker_log" "Pipe Worker started." 30

  kill "$server_pid" 2>/dev/null || true
  wait "$server_pid" 2>/dev/null || true

  server_pid="$(start_server "$backend_uri" "$server_log")"
  wait_for_ping 30

  wait_for_log "$worker_log" "Worker disconnected, retrying in 3s." 30

  output="$(stack exec periodic -- run "$func_name" "$input")"
  echo "$output"
  echo "$output" | grep -F "Result: ${input}"

  trap - RETURN
  cleanup
}

test_reconnect_run "file://periodic-reconnect.sqlite" "sqlite"
test_reconnect_pipe "file://periodic-reconnect.sqlite" "sqlite"
