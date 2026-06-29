#!/usr/bin/env bash
set -euo pipefail

wait_for_server() {
  for _ in $(seq 1 20); do
    if stack exec periodic -- ping >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  return 1
}

wait_for_run_result_auth() {
  local client_name="$1"
  local client_token="$2"
  local func_name="$3"
  local input="$4"
  local attempts="${5:-20}"
  for _ in $(seq 1 "$attempts"); do
    output="$(stack exec periodic -- \
      --client-name "$client_name" \
      --client-token "$client_token" \
      run "$func_name" "$input" 2>/dev/null || true)"
    if echo "$output" | grep -F "Result: ${input}" >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  return 1
}

tag="auth-func-$(date +%s)"
auth_file="periodic-auth-${tag}.txt"
allowed_func="auth-allowed-${tag}"
denied_func="auth-denied-${tag}"

rm -f /tmp/periodic.sock "$auth_file"

cat > "$auth_file" <<EOF
client-a token-a ${allowed_func}
worker-a token-worker-a ${allowed_func}
worker-b token-worker-b ${denied_func}
EOF

SERVER_PID=""
WORKER_A_PID=""
WORKER_B_PID=""

cleanup() {
  for pid in "${WORKER_A_PID:-}" "${WORKER_B_PID:-}" "${SERVER_PID:-}"; do
    if [ -n "$pid" ]; then
      kill "$pid" 2>/dev/null || true
      wait "$pid" 2>/dev/null || true
    fi
  done
  pkill -f "periodicd.*${auth_file}" 2>/dev/null || true
  rm -f /tmp/periodic.sock "$auth_file"
  rm -f "periodicd-${tag}.log" "periodic-run-a-${tag}.log" "periodic-run-b-${tag}.log"
}
trap cleanup EXIT

stack exec periodicd -- --path :memory: --auth-file "$auth_file" > "periodicd-${tag}.log" 2>&1 &
SERVER_PID=$!

wait_for_server

stack exec periodic-run -- \
  --client-name worker-a \
  --client-token token-worker-a \
  --data "$allowed_func" echo > "periodic-run-a-${tag}.log" 2>&1 &
WORKER_A_PID=$!

wait_for_run_result_auth client-a token-a "$allowed_func" "warmup-${tag}" 20

stack exec periodic-run -- \
  --client-name worker-b \
  --client-token token-worker-b \
  --data "$denied_func" echo > "periodic-run-b-${tag}.log" 2>&1 &
WORKER_B_PID=$!

if stack exec periodic -- \
  --client-name client-a \
  --client-token token-a \
  run "$denied_func" "denied-${tag}" | grep -F "Result:"; then
  echo "Expected client-a to be rejected for ${denied_func}"
  exit 1
fi

cat >> "$auth_file" <<EOF
client-b token-b ${denied_func}
EOF

wait_for_run_result_auth client-b token-b "$denied_func" "hot-reload-${tag}" 20

trap - EXIT
cleanup

echo "auth-func integration test passed"
