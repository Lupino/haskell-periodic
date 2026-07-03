#!/usr/bin/env bash
set -euo pipefail

wait_for_server() {
  for _ in $(seq 1 20); do
    if stack exec periodic -- \
      --client-name client-a \
      --client-token token-a \
      ping >/dev/null 2>&1; then
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
admin_func="auth-admin-${tag}"
prefix_base="auth-prefix-${tag}"
prefix_func="${prefix_base}-allowed"
suffix_tail="-suffix-${tag}"
suffix_func="auth-allowed${suffix_tail}"

rm -f /tmp/periodic.sock "$auth_file"

cat > "$auth_file" <<EOF
client client-a token-a ${allowed_func}
worker worker-a token-worker-a ${allowed_func}
worker worker-b token-worker-b ${denied_func}
client client-prefix token-prefix ${prefix_base}*
worker worker-prefix token-worker-prefix ${prefix_base}*
client client-suffix token-suffix *${suffix_tail}
worker worker-suffix token-worker-suffix *${suffix_tail}
admin admin-a token-admin ${admin_func}
EOF

SERVER_PID=""
WRONG_ROLE_PID=""
WORKER_A_PID=""
WORKER_B_PID=""
PREFIX_WORKER_PID=""
SUFFIX_WORKER_PID=""
ADMIN_WORKER_PID=""

cleanup() {
  for pid in "${WRONG_ROLE_PID:-}" "${WORKER_A_PID:-}" "${WORKER_B_PID:-}" "${PREFIX_WORKER_PID:-}" "${SUFFIX_WORKER_PID:-}" "${ADMIN_WORKER_PID:-}" "${SERVER_PID:-}"; do
    if [ -n "$pid" ]; then
      kill "$pid" 2>/dev/null || true
      wait "$pid" 2>/dev/null || true
    fi
  done
  pkill -f "periodicd.*${auth_file}" 2>/dev/null || true
  rm -f /tmp/periodic.sock "$auth_file"
  rm -f "periodicd-${tag}.log" "periodic-run-a-${tag}.log" "periodic-run-b-${tag}.log"
  rm -f "periodic-run-prefix-${tag}.log" "periodic-run-suffix-${tag}.log"
  rm -f "periodic-run-admin-${tag}.log"
  rm -f "periodic-run-wrong-role-${tag}.log"
}
trap cleanup EXIT

stack exec periodicd -- --path :memory: --auth-file "$auth_file" > "periodicd-${tag}.log" 2>&1 &
SERVER_PID=$!

wait_for_server

stack exec periodic-run -- \
  --client-name client-a \
  --client-token token-a \
  --data "$allowed_func" echo > "periodic-run-wrong-role-${tag}.log" 2>&1 &
WRONG_ROLE_PID=$!
sleep 3
if ! grep -F "Rejected unauthorized role" "periodicd-${tag}.log" >/dev/null 2>&1; then
  echo "Expected client-a to be rejected when registering as worker"
  exit 1
fi
kill "$WRONG_ROLE_PID" 2>/dev/null || true
wait "$WRONG_ROLE_PID" 2>/dev/null || true
WRONG_ROLE_PID=""

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
sleep 2

stack exec periodic-run -- \
  --client-name worker-prefix \
  --client-token token-worker-prefix \
  --data "$prefix_func" echo > "periodic-run-prefix-${tag}.log" 2>&1 &
PREFIX_WORKER_PID=$!

wait_for_run_result_auth client-prefix token-prefix "$prefix_func" "prefix-${tag}" 20

stack exec periodic-run -- \
  --client-name worker-suffix \
  --client-token token-worker-suffix \
  --data "$suffix_func" echo > "periodic-run-suffix-${tag}.log" 2>&1 &
SUFFIX_WORKER_PID=$!

wait_for_run_result_auth client-suffix token-suffix "$suffix_func" "suffix-${tag}" 20

stack exec periodic-run -- \
  --client-name admin-a \
  --client-token token-admin \
  --data "$admin_func" echo > "periodic-run-admin-${tag}.log" 2>&1 &
ADMIN_WORKER_PID=$!

wait_for_run_result_auth admin-a token-admin "$admin_func" "admin-worker-${tag}" 20

status_output="$(stack exec periodic -- \
  --client-name client-a \
  --client-token token-a \
  status 2>/dev/null || true)"
if ! echo "$status_output" | grep -F "$allowed_func" >/dev/null 2>&1; then
  echo "Expected client-a status to include ${allowed_func}"
  exit 1
fi
if echo "$status_output" | grep -F "$denied_func" >/dev/null 2>&1; then
  echo "Expected client-a status to hide ${denied_func}"
  exit 1
fi

prefix_status_output="$(stack exec periodic -- \
  --client-name client-prefix \
  --client-token token-prefix \
  status 2>/dev/null || true)"
if ! echo "$prefix_status_output" | grep -F "$prefix_func" >/dev/null 2>&1; then
  echo "Expected client-prefix status to include ${prefix_func}"
  exit 1
fi
if echo "$prefix_status_output" | grep -F "$denied_func" >/dev/null 2>&1; then
  echo "Expected client-prefix status to hide ${denied_func}"
  exit 1
fi

suffix_status_output="$(stack exec periodic -- \
  --client-name client-suffix \
  --client-token token-suffix \
  status 2>/dev/null || true)"
if ! echo "$suffix_status_output" | grep -F "$suffix_func" >/dev/null 2>&1; then
  echo "Expected client-suffix status to include ${suffix_func}"
  exit 1
fi
if echo "$suffix_status_output" | grep -F "$denied_func" >/dev/null 2>&1; then
  echo "Expected client-suffix status to hide ${denied_func}"
  exit 1
fi

admin_status_output="$(stack exec periodic -- \
  --client-name admin-a \
  --client-token token-admin \
  status 2>/dev/null || true)"
if ! echo "$admin_status_output" | grep -F "$allowed_func" >/dev/null 2>&1; then
  echo "Expected admin-a status to include ${allowed_func}"
  exit 1
fi
if ! echo "$admin_status_output" | grep -F "$denied_func" >/dev/null 2>&1; then
  echo "Expected admin-a status to include ${denied_func}"
  exit 1
fi

if stack exec periodic -- \
  --client-name client-a \
  --client-token token-a \
  run "$denied_func" "denied-${tag}" | grep -F "Result:"; then
  echo "Expected client-a to be rejected for ${denied_func}"
  exit 1
fi

if stack exec periodic -- \
  --client-name client-prefix \
  --client-token token-prefix \
  run "$denied_func" "prefix-denied-${tag}" | grep -F "Result:"; then
  echo "Expected client-prefix to be rejected for ${denied_func}"
  exit 1
fi

if stack exec periodic -- \
  --client-name client-suffix \
  --client-token token-suffix \
  run "$denied_func" "suffix-denied-${tag}" | grep -F "Result:"; then
  echo "Expected client-suffix to be rejected for ${denied_func}"
  exit 1
fi

wait_for_run_result_auth admin-a token-admin "$denied_func" "admin-denied-func-${tag}" 20

stack exec periodic -- \
  --client-name client-a \
  --client-token token-a \
  drop "$denied_func" >/dev/null 2>&1 || true
sleep 1
if ! grep -F "Rejected unauthorized DropFunc" "periodicd-${tag}.log" >/dev/null 2>&1; then
  echo "Expected client-a drop to be rejected for ${denied_func}"
  exit 1
fi

stack exec periodic -- \
  --client-name client-a \
  --client-token token-a \
  remove "$denied_func" "denied-job-${tag}" >/dev/null 2>&1 || true
sleep 1
if ! grep -F "Rejected unauthorized RemoveJob" "periodicd-${tag}.log" >/dev/null 2>&1; then
  echo "Expected client-a remove to be rejected for ${denied_func}"
  exit 1
fi

cat >> "$auth_file" <<EOF
client client-b token-b ${denied_func}
EOF

wait_for_run_result_auth client-b token-b "$denied_func" "hot-reload-${tag}" 20

trap - EXIT
cleanup

echo "auth-func integration test passed"
