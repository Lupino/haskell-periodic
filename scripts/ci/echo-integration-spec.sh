#!/usr/bin/env bash
set -euo pipefail

run_suite() {
  backend_uri="$1"
  tag="$2"
  db_file="$3"

  rm -f /tmp/periodic.sock
  if [ -n "$db_file" ]; then
    rm -f "$db_file"
  fi

  cat > "echo-pipe-${tag}.sh" <<EOF
#!/usr/bin/env bash
echo ready
while read -r msgid arg; do
  echo "\${arg}" >> pipe-events-${tag}.log
  echo "\${msgid} \${arg}"
done
EOF
  chmod +x "echo-pipe-${tag}.sh"
  : > "pipe-events-${tag}.log"

  PERIODIC_PATH="$backend_uri" stack exec periodicd > "periodicd-${tag}.log" 2>&1 &
  SERVER_PID=$!
  WORKER_PID=""

  stop_worker() {
    if [ -n "$WORKER_PID" ]; then
      kill "$WORKER_PID" 2>/dev/null || true
      wait "$WORKER_PID" 2>/dev/null || true
      WORKER_PID=""
    fi
  }

  cleanup() {
    stop_worker
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
    rm -f /tmp/periodic.sock
  }
  trap cleanup RETURN

  for i in $(seq 1 20); do
    if stack exec periodic -- ping >/dev/null 2>&1; then
      break
    fi
    sleep 1
  done
  stack exec periodic -- ping >/dev/null

  # Test 1: run action with periodic-run --data
  stack exec periodic-run -- --data "echo-run-func-${tag}" echo > "periodic-run-${tag}.log" 2>&1 &
  WORKER_PID=$!
  for i in $(seq 1 20); do
    if grep -F "Worker started." "periodic-run-${tag}.log" >/dev/null 2>&1; then
      break
    fi
    sleep 1
  done

  OUTPUT="$(stack exec periodic -- run "echo-run-func-${tag}" echo)"
  echo "$OUTPUT"
  echo "$OUTPUT" | grep -F "Result: echo"
  stop_worker

  # Test 2: submit action with periodic-run
  stack exec periodic-run -- "echo-submit-func-${tag}" echo > "periodic-run-${tag}.log" 2>&1 &
  WORKER_PID=$!
  for i in $(seq 1 20); do
    if grep -F "Worker started." "periodic-run-${tag}.log" >/dev/null 2>&1; then
      break
    fi
    sleep 1
  done

  stack exec periodic -- submit "echo-submit-func-${tag}" "echo-submit-${tag}"

  for i in $(seq 1 20); do
    if grep -Fx "echo-submit-${tag}" "periodic-run-${tag}.log" >/dev/null 2>&1; then
      break
    fi
    sleep 1
  done
  grep -Fx "echo-submit-${tag}" "periodic-run-${tag}.log"
  stop_worker

  # Test 3: run + submit actions with periodic-run-pipe
  stack exec periodic-run-pipe -- "echo-pipe-func-${tag}" "./echo-pipe-${tag}.sh" > "periodic-run-pipe-${tag}.log" 2>&1 &
  WORKER_PID=$!

  for i in $(seq 1 20); do
    if grep -F "Pipe Worker started." "periodic-run-pipe-${tag}.log" >/dev/null 2>&1; then
      break
    fi
    sleep 1
  done

  OUTPUT="$(stack exec periodic -- run "echo-pipe-func-${tag}" "echo-pipe-run-${tag}")"
  echo "$OUTPUT"
  echo "$OUTPUT" | grep -F "Result: echo-pipe-run-${tag}"

  stack exec periodic -- submit "echo-pipe-func-${tag}" "echo-pipe-submit-${tag}"

  for i in $(seq 1 20); do
    if grep -Fx "echo-pipe-submit-${tag}" "pipe-events-${tag}.log" >/dev/null 2>&1; then
      break
    fi
    sleep 1
  done

  grep -Fx "echo-pipe-submit-${tag}" "pipe-events-${tag}.log"

  trap - RETURN
  cleanup
}

run_rsa_modes_suite() {
  rm -f /tmp/periodic.sock
  rm -f periodic-rsa-test_*.pem

  stack exec periodic -- keygen periodic-rsa-test
  RSA_PRIVATE_KEY="periodic-rsa-test_private_key.pem"
  RSA_PUBLIC_KEY="periodic-rsa-test_public_key.pem"

  stack exec periodicd -- \
    --rsa-private-path "$RSA_PRIVATE_KEY" \
    --rsa-public-path "$RSA_PUBLIC_KEY" \
    > periodicd-rsa.log 2>&1 &
  SERVER_PID=$!
  WORKER_PID=""

  stop_worker() {
    if [ -n "$WORKER_PID" ]; then
      kill "$WORKER_PID" 2>/dev/null || true
      wait "$WORKER_PID" 2>/dev/null || true
      WORKER_PID=""
    fi
  }

  cleanup() {
    stop_worker
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
    rm -f /tmp/periodic.sock
  }
  trap cleanup RETURN

  for i in $(seq 1 20); do
    if stack exec periodic -- \
      --rsa-mode AES \
      --rsa-private-path "$RSA_PRIVATE_KEY" \
      --rsa-public-path "$RSA_PUBLIC_KEY" \
      ping >/dev/null 2>&1; then
      break
    fi
    sleep 1
  done
  stack exec periodic -- \
    --rsa-mode AES \
    --rsa-private-path "$RSA_PRIVATE_KEY" \
    --rsa-public-path "$RSA_PUBLIC_KEY" \
    ping >/dev/null

  for mode in Plain RSA AES; do
    mode_tag="$(echo "$mode" | tr '[:upper:]' '[:lower:]')"
    log_file="periodic-run-rsa-${mode_tag}.log"
    func_name="echo-rsa-func-${mode_tag}"
    job_name="echo-rsa-job-${mode_tag}"

    stack exec periodic-run -- \
      --data \
      --rsa-mode "$mode" \
      --rsa-private-path "$RSA_PRIVATE_KEY" \
      --rsa-public-path "$RSA_PUBLIC_KEY" \
      "$func_name" echo > "$log_file" 2>&1 &
    WORKER_PID=$!

    for i in $(seq 1 20); do
      if grep -F "Worker started." "$log_file" >/dev/null 2>&1; then
        break
      fi
      sleep 1
    done

    OUTPUT="$(stack exec periodic -- \
      --rsa-mode "$mode" \
      --rsa-private-path "$RSA_PRIVATE_KEY" \
      --rsa-public-path "$RSA_PUBLIC_KEY" \
      run "$func_name" "$job_name")"
    echo "$OUTPUT"
    echo "$OUTPUT" | grep -F "Result: ${job_name}"

    stop_worker
  done

  trap - RETURN
  cleanup
}

run_suite ":memory:" "memory" "periodic-memory.sqlite"
run_suite "file://periodic-sqlite.sqlite" "sqlite" "periodic-sqlite.sqlite"

if [ -n "${PERIODIC_TEST_POSTGRES_URI:-}" ]; then
  run_suite "$PERIODIC_TEST_POSTGRES_URI" "postgres" ""
else
  echo "Skipping postgres suite: PERIODIC_TEST_POSTGRES_URI is not set"
fi

run_rsa_modes_suite
