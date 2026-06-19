#!/usr/bin/env bash
set -euo pipefail

TOTAL_JOBS="${TOTAL_JOBS:-400}"
CONCURRENCY="${CONCURRENCY:-10}"
WORKER_THREADS="${WORKER_THREADS:-4}"
FUNC_NAME="${FUNC_NAME:-bench-e2e-echo}"
BACKEND_URI="${BACKEND_URI:-file:///private/tmp/periodic-e2e-bench.sqlite}"
SOCKET_URI="${SOCKET_URI:-unix:///tmp/periodic.sock}"
WARMUP_JOBS="${WARMUP_JOBS:-5}"

export PERIODIC_PORT="$SOCKET_URI"

BIN_DIR="$(stack path --local-install-root)/bin"
PERIODIC_BIN="${BIN_DIR}/periodic"
PERIODICD_BIN="${BIN_DIR}/periodicd"
PERIODIC_RUN_BIN="${BIN_DIR}/periodic-run"

for b in "$PERIODIC_BIN" "$PERIODICD_BIN" "$PERIODIC_RUN_BIN"; do
  if [ ! -x "$b" ]; then
    echo "missing binary: $b"
    echo "run: stack build periodic-client-exe periodic-server"
    exit 1
  fi
done

workdir="$(mktemp -d /private/tmp/periodic-e2e-bench.XXXXXX)"
lat_file="$workdir/latency_ms.txt"
fail_file="$workdir/failures.txt"
server_log="$workdir/periodicd.log"
worker_log="$workdir/periodic-run.log"

server_pid=""
worker_pid=""

now_ms() {
  perl -MTime::HiRes=time -e 'printf "%.0f\n", time() * 1000'
}

stop_process() {
  local pid="$1"
  local grace="${2:-5}"
  if [ -z "$pid" ]; then
    return 0
  fi
  kill "$pid" 2>/dev/null || true
  for _ in $(seq 1 "$grace"); do
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

cleanup() {
  stop_process "$worker_pid" 3
  stop_process "$server_pid" 5
  pkill -f "periodicd" 2>/dev/null || true
  pkill -f "periodic-run" 2>/dev/null || true
  rm -f /tmp/periodic.sock
}
trap cleanup EXIT

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

run_until_result() {
  local name="$1"
  local attempts="${2:-60}"
  local out=""
  for _ in $(seq 1 "$attempts"); do
    out="$("$PERIODIC_BIN" run "$FUNC_NAME" "$name" 2>/dev/null || true)"
    if echo "$out" | grep -F "Result: ${name}" >/dev/null 2>&1; then
      echo "$out"
      return 0
    fi
    sleep 0.1
  done
  echo "$out"
  return 1
}

if [[ "$BACKEND_URI" == file://* ]]; then
  rm -f "${BACKEND_URI#file://}"
fi
rm -f /tmp/periodic.sock

PERIODIC_PATH="$BACKEND_URI" "$PERIODICD_BIN" >"$server_log" 2>&1 &
server_pid="$!"
wait_for_ping 90

"$PERIODIC_RUN_BIN" --data --thread "$WORKER_THREADS" "$FUNC_NAME" echo >"$worker_log" 2>&1 &
worker_pid="$!"

for i in $(seq 1 "$WARMUP_JOBS"); do
  run_until_result "warmup-$i" >/dev/null
  sleep 0.05
done

: >"$lat_file"
: >"$fail_file"

export PERIODIC_BIN FUNC_NAME lat_file fail_file

batch_start_ms="$(now_ms)"
seq 1 "$TOTAL_JOBS" | xargs -P "$CONCURRENCY" -I{} bash -lc '
  id="$1"
  start_ms=$(perl -MTime::HiRes=time -e "printf \"%.0f\", time()*1000")
  name="bench-${id}"
  out="$($PERIODIC_BIN run "$FUNC_NAME" "$name" 2>/dev/null || true)"
  end_ms=$(perl -MTime::HiRes=time -e "printf \"%.0f\", time()*1000")
  latency_ms=$((end_ms - start_ms))

  if echo "$out" | grep -F "Result: ${name}" >/dev/null 2>&1; then
    echo "$latency_ms" >> "$lat_file"
  else
    echo "${name}|${latency_ms}|${out}" >> "$fail_file"
  fi
' _ {}
batch_end_ms="$(now_ms)"

wall_ms=$((batch_end_ms - batch_start_ms))
wall_sec=$(awk -v ms="$wall_ms" 'BEGIN { printf "%.6f", ms / 1000.0 }')

ok_count=$(wc -l < "$lat_file" | tr -d ' ')
fail_count=$(wc -l < "$fail_file" | tr -d ' ')

throughput=$(awk -v n="$ok_count" -v s="$wall_sec" 'BEGIN { if (s > 0) printf "%.2f", n / s; else print "0.00" }')

if [ "$ok_count" -gt 0 ]; then
  sorted_file="$workdir/latency_sorted.txt"
  sort -n "$lat_file" > "$sorted_file"

  p50_idx=$(( (ok_count * 50 + 99) / 100 ))
  p95_idx=$(( (ok_count * 95 + 99) / 100 ))
  p99_idx=$(( (ok_count * 99 + 99) / 100 ))

  p50=$(awk -v idx="$p50_idx" 'NR == idx { print; exit }' "$sorted_file")
  p95=$(awk -v idx="$p95_idx" 'NR == idx { print; exit }' "$sorted_file")
  p99=$(awk -v idx="$p99_idx" 'NR == idx { print; exit }' "$sorted_file")
  mean=$(awk '{ s += $1 } END { if (NR > 0) printf "%.2f", s / NR; else print "0.00" }' "$lat_file")
  min=$(awk 'NR == 1 { m = $1 } $1 < m { m = $1 } END { print m }' "$lat_file")
  max=$(awk 'NR == 1 { m = $1 } $1 > m { m = $1 } END { print m }' "$lat_file")
else
  p50=0
  p95=0
  p99=0
  mean=0
  min=0
  max=0
fi

echo "== Periodic E2E Throughput/Latency Benchmark =="
echo "backend        : $BACKEND_URI"
echo "jobs           : $TOTAL_JOBS"
echo "concurrency    : $CONCURRENCY"
echo "worker_threads : $WORKER_THREADS"
echo "wall_time_s    : $wall_sec"
echo "success        : $ok_count"
echo "failed         : $fail_count"
echo "throughput_rps : $throughput"
echo "latency_ms min/mean/p50/p95/p99/max : ${min}/${mean}/${p50}/${p95}/${p99}/${max}"
echo "logs_dir       : $workdir"

if [ "$fail_count" -gt 0 ]; then
  echo "sample failures:"
  head -n 5 "$fail_file" || true
fi
