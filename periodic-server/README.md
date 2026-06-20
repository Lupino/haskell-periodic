# periodic-server

Periodic task system haskell server.

## Persist hook metric queue tuning

When running `periodicd --hook persist`, metric writes are buffered by an async
bounded queue to avoid blocking the scheduler path. SQLite-backed stores write
drained metric batches in a transaction and enable WAL mode, `synchronous=NORMAL`,
`busy_timeout=5000`, and in-memory temp storage on connection setup.

Environment variables:

- `PERIODIC_METRIC_QUEUE_MAX_SIZE`:
  Queue capacity for pending metric events.
  Default: `10000`.
- `PERIODIC_METRIC_DROP_LOG_EVERY`:
  Log once every N dropped metric events when queue is full.
  Default: `1000`.

Example:

```bash
PERIODIC_METRIC_QUEUE_MAX_SIZE=20000 \
PERIODIC_METRIC_DROP_LOG_EVERY=200 \
periodicd --hook persist
```

These SQLite settings improve the single-daemon path. They do not make sharing
one SQLite file across multiple `periodicd` processes safe; use PostgreSQL when
running more than one server process against the same persistent store.

## Socket hook queue tuning

When running `periodicd --hook udp://...` or `periodicd --hook tcp://...`,
socket hook events are also written through an async bounded queue. This keeps
slow hook receivers from blocking the scheduler path.

Environment variables:

- `PERIODIC_SOCKET_HOOK_QUEUE_MAX_SIZE`:
  Queue capacity for pending socket hook events.
  Default: `10000`.
- `PERIODIC_SOCKET_HOOK_DROP_LOG_EVERY`:
  Log once every N dropped socket hook events when queue is full.
  Default: `1000`.
