# periodic-server

Periodic task system haskell server.

## Persist hook metric queue tuning

When running `periodicd --hook persist`, metric writes are buffered by an async
bounded queue to avoid blocking the scheduler path.

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
