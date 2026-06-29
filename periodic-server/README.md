# periodic-server

Periodic task system haskell server.

## Client role and func authorization

Use `periodicd --auth-file <PATH>` to enable per-identity role and func
authorization. The file is whitespace-separated, with one identity per line:

```text
client client-a token-a func1,func2
worker worker-a token-worker-a func1
admin admin-a token-admin admin_marker
```

Clients and workers authenticate during connection registration:

```bash
periodic --client-name client-a --client-token token-a run func1 job-1
periodic-run --client-name worker-a --client-token token-worker-a func1 echo
```

The first field is the identity role. Workers may use both client and worker
commands. Clients may only use client commands; worker registration commands
such as `CanDo` and `Broadcast` are rejected for client connections. Admin
identities may register as either client or worker and may use all commands and
funcs.

With an auth file configured, the server also checks func-bearing requests
against the authenticated identity's func set. Role mismatches and unauthorized
requests return the existing failure response and are logged by the server.

`status` is filtered to the identity's allowed funcs. Other commands without a
func name, such as `ping`, `config`, `dump`, and `shutdown`, are limited by role
only.

The auth file is polled for changes and hot-reloaded. Reload is atomic: a fully
valid file replaces the active rules, while parse or validation errors leave the
previous rules active.

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
