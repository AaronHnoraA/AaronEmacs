# Aaronnote Jupyter Kernel Server

This directory contains the minimal local Jupyter runtime used by Aaronnote
`@@cell` blocks. It is not a JupyterLab frontend integration.

The runtime provides:

- a private virtualenv under `jupyter/.venv`
- Jupyter Server and `ipykernel`
- optional local kernelspec templates, such as Sage
- isolated Jupyter config/data/runtime directories under `jupyter/.jupyter`

From `lisp/roam/aaronnote`:

```sh
npm run jupyter:bootstrap
npm run jupyter:server
```

`jupyter/scripts/run-jupyter-server.sh` starts `jupyter-server` for the
Aaronnote cell service; it does not start JupyterLab.

## Cell service behavior (`server/lib/jupyter-cell.mjs`)

The Node cell service owns the server lifecycle and each cell run:

- It lazily spawns the server on first use and shuts it (and idle kernels) down
  after the TTLs below. A cached kernel id that no longer exists on the server
  self-heals: the run re-provisions the kernel once before surfacing an error.
- Cell code and saved outputs live in a hidden `.cell/` directory beside the
  note (`<note>.<lang>.<session>.<ext>` script, `<note>.output.*.json` mirror).
  The output mirror is written atomically and a corrupt mirror is ignored rather
  than propagated as an error; concurrent cells sharing one kernel serialize
  their writes so they cannot clobber each other.
- Consecutive `stdout`/`stderr` stream chunks are merged, and total stream text
  is capped so a runaway loop cannot produce an unbounded payload. The inline
  widget view truncates long output further; **Popout** shows the full capped
  output.

### Environment variables

| Variable | Default | Purpose |
|---|---|---|
| `AARONNOTE_JUPYTER_HOST` | `127.0.0.1` | Server bind host. |
| `AARONNOTE_JUPYTER_PORT` | `8890` | Server port. |
| `AARONNOTE_JUPYTER_URL` | `http://host:port` | Point at an externally-managed server instead of spawning one. |
| `AARONNOTE_JUPYTER_KERNEL_IDLE_TTL_MS` | `600000` | Idle kernel reap delay. |
| `AARONNOTE_JUPYTER_SERVER_IDLE_TTL_MS` | `90000` | Idle server shutdown delay. |
| `AARONNOTE_JUPYTER_EXEC_TIMEOUT_MS` | `0` (off) | Per-execution timeout. |
| `AARONNOTE_JUPYTER_MAX_STREAM_BYTES` | `1048576` | Cap on merged stream text per run before truncation. |

## Known trade-offs

- The server runs with an **empty token/password on `127.0.0.1`**: any local
  process on this machine can reach the kernel API. This is a single-user
  local-only design; do not bind it to a non-loopback host without adding auth.
- Each execution opens a fresh kernel websocket rather than holding a persistent
  connection. The local handshake is cheap and this keeps request handling
  stateless; it is a deliberate choice, not an oversight.
