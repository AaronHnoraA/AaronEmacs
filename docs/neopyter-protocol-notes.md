# Neopyter Protocol Notes

Reverse-engineered from [SUSTech-data/neopyter](https://github.com/SUSTech-data/neopyter)
branch `master` (last pushed 2026-05-20). Confirms the wire contract for the Emacs client.

---

## Architecture

```
Browser (JupyterLab)
  └─ neopyter labextension ─────────── ws://HOST:PORT ──────────────► Emacs WS server
         (msgpack-rpc SERVER:              [direct mode]                  (RPC client)
          owns the dispatcher,
          sends responses)
```

**Direct mode role-crossing:**
- Emacs is the **WebSocket server** (listens at `ws://HOST:PORT`)
- The extension is the **WebSocket client** (connects to Emacs)
- The extension owns the RPC **dispatcher** (handles calls)
- Emacs is the RPC **client** (sends requests, receives responses)

This is counter-intuitive: the WS server is the RPC client.

---

## Transport / framing

| Property | Value |
|---|---|
| Transport | WebSocket (direct mode) |
| Frame type | **Text** frames |
| Payload encoding | **Base64** of raw msgpack bytes |
| Encoding in JS | `websocket.send(bytesToBase64(msgpackData))` |
| Decoding in JS | `base64ToBytes(event.data)` → decode msgpack stream |
| Encoding in Elisp | `(base64-encode-string (msgpack-encode obj) t)` |
| Decoding in Elisp | `base64-decode-region` → `msgpack-read-from-string` |

Source confirmation: `src/transport/websocketTransport.ts` lines:
```typescript
this.websocket.send(bytesToBase64(data));   // send
const buf = base64ToBytes(event.data);      // receive
```

---

## Message format (msgpack-rpc)

From `src/msgpackRpcProtocol.ts`:

### Request (editor → extension)
```
[0, msgid, method, params]
```
- `0` — type constant `Request`
- `msgid` — u32, monotonic, used to match response
- `method` — utf-8 string
- `params` — msgpack array of arguments

### Response (extension → editor)
```
[1, msgid, error, result]
```
- `1` — type constant `Response`
- `msgid` — matches the originating request
- `error` — null (no error) or string/object
- `result` — return value (null if error)

### Notification (extension → editor, Phase 5)
```
[2, method, params]
```
- `2` — type constant `Notification`
- No `msgid` (fire-and-forget)
- **Note:** upstream extension does not currently emit notifications.
  The Emacs server can receive them (`notification-handlers` alist is ready).
  A future upstream PR to SUSTech-data/neopyter is needed.

---

## Path convention (CRITICAL)

**All notebook-level and cell-level methods take the notebook `path` as their first
parameter.** The extension's `getNotebookModel(path)` looks up the active panel by path.

```typescript
// src/index.ts (simplified)
fullSync(path: string, cells: TCell[])
activateCell(path: string, index: number)
scrollToItem(path: string, index: number, align: string, margin: number)
runSelectedCell(path: string)
// … all notebook methods follow this pattern
```

**Paths must be relative to the JupyterLab root directory** (the directory from which
`jupyter lab` was launched, a.k.a. `--notebook-dir` / `--root-dir`).  JupyterLab's
content manager prepends this root before every file operation; sending an absolute path
produces a doubled path and an `ENOENT` error.

### Emacs-side configuration

```elisp
;; In lisp/init-neopyter.el (or etc/local.el for machine-specific roots):
(setq aaron-neopyter-jupyter-root "/Users/hc/Documents/Noema")
```

`aaron-neopyter-sync--rpc-path` strips this prefix from the absolute `.ipynb` path
before every RPC call, e.g.:
```
/Users/hc/Documents/Noema/project/lab/foo.ipynb
  → project/lab/foo.ipynb
```

If the root changes, run `M-x aaron-neopyter-detect-jupyter-root` (requires a notebook
open in Lab) to auto-detect it from `getCurrentNotebook`.

---

## Authoritative RPC method registry

Source: `src/index.ts` dispatcher (merged from docmanager, notebook, cell dispatchers).

### Health / utility
| Method | Params | Return |
|---|---|---|
| `getVersion` | `[]` | version string |
| `echo` | `[message]` | `"hello: {message}"` |
| `executeCommand` | `[command, args?]` | void |

### File / docmanager (paths are relative to Lab root)
| Method | Params | Return |
|---|---|---|
| `isFileExist` | `[path]` | bool |
| `isFileOpen` | `[path]` | bool |
| `openFile` | `[path]` | void |
| `openOrReveal` | `[path]` | void |
| `activateNotebook` | `[path]` | void |
| `createNew` | `[path, widget, kernel]` | void |
| `getCurrentNotebook` | `[]` | **relative** path string |
| `closeFile` | `[path]` | void |

### Notebook methods (all take `path` as first arg)
| Method | Params | Return |
|---|---|---|
| `save` | `[path]` | void |
| `getCellNum` | `[path]` | integer |
| `setCellNum` | `[path, n]` | void |
| `getCell` | `[path, idx]` | cell object |
| `insertCell` | `[path, idx, cell]` | void |
| `deleteCell` | `[path, idx]` | void |
| `setCellSource` | `[path, idx, source]` | void |
| `setCellType` | `[path, idx, type]` | void |
| `activateCell` | `[path, idx]` | void |
| `scrollToItem` | `[path, idx, align, margin]` | void |
| `setMode` | `[path, mode]` | void |
| `selectAbove` | `[path]` | void |
| `selectBelow` | `[path]` | void |
| `fullSync` | `[path, cells]` | void |
| `partialSync` | `[path, startIdx, endIdx, cells]` | void |
| `runSelectedCell` | `[path]` | void |
| `runAllAbove` | `[path]` | void |
| `runAllBelow` | `[path]` | void |
| `runAll` | `[path]` | void |
| `restartKernel` | `[path]` | void |
| `restartRunAll` | `[path]` | void |

`align` values: `"auto"` | `"start"` | `"end"` | `"center"`
`mode` values: `"command"` | `"edit"`

### Cell schema for sync methods
Minimum required fields:
```json
{"source": "string", "cell_type": "code|markdown|raw"}
```
Optional: `metadata` (object), `outputs` (array).

### Completion (not implemented in v1)
| Method | Params | Return |
|---|---|---|
| `complete` | `[options]` | completions |
| `reconciliatorComplete` | `[options]` | completions |
| `kernelComplete` | `[source, offset]` | completions |

---

## Percent-format cell grammar

Source: `lua/neopyter/parser/percent.lua`

```
separator_line   ::= "# %%" suffix?
suffix           ::= " " title? "[" cell_type "]" metadata?
                   | " " metadata_only
title            ::= non_bracket_text
cell_type        ::= "code" | "markdown" | "md" | "raw"
"md"             normalizes to "markdown"
default type     ::= "code"
```

First cell may have no separator (`no_separator = true`).
Tags example: `# %% tags=["foo","bar"]`

---

## Filename mapping

Default: `foo.ju.py` → `foo.ipynb`, `foo.ju.r` → `foo.ipynb`

Rule (in `aaron-neopyter-sync--own-mapper`):
1. Strip `.ju.` infix: `replace-regexp-in-string "\\.ju\\.\\([^.]+\\)\\'" ".\\1"`
2. Replace remaining extension with `.ipynb`

This produces the **absolute** OS path. Before sending over RPC, `aaron-neopyter-sync--rpc-path`
strips `aaron-neopyter-jupyter-root` to produce the relative path JupyterLab expects.

**Bug fixed in `my/jupytext--default-notebook-file`** (`lisp/init-jupyter-core.el`):
previously it used bare `file-name-sans-extension` which left the `.ju.` infix in the stem,
producing `foo.ju.ipynb`. Now it calls `my/jupytext--canonical-script-file` first.

---

## Proxy mode (not yet implemented)

In proxy mode:
- The Jupyter server extension listens on TCP `{host}:{port}` (default 9001)
- Emacs connects as a TCP **client**
- No base64 wrapping (raw msgpack bytes)
- Server: `neopyter/tcp_server.py`

The `aaron-neopyter--conn` struct already has a `mode` field reserved for proxy.
The transport abstraction in `aaron-neopyter-rpc.el` can be extended to support
a TCP client behind the same `rpc-request`/`rpc-notify` API.

---

## Limitations and future work

1. **Phase 5 (browser→Emacs events):** The extension dispatcher does not push
   `activeCellChanged` or `selectionChanged` notifications. The server is ready
   to receive them (`notification-handlers` alist). Upstream PR needed.
2. **Proxy mode:** TCP client transport not yet implemented.
3. **Bidirectional text editing:** Not supported (no CRDT/OT). Emacs is authoritative.
4. **Completion:** `kernelComplete` and `reconciliatorComplete` exist in the API
   but are not wired up to any Emacs completion backend.
5. **Partial sync:** `partialSync` is called from `aaron-neopyter-jupyter.el`
   but the sync layer always uses `fullSync` (safe default). Incremental diffing
   is gated behind `aaron-neopyter-partial-sync`.
