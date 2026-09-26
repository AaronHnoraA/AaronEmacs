# Jupyter parity audit — 2026-09-26

The goal remains full VS Code Jupyter capability alignment across Emacs editing,
Noema execution, and Remote placement. This is an evidence register, not a claim
that parity is complete. `.noema` work documents remain outside Jupyter under
D-023; ordinary `.ipynb` and Markdown `@@cell` sidecars are the notebook scope.

## Upstream reference and filesystem semantics

Primary references inspected:

- [VS Code kernel management](https://code.visualstudio.com/docs/datascience/jupyter-kernel-management): kernel source selection, existing servers, Python environments, remembered selection.
- [VS Code notebooks](https://code.visualstudio.com/docs/datascience/jupyter-notebooks): cell operations, execution, completion, variables/data viewer, output saving, notebook diff, export and debugging.
- [VS Code Remote SSH](https://code.visualstudio.com/docs/remote/ssh): opening a folder on an SSH host is a remote workspace operation.
- [emacs-jupyter](https://github.com/emacs-jupyter/jupyter): REPL, evaluation, introspection, Org Babel and server connectivity. The standalone Board REPL still uses this package; Noema notebook execution uses Noema's own protocol service.
- [Jupytext script formats](https://jupytext.readthedocs.io/en/latest/formats-scripts.html): percent code, Markdown and raw cells. Our editable ipynb projection uses an internal codec, not an external Jupytext process. Pairing/synchronization with ordinary `.py` files is a separate capability and is not implied by the projection.

Three placements must stay distinct:

| Mode | Notebook storage | Compute | Project / files |
| --- | --- | --- | --- |
| Local notebook + server URL | Existing local file | Jupyter Server kernel | Local project remains local; code paths resolve on the server |
| Remote workspace `/fs:TARGET:/...` | Target filesystem, through Remote | Target broker kernel or selected server | Remote file identity, workspace, environment and LSP belong to Remote |
| Jupyter Contents workspace | Jupyter Server Contents API | Same server | Dired browse, native notebook projection, save and execution use a Contents-backed Remote target; no local project is required |

A URL's `/user/alice/` or proxy prefix is an HTTP base path, not an OS directory.
It cannot establish a mapping to `/home/alice` without an explicit mapping.
A server URL alone therefore must not silently relocate the local project.

## Requirement / evidence register

| Requirement | Authoritative implementation / evidence | Status and remaining acceptance |
| --- | --- | --- |
| Local kernelspec launch | `server/jupyter/kernel-registry.mjs`, local real-kernel suites | Implemented; rerun full local real-kernel matrix for final acceptance |
| SSH broker launch | `init-aaronnote-jupyter-runtime.el`, live Aaron-PC probe | Real remote launch, execution and completion passed |
| Five ports / remote connection file | Runtime broker + Remote channel groups | Real attach and channel recreation passed; same state and local endpoints preserved |
| Network reconnect / kernel death | `jupyter-kernel-remote-reconnect.test.ts`, heartbeat tests | Unit coverage passed; channel recreation tested live. Whole-link loss during an in-flight execution remains to test |
| HTTP URL, port, proxy prefix, token | `init-aaronnote-jupyter-server.el`, server registry | Real forwarded `/audit/` server with authentication passed |
| Password, Hub, HTTPS/SNI, gateway | Server auth / connection implementations | Protocol unit checks exist; deployment-level matrix remains incomplete |
| Adopt / restart / shutdown ownership | Server registry + kernel registry | Live adopted client detach preserves owner; owned restart clears state; owned shutdown removes kernel |
| Server filesystem | `remote-backend-jupyter.el`, `contents-files.mjs`, `serverFile` API | Real Emacs Dired → ipynb edit/save → nested-directory kernel execution → persisted output passed on Aaron-PC; server storage limitations below |
| Remote library integration | Context, workspace environment, channels, keyed resources | Live probe uses these production APIs; no private SSH tunnel implementation in test |
| Server management UI | Jupyter Board URL add/edit/forget, routes, auth, catalog check | Implemented and ERT verified; graphical layout / accessibility and richer per-kernel actions remain |
| Cell snippets | Shared language-aware notebook table; `jraw` structural operation; Python magics | Added; retain `.noema` / Jupyter separation. Check all snippet modes before final acceptance |
| Cell structure / execution | Notebook codec, cell API, NCell commands | Existing code/Markdown/raw, run ranges, split/merge; multi-selection, section execution, undo parity need audit |
| Completion / inspection / LSP | CAPF, kernel introspection, kernel runtime LSP, shared project context | Real Aaron-PC source LSP resolves numpy with project Python; local notebook/remote kernel mismatch blocks local fallback. Broader language/server matrix remains |
| Variables / table viewer / rich output | `jupyter-variables-view.ts`, rendermime, widget runtime | Implemented pieces; sorting/filtering, large data and output-save UX need comparative verification |
| Debug cell / run by line | `server/jupyter/notebook-debug.mjs`, native Dape adapter | Real Aaron-PC server: breakpoints, variables, evaluation, stepping, Run by Line, cross-cell/module source and stop preserving kernel passed. Raw-port/local placement and graphical UX acceptance remain |
| Export | Native export command + nbconvert snapshot converter | Real script/HTML/PDF export of remote notebook, stored outputs and relative image passed; cells are not executed |
| Notebook diff | Cell-aware source/output/metadata text + native Ediff/VC | Real Git revision and Ediff cleanup passed; richer rendered comparison remains |
| Notebook trust | Renderer/execution policy audit | Incomplete: rich-output model still starts trusted; requires coordinated rendering/execution policy |
| Python environments and kernel MRU | Kernelspec finder and selectors | Environment discovery / installation prompts / MRU need explicit comparison and implementation |

## This increment

The remote development follow-up is documented in
[Kernel project context](jupyter-project-context.md): shared kernelspec metadata,
normal Remote/TRAMP Dired and shell, source LSP, and direnv. Real Aaron-PC checks
covered read/save, shell cwd/environment, numpy LSP hover, and a real remote
kernel using the same Python/root/direnv. No local-notebook mirroring or remote
editor instance was added. This does not close the full parity audit.

- Added a Jupyter Servers section to the native Board: direct URL versus Remote
  target routing, add/edit/forget, explicit asynchronous catalog checks, running
  kernel counts and notebook kernel-picker entry. Rendering performs no network
  requests; stale catalog responses cannot overwrite newer results.
- Pasted tokens are stripped before config persistence, held only for the Emacs
  session, and scoped to server URL, target, auth and user. Public catalogs also
  redact tokens in legacy URL configurations. Persistent credentials remain in
  auth-source. Editing connection identity releases the old recoverable route.
- Added a structural `jraw` action and language-aware code/Markdown/raw templates.
  Python Markdown/raw template bodies now start with a comment prefix. Added
  HTML, LaTeX, JavaScript, SQL and writefile cell magics (SQL requires its IPython
  extension). No magic snippet claims support from a kernel that lacks it.
- The live test exposed Emacs rejecting `/dev/urandom` as a non-regular file.
  The fallback now uses OS-backed OpenSSL/Python randomness rather than a
  timestamp and Emacs PRNG for kernel HMAC keys.

## Contents filesystem increment

- Jupyter Board's **Browse Files** opens a configured server in standard Dired.
  `my/noema-jupyter-server-browse` is the command; the file identity is
  `/fs:jupyter.<UTF-8 server id hex>:/server-relative/path`. Removed profiles
  remove their mounts. Gateways expose no Contents mount.
- Binary-safe reads/writes, Unicode filenames, metadata, directory completion,
  copy, rename and delete use Noema's authenticated server registry. No OS path
  mapping or local project mirror is inferred from the HTTP base path.
- Native ipynb projection edits save through ContentsManager. Noema accesses the
  same namespace directly, avoiding a recursive Node → Emacs → Node file call.
  A plain kernelspec name starts on the owning server, and its session receives
  the notebook's full relative path so nested working directories are correct.
- File-only targets expose no process capability or executable PATH. Dirvish
  shell probes and LSP process discovery are skipped; standard Dired and kernel
  completion/inspection remain available. Unsupported file operations fail
  explicitly instead of touching a same-named client path.
- ContentsManager controls persistence and hidden-file access. The protocol does
  not promise POSIX atomic replacement, chmod, symlinks, locks, or an atomic
  cross-client compare-and-swap. The API's optional timestamp precheck is not an
  atomic conflict guarantee. See the [REST API](https://jupyter-server.readthedocs.io/en/latest/developers/rest-api.html)
  and [ContentsManager contract](https://jupyter-server.readthedocs.io/en/latest/developers/contents.html).

The opt-in `make jupyter-contents-live-smoke` uses the same environment variables
as the broker probe below. It loads the full Emacs configuration and uses a
stdio test transport into the real Noema service; Contents HTTP, server auth,
Remote forwarding, notebook codec and kernel execution are real. It verifies
Dired listing, source projection, edit/save, server kernelspec choices, nested
cwd and persisted output. It does not validate graphical output rendering or
the production gateway transport. `test/jupyter-remote-fixture.py` prepares an
isolated remote environment and provides a PID-checked `--stop ROOT` cleanup.

## Validation and reproduction

### Notebook debugger increment

Notebook **Debug** and **Step** controls now use Dape through a single-client
loopback DAP bridge. Kernel requests use Jupyter's control channel and debug
events use IOPub, following the [Jupyter messaging protocol](https://jupyter-client.readthedocs.io/en/latest/messaging.html#debug-request).
The endpoint stays on the Emacs client even for a Contents-only workspace.
The kernel must advertise debugger support; `.noema` is refused before launch.

`C-c i g` starts Debug Cell, `C-c i .` starts Run by Line or advances one
statement, `C-c i c` continues, and `C-c i q` stops. Dape supplies the breakpoint,
stack, variable and evaluation interfaces. The notebook is read-only for the
duration of the source snapshot. Stop interrupts the current run and detaches
the debugger without shutting down its kernel. Output writes settle before
normal cleanup; a disconnected/unresponsive kernel bounds that wait.

The opt-in `make jupyter-debug-live-smoke` uses the same environment as the
Contents smoke test. On Aaron-PC it passed actual Dape breakpoint placement,
notebook source-line mapping, paused-frame variables, next/evaluate, Run by Line,
stop followed by reuse of retained kernel state, a breakpoint in a previously
executed cell, step-in to a server Python module, source retrieval by DAP
reference, and persisted outputs. The test bridge forwards production session
events; it exposed and fixed a missing post-debug session refresh and an output
write/stop race. Protocol regressions additionally cover conditional/log
breakpoint forwarding, Markdown rejection, fragmented DAP frames, cleanup,
capability checks and timeouts.

This does not establish every debugger scenario: local/raw-port deployments,
exception UI, preplaced breakpoints in arbitrary Remote module buffers and
graphical layout still need acceptance. Restart currently requires a fresh
notebook Debug command; editing during a debug run is deliberately locked.

### Board and connection maintenance

- Normal kernel selection no longer scans or lists raw `kernel-*.json` files.
  One **Connect to Existing Kernel…** entry opens the explicit attachment list.
- Clicking a profile keeps the existing detail page. Its JSON section is now
  editable; **Save to kernel.json** or `C-x C-s` validates the JSON and writes
  the original file in the owning filesystem. The header is read-only, edits
  support undo, and a changed original file prevents overwriting another edit.
  Saving invalidates kernelspec caches; changes apply on the next launch.
- Connection maintenance runs asynchronously every 15 minutes locally and
  during target discovery, throttled per target/directory. It removes only
  files older than ten minutes whose five local TCP ports refuse connections,
  whose filenames have no live process references, and whose identity remains
  unchanged through a second check. Symlinks, external addresses and uncertain
  liveness are retained. No kernel is terminated by the cleanup script.
  The initial local cleanup removed **159** abandoned files and retained two
  recent files.
- The actual `Open REPL` failure, `void-variable state`, was reproduced in
  emacs-jupyter's compiled monad functions. A targeted compatibility check
  reloads the unchanged upstream source after its macros/classes are available.
  `make jupyter-repl-live-smoke` passed actual local Python startup, execution
  through the REPL cell and shutdown; package sources remain untouched.

For this increment, Noema's full suite passed **2,525** tests (**16 skipped**)
with `VITEST_MAX_WORKERS=2`; unrestricted parallel runs exposed timing-sensitive
LaTeX and heading-fold checks, both passing with bounded worker concurrency.
Research ERT passed **333**, Jupyter ERT passed **211**, and the cleanup script's
two regression tests passed. The five changed Jupyter Lisp modules passed strict
byte compilation. Actual asynchronous maintenance through Remote retained the
two remaining local records. Go tests, build and install passed. The Aaron-PC
debug fixture was stopped and removed after the live tests.

`make jupyter-test` runs bridge, notebook, LSP-runtime and Board ERT suites.
`test/init-snippets-tests.el` separately checks the shared snippet activation
and expansion. This increment passed **195 Jupyter ERT checks**, **5 snippet
checks**, and the additional real CSPRNG fallback regression. Focused Node
checks covered server auth/registry, remote reconnect,
heartbeat and finder: **30 passed** under Node 26.5.0.

For the Contents increment, `make jupyter-test` passed **203 ERT checks**,
`make lsp-test` passed **126**, and `make research-test` passed **333**.
Noema `make test` passed **2,521** tests (**16 skipped**), and the required Go
tests, `make build` and `make install` passed. Remote strict byte compilation
passed. The Remote gateway regression uncovered an unmasked client Close
frame which the server acknowledged after the client had destroyed its socket,
triggering SIGPIPE in the full Emacs configuration. The gateway now rejects
unmasked control frames before replying, as required by [RFC 6455 §5.1](https://www.rfc-editor.org/rfc/rfc6455#section-5.1).
Both the rejection and normal masked Close acknowledgement have regressions;
the nine gateway tests pass. The final `make remote-test` rerun passed all **406 ERT checks** plus the Node source-agent suite.

The opt-in `make jupyter-live-smoke` uses
`test/jupyter-remote-live-smoke.el` and its Node companion. Set:

```sh
JUPYTER_LIVE_TARGET=aaron-pc \
JUPYTER_LIVE_ROOT=/tmp/your-isolated-jupyter-environment \
JUPYTER_LIVE_SSH=/usr/bin/ssh \
JUPYTER_LIVE_NODE=/path/to/node-26.5.0 \
make jupyter-live-smoke
```

The prepared root needs `venv/bin/{python,jupyter}`, `work/`, and `runtime/`.
Start an isolated Jupyter Server with its root in `work/` and
`JUPYTER_RUNTIME_DIR=.../runtime`; the probe reads its `jpserver-*.json` privately.
It sends credentials to the Node client on stdin, not command-line arguments.
The fixture server and environment remain the caller's cleanup responsibility;
the probe cleans its own kernels, routes, attachment file and test contents.

On **Aaron-PC**, Python 3.14.4, ipykernel 7.3.0, Jupyter Server 2.21.1,
jupyter-client 8.10.0 and ipywidgets 8.1.9 passed:

1. Broker launch, real execution and completion through all five routed ports.
2. Channel-group recreation with stable endpoints and `noema_audit_state` retained.
3. Remote connection-file attachment, execution and owner survival after detach.
4. Forwarded HTTP/WebSocket authentication and prefixed API paths.
5. Contents save/read/delete; server kernel start/adopt/detach/restart/shutdown.

The isolated fixture server was stopped and its `/tmp` environment removed.
Homebrew `/opt/homebrew/bin/ssh` returned `No route to host` while `/usr/bin/ssh`
connected to the same endpoint. The probe selects the working executable through
its process-local client path; no global SSH configuration was changed. The
underlying Homebrew SSH discrepancy remains undiagnosed.

Final acceptance still requires completing the missing rows, broad regression
checks and actual Emacs UI verification. A green routing probe alone is not full
VS Code parity.
