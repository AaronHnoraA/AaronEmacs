# Aaronnote × xwidget Integration Audit

Audit of the full chain: Emacs (`init-aaronnote.el`) ↔ xwidget-webkit ↔ Node HTTP
server (`web-host.mjs`) ↔ CodeMirror 6 app (`aaronnote/main.ts`, `src/cm6/`).

## Chain Overview

```
Emacs Lisp
  my/aaronnote--post()          POST /emacs/command  → Node web-host
  my/aaronnote--api-call()      POST /api            → Node web-host
  my/aaronnote--process-filter  ← stdout events      ← Node web-host
  xwidget-webkit-callback advice  (load-changed, document-title-changed)

Node web-host (web-host.mjs)
  HTTP server (127.0.0.1:PORT, port from env or ephemeral)
  Static: dist/aaronnote/  (Vite bundle)
  API: /emacs/command, /emacs/event, /api, /note-asset, /aaronnote-asset

Browser (xwidget-webkit or Appine)
  aaronnote/main.ts  → window.aaronnoteApi (injected by Emacs)
  src/cm6/           → CodeMirror 6 editor + live preview + vim
  aaronnote/xwidget-key-guard.ts  → key interception
```

Key event channel: `stdout` newline-delimited events, e.g.  
`aaronote-event:key:{"key":"C-x"}`, `aaronote-event:saved:{"file":"..."}`.

## Findings & Status

### P0 — Raw-HTML widgets rendered unsanitized ← **Fixed**

**Files:** `src/cm6/live-preview.ts:533`, `:1645`  
**Issue:** `HtmlInlineWidget` and `HtmlBlockWidget` assigned `this.source`
(user-authored markdown HTML) directly to `innerHTML` with no sanitization.
The rest of the codebase deliberately uses `html: false` in markdown-it plus
`DOMPurify.sanitize` in `export-html.ts`, `paste-html.ts`, and
`diagram-render.ts`. Tags like `<script>`, `<img onerror=...>`, `<iframe>` would
execute in the live DOM on every render while the cursor was outside the tag.

**Fix:** `src/sanitize-html.ts` — new shared helper `sanitizeEmbeddedHtml()`
using the same DOMPurify policy as `paste-html.ts` (`FORBID_TAGS: ["script",
"style", "iframe", "object", "embed"]`, URI allowlist). Both `toDOM()` sites now
call it. Tests in `tests/sanitize-html.test.ts`.

### P1a — `url-retrieve` with no hang timeout ← **Fixed**

**Files:** `lisp/roam/init-aaronnote.el:588` (`/emacs/command`), `:805` (`/api`)  
**Issue:** If the Node server hangs, the response buffer is never killed. Violates
the project resource lifecycle rule ("url-retrieve callbacks should kill their
response buffer").

**Fix:** Both call sites now store the returned buffer and arm a `run-at-time`
fallback timer (5 s for fire-and-forget POST, 10 s for API calls) that kills the
buffer if the server never replies.

### P1b — `xwidget-webkit-callback` advice installed permanently ← **Fixed**

**Files:** `lisp/roam/init-aaronnote.el:1001`  
**Issue:** `advice-add` ran unconditionally inside `with-eval-after-load 'xwidget`
with no idempotency guard. File reload accumulated advice chain entries.

**Fix:** `my/aaronnote--xwidget-advice-installed` flag ensures the advice is added
exactly once, mirroring the `my/aaronnote--activity-hooks-installed` pattern.

### P1c — xwidget session hash retains dead buffer pointers ← **Fixed**

**Files:** `lisp/init-browser.el`  
**Issue:** `my/xwidget--sessions` entries were only reclaimed on forced reload.
User-killed xwidget buffers left dead buffer pointers in the hash. Bounded leak
that grows with multi-file sessions.

**Fix:** `my/xwidget--session-id` defvar-local stores each buffer's session key.
`my/xwidget--session-cleanup` is registered as a buffer-local `kill-buffer-hook`
in `my/xwidget--record-buffer`; it `remhash`es the entry when the buffer is killed.

### P2 — Vim mode not reset on xwidget focus loss ← **Fixed**

**Files:** `aaronnote/main.ts:268`  
**Issue:** When the user switches to another Emacs buffer and back, the CM6 `blur`
event fired but there was no vim mode reset. Returning to the editor in silent
insert or visual mode caused unexpected keystroke interpretation.

**Fix:** `onBlurVimReset` patched after `createVimLite`; on blur, calls
`vim.setMode("normal")` if not already in normal mode. Comment explains the
intentional double `editor.focus()` on pointer events (xwidget focus timing).

### P3 — Performance (no action, documented)

All three items are minor optimizations on an already viewport-aware pipeline:

- CJK line cache clears entirely on `doc.line(1)` change; could be narrowed to
  `firstChangedLine` neighborhood. Marginal gain.
- `selectionSet`-only updates rebuild all decorations; could skip non-cursor-
  dependent tokens. Marginal gain.
- Per-keystroke `htmlBlockDecoField` patching follows the existing
  `patchNearChanges` pattern (does not do full rebuild on every selectionSet).

## What Was NOT Changed

- The `url-retrieve` callbacks already had `unwind-protect` buffer cleanup on the
  success/error path — only the hang/no-reply path was unguarded.
- `xwidget-webkit-callback` advice is NOT removed on `my/aaronnote-stop` because
  xwidget-webkit may still be in use for non-aaronnote pages; the callback
  function is a no-op for non-aaronnote buffers.
- No architectural changes to the bridge protocol, build system, or server routing.

## Test Verification

```sh
# TypeScript unit tests
cd lisp/roam/aaronnote
npm test -- tests/sanitize-html.test.ts tests/cm6/commands.test.ts tests/paste-html.test.ts

# Build
npm run build:aaronnote

# Elisp parse check
emacs --batch -Q --eval '(with-temp-buffer (insert-file-contents "lisp/roam/init-aaronnote.el") (emacs-lisp-mode) (check-parens))'
emacs --batch -Q --eval '(with-temp-buffer (insert-file-contents "lisp/init-browser.el") (emacs-lisp-mode) (check-parens))'

# ERT lifecycle tests
emacs --batch -Q -L lisp -L site-lisp/general.el -l test/init-aaronnote-tests.el \
  --eval '(ert-run-tests-batch-and-exit t)'
```

End-to-end manual checks:
1. Open a note containing `<script>alert(1)</script>` and `<img src=x onerror=alert(1)>` —
   no alert should fire; cursor outside tag renders benign tags.
2. Switch to another Emacs buffer mid-insert-mode and back — editor should be in normal mode.
3. `my/aaronnote-stop` then re-open — no duplicate advice, session hash clean.

---

## Pass 3 — Stability & resource lifecycle (2026-06-09)

### What was fixed

| # | Priority | File | Change |
|---|---|---|---|
| 1 | P1 | `init-aaronnote.el:373` | Process filter generation guard — `my/aaronnote--handle-process-line` now fires only when `(eq proc my/aaronnote--process)`; a dying old process can no longer clobber port/ready state |
| 2 | P1 | `aaronnote/main.ts:317` | Save file-identity guard — `savingFile = currentFile` captured before `await api.notes.save()`; metadata update returns early if note switched during flight |
| 3 | P2 | `server/lib/runtime.mjs:3823` | Copilot SIGKILL fallback — `stop()` now arms a 2 s `.unref()` timer to `SIGKILL` if child ignores SIGTERM; mirrors the existing Emacs-side escalation |
| 4 | P2 | `init-aaronnote.el:998` | Pending-file POST buffer guard — deferred `my/aaronnote--open-file-in-web` lambda captures buffer; wraps call in `(when (buffer-live-p pending-buf) ...)` |
| 5 | P3 | `src/diagram-render.ts:348,361,370` | `isConnected` guard — each post-await key check now also tests `!element.isConnected`, matching the pattern in `math-render.ts` |
| 6 | P3 | `init-aaronnote.el:379` | Accumulator cap — `aaronnote-pending` is reset if it exceeds 256 KB without a newline, preventing unbounded memory growth from pathological output |

New ERT test: `my/aaronnote-process-filter-ignores-stale-proc-ready-line` — asserts that a non-current proc emitting a `ready:` line does not mutate `my/aaronnote--port`.

### Verified false alarms (excluded)

- **Roam sync never schedules** — `queueRoamDbSync` is the accumulator by design; `syncRoamDb` is triggered on demand via the `aaronnote:api:notes:roam-sync` channel (`web-host.mjs:396`).
- **Activity hooks duplicate on restart** — already guarded by `my/aaronnote--activity-hooks-installed` (Pass 1).

### Out of scope (verified safe)

Bounded LRU caches (math/code/diagram/note-code/save-dedup), SSE client teardown, atomic save + per-file serialization, viewport settle generation guard (Pass 2), once-registered global listeners, `ResizeObserver` teardown in `measured-observer.ts`, `uncaughtException`/`unhandledRejection` logging-only handlers.

---

## Pass 4 — Cursor-move performance (2026-06-09)

### What was changed

| # | Priority | File | Change |
|---|---|---|---|
| 1 | P1 | `aaronnote/main.ts` | Removed the main app's `onSelectionChange` cursor-position hook; cursor memory is now saved on blur/open/pagehide/beforeunload/visibility hidden and explicit navigation events |
| 2 | P1 | `aaronnote/main.ts` | `selectionchange` no longer schedules snippets/math/cursor/TOC work; it only updates the floating selection toolbar when the browser selection touches the editor |
| 3 | P1 | `aaronnote/main.ts` | Pure cursor movement, scroll, resize, xwidget key guard movement, and formatting shortcuts no longer request TOC updates; document changes still update TOC through `onChange` |
| 4 | P2 | `aaronnote/main.ts` | Cursor-position cache update now replaces the current file entry in place instead of rebuilding the positions array on every tracked event |
| 5 | P2 | `aaronnote/main.ts` | In-flight cursor-position saves now queue one latest-position follow-up instead of dropping the final lifecycle flush |

### Benchmark

Benchmark file used: `/Users/hc/HC/Org/Aaronnote/tests/synthetic_qc_note_5mb.md` (5,254,496 bytes). The requested singular path `~/HC/Org/Aaronnote/test/5mb.md` was not present.

Command used while the temporary benchmark test existed (the test file was removed after the run):

```sh
AARONNOTE_BENCH_FILE=/Users/hc/HC/Org/Aaronnote/tests/synthetic_qc_note_5mb.md npm test -- tests/perf-cursor-bench.test.ts --reporter verbose
```

Result from the temporary explicit benchmark:

- `no-main-selection-hook`: 300 cursor moves in 5142.73 ms; `hook-events=0`
- `simulated-old-selection-hook`: 300 cursor moves in 4979.63 ms; `hook-events=300`

Interpretation: the dominant remaining 5MB cursor cost is CM6 selection/layout/decorations, not the removed JS callback alone. The removed callback still matters operationally because the real app no longer mutates cursor-position state nor requests TOC/snippet/math work from document-level `selectionchange` on every move.
