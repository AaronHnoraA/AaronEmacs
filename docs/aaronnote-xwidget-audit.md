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
