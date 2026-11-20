# Agenda navigation repair

Verified on 2026-11-19. Current contracts are in
[Agenda DSL](../../design/agenda-dsl.md) and the [Agenda guide](../../guide/agenda.md).

The native visitor previously opened a raw Markdown buffer, bypassing the live
Noema pane; its mouse map also lacked a dedicated task visitor. The replacement
uses the existing pane owner, exact client routing and a one-request wait for
the page's `current-file` event. The current foreground activity is applied
before navigation, using the existing manual-pause policy.

The renderer resolves the task against its unsaved Markdown through the native
read-only document inspector. It rejects ambiguous identities, stale results,
wrong full file paths and comment markers inside code examples. Native commands
cancel a pending visit permanently; page pauses and document loads cancel an
in-flight renderer inspection. Web Agenda and global attention share this path.

Validation results:

- `make agenda-test`: 49 + 12 + 79 ERT tests passed, including real gateway source
  operations and canonical pane/client tests.
- Noema `make test`: 359 files / 3693 tests passed; 7 files / 17 tests skipped.
- `go test -tags fts5 ./...`, Emacs `make research-test` and `make jupyter-test`
  passed. Noema `make install` rebuilt and linked the renderer and kernel.
- `scripts/check-agenda-navigation.mjs` passed in real WebKit: the actual host
  command listener selected a live unsaved task, kept the text, cancelled a
  delayed inspection on pause/resume and issued no saves. Its host is faked.
- Independent review also exercised the actual Emacs command loop for one-shot
  cancellation and hook removal after completion.
- Startup-state comparison found exactly one intended addition:
  `HOOK +noema-current-file-functions nil`. The callback is empty at startup;
  its renderer adapter is loaded on first visit. No unrelated startup state
  changed. Elisp parentheses and both repositories' diff checks passed.

The separate `scripts/check-native-agenda.mjs` smoke check still stops at line
116: its whole-cell equality assertion does not expect the `done:` date that
WorkNode completion now writes. The same assertion failed on unmodified Noema
HEAD `e4affce`, in a disposable checkout with the same build/resources. Its
earlier read-only document assertions passed. This check cannot be reported as
passing, and its later legacy write/clock checks were not reached.

`bin/emacs-app status` reported stopped. This pass did not test a physical mouse
click or focus transfer in a live Emacs WKWebView; batch pane tests and the real
WebKit editor check do not substitute for that final UI acceptance.
