# AGENT.md

Guidance for coding agents working in this Emacs configuration.

## Start Here

Before changing behavior, read the closest existing documentation instead of
reconstructing the system from scattered Elisp:

- `docs/README.md` is the documentation map.
- `docs/settings-cookbook.md` answers "where should this change live?"
- `docs/maintenance.md` covers package locks, vendored Elisp, runtime state,
  compile helpers, bootstrap, and recovery.
- `docs/dev-guide.md` covers completion, LSP, projects, TRAMP, terminals,
  browser/Appine, and AI integrations.
- `docs/remote-framework.md` is authoritative for logical `/fs` paths,
  target/pipeline/backend/session routing, routed processes and channels, and
  environment/PATH capsules.
- `docs/remote-parity.md` defines what “VS Code Remote-level” completion means;
  a registered capability without local, remote, and resilience tests is not
  complete.
- `docs/lsp-workflow.org` covers the LSP route model, Hub, Doctor, and how to
  add new routes or Eglot server mappings.
- `docs/jupyter-workflow.org` covers Org Babel, existing-kernel workflows,
  Jupytext, kernelspecs, and remote kernels.
- `docs/org-guide.md`, `docs/project-guide.md`, and
  `docs/research-notes-workflow.md` cover their respective domains.

If code and docs disagree, inspect the current code, make the minimal correct
change, and update stale docs when that is part of the task.

## Project Model

This is a personal Emacs configuration for macOS, currently targeting Emacs 31.
It uses `package.el` and `use-package`; do not introduce `straight.el`,
`elpaca`, or a second package-management stack.

Load order is:

```text
early-init.el -> init.el -> lisp/init-modules.el
```

`lisp/init-modules.el` is the authoritative module order. Most local modules
live under `lisp/`; language-specific modules live under `lisp/lang/`; Org and
Jupyter internals may live under `lisp/org/`.

When adding a module:

- Put it in the owning directory.
- End it with `(provide 'init-foo)`.
- Add it to `lisp/init-modules.el` in dependency order.
- Use `declare-function` for cross-module calls when byte compilation needs it.
- Keep application-local names under the existing `my/` convention; hook
  helpers usually use a `*-h` suffix.  The public remote framework is the
  deliberate exception and uses the `remote-*` namespace without `my/`.

## Do Not Reinvent Existing Surfaces

This config already has maintenance and workflow entry points. Reuse them:

- LSP: `my/register-lsp-mode-preference`,
  `my/register-eglot-server-program`, `my/language-server-manager`, and
  `my/language-server-doctor`.
- Jupyter: `my/org-babel-jupyter-native-backends`, remembered
  `kernel-*.json` connection files, Jupyter Hub/Doctor, and Jupytext helpers.
- Projects: `my/project-dispatch`, `my/project-switch`,
  `my/project-open-workbench`, `my/project-search-paths`, Projectile,
  Perspective, Transient, and `show-imenu`.
- Compile/maintenance: `my/compile-board`, `my/compile-dispatch`, `make`
  targets documented in `docs/maintenance.md`.
- Performance diagnostics: `my/performance-watch`,
  `my/performance-report-string`, `my/performance-snapshot`,
  `my/performance-hook-snapshot`, and `my/performance-org-buffer-snapshot`.
  Prefer these APIs over asking for screenshots when investigating runtime or
  Org performance.  In the watch buffer, `y` copies the full current report.
- Templates: the built-in `auto-insert` template layer in
  `lisp/init-auto-insert.el`; do not revive the old Doom/Yasnippet file-template
  path.
- Config registry: the `config` package under `site-lisp/config/`
  (`config-register`, `config-register-hook`, `config-register-file`,
  `config-get`, `config-set`, `config-reset`, `config-board`). When a setting
  should be user-tunable: register it, put the value in `etc/config-store.el`
  (or a registered `etc/` file), and **remove any scattered `setq` call** — that
  is variable pollution.  Do NOT encode values in Lisp code if they are tracked
  here.  See `docs/config-management.md`.

Do not add a repo-wide "unified tool" just to make unrelated domains look
consistent. Prefer the existing pattern: keep core behavior in the owning
module, put operational UI in a small `*-tools.el` module when needed, and
document the workflow in `docs/`.

## Performance Diagnostics

For runtime or Org performance work, start from the existing performance
surface:

- Interactive: `M-x my/performance-watch` or `SPC h p`.
- Agent/batch: evaluate `(my/performance-report-string)` for a full text page,
  or `(my/performance-snapshot)` for structured data.
- Keep auto refresh off unless a time series is needed; each refresh samples
  `ps` and can perturb the measurement.
- For Org issues, check LaTeX running/queued/pending, local
  `post-command-hook`, `after-change-functions`, `jit-lock-functions`, timers,
  and visible Org buffers before removing hooks.
- Do not slow the LaTeX preview overlay placement path to save unrelated Org UI
  cost.  Prefer optimizing visual decoration, table alignment, diagnostics, or
  side-buffer refresh paths first.

## Keybinding Rules

The keybinding layer is the vendored `site-lisp/general.el`. It is a startup
dependency, not an optional experiment.

- Do not add new `global-set-key` calls for normal configuration shortcuts.
- For leader bindings, edit the existing leader definitions in
  `lisp/init-evil.el`.
- For localleader bindings, use the existing localleader helpers.
- For global non-leader bindings, use `general-define-key`.
- For concrete mode keymap objects that only exist after a package loads, bind
  inside `with-eval-after-load`; `evil-define-key*` is acceptable there.
- Domain-neutral route-table helpers live in vendored `general.el` as
  `general-route-*`; concrete policy still belongs in the owning init module
  such as `lisp/init-open.el`.

When changing user-facing shortcuts, update the relevant docs, usually
`docs/daily-usage.md`, `README.md`, and sometimes `docs/dev-guide.md` or
`docs/settings-cookbook.md`.

## Package Rules

- Ordinary packages belong in the responsible module with `use-package` and
  `:ensure t`.
- VC packages must go through `my/package-ensure-vc` or
  `my/package-register-vc`; do not scatter raw `package-vc-install` calls.
- Package recovery and lock audit must keep working without loading the whole
  `init.el`; bootstrap logic belongs in `bootstrap.el` and `package-lock.el`.
- System dependencies such as `rg`, `git`, `latexmk`, `dvisvgm`, `hunspell`,
  `gls`, language servers, and local build chains are not Emacs package deps.

After package changes, use the documented lock/install/audit flow from
`docs/maintenance.md` and `docs/migration.md`.

## Runtime State

Runtime and cache data belongs under `var/`. Local private config and secrets
belong under `etc/`.

Do not reintroduce top-level runtime files such as:

- `projects.eld`
- `eln-cache`
- `tree-sitter`
- stray `.elc` files
- local API keys or machine-local query URLs

Existing state backup/restore flows are documented in `docs/maintenance.md`.

## Resource Lifecycle Rules

Prefer bounded, explicit lifetimes for features that allocate Emacs resources:

- Store timer objects returned by `run-at-time`, `run-with-timer`, or
  `run-with-idle-timer`, and cancel them when the owning buffer, mode, process,
  or UI surface is torn down.
- Async process sentinels should resolve outstanding callbacks, cancel request
  timers, clear chunk/read buffers, and avoid mutating a hash table while
  iterating over it.
- Async process startup should clean any temp files, markers, and log buffers if
  `make-process` or the surrounding setup fails.
- `url-retrieve` callbacks should kill their response buffer with
  `unwind-protect` after parsing or handing off the result.
- Global hooks installed for buffer-local features need reference-style cleanup:
  keep them while any buffer still uses the feature, and remove them when the
  last owner goes away.
- Caches keyed by buffers, windows, markers, or other live objects should either
  be bounded/cleared on teardown or use weak hash-table keys where appropriate.
- Markers and overlays created for async work should be released or deleted on
  success, failure, cancellation, and buffer teardown.
- Long-lived shared log buffers should be bounded or explicitly user-managed.

## Domain Policies

LSP:

- Default route is Eglot.
- `lsp-mode` is opt-in per major mode through
  `my/register-lsp-mode-preference`.
- Custom Eglot mappings go through `my/register-eglot-server-program`.
- Eglot and lsp-mode startup must use the shared `language-server` remote
  adapter.  Server placement defaults to the active target/workspace; resolve
  executables from its environment and never embed `/ssh:` or `/rpc:` paths in
  contacts.  Mark only genuine client-side UI helpers as client placement.
- Remote language integrations must preserve local feature parity.  For
  multi-process tools such as Lean, Node, Lake, workers, and helper ports must
  share the target environment; target-side HTTP/TCP helpers reach the client
  through `remote-channel` forwarding.
- Give each language-helper process instance its own owned discovery/port file;
  never let concurrent reconnects overwrite a project-global endpoint file.
  Validate local owners and clean up only files owned by the exiting process.
- Keep the three-layer split: `init-lsp.el` for routing/core setup,
  `init-lsp-ops.el` for backend-agnostic commands, and `init-lsp-tools.el` for
  Hub/Doctor/dispatch/session knobs.
- When LSP behaves strangely, use `my/language-server-doctor` before changing
  code.

Org/Jupyter/research:

- Org is intentionally feature-rich; do not add remote/large-buffer downgrades
  unless explicitly requested.
- Org roots default to `~/HC/Org/`; changing them requires checking capture,
  Typst notes, and bibliography paths.
- Org auto-insert is intentionally disabled by default to avoid capture/roam
  template collisions.
- Only route a language through Jupyter when it has a working Jupyter kernel or
  a valid existing-kernel connection workflow.
- Keep `org`, Jupytext/notebooks, and reusable `src/` code as separate sources
  of truth as described in `docs/research-notes-workflow.md`.

Browser/Appine:

- `browse-url` defaults to a backend menu whose default choice is xwidget.
- EWW is for text/reading.
- EWW and `xwidget-webkit` route opens should create/show separate browser
  buffers, not replace the current editing buffer or an already visible browser
  buffer.
- `xwidget-webkit` is the default real-webpage backend.
- Appine is available for native embedding, but should not be the default route
  while its native overlay is unstable outside the host buffer.
- macOS `open` is the system handoff for files and URLs.
- Keep browser routing manual; do not add "smart" URL auto-routing unless
  explicitly requested and documented.

Projects/remote/terminal:

- Project workflow is Projectile + Perspective + Transient + Treemacs/show-imenu.
- Project behavior belongs mostly in `lisp/init-project.el`; project shortcuts
  live in `lisp/init-evil.el`.
- Treat local files as target `local` and keep buffer/project identity in
  canonical `/fs:TARGET:/path` form.  `/ssh:` and `/rpc:` are compatibility
  inputs, not identities for new code.
- New custom file, process, executable, LSP, and environment integrations must
  use the APIs in `docs/remote-framework.md`.  Do not infer policy by parsing
  TRAMP method strings and do not expose physical backend paths outside a
  backend or explicit OS boundary.
- Treat SSH, FRP, Tailscale, jump hosts, and forwarding as ordered pipeline
  stages; treat native, TRAMP, and tramp-rpc as execution backends.  Do not
  collapse these two layers back into a single method string.
- Network operations without a file-name argument must use the explicit
  `remote-channel` APIs.  An unsupported remote channel must fail rather than
  silently opening a socket on the client machine.
- Keep `lisp/remote/` framework-only.  Consumers such as direnv, Eglot, Lean,
  Aaronote, terminals, and project tools stay in their owning modules and
  register adapters or environment maintainers.
- PATH must remain target/workspace isolated and ID-addressable.  Model changes
  as inherited maintainer/decorator layers; do not mutate global `exec-path` or
  reuse one remote target's environment for another.
- Prefer `remote-make-process`, `remote-process-file`, `remote-exec`, and
  `remote-executable-find` for custom processes.  Let ordinary Emacs file APIs
  flow through the `/fs` handler.
- SSH/TRAMP remains the compatibility foundation and fallback.  Do not silently
  strip features from remote buffers; route faster adapters such as tramp-rpc
  only when their capability and trust checks pass.

## Documentation Rules

Update docs in the same change when user-visible behavior changes.

- Daily workflow and keys: `docs/daily-usage.md`.
- Architecture and backend behavior: `docs/dev-guide.md` or a focused workflow
  doc.
- "Where should I change this?" guidance: `docs/settings-cookbook.md`.
- Bootstrap, locks, recovery, state, vendored Elisp: `docs/maintenance.md` and
  `docs/migration.md`.
- Top-level summary and highlights: `README.md`.
- Additions to `docs/` must also be linked from `docs/README.md`.

Do not leave docs describing old keybindings, old package flows, old state
paths, or removed routing behavior.

## Verification

For narrow Elisp edits, at minimum run:

```sh
emacs --batch -Q --eval '(with-temp-buffer (insert-file-contents "path/to/file.el") (emacs-lisp-mode) (check-parens))'
git diff --check
```

When the file requires local load paths, include them:

```sh
emacs --batch -Q -L lisp -L site-lisp/general.el --eval '...'
```

For behavior wrappers, add a focused batch smoke test when practical. For
package/bootstrap/state work, use the `make` targets documented in
`docs/maintenance.md` and `docs/migration.md`.


如果开发中有对项目整体性有影响的设计,需要会过来维护这个文档,使得项目可以被长期,统一,持续的维护.

保证每次修改git都是赶紧的,你实现完成后询问用户时候提交,通过则自动add + commit
