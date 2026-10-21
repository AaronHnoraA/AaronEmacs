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
  browsers, Portal, and AI integrations.
- `docs/remote-framework.md` is authoritative for logical `/fs` paths,
  target/pipeline/backend/session routing, routed processes and channels, and
  environment/PATH capsules.
- `docs/remote-files.md` covers temporary machines and the rsync file
  manager built on that framework.
- `docs/remote-parity.md` defines what “VS Code Remote-level” completion means;
  a registered capability without local, remote, and resilience tests is not
  complete.
- `docs/lsp-workflow.org` covers the lsp-mode route model, Hub, Doctor, and how
  to register target-aware language servers.
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
live under `lisp/`; language-specific modules live under `lisp/lang/`; the
Noema/Jupyter bridge lives under `lisp/roam/`.

`.noema` files are D-023 AI prompt/workflow documents, not Jupyter notebooks.
Their work blocks run through the ACP agent boundary and persist the latest
reply in nbformat outputs; they have no kernelspec, programming code cells,
Result cells, kernel controls, or Run All. The Jupyter bridge below applies to
ordinary `.ipynb` and Markdown `@@cell` sidecars. Node also enforces this
boundary if a generic Jupyter command is invoked from a `.noema` buffer.
JuText therefore has no LSP client: Company routes work control directives to
Noema's project capability CAPF and ordinary prose to dabbrev; Yasnippet is
manual there. Flymake shows Noema's existing structural validation, and RaTeX
previews prose math while skipping controls and code examples.

A Noema Project is not a Git or `project.el` concept (Noema D-038).
`noema.toml` may declare a Wiki repository (top-level `repository_id`, written
into every vault Git repository) and, independently, a research Project (a
`[project]` table).  The Project root is the nearest manifest with `[project]`
(`noema-project-root`); a repository manifest alone never makes one, so a vault
can hold many Projects.  A Project's agents run in its optional `workspace`
(`noema-project-workspace`), which may be a code repository elsewhere.
Resolve through `noema-project-root` / `noema-project-scope` rather than adding
another fallback chain, and hand the Noema host native paths only
(`noema-project-client-path` projects `/fs:`).  Visiting a `.noema` never
creates a Project, because previews and programs visit files too.  Creating a
`.noema` or running a work block goes through `noema-project-ensure`, which
asks first, offering the file's directory and its ancestors: a code
repository's root is proposed, a vault's is not.  Do not add silent
`noema-project-enable` calls to visit paths.  The same project boundary
authorizes an ordinary native `.ipynb` outside the Noema notes root: Jupyter
requests carry `projectRoot`, and only explicit commands (never visit,
refresh, save, or completion) may call `noema-project-ensure` for such a
notebook.

When adding a module:

- Put it in the owning directory.
- End it with `(provide 'init-foo)`.
- Add it to `lisp/init-modules.el` in dependency order.
- Use `declare-function` for cross-module calls when byte compilation needs it.
- Keep application-local names under the existing `my/` convention; hook
  helpers usually use a `*-h` suffix.  The public remote framework is the
  deliberate exception and uses the `remote-*` namespace without `my/`.
- For stable user-facing `M-x` entry points other than Transient menus, add a
  short purpose-first alias in the owning module (for example `config-health`
  or `jupyter-board`) while retaining the `my/` implementation name for
  existing callers. Do this selectively for top-level commands, not every
  helper or generated command. Define public Transient prefixes directly under
  their searchable names: `defalias` does not carry a prefix's registered menu
  layout to the alias. Keep commands, help text, and daily-use docs aligned.

## Remote-First Core Architecture

The Remote framework is core infrastructure for this repository, not an
optional feature used only by SSH buffers.  Any design involving file or
workspace identity, project roots, processes, executable/tool placement,
environment, file watching, services, terminals, sockets, streams, or port
forwarding — including a design that is likely to need remote support later —
must start from the contracts in `docs/remote-framework.md`.

Keep these rules:

- Model the client machine as target `local`.  Local and remote are two inputs
  to the same public API, lifecycle, and consumer code path; they are not two
  implementations for consumers to maintain.
- Consumer modules must not select behavior with `(equal target "local")`,
  `file-remote-p`, a TRAMP method, or a backend ID.  Differences in physical
  placement and capability belong in target, pipeline, backend, or explicit
  client-boundary implementations.  `file-remote-p` is acceptable only at an
  Emacs/TRAMP compatibility boundary, not as product policy.
- Preserve native Emacs compatibility.  Ordinary local buffers may keep native
  file names, while project/workspace/LSP framework boundaries canonicalize
  identity to `/fs:local:`.  Keep file interception scoped to `/fs:` and prefer
  decorating native APIs over globally advising or replacing them.
- If a consumer cannot express a required operation, extend the generic
  `remote-*` contract and implement native plus remote backends.  Do not hide a
  missing framework API behind a language- or package-specific remote branch.
- Connection, pipeline, session, service, watch, and channel ownership belongs
  to the framework.  Consumers register intent and recoverable resources; they
  do not own ad-hoc SSH processes, relay ports, or reconnect loops.
- Per-buffer or per-file consumers (VC, probes, auto-revert, watchers) decide
  from the backend's declared costs — `remote-file-operation-cost` and
  `remote-file-watch-cost` — never from `file-remote-p`.  A new backend must
  declare `:file-operation-cost`, `:file-watch-cost`, `:mtime-compare` and
  `:process-write-cost` (`remote-process-write-cost`).
- An explicit disconnect (Remote board, `tramp-cleanup-connection`,
  `tramp-cleanup-all-connections`) goes through
  `remote-workspace-disconnect-target` and must never be reported as a
  transport failure, which would auto-reconnect.  `file-remote-p`'s CONNECTED
  query on `/fs:` reflects the in-memory session pool; background consumers
  use it to avoid reopening a closed target.  Explicit disconnect closes all
  target-owned file, Dired and process buffers, preserving unsaved edits and
  kill-query vetoes with `remote-buffer-disconnected-p` set.  Deferred consumers
  must respect that flag.  Internal backend session replacement must bypass
  user-level teardown so transport recovery preserves editing buffers.
- A projected environment capsule makes `process-environment` and `exec-path`
  buffer-local.  Capture them before `with-temp-buffer`/`with-current-buffer`
  and rebind inside, or target processes silently fall back to the login PATH.
- Transport recovery replaces a workspace resource's handle, and closing a
  workspace or disconnecting its target releases resources without telling
  their consumers.  A consumer that keeps a resource reads the current value
  from its workspace by a stable key, or asks
  `remote-workspace-resource-live-p` before trusting its own record; it never
  caches the first handle.  A consumer that ends a resource itself also takes
  it off the workspace, which otherwise recovers it on every reconnect.
- A terminal neither waits for nor triggers an environment capsule build, and
  never opens a session to learn its shell: `remote-path-login-shell` asks only
  where the route already has one, otherwise the target execs `$SHELL` inside
  the PTY session.  Hot-path caches count the backends that provide a
  capability, not the backends on a link.
- Client code is loaded with `remote-require-on-client`.  A package required
  from a remote `default-directory` sends its load-time file operations to the
  target; `module-load` alone opens a file session there.
- On a backend that declares `round-trip` file operations, a helper folds its
  bookkeeping into the one command it runs anyway (stderr, PATH search) instead
  of adding file operations around it.  Measure on a high-latency target
  before calling a remote path fast; a LAN host hides every extra round trip.
- A terminal frontend that removes its buffer when the process ends registers
  `:disconnect-function` with `remote-terminal-adopt`, so a lost transport
  keeps the buffer for `remote-terminal-restart`.  Whether an exit can be a
  transport loss is decided by the process being routed, never by target ID
  or frontend name.
- `list-system-processes` and `process-attributes` are file-name-handler
  operations keyed by `default-directory`.  Code that asks about this
  machine's processes, above all from a timer, uses
  `remote-client-list-system-processes` / `remote-client-process-attributes`;
  otherwise a remote current buffer sends the question to the target, into a
  connection that may still be opening.  An automatic start (LSP and the like)
  never asks a minibuffer question; only an explicit command may.
- Failure handling follows the matrix in `docs/remote-framework.md`.  A
  connection attempt is synchronous, so nothing may wait on a link that is
  known to be down: one framework connection is one bounded attempt, an
  unreachable link is held for every capability and backend that shares it,
  and waiting for a host to return is done by an asynchronous probe, never by
  dialing in a loop.  A new consumer that walks ancestor directories registers
  its marker names and calls `remote-prefetch-ancestor-markers` first.
- Targets that do not come from `etc/remote.json` register through
  `remote-config-source-functions`, inside the staged load, so a reload keeps
  them and their sessions.  Temporary machines (`remote-machines.el`) are
  runtime state under `var/remote/`, stored passwords included; never move
  them into `etc/` or write a credential anywhere else.
- `remote-canonicalize-file-name` resolves a bare absolute path against the
  current buffer: inside a target's buffer `/Users/x/` names a folder of that
  target.  A command that means this machine resolves the path from a client
  directory (as `remote-files--client-directory` does) and tests it from a
  buffer of another target.
- A client-side bulk tool (rsync) addresses a folder by placement only:
  a plain path when `remote-client-file-name` answers, otherwise the backend's
  `:client-shell`.  Go through `remote-transfer` rather than building an SSH
  command in a consumer.  Such a tool uses a connection of its own, never the
  editing session's control socket, and a backend offering neither is refused,
  not served from the client's disk.  Transfers end with their target and
  are not rerun by transport recovery.
- A feature must not need a change to the operating system: no kernel or
  system extension, no lowered boot security policy, no privileged install.
  Use tools that run as the user, and drop a capability rather than depend on
  one that does not (sshfs/macFUSE mounts were removed for this reason).
- A feature is not complete merely because the local path works or a capability
  symbol exists.  Apply the local/remote/resilience criteria in
  `docs/remote-parity.md`, and add parity tests for every new shared contract.

## Runtime Builds (GNU Emacs and Neomacs)

This configuration runs on GNU Emacs 31 and, in principle, on Neomacs, which
tracks the GNU Emacs 31.1 Lisp layer but not its C/Objective-C runtime.  The
Neomacs migration is paused at 0.0.18 and Neomacs is not installed; keep the
rules below when touching the boundary so the work stays reusable.

- `lisp/init-neomacs.el` is the only module allowed to know which build is
  running.  It exposes `my/neomacs-p` and is loaded right after
  `init-macos`.  Consumers must not test `my/neomacs-p`, `window-system`,
  `(featurep 'ns)`, or `native-comp-available-p` to pick behaviour; extend the
  compatibility layer instead.  The one accepted exception is a genuinely
  build-specific command such as `+mac-swap-option-and-command`.
- Neomacs has no `mac-*-modifier` variables and no Hyper bit at all: its input
  bridge hard-codes Option to Meta and Command to Super.  The `H-` layer is
  restored by renaming modifiers in `key-translation-map`, not by rewriting
  bindings.  Keep new keybindings written for the GNU layout (Option as Hyper,
  Command as Meta).
- Neomacs has no native compilation.  Anything that fans out over
  `num-processors` needs a cap that is automatic on interpreted builds and
  inert on natively compiled ones; `my/elisp-elsa-worker-limit` is the pattern.
- Changes must stay a no-op on GNU Emacs.  Verify both builds before claiming
  a fix, and record new differences in `docs/neomacs-compat.md`.

## Do Not Reinvent Existing Surfaces

This config already has maintenance and workflow entry points. Reuse them:

- LSP: `my/register-language-server`,
  `my/register-language-server-feature`, `my/language-server-manager`, and
  `my/language-server-doctor`.
- Jupyter: the Noema bridge under `lisp/roam/init-aaronnote-jupyter-*.el` —
  `runtime` (kernelspec discovery, target process, connection file, the
  five-channel forward group), `server` (remote Jupyter servers over HTTP(S),
  `my/noema-jupyter-servers`), `cell` (the `.cell/` script mode, kernel
  completion and inspection), and `lsp` (kernel-following lsp-mode); plus
  `my/jupyter-board` for kernelspec management and Remote Doctor for
  diagnostics.
- Projects: `project-menu`, `my/project-switch`,
  `my/project-open-workbench`, `my/project-search-paths`, Projectile,
  Perspective, Transient, and `show-imenu`.
- Remote files: `remote-files` is a Dired/Dirvish menu over `remote-transfer`,
  not a screen: the other side is picked in a pop-up Dired, and jobs are
  claimed as tasks by `my/activity-board` (queue detail in
  `remote-transfer-list`).  `remote-machines` holds hand-entered targets.  Do
  not add another uploader, sync command, host list or progress indicator.  See
  `docs/remote-files.md`.
- Compile/maintenance: `my/compile-board`, `config-compile-menu`, `make`
  targets documented in `docs/maintenance.md`.
- Performance diagnostics: `my/performance-watch`,
  `my/performance-report-string`, `my/performance-snapshot`,
  `my/performance-hook-snapshot`, and `my/performance-org-buffer-snapshot`.
  Prefer these APIs over asking for screenshots when investigating runtime or
  Org performance.  In the watch buffer, `y` copies the full current report.
- Background application: `bin/emacs-app` is the single entry point for
  running Emacs as a long-lived background app on macOS (start, restart,
  kill, show, hide, status, state, quit, whitelisted actions, frame list and
  actions, login agent).
  The Raycast extension (`tools/raycast-emacs`) is the only control surface
  and only calls it; nothing else speaks `emacsclient`, and every call is
  bounded by a timeout.  The extension has one command and names no Emacs
  action itself: it shows `my/background-actions`, the whitelist in
  `lisp/init-background.el`, and can run nothing outside it.  M-x stays
  inside Emacs; do not bring back an external M-x or an Emacs popup frame.
  `act` and `frame` return a short-lived receipt; `result` queries completion
  without resending the action.  The launcher checks the protocol version in
  the same Emacs eval before queueing, so a newer Raycast cannot enqueue an
  unqueryable action in an older Emacs.  Raycast waits only a bounded time, then
  leaves Emacs responsible for prompts and later errors.
  `lisp/init-background.el` owns visibility, the Dock policy, frame drag
  strips and the action result table; native sources live in
  `tools/macos-background/` and build into `var/macos-background/`.
  Background means invisible frames, not a hidden application.  Only a
  background Emacs leaves the Dock: macOS ties the Dock icon, Cmd-Tab and the
  main menu together, so a visible Emacs stays a regular application.
  `make-frame` resets the policy, so it is reapplied from
  `after-make-frame-functions`.  Closing the only visible frame hides Emacs
  instead of erroring or quitting (narrow advice on `delete-frame` and
  `handle-delete-frame`).  This Emacs is expected to run for weeks and to be
  started, restarted and quit from Raycast.  The server answers before the
  initial frame exists, so anything a launcher asks for at startup must wait
  for `window-setup-hook` (`my/background-start-hidden`).  A frame opened for
  one resource owns it: closing the frame ends the resource, hiding keeps it
  (`my/ghostel--delete-frame-terminal-h`).  A long-lived helper that would
  survive a crashed Emacs must watch its owner's pid, as the Noema web host
  does through `NOEMA_HOST_OWNER_PID`; exit hooks alone do not run then.  yabai does not manage Emacs at all (`manage=off`
  for the application), so frames place and size themselves.
- Noema from outside Emacs: the Raycast entry has a Noema section, `Find Note`
  and `Noema Maintenance` (also installed as commands of their own, for a
  hotkey), and they do not go through Emacs at all.  Finding a page is
  therefore not in `my/background-actions`: an entry there would bring Emacs
  forward to do what Raycast already shows.  They read
  `var/aaronnote/host.json`, written by the running Noema host, and call its
  loopback `/api` (`tools/raycast-emacs/src/backend.ts`): search is answered
  by the Go kernel, maintenance (index refresh and rebuild, repository
  checkpoint and sync) by the host's own services.  Raycast is then the whole
  front end and no Emacs frame opens; only opening the chosen page goes
  through `bin/emacs-app act`, looked up in the whitelist by the action that
  takes a path.  Keep it that way round: Emacs control goes through
  `bin/emacs-app`, Noema data and maintenance go to the Noema backend, and
  neither is reimplemented in the extension.
- Page search and references: one implementation, the Noema kernel's
  (`site-lisp/noema/kernel/noema/wikisearch`).  `lisp/roam/init-noema-search.el`
  sends the typed text and shows the answer through the completion style
  `my/noema-search`, which neither filters nor sorts; do not complete over a
  local page list, and do not put the answer through orderless or flex, which
  drop what the kernel matched fuzzily.  Link text comes from the kernel too
  (`my/noema-insert-reference`, `M-r` in a Noema pane).
- Templates: the built-in `auto-insert` template layer in
  `lisp/init-auto-insert.el`; do not revive the old Doom/Yasnippet file-template
  path.
- Noema note templates are a separate catalogue in
  `site-lisp/noema/resources/templates/noema/`, exposed to Emacs through
  `templates/noema/`.  The runtime scans the files and creates notes; the
  Emacs Roam workbench reads that catalogue instead of keeping a second list.
  Use `# type: roam` or `# type: regular` to scope a template to graph nodes
  or Markdown documents excluded from the graph; keep the two results distinct.
- LaTeX math preview: RaTeX, configured in `lisp/lang/tex/init-latex.el`, with
  `my/latex-preview-doctor` as the diagnostic entry point.  Math macros and TeX
  compatibility rules have one source, `site-lisp/noema/resources/`, which the
  Emacs preview and Noema's KaTeX renderer both read through the relative links
  in `etc/` and `templates/`.  Do not add a second macro table, a second
  preamble, or a fourth parser for that subset — extend the shared files and
  the `shared/katex-macro-fixtures.json` contract instead.  On top of the
  shared files sit three user layers, later winning: a note's meta
  `macros-from:` project files, the user's global
  `~/.config/noema/katex-macros.tex` (managed in Noema's formula palette, also
  read by RaTeX) and the note's `#+begin macros` block.  They reuse the same
  parser; do not add another store for personal macros.  See
  `docs/latex-preview.md`.
- Agent Skills: the Noema capability resolver and its manager
  (`noema-skill-manager`).  Global Skills live flat in the vault library
  `my/noema-skills-directory` (`NOEMA_GLOBAL_SKILLS`), not in `etc/`;
  Skill packs (`packs` in `etc/noema/capabilities.json`, `@@pack(id)`) are
  link lists over them, patched per project by membership deltas only; and
  `skills.lock.json` beside that library is the single upstream record, maintained by
  `noema-skill-upstream` (install/check/diff/update/rollback).  Do not copy a
  Skill into a pack, hand-edit a locked Skill in place, or add another
  installer.  See `site-lisp/noema/docs/capabilities.md`.
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
  `lisp/init-evil.el` for shared prefixes, or the owning module for a domain
  suffix.  Do not bind a suffix already owned by another module: load order
  silently replaces the earlier binding.  Verify the resolved key after full
  startup and update `docs/daily-usage.md` with the same command.
- For localleader bindings, use the existing localleader helpers.
- For global non-leader bindings, use `general-define-key`.
- For concrete mode keymap objects that only exist after a package loads, bind
  inside `with-eval-after-load`; `evil-define-key*` is acceptable there.
- Dirvish single keys mirror `~/.config/yazi/keymap.toml` and have one
  source, the table in `lisp/init-dirvish-keys.el`.  Add or move a Dirvish key
  there, not in `init-windows.el`, and keep `my/dirvish-key-hints` (the `?`
  sheet) and the table in `docs/daily-usage.md` in step.
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

- `lsp-mode` is the only supported language-server client.  Eglot is built in
  but its commands are intentionally hidden so a second unmanaged client is
  not started accidentally.
- File-backed programming buffers auto-start only when a registered client and
  target-side executable are available.  Manual ensure reports missing tools;
  automatic startup stays quiet.
- Custom clients go through `my/register-language-server`; package-owned
  clients such as JDTLS are declared through
  `my/register-language-server-feature`.
- LSP is the strictest consumer of the Remote-first rule.  One logical
  workspace root and target context must determine document URIs, server
  process placement, executable lookup, environment/toolchain, file watchers,
  helper services, and network channels.  Never derive any of these from an
  unrelated current buffer or client-side `default-directory`.
- lsp-mode startup must use the shared `language-server` remote
  adapter.  Server placement defaults to the active target/workspace; resolve
  executables from its environment and never embed `/ssh:` or `/rpc:` paths in
  contacts.  Mark only genuine client-side UI helpers as client placement.
- Language modules must not maintain separate local and remote server choices
  or PATH setup.  Express tool availability as target capabilities or
  toolchain preferences and let the selected backend project them.  Any
  unavoidable lsp-mode compatibility workaround stays in the shared adapter
  boundary and must also be tested with target `local`.
- URI conversion must be anchored to the owning lsp-mode workspace.  A
  callback running in another buffer must not change
  the target identity of a document.
- Remote language integrations must preserve local feature parity.  For
  multi-process tools such as Lean, Node, Lake, workers, and helper ports must
  share the target environment; target-side HTTP/TCP helpers reach the client
  through `remote-channel` forwarding.
- Long-lived LSP watchers, helper services, and channels must be registered
  with their `remote-workspace` owner so reconnect can restore or explicitly
  report them.  Do not claim reconnect support until failure-injection tests
  cover it.
- Give each language-helper process instance its own owned discovery/port file;
  never let concurrent reconnects overwrite a project-global endpoint file.
  Validate local owners and clean up only files owned by the exiting process.
- Keep the layer split: `init-lsp.el` for routing/core setup,
  `init-lsp-ops.el` for backend-agnostic commands, `init-lsp-tools.el` for
  Hub/Doctor/dispatch/session knobs, `init-lsp-runtime.el` for the runtime
  provider layer (kernel-following analyzers etc.), and
  `init-lsp-toolchain.el` for project-level toolchain profiles.
- LSP document/protocol identity (workspace root, URI, server placement,
  diagnostic keys) is always canonicalized to `/fs:TARGET:/path`. Ordinary
  buffer identity is not: ordinary local buffers keep their native
  `buffer-file-name`, and the two are bridged by Emacs's own buffer-alias
  mechanism (`find-buffer-visiting`'s `get-file-buffer` step, which `/fs:`
  registers as a handler operation), not by rewriting `buffer-file-name`.
  Where lsp-mode's raw diagnostics lookup bypasses that alias, add a narrow
  advice that closes the gap instead of normalizing buffer identity into
  `/fs:` globally — see `my/lsp-mode--fix-path-casing-a` in `init-lsp.el`.
- Keep display work bounded: CodeLens, inlay hints, document colors, document
  links, and ranged semantic-token prefetch use visible windows plus
  `my/language-server-visible-render-margin`; protocol state and server caches
  may remain workspace-wide.  Do not turn a server refresh notification into
  buffer-global decoration creation.
- When LSP behaves strangely, use `my/language-server-doctor` before changing
  code.

Org/Jupyter/research:

- Noema Agenda task lists use the host's active-scope source service.
  `noema-agenda` is the searchable native entry; `noema-agenda-web` and the
  named hosted views are distinct entries.  Keep keys and menus on these
  names while `my/` functions own their implementations. Current Markdown
  file navigation uses a read-only snapshot of the editor buffer;
  it must not scan the vault or enter another project. Task writes use the
  versioned scoped owner, including protected-buffer and repeat semantics.
  Do not reintroduce regex or one-shot CLI task scans, direct prefix edits,
  temporary Org sources, or fallback writers after an API failure. See
  `docs/agenda.md` for the source lifecycle and editor snapshot contracts.
- Org is intentionally feature-rich; do not add remote/large-buffer downgrades
  unless explicitly requested.
- Org roots default to `~/HC/Org/`; changing them requires checking capture,
  Typst notes, and bibliography paths.
- Org auto-insert is intentionally disabled by default to avoid capture/roam
  template collisions.
- Only route a language through Jupyter when it has a working Jupyter kernel or
  a valid existing-kernel connection workflow.
- A kernel comes from one of three connectors, named by the cell's kernel
  string: a kernelspec launched by the Remote broker, `attach:<file>` for a
  running kernel reached through its connection file, or `server:<id>:...` for
  one on a Jupyter server/JupyterHub/gateway over HTTP(S).  Consumers must not
  branch on which; lifecycle differences belong in `kernel-registry.mjs`.
- A Jupyter server owned by a non-`local` target must be reached through
  `remote-channel` forwarding.  A target that cannot provide a channel is an
  error — never a client-side connection, which may silently reach a different
  server listening on that port here.
- Keep `org`, Jupytext/notebooks, and reusable `src/` code as separate sources
  of truth as described in `docs/research-notes-workflow.md`.

Noema Markdown panes:

- A Noema Markdown xwidget buffer is an inert placeholder; the CM6 page owns
  the document, selection and history.  An Emacs command that reads or edits
  "the buffer" must either be remapped in `my/noema-keys-mode-map` onto a page
  command, or ask the page for its saved range through `noema-md-bridge`
  (`noema-md-bridge-request` → `noema-md-bridge-handle-selection`) and run on
  the note's ordinary source buffer.  Do not implement text features against
  the placeholder, and never wait synchronously on the page in a command.
- Host chords the renderer forwards (`shouldForwardToEmacs` in
  `aaronnote/xwidget-key-guard.ts`, including Cmd+Arrow → windmove) are
  claimed in the capture phase, before CM6's own keymap; keep new forwarded
  chords on that path.  Every Noema page gets the same keyboard contract:
  the editor composes the primitives itself and every other page entry calls
  `installHostKeyboardBridge` (`aaronnote/host-keyboard.ts`); a test fails if
  a page skips it.  Never special-case one page or one buffer kind.
- A key forwarded from a page follows its command's result: the window the
  command opened or changed gets the keyboard
  (`my/noema--follow-forwarded-command`), including a window shown by a
  process within `my/noema-forwarded-command-grace` while the pane is still
  selected.  Only when nothing changed does the pane get its keyboard back.
  Bridged actions that show a buffer select it.
- Keys follow the native keyboard focus, never a list of keys.  Whenever
  Emacs takes the keyboard from a pane (`my/noema--release-xwidget-input-buffer`)
  the page is told (`host-owns-keyboard`) and hands macOS first responder
  back to the Emacs view through Emacs' own `messageHandlers.keyDown` "C-g"
  (`handOffNativeKeyboard`), so later keys reach Emacs natively and in order.
  A page without native focus (`document.hasFocus()` false) is still offered
  Ctrl/Cmd chords and function keys; `installNativeKeyboardYield`, the first
  key listener on every page, stops them before any page handler without
  preventing their default, so Emacs handles them.  Relaying host-owned keys
  (`handleHostOwnedKey`) is only the fallback for hosts without that handler.
  Emacs cannot type into WKWebView on macOS, so a pane entered from the
  keyboard gets its keys through the always-on recovery map (`key` command)
  until it is clicked.  Sandboxed output iframes run `FRAME_KEY_RELAY_SOURCE`
  (`src/frame-key-relay.ts`), a mirror of the chord and prefix gates kept
  identical by `tests/frame-key-relay.test.ts`; a new srcdoc frame that can
  take focus must include it.
- Pausing a pane releases its editor focus, and a click inside WebKit does not
  select the Emacs window, so Emacs can believe a pane just clicked is in the
  background.  The page refuses a `pause` while it holds the native keyboard
  and Emacs has not announced taking it (`holdsInputAgainstHostPause`), and
  reports `input-focus` instead.  Take the keyboard from a pane only through
  `my/noema--release-xwidget-input-buffer`, which makes that announcement.
- The page's Vim layer (`site-lisp/noema/aaronnote/vim-lite.ts`) takes native
  Vim as its reference, checked against results recorded from Neovim
  (`tests/vim-native-parity.test.ts`); `site-lisp/noema/docs/vim.md` lists the
  few deliberate differences.  Change behaviour by re-recording, not from
  memory.  Outside Insert a key an input method has taken is dropped on every
  path, the xwidget guard included: it is neither a command nor text.
- Commands bound to keyboard keys use `(interactive (list last-command-event))`,
  never `(interactive "e")`, which rejects keyboard events.
- Code that visits a file only to read, index or reference it (Treemacs tags,
  `noema-md-bridge`, agents) binds `my/programmatic-file-visit` to t.  An
  interactive command otherwise triggers the Markdown→Noema redirect, which
  blanks the visited buffer and reopens the page.
- A Treemacs heading jump runs `my/treemacs-tag-visit-functions`; a surface
  that shows the file elsewhere (a Noema pane) moves there instead of letting
  the raw buffer take over the window.

Browser and Portal:

- `browse-url` defaults to a backend menu whose default choice is xwidget.
- EWW is for text/reading.
- EWW and `xwidget-webkit` route opens should create/show separate browser
  buffers, not replace the current editing buffer or an already visible browser
  buffer.
- `xwidget-webkit` is the default real-webpage backend.
- Portal provides native Ghostty terminal panes on graphical macOS Emacs 31.1+;
  keep Ghostel for remote workspaces.
- macOS `open` is the system handoff for files and URLs.
- Keep browser routing manual; do not add "smart" URL auto-routing unless
  explicitly requested and documented.

Projects/remote/terminal:

- Project workflow is Projectile + Perspective + Transient + Treemacs/show-imenu.
- Treemacs outlines keep semantic button depth but render from a shallow fixed
  visual depth, include an `OUTLINE · FILE` owner row, and use LSP SymbolKind
  icons.  Preserve this separation from filesystem indentation.
- Project behavior belongs mostly in `lisp/init-project.el`; project shortcuts
  live in `lisp/init-evil.el`.
- Treat local files as target `local` and keep framework-owned project/workspace
  identity in canonical `/fs:TARGET:/path` form.  Ordinary local buffers remain
  native until they cross that boundary.  `/ssh:` and `/rpc:` are compatibility
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
- Keep `lisp/remote/` framework-only.  Consumers such as direnv, lsp-mode, Lean,
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
