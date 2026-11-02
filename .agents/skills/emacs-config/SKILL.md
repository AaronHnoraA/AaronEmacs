---
name: emacs-config
description: Change or explain the Emacs configuration in ~/.config/emacs: where code and settings live, its APIs, its CLI entry points (bin/emacs-app, emacsopen, make) and development rules. Not for general Emacs, Noema's code (noema-development) or using Noema (noema-emacs-workflow).
---

# Emacs configuration

Repository: `~/.config/emacs` (`~/.emacs.d` links to it). This file is only a
router. Open the one row your task needs; every rule lives in the file it
names, and that file wins over anything remembered.

## Where things are

| Need | Read |
| --- | --- |
| Load order, layers, which directory owns what | `docs/architecture.md` “加载链与分层”, “目录职责” |
| Modules: `aaron!`, categories, `lisp/CATEGORY/NAME/`, naming, `set-…!` interfaces, module board and doctor | `docs/design/module-system.md`; `AGENT.md` “Project Model” |
| Where a given change belongs | `docs/architecture.md` “改动落点速查”, then `docs/guide/settings-cookbook.md` |
| Which document covers a topic | `docs/README.md` |
| Emacs ↔ Noema boundary | `docs/architecture.md` “Emacs ↔ Noema 边界” |

## APIs

| Area | Read |
| --- | --- |
| Files, processes, environment, channels, workspaces, terminals (`remote-*`) | `docs/design/remote-framework.md` §3, §7, §8; rules in `AGENT.md` “Remote-First Core Architecture” |
| Temporary machines, rsync transfers | `docs/design/remote-files.md` |
| Language servers (`set-language-server!`, Hub, Doctor) | `docs/design/lsp-workflow.md`; `AGENT.md` “Domain Policies” → LSP |
| Jupyter kernels and the Noema bridge | `docs/design/jupyter-workflow.md` |
| User settings (`config-register`, `config-get/set`, `etc/config-store.el`) | `docs/design/config-management.md` “注册语法” |
| Keybindings (`map!`, `define-leader-key!`, general.el, Dirvish keys) | `AGENT.md` “Keybinding Rules”; keys listed in `docs/guide/daily-usage.md` |
| Windows, buffers, closing and fallback (Portal, Noema pages, popups, frames) | `docs/design/window-lifecycle.md` |
| Existing entry points not to duplicate | `AGENT.md` “Do Not Reinvent Existing Surfaces” |
| Performance diagnostics | `AGENT.md` “Performance Diagnostics” |
| Shared agent sessions, Compose, context, worktrees and quiet notifications | `docs/design/agent-management.md`; `lisp/tools/agent/` |
| Pi: private runtime and config, Skill catalog; Project cache and memory APIs for Agents | `docs/design/pi-sessions.md`; `docs/design/agent-management.md` C50–C52 |
| Noema's Elisp API | `docs/noema/design/elisp-api.md` |

## Commands outside Emacs

`docs/architecture.md` “命令行入口” lists every `bin/` command
(`emacs-app`, `emacsopen`, …) with its spec; `make help` lists the make
targets, described in `docs/ops/maintenance.md`.

To show the user a file, prefer `emacsopen -o FILE` (alias `emacs-open`), keeping
the Agent chat in its own window. Pi has no shell: use
`noema:api:emacs:open` with `[{"file":"/absolute/path","placement":"other"}]`;
`noema-editor-interaction` describes the editor APIs. Explicit user placement wins.
For CLI options see `emacsopen --help`; do not call
`emacsclient` yourself. With no placement it opens in the selected window,
reusing a visible window already showing that file. `-o` uses the other window
(splitting right if needed) and also reuses a visible file window. `-l`, `-r`,
`-a`, `-b` always split the selected window in that direction; `-f` makes a
new frame. Use `-n` only when the user's current window and focus should stay
as they are. `+LINE[:COL]` before a file, or `FILE:LINE[:COL]`, requests a
position. The command prints one JSON result per file, including the buffer,
window, frame, reuse and positioning result; check it for errors.

## Development rules

A change meant to keep behaviour is proved with `make state-baseline` /
`make state-check` (and `test-baseline` / `test-check` for wide changes):
`AGENT.md` “Verification”.

`AGENT.md` sections, read the ones your change touches: “Project Model”
(adding a module, naming), “Package Rules”, “Runtime State”, “Resource
Lifecycle Rules”, “Runtime Builds”, “Domain Policies”, “Verification”.
Commit only your own changes; the tree often holds someone else's work.

## Fixing a bug or changing behaviour

Read the owning `design/` document's 契约 (`C…`) and 已知差距 (`G…`) first and
work to them; `docs/README.md` “Changing behaviour through the documents” is
the loop. If the behaviour is not described there, stop and record it as a gap
rather than inventing the design.

## Documentation rules

`AGENT.md` “Documentation Rules”, and the table at the top of
`docs/README.md`: structural docs (`guide/ design/ acceptance/ ops/`) describe
the present with no dates; dated history goes to `records/`. Update the owning
doc in the same commit as the behaviour; a new doc takes its row in
`docs/README.md` first.
