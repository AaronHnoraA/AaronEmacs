# Emacs Docs

This directory contains the operational docs for the Emacs configuration. The documents themselves are written in Chinese; this README stays in English so README files remain consistent across repos.

## Core Architecture

The [Remote framework](remote-framework.md) is the repository's core execution
model, not an SSH-only feature. Filesystem-, project-, process-, environment-,
LSP-, service-, and socket-aware development must use it from the start, with
the client represented as target `local` rather than a parallel local
implementation. [Remote parity](remote-parity.md) defines completion, and
[the LSP workflow](lsp-workflow.org) applies the rule most strictly to
workspace roots, URIs, server placement, environments, watchers, helpers, and
channels.

## Start Here

- [quick-start.md](quick-start.md) First-time setup, system dependencies, fonts, path conventions, and bootstrap.
- [daily-usage.md](daily-usage.md) Daily entry points, high-frequency keybindings, and leader-group layout.
- [agenda.md](agenda.md) Noema `@@todo`/`@@project`/`@@clock` syntax, server agenda/project/clock view-model, repeaters, dependencies, and Web/Emacs entry points.
- [MarkWright → Noema audit](markwright-noema-audit-2026-09.md) Source-linked comparison of MarkWright with the Noema editor: merged slash-menu, inline HTML and print details; rendering, CSS, theme and lifecycle findings.
- [Marker / MarkText / files.md → Noema audit](markdown-editors-noema-audit-2026-10.md) Source-linked comparison of three Markdown editors with Noema: merged format toggles, CJK emphasis, context-aware paste, code-block input, word-sized undo, range-selection rendering, CRLF save and save retry; with a [per-file line inventory](markdown-editors-line-audit-2026-10.md).
- [Native Agenda integration study](../site-lisp/noema/docs/architecture/agenda-org-integration-study.md) Org Agenda UI reuse over native Markdown/WorkNode data, project-scoped indexing, and explicit Apple global attention; includes an isolated prototype.
- [slides-demo.md](../site-lisp/noema/docs/slides-demo.md) Ready-to-open `kind: slides` Noema deck with math and HTML examples.
- [settings-cookbook.md](settings-cookbook.md) “I want to change X” guidance that tells you where each kind of change belongs.
- [config-management.md](config-management.md) Unified `config` registry: one front door (`config-get`/`config-set`, `M-x my/config-board`) to view, edit, live-apply, and persist every registered setting.

## By Workflow

- [project-guide.md](project-guide.md) Project switching, project workbench flow, and how Treemacs / Perspective fit together.
- [latex-preview.md](latex-preview.md) In-buffer math preview: supported delimiters, the shared macro source Emacs and Noema both read, the doctor, and the troubleshooting order.
- [typst-math-macros.md](typst-math-macros.md) Shared Typst math macros and matching snippets for TCS, quantum computing, algebra, computing, and physics notes.
- [dev-guide.md](dev-guide.md) Programming, completion, LSP, debugging, terminals, remote work, browser integration, and AI.
- [workbench-tools.md](workbench-tools.md) Adopted Xenodium editing, file, GitHub, Bazel, calendar, Org, and macOS workbenches, with keybindings and design boundaries.
- [remote-framework.md](remote-framework.md) Core `/fs` identity plus target/pipeline/backend/session routing, process and channel APIs, compatibility boundaries, and current implementation gaps.
- [remote-io-review.md](remote-io-review.md) `emacs-io` audit, adopted resource ideas, rejected ownership/POSIX shortcuts, and Remote performance criteria.
- [remote-parity.md](remote-parity.md) VS Code Remote-level acceptance matrix, current coverage, completion criteria, and staged roadmap.
- [codex-session-lifecycle.md](codex-session-lifecycle.md) ACP session reuse, Codex Remote ownership limits, and the optional mobile-to-Noema message bridge.
- [remote-performance.md](remote-performance.md) Reproducible local/SSH latency measurements, implemented optimizations, version boundary, and remaining gaps.
- [research-notes-workflow.md](research-notes-workflow.md) Division of labor between notes, Jupytext notebooks, Jupyter, and reusable source code.
- [publish-workflow.md](publish-workflow.md) Personal site: where the hand-written pages live, the `make publish*` targets, the completeness/licence check, and the deploy path.
- [lsp-workflow.org](lsp-workflow.org) Language-server routing, Hub/Doctor tooling, and the maintenance model.
- [jupyter-workflow.org](jupyter-workflow.org) Kernel sources (kernelspec, `attach:`, remote Jupyter servers over HTTP(S)), protocol coverage, Remote routing rules, and the Jupyter Board.
- [jupyter-parity.md](jupyter-parity.md) VS Code alignment register, filesystem placement distinctions, Aaron-PC live evidence, and remaining acceptance work.
- [neopyter-protocol-notes.md](neopyter-protocol-notes.md) Historical: the Neopyter JupyterLab wire protocol. The client was removed; kept as reference only.

## Maintenance

- [neomacs-compat.md](neomacs-compat.md) Running this configuration on Neomacs: the modifier-rename layer, fringe and startup-frame fixes, Elsa concurrency, and why the 0.0.18 migration was paused. Neomacs is not installed; the layer is inert on GNU Emacs.
- [maintenance.md](maintenance.md) Package management, lock workflow, state directories, cleanup, troubleshooting, and maintenance cadence.
- [migration.md](migration.md) New-machine setup, restore workflow, and the migration lessons learned from this configuration.
- [aaronnote-xwidget-audit.md](aaronnote-xwidget-audit.md) Full-chain stability, HCI, and security audit of the Emacs ↔ xwidget ↔ aaronnote bridge.

## Shortest Path

- Want to install it: [quick-start.md](quick-start.md)
- Want keybindings: [daily-usage.md](daily-usage.md)
- Want Noema tasks/agenda: [agenda.md](agenda.md)
- Want to change behavior: [settings-cookbook.md](settings-cookbook.md)
- Want the project workflow: [project-guide.md](project-guide.md)
- Want math preview behaviour or a formula that will not render: [latex-preview.md](latex-preview.md)
- Want programming / LSP / remote details: [dev-guide.md](dev-guide.md)
- Want maintenance and lock/state guidance: [maintenance.md](maintenance.md)
- Running on Neomacs instead of GNU Emacs: [neomacs-compat.md](neomacs-compat.md)
