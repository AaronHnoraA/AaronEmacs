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
- [slides-demo.md](../lisp/roam/Noema/docs/slides-demo.md) Ready-to-open `kind: slides` Noema deck with math and HTML examples.
- [settings-cookbook.md](settings-cookbook.md) “I want to change X” guidance that tells you where each kind of change belongs.
- [config-management.md](config-management.md) Unified `config` registry: one front door (`config-get`/`config-set`, `M-x my/config-board`) to view, edit, live-apply, and persist every registered setting.

## By Workflow

- [project-guide.md](project-guide.md) Project switching, project workbench flow, and how Treemacs / Perspective fit together.
- [typst-math-macros.md](typst-math-macros.md) Shared Typst math macros and matching snippets for TCS, quantum computing, algebra, computing, and physics notes.
- [dev-guide.md](dev-guide.md) Programming, completion, LSP, debugging, terminals, remote work, browser integration, and AI.
- [remote-framework.md](remote-framework.md) Core `/fs` identity plus target/pipeline/backend/session routing, process and channel APIs, compatibility boundaries, and current implementation gaps.
- [remote-parity.md](remote-parity.md) VS Code Remote-level acceptance matrix, current coverage, completion criteria, and staged roadmap.
- [research-notes-workflow.md](research-notes-workflow.md) Division of labor between notes, Jupytext notebooks, Jupyter, and reusable source code.
- [lsp-workflow.org](lsp-workflow.org) Language-server routing, Hub/Doctor tooling, and the maintenance model.
- [neopyter-protocol-notes.md](neopyter-protocol-notes.md) Neopyter JupyterLab bridge: wire protocol, RPC method registry, architecture, and Emacs client design.

## Maintenance

- [maintenance.md](maintenance.md) Package management, lock workflow, state directories, cleanup, troubleshooting, and maintenance cadence.
- [migration.md](migration.md) New-machine setup, restore workflow, and the migration lessons learned from this configuration.
- [aaronnote-xwidget-audit.md](aaronnote-xwidget-audit.md) Full-chain stability, HCI, and security audit of the Emacs ↔ xwidget ↔ aaronnote bridge.

## Shortest Path

- Want to install it: [quick-start.md](quick-start.md)
- Want keybindings: [daily-usage.md](daily-usage.md)
- Want Noema tasks/agenda: [agenda.md](agenda.md)
- Want to change behavior: [settings-cookbook.md](settings-cookbook.md)
- Want the project workflow: [project-guide.md](project-guide.md)
- Want programming / LSP / remote details: [dev-guide.md](dev-guide.md)
- Want maintenance and lock/state guidance: [maintenance.md](maintenance.md)
