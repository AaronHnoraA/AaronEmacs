# Emacs Docs

This directory contains the operational docs for the Emacs configuration. The documents themselves are written in Chinese; this README stays in English so README files remain consistent across repos.

## Start Here

- [quick-start.md](quick-start.md) First-time setup, system dependencies, fonts, path conventions, and bootstrap.
- [daily-usage.md](daily-usage.md) Daily entry points, high-frequency keybindings, and leader-group layout.
- [agenda.md](agenda.md) Aaronnote `@@todo`/`@@project`/`@@clock` syntax, server agenda/project/clock view-model, repeaters, dependencies, and Web/Emacs entry points.
- [slides-demo.md](../lisp/roam/aaronnote/docs/slides-demo.md) Ready-to-open `kind: slides` Aaronnote deck with math and HTML examples.
- [settings-cookbook.md](settings-cookbook.md) “I want to change X” guidance that tells you where each kind of change belongs.
- [config-management.md](config-management.md) Unified `config` registry: one front door (`config-get`/`config-set`, `M-x my/config-board`) to view, edit, live-apply, and persist every registered setting.

## By Workflow

- [project-guide.md](project-guide.md) Project switching, project workbench flow, and how Treemacs / Perspective fit together.
- [typst-math-macros.md](typst-math-macros.md) Shared Typst math macros and matching snippets for TCS, quantum computing, algebra, computing, and physics notes.
- [dev-guide.md](dev-guide.md) Programming, completion, LSP, debugging, terminals, remote work, browser integration, and AI.
- [remote-framework.md](remote-framework.md) `/fs` identity plus target/pipeline/backend/session routing, process and channel APIs, compatibility boundaries, and current implementation gaps.
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
- Want Aaronnote tasks/agenda: [agenda.md](agenda.md)
- Want to change behavior: [settings-cookbook.md](settings-cookbook.md)
- Want the project workflow: [project-guide.md](project-guide.md)
- Want programming / LSP / remote details: [dev-guide.md](dev-guide.md)
- Want maintenance and lock/state guidance: [maintenance.md](maintenance.md)
