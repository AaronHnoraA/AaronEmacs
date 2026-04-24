# Aaron's Emacs Config

A macOS-first Emacs workstation for local software work, SSH/TRAMP remote editing, Org-based knowledge work, and a tightly integrated terminal/browser/AI toolchain.

This is not a minimal starter kit, and it is not trying to be a zero-assumption cross-platform distribution. It is a long-lived personal system built around one idea: coding, project navigation, remote work, note-taking, research, and publishing should share the same operating surface.

## Positioning

This configuration is designed to solve a concrete set of problems:

- Turn editing, search, project switching, build/test, and Git into a stable daily workflow.
- Keep local development, remote development, and knowledge work inside the same environment.
- Treat Org, LaTeX, Jupyter, citations, and research notes as first-class workflows rather than side features.
- Keep bootstrap, package locks, runtime state, and private local configuration explicit and recoverable.

## Core Capabilities

- Project-first workflow
  `projectile`, `perspective`, `transient`, Treemacs, `show-imenu`, and popup `vterm` are composed into a workspace-oriented project surface.
- Programming and debugging
  The default route is `eglot`, with `lsp-mode` used selectively where needed. Completion, diagnostics, build/test, debugging, tree-sitter, snippets, and code navigation are already wired together.
- Org and research writing
  Agenda, capture, org-roam, LaTeX preview, AUCTeX, Jupyter, citations, and PDF workflows are treated as long-term maintained parts of the system.
- Remote work and terminals
  TRAMP and `my/vterm-ssh` are first-class. The goal is not merely to “support remote files”, but to make remote work part of the default workflow.
- Browser and system integration
  `eww`, `xwidget-webkit`, Appine, and macOS `open` have explicit roles, with manual routing between them when needed.
- AI integration
  `gptel`, Claude Code, Codex CLI, and Copilot are integrated into the editing environment rather than bolted on as separate tools.
- Recoverable dependency management
  Bootstrap, lock export, state backup/restore, and health checks are part of the operational model, so new-machine setup and recovery are treated as normal workflows.

## Design Principles

- One package-management stack
  The configuration stays on `package.el` + `use-package`; it does not layer a second package manager on top.
- Clear module ownership
  Core behavior lives in the owning `lisp/` modules, language-specific behavior in `lisp/lang/`, and workflow documentation in `docs/`.
- Runtime state stays contained
  Runtime and cache data belong under `var/`; local private configuration and secrets belong under `etc/`.
- README is product-facing, docs are operational
  README files describe the system at a product level. Installation, usage, maintenance, and “where should I change this?” guidance live in `docs/`.

## Who This Is For

This configuration fits best if you want an Emacs environment with these characteristics:

- macOS GUI Emacs is the primary target.
- Emacs is expected to cover coding, projects, remote work, note-taking, writing, and research.
- Heavier UI and more external dependencies are acceptable tradeoffs for a more integrated day-to-day workflow.

It is probably not the right fit if your priority is:

- minimal startup surface and minimal dependencies
- Windows/Linux-first, fully uniform cross-platform behavior
- a teaching-oriented starter config for learning Emacs from scratch

## Documentation

Detailed operational documentation lives in [`docs/`](docs/). The documents themselves are written in Chinese; the README files stay in English.

- Doc index: [docs/README.md](docs/README.md)
- First setup and dependencies: [docs/quick-start.md](docs/quick-start.md)
- Daily usage and keybindings: [docs/daily-usage.md](docs/daily-usage.md)
- Project workflow: [docs/project-guide.md](docs/project-guide.md)
- Org inside Emacs: [docs/org-guide.md](docs/org-guide.md)
- Org knowledge base and site workflow: [docs/org-kb-guide.md](docs/org-kb-guide.md)
- Programming, LSP, remote work, terminals, AI: [docs/dev-guide.md](docs/dev-guide.md)
- Configuration cookbook: [docs/settings-cookbook.md](docs/settings-cookbook.md)
- Maintenance, locks, state recovery: [docs/maintenance.md](docs/maintenance.md)
- Migration notes: [docs/migration.md](docs/migration.md)
- Deep-dive workflow docs: [docs/lsp-workflow.org](docs/lsp-workflow.org), [docs/jupyter-workflow.org](docs/jupyter-workflow.org), [docs/research-notes-workflow.md](docs/research-notes-workflow.md)

## Repository Layout

- [early-init.el](early-init.el)
  Early startup optimization and minimal UI setup.
- [init.el](init.el)
  Main entry point for package setup, module loading, and startup integration.
- [bootstrap.el](bootstrap.el)
  Dependency restore/export entry point for the bootstrap workflow.
- [package-lock.el](package-lock.el)
  Generated dependency lock file.
- [lisp/](lisp/)
  Main configuration modules.
- [lisp/lang/](lisp/lang/)
  Language-specific modules.
- [site-lisp/](site-lisp/)
  Vendored local components.
- [docs/](docs/)
  Operational documentation and workflow notes.

## Environment Assumptions

- The primary target is macOS GUI Emacs, currently maintained around Emacs 31.
- Some local paths and workflow conventions are intentionally opinionated, such as `~/HC/Org/`, `etc/mygpt.json`, and `~/.ssh/config`.
- Some features depend on system tools, fonts, and local applications; the repository does not attempt to vendor those system dependencies.
- Vendored code and lock files may belong in Git. Runtime caches, build artifacts, local state, and secrets do not.

## Current Bias

The configuration is deliberately optimized around three main lines of work: local coding, SSH/TRAMP remote work, and Org-based research/writing. It does not aim to be minimal, and it does not try to erase every historical choice. The priority is long-term usefulness, recoverability, and a small number of stable entry points.
