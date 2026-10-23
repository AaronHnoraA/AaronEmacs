---
name: emacs-config
description: Maintain the user's Emacs configuration at ~/.config/emacs — settings, modules, packages, keybindings, the Remote framework, LSP, Jupyter, terminals, background app and its docs. Use for any change to or question about this configuration; not for generic Emacs advice, and not for Noema's own code (use noema-development).
---

# Maintaining the Emacs configuration

The configuration lives in `~/.config/emacs` (`~/.emacs.d` is a link to it).
This Skill only says where things are decided; the rules themselves are in the
repository, and they win over anything remembered.

## Read in this order

1. `AGENT.md` — the rules: load order, Remote-first architecture, keybinding,
   package, state, lifecycle and documentation rules. Read the sections your
   change touches before designing it.
2. `docs/README.md` — the map of every document, and what each directory may
   contain.
3. `docs/architecture.md` — layers, directory ownership, the Emacs ↔ Noema
   boundary, and a table of where each kind of change belongs.
4. The owning document under `docs/design/` (how it works now) or
   `docs/guide/` (how it is used), then the code.

`docs/records/` holds dated audits, incidents and measurements. Read them for
why something is the way it is, never as a description of the current system.

## Before changing anything

- A user setting is usually registered: try `M-x config-board`,
  `config-get`/`config-set`, and `etc/config-store.el` before editing a module.
- Files, projects, processes, executables, environments, terminals, sockets
  and LSP go through the `remote-*` API with the client as target `local`.
  Never branch on `file-remote-p` or a target name in a consumer.
- Reuse the existing entry points that `AGENT.md` lists under "Do Not
  Reinvent Existing Surfaces" instead of adding a parallel one.
- Noema's code is a separate repository at `site-lisp/noema/`; its docs live
  here in `docs/noema/`. For work on Noema itself, use `noema-development`.

## Finishing a change

- Update the matching `docs/design/` or `docs/guide/` document in the same
  commit; a new document first takes its slot in `docs/README.md`. Do not
  write dates or change logs into those documents.
- Verify as `AGENT.md` "Verification" says: `check-parens` on edited Elisp,
  `git diff --check`, the focused `make` test target (listed in
  `docs/ops/maintenance.md`), and `make health-startup` for anything loaded at
  startup.
- Commit only your own changes; the tree often holds someone else's
  unfinished work.
