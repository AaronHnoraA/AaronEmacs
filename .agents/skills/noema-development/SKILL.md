---
name: noema-development
description: Develop or change Noema — the CM6 editor pages, Node host, Go kernel, .noema research model, agents and capabilities, and their Emacs side. Use for design decisions and code changes in ~/.config/emacs/site-lisp/noema; to operate Noema as a user, use noema-emacs-workflow instead.
---

# Developing Noema

Noema is its own Git repository at `~/.config/emacs/site-lisp/noema/`, but its
documentation lives in the Emacs configuration at `~/.config/emacs/docs/noema/`.
A change to Noema is committed in the Noema repository; the documents it
changes are committed in the Emacs repository.

## Read in this order

1. `site-lisp/noema/AGENTS.md` — the product rules (D-023 work documents,
   D-038 Projects, Proposals, Findings, search and reference authority).
2. `docs/noema/architecture.md` — what each top-level directory owns and where
   a change belongs.
3. The owning document in `docs/noema/design/`, e.g. `current-architecture.md`
   (editor and host), `rendered-constructs.md`, `scroll-ownership.md`,
   `capabilities.md`, `wiki-workspace.md`, `agenda-dsl.md`, `elisp-api.md`.
4. `site-lisp/noema/CLAUDE.md` for the editor's core files, widget rules and
   invariants.
5. For anything the editor draws over Markdown source, the
   `rendered-construct` Skill.

`docs/noema/records/` holds dated studies, audits, change logs and incidents:
history, not the current contract.

## Design decisions already made

- Emacs owns the first-party interaction: buffers, navigation, JuText, Graph
  Board, Inspector, approvals and agent sessions. CM6 is an Emacs-hosted
  document surface; Node and Go are headless services.
- Markdown source is the truth for notes. A `.noema` file is an AI work
  document with stable WorkNode identity, never a Jupyter notebook.
- A Wiki repository and a research Project are separate; `[project]` in
  `noema.toml` declares a Project, and visiting a file never creates one.
- Structure changes go through validated, undoable transactions or reviewed
  Proposals. Agent reports are recorded first and applied by the editor.
  Findings need evidence review.
- Search and reference text have one implementation, in the Go kernel. Views
  show its answer and never rank or format again.
- Skills that describe a mechanism ship with the code that implements it;
  packs list flat Skill ids and never copy Skill text.

Do not turn a principle into a new feature, constraint or permission without a
source in `AGENTS.md`, the design docs or the user's direction. `AGENTS.md`
names `DESIGN.md` v1.8 as historical design authority; that file is not
available, so state the gap rather than guessing what it said.

## Finishing a change

- Update the matching `docs/noema/design/` or `guide/` document in the Emacs
  repository; no dates or change logs there.
- Tests: `npm test` (focused files first, per `CLAUDE.md`), `npm run
  build:pages` for type checks, `go test -tags fts5 ./...` under `kernel/`
  for kernel changes, and `make research-test` / `make agenda-test` from the
  Emacs root for Noema Elisp.
- The running Emacs serves `dist/pages`; a renderer change needs
  `make build-web` before it is visible.
