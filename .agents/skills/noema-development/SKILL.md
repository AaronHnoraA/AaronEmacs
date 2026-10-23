---
name: noema-development
description: Change or explain Noema's implementation in ~/.config/emacs/site-lisp/noema — its source syntax and parsers, the Wiki workspace and search, the kernel's databases and projections, and its Elisp, HTTP and MCP APIs. Use only for work on Noema's code or design; to use Noema as an author, use noema-emacs-workflow or noema-authoring-syntax instead.
---

# Noema

Code: `~/.config/emacs/site-lisp/noema/` (its own Git repository). Docs:
`~/.config/emacs/docs/noema/`, committed in the Emacs repository. This file is
only a router. Open the one row your task needs; the rules live in the file it
names.

## Orientation

| Need | Read |
| --- | --- |
| Product rules (D-023 work documents, D-038 Projects, Proposals, Findings) | `site-lisp/noema/AGENTS.md` |
| What each directory owns, where a change belongs | `docs/noema/architecture.md` |
| Editor, host and kernel as they work now | `docs/noema/design/current-architecture.md` |
| Editor core files, widget rules, test commands | `site-lisp/noema/CLAUDE.md` |

## Syntax

| Need | Read |
| --- | --- |
| Every source form, its parser and its spec | `docs/noema/design/syntax.md` |
| Planning commands (`@@todo`, `@@clock`, …) | `docs/noema/design/agenda-dsl.md` |
| `.noema` work-block directives | `docs/noema/design/capabilities.md` “Work-cell syntax” |
| Drawing a construct over source (widgets, reveal) | the `rendered-construct` Skill |
| Math macros and TeX compatibility | `docs/design/latex-preview.md`; export in `docs/noema/design/latex-export-style.md` |

## Wiki

`docs/noema/design/wiki-workspace.md`: “Layouts”, “Links and identity”,
“Search” (query syntax, reference text), “Namespaces”, “Git collaboration
cadence”, “Publishing boundary”. Search and reference text have one
implementation, `kernel/noema/wikisearch`.

## Database

| Store | Read |
| --- | --- |
| `wiki.db`, the disposable SQLite projection of Git-owned Markdown | `wiki-workspace.md` “Layouts”, “Index maintenance” |
| Agenda, Todo and attribute-view projections, generated only by Go | `current-architecture.md` “Node host”; `kernel/noema/agenda`, `kernel/noema/attributeview` |
| Clock journal `agenda-clocks.sqlite` | `agenda-dsl.md` |
| Research DAG, Proposals, Findings | `AGENTS.md`; `kernel/noema/research` |

## APIs

| Surface | Read |
| --- | --- |
| Elisp semantic API (`noema-current-*`, `noema-create-work`, …) | `docs/noema/design/elisp-api.md` |
| Host HTTP/SSE endpoints | `server/Features/<Feature>/api.mjs`; placement rule in `docs/noema/architecture.md` “改动落点” |
| MCP servers | `docs/noema/design/mcp.md` |
| Skills, packs, capability resolution | `docs/noema/design/capabilities.md` |

## Finishing a change

Update the owning `docs/noema/` document in the Emacs repository (no dates or
change logs there; a new syntax takes its row in `syntax.md`). Tests are
listed in `site-lisp/noema/CLAUDE.md`; `make research-test` and `make
agenda-test` from the Emacs root cover Noema's Elisp. A renderer change needs
`make noema-web-build` before the running Emacs shows it.
