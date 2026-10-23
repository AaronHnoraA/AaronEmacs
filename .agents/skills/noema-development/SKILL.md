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

## Agents and Runs

| Need | Read |
| --- | --- |
| Work documents, WorkNodes, Runs, Proposals, Findings (the rules) | `site-lisp/noema/AGENTS.md` D-023, D-038 |
| Run lifecycle: directives → RunSpec → stream → result | `server/lib/research-*.mjs` (`directives`, `runtime`, `run-stream`, `notebook`, `session-routing`); `kernel/noema/research` |
| Proposals and synthesis | `docs/noema/design/research-synthesis.md` |
| ACP agents and sessions in Emacs | `docs/noema/design/codex-session-lifecycle.md`; `lisp/noema-agent-*.el`, `noema-sessions.el` |
| Magent runtime | `docs/noema/design/magent-runtime.md`; `lisp/noema-interaction-magent*.el` |
| Composition and agent entry points | `docs/noema/design/interaction-plan.md`; `lisp/noema-compose.el` |
| Agent context selection, budget, memory and receipts | `docs/noema/design/agent-context.md`; `lisp/noema-context.el`, `server/lib/research-runtime.mjs` |
| Project memory | `docs/noema/design/hy-memory.md`; `server/lib/research-memory.mjs` |
| What a Run is given (Skills, packs, MCP) | `docs/noema/design/capabilities.md`, `mcp.md` |
| The guided workflow from the user's side | `docs/noema/guide/research-workflow.md` |

## APIs

| Surface | Read |
| --- | --- |
| Elisp semantic API (`noema-current-*`, `noema-create-work`, …) | `docs/noema/design/elisp-api.md` |
| Host HTTP/SSE endpoints | `server/Features/<Feature>/api.mjs`; placement rule in `docs/noema/architecture.md` “改动落点” |
| MCP servers | `docs/noema/design/mcp.md` |
| Skills, packs, capability resolution | `docs/noema/design/capabilities.md` |

## Showing a Noema file to the user

Use `emacsopen` to open a requested document in the running Emacs. The Emacs
configuration's `emacs-config` Skill, “Commands outside Emacs”, and
`emacsopen --help` own its placement options: default selected window, `-o`
other window, `-l`/`-r`/`-a`/`-b` directional splits, `-f` new frame, `-n`
leave focus alone, and `+LINE[:COL]` for a position. Default and `-o` reuse a
visible file window; explicit splits and new frames create one. Check the JSON
result. Do not open a file for the user by calling `emacsclient` directly.

## Fixing a bug or changing behaviour

Read the owning `docs/noema/design/` document first and work to what it says;
the loop is “Changing behaviour through the documents” in
`~/.config/emacs/docs/README.md`. Behaviour the document does not describe is
recorded there as a gap before it is changed. Window and close behaviour of
Noema pages inside Emacs is `docs/design/window-lifecycle.md`.

## Finishing a change

Update the owning `docs/noema/` document in the Emacs repository (no dates or
change logs there; a new syntax takes its row in `syntax.md`). Tests are
listed in `site-lisp/noema/CLAUDE.md`; `make research-test` and `make
agenda-test` from the Emacs root cover Noema's Elisp. A renderer change needs
`make noema-web-build` before the running Emacs shows it.
