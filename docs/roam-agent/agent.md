# Agent Workspace

`agent/` is the AI-maintained navigation layer for Aaron's Markdown note system. It helps agents find, summarize, and update knowledge without disturbing the human writing workflow.

## Principles

- Markdown files under `roam/` are canonical. Generated index/wiki files must never replace source notes.
- Note identity comes from `#+begin meta` blocks with `id`, `title`, `date`, and `tags`.
- Links come from normal relative Markdown links such as `[Title](../path/note.md)`.
- Keep derived Markdown deterministic and easy to diff.
- Do not commit unrelated user edits.
- Keep `project-overview.md` and `growth-log.md` compact.

## Directory Contract

- `index/`: generated indexes for fast lookup by title, path, tag, link, and backlink.
- `wiki/`: generated condensed note pages for quick AI reading.
- `skill/`: AI-usable scripts and maintenance procedures.
- `develop.md`: temporary development gate and vote ledger for autonomous tooling changes.

## Maintenance Flow

Run:

```sh
make llm
```

Lookup:

```sh
make lookup QUERY="What is a quantum state?"
```

The lookup agent may use `agent/index/` and `agent/wiki/` to find candidate notes, but precise answers must be verified against original `.md` files.

Useful checks:

```sh
python3 agent/skill/maintain.py
python3 agent/skill/check_agent_text_limits.py
```
