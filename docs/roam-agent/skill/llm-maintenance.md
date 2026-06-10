# Codex LLM Maintenance Task

You are maintaining Aaron's AI-facing layer for a Markdown note system.

Markdown files under `roam/` are canonical. Note metadata uses `#+begin meta` blocks with `id`, `title`, `date`, and `tags`; note links use normal relative Markdown links.

## Current Boundary

Generated `agent/index/` and `agent/wiki/` files may be refreshed with `python3 agent/skill/maintain.py` after source Markdown changes.

## Allowed Work

- Keep `agent/` documentation accurate.
- Keep lookup prompts pointed at original `.md` sources.
- Keep maintenance scripts from referencing org-roam as an active dependency.
- Do not modify human-authored Markdown notes unless the user explicitly asked for note edits.

## Checks

```sh
python3 -B -m py_compile agent/skill/check_agent_text_limits.py
python3 agent/skill/check_agent_text_limits.py
```
