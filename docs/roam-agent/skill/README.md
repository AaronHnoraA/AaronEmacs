# Agent Skills

Reusable AI maintenance tools live here.

Autonomous tooling edits are gated by [develop.md](/Users/hc/HC/Org/agent/develop.md): absent a direct human request, AI should only change maintenance tooling for severe defects or issues that have accumulated enough valid votes, and should clear resolved records after implementation.

- `llm-maintenance.md`: prompt used by `make llm` to delegate targeted maintenance to Codex.
- `lookup.md`: prompt used by `make lookup` for interactive read-only note retrieval with original-source verification.
- `maintain.py`: Markdown index/wiki generator for `roam/**/*.md`.
- `check_agent_text_limits.py`: verifies the 500-character limits for `agent/project-overview.md` and `agent/growth-log.md`.

Run the full maintenance workflow through:

```sh
make llm
```

Run an interactive read-only note lookup through:

```sh
make lookup
```
