# Codex Note Lookup Task

You are running an interactive lookup session against Aaron's Markdown note system.

This is a read-only retrieval task. Do not edit files, stage changes, commit, or push. If no initial query is provided, ask the user what they want to find before searching.

## Source Policy

- Use `agent/` as a fast navigation layer, not as final evidence.
- `agent/index/` and `agent/wiki/` help you find candidate notes quickly.
- Any precise answer must be verified by reading the original `.md` file under `roam/`.
- Precise means definitions, formulas, claims, dates, tags, IDs, links, backlinks, or wording-sensitive summaries.
- If you cannot verify a precise point in the original Markdown source, say that it was not verified.

## Search Order

1. Read `agent.md`.
2. Read `agent/project-overview.md` and `agent/growth-log.md`.
3. Read `agent/index/README.md`.
4. Search `agent/index/` for likely titles, tags, links, and backlinks.
5. Use `agent/wiki/` only to narrow candidate notes.
6. Open every original Markdown file needed to verify the final answer.

Useful commands:

```sh
rg -n "term|another term" agent/index agent/wiki roam
```

## Answer Rules

- Keep the conversation interactive: answer the current lookup, then wait for the next user question.
- Answer in the user's language unless the query asks otherwise.
- Keep the answer concise, but include enough source grounding to be useful.
- Cite local source paths for verified facts, preferably original Markdown paths.
- Distinguish clearly between "verified from original Markdown" and "found only in generated index/wiki".
- If multiple notes are relevant, explain the relation briefly.
- If the query is ambiguous, give the best matches and state the ambiguity.
