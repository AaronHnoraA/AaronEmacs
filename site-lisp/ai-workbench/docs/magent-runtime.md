# ai-workbench + embedded Magent runtime

ai-workbench now has one orchestration model for four execution engines:

| Engine | Sampling/execution | Tool and permission owner | Outer lifecycle owner |
|---|---|---|---|
| API | Magent + gptel | Magent | Magent |
| Codex CLI | `codex exec --json` / resume | Codex CLI | Magent |
| Claude Code | `claude -p --output-format stream-json` / resume | Claude Code | Magent |
| OpenCode | `opencode run --format json` / session resume | OpenCode | Magent |

`ai-workbench-open` opens agent-shell for the API engine and the unified
transcript for managed CLI engines. `ai-workbench-open-direct-terminal` keeps
the old direct terminal available as an explicit escape hatch.

Upstream CLI session identifiers are stored in the Magent session metadata, so
each project and engine resumes its own native context. Session and audit files
live below `var/ai-workbench/magent/sessions/`.

## Performance and safety boundaries

- CLI stdout is parsed incrementally as newline-delimited JSON; terminal text is
  never scraped.
- A JSON line is capped at 1 MiB, diagnostics at 256 KiB, and one assistant
  answer at 8 MiB.
- User/profile prompts are capped at 2 MiB; combined Magent context sent to a
  CLI is capped at 4 MiB; each project may retain at most 32 queued turns.
- Transcript buffers have a 16 MiB soft cap and trim old complete blocks.
- Streaming inserts through a marker and does not rebuild the transcript.
- CLI progress events reset Magent's inactivity timer without becoming Magent
  tool calls, preventing duplicate execution.
- stderr uses a bounded transient buffer that is killed when the process exits.
- Magent and its supporting packages remain lazy until an AI command is used.

Magent audits the managed outer turn, queue, session, cancellation, and
lifecycle. On CLI routes, individual file/shell/tool actions remain owned and
recorded by the native CLI rather than being replayed as Magent tool calls.

The configurable values are registered in `lisp/init-ai-ide.el` and persisted
through `etc/config-store.el`.
