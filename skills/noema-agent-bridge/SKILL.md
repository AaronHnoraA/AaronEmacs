---
name: noema-agent-bridge
description: From a separate Codex Remote task on the same Mac, find a live Codex ACP session in Noema or agent-shell and send it a message without reopening its task.
---

# Talk to an existing Noema agent session

Use this skill when the user wants a new Codex Remote chat to contact a Codex
ACP session already running inside Emacs. The new chat is a messenger; it does
not attach to that session or inherit its conversation history.

Run `python3 ~/.codex/skills/noema-agent-bridge/scripts/bridge.py list`.
Choose the exact `sessionId`, `agent`, and `workspace` from the result. If no
matching session is listed, report that it is unavailable; do not resume it or
start another process. The Emacs owner must first run
`M-x my/noema-agent-bridge-start` in that Emacs instance. The bridge uses only
the local Emacs server socket on the same Mac.

To pass the user's message, run the script's `send` command with those three
fields and `--message TEXT` or text on stdin. A busy interactive Codex turn
queues the message for its next turn; an idle agent receives a new ACP turn.
If a structured Noema Run owns the session, wait for it to finish and retry.
Report whether the
result is `queued` or `submitted`, without claiming the agent has completed it.
Save the returned `requestId`. Use `read` with the same session fields and
`--request-id ID` to retrieve its current state and streamed reply. For a
queued message, its reply begins only after agent-shell submits it. Do not
mistake `submitted` for completion; check `read` again when needed.

Run `interrupt` only when the user explicitly asks to stop the selected
agent's current turn. It does not terminate the agent process. Never use
Codex `resume` on the occupied task or kill the owning session as recovery.
