---
name: noema-agent-bridge
description: From another Agent on the same Mac, use Emacs's shared ACP API to delegate background tasks or contact an existing session. Emacs owns execution; callers never attach a second client to an occupied task.
---

# Collaborate through Emacs

Prefer `node ~/.config/emacs/site-lisp/noema/scripts/agent-api.mjs call emacs:agent -`
with a JSON argument array on stdin. The shared API supports `list`, `status`,
`tasks`, `create`, `spawn`, `send`, `result`, `read`, `stop` and `show`. Read the operational
skill at `~/.config/emacs/site-lisp/noema/resources/skills/noema-agent-operations/SKILL.md`
for background workers and completion notifications. Global Pi is the manager;
it uses this same API. Callers keep independent contexts, not inherited history.
For delegation and file handoffs, use `emacs-agent-collaboration`; `tasks` recovers
request IDs by root/target without returning prompts or replies.

For a new background task use `spawn` with `root`, `agent`, `prompt` and optional
`name`, `files`, `notify:{buffer,nativeSessionId}`. Save `requestId`; use `result`
for its bounded reply, or end your turn and receive the notification. `show`
opens the real worker buffer only when requested. Do not make the user relay
OCR output or worker messages.

## Local socket fallback

When the Noema HTTP host is unavailable and the local bridge is enabled, run
`python3 ~/.config/emacs/.agents/skills/noema-agent-bridge/scripts/bridge.py api`
with one JSON object on stdin. This is a transport adapter to the same Emacs API;
it owns no queue or reply tracking. It uses only this Mac's local Emacs socket.

The legacy `list` command returns exact `sessionId`, `agent`, `workspace` and
`buffer` fields for all public live ACP providers. `send`, `read`, `interrupt`
accept the first three identity fields; `send` takes `--message TEXT` or stdin,
and `read` also takes `--request-id ID`. They route to shared send/result/stop.
For a result after the worker exits, prefer `api` with action=result and requestId.

No matching session means unavailable, not permission to recreate its conversation.
Do not resume an occupied task with another CLI, kill it as recovery, or resend a
mutation after an ambiguous timeout. If this fallback is disabled, report that fact;
its explicit Emacs enable command is `M-x +noema/agent-bridge-start`.

`starting`, `running`, `queued` and `held` are not completion. Only the request's
terminal result describes how it ended; inspect artifacts before accepting its claim.
Stop only within the user's instruction, and keep permission decisions with the user.
