# Codex sessions in agent-shell

## Identities and ownership

| Concept | Current source of truth |
| --- | --- |
| Task identity | The mobile Codex Remote task is owned by Codex. ACP does not expose its task ID or owner to agent-shell. |
| Session identity | ACP `sessionId`, saved in `agent-shell--state` as `:session :id`; while loading, the requested ID is buffer local. Noema has a separate logical session ID for durable Runs. |
| Process identity | The live ACP adapter process in `:client :process`; its OS PID is not a session ID. `codex-acp` starts its own Codex app-server child. |
| Frontend | The agent-shell Emacs buffer. Several windows can show one buffer; closing a window leaves its execution running. |
| Execution host, workspace, transport | The Remote framework canonicalizes native, TRAMP, and `/fs:TARGET:/path` directories to a target plus workspace. ACP process placement still follows the existing Remote and TRAMP route. |

Before an explicit ACP session resume, agent-shell scans live buffers for the
same ACP session ID, agent, target, and workspace. A match reopens that buffer
without starting another adapter. A dead process never matches. Noema's session
and Run entry points use the same live-process check. This is an in-memory
lookup, so an Emacs crash leaves no registry file or stale ownership claim.

On `TASK_BUSY`, Codex's structured error code is used if supplied. ACP has no
standard busy code, so the exact provider message is the fallback. The failed
ACP client is shut down; the buffer shows the target, workspace, session ID,
and `M-x my/agent-shell-retry-task`. A failed resume does not silently fall
through to `session/new`. The retry command starts a new ACP connection only
after the user invokes it. No other client's process is terminated.

Killing an agent-shell buffer still shuts down its ACP process, as before.
Closing the popup/window only hides the buffer and leaves the process alive.
The existing explicit close and stop commands still work. Keeping an ACP
process alive after its owning buffer is killed would require a larger change
to agent-shell's state, callbacks, and cleanup, so buffer lifetime remains
coupled to process lifetime.

## Mobile Remote boundary

The installed `codex-acp` adapter spawns `codex app-server` over stdio for
each ACP client. ACP `session/load` and `session/resume` load conversation
history into that connection; they do not attach to an execution owned by a
different client. The mobile Codex Remote client uses Codex's managed
app-server daemon. The Codex CLI also provides `codex app-server proxy` to
connect stdio clients to a running daemon, but the installed ACP adapter does
not select that transport. Enabling it for agent-shell would be a separate,
opt-in integration with tests of shared thread ownership and permissions.
This patch does not claim to make a live agent-shell ACP task writable from
the mobile app. If the mobile app reports that another Codex session owns the
task, close the owning session before retrying there.

## Separate mobile task as a messenger

For the existing Emacs task, run `M-x my/noema-agent-bridge-start` once in the
running Emacs. This opens Emacs's local Unix server socket and enables a small
request handler. `M-x my/noema-agent-bridge-stop` disables the handler without
stopping any other use of the Emacs server. It is off after an Emacs restart
until started again.

Open a **new** Codex Remote task on the same Mac and invoke the globally
installed `noema-agent-bridge` skill. Its CLI lists live ACP sessions and
sends to the exact ACP session ID, agent, and canonical Remote workspace.
When an interactive turn is busy, its existing agent-shell prompt queue
receives the message; when idle, Noema submits a normal ACP text turn. A
structured Noema Run must finish before the bridge sends another turn, because
that Run does not use agent-shell's ordinary queue drain. The CLI may request
an explicit turn interruption, but never starts, resumes, or kills an agent
process. Each send returns a bridge request ID. `read` returns the reply text
captured from ACP events and its queued, running, completed, or failed state,
even while Noema defers rendering the hidden buffer. A new mobile task has
its own Codex conversation and does not inherit the Emacs session's history.
The bridge is unavailable if the Emacs process is gone or the mobile task runs
on a different host.

The bridge is intentionally local: it uses the already available
`emacsclient` Unix socket, and ACP process placement remains in the Remote
framework. There is no new SSH, TRAMP, or network service.

The `/` menu in agent-shell shows only commands announced by the ACP adapter
through `available_commands_update`. It does not copy CLI-only commands into
ACP, and `@` completion continues through the existing path.

Protocol and implementation references:

- [ACP session setup](https://github.com/agentclientprotocol/agent-client-protocol/blob/main/docs/protocol/v1/session-setup.mdx)
- [codex-acp connection implementation](https://github.com/agentclientprotocol/codex-acp/blob/main/src/CodexJsonRpcConnection.ts)
- [Codex managed app-server daemon](https://github.com/openai/codex/blob/main/codex-rs/app-server-daemon/README.md)
- [Codex Remote on mobile](https://developers.openai.com/blog/mastering-codex-remote-for-engineering)
