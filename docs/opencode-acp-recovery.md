# OpenCode ACP: prompt-stage regression (2026-09-15)

The installed Homebrew `opencode 1.18.30_1` initializes ACP and creates
sessions, but fails on `session/prompt` with JSON-RPC `-32603`. Its log reports
`SystemPrompt.environment` / `TypeError: undefined is not an object
(evaluating 'a.name')`. This is independent of the separate `/fs:local:` CWD
projection bug fixed in `init-ai-ide.el` and Noema's ACP adapter.

Upstream evidence:

- https://github.com/anomalyco/opencode/issues/48903
- https://github.com/anomalyco/opencode/issues/48372
- https://github.com/anomalyco/opencode/pull/48397

## Installed workaround

Emacs prefers the **official precompiled** macOS arm64 release, version
`1.18.30`, at `var/noema/tools/opencode/1.18.30-official/opencode`.
The release archive is:

https://github.com/anomalyco/opencode/releases/download/v1.18.30/opencode-darwin-arm64.zip

Its SHA-256, verified before extraction against GitHub release asset metadata:

`a5e43d6887386efc7d68ce49ae28e3bbdfdee3dfd1d7169b612c3ce67e53b1e8`

The binary is local ignored state, not checked into Git. The Homebrew binary,
shell PATH, real provider configuration, credentials, and histories are left
unchanged. Both plain agent-shell and Noema use the same default ACP command.
Custom `agent-shell-opencode-acp-command` settings take precedence. Set that
command explicitly to `("opencode" "acp")` to revert in a running Emacs, or
set `my/agent-shell-opencode-executable` to nil to disable this preference at
startup. Keep the validated build until its replacement passes the test below.

After installing/reloading the configuration, create a **new** OpenCode agent
buffer: existing buffers retain their process. Reload without restarting Emacs:

```elisp
(load-file (expand-file-name "lisp/init-ai-ide.el" user-emacs-directory))
```

## Real prompt regression (no external model calls)

```sh
node test/opencode-acp-smoke.mjs /opt/homebrew/bin/opencode
node test/opencode-acp-smoke.mjs "$PWD/var/noema/tools/opencode/1.18.30-official/opencode"
```

The test isolates XDG config/data/cache/state in a fresh temporary directory,
disables external skills and default plugins, and enables only a loopback
OpenAI-compatible mock provider. It exercises ACP initialization, session
creation, a real read of its own fixture, completion, session/load, and a second turn that
must retain the tool result. It does not modify HOME or use real credentials.
Temporary artifacts are retained at the printed path. Only test processes are
terminated. Assertions are expected to fail for the affected Homebrew binary
(zero provider requests), and pass for the official binary (three local
provider requests). This verifies prompt preparation and transport, not the
availability of a real cloud model or DNS.
