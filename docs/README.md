# Emacs Docs

Documentation for the Emacs configuration. The documents are written in Chinese; this README stays in English so README files remain consistent across repos.

## How this directory is organized

Every document is Markdown and lives in exactly one place, chosen by what it is:

| Directory | Contains | Rule |
| --- | --- | --- |
| `architecture.md` | The structure map | Layers, directories and boundaries; read it first. |
| `guide/` | How to use it | Current behaviour only. |
| `design/` | How it works now | Current behaviour only; no dates or change logs. Git log is the history. |
| `acceptance/` | What done means | Criteria, not evidence. |
| `ops/` | Maintenance and recovery | Current procedure only. |
| `records/` | Audits, incidents, measurements, retired notes | Named `topic-YYYY-MM.md`, written once and not updated to follow later code. A record never overrides a structural doc. |

A new document takes a slot in this map before it is written. Agents may write a document's body; adding a document or moving one between directories is a change to this map and is reviewed as one.

## Core Architecture

The [Remote framework](design/remote-framework.md) is the repository's core execution
model, not an SSH-only feature. Filesystem-, project-, process-, environment-,
LSP-, service-, and socket-aware development must use it from the start, with
the client represented as target `local` rather than a parallel local
implementation. [Remote parity](acceptance/remote-parity.md) defines completion, and
[the LSP workflow](design/lsp-workflow.md) applies the rule most strictly to
workspace roots, URIs, server placement, environments, watchers, helpers, and
channels.

- [architecture.md](architecture.md) Structure map of this config and Noema: load order, layers, directory ownership, the Emacs ↔ Noema boundary, and where a change belongs.

## guide/ — how to use it

Task-oriented: setup, daily keys, workflows, where to change things.

- [quick-start.md](guide/quick-start.md) First-time setup, system dependencies, fonts, path conventions, and bootstrap.
- [daily-usage.md](guide/daily-usage.md) Daily entry points, high-frequency keybindings, and leader-group layout.
- [settings-cookbook.md](guide/settings-cookbook.md) “I want to change X” guidance that tells you where each kind of change belongs.
- [dev-guide.md](guide/dev-guide.md) Programming, completion, LSP, debugging, terminals, remote work, browser integration, and AI.
- [project-guide.md](guide/project-guide.md) Project switching, project workbench flow, and how Treemacs / Perspective fit together.
- [research-notes-workflow.md](guide/research-notes-workflow.md) Division of labor between notes, Jupytext notebooks, Jupyter, and reusable source code.
- [workbench-tools.md](guide/workbench-tools.md) Adopted Xenodium editing, file, GitHub, Bazel, calendar, Org, and macOS workbenches, with keybindings and design boundaries.
- [publish-workflow.md](guide/publish-workflow.md) Personal site: where the hand-written pages live, the `make publish*` targets, the completeness/licence check, and the deploy path.

## design/ — how it works now

Current contracts and mechanisms. The code must match these; a change rewrites them in place.

- [remote-framework.md](design/remote-framework.md) Core `/fs` identity plus target/pipeline/backend/session routing, process and channel APIs, compatibility boundaries, and current implementation gaps.
- [remote-files.md](design/remote-files.md) Temporary machines remembered under `var/`, and file transfer from inside Dired/Dirvish: rsync upload/download/sync through a pop-up chooser, with jobs in the activity board.
- [lsp-workflow.md](design/lsp-workflow.md) Language-server routing, Hub/Doctor tooling, and the maintenance model.
- [jupyter-workflow.md](design/jupyter-workflow.md) Kernel sources (kernelspec, `attach:`, remote Jupyter servers over HTTP(S)), protocol coverage, Remote routing rules, and the Jupyter Board.
- [jupyter-project-context.md](design/jupyter-project-context.md) How a kernel gets its project: the Remote target owns connection identity, the kernelspec owns execution settings.
- [emacs-gateway.md](design/emacs-gateway.md) `remote-gateway`, the loopback JSON-RPC/WebSocket control plane through which Lean, Noema and other processes reach Emacs.
- [config-management.md](design/config-management.md) Unified `config` registry: one front door (`config-get`/`config-set`, `M-x config-board`) to view, edit, live-apply, and persist every registered setting.
- [agenda.md](design/agenda.md) Noema `@@todo`/`@@project`/`@@clock` syntax, server agenda/project/clock view-model, repeaters, dependencies, and Web/Emacs entry points.
- [latex-preview.md](design/latex-preview.md) In-buffer math preview: supported delimiters, the shared macro source Emacs and Noema both read, the doctor, and the troubleshooting order.
- [elegant-ui.md](design/elegant-ui.md) Aaron UI, the design tokens Emacs and Noema share, and where each side keeps its own styling.
- [codex-session-lifecycle.md](design/codex-session-lifecycle.md) ACP session reuse, Codex Remote ownership limits, and the optional mobile-to-Noema message bridge.
- [neomacs-compat.md](design/neomacs-compat.md) Running this configuration on Neomacs: the modifier-rename layer, fringe and startup-frame fixes, Elsa concurrency, and why the 0.0.18 migration was paused. Neomacs is not installed; the layer is inert on GNU Emacs.

## acceptance/ — what done means

Completion criteria a feature is checked against.

- [remote-parity.md](acceptance/remote-parity.md) VS Code Remote-level acceptance matrix, current coverage, completion criteria, and staged roadmap.

## ops/ — maintenance and recovery

Packages, locks, state, cleanup, new machines.

- [maintenance.md](ops/maintenance.md) Package management, lock workflow, state directories, cleanup, troubleshooting, and maintenance cadence.
- [migration.md](ops/migration.md) New-machine setup, restore workflow, and the migration lessons learned from this configuration.

## records/ — history

Dated and append-only. Read them for why something is the way it is, never for how it is now.

### records/audits/

Upstream comparisons, code audits and their line inventories.

- [agent-fleet-noema-audit-2026-10.md](records/audits/agent-fleet-noema-audit-2026-10.md) Source-linked review of the Herdr-based Agent Fleet: why it is not merged (second agent runtime, local-only socket, screen-detected state), and the worktree-isolated session and persistent parallel-attempt plan absorbed into Noema.
- [agent-workflow-landscape-audit-2026-10.md](records/audits/agent-workflow-landscape-audit-2026-10.md) Source-linked shortlist across agent sessions, human collaboration, durable DAG workflows, autonomous research, and software agents.
- [classic-workflow-dag-noema-audit-2026-10.md](records/audits/classic-workflow-dag-noema-audit-2026-10.md) Classic workflow lessons for dependency convergence, incomplete outputs, result staleness, and verifiable completion; includes the recent-event window fix.
- [emacs-vibe-package-2-noema-audit-2026-10.md](records/audits/emacs-vibe-package-2-noema-audit-2026-10.md) Source-linked review of conversation trees, temporary forks, Typst preview, and native Jupyter; includes the Noema Agent integration.
- [entrypoint-audit-2026-10.md](records/audits/entrypoint-audit-2026-10.md) Current entry map, comparisons with Lunarymacs, Doom, Spacemacs and Prelude, and the ongoing command and lifecycle review.
- [jupyter-parity-audit-2026-09.md](records/audits/jupyter-parity-audit-2026-09.md) VS Code alignment register, filesystem placement distinctions, Aaron-PC live evidence, and remaining acceptance work.
- [langgraph-openhands-letta-noema-audit-2026-10.md](records/audits/langgraph-openhands-letta-noema-audit-2026-10.md) Source-linked review of checkpoint versions, event/context views, subagent handoff, and memory isolation.
- [lexora-orca-noema-audit-2026-10.md](records/audits/lexora-orca-noema-audit-2026-10.md) Feature-by-feature source review, Noema upgrade decisions, and permission-boundary fixes.
- [markdown-editors-noema-audit-2026-10.md](records/audits/markdown-editors-noema-audit-2026-10.md) Source-linked comparison of three Markdown editors with Noema: merged format toggles, CJK emphasis, context-aware paste, code-block input, word-sized undo, range-selection rendering, CRLF save and save retry; with a [per-file line inventory](records/audits/markdown-editors-line-audit-2026-10.md).
- [markwright-noema-audit-2026-09.md](records/audits/markwright-noema-audit-2026-09.md) Source-linked comparison of MarkWright with the Noema editor: merged slash-menu, inline HTML and print details; rendering, CSS, theme and lifecycle findings; with a [per-file line inventory](records/audits/markwright-line-audit-2026-09.md).
- [mellos-thirdspace-noema-audit-2026-10.md](records/audits/mellos-thirdspace-noema-audit-2026-10.md) What remains to absorb from Mellos after the 2026-09-21 pass (whole-graph reads, affected consumers, verified baselines, ready frontier) and which ThirdSpace vault ideas fit Noema (agent routing contract, inbox default, report-only meta diagnostics).
- [noema-agent-upstream-audit-2026-09.md](records/audits/noema-agent-upstream-audit-2026-09.md) agent-shell / ACP audit and the move to package-managed upstreams, with the 2026-09-26 upgrade.
- [noema-xwidget-audit-2026-06.md](records/audits/noema-xwidget-audit-2026-06.md) Full-chain stability, HCI, and security audit of the Emacs ↔ xwidget ↔ noema bridge.
- [remote-io-review-2026-07.md](records/audits/remote-io-review-2026-07.md) `emacs-io` audit, adopted resource ideas, rejected ownership/POSIX shortcuts, and Remote performance criteria.
- [vibemux-noema-audit-2026-10.md](records/audits/vibemux-noema-audit-2026-10.md) Source-linked comparison of both Vibemux projects with Noema's Agent sessions, attention, recovery, and permission model.

### records/incidents/

One failure each: what broke, why, and the fix.

- [noema-run-completion-2026-09.md](records/incidents/noema-run-completion-2026-09.md) When an Agent Run counts as finished, model selection at session start, and derived Run state.
- [opencode-acp-recovery-2026-09.md](records/incidents/opencode-acp-recovery-2026-09.md) The OpenCode ACP prompt-stage regression and how it was worked around.

### records/measurements/

Measured numbers and the setup that produced them.

- [remote-performance-2026-09.md](records/measurements/remote-performance-2026-09.md) Reproducible local/SSH latency measurements, implemented optimizations, version boundary, and remaining gaps.

### records/retired/

Notes on code that has been removed.

- [neopyter-protocol-notes.md](records/retired/neopyter-protocol-notes.md) Historical: the Neopyter JupyterLab wire protocol. The client was removed; kept as reference only.

## Noema documents

Noema keeps its own docs in `../site-lisp/noema/docs/`. Two are useful from here:

- [Native Agenda integration study](../site-lisp/noema/docs/architecture/agenda-org-integration-study.md) Org Agenda UI reuse over native Markdown/WorkNode data, project-scoped indexing, and explicit Apple global attention; includes an isolated prototype.
- [slides-demo.md](../site-lisp/noema/docs/slides-demo.md) Ready-to-open `kind: slides` Noema deck with math and HTML examples.

## Working files

- [agent.md](agent.md) Prompt for the docs Q&A agent (`noema-interaction-docs`); it reads this directory to answer questions about the config.
- [TODO.md](TODO.md) Collected feature ideas and bugs waiting to be handled together.

## Shortest Path

- Want to install it: [quick-start.md](guide/quick-start.md)
- Want keybindings: [daily-usage.md](guide/daily-usage.md)
- Want Noema tasks/agenda: [agenda.md](design/agenda.md)
- Want to change behavior: [settings-cookbook.md](guide/settings-cookbook.md)
- Want the project workflow: [project-guide.md](guide/project-guide.md)
- Want math preview behaviour or a formula that will not render: [latex-preview.md](design/latex-preview.md)
- Want programming / LSP / remote details: [dev-guide.md](guide/dev-guide.md)
- Want maintenance and lock/state guidance: [maintenance.md](ops/maintenance.md)
- Running on Neomacs instead of GNU Emacs: [neomacs-compat.md](design/neomacs-compat.md)
