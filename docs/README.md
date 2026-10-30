# Emacs Docs

Documentation for the Emacs configuration. The documents are written in Chinese; this README stays in English so README files remain consistent across repos.

## How this directory is organized

This tree holds the documentation of both repositories: the Emacs configuration at the top level and Noema, which is maintained as its own repository in `site-lisp/noema/`, under [`noema/`](noema/). Both halves use the same categories. Every document is Markdown and lives in exactly one place, chosen by what it is:

| Directory | Contains | Rule |
| --- | --- | --- |
| `architecture.md` | The structure map | Layers, directories and boundaries; read it first. |
| `guide/` | How to use it | Current behaviour only. |
| `design/` | How it works now | Current behaviour only; no dates or change logs. Git log is the history. |
| `acceptance/` | What done means | Criteria, not evidence. |
| `ops/` | Maintenance and recovery | Current procedure only. |
| `records/` | Audits and studies, change logs, incidents, measurements, retired notes | Named `topic-YYYY-MM.md`, written once and not updated to follow later code. A record never overrides a structural doc. |
| `works/` | Projects in progress | One folder per project with a `README.md` workbench; third-party clones sit in its ignored `upstream/`. Never authoritative: settled results move into `design/` and `AGENT.md`, and the finished workbench moves to `records/`. |

A new document takes a slot in this map before it is written. Agents may write a document's body; adding a document or moving one between directories is a change to this map and is reviewed as one.

## Changing behaviour through the documents

A behaviour change or a bug fix starts in the owning `design/` document, not in a
prompt. The person settles the design there; an agent then reads the document and
implements it; tests and audits are checked against it. For that to work every
`design/` document is written with these sections (see
[window-lifecycle.md](design/window-lifecycle.md) for an example):

| Section | Holds |
| --- | --- |
| 范围 | What the document owns and what it leaves to others. |
| 模型 | The concepts and states, as the code implements them now. |
| 契约 | Numbered rules (`C1`, `C2`, …) that hold now and must keep holding. Numbers are stable; tests and commits cite them. |
| 实现位置 | Where each part lives in the code. |
| 已知差距 | Numbered gaps (`G1`, …): a rule that does not hold yet, or behaviour nobody has defined. A bug report is written here first. |
| 待定设计 | Questions only the person can decide. Once decided, the answer moves into 模型 and 契约. |

The loop:

1. **Record.** Describe the bug as a gap (`Gn`) in the owning document, or add the
   document if none owns the behaviour.
2. **Design.** The person answers the open questions and writes the target rules as
   contracts. An agent may draft, but the document must stay inside the person's
   model of the system.
3. **Implement.** An agent reads the document, changes the code to satisfy the
   contracts, and adds tests named after them.
4. **Close.** In the same commit the gap is deleted and its contract added; the
   document again describes what the code does.
5. **Audit.** An audit compares contracts with code and tests and is filed under
   `records/audits/`; findings go back to step 1.

Older design documents predate this structure and are converted when they are next
changed.

## Core Architecture

The [Remote framework](design/remote-framework.md) is the repository's core execution
model, not an SSH-only feature. Filesystem-, project-, process-, environment-,
LSP-, service-, and socket-aware development must use it from the start, with
the client represented as target `local` rather than a parallel local
implementation. [Remote parity](acceptance/remote-parity.md) defines completion, and
[the LSP workflow](design/lsp-workflow.md) applies the rule most strictly to
workspace roots, URIs, server placement, environments, watchers, helpers, and
channels.

# Emacs configuration

- [architecture.md](architecture.md) Structure map of this config and Noema: load order, layers, directory ownership, the Emacs ↔ Noema boundary, and where a change belongs.

## works/ — projects in progress

No active workbenches. The ignored upstream clones remain under `works/doom-template/upstream/` and `works/neo-emacs/upstream/` for reference.

## guide/ — how to use it

- [quick-start.md](guide/quick-start.md) First-time setup, system dependencies, fonts, path conventions, and bootstrap.
- [daily-usage.md](guide/daily-usage.md) Daily entry points, high-frequency keybindings, and leader-group layout.
- [documentation-center.md](guide/documentation-center.md) Reading map for this Emacs configuration, Noema, configuration settings, and the official Emacs manual.
- [settings-cookbook.md](guide/settings-cookbook.md) “I want to change X” guidance that tells you where each kind of change belongs.
- [dev-guide.md](guide/dev-guide.md) Programming, completion, LSP, debugging, terminals, remote work, browser integration, and AI.
- [project-guide.md](guide/project-guide.md) Project switching, project workbench flow, and how Treemacs / Perspective fit together.
- [research-notes-workflow.md](guide/research-notes-workflow.md) Division of labor between notes, Jupytext notebooks, Jupyter, and reusable source code.
- [workbench-tools.md](guide/workbench-tools.md) Adopted Xenodium editing, file, GitHub, Bazel, calendar, Org, and macOS workbenches, with keybindings and design boundaries.
- [publish-workflow.md](guide/publish-workflow.md) Personal site: where the hand-written pages live, the `make publish*` targets, the completeness/licence check, and the deploy path.

## design/ — how it works now

- [module-system.md](design/module-system.md) The module system: `aaron!` declaration with Doom categories, load phases with Remote as the foundation layer, `modulep!`, the load ledger (hooks, advice, timers, keys, files per module), the doctor and policy checks, and the module board.
- [remote-framework.md](design/remote-framework.md) Core `/fs` identity plus target/pipeline/backend/session routing, process and channel APIs, compatibility boundaries, and current implementation gaps.
- [remote-files.md](design/remote-files.md) Temporary machines remembered under `var/`, and file transfer from inside Dired/Dirvish: rsync upload/download/sync through a pop-up chooser, with jobs in the activity board.
- [lsp-workflow.md](design/lsp-workflow.md) Language-server routing, Hub/Doctor tooling, and the maintenance model.
- [jupyter-workflow.md](design/jupyter-workflow.md) Kernel sources (kernelspec, `attach:`, remote Jupyter servers over HTTP(S)), protocol coverage, Remote routing rules, and the Jupyter Board.
- [jupyter-project-context.md](design/jupyter-project-context.md) How a kernel gets its project: the Remote target owns connection identity, the kernelspec owns execution settings.
- [emacs-gateway.md](design/emacs-gateway.md) `remote-gateway`, the loopback JSON-RPC/WebSocket control plane through which Lean, Noema and other processes reach Emacs.
- [config-management.md](design/config-management.md) Unified `config` registry: one front door (`config-get`/`config-set`, `M-x config-board`) to view, edit, live-apply, and persist every registered setting.
- [command-shell.md](design/command-shell.md) The command shell: eshell per target context, the `+eshell-run` / `+eshell-run-sync` automation API with queueing and results, and `SPC o s`.
- [local-variable-trust.md](design/local-variable-trust.md) File and directory local variable trust: default roots, one-time approval, and persistent project decisions.
- [latex-preview.md](design/latex-preview.md) In-buffer math preview: supported delimiters, the shared macro source Emacs and Noema both read, the doctor, and the troubleshooting order.
- [elegant-ui.md](design/elegant-ui.md) Aaron UI, the design tokens Emacs and Noema share, and where each side keeps its own styling.
- [modeline.md](design/modeline.md) Doom modeline state block, theme colors, and native input ownership.
- [window-lifecycle.md](design/window-lifecycle.md) Windows, buffers and closing: the shown/undisplayed/ended state machine, close actions as verbs, single-instance buffers, and what a window falls back to.
- [neomacs-compat.md](design/neomacs-compat.md) Running this configuration on Neomacs: the modifier-rename layer, fringe and startup-frame fixes, Elsa concurrency, and why the 0.0.18 migration was paused. Neomacs is not installed; the layer is inert on GNU Emacs.

## acceptance/ — what done means

- [remote-parity.md](acceptance/remote-parity.md) VS Code Remote-level acceptance matrix, current coverage, completion criteria, and staged roadmap.

## ops/ — maintenance and recovery

- [maintenance.md](ops/maintenance.md) Package management, lock workflow, state directories, cleanup, troubleshooting, and maintenance cadence.
- [migration.md](ops/migration.md) New-machine setup, restore workflow, and the migration lessons learned from this configuration.

## records/ — history

Dated and append-only. Read them for why something is the way it is, never for how it is now.

### records/audits/

- [doom-template-2026-10.md](records/audits/doom-template-2026-10.md) Completed Doom-template restructuring workbench.
- [doom-template-comparison-2026-10.md](records/audits/doom-template-comparison-2026-10.md) Source-by-source Doom audit and optional behavior differences.
- [entrypoint-audit-2026-10.md](records/audits/entrypoint-audit-2026-10.md) Current entry map, comparisons with Lunarymacs, Doom, Spacemacs and Prelude, and the ongoing command and lifecycle review.
- [jupyter-parity-audit-2026-09.md](records/audits/jupyter-parity-audit-2026-09.md) VS Code alignment register, filesystem placement distinctions, Aaron-PC live evidence, and remaining acceptance work.
- [neo-emacs-2026-10.md](records/audits/neo-emacs-2026-10.md) Completed neo-emacs workbench.
- [neo-emacs-inventory-2026-10.md](records/audits/neo-emacs-inventory-2026-10.md) Each neo-emacs item’s final disposition.
- [neo-emacs-structure-2026-10.md](records/audits/neo-emacs-structure-2026-10.md) Historical framework comparison and migration plan.
- [remote-io-review-2026-07.md](records/audits/remote-io-review-2026-07.md) `emacs-io` audit, adopted resource ideas, rejected ownership/POSIX shortcuts, and Remote performance criteria.

### records/migrations/

- [naming-migration-2026-10.md](records/migrations/naming-migration-2026-10.md) Lookup table from the old `init-*.el` files and `my/` symbols to the Doom-templated `lisp/CATEGORY/NAME/` modules and `+DOMAIN-` / `+DOMAIN/COMMAND` / `aaron-` names.

### records/measurements/

- [remote-performance-2026-09.md](records/measurements/remote-performance-2026-09.md) Reproducible local/SSH latency measurements, implemented optimizations, version boundary, and remaining gaps.

### records/retired/

- [neopyter-protocol-notes.md](records/retired/neopyter-protocol-notes.md) Historical: the Neopyter JupyterLab wire protocol. The client was removed; kept as reference only.

# Noema

Noema is developed in its own repository (`site-lisp/noema/`); its documentation lives here. Code-adjacent contracts stay in that repository: `AGENTS.md`, `CLAUDE.md`, `README.md` and `UPSTREAMS.md`.

- [noema/architecture.md](noema/architecture.md) Repository map: what each top-level directory owns and where a change belongs.

## noema/guide/ — how to use it

- [agenda.md](noema/guide/agenda.md) Noema `@@todo`/`@@project`/`@@clock` syntax, server agenda/project/clock view-model, repeaters, dependencies, and Web/Emacs entry points.
- [layout-usage.md](noema/guide/layout-usage.md) Noema 自由排版：使用指南
- [plugin-merge-usage.md](noema/guide/plugin-merge-usage.md) 表格、关联笔记、Agent 与中文输入
- [research-workflow.md](noema/guide/research-workflow.md) Guided research workflow
- [snippets-and-jumps.md](noema/guide/snippets-and-jumps.md) Snippets and Cmd+[ / ] navigation
- [vim.md](noema/guide/vim.md) Noema 编辑器的 Vim 按键

## noema/design/ — how it works now

- [agent-context.md](noema/design/agent-context.md) Noema Agent 上下文管理：普通会话引用、研究 Run 冻结输入、预算、记忆、换代与回执契约。
- [agenda-dsl.md](noema/design/agenda-dsl.md) Agenda / Planning DSL
- [capabilities.md](noema/design/capabilities.md) Project Skills and MCP capabilities
- [codex-session-lifecycle.md](noema/design/codex-session-lifecycle.md) ACP session reuse, Codex Remote ownership limits, and the optional mobile-to-Noema message bridge.
- [current-architecture.md](noema/design/current-architecture.md) Noema 当前架构
- [elisp-api.md](noema/design/elisp-api.md) Noema semantic Emacs Lisp API
- [hy-memory.md](noema/design/hy-memory.md) Noema 自动记忆
- [interaction-plan.md](noema/design/interaction-plan.md) Noema composition and writing plan
- [latex-export-style.md](noema/design/latex-export-style.md) Noema LaTeX Export — Style Contract
- [magent-runtime.md](noema/design/magent-runtime.md) Noema + internalized Magent runtime
- [mcp.md](noema/design/mcp.md) Noema MCP
- [marginnote-agent-bridge.md](noema/design/marginnote-agent-bridge.md) MarginNote 原生插件 API 与 agent JavaScript 执行通道。
- [rendered-constructs.md](noema/design/rendered-constructs.md) 渲染组件（Rendered Constructs）统一架构
- [research-synthesis.md](noema/design/research-synthesis.md) Research synthesis and Proposals
- [scroll-ownership.md](noema/design/scroll-ownership.md) 视口归属：滚动只有一个主人
- [snippet-catalog.md](noema/design/snippet-catalog.md) Snippet catalog and upstream synchronization
- [syntax.md](noema/design/syntax.md) Noema source syntax: every form, its parser and its spec
- [wiki-workspace.md](noema/design/wiki-workspace.md) Noema Wiki workspace

## noema/records/ — history

### noema/records/audits/

- [adjustable-media-layout-study-2026-10.md](noema/records/audits/adjustable-media-layout-study-2026-10.md) Adjustable Media layout study
- [agenda-org-integration-study-2026-09.md](noema/records/audits/agenda-org-integration-study-2026-09.md) Org Agenda UI reuse over native Markdown/WorkNode data, project-scoped indexing, and explicit Apple global attention; includes an isolated prototype.
- [agent-fleet-noema-audit-2026-10.md](noema/records/audits/agent-fleet-noema-audit-2026-10.md) Source-linked review of the Herdr-based Agent Fleet: why it is not merged (second agent runtime, local-only socket, screen-detected state), and the worktree-isolated session and persistent parallel-attempt plan absorbed into Noema.
- [agent-workflow-landscape-audit-2026-10.md](noema/records/audits/agent-workflow-landscape-audit-2026-10.md) Source-linked shortlist across agent sessions, human collaboration, durable DAG workflows, autonomous research, and software agents.
- [capability-architecture-report-2026-09.md](noema/records/audits/capability-architecture-report-2026-09.md) Semantic API and capability architecture report
- [cherry-markdown-interaction-study-2026-07.md](noema/records/audits/cherry-markdown-interaction-study-2026-07.md) Cherry Markdown interaction study
- [classic-workflow-dag-noema-audit-2026-10.md](noema/records/audits/classic-workflow-dag-noema-audit-2026-10.md) Classic workflow lessons for dependency convergence, incomplete outputs, result staleness, and verifiable completion; includes the recent-event window fix.
- [emacs-vibe-package-2-noema-audit-2026-10.md](noema/records/audits/emacs-vibe-package-2-noema-audit-2026-10.md) Source-linked review of conversation trees, temporary forks, Typst preview, and native Jupyter; includes the Noema Agent integration.
- [git-ui-audit-2026-08.md](noema/records/audits/git-ui-audit-2026-08.md) 版本控制 UI 审计（2026-08-30）
- [langgraph-openhands-letta-noema-audit-2026-10.md](noema/records/audits/langgraph-openhands-letta-noema-audit-2026-10.md) Source-linked review of checkpoint versions, event/context views, subagent handoff, and memory isolation.
- [lexora-orca-noema-audit-2026-10.md](noema/records/audits/lexora-orca-noema-audit-2026-10.md) Feature-by-feature source review, Noema upgrade decisions, and permission-boundary fixes.
- [markdown-editors-line-audit-2026-10.md](noema/records/audits/markdown-editors-line-audit-2026-10.md) Marker / MarkText / files.md 逐文件行号清单（2026-10）
- [markdown-editors-noema-audit-2026-10.md](noema/records/audits/markdown-editors-noema-audit-2026-10.md) Source-linked comparison of three Markdown editors with Noema: merged format toggles, CJK emphasis, context-aware paste, code-block input, word-sized undo, range-selection rendering, CRLF save and save retry; with a [per-file line inventory](noema/records/audits/markdown-editors-line-audit-2026-10.md).
- [marker-marktext-filesmd-study-2026-10.md](noema/records/audits/marker-marktext-filesmd-study-2026-10.md) Marker / MarkText / files.md editor study (round 2)
- [markwright-line-audit-2026-09.md](noema/records/audits/markwright-line-audit-2026-09.md) MarkWright 固定提交的逐文件行号清单
- [markwright-noema-audit-2026-09.md](noema/records/audits/markwright-noema-audit-2026-09.md) Source-linked comparison of MarkWright with the Noema editor: merged slash-menu, inline HTML and print details; rendering, CSS, theme and lifecycle findings; with a [per-file line inventory](noema/records/audits/markwright-line-audit-2026-09.md).
- [mediawiki-roam-ux-audit-2026-10.md](noema/records/audits/mediawiki-roam-ux-audit-2026-10.md) Noema Wiki / Roam 创建与管理交互审计（对照 MediaWiki）
- [mellos-thirdspace-noema-audit-2026-10.md](noema/records/audits/mellos-thirdspace-noema-audit-2026-10.md) What remains to absorb from Mellos after the 2026-09-21 pass (whole-graph reads, affected consumers, verified baselines, ready frontier) and which ThirdSpace vault ideas fit Noema (agent routing contract, inbox default, report-only meta diagnostics).
- [noema-agent-upstream-audit-2026-09.md](noema/records/audits/noema-agent-upstream-audit-2026-09.md) agent-shell / ACP audit and the move to package-managed upstreams, with the 2026-09-26 upgrade.
- [noema-audit-2026-06.md](noema/records/audits/noema-audit-2026-06.md) Noema 审计报告 (2026-06)
- [noema-audit-2026-07.md](noema/records/audits/noema-audit-2026-07.md) Noema 全面工程审计
- [noema-xwidget-audit-2026-06.md](noema/records/audits/noema-xwidget-audit-2026-06.md) Full-chain stability, HCI, and security audit of the Emacs ↔ xwidget ↔ noema bridge.
- [org-roam-study-2026-08.md](noema/records/audits/org-roam-study-2026-08.md) org-roam and org-roam-ui architecture study
- [overleaf-source-editor-study-2026-07.md](noema/records/audits/overleaf-source-editor-study-2026-07.md) Overleaf Source Editor 架构审读与 Noema 迁移矩阵
- [pisper-agent-lifecycle-study-2026-09.md](noema/records/audits/pisper-agent-lifecycle-study-2026-09.md) Pisper study for Noema: agent management and interaction
- [siyuan-merge-audit-2026-08.md](noema/records/audits/siyuan-merge-audit-2026-08.md) 思源 → Noema：全量代码研判（值得 merge 的功能 / 特性 / 优化）
- [typora-syntax-survey-2026-07.md](noema/records/audits/typora-syntax-survey-2026-07.md) Typora 语法特性调研
- [vibemux-noema-audit-2026-10.md](noema/records/audits/vibemux-noema-audit-2026-10.md) Source-linked comparison of both Vibemux projects with Noema's Agent sessions, attention, recovery, and permission model.
- [zditor-interaction-study-2026-07.md](noema/records/audits/zditor-interaction-study-2026-07.md) Zditor interaction study for Noema

### noema/records/changes/

- [agenda-implementation-2026-09.md](noema/records/changes/agenda-implementation-2026-09.md) Noema Agenda 实装记录
- [perf-memory-upgrade-2026-07.md](noema/records/changes/perf-memory-upgrade-2026-07.md) Noema 性能 / 内存 / 页面清退升级
- [product-upgrade-2026-09.md](noema/records/changes/product-upgrade-2026-09.md) Noema product logic and 2026-09-25 upgrade

### noema/records/incidents/

- [noema-run-completion-2026-09.md](noema/records/incidents/noema-run-completion-2026-09.md) When an Agent Run counts as finished, model selection at session start, and derived Run state.
- [opencode-acp-recovery-2026-09.md](noema/records/incidents/opencode-acp-recovery-2026-09.md) The OpenCode ACP prompt-stage regression and how it was worked around.

## Working files

- [agent.md](agent.md) Prompt for the docs Q&A agent (`noema-interaction-docs`); it reads this directory to answer questions about the config.
- [TODO.md](TODO.md) Collected feature ideas and bugs waiting to be handled together.

## Shortest Path

- Want to install it: [quick-start.md](guide/quick-start.md)
- Want keybindings: [daily-usage.md](guide/daily-usage.md)
- Want Noema tasks/agenda: [agenda.md](noema/guide/agenda.md)
- Want to change behavior: [settings-cookbook.md](guide/settings-cookbook.md)
- Want the project workflow: [project-guide.md](guide/project-guide.md)
- Want math preview behaviour or a formula that will not render: [latex-preview.md](design/latex-preview.md)
- Want programming / LSP / remote details: [dev-guide.md](guide/dev-guide.md)
- Want maintenance and lock/state guidance: [maintenance.md](ops/maintenance.md)
- Running on Neomacs instead of GNU Emacs: [neomacs-compat.md](design/neomacs-compat.md)
- Want Noema's editor, agenda or capabilities: [noema/architecture.md](noema/architecture.md), then the matching `noema/design/` doc
