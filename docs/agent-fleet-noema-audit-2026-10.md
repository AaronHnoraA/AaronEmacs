# Agent Fleet → Noema / Emacs AI 工具箱审计（2026-10-03）

审计固定在 [Hirozy/agent-fleet `f23a0d4`](https://github.com/Hirozy/agent-fleet/tree/f23a0d484e1d9a10c263c2a0c5fa395436c77607)（约 1.1 万行 Elisp，13 个源文件）。对照对象是本配置的 AI 工具箱：[`init-ai-ide.el`](../lisp/init-ai-ide.el)（agent-shell/ACP 放置、`C-c A`）、Noema 的 [`noema-agent-acp.el`](../site-lisp/noema/lisp/noema-agent-acp.el)、[`noema-sessions.el`](../site-lisp/noema/lisp/noema-sessions.el)、[`noema-agent-inbox.el`](../site-lisp/noema/lisp/noema-agent-inbox.el)、[`noema-agent-abtop.el`](../site-lisp/noema/lisp/noema-agent-abtop.el)、[`noema-context.el`](../site-lisp/noema/lisp/noema-context.el)，以及终端入口 [`init-vterm-popup.el`](../lisp/init-vterm-popup.el)。

上游测试：本机 Emacs 31.0.91 上 `make test`（含 `byte-compile-error-on-warn`）420/420 通过。本机没有 `herdr` 与 Ghostel，因此未跑 `make test-live`，与 Herdr 服务端交互的结论均来自源码和 [`docs/PROTOCOL.md`](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/docs/PROTOCOL.md)。

## 结论：不整体合并，吸收两项机制

Agent Fleet 是 [Herdr](https://github.com/herdrdev/herdr) 的 Emacs 前端：agent 进程、PTY、状态、workspace 和 worktree 都由 Herdr 服务端持有，Emacs 通过本机 Unix socket 的 JSON RPC 操作它们。整体引入它与本配置的三条既有边界冲突：

| 冲突 | 上游实现 | 本配置规则 |
|---|---|---|
| 第二套 agent 运行时与会话注册表 | Herdr 持有 agent 进程与生命周期；Fleet 维护自己的 agent/workspace/task 缓存。见 [`herdr.el` 同步](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/herdr.el#L173-L222)。 | 所有 agent 会话由 `noema-agent-acp-adopt` 按项目登记，Sessions/Inbox/abtop 只有一个来源。与不暴露 Eglot 的理由相同：不并存一个未受管的第二客户端。 |
| 只能在本机 | 传输是 `make-network-process :path` 的本机 Unix socket（[`herdr-protocol.el`](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/herdr-protocol.el#L338)）；agent 的 PATH/环境来自 Herdr 服务端进程而非 workspace 环境 capsule。 | agent 必须跟随 workspace 的 target 放置，网络必须走 `remote-channel`（见 [remote-framework](remote-framework.md)）。Noema 的 ACP 会话已经满足本机/远程同一路径。 |
| 状态来自屏幕识别 | `agent_status` 由 Herdr 对 PTY 画面做 agent 识别（协议中的 `screen_detection_skipped` 字段，见 [PROTOCOL.md](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/docs/PROTOCOL.md#L214)）。 | 与 [Vibemux 审计](vibemux-noema-audit-2026-10.md) 的决定一致：状态以结构化 ACP 事件与 kernel Run 事实为准，不从终端文本推断。 |

因此不安装、不 vendor、不以 package-vc 锁定 Agent Fleet。值得吸收的是两项 Noema 当前**确实缺少**的机制（见下文 P1、P2），按 Noema 自己的边界实现。

## 功能对照

| Agent Fleet 能力 | 上游实现 | Noema / 本配置现状 | 决定 |
|---|---|---|---|
| 多 agent 总览与注意力 | 事件驱动 dashboard、`!` 跳到 blocked/done，打开时原子地 snapshot + 回放排队事件（[dashboard](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-dashboard.el)）。 | Inbox 已有跨项目 `!`、`u`、定向刷新与旧回包隔离；abtop 有用量/配额。 | 已覆盖，不吸收。 |
| 按 Project 归组，worktree 归回主仓库 | `git rev-parse --git-common-dir` 把 linked worktree 映射到主 checkout（[`agent-fleet-project.el`](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-project.el#L92-L115)）。 | Noema Project 由 `noema.toml` 的 `[project]` 决定（D-038），不是 Git 概念。 | 不改 Project 身份。若 P1 落地，worktree 会话归属其发起 Project，由 Noema 记录，不靠 Git 反推。 |
| 编辑器上下文交给 agent | `prompt-dwim`：文件、行范围、符号、小选区原文，粘贴进终端输入框（[`agent-fleet-project.el`](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-project.el#L350-L456)）。 | `noema-context-send-*` 按引用发送，经 `noema-agent-acp-agent-file` 映射到 agent 所在 target，已处理 worktree 符号链接。 | 已覆盖且更完整，不吸收。 |
| 每个 agent 独立 Git worktree | `agent-fleet-start :worktree t` 调 Herdr `worktree.create`；脏 worktree 删除需 force（[`agent-fleet-worktree.el`](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-worktree.el#L118-L196)）。 | **缺失。** Noema 会话都在 Project workspace 原 checkout 里工作；同一仓库多个写代码的会话会互相覆盖。 | **P1 吸收。** |
| 按 agent 打开 Magit status/diff | 从 agent cwd 找 checkout 根后打开 Magit（[`agent-fleet-magit.el`](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-magit.el#L90-L195)）。 | 需手动切到 workspace 再 `magit-status`。 | 随 P1 一起做：Sessions/Inbox 行上 `m`/`d` 打开该会话 checkout 的 Magit。 |
| 并行任务聚合状态 | 全部 done 才算 done，缺失成员为 failed，一个完成不杀其他（[`agent-fleet-parallel.el`](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-parallel.el#L127-L149)）。 | Run 有并行上限；kernel 有 Task/Job/Delegation 模型（Orchestration 板只读）。没有“同一提示分给多个 agent 各自在 worktree 里做、再比较”的入口。 | **P2，依赖 P1。** 聚合规则可直接借用，但任务组必须持久化在 kernel，不能像上游只存在 Emacs 内存。 |
| `$EDITOR` 桥 | agent CLI 的 Ctrl-G 编辑经 `emacsclient` 回到 Emacs，限时路由到发起窗口（[`agent-fleet-editor.el`](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-editor.el)）。 | ACP 会话不用 `$EDITOR`；只有 vterm 弹窗里直接跑的 CLI agent 会用到。 | 低优先级。若需要，放在终端层（`init-vterm-popup.el`），且远程 target 要另行设计 `emacsclient` 通道，不进 Noema。 |
| 终端 attach（Ghostel） | 交互时 attach 到 Herdr PTY。 | Agent 窗口本身就是 agent-shell buffer；终端 agent 有 vterm 弹窗。 | 不吸收。 |
| 服务端生命周期 | `herdr-start` 以 `nohup` 分离启动，退出 Emacs 后 agent 仍在。 | ACP 会话可经 `session/load`/`resume` 恢复，Run 与 Session 名由 kernel 持久保存。 | 已有对应的恢复模型，不吸收。 |

## 源码校验发现

1. **`prompt-dwim` 可能把主 checkout 的绝对路径交给 worktree agent。** 相对化的根是 agent 的 cwd（[L440-L443](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-project.el#L440-L443)），而候选 agent 按“同一 Project”筛选，这里包括 linked worktree 里的 agent（[L329-L348](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-project.el#L329-L348)）。从主 checkout 的 buffer 发给 worktree agent 时，文件不在 cwd 下，于是发送主 checkout 的绝对路径，agent 可能直接改主 checkout，隔离失效。README 写的是“相对于 agent 的 project root”，与代码不符。这是静态推断，未在 Herdr 上复现。**P1 必须避免这一点**：发给 worktree 会话的引用要映射到该 worktree 内的同名路径；该路径不存在时报错，不能退回到主 checkout。
2. **在远程 buffer 里运行 `herdr-start` 会把服务端启动到别处。** `herdr--launch-server-process` 调用 `process-file` 时没有绑定 `default-directory`（[`herdr.el` L588-L640](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/herdr.el#L588-L640)），可执行文件却用本机 `executable-find` 查找，之后又探测本机 socket。从 TRAMP 或 `/fs:` buffer 调用时，启动命令会发往远程主机，而就绪检查等待的是本机 socket，结果多半是超时或启动到错误的机器上。这也是静态推断。本配置避免此类问题的办法，是由 workspace target 统一决定进程放置。
3. **并行任务的分组只存在内存里。** `agent-fleet--tasks` 是普通 `defvar`（[L90](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-parallel.el#L90)），Emacs 重启后任务与 agent 的对应关系就丢了，worktree 却仍在。上游 ROADMAP 也把“Parallel-task recovery”列为 P1。P2 必须由 kernel 持久保存。
4. **交互式 `agent-fleet-parallel` 给每个 agent 发同一个提示。** 交互式入口只读入一个提示（[L182-L190](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/agent-fleet-parallel.el#L182-L190)），每个 agent 各自一份提示只能从 Lisp 调用实现。这是有意的简化，不算缺陷，但 P2 的入口需要明确它支持的是“同题多解”还是“分工”。

## 吸收计划

**P1：worktree 隔离的 Noema 会话。**
- 入口：`noema-agent-start` 增加“在新 worktree 中启动”选项，Sessions/Inbox 增加 `m`/`d`（Magit status/diff），删除 worktree 时检查是否有未提交改动。
- 放置：在 Project workspace 所在 target 上用 `remote-process-file` 执行 `git worktree add/remove`，worktree 路径用 `/fs:TARGET:/…` 表示，agent 进程经现有 `my/agent-shell-process-directory` 启动在那里。本机走同一条代码路径，不判断 `file-remote-p`。
- 归属：kernel 的 Session 记录发起 Project 和 worktree 路径，Inbox 按发起 Project 归组，不改 D-038 的 Project 定义；worktree 不得创建在 vault 内。
- 上下文：按发现 1 处理 `noema-context`，把引用映射到 worktree。
- 测试：本机与 Aaron-PC 远程各一组（创建、发送引用、Magit、脏 worktree 拒删、强制删除），按 [remote-parity](remote-parity.md) 补齐。

**P2：持久的并行尝试组。** 依赖 P1。组与成员存在 kernel，聚合规则借用上游：全部 done 才算 done，缺失成员算 failed，一个成员完成不终止其他成员。Inbox 中以组为单位显示，组内并排比较各成员的 diff。最终选用哪个结果由人决定，不自动合并。

**不做：** Herdr 客户端、PTY 状态识别、Ghostel attach、第二个 dashboard、`$EDITOR` 桥（等确实需要终端 agent 时，再在终端层单独评估）。

## 本轮落地与验证（P1）

- [`noema-agent-worktree.el`](../site-lisp/noema/lisp/noema-agent-worktree.el)：`C-c A w` / `noema-agent-worktree-start` 从 Project workspace 当前所在的分支切出 `noema/<名字>`，在仓库旁边的 `<仓库名>.noema-worktrees/<名字>/` 建 linked worktree，在 workspace 在 worktree 中的对应目录启动 Codex/Claude/OpenCode，会话登记在原 Project 下，名为 `worktree/<名字>`。分出点记在 `branch.<分支>.noemaBase`，它同时标记这个 worktree 是 Noema 建的。持久 Session 本来就记录 `executionTarget`，所以恢复会话会回到原来的 worktree，不需要改 kernel。
- 放置：所有 Git 调用都经 `process-file` 在 Emacs 目录中执行，传给 Git 的是 checkout 内的相对路径。Git 打印的目标原生路径只在原生路径之间比较，不拼回 Emacs 名，因此本机、`/fs:local:` 与远程 target 走同一条代码路径。临时 buffer 里会带上调用方的 `process-environment`/`exec-path`。
- 发现 1 的处理：[`noema-context.el`](../site-lisp/noema/lisp/noema-context.el) 把主 checkout 文件的引用改写成会话 worktree 里的同名文件，比较用 `file-in-directory-p`，会解析符号链接（Git 打印的是解析后的路径）。worktree 里没有这个文件就报错，不会退回主 checkout。
- 审阅：Sessions 列表与 Inbox 新增 `m`（Magit status）和 `d`。worktree 会话的 `d` 是工作区对分出点 merge-base 的 diff，已提交和未提交的改动一起显示；普通会话只显示未提交改动。
- 清理：`noema-agent-worktree-remove` 只列出 Noema 建的 worktree。还有会话在里面工作时拒绝删除（加 force 也一样）；有未提交改动时需要前缀参数；分支始终保留。
- 测试：[`noema-agent-worktree-tests.el`](../site-lisp/noema/test/elisp/noema-agent-worktree-tests.el) 用真实临时仓库覆盖了 9 项：创建与重名拒绝、子目录定位、主 checkout 引用改写与缺文件报错、经符号链接的改写、调用方环境保留、`noema-context` 引用、列出与删除的三道保护、diff 从分出点开始、名字规整。已加入 `make research-test`，和 `noema-context` 测试一起 38/38 通过；改动文件 byte-compile 无警告。另用 `/fs:local:` 逻辑路径实际跑了创建、checkout、引用改写和列出，结果与原生路径一致。
- **未完成**：Aaron-PC 与 Aaron-PC-Remote 本轮 SSH 都连不上，远程 target 的真机回归还没有跑。按 [remote-parity](remote-parity.md)，远程这一列仍算未验证。P2（持久的并行尝试组）需要 kernel schema，留待下一轮。

## 第二轮吸收（2026-10-03）

继续对照上游 [ROADMAP](https://github.com/Hirozy/agent-fleet/blob/f23a0d484e1d9a10c263c2a0c5fa395436c77607/ROADMAP.md) 的 P1“Attention workflow”和 Fleet 对所有 agent 的 `blocked`/`done` 提醒，找到 Noema 的一个缺口：系统通知与注意力只来自 Run（[`noema-agent-worker.el`](../site-lisp/noema/lisp/noema-agent-worker.el) 的 `--notify` 只在 Run 结束和 Run 的权限请求时触发）。手动会话、worktree 会话、popup 和恢复的对话请求权限或答完一轮时，没有任何提示。同时开几个 worktree 会话后离开，正好就是这种情况。

- [`noema-agent-acp.el`](../site-lisp/noema/lisp/noema-agent-acp.el)：每个登记的 agent-shell 会话都订阅 agent-shell 自己发出的结构化 `permission-request` 与 `turn-complete` 事件，不从终端文本推断。buffer 不在屏幕上时设置 `noema-agent-acp-attention`（`permission`/`done`）；Emacs 不在前台时经 `noema-agent-acp-notify-function` 发通知，正文带上会话名。被打断的一轮（`cancelled`）、ephemeral/probe 会话不提醒。窗口显示该 buffer、`noema-agent-acp-show-buffer` 或 `u` 会清除标记。
- 不重复：worker 注册 `noema-agent-acp-run-owned-functions`，有 open Run 的 buffer 让给 host 的 Run attention。
- 呈现：Sessions 列表与 Inbox 的第一列在 host 没有给出 attention 时显示这个标记。Inbox 按它排序（权限请求与 host 的 `permission` 同级），`!` 轮转也包括无持久记录的本地行。
- P1 补完：发给 worktree 会话的区域引用，若那几行在 worktree 副本里内容不同，就拒绝发送（行范围用与引用相同的计算方式，止于行首的选区不含该行），不再只是文档里的提醒。
- 测试：[`noema-agent-attention-tests.el`](../site-lisp/noema/test/elisp/noema-agent-attention-tests.el) 5 项（隐藏时标记与显示时清除、只在失焦时通知、可见/打断/Run 所有/ephemeral 时跳过、worker 所有权、Inbox 排序/轮转/读），worktree 测试新增区域一致性 1 项。`make research-test` 五组全部通过（2、110、15、197、115）；改动文件 byte-compile 无警告。

仍未做：Aaron-PC 远程真机回归（本轮未再尝试，状态同上）、P2 持久并行尝试组（需 kernel schema）。上游 ROADMAP 的其余条目（Session 默认名与 attach 端点固定、Ghostel、Consult 适配、child-frame）都属于 Herdr/终端前端，不适用于 Noema。
