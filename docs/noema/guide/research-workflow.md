# 项目 Agent 工作流

项目工作使用 `AGENTS.md`、普通 Markdown 和实际代码、数据、论文等文件。
需要跨会话接续时，按需在 `.agent/` 保存任务、记忆和交接记录；已有管理器的
数据库、对象库和原生 Session 记录仍由管理器维护。无需创建 `.noema`。

## 开始与接续

用共享 Agent Manager 打开或恢复项目会话。`M-x noema-project-overview` 汇总现有
Session、Run、任务、Attention 与能力；`M-x noema-sessions` 管理项目内的命名会话。
普通会话也可在未声明 Noema Project 的目录使用；持久 Noema 项目服务仍使用
`noema.toml` 的 `[project]` 与可选 `workspace`。

`noema-project` pack 提供主／子 Agent 分工、上下文、记忆和交接指导。主 Agent
检查整体目标和实际交付，子 Agent 获得必要资料、写入边界和完成条件。工具和
宿主负责实际执行；Skill 不模拟后台能力或强制增加工作文档。

## 任务图谱

多步工作由 Agent 在 `.agent/tasks/` 维护任务、依赖、领取、评审和决定，这是开发规范，不需要
你操作，也不需要 Noema 在运行。想看时 `M-x noema-project-graph`（`C-c A g`，或项目总览的
Agent cache）：一页显示缓存与记忆、任务树、需要处理的问题和最近日志。`g` 重新解析，
`RET` 打开任务文件，`m` 做保守维护。终端里用
`node ~/.config/emacs/site-lisp/noema/scripts/project-graph.mjs graph --text`。
契约见[项目任务图谱](../design/project-graph.md)。

## 围绕 Markdown 工作

在 Noema Markdown 页面选中正文，发送给已有 Agent 会话。问答直接回复；请求
改写时，可以在原位置生成 `ediff` 修改建议，比较排版或源码后接受、拒绝或评论。
保存评论后使用 Send feedback 继续交互。`noema-md` pack 提供语法与审阅指导。
普通 Jupyter notebook 和 Markdown `@@cell` 使用原有的执行、补全、LSP 与输出面。

## 记忆与证据

会话回忆与已验证结论分别标识。当前问题只召回相关材料；用原文与实际产物
核查重要结论，记录适用条件、来源版本、检查结果和不确定性。确认后的知识
整理进项目 Markdown，交接记录引用这些材料。

已有 Noema 服务继续提供冻结 RunSpec、上下文预算与回执、原生历史检索、CAS
产物和 Finding 审核。`M-x noema-history-search` 检索项目历史，`M-x noema-findings`
查看证据及审核状态。`M-x noema-research-propose-memory-from-run` 可从现有 Run 的
Handoff 原文提出记忆 Proposal，仍需证据审核。服务的 workstream 身份约束保持
原样；普通文档工作无需为此构造 Cell 或 WorkNode。

## 能力与界面

`M-x noema-capability-manager` 管理 Skill、pack 与 MCP；在 Agent 输入中可以检索
并起草引用，审阅后再发送。`noema-emacs` pack 只包含 Emacs 操作和受支持的 API，
维护源码使用 `emacs-noema-dev`。

旧 `.noema` 文件保留为源数据。专用 JuText、Graph Board、诊断／补全和 Work Output
接线已经移除；访问不会初始化项目、启动 Agent 或进入 Jupyter。

上下文机制见 [Agent 上下文管理](../design/agent-context.md)，记忆后端见
[自动记忆](../design/hy-memory.md)，操作与能力边界见
[交互](../design/interaction-plan.md)和[能力管理](../design/capabilities.md)。
