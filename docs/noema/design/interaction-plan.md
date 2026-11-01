# Noema composition and writing plan

## Goal

Offer strong writing and lightweight model interaction in any Emacs buffer
without building another frontend or LLM client.

## Implementation rule

Emacs Agent 层维护文件与选区引用、会话选择、原生队列和管理界面。
Noema 维护 Markdown、知识库与研究业务。借鉴 gptel 的选区交互，不把 gptel
作为 Noema 上下文、提问或改写的依赖。Magent 的独立采样适配仍可使用其原有后端。

提示词使用现有 Noema Markdown 文档。`noema-prompt-document` Skill 指导项目 Pi
创建并打开文档；不维护专用 Compose 界面或另一份 prompt 编辑状态。

Agent 的进程和原生对话由 agent-shell / ACP 管理；业务身份与执行身份分开。
上下文与冻结 Run 的预算见 [agent-context.md](agent-context.md)。

## Current entry points

1. `+agent/compose` 请求项目 Pi 申请普通 Markdown prompt 文档。
2. `+agent/compose-send` 将保存后的 Markdown 任务作为引用发送给已有 Agent。
3. `+agent/compose-add-context` 选择文件或跟踪选区；`+agent-context-inspect` 查看、移除引用。
4. `+agent/compose-menu` 打开普通 Agent 会话菜单。
5. `+agent/compose-rewrite` 交给选中的 Agent；Noema 页面使用文档提案 API。
6. `+agent/start` 启动真实 ACP 会话；`noema-pi-emacs-open` 打开常驻 Pi，
   `noema-pi-router-open` 打开项目子 Pi。
7. `noema-marginnote-sync` runs a MarginNote digitization prompt in a pinned
   session of its own with the managed `marginnote-digitize` pack.  The Noema
   AI menu (`%`) exposes it under `M`; there is no separate MarginNote menu.
   Recognition and workflow decisions remain in the member Skills.

Editable Noema profiles and prompt templates remain under `etc/noema/`.
Future improvements should compose these mature implementations rather than
forking or imitating them.

## Noema Markdown review

每个原生 ACP Session 首次收到简短 API 能力入口；后续请求只有要求、文件与
精确选区。起草、复制到剪贴板不消耗会话初始化；新原生 Session 再初始化。
完整语法与全部 API schema 不按轮注入。

本地 API MCP 只公布 `noema_api` 与 `noema_call`。前者搜索当前注册 channel 名字或
查看一个接口的参数与实际实现；后者通过同一后端调用。已有会话可使用
`scripts/agent-api.mjs`，不必为新增 MCP 重启会话。它每次从 `host.json` 解析活跃
端口。公共 Server 模式仍只暴露原有只读发布能力。

`agent:document:read` 返回保存版本、UTF-16 范围和已有提案反馈；
`agent:document:propose` 保留旧文与未选文字、生成或修订原地提案，然后复用
`notes:save` 的 CAS、索引更新与页面通知。版本冲突要求重新读取，不强制覆盖。
API 不替人接受提案。

在 Noema Markdown 页面选中文字，用 `+agent-context-send-region` 发送给已有 Agent
会话，并输入修改要求。页面通过 API 创建原地 `ediff` 提案：改写以旧／新行提案放回
原位置，问答仍只回答。卡片默认比较 Markdown 局部排版，Diff 可检查精确源码；
接受和拒绝沿用可撤销的文档事务。写下评论后用 Send feedback 保存并把提案引用
送回会话，继续同一审阅流程。文件审阅列表也有这个入口。
范围与交付契约见 [agent-context.md](agent-context.md)，语法见 [syntax.md](syntax.md)。

## 工作入口与保留能力

项目工作以普通 Markdown、`AGENTS.md` 与必要的 `.agent/` 记录组织。
`project-agent-management` 提供任务、子 Agent、上下文、记忆和交接指导。
`noema-project`、`noema-emacs`、`noema-md` 分别组织项目方法、Emacs 操作与
Markdown 语法和审阅；维护实现继续使用 `emacs-noema-dev`。

`.noema` 专用 JuText、Company/CAPF、Flymake、Graph Board、模板与 Work Output
适配已移除。普通 notebook 文本投影、Jupyter 输出、Markdown `@@cell`、通用
Company、LSP、RaTeX 和共享 Agent 管理继续保留。

Research 的模型、存储、冻结上下文、记忆、Session、权限和证据继续由原服务
维护；现有 workstream 约束没有靠 Skill 模拟解除。旧文件不初始化编辑工作区。
原编辑器消费的 `worknode.state` 请求会明确报告未应用，未知协调请求也会失败，
不能用一个成功确认掩盖已经不存在的编辑入口。

## Pi 层级与生命周期

Pi 的会话记录、历史、活跃指针与 Research 独立；物理运行时仍使用共享 ACP。
主程序启动保留 Dashboard，后台初始化 Emacs Pi；项目激活复用项目 Pi。
Ibuffer 有独立 Pi 组，Dashboard 的 `pi` 入口打开 Pi Sessions。
`RET` 恢复、`n` 新建、`a` 归档、`i` 导入旧引用。结束 Pi 不终止项目执行 Agent。

`emacs:pi` 按需提供 list、status、ensure、send、resume、new、archive。
列表分页，历史与当前连接状态分别报告。原生身份、重启、权限和队列仍以实际
运行时为准。框架与当前限制见 [Pi 会话与项目助手](../../design/pi-sessions.md)。

Agent 的调用统一经过 `agent:call`，再调用原有处理器；权限审批、用户回答和
Proposal 审核接口仍可查询说明，调用会明确返回仅供人类决策，不让 Pi 自授权限。
