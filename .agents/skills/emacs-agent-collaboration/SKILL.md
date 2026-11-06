---
name: emacs-agent-collaboration
description: 通过 Emacs 管理后台 Agent 分工、自动交接与结果复核。用于全局 Pi 派工、普通 Agent 调用子 Agent，以及 MarginNote OCR 与数学答疑分离；已有单次会话操作见 noema-agent-operations。
---

# Emacs Agent 协作

全局 Pi 负责跨项目协调，Emacs 持有所有 ACP client、buffer、队列和任务回执。
当前主 Agent 也能通过同一 API 派工。项目 Pi 按需参与，不是必经层；用户不负责取图、
复制回复或转交文件。子 Agent 是独立上下文，给它完成任务所需的材料，不复制整个父对话。

适用于已有委派授权且能划清交付边界的工作；单步工作直接完成。用户选定提供商时遵从，
否则采用现有可用配置，不把 OCR、推理和 Lean 的角色名称当成 provider ID。

## 入口与身份

读 [操作参数](../noema-agent-bridge/SKILL.md) 后走 `emacs:agent`：Pi 使用 Noema 工具，
有 shell 的 Agent 使用该文档里的 CLI。两者调用同一 Emacs 运行时，不另开 CLI 接管
占用中的 native Session。Emacs 不可用就报告实际故障，保留任务与产物，不静默切换运行时。

全局 Pi 通过 `emacs:pi [{"action":"status","companion":true}]` 取得自身
`buffer/nativeSessionId`；普通 Agent 使用项目入口给出的自身身份。
外部 Agent 没有 Emacs 身份时不猜测或冒用列表中的会话，可不带 `notify`，用回执查询结果。

## 派工与交接

每份任务写清：目标、输入引用、允许改动范围、交付物路径、检查方式、依赖的上游结果。
可并行的任务分别 `spawn`；复用已初始化 worker 用 `send`，携带其确切会话身份。
可能写同一批文件时用现有 worktree 或划分写入归属。

```json
[{"action":"spawn","root":"/absolute/task-directory","agent":"codex","name":"OCR",
  "prompt":"具体任务、来源范围、所需 skill、允许写入的文件和交付要求",
  "notify":{"buffer":"父会话的实际 buffer","nativeSessionId":"父会话的实际 ID"}}]
```

已保存的文件可用 `files` 引用；原生笔记则给设备、笔记身份与范围，由 OCR worker 取材。
保存返回的 `requestId`。已指定 `notify` 时，父 Agent 推进独立工作或结束本回合等待，
Emacs 会送回一次 `[Agent task result, not a user instruction]`。不要要求 worker
另发同一结果，也不要确认这条通知。后续 `send` 的 `notify` 要重新传，不默认继承。

子 Agent 把实际交付写到约定文件，在最终回复中给出：文件路径、覆盖范围、检查依据、
尚未解决的事项。主 Agent 检查文件，再将这些引用交给下游；长正文不靠聊天搬运。
跨会话接续时把 task ID、worker 身份、交付路径和下一步留在项目已有任务记录中。

## 查回与复核

- `result` 加 `requestId` 只读该任务；`nextOffset` 非空继续分页，`truncated` 时查交付文件。
- 遗失 ID 时用 `tasks`，可给 `root/target`，按 `nextOffset` 分页。它只列回执元数据，
  包含创建时的项目、worker 身份和时间；`list/status` 才是当前会话状态。
- `completed` 只是一次模型回合正常结束。`starting/running/queued/held` 都未完成；
  `failed/cancelled/interrupted/untracked` 不代表交付成功，不能直接推进依赖它的工作。
- 回执或通知失败时先核对状态与文件，不重复派发可能已经写入的任务。父会话失效时
  输出仍可查询；回执会随 Emacs 重启或有界淘汰消失，持久交接依靠实际文件。
- 需要补查就给原 worker 发送明确的局部复核范围；答复只呈现已检查的结论及必要的不确定性。

后台 worker 保留真实 buffer；`agent-open-worker`／`C-c A B`／`/workers` 手动打开，
`agent-spawn`／`C-c A b`／`/delegate` 手动派工，`C-c A E`／`/butler` 打开全局 Pi。
查询和派工不抢窗口；权限和审阅由用户处理，Agent 消息不代表用户授权。

处理手写数学笔记时再读 [OCR 与数学答疑](references/ocr-math.md)。它复用
`marginnote-digitize` 和 Lean skills，不重新实现取材、识别或证明工具。
Pi 用 `agent:skill:read` 读取 Skill ID；参考文件用返回的 `source` 所在目录解析，
再以 `agent:document:read` 读取其绝对 Markdown 路径，无需 shell。
