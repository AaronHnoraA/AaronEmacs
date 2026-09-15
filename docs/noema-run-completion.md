# Run 结束、模型选择与派生状态

2026-09-15：针对 Agent 已回复、OutputCell 仍 running，以及取消后无变化的修复。

- 等 agent-shell 的 `init-finished` 再派发任务；`init-session` 只说明会话已创建，默认模型和模式尚未设置完。
- ACP prompt 回执先按 Run ID 保存，再调用 UI/worker 回调。正文中的“完成”、输入提示符、静默时间都不能证明 Run 成功。
- worker 冻结终态和最后一段输出，按 FIFO 上报；可选 UI 清理抛错不能阻断上报。响应丢失时先查询持久状态，有限次重试，不重发 prompt。
- OutputCell 右键或 `…` → **Recheck Completion**：核对回执、持久 Run 和输出回写。过期租约只能收敛为 interrupted，不能猜测 completed。
- Cancel 后会补做检测；若完成与取消竞争，以持久终态为准。旧 Run 不得覆盖新 Run 的输出。
- popup agent-shell 成功切换模型后，按 Agent 保存至 `var/noema/agent-models.json`。新会话通过上游默认模型流程恢复；已下架模型回退到原配置。运行/启动中不能并发切换模型。
- 子 Work 未显式指定 Agent 时，优先使用命名会话或当前 lineage 的 Agent，最后才用文档/请求默认值。depends 不传递会话或 Agent；多父节点冲突需要 `@@agent(...)`。
- 路由预览使用当前未保存的文档，移除 outputs 以降低传输量，并共享一次运行状态查询。编辑前请求的旧响应不会覆盖新标签。
- 同一 Run 的持久 completed/cancelled/failed/interrupted 不得被旧 running 缓存覆盖；不同 ID 的新 Run 可以显示 running。

## 生效与验证

这次增加了 worker 结构字段，请重新启动 Emacs 后开启新的 Agent 会话，避免给旧的活动 worker 热加载不兼容结构。没有自动重跑或清空项目会话。

验证包含 `make research-test`、`make jupyter-test`、Noema 的 `make test` / `make build` / `make install`、Go fts5 全套测试，以及真实 OpenCode ACP 对本地模拟服务的离线 smoke（零付费模型请求）。图形界面的当前活动会话仍需重启后的实际使用确认。
