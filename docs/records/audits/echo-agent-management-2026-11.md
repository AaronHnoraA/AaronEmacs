# echo-agent 管理能力研究与 Agent 生命周期补全

用户要求继续研究 ECA、agent-shell-hq，并加入 echo-agent，以完善我们的管理、交互与
生命周期。ECA 和 HQ 此前已有[源码审计](eca-hq-agent-management-2026-10.md)，本轮
以 echo-agent 为新材料，同时核对这两个项目此前没有落地的部分。长期契约见
[Agent 管理 C37–C44](../../design/agent-management.md)。

## 研究范围

| 来源 | 固定修订 | 切面 |
| --- | --- | --- |
| [echo-agent](https://github.com/fuyuxiang/echo-agent/tree/84c8a20ca8887781fe34a0c15c3c8ecceffd895e) | `84c8a20ca8887781fe34a0c15c3c8ecceffd895e` | 桌面端的队列、权限、问答、回退、子代理、通知、会话控制、进程监管 |
| [eca-emacs](https://github.com/editor-code-assistant/eca-emacs/tree/d08930b63ec91898019a931f13556deca7433828) | `d08930b63ec91898019a931f13556deca7433828` | 注意导航与队列交互 |
| [agent-shell-hq](https://github.com/sreenivasvrao/agent-shell-hq/tree/1c8eed8fd598ffa4332988713b4eda7e1d45b85a) | `1c8eed8fd598ffa4332988713b4eda7e1d45b85a` | 公开命令 |
| agent-shell | 锁定版本 `1cfb569` | 原生队列、中断、会话初始化、事件顺序 |

研究副本只在临时目录，没有进入加载链、依赖或仓库。echo-agent 的执行引擎不接入：
它有自己的 Runtime，我们只比较管理语义。

## 用真实进程确认的缺陷

下列问题都先用真实 stdio 的 ACP 进程写成失败测试，再修复。

| 现象 | 证据 | 契约 |
| --- | --- | --- |
| 取消后有排队 prompt 时，上游在 ACP 响应回调里同步调用 `y-or-n-p`；隐藏会话或停止整池时会连续弹问。旧端到端测试把它 stub 掉了，所以从未暴露 | 问题文本 `Pending prompts: 2 … Continue?` 被间谍捕获 | C37 |
| 被人停下的队列留在 `:pending-prompts`，下一个正常回合会把它带走 | 上游只在 `end_turn` 时无条件发送 | C37 |
| `refusal`、`max_tokens` 被记为完成并提示 done | 快照无停止原因，attention 为 done | C37 |
| 隐藏空闲会话的 client 退出后，列表不变、无提醒，直到下一次提交失败 | 没有任何事件到达 `+agent-acp-changed-functions` | C38 |
| client 退出后仍显示待审批和忙碌，Review 跳到已失效的按钮，结束会话还问“有工作” | 快照直接读取残留的 `:tool-calls` | C38 |
| 程序化回合不清除上次错误，运行中显示 failed | `+agent-acp-prompt` 没走 `input-submitted` | C37 |
| 对空闲会话按停止仍发送 `session/cancel` | `+agent-acp-stop` 没有判断 | C38 |
| Run 期间在输入区提交，会并发第二个 `session/prompt`；Run 结束也不发送队列，提示却说会发送 | shell-maker 不知道结构化回合在运行，fixture 收到第二个 prompt | C37 |
| 退出 Emacs 只保护原生队列，运行中的回合和草稿直接丢失 | 上游查询只看 `:pending-prompts` | C43 |

## 采纳与不采纳

| 来源能力 | 决定 | 本地形态或理由 |
| --- | --- | --- |
| echo `queueTerminalPolicy`、`pauseQueued` | 采纳 | 只有 `end_turn` 推进队列；挂起区由 Emacs 持有，人明确恢复 |
| echo `QueuePanel`：编辑、排序、暂停、立即发送 | 采纳 | `tools/agent/queue` 的 ETAF 面板；立即发送走 steering，不擅自打断 |
| echo 进程退出 `clearAll` | 采纳 | 不清状态，而是让事实查询忽略已退出 client 的请求 |
| echo 通知 `promote`、ECA `go-to-next-attention` | 采纳 | `+agent/next-attention` 与通知点击直达原生控件 |
| echo 权限卡的请求预览、ECA 会话状态 | 采纳 | 快照 `:request`，管理器详情显示；决定仍在原生控件 |
| echo `FileChangesPanel` | 部分采纳 | 只记文件和次数；行级差异属于 Git 和原生 diff，不另算一份 |
| ECA 耗时显示 | 采纳 | 回合起始时间，只在已有事件上刷新，不加周期计时器 |
| HQ 自动命名 | 改用事实 | 显示提供方标题或最近 prompt，不再调用一次模型 |
| echo 持久 outbox | 不采纳 | ACP 会话随 Emacs 结束；C43 在退出时保护，崩溃恢复需要的 UI 成本高于收益 |
| echo `command-risk` 风险徽章 | 不采纳 | 原生审批已显示完整命令；正则分级会给出虚假的安全感 |
| echo Rewind、`pause` | 不采纳 | ACP 没有对应能力，不能用文本伪装（C3） |
| echo SubagentPanel、PlanPanel | 不采纳 | 子执行需要提供方报告（C3）；计划已在预览的原生聊天中渲染 |
| echo 重新生成三分法 | 不采纳 | 重放可能重复工具副作用；上方向键取回 prompt 由人决定 |
| echo 崩溃 watchdog、`process_supervisor` | 不采纳 | Emacs 是常驻宿主；ACP 子进程靠 stdio 结束，已有 owner 看护规则 |
| ECA `tool-call-accept-next` | 不采纳 | 与“决定由用户在原生控件提交”的 C19 冲突 |
| HQ `prompt` 启动策略（上游） | 不采纳 | 恢复失败会静默新建对话，违反严格恢复；改在 `+agent-acp--strict-initiate-session` 处选择 |

## 实现要点

- 挂起发生在 `turn-complete` 事件里。上游先同步发事件再进入提问分支，所以清空原生
  队列后它走“没有排队”的路径，不需要改动 pristine 包，也不在回调里提问。
- client 退出用 `add-function :around (process-sentinel …)` 观察，先读活动状态，再让
  acp.el 使挂起请求失败。`delete-process` 可能同步运行 sentinel，故 `agent-shell--shutdown`
  期间用动态变量标记；Remote 的显式断开由 `remote-buffer-disconnected-p` 排除。
- 恢复已保存对话时先握手、不建会话；选定后写入 `:resume-session-id`，走已有的严格
  恢复路径。选择在定时器里进行，因为回调不能读 minibuffer。
- 排队长度变化由 `agent-shell--prompt-queue-enqueue` 和 `-process-next` 的 advice
  上报，管理器和标签因此不再过时。

## 验证

| 检查 | 结果 |
| --- | --- |
| `test/agent-lifecycle-e2e.el` | 新增 30 项，真实 ACP 进程覆盖 C37–C44 |
| Makefile 的窗口、Agent 回归组（含新文件） | 329 项，327 通过，2 跳过，0 失败；去掉新文件时 298 项同样 0 失败 |
| `make research-test` | 全部通过（2 + 165 + 27 + 96 + 102） |
| `python3 test/agent-manager-gui-e2e.py queue` | 通过：真实 child frame、`C-c A q`、面板按键、返回来源、管理器入口、恢复只发送一次、`C-c A j` 落在审批按钮 |
| `python3 test/agent-manager-gui-e2e.py manager` | 通过 |
| `make state-check` | 20 行差异，全部是 4 个新键在各 keymap 的绑定 |
| Doctor | 0 errors，0 warnings，0 key conflicts；`tools/agent` 无 policy hit |
| 相关文件字节编译 | 无警告 |

全仓 `test-all` 共 137 个文件（不含名称带 e2e、live、smoke 的文件），所有 agent 文件
通过。`init-activity`、`init-neo-common`、`init-performance`、`lean-infoview` 和 `lean-tests`
在 10-31 基线中已失败；`portal-focus-gui` 需要 GUI。运行中另外发现 3 个文件失败，查明
都是测试问题，已在 `a0b05b8a` 修复：`remote-transfer` 的假后端没有跟上 `2d600f4b` 的
`:project` 签名；LaTeX 与 LSP 各有一项桩替换未缓存 trampoline 的原语，Emacs 在安装桩之后
才编译 trampoline，编译器调用到了桩。

## 验证边界

- **真实 SSH 未实测。** `agent-remote-routed-*` 在客户端无法直接访问的 Remote target
  上走完 `/fs:` 路由、target 上的进程、队列、改动路径、退出和严格恢复，但进程仍在
  本机，没有网络与断线。`AGENT_REMOTE_TARGET=<target>` 运行
  `test/agent-lifecycle-remote-live.el` 会把 fixture 放到目标的 `/tmp`，本轮写入远程被
  权限策略拒绝，该测试没有运行过。[Remote parity](../../acceptance/remote-parity.md)
  的韧性部分因此仍未验收。
- 没有向真实提供方发送 prompt，停止原因等行为来自协议 fixture。
- 没有像素级视觉验收，GUI 断言基于真实 frame、窗口和文本。
- 在临时拼接的大组合里，`output-workflow-compilation-is-owned-by-current-frame` 失败，
  单独运行与 Makefile 分组均通过，判定为测试之间的顺序干扰，未处理。
