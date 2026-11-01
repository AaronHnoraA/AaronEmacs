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
| ECA `tool-call-accept-next` | 不采纳 | 按键确认本身就是用户的决定，不违反 C19；不做的理由是 `/next` 已落在原生控件上，再按其 `y`/`n` 即可，无需另开入口 |
| HQ `prompt` 启动策略（上游） | 不采纳 | 恢复失败会静默新建对话，违反严格恢复；改在 `+agent-acp--strict-initiate-session` 处选择 |

## ECA 的交互逻辑与 HQ 的 Emacs 融合

用户明确了两者的学习重点：ECA 学交互和逻辑，HQ 学与 Emacs 的融合及 API。随后又要求
整理全部入口，不再开新口子，并指出日常只从 `agent-shell` 指令进入。

| 来源 | 学到的做法 | 落地 |
| --- | --- | --- |
| HQ 依附 agent-shell 的公开语义，自己不定义入口 | 入口语义要清楚 | 曾把 `agent-shell` 改成召回开关，用户更正：它就是用来打开新会话的。C49 定为总是新建，`C-c a` 负责召回和返回 |
| HQ 只用少量公开 API（`agent-shell-buffers`、`agent-shell-status`、`agent-shell-cwd`） | 不和上游的键和语义竞争 | C48 归还 `C-c C-t` 等原生键，`C-c C-c` 走统一停止 |
| HQ Peek 与 Toggle 共用同一份状态和列表 | 一份数据，多个视图 | C47 一张动作表生成前缀、菜单、斜杠命令 |
| ECA `eca-chat-mode-map` 让聊天内就能完成全部操作 | 动作要能在输入旁完成 | 沿用已有 `/resume`，本地斜杠命令从动作表生成，生成中也可用 |
| ECA `key-pressed-return` 按光标上下文决定 RET | 同一个键随场景做对的事 | 原生按钮和表单已是如此，不另加；`?` 在输入区外开菜单 |
| ECA `go-to-next-attention` | 跨项目注意导航 | `/next`，同一张表 |

整合后删除的重复入口：`+agent/peek`（管理器已是真实 buffer 预览）、鼠标的
`+agent-acp-menu`（右键打开同一菜单）、`Q` 继续队列（面板 `r`）、聊天内
`C-c C-a/x/r/k/w/f/t/q/d/j`、`C-c M-k`、`C-c ?`。

`C-c A` 的迁移：`a`→`n`，`n`→`N`，发送类全部进 `c` 且保留原字母（`x`→`c x`，
`s`→`c s`，`r`→`c r`），`.` 现在是菜单，`b` 删除。这些旧字母与管理器和菜单的含义冲突
（`s` 停止与发送、`x` 结束与发送、`a` 审批与新建），是这次改动的原因。

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
| Makefile 的窗口、Agent 回归组，含 `test/agent-lifecycle-e2e.el` | 335 项，0 失败，2 跳过 |
| `make research-test` | 2 + 207 + 27 + 96 + 102，全部通过 |
| `test/agent-manager-gui-e2e.py` 的 `queue`、`manager`、`display` | 全部通过：真实 child frame、按键、返回来源、双 frame、重启 |
| `test/agent-manager-gui-e2e.py entry` | 通过：`agent-shell` 总是新建，`C-c a` 召回和返回，回合中输入 `/queue`、`C-c C-c` 停止并挂起、`?`、`/agents`、`C-u C-u` 单一选择器 |
| 全仓 `make test-check` | 140 个文件，所有 agent 文件通过；6 个未过见下 |
| `make state-check` | 不是零差异，因为包含按键迁移和并行工作；非按键差异只有 `tools/agent/actions`、Portal 的 `C-c C-l` 和 Dashboard 按钮 |
| Doctor | 0 errors，0 warnings，0 key conflicts；`tools/agent` 无 policy hit |
| 相关文件字节编译 | 无警告 |

未过的 6 个文件都不是 agent 文件：`init-activity`、`init-neo-common`、`init-performance`、
`lean-infoview`、`lean-tests` 在 10-31 基线中已失败；`portal-focus-gui` 需要 GUI。

过程中修复的测试问题：`remote-transfer` 的假后端没有跟上 `2d600f4b` 的 `:project` 签名；
LaTeX 与 LSP 各有一项桩替换未缓存 trampoline 的原语，编译器调用到了桩；
`output-workflow-compilation-is-owned-by-current-frame` 默认没有打开的 workspace，
与 agent 测试同进程时走了 task 路径。

审查并行工作（C45、C46、Pi 项目标签）的管理入口时修复两处交界问题：池查询默认排除 Pi
后，`/next` 会漏掉等待审批的 Pi；从管理器菜单新建没有收起管理器，也没有用选中会话的
workspace。另读过 `views.el`、`ui/windows/actions.el`、Portal 改动和 `+agent-acp-read-transcript`，未发现
问题。对 `tools/agent` 和 Pi 的其余代码按本轮缺陷类型做针对性检查：回调里的同步提问、
阻塞等待、无 owner 的计时器，未发现新问题。这不是对 `949bd443` 和 Noema 提交的逐行审查，
删除的 Research 文件和 Jupyter 改动不在范围内。

## 验证边界

- **真实 SSH 未实测。** `agent-remote-routed-*` 在客户端无法直接访问的 Remote target
  上走完 `/fs:` 路由、target 上的进程、队列、改动路径、退出和严格恢复，但进程仍在
  本机，没有网络与断线。`AGENT_REMOTE_TARGET=<target>` 运行
  `test/agent-lifecycle-remote-live.el` 会把 fixture 放到目标的 `/tmp`，本轮写入远程被
  权限策略拒绝，该测试没有运行过。[Remote parity](../../acceptance/remote-parity.md)
  的韧性部分因此仍未验收。
- 没有向真实提供方发送 prompt，停止原因等行为来自协议 fixture。
- 没有像素级视觉验收，GUI 断言基于真实 frame、窗口和文本。
