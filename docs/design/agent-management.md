# Emacs Agent 管理

## 范围

Emacs 维护可供普通源码 buffer、Portal、知识库、文档工具和 Noema Research
复用的 agent 接入、会话管理与交互。Noema 维护 Markdown、知识库及研究文档的
业务语义，通过 Emacs 的公共接口调用和组织 agent。

ECA、agent-shell-hq 与 echo-agent 是源码研究对象，不是新增的运行时依赖。研究结论见
[ECA 与 HQ 审计](../records/audits/eca-hq-agent-management-2026-10.md)和
[echo-agent 审计](../records/audits/echo-agent-management-2026-11.md)。

## 模型

| 层 | 所有权 | 不同层之间的交付 |
| --- | --- | --- |
| 外部 agent 或模型运行时 | 模型请求、工具循环、原生对话历史、原生压缩与子代理 | 提供商支持的协议与明确的能力报告 |
| Emacs 通用 agent 层 | target/workspace、进程与连接、物理会话、模型选择、队列、管理视图、编辑器上下文、注意状态、通知策略 | provider adapter 与公共会话操作 |
| Noema Research | WorkNode DAG、RunSpec 冻结、研究上下文选择、持久 Run、Proposal、Finding、文档写回及研究权限事实 | 提交执行请求、消费事件和结果、附加业务关联 |
| Noema 知识库和编辑器 | Markdown、笔记、搜索、引用与局部审阅 | 调用通用 compose、context 与 agent 操作 |

通用会话与研究 Run 分别拥有身份。物理进程、原生 Session ID、Emacs 会话、
Research 的逻辑 Session 和 Run ID 不可互换。关闭视图只改变显示；中断回合、
结束会话、归档研究记录是不同操作。Remote 框架拥有进程放置、路径、环境与恢复。

通用实现位于 Emacs。Noema 通过薄适配关联 Research 身份和文档动作；普通会话
操作不会启动 Noema host，也不会创建研究项目。

### 工作流

`agent-shell` 指令用来打开新会话，`C-c a` 用来回到已有会话，两者分工不同。

| 动作 | 入口 | 结果 |
| --- | --- | --- |
| 新建 | `agent-shell` | 总是在当前窗口新建，不是开关 |
| 选择 | `C-u C-u agent-shell`、`C-u C-c a` | 选池成员、新建或已保存对话 |
| 召回 | `C-c a` | 回到本项目最近的会话；空池时选择新建或恢复 |
| 返回 | 在聊天里 `C-c a` | 收起聊天，回到来源窗口；agent 继续工作 |

进入聊天之后，所有操作都在 agent-shell 自己的工作方式里：

| 想做的事 | 方式 |
| --- | --- |
| 说话、排队、steering | 在输入区提交；生成中也可以，`M-RET` 走另一条路 |
| 不离开输入区操作会话 | 本地斜杠命令 `/queue`、`/next`、`/changes`、`/resume`、`/fork`、`/rename`、`/restart`、`/new`、`/side`、`/agents`，在 `/` 补全里列出 |
| 看所有动作 | 输入区外按 `?`，或任意位置 `C-c C-z`，打开同一菜单 |
| 停止回合 | agent-shell 原生的 `C-c C-c`，走统一的停止路径 |
| 模型、模式、思考级别 | agent-shell 原生的 `C-c C-v`、`C-c C-m`、`C-c C-t` |
| 回到编辑区、切 tab、管理器 | `C-c C-o`、`C-c C-n`/`C-c C-p`、`C-c C-l` |

需要跨会话时打开管理器（`H-A`、`SPC a a` 或 `/agents`）。不在聊天里时，
`C-c A` 加同一字母直接执行菜单里的动作，`C-c A c` 进入发送上下文。

### 日常入口与视图

`/next`（`C-c A j`）跳到下一个等待你的会话，待审批和问答直接落在原生控件上，
按其上的 `y`/`n` 决定；`/queue`（`q`）管理排队的 prompt；`/changes`（`l`）打开这个
会话改过的文件；`/resume`（`h`）恢复已保存对话，没有活动会话时也可用。
`H-A`（macOS Option-Shift-A）、`SPC a a` 和 `M-x agent-manager` 打开项目池管理器，
加前缀查看所有项目；从管理器里再次执行返回。`C-c A S/G/U` 分别选择同一浮窗的
All、Inbox、Usage 页签。GUI 使用 Aaron ETAF，按 target/workspace 分组；
Interactive、Portal、Worktrees、Noema Research 及 Previous generations
是会话行上的来源标签。左侧是可折叠的会话
导航，右侧直接显示原生聊天 buffer，保留其工具、审批、表单与输入。单击、Tab
或 j/k 导航时只预览，不清未读；Return 确认进入正常会话窗口，q 返回来源。
浮窗不占用来源窗格；TTY 使用 `+agent/sessions-table` 后备。

`i` 输入，`a` 定位待审批/问答，`n` 在选中会话的执行目录新建，`h` 原生历史
（没有活动选择时恢复已保存对话），`Q` 队列，`l` 改动的文件，
`/` 筛选，`p` 当前项目/全部，`z` 折叠组，`g` 刷新。`.` 或 `C-c C-z` 打开共享
Transient：停止、重连、改名、结束、按能力提供 fork、模型与队列；支持时
另有 Research 的工作导航、归档和上下文换代。`X` 停止同 target、workspace
项目池中的当前工作，各会话仍经过自己的业务 owner。聊天正文 `?` 打开同一菜单，
输入区保留问号；ETAF 的 `?` 显示原生快捷键帮助。

选择、搜索、滚动和折叠属于视图状态，刷新保留稳定实体。隐藏退订事件并释放
计时器及临时预览窗口；关闭还释放 UI Runtime。两者都保留 agent 进程、输入与队列。
Usage 仅读取已上报数据；未知值不显示为零。`+agent-top` 的 GUI 入口也指向该页签；
需要完整配额、远程控制诊断时，在操作菜单选 Detailed usage 或调用
`+agent-top-report`。这类显式诊断保留原有取数与控制能力。

普通会话默认显示在当前 frame 的右侧 Agent dock。A 切到 B 复用当前聊天窗格，
不改其他手工分屏；没有当前聊天时先复用本 frame 的已有视图，再分配空闲侧窗。
执行会话共用项目标签；Pi 使用独立的作用域标签和 Pi Sessions 入口。
显式移入/移出 popup、here 和领域干预保留放置意图。
返回点属于实际窗口，隐藏 embedded 视图恢复原编辑内容，隐藏 dock 撤下窗格。
各入口统一使用
`C-c C-e` 聚焦输入，`C-c C-o` 隐藏并返回，`C-c C-l` 打开管理器。原生常驻
输入区在生成过程中仍可编辑，提交走 provider 的 steering 或 agent-shell 队列。
通用收起通过 `+buffer-hide-function` 进入 Agent 控制器；控制器按实际窗口的
popup / dock / embedded 类型返回，Portal 保留用户调整后的高度。

同一项目新建两个会话，就有两个真实 buffer 和两个 client；标签、管理器预览与
多窗口展示只引用这些会话，不合并对话，也不为显示另起执行。终端的原生视图保持
单实例；Agent 的普通 Emacs buffer 可由用户在多个窗口展示，各窗口独立返回。

### 生命周期

| 操作或状态 | 执行与输入 | 视图与资源 |
| --- | --- | --- |
| starting | 握手未结束，不能当作空闲或自动回收 | 展示真实初始化状态 |
| 隐藏 / 关闭管理器 | 执行、会话、草稿和队列保留 | 返回来源，释放该 UI 的订阅和计时器 |
| 隐藏聊天 | 执行保持，原生 buffer 保留 | 撤下该视图，不终止 client |
| 排队 | 回合正常结束后由原生队列发送；面板可编辑、排序、立即发送 | 面板只拥有选择，关闭不影响队列 |
| 停止回合 | 先交给业务 owner，否则发送一次原生取消；没有运行中的回合时拒绝 | stopping；5 秒没有确认则 stop-unconfirmed |
| 回合异常结束 | `refusal`、`max_tokens` 等不算完成；队列挂起 | failed 提醒，`:stop-reason` 记录事实，下次输入清除 |
| client 退出 | 活动回合结果未知，为 interrupted，不重放；队列挂起；提交输入或重连恢复 | 当下更新视图并按 failed 提醒；过期请求不再待处理 |
| 停止未确认 | 仍可能运行，不伪造完成或自动重放 | 可检查、重启或明确结束；不自动杀其他回合 |
| 重启 / 重连 | 同一原生 ID，保留草稿、workspace 环境，队列转入挂起；活动请求不重放 | 保留显示归属；启动异常的未发送输入进入 `*Agent recovery*` |
| Research 重启 | 先停止 Run 并等待业务终态，再调用同一物理重启 | 原有 adoption hook 关联持久身份 |
| 结束会话 | 经过忙碌、队列和草稿确认；native cleanup 结束 client | 释放 buffer 订阅和计时器，provider 历史保留，可用 `C-c A h` 恢复 |
| 退出 Emacs | 有运行、草稿或队列时询问一次；临时会话不算 | 确认后各会话走普通结束 |

Agent buffer 以 `+buffer-end-function` 登记 `+agent-acp-close`，使通用的
`H-q` 结束动作与会话菜单使用同一确认和清理；改名不影响动作。取消确认保留
原会话、进程和草稿。关视图与 Agent 收起仍只改变显示。
Agent 的工作确认登记在本 buffer 的 `kill-buffer-query-functions`，视图与业务
关闭观察者登记在 `kill-buffer-hook`。菜单、Ibuffer、Perspective 与原生 kill
共用这些检查；已确认的内部强制结束只跳过 query，仍执行相同清理。原生
agent-shell 保留物理 client 的 cleanup；通用层不重复实现执行清理。

取消观察绑定当时的物理 state 和执行 token；新输入、回合结束和清理撤销观察。
订阅回调只在原 buffer 与原 state 仍匹配时执行。延迟登记和旧会话退役各自持有
计时器；buffer 结束时取消。退役回调重新检查身份、握手、活动、队列、草稿与可见性。
自动回收必须同时满足握手结束、无协议/业务活动、无待发队列和草稿、未显示且
来源允许回收，不能以静默或单个 busy 标志判断。

### 通知

agent 事件通过 `tools/agent/notifications` 交给 `aaron-notify`。默认策略：

| 事件 | 历史 | 主动提醒 |
| --- | --- | --- |
| 正常完成 | 一条可跳回会话的记录 | 默认无 |
| 等待审批或回答 | 记录，按会话去重 | 离开 Emacs 后，延迟并合并提醒 |
| 执行失败 | 记录 | 离开 Emacs 后，延迟并合并提醒 |
| 进度、token、工具流、开始 | 不逐项写通知 | 无 |
| 用户取消、短暂且已解决的等待 | 取消待发提醒 | 无 |

提醒不切换窗口、不聚焦 frame、不执行审批。正文只含会话标签和事件类型，
不复制 prompt、回复或工具输出。点击才执行跳转：单条通知落在仍未解决的原生
控件上，合并通知打开 Inbox。前台只记历史，保留用户正在
阅读的回显区。多个会话在合并窗口内等待时只发一条汇总；持续事件不会重复发送。

`aaron-notify` 的 `:silent t` 只记历史，不改未读数，不显示回显或系统通知。
原有调用默认保留原行为。批处理从不发送系统通知。系统后端及其点击能力的
实际限制仍见 [macOS 边界](macos-boundary.md)，接入 agent 不改变系统授权状态。

## 契约

- **C1 通用能力由 Emacs 维护。** 无需 `.noema`、Noema host 或研究 Project 就能
  启动、查看、切换、停止普通会话，及记录和处理其通知。Noema 的接入适配位于
  `lisp/noema/bridge/`；通用模块不得加载 Noema 来确定通知策略。
- **C2 业务语义由调用方维护。** Noema 保留 RunSpec、DAG、证据及结果写回。
  通用层不把会话树当 WorkNode DAG，不把会话完成当研究任务完成。Run 的完成
  通知等待 host 确认终态；ACP 回合结束不替代该确认。
- **C3 按能力暴露操作。** list/load/fork、usage、steer、权限回调等以 adapter
  的真实报告为准。未报告为 unavailable；不能用 buffer 存在推断进程存活，
  不能把静默当故障，不能把历史恢复当附着到另一客户端的活动执行。
- **C4 保留原生执行权威。** 已有 agent-shell/acp/shell-maker 依赖继续按组锁定。
  ECA 的工具循环、模型请求与子代理调度用于研究，不复制成与 CLI 竞争的第二套
  执行权威。借鉴 UI 时复用真实 buffer、原生队列与权限回应。
- **C5 遵守本地 UI 体系。** 使用 Aaron UI、原生 buffer、Transient 和现有窗口
  生命周期。新交互界面遵守 [Elegant UI](elegant-ui.md)。预览可取消并恢复原
  窗口；分组使用 target/workspace 身份，不另启 workspace 管理栈。
- **C6 注意状态与运行状态分开。** running、stopping、idle、failed 和等待
  permission/input 分别表达。读取清除未读，不解决待审批，也不抹去失败事实。
- **C7 默认安静。** 通用通知在前台只写有界历史；正常完成默认仅留历史。
  权限、输入和失败才有后台提醒资格。使用 `+agent-notify-events` 配置资格，
  `+agent-notify-delay` 合并短暂等待，`+agent-notify-cooldown` 限制提醒频率。
- **C8 不发送过时提醒。** 答复、继续执行、取消和结束 buffer 会撤销待发事件。
  同一会话同一状态只记一次；有新一轮工作才允许再次提醒。合并计时器在没有
  待发事件、模式关闭或 Emacs 退出时释放。
- **C9 通知不接管执行。** 通用控制器只消费结构化事件。Noema 的语义事件经
  适配器进入同一控制器，Run 与原生 ACP 通知互斥，避免双报。通知失败不得改变
  权限决定、结果和执行状态。原有自定义 Noema 通知回调仍归用户所有。
- **C10 运行时独立。** `tools/agent/acp`、渲染、用量面板、worktree 和普通会话
  管理不加载 Noema。项目目录、会话登记后的业务绑定、元数据、关闭清理和业务
  动作通过显式钩子扩展。普通项目的列表、改名、重启、原生 fork、预览和停止
  使用同一套 Emacs 会话数据；Noema 的持久归档及工作块导航只在业务适配中添加。
- **C11 管理入口直接且统一。** `H-A`、`SPC a a`、`agent-manager` 打开同一
  管理器；分组包含 target/workspace，筛选不改执行事实。GUI 使用 ETAF 公共组件，
  原生列表作为 TTY 后备。测试：`agent-manager-c11-*`。
- **C12 管理器只拥有视图。** 关闭或隐藏不结束 agent；选择和刷新不清未读，
  隐藏释放 UI 事件与计时器，重新打开补画。测试：`agent-manager-c12-*`。
- **C13 重启保留意图。** 原生重启传同一 session ID，保留显示归属、环境、草稿
  和原生队列；活动请求不重放。Research 的 Run 尚未终结时先要求停止。创建失败
  时保存未发送输入供恢复。测试：`agent-manager-c13-*`。
- **C14 回收保护所有工作。** 初始化、RPC、审批、问答、Run、队列、草稿或可见
  任一条件成立时不得自动回收。测试：`agent-manager-c14-*`。
- **C15 取消与回调有归属。** 重复停止不重复发送；超时只表达未确认。新回合、
  清理和 state 更换使旧观察与回调失效；订阅在所属 buffer 中执行。
  测试：`agent-manager-c15-*`。
- **C16 返回不破坏布局。** 普通会话只增加一个右侧 dock；聊天和 popup 的输入、
  隐藏按键一致。隐藏返回来源窗口并保留执行。测试：`agent-manager-c16-*`。
- **C17 日常管理共享一个界面。** `C-c A S/G/U` 分别打开同一管理器的会话、
  待处理与用量视图；项目可折叠，选择、筛选和折叠在刷新后保留。表格仅作 TTY
  后备与显式诊断入口。新建默认使用选中会话的 workspace。
- **C18 预览保留原生交互。** 管理器左侧选择，右侧显示同一真实 agent buffer。
  单击和 j/k 只预览；RET 确认进入，q 返回来源。预览不清未读，不接管显示归属，
  隐藏、关闭及会话失效均释放预览窗口，不让隐藏 frame 阻碍自动回收。
- **C19 操作菜单可发现。** 管理器的 `.` 与聊天正文的 `?` 使用同一分组 Transient；
  `C-c C-z` 两处通用。ETAF 的 `?` 保留原生快捷键帮助，聊天输入区仍可输入问号。
  待审批/问答可以直接定位原生控件，决定由用户提交。
  状态不支持的动作不得伪装成可用。全流程需覆盖真实 stdio 与原生聊天控件。
- **C20 各来源共享会话管理。** 普通、Portal、worktree 和 Noema Research 会话
  都由 Emacs 的注册表管理。领域适配只提供只读分组标签、业务身份和能力；快照
  不查询或启动 Noema host。旧物理代际单独标识，不能混作同名活动会话。
  会话切换复用当前视图，不删除用户手工保留的其他窗口；管理器临时预览不改变
  常规视图的来源与位置。
- **C21 提交上下文可展开。** 聊天先显示用户正文；引用、Skill 和记忆放在原生
  可折叠的 Context 区，默认收起。展示摘要不改变冻结输入、协议负载或持久来源。
- **C22 回收调度归 Emacs。** 通用层按 target/workspace 保留后台可恢复会话，
  使用一个有归属的单次计时器；没有后台候选或 Emacs 退出时取消。每次真正回收前
  重新检查初始化、RPC、Run、请求、草稿、队列、可见性与原生恢复能力。
  Pi 不参与暖会话清扫，个人会话默认保留。旧 Noema 设置和清扫命令为兼容入口，
  不再维持第二个周期计时器；Noema 仍决定研究项目何时结束。
- **C23 一个项目一个会话池。** 池以 Remote target 与规范化的已登记项目根为
  身份，成员来自同一物理会话注册表；普通、popup、here、worktree、Noema worker
  共用执行会话池。Pi 按全局／项目作用域独立管理，不进入执行池的 tab、选择器、
  管理器与批量停止。物理注册表仍包含全部会话；显式 API 可查询全部公共对象。
  池切换不创建进程，停止逐个调用执行 owner；私有导出会话不进入日常池。
- **C24 显示只改目标视图。** 从 agent A 切换 B 时复用当前聊天窗格，默认不由
  B 曾经的 popup/here 回调重新决定位置。其他手工分屏的会话和编辑窗格保持原样。
  编辑器发起显示时复用本 frame 的已有 agent 视图，未有时才增加右侧 dock。
  明确的新建、移入/移出 popup、独立干预窗口保留显式放置意图。
- **C25 返回按窗口维护。** 来源属于实际视图窗口，并与 frame 及显示的 buffer
  校验；A→B 不丢失来源。隐藏 dock 删除其视图；隐藏 embedded 视图恢复非 agent
  内容；绝不以 buffer 历史里的另一会话充当返回。失效窗口不重建，旧来源不抢占
  已被用作编辑器的窗格。没有来源历史的最后主窗格走通用项目目录/首页回退。
  每个 frame 独立维护。
- **C26 预览只拥有临时窗口。** 从预览发起任何会话的常规显示先回到所属父 frame，
  不在 child frame 新建 dock。从其他 frame 打开管理器重新定位到调用方，退出回到
  当次来源；旧预览、焦点和订阅不跨 frame 留存。
- **C27 日常入口召回项目池。** `C-c a` 召回当前项目最近会话，在 agent 中再次
  执行返回编辑区；加前缀选择池成员，空池明确选择或新建，不隐式启动。会话层
  直接查询 `+agent-pool-sessions`，新建调用 `+agent/start`，显示和返回调用
  `+agent-acp-show-buffer` / `+agent-acp-hide`。管理器为池总览与跨项目检查入口。
- **C28 所有结束入口一致。** 原生 buffer query 保护忙碌、队列和草稿，拒绝后
  不改视图、通知业务或结束 client；确认后关闭观察者与视图清理各执行一次。
  通用收起使用 Agent 显示控制器。测试：`c32-*`、`agent-close-c28-*`。
- **C29 既有入口覆盖原生输入。** `C-c a` 和 `C-c A` 从 Portal 原生终端转交到
  同一 Agent 命令及前缀；普通 Emacs 的 Normal、Insert、Emacs 状态保留同一入口。
  不另增混合管理键。测试：`agent-entry-c29-*`。
- **C37 停止和失败挂起队列。** 回合以 `end_turn` 结束才自动发送原生队列。取消、
  异常结束（`refusal`、`max_tokens` 等）、错误、client 退出、重启，
  以及 Research 的结构化回合结束（它绕过原生发送，下一步归 Run owner）把待发
  prompt 移到 Emacs 持有的挂起区，事件回调不提问。Run 期间在输入区提交的内容
  只进队列，不 steering 也不并发第二个 prompt，以保持 Run 的冻结输入；此后的普通回合不带走它们，由人在队列
  面板恢复。计数同时包含挂起区，结束确认和恢复记录也保护它。异常结束不算完成：
  `:stop-reason` 记录事实，并按 failed 提醒。测试：`agent-queue-c37-*`。
- **C38 client 退出是即时事实。** 进程结束的当下更新视图、挂起队列并按 failed
  提醒，不等下一次提交或卡死检查。已退出 client 的请求不再是待处理或忙碌，
  `:turn-state` 为 `interrupted` 表示结果未知，不重放；停止与审批入口明确拒绝。
  自身 `shutdown`、结束会话和 Remote 的显式断开不是故障。测试：`agent-process-c38-*`。
- **C39 队列可以管理。** `C-c A q`、会话菜单 `q`、管理器 `Q` 打开同一队列面板，
  能查看、编辑、删除、排序、挂起、恢复和立即发送；面板只改变待发文本，关闭不
  影响队列。立即发送在忙碌时走 steering，适配器不支持则拒绝，不擅自打断当前回合。
  测试：`agent-queue-c39-*`。
- **C40 注意导航直达请求。** `C-c A j` 按审批、问答、失败、断开、未读的顺序跨项目
  循环，Pi 也在其中，带前缀只看当前项目；Inbox 同序。待审批和问答直接落在原生控件上。点击
  单条通知同样直达仍未解决的请求，点击合并通知打开 Inbox。测试：`agent-attention-c40-*`。
- **C41 管理器说明会话在做什么。** 快照报告提供方标题、最近 prompt、待决请求的
  标题、已运行时长、停止原因和队列数；行与详情直接显示，筛选也覆盖标题和 prompt。
  这些只读内存状态，不解析正文、不调用模型。通知正文仍不含 prompt。
  测试：`agent-manager-c41-*`。
- **C42 找回已保存对话不需要活动会话。** 关闭最后一个会话或重启 Emacs 后，
  `C-c A h`、池选择器的恢复项按提供方的原生列表选择并严格恢复，不另存一份会话
  记录；取消不留下 buffer，恢复失败不改建新对话。测试：`agent-resume-c42-*`。
- **C43 退出 Emacs 保护工作。** 有运行中回合、草稿或队列的会话时，
  `save-buffers-kill-emacs` 只问一次，并列出会话；临时会话不算。批处理不询问。
  测试：`agent-exit-c43-*`。
- **C44 会话改动可查。** 每个会话按报告的工具 diff 和写文件事件累积有界的改动
  清单，`C-c A l`、会话菜单 `l`、管理器 `l` 选择并打开文件。清单是这个 client 的
  内存事实，不是 Git 状态，也不跨重启。测试：`agent-changes-c44-*`。
- **C45 管家复用执行接口。** `tools/agent/api` 在现有注册表和 ACP 上提供有界的
  list/status/create/send/read/stop/show；列表不返回 prompt、草稿或完整对话。
  除 list/create 外，调用者必须给出 buffer 和匹配的 nativeSessionId；可额外限制
  root/target。创建只初始化 client，发送复用原生队列且不自动清空挂起任务，读取
  只分页返回 transcript 并排除输入草稿。后台操作不选择窗口，不代理权限决策。
- **C46 空闲路径有界。** 不相关会话的状态变化不扫描暖会话候选；已调度的活跃
  候选复用现有单次计时器。Pi 的所有管理来源始终排除自动回收。恢复能力查询
  只读 ACP 能力字段，不为两个布尔值构造完整会话快照。API 和原生回合完成均
  通知公共订阅者，按确切回合去重；草稿检查不复制整段输入。
  原生历史查询不代表模型忙碌，不扣住新输入；自动回收仍保护所有在途 RPC。
- **C47 动作只有一张表。** `tools/agent/actions` 是所有 agent 动作的唯一列表，
  `C-c A`、会话菜单（`?`、`C-c C-z`、管理器 `.`、鼠标右键）和本地斜杠命令都由它生成；
  同一字母处处是同一动作。管理器的 `j`、`k`、`q` 是导航，队列因此是 `Q`。新增动作
  只改这张表，不另开键或菜单。测试：`agent-keys-c47-*`。
- **C48 不抢占 agent-shell 的键。** 聊天里的会话键只保留视图移动（`C-c C-e/o/n/p/l/z`），
  其余动作走菜单或斜杠命令。`C-c C-c` 重映射到统一的停止路径，Run 由其 owner 停止。
  测试：`agent-keys-c48-*`。
- **C49 `agent-shell` 打开新会话。** 它总是在当前窗口新建，在聊天里执行也一样，不做
  开关；`C-u C-u` 进入选择器。召回和返回只属于 `C-c a`。本地斜杠命令在提交前
  截获，生成中也可用，不进入模型或队列；提供方报告的同名命令优先，只有 `/resume`
  例外，因为显示哪个对话归 Emacs。测试：`agent-entry-c49-*`。

Agent 的真实聊天 buffer 是用户 buffer，名称不使用 Emacs 内部 buffer 的空格前缀。
它们进入全局 Ibuffer 的 Agent 分组和日常 Consult buffer 选择器；不进入全局 tab
只由 `tab-line-exclude` 控制。加载会话层时会迁移已有的旧管理名称，并同步
shell-maker 的目标名，不更换 buffer、进程或原生 session ID。

## 实现位置

| 职责 | 文件 |
| --- | --- |
| 依赖、Remote 接入与配置登记 | `lisp/tools/agent/config.el` |
| 物理会话、能力快照、项目池、队列与暖会话回收 | `lisp/tools/agent/acp.el` |
| 管家调用公共会话的语义接口 | `lisp/tools/agent/api.el` |
| 常规视图放置、来源返回、frame 边界与物理替换 | `lisp/tools/agent/views.el` |
| 会话列表、Inbox、可取消预览与通用操作 | `lisp/tools/agent/sessions.el` |
| ETAF Agent Manager、分组筛选和 UI 生命周期 | `lisp/tools/agent/manager.el` |
| 按能力提供的原生会话与领域动作菜单 | `lisp/tools/agent/menu.el` |
| 排队 prompt 的编辑、恢复与面板 | `lisp/tools/agent/queue.el` |
| 所有动作的唯一列表，生成前缀、菜单和斜杠命令 | `lisp/tools/agent/actions.el` |
| 渲染、用量、活动和工作树 | `lisp/tools/agent/{render,top,activity,worktree,worktree-board}.el` |
| 原生引用式上下文与 Markdown prompt 文档入口 | `lisp/tools/agent/{compose,context}.el` |
| 完整 gptel/Magent 源码与来源 | `site-lisp/{gptel,magent}/`、`site-lisp/AGENT-UPSTREAMS.md` |
| 通用通知策略与原生 ACP 事件订阅 | `lisp/tools/agent/notifications.el` |
| Noema 事件到通用通知的适配 | `lisp/noema/bridge/agent-notifications.el` |
| 通用通知历史与平台路由 | `lisp/core/aaron-notify.el` |
| Research 业务身份和知识库适配 | `site-lisp/noema/lisp/{noema-agent-acp,noema-context}.el` |
| Research 语义事件、持久会话与工作流 | `site-lisp/noema/lisp/{noema-agent-worker,noema-sessions}.el` |

会话快照包含 provider/native session、target/workspace、真实进程状态、busy、
attention、unread、错误事实、来源组、退役代际、usage、提供方标题、最近 prompt、
待决请求标题、回合起始时间、停止原因、队列与挂起数、改动文件数，和原生
list/load/resume/fork/steer 能力。Noema 通过显式钩子补充业务 ID、
注意原因、动作和菜单；通用选择器不启动或恢复会话。原生 fork 需要正向能力报告。

`H-A` / `SPC a a` 打开日常管理器；`C-c A S/G/U` 选择同一视图的页签。列表只在可见时合并刷新。Noema 的持久记录仍由其业务视图管理。

## 已知差距

G4–G9 与 G11 的项目池、显示归属、返回、预览及物理替换问题已按 C23–C27
修复；原始复现和真实多 frame 验证见
[项目池与窗口审计](../records/audits/agent-project-pool-display-2026-10.md)。

- **G1 提供商事件覆盖。** 普通 ACP 的 elicitation 已映射为 input；没有报告
  结构化事件的提供商仍不能通过解析聊天正文推断等待回答。
- **G2 外部执行附着。** 已在另一客户端执行的任务依旧只能显示真实的忙碌事实，
  不能把历史恢复伪装成附着或复制一份执行。
- **G13 真实 SSH 实测。** C37–C44 已在客户端无法直接访问的 Remote target 上经 `/fs:`
  路由验证（`agent-remote-routed-*`），但没有真实网络与断线。
  `test/agent-lifecycle-remote-live.el` 覆盖真实目标，需要允许写入目标的 `/tmp`，尚未运行。
- **G3 状态迁移。** 模型和配额偏好从旧 Noema 路径复制到 `var/agent/`，保留原文件；
  现有 worktree 使用的 `noemaBase/noemaGroup/noemaAgent` Git 元数据保持可读写，
  避免改名使已存在的分支失去归属。它们不要求 Noema 运行。

原生 transcript 与 Noema 的不可变 RunSpec 保留各自的权威，不新增完整对话副本。

## Pi 与 Markdown 文档入口

Pi 的独立 Session 管理、主／项目助手关系与生命周期见
[Pi 会话与项目助手](pi-sessions.md)；按需披露 API 和 prompt 文档 Skill
见 [Noema 交互](../noema/design/interaction-plan.md)。普通上下文由
`tools/agent/selection` 维护，Noema 提问和改写不加载 gptel；现有 Markdown
编辑器承载 prompt，不再维护专用 Compose 编辑器。
