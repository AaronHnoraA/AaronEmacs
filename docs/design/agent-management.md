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

### 文档协作的设计原则

文档保存工作内容和可检查的结果；批注保存请求、位置和协作事实。看板、Agenda
与图谱从这些事实展示工作，会话承担执行。选区发起任务后，用户仍可留在正文继续工作。

原生化沿用 Emacs 的 buffer、region、命令、会话池、窗口与 Remote 所有权。Noema
负责 Markdown 的呈现与保存；Agent 继续使用自己的运行时。入口提供完成当前操作
所需的最少信息，深入规则按需读取。明确授权的翻译、润色与精简直接编辑；需要人
判断的额外改动才提出审阅。

长期使用依靠稳定的文档与任务身份、可重放记录、原文守护和真实错误。隐藏界面
退订观察，后台执行和持久记录继续存在；重连不自动重放未知结果的请求。会话快照
说明协作者当前在做什么，不能据此证明某条任务已交付。批注结案以明确日志事件为准。

### 工作流

`agent-shell` 指令用来打开新会话，`C-c a` 用来回到已有会话，两者分工不同。

| 动作 | 入口 | 结果 |
| --- | --- | --- |
| 新建 | `agent-shell` | 总是在当前窗口新建，不是开关 |
| 选择 | `C-u C-u agent-shell`、`C-u C-c a` | 选池成员、新建或已保存对话 |
| 召回 | `C-c a` | 回到本项目最近的会话；空池时选择新建或恢复 |
| 切换 | `H-t`（macOS `Option-T`，Noema 页面和 Portal 终端里也可用；`C-S-t` 保留为备用） | 像 `Cmd-T` 选终端：不在屏幕上的会话、新建或历史，`RET`/`S-RET`/`Cmd-RET` 选窗口；在文档里先列这篇的协作者，可为这篇新建（C65） |
| 返回 | 在聊天里 `C-c a` | 收起聊天，回到来源窗口；agent 继续工作 |

进入聊天之后，所有操作都在 agent-shell 自己的工作方式里：

| 想做的事 | 方式 |
| --- | --- |
| 说话、排队、steering | 输入区 `RET` 换行；`Cmd-RET` 发送（生成中 steer），`Cmd-S-RET` 发送（生成中排队） |
| 不离开输入区操作会话 | 本地斜杠命令 `/queue`、`/next`、`/changes`、`/resume`、`/fork`、`/rename`、`/restart`、`/new`、`/side`、`/agents`、`/page`、`/file`，在 `/` 补全里列出 |
| 读公式多的回复 | `C-c A o`／`/page` 把上一条回复作为 Noema Markdown 页面在旁边打开；聊天本身不渲染公式 |
| 留下值得保存的回答 | `C-c A F`／`/file` 把上一条回复交给新建节点工作台，改好标题和位置后成为 Wiki 页面 |
| 看所有动作 | 输入区外按 `?`，或任意位置 `C-c C-z`，打开同一菜单 |
| 停止回合 | agent-shell 原生的 `C-c C-c`，走统一的停止路径 |
| 模型、模式、思考级别 | agent-shell 原生的 `C-c C-v`、`C-c C-m`、`C-c C-t` |
| 回到编辑区、切 tab、管理器 | `C-c C-o`、`C-c C-n`/`C-c C-p`、`C-c C-l` |

需要跨会话时打开管理器（`H-A`、`SPC a a` 或 `/agents`）。不在聊天里时，
`C-c A` 加同一字母直接执行菜单里的动作，`C-c A c` 进入发送上下文。

### 日常入口与视图

`/next`（`C-c A j`）跳到下一个等待你的会话，待审批和问答直接落在原生控件上，
按其上的 `y`/`n` 决定；`/queue`（`q`）管理排队的 prompt；`/changes`（`l`）打开这个
会话改过的文件；`/resume`（`h`）从 Claude Code、Codex、OpenCode、Antigravity 的统一历史恢复或接续对话，没有活动会话时也可用，列表按时间从新到旧（C55、C63）。
`H-A`（macOS Option-Shift-A）、`SPC a a` 和 `M-x agent-manager` 打开项目池管理器，
加前缀查看所有项目；从管理器里再次执行返回。`C-c A S/G/U` 分别选择同一浮窗的
All、Inbox、Usage 页签。GUI 使用 Aaron ETAF，按 target/workspace 分组；
Interactive、Portal、Worktrees、Noema Research 及 Previous generations
是会话行上的来源标签。左侧是可折叠的会话
导航；Sessions 与 Inbox 右侧直接显示原生聊天 buffer，保留其工具、审批、表单与输入。单击、Tab
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
该页签保留左侧会话选择，右侧使用完整浮窗展示上下文进度、输入／输出 tokens、
缓存读写、费用、回合及会话详情，不再占用空间预览聊天；`RET` 仍可进入真实会话。
指标随选择和既有状态事件刷新，缺少容量时不绘制百分比，未报告值显示“—”。
原生 usage 接口把未报告的 token 计数归一为零，页面对这些零值保守显示“—”；
明确给出的费用零值与已知回合零值保留为零，不修改提供商数据。
需要完整配额、远程控制诊断时，在操作菜单选 Detailed usage 或调用
`+agent-top-report`。这类显式诊断保留原有取数与控制能力。

队列采用带序号、正文和 held 状态的紧凑卡片，顶部汇总待发与挂起数量；列表可滚动，
完整文本保留在悬停提示中。原生 Sessions、Worktree attempts、Context 表格保留
原有行身份和动作，以统一标题、数量、可见列标题及空状态呈现。后台任务页使用
ETAF UI 的标准按钮、状态标签与分区；Detailed usage 保留原生诊断面板和控制能力。

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

聊天沿用原生 Markdown 和活动分组折叠。借鉴
[dsh-emacs 的视觉层次与输入反馈](https://github.com/vritser/dsh-emacs/blob/main/docs/ui-styling.md)，
用户标签和原生提示符使用 Aaron 的青色强调，角色标签没有背景徽章，用户正文
保持正文字体，思考和摘要降低对比度；
工具的完成、运行和失败分别使用语义色，Wave、Dragon、Lotus 切换后同步更新。
输入上方显示当前 `Cmd-RET`／`Cmd-S-RET` 的真实提交路径，有队列时显示数量、挂起数和
一小段待发内容；`/queue` 仍是唯一管理入口。连接中和 client 退出分别显示明确提示。
提示只属于显示，不进入复制文本、草稿、协议或撤销历史。`C-a` 在输入首行停在
提示符之后，续行和历史正文保留普通行首移动。`+agent-input-mode` 可在当前聊天关闭提示。

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
  的真实报告或已验证的 provider 协议为准。无已知协议支持的操作为 unavailable；不能用 buffer 存在推断进程存活，
  不能把静默当故障，不能把历史恢复当附着到另一客户端的活动执行。
- **C4 保留原生执行权威。** 已有 agent-shell/acp/shell-maker 依赖继续按组锁定。
  ECA 的工具循环、模型请求与子代理调度用于研究，不复制成与 CLI 竞争的第二套
  执行权威。借鉴 UI 时复用真实 buffer、原生队列与权限回应。
  Google Antigravity 复用 agent-shell 内置的 `antigravity` 配置，以
  `agy acp --dangerously-skip-permissions` 启动，自动批准所有工具权限请求；
  公共 agent ID `agy` 映射到 `antigravity`，普通、Portal、worktree、
  后台执行与 ACP 恢复入口均可选择。认证沿用原生 Google 登录，进程放置仍走
  workspace 的 Remote 环境。`+agent/resume-saved` 可查询其 ACP 原生历史，
  `/resume` 按 C63 纳入统一历史。
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
  清理和 state 更换使旧观察与回调失效；订阅在所属 buffer 中执行。领域审批
  broker 的异步应答只在同一物理 state、client 和原生请求仍待决时转发；停止、
  重启或另一应答已经收口后，迟到决定不得再发送 ACP 回复。
  测试：`agent-manager-c15-*`。
- **C16 返回不破坏布局。** 普通会话只增加一个右侧 dock；聊天和 popup 的输入、
  隐藏按键一致。隐藏返回来源窗口并保留执行。测试：`agent-manager-c16-*`。
- **C17 日常管理共享一个界面。** `C-c A S/G/U` 分别打开同一管理器的会话、
  待处理与用量视图；项目可折叠，选择、筛选和折叠在刷新后保留。表格仅作 TTY
  后备与显式诊断入口。新建默认使用选中会话的 workspace。
- **C18 预览保留原生交互。** 管理器的 Sessions 与 Inbox 左侧选择，右侧显示同一真实 agent buffer。
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
  会话保留各自的 ACP client 与原生 session ID，共享的 backend attachment 由
  Remote 管理。项目拥有的单个 forward/group 丢失时，仅恢复该资源，不借此
  取消同 transport 的 ACP 回合；真实 transport 故障按 Remote 的 pipeline 契约处理。
  项目池查询先按 target、规范根和管理类型筛选物理 buffer，再构造成员快照；
  一个项目的 tab/选择器不读取其他项目会话的完整 prompt、usage 和待决请求。
  本机绝对根从其他 target 的 buffer 中查询也保持本机身份。暖会话回收按同一
  规范 pool key 分组，仅从 buffer 已登记字段取身份，不为分组构造完整快照。
  `+agent-pool-buffers` 与 `+agent-pool-sessions` 查询同一批物理成员；tab 和批量
  关闭只取 buffer，不读完整快照。插件可在 `+agent-session-private-buffer-functions`
  中根据 buffer 已登记字段声明私有会话，不查磁盘或网络；旧的
  `+agent-session-private-functions` 仍收到完整快照，配置它时按需构造候选快照。
  两类过滤经 `+agent-session-private-p` 统一应用于池与管理器，不维持额外成员缓存。
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
  通用收起使用 Agent 显示控制器。逐会话的结束清理（client、输入提示、会话认领、
  延迟计时器、待发提醒）只作用于正在结束的 buffer：kill 钩子依次在当时的当前
  buffer 中运行，其他钩子改变它（删除 frame 会选中另一 frame 并换掉当前 buffer）
  也不得结束另一会话的 client，或让本会话的 client 成为孤儿进程；领域钩子删除
  frame 时保持当前 buffer。结束时仍挂起的结构化回合（Run、LaTeX 导出）以“会话已关闭”
  失败结清一次，调用方不必等自己的超时。测试：`c32-*`、`agent-close-c28-*`、
  `+noema-latex-intervention-kill-hook-keeps-its-buffer-current`。
- **C29 既有入口覆盖原生输入。** `C-c a` 和 `C-c A` 从 Portal 原生终端转交到
  同一 Agent 命令及前缀；普通 Emacs 的 Normal、Insert、Emacs 状态保留同一入口。
  不另增混合管理键。测试：`agent-entry-c29-*`。
- **C37 停止和失败挂起队列。** 回合以 `end_turn` 结束才自动发送原生队列。取消、
  异常结束（`refusal`、`max_tokens` 等）、错误、client 退出、重启，
  以及 Research 的结构化回合结束（它绕过原生发送，下一步归 Run owner）把待发
  prompt 移到 Emacs 持有的挂起区，事件回调不提问。Run 期间在输入区提交的内容
  只进队列，不 steering 也不并发第二个 prompt，以保持 Run 的冻结输入；此后的普通回合不带走它们，由人在队列
  面板恢复。计数同时包含挂起区，结束确认和恢复记录也保护它。异常结束不算完成：
  `:stop-reason` 记录事实，并按 failed 提醒。steering 已清空输入但被 provider 以
  结果或协议错误拒绝时，原文本进挂起区最前面，供人检查或恢复；它不随随后普通回合
  自动发送。测试：`agent-queue-c37-*`。
- **C38 client 退出是即时事实。** 进程结束的当下更新视图、挂起队列并按 failed
  提醒，不等下一次提交或卡死检查。已退出 client 的请求不再是待处理或忙碌，
  `:turn-state` 为 `interrupted` 表示结果未知，不重放；停止与审批入口明确拒绝。
  自身 `shutdown`、结束会话和 Remote 的显式断开不是故障。测试：`agent-process-c38-*`。
  Remote 在关闭资源前挂起待发 prompt，作废标题查询并取消停止/idle 调度；保留 buffer 的
  `:process-state` 为 `disconnected`，中断回合为未知结果。旧 client 的请求与队列
  恢复入口均拒绝继续发送，原生 session ID、草稿与挂起区保留，等待明确重连。
  已关闭、明确断开或 state 被替换的入站请求不执行文件/终端操作；晚到的审批
  回调也不再回复。关闭后的原生回调仍可结清原请求，并释放 client-ended 观察者，
  不因此发出失败提醒或重启 provider。
- **C39 队列可以管理。** `C-c A q`、会话菜单 `q`、管理器 `Q` 打开同一队列面板，
  能查看、编辑、删除、排序、挂起、恢复和立即发送；面板只改变待发文本，关闭不
  影响队列。立即发送在忙碌时走 steering，适配器不支持则拒绝，不擅自打断当前回合。
  steering 在等待审批或问答时需要确认；确认被取消或同步提交失败，原条目与顺序保留。
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
  list/tasks/status/create/spawn/send/result/read/stop/show；列表不返回 prompt、草稿或完整对话。
  除 list/tasks/create/spawn/result 外，调用者必须给出 buffer 和匹配的 nativeSessionId；可额外限制
  root/target。`create` 只初始化 client，`spawn` 还派发任务；发送复用原生队列且不自动清空挂起任务。
  `read` 分页返回 transcript 并排除输入草稿，`result` 只读任务回执。后台操作不选择窗口，不代理权限决策。
  精确 buffer/ID 查询直接校验该物理会话，不遍历其他会话的快照；status 复用这份
  只读快照，变更操作后重新读取。列表先按项目、target 和隐私过滤 buffer，再分页
  构造返回项；旧 snapshot 隐私谓词仍按兼容契约读取候选完整快照。
  无 root 的显式 target 查询覆盖该 target 的所有项目；二者都省略时才跨 target。
- **C46 空闲路径有界。** 不相关会话的状态变化不扫描暖会话候选；已调度的活跃
  候选复用现有单次计时器。Pi 的所有管理来源始终排除自动回收。恢复能力查询
  只读 ACP 能力字段，不为两个布尔值构造完整会话快照。API 和原生回合完成均
  通知公共订阅者，按确切回合去重；草稿检查不复制整段输入。
  原生历史查询不代表模型忙碌，不扣住新输入；自动回收仍保护所有在途 RPC。
  Activity 的事件更新只读取变更会话的忙碌事实与显示字段；定期核对不构造完整
  session snapshot。相同显示状态不重复发布活动变更。
  同一次同步启动中，相同 buffer、cwd、环境与 PATH 的 adapter CLI 查找只执行一次；
  不跨启动复用，也不缓存失败。ACP client 从自己的 context buffer 启动，使用
  `agent-shell` 的 target placement 与 interactive process class，local 同样经路由。
  标题的后台 `session/list` 只在所属 client 进程存活时发送；同一会话同一回合的
  并发刷新合为一条请求。请求期间有更新的回合时，完成后最多补一次最新刷新。
  回包须仍属于同一 state、client、进程、session ID 与 cwd；查询期间已经收到的新
  标题不被旧查询覆盖。失败不缓存、不循环重试，也不为标题启动或重启 client。
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
- **C50 ACP 事实可查询。** `emacs:agent` 的 `status` 附带提供商报告的
  `capabilities`（`list`、`load`、`resume`、`fork`、`steer`）和 `usage`（含 cached read/write，
  未上报时为 null，不当作零）；`list` 保持轻量。`create` 带 `worktree` 时走
  `+agent-worktree--start`，在 Project 工作目录的独立 worktree 启动，可带 `group`，
  并返回分支和路径。清理和 fork 仍是用户动作，API 不提供。
  所有创建先按 workspace 校验显式 target，再创建会话或 worktree。返回值沿用
  Emacs gateway 的 JSON 标记（nil 为 null、`:json-false` 为 false），经传输仍保持类型。
- **C51 普通会话的项目入口。** 普通 ACP 会话位于本机可达的 Noema 项目时，首个请求附带
  一个隐藏的 ACP 文本块，只含项目根、`noema-project-maintenance` 的路径和 API 命令，
  以及 Pi 的存在和本会话的身份（[Pi C22](pi-sessions.md)）。措辞是告知有什么可用，
  不要求先读（[Pi C23](pi-sessions.md)）。
  它是引用：不读取项目，不启动 host，不改变聊天中显示的提问。每个原生 Session
  只发一次。Run、probe、side、Pi、MarginNote 和 LaTeX 导出由各自的 owner 提供上下文，
  不收此块。实现在 `noema-agent-entry`，只在同步构建请求时绑定，晚于异步记忆召回。
  `/` 命令不收入口；命令中的路径与参数分别经过 shell 与 JSON 编码。
  只有对应原生 Session 的请求构建成功才记为已发送，构建失败不消耗入口。
- **C52 项目共享资源有一个入口。** `agent:project:status` 用一次只读调用回答项目根、
  工作目录、`.agent/` 的占用和压力，以及 Hy Memory 状态。不在项目内时 `project` 为
  null，项目还没有存储时 `cache` 为 null；两者都不创建任何东西。
  `agent:project:maintain` 立即执行既有的保守维护，`agent:memory:search` 用自选问题
  在项目范围内召回，`state` 不是 `ok` 时表示未知而非空。这些接口不持有状态：
  `.agent/` 的存储归 Go kernel（`research:cache:*`），`.agent/tasks/` 归 Agent 并经
  `agent:project:graph` 读写（`status` 的 `graph` 是摘要，没有时为 null），记忆归 Hy Memory。
  各类 Agent 的用法写在 `noema-project-maintenance`，属于 `noema-project` pack；任务图谱见
  [项目任务图谱](../noema/design/project-graph.md)。
- **C53 输入提示只负责显示。** 原生提示符前只有一个随插入移动的 overlay，按
  会话状态、初始化完成和 prompt-ready 事件更新；不逐帧扫描正文、不增加计时器。
  队列预览读取有界的文本片段，挂起项明确标记；忙碌提交按现有函数和 Run owner
  显示 steering／queue，不更改发送行为。停止模式、切 major mode 和结束 buffer
  释放 overlay、订阅与 buffer-local 输入字体覆盖。行首编辑不越过草稿起点，
  原生停止、模型和菜单按键保留。
  测试：`agent-appearance-c53-*` 与 `agent-flow-e2e-*`。
- **C54 管理页面共用呈现规则。** `tools/agent/ui` 维护公共指标、上下文进度和
  原生表格样式；页面只消费原有快照，不因展示增加取数、进程或计时器。Usage
  选择更新指标并撤下聊天预览，切回 Sessions／Inbox 恢复真实 buffer 预览；
  隐藏与返回保持 C12、C18。队列滚动不丢失条目或改变顺序，原生表格的列标题
  不作为工作实体，操作仍按行身份定位。主题切换使用 Aaron 语义色，字体覆盖在
  major mode 结束时释放。测试：`agent-pages-c54-*` 与管理器／队列 GUI 测试。
- **C55 恢复列表覆盖所属项目。** 保存的对话归 provider，Emacs 不复制也不另建索引。
  恢复选择器读各 provider 的本地存储（C63）；`+agent/resume-saved` 保留官方
  `session/list` 路径：`+agent-acp-list-saved` 列出当前目录，再列出会话
  所属的项目根（`+agent-acp-session-root`，会话在 vault 或其下工作目录启动时与当前
  目录不同）和当前仓库外层的项目（当前仓库嵌套在另一个仓库里时），每个目录只问一次。
  其他目录的对话带上所在目录，恢复时切到该目录，已在运行的对话按该目录识别；其他目录
  列表失败只少这部分，不影响当前目录。外层仓库不反向列出嵌套仓库的对话。选择器保持
  从新到旧，不交给补全界面排序；每项写标题、时间、短 ID，别处已打开的标 `● open`，
  其他目录的对话标出目录名。没有对话时说明查过哪些目录。没有活动会话时恢复，默认用
  最近使用的 agent。Claude 终端自带的 `/resume` 不显示 ACP 启动的 SDK 会话，这由 CLI 决定，
  按 ID 用 `claude --resume` 打开或在这里恢复。测试：`agent-c55-*`。
- **C56 输入区回车只写字。** 输入区内 `RET`、`S-RET` 换行（Markdown 列表照常续写），
  `Cmd-RET`（`M-RET`）发送，生成中 steer（不能 steer 的 provider 与 Run 改为排队），
  `Cmd-S-RET` 发送，生成中排队；空闲时两者都直接发送。输入区外 `RET` 保留原生动作，
  表单、按钮和补全菜单的 `RET` 不受影响。`C-v` 看剪贴板此刻的内容：有文本粘文本，
  只有图片时粘图片，`Cmd-V` 保持普通粘贴（剪贴板只保存最近一次复制，
  应用复制文本时常附带一张图，所以文本优先）。图片与截图始终从客户端采集，保存到
  会话 target 的 `.agent-shell/screenshots/` 时经过 Remote 文件 API，本机工具不接收
  逻辑远端路径。提示行写出同样的键。测试：`agent-c56-*`、`agent-remote-c56-*`。
- **C57 回复作为页面阅读。** 聊天 buffer 不渲染公式；`o`（`/page`）取上一条回复的
  Markdown 原文，写入 `var/agent/replies/`，在另一个窗口作为 Noema 页面打开，聊天保持
  可见。同一回复复用同一文件；目录只保留最近 20 份，正被访问的不删。没有回复时报告
  而不创建文件。测试：`agent-c57-*`。
- **C58 回答可以归档进 Wiki。** `F`（`/file`）取上一条回复的 Markdown（与 C57 同一来源），
  打开 Noema 现有的新建节点工作台：回复以标题开头时它成为页面标题，正文去掉该行，否则
  取首行作暂定标题。命令本身不写文件；人确认标题、位置和标签后按 `c`，由同一创建
  runtime 在同一次创建写入中把正文接在模板之后。通用层只公开 `+agent-last-reply`，建页留在
  `noema/roam`，不进入 `tools/agent`（C10）。没有回复时报告而不打开工作台。
  测试：`agent-c58-*`。
- **C59 旁路提问继承原生上下文。** `/side` 在提供方报告 fork 且原会话已有原生
  Session ID 时，fork 这个 Session，另开临时聊天；它知道原会话之前的对话，
  但有自己的后续历史。原会话的当前回合、队列和输入不动。不支持 fork 或原会话
  尚未初始化时另开空白临时聊天。旁路会话不进入项目的持久会话登记。
  测试：`agent-side-c59-*`。
- **C60 按任务交接。** `emacs:agent` 的 `spawn` 以独立 ACP 会话后台执行任务，
  初始化完成后发送；`send` 在已有会话中派发。两者返回 `requestId`，`result`
  只返回该任务的有界文本、状态、停止原因与分页位置，不用聊天正文匹配身份。
  同一会话复用原生队列；带回执的队列项通过结构化 ACP 请求发送原始内容，并独立完成，
  不与相同文本或其他任务合并。不同会话可并行。`end_turn` 才是 `completed`；
  取消、拒绝、RPC 失败、初始化失败与 client 退出保留不同终态；修改或移除队列项
  使原回执成为 `untracked`，不把后来的回答归给它。停止后的 `held` 队列不自动恢复。
  可选 `notify:{buffer,nativeSessionId}` 在终态向确切父 client 发送一次通知，忙时排队；
  父会话可结束回合等待，不须轮询或要求 worker 再发消息。父 client 已替换或退出时
  通知失败，结果仍可查。回执最多 128 个，每个保留 100000 字符；只淘汰已结束且无待发
  通知的项。满额在创建前拒绝，创建后派发准备失败则清理新 client。回执不跨 Emacs 重启，
  不替代原生产物和历史。`tasks` 按 root/target 分页列回执的身份、状态、Unix 秒时间戳，
  遵循会话隐私过滤，不返回 prompt 或回复；遗失 ID 可据此找回。`result` 同时给出任务
  创建时的项目与 worker 身份，worker 结束后仍保留。终态释放请求正文和旧 client 状态，
  通知尝试后释放父 client 引用。测试：`agent-delegation-c60-*`。
- **C61 后台执行 buffer。** `spawn` 创建 `subagent` 来源的真实 agent-shell buffer；
  `create` 可指定 `background:true`。它们不抢窗口，不参加普通聊天的默认标签或最近会话
  召回，仍出现在统一 Agent Manager、显式选择器和 API 列表，带 background 与 parent 信息。
  手动打开、隐藏与重连保留后台身份；关闭视图不停止任务。`agent-spawn`／`C-c A b`／
  `/delegate` 派工，`agent-open-worker`／`C-c A B`／`/workers` 打开已有 worker。
  `C-c A E`／`/butler` 召回全局 Pi；Pi 与其他 Agent 共用同一运行时。
  旧 local bridge 只翻译为公共 API，不另存回执或管理队列。
- **C62 临时任务与普通会话隔离。** ACP 层将 `subagent`、`probe`、`marginnote`、
  `latex-export` 及已有的 `side` 来源视为临时任务，在异步启动前复制并标记配置。
  其模型切换回调不写普通模型偏好，也不进入领域的持久会话登记；仅指定推理强度的
  pinned config 同样隔离。普通会话的显式模型选择仍可记忆，不重置已有用户偏好。
  `spawn`／`background:true` 的 `create` 可传 `modelId`、`effort`；workspace 与
  worktree 共用这一路径，在创建 worktree 前校验参数。私有副本去除被任务参数覆盖的
  普通默认选项，包括具体 ID 和 ACP 类别名；任务推理级别在保留的模型/模式选项之后应用。
  提供商实际选择必须在首个
  带回执任务的 prompt 前核验；不支持或不匹配就失败，不静默用其他模型工作。
  普通 `create` 拒绝任务参数；后续 `send` 尊重 worker 当前设置。后台可见性与生命周期
  分开：隐藏普通聊天不会因此变成一次性任务，手动打开 worker 也不会将它变成普通聊天。
  临时 worker 可按空闲保留策略回收，但初始化、运行、待决请求、队列、草稿与可见窗口
  仍阻止回收。通用回收不删除原生历史、产物或 worktree；私有导出沿用既有的历史清理规则。
  Noema 导出复用共享 pinned-config，不另建运行时。
  测试：`agent-task-c62-*`、`agent-api-c62-*`、`agent-delegation-c62-*`。
- **C63 一份跨 agent 的对话历史。** `/resume`（`h`）、Manager 的历史、项目池的恢复项
  都打开 `+agent/history`（`tools/agent/history`）。它只读 Claude Code
  （`~/.claude/projects/<目录>/<id>.jsonl`）、Codex（`~/.codex/state_5.sqlite` 的
  `threads`）、OpenCode（`~/.local/share/opencode/opencode.db`）和 Antigravity
  （`~/.gemini/antigravity-cli/conversation_summaries.db`）各自的存储，不复制、
  不另建索引；存储在 agent 运行的 target 上，经 Remote 文件与进程接口读取。范围是
  C55 的目录（当前目录、会话项目根、外层项目及其子目录），`C-u` 列出全部目录，本项目
  没有对话时自动列全部；临时目录（tmp、`var/noema/tmp`）里的会话和 Codex 的 `exec`、
  子 agent 线程不列出。选择器按新到旧列出 agent、时间、轮数、分支、目录与标题，别处
  已打开的标 `● open`，移动时在悬浮框（与 LSP 悬浮文档同样的
  child frame，候选右侧或 minibuffer 正上方）预览最后几轮对话；它不取窗口也不取键盘，
  滚轮与 `C-M-v` / `C-M-S-v` 原地滚动，每个对话在一次选择中只读一次，移动去抖，选择结束
  时销毁预览 frame 并清空 buffer，确认与取消都走同一清理路径；下次打开时重新创建，
  同一次选择内切换候选仍复用预览。预览与查看都从 transcript 末尾按固定大小的字节窗
  往前分页读取（只解码像消息的行，单条消息有字数上限），从不读整个文件：悬浮框最新的在上，
  滚到底再加载更早的一页，一个对话最多保留 60 条，一次选择最多缓存 24 个对话。`Cmd-RET`
  在只读 Markdown buffer 中查看该对话（按时间顺序，先载入最近 20 条，`<` 加载更早，`r`
  恢复，`o` 打开原始 transcript）。打开规则：在会话里用该会话的
  agent，否则用对话自己的 agent。同一 agent 走原生恢复（会话里原地恢复，保留窗口、
  popup 与 Noema 名字；否则新会话），并开启严格恢复；另一 agent 的对话，或原生恢复
  失败（`+agent-acp-restore-failed-functions`），开一个新会话并自动发送
  `resources/agent/resume-transcript.md`：只给 transcript 的链接（文件路径，或
  `opencode session export <id>`）和一行标题，不嵌入内容，也不要求先读或概括；之后的请求
  需要时由 agent 自己在文件里查找所需部分，以免占满长会话的上下文。
  Antigravity 的列表只读原生摘要库，排除空会话、已标记归档和子会话；
  多 workspace 按匹配的目录归属，URI 解码后显示。搜索索引未收录的会话仍列出，
  缺少索引时轮数留空。分页预览只读 `conversations/<id>.db` 的 `steps`，
  按原生 step 编号排序和翻页，每页限制读取数量和 payload 字节数；
  仅解码用户原文及公开回复的 protobuf 字段，跳过思考、工具记录与损坏消息，保留单条字数上限。
  原生 SQLite 无法打开已关闭的 WAL 数据库时，仅在没有 `-wal` 文件的情况下
  以 immutable 只读快照重试；仍有 WAL 的库保留正常读取，包含尚未 checkpoint 的记录。
  `agy` 与 agent-shell 的 `antigravity` 身份视为同一 agent，走原生恢复；
  跨 agent 接续和恢复失败时提供原生数据库路径及只读 SQL 命令，不导出或注入整份对话。
  已在运行的对话直接切过去。测试：`agent-c63-*`。

- **C64 结构化回合里输入照常 steer。** Noema 页面发起的请求、带回执的 API 任务和 Research Run
  都以结构化 ACP 请求发出，不经过 agent-shell 的输入层，所以普通提交会与它并发出第二个回合。
  它仍是忙碌的回合：`Cmd-RET`、`Cmd-S-RET` 和其他回合一样按 `agent-shell-busy-submit-*`
  的配置 steer 或排队。Research worker 在当前请求上标记 RunSpec 输入已冻结，后续提交排队；
  同一会话的其他回合独立判断。冻结时提示 `Cmd-RET queue (this turn's input is frozen)`，
  适配器缺乏 steering 时显示普通排队提示。
  配置使用的 `agy acp` 通过第二个 `session/prompt` 接收中途指令。Emacs 保留原生回合和
  结构化任务的完成回调，等它们的请求都收到响应后才结清并处理队列。被拒的 steer 按 C37
  放回挂起队列首位。测试：`agent-input-c64-*`。
- **C65 文档协作者绑定。** 协作者就是池里的会话，不另起注册表。会话多一项永久 buffer-local
  事实 `+agent-acp-session-file`：一个主文件（框架身份 `/fs:TARGET:/path`），nil 表示绑定在项目层；
  快照的 `:file` 读出它。Agent 实际运行时仍可读写任何文件，主文件之外改过的文件取自 C44 的
  改动记录（`+agent-binding-touched`），不另外采集。绑定按原生 Session ID 存在
  `var/agent/bindings.eld`（有上限），原生 ID 到达时写入；恢复同一对话、会话还没有绑定时取回
  主文件和名字，已有的绑定不被覆盖。`H-t` 在文档里（`+agent-document-file`：普通文件 buffer，
  或经 `+agent-document-file-functions` 注册的界面，例如 Noema 页面）先列这篇的协作者（在屏幕上的
  直接跳过去），再给每种 Agent 一项“为这篇新建”，新建的会话绑定这篇并以 `agent·文件名` 命名；
  其余会话随后，非文档 buffer 里列表不变。Pi 经 `emacs:agent` 的 `bind`（核对 buffer 与原生 ID）
  改绑，`list` 接受 `file`，`create`/`spawn` 接受 `file`。只改 Emacs 侧的信息流转，不改 Agent
  自身逻辑。整体设计见文档协作工作稿（迁入 `docs/design/` 前在仓库外）。测试：`agent-binding-*`、
  `agent-api-bind-*`、`agent-api-create-with-file-*`、`agent-switcher-c65-*`。
- **C66 修订带作者。** 每个 agent 会话有一个不透明令牌（`+agent-acp-session-token`，跨 client
  重启不变），经 `acp-make-client` 的环境变量 `NOEMA_AGENT_SESSION` 交给 Agent 进程；Noema API
  客户端在 `agent:document:propose/edit` 上自动带上 `authorSession`，host 用 `emacs:agent` 的
  `identify` 向 Emacs 取名字。新卡片写 `@@ediff-author {"name","session"}`，修订已有卡片保留原作者；
  Emacs 不可达时作者名为 `agent`，仍可归属。不是 Markdown 的文件不能内嵌卡片：`propose` 在其
  Project 下写 `.agent/collab/suggestions/<id>.json`（pending，含行列位置、旧文与新文），文件本身
  不动；没有 Project 时拒绝，`edit` 只接受 Markdown。所有入口都说明文档工具：页面入口（工具指南，
  API 路径指向 checkout 的 `scripts/`）、项目入口（一句话）、LaTeX 导出修复会话。Agent 自身逻辑不改。
  测试：`C66 *`（Noema）、`agent-collab-c66-*`、`agent-api-c66-*`、`noema-agent-entry-c66-*`。
- **C67 文档线程。** 在文档某处提问是一条线程，存于文档所在文件夹的 `.noema-comments.jsonl`
  （C70 的批注日志：引文、行、指派的协作者、消息）。`C-c A d`（`+agent-collab/ask`）
  在文件 buffer 里以选区或当前行提问；Noema 页面的“发给 Agent”经 `+agent-context--send` 的
  `:deliver` 同样成为线程。默认收件人是这篇文档的协作者（`+agent-context--session` 预选它，
  否则仍是本项目上次的会话）。一个协作者一次只接一条线程：它忙时下一条在 Emacs 侧等待，
  线程交给 Agent 时只发一条提示，回复不收集（C73）；Agent 经 API 回复、修改并结案。
  `C-c A k` 列出线程：`RET` 回到锚点，`v` 读全文，`r` 追问同一协作者，`c` 结案，`o` 进入
  agent-shell 深谈。非 Markdown 文件的修订（C66）由 `C-c A Y` 审阅：接受时旧文仍在原位或全文
  只出现一次才写入，否则拒绝；拒绝只改状态。一个已打开且有未保存修改的 buffer 不会替人保存。
  测试：`agent-collab-c67-*`、`agent-collab-c66-*`、`noema-md-bridge-c67-*`。
- **C68 直接改动。** Agent 绕过 API 用自己的工具改文件时，完成的 ACP 工具调用带来的 diff
  （旧文、新文）记入该会话的 `+agent-acp--direct-edits`（有上限，随 client 结束），与 C44 同源。
  `C-c A Z` 列出这篇文档上的直接改动，`RET` 看 diff，`u` 放回旧文：仅当新文在文件中恰好出现
  一次；Markdown 笔记经 `agent:document:edit` 写回，打开的页面随之更新，其余文件走 C67 的
  同一写入规则。新建整文件的改动不提供放回。测试：`agent-collab-c68-*`。
- **C69 全局可见性。** 会话池的模型（`tools/agent/topology.el`）：
  Pi 在上，管理它所在的项目；项目下是文档，文档下是绑定它的协作者，协作者到它另外改过的文件
  是虚线。节点标出状态（working / needs you / idle / disconnected）与文档的待审项（未拒绝的卡片
  加修订）和未结线程。模型来自会话快照与协作记录，不向 Agent 发查询；客户端 Graphviz
  `dot -Tjson` 排版。它画在协作面板的图谱页（C78，`C-c A H` 与 `M-x agent-topology` 也打开那里）。
  Treemacs 用自带注解接口
  （来源 `agent-collab`）标记：绑定的文件 ` ◆N`、待审 ` ✎M`、只被改过 ` ◇`，文件夹汇总其下文件
  （每个文件只计一次），正在被改的文件用 `+agent-presence-working`；同样随事件合并刷新，
  只在 Treemacs 与会话层都已加载后启用。测试：`agent-topology-c69-*`、`agent-presence-c69-*`。
- **C70 / C80 页面批注。** 选中文字加批注：锚点 `{==文字==}{>>#c-ID<<}` 留在笔记里并高亮，批注
  存在每个文件夹隐藏的 `.noema-comments.jsonl`（只追加的事件日志）；契约在
  [comments.md](../noema/design/comments.md)。页面没有右侧评论栏：光标落在高亮里或鼠标停在上面时，
  卡片在文字上方弹出（作者、时间、批注、交给了谁；✓ 完成，⋮ 编辑或删除）。高亮里可以有公式。
  页面批注经 Emacs 的 `comments-changed` 事件刷新任务面板。测试：`C80 *`、`scripts/check-comments.mjs`。
- **C73 批注即任务，没有回复。** 页面不显示 Agent。批注在协作面板的任务页收集，从那里交给某个 Agent
  （`+agent-collab-dispatch`）：只发一条提示（请求、批注 id、文件、行、引文，每个原生会话的第一条附
  Noema 工具指南），之后 Agent 经 `agent:document:edit` 修改文件，并用 `agent:document:comment
  {file,id}` 标为完成；批注不接受回复。Emacs 不读 Agent 的聊天；忙的会话由 agent-shell 自己的队列
  排队。1200 字符以内的引文完整传递；更长的注明是摘录，并要求读取原文后编辑。
  翻译、润色、精简等明确修改直接编辑；明确要审阅或额外提出的改动用 proposal。
  保留批注锚点，只改内文；`oldText` 继续核对当前源码。测试：`agent-collab-c73-*`、`C80 *`。
- **C74 Agent 经 API 修改，不必等，也不让人等。** `agent:document:edit`（笔记）与 `agent:file:edit`
  （任何文本文件）接受同一组写法：`{oldText,newText}`（原文唯一，像 Agent 自带的 Edit）、
  `{lines:[a,b],newText}`、`{after|before:行内文字,newText}`、`{append}`、位置 `{from,to,oldText,newText}`
  或整篇 `content`（`server/lib/edit-forms.mjs`）。按文字定位的修改落在最新版本上：人同时打字时位置
  偏移不影响，保存撞上并发写入就重读重试（最多三次）。笔记经 `notes:save` 保存，打开的页面只更新
  变化处；其他文件经 Emacs（`noema.file`，`tools/agent/edit.el`）改在访问它的 buffer 里——有人未保存
  的修改时只改 buffer、不替人保存，干净的 buffer 存盘，没有 buffer 时直接写文件；版本（文本哈希）和
  每处原文守护应用，变了就是冲突、由 host 重试。改写大半篇（`LARGE_CHANGE`）或反复冲突时拒绝并返回
  `needsPerson`，host 发 `agent-needs-person`，Emacs 把该会话标为等你并系统通知（点击打开聊天）；
  Agent 只在人于聊天里同意后才可带 `allowLarge`。打开的页面有人未保存的输入时，另一写入者的保存
  （Agent、Emacs）按行求出相对上次保存文本的改动，像 `@codemirror/collab` 那样变基到人的未保存改动
  之上，作为一次不进撤销历史的 transaction 原地应用，人的输入与光标保留，随后照常保存
  （`pages/merge-external.ts`）。只有两边改到同一段文字（或在同一点插入）才不合并，页面保留草稿并提示
  审阅；差异过大同样不合并。测试：`C74 *`、`agent-edit-c74-*`、`tests/merge-external.test.ts`。
- **C75 协作面板。** `H-t`（`agent-panel`，`tools/agent/panel.el`）在右侧开固定侧窗，`H-t` 或 `q`
  收起，点击别处不关。标签：任务（默认；批注即任务，范围本文件→本目录→目录及子目录→项目，
  `s` 切换；`a` 交给 Agent、`c` 标为完成或重开、`R` 显示已完成）、Pi、拓扑。面板说的是最近聚焦的
  文档（`+agent-binding-last-document`，由 `aaron-switch-buffer-hook` 记录）；只在显示时监听状态
  事件并合并为一次重绘，隐藏后什么都不挂。`H-T` 是原来的 Agent 选择器，默认针对同一篇文档。
  重绘按文档与批注 id（Agent 行按会话 buffer）保留选择，前方卡片增删不改变键盘操作对象；
  原对象不在当前视图时回到面板顶部，避免选中另一张卡片。空行上的任务操作只向上查找
  当前泳道；泳道标题与 Agent 行不指向上一项任务。页面里没有打开 Agent 的按钮。
  任务与顶部 Agent 行都可 `o` / `S-RET` 打开聊天；已指派会话不存在时提示重新交派，
  不打开另一位协作者。Markdown 卡片的 `RET` 共用 Agenda 的笔记锚点定位，
  等待页面就绪并守护 client 与窗口身份，不使用延时后向当前页发送行号。
  原生选区发送产生的线程有日志内的行号和完整引用而没有源码批注标记；这类
  `comment-reference` 优先定位同 id 的源码锚点，否则只定位当前文稿里唯一、精确匹配
  的原始引用。引用为空、失效或重复时提示，不猜旧行号；普通页面批注仍须有源码锚点。
  测试：`agent-panel-c75-*`。
- **C78 面板里的 Pi 与图谱。** 面板三页是同一侧窗里的三个 buffer。Pi 页就是这个项目 Pi 的
  agent-shell 会话本身：项目 Pi 的 `+agent-acp-display-buffer-function` 是 `+agent-panel-display-pi`
  （由 `noema-pi-router-project-display-function` 在挂接时设上，重启后保留），所以 `pi`、Pi 的提醒
  等任何地方打开它都落在面板里，不进普通的 Agent 视图，也不在 `H-T` 的会话列表里；全局 Pi 不变。
  图谱页是 `tools/agent/board.el`，手感沿用研究 Graph Board：窗口大小的一张 SVG，场景组带
  translate/scale 变换，平移缩放只改变换；单击选中、拖动平移、双击打开、右键菜单，滚轮平移、
  `C-` 滚轮以指针为中心缩放；`h/j/k/l` 按方位移动，`H`/`L` 沿边，`/` 跳转，`TAB` 折叠分支
  （标出藏了几个），`+ - 0 = .` 缩放、适配、居中，`?` 列出全部命令。对选中节点：`RET` 打开文档
  或项目、`o` 进聊天、`t` 看它的任务、`n` 为它新建 Agent、`s` 停止、`r` 重启客户端、`x` 关闭。
  正在工作的描蓝边，等你的描红边，断开的变淡。面板显示时才随事件合并重绘。在三页里
  `C-c 1/2/3` 切换；任务页与图谱页可直接按 `1/2/3`，Pi 页在 Evil normal 模式下也可
  按 `1/2/3`，insert 模式仍输入数字。页签可点。测试：`agent-panel-c78-*`、
  `agent-board-c78-*`。
- **C81 任务看板（借鉴 SharpMD，自行实现）。** 协作面板的任务页是看板：批注是卡片，泳道自上而下
  （侧栏窄）为 待处理 / 进行中 / 等你 / 暂停 / 完成。泳道不存储，由批注状态和接手会话的实时快照推出：
  已结案在“完成”，未指派在“待处理”，接手会话在等权限或回答在“等你”，会话已不在或断开在“暂停”，
  其余在“进行中”。卡片带接手 Agent 的实时标记（● 在做 / ! 等你 / ◌ 空闲 / × 已不在）。移动卡片即做
  泳道的动作：`>` 从待处理交给 Agent、从其余标为完成；`<` 往回（完成→进行中或待处理，其余→待处理）；
  `u` 收回（日志追加空指派）；`d` 完成；`TAB` 折叠泳道（“完成”默认折叠）。顶部一行列出本项目的
  Agent 与其状态，`RET` 进聊天。等你的卡片从已有会话快照显示具体请求或错误；过长时
  截短显示，鼠标悬停可读完整内容。`i` 就地展开或折叠完整引用、批注历史和等待请求，
  展开区域仍属于同一文档与批注 id，重绘保留展开状态。`/` 按字面搜索全部消息、引用、
  文件路径和接手人（不区分大小写）；搜索时展开泳道。`f` 筛选全部、未完成或某一泳道，
  选“完成”会展开完成泳道；`\` 清除搜索与状态筛选，保留范围；`R` 显示或折叠完成泳道。
  筛选显示命中数与总数；空结果可直接清除筛选。筛选、展开和打开聊天不写任务状态，
  不发送 prompt。`?` 显示操作帮助。只在面板显示时随事件重绘。测试：`agent-panel-c81-*`。
- **C76 发送即任务。** 指向文档某处的发送（`C-c A c` 的区域/整篇/光标处、页面选区、旧的
  `+agent-interaction-send-*`）经 `+agent-collab-task-delivery` 变成该处的批注线程、指派给选中会话
  并立即交出，不夺键盘；聊天与草稿、剪贴板路径不变。测试：`agent-collab-c76-*`。

## 实现位置

| 职责 | 文件 |
| --- | --- |
| 依赖、Remote 接入与配置登记 | `lisp/tools/agent/config.el` |
| 物理会话、能力快照、项目池、队列与暖会话回收 | `lisp/tools/agent/acp.el` |
| 管家调用公共会话的语义接口 | `lisp/tools/agent/api.el` |
| 有界任务回执、完成通知与后台派工命令 | `lisp/tools/agent/{requests,delegate}.el` |
| 常规视图放置、来源返回、frame 边界与物理替换 | `lisp/tools/agent/views.el` |
| 原生聊天配色、输入提示和可编辑行首 | `lisp/tools/agent/appearance.el` |
| 管理页面的指标、上下文进度和原生表格样式 | `lisp/tools/agent/ui.el` |
| 会话列表、Inbox、可取消预览与通用操作 | `lisp/tools/agent/sessions.el` |
| ETAF Agent Manager、分组筛选和 UI 生命周期 | `lisp/tools/agent/manager.el` |
| 按能力提供的原生会话与领域动作菜单 | `lisp/tools/agent/menu.el` |
| 排队 prompt 的编辑、恢复与面板 | `lisp/tools/agent/queue.el` |
| 所有动作的唯一列表，生成前缀、菜单和斜杠命令 | `lisp/tools/agent/actions.el` |
| 渲染、用量、活动和工作树 | `lisp/tools/agent/{render,top,activity,worktree,worktree-board}.el` |
| 原生引用式上下文与 Markdown prompt 文档入口 | `lisp/tools/agent/{compose,context}.el` |
| CLI Agent 工作台：CLI 会话与适配（Claude/Codex/OpenCode）、gptel/Magent 引擎、档案、文档问答（`+agent-interaction-*`） | `lisp/tools/agent/interaction*.el` |
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
- **G13 真实 SSH 长期与故障实测。** C37–C44 的确定性 ACP fixture 已在 Aaron-PC 上
  分别通过 shell TRAMP 与 tramp-rpc：目标 cwd/显式环境、暂停队列、进程退出、改动路径与
  保存会话恢复。`test/agent-lifecycle-remote-live.el` 可按 backend 复验；真实提供商长会话、
  传输半开和物理断网恢复仍未验证，不能把 fixture 的进程退出当作网络故障覆盖。
- **G14 Remote 上的项目入口。** Noema host 与 Hy Memory 在客户端，运行在其他 target 的
  Agent 既够不到 API 命令，也没有记忆。C51、C52 在这里不生效：会话不收入口，不是报错。
  要支持需要经 `remote-channel` 转发 host，尚未实现。
- **G15 撤销会话改动。** C68 已提供逐处放回（新文唯一出现时），但不是整回合撤销。 ZCode 在回合摘要里按文件撤销，先预检文件自改动后未被他人修改，
  多次修改按检查点倒序回放，有一个不安全就整体不写。C44 的清单来自 ACP 报告的 diff，
  不是写前内容的检查点，`oldText` 也可能只是片段。没有可靠的前内容和预检前，不提供
  “撤销”，以免把部分回退说成安全回退；需要先定契约。
- **G16 client 被关闭而非退出。** C38 只覆盖进程自行退出：退出时 `:client :process`
  仍在，会话显示断开、挂起队列。若 client 被 `agent-shell--shutdown` 关闭而 buffer
  仍在（删除 frame 的 kill 钩子曾这样关掉另一会话，见 C28），state 里已没有 client，
  `+agent-shell--dead-client-p` 不成立：会话不显示断开，下次提交时 agent-shell
  静默新建 client 并按原 ID 恢复，而不是走 `+agent-shell/reconnect` 的明确重连。
  C28 的修复消除了已知来源；是否把“buffer 活着但 client 已被关闭”定义为与退出相同
  的断开状态，需要先定契约。
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
