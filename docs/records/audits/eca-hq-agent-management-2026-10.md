# ECA 和 agent shell HQ 管理能力源码审计

通用 agent 的接入、会话、管理、编辑器交互和通知应由 Emacs 维护，供普通源码
buffer、Portal、配置文档、Noema 知识库及 Research 共同使用。Noema 保留 Markdown、
知识库与研究工作流的业务语义，调用和组织这些能力。这一职责划分是本次审计的
前提；不能用“已经有 Noema 的同类功能”作为继续把通用实现放进 Noema 的理由。

ECA 仅用于深度源码研究，不加入 package、server、模型请求或其他运行时依赖。
HQ 的交互也需要按本地 UI、Remote 和窗口规则重做适配。长期契约见
[Emacs Agent 管理](../../design/agent-management.md)。

## 审计范围

| 来源 | 固定修订 | 源码切面 |
| --- | --- | --- |
| [ECA](https://github.com/editor-code-assistant/eca/tree/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9) | `4d586f8d2481a5ae19bca6e8c80b4ad9952deac9` | agent 定义、工具状态机、权限、子代理、停止、fork、上下文缓存、历史分页、后台任务、编辑器请求 |
| [eca-emacs](https://github.com/editor-code-assistant/eca-emacs/tree/d08930b63ec91898019a931f13556deca7433828) | `d08930b63ec91898019a931f13556deca7433828` | 进程与 JSON-RPC、管理视图、注意状态、编辑器诊断和导航、inline chat、任务 UI、对应测试 |
| [agent-shell-hq](https://github.com/sreenivasvrao/agent-shell-hq/tree/1c8eed8fd598ffa4332988713b4eda7e1d45b85a) | `1c8eed8fd598ffa4332988713b4eda7e1d45b85a` | Peek、Toggle、状态分组、真实 buffer 预览、自动命名及资源释放 |
| Emacs | 起始 `3ab3d425671e28efb8da030f71173d860873b520`，含当前工作树 | `tools/agent`、Portal、Remote、Aaron UI、通知边界 |
| Noema | 起始 `5586d826759dc352bfdd6e1b5c87cb275b7e3ba8`，含当前工作树 | ACP、worker、Sessions、Inbox、ABTOP、worktree、context、compose 与职责约束 |

上游以源码及其测试断言作为静态证据，没有启动 ECA 服务。上游片段保存在被忽略的
`var/audits/agent-management/`，不参与 Emacs 的加载链。下文区分审计起点的发现与本轮落地；本地验证覆盖共享运行时、Noema 业务适配、
预览和通知，不代表已实测上游服务或每个提供商。

## 能力地图

| 主题 | 上游实现中的要点 | 本地应维护的边界 |
| --- | --- | --- |
| 身份 | ECA 分 workspace 连接、chat、tool-call 和子 chat；HQ 主要导航 buffer | Emacs 分 target、workspace、物理会话、原生 ID；业务 Run 单独关联 |
| 生命周期 | ECA 有 running、stopping、idle/error；停止先改变状态再清理工具 | Emacs 提供停止回合、关闭视图、结束会话；Noema 另持久化 Run 终态 |
| 权限 | ECA 有明确规则来源和工具状态转换 | Emacs 管通用审批交互与回应；Research 的领域约束和持久决策由 Noema 保留 |
| 人机问答 | ECA 用独立 request ID、问题与答复 promise，不把问题当普通输出 | Emacs 统一待回答状态；adapter 没有事件时显示 unavailable |
| 队列和 steering | ECA 支持活动回合中的后续意图；已有 agent-shell 自己管理 prompt 队列 | 复用原生队列，不在 Noema 和 UI 各排一份 |
| 子代理 | ECA 验证父子归属、占用 token、步骤与时间预算，并保留部分结果 | provider 原生子代理与 Emacs 发起的并行会话分别表达，不能虚构通用 ACP 能力 |
| 并发写入 | 子对话的上下文分离不意味着文件隔离 | Emacs 管 worktree 与执行目录，Research 管节点依赖和业务并发约束 |
| 上下文 | ECA 区分静态 prompt 身份、动态编辑器状态和工具输出裁剪 | Emacs 提供选区和文件引用；Noema 冻结本次 Run 的研究输入 |
| 历史和恢复 | ECA 历史分页带校验 cursor，fork 从指定内容边界复制历史 | 原生 transcript 保持权威；adapter 明确 load、fork 与 live attachment 的区别 |
| 用量和成本 | eca-emacs 将状态、模型、耗时和费用放在同一列表 | Emacs 的统一快照按来源报告用量；未知值不能显示成零 |
| 编辑器工具 | eca-emacs 接受诊断、定义和引用请求并连接 LSP/xref | 复用本地 LSP、CAPF、xref 和 Remote workspace；Noema 页面只提供上下文适配 |
| 管理视图 | ECA 按 workspace 展开 chat；HQ 导航即预览真实会话 | 通用视图消费同一会话状态，Noema 注入业务列和导航动作 |
| 注意和通知 | ECA 区分审批与问题，状态变化才通知 UI；HQ 给忙闲图标 | 运行状态、待处理原因和未读分离；通知由 Emacs 控制打扰程度 |
| 后台资源 | ECA 有有界任务输出、退出观察和清理；HQ 关闭 UI 时释放计时器 | 使用已有 Activity、任务和 Remote 资源所有权，不增设独立后台进程管理器 |

## 协议边界

[ECA 官网](https://eca.dev/)覆盖 chat、rewrite、completion、Skills、MCP 和子代理。
其[协议文档](https://eca.dev/protocol/)定义基于 JSON-RPC 2.0、受 LSP 启发的
双向请求、通知与流式交付，包含自己的 chat、editor 与配置方法。
据此判断，把 ECA 接到既有 ACP Run 需要协议和语义适配，不能只增加一个 ACP
agent 配置。本轮不接入 ECA；研究其管理原语和 Emacs 客户端实现即可。

## ECA 的执行和管理设计

### 显式状态转换和副作用分离

[`chat/tool_calls.clj`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/chat/tool_calls.clj#L200-L358)
把准备、检查权限、等待审批、允许运行、执行及停止列成转换表；
[`transition-tool-call!`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/chat/tool_calls.clj#L583-L630)
先记录状态，再执行有序动作，并用 finally 清理。这使“已批准”“执行中”“结果
已交付”成为可分别检查的事实，而不是一个 busy 布尔值。

本地值得采用的是事件和转换的可审查性。Emacs adapter 应统一给出停止请求、已停止、
连接丢失和失败；UI 只消费快照。Noema 的 host 已确认终态与 ACP 已结束回合仍须
分别处理，不把上游的内存状态机直接替换成 Research 数据库的状态机。

### 权限有来源且拒绝优先

[`approval-decision`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/tools.clj#L60-L138)
返回决定及规则来源，优先检查 deny，再检查会话内记忆、工具自身检查、ask、allow
与默认值。其记忆规则对 shell 使用解析后的命令键；解析不完整时继续询问。
[`decide-tool-call-action`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/chat/tool_calls.clj#L678-L790)
还区分 hook 拒绝某个工具与停止整个回合，hook allow 不覆盖配置中的 deny/ask。

可吸收的是带来源的审批解释、精确请求 ID，以及审批和执行回执分开。Research
已有版本、epoch 和持久权限记录，迁移应复用它们；通用 Emacs 会话不应为了审批
强制创建 Noema Project，也不应引入第二份相互独立的 allow 规则。

### 子代理是有归属的执行资源

[`owned-subagent` 和 `admit-resume`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/tools/agent.clj#L284-L344)
只允许同一父 chat 继续当前 server 认可的子代理；旧执行须已收敛、没有占用 token，
也没有活动工具资源。预算按新一次执行重置。
[`await-result`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/tools/agent.clj#L374-L454)
区分完成、父级停止、超时、步骤耗尽与失败，保留部分输出，并在有限时间内收尾。

应把这些约束用于 Emacs 发起的并行会话管理：父操作拥有子执行，取消需要传播，
预算和未完成结果必须可见。provider 内部的子代理则只展示其真实上报；没有报告
子执行 ID 的 ACP 会话不能凭文本拼出可控制的子代理树。上下文独立也不提供
worktree 隔离，文件写入仍需本地已有的 worktree 边界。

### 停止不等于完成或删除

[`prompt-stop`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/chat.clj#L2556-L2583)
先进入 stopping，拒绝活动工具并拆除后续回调；
[`fork-chat`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/chat.clj#L2763-L2807)
则创建新 ID 并复制到指定消息边界的历史。它们与清空、删除、关闭编辑器 buffer
是不同操作。

本地的“隐藏视图继续运行”规则应保留。通用管理菜单分别提供查看、停止回合、
结束会话与原生 fork；研究归档和 WorkNode 操作由 Noema 添加。恢复历史的能力
不能被描述成接管另一客户端正在执行的任务。

### 上下文的稳定性和可追查性

ECA 在 [`static-prompt-cache-signature`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/chat.clj#L84-L131)
按 prompt、contexts、rules、skills、tools 分别计算静态身份；已有会话还会固定
system prompt，并提示配置变化何时生效。它的
[`prune-tool-results!`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/chat.clj#L166-L210)
为溢出恢复裁剪旧工具结果。两者分别解决输入稳定性和窗口压力，不能混为一次
简单的字符串排序优化。

这对应本地两个检查项：普通上下文由 Emacs 维护同一份选择，Research 的冻结
输入仍由 Noema 记录；未来优化缓存时，单独验证 adapter 最终发送的消息边界与
真实 cached read/write，不能从 RunSpec 字节顺序推断命中收益。

[`chat/history.clj`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/chat/history.clj#L1-L81)
的 cursor 同时保存位置和内容校验，能识别历史被移动、裁剪或删除。可借鉴其
“来源已变化”的明确反馈，但不复制一份完整 transcript 来驱动本地导航。

### 人机问答和后台任务

[`ask-user`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/tools/ask_user.clj#L50-L91)
创建独立请求并等待结构化答复，支持取消。trust 模式还可配置超时后交给模型自行
判断；这项策略不能直接移入本地的必须回答或必须批准的操作。

[`background_tasks.clj`](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/background_tasks.clj#L140-L171)
观察真实退出结果；完成后的通知与记录淘汰
[另有独立路径](https://github.com/editor-code-assistant/eca/blob/4d586f8d2481a5ae19bca6e8c80b4ad9952deac9/src/eca/features/background_tasks.clj#L233-L283)。
本地应让任务进入已有 Activity/任务资源，而非让聊天 UI 自己维护无限增长的
进程、输出和重复完成消息。

## eca emacs 的编辑器耦合

### 管理视图和注意导航

[`eca-chat-status`](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/eca-chat.el#L3194-L3263)
把等待审批、等待回答、停止中、运行中与空闲分开，且只在会改变状态的事件上
触发管理视图更新，避开每个 token。对应
[`attention tests`](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/test/eca-attention-test.el)
检查跨会话导航、优先状态及问题与审批的区分。

[`eca-workspaces--render`](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/eca-workspaces.el#L251-L328)
保留实体位置、每个窗口的 point、滚动与折叠；其
[刷新计时器](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/eca-workspaces.el#L330-L385)
把状态变化合并，并仅在视图可见且有活动会话时更新耗时。

本地可以直接吸收这些管理逻辑：统一快照、按实体保留光标、待处理导航、隐藏后
不刷新。呈现仍使用 Aaron UI 和现有原生视图，既不照搬脸色和布局，让共享 Sessions/Inbox/Top 消费同一运行时快照；Noema 另保留业务记录视图。

### 工具应使用编辑器的现有能力

[`eca-editor.el`](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/eca-editor.el#L49-L271)
整合多种诊断来源，定义与引用请求
[优先连接 LSP，并有 xref 路径](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/eca-editor.el#L273-L578)。
这比让 agent 只读磁盘更深入地接入编辑器。适合在 Emacs 公共工具边界提供，
但服务的选择、URI、环境与目录必须来自本地同一个 Remote workspace；不能
直接采用“当前 buffer 恰好有什么 LSP”作为跨会话请求的所有权。

[`eca-chat-inline.el`](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/eca-chat-inline.el#L718-L863)
把 inline 展示作为真实 chat 的视图，第一次可 fork 已有历史，后续复用，关闭
overlay 与关闭会话分开。可用于审查本地 side chat 与 compose 的关联语义，
不意味着在 Noema CM6 内再画一套聊天或审批界面。

### Remote 接入只能借鉴机制

[`eca-process-start`](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/eca-process.el#L617-L687)
使用文件处理器并在远程路径上调整 TRAMP 连接共享。这个代码说明长连接和文件
请求会相互影响，但其 `file-remote-p` 分支、SSH 参数与自动安装策略不符合本地
consumer 契约。进程、环境、通道、恢复和 workspace 资源仍交给 `remote-*`。

## HQ 的 UI 管理逻辑

1. **导航即预览真实会话。**
   [preferred-buffer 与 preview](https://github.com/sreenivasvrao/agent-shell-hq/blob/1c8eed8fd598ffa4332988713b4eda7e1d45b85a/agent-shell-hq-peek.el#L101-L127)
   复用已有 viewport 或 shell buffer，避免复制 transcript。
   [取消](https://github.com/sreenivasvrao/agent-shell-hq/blob/1c8eed8fd598ffa4332988713b4eda7e1d45b85a/agent-shell-hq-peek.el#L336-L373)
   恢复原 buffer；本地适配还应完整保留窗口 point、start 与布局，使用已有
   Aaron 浮窗、Consult 预览或原生分栏，而非另建裸 posframe 生命周期。
2. **单一管理空间与返回。**
   [Toggle](https://github.com/sreenivasvrao/agent-shell-hq/blob/1c8eed8fd598ffa4332988713b4eda7e1d45b85a/agent-shell-hq-toggle.el#L486-L550)
   保存此前工作区并建立侧栏。交互值得学习；依赖的 Bad-ptr `persp-mode` 与
   本地 `perspective.el` 不同，应适配本地 workspace API。
3. **分组和状态不够成为运行时真相。**
   [状态与分组](https://github.com/sreenivasvrao/agent-shell-hq/blob/1c8eed8fd598ffa4332988713b4eda7e1d45b85a/agent-shell-hq-peek.el#L185-L224)
   主要看 buffer、busy 与 permission，并按 cwd 分组；dead 分支只检查 buffer
   存活。它不能代替进程健康、target/workspace 身份或 Research 的待处理事实。
4. **命名与资源仍要本地化。**
   [Label](https://github.com/sreenivasvrao/agent-shell-hq/blob/1c8eed8fd598ffa4332988713b4eda7e1d45b85a/agent-shell-hq-label.el#L41-L87)
   截取 buffer 尾部，调用外部 CLI，再经 shell-maker 改名。保持 shell-maker
   名称与进程关联的做法值得保留；直接 `make-process`、只改显示名和无限累积输出
   不适合作为本地通用命名服务。自动命名应显式触发、输出有界，并同步会话身份。

HQ 的
[两秒状态扫描与 teardown](https://github.com/sreenivasvrao/agent-shell-hq/blob/1c8eed8fd598ffa4332988713b4eda7e1d45b85a/agent-shell-hq-toggle.el#L528-L550)
有明确释放路径。本地已有事件钩子，更适合用事件合并替代周期扫描，并在隐藏后
取消订阅。

## 本地职责错位与迁移目标

| 发现 | 当前代码 | Emacs 应维护 | Noema 应保留 |
| --- | --- | --- | --- |
| F1 物理会话与业务身份混在同一边界 | `noema-agent-acp.el` | 启动、连接、模型、原生能力、事件、队列、停止、健康 | 逻辑 Session 与 Run 的业务绑定 |
| F2 通用 UI 由产品包承载 | `noema-agent-render.el`、`noema-agent-abtop.el` | 渲染节流、用量、配额、查看、停止和通用动作 | 研究详情与跳到 WorkNode |
| F3 会话列表强制依赖 Research | `noema-sessions.el` 加载 research、promote、worktree | 所有活动会话及原生历史的统一快照、筛选与导航 | 文档范围、持久名称、归档和研究 lineage |
| F4 通用上下文与写作入口放在 Noema | `noema-context.el`、`noema-compose.el` | gptel 选择、文件/选区引用、普通 rewrite、草稿及剪贴板交付 | 页面保存回执、原地 ediff 约定、Run 冻结与预算 |
| F5 并行代码工作通用但依赖研究注册表 | `noema-agent-worktree*.el` | Git worktree、执行目录、并行尝试、diff 与移除保护 | 尝试结果与研究 Project/WorkNode 的关联 |
| F6 技能库位置与运行时所有权容易混淆 | capability UI、`noema-capabilities.mjs`、vault Skills | 通用库管理入口、来源、配置和上游维护 | 本次 Run 的能力选择、冻结内容与 provenance |
| F7 注意状态、回显和系统通知分散 | ACP attention 与 worker notify | 去重、合并、前后台、冷却、历史和点击策略 | 发出可信的请求/终态语义事件 |
| F8 Pi 和编排有业务依赖 | `noema-pi-router.el`、`noema-orchestration.el` | 可复用的会话与子执行原语 | WorkNode 调度、Proposal、Finding 与研究协调 |

F6 迁移时应保留单一 resolver。把管理入口交给 Emacs 不意味着再用 Elisp 写一份
与 Node 规则竞争的解析器。先把 resolver 拆成可独立消费的能力，再让 Noema 在
准备 Run 时冻结其结果。Finding 的证据审核与通用对话记忆也必须继续分开。

## 通用接口与迁移顺序

拟议的 Emacs 公共层需要五类接口：provider 能力登记；会话 start/list/visit/
stop/close；模型、原生历史和上下文操作；结构化事件订阅；调用方附加业务关联。
快照至少分别提供 `provider`、`native-session-id`、`target`、`workspace`、
`process-state`、`turn-state`、`attention-kind`、模型、用量和可用动作。
本轮通过 `+agent-session-snapshot` 实现其中的运行时快照与能力查询；
持久业务身份由可选钩子关联，状态不是另一份持久对话记录。

依赖方向应为：

```text
普通 buffer / Portal / 配置文档 / Noema 知识库 / Research
                         │
                         ▼
                Emacs 通用 agent 管理
               │                  │
          provider adapter    context / UI / 通知
               │                  │
        agent-shell / ACP     Remote / Aaron UI / aaron-notify

Research 另外拥有 DAG、RunSpec、Proposal、Finding 与结果写回
```

| 顺序 | 交付 | 验收 |
| --- | --- | --- |
| 1 | 先把通知策略放在 Emacs，Noema 只发语义事件 | 独立加载不依赖 Noema；不双报；前台不打断；终态确认后通知。本轮实现 |
| 2 | 拆出物理会话、事件、渲染与能力查询，保留薄 Research 适配 | 关闭 Noema host 后，普通会话的启动、队列、模型、历史、停止仍可用；local/remote 同路径 |
| 3 | 将用量、会话列表、worktree 管理建立在同一快照上 | 普通项目不创建研究清单；同名会话不跨 target/workspace 混淆；未知值明确 |
| 4 | 吸收 ECA 的状态导航与 HQ 的可取消预览 | 光标、滚动、折叠和布局可恢复；隐藏后不刷新；Evil、minibuffer、TTY 行为可测 |
| 5 | 抽离普通 compose/context 和能力库入口 | 仍只有一份 gptel 选择与一份 resolver；Noema 的保存、冻结和证据语义保持独立 |

迁移不能只是整文件搬到 `lisp/tools/agent/`：如果搬完仍需要 `noema-research`、
host 或 durable registry 才能查看普通会话，F1/F3 并未解决。每一轮同时更新调用方、
测试和命名；不留下第二份实现和永久兼容别名。

## 本轮落实

- 通用 ACP、渲染、用量面板、会话列表、Inbox、worktree、Compose/context 与活动
  provider 已移到 Emacs `lisp/tools/agent/`；调用方和测试同步迁移，不保留旧命名别名。
- `+agent-session-snapshot` 报告 provider/native ID、target/workspace、真实进程状态、
  busy、attention、usage 和 list/load/fork/steer 能力；未知能力不会启用原生 fork。
  查看会话只清未读；待审批状态继续读取原生请求，错误事实保留至新输入。
- 普通会话的改名、重启、停止、列表、预览及原生 fork 不需要 Research host。
  Noema 的 `noema-agent-acp.el` 只关联业务 ID、持久动作、私有导出和研究注意原因；
  `noema-context.el` 只适配知识库查询与页面选区。
- 借鉴 HQ，以 Consult 在原窗口预览真实 buffer；退出先恢复原 buffer、point 和
  window-start，确认才走正常显示。共享列表只在可见时合并刷新。
- gptel/Magent 共 156 个源码及支持文件逐字节迁到 Emacs `site-lisp/`；许可证和完整
  原实现保留，来源见 `site-lisp/AGENT-UPSTREAMS.md`。agent-shell/acp/shell-maker
  和 fork-tree 仍是 pristine package-vc 依赖。ECA/HQ 没有加入加载链。
- 模型/配额偏好复制到 `var/agent/`，旧文件保留；既有 worktree 的 Git 元数据维持
  兼容，避免迁移使已有工作失去归属。
- 通知走 `tools/agent/notifications` 与 `aaron-notify :silent t`。正常完成只留历史，
  前台不占回显；等待审批、回答或失败才允许后台提醒，延迟 2 秒合并，间隔 30 秒。
  Run 通知等 host ACK，不重复发送原生回合完成；答复、取消或 buffer 结束撤销待发。
  多权限/问题请求逐 ID 跟踪，点击动作只在用户激活后执行。原自定义回调和禁用仍有效。

能力库 resolver 仍承担 Noema 的项目选择和 RunSpec 冻结，未复制成一套 Elisp resolver。
提供商子代理、跨客户端 live attachment 和没有结构化输入事件的后端，保持真实能力边界。
没有启动 ECA，没有给运行中的用户会话发送测试 prompt 或桌面通知，也没有重启 Emacs。

## 验证

| 检查 | 结果 |
| --- | --- |
| `-Q` 独立加载共享会话、用量、worktree、compose/context 与通知 | 成功；Noema features 为 nil |
| 共享运行时、用量、通知与 Noema 接线 | 62 项通过，含预览恢复、Fork 能力与注意事实测试 |
| Activity 的 agent/Noema 接线 | 8 项通过 |
| `make research-test` | 五组 2 + 154 + 28 + 204 + 121，共 509 项通过 |
| `make macos-test` | 22 项通过 |
| Agent 模块 Doctor | errors、warnings、violations、conflicts 均为空 |
| 共享模块、通知及薄适配字节编译/括号检查 | 通过，无编译警告；产物仅在 `var/` |
| 完整 gptel/Magent 迁移 | 156 个 tracked 文件与 Noema HEAD 逐字节一致 |
| 本地改动 `git diff --check` | 通过；原样迁入的 gptel 保留其既有空白 |

全仓 `test-check` 不是全绿。新增/迁入的共享测试全部通过；Activity 全组恢复为
基线已有的 2 个 Evil 按键失败，Performance 的页签键测试、Remote/Lean 等
另有失败。基线后半段大量 `exit=255 no result`，因此不能声称所有未触及测试
具有完整的前后对照，也不以这一轮迁移修补无关的 UI/Remote 功能。

`state-check` 使用显式改名映射。迁移预期差异为共享 upstream/activity feature、
独立 Activity provider/poll、Noema 通知抑制适配，以及 S/G/b 改接共享列表/Inbox/预览。
同一工作区并行变更还带来 LaTeX/C++ hooks、Flymake save hook、compile board 键表、
native-comp 和 Lean 自动部署配置差异；它们不归本轮提交。快照不是零差异。
验证证据保存在被忽略的 `var/agent-final-*.log` 和 `var/agent-extraction-state/`。
