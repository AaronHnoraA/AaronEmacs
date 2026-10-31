# Emacs Agent 管理

## 范围

Emacs 维护可供普通源码 buffer、Portal、知识库、文档工具和 Noema Research
复用的 agent 接入、会话管理与交互。Noema 维护 Markdown、知识库及研究文档的
业务语义，通过 Emacs 的公共接口调用和组织 agent。

ECA 与 agent-shell-hq 是源码研究对象，不是新增的运行时依赖。研究结论见
[源码审计](../records/audits/eca-hq-agent-management-2026-10.md)。

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
不复制 prompt、回复或工具输出。点击才执行跳转。前台只记历史，保留用户正在
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

## 实现位置

| 职责 | 文件 |
| --- | --- |
| 依赖、Remote 接入与配置登记 | `lisp/tools/agent/config.el` |
| 物理会话、能力快照、窗口与队列 | `lisp/tools/agent/acp.el` |
| 会话列表、Inbox、可取消预览与通用操作 | `lisp/tools/agent/sessions.el` |
| 渲染、用量、活动和工作树 | `lisp/tools/agent/{render,top,activity,worktree,worktree-board}.el` |
| gptel Compose 与引用式上下文 | `lisp/tools/agent/{compose,context}.el` |
| 完整 gptel/Magent 源码与来源 | `site-lisp/{gptel,magent}/`、`site-lisp/AGENT-UPSTREAMS.md` |
| 通用通知策略与原生 ACP 事件订阅 | `lisp/tools/agent/notifications.el` |
| Noema 事件到通用通知的适配 | `lisp/noema/bridge/agent-notifications.el` |
| 通用通知历史与平台路由 | `lisp/core/aaron-notify.el` |
| Research 业务身份和知识库适配 | `site-lisp/noema/lisp/{noema-agent-acp,noema-context}.el` |
| Research 语义事件、持久会话与工作流 | `site-lisp/noema/lisp/{noema-agent-worker,noema-sessions}.el` |

会话快照包含 provider/native session、target/workspace、真实进程状态、busy、
attention、unread、错误事实、usage 和原生 list/load/fork/steer 能力。Noema 通过显式钩子补充业务 ID、
注意原因、动作和菜单；通用选择器不启动或恢复会话。原生 fork 需要正向能力报告。

`C-c A S` 打开共享会话列表，`C-c A G` 只看需要注意的会话，`C-c A b` 使用
Consult 在原窗口预览真实 buffer。取消或确认时先恢复原窗口、位置和滚动，确认后
才执行正常显示。列表只在可见时合并刷新。Noema 的持久记录仍由其业务视图管理。

## 已知差距

- **G1 提供商事件覆盖。** 普通 ACP 的 elicitation 已映射为 input；没有报告
  结构化事件的提供商仍不能通过解析聊天正文推断等待回答。
- **G2 外部执行附着。** 已在另一客户端执行的任务依旧只能显示真实的忙碌事实，
  不能把历史恢复伪装成附着或复制一份执行。
- **G3 状态迁移。** 模型和配额偏好从旧 Noema 路径复制到 `var/agent/`，保留原文件；
  现有 worktree 使用的 `noemaBase/noemaGroup/noemaAgent` Git 元数据保持可读写，
  避免改名使已存在的分支失去归属。它们不要求 Noema 运行。

原生 transcript 与 Noema 的不可变 RunSpec 保留各自的权威，不新增完整对话副本。
