# Agent Manager 与生命周期验收

用户要求改善整个 agent-shell 操作流程，提供直接的管理入口，研究 ECA 的 UI、
组织与生命周期，以及 HQ 的 Emacs/API 耦合。此次改动沿用
[Agent 管理契约](../../design/agent-management.md)，不改变模型执行权威。

## 源码依据与取舍

| 来源 | 吸收的设计 | 本地实现 |
| --- | --- | --- |
| [eca-emacs workspaces](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/eca-workspaces.el) | workspace 组织、实体选择保留、注意原因、合并刷新、可见性决定 UI 工作 | ETAF Manager 的 workspace 分组、筛选、稳定实体引用和可见期间事件订阅 |
| [eca-emacs chat](https://github.com/editor-code-assistant/eca-emacs/blob/d08930b63ec91898019a931f13556deca7433828/eca-chat.el) | 生命周期和待处理事实分别表达；运行中仍保留交互能力 | 快照的 process/turn/attention/unread 分离；生成时仍能编辑原生输入 |
| [HQ peek](https://github.com/sreenivasvrao/agent-shell-hq/blob/1c8eed8fd598ffa4332988713b4eda7e1d45b85a/agent-shell-hq-peek.el) | 使用真实 shell buffer、项目分组、可返回的导航 | 沿用真实 buffer 预览；Manager 操作共享会话 API，不复制 transcript |
| [HQ toggle](https://github.com/sreenivasvrao/agent-shell-hq/blob/1c8eed8fd598ffa4332988713b4eda7e1d45b85a/agent-shell-hq-toggle.el) | 集中管理空间、回到来源、明确 teardown | Aaron 浮窗和既有 popup owner；隐藏撤销 UI 订阅，不增加 workspace 栈 |
| 本地锁定的 agent-shell / ACP / shell-maker | restart 的 session-id 参数、原生队列、订阅与 cleanup | 通过现有 adapter 调用原生 API，保持包源码原样 |

阅读的是之前审计保存的固定修订源码，同时核对项目入口。ECA 的工具循环、
权限引擎、模型调用和子代理调度没有复制到本地；HQ 没有加入运行依赖。
本轮没有更改 package lock、Remote transport 或引入新的进程管理器。

## 发现并修正的问题

1. 共享重启直接调用 `agent-shell-restart`，未传 session ID；上游明确定义该
   调用会创建新对话。现在传入同一原生 ID，保留名称、显示/隐藏 owner、workspace
   环境、草稿和原生待发队列。活动请求不复制。同步创建异常时，未发送文本放入
   `*Agent recovery*`。Research 的 Run 需先终结，然后调用同一重启 API。
2. 输入入口拒绝 busy 会话，阻断原生常驻输入和 steering/queue。现在区分协议
   活动与旧 shell busy 标志：进行中保留输入，已终结的旧 busy 仍可修复。
3. 自动回收仅检查一个 busy 标志和队列。现在覆盖握手、协议活动、审批、问答、
   Run、RPC、草稿和所有 frame 的可见性。恢复能力要求明确的肯定值，JSON
   `:false` 不再当作支持；对话树使用同一能力查询。
4. 普通取消没有共享的观察状态。现在相同请求只发送一次取消，五秒未确认显示
   stop-unconfirmed；不伪造终态，不自动重放或结束其他请求。观察校验 state、
   client、process、request-count、native session 和 token，防止过时回调影响新工作。
5. 延迟登记、退役和标签刷新缺少明确 owner。现在逐 buffer 持有并清理登记/退役
   计时器，共用一个合并标签刷新；事件订阅检查原 buffer/state，并在其所属
   buffer 内执行。退役执行前重新检查当前身份与工作事实。
6. 日常管理分散在深层按键中。`H-A`、`SPC a a`、`agent-manager` 打开同一
   管理器；普通会话使用一个右侧 dock，管理器本身不拆工作窗口。
7. popup 与普通会话的 `C-c C-e` 意义不同。现在统一为输入，`C-c C-o` 隐藏，
   `C-c C-l` 管理；原有 `C-c M-a` 继续添加 gptel 上下文。popup 的 owner 隐藏路径
   保留调整后的高度，移出时清理隐藏归属。

## 验证与边界

- 新增 `test/agent-manager-tests.el` 的 23 项 C11-C16 契约测试全部通过，覆盖真实 ETAF 挂载、
  长列表内部滚动、空视图、稳定选择、实际上游 restart、未发送输入、环境、
  Research owner、延迟取消、JSON false 能力、资源释放及窗口返回。
- `make research-test` 的 2 + 154 + 28 + 204 + 121，共 509 项通过。
- 共享运行时、ACP、popup、通知及新管理器的组合 ERT 共 160 项通过；Doctor、字节编译、
  括号和空白检查，证据分别在 `var/agent-ux-*.log`。
- `var/agent-ux-state/` 保留改动前后的全启动状态和逐文件测试基线。启动对照
  为 1 条旧项和 12 条新项，均是 Manager 入口与 popup 输入/隐藏快捷键的预期变化。
  管理器保持按需加载；正常启动不加载 ETAF Manager 或启动 agent。
- 全仓前后对照为 132 → 133 个测试文件；132 个既有文件的结果逐行一致，
  唯一新增行是 Manager 的 23 项成功测试。`test-check` 因新增文件返回差异，
  `state-check` 因预期键位变更返回差异，均没有隐藏或改写基线以制造零差异。
- 独立图形实例使用模拟会话挂载真实 ETAF Manager，检查四个会话、两个 workspace、
  审批/输入状态、操作按钮和滚动区域。没有向用户会话发送 prompt 或桌面通知。

全仓测试基线仍有 11 个文件失败或未产出结果，不能描述为全绿。
没有对真实 SSH 上正在执行的任务做断线注入，也没有验证每个 provider 的原生
session/load 行为；取消未确认和外部客户端忙碌因此继续明确表达未知结果。
本机未发现可用于热加载的默认 Emacs server socket；改动在下次重载或启动时生效，
未重启用户 Emacs。
