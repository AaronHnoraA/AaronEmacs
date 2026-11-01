# Agent Manager UI、交互与生命周期全流程验证

本轮承接用户对 Sessions、Inbox、Top、popup 和 Noema agent 组的反馈。长期契约见
[Agent 管理 C17–C22](../../design/agent-management.md)；此处记录本轮实现与验证边界。

## 源码学习落实到行为

沿用[源码审计的固定修订](eca-hq-agent-management-2026-10.md)，不引入 ECA/HQ 运行时。

| 源码依据 | 学习的机制 | 本地实现 |
| --- | --- | --- |
| ECA Emacs `eca-workspaces--render`、`--point-state`、`--window-state`、`--toggle-session` | 按 workspace 组织、实体身份与折叠独立于渲染、刷新保留导航位置 | target/workspace/来源组、稳定 ref、保留折叠与选择、可取消筛选 |
| ECA Emacs `eca-workspaces--chat-status-glyph`、`--chat-hint`、Transient | 审批、问答、运行与闲置分开；日常视图留给内容，操作按组组织 | All/Inbox/Usage 共用页面；状态与未读分别呈现；请求直达原生控件，菜单按能力显示 |
| ECA `chat/tool_calls.clj`、`chat/lifecycle.clj` | 工具、回合和 UI 有不同的状态与归属，取消不是成功，元数据不打断内容组 | 物理 process、turn、attention、unread 分开；取消 token 与 state 绑定；上下文展示与协议负载分开 |
| HQ `agent-shell-hq-peek--preferred-buffer`、`--preview-current`、toggle 的 `--preview-current` | 使用真实原生 buffer；导航时预览，确认后进入 | 左侧 ETAF 导航、右侧原生聊天；保留审批、问答、输入和原生工具折叠；不复制 transcript |
| HQ toggle `--capture-states`、`--maybe-refresh`、`--restore-idx`、`--teardown` | MRU 不当成状态变化；按实体恢复；视图退出释放自身资源 | 稳定排序与选择；事件合并刷新；隐藏退订并撤下预览，关闭释放 Runtime |
| 锁定的原生 agent-shell/ACP/shell-maker | 协议、进程、prompt queue、permission 与 elicitation 已有权威实现 | 通过公共交互命令和共享 adapter 操作；不添加第二条工具循环、权限规则或队列 |

ETAF 的方向键、Tab、Escape 和 `?` 有明确的框架归属。业务按键使用公开 Host `:keys`；
管理器的 `.` / `C-c C-z` 打开 Transient，`?` 保留原生帮助。聊天正文的 `?` 打开
同一 Transient，输入区可以正常输入问号。这避免了只改 minor-mode map 却被框架覆盖。

## 最终界面与归属

- `H-A`、`SPC a a` 和 `C-c A RET` 开关管理器；`C-c A S/G/U` 选择同一浮窗的
  All/Inbox/Usage。`+agent-top` 的 GUI 入口也进入 Usage。完整配额和远程控制能力
  保留在显式的 `+agent-top-report`，表格保留为 TTY 后备和诊断入口。
- 导航按 target、workspace 与来源组组织。Noema Research（包括 Pi）、Portal、
  worktree、普通会话与 Previous generations 共用 Emacs 注册表。Noema 只提供只读
  关联和领域动作；打开管理器不启动 Noema host。
- 原生聊天用紧凑文字 header，tab 显示会话名及活动提示，并有 Agents 入口。无工作
  的旧物理代际不挤占 tab；仍有请求、队列或草稿的旧代际保持可达，管理器保留记录。
  原生恢复能力不明确时保留旧 buffer，不以“已退役”为理由丢弃其历史。
- 普通常规显示清理同一 buffer 的重复窗口；临时预览保持独立生命周期。从预览执行
  原生输入/打开操作先返回父 frame 的显示 owner，避免在 child frame 里再建 dock。
- 研究正文先显示 prompt。引用、Skill、记忆等放入默认收起的原生 Context 区；
  发送的 ACP content blocks 与冻结输入保持原样。
- 模型、fork、history、队列和 Research 动作按实际能力提供；组停止逐个调用共享
  Stop，Research Run 仍由其业务 owner 取消。
- 暖会话清扫归 Emacs，按 target/workspace 保留最近的后台可恢复 client。
  单次 timer 只在有后台物理 client 时存在，最后一个离开及 Emacs 退出时取消。
  Noema 的旧变量是别名、旧命令转发到共享 sweep，旧周期 timer 被撤销。

## 验证证据

- `var/agent-flow-regressions.log`：170/170。包含 34 项管理器/生命周期契约、
  ACP、Portal、ETAF 宿主、Top 与真实 stdio 工作流。
- `var/agent-flow-noema-api.log`：250/250。包含 Research worker、Session、API、
  attention、Inbox 与 workflow；保护业务终态和原生执行边界。
- `test/agent-flow-e2e.el` 与 `test/fixtures/agent-flow-acp.mjs` 启动真实 Node ACP
  子进程，经原生 agent-shell 完成初始化、回复、审批按钮、elicitation 表单、忙时
  输入排队、重复取消、显式继续队列、带草稿同 ID 重启、历史加载与进程崩溃。
  断言协议事件只发一次；fixture 不访问网络、凭据或真实项目。
- `python3 test/agent-manager-gui-e2e.py` 在独立 GUI daemon 中启动两个真实 ACP
  client，验证实际 child frame、三入口、来源组、有效键图、预览未读、Transient、
  Return 确认、原窗口返回和关闭 UI 保留进程。按键通过真实 command loop 执行，
  包含 Transient 的 pre/post-command 清理。
- `var/agent-flow-providers.json`：真实 Codex、Claude、OpenCode 均完成原生初始化，
  分别报告 8、13、44 个可用模型。仅初始化，发送 prompt 数为 0。
- `var/agent-flow-opencode-official.log`：实际配置使用的官方 OpenCode binary，
  ACP → 本地确定性 provider 两轮对话、读文件工具与 session/load 成功；本地模型
  请求 3 次、付费模型请求 0 次。Homebrew 同名 binary 的旧 smoke 命令不支持
  `--cwd`，不能据此判断配置中的官方 binary 失效。
- `var/agent-flow-compile.log`：相关共享源文件与 Noema adapter/worker 编译无警告，
  tools/agent doctor 无 error、warning、policy hit 或 key conflict。bytecode 写入
  临时目录后删除，不覆盖用户的运行时构建。
- `var/agent-flow-state/` 保存本轮前后状态与全量 ERT 文件结果，区别有意改动与
  工作区内其他并行修改。全量检查 134 个文件，未见新增失败；剩余 7 个失败或无结果
  文件均已在基线中出现。相关验证单独列明，不能把全库描述成全绿。

## 验证边界

GUI 验证使用真实窗口、布局和交互断言。macOS 窗口截图导出失败，没有可用截图，
不把文字 dump 称为像素级视觉验收。三个真实 provider 的无 prompt 初始化不等于
三者都完成了付费模型对话。新真实流程验证在本机进行，未新增真实 SSH 目标故障演练。
既有 Remote 目录、环境和进程放置路径继续复用，相关回归保留。
