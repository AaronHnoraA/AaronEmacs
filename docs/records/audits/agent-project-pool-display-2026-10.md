# Agent 项目池与窗口管理审计

用户反馈会话之间的窗口、buffer 和返回逻辑整体不可用，并要求一个项目里的
agent-shell 作为一个池统一管理。本轮按
[Agent 管理 C23–C27](../../design/agent-management.md) 与
[窗口生命周期 C30](../../design/window-lifecycle.md) 审计全部显示入口。

## 发现与修复

修改前先保存启动状态和 135 个测试文件的结果，再写交叉操作复现。最初 8 条
全部失败，证据保存在 `var/agent-display-reproductions.log`。单条启动、预览、
隐藏分别成功，不能证明 A→B→隐藏、跨 frame 或重启恢复的组合正确。

| 原有问题 | 最终行为 |
| --- | --- |
| 默认显示遍历并退出其他 agent 窗口 | 只切换目标聊天窗格；保留手工分屏和其他 frame |
| A→B 后隐藏从 buffer 历史复活 A | 返回记录属于窗口；dock 撤窗，embedded 恢复原编辑内容、位置与滚动 |
| popup/here 的位置随 buffer 固定，旧闭包覆盖已改作编辑器的窗格 | 常规位置由当前视图决定；旧手动位置闭包失效，领域独立干预窗口保留显式契约 |
| Popup、here 不出现在普通会话的项目 tabs | 所有用户会话查询同一物理注册表，来源只作为行标签 |
| 管理器按来源拆组，停止只能作用于来源组 | 按 target 与规范化项目根组织池；池停止逐个经过执行 owner |
| 从预览 A 打开 B 会在子 frame 新建 dock | 先退出预览所属管理器，再在其父 frame 常规显示 |
| 管理器在另一个 frame 已打开时，当前 frame 调用只会隐藏旧管理器 | 保留内容与 Runtime，重建显示宿主并跟随本次调用者，退出返回本次来源 |
| 原生 restart kill 会隐式删除侧窗，通用异步结束回退也会拆窗 | 物理替换期间保留确切视图；原生 kill 查询与 client 清理照常执行，恢复每个窗口的返回记录 |
| 重启和 `/resume` 各走不同的显示分支 | 共享原生重启路径；同 ID 重连保留草稿与原生队列，历史恢复替换原窗格 |
| 关闭、改名、退役清理可能使用调用页面的项目或漏掉 dormant retired | 从被操作会话的 target/project 取池；退役清理查询整个池，名字不跨 target 抢占 |
| 同一 buffer 在多个 frame 的 popup，隐藏可能选中别的 frame | Portal 适配只隐藏当前 frame 的实际视图 |
| 从 worktree 会话新建会话，按目录重新发现项目会拆散原池 | 新建沿用原会话登记的项目根和实际执行目录 |
| 手动视图没有任何来源历史时停在 scratch 或另一 agent | 最后主窗格交给通用窗口层，返回项目目录；会话继续保留 |

池没有第二份执行注册表、对话副本或队列。通用实现位于 Emacs：`acp.el`
提供会话快照与池身份，`views.el` 管理显示和返回，`sessions.el` 提供日常入口，
`manager.el` 与 `menu.el` 消费公共操作。Noema 保留研究身份、Run 与业务动作。
私有导出任务仍不进入日常池；供用户接管的干预会话可管理，并保留独立显示函数。

## 日常操作

| 按键 | 行为 |
| --- | --- |
| `C-c a` | 召回当前项目最近实际使用的会话；聊天中再次执行返回编辑区 |
| `C-u C-c a` / `C-c C-a` | 选择当前项目池成员；空池明确选择新建，取消不启动 |
| `H-A` / `SPC a a` | 当前项目池管理器；前缀查看全部项目 |
| `C-c A S/G/U` | 同一个管理器中的 All / Inbox / Usage |
| `C-c C-n/p` | 在当前聊天窗格切换项目池会话 |
| `C-c C-o` | 隐藏当前视图，执行、草稿和队列保留 |
| `?`（聊天正文） / `C-c C-z` | 同一个原生 Transient 操作菜单 |

管理器左侧按项目折叠，来源与模型放在会话行，避免长标签挤掉信息；右侧预览
真实聊天 buffer。预览不清未读，也不提交审批。研究持久记录仍由 Noema 维护，
本轮没有把业务身份与物理会话合并为同一个执行对象。

## ECA 与 HQ 的学习落点

沿用[固定修订源码审计](eca-hq-agent-management-2026-10.md)，本轮重新核对
`eca-workspaces--point-state`、`--window-state`、`--render` 与 HQ toggle 的
`--capture-states`、`--restore-idx`、`--preview-current`、`--teardown`。

ECA 的 workspace 组织落实为 target/project 池；导航实体、折叠与刷新独立于
执行状态，来源不再分裂项目。HQ 的真实 buffer 预览与退出归还落实为临时预览
宿主、显式常规打开、每次调用的来源窗口。日常召回使用会话实际使用时间，后台
渲染和管理器预览不会成为新的使用记录。生命周期上继续区分显示、回合、物理
client 与研究 Run，关闭 UI 只释放自身资源。

没有引入 ECA 服务、HQ 运行依赖、第二套 Perspective 空间或模型工具循环。
本地已有 Aaron ETAF、Transient、Portal、Remote 与原生 ACP 仍各自负责原有层。

## 验证

- `test/agent-display-audit-tests.el` 覆盖最初复现及 target 隔离、隐私、取消、
  失效旧闭包、退役清理、后台关闭、失败重启和恢复输入。原始 8 条从全部失败
  变为通过。
- `var/agent-display-regressions.log`：188 条相关测试全部通过，包含真实 stdio
  的审批、问答、队列、取消、同 ID 重连和传输崩溃。最终显示审计包含 25 条。
- `var/agent-display-integration.log`：319 条 Noema、上下文、Compose 文档动作及
  snippet 检查全部通过；`var/agent-display-runtime-integration.log` 的 15 条通用
  运行时检查全部通过。
- `python3 test/agent-manager-gui-e2e.py`：独立 GUI daemon、两个真实 ACP client，
  验证实际按键、鼠标预览、原生筛选、三个页签、菜单、未读与关闭 UI 后继续执行。
- `python3 test/agent-manager-gui-e2e.py display`：独立 GUI daemon、三个真实 ACP
  client、两个普通 frame，验证项目池原生 tabs、手工分屏、后台审批不抢焦点、
  管理器跨 frame 重定位、预览切换退出、两处视图原位重启、草稿与队列保留、
  原生历史恢复、popup 重启及同 buffer 多 frame 的 popup 独立隐藏。
- 相关源文件编译到临时目录，无编译警告；不覆盖运行时编译产物。Doctor 报告
  0 errors、0 warnings、0 key conflicts；95 条 policy hits 保留在原报告。

完整仓库的启动状态和测试结果与最初基线比较，日志位于
`var/agent-display-{state,test}-check.log`。新增 `C-c a` 在四张继承键图中可见；
当前启动状态差异为 9 行移除、5 行增加：四张继承键图新增 `C-c a`；其余是
同时进行的 fingertip feature/变量移除、旧 project-tools 两个入口移除、
Noema Research mode 与 Perspective 清理 hook 移除，以及 Pi 的 startup hook
增加。这些独立变动保留在原始差异中，不以“全仓库无变化”描述这次行为调整。

完整测试覆盖 136 个文件，新增失败文件为 0，原有 7 个失败文件保持同样结果：
`init-activity`、`init-diagnostics-entry`、`init-neo-common`、`init-performance`、
`lean-infoview`、`lean`、`portal-focus-gui`。`make test-check` 因测试数量、增加和
移除发生预期变化仍返回 diff 状态，不代表全仓库全绿。完整检查之后补入的两个
工作树/无来源返回用例，已包含在最终 188 条相关复测中。

完整检查还发现上下文选择文件缺少闭括号、移除旧 Noema mode 后 snippet 测试
残留无 mode 的样例，以及旧测试要求领域会话排除于项目池。这些加载和契约集成
问题已修正；Pi 测试也隔离了新增 Emacs 常驻父会话，避免项目复用测试启动个人
Pi；Compose 检查改为覆盖当前 Markdown 文档、会话引用及公共动作接口。
独立补测见 `var/agent-display-integration.log`。

## 边界

本轮 GUI 使用真实 Emacs frame、窗口、buffer、原生聊天控件与本地 ACP 子进程，
模型输出由确定性 fixture 提供，不代表重新验证所有提供商网络、计费与外部任务
接管。提供商报告能力、历史恢复与活动执行附着的边界继续遵守 C3/C4。
没有做像素截图验收；通过的是实际窗口、焦点、按键与生命周期断言。
