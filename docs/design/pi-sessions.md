# Pi 会话与项目助手

## 范围

Pi 是 Emacs 的常驻助手，并为每个进入的项目维护项目助手。本文约定 Pi 的独立
会话管理，以及它与首页、buffer、执行 Agent、Markdown 和 Noema 服务的边界。

## 模型

```mermaid
flowchart TD
  D[Dashboard] --> E
  E --> M[Pi Sessions]
  E[Emacs Pi] --> P[项目 Pi]
  P --> W[执行任务的 Agent]
  M --> S[Pi 会话记录]
  S --> R[共享 ACP 运行时]
  E --> A[按需披露的 Noema API]
  P --> A
  A --> N[Markdown / 项目 / 上下文 / 记忆服务]
```

| 对象 | 负责什么 | 不代替什么 |
| --- | --- | --- |
| Pi 作用域 | Emacs 全局，或一个 target 上的项目根目录；管理关系属于作用域 | 原生对话身份 |
| Pi Session | 一个真实原生对话的身份、名称、作用域、创建与归档记录 | 进程或 buffer |
| 活跃指针 | 该作用域下一次继续哪一个 Pi Session | 整段 transcript |
| ACP client / buffer | 当前连接、队列、权限、草稿和显示 | 持久 Session 身份 |
| 执行 Agent | 接受明确任务与上下文，返回结果 | 项目 Pi 的管理对话 |

Emacs Pi 负责编辑器操作、跨项目路由和找到项目 Pi。项目 Pi 负责项目上下文、
任务分工、prompt 文档和交接。执行 Agent 只获得该任务需要的材料和写入范围。
项目 Pi 的管理关系指向 Emacs 作用域；主 Pi 换对话不会销毁项目 Pi 的历史。

## 契约

- **C1 独立会话域。** Pi 自己维护原生对话引用与活跃指针，保留同一项目的历史
  对话。Pi 不自动 `session:promote`、绑定 Research 会话名或创建 Workstream。
  Noema Research 的 Session、Run、权限与证据仍由原服务管理。
- **C2 复用运行时。** Pi 使用共享 ACP 连接、原生历史、队列与权限回应。
  Pi 的独立会话管理不再实现一套进程或 transcript。恢复使用真实 native ID；
  新对话与恢复分开，失败不能悄悄换成空白对话。刚初始化的空对话只有运行时
  身份；提供方确认保存历史后才成为持久记录，不为制造历史发送虚假 prompt。
- **C3 初始化没有任务副作用。** 主程序启动时进入 Dashboard，Pi 在后台初始化，
  不提交模型 prompt、不切换窗口。项目激活复用项目 Pi，切换项目不关闭旧会话。
  CLI 和 stdio MCP 入口按文件真实路径识别启动，兼容 Emacs 配置目录的符号链接。
- **C4 视图可发现。** Ibuffer 的 Pi 分组列出真实 Pi buffer；Pi Sessions 查看
  当前与历史对话，普通 Agent Manager 只管理执行会话。后台不显示窗口不等于隐藏
  buffer。Dashboard 直接打开主 Pi；Pi Sessions 保留独立历史入口。
- **C5 生命周期分开。** 关闭聊天视图不停止 client；停止当前回合、结束 Pi、
  换新对话、归档历史分别处理。结束项目 Pi 不结束该项目的执行 Agent。
- **C6 逐步披露。** 首次只提供小型 API 入口。先搜索能力名字，再查看一个接口，
  最后调用；本轮只发送任务、项目与位置引用。Skill 正文和参考章节按需读取。
- **C7 文档复用。** 申请 prompt 文档由 Skill 指导项目 Pi 创建、打开普通 Noema
  Markdown。用户保存并发送后才执行；改写用版本化提案 API，由人审阅。
- **C8 状态可核对。** native ID 与归档事实持久化；运行、忙碌、等待权限、可见与
  连接失败从实际 ACP 状态派生。恢复历史不假装附着到其他进程的活动执行。
- **C9 全局入口。** `M-x pi`、`C-c A P` 和 Dashboard 的 `pi` 召回同一个主 Pi。
  在该聊天窗口再次调用时收起视图并返回来源；前缀参数打开来源项目 Pi。
  主 Pi、项目 Pi、执行 Agent 分别保留真实 buffer 和会话身份。
- **C10 当前上下文是查询。** `emacs:pi` 的 `context` 从当前选中窗口解析编辑来源，
  返回文件、target、项目、位置和选区的引用，不复制正文、不保存、不启动会话。
  Noema 页面只报告已知文件身份，不用占位 buffer 伪造页面的行号、选区或修改状态。
  主 Pi 不把自己的启动目录当成用户当前项目。
  没有可用编辑来源时返回 `sourceAvailable:false`，目录与项目为空，不回退到 Pi 的 home。
- **C11 调度复用公共 Agent。** Pi 通过 `emacs:agent` 查询、创建、发送、读取结果
  或停止真实会话；没有另一份 Agent 注册表、队列或执行状态。变更和读取必须携带
  buffer 与 native ID，防止把旧指令发送到同名新会话。查询不打开 UI；创建不提交
  任务；发送默认后台受理，只有显式 focus/show 才显示。受理、排队和完成分别报告。
  正常完成后继续原生排队任务；停止、失败或拒绝后保留挂起任务，等待显式恢复。
- **C12 Idle 预热。** 自动启动和项目激活只登记一次 idle 工作，合并尚未预热的
  项目到最近一次选择。明确打开或发送立即复用原入口；关闭自动模式或退出取消
  待执行工作。已有对话不随项目切换结束，主 Pi 不按普通执行 Agent 回收。
- **C13 异步工作有归属。** 每个物理 Pi 的历史确认最多一个在途请求，合并同一
  回合的重复观察；回调检查 client 代际、native ID 和当前作用域归属。工具事件
  合并为一个有上限等待的协调查询，同一 client 只允许一个在途协调请求，只有派发时
  才检查 Research 项目。结束 client、关闭 buffer 或改变其 major mode 都会
  释放定时器、订阅和过期请求的写入资格，不让旧结果改变新会话的活跃指针。
- **C14 元数据按事实更新。** 使用文件签名校验的有界缓存，不轮询文件。外部替换
  和损坏在下次查询时可见；写入前编辑副本，失败不污染已读事实，相同值不重写。
  同一 native 回合重复通知不反复解析与保存整份索引。
- **C15 列表按需刷新。** Pi Sessions 合并突发状态事件，只有可见时才安排单次刷新；
  隐藏、关闭和退出取消待执行刷新。列表静止时没有周期刷新或元数据读写。
- **C16 回合通知一致。** 原生输入和 API 结构化发送完成后都经过共享回合通知，
  让历史确认与文件刷新收到同一事实；陈旧回调不报告当前会话完成。
- **C17 管理按作用域分开。** 主 Pi 属于全局 `emacs`，项目 Pi 属于对应项目。
  Pi 的 tab、切换、管理入口、批量关闭和关闭回退不混入执行 Agent；执行 Agent
  的项目池与管理器也不接管 Pi。Pi 聊天的管理按钮和 `C-c C-l` 打开本作用域的
  Pi Sessions。项目会话列表跟随明确的项目切换，主 Pi 列表保持全局；带前缀
  打开 Pi Sessions 才查看所有作用域。空项目列表的新建仍属于该项目。
  `/new`／`C-c A n` 在 Pi 中新建本作用域对话，在执行会话中创建执行 Agent。
  分类依据是管理角色来源，手动启动的 Pi 提供方仍是普通执行会话。共用 ACP
  物理注册表与生命周期，不增加另一套进程管理。同名执行会话不会使 Pi 退役，
  命名查询与替换也遵守管理作用域。

## 工作流程

1. **打开主程序：** 首页选择 Dashboard → 后台恢复 Emacs Pi → Ibuffer 出现 Pi。
   未配置或初始化失败时保留首页，并通过现有诊断与 Pi Sessions 显示事实。
2. **进入项目：** 根据 target 与项目根定位作用域 → 恢复活跃项目 Pi；没有真实
   原生记录时新建 → 后台登记管理关系。它不依赖 `.noema` 或研究 Project。
3. **准备任务：** 通过项目 Pi 申请 Markdown 文档 → 用户编辑 → 保存后发送给
   选中的执行 Agent。文档、会话、任务产物分别保留自己的身份。
4. **执行和反馈：** API 创建或修订提案 → 页面响应保存通知 → 人审核或评论 →
   同一 Session 按文件位置继续。提交成功与执行完成分别报告。
5. **长任务接续：** 先按预算检索与选择上下文，再生成可核查的交接；需要新对话
   时保留旧 native ID 和新旧关系，更新活跃指针。原生压缩按实际能力提供。

## 存储与实现

Pi 的 `sessions.json` 保存在自己的状态目录，包含 Session 记录与作用域的活跃
指针，仅保存元数据及原生引用。`assistants.json` 自动导入一次；首次进入一个
没有新记录的项目时也可读取旧 `.agent/pi.json`。来源文件保留。旧 Research
注册表使用 `Pi Sessions` 的 `i` 显式读取；导入不替换已有活跃对话，也不写回
Research。新建和归档后不再被旧索引悄悄重新激活。

Pi 原生 SDK 在首次 assistant 消息前不会保存对话。控制器通过成功恢复或原生
`session/list` 确认持久性，随后才保存活跃指针。未保存的空对话仍在 Ibuffer 和
Pi Sessions 中可见。早期索引中的此类引用保留并标为 `not saved`，不再自动
恢复。未知的导入引用不被静默丢弃；恢复失败保留引用并显示失败，用户可显式
换新对话。通用 Agent 的恢复策略保持自己的约定，Pi 启用严格恢复选项。

Dashboard 的 `pi` 入口打开主 Pi。`M-x noema-pi-sessions` 查看当前 Pi／项目的历史；
`C-u M-x noema-pi-sessions` 查看全部。`RET` 恢复，`n` 新建，`a`
归档，`i` 导入，`g` 刷新。初始化中的真实 buffer 也列出；有连接错误时显示
实际状态，不伪造可用的空白对话。Ibuffer 的 Pi 组同时列出管理视图和真实聊天。

普通 Pi 会话继续复用既有 Hy Memory 召回与写入；项目审核记忆、冻结 RunSpec 与
回执只在其实际服务约束下调用。需要 Workstream 的接口仍需要真实身份。
父子 Pi 不复制整段 transcript，跨会话交接使用实际 Markdown 记录和必要引用。

`noema-pi-router` 管理角色、启动与派发；独立 Session 模块管理记录与会话列表；
`tools/agent/acp` 管理实际 client；Ibuffer 与 Dashboard 由既有 Emacs UI 配置管理。
API 与 Markdown 审阅见 [Noema 交互](../noema/design/interaction-plan.md)。

| 实现入口 | 所有权 |
| --- | --- |
| `site-lisp/noema/lisp/noema-pi-router.el` | 主／项目 Pi、作用域、部署、管理关系和派发 |
| `site-lisp/noema/lisp/noema-pi-sessions.el` | 持久记录、活跃指针、导入、新建、恢复、归档和列表 |
| `lisp/tools/agent/acp.el` | 原生连接、历史查询、严格恢复、队列、权限和物理重启 |
| `lisp/tools/agent/api.el` | 公共 Agent 的语义调用，复用注册表与 ACP，不另存执行状态 |
| `lisp/tools/agent/{selection,context,compose}.el` | 编辑器选区、按 Session 注入和普通文档动作 |
| `server/Features/Agent/api.mjs` | API 名字披露、单接口说明、调用和版本化文档提案 |
| `noema-prompt-document` Skill | 创建／打开普通 Markdown 的操作指导 |

`noema:api:emacs:pi` 支持 `context/list/status/ensure/send/resume/new/archive`。`context`
无需 root；其他作用域操作显式传 companion 或 root。后台发送不改变窗口焦点。`list`
分页返回持久历史，并单独报告实际运行的主／项目 Pi；`resume/archive` 接受 Pi
自己的记录 `sessionId`，与 `nativeSessionId` 区分。`send` 的受理不表示回合完成。
历史使用 `total/nextOffset`，项目子会话使用 `childrenTotal/childrenNextOffset`，
两者均接受同一 offset/limit；历史为空时也能遍历所有项目子会话。

## 已知差距

- **G3** 远程项目 Pi 尚未接入完整 target 身份与 API 路由，当前明确拒绝。
- **G4** 原生压缩与跨对话的自动交接尚未做成 Pi 专属 API。当前按实际原生能力
  或 Skill 指导操作，不把指导宣称为已实现的后台接口。

验证入口：`make research-test` 包含独立 Session、Ibuffer、Dashboard、恢复失败
和原有 Agent 契约。可选 `make pi-live-smoke` 使用已安装的 `pi-acp`，在临时目录
通过原生 SDK 建立明确标识的历史 fixture，验证两个真实 client、精确恢复、复用、
物理重启后的父子关系和窗口保持，不提交模型任务、不改用户历史。API 与文档
写入另经真实 host 的 CAS 和页面通知检查。
同一 target 还运行本地 ACP 测试进程，验证管家入口、来源上下文、真实队列续发、
结果分页、草稿保护和停止挂起；`pi-butler-e2e` 在 GUI 中额外验证多 frame 来源。
`tests/agent-api.test.ts` 还以真实子进程验证符号链接下的 CLI 和 MCP 启动。
`test/pi-idle-tests.el` 覆盖 C12–C16 的合并、缓存失效、失败写入、晚到结果和资源释放；
`pi-butler-e2e` 验证 API 完成后的历史确认、排队续发与空闲无请求、无索引写入。
`make pi-idle-benchmark` 在隔离的临时索引与 buffer 上统计 JSON 解析、重复观察写入、
突发事件重画和暖会话候选检查次数；不连接模型，不读取用户会话。
