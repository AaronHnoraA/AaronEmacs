# 项目任务图谱

项目开发规范的数据层：Agent 在项目的 `.agent/` 里自己维护任务关系和工作痕迹，需要看时
解析一次。日常它只是一套 Skill pack 和一个脚本，没有常驻服务、没有 Web 页面、没有依赖。

取舍依据见 [PlanWeave 与 Trace Canvas 审计](../records/audits/planweave-trace-canvas-audit-2026-11.md)。

## 组成

| 部分 | 位置 | 职责 |
| --- | --- | --- |
| 数据 | `<project>/.agent/tasks/` | 任务定义和日志，Git 不跟踪 |
| 引擎 | `site-lisp/noema/server/lib/project-graph.mjs` | 全部规则；只用 Node 内置模块 |
| 脚本 | `site-lisp/noema/scripts/project-graph.mjs` | 有 shell 的 Agent 用，不需要 host |
| API | `noema:api:agent:project:graph` | Pi 经 `noema_call` 用，与脚本同一份代码 |
| Skill pack | `noema-project-graph` | 只讲判断；`agent_catalog: true`，随目录披露给 Agent 与 Pi |
| 页面 | `noema-project-graph`（`C-c A g`，项目总览的 Agent cache） | 解析展示；`m` 触发既有维护，`x`、`p` 停止 [Pi 的观察项](../../design/pi-watches.md) |

## 数据

```text
.agent/tasks/
  <id>.md         front matter（id title after related agent review cycles）加 Markdown 正文
  journal.jsonl   事实，只追加：plan claim release submit review state note decision commit
```

没有 front matter 的 Markdown 是自由笔记，工具不解析（`project-agent-management` 的约定）。

## 契约

测试在 `tests/project-graph.test.ts`、`tests/elisp/noema-project-graph-tests.el`。

- **G1 一事实一处。** 任务文件只说任务是什么；发生过什么只在日志。状态、领取、评审轮次
  每次读取时折叠，没有状态字段、索引或数据库，因此不存在漂移，也不需要修复流程。
- **G2 数据都在 `.agent/`。** 根是主检出看到的最近 Noema Project，否则主检出本身；
  linked worktree 落到主检出，不产生第二份。两者都不是时拒绝，不猜目录。读取不创建任何
  东西，`store: false` 表示还没有图谱；第一次写入才建目录，并确保 `.agent/.gitignore`。
- **G3 写入不留半成品。** 日志一行追加并 fsync；崩溃只会留下末尾半行，读取跳过并报告，
  下一次追加另起一行。任务文件 tmp + rename。写者之间用锁文件串行，只有持有者进程确实
  已死才解除，活着就等待后报 423，绝不按时间抢占。
- **G4 重试不生效两次。** 写操作可带 `requestId`，相同 id 返回原事件和 `replayed: true`。
- **G5 计划整体校验。** `plan` 先校验全部任务再写：非法 id、重复、未知引用、环、缺验收都
  使整批不写；`dryRun` 不创建存储。
- **G6 顺序可解释。** `phase` 只解释记录，不改变任何状态：`ready`、`waiting`（带 `on`）、
  `claimed`、`stale_claim`、`awaiting_review`、`blocked`、`blocked_by_dependency`、`cycle`、
  `done`、`dropped`。被放弃的依赖不算满足。有未完成任务却无一能动时 `deadlocked`。
- **G7 领取有租约。** 非 `ready` 不能领取。持有者的任何记录都续租，4 小时无记录即过期；
  过期不自动回收，只能显式 `take`，并记下从谁接手。
- **G8 完成要有依据。** `submit` 需要 `evidence`，且任务里的 `- [ ]` 已全部勾选；确实没做完
  用 `partial` 如实提交，未完成项进日志，`check` 持续报告。`done` 只能经 `submit`、`review`。
- **G9 评审是回路，不是形式。** `review: required` 的任务提交后等待另一个 Agent；
  `needs_changes` 带反馈回到 `todo`，默认两轮，用尽变 `blocked` 由人决定，绝不算完成。
  自评需要显式 `self`，并被报告。
- **G10 引用可查。** commit 记录只存 sha，Git 不认识时拒绝；非 Git 目录不校验。
- **G11 规则在代码里。** 拒绝信息说明原因和出路，`phase` 带 `advice`，`check` 的每条问题带
  `fix`，`status` 给下一步。新增判定必须同时有建议文本（测试保证）。Skill 只写机器判断不了
  的事，体积由测试卡住；一条规则能由工具执行，就不写进 Skill。
- **G12 分三层。** 破坏结构的拒绝；工具能判断但不应阻塞的警告（无证据完成、完成项下有
  未完成依赖、长期安静、失效引用、高扇出未评审、半行、残留临时文件）；模型判断留给
  Skill。沿用 Noema 的规则：拒绝不能逼人说谎，所以 G8、G9 都留了如实的出路。
- **G13 只读页面。** 页面每次打开或 `g` 时解析，不留计时器和状态，只显示当前刷新的回答；
  `project:status` 与 Agent 看到的是同一份答案。`m` 调 `agent:project:maintain`，其中图谱部分
  只删一小时前的临时文件，永不碰任务和日志。
- **G14 项目入口一个。** `agent:project:status` 的 `graph` 是摘要，没有图谱时为 null。
  `agent:project:graph` 是唯一读写入口。普通 ACP 会话的隐藏入口仍只指向
  `noema-project-maintenance`，不新增注入。

## 不做

- 不接 Research 的 Task/Job/Workstream：Task 依赖不能跨 workstream，而 workstream 基本一会话
  一个，项目级的图放不进去。研究 Run、Finding、Proposal 保持原样。
- 不写 Agenda `@@todo`、`.noema` 或 WorkNode。
- 不自动重跑、不自动派发、不因图谱状态改变任何文件。
- 不做 Web 画布。需要图时看 `graph --text` 或 Emacs 页面。
