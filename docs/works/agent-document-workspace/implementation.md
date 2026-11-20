# 文档驱动的原生任务协作实施计划

> **For agentic workers:** REQUIRED SUB-SKILL: Use `executing-plans` to implement this plan task-by-task in the current session. Steps use checkbox (`- [ ]`) syntax for tracking. 本计划待审阅，本文中的接口均是拟新增或拟扩展接口。

**Goal:** 在现有文档与批注上完成“派工、持续观察、原处介入、写回结果、Review、人工完成”的完整闭环。

**Architecture:** 扩展同一个批注日志的生命周期事实；任务、执行尝试、会话与交付分别有身份。
共享纯折叠规则定义状态，已有 ACP 回执与原生队列报告执行事实，ETAF 侧栏与 Agenda / Inbox 只投影这些事实。
主编辑区继续使用普通 Emacs buffer 或已有 Noema 页面，交付与审阅回到原文。

**Tech Stack:** Emacs Lisp、ERT、ETAF / ETAF UI / Aaron UI、现有 agent-shell / ACP、Noema Node host 与 CM6、现有 Agenda 服务、Markdown / JSONL。

**Spec:** [原生设计](README.md)，同时读取 [源码学习](sources.md)、[Agent 管理](../../design/agent-management.md)、[批注契约](../../noema/design/comments.md)。

## Global Constraints

- 文档是工作的地方，也是结果的归属。打开任务首先定位原文；任务不拥有另一份研究正文。
- 同一任务的四阶段是 `todo` / `doing` / `review` / `done`；排队、审批、回答、冲突和断开另记原因。
- `end_turn` 只证明本次执行回合结束；没有明确交付不能进入 Review，更不能 Done。
- Agent 的完成请求进入 Review，用户接受才写人工批准。公开 Agent API 拒绝 `accept`，不相信自报的 `actor: human`。
- 扩展现有 `.noema-comments.jsonl`，不新建任务数据库、另一份批注存储、会话注册表或队列。
- 新交互视图使用 ETAF 公共组件和 Aaron 主题；Pi 仍是实际 agent-shell buffer，Graph 仍是现有 board。
- 新动作加入 `tools/agent/actions`；复用 `H-t` / `H-T`，不追加一组全局键位。
- 沿用 Noema Project / workspace 与 `/fs:TARGET:/path` 身份；consumer 不按 `file-remote-p` 或 target 名分叉。
- 事件驱动、可见时重绘；任务持久化不能因侧栏关闭而停止。未知执行不自动重放。
- JS 与 Elisp 的生命周期折叠由相同 fixtures 核对。源码、未保存 buffer、版本检查仍走既有写入边界。
- 只提交本任务的文件与改动，不改变另一个会话的暂存区。共用文件必须等对方完成该接口，或在隔离 worktree 集成。

## Review Focus

1. 同会话的第二项任务仍排队时，第一项的 busy / end_turn 不应改变它的阶段：任务 2 的原生队列 fixture。
2. 用户改了请求或发起第二次尝试，第一次的晚到提交不能结案新工作：任务 1 的 revision / attempt fixtures 与任务 3 的 API 测试。
3. 同目录 b 文档不能按 a 文档的 id 查询、结案或修改它的批注：任务 1 的公共查询 / 修改入口测试。
4. 审阅结果时文件已被别人修改，不能把旧交付当作当前版本批准：任务 3 的审阅版本测试。
5. 没有活动会话、侧栏隐藏或 Emacs 重启，待审任务仍可定位且不会重新执行：任务 4 的脱离会话投影与任务 5 的恢复验收。

## 工作树与写入归属

规划时 Emacs HEAD 是 `5279f5b2`，Noema HEAD 是 `408bbd3`。共享工作树里另一个会话
正在重构批注锚点、日志、页面呈现、`collab.el` 与 `panel.el`；这些未提交变化不属于本任务。
实施前重新检查两边 HEAD、暂存与未暂存变化。不能把这两个 SHA 当作最终集成基线。

本任务先独立建立纯生命周期模型和回执扩展。需要接入另一个会话拥有的现有文件时，
以它完成后的接口为基线进行一次完整集成；不能覆盖工作树去恢复旧版，也不能只创建
新模块却让消费者一直留在二态批注上。最终验收必须覆盖任务 1–5 全部。

## 接口约定

生命周期事件追加到已有批注日志：

```text
{v:1, op:"task", id, opId, expectedRevision, action,
 attemptId?, requestRevision?, actor:{kind,name?,session?}, payload?, at}
```

`action` 固定为 `dispatch`、`execution`、`submit`、`accept`、`changes`、`reopen`。
`execution` 的 payload 固定包含 `requestId`、`status` 与可选 `reason`；状态值来自
现有回执：starting / queued / running / held / completed / failed / cancelled / interrupted / untracked。

单个任务投影含 `state`、`revision`、`requestRevision`、`attempt`、`delivery`、
`attention`、`history`。`attempt` 记录 id、session、requestId、requestRevision 与
executionStatus。`delivery` 记录 id、attemptId、结果位置 `outputs`、检查说明 `checks`
及未解决事项 `remaining`。这些字段是记录，不能触发执行其中描述的 shell 命令。

`requestRevision` 是请求与引文的 UTF-8 摘要：SHA-256(`quote + U+0000 + text`)，
不包含可变的行号。相同文件里的重复 id 属于输入错误，不替人猜选一条。

事件 `expectedRevision` 只等于当前任务 revision 时才能接纳；已接纳的 opId 再读
得到同一个结果。复用 opId 携带不同内容报冲突。旧 attempt 或旧 requestRevision
不能改变当前阶段。原始拒绝事件留在日志供诊断，不成为新的权威状态。
操作内容比较不包含传输时间 at；同一次操作重试不能仅因时间变化被误判冲突。

顺序折叠从 thread 创建事件建立初始状态，逐条处理已有 edit / resolve / reopen / move
及新 task 事件；不能先取最终 thread 快照，再把旧生命周期事件重复套用。用户改请求
使 requestRevision 改变；仍在运行的旧 attempt 留档，后续提交必须核对新版本。

## 任务 1：同一批注日志里的持久生命周期

**Files:**
- Create: Noema `shared/comment-workflow.mjs`、`shared/comment-workflow.d.mts`、`shared/comment-workflow-fixtures.json`。
- Create: Noema `tests/comment-workflow.test.ts`。
- Create: Emacs `lisp/tools/agent/task-workflow.el`、`test/agent-task-workflow-tests.el`。
- Modify after ownership handoff: Noema `server/lib/note-comments.mjs`、其 `.d.mts`、`tests/note-comments.test.ts`。
- Modify: Emacs `Makefile`，为新纯模型测试加独立 `agent-task-workflow-test` 目标；不把简单模型测试绑到整个配置启动。

**Interfaces:**
- `taskRequestRevision({quote,text}) -> string`：上述 UTF-8 摘要；运行于 Node，fixtures 记录精确结果。
- `initialTaskWorkflow(thread) -> TaskWorkflow`：旧 open 为 todo、旧 resolved 为 done；没有旧人工审阅记录时不补造 reviewedBy。
- `reduceTaskWorkflow(task,event) -> {task,accepted,error?}`：纯增量折叠；TaskWorkflow 的字段如上。
- `transitionThreadTask({file,id,event}) -> {ok,task,accepted,conflict?}`：现有 append 边界写入、重读确认；thread 必须属于传入 file。
- Elisp `(+agent-task-workflow-initial THREAD)`、`(+agent-task-workflow-reduce TASK EVENT)`：同一 alist 输出、同一 fixtures。
- Elisp `(+agent-task-workflow-request-revision QUOTE TEXT)`：与 JS 同摘要。

fixtures 的顶层为数组，每项 `{name,thread,steps:[{event,accepted}],expected}`。
JS 的 `comment-workflow-shared-fixtures` 与 ERT 的 `agent-task-workflow-shared-fixtures`
逐步核对 accepted，最后深比较整个 expected 投影。摘要另用下列精确断言：

```js
test("comment-workflow-utf8-request-revision", () => {
  expect(taskRequestRevision({quote: "假设😊", text: "必要？"})).toBe(
    "bcd7bf86ca8a2b444510196d724000ae4a6fcd7572c1a91573073d33badfbd6a");
});
```

- [ ] **1. 写失败测试。** JS 与 ERT 共享 fixtures，至少固定下列断言：

```text
open -> todo; resolved -> done, reviewedBy absent
dispatch(a1, rev0) -> todo / attempt a1
execution(a1, running, rev1) -> doing
submit(a1, current request, outputs=[proof.md]) -> review
accept(person, current delivery) -> done / thread resolved
accept(agent) -> rejected, state unchanged
changes(person, feedback) -> todo, first delivery retained
late submit(a1) after dispatch(a2) -> rejected
two distinct events at rev3 -> exactly one accepted
same opId and content twice -> one history entry
same opId with different content -> conflict
same opId/content with a new at -> still idempotent
edit request after dispatch -> request-changed; old submit rejected
Unicode quote/text -> exact shared digest
file a + task id from file b -> no such task
```

- [ ] **2. 确认失败。** Noema 运行 `npm test -- tests/comment-workflow.test.ts`；
  Emacs 运行 `make agent-task-workflow-test`；新符号尚未实现，不能因环境加载失败冒充行为失败。
- [ ] **3. 实现上述接口。** 在现有 fold 中保留 task 事件，单次折叠线性处理；完成状态与 thread.status
  从同一事件派生。保留旧事件和未知事件，不重写历史日志。并发写入后以重放接纳结果答复。
  `getThread(file,id)` 本身必须核对 thread.note 与文件身份；现有 reply / resolve / edit / delete / assign
  的公共入口都经同一检查。只修新增 transition 接口会留下已复现的旧入口越界。
  产品代码首次修改前在隔离环境记录 `make state-baseline`；涉及既有界面迁移的宽改动同时记录
  `make test-baseline`。不覆盖其他会话正在使用的共享基线。
- [ ] **4. 验证。** 两个新测试加 `npm test -- tests/note-comments.test.ts`；
  校验 JS / Elisp fixture 全量一致，旧批注创建、编辑、改名和移动的测试仍通过。
- [ ] **5. 提交。** 两仓库分别提交这一模型切片与相应现行契约；不包含对方在途呈现变化。

## 任务 2：准确绑定原生执行，保存确认后才交出

**Files:**
- Modify: `lisp/tools/agent/requests.el`、`context.el`、`collab.el`。
- Create: `lisp/tools/agent/tasks.el`、`test/agent-tasks-tests.el`。
- Modify: `test/agent-delegation-tests.el`、`test/agent-collab-tests.el`、`Makefile`。

**Interfaces:**
- Consumes: 任务 1 的 projection、`transitionThreadTask` 与 requestRevision。
- Extend `(+agent-requests-submit BUFFER CONTENT &optional NOTIFY OPTIONS)`；OPTIONS 是 plist，
  `:capture-output` 默认 t，`:on-change` 收到不含聊天正文的回执快照，`:context` 保存任务身份。
  旧三参数调用保持语义。callback 能读取 `:id`、`:status`、`:reason`、`:native` 与 `:context`。
- Produce `(+agent-tasks-dispatch FILE ID SESSION &optional CALLBACK)`：返回派发操作 id；
  CALLBACK 在确认保存与进入原生队列后收到结果；失败收到具体错误，不报“已交出”。
- Produce `(+agent-tasks-request-changed RECEIPT)`：为对应 task / attempt 追加 execution 事件；不消费聊天 chunk。
- Produce `(+agent-tasks-reconcile FILE)`：读取持久任务，无法证明回执仍存在时标为执行未知，不自动发提示。

- [ ] **1. 写失败测试。** 在现有真实 ACP fixture 上增加断言：

```text
save pending -> requests-submit call count 0
save rejected -> request absent, draft and reason retained
duplicate dispatch click -> same opId and one queue item
session has t1 running and t2 queued -> only t1 doing
t1 end_turn -> no t2 completion; t1 without delivery gets delivery-missing
capture-output nil + message chunks -> receipt text stays empty
default capture-output + message chunks -> existing delegation result unchanged
queue removed / held / edited / client replaced -> matching attention reason
initialization failure before session id -> failed task, no lost request
late callback from a1 after a2 -> current task unchanged
```

- [ ] **2. 确认失败。** 新 ERT 文件与 `agent-delegation-` 子集分别运行，失败必须指向新增行为。
- [ ] **3. 实现接口并接通消费者。** `collab` 的面板派工与 C76 发送走同一 dispatch。
  工具指南仍每个原生会话只附一次。保留原有原生队列，不按文本匹配请求。
  request 的 starting / running 回调可能在 submit 返回前到达，必须通过创建时的 context 关联。
- [ ] **4. 验证。** `make agent-delegation-test` 与定向 ERT `agent-tasks-`、`agent-collab-`；
  确认普通聊天、steer、Research 冻结回合与含输出的已有后台回执没有变更。
- [ ] **5. 提交。** 记录本切片实际运行命令及结果；错误或结束回调不能弹 minibuffer 问题。

## 任务 3：文档交付、人工接受与同一任务重做

**Files:**
- Modify after handoff: Noema `server/Features/Agent/api.mjs`、`server/Features/Comments/api.mjs`、`resources/agent/tools.md`。
- Create: Noema `server/lib/comment-delivery.mjs`、`tests/comment-delivery.test.ts`。
- Modify: Noema `tests/agent-api.test.ts`；Emacs `lisp/tools/agent/tasks.el`、`actions.el`、`collab.el`、`test/agent-tasks-tests.el`。
- Modify: owning `docs/design/agent-management.md`、`docs/noema/design/comments.md`，不把拟议行为写成已通过。

**Interfaces:**
- Consumes: 任务 1 的 revision / attempt 和任务 2 的任务关联。
- Extend existing `agent:document:comment`：`{file,id,action:"submit",attemptId,requestRevision,opId,delivery}`。
  旧 `resolve:true` 请求转为提交审阅；缺少当前 attempt 或交付时返回 `needsDelivery`，不能直接 Done。
  同一公开 Agent 入口拒绝 accept、changes 与伪造的人类 actor。
- `validateCommentDelivery(delivery,{openDocument,readFile}) -> Promise<Delivery>`：outputs 至少一项，
  每项 file 必须在声明的目标范围，保存版本或待审修订可核对；结果文本不从聊天收集。
- `(+agent-tasks-review FILE ID)`：回到结果文档及关联修订；不进入聊天。
- `(+agent-tasks-accept FILE ID)`、`(+agent-tasks-request-changes FILE ID FEEDBACK)`、`(+agent-tasks-reopen FILE ID)`：
  原生用户动作，接受具体 delivery 版本；在公共 Agent API 中没有可伪造的“本人同意”参数。
- 编辑 / 提议工具接受可选 `{taskId,attemptId}` 关联；未带任务的普通修改继续有效，不按会话 busy 猜归属。

- [ ] **1. 写失败测试。**

```text
valid current delivery with 2 outputs -> review; source file remains authoritative
no output / unsaved version / wrong attempt / changed request -> reject, no Done
old completion call without delivery -> needsDelivery
agent accept with actor:{kind:person} -> reject
user accept -> done + one accepted event, no second state store
request changes -> same task todo + saved feedback + retained first delivery
output changed after submit -> review shows stale; accept cannot approve old version silently
pending ediff -> review opens existing card; acceptance remains guarded
direct ACP diff fragment -> marked partial; no invented whole-task rollback
renamed file / non-ASCII anchor / duplicate quote -> correct identity or explicit locate-needed
```

- [ ] **2. 确认失败。** `npm test -- tests/comment-delivery.test.ts tests/agent-api.test.ts` 和对应定向 ERT。
- [ ] **3. 实现接口。** 普通文件沿用 C74 的未保存 buffer 规则；Noema 通过既有 notes 保存，失败时不回退写盘。
  检查说明标为 Agent 报告，不能把描述的检查命令当作 host 已执行的验证。
  用户手改文件仍可能改变完成表示，但不会生成“人工接受本次交付”的记录。
- [ ] **4. 验证。** 两文件输出、原位修订与反馈二次执行都经实际写入路径；保留现有 comment / propose / edit 回归。
- [ ] **5. 提交。** 工具指南、所有调用入口与新完成语义一起更新，避免旧提示仍要求 Agent 直接结案。

## 任务 4：原生多任务侧栏、Agenda 与全局人工介入

**Files:**
- Create: `lisp/tools/agent/tasks-ui.el`、`test/agent-tasks-ui-tests.el`。
- Modify: `tasks.el`、`panel.el`、`sessions.el`、`manager.el`、`actions.el`、`notifications.el`。
- Modify: `test/agent-panel-tests.el` 与相关 manager / notifications ERT。
- Modify: `test/agent-manager-tests.el`、`test/agent-notifications-tests.el`。
- Modify after handoff: Noema `server/lib/agenda-comments.mjs`、`tests/agenda-comments.test.ts`。
- Modify: Noema `lisp/noema-agenda.el`、`tests/elisp/noema-agenda-tests.el`、
  `pages/agenda-view.ts`、`tests/agenda-view.test.ts`。

**Interfaces:**
- `(+agent-tasks-query SCOPE FILTER) -> list`：SCOPE 含明确 root / file / kind，FILTER 含 search / state / assignee / priority；
  返回共享任务 projection，按稳定 task key 去重。不隐式扫描未进入的项目。
- `(+agent-tasks-attention-items &optional ROOT) -> list`：包括没有活会话的 Review 项；
  项含 `:kind 'document-task`、`:file`、`:id`、`:reason`、`:action`，不伪装成 session buffer。
- ETAF `+agent-tasks-page` component 的参数为 `&key model owner layout`，layout 是 sidebar / board；
  两种布局读同一 ref model。挂接现有 H-t 侧窗，模型更新不反复 remount。
- 既有 Inbox 与 `/next` 合并 session attention 与上述 task attention，选择动作按 kind 分派。
- Agenda 批注对象额外返回 `commentState`；普通 todo 状态集不扩展。写回 review 不降为 todo。

- [ ] **1. 写失败测试。**

```text
30 task projections + zero active sessions -> all Review tasks visible and openable
duplicate view membership -> one task and one count
explicit project filter stays pinned while another document gains focus
query matches title / file / assignee / priority and has deterministic order
events update visible row without moving selection, expanded row or scroll
RET -> document anchor; chat only through explicit chat action
Pi / Graph tabs retain their actual buffers and source window ownership
hidden panel -> no view subscriptions or redraw; task execution still persists
100 ACP state events -> coalesced draw, no whole-project rescan per event
Agenda review -> read/write/read remains review; Done uses user acceptance
Inbox includes task without live session; same waiting event produces one notification
TTY and GUI both dispatch, locate, review and accept
```

- [ ] **2. 确认失败。** 定向 ERT 与 `npm test -- tests/agenda-comments.test.ts`；界面测试使用实际 ETAF 挂载。
- [ ] **3. 实现接口及适配。** 复用已有 scoped Agenda / 批注索引，文件变化只使相应项失效；
  已有目录日志清单可缓存，不增加后台扫描器。状态更新必须与视图订阅生命周期分开。
  窄侧栏按“需要我、Review、In Progress、Todo”分组，Done 折叠；展开视图可按四阶段列。
- [ ] **4. 验证。** 原生窗口中核对主文档焦点、q / H-t 收起、Pi 与 Graph 往返、筛选与稳定选择。
  Web Agenda 与原生 Agenda 验证同一实际批注，不能只检验字段名存在。
- [ ] **5. 提交。** 更新日用入口、窗口归属及相应契约；没有逐 token 重绘或按 provider 定制 UI。

## 任务 5：完整价值流验收、恢复与实施交接

**Files:**
- Create: `test/agent-document-workflow-e2e.el`、Noema `tests/comment-workflow-integration.test.ts`。
- Create: `docs/works/agent-document-workspace/verification.md`，记录真实命令、输入、输出与未验证限制。
- Modify: `Makefile` 加 `agent-document-workflow-test` 目标，聚合本功能的定向 suite；
  `docs/README.md` 在记录文件出现前添加入口。
- Modify: 当前设计、日用说明与本工作台状态；完成后按仓库规则归档工作记录。

**Interfaces:**
- Consumes: 任务 1–4 的真实生产接口与现有 ACP fixture，不在测试里绕过派工、保存与 review 写入。
- `make agent-document-workflow-test`：运行纯模型、回执、任务、原生 UI 与完整文档场景的定向检查。
- `make research-test`、`make agenda-test`、Noema 定向 suite 与 `make noema-web-build`：按最后实际修改范围执行。

- [ ] **1. 建立失败的整体场景。** 真实临时 `proof.md` 的 Lemma 下创建批注，两个会话串行 / 并行处理
  30 项任务中的选定项：保存、派工、原文写回、交付 Review、用户要求修改、再次交付、接受 Done。
  两个输出文件与一个 ediff 卡片实际生成；侧栏 / Agenda / Inbox 指向同一 task id。
- [ ] **2. 验证故障路径。** 覆盖保存拒绝、队列改动、取消、client 替换、旧 attempt 回调、用户修改请求、
  同 revision 并发事件和重复操作。崩溃重启后从真实日志恢复，不能靠测试内存假装已持久化。
- [ ] **3. 验证原生交互。** GUI 与 TTY 完成发起、定位、审阅和接受；检查隐藏侧栏后仍持久化。
  使用 30 项任务和 100 次事件记录扫描 / 重绘次数与墙钟耗时，不以空列表演示代替规模验收。
- [ ] **4. 验证路径与能力。** 原生路径和 `/fs:local:` 指向相同任务；远程 fixture 缺少 G14 的
  API 转发能力时明确拒绝，不能访问客户端同名文件。真实 SSH 只在已有可用目标上实测，未实测的
  范围记为未验证，不能声称 G14 已解决。
- [ ] **5. 运行适用的最终检查。** 上述定向检查通过后跑实际受影响的聚合目标、`check-parens`、
  `git diff --check`；渲染改动完成构建后，再在运行中的 Emacs 页面验证。
  Noema 仓库要求的 `make test`、`make build`、`make install` 与
  `go test -tags fts5 ./...`（`kernel/`）也属于最终检查；Emacs 侧执行
  `make research-test`、`make agenda-test`、`make jupyter-test`。
  最后运行 `make state-check`；每项预期的新增状态、hook 和键位差异都有解释。宽改动运行
  `make test-check`，将既有失败与本次新增失败分开记录。
- [ ] **6. 对照 Spec §10 逐项复核并提交证据。** 有缺项即保持任务未完成；通过后才在现行设计中
  关闭对应缺口。产品设计、代码与交付记录都要能从任务恢复，不把本计划的勾选视作功能证据。

## Spec 覆盖与执行安排

| 设计要求 | 实现与证据归属 |
| --- | --- |
| 文档优先、同一 task / attempt / delivery 身份 | 任务 1、2、3 |
| 保存确认、原生队列、反馈 steer / queue、生命周期 | 任务 2，既有 context / ACP 回归 |
| 原处结果、逐处修订、人工审阅与重做 | 任务 3 |
| 多任务原生侧栏、全屏总览、Pi / Graph、范围筛选 | 任务 4 |
| Agenda / Inbox、无会话待审、通知与可见性性能 | 任务 4、5 |
| 并发、持久化、未知执行恢复、路径与能力边界 | 任务 1、2、5 |
| 一次完整科研任务，及设计的十项验收 | 任务 5；任一未通过都不能宣称整体完成 |

建议在本会话原生执行，先完成共同接口，再按以上顺序集成；任务 1–4 的接口依赖
较紧，持续保留当前上下文有助于核对。与 Claude 的既有重构按文件归属交接，不要求
用户搬运消息或合并相互覆盖的代码。每个可独立验证的切片分别提交，两仓库独立记录。

实施前须审阅本计划；仍需确定的只是执行方法与是否认可这些具体接口，不重问已经
明确的文档中心哲学、技术栈和最终闭环。实施不能停在纯状态模型或一个看起来能用的侧栏。
