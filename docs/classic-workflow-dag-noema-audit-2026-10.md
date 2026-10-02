# Noema 的执行契约已经很强，经典 DAG 项目补上的是收敛诊断（2026-10-02）

## 结论摘要

- Airflow、Snakemake、Luigi 共同证明，一套可靠 DAG 不能只保存节点和边；它还要解释一个节点为何可运行、为何被阻塞、输出是否完整、输入变化是否让旧结果失效，以及整张图是否已经无法继续推进。
- Noema 的 Job 层已经具备冻结的 Invocation、同 workstream 依赖校验、claim 时依赖门禁、worker/能力/预算约束和 CompletionCondition。它不需要照搬一个新的工作流引擎。
- Noema 最值得内化的是三种只读投影：依赖收敛原因、输出完整性、结果陈旧原因。这些投影应帮助人判断，不应自动改写 WorkNode 状态或重跑研究任务。
- 本轮确认并修复了一个长期运行 bug：Orchestration Snapshot 原先读取全项目最早 1000 条事件后再筛 workstream。事件历史超过 1000 条时，面板会遗漏最新事件，也会被其他 workstream 挤占额度。现在内核会先按 workstream 筛选，再返回最近 1000 条，展示顺序仍为从旧到新。

## 审计范围与版本

本轮固定源码版本为：

- [Apache Airflow a3e0715](https://github.com/apache/airflow/tree/a3e07159a0bf986b9e5ac1dab3730c7a10cf5762)
- [Snakemake 5bf5e21](https://github.com/snakemake/snakemake/tree/5bf5e21b0d0e7e65e247c1f2c61c1a4f74a1a752)
- [Luigi 715f65c](https://github.com/spotify/luigi/tree/715f65c4a56a908ef0a1df4df6fc33b8420e2e6c)

对照对象为当前 Noema 工作树中的 WorkNode、Task、Job、Invocation、Run、Artifact/CAS 和事件投影。对三个上游项目做静态源码审计，并用其官方文档交叉核对语义；没有运行上游项目的端到端测试。

## 三个经典项目分别解决了什么

| 项目 | 它最成熟的部分 | 对 Noema 的价值 | 不应照搬的部分 |
| --- | --- | --- | --- |
| Airflow | Task Instance 状态、Trigger Rule、整图终态和无可运行节点的 deadlock 判断 | 给 Job/Task Board 增加可解释的收敛状态 | 用运算 DAG 的叶节点终态决定人工研究结论 |
| Snakemake | 输出 incomplete 标记、输入/代码/参数/软件环境变化原因、输出校验 | 给产生文件副作用的 Invocation 标记输出完整性，并报告陈旧原因 | 因文件变化自动重跑或自动改变 WorkNode 状态 |
| Luigi | `requires`/`output`/`complete` 的简洁契约、运行前重新验证依赖、动态依赖 | 保持依赖门禁简单，并在真正执行前重新验证 | 默认把“run 返回”当 DONE，以及让 Agent 运行时动态改写研究图 |

## Airflow：图的状态必须能解释“为什么现在走不下去”

Airflow 区分 DAG 中的 Task 定义和一次 DagRun 中的 Task Instance。Task Instance 不只有 queued/running/success/failed，还包括 scheduled、up_for_retry、upstream_failed、deferred、awaiting_input 等状态。其价值在于状态描述阻塞原因，而不只是描述“还没完成”。

在 [DagRun.update_state](https://github.com/apache/airflow/blob/a3e07159a0bf986b9e5ac1dab3730c7a10cf5762/airflow-core/src/airflow/models/dagrun.py#L1251-L1412) 中，调度器会先计算可调度、已变化、已完成和未完成 Task Instance；如果仍有未完成任务，但没有任何任务可运行，它会把整次 DagRun 判为 deadlock failure。这个机制适合 Noema 的 Job 平面：Board 可以派生 `ready`、`running`、`waiting_on_dependencies`、`blocked_by_failed_dependency`、`awaiting_input`、`exhausted`、`deadlocked`，并列出导致判断的 Job ID。

Airflow 同时提供了一个反例。[`_tis_for_dagrun_state`](https://github.com/apache/airflow/blob/a3e07159a0bf986b9e5ac1dab3730c7a10cf5762/airflow-core/src/airflow/models/dagrun.py#L1168-L1192) 主要根据有效叶节点决定 DagRun 终态。官方文档也提醒：若叶节点采用 `all_done`，它可能在上游失败后成功，最终使整个 DagRun 显示成功。Noema 不应让“清理节点成功”覆盖中间失败，也不应从 Job 叶节点推导 WorkNode 的研究结论已经成立。Task/Job 收敛和 WorkNode 结论验收应保持两个层次。

Airflow 的 [Trigger Rule 依赖检查](https://github.com/apache/airflow/blob/a3e07159a0bf986b9e5ac1dab3730c7a10cf5762/airflow-core/src/airflow/ti_deps/deps/trigger_rule_dep.py) 统计上游 success、failed、upstream_failed、skipped 和 removed，再决定下游是否可执行。Noema 可以吸收这种“原因向量”，但不需要把 `all_success`、`one_failed` 等整套运算语义塞进 WorkNode 边。Job 依赖保持“全部完成才可 claim”更容易审计。

## Snakemake：成功之前，输出首先是“不完整的”

Snakemake 的 [Persistence.started / finished / incomplete](https://github.com/snakemake/snakemake/blob/5bf5e21b0d0e7e65e247c1f2c61c1a4f74a1a752/src/snakemake/persistence/__init__.py#L385-L470) 形成了一个稳健的副作用协议：

1. Job 开始时，先给每个预期输出写 incomplete marker。
2. Job 成功时，记录 rule、代码、输入、日志、参数、shell command、输入 checksum、软件栈和起止时间。
3. 元数据写完后才清除 incomplete marker。
4. 若进程中断，已经出现的输出文件仍会被识别为 incomplete，不能仅因“文件存在”而当成成功结果。

Noema 的 Invocation 已冻结输入 Artifact、ContextSnapshot、DisclosureView、worker、runtime、资源、ProblemModelVersion、policy hash 和 idempotency key；完成 Job 时也验证 CompletionCondition 和输出 Artifact 是否存在。这比只检查文件存在更严格。缺口出现在工作区副作用：若 Agent/工具在失败前写了文件，当前 UI 需要明确区分 `captured_complete`、`partial_outputs_present`、`capture_missing` 和 `no_declared_outputs`。这个状态属于 Invocation/Artifact 捕获质量，不能自动改变 WorkNode。

Snakemake 的 [`update_needrun`](https://github.com/snakemake/snakemake/blob/5bf5e21b0d0e7e65e247c1f2c61c1a4f74a1a752/src/snakemake/dag.py#L1339-L1520) 不只返回一个布尔值，而是保存需要重跑的原因：forced、missing output、updated input、unfinished queue input、missing/outdated metadata、code changed、params changed、input set changed、software environment changed。对 Noema 来说，对应物应是 `staleness reasons` 报告：

- 父 Work 输出摘要变化；
- 引用的 Source/Artifact 缺失或摘要变化；
- prompt/RunSpec 变化；
- capability 或 runtime 环境变化；
- 输出捕获不完整；
- 当前版本无法验证。

这些原因只回答“旧结果基于什么，现在什么变了”。遵守 Noema 的现有边界：SourceChanges 和新的父输出诊断只报告，不自动重跑，也不自动把 WorkNode 设为 `regressed`。

## Luigi：简单完成契约的优点和陷阱

Luigi 的 [`Task.requires`](https://github.com/spotify/luigi/blob/715f65c4a56a908ef0a1df4df6fc33b8420e2e6c/luigi/task.py#L628-L644) 描述依赖，默认 [`Task.complete`](https://github.com/spotify/luigi/blob/715f65c4a56a908ef0a1df4df6fc33b8420e2e6c/luigi/task.py#L590-L605) 只检查所有 `output()` 是否存在。Worker 默认会在执行前重新检查未满足依赖，这一点与 Noema 在 `ClaimJob` 中重新检查所有依赖 Job 是否 completed 相同。

Luigi 也暴露出一个常见陷阱。在 [worker 执行路径](https://github.com/spotify/luigi/blob/715f65c4a56a908ef0a1df4df6fc33b8420e2e6c/luigi/worker.py#L190-L228) 中，`check_complete_on_run` 默认是 false；普通 Task 的 `run()` 返回后，Worker 可以直接标为 DONE，而不再次调用 `complete()`。Noema 当前要求 completed Job 满足冻结的 CompletionCondition，且引用的输出 Artifact 必须存在，这一部分不应退回 Luigi 的宽松默认值。

Luigi 允许生成器式 `run()` 在运行中 yield 新依赖。这适合数据管道的动态发现，但不适合 Agent 直接修改 Noema 的研究结构。Agent 发现新任务时，应继续创建 Proposal，由人一次性接受结构变化；Job 运行时动态出现的外部前置条件可以记录为 unresolved/blocked reason，不能偷偷向 WorkNode 图加边。

## Noema 现状核对

### 已经具备，不必重复实现

- [Task 创建](../site-lisp/noema/kernel/noema/research/orchestration.go) 要求依赖 Task 已存在且属于同一 workstream。依赖只能从已有 Task 指向新 Task，因此创建路径不会形成回边；WorkNode 图本身另有显式环检测。
- Job 创建要求依赖 Job 已存在且属于同一 workstream；Job claim 会在事务里重新确认全部依赖均 completed，并校验 attempt、worker 状态、执行模式、能力、预算和输入 Artifact。
- Invocation 冻结执行事实，completed Job 需要满足 CompletionCondition。Noema 在这一点上已经强于 Luigi 的默认完成模型。
- WorkNode 是人控制的研究文档，SourceChanges 只报告。这个设计比把研究结论变成自动构建缓存更适合 Noema。

### 本轮修复：事件窗口取错方向且取错范围

原实现位于 [research-runtime.mjs](../site-lisp/noema/server/lib/research-runtime.mjs)：`orchestrationSnapshot` 调用 `events({after: 0, limit: 1000})`，得到全项目最早 1000 条事件，再在 Node 层按 workstream 过滤。后果有两个：

- 第 1001 条以后的新事件永远不会出现在 Snapshot；
- 其他 workstream 的旧事件会占用 1000 条额度。

修复跨越四层：

- [store.go](../site-lisp/noema/kernel/noema/research/store.go) 新增 `RecentEvents`，在 SQL 中先按 notebook/workstream 过滤，再按 seq 倒序截取最近窗口，最后恢复时间正序；同时增加 `(workstream_id, seq)` 索引；
- [noema_research.go](../site-lisp/noema/kernel/api/noema_research.go) 的 events API 接受 `workstreamId` 与 `latest`；
- [kernel-research-provider.mjs](../site-lisp/noema/server/lib/kernel-research-provider.mjs) 透传新参数；
- `orchestrationSnapshot` 请求当前 workstream 的最近 1000 条事件，同时保留 Node 端防御性过滤。

相关测试覆盖“先筛 workstream 再取最近 N 条”、API 路由和 Snapshot 的请求契约。

## 建议实现顺序

### P1：依赖收敛投影

在 Task/Job Board 增加纯派生状态和原因列表：

| 派生状态 | 可证明条件 |
| --- | --- |
| `ready` | queued，依赖 completed，存在满足要求的可用 worker |
| `waiting_on_dependencies` | 至少一个依赖仍为 queued/claimed/running |
| `blocked_by_failed_dependency` | 至少一个依赖 failed/cancelled/exhausted，且当前没有合法恢复路径 |
| `awaiting_input` | 当前 Invocation 有未解决的人类 input/permission request |
| `exhausted` | attempts_started 达到 attempts_max，且未完成 |
| `deadlocked` | workstream 仍有非终态 Job，但没有 ready/running/awaiting_input，也没有可自动发生的状态转换 |

这只是解释现有权威记录。不要持久化第二套状态，也不要据此自动关闭 Task。

### P1：Invocation 输出完整性

对声明会修改工作区或产生 Artifact 的 Job，在 Invocation 开始时记录预期输出/捕获范围；终态时记录 capture completion。失败或失联后的文件可作为 partial evidence 查看，但不能被展示成已验证输出。这个机制与本轮已经修复的“失败 Run 输出/Handoff 必须带未验证警告”一致。

### P1：结果陈旧原因

复用 CAS、RunSpec context manifest、ArtifactLink 和 SourceChanges，比较旧 Run 冻结时的输入与当前输入。先实现父 Work 输出摘要、Artifact 缺失和 SourceChanges 三类原因。结果展示为报告，重新运行仍由人触发。

### P2：完成条件检查器

为常见 Job kind 提供可复用的 CompletionCondition：命令退出码、测试摘要、Artifact media type、JSON schema、非空文件、checksum、工作区捕获完成。条件在 Job 创建/claim 时冻结，在完成时统一解释，Inspector 显示每项通过或失败的证据。

## 明确不吸收

- 不从叶 Job 成功推导整个 WorkNode DAG 的研究结论成功。
- 不因源文件、父输出或环境变化自动改写 WorkNode 状态。
- 不让 Agent 在运行时直接添加 WorkNode/Task 边；结构变化继续走 Proposal。
- 不把“输出文件存在”当成完成，也不把“Agent 进程退出 0”单独当成研究验收。
- 不复制 Airflow 的完整 Trigger Rule 语言到研究文档。

## 验证

- `go test ./noema/research ./api`：通过。
- `npm test -- tests/research-runtime.test.ts`：52/52 通过，使用项目规定的 Node 26.5.0 与 npm 11.17.0。
- `go test -tags fts5 ./...`、`make build`、`make install`、AaronEmacs `make research-test` 和 `make jupyter-test`：通过。
- 完整 `make test` 共运行 2,858 个测试，2,840 通过、16 跳过、2 失败。失败均为工作区已有 CM6 粗体二次切换改动：多行列表和软换行段落第二次执行 bold 未移除标记；与本轮 research event 路径无交集。
- 上游三项目仅做固定 commit 的静态源码审计；上游端到端行为未在本机复现。
