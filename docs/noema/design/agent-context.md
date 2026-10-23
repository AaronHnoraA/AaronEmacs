# Noema Agent 上下文管理

## 范围

本文是 Noema 向 Agent 交付上下文的设计归属：普通 ACP 会话的编辑器引用、研究 Run 的冻结输入、自动上下文预算、会话重建、记忆与可核查回执。用户操作见 [研究工作流](../guide/research-workflow.md)；`@@ctx` 的合法语法见 [syntax.md](syntax.md)；会话命名和原生 Codex 进程生命周期见 [codex-session-lifecycle.md](codex-session-lifecycle.md)；Skill/MCP 的定义与选择见 [capabilities.md](capabilities.md)；Hy Memory 的后端配置见 [hy-memory.md](hy-memory.md)。

## 模型

| 层次 | 内容及权威 | 生命周期 |
| --- | --- | --- |
| 原生会话 | agent-shell/ACP 和 Agent 持有对话历史；Noema 记录项目内的逻辑 Session 名与原生 Session ID | 随原生会话恢复、分支或结束 |
| 普通交互附加上下文 | gptel 的当前选择经 `noema-context.el` 变成路径、行号与 ACP `resource_link`；Agent 按引用读取文件 | 本次提问 |
| 研究 Run 输入 | Node host 将 prompt、上下文内容、能力与会话路由冻结；RunSpec 记录上下文清单、摘要及选择原因 | 随 Run 持久保存 |
| 自动记忆 | 已审核 Finding 与未经核实的 Hy Memory 各自召回；两者都受项目边界限制 | 下次提问或 Run 准备时重新选择 |

Noema 的 64 KiB 限额只约束它附加到**研究 Run** 的上下文字节，不代表 Agent 的模型窗口大小。继续一个原生会话时，Agent 原有的对话历史仍在；Noema 的 RunSpec 只描述 Noema 本次选择并发送的内容。ACP 适配器或 Agent 自行加入的上下文不在回执的可观测范围内。

## 契约

**C1｜普通交互使用引用。** `noema-context.el` 复用 `gptel-context` 中的选区、buffer 与文件选择。可保存的文件以一行路径或 `path:START-END` 进入 prompt，每个不同文件附一个 ACP `resource_link`；此路径不内嵌文件正文。未保存或不可读的选择会报告并跳过。会话忙时，agent-shell 的队列保留文本路径引用，下一轮再送。

**C2｜研究 Run 先冻结再派发。** `.noema` 工作块先保存，再只解析开头控制区的 `@@agent`、`@@session`、`@@ctx`、`@@skill` 与 `@@pack`。Node host 在 `run:prepare` 中决定会话、解析引用、选择能力并冻结 RunSpec 和 context items；Emacs worker 只消费这份冻结输入。控制区以后的 `@@...` 是 prompt 数据。`local_only` 的 cell 或 note 不得被注入 Run。

**C3｜显式引用完整保留。** `@@ctx(cell:...)`、`result:...`、`file:...`、`note:...`、`artifact:...`、`git.diff`、`handoff.latest`、`lineage[:1-3]` 与 `depends` 按各自来源解析。相同内容去重。显式声明的内容不自动截短；显式项超过 64 KiB 就拒绝准备，并要求缩小引用。`@@ctx(none)` 关闭自动上下文，保留同一工作块的其他显式引用、项目说明和已选择的能力。

**C4｜自动来源有明确边界。** 直线 `lineage` 的父 Session 仍以父节点为最新工作、使用同一 Agent 且可续接时，默认续接该原生会话；`continued` 路由不再自动加入项目说明或重贴 DAG 上游，已有对话历史由 Agent 保留。显式 `@@ctx`、选定的能力及匹配记忆仍按本 Run 发送。新建或重建的工作会话收到项目与执行目录说明，并先接收直接父 WorkNode 的最新输出，再接收上游源块；输出若来自未完成 Run，必须标成部分或未核实。`@@session(fresh)` 仍可得到其 DAG 上游。`depends` 边只决定顺序；只有显式 `@@ctx(depends)` 或 `result:<work-node-id>` 才带入依赖输出。重建的 fork 可从父 Run 输出和最新持久 Handoff 取得自动上下文；它不声称继承了父会话的隐藏对话历史。

**C5｜预算先保必要项。** 显式 `@@ctx`、项目说明、已选择的 pack 前导语与 Skill 内容占用 64 KiB 预算后，自动项才分配余量。自动项优先尝试完整保留；DAG 合流时，多条直接父输出各预留最多 1 KiB 的有效片段，短输出按实际大小计，空间不足时尽可能覆盖更多父节点，同量按文档顺序。其余自动项可截取或省略，原因与原始字节数写入 RunSpec。截取自动项至少须有 1 KiB 可用空间。

**C6｜截取确定且可追查。** 父输出与 Handoff 保留开头和结尾；片段预算达到 4 KiB，且中段的相关段落覆盖本次请求至少四分之一时，也按文档顺序保留相关中段。匹配词按其在各段中的稀有程度加权；没有有效匹配时只保留首尾。截取算法只依赖原文字节、prompt 和预算，不交给模型决定。达到 4 KiB 的截断标记还在 160 字节内说明跳过和接回的章节；标题跟随正文，围栏代码块整体保留或舍弃，重复及相邻片段会合并。其他自动引用只保留开头。

标记说明省略的字节数；只有 Run 配备 `noema-research` 工具时才给出读取原文的调用。整体省略的可读项汇总为一个同样计入预算的 `context-omitted` 项。再次读取 cell 得到的是当时的当前内容，可能已不同于冻结版本。

**C7｜记忆分开标识。** Run 可在同一 workstream 内召回最多三个非 `local_only`、有证据跨度、状态为 `supported` 或 `accepted` 且验证级别为 `reproduced`、`human_reviewed` 或 `formally_verified` 的 Finding；所选片段先受 3,600 字节预算约束。查询最多取 32 个不同词；长 prompt 保留前 16 和后 16 个，兼顾汉字二元组与拉丁标识符。每条 Finding 至少需匹配三个词、达到其加权词汇的一半；常见词权重低。与已选 Finding 的词重合达到 80% 的改写不再占槽位。弱匹配不召回。

Hy Memory 使用同一项目根隔离普通 ACP 对话与 Run，但召回项标记为未经核实的会话背景，不成为 Finding 或研究证据。普通会话在提问前召回、完成后异步记忆；Run 在冻结时召回、完成后异步记忆。`@@ctx(none)` 关闭 Run 的两种自动召回；召回故障不阻止执行。将 Handoff 的具体结论转为可召回 Finding，仍须先提出带精确证据跨度的 Proposal，再由人审核。

**C8｜ACP 发送保持冻结内容。** Worker 使用 RunSpec 的 prompt、context items 和 MCP server 配置，不在派发时重新扫描 Skill 或重读文件。适配器支持嵌入上下文时，先发送 prompt 文本块，再按冻结顺序发送每项 ACP `resource`；否则按相同顺序拼接进文本 prompt。RunSpec 的 `created_at`、摘要及回执元数据不进入这些 prompt 块。普通交互的 `resource_link` 与研究 Run 的冻结 `resource` 是两条不同的交付路径。

**C9｜窗口压力只依据新鲜报告。** ACP 报告累计 Session context 使用量与窗口后，后续 Run 可按 `noema-agent-worker-context-rollover-ratio` 判断是否换代；默认阈值为 85%，有效配置范围为 50%–99%。没有可靠报告时状态为 unavailable，不推断为 0%，也不自动换代。达到阈值的下一次 Run 从最新持久 Handoff 重建；预览只显示判断，不发起换代。Run 结束时只把本次开始后收到的累计 Session 用量快照写入回执；无新报告则清除用于未来判断的旧读数。

**C10｜回执只报告已选择的事实。** `C-c j p` 预览本次路由、上下文引用、字节数、截取与省略以及窗口压力。`C-c j i` 的 Inspector 从不可变 RunSpec 读取已完成或 `preparing` Run 的输入、摘要与用量快照；`preparing` 表示已冻结但尚未派发。用量是带时间戳的累计 Session 快照，不是本 Run 的 token 消耗；未报告时显示 unavailable，不借用 Session 后来的读数。

**Upstream now** 只比较当前已保存 `.noema` 文件与冻结的 `result:` 输出，分别报告未变、同 Run 内容已变、由另一个 Run 覆盖、缺失或无法核实。截断输出同时冻结完整来源的摘要，老版本缺摘要时可能无法核实。比较不修改旧 RunSpec 或 WorkNode 状态，也不包含编辑器中尚未保存的改动。Agent 可通过 `research_run {action: "context"}` 读取有界回执；`research_cell {action: "outline"}` 只给直接相邻节点的状态、Run ID、大小与摘要，需要全文时再用 `read` 或 `neighbors`。`local_only` 邻居不进入返回值。

## 实现位置

| 职责 | 实现 |
| --- | --- |
| 普通会话文件引用与选择 | `lisp/noema-context.el`、`lisp/noema-agent-acp.el` |
| 普通会话的隐式记忆上下文 | `lisp/noema-hy-memory.el`、`server/lib/hy-memory-bridge.mjs` |
| 工作块控制区与会话路由 | `server/lib/research-directives.mjs`、`server/lib/research-session-routing.mjs` |
| 引用解析、预算、冻结、预览与换代 | `server/lib/research-runtime.mjs`、`server/lib/research-context-excerpt.mjs` |
| Finding 召回 | `server/lib/research-memory.mjs` |
| ACP 派发、用量与终态 | `lisp/noema-agent-worker.el`、`lisp/noema-agent-acp.el` |
| JuText 入口、预览与 Inspector | `lisp/noema-research-mode.el` |

## 已知差距

**G1｜跨独立 Run 的稳定前缀缺失。** 研究 Run 的 ACP 内容从会变化的工作 prompt 开始，再接项目说明、显式引用、DAG/Handoff、pack/Skill、Finding 和 Hy Memory。对于按完整前缀匹配的模型缓存，变化发生在前面时，后面未变的 Skill 或资料也无法作为同一个可复用前缀。原生会话续接可能复用此前未改变的对话历史；这与两个新 Run 共享静态内容是不同情况。改变上下文顺序或交付形式会影响 Agent 所见的语义，不能只凭字节顺序推断实际收益。

**G2｜缺少 Run 级缓存效果回执。** ACP 若报告 cached read/write，`noema-agent-acp-usage` 可读这两个计数，Agent ABTOP 也可显示它们；kernel 保存的是累计 Session `cachedTokens` 合计。Run Inspector 目前只显示累计上下文窗口及 input/output，没有同一次 Run 的 cache read、write、延迟和总输入成本对照。ACP 适配器或 Agent 自行加入的上下文仍不在 Noema 的 RunSpec 可观测范围内。

## 待定设计

**Q1｜哪些内容应成为稳定且实际可缓存的前缀？** 候选是项目说明、不会随 Run 改变的 Skill/pack 和工具定义；工作 prompt、上游输出及召回记忆随 Run 改变。需要先确认各 ACP 适配器如何把内容块渲染给模型、是否支持可复用的消息边界或显式缓存断点，并继续保证 RunSpec 精确记录本次发送内容。仅将静态项移到同一条动态 user 消息前面，未必形成提供商会缓存的断点。

**Q2｜用什么数据决定优化？** 在相同模型、工具和项目下比较独立 Run、续接会话、fork/换代以及自动记忆开关的报告值：每轮输入、cached read/write、延迟、费用和实际纳入的上下文。ACP 不报告的字段应保持 unavailable。确定收益和审计语义后，再修改本文契约与实现。
