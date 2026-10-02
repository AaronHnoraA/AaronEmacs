# LangGraph / OpenHands Agent SDK / Letta Code → Noema 深入审计（2026-10-02）

## 范围

继续研究 Noema 的研究 DAG、Agent 组织和上下文继承。固定源码版本为 [LangGraph 157a06d](https://github.com/langchain-ai/langgraph/tree/157a06dda988d85afeb8751ff27b35ab3f4f8bf4)、[OpenHands Agent SDK 2414d6e](https://github.com/OpenHands/software-agent-sdk/tree/2414d6ee5e31bede2e78211f72b58e9949575a75)、[Letta Code 1fcc966](https://github.com/letta-ai/letta-code/tree/1fcc9666817ab852bc2532a3a989f712e1fd6c19)。对照当前 Noema 工作树。以下为相关执行和上下文路径的静态代码审计；没有运行三个上游项目的端到端测试。

## 逐项目核对

| 项目 | 源码中的机制 | Noema 已有机制 | 可吸收的部分 |
| --- | --- | --- | --- |
| LangGraph | [Checkpoint](https://github.com/langchain-ai/langgraph/blob/157a06dda988d85afeb8751ff27b35ab3f4f8bf4/libs/checkpoint/langgraph/checkpoint/base/__init__.py#L39-L148) 保存 channel values、各 channel 版本、各节点已看过的版本、来源和父 checkpoint；[apply_writes](https://github.com/langchain-ai/langgraph/blob/157a06dda988d85afeb8751ff27b35ab3f4f8bf4/libs/langgraph/langgraph/pregel/_algo.py#L235-L323) 按确定顺序应用一次执行的写入，再更新版本。[待写入项](https://github.com/langchain-ai/langgraph/blob/157a06dda988d85afeb8751ff27b35ab3f4f8bf4/libs/langgraph/langgraph/pregel/_loop.py#L510-L539) 按 Task ID 进入 checkpointer。 | Noema 的 WorkNode 图是研究意图，不是自动执行图；Job 有依赖、Invocation 和 lease。RunSpec 的上下文项已有来源 URI 与内容摘要，并冻结到 CAS。 | 对研究图做只读的“消费版本”诊断：比较下游 Run 冻结时的父输出摘要与当前父输出摘要，报告 changed、missing 或 unverifiable。保留 WorkNode 人工状态，不据摘要自动重跑或改成 regressed。 |
| OpenHands SDK | [EventLog](https://github.com/OpenHands/software-agent-sdk/blob/2414d6ee5e31bede2e78211f72b58e9949575a75/openhands-sdk/openhands/sdk/conversation/event_store.py#L25-L222) 持久追加事件，写时验证事件 ID 与父事件；旧线性事件按序回退为祖先链。[Condensation](https://github.com/OpenHands/software-agent-sdk/blob/2414d6ee5e31bede2e78211f72b58e9949575a75/openhands-sdk/openhands/sdk/event/condenser.py#L12-L107) 记录被折叠的事件 ID 与摘要；[View](https://github.com/OpenHands/software-agent-sdk/blob/2414d6ee5e31bede2e78211f72b58e9949575a75/openhands-sdk/openhands/sdk/context/view/view.py#L18-L151) 是派生的模型输入，压缩后校验工具调用与观察结果的配对、唯一性及批次原子性。 | Noema 保留原生 ACP 会话，Run 事件和 CAS 是研究证据；路由和 Handoff 负责重建 fork。ACP transcript 与权限回调由 agent-shell 边界负责。 | 给重建上下文和压缩说明明确“原始记录、派生视图、模型实际收到的内容”三种身份。测试恢复后的 tool call/result 和权限答复闭环，尤其重复回包、晚到观察结果。无需在 Noema 另造 ACP transcript 存储。 |
| Letta Code | [forkParentConversation](https://github.com/letta-ai/letta-code/blob/1fcc9666817ab852bc2532a3a989f712e1fd6c19/src/agent/subagents/fork-conversation.ts#L49-L105) 在 fork 前校验模型，fork 后配置工具集，配置失败清理隐藏子会话。[memory-handoff](https://github.com/letta-ai/letta-code/blob/1fcc9666817ab852bc2532a3a989f712e1fd6c19/src/agent/subagents/memory-handoff.ts#L9-L89) 给新 Worker 自足任务说明，父会话记录只作为按需读取的只读引用。[memory-worktree](https://github.com/letta-ai/letta-code/blob/1fcc9666817ab852bc2532a3a989f712e1fd6c19/src/agent/memory-worktree.ts#L385-L651) 隔离记忆编辑并区分未提交、父目录脏、合并冲突和成功。 | Noema 会话经 lineage 路由，显式上下文、父输出、Handoff 和经人工审阅的 Finding 被冻结。Finding 候选经 Proposal 审阅；Agent 不能直接把聊天内容升级为研究事实。 | 子 Agent 任务说明可明确目标、可读来源、编辑边界和验收；父会话仅在任务确需时提供引用。代码 Agent 的可选工作树可以借鉴配置失败清理及冲突状态。Noema 的研究记忆仍由人工审核的证据链管理。 |

### 两个容易混淆的边界

1. **版本变化不等于研究结论撤回。** LangGraph 的 versions_seen 控制自己的自动图执行；Noema [SourceChanges](../site-lisp/noema/kernel/noema/research/capture.go) 已报告文件证据变化，且 [WorkNode 校验](../site-lisp/noema/server/lib/research-notebook.mjs) 会警告部分不一致状态。父 Work 输出的摘要比较应同样只生成提示，供人判断是否需要重跑或调整研究状态。
2. **压缩视图不等于完整经历。** OpenHands 把压缩事件和完整事件日志分离；Letta 也让新记忆 Worker 按需读取父记录。Noema [RunSpec 与 context manifest](../site-lisp/noema/server/lib/research-runtime.mjs) 已冻结显式内容；Inspector 应展示内容来自原生会话、重建 Handoff 还是父输出，以及自动上下文是否截断/遗漏。不得把重建摘要说成完整继承了父 Agent 的隐含状态。

## 建议实现顺序

1. **P1：父输出失效诊断。** 只读查询一个 Run 的冻结 context manifest，提取 result: 引用、来源 Run ID 和摘要；与当前 WorkNode 最新输出比较，返回 unchanged、changed、missing、unverifiable。这里沿用现有 CAS 和 WorkNode 索引，不另建研究 DAG 状态机。状态变化仍通过现有文档编辑/Proposal 路径。
2. **P1：恢复来源展示。** 在 Inspector 的 Run 详情列出 fork 模式、parent session/Run、Handoff 来源终态、截断/遗漏和缺失附件等已经可得的事实。新的“恢复降级”字段只记录可证明的缺失，不推断 Agent 的主观记忆。
3. **P2：Agent 任务契约。** 在未来真正启动受监督的代码子 Agent 时，要求任务目标、允许编辑范围、工作树/Remote owner、验收证据和当前 Invocation。失败、失联和未观察到启动分别显示，避免凭进程或投递状态结算 Job。
4. **P2：上下文视图契约测试。** 如果 Noema 将来自己生成可发送给模型的压缩 transcript，先规定工具调用与结果成对、同一批工具调用不可只保留一半、摘要指向原始事件 ID。当前 ACP 原生 transcript 仍由 agent-shell 管理。

本轮没有从这三个上游项目复制代码，也没有找到足以立即改动 Noema 核心协议的已证实新 bug。以上建议为源码比较后的设计推论；具体落地须按 Noema 的现有 Project、CAS、人工状态权威和 ACP 边界实现。
