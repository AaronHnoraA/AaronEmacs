# 文档任务工作区：源码学习记录

阅读日期：2026-11-18。本文只记录读到的实现和设计判断，没有安装、运行或验证
第三方产品的完整行为。固定提交便于复查，产品现状可能继续变化。
我们的拟议设计见 [README.md](README.md)。

## 阅读范围

| 项目 | 本次固定提交 | 阅读重点 |
| --- | --- | --- |
| Nimbalyst | `30482d5b719cd7609fb02645008c2f5088565ab9` | 任务状态、审阅权限、Inbox 与列表投影 |
| SharpMD | `77ec504ab29f3898c334285390cb51a288947705` | 批注锚点、MCP 接口关系、看板数据、Agents Sidebar |
| TaskFlow | `9036df6456bb8a46672810d95ecda3e8a9f6eb4b` | Worker 如何领取、执行与更新任务 |
| Lovelace | `afa172ec54df49831d0721bc9df44389685bac7b` | MCP 与项目模型的边界 |
| TaskNotes | `e2ad5d8ac5c641080909fc87f23f0fbb79e8450c` | 基于最新文档的局部修改与多视图动作 |
| Claudian | `1358534a5abac3df6315014032c2edf03339b493` | 选区 / 光标上下文与局部修改入口 |

## Nimbalyst：优先学习任务与审阅

`task.yaml` 明确列出 to-do、in-progress、in-review、changes-requested、approved、
done 等值，并将 title / workflowStatus / priority / assignee / tags 指定为语义角色。
这说明视图可以按同一语义读不同对象字段；无需让各个列表各写一套规则。
[任务模型](https://github.com/nimbalyst/nimbalyst/blob/30482d5b719cd7609fb02645008c2f5088565ab9/packages/runtime/src/plugins/TrackerPlugin/models/builtins/task.yaml)

review core 把 approved 标为 human-only，runtime adapter 为对象类型取出其实际
支持的审阅阶段。这里值得学习的是在工具边界区分 Agent 提交与人工批准。
不能据此宣称所有文件修改都受此限制，也不能把 approved 的限制扩大成所有 done
调用均被禁止。
[review core](https://github.com/nimbalyst/nimbalyst/blob/30482d5b719cd7609fb02645008c2f5088565ab9/packages/tracker-core/src/trackerReview.ts)、
[runtime adapter](https://github.com/nimbalyst/nimbalyst/blob/30482d5b719cd7609fb02645008c2f5088565ab9/packages/runtime/src/plugins/TrackerPlugin/models/trackerReview.ts)

`trackerInbox.ts` 是无 I/O 的判定模块：从是否分派、是否改过优先级、是否加入集合、
是否离开初始状态等事实推导待分拣项，并区分用户选择与系统默认值。
这可借鉴为统一“需要我处理”的投影，但其 untriaged 与我们的 provider 审批、
科研 Review 并非同一种等待，不能直接映射为一个状态。
[Inbox 实现](https://github.com/nimbalyst/nimbalyst/blob/30482d5b719cd7609fb02645008c2f5088565ab9/packages/runtime/src/plugins/TrackerPlugin/models/trackerInbox.ts)

`trackerRowData.ts` 将筛选和排序从表格界面中抽出来，共用标题、路径、执行者、
标签等搜索字段。借鉴点是侧栏、展开总览与搜索使用同一个任务投影。
[列表投影](https://github.com/nimbalyst/nimbalyst/blob/30482d5b719cd7609fb02645008c2f5088565ab9/packages/runtime/src/plugins/TrackerPlugin/components/trackerRowData.ts)

另读到 AgentTranscript 下的 `TodosSidebar.tsx`：它接受一组 TodoItem，按进行中、
待办和完成分组。这是会话内 Todo 组件，不能仅凭文件名把它当作全项目任务管理器。
[会话 Todo 侧栏](https://github.com/nimbalyst/nimbalyst/blob/30482d5b719cd7609fb02645008c2f5088565ab9/packages/runtime/src/ui/AgentTranscript/components/TodosSidebar.tsx)

项目工作流文档要求任务关联会话，bug 经 in-progress、in-review，在特定提交引用
时由项目逻辑完成。这是该项目自己的工作约定。我们采用任务 / 会话关联，完成条件
改为用户对文档交付的明确裁定。
[工作流说明](https://github.com/nimbalyst/nimbalyst/blob/30482d5b719cd7609fb02645008c2f5088565ab9/docs/TRACKER_WORKFLOWS.md)

实际的 `trackerSessionLaunch.ts` 在创建前核对条目的 workspace，构造包含来源文档
的 prompt，并为任务保留 trackerLinkId；项目在选择过程中变化时拒绝用新项目启动
旧任务。可学习的是从任务发起执行时固定来源身份，并在真正发送前再次检查。
我们对应使用 Noema Project / workspace 与 `/fs:` 身份，不复制其 Electron 启动器。
[任务启动边界](https://github.com/nimbalyst/nimbalyst/blob/30482d5b719cd7609fb02645008c2f5088565ab9/packages/electron/src/renderer/components/TrackerMode/trackerSessionLaunch.ts)

## SharpMD：批注与 Agents Sidebar

`src/comments.js` 开头明确说服务器不唤醒 AI：外部 MCP 客户端读取批注并关闭它。
创建时选区或 block 的文字成为引文，保存尚未完成的正文后才提交批注。显示时按
去空白后的引文在渲染 block 中寻找位置；找不到时批注留在列表中。
值得学习保存先于派工和失去锚点后保留请求；我们的唯一定位与版本守护要更严格。
[批注实现](https://github.com/SharpMD/sharpmd/blob/77ec504ab29f3898c334285390cb51a288947705/src/comments.js)

`src/board.js` 把看板列与卡片保存在 Markdown 块中，卡片有 id、创建和更新时间；
移动到完成列改变 done 状态。它证明了稳定任务身份与文本可读性可以共存。
我们的任务直接位于文档批注，不复制同一请求到另一个看板正文。
[看板格式与状态](https://github.com/SharpMD/sharpmd/blob/77ec504ab29f3898c334285390cb51a288947705/src/board.js)

`src/agents.js` 的 Agent 由 MCP 客户端注册，界面展示任务、等待原因、文件 / 卡片
链接和父子关系。该文件也明确使用可见时的 5 / 15 秒轮询及超时“无信号”语义。
我们学习信息密度与等待原因展示，状态使用真实 ACP 事件；不把这套临时服务器
注册表、轮询或云账户要求带进 Emacs。
[Agents Sidebar](https://github.com/SharpMD/sharpmd/blob/77ec504ab29f3898c334285390cb51a288947705/src/agents.js)

## TaskFlow：执行后进入审阅

所读 Electron worker 筛选 assignee 为 ai-agent 且 TODO / NEED_FIX 的任务，进入
IN_PROGRESS 后调用 Claude executor，成功更新 IN_REVIEW，失败更新 NEED_FIX。
它另有 taskQueue、执行日志和轮询配置。值得学习执行状态与审阅状态分开；我们
已有 ACP 队列与进程 owner，不再加入这个 Worker，也不把它的成功返回视作科研正确。
[Worker 实现](https://github.com/jiwonp7747/taskflow/blob/9036df6456bb8a46672810d95ecda3e8a9f6eb4b/frontend/electron/services/aiWorker.service.ts)

## Lovelace：工具调用模型而非界面

`packages/mcp/src/server.ts` 使用 core 的 createTicket、updateTicket、logSession、
loadProject 等接口，并从 schema 生成部分工具说明。可学习同一模型服务工具和
界面的边界。本次未继续核查其全部存储和执行路径，不据此声称已验证其并发或恢复。
[MCP 入口](https://github.com/lovelace-co/lovelace/blob/afa172ec54df49831d0721bc9df44389685bac7b/packages/mcp/src/server.ts)

## TaskNotes：在最新内容上改命名字段

TaskUpdateService 的 frontmatter 回调重新读取当前字段，再应用调用方指定的更改；
core VaultMutationService 用每文件的 Promise 队列串行化自身修改，并交给 Obsidian
的文档修改 API。kanbanCardActions 调用同一任务服务。
借鉴点是基于最新文本修改指定字段，以及多视图共用写入；该队列只约束自身调用，
不能据此断言它为所有外部进程提供原子写入保证。
[任务写入](https://github.com/callumalpass/tasknotes/blob/e2ad5d8ac5c641080909fc87f23f0fbb79e8450c/src/services/task-service/TaskUpdateService.ts)、
[文件修改边界](https://github.com/callumalpass/tasknotes/blob/e2ad5d8ac5c641080909fc87f23f0fbb79e8450c/src/core/VaultMutationService.ts)、
[看板动作](https://github.com/callumalpass/tasknotes/blob/e2ad5d8ac5c641080909fc87f23f0fbb79e8450c/src/bases/kanbanCardActions.ts)

## Claudian：选区与光标是两种输入

inlineEditCommand 在有选区时传选中文本；没有选区时构造光标上下文，空行可取前后
最近非空行。editorContext 给出文件路径及位置提示。借鉴点是上下文来自用户正在
工作的编辑器，不要求再到聊天里解释“这里”；具体存储和派工仍沿用我们的任务模型。
[编辑入口](https://github.com/YishenTu/claudian/blob/1358534a5abac3df6315014032c2edf03339b493/src/features/inline-edit/inlineEditCommand.ts)、
[上下文](https://github.com/YishenTu/claudian/blob/1358534a5abac3df6315014032c2edf03339b493/src/core/prompt/editorContext.ts)

## 原生化结论

学习的是：原处提出问题、任务与会话分离、结果可定位、审阅有独立阶段、用户介入
有准确目标、多个视图共用模型和写入。实现归属仍是 Emacs Lisp + ETAF / Aaron UI、
现有 ACP 会话层、Noema CM6 / host / kernel 与 Markdown；没有导入第三方前端、
执行器、任务数据库或资料目录。
