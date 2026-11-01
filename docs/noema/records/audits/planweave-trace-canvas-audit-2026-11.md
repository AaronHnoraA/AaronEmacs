# PlanWeave 与 Trace Canvas：学什么，不学什么（2026-11-02）

对照项目级 Agent 工作，判断能给 Noema 带来什么。结果是
[项目任务图谱](../../design/project-graph.md)。没有复制任何上游代码。

## 范围

静态阅读，未运行上游：

- [PlanWeave `727f126`](https://github.com/GaosCode/PlanWeave/tree/727f126)：3196 个文件，Electron、服务端、Agent Host。
- [Trace Canvas `5784019`](https://github.com/CallSymmetryEcho/trace-canvas/tree/5784019)：约 3600 行 `.mjs`，零依赖。

## 动手前核对的 Noema 事实

只读查询 `~/.config/emacs/.agent/state.sqlite`：`tasks=0, jobs=0, invocations=0, proposals=0,
findings=0, work_nodes=2, runs=4`（4 个 Run 集中在约 87 秒内），`sessions=27`，对应 24 个
`workstreams`。vault 里 36 行 `@@todo` 分布在 7 个文件，没有一行带依赖。

结论：Graph Board 退役后 WorkNode 没有创作入口；Task/Job 的代码在，但没有驱动者，也没有数据；
Task 依赖不能跨 workstream，而 workstream 基本一会话一个，所以这些模型放不下项目级的图。
这是不在其上构建的理由，不是说它们有缺陷。

## 一句话定位

PlanWeave 面向未来：把计划变成可领取、可评审的工作。Trace Canvas 面向过去：把会话变成
可读的线路图。Noema 需要二者的数据结构，不需要它们的外壳。

## 采纳

| 来源 | 做法 | 在 Noema 中 |
| --- | --- | --- |
| PlanWeave | 文件即节点，定义与运行状态分离 | `<id>.md` 与日志分开（G1） |
| PlanWeave | draft 校验后整体导入 | `plan` 整批校验、`dryRun`（G5） |
| PlanWeave | Review 是一等公民，反馈回到实现 | 评审回路（G9） |
| PlanWeave | 协调者只分派、runner、reviewer 分工 | `noema-project-run`、`noema-project-review` |
| PlanWeave | `plan-auditor` 的 Flow Coverage | 压缩进 `noema-project-plan`，机械部分进 `check` |
| Trace Canvas | 记录分类，决定单独成类 | 日志的 `decision`、`commit` |
| Trace Canvas | 提交只存 sha，内容现问 Git | G10 |
| Trace Canvas | 记录带会话，能回到当时的对话 | `sessions`，返工时优先恢复 |
| Trace Canvas | 同一线的工作交给最后干过的会话 | `next` 的 `lastSession` |
| Trace Canvas | 安静的线轻提示，不打断 | `quiet` 警告（固定 3 天，不做节奏估算） |
| 经典 DAG 审计 | 依赖收敛的原因向量（此前的 P1） | `phase` 与 `on`（G6） |

## 改良

- **PlanWeave 的 state 与 results 会漂移**，所以它需要 `plan-recovery`、doctor、恢复事务。
  这里同一事实只存一处（G1），这类问题不存在。
- **PlanWeave 在轮次用尽时把评审块标为 `completed`**
  （`reviewSubmission.ts:638`，`completionReason: max_cycles_reached`）。这里变 `blocked` 等人（G9）。
  未核实该状态在各 `completionPolicy` 下对下游的影响。
- **Trace Canvas 的锁 5 秒后按时间抢占**，它自己的 `why` 也承认慢写入可能被覆盖。
  这里只在持有者进程已死时解除（G3）。
- **Trace Canvas 整文件重写 JSON**。这里只追加，崩溃面只有最后一行（G3）。

## 不采纳

| 内容 | 原因 |
| --- | --- |
| Electron 画布、Web 页面、SSE | Emacs 是唯一界面；这个能力不该常驻 |
| 协作服务、Agent Host、远程调度 | Remote 框架和 `emacs:agent` 已管进程与目标 |
| Executor 配置与信任 | 能力管理已有；任务只写可选的 `agent` |
| Claude/Codex hooks 自动记录 | 写用户的全局配置，且记录过程流水；ACP 与 History 已有事实 |
| 读 Claude 桌面版私有会话文件 | 格式不属于我们 |
| Git 同步多机画布 | `.agent/` 不进 Git，是项目本地缓存 |
| 块级结构（Task 含多个 Block） | 扁平任务加评审轮次够用，没有需要它的调用方 |
| AI 梳理、复盘排行、token 统计 | 现在没有使用者，等真有需要 |
| 把 Noema 的 Task/Job 作为底座 | 见上面的事实 |

## 过程中发现

- `kernel/mcp/coordinator.md` 仍把项目 Pi 指向已退役的 Graph Board，已改。
- 端到端运行发现脚本把 `--text` 当成 `graph` 的绘图开关，吞掉了 `note` 的正文。单测没覆盖到，
  已修并加回归测试。手写的 Elisp 测试数据也测不出这种问题，页面最后用真实脚本输出和网关
  同样的 JSON 解析验证。
