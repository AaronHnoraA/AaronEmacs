# Open Math Harness 审计（2026-11）

对象：[openmathmodel/open-math-harness](https://github.com/openmathmodel/open-math-harness)
0.41.0（Nucleus，提交 `687f2a2`），Apache-2.0，Rust，约 12 万行。克隆在
`docs/works/open-math-harness/upstream/`（不进 Git）。目的：学习它在数学工作上的做法，
审计 Emacs 与 Noema 的 Agent 工作流、产品逻辑与人机交互，并把值得的部分翻译进来。
按“只采纳确有改进的上游”的原则逐项判断。

## 读了什么

| 上游 | 内容 |
| --- | --- |
| `AGENTS.md`、`README.md` | 微内核原则：机制在内核、行为在服务；文本不授予权限；失败即重来 |
| `interfaces/lean`、`program-files/lean`（rules.rs、verify.rs） | 冻结陈述协议、禁用词、`#print axioms`、`pp.all` 类型比对、check/analyze/complete |
| `program-files/prover`（prover-agent.j2、putnam.j2） | 证明 Agent 的工作方法：STATE.md 索引、主张状态、反例范围、上下文重置后从文件接续 |
| `program-files/discovery`、`interfaces/explore` | Dream-RSI 树搜索：CONTINUE(v)、探索策略、计划看板、失败归因、分支指针 |
| `interfaces/swarm`、`chat-agent/src/swarm`、`prompts/.../swarm` | Ultramath：方向注册、模型查重、发现看板、报告与裁决、中断恢复 |
| `memories/`、`program-files/memory` | 会话提取、受限 Agent 合并进 MEMORY.md、引用计数决定保留 |

## 上游的数学做法里真正有效的东西

1. **判定权不在模型。** 证明是否成立只由 Lean 决定；陈述来自可信一方，候选连陈述的
   一个空格都不能改；读 Lean 输出时只读每次运行随机标记之后的报告；类型用 `pp.all`
   比，挡住“表面相同、含义不同”的改写。
2. **并行要有方向的唯一性。** 多个 Agent 攻同一题，最大的浪费是重复。先注册方向、
   由一次独立的模型调用查重、查重失败就拒绝，比事后合并便宜得多。
3. **裁判只被唤醒一次。** 报告有长度上限，超长退回而不截断；裁判没被告知就不收报告；
   两次拒绝结束一个方向。规则在服务里，不在提示词里。
4. **工作方法写进成员提示词。** 保持一个 60 行以内的 STATE 索引，区分已证、猜想与数值证据，
   反例写明它推翻的确切命题与测试范围，失败的命令什么也不推翻；上下文重置后从文件接续。
5. **事实先落盘，动作后做；说清失败了什么。** 中断就是中断，不伪造完成，不重放副作用。

## 对照：数学工作的完整周期

| 阶段 | 我们已有 | 上游 | 差距与决定 |
| --- | --- | --- | --- |
| 问题与文献 | Noema notes、Wiki、`lit-review`、MarginNote OCR | 无 | 我们更强；保留 |
| 框架与计划 | `proof-plan`、`planning.md` 的 `@@todo`、任务图谱 | 计划看板（discovery 内） | 计划留在 Markdown；任务图谱用于多会话工程 |
| 单线推进 | agent-shell 会话、Pi、后台 worker（C60–C62） | prover 循环 | 方法论缺失 → 写进 `noema-math-research` 与成员提示词 |
| 并行探索 | `+agent-worktree-parallel` 面向代码的并行尝试 | Ultramath | **缺口**：没有方向唯一性与裁判回路 → 实现 exploration |
| 形式验证 | `lean4` Skill、Lean LSP、`checks/lean/` | Lean 判定协议 | **缺口**：没有“由 Lean 判定、陈述冻结”的工具 → 翻译 lean-judge |
| 纸面审查 | `proof-review` | 裁判提示词 | 采纳裁判规则（有漏洞就拒绝并指出位置） |
| 记录与留痕 | notes、`log.md`、Finding、`.agent/handoffs` | 运行记录 jsonl | 留痕改为 Markdown：问题页、笔记本、summary |
| 记忆 | Hy Memory 自动召回与提取、项目文档 | MEMORY.md 合并 + 引用闭环 | 分层写进 Skill；引用闭环记为差距 G2 |
| 写作 | `math-prose`、`paper-polish`、LaTeX 导出 | 无 | 我们更强；保留 |

## 采纳并翻译

- **Lean 判定**（`lean-judge.mjs`）：rules.rs 的词法、禁用词、陈述与前言规则，verify.rs 的
  五项检查、报告标记、`pp.all` 比对和“无判定 ≠ 拒绝”。去掉了 `import Mathlib` 唯一头部的限制
  （它只为常驻 REPL 与冷启动一致；这里没有 REPL），理论名取自冻结文件而不是题号。
  按 Lean 4 实际输出修正了诊断解析（`error(lean.tag):` 带标签的形式）。
- **探索**（`exploration.mjs`）：Ultramath 的看板规则、模型查重（失败即拒绝）、一次唤醒、
  两次拒绝、中断与恢复。改动：方向和发现之外的思考全部写进成员自己的 Markdown 笔记本，
  中断后从笔记本而不是对话记录恢复；成员是普通后台 agent-shell 会话；时钟在 Noema host 里。
- **工作方法**：prover-agent.j2 的 STATE 索引与证据纪律，并入成员提示词与 `noema-math-research`；
  状态标签沿用你已有笔记里的 `[known]`、`[known, verify]`、`[open]`，扩充为证明、证据、猜想、反驳。
- **一次模型调用**（`pi-model.mjs`）：从 Hy Memory 网关抽出共用；host 缺系统 CA 时在子进程调用。

## 不采纳

| 上游 | 理由 |
| --- | --- |
| 能力微内核、bubblewrap、Linux 身份 | 单人、本机；本配置不依赖系统级改动；信任边界是用户本身 |
| 多人团队与审批主体 | 不需要多人协作；人始终可在 Emacs 页面裁决 |
| Dream-RSI 树搜索与探索策略进程 | 需要常驻 REPL 和大量模型预算；冻结陈述 + `check` 已能支撑迭代 |
| analyze/complete（空心引理、自动补全） | 依赖常驻 REPL；记为 math-exploration G4 |
| MEMORY.md 合并 Agent | 与“确认的知识进项目文档”的约定重叠；Hy Memory 已承担提取召回 |
| 计划看板 | 成员按方向注册，探索看板已覆盖其作用 |

## 项目识别、挂载与记忆的审计

- **项目识别。** 只有声明 `[project]` 的 `noema.toml` 才是 Noema 项目（D-038）。实测你的库里
  `public/*` 与 `private/research` 只声明了 `repository_id`；研究项目 `UNSW/LCE(202610)` 有
  `[project]`，`ISO(202603)` 没有。因此大多数会话收不到项目入口，Hy Memory 按目录分区。
  这符合“访问文件不创建项目”的契约，但 Agent 容易拿 cwd 当项目根。决定：Skill 明确要求先用
  `agent:project:status` 与 `emacs:pi context` 向 Emacs 查询，不是项目却要做项目级工作时请人
  启用，而不是自己写清单。
- **项目布局。** 你的真实项目已形成稳定的 Markdown 习惯：`AGENTS.md` 写约定，`notes/` 里的
  主张带标签，`checks/lean`、`checks/python` 放机器检查，`planning.md`、`log.md` 计划与留痕，
  Wiki 用 roam 链接引用。`noema-math-research` 把这套习惯写成所有 Agent 共用的标准，
  探索目录 `explorations/` 作为同一布局的一部分。
- **一个入口。** `agent:project:status` 现在同时报告项目的探索（运行、中断、待裁决数量），
  Agent 一次只读调用即可看到项目全貌。
- **记忆召回实测。** 本机 Emacs 项目分区存有 51 条记忆，召回却一条都拿不到：NAS 上的
  embedding 服务从这台 Mac 连不上（请求超时），而 Hy Memory 在这种情况下返回空结果、不报错，
  于是 `agent:memory:search` 会把“查不了”说成“没有”。桥现在把这种情况如实报为失败（M6）。
  网络本身需要人处理。
- **记忆。** 四层：项目文档（权威）、Finding（经审核）、Hy Memory（自动、未核实）、探索与日志
  （发生过什么）。上游的引用闭环已在召回侧实现（Hy Memory M1–M6）：召回的记忆可引用，
  引用落在项目的 `.agent/memory/citations.jsonl`，长期无人引用的记忆不再注入。

## 人机交互

- 发起：在聊天里 `/explore`（`C-c A M`）以当前会话为裁判；或 Agent 自己调用 API。
- 观察：`/explorations`（`C-c A V`）或项目总览的 Explorations；每个文件都是 Noema 笔记。
- 裁决：裁判会话被唤醒一次（经 ACP 队列，Inbox 与通知照常）；人可在页面 `a`/`r` 直接裁决。
- 收尾：`summary.md` 留档，裁判被告知一次；结论回到 notes 由人决定。

## 验证

- `tests/lean-judge.test.ts` 9 项，包括在 `~/Documents/Noema` 的 Mathlib 上真实运行；另以命令行
  在真实 Lean 上验证：正确证明通过，含 `sorry`、改写定义、私加公理被拒，`check` 报 sorry 与
  带行列的错误。
- `tests/exploration.test.ts` 11 项；`tests/elisp/noema-exploration-tests.el` 4 项；
  相邻的项目、图谱、观察项、API、Skill 契约测试全部通过。
- 本机真实调用 Pi 模型一次，确认 `pi-model.mjs` 在缺少 `NODE_EXTRA_CA_CERTS` 的进程中可用。
- 未做：运行中的 host 上真实成员的一轮完整探索（math-exploration G1）。
