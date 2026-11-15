# 数学探索与 Lean 判定

数学工作有两件事不能交给模型自己说了算：证明是否成立，以及哪些方向已经有人做过。
前者交给 Lean 判定，后者交给探索看板。两者都把思路和痕迹留在项目的 Markdown 里，
人随时能在 Noema 里读到；Agent 与 session 按 `noema-math-research` Skill 的统一标准维护。

来源与取舍见 [Open Math Harness 审计](../../records/audits/open-math-harness-2026-11.md)。
Lean 判定协议与探索看板的规则翻译自 Open Math Harness（Apache-2.0），按本配置的
Remote、Markdown 与 agent-shell 约定重写，没有引入它的微内核或多人协作模型。

## 组成

| 部分 | 位置 | 职责 |
| --- | --- | --- |
| Lean 判定 | `site-lisp/noema/server/lib/lean-judge.mjs` | 冻结陈述的文本规则与严格 verify 协议；纯函数，只有 `runLean` 启动进程 |
| 判定入口 | `scripts/lean-judge.mjs`，`noema:api:agent:lean:judge` | 有 shell 的 Agent 不需要 host；Pi 走 API |
| 探索引擎 | `server/lib/exploration.mjs` | 看板规则、journal 折叠、成员起停、监督时钟、Markdown 文件 |
| 探索入口 | `noema:api:agent:exploration`，`scripts/exploration.mjs` | 命令行把每次调用转给 host，host 持有时钟和唯一写者 |
| 一次模型调用 | `server/lib/pi-model.mjs` | 方向查重；与 Hy Memory 网关共用 Pi 运行时与 Codex 登录 |
| 提示词 | `resources/agent/exploration/{member,coordinator,duplicate,resume}.md` | 数据，改动不需编译或重启 |
| Emacs 页面 | `site-lisp/noema/lisp/noema-exploration.el` | 列出项目的探索，打开 Markdown，人可裁决、停止、恢复、发起 |
| 标准 | `resources/skills/noema-math-research/`，pack `noema-math` | 项目布局、状态标签、完整周期、记忆分层；只写判断 |

## Lean 判定

冻结陈述是人的文件：导入、陈述用到的定义，加一条 `theorem 名字 … := sorry`。
`statement` 检查文件可否用于判定；`check` 给部分证明分级；`verify` 判定是否证出。

- **L1 标准只来自人的文件。** 请求只给文件路径，陈述从 `statement` 文件读取；候选文件
  和模型都不能改写判定标准。冻结文件的定理必须只由 `sorry` 证明、证明之前不得有
  `sorry`、不得含执行代码的词、最后一行不能以 `in` 结尾，否则拒绝并说明原因。
- **L2 按代码读，不按文本读。** 注释与字符串先被抹成空格再检查，因此注释里的
  `sorry` 不算，注释里伪造的第二份定义也挪不动陈述的位置。词法上可能与 Lean 读法
  不一致的写法（原始串、插值串、转义、字符字面量紧邻注释或 `«`）一律拒绝，不猜。
  执行代码或改语法的词（`#eval`、`macro`、`elab`、`open Lean`、`IO`、`unsafe` 等）、
  `native_decide`、用户 `axiom`、`set_option pp.*`/`debug.*` 在部分证明里也禁止。
- **L3 前言逐行，陈述逐字。** 候选的前几行代码必须与冻结前言逐行相同（空行和注释除外，
  字符串里每个字符都算），下一行必须开始一条新命令；冻结的 `theorem … :=` 必须
  逐字出现在前言之后。拒绝信息原样给出第一处不同的行或完整陈述。
- **L4 verify 的五项检查。** 无禁用词；陈述与前言保留；Lean 以冻结前言本身、分隔命令、
  再接候选前言之后的部分来编译；`#print axioms` 不含 `sorryAx`，且只用 `propext`、
  `Classical.choice`、`Quot.sound`；`pp.all` 类型与冻结文件自身算出的类型逐字相同，
  从而挡住保留表面文字却换了含义的改写。
- **L5 只读标记之后的报告。** 每次运行生成不可预测的标记，只解析紧跟它的一行公理和类型块；
  候选自己打印的任何内容都不会被当作判定。
- **L6 check 来自 Lean 的诊断。** `invalid`（文本规则拒绝）、`errors`（带行列，行号是候选
  自己的）、`sorries`、`complete`。`complete` 只是候选，`verify` 才算数。
- **L7 无判定与拒绝分开。** Lean 无法启动、超时或被信号结束时回答 `no_verdict`，可以重试；
  Lean 运行到底并报错才是拒绝。Lake 项目取冻结文件之上最近的 `lakefile`，可显式传入；
  超时默认 5 分钟、最多 30 分钟；子进程只拿 `PATH HOME LANG …` 等少数环境变量。
- **L8 原陈述的类型缓存按 Lake 项目与文本。** 同一文本在另一个 Lake 项目里可能有不同含义，
  缓存键同时包含两者；缓存放在 `var/noema/lean-judge/`。

## 探索

`explorations/<id>/` 位于 Noema 项目根下：`problem.md`、每个成员一本笔记本 `m1.md …`
与草稿目录 `m1/ …`、结束时的 `summary.md`，以及只记事实的 `journal.jsonl`。
发起者（一个 agent-shell 会话）是裁判；人也可以在 Emacs 页面裁决。

- **E1 思路在 Markdown 里。** `start` 写 `problem.md`（带 Noema meta，链接来源笔记与成员）和
  每个成员的笔记本骨架（Direction、State、Work、Report），再派出成员。笔记本只由它的
  成员写；引擎只改写其中的 `## Direction` 一节。发生过什么只在 journal，状态每次读取时折叠。
- **E2 发起者是会话。** `coordinator` 必须是一个活着的会话的 `{buffer, nativeSessionId}`，
  由 `emacs:agent status` 确认；成员默认使用它的 provider，可指定 `agent`、`modelId`、`effort`。
  成员是 `subagent` 来源的后台 agent-shell 会话（Agent 管理 C60–C62），人可在 Agent Manager
  里打开。成员 1–8 个（默认 4），时限 10–1440 分钟（默认 120）。
- **E3 先注册方向。** 成员在注册方向前只能读看板；一个成员同时只持有一个方向，换方向要先
  `abandon` 并写明原因，原因留在看板上。
- **E4 看板有界。** 方向至多 40 个、每个方向至多 32 条发现；方向 600 字节、发现 800 字节、
  报告 1500 字节。超长一律拒绝并要求缩短，从不截断。
- **E5 查重不放行未比较的方向。** 注册时由一次 Pi 模型调用（只给一个工具，提案与看板按行
  加框作为数据）判断是否与任何已有方向相同，进行中、成功、放弃、死亡的都算。调用失败、
  回答不是那个工具或指向不存在的方向时拒绝注册；比较期间看板若有新方向，只与新方向再比。
  发起时 `dedup: false` 可关闭查重。
- **E6 报告必须先告诉裁判。** 提交时先给裁判会话发一条带来源头的消息（报告、文件、笔记本、
  裁决命令），发送失败则拒绝这次提交，成员稍后再交；一份报告只唤醒裁判一次。文件必须在
  探索目录之内。
- **E7 有 Lean 陈述时，Lean 先判。** 发起时给 `lean`，此后提交必须附 `proof`；引擎先 `verify`，
  不被接受就记一条 `judged` 并把 Lean 的理由还给成员，裁判不被打扰；`no_verdict` 时请成员
  重交。被接受的证明在消息里注明公理，裁判只需判断它是否回答了问题和标准。
- **E8 两次拒绝结束一个方向。** 第一次拒绝把理由发回成员，同一方向再试一次；第二次拒绝该方向
  结束，成员可在时限内注册新方向。拒绝必须写明漏洞在哪。接受使该成员完成，其余成员继续；
  `finish` 停下所有成员并以 solved 结束。人的裁决记为 `person`。
- **E9 消息说明来源。** 发给裁判与成员的消息都以 `[From exploration …]` 开头，并写明不是人；
  它们不携带人的批准。
- **E10 监督时钟在 host 里。** 每个运行中的探索一个计时器（至多每 2 分钟），关闭时为零。
  它查询成员任务回执：回合已结束而没有报告的记 `idle`，回执不存在的记 `lost`；持有方向的
  成员 idle 或 lost 时方向显示 stalled。到时限停止成员，有接受的报告则 solved，否则 expired。
  没有活着的成员且没有待裁决报告时结束：solved 或 exhausted。待裁决报告让探索继续到时限。
  host 重启后，任何读取（`list`、`status`、页面）都会重新挂上时钟。
- **E11 中断与恢复。** 所有未完成成员的会话都消失（例如 Emacs 重启）时记 `interrupt` 并告诉裁判。
  `resume` 为每个未完成的成员新开会话，提示它从自己的笔记本与看板接续，并告知它仍持有的方向；
  原对话不重放。时限已过或不足两分钟时必须给 `minutes`。
- **E12 结束只发生一次。** 先写 `end` 事实，再写 `summary.md`（已接受的方向在前，链接笔记本），
  再唤醒裁判一次；裁判自己停止或以 `finish` 结束时不再唤醒它。
- **E13 Emacs 页面只读答案。** `noema-exploration` 每次打开或 `g` 时调用 `list` 与 `status`，
  不留计时器或状态；`RET` 打开 problem、笔记本或 summary，`a`/`r` 以人的身份裁决待审报告，
  `s` 停止，`R` 恢复，`n` 发起。`noema-exploration-start` 以当前聊天（或选中的项目会话）为裁判，
  并把裁判说明发给它。动作表：`C-c A M`／`/explore` 发起，`C-c A V`／`/explorations` 页面。
- **E14 命令行与 API 同一份代码。** `scripts/exploration.mjs` 把每个操作转给 host；
  `agent:project:status` 的 `explorations` 给出运行、中断与待裁决报告的数量。

测试：`tests/lean-judge.test.ts`（L1–L8，真实 Lean 用例需 `NOEMA_LEAN_JUDGE_LIVE=1`）、
`tests/exploration.test.ts`（E1–E14）、`tests/elisp/noema-exploration-tests.el`（E13）。

## 不做

- 不做 Dream-RSI 证明树搜索：它要常驻的 Lean REPL、策略进程和大量模型预算，
  对单人研究的收益不抵成本。冻结陈述协议与 `check` 已足够支撑人和 Agent 迭代证明。
- 不做多人团队、审批主体或能力内核；这里的身份是会话的 buffer 与 native ID，信任边界是本机用户。
- 不自动把探索结论写进人的笔记；结论回到 notes 由裁判在人要求时做，或由人自己做。
- 不复制上游的 MEMORY.md 合并代理：项目记忆的分层见 `noema-math-research`，引用闭环见 [Hy Memory](hy-memory.md)。

## 已知差距

- **G1 真实端到端。** 引擎、页面与判定都有确定性测试，判定另有真实 Mathlib 用例；
  尚未在运行中的 host 上用真实成员跑完整一轮探索（需要模型预算与人的在场）。
- **G2 记忆引用闭环。** 已在召回侧实现（[Hy Memory](hy-memory.md) M1–M6）：引用被计数，
  长期未被引用的记忆退出注入。没有做的是上游的合并步骤，即由受限 Agent 把高引用记忆整理成
  项目文档；确认过的知识仍由人或裁判写进 notes。
- **G3 Remote 目标。** 成员按项目 workspace 启动，跟随 Agent 管理的 Remote 规则；Lean 判定在
  本机运行 `lake`。项目在其他 target 上时，判定尚未经 `remote-process-file` 路由。
- **G4 `sorry` 的目标。** Lean 命令行不打印 `sorry` 处的目标，`check` 只报数量；上游用常驻 REPL
  列出目标并分析“空心”引理，这里没有引入 REPL。
