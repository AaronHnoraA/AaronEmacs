# Pi 的观察项

Agent 活不长，项目 Pi 活得长。观察项（watch）让短命的 Agent 留下一句“这件事发生时，告诉谁
或跑什么”，自己可以结束。倒计时、定时检查和 hook 是同一个对象，只有触发条件不同。

项目 Pi 是手动启动的，观察项跟着它：Pi 开着才运行，关掉什么都不留，观察项等下次启动。没有
服务，host 和 Emacs 里也没有定时器。Agent 怎样和 Pi 对话见 [Pi 会话](pi-sessions.md) C22。

PlanWeave、Trace Canvas 用 Claude/Codex 的 hooks 自动记录，要写各家 Agent 的全局配置，所以
[审计](../noema/records/audits/planweave-trace-canvas-audit-2026-11.md)没有采纳。这里观察的是项目
日志，不碰任何 Agent 的配置，对所有 Agent 一样。

## 分层

基础设施负责执行，协议只说约定，规则不写进 Skill。

| 层 | 位置 | 职责 |
| --- | --- | --- |
| 数据 | `<project>/.agent/pi/` | `journal.jsonl` 全部事实，`watcher.json` 当前执行者，Git 不跟踪 |
| 事实与规则 | `server/lib/pi-watch.mjs`，经 `noema:api:agent:pi:watch` | `list add cancel`，只读写事实，不执行 |
| Pi 插件 | `resources/pi/extensions/noema-manager/watcher.mjs` | 唯一的执行者，随项目 Pi 会话启停 |
| Emacs 组件 | `noema-project-graph` 的 Watches 段；动作表的 `/` 指令 | 查看，`x` 取消；`/cache` 等不走模型 |
| 协议 | `noema-project-graph` pack 的 `noema-pi` Skill | 何时用、怎么说；拒绝信息和 `fix` 来自引擎 |

pi-acp 既不公布 Pi 扩展的指令，也不转发它们的输出，所以管理指令是动作表的行，在聊天输入框
本地执行，不是 Pi 扩展指令。`/watch` 用 minibuffer 问时间和内容。

## 触发与动作

| `when` | 含义 |
| --- | --- |
| `{after:"20m"}` | 到点一次 |
| `{every:"10m", times}` | 重复，最短 1 分钟 |
| `{on:{kind, task, agent}}` | 任务图谱事件：`plan claim release submit review state note decision commit` |
| `{on:{task, phase}}` | 条件：任务到达该 phase 时一次 |

| `do` | 含义 |
| --- | --- |
| `{prompt, to}` | `to` 是 `pi`（默认）、`companion` 或 `{buffer, nativeSessionId}` |
| `{script, args, report}` | 运行项目内脚本；`report` 同 `to`，结束时汇报 |

## 契约

测试在 `tests/pi-watch.test.ts`。

- **W1 数据在项目里。** 根与任务图谱相同。第一次 `add` 或项目 Pi 启动才建 `.agent/pi/`。
- **W2 消息说明来源。** 送达的消息带观察项 id、原因和设置者，并写明不是用户，经 `emacs:pi`、
  `emacs:agent` 走真实 ACP 队列。它不带人的批准。
- **W3 跟随 Pi。** 只有运行中的项目 Pi 执行，同一项目只有一个；主 Pi 不执行。Pi 关闭时不留
  定时器和监听；到点的和新发生的图谱事件等下次启动各补一次。`list` 和 `add` 的 `running` 说明现状。
- **W4 每一项都会结束。** 到点、次数用尽或一周到期。`on` 默认只触发一次。要继续就重新加。
- **W5 hook 看事实，不靠通知。** 图谱事件从日志读，所以没有 host 的脚本写入也算，添加之前的
  不算。`phase` 是条件，不是事件。
- **W6 目标不在了改告诉 Pi。** 会话已不存在时消息转给项目 Pi；都未受理时报告 `failed`，不重试。
- **W7 不重放。** `fire` 事实先写，动作后做。Pi 崩溃后没有 `result` 的记为 `unknown` 并报告，
  需要有人核对效果。
- **W8 写入不留半成品。** 与任务图谱相同的追加和半行规则（G3）。
- **W9 消息有预算。** 每个项目 24 小时内最多 48 条消息，因为每条都会启动一个模型回合；超出后
  等待并报告 `budget`。不汇报的脚本不计入。
- **W10 人能取消。** 页面 `x`，或 `cancel`。关掉 Pi 就全部停下。
- **W11 脚本受限。** 文件的真实路径必须在项目根或 `workspace` 内且可执行；不经 shell，参数是
  字符串数组；环境只给 `PATH HOME LANG LC_ALL TMPDIR USER SHELL` 和 `NOEMA_PROJECT_ROOT`、
  `NOEMA_WATCH_ID`，凭据不传入。10 分钟超时，输出只留最后 2000 字符。Pi 没有 shell，这是它唯一
  的执行入口；能写项目文件的 Agent 由此能让 Pi 在无人值守时运行代码，所以项目文件要当作代码对待。
- **W12 闲置无开销。** Pi 关闭时为零。Pi 开着时只有一个目录监听，有观察项时加一个定时器，
  有 hook 时再加一个目录监听。
