# Pi 的观察项

Agent 活不长，Pi 和 host 活得长。观察项（watch）让短命的 Agent 留下一句“这件事发生时，
告诉谁或跑什么”，自己可以结束。倒计时、定时检查和 hook 是同一个对象，只有触发条件不同。

它不是独立服务：事实在项目的 `.agent/pi/`，host 里一个进程内的观察者负责执行，host 跟随
Emacs 生死。Agent 怎样和 Pi 对话见 [Pi 会话](pi-sessions.md) C22。

## 组成

| 部分 | 位置 | 职责 |
| --- | --- | --- |
| 数据 | `<project>/.agent/pi/` | `journal.jsonl` 全部事实，`runs/` 脚本日志，Git 不跟踪 |
| 引擎 | `site-lisp/noema/server/lib/pi-watch.mjs` | 校验、折叠、观察者；日志原语在 `agent-journal.mjs`，与任务图谱共用 |
| API | `noema:api:agent:pi:watch` | `list log add cancel pause resume`；`agent:project:status` 带 `watches` 摘要 |
| 会话事实 | `site-lisp/noema/lisp/noema-pi-watch.el` | 只在项目已有观察项时上报 `turn-end`、`client-exit` |
| Skill | `noema-pi` | 何时用；规则在引擎，随 `add` 的拒绝信息和 `list` 的 `fix` 返回 |
| 页面 | `noema-project-graph` 的 Watches 段 | 查看；`x` 取消一项，`p` 暂停或恢复全部 |

`var/noema/pi-watch-roots.json` 只列有观察项的项目根，host 启动时从这里找回它们。它可以丢：
`agent:project:status` 和新的 `add` 都会重新登记，事实始终在项目里。

## 触发与动作

| `when` | 含义 |
| --- | --- |
| `{after:"20m"}`、`{at:ISO}` | 到点一次 |
| `{every:"10m", times?, until?}` | 重复，最短 1 分钟 |
| `{on:{kind, task, agent}}` | 任务图谱事件：`plan claim release submit review state note decision commit` |
| `{on:{kind:"turn-end"\|"client-exit", buffer, agent}}` | 执行会话的事实 |
| `{on:{kind:"script-ok"\|"script-failed", watch}}` | 脚本结束 |
| `{on:{task, phase}}` | 条件：任务到达该 phase 时一次 |

| `do` | 含义 |
| --- | --- |
| `{prompt, to}` | `to` 是 `pi`（默认）、`companion` 或 `{buffer, nativeSessionId}` |
| `{script, args, timeout, report}` | 运行项目内脚本；`report` 同 `to`，结束时汇报 |

## 契约

测试在 `tests/pi-watch.test.ts`、`tests/elisp/noema-pi-watch-tests.el`。

- **W1 数据在项目里。** 根与任务图谱相同；第一次 `add` 才建 `.agent/pi/`，读取不创建东西。
- **W2 消息说明来源。** 送达的消息带观察项 id、原因和设置者，并写明不是用户。它不带人的批准。
- **W3 错过的补一次。** 时间按墙钟算。host 不在时错过的到点，下次启动补一次并标明迟到，
  重复项不连补。
- **W4 每一项都会结束。** 到点、次数用尽或寿命到期；默认寿命 7 天，上限 30 天。`on` 默认
  只触发一次。不存在忘了就永远跑下去的观察项，要继续就重新加。
- **W5 hook 看事实，不靠通知。** 图谱事件从日志读，所以没有 host 的脚本写入也算，host 重启后
  从游标补上。添加之前的事实不算。`phase` 是条件，不是事件。会话事实只有在某一项需要时才写入。
- **W6 没受理的才重发。** 消息未被受理时按 1、5、15 分钟重试，之后报告 `failed`。目标会话已
  不存在时改告诉 Pi，不丢。受理之后不重发。
- **W7 不重放。** `fire` 事实先写，动作后做。崩溃后没有 `result` 的记为 `unknown` 并报告，
  需要有人核对效果。脚本失败不重试。
- **W8 写入不留半成品。** 与任务图谱相同的追加、半行、锁和 `requestId` 规则（G3、G4）。
- **W9 消息有预算。** 每个项目 24 小时内最多 48 条消息，因为每条都会启动一个模型回合；超出后
  等待并报告 `budget`。不汇报的脚本不计入。
- **W10 人能停下一切。** `pause` 保留全部观察项但都不触发；`cancel` 结束一项。页面可以直接做。
- **W11 脚本受限。** 文件的真实路径必须在项目根或 `workspace` 内且可执行；不经 shell，参数是
  字符串数组；环境只给 `PATH HOME LANG LC_ALL TMPDIR USER SHELL` 和 `NOEMA_PROJECT_ROOT`、
  `NOEMA_WATCH_ID`，host 的凭据不传入。默认 10 分钟超时，上限 1 小时。日志单份 1 MiB，
  保留最近 40 份。Pi 没有 shell，这是它唯一的执行入口；能写项目文件的 Agent 由此能让 Pi 在
  无人值守时运行代码，所以项目文件要当作代码对待。
- **W12 闲置无开销。** 没有观察项时没有定时器、文件监听和进程。有观察项时只有一个定时器，
  有 hook 时才监听一个目录。暂停时什么都不留。普通会话的回合结束只多一次 `stat`。
