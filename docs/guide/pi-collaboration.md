# 用 Pi 协作

Pi 是 Emacs 的全局管家，可以跨项目安排后台执行 Agent；项目 Pi 保留各项目自己的管理上下文。
这份指南讲 Agent 怎样交接结果、找到 Pi、把事情留到以后和保存任务状态。

设计与契约在 [Pi 会话](../design/pi-sessions.md)、[Pi 的观察项](../design/pi-watches.md)和
[项目任务图谱](../noema/design/project-graph.md)。

## 三样东西

| 东西 | 回答什么 | 存在哪 |
| --- | --- | --- |
| 任务图谱 | 要做什么、按什么顺序、谁在做、为什么 | `.agent/tasks/` |
| Pi 的观察项 | 以后发生什么时做什么 | `.agent/pi/` |
| 项目 Pi | 全局由谁看着、Agent 找谁 | 原生会话，`var/noema/pi/` 只记引用 |

分两层：工具执行规则，Skill 只讲约定。Pi 插件和 Emacs 组件是基础设施；`noema-project-graph`
pack 是协议和契约，随 Skill 目录披露给 Agent，用到才读。

## 日常

全局 Pi 用 `C-c A E`、`/butler` 或 `M-x noema-pi-emacs-open`。可以直接告诉它
“安排一个 OCR Agent 读取这段笔记，再让答疑 Agent 检查推导”。它通过 Emacs 的
`spawn` 创建后台 worker，通过 `notify` 接收结果。你不需要取图、复制回复或管理 session ID。
后台 worker 保留可手动打开的真实 buffer；`C-c A B`／`agent-open-worker` 打开它，
Agent Manager 统一显示状态。`C-c A b`／`agent-spawn` 也可直接派后台任务。

协作 skill 是 `emacs-agent-collaboration`：按任务提供输入、写入范围、交付文件和
核验要求；OCR 与数学答疑的交接有独立参考。Pi 从 Skill 目录按需读取，Codex 可用
`$emacs-agent-collaboration`。遗失任务 ID 时用 `emacs:agent tasks` 按项目查回，
再读 `result`，不必翻整段聊天或重新派发。

项目 Pi 是手动打开的：`M-x pi`、`C-c A P`，或在聊天里输入 `/pi`。打开它，观察项才开始运行；
关掉它，一切停下，观察项等下次打开。

| 操作 | 入口 |
| --- | --- |
| 看任务图谱、缓存、观察项 | `/cache`、`C-c A g` |
| 取消一个观察项 | 在页面的那一行按 `x` |
| 倒计时后告诉 Pi | `/watch`、`C-c A L`，minibuffer 问时间和内容 |
| 看哪些会话在等你 | `/inbox`、`C-c A G` |
| 看花了多少 | `/usage`、`C-c A U` |
| 查 Pi 的配置 | `/doctor`、`C-c A D` |

这些都在 Emacs 本地执行，不走模型，不花 token。

## 项目知识怎么接续

给项目 Pi 一份新来源或一个变更线索时，可以请它从现有 Wiki 页面中找相关主题，
协调需要更新的几篇页面，并说明依据与未解决的冲突。Pi 按需读取
`noema-project-docs` Skill；它自己能处理的 Markdown 用 Noema 文档接口，
需要核对源码或其他原始材料时交给能读取的执行 Agent。页面逐篇保存或审阅，
Pi 最后说明已更新的范围，便于下一次接续。工作方法见
[项目 Agent 工作流](../noema/guide/research-workflow.md#用新来源维护-wiki)。

## Agent 怎样找 Pi

项目里的 ACP 会话第一次提问时，会收到一个隐藏的入口提示：项目在哪、Pi 存在、自己的 `buffer`
和 `nativeSessionId`。需要时它读 `noema-pi` Skill，然后：

```json
[{"action":"send","root":"<项目根>","prompt":"...",
  "from":{"buffer":"<自己的 buffer>","nativeSessionId":"<自己的 ID>"}}]
```

发给 `noema:api:emacs:pi`。Emacs 核对身份后加上消息头，里面有 Pi 可以直接用的回复调用。Pi 的
回复进入这个 Agent 的会话，开头是 `[From Pi, not the person]`，忙时排队，所以 Agent 发完可以
结束回合。这个开头由 MCP 入口加上，模型省略不了，但它只是文字，不是身份证明。Agent 之间的话
都不算你的批准。这一切都在 Noema 和 Emacs 这边，不改 Codex、Claude 或 pi-acp 的官方代码。

## 把事情留到以后

Agent 把“回合结束后还要发生的事”写成观察项，调用 `noema:api:agent:pi:watch`：

```json
[{"op":"add","root":"<项目根>","agent":"codex","why":"CI 跑完要看",
  "when":{"after":"20m"},
  "do":{"prompt":"看 CI 结果","to":{"buffer":"...","nativeSessionId":"..."}}}]
```

| 想要 | 写法 |
| --- | --- |
| 20 分钟后 | `"when":{"after":"20m"}` |
| 每 10 分钟，共 6 次 | `"when":{"every":"10m","times":6}` |
| 任务 x 提交时 | `"when":{"on":{"kind":"submit","task":"x"}}` |
| 任务 x 能开始时 | `"when":{"on":{"task":"x","phase":"ready"}}` |
| 跑项目脚本并汇报 | `"do":{"script":"scripts/check.sh","report":"pi"}` |

每项最多活一周。消息每个项目 24 小时最多 48 条，因为每条都启动一个模型回合。回执的 `running`
为 false 表示项目 Pi 没开，观察项在等它。

## 多 Agent 的约定

1. 开始多步工作先看 `status`，接着上次停下的地方做。
2. 用 `claim --session <nativeSessionId>` 领取，接手的人才能恢复你的会话。
3. 决定和提交用 `note` 记，不记流水；`submit` 要有证据。
4. 会写同一批文件的并行任务，用 `after` 排序或用 worktree 隔离。
5. 等别人时不空转，用观察项等 `submit` 或 `phase`，或结束回合等 Pi 回复。
6. 评审交给另一个 Agent；轮次用尽由人决定。

规则大多由工具执行，被拒绝时错误信息会说下一步。约定的全文在 pack 的 Skill 里，不要抄到别处。

## 花销

项目 Pi 一个回合的上下文约 3900 token、约 0.001 到 0.002 美元，其中提示词和 Skill 目录内容稳定，可以走提示词缓存。Skill
正文只有用到才读。任务图谱和观察项本身不花 token，只有发给 Agent 的消息才启动模型回合。详细数字
见[这次审计](../records/audits/pi-collaboration-2026-11.md)。

## 排错

| 现象 | 看什么 |
| --- | --- |
| 观察项不触发 | `list` 的 `running`：项目 Pi 要开着。看 `findings` 的 `budget` |
| `findings` 有 `unknown` | Pi 在执行中退出，效果未知，不会重放，要人核对 |
| `findings` 有 `failed` | 看 `last`，修好后重新加 |
| `takes one object` | `args` 要写成 `[{...}]`，不是值的列表 |
| `Session identity no longer matches` | 会话换过了，重新 `list` 取身份 |
| `A script must be a file of this Project` | 脚本要在项目根或 `workspace` 内，软链接按真实路径算 |

能写项目文件的 Agent，能让 Pi 在无人看着时运行项目脚本。把项目里的可执行文件当作代码对待。
