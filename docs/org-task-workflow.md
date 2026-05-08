# Org Task Workflow

这份文档只讲 Org 里的 agenda、TODO 状态和 tags。知识库目录组织看
[org-kb-guide.md](org-kb-guide.md)，Org 总入口看 [org-guide.md](org-guide.md)。

## 入口

- `H-o w`
  打开 `my/org-task-dispatch`，常用状态、规划、移动、标记和视图都在这里。
- `H-o t`
  直接调用 `org-todo`。
- `H-o s` / `H-o d`
  schedule / deadline。
- `H-o w` 里的 `T` / `U` / `K`
  active timestamp / inactive timestamp / calendar。
- `H-o w` 里的 `I` / `O` / `G`
  clock in / clock out / clock goto。
- `H-o r` / `H-o A`
  refile / archive。
- `H-o q`
  手动编辑 tags。
- `C-c a`
  打开 `org-agenda`。
- `org-agenda` 里按 `o`
  打开 Overview / Dashboard。

配置文件是 [lisp/org/init-org-agenda.el](../lisp/org/init-org-agenda.el)。

## 扫描范围和性能

Agenda / tags view / refile 不在后台扫描，也不挂 idle timer。`H-o w` 打开
transient 面板本身也不会扫描文件。只有真正打开 agenda、tags view 或 refile
时，才临时计算一次
`org-agenda-files`。

扫描范围按当前 buffer 的位置决定：

1. 如果当前 buffer / `default-directory` 在 `~/HC/Org/` 下，扫描整个
   `~/HC/Org/`。
2. 如果不在 Org 库下，但当前目录属于一个项目，扫描该项目下的 `.org` 文件。
3. 如果没有项目，扫描当前目录下的 `.org` 文件。

扫描会跳过常见重目录：`.git`、`.cache`、`.direnv`、`.venv`、`node_modules`、
`public`、`ltximg`。

## TODO 状态

状态只表达“事情推进到哪一步”，不要拿它当分类系统。分类、领域和上下文交给 tags。

| State | 用法 |
|-------|------|
| `TODO` | 收进系统但还没确定下一步。 |
| `NEXT` | 已经明确下一步，可以直接做。 |
| `WIP` | 正在做，适合今天或当前上下文保持可见。 |
| `WAIT` | 等别人、等结果、等外部事件。切到这个状态会记录 note。 |
| `HOLD` | 暂停，不是废弃，只是现在不推进。切到这个状态会记录 note。 |
| `REVIEW` | 初稿、实现或证明已经有了，需要检查、润色或验算。 |
| `DONE` | 完成。 |
| `CANCELLED` | 取消。切到这个状态会记录 note。 |
| `DROPPED` | 主动放弃或不再值得维护。 |

推荐流：

```text
TODO -> NEXT -> WIP -> REVIEW -> DONE
             -> WAIT
             -> HOLD
             -> CANCELLED / DROPPED
```

## 快速工具

`H-o w` 里分四组：

- State
  一键设置 `TODO`、`NEXT`、`WIP`、`WAIT`、`HOLD`、`REVIEW`、`DONE`、
  `CANCELLED`、`DROPPED`。
- Plan / Time
  设置 schedule、deadline、timestamp、calendar、clock、effort、priority、property、
  refile、archive。
- Markers
  一键 toggle 常用标记：`confusion`、`shush`、`blocked`、`followup`、`review`。
- Views
  打开 overview agenda，以及 `confusion`、`shush`、`blocked`、`followup`、
  `review` 的 tags view。

这些工具里，TODO 状态、tags、schedule、deadline、priority、property、refile、
archive 和 clock 都是 Org heading 级别的元数据，所以会作用在当前标题上。这是
Org 的原生模型，不是额外扫描或后台逻辑。`T` / `U` 是例外：它们只在当前位置插入
active / inactive timestamp。

## Tags

Tags 使用英文，目标是让标题可以横向筛选。建议每条笔记控制在 3 到 5 个 tags。

### 状态和处理

| Tag | 用法 |
|-----|------|
| `inbox` | 未处理输入。 |
| `draft` | 初稿，还没整理。 |
| `working` | 正在维护。 |
| `review` | 需要复查。 |
| `stable` | 已经相对稳定。 |
| `blocked` | 被外部条件卡住。 |
| `followup` | 需要后续动作。 |
| `confusion` | 困惑点，不确定、没想通、证明或概念有缺口。 |
| `shush` | 值得记住的地方。这个名字保留一点玩笑感，用来标记“别丢，之后要回看”。 |
| `question` | 明确的问题。 |

### 笔记类型

- `idea`
- `concept`
- `method`
- `definition`
- `theorem`
- `proof`
- `example`
- `counterexample`
- `summary`
- `paper`
- `book`
- `dataset`
- `experiment`

### 活动上下文

- `research`
- `learning`
- `writing`
- `teaching`
- `work`
- `dev`
- `meeting`
- `project`
- `reading`

### 领域

- `math`
- `tcs`
- `cs`
- `qc`
- `crypto`
- `complexity`
- `algorithm`
- `logic`
- `philosophy`
- `physics`

## 用法建议

- 学术阅读：
  `paper:research:draft`，困惑处再加 `confusion`，关键定义或 trick 加 `shush`。
- 证明开发：
  `proof:tcs:working`，卡住时加 `blocked` 或 `confusion`，证明完成后状态切到
  `REVIEW`。
- 开发任务：
  `dev:project:working`，明确下一步用 `NEXT`，等待外部依赖用 `WAIT`。
- 课程学习：
  `learning:course:draft`，需要复习用 `review`，值得背的点用 `shush`。
- 工作会议：
  `meeting:work:followup`，会后 action item 设成 `TODO` 或 `NEXT`。

## 维护原则

- TODO state 只表示推进状态。
- Tag 表示性质、领域、上下文和轻量标记。
- `confusion` 是主动管理疑问，不是负面标签。
- `shush` 是“值得记住”，比 `important` 更轻，适合快速标记。
- `DONE` 后如果内容仍值得沉淀，补 `stable` 或移进 roam。
