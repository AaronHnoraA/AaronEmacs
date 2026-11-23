# NocoDB 研究：数据库、表格手感与 AI 一体化

对象：`upstream/`，NocoDB 浅克隆 `eabb8b1`（2026-11-23），被 `.gitignore` 的
`/docs/works/*/upstream/` 忽略。本页是工作台，不是权威文档；定下来的结论已经进入代码和
下文列出的文档。

## 先说结论

NocoDB 值得学的是**视图与数据分离的数据库手感**：同一张表有多个视图，每个视图各自
排序、过滤、分组、隐藏列、按列聚合，都不动数据；Enter 编辑、Space 展开整条记录、
Delete 清空、方向键按格移动。它的 AI、工作流、文档、Agent 在公开仓库里大多是空壳
（见“社区版与闭源边界”），能学的只有类型设计和交互思路。

Noema 已经有同一种“视图”：`#+begin av` 属性视图是对 Markdown 事实的查询，由 Go kernel
求值，单元格修改写回源文件。所以本轮没有把 NocoDB 的数据库搬进来，而是做了三件
符合 Noema/Emacs 根的事：

1. **Emacs 原生属性视图** `noema-attribute-view`（`noema-menu` `!v`）：同一个
   `#+begin av` 在 Emacs buffer 里画成表，`RET` 跳源、`e` 改格。求值和写回走页面已在
   用的两个 host 通道，没有新后端、没有第二个求值器。
2. **CSV 表格视图** `+csv/grid`（csv-mode 里 `SPC m g`）：CSV buffer 是表，grid 是视图。
   按 NocoDB 表格的手感做了键位、过滤、排序、分组、隐藏列、聚合栏和记录表单；编辑是对
   CSV 文本的原位修改，撤销和保存都是 Emacs 原生的。
3. **顺手修正**：clutch 查询控制台原先落在配置根目录 `clutch/`（还被 git 跟踪），现在
   放在 `var/clutch/`。

## 红线怎么守

| 红线 | 做法 |
| --- | --- |
| Noema 哲学：Markdown 是源，视图不发明源里放不下的形式 | 原生属性视图只读 host 的答案；写只经 `attribute-view-cell-patch`，它改 Markdown。CSV 视图同理：唯一的数据是 CSV 文本。 |
| 一种语法只有一个解析器 | 属性视图的 spec 原样发给 host，由 Go `kernel/noema/attributeview` 求值，Elisp 不解析过滤/排序语法。CSV grid 的过滤是表格 UI 操作，不是 Noema 语法，不与 av 冲突。 |
| Emacs-native 演化 | 页面里才有的表格，现在 Emacs 里也有；CSV 原来只有对齐显示，现在有数据库式视图。两者都是普通 special-mode buffer，Evil 初始状态为 emacs，`q` 关闭视图不影响数据。 |
| Noema 已调优的部分保持原样 | 页面的 av widget、Go 求值器、写回路径、Agenda 一行未改。 |
| 性能 | 见下面“性能”。 |
| 不引入第二套数据库客户端 | 外部数据库仍用 clutch（已有暂存编辑、外键跳转、记录视图），CSV 视图只补 CSV 这一个缺口。 |

## NocoDB 代码地图（审计范围）

pnpm + lerna monorepo，Sustainable Use License（源码可见，限制商用；不是 AGPL）。

| 包 | 内容 |
| --- | --- |
| `packages/nocodb` | NestJS + Knex 后端，约 1400 个 ts 文件 |
| `packages/nc-gui` | Nuxt 3 / Vue 3 / Pinia 前端，约 1760 个 ts/vue 文件 |
| `packages/nocodb-sdk` | 前后端共用：类型、公式解析、聚合、工作流/Agent/Chat 类型 |
| `packages/noco-integrations` | AI、同步等集成插件 |

### 数据库与数据管理

- **元数据和数据分开**：表/列/视图/过滤/权限存在 NocoDB 自己的 meta 库
  （`meta/meta.service.ts`，迁移在 `meta/migrations/v0..v2`）；用户数据留在原来的
  PG/MySQL/SQLite（Source），连接由 `NcConnectionMgrv2` 管。`meta-sync` 任务在外部
  schema 变化后把元数据同步回来。
- **缓存**：`cache/NocoCache.ts`，Redis 或进程内 mock；key 是
  `nc:noco:{workspace}:{base}:{scope}:{id}`，按 workspace/base 天然隔离。模型类
  （`models/*.ts`，约 100 个）一律先查缓存再落 meta 库。
- **一次列表请求**：`data-table.controller → data-table.service.dataList →
  BaseModelSqlv2.list()`：选列 → 解析 sort/where → 搜索 → 行级权限（RLS）条件 →
  视图过滤 → 分页 → Knex 生成 SQL。Link/Lookup/Rollup 编译成子查询或 CTE 拼进同一条
  SQL（`generateLookupSelectQuery.ts`、`genRollupSelectv2.ts`、`cte-generator/`）。
  `BaseModelSqlv2` 主文件加子目录约 2.3 万行。
- **字段类型策略**：`db/field-handler/handlers/` 每种类型一个目录，负责该类型的过滤、
  排序、序列化，必要时按方言分文件。46 种 UIType。
- **公式**：jsep 解析成 AST（sdk `lib/formula/`，前端也要用来校验），后端
  `formulav2/formulaQueryBuilderv2.ts` 编译成 SQL，`functionMappings/` 按方言
  （pg/mysql/sqlite/mssql/oracle/databricks）实现 91 个函数。
- **保护性设施**：`db/queryGuard.ts` 按 async 链限定一次请求能跑的 SQL；
  `helpers/coalescer.ts` 把同一主体的多次触发合并成一次 flush；`db/search/` 是独立的
  搜索索引（构建队列、scope、live 更新）。

**对 Noema 的启发**

| NocoDB | Noema 现状 | 判断 |
| --- | --- | --- |
| meta 库 + 用户库，meta-sync | 文件是唯一事实，`wiki.db` 是可丢弃索引 | Noema 的模型更适合一个人的 Markdown 知识库；不引入 meta 库。 |
| 视图是表上的查询，不改数据 | `#+begin av` 是 Markdown 事实上的查询 | 已一致。缺的是 Emacs 里的视图，本轮补上。 |
| 字段类型策略（每类型一个 handler） | `ATTRIBUTE_VIEW_FIELD_TYPES` + Go `compareTypedValues` | 已有同样的结构，规模小，不必拆目录。 |
| 公式编译成 SQL | av 的 `template` 列、`calc` | 不引入 SQL 公式；数据不在 SQL 里。 |
| 缓存 key 带 workspace/base 前缀 | kernel 的 LRU 与 generation 失效（Knowledge Dock 等） | 已一致。 |
| queryGuard / coalescer | 各处有界 LRU、去抖 | 思路可在新 host 端点里复用：凡是“触发多次、做一次”的通知走合并。 |
| SiYuan 遗留的 `kernel/av` JSON 数据库（MCP `database` 工具、CLI `database`） | 代码在，但数据存在 JSON 里不在 Markdown 里 | **继续不启用**：违背 Markdown-first。 |

### 可视化表格与操作手感

- 表格是 canvas 绘制（`components/smartsheet/grid/canvas/`，`index.vue` 4841 行）：
  数据分块取（`CHUNK_SIZE 50`、`BUFFER_SIZE 100`、`PREFETCH_THRESHOLD 50`），重绘用
  `requestAnimationFrame` 合并（`loaders/ActionManager.ts`），单元格 Markdown 先异步
  解析进 1000 项 LRU 再画（`loaders/markdownLoader.ts`），未完成的解析不重复发起。
- 键位（`composables/useKeyboardNavigation.ts`）：Enter 编辑，Space 展开记录，
  Delete/Backspace 清空（多选时清范围），Cmd-C 复制，Alt-R 加行，Alt-C 加列，Tab 和方向键
  移动，Cmd+方向键到边，Shift 扩选，输入法组合中不抢键。
- 视图：Grid、Gallery、Kanban、Form、Calendar、Map 开源；List、Timeline、Gantt 在
  社区版是空壳。工具栏是 Fields（隐藏/排序列）、Filter、Sort、Group、Row height、
  Row color；底部聚合栏按列选择聚合函数。

**CSV grid 采用的部分**（`lisp/lang/filetypes/csv-grid.el`）

| NocoDB | Emacs grid |
| --- | --- |
| 方向键/Tab 按格移动，当前格高亮 | `←→ TAB S-TAB` 按格，`↑↓ n p j k` 保持列换行；当前格用 overlay 高亮 |
| Enter 编辑 / Space 展开 / Delete 清空 / Cmd-C 复制 | `RET`/`e` 编辑，`SPC` 记录表单，`DEL` 清空，`w`/`M-w` 复制 |
| Alt-R 加行 | `+` 加行并直接编辑首格；`D` 删行（确认） |
| 撤销（社区版 `useUndoRedo` 是空实现） | `u` 在 CSV buffer 里 `undo`；所有编辑都是文本编辑，天然可撤销 |
| Filter / Sort / Group / Fields | `/` 叠加过滤（`~ !~ = != > < >= <= empty filled`，数字列按数值比较），`\` 清除；`s` 升/降/取消；`G` 分组；`h` 隐藏列，`H` 全部显示 |
| 聚合栏 | `a` 循环本列聚合：count、unique，数字列另有 sum/avg/min/max；分组时每组一行 |
| 展开记录 | 记录表单 buffer，一字段一段，`RET` 改字段，`n`/`p` 按视图顺序换记录 |
| 冻结表头 | `header-line` 显示列名，随横向滚动 |
| 工具栏 | `?` 打开 transient 菜单（Cell / Row / View / Columns 四组） |

没有采用：canvas 绘制、分块网络取数（本地文本无需）、行颜色条件、多选范围操作。

### Markdown 与渲染

- NocoDB 的 Markdown 只用在富文本单元格：tiptap（ProseMirror）编辑，markdown-it 序列化
  （`helpers/tiptap-markdown/`，改编自 MIT 的 tiptap-markdown），提及写成
  `@(id|email|name)`。Documents 是 ProseMirror JSON + Yjs 协作（`meta/migrations/docs-content`），
  模型在社区版是空壳。
- 对照：Noema 的文档就是 Markdown 源本身，CM6 `EditorState` 是唯一编辑权威，渲染构件
  只是源的视图。ProseMirror JSON 的路线与 Noema 的根相反，不学。
- 可学的是渲染缓存的形状：按 key 的有界 LRU、同 key 解析在途时不重复发起、全部落定后
  只触发一次重绘。Noema 的 `math-render.ts` 缓存已是同类做法；若将来 av widget 要把单元
  格渲染成内联 Markdown，应按这个形状接 `renderInlineMarkdownHTML`。

### AI 一体化、工作流与 Agent

社区版只有类型和前端，后端模型是空壳（下一节），以下来自 `nocodb-sdk` 类型和 `nc-gui`：

- **AI 列**（`models/AIColumn.ts`）：一列绑定集成和模型，prompt 里用 `{列名}` 引用
  同一行的字段（存为列 id，改名后经 `substituteColumnIdWithAliasInPrompt` 还原），
  按行生成值。另有 AI 建表、建视图、预测下一列/公式（`lib/ai/index.ts` 的序列化类型）。
- **工作流**（`lib/workflow/`）：节点分 trigger/action/flow 三类，带端口、表单定义和
  触发方式（none/webhook/cron/polling）；画布用 vue-flow。执行结果模型很细：每个节点有
  status、input/output、logs、`attempts`（每次失败的尝试）、`retryable`、`retryAfterMs`、
  `deferred`（资源满而未开始，不算重试）、`simulated`（只检查未执行）、`needsRun`
  （只能靠真正执行来检查，与“坏了”区分开），循环有逐次迭代记录；执行可在节点边界
  暂停或取消。
- **Agent / Chat**（`lib/agent/`、`lib/chat/`）：Agent 带角色上限（commenter/viewer）、
  触发器、Web/计算工具；编码 Agent 按 `nocodb-agent/{slug}` 开分支、提 PR、指定评审人；
  Chat 有工具调用状态、会话状态和“factory”任务列表。MCP 按 base 发 token。

**对 Noema 的启发**

| NocoDB | Noema 对应 | 判断 |
| --- | --- | --- |
| AI 列直接写值 | Agent 改文档走 `agent:document:edit` / `propose`（ediff 卡片，人确认） | 保持 Noema 的“可审阅提案”。若要“让 Agent 填一列”，应写成对属性块的 ediff 提案，而不是直接写入。 |
| 可视化工作流画布 | Pi watches（`agent:pi:watch`）+ 任务图谱 `.agent/tasks/`，都是 Markdown/日志 | 不引入画布（一个人、Markdown-first）。 |
| 节点结果的 `needsRun` / `simulated` / `retryable` / `attempts` | Pi watch 触发记录、任务图谱 journal | 值得学：失败要区分“暂时的、可重试”“需要人来跑”“确实坏了”，并保留每次尝试。这应作为 `pi-watches.md` / `project-graph.md` 的待定设计由人决定，本轮不改。 |
| Agent 角色上限 | ACP 权限模式、`agent-management` 的所有权契约 | 已有；更强（Emacs 持有执行）。 |
| 编码 Agent 开分支提 PR | `tools/agent` worktree | 已有。 |
| MCP 按 base 发 token | kernel MCP 服务 | 已有，单人本机无需 token 体系。 |

一体化思路本身值得记下：NocoDB 把数据（表）、视图、自动化（工作流）、AI（列/Agent）
放在**同一个对象模型**上，AI 和工作流引用的都是“列/表/视图”的 id。Noema 的对应物是
Markdown 里的稳定身份（`{#UUIDv7}` 块 id、属性块、`@@todo`）：Agent、Agenda、属性视图、
任务图谱都引用这些身份。继续往这个方向走，就能在不引入数据库的前提下得到同样的一体化。

## 社区版与闭源边界

以下后端模型在公开仓库里是空壳（`static async get(..._args) { return null }` 之类）：
Agent、ChatSession、ChatMessage、Workflow、Script、Dashboard、Widget、Document、
Interface/InterfacePage、App/AppVersion、Gantt/Timeline/List 视图、Bookmark、
各类 Section、Workspace/WorkspaceUser。前端的撤销/重做 `useUndoRedo` 也是空实现。
因此“抄代码”只适用于表格交互、Markdown 序列化等真正开源的部分；许可证是
Sustainable Use License，若要照搬代码需逐文件核对（`helpers/tiptap-markdown/` 是 MIT）。
本轮没有照搬任何 NocoDB 代码，只借了交互设计。

## 性能

CSV grid（10 万行、4.1 MB、每 10 行一个带引号的字段，batch Emacs 实测）：

| 操作 | 耗时 |
| --- | --- |
| 解析 | 0.28 s |
| 打开（解析 + 推断类型 + 绘制） | 0.59 s |
| 按数字列排序 | 0.94 s（先算排序键再排序；逐次比较时是 2.9 s） |
| 过滤 | 0.70 s |
| 编辑非排序列 | 0.10 s（只重读该记录，后续记录起点平移，沿用上次行序） |
| 编辑排序列 | 0.97 s（需要重排） |

边界：最多画 `+csv-grid-max-rows`（2000）行，超过时提示过滤或分组；超过
`+csv-grid-max-bytes`（30 MB）拒绝打开；列宽上限 40 字符。没有用 vtable，因为它为每个
单元格算像素宽度，2000×10 格就是两万次 `string-pixel-width`。CSV buffer 变化后，可见的
grid 在 0.3 s 空闲后重绘；最后一个 grid 关闭时摘掉 CSV buffer 上的钩子和计时器。

属性视图的计算全部在 host；Elisp 只画结果（行数受 spec 的 `limit` 约束，默认 50），
旧请求的答案按 generation 丢弃。

## 验证

- `site-lisp/noema/tests/elisp/noema-attribute-view-tests.el`：6 项（找块跳过代码围栏、
  spec 原样发送、画行/诊断/计算、丢弃过期答案、kanban 分组、编辑经 host 写回后重新求值）。
- `test/csv-grid-tests.el`：12 项（引号/换行/空行解析、按需加引号、类型推断、数字排序空值
  垫底、过滤叠加且不改源、分组与聚合、编辑写回、拒绝过期记录、增删行、按格移动、
  隐藏列、关闭最后一个视图后摘钩子）。

## 留给人决定的

- av 是否增加 `source: csv FILE`，让 Markdown 里的视图直接查询 vault 中的 CSV。这要改
  `shared/attribute-view.mjs`、Go 求值器和 fixture，属于 `syntax.md` 的新形式。
- Pi watch 与任务图谱是否采用 NocoDB 式的失败分类（retryable / needsRun / deferred）和
  逐次尝试记录。
- “让 Agent 填一列”是否作为 `agent:document:propose` 的一种：对一个 av 的某列，为每行
  生成属性块 ediff 卡片。
