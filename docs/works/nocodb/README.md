# NocoDB 研究：数据库、表格手感与 AI 一体化

对象：`upstream/`，NocoDB 浅克隆 `eabb8b1`（2026-11-23），被 `.gitignore` 的
`/docs/works/*/upstream/` 忽略。本页是工作台，不是权威文档；定下来的结论已经进入代码和
下文列出的文档。三份附表：[inventory.md](inventory.md) 是 NocoDB 每个目录的读法与结论；
[nocodb-files.md](nocodb-files.md)（`nocodb_files.py` 生成）读入并检查了 NocoDB 全部 3508 个源文件
（约 78 万行）的行数、导出、头注释与是否为社区版空壳，找出 48 个空壳，证实 Agent、Chat、Workflow、
Script、Dashboard、Document 等后端与前端 Chat/撤销都是空实现；[noema-coverage.md](noema-coverage.md)
（`noema_coverage.py` 生成）是 Noema 全部 542 个源文件的测试与文档覆盖。

## 先说结论

NocoDB 值得学的是**视图与数据分离的数据库手感**：同一张表有多个视图，每个视图各自
排序、过滤、分组、隐藏列、按列聚合，都不动数据；Enter 编辑、Space 展开整条记录、
Delete 清空、方向键按格移动。它的 AI、工作流、文档、Agent 在公开仓库里大多是空壳
（见“社区版与闭源边界”），能学的只有类型设计和交互思路。

Noema 已经有同一种“视图”：`#+begin av` 属性视图是对 Markdown 事实的查询，由 Go kernel
求值，单元格修改写回源文件。所以本轮没有把 NocoDB 的数据库搬进来，而是做了几件
符合 Noema/Emacs 根的事：

1. **Emacs 原生属性视图** `noema-attribute-view`（`noema-menu` `!v`）：同一个
   `#+begin av` 在 Emacs buffer 里画成表，`RET` 跳源、`e` 改格。求值和写回走页面已在
   用的两个 host 通道，没有新后端、没有第二个求值器。
2. **CSV 表格视图** `+csv/grid`（csv-mode 里 `SPC m g`）：CSV buffer 是表，grid 是视图。
   按 NocoDB 表格的手感做了键位、过滤、排序、分组、隐藏列、聚合栏和记录表单；编辑是对
   CSV 文本的原位修改，撤销和保存都是 Emacs 原生的。
3. **Markdown 表格视图** `+markdown/table-grid`（markdown-mode `SPC m g`，`noema-menu`
   `!t`）：笔记里的管道表格就是一张小表，用同一个 grid 视图排序、过滤、分组、汇总，编辑
   直接写回表格行。CSV 和 Markdown 表格共用一份视图实现，只有“读一条记录、编码、加删行”
   按源格式分派。
4. **顺手修正**：clutch 查询控制台原先落在配置根目录 `clutch/`（还被 git 跟踪），现在
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
- **数据管理服务**（`services/`）：SmartText 写入回传内容哈希做乐观并发（`expectedMdHash`）；
  `duplicate-detection` 按列找重复值；`dependency` 在改列、删列前列出会被破坏的依赖；
  `formula-column-type-changer` 与 `column-data-backup-handler` 改类型前先建带撤销的备份列；
  `link-placeholder` 删除链接列时换成装着显示值的文本列；`meta-diffs` 发现外部 schema 漂移并同步。
  共同点是“破坏性改动先保住数据、并说清会影响谁”。
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
| 乐观并发（内容哈希） | Agenda/属性视图写回的 `expectedRevision`、块属性版本校验 | 已一致。 |
| 重复值检测 | — | **已采用**：grid 的 `duplicates` 聚合。 |
| 改动前报告依赖；改类型先备份；删链接列留文本 | 笔记改名/删除与反向链接、wanted pages | 原则可借：Noema 删除或改名会断链的操作，先列出反向链接。属于 `wiki-workspace.md` 的待定设计。 |
| `meta-diffs` 外部漂移同步 | 文件监视与外部修改检测（`current-architecture.md`） | 已一致。 |
| SiYuan 遗留的 `kernel/av` JSON 数据库（MCP `database` 工具、CLI `database`） | 代码在；`database` 工具随 kernel MCP 注册。`model/markdown_native_guard.go` 拒绝一切指向 Markdown 笔记本块的 AV 写操作（含全部镜像块的预检），只剩不挂块的兼容视图写在 kernel 自己的 `storage/av` | **不启用为 Noema 的数据库**：JSON 不在 Markdown 里。现有防护足够，不改代码。 |

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
| Enter 编辑 / Space 展开 / Delete 清空 / Cmd-C 复制 | `RET` 编辑，`e` 按纯文本编辑，`SPC` 记录表单，`DEL` 清空，`w`/`M-w` 复制 |
| 复选框单击翻转；单选列下拉 | 勾选列 `RET` 直接翻转并保留大小写，画成 ☑/☐；单选列 `RET` 补全该列已有的值（仍接受新值） |
| CSV 导入的类型探测（`jobs/data-import/csv-type-detector.ts`） | 借用其规则：成对布尔写法表（去掉 1/0，让给数字列）；单选 = 不超过 64 种取值且不超过非空格数的一半。注意上游当前版本的 `detectColumnTypes` 已关闭探测、一律返回文本，这里只借规则 |
| Alt-R 加行 | `+` 加行并直接编辑首格；`D` 删行（确认） |
| 撤销（社区版 `useUndoRedo` 是空实现） | `u` 在 CSV buffer 里 `undo`；所有编辑都是文本编辑，天然可撤销 |
| Filter / Sort / Group / Fields | `/` 叠加过滤（`~ !~ = != > < >= <= empty filled`，数字列按数值比较），`\` 清除；`s` 升/降/取消；`G` 分组；`h` 隐藏列，`H` 全部显示 |
| 聚合栏 | `a` 循环本列聚合：count、unique、duplicates，数字列另有 sum/avg/min/max；分组时每组一行 |
| 展开记录 | 记录表单 buffer，一字段一段，`RET` 改字段，`n`/`p` 按视图顺序换记录 |
| 冻结表头 | `header-line` 显示列名，随横向滚动 |
| 工具栏 | `?` 打开 transient 菜单（Cell / Row / View / Columns 四组） |

Markdown 表格作为源（同一文件里的适配层）：表格范围用 marker 记住，跳过分隔行；单元格按
未转义的 `|` 切分（GFM 规则：`\\|` 是转义的反斜杠加真正的分隔符）；写回把 `|` 写成 `\|`、换行写成
`<br>`。在 Noema 页面上，按 AGENT.md 的规则在笔记的普通源 buffer 上工作；视图自己打开的
源 buffer 每次改动立即保存（页面经文件监视重读），最后一个视图关闭时关掉它；用户自己开着的
源 buffer 只改不存。

没有采用：canvas 绘制、分块网络取数（本地文本无需）、行颜色条件、多选范围操作。

### Markdown 与渲染

NocoDB 里 Markdown 出现在三处，用了两套解析器：

| 场景 | 实现 | 要点 |
| --- | --- | --- |
| 富文本单元格的编辑 | tiptap（ProseMirror）+ `helpers/tiptap-markdown/`（改编自 MIT 的 tiptap-markdown） | 编辑时是 ProseMirror 文档，保存时序列化回 Markdown；提及写成 `@(id\|email\|name)` |
| 富文本的 DOM 渲染（展开格、表单、邮件预览） | `NcMarkdownParser`（`helpers/tiptap/functionality/markdown/markdown.ts`）：markdown-it + task-lists + 链接规则 + 提及，再经 DOMPurify | 单例；`patchedRender` 按 `maxBlockTokens` 只渲染前 N 个块（块栈配平后才截断），用于预览；清洗配置禁掉 `form/button/select/textarea/style/iframe/object/embed/img/…/dialog` 和 `style/id/srcset/formaction/popover*` 等属性，class 只留渲染器自己产生的几种，`input` 只留只读复选框 |
| canvas 表格里的单元格 | `components/smartsheet/grid/canvas/utils/markdownUtils.ts`：手写的 inline 标记分词器 | 只认 `** * _ ~~ <u> <s>`、链接、`\` 转义和 `@(…)` 提及；没有代码 span 规则，反引号原样显示。解析结果进 1000 项 LRU（`loaders/markdownLoader.ts`），同 key 在途不重复解析，全部落定后触发一次重绘 |
| Documents（EE） | ProseMirror JSON + Yjs（`meta/migrations/docs-content`：`yjs_state`、`doc_revisions`） | 社区版模型是空壳 |

对照 Noema：

- **一种语法一个解析器**是 Noema 的硬规则（`syntax.md` 开头；`shared/*.mjs` + fixture 钉住 Go 端）。
  NocoDB 的 DOM 渲染和 canvas 渲染对同一段 Markdown 给出不同结果（代码 span 就是例子），正是
  这条规则要防的分叉。不学。
- **文档模型**：ProseMirror JSON / Yjs 把文档变成编辑器私有的结构，Noema 的文档就是 Markdown
  源，CM6 `EditorState` 是唯一编辑权威，渲染构件只是源的视图（`rendered-constructs.md`）。不学。
- **清洗策略**：学了一部分。对照后发现 Noema 的 `sanitizeEmbeddedHtml` 允许 `<form>`、
  `formaction`、`popover*` 和 `<dialog>`，而笔记里的 HTML 也来自网页摘录、导入和 Agent，
  页面又带宿主桥接能力。已收紧（见“Noema 审计”）。`style`、`id` 和普通按钮保留，因为笔记
  的 HTML 块会用到。
- **有界预览**：`maxBlockTokens` 的“块栈配平后再截断”是个干净的做法。Noema 的链接悬停、
  搜索摘录目前用的是文本截取；若将来要做“只渲染前几块”的富预览，应在 `src/render-html.ts`
  里以同样方式在 token 层截断，而不是截字符串。
- **渲染缓存形状**：有界 LRU、在途去重、落定后一次重绘。Noema 的 `math-render.ts` 已是同类
  做法；若 av widget 要把单元格渲染成内联 Markdown，应按这个形状接 `renderInlineMarkdownHTML`。

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
- **表达式与可视化编辑**（`lib/workflow/expressionCatalog.ts`、`transforms.ts`）：变量写成
  `{{ $("Node").fields.Title }}`，可调用的方法是白名单（目录与执行器的白名单由
  `expressionCatalog.test.ts` 保持一致）。可视化的“变换芯片”（大写、截取、加日期等）每个都有
  `apply`（套一层）和 `peel`（从最外层剥回参数），所以芯片只是表达式文本的视图，保存的永远是
  普通表达式；参数里的花括号会被去掉，免得提前结束 `{{ }}`。画布上的“加号节点”只属于 UI，
  保存前由 `filterOutPlusNodes` 过滤（`utils/workflowGraphUtils.ts`）。
- **步骤引用、改名与测试新鲜度**（`nc-gui/utils/workflowUtils.ts`）：步骤按标题引用
  （`$('标题')`），改标题时 `updateVariableReferences` 重写所有配置中的单、双引号引用；
  `getRunStepReferenceIssues` 报出引用了不存在的步骤，或从循环外引用循环体里的步骤（每个元素各有
  一个结果）；步骤的测试结果有新鲜度（未测、过期、失败），上游没有当前有效的通过结果时下游不能测
  （`findBlockingParentNodes`）。
- **MCP**（`nocodb/src/mcp/`）：13 个工具（库信息、表清单、表结构、查询、读、计数、附件、聚合、
  增改删、关联/解除）。输入 schema 严格；过滤先编译再校验（`filter-compile`、`filter-verify`）；
  `read-echo` 让“读出、改一处、整条写回”可行：只读键丢弃，不可写键若被改则拒绝，“对不可写字段的
  修改永远不会显示成功”；工具描述写全查询语法（`descriptions.ts`）。
- **节点运行契约**（`noco-integrations/core/src/workflow-node/types.ts`）：节点拿到
  `inputs.config`、`testMode`、取消用的 `AbortSignal`、按 id 取集成的 `getIntegration`，以及
  `htmlInputPaths`：哪些配置路径的模板是富文本、插值时做过 HTML 转义，由模板一次决定，节点
  不得从插值结果反推，否则一条记录的内容就能翻转转义模式。
- **AI 集成**（`noco-integrations/core/src/ai/types.ts`）：一个集成抽象，按“活动”
  （`AiUseCase`）路由模型：聊天路由器、建表、数据问答、支持、页面专职 Agent，会话标题、
  历史压缩、回合末总结（写给后续回合做上下文）、建议芯片，公式、Cmd+K 理解、文档写作/
  总结/翻译、补全、嵌入。用户明确选了模型的功能（AI 字段、AI 按钮、工作流 AI 节点）直接传
  `customModel`，不走路由。底层是 Vercel AI SDK，可选的 devtools 中间件记录每一次调用；
  usage 字段供计费。
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
| 表达式是源，芯片是视图（`apply`/`peel` 可逆） | `syntax.md`：视图不发明源里放不下的形式，编辑器写回读到的形式 | 同一条原则，NocoDB 在工作流编辑器里做到了。可作为 Noema 构件的检查方法：每个可视化编辑都应能从源“剥”回来。本轮实测发现 Noema 的规划属性写回会把 `{prio: B}` 改成 `{prio=A}`，已记为 `agenda-dsl.md` G1。 |
| 只属于 UI 的节点保存前过滤 | 渲染构件不写回文档（rendered-constructs 契约） | 已一致。 |
| `htmlInputPaths`：转义模式由模板一次决定 | Agent 上下文与 ediff 回执 | 原则可借：一个值是否转义、是否可信，应由声明它的地方决定，不从内容推断。 |
| 按活动路由模型（`AiUseCase`） | 各 Agent 由 provider 自己选模型；Pi、Copilot、LaTeX 导出各自配置 | 值得考虑：把“标题、总结、压缩这类无人值守活动用便宜模型，构建类用强模型”收成一张表。属于 `agent-management.md` 的待定设计。 |
| 回合末总结写给后续回合 | Hy Memory、交接与任务图谱 journal | 已有，且落在项目 Markdown 里，更合 Noema 的根。 |
| devtools 记录每次调用 | ACP 事件、Agent Manager 的 Usage 页 | 已有，数据来自协议事件而非渲染（AGENT.md）。 |
| 步骤按标题引用，改名时重写引用 | 笔记链接、块引用 `((id))` | Noema 用稳定 id 引用块，不怕改名，比按标题更稳；按标题的 `[[链接]]` 改名重写由 `noema-move-note` 负责。 |
| 测试结果新鲜度与上游阻塞 | Research 的冻结 RunSpec、任务图谱依赖 | 已有同类机制；“过期”状态可借来表达上游变化后的结论复核。 |
| MCP 读回配对：不可写字段被改则拒绝 | `agent:document:edit` / `propose`、C24“说清楚实际失败了什么” | 同一原则。可作为 Noema `agent:*` 写入工具的检查项：写入请求里改了不可写的部分，要明确拒绝而不是静默忽略。 |
| 描述写全语法 | `resources/agent/tools.md`（C26） | 已有。 |

一体化思路本身值得记下：NocoDB 把数据（表）、视图、自动化（工作流）、AI（列/Agent）
放在**同一个对象模型**上，AI 和工作流引用的都是“列/表/视图”的 id。Noema 的对应物是
Markdown 里的稳定身份（`{#UUIDv7}` 块 id、属性块、`@@todo`）：Agent、Agenda、属性视图、
任务图谱都引用这些身份。继续往这个方向走，就能在不引入数据库的前提下得到同样的一体化。

### 其余模块

| 模块 | 内容 | 对 Noema |
| --- | --- | --- |
| Webhook（`Hook`、`HookFilter`、`HookLog`） | 表事件触发 HTTP，带过滤条件和调用日志 | Pi watches 已覆盖“事件 → 动作”，单人本机不需要出站 webhook。 |
| 导入导出（`jobs/at-import`、`data-import`、`data-export`，`extensions/data-exporter`） | Airtable 导入、CSV/Excel 导入、导出 | CSV 就地成为表（`+csv/grid`），无需导入；导出由 Markdown/CSV 文本本身承担。 |
| 评论与反应（`Comment`、`CommentReaction`） | 按行评论 | Noema 有锚定在文本里的评论（`.noema-comments.jsonl`，C70/C80），不新增。 |
| 审计（`Audit`、`meta/audit.service.ts`） | 每次修改留痕 | Git 历史就是审计。 |
| 行颜色（`RowColorCondition`） | 按条件给行或单元格上色 | 未采用；可作为 grid 的后续想法，按 `face` 而非颜色值实现。 |
| 权限、RLS、角色可见性（`Permission`、`ModelRoleVisibility`、`getRlsConditions`） | 多用户 | 单人设计，不需要（见 Markdown-first、单人原则）。 |
| 共享视图、公开表单（`public-datas.controller.ts`） | 对外分享 | 发布走 `make publish*` 的个人站点，不新增。 |
| 在线状态（`RecordPresenceBadge`） | 多人同时编辑提示 | 单人，不需要。 |

## Noema 审计

### 全量测试（2026-11-24，工作区含其它任务的未提交改动）

| 套件 | 结果 |
| --- | --- |
| `npm test`（前端、Node host、架构守护测试） | 373 个文件、3833 项通过，17 项原本标记为跳过 |
| `make kernel-test`（新增，`-tags fts5`） | `kernel/noema/...` 与 `kernel/mcp/...` 共 16 个包通过 |
| `tests/elisp/` 18 个文件 | 约 230 项全部通过（需完整配置的 7 个文件在完整配置下通过） |
| 实机：真实 Noema host + Go kernel，batch Emacs 加载完整配置 | 属性视图在 vault 范围走 `kernel-attribute-view`，28 条 todo 中画出前 5 条，计数栏正确；在 scratch 笔记上改格写回文件并重新求值 |
| 实机：独立 daemon（完整配置、真实事件循环、tty frame） | grid 显示在窗口里时，外部给 CSV 加一行，0.3 s 空闲重绘后 grid 读到第 3 行且 tick 对齐；Markdown 表格 grid 写回表格行；`noema-attribute-view` 从入口命令读出块、经真实 host 走 Go kernel 画表；Jupyter 运行时清理在 target `local` 上用真实主机名删除文件、错主机拒绝并保留 |

### 全文件覆盖

[noema-coverage.md](noema-coverage.md) 列出全部 542 个源文件（约 21.5 万行）：475 个有测试引用
（补测试前 472），382 个有文档提到，37 个（3736 行）两者都没有（扫描文档时排除本工作台，免得
自引用）。对这 37 个再查引用方：

- **死代码**：`server/lib/lsp-base.mjs`（158 行）从第一次提交起没有任何引用；**已删除**，测试仍全部通过。
- 其余 36 个都有引用方，大多是页面入口（`slides-main`、`markdown-box-lab-main`、资源清理页）、
  Worker 与小工具（`tikz-worker`、`coalesced-timer`、`heading-slug`），由引用它们的模块间接覆盖。
- **核心模块已补测试**：`shared/block-identity.mjs`（块 id 唯一的解析器）原先没有测试直接引用，
  新增 `tests/block-identity.test.ts`；Go `kernel/noema/identity` 包原先没有 `_test.go`，新增
  `identity_test.go`，并钉住两端的 id 形状一致。块身份是 Noema 一体化的锚。
- **仍缺直接测试的较大模块**： `server/jupyter/` 的 wire-protocol、raw-socket、kernel-process
  （有文档无直接测试）、`server/lib/jupyter-notebook-format.mjs`（411 行）、
  `src/cm6/extensions/visual/widgets/inline-commands.ts`（1332 行）。

### 按模块

| 模块 | 读了什么 | 结论 |
| --- | --- | --- |
| 属性视图（`shared/attribute-view.mjs`、`kernel/noema/attributeview`、`buildAttributeView`、`patchAttributeViewCell`） | 解析、求值、回退、写回 | spec 只在 JS 与 Go 两处解析并有 fixture；canonical scope 下 Go 不可用时拒绝回退（`requireGoCore`）；块属性写回带版本校验重试。可编辑范围是 todo 的 planning 键和带 UUIDv7 的块属性。 |
| 规划 DSL 写回（`serializeInlineAttrs`、`serializeSemanticAttrs`） | 实测与两端序列化器 | 行内属性被改写成 `key=value`，与文档和“写回读到的形式”不一致：`agenda-dsl.md` G1。 |
| SiYuan 遗留数据库（`kernel/av`、MCP `database`、CLI `database`） | 注册与防护 | 工具随 MCP 注册，但 `markdown_native_guard.go` 拒绝一切指向 Markdown 笔记本块的写入（含镜像块预检）。不改。 |
| MCP 暴露（`kernel/mcp/server.go`） | 投影与策略 | 默认 `/mcp` 暴露非 research 工具，由 `ExposurePolicy` 收窄。 |
| 嵌入 HTML 清洗（`src/sanitize-html.ts`） | 对照 NocoDB 策略 | **已修**：禁止 `form`、`dialog` 与 `formaction`、`form`、`popover*`。附带发现：happy-dom 的 NodeIterator 在 DOMPurify 删除元素后跳过其后兄弟节点的属性清洗，所以测试按向量分开写；导出渲染实测仍会删掉 iframe 之后的事件属性。 |
| HTML 导出（`src/export-html.ts`、`scripts/render-html.mjs`） | 实测 `#+begin html` | 裸 HTML 被转义（安全）；`html` 块里的 `<iframe>` 在编辑器被删、在导出中保留，`syntax.md` 原先写“iframes are removed”。已改文档并标为待定。 |
| Node host 结构 | `server/lib` 与 `server/Features` 体量 | `server/lib` 约 2.3 MB、`Features` 约 140 KB，`runtime.mjs` 9749 行；与 `architecture.md` “迁移未完成”的描述一致。 |
| 测试入口 | Makefile、`scripts/test-all` | **已修**：Go 测试需要 `fts5` 标签但没有入口（裸 `go test` 18 项失败），新增 `make kernel-test`；Noema 的 Elisp 测试不在任何汇总里，`scripts/test-all` 现在包含它们。 |
| Emacs 侧 `lisp/noema/`（module-doctor） | 策略报错 | **已修** `bridge/note-code.el`（改问 `remote-client-file-name`）与 `bridge/config.el`（TRAMP 兼容边界，标注 `aaron-allow`）。`noema/jupyter` 的 5 处记为 `jupyter-workflow.md` G1–G3，需要 Aaron-PC 远端测试后再改。 |
| 仓库卫生 | 跟踪文件 | **已修**：Noema 跟踪的 `.el~` 备份已移除并忽略 `*~`；Emacs 根目录被跟踪的 `clutch/` 移到 `var/clutch/`。`upstream/org-mode` 是子模块，不是误提交。 |
| 块身份文法（`shared/block-identity.mjs`、`kernel/noema/markdown/scan.go`、av 标题清理） | 三处正则对照，补测试 | 锚点 `{#…}` 接受标签形 id（如 `fig:setup`），而可引用的 `((id))` 与 Go 扫描只认 UUIDv7/旧版形状：这看来是有意的“标签 vs 身份”之分。真正不一致的是标题清理：其它环境用 `parseOrgEnvIdentityTitle` 剥掉任何锚点，av 的 `cleanTitle`（JS）与 `identityTail`（Go）只剥 UUID 形状，所以 `#+begin av 标题 {#fig:x}` 的锚点会显示在视图标题里。另有 `shared/columns-layout.mjs`（另一任务未提交的新文件）也自写了一份。 |
| 页面里的 `attribute-view.ts` widget、`syntax.md`、`rendered-constructs.md` | — | 另一项任务有未提交改动，本轮只在 `syntax.md` 提交了自己的行。 |

没有跑的：`scripts/check-*.mjs` 这类 WebKit 驱动的交互检查（需要图形环境），以及在用户
正在运行的 Emacs 图形界面里的手动操作（本次 Emacs 未运行，没有替用户启动常驻实例）。

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
| 单选列补全候选 | 0.04 s |

边界：最多画 `+csv-grid-max-rows`（2000）行，超过时提示过滤或分组；超过
`+csv-grid-max-bytes`（30 MB）拒绝打开；列宽上限 40 字符。没有用 vtable，因为它为每个
单元格算像素宽度，2000×10 格就是两万次 `string-pixel-width`。CSV buffer 变化后，可见的
grid 在 0.3 s 空闲后重绘；最后一个 grid 关闭时摘掉 CSV buffer 上的钩子和计时器。

属性视图的计算全部在 host；Elisp 只画结果（行数受 spec 的 `limit` 约束，默认 50），
旧请求的答案按 generation 丢弃。

## 验证

- `site-lisp/noema/tests/elisp/noema-attribute-view-tests.el`：6 项（找块跳过代码围栏、
  spec 原样发送、画行/诊断/计算、丢弃过期答案、kanban 分组、编辑经 host 写回后重新求值）。
- `test/csv-grid-tests.el`：21 项（Markdown 表格：围栏外找表、读表含转义竖线、原位写回且
  与重读一致、增删行、自己打开的笔记每次保存并在最后一个视图关闭时释放；CSV：引号/换行/空行解析、按需加引号、类型推断、数字排序空值
  垫底、过滤叠加且不改源、分组与聚合、编辑写回、拒绝过期记录、增删行、按格移动、
  隐藏列、关闭最后一个视图后摘钩子、勾选与单选列识别、保留大小写的翻转、单选补全、
  勾选列排序与计数）。

## 决定

用户授权由我决定并提交。每一项的决定与落点：

| 问题 | 决定 | 落点 |
| --- | --- | --- |
| 规划属性行内写法被改成 `key=value` | 保持代码：`planning-semantic-fixtures.json` 把它钉为规范（保留别名与顺序）。文档按“代码加 fixture 为准”改正 | `agenda-dsl.md` 语法一节，G1 关闭 |
| 导出时 `html` 块的 `<iframe>` | 与编辑器同一策略，删除（vault 中没有这样用的块）；并修好 happy-dom 下清洗漏掉兄弟节点的问题 | Noema `a6eed1f`，`syntax.md` |
| av 标题里的标签形锚点 | 与其它环境一致，剥掉；JS 用 `parseOrgEnvIdentityTitle`，Go 用同一 id 文法，两条 fixture 钉住 | Noema `1d18f02` |
| 锚点可为标签、引用只能是身份 | 维持现状（有意的区分），由新测试钉住两端形状一致 | Noema `5362a11`、`d567482` |
| `noema/jupyter` Remote-first 违规 | 修：清理统一路径、服务器按放置决定直连、TRAMP 写法检查标注为兼容边界；doctor 对 noema 清零 | Emacs `d27a30c3`，`jupyter-workflow.md` |
| 删除/改名前列出反向链接 | 采用于删除：移入废纸篓的确认框写出链接到此处的笔记。改名已由 `noema-move-note` 重写链接 | Noema `8259daa` |
| “让 Agent 填一列” | 不做直接写入（违背可审阅提案）。若将来需要，按 `agent:document:propose` 为每行生成属性块 ediff | 本页 |
| av 增加 `source: csv` | 暂不做：CSV 已由 `+csv/grid` 在 Emacs 里成为表；新增来源要跨 JS/Go/fixture 扩一种语法，等有笔记真正需要时再加 | 本页 |
| 按活动路由模型（NocoDB `AiUseCase`） | 不做：模型由各 ACP provider 自己选，Noema 再加一层配置会与 provider 设置重复 | 本页 |
| Pi watch 的失败分类（retryable / needsRun / deferred） | 暂不做：Pi watch 本身“一定结束、不重放”（`pi-watches.md`），没有重试路径，分类没有用处；若加入重试再采用 | 本页 |
