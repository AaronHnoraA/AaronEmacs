# NocoDB 全量清单

`upstream/` 浅克隆 `eabb8b1`（2026-11-23）的每个目录：做什么、读到什么程度、对 Noema 的结论。
“读法”一列：**深读**＝逐函数读过关键文件；**通读**＝读过文件头注释与导出，抽读实现；**清点**＝列出文件、
按名字与头注释归类。结论汇总在 [README](README.md)。

## 仓库与包

| 包 | 体量 | 读法 | 内容 | 结论 |
| --- | --- | --- | --- | --- |
| `packages/nocodb` | 约 1400 个 ts，约 31 万行 | 见下表 | NestJS + Knex 后端 | 见下表 |
| `packages/nc-gui` | 约 1760 个 ts/vue | 见下表 | Nuxt 3 / Vue 3 / Pinia 前端 | 见下表 |
| `packages/nocodb-sdk` | 约 350 个 ts | 见下表 | 前后端共用：类型、公式、聚合、工作流/AI/Agent/Chat 类型 | 见下表 |
| `packages/nocodb-sdk-v2` | 7 个 ts | 清点 | v3 API 的新 SDK | 与 Noema 无关 |
| `packages/noco-integrations` | `core/src`：actions、ai、auth、channel、nocodb、sync、workflow-node、registry | 深读 ai、workflow-node；通读其余 | 集成插件的统一契约 | AI 用例路由、节点运行契约已记入 README |
| `packages/nc-secret-mgr` | 7 个 ts | 清点 | 数据源凭据加密的命令行工具 | 单人本机无需 |
| `packages/nc-integration-scaffolder`、`nc-mail-assets`、`nc-lib-gui` | 很小 | 清点 | 脚手架、邮件模板、打包后的 GUI | 无关 |
| 根目录 `charts/`、`docker-compose/`、`scripts/`、`markdown/` | — | 清点 | 部署、构建脚本、插件说明 | 无关 |

## 后端 `packages/nocodb/src`

| 目录 | 文件/行 | 读法 | 内容 | 对 Noema |
| --- | --- | --- | --- | --- |
| `db/BaseModelSqlv2*`（主文件 + 子目录） | 约 2.3 万行 | 深读 `list()`、insert/delete/links 入口 | 一切记录读写，Link/Lookup/Rollup 编译成子查询/CTE | 视图是表上的查询：Noema 的 `#+begin av` 已同构；不引入 SQL |
| `db/formulav2`、`db/functionMappings` | 约 1100 + 8 个方言文件 | 深读编译器入口与 pg 映射 | jsep AST → 各方言 SQL，91 个函数 | 数据不在 SQL 里，不引入；av 的 `template`/`calc` 已够 |
| `db/field-handler` | 每类型一个目录 | 通读 | 按字段类型的过滤/排序/序列化策略 | 与 `ATTRIBUTE_VIEW_FIELD_TYPES` + Go 类型比较同构 |
| `db/conditionV1`、`conditionV2.ts`、`sortV2.ts`、`search/` | — | 通读 | 过滤、排序、搜索索引（构建队列、scope、live） | Noema 搜索唯一实现在 kernel `wikisearch`，不新增 |
| `db/sql-client`、`sql-mgr`、`sql-migrator`、`sql-data-mapper`、`dbQueryClient/` | 约 20 个 + | 清点 | 多方言连接、迁移 | 外部数据库归 clutch |
| `db/cte-generator`、`links`、`relation-manager.ts`、webhook handlers | — | 通读 | 关系与级联、更新后触发 | Noema 关系是 Markdown 链接与块引用 |
| `meta/`（含 `migrations/v0..v2`、audit、chat、docs-content、operation-logs） | 269 / 2.2 万 | 通读服务，清点迁移 | 元数据库与其迁移 | 文件即事实、`wiki.db` 可丢弃，不引入 meta 库 |
| `models/` | 97 / 3.1 万 | 深读 Model、AIColumn、Document、RowColorCondition；清点其余 | Active-Record 元数据模型；约 28 个在 CE 是空壳（README“社区版与闭源边界”） | 缓存优先读法与 Noema kernel LRU 一致 |
| `cache/` | 6 / 1651 | 深读 | Redis 或进程内，key 带 workspace/base | 已一致 |
| `services/` | 172 / 6 万 | 深读 data-table、mcp-token；通读 smart-text、duplicate-detection、dependency、meta-diffs、column-data-backup-handler、formula-column-type-changer、link-placeholder、search-index、sync、command-palette；清点其余 | 每个资源一个服务 | 乐观并发（`expectedMdHash`）已有对应；重复检测已采用到 grid；破坏性改动前报告依赖、改类型先备份、删链接列留文本，记入 README |
| `controllers/`（含 `v3/`、`internal/`、`old-datas/`） | 80 / 1.1 万 | 清点，抽读 data-table | REST 入口 | 无关 |
| `mcp/` | 21 / 3818 | 深读 `mcp.service.ts` 工具注册、`descriptions.ts`、`tools/read-echo.ts`、`filter-verify.ts` | 13 个 MCP 工具（库信息、表清单、表结构、查询、读、计数、附件、聚合、增改删、关联/解除） | 严格 schema、读回配对、不可写字段被改则拒绝、描述写全语法：记入 README，对照 Noema `agent:*` 工具 |
| `command-registry/` | 4 / 953 | 通读 | 命令面板操作的注册与 scope | Noema 用 `tools/agent/actions` 一张表生成键位、菜单与 slash 命令，已有 |
| `modules/auth`、`oauth`、`jobs`、`event-emitter` | 114 / 3.5 万 | 通读 jobs 清单，清点其余 | 认证、OAuth、任务队列（导入导出、meta-sync、缩略图、webhook、搜索索引） | 单人本机不需要认证；任务由 Emacs activity/eshell 承担 |
| `helpers/` | 180 / 2.8 万 | 通读名字与头注释，深读 coalescer、queryGuard 相关 | 杂项：错误、校验、SSRF 防护、CSV 公式转义、keyed semaphore、trash、view lock… | coalescer、keyed semaphore 的思路可在 Noema host 复用；`csvFormulaEscape` 提醒：导出 CSV 时以 `= + - @` 开头的值要防公式注入（grid 不导出，暂不涉及） |
| `middlewares/`、`guards/`、`interceptors/`、`filters/`、`decorators/`、`strategies/` | 34 / 约 4200 | 清点 | NestJS 横切：权限、限流、签名、异常 | 无关 |
| `plugins/` | 65 / 4477 | 清点 | 存储（S3、GCS、R2…）与通知（Slack、SMTP…）插件 | 无关 |
| `utils/`、`interface/`、`types/`、`constants/` | 102 / 约 1.4 万 | 清点 | 全局常量、配置、类型 | 无关 |
| `version-upgrader/`、`Upgrader.ts` | 15 / 2474 | 清点 | 跨版本数据升级 | Noema 的 `.noema` 退役等迁移由 records 记录 |
| `run/`、`gateways/`、`socket/`、`redis/`、`providers/`、`integrations/`、`schema/` | 小 | 清点 | 启动方式、WebSocket、Redis 等 | 无关 |

## 前端 `packages/nc-gui`

| 目录 | 文件 | 读法 | 内容 | 对 Noema |
| --- | --- | --- | --- | --- |
| `components/smartsheet/grid/canvas` | `index.vue` 4841 行 + composables/loaders/utils | 深读键盘、数据分块、Markdown loader | canvas 表格 | 手感已采用到 `+csv/grid`；canvas 与分块取数不需要 |
| `components/smartsheet`（其余：calendar、gallery、kanban、form、gantt、timeline、map、list、toolbar、expanded-form、column、header） | 278 | 通读 toolbar 与 expanded-form，清点其余 | 各视图与工具栏 | Noema 已有 table/gallery/kanban（av）、Agenda Gantt（Web）；Form、Map 不需要 |
| `components/cell`、`virtual-cell` | 124 | 清点，抽读 attachment、checkbox | 实体列与虚拟列渲染 | 勾选、单选的交互已采用到 grid |
| `components/nc`、`general`、`dlg`、`account`、`workspace`、`dashboard`、`project` | 约 450 | 清点 | 设计系统组件、对话框、侧栏、工作区 | UI 设计归 Aaron UI/ETAF，不引入 |
| `components/ai`、`connect-ai`、`chat`、`agent`、`skill`、`cmd-k/j/l`、`erd`、`extensions`、`webhook`、`feed`、`payment`、`permissions`、`rls`、`roles`、`share`、`shared-view`、`shell`、`tour`、`template`… | 约 150 | 清点；chat/agent 在 CE 只有 3 行占位 | — | AI/Chat 前端在 CE 也基本是占位 |
| `composables/` | 201 | 清点，深读 `useUndoRedo`（CE 为空实现） | 状态与行为 | 撤销：Emacs 文本撤销天然具备 |
| `helpers/tiptap*`、`helpers/workflow*` | 90 | 深读 `markdown.ts`、tiptap-markdown 提及、workflow 表达式 HTML | 富文本、Markdown、工作流辅助 | 两套 Markdown 解析器与清洗策略记入 README |
| `utils/`（含 `workflowUtils.ts`、`workflowGraphUtils.ts`、`workflowExpressionUtils.ts`） | 108 | 深读三个 workflow 文件 | 工具函数 | 步骤按标题引用与改名重写、测试结果新鲜度与阻塞、循环内引用检查：记入 README |
| `store/` | 30 | 通读 workflow、views、tables | Pinia 状态 | 无关 |
| `pages/`、`layouts/`、`middleware/`、`plugins/`、`lib/`、`modules/`、`workers/`、`extensions/`、`tours/`、`lang/` | — | 清点 | 路由、布局、插件、多语言 | 无关 |

## SDK `packages/nocodb-sdk/src/lib`

| 模块 | 读法 | 对 Noema |
| --- | --- | --- |
| `workflow/`（interface、node、constants、expressionCatalog、transforms） | 深读 | 执行结果分类、`apply`/`peel` 芯片、白名单表达式：记入 README |
| `ai/`、`agent/`、`chat/`、`mcpToken`、`skill`、`scripts` | 深读 ai、agent、chat 类型；清点其余 | AI 序列化类型、Agent 角色上限、Chat 工具调用状态：记入 README |
| `formula/`、`formulaHelpers`、`parser`、`filter*`、`aggregation*`、`columnRules`、`UITypes`、`viewUtils`、`kanban*`、`timeline*`、`gantt*` | 通读 formula 与 aggregation 名称表，清点其余 | 聚合函数表与 av `calc` 运算符基本一致 |
| `Document*`、`docs`、`smartText`、`commentAnnotation`、`comment` | 清点 | 文档与评论在 CE 是空壳；Noema 评论锚定在文本里 |
| 其余（payment、whiteLabel、oauth、permission、rls、environment、vault、snapshotSchedule、managed-app、hq-roadmap、jalali…） | 清点 | 多用户、商业化与地区功能，无关 |

## 没有读到逐行的部分

后端约 31 万行、前端约 1760 个文件，没有逐行读完。没有逐行读的是与 Noema 无关的多用户、计费、
存储插件、方言连接器、REST 控制器和设计系统组件；它们按名字与头注释归类在上表。与 Noema 相关的
数据视图、表格交互、Markdown、AI、工作流、MCP 这六块都是深读。
