# Mellos Mapping 与 ThirdSpace 对 Noema 的启发（2026-10-03）

## 结论摘要

- **Mellos Mapping 不是第一次进入 Noema。** 2026-09-21 的 Noema 提交 `06914db`、`45e8606`、`e75e58e` 已经吸收了它的核心纪律：`regressed` 及其向上传播、`graph.declare` 整图幽灵声明、无证据 `done` 的认知性警告、`research_cell {action:"changes"}` 陈旧报告和 `noema-work-dag` Skill。本轮只审计那之后 Mellos 0.23–0.27 的新增，以及当时没有落地的部分。
- Mellos 剩下最值得内化的是 **Agent 的读侧契约**：用一次有界查询拿到整张图的状态、按状态过滤、读有深度上限的依赖邻域，并在陈旧报告里直接列出受影响的下游节点。Noema 目前只能按 `cellId` 逐个读，依赖邻域也只有一跳。
- Mellos 的 **验证时基线**（`sources[].sha256` 只在验证通过后写入）比 Noema 现在的 **Run 写入时基线**（`artifact_links` 记录 Run 写出的哈希）更贴近“done 的依据是否仍然成立”。
- Mellos 的 ultra 并行模式不应照搬成执行按钮：`.noema` 明确没有 Run All。可吸收的是只读的 **ready frontier**，即依赖全部 `done`、自身未完成、可以同时开工的节点集合。
- ThirdSpace 的价值不在目录模板。`00-系统 / 01-收件箱 / …` 是 PARA 式物理目录，与 Noema 的“位置不是身份，namespace 是逻辑域”相反。值得吸收的是三件事：**写给 Agent 的知识库路由契约**、**按仓库声明的 meta 约定加只报告的审计**，以及 **“不确定就进收件箱”的默认落点**。
- ThirdSpace 自己的模板存在契约漂移：模板附带、并被 `CLAUDE.md` 和主 Skill 指定为必读的是 `.thirdspace/schema/taxonomy.yaml`，而 `init` 另外生成、`audit-subsystems` 检查的是 `workspace-taxonomy.yaml`。结果是未运行 `init` 时审计报 error，运行后则存在两份内容独立的 taxonomy；`knowledge` 的 `manifest.yaml` 和 `SKILL.md` 给出的落盘路径也互相矛盾。这说明同一规则在 YAML、Skill 文本和脚本里各写一遍、又没有契约测试时必然漂移。Noema 若吸收，规则只能有一个来源。

## 审计范围与版本

- [GuangminJu/mellos-mapping 1fe8b5c](https://github.com/GuangminJu/mellos-mapping/tree/1fe8b5ce42f39eb018a3df86d64a77c8ff47f52e)（0.27.1，2026-09-22）
- [zzyong24/thirdspace-vault-template cb49b87](https://github.com/zzyong24/thirdspace-vault-template/tree/cb49b877150651ced4b8882ef491a8292d8a108e)
- [zzyong24/thirdspace-dashboard 9c06455](https://github.com/zzyong24/thirdspace-dashboard/tree/9c0645569c80700f3722db5414ee95819bbd237f)

对照对象是当前 `site-lisp/noema` 的 WorkNode/Proposal/research MCP、Graph Board，以及 `~/Documents/Noema` 的实际 Wiki 布局和 `wiki.db` 诊断。三项均为静态源码审计，未运行上游程序。

## 一、Mellos Mapping → Noema work DAG

### 已经对齐，不必重做

| Mellos | Noema 现状 |
| --- | --- |
| ghost design 先于代码 | `graph.declare` Proposal，整图接受或否决，Graph Board 画幽灵块 |
| `regressed` 向上传播 | `noema-research-regression-targets` 沿 lineage + depends 传给所有 `done` 下游 |
| done 必须带 evidence；□ 空心表示无证据的 done | `done` 却无 Run/outcome、最近 Run 失败、压在 `regressed` 上都会发出 `:warnings`，只提示、不拦截 |
| “ledger, not a judge” | 结构损坏拒绝，纪律问题只警告；`research_state` 只能改 Run 自己拥有的节点 |
| 远景缩放/分组聚合、submap 下钻 | Graph Board 的语义缩放、焦点深度、Smart Fold 和 fold summary |
| `expectedRevision` / CONFLICT | `graph.declare` 带 `expectedRevision`；文档写入走 revision CAS |
| `mmap_read changes` | `research_cell {action:"changes"}`（`Store.SourceChanges`） |

### 差距 1（P1）：Agent 没有整图读取入口

Mellos 0.23 的 `mmap_read` 以 `pages → map → nodes/edges → neighborhood` 渐进读取：按 status/layer/query 过滤、字段投影、`limit ≤ 100`，并使用绑定 revision 的游标。新会话或上下文压缩后，先读 page 列表和 `context.summary/next`，再只读相关节点。

Noema 的 `research_cell` 必须同时给出 `notebookId` 和 `cellId`（`kernel/noema/research/knowledge.go` 的 `ReadResearchCell`）。`neighbors` 只返回一跳的 Parents/Children/Dependencies/Dependents。Agent 无法回答“这个 workstream 里哪些是 `regressed`”或“我依赖的东西往下两层是什么状态”，只能逐个猜 ID。

建议：在 `research_cell` 增加 `action:"list"`，按 status/kind 过滤并只返回 id、title、status、lineage、depends，带 `limit` 和 notebook revision。再给 `neighbors` 增加 `direction`（dependencies/consumers/both）和 `depth`（0–4）。两者都是对现有解析结果的只读投影，不新增存储，继续遵守 `local_only` 过滤。

### 差距 2（P1）：陈旧报告不列出受影响的下游

`mmap_read {resource:"changes"}` 除文件状态外，还返回最多 100 个受影响的 consumer ID。Noema 的 `researchChangesHandler` 只返回 `sources` 和 `changed` 计数。`regressed` 已经有一套下游遍历（`noema-research-regression-targets`），陈旧报告可以复用同一张合并 DAG，在 `changed > 0` 时附上 `affectedConsumers`。仍然只报告，不改状态。

### 差距 3（P2）：基线应在验证时冻结

Noema 的 `SourceChanges` 比较当前文件与 **Run 写出该文件时** 的哈希（`capture.go`，只看 `created/modified`）。这里有三个盲区：

1. Run 之后、人或其他工具手工修正，再标记 `done`：基线停在修正之前，报告从一开始就显示 changed。
2. 节点依赖但没有写入的文件（读入的数据、被证明的引理文件）根本不在基线里。
3. 被 `research_state done` 认可的版本，与最后一次 Run 写出的版本不一定相同。

Mellos 的做法是验证通过后，才把当前哈希写入节点的 `sources[].sha256`。对应到 Noema：`research_state` 在 `done` 时可选携带 `sources: [path…]`，由 Emacs 应用协调请求时计算哈希并冻结为该节点的 verified baseline。`changes` 优先比较 verified baseline，没有时再回退到 Run 写入基线。应用路径仍然只有 Emacs 一个文档权威。

### 差距 4（P2）：ready frontier 只读视图

Mellos 的分层带让“同层可并行”一眼可见，ultra 模式据此并行开工。Noema 的 DAG 不分带，但可以派生同样的事实：节点未 `done`/`dropped`，并且所有 lineage 父节点与 depends 都是 `done`。这和 `workReadiness` 用的是同一个判断，只是从单节点扩展到整张图。

在 Graph Board 上高亮这些节点（或加一个过滤键），并在 `research_cell list` 中返回 `ready: true`。它回答“下一步可以同时开哪些”，不提供批量执行：`.noema` 没有 Run All，开几个 Run 仍由人逐个决定，并受 `noema-agent-worker-max-concurrent-runs` 约束。

### 可选（P3）

- **Workstream 级 checkpoint**：Mellos 的 `context.summary/next`（≤ 2000 字）。Noema 已有逐 Run 的 Handoff，但缺少整张图的“现在在哪、下一步是什么”。若要加，应作为 notebook metadata，由人或 Proposal 编辑，不让 Agent 直接改写。
- **严格向下分层（rank）**：Mellos 用 rank 保证无环，无需检测。Noema 已有显式环检测，研究图也不天然分层，**不吸收**。
- **映射策略（always/complex/on-request）与 pane presence**：属于 Claude Code 插件的宿主问题。Noema 的 Run 本来就绑定 WorkNode，**不吸收**。

## 二、ThirdSpace → Noema 知识库结构

### ThirdSpace 是什么

ThirdSpace 是一个 Obsidian vault 模板：8 个编号的物理工作区（系统/收件箱/日记/知识/项目/资源/输出/归档），`.thirdspace/` 下 5 份 YAML schema（工作区索引、taxonomy、frontmatter 必填 9 字段、子系统契约、事件采集），每个工作区一个 `WORKSPACE.md` 和一个 workspace Skill，再加一个 1715 行的 `thirdspace-vault.mjs` 负责 resolve/route/create/audit/hook 安装。Dashboard 插件只是统计、热力图和 Todo 面板。

### 与 Noema 的根本差异

| 维度 | ThirdSpace | Noema |
| --- | --- | --- |
| 身份 | 文件路径 + `YYYYMMDD_主题.md` | `#+begin meta` 中的稳定 `id`，`roam://id` 链接，移动不破链 |
| 分类 | 物理目录即分类，主题目录只许一层 | 仓库默认 namespace + 页面级 `namespace` 覆盖，物理位置无关 |
| 隐私 | 无 | `public/` 与 `private/` 分区，发布器永不读取 `private/` |
| 索引 | 无，靠 Agent 扫描 | `wiki.db` 增量投影 + FTS + backlinks + diagnostics |
| Agent 入口 | `CLAUDE.md` + 5 份 YAML + 16 个 Skill | MCP `noema-knowledge` 面（document/search/tag/inbox/dailynote…） |
| 元数据 | 9 个必填字段 | `id/title/date/tags` 等，无统一约定 |

结论：**不引入编号物理目录，也不引入 9 字段必填。** 二者都会把 Noema 已解决的“位置不是身份”退回去。

### 可吸收 1（P1）：写给 Agent 的知识库路由契约

ThirdSpace 最好的设计是语义路由：不维护关键词表，而是把每个工作区的 `desc`，加上 `scan_dirs: true` 的工作区的当前一级子目录，一起交给模型判断落点。新建的子目录自动可路由。

Noema 现状：`document {action:"create"}` 要求 Agent 自己给出 `notebook` 和 `path`。Agent 看不到各仓库的用途，也看不到 `config.json` 中的创建 profile。`noema.toml` 只有 `namespace` 与别名，没有用途描述。

建议：

- 在 `noema.toml` 增加可选 `description`（这个仓库/namespace 放什么、不放什么）。它跟随仓库进 Git，与 namespace 同源，不另建 YAML。
- 在 knowledge MCP 面增加只读的 `workspace {action:"route"}`（或扩展现有的 `workspace` 工具），返回每个已索引仓库的 partition、namespace、aliases、description、一级目录列表和 active creation profile。Agent 据此选择，最终写入仍走 `document create`。
- 规则写进一个全局 Skill（放在 `my/noema-skills-directory`，由 `noema-skill-manager` 管理），不要再造第 17 份 schema 文件。

### 可吸收 2（P1）：“不确定就进收件箱”

ThirdSpace 的默认落点是 `01-收件箱/待整理/`，`processed` 之后必须迁出或归档。Noema 已有 `inbox` MCP 工具，实际 vault 中也有 `private/research/daily/inbox/`，但两者没有被声明为 Agent 的默认落点。建议在路由契约里写明：无法确定 namespace 时，写入当前 creation profile 所在的私有仓库的 inbox；永远不默认写入 `public/`。这同时保护了发布边界。

### 可吸收 3（P2）：meta 约定 + 只报告的审计

ThirdSpace 的 `audit-subsystems` 检查控制文件、工作区、Skill 和缺失的 frontmatter，再写一份维护报告，**不移动文件**。批量迁移先写 manifest 再执行。

Noema 的 `wiki.db` diagnostics 目前只有结构类代码：`duplicate-page-id`、`duplicate-block-id`、`missing-block-id`、`missing-partition`、`missing-repository-manifest`、`non-git-directory`、`legacy-layout`。实际 vault 的 32 篇笔记里可以看到以下内容漂移：

- `kind` 有 `default`、`page` 和缺省三种；
- 旧导入笔记带 `source: roam/...`，新笔记带 `namespace`；
- 标签存在拼写近似（如 `alegbra` 与 `algebra`）。

（注意：`#+begin summary` 写在 meta 块内是 Noema 支持的格式，见 `server/lib/runtime.mjs`，不是错误。）

建议：`noema.toml` 可选声明本仓库的 `required_meta` 和 `kinds`，索引时产生 `warning` 级别的 `meta-missing-field`、`meta-unknown-kind`、`tag-near-duplicate` 诊断。它们进入同一张 `diagnostics` 表，只报告不改写。批量修复走 Proposal 或显式命令。

### 不吸收

- `YYYYMMDD_主题.md` 命名、编号工作区、“主题目录只许一层”：Noema 用 id 和 namespace 解决同一问题。
- Agent Stop Hook 按关键词（`completed`、`全部通过`…）把事件写进工作日志：触发条件不可靠，而且 Noema 的 Run/Handoff/history FTS 已经是更准确的事件记录。
- Hook、crontab 和全局 Skill 软链的“一句话初始化”：Noema 的安装与同步属于 Emacs 配置和 `wiki-auto-sync`，不归 vault 管。
- Dashboard 热力图和贪吃蛇：属于展示层；Agenda 已经承担任务视图。

## 附：本次核对中发现的 vault 现状偏差

这些问题不是代码 bug，但与 `site-lisp/noema/docs/wiki-workspace.md` 的描述不一致，建议另行处理：

1. `public/Philosophy` 不是 Git 仓库，因此其中 2 篇笔记未被索引（`wiki.db` 中唯一的诊断 `non-git-directory`）。
2. 文档中的 Deferred Legacy migration 规划了 `public: Philosophy, QC, books, learn, math, papers, references` 与 `private: daily, project, scratch`。实际上 `books/learn/papers/references/daily/project/scratch` 全部位于 `private/research/` 之下，`public/AI`、`Bio`、`CS` 是空仓库。
3. 全局 Skill 库位于 `public/README/Skills`，即一个名为 README 的公开仓库内。若这是有意安排，`wiki-workspace.md` 应写明；否则应确认它不会被发布器当作 Wiki 页面输出。

## 建议顺序

1. P1：`research_cell list` + 带 depth/direction 的 `neighbors`；`changes` 附带 `affectedConsumers`。
2. P1：`noema.toml description` + 路由读取 + 知识库路由 Skill（包含 inbox 默认落点）。
3. P2：验证时基线；Graph Board ready frontier。
4. P2：meta 约定的只报告诊断。

每一项实现时都应补对应的 Go/Node 测试；涉及 Elisp 与 Node 两侧的判断（例如 ready frontier）应两侧同时实现并各有用例，与认知性警告的做法一致。
