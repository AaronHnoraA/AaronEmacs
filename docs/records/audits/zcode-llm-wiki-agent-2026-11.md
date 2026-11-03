# ZCode 与 LLM Wiki 研究：Agent 流程与 Noema 知识沉淀

用户要求根据 ZCode 和 Karpathy 的 LLM Wiki 优化 agent-shell 的 Emacs agent 全流程
（API、融合、交互、功能、管理）以及 Noema。研究副本只放在临时目录，没有进入加载链、
依赖或仓库。长期契约见 [Agent 管理](../../design/agent-management.md) C58 与 G15。

## 研究范围

| 来源 | 固定修订 | 切面 |
| --- | --- | --- |
| [zai-org/ZCode](https://github.com/zai-org/ZCode/tree/a60ba6a7e5a06742270ca605f5d2cc2766afa7e1) | `a60ba6a7e5a06742270ca605f5d2cc2766afa7e1`（v3.17.1） | `docs/` 下的会话生命周期、子代理、上下文、回退、问答、工具输出预算等设计 |
| [karpathy/LLM Wiki](https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f) | 2026-11-03 抓取 | 原始资料 → LLM 维护的 Wiki → schema；Ingest、Query、Lint；index 与 log |

ZCode 是带自己 Runtime、Host、Gateway 的完整产品。按 C4，它的执行引擎不接入，只比较
管理语义。LLM Wiki 是一份思路，不是实现。

## 同期修复

研究期间用户报告 Wiki 首页按钮在 Portal 中点不动。真实 Emacs 中复现：Wiki 本身是
Portal 里的 Noema 页面，`+noema-portal-buffer-p` 把 nil 当作当前 buffer，打开一篇
还没有页面的笔记时查找失败被当成 Wiki 自己，随后 `buffer-local-value` 收到 nil，
所有打开页面的按钮都报 `Noema event parse failed: Wrong type argument: bufferp, nil`。
在无头 WebKit 中同一页面的视图切换按钮全部可用，确认问题只在 Emacs 侧。修复让
nil 一律不是页面，测试 `+noema-portal-wiki-surface-opens-a-note-not-yet-open`。

## 采纳与不采纳

| 来源能力 | 决定 | 本地形态或理由 |
| --- | --- | --- |
| LLM Wiki：好的回答要归档回 Wiki，不留在聊天里 | 采纳 | C58：`/file`（`C-c A F`）把上一条回复交给现有的节点工作台，人改标题、位置和标签后创建。`/page` 仍是临时阅读，目录有界 |
| LLM Wiki：Lint（孤页、缺页、矛盾、过时） | 已有 | Wiki 的 Reports、Wanted 和依赖视图；矛盾与过时属判断，留给 Agent，不做规则 |
| LLM Wiki：`index.md` 与 `log.md` | 不采纳 | 索引由 kernel 的 `wikisearch` 回答，日志是各仓库的 Git 历史；再写两份文件就是第二个事实源 |
| LLM Wiki：schema 文件 | 已有 | `AGENTS.md`、`noema-project-maintenance` Skill 与 C51 的项目入口 |
| LLM Wiki：Ingest 一次改 10–15 页 | 已有 | `agent:document:edit`／`propose`（Pi C25）；多页改动经审阅卡片 |
| ZCode：idle TTL 加高低水位的常驻池 | 已有 | C14、C22、C46 保护所有工作并用单个有归属的计时器；本地进程数小，不加数量迟滞 |
| ZCode：单任务重启保留身份、草稿，不重放 | 已有 | C13、C38 |
| ZCode：`ReadSessionContext` 按需读其他会话 | 已有 | C45 的 `read` 分页返回 transcript |
| ZCode：子代理 `RespondToCoordinator` | 已有 | Pi C22：Agent 经 `emacs:pi send` 带 `from` 回话 |
| ZCode：Summary 文件撤销与安全预检 | 记为差距 | G15。C44 已有本会话的改动清单，但 ACP diff 不是检查点；没有确定的前内容就不能声称“安全撤销” |
| ZCode：AskUserQuestion 五分钟后空答案继续 | 不采纳 | 与 C9 冲突：通知不接管执行，Emacs 不替人回答 |
| ZCode：消息级 cascade rewind | 不采纳 | ZCode 自己已下线：历史回退与文件回退混在一起会误导 |
| ZCode：MCP 图片与工具输出预算 | 不采纳 | 属于模型运行时，C4 归提供商 |
| ZCode：Prompt Enhance | 不采纳 | 需要另一条模型请求路径；Prompt 文档已经走现有 Markdown 编辑器 |
| ZCode：`/init` 生成 `AGENTS.md` | 不采纳 | 提供商自带同名命令，C49 让提供方命令优先 |
| ZCode：Plan 独立于权限模式 | 已有 | 模式由提供方报告，`C-c C-m` 原样切换 |

## 验证

- `agent-c58-*`、`agent-c57-*`、`agent-keys-c47-*`、`agent-entry-c49-*`：12 项通过。
- Portal 相关的 Noema 测试 7 项通过。
- 真实 Emacs 中从 Portal Wiki 打开同一篇此前失败的笔记，页面打开，Wiki 留在后台。
