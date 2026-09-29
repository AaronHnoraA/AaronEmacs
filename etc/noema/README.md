# Noema 全局 Skill / MCP 库

主配置（启停、patch、Skill 包、MCP）是本目录的 `capabilities.json`。**全局 Skill 库不在这里**：
自 2026-09-21 起 `NOEMA_GLOBAL_SKILLS` 指向 vault 的
`~/Documents/Noema/public/README/Skills/skills/`（`my/noema-skills-directory`），和笔记同一个
git 版本化；上游 lock `skills.lock.json` 与 helper checkout `vendor/` 也在那个 `Skills/` 目录下。
本目录原来的 `skills/`、`vendor/`、`skills.lock.json` 已于 2026-09-29 迁过去——此前它们对 host
不可见。外部客户端
通过 `linked/` 下的符号链接接入。原有 `profiles/`、`templates/`、`snippets/`
未改动。新服务代码需要重启 Noema host；之后在管理器按 `C-c g` 刷新即可重读来源。

## 日常使用

三个管理器已注册到 Emacs 冷启动的 autoload；不需要先打开 JuText。
无论从哪里调用都直接打开全局页，不选择项目，也不需要 `noema.toml`。
从项目内打开会记住项目上下文，随后可以点击页签或按 `C-c 1/2/3` 切页：

- `1 Global`：全局库，配置写入本目录的 `capabilities.json`，新 Skill 写入 vault 的 `Skills/skills/`。
- `2 Project Patch`：只列出本项目明确微调或选择的条目，不列出继承的全部全局库。
- `3 Local Skills`：只列出当前项目 `.agents/skills/` 内的 Skill；创建/导入也写这里。
- `4 Packs`：Skill 包。包只是平铺 Skill 的链接列表加一段可选 preamble，不复制 Skill 内容；
  `RET` 展开成员，成员行上 `RET` 跳到平铺库里的那个 Skill，`C-c e/i/f` 等直接作用于该 Skill。

没有项目时，后两页不可用，但不影响全局操作。每次重新调用管理器仍先打开全局页。

- `M-x noema-skill-manager` / `M-x noema-mcp-manager`：全局管理入口。
- `C-c C-a` 或工具栏 Actions：打开完整菜单。Evil 保持 normal state，`j/k/f/d/u/数字` 等保持原义。
- `C-c f` / `[Edit]`：右侧打开文件；`C-c o` / `[Folder]`：右侧 Dired，管理页保留在左侧。
- `C-c p` / `[Patch → Project]`：为全局 Skill 创建项目差异；MCP 仍使用字段级 JSON merge patch。
- `C-c y` / `[Copy → Local]`：复制全局 Skill 及资源，成为独立 Local Skill。
- `C-c s`：用 agent-shell 起草微调请求，用户确认发送。不会自动执行模型请求。
- `C-c u`：插入 `@@skill` 或 `@@pack`；`C-c e/d`：启停；`C-c t/l`：MCP 测试/详情。
- `C-c N` / `C-c m`：新建全局包 / 编辑包成员与 preamble；包上 `C-c p`：写本项目对包的 patch。
- `C-c U` / `[Upstream]`：上游版本管理（见下文）。
- `C-c L/G/D`：链接来源、全局配置、全局目录；`C-c n/I`：创建/导入。

### Patch 是差异，不是完整 Markdown 覆盖

项目 `.agents/skill-patches/<id>/skill.patch` 保存 unified diff；
`PATCH-BASE.md` 只是只读参考快照，不是 Local Skill，也不作为替换正文注入。
运行时从全局有效内容出发，在隔离临时目录调用系统 `patch -t -N -F 0` 应用差异。
不写全局原件、不启动 shell 解释器，不接受多文件补丁、路径跳转或 ed 脚本。
基线 SHA-256 不一致、hunk 冲突、frontmatter 损坏时阻止使用；不得静默部分应用。

在 Global 选 Skill → Patch → 回到左侧按 `C-c s`，输入具体微调要求；
agent 使用自己的 file-patch/apply_patch 工具只维护 `.patch` 中的差异。
在 agent-shell 确认发送，保存后回管理器 `C-c g` 校验并刷新。
旧 `content_file` 覆盖仍能读取；再按 Patch 时会转为真正 diff，保留旧 MD 编辑文件。
只有 Copy → Local 才创建可自由编辑的完整 Skill 副本。

JuText work block 的开头可直接输入 `@@`，由 Company/CAPF 补全指令；例如：

```text
%% work 证明检查
@@skill(lean4)
请检查这个 Lean 证明，保持定理陈述不变。
```

数学英文写作选 `@@skill(math-prose)`，符号运算选 `@@skill(sympy)`；
一次带上整组 Skill 用 `@@pack(math-writing)`（见下文 Skill 包）。
六个新 Skill 默认只入库，不自动加入所有 Run。不要把所有 Skill 都启用。
冻结的 Skill 内容附带原始目录定位信息，引用资料按需读取，不整库塞入上下文。

## Skill 包

`capabilities.json` 的 `packs.definitions` 定义全局包；`@@pack(id)` 在 work block
开头选用，等同于逐个 `@@skill` 其成员（成员的 `requires` 依赖照常闭包），
非空 preamble 作为 `pack:<id>` 上下文冻结进 RunSpec。

| 包 | 成员 | preamble 要点 |
| --- | --- | --- |
| `math-writing` | math-prose、claim-precision、paper-polish、intro-drafter、pre-submission-reviewer | 保持假设与结论强度；记号先定义后使用；区分已证/引用/猜想 |
| `math-verification` | proof-plan、proof-review、lean4、lean-proof、mathlib-build、mathlib-review、sympy | 只有通过检查的证明才算验证；符号/数值计算只是证据 |
| `quantum-computing` | qiskit、cirq、pennylane、qutip、sympy | Nielsen–Chuang 记号；Qiskit 小端、Cirq/PennyLane 首比特在左，跨框架时写明约定 |
| `software-dev` | brainstorming、writing-plans、executing-plans、test-driven-development、systematic-debugging、verification-before-completion、requesting-code-review、receiving-code-review | 来自 superpowers；提到的其它 skill/子代理只在本会话真有时使用；仓库 AGENTS.md/CLAUDE.md 优先 |

项目想调整某个包：在 Packs 页选包按 `C-c p`，写入项目 `noema-capabilities.json` 的
`packs.patches`，只有 `members_remove`、`members_add`、`preamble_append` 三种增量，
不改全局定义，也不改成员 Skill 内容（要改 Skill 内容用普通 Skill patch）。
成员缺失或在项目里被禁用时包无效：未使用时是警告，`@@pack` 时阻止运行并提示用
`members_remove` 修复。

## 上游版本管理

vault `Skills/skills.lock.json` 是全局 Skill 的来源记录（自己仓库里的研究类 Skill 本身就是源头，不入 lock）：仓库、跟踪 ref（默认 `HEAD`）、固定 commit、
仓库内路径、许可证，以及安装时 SKILL.md 和整个目录的 SHA-256。`C-c U`（或
`M-x noema-skill-upstream`）打开版本页：

- `C-c g` 联网检查各上游是否有新 commit；平时打开只读本地，不联网。
- `RET` / `C-c =` 预览更新：SKILL.md diff 和增删改的文件，不写盘。
- `C-c u` 更新到上游最新；`C-c R` 固定到历史 commit 或输入的 SHA（回滚）。
  每次替换把旧 commit 记入 `history`（最多 20 条）。
- 目录被就地改过（`Local` 显示 `modified`）时拒绝覆盖，除非确认丢弃；
  微调请放在项目 patch 里，全局副本保持与上游一致。
- `C-c i` 从 `owner/repo`（GitHub 简写）或任意 git URL 安装新 Skill 并写入 lock；
  不覆盖已存在的目录，拒绝含符号链接的 Skill，Skill 子目录没有许可证时带上仓库根的 LICENSE。
- `C-c l` 查看该 Skill 目录在本配置仓库里的 git 历史。

`lean4` 的 `vendor/` helper checkout 不随更新变化，需要时按其 lock 的 commit 手动同步。

## 已安装的 Skill（2026-09-29 新增）

| Skill | 来源（固定 commit 见 lock） | 许可证 |
| --- | --- | --- |
| `qiskit`、`cirq`、`pennylane`、`qutip` | [K-Dense 科研工具库](https://github.com/K-Dense-AI/claude-scientific-skills) | Apache-2.0（qutip 为 MIT） |
| `brainstorming`、`writing-plans`、`executing-plans`、`test-driven-development`、`systematic-debugging`、`verification-before-completion`、`requesting-code-review`、`receiving-code-review` | [obra/superpowers](https://github.com/obra/superpowers) | MIT |
| `claim-precision`、`paper-polish`、`intro-drafter`、`pre-submission-reviewer`、`proof-plan`、`proof-review` | 自己的 Noema `Public-README` 仓库 `Skills/skills/` | 自有 |

这些 Skill 只入库、不默认启用；`noema-research-workflow` 模板里的 `@@skill(proof-plan)` 等
现在都能解析到。

## 已安装的数学相关 Skill（2026-09-15）

| Skill | 用途 | 来源和可信度依据 |
| --- | --- | --- |
| `lean4` | Lean / mathlib 搜索、形式化、证明修复、审查 | [Cameron Freer 的仓库](https://github.com/cameronfreer/lean4-skills)；[作者主页](https://www.cfreer.org/)可核实 MIT 研究职位及数学/形式化研究，[剑桥报告](https://www.cst.cam.ac.uk/seminars/list/246775)介绍实际形式化工作 |
| `lean-proof` | 官方的逐步证明方法 | [Lean 官方团队](https://github.com/leanprover/skills)，不是同名聚合仓库 |
| `mathlib-build` | 缓存优先、按需构建 Mathlib | 同上 |
| `mathlib-review` | Mathlib API、simp、命名与 PR 审查 | 同上 |
| `math-prose` | 英文数学论文、定理证明及公式周边文字，保留假设与结论强度 | [Tianhua Gao 的仓库](https://github.com/TianhuaGao/math-prose)和[作者科研主页](https://tianhuagao.github.io/)；附语料与边界检查。不是 Tao、Wiles 等人亲自调优或背书的版本 |
| `sympy` | 符号代数、微积分、矩阵、方程及数值转换 | [K-Dense 科研工具库](https://github.com/K-Dense-AI/claude-scientific-skills/tree/330c8e764435a731eff571e3efdda70b363d0792/skills/sympy)，不声称由著名数学家亲自调优；计算结果仍需检查定义域、假设及严格证明 |

这里“名家著作作为参考”和“名家亲自维护”是两回事；没有把无法核实的宣传当作背书。
`skills.lock.json` 记录固定 commit、许可证与 SKILL.md 校验值。各目录保留原始
SKILL.md 和 supporting resources，未执行安装钩子或修改 shell 配置。
Math Prose 的语料校验和 16 项本地测试已通过；这些不是数学任务准确率评测。

`vendor/lean4-skills/` 是同一固定 commit 的稀疏 checkout，保存 Lean Skill 的
`bin/`、`lib/scripts/` 等 portable helper runtime。全局配置的 `lean4` patch
提供定位说明；没有假装安装 Claude/Codex 专有命令、hooks 或 subagent 定义。

## 链接的客户端库

| 来源 | 链接内容 |
| --- | --- |
| Claude | `~/.claude/skills`、`~/.claude.json`，以及供手动浏览的 plugins 目录 |
| Codex | `~/.codex/skills` 和 `.system`；当前 Data、Life Science、plugin-management 的 Skill 目录；`~/.codex/config.toml`；供浏览的插件缓存 |
| OpenCode | `~/.config/opencode/skills`、`~/.config/opencode/opencode.json` |
| Pi | `~/.pi/agent/skills`、`~/.pi/agent/mcp.json`（采用 `mcpServers` 格式的扩展） |

接入的 Skill 数量随已安装的插件版本和链接状态变化。Claude/OpenCode 尚无独立 Skill 目录；Pi
尚无上述 Skill/MCP 文件，所以为它们预留了可识别的空链接入口，没有虚报已安装。
现有三个客户端的全局配置暂时都没有 MCP 条目。没有导入 Claude 项目历史、
登录凭据库、未安装插件市场或任意插件 hooks。Codex 插件路径固定到当前安装
版本；升级插件后通过 `C-c G` 更新来源路径或调整对应链接。

来源顺序：内置 → 外部来源（sources 顺序）→ 本全局库 → 项目共享层 → 项目。
同名 Skill/MCP 由后者覆盖，管理器可查 shadowed provenance。
外部 Skill 根目录只扫描一层，不递归遍历整个插件仓库。
Codex 插件缓存的目录带版本号；插件升级后用管理器 `C-c L` 检查链接根目录，
若显示“not created yet”，把 `linked/codex/` 中对应符号链接改指向已安装版本，
再按 `C-c g` 刷新。断开的 Skill 根目录不会出现在 `@@skill` 候选中。

MCP 原生格式由 Node 异步解析；不复制原配置和密钥。外部 MCP 在 Noema
一律默认关闭；native 禁用仍显示为禁用，可通过 Noema 全局或项目启用覆盖。
不支持的 cwd、工具 allow/deny list、显式 OAuth 配置会标为不兼容，不能静默丢弃。
OAuth 登录态不跨客户端搬运。Noema 项目覆盖与原客户端设置是分开的。

这是一份供 Noema 管理的共享来源视图，并非把新 Skill 反向安装进所有客户端。

## 可选 Lean MCP

全局配置预置了默认关闭的 `lean-lsp`（`uvx lean-lsp-mcp==0.30.0`），来源：
[Lean LSP MCP](https://github.com/oOo0oOo/lean-lsp-mcp)。它需要 `uvx` 和对应
项目的 Lean toolchain；本次没有安装 uv/Lean 或下载 Mathlib，当前 shell 未找到 uvx。
准备好 Lean 项目后，可在 MCP 管理器 `C-c e` 启用、`C-c t` 测试。首轮下载依赖可能超过
20 秒的手动探测期限，应先在终端完成依赖准备。未把该预置条目标记为连接成功。

## 性能与更新

补全键入路径只读内存；全局/项目独立缓存、按需异步刷新，配置保存或 `C-c g`
刷新失效。MCP 仅显式 `C-c t` 时探测，20 秒截止、16 KiB stderr 上限，不调用工具。
Skill patch 仅在解析时按注册路径读取，不扫描补丁树；输入/结果各限 1 MiB，
子进程 5 秒截止，16 项有界内容缓存避免重复执行未变化的补丁。
切页复用 30 秒内的缓存；全局写入会同时使项目缓存失效。
本机批处理基准：1,000 个缓存 Skill，10,000 次 CAPF 平均约 0.057 ms/次；
1 MiB 普通正文约 0.0004 ms/次。不含 Company 弹窗绘制，也不代表实际模型延迟。

新增或升级 Skill 用上面的上游版本页（`C-c U`）：安装固定 commit 并写 lock，
升级前可预览 diff，可回滚。不要在输入事件或 Emacs 启动时联网更新整个库。

## 已运行 Emacs 的更新

本次同时迁移了 ACP 包来源，完成当前任务后正常重启 Emacs。
不要在有活动 agent 时卸载 feature 或热加载新渲染适配层；旧进程不由迁移强制终止。
若暂不重启，只更新管理 UI 可以在 `M-:` 求值：

```elisp
(mapc #'load '("noema-api" "noema-capability-actions" "noema-capability-ui"))
```

本次增加了独立的全局 API，旧 Node host 不会自动加载新代码。待没有正在运行
的 Noema 任务时，执行 `M-x my/noema-stop`，再打开管理器，host 会自动重启。
停止 host 会断开 Noema 页面和进行中的请求，不要在工作运行中执行。
全局 API 使用独立 channel，旧 host 会报不支持，不会误写默认项目配置。
