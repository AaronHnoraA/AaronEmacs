# 结构总览

本文是 Emacs 配置与 Noema 两个仓库的结构地图：每层在哪、谁依赖谁、改动该落在哪一层。
各层的契约细节不在这里重复，见文中链接的专题文档。Noema 自己的目录地图见
[`docs/noema/architecture.md`](noema/architecture.md)。

## 两个仓库

```text
~/.config/emacs/                 Emacs 配置仓库（本仓库）
└── site-lisp/noema/             Noema 产品仓库（独立 Git，origin Aaron-git:Noema/Noema；
                                 被本仓库 .gitignore 忽略，不是 submodule）
```

- Emacs 配置拥有 UI 组合：窗口、buffer、键位、命令、Remote 框架、LSP、终端。
- Noema 拥有知识/研究的数据与服务：Markdown 编辑面（CM6）、Node host、Go kernel、
  `.noema` 工作文档模型、Agent/Skill/MCP 能力，以及一组随产品发布的 Elisp（`lisp/noema-*.el`）。
- Emacs 是 Noema 唯一的第一方 UI；Node host 与 Go kernel 是 headless 服务。
- 文档只有一棵树：Noema 的文档也在本仓库 `docs/noema/`，Noema 仓库只保留贴着代码的
  `AGENTS.md`、`CLAUDE.md`、`README.md`、`UPSTREAMS.md`。
- 两个仓库分别提交。改动跨越边界时（例如 xwidget 协议、`host.json`、共享 KaTeX 宏）
  两边的测试和文档要在同一轮里一起更新。

## Emacs 配置：加载链与分层

```text
early-init.el → init.el → lisp/init-modules.el（唯一的模块顺序）
```

`init.el` 只做包管理初始化（`package.el` + `use-package`）、登录 shell 环境、
`load-path` 与 `etc/` 配置载入；之后由 `init-modules.el` 里的 `aaron!` 声明决定
一切：每个模块带 Doom 分类，`:optional`（失败记录、不中断启动）、`:after`（延迟）、
`:autoload`（只注册命令）、`:doctor`（模块自己的诊断）。加载器、加载账本、诊断与
模块面板见 [module-system.md](design/module-system.md)。

模块按 Doom 的分类放在 `lisp/分类/模块/`，加载顺序由 `aaron!` 声明决定。下表按依赖方向自下而上排列：

| 分类 | 模块 | 内容 |
| --- | --- | --- |
| `:core` | `lisp/core/aaron-*.el`：`aaron-lib`、`aaron-emacs`、`aaron-modules`、`aaron-doctor`、`aaron-modules-board`，以及 `aaron-utils`、`aaron-base`、`aaron-open`、`aaron-patches`、`aaron-funcs`、`aaron-packages` | 宏、生命周期 hook、模块系统、工具函数、基础设置、打开路由、上游补丁、包工具 |
| `:os` | `macos`、`macos-tools`、`neomacs` | 平台与构建兼容层（只有 `os/neomacs` 知道当前是哪个构建） |
| `:remote` | `lisp/remote/remote-*.el`（框架库），`framework`（入口与 TRAMP 配置）、`gateway` | `/fs:TARGET:/path` 身份、target/pipeline/backend/session、进程、通道、环境胶囊、workspace 资源；深度 -100，永远紧跟核心与系统层。见 [remote-framework.md](design/remote-framework.md) |
| `:ui` | `theme`、`activity`、`tabbar`、`popup`、`windows`、`treemacs`、`workspaces`、`scrollview`、`session`、`rainbow-delimiters` | 主题与 mode-line、活动指示、标签栏、窗口显示规则、窗口与工作区 |
| `:completion` | `minibuffer`、`consult-project`、`search`、`search-extra`、`telescope`、`fzf` | 补全、搜索、选择器 |
| `:editor` | `evil`、`format`、`fold`、`navigation`、`navigation-extra`、`editing`、`snippets`、`file-templates`、`expand-region`、`harpoon`、`text`、`smartparens`、`avy`、`multiple-cursors` | 编辑、导航、折叠、格式化、模板 |
| `:emacs` | `dired`、`dirvish`、`files`、`smerge`、`scratch`、`dir-locals`、`rename` | 内置功能的配置与工作台 |
| `:checkers` | `problems`、`diagnostics`、`diagnostics-extra`、`spell`、`grammar` | 诊断面板、拼写、LanguageTool |
| `:tools` | `lsp`、`lsp-ops`、`lsp-hub`、`symbols`、`workspace-symbol`、`code-actions`、`git`、`project`、`project-local`、`run`、`task`、`test`、`output`、`debugger`、`debug-profile`、`jupyter`、`tree-sitter`、`direnv`、`agent`、`copilot`、`clutch`、`zoxide`、`connectboard`、`menus` | LSP（分层见 [lsp-workflow.md](design/lsp-workflow.md)）、Git、项目（Projectile + Perspective + Transient，见 [project-guide.md](guide/project-guide.md)）、运行/测试/调试、Jupyter 管理、AI |
| `:lang` | `filetypes`、`org`、`markdown`、`auctex`、`latex`、`python`、`java`、`lean`、`beancount`、`cpp`、`rust`、`elisp` 等 | 各语言；多数由 `tools/lsp` 在加载末尾 `require` |
| `:term` | `portal`、`shell` | 终端（Portal/Ghostel） |
| `:noema` | `bridge`（含 publish、agenda、note-code、portal、agent-bridge）、`roam`、`search`、`jupyter`、`activity` | Markdown → Noema 页面、xwidget 桥、知识搜索、roam 导航、Jupyter 桥、发布 |
| `:app` | `browser`、`calendar`、`joplin`（未声明） | 浏览器、日历 |
| `:config` | `default`（Doom 风格默认值）、`maintenance`、`config-tools`、`performance`、`health`、`server`、`background`、`compile`、`function-keys`、`hyper`、`mouse` | 运维、配置注册表 UI、性能观测、健康检查、后台应用（`bin/emacs-app`）、按键层 |

### 目录职责

```text
lisp/init-modules.el  aaron! 声明：全部模块、分类与加载顺序
lisp/core/            核心，以 Doom 的 doom-lib/doom-emacs/doom-modules 为模板，平铺的 aaron-*.el
lisp/CATEGORY/NAME/   模块：config.el（provide CATEGORY/NAME/config）与同目录的其他文件
                      （provide CATEGORY/NAME/FILE），按需 init.el、autoload/、doctor.el、README.md
lisp/remote/          Remote 框架库（remote-*.el，public 命名）与 framework/、gateway/ 两个模块；
                      消费者留在各自模块
site-lisp/            自维护或 vendored 的 Elisp 包：
  general.el/           键位层（启动依赖）
  config/               配置注册表（config-register / config-board）
  aaron-ui/             board 组件库
  portal/               原生 Ghostty 终端窗格
  ratex.el/             数学预览
  dape-toolbar/, remote_ikernel/
  vendored/             单文件第三方包（Andy Stewart 的 fingertip、thing-edit 等，
                        symbol-overlay、winpoint），原样保留上游头注释
  noema/                Noema 产品仓库（见下）
etc/                  私有配置与密钥：config-*.el、config-store.el、remote.json、
                      以及指向 Noema 共享资源的链接（katex-macros、noema/）
.agents/skills/       维护本配置与 Noema 代码用的 Skill（emacs-config、noema-development 等）；
                      .claude/skills 与 etc/noema/linked/emacs/skills 都链接到这里
var/                  运行时状态与缓存（eln-cache、remote/、noema/host.json …）
templates/            auto-insert 模板；templates/noema 链到 Noema 模板目录
bin/                  emacs-app（后台应用唯一入口）及辅助脚本
tools/                raycast-emacs（唯一外部控制面）、macos-background 原生源码等
test/                 ERT 测试（*-tests.el）
docs/                 两个仓库的全部文档，入口 docs/README.md：guide/ 用法、design/ 现行机制、
                      acceptance/ 验收、ops/ 运维、records/ 审计与历史（带日期，不更新）、
                      works/ 进行中的项目；noema/ 下是 Noema 的文档，分类相同
```

### 横切机制

这些机制贯穿所有层，新代码复用它们而不是另起一套：

- 身份与执行：Remote 框架（`remote-*` API）。本机是 target `local`，消费者不按
  `file-remote-p` 或 target ID 分支。
- 配置值：`site-lisp/config` 注册表 + `etc/config-store.el`，见
  [config-management.md](design/config-management.md)。
- 键位：`general.el`；leader 前缀在 `editor/evil/config.el`，域内后缀在所属模块，见
  [daily-usage.md](guide/daily-usage.md)。
- 运行时资源：timer/进程/overlay 的生命周期规则见 `AGENT.md` “Resource Lifecycle Rules”。
- 观测：`+performance-*`、`config-health`、Remote Doctor、LSP Doctor、
  `+latex/preview-doctor`。

## Emacs ↔ Noema 边界

```text
Emacs (lisp/noema/bridge/config.el)
  │ 启动并监督
  ▼
Node host  site-lisp/noema/web-host.mjs    HTTP/SSE、静态资源、server/Features/*
  │ 监督                  ▲ xwidget/Appine 加载 dist/pages
  ▼                       │
Go kernel  site-lisp/noema/kernel          Markdown 打开/保存(CAS)、索引、搜索、引用、
                                           研究 DAG、MCP
Raycast ──(var/noema/host.json → loopback /api)──▶ Node host
```

| 通路 | Emacs 侧 | Noema 侧 |
| --- | --- | --- |
| Markdown 页面 | `lang/markdown`（重定向）、`noema/bridge`（xwidget 桥） | `pages/`（页面）、`src/cm6/`（编辑器） |
| 按键 | `noema-xwidget-keys`、`+noema-keys-mode-map` | `pages/host-keyboard.ts`、`xwidget-key-guard.ts` |
| 页面文本给 Emacs 工具 | `noema-md-bridge` | 页面保存的选区 |
| 搜索与引用 | `noema/search` | `kernel/noema/wikisearch`、`references.go` |
| Agenda | `noema-agenda`、`noema/bridge/agenda-*` | `server/lib/agenda-*`、`kernel/noema/agenda` |
| `.noema` 工作文档 | `noema-research*`（JuText、Graph Board） | `kernel/noema/research`、`server/Features/Research` |
| Agent | `noema-agent-*`、`tools/agent` | ACP 边界、`kernel/agent`、`kernel/mcp` |
| Jupyter（`.ipynb`） | `noema/jupyter/*`、`tools/jupyter` | `server/Features/Jupyter`、`server/jupyter/`、`jupyter/` |
| 数学宏 | `lang/latex`（RaTeX） | `resources/katex-macros`、`shared/katex-macros.mjs` |

Noema 的 Elisp 由 `tools/agent` 把 `site-lisp/noema/lisp` 加入 `load-path`。约定：
产品语义（数据模型、Agent、Proposal、能力）放在 Noema 的 `lisp/noema-*.el`；
只与本配置的窗口、键位、Remote、Portal 相关的胶水放在本仓库 `lisp/noema/`。

## 命令行入口

从 Emacs 外部调用的命令都在 `bin/`，每个只做一件事，细节在所列文档：

| 命令 | 作用 | 规格 |
| --- | --- | --- |
| `bin/emacs-app` | 后台 Emacs 的唯一入口：start/restart/kill/show/hide/status、白名单动作 `act`、`frame`、`terminal`、登录项 | `AGENT.md` “Background application”，[daily-usage.md](guide/daily-usage.md) “Emacs 作为后台应用” |
| `bin/emacsopen` | 在运行中的 Emacs 打开文件，供 agent、脚本与终端调用：位置 `-o`/`-l`/`-r`/`-a`/`-b`/`-f`（其他窗口、四向分割、新 frame；默认选中窗口，`here`/`-o` 复用已显示该文件的窗口），`+行[:列]` 或 `文件:行[:列]`，`-n` 不抢焦点；每个文件输出一行 JSON（buffer、窗口、frame、是否复用与定位），失败退出非零；不依赖 Ghostty。实现 `aaron-open-place`（`lisp/core/aaron-open.el`）；`~/.local/bin/emacsopen` 链到这里 | `emacsopen --help` |
| `bin/install-remote-ikernel` | 安装 `site-lisp/remote_ikernel` | [jupyter-workflow.md](design/jupyter-workflow.md) |
| `bin/sage-jupyter-kernel` | SageMath kernel 启动包装，日志写 `var/log/`；由 Noema 的 sagemath kernelspec 模板调用 | — |
| `make <target>` | 编译、测试、锁文件、健康检查；`make help` 列出全部 | [maintenance.md](ops/maintenance.md) |

Raycast 扩展只调用 `bin/emacs-app`；新增外部入口放进 `bin/`，并在这张表里占一行。

## 改动落点速查

| 想改的东西 | 落点 |
| --- | --- |
| 新本地模块 | 所属目录 + `init-modules.el` 中按依赖插入 |
| 可调设置 | `config-register` + `etc/config-store.el` |
| 文件/进程/网络/环境 | `remote-*` API；缺能力就扩展框架并实现 native 与远程 backend |
| 语言服务器 | `+register-language-server`，语言模块不维护本地/远程两套 |
| Noema 页面行为 | Noema 仓库 `pages/` 或 `src/cm6/`，不在 placeholder buffer 上实现 |
| Noema 数据、搜索、引用 | Go kernel；Node/Emacs 只转发与显示 |
| `.noema` 语义 | Noema `lisp/noema-research*.el` 与 kernel `noema/research` |

更细的“该改哪里”见 [settings-cookbook.md](guide/settings-cookbook.md)。

## 命名

符号命名照 Doom：核心 `lisp/core/` 用 `aaron-`（私有 `aaron--`，命令 `aaron/`），对应 Doom
的 `doom-`；模块的普通符号是 `+领域词-…`（如 `+noema-roam-root`，私有 `+…--…`），命令是
`+领域/动作`（如 `+git/board`、`+language-server/doctor`）；hook 函数以 `-h` 结尾，advice 以
`-a` 结尾；宏沿用 Doom 的 `!` 名字。`remote-*`、`config-*`、`noema-*`、`aaron-ui-*` 等公开
子系统保留自己的命名空间。旧的 `my/` 命名已全部迁移（对照表见
[records/migrations/naming-migration-2026-10.md](records/migrations/naming-migration-2026-10.md)），不留兼容别名。
`aaron-` 与下面的旧名 Aaronnote 无关。

Noema 早先叫 Aaronnote。两个仓库里的文件、模块、通道（`noema:api:*`）、环境变量
（`NOEMA_*`）、CSS 类与状态目录都已改为 `noema`；Noema 的页面源码目录是 `pages/`，
构建到 `dist/pages`。旧名只保留在读取既有数据的地方：`@aaronnote` 代码标记、配置里
保存的 `aaronnote` 主题 id、`core/aaron-base.el` 清理旧 `my/aaronnote-*` 命令历史，以及
`init.el` 启动时把 `var/aaronnote/` 一次性并入 `var/noema/`。不要新增 `aaronnote` 名字。

Jupyter 在 Emacs 侧分两部分：`lisp/tools/jupyter/`（kernelspec 管理、Jupyter Board）与
`lisp/noema/jupyter/`（Noema 的 notebook/cell 桥）；Noema 的 `jupyter/`
放 kernelspec 模板与安装脚本。
