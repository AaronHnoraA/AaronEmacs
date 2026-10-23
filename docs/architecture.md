# 结构总览

本文是 Emacs 配置与 Noema 两个仓库的结构地图：每层在哪、谁依赖谁、改动该落在哪一层。
各层的契约细节不在这里重复，见文中链接的专题文档。Noema 自己的目录地图见
[`site-lisp/noema/docs/architecture/repository-map.md`](../site-lisp/noema/docs/architecture/repository-map.md)。

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
- 两个仓库分别提交。改动跨越边界时（例如 xwidget 协议、`host.json`、共享 KaTeX 宏）
  两边的测试和文档要在同一轮里一起更新。

## Emacs 配置：加载链与分层

```text
early-init.el → init.el → lisp/init-modules.el（唯一的模块顺序）
```

`init.el` 只做包管理初始化（`package.el` + `use-package`）、登录 shell 环境、
`load-path` 与 `etc/` 配置载入；之后的一切按 `init-modules.el` 的顺序加载。
可选模块用 `my/require-module-safely`（失败记入 Health board，不中断启动），
依赖某个包的模块用 `my/require-module-after-any-feature` 延迟。

按依赖方向，自下而上：

| 层 | 位置 | 内容 |
| --- | --- | --- |
| 0 基础 | `init-utils`、`init-base`、`init-open`、`patches/`、`init-macos`、`init-neomacs` | 工具函数、基础设置、打开路由、上游补丁、平台与构建兼容层（只有 `init-neomacs` 知道当前是哪个构建） |
| 1 Remote 框架 | `lisp/remote/`（`init-remote` 入口） | `/fs:TARGET:/path` 身份、target/pipeline/backend/session、进程、通道、环境胶囊、workspace 资源。核心基础设施，见 [remote-framework.md](remote-framework.md) |
| 2 外壳 | `init-emacs-gateway`、`init-ui`、`init-activity*`、`init-tabbar`、`init-display-rules`、`init-tools`、`init-evil` | 外部网关、主题与 mode-line、活动指示、标签栏、窗口显示规则、leader 键（`general.el`） |
| 3 开发 | `init-lsp*`、`init-format`、`telescope/`、`init-symbols`、`init-code-actions`、`lisp/git/`、`init-dev`、`init-debug*`、`init-test`、`init-task`、`init-project-run` | LSP 路由与工具（分层见 [lsp-workflow.org](lsp-workflow.org)）、诊断面板、Git、调试/测试/任务 |
| 4 工作台 | `init-navigation*`、`init-dired`、`init-dirvish-keys`、`init-*-workbench`、`init-minibuffer`、`init-consult-project`、`init-windows`、`init-project*`、`init-workspaces`、`init-treemacs-bridge`、`init-session` | 导航、文件管理、补全、窗口、项目（Projectile + Perspective + Transient），见 [project-guide.md](project-guide.md) |
| 5 运维 | `init-maintenance`、`config-tools`、`init-performance`、`init-diagnostics-*`、`init-health`、`init-server`、`init-background` | 包锁/状态维护、配置注册表 UI、性能观测、健康检查、server 与后台应用（`bin/emacs-app`） |
| 6 Noema 桥 | `lang/init-md`、`lisp/roam/` | Markdown → Noema 页面、xwidget 桥、知识搜索、roam 导航、Jupyter 桥、发布 |
| 7 应用与输入 | `init-portal`、`init-text`、`init-shell`、`init-remote-connectboard`、`init-jupyter-board`、`init-search*`、`init-direnv`、编辑小件、`init-auctex`/`init-latex`、`init-browser`、`init-function-keys`、`init-hyper`、`init-mouse` | 终端（Portal/Ghostel）、搜索、浏览器、LaTeX、按键层 |

语言模块在 `lisp/lang/`（`lean/`、`tex/`、`beancount/` 为多文件子目录），由各自的
主模块或 `auto-mode-alist` 引入。

### 目录职责

```text
lisp/                 init-*.el 本地模块与本仓库自写的单文件包（如 direnv.el）
lisp/remote/          Remote 框架，只放框架代码；消费者留在各自模块
lisp/roam/            Noema 的 Emacs 侧桥接（aaronnote = Noema 的旧名，文件名保留）
lisp/lang/            语言模块
lisp/git/             Git/GitHub 工作台
lisp/telescope/       Telescope 风格选择器（含诊断选择器 init-problems）
lisp/patches/         对上游包的窄补丁
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
var/                  运行时状态与缓存（eln-cache、remote/、aaronnote/host.json …）
templates/            auto-insert 模板；templates/noema 链到 Noema 模板目录
bin/                  emacs-app（后台应用唯一入口）及辅助脚本
tools/                raycast-emacs（唯一外部控制面）、macos-background 原生源码等
test/                 ERT 测试（*-tests.el），按模块命名
docs/                 运维文档，入口 docs/README.md
```

### 横切机制

这些机制贯穿所有层，新代码复用它们而不是另起一套：

- 身份与执行：Remote 框架（`remote-*` API）。本机是 target `local`，消费者不按
  `file-remote-p` 或 target ID 分支。
- 配置值：`site-lisp/config` 注册表 + `etc/config-store.el`，见
  [config-management.md](config-management.md)。
- 键位：`general.el`；leader 前缀在 `init-evil.el`，域内后缀在所属模块，见
  [daily-usage.md](daily-usage.md)。
- 运行时资源：timer/进程/overlay 的生命周期规则见 `AGENT.md` “Resource Lifecycle Rules”。
- 观测：`my/performance-*`、`config-health`、Remote Doctor、LSP Doctor、
  `my/latex-preview-doctor`。

## Emacs ↔ Noema 边界

```text
Emacs (lisp/roam/init-aaronnote.el)
  │ 启动并监督
  ▼
Node host  site-lisp/noema/web-host.mjs    HTTP/SSE、静态资源、server/Features/*
  │ 监督                  ▲ xwidget/Appine 加载 dist/aaronnote
  ▼                       │
Go kernel  site-lisp/noema/kernel          Markdown 打开/保存(CAS)、索引、搜索、引用、
                                           研究 DAG、MCP
Raycast ──(var/aaronnote/host.json → loopback /api)──▶ Node host
```

| 通路 | Emacs 侧 | Noema 侧 |
| --- | --- | --- |
| Markdown 页面 | `lang/init-md`（重定向）、`roam/init-aaronnote`（xwidget 桥） | `aaronnote/`（页面）、`src/cm6/`（编辑器） |
| 按键 | `noema-xwidget-keys`、`my/noema-keys-mode-map` | `aaronnote/host-keyboard.ts`、`xwidget-key-guard.ts` |
| 页面文本给 Emacs 工具 | `noema-md-bridge` | 页面保存的选区 |
| 搜索与引用 | `roam/init-noema-search` | `kernel/noema/wikisearch`、`references.go` |
| Agenda | `noema-agenda`、`roam/init-aaronnote-agenda-*` | `server/lib/agenda-*`、`kernel/noema/agenda` |
| `.noema` 工作文档 | `noema-research*`（JuText、Graph Board） | `kernel/noema/research`、`server/Features/Research` |
| Agent | `noema-agent-*`、`init-ai-ide` | ACP 边界、`kernel/agent`、`kernel/mcp` |
| Jupyter（`.ipynb`） | `roam/init-aaronnote-jupyter-*`、`init-jupyter-board` | `server/Features/Jupyter`、`server/jupyter/`、`jupyter/` |
| 数学宏 | `lang/tex/init-latex`（RaTeX） | `resources/katex-macros`、`shared/katex-macros.mjs` |

Noema 的 Elisp 由 `init-ai-ide` 把 `site-lisp/noema/lisp` 加入 `load-path`。约定：
产品语义（数据模型、Agent、Proposal、能力）放在 Noema 的 `lisp/noema-*.el`；
只与本配置的窗口、键位、Remote、Portal 相关的胶水放在本仓库 `lisp/roam/`。

## 改动落点速查

| 想改的东西 | 落点 |
| --- | --- |
| 新本地模块 | 所属目录 + `init-modules.el` 中按依赖插入 |
| 可调设置 | `config-register` + `etc/config-store.el` |
| 文件/进程/网络/环境 | `remote-*` API；缺能力就扩展框架并实现 native 与远程 backend |
| 语言服务器 | `my/register-language-server`，语言模块不维护本地/远程两套 |
| Noema 页面行为 | Noema 仓库 `aaronnote/` 或 `src/cm6/`，不在 placeholder buffer 上实现 |
| Noema 数据、搜索、引用 | Go kernel；Node/Emacs 只转发与显示 |
| `.noema` 语义 | Noema `lisp/noema-research*.el` 与 kernel `noema/research` |

更细的“该改哪里”见 [settings-cookbook.md](settings-cookbook.md)。

## 已知结构问题

阅读时发现、尚未处理的整理项。动手前逐项确认影响面，并同步本表：

- Jupyter 相关模块分散在三处：`lisp/init-jupyter-*`、`lisp/roam/init-aaronnote-jupyter-*`
  与 Noema `jupyter/`。前者是 kernelspec 管理与 Board，后者是 Noema 桥；命名不能反映
  这一分工。
- `aaronnote` 是 Noema 的旧名，仍出现在 `lisp/roam/` 文件名、`var/aaronnote/` 与 Noema 的
  `aaronnote/` 目录。改名牵涉状态路径与外部工具（Raycast 读 `host.json`），需要迁移步骤，
  不应顺手改。
