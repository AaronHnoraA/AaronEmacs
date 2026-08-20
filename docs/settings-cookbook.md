# Settings Cookbook

这份文档回答“我要改点什么，应该改哪里”。

## 0. 统一配置入口（优先看这里）

已注册进 `config` 注册表的项，直接用原生 Customize 改即可——**即时生效、自动持久化**，
不用找模块、不用重启：

- `M-x config-board` / `M-x customize-group RET config RET`，或 leader `SPC h c`。
- Lisp：`(config-get 'X)` / `(config-set 'X V)` / `(config-reset 'X)`。

注册表与注册语法见 [config-management.md](config-management.md)。下面各节是「该改哪个
文件」的底层说明；当某项尚未注册时按下面的指引改源码，并顺手把它注册进 `config`。

## 0.1 涉及开发路径、进程或连接时先看 Remote 框架

Remote 是核心开发模型，不是只有 SSH 场景才需要看的附加模块。任何设置或功能如果
涉及文件身份、project/workspace root、进程、可执行文件、PATH/toolchain、watch、
terminal、service、socket 或端口，都先读
[remote-framework.md](remote-framework.md)，再决定修改位置。

- 通用 target/pipeline/backend、路径投影、process/channel 与生命周期能力：
  改 [lisp/remote/](../lisp/remote/) 并同时补 native 与 remote 测试。
- 某个语言或 package 的业务规则：留在所属模块，但只调用 `remote-*` API，不自己
  解析 TRAMP、拼 SSH 命令或按 `"local"`/`file-remote-p` 分叉。
- target、pipeline 与 backend 配置：改 [etc/remote.json](../etc/remote.json)。
- 本地也是 target `local`。consumer 应走与远程相同的 API；普通本地 buffer 只在
  框架外保留原生文件名，以继续兼容 Emacs package。

LSP 尤其严格：workspace root、URI、server、环境、watcher、helper 和 channel 必须
来自同一个 target context。若一个语言接入需要分别写“local contact”和“remote
contact”，优先补齐 Remote/LSP adapter，而不是把分支写进语言模块。

## 1. 我要改主题

文件：

- [lisp/init-ui.el](../lisp/init-ui.el)

现在默认：

- `kanagawa-wave`

改法：

1. 换掉 `load-theme`
2. 如有主题包依赖，改对应 `use-package`

## 2. 我要改字体

文件：

- [lisp/init-base.el](../lisp/init-base.el)
- [lisp/init-text.el](../lisp/init-text.el)
- [lisp/init-auctex.el](../lisp/init-auctex.el)

重点变量：

- `my/font-body`
- `my/font-code`
- `my/font-title`
- `my/font-cn`
- `my/font-body-width`
- `my/h-body`
- `my/h-code`
- `my/h-title`
- `my/scale-cn`
- `my/prose-line-spacing`

职责：

- `lisp/init-base.el`
  基础字体族、字号、中文 fontset 绑定、共享排版 helper。
- `lisp/init-text.el`
  Markdown 里什么时候启用 prose 排版。
- `lisp/init-auctex.el`
  LaTeX 里什么时候启用 prose 排版。

改完后可以执行：

- `M-x my/font-reset-all`

## 3. 我要改 LaTeX 实时预览

文件：[lisp/lang/tex/init-auctex.el](../lisp/lang/tex/init-auctex.el)。

TeXpresso 的本地 checkout、Emacs mode 和构建产物统一放在 `var/texpresso/`。常用维护入口：

- `make texpresso-install`：用 Homebrew 补齐 `mupdf` / `sdl2`，clone 或更新源码并构建。
- `make texpresso-build`：只重建已有 checkout。
- `make texpresso-test`：用 TeX Live 和 dummy SDL driver 跑上游 sample smoke test。

固定集成路径由 `my/texpresso-root`、`my/texpresso-elisp-directory` 和 `my/texpresso-binary`
派生，不写入私人绝对路径。实时 viewer 使用 TeXpresso 自己的 SDL/MuPDF 窗口；PDF Tools 继续负责
正式构建后 PDF 的阅读、搜索、批注和传统 SyncTeX。

## 4. 我要改 AI 助手配置

文件：

- [lisp/init-ai-ide.el](../lisp/init-ai-ide.el)

当前 AI 相关源码由 `site-lisp/ai-workbench/vendor/` 提供。

### Claude Code

变量：

- `claude-code-ide-cli-path`  — Claude CLI 可执行路径（默认 `/Users/hc/.local/bin/claude`）

常用入口：

- `C-c C-'` / `C-c a`  — 打开 claude-code-ide 菜单
- `F12` / `H-l`        — 同上（全局快捷键 / macOS Option 快捷键）

只有在你要换 CLI 路径或调整 MCP tools 时才需要改 Elisp。

### Codex CLI

变量：

- `codex-cli-executable`      — Codex 可执行文件名（默认 `"codex"`）
- `codex-cli-terminal-backend` — 终端后端（默认 `vterm`）
- 本地 override：Codex buffer 用底部普通窗口打开，高度约 18 行，可继续分屏

键位前缀 `C-c c`；参见文件顶部注释。

### Noema LaTeX 导出（CMD+P）

变量（`config-defvar`，组 `my/noema`，落盘 `etc/config-store.el`）：

- `my/noema-latex-export-engine`      — `"codex"`（先验证 Pandoc 稿，再做至多一次 gated polish；编译失败时才允许多轮 repair）或 `"mechanical"`（从不启动 agent）
- `my/noema-latex-export-agent`       — polish/repair 后端 `"codex"`（默认）/ `"claude"` / `"opencode"`；配置里选定
- `my/noema-latex-export-max-attempts` — 有具体编译反馈时的 repair 上限（默认 3；保真 gate 失败会立即回退，不消耗重复尝试）
- `my/noema-latex-export-model`        — 传给当前后端的模型 id（空 = 默认）
- `my/noema-codex-model`               — 仅 codex 的模型 id（空 = 默认）
- CLI 路径复用：codex → `codex-cli-executable`，claude → `claude-code-ide-cli-path`，opencode → `my/noema-opencode-executable`

模板：`templates/latex/*.tex`（`aaronnote-article` / `aaronnote-report` /
`aaronnote-assignment`），每个文件首行 `% aaronnote-template: {json}` 可声明显示名、
编译引擎、document role、共享文件及 typed fields（text/select、required、options、
placeholder、description、group、escape）。新增模板即放一个带该首行的 `.tex`。契约见
`agents/latex-export/AGENTS.md` 与 `docs/latex-export-style.md`。

### 窗口切换 / 交换

文件：

- [lisp/init-windows.el](../lisp/init-windows.el)

现有快捷键：

- `M-o` — `ace-window`，快速选择目标窗口
- `M-S-o` — 交换当前窗口和另一个窗口
- `M-<left/right/up/down>` — `windmove`，按方向切换窗口

交换窗口的策略：

- 只有两个窗口时，直接和另一个窗口交换
- 多于两个窗口时，用 `ace-window` 选目标窗口再交换

## 6. 我要改默认打开方式

文件：

- [lisp/init-open.el](../lisp/init-open.el)
- [site-lisp/general.el/general.el](../site-lisp/general.el/general.el)

改默认 URL / 搜索 / 文件 / PDF 打开方式时，优先改 `lisp/init-open.el`
里的 `my/open-routes`。

`my/open-routes` 使用 `general.el` 提供的 `general-route-*` DSL helpers：
具体策略留在 `init-open.el`，通用的 route 读取、别名归一和菜单选择逻辑在
`site-lisp/general.el/general.el`。

常用值：

- `url` 的 `:default menu` 表示 `browse-url` 每次弹菜单
- `url` 的 `:menu-default xwidget` 表示菜单默认项是 `xwidget`
- `system` 表示 macOS `open` / Linux `xdg-open` / Windows shell open
- `pdf` 默认走 `system`，普通 `find-file` 打开 PDF 仍由 `pdf-tools` 处理

## 7. 我要增删 leader 键

文件：

- [lisp/init-evil.el](../lisp/init-evil.el)
- [lisp/init-funcs.el](../lisp/init-funcs.el)
- [site-lisp/general.el/general.el](../site-lisp/general.el/general.el)

你会看到：

- 全局 leader
- 模式 localleader
- Elisp localleader

当前按键绑定链路：

- `define-leader-key` 是配置里的 leader 入口，定义在 `lisp/init-evil.el`
- `my/evil-define-key` 是底层封装，定义在 `lisp/init-funcs.el`
- `general.el` 是 vendored 的按键绑定 DSL，放在 `site-lisp/general.el/`

`general.el` 的职责很窄：声明式定义快捷键。它不是 UI、补全、LSP 或包管理框架。这里引入它，主要是为了统一处理 Evil state、keymap、leader key 和批量绑定，尤其是符号 keymap，例如 `'global`。

这类绑定会走 `general-define-key`：

```elisp
(general-define-key
 :states 'normal
 :keymaps 'global
 "<leader>ff" #'find-file)
```

真实 keymap object 仍然优先用 Evil 自己的 `evil-define-key*`。所以日常加 leader 键时，不需要直接改 `general.el`，只需要改 `define-leader-key` 的绑定列表。

改法：

1. 找到 `define-leader-key`
2. 在对应分组加命令
3. 如果是 mode 专属功能，优先放 localleader

## 8. 我要加一个新包

### 普通包

直接在对应模块里：

```elisp
(use-package some-package
  :ensure t
  ...)
```

### VC 包

用：

```elisp
(my/package-ensure-vc 'some-package "https://github.com/owner/repo.git")
```

文件：

- [lisp/init-package-utils.el](../lisp/init-package-utils.el)

改完后执行：

```sh
make install
```

或者：

```sh
emacs --debug-init -q -l ./bootstrap.el
```

把锁文件更新掉。

## 8.1 我要改数据库客户端配置

文件：

- [lisp/init-clutch.el](../lisp/init-clutch.el)
- `etc/clutch-config.el`（本地私有配置，默认不进 Git）
- [etc/clutch-config.el.example](../etc/clutch-config.el.example)

职责：

- `lisp/init-clutch.el`
  负责 `clutch` 的 VC 包声明、懒安装、懒加载和入口命令。
- `etc/clutch-config.el`
  放你自己的 `clutch-connection-alist`、超时设置和连接别名。
- `etc/clutch-config.el.example`
  只是示例结构，方便对照。

入口：

- `M-x clutch-query-console`
- `SPC o q`

建议：

- 密码优先放 `auth-source` / `pass`，不要把 `:password` 直接写进主配置。
- 如果你只是改连接列表，通常只需要动 `etc/clutch-config.el`。

## 9. 我要改 snippet

文件：

- [snippets/](../snippets/)
- [lisp/init-snippets.el](../lisp/init-snippets.el)

入口：

- `C-c y n`
  新建 snippet
- `C-c y v`
  找 snippet 文件

## 10. 我要改补全行为

看这些文件：

- [lisp/init-minibuffer.el](../lisp/init-minibuffer.el)
- [lisp/init-search.el](../lisp/init-search.el)
- [lisp/init-lsp.el](../lisp/init-lsp.el)
- [lisp/init-lsp-ops.el](../lisp/init-lsp-ops.el)
- [lisp/init-lsp-tools.el](../lisp/init-lsp-tools.el)
- [lsp-workflow.org](lsp-workflow.org)

职责划分：

- `init-minibuffer.el`
  `vertico/orderless/consult/embark`
- `init-search.el`
  `ivy/counsel/swiper`
- `init-lsp.el`
  `company/company-box/eglot/lsp-mode` 核心路由
- `init-lsp-ops.el`
  organize imports / restart / shutdown / log / session 这类 backend-agnostic 操作
- `init-lsp-tools.el`
  Hub / Doctor / dispatch / runtime 调参

如果你是新增某个语言服务器映射：

- `lsp-mode` 例外路由，优先用 `my/register-lsp-mode-preference`
- 自定义 Eglot server，优先用 `my/register-eglot-server-program`
- server 默认放在 workspace target；不要用 `file-remote-p` 注册另一套 contact
- executable、环境和 URI 必须从 server 所属 logical root 推导，不能依赖当前
  buffer 恰好位于本地还是远端
- client-side UI helper 必须显式声明 client placement，并通过 Remote bridge/channel
  连接 target peer

不要再到处散落手写 `add-to-list 'eglot-server-programs`。

## 11. 我要改远程和终端行为

文件：

- [lisp/remote/](../lisp/remote/)
- [etc/remote.json](../etc/remote.json)
- [lisp/init-shell.el](../lisp/init-shell.el)

重点：

- `remote-board`
- `remote-register-target` / `remote-register-pipeline` / `remote-register-backend`
- `remote-make-process` / `remote-exec`
- `remote-make-network-process` / `remote-open-network-stream` / `remote-port-forward`
- `remote-environment-ensure` / `remote-environment-derive`
- `my/vterm-ssh`

target、pipeline、TRAMP/tramp-rpc backend、逻辑 `/fs` 路径和 PATH 环境层的完整
设计见 [remote-framework.md](remote-framework.md)。`lisp/remote/` 只放通用框架；
具体插件接入留在所属模块，但不得重新拥有物理路径、spawn、连接、转发或恢复逻辑。

## 12. 我要改项目管理行为

文件：

- [lisp/init-project.el](../lisp/init-project.el)
- [lisp/init-evil.el](../lisp/init-evil.el)

重点：

- `my/project-search-paths`
- `my/project-dispatch`
- `my/project-open-workbench`
- `my/project-switch`
- `show-imenu`

职责：

- `init-project.el`
  项目工作流本体：Projectile / Perspective / Treemacs / transient / `show-imenu`
- `init-evil.el`
  `SPC p` 这一组项目快捷键，以及 `SPC c i`

如果你想改“项目切换后自动做什么”，优先看：

- `my/project-switch`
- `my/project-open-workbench`

如果你想改“自动发现项目从哪里找”，改：

- `my/project-search-paths`

## 13. 我要调整 Dired / Dirvish

文件：

- [lisp/init-dired.el](../lisp/init-dired.el)
- [lisp/init-windows.el](../lisp/init-windows.el)

职责：

- `init-dired.el`
  基础 dired 行为
- `init-windows.el`
  `dirvish` 的 UI 和键位

## 14. 我要改运行时状态目录

文件：

- [lisp/init-base.el](../lisp/init-base.el)

重点变量：

- `my/state-dir`
- `my/backup-dir`
- `my/auto-save-dir`
- `my/lockfile-dir`

现在所有这些都写到 [var/](../var/)。

## 15. 我要改新建文件模板（auto-insert）

文件：

- [lisp/init-auto-insert.el](../lisp/init-auto-insert.el)
- [templates/](../templates/)

入口：

- `SPC f t`
  选择并切换当前 buffer 对应 kind 的模板（会用新模板替换当前 buffer 内容）
- `SPC f T`
  查看当前 buffer 命中的 kind、当前选择的模板、以及是否会自动插入

默认策略：

- `auto-insert-mode` 全局开，但只对 `my/template-auto-insert-enabled-kinds` 里的 kind 生效
- `org` 默认不在 allowlist 里（避免干扰 `org-capture` 和 note 模板）
- 模板存放在 `templates/<kind>/`；模板里的占位符支持 `{{date}}` / `{{title}}` / `{{file}}` / `{{author}}` / `{{cursor}}` 等

Noema / Roam Node 的 Markdown 模板集中在
[templates/noema/](../templates/noema/)。该目录与 `templates/latex/`、`templates/tex/`
链接到 Noema 仓库；其它模板目录由 Emacs 仓库维护。它们使用 Noema 的
snippet-style header、`{{title}}` / `{{date}}` / `{{tags}}` 等变量和 `$0` tabstop，
由 Noema runtime 展开，不进入普通 `auto-insert` allowlist。

Typst 模板集中在 [templates/typst/](../templates/typst/)。当前 assignment 模板会
导入项目根目录下的 `/_typst/assignment.typ`；插入模板时 Emacs 会自动创建这些
`_typst/*.typ` 软链，样式源文件统一维护在 [notes/](../notes/)。

`default.typ`、`academic-report.typ`、`manuscript-review.typ`、
`rebuttal-letter.typ`、`longform-book.typ`、`project-financial-report.typ`、
`lab-report.typ`、`grant-proposal.typ` 是普通独立 Typst 文档模板。
`touying-simple.typ` 是 Touying slides 模板。

项目内按目录开关（`.dir-locals.el`）：

```elisp
((nil . ((my/template-auto-insert-enabled . t)
         (my/template-auto-insert-enabled-kinds . (c cc sh python js ts tex))
         (my/template-current-override . ((python . "module.py")
                                          (tex . "ctex-article.tex"))))))
```

说明：

- `my/template-current-override` 只接受”文件名”（不能带路径分隔符），指向 `templates/<kind>/` 下的模板文件
- 新建文件模板只保留这一套内置 `auto-insert`（已移除 Doom 那套 Yasnippet file-templates 的遗留实现，避免重复/分叉维护）

## 16. 我要配置 Jupyter

### 远程 Jupyter server（HTTP/HTTPS）

服务器清单是注册过的 config 项 `my/noema-jupyter-servers`，值放在
`etc/config-store.el`（或 config board 里改），**不要用 `setq`**：

```elisp
(my/noema-jupyter-servers
 ((:id "hpc" :name "Cluster Lab"
   :url "http://127.0.0.1:8888/"   ; 也可以整条粘贴 jupyter 打印的 ?token=... URL
   :kind server                    ; server（默认）或 gateway
   :auth token                     ; token / password / hub / none
   :target "cluster")))            ; 拥有它的 Remote target，默认 "local"
```

密钥不写在这里。`:auth token` / `:auth password` 通过 `auth-source-search`
按 URL 的 host（以及 `:user`）查找，所以放在 authinfo/GPG 里：

```
machine lab.example.org login aaron password <token>
```

`:target` 不是 `local` 时，Emacs 先在那个 target 上开 `remote-port-forward`，
把 `127.0.0.1:<本地端口>` 的 URL 交给 Noema，并附带真实主机名供 TLS 校验。
**target 开不出通道时会直接报错**，不会退回到本机直连 —— 远端 server 通常绑在
loopback，本机同端口很可能是另一个进程。

自签名证书用 `:insecure t`（只对这一台生效）。

### 默认 kernel / 语言 / session

按项目设置，写在项目的 `.dir-locals.el` 里：

```elisp
((nil . ((eval . (setq-local my/project-local-settings
                             '(:aaronnote-jupyter (:language python
                                                   :kernel sagemath
                                                   :session research))))))))
```

这里的 `python` 是 notebook language；`sagemath` 才是 kernelspec 名。

### 超时和输出上限

Noema 侧用环境变量，默认值和含义见
`lisp/roam/Noema/jupyter/README.md` 的 Environment variables 表
（执行超时、stdin 等待上限、stream 字节上限、实时输出合并窗口等）。

### 诊断

`M-x remote-doctor`（带 probe）会检查目标上的 `python3`/`jupyter`，
并对每台配置的 server 解析连接、请求 `/api/status`，所以坏掉的 forward
会作为 server 故障直接报出来，而不是过一会儿变成莫名其妙的 kernel 失败。

kernelspec 本身用 `M-x my/jupyter-board`（Hyper `j`）管理。

## 17. 我要配置项目本地 `.dir-locals.el`

文件：

- [lisp/init-dir-locals.el](../lisp/init-dir-locals.el)
- [templates/emacs/](../templates/emacs/)

入口（`SPC p e`）：

- `SPC p e e`  编辑当前项目的 `.dir-locals.el`
- `SPC p e c`  从模板创建 `.dir-locals.el`（如已有则确认替换）
- `SPC p e m`  将某个模板合并进现有 `.dir-locals.el`（按 mode key 合并，模板优先）
- `SPC p e r`  重新加载当前 buffer 的 dir-locals，并刷新 direnv 环境（PATH 等）
- `SPC p e s`  将文件里的非 `eval` 变量全部加入 `safe-local-variable-values` 并保存
- `SPC p e d`  查看哪些 dir-locals 条目对当前 buffer 的 major-mode 生效

可用模板（`templates/emacs/`）：

| 模板名 | 用途 |
|---|---|
| `python-venv` | Python `.venv` 虚拟环境 |
| `python-uv` | Python uv 项目 |
| `python-conda` | Python conda 环境（替换 `myenv`）|
| `cc-cmake` | C/C++ CMake 外构建 |
| `cc-meson` | C/C++ Meson 构建 |
| `nix-flake` | 本地 nix flake（`nix develop`）|
| `nix-gcc` | GCC 工具链，通过 nix-shell |
| `nix-clang` | Clang 工具链，通过 nix-shell |
| `nix-shell` | 通用 nix-shell 任务包装 |
| `sagemath` | SageMath 脚本 / Jupyter 内核 |
| `node` | Node.js / npm / TypeScript |
| `lsp-workspace` | 自定义 Eglot workspace 配置 |
| `emacs-lisp` | Emacs Lisp 包项目 |
| `indent-2` | 项目全局 2 空格缩进 |
| `indent-4` | 项目全局 4 空格缩进 |
| `direnv` | direnv 任务占位 |

**合并策略**：按 mode key（`nil` / `python-ts-mode` 等）合并，每个 mode 内按变量名合并，模板值覆盖旧值，新 mode 追加到末尾。

**env 刷新**：`SPC p e r` 在 dir-locals 重载后会调用
`my/direnv-update-environment-maybe`。仓库内置的
[lisp/direnv.el](../lisp/direnv.el) 把 direnv 注册为 Remote 框架的 workspace
environment maintainer；本地与远端都生成隔离的 `target@workspace` 环境实例，
远端执行优先 tramp-rpc、回退 TRAMP。PATH、编译器路径等变量只投影到所属 buffer
和它启动的进程，不需要重启，也不会污染别的 target。

**`eval` 说明**：`silence` 命令跳过 `eval` 条目（涉及安全确认）。如需 silence `eval` 形式，手动将其加入 `safe-local-eval-forms`。

---

## 新建只读 dashboard / report / hub buffer

需要新建只读面板（`special-mode` 风格）时，**请使用 `aaron-ui-board`**，不要手工构建 face 和 insert 样板：

```elisp
(require 'aaron-ui-board)

(define-derived-mode my/foo-mode aaron-ui-board-mode "Foo")

(defun my/foo-refresh ()
  (interactive)
  (let ((inhibit-read-only t))
    (aaron-ui-board-render
     (lambda ()
       (aaron-ui-board-insert-page-header "Foo" :icon 'gear)
       (aaron-ui-board-insert-section "Details")
       (aaron-ui-board-insert-field "Key" "Value")
       (aaron-ui-board-insert-key-hints "Keys: g refresh  q quit")))))

(defun my/foo ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Foo*")
    (my/foo-mode)
    (aaron-ui-board-set-header "Foo" 'gear)
    (setq-local aaron-ui-board-refresh-function #'my/foo-refresh)
    (my/foo-refresh))
  (pop-to-buffer "*Foo*"))
```

完整 API 见 [dev-guide.md](dev-guide.md) §10。

## 新建 transient dispatch 菜单

直接用 `transient-define-prefix` 即可。`aaron-ui-transient` 在启动时自动将 Kanagawa
palette 应用到所有 transient 共享 faces，**新菜单无需单独配色**。如需调整颜色，
修改 `site-lisp/aaron-ui/aaron-ui-transient.el` 中的 `aaron-ui-transient-apply-faces`
即可影响全部菜单。详见 [dev-guide.md](dev-guide.md) §11。
