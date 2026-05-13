# Settings Cookbook

这份文档回答“我要改点什么，应该改哪里”。

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
- [lisp/note/init-note.el](../lisp/note/init-note.el)
- [lisp/note/typst/note.typ](../lisp/note/typst/note.typ)
- [lisp/note/typst/math.typ](../lisp/note/typst/math.typ)
- [lisp/init-text.el](../lisp/init-text.el)
- [lisp/init-auctex.el](../lisp/init-auctex.el)
- `~/HC/Org/_typst/publish.typ`

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
- `lisp/note/init-note.el`
  Typst note 根目录、索引、跳转、预览和 helper 写入。
- `lisp/note/typst/note.typ`
  Typst note 的页面、字体、标题、卡片块和跨 note helper。
- `lisp/note/typst/math.typ`
  TCS、量子计算、代数、计算和物理常用数学宏；宏表见 [typst-math-macros.md](typst-math-macros.md)。
- `~/HC/Org/_typst/publish.typ`
  发布 PDF 的页面、字体和视觉补齐；只影响 `make publish`，不影响 Emacs 里的日常 preview。
- `lisp/init-text.el`
  Markdown 里什么时候启用 prose 排版。
- `lisp/init-auctex.el`
  LaTeX 里什么时候启用 prose 排版。

改完后可以执行：

- `M-x my/font-reset-all`

## 3. 我要改 Note 根目录

文件：

- [lisp/note/init-note.el](../lisp/note/init-note.el)

重点变量：

- `my/note-root`
- `my/note-roam-dir`
- `my/note-db-file`
- `my/note-style-directory`

如果你迁移目录，建议一起检查：

- `templates/typst/`
- `notes/*.typ`
- `my/template-typst-style-links`
- Tinymist / Typst 的 project root

## 4. 我要改 Note / Typst 写作入口

文件：

- [lisp/note/init-note.el](../lisp/note/init-note.el)
- [lisp/note/init-note-tools.el](../lisp/note/init-note-tools.el)
- [lisp/init-macos.el](../lisp/init-macos.el)
- [lisp/init-auto-insert.el](../lisp/init-auto-insert.el)

这里集中维护：

- `C-c n ...` 的 Typst note map
- `H-o ...` 的 macOS note 前缀
- `H-y` / `C-c n y` 的图片粘贴
- `C-c n z` / `H-o z` 的 Zotero metadata 填充
- `templates/typst/assignment.typ` 插入时创建项目内 `_typst/*.typ` 软链

发布入口不在 Emacs 配置仓库里，而在 `~/HC/Org`：

- `make publish`
  扫描 `roam/**/*.typ`，增量编译 `public/roam/**/*.pdf`，刷新 archive / graph 数据。
- `~/HC/Org/_typst/publish.typ`
  发布 PDF 专用 Typst 样式。不要为 PDF 视觉补一个 note CSS；PDF 里的视觉缺口直接用 Typst 补。

## 5. 我要改 AI 助手配置

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

不要再到处散落手写 `add-to-list 'eglot-server-programs`。

## 11. 我要改远程和终端行为

文件：

- [lisp/init-shell.el](../lisp/init-shell.el)
- [lisp/init-base.el](../lisp/init-base.el)

重点：

- `my/vterm-ssh`
- `my/ssh-config-hosts`
- TRAMP 相关 `setq`

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

## 14. 我要改 Typst note 样式

文件：

- [lisp/note/typst/note.typ](../lisp/note/typst/note.typ)
- [lisp/note/typst/math.typ](../lisp/note/typst/math.typ)
- [lisp/note/typst/extension.typ](../lisp/note/typst/extension.typ)
- `~/HC/Org/_typst/publish.typ`
- [notes/assignment.typ](../notes/assignment.typ)
- [notes/rho.typ](../notes/rho.typ)
- [notes/aleph-notas.typ](../notes/aleph-notas.typ)

职责：

- `lisp/note/typst/note.typ`
  日常长期 note 的页面、字体、标题、目录、卡片块和跨 note helper。`M-x my/note-db-sync`
  会把 note 根目录的 `/_typst/note.typ` 链到这里。
- `lisp/note/typst/math.typ`
  日常 note 的数学记号模块。`note.typ` 会 re-export 它，`M-x my/note-db-sync`
  会把 note 根目录的 `/_typst/math.typ` 链到这里。
- `lisp/note/typst/extension.typ`
  日常 note 的第三方 Typst package 配置层。这里维护 `codly` 代码块配置，以及
  pseudocode、diagram、automata、quantum circuit 和 CeTZ canvas 等可选写作别名；
  `M-x my/note-db-sync` 会把 note 根目录的 `/_typst/extension.typ` 链到这里。
- `lisp/note/typst/publish.typ`
  网站发布时的 PDF 样式。`bin/publish-site` 会临时把 note 里的 `"/_typst/note.typ"`
  import 指到 note 根目录的 `/_typst/publish.typ` 软链，所以公开 PDF 的页眉页脚、纸张、标题、目录和卡片视觉都在这里补。
- `lisp/note/assets/`
  网站壳使用的 `css/`、`js/`、`homepage.html` 和 `notes.html` 源文件。`~/HC/Org`
  根目录下的同名静态文件通过软链指向这里；`public/` 仍由发布脚本生成自包含文件。
- `notes/assignment.typ`
  项目外 assignment 模板样式。
- `notes/rho.typ` / `notes/aleph-notas.typ`
  从旧 LaTeX class 迁过来的额外写作样式。

日常 note、publish 样式和网站静态资源都通过项目根目录里的软链消费。改完 Emacs
配置里的源文件后，旧项目不需要复制文件；只要软链存在，重新编译或发布即可吃到新版本。

## 15. 我要改运行时状态目录

文件：

- [lisp/init-base.el](../lisp/init-base.el)

重点变量：

- `my/state-dir`
- `my/backup-dir`
- `my/auto-save-dir`
- `my/lockfile-dir`

现在所有这些都写到 [var/](../var/)。

## 16. 我要改新建文件模板（auto-insert）

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

Typst 模板集中在 [templates/typst/](../templates/typst/)。当前 assignment 模板会
导入项目根目录下的 `/_typst/assignment.typ`；插入模板时 Emacs 会自动创建这些
`_typst/*.typ` 软链，样式源文件统一维护在 [notes/](../notes/) 和
[lisp/note/typst/](../lisp/note/typst/)。

其中 `default.typ` 以及 research / proof / experiment / meeting / writing 等模板是
Typst note 模板，会导入 `/_typst/note.typ`。`academic-report.typ`、
`manuscript-review.typ`、`rebuttal-letter.typ`、`longform-book.typ`、
`project-financial-report.typ`、`lab-report.typ`、`grant-proposal.typ` 是普通独立
Typst 文档模板，不依赖 note helper。`touying-simple.typ` 是 Touying slides 模板，
也保持独立，不进入 note helper。

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

**env 刷新**：`SPC p e r` 在 dir-locals 重载后会调用 `my/direnv-update-environment-maybe`，使 PATH、编译器路径等 shell 层变量在 Emacs 里同步更新，不需要重启。

**`eval` 说明**：`silence` 命令跳过 `eval` 条目（涉及安全确认）。如需 silence `eval` 形式，手动将其加入 `safe-local-eval-forms`。
