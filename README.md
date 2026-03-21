# Aaron's Emacs Config

这是 Aaron 当前长期使用的主力 Emacs 配置。

这份 README 以“现在实际能做什么”为主，不再按某次重构历史组织；如果文档和代码冲突，以代码为准，本文件尽量描述当前真实工作流和环境约定。

## 当前功能概览

- 启动与基础层
  - [early-init.el](early-init.el) 关闭 `package-enable-at-startup`，前置做 GC / `file-name-handler-alist` / 极简 UI 优化。
  - [init.el](init.el) 负责 `package.el`、`use-package`、macOS GUI/daemon 下的 `exec-path-from-shell`、模块装配以及本地 Elisp 编译 helper。
  - [lisp/init-modules.el](lisp/init-modules.el) 按顺序加载功能模块，[lisp/lang/](lisp/lang/) 负责语言专项配置。
- UI 与编辑体验
  - 主题是 `kanagawa-wave`，配 `doom-modeline`、`dashboard`、`shackle`、`tab-bar`、`popper`。
  - 默认启用全局行号、fill-column 指示线、ligature、自定义字体与中文字体绑定。
  - 帮助系统已经切到 `helpful`，多光标编辑使用 `multiple-cursors`。
  - `evil` + `evil-collection` + `evil-surround` 是主工作流，`SPC` 是 leader key。
- 补全与搜索
  - 当前是双栈并存：
    - `vertico` + `orderless` + `consult` + `embark` + `marginalia` 负责主搜索/补全工作流，`C-s`、`C-x C-r` 等高频入口已经切到 Consult。
    - `ivy` + `counsel` 仍保留给少数兼容入口和 Projectile/Org 的旧工作流。
  - 现在额外补了一层 `telescope` 统一入口：用 `transient` 把常用 `consult` picker、项目切换和符号搜索收在一个面板里，并单独给这套入口打开更积极的 preview。
  - `C-x C-f` 已切回原生 `find-file`，配合 `vertico-directory` 和原生文件补全过滤，避免旧的 Ivy 文件选择问题。
- 编程与调试
  - `company` + `company-prescient` + `company-box` 做补全，默认走 `eglot`，少数语言显式切到 `lsp-mode`，诊断统一走 `flymake`。
  - `eldoc-box`、`breadcrumb`、`Treemacs` 提供文档悬浮、breadcrumb 和左侧文件/符号导航。
  - `eglot` 的 inlay hints 现在默认开启。
  - 现在补了一层 `Language Server Hub` / `Doctor`，可以直接看路由、server 映射、当前 buffer 状态、日志入口和 session 级调参。
  - `dape` 负责调试，`citre` 补充 tags/xref，`devdocs`、`quickrun`、`yasnippet`、`hideshow` 都已接入。
  - tree-sitter 相关配置包含 `treesit-auto`、`treesit-fold`，并在打开文件后延迟切到 `*-ts-mode`。
- 项目 / 文件 / 窗口
  - `projectile` + `counsel-projectile` 负责项目识别、文件检索、项目搜索。
  - 现在补了一层项目工作台：`transient` + 自定义 `my/project-*` 命令，把切项目、搜项目、开项目、开 Magit、开 vterm 串成一个工作流。
  - `show-imenu` 会 smart-toggle 左侧 `Treemacs`；有项目时用项目根，没有项目时用当前目录，并自动跟随当前文件和光标所在符号。
  - `Treemacs` 统一走左侧 side window，从树里打开文件默认落到最近访问的编辑窗口，`perspective` 负责按项目切工作区。
  - 文件浏览以 `dirvish` 接管 `dired`，配 `diredfl`、`nerd-icons`、`consult-find` / `consult-ripgrep`。
  - `winner-mode`、`ace-window`、`centaur-tabs` 也都在用。
- Org / 学术写作
  - Org 根目录固定在 `~/HC/Org/`。
  - 有 agenda、capture、refile、org-roam、org-roam-ui、org-ref、cdlatex、AUCTeX、pdf-tools、Jupyter、Zotero/MarginNote 链接支持。
  - Org UI 比较重：`mixed-pitch`、`valign`、`org-modern`、`org-modern-indent`、`org-appear`、`olivetti`、特殊 block overlay、按可见区域触发的 LaTeX 预览。
  - 这些增强默认保持开启，不再按远端或大文件场景主动降载。
- 终端 / 远程
  - 内置了 `shell`、`term`、`vterm` 的终端工作流，日常弹出终端以 popup `vterm` 池为主。
  - `M-x my/vterm-ssh` 会优先读取 `~/.ssh/config` 的主机名，打开专用 vterm 并直接 SSH。
  - TRAMP 做了 PATH、ControlMaster、超时、spinner、持久化文件和 autosave 目录配置。
- 浏览器 / 外部整合
  - `browse-url` 会按 URL 复杂度在 `eww` 和 `xwidget-webkit` 之间分流。
  - 还保留了 EAF 浏览器 / PDF / Git / mindmap 等入口，以及 EWW / Xwidget / EAF 互转函数。
  - `atomic-chrome`、`webjump`、`fanyi`、`rcirc`、`gnus` 都在配置里。
- AI
  - `gptel` 支持默认 ChatGPT / GitHub Copilot 后端，也支持从 `var/mygpt.json` 读取多后端配置、preset、context 和 rewrite 工作流。
  - `copilot` 在 `prog-mode` 和 `org-mode` 下自动启用。

## 仓库结构

- [init.el](init.el)
  Emacs 入口，负责包系统、启动参数和模块加载。
- [early-init.el](early-init.el)
  提前做启动期性能优化和基础 UI 精简。
- [bootstrap.el](bootstrap.el)
  负责从当前环境导出锁文件，或从锁文件恢复依赖。
- [package-lock.el](package-lock.el)
  自动生成的依赖锁文件。
- [docs/](docs/)
  中文使用手册，覆盖安装、日常操作、项目管理、Org、LSP/远程、维护和自定义；另外补了 LSP / Jupyter 的专题 workflow 文档。
- [lisp/](lisp/)
  主配置模块，覆盖 UI、补全、项目、Org、终端、EAF 等。
- [lisp/lang/](lisp/lang/)
  语言专项配置，目前覆盖 C/C++、Rust、Python、OCaml、Haskell、Lean、Nix、Shell、JS、HTML、Markdown、Bazel、Sage、Vale、Elisp 等。
- [site-lisp/emacs-application-framework/](site-lisp/emacs-application-framework/)
  本地 EAF 源码。
- [snippets/](snippets/)
  `yasnippet` 自定义 snippet。
- [tools/](tools/)
  本地辅助脚本，比如 Org LaTeX 预览脚本。

## 常用入口

- `SPC`
  Evil leader，总入口。
- `M-x`
  走原生 `execute-extended-command`，配 `amx` 历史排序。
- `M-x telescope`
  打开统一的 Telescope 风格搜索面板。
- `SPC SPC`
  打开统一的 Telescope 风格搜索面板。
- `SPC SPC I`
  打开当前 workspace / 项目的 symbols；输入关键词后走 `xref-find-apropos` + `consult-xref`，并直接预览候选位置。
- `SPC SPC m`
  打开 bookmark picker；当前项目书签优先，没有书签时打开列表缓冲区。
- `C-x C-f`
  走 `find-file` + `vertico-directory`。
- `C-x b`
  走 `consult-buffer`。
- `C-s`
  走 `consult-line`。
- `C-x C-r`
  走 `consult-recent-file`。
- `C-x g`
  打开 `magit-status`。
- `C-c p`
  Projectile 前缀。
- `C-c p .`
  项目工作台。
- `SPC p .`
  项目工作台（Evil leader）。
- `SPC p o`
  项目工作台式打开当前目标项目。
- `SPC c i`
  打开 `show-imenu` 左侧 Treemacs 导航。
- `SPC c t` / `SPC c n` / `SPC c N` / `SPC c p` / `SPC c T`
  测试菜单 / 当前附近测试 / 当前文件测试 / 当前项目测试 / 重跑上次测试。
- `SPC c .`
  打开 code menu；里面补了 `b` / `B` 做 build / rerun build，常见项目会自动识别 `make`、`cmake`、`ninja`。
- `za` / `zo` / `zc` / `zR` / `zM`
  代码折叠：切换 / 打开 / 关闭 / 全展开 / 全折叠。
- `SPC v v` / `SPC v f` / `SPC v s` / `SPC v b`
  结构选择：扩选、整函数、语句、代码块。
- `SPC b .`
  打开 bookmark 管理菜单。
- `C-x r .`
  打开 bookmark 管理菜单。
- `C-x r j`
  跳转 bookmark，带预览。
- `C-x r l` / `C-x r n` / `C-x r p`
  切换当前行书签 / 下一个行书签 / 上一个行书签。
- `SPC b l` / `SPC b t` / `SPC b n` / `SPC b p` / `SPC b L`
  书签列表 / 切换当前行书签 / 下一个行书签 / 上一个行书签 / 直接设置当前行书签。
- `*Bookmarks*`
  列表里 `RET` 跳转，`D` 删除；当前项目书签优先显示。
- `SPC n a` / `SPC n e` / `SPC n u`
  结构导航：函数开头、函数结尾、跳到外层结构。
- `[f` / `]f`
  上一个 / 下一个函数。
- `C-c p g`
  `consult-ripgrep`。
- `C-c y`
  snippet 前缀，`C-c y y` 展开，`C-c y i` 插入。
- `C-c t`
  打开 `treemacs`。
- `C-c o d`
  `dirvish-dwim`。
- <code>M-`</code>
  弹出/收起当前 popup `vterm`。
- `C-c e`
  弹出/收起当前 popup `vterm`，临时实例失焦自动收起。
- `C-c E`
  在 popup `vterm` 池里切换到下一个实例；`C-u C-c E` 新建一个实例。
- `C-c M-e`
  切换当前 popup `vterm` 的固定状态。
- <code>C-`</code>
  `popper-toggle`。
- `M-x my/vterm-ssh`
  基于 `~/.ssh/config` 选择主机并直接 SSH。
- `M-x my/project-popup-vterm-app`
  在当前项目根目录新建 popup `vterm`，从 `lazygit` / `btop` / `codex` 里选一个运行；应用退出后终端一起退出。
- `C-c a`
  `org-agenda`。
- `C-c c`
  `org-capture`。
- `C-c n`
  `org-roam` 前缀。
- `C-c w e`
  `eww-browse-url`。
- `C-c w x`
  `xwidget-webkit-browse-url`。
- `C-c w w`
  统一的 `browse-url` 入口。
- `SPC l l` / `C-c l l`
  打开 LLM workflow 菜单。
- `SPC l c` / `C-c l c`
  打开 gptel chat。

## 环境约定与依赖

这套配置明显偏 macOS 图形界面环境，Linux 不一定完全不可用，但部分体验是按 macOS 定制的。

- 路径约定
  - Org 根目录固定为 `~/HC/Org/`。
  - GPT 配置文件默认是 `var/mygpt.json`。
  - SSH 主机列表默认从 `~/.ssh/config` 读取。
  - tree-sitter 动态库目录固定为 `tree-sitter/`。
  - 备份、自动保存和 lock 文件统一写到 `var/` 下，不再污染项目目录。
- 外部工具 / 应用
  - `gls`：`dirvish` / `dired` 在 macOS 下按 GNU ls 的参数工作。
  - `terminal-notifier`：macOS 通知使用它。
  - Brave：`xwidget-webkit` 和 EAF 里写死了 Brave 的 cookie/cache 路径。
  - EAF：默认从 `site-lisp/emacs-application-framework/` 本地加载。
- 字体
  - 当前配置直接引用 `Merriweather`、`Fira Code`、`Excalifont`、`FZLiuGongQuanKaiShuJF`、`JetBrainsMono Nerd Font`。
  - 缺字体不会阻止启动，但界面效果会和当前配置作者机器明显不同。

## 包管理与 Bootstrap

普通包：

- 主要通过 `use-package` 的 `:ensure t` 或 `package-install` 安装。
- 最终会记录到 `package-selected-packages`。

VC 包：

- 统一通过 [lisp/init-package-utils.el](lisp/init-package-utils.el) 里的 `my/package-ensure-vc` 或 `my/package-register-vc` 声明。
- 这样 `bootstrap.el` 才能把它们写进 `package-vc-selected-packages`，并导出到 [package-lock.el](package-lock.el)。

更新或恢复依赖时执行：

```sh
make install
```

或者：

```sh
emacs --debug-init -q -l ./bootstrap.el
```

`bootstrap.el` 的实际策略是：

- 如果当前机器已经装了足够多的第三方包，就把它当成“已有环境”，加载 [init.el](init.el) 后重写 [package-lock.el](package-lock.el)。
- 如果当前机器几乎没有第三方包，就把它当成“新环境”，从 [package-lock.el](package-lock.el) 安装 archive 包和 VC 包。

新增 VC 包时不要手写散落的 `package-vc-install`，请用：

```elisp
(my/package-ensure-vc 'some-package "https://github.com/owner/repo.git")
```

如果要固定 revision：

```elisp
(my/package-ensure-vc 'some-package "https://github.com/owner/repo.git" "commit-or-tag")
```

## 当前已知现状

- 现在仍然是 `ivy/counsel` 和 `vertico/consult/embark` 双栈并存，但高频搜索入口已经优先走 `consult`。
- 一些路径和应用绑定是硬编码的，比如 `~/HC/Org/`、Brave 数据目录、`var/mygpt.json`。
- 这套配置优先照顾“本地编码 + SSH/TRAMP + Org 笔记/学术写作”三类日常使用，不追求最小依赖或跨平台零定制。
