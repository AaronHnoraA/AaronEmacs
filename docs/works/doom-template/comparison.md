# Doom 与本配置的逐项对比（P1d、P1e）

本文档对照了 Doom 的全局默认值、leader 布局、模块目录和延迟加载做法，记录本配置与它们的差异。比较时，本配置一侧取的是迁移后整套配置完整启动后的运行时值。

原则：用户习惯和原有逻辑不改，本阶段只优化后端。凡是用户会感觉到的差异，这里只列为候选，等用户逐项决定；用户感觉不到的部分才直接做。

## 1. 模块目录对照（H）

Doom 的模块与本配置模块的对应关系，按 Doom 的分类排列。语言类只列本配置用到的语言。

| Doom 模块 | 本配置 | 备注 |
| --- | --- | --- |
| `:completion vertico` / `company` / `corfu` | `completion/minibuffer`（vertico + consult）、Company 在 `tools/lsp` | 一致：补全前端只有一个 |
| `:ui doom` / `modeline` / `dashboard` | `ui/theme`（aaron-ui 主题、doom-modeline、dashboard） | Doom 拆成三个模块，本配置合在一个模块里 |
| `:ui popup` | `ui/popup`（shackle） | 已采用 `set-popup-rule!`，见“注册接口” |
| `:ui workspaces` / `tabs` | `ui/workspaces`（Perspective）、`ui/tabbar` | 一致 |
| `:ui treemacs` / `vc-gutter` / `hl-todo` | `ui/treemacs`、`tools/git`（diff-hl）、`editor/fold` 中的 hl-todo | 一致 |
| `:ui window-select` / `nav-flash` | `ui/windows`（ace-window）、`editor/navigation`（pulse） | 一致 |
| `:ui ligatures` / `indent-guides` / `zen` / `minimap` / `unicode` / `emoji` | 部分在 `ui/theme`（ligature） | 其余未采用，属于外观选择 |
| `:editor evil` / `snippets` / `file-templates` / `fold` / `format` / `multiple-cursors` | 同名模块 | 一致 |
| `:editor whitespace` / `word-wrap` / `rotate-text` / `lispy` / `parinfer` | `editor/format`（ws-butler）、`editor/smartparens` | 结构编辑用 smartparens，没有 lispy/parinfer |
| `:emacs dired` / `ibuffer` / `undo` / `vc` / `tramp` / `electric` / `eww` | `emacs/dired`、`emacs/dirvish`、`aaron-base`（ibuffer、undo-tree）、`tools/git`、`remote/framework`、`app/browser` | TRAMP 位于 Remote 框架之下，比 Doom 强 |
| `:term vterm` / `eshell` / `ghostel` / `shell` | `term/portal`（Ghostty）、`term/shell` | 一致：Portal 加 Ghostel |
| `:checkers syntax` / `spell` / `grammar` | `checkers/diagnostics*`（flymake）、`checkers/spell`、`checkers/grammar`（LanguageTool） | 一致 |
| `:tools lsp` / `lookup` / `eval` / `debugger` / `direnv` / `magit` / `tree-sitter` / `pdf` / `biblio` / `llm` / `editorconfig` / `make` | `tools/lsp*`、`editor/navigation`（xref）、`tools/run`、`tools/debugger`、`tools/direnv`、`tools/git`、`tools/tree-sitter`、`lang/auctex`（pdf-tools）、`lang/latex`（Zotero）、`tools/agent`、`config/default`（editorconfig）、`tools/task` | 查找走 xref 后端链，不另建 handler 表 |
| `:tools docker` / `ansible` / `terraform` / `pass` / `upload` / `tmux` / `collab` / `ein` | 无，或由 Remote/Jupyter 覆盖 | ein 的位置由 Noema Jupyter 桥替代 |
| `:lang org` / `markdown` / `latex` / `python` / `cc` / `rust` / `java` / `lean` / `nix` / `sh` / `web` / `javascript` / `haskell` / `ocaml` / `beancount` / `emacs-lisp` | `lang/*` 同名或对应模块 | Markdown 由 Noema 接管 |
| `:os macos` / `tty` | `os/macos`、`os/neomacs` | 一致 |
| `:app calendar` / `rss` / `irc` / `emms` / `everywhere` | `app/calendar` | 其余未采用 |
| `:email *` | 无 | — |
| `:input chinese` / `japanese` / `layout` / `bidi` | 无 | 第二阶段 neo-emacs 的 rime 落在这里（`:input rime`） |
| `:config default` / `literate` | `config/default`（Doom 风格默认值）、各 `config/*` | 一致；不采用 literate 配置 |

本配置独有、Doom 没有的模块：

- `:remote`：底层 Remote 框架。
- `:noema`：Noema 桥。
- `tools/project*`、`tools/jupyter`、`config/health`、`config/performance`、`config/background`、`config/maintenance`、`ui/activity`。

## 2. 全局默认值（E）

比较方法：取 Doom `doom-emacs.el` 和 `:config default` 里设置的 135 个变量。其中 68 个本配置也设置或用到。另外 67 个本配置没设置，对这 67 个读取本配置启动后的值。剔除以下几类之后，剩下的是真实差异：

- 已由 no-littering 处理的状态文件路径；
- 平台无关的变量；
- 只在 which-key 加载后才存在的变量。

| 变量 | Doom | 本配置现值 | 影响 |
| --- | --- | --- | --- |
| `find-file-suppress-same-file-warnings` | `t` | `nil` | 打开同一文件的不同路径（软链）时不再提示 |
| `minibuffer-prompt-properties` | 加 `cursor-intangible` | 只读 | 光标不能进入 minibuffer 提示符 |
| `split-width-threshold` / `split-height-threshold` | `160` / `nil` | `150` / `80` | `display-buffer` 偏向左右分屏、不再上下分 |
| `truncate-partial-width-windows` | `nil` | `50` | 窄窗口不再强制截断长行 |
| `display-line-numbers-width` / `display-line-numbers-widen` | `3` / `t` | `nil` / `nil` | 行号列宽固定；narrow 后仍按全文编号 |
| `blink-matching-paren` | `nil` | `t` | 输入闭括号时不再跳光标闪烁 |
| `show-paren-delay` | `0.1` | `0.125` | 括号高亮略快 |
| `comint-buffer-maximum-size` | `2048` | `1024` | REPL 保留更多行 |
| `ediff-diff-options` | `"-w"` | `""` | ediff 忽略空白差异 |
| `revert-without-query` | `'(".")` | `nil` | 外部改动的未修改文件直接重载、不询问 |
| `frame-title-format` | `"%b – Doom Emacs"` 风格 | `"%b"` | 外观差异，不必采用 |
| `shift-select-mode` | `nil` | `t` | evil 下影响很小 |

这些都会被用户感觉到，所以只列为候选。

## 3. 延迟到首次使用（Doom 的启动优化）

Doom 不在启动时开启这些模式，而是挂到生命周期 hook 上（本配置已经有这些 hook，见 `aaron-emacs`）。本配置现在都在 `after-init` 开启：

| 模式 | Doom 的时机 | 本配置 |
| --- | --- | --- |
| `recentf-mode` | `first-file` | `after-init`（`aaron-base`） |
| `savehist-mode`、`save-place-mode` | `first-input` | `after-init`（`aaron-base`） |
| `show-paren-mode` | `first-buffer` | `after-init` |
| `global-hl-line-mode` | `first-input` | `after-init` |
| `global-so-long-mode` | `first-file` | `after-init` |
| `which-key-mode` | `first-input` | 模块加载时（`tools/menus`） |
| `global-auto-revert-mode` | `first-file` | 模块加载时（`config/default`） |
| `gcmh-mode` | `first-buffer` | `after-init`（`tools/menus`） |

模块账本还发现两处启动期间的重加载：

- `lang/auctex` 本应在 TeX/PDF 之后才加载，但 `tex-site` 和 `pdf-tools` 在启动时就被加载，结果它约 0.9s 的初始化落在了启动期间。
- `lang/org` 跟随 org 加载，而 org 在启动时就被加载了。

Doom 的做法：

- 把重包放进 `aaron-load-packages-incrementally`，空闲时分块加载；
- 把模式挂到 `first-*` hook 上。

这样做后，用户看到的功能不变，变化的只是“第一次用到时才就绪”的时机。改动需要用户确认，因为首次打开文件或首次按键那一刻会多付出这部分成本。

## 4. leader 布局（F）

| 键 | Doom | 本配置 |
| --- | --- | --- |
| `SPC SPC` | 项目内找文件 | `telescope` 统一搜索面板 |
| `SPC TAB` / `SPC t` | 工作区 / toggle | 工作区 / 工作区（`t` 含义不同） |
| `SPC b f p g s o q h c w n r` | buffer、file、project、git、search、open、quit、help、code、window、notes、remote | 同组含义基本一致（`n`、`N` 是 Noema） |
| `SPC d` | debugger | 未绑定（调试在 `SPC c j`） |
| `SPC i` | insert | 未绑定 |
| `SPC : ; . , \` * / x X u RET ~` | M-x、eval、找文件、切 buffer、上个 buffer、项目搜索、scratch、capture、universal-arg、书签、popup 开关 | 只有 `;`（avy-resume）、`'`（vertico-repeat） |
| — | — | 本配置独有：`a`（app）、`e`（edit）、`F`（frame）、`j`、`v` / `V`（选区）、`z`、`m`（localleader） |

候选：只把 Doom 那些在本配置里没被占用的键（`d . , \` * / x u RET ~`）作为新增绑定，不改动任何已有键。是否采用由用户决定。

## 5. 注册接口（D）

做法：按 Doom 的 `set-…!` 形式整理本配置里已经存在、但分散在各处的注册代码，没有消费者的接口不加。具体是：

- **`set-popup-rule!` / `set-popup-rules!`**：在 `ui/popup` 的 autoload 里。各模块注册自己的弹窗规则，默认规则由 `ui/popup` 注册。
- **`set-evil-initial-state!`**：在 `editor/evil` 的 autoload 里。模块不再自己写 `with-eval-after-load 'evil`。Remote 框架层不依赖编辑器层，所以继续直接调用。
- **语言服务器注册改名为 `set-…!`**：`set-language-server!`、`set-language-server-feature!`、`set-language-server-runtime-provider!`、`set-language-server-toolchain-provider!`；从未定义、只有 `fboundp` 保护着调用的 `set-lsp-mode-preference!` 两处死调用删除。
- **没有加的接口**：
  - `set-lookup-handlers!`：查找已经是 Emacs 自己的 `xref-backend-functions` 链，再加一层会和它重复；
  - `set-formatter!`：格式化只走 LSP；
  - `set-file-template!`：模板本身就是按类型分的目录数据；
  - `set-company-backend!`：原有的同类宏 `set-company-backends-for!` 没有任何调用者，直接删掉，不改名。

## 6. 启用模块的实现对照

对照的是本配置实际使用的领域；Doom 的未启用语言、第二套补全前端及另外的
终端运行时不进入本轮。保留现有按键、窗口入口和输入方式。

| Doom 实现 | 本配置现状与处理 |
| --- | --- |
| `ui/popup` | Shackle/Popper、Portal 生命周期和 `set-popup-rule!` 已覆盖；Doom 的整套 popup 窗口管理会重叠，不移入。 |
| `ui/workspaces` | Perspective 的切换前 hook 清掉离开 buffer 的 Evil Visual 和普通选区；现用 Perspective 包已经按工作区隔离 Winner，删掉移植自另一种 `persp-mode` 的无效 Winner 函数。自动保存、关 frame 时删工作区、过滤远程 buffer 与现有契约冲突。 |
| `ui/tabs`、`ui/modeline` | 顶栏是原生 tab-bar/tab-line，Doom 的 Centaur Tabs 是另一套 UI；modeline 已有 doom-modeline 和 neo 的 Evil 状态箭头。保留现有布局与鼠标行为。 |
| `ui/vc-gutter` | diff-hl 已异步更新，较高行的竖线修复已从 neo 吸收。Doom 的已暂存改动显示、实时刷新和左右 fringe 选择会改变现有显示；不改。 |
| `ui/treemacs`、`ui/window-select`、`ui/nav-flash` | 现有 Treemacs 工作区桥、ace-window、pulse 已覆盖相应入口。`nav-flash` 使用的 `persp-activated-functions` 属于另一种包；本配置中同名挂钩不运行，连同 tab line 和 tab 同步里的两处无效挂钩删掉，不新增闪烁。 |
| `editor/evil`、`multiple-cursors` | Evil/evil-collection 与多光标已有自己的键表和状态收尾；Doom 的按键、初始状态及逃逸策略会改变手感。 |
| `editor/fold`、`format`、`word-wrap`、`whitespace` | 折叠已含 hideshow/treesit 与持久状态，格式化走 LSP，空白清理走 ws-butler。Doom 的 Apheleia、自动缩进猜测和换行策略会改变编辑结果。 |
| `editor/snippets`、`file-templates` | Yasnippet 和模板入口已有；neo 的适用片段已经逐个处理。Doom 的全局 Yas 启动与模板规则不移入。 |
| `completion/vertico`、`company`；`checkers/syntax`、`spell`、`grammar` | 现有 Vertico/Consult、Company、Flymake 和拼写/语法链保持单一前端。Doom 的 Corfu、Flycheck 和提示位置/延迟设置不叠加。 |
| `tools/lookup`、`eval`、`lsp`、`direnv` | 查找用 xref 后端链，求值与 LSP/Remote 环境已有各自路由。Doom 的第二套 lookup/eval 分发和 envrc 会重复运行。 |
| `tools/magit` | 已有同窗显示、Magit 与 diff-hl；当前 Projectile 为 `alien` 索引且缓存关闭，Doom 的状态刷新后清缓存逻辑没有收益。其自动回退和 buffer 重命名策略会改变现有行为。 |
| `tools/debugger`、`tree-sitter`、`pdf`、`editorconfig` | Dape 状态目录启动时已建立；treesit、PDF、EditorConfig 均已有所有者。Doom 的断点目录补丁、grammar/字体/PDF 展示参数不照搬。 |
| `emacs/dired`、`ibuffer`、`undo`、`vc`、`tramp`、`electric`、`eww` | Dirvish 20260725 已内建 Doom 的多 frame 重绘保护；Dired、ibuffer、undo-tree 和远程 VC/TRAMP 有现有路由。Doom 的 undo 数据剥去文本属性、目录操作默认值、RET/浏览器键位会改变结果。 |
| 使用中的 `lang/*` | Python、C/C++、Rust、Shell、Web/JS、LaTeX 的 mode/LSP/缩进已有模块管理；Doom 的 REPL、格式化器、缩进、快捷键差异属于编辑行为，本轮不覆盖。Noema 接管 Markdown/笔记视图。 |
| `term/ghostel`、`os/macos` | 终端走 Portal 原生 Ghostty；Doom 的 Ghostel buffer 模式补丁不适用。macOS 的回收站、frame 复用等设置已在本配置。 |

验证：工作区切换的 Visual 与普通选区各有一个 ERT 测试；工作区 8 项、frame/tab
16 项、LSP UI 30 项通过。启动状态对比中，新增的有效切换前 hook 是预期行为；
其余差异只是删除未由当前 Perspective 调用的旧挂钩。

## 7. 内核行为逐行对照（`doom-emacs.el`）

第 2 节只比了变量；这里逐个看 `doom-emacs.el` 里的函数、hook 和 advice。

| Doom | 本配置 | 处理 |
| --- | --- | --- |
| `auto-save-file-name-transforms` 用 sha1 命名；`make-backup-file-name-1` 对长路径取哈希 | 用可读的 `!` 拼接全路径 | **吸收并改良**：只有超过 180 个字符的名字才哈希（给 diff-hl 在后面加的 40 位提交号留出余量）（`aaron-state-file-name-fit`，同时管 auto-save、lock、backup）。实测路径拼出的名字超过 APFS 的 255 字符上限时，Emacs 不报错地跳过自动保存和文件锁 |
| `savehist-save-hook` 去掉历史变量的文本属性、去掉不能打印的寄存器 | 配了保存 `kill-ring`、`mark-ring`、`global-mark-ring`，实际一个都没存下 | **吸收并改良**：savehist 遇到一处不可打印的值就整个丢掉变量；mark ring 是 marker，永远存不下，删掉；kill ring 只在 savehist 的临时 buffer 里换成无属性副本（`aaron-savehist-plain-kill-ring-h`），不像 Doom 那样改动会话里的值。寄存器本配置不保存，不需要 |
| daemon 下 `recentf-autosave-interval 1200` | 只在正常退出时保存最近文件 | **吸收**：本配置的 Emacs 作为后台 app 一次运行几周，改为每 600 秒保存，登记为 config 设置；registry 的 Customize 桥会替换 recentf 自带的 setter，所以在 `:set` 里自己重启 recentf 的计时器 |
| `doom-guess-mode-h`：保存 `fundamental-mode` 文件后重新判断模式 | 没有 | **吸收**：`+files-guess-mode-h`（`emacs/files`），同样只作用于当前窗口的 buffer；Evil 状态由已有的 `set-auto-mode` advice 保持 |
| `auto-save-include-big-deletions t` | Emacs 默认 `nil`：删掉一大段后关掉该 buffer 的自动保存 | **吸收**：登记为 config 设置，值 `t`；原文件仍在磁盘上，自动保存不该因为删除而停 |
| `bidi-display-reordering 'left-to-right`、`gnutls-*`、`tls-program` | `t`；NSM `medium` | **不收**：前者文档里只有 `nil` 特殊，`'left-to-right` 与 `t` 等价；TLS 由 NSM 交互确认，Doom 只在 batch 下报错 |
| `minibuffer-prompt-properties` 加 `cursor-intangible`，minibuffer 里开 `cursor-intangible-mode` | 只读提示符，光标能进去 | **吸收**（第 2 节的候选）：光标进了提示符只会得到 “Text is read-only”，没有用途；放在 `completion/minibuffer` |
| `echo-keystrokes`、`blink-matching-paren`、`display-line-numbers-widen`、`split-*-threshold`、`window-divider` | 各有本配置的值 | **不收**：属于手感和外观，第 2 节已列为候选，保持现状 |

### `editor/evil` 的补丁

| Doom | 处理 |
| --- | --- |
| `+evil-join-a`：`J` 去掉注释符 | **吸收并简化**：只在 `evil-join` 期间给 `join-line` 加前置动作，用 `comment-search-forward` 和语法状态判断；C 块注释中间行的 `*` 也去掉。对齐 Neovim 默认的 `formatoptions` `j` |
| `gq`/`gw` 不压缩空格 | **吸收**：Vim 本来不压缩 |
| `evil-ex-search` 后 `isearch-clean-overlays` | **不收**：修的是 org-fold 的折叠状态，本配置不用 Org |
| `evil-visual-update-x-selection-p nil` | **不收**：macOS 上 PRIMARY 是 Emacs 内部模拟的，不起进程，也不碰系统剪贴板 |
| `=` 后光标不动、2–9 号 mark 全局、`o`/`O` 续写注释、分屏后居中 | **不收**：都偏离原生 Vim 的行为 |
| Ex 文件名修饰符 `%:p:h` | **不收**：Doom 用 `:override` 换掉 `evil-ex-replace-special-filenames`，丢了 Evil 对远程文件取 localname 的处理（`/fs:` buffer 里的 `:!` 会拿到错误路径）；找同目录文件本配置走 Consult 和 Dirvish |

### `ui/vc-gutter` 的补丁

| Doom | 处理 |
| --- | --- |
| diff-hl 在 TRAMP buffer 上生成临时文件时问 “unsafe temporary file” | **不需要**：本配置的 auto-save 规则把所有名字放进 `var/auto-save`，不落到 `temporary-file-directory` |
| `diff-hl-make-temp-file-name` 在 auto-save 名后接 `.~提交号.~` | **发现**：可读名字上限因此从 230 降到 180（`aaron-state-file-name-max-length`） |
| `diff-hl-revert-hunk` 后恢复光标、弹窗按内容缩小 | **不收**：本配置的回退弹窗走 popup 规则，光标跳动没有复现 |
| `vc-git-diff-switches --histogram`、切窗口/ESC 时刷新、flydiff | **不收**：已是异步更新加 Magit 刷新 hook；flydiff Doom 在 macOS 上也关着 |

### `tools/magit` 的补丁

| Doom | 处理 |
| --- | --- |
| `magit-diff-visit-file-hook` 里 `reveal-post-command` | **吸收并推广**：跳转落进折叠时打开挡住光标的折叠（`+fold-reveal-point-h`，在 `editor/fold`），挂在 xref、imenu、better-jumper 和 Magit 访问文件上；hideshow 和 treesit-fold 的 overlay 都带 `isearch-open-invisible`，Consult 已自己处理 |
| `magit-status-mode` 里 `long-line-threshold nil`（magit#5320） | **吸收** |
| `magit-uniquify-buffer-names nil`、`magit-save-repository-buffers nil`、`+magit-display-buffer-fn`、`q` 杀 buffer、fringe 加宽 | **不收**：改变 buffer 名、保存提示和窗口布局，属于现有习惯；Magit 按目录而不是名字找已有 buffer，同名项目不会互相覆盖 |
| Magit 刷新后清 Projectile 缓存 | **不收**：第 6 节已记，Projectile 是 `alien` 索引且不缓存 |

### 其余启用模块的补丁

| Doom 模块 | 补丁 | 处理 |
| --- | --- | --- |
| `tools/pdf` | pdf-view 里 Evil normal 光标置空，避免页面闪烁（core#1107） | **吸收**：放在 `+pdf-view-setup-interaction` |
| `completion/company` | company-box 检查子 frame 是否还活着、子 frame 不要 tab-bar、补全后触发 eldoc | **已在上游**：当前 company-box 的 `company-box--get-frame` 已检查 `frame-live-p`，`company-box-frame-parameters` 已有 `tab-bar-lines 0`，eldoc 命令表里已有补全命令 |
| `emacs/tramp` | 编译时保留 SSH ControlMaster、Magit 的 `magit-tramp-pipe-stty-settings 'pty`、LSP 关掉 direct async | **已有**：`remote/framework/tramp.el` 已处理前两项；LSP 走 Remote 框架的进程路由 |
| `emacs/dired` | Dirvish 多 frame 重绘去抖、切工作区前关 Dirvish 会话 | **已在上游**（Dirvish 20260725）/ 工作区审计已处理 |
| `editor/snippets` | 补全文档预览时禁止 snippet 弹出询问 | **不需要**：只有 Org 片段会询问，本配置不用 Org；预览错误已由 `+guard-doc-buffer` 吞掉 |
| `emacs/undo` | undo-fu 会话文件名哈希、undo-tree 可视化 diff 窗口 | **不需要**：本配置的 undo-tree 历史用“标签 + SHA-1”命名；diff 窗口默认不开 |
| `emacs/vc` | `vc-ignore-dir-regexp` 加 `node_modules`、git-timemachine 着色 | **不收**：前者让 `node_modules` 下的文件失去 VC 信息；本配置没有 git-timemachine |
| `tools/tree-sitter` | 30.x 的 ts-mode 改写 `auto-mode-alist`、grammar 目录 | **不需要**：31.1 已修；grammar 已在 `var/` |
| `editor/whitespace` | 子 frame 不开 whitespace-mode | **不需要**：本配置只在访问文件的 buffer 里开 |
| `tools/direnv`、`term/ghostel` | envrc 提前到 major mode body 之后、ghostel 清 `DIRENV_*` | **不适用**：环境走 Remote 环境胶囊；后台 Emacs 不从 direnv 环境启动 |
| `editor/multiple-cursors`、`tools/eval`、`ui/treemacs` | evil-mc 初始化、quickrun 结果放 overlay、treemacs-nerd-icons 加载顺序 | **不适用**：本配置用 multiple-cursors、quickrun 弹窗和自己的 Treemacs 主题 |
| `ui/hl-todo` | 增加 REVIEW、DEPRECATED、BUG 关键字 | **不收**：外观 |
| `lang/emacs-lisp` | apropos 结果的按钮改用 helpful | **吸收**：本配置已把 `describe-*` 命令换成 Helpful，但 apropos 按钮直接调用函数，仍打开普通 `*Help*`；放在 `ui/theme` 的 helpful 配置旁 |
| `lang/python`、`lang/cc`、`lang/beancount`、`lang/rust`、`lang/nix` | `PYTHON_BASIC_REPL`、C 模式 remap 的空项、bean-check 的 flymake 修正、rust-analyzer 悬停缓存、nix 补全延迟 | **不需要**：Emacs 31 的 python.el 已设 `PYTHON_BASIC_REPL`；本配置 `c-mode` 每次都 remap 到 `c-ts-mode`；Beancount 走 language server 而不是 flymake-bean；本配置没有 company-nixos-options；悬停缓存问题无法在本配置复现 |

### `config/default` 的命令

| Doom | 处理 |
| --- | --- |
| `+default/yank-buffer-path-relative-to-project` | **吸收**：`file-copy-project-paths`（`file-menu` 的 `r`），沿用文件菜单的选择规则（Dired 选区、标记、当前文件），用项目的规范写法比较 |
| `+default--delete-backward-char-a`（智能退格）、`newline-above/below`、`yank-pop`、`man-or-woman` | **不收**：退格手感改变；其余已有对应（`SPC e o/O`、Consult、`man`） |
| `+default/search-*`、`+default/browse-*` | **已有**：telescope 统一搜索面板与项目工作台 |
| `completion/vertico` | CRM 提示前缀、`vertico-directory-tidy`、Consult 异步参数、Embark 导出、company 补全时改用 basic 优先的 styles | **已有或不收**：Emacs 31 的 `crm-prompt` 自带 `[%d] %p`；其余参数本配置已是相同值；company 的 styles 是本配置有意按类别设置的 |
| `tools/lsp` | 最后一个 buffer 关掉后推迟 3 秒再关 server | 先记为缺口 G-L1，后已实现（第 21 节） |
| `tools/lsp` | GC/IPC 优化模式、ESC 关签名、`lsp-ui-doc` 尺寸 | **已有或不收**：本配置 `read-process-output-max` 已是 4MB、GC 由 gcmh 管；ESC 已有 `+lsp-dismiss-popup-h`；doc 尺寸是外观 |

## 8. 内核库逐文件对照（`core/lisp/lib/`）

| 文件 | 内容 | 处理 |
| --- | --- | --- |
| `text.el` | `doom/set-indent-width`、`toggle-indent-style`、`retab` | **吸收**：`editing-menu` 的 Indentation 列（`+editing/set-indent-width` 借用 Emacs 自带 EditorConfig 的“模式 → 缩进变量”表，ts-mode 和 `evil-shift-width` 一并设置） |
| `text.el` | `syntax-ppss` 记忆化、`point-in-comment/string-p`、region 辅助、`backward-to-bol-or-indent`、`forward-to-last-non-comment-or-eol` | **已有或不需要**：行首/行尾在注释与缩进之间切换由 `mwim` 提供；其余是 Doom 自己命令的辅助 |
| `text.el` | `delete-backward-word`（不进 kill ring）、`backward-kill-to-bol-and-indent`、`delete-trailing-newlines`、`dos2unix`/`unix2dos` | **不收**：前两项改变删除手感；后两项分别是 `delete-blank-lines` 和 `C-x RET f` |
| `buffers.el` | `doom/kill-buried-buffers` | **吸收**：`kill-buried-buffers`（`+windows/kill-buried-buffers`），后台 Emacs 一次运行几周，用来清掉没在任何窗口里显示的 buffer |
| `buffers.el` | real/unreal buffer 判定、fallback buffer、`kill-other-buffers`、`kill-matching-buffers`、`kill-project-buffers`、`save-and-kill-buffer` | **已有**：`+buffer-single-instance-p` 与窗口生命周期规则、`SPC b K`、Emacs 的 `kill-matching-buffers` 和 `project-kill-buffers` |
| `files.el` | `doom-files--update-refs`：删除、移动文件后更新 recentf、VC、Magit、Projectile 缓存 | **吸收并修正**：`SPC f D` / `SPC f R` 之后按名字从最近文件里去掉旧路径（换成新路径）并刷新 VC 状态。审计中发现 `+delete-current-file` 调 `delete-file` 时没传 TRASH，`delete-by-moving-to-trash` 开着也直接永久删除；改为和 `delete-file`、Dired 一样默认进废纸篓，前缀参数才永久删除 |
| `files.el` | `doom/sudo-*`、`copy/move-this-file`、`file-size`、`glob`、`file-cookie` | **已有或不需要**：`+default/sudo-*`、`+copy-current-file`、`+rename-current-file`；其余是 Doom 自己的工具函数 |
| `ui.el` | 最大化/放大窗口、间接 narrow、退出确认、`shut-up` advice | **已有或不收**：`SPC e 1`、`narrow-or-widen`、`clone-indirect-buffer`、`shut-up!`；退出确认会挡住 Raycast 退出后台 Emacs |
| `help.el` | `doom/help-search-load-path` | **吸收**：`elisp-search-load-path`（`+elisp/search-load-path`），把 `load-path` 上的目录整组交给 `consult-ripgrep` |
| `help.el` | `describe-active-minor-mode`、`describe-char` 取鼠标位置、`describe-package`/`describe-autodef`/`describe-module`、报 bug | **不收**：`describe-mode` 已列出启用的 minor mode；其余是 Doom 自己的模块和包系统的帮助 |
| `fonts.el`、`themes.el` | 字号增减、big-font 模式、`custom-set-faces!`、重载主题 | **已有**：`aaron/apply-font-config` 跟随字号变化，Emacs 29 的 `global-text-scale-adjust`，`+theme-normalize-faces` 在换主题后修 face |
| `sessions.el` | 保存/恢复会话、`doom/restart` | **已有**：`ui/session`；重启走 `bin/emacs-app restart` |
| `debug.el` | `doom-debug-mode`（批量打开调试变量）、`doom/info`、profiler 开关 | **已有或不需要**：`+performance/watch` 与 profiler 入口；`config-health` 输出环境信息 |
| `projects.el`、`config.el` | 在其他项目找文件、浏览配置目录、`doom/reload` | **已有**：项目工作台、`+project/switch`；重新加载走模块系统 |
| `store.el`、`system.el`、`process.el`、`plist.el`、`strings.el`、`git.el`、`packages.el`、`sandbox.el` | 持久化 KV、系统识别、进程辅助、plist/字符串工具、Doom 自己的 git 与包、沙盒 | **不需要**：对应本配置的 config 注册表、`os/*` 层、Remote 进程 API、`package-lock.el`；沙盒是 Doom CLI 的功能 |

## 9. 框架文件对照

| 文件 | 内容 | 处理 |
| --- | --- | --- |
| `doom-lib.el` | 宏与基础函数 | **已移植**（P1a）；`load!`、`add-load-path!`、`doom-lookup-key`、`versionp!`、关键字工具、context 栈、profile 路径没有消费者：本配置的模块按 feature 路径 `require`，没有 Doom 的私有目录和 profile |
| `doom-modules.el` | 模块声明、flag、加载顺序 | **已移植**（P1b，`docs/design/module-system.md`）；补上 `doom-module-at-point`：`aaron-module-at-point` 让 `aaron/describe-module`、`aaron/load-module` 以光标处的模块为默认值 |
| `doom-packages.el`、`doom-straight.el`、`doom-elpaca.el` | straight/elpaca 包管理 | **不收**：`AGENT.md` 规定只用 `package.el` + `package-lock.el` |
| `doom-cli.el`、`doom-profiles.el` | `doom sync/doctor/upgrade`、多 profile | **已有对应**：`make` 目标与 `module-doctor`；单 profile |
| `doom-docs.el` | Doom 文档的 Org 阅读模式 | **不需要**：文档是 Markdown，入口是文档中心 |
| `doom.el`、`early-init.el` | 启动优化、目录变量、native-comp 临时目录 | **已有**：`early-init.el` 已在启动期间清空 `file-name-handler-alist`、关掉隐式 frame 调整；运行时文件在 `var/` |

## 10. 语言模块逐个对照

| Doom 模块 | 内容 | 处理 |
| --- | --- | --- |
| `lang/sh` | `.bats`、`.zunit`、`.env`、`#compdef` 开头的文件进 `sh-mode` | **吸收** `.bats`、`.zunit`、`#compdef`（`.env` 本配置归 `conf-mode`） |
| `lang/sh` | 引号里变量着色、常用命令着色、函数名可带 `-` 的 imenu、`sh-set-shell` 静音、`sh-indent-after-continuation 'always`、反引号配对、company-shell | **不收**：着色和续行缩进是外观与缩进风格；补全走 bash-language-server |
| `lang/javascript` | `.mts`/`.cts` 走 TypeScript 模式；Node 栈帧 `at fn (file:行:列)` 作为编译错误可跳转 | **吸收**：之前 `.mts`/`.cts` 打开是 `fundamental-mode`；`+typescript-module-file-mode` 在访问时取 `.ts` 当前对应的模式；`node` 加进 `compilation-error-regexp-alist` |
| `lang/javascript` | `.es6`/`.pac`、`js-chain-indent`、`package.json` 为项目根、忽略 `node_modules`、nodejs-repl、npm 项目模式 | **已有或不收**：Projectile 已忽略 `node_modules`、项目根由本配置的根解析决定；链式缩进是风格；REPL 与 npm 模式没有使用者 |
| `lang/web` | emmet、Jekyll/Django/WordPress/Angular/React 项目模式、`+html`/`+css` | **不收**：用户不写这些框架的模板；Vue/CSS 缩进已在 neo 审计中处理 |
| `lang/cc` | `find-sibling-rules` 在源文件和头文件之间切换 | **吸收**：本配置的规则只连接源文件和 `_test`/`_benchmark`，`SPC f o` 从 `foo.cpp` 到不了 `foo.h`；补上同目录 `.c/.cc/.cpp/.cxx` ↔ `.h/.hh/.hpp/.hxx` |
| `lang/cc` | `.h` 按内容判断 C/C++/ObjC、ffap 头文件路径、doom 缩进风格、cmake/glsl/cuda | **不收**：`.h` 走 Emacs 的 `c-or-c++-mode`；缩进风格不改；头文件跳转走 clangd |
| `lang/python` | `python3` 作解释器、`Pipfile`/`.flake8` 为 conf、pytest 键、nose、pyvenv、pip-requirements | **已有或不收**：解释器由项目 toolchain profile 决定；`Pipfile`、`.flake8` 已是 `conf-mode`；测试走 `test-nearest` |
| `lang/rust` | rust-analyzer 悬停签名的提炼方法、`rust-indent-method-chain`、rustic | **吸收并推广**：发现本配置用 `+rust-analyzer`、`+clangd` 注册，lsp-mode 按 `rust-analyzer`、`clangd` 分派的签名提炼从未生效（Rust 类型的 echo area 显示 `// size = …` 注释行）。`set-language-server!` 加 `:upstream-id`，共享 adapter 按它分派；Doom 自己那段 rust 签名解析依赖 rust-analyzer 输出格式，不收 |
| `lang/emacs-lisp` | `elisp-flymake-byte-compile` 带上 `load-path` | **吸收并修正**：本配置每个文件的字节编译检查都停在第一个 `require`（“Cannot open load file”），什么也查不出；只追加 `load-path` 还不够，目录模块里 `./config.el` 就是模块自己，会遮住核心的 `config`（“Recursive require”），所以会话的 `load-path` 放在 `./` 前面。修好后查出 `editor/editing` 缺的 4 个 `declare-function`，一并补上 |
| `lang/emacs-lisp` | eldoc 在变量文档旁显示当前值 | **吸收**：改用 Emacs 31 自带的 `elisp-eldoc-var-docstring-with-value`，不移植 Doom 的 advice |
| `lang/emacs-lisp` | 改写 `calculate-lisp-indent`（plist 对齐）、`;;;` outline、额外着色、highlight-quoted、非 package 文件压低编译警告、checkdoc 放宽、buttercup、working buffer、ielm 着色 | **不收**：缩进改写会重排现有代码；其余是外观或 Doom 自己的测试/求值约定；本配置的 checkdoc 警告保持原样 |
| `lang/latex` | `fill-nobreak-predicate` 加 `texmathp` | **吸收**：`+auctex-keep-math-unbroken-h`，填充段落时不在行内公式中间换行 |
| `lang/latex` | `.auctex-auto` 隐藏目录、`TeX-save-query nil`、`chktex` 参数、smartparens 的 `\\left(` 配对、条目续行缩进、viewer 列表、tex-fold、cdlatex/company | **已有或不收**：本配置把 AUCTeX 的解析缓存放到项目外（`+auctex-setup-auto-cache`），比 Doom 更干净；保存询问与缩进是习惯；预览走 RaTeX 与 PDF Tools |
| `lang/nix` | `flake.lock` 用 JSON 模式、NixOS 的 `tramp-remote-path`、nix 补全与 REPL | **吸收并扩大**：本配置把所有 `.lock` 当 TOML，`flake.lock`、`composer.lock`、`Pipfile.lock` 其实是 JSON，现在改走 JSON 模式；远程 PATH 走 Remote 环境胶囊 |
| `lang/haskell`、`lang/ocaml`、`lang/lean`、`lang/markdown` | stack/dune 项目根、`.hi` 忽略、cabal 构建键、utop/merlin、Lean 3/nael、Markdown 编译与插入键 | **不收**：项目根由本配置的根解析决定；Haskell/OCaml 走 LSP；Lean 4 用 lean4-mode 与本配置的 infoview；Markdown 由 Noema 接管 |

## 11. 其余模块逐个对照

| Doom 模块 | 内容 | 处理 |
| --- | --- | --- |
| `os/macos` | `locate-command "mdfind"` | **吸收并修正**：本机没有 locate 数据库，`consult-locate-args` 的 `--ignore-case` 又是 GNU 参数，`M-x locate` 和 `consult-locate` 都查不到东西。改用 `mdfind -name`；`locate` 会把 stderr 当结果读，而 mdfind 总在 stderr 打日志，所以经 `/bin/sh` 丢掉 |
| `os/macos` | Keychain 作 auth-source、`ns-auto-titlebar`、`ns-pop-up-frames nil`、`delete-by-moving-to-trash` | **不收或已有**：Keychain 查询可能弹出系统授权框；标题栏与 frame 由 `config/background` 管；废纸篓已开启 |
| `term/eshell` | `eshell-hist-ignoredups` | **吸收**：连续重复的命令只记一次 |
| `term/eshell` | 提示符、别名表、只读保护已执行的输入输出、执行中关 undo、切分窗口开新 shell、visual 命令、各种键 | **不收**：eshell 的行为由 `docs/design/command-shell.md` 定义（自动化 API、输出规整），键位以本配置为准 |
| `emacs/undo` | undo-fu-session 不保存 `.gpg`、`COMMIT_EDITMSG`、`git-rebase-todo` 的历史 | **吸收**：本配置的 undo-tree 给每个文件都写历史，加密文件的明文会进 `var/undo-tree-history/`；现在加密文件（`.gpg`/`.age`/`epa-file-name-regexp`）和 Git 消息文件不保存也不读取历史。检查过现有历史目录，没有已泄漏的文件 |
| `emacs/undo` | undo-fu、vundo、undo 上限、zstd 压缩 | **不收**：本配置用 undo-tree，上限已更大 |
| `emacs/dired` | `-v` 自然排序、远程去掉 GNU ls 参数、virtual buffer 不自动刷新、ESC 退出 wdired | **已有或不收**：dwim、自动刷新、`gls`、目录创建询问已一致；排序是外观；远程 Dired 由 Remote 框架处理 |
| `emacs/vc` | `vc-handled-backends '(SVN Git Hg)`、`node_modules` 不进 VC、冲突标记自动开 smerge、log-view/annotate/vc-dir 的 Evil 状态、git-timemachine | **已有且更细**：本配置只保留 Git，并按 target 的代价对远程 buffer 关掉 VC（`+disable-remote-vc-h`）；smerge 由 `emacs/smerge` 模块管；没有 git-timemachine |
| `checkers/spell` | spell-fu、按模式跳过代码/链接/公式 face、aspell 参数、个人词典路径 | **不需要**：本配置只在 Git 提交信息里开 flyspell（hunspell），Noema 的文本由 Noema 自己检查 |
| `tools/lookup` | `+lookup/definition` 等多级 handler、dumb-jump、在线搜索、词典/同义词 | **不收**：查找走 xref 后端链（第 5 节）；用户不用翻译和词典 |
| `editor/format` | apheleia、按项目缩进的 clang-format/prettier 参数 | **不收**：格式化只走 LSP（第 5 节） |
| `tools/pdf` | 打开大 PDF 不问“文件很大，确定打开？” | **吸收**：`+pdf--skip-large-file-prompt-a`，其他大文件照常询问 |
| `tools/pdf` | HiDPI 缩放、关掉 imagemagick、Evil 光标、关闭时清理注释 buffer、`q` 杀 buffer、epdfinfo 缺失时退回、midnight 颜色热更新、saveplace-pdf-view | **已有或不收**：缩放已开；光标见前面；`q` 与关闭行为以窗口生命周期为准；其余没有使用者 |
| `tools/editorconfig` | 有 `indent_*` 时不让 dtrt-indent 覆盖、trim 走 ws-butler、排除 Office 文件 | **已有**：`config/default` 里同样处理 |
| `ui/popup`、`ui/tabs`、`ui/window-select` | Doom 自己的 popup 系统、centaur-tabs、ace-window 键与范围 | **不收**：第 6 节已记；ace-window 已有 |
| `lang/json`、`lang/data` | `.jshintrc`/`.jscsrc` 为 JSON、`.xslt` 为 XML | **吸收**：之前这两类文件打开没有模式；`.plist`、`pom.xml`、`.xsd`、`.rss` 已是 `nxml-mode` |
| `lang/json`、`lang/yaml`、`lang/data`、`lang/graphviz`、`lang/plantuml`、`lang/scala` | json-mode 的路径/布尔键、`Procfile` 当 YAML、nxml 自动补全与 xmllint、csv 键、graphviz 预览键、plantuml jar 下载、scala 缩进与 REPL | **不收**：tree-sitter 模式已覆盖；`Procfile` 不是 YAML；其余是键位与 Doom 自己的格式化/REPL 体系 |
| `tools/debugger`、`editor/snippets`、`checkers/grammar`、`ui/nav-flash`、`app/calendar` | dape 断点目录、ESC 中止 snippet、选区包裹 snippet、LanguageTool 路径、writegood、跳转闪烁点、calfw 键 | **已有或不收**：dape 状态目录启动时建立；snippet 与闪烁是手感；语法检查已用 LanguageTool；日历键以本配置为准 |
| `tools/tree-sitter` | 缺 grammar 时安静地退回、统一遵守 `treesit-auto-install-grammar` | **吸收**：本配置的约定是“打开文件时不提示、不安装 grammar”（treesit-auto 已设），但 Emacs 31 自己的 `treesit-auto-install-grammar` 还是 `ask`，打开 `.ts` 仍会问；登记为 config 设置，值 `never`。本机缺 TypeScript grammar，需要时用 `M-x treesit-install-language-grammar` |
| `tools/tree-sitter` | 30.x ts-mode 副作用、`set-tree-sitter!` 映射、font-lock 级别 4 | **不需要**：31.1 已修；映射由 `+treesit-major-mode-remap-candidates` 负责 |
| `emacs/tramp` | `remote-file-name-inhibit-*`、复制阈值、scp 直传、补全缓存、ssh 默认方法、magit/project/vc-git 结果记忆化 | **已有或不收**：前六项本配置已是相同值；记忆化没有失效机制，本配置的 Remote 框架有自己的根目录预取与缓存 |
| `lang/beancount` | electric 货币、`custom` 指令着色、nerd-icons、flymake-bean、补全增强 | **不收**：记账走本配置的 language server 与录入向导；着色与图标是外观 |
| `checkers/syntax` | flycheck 体系、ESC 时重跑检查、flymake 右侧 fringe、不环绕 | **不收**：本配置用 Flymake，空闲时自动检查；位置和环绕是偏好 |
| `emacs/ibuffer`、`ui/treemacs`、`ui/ligatures`、`ui/doom`、`ui/dashboard` | 列宽与图标、Treemacs 主题加载顺序、连字表、Doom 主题与 solaire、Doom 首页 | **不收**：外观，第 1、6 节已列 |
| `config/default`（`config.el`） | avy 范围、GPG loopback、`RET` 续写注释、智能退格、tabulated-list 的 `q`、macOS `S-iso-lefttab`、woman manpath | **已有或不收**：GPG 已走 minibuffer；续写注释与退格改变手感；其余已有或不适用。`+evil-bindings.el`/`+emacs-bindings.el` 的键位见第 4 节，仍待用户决定 |

未逐个展开的 Doom 模块都是本配置不用的领域：`completion/corfu ivy helm ido`、`editor/god lispy objed parinfer rotate-text`、`email/*`、`input/*`、`app/emms everywhere irc rss`、`os/tty`、`term/shell term vterm`、`tools/ansible biblio collab docker ein llm pass terraform tmux upload make`、`ui/deft doom-quit emoji indent-guides minimap neotree ophints smooth-scroll unicode vi-tilde-fringe zen`，以及本配置不写的语言（`lang/` 下除上面各节以外的模块；`lang/java`、`lang/org` 用户不用）。

## 12. 补读的内核库与框架文件（全文）

| 文件 | 全文读后的处理 |
| --- | --- |
| `lib/sandbox.el` | **吸收**：`M-x elisp-sandbox`，在 `emacs -Q`（带当前 `load-path`）里运行一段 Elisp。只做“原生 Emacs + 已装包”一种：另起一个加载完整配置的实例会和正在运行的后台 Emacs 抢 server 与状态 |
| `lib/store.el` | 已标记弃用的带 TTL 的键值缓存；本配置的持久化走 config 注册表与 `var/`，不收 |
| `lib/system.el` | 发行版识别、图标、CPU 数、symlink 支持检测；本配置只跑 macOS，CPU 数用 `num-processors`，不收 |
| `lib/process.el` | `doom-call-process`/`doom-exec-process`；本配置的进程走 Remote 进程 API，不收 |
| `lib/plist.el` | 已弃用，Doom 自己推荐 `map.el`，不收 |
| `lib/strings.el` | `doom-pcre-quote`；本配置把输入交给 Consult 转换，不需要 |
| `lib/git.el` | `git rev-parse --show-toplevel`；本配置的根由项目层解析，不收 |
| `lib/packages.el` | straight.el 的 pin 升级命令；本配置的版本锁在 `package-lock.el`，用 `make lock`，不适用 |
| `lib/config.el` | `doom/reload`、打开私有配置；本配置重载走模块系统，不收 |
| `lib/projects.el` | `set-project-type!`、在其他项目找文件、把目录登记为项目；后两项已有（`+project/switch`、`project-add-to-workbench`），前者没有使用者 |
| `lib/help.el`（全文） | `doom/help-package-config` | **吸收**：`elisp-find-package-config`，在本配置的 `lisp/` 里找设置某个包的位置 |
| `lib/help.el`（全文） | `describe-option`（只列 defcustom）、`describe-package`（带 straight 信息与模块来源）、`describe-autodef`、`search-loaded-files`、主页与报 bug、旧别名 | **不收**：前两项是 Emacs 自带命令的变体，模块来源已在 `aaron/describe-module`；其余属于 Doom 自己 |

## 13. `doom-emacs.el` 余下部分（全文）

| 段落 | 处理 |
| --- | --- |
| 第 1–100 行：29.1–31 的向前兼容（`enable-theme-functions`、treesit 空函数、`major-mode-remap`、`safe-local-variable-directories`、`mode-line-invisible-mode`） | **不需要**：本配置只跑 Emacs 31 |
| 字体、主题变量与 emoji/符号回退字体表 | **已有**：`aaron/apply-font-config` |
| `doom-file-lines-threshold-alist`（按行数开 so-long） | **已有**：`aaron-base` 的 so-long 判定 |
| 生命周期 hook | **已移植**（`aaron-emacs`） |
| `custom-theme-directory`、`custom-file` | **已有**：`custom-file` 在 `etc/custom.el` |
| `en/disable-command` 写到 `custom-file` | **吸收**：Emacs 把“以后都启用这个命令”写进 `user-init-file`，即受版本控制的 `init.el`；改写到 `etc/custom.el`（`aaron--disabled-commands-to-custom-file-a`）。检查过 `init.el` 历史，还没被写过 |

## 14. 框架文件（全文）

| 文件 | 全文读后的处理 |
| --- | --- |
| `early-init.el`、`doom.el` | 启动优化逐条对照：GC、`file-name-handler-alist`、隐式 frame 调整、启动画面本配置已有；`auto-mode-case-fold nil` **不收**（`Photo.JPG`、`Paper.PDF` 这类大写扩展名会找不到模式）；启动期间隐藏 mode line 与 `inhibit-message` 对隐藏启动的后台 Emacs 没有意义；native-comp 临时目录重定向针对 Linux tmpfs；`doom-finalize` 的 GC 恢复、server、计时本配置已有 |
| `doom-emacs.el` 字体与主题加载 | 符号/emoji 回退字体和 Nerd Font 私用区已在 `aaron-base`；按系统深浅色选主题是外观 |
| `doom-lib.el` | 宏逐个对照，均已移植（`letf!` 的关键字 advice 分支本配置绑定的是 advice 本身，更正确）；向前兼容垫片只为旧 Emacs；context 栈、profile 路径、Doom 错误类型属于它的 CLI 与 profile |
| `doom-modules.el` | 模块表、flag 合并与否定、`modulep!`、按深度排序已移植；模块改名的兼容重映射不收（本配置改名不留兼容，见 `AGENT.md`） |
| `doom-packages.el`、`doom-straight.el`、`doom-elpaca.el` | straight 的安装、pin、native-comp 等待、clone 失败重试；本配置用 `package-vc`，克隆失败的包不会进 `package-alist`，下次 bootstrap 会重装，`make audit-lock` 会报出 |
| `doom-profiles.el` | profile 的 init 文件与 loaddefs 生成；本配置的模块系统自己生成 autoload |
| `doom-docs.el` | Doom 文档的 Org 阅读模式与 `kbd:`/`M-x:`/`module:` 链接；本配置文档是 Markdown，由文档中心呈现 |
| `doom-cli.el` | `doom` 命令行框架（参数解析、帮助、日志、会话重启）；本配置的对应物是 `bin/emacs-app` 与 `make` |

## 15. 模块全文：`os`、`app`

| 模块 | 处理 |
| --- | --- |
| `os/macos`（`autoload.el`） | **吸收** `reveal-in-finder`、`reveal-project-in-finder`：`macos-tools-menu` 的 `r`/`R`，只接受本机能读的路径；`open-with`、iTerm/Transmit/LaunchBar 不收（本配置用 `macos-open-at-point` 与 Portal） |
| `os/macos`（`config.el`、`doctor.el`、`packages.el`） | `mdfind` 已吸收；Keychain auth-source、`ns-auto-titlebar` 不收（见第 11 节）；doctor 只针对 Emacs ≤ 29.2 |
| `os/tty` | 终端 Emacs 的剪贴板、光标形状、kkp 键协议；本配置只用图形界面 |
| `app/calendar` | calfw + org-gcal；本配置的日历是内置 `calendar` 的全年视图，用户不用 Org |
| `app/emms`、`app/everywhere`、`app/irc`、`app/rss` | 用户不用音乐播放、emacs-everywhere、IRC、RSS（neo 审计也已记 RSS 不收） |

## 16. 模块全文：`checkers`、`emacs`

| 模块 | 处理 |
| --- | --- |
| `checkers/grammar`、`spell` 全部 autoload | LanguageTool 已有；flyspell 加词、纠错、spell-fu 不适用（flyspell 只在 Git 提交信息中） |
| `checkers/syntax` | Flymake 只在项目里字节编译 Elisp（CVE-2024-53920）**已有**：本配置用 `trusted-content`；`next-error` 指向 Flymake、ESC 重查、flycheck 体系不收 |
| `emacs/dired` | `dired-vc-rename-file` **已有**；远程去掉 GNU ls 参数不需要（TRAMP 逐主机检测 ls 支持的参数，远程主机是 GNU ls）；`dired-omit-files`、`dired-guess-shell-alist-user`、Dirvish 键与属性不收（隐藏文件由 Dirvish 按 yazi 键表控制） |
| `emacs/electric` | `set-electric!`、续写注释、电子缩进词不收（手感） |
| `emacs/eww` | 页内链接/标题跳转、复制 URL、字号；本配置 EWW 只作阅读，已有刷新、阅读模式与转 xwidget；按标题改名不收（第 10 节） |
| `emacs/ibuffer` | 人类可读大小列、工作区过滤、图标；外观 |
| `emacs/undo` | `+tree` 分支逐行看过：剥离撤销记录的文本属性，实测本机最大的历史文件 316 KB 里属性只占约 1.6 KB，不收；不保存加密文件与 Git 消息的历史已吸收；可视化 diff 窗口不收 |
| `emacs/vc` | git-link（本配置有 browse-at-remote）、git-timemachine、smerge 键；已有或不用 |

## 17. 模块全文：`completion`

| 模块 | 处理 |
| --- | --- |
| `completion/vertico`（autoload） | **吸收**两项：`+vertico-orderless-disambiguation-dispatch` 的 `$` 行尾锚点（实测本配置 `consult-line` 里 `foo$` 一个都匹配不到，因为 Consult 在候选末尾加了不可见的 tofu 字符），改为 `+minibuffer-orderless-end-anchor-dispatch`；`+vertico/embark-export-write`，minibuffer 里 `C-c C-e` 直接导出成可编辑 buffer |
| `completion/vertico`（其余） | 项目搜索包装、Ex 搜索命令、跳转列表、which-key 风格的 embark 提示、fd 版本判断、远程用 basic 补全、工作区 buffer 源；已有对应（telescope、`consult-ripgrep`、better-jumper）或属于 Doom 的工作区实现 |
| `completion/company` | 后端注册表、整行补全（Evil 的 `C-x C-l` 已有）、切换自动补全、在 minibuffer 里选候选；不收（第 5 节） |
| `completion/corfu`、`helm`、`ido`、`ivy` | 另一套补全前端，本配置只用 Vertico + Company |
| `completion/corfu`（全文） | dabbrev 跳过 `pdf-view`/`doc-view`/`tags-table` buffer | **吸收**：实测 Company 的 dabbrev 会扫描 PDF buffer 里的原始字节，在正文里补出 `FlateDecode` 一类词；`dabbrev-expand` 与 Company 的 dabbrev 现在都跳过这些模式 |
| `completion/corfu`（其余） | Corfu 本身、cape、RET 行为、minibuffer 补全、ispell capf 出错自动关闭、yasnippet-capf | 另一套补全前端，不收 |
| `completion/helm`、`ido`、`ivy`（全文） | 各自的项目搜索、Ex 命令、posframe、ivy-rich 等 | 不用这些前端 |
| `completion/ivy`、`vertico`（全文） | `wgrep-auto-save-buffer t` | **吸收**：本配置应用 wgrep 修改后不保存文件，`C-c C-e` 导出编辑后还要逐个保存；现在 `C-c C-c` 一并写盘（端到端测试） |
| `completion/vertico`（`config.el` 全文） | 用 minibuffer 做 buffer 内补全、multiform 高亮目录与已启用模式、Consult 预览键、consult-dir 容器源、ripgrep PCRE 检查 | 不收或已满足：buffer 内补全是 Company；高亮与预览键是外观和手感；本机 ripgrep 支持 PCRE 前瞻 |

## 18. 模块全文：`config`

| 文件 | 处理 |
| --- | --- |
| `config/default/config.el` | **吸收**：Insert 状态下开 `delete-selection-mode`（输入覆盖鼠标或 Shift 选中的文字，离开 Insert 关掉）。不收：`man` 换 `woman`（实测 macOS 上 `man` 补全首次 1.3 秒、之后有缓存）；GPG 默认收件人（本机没有 `.gpg` 文件）；智能退格与 RET 续写注释（手感）；macOS `s-` 键与 help 键表（第 4 节）；avy、woman 路径 |
| `config/default/autoload/*.el` | 复制项目相对路径已吸收；`compile`/搜索/浏览/新 buffer/重启 server/诊断等包装与本配置已有命令重复 |
| `config/default/+emacs-bindings.el`、`+evil-bindings.el` | 键位全文看过，归入第 4 节的候选：Doom 的 minibuffer `C-r` 取寄存器、`C-u` 删到缩进是 Vim 命令行的行为，可与 `SPC .` `SPC ,` 等空位一并由用户决定；其余命令本配置已有或属于不用的模块 |
| `config/literate` | 本配置不是 Org literate 配置 |

## 19. 模块全文：`editor`

| 模块 / 文件 | 处理 |
| --- | --- |
| `editor/evil`（`config.el` 的 `map!` 与 autoload `unimpaired.el`、`evil.el`、`textobjects.el`） | **吸收**与 Neovim 0.10/0.11 默认键重合的部分：`]b`/`[b`、`] SPC`/`[ SPC`、`]q`/`[q`（`next-error`）、`]d`/`[d`（Flymake）、`gc`/`gcc` 注释、Visual `*`/`#` 按选中原文搜索、行选择 Visual `@q` 逐行执行，以及整个 buffer 文本对象 `ig`/`ag`。不收 Doom 与 Neovim 不同的键：`]f` 下一个文件（本配置是下一个函数）、`gp` 重选粘贴（Vim 是粘贴后移到其后）、`]c` 注释、`zx` 关 buffer、Visual `<`/`>` 保持选择 |
| `editor/evil/autoload/advice.el` | **吸收**：`+evil-replace-filename-modifiers-a` 改写为 `+evil-expand-file-name-modifiers-a`，只在 `%`/`#` 后带修饰符时接手，裸 `%`/`#` 仍由 Evil 处理；`+evil-delete` 改写为 wgrep 里的 `+evil:wgrep-delete`。`+evil-join-a` 已有（第 12 节）。`o`/`O` 续写注释不收：Neovim 默认 `formatoptions` 没有 `o`。分屏更新窗口历史、minibuffer 里 hippie 的语法表不适用 |
| `editor/evil/autoload/ex.el`、`+commands.el` | `:retab` Evil 已有；`:align`、`:rev`、`:mv`/`:cp`/`:rm`、工作区与会话 Ex 命令不收：本配置这些操作走 leader 键与菜单 |
| `editor/evil/config.el`（`use-package` 部分） | 已有：`set-auto-mode` 保持状态、`evil-symbol-word-search`、`evil-ex-interactive-search-highlight`、escape 清高亮。不收：`evil-kbd-macro-suppress-motion-error`（Vim 里动作失败会结束宏，递归宏靠它停下）、`evil-ex-visual-char-range`（Vim 的 `:'<,'>` 总是整行）、`evil-indent` 不移动光标（Vim 的 `=` 移到首行）、2–9 号标记全局化、Vim 式保存消息、光标颜色；evil-easymotion/snipe/embrace/exchange/lion/numbers/traces 等包（本配置用 avy 与 evil-surround） |
| **G-E1（待用户决定）** | 审计时发现 evil-collection **从未启用**：`use-package evil` 在 `:config` 里先开了 `evil-mode`，之后 evil-collection 的 `:hook (evil-mode . evil-collection-init)` 才加上，所以 `evil-collection-init` 不会运行（交互启动实测 `(featurep 'evil-collection)` 为 nil；只有 `SPC q E` 重开 `evil-mode` 才会触发）。`evil-collection-mode-list` 的过滤、unimpaired 的 repeat 选项因此都没有作用。Doom 默认（不加 `+everywhere`）也不装 evil-collection。启用它会改变 Magit、help、Info 等所有特殊模式的键，属于手感变化，留给用户决定：启用（修正顺序）或删除这段声明 |
| `editor/file-templates` | 本配置用内置 `auto-insert` 模板层（AGENT.md 规定不回到 yasnippet 文件模板）；Doom 的规则表、license 片段与 Doom 自身 README 模板不收 |
| `editor/fold`（autoload `fold.el`） | **吸收** `+fold/next`/`+fold/previous`：`zj`/`zk` 跳到下一个/上一个已关闭的折叠（Evil 未绑定这两个键）。改写为不分后端：hideshow、treesit-fold、Org 都把关闭的折叠显示为跨行的不可见文本，以此识别，单行隐藏的链接或强调标记不算。其余开合命令本配置已有按后端分派的实现与状态持久化 |
| `editor/fold`（`hideshow.el`、`config.el`） | `hs-special-modes-alist` 里 vimrc、haml、ruby、matlab、nxml、LaTeX、YAML 缩进块的条目：本配置这些语言走 treesit-fold 或不用；vimish-fold 手动折叠、折叠省略号外观不收 |
| `editor/format` | 不收（记为候选 **G-F1**）：本配置只用 LSP 格式化（`SPC c f`），没有 apheleia 那样的外部格式化器（shfmt、prettier、clang-format 等无 LSP 时也能用）；加它需要按 Remote 规则把格式化进程放到 target 上，是独立的工作。选区格式化在 Neovim 里是 LSP 设置 `formatexpr` 后的 `gq`，会改变 `gq` 折注释的手感，一并留给用户决定。`+format/org-block` 不适用（用户不用 Org 写代码块） |
| `editor/multiple-cursors` | **吸收** Doom 的 mc.el 与 Evil 兼容做法：实测 `viw` 选中 `foo` 后按 `gn`，多光标拿到的是 `fo`（字符 Visual 下 Emacs 区域不含最后一个字符），且 mc 在各光标上执行 Emacs 命令而非 Evil 命令。现在从 Evil 状态建立多光标时把 Evil 选区原样交给 mc 并切到 Emacs 状态，`Esc` 结束多光标并回到 Normal。evil-mc、evil-multiedit 与 Ex 命令 `:mc` 不收（另一套多光标实现） |
| `editor/god`、`lispy`、`objed`、`parinfer`、`rotate-text` | 另一套编辑模型或用户不用的包 |
| `editor/snippets` | yasnippet 字段内 `C-a`/`C-e` 跳字段首尾、退格不越过字段、空字段按 Delete 跳过、Escape 放弃 snippet、私有 snippet 优先、在 Visual 选区上展开：本配置的 snippet 层（`C-c y`）键位与 Copilot 共用 `yas-keymap`，属于手感，不收 |
| `editor/whitespace` | dtrt-indent 猜缩进**已有**（`config/default`）；ws-butler **已有**；高亮与 `indent-tabs-mode` 相反的缩进（外观，不收） |
| `editor/word-wrap` | `+word-wrap-mode`（adaptive-wrap 加按语言缩进的折行）：本配置长行按模式各自处理，不收 |

## 20. 模块全文：`email`、`input`、`term`

| 模块 | 处理 |
| --- | --- |
| `email/mu4e`、`notmuch`、`wanderlust` | 用户邮件走 Gnus（`SPC a g`），不用这三个客户端；mu4e 的空主题检查 Gnus 的 `message-check` 已有 |
| `input/bidi`、`japanese`、`layout`（bépo、azerty） | 用户不写从右到左文字和日文，键盘是 QWERTY |
| `input/chinese` | pyim、liberime 输入法和 pangu 自动空格不收（用户用系统输入法）；`fcitx-evil-turn-on`（离开 Insert 时关掉中文输入法）在 macOS 上对应切换输入源，见下一节 |
| `term/eshell` | **吸收**两项：以空格开头的命令不进历史（`eshell-input-filter` 组合默认过滤与 `eshell-input-filter-initial-space`）；`emacs FILE`/`e FILE` 在本 Emacs 打开文件（本配置已有 `vi`/`vim`/`nvim` 同类别名）。重复命令只记一次已在第 16 节吸收。不收：提示符、输出只读与命令期间停用撤销（本配置 eshell 在 Emacs 状态，且有自己的输出规范化与命令队列）、Evil 键、别名表、分屏与工作区跟踪、esh-help、did-you-mean、语法高亮 |
| `term/ghostel` | 本配置已用 Ghostel（远程工作区）与 Portal；去掉 `DIRENV_*` 变量的问题未在本配置复现（终端从 PTY 里执行目标 `$SHELL`），不收 |
| `term/shell`、`term`、`vterm` | `+shell-idle-p` 用 `file-remote-p` 分支，与 Remote 规则相悖；本配置终端走 Portal/Ghostel，不收 |
| `input/chinese`（`fcitx-evil-turn-on`） | **吸收为 macOS 版**：用户启用了 ABC 与系统拼音，Normal 状态下拼音会吞掉 `j`、`k` 等键。新增动态模块 `tools/macos-input/input.m`（Carbon TIS，`bin/emacs-app build` 编到 `var/macos-input/`，只在用户态运行），`os/macos/input` 在离开 Insert 时切到 `+macos-input-ascii-source`、进入 Insert 时切回原输入法；batch 下不切换。实测（交互 Emacs）：Insert 拼音 → Normal ABC → 再进 Insert 拼音 |

## 21. 模块全文：`tools`

| 模块 | 处理 |
| --- | --- |
| `tools/lsp`（`+lsp.el`） | **吸收** G-L1：最后一个 buffer 关闭后推迟 `+language-server-defer-shutdown` 秒再停 server。审计时发现这是回归：迁到 lsp-mode 前本配置在 Eglot 上已有同样的推迟（`my/language-server-defer-shutdown`，3 秒），迁移（`6d49b4b0`）丢了实现，设置项和值 3 一直留在 store 里却无人读取。按 `lsp-workflow.md` 的 D1–D4 实现：显式停止、资源关闭、断开的 target 立即生效并取消计时器，推迟期间 server 已死则不动。`make lsp-live-smoke` 的 C/Python 通过，Java 的 jdtls 在改动前后都以退出码 13 启动失败（与本改动无关） |
| `tools/lsp`（其余） | `+lsp-signature-stop-maybe-h`（Esc 关签名）已有；`lsp-enable-folding` 等默认已按本配置的 Hub 管理；company 后端、eglot 部分不适用（Eglot 刻意隐藏） |
| `tools/direnv` | 提前到 `change-major-mode-after-body-hook` 更新环境：本配置的 direnv 走 Remote 环境 capsule，在启动 LSP 前等环境就绪（`+lsp-mode--waiting-for-direnv`），已覆盖。direnv 成功后重启 LSP 不收：本配置 direnv 自动刷新，每次成功都重启会让远程 server 反复重启；环境真的变化时用 `SPC c R` |
| `tools/editorconfig` | 压缩包与 Office 文件排除**已有**；`editorconfig` 设了缩进时不再猜缩进：本配置 dtrt-indent 已在 editorconfig 之后判断 |
| `tools/eval` | `eval-expression-print-length/level` 不截断**已有**；eros 结果浮层**已有**（`editing`）；REPL 注册表、quickrun 弹窗收缩与 Evil 选区替换不收（本配置运行走 `compile`/`+output` 与 quickrun 原样） |
| `tools/lookup` | 本配置的跳转是 `editor/navigation`（LSP → xref → 搜索的回退链已有）；在线搜索已有（`+browser/open-search` 与 `tools/menus` 的搜索引擎表）；Dash docset、字典、bug-reference 打开 issue 不收 |
| `tools/ansible`、`biblio`、`collab`、`docker`、`ein`、`llm` | 用户不用 Ansible、Org 引用、CRDT 协作、Docker、EIN（Jupyter 走 Noema）；LLM 走 agent-shell/ACP |
| `tools/debugger` | dape **已有**；断点跨会话保存（`dape-breakpoint-global-mode`，文件在已存在的 `var/dape/`）**已有**，Doom 的建目录补丁不需要 |
