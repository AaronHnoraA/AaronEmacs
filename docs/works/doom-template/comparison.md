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
| `tools/lsp` | 最后一个 buffer 关掉后推迟 3 秒再关 server | **记为缺口** G-L1（`docs/design/lsp-workflow.md`）：LSP 生命周期属于 Remote/LSP 契约，显式断开和手动关闭必须立即生效，需要先定契约并补故障注入测试 |
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
