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
