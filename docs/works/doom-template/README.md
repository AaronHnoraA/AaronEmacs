# 以 Doom 为模板升级配置（第一阶段）

方向（用户定）：**不依赖 Doom，但把 Doom 里好、而本配置没有或没做到的东西直接拿过来，包括它的抽象结构。** 先做一轮配置一体化和抽象化升级，再在这个基础上吸收 neo-emacs（第二阶段，见 [../neo-emacs/](../neo-emacs/README.md)）。

- 上游代码在 `upstream/`，由 `.gitignore` 忽略：
  - `upstream/core`：[doomemacs/doomemacs](https://github.com/doomemacs/doomemacs)，`59cdaa3`（2026-10-02），是 Doom 的内核。
  - `upstream/modules`：[doomemacs/modules](https://github.com/doomemacs/modules)，`42a0479`（2026-10-02）。v3 起模块从内核仓库拆了出来，共 154 个模块，约 4.2 万行。
  - 更新用 `git -C docs/works/doom-template/upstream/<core|modules> pull`。
- 许可：两个仓库都是 MIT。拿过来的代码在文件或函数头注明出处，格式为 `Adapted from Doom Emacs (MIT, Henrik Lissner): <上游路径> @ <commit>`。
- 骨架不变：`package.el` + `use-package`、`init-modules.el` 作为唯一顺序、Remote-first、config 注册表、`general.el`、`my/` 命名约定。Doom 的 straight 包管理、profiles 和 `doom` CLI 框架不引入。

## Doom 的抽象结构，逐层对照

每层先说 Doom 怎么做，再说本配置现状，最后是处理方式。处理方式分四种：**拿代码**（直接改写进来）、**拿结构**（学设计，实现按本配置的约束写）、**比对**（双方都有，逐条合并）、**不拿**（写明理由）。

### A. 模块系统

Doom 的一个模块是一个目录 `modules/<分类>/<模块>/`，里面的文件分工固定：

| 文件 | 作用 |
| --- | --- |
| `init.el` | 很早执行，用于必须在任何包加载前设置的东西 |
| `config.el` | 模块行为 |
| `autoload/*.el`、`autoload.el` | `;;;###autoload` 标注的命令和函数。生成 autoloads 后，启动时不加载，第一次调用才加载 |
| `packages.el` / `.doommodule` | v3 的模块清单，是纯数据：`flags`（可用 flag）、`obsolete`（改名的 flag 和过期版本）、`packages`（含 `:if (modulep! +flag)` 条件）、`checks`（依赖检查） |
| `doctor.el` | 自检：flag 之间的依赖、外部可执行文件是否存在 |
| `README.org` | 固定章节：Description / Module flags / Packages / Hacks / Prerequisites / Usage / Configuration / Troubleshooting / FAQ |

其余机制：

- 用户在 `doom!` 里按分类声明启用哪些模块、带哪些 flag。
- 模块代码里用 `(modulep! +flag)` 或 `(modulep! :lang python)` 判断 flag 和其它模块是否启用。
- 加载顺序由声明顺序加上 `:depth` 决定。

本配置现状：

- `lisp/init-modules.el` 是一张 83 行的平铺 require 列表。可选模块和延迟模块有专门的函数，加载失败会进 Health board。
- 包声明写在使用它的模块里，锁定靠 `package-lock.el`。
- 自检分散在各领域：LSP Doctor、Remote Doctor、LaTeX doctor、`config-health`。
- 没有 flag 机制，没有统一的模块清单。命令按模块整体加载，部分用 `autoload` 手工注册。

处理：**拿结构**，它是第一阶段的主干。

1. **模块声明**：`init-modules.el` 改成分类声明，参照 `doom!` 的写法。宏展开后仍是现在的 require 序列，开关和 flag 的值放进 config 注册表。
2. **模块清单**：每个模块用一份纯数据描述自己：分类、flag、过期 flag、用到的外部可执行文件、自检。参照 `.doommodule` 的字段，但不含包声明，包仍按 use-package 的做法写在模块里。
3. **自检统一**：清单里的 `checks` 汇总进 `config-health`。现有的各个 doctor 保留，作为深入诊断的入口，Health 只负责汇总和链接。
4. **`autoload/` 拆分**：只在交互时才用到的大块命令（board、menu、doctor）移到模块的 autoload 文件，用 `loaddefs-generate` 生成 autoloads。要结合 `make` 的编译流程和启动时间测量来做。
5. **`modulep!`**：等价物叫 `my/module-p`，模块之间用它判断对方是否启用，代替现在的 `featurep` 或 `fboundp` 试探。
6. **模块文档模板**：采用 Doom README 的章节结构，但内容放在 `docs/` 里（每个分类目录一份索引），不在代码目录里放第二份。

### B. 生命周期 hook

Doom 在 `core/lisp/doom-emacs.el` 和 `doom.el` 里提供了这些 hook：

| Doom hook | 含义 | 本配置 |
| --- | --- | --- |
| `doom-first-input-hook` | 第一次按键 | 没有 |
| `doom-first-file-hook` | 第一次打开文件 | 没有 |
| `doom-first-buffer-hook` | 第一次切到真实 buffer | 没有 |
| `doom-after-init-hook` / `doom-init-ui-hook` | 初始化完成 / UI 就绪 | 用的是 `after-init` / `window-setup`，零散 |
| `doom-load-theme-hook` | 换主题 | `enable-theme-functions`、`after-load-theme-hook`（init-ui） |
| `doom-switch-buffer/window/frame-hook` | 切换 buffer / 窗口 / frame（frame 的那个有防抖） | 没有统一的 hook |
| `MODE-local-vars-hook` | 在 dir-locals 和 file-locals 应用之后执行的 mode hook | 没有 |
| `doom-escape-hook` | 统一的 ESC/C-g 收尾 | 已有 `my/escape-hook` |
| 增量加载 `doom-incremental-packages` | 空闲时分批预加载重包（org、magit 等） | 没有（LSP 有单独的预热） |

处理：**拿代码**。这些 hook 定义短、语义清楚，可以直接改写进核心层。之后把各模块里零散的 `after-init`、`window-setup`、`post-command` 首次触发逻辑迁到这些 hook 上。

注意：Remote 规则要求后台任务不能碰远端。增量加载和 switch hook 的消费者要按 `remote-file-operation-cost` 判断，不能在远端 buffer 里触发文件操作。

### C. 宏与基础库（`core/lisp/doom-lib.el`）

`after!`、`add-hook!`、`remove-hook!`、`setq-hook!`、`unsetq-hook!`、`add-transient-hook!`、`defadvice!`/`undefadvice!`、`cmd!`/`cmd!!`/`cmds!`、`fn!`、`letf!`、`quiet!`、`defer-until!`、`defer-feature!`、`with-delayed-gc!`、`doom-log`、`doom-context`，以及 `doom-run-hooks`（逐个执行 hook 函数，出错时报出是哪个函数）。

本配置现状：只有 `set-company-backends-for!`、`shut-up!`（在 `init-funcs.el`），其余都是手写的 `with-eval-after-load`、`add-hook` 加 lambda、`advice-add`。

处理：**拿代码**，集中放进一个核心库文件。

- 本配置已有 `shut-up!` 这种不带前缀、以 `!` 结尾的宏名先例，所以这批宏沿用 Doom 的名字。函数名仍按 `my/` 约定。
- 好处：hook 和 advice 有名字，可以移除，也能在 `my/performance-hook-snapshot` 里看出是谁。`doom-run-hooks` 式的报错能指到具体函数。
- 现有代码不强制一次性改写，新代码和后续迁移时使用。

### D. 领域注册 API（`set-…!`）

这是 Doom 一体化的关键：

- 语言模块不直接去改 popup、补全、查找、格式化、REPL 这些子系统，而是调用 `set-…!` 声明意图，由拥有该子系统的模块统一实现。
- 一个语言模块因此基本就是一串声明。
- Doom 有这些：`set-popup-rule(s)!`、`set-lookup-handlers!`、`set-formatter!`、`set-repl-handler!`、`set-eval-handler!`、`set-company-backend!`、`set-file-template(s)!`、`set-evil-initial-state!`、`set-electric!`、`set-ligatures!`、`set-docsets!`、`set-tree-sitter!`、`set-next-checker!`、`set-eglot-client!`、`set-lsp-priority!`、`set-yas-minor-mode!`、`set-eshell-alias!`、`set-modeline!`。

本配置已经有同类做法，但只覆盖少数领域：

- `my/register-language-server`、`my/register-language-server-feature`（LSP）
- `config-register`（设置）
- `set-company-backends-for!`
- `my/display-rules-shackle-rules`（常量表，只能在一个文件里改）
- auto-insert 模板

处理：**拿结构**，按领域补齐注册 API，实现落在拥有该子系统的模块里，再把 `lisp/lang/` 下的模块改成声明式。下表的“现状”需要逐项核实后再定稿：

| API | 拥有它的模块 | 现状 |
| --- | --- | --- |
| popup 规则 | `init-display-rules`（shackle） | 只有固定常量表，没有注册函数 |
| 查找（定义 / 引用 / 文档 / 实现）handler 链 | `init-navigation` 或 LSP 层 | 没有统一的 handler 链 |
| 格式化 | `init-format` | 没有注册表 |
| REPL / eval | `init-dev` | 没有 |
| 补全后端 | Company | 已有 `set-company-backends-for!` |
| 文件模板 | `init-auto-insert` | 有，形式不同 |
| evil 初始状态 | `init-evil` | 待核实 |
| tree-sitter | `init-treesit` | 待核实 |

### E. 全局默认行为（`core/lisp/doom-emacs.el` 约 2000 行）

内容包括：安全默认值、运行时优化、编码、文件处理与 backup/autosave、格式化、剪贴板、去掉菜单/工具/滚动条、滚动、光标、fringe、窗口与 frame、minibuffer、行号、ESC 统一处理、按 mode 统一缩进变量（`doom-set-indent`）、`kill-current-buffer` 的 fallback buffer、大文件阈值（`doom-file-lines-threshold-alist`）、字体和主题加载，以及 recentf、savehist、saveplace、which-key、winner、show-paren、hl-line 等内置包的设置。

本配置现状：`init-base.el`（2185 行）和 `init-doom-extra.el`（680 行）已经吸收了一部分。

处理：**比对**。逐节对照，缺的拿代码，冲突的逐条裁决，结果记进清单。这一层工作量最大，但每一条都很小。

### F. 键位层（`modules/config/default`、`map!`）

Doom 的做法：

- `+evil-bindings.el` 定义了完整的 leader 布局：`SPC f/b/p/s/g/o/t/c/h/w…`，每组都有固定含义。
- `map!` 一个宏同时处理 state、keymap、leader、localleader 和 `:after`。

本配置现状：用 `general.el`，leader 前缀定义在 `init-evil.el`，各领域的后缀在所属模块里。

处理：

- 布局**比对**：逐组对照 Doom 的 leader 分组，找出缺的常用命令和含义不一致的组，结果写进 `daily-usage.md`。
- `map!` **不拿**：`general.el` 是规定的键位层，再加一套宏等于两套语法。

### G. 包管理和 CLI（`core/lisp/doom-packages.el`、`doom-cli.el`、`doom-profiles.el`）

| Doom 有的 | 处理 |
| --- | --- |
| `doom sync/upgrade/gc` | **不拿**，已有 `package-lock.el`、`bootstrap.el` 和 `make` 目标 |
| `doom doctor` | **拿结构**，并入 A.3 的 Health 汇总 |
| `doom env` | **比对**：现有登录 shell 环境的加载方式，和 Doom 生成 env 文件的做法 |
| straight、profiles | **不拿**，AGENT.md 明确不用第二套包管理 |

### H. 模块目录（154 个）

第一阶段后半段，按领域把 Doom 的模块和本配置对应的模块逐个对比，补上缺的功能。优先看本配置重度依赖、而 Doom 做得成熟的模块：

- `ui/popup`、`ui/workspaces`、`ui/modeline`、`ui/vc-gutter`
- `editor/evil`、`editor/format`、`editor/snippets`、`editor/file-templates`
- `completion/company`、`completion/vertico`
- `tools/lookup`、`tools/lsp`、`tools/eval`、`tools/magit`
- `emacs/dired`、`emacs/undo`、`emacs/vc`、`emacs/tramp`
- `term/ghostel`
- `lang/org`、`lang/python`、`lang/java`、`lang/markdown`
- `os/macos`

## 推进顺序

1. **P1a 核心库**：C 的宏，加上 B 的生命周期 hook 和增量加载，放进核心层，并补上测试。现有行为不变。
2. **P1b 模块系统**：A 的模块声明、清单、flag、Health 汇总，以及 `lisp/` 按分类分目录（feature 名不变）。行为不变。
3. **P1c 注册 API**：D，从 popup、查找、格式化、eval 开始，逐个把语言模块改成声明式。
4. **P1d 默认行为比对**：E 和 F。
5. **P1e 模块目录比对**：H，按领域逐个进行。
6. **收尾**：结构结论写进 `docs/architecture.md`、`AGENT.md` 和对应的 design 文档，本工作台移到 `records/`。之后开始第二阶段（neo-emacs）。

每一步一个或几个提交，每个提交都要能完整启动，并通过 `make` 测试和 `config-health`。

## 待定（需要你拍板）

- **分类名**：用 Doom 的分类（`:core :ui :editor :emacs :completion :checkers :tools :lang :term :app :os :input :config`），还是用本配置现有的 0–7 层？既然以 Doom 为模板，我倾向用 Doom 的分类名做目录。Remote 和 Noema 桥是 Doom 没有的，作为额外分类保留。
- **宏名**：C 的宏是否沿用 Doom 原名（`after!`、`add-hook!`…）。我倾向沿用：已有 `shut-up!` 这个先例，而且对照 Doom 源码时最省事。
- **第一步从哪开始**：我建议从 P1a 开始，它最小、最独立，后面每一步都要用到它。

## 决定

（尚无）
