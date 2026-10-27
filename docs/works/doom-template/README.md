# 以 Doom 为模板升级配置（第一阶段）

方向（用户定）：**不依赖 Doom，但把 Doom 里好、而本配置没有或没做到的东西直接拿过来，包括它的抽象结构。** 先做一轮配置一体化和抽象化升级，再在这个基础上吸收 neo-emacs（第二阶段，见 [../neo-emacs/](../neo-emacs/README.md)）。

- 上游代码在 `upstream/`，由 `.gitignore` 忽略：
  - `upstream/core`：[doomemacs/doomemacs](https://github.com/doomemacs/doomemacs)，`59cdaa3`（2026-10-02），是 Doom 的内核。
  - `upstream/modules`：[doomemacs/modules](https://github.com/doomemacs/modules)，`42a0479`（2026-10-02）。v3 起模块从内核仓库拆了出来，共 154 个模块，约 4.2 万行。
  - 更新用 `git -C docs/works/doom-template/upstream/<core|modules> pull`。
- 许可：两个仓库都是 MIT。拿过来的代码在文件或函数头注明出处，格式为 `Adapted from Doom Emacs (MIT, Henrik Lissner): <上游路径> @ <commit>`。
- 骨架不变：`package.el` + `use-package`、`init-modules.el` 作为唯一顺序、Remote-first、config 注册表、`general.el`。命名改为照 Doom（见“决定”）。Doom 的 straight 包管理、profiles 和 `doom` CLI 框架不引入。

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
5. **`modulep!`**：等价物就叫 `modulep!`，模块之间用它判断对方是否启用，代替现在的 `featurep` 或 `fboundp` 试探。
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

- 本配置已有 `shut-up!` 这种不带前缀、以 `!` 结尾的宏名先例，所以这批宏沿用 Doom 的名字；函数和变量按“决定”里的命名。
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
- `lang/python`、`lang/java`、`lang/markdown`
- `os/macos`

## 推进顺序

1. **P1a 核心库**：C 的宏，加上 B 的生命周期 hook 和增量加载，放进核心层，并补上测试。现有行为不变。
2. **P1b 模块系统**：A 的模块声明、清单、flag、Health 汇总，以及 `lisp/` 按分类分目录（feature 名不变）。行为不变。
3. **P1c 注册 API**：D，从 popup、查找、格式化、eval 开始，逐个把语言模块改成声明式。
4. **P1d 默认行为比对**：E 和 F。
5. **P1e 模块目录比对**：H，按领域逐个进行。
6. **收尾**：结构结论写进 `docs/architecture.md`、`AGENT.md` 和对应的 design 文档，本工作台移到 `records/`。之后开始第二阶段（neo-emacs）。

每一步一个或几个提交，每个提交都要能完整启动，并通过 `make` 测试和 `config-health`。

## 决定

- 2026-10-26 分类：`lisp/` 下按 Doom 的分类分目录（`core ui editor emacs completion checkers tools lang term app os input config`），Remote 与 Noema 桥作为额外分类保留。
- 2026-10-26 命名：全量照 Doom。核心 `aaron-`（`aaron--`、`aaron/`），模块 `+模块-`（`+模块--`、`+模块/`），hook 函数 `-h`，advice `-a`，宏沿用 Doom 原名。核心文件名也对应 Doom：`aaron-lib.el` ↔ `doom-lib.el`，`aaron-emacs.el` ↔ `doom-emacs.el`。
- 2026-10-26 `my/` 迁移：随 P1b 模块搬迁逐个改名，`etc/config-store.el` 键与 Noema/Raycast 引用在同一改动里迁移，不留兼容别名和 fallback（用户：直接融合）。
- 2026-10-26 用户习惯和原有逻辑不变：本轮只优化后端。发现的键冲突、提前加载等只报告，不擅自改行为。
- 2026-10-26 Remote 库是底层：`:remote` 分类固定在 `:core`/`:os` 之后加载，模块系统发起的后续加载都经过它。
- 2026-10-26 第一步做 P1a。

## 进度

- [x] P1a 核心库：`lisp/core/aaron-lib.el`（`after!` `add-hook!` `remove-hook!` `setq-hook!` `unsetq-hook!` `add-transient-hook!` `defadvice!` `undefadvice!` `letf!` `quiet!` `quiet!!` `with-delayed-gc!` `cmd!` `cmd!!` `cmds!` `fn!` `lambda!` `defer-until!` `defer-feature!` `file!` `dir!`，`aaron-log`、`aaron-run-hooks`、`aaron-run-hook-on`）；`lisp/core/aaron-emacs.el`（`aaron-first-input/file/buffer-hook`、`aaron-init-ui-hook`、`aaron-switch-buffer/window/frame-hook`、`MODE-local-vars-hook`、`aaron-load-packages-incrementally`）。`make core-test` 32 项。与 Doom 的差异：
  - `aaron-run-hook-on` 触发后把自己从所有触发点摘掉（Doom 留在 `pre-command-hook` 上）。
  - 用 `after-init-time` 判断启动完成，代替 Doom 的 `doom-context`。
  - 增量加载在本机目录、只留 `jka-compr` handler 的环境里 `require`，远端 buffer 里也不会把加载发到 target（Remote 规则）；定时器存起来，可用 `aaron/cancel-incremental-loading` 取消。
  - `letf!` 的 `(defadvice FN HOW ADVICE)` 形式按文档实现（Doom 上游把 FN 当成了 advice）。
  - `after!` 不再查 Doom 的 `doom-disabled-packages`；`load!`、`add-load-path!`、`versionp!`、`doom-context`、pcase 扩展没拿。
  - 尚未有模块挂到这些 hook 上；迁移现有的 `after-init`/`window-setup`/首次触发逻辑归入 P1b。
- [x] P1b-1 模块系统（`docs/design/module-system.md`）：`lisp/core/aaron-modules.el`（`aaron!`、`modulep!`、Doom 的 init/config 两阶段与四个阶段 hook、目录模块与 feature 模块、目录模块 autoloads、`:optional/:after/:autoload/:doctor/:depth`、`aaron/load-module`）；`init-modules.el` 改为 `aaron!` 声明（98 个模块，Doom 分类加 `:remote`、`:noema`）；`:core/:os/:remote` 固定为最先加载的底层，启动后由模块系统发起的加载都走 `remote-require-on-client`。
  - 监督：加载账本记录每个模块装的 hook（693）、advice（327）、timer（66）、键、以及它 `require` 进来的文件（173），并区分包默认键与本配置的键来检测模块间键冲突；`aaron-doctor`（加载状态、冲突、`AGENT.md` 规则的策略检查、目录模块 `doctor.el`）；`module-board` / `module-doctor`；Health 的 “Modules” 一节与 `make doctor` 接入同一份数据；LSP/Remote/LaTeX/Jupyter/DAP 的 doctor 以 `:doctor` 挂到模块上；`config` 注册表的设置按来源文件归到模块。
  - 验证：启动状态快照（特性、全部 hook 顺序、advice、所有 keymap 绑定、全部 custom 变量，23197 行）与迁移前逐行比较，只多出新特性和四个阶段 hook；账本开关对启动耗时的影响在测量误差内；`make core-test` 46 项、Health 测试 13 项通过。
  - 去掉了 `my/require-module-safely` 等旧入口和兼容别名，Health 与其测试直接改用新接口（用户要求：不留 fallback，直接融合）。
  - 发现（只报告，未改用户行为）：5 个由加载顺序决定的键冲突（`C-c w P/s/p`、`` M-` ``、`M-o`）；`init-auctex` 因 `tex-site`/`pdf-tools` 启动即加载而在启动时耗时约 0.9s；95 处策略命中（41 `file-remote-p`、47 `global-set-key`、7 `"local"` 比较）；按分类重排声明会改变 166 行启动状态，说明存在隐式顺序依赖，重排随目录迁移逐个做。
- [x] P1b-2 目录迁移与改名（2026-10-26，一次完成，用户要求不留 fallback）：
  - **目录**：190 个文件迁入 `lisp/分类/模块/`，每个文件的 feature 就是它在 `lisp/` 下的路径（`tools/lsp/config`）；核心是平铺的 `lisp/core/aaron-*.el`；Remote 框架库原位保留，`remote/framework/.aaronmodule` 把它归入 framework 模块。
  - **改名**：5799 个 `my/` 符号全部改名，对照表见 `docs/records/migrations/naming-migration-2026-10.md`。同一改动里迁移了所有调用方：Noema 仓库的 Elisp/TS、`bin/emacs-app`、Raycast、`etc/config-store.el` 等私有配置，以及 M-x 历史、transient 历史等状态文件。
  - **声明**：`aaron!` 改用新的模块键，原先由 `tools/lsp` 连带加载的 lang 模块、`noema/jupyter`、`checkers/grammar` 和 `config/compile` 在它们实际加载的位置补上声明。
  - **删除**：`init-mail`、`init-stock`、`init-ignored`（空模块），`lisp/roam/` 下一个无主的编辑器备份文件。
  - **验证**：迁移前的启动状态快照按改名表换算后，与迁移后的快照逐行比较，剩下的差异只有比较脚本的编码误差和主机名变化（`Mac` → `AaronMac.local`）。全部 Makefile 测试目标通过；`agenda-apple-test` 卡在 Node 版本，与本改动无关。`remote-transfer-test` 原本就跑不起来，修好 Makefile 后有 4 项失败，迁移前同样失败。Noema 测试 3360 项通过。另外把每个测试文件在新旧两棵树上各跑一遍，逐项比对结果。
- [x] P1c 注册 API（[comparison.md](comparison.md) 第 5 节，[module-system.md](../../design/module-system.md) “注册接口”）：
  - **popup**：`set-popup-rule!` / `set-popup-rules!` 放在 `ui/popup/autoload/`，shackle 由 `ui/popup` 统一管理，7 条开发 buffer 规则分别归入它们所属的模块。
  - **evil 状态**：新增 `set-evil-initial-state!`，12 处改用它；本配置自己的 board mode 从 `editor/evil` 的集中列表搬回各自模块。
  - **LSP 注册**：5 个函数改名为 `set-…!`。
  - **删除**：从未被调用的 `set-company-backends-for!`，以及两处从未定义、只有 `fboundp` 保护的偏好注册调用。
  - **autoloads**：目录模块的 autoloads 生成后，路径改为相对 `lisp/`。
  - **验证**：启动状态只多出 `module-autoloads` 这个 feature；`evil-emacs-state-modes` 成员不变；`shackle-rules` 仍是 23 条，`compilation-mode` 仍排第一，其余开发规则之间的谓词互不重叠。core 50 / lsp 131 / ui 96 项测试通过。
- [x] 行为不变的验证固化为硬规范（`AGENT.md` “Verification”）：`make state-baseline` / `state-check`、`make test-baseline` / `test-check`，工具在 `scripts/state-snapshot.el`、`scripts/test-all`。
- [x] P1f 把 `site-lisp` 里本配置自己的基础设施收进核心（对应 Doom 的 `core/lisp/lib/`）。
  - **收进核心**：`config`（配置注册表）平铺进 `lisp/core/`；`general.el` 由用户完全内化，也平铺进 `lisp/core/`，上游测试移到 `test/general/`，Cask/Makefile/README 等上游构建文件删除；`aaron-ui`（board 组件库、主题 token、`vendor/` 主题、`ghostty.conf`）整个目录进 `lisp/core/aaron-ui/`，`~/.config/ghostty/config` 链接和 `scripts/build-portal-local` 已更新。feature 和 `config-*` / `general-*` / `aaron-ui-*` 公开命名不变。
  - **留在 `site-lisp`**：`dape-toolbar` 是独立发布的包（有 Eask、LICENSE、CHANGELOG）；`ratex.el`、`vendored/`、`noema`、`portal`、`remote_ikernel` 不动。
  - **删除**：空的 `site-lisp/init.sh`。
  - **验证**：`state-check` 只剩 `+compile-target-directories` 去掉 `site-lisp/config` 这一行；`test-check` 无变化。顺带修好 `make activity-test` / `performance-test`：这两个 `-Q` 套件单独加载模块，自 `set-…!` 接口改为 autoload 后就找不到它们，现在先调用 `aaron-modules-load-standalone-autoloads`。
- [x] P1g 用 Doom 的抽象增强 config 与 general.el。
  - **键位**：新增 `lisp/core/aaron-keybinds.el`，改写自 Doom 的 `+keybinds.el`：`map!`、`define-leader-key!`、`define-localleader-key!`（原 `aaron-leader!` / `aaron-local-leader!`，41 处调用一并改名）、`define-key!`、`undefine-key!`。leader 机制不变（Evil `<leader>`），`:desc` 转成 general 的 `:which-key`。没有调用者的 `aaron-evil-define-key`、`aaron-evil-global-leader-set`、`aaron-leader-key-label` 删除。
  - **模块化**：`config`、`general`、`keybinds` 成为 `:core` 的前三个模块，面板上有账本；`init.el` 不再单独 `require 'config`。
  - **配置注册表**：`:source` 改记 `lisp/` 下的路径（迁移后所有模块文件都叫 `config.el`，原来的文件名归属已失效，面板上每个模块都会显示所有 `config.el` 的设置）。新增 doctor 策略 `config-setq`，并让策略支持函数正则和 `:when` 过滤。
  - **验证**：`state-check` 只多出 `FEATURE aaron-keybinds`；`test-check` 见提交说明。
  - **发现（已修，见下一项）**：`config-setq` 在加载期间命中 5 处：`completion-styles`（`completion/minibuffer` 的 `:init` 设为 `(orderless basic partial-completion)`，盖过 `etc/config-store.el` 里的 `(basic partial-completion substring flex)`，board 显示的存储值实际不生效）、`undo-tree-auto-save-history`（`aaron-base` 两处，与存储值相同）、`treesit-language-source-alist`（`editor/fold`，另有 `etc/config-treesit.el`）、`persp-state-default-file`（`ui/session`，登记前先给默认值）。
- [x] P1h 修复 P1g 和账本发现的问题，行为保持当前实际生效的样子。
  - `completion-styles`：存储值改成实际生效的 `(orderless basic partial-completion)`，去掉 `completion/minibuffer` 里的 `setq`。
  - `treesit-language-source-alist`：去掉 `editor/fold` 里 3 项的 `setq`；它在 treesit 晚于存储加载时会把 16 项的存储值盖掉。
  - `undo-tree-auto-save-history`：去掉 `aaron-base` 里两处与存储值相同的 `setq`。`persp-state-default-file` 是登记前给的计算默认值，标 `aaron-allow`。
  - 键冲突 5 处：删除被后加载模块覆盖、从未生效的绑定（`aaron-base` 的 `` M-` `` popper-cycle、`ui/windows` 的 `C-c w s`、`config/default` 的 `M-o` link-hint、`term/portal` 的 `C-c w p/P`）。`daily-usage.md` 里写的 `C-c w p/P` 移入/移出 popup 本来就不生效，已改为右键菜单。`term/portal/popup.el` 的 `global-set-key` 改用 `general-define-key`。
  - 验证：`config-setq` 0 处，键冲突 0 处；`state-check` 只剩 `hexl-mode-map`（加载时复制了当时的 `global-map`）里两条旧绑定消失；`test-check` 全部一致。
- [x] P1d 默认行为与键位比对：[comparison.md](comparison.md) 第 2–4 节。leader 的 10 个空位已加入；首次使用延迟已按用户选择吸收，`save-place` 与 Emacs 默认开启的 `show-paren` 保留原时机；其他手感候选仍待决定。
- [x] P1e 模块目录比对：[comparison.md](comparison.md) 第 1 节。
- [x] 启用模块的实现对照：[comparison.md](comparison.md) 第 6 节。工作区切换前清理
  Evil Visual/普通选区；删除 Winner、tab 同步、tab line 和导航里从另一种
  `persp-mode` 移来的无效挂钩。其余差异按现有输入、窗口和 Remote 契约保留。
- [x] 逐行审计（2026-10-27）：`doom-emacs.el` 的函数、hook、advice，`lib/` 的命令，
  以及本配置启用领域对应模块里的每个 HACK/FIX/PERF 补丁，结果在
  [comparison.md](comparison.md) 第 7 节。收进来的：长路径状态文件名哈希、
  savehist 保存 kill ring、recentf 定时保存、大段删除后继续自动保存、保存后重判
  major mode、minibuffer 提示符不可进入、`J` 去注释符与 `gq` 不压缩空格、跳转时
  打开折叠、Magit 关长行优化、PDF 页面隐藏光标、apropos 用 Helpful、复制项目相对
  路径。延迟关闭 LSP server 先记为缺口 G-L1，后已按契约实现（第 21 节）。第 2 节的手感
  与第 4 节未吸收的键位候选仍待用户决定。
- [x] 逐文件补完（2026-10-27）：`core/lisp/lib/` 每个文件、框架文件和本配置用到的
  每个语言与工具模块，结果在 [comparison.md](comparison.md) 第 8–11 节。期间修了
  本配置自己的缺陷：`SPC f D` 不进废纸篓、加密文件的 undo 历史写成明文、Elisp 的
  Flymake 编译检查在第一个 `require` 就失败、lsp-mode 按 server id 的签名提炼对
  `+rust-analyzer`/`+clangd` 不生效、`.lock` 全按 TOML、`.mts`/`.cts` 无模式、macOS
  上 `locate` 查不到、打开文件仍会提示安装 grammar。
