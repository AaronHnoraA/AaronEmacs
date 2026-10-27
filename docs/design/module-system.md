# 模块系统

配置由一张模块表组成：声明、加载、查询、监督都走同一套机制。实现位于 `lisp/core/`，以 Doom Emacs 的 `doom-modules.el` 和 profile loader 为模板（MIT，来源写在各文件头）。

| 文件 | 内容 |
| --- | --- |
| `aaron-lib.el` | 宏和 hook 运行器（`after!`、`add-hook!`、`defadvice!`、`aaron-run-hooks` 等） |
| `aaron-emacs.el` | 生命周期 hook 与增量加载 |
| `aaron-modules.el` | 模块记录、`aaron!` 声明、`modulep!`、加载器、加载账本（ledger） |
| `aaron-doctor.el` | 模块诊断：加载状态、账本冲突、策略检查、模块的 `doctor.el` |
| `aaron-modules-board.el` | 监督界面：`aaron/modules`、`aaron/describe-module`、`aaron/doctor` |

`lisp/init-modules.el` 只保留配置自己的 `aaron!` 声明，它就是唯一的模块顺序。

## 模块

一个模块用 `(分类 . 名字)` 作为键，例如 `(:ui . theme)`。

分类沿用 Doom：`:core :os :ui :completion :editor :emacs :term :checkers :tools :lang :app :config`。另外加两个本配置特有的分类：

- `:remote`：Remote 框架。
- `:noema`：Noema 桥。

每个模块是一个目录 `lisp/分类/名字/`，按需放这些文件：

- `config.el`：config 阶段加载，结尾 `(provide '分类/名字/config)`。
- 同目录的其他文件 `FILE.el`：结尾 `(provide '分类/名字/FILE)`，由 `config.el` 或别的模块 `require`。
- `init.el`：init 阶段加载。
- `autoload/*.el`：其中的 `;;;###autoload` 会生成到 `var/aaron/module-autoloads.el`，源文件变化时在启动时重新生成。
- `doctor.el`：自检。
- `.aaronmodule`：元数据，alist 形式，读 `depth` 和 `files`（目录外也属于本模块的源文件，相对模块目录的 glob，例如 Remote 框架库）。
- `README.md`：模块说明。

例外：

- `:core` 是平铺的 `lisp/core/aaron-*.el`，和 Doom 的 `doom-*.el` 一样，声明为 `(base :feature aaron-base)`。
- `config`、`config-tools`、`config-custom`（配置注册表）、`general`（键位 DSL）和 `aaron-ui/`（board 组件、主题 token、`vendor/` 主题、`ghostty.conf`）也在核心里，但保留各自的公开命名（`config-*`、`general-*`、`aaron-ui-*`），用 `:feature` 声明。`aaron-ui/` 是核心里唯一的子目录，因为它带着主题和非 Elisp 文件。`:core` 的前三项是 `config`、`general`、`keybinds`（`aaron-keybinds`）：注册表最先加载，后面每个模块加载时都能登记设置；键位层随后，所以模块面板也给这三项记账。
- `aaron-keybinds` 是 Doom `+keybinds.el` 的改写：`map!`、`define-leader-key!`、`define-localleader-key!`、`define-key!`、`undefine-key!`。leader 仍是 Evil 自己的 `<leader>` / `<localleader>`（normal state，在 `editor/evil` 里设置），`:desc` 转成 general 的 `:which-key`，所以 which-key 标签和以前一样；不带 Doom 的 `:prefix-map`、leader 键选项和 `general-auto-unbind-keys`。
- Remote 框架库 `lisp/remote/remote-*.el` 保留 `remote-*` 的公开 feature 名。

**与 Doom 的差异**：Doom 按路径加载 `config.el`，模块之间通过 `autoload/` 共享函数。本配置让每个模块文件都是一个 feature，名字就是它在 `lisp/` 下的路径，所以只要 `lisp/` 在 `load-path` 上就能 `require`。这样做有三个原因：

- 模块之间有约 180 处 `require`；
- `with-eval-after-load` 用到模块 feature；
- 异步 native 编译要在独立子进程里找到宏的来源。

这些都依赖 feature。由此带来两个好处：加载顺序完全保持迁移前的样子；一个模块被别的模块提前 `require` 过，就不会再加载第二次。

## 声明

```elisp
(aaron!
 :core   (base :feature aaron-base)
 :os     (:when (eq system-type 'darwin) (macos :optional t))
 :remote (framework :doctor remote-doctor)
 :lang   markdown
         (latex :autoload (+latex/preview-doctor) :doctor +latex/preview-doctor))
```

条目可以是 `NAME`，也可以是 `(NAME FLAGS... PROPERTIES...)`。FLAGS 是 `+x` / `-x`，重复声明同一个模块时按顺序合并。PROPERTIES 有：

| 属性 | 含义 |
| --- | --- |
| `:optional t` | 加载失败只记录并警告，启动继续；否则失败会中断启动 |
| `:after FEATURES` | 任一 FEATURE 加载后才加载，之前处于 `waiting` |
| `:autoload CMDS` | 只为这些命令注册 autoload，不加载模块 |
| `:feature SYM` | 实际 `require` 的 feature；目录模块默认是 `分类/名字/config` |
| `:doctor CMD` | 模块自己的交互式 doctor，由模块面板提供入口 |
| `:depth N` | 加载位置，也可以写成 `(INIT . CONFIG)` |

还可以用条件形式选择模块：`(:when COND …)`、`(:unless COND …)`、`(:if COND THEN ELSE…)`、`(:cond (COND …)…)`。同一个分类关键字可以出现多次。

`modulep!` 查询某个模块（以及它的 flag）是否已声明：

- `(modulep! :tools lsp)`
- `(modulep! :t m +flag)`
- 在模块自己的文件里可以省略分类和名字：`(modulep! +flag)`。

## 加载

`aaron-modules-load` 照 Doom 的阶段依次执行：

1. 加载目录模块的 autoloads。
2. foundation 模块（分类深度为负的模块）的 `init.el`。
3. 运行 `aaron-before-modules-init-hook`。
4. 其余模块的 `init.el`。
5. 运行 `aaron-after-modules-init-hook`。
6. 运行 `aaron-before-modules-config-hook`。
7. 每个模块的 `config.el` 或 feature。
8. 运行 `aaron-after-modules-config-hook`。
9. 深度 ≥ 100 的模块。

同一阶段内先按深度排序，深度相同时按声明顺序。

**分类深度**：`:core` -110、`:os` -105、`:remote` -100，其余分类默认 0。所以 Remote 框架永远紧跟在核心和系统层之后，是其余所有模块的底层；无论 `:remote` 模块在声明里写在哪里，都会提前到这个位置。

**通过 Remote 加载**：`aaron-modules-load` 结束后，凡是由模块系统发起的加载都经过 `remote-require-on-client`，所以从远端 buffer 触发的加载不会把文件操作发到 target。包括：

- `:after` 模块到点加载；
- `aaron/load-module`；
- 增量加载（`aaron-load-packages-incrementally`）；
- 目录模块在客户端目录下加载。

启动期间则直接用 `require`：这时当前 buffer 是本地的，而 `remote-require-on-client` 对 `process-environment` / `exec-path` 的临时绑定会丢掉模块在加载时做的设置。

`:autoload` 模块由 Emacs 自己的 autoload 机制加载，不经过上面这条路径。

**记录**：每个模块记下状态（`loaded` / `failed` / `waiting` / `autoload`）、加载耗时和错误。这些信息在三处使用：

- `aaron-module-load-failures`、`aaron-module-optional-modules` 和 `aaron-module-deferred-triggers` 供 Health 使用；
- `aaron/load-module` 用来重试失败的模块，或者立刻加载正在等待的模块；
- `aaron-modules-init-time` 是全部模块的加载耗时。

## 注册接口（`set-…!`）

模块用 Doom 风格的 `set-…!` 接口声明自己需要什么，由拥有对应子系统的模块统一实现。这些接口放在实现它的模块的 `autoload/` 里，因此任何模块在加载时都能调用，不必先 `require`，也不必关心那个模块是否已经加载。

| 接口 | 实现模块 | 用法 |
| --- | --- | --- |
| `set-popup-rule!`、`set-popup-rules!` | `ui/popup`（shackle） | 声明本模块 buffer 的弹出方式。规则写法同 shackle（缓冲区名、`:regexp t` 正则或 major mode，加 shackle 的键）。和 Doom 一样，后声明的优先。`ui/popup` 自己只声明内置 buffer 的默认规则 |
| `set-evil-initial-state!` | `editor/evil` | 声明本模块 mode 的初始 Evil 状态；Evil 尚未加载时会等它加载后再生效。Remote 框架层不依赖编辑器层，所以仍直接调用 `evil-set-initial-state` |
| `set-language-server!`、`set-language-server-feature!` | `tools/lsp` | 注册自定义 lsp-mode client / 声明包自带的 client |
| `set-language-server-toolchain-provider!`、`set-language-server-runtime-provider!` | `tools/lsp` | 注册项目工具链与运行时 provider |

只有在已经存在、只是分散各处的注册代码需要收拢时，才加一个这样的接口。查找走 Emacs 自己的 `xref-backend-functions` 链；格式化先走 LSP，没有活动服务器时由 `editor/format` 的规则在 target 上运行外部 CLI；文件模板按目录组织。这些规则目前由格式化模块统一拥有，所以没有加 `set-lookup-handlers!`、`set-formatter!` 和 `set-file-template!`。

## 加载账本

模块加载期间（`aaron-modules-load`、每次延迟加载、`aaron/load-module`），账本临时挂上 advice，记录当前模块装了什么：

| 种类 | 来源 |
| --- | --- |
| `hooks` | 非 buffer-local 的 `add-hook` |
| `advice` | `advice-add` |
| `timers` | `run-at-time`（含 `run-with-timer`）和 `run-with-idle-timer` 返回的 timer |
| `keys` | 本配置自己代码里的 `define-key`（`keymap-set`、`general.el`、`evil-define-key` 最终都会调用它） |
| `files` | `aaron-module-load-path` 下第一次由该模块加载的源文件，用来把 `require` 进来的子文件归到模块名下 |

规则：

- **代码归属**：模块加载期间注册的 `eval-after-load` 函数，在启动期间运行时仍然归属于注册它的模块。
- **键冲突**：只有“本配置的代码”才拥有按键，包括 `lisp/` 下的文件，以及模块注册的 `eval-after-load` 函数。包自己的默认绑定归 `:package` 所有，模块覆盖它不算冲突。两个模块把同一个键绑成不同命令时，记入 `aaron-module-key-conflicts`；结果完全由加载顺序决定，两个模块中必须由一个改动。
- **范围**：advice 只在加载期间存在，加载结束即移除，启动后的状态与不记账时完全相同。之后才运行的代码不会被记账。记账可以用 `aaron-module-ledger-enabled` 关闭，它对启动耗时的影响在测量误差之内。

## 诊断与策略

`aaron-doctor-module` 给一个模块出诊断，数据有四个来源：

- **加载状态**：失败记为 error，等待记为 warning。
- **账本键冲突**。
- **策略检查**：`aaron-module-policies` 用 `AGENT.md` 里的规则扫描模块源码，跳过注释和字符串：
  - `file-remote-p` 分支、与 `"local"` 比较、经 `default-directory` 查询进程：`:remote` 分类除外；
  - `global-set-key`；
  - 直接调用 `package-vc-install`；
  - 定义旧的 `my/` 名字；
  - `config-setq`：在加载期间用 `setq` / `setq-default` / `setopt` 设置一个已在 `config` 注册表登记的设置。这种值会盖过 `etc/` 里的存储值，而 config-board 显示的仍是存储值。函数体、hook 体和 advice 体里的赋值（`aaron-doctor-runtime-forms`）是运行时改动，不算。

  策略的 `:regexp` 可以是返回正则的函数（`config-setq` 的正则取自注册表），`:when` 是在每个匹配处调用的过滤函数。
- **模块的 `doctor.el`**：用 Doom 的 `assert!`、`warn!`、`error!`、`explain!` 写。

例外：某一行确实是规则允许的例外（例如 TRAMP 兼容边界上的 `file-remote-p`）时，在同一行加注释 `aaron-allow: 规则ID`。

模块自己的深度 doctor（LSP、Remote、LaTeX、Jupyter、DAP）是交互式的，用 `:doctor` 声明，在模块面板上提供入口，不在批量诊断里运行。

## 监督入口

**`M-x module-board`**（`aaron/modules`）按分类列出每个模块：

- 状态、加载耗时、装了多少 hook / advice / timer / 键、flag、诊断条数；
- 全局键冲突；
- 还没有归属任何模块的源文件。

在一行上按 RET 打开 `aaron/describe-module`，显示：

- 源文件；
- 在 `config` 注册表里登记的设置：注册表把登记所在文件记为 `:source`，取它在 `lisp/` 下的路径（`config-source-label`，如 `tools/lsp/config.el`），面板按模块源文件的同一标签归属；
- 诊断与策略命中，可以跳到源码行；
- 键冲突、hook、advice；
- 仍在运行的 timer，可以取消；
- 按键。

直接 `M-x aaron/describe-module`（或 `aaron/load-module`）时，读模块的提示以光标处的
模块为默认值（`aaron-module-at-point`）：在模块自己的 `lisp/CATEGORY/NAME/` 文件里是
这个模块；在 `aaron!` 声明里是光标下的名字，类别取上方最近一个开头的类别关键字。
只认已经声明的模块。

另外三个入口：

- **`M-x module-doctor`**（`aaron/doctor`）：诊断全部模块，然后打开面板。
- **`config-health`**：“Modules” 一节汇总声明数、各状态数、加载耗时、键冲突和诊断结果，并链接到面板。
- **`make doctor`**：在批处理里输出同一份诊断报告。

## 已知缺口

- **分类顺序**：声明保持迁移前的加载顺序，所以分类关键字会重复出现。按分类重排会使 166 行启动状态不同（hook 顺序、evil-collection 的键、`config-custom` 同步数量），说明模块之间还有隐式的顺序依赖。重排会改变行为，需要逐组决定，每一步都用启动状态快照对比验证。
- **账本只覆盖加载期间**：启动后才懒加载的文件（如 `config/compile`、`noema/bridge/publish`）按所在目录归属；它们在之后运行时装的 hook、advice、timer 和键不进账本。
- **`:after` 被启动期间触发**：`lang/auctex` 声明在 `tex`/`pdf-tools` 之后加载，但这两个在启动期间就被加载了，结果它在启动时耗时约 0.9s。
