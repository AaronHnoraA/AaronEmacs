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

一个模块用 `(分类 . 名字)` 作为键，例如 `(:ui . init-ui)`。

分类沿用 Doom：`:core :os :ui :completion :editor :emacs :term :checkers :tools :lang :app :config`。另外加两个本配置特有的分类：

- `:remote`：Remote 框架。
- `:noema`：Noema 桥。

模块有两种形式：

- **目录模块**：`lisp/分类/名字/`，目录里按需放这些文件：
  - `init.el`：init 阶段加载。
  - `config.el`：config 阶段加载。
  - `autoload/*.el`：其中的 `;;;###autoload` 会生成到 `var/aaron/module-autoloads.el`，源文件变化时在启动时重新生成。
  - `doctor.el`：自检。
  - `.aaronmodule`：元数据，alist 形式，目前读 `depth`。
  - `README.md`：模块说明。

  这是目标形态。
- **feature 模块**：现有的单文件 `init-foo.el`，用 `require` 加载。模块逐个迁入目录形态时同时改名（见 `AGENT.md` 的命名规则），不保留兼容别名。

## 声明

```elisp
(aaron!
 :core   init-base
 :os     (:when (eq system-type 'darwin) (init-macos :optional t))
 :remote (init-remote :doctor remote-doctor)
 :lang   (init-org-tools :after org)
         (init-latex :autoload (my/latex-preview-doctor) :doctor my/latex-preview-doctor))
```

条目可以是 `NAME`，也可以是 `(NAME FLAGS... PROPERTIES...)`。FLAGS 是 `+x` / `-x`，重复声明同一个模块时按顺序合并。PROPERTIES 有：

| 属性 | 含义 |
| --- | --- |
| `:optional t` | 加载失败只记录并警告，启动继续；否则失败会中断启动 |
| `:after FEATURES` | 任一 FEATURE 加载后才加载，之前处于 `waiting` |
| `:autoload CMDS` | 只为这些命令注册 autoload，不加载模块 |
| `:feature SYM` | 实际 `require` 的 feature，默认就是 NAME |
| `:doctor CMD` | 模块自己的交互式 doctor，由模块面板提供入口 |
| `:depth N` | 加载位置，也可以写成 `(INIT . CONFIG)` |

还可以用条件形式选择模块：`(:when COND …)`、`(:unless COND …)`、`(:if COND THEN ELSE…)`、`(:cond (COND …)…)`。同一个分类关键字可以出现多次。

`modulep!` 查询某个模块（以及它的 flag）是否已声明：

- `(modulep! :tools init-lsp)`
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
  - 目录模块里定义 `my/` 名字。
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
- 在 `config` 注册表里登记的设置（按 `:source` 文件归属）；
- 诊断与策略命中，可以跳到源码行；
- 键冲突、hook、advice；
- 仍在运行的 timer，可以取消；
- 按键。

另外三个入口：

- **`M-x module-doctor`**（`aaron/doctor`）：诊断全部模块，然后打开面板。
- **`config-health`**：“Modules” 一节汇总声明数、各状态数、加载耗时、键冲突和诊断结果，并链接到面板。
- **`make doctor`**：在批处理里输出同一份诊断报告。

## 已知缺口

- **分类顺序**：声明目前保持迁移前的加载顺序，所以分类关键字会重复出现。按分类重排后有 166 行启动状态不同（hook 顺序、evil-collection 的键、`config-custom` 同步数量），说明模块之间还有隐式的顺序依赖。重排要随模块迁入目录逐个进行，每一步都用启动状态快照对比验证。
- **加载顺序决定的键冲突**：`global-map` 里的 `C-c w P/s/p`（`init-shell` 与 `init-browser`）、`` M-` ``（`init-base` 的 popper 与 `init-shell`）、`M-o`（`init-doom-extra` 与 `init-windows`）。现在由后加载的模块生效，改动需要决定由哪个模块持有这些键。
- **启动后才懒加载的源文件**：例如 `init-compile`、`remote-files`、`init-noema-publish`，这些文件还没有归属；迁入目录后由目录决定归属。
- **`:after` 被启动期间触发**：`init-auctex` 声明在 `tex`/`pdf-tools` 之后加载，但这两个在启动期间就被加载了，结果它在启动时耗时约 0.9s。
