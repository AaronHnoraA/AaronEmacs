# 框架对照与结构融合方案

本文只回答一个问题：neo-emacs 的框架和本配置的框架，在**文件结构**和**项目组织**上怎么合成一个。

逐条配置怎么吸收，记在 [inventory.md](inventory.md)（下一步）。

> 第一阶段改为先以 Doom 为模板升级骨架（[doom-template](../doom-template/README.md)）。本文 §3 的目录和模块声明方案以那边定稿为准，§2、§3.3 的映射关系仍然有效。

原则（用户定）：**全量吸收 neo-emacs 的配置和细节，骨架用本配置的。**

## 1. 两边的框架

### neo-emacs：Doom 内核 + 私有目录

neo-emacs 分两层。Doom 本身装在 `~/.config/emacs`，作者的配置放在 `$DOOMDIR`（`~/.doom.d`），也就是 upstream 这个仓库：

```text
~/.doom.d/                      ($DOOMDIR，上游仓库)
├── init.el                     (doom! :分类 模块 (模块 +flag) …)：模块开关和顺序
├── packages.el                 (package! 名 :recipe …)：私有包声明，doom sync 时用 straight 安装
├── config.el                   私有胶水：字体/主题/键位/退出流程/TRAMP 补丁…（724 行）
├── keymap.el                   零散的 map!
├── modules/neo-emacs/<模块>/   作者的 20 个模块
│   ├── config.el               模块行为（加载时执行）
│   ├── packages.el             模块自己的包（可选）
│   └── readme.org (+md/html)   模块说明（多数有）
├── lisp/lr-macos-glass.el      单文件扩展
├── neoemacs/                   随仓库带的二进制和本地包：rime 动态库、字体、
│                               tramp-rpc 二进制、ultra-scroll、org-modern-indent…（54M）
├── snippets/<major-mode>/      yasnippet 片段
├── patches/emacs30|31/         编译 Emacs 用的补丁（毛玻璃、不透明 face 等）
└── images/                     README 与启动页图片
```

加载流程：Doom 内核先起来，再按 `doom!` 里写的顺序，对每个启用的模块依次加载它的 `init.el` 和 `config.el`，最后加载私有的 `config.el`。

- `+flag` 用来开关模块里的可选部分，模块代码里用 `(modulep! +flag)` 判断。
- 包在 `doom sync` 时统一安装，启动时不安装。
- `init.el` 里有 13 个 neo-emacs 模块是启用的：frontend eglot shell common org rime translate find sql modelinexp elfeed git agentshell。
- 另外 7 个没启用：modeline windows alpha addspace eredis telega java。
- 除了作者自己的模块，它还依赖 Doom 的约 40 个模块（`:completion corfu`、`:ui popup/workspaces/vc-gutter`、`:editor evil`、`:tools lsp +eglot` 等）。这些 Doom 模块本身也带了大量默认行为。

### 本配置：单一顺序 + 分层 + 横切机制

```text
~/.config/emacs/
├── early-init.el → init.el     包管理（package.el + use-package）、登录 shell 环境、load-path、etc/
├── lisp/init-modules.el        唯一的模块顺序（require / my/require-module-safely /
│                               my/require-module-after-any-feature）
├── lisp/init-*.el              约 100 个平铺模块，按 docs/architecture.md 的 0–7 层排列
├── lisp/{remote,roam,lang,git,telescope,patches}/   有边界的子系统
├── site-lisp/                  自维护/vendored 包（general.el、config、aaron-ui、portal、noema…）
├── etc/                        私有配置、config-store.el、remote.json
├── var/                        运行时状态
├── snippets/ templates/        片段与 auto-insert 模板
├── test/                       ERT，按模块命名
└── docs/                       guide/design/acceptance/ops/records/works
```

## 2. 概念对照

| neo-emacs / Doom | 本配置的对应物 | 融合时怎么处理 |
| --- | --- | --- |
| `init.el` 的 `doom!` 声明（分类 + 开关 + 顺序） | `lisp/init-modules.el` | **吸收形式**：把 `init-modules.el` 改成分类声明（见 §3.2），仍是唯一顺序 |
| 分类 `:ui :editor :tools :lang :app …` | architecture.md 的 0–7 层 | 以本配置的分层为准，给每层起分类名，对应到目录 |
| `modules/<分类>/<模块>/` 目录 | 平铺的 `lisp/init-*.el` | **吸收**：按分类分子目录；feature 名 `init-foo` 不变 |
| `+flag` / `modulep!` | 无（只有可选模块和延迟加载） | **吸收**：flag 放进 config 注册表，模块里用 `my/module-flag-p` 读 |
| 模块里的 `packages.el` | 模块内的 `use-package … :ensure t` + `package-lock.el` | 保留本配置做法：包声明就写在使用它的模块里，不另开 packages 文件 |
| `package! :recipe (:host github …)` | `my/package-register-vc` / `my/package-ensure-vc` | 一一转写；不引入 straight |
| `unpin!`、`:pin` | `package-lock.el` | 锁定和解锁走现有的 lock 流程 |
| 模块 `readme.org` | `docs/guide`、`docs/design` 集中存放 | **吸收**：每个分类目录放一份 `README.md` 作为索引，只写本目录的模块和入口，规则仍写在 `docs/` |
| `config.el` 私有胶水 | 没有对应的大杂烩文件 | **不保留形式**：724 行逐条拆进所属模块 |
| `keymap.el` / `map!` | `general.el`；leader 在 `init-evil.el`，域内后缀在所属模块 | 转写成 `general-define-key` / leader 定义；冲突逐键裁决 |
| `after!` / `use-package!` / `setq-hook!` / `defadvice!` | `with-eval-after-load` / `use-package` / `add-hook` / `advice-add` | 机械转写 |
| `doom-load-theme-hook` | `enable-theme-functions`、`after-load-theme-hook`（init-ui） | 转写 |
| `doom-switch-buffer-hook` / `doom-switch-window-hook` | `window-buffer-change-functions` / `window-selection-change-functions` | 转写 |
| `user-private-dir`、硬编码路径、代理、JDK 路径 | `etc/`（私有）+ config 注册表；工具链走 Remote 环境胶囊 | 值放进 `etc/config-store.el`，不写死在 Lisp 里 |
| `neoemacs/` 二进制与本地包 | `site-lisp/vendored/`（Elisp）、`var/`（下载产物）、系统依赖（brew） | Elisp 进 vendored；二进制不进 git，由安装步骤生成 |
| `snippets/` | `snippets/` | 按 major-mode 合并，重名的逐个比较 |
| `patches/emacs31/` | `docs/ops`（构建说明），以及 `init-neomacs` 兼容层 | 依赖补丁的特性（毛玻璃、`:ns-opaque-background`）在兼容层里判断能力是否存在，没有补丁时不生效 |
| Doom 自带模块的默认行为 | `init-doom-extra.el` 已收了一部分 | 他用到的 Doom 模块逐个列入 inventory，缺的行为补进对应模块 |

## 3. 融合后的结构

### 3.1 目录：按分类分子目录，feature 名不变

```text
lisp/
├── init-modules.el            分类声明（唯一顺序）
├── core/      层 0  init-utils init-base init-open init-macos init-neomacs …
├── remote/    层 1  已有，不动
├── ui/        层 2  init-ui init-tabbar init-display-rules init-activity …
│                    ＋ neo：主题与字体细节、glass、modeline 细节、ultra-scroll
├── editor/    层 2/4 init-evil init-editing-workbench init-avy init-multiple-cursors …
│                    ＋ neo：键位习惯、drag-stuff、ialign、auto-space
├── tools/     层 3  init-lsp* init-format init-dev init-debug* init-test init-task …
│                    ＋ neo：补全细节、Java mapper 跳转、verb
├── git/       已有
├── telescope/ 已有
├── workbench/ 层 4  init-navigation* init-dired init-windows init-project* init-workspaces …
│                    ＋ neo：dirvish/dired 细节、窗口布局切换、bookmark+
├── ops/       层 5  init-maintenance init-performance init-health init-server init-background …
│                    ＋ neo：keyfreq、退出流程保护
├── roam/      层 6  已有（Noema 桥）
├── app/       层 7  init-portal init-shell init-browser init-search* init-clutch …
│                    ＋ neo：rime、translate、elfeed、telega、eredis、agent-shell 面板
├── input/     层 7  init-function-keys init-hyper init-mouse
├── lang/      已有  ＋ neo：java/frontend/sql 的细节
└── patches/   已有
```

- `init.el` 把每个分类目录加进 `load-path`，`(require 'init-foo)` 照常可用。所以挪文件不用改调用方、测试和文档里的 feature 名，只改路径。
- 每个分类目录有一份 `README.md`：一行说明本层做什么，再列出模块，每个模块一句话加入口命令。这是对 neo 模块 readme 的吸收。详细规则仍在 `docs/`，不写两份。
- 层的依赖方向不变：下层不 require 上层。这是 architecture.md 已有的约束，搬家时要保持。

### 3.2 模块声明：`init-modules.el` 改成分类形式

现在是一长串 `require`。改成 `doom!` 式的分类声明，宏展开后仍是同样的 require 序列：

```elisp
(my/modules!
 :core      init-utils init-base init-open (patches/init-patches)
            (init-macos :os darwin :safe t) (init-neomacs :safe t)
 :remote    init-remote init-emacs-gateway
 :ui        init-ui init-activity init-activity-noema init-tabbar init-display-rules
            (init-glass :safe t)                        ; neo: liquid glass
 :editor    init-evil …
 :tools     init-lsp init-format …
 :app       (init-rime :os darwin :safe t) (init-translate :safe t)
            (init-feeds :safe t) (init-telega :safe t)
 :lang      …)
```

- 一项可以是裸 feature，也可以带属性：`:safe` 对应 `my/require-module-safely`，`:after (org)` 对应 `my/require-module-after-any-feature`，`:os` 是平台条件，`:autoload (cmd …)` 只注册命令，`:flags (+foo)` 是默认 flag。
- 开关和 flag 的值放在 config 注册表里（`my/module-disabled`、`my/module-flags`），在 `config-board` 里可以改。代码里不出现 setq。这里吸收的是 `doom!` 里“注释掉一行就关掉模块”的体验，但没有引入 Doom。
- Health board 已经记录可选模块和加载失败，声明式写法正好给它提供完整的模块表。
- 包恢复依旧不用加载 `init.el`：声明宏只管加载顺序，不管装包。

### 3.3 neo 的东西落在哪

- **有现成归属的**（字体、主题、补全、dired、git、org、键位…）：拆进对应模块。不新建“neo”文件，也不在代码里标“来自 neo”；来源只记在 inventory 里。
- **本配置没有的领域**（rime、translate、elfeed、telega、eredis、auto-space、glass、keyfreq）：在对应分类下新建模块，名字沿用 `init-*`，比如 `app/init-rime.el`、`ui/init-glass.el`。
- **和本配置硬约束冲突的**：保留功能，换实现，在 inventory 里写明怎么换。
  - eglot：AGENT.md 规定 lsp-mode 是唯一客户端。他的 JDT 配置（`jdt://` 反编译缓存、Maven sources、`java.apply.workspaceEdit`、补全注解去重）改到 lsp-java 和本配置的 LSP 层上。
  - corfu：他用 corfu，本配置补全走 Company。外观和行为参数映射过来，不并存两个前端。
  - vterm：本配置的终端是 Portal/Ghostel。他对 vterm 的字体、margin、光标、模式切换细节迁到 Ghostel/Portal 上。
  - Doom popup / popwin / zygospore：映射到 `init-display-rules` 和 `init-windows`。
  - flycheck：本配置用 flymake，只迁规则（哪些模式不检查）。
  - `file-remote-p` 分支：他的退出保护、recentf、projectile root 都按 `file-remote-p` 分支。按 Remote-first 规则，改成问 backend 声明的代价，并且走 `remote-workspace-disconnect-target` 的语义。
  - 硬编码的 JDK、Maven、代理路径：改走环境胶囊和 `etc/`。

## 4. 推进顺序

1. **inventory.md**：把 upstream 每个文件拆成条目（设置 / 包 / 键 / 函数 / hook），每条写目标模块和处理方式（直接吸收 / 已有、需比较 / 换实现 / 不收及理由），附上用到的 Doom 模块清单。
2. **结构先行**：做 §3.2 的声明宏和 §3.1 的目录搬迁，行为零变化。验证：全量启动、`make` 测试、`config-health` 无失败、`check-parens`。
3. **按分类吸收**：每次一个分类，一个提交。对应的 guide/design 文档和 `daily-usage.md` 的键位表同步更新。
4. **收尾**：架构结论写进 `docs/architecture.md` 和 `AGENT.md`，本工作台移到 `records/`。

## 5. 待定（需要你拍板）

- 分类名和分法是否按 §3.1。另一种分法是照 Doom 的名字（`:completion :ui :editor :emacs :term :checkers :tools :os :lang :app`）。
- 键位冲突的总体原则。他的 `;` 前缀（`; w` 保存、`; x` M-x、`; d` agent 面板…）、`J/K` 切 workspace、`C-j/C-k` 滚动，和本配置现有的键可能撞车：是以他为准，还是以你现有的为准、他的另找位置？
- 主题和字体：他用 tokyonight-night + JetBrains Mono + 方正悠宋，你用 aaron-ui 主题。是整体换掉，还是只吸收机制（CJK 字号联动、按模式换字体、line-number face）？
