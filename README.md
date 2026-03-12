# Aaron's Emacs Config

这是一个长期使用中的 Emacs 主力配置，目标是：

- 保持核心体验稳定，不为了“现代化”牺牲现有工作流。
- 在不破坏功能的前提下持续优化启动速度、维护成本和迁移体验。
- 同时支持普通 ELPA/MELPA 包和 `package-vc` 安装的包。

## 当前状态

这套配置已经做过一轮结构化重构，重点包括：

- 启动链去重，减少重复初始化和重复启用的全局 mode。
- 可选模块尽量改成命令或 hook 驱动，避免无意义的启动期开销。
- 统一 VC 包声明入口，避免 `package-vc-install` 散落在多个文件里。
- LSP、Org、TRAMP 开始按“本地 / 远端 / 大文件”三种场景做差异化取舍。
- `bootstrap.el` 现在会同时记录和恢复：
  - `package-selected-packages`
  - `package-vc-selected-packages`

## 目录结构

- [init.el](/Users/hc/.config/emacs/init.el)
  Emacs 入口，负责包系统、基础启动参数和模块加载。
- [early-init.el](/Users/hc/.config/emacs/early-init.el)
  提前做 GC、UI 和 file-name-handler 的启动优化。
- [bootstrap.el](/Users/hc/.config/emacs/bootstrap.el)
  迁移/初始化入口，负责导出或恢复 lock 文件。
- [package-lock.el](/Users/hc/.config/emacs/package-lock.el)
  自动生成的锁文件。
- [lisp/](/Users/hc/.config/emacs/lisp)
  主配置模块。
- [lisp/lang/](/Users/hc/.config/emacs/lisp/lang)
  语言相关模块。

## 包管理约定

普通包：

- 通过 `use-package` 的 `:ensure t` 或 `package-install` 安装。
- 最终导出到 `package-selected-packages`。

VC 包：

- 一律通过 [lisp/init-package-utils.el](/Users/hc/.config/emacs/lisp/init-package-utils.el) 里的 `my/package-ensure-vc` 声明。
- 这个 helper 会做两件事：
  - 缺包时用 `package-vc-install` 安装。
  - 无论是否已安装，都把 recipe 记录到 `my/package-vc-recipes`。
- `bootstrap.el` 导出 lock 文件时，会把这些 recipe 自动写入 `package-vc-selected-packages`。

这样做的目的，是让 VC 包不再只是“本机装过一次”，而是变成可迁移、可重建的配置一部分。

## Bootstrap 与迁移

### 1. 在已有环境里更新 lock 文件

在当前机器执行：

```sh
make install
```

或者：

```sh
emacs --debug-init -q -l ./bootstrap.el
```

`bootstrap.el` 会根据当前环境判断：

- 如果本机已经有足够多的第三方包，认为这是“源环境”，会加载 [init.el](/Users/hc/.config/emacs/init.el) 并导出新的 [package-lock.el](/Users/hc/.config/emacs/package-lock.el)。
- 如果本机几乎没有第三方包，认为这是“目标环境”，会从 [package-lock.el](/Users/hc/.config/emacs/package-lock.el) 恢复。

### 2. 在新机器上恢复

复制整个配置目录后，执行：

```sh
make install
```

这会自动完成两类包的恢复：

- archive 包用 `package-install`
- VC 包用 `package-vc-install`

## 新增 VC 包的正确方式

不要再手写散落的 `package-vc-install`。

请用：

```elisp
(my/package-ensure-vc 'some-package "https://github.com/owner/repo.git")
```

如果需要固定 revision：

```elisp
(my/package-ensure-vc 'some-package "https://github.com/owner/repo.git" "commit-or-tag")
```

这样它才能被 `bootstrap.el` 自动记录进 lock 文件。

## 启动优化原则

当前配置遵循这些规则：

- 必须在启动时就生效的能力，才允许 eager load。
- 只提供命令、mode、hook 的模块，优先延后到首次使用时加载。
- 避免同一能力在多个模块里重复初始化。
- 避免在 batch / noninteractive 场景下做 GUI 专属初始化。

例如：

- `exec-path-from-shell` 只在 macOS 的 GUI/daemon 场景下初始化。
- `gptel`、`jupyter`、`joplin`、部分 tree-sitter 工具改成按命令或 mode 触发。
- `server`、`recentf`、`save-place`、`so-long` 等重复启动点已收口。
- `eglot`、`company-box`、`eldoc-box`、`aggressive-indent` 会避开大文件和重 UI 不合适的场景。
- Org 的 `mixed-pitch`、`valign`、`org-appear`、`org-fragtog` 等视觉增强只在合适的本地图形界面里启用。
- TRAMP 会补上更完整的远端 PATH，方便远端 `clangd`、`pyright`、`ruff`、`node` 之类工具被找到。

## 核心体验约定

这套配置当前优先保证三种日常体验：

- 本地编码：尽量保留丰富补全和文档 UI，但对大文件自动降载。
- SSH / TRAMP 编码：减少花哨 UI，优先保证响应、PATH 完整性和连接稳定。
- Org 笔记：本地笔记保留视觉增强，远端或超大笔记自动回退到更轻的模式。

另外现在可以直接用 `M-x my/vterm-ssh`，它会优先读取 `~/.ssh/config` 里的 Host，开一个专用 `vterm` 并直接发起 SSH 连接。

## 还在持续优化的点

这套配置还没有到“满分”状态，当前剩下的主要工作是：

- 继续拆分 [lisp/init-base.el](/Users/hc/.config/emacs/lisp/init-base.el) 这个大文件。
- 统一 minibuffer/completion 栈，降低 `ivy/counsel` 与 `vertico/consult` 双栈并存的维护成本。
- 继续清理历史遗留包和不再使用的安装记录。

但这些优化都以“不破坏核心体验”为前提。
