# Quick Start

这份文档解决三个问题：

1. 这套配置依赖什么。
2. 新机器怎么装起来。
3. 装好后哪些目录和文件最重要。

先记一个结论：

- 默认一键入口是 `make up`
- Emacs 包依赖已经纳入锁文件和恢复链路
- 系统外部依赖仍然需要机器本身满足

## 1. 环境定位

这套配置明显偏：

- macOS 图形界面 Emacs
- 重 UI、重功能、重集成
- 本地编码 + SSH/TRAMP + Typst note / 学术写作

Linux 不是不能用，但部分体验默认按 macOS 配置。

macOS 图形界面下当前修饰键约定是：

- `Command = Meta (M-)`
- `Option = Hyper (H-)`

因此常见编辑键位里，`M-c` / `M-v` 会直接走系统剪贴板复制 / 粘贴；`H-<tab>`
是统一的“切换当前折叠”入口；`H-?` 会调用一次性 Codex 读取本地 `docs/`
回答使用问题。

## 2. 核心目录

- [init.el](../init.el)
  主入口。
- [early-init.el](../early-init.el)
  提前做启动优化。
- [lisp/](../lisp/)
  主配置模块。
- [lisp/lang/](../lisp/lang/)
  语言专项配置。
- [bootstrap.el](../bootstrap.el)
  依赖导出/恢复入口。
- [package-lock.el](../package-lock.el)
  锁文件。
- [var/](../var/)
  运行时状态目录。备份、自动保存、eln-cache、transient、projectile、dirvish 等状态都集中放这里。
- [docs/](.)
  这套使用文档。

## 3. 首次安装

在配置目录执行：

```sh
make up
```

如果你还带着旧机器导出的状态快照：

```sh
make up SNAPSHOT=/path/to/emacs-state-YYYYMMDD-HHMMSS.tar.gz
```

`make up` 会：

- 可选先恢复本地状态
- 按 `package-lock.el` 恢复依赖
- 跑完整 bootstrap health 验收

这里有个刻意的设计原则：

- `make install`
- `make audit-lock`

都只走 `bootstrap.el` + `package-lock.el`，不要求先正常加载整套配置。
因为恢复阶段本来就处在“配置依赖可能还没满足”的状态，这一步必须专心做下载安装和审计。

这条链路已经覆盖：

- ELPA / NonGNU / Org / MELPA 普通包
- `package-vc` 管理的 VC 包
- 首次拉起时最容易炸的主题、ligature、`tramp-rpc`、`vterm`、`pdf-tools` 这类 Emacs 内部依赖

它不负责安装系统级依赖，例如 Homebrew / apt 层面的工具。

如果你只想做最小拉起：

```sh
make setup
```

如果你只想恢复依赖，不跑检查：

```sh
make install
```

如果你是维护者，要把当前环境导回锁文件：

```sh
make lock
```

如果你想检查当前环境和锁文件是否已经漂移：

```sh
make audit-lock
```

如果你想做一次更完整的迁移验收：

```sh
make bootstrap-health
```

直接调用 bootstrap 时，推荐显式指定模式：

```sh
BOOTSTRAP_MODE=install emacs -q -l ./bootstrap.el
BOOTSTRAP_MODE=export emacs -q -l ./bootstrap.el
```

推荐流程：

- 新机器 / 新 clone：`make up`
- 只恢复包：`make install`
- 新增或删除包后更新锁文件：`make lock`
- 检查锁文件漂移：`make audit-lock`
- 做完整拉起验收：`make bootstrap-health`

如果你还想把本地 project/session/history 状态一起带走：

```sh
make state-backup
make state-restore SNAPSHOT=/path/to/emacs-state-YYYYMMDD-HHMMSS.tar.gz
```

直接启动 `init.el` 时，如果检测到本地几乎还没有第三方包，也会先按
`package-lock.el` 补齐依赖，避免在 theme、modeline 之类的首批模块上中途失败。
不过首装依然建议优先跑一次 `make up`。

## 4. 必装外部依赖

这里说的是系统级依赖，不是 Emacs 包依赖。

换句话说：

- Emacs 包依赖：`make up` / `make install` 负责
- 系统外部依赖：你机器上要先有，或后续再脚本化安装

### 基础工具

- `git`
- `ripgrep`，用于 `consult-ripgrep`、`rg`
- `fzf`，用于 `fzf` 集成
- `hunspell`，用于拼写检查
- `terminal-notifier`，macOS 通知
- `gls`，推荐通过 coreutils 提供，供 `dired` / `dirvish` 使用

### 编程相关

- `gh`
  GitHub issue、PR、contributor 和 patch 工作台；首次使用前运行 `gh auth login`
- `bazelisk`
  Bazel 工作台首选入口；目标机器只有 `bazel` 时会自动回退
- `buildifier`
  BUILD、MODULE 和 `.bzl` 文件保存时格式化
- `clangd`
- `rust-analyzer`
- Python 语言服务器
  这套配置走 `eglot`，你需要自己装对应 server
- `languagetool`（Homebrew）
  文本 buffer 与 Noema 默认在输入停顿后仅检查带上下文的可视区域；结果进入统一 Flymake 诊断面，但不显示抢眼的行尾文案。光标停在问题上或鼠标悬停时会在 echo area 简要说明原因，`M-n` / `M-p` 可跳转，右键菜单提供替换/跳过/忽略规则/加入文件词典；左键保持普通编辑行为。`C-c i g` / `Cmd-Shift-C` 手动检查，NAS 失败时仅手动检查回退本地 CLI；`C-u C-c i g` 检查整个 Emacs buffer，`C-c i a` 修正当前位置，`C-c i A` 从当前问题中选择，`C-c i x` 清除到下一次编辑。Noema 的 `Tools > LanguageTool` 可管理 NAS、语言、检查级别、性能档位、超时、实时检测和手动 CLI fallback
- `vscode-html-language-server`
  用于 HTML / Vue HTML

### Typst / 学术写作

- `typst`
- `tinymist`
- `pngpaste`
  用于把 macOS 剪贴板图片粘到 Typst note

### 终端 / 远程

- `zsh`
- `ssh`
- `~/.ssh/config`
  `my/vterm-ssh` 会优先读这里的 Host

## 5. 字体依赖

当前配置直接引用这些字体：

- `Merriweather`
- `Fira Code`
- `Excalifont`
- `FZLiuGongQuanKaiShuJF`
- `JetBrainsMono Nerd Font`

缺字体不一定阻止启动，但界面观感会明显变化。

## 6. 路径约定

### Typst

项目外写 assignment 时，用 `templates/typst/assignment.typ` 插入模板；插入时会在项目根目录创建
`_typst/*.typ` 软链，指向 [notes/](../notes/) 里维护的样式文件。

### AI 助手

[lisp/init-ai-ide.el](../lisp/init-ai-ide.el) 提供 Noema 的统一 AI/agent 入口。
gptel、agent-shell、acp.el、shell-maker、Magent 及现有 CLI 兼容代码均完整内化在
`site-lisp/noema/upstream/`，不再从 `package.el`、VC package 或旧 workbench 目录加载。

**Noema AI/agent**

- `M-x noema` / `C-c A W` — 打开 Noema（默认进入 Magent 的 agent-shell 会话）
- `M-x noema-agent-start` / `C-c A a` — 选择并启动 Magent、Codex、Claude、OpenCode 或 Pi agent-shell 会话
- `M-x noema-compose` / `C-c A c` — 使用完整 gptel UI 打开任意 buffer compose
- `C-c A s` — 从当前 buffer 发送；`C-c A m` — gptel transient；`C-c A .` / `C-c M-a` — 添加上下文
- `C-c A r` — 使用 gptel rewrite/diff 预览；`C-c A p` — 把当前 agent-shell 会话纳入 Noema research
- CLI sampler 只作为 gptel backend 的降级，不再提供第二套 interaction Hub/transcript/session UI；region、buffer、file 一律从 gptel compose/context 发送。
- profile 与 prompt 模板在 `etc/noema/`；Magent session/audit 状态在 `var/noema/`

Noema 不重写 gptel 或 agent-shell。gptel 提供 compose/context/rewrite UI，agent-shell + ACP
持有结构化外部 agent 会话，Magent 提供本地 agent、queue、ledger 与 gptel adapter；Noema
把这些能力连接到 Project、WorkNode、Run 和 Artifact。

Claude/Codex 的兼容源码仍在 Noema `upstream/`，但不再绑定全局快捷键；从
`C-c A a` 的 agent-shell 入口选择它们。
- 需要先在终端 `npm install -g @anthropic-ai/claude-code` 或 brew 安装 claude CLI

**Codex CLI**（可选）

- 入口前缀：`C-c c`
- 需要 `codex` 可执行文件在 PATH 中

### 运行时状态目录

配置已经统一把容易污染项目目录的文件收到了 [var/](../var/)：

- backup
- auto-save
- eln-cache
- lockfiles
- tramp
- company / copilot / projectile / transient / dirvish / treemacs 等状态目录

编译和清理现在统一走：

- `M-x my/compile-board`
- `SPC c b`

## 7. 启动后先确认什么

建议启动 Emacs 后依次确认：

1. 主题和字体是否正常
2. `C-x C-f` / `C-x b` / `C-s` 是否符合预期
3. `M-x my/vterm-ssh` 是否能读到 SSH 主机
4. `C-c C-'` 是否能打开 claude-code-ide 菜单（需要 claude CLI 已安装）
5. `C-x g` 是否能打开 Magit

## 8. 下一步看什么

- 日常使用：看 [daily-usage.md](daily-usage.md)
- Typst 写作：看 [settings-cookbook.md](settings-cookbook.md)
- 编程 / 远程：看 [dev-guide.md](dev-guide.md)
- 想自己改：看 [settings-cookbook.md](settings-cookbook.md)
