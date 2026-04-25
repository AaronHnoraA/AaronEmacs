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
- 本地编码 + SSH/TRAMP + Org 笔记/学术写作

Linux 不是不能用，但部分体验默认按 macOS 配置。

macOS 图形界面下当前修饰键约定是：

- `Command = Meta (M-)`
- `Option = Hyper (H-)`

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

- `clangd`
- `rust-analyzer`
- Python 语言服务器
  这套配置走 `eglot`，你需要自己装对应 server
- `vale`
  如果要用写作 lint
- `vscode-html-language-server`
  用于 HTML / Vue HTML

### Org / 学术写作

- `xelatex`
- `dvisvgm`
- [tools/org-xdvisvgm-hires](../tools/org-xdvisvgm-hires) 依赖链能正常执行

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

### Org

[lisp/init-org.el](../lisp/init-org.el) 里默认写死：

- `~/HC/Org/`
- `~/HC/Org/roam/`
- `~/HC/Org/daily/`

### AI 助手

[lisp/init-ai-ide.el](../lisp/init-ai-ide.el) 管理两套 AI 助手：

当前本地源码来源已经切到 `site-lisp/ai-workbench/vendor/` 下的 vendored 包，
不再依赖这里现场拉取 VC 包。

**AI Workbench**（统一入口，实验中）

- `M-x ai-workbench` — 直接弹出当前后端的交互 buffer
- `M-x ai-workbench-compose-buffer` — 打开 compose buffer
- `C-c M-a` — 在当前文件里打开引用式 AI 工具入口
- `C-c A w` — 打开 workbench
- `C-c A m` — 打开 compose buffer
- `C-c A i r` / `C-c A i b` / `C-c A i f` — 直接把 region / 当前 buffer / 文件发给当前 backend
- 第一次打开会先选择 backend；profile 现在先固定为 `default`，文本在 `etc/ai-workbench/profiles/default.txt`
- workbench 不再停在中转页面，统一入口会直接把 Claude/Codex 的交互 session 弹出来
- 引用式工具入口生成的 prompt 会进入 popup compose buffer，并清掉上一次残留内容
- 后端 session 创建时会自动在项目目录启动，并自动注入一次 workdir/profile 提示
- compose buffer:
  `C-c C-c` 发送，`C-c C-b` 切后端，`C-c C-r` 注入 region，
  `C-c C-e` 注入当前 buffer，`C-c C-f` 注入文件
- diff 候选仍然先进入 Emacs diff buffer，再由用户决定是否 apply；引用式入口会在 `var/ai-workbench/` 放修改清单 manifest，profile 启动期会说明维护规则
- 当前先复用 Claude/Codex 的现有会话能力，同时保留旧快捷键

**Claude Code**（主力）

- CLI 路径：`claude-code-ide-cli-path`（默认 `/Users/hc/.local/bin/claude`）
- 入口：`C-c C-'` / `F12` / `H-l`
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
2. `M-x org-agenda` 是否能打开
3. `C-x C-f` / `C-x b` / `C-s` 是否符合预期
4. `M-x my/vterm-ssh` 是否能读到 SSH 主机
5. `C-c C-'` 是否能打开 claude-code-ide 菜单（需要 claude CLI 已安装）
6. `C-x g` 是否能打开 Magit

## 8. 下一步看什么

- 日常使用：看 [daily-usage.md](daily-usage.md)
- Org：看 [org-guide.md](org-guide.md)
- 编程 / 远程：看 [dev-guide.md](dev-guide.md)
- 想自己改：看 [settings-cookbook.md](settings-cookbook.md)
