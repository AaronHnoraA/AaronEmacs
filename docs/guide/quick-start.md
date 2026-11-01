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

- [init.el](../../init.el)
  主入口。
- [early-init.el](../../early-init.el)
  提前做启动优化。
- [lisp/](../../lisp)
  主配置模块。
- [lisp/lang/](../../lisp/lang)
  语言专项配置。
- [bootstrap.el](../../bootstrap.el)
  依赖导出/恢复入口。
- [package-lock.el](../../package-lock.el)
  锁文件。
- [var/](../../var)
  运行时状态目录。备份、自动保存、eln-cache、transient、projectile、dirvish 等状态都集中放这里。
- [docs/](..)
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
- 首次拉起时最容易炸的主题、ligature、`tramp-rpc`、`portal`、`pdf-tools` 这类 Emacs 内部依赖

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
  文本 buffer 与 Noema 默认在输入停顿后仅检查带上下文的可视区域，并按段落缓存结果：编辑后只重发被改动的段落。问题按 LanguageTool 的类别着色——拼写红、语法橙、标点黄、用词紫、排版青、风格蓝——Emacs 与 Noema 一致。Emacs 中结果进入统一 Flymake 诊断面（不显示行尾文案），光标停留或悬停时 echo area 显示「类别: 说明 → 建议」，`M-n` / `M-p` 跳转，手动检查按类别汇总数量；右键菜单提供替换/忽略此处/加入词典/关闭规则。Noema 中单击下划线打开卡片：按类别着色的类别名、说明、建议按钮，以及忽略/加入词典/关闭规则。“忽略”按规则与所在行上下文记忆，别处编辑不会让它重现。加入的词写入共享的 `etc/prose-accepted-words.txt`，关闭的规则写入共享的 `etc/prose-disabled-rules.txt`，两端都读这两个文件；`M-x +languagetool/enable-rule` 重新启用规则。`C-c i g` / `Cmd-Shift-C` 手动检查，NAS 失败时仅手动检查回退本地 CLI；`C-u C-c i g` 检查整个 Emacs buffer，`C-c i a` 修正当前位置，`C-c i A` 从当前问题中选择，`C-c i x` 清除到下一次编辑。Noema 的 `Tools > LanguageTool` 可管理 NAS、语言、检查级别、性能档位、超时、实时检测和手动 CLI fallback
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
  `+portal/ssh` 会优先读这里的 Host

## 5. 字体依赖

当前配置直接引用这些字体：

- `Merriweather`
- `Fira Code`
- `Excalifont`
- `FZLiuGongQuanKaiShuJF`
- `JetBrainsMono Nerd Font`

缺字体不一定阻止启动，但界面观感会明显变化。

## 6. 路径约定

### AI 助手

共享配置入口是 [lisp/tools/agent/config.el](../../lisp/tools/agent/config.el)。
Emacs 管理 Agent 的连接、队列、权限、上下文与视图；Noema 提供 Markdown 和知识服务。
agent-shell、acp.el、shell-maker 使用固定版本的 package-vc 依赖，可选上游源码
见 `site-lisp/AGENT-UPSTREAMS.md`。Noema 的这些交互不依赖 gptel。

- 打开 Emacs 主程序进入 Dashboard，常驻 Pi 后台初始化；进入项目时复用项目 Pi。
- `M-x noema` / `C-c A W` 打开项目 Pi；`M-x noema-pi-emacs-open` 打开主 Pi。
- Dashboard 的 `pi`、`M-x pi` / `C-c A P` 召回主 Pi，再次调用收起；带前缀打开项目 Pi。
  `M-x noema-pi-sessions` 管理历史；Ibuffer 的 Pi 组列出实际 buffer。
- `C-c A a` 选择并启动普通 Agent；`C-c A S` 打开共享 Agent Manager。
- `C-c A c` 通过项目 Pi Skill 创建并打开普通 Markdown prompt 文档，编辑保存后
  用 `C-c A s` 发给已有 Agent；`C-c A m` 打开会话菜单。
- `C-c A .` 添加上下文，`C-c A ,` 检视，`C-c A x/v/B/f/@` 发送上下文、选区、
  buffer、文件或光标位置的引用。
- `C-c A r` 请求改写；Noema 通过文档 API 生成审阅提案，用户接受或反馈。
- `C-c A p` 仅用于普通任务 Agent 的 Research 关联，Pi 会话单独管理。

完整流程见 [Pi 会话与项目助手](../design/pi-sessions.md)、
[Agent 管理](../design/agent-management.md) 和
[Noema 交互](../noema/design/interaction-plan.md)。Skill／pack 按项目方法、Emacs 操作
和 Markdown 分为 `noema-project`、`noema-emacs`、`noema-md`。

### 运行时状态目录

配置已经统一把容易污染项目目录的文件收到了 [var/](../../var)：

- backup
- auto-save
- eln-cache
- lockfiles
- tramp
- company / copilot / projectile / transient / dirvish / treemacs 等状态目录

编译和清理现在统一走：

- `M-x +compile/board`
- `SPC c b`

## 7. 启动后先确认什么

建议启动 Emacs 后依次确认：

1. 主题和字体是否正常
2. `C-x C-f` / `C-x b` / `C-s` 是否符合预期
3. `M-x +portal/ssh` 是否能读到 SSH 主机
4. `C-c C-'` 是否能打开 claude-code-ide 菜单（需要 claude CLI 已安装）
5. `C-x g` 是否能打开 Magit

## 8. 下一步看什么

- 日常使用：看 [daily-usage.md](daily-usage.md)
- Typst 写作：看 [settings-cookbook.md](settings-cookbook.md)
- 编程 / 远程：看 [dev-guide.md](dev-guide.md)
- 想自己改：看 [settings-cookbook.md](settings-cookbook.md)
