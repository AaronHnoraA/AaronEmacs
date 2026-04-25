# Maintenance

这份文档讲长期维护，不讲日常使用。

## 1. 依赖管理

先记住边界：

- Emacs 包依赖已经进入锁文件、恢复和审计链路
- 系统级依赖还在 `make` 之外，需要机器层自己满足
- 恢复和锁文件审计不应依赖整套配置加载

### 更新锁文件

执行：

```sh
make lock
```

校验当前环境和锁文件是否一致：

```sh
make audit-lock
```

### 什么时候该更新锁文件

- 你新增了 package
- 你删除了 package
- 你把 VC 包 recipe 改了
- 你在另一台机器上完成了一次安装整理，想同步回来

### 当前已经解决到什么程度

就 Emacs 自身 package 依赖而言，现在已经可以：

- `make install`
  确定性恢复锁文件里的包
- `make audit-lock`
  检查当前环境和锁文件是否漂移
- `make up`
  做一键恢复 + 验收

其中前两步故意只依赖：

- `bootstrap.el`
- `package-lock.el`

不会先加载 `init.el`。

但像这些仍然属于系统外部依赖：

- `rg`
- `git`
- `latexmk`
- `dvisvgm`
- `hunspell`
- `gls`
- 本地编译工具链

这层目前还没有做成自动安装，只能检查、记录、补装。

### 本地 vendored Elisp

`site-lisp/` 里放的是不走 package-lock 的本地 Elisp。

当前启动链里有一个重要项：

- [site-lisp/general.el/general.el](../site-lisp/general.el/general.el)

`general.el` 是按键绑定 DSL，主要服务 `lisp/init-funcs.el` 里的 `my/evil-define-key`。它让 leader 绑定可以统一处理 Evil state、符号 keymap、真实 keymap object 和批量绑定。

维护边界：

- 它现在是启动依赖，不是可随手删除的实验目录
- `init.el` 会把 `site-lisp/general.el/` 加入 `load-path`
- 如果换机器、新 clone 或清理 `site-lisp/` 时漏掉它，`require 'general` 会导致启动链失败
- 如果继续 vendored 管理，需要把整个 `site-lisp/general.el/` 保留在仓库或迁移快照中
- 如果改成 package / VC 包管理，需要同步更新 `init.el` 的 load-path 和 package 恢复流程

### 新机器迁移时怎么做

执行：

```sh
make up
```

如果需要把旧机器的本地状态一起带回来：

```sh
make up SNAPSHOT=/path/to/emacs-state-YYYYMMDD-HHMMSS.tar.gz
```

这个入口的目标是：

- 可选先恢复状态快照
- 按 `package-lock.el` 恢复依赖
- 跑完整 bootstrap health
- 尽早暴露 `pdf-tools` / `vterm` / VC checkout / 锁文件漂移这类迁移问题

更轻量的路径才是：

```sh
make setup
```

这条命令的目标是：

- 按 `package-lock.el` 恢复依赖
- 避免把“本地碰巧已有几个包”误判成已有开发环境
- 立刻跑一次启动 smoke check，尽早暴露 `pdf-tools` / `vterm` / VC checkout 这类迁移问题

如果你想把这一步提升成更接近“迁移验收”的流程，执行：

```sh
make bootstrap-health
```

它会把：

- install
- startup / byte / native health
- critical doctor
- lock drift audit

串起来一次跑完。

如果你要做真正的跨机器恢复，推荐顺序是：

1. 旧机器执行 `make state-backup`
2. 新机器 clone 后执行 `make up SNAPSHOT=/path/to/archive.tar.gz`
3. 确认 `make audit-lock` 仍然是干净的

## 2. Elisp 编译

文件：

- [lisp/init-compile.el](../lisp/init-compile.el)

常用入口：

- `M-x my/compile-board`
  统一 board。看状态、做编译、做清理、跑 smoke check。
- `M-x my/compile-dispatch`
  transient 菜单版入口。
- `M-x my/byte-compile-config`
  编译整套本地配置。
- `M-x my/native-compile-config`
  对整套本地配置排队 native compile。
- `M-x my/byte-compile-current-file`
- `M-x my/native-compile-current-file`
- `M-x my/native-comp-open-log`

命令行入口：

- `make help`
- `make compile`
- `make compile-byte-force`
- `make compile-native`
- `make compile-native-force`
- `make clean-build`
- `make clean-eln`
- `make health`

leader 入口：

- `SPC c b`
  打开 compile board
- `SPC c ?`
  打开 compile dispatch
- `SPC c e`
  byte-compile `lisp/`
- `SPC c E`
  native-compile `lisp/`

默认策略：

- package 安装时允许 native compile
- native compile cache 统一放到 [var/eln-cache](../var/eln-cache)
- JIT native compile 开启
- async warning 策略走 `silent`
- `custom.el` 不纳入统一编译目标
- 不自动做 byte compile
- 启动时会删除本配置自有的 `lisp/` 和 `site-lisp/` 下 `.elc`，避免坏字节码或较新的错误 `.elc` 压过源码；`elpa/` 包字节码不受影响
- 可选自动 native compile on save，由 board / dispatch 统一开关

编译策略：

- 日常入口用 `make compile`、`make compile-byte`、`M-x my/byte-compile-config` 或 compile board 的 `[byte config]`
- 这些入口主要用于诊断 byte-compile warning/error；生成的本地 `.elc` 是可丢弃产物，下一次正常启动会自动删除
- 在同一个 Emacs 会话内重复运行时，这些入口仍是增量编译，只会处理缺失或源码比 `.elc` 更新的文件
- 只有明确需要重建字节码时才用 `make compile-byte-force`、compile board 的 `[force byte]` 或 dispatch 里的 `force byte config`
- 修启动链、清理过旧字节码、怀疑 `.elc` 内容坏了时，才算需要 force

## 3. 性能 / 功耗观察

文件：

- [lisp/init-performance.el](../lisp/init-performance.el)

常用入口：

- `M-x my/performance-watch`
  打开运行时观察 board。
- `SPC h p`
  同上。

这个 buffer 默认打开到独立 frame，避免和 dashboard/Org 分屏共绘。默认只在打开和按 `g` 时采样；如果按 `a` 临时开启自动刷新，`q` 退出时会停止刷新 timer 并关闭监管 frame。顶部有用法区和概览条形图。里面可以看：

- `ps` 里的 Emacs CPU、内存、RSS、子进程。
- Emacs runtime：buffer/process/timer/GC/read-process-output-max。
- Emacs process 列表。
- hook 表的全局和当前 buffer-local 激活数量。
- Org buffer 列表、是否可见、局部 hook 数量、LaTeX 预览队列/overlay/pending 数。
- 最大 buffer 列表和 timer/idle timer 分组。

给 agent 或批处理用的只读 API：

- `(my/performance-report-string &optional inspected-buffer)`
  返回完整纯文本报告，不打开 board，也不启动刷新 timer。
- `(my/performance-snapshot &optional inspected-buffer)`
  返回结构化 plist，包含当前采样、hook、Org buffer、最大 buffer、timer 和 idle timer 摘要。
- `(my/performance-hook-snapshot &optional buffer hooks)`
  返回指定 buffer 的 hook 计数和本地条目。
- `(my/performance-org-buffer-snapshot)`
  返回 Org buffer 的可见性、局部 hook 数量、LaTeX queue/overlay/pending 等状态。

记录保存在 [var/performance/](../var/performance/)：

- `s` 会把当前采样追加到当天的 `performance-YYYYMMDD.tsv`。
- `R` 会切换录制模式，录制开启后每次刷新都会追加一条。
- `o` 打开记录目录。

按键：

- `g` 立即刷新。
- `y` 复制当前完整页面，方便粘给 agent 或 issue。
- `a` 切换自动刷新。
- `s` 保存当前采样。
- `R` 切换录制模式。
- `o` 打开记录目录。
- `p` 启动 CPU profiler。
- `P` 打开 profiler report。

## 4. 状态目录

集中在 [var/](../var/)：

- backup
- auto-save
- lockfiles
- tramp
- eln-cache
- company
- copilot
- projectile
- transient
- dirvish
- treemacs
- org

这意味着：

- 项目目录更干净
- 状态问题可以定向删除
- 出故障时比“散落在仓库里”更容易排查

### 状态备份与恢复

这套配置现在提供本地状态快照：

```sh
make state-backup
make state-restore SNAPSHOT=/path/to/emacs-state-YYYYMMDD-HHMMSS.tar.gz
```

它会优先覆盖迁移价值高但不该进 git 的内容，例如：

- `etc/`
- project / projectile / transient 状态
- session / recentf / savehist / save-place
- org 持久化状态

不会把 `eln-cache`、包目录、其他重建型缓存也打进去。

快照默认写到：

- [var/backup-snapshots](../var/backup-snapshots)

## 4. 常见清理

### 清编译产物

优先直接在 `my/compile-board` 里做：

- clean `.elc`
- clean config `.eln`
- reset eln cache
- clean all managed artifacts

### 清 transient 历史

删：

- [var/transient](../var/transient)

### 清 projectile 历史

删：

- [var/projectile](../var/projectile)

### 清 dirvish / treemacs 状态

删：

- [var/dirvish](../var/dirvish)
- [var/treemacs](../var/treemacs)

### 清 backup / auto-save

删：

- [var/backup](../var/backup)
- [var/auto-save](../var/auto-save)

### 清 TRAMP 状态

删：

- [var/tramp](../var/tramp)

## 5. 升级 package 的基本原则

- 普通包：优先 `use-package :ensure t`
- VC 包：优先 `my/package-ensure-vc`
- 不要到处手写零散的 `package-vc-install`
- 包职责尽量放回对应模块，不要堆到 `init.el`

## 6. 当前需要记住的结构边界

- [lisp/init-base.el](../lisp/init-base.el)
  基础行为、字体、系统交互、状态目录
- [lisp/init-ui.el](../lisp/init-ui.el)
  主题、dashboard、help、window behavior
- [lisp/init-minibuffer.el](../lisp/init-minibuffer.el)
  vertico/orderless/consult/embark
- [lisp/init-search.el](../lisp/init-search.el)
  ivy/counsel/swiper
- [lisp/init-lsp.el](../lisp/init-lsp.el)
  company/eglot/lsp-mode/flymake/dape
- [lisp/init-lsp-tools.el](../lisp/init-lsp-tools.el)
  language server hub / doctor / dispatch / runtime knobs
- [lisp/init-org.el](../lisp/init-org.el)
  Org 全家桶
- [lisp/init-shell.el](../lisp/init-shell.el)
  shell/eshell/vterm/ssh

## 7. 常见故障排查

### Emacs 能启动，但某功能没反应

先看：

1. 这个命令是不是懒加载的
2. 依赖包是否已安装
3. 外部工具是否在 PATH

### warning 老是冒出来打断操作

现在普通 `warning` 默认只记录到 `*Warnings*`，不会自动弹窗。

需要手动查看时：

1. `SPC h w`
2. 或者 `M-x my/show-warnings-buffer`
3. native compile 相关问题先看 `M-x my/native-comp-open-log`

### LSP 挂了

看：

1. `M-x my/language-server-doctor`
2. 当前 buffer 的 route policy / active backend 对不对
3. 语言服务器 executable 或 feature 是否存在
4. `M-x my/language-server-manager` 里能不能直接开 log / session
5. 当前 major mode 是否真的命中了你以为的那条路由

### Org LaTeX 预览挂了

看：

1. `xelatex`
2. `dvisvgm`
3. [tools/org-xdvisvgm-hires](../tools/org-xdvisvgm-hires)

### `my/vterm-ssh` 没读到主机

看：

1. `~/.ssh/config` 是否可读
2. Host 行是不是通配符
3. 是否真的写成了独立 Host 条目

### Dired 排版奇怪

看：

1. `gls` 是否存在
2. macOS 上 coreutils 是否装好

## 8. 推荐维护节奏

每隔一段时间做一次：

1. 更新依赖
2. 跑一次 batch 启动检查
3. 清理不用的键位和旧包
4. 同步更新 [docs/](.)

如果你继续大改配置，记得同时更新 [docs/README.md](README.md) 里的索引。
