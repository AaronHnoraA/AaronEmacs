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

Aaron UI / Noema 设计 token 另有一条不依赖 Noema 构建的审计链：

```sh
make ui-tokens
make ui-test
make audit-ui-tokens
```

修改 `site-lisp/aaron-ui/aaron-ui.el` 中的 token 后运行 `make ui-tokens`，
把确定性 CSS 导出物一并提交。Noema 运行时只读取该 CSS，不依赖 Emacs。

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

### TeXpresso 本地构建

TeXpresso 不进入 `package-lock.el`：源码、上游 Emacs mode 和二进制都在被 git 忽略的
`var/texpresso/`，系统库由 Homebrew 提供。首次安装或升级：

```sh
make texpresso-install
```

这个 target 会确认 `mupdf` 和 `sdl2`（Homebrew 当前可能以 `sdl2-compat` 提供）存在，随后 clone
或 fast-forward 更新 [let-def/texpresso](https://github.com/let-def/texpresso)，最后在本地 checkout
运行 `make all`。只重建和 headless smoke test 分别使用：

```sh
make texpresso-build
make texpresso-test
```

`make clean-state` 会连同其他 runtime state 删除整个 `var/`，因此执行后需要重新运行
`make texpresso-install`。配置在二进制缺失时不会让 Emacs 启动失败，调用预览命令时会给出上述恢复入口。

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
- 编译目标目录为 `lisp/` 与 `site-lisp/config/`（config 注册表是每次启动都要加载的基础设施，纳入 `my/byte-compile-config` / `my/native-compile-config`）
- 启动时会删除本配置自有的 `lisp/` 和 `site-lisp/` 下 `.elc`，避免坏字节码或较新的错误 `.elc` 压过源码；`elpa/` 包字节码不受影响
- 因为 `.elc` 每次启动都会被删，config 的启动加速来自 **native compile** 的 `.eln`（持久缓存在 `var/eln-cache/`，加载源码时自动采用）；byte compile 仅用于诊断 warning
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

- `ps` 里的 Emacs CPU、内存、RSS、子进程（原始 `ps` 行，只到直接子进程）。
- **Toolchain Memory**：Emacs 整个后代进程树（递归，不止直接子进程）的 RSS
  汇总——包含 Noema web-host 自己再 spawn 出来的 Copilot LSP、Jupyter
  kernel 这类"孙进程"，用缩进树 + 合计展示，不用再手动拼 `ps`/`pgrep` 审计
  工具链内存。结构化数据对应 `(my/performance-snapshot)` 里的
  `:children-rss-kb`/`:children-count`（只加进结构化 sample，没有写入 TSV
  记录，避免改动已有时序数据的列结构）。
- Emacs runtime：buffer/process/timer/GC/read-process-output-max/memory-use-counts。
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

默认低功耗取向：

- macOS 默认使用普通滚轮/触控板滚动，不常驻 `pixel-scroll-precision-mode`；
  需要更顺滑手感时可以运行 `M-x my/macos-toggle-pixel-scroll-precision` 临时打开。
- 原生 GUI scrollbar 保持关闭；`scrollview` 只在编辑/阅读 buffer 绘制右 fringe
  全文概览。大 buffer 自动停用标记收集，但保留低成本滚动块。
- macOS 的显式 idle GC 只在焦点离开后作为补充清理，默认不再每次退出 minibuffer 后排一个 GC；
  普通活跃/空闲 GC 交给 `gcmh`。
- 行号不再全局绘制，只在代码/配置类 buffer 自动打开；普通文本、帮助、终端、侧边栏可手动用
  `SPC w l` / `M-x my/toggle-line-numbers` 打开。
- ligature 不再全局启用，只在代码/文本/配置/Org/Markdown 等编辑 buffer 本地启用。
- TODO 高亮不再全局启用，只在编辑 buffer 本地启用。
- `whitespace-mode` 默认只检查本地的代码/配置文件，跳过文本、大文件、远程文件和 `so-long` buffer。
- auto-revert 优先使用文件通知，普通 buffer 不做高频轮询；PDF buffer 单独保留较快刷新。
- `amx` 不保留重复 idle 更新 timer；命令索引在交互入口按需刷新。
- `direnv` 不挂 `post-command-hook`；打开文件、切换 buffer/window、加载 dir-locals
  时异步刷新，避免远程 Nix 环境阻塞文件访问；只有 compile/task/LSP 等即将启动
  target 进程的显式边界才等待环境就绪。同一 envrc root 的自动触发合并为一次
  enter 报告；选中 buffer 离开 envrc 树时恢复该 buffer 的基础环境并报告 leave。
  环境仍然是 buffer-local capsule，不会因为切换窗口而破坏其他项目 buffer。
- tab line 的 buffer-list 高频缓存失效走短 idle 合并。
- Treemacs 文件/符号跟随只在 Treemacs 窗口可见时安装按键级 hook；隐藏后自动卸载并取消等待中的
  idle timer。
- symbol-overlay 的编辑刷新 hook 只在当前 buffer 开启模式或存在高亮时安装，不再全局空跑。
- Org 的 rich UI 保留完整能力，但 pretty block/LaTeX 这类重渲染只面向当前可见窗口；buffer 隐藏时取消
  等待中的可视区 timer，不做退回全 buffer 的后台刷新。
- LSP/Flymake/Copilot/company/Treemacs/Org Roam 的自动后台工作都做了短延迟合并，避免每次按键后立刻唤醒一串任务。
  Copilot、Company、Flymake 默认等待更明确的输入停顿后再启动。
- diagnostics mode line 使用缓存，只在 buffer 变化或 Flymake 发布新诊断后重新统计。

记录保存在 [var/performance/](../var/performance/)：

- `s` 会把当前采样追加到当天的 `performance-YYYYMMDD.tsv`。
- `R` 会切换录制模式，录制开启后每次刷新都会追加一条。
- `o` 打开记录目录。

按键：

- `g` 立即刷新。
- `G` 手动执行一次 `garbage-collect`，并报告 RSS 前后变化。
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

### 清 Org 图片、附件和 LaTeX preview cache

入口：

- `M-x my/org-maintenance-board`
- macOS GUI 下 `H-o b`

这个 board 只在打开或手动刷新时扫描。扫描范围和 agenda 一致：

- 在 `~/HC/Org/` 里打开时扫整个 Org 库
- 在其他项目里打开时扫当前项目
- 没有项目时扫当前目录

媒体清理规则：

- clean media：删除未引用图片和未引用附件
- clean images：删除未引用图片；图片只用同目录/子目录里的 Org 链接判断引用，不做全库交叉引用
- clean attachments：删除 `attachments/`、`attach/`、`files/` 下未引用附件；附件会看整个当前项目/scope 的 Org 链接
- prune latex：每个 `ltximg/` 目录只保留最新
  `my/org-maintenance-latex-cache-max-files-per-dir` 个 preview cache 文件
- clear latex：删除当前 scope 下所有 `ltximg/` preview cache 文件
- empty dirs：删除清理后留下的空 `img/`、`images/`、attachment、`ltximg/` 目录
- clean all：清未引用媒体、按阈值裁剪 LaTeX cache，再删空目录

防护规则：

- `public/`、`publish/`、`dist/`、`build/`、`css/`、`js/`、`CV/` 不参与媒体清理
- `ltximg/` 和 `org-latex-preview-cache/` 不参与图片/附件清理，只走 LaTeX cache 工具
- 文件名带 `keep-` 前缀时豁免清理
- `H-o k` 可以把当前 Org `file:` 链接指向的实际文件加上 `keep-` 前缀，并同步更新链接
- `H-o m` 可以重命名当前 Org `file:` 链接指向的实际文件，并同步更新链接
- `H-o O` 用系统 `open` 打开光标处的 Org link、图片链接或文件名

board 会缓存同一 scope 的扫描结果；只要 Org、媒体和 cache 文件签名没变化，后续按钮
动作会复用解析结果，避免重复 parse 所有 Org 链接。

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
- [lisp/telescope/](../lisp/telescope/)
  `telescope` 统一入口，以及 diagnostics picker 这类 Telescope 风格的选择器
- [lisp/init-lsp.el](../lisp/init-lsp.el)
  company/eglot/lsp-mode/flymake/dape
- [lisp/init-lsp-tools.el](../lisp/init-lsp-tools.el)
  language server hub / doctor / dispatch / runtime knobs
- [lisp/org/init-org.el](../lisp/org/init-org.el)
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

### Lean / Noema 孤儿进程占内存

Lean 4 LSP 是 watchdog 架构：eglot 管理的是 `lean --server`/`lake serve`
(或经 `lean-proxy.mjs` 包装的版本)，它为每个打开的 `.cell/*.lean` 文件 fork
一个 `lean --worker` 子进程，真正的 elaboration 堆（常常几 GB）在 worker 里。
如果 watchdog 被硬杀（Emacs 崩溃、`kill -9`），worker 会被 reparent 到 PID 1
并常驻。

- 正常路径：`kill-emacs-hook` 里的 `my/eglot-shutdown-all-on-exit-h`
  （`lisp/init-lsp.el`）会在退出时干净关闭所有 eglot 服务器；
  `lean-proxy.mjs` 收到 SIGTERM/SIGINT 时会用 `killDownstream()` 杀整个下游
  进程组，而不只是直接子进程。
- 自愈路径：`my/lean-sweep-orphan-workers`（`lisp/lang/lean/init-lean.el`，
  模块加载时空闲 5 秒后自动跑一次，也可以手动 `M-x` 调用）只会杀
  **ppid=1 且命令行匹配 `.cell/` 路径**的 `lean --worker`，不会碰任何还有活
  watchdog 父进程的 worker。
- 排查：`pgrep -fl 'lean --worker'` 看 ppid 是不是 1；`ps -p <pid> -o rss`
  看占了多少内存。

Noema 的 Jupyter kernel 同理：`server/jupyter/kernel-process.mjs` 用
`detached: true` 启动 kernel，一旦孵化它的 node 进程（web-host 或某个临时
诊断 harness）整体退出而没有干净调用 `shutdown()`，kernel 会被 reparent 到
PID 1。`server/jupyter/kernel-registry.mjs` 的 `sweepOrphanKernels` 只能靠
per-runtimeDir 的 `aaronnote-owned.json` sidecar 回收**同一个 runtimeDir 的
上一轮**孤儿；如果调用方每次都 `mkdtemp` 一个新的临时 runtimeDir（例如一次性
诊断脚本），sidecar 永远追不上。`web-host.mjs` 启动时额外跑一次
`sweepGlobalOrphanKernels`：只要是 ppid=1 且命令行匹配
`ipykernel_launcher ... aaronnote-kernel-*.json` 的进程，不管来自哪个
runtimeDir 一律清理（这个命名模式只有本模块会用到，正常存活的 kernel
ppid 是它的 node 宿主进程，不会命中）。

**写临时诊断脚本时的规则**：不要给 `createJupyterCellService`/
`createKernelRegistry` 传一次性 `mkdtemp` 出来的 runtimeDir 然后直接退出；
要么复用稳定的 vault runtimeDir，要么显式 `await service.shutdown()`
之后再退出。

### 稳态内存调参

孤儿进程之外，长期运行的稳态 RSS 主要由三个参数决定，都走 config
registry(`etc/config-store.el`，改值用 `config-set` 或直接编辑该文件,
不要散落 `setq`):

| 键 | 位置 | 默认 | 作用 |
|---|---|---|---|
| `my/gcmh-high-cons-threshold` | `lisp/init-tools.el` (gcmh) | 128 MB | Emacs 空闲 GC 前允许堆积的垃圾上限；LSP 活跃时会被 `lisp/init-lsp.el` 的 `my/language-server-performance-gcmh-factor`(默认 2) 临时翻倍。之前是 512 MB(LSP 活跃时 1 GB),下调后 GC 更勤但单次更短,交互延迟影响很小。 |
| `my/copilot-server-max-heap-mb` | `lisp/init-copilot.el` | 1024 | Emacs 自己 spawn 的 Copilot language server 的 V8 堆上限,通过 `NODE_OPTIONS` 注入(该 LS 不是走命令行参数,`copilot-server-args` 只能传给二进制本身,包不去掉支持自定义 env,所以用 `copilot--make-connection` 的 advice)。同一个键的值会通过 `AARONNOTE_COPILOT_MAX_HEAP_MB` 环境变量传给 Noema 侧自己 spawn 的第二份 Copilot LSP 实例(`server/lib/runtime.mjs` 的 `CopilotLspClient`),两边共享一个上限。**实测 384/512 MB 都会让该 LS 在启动几秒内以 `SIGABRT`(V8 `Ineffective mark-compacts near heap limit` OOM)崩溃退出——它自身(sharp 原生模块 + tf-idf/diff worker 的宿主进程)启动期就需要 ~700 MB old-space,768 MB 是实测最低可用值;1024 MB 留出安全余量,仍比不设上限时的 4 GB 默认值收紧很多。改这个键前先用 `node --max-old-space-size=<N> dist/language-server.js --stdio` 走一遍 initialize 握手再空闲等 20s 确认不 OOM,不要凭感觉调小。** |
| `my/noema-web-host-max-heap-mb` | `lisp/roam/init-aaronnote.el` | 512 | Noema `web-host.mjs` 自身的 V8 堆上限,**必须用 `--max-old-space-size` 命令行 flag 而不是 `NODE_OPTIONS` 环境变量**——web-host 的 `process.env` 会原样传给它 shell 出去的 codex/claude/opencode CLI(LaTeX export 用),用环境变量会把堆上限错误地传染给这些 node 程序。 |
| `my/noema-latex-export-agent-idle-timeout` | `lisp/roam/init-aaronnote.el` | 180 | LaTeX export agent 无输出多久后做一次存活检查；活着就继续等，不会 kill。 |
| `my/noema-latex-export-agent-hard-timeout` | `lisp/roam/init-aaronnote.el` | 900 | 单次 agent attempt 的绝对上限；先 SIGTERM，默认留十秒收尾后才 SIGKILL。 |

Noema 侧的 Copilot LSP 实例(`CopilotLspClient`)另外做了**空闲 TTL 自动
停止**:`AARONNOTE_COPILOT_IDLE_TTL_MS`(默认 15 分钟,0 关闭)内如果没有
真实的补全请求(inline/shown/accept,不含单纯的 status 轮询)、且没有
pending 请求,就会自动 `stop()`;下次任何请求经 `ensureReady()` 自动重启。
用一次 web 端 copilot 不会永久多驻留一个 ~400 MB 的进程。

排查:`(my/performance-report-string)` 的 Toolchain Memory 区块能直接看到
两份 Copilot LSP(如果都在跑)和它们各自的 RSS。

## 8. 推荐维护节奏

每隔一段时间做一次：

1. 更新依赖
2. 跑一次 batch 启动检查
3. 清理不用的键位和旧包
4. 同步更新 [docs/](.)

如果你继续大改配置，记得同时更新 [docs/README.md](README.md) 里的索引。
