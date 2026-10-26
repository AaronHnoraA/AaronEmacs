# Dev, LSP, Remote Guide

这份文档覆盖：

- 补全和 LSP
- 调试
- 项目导航
- 终端与远程
- 浏览器与外部集成

## 0. 核心开发约束：Remote-first

Remote 框架不是“远程功能模块”，而是这套配置中文件与开发工具的核心执行模型。
任何涉及 filesystem、project/workspace、进程、可执行文件、环境、watch、terminal、
service、socket 或 port 的新设计，即使第一版只在本机使用，也必须先用
`target + pipeline + backend + workspace` 表达。本机是 target `local`，不是一条
由 consumer 自己维护的快捷分支。

开发时遵守以下边界：

- consumer 只表达“在哪个 target/workspace 做什么”，不判断 `"local"`、
  `file-remote-p`、TRAMP method 或 backend ID 来选择功能；
- 物理路径、spawn 形式、连接与转发差异留在 backend/transport/client boundary；
- 普通本地 buffer 继续享受 Emacs 原生文件名和 package 兼容性；进入
  project/workspace/LSP 框架边界后，本地与远程身份统一为 `/fs:TARGET:/path`；
- 框架 API 不够时，先补通用 `remote-*` 契约及 native/remote 实现，不在语言或
  package 模块里补一段一次性的远程代码；
- local、remote 和断线/取消/恢复测试共同决定能力是否完成，验收见
  [remote-parity.md](../acceptance/remote-parity.md)。

## 1. 补全栈

当前是双栈并存：

- 现代 completion：
  - `vertico`
  - `orderless`
  - `consult`
  - `embark`
  - `marginalia`
- 兼容保留：
  - `ivy`
  - `counsel`

### 你会实际感受到的行为

- `M-x` 走原生 `execute-extended-command`，配 `amx` 历史排序
- `C-x C-f` 走 `find-file` + `vertico-directory`
- `C-x b` 走 `consult-buffer`
- `C-s` 走 `consult-line`
- `C-x C-r` 走 `consult-recent-file`
- `C-x g` 打开 `magit-status`
- `M-o` 在 minibuffer 里走 `embark-act`

## 2. LSP 与补全

### Remote-first 强制不变量

LSP 对路径和进程身份最敏感，因此比普通 consumer 更严格：

- 一个 lsp-mode workspace 只能有一个 owning logical root；
  文档 URI、server cwd、executable、PATH/toolchain、watcher、helper service 和
  channel 都从这个 root 的 target context 推导；
- URI 回调必须使用拥有该 server/workspace 的 root，不能因为回调恰好运行在另一个
  buffer，就从当前 `default-directory` 猜 target；
- server 默认 placement 是 `target`。只有 xwidget、浏览器、前端 proxy 等确实必须
  接触本机 UI 的 helper 才能显式标为 `client`，它连接 target peer 时仍使用
  backend 提供的 stdio/channel bridge；
- 语言模块不能用 `file-remote-p` 选择另一套 server、PATH、参数或功能降级。
  lsp-mode 自身要求的兼容修饰只能集中在共享
  `language-server` adapter 边界；
- `local` target 必须走同一注册、路由、环境和 URI 投影流程。新增 LSP 接入至少要
  有 local/remote root 与 URI 测试；宣称可重连前，还要覆盖 watcher/helper/channel
  的故障恢复。

完整维护契约见 [lsp-workflow.md](../design/lsp-workflow.md)。现有语言模块中残留的
local/remote 特判属于待迁移债务，不是新增接入可复制的模式。

### 默认组件

- `company`
- `company-prescient`
- `company-box`
- `lsp-mode`
- `flymake`
- `flymake-diagnostic-at-point`
- `lsp-ui`
- `breadcrumb`
- `Treemacs`

### 常用命令

- macOS GUI 下也可以直接用一层 `Option(H-)` 降深度：
  `H-e` code menu，`H-d` diagnostics menu，`H-x` / `H-t` `telescope`，`H-i` `show-imenu`，`H-u` language server 菜单，`H-j` `dape`，`H-n` 最近测试
- `SPC c a`
  code action
- `SPC c f`
  format buffer
- `SPC c r`
  rename
- `SPC c i`
  打开 `show-imenu`
  左侧 `Treemacs` smart toggle，并跟随当前文件和光标所在 symbol；Outline
  使用浅层缩进、文件归属标题和 SymbolKind 图标
- `SPC c I`
  文档浮窗
- `SPC c t`
  测试菜单
- `SPC c n`
  当前附近测试
- `SPC c N`
  当前文件测试
- `SPC c p`
  当前项目测试
- `SPC c T`
  重跑上次测试
- `SPC c L`
  切换 CodeLens（默认开启，只渲染可见区域及上下缓冲）
- `SPC c s`
  语言服务器菜单：Hub / Doctor / 调参 / log / session / config
- `SPC c o`
  organize imports
- `SPC c R`
  restart language server

### 跳转

- `C-h e`
  定义
- `C-h r`
  引用
- `C-h i`
  实现
- `C-h t`
  类型定义

### 诊断

- `M-n`
  下一个诊断
- `M-p`
  上一个诊断
- `C-c !`
  显示当前 buffer 诊断

### Hub / Doctor

现在语言服务器栈补了一层可视化维护面：

- `M-x +language-server/manager`
  打开 `*Language Server Hub*`
- `M-x +language-server/doctor`
  打开 `*Language Server Doctor*`

Hub 里可以直接看：

- 当前 buffer 的 route policy / active backend / workspace config
- 注册的 lsp-mode client/feature 路由
- 一组 session 级调参入口

Doctor 更适合快速排查：

- 关键 library 是否存在
- 本地 server executable 是否存在
- 当前 buffer 命中了哪条路由

详细模型看 [lsp-workflow.md](../design/lsp-workflow.md)。

### Lean (lsp-mode + xwidget infoview)

Lean 4 走自定义 `lean-mode`，并统一使用 lsp-mode。模块分层如下：

| 模块 | 职责 |
|------|------|
| `lisp/lang/lean/config.el` | 主 mode、lsp-mode client 注册、project 定位、ripgrep 符号搜索、UI 总入口 |
| `lisp/lang/lean/lsp.el` | `textDocument/publishDiagnostics`、`$/lean/fileProgress`、`lean/restartFile` 通知处理，fringe/sideline 进度与 Flymake 后端 |
| `lisp/lang/lean/jump.el` | 基于 `$/lean/plainGoal` 的 goal 变化跳转 |
| `lisp/lang/lean/infoview.el` | 官方 xwidget infoview 桥接（`C-c C-i`）|
| `lisp/lang/lean/lean4-infoview-bridge/` | `lean-proxy.mjs`：既是 lsp-mode 的 transport，也用 HTTP+SSE 服务官方 React infoview |

Lean 的通知/请求 payload 一律用 `lsp-get` 读取：lsp-mode 只有在以
`LSP_USE_PLISTS` 编译时才给 plist，本仓库的构建给的是 hash table，用
`plist-get` 会静默取到 nil（诊断、进度、goal 跳转全部失效）。

**键位（lean-mode buffer）**

| 键 | 命令 |
|----|------|
| `C-c C-i` | 切换官方 xwidget infoview |
| `C-c i r` | 重启 infoview（连带 proxy 与 `lake serve`）|
| `C-c C-r` | 重启 lsp-mode workspace |
| `C-c C-d` | 重刷文件依赖 |
| `C-c C-a` | lsp-mode code actions |
| `C-c C-l` | 打开 Lean dev log |
| `C-c C-k` | 查 unicode 输入法键位 |

**Xwidget infoview 架构**

1. lsp-mode 启动的"语言服务器"就是 `lean-proxy.mjs`（client placement，跑在本
   机，因为它要渲染本机的 xwidget）。proxy 的 downstream 才是 workspace target
   上的 `lake serve`，由 `remote-local-bridge-command` 投射过去。proxy 对 JSON-RPC
   是透明转发，只是顺手 tap 下诊断/进度并缓存 `initialize` 结果。
2. proxy 在本机随机端口上用 HTTP+SSE 服务官方 `@leanprover/infoview`，并把端口
   注册到 Emacs gateway；`lean--iv-wait-for-port` 等到 peer 出现后打开
   xwidget-webkit（`http://127.0.0.1:<N>/`）。远程 target 的端口经
   `remote-port-forward` 暴露到本机。
3. **Emacs→infoview**：只同步光标。`lean-iv-sync-cursor-h` 在 `post-command-hook`
   里 POST `/cursor`；文档同步完全由 lsp-mode 经 proxy 完成。
4. **Infoview→Emacs 反向通道**：走标准 LSP server→client 消息，由 proxy 注入：
   - `window/showDocument` → lsp-mode 原生处理（点击 goal 定位）
   - `workspace/applyEdit` → lsp-mode 原生处理（"Try this" 建议、code actions）
   - `lean/restartFile` → 自定义通知，`lean-handle-restart-file` 调用
     `lean-refresh-file-dependencies`
5. **生命周期**：proxy 与 `lake serve` 同生共死——lake 意外退出时 proxy 也退出，
   否则 stdio 还活着会让 lsp-mode 以为 workspace 健康，而每个请求都永远挂起。
   `lsp-restart` 为 `auto-restart`，于是新的 proxy+lake 会被拉起（新端口），
   `lean-iv-server-restarted-h` 把已打开的 infoview 重新指向新端口。页面加载时
   若 proxy 报 `running:false`，infoview 直接显示服务器已退出，而不是一直停在
   "Waiting for Lean server to start"。

**补全**：lean buffer 使用全局 capf + corfu + nerd-icons 补全，不启用 company-mode。

远程 `/fs:` buffer 仍在本机显示 xwidget；target 上的 Lean bridge 端口通过
`remote-port-forward` 暴露到本机。若 target、Node/Lake 或 forward 不可用，
`C-c C-i` 会保留逻辑 project identity 并在 Lean dev log 中报告失败。

## 3. 调试

这套配置使用 `dape`。

入口：

- `SPC c j` / `H-j` / `F7`
  调试菜单：启动 Dape、选择项目 profile、步进、断点、REPL、locals/watch、adapter doctor
- `SPC c J`
  直接进入 `M-x dape`
- `M-x dape`

项目调试 profile 可用 `M-x debug-profile-menu`、`SPC r d` 或 `H-D` 选择；
`SPC r D` 重跑当前 frame、当前项目最近一次已提交给 Dape 的 profile。切换 frame
或项目不会误用另一处的配置；启动同步报错不会覆盖上次记录。正在运行的会话仍用
Dape 原生 `dape-restart` 重启。

行号/margin 区域可以点击切换断点；左 fringe 保留给折叠图标，调试断点可用
`S-mouse-1` 在 fringe 上切换。调试期间还有 `hydra-dape-mode`。

默认配置覆盖常见 DAP 工作流：

- Python: `debugpy` / module
- JavaScript、TypeScript、Chrome、Node attach: `js-debug`
- Java: `jdtls` + `vscode-java-debug`
- C、C++、Rust: `lldb-dap` / `lldb-vscode` / `gdb` / `cpptools`
- Go: `dlv` / test binary
- Shell、.NET、PHP、Ruby、OCaml: 对应 adapter

`M-x +debug/adapter-doctor` 可以查看 adapter 可用性和缺失安装项。

## 4. 编译与测试

- `SPC c .`
  code menu；`b` 选 build，`B` 重跑上次 build。会优先识别常见项目里的 `make`、`cmake`、`ninja`
- `SPC c c`
  `compile`
- `SPC c C`
  `recompile`
- `SPC c k`
  停止当前编译
- `SPC c l`
  切到编译 / 测试输出
- `SPC c t`
  测试菜单
- `SPC c n`
  当前附近测试
- `SPC c N`
  当前文件测试
- `SPC c p`
  当前项目测试
- `SPC c T`
  重跑上次测试

## 5. 项目导航

### 项目工作流

现在项目层不再只是单独用 `Projectile` 或 `Treemacs`，而是：

- `Projectile`
  管已知项目、项目文件、项目搜索
- `Perspective`
  按项目切工作区
- `Treemacs`
  做左侧文件树和符号导航
- `Transient`
  提供项目工作台

### 最常用命令

- `SPC p .`
  项目工作台
- `SPC p p`
  切项目
- `SPC p o`
  打开项目工作台式入口
- `SPC p f`
  当前项目找文件
- `SPC p s`
  当前项目全文搜索
- `SPC p d`
  项目根目录
- `SPC p m`
  当前项目 Magit
- `SPC p v`
  当前项目 portal
- `C-c p .`
  非 Evil 场景下打开项目工作台

### 添加 / 发现项目

- `SPC p a`
  手动添加一个项目根目录
- `SPC p D`
  按目录批量扫描项目

如果你有固定项目根，去看 [project-guide.md](project-guide.md) 里的 `+project-search-paths`。

### Treemacs

- `C-c t`
  打开 Treemacs
- `M-0`
  焦点跳到 Treemacs

当前 Treemacs：

- 左侧 side window
- `show-imenu` 也是用它做 smart toggle
- 从树里打开文件/符号时默认进入最近使用的编辑窗口
- 开启 filewatch
- 开启 follow mode
- 开启 project-follow mode
- 开启当前 symbol 跟随
- 开启 deferred git mode

### Dirvish

- `C-c o d`
  `dirvish-dwim`
- `C-c o f`
  `dirvish-fd`

## 6. 终端

### Shell

- `M-x shell-toggle`
  弹出 shell

### Portal

- `M-\``
  `+portal-popup/toggle`
- `C-c e`
  弹出/收起全局 popup；新建终端时按最近聚焦的项目和 target 启动
- `C-c C-e`
  智能弹出或收回当前 popup `portal`
- `C-c E`
  切换到下一个 popup `portal`；`C-u C-c E` 新建一个
- `C-c M-E`
  新建 popup `portal`
- `C-c M-e`
  切换当前 popup `portal` 的固定状态
- `SPC o e`
  `+portal-popup/toggle`
- `SPC o E`
  切换到下一个 popup `portal`
- `SPC o F`
  切换当前 popup `portal` 的固定状态
- `SPC o t`
  `+portal-popup/toggle`
- `SPC o v`
  新建 `portal`
- `SPC o V`
  创建命名 `portal`
- `SPC o S`
  `+portal/ssh`
- `M-x +project/popup-portal-app`
  在当前 project 根目录的新 popup `portal` 里运行 `lazygit` / `btop` / `yazi` / `tmux`

### Portal

- `SPC o P` / `M-x +portal/terminal`
  在当前 Emacs 窗口打开 GhosttyKit 原生终端。需要 macOS GUI Emacs 31.1+。
  本地目录使用 Portal；远端 workspace 继续使用 Portal。
- 普通 Portal 终端顶部使用独立标签组；全局工作区栏不占用顶行。
  终端标签不混入普通文件或 popup 标签：当前终端显示，其他已在任意分屏或 frame
  显示的终端隐藏，终端不再显示时恢复为标签；点击切换，右键重命名、关闭、移动位置或新建。
  `M-t` / `Cmd-t` 在任何 buffer 打开终端选择器，当前终端不出现在自己的候选列表里。
  `Enter` 在当前窗口、`Shift-Enter` 在右侧、`Cmd-Enter` 在下方、`Cmd+Shift+Enter`
  在新 frame 打开；已有终端会从旧窗口移出。普通 `Enter` 选择 popup 终端则切换对应弹窗。
- Portal 锁定在 `package-lock.el` 的 VC revision，源码和已经验证的构建存放在
  `site-lisp/portal/`（不纳入本仓库）。首次在新机器安装时依 Portal 提示构建原生模块。

## 6.1 数据库 / SQL

- `M-x clutch-query-console`
  打开 `clutch` 的连接选择器和查询控制台
- `SPC o q`
  同上

这套配置把数据库客户端接到了 `clutch`，但保持懒加载：

- 包声明在 [lisp/tools/clutch/config.el](../../lisp/tools/clutch/config.el)
- 第一次打开控制台时，才会安装/加载 `clutch`
- 连接配置单独放在本地 `etc/clutch-config.el`

连接建议：

- `clutch-connection-alist` 放在 `etc/clutch-config.el`
- 密码优先走 `auth-source` / `pass`
- 不默认接管 `.sql` 文件，避免打断现有 SQL 编辑流；需要时手动 `M-x clutch-mode`

## 6. Remote 框架、TRAMP 与 tramp-rpc

### 设计目标

- Remote 是所有可能涉及远端的开发能力的核心抽象，而不是 SSH 专用旁路
- `local` target 与其他 target 走相同的 consumer API、资源所有权和验收流程
- buffer 使用与 transport/backend 无关的 `/fs:TARGET:/path` 逻辑身份
- target、有序 transport pipeline、执行 backend、复用 session 和调用者偏好分别注册
- 普通 Emacs 文件 API 保留 TRAMP 的完整兼容性；默认 adapter 偏好仍是
  native → tramp → tramp-rpc，单个 target 可以在 `etc/remote.json` 用
  `preferences` 把文件操作也切到 tramp-rpc（当前 `Aaron-*` 就是这样配置的）
- lsp-mode、direnv、环境探测与自定义进程可以优先 tramp-rpc，失败时回退 TRAMP
- 需要"每个文件一次子进程"的功能用 `remote-file-operation-cost` 判断开关，
  VC/diff-hl 因此在 batched backend 上与本地一致，在 shell TRAMP 上仍然关闭
- 每个 buffer 挂文件通知的功能用 `remote-file-watch-cost` 判断；`push` 路由
  （本机、tramp-rpc）上的远程 buffer 因此像本地一样随外部修改自动刷新
- socket/stream/port-forward 走显式 channel API，远端不支持时不会静默落到本机
- PATH 按 target/workspace ID 隔离，并由 host probe、direnv、toolchain 等分层维护

### 你应该怎么用

- `M-x remote-board`：按 target 查看 route、连接状态、配置/当前/最近文件夹和活动转发端口；可见时随连接、workspace 和 channel 生命周期刷新。`RET`/`o` 打开文件夹并建立受管理的 workspace（端口行复制本地地址），`f` 在目标文件系统中补全目录，`c` 关闭选中文件夹的 workspace 与所属资源，`C` 断开整个 target 的 workspace、连接和 buffer（保留未保存编辑及拒绝关闭的 buffer，并停止自动启动），`p` 转发目标端口（`C-u p` 指定目标侧 host，`C-u C-u p` 还可指定本地端口），`n` 命名选中转发，`P` 更换其本地端口，`k` 关闭转发并移除恢复资源，`r` 后台重连 workspace（已连接时用 `C-u r`），`w` 复制行的 URI 或本地端口地址，`s` 刷新本机缓存状态，`g` 重读 target 配置
- 编辑远程文件：使用 `fs://target/path` 或 `/fs:target:/path`
- 旧的 `/ssh:host:/path` 仍可打开，进入 buffer 后会 canonicalize 为 `/fs`
- 在当前本地/远端 workspace 打开终端：`C-c e`（`+portal-popup/toggle`）
- 按主机名直接开终端：`M-x +portal/ssh`

`/fs:` 远程 buffer 的模式栏显示当前 target，点击可打开 Remote 面板。普通本地
buffer 不显示这一项。面板中的最近文件夹由显式的 Remote 打开操作记忆，并由
`savehist` 持久化；绘制面板不连接远端，只有实际打开文件夹时才检查目标目录。

`C-c e` 是推荐入口。它读取当前 buffer 的 `/fs:TARGET:/path` 上下文，通过
remote process adapter 启动 portal，并把 terminal 生命周期登记给 workspace；
本地与远端走同一条路径。popup 的 tab 和当前窗口在所有项目及 target 间共享；
新建 terminal 或 agent 时读取最近聚焦的工作 buffer，在该项目和 target 启动进程。
切换旧 tab 只显示原有 buffer，不改变其进程所属的 workspace。

`+portal/ssh` 会优先读：

- `~/.ssh/config`

所以最好把常用机器写成 Host 别名。

本地文件同样属于 `local` target。自定义进程不要自行拼 `/ssh:` 或 `/rpc:`：
使用 `remote-make-process`、`remote-process-file`、`remote-exec` 和
`remote-executable-find`。没有 file-name 参数的网络操作使用
`remote-make-network-process`、`remote-open-network-stream` 与
`remote-port-forward`。完整对象模型、能力矩阵、配置 schema、当前缺口与接入清单
见 [remote-framework.md](../design/remote-framework.md)。

开发新的 project/LSP/search/SCM/debug/task 集成时，所属模块可以保留业务策略，
但 path projection、process placement、environment、session、watch、service 和
channel lifecycle 必须委托给 Remote 框架。若必须写 local/remote 条件分支，先把它
视为框架缺少能力的信号，而不是正常的 consumer 实现方式。

## 7. 浏览器

### 浏览器入口

- `eww`
- `xwidget-webkit`

### 常用命令

- `C-c w e`
  `aaron/open-eww-url`
- `C-c w x`
  `aaron-open-xwidget-url`
- `C-c w s`
  交互选择当前页面的目标后端
- `C-c w E` / `C-c w X` / `C-c w O`
  当前页面直达 `eww` / `xwidget-webkit` / macOS `open`
- `C-c w d`
  kill 当前 `eww` / `xwidget-webkit` buffer 并删除对应窗口；browser buffer 的 `M-w` 只关视图
- `C-c w w`
  统一 `browse-url`

### 当前逻辑

默认打开方式集中在 `lisp/core/aaron-open.el` 的 `aaron-open-routes`，通用 DSL helper
在 vendored `site-lisp/general.el/general.el` 的 `general-route-*`。`browse-url`
默认走 `menu`，菜单默认项是 `xwidget`，不做 URL 复杂度自动分流。分流保持手动：

browser pipeline 额外提供：

- EWW <-> Xwidget
- EWW/Xwidget 默认打开到独立浏览 buffer，连续多开不覆盖已显示的浏览 buffer
- 当前页面 -> macOS `open`
- 统一搜索入口：`+browser/open-search`
- 当前页面后端切换：`+browser/switch-to`

因此当前建议是：

- 文本/阅读优先 `eww`
- 默认真实网页优先 `xwidget-webkit`
- 文件、URL 需要系统应用接管时走 `system` / macOS `open`

**xwidget 焦点**：页面加载完成（`load-finished`）时自动开启 `xwidget-webkit-edit-mode`、
注入一次 `<right>` 唤醒 native WebKit first-responder、并运行 buffer-local `+xwidget-focus-script`
（如 Noema 的 CodeMirror 聚焦 JS）。打开后无需手动先按键即可直接输入。
相关开关：`+xwidget-auto-focus-on-load`、`+xwidget-prime-native-focus`、`+xwidget-prime-key`。
手动恢复焦点：`M-x +xwidget/focus`（xwidget buffer 内绑定到 `i`）。
Noema 页面在暂停状态下收到真实点击时，会向 Emacs 报告页面已取得输入焦点。
Emacs 只用这个事件更新该 pane 的前台活动状态，不重新选择窗口，以免 macOS
把 WebKit 刚取得的 first-responder 夺走。下一次 Emacs 命令或窗口切换会清除
这个临时活动状态；平时没有额外轮询。

## 8. Noema Markdown Web/xwidget

### 架构

Markdown 笔记现在由 Noema Web 自己编辑和渲染，Emacs 只负责启动、
打开、粗粒度命令和文件定位：

```
Emacs (noema/bridge/config.el)
  └─ spawn node site-lisp/noema/web-host.mjs
       ├─ 静态服务 site-lisp/noema/dist/pages/
       ├─ 运行时 site-lisp/noema/server/ + shared/
       ├─ 注入 window.noemaApi 适配器（替代 Electron preload）
       ├─ POST /api            ← Web app 调 runtime 保存/索引/文件操作
       ├─ GET  /events         ← Emacs 控制事件推送到页面
       └─ POST /emacs/command  ← Emacs 粗粒度控制通道
  └─ 当前 window 直接切到 xwidget，加载 http://127.0.0.1:<port>/?file=<md>
```

笔记仓库通过 `.roam` 符号链接挂载（机器本地，不入 git）。Noema 是
`site-lisp/noema/` 下的完整项目，源码、build、runtime、plugins 和
roam-tools JS 都在其中；运行时不依赖 `~/HC/Org`。该目录是**独立仓库**，由
AaronEmacs 根仓库的 `.gitignore` 排除，不是 submodule；新机器应将 Noema remote
clone 到该路径，再按 Noema 的 README 安装依赖。

Noema 与 Emacs 共用：

- 状态与缓存：`var/noema/`
- snippets：`snippets/`（`markdown-mode/`、`tex-mode/` 已直接并入 AaronEmacs；Noema 资源路径指向这份唯一副本）
- templates：`templates/`（只有 `noema/`、`latex/`、`tex/` 链接到 Noema，其它模板由 Emacs 仓库维护）

### 关键文件

| 文件 | 用途 |
|------|------|
| `lisp/noema/bridge/config.el` | Emacs 入口：进程管理、xwidget 打开、控制命令 |
| `site-lisp/noema/` | 完整 Noema 独立 git 项目；安装位置固定，但由根仓库忽略，不是 submodule 或符号链接 |
| `site-lisp/noema/web-host.mjs` | Node HTTP+SSE 桥接服务器和 preload adapter |
| `site-lisp/noema/dist/pages/` | Noema static build |
| `.roam` | → Markdown 笔记目录（`NOEMA_ROOT`） |

### 边界

- Noema Web 拥有 CodeMirror 文档状态、输入手感、保存、文件树、graph。
- Emacs 打开 `.md` / `.markdown` / `README.md` 时直接交给 Noema，并关闭临时 Markdown buffer。
- Emacs 不再通过 `after-change-functions` 推送 buffer 全文，也不做 per-keystroke preview。
- Emacs 侧命令只做打开当前 note、打开 graph、发送小型 command，以及接收 Web 端 “open in Emacs” 事件。
- 后续融合优先通过 webhook/pipeline 做文件管理、外部动作和索引刷新，而不是恢复 Emacs→browser 实时全文同步。

### stdout 事件协议

web-host 向 Emacs 进程 stdout 发送换行分隔的文本事件，由
`+noema--handle-process-line` 解析：

| 前缀 | 格式 | 用途 |
|------|------|------|
| `aaronote-web-host:ready:` | `:<port>` | 服务器启动完成 |
| `aaronote-event:goto:` | `<line>:<col>` | 跳转到当前 buffer 位置 |
| `aaronote-event:open:` | JSON `{file,line,col,tag}` | 在 Emacs 打开文件/位置 |
| `aaronote-event:current-file:` | JSON `{file}` | 告知 Emacs 当前活跃笔记 |
| `aaronote-event:saved:` | JSON `{file}` | 笔记保存成功后触发 roam index 刷新 |

`saved` 事件是 Emacs roam 缓存自动更新的驱动源。收到后 Emacs 调用
`+noema-roam--runtime-sync`，sentinel 清空 in-process 缓存。这保证补全/
反向链接/xref 在每次 web 保存后自动更新，无需手动 sync。vault 外部改动
（Emacs 保存、dired、git pull）由 Node 侧 `fs.watch` 检测，经 SSE
`notes-index-changed` 事件驱动 web 端增量刷新，无需 Emacs 轮询。

手动 "y sync DB"（`+noema-roam/sync`）走 `/api` 路径，成功回调同样清空
in-process 缓存，与自动刷新保持一致。

### 页面搜索

搜索只有一个实现：Noema 的 Go 内核（`site-lisp/noema/kernel/noema/wikisearch`）。
Emacs 不保存页面列表，也不自己做匹配。`lisp/noema/search/config.el` 把输入原样发到
host 的 `noema:api:knowledge:search` 通道，内核返回已经排好序的结果，以及每条结果
是怎么找到的（`matchKind`）、标题里命中的区间（`titleRanges`）、摘要、纠正过的词
（`corrected`）和“可以创建的页面”（`create`）。

- `+noema-roam/find-note`（`H-o` hub 的 `j`）在 minibuffer 里边输入边查询。候选按
  “标题 / 相近标题 / 正文 / 纠正后的正文”分组，旁边显示文件路径和摘要。
- minibuffer 用专门的补全样式 `+noema-search`，它不过滤也不重排。内核把 `maintian`
  匹配到 `maintain`；任何再比较一次“输入与候选”的样式（orderless、flex）都会把这条
  结果丢掉。新增一个向内核查询的读取函数时，沿用 `+noema-search-read-page`，不要
  回到对本地快照做 `completing-read`。
- 语法与网页端搜索框相同：`Namespace:` 前缀、`tag:` `path:` `file:` `content:`
  `line:(a b)`、`"短语"`、`-词`、`OR`、`/正则/`。完整说明在
  `docs/noema/design/wiki-workspace.md` 的 Search 一节。
- 确认一个没有任何页面叫这个名字的标题，会带着它打开 New Page 工作台。
- host 没运行时，find-note 退回到 Emacs 自己扫描的笔记列表，只能按 ID 补全。

### 插入引用（`Cmd+R` / `M-r`）

在笔记里按 `Cmd+R`，光标处弹出引用选择器。前缀沿用 roam 自己的语法：

| 输入 | 列出 |
| --- | --- |
| （空） | 本页的标题、标签、带 ID 的块（按文档顺序），然后是最近的页面 |
| `文字` | 本页的标题/标签/块，然后是页面（与搜索同一套匹配，含拼写容错） |
| `#文字` | 标签和块：先本页，再所有页面 |
| `@文字` | 标题：先本页，再所有页面 |
| `页面#文字` / `页面@文字` | 某个页面内部的标签或标题；在页面行按 `Tab` 会写成 `ID@` |
| `@@文字` | 文献键（按键名、标题、作者、年份匹配） |
| `/文字` | 工作区里的文件；图片插入为嵌入，路径相对当前笔记 |

回车插入，`Esc` 关闭。插入的是 roam 链接：页面 `[标题](roam://ID)`，标签和块
`[名字](roam://ID#标签)`，标题 `[标题](roam://ID@上级@本级)`。插入后显示文字处于选中
状态：直接打字就换成自己的措辞，按方向键则保留默认。

- 候选和链接文本都由内核给出（搜索接口的 `references` 模式）。页面里的弹层
  （`site-lisp/noema/pages/reference-picker.ts`）和 Emacs 命令都不做匹配，也不拼
  链接语法。
- 在正文里手打 `[[` 或 `roam://` 也会打开同一个选择器，选中后替换掉已经打出的部分；
  `Esc` 之后同一处不会再弹，可以继续手写。原来补全弹层（company）里针对 roam 的四路
  补全源（`[[`、`roam://`、`roam://ID#`、`roam://ID@`）因此删除。路径链接
  （`[x](./文件.md#标签)`）、行内 `@@tag[`、文献、任务依赖等补全没有别的入口，保持原样。
- `Cmd+Shift+R`（Emacs 持有键盘时是 `M-R`）给光标处打标签并复制引用，即工具面板里的
  “Tag / copy reference”。
- Emacs 持有键盘时，Noema 窗格里的 `M-r` 调用 `+noema/insert-reference`，它让页面
  打开同一个弹层。在其他缓冲区里 `M-x +noema/insert-reference` 用 minibuffer 做同样
  的查询，问一次显示文字（默认是内核给的），再把链接插入到光标处。
- `M-r` 在普通 xwidget 浏览器缓冲区里仍是“刷新内容”；只有 Noema 窗格里改成了插入引用。
- Vim 的普通/可视模式下，输入法送来的按键（合成中、键码 229、全角字符）被整个吞掉：
  既不当作 Vim 命令，也不写进笔记。切回英文输入后按键照常是命令。

### 从 Raycast 打开终端

Raycast 的 Emacs 插件有独立的 **New Terminal** 命令；`skhd` 将全局
`Option+Enter` 绑定到这个 Raycast 命令，在新 Emacs frame 中打开 Portal
终端。Emacs 未运行时会先启动。

### 从 Raycast 使用 Noema

Raycast 的 “Emacs” 入口里有一个 Noema 分区，其中两项不经过 Emacs（它们也各自是独立
命令，方便单独绑定快捷键）。查找页面因此不在 Emacs 的动作白名单里：

- **Find Note**：边输入边搜索，Raycast 自己显示结果，语法和 Emacs、网页端相同。回车
  在 Emacs 里打开所选页面；`Cmd+L` 复制 wiki 链接，`Cmd+Shift+F` 在 Finder 里显示。
- **Noema Maintenance**：索引状态（页数、上次运行、错误）和每个仓库的同步状态；可以刷新或重建
  索引、对某个仓库做检查点或立即同步。

两者都读取 `var/noema/host.json`（Noema host 启动监听时写入）找到 host，然后调用
它的本机 `/api`。host 没运行时显示“Noema is not running”，不会去启动或唤起 Emacs。

### 诊断

- 服务日志：buffer ` *noema-web-host*`
- 手动停止：`M-x +noema/stop`
- 重新打开当前 note：`M-x +noema/refresh`（localleader `r`）
- 健康状态：`M-x +health/report` → Noema 栏（process、ready、runtime、last-sync）

## 9. Noema AI/agent

- `M-x noema` / `C-c A W` — Noema 主入口
- `M-x noema-agent-start` / `C-c A a` — 启动包管理的 agent-shell 上的结构化 agent
- agent-shell 原生 `turn-complete` 事件由 `noema-agent-acp.el` 转发给本配置，裸启动的
  Claude 等 agent-shell 会话也在创建 buffer 时订阅：每轮结束后只检查可见及最近的普通
  文件、JuText，以及打开的 Noema Markdown 页面。底层按 buffer-local
  `+auto-revert-agent-refresh-function` 分发：普通文件走安全 auto-revert，拥有磁盘合并规则
  的模式可注册自己的函数；无普通文件 buffer 的页面可注册
  `+auto-revert-agent-refresh-functions`，Markdown 页面就在这条路径上。普通远程文件
  也参与：`batched` 路由可在焦点事件检查，`round-trip` 路由只在 ACP 完成或用户显式
  解决冲突、或旧 buffer 再次显示时检查；断开的目标一律跳过。JuText 在无草稿时接收外部正文编辑，
  有草稿时保留本地内容并合并运行输出；页面先比较时间戳，确有变化
  且没有草稿才读取正文；不 advise ACP 内部函数，也不扫描整个项目。
- `M-x noema-agent-worktree-start` / `C-c A w` — 让会话在 Project workspace 仓库的独立 linked worktree（分支 `noema/<名字>`，基点记在 `branch.<分支>.noemaBase`）里工作，会话仍登记在发起它的 Project 下（D-038 不变）。Git 都经 `process-file` 在 Emacs 目录里运行，本机和 `/fs:` 远程走同一条代码路径；临时 buffer 里会带上调用方的环境 capsule。`noema-context` 把主 checkout 的引用改写到会话的 worktree（`noema-agent-worktree-redirect`），worktree 里没有这个文件就报错，区域引用的那几行在副本里内容不同也报错。Sessions 与总览的 `m`/`d` 打开该会话 checkout 的 Magit。设计来源见 [Agent Fleet 审计](../noema/records/audits/agent-fleet-noema-audit-2026-10.md)
- `M-x noema-agent-worktree-parallel` / `C-c A n` 与 `M-x noema-agent-worktree-board` / `C-c A A` — 并行尝试组（`noema-agent-worktree-board.el`）。组只记在各尝试分支的 `branch.<b>.noemaGroup`/`noemaAgent` 上，成员从 `git worktree list` 加一次 `git config --get-regexp` 得出，状态从活会话现读，不另建注册表，也不改 kernel。整组状态 waiting > working > settled，一份完成不终止其他。看板只在打开和 `g` 时调用 Git；可见时随 `noema-agent-acp-changed-functions` 重绘，buffer 被 kill 时移除 hook
- `M-x noema-agent-acp-conversation-tree` / `C-c A T` — 从当前 Noema Agent 会话浏览原生 ACP 对话树；Agent 窗口 `C-c C-t`、Sessions 列表 `T` 也可进入。`agent-shell-fork-tree` 作为固定版本的 package-vc 依赖；Noema 经 ACP 边界调用它，分叉后登记为命名会话，缓存仅驻内存（`agent-shell-fork-tree-cache-directory=nil`）。它按共享文本前缀展示，不是 WorkNode DAG 或 Noema `parentName` 的替代品
- `M-x noema-agent-inbox` / `C-c A G` — 汇总已知 Noema Project 的会话状态、待处理项和活动 agent；`RET` 跳转，`!` 跨项目轮转待处理项，`u` 经 `session:name:read` 标记已读并只刷新所属 Project，`G` 重扫项目根，`m`/`d` 打开所选会话 checkout 的 Magit status / 自分出点起的 diff。没有 Run 的活会话请求权限或答完一轮而 buffer 不在屏幕上时，`noema-agent-acp-attention` 在第一列标 `!approve`/`done` 并参与 `!` 轮转，显示该 buffer 或按 `u` 清除，Emacs 不在前台时经 `noema-agent-acp-notify-function` 发系统通知；有 open Run 的 buffer 由 `noema-agent-acp-run-owned-functions`（worker 注册）让给 host 的 Run attention，不重复报。可见时 ACP 状态事件只重绘，worker 的 Run 结束及权限/输入事件只合并查询所属 Project，慢回包由逐 Project 请求序号隔离；隐藏时停止监听，再显示补查。外部客户端变更仍可用 `g` 刷新
- `M-x noema-agent-abtop` / `C-c A U` — btop 风格的 agent 总览（quota / sessions / 详情，带停止、强制
  终止、重启等管理键）。读取边界都在 `noema-agent-acp.el`：`noema-agent-acp-usage` 读 agent-shell 的
  `:usage`（Run 记录的 `sessionUsage` 也经它）；`agent-shell--save-usage` 与
  `--update-usage-from-notification` 上的 `:after` advice 记录上下文峰值、压缩次数（借鉴 abtop：
  上下文较上次跌破 70% 且上次 ≥10k）和 Claude `_claude/rateLimit`（`noema-agent-acp-rate-limits`），
  然后运行 `noema-agent-acp-changed-functions`；该钩子也在 agent-shell 每次完整的 header 更新（会话建立、
  模型/模式切换、一轮开始结束，不含忙碌动画的缓存帧）和 agent buffer 被杀时运行。Codex 额度只在渲染时读最新会话文件末尾 128 KiB，
  按文件 mtime/size 缓存。不做进程扫描或轮询
- `M-x noema-compose` / `C-c A c` — 直接复用内化 gptel 的 compose UI
- `C-c A s/m/./r` — send、transient、context、rewrite/diff
- `C-c A p` — 把当前 agent-shell session 纳入 research
- `C-c A x/v/B/f/@` — 把当前上下文、选区、buffer、文件、光标位置交给某个 agent
  会话；`C-c A ,` 先检视，`C-c A d` 只草拟不提交。`v/B/f/@` 只发送点名的那一段；每次都询问会话，
  只列运行中的 ACP 会话，本项目优先、上次仍运行的选择预选（`noema-context-always-ask-session`）
- agent-shell 输入 `/resume` — 向当前 Codex/Claude 等 ACP adapter 读取同工作目录的原生
  `session/list`，选中后以原生 ID 走 `session/load`/`session/resume`；已有 buffer 直接切换，
  其他历史会话替换当前 Agent 或 popup buffer、不增加 tab。Noema 已登记的 ID 恢复到原名。
  原生历史由 agent 后端保存，不靠 Emacs transcript。
- Noema Markdown 页面里没有 Emacs 文本：`C-c A v/B/@/./c/s/r` 和 `C-c A e` 经
  `noema-md-bridge` 让页面先保存并报告选区的准确位置，再在笔记的 Emacs 源 buffer
  上运行 gptel / agent UI；常用文本命令（`H-i`、`H-s`、复制粘贴、`C-x C-s` 等）在
  `+noema-keys-mode-map` 里重定向到页面操作，见 daily-usage“Noema 页面里的
  Emacs 按键”
- 每个 agent session 仍是真正可交互的 agent-shell buffer，但不进入全局
  tab-line/tab-bar。同一项目的所有 agent/session 作为 tab 显示在右下角同一个
  Agent 窗口的 tab-line 上，切换 tab 就是切换窗口里的真实 buffer，不复制
  transcript。点击 tab，或 `C-c C-a` 选择、`C-c C-n`/`C-c C-p` 切换 session；
  右键 tab 打开管理菜单（停止、重启、关闭、重命名、fork、归档等），`?` 查看全部
  按键。Noema Run 结束后补回输入提示符，`C-c C-e` 或 `C-c A i` 聚焦输入。
- 重跑 work 块从上游状态重建会话（独占会话沿用原名、换新一代，共用会话则分支），新对话
  注入项目根、文档路径和 lineage 上游块。权限由 kernel 判定：项目内自动批准，项目外与
  网络进入 Attention 弹窗，sudo/凭据/`git push`/改写历史始终拒绝。
- 取消链路：准备中的 Run 被取消时 kernel 立即结束它、拒绝迟到的 attach/start，并把
  cancelled 写回格子；运行中的 Run 由 worker 发 ACP `session/cancel`、撤回待批权限，
  3 秒无响应再关停 agent 进程。JuText `C-c C-z` 按光标所在格子取消。
- `M-x noema-skill` 直接打开 Skill／Pack 输入补全搜索；在搜索输入时按
  `Cmd+,` 进入完整 Skill 管理 buffer。在 agent-shell 或 Portal 终端中选中结果，
  只在输入处草拟引用，不自动发送。`noema-mcp-manager` /
  `noema-capability-manager` 打开完整管理页；
  从 agent-shell 调用它们时仍进入只读 lookup（`M-x noema-capability-lookup`）。
- `.noema` 的 JuText 是研究工作文档，没有语言服务器。Company 在 work 开头的
  `@@agent`、`@@session`、`@@ctx`、`@@skill` 控制行只使用 Noema CAPF；普通正文
  用文字补全，Yasnippet 模板用 `C-c y y` 显式展开，避免 TeX/Markdown 模板混进
  Skill 候选。候选取自同一项目的能力解析缓存；启用状态和来源显示在注释里，
  无效或明确禁用的 Skill 不可选。Flymake 把现有结构校验的错误和警告标在
  对应块标题；公式预览复用 RaTeX 和共享 KaTeX 宏。
- 会话登记（`noema-agent-acp.el`）：`noema-agent-acp-start` 用 `:origin` 记下是哪个
  入口开的（`run`/`popup`/`manual`/`foreign`/`pi`/`takeover`/`probe`/`side`），
  `noema-agent-acp-adopt` 是唯一登记点，负责解析项目 root、生成唯一会话名、调用
  `noema-agent-acp-mark-session-buffer`；裸 `M-x agent-shell` 经管理入口以
  `manual` 来源在调用窗口启动，其他程序自行创建的 agent-shell buffer 仍由
  `agent-shell-mode-hook` 收编（`noema-agent-acp-adopt-foreign-sessions` 可关）。
  root 解析是纯查询（含 `[project]` 的 `noema.toml` → projectile → project.el →
  该目录，统一投影成本机原生路径），绝不创建 Project。只有已经是 Noema 项目时才额外 `session:promote` + `session:name:bind`
  写进 Node 持久注册表，失败不影响会话。`noema-agent-acp-sessions` 是合并视图，
  `noema-sessions-read` 是统一的会话选择器（可选“新开一个”；context send
  还可在列表末尾选择复制 prompt 到剪贴板，不需要 ACP 会话）。
  `noema-agent-acp-ephemeral-origins`（默认 `side`、`latex-export`）里的来源不写持久注册表。
- 生命周期：所有自动停止路径（warm 清扫、项目最后一个 `.noema` 关闭后的收尾、项目
  关闭期间结束的 Run）统一经 `noema-agent-acp-auto-stoppable-p`——空闲、没有排队的
  prompt（agent-shell 的 `:pending-prompts`）、不可见、来源在
  `noema-agent-acp-auto-stop-origins`。`agent-shell-mode-hook` 给每个 agent buffer
  装 `after-change-functions` 记录活动时间；`noema-agent-worker-stop-buffer` 在
  `noema-agent-acp-busy-p`（含 shell-maker 的交互回答）时拒绝停止。
- 放置：进程目录由 `noema-agent-acp-process-directory-function` 决定，本配置设为
  `+agent-shell-process-directory`，与裸 `agent-shell` 的 `agent-shell-cwd` advice
  是同一函数；远端 agent 的契约见 `docs/design/remote-framework.md` 第 7 节。
- 并行：RunSpec 冻结仍经 Magent 全局队列逐个进行，Run 派发后立即释放队列票据；
  同时执行的 Run 数由 `noema-agent-worker-max-concurrent-runs` 限制，超出的按顺序
  在 `noema-agent-worker--slot-waiting` 等待。同一文件的并发编辑由 kernel 权限策略
  转为人工决定（`policyReason`），不拒绝。设计来源与取舍见
  `docs/noema/records/audits/pisper-agent-lifecycle-study-2026-09.md`。
- 编辑器上下文（`noema-context.el`）：挑选完全复用 gptel 的 `gptel-context` 与
  `*gptel-context*` 检视 buffer，只有一份选择。发送时把它解析成引用
  （`noema-context-references`），再构造内容块：一个 text 块列出
  `path:START-END`（与 `agent-shell--get-region-context` 同格式），每个文件一个
  `resource_link`。**不发 embedded `resource`**，所以不吃 `embeddedContext` 能力
  与 `agent-shell-embed-file-size-limit` 的限制，也不把正文塞进请求。
  agent-shell 内部符号（`--resolve-path`、`--read-file-content`、
  `--prompt-queue-enqueue`）一律经 `noema-agent-acp-file-uri` /
  `-file-metadata` / `-enqueue` 这层边界，不在 `noema-context.el` 里直接引用。
- 编排面板（`noema-orchestration.el`，`C-c A O`）：kernel 的
  Task/Job/Invocation/Worker/Delegation 模型一直完整，但 D-016/D-017 退役
  Orchestration Lab 之后就没有任何界面，`orchestration:snapshot` 无人调用。
  这个 board 就是那个界面。它**只读**——作业与开 Run 仍在 Graph Board——回答
  Graph Board 答不了的问题：什么被分解成了什么、谁排在谁后面、哪个 worker 持有
  认领、以及哪个 agent 委派给了哪个（delegation 是 Graph Board 没有的关系）。
  一次快照喂所有视图（`t`/`j`/`w`/`d`/`e`，`TAB` 轮换），所以切视图不重新请求，
  整块板子是同一个时刻的一致快照而不是五次独立读取。
- 两个 MCP 面：`/mcp` 是知识库（笔记、搜索、块、标签），`/mcp/research` 是 AI 流程
  （`research_cell` / `research_run` / `research_state` / `artifact` /
  `proposal.create`），`/mcp/coordinator` 是 Pi。注册表仍然只有一个，端点只差一个投影
  谓词（`tools.SurfaceForTool`）；没标 `Surface` 的工具算知识库，所以新增工具不会悄悄
  扩大 research 面。能力 id 相应拆成 `noema-knowledge` / `noema-research`，配置里写旧的
  `noema` 会被展开成两者（否则「禁用 noema」只会关掉一半）。
- 技能库：`resources/skills/` 只放描述 Noema 自身机制的技能（随代码版本走）；其余住在
  `<NOEMA_ROOT>/public/README/Skills/skills/`，由 `NOEMA_GLOBAL_SKILLS` 指向（`+noema-skills-directory`
  给默认值），和笔记同一个 git、同一套 search。约定是可移植的那套
  `<kebab-id>/SKILL.md`，深度放 `references/<topic>.md` 按需读——解析结果带
  `directory` 和 `references` 列表，SKILL.md 超过约 8 KiB 会给一条提示。
- 拒绝 / 警告 / 品味三分法：结构性损坏才拒绝；**认知性失守只警告**
  （`done-without-basis`、`done-run-mismatch`、`done-over-regressed`、
  `active-without-run`），走 `noema-research-validate` 的 `:warnings` 通道，而
  `structure-edit` 只 diff `:errors`，所以提示永远不会变成门禁。建模品味留在 Skill 里。
- 陈旧检测的推导链（零 schema 改动）：节点 → `artifact_links`（每个 Run 创建/修改的文件，
  带 `run_id` + `work_node_id`）→ `artifacts.sha256` → 与磁盘当前哈希比较。
  `Store.SourceChanges` 实现，`research_cell {action: "changes"}` 暴露；**只报告，
  不改任何状态**。
- 交付诚实：`research_state` 只记录一条 `worknode.state` 协调请求，由 Emacs 应用；
  `research_state {action: "status", requestId}` 让 agent 确认它到底落没落地
  （pending / claimed / done / failed），而不是假设。
- profile 与模板在 `etc/noema/`，运行状态在 `var/noema/`

源码边界：Noema 代码在 `site-lisp/noema/lisp/`；agent-shell、acp.el、shell-maker
由 package-vc 管理，固定 revision 在 `tools/agent/config.el` / `package-lock.el`。
`noema-agent-render.el` 保留隐藏输出性能策略，不修改上游包文件。
gptel、Magent、Claude/Codex 兼容源码仍在 `site-lisp/noema/upstream/`。
任何一层都不能用自研简化实现替代成熟工具链。

### Claude Code

- 独立的 claude-code-ide 菜单不再全局绑定；需要 Claude 时从 Noema agent 入口选择。
- CLI 路径在 `lisp/tools/agent/config.el` 的 `claude-code-ide-cli-path` 中配置。
- 兼容源码保留在 `site-lisp/noema/upstream/claude-code-ide/`。

### Codex CLI

- `C-c c t` — 切换面板
- `C-c c s` / `C-c c q` — 启动 / 停止
- `C-c c p` / `C-c c r` / `C-c c f` — 发送 prompt / region / file
- 本地源码由 `site-lisp/noema/upstream/codex-cli/` 提供

### Copilot

- 在 `prog-mode` 和 `org-mode` 默认启用
- 接受补全：
  - `s-]`（macOS `⌘ ]`）整段接受；`M-]` 作为 modifier 兼容别名
  - `M-}`
  - 中文输入法下可用对应中文标点键，例如 `M-】` / `M-｝`
- `TAB` 不接受 Copilot：Company 候选框开启时选中候选，否则继续走 snippet / 缩进

## 9. 如果 LSP 或远程不工作

优先检查：

1. 先开 `M-x +language-server/doctor`
2. 看当前 buffer 的 route policy / active backend / executable
3. 远端 PATH 里是否真的有这些 server
4. `lsp-mode` 是否已经 attach
5. TRAMP 主机是否能正常登录
6. *Messages* 出现 `Error processing message ...` 时，`M-x +language-server/message-errors`
   （language server 菜单 `x`）给出对应的调用栈；lsp-mode 本身只打印错误对象

更详细的维护和排查见 [lsp-workflow.md](../design/lsp-workflow.md) 和 [maintenance.md](../ops/maintenance.md)。

Citre 的 definition/reference fallback 也复用 `lsp-mode` 的 `xref-lsp`
backend，然后才尝试 tags/global；它内置的 Eglot adapter 不在本配置的
backend list 中。

## 10. Board UI 工具库 (`aaron-ui-board`)

所有只读 dashboard / hub / report buffer 都通过 `site-lisp/aaron-ui/aaron-ui-board.el`
渲染，该库建立在 `aaron-ui` 语义 palette 之上，提供统一的 Kanagawa Wave 风格：
大标题 + icon、badge 标签/统计、action toolbar、section 分区 + count badge、
label-badge 字段行、hl-line 行高亮、styled header-line。

**基础 API**

| 函数 | 说明 |
|------|------|
| `(aaron-ui-board-set-header TITLE ICON &optional STATUS)` | 设置 header-line |
| `(aaron-ui-board-render RENDERER)` | 保位刷新（保留当前行/item） |
| `(aaron-ui-board-insert-page-header TITLE &key icon subtitle stats actions)` | 渲染页头 |
| `(aaron-ui-board-insert-section TITLE &optional COUNT TONE)` | 渲染 section 标题 |
| `(aaron-ui-board-insert-field LABEL VALUE &optional FACE)` | 渲染字段行 |
| `(aaron-ui-board-insert-row &key id icon badge title meta detail action ...)` | 渲染可点击行 |
| `(aaron-ui-board-insert-badge LABEL &optional TONE)` | 渲染 badge（info/success/warning/danger/muted）|
| `(aaron-ui-board-insert-action LABEL COMMAND HELP &optional PRIMARY)` | toolbar 按钮 |
| `(aaron-ui-board-insert-actions ACTIONS)` | 渲染多个按钮（plist 列表）|
| `(aaron-ui-board-insert-metric LABEL VALUE &optional RATIO SUFFIX)` | 带进度条的指标行 |
| `(aaron-ui-board-bar RATIO &optional WIDTH)` | 返回文本进度条字符串 |
| `(aaron-ui-board--level-face RATIO)` | 根据 ratio 返回 good/warn/bad face |
| `(aaron-ui-board-insert-openable-path PATH &optional LABEL)` | 可点击路径按钮 |
| `(aaron-ui-board-insert-key-hints TEXT)` | 渲染 dim 的快捷键提示行 |

**新建 dashboard 的步骤**

1. `(require 'aaron-ui-board)` 并从 `aaron-ui-board-mode` 派生 major mode
2. 在 `with-current-buffer` 里调用 `(aaron-ui-board-set-header TITLE ICON)` 和
   `(setq-local aaron-ui-board-refresh-function #'my-refresh-fn)`
3. 刷新函数内调用 `(aaron-ui-board-render (lambda () ...))`，lambda 内用上述
   `insert-*` 原语构建内容
4. 其他自定义按键用 `(local-set-key …)` 叠加即可

**已迁移的 dashboards**

- Noema Roam views（`noema/roam/ui.el` → shim）
- Config Health（`config/health/config.el`）
- Language Server Hub + Doctor（`tools/lsp-hub/config.el`）
- Performance Watch（`config/performance/config.el`）
- Compile Board（`config/compile/config.el`）
- 当前 buffer 的 Diagnostics UI（`checkers/diagnostics/config.el`）：每个 frame 共用一个面板，
  三种严重程度只改变筛选；项目列表使用 Emacs 原生 Flymake

## 11. Transient 菜单 + which-key 主题（`aaron-ui-transient`）

所有 `transient-define-prefix` 菜单（23+个 dispatch，包括 health、compile、git、
lsp、project 等）和 which-key popup 通过 `site-lisp/aaron-ui/aaron-ui-transient.el`
统一配色，**无需对每个 dispatch 单独修改**。

- heading = accent-cyan bold；key = accent-cyan；stay = accent-green；
  exit = accent-mauve；return = accent-yellow；noop/inactive = fg-faint
- 新增 dispatch 自动继承，不需要任何额外设置
- 配置入口：`lisp/tools/menus/config.el` → `(require 'aaron-ui-transient)`
- 主题切换后自动重新应用（`after-load-theme-hook` + signature guard）
