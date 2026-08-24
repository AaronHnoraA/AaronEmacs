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
  [remote-parity.md](remote-parity.md)。

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

完整维护契约见 [lsp-workflow.org](lsp-workflow.org)。现有语言模块中残留的
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

- `M-x my/language-server-manager`
  打开 `*Language Server Hub*`
- `M-x my/language-server-doctor`
  打开 `*Language Server Doctor*`

Hub 里可以直接看：

- 当前 buffer 的 route policy / active backend / workspace config
- 注册的 lsp-mode client/feature 路由
- 一组 session 级调参入口

Doctor 更适合快速排查：

- 关键 library 是否存在
- 本地 server executable 是否存在
- 当前 buffer 命中了哪条路由

详细模型看 [lsp-workflow.org](lsp-workflow.org)。

### Lean (lsp-mode + xwidget infoview)

Lean 4 走自定义 `lean-mode`，并统一使用 lsp-mode。模块分层如下：

| 模块 | 职责 |
|------|------|
| `lisp/lang/lean/init-lean.el` | 主 mode、lsp-mode client 注册、project 定位、ripgrep 符号搜索、UI 总入口 |
| `lisp/lang/lean/init-lean-lsp.el` | `$/lean/fileProgress` 通知、fringe/sideline 进度与 Flymake 兼容层 |
| `lisp/lang/lean/init-lean-infoview.el` | 官方 xwidget infoview 桥接（`C-c C-i`）|
| `lisp/lang/lean/lean4-infoview-bridge/` | Node.js HTTP bridge：转发 LSP、服务官方 React infoview |

**键位（lean-mode buffer）**

| 键 | 命令 |
|----|------|
| `C-c C-i` | 切换官方 xwidget infoview |
| `C-c C-r` | 重启 lsp-mode workspace |
| `C-c C-d` | 重刷文件依赖 |
| `C-c C-a` | lsp-mode code actions |
| `C-c C-l` | 打开 Lean dev log |
| `C-c C-k` | 查 unicode 输入法键位 |

**Xwidget infoview 架构**

1. `lean-iv-toggle` 启动 `lisp/lang/lean/lean4-infoview-bridge/server.mjs`（Node.js），参数为
   `<port> <project-root>`；server 内部运行 `lake serve`（LSP）。
2. Bridge 在 stdout 输出 `LEAN_INFOVIEW_PORT=<N>`，Emacs 进程 filter 捕获后开
   启 xwidget-webkit 窗口（`http://127.0.0.1:<N>/`），加载官方 `@leanprover/infoview`。
3. **Emacs→infoview**：`lean-iv-sync-cursor-h` 在每次 `post-command-hook` 调用
   `window.updateCursor(uri, line, char)`；文本变化通过 HTTP POST `/cursor` 同步。
4. **Infoview→Emacs 反向通道**：infoview 点击/触发动作时，前端 POST 到 bridge
   的 `/editor/<cmd>` 路由；bridge 将 `EMACS_CMD={...}` 写到 stdout；Emacs 进程
   filter 解析后分发：
   - `show-document` → `pop-to-buffer` + 跳转行列（点击 goal 定位）
   - `insert-text` → 在活跃 lean buffer 插入文字（"Try this" 建议）
   - `apply-edit` → 应用文本编辑（code actions）
   - `restart-file` → 调用 `lean-refresh-file-dependencies`

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

行号/margin 区域可以点击切换断点；左 fringe 保留给折叠图标，调试断点可用
`S-mouse-1` 在 fringe 上切换。调试期间还有 `hydra-dape-mode`。

默认配置覆盖常见 DAP 工作流：

- Python: `debugpy` / module
- JavaScript、TypeScript、Chrome、Node attach: `js-debug`
- Java: `jdtls` + `vscode-java-debug`
- C、C++、Rust: `lldb-dap` / `lldb-vscode` / `gdb` / `cpptools`
- Go: `dlv` / test binary
- Shell、.NET、PHP、Ruby、OCaml: 对应 adapter

`M-x my/debug-adapter-doctor` 可以查看 adapter 可用性和缺失安装项。

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
  当前项目 vterm
- `C-c p .`
  非 Evil 场景下打开项目工作台

### 添加 / 发现项目

- `SPC p a`
  手动添加一个项目根目录
- `SPC p D`
  按目录批量扫描项目

如果你有固定项目根，去看 [project-guide.md](project-guide.md) 里的 `my/project-search-paths`。

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

### VTerm

- `M-\``
  `vterm-toggle`
- `C-c e`
  弹出/收起当前 workspace 的 popup `vterm`；远端 buffer 会打开远端终端
- `C-c C-e`
  智能弹出或收回当前 popup `vterm`
- `C-c E`
  切换到下一个 popup `vterm`；`C-u C-c E` 新建一个
- `C-c M-E`
  新建 popup `vterm`
- `C-c M-e`
  切换当前 popup `vterm` 的固定状态
- `SPC o e`
  `vterm-toggle`
- `SPC o E`
  切换到下一个 popup `vterm`
- `SPC o F`
  切换当前 popup `vterm` 的固定状态
- `SPC o t`
  `vterm-toggle`
- `SPC o v`
  新建 `vterm`
- `SPC o V`
  创建命名 `vterm`
- `SPC o S`
  `my/vterm-ssh`
- `M-x my/project-popup-vterm-app`
  在当前 project 根目录的新 popup `vterm` 里运行 `lazygit` / `btop` / `yazi` / `tmux`

## 6.1 数据库 / SQL

- `M-x clutch-query-console`
  打开 `clutch` 的连接选择器和查询控制台
- `SPC o q`
  同上

这套配置把数据库客户端接到了 `clutch`，但保持懒加载：

- 包声明在 [lisp/init-clutch.el](../lisp/init-clutch.el)
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
- 普通 Emacs 文件 API 保留 TRAMP 的完整兼容性
- lsp-mode、direnv、环境探测与自定义进程可以优先 tramp-rpc，失败时回退 TRAMP
- socket/stream/port-forward 走显式 channel API，远端不支持时不会静默落到本机
- PATH 按 target/workspace ID 隔离，并由 host probe、direnv、toolchain 等分层维护

### 你应该怎么用

- `M-x remote-board`：查看/打开 logical target、当前 route 与健康状态
- 编辑远程文件：使用 `fs://target/path` 或 `/fs:target:/path`
- 旧的 `/ssh:host:/path` 仍可打开，进入 buffer 后会 canonicalize 为 `/fs`
- 在当前本地/远端 workspace 打开终端：`C-c e`（`vterm-toggle`）
- 按主机名直接开终端：`M-x my/vterm-ssh`

`C-c e` 是推荐入口。它读取当前 buffer 的 `/fs:TARGET:/path` 上下文，通过
remote process adapter 启动 vterm，并把 terminal 生命周期登记给 workspace；
本地与远端走同一条路径。popup 只在同一 workspace 内复用和循环，不会把本地
terminal 带到 WSL/SSH buffer，也不会跨两个远端 target 串线。

`my/vterm-ssh` 会优先读：

- `~/.ssh/config`

所以最好把常用机器写成 Host 别名。

本地文件同样属于 `local` target。自定义进程不要自行拼 `/ssh:` 或 `/rpc:`：
使用 `remote-make-process`、`remote-process-file`、`remote-exec` 和
`remote-executable-find`。没有 file-name 参数的网络操作使用
`remote-make-network-process`、`remote-open-network-stream` 与
`remote-port-forward`。完整对象模型、能力矩阵、配置 schema、当前缺口与接入清单
见 [remote-framework.md](remote-framework.md)。

开发新的 project/LSP/search/SCM/debug/task 集成时，所属模块可以保留业务策略，
但 path projection、process placement、environment、session、watch、service 和
channel lifecycle 必须委托给 Remote 框架。若必须写 local/remote 条件分支，先把它
视为框架缺少能力的信号，而不是正常的 consumer 实现方式。

## 7. 浏览器

### 三套入口

- `eww`
- `xwidget-webkit`
- `appine`

### 常用命令

- `C-c w e`
  `my/open-eww-url`
- `C-c w x`
  `my/open-xwidget-url`
- `C-c w a`
  `my/appine-open-url`
- `C-c w s`
  交互选择当前页面的目标后端
- `C-c w E` / `C-c w X` / `C-c w A` / `C-c w O`
  当前页面直达 `eww` / `xwidget-webkit` / `appine` / macOS `open`
- `C-c w d` / browser buffer `M-w`
  kill 当前 `eww` / `xwidget-webkit` buffer 并删除对应窗口，或关闭当前 Appine tab
- `C-c w k`
  `my/appine-kill-all`
- `C-c w w`
  统一 `browse-url`

### 当前逻辑

默认打开方式集中在 `lisp/init-open.el` 的 `my/open-routes`，通用 DSL helper
在 vendored `site-lisp/general.el/general.el` 的 `general-route-*`。`browse-url`
默认走 `menu`，菜单默认项是 `xwidget`，不做 URL 复杂度自动分流。分流保持手动：

browser pipeline 额外提供：

- EWW <-> Xwidget
- EWW/Xwidget <-> Appine
- EWW/Xwidget 默认打开到独立浏览 buffer，连续多开不覆盖已显示的浏览 buffer
- 当前页面 -> macOS `open`
- 统一搜索入口：`my/browser-open-search`
- 当前页面后端切换：`my/browser-switch-to`
- Appine 便携动作：打开文件、打开光标下链接、前进后退、刷新、标签切换、全杀
- Appine board 的文件、目录、URL、tab registry 上都有 macOS `open` 入口

因此当前建议是：

- 文本/阅读优先 `eww`
- 默认真实网页优先 `xwidget-webkit`
- 原生嵌入或 AppKit 文件查看需要时手动走 `appine`
- 文件、URL 需要系统应用接管时走 `system` / macOS `open`
- 关闭 Appine 的最后一个标签会自动清掉 host buffer

**xwidget 焦点**：页面加载完成（`load-finished`）时自动开启 `xwidget-webkit-edit-mode`、
注入一次 `<right>` 唤醒 native WebKit first-responder、并运行 buffer-local `my/xwidget-focus-script`
（如 Noema 的 CodeMirror 聚焦 JS）。打开后无需手动先按键即可直接输入。
相关开关：`my/xwidget-auto-focus-on-load`、`my/xwidget-prime-native-focus`、`my/xwidget-prime-key`。
手动恢复焦点：`M-x my/xwidget-focus`（xwidget buffer 内绑定到 `i`）。

## 8. Noema Markdown Web/Appine

### 架构

Markdown 笔记现在由 Noema Web 自己编辑和渲染，Emacs 只负责启动、
打开、粗粒度命令和文件定位：

```
Emacs (init-aaronnote.el)
  └─ spawn node lisp/roam/Noema/web-host.mjs
       ├─ 静态服务 lisp/roam/Noema/dist/aaronnote/
       ├─ 运行时 lisp/roam/Noema/server/ + shared/
       ├─ 注入 window.aaronnoteApi 适配器（替代 Electron preload）
       ├─ POST /api            ← Web app 调 runtime 保存/索引/文件操作
       ├─ GET  /events         ← Emacs 控制事件推送到页面
       └─ POST /emacs/command  ← Emacs 粗粒度控制通道
  └─ 当前 window 直接切到 Appine/xwidget，加载 http://127.0.0.1:<port>/?file=<md>
```

笔记仓库通过 `.roam` 符号链接挂载（机器本地，不入 git）。Noema 是
`lisp/roam/Noema/` 下的完整项目，源码、build、runtime、plugins 和
roam-tools JS 都在其中；运行时不依赖 `~/HC/Org`。

Noema 与 Emacs 共用：

- 状态与缓存：`var/aaronnote/`
- snippets：`snippets/`（只有 `markdown-mode/`、`tex-mode/` 链接到 Noema，其它语言由 Emacs 仓库维护）
- templates：`templates/`（只有 `noema/`、`latex/`、`tex/` 链接到 Noema，其它模板由 Emacs 仓库维护）

### 关键文件

| 文件 | 用途 |
|------|------|
| `lisp/roam/init-aaronnote.el` | Emacs 入口：进程管理、Appine/xwidget 打开、控制命令 |
| `lisp/roam/Noema/` | 完整 Noema 项目（链接到 `~/HC/SOURCE/Noema`） |
| `lisp/roam/Noema/web-host.mjs` | Node HTTP+SSE 桥接服务器和 preload adapter |
| `lisp/roam/Noema/dist/aaronnote/` | Noema static build |
| `.roam` | → Markdown 笔记目录（`AARONNOTE_ROOT`） |

### 边界

- Noema Web 拥有 CodeMirror 文档状态、输入手感、保存、文件树、graph。
- Emacs 打开 `.md` / `.markdown` / `README.md` 时直接交给 Noema，并关闭临时 Markdown buffer。
- Emacs 不再通过 `after-change-functions` 推送 buffer 全文，也不做 per-keystroke preview。
- Emacs 侧命令只做打开当前 note、打开 graph、发送小型 command，以及接收 Web 端 “open in Emacs” 事件。
- 后续融合优先通过 Appine/webhook/pipeline 做文件管理、外部动作和索引刷新，而不是恢复 Emacs→browser 实时全文同步。

### stdout 事件协议

web-host 向 Emacs 进程 stdout 发送换行分隔的文本事件，由
`my/noema--handle-process-line` 解析：

| 前缀 | 格式 | 用途 |
|------|------|------|
| `aaronote-web-host:ready:` | `:<port>` | 服务器启动完成 |
| `aaronote-event:goto:` | `<line>:<col>` | 跳转到当前 buffer 位置 |
| `aaronote-event:open:` | JSON `{file,line,col,tag}` | 在 Emacs 打开文件/位置 |
| `aaronote-event:current-file:` | JSON `{file}` | 告知 Emacs 当前活跃笔记 |
| `aaronote-event:saved:` | JSON `{file}` | 笔记保存成功后触发 roam index 刷新 |

`saved` 事件是 Emacs roam 缓存自动更新的驱动源。收到后 Emacs 调用
`my/noema-roam--runtime-sync`，sentinel 清空 in-process 缓存。这保证补全/
反向链接/xref 在每次 web 保存后自动更新，无需手动 sync。vault 外部改动
（Emacs 保存、dired、git pull）由 Node 侧 `fs.watch` 检测，经 SSE
`notes-index-changed` 事件驱动 web 端增量刷新，无需 Emacs 轮询。

手动 "y sync DB"（`my/noema-roam-sync`）走 `/api` 路径，成功回调同样清空
in-process 缓存，与自动刷新保持一致。

### 诊断

- 服务日志：buffer ` *aaronnote-web-host*`
- 手动停止：`M-x my/noema-stop`
- 重新打开当前 note：`M-x my/noema-refresh`（localleader `r`）
- 健康状态：`M-x my/health-report` → Noema 栏（process、ready、runtime、last-sync）

## 9. AI

### AI Workbench

- `M-x ai-workbench` — 直接打开当前项目的交互 backend session
- `M-x ai-workbench-compose-buffer` — 打开统一 compose buffer
- `C-c M-a` — 在当前文件里打开引用式 AI 工具入口
- `C-c A w` / `C-c A m` — workbench / compose 快捷入口
- `C-c A i r` / `C-c A i b` / `C-c A i f` — 直接发送 region / buffer / file
- 第一次打开会选择 backend，session 默认 profile 先记为 `default`，profile 文本从 `etc/ai-workbench/profiles/default.txt` 读取
- 统一入口不再停在中转 frontend，而是直接弹出 Claude/Codex 交互 buffer
- Codex 默认走交互 terminal session，不默认走 `exec --json`
- session 创建时会自动在项目目录启动，并自动注入一次 workdir/profile 提示
- 如果输出中包含 patch，会额外进入 diff buffer，走 `a`/`x` accept/reject
- 引用式入口会写 `var/ai-workbench/` manifest，profile 启动期要求 AI 把所有修改文件写入清单，diff review 会读取它
- 这层当前是主推荐入口，底层仍复用 Claude Code / Codex CLI

### Claude Code

- `C-c C-'` / `C-c a` — claude-code-ide 菜单
- `F12` / `H-l`       — 同上
- 配置：`lisp/init-ai-ide.el`，变量 `claude-code-ide-cli-path`
- 本地源码由 `site-lisp/ai-workbench/vendor/claude-code-ide/` 提供

### Codex CLI

- `C-c c t` — 切换面板
- `C-c c s` / `C-c c q` — 启动 / 停止
- `C-c c p` / `C-c c r` / `C-c c f` — 发送 prompt / region / file
- 本地源码由 `site-lisp/ai-workbench/vendor/codex-cli/` 提供

### Copilot

- 在 `prog-mode` 和 `org-mode` 默认启用
- 接受补全：
  - `s-]`（macOS `⌘ ]`）整段接受；`M-]` 作为 modifier 兼容别名
  - `M-}`
  - 中文输入法下可用对应中文标点键，例如 `M-】` / `M-｝`
- `TAB` 不接受 Copilot：Company 候选框开启时选中候选，否则继续走 snippet / 缩进

## 9. 如果 LSP 或远程不工作

优先检查：

1. 先开 `M-x my/language-server-doctor`
2. 看当前 buffer 的 route policy / active backend / executable
3. 远端 PATH 里是否真的有这些 server
4. `lsp-mode` 是否已经 attach
5. TRAMP 主机是否能正常登录

更详细的维护和排查见 [lsp-workflow.org](lsp-workflow.org) 和 [maintenance.md](maintenance.md)。

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

- Noema Roam views（`init-md-roam-ui.el` → shim）
- Config Health（`init-health.el`）
- Language Server Hub + Doctor（`init-lsp-tools.el`）
- Performance Watch（`init-performance.el`）
- Compile Board（`init-compile.el`）
- Diagnostics UI（`init-diagnostics-ui.el`）
- Appine Board（`init-appine.el`）

## 11. Transient 菜单 + which-key 主题（`aaron-ui-transient`）

所有 `transient-define-prefix` 菜单（23+个 dispatch，包括 health、compile、git、
lsp、project 等）和 which-key popup 通过 `site-lisp/aaron-ui/aaron-ui-transient.el`
统一配色，**无需对每个 dispatch 单独修改**。

- heading = accent-cyan bold；key = accent-cyan；stay = accent-green；
  exit = accent-mauve；return = accent-yellow；noop/inactive = fg-faint
- 新增 dispatch 自动继承，不需要任何额外设置
- 配置入口：`lisp/init-tools.el` → `(require 'aaron-ui-transient)`
- 主题切换后自动重新应用（`after-load-theme-hook` + signature guard）
