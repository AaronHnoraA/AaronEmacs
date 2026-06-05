# Dev, LSP, Remote Guide

这份文档覆盖：

- 补全和 LSP
- 调试
- 项目导航
- 终端与远程
- 浏览器与外部集成

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

### 默认组件

- `company`
- `company-prescient`
- `company-box`
- `eglot`
- `flymake`
- `flymake-diagnostic-at-point`
- `eldoc-box`
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
  左侧 `Treemacs` smart toggle，并跟随当前文件和光标所在 symbol
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
  语言服务器菜单
  Hub / Doctor / 调参 / log / session / config
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
- 显式 `lsp-mode` 路由
- 自定义 Eglot server 映射
- 一组 session 级调参入口

Doctor 更适合快速排查：

- 关键 library 是否存在
- 本地 server executable 是否存在
- 当前 buffer 命中了哪条路由

详细模型看 [lsp-workflow.org](lsp-workflow.org)。

### Lean (eglot + xwidget infoview)

Lean 4 走自定义 `lean-mode`（eglot 作为 LSP 客户端），不再使用 `lean4-mode` +
`lsp-mode`。模块分层如下：

| 模块 | 职责 |
|------|------|
| `lisp/lang/lean/init-lean.el` | 主 mode、eglot 注册、project 定位、ripgrep 符号搜索、UI 总入口 |
| `lisp/lang/lean/init-lean-eglot.el` | `$/lean/fileProgress` 通知、fringe/sideline 进度与 Flymake 兼容层 |
| `lisp/lang/lean/init-lean-infoview.el` | 官方 xwidget infoview 桥接（`C-c C-i`）|
| `lisp/lang/lean/lean4-infoview-bridge/` | Node.js HTTP bridge：转发 LSP、服务官方 React infoview |

**键位（lean-mode buffer）**

| 键 | 命令 |
|----|------|
| `C-c C-i` | 切换官方 xwidget infoview |
| `C-c C-r` | 重连 eglot |
| `C-c C-d` | 重刷文件依赖 |
| `C-c C-a` | eglot code actions |
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

TRAMP 远程 buffer：xwidget infoview 不可用（xwidget 需要本地 bridge）。

## 3. 调试

这套配置使用 `dape`。

入口：

- `SPC c j`
- `M-x dape`

调试期间还有 `hydra-dape-mode`。

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
  弹出/收起当前 popup `vterm`
- `C-c E`
  切换到下一个 popup `vterm`；`C-u C-c E` 新建一个
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

## 6. 远程与 TRAMP

### 设计目标

- 强功能优先
- SSH / TRAMP 不主动减配
- PATH、ControlMaster、session timeout 都已经配好

### 你应该怎么用

- 编辑远程文件：直接 `find-file` 打开 `/ssh:host:/path`
- 开交互式远程终端：优先 `M-x my/vterm-ssh`

`my/vterm-ssh` 会优先读：

- `~/.ssh/config`

所以最好把常用机器写成 Host 别名。

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

## 8. AI

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
  - `M-]`
  - `M-}`
  - 中文输入法下可用对应中文标点键，例如 `M-】` / `M-｝`

## 9. 如果 LSP 或远程不工作

优先检查：

1. 先开 `M-x my/language-server-doctor`
2. 看当前 buffer 的 route policy / active backend / executable
3. 远端 PATH 里是否真的有这些 server
4. `eglot` 或 `lsp-mode` 是否已经 attach
5. TRAMP 主机是否能正常登录

更详细的维护和排查见 [lsp-workflow.org](lsp-workflow.org) 和 [maintenance.md](maintenance.md)。
