# Settings Cookbook

这份文档回答“我要改点什么，应该改哪里”。

## 1. 我要改主题

文件：

- [lisp/init-ui.el](../lisp/init-ui.el)

现在默认：

- `kanagawa-wave`

改法：

1. 换掉 `load-theme`
2. 如有主题包依赖，改对应 `use-package`

## 2. 我要改字体

文件：

- [lisp/init-base.el](../lisp/init-base.el)
- [lisp/init-org.el](../lisp/init-org.el)
- [lisp/org/init-org-ui.el](../lisp/org/init-org-ui.el)
- [lisp/init-text.el](../lisp/init-text.el)
- [lisp/init-auctex.el](../lisp/init-auctex.el)

重点变量：

- `my/font-body`
- `my/font-code`
- `my/font-title`
- `my/font-cn`
- `my/font-body-width`
- `my/h-body`
- `my/h-code`
- `my/h-title`
- `my/scale-cn`
- `my/prose-line-spacing`
- `my/org-prose-body-width`

职责：

- `lisp/init-base.el`
  基础字体族、字号、中文 fontset 绑定、共享排版 helper。
- `lisp/init-org.el`
  Org 里什么时候启用 prose 排版。
- `lisp/org/init-org-ui.el`
  Org prose 行距、单窗口阅读宽度、标题和 special block 视觉层。
- `lisp/init-text.el`
  Markdown 里什么时候启用 prose 排版。
- `lisp/init-auctex.el`
  LaTeX 里什么时候启用 prose 排版。

改完后可以执行：

- `M-x my/font-reset-all`

## 3. 我要改 Org 根目录

文件：

- [lisp/init-org.el](../lisp/init-org.el)

重点变量：

- `my-org-root`
- `my-org-roam-dir`
- `my-org-daily-dir`
- `my-org-notes-file`
- `my-org-diary-file`

如果你迁移目录，建议一起检查：

- capture 模板
- `org-roam-directory`
- bibliography 路径

## 4. 我要改 AI 助手配置

文件：

- [lisp/init-ai-ide.el](../lisp/init-ai-ide.el)

### Claude Code

变量：

- `claude-code-ide-cli-path`  — Claude CLI 可执行路径（默认 `/Users/hc/.local/bin/claude`）

常用入口：

- `C-c C-'` / `C-c a`  — 打开 claude-code-ide 菜单
- `F12` / `H-l`        — 同上（全局快捷键 / macOS Option 快捷键）

只有在你要换 CLI 路径或调整 MCP tools 时才需要改 Elisp。

### Codex CLI

变量：

- `codex-cli-executable`      — Codex 可执行文件名（默认 `"codex"`）
- `codex-cli-terminal-backend` — 终端后端（默认 `vterm`）
- 本地 override：Codex buffer 用底部普通窗口打开，高度约 18 行，可继续分屏

键位前缀 `C-c c`；参见文件顶部注释。

### 窗口切换 / 交换

文件：

- [lisp/init-windows.el](../lisp/init-windows.el)

现有快捷键：

- `M-o` — `ace-window`，快速选择目标窗口
- `M-S-o` — 交换当前窗口和另一个窗口
- `M-<left/right/up/down>` — `windmove`，按方向切换窗口

交换窗口的策略：

- 只有两个窗口时，直接和另一个窗口交换
- 多于两个窗口时，用 `ace-window` 选目标窗口再交换

## 5. 我要改默认打开方式

文件：

- [lisp/init-open.el](../lisp/init-open.el)
- [site-lisp/general.el/general.el](../site-lisp/general.el/general.el)

改默认 URL / 搜索 / 文件 / PDF 打开方式时，优先改 `lisp/init-open.el`
里的 `my/open-routes`。

`my/open-routes` 使用 `general.el` 提供的 `general-route-*` DSL helpers：
具体策略留在 `init-open.el`，通用的 route 读取、别名归一和菜单选择逻辑在
`site-lisp/general.el/general.el`。

常用值：

- `url` 的 `:default menu` 表示 `browse-url` 每次弹菜单
- `url` 的 `:menu-default xwidget` 表示菜单默认项是 `xwidget`
- `system` 表示 macOS `open` / Linux `xdg-open` / Windows shell open
- `pdf` 默认走 `system`，普通 `find-file` 打开 PDF 仍由 `pdf-tools` 处理

## 6. 我要增删 leader 键

文件：

- [lisp/init-evil.el](../lisp/init-evil.el)
- [lisp/init-funcs.el](../lisp/init-funcs.el)
- [site-lisp/general.el/general.el](../site-lisp/general.el/general.el)

你会看到：

- 全局 leader
- Org localleader
- Elisp localleader

当前按键绑定链路：

- `define-leader-key` 是配置里的 leader 入口，定义在 `lisp/init-evil.el`
- `my/evil-define-key` 是底层封装，定义在 `lisp/init-funcs.el`
- `general.el` 是 vendored 的按键绑定 DSL，放在 `site-lisp/general.el/`

`general.el` 的职责很窄：声明式定义快捷键。它不是 UI、补全、LSP 或包管理框架。这里引入它，主要是为了统一处理 Evil state、keymap、leader key 和批量绑定，尤其是符号 keymap，例如 `'global`。

这类绑定会走 `general-define-key`：

```elisp
(general-define-key
 :states 'normal
 :keymaps 'global
 "<leader>ff" #'find-file)
```

真实 keymap object 仍然优先用 Evil 自己的 `evil-define-key*`。所以日常加 leader 键时，不需要直接改 `general.el`，只需要改 `define-leader-key` 的绑定列表。

改法：

1. 找到 `define-leader-key`
2. 在对应分组加命令
3. 如果是 mode 专属功能，优先放 localleader

## 7. 我要加一个新包

### 普通包

直接在对应模块里：

```elisp
(use-package some-package
  :ensure t
  ...)
```

### VC 包

用：

```elisp
(my/package-ensure-vc 'some-package "https://github.com/owner/repo.git")
```

文件：

- [lisp/init-package-utils.el](../lisp/init-package-utils.el)

改完后执行：

```sh
make install
```

或者：

```sh
emacs --debug-init -q -l ./bootstrap.el
```

把锁文件更新掉。

## 7.1 我要改数据库客户端配置

文件：

- [lisp/init-clutch.el](../lisp/init-clutch.el)
- `etc/clutch-config.el`（本地私有配置，默认不进 Git）
- [etc/clutch-config.el.example](../etc/clutch-config.el.example)

职责：

- `lisp/init-clutch.el`
  负责 `clutch` 的 VC 包声明、懒安装、懒加载和入口命令。
- `etc/clutch-config.el`
  放你自己的 `clutch-connection-alist`、超时设置和连接别名。
- `etc/clutch-config.el.example`
  只是示例结构，方便对照。

入口：

- `M-x clutch-query-console`
- `SPC o q`

建议：

- 密码优先放 `auth-source` / `pass`，不要把 `:password` 直接写进主配置。
- 如果你只是改连接列表，通常只需要动 `etc/clutch-config.el`。

## 8. 我要改 snippet

文件：

- [snippets/](../snippets/)
- [lisp/init-snippets.el](../lisp/init-snippets.el)

入口：

- `C-c y n`
  新建 snippet
- `C-c y v`
  找 snippet 文件

## 9. 我要改补全行为

看这些文件：

- [lisp/init-minibuffer.el](../lisp/init-minibuffer.el)
- [lisp/init-search.el](../lisp/init-search.el)
- [lisp/init-lsp.el](../lisp/init-lsp.el)
- [lisp/init-lsp-ops.el](../lisp/init-lsp-ops.el)
- [lisp/init-lsp-tools.el](../lisp/init-lsp-tools.el)
- [lsp-workflow.org](lsp-workflow.org)

职责划分：

- `init-minibuffer.el`
  `vertico/orderless/consult/embark`
- `init-search.el`
  `ivy/counsel/swiper`
- `init-lsp.el`
  `company/company-box/eglot/lsp-mode` 核心路由
- `init-lsp-ops.el`
  organize imports / restart / shutdown / log / session 这类 backend-agnostic 操作
- `init-lsp-tools.el`
  Hub / Doctor / dispatch / runtime 调参

如果你是新增某个语言服务器映射：

- `lsp-mode` 例外路由，优先用 `my/register-lsp-mode-preference`
- 自定义 Eglot server，优先用 `my/register-eglot-server-program`

不要再到处散落手写 `add-to-list 'eglot-server-programs`。

## 10. 我要改远程和终端行为

文件：

- [lisp/init-shell.el](../lisp/init-shell.el)
- [lisp/init-base.el](../lisp/init-base.el)

重点：

- `my/vterm-ssh`
- `my/ssh-config-hosts`
- TRAMP 相关 `setq`

## 11. 我要改项目管理行为

文件：

- [lisp/init-project.el](../lisp/init-project.el)
- [lisp/init-evil.el](../lisp/init-evil.el)

重点：

- `my/project-search-paths`
- `my/project-dispatch`
- `my/project-open-workbench`
- `my/project-switch`
- `show-imenu`

职责：

- `init-project.el`
  项目工作流本体：Projectile / Perspective / Treemacs / transient / `show-imenu`
- `init-evil.el`
  `SPC p` 这一组项目快捷键，以及 `SPC c i`

如果你想改“项目切换后自动做什么”，优先看：

- `my/project-switch`
- `my/project-open-workbench`

如果你想改“自动发现项目从哪里找”，改：

- `my/project-search-paths`

## 12. 我要调整 Dired / Dirvish

文件：

- [lisp/init-dired.el](../lisp/init-dired.el)
- [lisp/init-windows.el](../lisp/init-windows.el)

职责：

- `init-dired.el`
  基础 dired 行为
- `init-windows.el`
  `dirvish` 的 UI 和键位

## 13. 我要改 Org 重 UI

文件：

- [lisp/init-org.el](../lisp/init-org.el)
- [lisp/init-funcs.el](../lisp/init-funcs.el)

现在策略是：

- 默认不做破坏体验的整体降载
- 图形界面下保留完整 UI，但让高频成本明确的功能按需或合并刷新
- `valign` 只在 Org buffer 已有表格或新插入表格时启用
- `org-appear` 在 point 和编辑状态未变化时跳过重复解析
- `org-roam` 侧边 buffer 跟随刷新走短 idle 合并，避免每个命令都重算
- LaTeX preview overlay 挂载、公式编辑预编译和可视区域抢占路径不作为降频对象

如果你以后想重新做降载，优先改：

- `my/rich-ui-buffer-p`
- `my/org-enable-*`
- `my/org-enable-jit-pretty-blocks`

## 14. 我要改运行时状态目录

文件：

- [lisp/init-base.el](../lisp/init-base.el)

重点变量：

- `my/state-dir`
- `my/backup-dir`
- `my/auto-save-dir`
- `my/lockfile-dir`

现在所有这些都写到 [var/](../var/)。

## 15. 我要改新建文件模板（auto-insert）

文件：

- [lisp/init-auto-insert.el](../lisp/init-auto-insert.el)
- [templates/](../templates/)

入口：

- `SPC f t`
  选择并切换当前 buffer 对应 kind 的模板（会用新模板替换当前 buffer 内容）
- `SPC f T`
  查看当前 buffer 命中的 kind、当前选择的模板、以及是否会自动插入

默认策略：

- `auto-insert-mode` 全局开，但只对 `my/template-auto-insert-enabled-kinds` 里的 kind 生效
- `org` 默认不在 allowlist 里（避免干扰 `org-capture`/`org-roam` 的 capture 模板）
- 模板存放在 `templates/<kind>/`；模板里的占位符支持 `{{date}}` / `{{title}}` / `{{file}}` / `{{author}}` / `{{cursor}}` 等

项目内按目录开关（`.dir-locals.el`）：

```elisp
((nil . ((my/template-auto-insert-enabled . t)
         (my/template-auto-insert-enabled-kinds . (c cc sh python js ts tex))
         (my/template-current-override . ((python . "module.py")
                                          (tex . "ctex-article.tex"))))))
```

说明：

- `my/template-current-override` 只接受“文件名”（不能带路径分隔符），指向 `templates/<kind>/` 下的模板文件
- 新建文件模板只保留这一套内置 `auto-insert`（已移除 Doom 那套 Yasnippet file-templates 的遗留实现，避免重复/分叉维护）
