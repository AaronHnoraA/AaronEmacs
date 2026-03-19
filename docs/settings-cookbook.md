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
- [lisp/init-text.el](../lisp/init-text.el)
- [lisp/init-auctex.el](../lisp/init-auctex.el)

重点变量：

- `my/font-body`
- `my/font-code`
- `my/font-title`
- `my/font-cn`
- `my/h-body`
- `my/h-code`
- `my/h-title`
- `my/scale-cn`

职责：

- `lisp/init-base.el`
  基础字体族、字号、中文 fontset 绑定、共享排版 helper。
- `lisp/init-org.el`
  Org 里什么时候启用 prose 排版。
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

## 4. 我要改 GPT 后端

文件：

- [lisp/init-gpt.el](../lisp/init-gpt.el)
- `var/mygpt.json`

通常只需要改：

- `var/mygpt.json`

现在这份 JSON 同时支持：

- 旧的单后端格式
- 新的多后端 `backends` 列表格式

常用入口：

- `M-x my/gptel-open-config`
- `SPC l l` / `C-c l l`

只有在你要换加载逻辑时才需要改 Elisp。

## 5. 我要增删 leader 键

文件：

- [lisp/init-evil.el](../lisp/init-evil.el)

你会看到：

- 全局 leader
- Org localleader
- Elisp localleader

改法：

1. 找到 `define-leader-key`
2. 在对应分组加命令
3. 如果是 mode 专属功能，优先放 localleader

## 6. 我要加一个新包

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

## 7. 我要改 snippet

文件：

- [snippets/](../snippets/)
- [lisp/init-snippets.el](../lisp/init-snippets.el)

入口：

- `C-c y n`
  新建 snippet
- `C-c y v`
  找 snippet 文件

## 8. 我要改补全行为

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

## 9. 我要改远程和终端行为

文件：

- [lisp/init-shell.el](../lisp/init-shell.el)
- [lisp/init-base.el](../lisp/init-base.el)

重点：

- `my/vterm-ssh`
- `my/ssh-config-hosts`
- TRAMP 相关 `setq`

## 10. 我要改项目管理行为

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

## 11. 我要调整 Dired / Dirvish

文件：

- [lisp/init-dired.el](../lisp/init-dired.el)
- [lisp/init-windows.el](../lisp/init-windows.el)

职责：

- `init-dired.el`
  基础 dired 行为
- `init-windows.el`
  `dirvish` 的 UI 和键位

## 12. 我要改 Org 重 UI

文件：

- [lisp/init-org.el](../lisp/init-org.el)
- [lisp/init-funcs.el](../lisp/init-funcs.el)

现在策略是：

- 默认不主动降载
- 图形界面下尽量全开

如果你以后想重新做降载，优先改：

- `my/rich-ui-buffer-p`
- `my/org-enable-*`
- `my/org-enable-jit-pretty-blocks`

## 13. 我要改运行时状态目录

文件：

- [lisp/init-base.el](../lisp/init-base.el)

重点变量：

- `my/state-dir`
- `my/backup-dir`
- `my/auto-save-dir`
- `my/lockfile-dir`

现在所有这些都写到 [var/](../var/)。
