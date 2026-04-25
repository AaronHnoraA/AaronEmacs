# Org Guide

这套配置的 Org 是重功能路线，不主动对远程和大文件降载。

如果你要看的不是 Emacs 里的按键和模块，而是 `~/HC/Org/` 这个个人知识库 / 站点源目录怎么组织、`daily/` 和 `roam/` 怎么分工、tag 怎么约定，另见 [org-kb-guide.md](org-kb-guide.md)。

## 1. 目录结构

在 [lisp/init-org.el](../lisp/init-org.el) 里默认是：

- `~/HC/Org/`
  主目录
- `~/HC/Org/roam/`
  org-roam
- `~/HC/Org/daily/`
  每日 capture
- `~/HC/Org/references/`
  参考文献和 bib

## 2. 核心入口

- `C-c a`
  `org-agenda`
- `C-c c`
  `org-capture`
- `C-c n`
  org-roam 前缀
- `C-c C-x v`
  手动刷新当前可视区域 LaTeX 预览

## 3. Capture 模板

当前默认模板在 [lisp/init-org.el](../lisp/init-org.el) 里，包括：

- `i`
  Idea
- `b`
  Inbox
- `m`
  Mail
- `n`
  Note
- `t`
  Meeting
- `u`
  Uni Task
- `l`
  Life Task

这些模板会落到 `daily/` 下不同子目录。

## 4. Agenda

### 基本逻辑

- 默认不再自动扫描 `my-org-root`
- `org-agenda-files` 初始为空
- 如果以后要重新启用 agenda，直接手动设置 `org-agenda-files`

### 常用命令

- `C-c a`
  打开 agenda

## 5. Org Roam

当前前缀是 `C-c n`：

- `C-c n f`
  `org-roam-node-find`
- `C-c n i`
  `org-roam-node-insert`
- `C-c n t`
  添加 tag
- `C-c n a`
  添加 alias
- `C-c n o`
  创建 `org-id`
- `C-c n l`
  切换 roam buffer

说明：

- `org-roam` 的“模板”来自 `org-roam-capture-templates`（见 [lisp/org/init-org-roam.el](../lisp/org/init-org-roam.el)），不走新建文件 `auto-insert` 模板系统。
- 另外这套配置里 `org` 的 `auto-insert` 默认是关闭的（不在 allowlist），避免新建 roam/capture 文件时发生模板叠加。

## 6. Org UI 增强

默认启用的增强包括：

- `mixed-pitch`
- `valign`
- `org-modern`
- `org-modern-indent`
- `org-appear`
- `org-fragtog`
- `olivetti`
- 自定义 special block overlay
- LaTeX 可见区域按需预览

现在这些增强默认不再因为远端或大文件自动关闭。

## 7. Org Localleader

在 Org buffer 里用 `SPC m`：

- `SPC m a`
  archive subtree
- `SPC m d`
  deadline
- `SPC m e`
  effort
- `SPC m n`
  roam insert
- `SPC m N`
  roam find
- `SPC m r`
  refile
- `SPC m s`
  schedule
- `SPC m t`
  todo
- `SPC m v`
  预览当前可视区域 LaTeX
- `SPC m z`
  从 BibTeX block 填充 Zotero 模板字段

## 8. 学术写作

### LaTeX

- Org 里启用了 `cdlatex`
- 可见区域 LaTeX 预览走自定义 `xelatex + dvisvgm` 异步 SVG 流程：当前窗口优先入队，窗口下方会比上方预取更多行，滚动产生的旧可视区域队列和旧自动渲染进程会自动回收
- 正在编辑的公式预渲染走独立 cache-only 管线：它不会被可视区域回收取消，必要时会让自动可视渲染让出并发槽；离开公式后只挂缓存或等待同一个预渲染结果，短延迟复查并补挂 overlay
- LaTeX 预览的滚动触发有短 debounce，并且只在 idle 后扫描可视区域，避免滚动手感被同步渲染拖慢
- 与 `RaTeX` 共存时，公式编辑期间会忽略预览 UI 引发的合成滚动事件，避免可视区域预览被误触发
- AUCTeX 和 `pdf-tools` 已配置
- AUCTeX 支持 `latexmk -pvc` 实时编译和 Sioyek 双向同步

### 引用

- `org-ref`
- `bibtex-completion`
- Zotero / MarginNote 自定义链接

### PDF

- `pdf-tools` 已启用
- AUCTeX 查看器走 Sioyek

## 9. 如果 Org 看起来不对

优先检查：

1. 字体是否齐全
2. `~/HC/Org/` 是否存在
3. `xelatex` / `dvisvgm` 是否可用
4. [tools/org-xdvisvgm-hires](../tools/org-xdvisvgm-hires) 是否有执行权限

## 10. 想改 Org 的入口

看 [settings-cookbook.md](settings-cookbook.md)：

- 改 Org 根目录
- 改 capture 模板
- 改 agenda 策略
- 改 org-roam 模板
- 改 heavy UI
