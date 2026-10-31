# Emacs 配置代码审计 2026-10

审计 `lisp/`、`early-init.el`、`init.el`，找 bug 和不必要的损耗。起点提交 `3ab3d425`。
这是一次性记录，不具权威性；结论已落到代码和对应文档。

## 覆盖范围

没有逐行读完全部 11 万行。实际分三层：

| 方式 | 范围 |
|------|------|
| 逐行阅读 | `early-init.el`、`init.el`、`init-modules.el`；`lisp/core/` 的 `aaron-base`、`aaron-emacs`、`aaron-lib`、`aaron-open`、`aaron-funcs`、`aaron-notify`、`aaron-patches`、`config`；`ui/tabbar`、`ui/modeline`、`ui/activity`（前 560 行）、`config/default`、`emacs/files`、`editor/editing`、`os/macos` |
| 定点阅读 | 全部全局 `post-command` / `pre-command` / `after-change` / `buffer-list-update` / `window-*-change` 钩子函数；Noema 活动快照；Treemacs follow；Dashboard 缓存；`var` 清理定时器 |
| 全量机械检查 | 217 个文件的字节编译警告；S 表达式分析（数字当布尔、字符串键用 `eq`/`assq`、破坏性调用丢弃返回值、格式串风险、匿名钩子、重复定时器）；完整启动后的契约检查（钩子参数、advice 签名、按键目标、transient 后缀、`:on-change`、`config-store` 孤儿键） |

未逐行阅读：`lisp/remote/`（已有 `remote-io-review-2026-07.md` 和大量测试）、`tools/lsp/`、
`noema/`、`lang/`、`core/general.el`（内化的第三方代码）。这些部分只经过机械检查和定点阅读。

## 已修复

实测数据来自这台机器。

**损耗**

- 启动时的 `.elc` 清扫递归 8 万个文件的 Node 目录：674 ms 降到 37 ms。
- 退役 Aaronnote 命令的清理在每次 savehist 保存时跑 `mapatoms`：每 5 分钟卡 107 ms，且已无可清内容。整体删除。
- `set-fontset-font` 不去重：每次换主题、开 frame、开 prose buffer 都让 fontset 永久增长并丢弃已实现的 fontset。现在每个 frame 字体只添加一次，prose 钩子不再重复。
- 居中标签页每次渲染重建图标字符串 O(k²) 次，排序 O(n²)。
- undo-tree 旧路径回退让每次访问远程文件多两次 stat；磁盘上已无旧格式历史。删除。
- gcmh 默认 800 KB 的空闲阈值让空闲 Emacs 约每 40 秒全堆 GC 一次（约 70 ms）。新增 `+gcmh-low-cons-threshold`，取 32 MB。
- `+dtrt-indent-enable-maybe` 逐级查 `.editorconfig` 时未预取，违反祖先标记规则。

**Bug**

- `window-buffer-change-functions` 的全局值传入 frame，只有 buffer-local 项才传 window。两个处理函数检查 `window-live-p`，因此从未经钩子运行过：显示高成本远程文件时的检查，以及浮动终端的 frame owner 清理。它们的测试直接传 window，掩盖了问题。
- `aaron-base` 重新定义了 `early-init` 的警告 advice，加载后警告不再进入 suppressed 日志。现在只保留一份，日志有上限。
- 启动后的恢复逻辑覆盖了 store 的 `gc-cons-percentage`，0.3 从未生效。
- `init.el` 重复设置无穷大 GC 阈值，`aaron/reload-init` 会把它留下。
- 新 client frame 的 PATH 刷新丢掉 `tools/`。
- `+rename-current-file` 把 buffer 命名为完整路径，并留下已修改标记。
- 节假日闰年判断把 `(% year 100)` 当布尔值。
- 中文缩放项用 `assq` 删除字符串键，永远删不掉。
- `aaron/font-reset-all` 调用不存在的 `font-cache-reset`。
- Performance Watch 的 `P` 在 `profiler` 未加载时指向未定义函数；Noema 前缀 `i` 同理。
- `etc/config-languages.el` 的 `flymake-fringe-indicator-position` 是孤儿项，未注册，不生效。

## 未处理

留给后续决定，均未改动：

- `aaron-font-math-symbols` 文档称“有序回退”，但循环 `prepend` 让列表最后一个已安装字体优先。修正会改变数学符号的字形，需要本人决定。
- `+sudo-file-path` 依据 `file-remote-p` 拼 TRAMP 路径，对 `/fs:` 名字不成立。
- `lean-sideline-window-size-h` 依赖当前 buffer，而窗口变化钩子不保证它；调整非选中的 Lean 窗口时可能不刷新 sideline。`lean-tests` 在基线上就跑不起来，无法验证修改。
- doctor 仍有 98 条 policy 命中，主要是 `global-set-key`（`config/mouse`、`ui/tabbar`）和 `file-remote-p`。
- 18 处匿名钩子 lambda，违反命名规则但无运行时影响。

## 量过之后决定不改

- `lisp/` 全部解释执行：`early-init` 每次启动删除 `.elc`，运行中的 Emacs 里 `+tab-line-format`、`remote-fs-file-name-handler` 等都是解释函数。字节编译 222 个文件后，批量启动约 4.3 s 降到约 3.9 s，`+activity-mode-line-text` 3000 次 80 ms 降到 69 ms。时间主要在 C 原语上，收益不足以抵消陈旧字节码的风险，现有设计保留。
- `font-family-list` 单次约 0.1 ms，不需要缓存。

## 流程教训

- `var/state-check/` 是共享目录。并发会话跑 `make state-baseline` 或 `make test-baseline` 会静默覆盖基线。本次用单独的 worktree 和自己的目录重建了比较。
- 配置自己的 `display-warning` advice 会吞掉字节编译警告，批量编译得到空结果不代表没有问题。
