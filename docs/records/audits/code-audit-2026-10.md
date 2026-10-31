# Emacs 配置代码审计 2026-10

审计 `lisp/`、`early-init.el`、`init.el`，找 bug 和不必要的损耗。起点提交 `3ab3d425`。
这是一次性记录，不具权威性；结论已落到代码和对应文档。

## 覆盖范围

`lisp/` 下已跟踪的 230 个文件（约 11.7 万行，不含 `vendor/kanagawa-themes` 主题）和 5 个未提交的新文件、`early-init.el`、`init.el`、`bootstrap.el`、`scripts/*.el` 已全部逐行读完。`site-lisp/`（Noema、Magent、gptel 等）和 `test/` 不在范围内，`etc/*.el` 是数据，只做了孤儿键检查。

第一阶段分三层：

| 方式 | 范围 |
|------|------|
| 逐行阅读 | `early-init.el`、`init.el`、`init-modules.el`；`lisp/core/` 的 `aaron-base`、`aaron-emacs`、`aaron-lib`、`aaron-open`、`aaron-funcs`、`aaron-notify`、`aaron-patches`、`config`；`ui/tabbar`、`ui/modeline`、`ui/activity`（前 560 行）、`config/default`、`emacs/files`、`editor/editing`、`os/macos` |
| 定点阅读 | 全部全局 `post-command` / `pre-command` / `after-change` / `buffer-list-update` / `window-*-change` 钩子函数；Noema 活动快照；Treemacs follow；Dashboard 缓存；`var` 清理定时器 |
| 全量机械检查 | 217 个文件的字节编译警告；S 表达式分析（数字当布尔、字符串键用 `eq`/`assq`、破坏性调用丢弃返回值、格式串风险、匿名钩子、重复定时器）；完整启动后的契约检查（钩子参数、advice 签名、按键目标、transient 后缀、`:on-change`、`config-store` 孤儿键） |

后续继续逐行阅读，已读完：`lisp/core/`（`general.el` 除外）、`ui/`、`emacs/`、`completion/`、`os/`、`app/`、`editor/`、`term/`、`checkers/`、`config/`、`lang/`，以及 `tools/` 的 `lsp`、`lsp-hub`、`lsp-ops`、`tree-sitter`、`project`。`tools/project` 读完但发现未处理。

之后读完其余 `tools/`、`noema/`、`remote/` 和 `core/general.el`。`tools/agent/` 读的是有另一会话未提交改动的工作树版本，其中 `manager.el`、`menu.el`、`tools/project/tools.el`、`ui/windows/actions.el`、`os/neomacs/trial.el` 尚未提交，读过但未改。

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

**第二批**

- `archive-mode` 接管了 `.tar`、`.tgz` 和所有压缩后缀，打开时报 "Buffer format not recognized"。
- Markdown 路由按子串 `md` 删除 `auto-mode-alist` 项，`.bat`、`.cmd` 失去 `bat-mode`；加载 `markdown-mode` 后路由也被顶掉。
- Lean 光标钩子在每条命令上走两遍项目根，无 infoview 时也一样；符号搜索在远程项目上跑到 client；非选中窗口的 sideline 不随尺寸刷新。
- `native-comp-speed` 用了旧变量名，从未生效；`native-comp-deferred-compilation` 是 JIT 开关的别名，已删除。
- LaTeX 宏文件里未闭合的 `[` 让解析器死循环；AUCTeX 工作流钩子在 LaTeX buffer 跑两遍。
- Bazel 跳转先展开文件名再读匹配数据，行列号被破坏。
- Compile Board、LSP Hub、Doctor 丢失看板公共按键；`language-server-menu` 的 `x` 绑定冲突，错误栈入口改为 `X`。
- 其余：Flymake 双启动、Lens 与装饰过滤按项布局窗口、tldr、Dired 逐行 stat、dir-locals 合并、模板占位、shell 哨兵、诊断面板刷新。

**Remote**

- 网关每个随机 ID 触发一次完整 GC；pipeline 的 connect timeout 未按截止时间计算；环境合并做了前缀而非精确匹配。
- `remote-backend-tramp-rpc` 的 git 探测在启动时跑 15 次，加备忘后 3 次（170 → 32 ms）。`+tramp-memoize` 不再缓存 nil，`project-current` 的备忘不再吞掉 prompt 参数。死代码 `+tramp-lsp-*` 删除。
- `remote-transfer-list-clear` 留下隐藏日志 buffer；IPv6 主机名未加方括号。
- tramp-rpc 加入 TRAMP 的几个操作没有路由契约，Doctor 在每个 target 报错。补上后，基线上 9 个 remote 测试失败全部修掉。

**Noema / Jupyter**

- Copilot 的 Noema 内联补全同步阻塞 Emacs，改为异步；UTF-16 换算 105 KB 文本从 27–89 ms 降到 1–2 ms。
- Dashboard 热力图启动时等 Node（307 → 1.8 ms），改为异步；note 记录按根缓存，解析改哈希查找。
- `notebook.el` 的正则在 Elisp 字符串里写成单反斜杠的 `\|`，实际是字面 `|`，bash/zsh/shell、javascript/node 的语言推断从未成立。
- kernel 事件对每个非 notebook buffer 算 truename；`json-serialize` 的 unibyte 结果直接插入显示 buffer，非 ASCII 成八进制。
- 删除一批未使用的 helper 和别名（`tools/jupyter`、`noema/bridge`、`noema/roam`）。

**general.el 与 Agent 层**

- `general--record-keybindings` 占 `general-define-key` 启动耗时的 68/97 ms，只服务 `general-describe-keybindings`，现在按需开启（97 → 33 ms）。同时修 `permanent-local` 写错变量、`:keymp`、`sybmol`。
- `+agent-render--visible` 是全局 `window-buffer-change-functions` 的第三个错收 window 的函数：隐藏时延迟的输出只在经 `+agent-acp-show-buffer` 打开时才刷出。
- `+agent-context--send` 的 `references` 被 `let*` 遮蔽，`+agent-context-clear-after-send` 对共享选区从不生效。
- `+agent-acp-trim-buffer` 把隐藏会话的 point 留在顶部，且二分时复制整个 buffer 约 18 次；改用 `position-bytes`，提示符不再可能被裁掉。
- 尝试看板在 ACP 事件内同步重绘，每成员解析两遍目录，现在合并到一次定时重绘。
- `bootstrap.el` 安装失败仍以 0 退出，`make install` 报成功；现在全部尝试后以 1 退出。

## 未处理

留给后续决定，均未改动：

- `aaron-font-math-symbols` 文档称“有序回退”，但循环 `prepend` 让列表最后一个已安装字体优先。修正会改变数学符号的字形，需要本人决定。
- `+sudo-file-path` 依据 `file-remote-p` 拼 TRAMP 路径，对 `/fs:` 名字不成立。
- `lean-sideline-window-size-h` 依赖当前 buffer，而窗口变化钩子不保证它；调整非选中的 Lean 窗口时可能不刷新 sideline。`lean-tests` 在基线上就跑不起来，无法验证修改。
- doctor 仍有 95 条 policy 命中，主要是 `global-set-key`（`config/mouse`、`ui/tabbar`）和 `file-remote-p`。
- 18 处匿名钩子 lambda，违反命名规则但无运行时影响。
- `aggressive-indent` 挂在不存在的 `elisp-mode-hook` 上，从未在 Elisp 启用；修正会改变编辑行为，需要本人决定。
- `use-package cc-mode` 的 `:custom` 全局设置 `tab-width` 和 `indent-tabs-mode`。
- `+pdf-view-auto-refresh-interval` 写入 buffer-local 的 `auto-revert-interval`，不生效。
- Lean `M-[` 向后逐字符发同步请求，文件头较长时可能卡顿。
- `lean-tests.el` 加载已不存在的文件，`lean-infoview-tests` 有 5 个 Eglot 时代的失败。
- `comp` 在启动时仍被加载：模块账本 advice `define-key` 这个原语，需要 trampoline。
- 网关 `target` placement 下 `emacs.eval` 在共享 target 上的可达性：调用方目前全在 client，无 token 设计需要决定。
- `tools/project` 残留（`+treemacs-normalize-flat-imenu-index`、`+project--remember-dir-visible-only` 的 advice-remove、匿名 `treemacs--follow` advice）：该文件当时由另一会话修改，未动。
- 进程退出后 `/tmp/.emacs-remote-ssh-UID-PID` 目录留存；`remote-backend-tramp-stream` 的 `cl-letf make-network-process` 会影响协商期间触发的定时器；`--attach-forward` 的 sentinel 包装在消费者替换 sentinel 后丢失。
- `remote-source` 过滤器对巨型帧是平方复杂度；git board 临时 buffer 没带 capsule。
- Jupyter：Contents 后端重命名到本地备份路径报错、null 日期；`+jupyter-target-command` 每次看板刷新同步探测两次；`jupyter/project.el` 用 comint `shell`，不合 eshell 约定；`runtime.el`、`project.el`、`server.el` 有 `(equal target "local")` 分支。
- `AGENT.md` 仍说 `lsp`（kernel-following），文件由另一会话持有。
- Agent：`+agent-session-capability-p` 取一个字段却构建完整快照；`+agent-acp--retire-transcript-workspace`、`--right-pane-p` 是旧布局迁移残留，`manager.el` 的 `fork`/`model`/`queue` 分支无调用；`+agent-acp--tab-line` 每次重绘都建 keymap 和排序全部 buffer；`help` 里的 `\\[+agent-session-list]` 未绑定键。
- `general.el` 大量上游代码（lispy/worf/lpy definer、key-chord）未使用，`general--delay` 每个未解析绑定一个 `after-load` 闭包。
- 终端选择器排除已打开窗口后，无法再选中可见终端把它移到新 frame。

## 结束时的测试

`scripts/test-all` 跑 134 个文件、2314 个测试。失败与起点对比：

- 起点已有、仍在：`init-activity`（2）、`init-diagnostics-entry`（1）、`init-neo-common`（1）、`init-performance`（1）、`lean-infoview`（5），以及跑不起来的 `lean-tests`、`portal-focus-gui-tests`（后者要 GUI）。
- 起点已有、已修：全部 `remote-*`、`init-noema`。
- 新出现、不是本次改动：`noema-agent-worker-keeps-one-hidden-named-buffer-per-session`。另一会话在 `acp.el` 工作树里把 agent buffer 名去掉了前导空格，已在工作树同步改测试预期（随那边的 `acp.el` 一起提交），204 个全过。
- 并行时偶发：`init-frame-buffers` 失败 1 次，单独连跑 4 次全过，未查明原因。

启动状态：在两个干净 worktree 里对比 `3ab3d425` 和 `272d4fe4`，388 行差异。大部分是 worktree 路径不同的噪音；其余是 `f900bd58` 迁入的 agent 设置与 `C-c A` 键位，以及本次预期内的改动（Compile Board 恢复公共按键、`+gcmh-low-cons-threshold`、删除 `+tramp-lsp-pipe-mode-servers` 和退役 savehist 钩子、不再加载 `lisp-mnt`）。

## 量过之后决定不改

- `lisp/` 全部解释执行：`early-init` 每次启动删除 `.elc`，运行中的 Emacs 里 `+tab-line-format`、`remote-fs-file-name-handler` 等都是解释函数。字节编译 222 个文件后，批量启动约 4.3 s 降到约 3.9 s，`+activity-mode-line-text` 3000 次 80 ms 降到 69 ms。时间主要在 C 原语上，收益不足以抵消陈旧字节码的风险，现有设计保留。
- `font-family-list` 单次约 0.1 ms，不需要缓存。

## 流程教训

- 这次 worktree 对比才发现 `fa205786` 手工拼的 blob 多一个 `)`，`main` 的干净 checkout 从那时起加载 `checkers/diagnostics` 失败，而工作树是对的，测试全跑在工作树上看不出来。部分提交必须在干净 checkout 上验证。已由 `272d4fe4` 修复。

- `var/state-check/` 是共享目录。并发会话跑 `make state-baseline` 或 `make test-baseline` 会静默覆盖基线。本次用单独的 worktree 和自己的目录重建了比较。
- 配置自己的 `display-warning` advice 会吞掉字节编译警告，批量编译得到空结果不代表没有问题。
