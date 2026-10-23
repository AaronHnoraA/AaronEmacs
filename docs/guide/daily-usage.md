# Daily Usage

这份文档只讲高频操作，不讲配置原理。

## 1. 你每天最常用的入口

- macOS GUI 下：
  `Command = Meta (M-)`，`Option = Hyper (H-)`
- `M-c` / `M-v`
  系统剪贴板复制 / 粘贴
- `SPC`
  Evil leader，总入口。
- `<Esc>`
  编辑 buffer 中一次完成 Evil normal-state 恢复和搜索高亮清理；在未启用 Evil 的
  buffer 中走统一取消逻辑。Minibuffer、isearch、Portal 和浏览器仍保留各自的局部
  Escape 行为，不会把未处理的按键漏给 macOS 全屏。
- `SPC h K`
  在 Emacs 内打开本快捷键索引。
- `SPC SPC`
  `telescope` 统一搜索面板
- `M-x`
  原生 `M-x` + `amx` 历史排序
- `M-x command-hub` / `M-x keybinding-guide`
  按用途浏览主要命令 / 打开本快捷键索引；对应 `H-,` / `SPC h K`。
- `M-x mode-keybindings` / `SPC h m`
  查看当前 major mode 实际生效的按键；更适合查局部模式，完整文字索引仍用
  `keybinding-guide`。
- `M-x config-health` / `M-x config-compile`
  配置健康面板 / 编译面板。健康面板打开和 `g` 刷新只读取当前状态；
  `s`、`c`、`N` 明确运行 startup、byte、native 检查，`b` 跳到编译面板。
  对应菜单为 `config-health-menu` / `config-compile-menu`。
- `M-x project-workbench` / `M-x activity-board` / `M-x jupyter-board`
  项目工作台、后台活动板和 Jupyter 管理板的直达名称。Jupyter 板 `g` 只刷新，
  `s` 显式清理所选 target 的废弃连接文件。
- `M-x project-menu` / `workspace-menu` / `project-run-menu` / `task-menu` / `test-menu`
  项目、工作区、运行、构建和测试的命令菜单。
- `M-x noema-menu` / `noema-wiki-menu` / `noema-ai-menu`
  Noema 主菜单、Wiki 菜单和 AI 菜单。`H-o` 与 Noema 顶栏菜单打开主菜单；
  `SPC N m` 与 Markdown 中的 `C-c r m` 打开 Wiki 菜单。
- `M-x noema-reports-menu`
  同一菜单汇集 Wiki 网页报告总览和七种 Emacs 报告（Wanted、孤立页、孤立附件、
  死端页、无分类页、最多链接页、标签重叠）。主菜单 `!n`、Wiki 菜单 `D` 打开它，
  `t` 查看标签重叠；也可直接运行 `M-x noema-report-tag-overlap`。
  原 `!r` 和 Wiki 菜单 `!` 仍直接打开网页报告总览。Emacs 报告按 frame
  保留各自视图，`q` 回到原页面并释放临时报告；关闭 frame 也会清理其报告。
  批量重命名／删除 Roam 标签使用 Noema 编辑器的 Tools 面板，那里有标签建议、
  明确确认和修改结果；这里的标签重叠报告只读。
- `M-x noema-move-note` / `noema-menu` 的 `m`
  选一篇 Roam 笔记后，用 Emacs 原生文件名补全指定目标目录与文件名。Noema host
  一次完成移动和引用改写；已打开的 Emacs 源文件 buffer 跟随新路径。若该笔记在
  一个 Noema 编辑页打开，该页先保存草稿，再移动并打开新路径；同时在多个编辑页
  打开时，先关闭多余页面。
- `M-x noema-trash-note` / `noema-menu` 的 `!d`
  在当前 Noema 编辑页输入 `TRASH` 确认；页面先保存当前草稿，再把文件移到可恢复的
  废纸篓。成功后回到这个页面访问过的上一篇有效笔记；没有上一篇时打开 Wiki 首页。
  若移动期间又产生编辑，页面会保留这些内容为未命名草稿，并提示先复制内容。
  同一笔记的其他 Noema 页面也会收到删除事件：干净页面返回上一篇，未保存的页面
  保留草稿并断开旧路径。原生 Emacs buffer 若有修改会变为未命名草稿；干净的
  buffer 则关闭，窗口返回上一页；无可返回页时打开项目目录或关闭多余窗口。
- `M-x project-run` / `project-rerun` / `task-run` / `task-build`
  直接选运行 profile、重跑本 frame 上次 profile、选项目任务或构建；任务和构建
  也有 `task-rerun` / `task-build-rerun`。运行菜单只列出实际存在的脚本或目标。
- `M-x bazel-target-menu`（或 `SPC r z`）
  在当前 target 查询 Bazel label，并在发起操作的 frame 运行、插入或跳转；
  原生 `bazel-build` / `bazel-test` / `bazel-run` 及 Bazel mode 键位仍可用。
- `M-x test-nearest` / `test-file` / `test-project` / `test-rerun`
  直接运行附近、当前文件、当前项目测试，或重跑上次成功启动的测试命令。
- `M-x project-add-to-workbench` / `project-discover-in-directory` / `project-remove-from-workbench`
  项目工作台的登记、批量发现与完整移除；原生 `project.el` 的独立项目列表仍可用。
- `M-x git-menu` / `language-server-menu` / `debug-menu` / `file-menu` / `editing-menu`
  Git、语言服务器、调试、文件和编辑的命令菜单；`jupyter-board-menu` 打开
  Jupyter 板上的完整动作菜单。菜单直接使用可搜索名称；内部具体动作仍由各领域模块实现。
- `M-x copy-as-format-menu`（或 `editing-menu` 中按 `c`）
  选格式复制当前选区；没有选区时复制整篇。菜单只影响本次复制，不更改
  `copy-as-format` 原生命令的默认格式，也不移动光标或选区。
- `M-x macos-tools-menu`（或 `SPC a m`）
  打开 macOS 工具菜单；`macos-pick-color` 复制颜色，带前缀参数时插入；
  `macos-open-at-point` 用系统应用打开光标处的链接或文件。
- `M-x mouse-actions-menu`（或外设映射的 `H-C-M-m`）
  打开可点击的常用动作板。代码和文字的右键菜单按所在位置提供动作；
  点击 mode line 的 Git 状态段打开 Git 菜单，快速切换窗口后也可立即再次打开。
- `M-x diagnostics-menu` / `code-actions-menu`
  诊断菜单汇集 Flymake 项目列表、当前 buffer 面板和筛选跳转；代码动作菜单汇集
  LSP 编辑、符号查看和构建。对应 `SPC c ?` / `SPC c .`、`H-d` / `H-e`。
- `M-x file-copy-paths` / `file-copy-parent-paths` / `dired-selected-size` / `dired-selection`
  `file-menu` 的 `o` 打开文件、`O` 在新 frame 打开，均使用现有文件入口；
  菜单中还有路径和 Dired 动作。`SPC f .` 与主命令面板的“files”打开同一菜单。
  选择项临时列表按 `q` 回到原目录并自动释放。
  图片裁剪用 `M-x image-crop`，也在编辑菜单中。
- `M-x telescope`
  `telescope` 统一搜索面板
- `C-x C-f`
  文件名补全沿用 `find-file` + `vertico-directory`；`Enter` 在当前窗口打开，
  `Shift+Enter` 在右侧分屏打开，`Cmd+Enter` 选择已有文件时在下方分屏打开；
  输入新文件名时，`Cmd+Enter` 按当前输入的名字精确新建，不选择相似候选。
  `Cmd+Shift+Enter` 用当前输入的文件名在新 Emacs 窗口打开（包括尚未创建的文件）。
  若候选与输入相似，`Ctrl+Enter` 可按当前输入的文件名打开。
  `SPC f f`、`H-f`、`M-x file-open` 使用同一提示；`M-x file-open-new-frame`
  可直接打开新窗口的文件提示。从 `/fs:` buffer 输入本机绝对路径（例如
  `/Users/hc/...`）会打开本机文件；相对路径仍在当前 target 查找。要打开
  另一台机器上的绝对路径，请输入 `/fs:目标ID:/路径`。`M-x find-file`
  仍保留 Emacs 原生命令的路径语义。
- `C-x b`
  `consult-buffer`：Buffer 候选先列当前 frame 与 workspace 中访问过的 buffer，
  包括在该 frame 埋藏的 buffer；提示中按 `<` 再按 `o` 可查其他 frame/workspace 的
  buffer。`Enter` 在当前窗口打开，`Shift+Enter` 在右侧分屏打开，`Cmd+Enter`
  在下方分屏打开；最近文件、bookmark 等 Consult 来源同样适用。
  `H-b`、`SPC b b` 使用同一入口。
- `C-s`
  `consult-line`
- `C-c p`
  Projectile 前缀
- `C-c p g`
  `consult-ripgrep`
- `C-x g`
  Magit
- `M-\``
  `my/portal-popup-toggle`
- `C-c e`
  切换全局 popup 的当前 tab；首次打开或新建终端时，从最近聚焦的项目和 target 启动
- `C-c C-e`
  智能弹出或收回当前 popup `portal`
- `C-c E`
  切换到下一个 popup `portal`，`C-u C-c E` 新建一个
- `C-c M-E`
  新建 popup `portal`
- `C-c M-e`
  切换当前 popup `portal` 的固定状态
- `C-c w p` / `C-c w P`
  把当前普通 buffer 移进 popup / 把当前 popup buffer 移到普通窗口；需要选窗口时显示
  `ace-window` 编号。顶部 `+term` 的右键菜单也有这两个动作
- `C-\``
  `popper-toggle`
- `C-x w d` / `C-u C-x w d`
  切换当前窗口的软 / 强 dedicated 状态；Doom modeline 显示 `d` / `D`
- `F1` / `F2` / `F3` / `F4`
  `help-command` / `telescope` / 项目工作台 / 项目 `ripgrep`
- `F5` / `F6` / `F7` / `F8`
  运行 profile / 测试菜单 / 调试菜单 / `olivetti-mode`
- `F9` / `F10`
  Noema 日程 / popup `portal`

### macOS Option `H-`

- `H-,` / `H-x`
  Hyper 管理菜单 / `telescope`
- `H-f` / `H-F` / `H-b` / `H-B`
  打开文件 / 其他窗口打开文件 / 切 buffer / 其他窗口切 buffer
- `H-r` / `H-s` / `H-g` / `H-t`
  最近文件 / 当前 buffer 搜索 / 项目 ripgrep / `telescope`
- `H-p` / `H-P` / `H-R` / `H-T`
  项目菜单 / workspace 菜单 / run profile 菜单 / test 菜单
- `H-m` / `H-a`
  `magit-status` / Noema 日程
- `H-h` / `H-H` / `H-z` / `H-Z`
  help / health 菜单 / zoxide 跳目录 / 当前文件目录
- `H-e` / `H-E` / `H-d` / `H-D`
  code menu / compile menu / diagnostics menu / debug profile 菜单
- `H-i` / `H-u` / `H-j` / `H-n` / `H-N`
  `show-imenu` / language server 菜单 / 调试菜单 / 最近测试 / output 菜单
- `H-\`` / `H-q` / `H-Q` / `H-w`
  `popper-toggle` / 关闭当前 buffer / 退出 Emacs / 关当前 frame；若只剩一个 frame，`H-w` 埋藏当前 buffer
- `H-0` / `H-1` / `H-2` / `H-3`
  关当前窗口 / 单窗口切换 / 上下分屏 / 左右分屏
- `H-o` / `H-O` / `H-k` / `H-K`
  Noema 全功能 hub (见下) / 上方开新行 / 向下复制当前行或区域 / 向上复制当前行或区域
- `H-<up>` / `H-<down>` / `H--` / `H-=`
  上移 / 下移当前行或区域 / 收缩选择 / 扩大选择
- `H-;` / `H-'` / `H-[` / `H-]` / `H-/`
  注释切换 / 多光标按行 / 上一个相同项 / 下一个相同项 / 全选相同项
- `H-X` / `H-c` / `H-v`
  剪切 / 复制 / 粘贴
- `H-<tab>`
  切换当前折叠（Org 标题、tree-sitter 折叠、hideshow 折叠统一走这个入口）

`H-x` 现在直接打开 `telescope`，和 `SPC SPC` / `H-t` / `F2` 是同一个入口。
普通 `M-x` 仍走默认 `amx` 行为。

`H-,` 内部按键按用途分组：board 入口用 `c/g/L/j/a/h`，菜单入口用
`C/p/w/r/k/t/o`，其中 `C` 打开原生 Customize 设置菜单；代码和运维入口用 `./e/d/D/u/x`，其中 `x` 打开
`telescope`；其他工具用
`K/G/f/i/m/s/n/R/P`，其中 `K` 查看当前模式实际按键、`G` 打开完整键位指南，
`f` 是文件菜单，`i` 是编辑菜单。

打开 `.md`、`.markdown` 或 `README.md` 时，Emacs 会按当前 Markdown 打开设置
交给 Noema Web/xwidget，页面成功打开后才关闭临时 Markdown buffer。等待 Noema
启动时，占位页按 `g` 重试、`e` 改用 Emacs 编辑原文件、`q` 关闭；超时后仍可
重试，选择原生编辑会取消旧的 Noema 打开请求。`M-x markdown-edit-raw` 也可直接
进入原生编辑。使用 Noema 页面时，Markdown
编辑、保存、文件树和 graph 都在页面内完成；Emacs 保留粗粒度 bridge 命令。

`H-o` 打开 Noema 全功能 hub（单页 Transient）：

`SPC N` 是 Noema 的 leader 分组：`m` 菜单、`a` 日程、`d` 文件、`v` Git、
`S` Wiki 索引状态、`e` 原始 Markdown、`o` 打开当前笔记。运行和调试继续使用
`SPC r`：`r R` 重跑上个运行 profile，`r V` 恢复状态快照，`r d` 调试 profile，
`r o` / `M-x output-menu` 输出菜单；`r O` / `M-x output-switch` 打开本 frame
最近一份仍在的输出。关闭最新输出后，会找回本 frame 上一份输出。

| 分组 | 常用键 |
|------|--------|
| **Note (web)** | `o` 打开当前, `O` 选文件, `s` 保存, `!d` 移到废纸篓, `r` 刷新, `!f` 手动从磁盘重载, `%` AI/agent 子菜单, `f` 聚焦, `e` Esc/normal, `v` 切换源码视图, `R` Emacs 原始编辑 |
| **Find/Browse** | `j` 查找笔记, `/` 搜索（支持 `intitle:` `incategory:` `linksto:` 操作符）, `l` 最近, `.` 跟随链接, `b` 反向链接, `x` 相关, `G` 跳转定义 |
| **Insert** | `i` roam 链接, `I` TOC 链接, `t` tag id, `T` tag-id 链接, `w` 复制链接到此处, `c` note-code |
| **Knowledge** | `h` Wiki 首页, `n` 新页面, `d` 日记, `a` 标签, `C` 命名空间, `g` 图谱, `k` 任务, `A` 日程, `L` 日程日志, `F` 当前文件任务, `M` 仓库管理 |
| **Wiki pages** | `!r` 网页报告总览, `!n` 详细报告菜单, `!w` Wanted Pages, `!p` 所有页面, `!s` 同步 |
| **Index / Host** | `y` 增量刷新, `Z` 原子重建, `S` 索引状态, `P` 暂停/恢复, `H` 运行状态, `D` dired, `m` 移动笔记, `X` magit, `q` 停止服务 |
| **Format (web)** | `1`–`5` 粗/斜/行内代码/高亮/删除线, `6`–`9` 引用/列表/任务, `0` 代码块, `$` 行内公式, `E` 数学块, `p` 标题菜单, `z` 表格, `u` 目录, `U`/`Y` 撤销/重做, `V` 粘贴；`^`/`_` 上下标, `N` 脚注, `K` 修订, `@` 属性, `[`/`]` 移块 |

**Wiki 搜索操作符**（`/` 搜索时可混用）：
- `intitle:关键词` 或 `title:关键词` — 仅匹配标题
- `incategory:qc/algorithms` 或 `tag:qc` — 按嵌套标签/分类过滤
- `linksto:slug` — 链接到指定笔记的笔记（逆向链接 as 搜索）
- 不带前缀的词 → 全文搜索（与原来行为一致）

**Special pages 功能说明**（MediaWiki 对应）：
- `!w` Wanted pages — 被链接但尚不存在的笔记；点击行直接进入新建流程
- `!o` Orphaned pages — 没有任何入链的笔记（日记除外）
- `!d` Dead-end pages — 没有任何出链的笔记
- `!u` Uncategorized — 没有标签的笔记
- `!h` Most-linked hubs — 按入链数量排序的枢纽笔记

`M` 管理仪表板（MediaWiki Special:Statistics）内嵌所有统计数据 + 快捷进入各 Special page + Tag 工具（重命名/删除/重叠分析）+ Move note。

Lean 4 buffer 里 `C-c C-i` 打开右侧官方 xwidget infoview。
目标、hypotheses、诊断、trace、Try this、code actions 等交互都走
`lisp/lang/lean/lean4-infoview-bridge/` 里的官方 React infoview bridge。
Lean 服务器挂掉时 lsp-mode 会自动重启，infoview 也会自己指到新的 proxy；
`C-c i r` 手动重启一整套（proxy + `lake serve` + 页面）。

`F` 当前文件任务跳转直接解析缓冲区中的原生任务，包含尚未保存的修改；
代码示例和普通 `TODO` 文字不进入任务列表。查询异步执行，不为跳转扫描整库
或其他项目。等待期间改动/切走缓冲区时请重新调用 `F`。`k` 的 Roam 任务列表
与 Agenda 共用已进入的 scopes；完成、元数据和依赖修改也经过同一个版本检查。

Noema 任务使用 `@@todo(state) [text] {key: value}`，例如
`@@todo(doing) [Write proof] {prio: A, ddl: 2026-05-20, repeat: +1w}`。
原生 Agenda 和托管 Web 共用 scope 索引与任务服务，修改写回原始 Markdown
命令或 `.noema` WorkNode 元数据。`M-x noema-agenda`、`F9`、`H-a` 或 dispatch `A`
打开原生 Agenda；`M-x noema-agenda-web` 打开含 Gantt 等视图的 Web Agenda。
原生 `c` 和 Web New todo 共用 Task / Deadline / Appointment 模板；`C-u c`
可改目标 Markdown 文件。保存失败保留输入，原生再按 `c` 继续修改。
`my/noema-agenda-capture-templates` 可配置字段、默认值和按日期命名的文件，
修改后重启 Noema host；配置示例见 [Agenda 文档](../noema/guide/agenda.md#shared-capture-templates)。
项目明确进入后才扫描；切到无项目关联的 Perspective，或使用项目菜单 `l` /
`M-x my/project-leave`，释放项目监听和缓存，知识库继续常驻。
原生 `I/O` 开始/停止计时，`v k` 查看报表和待回写记录，`R/K` 处理待回写冲突。
Apple 集成先用 `my/noema-agenda-apple-enable` 启用相应类型，再用原生 `P`
或 Web 的 Promote 显式提升任务；Calendar 需要开始/结束时间（原生 `s/E`）。
`v a` / Web Global attention 查看全局关注，刷新不会激活未进入的项目；
`RET` 才进入源项目。个人 Apple 数据和跨设备同步尚未完成实机验证。
完整语法和 view-model 见
[`agenda.md`](../noema/guide/agenda.md)。

Graph 搜索框支持全文词和
`tag:` / `alias:` / `path:` / `title:` 过滤，并会提示 tag / alias / path 等候选。
本地 graph xwidget buffer 里 `M-w` 会 kill graph buffer 并关掉 graph websocket。

### Noema 页面里的 Emacs 按键

Noema 默认由 Portal Web 承载，与 Portal 终端共用 `portal-input`：按键穿透、
`Cmd` 到 Emacs `Meta` 的转换，以及切换窗口后的自动聚焦由同一层处理。
Noema 只补充编辑器命令映射。顶栏的返回、前进、刷新、编辑器聚焦和菜单按钮
属于 Emacs。`xwidget` 仍可显式选择；以下专门说明它的旧键盘桥接行为。

在 xwidget 后端，所有 Noema 页面（md 编辑器、Jupyter output、Wiki、Agenda、Config、Slides）都把 `H-`（Option）
组合键、`C-x` / `C-c` 前缀序列、`C-g`，以及 `M-x` / `M-w` / `M-W` / `M-q` / `M-o` 交给 Emacs；
这些键在 CodeMirror 自己的按键表之前被截获，所以 `C-c C-e` 不会再同时把光标移到行尾，
`H-u` 也不会执行两次。`Cmd`+方向键（`M-<left>` 等）也交给 Emacs，走 windmove
直接切到相邻窗口，和普通 buffer 里一样；md 里行首/行尾请用 `0`/`$` 或 `Home`/`End`，
`Shift+Cmd+方向键` 和 `Option+方向键` 仍是页面里的选择与按词移动。

从页面触发的 Emacs 命令执行完后，键盘焦点跟着命令的结果走：它打开或切换了哪个普通
Emacs 窗口（agent 会话、compose、Treemacs、portal、源 buffer……），就选中那个窗口，
不会回到页面编辑器。什么都没打开的命令（`M-q` 回答 n、取消的 `M-x`、只输出消息）把键盘
还给页面。命令返回后 2 秒内才由进程弹出的窗口（启动 agent、终端等）同样会接过键盘，
但前提是你还停在原页面没动。`C-u` 前缀、minibuffer 输入和 transient 菜单都会等它们结束再判断。页面发起的
`C-c A v/@` 这类发送，最后会停在 agent 会话的输入处。
Emacs 接管键盘时（页面里触发的命令打开了 portal、agent、minibuffer，或 `Cmd`+方向键
移走），页面会把 macOS 的原生键盘焦点交还给 Emacs，之后所有按键——前缀序列
`C-x 3`、回车、方向键、中文输入法——都直接、按顺序进 Emacs，不再经过页面转发，
也没有按键特判。页面没有原生焦点时，macOS 仍会把 Ctrl/Cmd 组合键和方向键先给
页面看一眼；每个 Noema 页面都会原样放行给 Emacs，不会截成前缀或移动页面光标。
用键盘（windmove / ace-window）回到 Noema pane 时，Emacs 把按键转交给页面，可以
正常移动和编辑；这时用不了输入法，要输入中文先在页面里点一下，页面即取回原生键盘。

xwidget buffer 本身没有正文，所以作用于“当前 buffer 文本”的常用命令会被重定向到
页面上的等价操作（`my/noema-keys-mode-map`）：

| 键 / 命令 | 在 Noema 页面里 |
|-----------|-----------------|
| `H-i`（`show-imenu`） | 和其他 buffer 一样打开 Treemacs 大纲，跟随页面的笔记；点击标题时页面跳到该标题，不打开原始 Markdown |
| `imenu` / `consult-imenu` / `consult-outline` | 在 minibuffer 里选标题并跳转 |
| `H-s`、`consult-line`、`isearch` | 打开页面查找栏 |
| `H-c` / `M-c`、`H-X`、`H-v` / `M-v` | 复制 / 剪切 / 粘贴页面选区（含 Jupyter 输出选区） |
| `C-x h` | 全选笔记 |
| `C-x C-s` | 保存笔记 |
| `C-x u`、`undo-redo` | 页面 undo / redo |
| `refresh-file`、`revert-buffer`、`H-o !f` | `my/noema-refresh-file`：从磁盘重载 |

`M-x refresh-file` 在任何能 revert 的 buffer 里都是“从磁盘重载”：Noema 页面走
`my/noema-refresh-file`，`.noema` 笔记本重新投影 JuText，`*Noema DAG*` 重载源笔记本
并重绘；Lisp/C/shell/SCSS 仍是原来的格式化并保存。
Emacs 内的 Claude、Codex 等 ACP agent 完成一轮时，会检查已打开的 Noema Markdown
页面及可见、最近的 JuText 文件；JuText 的未修改 buffer 会接收已有区块的外部正文编辑，
有未保存编辑时保留本地草稿并合并运行输出。无需手动刷新，也不按时间轮询。
Markdown 页面先检查文件时间戳；
笔记库另有共享目录 watch 接收文件变更。没有未保存修改且文件确实更新的页面会自动
从磁盘重载；有草稿时保留页面内容并提示冲突。Emacs 保存同一个 Markdown
文件（例如接受 gptel rewrite）也会通知页面。`my/noema-refresh-file` 可在漏掉文件事件时
手动重载；它不会先把页面草稿写回磁盘，有未保存修改时会拒绝，`C-u` 则丢弃草稿后重载。

普通本地文件及远程文件 buffer 在 agent 写回磁盘后会自动刷新：支持推送通知的路由
即时接收文件事件，其余远程路由在 ACP 回合结束时对可见及最近的文件检查一次，
更早的隐藏 buffer 再次显示时补查一次；
已主动断开的远程目标不会被自动重连。若你同时有未保存编辑，
Emacs 会保留它们并提示冲突；运行 `M-x my/auto-revert-resolve-current-buffer` 可选择
Ediff 合并、接受磁盘版本或保留本地版本并覆盖磁盘。

Jupyter 输出（`@@cell` 的 Output 区域和右侧 Jupyter Output 页面）可以直接拖选文字，
`M-c` 复制选区；Output 工具栏的 **Copy** 在没有选区时复制整段纯文本输出，
Output 页面右键菜单也有 Copy / Copy Output。

Noema 编辑器的 xwidget window 使用 Emacs 原生 chrome：顶部铅笔按钮集中提供
Page、Agenda、Graph、Tools、Source、Save，点击后仍调用原 Web 面板和保存逻辑；
Vim mode、只读和全文/选区/本节字数显示在 Web 编辑区右上角的小浮窗，
Emacs mode-line 保持原样。Opening/Saved/Edited 等日常状态静默；LaTeX 进度、明确操作
结果和错误等关键反馈经过去重与短间隔合并后进入 Emacs echo，error 立即显示；
`my/noema-echo-severity` 可配置为仅 error、warning + error 或完全关闭。这个布局只应用于
Noema 自己的 xwidget buffer，不改变普通网页的 xwidget 控制栏。

编辑区输入 `/`（或中文输入法的 `、`）打开快速插入菜单；`/` 紧跟中日韩文字时也会触发，
`、` 仍只在行首或空白后触发。筛选支持中文名、全拼和首字母（如 `bt` 标题、`wxlb`
无序列表、`gs` 公式、`fg` 分割线）；分割线总会在上方留空行，避免把上一行变成标题。

Noema 的 Emacs 原生 roam buffer（Agenda、Tasks、TOC、Backlinks、Related、
Management、DB Status、note list 和 Roam Selector）使用统一的紧凑 workbench UI：
header-line 显示当前视图状态，正文使用工具栏、分组、状态徽章和可点击行。通用按键为
`g` 刷新、`q` 关闭、`RET` 打开当前行、`j` / `k` 或 `n` / `p` 上下移动，
`TAB` / `S-TAB` 在工具栏按钮间移动。Roam Selector 另外保留 `/` / `s` 搜索、
`g` 回根目录、`.` 回当前 note context、`u` / `^` 上一级、`r` 刷新和 `i`
直接插入当前目标。这些是 Emacs buffer 的界面和按键，不影响 Noema Web UI。

`M-x roam-new-note`、`C-c r n` / Roam 菜单里的 `Create node` 打开
当前 frame 的新建面板（首个名为 `*roam-new-node*`），默认创建可链接的 Roam node。
`M-x markdown-new-document` 打开同一面板，默认创建普通 Markdown，路径从当前
笔记目录开始。先在 `CREATE AS` 行按 `RET` 或 `t` 选择两种文档类型，再按 `T`
挑该类型的模板；`c` 创建，`R` 重置。Title、Save path、Render kind 和 Tags
可以直接输入。`RENDER KIND` 控制 Noema 页面的呈现方式，与是否加入 Roam 图无关。
同一 frame、同一类型和目录的命令重复触发时会回到未提交的表单，不清掉已输入内容；
另一 frame 有独立草稿。`R` 只重置当前文档类型，`q` 取消并释放本 frame 的表单，
关闭 frame 也清理它；随后再次打开会得到新草稿。刷新模板目录会保留正在
输入的字段，并重新检查当前模板是否仍适用于该类型。
创建成功后按 Markdown 的现有打开设置进入 Emacs 或 Noema；Noema 冷启动时会保留
文件占位页等待页面就绪。如果文件已经创建但打开报错，表单改为“已创建”状态：`c` 只重试
打开同一文件，不会再运行创建；`R` 开始新草稿，`q` 关闭表单，均保留已创建的文件。
创建实际走
Noema runtime，所以默认值、路径校验、meta、模板变量和 tabstop 展开逻辑保持一致。
Markdown 模板统一存放在 `templates/noema/`，供 Emacs 启动的 Noema 与
Roam Node 共用。`roam` 是可链接的知识节点：带稳定 ID、参与 Roam 图；
`regular` 是普通文档：不分配 Roam ID，meta 中写入 `roam: off`。
已有的普通 Markdown 即使写了 `#+begin meta` 或使用 Noema 语法，只要没有
显式 `id`，就不会进入 Roam 图；`roam: off` 会显式排除已有 ID 的文档。
选模板时只显示对应类型：Roam 有通用笔记、原子想法、概念、论断与证据、
文献阅读、来源卡片、研究问题、实验、人物、主题地图、决策、日记和周回顾；
普通 Markdown 有空白文档、会议纪要、项目简报、任务计划、报告、操作指南、
规格说明、检查清单、实验日志、文章草稿和项目 README。
模板列表从运行时读取；运行时不可用时不会显示虚构模板名。
Tags 在面板中显示为 `#tag`，创建前按 runtime 规则去掉显示用 `#`、
大小写去重并排序，保证面板、payload 和最终 meta 一致。所有新建 node 的 meta 都会带
一个空的嵌套 `summary` block，可直接在 Abstract
或 Properties 中编辑；模板自带 meta 时也会自动补齐，不需要每个模板重复声明。

Noema 页面顶栏的窗口菜单(布局图标)第一级里,`Noema: Split right` / `Split below` 下面有
`Noema: Split to new frame`:和分屏一样为当前笔记开一个独立的可编辑 pane,只是放进一个新
frame 里,原页面留在原处(`my/noema-open-current-note-split-frame`)。

### Noema Slides

在 meta 中设置 `kind: slides` 后，Noema 默认进入 **Reveal** 展示视图。每个一级标题
（`# Title`）开始一张新 slide；二级标题及以下、公式、图片、org-env 和手写 HTML 都留在当前页，
代码围栏里的 `#` 不会分页。meta 与第一个一级标题前的内容不显示为 slide。Reveal 负责 16:9
画布、缩放、动画、fragment 和翻页；Noema 负责把每一页 Markdown 先渲染成 HTML，因此
两边不会有第二套 Markdown 解释器。

一级标题 `#` 默认向右分页；其下的二级标题 `##` 自动成为纵向页。← / → 在一级标题之间移动，
↑ / ↓ 在同一一级标题的 stack 内移动。一级、二级标题都直接在 slide 内容中渲染；展示页不再
额外绘制左侧目录、顶部标题栏或底部进度线。旧的 `@@slides(vertical) []` 标记仍兼容，但新笔记不再需要它。
若二级标题带现有的 `<!-- omit in toc -->` 标记，它不会建立纵向页，而会作为当前 slide 的普通
二级标题继续由 Noema renderer 渲染。

`M-/` / `Cmd-/` 在 slides note 中切换 **Reveal 展示** 与完整、连续的 **Noema 普通笔记页**；
两个视图各自保存位置，不做鼠标、光标或选区同步。铅笔 Tools 中的
**Slides theme** 可即时切换并记住亮色/暗色展示，**Source view** 仍能进入真正的 Markdown 源码。
演示时左下角胶囊改为整块亮暗主题开关，右侧写作统计隐藏；回到编辑后该胶囊恢复为
Vim 模式状态与 Tools 入口。普通笔记的 `M-/` 仍是 Source 切换。

普通、非 slides 的 Markdown 笔记在铅笔 Tools 中提供 **Slide view**：它在新页面临时把当前
Markdown 展示成基础只读演示；没有标题时整篇作为一页。它与 slides note 共用同一套分页、
Reveal 初始化、重建和销毁管线，仅关闭交互扩展。该页面不加载 Jupyter cell，也不注入
`.slides` JavaScript/CSS mirror；同一 Tools 中的 **Slides theme** 决定新页面的亮暗主题。页面
保存后自动刷新，并在关闭时销毁 Reveal 实例。
新页面顶栏的返回箭头直接回到这篇 Markdown 的编辑页。

### LiveTeX 公式键盘

光标位于行内或块级公式内时，按 `Cmd-/` 打开完整的 LiveTeX 可视化编辑；也可在公式上
右键选择 **Open LiveTeX** 可打开完整的可视化编辑。LiveTeX 顶部的 ✓ 应用公式，
右上角 × 取消本次修改并关闭，原 Markdown 公式保持不变。**∑ 公式工具** 是独立的分类浮层，
提供结构、微积分、希腊字母、关系、代数与群论、集合、箭头、函数、物理、矩阵、样式、搜索，
最后一个分类是 **宏**。代数分类包括左／右半直积、花环积、正规子群关系、函子与群名；
搜索可用中文、LaTeX 命令或分散的英文词（例如 `semi group product`），也会搜到所有生效的宏。

**宏** 分类分四层，后面的覆盖前面的：**共享**（`site-lisp/noema/resources/katex-macros/`，
文件管理，含 physics 宏包的 `\abs \pqty \dv \pdv \comm \mel \pmqty …`）、**项目**（meta 里
`macros-from: ../assignment.cls, notation.sty`，按笔记所在目录读取，远程笔记在其 target 上读取；
只取 `\newcommand`/`\def`/`\DeclareMathOperator` 里不含 `@` 的记号）、**全局**
（`~/.config/noema/katex-macros.tex`，在这里增删改）和 **本笔记**（meta 里的
`#+begin macros … #+end macros` 块，在这里增删改会直接写回笔记）。每一行点一下即插入该宏，
带参数的宏按参数个数插入占位符；被覆盖的同名宏标“覆盖”。KaTeX 预览、MathLive 弹出编辑器与
弹出预览、Emacs 里的 RaTeX 预览和 LaTeX 导出用的是同一套分层宏。打开笔记时宏在首次渲染前就装好；
编辑 meta 里的宏块，停止输入约半秒后只重渲一次。
在 Markdown 中按 `Cmd-Shift-/` 直接打开工具板，无须先进入 LiveTeX；再次按下或按 `Esc` 关闭。
默认选中分类，`Tab` / `Shift-Tab` 切换分类，分类末尾的 `Tab` 或 `Cmd-F` 进入搜索；
方向键选择磁贴，`Enter` 插入到当前公式源码，普通文本位置会新建行内公式。
从 Markdown 正文直接打开时，选中一个磁贴即自动关闭工具板；从 LiveTeX 弹窗打开时可连续选择。
在空的 `\(\)` 中按 `Enter`，可把公式源码展开为多行；继续输入和换行时不会触发 Markdown
列表或引用的自动续行。离开源码后它仍作为**行内公式**嵌在原段落，KaTeX 按 `\(…\)`
的行内语义渲染。Insert 模式下用左右方向键从公式两端进入源码；从相邻行用上下方向键
跨过折叠公式时也会进入。公式内部的空白行是 TeX 段落分隔，保留为源码而不折叠成公式。
单行和跨行公式展开后使用相同的 TeX 源码字体、底色与语法高亮。收起后的公式使用
KaTeX 默认字体，变量、黑板粗体、运算符与可伸缩结构由 KaTeX 分别绘制。

在某一页标题后写 `@@slides(reveal) []`，该标记在编辑预览中隐藏，并将那一页交给原生 Reveal
HTML：顶层 `<section data-background-color="…">…</section>` 会被直接接入 Reveal，支持
`data-auto-animate`、`fragment` 等 Reveal 指令。第一次打开 slides note 时会创建相邻的
`.slides/<note>.js` 与 `.slides/<note>.css` mirror；铅笔 Tools 中的 **Reveal mirror** 会在 Emacs
打开 JS mirror。它在 Reveal 初始化后以 ES module 运行，默认导出函数接收 `{ Reveal, root, file }`。
可从 Roam Node 的 Slides 模板创建，或参考 `templates/noema/slides/markdown-mode/demo`。

### Noema LaTeX 导出

在 Noema 的 Tools 中选择 `Export LaTeX`，或在页面内按 `⌘P`。导出先打开专用范围
选择器，不会再用一个模糊的空白 TOC 输入框：

- `Whole note` 导出全文；有文本选区时会额外提供 `Text selection`。
- 每个 heading 都按真实层级缩进显示；选择 heading 会连同它的所有子章节一起导出。
- `cursor` 标记光标当前所在的最深章节；章节多时可以搜索过滤。
- `↑` / `↓` 选择，`Enter` 确认，`Esc` 取消；双击章节可直接进入保存路径选择。

选完范围后会再让你**选择模板**（`Article` 默认、`Report`、`Assignment`），模板若声明了
额外字段（如 Assignment 的课程代码 / 学期 / 学号）会弹出表单，默认值按 note 记忆。

导出先由 Noema 预处理私有语法，再用 **Pandoc** 完整解析标准 Markdown。服务端先在隔离的
staging 目录验证机械稿；`codex` 模式随后保留一次受严格 gate 保护的 AI 润色机会。已编译稿无论
agent 超时、review 失败或改动 citation/code/resource 等不应触碰的 payload，都立即停止，**绝不 retry 2/3，也不会把未润色的 Pandoc
稿伪装成成功结果**；上一次可用 `.tex` / PDF 保持不变。非致命 overfull 等版式诊断会作为精确反馈
交给 agent。只有机械稿实际编译失败时，所选 AI 后端才可依据编译反馈进行多轮修复。
介入，并经过 review、关键 payload 和编译 gate。章节/列表/段落包装、公式对齐与合理断行允许由
agent 处理，正文及数学含义主要由 prompt 和逐项 review 约束，不再用逐 token 结构比较误杀。
标题由文件名意图、模板用途和一个主主题确定性生成；显式 meta 标题
始终优先。所有最终产物先完整验证、后原子替换，失败导出不会覆盖上一次可用的 `.tex` / PDF。
任务结果会显示 agent 实际耗时以及 `applied / kept` 数量；review 由 host 预生成精确 candidate 模板，
缺失证据会显示 warning，但不会反过来否决一个已通过关键 payload 与编译检查的排版结果。agent 超时
不再按固定三分钟直接杀进程：三分钟无输出时只检查进程是否仍存活，存活就继续等待；单次 attempt
默认有十五分钟硬上限。Emacs 中的导出 agent 通过 Emacs ACP 运行；导出任务结束后自动关闭它的临时会话。
失败的 LaTeX task 会在 Emacs 弹出错误并在 echo 区询问是否人工介入。接受后会打开独立的 agent
窗口，错误和源文件路径预填在可编辑输入中，不会自动发送；你可以手动修改 prompt、笔记或导出设置。
Task Manager 的 `LaTeX exports` 页也提供 `Intervene in Emacs` 和 `Rerun`。`Rerun` 用相同输入
新建任务，不会复活旧进程；关闭失败提示后仍可从 Task Manager 介入。

Codex、Claude、OpenCode 都只在每次导出的隔离 staging 目录中工作；style contract、两个 skill、
source/draft/template/review 会预先复制到该目录，避免 agent 因找不到上下文向父目录探索。网络权限
保持开放。任务卡的 `Agent audit` 可展开查看最终 audit 摘要以及每项 `applied / kept` 的具体理由。

引用会默认扫描 note 所在目录的 `./bib/*.bib`，正文直接写 `@@cite` 即可：

```text
#+begin meta
title: Example
#+end meta

See @@cite(iso) [Str87] {locator: p. 406}.
```

默认 `./bib` 不存在时不会报错。`bib:` 可用半角逗号追加多个其他目录或具体 `.bib` 文件，例如
`bib: ../shared-bib, ./references.bib`；路径本身包含逗号时可写成
`bib: "./refs,2026.bib", ../shared-bib`。目录中每个文件的 basename 是短 namespace，也可使用补全
给出的完整 namespace。多 key 用分号分隔，`prefix` / `locator` / `suffix` 会保留到 PDF。heading
或文本选区导出仍使用当前未保存全文的 meta/bib 上下文。未知/歧义 namespace、缺 key、损坏的
BibTeX 或部分解析成功的多引用都会在写文件前阻断并给出明确诊断，不能再静默生成 `[ns:key]`
占位或丢掉其中一项。代码、数学、HTML comment 和私有 block 中的字面 `@@cite` 不参与引用编号。
meta 内只有嵌套 Summary/Abstract 的正文参与引用解析；其中的 citation、Markdown link、编号和
打开/右键交互与外层正文一致，其他 metadata 字段仍保持私有。
metadata 同时支持 Noema meta block 与 YAML front matter；BibTeX value 支持 `@string` 前向引用、
`#` 拼接、标准月份宏与 TeX accent。未知/循环宏、畸形 field、未闭合 citation key/args 都会报告
带行列位置的诊断。quoted `bib:` 路径可包含逗号，链接 URL 中的 `@@cite` 保持字面量，而可见链接
label 中的 citation 正常解析。

后端可用 `my/noema-latex-export-agent` 选择：`codex`（默认）/ `claude` / `opencode`，都以
非交互、免确认方式运行，且在配置里选定、不会每次询问。引擎开关 `my/noema-latex-export-engine`
（`codex` = verified-first + 单次 gated polish / 必要时 repair；`mechanical` = 从不启动 agent）。中间校验用 draft
模式加速；编译会按日志重跑，最多三遍，直到引用稳定。见 Noema 的 `docs/latex-export-style.md`。
空闲存活检查和硬上限分别由 `my/noema-latex-export-agent-idle-timeout`（默认 180 秒）与
`my/noema-latex-export-agent-hard-timeout`（默认 900 秒）控制。

标题、章节名和 theorem/proof 标签中的 `\(...\)` 会保留为 LaTeX 数学，而不是被转义成
`\textbackslash`。输出路径按 note 记忆，写入是原子的，并强制使用 `.tex` 后缀。未闭合的
display math、代码 fence 或 `#+begin` block 会在写文件前报出明确错误，避免留下半成品。

### LaTeX 预览（AUCTeX + PDF Tools）

`.tex` 文件用 AUCTeX 做编辑、补全、RefTeX、master-file 识别和正式构建（latexmk，
`XeLaTeXMk`/`PdfLaTeXMk`），预览走 PDF Tools + SyncTeX。latexmk 的可执行路径按当前 buffer 的
Remote target 解析：本地 target 用本机 PATH 上的 latexmk，远程 target 会去该 target 上找。

- `C-c C-p`：编译并查看（`TeX-command-run-all`）。
- `C-c C-g` / `M-RET`：正向跳转到当前源码位置对应的 PDF 位置；PDF 不存在时会先编译。
- `M-x latex-preview-menu`：统一菜单，可正向同步、打开已构建的 PDF、编译并查看、或走
  AUCTeX 自带的 `TeX-view`。

远程 target 上的 buffer 目前会在执行构建命令时直接报错拒绝：AUCTeX 自身的进程启动用的是
`start-process`，遇到远程 `default-directory` 时会静默改到本机 `~` 下编译，而不是报错，所以
配置选择显式拒绝而不是让它悄悄编译错文件。texlab 提供的诊断/补全在远程 target 上仍然可用。
PDF 的阅读、批注、搜索仍由 PDF Tools 提供。

### 启动 Dashboard 的 Agenda 卡片

Agenda 卡片在启动画面最下面（Recent Files / Projects 之后、footer 之前），Roam 热力图
仍在上面。

- 点标题「Agenda · next 7 days」打开 Noema Agenda；点任务标题直接定位到该任务。
- 首次没有本机缓存时显示 `Agenda cache is not ready`；进入 Dashboard 后异步查询
  Noema host，成功时保存供下次启动先显示。一次查询超过 15 秒会显示
  `Agenda unavailable · … [retry]`；点 `[retry]` 再发起查询。Dashboard 隐藏时
  不发起查询，重新进入时再刷新。多个同时触发的刷新共用一次请求。

## 2. Leader 键分组

### 文件 `SPC f`

- `SPC f f`
  打开文件；输入时 `Cmd+Shift+Enter` 改在新 Emacs 窗口打开
- `SPC f F`
  其他窗口打开文件
- `SPC f r`
  最近文件
- `SPC f o`
  `find-sibling-file`
- `SPC f C`
  复制当前文件
- `SPC f R`
  重命名当前文件
- `SPC f D`
  删除当前文件

### Buffer / Bookmark `SPC b`

- `SPC b b`
  与 `C-x b` 相同的 Consult buffer 入口
- `SPC b B` / `H-B`
  使用相同候选，在其他 Emacs 窗口打开
- `SPC b F` / `M-x buffer-open-new-frame`
  从当前 frame/workspace 的 buffer 候选中选择，并在**新建的** Emacs frame 打开；
  `<o` 可选择其他 frame/workspace 的 buffer。Noema Markdown 页面会创建独立 pane。
  原生 `C-x 5 b` 仍按 Emacs 的规则在其他 frame 打开，可能复用已有 frame。
- `SPC b .`
  打开 `bookmark-actions-menu`；也可用 `M-x bookmark-actions-menu` 搜索
- `C-x r .`
  打开同一个 `bookmark-actions-menu`
- `C-x r j`
  用 Consult 查找书签；选中候选时按需预览
- `C-x r l`
  打开 Emacs 原生书签列表
- `C-x r n` / `C-x r p`
  下一个 / 上一个行书签
- `SPC b c`
  clone indirect buffer
- `SPC b x`
  `persistent-scratch`：切换本 frame 的持久 `*scratch*`；原生 `M-x scratch-buffer` 仍可用
- `SPC p x` / `M-x project-scratch`
  切换当前项目的持久 scratch；同名项目各有独立 buffer 和保存文件
- `SPC b z`
  埋藏当前 buffer；无上一页时打开项目目录，临时分屏则关闭
- `SPC b j`
  用 Consult 查找书签；选中候选时按需预览
- `SPC b J`
  用 Emacs 原生书签命令在其他窗口跳转
- `SPC b m`
  用 Emacs 原生 `bookmark-set` 设置书签
- `SPC b M`
  用原生 `bookmark-set-no-overwrite` 新增同名书签，避免覆盖现有记录
- `SPC b r`
  用 Emacs 原生 `bookmark-rename` 重命名书签
- `SPC b l`
  打开 Emacs 原生书签列表；可按列排序、标记并批量删除
- `SPC b t`
  切换当前行书签
- `SPC b n` / `SPC b p`
  下一个 / 上一个行书签
- `SPC b L`
  直接设置当前行书签

`SPC b j` / `C-x r j` 和 `SPC SPC m` 使用 Consult 的同一个 bookmark picker。
上下移动候选时按需预览目标位置，确认后由 Emacs 原生 bookmark 跳转；没有匹配项时
Consult 也允许创建新书签。列表使用 Emacs 原生的 `list-bookmarks`，不会为了显示所有
候选而访问每一个书签文件。书签沿用 Emacs 的全局列表；从当前 frame 跳转，
`SPC b J` 则在当前 frame 的其他窗口打开。菜单中的普通设置、同名新增、重命名和删除
也直接使用原生命令；行书签切换和跳转是配置扩展。从 Remote buffer 设置书签时，
默认书签文件仍保存在本机。

### 编辑 `SPC e`

- `SPC e d`
  向下复制当前行/区域
- `SPC e D`
  向上复制当前行/区域
- `SPC e o`
  在下方开新行
- `SPC e O`
  在上方开新行
- `SPC e j`
  下移当前行/区域
- `SPC e k`
  上移当前行/区域
- `SPC e b`
  将光标所在的成对括号在 `()`、`[]`、`{}` 之间轮换；负前缀反向轮换
- `SPC e 1`
  单窗口 / 恢复窗口布局切换

### 临时机器与远程文件传输

选 target 的提示（`SPC o r`、`SPC o f`）里有一项 `临时机器`：选它就输入地址和
密码/密钥，记进 `var/remote/` 并直接打开。Remote 面板（`SPC o R`）上：`m` 记住
一台手动输入的机器，`M` 管理这些机器，`F` 打开 target 文件夹并弹出传输菜单，
`t` 看传输队列。

Remote 面板和路由、连接历史页按 `q` 返回上一页；历史页退出后会释放。
同一个 frame 再开 Remote 面板会保留排序，不同 frame 的面板和历史页各自显示。
临时机器列表也按 `q` 返回，并在各 frame 保留自己的排序。
`M-x remote-doctor` 的报告按 `g` 刷新、`q` 返回；`C-u g` 会连接 target 执行探测。

传输是 Dirvish 里的菜单，不是单独的界面：在任何文件夹里按 `M`（或 `C-c C-t`，
`M-x remote-files`），`c` 把标记的文件送到对面，`g` 从对面取文件，`s` / `r`
同步文件夹，`=` 比较。菜单中的 `o` 可以更换对面机器；在弹出的 Dirvish 里也能
用 `C-c C-o` 从默认的 local 切到其他机器，再选文件或文件夹。
`C-c C-c` 确认、`C-c C-k` 取消。任务在皮卡丘的 `*Activity*` 里。完整说明见
[remote-files.md](../design/remote-files.md)。

### 关闭 Remote target

`M-x remote-board` 的 `C` 关闭所选 target 的 workspace、连接及全部所属 buffer，
包括文件、Dired、LSP 通讯和任务输出。`M-x tramp-cleanup-connection` 同样关闭其
对应 target；`tramp-cleanup-all-connections` 对全部 TRAMP target 执行此清理。
未保存的编辑和拒绝关闭的 buffer 会保留，echo 区列出名称，后台停止自动启动。
保留文件中的 direnv、Copilot 和 LSP 延迟回调也会停止，避免清理后再次连接。
保存后可再次清理；保留文件中需要恢复 LSP 时，显式运行
`M-x my/language-server-ensure`。正常网络掉线仍会恢复连接并保留编辑 buffer。

### Git `SPC g`

远端 buffer 现在和本地一样有 Git 集成：modeline 分支、diff-hl gutter 和上面这些
命令都可用，前提是该 target 的文件操作走 batched backend（`native` 或
`tramp-rpc`）。仍然走 shell TRAMP 的 target 会继续关掉 VC——那里每次探测都要一次
独立往返，开着只会让编辑卡住。想知道当前 buffer 属于哪一类，
`M-x remote-doctor` 的 `route:file-read` 一行会直接写出来。行号也不再因为文件
在别的 target 上而消失。

- `SPC g .`
  Git Hub；把状态、当前文件 diff / log / blame / stage、merge conflict 收到一个 transient 菜单里
- `SPC g g`
  `magit-status`
- `SPC g w`
  打开 Git 工作台，列表看当前仓库文件状态；`RET` 打开文件，`d` diff，`l` log，`B` blame，`s` / `u` stage / unstage
- `SPC g t`
  打开 `gittree` 可视化；当前窗口显示带颜色的 `git log --graph --decorate --oneline --all`
- `SPC g d`
  当前文件直接对比任意 Git revision 和现在的 buffer，当前窗口打开 unified diff
- `SPC g =`
  当前文件直接对比 `HEAD` 和现在的 buffer
- `SPC g b`
  当前文件对比当前 branch 基线；优先取 upstream merge-base，没有 upstream 时退回仓库 root commit
- `SPC g l`
  当前文件历史
- `SPC g B`
  blame 切换
- `SPC g S` / `SPC g U`
  stage / unstage 当前文件
- `SPC g [` / `SPC g ]`
  上一个 / 下一个 hunk
- `SPC g r`
  回滚当前 hunk
- `SPC g s`
  stage 当前 hunk
- `SPC g h`
  查看当前 hunk
- merge conflict 文件内：`M-x merge-conflict-menu` / `SPC g m` 打开冲突菜单；
  Emacs 原生 `C-c ^` 前缀也保留。在 Evil normal 状态可直接用 `o` ours、`t` theirs、
  `b` both、`B` base、`n` / `p` 或 `[c` / `]c` 跳冲突、`e` 进 `ediff`、`q` 打开菜单；
  退出 `smerge-mode` 后这些单键恢复原义。
- `gittree` buffer 内
  `RET` / `o` 或鼠标点 commit 查看当前 commit，`n` / `p` 上下跳 commit，`y` 复制 hash，`g` 刷新，`q` 退出回原 buffer

macOS GUI 下也可以直接用 `Option(H-)` 拉平这组编辑操作：

- `H-O`
  上方开新行
- `H-k` / `H-K`
  向下 / 向上复制当前行或区域
- `H-<up>` / `H-<down>`
  上移 / 下移当前行或区域
- `H--` / `H-=`
  收缩 / 扩大选择
- `H-;`
  注释或取消注释当前行/区域
- `H-'` / `H-[` / `H-]` / `H-/`
  多光标按行 / 上一个相同项 / 下一个相同项 / 全选相同项

### Help `SPC h`

- `SPC h f`
  `helpful-callable`
- `SPC h c`
  设置菜单 `config-dispatch`；在菜单里按 `c` 打开 `config-board`
- `SPC h C`
  `helpful-command`
- `SPC h v`
  `helpful-variable`
- `SPC h k`
  `helpful-key`
- `SPC h K`
  以只读方式打开本快捷键索引
- `SPC h m`
  显示当前 major mode 的实际键位（`mode-keybindings`）
- `SPC h H` / `SPC h i`
  健康检查菜单 / 本次启动统计
- `SPC h w`
  打开 `*Warnings*` 日志
- `SPC h d`
  `devdocs-lookup`
- `SPC h t`
  `tldr`

### Code `SPC c`

- `SPC c ?`
  diagnostics menu
  统一入口：当前 buffer 面板、项目原生 Flymake 列表，以及按严重度筛选的 picker
- `SPC c b` / `SPC c B`
  配置编译板 / 编译菜单
- `SPC c !`
  当前 buffer 的 problems 面板
- `SPC c a`
  code actions
- `SPC c .`
  code actions menu；`b` build，`B` rerun build，自动识别常见的 `make` / `cmake` / `ninja`
- `SPC c f`
  format buffer
- `SPC c r`
  rename
- `SPC c o`
  organize imports
- `SPC c R`
  restart language server
- `SPC c L`
  切换当前 buffer 的 CodeLens；默认开启。CodeLens、inlay hint、文档颜色/链接和
  semantic-token 预取只覆盖可见区上下的小段缓冲，服务器缓存仍保留完整状态
- `SPC c s`
  语言服务器菜单，可以进 Hub / Doctor / 调参 / log / session / config
- `SPC c S`
  搜索当前 workspace 的语言服务器符号
- `SPC c i`
  `show-imenu`
  左侧 smart-toggle `treemacs`，并跟随当前文件和光标所在 symbol。展开文件后
  Outline 从固定浅层开始，以 `OUTLINE · 文件名` 标明归属，并按类、结构体、接口、
  方法、字段、变量等显示不同 VS Code 风格图标。
- `SPC c I`
  `lsp-ui-doc-glance`
- `Esc`
  关闭当前 LSP hover / signature / peek 弹层；没有 LSP 弹层时继续执行普通 Evil Escape。
  关掉 hover 后光标不动它不会自己弹回来，移到别的符号或 `SPC c I` / `C-h d` 再显示。
  hover 子窗口贴在光标所在行的上方或下方，不会盖住那一行；服务器返回跨多行的 hover
  range 时（Lean 就是）也一样
- `SPC m d`（TeX/LaTeX buffer 内）
  `my/latex-preview-doctor`：公式预览体检——后端是否存活、在途请求、宏目录与
  宏条数、最近一次错误、光标处检测到的公式。预览不出来或卡顿先看这个，
  见 [latex-preview.md](../design/latex-preview.md)
- `SPC m p`（TeX/LaTeX buffer 内）
  `ratex-refresh-previews`
- `SPC c j`
  调试菜单：启动 Dape、profile、步进、断点、REPL、locals/watch、adapter doctor
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

- `SPC c c`
  `compile`
- `SPC c C`
  `recompile`
- `SPC c D`
  当前 buffer 的 Diagnostics 面板；错误、警告、提示共用当前 frame 的一个面板，
  `e` / `w` / `n` 筛选，`d` 显示全部，`q` 返回来源
- `SPC c P`
  切换当前项目的 Emacs 原生 Flymake 诊断列表；包含未打开文件的已知诊断
- `SPC c x`
  `quickrun`
- `SPC c y`
  `my/note-code-copy-reference` — 从代码 buffer 生成 `@@note-code(path)[tag]` 到剪贴板。
  选中区域：提示输入 tag，在区域首行前自动插入 `@noema TAG` 注释标记，然后复制引用。
  未选中：查找光标上方最近的 `@noema`/`@note-code` 标记，复制对应引用。
  Path 规则：`/...` 表示从当前 content root 开始；roam vault 内是 roam-root，其他项目文件是 project.el 根目录。裸相对路径保留为从当前 note 目录开始。

测试 runner 优先按当前文件语言选择：混合仓库中的 Go、Python、Rust 和 Node 文件各走
自己的工具；没有可识别源文件时按项目标记回退。Go 根目录文件的“当前文件”范围只测
根包，项目范围才使用 `./...`。项目本地 `:test` 覆盖仍优先于自动检测。

### Open `SPC o`

- `SPC o d`
  `dirvish-dwim`
- `SPC o D`
  `dirvish-fd`
- `SPC o q`
  `clutch-query-console`
- `SPC o e`
  `my/portal-popup-toggle`
- `SPC o E`
  切换到下一个 popup `portal`
- `SPC o F`
  切换当前 popup `portal` 的固定状态
- `SPC o t`
  `my/portal-popup-toggle`
- `SPC o v`
  直接打开新 `portal`
- `M-x my/project-popup-portal-app`
  在当前项目根目录的新 popup `portal` 里运行 `lazygit` / `btop` / `yazi` / `tmux`

顶部 `+term` 右键可打开启动菜单，`Applications` 列出上述已配置程序。
弹窗终端的标签左键切换，右键可重命名、关闭或移动标签位置。
`Agent` 子菜单（也可点击 `+Agent`）提供 Claude / Codex / OpenCode；它们使用
原生 `agent-shell` / ACP buffer，不在 portal 里运行 CLI，共用同一个顶部弹窗、
标签池、自动收起和固定逻辑。`C-c C-e` 折叠/打开，`C-c E` 切换标签，
`C-c M-e` 固定。Agent 原有的模型、会话模式和权限提示保留；仅点击启动时
加载 Agent 依赖。ACP adapter 沿用现有 agent-shell 配置；在远端 `/fs:` 工作区里
启动时，agent 进程直接运行在该 target 上，只要 target 的 PATH 里有对应二进制
（如 `claude-agent-acp`、`codex-acp`、`opencode`）。本地与远端走同一条路径，
裸 `M-x agent-shell` 同样适用。查找用的是该 workspace 的环境（PATH 与项目
direnv），因此项目 `.envrc` 提供的 agent 也能找到。popup agent 与 popup 终端一样从
项目根目录启动（本地与远端相同），不在项目里时用当前目录。注意 `codex` / `claude`
CLI 本身不说 ACP：远端还需要安装适配器，例如
`npm i -g --prefix ~/.local @zed-industries/codex-acp @agentclientprotocol/claude-agent-acp`。
这两个适配器各自内置一份 CLI，默认跑内置版本，模型列表因此落后（例如只有
Opus 5）。所以 Emacs 会用同一个 workspace 环境查找 `claude` / `codex`，并通过
`CLAUDE_CODE_EXECUTABLE` / `CODEX_PATH` 让适配器改用 shell 里那份会自动更新的
CLI；本地和远端规则相同。没有 fallback：target 上找不到 CLI 时直接报错并写明
target。本机已删除适配器内置的 CLI（`@anthropic-ai/claude-agent-sdk-darwin-arm64`、
`@openai/codex*`），`npm update -g` 重装适配器后会重新带回，需再删一次；唯一的
`claude` / `codex` 在 `~/.local/bin`，由 `~/.zprofile` 放进登录 shell 的 PATH，
Emacs 经 `exec-path-from-shell -l` 取得。Pi 与 OpenCode 没有内置 CLI，不受影响。
- `SPC o V`
  命名 `portal`
- `SPC o f`
  文件传输菜单（rsync 上传/下载/同步）；不在 Dired 里时先问 target 并打开它的
  文件夹。Dirvish 里用 `M`。详见 [remote-files.md](../design/remote-files.md)
- `SPC o M`
  临时机器列表：手动输入地址和密码/密钥、记在 `var/` 里的 target
- `SPC o S`
  `my/portal-ssh`
- `SPC o s`
  `shell-toggle`
- `SPC o w`
  `my/open-eww-url`
- `SPC o x`
  `my/open-xwidget-url`
- `SPC o P`
  `my/portal-terminal`，在当前窗口打开原生 Ghostty 终端（本地 macOS GUI）

普通 Portal 终端在窗口顶部有独立的终端标签组；全局工作区栏不占用顶行。
终端标签不混入普通文件标签或 popup 标签。
各分屏使用同一终端标签列表，但已在其他分屏显示的终端不再列出；点击只切换
被点击的分屏。普通页面沿用原来的文件标签；Noema 页面若已在其他分屏打开，
不会在当前分屏的标签里重复出现。
左键切换，右键可新建、重命名、关闭或调整顺序。
`M-t`（macOS `Cmd-t`）在任何 buffer 打开终端选择器；在终端里调用时，列表不显示当前终端。
列表包括普通终端、弹窗终端，最后一项是新建终端。`Enter` 在当前窗口打开，
`Shift-Enter` 在右侧分屏打开，`Cmd-Enter` 在下方分屏打开；选择弹窗终端时直接切换到对应弹窗。

- `SPC o W`
  统一搜索入口，可选搜索引擎和浏览后端
- `SPC o B`
  在 `eww` / `xwidget` / macOS `open` 之间切换当前页面

Portal 终端聚焦时，`C-c e` 也会切换 popup。此时 `C-c` 是 Emacs 前缀；要把
原本的 `C-c` 发送给终端（例如中断程序），连按 `C-c C-c`。本地 Ghostty 和
远端终端正常退出后都会按进程事件关闭 buffer；远端连接意外断开时保留可重连的
buffer。popup 中的 buffer 关闭时，其窗口也随之收起。mode line 使用普通 Emacs
buffer 的样式。

`emacsopen [-o|-l|-r|-a|-b|-f] [-n] [+行[:列]] 文件…` 在运行中的 Emacs 打开文件：默认选中窗口，`-o` 其他窗口，`-l/-r/-a/-b` 向左右上下分割，`-f` 新 frame，`-n` 不抢焦点；每个文件输出一行 JSON，供 agent 与脚本使用，`emacsopen --help` 列出全部选项。
Yazi 按 `Enter` 打开光标下的文本或目录时也使用它；PDF、图片等仍交给 macOS 应用。

### Browser `C-c w`

显式打开的 `xwidget` 和 Noema 页眉提供历史导航；
旁边的面板图标左键展开全局 popup，右键列出未显示的 Noema buffer。
键盘 `b` / `f` 仍可直接返回 / 前进。

- `C-c w w`
  统一 `browse-url` 入口，默认使用 Portal
- `C-c w p` / `C-c w e` / `C-c w x`
  直接用 Portal / `eww` / `xwidget-webkit` 打开 URL；这些后端
  会在独立浏览 buffer 里打开，连续多开也不会顶掉已显示的浏览 buffer
- `C-c w P` / `C-c w E` / `C-c w X` / `C-c w O`
  把当前页面快速切到 Portal / `eww` / `xwidget-webkit` / macOS `open`
- `C-c w s`
  交互选择目标后端；可选 `portal` / `xwidget` / `eww` / `system`
- `C-c w d`
  关闭当前浏览后端；在 Portal / `eww` / `xwidget-webkit` buffer 中也可以按 `M-w`，
  会同时 kill browser buffer 并删除对应窗口
当前策略是手动分流：默认打开方式统一在 `lisp/init-open.el` 的
`my/open-routes` 里维护，route DSL helper 来自 vendored `general.el` 的
`general-route-*`。`browse-url` 默认走 Portal；
`eww` 适合阅读，`system` 用系统应用处理文件或链接。

### Tab `SPC t`

- `SPC t n`
  新建 tab 与同名 Perspective workspace
- `SPC t N`
  指定新 tab/workspace 名称；与现有 tab 或 workspace 重名会提示
- `SPC t t`
  调用 Emacs 原生 `tab-switch`
- `SPC t d` / `SPC t D`
  原生 `tab-close` / `tab-close-other`，只关闭窗口布局；Perspective workspace 和 buffer 保留
- `SPC t g`
  工作区菜单；其中 `d` 明确删除 Perspective workspace 及其专属 buffer，并关闭同名 tab；
  若它是唯一的 tab，则将该 tab 改名为保留下来的 workspace
- `SPC t r`
  同时重命名当前 tab 与 Perspective workspace
- `SPC t [`
  上一个 Perspective workspace
- `SPC t ]`
  下一个 Perspective workspace

顶部的 workspace tab 使用 Emacs 原生 frame 边界。居中的 buffer tab 只列当前 frame
访问过、且属于当前 workspace 的 buffer；Noema 的 Markdown 页面也以笔记文件名入列。
同一个 buffer 在两个 frame 中打开时，会出现在两边，左右移动 tab 只改变本 frame 的顺序。
`M-x ibuffer` 保留按类型分组，新增 `Frame` 列显示 `F1`、`F2` 等访问过该 buffer 的
frame；`s F` 可按当前 frame 优先排序。后台基础设施 buffer 不显示 frame 标记。
`SPC r s` / `M-x workspace-session-save` 保存所有 Perspective frame 的工作区布局；
`SPC r l` / `M-x workspace-session-load` 从同一文件恢复。路径由 Perspective 原生的
`persp-state-default-file` 设置，原生 `persp-state-save` / `persp-state-load` 也使用它。
工作台保存入口原子替换状态文件，失败时保留上一次文件；加载会按 Perspective 的
原生行为恢复 frame，重复加载可能再创建 frame。退出自动保存默认关闭，可通过
`my/session-auto-save` 启用。未关联文件的 buffer 不在 Perspective 状态文件中，
持久 scratch 的正文由其独立状态文件保存。

ibuffer 仍是全局总览；日常 `C-x b`、`H-b`、`SPC b b` 的 Buffer 候选则从本
frame/workspace 开始，通过 `<o` 临时跨越边界。埋藏 buffer 只从可见 tab 消失，
仍能在本 frame 的切换候选与 ibuffer 的 `Frame` 标记中找到。
在 ibuffer 中访问同一 Noema 笔记的重复条目时，会打开它的主页面。`RET`
在当前窗口打开，`Shift+Enter` 在右侧分屏打开，`Cmd+Enter` 在下方分屏打开；
列表保留在原窗口。单窗口前缀和不选中窗口仍沿用 ibuffer 的原生操作，现有标记保留。

ibuffer、帮助页和其他用 `quit-window` 退出的临时页面，优先返回同一窗口的上一页；
已经在其他窗口显示的 buffer 不作自动返回目标。临时创建且无返回页的窗口会关闭；
最后一个窗口无可返回页时打开项目目录。如果弹窗有精确的来源页，仍由 Emacs 原生返回；
自动补选遵守该 frame 的 buffer 过滤规则，不会把旧 xwidget 页当作普通候选。
`SPC b z` 和最后一个 frame 上的 `H-w` 保留原本的埋藏动作，埋藏后也按此规则返回。
`Cmd+Shift+W`（Emacs 的 `M-W`）仍从当前 Perspective 弹出 buffer 列表供选择；
选中的是当前页时，杀掉后按同一规则收尾。`C-x k` 仍是原生 Perspective 选择器。
测试、任务、构建和运行 profile 的输出按发起的 frame 命名为 `*test F1*` 等，
各 frame 独立保存“重跑上次”与最近输出。输出面板的 `R` 仍用原生 `recompile`
重跑该输出 buffer 自己的命令；直接 `M-x compile` 仍采用 Emacs 原生命名。

### Project `SPC p`

- `SPC p .`
  打开项目工作台
- `SPC p p`
  切项目
- `SPC p o`
  旧快捷键，与 `SPC p p` 进入同一项目工作流
- `SPC p f`
  当前项目找文件
- `SPC p s`
  当前项目全文搜索
- `SPC p d`
  打开项目根目录
- `SPC p m`
  打开当前项目 Magit
- `SPC p v`
  打开当前项目 portal
- `SPC p a`
  手动添加项目
- `SPC p D`
  批量扫描目录下的项目
- `SPC p .` 后按 `x`
  彻底移除一个项目及其相关状态（包含 Projectile、`project.el`、Treemacs、perspective、项目 buffer/portal）；`SPC p x` 打开项目 scratch
- `SPC p l`
  查看当前项目 project-local overrides（来自 `my/project-local-overrides` 全局配置）
- `SPC p L`
  快捷打开 `.dir-locals.el`（同 `SPC p e e`）

### Dir-locals / 项目环境 `SPC p e`

- `SPC p e e`
  编辑当前项目 `.dir-locals.el`
- `SPC p e c`
  从模板创建 `.dir-locals.el`
- `SPC p e m`
  将模板合并进现有 `.dir-locals.el`
- `SPC p e r`
  重载 dir-locals 并刷新 direnv 环境（PATH 等）
- `SPC p e s`
  将所有非 `eval` 变量静默（加入 `safe-local-variable-values`）
- `SPC p e d`
  查看哪些 dir-locals 条目对当前 buffer 生效

可用模板：`python-venv`、`python-uv`、`python-conda`、`cc-cmake`、`cc-meson`、`nix-flake`、`nix-gcc`、`nix-clang`、`nix-shell`、`sagemath`、`node`、`lsp-workspace`、`emacs-lisp`、`indent-2`、`indent-4`、`direnv`。详见 [settings-cookbook.md § 16](settings-cookbook.md)。

## 3. 搜索与跳转

- `SPC SPC`
  打开 `telescope`
- `H-x` / `H-t` / `F2`
  同样打开 `telescope`
- `SPC SPC f`
  当前项目找文件
- `SPC SPC b`
  统一切换 buffer
- `SPC SPC g`
  当前项目 ripgrep
- `SPC SPC I`
  当前 workspace / 项目 symbols；输入时实时刷新候选并 preview
- `SPC SPC i`
  当前 buffer symbols；输入时实时 preview 到候选 symbol
- `SPC SPC m`
  bookmark picker；当前项目条目优先，移动候选时预览目标位置，没有书签时打开 bookmark 列表
- `SPC SPC !` / `SPC SPC ?`
  当前 buffer / 当前项目 diagnostics picker
- `SPC SPC B` / `SPC SPC D`
  当前 buffer 自定义诊断面板 / 当前项目原生 Flymake 列表；项目列表按项目分开，
  会随 Flymake 自动更新，并包含未打开文件的已知诊断
- `SPC SPC e` / `SPC SPC w` / `SPC SPC n`
  当前 buffer errors / warnings / notes picker，移动候选时预览，确认后跳转
- `SPC SPC E` / `SPC SPC W` / `SPC SPC N`
  当前项目 errors / warnings / notes picker，移动候选时预览，确认后跳转
- `SPC SPC d`
  diagnostics menu
- `C-s`
  当前 buffer 搜索
- `C-x C-r`
  最近文件
- `SPC s p`
  `consult-ripgrep`
- `SPC s s`
  `consult-line`
- `SPC s i`
  `imenu`
- `C-c p .`
  非 Evil 下打开项目工作台
- `C-;`
  `avy-goto-char`
- `C-:`
  `avy-goto-char-2`
- `C-'`
  `avy-goto-word-1`

## 4. 结构导航

- `M-.` / `gd` / `SPC n d`
  查找定义
- `M-?` / `gr` / `SPC n r`
  查找引用
- `M-,` / `SPC n b`
  返回上一跳；`SPC n f` 前进。优先使用当前窗口的跳转历史，历史中没有可用跳点时使用 Emacs 的 xref 历史
- `SPC n a`
  跳到当前函数开头
- `SPC n e`
  跳到当前函数结尾
- `SPC n [`
  上一个函数
- `SPC n ]`
  下一个函数
- `SPC n u` / `M-x structure-up`
  跳到外层结构；有 Tree-sitter 语法树时按结构节点上跳，否则使用 Emacs 原生括号导航
- `SPC n l` / `C-c C-j` / `M-x open-file-location`
  打开光标处或选区中的 `file:line:column`；前缀参数在其他窗口打开。本机绝对路径在 `/fs:` buffer 中仍指向本机；远程绝对路径要写完整 `/fs:目标ID:/路径`。行号无效时保持原窗口
- `[f`
  上一个函数
- `]f`
  下一个函数

## 5. 折叠与结构选择

### 文档概览

GUI frame 的两侧 fringe 分工如下：

- 左侧显示当前位置对应的 Flymake、Git 和代码折叠 indicator
- 右侧显示 `scrollview` 全文概览，滚动块和标记都可以直接点击跳转
- TTY 没有 fringe 时，全文概览自动退回右 margin

右侧概览默认汇总搜索结果、诊断、Git 改动、书签和 `symbol-overlay`。超过
20,000 行或 1 MB 的 buffer 只保留滚动条，不扫描全文标记。帮助、Dired、编译、
终端和临时面板默认不启用；需要时可运行 `M-x scrollview-mode`。

- `SPC j n`
  跳到下一个概览标记
- `SPC j p`
  跳到上一个概览标记
- `SPC j v`
  显示概览标记图例

### 折叠

- `za`
  切换当前折叠
- `zo`
  打开当前折叠的一层；内部已有折叠继续保持折叠
- `zO`
  递归展开当前 zone / subtree
- `zc`
  关闭当前折叠
- `zR`
  展开当前 buffer 的所有折叠
- `zM`
  折叠当前 buffer 的所有折叠
- `H-<tab>`
  同 `za`；在 Org 标题上也走这套统一折叠入口
- `H-S-<tab>` / `H-<backtab>`
  同 `zO`，递归展开当前 zone / subtree
- `SPC z a`
  同 `za`
- `SPC z o`
  同 `zo`
- `SPC z O`
  同 `zO`
- `SPC z c`
  同 `zc`
- `SPC z R`
  同 `zR`
- `SPC z M`
  同 `zM`

后端规则：

- `org-mode`：标题折叠走 Org 自己的 subtree folding
- `*-ts-mode`：只启用 `treesit-fold`，并启用可点击的左 fringe indicator
- 其他 `prog-mode`：只启用 `hideshow`；Emacs 31+ 使用内置隐藏行计数，indicator
  和折叠占位文本都可用鼠标点击
- C/JavaScript 一类花括号语言使用新版 `hideshow` 的原生折叠边界，结束花括号和
  后续 `else` 保持可见，不额外移动 overlay

打开文件时：

- Org 和 Typst 打开文件时默认不自动折叠；文件自己的 `#+startup:` 设置仍可覆盖 Org
  的 startup 行为
- Org 标题在半展开状态时，`za` / `H-<tab>` 会收起整个 subtree；只有完全收起时
  才打开一层
- 在 `#+title:` 或第一个 heading 前按 `za` / `H-<tab>` 时，会切换整个 Org
  buffer 的 compact / open 状态
- 代码 buffer 若没有保存过手动折叠状态，会按 `my/fold-prog-startup` 应用默认折叠；
  Org / Typst 不恢复保存的折叠状态
- 自动默认折叠不会写入 `var/fold-state.el`；只有通过 `za` / `zo` / `zc` /
  `zM` 或 `SPC z ...` 改过的代码折叠状态才持久化；`zR` 展开全部会清掉当前
  buffer 的保存折叠状态，下次重新回到默认压缩视图
- Org 的 inline image、LaTeX preview 和 special-block 卡片刷新只看可见且未折叠
  的区域；展开 subtree 后会用 idle timer 合并调度可见区渲染，折叠动作本身不
  同步扫描整块可视区
- `treesit-fold` 和 `hideshow` 代码块折叠后，隐藏内容里的 Flymake / LSP 诊断会
  压缩到可见折叠行显示为 `E` / `W` / `N` 计数，鼠标悬停可看前几条具体消息。
  Org 不做这层诊断汇总，避免折叠大纲时额外扫描标题和正文。

### 结构选择 `SPC v`

- `SPC v v`
  逐级扩选
- `SPC v V`
  缩回上一步
- `SPC v f`
  选整个函数 / method / class
- `SPC v F`
  选函数 / class 的 body
- `SPC v s`
  选当前语句
- `SPC v e`
  选当前表达式
- `SPC v b`
  选当前代码块
- `SPC v B`
  选当前代码块内部
- `SPC v p`
  选下一层外层结构

## 6. 多光标和 snippet

### 多光标

- `C-S-c C-S-c`
  对选中多行建立多光标
- `C->`
  选中下一个相同项
- `C-<`
  选中上一个相同项
- `C-c C-<`
  全选相同项
- Evil visual 下：
  - `g n`
  - `g p`
  - `g a`

### Snippet

- `C-c y y`
  展开 snippet
- `C-c y i`
  插入 snippet
- `C-c y n`
  新建 snippet
- `C-c y v`
  打开 snippet 文件

补全弹窗在 LSP buffer（本地与远端相同）里也会列出 snippet：C/C++ 输入 `p ..`
弹出 `..`（选中得到 `p->`），ipynb 投影里输入 `j` 弹出 `jcode` / `jmd` / `jraw` /
`jcell` 等 cell 模板。`.`、`->` 这类触发符之后只保留刚好敲出的多字符 key，不会把
全部模板混进成员补全；单字符 key（如 Python 的 `.` → `self.`）只能用 `C-c y y`。

## 5. Dired / Dirvish

- `C-c o d`
  打开 Dirvish
- `C-c o f`
  `dirvish-fd`

`C-c o d` 会根据窗口情况打开普通单窗口视图或带左侧上级目录和右侧预览的视图。
在 Dirvish 中按 `m l`（或用 `M-x dirvish-layout-toggle`）可随时在两者之间切换；
切换只改变当前 Dirvish 会话的布局。

Dirvish 里的单键跟 `~/.config/yazi/keymap.toml` 一致（也就是 ranger 那一套），
定义在 [lisp/init-dirvish-keys.el](../../lisp/init-dirvish-keys.el)。不经 Dirvish 的
纯 Dired 仍是 Emacs 原生键位。

| 键 | 作用 |
| --- | --- |
| `j` / `k` | 下 / 上 |
| `h` / `l`，`←` / `→` | 上级目录 / 进入目录或打开文件 |
| `S-←` / `S-→` | 历史后退 / 前进 |
| `C-u` / `C-d`，`C-b` / `C-f` | 半页 / 整页 |
| `g g` / `g b`（或 `G`） | 顶部 / 底部 |
| `g h` `g a` `g d` `g p` `g v` `g m` `g w` `g u` `g c` `g l` | Home、Desktop、Downloads、Pictures、Movies、Music、Documents、Public、`~/.config`、`~/.local` |
| `g SPC` / `g f` / `g r` | 从光标所在目录补全并跳转 / 跟随符号链接 / 刷新 |
| `SPC` | 选中 / 取消选中并下移 |
| `C-a` / `C-r` / `Esc` | 全选 / 反选 / 清除选择 |
| `y` / `x` | 复制 / 剪切选中的文件（没选就是光标所在项） |
| `p p` / `p f` | 粘贴到光标所在目录 / 强制覆盖粘贴。跨目标源文件按来源目标分别排 rsync 任务，不阻塞 |
| `Y` / `X` | 取消复制 / 剪切 |
| `p l` / `p L` / `p h` | 在光标所在目录建符号链接（绝对 / 相对）/ 硬链接；加前缀参数可另选目标目录 |
| `p m` | Dirvish 原生复制粘贴菜单；其中复制、移动和链接也默认使用光标所在目录 |
| `d` / `D` | 移到回收站 / 永久删除（都会先确认） |
| `a`（或 `c r`） | 在光标所在目录新建文件；光标在文件上则取其父目录，输入以 `/` 结尾则新建目录；提示可补全路径 |
| `r r` / `r i` / `r a` / `r f`（`r c`） | 重命名：光标在扩展名前 / 开头 / 末尾 / 清空 |
| `c m` | 重命名并清空文件名，保留扩展名 |
| `c c` / `c d` / `c f` / `c n` | 复制路径 / 光标所在目录路径 / 文件名 / 不含扩展名的文件名 |
| `RET` | 在 Emacs 里打开 |
| `Shift+Enter` / `Cmd+Enter` | 在右侧 / 下方分屏打开，原文件列表保留；普通 Dired 同样可用 |
| `Cmd+Shift+Enter` | 在独立的新 Emacs 窗口打开文件或目录；`M-x dirvish-open-new-frame` 可搜索到相同动作 |
| `o o` | 用系统默认程序打开 |
| `o c`、`:` / `O`、`;` | 对文件运行 shell 命令：等待 / 异步 |
| `S` | 在光标所在目录开终端 |
| `f` | 过滤（`dirvish-narrow`） |
| `/`，`n` / `N` | 搜文件名，下一个 / 上一个匹配 |
| `s` / `z` / `Z` | 从光标所在目录 fd 搜索 / 模糊查找文件；zoxide 跳转 |
| `.` | 显示 / 隐藏 dotfiles |
| `m s` `m p` `m b` `m m` `m o` / `m n` | 显示详情（大小、时间、权限、所有者）/ 只显示文件名 |
| `m l` | 切换普通视图与左侧上级目录、右侧预览的布局 |
| `, m` `, M` `, b` `, B` `, e` `, E` `, a` `, A` `, n` `, N` `, s` `, S` | 排序：修改时间、创建时间、扩展名、字母、自然、大小；大写为倒序 |
| `, ,` | Dirvish 自带的排序菜单 |
| `TAB` | 展开 / 折叠子目录（原地树形） |
| `S-TAB` | 文件信息菜单 |
| `t`，`1`–`9`，`-` / `=`，`_` / `+` | 新标签页、切到第 N 个、上一个 / 下一个、与左 / 右交换 |
| `w` | 任务管理（皮卡丘的 `*Activity*`） |
| `M` | 光标所在目录的文件传输菜单（rsync 上传 / 下载 / 同步），见 [remote-files.md](../design/remote-files.md) |
| `?` | 按键提示：在下方弹出这张表，`q` 关闭 |
| `~` | Dirvish 总菜单 |
| `q` | 退出 |

常用扩展动作也可从 `M-x` 搜索：`dirvish-select-for-copy`、
`dirvish-select-for-move`、`dirvish-paste-here`、`dirvish-create-here`、
`dirvish-trash`、`dirvish-go-directory` 和 `dirvish-key-help`。按键直接绑定这些名称；`h` / `l` 则直接
使用 Emacs 原生的 `dired-up-directory` / `dired-find-file`。
固定目录跳转、排序和数字标签页只有按键入口，不占用 `M-x` 的命令列表。

Dirvish 的复制、移动和链接在客户端可直接访问时沿用原生独立 Emacs 子进程，
`/fs:local:` 等逻辑路径先转换为它可识别的本机路径。目标没有客户端可见路径时，
同目标操作改由 Remote 框架在目标端异步执行，进入 `*Remote Transfers*` 队列；
任务全局共用，但每个 frame 的队列表格各自保存排序和阅读位置；
完成后刷新显示该目录的 Dired 视图，移动还刷新来源目录。已有文件在复制或移动前
询问一次，`p f` 则直接覆盖；链接不会覆盖已有项。不同来源的文件若同名，会在排队前
报错，避免并行任务覆盖同一个落点。跨目标复制仍用 rsync 队列，跨目标剪切不自动转成
复制加删除。

和 yazi 不完全一样的地方：

- `m` 组在 yazi 里分别切换单列，这里只有“显示全部详情 / 只显示名字”两档；
- `, b` 按创建时间排序依赖 GNU `ls`（`gls`），`, r` 随机排序没有做；
- `v` 视觉选择、`R b` 回收站管理、`M p` 挂载硬盘没有对应；
- `?` 是按键提示而不是向前搜索（用 `N`）；提示内容在
  `my/dirvish-key-hints`，改键时一并改；
- `TAB` 保留 Dirvish 的子目录展开，yazi 的 `TAB`（文件信息）放在 `S-TAB`；
- `-` / `=` 按你的 yazi 配置是切换标签页，所以创建链接放在 `p l` / `p L` / `p h`。

被让出单键的原 Dirvish / Dired 命令：

| 命令 | 原来 | 现在 |
| --- | --- | --- |
| 标记 | `m` | `SPC` |
| 快速访问 `dirvish-quick-access` | `a` | `'` |
| 复制粘贴菜单 `dirvish-yank-menu` | `y` | `p m` |
| 排序菜单 `dirvish-quicksort` | `s` | `, ,` |
| 文件信息 `dirvish-file-info-menu` | `f` | `S-TAB` |
| 子树展开 `dirvish-subtree-toggle` | `TAB` / `l` | `TAB` |
| 总菜单 `dirvish-dispatch` | `?` | `~`（`?` 现在是按键提示） |
| 刷新 | `g` | `g r` |
| `wdired` | `C-c C-e` | 不变 |
| 前缀参数 `C-u` | `C-u` | Dirvish 里是上翻半页，用 `M-数字` 代替 |

## 6. 窗口和弹出层

- `C-x 1`
  `my/toggle-delete-other-windows` — 最大化当前窗口，再次执行恢复先前布局（依赖 winner-mode）
- `M-o`
  `ace-window`；Noema xwidget 里的目标编号会显示在页面左上角
- `M-O`
  交换当前窗口与目标窗口
- `M-\``
  `my/portal-popup-toggle`
- `H-\`` / `C-\``
  `popper-toggle`
- `C-M-\``
  改变 popup 类型

### 从其他应用里操作 Emacs(Raycast)

Raycast 扩展 `tools/raycast-emacs` 只有一个入口 **Emacs**,热键在 Raycast 里录在它上面。
它是这份 Emacs 的日常控制面,启动、重启、退出都从这里做。打开后一个列表分三组,搜索框
同时过滤三组:

- **Actions** 白名单动作,回车执行;带参数的动作(比如 Open File)会再让你输入一行。
- **Frames** macOS 图形顶层 frame（TTY 客户端不在列表中）,隐藏的带 `hidden` 标记。回车聚焦(隐藏的会先显示出来),
  `Cmd-H` 隐藏,`Cmd-O` 只留这一个(其余隐藏),`Ctrl-X` 关闭,`Ctrl-Shift-X` 关闭其余;
  `Cmd-N` 新建,`Cmd-Shift-S` / `Cmd-Shift-H` 全部显示 / 全部隐藏。
- **Emacs** 一行状态:可见 / 后台 / 没应答 / 没运行,以及运行时长、常驻内存、buffer 数、
  子进程数。回车显示或隐藏;没运行时回车在后台启动(第二个动作是启动并显示)。任何一行上
  `Cmd-R` 刷新,`Cmd-Shift-R` 重启;Force Quit Emacs 用于没应答的 Emacs,会先确认。

每次打开 Raycast 列表都会重新确认 Emacs 状态；确认前显示 Checking，查询失败则显示
Could not check Emacs，不会把上次缓存的没应答状态或 frame 当作当前状态。此时仍可用
Show or Start Emacs 叫出已有窗口或重新启动，并可继续使用独立的 Noema 入口。
修改 `tools/raycast-emacs` 的扩展源码后，在该目录运行 `npm run dev`，等显示构建成功后
即可停止监听；`npm run build` 只检查构建，不会刷新 Raycast 中正在使用的扩展。

动作和 frame 操作先取得 Emacs 的排队编号，再在限定时间内查询最终结果。
若 Raycast 已更新而运行中的 Emacs 仍是旧协议，排队前会报错并提示重载
`init-background.el`，不会执行一个无法查询结果的动作。
无效编号、过期 frame、已确认的执行错误或接收前没有应答时，Raycast 提示并保留列表，
方便修正后重试。确认成功后按原交互关闭 Raycast 或刷新 frame 列表；接收后若动作
仍在等待 Emacs 输入，Raycast 提示结果尚未确认，并沿用该入口的退出或停留方式。
后续由 Emacs 接管，避免重复执行。
启动和重启继续在后台进行。隐藏和关闭 frame 会留在列表里，方便连续整理。

在 Emacs 里,`SPC F h`(`my/frame-hide`)只隐藏当前 frame,不删除它,其他 frame 不受影响;
白名单里的 Hide Frame 是同一个命令。隐藏的是最后一个可见 frame 时,Emacs 整个退到后台。

白名单是 `lisp/init-background.el` 里的 `my/background-actions`,每项是
`(标题 命令)` 或 `(标题 命令 参数提示)`。改它不需要重新构建扩展,下次打开列表就生效;
列表之外的命令从 Raycast 里执行不了。M-x 本身只在 Emacs 里用。
命令行传入无效动作编号、frame 编号或操作名会直接报错，不会当成有效操作执行。

动作在后台执行:只有它要读 minibuffer 或改变了主 frame 显示的内容时,Emacs 才会被叫到
前台;自己开 frame 的动作(比如终端)只出现那一个 frame。动作执行失败时会显示 Emacs，
并在消息区写出动作名与错误。限定时间内发生的执行错误也会回到 Raycast；
超过查询时限或需要在 Emacs 中回答提问的动作仍以 Emacs 消息区为最终反馈。

### Emacs 作为后台应用

Emacs 常驻运行,平时没有可见 frame,需要时再叫到前台。入口是
`bin/emacs-app`,Raycast 扩展只是调用它:

- `bin/emacs-app start --background`
  启动并留在后台;Emacs 已经在运行时什么都不做。隐藏在启动完成后才执行
  (`my/background-start-hidden`),不会出现"刚藏起来又弹出来"
- `bin/emacs-app restart` / `kill`
  退出再启动,保持原来的可见或后台状态;Emacs 在等你回答提问时会报错而不是一直等。
  `kill` 给没应答的 Emacs 用:先 SIGTERM(仍会跑退出钩子),20 秒后才 SIGKILL
- `bin/emacs-app state`
  一次返回状态、运行时长、内存、白名单动作和 frame 列表的 JSON;Emacs 没应答或没运行时
  也返回 `{"status":"busy"}` / `{"status":"stopped"}`
- `bin/emacs-app show` / `hide` / `toggle`
  叫到前台 / 送回后台(`my/background-show`、`my/background-hide`)
- `bin/emacs-app actions` / `act ID [ARGUMENT]`
  列出白名单动作，或按编号排队执行；`act` 返回含请求编号的 JSON
- `bin/emacs-app result ID`
  查询请求的 `pending`、`ok`、`error` 或 `unknown` 结果；记录仅在当前 Emacs
  会话中短期保留，重启后不能继续查
- `bin/emacs-app terminal`
  新开一个 Portal 终端,每次都是新实例,单独占一个居中的浮动 frame;在 M-x 里是
  `my/portal-open-new`。Emacs 未运行时会先启动，待图形窗口初始化完成后再开终端。
- `bin/emacs-app status`
  `visible` / `background` / `busy`(在运行但没应答,比如正在启动或等你回答提问)/ `stopped`
- `bin/emacs-app quit`
  先把 Emacs 叫到前台再退出,保证它的确认提问看得见
- `bin/emacs-app frames` / `frame focus|hide|hide-others|close|close-others ID` /
  `frame new|show-all|hide-all`
  列出顶层 frame，或排队操作某一个／全部 frame；`frame` 同样返回请求编号
- `bin/emacs-app install` / `uninstall`
  编译原生模块并安装 / 移除登录 LaunchAgent `local.emacs.background`(登录时后台启动)

`bin/emacs-app` 启动前会清掉 `LC_*` 并保证 `LANG` 是 UTF-8:Emacs 和它启动的进程继承
启动器的环境,而 Raycast 给的 `LC_ALL` 不是合法的 POSIX locale,会让 `pbpaste` 这类工具
退回 C locale,把中文粘贴成一串 `?`。

新 frame(启动时的第一个、Raycast 的 New Frame、`SPC F n`)默认占鼠标所在显示器工作区的
66% × 86% 并居中,由 `my/frame-default-size` 控制(设为 nil 则用 Emacs 自己的默认大小);
浮动终端仍用 `my/portal-floating-frame-size`。

后台是 frame 不可见,不是应用隐藏,所以新开一个 frame(比如终端)时不会把其他 frame 一起带出来。

关掉最后一个可见 frame(`delete-frame`、`:q`、点关闭按钮)不会报错也不会退出 Emacs,
而是把它送回后台。真正退出用 `bin/emacs-app quit`,或 Raycast 列表里的 Quit Emacs。

Emacs 在后台时不占 Dock,也不出现在 Cmd-Tab 里;叫到前台后恢复为普通应用,Dock
图标、Cmd-Tab 和顶部主菜单都回来(macOS 把这三样绑在一起,不能只去掉 Dock 图标)。
由 `my/background-hide-dock-icon` 控制,靠 `tools/macos-background/window.m`
编译出的动态模块实现;没有编译时 Dock 图标始终显示。

同一个模块让每个顶层 frame 的顶部边框可拖动（默认 6 点，
`my/frame-drag-handle-height`）。它不覆盖下面的标签，也不绘制拖动条；
按住原来向上调整窗口大小的边框即可移动无标题栏的 frame。

长期运行时不留孤儿:Noema web host 启动时拿到 Emacs 的 pid(`NOEMA_HOST_OWNER_PID`),
Emacs 崩溃或被强制退出后几秒内自己收尾退出,连同它的 kernel;正常退出时的 SIGHUP 也走
同一套收尾。

`my/portal-open-new` 开的浮动终端,frame 和 shell 同生共死:关掉 frame(关闭按钮、
`delete-frame`、Raycast 的 Close Frame)会结束它的 shell 和 buffer;想留着就用隐藏。
它在 shell 退出时会连 buffer 和 frame 一起关掉;
主窗口里的终端不受影响。

### 后台任务指示器

Mode line 上常驻一只皮卡丘(`assets/activity/` 里的 28×20 XPM 像素图)。它**一直在**,
没事时站着,有后台任务时开始跑,多于一个任务时在后面跟上数字。
**鼠标左键点它打开 `M-x activity-board`。** 终端(无图形界面)里自动降级成颜文字
`ᕕ( ᴗ )ᕗ` / `ᕕ( ᐛ )ᗴ`,不会变成空白。

机理只有一条链路,在 [lisp/init-activity.el](../../lisp/init-activity.el):

1. **枚举的根是 `(process-list)`** —— Emacs 自己的异步子进程注册表,也就是
   `M-x list-processes` 看到的那张表。以这个为根是为了**不用维护**:以后接任何新的
   语言服务器、agent 工具或后台守护进程,它都会自动出现在板面上,这个模块不需要知道
   它的存在。
2. **但它太广,不能直接数。** 里面同时装着三种东西,只有第一种是会结束的活动:

   | 分类 | 内容 |
   | --- | --- |
   | `task` | 有限的工作:compilation、native-comp worker、remote task、文件传输(rsync) |
   | `service` | 常驻守护进程:语言服务器、epdfinfo、agent 会话 |
   | `connection` | network / serial 端点,TRAMP 在内 |

   全数一遍会永远停在几十,没有信息量。所以**板面按三组展示,指示器只数 `task`**。
3. **谁是 task 不靠猜。** 不看进程名也不看命令行,而是问**已经拥有这些工作的注册表**:
   `compilation-in-progress`、`remote-tasks`、`comp-async-compilations`、
   `remote-transfer-jobs`。某个进程是
   task,当且仅当有 owner 认领它。进程任务在 `my/activity--owned-task-processes`
   接入 owner;没有 Emacs 子进程的异步工作通过 `my/activity-provider-functions`
   提供缓存快照。出现在进程板面上不需要另行登记。
4. **两类活动没有进程,单独登记**:native 编译**排队中**(还没 spawn worker)和同步
   阻塞的工作。后者用宏 `my/with-activity` 包住,`unwind-protect` 保证正常返回、报错、
   `C-g` 都会注销。目前唯一的内置触发点是**打开一个每次文件操作都要单独走一趟 shell
   往返的远程文件**(判据是 Remote 后端声明的 `remote-file-operation-cost`,不是
   `file-remote-p`,所以 tramp-rpc 那批快后端不会触发)。
5. **性能约束**:`mode-line-misc-info` 里的 `:eval` 在**每个窗口每次 redisplay** 都跑,
   所以那段只读两个缓存值,从不扫描。扫描在一个**自我重排的单 timer** 里:空闲时按
   `my/activity-poll-interval`(2 秒)慢轮询,一有任务立刻改按
   `my/activity-indicator-fps` 重排。显式登记和 agent 状态通知会立即重新武装;
   普通进程与 Noema Core task 的变化最多等一次 2 秒轮询。

**Noema 也接入了同一计数**,适配器在
[lisp/roam/init-activity-noema.el](../../lisp/roam/init-activity-noema.el):

- LaTeX 导出读取 Noema Core task pool,从排队、转换、agent 润色到编译 PDF 都属于
  一个任务。完成、失败或取消后移出计数。按 `x` 走 Core 的取消接口,`RET` 打开源笔记。
- agent 工作读取 `noema-agent-acp-sessions` 的 busy 状态,空闲会话仍是 service。
  `RET` 打开会话,`x` 通过 ACP owner 取消 Run 或中断当前 turn。导出内部的
  `latex-export` 会话由导出任务覆盖,不会重复计数。
- Core task 快照每 2 秒异步读取一次,不会跟着 8 FPS 发请求,也不会为了监控启动
  Noema。host 停止时清空快照,重启或关闭指示器前的旧回复不会恢复过期任务。

`*Activity*` 板面是**集中入口,不是替代品**。页面用 ETAF 保留布局、焦点和刷新状态，
颜色和图标仍取自 Aaron UI 的原有主题。Emacs 自己给每种后台工作都带了管理器,
每个都比重写一遍更称职,所以板面负责「什么在跑 + 能对它做什么」,更深的细节一键交给原生工具。
任务数据是全局的；每个 frame 有自己的 Activity 面板 buffer，筛选、定时器和阅读位置
各自保存。`q` 仍用 Emacs 原生窗口返回；关闭 frame 时释放它的面板。
ETAF 只在第一次打开这个整页时加载，常驻 mode line 和 Dashboard 不会因此加载它。

**移动和其它 board 完全一致**:`j`/`n` 下移,`k`/`p` 上移,`TAB` 下一个按钮,
`RET` 打开当前行。停止类操作**刻意避开导航键**,放在 `x`/`i`/`X`。
按到没绑定的字母在 echo 区列出可用的键。ETAF 行焦点和鼠标点击都调用原来的
进程／任务动作；`j/k` 与停止键由 Activity 页自己的快捷键层提供。

按行操作:

| 键 | 作用 |
| --- | --- |
| `RET` | 打开当前行(传输任务 = 队列日志；其他进程 = 进程 buffer；按钮行 = 按下按钮) |
| `v` | 同上,显式跳 buffer |
| `d` | 详情:Emacs 侧字段(status/pid/type/buffer/tty/query/command)+ **OS 侧 `process-attributes`**(rss、etime、nice、majflt…)；详情页 `q` 返回 Activity 并释放临时 buffer |
| `w` | 复制命令行到 kill ring |
| `x` | 停止。有 owner 的走 owner 的取消路径(如 `remote-task-cancel`);**没有任何注册表认领的进程会先问一遍再杀** |
| `i` | Remote Task／Transfers 走 owner 取消接口，确保远端任务和传输队列记录结束状态；其他进程发送 SIGINT |
| `X` | 先确认；Remote Task／Transfers 走 owner 取消接口，避免只杀本地 relay 而留下远端工作；其他进程发送 SIGKILL |

视图控制:

| 键 | 作用 |
| --- | --- |
| `g` | 刷新 |
| `a` | 自动刷新开关(**默认关**) |
| `/` | 按名字/命令行过滤,空输入清除 |
| `t` | 显示/隐藏 Emacs 定时器一节 |
| `q` | 关闭 |

跳到原生管理器:

| 键 | 去哪 |
| --- | --- |
| `P` | `list-processes` —— 内置原始进程表,字段最全 |
| `T` | `list-timers` —— 内置定时器表 |
| `H` | `list-threads` —— 内置线程表 |
| `O` | `proced` —— 内置 **OS** 进程管理器,看这个 Emacs 之外的东西 |
| `M` | `memory-report` —— 内置内存报告 |

板面底部还有两排按钮:「Built-in managers」(上面五个 + `*Messages*`)和
「This configuration」(`my/performance-watch`、`remote-board`、`my/compile-board`、
`my/language-server-manager`、`config-board`)。按钮只在对应命令存在时出现。

**性能**(这一页的硬约束,改之前先读):

- **`process-attributes` 是每进程一次系统调用,绝不能进渲染路径。** 它只在两处被调用:
  `d` 的详情页,以及某个进程**第一次**被看到时取它的真实启动时间(之后永久缓存在
  弱键表里)。实测:首次渲染每个活进程 1 次调用,之后 10 次渲染 0 次。
- 页面刷新只读取现有进程、provider 快照和可选的定时器列表，不在 ETAF 的
  响应式排版里执行网络请求；详细 OS 属性仍只在按 `d` 时按需读取。
- **自动刷新默认关**。打开后是一个受 `my/activity-board-refresh-interval` 约束的
  timer,只在所属 frame 的板面**可见**时重绘；板面隐藏后下一次计时即停止，
  buffer 被 kill 或 frame 关闭时立即取消。

要改皮卡丘的样子、速度或给别的慢操作也加上,见
[settings-cookbook.md](settings-cookbook.md#我要改后台任务指示器).

## 7. 有冲突时优先记住什么

- `M-w` 关闭当前 buffer，行为与 `C-x k` 一致
- 关 frame 用 `H-w`
- 普通 warning 现在只写入 `*Warnings*`，不再自动弹窗抢操作；需要时用 `SPC h w`
- `C-c y` 现在是 snippet 前缀，不再直接展开
- `C-c n` 是 Typst note 前缀，不再给 centaur-tabs

## 8. Noema AI 与 agent

Noema 统一承接轻量模型交互与结构化 coding-agent 会话。gptel 是直接复用的 compose/context/rewrite UI；agent-shell + ACP 是结构化 session/stream/permission 路径；Magent 提供本地 agent、queue、ledger 和 gptel adapter。

| 键 | 功能 |
|----|------|
| `C-c A W` | 打开 Noema（默认 Magent agent-shell） |
| `C-c A a` | 选择 Magent/Codex/Claude/OpenCode/Pi agent-shell |
| `C-c A w` | 在当前 Project 仓库的新 Git worktree 里启动 Codex/Claude/OpenCode 会话 |
| `C-c A n` | 同一任务交给几个 agent 并行尝试，各自一个 worktree |
| `C-c A A` | 并行尝试看板：比较各尝试的状态与改动 |
| `C-c A c` | 打开 gptel compose buffer；在 Noema 页面里带上选区（没有选区则整篇笔记）作为 context |
| `C-c A s` | 从当前 buffer 发送 gptel 请求；在 Noema 页面里同 `C-c A c` |
| `C-c A m` | 打开 gptel transient 设置 |
| `C-c A .` | 把 region/buffer/file 加入 gptel context；在 Noema 页面里加入选区，没有选区则加入整篇 |
| `C-c A r` | gptel rewrite/diff 预览；在 Noema 页面里改写选区（没有选区则光标所在行） |
| `C-c A e` | 在 Emacs 源 buffer 里打开 Noema 页面的笔记，并选中页面选区 |
| `C-c A p` | 把当前 agent-shell session 纳入 research |
| `C-c A G` | 跨 Project 的 agent 总览：待处理、未读、运行状态与会话跳转 |
| `C-c A U` | agent 总览（abtop）：额度、会话状态/模型/上下文/token/费用，停止、强制终止、跳转 |
| `C-c A O` | 编排面板：Task / Job / Worker / Delegation / 事件 |
| `C-c A x` | 把当前上下文发给某个 agent 会话（引用，不拷贝正文）|
| `C-c A v` | 只把选区发给某个会话；在 Noema Markdown 页里直接使用网页选区 |
| `C-c A B` | 只把当前 buffer 的文件发给某个会话（Noema 页面里是整篇笔记） |
| `C-c A f` | 只把某个文件发给某个会话 |
| `C-c A @` | 告诉会话光标在哪（文件:行:列 + 所在定义；Noema 页面里是光标所在行）|
| `C-c A ,` | 发送前检视/删减上下文 |
| `C-c A d` | 同 `C-c A x`，但只塞进输入区不提交 |

Noema Markdown 页的选区工具栏有 **Agent** 和 **Rewrite** 按钮，`...` 里还有
Add to AI context、gptel compose、在 Emacs 源 buffer 中选中；`H-o %` 是同一组操作的
Transient。所有这些都先保存笔记（发送整篇笔记时不需要页面应答，直接用磁盘上的文件），再把选区的准确位置（行 + 字符列）交给
`noema-md-bridge`，由它在笔记的 Emacs 源 buffer 上运行对应的 gptel / agent UI。
Rewrite 的 diff / ediff / accept 审阅在源 buffer 里完成；源 buffer 处于
`noema-md-bridge-source-mode`（mode line `Noema↔`），修改在空闲
`noema-md-bridge-autosave-delay` 秒后保存，页面随即重载；`C-c C-c` 保存并回到页面。
页面没有选区等无法完成的情况会直接在 echo area 说明原因；页面
`noema-md-bridge-answer-timeout` 秒内没有应答时 Emacs 也会提示（通常是页面需要
`H-o r` 刷新或 `H-o B` 重建）。
Visual 模式借鉴 LaTeX 的段落节奏：首个空行清楚分段，连续空行稍微收紧；每个源文件空行仍可见、可编辑，移动光标不会改变它们的高度。源文件的换行在编辑视图中始终保持换行，避免输入一个字符后与下一行合并。

### Noema Markdown 编辑行为

依据 [Marker / MarkText / files.md 审计](markdown-editors-noema-audit-2026-10.md)：

- 格式是**切换**：`Cmd-B` / `Cmd-I` / `Cmd-Shift-X` 与选区工具栏在已有格式内再按即移除；选区首尾空白留在标记外；多光标逐个生效；中文标点旁的 `**（注）**` 也会渲染。`Mod-\` 或工具栏 `Tx` 清除选区内全部行内格式，工具栏按钮高亮显示已生效的格式。
- 标题、引用、列表命令作用于选中的所有行，同类型再按即还原；快速插入里有“Promote / Demote heading”。
- 回车：勾选过的任务续出未勾选的新任务；```` ```lang ```` 行尾回车自动补闭合围栏；单行 `| a | b |` 回车生成表格；代码块里 `{}` 之间回车展开缩进，`Tab` 在光标处缩进，`Mod-Enter` 跳出代码块。
- 粘贴看落点：代码里粘纯文本，表格行里换行变 `<br>`，链接 `](…)` 里只取 URL，选中文字粘 URL 成为链接；表格网页/Excel 粘进来是 Markdown 表格。拖入图片或 PDF 会存入附件并在落点插入链接。
- 撤销按词：连续输入在空白处、输入与删除切换处分段。
- 页面查找栏默认智能大小写（查询含大写才区分），`Aa` / `W` / `.*` 切换区分大小写、全词、正则，`⇄` 打开替换（`Enter` 替换当前，`Alt-Enter` 全部替换）。
- 范围选择不再把经过的标记、图片、图表展开成源码；只有光标所在处显示源码。
- CRLF 文件按原换行保存；保存失败会自动按退避重试，状态栏显示倒计时。
- 列表/引用：空项回车退出时自动留空行，之后输入的文字不会并入上一项；嵌套空项回车只退一级。`Shift-Enter` 在列表项/引用内换行（保留缩进或 `>`，不新建项目）。单独一行的 `<details>`、`<div …>` 等块级标签回车自动补闭合标签。
- `Tab` 在行内格式内容末尾（`**粗|**`、`` `码|` ``、`[文字|](url)`、`\(x|\)`）直接跳到标记之后。
- 表格：方向键/退格在单元格文字边缘跨格移动，越过表格回到正文；`Mod-Enter` 在下方插入一行。拖选、`Shift` 点击或 `Shift+方向键` 选中矩形单元格：`Mod-C` 复制为 GFM 子表格，`Delete` 先清空、再删整行/整列/整表，`Mod-A` 依次扩到整表、全文。`|-|:-:|` 这种短分隔行、以及首尾不写 `|` 的 `a | b` 表格也会渲染；表格体一直延续到空行或下一个块，表格正下方紧挨着的普通文字会成为一行（与导出一致），需要分开时空一行。
- `:smile:` 这类 emoji 短码在编辑视图直接显示；输入 `:sm` 弹出 emoji 补全。`![](clip.mp4)` / `![](talk.mp3)` 渲染为可播放的视频/音频（不自动播放）。
- 图片后按退格（或图片前按 Delete）先选中整张图片，再按一次删除，不会把图片拆成半截源码。
- 图表（` ```mermaid `、` ```marmind `/` ```markmind `）按图片排版：高度跟着内容走，没有固定窗口，点一下图就像点别的正文一样把光标放进去显示源码。需要放大时用图右上角悬停出现的 `⤢` 打开查看器，在那里缩放、平移、`Fit`，`Esc` 或 `Close` 关闭；缩放只属于查看器，不会被正文编辑打断。
- 图表配色跟随当前主题：Mermaid 用从 `--aaron-*` 变量推出来的调色板（深色主题就是深色图），不再是浅色默认主题垫一张白卡片。换主题会自动重画。
- `![说明](page.html)` 嵌入一个 HTML 页面（沙箱 iframe，不共享 Noema 的存储）。它在滚动到附近时才加载，
  离开视口约 90 秒或窗格隐藏约 60 秒后卸载为空白页、回来时重新加载，所以一篇笔记里多个交互页面
  不会一直在后台跑。图片悬停工具条的 `⤢` 全屏显示（同一个页面原地放大，状态保留），`Esc` 或 `×`
  退出。页面可用 `parent.postMessage` 与 Noema 通信：`{type: "noema:open-url", href: "roam://<id>"}`
  打开笔记，`{type: "noema:resize", height: 480}` 调整高度（笔记里写了高度时忽略），
  `{type: "noema:fullscreen"}` 切换全屏。
- `![说明](diagram.drawio)` 像引用图片一样引用 draw.io 文件：Noema 调本机 draw.io 导出 SVG 并缓存，按原图大小显示，不加载在线编辑器、不联网，导出时按当前主题要 light 或 dark 版本。要改图就用 draw.io 打开那个 `.drawio` 文件，存盘后 Noema 自动重新导出。`#page=2` 指定第几页（从 1 数）；本机没装 draw.io 时显示一张写明原因的占位图（可用 `NOEMA_DRAWIO_BIN` 指定路径）。`.drawio.svg` / `.drawio.png` 本来就是图片，直接当图片渲染。
- draw.io 导出一次约 1 秒（要起一个 draw.io 进程），之后按「文件路径 + 修改时间 + 大小 + 页码 + 格式」命中磁盘缓存，命中是毫秒级，重复渲染只走 ETag 304。同时导出的并发上限默认 2（`NOEMA_DRAWIO_CONCURRENCY`），一篇笔记里十几张图不会同时拉起十几个 draw.io。
- LaTeX 导出里图表就是图：`.drawio` 由 draw.io 直接导成矢量 PDF，mermaid/marmind 由编辑器页面渲染成 SVG 随请求发过去、再用 `rsvg-convert` 转 PDF；两者都作为附带文件写到 `.tex` 旁边，所以存下来的 `.tex` 单独也能编译。某张图没能成图时保留原样并在任务警告里说明，不会整篇导出失败。
- 发布/独立 HTML 里 `.drawio` 的 SVG 直接内联（导出 URL 只在本机宿主下有效，独立文件里会是死链），和 TikZ 一样的处理。server 模式同样能取 draw.io 导出；marmind 在 server 模式本来就由页面自己渲染。
- 右键菜单提供“Duplicate / Move Up / Move Down / Delete Block”。Markdown 中输入关键词可从 snippet 补全展开结构：`h1`–`h6` 标题、`ul` / `ol` 列表、`bq` 引用、`math` 公式块、`hr` 分隔线、`mer` Mermaid、`mind` 思维导图等；`/` 是普通文本，不弹命令菜单。悬停脚注引用显示脚注内容，未定义的会提示。

### 所有 agent 会话按项目统一登记

`.noema` work 块起的会话、popup 里的 agent、`C-c A a` 手动开的、以及直接
`M-x agent-shell` 开的，全部会被登记到同一个按项目组织的注册表：有项目 root、
有会话名（例如 `popup/claude`、`foreign/codex-2`）、有来源标记。因此
`C-c A S`（列表）和 `C-c A b`（切换）能看到并管理全部会话，`C-c A x` 这类
命令也可以点名把上下文交给其中任意一个。

会话列表（`C-c A S`）第一列是注意力：`!approve`/`!input` 表示正等你决定，
`failed` 表示最近一次 Run 失败（读过也不会消失，要等后续成功的 Run），`new`
表示有你还没看的已结束 Run；需要你的行排在最前。Last Run 列带失败类型，
`retry` 表示限流、断网、租约丢失这类可原样重试的失败。按键：`u` 标记已读，
`!` 跳到下一个需要处理的会话，`R` 重跑失败 Run 的 work 块（不可重试的失败会先
确认），`s` 在该会话旁开侧聊（同 agent、同目录、不带历史，不进持久注册表，
闲置且隐藏后自动回收；agent 标签里是 `C-c C-q`）。运行中但 30 秒无输出的会话
显示为 `running, idle 45s`。

多个 work 块可以同时执行（`noema-agent-worker-max-concurrent-runs`，默认 3，
设为 1 即完全串行）；mode line 的 `Noema[▶2 ⋯1 !1]` 分别是执行中、等待执行
槽位、等你决定的数量。两个仍在运行的 Run 要改同一个文件时，后一个的编辑请求
不会自动批准，会带着原因（`why:`）进入 Attention；Attention 里编辑请求会显示
`+N -M` 行数和 `diff` 按钮。Emacs 不在前台时，等待决定和 Run 结束会发系统通知
（`noema-agent-worker-notify-function`，设为 nil 关闭）。

没有 Run 的会话（`C-c A a`、`C-c A w`、popup、恢复的对话）也会提醒：它请求权限或
答完一轮时，如果它的 buffer 不在屏幕上，Sessions 列表和总览的第一列会标
`!approve` 或 `done`，`!` 能跳到它，打开它或按 `u` 清除；Emacs 不在前台时还会
发系统通知（`noema-agent-acp-notify-function`，设为 nil 关闭）。你自己打断的一轮
不算。这个标记只存在当前 Emacs 里；Run 的提醒仍以 host 为准，不会重复报。

`C-c A G` / `M-x noema-agent-inbox` 打开全局 agent 总览。它汇总笔记根目录中
已有的 Noema Project、Emacs 已知的项目和当前打开的 agent；只有带 `[project]`
的清单才进入持久会话查询，普通仓库中的活动 agent 只作为本地行显示。
总览按待审批、待输入、失败、未读优先排序。`RET` 打开会话，`j` 跳到最近 Run 的
work 块，`p` 打开其 Project，`s` 打开该 Project 的 Session 列表，`g` 刷新状态，`G` 重新发现
Project。`!` 跨项目跳到下一个待处理或未读 Session；`u` 标记当前 Session 已读，失败状态仍保留。
可见时，活动 agent 的状态变化会自动重绘；本地 Run 的完成及权限、输入请求变化会合并刷新
所属 Project 的会话；`u` 成功后也只刷新所属 Project，保留其他 Project 的列表。隐藏后停止查询，再次显示时补查；其他客户端
造成的变更可按 `g` 获取。
总览只读取现有注册表，不会启动 agent 或创建 Project；标记已读是显式操作。

同一仓库里同时开几个写代码的会话时，用 `C-c A w` / `M-x noema-agent-worktree-start`
给每个会话一个自己的 Git worktree：它从 Project workspace 当前所在的分支切出
`noema/<名字>` 分支，worktree 放在仓库旁边的 `<仓库名>.noema-worktrees/<名字>/`
（不在仓库内，也就不会出现在仓库的 status 或搜索里）。会话仍登记在原 Project 下，
名为 `worktree/<名字>`；恢复会话时回到它原来的 worktree。本机和远程 Project 用同一套命令。
用 `C-c A x` 等命令把主 checkout 里的文件或选区发给这个会话时，引用会指向 worktree
里的同名文件，避免 agent 改到主 checkout；worktree 里没有这个文件就报错，不会退回主 checkout。
选区按你正在编辑的 buffer 计行号；如果 worktree 里那几行已经不一样，会直接拒绝，并提示你到 worktree 的副本里重新选择。
Sessions 列表（`C-c A S`）和总览（`C-c A G`）里，`m` 打开该会话所在 checkout 的
Magit status；`d` 显示它从分出时起的全部改动（已提交和未提交的一起），
普通会话则只显示未提交改动。`M-x noema-agent-worktree-remove` 只列出 Noema 建的 worktree：
还有会话在里面工作的不删；有未提交改动的要加前缀参数才删；分支始终保留，
已提交的工作不会丢。

想让几个 agent 同时做同一件事、再挑一个结果时，用 `C-c A n` /
`M-x noema-agent-worktree-parallel`：输入组名、选 agent（同一个选两次就有两份尝试）和
一句提示。每份尝试是一个 worktree 会话，分支名 `noema/<组名>-<序号>-<agent>`，提示在会话
就绪后自动发出；某个 agent 起不来只会报告，其余照常开始。`C-c A A` /
`M-x noema-agent-worktree-board` 打开看板，每行一份尝试：组名后括号里是整组状态
（有一份在等权限就是 waiting，否则有一份在做就是 working，都做完或会话已关就是 settled），
然后是 agent、这份尝试的状态（waiting / working / done / idle / closed）和改动（自分出起的
提交数、工作区相对分出点的文件与行数、新增的未跟踪文件数）。改动在打开看板和按 `g` 时
重新从 Git 读取，状态则随会话事件即时更新。`RET` 打开该会话，`m`/`d` 用 Magit 查看，
`k` 删掉这份尝试的 worktree，`K` 删掉整组（有会话还开着就不删；有未提交改动时需要前缀
参数，否则一份都不删）；分支一律保留。分组信息记在各分支的 Git 配置里
（`branch.<分支>.noemaGroup`），重启 Emacs 后照样在；挑中的结果由你自己用 Magit 合并。

`C-c A U` / `M-x noema-agent-abtop` 打开 btop 风格的 agent 总览（aaron-ui 配色），三块面板：
- **quota**：三个工具的额度总是全部列出，每个工具列出它应有的窗口（`5h`/`7d`），显示已用比例、重置倒计时
  和数据年龄；还没有数据的窗口显示为一行说明，而不是消失。
  - claude：两个来源写进同一张表。一是 claude-agent-acp 在 `usage_update` 里附带的 `_claude/rateLimit`
    （一轮对话中上报，新版带 `unifiedWindows`，一次给出 5h 和 7d）；二是打开面板或按 `g` 时向 Claude 的
    usage 接口（Claude Code `/usage` 用的同一个）要一次，5 分钟内不重复请求。令牌从 macOS 钥匙串
    （`Claude Code-credentials`，异步 `security`）或 `~/.claude/.credentials.json` 读取，只用于这一次请求，
    不保存、不写日志、**从不刷新**（刷新会轮换 Claude Code 的 refresh token 让它掉登录）；过期时面板提示
    打开 Claude Code 续期。`noema-agent-abtop-claude-usage` 设为 nil 可完全离线。最后一次的值存进
    `var/noema/agent-rate-limits.json`（值变了才写，或每 5 分钟一次保持数据年龄准确）。
  - codex：本机 Codex 会话文件（`$CODEX_HOME/sessions`）最新一条 `token_count`；当前套餐没有的窗口显示为缺失。
  - opencode：按 `~/.local/share/opencode/auth.json` 里登录的 provider 列出（只读 provider 名，不保留凭据）：
    openai（ChatGPT 登录）与 codex 共用额度；github-copilot 的 premium 额度本地没有记录。
- **sessions**：所有活动会话（Run、popup 池、手开的）一行一个：状态（`● work`/`◐ wait`/`○ idle`，
  `+N` 是排队的 prompt）、模型、上下文条（≥70% 黄、≥90% 红）、输入/输出 token、费用、轮数、
  压缩次数（上下文较上次骤降 30% 以上记一次）、空闲时间。窄窗口会依次隐藏 Cmp、Agent、Project 等列。
- **session**：光标所在会话的详情：root、会话 id、上下文 used/size 与峰值、token 分项、费用、队列。

按键：`RET`/`o` 打开会话，`s` 停止当前工作（取消 Run 或中断本轮），`K` 强制终止（进程和 buffer
一起结束），`x` 关闭，`R` 重启，`r` 重命名，`C` 在下个 Run 滚动到最新 handoff，`l` 该项目的会话
列表，`t` 切换项目范围（`C-u C-c A U` 直接只看当前项目），`TAB` 在面板标题上折叠/展开（其他位置跳到
下一个会话），`1`/`2`/`3` 折叠 quota/sessions/session，`g` 刷新。光标不在会话行上时，操作作用于详情面板里的会话。
数据全部来自 Emacs 已有的状态（`noema-agent-acp-usage`）：不扫进程、不轮询；面板在可见的 frame 上显示时，
会话开启/关闭、模型切换、开始/结束一轮、上报用量之后合并 1 秒重绘一次。面板被切走、埋掉或 frame 最小化后，
下一次变化到来时它就退订，之后完全不耗电；重新显示时再订阅并补画一次。agent 不上报的字段显示 `-`；刚启动、还没完成
握手的会话模型也是 `-`，握手完成后自动补上。

项目 root 取最近的、带 `[project]` 表的 `noema.toml`（只有 `repository_id` 的
vault 清单不算项目），没有就退回普通项目根目录；**不会**为了登记而创建项目。
一个 vault 里可以有多个项目；项目的 agent 在它的 workspace 里运行，默认就是项目
根目录，也可以用 `M-x noema-project-set-workspace` 指到别处的代码仓库。在真正的 Noema 项目里，会话还会额外写进持久注册表，于是它
和 Run 的会话一样可以重命名、fork、归档、看 context 用量；在普通仓库里它只存在
于 Emacs 侧，列表里显示为 `local`。`M-x agent-shell` 在当前窗口启动并登记到 Noema；
其他程序启动的原生 agent-shell buffer 仍由
`noema-agent-acp-adopt-foreign-sessions` 控制是否收编。

### 会话不会被后台自动关掉

自动回收只有两条路径：每 5 分钟的 warm-buffer 清扫（空闲超过
`noema-agent-worker-warm-idle-seconds`，默认 30 分钟），以及项目最后一个 `.noema`
关闭 `noema-pi-stop-delay` 秒后的项目收尾。Pi 是项目管理会话，不参与 warm-buffer 清扫；
项目收尾仍会关闭它。两条路径都先问
`noema-agent-acp-auto-stoppable-p`：会话必须空闲（没有进行中的回答或 Run）、不在
任何窗口可见，且来源属于 `noema-agent-acp-auto-stop-origins`（默认 `run`、`probe`、
`pi`）。空闲时间从 buffer 最后一次变化（输入或输出）算起，而不是上次被显示的
时间。所以你自己开的会话（popup、`C-c A a`、裸 `M-x agent-shell`）只会在你
`C-c C-k` 或交互式 `noema-pi-close-project` 时关闭；进行中的回答任何时候都不会被
自动停止。被停止的会话名与 native id 仍在，下次可恢复。

### 把 buffer / 选区 / 光标交给会话

上下文的挑选仍然是 gptel 那一套（`C-c A .` / `C-c M-a` 的 `gptel-add`：选区、
buffer、文件、Dired 标记，带 overlay 高亮），**只有一份**选择，既可以给 gptel
compose 用，也可以用 `C-c A x` 交给某个 ACP 会话。

交出去的**永远是引用，不是正文**：选区变成 prompt 里的一行 `path:12-40`（就是
agent-shell 自己写引用的格式），每个文件再附一个 `resource_link` 内容块
（`file://` 绝对路径 + 相对名 + mime + 字节数）。agent 用自己的文件工具去读需要
的部分，所以挂一个三千行的文件只花一行 prompt。因此被引用的 buffer 必须已保存；
有未保存改动时会先问你要不要保存，跳过的条目会在 echo area 列出来
（`noema-context-save-before-send` 可改成总是保存或总是跳过）。

`C-c A v` / `B` / `f` / `@` 只发送命令点名的那一段，不会顺带发出之前用
`C-c A .` 攒下的共享 context；要发送攒下的全部内容用 `C-c A x`。

每次发送都会询问目标会话，只列出 ACP 进程仍在运行的 agent-shell 会话，本项目
优先；本项目上一次选的会话若仍在运行则排在第一位并预选，直接 `RET` 即重复选择。
历史会话不会出现在发送菜单中；需要先在 agent-shell 输入 `/resume` 恢复会话。
菜单最底下的 `Copy prompt to clipboard` 把同一段提问和文件行号引用复制到系统
剪贴板，可直接粘贴到 portal 里的 agent；即使没有打开 ACP 会话也可以选。
这项操作不会切换会话或清空已选的 context。
想恢复“静默复用上次会话、`C-u` 才重选”，把 `noema-context-always-ask-session`
设为 nil。会话还在初始化就先排队；正在回答时走 agent-shell 自己的
pending 队列。

在 agent-shell 输入 `/resume` 并回车，会读取当前 agent、当前工作目录的原生
ACP 会话列表，排除当前会话并按最近使用排序，然后选择要恢复的会话。已有活动 buffer 会直接
切换到它；未打开的历史会话会替换当前 Agent 或 popup 窗口中的 buffer，不增加新 tab。
Noema 已登记的原生 session ID 会沿用原会话名。Codex 与 Claude 的会话
历史由各自的官方后端保存，Noema 只登记名字与原生 ID，不复制 transcript。

`M-x agent-shell` 从当前工作 buffer 启动 Noema 管理的 agent，并显示在当前窗口；
顶部 `+Agent` 和 `M-x my/agent-shell` 才在全局 popup 中打开。两条入口使用同一套
项目目录、target、环境和会话登记。
`C-u C-u M-x agent-shell` 保留上游的已有会话选择器。

同一项目的 agent/session 以 tab 形式共用右下角一个 Agent 窗口；每个 tab 都是
真正的 agent-shell buffer，可以直接输入、`C-c C-c` 中断。Noema Run 结束后会补回
输入提示符，`C-c C-e`（任意位置 `C-c A i`）跳到输入处。这些 buffer 不进入全局
tab-line/tab-bar。

| Agent 窗口按键 | 作用 |
|----|------|
| 左键 / 右键 tab | 切换 session / 管理菜单（含关闭已退役 tab） |
| `?`（输入区外）、`C-c ?` | 全部按键帮助 |
| `C-c C-a` / `C-c C-n` / `C-c C-p` | 选择 / 下一个 / 上一个 session |
| `C-c C-e` | 聚焦输入提示符 |
| `C-c C-x` | 停止当前 Run 或回合 |
| `C-c C-r` | 重启 session 并恢复原对话 |
| `C-c C-k` / `C-c M-k` | 关闭当前 tab / 关闭其他 tab（名字与历史保留） |
| `C-c C-w` / `C-c C-f` / `C-c C-d` | 重命名 / fork / 归档 session |
| `C-c C-t` | 当前原生 ACP 对话树 |
| `C-c C-j` / `C-c C-l` / `C-c C-z` | 跳到最近的 work 块 / session 列表 / 管理菜单 |

`C-c A T` 可从当前项目上下文打开 agent 的对话树；Sessions 列表中按 `T`
打开光标所在会话的树，已关闭的原生会话会先恢复。树里 `g` 发现和更新分支，
`C-u g` 全量重建，`/` 搜索当前对话，`s` 标注回合，`d` 看工具 diff；在
`↳` 原生会话行按 `RET` 继续、按 `f` 从该会话末端创建原生分叉。新分叉进入
Noema 的命名会话列表，可以重命名并作为 `@@session` 使用。普通历史回合只供
预览，通用 ACP 不能从任意旧回合重新开始。树按同一 agent 和执行目录发现候选，
只显示与当前对话共享非空历史前缀的会话；共享前缀不等于后端真实的父子关系。
树索引仅保留在当前 Emacs 内存，不另存一份对话正文。后端需支持 ACP
`session/list` 与 `session/load`。Noema 的 `F` 是为下次
Run 声明新命名会话，与树里的原生 `f` 不同；WorkNode DAG 仍描述研究任务依赖。

重跑同一个 work 块会按上游重新开始，不会叠在上一次尝试后面（独占的 session 沿用原名换新一代）。

`.noema` 的 DAG 里，`!` 把选中的 work 标成 `regressed`——「本来验证过、被后来的改动打坏了」。
它会问你什么坏了（记在该节点上），并把状态传给合并工作 DAG 里所有 `done` 的下游节点
（lineage 与 `depends` 都算）：下游要重新声明 done，而不是继承。进行中和已放弃的工作不受影响。
regressed 节点不会被变暗也不会被自动折叠，画上用 `✗` 和警示描边标出。通用的 `t`（状态）
命令选 `regressed` 走的是同一条传播路径。

`.noema` 的 DAG（Graph Board）：`f` 以选中节点为根聚焦（`^` 根上移一层、`[`/`]` 调深度、
`b` 回到上一个焦点）；`h/j/k/l` 按画面移动，`H`/`L` 到 lineage 父/第一个子，`{`/`}` 到兄弟，
`/` 按标题跳转。work 块里 `@@ctx(lineage:2)` 扩大祖先范围，`@@ctx(none)` 关闭自动附加的上下文；
自动附加的上游结论超出 64 KiB 预算时会被截断或省略（写进 RunSpec），不会让 Run 失败。
运行前可在 work 块上按 `C-c j p`（或右键 Preview context）预演：显示将用的 agent、会话、
推导原因，以及每条上下文的字节数、是否自动附加（auto）/被截断（cut）、被省略的条目和总量；
不会真的运行或触发 compaction。
Sessions 列表（`C-c A S`）的 Context 列显示上下文窗口占用与 token 总量，`c` 让该会话下一次 Run
从最近的 Handoff 重建对话。列表里 State 为 `local` 的行是只登记在 Emacs 侧的会话
（popup、手动、裸 agent-shell，且项目没有 `noema.toml`）：`RET` 切过去、`k` 关掉都可用，
重命名 / fork / 归档 / compact 需要持久记录，会明确报错。
项目内的读写与执行自动批准；项目外和网络请求会弹出 Noema Attention 由你批准。

Inspector（节点上 `C-c C-i`）现在除了结构错误，还会提示**没有依据的断言**：标了 done 却
既没有 Run 也没有 outcome、标了 done 但最近一次 Run 是失败的、done 压在一个 regressed
上面、标了 active 却没有任何 Run。这些只是提示——它们走的是 warning 通道，`structure-edit`
只回滚新引入的 error，所以提示永远不会挡住你的编辑。

`C-c j p` 的运行预演会告诉你这个块正压在什么上面：有几个依赖没完成、有几个已经 regressed、
以及这个块是不是已经标了 done。同样只是提示，Run 照常能起。

技能库在 `~/Documents/Noema/public/README/Skills/`（放在 README 命名空间里，因为那才是
覆盖它的 git 仓库），按 Portable Agent Plugins 结构组织：根目录有 `plugin.json` 与
`mcp.json`，技能在 `skills/` 下——每个技能一个目录、一份
`SKILL.md`，深度放 `references/` 里按需读。加一个技能就是新建一个目录，下一次 Run 就能
`@@skill(<id>)` 选到，不需要发版。描述 Noema 自身机制的技能（`noema-work-dag`）仍随代码走。
在 `.noema` 的 work 块开头输入 `@@skill(` 后，Company 会列出当前项目可选的
Skill；首次打开时会异步加载，随后自动更新。列表与 Skill 管理器使用同一份
能力解析结果，但管理器还展示无效或明确禁用的项目。普通正文不再自动混入
TeX/Markdown snippet；需要模板时用 `C-c y y`。结构问题由 Flymake 在块标题
标出，数学公式由 RaTeX 在光标处预览，代码围栏和控制行不会被当成公式。

MCP 现在是两个面：知识库（笔记/搜索/标签）和 AI 流程（work DAG/Run/artifact/Proposal）
分别在 `/mcp` 和 `/mcp/research`，能力 id 是 `noema-knowledge` 和 `noema-research`，
可以单独启用。

动手之前 agent 可以用 `proposal.create` 的 `graph.declare` 把**整张计划图**一次提出来：
块之间可以互相引用，Graph Board 按声明的形状画成一组虚线幽灵节点，你一次接受或否决整张图
（写入走单次 revision 比较交换，所以不会留下半张计划）。加单块仍用 `cell.create`。

Run 里的 agent 可以用 `research_state` 汇报**它自己那个 WorkNode** 的状态
（`active` / `waiting` / `done` / `regressed` / `dropped`，带一句理由或证据），改不了别的节点。
kernel 只记录这份汇报，真正落盘的是 Emacs：它把请求当成一次普通的、可撤销的结构编辑应用，
所以文档权威仍然只有一个。`done` 要带证据、坏了先标 `regressed` 再修这套纪律写在内置
Skill `noema-work-dag` 里，可以用 `@@skill(noema-work-dag)` 给某个 work 块启用。

取消：JuText 里 `C-c C-z` 只取消光标所在 work 块的执行，运行中、排队中、正在准备都有效；
网页 Cancel Run 对还在准备的 Run 立即生效；Agent 窗口 `C-c C-x` 停止当前 Run，`C-c C-c`
直接中断当前回合，不再询问。取消时待批准的权限一并撤回；agent 3 秒内不响应取消时会强制
停掉它的进程，session 名和历史保留，下次使用时恢复。

可编辑 profile 与 prompt 资源位于 `etc/noema/`。一次性 CLI sampler 仅保留为
gptel backend 的降级；不再有独立 interaction Hub/transcript/session UI。选区、buffer
和文件的挑选一律使用 gptel 的 context UI，去向由你决定：gptel compose，或
`C-c A x` 交给某个 ACP 会话。完整上游源码位于
`site-lisp/noema/upstream/`；当前代码不得回退到旧 workbench 命名或另一份外部 checkout。

## 9. Jupyter cell —— 普通笔记与 kernel

本节只描述普通 Markdown `@@cell` sidecar 与 `.ipynb`。D-023 `.noema` 是
AI prompt/工作流程文档：按 `C-c C-c` 运行 work 块时走 ACP agent，回复进入右侧
OutputArea 并保存到该块 outputs；它没有 kernel 选择、重启、Run All 或编程代码块。

笔记里的 `@@cell(language, session) [id]` 块由 Noema 渲染，**源码、cell 结构和
运行逻辑由 Noema 管理**：在 cell 上点 Edit，Noema 会打开笔记旁 `.cell/` 下的
标准 `NOTE.LANGUAGE.SESSION.ipynb`；普通 ipynb 也走同一套 UI。磁盘上始终是
nbformat 4.5；Emacs 只提供可编辑的 percent-style 源码投影和
`my/noema-jupyter-cell-mode` 控件，通过 Noema API 操作 notebook。

`LANGUAGE` 是语言而不是 kernel 名：SageMath kernel 归在 Python 语言下，文件名为
`NOTE.python.SESSION.ipynb`，而 `sagemath` 保存在 notebook kernelspec 中。

新建 `.ipynb` 不会留下空文件：`find-file` 一个还不存在（或被别的工具建成空的）
`.ipynb` 时，会先写入一份合法的 nbformat 4.5 模板（一个空 code cell），再打开
percent-style 源码投影。模板里的 kernelspec 由 `my/noema-jupyter-notebook-new-kernelspec`
决定（默认 Python 3），其中的 `language` 同时决定新 notebook 用哪个 major mode 打开。

Kernel 是全局资源，不属于 note 或单个 buffer。每个 notebook session 显式选择
“启动 kernelspec / 连接已有 kernel / No Kernel”；关闭 buffer 不会停 kernel，切换时
只有无人共享的旧 owned kernel 才会关闭。kernelspec 写在 ipynb metadata，运行中的
`kernelId` 不写入文件。

如果退出 Emacs 时远端断线，已发起但未确认的 broker kernel 关闭会保存到本机状态目录。
下次打开 `M-x jupyter-board`，切到对应 target 后可看到 **UNCONFIRMED KERNEL SHUTDOWN**；
选中该行按 `k` 或点 **Retry shutdown**。重试会先核对目标进程仍使用该 kernel 的专属
连接文件；目标仍不可达或 PID 已属于别的进程时，记录会保留，绝不凭旧 PID 强制关闭。

（旧的 Neopyter / `*.ju.py` JupyterLab 实时同步已经移除，`aaron-neopyter-*` 命令
不再存在。）

**notebook 源码投影里的键：**

| 键 | 命令 | 说明 |
|----|------|------|
| `C-c C-c` | `my/noema-jupyter-cell-run-current` | 运行光标所在 cell |
| `C-c C-r` | `my/noema-jupyter-cell-restart-run-all` | 重启 kernel 并跑全部 cell |
| `C-c C-k` | `my/noema-jupyter-cell-interrupt` | 中断 kernel |
| `C-c C-s` | `my/noema-jupyter-cell-sync-buffer` | 把整个 buffer 同步回 Noema |
| `C-c C-o` / `M-RET` / `Cmd-RET` | `my/noema-jupyter-cell-jump-output` | 跳到统一页面中当前 cell 的 output |
| `C-c C-i` / `S-TAB` | `my/noema-jupyter-cell-inspect` | 查看符号文档（前缀参数看源码） |
| `C-c i K` | `my/noema-jupyter-cell-select-kernel` | 启动 spec、连接 running kernel 或设为 No Kernel |
| `C-c C-p` | `my/noema-jupyter-output-page` | 在当前 buffer 下方打开单例 Jupyter workspace |
| `C-c i v` / `C-c i t` | Variables / Manage | 跳到统一 workspace 的变量或全局管理面板 |

buffer 顶部还有一行可点击控制：kernel/status、Run、All、Stop、Restart、Cell、
Outputs、Vars、Manage。即使 point 不在代码 cell 内，kernel 与文档级按钮仍可用。

统一 workspace 只展示 output，不重复代码：左侧管理 Server/Running Kernels/Specs，
中间按 notebook tab 展示 output cell，右侧是 Cell Inspector/Variables/Sessions，底部是
全局 Tasks。页面里的 Run 等按钮和 Emacs controls 都调用同一个 Noema controller。

**Emacs snippet 动作**：在 ipynb 源码投影里输入触发词后按 `C-c y y`。这层不是
Yasnippet 模板，而是复用 snippet 展开入口调用 Noema API；普通 snippet 仍回退到
Yasnippet。`jcode` / `jmd` 在下方新建 code / Markdown cell，ID 由 Noema 自动生成；
另有 `jabove`、`jdup`、`jsplit`、`jmerge`、`jrun`、`jrunnext`、`jall`、
`jrunabove`、`jrunbelow`、`jclear`、`jclearall`、`jout`、`jvars`、`jmanage`、
`jkernel`。`C-c y j` 可以不输入触发词直接选动作。

**补全**：`completion-at-point` 会先问 kernel（`complete_request`），
所以前面 cell 定义的变量、DataFrame 列名、IPython magic、Sage 的 builtin 都能补出来 ——
这些是 Pyright 结构上看不见的。kernel 没在跑或没有结果时自动透传给 lsp-mode，
不会因为打字就顺带启动一个 kernel。

**`input()` 可用**：cell 里调用 `input()` / `getpass()` 时，Noema 会在 cell 下方
弹出输入框；按 Esc 或 Cancel 相当于 EOF，cell 以 `EOFError` 结束，不会把 kernel 卡住。

**输出是实时的**：长任务的 stdout 边跑边显示，不用等 cell 结束。

**连远程 Jupyter server**：集群上的 lab、JupyterHub 或 kernel gateway 通过
`my/noema-jupyter-servers` 配置（config board 或 `etc/config-store.el`），
token/密码从 `auth-source` 读取，不写进仓库。配好之后 kernel 选择器里会多出
`server:<id>:<kernelspec>` 和已经在跑的 `server:<id>:kernel:<id>`。
服务器属于某个 Remote target 时，Emacs 会先开通道再把本地 URL 交给 Noema。
详见 `docs/design/jupyter-workflow.md`。
