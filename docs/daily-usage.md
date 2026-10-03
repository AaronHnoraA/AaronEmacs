# Daily Usage

这份文档只讲高频操作，不讲配置原理。

## 1. 你每天最常用的入口

- macOS GUI 下：
  `Command = Meta (M-)`，`Option = Hyper (H-)`
- `M-c` / `M-v`
  系统剪贴板复制 / 粘贴
- `H-?`
  用一次性 CLI 请求问本地 `docs/` 里的这套 Emacs 配置怎么用；结果显示在浮层。
  默认引擎 Codex；问题前加 `:c ` 改用 CC（`claude -p`），加 `:o ` 改用 OpenCode。
  例：`C-c A ?` / `H-?` → `:c 如何配置 LSP？`
- `SPC`
  Evil leader，总入口。
- `<Esc>`
  编辑 buffer 中一次完成 Evil normal-state 恢复和搜索高亮清理；在未启用 Evil 的
  buffer 中走统一取消逻辑。Minibuffer、isearch、VTerm 和浏览器仍保留各自的局部
  Escape 行为，不会把未处理的按键漏给 macOS 全屏。
- `SPC h K`
  在 Emacs 内打开本快捷键索引。
- `SPC SPC`
  `telescope` 统一搜索面板
- `M-x`
  原生 `M-x` + `amx` 历史排序
- `M-x telescope`
  `telescope` 统一搜索面板
- `C-x C-f`
  `find-file` + `vertico-directory`
- `C-x b`
  `consult-buffer`
- `C-s`
  `consult-line`
- `C-c p`
  Projectile 前缀
- `C-c p g`
  `consult-ripgrep`
- `C-x g`
  Magit
- `M-\``
  `vterm-toggle`
- `C-c e`
  切换当前 workspace 的 popup `vterm`；在 `/fs:` 远端 buffer 中直接打开同一
  target/workspace 的远端终端
- `C-c C-e`
  智能弹出或收回当前 popup `vterm`
- `C-c E`
  切换到下一个 popup `vterm`，`C-u C-c E` 新建一个
- `C-c M-E`
  新建 popup `vterm`
- `C-c M-e`
  切换当前 popup `vterm` 的固定状态
- `C-\``
  `popper-toggle`
- `C-x w d` / `C-u C-x w d`
  切换当前窗口的软 / 强 dedicated 状态；Doom modeline 显示 `d` / `D`
- `F1` / `F2` / `F3` / `F4`
  `help-command` / `telescope` / 项目工作台 / 项目 `ripgrep`
- `F5` / `F6` / `F7` / `F8`
  运行 profile / 测试菜单 / 调试菜单 / `olivetti-mode`
- `F9` / `F10` / `F12`
  `org-agenda` / popup `vterm` / Claude Code 菜单

### macOS Option `H-`

- `H-,` / `H-x`
  Hyper 管理菜单 / `telescope`
- `H-f` / `H-F` / `H-b` / `H-B`
  打开文件 / 其他窗口打开文件 / 切 buffer / 其他窗口切 buffer
- `H-r` / `H-s` / `H-g` / `H-t`
  最近文件 / 当前 buffer 搜索 / 项目 ripgrep / `telescope`
- `H-p` / `H-P` / `H-R` / `H-T`
  项目工作台 / workspace 菜单 / run profile 菜单 / test 菜单
- `H-m` / `H-a` / `H-l` / `H-y`
  `magit-status` / `org-agenda` / Claude Code 菜单 / 粘贴剪贴板图片到 Typst note
- `H-h` / `H-H` / `H-z` / `H-Z`
  help / health 菜单 / zoxide 跳目录 / 当前文件目录
- `H-e` / `H-E` / `H-d` / `H-D`
  code menu / compile menu / diagnostics menu / debug profile 菜单
- `H-i` / `H-u` / `H-j` / `H-n` / `H-N`
  `show-imenu` / language server 菜单 / 调试菜单 / 最近测试 / output 菜单
- `H-\`` / `H-q` / `H-Q` / `H-w`
  `popper-toggle` / 关闭当前 buffer / 退出 Emacs / 关当前 frame
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
`p/w/r/k/t/o`，代码和运维入口用 `./e/d/D/u/x`，其中 `x` 打开
`telescope`；其他工具用
`m/s/n/J/R/P`。

打开 `.md`、`.markdown` 或 `README.md` 时，Emacs 会直接把文件交给
Noema Web/Appine，并关闭临时 Markdown buffer。Markdown 编辑、保存、
文件树和 graph 都在 Noema 内完成；Emacs 只保留粗粒度 bridge 命令。

`H-o` 打开 Noema 全功能 hub（单页 Transient）：

| 分组 | 常用键 |
|------|--------|
| **Note (web)** | `o` 打开当前, `O` 选文件, `s` 保存, `r` 刷新, `!f` 从磁盘重载（agent 改过文件后用）, `%` AI/agent 子菜单, `f` 聚焦, `e` Esc/normal, `v` 切换源码视图, `R` Emacs 原始编辑 |
| **Find/Browse** | `j` 查找笔记, `/` 搜索（支持 `intitle:` `incategory:` `linksto:` 操作符）, `l` 最近, `.` 跟随链接, `b` 反向链接, `x` 相关, `G` 跳转定义 |
| **Insert** | `i` roam 链接, `I` TOC 链接, `t` tag id, `T` tag-id 链接, `w` 复制链接到此处, `c` note-code |
| **Knowledge** | `n` 新笔记, `d` 今日日记, `a` 按标签浏览, `C` 分类层次浏览（MediaWiki Category），`g` roam graph, `k` 任务, `A` 日程, `L` 日程日志, `F` 当前文件任务跳转, `M` 维护仪表板 |
| **Special pages (wiki)** | `!` 报告总入口, `!w` Wanted Pages, `!o` 孤立页, `!d` 死端页, `!u` 无标签页, `!h` 最多链接页 |
| **Index/Files** | `y` 同步 DB, `u` 增量更新, `Z` 全量重建, `S` DB 状态, `D` dired, `m` 移动笔记（自动重写链接）, `V` magit, `q` 停止服务 |
| **Format (web)** | `1-9/0` 粗/斜/代码/高亮/删除线/引用/列表×3/代码块, `p` 段落菜单, `z` 表格, `E` 数学块, `C` 目录, `U/Y` undo/redo |

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
命令或 `.noema` WorkNode 元数据。`M-x my/noema-roam-agenda` 或 dispatch `A`
打开原生 Agenda；`my/noema-roam-agenda-web` 保留 Web 的 Gantt 等视图。
原生 `c` 和 Web New todo 共用 Task / Deadline / Appointment 模板；`C-u c`
可改目标 Markdown 文件。保存失败保留输入，原生再按 `c` 继续修改。
`my/noema-agenda-capture-templates` 可配置字段、默认值和按日期命名的文件，
修改后重启 Noema host；配置示例见 [Agenda 文档](agenda.md#shared-capture-templates)。
项目明确进入后才扫描；切到无项目关联的 Perspective，或使用项目菜单 `l` /
`M-x my/project-leave`，释放项目监听和缓存，知识库继续常驻。
原生 `I/O` 开始/停止计时，`v k` 查看报表和待回写记录，`R/K` 处理待回写冲突。
Apple 集成先用 `my/noema-agenda-apple-enable` 启用相应类型，再用原生 `P`
或 Web 的 Promote 显式提升任务；Calendar 需要开始/结束时间（原生 `s/E`）。
`v a` / Web Global attention 查看全局关注，刷新不会激活未进入的项目；
`RET` 才进入源项目。个人 Apple 数据和跨设备同步尚未完成实机验证。
完整语法和 view-model 见
[`agenda.md`](agenda.md)。

Graph 搜索框支持全文词和
`tag:` / `alias:` / `path:` / `title:` 过滤，并会提示 tag / alias / path 等候选。
本地 graph xwidget buffer 里 `M-w` 会 kill graph buffer 并关掉 graph websocket。

### Noema 页面里的 Emacs 按键

所有 Noema 页面（md 编辑器、Jupyter output、Wiki、Agenda、Config、Slides）都把 `H-`（Option）
组合键、`C-x` / `C-c` 前缀序列、`C-g`，以及 `M-x` / `M-w` / `M-W` / `M-q` / `M-o` 交给 Emacs；
这些键在 CodeMirror 自己的按键表之前被截获，所以 `C-c C-e` 不会再同时把光标移到行尾，
`H-l` / `H-u` 也不会执行两次。`Cmd`+方向键（`M-<left>` 等）也交给 Emacs，走 windmove
直接切到相邻窗口，和普通 buffer 里一样；md 里行首/行尾请用 `0`/`$` 或 `Home`/`End`，
`Shift+Cmd+方向键` 和 `Option+方向键` 仍是页面里的选择与按词移动。

从页面触发的 Emacs 命令执行完后，键盘焦点跟着命令的结果走：它打开或切换了哪个普通
Emacs 窗口（agent 会话、compose、Treemacs、vterm、源 buffer……），就选中那个窗口，
不会回到页面编辑器。什么都没打开的命令（`M-q` 回答 n、取消的 `M-x`、只输出消息）把键盘
还给页面。命令返回后 2 秒内才由进程弹出的窗口（启动 agent、终端等）同样会接过键盘，
但前提是你还停在原页面没动。`C-u` 前缀、minibuffer 输入和 transient 菜单都会等它们结束再判断。页面发起的
`C-c A v/@` 这类发送，最后会停在 agent 会话的输入处。
Emacs 接管键盘时（页面里触发的命令打开了 vterm、agent、minibuffer，或 `Cmd`+方向键
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
`my/noema-refresh-file` 用于 agent 或外部程序改过文件之后。它不会像普通刷新那样先把
页面草稿写回磁盘；页面有未保存修改时会拒绝，`C-u` 则丢弃草稿后重载。
在 Emacs 里保存同一个 Markdown 文件（例如接受 gptel rewrite）时，没有未保存修改的
Noema 页面会自动重载。

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

`C-c r n` / Roam 菜单里的 `Create node` 打开唯一的 `*roam-new-node*` 原生新建面板；
不再并列显示含义相同的 New note / Create node 两个入口。字段与 Noema create-node
一致：Type、Title、Save path、Kind、Template、Tags；Title、Save
path、Kind 和 Tags 可在面板里直接输入，`c` 创建，`t` / `RET` 切换 roam / regular，
`T` 选模板，`R` 重置。创建实际走
Noema runtime，所以默认值、路径校验、meta、模板变量和 tabstop 展开逻辑保持一致。
Markdown 模板统一存放在 `templates/noema/`，供 Emacs 启动的 Noema 与
Roam Node 共用。Tags 在面板中显示为 `#tag`，创建前按 runtime 规则去掉显示用 `#`、
大小写去重并排序，保证面板、payload 和最终 meta 一致。所有新建 node 的 meta 都会带
一个空的嵌套 `summary` block，可直接在 Abstract
或 Properties 中编辑；模板自带 meta 时也会自动补齐，不需要每个模板重复声明。

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
- `M-x my/latex-preview-dispatch`：统一菜单，可正向同步、打开已构建的 PDF、编译并查看、或走
  AUCTeX 自带的 `TeX-view`。

远程 target 上的 buffer 目前会在执行构建命令时直接报错拒绝：AUCTeX 自身的进程启动用的是
`start-process`，遇到远程 `default-directory` 时会静默改到本机 `~` 下编译，而不是报错，所以
配置选择显式拒绝而不是让它悄悄编译错文件。texlab 提供的诊断/补全在远程 target 上仍然可用。
PDF 的阅读、批注、搜索仍由 PDF Tools 提供。

### 启动 Dashboard 的 Agenda 卡片

Agenda 卡片在启动画面最下面（Recent Files / Projects 之后、footer 之前），Roam 热力图
仍在上面。

- 点标题「Agenda · next 7 days」打开 Noema Agenda；点任务标题直接定位到该任务。
- Agenda 数据是异步取的：web-host 还在启动时先显示 `Agenda is loading…`，host 就绪
  后自动补上；期间每 `my/dashboard-agenda-retry-interval` 秒重试一次，超过
  `my/dashboard-agenda-timeout` 秒仍无应答就显示
  `Agenda unavailable · … [retry]`，点 `[retry]` 重新拉取（等价于刷新 Dashboard）。
  两个参数都在 config 注册表里（`M-x config-board`，group `appearance`）。

## 2. Leader 键分组

### 文件 `SPC f`

- `SPC f f`
  打开文件
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
  切 buffer
- `SPC b .`
  打开 bookmark 管理菜单
- `C-x r .`
  打开 bookmark 管理菜单
- `C-x r j`
  跳转 bookmark（带 preview）
- `C-x r l`
  切换当前行书签
- `C-x r n` / `C-x r p`
  下一个 / 上一个行书签
- `SPC b c`
  clone indirect buffer
- `SPC b x`
  `scratch-buffer`
- `SPC b z`
  bury buffer
- `SPC b j`
  跳转 bookmark（带 preview）
- `SPC b J`
  在其他窗口跳转 bookmark
- `SPC b m`
  设置 bookmark
- `SPC b r`
  重命名 bookmark
- `SPC b l`
  打开 bookmark 列表；`RET` 跳转，`D` 删除，当前项目条目优先
- `SPC b t`
  切换当前行书签
- `SPC b n` / `SPC b p`
  下一个 / 上一个行书签
- `SPC b L`
  直接设置当前行书签

`SPC b j` / `C-x r j` 和 `SPC SPC m` 使用同一个 bookmark picker：
候选里会显示 bookmark 名称、类型、项目、文件、行号和当前行摘要；当前项目的
bookmark 排在前面。上下移动候选时会预览目标位置，确认后跳转。没有 bookmark
时会打开 bookmark 列表，方便直接管理。

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
- merge conflict 文件内
  `o` ours，`t` theirs，`b` both，`B` base，`n` / `p` 或 `[c` / `]c` 跳冲突，`e` 进 `ediff`，`q` 打开冲突菜单
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
  `helpful-command`
- `SPC h v`
  `helpful-variable`
- `SPC h k`
  `helpful-key`
- `SPC h K`
  以只读方式打开本快捷键索引
- `SPC h w`
  打开 `*Warnings*` 日志
- `SPC h d`
  `devdocs-lookup`
- `SPC h t`
  `tldr`

### Code `SPC c`

- `SPC c ?`
  diagnostics hub
  统一入口：当前 / 项目 picker、buffer / project panel、error / warning / note 过滤都在这里
- `SPC c !`
  当前 buffer diagnostics picker
- `SPC c a`
  code actions
- `SPC c .`
  code menu；`b` build，`B` rerun build，自动识别常见的 `make` / `cmake` / `ninja`
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
  见 [latex-preview.md](latex-preview.md)
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
  当前 buffer diagnostics panel
- `SPC c P`
  当前项目 diagnostics panel
- `SPC c x`
  `quickrun`
- `SPC c y`
  `my/note-code-copy-reference` — 从代码 buffer 生成 `@@note-code(path)[tag]` 到剪贴板。
  选中区域：提示输入 tag，在区域首行前自动插入 `@aaronnote TAG` 注释标记，然后复制引用。
  未选中：查找光标上方最近的 `@aaronnote`/`@note-code` 标记，复制对应引用。
  Path 规则：`/...` 表示从当前 content root 开始；roam vault 内是 roam-root，其他项目文件是 project.el 根目录。裸相对路径保留为从当前 note 目录开始。

### Open `SPC o`

- `SPC o d`
  `dirvish-dwim`
- `SPC o D`
  `dirvish-fd`
- `SPC o q`
  `clutch-query-console`
- `SPC o e`
  `vterm-toggle`
- `SPC o E`
  切换到下一个 popup `vterm`
- `SPC o F`
  切换当前 popup `vterm` 的固定状态
- `SPC o t`
  `vterm-toggle`
- `SPC o v`
  直接打开新 `vterm`
- `M-x my/project-popup-vterm-app`
  在当前项目根目录的新 popup `vterm` 里运行 `lazygit` / `btop` / `yazi` / `tmux`

顶部 `+term` 或标签上右键可打开启动菜单，`Applications` 列出上述已配置程序。
`Agent` 子菜单（也可点击 `+Agent`）提供 Claude / Codex / OpenCode；它们使用
原生 `agent-shell` / ACP buffer，不在 vterm 里运行 CLI，共用同一个顶部弹窗、
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
  命名 `vterm`
- `SPC o S`
  `my/vterm-ssh`
- `SPC o s`
  `shell-toggle`
- `SPC o w`
  `my/open-eww-url`
- `SPC o x`
  `my/open-xwidget-url`
- `SPC o a`
  `my/appine-open-url`
- `SPC o W`
  统一搜索入口，可选搜索引擎和浏览后端
- `SPC o B`
  在 `eww` / `xwidget` / `appine` / macOS `open` 之间切换当前页面

### Browser `C-c w`

- `C-c w w`
  统一 `browse-url` 入口，默认弹选择菜单，默认项是 `xwidget`
- `C-c w e` / `C-c w x` / `C-c w a`
  直接用 `eww` / `xwidget-webkit` / `appine` 打开 URL；`eww` 和 `xwidget-webkit`
  会在独立浏览 buffer 里打开，连续多开也不会顶掉已显示的浏览 buffer
- `C-c w E` / `C-c w X` / `C-c w A` / `C-c w O`
  把当前页面快速切到 `eww` / `xwidget-webkit` / `appine` / macOS `open`
- `C-c w s`
  交互选择目标后端；可选 `xwidget` / `appine` / `eww` / `system`
- `C-c w f` / `C-c w g`
  用 `appine` 打开文件 / 打开光标下 URL
- `C-c w h` / `C-c w l` / `C-c w r`
  `appine` 后退 / 前进 / 刷新
- `C-c w [` / `C-c w ]` / `C-c w 0`
  `appine` 上一标签 / 下一标签 / 关闭当前标签
- `C-c w d`
  关闭当前浏览后端；在 `eww` / `xwidget-webkit` buffer 中也可以按 `M-w`，
  会同时 kill browser buffer 并删除对应窗口
- `C-c w ?` / `C-c w k`
  打开 Appine board / 清理全部 Appine view

当前策略是手动分流：默认打开方式统一在 `lisp/init-open.el` 的
`my/open-routes` 里维护，route DSL helper 来自 vendored `general.el` 的
`general-route-*`。`browse-url` 会先让你选后端，默认项是 `xwidget`；
`appine` 保留为原生嵌入/文件查看入口，`eww` 适合阅读，`system` 用系统
应用处理文件或链接。

### Appine `SPC a p`

- `SPC a p a`
  打开 URL 到 `appine`
- `SPC a p f`
  用 `appine` 打开文件
- `SPC a p p`
  用 `appine` 打开光标下 URL
- `SPC a p h` / `SPC a p l` / `SPC a p r`
  后退 / 前进 / 刷新
- `SPC a p [` / `SPC a p ]` / `SPC a p c`
  上一标签 / 下一标签 / 关闭当前标签
- `SPC a p k`
  `my/appine-kill-all`
- `SPC a p R`
  `my/appine-restart`
- `SPC a p s`
  切换当前页面到 `eww` / `xwidget` / `appine` / macOS `open`
- `SPC a p S`
  统一搜索入口

关闭 Appine 的最后一个标签时会自动清掉 `*Appine Window*` host buffer。
Appine board 里的文件、目录、URL 和 tab registry 都带 `[open]` / `mac open`
入口，用 macOS `open` 交给系统应用处理。

### Tab `SPC t`

- `SPC t n`
  新 tab
- `SPC t t`
  切 tab
- `SPC t r`
  重命名 tab
- `SPC t [`
  上一个 centaur tab
- `SPC t ]`
  下一个 centaur tab

### Project `SPC p`

- `SPC p .`
  打开项目工作台
- `SPC p p`
  切项目
- `SPC p o`
  打开项目工作台式入口
- `SPC p f`
  当前项目找文件
- `SPC p s`
  当前项目全文搜索
- `SPC p d`
  打开项目根目录
- `SPC p m`
  打开当前项目 Magit
- `SPC p v`
  打开当前项目 vterm
- `SPC p a`
  手动添加项目
- `SPC p D`
  批量扫描目录下的项目
- `SPC p x`
  彻底移除一个项目及其相关状态（包含 Projectile、`project.el`、Treemacs、perspective、项目 buffer/vterm）
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
  当前 buffer / 当前项目 diagnostics board，适合长期打开查看和过滤
- `SPC SPC e` / `SPC SPC w` / `SPC SPC n`
  当前 buffer errors / warnings / notes picker，移动候选时预览，确认后跳转
- `SPC SPC E` / `SPC SPC W` / `SPC SPC N`
  当前项目 errors / warnings / notes picker，移动候选时预览，确认后跳转
- `SPC SPC d`
  diagnostics hub
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

- `SPC n a`
  跳到当前函数开头
- `SPC n e`
  跳到当前函数结尾
- `SPC n [`
  上一个函数
- `SPC n ]`
  下一个函数
- `SPC n u`
  跳到外层结构
- `SPC n l` / `C-c C-j`
  打开光标处或选区中的 `file:line:column`；前缀参数在其他窗口打开
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
- 在 Dired 里：
  - `H`
    显示/隐藏 dotfiles
  - `C-c C-e`
    进入 `wdired`

## 6. 窗口和弹出层

- `C-x 1`
  `my/toggle-delete-other-windows` — 最大化当前窗口，再次执行恢复先前布局（依赖 winner-mode）
- `M-o`
  `ace-window`；Noema xwidget 里的目标编号会显示在页面左上角
- `M-O`
  交换当前窗口与目标窗口
- `M-\``
  `vterm-toggle`
- `H-\`` / `C-\``
  `popper-toggle`
- `C-M-\``
  改变 popup 类型

### 后台任务指示器

Mode line 上常驻一只皮卡丘(`assets/activity/` 里的 24×16 XPM 像素图)。它**一直在**,
没事时站着,有后台任务时开始跑,多于一个任务时在后面跟上数字。
**鼠标左键点它打开 `M-x my/activity-board`。** 终端(无图形界面)里自动降级成颜文字
`ᕕ( ᴗ )ᕗ` / `ᕕ( ᐛ )ᗴ`,不会变成空白。

机理只有一条链路,在 [lisp/init-activity.el](../lisp/init-activity.el):

1. **枚举的根是 `(process-list)`** —— Emacs 自己的异步子进程注册表,也就是
   `M-x list-processes` 看到的那张表。以这个为根是为了**不用维护**:以后接任何新的
   语言服务器、agent 工具或后台守护进程,它都会自动出现在板面上,这个模块不需要知道
   它的存在。
2. **但它太广,不能直接数。** 里面同时装着三种东西,只有第一种是会结束的活动:

   | 分类 | 内容 |
   | --- | --- |
   | `task` | 有限的工作:compilation、native-comp worker、remote task |
   | `service` | 常驻守护进程:语言服务器、epdfinfo、agent 会话 |
   | `connection` | network / serial 端点,TRAMP 在内 |

   全数一遍会永远停在几十,没有信息量。所以**板面按三组展示,指示器只数 `task`**。
3. **谁是 task 不靠猜。** 不看进程名也不看命令行,而是问**已经拥有这些工作的注册表**:
   `compilation-in-progress`、`remote-tasks`、`comp-async-compilations`。某个进程是
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
[lisp/roam/init-activity-noema.el](../lisp/roam/init-activity-noema.el):

- LaTeX 导出读取 Noema Core task pool,从排队、转换、agent 润色到编译 PDF 都属于
  一个任务。完成、失败或取消后移出计数。按 `x` 走 Core 的取消接口,`RET` 打开源笔记。
- agent 工作读取 `noema-agent-acp-sessions` 的 busy 状态,空闲会话仍是 service。
  `RET` 打开会话,`x` 通过 ACP owner 取消 Run 或中断当前 turn。导出内部的
  `latex-export` 会话由导出任务覆盖,不会重复计数。
- Core task 快照每 2 秒异步读取一次,不会跟着 8 FPS 发请求,也不会为了监控启动
  Noema。host 停止时清空快照,重启或关闭指示器前的旧回复不会恢复过期任务。

`*Activity*` 板面是**集中入口,不是替代品**。Emacs 自己给每种后台工作都带了管理器,
每个都比重写一遍更称职,所以板面负责「什么在跑 + 能对它做什么」,更深的细节一键交给原生工具。

**移动和其它 board 完全一致**:`j`/`n` 下移,`k`/`p` 上移,`TAB` 下一个按钮,
`RET` 打开当前行。停止类操作**刻意避开导航键**,放在 `x`/`i`/`X`。
按到没绑定的字母在 echo 区列出可用的键。行内的鼠标/RET 映射保留页面的键盘映射,
所以光标移进进程行、agent 行或详情行后,导航和动作键仍然有效。

按行操作:

| 键 | 作用 |
| --- | --- |
| `RET` | 打开当前行(进程行 = 跳到它的 buffer;按钮行 = 按下按钮) |
| `v` | 同上,显式跳 buffer |
| `d` | 详情:Emacs 侧字段(status/pid/type/buffer/tty/query/command)+ **OS 侧 `process-attributes`**(rss、etime、nice、majflt…) |
| `w` | 复制命令行到 kill ring |
| `x` | 停止。有 owner 的走 owner 的取消路径(如 `remote-task-cancel`);**没有任何注册表认领的进程会先问一遍再杀** |
| `i` | SIGINT,礼貌停止,让编译器/shell 有机会收尾 |
| `X` | SIGKILL,先问一遍 |

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
- 单次渲染实测 **0.37ms**,只走 `process-list` 和 `timer-list`,不碰网络和磁盘。
- **自动刷新默认关**。打开后是一个受 `my/activity-board-refresh-interval` 约束的
  timer,且只在板面**可见**时重绘;buffer 被 kill 时经 `kill-buffer-hook` 取消。

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
- `![说明](diagram.drawio)` 像引用图片一样引用 draw.io 文件：Noema 调本机 draw.io 导出 SVG 并缓存，按原图大小显示，不加载在线编辑器、不联网，导出时按当前主题要 light 或 dark 版本。要改图就用 draw.io 打开那个 `.drawio` 文件，存盘后 Noema 自动重新导出。`#page=2` 指定第几页（从 1 数）；本机没装 draw.io 时显示一张写明原因的占位图（可用 `NOEMA_DRAWIO_BIN` 指定路径）。`.drawio.svg` / `.drawio.png` 本来就是图片，直接当图片渲染。
- draw.io 导出一次约 1 秒（要起一个 draw.io 进程），之后按「文件路径 + 修改时间 + 大小 + 页码 + 主题」命中磁盘缓存，命中是毫秒级，重复渲染只走 ETag 304。同时导出的并发上限默认 2（`NOEMA_DRAWIO_CONCURRENCY`），一篇笔记里十几张图不会同时拉起十几个 draw.io。
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
选区的行号取自你正在编辑的 buffer，所以 worktree 里的文件改动较大时，行号可能对不上。
Sessions 列表（`C-c A S`）和总览（`C-c A G`）里，`m` 打开该会话所在 checkout 的
Magit status；`d` 显示它从分出时起的全部改动（已提交和未提交的一起），
普通会话则只显示未提交改动。`M-x noema-agent-worktree-remove` 只列出 Noema 建的 worktree：
还有会话在里面工作的不删；有未提交改动的要加前缀参数才删；分支始终保留，
已提交的工作不会丢。

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
于 Emacs 侧，列表里显示为 `local`。不想自动收编裸 `M-x agent-shell` 的话，把
`noema-agent-acp-adopt-foreign-sessions` 设为 nil。

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
剪贴板，可直接粘贴到 vterm 里的 agent；即使没有打开 ACP 会话也可以选。
这项操作不会切换会话或清空已选的 context。
想恢复“静默复用上次会话、`C-u` 才重选”，把 `noema-context-always-ask-session`
设为 nil。会话还在初始化就先排队；正在回答时走 agent-shell 自己的
pending 队列。

在 agent-shell 输入 `/resume` 并回车，会读取当前 agent、当前工作目录的原生
ACP 会话列表，排除当前会话并按最近使用排序，然后选择要恢复的会话。已有活动 buffer 会直接
切换到它；未打开的历史会话会替换当前 Agent 窗口中的 buffer，不增加新 tab。
Noema 已登记的原生 session ID 会沿用原会话名。Codex 与 Claude 的会话
历史由各自的官方后端保存，Noema 只登记名字与原生 ID，不复制 transcript。

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
详见 `docs/jupyter-workflow.org`。
