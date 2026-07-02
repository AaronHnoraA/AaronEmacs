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
  切换当前 popup `vterm`
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
  Aaronnote 全功能 hub (见下) / 上方开新行 / 向下复制当前行或区域 / 向上复制当前行或区域
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
Aaronnote Web/Appine，并关闭临时 Markdown buffer。Markdown 编辑、保存、
文件树和 graph 都在 Aaronnote 内完成；Emacs 只保留粗粒度 bridge 命令。

`H-o` 打开 Aaronnote 全功能 hub（单页 Transient）：

| 分组 | 常用键 |
|------|--------|
| **Note (web)** | `o` 打开当前, `O` 选文件, `s` 保存, `r` 刷新, `f` 聚焦, `e` Esc/normal, `v` 切换源码视图, `R` Emacs 原始编辑 |
| **Find/Browse** | `j` 查找笔记, `/` 搜索（支持 `intitle:` `incategory:` `linksto:` 操作符）, `l` 最近, `.` 跟随链接, `b` 反向链接, `x` 相关, `G` 跳转定义 |
| **Insert** | `i` roam 链接, `I` TOC 链接, `t` tag id, `T` tag-id 链接, `w` 复制链接到此处, `c` note-code |
| **Knowledge** | `n` 新笔记, `d` 今日日记, `a` 按标签浏览, `C` 分类层次浏览（MediaWiki Category），`g` roam graph, `k` 任务, `A` 日程, `M` 维护仪表板 |
| **Special pages (wiki)** | `!` 报告总入口, `!w` Wanted Pages, `!o` 孤立页, `!d` 死端页, `!u` 无标签页, `!h` 最多链接页 |
| **Index/Files** | `y` 同步 DB, `u` 增量更新, `F` 全量重建, `S` DB 状态, `D` dired, `m` 移动笔记（自动重写链接）, `V` magit, `q` 停止服务 |
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

Note task chip 支持 `priority`、`due`、`scheduled`、`repeat`，例如
`#todo(priority: "A", due: "2026-05-20")[Write proof]`。`*Note Agenda*`
里 `c` 循环状态，`t` 选择状态，`s` 设置 scheduled，`D` 设置 due，`p`
设置 priority，`r` 设置 repeat，`.` 设 scheduled=today，`!` 设 due=today，`<` / `>`
前后移动日期，`0` 清掉日期 metadata，`?` 打开同一套 task 菜单。Agenda / board buffer
排除 Evil，但保留 `h/j/k/l` 轻导航；默认扫描只进 `todo / doing / waiting`，`done / cancelled`
只在 `C-u`、all 视图或 `C` closed board 里进入扫描范围；closed board 只扫 `done / cancelled`。
日期输入走 Org 的 date prompt，可用弹出的 calendar 和 `+2w`、`Fri` 这类 Org 解析。Graph 搜索框支持全文词和
`tag:` / `alias:` / `path:` / `title:` 过滤，并会提示 tag / alias / path 等候选。
本地 graph xwidget buffer 里 `M-w` 会 kill graph buffer 并关掉 graph websocket。

Aaronnote 的 Emacs 原生 roam buffer（Agenda、Tasks、TOC、Backlinks、Related、
Management、DB Status、note list 和 Roam Selector）使用统一的紧凑 workbench UI：
header-line 显示当前视图状态，正文使用工具栏、分组、状态徽章和可点击行。通用按键为
`g` 刷新、`q` 关闭、`RET` 打开当前行、`j` / `k` 或 `n` / `p` 上下移动，
`TAB` / `S-TAB` 在工具栏按钮间移动。Roam Selector 另外保留 `/` / `s` 搜索、
`g` 回根目录、`.` 回当前 note context、`u` / `^` 上一级、`r` 刷新和 `i`
直接插入当前目标。这些是 Emacs buffer 的界面和按键，不影响 Aaronnote Web UI。

`C-c r n` / Roam 菜单里的 `new note` 打开 `*roam-new*` 原生新建面板。字段与
Aaronnote New Note 一致：Type、Title、Save path、Kind、Template、Tags；Title、Save
path、Kind 和 Tags 可在面板里直接输入，`c` 创建，`t` / `RET` 切换 roam / regular，
`T` 选模板，`R` 重置。创建实际走
Aaronnote runtime，所以默认值、路径校验、meta、模板变量和 tabstop 展开逻辑保持一致。
Markdown 模板统一存放在 `templates/aaronnote/markdown-mode/`，供 Emacs 启动的 Aaronnote 与
Roam New 共用。

### Aaronnote LaTeX 导出

在 Aaronnote 的 Tools 中选择 `Export LaTeX`，或在页面内按 `⌘P`。导出先打开专用范围
选择器，不会再用一个模糊的空白 TOC 输入框：

- `Whole note` 导出全文；有文本选区时会额外提供 `Text selection`。
- 每个 heading 都按真实层级缩进显示；选择 heading 会连同它的所有子章节一起导出。
- `cursor` 标记光标当前所在的最深章节；章节多时可以搜索过滤。
- `↑` / `↓` 选择，`Enter` 确认，`Esc` 取消；双击章节可直接进入保存路径选择。

选完范围后会再让你**选择模板**（`Article` 默认、`Report`、`Assignment`），模板若声明了
额外字段（如 Assignment 的课程代码 / 学期 / 学号）会弹出表单，默认值按 note 记忆。

导出分两步：先用确定性的**机械转换**生成草稿，再由 **AI 后端**在草稿上做格式润色，并由服务端
把成稿套进模板后**实际编译**校验；编译失败会带着日志让 AI 重试，若始终失败则回退到机械草稿。
AI 不可用时静默回退到机械草稿，CMD+P 始终可用。AI 只改格式、不改正文（附带一个词表保真提醒），
并会顺带给文档取一个合适的**标题**（无显式 meta 标题时用 AI 标题，其次首个 `#` 标题，最后才是
文件名）。导出每一步（转换 / 润色 / 编译 / 重试）都会在状态栏 **echo 进度**。

后端可用 `my/aaronnote-latex-export-agent` 选择：`codex`（默认）/ `claude` / `opencode`，都以
非交互、免确认方式运行，且在配置里选定、不会每次询问。引擎开关 `my/aaronnote-latex-export-engine`
（`codex` / `mechanical`）。编译校验用 draft 模式（`-draftmode` / `-no-pdf`）加速。见 Aaronnote 的
`docs/latex-export-style.md`。

标题、章节名和 theorem/proof 标签中的 `\(...\)` 会保留为 LaTeX 数学，而不是被转义成
`\textbackslash`。输出路径按 note 记忆，写入是原子的，并强制使用 `.tex` 后缀。未闭合的
display math、代码 fence 或 `#+begin` block 会在写文件前报出明确错误，避免留下半成品。

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

### Git `SPC g`

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
  语言服务器菜单
  可以进 Hub / Doctor / 调参 / log / session / config
- `SPC c i`
  `show-imenu`
  左侧 smart-toggle `treemacs`，并跟随当前文件和光标所在 symbol
- `SPC c I`
  `eldoc-box-help-at-point`
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
  `ace-window`
- `M-\``
  `vterm-toggle`
- `H-\`` / `C-\``
  `popper-toggle`
- `C-M-\``
  改变 popup 类型

## 7. 有冲突时优先记住什么

- `M-w` 关闭当前 buffer，行为与 `C-x k` 一致
- 关 frame 用 `H-w`
- 普通 warning 现在只写入 `*Warnings*`，不再自动弹窗抢操作；需要时用 `SPC h w`
- `C-c y` 现在是 snippet 前缀，不再直接展开
- `C-c n` 是 Typst note 前缀，不再给 centaur-tabs

## 8. AI Workbench

统一入口，把 CLI 引擎（CC/Codex/OpenCode）和 HTTP 模型（ChatGPT、Claude-API 等）放到同一个 picker 里。`gptel` 是内部 HTTP 集成层，不在 picker 里显示。

| 键 | 功能 |
|----|------|
| `C-c A W` | 打开/选择引擎（首次弹 picker：Codex/CC/OpenCode/ChatGPT/Claude…） |
| `C-c A .` | 带上下文发送 prompt 到当前引擎 |
| `C-c A w` | writing prompt |
| `C-c A k` | 关闭当前引擎 session 并重置选择 |
| `C-c A H` | 打开管理 Hub（CLI Engines + Chat Models + Profiles） |
| `C-c A ?` | docs-ask（默认 Codex；`:c ` 前缀用 CC；`:o ` 前缀用 OpenCode） |
| `C-c A i r/b/f` | 发送选区 / 当前 buffer / 文件给当前引擎 |
| `C-c g` | 直接打开 HTTP chat buffer（gptel）|
| `C-c G` | 从 JSON 重新加载 HTTP chat 后端 |

HTTP 后端在 `etc/ai-workbench/backends.json` 里配置（OpenAI、Anthropic、Ollama 等），CLI 引擎（Codex、OpenCode）通过 `ai-workbench-adapter-*.el` 的 `defcustom` 配置可执行路径。

## 9. Neopyter — JupyterLab 实时同步

对 `*.ju.py` / `*.ju.r` 文件，`aaron-neopyter-mode` 会自动激活（取代原来的 `jupytext-mode`，
后者作为 sub-mode 保留，负责保存到磁盘）。打开文件后 Emacs 会自动启动 WebSocket 服务并尝试连接
JupyterLab 扩展。

**前置条件：**
```bash
pip install neopyter
# JupyterLab → Settings → Neopyter → Mode: direct, IP: 127.0.0.1, Port: 9001
```

**常用操作：**

| 键 | 命令 | 说明 |
|----|------|------|
| `C-c C-o` | `aaron-neopyter-open-notebook` | 在 JupyterLab 里打开/创建配对的 `.ipynb` |
| `C-c C-s` | `aaron-neopyter-sync-current` | 立即强制同步当前 buffer |
| `C-c C-c` | `aaron-neopyter-run-cell` | 运行当前 cell |
| `C-c C-a` | `aaron-neopyter-run-all-above` | 运行光标以上所有 cell |
| `C-c C-b` | `aaron-neopyter-run-all-below` | 运行光标以下所有 cell（含当前） |
| `C-c C-r` | `aaron-neopyter-restart-kernel` | 重启 kernel |
| `C-c C-l` | `aaron-neopyter-toggle-follow-point` | 开关 JupyterLab 随光标滚动 |
| `M-x aaron-neopyter-connect` | — | 手动启动 WS 服务（通常自动） |
| `M-x aaron-neopyter-health-check` | — | 检查连接状态 + 版本 |
| `M-x aaron-neopyter-detect-jupyter-root` | — | 从当前打开的 notebook 自动推断 Lab 根目录 |

**文件映射：** `foo.ju.py` → `foo.ipynb`（去掉 `.ju.` infix）；路径以 `aaron-neopyter-jupyter-root`
为基准转换成 JupyterLab 能识别的相对路径。

**调试：** `M-x aaron-neopyter-show-log` 查看 RPC 日志；`(setq aaron-neopyter-debug t)` 启用详细输出。

协议细节见 `docs/neopyter-protocol-notes.md`；配置选项见 `docs/settings-cookbook.md` 第 16 节。
