# Typst Note Guide

这套 note 系统现在以 Typst 为中心。旧的 Org 文档、导出类和 `H-o` Org 入口不再作为主线维护；笔记、assignment 和项目外写作都走 `.typ`。

## 1. 模块边界

- [lisp/note/init-note.el](../lisp/note/init-note.el)
  负责 note 根目录、索引、跳转、反链、doctor、Tinymist 预览同步，以及写入 note helper。
- [lisp/note/init-note-metadata.el](../lisp/note/init-note-metadata.el)
  编辑 metadata 块：加 tag、加 alias。tag 支持已有 tag completion，也支持 `#math algebra`、`:math:algebra:`、逗号分隔这类轻量输入。
- [lisp/note/init-note-capture.el](../lisp/note/init-note-capture.el)
  分类 daily capture (`my/note-capture`) 和当天滚动 daily note (`my/note-daily-today`)。
- [lisp/note/init-note-reference.el](../lisp/note/init-note-reference.el)
  Typst 侧 reference 工具：创建 `<label>`、插入 `@label` / `@citekey`、从当前 buffer / indexed note / 任意文件选择 target，并提供 `my/reference-insert-dispatch` 按 mode 路由到 Typst / Org reference。
- [lisp/note/init-note-agenda.el](../lisp/note/init-note-agenda.el)
  跨所有 note 收集 `#todo / #doing / #waiting / #done / #cancelled` chip，渲染 list agenda 或 board 到 `*Note Agenda*`。
- [lisp/note/init-note-graph.el](../lisp/note/init-note-graph.el)
  本地 graph 视图和前端搜索数据。
- [lisp/note/init-note-tools.el](../lisp/note/init-note-tools.el)
  Zotero metadata 填充、剪贴板图片粘贴。
- [lisp/note/publish.el](../lisp/note/publish.el)
  旧 Org 网站发布配置；现在跟 note 发布链放在一起，发布根目录显式指向 `~/HC/Org/`。
- [lisp/note/typst/note.typ](../lisp/note/typst/note.typ)
  note 内部共享 Typst 样式和 helper。
- `~/HC/Org/_typst/publish.typ`
  发布用 PDF 样式。它只影响 `make publish` 产物，不改变日常编辑 preview。
- `~/HC/Org/bin/publish-site`
  扫描 Typst note metadata，增量编译公开 PDF，并刷新网站 archive / graph 数据。
- [notes/](../notes/)
  项目外写作样式，当前包含 `assignment.typ`、`rho.typ`、`aleph-notas.typ`。
- [templates/typst/](../templates/typst/)
  Emacs 插入模板，当前主用 `assignment.typ`。

默认 note 根目录仍指向 `~/HC/Org/`，默认新建目录仍是 `~/HC/Org/roam/`。这是现有文件布局兼容，不代表继续用 Org 格式。

## 2. Note 文件

每个 note 顶部用 Typst metadata：

```typst
#import "/_typst/note.typ": *
#show: note-entry

#metadata((
  kind: "note",
  id: "20260511T120000-example",
  title: "Example",
  date: "2026-05-11",
  tags: ("math", "draft"),
  aliases: (),
)) <note>
```

跨 note 链接：

```typst
#note("20260511T120000-example")[Example]
```

普通 Typst link：

```typst
#link("relative/path.typ")[Target]
#link("https://example.com")[Website]
```

Emacs 里优先把 Typst note 文件识别为 note id：通过 `my/note-reference-insert` 选到 indexed note 或带 note metadata 的 `.typ` 文件时，默认生成 `#note("id")[title]`，而不是路径型 `#link(...)`。普通 `#link(...)` 仍可用于 URL、非 note 文件或确实需要路径语义的目标；如果历史 `#link("some-note.typ")[...]` 指向的是 note 文件，索引反链时也会归一到目标 note id。

Zotero 链接：

```typst
#zoterolink("zotero://select/items/1_BJ9K8NAX")[Paper title]
```

在 Emacs note buffer 里，`RET` / evil normal `RET` / `C-c n RET` 会统一走 note reference resolver：

- `#note("id")[label]`：打开目标 note。
- `#link("path")[label]`：URL 用浏览器打开；文件路径用 `find-file`；如果目标文件是 note，则优先进入 note。
- `#zoterolink("zotero://...")[label]`：调用系统 URL handler 打开 Zotero。
- `@label`：跳到当前 buffer 内的 `<label>`。

Tinymist preview 里点击蓝色下划线回源时也复用这套 resolver；如果点到 note/link/reference，就直接进入目标，否则才回到普通 preview→source 同步。

本地 Typst 引用仍用原生 label：

```typst
= Statement <sec-statement>
$ x = y $ <eq-main>
See @sec-statement and @eq-main.
```

`C-c n p` / `H-o p` 会在当前行创建或复用 `<label>`，并复制 `@label`。heading 生成 `sec-...`，figure/image 生成 `fig-...`，公式行生成 `eq-...`，普通文本生成 `ref-...`。

保存 note 时默认会单文件更新索引；可用 `my/note-auto-sync-on-save` 关闭。`M-x my/note-db-sync` 会增量同步 `var/note/note.db`，并在 note 根目录写入：

- `_typst/note.typ`
  从 [lisp/note/typst/note.typ](../lisp/note/typst/note.typ) 复制出来的 helper。
- `_typst/notes/<id>.typ`
  每个 note 的小 wrapper，用于跨文件 import/include。

note helper 提供 `note-entry`、`note-theme`、`note`、`note-include`、`note-transclude`、`note-import-path`，以及 `definition`、`theorem`、`proof`、`example`、`remark`、`summary`、`question`、`important`、`warning`、`tip`、`info` 等卡片块。

批注 / 脚注 / 侧注 helper 也在 `/_typst/note.typ` 里。常用写法：

```typst
#comment[
  这里放普通批注。适合记录“为什么这里这样写”。
]

#annotation[
  这里放更正式的注解，颜色比 comment 稍暖。
]

#marginal[
  旁注块。单独一行使用；PDF 里会浮放到正文左侧，不占正文流。
]

#sidenote[
  侧边评论。单独一行使用；PDF 里会浮放到正文右侧，不占正文流。
]

正文里可以直接接脚注#fn[这里是脚注内容]，也可以写成#foot[同义别名]。

需要文本顶标或底标时，用不会遮蔽 Typst 内置对齐名的别名：
H#topmark[2]O 或 X#bottommark[i]。
```

`#comment`、`#annotation` 更像完整块；`#marginal` / `#margin-note` / `#mnote` 是左侧零高度浮放块，`#sidenote` / `#sidecomment` 是右侧零高度浮放块，最好独立成行放在要标注的段落之后；`#fn`、`#foot`、`#topmark`、`#bottommark` 是行内用法。不要定义 `#top` / `#bottom` 这类别名，因为 Typst 自己有 `top`、`bottom` 对齐值，遮蔽后会破坏 `place(top + left)` 这种页面背景代码。

Bib helper：`#bib()` 默认读取 `/references/references.bib`，等价别名是 `#references()`；可用 `#bib(path: "/path/to/file.bib", style: "apa")` 指定 bibliography 文件和样式。

`M-x my/note-doctor` 检查悬空 `#note`、重复 id、缺 date、孤立 note 和 alias 冲突；报告里的路径可点击跳转。

行内任务 chip：`#todo[...]`、`#doing[...]`、`#waiting[...]`、`#done[...]`、`#cancelled[...]`。常配合列表使用，例如 `- #todo[Write proof of Lemma 3]`。这些 chip 是 `my/note-agenda` 的扫描入口，调用形式 `#<state>(metadata...)[body]` 不要改；没有 metadata 的旧写法仍然有效。支持的 metadata 是 `priority`、`due`、`scheduled`、`repeat`：

```typst
- #todo(priority: "A", due: "2026-05-20")[Write proof of Lemma 3]
- #doing(scheduled: "2026-05-12")[Polish lemma 2 statement]
- #waiting(repeat: "1w")[Send weekly reading update]
```

`done` 和 `cancelled` 会自动给 body 加灰色 + 删除线。

日常 note 始终导入 `/_typst/note.typ`。这个文件由 Emacs helper 同步，负责写作时的样式、Tinymist preview 和跨 note helper。发布时不要在每篇 note 里改 import；发布脚本会临时把第一处 `"/_typst/note.typ"` 指到 `"/_typst/publish.typ"`。

## 3. 多文件组合

用 wrapper import 另一个 note 暴露的内容：

```typst
#let linked = note-import-path("note-linked")
#import linked: cross-file-sum-equation
```

整篇 include：

```typst
#note-include("note-linked")
```

`note-include` / `note-transclude` 会把被包含文件标成非入口文档，避免子文件自己的目录重复出现在当前 PDF。公式、定理、章节引用仍然使用 Typst 原生 `<label>` / `@label`。

`my/note-reference-insert`（`C-c n r` / `H-o r` / Typst buffer 中 `H-p`）提供 Org-style reference picker：

- 默认先选 source：Current buffer、Note node、File path。
- 然后选 target：file-level target、heading scope、具体 `<label>`，支持 Consult preview。
- `TAB` / `M-RET` 进入 heading scope；`RET` 确认当前 target。
- 无 label 且没有子 target 的 heading 不显示，避免空 scope 反复补全。
- 同文件 concrete label 插入 `@label`。
- 跨 note 文件优先插入 `#note("id")[description]`。
- 普通非 note 文件退回 `#link("relative/path")[description]`。
- prefix 调用 `C-u C-c n r` 只从当前 buffer 的本地 labels / citations 里选。

`my/note-citation-insert`（`C-c n R` / `H-o R`）从 `my/note-bibliography-files`、`pv/org-bibtex-files` 或 `/references/references.bib` 读取 BibTeX，插入 Typst `@citekey`。

## 4. Capture / Daily / Agenda

这部分接住了过去 org-capture + org-agenda 的工作流。所有产物都是 Typst note，直接进 `note.db` 索引。

**New note**：`my/note-new`（`H-o n` / `C-c n n`）保留旧入口，创建默认 roam note。创建时会先选目标文件夹；默认跟随当前 note 所在目录，否则落到 `my/note-directory`。默认不强制输入 tag；用 prefix 调用或打开 `my/note-new-read-tags` 后才提示 tag。tag 输入支持 `math algebra`、`math,algebra`、`:math:algebra:`、`#math #algebra`。`my/note-new-of-type`（`H-o N` / `C-c n N`）从 `my/note-types` 选择模板；默认提供 `default`、`literature`、`fleeting`。类型可以定义文件名前缀、默认 tags、id 策略和模板函数。

**Capture**：`my/note-capture`（`H-o c` / `C-c n c`）从 `my/note-capture-categories` 默认列表 `idea / inbox / mail / note / meeting / protocol / uni / life` 里选分类，输入 title，写入 `<note-directory>/daily/<category>/<slug>-YYYYMMDD.typ`。文件自带完整 metadata + 一级标题，落地后立即触发单文件索引更新。新增分类只需修改 `my/note-capture-categories`。

**Daily**：`my/note-daily-today`（`H-o d` / `C-c n d`）打开当天的滚动文件 `<note-directory>/daily/YYYY-MM-DD.typ`（所有时段写在同一篇），并在末尾追加 `== HH:MM` 小节作为新时段入口。文件不存在时自动写 metadata 并 sync。文件名格式由 `my/note-daily-rolling-file-format` 控制。

**Agenda**：`my/note-agenda`（`H-o A` / `C-c n A`）regex 扫所有 note 找任务 chip 调用，按 state 分组渲染到 `*Note Agenda*` buffer，每条以 `file:line` 按钮起头，点击/回车跳转。默认只展示 `my/note-agenda-active-states`（`todo` / `doing` / `waiting`），并且底层 regexp 也只匹配这些 active state；前缀 `C-u` 等价于 `my/note-task-states` 全集（加上 `done` / `cancelled`），closed task 只在显式 all 视图里进入扫描范围。扫描会在进入目录前排除 `_typst`、`public`、`var`、`.git` 等 `my/note-excluded-directories`，也会忽略行注释里的 `// #todo[...]`。

**Agenda board**：`my/note-agenda-board`（`H-o b` / `C-c n b`）用 Org agenda board 的方式按 state 分列展示，默认列是 `todo / doing / waiting`，由 `my/note-agenda-board-states` 控制。Board 顶部有 dated task calendar preview；agenda / board buffer 从 Evil 排除，但保留 `h/j/k/l` 轻导航。Agenda buffer 内 `g` 刷新，`b` / `B` 切 board（active / all），`C` 切 closed board（只扫 `done / cancelled`，不走 all），`v` / `V` 切 list（active / all），`?` 打开 task dispatch。

Agenda / board 里可以直接改任务：

- `RET` / `o` 打开源位置；board 里 `h` / `l` 横向切列，`j` / `k` 上下移动。
- `c` 循环状态；`t` 从 `todo / doing / waiting / done / cancelled` 里选状态。
- `s` 设置 `scheduled`；`D` 设置 `due`；`p` 设置 priority；`r` 设置 repeat。
- `.` 设置 scheduled 为 today；`!` 设置 due 为 today；`<` / `>` 把已有日期前后移动一天。
- `0` 清掉 `due / scheduled / repeat`。
- `H-o T` / `C-c n T` 打开 task dispatch；在 Typst note buffer 里可以插入或包裹 region 成 `#todo[]` / `#doing[]` / `#waiting[]`。

`s` / `D` 读取日期时直接用 Org 的 date prompt：会弹 calendar，支持鼠标选日期、`+2w`、`Fri`、`sep 12` 等 Org 日期输入；清空日期仍用 `0` 或 task dispatch 的 clear dates。

任务 chip 是行内调用，常配列表：

```typst
- #todo(priority: "A", due: "2026-05-20")[Write proof of Lemma 3]
- #doing(scheduled: "2026-05-12")[Polish lemma 2 statement]
- #waiting[Reviewer feedback on §3]
- #done[Submit COMP3927 assignment]
- #cancelled[Old framing of Theorem 1]
```

视觉上是圆角胶囊 + metadata 小胶囊 + 跟在后面的文本；`done` / `cancelled` 自动给 body 上灰色 + 删除线。scanner 锚定 `#<state>(metadata...)[body]` 这个调用形式，不要把 chip 重命名或改成块。要加新 state，同时改 `lisp/note/typst/note.typ` 的 `task-tag` 调用和 Emacs 端 `my/note-task-states`。

**Graph search**：`my/note-graph`（`H-o g` / `C-c n g`）生成本地 graph 数据，前端搜索支持普通全文词，也支持轻量字段过滤：

```text
hilbert tag:math alias:operator path:QC title:state
```

输入 `tag:`、`alias:`、`path:`、`title:`、`group:`、`section:` 或 `#` 时，前端会用当前数据里的 tag / alias / path / title 候选做补全提示，支持 `alias:"multi word"` 这种带空格值。发布站点共用同一套 JS，因此 publish 后也有同样搜索体验。本地 graph 的 `data.js` 会包含 title、summary、正文 search text、tags、aliases、path、refs/backlinks；public `data.js` 只给公开 note 写入正文搜索文本，private note 仍只暴露 sealed summary。本地 graph xwidget buffer 里 `M-w` 会 kill graph buffer 并关闭 graph websocket server。

## 5. 发布 Workflow

Org repo 的发布入口是：

```sh
cd ~/HC/Org
make publish
```

当前发布模型是“网站索引 + 浏览器直接打开 PDF”：

- `roam/**/*.typ` 是唯一 note 源文件。
- `bin/publish-site` 读取 `#metadata((kind: "note", ...)) <note>` 和 `#note("id")[Title]`。
- 每篇公开 note 编译成同路径 `public/roam/**/*.pdf`。
- private note 编译成同路径 sealed PDF，占位页来自 `~/HC/Org/_typst/private.typ`。
- `public/js/data.js` 只保存 archive / graph 需要的标题、标签、摘要、引用和 PDF 链接。
- PDF 内的 `#note("id")[Title]` 会在发布临时源里改写为可点击的 web/PDF 链接；默认用 `/roam/...pdf`，需要绝对域名时设置 `PUBLISH_BASE_URL`。
- 不再生成每篇 note 的 HTML wrapper；浏览器自己的 PDF viewer 负责阅读界面。

因此，`note.css` / `publish.css` 只影响网站壳、archive 和 graph，不影响 PDF 里的 note 视觉。公开 PDF 缺的页面、标题、目录、卡片、页眉页脚和页面背景等效果，应该补到 `~/HC/Org/_typst/publish.typ`，不要补一个发布专用 CSS，也不要把日常 `/_typst/note.typ` 改成只适合发布。`publish.typ` 通过 `note-theme.with(...)` 基于日常 note 样式升级；可以保留公开版的旧纸/folio 主色，但新增背景、边框和目录包装应继续放在 publish 层。

private 配置优先跟随目录。`bin/publish-site` 里的 `PRIVATE_PATH_PREFIXES` 默认把 `roam/daily/` 和 `roam/project/` 整个屏蔽；这些目录下的 note 不需要逐篇加 metadata。单篇 note 仍可用 metadata 额外 opt-in。任一条件命中时，note 会从公开列表和搜索里隐藏，PDF 正文会替换成 sealed 页面，但 graph 数据仍保留标题和 note links，避免关系图断边。发布脚本不会分发正文、摘要、tags 或 private note assets：

- `private: true`
- `hidden: true`
- `publish: false`
- `visibility: "private"` / `"hidden"` / `"sealed"`
- tags 里有 `"private"`、`"hidden"`、`"sealed"`、`"no-export"`、`"noexport"`

发布是增量的。依赖快照写在 `public/.deps/`；只有 note 本身、图片、wrapper、`_typst/publish.typ` 等输入变了，才会重新编译对应 PDF。另有忽略提交的 `public/.publish-state.json` 记录上次成功发布的 git `HEAD`；当相关发布输入干净且 `HEAD` 没变时，`make publish` 会整轮快速跳过。`make force` 会绕过这些缓存。

`agent/index/` 还是旧的派生索引层，先保留到后续 Typst index refresh；精确内容仍然回到 `roam/**/*.typ` 核对。

## 6. Assignment 模板

项目外写 assignment 时用 Typst 模板，不再走 LaTeX export：

1. 打开目标 `.typ` 文件。
2. 执行 `M-x my/template-switch`，kind 选 `typst`，template 选 `assignment.typ`。
3. 模板插入时会在当前项目根目录创建 `_typst/*.typ` 软链：
   - `_typst/assignment.typ` -> `~/.config/emacs/notes/assignment.typ`
   - `_typst/rho.typ` -> `~/.config/emacs/notes/rho.typ`
   - `_typst/aleph-notas.typ` -> `~/.config/emacs/notes/aleph-notas.typ`
   - `_typst/note.typ` -> `~/.config/emacs/lisp/note/typst/note.typ`

插入后的文件只需要写：

```typst
#import "/_typst/assignment.typ": *
#show: body => assignment-theme(
  title: "Assignment",
  author: "hc",
  date: "2026-05-11",
  body,
)
```

这样模板在任何项目里都能用 root-relative import，样式文件仍集中维护在 Emacs 配置仓库。手动编译时，如果 Typst 没有自动识别 project root，可以显式指定：

```sh
typst compile --root <project-root> main.typ main.pdf
```

`notes/assignment.typ` 对应旧 `latex/assignment.cls` 的常用能力：页眉页脚、目录、problem/solution、代码块、数学 operator 和页面引用。`notes/rho.typ`、`notes/aleph-notas.typ` 是从旧 cls 迁过来的项目外写作样式。

## 7. 快捷键

Typst buffer 里：

- `C-c C-p`
  打开 Tinymist preview。
- `C-c C-j`
  preview 同步到当前源码位置。
- `M-]` / `M-[`
  在 Typst 里优先按 `[...]` 内容块跳到右/左括号；有 Copilot ghost text 时 `M-]` 仍先接受补全。Typst mode 默认自动尝试开启 Copilot。
- `C-c n n`
  新建 Typst note；选择目标文件夹，默认不强制输入 tag。
- `C-u C-c n n`
  新建 Typst note，并提示可选 tags。
- `C-c n c`
  capture：从 `idea / inbox / mail / note / meeting / protocol / uni / life` 里选分类，写入 `daily/<sub>/<slug>-YYYYMMDD.typ`。
- `C-c n d`
  打开当天 `daily/YYYY-MM-DD.typ`（不存在就新建），并在末尾追加 `== HH:MM` 小节。
- `C-c n f`
  查找 note。
- `C-c n i`
  插入 `#note("id")[title]`（跨 note 链接）。
- `C-c n p`
  在当前行创建或复用 `<label>`，复制 `@label`。
- `C-c n r`
  插入 reference：可从当前 buffer、indexed note 或文件路径选 target；同文件插 `@label`，跨 note 优先插 `#note("id")[...]`，普通文件退回 `#link(...)`。
- `C-u C-c n r`
  只从当前 buffer 的本地 labels / citations 里选。
- `C-c n R`
  从 BibTeX 插入 Typst `@citekey`。
- `C-c n l`
  查看当前 note 的反链。
- `C-c n A`
  Note Agenda：跨所有 note 收集 `#todo / #doing / #waiting` 任务 chip；前缀 `C-u` 同时列出 `#done / #cancelled`。
- `C-c n b`
  Note Agenda Board：按 state 分列展示任务 chip。
- `C-c n g`
  打开本地 graph；搜索框支持全文词和 `tag:` / `alias:` / `path:` / `title:`。
- `C-c n t`
  给当前 note 加 tag，可一次输入多个 tag，支持空格、逗号、`:tag:`、`#tag`。
- `C-c n a`
  加 alias。
- `C-c n s`
  重建 note 索引。
- `C-c n y`
  粘贴剪贴板图片，保存到当前文件旁的 `img/<file>/`，插入 `#image(...)`。
- `C-c n z`
  用 Zotero/BibTeX 内容填充模板里的 `${title}`、`${author}`、`${year}`、`${citekey}`、`${doi}`。

macOS GUI 下，`H-o` 已经转成 note 前缀：

- `H-o n` 新建 note（可选文件夹；默认不强制 tag）
- `H-o c` capture（分类 daily 笔记）
- `H-o d` 当天 daily note
- `H-o f` 查找 note
- `H-o i` 插入 note link
- `H-o p` 创建或复用当前行 label，并复制 `@label`
- `H-o r` 插入 reference（`@label` / `#note(...)` / `#link(...)`）
- `H-o R` 插入 citation `@citekey`
- `H-o l` 反链
- `H-o A` Note Agenda（任务 chip 聚合）
- `H-o b` Note Agenda Board
- `H-o s` 重建索引
- `H-o o` 打开光标处 note/link/reference；没有可打开目标时同步 preview
- `H-o RET` 打开光标处 note/link/reference
- `H-o t` 加 tag
- `H-o a` 加 alias
- `H-o y` 粘贴图片
- `H-o z` Zotero metadata 填充

Typst note buffer 里 evil normal `RET` 和 `C-c n RET` 都会打开光标处 `#note`、普通 `#link`、`#zoterolink` 或本地 `@label`。如果当前位置不是可打开引用，`H-o o` / `RET` 才退回 Tinymist preview sync。

`H-p` 现在是 reference dispatcher：Typst buffer 走 `my/note-reference-insert`，Org buffer 仍然走原来的 `my/org-reference-insert-link`。

全局 `H-y` 也指向 Typst note 图片粘贴。

## 8. Snippet

Typst snippets 在 [snippets/typst-ts-mode/](../snippets/typst-ts-mode/)。`typst-mode/` 是同目录软链，所以 `typst-mode`、`typst-ts-mode`、`my/typst-mode` 共用同一套模板。

这套 snippet 保留了原来 Org/TeX 写作的触发词，但 body 已经改成 Typst：

- 数学符号 / 希腊字母：`aaaa`、`RR`、`frac`、`sum`、`prod`、`int`、`lim`。
- 装饰：`bar`、`hat`、`tilde`、`dot`、`ddot`、`vec`。
- 矩阵：`mat`、`pmat`、`bmat`、`cases`。
- 分节和引用：`sec`、`sub`、`ref`、`cite`、`figure:ref`、`table:ref`。
- 块：`def`、`thm`、`lem`、`proof`、`que`、`summ`、`imp`、`warn`、`tip`、`info`、`rem`、`ex`、`sol`。
- 批注 / 标注：`comment` / `cmt`、`annotation` / `ann`、`marginal`、`mnote`、`sidenote` / `side`。
- 行内注记：`fn` / `foot`、`topmark` / `supmark`、`bottommark` / `submark`。
- note link：`nlink` 展开成 `#note("id")[title]`。

## 9. 不再维护

- 不再维护 Org 文档主线。
- 不再维护旧 LaTeX `latex/*.cls` 导出类。
- 不再把 `H-o` 当作 Org 前缀。
- 不做 Org 到 Typst 的自动转换。
- 不实现 Org Babel 的 Typst 等价层。
- 不再用 per-note HTML 导出来承载公式和 note 页面视觉。
