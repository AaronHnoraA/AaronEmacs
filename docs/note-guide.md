# Typst Note Guide

这套 note 系统现在以 Typst 为中心。旧的 Org 文档、导出类和 `H-o` Org 入口不再作为主线维护；笔记、assignment 和项目外写作都走 `.typ`。

## 1. 模块边界

- [lisp/note/init-note.el](../lisp/note/init-note.el)
  负责 note 根目录、索引、跳转、反链、Tinymist 预览同步，以及写入 note helper。
- [lisp/note/init-note-metadata.el](../lisp/note/init-note-metadata.el)
  编辑 metadata 块：加 tag（completing-read 已有 tag）、加 alias。
- [lisp/note/init-note-capture.el](../lisp/note/init-note-capture.el)
  分类 daily capture (`my/note-capture`) 和当天滚动 daily note (`my/note-daily-today`)。
- [lisp/note/init-note-reference.el](../lisp/note/init-note-reference.el)
  扫当前 buffer 的 `<label>` 候选并插入 `@label`；同时提供 `my/reference-insert-dispatch` 按 mode 路由到 Typst / Org reference。
- [lisp/note/init-note-agenda.el](../lisp/note/init-note-agenda.el)
  跨所有 note 收集 `#todo / #doing / #waiting / #done / #cancelled` chip，分组成 `*Note Agenda*`。
- [lisp/note/init-note-graph.el](../lisp/note/init-note-graph.el)
  本地 graph 视图。
- [lisp/note/init-note-tools.el](../lisp/note/init-note-tools.el)
  Zotero metadata 填充、剪贴板图片粘贴。
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

`M-x my/note-db-sync` 会重建 `var/note/note.db`，并在 note 根目录写入：

- `_typst/note.typ`
  从 [lisp/note/typst/note.typ](../lisp/note/typst/note.typ) 复制出来的 helper。
- `_typst/notes/<id>.typ`
  每个 note 的小 wrapper，用于跨文件 import/include。

note helper 提供 `note-entry`、`note-theme`、`note`、`note-include`、`note-transclude`、`note-import-path`，以及 `definition`、`theorem`、`proof`、`example`、`remark`、`summary`、`question`、`important`、`warning`、`tip`、`info` 等卡片块。

行内任务 chip：`#todo[...]`、`#doing[...]`、`#waiting[...]`、`#done[...]`、`#cancelled[...]`。常配合列表使用，例如 `- #todo[Write proof of Lemma 3]`。这些 chip 是 `my/note-agenda` 的扫描入口，调用形式 `#<state>[body]` 不要改。`done` 和 `cancelled` 会自动给 body 加灰色 + 删除线。

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

`note-include` / `note-transclude` 会把被包含文件标成非入口文档，避免子文件自己的目录重复出现在当前 PDF。公式、定理、章节引用仍然使用 Typst 原生 `<label>` / `@label`；`my/note-reference-insert`（`C-c n r` / `H-o r`）会扫当前 buffer 里所有 `<label>` 给 completing-read 选择再插 `@label`，infrastructure 标签（`<note>` 这种）自动从候选里排除。

## 4. Capture / Daily / Agenda

这部分接住了过去 org-capture + org-agenda 的工作流。所有产物都是 Typst note，直接进 `note.db` 索引。

**Capture**：`my/note-capture`（`H-o c` / `C-c n c`）从 `my/note-capture-categories` 默认列表 `idea / inbox / mail / note / meeting / protocol / uni / life` 里选分类，输入 title，写入 `<note-directory>/daily/<category>/<slug>-YYYYMMDD.typ`。文件自带完整 metadata + 一级标题，落地后立即触发 `my/note-db-sync`。新增分类只需修改 `my/note-capture-categories`。

**Daily**：`my/note-daily-today`（`H-o d` / `C-c n d`）打开当天的滚动文件 `<note-directory>/daily/YYYY-MM-DD.typ`（所有时段写在同一篇），并在末尾追加 `== HH:MM` 小节作为新时段入口。文件不存在时自动写 metadata 并 sync。文件名格式由 `my/note-daily-rolling-file-format` 控制。

**Agenda**：`my/note-agenda`（`H-o A` / `C-c n A`）regex 扫所有 note 找任务 chip 调用，按 state 分组渲染到 `*Note Agenda*` buffer，每条以 `file:line` 按钮起头，点击/回车跳转。默认只展示 `my/note-agenda-active-states`（`todo` / `doing` / `waiting`），前缀 `C-u` 等价于 `my/note-task-states` 全集（加上 `done` / `cancelled`）。

任务 chip 是行内调用，常配列表：

```typst
- #todo[Write proof of Lemma 3]
- #doing[Polish lemma 2 statement]
- #waiting[Reviewer feedback on §3]
- #done[Submit COMP3927 assignment]
- #cancelled[Old framing of Theorem 1]
```

视觉上是圆角胶囊 + 跟在后面的文本；`done` / `cancelled` 自动给 body 上灰色 + 删除线。scanner 锚定 `#<state>[body]` 这个调用形式，不要把 chip 重命名或改成块。要加新 state，同时改 `lisp/note/typst/note.typ` 的 `task-tag` 调用和 Emacs 端 `my/note-task-states`。

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

因此，`note.css` / `publish.css` 只影响网站壳、archive 和 graph，不影响 PDF 里的 note 视觉。公开 PDF 缺的页面、标题、目录、卡片、页眉页脚等效果，应该补到 `~/HC/Org/_typst/publish.typ`，不要补一个发布专用 CSS，也不要把日常 `/_typst/note.typ` 改成只适合发布。

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
- `C-c n n`
  新建 Typst note。
- `C-c n c`
  capture：从 `idea / inbox / mail / note / meeting / protocol / uni / life` 里选分类，写入 `daily/<sub>/<slug>-YYYYMMDD.typ`。
- `C-c n d`
  打开当天 `daily/YYYY-MM-DD.typ`（不存在就新建），并在末尾追加 `== HH:MM` 小节。
- `C-c n f`
  查找 note。
- `C-c n i`
  插入 `#note("id")[title]`（跨 note 链接）。
- `C-c n r`
  插入 `@label`：扫当前 buffer 里的所有 `<label>`，带行内容预览。
- `C-c n l`
  查看当前 note 的反链。
- `C-c n A`
  Note Agenda：跨所有 note 收集 `#todo / #doing / #waiting` 任务 chip；前缀 `C-u` 同时列出 `#done / #cancelled`。
- `C-c n t`
  给当前 note 加 tag，从全库已有 tag 集合 completing-read。
- `C-c n a`
  加 alias。
- `C-c n s`
  重建 note 索引。
- `C-c n y`
  粘贴剪贴板图片，保存到当前文件旁的 `img/<file>/`，插入 `#image(...)`。
- `C-c n z`
  用 Zotero/BibTeX 内容填充模板里的 `${title}`、`${author}`、`${year}`、`${citekey}`、`${doi}`。

macOS GUI 下，`H-o` 已经转成 note 前缀：

- `H-o n` 新建 note
- `H-o c` capture（分类 daily 笔记）
- `H-o d` 当天 daily note
- `H-o f` 查找 note
- `H-o i` 插入 note link
- `H-o r` 插入 `@label` 引用
- `H-o l` 反链
- `H-o A` Note Agenda（任务 chip 聚合）
- `H-o s` 重建索引
- `H-o o` 打开 preview 并同步
- `H-o RET` 打开光标处 note
- `H-o t` 加 tag
- `H-o a` 加 alias
- `H-o y` 粘贴图片
- `H-o z` Zotero metadata 填充

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
- note link：`nlink` 展开成 `#note("id")[title]`。

## 9. 不再维护

- 不再维护 Org 文档主线。
- 不再维护旧 LaTeX `latex/*.cls` 导出类。
- 不再把 `H-o` 当作 Org 前缀。
- 不做 Org 到 Typst 的自动转换。
- 不实现 Org Babel 的 Typst 等价层。
- 不再用 per-note HTML 导出来承载公式和 note 页面视觉。
