# Noema source syntax

The complete reference for what Noema reads from disk: the files and
directories a vault uses, and every form a note or work document may contain
on top of CommonMark/GFM.  Each section names the module that parses the form;
where another document owns a family in depth (planning, work-cell
directives, search, export), this page gives the forms and links there for
the full grammar.  When this page and its owner disagree, the owner (code plus
fixtures) is right and this page is stale.

Rules that hold for every form:

- A note is plain Markdown and stays readable in any other editor.  A view
  never invents a form the source cannot hold, and the editor writes back the
  form it read.
- A grammar that the browser, the Node host and the Go kernel all read has one
  parser in `shared/*.mjs`.  `src/` imports it through a typed facade, and a Go
  port is pinned to it by a `shared/*-fixtures.json` file.
- Nothing inside a code span, a code fence or an HTML comment is a Noema
  form.  `\@@` escapes a command; `\[`/`\]` inside a bracketed command body
  are literal.
- Only the opening lines of a note can hold its metadata; later lookalikes
  are examples.  In a `.noema` work block only the leading region is control
  text; later lookalikes are data.
- A new form takes its place on this page in the change that adds its parser.

Paths are relative to the Noema repository `site-lisp/noema/`.

## Files and directories

### Vault layout

```text
~/Documents/Noema/                 note root (NOEMA_ROOT)
├── .envrc  flake.nix  Makefile    user-owned toolchain; .envrc is exported to tool subprocesses
├── .noema/wiki.db                 disposable SQLite index, never committed
├── public/<repo>/.git             published partition, one Git repository per child
└── private/<repo>/.git            never published
```

`workspace.layout` is `wiki` (above) or `legacy` (one repository).  A child
directory without `.git` is reported, never indexed.  Spec:
[wiki-workspace.md](wiki-workspace.md) “Layouts”.

### Files Noema recognizes

| File | Meaning | Owner / spec |
| --- | --- | --- |
| `*.md` | knowledge note (Wiki page unless `roam: off`) | this page |
| `*.noema` | legacy work-document JSON, retained for model/runtime data; no dedicated editor or Jupyter path | `AGENTS.md` D-023 |
| `*.prompt` | standalone prompt with the same leading directives, plus `@@workstream(ws_…)` and legacy single-`@` spellings | `server/lib/research-directives.mjs` |
| `*.ipynb` | ordinary notebook, full Jupyter support | [../../design/jupyter-workflow.md](../../design/jupyter-workflow.md) |
| `.cell/NOTE.LANGUAGE.SESSION.ipynb` | sidecar holding a note's `@@cell` sources and outputs; `LANGUAGE` is the notebook language, not the kernelspec | jupyter-workflow.md |
| `noema.toml` | Wiki repository manifest and/or research Project (below) | `AGENTS.md` D-038 |
| `.agent/` | a Project's state (`state.sqlite`), ignores itself | `server/lib/research-project.mjs` |
| `*.typ` | Typst file: editable, not a Wiki page | [../../guide/research-notes-workflow.md](../../guide/research-notes-workflow.md) |
| images, PDFs, `*.html` | attachments: only their metadata is indexed | wiki-workspace.md |
| `~/.config/noema/katex-macros.tex` | the user's global math macros | [../../design/latex-preview.md](../../design/latex-preview.md) |

### A note's own files

A note owns two directories beside it, named by its file stem
(`noteAssetFolderName` in `shared/tikz-source.mjs`: NFKC, path-unsafe
characters and whitespace become `-`):

```text
notes/Tensor Basics.md
notes/images/Tensor-Basics/        images the note shows (paste, drop, TikZ, screenshots)
notes/attachments/Tensor-Basics/   every other file it links (PDF, data, HTML)
```

The note links them relatively, `![](./images/Tensor-Basics/fig-1.png)`.  A
name already taken gets a numeric suffix, never overwrites.  Paste and import
(`storeAsset`, `storeAssetFromPath` in `server/lib/runtime.mjs`) write here,
Asset Maintenance checks these folders for orphans, and a page move carries
them along; a file elsewhere is not the note's.  Working material that the
note does not show (sources, candidates, logs, caches) belongs to the tool
that made it, not in the vault.

### `noema.toml`

```toml
schema = 1
repository_id = "019…"              # written by Wiki registration: a Wiki repository
namespace = "Mathematics"           # default namespace of the repository's pages
namespace_aliases = ["Math", "数学"]

[project]                           # only this table makes a research Project
id = "…"
workspace = "../code"               # optional: where the Project's agents run
```

A repository manifest alone is never a Project; the Project root is the
nearest manifest with `[project]`.  Resolution: `noema-project-root` (Elisp)
and `server/lib/research-project.mjs` (Node).

### Source-file markers

Code outside notes marks regions for embedding with a comment
`<comment> @noema TAG`; a region ends at the next `@noema` or `@note-code`
marker.  The old spelling `@aaronnote` is still read.  A Markdown note embeds
a region with `@@note-code(path) [TAG]`, a Typst note with `#note-code`.

### Note templates

`resources/templates/noema/markdown-mode/*` (and `slides/`) are snippet files
with a header, then the body:

```text
# name: Markdown · article draft
# key: article
# type: regular          # roam = a graph page, regular = a plain document
# group: Writing
# --
# {{title}}
```

Placeholders are `{{title}}` and `{{date}}` (the author's calendar date).

## Note metadata

```text
#+begin meta
id: 019…
title: Tensor Product
date: 2026-10-23
tags: [math, algebra]
#+begin summary Abstract
One paragraph.
#+end summary
#+end meta
```

`key: value` per line; a list is `[a, b]` or `a, b`.  A leading `---` YAML
block is also read (`src/simple-frontmatter.ts`).  Parsers:
`src/org-meta.ts`, `shared/meta-summary.mjs`, kernel
`model/markdown_note_catalog.go`; edits go through `kernel/noema/metadata`.

| Key | Meaning | Read by |
| --- | --- | --- |
| `id` | stable page identity (UUIDv7; older ids kept) | index, links |
| `title` | page title | index, cover, export |
| `author` | article byline | LaTeX export |
| `date` | date shown on the cover and exported | cover, export |
| `tags` | tags (also inline `@@tag[…]`) | index |
| `aliases` | other titles the page answers to | index, links |
| `refs` | pages this note references besides the links in its body | index (backlinks) |
| `kind` (`kinds`) | page kind: `default`, `slides`, … | index, slides |
| `namespace` | overrides the repository namespace | index, links |
| `project` (`proj`) | planning project key | agenda |
| `source` | where the content came from; also a key the note is found by | index, lookups |
| `summary` | one-line summary (or the `summary` block) | index, cover, article export |
| `roam` | `off` keeps the page out of the Wiki graph | index |
| `macros-from` | `.cls/.sty/.tex` files whose macros the note imports | `shared/note-macros.mjs` |
| `css` | absolute path or URL of an extra stylesheet | HTML export |
| `annotations` | `none` drops review annotations from export | LaTeX export |
| `private`, `hidden` | `true` keeps a public page out of publishing | publisher |
| `order`, `pinned` | reading order and the Pinned shelf of the public reader | publisher |

Nested blocks inside `meta`: `#+begin summary [title]` (the abstract) and
`#+begin macros` (the note's own `\newcommand`s, which win over every other
macro layer).

## Markdown extensions

Base grammar is CommonMark plus GFM tables, task lists, strikethrough and
autolinks.  Noema adds, in `src/render-html.ts` and the editor's Lezer rules:

| Form | Meaning |
| --- | --- |
| `==text==`, `^sup^`, `~sub~` | highlight, superscript, subscript |
| `:name:` | emoji shortcode |
| `**（注）**说明` | emphasis next to CJK punctuation works (`src/cjk-emphasis.ts`) |
| `[toc]` on its own line | table of contents |
| `> [!type] Title` | callout; `type` is any word (`note`, `tip`, `warning`, …); first line is the title |
| `[^id]` / `[^id]: text` | footnote; continuation lines indented two or more spaces |
| `{==text==}{>>#c-ID<<}` | comment anchor: the commented text and its comment's id (C70, C80). The comment lives in the folder's hidden `.noema-comments.jsonl`; the editor hides the markers, highlights the text and pops the comment's card up above it; the text may hold math. HTML and the Wiki index keep only the text. One line, not nested |
| `## Heading {#slug}` | explicit heading anchor |
| `## Heading <!-- omit from toc -->` | heading left out of the TOC and the slide list |
| `1)` and `(a)` list markers | ordered lists with matching labels in export |
| `\` at line end | hard line break |
| ` ```python title="ridge.py" {2-3,5} ` | code fence attributes (`src/code-info.ts`): the language is the first word; `title` is shown before the language badge and as the exported `<figcaption>`; `{…}` marks body lines (from 1) with a band. Other editors read only the language |
| safe inline HTML (`<kbd>`) | sanitized by `src/sanitize-html.ts`; scripts, styles, iframes, objects, embeds, forms and dialogs are removed, and so are the `formaction`, `form` and `popover*` attributes |

Heading numbers are display-only (`src/heading-number.ts`) and never enter
the source.

## Block environments

```text
#+begin KIND [title] [{#id key=value}] [{attrs}]
…
#+end KIND
```

Open and close lines pair by kind and nest.  Parser: `ORG_ENV_OPEN_RE` in
`src/render-html.ts`; editor widgets in
`src/cm6/extensions/visual/widgets/block-extras.ts`.

| Kind | Meaning |
| --- | --- |
| `meta`, `summary`, `macros` | note metadata (above) |
| `theorem`/`thm`, `lemma`, `proposition`/`prop`, `corollary`/`cor`, `definition`/`defn`, `property`, `axiom`, `assumption`, `conjecture`, `claim`, `remark`, `notation`, `observation`, `convention`, `example`, `exercise`, `solution`, `algorithm`, `question` | titled mathematical environment; LaTeX export maps it to the environment of the same name |
| `proof` | proof; a title `=>` or `<=` exports as Proof (⇒)/(⇐) |
| `note`, `info`, `attention`, `warning`, `important` | admonition |
| `comment` | collapsed comment, shown on demand |
| `fold` | folded region |
| `progress [title]` | a Markdown list of named percentages or completed/total amounts, rendered as aligned progress rows |
| `stats [title]` | a GFM table rendered as metric cards with optional trend, note, series and semantic accent |
| `columns [{widths=60-40 valign=top gap=md rule=on align=left}]` | multi-column layout; each pane separated by `#+col [{align=...}]` holds arbitrary multi-block Markdown (headings, lists, math, code, images, tables, nested org-envs), or `{mode=flow cols=2..4}` for newspaper-style flowing prose; exports to LaTeX `minipage` / `multicols` (`shared/columns-layout.mjs`) |
| `tikz [id]` | TikZ/PGFPlots figure, kept verbatim in LaTeX (`shared/tikz-source.mjs`) |
| `html` | raw HTML block, sanitized by `src/sanitize-html.ts` in the editor and in HTML export alike (`src/render-html.ts`), so a published page shows what the editor showed |
| `av [title]` | attribute view (below) |
| `embed [title]` | query embed (below) |
| `lean4` | Lean source carrier, not rendered as an environment |
| `ediff [title]` | local pending edit at its original lines; editor review card, old side used for export |
| any other word | generic titled box with that label |

Which kinds are prose for spelling and diagnostics:
`TECHNICAL_ORG_ENV_KINDS` in `shared/prose-mask.mjs`.  Which kinds can carry
a block id: everything except `NON_SEMANTIC_ORG_ENVS` in
`shared/block-identity.mjs`.

### Progress rows

Use `#+begin progress [title]` and `#+end progress` around a flat unordered list.
Each item separates its inline Markdown label and amount with `:` or `：`.

```markdown
#+begin progress 文档交付
- 解析：75%
- **复核**: 4/8
#+end progress
```

HTML and the visual editor display the named rows and keep the amount visible
as text. The bar stops at its maximum and the text shows the written amount.
An invalid list, including a fraction with zero total, uses ordinary Markdown
rendering inside the environment. Entering the block in the editor reveals
the original text. Editing or switching surface leaves the source in the
same Markdown form. The default LaTeX conversion keeps the title and original
items in its ordinary environment fallback.

### Metric cards

Use `#+begin stats [title]` and `#+end stats` around one GFM table. The first
two columns give the metric name and its formatted value. Optional columns in
order give the trend, a note, a comma-separated series of finite numbers and a
semantic accent. Column headings can use the document's language.

```markdown
#+begin stats 文档交付
| 指标 | 数值 | 趋势 | 说明 | 走势 | 强调 |
| --- | ---: | --- | --- | --- | --- |
| 验收用例 | 403 项 | +18% | 完整验收 | 4,6,5,8 | success |
| 遗留问题 | 0 | | | | neutral |
#+end stats
```

Accents are `neutral`, `primary`, `info`, `success`, `warning`, `danger` and
`important`. Trend direction alone does not assign an accent. An incompatible
body renders as ordinary Markdown. The editor shows the table on entering the
body, and the default LaTeX conversion keeps the table and its headings.

### Data charts

Use `#+begin chart [title] {type=bar}` and `#+end chart` around one GFM table.
The first column labels observations. Put quantities and units in the
remaining column headings. Each row has the same number of cells as the header.
Keep an explicit empty cell for a missing observation where the chart permits it.

```markdown
#+begin chart 耗时差 {type=bar accent=info}
| 阶段 | 相对基准 (ms) |
| --- | ---: |
| 解析 | -4 |
| 布局 | 0 |
| 绘制 | 8 |
#+end chart
```

- `bar` is the default. It uses one measurement column and a shared zero line.
- `stacked` uses one nonnegative measurement column with a positive total.
  The legend keeps source order, exact inputs and their shares of the total.
- `heatmap` uses one or more measurement columns. Cells show exact values,
  with separate colors for negative and positive values and a dash for missing data.
- `line` uses one measurement column and equal spacing in source order.
  Empty cells break the line. The table keeps the observation labels and exact values.

`accent` uses the same semantic roles as metric cards. The editor reveals
the table and options on entering the block. A nonrectangular table remains
readable as source. Other unsupported shapes use ordinary Markdown. LaTeX
uses the display title and the source table.

### Timelines and steps

Use `#+begin timeline [title]` around a Markdown list of events. Separate the
time and event title with ` · ` or ` — ` to give them distinct rows. Continue
with ordinary list content. A leading `[success]`, `[warning]`, `[danger]` or `[info]` on an
event adds a visible status label. Add `{orientation=horizontal}` for cards
across a wide content area. A narrow area displays the events in one column.
The `card=true|false` and `list-style=decimal|bullet|none` options select the
card surface and the marker style. Horizontal timelines use cards by default.

```markdown
#+begin timeline 发布记录 {orientation=horizontal}
- [success] **11 月 20 日** · 完成解析

  记录关键边界与验证结果。

- **11 月 25 日** · 交付
#+end timeline
```

`#+begin steps [title]` uses an ordered Markdown list. The authored starting
number carries through to the visual list. An optional `[x]` marks a done
step. The first `[ ]` marks the current step and later unchecked steps are
pending. The `list-style` option takes the same three marker styles.

```markdown
#+begin steps 审阅流程
3. [x] 锁定范围
4. [ ] 对照内容
5. [ ] 提交
#+end steps
```

Both environments reveal the list in the editor on entry. LaTeX keeps the
display title and original Markdown list.

### Sortable document tables

Use `#+begin table [title]` around one GFM table to use the document table
style. Add `sortable=true` for header buttons and `copy=true` for a copy
button in Noema. Sorting keeps the authored order of equal values and toggles
direction on the active header. Copy writes the displayed order as GFM with
its inline Markdown and column alignment. Entering a cell opens its source
position, including after the view has been sorted.

Numeric sorting accepts signed decimals and scientific notation. Ascending
order groups numbers before text. Reference links keep their definitions when
copied. In a narrow layout with hidden columns, the `More →` button scrolls
within the table.

```markdown
#+begin table 阶段耗时 {sortable=true copy=true highlight-cells="2:2"}
| 阶段 | 耗时 (ms) |
| --- | ---: |
| 解析 | 4 |
| 布局 | 7 |
#+end table
```

`highlight-rows`, `highlight-columns` and `highlight-cells` take
space-separated one-based coordinates in the authored table. Row indexes
start at the first data row. A cell uses `row:column`. `highlight` takes a
metric card's semantic accent. `align`, `width` and `text-align` position the
table and give its default cell alignment. GFM column alignment takes priority.
Published HTML and LaTeX present the formatted table in its authored order.

### Document frames and alignment

`#+begin box [title]` uses ordinary Markdown inside a quiet frame. Its options
include `surface=none|paper|quiet|raised`, `padding`, `width`, `align` and
`text-align`.

`#+begin card [title]` can add a `description`, semantic `accent`, optional
`icon=link|file|check|info`, and a primary `href`. Use `action` for the link
label. Ordinary links in the Markdown body keep their own destinations.

`#+begin panel [title]` can display `status`, `accent` and `focus=true`.
`variant=flat` uses the outline surface. Box, card, and panel widths are
bounded by the document column.

```markdown
#+begin card 使用手册 {description="命令与示例" href=https://example.com action="查看手册" accent=info}
在这里补充阅读说明和 [版本记录](https://example.org)。
#+end card

#+begin panel 交付 {status="待复核" focus=true accent=warning}
核对窄窗和键盘操作。
#+end panel
```

Wrap ordinary Markdown in `center`, `left`, `right` or `justify` environments
to align their contents. Opening any of these environments in the editor
exposes the original Markdown. LaTeX preserves titles, body content, card
descriptions and destinations, and panel status text.

### Home, link, and image cards

`home-box` introduces a document or section. It accepts a `description`,
semantic `accent`, `surface`, optional `href` and `action`, and an ordinary
Markdown body.

```markdown
#+begin home-box 渲染工作台 {description="从渲染规则到实机检查" accent=info href="#guide" action="查看阅读指南"}
先核对源码与预览，再检查深浅主题和键盘操作。
#+end home-box
```

`link-card` uses `href` and `action`, or promotes a standalone Markdown
link at the start of its body. `image-card` uses a standalone leading
Markdown image as the cover. Reference-style images and normal attachment
paths work here. For short one-line card definitions, `src` and `alt` can
specify the cover in the opening line. Both card types may include a
description and additional Markdown. The main action can be reached with
Tab or by clicking the card surface. Secondary links keep their own targets,
and the editable body and environment heading still open the source.

```markdown
#+begin link-card 键盘与交互 {description="快捷键、焦点和阅读位置"}
[打开交互清单](interaction.md)
#+end link-card

#+begin image-card 窄窗专题 {description="布局示例" href="interaction.md" action="阅读窄窗指南"}
![窄窗下单列文档](assets/ui-narrow.png)
#+end image-card
```

LaTeX keeps card descriptions, destinations, and Markdown body images.
A cover specified with `src` and `alt` is exported as its path and
alternative text. All three card types accept `align` and `width`.

### Reading and navigation blocks

`breadcrumb` uses a list of paths or links in reading order. The last item
is the current page. The optional `separator` sets the visible delimiter.

```markdown
#+begin breadcrumb 文档位置
- [工作区](#workspace)
- [渲染手册](#renderer)
- 当前页
#+end breadcrumb
```

`keymap` displays one shortcut per list item. Separate the shortcut and its
description with a colon. Use spaces between sequential keys and ` / `
between alternatives. `columns` accepts `1`, `2`, or `3` and narrows to fit
the page. `statusbar` can use the same list, optional `mode` and `note`, and
a comma-separated `keys` attribute. The keys display as labels.

```markdown
#+begin keymap 常用快捷键 {columns=2}
- C-x C-f / ⌘O : 打开文件
- M-: : 求值
#+end keymap

#+begin statusbar 编辑状态 {mode=READ keys="C-f:搜索, C-x C-s:保存" note="待复核"}
#+end statusbar
```

`log` aligns optional timestamps (`HH:mm`, `HH:mm:ss`, or the same time
preceded by `YYYY-MM-DD` and a space or `T`), known severity labels, and
messages. Severity includes `TRACE`, `DEBUG`, `INFO`, `NOTICE`, `WARN`,
`WARNING`, `ERROR`, `FATAL`, `OK`, and `SUCCESS`. It is case-insensitive.
Entries may omit the timestamp. Ordinary list entries remain readable.

```markdown
#+begin log 渲染记录
- 09:10:18 INFO 载入 Markdown
- 09:12:30 [WARN] 重新测量长行、公式和组件宽度
- 本轮保留完整的原始文本。
#+end log
```

`pros-cons` displays two Markdown sections. Separate them with `#+col` and
set their labels with `pros` and `cons`. Narrow windows show them in source
order, one below the other.

```markdown
#+begin pros-cons 排版选择 {pros="优势" cons="限制"}
- 文档使用普通 Markdown。

#+col

- 高级组件需要单独验证。
#+end pros-cons
```

Entering a block in the editor exposes its source. LaTeX keeps the original
body, status metadata, and the labels of valid comparison sections.

### QR codes

Use `#+begin qrcode [title] {text="value"}` to encode a short value. The
optional body is a Markdown caption. The QR code is generated locally, and
the encoded value remains visible as plain text.

```markdown
#+begin qrcode 阅读手册 {text="https://example.com/noema/guide?lang=zh&view=compact"}
用手机扫描，打开手册。
#+end qrcode
```

For multiline values and text containing braces, omit `text` and use a
single plain or `text` fence. Spaces, punctuation, and line breaks inside
the fence become part of the code. If a value exceeds the encoder's capacity,
it remains visible with a short message. HTML includes the generated symbol.
LaTeX keeps the caption and the literal content as readable text.

```markdown
#+begin qrcode 传递文字
~~~text
阅读手册 · 自动折行
状态：已复核 ✓
~~~
#+end qrcode
```

### Tabbed documents and examples

`tabs` uses two or more top-level Markdown headings of the same level as
page boundaries. The first heading sets that level. Deeper headings belong
to the current page. Content before the first heading appears above the
controls. `tab-panel` adds a frame, and `swiper` adds previous and next
buttons. Use `selected=2` to start on the second page and `accent` for a
semantic highlight.

```markdown
#+begin tabs 验收笔记 {selected=2}
### 摘要
用普通 Markdown 保存原始证据。

### 实机结果
检查窄窗、主题和键盘操作。
#+end tabs
```

`code-tabs` accepts the same headings. A body containing consecutive code
fences can omit the headings. Its tab names come from the fence languages.

````markdown
#+begin code-tabs 调用示例
```bash
noema render notes.md
```
```python
print("Hello, Noema")
```
#+end code-tabs
````

Tab moves to the selected control. Left and Right switch pages, and Home
and End select the first and last page. The choice stays with the block
when editing other parts of the document. Clicking the page content in an
editable tab opens its Markdown. Exported HTML and LaTeX include every page
in source order.

### Details and accordions

`#+begin details [title]` provides a single disclosure. It starts closed
unless you set `open=true`. Its body accepts ordinary Markdown. The source
control opens the block in the editor, and a click in the expanded body
goes to its Markdown.

```markdown
#+begin details 阅读说明 {open=true}
从原文开始核对渲染结果。
#+end details
```

`accordion` divides its body into sections with same-level Markdown headings,
using the same heading rule as `tabs`. It allows one open section by default.
`open=2` starts with the second section open. For independent sections, use
`multiple=true` with `open="1,3"`. Use `expand=true` to open every section
initially. The reader can expand or collapse sections using the mouse, Enter,
or Space. The editor remembers the choices during editing elsewhere.

```markdown
#+begin accordion 核对清单 {open=2}
### 证据
保留原文、数据和实际界面截图。

### 方法
检查所选区间和链接。
#+end accordion
```

Published HTML keeps every section and its default disclosure state. Printed
publications, PDF output, and LaTeX include the complete content.

### Read-only comparisons

`diff` compares two fenced snippets in source order. An optional Markdown
heading immediately before each fence supplies its display label. The
`before` and `after` attributes also set labels. The view defaults to
`auto`, which uses a paired layout in a wide column and a unified layout in
a narrow column. Set `view=split` or `view=unified` for a fixed initial view.

````markdown
#+begin diff 布局变更 {before="原设置" after="新设置"}
~~~text
columns: 2
navigation: sidebar
~~~

~~~text
columns: auto
navigation: drawer
~~~
#+end diff
````

The editor can switch layouts or copy either snippet. Clicking a line opens
its source. Both line number sequences are retained, including in the
unified view. Comparisons that exceed the alignment limit show two labelled
literal snippets. HTML displays the comparison and LaTeX retains both
snippets as code in source order, including when a diff is inside a quotation.

### Pending Markdown edits (`ediff`)

An `ediff` environment replaces the exact Markdown lines **at its position**
with a local diff. Put it where the old lines currently are, including inside
a table. A block covers one contiguous span; use separate blocks for
separated edits. Its old side remains the approved Markdown until a person
accepts.

```text
The existing paragraph stays here.
#+begin ediff Clarify the context receipt
@@ediff-comment "Could we state the limit more directly?"
-old first line
-old second line
+new first line
+new second line
#+end ediff
The next paragraph stays here.
```

Every diff line begins with one space (unchanged context), `-` (old only), or
`+` (new only). A literal Markdown line beginning with one of these
characters gets another prefix. The block can express a replacement,
insertion (no old lines), or deletion (no new lines). For a final line without
a newline, use `\ No newline at end of file` immediately after that side's
last line; the marker is valid only at the end of the note. The card derives
GitHub-like old/new line numbers and a blue hunk heading from the block's
position. Preview compares the old and new Markdown fragments using the
shared renderer; Diff shows the exact source lines. Its compact source view
shows all changed lines when there are at most four. Longer edits preview the
first old and first new line (two lines for a one-sided edit), and Details
states how many changed lines are hidden. The heading shows total added/deleted
counts; Details reveals every line and its context and expands the rendered
fragments. View controls never modify the proposal.
Source has no file headers or absolute
hunk coordinates to rebase.

`@@ediff-comment "JSON string"` lines may appear after the opener and before
the diff lines. One `@@ediff-author {"name": "...", "session": "..."}` line
names the collaborator that proposed the card (written by
`agent:document:propose`, kept when the card is revised); the card shows the
name beside its title. Review metadata lines (`@@ediff-author`,
`@@ediff-status`, `@@ediff-comment`) may come in any order before the diff
lines. Reject inserts `@@ediff-status rejected` immediately after
the opener and leaves the old side active for agent follow-up. Accept replaces
only that environment with its new side in one undoable edit. HTML, LaTeX and
Wiki indexing resolve valid pending and rejected environments to the old
side, including lines inside tables. Malformed blocks remain literal source
for repair. The Tools panel has a file-wide review list; each card remains at
its affected lines. Editor implementation: `src/ediff.ts`,
`src/cm6/extensions/visual/widgets/ediff.ts`, and the org-env branch of
`block-extras.ts`.

Saved comments remain visible on the card. A commented or rejected proposal
offers Send feedback, which saves and sends its exact range through the
existing Noema Markdown-to-agent bridge. The agent receives the file reference
and in-place proposal instructions; session choice and the revision request
use the existing interaction. See [agent-context.md](agent-context.md).

### Attribute view

```text
#+begin av Open work
source: todo
columns: text, status, project, prio, ddl
filter: status in todo|doing
filter: project = paper
sort: prio asc, ddl asc
limit: 20
view: table
#+end av
```

Keys: `source` (`planning`, `todo`, `project`, `milestone`, `clock`, `block`,
`prose`, `org-env`), `columns`, `filter`, `filter-any`/`filter-all`, `type
COLUMN TYPE`, `calc COLUMN OP`, `sort`, `limit`, `group`, `view` (`table`,
`gallery`, `kanban`).  Field types, filter and calc operators are the
`ATTRIBUTE_VIEW_*` lists in `shared/attribute-view.mjs`; the projection is
computed by `kernel/noema/attributeview`.  Fixtures:
`shared/attribute-view-fixtures.json`.
In Emacs, `noema-attribute-view` (`lisp/noema-attribute-view.el`) draws the
same host answer as a native table and edits cells through the same
cell-patch channel; it never evaluates a spec itself.

### Query embed

```text
#+begin embed Recent claims {#UUIDv7}
sql: SELECT …
#+end embed
```

The compact `#+begin embed :sql SELECT …` is also read
(`shared/embed-query.mjs`).

## Block identity and properties

| Form | Meaning | Owner |
| --- | --- | --- |
| `{#ID key=value …}` at a block's end | stable block anchor with properties | `shared/block-identity.mjs`, `shared/block-properties.mjs`, `block-property-fixtures.json` |
| `((UUIDv7))`, `((UUIDv7 "label"))` | reference to a block (SiYuan's legacy `14digits-7chars` ids accepted) | `shared/block-identity.mjs` |
| `{wrap: right, width: 40%}` after an image, figure or block | layout: `wrap`/`float` side, `align`/`position`, `width`/`size`/`w`, `height`/`h` | `shared/layout-attrs.mjs`, Go port pinned by `latex-transform-fixtures.json` |

## Commands

One grammar for every `@@` command (`shared/command-syntax.mjs`):

```text
@@name(switch) [body] {key: value; key: value}
@@name(switch) [body] {
  key: value
}
```

Attributes split on `;`, or on `,` when another `key:` follows; `=` may
replace `:`; values may be quoted.  In the body `\]` and `\\` are escapes.
Editor rendering: `src/cm6/extensions/visual/widgets/inline-commands.ts`.

| Command | Meaning |
| --- | --- |
| `@@todo(status) [title] {…}`, `@@itodo` | task; `@@todo(doing) bare title` also works for quick capture |
| `@@project(status) Title {…}` | planning project |
| `@@milestone(tag) [title] {date: …}` | dated milestone |
| `@@clock [task] {from: …; to: …; note: …}` | time span against a task |
| `@@tag[name]` | inline tag |
| `@@cite(namespace) [key1; key2] {locator: "p. 12"; prefix: "see"; suffix: …}` | citation from a bibliography namespace (`shared/bibliography-syntax.mjs`, `bibliography-fixtures.json`) |
| `@@revision(kind) [original] {advice: "…"; reason: "…"}` | review suggestion; kinds `suggest`, `question`, `error`, `ok`, `note`; old colour names `indigo`, `yellow`, `red`, `green`, `teal` still read (`shared/revision-kinds.mjs`) |
| `@@comment [text]`, `@@comment(true) [text]` | private comment, collapsed / always shown |
| `@@scomment [text]` | side comment, shown beside the text in the editor and LaTeX export; inside a footnote or box the LaTeX form stays inline, and long margin notes can continue across pages |
| `@@part [title]`, `@@section(level) [title] {id: …}` | semantic headings; `level` is empty/`sec`/`section`, `sub`, `subsub`, `subsubsub` (`src/semantic-outline.ts`) |
| `@@latexmk(mark)` | LaTeX layout mark: `newline`, `nbsp`, `allowbreak`, `noindent`, `newpage`, `clearpage`, `nopagebreak`, `keepnext`, `appendix` (`shared/latex-marks.mjs`) |
| `@@latexmk(prompt) [instruction]` | instruction for the LaTeX export's typesetting Agent, about this spot or the whole document (layout, page setup, wording); shown as a chip, never typeset. The draft carries `\noemaprompt{…}` (expands to nothing); the Agent carries it out and removes it, the host strips any left and warns that it was not carried out |
| `@@slides(reveal) []`, `@@slides(vertical) []` | slide deck markers (below) |
| `@@cell(language, session) [cell-id]` | executable cell backed by the `.cell/` sidecar |
| `@@note-code(path) [TAG]` | embed the source region marked `@noema TAG` (`shared/note-code.mjs`) |
| `@@lean4(selector) [TAG]` | embed a region of the note's matching `.lean` file (`shared/lean-placeholder.mjs`) |

A whole-line `@@todo`, `@@itodo`, `@@comment` or `@@cell` is hidden in
rendered HTML; how each command exports is
[latex-export-style.md](latex-export-style.md) “Construct mapping”.

### Planning

Kinds `todo | itodo | project | milestone | clock`; task statuses `todo`,
`doing`, `done`, `blocked`, `cancelled`; a project defaults to `active`.

```text
@@todo(doing) [Write the tutorial] {ddl: tomorrow; prio: A; project: noema}
@@todo(todo) [Proof-read] {
  sche: +2d
  effort: 90m
  after: #019…
  repeat: ++1w
}
```

Keys, aliases (`due`/`deadline` → `ddl`, …), dates (`today`, `+3d`, ISO),
repeaters (`+`, `++`, `.+`), durations, dependency references and stable ids
are specified in [agenda-dsl.md](agenda-dsl.md) “Structural grammar” through
“Stable ids”.  Parsers: `shared/planning-dsl.mjs`,
`shared/planning-values.mjs`; fixtures `planning-*-fixtures.json`.  A bad
value becomes a diagnostic, never a lost task.

## Links and references

| Form | Meaning |
| --- | --- |
| `[[Title]]`, `[[Title\|label]]` | page by title or alias: the source repository first, then the global index |
| `[[Namespace:Title]]`, `[[public/Namespace:Title]]` | namespace-qualified, then partition-qualified page |
| `[[roam://ID\|label]]` | exact page, written by completion; survives moves and renames |
| `[Title](roam://ID)` | page link written by the reference popup |
| `[tag](roam://ID#tag)` | a tag (or `\tag{}` label, or block) on a page |
| `[Heading](roam://ID@above@heading)` | a heading, by the slugs of the headings above it |
| `[text](relative/path.md)`, `![alt](img.png)` | file link / image; `<…>` or spaces allowed in local paths |
| `[](page.html)` | embed a local HTML file |
| `[label](notebook.ipynb#cell)` | link into a notebook |
| `[text](#heading)` | a heading or tag of the current page |

A colon separates a namespace only when a page answers to that reading, so
`[[Chapter 1: Scope]]` still links a title.  Link text is produced by the Go
kernel's reference lookup, never assembled by hand.  Unlinked mentions of
another page's title are found as virtual references, never stored
(`shared/virtual-references.mjs`).  Parsers: `shared/wiki-link.mjs`,
`shared/note-refs.mjs`.  Spec: [wiki-workspace.md](wiki-workspace.md) “Links
and identity”, “Namespaces”, “Referring to something”.

In the editor, `[[` and `roam://` open the reference popup; inside it `@text`
finds headings, `Page#text` / `Page@text` a page's tags or headings, `@@text`
citation keys and `/text` files.  The search box's own query language
(`intitle:`, `tag:`, `ns:`, `after:2026-10`, …) is
[wiki-workspace.md](wiki-workspace.md) “Query syntax”
(`shared/knowledge-query.mjs`, `kernel/noema/wikisearch/query.go`).

## Math

| Form | Meaning |
| --- | --- |
| `\( … \)` | inline math |
| `\[ … \]` on their own lines | display math |
| `\tag{label}` in display math | equation label: jump target and reference target |
| `\begin{align} … \end{align}` and other environments inside display math | KaTeX environments |

Macros come from four layers, later winning: `resources/katex-macros/*.tex`
(shared with the Emacs preview through `etc/`), `~/.config/noema/katex-macros.tex`,
the note's `macros-from:` files, and its `#+begin macros` block.  TeX
compatibility rules are `shared/tex-compat-rules.json`; the macro contract is
`shared/katex-macro-fixtures.json`.  Spec:
[../../design/latex-preview.md](../../design/latex-preview.md).

## Diagrams and figures

| Form | Meaning |
| --- | --- |
| ` ```mermaid ` | Mermaid diagram |
| ` ```marmind ` (also `mindmap`, `markmind`) | interactive mind map: indentation is the hierarchy, `{…}` sets size and placement |
| `#+begin tikz` | TikZ figure, rendered in the browser and kept verbatim in LaTeX |
| `#+begin layout {…}` | Markdown blocks or figures side by side; `mode=flow` for continuous prose |

Supported fence languages: `supportedDiagramLang` in `src/diagram-langs.ts`.

## Slides

A note with `kind: slides` is also a Reveal deck.  Each `#` heading starts a
horizontal slide; a `##` heading under it starts a vertical slide in that
stack.  `@@slides(reveal) []` and `@@slides(vertical) []` are structural
markers, hidden in the editor.  Parser: `pages/slide-presentation.ts`;
example: `examples/slides-demo.md`.

## `.noema` work blocks

The leading region of a work block's prompt holds at most one directive per
line, then the prompt:

```text
@@agent(codex)
@@session(continue)
@@ctx(lineage)
@@skill(proof-review)
@@pack(math-verification)
@@todo(todo) [Check the lemma] {ddl: tomorrow}

Check the disputed lemma.
```

| Directive | Values |
| --- | --- |
| `@@agent(id)` | agent id |
| `@@session(…)` | `continue`, `fork`, `fresh`, a project session name, or `parent:child` |
| `@@ctx(…)` | `none`, `lineage`, `lineage:1`–`3`, `depends`, `git.diff`, `handoff.latest`, `cell:ID`, `result:wn_…`, `file:PATH`, `note:ID`, `artifact:art_…` |
| `@@skill(id)`, `@@pack(id)` | Skill or pack selected for this Run |
| `@@todo`, `@@clock` | the block's own agenda item; one leading `@@todo`, clocks after it; keys `WORK_AGENDA_KEYS` in `shared/work-agenda.mjs` |

There is no `@@mcp` and no patch syntax.  Parser:
`server/lib/research-directives.mjs`.  Spec:
[capabilities.md](capabilities.md) “Work-cell syntax”; document model:
`AGENTS.md` D-023.
