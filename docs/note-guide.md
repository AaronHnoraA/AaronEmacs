# Typst Note Guide

这套 note 系统是 Typst-first 的知识库试验层，不复用 `org-roam` 数据库，也不做 Org
自动转换。

## 1. 定位

- `.typ` 是新的长期笔记格式。
- `org` 继续保留 agenda、capture、Babel/Jupyter、旧站点发布等职责。
- Typst note 只负责节点、标签、跨笔记链接、反链和增量索引。

## 2. 文件和模块

- 配置模块：`lisp/note/init-note.el`
- 模板目录：`templates/typst/`
- 索引数据库：`var/note/note.db`
- 默认扫描根：`~/HC/Org/`
- 默认新建目录：`~/HC/Org/roam/`

## 3. Typst note 格式

每个 note 文件顶部使用 Typst 原生 metadata：

```typst
#metadata((
  kind: "note",
  id: "20260511T120000-example",
  title: "Example",
  date: "2026-05-11",
  tags: ("math", "draft"),
  aliases: (),
)) <note>
```

跨笔记链接使用：

```typst
#note("20260511T120000-example")[Example]
```

`M-x my/note-db-sync` 会重建 note 索引，并更新 `my/note-root/_typst/note.typ`
里的共享 helper。渲染时 `#note(...)` 是带颜色和下划线的普通文本，不生成 Typst
`link(...)`，避免 Tinymist preview 把跨 note 跳转解释成浏览器内 URL。
从 preview 点击回源码时，如果落点在 `#note(...)` 源码区域，Emacs 再打开对应 note。

note helper 使用 root-relative import：

```typst
#import "/_typst/note.typ": *
#show: note-entry
```

这里的 `/` 是 Typst project root，不是系统根目录。preview/compile 时会把
`my/note-root` 作为 Tinymist/Typst root，因此 note 文件可以放在任意子目录，
不需要写脆弱的 `../_typst/note.typ`。

`note-entry` 会注入暖灰页面背景和文档顶部目录，并从 Emacs 配置里的 `my/font-body`、
`my/font-cn`、`my/font-code`、`my/font-title` 和
`my/typst-preview-math-font` 生成 Typst 正文、中文、代码、标题和数学字体设置。
`note-theme` 只负责基础样式，供 include 时复用，避免子文件重复生成目录。
helper 也提供 `definition`、`theorem`、`proof`、`question`、`summary`、
`important`、`warning`、`tip`、`info` 等轻量卡片块，风格对齐 Org special block
和 Previewer 的 Org CSS。
预览/编译时会在文档顶部插入 `outline(title: [目录], depth: 2)`，方便 Tinymist
preview 里快速导航。

文内章节、定理和公式引用继续用 Typst 自己的 `<label>` / `@label`。

## 4. 多文件组合

`M-x my/note-db-sync` 会把 roam id 写进 `_typst/note.typ` 的路径表。可用 helper：

```typst
#let linked = note-import-path("note-linked")
#import linked: cross-file-sum-equation

#note-include("note-linked")
```

`note-import-path(id)` 返回 root-relative Typst 路径，适合配合 `#import` 引入某个
note 暴露出来的 `#let` 内容。`note-include(id)` / `note-transclude(id)` 会整篇包含
对应 note，并在 include 期间标记子文件为非入口文档，所以子文件自己的入口目录不会重复
出现在当前 PDF。整篇 include 仍会带入目标文件的 heading / metadata，适合草稿组合；
可复用公式、定义等更推荐在目标 note 中写 `#let name = [...]`，再用
`#import note-import-path("id"): name`。

## 5. 命令

在 Typst buffer 里：

- `C-c C-p`
  用 macOS 系统浏览器打开 Tinymist 官方 preview；同一文件会复用现有进程保留增量更新，切到另一个 Typst buffer 后再次执行会杀掉旧 preview 并预览当前文件。preview 已经打开时，切换到另一个 Typst buffer 会自动跟随。
- `C-c C-j`
  把 preview 滚动到当前源码位置。
- `C-c n n`
  新建 Typst note。
- `C-c n f`
  查找 note。
- `C-c n i`
  插入 `#note("id")[title]` 链接。
- `C-c n l`
  查看当前 note 的反链。
- `C-c n s`
  重建 note 索引。

## 6. Snippet（yasnippet）

Typst buffer 使用 `snippets/typst-ts-mode/` 下的 yasnippet。`typst-mode/` 是同
目录的软链，所以 `typst-mode` / `typst-ts-mode` / `my/typst-mode` 都能用同一
份模板。

这套 snippet 是把 `org-mode/` 与 `tex-mode/` 里的 LaTeX 写作快捷键整体翻译成
Typst 等价语法，触发关键词与 org 保持一致，便于直接迁移肌肉记忆：

- 数学符号 / 希腊字母：`aaaa` → `alpha`，`RR` → `RR`，`frac` →
  `frac(_, _)`，`sum` / `prod` / `int` / `lim` 等。
- 加重 / 装饰：`bar` `hat` `tilde` `dot` `ddot` `vec` 分别对应 `overline()`、
  `hat()`、`tilde()`、`dot()`、`dot.double()`、`arrow()`。
- 矩阵：`mat` `pmat` `bmat` `Bmat` `vmat` `Vmat` 通过 `mat(delim: ...)` 生成；
  `cas` 生成 `cases(...)`；`iden` 生成单位阵。
- 分节：`sec` / `sub` / `subs` / `par` / `subp` 直接展开成 `=` ~ `=====`；
  `*l` 变体附带 `<sec:slug>` 等 label。
- 引用：`ref` / `cite` / `figure:ref` / `section:ref` / `table:ref` /
  `algo:ref` / `listing:ref` 都展开为 Typst `@label` 形式。
- 图表：`figure` / `figure:acm` / `table` / `table:acm` 用 `#figure(...)` +
  `image()` / `table()` 包装，并自带 `<fig:_>` / `<tab:_>` label。
- 块（theorem / lemma / proof / definition / remark / note / tip / info /
  warning / important / question / solution / example / problem / ...）展开为
  `#theorem[...]` 等函数调用风格，假设用户在 setup 里有 `#let theorem = ...`
  之类的辅助函数；如果还没定义，把它们当作普通占位 placeholder 替换即可。
- note 卡片块已有短触发：`def`、`thm`、`lem`、`cor`、`prop`、`proof`、
  `que`、`summ`、`imp`、`warn`、`tip`、`info`、`rem`、`ex`、`sol`、`ncard`；
  `nlink` 展开为跨 note 引用 `#note("id")[title]`。
- 量子（`ket` / `bra` / `bk` / `me` / `dyad` / `outer` / `expval` / `comm` /
  `anticomm` / `bell` / `proj` / `qft` / `sch` / ...）用便携的
  `lr(|psi angle.r)` 形式书写，不依赖 `physica` 包。
- 密码学 / 复杂性（`adv` `game` `negl` `PRF` `PRG` `Enc` `Dec` `Sign`
  `Vrfy` `MAC` `KDF` `INDCPA` `EUFCMA` `BPP` `BQP` `bigO` `bigOmega`
  `bigTheta` `tildeO` ...）展开为对应 Typst 数学表达式。

完整列表见 `snippets/typst-ts-mode/`；触发器与 `snippets/org-mode/` /
`snippets/tex-mode/` 同名，只有 body 改成 Typst 写法。如果关键词在原 org / tex
集合里就有冲突（如 `par` 既是 paragraph 又是 partial derivative），这里也保留
双份，让 yasnippet 走选单。

## 7. 明确不做

- 不转换 Org 文件。
- 不实现 Babel 等价代码执行。
- 不替换 org-roam / org-agenda / org-publish。
- 不直接写 org-roam SQLite 数据库。
