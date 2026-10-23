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
| `*.noema` | AI work document: nbformat 4.5 of work blocks, edited in JuText; never a Jupyter notebook | `AGENTS.md` D-023 |
| `*.prompt` | standalone prompt with the same leading directives, plus `@@workstream(ws_…)` and legacy single-`@` spellings | `server/lib/research-directives.mjs` |
| `*.ipynb` | ordinary notebook, full Jupyter support | [../../design/jupyter-workflow.md](../../design/jupyter-workflow.md) |
| `.cell/NOTE.LANGUAGE.SESSION.ipynb` | sidecar holding a note's `@@cell` sources and outputs; `LANGUAGE` is the notebook language, not the kernelspec | jupyter-workflow.md |
| `noema.toml` | Wiki repository manifest and/or research Project (below) | `AGENTS.md` D-038 |
| `.agent/` | a Project's state (`state.sqlite`), ignores itself | `server/lib/research-project.mjs` |
| `*.typ` | Typst file: editable, not a Wiki page | [../../guide/research-notes-workflow.md](../../guide/research-notes-workflow.md) |
| images, PDFs, `*.html` | attachments: only their metadata is indexed | wiki-workspace.md |
| `~/.config/noema/katex-macros.tex` | the user's global math macros | [../../design/latex-preview.md](../../design/latex-preview.md) |

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
| `date` | date shown on the cover and exported | cover, export |
| `tags` | tags (also inline `@@tag[…]`) | index |
| `aliases` | other titles the page answers to | index, links |
| `refs` | pages this note references besides the links in its body | index (backlinks) |
| `kind` (`kinds`) | page kind: `default`, `slides`, … | index, slides |
| `namespace` | overrides the repository namespace | index, links |
| `project` (`proj`) | planning project key | agenda |
| `source` | where the content came from; also a key the note is found by | index, lookups |
| `summary` | one-line summary (or the `summary` block) | index, cover |
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
| `## Heading {#slug}` | explicit heading anchor |
| `## Heading <!-- omit from toc -->` | heading left out of the TOC and the slide list |
| `1)` and `(a)` list markers | ordered lists with matching labels in export |
| `\` at line end | hard line break |
| safe inline HTML (`<kbd>`) | sanitized by `src/sanitize-html.ts`; scripts, iframes and objects are removed |

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
| `layout {cols=3 mode=grid widths=30-40-30 align=center}` | side-by-side group; `cols` 1–4, `mode` `grid`/`flow` (`src/layout-group.ts`) |
| `tikz [id]` | TikZ/PGFPlots figure, kept verbatim in LaTeX (`shared/tikz-source.mjs`) |
| `html` | sanitized raw HTML block |
| `av [title]` | attribute view (below) |
| `embed [title]` | query embed (below) |
| `lean4` | Lean source carrier, not rendered as an environment |
| any other word | generic titled box with that label |

Which kinds are prose for spelling and diagnostics:
`TECHNICAL_ORG_ENV_KINDS` in `shared/prose-mask.mjs`.  Which kinds can carry
a block id: everything except `NON_SEMANTIC_ORG_ENVS` in
`shared/block-identity.mjs`.

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
| `#+begin layout {…}` | figures side by side |

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
