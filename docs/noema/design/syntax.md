# Noema source syntax

Every syntax Noema adds on top of CommonMark/GFM, in one table per family.
Each row names the form, the module that parses it and the document that
specifies it in detail; this page is the map, not the full grammar.  When a
row and its owner disagree, the owner (code plus fixtures) is right and this
page is stale.

Rules that hold for every family:

- A Markdown note is plain text and stays readable in any other editor; a
  rendered view never invents a form the source cannot hold.
- A grammar the browser, the Node host and the Go kernel all read has one
  parser in `shared/*.mjs`.  `src/` imports it through a typed facade, and a Go
  port is pinned to it by a fixture file in `shared/*-fixtures.json`.
- A command written inside a code span or fence is code, not a command.
  `\@@` escapes a command start.
- A new syntax takes a row here in the same change that adds its parser.

Paths are relative to the Noema repository `site-lisp/noema/`.

## Markdown extensions

| Form | Meaning | Owner |
| --- | --- | --- |
| `==text==`, `^sup^`, `~sub~` | highlight, superscript, subscript | `src/render-html.ts`, editor inline rules |
| `[toc]` | table of contents | `src/render-html.ts`, `src/cm6/toc-index.ts` |
| `> [!note]` … | callout (`tip`, `warning`, …) | `src/render-html.ts` |
| `[^id]` / `[^id]: text` | footnote reference / definition | `src/render-html.ts` |
| `\( … \)`, `\[ … \]` | inline / display math (KaTeX) | `src/inline-math.ts`, `src/math-render.ts` |
| `\tag{label}` in display math | equation label, jump and reference target | `src/equation-tags.ts` |
| ` ```mermaid `, ` ```marmind ` | diagram, interactive mind map | `src/diagram-langs.ts`, `src/diagram-render.ts` |

## Note metadata

Only a `meta` block in the opening lines of a note
(`ORG_META_PREAMBLE_LINE_LIMIT` lines) is metadata; later ones are examples.

```text
#+begin meta
id: <stable-id>
title: Title
tags: [a, b]
macros-from: notation.sty
#+begin summary Abstract
Summary text.
#+end summary
#+begin macros
\newcommand{\Mon}{\operatorname{Mon}}
#+end macros
#+end meta
```

| Part | Owner | Spec |
| --- | --- | --- |
| `key: value` fields, `summary` | `src/org-meta.ts`, `shared/meta-summary.mjs` | [current-architecture.md](current-architecture.md) |
| `macros-from:`, `#+begin macros` | `shared/note-macros.mjs` | [../../design/latex-preview.md](../../design/latex-preview.md) |
| A leading `---` YAML front matter | `src/simple-frontmatter.ts` | read only, for imported notes |

## Block environments

`#+begin KIND [title] [{#id key=value}]` … `#+end KIND`, paired by kind.

| Kind | Meaning | Owner |
| --- | --- | --- |
| `meta`, `summary`, `macros` | note metadata (above) | `src/org-meta.ts` |
| `comment`, `fold` | hidden comment, folded region | `block-extras.ts` |
| `html` | sanitized HTML | `src/sanitize-html.ts` |
| `tikz` | TikZ/PGFPlots figure | `shared/tikz-source.mjs` |
| `layout` | figure layout group | `shared/layout-attrs.mjs`, `src/layout-group.ts` |
| `av` | attribute view (database table) | `shared/attribute-view.mjs` |
| `embed` | query embed: `sql:` body or `:sql` title | `shared/embed-query.mjs` |
| `lean4` | Lean source carrier | `shared/lean-placeholder.mjs` |
| any other (`theorem`, `proof`, `note`, `answer`, …) | titled environment, exported as a LaTeX environment | `src/render-html.ts`, [latex-export-style.md](latex-export-style.md) |

`block-extras.ts` is `src/cm6/extensions/visual/widgets/block-extras.ts`.
Which kinds count as prose for spelling and diagnostics is
`TECHNICAL_ORG_ENV_KINDS` in `shared/prose-mask.mjs`.

## Block identity and properties

| Form | Meaning | Owner |
| --- | --- | --- |
| `{#UUIDv7 key=value}` at a block's end | portable block id and properties | `shared/block-identity.mjs`, `shared/block-properties.mjs`, fixtures `block-property-fixtures.json` |
| `{key: value, …}` after a figure or block | layout attributes (`wrap`, `width`, …) | `shared/layout-attrs.mjs`, Go port pinned by `latex-transform-fixtures.json` |

Environments that cannot carry an id are `NON_SEMANTIC_ORG_ENVS` in
`shared/block-identity.mjs`.

## Commands

One grammar, `shared/command-syntax.mjs`:

```text
@@name(switch) [body] {key: value; key: value}
@@name(switch) [body] {
  key: value
}
```

Attributes split on `;`, or on `,` before another `key:`; `=` works for `:`.
Rendering is `src/cm6/extensions/visual/widgets/inline-commands.ts`.

| Command | Meaning | Spec |
| --- | --- | --- |
| `@@todo`, `@@itodo`, `@@project`, `@@milestone`, `@@clock` | planning items | [agenda-dsl.md](agenda-dsl.md); parser `shared/planning-dsl.mjs`, fixtures `planning-*-fixtures.json` |
| `@@tag[name]` | tag | `inline-commands.ts` |
| `@@cite(style) [key] {locator: …}` | citation | `shared/bibliography-syntax.mjs`, fixtures `bibliography-fixtures.json` |
| `@@revision(kind) [original] {advice: "…"; reason: "…"}` | review suggestion; kinds in `shared/revision-kinds.mjs` | `src/authoring-syntax.ts` |
| `@@comment`, `@@scomment` | inline comment, side comment | `inline-commands.ts` |
| `@@part [title]`, `@@section(level) [title] {id: …}` | semantic headings for outline and export; levels in `SWITCH_LEVELS` | `src/semantic-outline.ts`, [latex-export-style.md](latex-export-style.md) |
| `@@latexmk(mark)` | LaTeX layout mark; names in `shared/latex-marks.mjs` | [latex-export-style.md](latex-export-style.md) |
| `@@slides(reveal)` | Reveal deck marker | `inline-commands.ts` |
| `@@cell(lang) [id]` | executable cell in a Markdown note (sidecar) | `src/cm6/extensions/visual/widgets/ceil-shared.ts`, [../../guide/research-notes-workflow.md](../../guide/research-notes-workflow.md) |
| `@@note-code(path) [id]` | embedded project code | `shared/note-code.mjs`, [rendered-constructs.md](rendered-constructs.md) |
| `@@lean4(selector) [tag]` | Lean placeholder | `shared/lean-placeholder.mjs` |

## Links and references

| Form | Meaning | Owner |
| --- | --- | --- |
| `[[Title]]`, `[[Title\|label]]`, `[[ns:Title]]` | wiki link by title or namespace | `shared/wiki-link.mjs` |
| `[[roam://<id>#<frag>\|label]]` | stable link written by the reference popup | `shared/wiki-link.mjs`, `shared/note-refs.mjs` |
| `[text](file.md)`, `![alt](img.png)` | ordinary file link / image | `src/markdown-link.ts`, `src/image-attrs.ts` |
| unlinked keyword of another page | virtual reference (suggested, not stored) | `shared/virtual-references.mjs` |
| `after:`, `before:` filters | knowledge search query | `shared/knowledge-query.mjs` |

Reference text is produced by the Go kernel, never assembled by a client:
[wiki-workspace.md](wiki-workspace.md).

## `.noema` work blocks

A `.noema` file is nbformat 4.5 holding work blocks, edited in JuText.  Only
the leading region of a work block's prompt is control text:

```text
@@agent(codex)
@@session(continue)
@@ctx(lineage)
@@skill(id)
@@pack(id)
@@todo(todo) [Task] {ddl: tomorrow}
```

Later lookalikes, in prose or outputs, are data.  The directive grammar,
`@@ctx` forms and Skill/pack selection are specified in
[capabilities.md](capabilities.md) ("Work-cell syntax"); agenda keys on a work
block are `WORK_AGENDA_KEYS` in `shared/work-agenda.mjs`.  The document model
is D-023 in `AGENTS.md`.
