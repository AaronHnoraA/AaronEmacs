# Aaronnote

A Typora-style Markdown editor built on CodeMirror 6.

## State Model

Markdown source is the runtime document. The live editor authority is the CM6
`EditorState`: document text, selection, compartments, extensions, history, and
decorations. Rendered preview behavior is implemented with Lezer Markdown syntax
trees plus CM6 decorations/widgets; it must not depend on a parallel document
model.

Source mode and preview mode are both CM6 surfaces. `editor.toggleSource()`
switches the live-preview compartments on and off instead of swapping to a
separate editor implementation.

## Core Files

| File | Responsibility |
|---|---|
| `src/lib.ts` | Public library API. |
| `src/editor-api.ts` | Stable `createEditor()` facade and controller types. |
| `src/cm6/editor-cm6.ts` | CM6 `EditorView` construction and public editor methods. `getMarkdown()` is memoized by immutable-`Text` identity; prefer `getMarkdownLength()` when only length is needed. |
| `src/cm6/live-preview.ts` | Inline Markdown preview decorations and line classes. |
| `src/cm6/close-brackets-vscode.ts` | VSCode-style bracket pairing, selection wrapping, overtyping, and paired deletion. |
| `src/cm6/lezer-link-ext.ts` | Lezer `LinkEnd` replacement that preserves nested brackets inside inline link/image text. |
| `src/cm6/commands.ts` | Editing commands, block context, and quick insert registry. |
| `src/cm6/widgets/*.ts` | Math, code fence, image, task, TOC, org-env, and related widgets. |
| `src/render-html.ts` | Shared Markdown-to-HTML export/publish renderer. |
| `src/math-render.ts` | KaTeX render + HTML cache. Cache key includes the active macro-set version. |
| `src/katex-macros.ts` | Global KaTeX macro state (`setKatexMacros`/`getKatexMacros`/`getKatexMacrosVersion`); re-exports the parser. |
| `shared/katex-macros.mjs` | Browser-safe `\newcommand`/`\DeclareMathOperator`/`\def` → KaTeX macros parser. |
| `server/lib/katex-macros.mjs` | Node loader: reads `*.tex` from a folder and parses via the shared parser. |
| `src/attrs-syntax.ts` | Shared `{key: value}` trailing-attribute block parser used by command-syntax and image-attrs. |
| `src/layout-attrs.ts` | Layout-attribute normalization (align, wrap, width, height) and CSS-class/style helpers. |
| `src/image-attrs.ts` | Image-specific layout attr reader/writer and DOM/token applicators, built on `layout-attrs.ts`. `imageLayoutToTrailingAttrs` serializes a layout back to `{...}` source (round-trips through `imageLayoutFromAttrs`); used by the image widget's hover toolbar. |
| `src/command-syntax.ts` | Inline `@@cmd` and block `#+begin kind` command parser, now delegates to `attrs-syntax.ts`. |
| `src/styles/*.css` | CM6 editor chrome and swappable Markdown themes. |
| `aaronnote/main.ts` | Emacs-embedded app shell: notes UI, command palette, jump stack. |
| `aaronnote/latex-export-scope.ts` | Pure whole-note/selection/heading-subtree range model used by the LaTeX scope picker. |
| `server/lib/runtime.mjs` | Server-side note/index/save/runtime; Copilot LSP bridge. |
| `server/lib/latex-export.mjs` | Mechanical Markdown-to-LaTeX base conversion (`mechanicalConvert`/`aaronnoteMarkdownToLatex`), template rendering, validation, atomic `.tex` writes. Merges agent rules via `options.rules`. |
| `server/lib/latex-export-codex.mjs` | Codex polish of the mechanical draft: staged workdir, compile-verify retry loop, prose-fidelity warnings, agent-rule loading. Falls back to the draft. See `docs/latex-export-style.md`. |
| `agents/latex-export/` | Codex export contract (`AGENTS.md`), the agent-maintained `mechanical/rules.json` (envMap/commentBlocks merged into the base converter), and `notes.md`. Edited only on a maintenance pass, never during a normal export. |
| `server/lib/watch.mjs` | Recursive fs watcher for vault freshness; SSE broadcast on batch change. |
| `server/lib/tmp.mjs` | Runtime temp staging (`mkdtemp`, atomic writes, TTL orphan sweep). |
| `server/lib/copilot.mjs` | Re-export barrel for Copilot LSP bridge (uses Emacs-managed binary). |
| `web-host.mjs` | Node HTTP server: API handlers, `/graph` route, static serving, Emacs event bridge. |
| `src/cm6/heading-fold.ts` | Heading fold service + hover-only chevron widget; reuses `tocIndexField`. |
| `src/cm6/ordered-list-renumber.ts` | Auto-renumber ordered lists; bounded `ensureSyntaxTree`; single-undo transaction. |
| `src/cm6/toc-index.ts` | Incremental TOC / heading index state field; used by outline and fold. |
| `src/copilot/index.ts` | Built-in Copilot inline UI and key handling for the main editor. |

## Emacs handoff

This editor is embedded in Emacs via xwidget/Appine. Panels and subsystems that
were part of the original standalone Electron app are now delegated to native
Emacs equivalents:

| Removed subsystem | Emacs equivalent |
|---|---|
| Git panel (commit/diff/pull/push) | `magit` |
| Agenda / todos panel | `my/typst-roam-todos` |
| Filesystem browser ranger | `dired`, roam selector |
| Lean interactive editor (placeholders, infoview, child editors) | `lang/lean/` (Emacs LSP) |
| Jupyter panel | Org Babel / Jupytext in Emacs |
| In-editor roam graph | `my/aaronnote-roam-graph` → `/graph` standalone route |
| Plugin runtime + roamlookup | removed; Copilot is a built-in |

`lean` and `#+begin lean4` code blocks render as **static syntax-highlighted
snippets** in the web editor (no LSP process started from the browser).

## Widget Rules

All CM6 widgets that contribute vertical height must extend `MeasuredWidget`
(`src/cm6/widgets/measured-widget.ts`) instead of bare `WidgetType`.
Call `this.registerMeasured(dom, view)` at every `toDOM()` return point.

```typescript
class MyWidget extends MeasuredWidget {
  protected measureKey() { return "my:" + this.stableId; }
  toDOM(view: EditorView): HTMLElement {
    const el = document.createElement("div");
    // … build DOM …
    return this.registerMeasured(el, view);
  }
}
```

- No vertical `margin` on the widget root — CM6 measures border-box only; root
  vertical margins are invisible to the height map and cause cursor drift.
  Use root `padding` or child layout for vertical spacing instead.
- Override `measureGroupKey()` and `estimatedHeightFallback()` when scroll
  estimates matter; a fallback near the eventual height beats CM6's 1-line default.
- For widgets that support `layout.wrap` (CSS float): see the "Float-wrap
  coexistence" section in `docs/maintenance.md` — Pattern A for inline-replace
  widgets, Pattern B for `block:true` widgets.

## KaTeX macros

Custom KaTeX macros are defined as `.tex` files (LaTeX `\newcommand` syntax) in
`etc/katex-macros/` in the Emacs config and apply **globally** to every note.
Flow: the Node server reads the folder (env `AARONNOTE_KATEX_MACROS_DIR`, wired in
`lisp/roam/init-aaronnote.el`) via `server/lib/katex-macros.mjs`; the browser
fetches them through `api.config.katexMacros()` (channel
`aaronnote:api:config:katex-macros`) and installs them with `setKatexMacros`
before the first note renders; `scripts/render-html.mjs` does the same for
export/publish. `renderMathHTML` reads the active map on every call and folds the
macro-set version into its cache key. See `etc/katex-macros/README.md`.

## Invariants

1. Markdown source offsets are the stable cross-system coordinate space.
2. Public API methods should mutate the CM6 document with transactions whenever
   possible, preserving selection and history.
3. Preview widgets are views over source text. They must map clicks/commands back
   to source ranges rather than storing independent state.
4. Shared behavior belongs in `src/`; app shell code under `aaronnote/` should use
   the public editor facade instead of reaching into widget internals.
5. Styles should target `.cm-editor` and CM6/widget classes. Do not add legacy
   editor compatibility selectors.
6. Widget height re-measurement on window resize is handled by `MeasuredWidget`'s
   `ResizeObserver`; widgets must not add their own `window.resize` listeners.

## Testing

Use focused tests first:

```sh
npm test -- tests/editor-api.test.ts tests/cm6/roundtrip.test.ts tests/cm6/commands.test.ts
```

For broader changes, run the full suite from `Aaronnote/`:

```sh
npm test
```
