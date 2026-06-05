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
| `src/cm6/editor-cm6.ts` | CM6 `EditorView` construction and public editor methods. |
| `src/cm6/live-preview.ts` | Inline Markdown preview decorations and line classes. |
| `src/cm6/commands.ts` | Editing commands, block context, and quick insert registry. |
| `src/cm6/widgets/*.ts` | Math, code fence, image, task, TOC, org-env, Lean placeholders, and related widgets. |
| `src/cm6/widgets/lean-placeholder.ts` | Embedded `@@lean4(selector) [tag]` child editor, Lean LSP region mapping, Lean-local Vim/jump, and Copilot auxiliary editor registration. |
| `shared/lean-placeholder.mjs` | Shared placeholder syntax: `parseLeanPlaceholderLine`, `formatLeanPlaceholder`, `canonicalLeanSelector`, `scanMarkdownLeanPlaceholders`. Consumed by both the client widget and server-side region/mirror helpers. |
| `src/render-html.ts` | Shared Markdown-to-HTML export/publish renderer. |
| `src/attrs-syntax.ts` | Shared `{key: value}` trailing-attribute block parser used by command-syntax and image-attrs. |
| `src/layout-attrs.ts` | Layout-attribute normalization (align, wrap, width, height) and CSS-class/style helpers. |
| `src/image-attrs.ts` | Image-specific layout attr reader and DOM/token applicators, built on `layout-attrs.ts`. |
| `src/command-syntax.ts` | Inline `@@cmd` and block `#+begin kind` command parser, now delegates to `attrs-syntax.ts`. |
| `src/styles/*.css` | CM6 editor chrome and swappable Markdown themes. |
| `aaronnote/main.ts` | Desktop app shell: notes UI, command palette, ranger tabs, jump stack, panel orchestration, plugin boot. |
| `aaronnote/filesystem.ts` | Notes Filesystem/Recent ranger rendering and keyboard navigation. |
| `aaronnote/lean-panel.ts` | Left Lean panel: Infoview/messages, bottom-pinned outline, restart/stop/cache controls. |
| `server/lib/lean*.mjs` | Lean request dispatcher, mirror path helpers, tagged-region parsing, and LSP process support. |
| `server/lib/runtime.mjs` | Server-side note/index/save/runtime implementation, including Lean file language ids for Copilot. |
| `plugin/copilot/index.ts` | Copilot inline UI and key handling for the main editor plus auxiliary editors such as Lean child editors. |

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
6. Embedded Lean uses `@@lean4(selector) [tag]` placeholders. Omitting the
   selector defaults to the note's mirror file. The canonical Lean project root
   is `<notesRoot>/.lean/`; do not recreate a duplicate `<notesRoot>/.lake/`.
   Placeholder identity is `(selector, tag)`; both sides use `canonicalLeanSelector`
   from `shared/lean-placeholder.mjs` as the single source of truth.
7. Lean child-editor polish must reuse existing LSP/editor state where possible:
   diagnostics, progress, completion kind icons, Copilot, and jump overlays
   should not add polling or full-file scans on input. The server-side
   `getRegionNeighbors` (full-file scan for insertion ordering) must only be
   called when a region does not yet exist — use `readOrEnsureLeanRegionFromRequest`
   as the entry point so existing regions short-circuit without scanning.
   Widget height re-measurement on window resize is handled by `MeasuredWidget`'s
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
