# Lean 4 Integration

Aaronnote embeds Lean 4 through markdown placeholders that point into the
note's mirrored Lean file. Markdown stays compact and readable, while Lean code
lives in real `.lean` files inside the notes root's `.lean/` Lake project.

## Realtime Status

- [x] Product direction locked: use `@@lean4 [tag]`, not `#+begin lean4`.
- [x] Storage direction locked: one Markdown note maps to one mirror `.lean` file.
- [x] Project layout locked: the Lake project and cache live under
  `<notesRoot>/.lean/`; the notes root must not keep a duplicate `.lake/`.
- [x] Region direction locked: a Lean region starts at `-- @aaronnote <tag>` and ends at the next Aaronnote tag or EOF.
- [x] Region parser and mirror-file helpers implemented.
- [x] Server IPC added for ensure/read/update tagged Lean regions.
- [x] CM6 placeholder widget renders embedded Lean editors as isolated block widgets.
- [x] Insert command and `Cmd/Ctrl+Shift+L` shortcut added.
- [x] Old org-env Lean LSP sync path disabled for Markdown notes.
- [x] LSP diagnostics/goals/hover/completion/semantic tokens remapped from full Lean file positions into embedded regions.
- [x] Lean input abbreviations imported from `lean4-mode` and scoped to the embedded Lean editor.
- [x] Embedded editor syntax highlighting uses Lean Tree-sitter/WASM via `@arborium/lean`, with Lean LSP semantic tokens layered on top when available.
- [x] Embedded editor has Lean-local Tab/Shift-Tab indentation and a child-editor Vim layer; outer Markdown Vim/ranger bindings do not receive those keys.
- [x] Lean LSP diagnostics render both inline underlines and lightweight
  child-editor gutter markers; Lean progress also uses the same gutter channel.
- [x] Lean LSP completion rows show item-kind icons from LSP metadata, with no
  extra completion or symbol requests.
- [x] `roam/Makefile` provides Lean project operations: `make update`, `make cache`, `make build`, `make clean`, and `make info`.
- [x] Notes page includes a Lean tab for project/toolchain/package info and manual project commands.
- [x] Left Lean panel uses a scrollable Infoview/messages area and a pinned
  bottom outline area with per-note runtime width and outline-height memory.
- [x] Embedded editor keyboard events are isolated from outer Markdown/Vim handling.
- [x] Lean panel restart/stop controls work with active `@@lean4 [tag]` regions.
- [x] Lean panel includes a collapsible, height-resizable outline backed by
  Lean LSP `textDocument/documentSymbol`; the outline is outside the Infoview
  scroll flow so changing goals do not move it.
- [x] Embedded Lean editors participate in the app-wide source/preview mapping,
  so preview jumps account for child-editor heights instead of using raw line
  offsets.
- [x] `Ctrl+Enter` works from an embedded Lean editor to open/close the Notes
  ranger; opening Filesystem or Recent hides the Lean side panel first.
- [x] Notes ranger can switch between Filesystem and Recent with `Tab`; Recent
  supports arrow keys, `h/j/k/l`, `Home`, `End`, and `Enter`.
- [x] Embedded Lean editors register as auxiliary Copilot editors, and standalone
  `.lean` files use the Lean language id for Copilot requests.
- [x] Publish/PDF export renders `@@lean4 [tag]` placeholders as static Lean 4
  code cells by reading the mirror `.lean` region data, without starting LSP.
- [x] Parser/region unit tests added.
- [x] Verified with `npm run build:aaronnote` and
  `npm test -- copilot-plugin`.
- [x] Multi-selector support: `@@lean4(selector) [tag]` links a placeholder to an
  alternate Lean file (`newfile:N` mirror or a relative path link). The selector
  is optional; omitting it keeps the default mirror behavior.
- [x] Shared placeholder module (`shared/lean-placeholder.mjs`) extracted:
  `parseLeanPlaceholderLine`, `formatLeanPlaceholder`, `canonicalLeanSelector`,
  and `scanMarkdownLeanPlaceholders` are now the single source of truth for both
  the client widget and all server-side region/mirror helpers.
- [x] Lean block manager modal (`Insert Lean Block`): replaces bare `insertLeanBlock`
  for interactive use. Shows target mode (default/mirror/link), mirror number with
  existence hint, and tag selection with duplicate warning.
- [x] LSP document defer-close (`AARONNOTE_LEAN_DOCUMENT_IDLE_MS`, default 90 s):
  switching between lean blocks no longer tears down and reopens the LSP document
  immediately, saving ~500 ms per block switch.
- [x] Incremental placeholder index: `patchLeanPlaceholderIndex` rescans only
  changed lines on each keystroke, replacing a full-document scan.
- [x] Read-only Lean LSP navigation: definition, declaration, type definition,
  implementation, references, and hover are available from Vim keys, the
  command palette, and the embedded editor's native context menu.
- [x] Common Lean edit tools: line/block comment toggle, duplicate, move, join,
  trim trailing whitespace, indent, and outdent are region-local and undoable.
- [x] Navigation results route to embedded regions when possible. Multi-result
  queries use a searchable picker; non-embedded source opens in Kitty/Nvim.

Update this section whenever an implementation stage lands.

## User Model

Markdown contains only a Lean placeholder:

```markdown
The following Lean fragment proves the local claim.

@@lean4 [group-cancel]
```

An optional selector in parentheses points the placeholder to a specific Lean file:

```markdown
@@lean4(../UNSW/GraphTensor.lean) [graph-iso]   -- relative path link
@@lean4(newfile:2) [scratch]                    -- extra mirror file
```

Selectors are normalized by `canonicalLeanSelector` (in `shared/lean-placeholder.mjs`)
before use. Omitting the selector is equivalent to `selector = ""` (default mirror).

For a Markdown note:

```text
roam/math/group.md
```

Aaronnote derives the Lean file:

```text
roam/.lean/math/group.lean        (default mirror, no selector)
roam/.lean/math/group.mirror-2.lean   (newfile:2 selector)
```

The Lean file contains ordinary Lean source plus Aaronnote tag markers:

```lean
import Mathlib

variable (G : Type*) [Group G]

-- @aaronnote group-cancel
example (a b : G) : a * b * b⁻¹ = a := by
  simp

-- @aaronnote second-fragment
#check Nat
```

The `group-cancel` region is everything after `-- @aaronnote group-cancel`
until the next `-- @aaronnote ...` marker or EOF. Content before the first tag is
file prelude and is not rendered as an embedded block.

## Project Layout

For a notes root such as `roam/`, the Lean project root is:

```text
roam/.lean/
```

That directory owns `lakefile.toml`, `lean-toolchain`, `lake-manifest.json`,
mirror Lean files, and the Lake cache at:

```text
roam/.lean/.lake/
```

Do not keep or recreate `roam/.lake/`. It is a stale duplicate cache outside
the active Lean project root and can make it unclear which Lake state the app is
using. Project commands should run from `roam/.lean/` or through the notes-root
`Makefile` wrappers.

## Editing Experience

- In markdown preview mode, a whole-line `@@lean4 [tag]` is replaced by an embedded Lean editor.
- The embedded editor edits the matching region in the derived `.lean` file, not
  the Markdown document.
- The Markdown document keeps only the placeholder text.
- The embedded editor is mounted immediately as a block widget with Shadow DOM
  isolation, so the outer Markdown CM6 styles and event handling do not drive
  the inner editor.
- Cursor movement inside the embedded editor drives Lean hover, diagnostics, goals,
  expected type, semantic tokens, and the left Infoview panel.
- The left Lean panel is a drawer with a scrollable Infoview/messages pane above
  a fixed bottom outline. The panel width and outline height are draggable and
  remembered per note for the current app session.
- The outline uses Lean LSP document symbols instead of scanning mirror files,
  can be collapsed or resized vertically, and outline rows jump back to embedded
  Lean regions by their LSP line/character position.
- Keyboard input while the embedded Lean editor is focused is consumed by the
  child editor and is not forwarded to the outer Markdown editor or Vim layer.
- Lean LSP is opened only for real derived `.lean` files and only after the
  user interacts with a Lean editor or requests LSP-backed data such as goals,
  hover, completion, diagnostics, or Infoview content. Markdown text is never
  sent to Lean. Notes without Lean placeholders do not start Lean, and switching
  away lets the Lean server idle instead of stopping it immediately.
- The embedded editor uses Lean-only behavior: completion comes from Lean LSP
  `textDocument/completion`, syntax color comes from Lean Tree-sitter plus Lean
  semantic tokens, completion item-kind icons come from the returned LSP items,
  and Lean symbol input uses the `lean4-mode` abbreviation table. Markdown and
  TeX modes do not provide snippets or completion inside this editor.
- The embedded editor owns basic editing behavior: Tab inserts/indents inside
  Lean, Shift-Tab unindents, Enter keeps indentation, and Escape enters a
  Lean-local Vim normal mode with basic movement, delete/yank/paste, visual
  selection, undo/redo, and line opening commands.
- `Cmd/Ctrl+/` toggles Lean line comments while the embedded editor is focused;
  outside Lean it keeps its Markdown source/preview behavior. `Alt+Shift+A`
  toggles block comments, `Alt+Up/Down` moves lines, and `Alt+Shift+Up/Down`
  duplicates lines.
- Right-clicking an embedded Lean editor opens native **Lean Symbol** and
  **Lean Edit** submenus. Read-only symbol navigation is also exposed in the
  command palette and Lean-local Vim normal mode.
- Definition-style results jump directly to an embedded region when Aaronnote
  owns the target. Multiple targets open a searchable picker. Targets outside
  embedded regions open a fresh Kitty window running Nvim; `AARONNOTE_KITTY`
  and `AARONNOTE_NVIM` override executable discovery when needed.
- Lean-local Vim normal mode supports `s` as the same visible jump overlay used
  by Markdown mode. `S` remains the simple character-search command.
- When completion is open, ArrowUp/ArrowDown/PageUp/PageDown/Enter/Tab and
  `Cmd/Ctrl+1..9` select or accept Lean LSP candidates inside the popup. These
  keys are not forwarded to the outer Markdown editor.
- Lean hover/completion documentation is capped in height and scrolls inside the
  tooltip, so long docs do not cover the whole editor.
- Lean diagnostics and progress use existing LSP state to draw gutter markers;
  this does not add polling or extra LSP requests.
- Find (`Cmd/Ctrl+F`) can search both Markdown and embedded Lean editors. The
  find bar scope can be set to `Code` to search only Lean regions.
- Copilot inline suggestions work inside embedded Lean editors and standalone
  `.lean` files. The Lean child editor registers with the Copilot plugin only
  while mounted, so the plugin reuses the same request/debounce path as the main
  editor instead of creating a second client.
- Static publish and desktop PDF export replace `@@lean4 [tag]` with a read-only
  Aaronnote-style Lean code cell and syntax-highlight the exported source.
- If the tag is missing, the widget shows a missing-region state and can create
  the marker in the derived Lean file.

## Insert Flow

The command palette exposes two commands:

- **Insert Lean block** (`Cmd/Ctrl+Shift+L`) — quick insert into the default
  mirror with a generated tag, no modal.
- **Lean block manager** — interactive modal that lets you choose the target
  (default mirror / mirror number / Lean file link), tag mode (new generated tag
  or existing tag in the selected file), and shows early validation:
  - "New mirror file will be created" when the mirror number has no existing file.
  - "Mirror exists with N tags" when the mirror already contains regions.
  - "Tag already exists — will write to the same region" when a new-tag name
    matches an existing region (not an error; inserts a second reference).

Default quick-insert behavior:

1. Derive the current note's mirror Lean file.
2. Create the file if it does not exist.
3. Generate a unique tag, for example `lean-20260523-143012`.
4. Append `-- @aaronnote <tag>` to the Lean file.
5. Insert `@@lean4 [<tag>]` into Markdown at the cursor.
6. Focus the embedded Lean editor for that tag.

## Lean LSP Navigation

Embedded Lean editors expose read-only LSP navigation without allowing arbitrary
workspace edits. All navigation first syncs the current region body back into
the derived `.lean` file, opens the corresponding Lean LSP document if needed,
and sends the request at the full-file Lean position.

Supported LSP actions:

- Definition: `textDocument/definition`.
- Declaration: `textDocument/declaration`.
- Type definition: `textDocument/typeDefinition`.
- Implementation: `textDocument/implementation`.
- References: `textDocument/references`, with declaration included.
- Hover: `textDocument/hover`, rendered in the embedded editor tooltip channel.

Navigation entry points are intentionally shared:

- Lean Vim normal mode: `gd`, `gD`, `gy`, `gi`, `gr`, and `K`.
- `Cmd/Ctrl` + left click: definition at the clicked Lean token.
- Native right-click menu: **Lean Symbol** submenu.
- Command palette: `Lean: Go to Definition`, `Lean: Find References`, and the
  other `Lean:` symbol commands.

Result routing:

1. LSP `Location`, `Location[]`, and `LocationLink[]` values are normalized to
   `{ uri, file, range, summary }` and deduped by file/line/character.
2. A single non-reference result jumps immediately. Reference results and other
   multi-result responses open the Lean locations picker.
3. If the target position belongs to an Aaronnote `-- @aaronnote <tag>` region,
   Aaronnote dispatches an in-app region jump. Cross-note jumps open the target
   note and replay the region jump after the embedded editor mounts.
4. If the target is outside an embedded region, for example Mathlib, prelude, or
   another external `.lean` source, Aaronnote opens the target in Kitty/Nvim.
5. Before navigating, Aaronnote records a normal app jump-stack entry so
   **Jump back** can return to the Markdown/Lean origin.

The locations picker reuses the command-palette UI model: type to filter by file
or source-line summary, use ArrowUp/ArrowDown to move, Enter to pick, and Escape
to cancel.

## Lean Edit Tools

Lean edit tools mutate only the embedded child document. The usual region save
path then writes the body back to the mirror `.lean` file, so every action stays
inside the current `@@lean4` region and remains undoable as a single editor
transaction.

Available edit actions:

- Toggle line comment: inserts or removes Lean `--` comments at line indentation.
- Toggle block comment: wraps or unwraps the selection with `/- ... -/`.
- Duplicate up/down: copies the selected line range above or below.
- Move up/down: swaps the selected line range with the adjacent line.
- Join lines: joins the selected lines, collapsing indentation on following lines.
- Delete trailing whitespace: trims the whole embedded Lean region.
- Indent/outdent: uses the Lean indent unit and selected-line semantics.

Keyboard entry points:

- `Cmd/Ctrl+/`: toggle line comments when focus is inside Lean. The desktop menu
  shows the same shortcut for source toggle, but does not register the accelerator
  globally; the renderer routes by focus so Markdown keeps source/preview toggle.
- `Alt+Shift+A`: toggle block comments.
- `Alt+Up/Down`: move selected lines up/down.
- `Alt+Shift+Up/Down`: duplicate selected lines up/down.
- Vim normal `gcc`: toggle line comment.
- Vim normal `J`: join current and next line.
- Vim visual and visual-line `gc` / `gb`: toggle line/block comments.
- Vim visual-line `y` and `d`: use a linewise register, so `p` / `P` paste whole
  lines instead of character ranges.

Mouse/menu entry points:

- Right-click inside an embedded Lean editor opens a native context menu with:
  - **Lean Symbol**: definition, declaration, type definition, implementation,
    references, and hover.
  - **Lean Edit**: comment toggles, duplicate, move, join, trim whitespace,
    indent, and outdent.

## External Lean Sources

When LSP navigation resolves outside an Aaronnote-managed region, Aaronnote opens
the target in a fresh Kitty window running Nvim at the LSP position. LSP positions
are 0-based; the generated Nvim `cursor()` call is 1-based.

Executable discovery order:

1. `AARONNOTE_KITTY` and `AARONNOTE_NVIM`, when set.
2. Common GUI-app Homebrew paths: `/opt/homebrew/bin` and `/usr/local/bin`.
3. The process `PATH`.

The launcher always builds an argv array and starts Kitty with detached
`spawn(..., { stdio: "ignore" })`; it does not shell-concatenate file paths or
commands. If Kitty, Nvim, or the target file cannot be found, the renderer shows
the failure in the Aaronnote status bar.

## Keyboard and Navigation

| Context | Key | Behavior |
| --- | --- | --- |
| Embedded Lean editor | `Ctrl+Enter` | Toggle between the editor and Notes ranger. Opening Filesystem or Recent hides the Lean panel first. |
| Notes page | `Tab` | Switch between Filesystem and Recent. |
| Recent list | `ArrowLeft` / `ArrowRight` / `h` / `l` | Move selection by one item. |
| Recent list | `ArrowUp` / `ArrowDown` / `k` / `j` | Move selection by one visual row. |
| Recent list | `Home` / `End` | Jump to first or last recent item. |
| Recent list | `Enter` | Open the selected recent item. |
| Lean Vim normal mode | `s`, then query, then label | Jump to a visible match in the embedded Lean editor. |
| Lean Vim normal mode | `S`, then character | Use the lightweight Lean-local character search. |
| Lean Vim normal mode | `gd` / `gD` / `gy` / `gi` / `gr` | Go to definition / declaration / type definition / implementation / references. |
| Lean Vim normal mode | `K` | Show hover information. |
| Lean Vim normal mode | `gcc` | Toggle the current line comment. |
| Lean Vim normal mode | `J` | Join the current line with the next line. |
| Lean Vim visual mode | `gc` / `gb` | Toggle line / block comments for the selection. |
| Lean Vim visual-line mode | `gc` / `gb` | Toggle line / block comments for selected lines. |
| Lean Vim visual-line mode | `y` / `d`, then `p` / `P` | Copy/delete and paste selected whole lines. |
| Embedded Lean editor | `Cmd/Ctrl+/` | Toggle line comments. |
| Embedded Lean editor | `Alt+Shift+A` | Toggle a block comment. |
| Embedded Lean editor | `Alt+Up/Down` | Move selected lines. |
| Embedded Lean editor | `Alt+Shift+Up/Down` | Duplicate selected lines. |
| Copilot in insert mode | `Cmd+]` / `Cmd+Right` | Accept visible inline suggestion, otherwise advance snippet/delimiter. |
| Copilot in insert mode | `Cmd+}` then character | Accept the visible inline suggestion through the next occurrence of that character. |

## LSP Lifecycle

- Aaronnote starts `lake serve` in `roam/.lean/`.
- Project maintenance is explicit. The notes root has a `Makefile` with `update`,
  `cache`, `build`, `clean`, and `info` targets. The Notes → Lean page runs those
  targets on demand and shows Lean/Lake version, toolchain, Lake project path,
  and manifest packages.
- Aaronnote does not run `lake build` or automatically repair Mathlib/ProofWidgets
  caches during editor startup. When Lean reports stale widgets or missing
  artifacts, use Notes → Lean → Cache, which runs `make cache` (`lake exe cache get`).
- Each active Lean editor opens its derived real `.lean` file as the LSP
  document on demand. Multiple embedded regions in the same file share the LSP
  document through server-side reference counting.
- Region edits update the full Lean file text and send `textDocument/didChange`
  for the real file URI.
- Diagnostics and progress notifications are stored by full Lean file URI, then
  projected into embedded editors by region offsets.
- Goal and hover requests convert region-local cursor positions to full-file Lean
  positions before calling Lean LSP.
- Completion requests use the same full-file position mapping and preserve LSP
  `insertText`, `filterText`, details, and documentation in the popup.
- Tree-sitter/WASM highlights the child editor immediately. Semantic token
  notifications are filtered to the active region and layered into the child
  editor when Lean LSP publishes them. There is no handwritten Lean parser in
  the placeholder path.

## Performance Boundaries

- Lean UI markers are derived from already-stored diagnostics, progress, and
  completion items. They must not introduce background polling or full-file scans.
- Lean edit tools must stay region-local. Do not apply LSP workspace edits or
  code actions directly to the full file unless every edit is first proven to be
  inside the mounted region.
- Server-side region neighbor lookup (`getRegionNeighbors`) scans the full
  markdown file to determine insertion ordering. This scan must only run when a
  region does not yet exist — all request handlers must go through
  `readOrEnsureLeanRegionFromRequest`, which checks for an existing region first
  and skips the scan for the common case (region already present).
- Placeholder index updates are incremental: `patchLeanPlaceholderIndex` rescans
  only lines touched by the current transaction. Full-document scans happen only
  on cold load.
- Widget height re-measurement on window resize is handled by `MeasuredWidget`'s
  shared `ResizeObserver`. Widgets must not add their own `window.resize`
  listeners; CM6 and the `ResizeObserver` together are sufficient.
- LSP documents are kept open for `AARONNOTE_LEAN_DOCUMENT_IDLE_MS` (default
  90 s) after the last reference drops. Rapid block switching does not cause
  repeated `didClose`/`didOpen` round-trips.
- Outline data comes from `textDocument/documentSymbol` on demand and is kept in
  the panel state; the outline UI reuses its existing resize/collapse controls.
- Copilot auxiliary registration is mount-scoped. Destroying the embedded editor
  dispatches the matching unregister event so stale Lean editors do not keep
  listeners or request completions.
- Source/preview jump correction uses the existing child-editor geometry at the
  moment of navigation. It should not continuously measure all Lean widgets on
  every input.

## Implementation Plan

1. Add pure tagged-region helpers:
   - derive mirror Lean path from Markdown path;
   - scan `-- @aaronnote <tag>` markers;
   - read, create, update, and locate sequential regions;
   - convert region-local offsets to full-file offsets and back.
2. Add server IPC actions:
   - `ensure-region`;
   - `read-region`;
   - `update-region`;
   - `open-region-file`;
   - `get-region-meta`.
3. Add renderer API wrappers for those IPC actions.
4. Replace the Lean CM6 markdown behavior:
   - scan whole-line `@@lean4 [tag]` placeholders;
   - render an isolated block widget containing the embedded Lean editor;
   - sync child editor changes to the real Lean region;
   - keep parent Markdown unchanged.
5. Rewire Lean panel and LSP mapping:
   - active Lean context becomes `(notePath, leanPath, tag, region offsets)`;
   - hover/goals use full-file Lean positions;
   - diagnostics and semantic tokens are filtered and displayed per active region;
   - completion uses Lean LSP only.
6. Add command palette and shortcut support for inserting a new Lean placeholder.
7. Disable the old `#+begin lean4` LSP path once the placeholder flow is usable.
8. Keep migrating interaction details from `~/.config/emacs/elpa/lean4-mode/`:
   - InfoView debounce and section layout from `lean4-info.el`;
   - Lean input abbreviations from `lean4-input.el`;
   - project root/toolchain rules from `lean4-mode.el` and `lean4-lake.el`.

## Tests

- Unit tests for `@@lean4 [tag]` scanning.
- Unit tests for tagged Lean region parsing and update.
- Server tests for creating and updating mirror Lean regions.
- CM6 tests for rendering placeholders as embedded Lean editors.
- Shortcut/command tests for inserting a new placeholder.
- LSP mapping tests for converting diagnostics and goals between full-file and
  region-local positions.
- Regression tests that `@@todo` and `@@tag` behavior is unchanged.

## Deprecated Behavior

The old `#+begin lean4 ... #+end lean4` model is deprecated. New Lean content
must use `@@lean4 [tag]`. Existing org-env Lean support should not remain a
second long-term LSP path.

The current Lean LSP navigation layer is intentionally read-only. Rename,
workspace-wide code actions, formatting, import organization, and arbitrary LSP
workspace edits remain out of scope until Aaronnote has a region-aware edit
application policy that can reject or split edits crossing `@@lean4` boundaries.
