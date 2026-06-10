/**
 * Heading folding for aaronnote.
 *
 * Reuses the incremental tocIndexField (toc-index.ts) — zero new document scanning.
 * Only headings with source === "markdown" are foldable.
 *
 * Chevron widget:
 * - Inline <span> positioned into the left margin.
 * - Zero vertical height — bare WidgetType (no MeasuredWidget per CLAUDE.md).
 * - Invisible by default; shown on heading line hover via CSS.
 *
 * Fold commands: fold-heading, unfold-heading, toggle-fold,
 *                fold-all-headings, unfold-all-headings
 * vim-lite: zc, zo, za, zM, zR
 */
import { codeFolding, foldAll, foldCode, foldService, unfoldAll, unfoldCode } from "@codemirror/language";
import { RangeSetBuilder, type EditorState, type Extension } from "@codemirror/state";
import { Decoration, type DecorationSet, EditorView, ViewPlugin, WidgetType } from "@codemirror/view";
import { tocIndexFromState, type MarkdownHeading } from "./toc-index.ts";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

interface HeadingWithLine extends MarkdownHeading {
  lineNumber: number;
}

function markdownHeadingsWithLines(state: EditorState): HeadingWithLine[] {
  const index = tocIndexFromState(state);
  return index.headings
    .filter((h) => h.source === "markdown")
    .map((h) => ({
      ...h,
      lineNumber: state.doc.lineAt(h.markerFrom ?? h.pos).number,
    }));
}

function foldRangeForHeading(
  headings: HeadingWithLine[],
  h: HeadingWithLine,
  doc: EditorView["state"]["doc"],
): { from: number; to: number } | null {
  const headingLine = doc.line(h.lineNumber);
  let end = doc.line(doc.lines).to;

  for (const other of headings) {
    if (other === h || other.lineNumber <= h.lineNumber) continue;
    if ((other.renderLevel ?? other.level) <= (h.renderLevel ?? h.level)) {
      end = doc.line(other.lineNumber - 1).to;
      break;
    }
  }

  if (end <= headingLine.to) return null;
  return { from: headingLine.to, to: end };
}

// ---------------------------------------------------------------------------
// foldService — tells CodeMirror how to compute fold ranges for headings
// ---------------------------------------------------------------------------

const headingFoldService = foldService.of((state, lineStart) => {
  const headings = markdownHeadingsWithLines(state);
  const lineNumber = state.doc.lineAt(lineStart).number;
  const h = headings.find((heading) => heading.lineNumber === lineNumber);
  if (!h) return null;
  return foldRangeForHeading(headings, h, state.doc);
});

// ---------------------------------------------------------------------------
// Chevron widget — zero height inline span, CSS-hidden until hover/folded
// ---------------------------------------------------------------------------

class ChevronWidget extends WidgetType {
  eq() { return true; }
  toDOM() {
    const span = document.createElement("span");
    span.className = "cm-heading-fold-arrow";
    span.setAttribute("aria-hidden", "true");
    return span;
  }
  ignoreEvent() { return false; }
}

const chevronWidget = Decoration.widget({ widget: new ChevronWidget(), side: -1 });

function buildChevronDecos(view: EditorView): DecorationSet {
  const headings = markdownHeadingsWithLines(view.state);
  if (headings.length === 0) return Decoration.none;

  const builder = new RangeSetBuilder<Decoration>();
  const { from: vpFrom, to: vpTo } = view.viewport;

  for (const h of headings) {
    const markerPos = h.markerFrom ?? h.pos;
    if (markerPos < vpFrom || markerPos > vpTo) continue;
    // Only add chevron if the heading has something to fold
    const range = foldRangeForHeading(headings, h, view.state.doc);
    if (!range) continue;
    const lineStart = view.state.doc.line(h.lineNumber).from;
    builder.add(lineStart, lineStart, chevronWidget);
  }

  return builder.finish();
}

const chevronPlugin = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;
    constructor(view: EditorView) { this.decorations = buildChevronDecos(view); }
    update(update: { docChanged: boolean; viewportChanged: boolean; view: EditorView }) {
      if (update.docChanged || update.viewportChanged) {
        this.decorations = buildChevronDecos(update.view);
      }
    }
  },
  { decorations: (v) => v.decorations },
);

// ---------------------------------------------------------------------------
// Commands
// ---------------------------------------------------------------------------

export function foldHeadingAtCursor(view: EditorView): boolean { return foldCode(view); }
export function unfoldHeadingAtCursor(view: EditorView): boolean { return unfoldCode(view); }
export function toggleFoldAtCursor(view: EditorView): boolean {
  if (!unfoldCode(view)) return foldCode(view);
  return true;
}
export function foldAllHeadings(view: EditorView): boolean { return foldAll(view); }
export function unfoldAllHeadings(view: EditorView): boolean { return unfoldAll(view); }

// ---------------------------------------------------------------------------
// Extension
// ---------------------------------------------------------------------------

export const headingFoldExtension: Extension = [
  codeFolding(),
  headingFoldService,
  chevronPlugin,
];
