/**
 * Phase 5 — Command dispatch and quick-insert for the CM6 kernel.
 *
 * Since CM6 doc IS the markdown source, every command is a plain text
 * mutation. No schema round-trip is needed.
 *
 * runCommandCM6      — implements all EditorCommand variants
 * getBlockContextCM6 — reads the Lezer syntax tree at the cursor
 * createQuickInsertRegistry — factory for per-editor provider set
 */

import { EditorView } from "@codemirror/view";
import { syntaxTree } from "@codemirror/language";
import type {
  EditorBlockContext,
  EditorCommand,
  QuickInsertContext,
  QuickInsertItem,
  QuickInsertProvider,
} from "../editor-api.ts";
import {
  blockCommands,
  builtInQuickInsertProvider,
  quickMatches,
} from "../editor-api.ts";
import { indentLess, indentMore } from "@codemirror/commands";

// ---------------------------------------------------------------------------
// Inline wrap (bold / italic / highlight / strike / code / link / image)
// ---------------------------------------------------------------------------

function wrapInline(view: EditorView, open: string, close: string): boolean {
  const { from, to } = view.state.selection.main;
  if (from === to) {
    view.dispatch({
      changes: { from, insert: open + close },
      selection: { anchor: from + open.length },
      scrollIntoView: true,
    });
  } else {
    const selected = view.state.doc.sliceString(from, to);
    const wrapped = open + selected + close;
    view.dispatch({
      changes: { from, to, insert: wrapped },
      selection: { anchor: from + open.length, head: from + open.length + selected.length },
      scrollIntoView: true,
    });
  }
  return true;
}

// ---------------------------------------------------------------------------
// Line prefix transform (headings / blockquote / list types)
// ---------------------------------------------------------------------------

function mutateCurrentLine(view: EditorView, fn: (line: string) => string): boolean {
  const { from } = view.state.selection.main;
  const line = view.state.doc.lineAt(from);
  const newText = fn(line.text);
  view.dispatch({
    changes: { from: line.from, to: line.to, insert: newText },
    selection: { anchor: line.from + newText.length },
    scrollIntoView: true,
  });
  return true;
}

// Strip common list/task prefixes so commands can re-apply cleanly.
const LIST_PREFIX_RE = /^\s*(?:[-*+]\s+|\d+[.)]\s+|- \[[ xX]\]\s+)/;
const EMPTY_LIST_RE = /^(\s*)(?:[-*+]\s+|\d+[.)]\s+|- \[[ xX]\]\s*)$/;
const LIST_LINE_RE = /^(\s*)(?:[-*+]\s+|\d+[.)]\s+|- \[[ xX]\]\s+)/;
const EMPTY_QUOTE_RE = /^\s{0,3}>\s?$/;
const EMPTY_QUOTE_LIST_RE = /^(\s{0,3}(?:>\s*)+)(?:[-*+]\s*|\d+[.)]\s*|- \[[ xX]\]\s*)$/;
const CONTINUE_MARKUP_RE = /^(\s{0,3}(?:>\s*)*)(\s*)(?:(- \[[ xX]\]\s+)|([-*+])\s+|(\d+)([.)])\s+)(.*)$/;
const CONTINUE_QUOTE_RE = /^(\s{0,3}(?:>\s*)+)(.*)$/;

// ---------------------------------------------------------------------------
// Block insert (inserts below current line when it is non-empty)
// ---------------------------------------------------------------------------

function insertBlock(view: EditorView, text: string, cursorOffset: number): void {
  const { from } = view.state.selection.main;
  const doc = view.state.doc;
  const line = doc.lineAt(from);

  if (line.text.trim().length === 0) {
    // Replace the blank line in-place
    view.dispatch({
      changes: { from: line.from, to: line.to, insert: text },
      selection: { anchor: line.from + cursorOffset },
      scrollIntoView: true,
    });
  } else {
    // Insert after current line
    view.dispatch({
      changes: { from: line.to, insert: "\n" + text },
      selection: { anchor: line.to + 1 + cursorOffset },
      scrollIntoView: true,
    });
  }
}

export function exitEmptyMarkdownBlock(view: EditorView): boolean {
  const sel = view.state.selection.main;
  if (!sel.empty) return false;
  const line = view.state.doc.lineAt(sel.from);
  const quoteList = line.text.match(EMPTY_QUOTE_LIST_RE);
  if (quoteList) {
    const prefix = quoteList[1] ?? "";
    view.dispatch({
      changes: { from: line.from, to: line.to, insert: prefix },
      selection: { anchor: line.from + prefix.length },
      scrollIntoView: true,
    });
    return true;
  }
  if (!EMPTY_LIST_RE.test(line.text) && !EMPTY_QUOTE_RE.test(line.text)) return false;
  view.dispatch({
    changes: { from: line.from, to: line.to, insert: "" },
    selection: { anchor: line.from },
    scrollIntoView: true,
  });
  return true;
}

export function continueMarkdownMarkup(view: EditorView): boolean {
  const sel = view.state.selection.main;
  if (!sel.empty) return false;
  const line = view.state.doc.lineAt(sel.from);
  const beforeCursor = view.state.doc.sliceString(line.from, sel.from);
  const match = beforeCursor.match(CONTINUE_MARKUP_RE);
  if (!match) return false;
  const content = match[7] ?? "";
  if (content.trim().length === 0) return false;

  const quotePrefix = match[1] ?? "";
  const indent = match[2] ?? "";
  const task = match[3];
  const bullet = match[4];
  const ordered = match[5];
  const orderedDelim = match[6] ?? ".";
  const nextMarker = task
    ? task
    : bullet
      ? `${bullet} `
      : `${Number(ordered) + 1}${orderedDelim} `;
  const insert = `\n${quotePrefix}${indent}${nextMarker}`;
  view.dispatch({
    changes: { from: sel.from, insert },
    selection: { anchor: sel.from + insert.length },
    scrollIntoView: true,
  });
  return true;
}

export function continueMarkdownQuote(view: EditorView): boolean {
  const sel = view.state.selection.main;
  if (!sel.empty) return false;
  const line = view.state.doc.lineAt(sel.from);
  const beforeCursor = view.state.doc.sliceString(line.from, sel.from);
  const match = beforeCursor.match(CONTINUE_QUOTE_RE);
  if (!match) return false;
  const content = match[2] ?? "";
  if (content.trim().length === 0) return false;
  const prefix = match[1] ?? "";
  const insert = `\n${prefix}`;
  view.dispatch({
    changes: { from: sel.from, insert },
    selection: { anchor: sel.from + insert.length },
    scrollIntoView: true,
  });
  return true;
}

export function continueMarkdownBlock(view: EditorView): boolean {
  return continueMarkdownMarkup(view) || continueMarkdownQuote(view);
}

function lineStartOffsets(lines: readonly string[]): number[] {
  const offsets: number[] = [];
  let pos = 0;
  for (const line of lines) {
    offsets.push(pos);
    pos += line.length + 1;
  }
  return offsets;
}

function mapPosAcrossLinePrefixChange(
  pos: number,
  blockFrom: number,
  oldDocLines: readonly { from: number; text: string }[],
  newLines: readonly string[],
): number {
  const index = oldDocLines.findIndex((line, lineIndex) => {
    const end = line.from + line.text.length;
    return pos >= line.from && (pos <= end || lineIndex === oldDocLines.length - 1);
  });
  if (index < 0) return pos;
  const oldLine = oldDocLines[index]!;
  const oldCol = Math.max(0, pos - oldLine.from);
  const delta = (newLines[index]?.length ?? oldLine.text.length) - oldLine.text.length;
  const newOffsets = lineStartOffsets(newLines);
  return blockFrom + (newOffsets[index] ?? 0) + Math.max(0, oldCol + delta);
}

export function indentMarkdownList(view: EditorView, direction: 1 | -1): boolean {
  const sel = view.state.selection.main;
  const doc = view.state.doc;
  const startLine = doc.lineAt(sel.from).number;
  const endLine = doc.lineAt(Math.max(sel.from, sel.to - (sel.to > sel.from ? 1 : 0))).number;
  const oldDocLines: Array<{ from: number; text: string }> = [];
  const newLines: string[] = [];
  let changed = false;

  for (let lineNum = startLine; lineNum <= endLine; lineNum++) {
    const line = doc.line(lineNum);
    oldDocLines.push({ from: line.from, text: line.text });
    if (!LIST_LINE_RE.test(line.text)) {
      newLines.push(line.text);
      continue;
    }
    if (direction > 0) {
      newLines.push(`  ${line.text}`);
      changed = true;
      continue;
    }
    const next = line.text.replace(/^ {1,2}/, "");
    newLines.push(next);
    changed = changed || next !== line.text;
  }

  if (!changed) return false;
  const blockFrom = oldDocLines[0]!.from;
  const blockTo = oldDocLines.at(-1)!.from + oldDocLines.at(-1)!.text.length;
  view.dispatch({
    changes: { from: blockFrom, to: blockTo, insert: newLines.join("\n") },
    selection: {
      anchor: mapPosAcrossLinePrefixChange(sel.anchor, blockFrom, oldDocLines, newLines),
      head: mapPosAcrossLinePrefixChange(sel.head, blockFrom, oldDocLines, newLines),
    },
    scrollIntoView: true,
  });
  return true;
}

export function indentMarkdownBlock(view: EditorView, direction: 1 | -1): boolean {
  if (indentMarkdownList(view, direction)) return true;
  const fallback = direction > 0 ? indentMore : indentLess;
  fallback(view);
  return true;
}

// ---------------------------------------------------------------------------
// Block context from Lezer tree
// ---------------------------------------------------------------------------

export function getBlockContextCM6(view: EditorView): EditorBlockContext {
  const { from } = view.state.selection.main;
  const doc = view.state.doc;

  const curLine = doc.lineAt(from);
  let type = "paragraph";
  let blockFrom = curLine.from;
  let blockTo = curLine.to;
  let contentFrom = curLine.from;
  let contentTo = curLine.to;

  let cur = syntaxTree(view.state).resolve(from, -1);
  while (cur && cur.name !== "Document") {
    const name = cur.name;
    if (name === "FencedCode" || name === "CodeBlock" || name === "IndentedCode") {
      type = "code_block";
      blockFrom = cur.from;
      blockTo = cur.to;
      const textNode = cur.getChild("CodeText");
      contentFrom = textNode?.from ?? blockFrom;
      contentTo = textNode?.to ?? blockTo;
      break;
    }
    if (name === "TableCell" || name === "TableHeader" || name === "Table") {
      type = "table_cell";
      blockFrom = cur.from;
      blockTo = cur.to;
      contentFrom = blockFrom;
      contentTo = blockTo;
      break;
    }
    if (/^ATXHeading[1-6]$/.test(name) || /^SetextHeading[12]$/.test(name)) {
      type = "heading";
      blockFrom = cur.from;
      blockTo = cur.to;
      contentFrom = headingContentFrom(view, cur.from, cur.to);
      contentTo = cur.to;
      break;
    }
    if (name === "Blockquote") {
      type = "blockquote";
      blockFrom = cur.from;
      blockTo = cur.to;
      contentFrom = blockFrom;
      contentTo = blockTo;
      break;
    }
    if (name === "ListItem") {
      type = "list_item";
      blockFrom = cur.from;
      blockTo = cur.to;
      contentFrom = listItemContentFrom(view, cur.from, cur.to);
      contentTo = blockTo;
      break;
    }
    if (name === "Paragraph") {
      type = "paragraph";
      blockFrom = cur.from;
      blockTo = cur.to;
      contentFrom = blockFrom;
      contentTo = blockTo;
      break;
    }
    if (!cur.parent) break;
    cur = cur.parent;
  }

  const text = blockContextText(view, type, blockFrom, blockTo, contentFrom, contentTo);

  let rect: { left: number; top: number; bottom: number } | null = null;
  try {
    const coords = view.coordsAtPos(from);
    if (coords) rect = { left: coords.left, top: coords.top, bottom: coords.bottom };
  } catch { /* view may not be mounted yet */ }

  return {
    type,
    from: blockFrom,
    to: blockTo,
    contentFrom,
    contentTo,
    text,
    empty: text.trim().length === 0,
    depth: 1,
    parentType: null,
    sourceMode: false,
    commands: blockCommands(type),
    rect,
  };
}

function headingContentFrom(view: EditorView, from: number, to: number): number {
  const raw = view.state.doc.sliceString(from, to);
  const atx = raw.match(/^\s{0,3}#{1,6}\s+/);
  if (atx) return from + atx[0].length;
  return from;
}

function listItemContentFrom(view: EditorView, from: number, to: number): number {
  const raw = view.state.doc.sliceString(from, to);
  const marker = raw.match(/^\s*(?:[-*+]\s+|\d+[.)]\s+|- \[[ xX]\]\s+)/);
  if (marker) return from + marker[0].length;
  return from;
}

function blockContextText(
  view: EditorView,
  type: string,
  from: number,
  to: number,
  contentFrom: number,
  contentTo: number,
): string {
  const doc = view.state.doc;
  if (type === "blockquote") {
    return doc.sliceString(from, to)
      .split("\n")
      .map((line) => line.replace(/^\s{0,3}>\s?/, ""))
      .join("\n");
  }
  return doc.sliceString(contentFrom, contentTo);
}

// ---------------------------------------------------------------------------
// Copy code block at cursor
// ---------------------------------------------------------------------------

function codeBlockAtCursor(view: EditorView): string | null {
  const { from } = view.state.selection.main;
  let cur = syntaxTree(view.state).resolve(from, -1);
  while (cur && cur.name !== "Document") {
    if (cur.name === "FencedCode") {
      const textNode = cur.getChild("CodeText");
      return textNode ? view.state.doc.sliceString(textNode.from, textNode.to) : "";
    }
    if (!cur.parent) break;
    cur = cur.parent;
  }
  return null;
}

// ---------------------------------------------------------------------------
// Table manipulation (text-level)
// ---------------------------------------------------------------------------

type TableInfo = {
  lines: string[];
  startLineNum: number; // 1-based doc line number
  currentRowIdx: number; // 0-based index within table
  currentColIdx: number; // 0-based index within current row
};

function findTableInfo(view: EditorView): TableInfo | null {
  const { from } = view.state.selection.main;
  const doc = view.state.doc;
  const curLine = doc.lineAt(from);
  if (!/^\s*\|.*\|\s*$/.test(curLine.text)) return null;

  let start = curLine.number;
  while (start > 1 && /^\s*\|.*\|\s*$/.test(doc.line(start - 1).text)) start--;
  let end = curLine.number;
  while (end < doc.lines && /^\s*\|.*\|\s*$/.test(doc.line(end + 1).text)) end++;

  const lines: string[] = [];
  for (let i = start; i <= end; i++) lines.push(doc.line(i).text);
  return {
    lines,
    startLineNum: start,
    currentRowIdx: curLine.number - start,
    currentColIdx: columnIndexAtOffset(curLine.text, from - curLine.from),
  };
}

function splitCells(row: string): string[] {
  return row.split("|").slice(1, -1).map((c) => c.trim() || " ");
}

function buildRow(cells: string[]): string {
  return "| " + cells.join(" | ") + " |";
}

function isSeparatorRow(row: string): boolean {
  const compact = row.replace(/\s/g, "");
  return compact.includes("-") && /^\|[-|:]+\|$/.test(compact);
}

function columnIndexAtOffset(row: string, offset: number): number {
  const cellCount = splitCells(row).length;
  if (cellCount <= 0) return 0;
  let col = 0;
  for (let i = 0; i < row.length; i++) {
    if (i >= offset) break;
    if (row[i] === "|") col++;
  }
  return Math.max(0, Math.min(cellCount - 1, col - 1));
}

function rowOffset(lines: string[], rowIdx: number): number {
  return lines.slice(0, rowIdx).reduce((s, line) => s + line.length + 1, 0);
}

function cellOffset(row: string, colIdx: number): number {
  let seen = -1;
  for (let i = 0; i < row.length; i++) {
    if (row[i] !== "|") continue;
    seen++;
    if (seen === colIdx) return Math.min(row.length, i + 2);
  }
  return Math.max(0, row.length - 1);
}

function runTableCommandCM6(view: EditorView, command: EditorCommand): boolean {
  const info = findTableInfo(view);
  if (!info) return false;
  const { lines, startLineNum, currentRowIdx, currentColIdx } = info;
  const doc = view.state.doc;
  const startPos = doc.line(startLineNum).from;
  const endPos = doc.line(startLineNum + lines.length - 1).to;

  let newLines = [...lines];
  let newCursorRow = currentRowIdx;
  let newCursorCol = currentColIdx;

  if (command === "table-insert-row") {
    const colCount = splitCells(lines[0] ?? "").length;
    const emptyRow = buildRow(Array(colCount).fill(" "));
    const insertAt = currentRowIdx + 1;
    newLines.splice(insertAt, 0, emptyRow);
    newCursorRow = insertAt;
  } else if (command === "table-delete-row") {
    // Don't delete header (row 0) or separator (row 1)
    if (lines.length <= 2 || currentRowIdx <= 1 || isSeparatorRow(lines[currentRowIdx] ?? "")) {
      return false;
    }
    newLines.splice(currentRowIdx, 1);
    newCursorRow = Math.max(2, currentRowIdx - 1);
  } else if (command === "table-insert-column") {
    newCursorCol = currentColIdx + 1;
    newLines = newLines.map((line, rowIdx) => {
      const cells = splitCells(line);
      const cell = rowIdx === 1 ? "---" : " ";
      cells.splice(newCursorCol, 0, cell);
      return buildRow(cells);
    });
  } else if (command === "table-delete-column") {
    const colCount = splitCells(lines[0] ?? "").length;
    if (colCount <= 1) return false;
    const cursorCol = Math.max(0, Math.min(colCount - 1, currentColIdx));
    newLines = newLines.map((line) => {
      const cells = splitCells(line);
      if (cells.length > 1) cells.splice(cursorCol, 1);
      return buildRow(cells);
    });
    newCursorCol = Math.max(0, Math.min(cursorCol, colCount - 2));
  } else {
    return false;
  }

  const newText = newLines.join("\n");
  const cursorRow = newLines[Math.max(0, Math.min(newCursorRow, newLines.length - 1))] ?? "";
  const cursor = startPos + rowOffset(newLines, newCursorRow) + cellOffset(cursorRow, newCursorCol);
  view.dispatch({
    changes: { from: startPos, to: endPos, insert: newText },
    selection: { anchor: cursor },
    scrollIntoView: true,
  });
  return true;
}

// ---------------------------------------------------------------------------
// Main dispatch
// ---------------------------------------------------------------------------

export function runCommandCM6(view: EditorView, command: EditorCommand, value = ""): boolean {
  // ── Inline marks ────────────────────────────────────────────────────────
  if (command === "bold") return wrapInline(view, "**", "**");
  if (command === "italic") return wrapInline(view, "*", "*");
  if (command === "highlight") return wrapInline(view, "==", "==");
  if (command === "strike") return wrapInline(view, "~~", "~~");
  if (command === "code") return wrapInline(view, "`", "`");

  if (command === "link") {
    const { from, to } = view.state.selection.main;
    const sel = from === to ? "link" : view.state.doc.sliceString(from, to);
    const href = value || "https://";
    const text = `[${sel}](${href})`;
    const hrefFrom = from + sel.length + 3;
    view.dispatch({
      changes: { from, to, insert: text },
      selection: { anchor: hrefFrom, head: hrefFrom + href.length },
      scrollIntoView: true,
    });
    return true;
  }

  if (command === "image-edit") {
    const { from, to } = view.state.selection.main;
    const sel = from === to ? "alt" : view.state.doc.sliceString(from, to);
    const src = value || "src";
    const text = `![${sel}](${src})`;
    const srcFrom = from + sel.length + 4;
    view.dispatch({
      changes: { from, to, insert: text },
      selection: { anchor: srcFrom, head: srcFrom + src.length },
      scrollIntoView: true,
    });
    return true;
  }

  // ── Block inserts ────────────────────────────────────────────────────────
  if (command === "code-block") {
    const { from, to } = view.state.selection.main;
    const lang = value || "";
    const body = from === to ? "" : view.state.doc.sliceString(from, to);
    const template = `\`\`\`${lang}\n${body}\n\`\`\``;
    insertBlock(view, template, lang.length + 4 + body.length);
    return true;
  }

  if (command === "insert-table") {
    insertBlock(view, "| Column 1 | Column 2 |\n| --- | --- |\n|  |  |", 2);
    return true;
  }

  if (command === "insert-math-block") {
    insertBlock(view, "$$\n\n$$", 3);
    return true;
  }

  if (command === "insert-toc") {
    insertBlock(view, "[toc]", 5);
    return true;
  }

  if (command === "insert-org-env") {
    const kind = (value || "note").trim() || "note";
    const open = `#+begin ${kind}`;
    insertBlock(view, `${open}\n\n#+end ${kind}`, open.length + 1);
    return true;
  }

  // ── Utility ──────────────────────────────────────────────────────────────
  if (command === "paragraph-menu") return false;

  if (command === "copy-code") {
    const text = codeBlockAtCursor(view);
    if (text == null) return false;
    if (navigator.clipboard?.writeText) {
      void navigator.clipboard.writeText(text);
      return true;
    }
    return false;
  }

  // ── Table ────────────────────────────────────────────────────────────────
  if (
    command === "table-insert-row" ||
    command === "table-insert-column" ||
    command === "table-delete-row" ||
    command === "table-delete-column"
  ) return runTableCommandCM6(view, command);

  // ── Line prefix commands (heading / blockquote / lists) ──────────────────
  const headingMatch = command.match(/^heading-([1-6])$/);
  if (headingMatch) {
    const level = Number(headingMatch[1]);
    return mutateCurrentLine(view, (line) =>
      `${"#".repeat(level)} ${line.replace(/^\s{0,3}#{1,6}\s+/, "")}`);
  }

  if (command === "blockquote") {
    return mutateCurrentLine(view, (line) => line.startsWith("> ") ? line : `> ${line}`);
  }
  if (command === "bullet-list") {
    return mutateCurrentLine(view, (line) => `- ${line.replace(LIST_PREFIX_RE, "")}`);
  }
  if (command === "ordered-list") {
    return mutateCurrentLine(view, (line) => `1. ${line.replace(LIST_PREFIX_RE, "")}`);
  }
  if (command === "task-list") {
    return mutateCurrentLine(view, (line) => `- [ ] ${line.replace(LIST_PREFIX_RE, "")}`);
  }

  return false;
}

// ---------------------------------------------------------------------------
// Quick-insert context + registry
// ---------------------------------------------------------------------------

function buildQuickInsertContext(view: EditorView, query: string): QuickInsertContext {
  const { from } = view.state.selection.main;
  const doc = view.state.doc;
  const maxChars = 1200;
  const before = doc.sliceString(Math.max(0, from - maxChars), from);
  const after = doc.sliceString(from, Math.min(doc.length, from + maxChars));
  return {
    query,
    block: getBlockContextCM6(view),
    before,
    after,
    sourceMode: false,
  };
}

export type QuickInsertRegistry = {
  register(provider: QuickInsertProvider): () => void;
  getItems(view: EditorView, query?: string): QuickInsertItem[];
  run(view: EditorView, item: QuickInsertItem): boolean;
};

export function createQuickInsertRegistry(): QuickInsertRegistry {
  const providers = new Set<QuickInsertProvider>();

  return {
    register(provider) {
      providers.add(provider);
      return () => providers.delete(provider);
    },

    getItems(view, query = "") {
      const ctx = buildQuickInsertContext(view, query);
      const items: QuickInsertItem[] = [
        ...builtInQuickInsertProvider(ctx),
        ...Array.from(providers).flatMap((p) => {
          try { return Array.from(p(ctx)).filter((item) => quickMatches(item, query)); }
          catch { return []; }
        }),
      ];
      const byId = new Map<string, QuickInsertItem>();
      for (const item of items) if (!byId.has(item.id)) byId.set(item.id, item);
      return [...byId.values()].slice(0, 18);
    },

    run(view, item) {
      if (item.markdown != null) {
        const { from, to } = view.state.selection.main;
        const md = item.markdown;
        const cursorOffset = item.select === "start" ? 0 : md.length;
        view.dispatch({
          changes: { from, to, insert: md },
          selection: { anchor: from + cursorOffset },
          scrollIntoView: true,
        });
        return true;
      }
      if (item.command) return runCommandCM6(view, item.command, item.value);
      return false;
    },
  };
}
