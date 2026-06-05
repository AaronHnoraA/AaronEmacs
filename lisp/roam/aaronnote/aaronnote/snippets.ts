import type { Editor } from "../src/lib.ts";
import type { SnippetSummary } from "./types.ts";

export type SnippetTabstop = {
  index: number;
  from: number;
  to: number;
  primary: boolean;
  text?: string;
};

export type ParsedSnippet = {
  text: string;
  tabstops: SnippetTabstop[];
};

type SnippetFrame = {
  stops: SnippetTabstop[];
  order: number[];
  cursor: number;
  activeIndex: number | null;
};

function normalizeSnippetBody(body: string): string {
  return body.replace(/(^|\n)([ \t]*\$\$[\s\S]*?\n[ \t]*\$\$)\n(\$0)$/, "$1$2$3");
}

function sortedStopIndexes(stops: SnippetTabstop[]): number[] {
  const indexes = [...new Set(stops.map((stop) => stop.index))];
  return indexes.sort((a, b) => {
    if (a === 0) return 1;
    if (b === 0) return -1;
    return a - b;
  });
}

function mapPointThroughReplacement(point: number, from: number, to: number, newSize: number): number {
  const delta = newSize - (to - from);
  if (point <= from) return point;
  if (point >= to) return point + delta;
  return from + newSize;
}

function mapSelectionThroughReplacement(
  selection: { from: number; to: number },
  from: number,
  to: number,
  newSize: number,
): { from: number; to: number } {
  return {
    from: mapPointThroughReplacement(selection.from, from, to, newSize),
    to: mapPointThroughReplacement(selection.to, from, to, newSize),
  };
}

export function expandSnippetBody(snippet: SnippetSummary): ParsedSnippet {
  const body = normalizeSnippetBody(snippet.body ?? "");
  const values = new Map<number, string>();
  const tabstops: SnippetTabstop[] = [];
  let text = "";

  function valueFor(index: number, fallback: string): string {
    if (!values.has(index)) values.set(index, fallback);
    return values.get(index) ?? "";
  }

  function pushTabstop(index: number, value: string): void {
    const from = text.length;
    text += value;
    tabstops.push({ index, from, to: text.length, primary: false, text: value });
  }

  function parseChoiceOptions(raw: string): string[] {
    return raw.split(",").map((x) => x.trim()).filter(Boolean);
  }

  function findChoiceEnd(source: string, start: number): number {
    for (let pos = start; pos < source.length - 1; pos++) {
      if (source[pos] === "|" && source[pos + 1] === "}") return pos;
    }
    return -1;
  }

  function skipTemplate(source: string, start = 0, endChar = ""): number {
    let i = start;
    while (i < source.length) {
      if (endChar && source[i] === endChar) return i + 1;
      if (source[i] === "$" && source[i + 1] === "{") {
        let pos = i + 2;
        let digits = "";
        while (/\d/.test(source[pos] ?? "")) {
          digits += source[pos];
          pos++;
        }
        if (!digits) {
          i++;
          continue;
        }
        const marker = source[pos];
        if (marker === "}") {
          i = pos + 1;
          continue;
        }
        if (marker === "|") {
          const end = findChoiceEnd(source, pos + 1);
          if (end >= 0) {
            i = end + 2;
            continue;
          }
        }
        if (marker === ":") {
          i = skipTemplate(source, pos + 1, "}");
          continue;
        }
      }
      i++;
    }
    return i;
  }

  function parseTemplate(source: string, start = 0, endChar = ""): number {
    let i = start;
    while (i < source.length) {
      if (endChar && source[i] === endChar) return i + 1;

      if (source[i] !== "$") {
        text += source[i];
        i++;
        continue;
      }

      if (source[i + 1] === "{") {
        let pos = i + 2;
        let digits = "";
        while (/\d/.test(source[pos] ?? "")) {
          digits += source[pos];
          pos++;
        }
        if (!digits) {
          text += source[i];
          i++;
          continue;
        }

        const index = Number(digits);
        const marker = source[pos];
        if (marker === "}") {
          pushTabstop(index, index === 0 ? "" : valueFor(index, ""));
          i = pos + 1;
          continue;
        }
        if (marker === "|") {
          const end = findChoiceEnd(source, pos + 1);
          if (end >= 0) {
            const options = parseChoiceOptions(source.slice(pos + 1, end));
            pushTabstop(index, valueFor(index, options[0] ?? ""));
            i = end + 2;
            continue;
          }
        }
        if (marker === ":") {
          if (values.has(index)) {
            const end = skipTemplate(source, pos + 1, "}");
            pushTabstop(index, values.get(index) ?? "");
            i = end;
            continue;
          }
          const from = text.length;
          const end = parseTemplate(source, pos + 1, "}");
          const value = text.slice(from);
          values.set(index, value);
          tabstops.push({ index, from, to: text.length, primary: false, text: value });
          i = end;
          continue;
        }
        text += source[i];
        i++;
        continue;
      }

      let pos = i + 1;
      let digits = "";
      while (/\d/.test(source[pos] ?? "")) {
        digits += source[pos];
        pos++;
      }
      if (digits) {
        const index = Number(digits);
        pushTabstop(index, index === 0 ? "" : valueFor(index, ""));
        i = pos;
        continue;
      }

      text += source[i];
      i++;
    }
    return i;
  }

  parseTemplate(body);

  const seen = new Set<number>();
  for (const stop of tabstops) {
    if (!seen.has(stop.index)) {
      stop.primary = true;
      seen.add(stop.index);
    }
  }

  return { text, tabstops };
}

function nodeContains(root: HTMLElement, node: Node): boolean {
  return node === root || root.contains(node);
}

function textLength(node: Node): number {
  return node.textContent?.length ?? 0;
}

function textOffsetIn(root: HTMLElement, boundaryNode: Node, boundaryOffset: number): number | null {
  let offset = 0;
  let found = false;

  function visit(node: Node): void {
    if (found) return;
    if (node === boundaryNode) {
      if (node.nodeType === Node.TEXT_NODE) {
        offset += Math.max(0, Math.min(boundaryOffset, node.textContent?.length ?? 0));
      } else {
        const children = Array.from(node.childNodes);
        for (const child of children.slice(0, Math.max(0, boundaryOffset))) {
          offset += textLength(child);
        }
      }
      found = true;
      return;
    }
    if (node.nodeType === Node.TEXT_NODE) {
      offset += node.textContent?.length ?? 0;
      return;
    }
    for (const child of Array.from(node.childNodes)) visit(child);
  }

  visit(root);
  return found ? offset : null;
}

function domPointAtTextOffset(root: HTMLElement, target: number): { node: Node; offset: number } {
  const clamped = Math.max(0, target);
  let offset = 0;
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  let current = walker.nextNode();
  while (current) {
    const len = current.textContent?.length ?? 0;
    if (offset + len >= clamped) {
      return { node: current, offset: clamped - offset };
    }
    offset += len;
    current = walker.nextNode();
  }
  return { node: root, offset: root.childNodes.length };
}

export function insertExpandedSnippetIntoContentEditable(
  root: HTMLElement,
  snippet: SnippetSummary,
  deleteBefore = 0,
): boolean {
  const { text } = expandSnippetBody(snippet);
  if (!text) return false;
  const selection = root.ownerDocument.defaultView?.getSelection() ?? window.getSelection();
  if (!selection || selection.rangeCount === 0) return false;
  const range = selection.getRangeAt(0);
  if (!nodeContains(root, range.startContainer) || !nodeContains(root, range.endContainer)) return false;

  const startOffset = textOffsetIn(root, range.startContainer, range.startOffset);
  if (startOffset == null) return false;
  const replaceFrom = domPointAtTextOffset(root, startOffset - deleteBefore);
  const replaceRange = range.cloneRange();
  replaceRange.setStart(replaceFrom.node, replaceFrom.offset);
  replaceRange.deleteContents();

  const textNode = root.ownerDocument.createTextNode(text);
  replaceRange.insertNode(textNode);
  replaceRange.setStart(textNode, text.length);
  replaceRange.collapse(true);
  selection.removeAllRanges();
  selection.addRange(replaceRange);
  return true;
}

export class SnippetSession {
  private frames: SnippetFrame[] = [];
  private readonly editor: Editor;

  constructor(editor: Editor) {
    this.editor = editor;
  }

  clear(): void {
    this.frames = [];
  }

  active(): boolean {
    return this.frames.length > 0;
  }

  insert(snippet: SnippetSummary, deleteBefore = 0): boolean {
    const { text, tabstops } = expandSnippetBody(snippet);
    if (!text) return false;
    const parent = this.topFrame();
    if (parent) this.syncActive(parent, false);
    const selection = this.editor.getSelection();
    const replaceFrom = Math.max(0, selection.from - deleteBefore);
    const replaceTo = selection.to;
    const inserted = this.editor.insertText(text, deleteBefore);
    this.mapReplacement(replaceFrom, replaceTo, inserted.to - inserted.from);
    const stops = this.mapInsertedStops(tabstops, inserted.from);
    if (stops.length === 0) return true;
    const frame: SnippetFrame = {
      stops: stops.map((stop) => ({
        ...stop,
      })),
      order: sortedStopIndexes(tabstops),
      cursor: -1,
      activeIndex: null,
    };
    this.frames.push(frame);
    if (!this.next()) this.frames.pop();
    return true;
  }

  next(): boolean {
    let childCompleted = false;
    while (this.frames.length > 0) {
      const frame = this.topFrame()!;
      this.syncActive(frame, childCompleted);
      childCompleted = false;
      frame.cursor += 1;
      if (frame.cursor >= frame.order.length) {
        this.frames.pop();
        childCompleted = true;
        continue;
      }
      const index = frame.order[frame.cursor]!;
      const target = frame.stops.find((stop) => stop.index === index && stop.primary)
        ?? frame.stops.find((stop) => stop.index === index);
      if (!target) continue;
      frame.activeIndex = index;
      this.selectStop(target);
      return true;
    }
    return false;
  }

  previous(): boolean {
    while (this.frames.length > 0) {
      const frame = this.topFrame()!;
      this.syncActive(frame, false);
      frame.cursor -= 1;
      if (frame.cursor < 0) {
        frame.cursor = -1;
        frame.activeIndex = null;
        return false;
      }
      const index = frame.order[frame.cursor]!;
      const target = frame.stops.find((stop) => stop.index === index && stop.primary)
        ?? frame.stops.find((stop) => stop.index === index);
      if (!target) continue;
      frame.activeIndex = index;
      this.selectStop(target);
      return true;
    }
    return false;
  }

  private topFrame(): SnippetFrame | null {
    return this.frames[this.frames.length - 1] ?? null;
  }

  private syncActive(frame: SnippetFrame, preferStoredEnd: boolean): void {
    if (frame.activeIndex == null) return;
    const primary = frame.stops.find((stop) => stop.index === frame.activeIndex && stop.primary);
    if (!primary) return;

    const selection = this.editor.getSelection();
    let restoreSelection = selection;
    const selectionEnd = Math.max(selection.from, selection.to);
    const selectionInsidePrimary = selection.from >= primary.from && selectionEnd <= primary.to;
    const replacementEnd = preferStoredEnd
      ? primary.to
      : selectionInsidePrimary
        ? selectionEnd
        : Math.max(primary.to, selectionEnd);
    const value = this.editor.textBetween(primary.from, replacementEnd);
    const oldTo = primary.to;
    const oldText = primary.text;
    const oldSize = oldTo - primary.from;
    const newSize = value.length;
    const delta = newSize - oldSize;
    if (oldText != null && value !== oldText) this.dropStopsInside(frame, primary, oldTo);
    primary.text = value;
    primary.to = primary.from + newSize;

    if (delta !== 0) this.shiftStopsAfter(primary.from, delta, primary);

    const mirrors = frame.stops
      .filter((stop) => stop.index === frame.activeIndex && stop !== primary)
      .sort((a, b) => b.from - a.from);
    for (const mirror of mirrors) {
      const mirrorOldSize = mirror.to - mirror.from;
      const oldMirrorFrom = mirror.from;
      const oldMirrorTo = mirror.to;
      const inserted = this.editor.replaceRange(mirror.from, mirror.to, value, "end");
      const mirrorDelta = value.length - mirrorOldSize;
      mirror.from = inserted.from;
      mirror.to = inserted.to;
      mirror.text = value;
      if (mirrorDelta !== 0) {
        restoreSelection = mapSelectionThroughReplacement(restoreSelection, oldMirrorFrom, oldMirrorTo, value.length);
        this.mapReplacement(oldMirrorFrom, oldMirrorTo, value.length, mirror);
      }
    }
    this.editor.setSelection(restoreSelection.from, restoreSelection.to);
  }

  private dropStopsInside(frame: SnippetFrame, primary: SnippetTabstop, oldTo: number): void {
    frame.stops = frame.stops.filter((stop) => {
      if (stop === primary) return true;
      return !(stop.from >= primary.from && stop.to <= oldTo);
    });
  }

  private shiftStopsAfter(anchor: number, delta: number, except: SnippetTabstop): void {
    for (const frame of this.frames) {
      for (const stop of frame.stops) {
        if (stop === except) continue;
        if (stop.from > anchor) {
          stop.from += delta;
          stop.to += delta;
        } else if (stop.to > anchor) {
          stop.to += delta;
        }
      }
    }
  }

  private mapReplacement(from: number, to: number, newSize: number, except?: SnippetTabstop): void {
    const delta = newSize - (to - from);
    for (const frame of this.frames) {
      for (const stop of frame.stops) {
        if (stop === except) continue;
        if (stop.to <= from) continue;
        if (stop.from >= to) {
          stop.from += delta;
          stop.to += delta;
          continue;
        }
        stop.from = Math.min(stop.from, from);
        stop.to = Math.max(stop.from + newSize, stop.to + delta);
      }
    }
  }

  private selectStop(stop: SnippetTabstop): void {
    this.editor.setSelection(stop.from, stop.to);
  }

  private mapInsertedStops(
    tabstops: SnippetTabstop[],
    insertedFrom: number,
  ): SnippetTabstop[] {
    return tabstops.map((stop) => {
      return {
        ...stop,
        from: insertedFrom + stop.from,
        to: insertedFrom + stop.to,
      };
    });
  }
}

export function snippetLabel(snippet: SnippetSummary): string {
  return snippet.key || snippet.name || "snippet";
}

export function snippetDetail(snippet: SnippetSummary): string {
  const kind = snippet.kind ? `kind:${snippet.kind}` : "";
  return [snippet.name, snippet.mode, kind, snippet.group].filter(Boolean).join(" / ");
}

export function snippetScore(snippet: SnippetSummary, query: string): number {
  const key = (snippet.key ?? "").toLowerCase();
  const name = (snippet.name ?? "").toLowerCase();
  const mode = (snippet.mode ?? "").toLowerCase();
  const group = (snippet.group ?? "").toLowerCase();
  const kind = (snippet.kind ?? "").toLowerCase();
  if (key === query) return 0;
  if (key.startsWith(query)) return 1;
  if (name.startsWith(query)) return 2;
  if (key.includes(query)) return 3;
  if (name.includes(query)) return 4;
  if (mode.includes(query) || group.includes(query) || kind.includes(query)) return 5;
  return Number.POSITIVE_INFINITY;
}

export function matchingSnippetsForPrefix(
  snippets: readonly SnippetSummary[],
  prefix: string,
  options: { mode?: string; kind?: string; limit?: number } = {},
): SnippetSummary[] {
  const query = prefix.toLowerCase();
  const mode = options.mode || "";
  const activeKind = (options.kind || "").toLowerCase();
  const limit = Math.max(1, options.limit ?? 10);
  return snippets
    .filter((snippet) => !mode || snippet.mode === mode)
    .filter((snippet) => {
      const snippetKind = (snippet.kind || "").toLowerCase();
      return !snippetKind || snippetKind === activeKind;
    })
    .map((snippet) => ({ snippet, score: snippetScore(snippet, query) }))
    .filter((item) => Number.isFinite(item.score))
    .sort((a, b) => {
      if (a.score !== b.score) return a.score - b.score;
      return snippetLabel(a.snippet).localeCompare(snippetLabel(b.snippet));
    })
    .slice(0, limit)
    .map((item) => item.snippet);
}
