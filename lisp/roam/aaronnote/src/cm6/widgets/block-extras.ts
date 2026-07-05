/**
 * Phase 6 — Regex-scanned block widgets for the CM6 kernel.
 *
 * CM6 constraint: block:true decorations must come from StateField, not ViewPlugin.
 * This entire module uses StateField (full-doc scan).
 *
 * Three widget types:
 *
 *   [toc]  (case-insensitive, own line)
 *   #+begin <type> … #+end <type>  (org-mode style blocks)
 *   --- … ---  (YAML front matter at document start)
 *   --- / *** / ___  (horizontal rule)
 */

import {
  Decoration,
  EditorView,
  ViewPlugin,
  type DecorationSet,
  type ViewUpdate,
} from "@codemirror/view";
import { MeasuredWidget } from "./measured-widget.ts";
import { shortHash } from "./measured-observer.ts";
import { StateEffect, StateField, type ChangeSet, type EditorState, type Extension, type Text } from "@codemirror/state";
import type { Range as CMRange } from "@codemirror/state";
import {
  getBlockMathRanges,
  mergeOverlappingRanges,
  positionInsideAnyRange,
  rangeOverlapsAny,
} from "../math-ranges.ts";
import {
  changesMightAffectFencedCodeRanges,
  fencedCodeRangesExtension,
  getFencedCodeRanges,
} from "../code-ranges.ts";
import {
  metaEntryMap,
  metaRoamIndexed,
  metaTags,
  parseMetaEntries,
  renderMarkdownInlineHTML,
  renderMarkdownHTML,
  showMetaTag,
} from "../../render-html.ts";
import { applyImageLayout, imageLayoutFromAttrs, readImageTrailingAttrs, type ImageLayoutAttrs } from "../../image-attrs.ts";
import { supportedDiagramLang } from "../../diagram-langs.ts";
import { api } from "../../../aaronnote/api-client.ts";
import { tocIndexFromState, type MarkdownHeading } from "../toc-index.ts";
import { scanInlineCommands } from "../../command-syntax.ts";
import { semanticOutlineFromCommand, type SemanticOutline } from "../../semantic-outline.ts";
import { highlightCodeForEditor } from "../../code-highlight-async.ts";

// ---------------------------------------------------------------------------
// TOC fold state (session-level, not editor history)
// ---------------------------------------------------------------------------

export const tocFoldEffect = StateEffect.define<{ key: string; folded: boolean }>();

function tocFoldReducer(state: Map<string, boolean>, effects: readonly StateEffect<unknown>[]): Map<string, boolean> {
  let next: Map<string, boolean> | undefined;
  for (const effect of effects) {
    if (effect.is(tocFoldEffect)) {
      if (!next) next = new Map(state);
      if (effect.value.folded) next.set(effect.value.key, true);
      else next.delete(effect.value.key);
    }
  }
  return next ?? state;
}

const tocFoldField = StateField.define<Map<string, boolean>>({
  create: () => new Map(),
  update(state, tr) { return tocFoldReducer(state, tr.effects); },
});

// ---------------------------------------------------------------------------
// Regexes / parsers
// ---------------------------------------------------------------------------

// [toc] alone on a line
const TOC_LINE_RE = /^[ \t]*\[toc\][ \t]*$/im;

const HR_LINE_RE = /^[ \t]{0,3}((?:-[ \t]*){3,}|(?:\*[ \t]*){3,}|(?:_[ \t]*){3,})$/;

export interface OrgEnvBlock {
  from: number;
  to: number;
  openFrom: number;
  openTo: number;
  bodyFrom: number;
  bodyTo: number;
  closeFrom: number;
  closeTo: number;
  kind: string;
  title: string;
  body: string;
  titleAnchor: number;
  depth: number;
}

export interface OrgEnvContext {
  kind: string;
  depth: number;
}

interface OrgEnvOpenLineInfo {
  kind: string;
  title: string;
  titleAnchor: number;
}

interface OrgEnvTitlePatch {
  blocks: readonly OrgEnvBlock[];
  newBlock: OrgEnvBlock;
}

declare global {
  interface Window {
    AaronnoteCurrentFile?: () => string;
    AaronnoteResolveAssetUrl?: (src: string) => string;
  }
}

const ORG_ENV_OPEN_LINE_RE = /^([ \t]*#\+\s*begin\s+)(\S+)(?:([ \t]+)([^\n]*?))?[ \t]*$/i;
const ORG_ENV_SCAN_OPEN_RE = /^[ \t]*#\+\s*begin\s+(\S+)(?:[ \t]+([^\n]*))?[ \t]*$/i;

function orgEnvBoundaryRe(kind: string, boundary: "begin" | "end"): RegExp {
  const escapedKind = kind.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  if (boundary === "begin") return new RegExp(`^[ \\t]*#\\+\\s*begin\\s+${escapedKind}(?:\\s|$)`, "i");
  return new RegExp(`^[ \\t]*#\\+\\s*end\\s+${escapedKind}[ \\t]*$`, "i");
}

function combineExcludedRanges(
  ...lists: Array<ReadonlyArray<{ from: number; to: number }>>
): Array<{ from: number; to: number }> {
  return mergeOverlappingRanges(lists.flatMap((list) => Array.from(list)));
}

function blockExtraExcludedRanges(state: EditorState): Array<{ from: number; to: number }> {
  return combineExcludedRanges(getBlockMathRanges(state), getFencedCodeRanges(state));
}

// Depth-aware scanner: handles nested #+begin <kind> … #+end <kind>.
function scanOrgEnvBlocks(
  text: string,
  depthLevel = 0,
  baseOffset = 0,
  excludedRanges: ReadonlyArray<{ from: number; to: number }> = [],
): OrgEnvBlock[] {
  const results: OrgEnvBlock[] = [];
  let i = 0;
  while (i < text.length) {
    // Advance to the start of the next line
    const lineEnd = text.indexOf("\n", i);
    const lineEndPos = lineEnd === -1 ? text.length : lineEnd;
    if (positionInsideAnyRange(baseOffset + i, excludedRanges)) { i = lineEndPos + 1; continue; }
    const line = text.slice(i, lineEndPos);
    const openMatch = ORG_ENV_SCAN_OPEN_RE.exec(line);
    if (!openMatch) { i = lineEndPos + 1; continue; }

    const kind = openMatch[1].toLowerCase();
    if (kind === "lean4") { i = lineEndPos + 1; continue; }
    const title = (openMatch[2] ?? "").trim();
    const blockFrom = i;
    const bodyStart = lineEndPos + 1;

    // Find matching #+end kind at this depth level
    const openRe = orgEnvBoundaryRe(kind, "begin");
    const closeRe = orgEnvBoundaryRe(kind, "end");

    let depth = 1, pos = bodyStart, closeFrom = -1, closeTo = -1;
    while (pos < text.length) {
      const nl = text.indexOf("\n", pos);
      const nextEnd = nl === -1 ? text.length : nl;
      if (positionInsideAnyRange(baseOffset + pos, excludedRanges)) { pos = nextEnd + 1; continue; }
      const cur = text.slice(pos, nextEnd);
      if (closeRe.test(cur)) { depth--; if (depth === 0) { closeFrom = pos; closeTo = nextEnd; break; } }
      else if (openRe.test(cur)) depth++;
      pos = nextEnd + 1;
    }

    if (closeFrom < 0) { i = lineEndPos + 1; continue; }

    const titleIndex = openMatch[2] ? line.indexOf(openMatch[2]) : -1;
    const body = text.slice(bodyStart, closeFrom);
    results.push({
      from: blockFrom,
      to: closeTo,
      openFrom: blockFrom,
      openTo: lineEndPos,
      bodyFrom: bodyStart,
      bodyTo: closeFrom,
      closeFrom,
      closeTo,
      kind,
      title,
      body,
      titleAnchor: titleIndex >= 0 ? blockFrom + titleIndex : lineEndPos,
      depth: depthLevel,
    });
    if (kind !== "meta") {
      for (const nested of scanOrgEnvBlocks(body, depthLevel + 1, baseOffset + bodyStart, excludedRanges)) {
        results.push({
          ...nested,
          from: bodyStart + nested.from,
          to: bodyStart + nested.to,
          openFrom: bodyStart + nested.openFrom,
          openTo: bodyStart + nested.openTo,
          bodyFrom: bodyStart + nested.bodyFrom,
          bodyTo: bodyStart + nested.bodyTo,
          closeFrom: bodyStart + nested.closeFrom,
          closeTo: bodyStart + nested.closeTo,
          titleAnchor: bodyStart + nested.titleAnchor,
        });
      }
    }
    i = closeTo + 1;
  }
  return results.sort((a, b) => a.from - b.from || a.to - b.to);
}

function setSourceRange(el: HTMLElement, from: number, to: number): void {
  el.dataset.cmSourceFrom = String(from);
  el.dataset.cmSourceTo = String(to);
}

export function orgEnvExitTarget(state: EditorState): number | null {
  const pos = state.selection.main.from;
  const containing = orgEnvBlocksFromState(state)
    .filter((block) => block.openFrom < pos && pos <= block.closeTo)
    .sort((a, b) => (a.to - a.from) - (b.to - b.from))[0];
  if (!containing) return null;
  return state.doc.sliceString(containing.closeTo, containing.closeTo + 1) === "\n"
    ? containing.closeTo + 1
    : containing.closeTo;
}

export function orgEnvContextForRange(state: EditorState, from: number, to: number): OrgEnvContext | null {
  const containing = orgEnvBlocksFromState(state)
    .filter((block) => (
      block.kind !== "meta"
      && block.bodyFrom <= from
      && to <= block.bodyTo
    ))
    .sort((a, b) => (a.to - a.from) - (b.to - b.from))[0];
  return containing ? { kind: containing.kind, depth: containing.depth } : null;
}

function buildOrgEnvSource(kind: string, title: string, body: string): string {
  const bodyWithCloseNewline = body.endsWith("\n") ? body : `${body}\n`;
  return `${buildOrgEnvOpenLine(kind, title)}\n${bodyWithCloseNewline}#+end ${kind}`;
}

function buildOrgEnvOpenLine(kind: string, title: string): string {
  return title.trim().length > 0 ? `#+begin ${kind} ${title.trim()}` : `#+begin ${kind}`;
}

function parseOrgEnvOpenLine(line: string): OrgEnvOpenLineInfo | null {
  const match = ORG_ENV_OPEN_LINE_RE.exec(line);
  if (!match) return null;
  const rawTitle = match[4] ?? "";
  const title = rawTitle.trim();
  const titleAnchor = title.length > 0
    ? match[1].length + match[2].length + (match[3] ?? "").length + Math.max(0, rawTitle.search(/\S/))
    : line.length;
  return {
    kind: match[2].toLowerCase(),
    title,
    titleAnchor,
  };
}

function stopEditorPropagation(event: Event): void {
  event.stopPropagation();
}

function renderDiagramPreview(source: string, lang: string, div: HTMLElement): void {
  const key = `mermaid\n${lang}\n${source.trim()}`;
  div.dataset.diagramRenderKey = key;
  div.textContent = "Loading diagram renderer...";
  void import("../../diagram-render.ts")
    .then(({ renderMermaidLazy }) => {
      if (div.dataset.diagramRenderKey !== key) return;
      renderMermaidLazy(source, div, (err) => {
        div.classList.add("cm-diagram-error");
        div.textContent = err;
      }, { lang });
    })
    .catch((err: unknown) => {
      if (div.dataset.diagramRenderKey !== key) return;
      div.classList.add("cm-diagram-error");
      div.textContent = err instanceof Error ? err.message : String(err);
    });
}

function enhanceRenderedMarkdown(root: HTMLElement): void {
  root.querySelectorAll<HTMLElement>("pre > code[class*='language-']").forEach((code) => {
    const langClass = Array.from(code.classList).find((cls) => cls.startsWith("language-")) ?? "";
    const lang = langClass.slice("language-".length);
    if (!supportedDiagramLang(lang)) return;
    const pre = code.parentElement;
    if (!(pre instanceof HTMLPreElement)) return;
    const div = document.createElement("div");
    div.className = "cm-mermaid-block-preview";
    renderDiagramPreview(code.textContent ?? "", lang, div);
    pre.replaceWith(div);
  });
}

function stopInteractiveWidgetEvents(root: HTMLElement): void {
  for (const type of ["mousedown", "mouseup", "click", "dblclick", "keydown", "keyup", "beforeinput", "input"]) {
    root.addEventListener(type, stopEditorPropagation);
  }
}

type TikzAssetResult = {
  ok?: boolean;
  markdownPath?: string;
  message?: string;
};

type CeilKernelSpec = { name: string; displayName?: string; language?: string };

type CeilMeta = {
  kernel: string;
  session: string;
  id: string;
  language: string;
  attrs: Array<{ key: string; value: string }>;
  changed: boolean;
};

type CeilCellContext = {
  cellId: string;
  kernel: string;
  session: string;
  language: string;
  code: string;
};

type CeilExecutionResult = {
  ok?: boolean;
  cellId?: string;
  kernel?: string;
  session?: string;
  status?: string;
  executionCount?: number | null;
  outputs?: Array<Record<string, unknown>>;
  message?: string;
};

const clearTikzDirtyEffect = StateEffect.define<string>();
const tikzAssetCache = new Map<string, Promise<TikzAssetResult>>();
const tikzRenderedSourceByAsset = new Map<string, string>();
const tikzPendingSourceByAsset = new Map<string, string>();
const DEFAULT_CEIL_KERNEL = "python3";
const DEFAULT_CEIL_SESSION = "default";
const DEFAULT_CEIL_LANGUAGE = "python";
let ceilKernelsCache: CeilKernelSpec[] | null = null;
let ceilKernelsPending: Promise<CeilKernelSpec[]> | null = null;
const ceilOutputCache = new Map<string, CeilExecutionResult>();

function setBoundedMap<K, V>(map: Map<K, V>, key: K, value: V, limit = 128): void {
  map.set(key, value);
  if (map.size <= limit) return;
  const oldest = map.keys().next();
  if (!oldest.done) map.delete(oldest.value);
}

function pad2(value: number): string {
  return String(value).padStart(2, "0");
}

function tikzTimestamp(date = new Date()): string {
  return [
    String(date.getFullYear()),
    pad2(date.getMonth() + 1),
    pad2(date.getDate()),
    "-",
    pad2(date.getHours()),
    pad2(date.getMinutes()),
    pad2(date.getSeconds()),
  ].join("");
}

function nextTikzTimestamp(previous: string): string {
  const next = tikzTimestamp();
  return next === previous ? tikzTimestamp(new Date(Date.now() + 1000)) : next;
}

function tikzGeneratedId(timestamp: string): string {
  return `tikz-${timestamp}`;
}

function splitTikzTitle(title: string): { head: string; attrsRaw: string; layout: ImageLayoutAttrs } {
  const raw = String(title || "").trim();
  const open = raw.indexOf("{");
  if (open < 0) return { head: raw, attrsRaw: "", layout: imageLayoutFromAttrs({}) };
  const trailing = readImageTrailingAttrs(raw, open);
  if (!trailing || raw.slice(trailing.to).trim()) return { head: raw, attrsRaw: "", layout: imageLayoutFromAttrs({}) };
  return {
    head: raw.slice(0, open).trim(),
    attrsRaw: trailing.raw,
    layout: imageLayoutFromAttrs(trailing.attrs),
  };
}

function completeTikzTitle(title: string): { id: string; timestamp: string; attrsRaw: string; layout: ImageLayoutAttrs; changed: boolean } {
  const parsed = splitTikzTitle(title);
  const parts = parsed.head.split(/\s+/).filter(Boolean);
  if (parts.length >= 2) return { id: parts[0]!, timestamp: parts[1]!, attrsRaw: parsed.attrsRaw, layout: parsed.layout, changed: false };
  const timestamp = tikzTimestamp();
  const id = parts[0] || tikzGeneratedId(timestamp);
  return { id, timestamp, attrsRaw: parsed.attrsRaw, layout: parsed.layout, changed: true };
}

function tikzDirtyKeyFromTitle(title: string): string {
  const parsed = splitTikzTitle(title);
  return parsed.head.split(/\s+/, 1)[0] || "";
}

function currentNoteFile(): string {
  return window.AaronnoteCurrentFile?.() || "";
}

function cleanCeilToken(value: string, fallback: string): string {
  const clean = String(value || "").trim();
  return clean || fallback;
}

function stripCeilKernelParens(value: string): string {
  const clean = String(value || "").trim();
  const match = /^\(([^()]+)\)$/.exec(clean);
  return (match?.[1] ?? clean).trim();
}

function ceilLanguageForKernel(kernel: string, requested = ""): string {
  const explicit = requested.trim().toLowerCase();
  if (explicit) return explicit;
  const clean = kernel.toLowerCase();
  if (clean.includes("sage")) return "python";
  if (clean.includes("python") || clean === "py" || clean === "python3") return "python";
  if (clean.includes("julia")) return "julia";
  if (clean === "r" || clean.startsWith("ir")) return "r";
  if (clean.includes("bash") || clean.includes("zsh") || clean.includes("shell")) return "bash";
  if (clean.includes("typescript") || clean === "ts") return "typescript";
  if (clean.includes("javascript") || clean === "js" || clean.includes("node")) return "javascript";
  return "python";
}

type CeilCommandRange = {
  from: number;
  to: number;
  argsRaw: string;
  idRaw: string;
};

type CeilCommandMeta = CeilMeta & {
  rawArgs: string;
};

const CEIL_COMMAND_LINE_RE = /^([ \t]*)@@cell(?:[ \t]*\(([^)\n]*)\))?(?:[ \t]+\[([^\]\n]*)\])?[ \t]*$/i;

function parseCeilCommandLine(text: string, lineFrom: number): CeilCommandRange | null {
  const match = CEIL_COMMAND_LINE_RE.exec(text);
  if (!match) return null;
  const leading = match[1]?.length ?? 0;
  return {
    from: lineFrom + leading,
    to: lineFrom + text.length,
    argsRaw: match[2] ?? "",
    idRaw: match[3] ?? "",
  };
}

function ceilCommandGeneratedId(file: string, range: CeilCommandRange): string {
  return `ceil-${shortHash(`${file}\n${range.from}\n${range.argsRaw}\n${range.idRaw}`)}`;
}

function parseCeilCommand(range: CeilCommandRange, file: string): CeilCommandMeta {
  const args = range.argsRaw.split(",").map((part) => part.trim()).filter(Boolean);
  let language = args[0] || DEFAULT_CEIL_LANGUAGE;
  let kernel = args[1] || "";
  let session = args[2] || DEFAULT_CEIL_SESSION;
  if (args.length === 1 && /python3|sage|julia|ir|bash|zsh|node|javascript|typescript/i.test(args[0]!)) {
    kernel = args[0]!;
    language = ceilLanguageForKernel(kernel);
  }
  kernel = cleanCeilToken(stripCeilKernelParens(kernel), DEFAULT_CEIL_KERNEL);
  session = cleanCeilToken(session, DEFAULT_CEIL_SESSION);
  language = ceilLanguageForKernel(kernel, language);
  const id = cleanCeilToken(range.idRaw, ceilCommandGeneratedId(file, range));
  return {
    kernel,
    session,
    id,
    language,
    attrs: [],
    rawArgs: range.argsRaw,
    changed: !range.argsRaw.trim() || !range.idRaw.trim() || args[0] !== language || args[1] !== kernel,
  };
}

function formatCeilCommand(meta: CeilCommandMeta): string {
  const args = meta.session && meta.session !== DEFAULT_CEIL_SESSION
    ? `${meta.language}, ${meta.kernel}, ${meta.session}`
    : `${meta.language}, ${meta.kernel}`;
  return `@@cell(${args}) [${meta.id}]`;
}

function replaceCeilCommandLine(view: EditorView, from: number, insert: string): void {
  const line = view.state.doc.lineAt(from);
  const range = parseCeilCommandLine(line.text, line.from);
  if (!range) return;
  const prefix = line.text.slice(0, range.from - line.from);
  const next = `${prefix}${insert}`;
  if (line.text === next) return;
  view.dispatch({ changes: { from: line.from, to: line.to, insert: next } });
}

function scheduleCeilCommandLineUpdate(view: EditorView, from: number, insert: string): void {
  window.requestAnimationFrame(() => {
    if (!view.dom.isConnected) return;
    replaceCeilCommandLine(view, from, insert);
  });
}

function ceilOutputKey(file: string, meta: CeilMeta, body: string): string {
  return `${file}\0${meta.kernel}\0${meta.session}\0${meta.id}\0${shortHash(body)}`;
}

function sameCeilContext(a: CeilMeta, b: CeilMeta): boolean {
  return a.kernel === b.kernel && a.session === b.session;
}

function ceilCommandCellsForContext(state: EditorState, target: CeilMeta, file: string): CeilCellContext[] {
  const ranges = state.field(blockExtraRangesField, false) ?? scanBlockExtraRanges(state.doc, blockExtraExcludedRanges(state));
  return ranges.ceilCommands
    .map((range) => parseCeilCommand(range, file))
    .filter((meta) => sameCeilContext(meta, target))
    .map((meta) => ({
      cellId: meta.id,
      kernel: meta.kernel,
      session: meta.session,
      language: meta.language,
      code: "",
    }));
}

function highlightedCeilCode(code: string, language: string): HTMLElement {
  const pre = document.createElement("pre");
  const codeEl = document.createElement("code");
  pre.append(codeEl);
  const ranges = highlightCodeForEditor(language, code);
  let pos = 0;
  for (const range of ranges) {
    if (range.from > pos) codeEl.append(document.createTextNode(code.slice(pos, range.from)));
    const span = document.createElement("span");
    span.className = range.className;
    span.textContent = code.slice(range.from, range.to);
    codeEl.append(span);
    pos = range.to;
  }
  if (pos < code.length) codeEl.append(document.createTextNode(code.slice(pos)));
  return pre;
}

function resolveAssetSrc(src: string): string {
  return window.AaronnoteResolveAssetUrl?.(src) ?? src;
}

function ensureTikzAsset(file: string, id: string, timestamp: string, source: string): Promise<TikzAssetResult> {
  const key = `${file}\n${id}\n${timestamp}\n${source}`;
  let existing = tikzAssetCache.get(key);
  if (!existing) {
    existing = api.assets.renderTikz({ file, id, timestamp, source })
      .catch((err: unknown) => ({
        ok: false,
        message: err instanceof Error ? err.message : String(err),
      }));
    tikzAssetCache.set(key, existing);
    if (tikzAssetCache.size > 128) {
      const oldest = tikzAssetCache.keys().next().value as string | undefined;
      if (oldest) tikzAssetCache.delete(oldest);
    }
  }
  return existing;
}

function tikzSourceCacheKey(file: string, id: string): string {
  return `${file}\n${id}`;
}

function scheduleTikzOpenLineUpdate(
  view: EditorView,
  from: number,
  makeTitle: (info: OrgEnvOpenLineInfo) => string | null,
  effects: StateEffect<unknown>[] = [],
): void {
  window.requestAnimationFrame(() => {
    if (!view.dom.isConnected) return;
    const line = view.state.doc.lineAt(from);
    const info = parseOrgEnvOpenLine(line.text);
    if (!info || info.kind !== "tikz") return;
    const title = makeTitle(info);
    if (!title) return;
    view.dispatch({
      changes: {
        from: line.from,
        to: line.to,
        insert: `#+ begin tikz ${title}`,
      },
      effects,
    });
  });
}

// ---------------------------------------------------------------------------
// Widgets
// ---------------------------------------------------------------------------

type TocHeading = MarkdownHeading;

interface BlockExtraRanges {
  toc: Array<{ from: number; to: number }>;
  semanticHeadings: Array<{ from: number; to: number; outline: SemanticOutline }>;
  ceilCommands: CeilCommandRange[];
  hrs: Array<{ from: number; to: number }>;
  frontMatter: { from: number; to: number; body: string } | null;
}

class TocWidget extends MeasuredWidget {
  headings: TocHeading[];
  foldState: ReadonlyMap<string, boolean>;
  signature: string;

  constructor(headings: TocHeading[], foldState: ReadonlyMap<string, boolean>) {
    super();
    this.headings = headings;
    this.foldState = foldState;
    this.signature = tocSignature(headings, foldState);
  }

  protected measureKey(): string { return "toc:" + shortHash(this.signature); }

  protected measureGroupKey(): string {
    const bucket = Math.min(8, Math.ceil(this.headings.length / 8));
    return `toc:count:${bucket}`;
  }

  protected estimatedHeightFallback(): number {
    let visible = 0;
    forEachVisibleTocHeading(this.headings, this.foldState, () => { visible++; });
    return Math.max(58, 38 + visible * 26);
  }

  eq(other: TocWidget): boolean {
    return this.signature === other.signature;
  }

  toDOM(view: EditorView): HTMLElement {
    const foldState = view.state.field(tocFoldField, false) ?? this.foldState;
    const div = document.createElement("div");
    div.className = "toc cm-toc";
    div.addEventListener("mousedown", (event) => {
      event.preventDefault();
      event.stopPropagation();
    });

    if (this.headings.length === 0) {
      const empty = document.createElement("div");
      empty.className = "toc-empty";
      empty.textContent = "(no headings yet)";
      div.append(empty);
      return this.registerMeasured(div, view);
    }

    // Determine which headings have children (for chevron rendering)
    const hasChildren = new Set<number>();
    for (let i = 0; i < this.headings.length - 1; i++) {
      if (this.headings[i + 1]!.level > this.headings[i]!.level) hasChildren.add(i);
    }

    const ul = document.createElement("ul");
    ul.className = "toc-list";
    forEachVisibleTocHeading(this.headings, foldState, (heading, idx, fKey) => {
      const isFolded = foldState.get(fKey) ?? false;
      const li = document.createElement("li");
      li.className = `toc-item toc-h${heading.level}`;
      li.style.setProperty("--toc-depth", String(Math.max(0, heading.level - 1)));
      li.dataset.level = String(heading.level);
      li.dataset.foldKey = fKey;

      if (hasChildren.has(idx)) {
        const chevron = document.createElement("button");
        chevron.type = "button";
        chevron.className = `toc-fold-chevron${isFolded ? " is-folded" : ""}`;
        chevron.setAttribute("aria-label", isFolded ? "Expand" : "Collapse");
        chevron.addEventListener("click", (event) => {
          event.preventDefault();
          event.stopPropagation();
          const nowFolded = !(foldState.get(fKey) ?? false);
          view.dispatch({
            effects: tocFoldEffect.of({ key: fKey, folded: nowFolded }),
          });
        });
        li.append(chevron);
      }

      const span = document.createElement("span");
      span.className = "toc-item-text";
      span.textContent = heading.text || "(empty heading)";
      span.title = heading.text || "(empty heading)";
      li.append(span);

      li.addEventListener("click", (event) => {
        event.preventDefault();
        event.stopPropagation();
        const currentHeadings = tocHeadingsFromState(view.state);
        const currentKeys = tocFoldKeys(currentHeadings);
        const currentHeading = currentHeadings[currentKeys.indexOf(fKey)] ?? heading;
        view.dispatch({ selection: { anchor: currentHeading.pos }, scrollIntoView: true });
        view.focus();
        const dom = view.domAtPos(currentHeading.pos).node;
        const el = dom instanceof Element ? dom : dom.parentElement;
        el?.scrollIntoView({ block: "start", behavior: "smooth" });
      });
      ul.append(li);
    });

    div.append(ul);
    return this.registerMeasured(div, view);
  }

  ignoreEvent(): boolean { return true; }
}

const SEMANTIC_HEADING_ESTIMATED_HEIGHT: Record<number, number> = {
  1: 458,
  2: 236,
  3: 180,
  4: 135,
  5: 101,
};

class SemanticHeadingWidget extends MeasuredWidget {
  outline: SemanticOutline;
  from: number;
  to: number;

  constructor(outline: SemanticOutline, from: number, to: number) {
    super();
    this.outline = outline;
    this.from = from;
    this.to = to;
  }

  protected measureKey(): string {
    return ["sem", this.outline.level, this.outline.kind, this.outline.slug, shortHash(this.outline.text)].join(":");
  }

  protected measureGroupKey(): string {
    const textBucket = Math.min(4, Math.ceil(this.outline.text.length / 36));
    return ["sem", "level", this.outline.level, "text", textBucket].join(":");
  }

  protected estimatedHeightFallback(): number {
    return SEMANTIC_HEADING_ESTIMATED_HEIGHT[this.outline.level] ?? SEMANTIC_HEADING_ESTIMATED_HEIGHT[2]!;
  }

  eq(other: SemanticHeadingWidget): boolean {
    return this.from === other.from
      && this.to === other.to
      && this.outline.level === other.outline.level
      && this.outline.kind === other.outline.kind
      && this.outline.label === other.outline.label
      && this.outline.text === other.outline.text
      && this.outline.slug === other.outline.slug;
  }

  toDOM(view: EditorView): HTMLElement {
    const div = document.createElement("div");
    div.className = "cm-semantic-heading aaronnote-section-heading";
    div.dataset.sectionKind = this.outline.kind;
    div.dataset.sectionLabel = this.outline.label;
    div.dataset.outlineLevel = String(this.outline.level);
    div.style.setProperty("--outline-level", String(this.outline.level));
    setSourceRange(div, this.from, this.to);

    const inner = document.createElement("div");
    inner.className = "aaronnote-section-heading-inner";

    const label = document.createElement("span");
    label.className = "aaronnote-section-label";
    label.textContent = this.outline.label;
    const title = document.createElement("span");
    title.className = "aaronnote-section-title";
    title.textContent = this.outline.text;
    inner.append(label, title);
    div.append(inner);

    div.addEventListener("mousedown", (event) => {
      event.preventDefault();
      event.stopPropagation();
      view.dispatch({ selection: { anchor: this.from }, scrollIntoView: true });
      view.focus();
    });
    window.requestAnimationFrame(() => {
      if (div.isConnected && view.dom.isConnected) view.requestMeasure();
    });
    return this.registerMeasured(div, view);
  }

  ignoreEvent(): boolean { return false; }
}

function tocFoldKeys(headings: readonly TocHeading[]): string[] {
  const counts = new Map<string, number>();
  const stack: Array<{ level: number; ordinal: number }> = [];
  return headings.map((heading) => {
    while (stack.length > 0 && heading.level <= stack[stack.length - 1]!.level) {
      stack.pop();
    }
    const parentPath = stack.map((part) => part.ordinal).join(".");
    const siblingGroup = `${parentPath}|${heading.level}`;
    const ordinal = (counts.get(siblingGroup) ?? 0) + 1;
    counts.set(siblingGroup, ordinal);
    const path = parentPath ? `${parentPath}.${ordinal}` : String(ordinal);
    stack.push({ level: heading.level, ordinal });
    return `${path}:${heading.level}:${heading.text}`;
  });
}

function tocHeadingsFromState(state: EditorState): TocHeading[] {
  return tocIndexFromState(state).headings.filter((heading) => !heading.omit);
}

function forEachVisibleTocHeading<T extends TocHeading>(
  headings: readonly T[],
  foldState: ReadonlyMap<string, boolean>,
  visit: (heading: T, index: number, foldKey: string) => void,
): void {
  const foldedDepths: number[] = [];
  const foldKeys = tocFoldKeys(headings);
  for (let idx = 0; idx < headings.length; idx++) {
    const heading = headings[idx]!;
    while (foldedDepths.length > 0 && heading.level <= foldedDepths[foldedDepths.length - 1]!) {
      foldedDepths.pop();
    }
    const visible = foldedDepths.length === 0;
    const foldKey = foldKeys[idx]!;
    if (visible) visit(heading, idx, foldKey);
    if (visible && foldState.get(foldKey)) foldedDepths.push(heading.level);
  }
}

function tocSignature(headings: TocHeading[], foldState?: ReadonlyMap<string, boolean>): string {
  const keys = tocFoldKeys(headings);
  const base = headings.map((h, index) => `${keys[index]}\t${h.level}\t${h.text}\t${h.source || "markdown"}\t${h.kind || ""}`).join("\n");
  if (!foldState || foldState.size === 0) return base;
  const foldedKeys = keys.filter((k) => foldState.get(k)).join(",");
  return `${base}\nfold:${foldedKeys}`;
}

function tocContentSignature(state: EditorState): string {
  return tocSignature(tocHeadingsFromState(state));
}

function scanBlockExtraLineRanges(
  doc: Text,
  startLine = 1,
  endLine = doc.lines,
  excludedRanges: ReadonlyArray<{ from: number; to: number }> = [],
): Pick<BlockExtraRanges, "toc" | "semanticHeadings" | "ceilCommands" | "hrs"> {
  const toc: Array<{ from: number; to: number }> = [];
  const semanticHeadings: Array<{ from: number; to: number; outline: SemanticOutline }> = [];
  const ceilCommands: CeilCommandRange[] = [];
  const hrs: Array<{ from: number; to: number }> = [];
  for (let lineNum = Math.max(1, startLine); lineNum <= Math.min(doc.lines, endLine); lineNum++) {
    const line = doc.line(lineNum);
    if (rangeOverlapsAny(line.from, line.to, excludedRanges)) continue;
    if (TOC_LINE_RE.test(line.text)) toc.push({ from: line.from, to: line.to });
    const ceilCommand = parseCeilCommandLine(line.text, line.from);
    if (ceilCommand) ceilCommands.push(ceilCommand);
    const trimmed = line.text.trim();
    if (trimmed.startsWith("@@part") || trimmed.startsWith("@@section")) {
      const command = scanInlineCommands(trimmed)[0];
      const outline = command && command.fullFrom === 0 && command.fullTo === trimmed.length
        ? semanticOutlineFromCommand(command)
        : null;
      if (outline) semanticHeadings.push({ from: line.from, to: line.to, outline });
    }
    if (HR_LINE_RE.test(line.text)) hrs.push({ from: line.from, to: line.to });
  }
  return { toc, semanticHeadings, ceilCommands, hrs };
}

function scanBlockExtraRanges(
  doc: Text,
  excludedRanges: ReadonlyArray<{ from: number; to: number }> = [],
): BlockExtraRanges {
  const { toc, semanticHeadings, ceilCommands, hrs } = scanBlockExtraLineRanges(doc, 1, doc.lines, excludedRanges);
  return { toc, semanticHeadings, ceilCommands, hrs, frontMatter: scanFrontMatter(doc) };
}

const blockExtraRangesField = StateField.define<BlockExtraRanges>({
  create: (state) => scanBlockExtraRanges(state.doc, blockExtraExcludedRanges(state)),
  update(ranges, tr) {
    if (tr.docChanged) {
      return canMapBlockExtraRanges(tr.startState.doc, tr.changes, ranges)
        ? mapBlockExtraRanges(ranges, tr.changes)
        : canPatchBlockExtraRangesNearChanges(tr.startState.doc, tr.changes, ranges)
          ? patchBlockExtraRangesNearChanges(tr.state.doc, ranges, tr.changes, blockExtraExcludedRanges(tr.state))
          : scanBlockExtraRanges(tr.state.doc, blockExtraExcludedRanges(tr.state));
    }
    return ranges;
  },
});

function canMapBlockExtraRanges(doc: Text, changes: ChangeSet, ranges: BlockExtraRanges): boolean {
  if (changesMightAffectFencedCodeRanges(doc, changes)) return false;
  let canMap = true;
  changes.iterChanges((fromA, toA, _fromB, _toB, inserted) => {
    if (!canMap) return;
    const fromLine = doc.lineAt(Math.min(fromA, doc.length));
    const toLine = doc.lineAt(Math.min(Math.max(fromA, toA), doc.length));
    const oldText = doc.sliceString(fromLine.from, toLine.to);
    const newText = inserted.toString();
    if (/[\n\[\]\-*_@(){}]/.test(oldText) || /[\n\[\]\-*_@(){}]/.test(newText)) {
      canMap = false;
      return;
    }
    if (ranges.frontMatter && fromA <= ranges.frontMatter.to && toA >= ranges.frontMatter.from) {
      canMap = false;
    }
  });
  return canMap;
}

function canPatchBlockExtraRangesNearChanges(doc: Text, changes: ChangeSet, ranges: BlockExtraRanges): boolean {
  if (changesMightAffectFencedCodeRanges(doc, changes)) return false;
  let canPatch = true;
  changes.iterChanges((fromA, toA, _fromB, _toB, inserted) => {
    if (!canPatch) return;
    const removed = doc.sliceString(fromA, toA);
    const added = inserted.toString();
    if (removed.includes("\n") || added.includes("\n")) {
      canPatch = false;
      return;
    }
    const changedLine = doc.lineAt(Math.min(fromA, doc.length));
    if (changedLine.number <= 2) {
      canPatch = false;
      return;
    }
    if (ranges.frontMatter && fromA <= ranges.frontMatter.to && toA >= ranges.frontMatter.from) {
      canPatch = false;
    }
  });
  return canPatch;
}

function mapBlockExtraRanges(ranges: BlockExtraRanges, changes: ChangeSet): BlockExtraRanges {
  return {
    toc: ranges.toc.map((range) => ({ from: changes.mapPos(range.from), to: changes.mapPos(range.to) })),
    semanticHeadings: ranges.semanticHeadings.map((range) => ({ from: changes.mapPos(range.from), to: changes.mapPos(range.to), outline: range.outline })),
    ceilCommands: ranges.ceilCommands.map((range) => ({ ...range, from: changes.mapPos(range.from), to: changes.mapPos(range.to) })),
    hrs: ranges.hrs.map((range) => ({ from: changes.mapPos(range.from), to: changes.mapPos(range.to) })),
    frontMatter: ranges.frontMatter
      ? {
        ...ranges.frontMatter,
        from: changes.mapPos(ranges.frontMatter.from),
        to: changes.mapPos(ranges.frontMatter.to),
      }
      : null,
  };
}

function patchBlockExtraRangesNearChanges(
  doc: Text,
  ranges: BlockExtraRanges,
  changes: ChangeSet,
  excludedRanges: ReadonlyArray<{ from: number; to: number }>,
): BlockExtraRanges {
  let fromB = Number.POSITIVE_INFINITY;
  let toB = 0;
  changes.iterChanges((_fromA, _toA, nextFrom, nextTo) => {
    fromB = Math.min(fromB, nextFrom);
    toB = Math.max(toB, nextTo);
  });
  if (!Number.isFinite(fromB)) return mapBlockExtraRanges(ranges, changes);
  const startLine = Math.max(1, doc.lineAt(Math.min(fromB, doc.length)).number - 1);
  const endLine = Math.min(doc.lines, doc.lineAt(Math.min(toB, doc.length)).number + 1);
  const affectedFrom = doc.line(startLine).from;
  const affectedTo = doc.line(endLine).to;
  const mapped = mapBlockExtraRanges(ranges, changes);
  const scanned = scanBlockExtraLineRanges(doc, startLine, endLine, excludedRanges);
  return {
    toc: [
      ...mapped.toc.filter((range) => range.to < affectedFrom || range.from > affectedTo),
      ...scanned.toc,
    ].sort((a, b) => a.from - b.from || a.to - b.to),
    semanticHeadings: [
      ...mapped.semanticHeadings.filter((range) => range.to < affectedFrom || range.from > affectedTo),
      ...scanned.semanticHeadings,
    ].sort((a, b) => a.from - b.from || a.to - b.to),
    ceilCommands: [
      ...mapped.ceilCommands.filter((range) => range.to < affectedFrom || range.from > affectedTo),
      ...scanned.ceilCommands,
    ].sort((a, b) => a.from - b.from || a.to - b.to),
    hrs: [
      ...mapped.hrs.filter((range) => range.to < affectedFrom || range.from > affectedTo),
      ...scanned.hrs,
    ].sort((a, b) => a.from - b.from || a.to - b.to),
    frontMatter: mapped.frontMatter,
  };
}

class OrgEnvOpenWidget extends MeasuredWidget {
  kind: string;
  title: string;
  anchor: number;
  depth: number;

  constructor(kind: string, title: string, anchor: number, depth: number) {
    super();
    this.kind = kind;
    this.title = title;
    this.anchor = anchor;
    this.depth = depth;
  }

  protected measureKey(): string { return "oopen:" + this.kind + ":" + this.title; }

  protected measureGroupKey(): string { return "oopen:" + this.kind; }

  protected estimatedHeightFallback(): number { return -1; }

  eq(other: OrgEnvOpenWidget): boolean {
    return this.kind === other.kind
      && this.title === other.title
      && this.anchor === other.anchor
      && this.depth === other.depth;
  }

  toDOM(view: EditorView): HTMLElement {
    const div = document.createElement("div");
    div.className = "cm-org-env-heading-widget org-env-heading";
    div.dataset.orgEnvKind = this.kind;
    div.style.setProperty("--org-env-depth", String(this.depth));
    div.dataset.label = envLabel(this.kind);
    const label = document.createElement("span");
    label.className = "cm-org-env-label org-env-heading-label";
    label.textContent = envLabel(this.kind);
    const title = document.createElement("span");
    title.className = "org-env-heading-title";
    title.dataset.empty = this.title ? "false" : "true";
    title.innerHTML = renderMarkdownInlineHTML(this.title);
    div.append(label, title);
    div.addEventListener("mousedown", (event) => {
      event.preventDefault();
      event.stopPropagation();
      view.dispatch({ selection: { anchor: this.anchor }, scrollIntoView: true });
      view.focus();
    });
    return this.registerMeasured(div, view);
  }

  ignoreEvent(): boolean { return false; }
}

class OrgEnvEndWidget extends MeasuredWidget {
  kind: string;
  depth: number;

  constructor(kind: string, depth: number) {
    super();
    this.kind = kind;
    this.depth = depth;
  }

  protected measureKey(): string { return "oend:" + this.kind; }

  protected measureGroupKey(): string { return "oend:" + this.kind; }

  protected estimatedHeightFallback(): number { return -1; }

  eq(other: OrgEnvEndWidget): boolean {
    return this.kind === other.kind && this.depth === other.depth;
  }

  toDOM(view: EditorView): HTMLElement {
    const div = document.createElement("div");
    div.className = "cm-org-env-end-widget";
    div.dataset.orgEnvKind = this.kind;
    div.style.setProperty("--org-env-depth", String(this.depth));
    return this.registerMeasured(div, view);
  }

  ignoreEvent(): boolean { return false; }
}

function envLabel(kind: string): string {
  const labels: Record<string, string> = {
    ceil: "Jupyter",
    html: "HTML",
    meta: "Meta",
    theorem: "Theorem",
    thm: "Theorem",
    definition: "Definition",
    defn: "Definition",
    lemma: "Lemma",
    corollary: "Corollary",
    cor: "Corollary",
    proposition: "Proposition",
    prop: "Proposition",
    property: "Property",
    proof: "Proof",
    example: "Example",
    attention: "Attention",
    warning: "Warning",
    note: "Note",
    info: "Info",
    comment: "Comment",
    summary: "Summary",
    fold: "Fold",
    tikz: "TikZ",
    convention: "Convention",
    axiom: "Axiom",
    assumption: "Assumption",
    conjecture: "Conjecture",
    claim: "Claim",
    remark: "Remark",
    notation: "Notation",
    observation: "Observation",
    exercise: "Exercise",
    solution: "Solution",
    algorithm: "Algorithm",
    question: "Question",
  };
  return labels[kind] ?? kind;
}

function fallbackCeilKernels(current: string): CeilKernelSpec[] {
  const names = [current, DEFAULT_CEIL_KERNEL, "sagemath-10.9"].filter(Boolean);
  return Array.from(new Set(names)).map((name) => ({ name, displayName: name }));
}

function loadCeilKernels(current: string): Promise<CeilKernelSpec[]> {
  if (ceilKernelsCache) return Promise.resolve(ceilKernelsCache);
  if (ceilKernelsPending) return ceilKernelsPending;
  ceilKernelsPending = api.jupyterCell.kernels()
    .then((result) => {
      const kernels = Array.isArray(result.kernels) && result.kernels.length > 0
        ? result.kernels.map((kernel) => ({
            name: String(kernel.name || ""),
            displayName: String(kernel.displayName || kernel.name || ""),
            language: String(kernel.language || ""),
          })).filter((kernel) => kernel.name)
        : fallbackCeilKernels(current);
      ceilKernelsCache = kernels;
      return kernels;
    })
    .catch(() => fallbackCeilKernels(current))
    .finally(() => { ceilKernelsPending = null; });
  return ceilKernelsPending;
}

function populateCeilKernelSelect(select: HTMLSelectElement, kernels: CeilKernelSpec[], current: string): void {
  const all = [...kernels];
  if (!all.some((kernel) => kernel.name === current)) all.unshift({ name: current, displayName: current });
  const selected = select.value || current;
  select.replaceChildren();
  for (const kernel of all) {
    const option = document.createElement("option");
    option.value = kernel.name;
    option.textContent = kernel.displayName && kernel.displayName !== kernel.name
      ? `${kernel.displayName} (${kernel.name})`
      : kernel.name;
    select.append(option);
  }
  select.value = all.some((kernel) => kernel.name === selected) ? selected : current;
}

function mimeText(data: Record<string, unknown>, key: string): string {
  const value = data[key];
  if (typeof value === "string") return value;
  if (Array.isArray(value)) return value.map((item) => String(item ?? "")).join("");
  return "";
}

function appendCeilMimeBundle(root: HTMLElement, data: Record<string, unknown> = {}): void {
  const html = mimeText(data, "text/html");
  if (html) {
    const frame = document.createElement("iframe");
    frame.className = "cm-ceil-output-html";
    frame.sandbox.add("allow-scripts");
    frame.srcdoc = html;
    root.append(frame);
    return;
  }
  const png = mimeText(data, "image/png");
  if (png) {
    const img = document.createElement("img");
    img.className = "cm-ceil-output-image";
    img.src = `data:image/png;base64,${png}`;
    img.alt = "Jupyter output";
    root.append(img);
    return;
  }
  const svg = mimeText(data, "image/svg+xml");
  if (svg) {
    const frame = document.createElement("iframe");
    frame.className = "cm-ceil-output-html";
    frame.sandbox.add("allow-scripts");
    frame.srcdoc = svg;
    root.append(frame);
    return;
  }
  const markdown = mimeText(data, "text/markdown");
  if (markdown) {
    const div = document.createElement("div");
    div.className = "cm-ceil-output-markdown";
    div.innerHTML = renderMarkdownHTML(markdown);
    enhanceRenderedMarkdown(div);
    root.append(div);
    return;
  }
  const plain = mimeText(data, "text/plain") || JSON.stringify(data, null, 2);
  const pre = document.createElement("pre");
  pre.textContent = plain;
  root.append(pre);
}

function renderCeilOutputs(root: HTMLElement, result: CeilExecutionResult | null): void {
  root.replaceChildren();
  if (!result) {
    const empty = document.createElement("div");
    empty.className = "cm-ceil-output-empty";
    empty.textContent = "No output";
    root.append(empty);
    return;
  }
  const outputs = Array.isArray(result.outputs) ? result.outputs : [];
  if (outputs.length === 0) {
    const empty = document.createElement("div");
    empty.className = "cm-ceil-output-empty";
    empty.textContent = result.status === "error" ? (result.message || "Execution failed") : "No output";
    root.append(empty);
    return;
  }
  for (const output of outputs) {
    const item = document.createElement("div");
    item.className = "cm-ceil-output-item";
    const type = String(output.output_type || "");
    if (type === "stream") {
      const pre = document.createElement("pre");
      pre.dataset.stream = String(output.name || "stdout");
      pre.textContent = String(output.text || "");
      item.append(pre);
    } else if (type === "error") {
      item.classList.add("cm-ceil-output-error");
      const pre = document.createElement("pre");
      const traceback = Array.isArray(output.traceback) ? output.traceback.map(String).join("\n") : "";
      pre.textContent = traceback || [output.ename, output.evalue].filter(Boolean).map(String).join(": ");
      item.append(pre);
    } else {
      appendCeilMimeBundle(item, (output.data && typeof output.data === "object" ? output.data : {}) as Record<string, unknown>);
    }
    root.append(item);
  }
}

function openCeilOutputPopup(result: CeilExecutionResult | null): void {
  const overlay = document.createElement("div");
  overlay.className = "cm-ceil-output-popover";
  const panel = document.createElement("div");
  panel.className = "cm-ceil-output-popover-panel";
  const header = document.createElement("div");
  header.className = "cm-ceil-output-popover-header";
  const title = document.createElement("span");
  title.textContent = "Jupyter output";
  const close = document.createElement("button");
  close.type = "button";
  close.textContent = "Close";
  const body = document.createElement("div");
  body.className = "cm-ceil-output-popover-body";
  renderCeilOutputs(body, result);
  const destroy = (): void => overlay.remove();
  close.addEventListener("click", destroy);
  overlay.addEventListener("mousedown", (event) => {
    if (event.target === overlay) destroy();
  });
  window.addEventListener("keydown", function onKey(event) {
    if (event.key !== "Escape") return;
    window.removeEventListener("keydown", onKey);
    destroy();
  });
  header.append(title, close);
  panel.append(header, body);
  overlay.append(panel);
  document.body.append(overlay);
}

class CeilCommandWidget extends MeasuredWidget {
  range: CeilCommandRange;

  constructor(range: CeilCommandRange) {
    super();
    this.range = range;
  }

  protected measureKey(): string {
    return `ceilcmd:${this.range.from}:${this.range.argsRaw}:${this.range.idRaw}`;
  }

  protected measureGroupKey(): string { return "ceilcmd"; }

  protected estimatedHeightFallback(): number { return 132; }

  eq(other: CeilCommandWidget): boolean {
    return this.range.from === other.range.from
      && this.range.to === other.range.to
      && this.range.argsRaw === other.range.argsRaw
      && this.range.idRaw === other.range.idRaw;
  }

  toDOM(view: EditorView): HTMLElement {
    const file = currentNoteFile();
    const meta = parseCeilCommand(this.range, file);
    if (meta.changed) scheduleCeilCommandLineUpdate(view, this.range.from, formatCeilCommand(meta));

    const block = document.createElement("div");
    block.className = "cm-ceil-cell-widget cm-ceil-command-widget";
    setSourceRange(block, this.range.from, this.range.to);

    const header = document.createElement("div");
    header.className = "cm-ceil-header";

    const label = document.createElement("span");
    label.className = "cm-ceil-label";
    label.textContent = "Jupyter";

    const languageInput = document.createElement("input");
    languageInput.className = "cm-ceil-language";
    languageInput.type = "text";
    languageInput.value = meta.language;
    languageInput.spellcheck = false;
    languageInput.setAttribute("aria-label", "Language");

    const kernelSelect = document.createElement("select");
    kernelSelect.className = "cm-ceil-kernel";
    kernelSelect.setAttribute("aria-label", "Kernel");
    populateCeilKernelSelect(kernelSelect, ceilKernelsCache ?? fallbackCeilKernels(meta.kernel), meta.kernel);
    const loadKernels = (): void => {
      void loadCeilKernels(meta.kernel).then((kernels) => {
        if (!kernelSelect.isConnected) return;
        populateCeilKernelSelect(kernelSelect, kernels, kernelSelect.value || meta.kernel);
      });
    };
    kernelSelect.addEventListener("pointerdown", loadKernels);
    kernelSelect.addEventListener("focus", loadKernels);

    const status = document.createElement("span");
    status.className = "cm-ceil-status";
    status.textContent = meta.id;

    const buttonBar = document.createElement("div");
    buttonBar.className = "cm-ceil-actions";
    const source = document.createElement("div");
    source.className = "cm-ceil-source cm-ceil-source-compact";
    source.textContent = "Loading source...";
    const outputWrap = document.createElement("div");
    outputWrap.className = "cm-ceil-output-wrap";
    const outputHeader = document.createElement("div");
    outputHeader.className = "cm-ceil-output-toolbar";
    const outputTitle = document.createElement("span");
    outputTitle.textContent = "Output";
    const outputTools = document.createElement("div");
    outputTools.className = "cm-ceil-output-tools";
    const output = document.createElement("div");
    output.className = "cm-ceil-output cm-ceil-output-limited";
    const cacheKey = ceilOutputKey(file, meta, `script:${meta.id}`);
    let lastResult = ceilOutputCache.get(cacheKey) ?? null;
    renderCeilOutputs(output, lastResult);

    const makeButton = (text: string, title: string, run: () => Promise<void> | void): HTMLButtonElement => {
      const button = document.createElement("button");
      button.type = "button";
      button.textContent = text;
      button.title = title;
      button.addEventListener("mousedown", stopEditorPropagation);
      button.addEventListener("click", (event) => {
        event.preventDefault();
        event.stopPropagation();
        void run();
      });
      return button;
    };

    const refreshSource = async (): Promise<void> => {
      if (!file) {
        source.textContent = "Save note first";
        return;
      }
      try {
        const result = await api.jupyterCell.readScriptCell({
          file,
          cellId: meta.id,
          kernel: meta.kernel,
          session: meta.session,
          language: meta.language,
        });
        const code = String(result.code ?? "");
        source.replaceChildren(code.trim() ? highlightedCeilCode(code, meta.language) : highlightedCeilCode("", meta.language));
        if (!code.trim()) source.dataset.empty = "true";
        const savedOutput = result.output && typeof result.output === "object" ? result.output as CeilExecutionResult : null;
        lastResult = savedOutput;
        renderCeilOutputs(output, lastResult);
        if (lastResult) ceilOutputCache.set(cacheKey, lastResult);
        view.requestMeasure();
      } catch (err) {
        source.textContent = err instanceof Error ? err.message : String(err);
      }
    };

    const writeCommandLine = (): void => {
      const nextKernel = kernelSelect.value || DEFAULT_CEIL_KERNEL;
      const nextLanguage = languageInput.value.trim() || ceilLanguageForKernel(nextKernel);
      replaceCeilCommandLine(view, this.range.from, formatCeilCommand({
        ...meta,
        kernel: nextKernel,
        language: ceilLanguageForKernel(nextKernel, nextLanguage),
        changed: false,
      }));
    };

    languageInput.addEventListener("blur", writeCommandLine);
    languageInput.addEventListener("keydown", (event) => {
      event.stopPropagation();
      if (event.key === "Enter") {
        event.preventDefault();
        writeCommandLine();
        view.focus();
      }
    });
    kernelSelect.addEventListener("change", (event) => {
      event.stopPropagation();
      languageInput.value = ceilLanguageForKernel(kernelSelect.value || DEFAULT_CEIL_KERNEL, languageInput.value);
      writeCommandLine();
    });

    const setBusy = (busy: boolean): void => {
      for (const button of buttonBar.querySelectorAll<HTMLButtonElement>("button")) {
        if (button.dataset.ceilInterrupt === "true") continue;
        button.disabled = busy;
      }
      languageInput.disabled = busy;
      kernelSelect.disabled = busy;
    };

    const editButton = makeButton("Edit", "Open hidden source script", async () => {
      if (!file) {
        status.textContent = "Save note first";
        return;
      }
      setBusy(true);
      status.textContent = "Opening...";
      try {
        await api.jupyterCell.openScript({
          file,
          cellId: meta.id,
          kernel: meta.kernel,
          session: meta.session,
          language: meta.language,
          storage: "script",
          cells: ceilCommandCellsForContext(view.state, meta, file),
        });
        status.textContent = meta.id;
        await refreshSource();
      } catch (err) {
        status.textContent = err instanceof Error ? err.message : String(err);
      } finally {
        setBusy(false);
      }
    });

    const runButton = makeButton("Run", "Run this hidden script cell", async () => {
      if (!file) {
        status.textContent = "Save note first";
        return;
      }
      setBusy(true);
      status.textContent = "Running...";
      output.textContent = "Running...";
      try {
        const result = await api.jupyterCell.executeScriptCell({
          file,
          cellId: meta.id,
          kernel: meta.kernel,
          session: meta.session,
          language: meta.language,
        }) as CeilExecutionResult;
        lastResult = result;
        ceilOutputCache.set(cacheKey, result);
        status.textContent = result.executionCount != null ? `In [${result.executionCount}]` : (result.status || meta.id);
        renderCeilOutputs(output, result);
        await refreshSource();
      } catch (err) {
        lastResult = {
          ok: false,
          status: "error",
          message: err instanceof Error ? err.message : String(err),
          outputs: [{ output_type: "error", traceback: [err instanceof Error ? err.message : String(err)] }],
        };
        renderCeilOutputs(output, lastResult);
        status.textContent = "Error";
      } finally {
        setBusy(false);
        view.requestMeasure();
      }
    });

    const interruptButton = makeButton("Interrupt", "Interrupt this kernel session", async () => {
      if (!file) return;
      status.textContent = "Interrupting...";
      try {
        await api.jupyterCell.interrupt({ file, kernel: meta.kernel, session: meta.session });
        status.textContent = meta.id;
      } catch (err) {
        status.textContent = err instanceof Error ? err.message : String(err);
      }
    });
    interruptButton.dataset.ceilInterrupt = "true";

    const restartButton = makeButton("Restart", "Restart this kernel session", async () => {
      if (!file) return;
      status.textContent = "Restarting...";
      try {
        await api.jupyterCell.restart({ file, kernel: meta.kernel, session: meta.session });
        status.textContent = meta.id;
      } catch (err) {
        status.textContent = err instanceof Error ? err.message : String(err);
      }
    });

    const clearButton = makeButton("Clear", "Clear output", () => {
      lastResult = null;
      ceilOutputCache.delete(cacheKey);
      renderCeilOutputs(output, null);
      if (file) {
        void api.jupyterCell.clearScriptCellOutput({
          file,
          cellId: meta.id,
          kernel: meta.kernel,
          session: meta.session,
          language: meta.language,
        }).catch(() => {});
      }
      view.requestMeasure();
    });
    const refreshButton = makeButton("Refresh", "Reload source and saved output", async () => {
      status.textContent = "Refreshing...";
      await refreshSource();
      status.textContent = meta.id;
    });
    const foldButton = makeButton("Fold", "Fold output", () => {
      outputWrap.classList.toggle("is-folded");
      foldButton.textContent = outputWrap.classList.contains("is-folded") ? "Show" : "Fold";
      view.requestMeasure();
    });
    const expandButton = makeButton("Popout", "Show full output", () => openCeilOutputPopup(lastResult));

    buttonBar.append(editButton, runButton, interruptButton, restartButton, clearButton);
    outputTools.append(refreshButton, foldButton, expandButton);
    outputHeader.append(outputTitle, outputTools);
    outputWrap.append(outputHeader, output);
    header.append(label, languageInput, kernelSelect, status, buttonBar);
    block.append(header, source, outputWrap);
    stopInteractiveWidgetEvents(block);
    void refreshSource();
    return this.registerMeasured(block, view);
  }

  ignoreEvent(): boolean { return false; }
}

class MetaWidget extends MeasuredWidget {
  body: string;
  from: number;
  to: number;

  constructor(body: string, from: number, to: number) {
    super();
    this.body = body;
    this.from = from;
    this.to = to;
  }

  protected measureKey(): string { return "meta:" + shortHash(this.body); }

  protected measureGroupKey(): string { return "meta"; }

  protected estimatedHeightFallback(): number { return 210; }

  eq(other: MetaWidget): boolean {
    return this.body === other.body && this.from === other.from && this.to === other.to;
  }

  toDOM(view: EditorView): HTMLElement {
    const div = document.createElement("div");
    div.className = "cm-org-env-block org-env-block";
    setSourceRange(div, this.from, this.to);
    div.setAttribute("data-kind", "meta");
    div.dataset.label = envLabel("meta");
    renderMetaWidget(div, view, this.body, this.from, this.to);
    return this.registerMeasured(div, view);
  }

  ignoreEvent(): boolean { return true; }
}

class CommentWidget extends MeasuredWidget {
  title: string;
  body: string;
  from: number;
  to: number;
  depth: number;

  constructor(title: string, body: string, from: number, to: number, depth: number) {
    super();
    this.title = title;
    this.body = body;
    this.from = from;
    this.to = to;
    this.depth = depth;
  }

  protected measureKey(): string { return "cmnt:" + shortHash(this.title + ":" + this.body); }

  protected measureGroupKey(): string {
    return `cmnt:lines:${Math.min(8, Math.ceil(this.body.split(/\n/).length / 5))}`;
  }

  protected estimatedHeightFallback(): number {
    return 54 + this.body.split(/\n/).length * 22;
  }

  eq(other: CommentWidget): boolean {
    return this.title === other.title
      && this.body === other.body
      && this.from === other.from
      && this.to === other.to
      && this.depth === other.depth;
  }

  toDOM(view: EditorView): HTMLElement {
    const block = document.createElement("org-env-block");
    block.className = "cm-org-env-comment-widget org-env-block";
    setSourceRange(block, this.from, this.to);
    block.dataset.cmOpenSource = "true";
    block.setAttribute("data-kind", "comment");
    block.setAttribute("data-title", this.title);
    block.setAttribute("data-label", envLabel("comment"));
    block.setAttribute("data-comment-open", "false");
    block.style.setProperty("--org-env-depth", String(this.depth));

    const button = document.createElement("button");
    button.type = "button";
    button.className = "org-env-comment-button";
    button.setAttribute("aria-expanded", "false");
    const label = document.createElement("span");
    label.className = "org-env-comment-label";
    label.innerHTML = this.title.trim() ? renderMarkdownInlineHTML(this.title.trim()) : "comment";
    const state = document.createElement("span");
    state.className = "org-env-comment-state";
    state.textContent = "show";
    button.append(label, state);
    button.addEventListener("mousedown", stopEditorPropagation);
    button.addEventListener("click", (event) => {
      event.preventDefault();
      event.stopPropagation();
      const open = content.hidden === true;
      content.hidden = !open;
      block.classList.toggle("org-env-comment-open", open);
      block.setAttribute("data-comment-open", open ? "true" : "false");
      button.setAttribute("aria-expanded", open ? "true" : "false");
      state.textContent = open ? "hide" : "show";
      if (block.isConnected) view.requestMeasure();
    });

    const content = document.createElement("div");
    content.className = "org-env-content";
    content.hidden = true;
    content.innerHTML = renderMarkdownHTML(this.body.trim());
    enhanceRenderedMarkdown(content);
    stopInteractiveWidgetEvents(content);

    block.append(button, content);
    return this.registerMeasured(block, view);
  }

  ignoreEvent(): boolean { return false; }
}

class FoldWidget extends MeasuredWidget {
  title: string;
  body: string;
  from: number;
  to: number;
  depth: number;

  constructor(title: string, body: string, from: number, to: number, depth: number) {
    super();
    this.title = title;
    this.body = body;
    this.from = from;
    this.to = to;
    this.depth = depth;
  }

  protected measureKey(): string { return "fold:" + shortHash(this.title + ":" + this.body); }

  protected measureGroupKey(): string {
    return `fold:lines:${Math.min(8, Math.ceil(this.body.split(/\n/).length / 5))}`;
  }

  protected estimatedHeightFallback(): number { return 46; }

  eq(other: FoldWidget): boolean {
    return this.title === other.title
      && this.body === other.body
      && this.from === other.from
      && this.to === other.to
      && this.depth === other.depth;
  }

  toDOM(view: EditorView): HTMLElement {
    const block = document.createElement("org-env-block");
    block.className = "cm-org-env-fold-widget org-env-block";
    setSourceRange(block, this.from, this.to);
    block.dataset.cmOpenSource = "true";
    block.setAttribute("data-kind", "fold");
    block.setAttribute("data-title", this.title);
    block.setAttribute("data-label", envLabel("fold"));
    block.setAttribute("data-fold-open", "false");
    block.style.setProperty("--org-env-depth", String(this.depth));

    const button = document.createElement("button");
    button.type = "button";
    button.className = "org-env-fold-summary";
    button.setAttribute("aria-expanded", "false");

    const marker = document.createElement("span");
    marker.className = "org-env-fold-marker";
    marker.setAttribute("aria-hidden", "true");
    const title = document.createElement("span");
    title.className = "org-env-fold-title";
    title.innerHTML = renderMarkdownInlineHTML(this.title.trim() || "Details");
    button.append(marker, title);
    button.addEventListener("mousedown", stopEditorPropagation);
    button.addEventListener("click", (event) => {
      event.preventDefault();
      event.stopPropagation();
      const open = content.hidden === true;
      content.hidden = !open;
      block.classList.toggle("org-env-fold-open", open);
      block.setAttribute("data-fold-open", open ? "true" : "false");
      button.setAttribute("aria-expanded", open ? "true" : "false");
      if (block.isConnected) view.requestMeasure();
    });

    const content = document.createElement("div");
    content.className = "org-env-fold-content org-env-content";
    content.hidden = true;
    content.innerHTML = renderMarkdownHTML(this.body.trim());
    enhanceRenderedMarkdown(content);
    stopInteractiveWidgetEvents(content);

    block.append(button, content);
    return this.registerMeasured(block, view);
  }

  ignoreEvent(): boolean { return false; }
}

class HtmlWidget extends MeasuredWidget {
  body: string;
  from: number;
  to: number;

  constructor(body: string, from: number, to: number) {
    super();
    this.body = body;
    this.from = from;
    this.to = to;
  }

  protected measureKey(): string { return "html:" + shortHash(this.body); }

  protected measureGroupKey(): string {
    return `html:lines:${Math.min(8, Math.ceil(this.body.split(/\n/).length / 6))}`;
  }

  protected estimatedHeightFallback(): number {
    return Math.max(48, this.body.split(/\n/).length * 24);
  }

  eq(other: HtmlWidget): boolean {
    return this.body === other.body && this.from === other.from && this.to === other.to;
  }

  toDOM(view: EditorView): HTMLElement {
    const div = document.createElement("div");
    div.className = "cm-html-env-widget";
    setSourceRange(div, this.from, this.to);
    div.innerHTML = renderMarkdownHTML(buildOrgEnvSource("html", "", this.body));
    stopInteractiveWidgetEvents(div);
    return this.registerMeasured(div, view);
  }

  ignoreEvent(): boolean { return true; }
}

class TikzWidget extends MeasuredWidget {
  title: string;
  body: string;
  from: number;
  to: number;
  dirty: boolean;

  constructor(title: string, body: string, from: number, to: number, dirty: boolean) {
    super();
    this.title = title;
    this.body = body;
    this.from = from;
    this.to = to;
    this.dirty = dirty;
  }

  protected measureKey(): string { return "tikz:" + this.title; }

  protected measureGroupKey(): string { return "tikz"; }

  protected estimatedHeightFallback(): number { return 260; }

  eq(other: TikzWidget): boolean {
    return this.title === other.title && this.body === other.body && this.from === other.from && this.to === other.to && this.dirty === other.dirty;
  }

  toDOM(view: EditorView): HTMLElement {
    const figure = document.createElement("figure");
    figure.className = "cm-image-widget cm-visual-attachment cm-visual-attachment-html cm-tikz-env-widget aaronnote-tikz";
    setSourceRange(figure, this.from, this.to);
    figure.dataset.cmOpenSource = "true";

    const card = document.createElement("div");
    card.className = "cm-image-render cm-visual-file-card cm-visual-file-card-html cm-tikz-env-card";
    figure.append(card);

    const meta = completeTikzTitle(this.title);
    applyImageLayout(figure, meta.layout);
    const file = currentNoteFile();
    if (meta.changed) {
      card.textContent = "Preparing TikZ...";
      scheduleTikzOpenLineUpdate(view, this.from, (info) => {
        const current = completeTikzTitle(info.title);
        return current.changed ? `${current.id} ${current.timestamp}${current.attrsRaw ? ` ${current.attrsRaw}` : ""}` : null;
      });
      stopInteractiveWidgetEvents(figure);
      return this.registerMeasured(figure, view);
    }
    if (!file) {
      card.textContent = "TikZ render needs a saved note file";
      stopInteractiveWidgetEvents(figure);
      return this.registerMeasured(figure, view);
    }

    const sourceCacheKey = tikzSourceCacheKey(file, meta.id);
    const previousRenderedSource = tikzRenderedSourceByAsset.get(sourceCacheKey);
    const pendingSource = tikzPendingSourceByAsset.get(sourceCacheKey);
    const bodyChanged = this.dirty || (previousRenderedSource !== undefined && previousRenderedSource !== this.body);
    if (bodyChanged && pendingSource !== this.body) {
      card.textContent = "Updating TikZ...";
      setBoundedMap(tikzPendingSourceByAsset, sourceCacheKey, this.body);
      scheduleTikzOpenLineUpdate(view, this.from, (info) => {
        const current = completeTikzTitle(info.title);
        if (current.changed) return `${current.id} ${current.timestamp}${current.attrsRaw ? ` ${current.attrsRaw}` : ""}`;
        const timestamp = nextTikzTimestamp(current.timestamp);
        return `${current.id} ${timestamp}${current.attrsRaw ? ` ${current.attrsRaw}` : ""}`;
      }, [clearTikzDirtyEffect.of(meta.id)]);
      stopInteractiveWidgetEvents(figure);
      return this.registerMeasured(figure, view);
    }

    card.textContent = "Rendering TikZ...";
    void ensureTikzAsset(file, meta.id, meta.timestamp, this.body).then((result) => {
      if (!figure.isConnected) return;
      if (!result.ok || !result.markdownPath) {
        tikzPendingSourceByAsset.delete(sourceCacheKey);
        card.textContent = result.message || "TikZ render failed";
        view.requestMeasure();
        return;
      }
      setBoundedMap(tikzRenderedSourceByAsset, sourceCacheKey, this.body);
      tikzPendingSourceByAsset.delete(sourceCacheKey);
      const img = document.createElement("img");
      img.className = "cm-image-render cm-tikz-env-image";
      img.src = resolveAssetSrc(result.markdownPath);
      img.alt = `TikZ ${meta.id}`;
      img.loading = "lazy";
      img.decoding = "async";
      img.addEventListener("load", () => { if (figure.isConnected) view.requestMeasure(); });
      img.addEventListener("error", () => { if (figure.isConnected) view.requestMeasure(); });
      figure.replaceChildren(img);
      view.requestMeasure();
    });

    stopInteractiveWidgetEvents(figure);
    return this.registerMeasured(figure, view);
  }

  ignoreEvent(): boolean { return false; }
}

function renderMetaWidget(
  root: HTMLElement,
  view: EditorView,
  body: string,
  from: number,
  to: number,
): void {
  const meta = document.createElement("div");
  meta.className = "org-env-meta aaronnote-meta-cover";
  const entries = parseMetaEntries(body);
  if (!metaRoamIndexed(entries)) {
    const badge = document.createElement("span");
    badge.className = "aaronnote-meta-roam-badge";
    badge.title = "Not in roam database";
    badge.setAttribute("aria-label", "Not in roam database");
    badge.textContent = "🔕";
    meta.append(badge);
  }

  if (entries.length === 0) {
    const empty = document.createElement("span");
    empty.className = "org-env-meta-empty";
    empty.textContent = "No metadata";
    meta.append(empty);
    root.append(meta);
    return;
  }

  const writeMeta = (): void => {
    const lines = Array.from(meta.querySelectorAll<HTMLInputElement | HTMLTextAreaElement>(".org-env-meta-value"))
      .map((input) => `${input.dataset.key}: ${input.value.trim()}`);
    view.dispatch({
      changes: { from, to, insert: buildOrgEnvSource("meta", "", lines.join("\n")) },
    });
  };

  function makeInput(entry: { key: string; value: string }, className: string, label: string): HTMLInputElement;
  function makeInput(
    entry: { key: string; value: string },
    className: string,
    label: string,
    multiline: true,
  ): HTMLTextAreaElement;
  function makeInput(
    entry: { key: string; value: string },
    className: string,
    label: string,
    multiline = false,
  ): HTMLInputElement | HTMLTextAreaElement {
    const value = multiline ? document.createElement("textarea") : document.createElement("input");
    value.className = `org-env-meta-value ${className}`;
    value.setAttribute("aria-label", label);
    value.spellcheck = false;
    value.value = entry.value;
    value.dataset.key = entry.key;
    if (value instanceof HTMLInputElement) {
      value.type = "text";
    } else {
      value.rows = 1;
      value.wrap = "soft";
    }
    const resize = (): void => {
      if (!(value instanceof HTMLTextAreaElement)) return;
      value.style.height = "auto";
      value.style.height = `${value.scrollHeight}px`;
    };
    value.addEventListener("mousedown", stopEditorPropagation);
    value.addEventListener("click", stopEditorPropagation);
    value.addEventListener("beforeinput", stopEditorPropagation);
    value.addEventListener("input", (event) => {
      event.stopPropagation();
      resize();
    });
    value.addEventListener("keyup", stopEditorPropagation);
    value.addEventListener("paste", stopEditorPropagation);
    value.addEventListener("cut", stopEditorPropagation);
    value.addEventListener("blur", writeMeta);
    const handleKeydown = (event: Event): void => {
      const keyEvent = event as KeyboardEvent;
      event.stopPropagation();
      if (keyEvent.key === "Enter") {
        event.preventDefault();
        writeMeta();
        view.focus();
      }
    };
    value.addEventListener("keydown", handleKeydown);
    queueMicrotask(resize);
    return value;
  }

  const byKey = metaEntryMap(entries);
  const titleEntry = entries.find((entry) => entry.key.toLowerCase() === "title");
  const dateEntry = entries.find((entry) => entry.key.toLowerCase() === "date");
  const tagsEntry = entries.find((entry) => entry.key.toLowerCase() === "tags");
  const sourceEntry = entries.find((entry) => entry.key.toLowerCase() === "source");

  if (titleEntry) {
    meta.append(makeInput(titleEntry, "aaronnote-meta-title", "Title", true));
  } else {
    const title = document.createElement("h1");
    title.className = "aaronnote-meta-title";
    title.textContent = "Untitled";
    meta.append(title);
  }

  if (dateEntry) {
    meta.append(makeInput(dateEntry, "aaronnote-meta-date", "Date"));
  }

  const tagValues = metaTags(byKey.get("tags") || "");
  const visibleTagValues = tagValues.filter(showMetaTag);
  if (tagsEntry || tagValues.length > 0) {
    const tags = document.createElement("nav");
    tags.className = "aaronnote-meta-tags";
    tags.setAttribute("aria-label", "Tags");
    for (const tagValue of visibleTagValues) {
      const tag = document.createElement("button");
      tag.type = "button";
      tag.className = "aaronnote-meta-tag";
      tag.textContent = `#${tagValue}`;
      tag.addEventListener("mousedown", stopEditorPropagation);
      tag.addEventListener("click", (event) => {
        event.preventDefault();
        event.stopPropagation();
        document.dispatchEvent(new CustomEvent("knowledge:apply-tag", { detail: { tag: tagValue } }));
      });
      tags.append(tag);
    }
    if (tagsEntry) {
      const tagInput = makeInput(tagsEntry, "aaronnote-meta-hidden", "Tags");
      tagInput.type = "hidden";
      meta.append(tagInput);
    }
    if (visibleTagValues.length > 0) {
      meta.append(tags);
    }
  }

  if (sourceEntry) {
    const source = makeInput(sourceEntry, "aaronnote-meta-hidden", "Source");
    source.type = "hidden";
    meta.append(source);
  }

  const shownKeys = new Set(["title", "date", "tags", "source"]);
  for (const entry of entries) {
    if (shownKeys.has(entry.key.toLowerCase())) continue;
    const hidden = makeInput(entry, "aaronnote-meta-hidden", entry.key);
    hidden.type = "hidden";
    meta.append(hidden);
  }

  root.append(meta);
}

class FrontMatterWidget extends MeasuredWidget {
  body: string;
  from: number;
  to: number;

  constructor(body: string, from: number, to: number) {
    super();
    this.body = body;
    this.from = from;
    this.to = to;
  }

  protected measureKey(): string { return "fm:" + shortHash(this.body); }

  protected measureGroupKey(): string {
    return `fm:lines:${Math.min(5, Math.ceil(this.body.split(/\n/).length / 4))}`;
  }

  protected estimatedHeightFallback(): number {
    return 36 + this.body.split(/\n/).length * 18;
  }

  eq(other: FrontMatterWidget): boolean {
    return this.body === other.body && this.from === other.from && this.to === other.to;
  }

  toDOM(view: EditorView): HTMLElement {
    const div = document.createElement("div");
    div.className = "cm-front-matter-block";
    setSourceRange(div, this.from, this.to);
    const label = document.createElement("span");
    label.className = "cm-front-matter-label";
    label.textContent = "YAML";
    const content = document.createElement("pre");
    content.className = "cm-front-matter-content";
    content.textContent = this.body.trim();
    div.append(label, content);
    return this.registerMeasured(div, view);
  }

  ignoreEvent(): boolean { return false; }
}

class HorizontalRuleWidget extends MeasuredWidget {
  from: number;
  to: number;

  constructor(from: number, to: number) {
    super();
    this.from = from;
    this.to = to;
  }

  protected measureKey(): string { return "hr"; }

  protected measureGroupKey(): string { return "hr"; }

  protected estimatedHeightFallback(): number { return 46; }

  eq(other: HorizontalRuleWidget): boolean {
    return this.from === other.from && this.to === other.to;
  }

  toDOM(view: EditorView): HTMLElement {
    const hr = document.createElement("hr");
    hr.className = "cm-horizontal-rule";
    setSourceRange(hr, this.from, this.to);
    return this.registerMeasured(hr, view);
  }

  ignoreEvent(): boolean { return false; }
}

function selectionTouchesRange(state: EditorState, from: number, to: number): boolean {
  const sel = state.selection.main;
  if (sel.empty) return sel.from >= from && sel.from <= to;
  return sel.from < to && sel.to > from;
}

function addOrgEnvBoundaryDecos(
  decos: CMRange<Decoration>[],
  state: EditorState,
  block: OrgEnvBlock,
): void {
  const openActive = selectionTouchesRange(state, block.openFrom, block.openTo);
  const closeActive = selectionTouchesRange(state, block.closeFrom, block.closeTo)
    && state.selection.main.from > block.closeFrom;

  if (!openActive) {
    decos.push(
      Decoration.replace({
        widget: new OrgEnvOpenWidget(block.kind, block.title, block.titleAnchor, block.depth),
        block: true,
      }).range(block.openFrom, block.openTo),
    );
  } else {
    decos.push(Decoration.mark({ class: "syntax-hint" }).range(block.openFrom, block.openTo));
  }

  if (!closeActive) {
    decos.push(
      Decoration.replace({
        widget: new OrgEnvEndWidget(block.kind, block.depth),
        block: true,
      }).range(block.closeFrom, block.closeTo),
    );
  } else {
    decos.push(Decoration.mark({ class: "syntax-hint" }).range(block.closeFrom, block.closeTo));
  }
}

interface OrgEnvRailMeasure {
  kind: string;
  depth: number;
  top: number;
  height: number;
  left: number;
}

const orgEnvBlocksField = StateField.define<readonly OrgEnvBlock[]>({
  create: (state) => scanOrgEnvBlocks(state.doc.toString(), 0, 0, blockExtraExcludedRanges(state)),
  update(blocks, tr) {
    if (!tr.docChanged) return blocks;
    if (!canMapOrgEnvBlocks(tr.startState.doc, blocks, tr.changes)) {
      return patchOrgEnvBlocksForTitleChange(tr.startState.doc, tr.state.doc, blocks, tr.changes)?.blocks
        ?? scanOrgEnvBlocks(tr.state.doc.toString(), 0, 0, blockExtraExcludedRanges(tr.state));
    }
    return mapOrgEnvBlocks(blocks, tr.changes, tr.state.doc);
  },
});

const dirtyTikzBlocksField = StateField.define<ReadonlySet<string>>({
  create: () => new Set<string>(),
  update(value, tr) {
    let next: Set<string> | null = null;
    for (const effect of tr.effects) {
      if (!effect.is(clearTikzDirtyEffect)) continue;
      if (!next) next = new Set(value);
      next.delete(effect.value);
    }
    if (!tr.docChanged) return next ?? value;
    const blocks = tr.startState.field(orgEnvBlocksField, false) ?? [];
    for (const block of blocks) {
      if (block.kind !== "tikz") continue;
      const key = tikzDirtyKeyFromTitle(block.title);
      if (!key) continue;
      if (!changesTouchRange(tr.changes, block.bodyFrom, block.bodyTo)) continue;
      if (!next) next = new Set(value);
      next.add(key);
    }
    return next ?? value;
  },
});

function orgEnvBlocksFromState(state: EditorState): readonly OrgEnvBlock[] {
  return state.field(orgEnvBlocksField, false) ?? scanOrgEnvBlocks(state.doc.toString(), 0, 0, blockExtraExcludedRanges(state));
}

function canMapOrgEnvBlocks(doc: Text, blocks: readonly OrgEnvBlock[], changes: ChangeSet): boolean {
  if (changesMightAffectFencedCodeRanges(doc, changes)) return false;
  let canMap = true;
  changes.iterChanges((fromA, toA, _fromB, _toB, inserted) => {
    if (!canMap) return;
    const removed = doc.sliceString(fromA, toA);
    const added = inserted.toString();
    if (/^\s*#\+(?:begin|end)\b/im.test(removed) || /^\s*#\+(?:begin|end)\b/im.test(added)) {
      canMap = false;
      return;
    }
    if (blocks.some((block) => (
      (fromA <= block.openTo && toA >= block.openFrom)
      || (fromA <= block.closeTo && toA >= block.closeFrom)
    ))) {
      canMap = false;
      return;
    }
    if (blocks.some((block) => block.kind === "meta" && fromA <= block.to && toA >= block.from)) {
      canMap = false;
    }
  });
  return canMap;
}

function mapOrgEnvBlock(block: OrgEnvBlock, changes: ChangeSet, doc: Text): OrgEnvBlock {
  const bodyFrom = changes.mapPos(block.bodyFrom);
  const bodyTo = changes.mapPos(block.bodyTo);
  return {
    ...block,
    from: changes.mapPos(block.from),
    to: changes.mapPos(block.to),
    openFrom: changes.mapPos(block.openFrom),
    openTo: changes.mapPos(block.openTo),
    bodyFrom,
    bodyTo,
    closeFrom: changes.mapPos(block.closeFrom),
    closeTo: changes.mapPos(block.closeTo),
    body: changes.touchesRange(block.bodyFrom, block.bodyTo)
      ? doc.sliceString(bodyFrom, bodyTo)
      : block.body,
    titleAnchor: changes.mapPos(block.titleAnchor),
  };
}

function firstChangedPosition(changes: ChangeSet): number {
  let first = Number.POSITIVE_INFINITY;
  changes.iterChanges((fromA) => {
    first = Math.min(first, fromA);
  });
  return first;
}

function mapOrgEnvBlocks(blocks: readonly OrgEnvBlock[], changes: ChangeSet, doc: Text): readonly OrgEnvBlock[] {
  const firstChanged = firstChangedPosition(changes);
  return blocks.map((block) => block.to < firstChanged ? block : mapOrgEnvBlock(block, changes, doc));
}

function patchOrgEnvBlocksForTitleChange(
  oldDoc: Text,
  newDoc: Text,
  blocks: readonly OrgEnvBlock[],
  changes: ChangeSet,
): OrgEnvTitlePatch | null {
  let changeCount = 0;
  let fromA = 0;
  let toA = 0;
  let insertedText = "";
  changes.iterChanges((changeFromA, changeToA, _nextFrom, _nextTo, inserted) => {
    changeCount++;
    fromA = changeFromA;
    toA = changeToA;
    insertedText = inserted.toString();
  });
  if (changeCount !== 1) return null;

  const removed = oldDoc.sliceString(fromA, toA);
  if (removed.includes("\n") || insertedText.includes("\n")) return null;
  if (/^\s*#\+(?:begin|end)\b/im.test(removed) || /^\s*#\+(?:begin|end)\b/im.test(insertedText)) {
    return null;
  }

  const touchedBlocks = blocks.filter((block) => (
    block.kind !== "meta"
    && fromA <= block.openTo
    && toA >= block.openFrom
  ));
  if (touchedBlocks.length !== 1) return null;

  const oldBlock = touchedBlocks[0]!;
  const oldLine = oldDoc.lineAt(oldBlock.openFrom);
  if (oldLine.from !== oldBlock.openFrom || oldLine.to !== oldBlock.openTo) return null;
  const oldInfo = parseOrgEnvOpenLine(oldLine.text);
  if (!oldInfo || oldInfo.kind !== oldBlock.kind) return null;

  const changeLine = oldDoc.lineAt(Math.min(fromA, oldDoc.length));
  const changeEndLine = oldDoc.lineAt(Math.min(Math.max(fromA, toA), oldDoc.length));
  if (changeLine.number !== oldLine.number || changeEndLine.number !== oldLine.number) return null;

  if (oldBlock.title.length > 0) {
    if (fromA < oldBlock.titleAnchor || toA > oldBlock.openTo) return null;
  } else if (fromA !== oldBlock.openTo || !/^[ \t]/.test(insertedText)) {
    return null;
  }

  const mappedBlocks = mapOrgEnvBlocks(blocks, changes, newDoc);
  const touchedIndex = blocks.indexOf(oldBlock);
  const mappedBlock = mappedBlocks[touchedIndex]!;
  const newLine = newDoc.lineAt(mappedBlock.openFrom);
  if (newLine.from !== mappedBlock.openFrom) return null;
  const newInfo = parseOrgEnvOpenLine(newLine.text);
  if (!newInfo || newInfo.kind !== oldBlock.kind) return null;

  const newBlock: OrgEnvBlock = {
    ...mappedBlock,
    openFrom: newLine.from,
    openTo: newLine.to,
    bodyFrom: Math.min(newLine.to + 1, newDoc.length),
    kind: newInfo.kind,
    title: newInfo.title,
    titleAnchor: newLine.from + newInfo.titleAnchor,
  };
  const nextBlocks = mappedBlocks.map((block, index) => index === touchedIndex ? newBlock : block);
  return { blocks: nextBlocks, newBlock };
}

class OrgEnvRailPlugin {
  layer: HTMLElement;

  constructor(view: EditorView) {
    this.layer = document.createElement("div");
    this.layer.className = "cm-org-env-rail-layer";
    view.dom.append(this.layer);
    this.schedule(view);
  }

  update(update: ViewUpdate): void {
    if (update.docChanged || update.viewportChanged || update.geometryChanged) {
      this.schedule(update.view);
    }
  }

  destroy(): void {
    this.layer.remove();
  }

  private schedule(view: EditorView): void {
    view.requestMeasure({
      read: () => measureOrgEnvRails(view),
      write: (rails) => this.writeRails(rails),
    });
  }

  private writeRails(rails: OrgEnvRailMeasure[]): void {
    const next = document.createDocumentFragment();
    for (const rail of rails) {
      if (rail.height <= 0) continue;
      const div = document.createElement("div");
      div.className = "cm-org-env-rail";
      div.dataset.orgEnvKind = rail.kind;
      div.dataset.orgEnvDepth = String(rail.depth);
      div.style.left = `${rail.left}px`;
      div.style.top = `${rail.top}px`;
      div.style.height = `${rail.height}px`;
      next.append(div);
    }
    this.layer.replaceChildren(next);
  }
}

function buildOrgEnvBodyLineDecoRanges(
  state: EditorState,
  startLine = 1,
  endLine = state.doc.lines,
): CMRange<Decoration>[] {
  const decos: CMRange<Decoration>[] = [];
  const lineBlocks = new Map<number, OrgEnvBlock>();
  const doc = state.doc;
  const firstLine = Math.max(1, startLine);
  const lastLine = Math.min(doc.lines, endLine);
  if (firstLine > lastLine) return decos;
  const windowFrom = doc.line(firstLine).from;
  const windowTo = doc.line(lastLine).to;

  for (const block of orgEnvBlocksFromState(state)) {
    if (block.kind === "meta") continue;
    if (block.kind === "ceil") continue;
    if (block.kind === "fold" && !selectionTouchesRange(state, block.from, block.to)) continue;
    if (block.bodyTo < windowFrom || block.bodyFrom > windowTo) continue;
    const fromLine = doc.lineAt(Math.max(block.bodyFrom, windowFrom));
    const toLine = doc.lineAt(Math.min(block.bodyTo, windowTo));
    for (let lineNum = fromLine.number; lineNum <= toLine.number; lineNum++) {
      const line = doc.line(lineNum);
      if (line.from >= block.closeFrom) break;
      if (line.to < block.bodyFrom) continue;
      const current = lineBlocks.get(line.from);
      if (!current || block.depth >= current.depth) {
        lineBlocks.set(line.from, block);
      }
    }
  }

  for (const [lineFrom, block] of lineBlocks) {
    decos.push(
      Decoration.line({
        attributes: {
          class: "cm-org-env-line cm-org-env-body-line",
          "data-org-env-kind": block.kind,
          "data-org-env-depth": String(block.depth),
          style: `--org-env-depth: ${block.depth};`,
        },
      }).range(lineFrom),
    );
  }

  decos.sort((a, b) => a.from - b.from || a.to - b.to);
  return decos;
}

function buildOrgEnvBodyLineDecos(state: EditorState): DecorationSet {
  return Decoration.set(buildOrgEnvBodyLineDecoRanges(state), true);
}

const orgEnvBodyLineDecorations = StateField.define<DecorationSet>({
  create: (state) => buildOrgEnvBodyLineDecos(state),
  update(value, tr) {
    if (tr.docChanged) {
      const blocks = tr.startState.field(orgEnvBlocksField, false) ?? orgEnvBlocksFromState(tr.startState);
      if (
        canMapOrgEnvBlocks(tr.startState.doc, blocks, tr.changes)
        || patchOrgEnvBlocksForTitleChange(tr.startState.doc, tr.state.doc, blocks, tr.changes)
      ) {
        const mapped = value.map(tr.changes);
        return changesContainNewline(tr.startState.doc, tr.changes)
          ? patchOrgEnvBodyLineDecosNearChanges(tr.state, mapped, tr.changes)
          : mapped;
      }
      return buildOrgEnvBodyLineDecos(tr.state);
    }
    return value.map(tr.changes);
  },
  provide: (f) => EditorView.decorations.from(f),
});

function patchOrgEnvBodyLineDecosNearChanges(
  state: EditorState,
  mapped: DecorationSet,
  changes: ChangeSet,
): DecorationSet {
  let fromB = Number.POSITIVE_INFINITY;
  let toB = 0;
  changes.iterChanges((_fromA, _toA, nextFrom, nextTo) => {
    fromB = Math.min(fromB, nextFrom);
    toB = Math.max(toB, nextTo);
  });
  if (!Number.isFinite(fromB)) return mapped;
  const centerFrom = state.doc.lineAt(Math.min(fromB, state.doc.length)).number;
  const centerTo = state.doc.lineAt(Math.min(toB, state.doc.length)).number;
  const startLine = Math.max(1, centerFrom - 1);
  const endLine = Math.min(state.doc.lines, centerTo + 1);
  const affectedFrom = state.doc.line(startLine).from;
  const affectedTo = state.doc.line(endLine).to;
  return mapped
    .update({ filterFrom: affectedFrom, filterTo: affectedTo, filter: () => false })
    .update({ add: buildOrgEnvBodyLineDecoRanges(state, startLine, endLine), sort: true });
}

function measureOrgEnvRails(view: EditorView): OrgEnvRailMeasure[] {
  const viewportBlocks = view.viewportLineBlocks;
  if (viewportBlocks.length === 0) return [];

  const viewRect = view.dom.getBoundingClientRect();
  const contentRect = view.contentDOM.getBoundingClientRect();
  const rootFontSize = Number.parseFloat(getComputedStyle(document.documentElement).fontSize) || 16;
  const depthStep = rootFontSize * 1.1;
  const baseLeft = contentRect.left - viewRect.left;
  const docTop = view.documentTop - viewRect.top;
  const visibleFrom = Math.min(...view.visibleRanges.map((range) => range.from));
  const visibleTo = Math.max(...view.visibleRanges.map((range) => range.to));
  const visibleTop = docTop + viewportBlocks[0]!.top;
  const visibleBottom = docTop + viewportBlocks[viewportBlocks.length - 1]!.bottom;

  return orgEnvBlocksFromState(view.state)
    .filter((block) => (
      block.kind !== "meta"
      && block.kind !== "comment"
      && block.kind !== "fold"
      && block.kind !== "html"
      && block.kind !== "tikz"
      && block.kind !== "ceil"
      && block.openFrom <= visibleTo
      && block.closeTo >= visibleFrom
    ))
    .map((block) => {
      const openVisible = block.openFrom >= visibleFrom && block.openFrom <= visibleTo;
      const closeVisible = block.closeTo >= visibleFrom && block.closeTo <= visibleTo;
      const top = openVisible ? docTop + view.lineBlockAt(block.openFrom).top : visibleTop;
      const bottom = closeVisible
        ? docTop + view.lineBlockAt(block.closeFrom).bottom
        : visibleBottom;
      return {
        kind: block.kind,
        depth: block.depth,
        top,
        height: Math.max(0, bottom - top),
        left: baseLeft + block.depth * depthStep,
      };
    });
}

// ---------------------------------------------------------------------------
// Decoration builder (full-doc scan — these constructs are sparse)
// ---------------------------------------------------------------------------

function addOrgEnvBlockExtraDecos(
  decos: CMRange<Decoration>[],
  occupied: Array<[number, number]> | null,
  state: EditorState,
  block: OrgEnvBlock,
): void {
  if (block.kind === "meta") {
    decos.push(
      Decoration.replace({
        widget: new MetaWidget(block.body, block.from, block.to),
        block: true,
      }).range(block.from, block.to),
    );
    occupied?.push([block.from, block.to]);
    return;
  }
  if (block.kind === "html") {
    decos.push(
      Decoration.replace({
        widget: new HtmlWidget(block.body, block.from, block.to),
        block: true,
      }).range(block.from, block.to),
    );
    occupied?.push([block.from, block.to]);
    return;
  }
  if (block.kind === "tikz") {
    const dirtyTikzBlocks = state.field(dirtyTikzBlocksField, false);
    const dirtyKey = tikzDirtyKeyFromTitle(block.title);
    decos.push(
      Decoration.replace({
        widget: new TikzWidget(block.title, block.body, block.from, block.to, Boolean(dirtyKey && dirtyTikzBlocks?.has(dirtyKey))),
        block: true,
      }).range(block.from, block.to),
    );
    occupied?.push([block.from, block.to]);
    return;
  }
  if (block.kind === "comment" && !selectionTouchesRange(state, block.from, block.to)) {
    decos.push(
      Decoration.replace({
        widget: new CommentWidget(block.title, block.body, block.from, block.to, block.depth),
        block: true,
      }).range(block.from, block.to),
    );
    occupied?.push([block.from, block.to]);
    return;
  }
  if (block.kind === "fold" && !selectionTouchesRange(state, block.from, block.to)) {
    decos.push(
      Decoration.replace({
        widget: new FoldWidget(block.title, block.body, block.from, block.to, block.depth),
        block: true,
      }).range(block.from, block.to),
    );
    occupied?.push([block.from, block.to]);
    return;
  }
  addOrgEnvBoundaryDecos(decos, state, block);
}

function buildBlockExtraDecoRanges(
  state: EditorState,
  windowFrom = 0,
  windowTo = state.doc.length,
): CMRange<Decoration>[] {
  const decos: CMRange<Decoration>[] = [];
  const occupied: Array<[number, number]> = [];
  const sel = state.selection.main;
  const excludedRanges = blockExtraExcludedRanges(state);
  const headings = tocHeadingsFromState(state);
  const ranges = state.field(blockExtraRangesField, false) ?? scanBlockExtraRanges(state.doc, excludedRanges);
  const foldState = state.field(tocFoldField, false) ?? new Map<string, boolean>();

  // ── [toc] ──────────────────────────────────────────────────────────────
  for (const range of ranges.toc) {
    if (range.to < windowFrom || range.from > windowTo) continue;
    if (rangeOverlapsAny(range.from, range.to, excludedRanges)) continue;
    if (!(sel.from <= range.to && sel.to >= range.from)) {
      decos.push(
        Decoration.replace({ widget: new TocWidget(headings, foldState), block: true }).range(range.from, range.to),
      );
      occupied.push([range.from, range.to]);
    }
  }

  // ── @@part / @@section semantic headings ──────────────────────────────
  for (const range of ranges.semanticHeadings) {
    if (range.to < windowFrom || range.from > windowTo) continue;
    if (rangeOverlapsAny(range.from, range.to, excludedRanges)) continue;
    if (occupied.some(([from, to]) => range.from < to && range.to > from)) continue;
    if (sel.from >= range.from && sel.from <= range.to) {
      decos.push(Decoration.mark({ class: "syntax-hint" }).range(range.from, range.to));
      continue;
    }
    decos.push(
      Decoration.replace({ widget: new SemanticHeadingWidget(range.outline, range.from, range.to), block: true }).range(range.from, range.to),
    );
    occupied.push([range.from, range.to]);
  }

  // ── @@cell(...) [id] Jupyter cells ────────────────────────────────────
  for (const range of ranges.ceilCommands) {
    if (range.to < windowFrom || range.from > windowTo) continue;
    if (rangeOverlapsAny(range.from, range.to, excludedRanges)) continue;
    if (occupied.some(([from, to]) => range.from < to && range.to > from)) continue;
    decos.push(
      Decoration.replace({ widget: new CeilCommandWidget(range), block: true }).range(range.from, range.to),
    );
    occupied.push([range.from, range.to]);
  }

  // ── org-env #+begin … #+end ────────────────────────────────────────────
  // Most org-env bodies remain normal CM6 markdown. Specialized blocks such as
  // meta/html/tikz/ceil replace the whole source region with purpose-built UI.
  const orgEnvBlocks = orgEnvBlocksFromState(state);
  for (const block of orgEnvBlocks) {
    if (block.to < windowFrom || block.from > windowTo) continue;
    addOrgEnvBlockExtraDecos(decos, occupied, state, block);
  }

  // ── YAML front matter (only at offset 0) ───────────────────────────────
  const frontMatter = ranges.frontMatter;
  if (frontMatter) {
    const { from, to, body } = frontMatter;
    if (to >= windowFrom && from <= windowTo && !rangeOverlapsAny(from, to, excludedRanges) && !(sel.from < to && sel.to > from)) {
      decos.push(
        Decoration.replace({ widget: new FrontMatterWidget(body, from, to), block: true }).range(from, to),
      );
      occupied.push([from, to]);
    }
  }

  // ── Horizontal rule ────────────────────────────────────────────────────
  for (const range of ranges.hrs) {
    if (range.to < windowFrom || range.from > windowTo) continue;
    if (rangeOverlapsAny(range.from, range.to, excludedRanges)) continue;
    if (occupied.some(([from, to]) => range.from < to && range.to > from)) continue;
    if (sel.from >= range.from && sel.from <= range.to) {
      decos.push(Decoration.mark({ class: "syntax-hint" }).range(range.from, range.to));
      continue;
    }
    decos.push(
      Decoration.replace({ widget: new HorizontalRuleWidget(range.from, range.to), block: true }).range(range.from, range.to),
    );
  }

  decos.sort((a, b) => a.from - b.from || a.to - b.to);
  return decos;
}

function buildBlockExtraDecos(state: EditorState): DecorationSet {
  const decos = buildBlockExtraDecoRanges(state);
  return Decoration.set(decos, true);
}

function buildTocWidgetDecoRanges(
  state: EditorState,
  ranges = state.field(blockExtraRangesField, false)?.toc ?? [],
): CMRange<Decoration>[] {
  if (ranges.length === 0) return [];
  const decos: CMRange<Decoration>[] = [];
  const sel = state.selection.main;
  const excludedRanges = blockExtraExcludedRanges(state);
  const headings = tocHeadingsFromState(state);
  const foldState = state.field(tocFoldField, false) ?? new Map<string, boolean>();
  for (const range of ranges) {
    if (rangeOverlapsAny(range.from, range.to, excludedRanges)) continue;
    if (sel.from <= range.to && sel.to >= range.from) continue;
    decos.push(
      Decoration.replace({ widget: new TocWidget(headings, foldState), block: true }).range(range.from, range.to),
    );
  }
  return decos;
}

function changesContainNewline(doc: Text, changes: ChangeSet): boolean {
  let found = false;
  changes.iterChanges((fromA, toA, _fromB, _toB, inserted) => {
    if (found) return;
    found = doc.sliceString(fromA, toA).includes("\n") || inserted.toString().includes("\n");
  });
  return found;
}

function changesTouchRange(changes: ChangeSet, from: number, to: number): boolean {
  let touched = false;
  changes.iterChanges((fromA, toA) => {
    if (touched) return;
    touched = fromA <= to && toA >= from;
  });
  return touched;
}

function activeBlockExtraKey(state: EditorState): string {
  const sel = state.selection.main;
  const parts: string[] = [];
  const ranges = state.field(blockExtraRangesField, false) ?? scanBlockExtraRanges(state.doc, blockExtraExcludedRanges(state));
  const blocks = orgEnvBlocksFromState(state);

  for (const range of ranges.toc) {
    if (sel.from <= range.to && sel.to >= range.from) parts.push(`toc:${range.from}:${range.to}`);
  }
  for (const range of ranges.semanticHeadings) {
    if (sel.from <= range.to && sel.to >= range.from) parts.push(`semantic:${range.from}:${range.to}`);
  }
  for (const range of ranges.ceilCommands) {
    if (sel.from <= range.to && sel.to >= range.from) parts.push(`ceilcmd:${range.from}:${range.to}`);
  }
  if (ranges.frontMatter && sel.from < ranges.frontMatter.to && sel.to > ranges.frontMatter.from) {
    parts.push(`front:${ranges.frontMatter.from}:${ranges.frontMatter.to}`);
  }
  for (const range of ranges.hrs) {
    if (sel.from >= range.from && sel.from <= range.to) parts.push(`hr:${range.from}:${range.to}`);
  }
  for (const block of blocks) {
    if ((block.kind === "comment" || block.kind === "fold") && selectionTouchesRange(state, block.from, block.to)) {
      parts.push(`${block.kind}:${block.from}:${block.to}`);
      continue;
    }
    if (selectionTouchesRange(state, block.openFrom, block.openTo)) {
      parts.push(`org-open:${block.openFrom}:${block.openTo}:${block.from}:${block.to}`);
    }
    if (selectionTouchesRange(state, block.closeFrom, block.closeTo)) {
      parts.push(`org-close:${block.closeFrom}:${block.closeTo}:${block.from}:${block.to}`);
    }
  }
  return parts.join("|");
}

function blockExtraPatchRangesFromKey(key: string): Array<{ from: number; to: number }> {
  if (!key) return [];
  return key.split("|")
    .map((part) => {
      const pieces = part.split(":");
      const from = Number(pieces[pieces.length - 2]);
      const to = Number(pieces[pieces.length - 1]);
      return Number.isFinite(from) && Number.isFinite(to) && from <= to ? { from, to } : null;
    })
    .filter((range): range is { from: number; to: number } => Boolean(range));
}

function mergeBlockExtraPatchRanges(ranges: Array<{ from: number; to: number }>): Array<{ from: number; to: number }> {
  const sorted = ranges.sort((a, b) => a.from - b.from || a.to - b.to);
  const merged: Array<{ from: number; to: number }> = [];
  for (const range of sorted) {
    const previous = merged[merged.length - 1];
    if (previous && range.from <= previous.to) {
      previous.to = Math.max(previous.to, range.to);
    } else {
      merged.push({ ...range });
    }
  }
  return merged;
}

function patchBlockExtraDecosForSelectionChange(
  state: EditorState,
  current: DecorationSet,
  oldKey: string,
  newKey: string,
): DecorationSet {
  const ranges = mergeBlockExtraPatchRanges([
    ...blockExtraPatchRangesFromKey(oldKey),
    ...blockExtraPatchRangesFromKey(newKey),
  ]);
  if (ranges.length === 0) return current;

  let next = current;
  const add: CMRange<Decoration>[] = [];
  for (const range of ranges) {
    next = next.update({ filterFrom: range.from, filterTo: range.to, filter: () => false });
    add.push(...buildBlockExtraDecoRanges(state, range.from, range.to));
  }
  return next.update({ add, sort: true });
}

function canMapBlockExtraDecos(state: EditorState, changes: ChangeSet): boolean {
  const ranges = state.field(blockExtraRangesField, false) ?? scanBlockExtraRanges(state.doc, blockExtraExcludedRanges(state));
  const blocks = state.field(orgEnvBlocksField, false) ?? orgEnvBlocksFromState(state);

  if (!canMapBlockExtraRanges(state.doc, changes, ranges)) return false;
  if (!canMapOrgEnvBlocks(state.doc, blocks, changes)) return false;

  if (ranges.toc.some((range) => changesTouchRange(changes, range.from, range.to))) return false;
  if (ranges.semanticHeadings.some((range) => changesTouchRange(changes, range.from, range.to))) return false;
  if (ranges.ceilCommands.some((range) => changesTouchRange(changes, range.from, range.to))) return false;
  if (ranges.hrs.some((range) => changesTouchRange(changes, range.from, range.to))) return false;
  if (ranges.frontMatter && changesTouchRange(changes, ranges.frontMatter.from, ranges.frontMatter.to)) return false;
  if (blocks.some((block) => (
    (block.kind === "meta" || block.kind === "comment" || block.kind === "fold" || block.kind === "html" || block.kind === "tikz")
    && changesTouchRange(changes, block.from, block.to)
  ))) {
    return false;
  }

  return true;
}

function patchBlockExtraDecosForOrgEnvTitleChange(
  state: EditorState,
  mapped: DecorationSet,
  block: OrgEnvBlock,
): DecorationSet {
  const decos: CMRange<Decoration>[] = [];
  addOrgEnvBlockExtraDecos(decos, null, state, block);
  decos.sort((a, b) => a.from - b.from || a.to - b.to);

  const fullBlockWidgetActive = block.kind === "meta"
    || block.kind === "html"
    || block.kind === "tikz"
    || block.kind === "ceil"
    || ((block.kind === "comment" || block.kind === "fold") && !selectionTouchesRange(state, block.from, block.to));
  let next = fullBlockWidgetActive
    ? mapped.update({ filterFrom: block.from, filterTo: block.to, filter: () => false })
    : mapped
        .update({ filterFrom: block.openFrom, filterTo: block.openTo, filter: () => false })
        .update({ filterFrom: block.closeFrom, filterTo: block.closeTo, filter: () => false });
  next = next.update({ add: decos, sort: true });
  return next;
}

function patchTocWidgetDecos(
  state: EditorState,
  current: DecorationSet,
): DecorationSet {
  const ranges = state.field(blockExtraRangesField, false)?.toc ?? [];
  if (ranges.length === 0) return current;
  let next = current;
  for (const range of ranges) {
    next = next.update({ filterFrom: range.from, filterTo: range.to, filter: () => false });
  }
  const add = buildTocWidgetDecoRanges(state, ranges);
  return next.update({ add, sort: true });
}

function scanFrontMatter(doc: Text): { from: number; to: number; body: string } | null {
  if (doc.lines < 2 || doc.line(1).text.trim() !== "---") return null;
  const bodyLines: string[] = [];
  for (let lineNum = 2; lineNum <= doc.lines; lineNum++) {
    const line = doc.line(lineNum);
    if (line.text.trim() === "---") {
      return { from: 0, to: line.to, body: bodyLines.join("\n") };
    }
    bodyLines.push(line.text);
  }
  return null;
}

// ---------------------------------------------------------------------------
// StateField export
// ---------------------------------------------------------------------------

const blockExtrasDecorations = StateField.define<DecorationSet>({
  create: (state) => buildBlockExtraDecos(state),
  update(value, tr) {
    if (tr.effects.some((effect) => effect.is(tocFoldEffect))) {
      return patchTocWidgetDecos(tr.state, value);
    }
    if (tr.docChanged) {
      if (canMapBlockExtraDecos(tr.startState, tr.changes)) {
        const mapped = value.map(tr.changes);
        return (tr.state.field(blockExtraRangesField, false)?.toc.length ?? 0) > 0
          && tocContentSignature(tr.startState) !== tocContentSignature(tr.state)
          ? patchTocWidgetDecos(tr.state, mapped)
          : mapped;
      }
      const blocks = tr.startState.field(orgEnvBlocksField, false) ?? orgEnvBlocksFromState(tr.startState);
      const titlePatch = patchOrgEnvBlocksForTitleChange(tr.startState.doc, tr.state.doc, blocks, tr.changes);
      return titlePatch
        ? patchBlockExtraDecosForOrgEnvTitleChange(tr.state, value.map(tr.changes), titlePatch.newBlock)
        : buildBlockExtraDecos(tr.state);
    }
    if (tr.selection != null) {
      const oldKey = activeBlockExtraKey(tr.startState);
      const newKey = activeBlockExtraKey(tr.state);
      if (oldKey !== newKey) return patchBlockExtraDecosForSelectionChange(tr.state, value, oldKey, newKey);
    }
    return value.map(tr.changes);
  },
  provide: (f) => EditorView.decorations.from(f),
});

const orgEnvRailExtension = ViewPlugin.fromClass(OrgEnvRailPlugin);

export const blockExtrasExtension: Extension = [
  fencedCodeRangesExtension,
  blockExtraRangesField,
  tocFoldField,
  orgEnvBlocksField,
  dirtyTikzBlocksField,
  blockExtrasDecorations,
  orgEnvBodyLineDecorations,
  orgEnvRailExtension,
];
