/**
 * CodeMirror 6 editor kernel — Phase 1 minimum viable skeleton.
 *
 * Implements the public `Editor` interface.
 * CM6 doc IS the markdown source, so:
 *   - no parser/serializer needed (getMarkdown = doc.toString())
 *   - no source-mode toggle (the whole doc is always source)
 *   - CM6 positions == markdown byte offsets
 *
 * CM6 doc positions are the markdown source offsets used by the public API.
 */

import { Compartment, EditorSelection, EditorState, Transaction, type Extension } from "@codemirror/state";
import {
  EditorView,
  keymap,
  highlightActiveLine,
  rectangularSelection,
} from "@codemirror/view";
import {
  history,
  undo as cmUndo,
  redo as cmRedo,
  defaultKeymap,
  historyKeymap,
  indentWithTab,
} from "@codemirror/commands";
import { closeBrackets } from "@codemirror/autocomplete";
import { syntaxTree } from "@codemirror/language";
import { markdown, markdownLanguage } from "@codemirror/lang-markdown";
import { livePreviewExtension } from "./live-preview.ts";
import { disposeHighlightWorker } from "../code-highlight-async.ts";
import { disposeMathRuntime } from "../math-render.ts";
import { mathExtension } from "./widgets/math.ts";
import { fencedCodeExtension } from "./widgets/fenced-code.ts";
import { taskListExtension } from "./widgets/task-list.ts";
import { imageExtension } from "./widgets/image.ts";
import { blockExtrasExtension, orgEnvExitTarget } from "./widgets/block-extras.ts";
import { inlineCommandsExtension } from "./widgets/inline-commands.ts";
import {
  runCommandCM6,
  getBlockContextCM6,
  createQuickInsertRegistry,
  exitEmptyMarkdownBlock,
  indentMarkdownList,
} from "./commands.ts";
import { markdownFromClipboard } from "../clipboard.ts";
import { renderMarkdownHTML } from "../render-html.ts";
import { blockMathRangesExtension, getBlockMathRanges, positionInsideAnyRange } from "./math-ranges.ts";
import { scanInlineMathRanges } from "../inline-math.ts";
import { findHighlightExtension } from "./find-highlight.ts";
import { roamLinkStatusExtension } from "./roam-link-status.ts";
import { tocIndexExtension } from "./toc-index.ts";
import { proseDiagnosticsExtension } from "./prose-diagnostics.ts";
import { scheduleViewportDecorationRefresh } from "./viewport-refresh.ts";

import type { SyntaxNode } from "@lezer/common";
import type {
  Editor,
  EditorBlockContext,
  EditorCommand,
  EditorOptions,
  QuickInsertItem,
  QuickInsertProvider,
  SetMarkdownOptions,
  WritingModeOptions,
} from "../editor-api.ts";

function sourceRangeElement(target: EventTarget | null): HTMLElement | null {
  if (target instanceof Element) {
    return target.closest<HTMLElement>("[data-cm-source-from][data-cm-source-to]");
  }
  if (target instanceof Text) {
    return target.parentElement?.closest<HTMLElement>("[data-cm-source-from][data-cm-source-to]") ?? null;
  }
  return null;
}

function mathBlockSourceAnchor(docText: string, from: number, to: number): number {
  const raw = docText.slice(from, to);
  const open = raw.indexOf("$$");
  if (open < 0) return Math.min(to - 1, from + 1);
  const firstNewline = raw.indexOf("\n", open + 2);
  if (firstNewline < 0) return Math.min(to - 1, from + 2);
  return Math.min(to - 1, from + firstNewline + 1);
}

function sourceAnchorForClick(source: HTMLElement, event: MouseEvent, from: number, to: number): number {
  const explicit = Number(source.dataset.cmSourceAnchor);
  if (Number.isFinite(explicit)) {
    return Math.max(from, Math.min(to, explicit));
  }
  const rect = source.getBoundingClientRect();
  const innerFrom = Math.min(to - 1, from + 1);
  const innerTo = Math.max(innerFrom, to - 1);
  if (rect.width <= 0 || innerTo <= innerFrom) return innerFrom;
  const ratio = Math.min(1, Math.max(0, (event.clientX - rect.left) / rect.width));
  return Math.round(innerFrom + ratio * (innerTo - innerFrom));
}

function eventTargetElement(target: EventTarget | Node | null): Element | null {
  if (target instanceof Element) return target;
  if (target instanceof Text) return target.parentElement;
  return null;
}

type NativeCaretPosition = { node: Node; offset: number };

function nativeCaretPositionFromPoint(x: number, y: number): NativeCaretPosition | null {
  const doc = document as Document & {
    caretPositionFromPoint?: (x: number, y: number) => { offsetNode: Node; offset: number } | null;
    caretRangeFromPoint?: (x: number, y: number) => Range | null;
  };
  const caret = doc.caretPositionFromPoint?.(x, y);
  if (caret?.offsetNode) return { node: caret.offsetNode, offset: caret.offset };
  const range = doc.caretRangeFromPoint?.(x, y);
  if (range) return { node: range.startContainer, offset: range.startOffset };
  return null;
}

function hasWrappedLayout(view: EditorView): boolean {
  return Boolean(view.dom.querySelector(".aaronnote-image-wrap, .aaronnote-table-wrap, .aaronnote-diagram-wrap"));
}

function realRectContainsY(el: Element, y: number): boolean {
  const rects = Array.from(el.getClientRects()).filter((rect) => rect.width > 0 || rect.height > 0);
  if (rects.length === 0) return true;
  return rects.some((rect) => y >= rect.top - 4 && y <= rect.bottom + 4);
}

export function calibrateWrappedLayoutClick(view: EditorView, event: MouseEvent): boolean {
  if (event.button !== 0 || event.shiftKey || event.metaKey || event.ctrlKey || event.altKey) return false;
  const target = eventTargetElement(event.target);
  if (
    target?.closest("input, textarea, select, button, a, [contenteditable='true'], .cm-diagram-toolbar, .cm-diagram-interactive svg")
    || sourceRangeElement(event.target)
    || !hasWrappedLayout(view)
  ) {
    return false;
  }

  const caret = nativeCaretPositionFromPoint(event.clientX, event.clientY);
  if (!caret || !view.contentDOM.contains(caret.node)) return false;
  const line = eventTargetElement(caret.node)?.closest<HTMLElement>(".cm-line");
  if (!line || !realRectContainsY(line, event.clientY)) return false;

  let anchor: number;
  try {
    anchor = view.posAtDOM(caret.node, caret.offset);
  } catch {
    return false;
  }
  if (!Number.isFinite(anchor)) return false;

  const mapped = view.posAtCoords({ x: event.clientX, y: event.clientY });
  if (mapped != null && Math.abs(mapped - anchor) <= 1) return false;

  event.preventDefault();
  event.stopPropagation();
  window.setTimeout(() => {
    if (!view.dom.isConnected) return;
    view.dispatch({ selection: { anchor } });
    window.setTimeout(() => {
      if (view.dom.isConnected) view.focus();
    }, 0);
  }, 0);
  return true;
}

function hrefFromLinkNode(state: EditorState, from: number, to: number): string | null {
  let href: string | null = null;
  syntaxTree(state).iterate({
    from,
    to,
    enter(node) {
      if (href) return false;
      if (node.name !== "URL") return;
      href = state.doc.sliceString(node.from, node.to).trim();
      return false;
    },
  });
  return href;
}

function markdownHrefFromLineAt(state: EditorState, pos: number): string | null {
  const line = state.doc.lineAt(Math.max(0, Math.min(pos, state.doc.length)));
  const re = /!?\[[^\]\n]*\]\(([^)\n]+)\)/g;
  let match: RegExpExecArray | null;
  while ((match = re.exec(line.text)) !== null) {
    const from = line.from + match.index;
    const to = from + match[0].length;
    if (pos < from || pos > to) continue;
    return (match[1] || "")
      .replace(/\s+"[^"]*"\s*$/, "")
      .replace(/\s+'[^']*'\s*$/, "")
      .trim() || null;
  }
  return null;
}

export function markdownHrefAt(state: EditorState, pos: number): string | null {
  const docLen = state.doc.length;
  const clamped = Math.max(0, Math.min(pos, docLen));
  if (positionInsideAnyRange(clamped, getBlockMathRanges(state))) return null;
  const line = state.doc.lineAt(clamped);
  if (positionInsideAnyRange(clamped, scanInlineMathRanges(line.text, line.from))) return null;

  const wikilink = wikilinkHrefAt(state, pos);
  if (wikilink) return wikilink;
  const positions = clamped > 0 ? [clamped, clamped - 1] : [clamped];

  for (const targetPos of positions) {
    let node: SyntaxNode | null = syntaxTree(state).resolveInner(targetPos, -1);
    while (node) {
      if (node.name === "Link" || node.name === "Autolink" || node.name === "Image") {
        const href = hrefFromLinkNode(state, node.from, node.to);
        if (href) {
          if (jupyterHref(href)) {
            const lineHref = markdownHrefFromLineAt(state, clamped);
            if (lineHref && jupyterHref(lineHref)) return lineHref;
          }
          return href;
        }
      }
      if (node.name === "URL") {
        const href = state.doc.sliceString(node.from, node.to).trim();
        if (href) {
          if (jupyterHref(href)) {
            const lineHref = markdownHrefFromLineAt(state, clamped);
            if (lineHref && jupyterHref(lineHref)) return lineHref;
          }
          return href;
        }
      }
      node = node.parent;
    }
  }

  return markdownHrefFromLineAt(state, clamped);
}

function wikilinkHrefAt(state: EditorState, pos: number): string | null {
  const line = state.doc.lineAt(Math.max(0, Math.min(pos, state.doc.length)));
  const text = line.text;
  const re = /\[\[([^\]\n]+)\]\]/g;
  let match: RegExpExecArray | null;
  while ((match = re.exec(text)) !== null) {
    const from = line.from + match.index;
    const to = from + match[0].length;
    if (pos < from || pos > to) continue;
    const ref = match[1]?.trim();
    if (!ref) return null;
    return `roam://${encodeURIComponent(ref)}`;
  }
  return null;
}

function includeRefAt(state: EditorState, pos: number): string | null {
  const line = state.doc.lineAt(Math.max(0, Math.min(pos, state.doc.length)));
  const match = line.text.match(/^\s*@@include\s+(?:\[([^\]\n]+)\]|(\S+))/);
  const ref = (match?.[1] || match?.[2] || "").trim();
  if (!match || !ref) return null;
  const start = line.from + (match.index || 0);
  const end = start + match[0].length;
  return pos >= start && pos <= end ? ref : null;
}

function linkOpensNewWindow(href: string, event: MouseEvent): boolean {
  void href;
  return event.button === 1 && primaryLinkModifier(event);
}

function primaryLinkModifier(event: MouseEvent): boolean {
  if (event.metaKey && !event.ctrlKey) return true;
  return !/Mac/.test(navigator.platform) && event.ctrlKey && !event.metaKey;
}

function isLinkOpenMouseEvent(event: MouseEvent): boolean {
  if (event.shiftKey) return false;
  if (event.button !== 0 && event.button !== 1) return false;
  return primaryLinkModifier(event);
}

function openIncludeFromEvent(view: EditorView, event: MouseEvent): boolean {
  if (!isLinkOpenMouseEvent(event)) return false;
  const pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
  if (pos == null) return false;
  const ref = includeRefAt(view.state, pos);
  if (!ref) return false;

  event.preventDefault();
  event.stopPropagation();
  view.dom.dispatchEvent(new CustomEvent("aaronnote:book-include-open", {
    bubbles: true,
    detail: { ref, newWindow: linkOpensNewWindow(ref, event) },
  }));
  return true;
}

function openMarkdownLinkFromEvent(view: EditorView, event: MouseEvent): boolean {
  if (!isLinkOpenMouseEvent(event)) return false;
  const pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
  if (pos == null) return false;
  const href = markdownHrefAt(view.state, pos);
  if (!href) return false;

  event.preventDefault();
  event.stopPropagation();
  view.dom.dispatchEvent(new CustomEvent("aaronnote:open-url", {
    bubbles: true,
    detail: { href, newWindow: linkOpensNewWindow(href, event) },
  }));
  return true;
}

function previewMarkdownLinkFromEvent(view: EditorView, event: MouseEvent): boolean {
  if (!primaryLinkModifier(event)) return false;
  const pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
  if (pos == null) return false;
  const href = markdownHrefAt(view.state, pos);
  if (!href) return false;

  event.preventDefault();
  event.stopPropagation();
  view.dom.dispatchEvent(new CustomEvent("aaronnote:preview-url", {
    bubbles: true,
    detail: { href, x: event.clientX, y: event.clientY },
  }));
  return true;
}

function jupyterHref(href: string): boolean {
  return /\.ipynb(?:[?@#]|$)/i.test(String(href || "").trim());
}

function attachmentHref(href: string): boolean {
  const raw = String(href || "").trim();
  if (!raw || raw.startsWith("#")) return false;
  const protocol = raw.match(/^([A-Za-z][\w+.-]*):/)?.[1]?.toLowerCase();
  if (protocol && protocol !== "file") return false;
  const path = raw
    .replace(/^file:(?:\/\/)?/i, "")
    .split(/[?#]/, 1)[0]
    ?.trim() ?? "";
  return Boolean(path) && !/\.(?:md|markdown|typ)$/i.test(path);
}

function openAttachmentContextMenuFromEvent(view: EditorView, event: MouseEvent): boolean {
  if (primaryLinkModifier(event)) return previewMarkdownLinkFromEvent(view, event);
  const pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
  if (pos == null) return false;
  const href = markdownHrefAt(view.state, pos);
  if (!href || !attachmentHref(href)) return false;

  event.preventDefault();
  event.stopPropagation();
  view.dom.dispatchEvent(new CustomEvent("aaronnote:attachment-context-menu", {
    bubbles: true,
    detail: { href },
  }));
  return true;
}

// ---------------------------------------------------------------------------
// Public factory
// ---------------------------------------------------------------------------

export function createEditorCM6(host: HTMLElement, options: EditorOptions): Editor {
  const qiRegistry = createQuickInsertRegistry();
  const previewCompartment = new Compartment();
  let inSource = false;
  // Preserve the stable outer DOM shape so themes and layout CSS work
  // without coupling to the editor implementation.
  const wrap = document.createElement("div");
  wrap.className = "typora-web-wrap";
  const editorHost = document.createElement("div");
  editorHost.className = "typora-web-editor-host";
  wrap.append(editorHost);
  host.append(wrap);
  const caretFlash = document.createElement("div");
  caretFlash.className = "typora-web-caret-flash";
  caretFlash.hidden = true;
  document.body.append(caretFlash);

  const initialDoc = options.initialContent ?? "";
  const createState = (doc: string): EditorState => EditorState.create({
    doc,
    extensions: buildExtensions(options, previewCompartment, () => inSource),
  });

  const view = new EditorView({
    state: createState(initialDoc),
    parent: editorHost,
  });
  scheduleViewportDecorationRefresh(view);
  void document.fonts?.ready.then(() => {
    if (view.dom.isConnected) view.requestMeasure();
  });

  const onSourceWidgetMouseDown = (event: MouseEvent): void => {
    if (event.button !== 0 || event.shiftKey || event.metaKey || event.ctrlKey || event.altKey) {
      return;
    }
    const target = event.target;
    if (
      target instanceof Element
      && target.closest("input, textarea, select, button, a")
    ) {
      return;
    }
    if (
      target instanceof Element
      && target.closest(".cm-diagram-toolbar, .cm-diagram-interactive svg")
    ) {
      return;
    }
    const source = sourceRangeElement(event.target);
    if (!source) return;
    const openSource = source.dataset.cmOpenSource === "true";
    const mathBlock = source.dataset.cmMathBlock === "true";
    if (!openSource && !mathBlock) return;
    const from = Number(source.dataset.cmSourceFrom);
    const to = Number(source.dataset.cmSourceTo);
    if (!Number.isFinite(from) || !Number.isFinite(to) || from >= to) return;
    event.preventDefault();
    event.stopPropagation();
    event.stopImmediatePropagation();
    const anchor = mathBlock
      ? mathBlockSourceAnchor(view.state.doc.toString(), from, to)
      : sourceAnchorForClick(source, event, from, to);
    view.dispatch({ selection: { anchor }, scrollIntoView: true });
    view.focus();
    flashCaret();
  };
  view.contentDOM.addEventListener("mousedown", onSourceWidgetMouseDown, { capture: true });

  // ---------------------------------------------------------------------------
  // Internal helpers
  // ---------------------------------------------------------------------------

  function getMarkdown(): string {
    return view.state.doc.toString();
  }

  function dispatchWithSelect(
    from: number,
    to: number,
    text: string,
    select: "start" | "end" | "all" | undefined,
  ): { from: number; to: number } {
    const insertTo = from + text.length;
    const anchor =
      select === "start" ? from :
      select === "all"   ? from :
      insertTo; // "end" or undefined
    const head = select === "all" ? insertTo : anchor;
    view.dispatch({
      changes: { from, to, insert: text },
      selection: { anchor, head },
      scrollIntoView: true,
    });
    return { from: anchor, to: head };
  }

  // ---------------------------------------------------------------------------
  // Editor interface implementation
  // ---------------------------------------------------------------------------

  const editor: Editor = {
    getMarkdown,

    async getMarkdownAsync(): Promise<string> {
      return getMarkdown();
    },

    getHTML(): string {
      return renderMarkdownHTML(getMarkdown());
    },

    setMarkdown(md: string, setOptions: SetMarkdownOptions = {}): void {
      if (setOptions.history === "reset") {
        view.setState(createState(md));
        scheduleViewportDecorationRefresh(view);
        return;
      }
      const len = view.state.doc.length;
      view.dispatch({
        changes: { from: 0, to: len, insert: md },
        selection: { anchor: 0 },
        scrollIntoView: true,
        annotations: setOptions.history === "skip" ? [Transaction.addToHistory.of(false)] : undefined,
      });
    },

    insertText(text: string, deleteBefore = 0): { from: number; to: number } {
      const { from, to } = view.state.selection.main;
      const insertFrom = from - deleteBefore;
      view.dispatch({
        changes: { from: insertFrom, to, insert: text },
        selection: { anchor: insertFrom + text.length },
        scrollIntoView: true,
      });
      return { from: insertFrom, to: insertFrom + text.length };
    },

    setSelection(from: number, to?: number): void {
      view.dispatch({
        selection: { anchor: from, head: to ?? from },
        scrollIntoView: true,
      });
    },

    setMarkdownSelection(from: number, to?: number): void {
      editor.setSelection(from, to);
    },

    getMarkdownSelection(): { from: number; to: number } {
      const { from, to } = view.state.selection.main;
      return { from, to };
    },

    getMarkdownSelectionRange(): { anchor: number; head: number } {
      const { anchor, head } = view.state.selection.main;
      return { anchor, head };
    },

    getSelection(): { from: number; to: number } {
      return editor.getMarkdownSelection();
    },

    replaceMarkdownRange(
      from: number,
      to: number,
      text: string,
      select?: "start" | "end" | "all",
    ): { from: number; to: number } {
      return dispatchWithSelect(from, to, text, select);
    },

    textBetween(from: number, to: number): string {
      return view.state.doc.sliceString(from, to);
    },

    replaceRange(
      from: number,
      to: number,
      text: string,
      select?: "start" | "end" | "all",
    ): { from: number; to: number } {
      return dispatchWithSelect(from, to, text, select);
    },

    undo(): boolean {
      return cmUndo(view);
    },

    redo(): boolean {
      return cmRedo(view);
    },

    runCommand(command: EditorCommand, value = ""): boolean {
      return runCommandCM6(view, command, value);
    },

    getBlockContext(): EditorBlockContext {
      return getBlockContextCM6(view);
    },

    registerQuickInsertProvider(provider: QuickInsertProvider): () => void {
      return qiRegistry.register(provider);
    },

    getQuickInsertItems(query = ""): QuickInsertItem[] {
      return qiRegistry.getItems(view, query);
    },

    runQuickInsert(item: QuickInsertItem): boolean {
      return qiRegistry.run(view, item);
    },

    setWritingMode(modeOptions: WritingModeOptions): void {
      wrap.classList.toggle("typora-web-focus-mode", !!modeOptions.focusMode);
      wrap.classList.toggle("typora-web-typewriter-mode", !!modeOptions.typewriterMode);
    },

    cursorContext(maxChars = 512): {
      before: string;
      after: string;
      rect: { left: number; top: number; bottom: number } | null;
      rectAtOffset: (offset: number) => { left: number; top: number; bottom: number } | null;
    } {
      const { from } = view.state.selection.main;
      const docLen = view.state.doc.length;
      const beforeStart = Math.max(0, from - maxChars);
      const afterEnd = Math.min(docLen, from + maxChars);
      const before = view.state.doc.sliceString(beforeStart, from);
      const after = view.state.doc.sliceString(from, afterEnd);

      function rectAt(offset: number) {
        try {
          const coords = view.coordsAtPos(offset);
          if (!coords) return null;
          return { left: coords.left, top: coords.top, bottom: coords.bottom };
        } catch {
          return null;
        }
      }

      return { before, after, rect: rectAt(from), rectAtOffset: (offset: number) => rectAt(beforeStart + offset) };
    },

    cursorRect(): { left: number; top: number; bottom: number } | null {
      const { from } = view.state.selection.main;
      try {
        const coords = view.coordsAtPos(from);
        return coords ? { left: coords.left, top: coords.top, bottom: coords.bottom } : null;
      } catch {
        return null;
      }
    },

    toggleSource(): void {
      const { head } = view.state.selection.main;
      const beforeTop = coordsTopAt(head);
      const enteringPreview = inSource;
      inSource = !inSource;
      view.dispatch({
        effects: [
          previewCompartment.reconfigure(inSource ? [] : previewExtensions()),
        ],
      });
      preserveCursorScreenTop(head, beforeTop, enteringPreview);
      view.focus();
    },

    isSourceMode(): boolean {
      return inSource;
    },

    focus(): void {
      view.focus();
    },

    revealCursor(): void {
      const { from } = view.state.selection.main;
      view.dispatch({ selection: { anchor: from }, scrollIntoView: true });
      flashCaret();
    },

    destroy(): void {
      view.contentDOM.removeEventListener("mousedown", onSourceWidgetMouseDown, { capture: true });
      view.destroy();
      caretFlash.remove();
      wrap.remove();
      disposeHighlightWorker();
      void import("../diagram-render.ts").then(({ disposeDiagramRuntime }) => disposeDiagramRuntime());
      disposeMathRuntime();
    },

    // Expose the CM6 EditorView as an escape hatch.
    get view() {
      return view;
    },
  };

  return editor;

  function flashCaret(): void {
    window.requestAnimationFrame(() => {
      const { from } = view.state.selection.main;
      const coords = view.coordsAtPos(from);
      if (!coords) return;
      caretFlash.hidden = false;
      caretFlash.style.left = `${coords.left}px`;
      caretFlash.style.top = `${coords.top}px`;
      caretFlash.style.height = `${Math.max(16, coords.bottom - coords.top)}px`;
      caretFlash.classList.remove("is-active");
      void caretFlash.offsetWidth;
      caretFlash.classList.add("is-active");
      window.setTimeout(() => {
        caretFlash.classList.remove("is-active");
        caretFlash.hidden = true;
      }, 950);
    });
  }

  function coordsTopAt(pos: number): number | null {
    try {
      return view.coordsAtPos(pos)?.top ?? null;
    } catch {
      return null;
    }
  }

  function preserveCursorScreenTop(pos: number, beforeTop: number | null, repeatAfterWidgetLoad: boolean): void {
    const adjust = (): void => {
      if (!view.dom.isConnected) return;
      const afterTop = coordsTopAt(pos);
      if (beforeTop != null && afterTop != null) {
        const delta = afterTop - beforeTop;
        if (Math.abs(delta) >= 1) scrollEditorSurface(delta);
      }
      view.dispatch({ effects: EditorView.scrollIntoView(pos, { y: "nearest" }) });
    };
    window.requestAnimationFrame(() => window.requestAnimationFrame(adjust));
    if (repeatAfterWidgetLoad) {
      window.setTimeout(adjust, 240);
      window.setTimeout(adjust, 800);
    }
  }

  function scrollEditorSurface(delta: number): void {
    const before = host.scrollTop;
    host.scrollTop += delta;
    if (Math.abs(host.scrollTop - before) < 1) window.scrollBy(0, delta);
  }
}

// ---------------------------------------------------------------------------
// Extension setup
// ---------------------------------------------------------------------------

function previewExtensions(): Extension[] {
  return [
    blockMathRangesExtension,
    livePreviewExtension,
    blockExtrasExtension,
    mathExtension,
    fencedCodeExtension,
    taskListExtension,
    imageExtension,
    inlineCommandsExtension,
  ];
}

function exitCurrentOrgEnv(view: EditorView): boolean {
  const target = orgEnvExitTarget(view.state);
  if (target == null) return false;
  view.dispatch({ selection: { anchor: target }, scrollIntoView: true });
  return true;
}

const SELECTION_WRAP_INPUT_PAIRS = new Map<string, string>([
  ["(", ")"],
  ["[", "]"],
  ["{", "}"],
  ["<", ">"],
  ['"', '"'],
  ["'", "'"],
  ["`", "`"],
  ["*", "*"],
  ["_", "_"],
  ["“", "”"],
  ["‘", "’"],
  ["「", "」"],
  ["『", "』"],
  ["《", "》"],
]);

export function wrapSelectedMarkdownInput(view: EditorView, _from: number, _to: number, text: string): boolean {
  const close = SELECTION_WRAP_INPUT_PAIRS.get(text);
  if (close == null) return false;

  const ranges = view.state.selection.ranges;
  if (ranges.length === 0 || ranges.some((range) => range.empty)) return false;

  const changes = ranges.map((range) => ({
    from: range.from,
    to: range.to,
    insert: text + view.state.doc.sliceString(range.from, range.to) + close,
  }));

  let offset = 0;
  const nextRanges = ranges.map((range) => {
    const from = range.from + offset + text.length;
    const to = range.to + offset + text.length;
    offset += text.length + close.length;
    return EditorSelection.range(from, to);
  });

  view.dispatch({
    changes,
    selection: EditorSelection.create(nextRanges),
    scrollIntoView: true,
  });
  return true;
}

function buildExtensions(options: EditorOptions, previewCompartment: Compartment, isSourceMode: () => boolean) {
  return [
    EditorState.allowMultipleSelections.of(true),
    EditorView.clickAddsSelectionRange.of((event) => event.altKey || event.metaKey || event.ctrlKey),
    history({ minDepth: 200, newGroupDelay: 500 }),
    closeBrackets(),
    EditorView.inputHandler.of(wrapSelectedMarkdownInput),
    rectangularSelection(),
    keymap.of([
      { key: "Enter", run: exitEmptyMarkdownBlock },
      { key: "Mod-Enter", run: exitCurrentOrgEnv },
      { key: "Tab", run: (view) => indentMarkdownList(view, 1) || indentWithTab.run?.(view) === true },
      { key: "Shift-Tab", run: (view) => indentMarkdownList(view, -1) },
      { key: "Mod-d", run: selectNextMarkdownOccurrence },
      { key: "Mod-Shift-z", run: cmRedo },
      { key: "Meta-Shift-z", run: cmRedo },
      indentWithTab,
      ...defaultKeymap,
      ...historyKeymap,
    ]),
    markdown({ base: markdownLanguage }),
    highlightActiveLine(),
    tocIndexExtension,
    previewCompartment.of(isSourceMode() ? [] : previewExtensions()),
    findHighlightExtension,
    roamLinkStatusExtension,
    proseDiagnosticsExtension,
    EditorView.lineWrapping,
    EditorView.updateListener.of((update) => {
      if (update.docChanged && options.onChange) {
        if (options.onChange.length === 0) {
          (options.onChange as () => void)();
        } else {
          const md = update.state.doc.toString();
          options.onChange(md);
        }
      }
    }),
    EditorView.domEventHandlers({
      mousedown: (event, eventView) => event.button === 0 && (
        openIncludeFromEvent(eventView, event)
        || openMarkdownLinkFromEvent(eventView, event)
        || calibrateWrappedLayoutClick(eventView, event)
      ),
      auxclick: (event, eventView) => event.button === 1 && (
        openIncludeFromEvent(eventView, event)
        || openMarkdownLinkFromEvent(eventView, event)
      ),
      contextmenu: (event, eventView) => openAttachmentContextMenuFromEvent(eventView, event),
      focus: () => { options.onFocus?.(); return false; },
      blur: () => { options.onBlur?.(); return false; },
      paste: (event, pasteView) => {
        const data = event.clipboardData;
        if (!data || data.files.length > 0) return false;
        const text = markdownFromClipboard(data);
        if (!text) return false;
        event.preventDefault();
        const { from, to } = pasteView.state.selection.main;
        pasteView.dispatch({
          changes: { from, to, insert: text },
          selection: { anchor: from + text.length },
          scrollIntoView: true,
        });
        pasteView.focus();
        return true;
      },
    }),
  ];
}

function wordRangeAt(state: EditorState, pos: number): { from: number; to: number } | null {
  const doc = state.doc;
  const line = doc.lineAt(pos);
  const offset = pos - line.from;
  const isWord = (ch: string): boolean => /[\p{L}\p{N}_-]/u.test(ch);
  let from = offset;
  let to = offset;
  while (from > 0 && isWord(line.text[from - 1] ?? "")) from--;
  while (to < line.text.length && isWord(line.text[to] ?? "")) to++;
  if (from === to) return null;
  return { from: line.from + from, to: line.from + to };
}

function selectNextMarkdownOccurrence(view: EditorView): boolean {
  const state = view.state;
  const main = state.selection.main;
  let query = main.empty ? "" : state.doc.sliceString(main.from, main.to);
  let firstFrom = main.from;
  let firstTo = main.to;
  if (!query) {
    const word = wordRangeAt(state, main.from);
    if (!word) return false;
    query = state.doc.sliceString(word.from, word.to);
    firstFrom = word.from;
    firstTo = word.to;
  }
  if (!query) return false;
  const start = main.to;
  const after = state.doc.sliceString(start);
  let index = after.indexOf(query);
  let from = index >= 0 ? start + index : -1;
  if (from < 0) {
    const before = state.doc.sliceString(0, Math.max(0, firstFrom));
    index = before.indexOf(query);
    if (index < 0) return false;
    from = index;
  }
  const range = EditorSelection.range(from, from + query.length);
  const ranges = [
    ...state.selection.ranges,
    ...(main.empty && !state.selection.ranges.some((r) => r.from === firstFrom && r.to === firstTo)
      ? [EditorSelection.range(firstFrom, firstTo)]
      : []),
    range,
  ].sort((a, b) => a.from - b.from || a.to - b.to);
  view.dispatch({
    selection: EditorSelection.create(ranges, ranges.length - 1),
    scrollIntoView: true,
  });
  return true;
}
