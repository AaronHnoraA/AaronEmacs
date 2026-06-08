import type { Editor } from "../src/lib.ts";
import type { Text } from "@codemirror/state";

export type VimLiteMode = "insert" | "normal" | "visual" | "visual-line";

export type VimLiteKey = {
  key: string;
  ctrlKey?: boolean;
  metaKey?: boolean;
  altKey?: boolean;
  shiftKey?: boolean;
  isComposing?: boolean;
};

export type VimLiteController = {
  mode(): VimLiteMode;
  setMode(mode: VimLiteMode): void;
  handleKey(event: VimLiteKey): boolean;
  handleKeyDown(event: KeyboardEvent): boolean;
};

type VimLiteOptions = {
  onModeChange?: (mode: VimLiteMode) => void;
  onUndo?: () => boolean;
  onRedo?: () => boolean;
  onIndent?: (direction: 1 | -1) => boolean;
};

type LineInfo = {
  start: number;
  end: number;
  column: number;
};

function hasCommandModifier(event: VimLiteKey): boolean {
  return Boolean(event.metaKey || event.altKey || event.ctrlKey);
}

function isEscape(event: VimLiteKey): boolean {
  return event.key === "Escape" || Boolean(event.ctrlKey && event.key === "[");
}

function clamp(value: number, min: number, max: number): number {
  return Math.max(min, Math.min(max, value));
}

function targetInEditor(host: HTMLElement, target: EventTarget | null): boolean {
  return target instanceof Node && host.contains(target);
}

function editableEventTarget(host: HTMLElement, target: EventTarget | null): HTMLElement | null {
  if (!(target instanceof Node) || !host.contains(target)) return null;
  const el = target instanceof Element ? target : target.parentElement;
  const editable = el?.closest<HTMLElement>("input, textarea, select, [contenteditable='true']");
  if (!editable) return null;
  if (editable.classList.contains("cm-content")) return null;
  return editable;
}

function selectionInEditable(editable: HTMLElement): Selection | null {
  const selection = editable.ownerDocument.getSelection?.() ?? window.getSelection();
  if (!selection || selection.rangeCount === 0) return null;
  const anchor = selection.anchorNode;
  const focus = selection.focusNode;
  if (!anchor || !focus || !editable.contains(anchor) || !editable.contains(focus)) return null;
  return selection;
}

function isRichEditable(editable: HTMLElement): boolean {
  return editable.isContentEditable
    || editable.contentEditable === "true"
    || editable.getAttribute("contenteditable") === "true";
}

function moveEditableSelection(
  editable: HTMLElement,
  direction: "forward" | "backward",
  granularity: "character" | "word" | "line" | "lineboundary",
): boolean {
  const selection = selectionInEditable(editable);
  const modify = (selection as (Selection & {
    modify?: (alter: "move", direction: "forward" | "backward", granularity: string) => void;
  }) | null)?.modify;
  if (typeof modify !== "function" || !selection) return false;
  modify.call(selection, "move", direction, granularity);
  return true;
}

function doc(editor: Editor): Text {
  return editor.view.state.doc;
}

function docLineInfo(text: Text, pos: number): LineInfo {
  const line = text.lineAt(clamp(pos, 0, text.length));
  return { start: line.from, end: line.to, column: clamp(pos, line.from, line.to) - line.from };
}

function docLineRange(text: Text, pos: number): { from: number; to: number; cursor: number } {
  const line = text.lineAt(clamp(pos, 0, text.length));
  const to = line.to < text.length ? line.to + 1 : line.to;
  return { from: line.from, to, cursor: line.from };
}

function docLineSelectionRange(text: Text, anchor: number, head: number): { from: number; to: number } {
  const a = docLineRange(text, anchor);
  const h = docLineRange(text, head);
  return {
    from: Math.min(a.from, h.from),
    to: Math.max(a.to, h.to),
  };
}

function docChar(text: Text, pos: number): string {
  if (pos < 0 || pos >= text.length) return "";
  return text.sliceString(pos, pos + 1);
}

function wordChar(ch: string): boolean {
  return /[A-Za-z0-9_]/.test(ch);
}

function currentHead(editor: Editor): number {
  // The moving end of the selection (CM6 head), not the larger offset — visual
  // mode relies on this to extend a selection backward past its anchor.
  return editor.getMarkdownSelectionRange().head;
}

function setPos(editor: Editor, pos: number): void {
  editor.setMarkdownSelection(clamp(pos, 0, doc(editor).length));
}

function setSelection(editor: Editor, anchor: number, head: number): void {
  // Preserve direction: anchor stays fixed, head is the moving end. The
  // highlighted span is [min,max] either way, but keeping head distinct lets
  // subsequent motions pivot on the correct end.
  const length = doc(editor).length;
  editor.setMarkdownSelection(clamp(anchor, 0, length), clamp(head, 0, length));
}

function moveChar(editor: Editor, dir: -1 | 1): void {
  const text = doc(editor);
  const selection = editor.getMarkdownSelection();
  const pos = selection.from === selection.to
    ? selection.from + dir
    : (dir < 0 ? selection.from : selection.to);
  setPos(editor, clamp(pos, 0, text.length));
}

function moveLine(editor: Editor, dir: -1 | 1, goalColumn: number | null): number | null {
  const text = doc(editor);
  const pos = editor.getMarkdownSelection().from;
  const line = docLineInfo(text, pos);
  const desired = goalColumn ?? line.column;
  if (dir < 0) {
    if (line.start === 0) return desired;
    const prev = docLineInfo(text, line.start - 1);
    setPos(editor, Math.min(prev.start + desired, prev.end));
    return desired;
  }
  if (line.end >= text.length) return desired;
  const next = docLineInfo(text, line.end + 1);
  setPos(editor, Math.min(next.start + desired, next.end));
  return desired;
}

function lineBoundary(editor: Editor, which: "start" | "end"): void {
  const pos = editor.getMarkdownSelection().from;
  const line = docLineInfo(doc(editor), pos);
  setPos(editor, which === "start" ? line.start : line.end);
}

function docBoundary(editor: Editor, which: "start" | "end"): void {
  setPos(editor, which === "start" ? 0 : doc(editor).length);
}

function moveWord(editor: Editor, dir: -1 | 1): void {
  const text = doc(editor);
  let pos = editor.getMarkdownSelection().from;
  if (dir > 0) {
    while (pos < text.length && wordChar(docChar(text, pos))) pos++;
    while (pos < text.length && !wordChar(docChar(text, pos))) pos++;
  } else {
    pos = Math.max(0, pos - 1);
    while (pos > 0 && !wordChar(docChar(text, pos))) pos--;
    while (pos > 0 && wordChar(docChar(text, pos - 1))) pos--;
  }
  setPos(editor, pos);
}

function searchChar(editor: Editor, ch: string, dir: -1 | 1): void {
  const text = doc(editor).toString();
  const pos = editor.getMarkdownSelection().from;
  let next = dir > 0 ? text.indexOf(ch, pos + 1) : text.lastIndexOf(ch, pos - 1);
  if (next < 0) next = dir > 0 ? text.indexOf(ch, 0) : text.lastIndexOf(ch);
  if (next >= 0) setPos(editor, next);
}

function deleteChar(editor: Editor): string {
  const text = doc(editor);
  const { from, to } = editor.getMarkdownSelection();
  const end = from === to ? Math.min(from + 1, text.length) : to;
  if (from >= end) return "";
  const deleted = text.sliceString(from, end);
  editor.replaceMarkdownRange(from, end, "", "start");
  return deleted;
}

function deleteLine(editor: Editor): string {
  const text = doc(editor);
  const { from, to } = editor.getMarkdownSelection();
  const range = to > from ? { from, to } : docLineRange(text, from);
  if (range.from >= range.to) return "";
  const deleted = text.sliceString(range.from, range.to);
  const fallbackPos = range.from > 0 && range.to >= text.length ? range.from - 1 : range.from;
  editor.replaceMarkdownRange(range.from, range.to, "", "start");
  setPos(editor, fallbackPos);
  return deleted;
}

function currentSelectionText(editor: Editor): string {
  const { from, to } = editor.getMarkdownSelection();
  return from < to ? doc(editor).sliceString(from, to) : "";
}

function replaceChar(editor: Editor, ch: string): void {
  const text = doc(editor);
  const { from, to } = editor.getMarkdownSelection();
  const end = from === to ? Math.min(from + 1, text.length) : to;
  if (from >= end) return;
  editor.replaceMarkdownRange(from, end, ch.repeat(Math.max(1, end - from)), "end");
}

function insertText(editor: Editor, text: string, where: "before" | "after"): void {
  const length = doc(editor).length;
  const selection = editor.getMarkdownSelection();
  const insertAt = where === "after" ? Math.min(length, selection.to + 1) : selection.from;
  editor.replaceMarkdownRange(insertAt, insertAt, text, "end");
}

function openLine(editor: Editor, where: "above" | "below"): void {
  const text = doc(editor);
  const pos = editor.getMarkdownSelection().from;
  const line = docLineInfo(text, pos);
  const insertAt = where === "above" ? line.start : line.end;
  editor.replaceMarkdownRange(insertAt, insertAt, "\n", "end");
  setPos(editor, where === "above" ? insertAt : insertAt + 1);
}

export function createVimLite(
  editor: Editor,
  host: HTMLElement,
  options: VimLiteOptions = {},
): VimLiteController {
  let mode: VimLiteMode = "insert";
  let goalColumn: number | null = null;
  let pending = "";
  let visualAnchor: number | null = null;
  let visualHead: number | null = null;
  let register = "";

  function yank(text: string): void {
    if (!text) return;
    register = text;
    (window as unknown as Record<string, unknown>).__aaronoteVimRegister = text;
    void fetch("/api/clipboard", {
      method: "POST",
      headers: { "Content-Type": "text/plain; charset=utf-8" },
      body: text,
    }).catch(() => {});
  }

  function resetMotionMemory(): void {
    goalColumn = null;
  }

  function setMode(next: VimLiteMode): void {
    if (mode === next) return;
    mode = next;
    pending = "";
    visualAnchor = null;
    visualHead = null;
    resetMotionMemory();
    options.onModeChange?.(mode);
  }

  // The tracked moving end of the visual selection. Prefer the local
  // visualHead (authoritative once visual mode is driving the selection) and
  // fall back to the editor's live head when first entering visual mode.
  function headPos(): number {
    return visualHead ?? currentHead(editor);
  }

  function setVisualHead(head: number): void {
    if (visualAnchor == null) visualAnchor = currentHead(editor);
    visualHead = head;
    setSelection(editor, visualAnchor, visualHead);
  }

  function enterVisual(): void {
    setMode("visual");
    visualAnchor = currentHead(editor);
    visualHead = visualAnchor;
  }

  function enterVisualLine(): void {
    const head = currentHead(editor);
    setMode("visual-line");
    visualAnchor = head;
    visualHead = head;
    const range = docLineSelectionRange(doc(editor), visualAnchor, visualHead);
    setSelection(editor, range.from, range.to);
  }

  function visualMoveChar(dir: -1 | 1): void {
    resetMotionMemory();
    setVisualHead(clamp(headPos() + dir, 0, doc(editor).length));
  }

  function visualMoveLine(dir: -1 | 1): void {
    const text = doc(editor);
    const pos = headPos();
    const line = docLineInfo(text, pos);
    const desired = goalColumn ?? line.column;
    goalColumn = desired;
    if (dir < 0 && line.start > 0) {
      const prev = docLineInfo(text, line.start - 1);
      setVisualHead(Math.min(prev.start + desired, prev.end));
    } else if (dir > 0 && line.end < text.length) {
      const next = docLineInfo(text, line.end + 1);
      setVisualHead(Math.min(next.start + desired, next.end));
    }
  }

  function visualLineMove(dir: -1 | 1): void {
    const text = doc(editor);
    const current = docLineRange(text, headPos());
    let nextPos = dir > 0 ? current.to : Math.max(0, current.from - 1);
    if (dir > 0 && current.to >= text.length) nextPos = current.cursor;
    const next = docLineRange(text, nextPos);
    visualHead = next.cursor;
    const range = docLineSelectionRange(text, visualAnchor ?? next.cursor, visualHead);
    setSelection(editor, range.from, range.to);
  }

  function visualLineBoundary(which: "start" | "end"): void {
    resetMotionMemory();
    const line = docLineInfo(doc(editor), headPos());
    setVisualHead(which === "start" ? line.start : line.end);
  }

  function visualMoveWord(dir: -1 | 1): void {
    resetMotionMemory();
    const text = doc(editor);
    let pos = headPos();
    if (dir > 0) {
      while (pos < text.length && wordChar(docChar(text, pos))) pos++;
      while (pos < text.length && !wordChar(docChar(text, pos))) pos++;
    } else {
      pos = Math.max(0, pos - 1);
      while (pos > 0 && !wordChar(docChar(text, pos))) pos--;
      while (pos > 0 && wordChar(docChar(text, pos - 1))) pos--;
    }
    setVisualHead(pos);
  }

  function deleteLineCommand(): void {
    resetMotionMemory();
    yank(deleteLine(editor));
    setMode("normal");
  }

  function yankSelection(): void {
    resetMotionMemory();
    yank(currentSelectionText(editor));
    setMode("normal");
  }

  function yankLine(): void {
    resetMotionMemory();
    const text = doc(editor);
    const range = docLineRange(text, editor.getMarkdownSelection().from);
    if (range.from < range.to) yank(text.sliceString(range.from, range.to));
    setMode("normal");
  }

  function paste(where: "before" | "after"): void {
    resetMotionMemory();
    const doInsert = (text: string) => {
      if (text) {
        register = text;
        insertText(editor, text, where);
      }
      setMode("normal");
    };
    if (register) {
      doInsert(register);
      return;
    }
    void fetch("/api/clipboard")
      .then(r => r.ok ? r.text() : Promise.reject())
      .then(doInsert)
      .catch(() => setMode("normal"));
  }

  function appendChar(): void {
    const text = doc(editor);
    const selection = editor.getMarkdownSelection();
    const pos = selection.from === selection.to ? selection.from : selection.to;
    const line = docLineInfo(text, pos);
    setPos(editor, Math.min(line.end, pos + 1));
    setMode("insert");
  }

  function editableNormalCommand(key: string, editable: HTMLElement): boolean {
    if (!isRichEditable(editable)) {
      if (key === "i" || key === "a") {
        setMode("insert");
        return true;
      }
      pending = "";
      return key.length === 1;
    }

    const move = (
      direction: "forward" | "backward",
      granularity: "character" | "word" | "line" | "lineboundary",
    ): boolean => {
      resetMotionMemory();
      return moveEditableSelection(editable, direction, granularity);
    };

    switch (key) {
      case "h":
      case "ArrowLeft":
      case "Backspace":
        move("backward", "character");
        return true;
      case "l":
      case "ArrowRight":
      case " ":
        move("forward", "character");
        return true;
      case "j":
        move("forward", "line");
        return true;
      case "k":
        move("backward", "line");
        return true;
      case "ArrowDown":
      case "ArrowUp":
        return false;
      case "0":
        move("backward", "lineboundary");
        return true;
      case "$":
        move("forward", "lineboundary");
        return true;
      case "w":
        move("forward", "word");
        return true;
      case "b":
        move("backward", "word");
        return true;
      case "i":
        setMode("insert");
        return true;
      case "a":
        move("forward", "character");
        setMode("insert");
        return true;
      case "Escape":
        setMode("normal");
        return true;
      default:
        pending = "";
        return key.length === 1;
    }
  }

  function normalCommand(key: string): boolean {
    if (pending === "d") {
      pending = "";
      if (key === "d") {
        deleteLineCommand();
        return true;
      }
      return true;
    }
    if (pending === "y") {
      pending = "";
      if (key === "y") {
        yankLine();
        return true;
      }
      return true;
    }
    if (pending === "r") {
      pending = "";
      if (key.length === 1) {
        resetMotionMemory();
        replaceChar(editor, key);
        setMode("normal");
      }
      return true;
    }
    if (pending === "g") {
      pending = "";
      if (key === "g") {
        resetMotionMemory();
        docBoundary(editor, "start");
      }
      return true;
    }
    if (pending === ">") {
      pending = "";
      if (key === ">") {
        resetMotionMemory();
        options.onIndent?.(1);
      }
      return true;
    }
    if (pending === "<") {
      pending = "";
      if (key === "<") {
        resetMotionMemory();
        options.onIndent?.(-1);
      }
      return true;
    }
    if (pending === "s" || pending === "S") {
      const dir = pending === "s" ? 1 : -1;
      pending = "";
      if (key.length === 1) {
        resetMotionMemory();
        searchChar(editor, key, dir);
      }
      return true;
    }

    switch (key) {
      case "h":
      case "ArrowLeft":
      case "Backspace":
        resetMotionMemory();
        moveChar(editor, -1);
        return true;
      case "l":
      case "ArrowRight":
      case " ":
        resetMotionMemory();
        moveChar(editor, 1);
        return true;
      case "j":
      case "ArrowDown":
        goalColumn = moveLine(editor, 1, goalColumn);
        return true;
      case "k":
      case "ArrowUp":
        goalColumn = moveLine(editor, -1, goalColumn);
        return true;
      case "0":
        resetMotionMemory();
        lineBoundary(editor, "start");
        return true;
      case "$":
        resetMotionMemory();
        lineBoundary(editor, "end");
        return true;
      case "w":
        resetMotionMemory();
        moveWord(editor, 1);
        return true;
      case "b":
        resetMotionMemory();
        moveWord(editor, -1);
        return true;
      case "u":
        return options.onUndo?.() ?? false;
      case "g":
        pending = "g";
        return true;
      case "G":
        resetMotionMemory();
        docBoundary(editor, "end");
        return true;
      case "i":
        setMode("insert");
        return true;
      case "v":
        enterVisual();
        return true;
      case "V":
        enterVisualLine();
        return true;
      case "a":
        appendChar();
        return true;
      case "I":
        resetMotionMemory();
        lineBoundary(editor, "start");
        setMode("insert");
        return true;
      case "A":
        resetMotionMemory();
        lineBoundary(editor, "end");
        setMode("insert");
        return true;
      case "o":
        resetMotionMemory();
        openLine(editor, "below");
        setMode("insert");
        return true;
      case "O":
        resetMotionMemory();
        openLine(editor, "above");
        setMode("insert");
        return true;
      case "x":
      case "Delete":
        resetMotionMemory();
        yank(deleteChar(editor));
        return true;
      case "p":
        paste("after");
        return true;
      case "P":
        paste("before");
        return true;
      case "s":
      case "S":
        pending = key;
        return true;
      case "r":
        pending = "r";
        return true;
      case "d":
        pending = "d";
        return true;
      case "y":
        pending = "y";
        return true;
      case ">":
        pending = ">";
        return true;
      case "<":
        pending = "<";
        return true;
      case "Escape":
        setMode("normal");
        return true;
      default:
        pending = "";
        return key.length === 1;
    }
  }

  function visualCommand(key: string): boolean {
    if (pending === "d") {
      pending = "";
      if (key === "d") {
        deleteLineCommand();
        return true;
      }
      return true;
    }
    if (pending === "r") {
      pending = "";
      if (key.length === 1) {
        resetMotionMemory();
        replaceChar(editor, key);
      }
      setMode("normal");
      return true;
    }
    switch (key) {
      case "h":
      case "ArrowLeft":
      case "Backspace":
        visualMoveChar(-1);
        return true;
      case "l":
      case "ArrowRight":
      case " ":
        visualMoveChar(1);
        return true;
      case "j":
      case "ArrowDown":
        visualMoveLine(1);
        return true;
      case "k":
      case "ArrowUp":
        visualMoveLine(-1);
        return true;
      case "0":
        visualLineBoundary("start");
        return true;
      case "$":
        visualLineBoundary("end");
        return true;
      case "w":
        visualMoveWord(1);
        return true;
      case "b":
        visualMoveWord(-1);
        return true;
      case "x":
      case "Delete":
      case "d":
        resetMotionMemory();
        yank(deleteChar(editor));
        setMode("normal");
        return true;
      case "y":
        yankSelection();
        return true;
      case "r":
        pending = "r";
        return true;
      case "v":
      case "Escape":
        setMode("normal");
        return true;
      default:
        pending = "";
        return key.length === 1;
    }
  }

  function visualLineCommand(key: string): boolean {
    switch (key) {
      case "j":
      case "ArrowDown":
        visualLineMove(1);
        return true;
      case "k":
      case "ArrowUp":
        visualLineMove(-1);
        return true;
      case "x":
      case "d":
      case "Delete":
        deleteLineCommand();
        return true;
      case "y":
        yankSelection();
        return true;
      case "V":
      case "v":
      case "Escape":
        setMode("normal");
        return true;
      default:
        pending = "";
        return key.length === 1;
    }
  }

  return {
    mode: () => mode,
    setMode,
    handleKey(event: VimLiteKey): boolean {
      if (event.isComposing) return false;
      if (isEscape(event)) {
        setMode("normal");
        return true;
      }

      if (mode === "insert") {
        if (!hasCommandModifier(event) && !event.shiftKey && (event.key === "ArrowDown" || event.key === "ArrowUp")) {
          goalColumn = moveLine(editor, event.key === "ArrowDown" ? 1 : -1, goalColumn);
          return true;
        }
        return false;
      }
      if (event.ctrlKey && !event.metaKey && !event.altKey && !event.shiftKey && event.key.toLowerCase() === "r") {
        return options.onRedo?.() ?? false;
      }
      if (hasCommandModifier(event)) return false;

      const handled = mode === "visual-line"
        ? visualLineCommand(event.key)
        : mode === "visual"
          ? visualCommand(event.key)
          : normalCommand(event.key);
      return handled;
    },
    handleKeyDown(event: KeyboardEvent): boolean {
      if (!targetInEditor(host, event.target)) return false;
      if (event.isComposing) return false;
      if (isEscape(event)) {
        event.preventDefault();
        setMode("normal");
        return true;
      }

      const editable = editableEventTarget(host, event.target);
      if (editable) {
        if (mode === "insert") return false;
        if (mode === "normal") {
          const handled = editableNormalCommand(event.key, editable);
          if (handled) event.preventDefault();
          return handled;
        }
        return false;
      }

      const handled = this.handleKey(event);
      if (handled) event.preventDefault();
      return handled;
    },
  };
}
