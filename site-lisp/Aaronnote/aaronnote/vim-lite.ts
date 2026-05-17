import { TextSelection } from "prosemirror-state";

import type { Editor } from "../src/lib.ts";

export type VimLiteMode = "insert" | "normal";

export type VimLiteController = {
  mode(): VimLiteMode;
  setMode(mode: VimLiteMode): void;
  handleKeyDown(event: KeyboardEvent): boolean;
};

type VimLiteOptions = {
  onModeChange?: (mode: VimLiteMode) => void;
};

type TextareaLineInfo = {
  start: number;
  end: number;
  column: number;
};

function hasCommandModifier(event: KeyboardEvent): boolean {
  return event.metaKey || event.altKey || event.ctrlKey;
}

function isEscape(event: KeyboardEvent): boolean {
  return event.key === "Escape" || (event.ctrlKey && event.key === "[");
}

function clamp(value: number, min: number, max: number): number {
  return Math.max(min, Math.min(max, value));
}

function sourceTextarea(host: HTMLElement): HTMLTextAreaElement | null {
  return host.querySelector<HTMLTextAreaElement>(".typora-web-source:not([hidden])");
}

function targetInEditor(host: HTMLElement, target: EventTarget | null): boolean {
  return target instanceof Node && host.contains(target);
}

function sourceLineInfo(value: string, pos: number): TextareaLineInfo {
  const start = value.lastIndexOf("\n", Math.max(0, pos - 1)) + 1;
  const next = value.indexOf("\n", pos);
  const end = next < 0 ? value.length : next;
  return { start, end, column: pos - start };
}

function setSourcePos(textarea: HTMLTextAreaElement, pos: number): void {
  const clamped = clamp(pos, 0, textarea.value.length);
  textarea.setSelectionRange(clamped, clamped);
  textarea.focus();
}

function sourceMoveChar(textarea: HTMLTextAreaElement, dir: -1 | 1): void {
  setSourcePos(textarea, (textarea.selectionStart ?? 0) + dir);
}

function sourceMoveLine(textarea: HTMLTextAreaElement, dir: -1 | 1, goalColumn: number | null): number | null {
  const value = textarea.value;
  const pos = textarea.selectionStart ?? 0;
  const line = sourceLineInfo(value, pos);
  const desired = goalColumn ?? line.column;
  if (dir < 0) {
    if (line.start === 0) return desired;
    const prevEnd = line.start - 1;
    const prev = sourceLineInfo(value, prevEnd);
    setSourcePos(textarea, Math.min(prev.start + desired, prev.end));
    return desired;
  }
  if (line.end >= value.length) return desired;
  const nextStart = line.end + 1;
  const next = sourceLineInfo(value, nextStart);
  setSourcePos(textarea, Math.min(next.start + desired, next.end));
  return desired;
}

function sourceLineBoundary(textarea: HTMLTextAreaElement, which: "start" | "end"): void {
  const pos = textarea.selectionStart ?? 0;
  const line = sourceLineInfo(textarea.value, pos);
  setSourcePos(textarea, which === "start" ? line.start : line.end);
}

function wordChar(ch: string): boolean {
  return /[A-Za-z0-9_]/.test(ch);
}

function sourceMoveWord(textarea: HTMLTextAreaElement, dir: -1 | 1): void {
  const value = textarea.value;
  let pos = textarea.selectionStart ?? 0;
  if (dir > 0) {
    while (pos < value.length && wordChar(value[pos] ?? "")) pos++;
    while (pos < value.length && !wordChar(value[pos] ?? "")) pos++;
  } else {
    pos = Math.max(0, pos - 1);
    while (pos > 0 && !wordChar(value[pos] ?? "")) pos--;
    while (pos > 0 && wordChar(value[pos - 1] ?? "")) pos--;
  }
  setSourcePos(textarea, pos);
}

function sourceDeleteChar(textarea: HTMLTextAreaElement): void {
  const pos = textarea.selectionStart ?? 0;
  if (pos >= textarea.value.length) return;
  textarea.setRangeText("", pos, pos + 1, "start");
  textarea.dispatchEvent(new Event("input", { bubbles: true }));
}

function sourceOpenLine(textarea: HTMLTextAreaElement, where: "above" | "below"): void {
  const pos = textarea.selectionStart ?? 0;
  const line = sourceLineInfo(textarea.value, pos);
  const insertAt = where === "above" ? line.start : line.end;
  const text = where === "above" ? "\n" : "\n";
  textarea.setSelectionRange(insertAt, insertAt);
  textarea.setRangeText(text, insertAt, insertAt, "end");
  if (where === "above") setSourcePos(textarea, insertAt);
  textarea.dispatchEvent(new Event("input", { bubbles: true }));
}

function pmSetPos(editor: Editor, pos: number, bias: -1 | 1 = 1): void {
  const view = editor.view;
  const doc = view.state.doc;
  const clamped = clamp(pos, 0, doc.content.size);
  const selection = TextSelection.near(doc.resolve(clamped), bias);
  view.dispatch(view.state.tr.setSelection(selection).scrollIntoView());
  view.focus();
}

function pmMoveChar(editor: Editor, dir: -1 | 1): void {
  const selection = editor.view.state.selection;
  const pos = selection.empty
    ? selection.from + dir
    : (dir < 0 ? selection.from : selection.to);
  pmSetPos(editor, pos, dir);
}

function pmMoveLine(editor: Editor, dir: -1 | 1, goalX: number | null): number | null {
  const view = editor.view;
  try {
    const rect = view.coordsAtPos(view.state.selection.from);
    const lineHeightRaw = Number.parseFloat(window.getComputedStyle(view.dom).lineHeight);
    const lineHeight = Number.isFinite(lineHeightRaw) ? lineHeightRaw : 20;
    const x = goalX ?? rect.left;
    const y = dir > 0 ? rect.bottom + lineHeight * 0.75 : rect.top - lineHeight * 0.75;
    const found = view.posAtCoords({ left: x, top: y });
    if (found) pmSetPos(editor, found.pos, dir);
    return x;
  } catch {
    return goalX;
  }
}

function pmLineBoundary(editor: Editor, which: "start" | "end"): void {
  const { selection } = editor.view.state;
  const $from = selection.$from;
  if (!$from.parent.isTextblock) return;
  pmSetPos(editor, which === "start" ? $from.start() : $from.end(), which === "start" ? 1 : -1);
}

function pmDocBoundary(editor: Editor, which: "start" | "end"): void {
  pmSetPos(editor, which === "start" ? 0 : editor.view.state.doc.content.size, which === "start" ? 1 : -1);
}

function pmCharAt(editor: Editor, pos: number): string {
  const doc = editor.view.state.doc;
  if (pos < 0 || pos >= doc.content.size) return "";
  return doc.textBetween(pos, Math.min(pos + 1, doc.content.size), "\n", "\n").charAt(0);
}

function pmMoveWord(editor: Editor, dir: -1 | 1): void {
  const max = editor.view.state.doc.content.size;
  let pos = editor.view.state.selection.from;
  if (dir > 0) {
    while (pos < max && wordChar(pmCharAt(editor, pos))) pos++;
    while (pos < max && !wordChar(pmCharAt(editor, pos))) pos++;
  } else {
    pos = Math.max(0, pos - 1);
    while (pos > 0 && !wordChar(pmCharAt(editor, pos))) pos--;
    while (pos > 0 && wordChar(pmCharAt(editor, pos - 1))) pos--;
  }
  pmSetPos(editor, pos, dir);
}

function pmDeleteChar(editor: Editor): void {
  const view = editor.view;
  const { from, to } = view.state.selection;
  const end = from === to ? Math.min(from + 1, view.state.doc.content.size) : to;
  if (from >= end) return;
  view.dispatch(view.state.tr.delete(from, end).scrollIntoView());
  view.focus();
}

function pmOpenLine(editor: Editor, where: "above" | "below"): void {
  const view = editor.view;
  const $from = view.state.selection.$from;
  if (!$from.parent.isTextblock) {
    pmSetPos(editor, view.state.selection.from);
    return;
  }
  const linePos = where === "above" ? $from.start() : $from.end();
  view.dispatch(view.state.tr.insertText("\n", linePos, linePos).scrollIntoView());
  pmSetPos(editor, where === "above" ? linePos : linePos + 1);
}

export function createVimLite(
  editor: Editor,
  host: HTMLElement,
  options: VimLiteOptions = {},
): VimLiteController {
  let mode: VimLiteMode = "insert";
  let sourceGoalColumn: number | null = null;
  let pmGoalX: number | null = null;
  let pending = "";

  function resetMotionMemory(): void {
    sourceGoalColumn = null;
    pmGoalX = null;
  }

  function setMode(next: VimLiteMode): void {
    if (mode === next) return;
    mode = next;
    pending = "";
    resetMotionMemory();
    options.onModeChange?.(mode);
  }

  function withSurface(
    sourceAction: (textarea: HTMLTextAreaElement) => void,
    pmAction: () => void,
  ): void {
    const textarea = sourceTextarea(host);
    if (textarea) sourceAction(textarea);
    else pmAction();
  }

  function moveChar(dir: -1 | 1): void {
    resetMotionMemory();
    withSurface((textarea) => sourceMoveChar(textarea, dir), () => pmMoveChar(editor, dir));
  }

  function moveLine(dir: -1 | 1): void {
    withSurface(
      (textarea) => { sourceGoalColumn = sourceMoveLine(textarea, dir, sourceGoalColumn); },
      () => { pmGoalX = pmMoveLine(editor, dir, pmGoalX); },
    );
  }

  function moveLineBoundary(which: "start" | "end"): void {
    resetMotionMemory();
    withSurface((textarea) => sourceLineBoundary(textarea, which), () => pmLineBoundary(editor, which));
  }

  function moveWord(dir: -1 | 1): void {
    resetMotionMemory();
    withSurface((textarea) => sourceMoveWord(textarea, dir), () => pmMoveWord(editor, dir));
  }

  function deleteChar(): void {
    resetMotionMemory();
    withSurface(sourceDeleteChar, () => pmDeleteChar(editor));
  }

  function openLine(where: "above" | "below"): void {
    resetMotionMemory();
    withSurface((textarea) => sourceOpenLine(textarea, where), () => pmOpenLine(editor, where));
    setMode("insert");
  }

  function appendChar(): void {
    moveChar(1);
    setMode("insert");
  }

  function normalCommand(key: string): boolean {
    if (pending === "g") {
      pending = "";
      if (key === "g") {
        resetMotionMemory();
        withSurface((textarea) => setSourcePos(textarea, 0), () => pmDocBoundary(editor, "start"));
        return true;
      }
      return true;
    }

    switch (key) {
      case "h":
      case "ArrowLeft":
      case "Backspace":
        moveChar(-1);
        return true;
      case "l":
      case "ArrowRight":
      case " ":
        moveChar(1);
        return true;
      case "j":
      case "ArrowDown":
        moveLine(1);
        return true;
      case "k":
      case "ArrowUp":
        moveLine(-1);
        return true;
      case "0":
        moveLineBoundary("start");
        return true;
      case "$":
        moveLineBoundary("end");
        return true;
      case "w":
        moveWord(1);
        return true;
      case "b":
        moveWord(-1);
        return true;
      case "g":
        pending = "g";
        return true;
      case "G":
        resetMotionMemory();
        withSurface(
          (textarea) => setSourcePos(textarea, textarea.value.length),
          () => pmDocBoundary(editor, "end"),
        );
        return true;
      case "i":
        setMode("insert");
        return true;
      case "a":
        appendChar();
        return true;
      case "I":
        moveLineBoundary("start");
        setMode("insert");
        return true;
      case "A":
        moveLineBoundary("end");
        setMode("insert");
        return true;
      case "o":
        openLine("below");
        return true;
      case "O":
        openLine("above");
        return true;
      case "x":
      case "Delete":
        deleteChar();
        return true;
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
    handleKeyDown(event: KeyboardEvent): boolean {
      if (!targetInEditor(host, event.target)) return false;
      if (event.isComposing) return false;
      if (isEscape(event)) {
        event.preventDefault();
        setMode("normal");
        return true;
      }
      if (mode === "insert") return false;
      if (hasCommandModifier(event)) return false;

      const handled = normalCommand(event.key);
      if (handled) event.preventDefault();
      return handled;
    },
  };
}
