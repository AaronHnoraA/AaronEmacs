import type { Editor } from "../src/lib.ts";
import type { VimLiteController } from "./vim-lite.ts";
import {
  cursorCharLeft,
  cursorCharRight,
  cursorLineBoundaryBackward,
  cursorLineBoundaryForward,
  cursorLineDown,
  cursorLineUp,
  cursorPageDown,
  cursorPageUp,
  indentWithTab,
  insertNewlineAndIndent,
  selectCharLeft,
  selectCharRight,
  selectLineBoundaryBackward,
  selectLineBoundaryForward,
  selectLineDown,
  selectLineUp,
  selectPageDown,
  selectPageUp,
} from "@codemirror/commands";
import { exitEmptyMarkdownBlock, indentMarkdownList } from "../src/cm6/commands.ts";

type XwidgetControlKey = "Escape" | "Delete" | "Backspace";
type XwidgetSpecialKey =
  | "Enter"
  | "Tab"
  | "ArrowLeft"
  | "ArrowRight"
  | "ArrowUp"
  | "ArrowDown"
  | "Home"
  | "End"
  | "PageUp"
  | "PageDown";
type XwidgetKeyContext = {
  editor: Editor;
  editorHost: HTMLElement;
  vim: Pick<VimLiteController, "handleKey" | "mode" | "setMode">;
  enabled?: boolean;
};

const XWIDGET_CONTROL_KEYS = new Set<XwidgetControlKey>(["Escape", "Delete", "Backspace"]);
const XWIDGET_SPECIAL_KEYS = new Set<XwidgetSpecialKey>([
  "Enter",
  "Tab",
  "ArrowLeft",
  "ArrowRight",
  "ArrowUp",
  "ArrowDown",
  "Home",
  "End",
  "PageUp",
  "PageDown",
]);
const DUPLICATE_BEFOREINPUT_MS = 80;
let lastHandledKeydown: { editor: Editor; key: string; at: number } | null = null;

function targetElement(target: EventTarget | null): Element | null {
  if (target instanceof Element) return target;
  if (target instanceof Node && target.parentElement) return target.parentElement;
  return null;
}

function isTextEditingTarget(target: EventTarget | null, editorHost: HTMLElement): boolean {
  const element = targetElement(target);
  if (!element) return false;
  if (element.closest("input, textarea, select")) return true;
  const editable = element.closest<HTMLElement>("[contenteditable]");
  if (!editable || editable.contentEditable === "false") return false;
  return !(editorHost.contains(editable) && editable.classList.contains("cm-content"));
}

function hardStop(event: Event): void {
  event.preventDefault();
  event.stopPropagation();
  event.stopImmediatePropagation();
}

function xwidgetControlText(text: string | null): boolean {
  return typeof text === "string" && /[\u0008\u001b\u007f]/u.test(text);
}

function controlKeyFromKeyboardEvent(event: KeyboardEvent): XwidgetControlKey | null {
  return XWIDGET_CONTROL_KEYS.has(event.key as XwidgetControlKey)
    ? event.key as XwidgetControlKey
    : null;
}

function controlKeyFromInputEvent(event: InputEvent): XwidgetControlKey | null {
  if (event.inputType === "deleteContentBackward") return "Backspace";
  if (event.inputType === "deleteContentForward") return "Delete";
  const data = event.data;
  if (!xwidgetControlText(data)) return null;
  if (data!.includes("\u001b")) return "Escape";
  if (data!.includes("\u007f")) return "Delete";
  if (data!.includes("\u0008")) return "Backspace";
  return null;
}

function specialKeyFromKeyboardEvent(event: KeyboardEvent): XwidgetSpecialKey | null {
  return XWIDGET_SPECIAL_KEYS.has(event.key as XwidgetSpecialKey)
    ? event.key as XwidgetSpecialKey
    : null;
}

function specialKeyFromInputEvent(event: InputEvent): XwidgetSpecialKey | null {
  if (event.inputType === "insertParagraph" || event.inputType === "insertLineBreak") return "Enter";
  if (event.inputType === "insertText" && event.data === "\t") return "Tab";
  return null;
}

function shouldHandleXwidgetControlEvent(
  event: KeyboardEvent | InputEvent,
  editorHost: HTMLElement,
  key: XwidgetControlKey | null,
): key is XwidgetControlKey {
  if (event.defaultPrevented || event.isComposing) return false;
  if (event instanceof KeyboardEvent && (event.ctrlKey || event.metaKey || event.altKey)) return false;
  if (!key) return false;
  if (isTextEditingTarget(event.target, editorHost)) return false;
  if (isTextEditingTarget(document.activeElement, editorHost)) return false;
  return true;
}

function shouldHandleXwidgetSpecialEvent(
  event: KeyboardEvent | InputEvent,
  context: XwidgetKeyContext,
  key: XwidgetSpecialKey | null,
): key is XwidgetSpecialKey {
  if (context.enabled === false || context.vim.mode() !== "insert") return false;
  if (event.defaultPrevented || event.isComposing) return false;
  if (!key) return false;
  if (event instanceof KeyboardEvent && (event.ctrlKey || event.metaKey || event.altKey)) return false;
  if (isTextEditingTarget(event.target, context.editorHost)) return false;
  if (isTextEditingTarget(document.activeElement, context.editorHost)) return false;
  return true;
}

function deleteFromEditor(editor: Editor, key: "Delete" | "Backspace"): void {
  const { from, to } = editor.getMarkdownSelection();
  if (from !== to) {
    editor.replaceMarkdownRange(from, to, "", "start");
    return;
  }

  const docLength = editor.view.state.doc.length;
  if (key === "Backspace") {
    if (from > 0) editor.replaceMarkdownRange(from - 1, from, "", "start");
    return;
  }

  if (from < docLength) editor.replaceMarkdownRange(from, from + 1, "", "start");
}

function nowMs(): number {
  return globalThis.performance?.now?.() ?? Date.now();
}

function noteHandledKeydown(editor: Editor, key: string): void {
  lastHandledKeydown = { editor, key, at: nowMs() };
}

function recentlyHandledKeydown(editor: Editor, key: string): boolean {
  return Boolean(
    lastHandledKeydown
      && lastHandledKeydown.editor === editor
      && lastHandledKeydown.key === key
      && nowMs() - lastHandledKeydown.at < DUPLICATE_BEFOREINPUT_MS,
  );
}

function runEditorControlKey(key: XwidgetControlKey, context: XwidgetKeyContext): void {
  if (key === "Escape") {
    context.vim.setMode("normal");
    context.editor.focus();
    return;
  }

  if (context.vim.mode() === "insert") {
    deleteFromEditor(context.editor, key);
  } else {
    context.vim.handleKey({ key });
  }
  context.editor.focus();
}

function runEditorSpecialKey(key: XwidgetSpecialKey, context: XwidgetKeyContext, shiftKey = false): boolean {
  const view = context.editor.view;
  const command = (() => {
    switch (key) {
      case "Enter":
        return () => exitEmptyMarkdownBlock(view) || insertNewlineAndIndent(view);
      case "Tab":
        return () => shiftKey
          ? indentMarkdownList(view, -1)
          : indentMarkdownList(view, 1) || indentWithTab.run?.(view) === true;
      case "ArrowLeft":
        return shiftKey ? selectCharLeft : cursorCharLeft;
      case "ArrowRight":
        return shiftKey ? selectCharRight : cursorCharRight;
      case "ArrowUp":
        return shiftKey ? selectLineUp : cursorLineUp;
      case "ArrowDown":
        return shiftKey ? selectLineDown : cursorLineDown;
      case "Home":
        return shiftKey ? selectLineBoundaryBackward : cursorLineBoundaryBackward;
      case "End":
        return shiftKey ? selectLineBoundaryForward : cursorLineBoundaryForward;
      case "PageUp":
        return shiftKey ? selectPageUp : cursorPageUp;
      case "PageDown":
        return shiftKey ? selectPageDown : cursorPageDown;
    }
  })();
  const handled = command(view);
  if (handled) context.editor.focus();
  return handled;
}

function shouldHandleXwidgetVimKey(event: KeyboardEvent | InputEvent, context: XwidgetKeyContext): boolean {
  if (context.enabled === false || context.vim.mode() === "insert") return false;
  if (event.defaultPrevented || event.isComposing) return false;
  if (isTextEditingTarget(event.target, context.editorHost)) return false;
  if (isTextEditingTarget(document.activeElement, context.editorHost)) return false;
  return true;
}

export function handleXwidgetSpecialKeydown(event: KeyboardEvent, context: XwidgetKeyContext): boolean {
  const key = specialKeyFromKeyboardEvent(event);
  if (!shouldHandleXwidgetSpecialEvent(event, context, key)) return false;
  if (!runEditorSpecialKey(key, context, event.shiftKey)) return false;
  hardStop(event);
  noteHandledKeydown(context.editor, event.key);
  return true;
}

export function handleXwidgetControlKeydown(
  event: KeyboardEvent,
  context: XwidgetKeyContext,
): boolean {
  if (context.enabled === false) return false;
  const key = controlKeyFromKeyboardEvent(event);
  if (!shouldHandleXwidgetControlEvent(event, context.editorHost, key)) return false;

  hardStop(event);
  noteHandledKeydown(context.editor, key);
  runEditorControlKey(key, context);
  return true;
}

export function handleXwidgetVimKeydown(event: KeyboardEvent, context: XwidgetKeyContext): boolean {
  if (!shouldHandleXwidgetVimKey(event, context)) return false;
  const handled = context.vim.handleKey({
    key: event.key,
    ctrlKey: event.ctrlKey,
    metaKey: event.metaKey,
    altKey: event.altKey,
    shiftKey: event.shiftKey,
    isComposing: event.isComposing,
  });
  if (!handled) return false;

  hardStop(event);
  noteHandledKeydown(context.editor, event.key);
  context.editor.focus();
  return true;
}

export function handleXwidgetControlBeforeInput(event: InputEvent, context: XwidgetKeyContext): boolean {
  if (context.enabled === false) return false;
  const key = controlKeyFromInputEvent(event);
  if (!shouldHandleXwidgetControlEvent(event, context.editorHost, key)) return false;
  hardStop(event);
  if (!recentlyHandledKeydown(context.editor, key)) runEditorControlKey(key, context);
  return true;
}

export function handleXwidgetSpecialBeforeInput(event: InputEvent, context: XwidgetKeyContext): boolean {
  const key = specialKeyFromInputEvent(event);
  if (!shouldHandleXwidgetSpecialEvent(event, context, key)) return false;
  hardStop(event);
  if (!recentlyHandledKeydown(context.editor, key)) runEditorSpecialKey(key, context);
  return true;
}

export function handleXwidgetVimBeforeInput(event: InputEvent, context: XwidgetKeyContext): boolean {
  if (!shouldHandleXwidgetVimKey(event, context)) return false;
  if (!event.inputType.startsWith("insert") || typeof event.data !== "string" || event.data.length === 0) return false;
  hardStop(event);
  if (event.data.length === 1 && !recentlyHandledKeydown(context.editor, event.data)) {
    context.vim.handleKey({ key: event.data });
    context.editor.focus();
  }
  return true;
}

export function shouldGuardXwidgetControlKeydown(event: KeyboardEvent, editorHost: HTMLElement): boolean {
  if (!shouldHandleXwidgetControlEvent(event, editorHost, controlKeyFromKeyboardEvent(event))) return false;
  return true;
}

export function guardXwidgetControlKeydown(event: KeyboardEvent, editorHost: HTMLElement): boolean {
  if (!shouldGuardXwidgetControlKeydown(event, editorHost)) return false;
  hardStop(event);
  return true;
}

export function guardXwidgetControlBeforeInput(event: InputEvent): boolean {
  if (!shouldHandleXwidgetControlEvent(event, document.body, controlKeyFromInputEvent(event))) return false;
  hardStop(event);
  return true;
}
