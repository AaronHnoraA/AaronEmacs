import type { Editor } from "../src/lib.ts";
import type { VimLiteMode } from "./vim-lite.ts";

type CursorRect = { left: number; top: number; bottom: number };

export function createVimCursor(): HTMLElement {
  const el = document.createElement("div");
  el.className = "aaronnote-vim-cursor";
  el.hidden = true;
  document.body.appendChild(el);
  return el;
}

export function updateVimCursor(
  el: HTMLElement,
  editor: Editor,
  mode: VimLiteMode,
  rect?: CursorRect | null,
): void {
  if (mode === "insert") {
    el.hidden = true;
    return;
  }
  const cursorRect = rect === undefined ? editor.cursorRect() : rect;
  if (!cursorRect) {
    el.hidden = true;
    return;
  }
  const height = Math.max(16, cursorRect.bottom - cursorRect.top);
  const width = mode === "visual-line" ? 4 : Math.max(8, Math.round(height * 0.58));
  el.style.left = `${cursorRect.left}px`;
  el.style.top = `${cursorRect.top}px`;
  el.style.width = `${width}px`;
  el.style.height = `${height}px`;
  el.hidden = false;
}
