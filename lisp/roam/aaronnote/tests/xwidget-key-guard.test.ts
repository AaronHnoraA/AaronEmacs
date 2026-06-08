import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import { createEditor } from "../src/lib.ts";
import { createVimLite } from "../aaronnote/vim-lite.ts";
import {
  guardXwidgetControlBeforeInput,
  handleXwidgetEmacsKeydown,
  handleXwidgetControlBeforeInput,
  guardXwidgetControlKeydown,
  handleXwidgetControlKeydown,
  handleXwidgetSpecialBeforeInput,
  handleXwidgetSpecialKeydown,
  handleXwidgetVimBeforeInput,
  handleXwidgetVimKeydown,
} from "../aaronnote/xwidget-key-guard.ts";

function runGuard(
  target: HTMLElement,
  key: string,
  host: HTMLElement,
  init: KeyboardEventInit = {},
): { guarded: boolean; defaultPrevented: boolean } {
  let guarded = false;
  const listener = (event: KeyboardEvent): void => {
    guarded = guardXwidgetControlKeydown(event, host);
  };
  document.addEventListener("keydown", listener, true);
  const event = new KeyboardEvent("keydown", {
    key,
    bubbles: true,
    cancelable: true,
    ...init,
  });
  target.dispatchEvent(event);
  document.removeEventListener("keydown", listener, true);
  return { guarded, defaultPrevented: event.defaultPrevented };
}

function withMounted<T extends HTMLElement>(element: T): T {
  document.body.appendChild(element);
  return element;
}

describe("xwidget key guard", () => {
  test("guards known control keys outside editor and text controls", () => {
    const host = withMounted(document.createElement("section"));
    const button = withMounted(document.createElement("button"));
    try {
      for (const key of ["Escape", "Delete", "Backspace"]) {
        const result = runGuard(button, key, host);
        expect(result.guarded).toBe(true);
        expect(result.defaultPrevented).toBe(true);
      }
    } finally {
      button.remove();
      host.remove();
    }
  });

  test("does not guard ordinary text keys or modified control keys", () => {
    const host = withMounted(document.createElement("section"));
    const button = withMounted(document.createElement("button"));
    try {
      expect(runGuard(button, "a", host)).toEqual({ guarded: false, defaultPrevented: false });
      expect(runGuard(button, "Delete", host, { metaKey: true })).toEqual({ guarded: false, defaultPrevented: false });
      expect(runGuard(button, "Escape", host, { ctrlKey: true })).toEqual({ guarded: false, defaultPrevented: false });
    } finally {
      button.remove();
      host.remove();
    }
  });

  test("leaves text editing targets alone", () => {
    const host = withMounted(document.createElement("section"));
    const input = withMounted(document.createElement("input"));
    const textarea = withMounted(document.createElement("textarea"));
    const editable = withMounted(document.createElement("div"));
    editable.contentEditable = "true";
    try {
      for (const target of [input, textarea, editable]) {
        expect(runGuard(target, "Delete", host)).toEqual({ guarded: false, defaultPrevented: false });
        expect(runGuard(target, "Backspace", host)).toEqual({ guarded: false, defaultPrevented: false });
      }
    } finally {
      input.remove();
      textarea.remove();
      editable.remove();
      host.remove();
    }
  });

  test("guards editor-host control keys before CodeMirror sees them", () => {
    const host = withMounted(document.createElement("section"));
    const editorContent = document.createElement("div");
    host.appendChild(editorContent);
    try {
      expect(runGuard(editorContent, "Delete", host)).toEqual({ guarded: true, defaultPrevented: true });
      expect(runGuard(editorContent, "Escape", host)).toEqual({ guarded: true, defaultPrevented: true });
    } finally {
      host.remove();
    }
  });

  test("handles Delete and Backspace through the editor API even when focus is not in CM6", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "abc" });
    const vim = createVimLite(editor, host);
    const target = document.body;
    editor.setMarkdownSelection(1);
    try {
      const del = new KeyboardEvent("keydown", { key: "Delete", bubbles: true, cancelable: true });
      Object.defineProperty(del, "target", { value: target });
      expect(handleXwidgetControlKeydown(del, { editor, editorHost: host, vim })).toBe(true);
      expect(del.defaultPrevented).toBe(true);
      expect(editor.getMarkdown()).toBe("ac");

      const backspace = new KeyboardEvent("keydown", { key: "Backspace", bubbles: true, cancelable: true });
      Object.defineProperty(backspace, "target", { value: target });
      expect(handleXwidgetControlKeydown(backspace, { editor, editorHost: host, vim })).toBe(true);
      expect(backspace.defaultPrevented).toBe(true);
      expect(editor.getMarkdown()).toBe("c");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("maps xwidget beforeinput control bytes into editor actions", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "abc" });
    const vim = createVimLite(editor, host);
    editor.setMarkdownSelection(1);
    try {
      const delText = new InputEvent("beforeinput", {
        bubbles: true,
        cancelable: true,
        data: "\u007f",
        inputType: "insertText",
      });
      Object.defineProperty(delText, "target", { value: document.body });
      expect(handleXwidgetControlBeforeInput(delText, { editor, editorHost: host, vim })).toBe(true);
      expect(delText.defaultPrevented).toBe(true);
      expect(editor.getMarkdown()).toBe("ac");

      vim.setMode("insert");
      const escText = new InputEvent("beforeinput", {
        bubbles: true,
        cancelable: true,
        data: "\u001b",
        inputType: "insertText",
      });
      Object.defineProperty(escText, "target", { value: document.body });
      expect(handleXwidgetControlBeforeInput(escText, { editor, editorHost: host, vim })).toBe(true);
      expect(escText.defaultPrevented).toBe(true);
      expect(vim.mode()).toBe("normal");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("maps delete beforeinput inputTypes into editor actions", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "abc" });
    const vim = createVimLite(editor, host);
    editor.setMarkdownSelection(2);
    try {
      const backspace = new InputEvent("beforeinput", {
        bubbles: true,
        cancelable: true,
        data: null,
        inputType: "deleteContentBackward",
      });
      Object.defineProperty(backspace, "target", { value: document.body });
      expect(handleXwidgetControlBeforeInput(backspace, { editor, editorHost: host, vim })).toBe(true);
      expect(editor.getMarkdown()).toBe("ac");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("handles insert-mode Enter through CM6 commands when focus is not in CM6", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "abc" });
    const vim = createVimLite(editor, host);
    vim.setMode("insert");
    editor.setMarkdownSelection(1);
    try {
      const event = new KeyboardEvent("keydown", { key: "Enter", bubbles: true, cancelable: true });
      Object.defineProperty(event, "target", { value: document.body });
      expect(handleXwidgetSpecialKeydown(event, { editor, editorHost: host, vim })).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(editor.getMarkdown()).toBe("a\nbc");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("handles insert-mode Tab and Shift-Tab through CM6 list indentation", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "- item" });
    const vim = createVimLite(editor, host);
    vim.setMode("insert");
    editor.setMarkdownSelection(2);
    try {
      const tab = new KeyboardEvent("keydown", { key: "Tab", bubbles: true, cancelable: true });
      Object.defineProperty(tab, "target", { value: document.body });
      expect(handleXwidgetSpecialKeydown(tab, { editor, editorHost: host, vim })).toBe(true);
      expect(tab.defaultPrevented).toBe(true);
      expect(editor.getMarkdown()).toBe("  - item");

      const shiftTab = new KeyboardEvent("keydown", { key: "Tab", shiftKey: true, bubbles: true, cancelable: true });
      Object.defineProperty(shiftTab, "target", { value: document.body });
      expect(handleXwidgetSpecialKeydown(shiftTab, { editor, editorHost: host, vim })).toBe(true);
      expect(editor.getMarkdown()).toBe("- item");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("keeps insert-mode Shift-Tab from escaping focus on plain text", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "plain" });
    const vim = createVimLite(editor, host);
    vim.setMode("insert");
    editor.setMarkdownSelection(5);
    try {
      const event = new KeyboardEvent("keydown", { key: "Tab", shiftKey: true, bubbles: true, cancelable: true });
      Object.defineProperty(event, "target", { value: document.body });
      expect(handleXwidgetSpecialKeydown(event, { editor, editorHost: host, vim })).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(editor.getMarkdown()).toBe("plain");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("handles Backtab as insert-mode Shift-Tab", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "  - item" });
    const vim = createVimLite(editor, host);
    vim.setMode("insert");
    editor.setMarkdownSelection(4);
    try {
      const event = new KeyboardEvent("keydown", { key: "Backtab", bubbles: true, cancelable: true });
      Object.defineProperty(event, "target", { value: document.body });
      expect(handleXwidgetSpecialKeydown(event, { editor, editorHost: host, vim })).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(editor.getMarkdown()).toBe("- item");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("handles insert-mode arrow keys through CM6 cursor commands", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "abc" });
    const vim = createVimLite(editor, host);
    vim.setMode("insert");
    editor.setMarkdownSelection(2);
    try {
      const event = new KeyboardEvent("keydown", { key: "ArrowLeft", bubbles: true, cancelable: true });
      Object.defineProperty(event, "target", { value: document.body });
      expect(handleXwidgetSpecialKeydown(event, { editor, editorHost: host, vim })).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(editor.getMarkdownSelection().from).toBe(1);
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("maps insertParagraph beforeinput into CM6 Enter behavior", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "abc" });
    const vim = createVimLite(editor, host);
    vim.setMode("insert");
    editor.setMarkdownSelection(1);
    try {
      const event = new InputEvent("beforeinput", {
        bubbles: true,
        cancelable: true,
        data: null,
        inputType: "insertParagraph",
      });
      Object.defineProperty(event, "target", { value: document.body });
      expect(handleXwidgetSpecialBeforeInput(event, { editor, editorHost: host, vim })).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(editor.getMarkdown()).toBe("a\nbc");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("handles Escape as a first-layer Vim mode switch", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "abc" });
    const vim = createVimLite(editor, host);
    const target = editor.view.contentDOM;
    vim.setMode("insert");
    try {
      const event = new KeyboardEvent("keydown", { key: "Escape", bubbles: true, cancelable: true });
      Object.defineProperty(event, "target", { value: target });
      expect(handleXwidgetControlKeydown(event, { editor, editorHost: host, vim })).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(vim.mode()).toBe("normal");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("handles normal-mode Vim keydown even when focus is not in CM6", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "aa\nbbbb\ncc" });
    const vim = createVimLite(editor, host);
    vim.setMode("normal");
    editor.setMarkdownSelection(1);
    try {
      const down = new KeyboardEvent("keydown", { key: "j", bubbles: true, cancelable: true });
      Object.defineProperty(down, "target", { value: document.body });
      expect(handleXwidgetVimKeydown(down, { editor, editorHost: host, vim })).toBe(true);
      expect(down.defaultPrevented).toBe(true);
      expect(editor.getMarkdownSelection().from).toBe(4);

      const deleteChar = new KeyboardEvent("keydown", { key: "x", bubbles: true, cancelable: true });
      Object.defineProperty(deleteChar, "target", { value: document.body });
      expect(handleXwidgetVimKeydown(deleteChar, { editor, editorHost: host, vim })).toBe(true);
      expect(editor.getMarkdown()).toBe("aa\nbbb\ncc");
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("maps normal-mode beforeinput text into Vim commands instead of inserting", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "aa\nbbbb\ncc" });
    const vim = createVimLite(editor, host);
    vim.setMode("normal");
    editor.setMarkdownSelection(1);
    try {
      const input = new InputEvent("beforeinput", {
        bubbles: true,
        cancelable: true,
        data: "j",
        inputType: "insertText",
      });
      Object.defineProperty(input, "target", { value: document.body });
      expect(handleXwidgetVimBeforeInput(input, { editor, editorHost: host, vim })).toBe(true);
      expect(input.defaultPrevented).toBe(true);
      expect(editor.getMarkdown()).toBe("aa\nbbbb\ncc");
      expect(editor.getMarkdownSelection().from).toBe(4);
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("leaves insert-mode ordinary beforeinput alone", () => {
    const host = withMounted(document.createElement("section"));
    const editor = createEditor(host, { initialContent: "abc" });
    const vim = createVimLite(editor, host);
    vim.setMode("insert");
    try {
      const input = new InputEvent("beforeinput", {
        bubbles: true,
        cancelable: true,
        data: "j",
        inputType: "insertText",
      });
      Object.defineProperty(input, "target", { value: document.body });
      expect(handleXwidgetVimBeforeInput(input, { editor, editorHost: host, vim })).toBe(false);
      expect(input.defaultPrevented).toBe(false);
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("blocks xwidget control text before it is inserted", () => {
    const delText = new InputEvent("beforeinput", {
      bubbles: true,
      cancelable: true,
      data: "\u007f",
      inputType: "insertText",
    });
    expect(guardXwidgetControlBeforeInput(delText)).toBe(true);
    expect(delText.defaultPrevented).toBe(true);

    const escText = new InputEvent("beforeinput", {
      bubbles: true,
      cancelable: true,
      data: "\u001b",
      inputType: "insertText",
    });
    expect(guardXwidgetControlBeforeInput(escText)).toBe(true);
    expect(escText.defaultPrevented).toBe(true);

    const normalText = new InputEvent("beforeinput", {
      bubbles: true,
      cancelable: true,
      data: "a",
      inputType: "insertText",
    });
    expect(guardXwidgetControlBeforeInput(normalText)).toBe(false);
    expect(normalText.defaultPrevented).toBe(false);
  });

  test("releases web input focus before forwarding a top-level Emacs key", () => {
    const input = withMounted(document.createElement("input"));
    const forwarded: string[] = [];
    const win = window as Window & {
      aaronnoteApi?: { emacs?: { key?: (key: string) => unknown } };
    };
    const previousApi = win.aaronnoteApi;
    try {
      win.aaronnoteApi = {
        emacs: {
          key: async (key) => {
            forwarded.push(key);
          },
        },
      };
      input.focus();
      expect(document.activeElement).toBe(input);

      const event = new KeyboardEvent("keydown", {
        key: "ø",
        code: "KeyO",
        altKey: true,
        bubbles: true,
        cancelable: true,
      });
      Object.defineProperty(event, "target", { value: input });

      expect(handleXwidgetEmacsKeydown(event)).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(forwarded).toEqual(["H-o"]);
      expect(document.activeElement).not.toBe(input);
    } finally {
      win.aaronnoteApi = previousApi;
      input.remove();
    }
  });
});
