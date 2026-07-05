import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import {
  handleAaronnoteJupyterBeforeInput,
  handleAaronnoteJupyterKeydown,
  installAaronnoteJupyterKeyboard,
} from "../jupyter/labextension/src/keyboard.ts";

function mountedEditor(): { host: HTMLElement; content: HTMLElement } {
  const host = document.createElement("div");
  host.className = "cm-editor cm-focused";
  const content = document.createElement("div");
  content.className = "cm-content";
  content.contentEditable = "true";
  host.appendChild(content);
  document.body.appendChild(host);
  return { host, content };
}

function keydown(target: EventTarget, key: string): KeyboardEvent {
  const event = new KeyboardEvent("keydown", {
    key,
    bubbles: true,
    cancelable: true,
  });
  target.dispatchEvent(event);
  return event;
}

describe("Aaronnote Jupyter keyboard bridge", () => {
  test("normalizes raw xwidget Delete keydown before it can insert a glyph", () => {
    const { host, content } = mountedEditor();
    const normalized: string[] = [];
    content.addEventListener("keydown", (event) => normalized.push(event.key));
    try {
      const event = new KeyboardEvent("keydown", {
        key: "\u007f",
        bubbles: true,
        cancelable: true,
      });
      Object.defineProperty(event, "target", { value: document.body });
      expect(handleAaronnoteJupyterKeydown(event)).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(normalized).toEqual(["Delete"]);
    } finally {
      host.remove();
    }
  });

  test("normalizes raw xwidget Backspace beforeinput", () => {
    const { host, content } = mountedEditor();
    const normalized: string[] = [];
    content.addEventListener("keydown", (event) => normalized.push(event.key));
    try {
      const event = new InputEvent("beforeinput", {
        data: "\u0008",
        inputType: "insertText",
        bubbles: true,
        cancelable: true,
      });
      Object.defineProperty(event, "target", { value: document.body });
      expect(handleAaronnoteJupyterBeforeInput(event)).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(normalized).toEqual(["Backspace"]);
    } finally {
      host.remove();
    }
  });

  test("does not intercept ordinary keys or native input fields", () => {
    const { host, content } = mountedEditor();
    const input = document.createElement("input");
    document.body.appendChild(input);
    try {
      expect(handleAaronnoteJupyterKeydown(keydown(content, "a"))).toBe(false);
      expect(handleAaronnoteJupyterKeydown(keydown(input, "Delete"))).toBe(false);
    } finally {
      input.remove();
      host.remove();
    }
  });

  test("routes standard Delete from document body to the focused editor", () => {
    const { host, content } = mountedEditor();
    const normalized: string[] = [];
    content.addEventListener("keydown", (event) => normalized.push(event.key));
    try {
      const event = new KeyboardEvent("keydown", {
        key: "Delete",
        bubbles: true,
        cancelable: true,
      });
      Object.defineProperty(event, "target", { value: document.body });
      expect(handleAaronnoteJupyterKeydown(event)).toBe(true);
      expect(event.defaultPrevented).toBe(true);
      expect(normalized).toEqual(["Delete"]);
    } finally {
      host.remove();
    }
  });

  test("installer attaches capture listeners and returns a disposer", () => {
    const { host, content } = mountedEditor();
    const normalized: string[] = [];
    content.addEventListener("keydown", (event) => normalized.push(event.key));
    try {
      const dispose = installAaronnoteJupyterKeyboard();
      keydown(document.body, "\u007f");
      dispose();
      keydown(document.body, "\u007f");
      expect(normalized).toEqual(["Delete"]);
    } finally {
      host.remove();
    }
  });
});
