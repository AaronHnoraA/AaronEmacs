import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import { setupCopilot } from "../src/copilot/index.ts";

class FakeEditor {
  markdown: string;
  selection: { from: number; to: number };
  cursorBefore = "";
  cursorAfter = "";
  insertions: string[] = [];

  constructor(markdown: string) {
    this.markdown = markdown;
    this.selection = { from: markdown.length, to: markdown.length };
  }

  getMarkdown(): string {
    return this.markdown;
  }

  getMarkdownSelection(): { from: number; to: number } {
    return this.selection;
  }

  insertText(text: string): { from: number; to: number } {
    const from = this.selection.from;
    const to = this.selection.to;
    this.insertions.push(text);
    this.markdown = `${this.markdown.slice(0, from)}${text}${this.markdown.slice(to)}`;
    this.selection = { from: from + text.length, to: from + text.length };
    return { from, to: from + text.length };
  }

  cursorContext(): { before: string; after: string; rect: { left: number; top: number; bottom: number } } {
    return { before: this.cursorBefore, after: this.cursorAfter, rect: { left: 0, top: 0, bottom: 20 } };
  }

  revealCursor(): void {}
}

function waitForMicrotasks(): Promise<void> {
  return new Promise((resolve) => window.setTimeout(resolve, 0));
}

function installNativeCopilot(handler: (action: string, body: unknown) => Promise<unknown>): () => void {
  const target = window as Window & { aaronnoteApi?: { copilot?: { request?: (action: string, body?: unknown) => Promise<unknown> } } };
  const oldApi = target.aaronnoteApi;
  target.aaronnoteApi = {
    ...(oldApi ?? {}),
    copilot: {
      ...(oldApi?.copilot ?? {}),
      request: handler,
    },
  };
  return () => {
    if (oldApi) target.aaronnoteApi = oldApi;
    else delete target.aaronnoteApi;
  };
}

describe("copilot plugin insertion", () => {
  test("accepting a suggestion inserts at the cursor instead of replacing the LSP range", async () => {
    const host = document.createElement("div");
    const target = document.createElement("button");
    host.appendChild(target);
    document.body.appendChild(host);

    const editor = new FakeEditor("prefix");
    const handlers: {
      key?: (event: KeyboardEvent) => boolean;
      action?: (action: string) => void;
    } = {};

    const restoreApi = installNativeCopilot(async (action) => {
      if (action === "inline") {
        return {
          items: [{
            insertText: "prefixSuffix",
            range: { from: 0, to: editor.markdown.length },
            item: { insertText: "prefixSuffix" },
          }],
        };
      }
      return { ok: true };
    });

    const cleanup = setupCopilot({
      editor,
      host,
      currentFile: () => "/tmp/copilot.md",
      vimMode: () => "insert",
      setStatus: () => {},
      onChange: () => () => {},
      onKeyDown: (handler: (event: KeyboardEvent) => boolean) => {
        handlers.key = handler;
        return () => {
          delete handlers.key;
        };
      },
      onAction: (handler: (action: string) => void) => {
        handlers.action = handler;
        return () => {
          delete handlers.action;
        };
      },
      onSettingsChange: () => () => {},
      getSettings: () => ({ idleDelayMs: 999_999, largeBufferThresholdKb: 512 }),
      onDocumentEvent: () => () => {},
      jumpSnippetNext: () => false,
      jumpSnippetPrevious: () => false,
      forwardDelimiter: () => false,
      backwardDelimiter: () => false,
    });

    try {
      target.focus();
      handlers.action?.("trigger");
      await waitForMicrotasks();
      await waitForMicrotasks();
      expect(document.querySelector(".aaronnote-copilot-ghost")?.textContent).toBe("Suffix");

      target.addEventListener("keydown", (event) => {
        handlers.key?.(event);
      });
      target.dispatchEvent(new KeyboardEvent("keydown", {
        key: "]",
        metaKey: true,
        bubbles: true,
        cancelable: true,
      }));

      expect(editor.insertions).toEqual(["Suffix"]);
      expect(editor.markdown).toBe("prefixSuffix");
    } finally {
      cleanup();
      restoreApi();
      host.remove();
    }
  });

  test("cmd-right-bracket accepts visible copilot text before snippet jumping", async () => {
    const host = document.createElement("div");
    const target = document.createElement("button");
    host.appendChild(target);
    document.body.appendChild(host);

    const editor = new FakeEditor("prefix");
    const handlers: {
      key?: (event: KeyboardEvent) => boolean;
      action?: (action: string) => void;
    } = {};
    let jumpedSnippet = false;

    const restoreApi = installNativeCopilot(async (action) => {
      if (action === "inline") {
        return {
          items: [{
            insertText: "prefixTopLevel",
            range: { from: 0, to: editor.markdown.length },
            item: { insertText: "prefixTopLevel" },
          }],
        };
      }
      return { ok: true };
    });

    const cleanup = setupCopilot({
      editor,
      host,
      currentFile: () => "/tmp/copilot.md",
      vimMode: () => "insert",
      setStatus: () => {},
      onChange: () => () => {},
      onKeyDown: (handler: (event: KeyboardEvent) => boolean) => {
        handlers.key = handler;
        return () => {
          delete handlers.key;
        };
      },
      onAction: (handler: (action: string) => void) => {
        handlers.action = handler;
        return () => {
          delete handlers.action;
        };
      },
      onSettingsChange: () => () => {},
      getSettings: () => ({ idleDelayMs: 999_999, largeBufferThresholdKb: 512 }),
      onDocumentEvent: () => () => {},
      jumpSnippetNext: () => {
        jumpedSnippet = true;
        return true;
      },
      jumpSnippetPrevious: () => false,
      forwardDelimiter: () => false,
      backwardDelimiter: () => false,
    });

    try {
      target.focus();
      handlers.action?.("trigger");
      await waitForMicrotasks();
      await waitForMicrotasks();

      target.addEventListener("keydown", (event) => {
        handlers.key?.(event);
      });
      target.dispatchEvent(new KeyboardEvent("keydown", {
        key: "]",
        metaKey: true,
        bubbles: true,
        cancelable: true,
      }));

      expect(jumpedSnippet).toBe(false);
      expect(editor.insertions).toEqual(["TopLevel"]);
      expect(editor.markdown).toBe("prefixTopLevel");
    } finally {
      cleanup();
      restoreApi();
      host.remove();
    }
  });

  test("cmd-shift-right-bracket enters accept-to-char mode on bracket-key layouts", async () => {
    const host = document.createElement("div");
    const target = document.createElement("button");
    host.appendChild(target);
    document.body.appendChild(host);

    const editor = new FakeEditor("prefix");
    const handlers: {
      key?: (event: KeyboardEvent) => boolean;
      action?: (action: string) => void;
    } = {};

    const restoreApi = installNativeCopilot(async (action) => {
      if (action === "inline") {
        return {
          items: [{
            insertText: "prefixAlphaBeta",
            range: { from: 0, to: editor.markdown.length },
            item: { insertText: "prefixAlphaBeta" },
          }],
        };
      }
      return { ok: true };
    });

    const cleanup = setupCopilot({
      editor,
      host,
      currentFile: () => "/tmp/copilot.md",
      vimMode: () => "insert",
      setStatus: () => {},
      onChange: () => () => {},
      onKeyDown: (handler: (event: KeyboardEvent) => boolean) => {
        handlers.key = handler;
        return () => {
          delete handlers.key;
        };
      },
      onAction: (handler: (action: string) => void) => {
        handlers.action = handler;
        return () => {
          delete handlers.action;
        };
      },
      onSettingsChange: () => () => {},
      getSettings: () => ({ idleDelayMs: 999_999, largeBufferThresholdKb: 512 }),
      onDocumentEvent: () => () => {},
      jumpSnippetNext: () => false,
      jumpSnippetPrevious: () => false,
      forwardDelimiter: () => false,
      backwardDelimiter: () => false,
    });

    try {
      target.focus();
      handlers.action?.("trigger");
      await waitForMicrotasks();
      await waitForMicrotasks();

      target.addEventListener("keydown", (event) => {
        handlers.key?.(event);
      });
      target.dispatchEvent(new KeyboardEvent("keydown", {
        key: "]",
        code: "BracketRight",
        metaKey: true,
        shiftKey: true,
        bubbles: true,
        cancelable: true,
      }));
      target.dispatchEvent(new KeyboardEvent("keydown", {
        key: "B",
        bubbles: true,
        cancelable: true,
      }));

      expect(editor.insertions).toEqual(["AlphaB"]);
      expect(editor.markdown).toBe("prefixAlphaB");
    } finally {
      cleanup();
      restoreApi();
      host.remove();
    }
  });

  test("document eligibility uses the active cursor tail instead of markdown line tail", async () => {
    const host = document.createElement("div");
    const target = document.createElement("button");
    host.appendChild(target);
    document.body.appendChild(host);

    const editor = new FakeEditor("prefix suffix");
    editor.selection = { from: "prefix".length, to: "prefix".length };
    editor.cursorAfter = "";
    const handlers: {
      action?: (action: string) => void;
    } = {};
    let inlineRequests = 0;

    const restoreApi = installNativeCopilot(async (action) => {
      if (action === "inline") {
        inlineRequests += 1;
        return {
          items: [{
            insertText: "Suffix",
            range: { from: editor.selection.to, to: editor.selection.to },
            item: { insertText: "Suffix" },
          }],
        };
      }
      return { ok: true };
    });

    const cleanup = setupCopilot({
      editor,
      host,
      currentFile: () => "/tmp/document.md",
      vimMode: () => "insert",
      setStatus: () => {},
      onChange: () => () => {},
      onKeyDown: () => () => {},
      onAction: (handler: (action: string) => void) => {
        handlers.action = handler;
        return () => {
          delete handlers.action;
        };
      },
      onSettingsChange: () => () => {},
      getSettings: () => ({ idleDelayMs: 999_999, largeBufferThresholdKb: 512 }),
      onDocumentEvent: () => () => {},
      jumpSnippetNext: () => false,
      jumpSnippetPrevious: () => false,
      forwardDelimiter: () => false,
      backwardDelimiter: () => false,
    });

    try {
      target.focus();
      handlers.action?.("trigger");
      await waitForMicrotasks();
      await waitForMicrotasks();
      expect(inlineRequests).toBe(1);
      expect(document.querySelector(".aaronnote-copilot-ghost")?.textContent).toBe("Suffix");
    } finally {
      cleanup();
      restoreApi();
      host.remove();
    }
  });

  test("large documents send only a cursor-local completion window", async () => {
    const host = document.createElement("div");
    const target = document.createElement("button");
    host.appendChild(target);
    document.body.appendChild(host);

    const markdown = `${"a".repeat(2000)}\nneedle`;
    const editor = new FakeEditor(markdown);
    const handlers: {
      action?: (action: string) => void;
    } = {};
    let inlineBody: { content: string; offset: number; window?: { from: number; to: number } } | null = null;

    const restoreApi = installNativeCopilot(async (action, body) => {
      if (action === "inline") {
        inlineBody = body as { content: string; offset: number; window?: { from: number; to: number } };
        return {
          items: [{
            insertText: "needleSuffix",
            range: { from: inlineBody!.offset - "needle".length, to: inlineBody!.offset },
            item: { insertText: "needleSuffix" },
          }],
        };
      }
      return { ok: true };
    });

    const cleanup = setupCopilot({
      editor,
      host,
      currentFile: () => "/tmp/large.md",
      vimMode: () => "insert",
      setStatus: () => {},
      onChange: () => () => {},
      onKeyDown: () => () => {},
      onAction: (handler: (action: string) => void) => {
        handlers.action = handler;
        return () => {
          delete handlers.action;
        };
      },
      onSettingsChange: () => () => {},
      getSettings: () => ({ idleDelayMs: 999_999, largeBufferThresholdKb: 1 }),
      onDocumentEvent: () => () => {},
      jumpSnippetNext: () => false,
      jumpSnippetPrevious: () => false,
      forwardDelimiter: () => false,
      backwardDelimiter: () => false,
    });

    try {
      target.focus();
      handlers.action?.("trigger");
      await waitForMicrotasks();
      await waitForMicrotasks();

      expect(inlineBody).not.toBeNull();
      expect(inlineBody!.content.length).toBeLessThanOrEqual(1024);
      expect(inlineBody!.content).toContain("needle");
      expect(inlineBody!.offset).toBe(inlineBody!.content.indexOf("needle") + "needle".length);
      expect(inlineBody!.window?.from).toBeGreaterThan(0);
      expect(document.querySelector(".aaronnote-copilot-ghost")?.textContent).toBe("Suffix");
    } finally {
      cleanup();
      restoreApi();
      host.remove();
    }
  });

});
