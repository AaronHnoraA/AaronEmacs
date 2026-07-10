import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";

import { createEditor } from "../src/editor-api.ts";
import { refreshViewportDecorationsNow } from "../src/cm6/viewport-refresh.ts";

afterEach(() => {
  delete window.AaronnoteBibliography;
});

describe("inline citation widget", () => {
  test("re-renders immediately when the server-backed bibliography model changes", () => {
    const citation = "@@cite(iso) [Str87] {locator: p. 406}";
    const markdown = `Text ${citation}.\n\nNext paragraph.`;
    let version = 0;
    let label = "[?]";
    let observedRange = { from: -1, to: -1 };
    const opened: Array<{ from: number; to: number; jump: boolean }> = [];
    window.AaronnoteBibliography = {
      version: () => version,
      citationLabel: (from, to) => {
        observedRange = { from, to };
        return { label, error: label === "[?]" };
      },
      openCitation: (from, to, _rect, jump) => opened.push({ from, to, jump }),
    };

    const host = document.createElement("div");
    document.body.appendChild(host);
    const editor = createEditor(host, { kernel: "cm6", initialContent: markdown });
    try {
      editor.setMarkdownSelection(markdown.length);
      expect(host.querySelector<HTMLElement>(".inline-cite-widget")?.textContent).toBe("[?]");
      expect(markdown.slice(observedRange.from, observedRange.to)).toBe(citation);

      label = "[1, p. 406]";
      version += 1;
      refreshViewportDecorationsNow(editor.view);

      expect(host.querySelector<HTMLElement>(".inline-cite-widget")?.textContent).toBe("[1, p. 406]");
      expect(host.querySelector(".inline-cite-widget")?.classList.contains("is-error")).toBe(false);

      const widget = host.querySelector<HTMLElement>(".inline-cite-widget")!;
      expect(widget.dataset.cmOpenSource).toBe("true");
      widget.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, button: 0 }));
      widget.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, button: 0, metaKey: true }));

      expect(opened).toEqual([
        { ...observedRange, jump: true },
      ]);
      expect(editor.getMarkdownSelection()).toEqual({ from: markdown.length, to: markdown.length });
      expect(host.querySelector(".inline-cite-widget")).toBeTruthy();
    } finally {
      editor.destroy();
      host.remove();
    }
  });

  test("plain left click reveals citation source without invoking bibliography actions", () => {
    const citation = "@@cite(iso) [Str87] {locator: p. 406}";
    const markdown = `Before ${citation} after`;
    const citationFrom = markdown.indexOf(citation);
    const citationTo = citationFrom + citation.length;
    let opened = 0;
    window.AaronnoteBibliography = {
      version: () => 1,
      citationLabel: () => ({ label: "[1, p. 406]" }),
      openCitation: () => { opened += 1; },
    };

    const host = document.createElement("div");
    document.body.appendChild(host);
    const editor = createEditor(host, { kernel: "cm6", initialContent: markdown });
    try {
      editor.setMarkdownSelection(markdown.length);
      const widget = host.querySelector<HTMLElement>(".inline-cite-widget")!;
      widget.dispatchEvent(new MouseEvent("mousedown", {
        bubbles: true,
        cancelable: true,
        button: 0,
        clientX: 1,
        clientY: 1,
      }));

      const selection = editor.getMarkdownSelection();
      expect(selection.from).toBe(selection.to);
      expect(selection.from).toBeGreaterThanOrEqual(citationFrom);
      expect(selection.from).toBeLessThanOrEqual(citationTo);
      expect(opened).toBe(0);
    } finally {
      editor.destroy();
      host.remove();
    }
  });
});
