/**
 * Phase 5 — CM6 command dispatch tests.
 *
 * All tests are skipped until CM6 deps are installed (CM6_READY = false).
 * After `npm install @codemirror/{state,view,language,commands,lang-markdown}`:
 *   1. Set CM6_READY = true
 *   2. In editor-api.ts, activate the cm6 dispatch (remove the throw stub)
 */

import { describe, it, expect } from "@voidzero-dev/vite-plus-test";

const CM6_READY = true;
const maybeDescribe = CM6_READY ? describe : describe.skip;

// ---------------------------------------------------------------------------
// The tests below are structural stubs — they will run once CM6_READY = true
// ---------------------------------------------------------------------------

maybeDescribe("CM6 runCommand — inline marks", () => {
  it("bold: wraps empty selection with ** delimiters", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "hello" });
    ed.setSelection(2, 2);
    ed.runCommand("bold");
    expect(ed.getMarkdown()).toBe("he****llo");
    expect(ed.getMarkdownSelection()).toEqual({ from: 4, to: 4 });
    ed.destroy();
  });

  it("bold: wraps non-empty selection", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "hello world" });
    ed.setSelection(0, 5);
    ed.runCommand("bold");
    expect(ed.getMarkdown()).toBe("**hello** world");
    expect(ed.getMarkdownSelection()).toEqual({ from: 2, to: 7 });
    ed.destroy();
  });

  it("italic: inserts * delimiters at cursor", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "" });
    ed.runCommand("italic");
    expect(ed.getMarkdown()).toBe("**");
    ed.destroy();
  });

  it("code: wraps selection with backticks", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "foo" });
    ed.setSelection(0, 3);
    ed.runCommand("code");
    expect(ed.getMarkdown()).toBe("`foo`");
    ed.destroy();
  });

  it("highlight and strike wrap selected text", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "hello world" });
    ed.setSelection(0, 5);
    ed.runCommand("highlight");
    expect(ed.getMarkdown()).toBe("==hello== world");
    expect(ed.getMarkdownSelection()).toEqual({ from: 2, to: 7 });
    ed.setSelection(ed.getMarkdown().indexOf("world"), ed.getMarkdown().length);
    ed.runCommand("strike");
    expect(ed.getMarkdown()).toBe("==hello== ~~world~~");
    ed.destroy();
  });

  it("highlight and strike place the cursor between empty delimiters", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "" });
    ed.runCommand("highlight");
    expect(ed.getMarkdown()).toBe("====");
    expect(ed.getMarkdownSelection()).toEqual({ from: 2, to: 2 });
    ed.runCommand("strike");
    expect(ed.getMarkdown()).toBe("==~~~~==");
    expect(ed.getMarkdownSelection()).toEqual({ from: 4, to: 4 });
    ed.destroy();
  });

  it("link: inserts link with default href when no selection", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "" });
    ed.runCommand("link");
    expect(ed.getMarkdown()).toBe("[link](https://)");
    ed.destroy();
  });

  it("link: wraps selection as link text", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "click here" });
    ed.setSelection(0, 10);
    ed.runCommand("link", "https://example.com");
    expect(ed.getMarkdown()).toBe("[click here](https://example.com)");
    ed.destroy();
  });
});

maybeDescribe("CM6 runCommand — heading / line prefix", () => {
  it("heading-1: prefixes current line", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "My title" });
    ed.setSelection(3, 3);
    ed.runCommand("heading-1");
    expect(ed.getMarkdown()).toBe("# My title");
    ed.destroy();
  });

  it("heading-2: replaces existing heading prefix", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "# My title" });
    ed.setSelection(5, 5);
    ed.runCommand("heading-2");
    expect(ed.getMarkdown()).toBe("## My title");
    ed.destroy();
  });

  it("blockquote: prefixes line with > ", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "note" });
    ed.runCommand("blockquote");
    expect(ed.getMarkdown()).toBe("> note");
    ed.destroy();
  });

  it("bullet-list: converts plain line to list item", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "item" });
    ed.runCommand("bullet-list");
    expect(ed.getMarkdown()).toBe("- item");
    ed.destroy();
  });

  it("task-list: converts bullet to task item", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "- item" });
    ed.runCommand("task-list");
    expect(ed.getMarkdown()).toBe("- [ ] item");
    ed.destroy();
  });
});

maybeDescribe("CM6 markdown typing affordances", () => {
  it("wraps selected markdown when typing paired punctuation", async () => {
    const { createEditorCM6, wrapSelectedMarkdownInput } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "hello" });
    ed.setSelection(0, 5);

    expect(wrapSelectedMarkdownInput(ed.view, 0, 5, "(")).toBe(true);
    expect(ed.getMarkdown()).toBe("(hello)");
    expect(ed.getMarkdownSelection()).toEqual({ from: 1, to: 6 });
    ed.destroy();
  });

  it("keeps empty cursor punctuation on the default input path", async () => {
    const { createEditorCM6, wrapSelectedMarkdownInput } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "hello" });
    ed.setSelection(2, 2);

    expect(wrapSelectedMarkdownInput(ed.view, 2, 2, "(")).toBe(false);
    expect(ed.getMarkdown()).toBe("hello");
    ed.destroy();
  });

  it("Enter exits an empty markdown list item", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const { exitEmptyMarkdownBlock } = await import("../../src/cm6/commands.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "- " });
    ed.setSelection(2, 2);

    expect(exitEmptyMarkdownBlock(ed.view)).toBe(true);
    expect(ed.getMarkdown()).toBe("");
    expect(ed.getMarkdownSelection()).toEqual({ from: 0, to: 0 });
    ed.destroy();
  });

  it("Tab and Shift-Tab indent selected markdown list items", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const { indentMarkdownBlock, indentMarkdownList } = await import("../../src/cm6/commands.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "- one\n- two\nplain" });
    ed.setSelection(0, "- one\n- two".length);

    expect(indentMarkdownList(ed.view, 1)).toBe(true);
    expect(ed.getMarkdown()).toBe("  - one\n  - two\nplain");
    expect(indentMarkdownList(ed.view, -1)).toBe(true);
    expect(ed.getMarkdown()).toBe("- one\n- two\nplain");
    ed.setSelection(ed.getMarkdown().length);
    expect(indentMarkdownBlock(ed.view, -1)).toBe(true);
    expect(ed.getMarkdown()).toBe("- one\n- two\nplain");
    ed.destroy();
  });
});

maybeDescribe("CM6 runCommand — block insert", () => {
  it("code-block: inserts fenced code block after current line", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "intro" });
    ed.setSelection(5, 5);
    ed.runCommand("code-block", "ts");
    expect(ed.getMarkdown()).toBe("intro\n```ts\n\n```");
    ed.destroy();
  });

  it("insert-math-block: inserts $$...$$ block", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "" });
    ed.runCommand("insert-math-block");
    expect(ed.getMarkdown()).toBe("$$\n\n$$");
    ed.destroy();
  });

  it("insert-org-env: inserts #+begin/end block", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "" });
    ed.runCommand("insert-org-env", "theorem");
    expect(ed.getMarkdown()).toBe("#+begin theorem\n\n#+end theorem");
    ed.destroy();
  });
});

maybeDescribe("CM6 runCommand — table editing", () => {
  it("table commands insert/delete rows and columns as markdown", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, {
      initialContent: "| A | B |\n| --- | --- |\n| 1 | 2 |",
    });
    ed.setSelection(ed.getMarkdown().indexOf("1"));

    expect(ed.runCommand("table-insert-row")).toBe(true);
    expect(ed.getMarkdown()).toBe("| A | B |\n| --- | --- |\n| 1 | 2 |\n|   |   |");

    expect(ed.runCommand("table-insert-column")).toBe(true);
    expect(ed.getMarkdown()).toBe("| A |   | B |\n| --- | --- | --- |\n| 1 |   | 2 |\n|   |   |   |");

    expect(ed.runCommand("table-delete-column")).toBe(true);
    expect(ed.getMarkdown()).toBe("| A | B |\n| --- | --- |\n| 1 | 2 |\n|   |   |");

    const emptyRow = ed.getMarkdown().lastIndexOf("|   |   |");
    expect(emptyRow).toBeGreaterThanOrEqual(0);
    ed.setSelection(emptyRow + 2);
    expect(ed.runCommand("table-delete-row")).toBe(true);
    expect(ed.getMarkdown()).toBe("| A | B |\n| --- | --- |\n| 1 | 2 |");
    ed.destroy();
  });
});

maybeDescribe("CM6 getBlockContext", () => {
  it("returns paragraph type in plain text", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "Hello world" });
    ed.setSelection(3, 3);
    const ctx = ed.getBlockContext();
    expect(ctx.type).toBe("paragraph");
    expect(ctx.sourceMode).toBe(false);
    ed.destroy();
  });

  it("returns code_block type inside fenced code", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "```ts\nconst x = 1;\n```" });
    ed.setSelection(10, 10);
    const ctx = ed.getBlockContext();
    expect(ctx.type).toBe("code_block");
    expect(ctx.text.trim()).toBe("const x = 1;");
    ed.destroy();
  });

  it("returns table_cell type inside a GFM table", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, {
      initialContent: "| A | B |\n| --- | --- |\n| 1 | 2 |",
    });
    ed.setSelection(ed.getMarkdown().indexOf("1"));
    const ctx = ed.getBlockContext();
    expect(ctx.type).toBe("table_cell");
    expect(ctx.commands).toEqual([
      "table-insert-row",
      "table-insert-column",
      "table-delete-row",
      "table-delete-column",
    ]);
    ed.destroy();
  });
});

maybeDescribe("CM6 quick insert", () => {
  it("getQuickInsertItems: returns built-in items", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "" });
    const items = ed.getQuickInsertItems();
    expect(items.length).toBeGreaterThan(0);
    ed.destroy();
  });

  it("getQuickInsertItems: filters by query", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "" });
    const items = ed.getQuickInsertItems("math");
    expect(items.every((i) => JSON.stringify(i).toLowerCase().includes("math"))).toBe(true);
    ed.destroy();
  });

  it("registerQuickInsertProvider: custom provider items appear", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "" });
    const unregister = ed.registerQuickInsertProvider(() => [
      { id: "custom-item", label: "Custom", markdown: "custom text" },
    ]);
    const items = ed.getQuickInsertItems("");
    expect(items.some((i) => i.id === "custom-item")).toBe(true);
    unregister();
    const items2 = ed.getQuickInsertItems("");
    expect(items2.some((i) => i.id === "custom-item")).toBe(false);
    ed.destroy();
  });

  it("runQuickInsert: markdown item inserts text", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "" });
    ed.runQuickInsert({ id: "x", label: "x", markdown: "## Section" });
    expect(ed.getMarkdown()).toBe("## Section");
    ed.destroy();
  });

  it("runQuickInsert: command item executes command", async () => {
    const { createEditorCM6 } = await import("../../src/cm6/editor-cm6.ts");
    const host = document.createElement("div");
    const ed = createEditorCM6(host, { initialContent: "text" });
    ed.setSelection(0, 4);
    ed.runQuickInsert({ id: "b", label: "bold", command: "bold" });
    expect(ed.getMarkdown()).toBe("**text**");
    ed.destroy();
  });
});
