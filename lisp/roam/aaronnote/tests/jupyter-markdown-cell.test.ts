import { EditorState } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";

import {
  installAaronnoteMarkdownCellBridge,
  type AaronnoteMarkdownCellEditor,
} from "../jupyter/labextension/src/markdown-cell-core.ts";

type FakeEditor = AaronnoteMarkdownCellEditor & {
  changes: string[];
  emit(markdown: string): void;
};

const fakeEditors: FakeEditor[] = [];

function createFakeEditor(host: HTMLElement, options: {
  initialContent?: string;
  onChange?: (markdown: string) => void;
}): FakeEditor {
  let markdown = options.initialContent ?? "";
  host.dataset.fakeAaronnote = "true";
  const editor: FakeEditor = {
    changes: [markdown],
    getMarkdown: () => markdown,
    setMarkdown: (next) => {
      markdown = next;
      editor.changes.push(next);
    },
    focus: () => undefined,
    destroy: () => {
      host.dataset.destroyed = "true";
    },
    emit: (next) => {
      markdown = next;
      editor.changes.push(next);
      options.onChange?.(next);
    },
  };
  fakeEditors.push(editor);
  return editor;
}

function markdownCell(markdown = "# Title"): {
  cell: HTMLElement;
  sourceEditor: HTMLElement;
  view: EditorView;
} {
  const cell = document.createElement("div");
  cell.className = "jp-Cell jp-MarkdownCell";
  const input = document.createElement("div");
  input.className = "jp-InputArea-editor";
  const sourceEditor = document.createElement("div");
  input.append(sourceEditor);
  cell.append(input);
  document.body.append(cell);
  const view = new EditorView({
    state: EditorState.create({ doc: markdown }),
    parent: sourceEditor,
  });
  return { cell, sourceEditor: view.dom, view };
}

afterEach(() => {
  fakeEditors.length = 0;
  document.body.replaceChildren();
});

describe("Aaronnote Jupyter Markdown cell bridge", () => {
  test("mounts an Aaronnote editor for markdown cells and hides the Jupyter source editor", () => {
    const { cell, sourceEditor } = markdownCell("# Heading\n\ntext");
    const bridge = installAaronnoteMarkdownCellBridge({
      document,
      createEditor: createFakeEditor,
      scanIntervalMs: 0,
    });

    try {
      expect(bridge.size()).toBe(1);
      expect(fakeEditors[0]?.changes).toEqual(["# Heading\n\ntext"]);
      expect(sourceEditor.classList.contains("aaronnote-jupyter-source-hidden")).toBe(true);
      expect(sourceEditor.style.display).toBe("none");
      expect(cell.querySelector(".aaronnote-jupyter-mdcell-host")).toBeTruthy();
    } finally {
      bridge.dispose();
    }
  });

  test("writes Aaronnote Markdown changes back into the Jupyter CodeMirror view", () => {
    const { view } = markdownCell("before");
    const bridge = installAaronnoteMarkdownCellBridge({
      document,
      createEditor: createFakeEditor,
      scanIntervalMs: 0,
    });

    try {
      fakeEditors[0]!.emit("after");
      expect(view.state.doc.toString()).toBe("after");
    } finally {
      bridge.dispose();
    }
  });

  test("refreshes Aaronnote when Jupyter changes the source document", () => {
    const { view } = markdownCell("one");
    const bridge = installAaronnoteMarkdownCellBridge({
      document,
      createEditor: createFakeEditor,
      scanIntervalMs: 0,
    });

    try {
      view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: "two" } });
      bridge.scan();
      expect(fakeEditors[0]?.getMarkdown()).toBe("two");
      expect(fakeEditors[0]?.changes).toEqual(["one", "two"]);
    } finally {
      bridge.dispose();
    }
  });

  test("ignores non-markdown cells", () => {
    const cell = document.createElement("div");
    cell.className = "jp-Cell jp-CodeCell";
    document.body.append(cell);
    new EditorView({
      state: EditorState.create({ doc: "print(1)" }),
      parent: cell,
    });

    const bridge = installAaronnoteMarkdownCellBridge({
      document,
      createEditor: createFakeEditor,
      scanIntervalMs: 0,
    });

    try {
      expect(bridge.size()).toBe(0);
      expect(fakeEditors).toHaveLength(0);
    } finally {
      bridge.dispose();
    }
  });

  test("dispose restores the original Jupyter editor", () => {
    const { sourceEditor } = markdownCell("text");
    const bridge = installAaronnoteMarkdownCellBridge({
      document,
      createEditor: createFakeEditor,
      scanIntervalMs: 0,
    });
    const host = document.querySelector<HTMLElement>(".aaronnote-jupyter-mdcell-host")!;

    bridge.dispose();

    expect(sourceEditor.classList.contains("aaronnote-jupyter-source-hidden")).toBe(false);
    expect(sourceEditor.style.display).toBe("");
    expect(host.isConnected).toBe(false);
    expect(host.dataset.destroyed).toBe("true");
  });
});
