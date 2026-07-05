import { EditorState } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";

import {
  installAaronnoteMarkdownDocumentBridge,
} from "../jupyter/labextension/src/markdown-document-core.ts";
import type { AaronnoteMarkdownCellEditor } from "../jupyter/labextension/src/markdown-cell-core.ts";

type FakeEditor = AaronnoteMarkdownCellEditor & { emit(markdown: string): void };
const fakeEditors: FakeEditor[] = [];

function createFakeEditor(host: HTMLElement, options: {
  initialContent?: string;
  onChange?: (markdown: string) => void;
}): FakeEditor {
  let markdown = options.initialContent ?? "";
  const editor: FakeEditor = {
    getMarkdown: () => markdown,
    setMarkdown: (next) => { markdown = next; },
    focus: () => undefined,
    destroy: () => { host.dataset.destroyed = "true"; },
    emit: (next) => {
      markdown = next;
      options.onChange?.(next);
    },
  };
  fakeEditors.push(editor);
  return editor;
}

function fileEditor(name: string, markdown = "# Title"): {
  container: HTMLElement;
  source: HTMLElement;
  view: EditorView;
} {
  const tab = document.createElement("div");
  tab.id = `tab-${Math.random()}`;
  tab.textContent = name;
  const wrapper = document.createElement("div");
  wrapper.className = "jp-MainAreaWidget jp-Document";
  wrapper.setAttribute("aria-labelledby", tab.id);
  const container = document.createElement("div");
  container.className = "jp-FileEditor";
  const source = document.createElement("div");
  container.append(source);
  wrapper.append(container);
  document.body.append(tab, wrapper);
  const view = new EditorView({
    state: EditorState.create({ doc: markdown }),
    parent: source,
  });
  return { container, source: view.dom, view };
}

afterEach(() => {
  fakeEditors.length = 0;
  document.body.replaceChildren();
});

describe("Aaronnote Jupyter Markdown document bridge", () => {
  test("mounts for Markdown documents and writes changes to Jupyter", () => {
    const { container, source, view } = fileEditor("note.md", "before");
    const sizes: number[] = [];
    const bridge = installAaronnoteMarkdownDocumentBridge({
      document,
      createEditor: createFakeEditor,
      scanIntervalMs: 0,
      onSizeChange: (size) => sizes.push(size),
    });

    try {
      expect(bridge.size()).toBe(1);
      expect(source.style.display).toBe("none");
      expect(container.querySelector(".aaronnote-jupyter-document-host")).toBeTruthy();
      expect(fakeEditors[0]?.getMarkdown()).toBe("before");
      fakeEditors[0]!.emit("after");
      expect(view.state.doc.toString()).toBe("after");
      expect(sizes.at(-1)).toBe(1);
    } finally {
      bridge.dispose();
    }
  });

  test("ignores non-Markdown file editors", () => {
    fileEditor("script.py", "print(1)");
    const bridge = installAaronnoteMarkdownDocumentBridge({
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

  test("refreshes from Jupyter and restores its editor on dispose", () => {
    const { source, view } = fileEditor("README", "one");
    const bridge = installAaronnoteMarkdownDocumentBridge({
      document,
      createEditor: createFakeEditor,
      scanIntervalMs: 0,
    });

    view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: "two" } });
    bridge.scan();
    expect(fakeEditors[0]?.getMarkdown()).toBe("two");
    bridge.dispose();
    expect(source.style.display).toBe("");
    expect(document.querySelector(".aaronnote-jupyter-document-host")).toBeNull();
  });
});
