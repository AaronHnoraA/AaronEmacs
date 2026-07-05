import { EditorState } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";

import {
  createAaronnoteMarkdownExtensions,
  isAaronnoteMarkdownSource,
  toggleAaronnoteMarkdownSource,
} from "../src/cm6/editor-cm6.ts";
import { isMarkdownMimeType } from "../jupyter/labextension/src/embedded-markdown-core.ts";
import { replaceMarkdownCellFactory } from "../jupyter/labextension/src/native-cell-factory-core.ts";

afterEach(() => {
  document.body.replaceChildren();
});

describe("Aaronnote embedded CM6 extensions", () => {
  test("run in the host EditorView and toggle source without creating another editor", () => {
    const host = document.createElement("div");
    document.body.append(host);
    const view = new EditorView({
      state: EditorState.create({
        doc: "# Title\n\n**bold**",
        extensions: [createAaronnoteMarkdownExtensions()],
      }),
      parent: host,
    });

    try {
      expect(host.querySelectorAll(".cm-editor")).toHaveLength(1);
      expect(isAaronnoteMarkdownSource(view)).toBe(false);
      expect(toggleAaronnoteMarkdownSource(view)).toBe(true);
      expect(isAaronnoteMarkdownSource(view)).toBe(true);
      view.dispatch({ changes: { from: view.state.doc.length, insert: "!" } });
      expect(view.state.doc.toString()).toBe("# Title\n\n**bold**!");
    } finally {
      view.destroy();
    }
  });

  test("recognizes Jupyter Markdown MIME variants only", () => {
    expect(isMarkdownMimeType("text/markdown")).toBe(true);
    expect(isMarkdownMimeType("text/x-ipythongfm")).toBe(true);
    expect(isMarkdownMimeType("text/x-python")).toBe(false);
  });
});

describe("Aaronnote native cell factory core", () => {
  test("replaces the Markdown constructor exactly once", () => {
    const original = { kind: "jupyter" };
    const native = { kind: "aaronnote" };
    const factory = {
      createMarkdownCell: () => original,
    };

    expect(replaceMarkdownCellFactory(factory, () => native)).toBe(true);
    expect(factory.createMarkdownCell()).toBe(native);
    expect(replaceMarkdownCellFactory(factory, () => original)).toBe(false);
    expect(factory.createMarkdownCell()).toBe(native);
  });
});
