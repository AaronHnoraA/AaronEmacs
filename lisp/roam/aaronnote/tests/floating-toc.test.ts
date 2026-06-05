import { EditorState, Text } from "@codemirror/state";
import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import { inlineTagAnchorsFromText, markdownHeadingsFromText } from "../aaronnote/floating-toc.ts";
import { tocIndexExtension, tocIndexFromState } from "../src/cm6/toc-index.ts";

describe("floating toc heading scan", () => {
  test("scans headings from CM6 Text without materializing markdown", () => {
    const doc = Text.of([
      "# Alpha",
      "body",
      "  ## Beta ###",
      "####Nope",
      "### Gamma",
    ]);

    expect(markdownHeadingsFromText(doc).map((heading) => ({
      level: heading.level,
      text: heading.text,
      pos: heading.pos,
    }))).toEqual([
      { level: 1, text: "Alpha", pos: 2 },
      { level: 2, text: "Beta", pos: 18 },
      { level: 3, text: "Gamma", pos: 40 },
    ]);
  });

  test("scans inline tag anchors without treating code as anchors", () => {
    const doc = Text.of([
      "# Alpha",
      "body @@tag[alpha]",
      "multi @@tag[first] and @@tag[second]",
      "`@@tag[code]`",
      "```",
      "@@tag[fenced]",
      "```",
      "tail @@tag[tail]",
    ]);

    expect(inlineTagAnchorsFromText(doc).map((anchor) => anchor.tag)).toEqual(["alpha", "first", "second", "tail"]);
  });

  test("does not treat fenced markdown-looking lines as headings", () => {
    const doc = Text.of([
      "# Alpha",
      "```",
      "# Example",
      "```",
      "## Beta",
    ]);

    expect(markdownHeadingsFromText(doc).map((heading) => heading.text)).toEqual(["Alpha", "Beta"]);
  });

  test("updates toc index around changed lines", () => {
    let state = EditorState.create({
      doc: "# Alpha\nbody @@tag[alpha]\n## Beta",
      extensions: [tocIndexExtension],
    });

    state = state.update({
      changes: { from: 0, to: "# Alpha".length, insert: "# Renamed" },
    }).state;
    state = state.update({
      changes: { from: state.doc.length, insert: "\nbody @@tag[tail]" },
    }).state;

    const index = tocIndexFromState(state);
    expect(index.headings.map((heading) => heading.text)).toEqual(["Renamed", "Beta"]);
    expect(index.anchors.map((anchor) => anchor.tag)).toEqual(["alpha", "tail"]);
  });

  test("semantic part and sections outrank markdown headings", () => {
    const doc = Text.of([
      "@@part [Foundations]",
      "@@section [Linear algebra]",
      "@@section(sub) [Inner products]{id: inner-products}",
      "# Markdown detail",
    ]);

    expect(markdownHeadingsFromText(doc)).toEqual([
      expect.objectContaining({ level: 1, text: "Foundations", source: "semantic" }),
      expect.objectContaining({ level: 2, text: "Linear algebra", source: "semantic" }),
      expect.objectContaining({ level: 3, text: "Inner products", slug: "inner-products", source: "semantic" }),
      expect.objectContaining({ level: 6, renderLevel: 1, text: "Markdown detail", source: "markdown" }),
    ]);
  });

  test("toc index falls back correctly when fence structure appears", () => {
    let state = EditorState.create({
      doc: "# Alpha\n@@tag[alpha]\n",
      extensions: [tocIndexExtension],
    });

    state = state.update({
      changes: { from: state.doc.length, insert: "```\n@@tag[code]\n```\n@@tag[tail]" },
    }).state;

    const index = tocIndexFromState(state);
    expect(index.anchors.map((anchor) => anchor.tag)).toEqual(["alpha", "tail"]);
  });

  test("toc index keeps fenced tag text out during body edits", () => {
    let state = EditorState.create({
      doc: "# Alpha\n@@tag[alpha]\n```\n# Example\n@@tag[code]\n```\n@@tag[tail]",
      extensions: [tocIndexExtension],
    });

    const codeLine = state.doc.line(5);
    state = state.update({
      changes: { from: codeLine.to, insert: " edited" },
    }).state;

    const index = tocIndexFromState(state);
    expect(index.headings.map((heading) => heading.text)).toEqual(["Alpha"]);
    expect(index.anchors.map((anchor) => anchor.tag)).toEqual(["alpha", "tail"]);
  });
});
