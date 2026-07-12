import { EditorState } from "@codemirror/state";
import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import { slideRangesFromState } from "../aaronnote/slide-deck.ts";
import { tocIndexExtension } from "../src/cm6/toc-index.ts";

describe("slides deck ranges", () => {
  test("maps H1 to horizontal pages and H2 to vertical pages", () => {
    const doc = [
      "#+begin meta",
      "kind: slides",
      "#+end meta",
      "",
      "# First",
      "intro",
      "## Detail",
      "",
      "# Second",
      "tail",
    ].join("\n");
    const state = EditorState.create({ doc, extensions: [tocIndexExtension] });
    const slides = slideRangesFromState(state);

    expect(slides.map((slide) => ({ title: slide.title, parentTitle: slide.parentTitle, vertical: slide.vertical, from: doc.slice(slide.from, slide.from + 2), body: doc.slice(slide.from, slide.to) }))).toEqual([
      expect.objectContaining({ title: "First", parentTitle: "First", vertical: false, from: "# ", body: "# First\nintro\n" }),
      expect.objectContaining({ title: "Detail", parentTitle: "First", vertical: true, from: "##", body: "## Detail\n\n" }),
      expect.objectContaining({ title: "Second", parentTitle: "Second", vertical: false, from: "# ", body: "# Second\ntail" }),
    ]);
  });

  test("does not turn fenced heading-looking source into a slide", () => {
    const state = EditorState.create({
      doc: "# Real\n```md\n# Not a slide\n```\n# Last",
      extensions: [tocIndexExtension],
    });
    expect(slideRangesFromState(state).map((slide) => slide.title)).toEqual(["Real", "Last"]);
  });

  test("keeps TOC-omitted H2 inside its current slide", () => {
    const doc = [
      "# Parent",
      "intro",
      "## Local detail <!-- omit in toc -->",
      "same page",
      "## Vertical",
      "below",
    ].join("\n");
    const state = EditorState.create({ doc, extensions: [tocIndexExtension] });
    const slides = slideRangesFromState(state);

    expect(slides).toHaveLength(2);
    expect(doc.slice(slides[0]!.from, slides[0]!.to)).toContain("## Local detail");
    expect(slides[1]).toEqual(expect.objectContaining({ title: "Vertical", parentTitle: "Parent", vertical: true }));
  });
});
