import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import {
  buildLatexExportScopes,
  latexExportScopeContent,
  latexHeadingRange,
} from "../aaronnote/latex-export-scope.ts";

describe("LaTeX export scopes", () => {
  const markdown = [
    "Intro.",
    "",
    "# Alpha",
    "A.",
    "",
    "## Nested",
    "N.",
    "",
    "# Hidden boundary",
    "H.",
    "",
    "# Omega",
    "O.",
    "",
  ].join("\n");
  const marker = (text: string) => markdown.indexOf(text);
  const headings = [
    { level: 1, text: "Alpha", pos: marker("Alpha"), markerFrom: marker("# Alpha") },
    { level: 2, text: "Nested", pos: marker("Nested"), markerFrom: marker("## Nested") },
    { level: 1, text: "Hidden boundary", pos: marker("Hidden boundary"), markerFrom: marker("# Hidden boundary"), omit: true },
    { level: 1, text: "Omega", pos: marker("Omega"), markerFrom: marker("# Omega") },
  ];

  test("uses omitted headings as subtree boundaries without offering them", () => {
    const range = latexHeadingRange(markdown, headings, 0);
    expect(markdown.slice(range.from, range.to)).toContain("## Nested");
    expect(markdown.slice(range.from, range.to)).not.toContain("# Hidden boundary");

    const scopes = buildLatexExportScopes({ markdown, headings, cursor: marker("N.") });
    expect(scopes.map((scope) => scope.title)).toEqual(["Whole note", "Alpha", "Nested", "Omega"]);
    expect(scopes.find((scope) => scope.active)?.title).toBe("Nested");
  });

  test("offers an explicit text-selection scope and extracts exact content", () => {
    const from = marker("A.");
    const to = from + 2;
    const scopes = buildLatexExportScopes({
      markdown,
      headings,
      selection: { from, to },
      cursor: from,
    });
    const selection = scopes.find((scope) => scope.kind === "selection")!;
    expect(selection.title).toBe("Text selection");
    expect(latexExportScopeContent(markdown, selection)).toBe("A.");
  });
});
