import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { parseCspellDiagnostics, parseValeDiagnostics } from "../server/lib/prose-check.mjs";

describe("prose check diagnostic mapping", () => {
  test("Vale diagnostics preserve replacements and the matched word", () => {
    const source = "This is teh text.";
    const output = JSON.stringify({
      "note.md": [{
        Line: 1,
        Span: [9, 11],
        Match: "teh",
        Message: "Possible typo",
        Check: "Vale.Spelling",
        Severity: "warning",
        Action: { Name: "replace", Params: ["the"] },
      }],
    });
    expect(parseValeDiagnostics(output, source)).toEqual([expect.objectContaining({
      from: 8,
      to: 11,
      word: "teh",
      suggestions: ["the"],
    })]);
  });

  test("CSpell diagnostics expose bounded source offsets", () => {
    const source = "alpha mispelled omega";
    const separator = "\u001f";
    const output = `1${separator}7${separator}mispelled${separator}Unknown word${separator}misspelled`;
    expect(parseCspellDiagnostics(output, source)).toEqual([expect.objectContaining({
      from: 6,
      to: 15,
      word: "mispelled",
      suggestions: ["misspelled"],
    })]);
  });
});
