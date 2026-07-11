import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { parseLanguageToolDiagnostics } from "../server/lib/prose-check.mjs";

describe("prose check diagnostic mapping", () => {
  test("LanguageTool diagnostics preserve replacements and source offsets", () => {
    const source = "This are a bad sentence.";
    const output = JSON.stringify({
      matches: [{
        offset: 0,
        length: 8,
        message: "The verb 'are' is plural.",
        replacements: [{ value: "This is" }],
        rule: {
          id: "PLURAL_VERB_AFTER_THIS",
          issueType: "grammar",
          category: { id: "GRAMMAR" },
        },
      }],
    });
    expect(parseLanguageToolDiagnostics(output, source)).toEqual([expect.objectContaining({
      from: 0,
      to: 8,
      word: "This are",
      suggestions: ["This is"],
    })]);
  });
});
