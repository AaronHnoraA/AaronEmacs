import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import { collectBrowserSpellWords, maskAaronnoteProse } from "../shared/prose-mask.mjs";

function visible(masked: string): string {
  return masked.replace(/[^\S\n]+/g, " ");
}

describe("Aaronnote prose mask", () => {
  test("keeps ordinary prose and todo text while masking command syntax", () => {
    const md = "This recieve stays.\n@@todo(doing) [Fix teh word]{ddl: 2026-05-20}\n@@lean4 [proof-main]\n";
    const masked = maskAaronnoteProse(md);
    expect(masked.length).toBe(md.length);
    expect(masked).toContain("This recieve stays.");
    expect(masked).toContain("Fix teh word");
    expect(masked).not.toContain("@@todo");
    expect(masked).not.toContain("doing");
    expect(masked).not.toContain("ddl");
    expect(masked).not.toContain("proof-main");
  });

  test("keeps prose and comment text while masking @@comment syntax", () => {
    const md = "This stays visible.\n@@comment [Fix teh annotation]{k: v}\n";
    const masked = maskAaronnoteProse(md);
    expect(masked.length).toBe(md.length);
    expect(masked).toContain("This stays visible.");
    expect(masked).toContain("Fix teh annotation");
    expect(masked).not.toContain("@@comment");
    expect(masked).not.toContain("k: v");
  });

  test("masks math, code, and technical org env bodies", () => {
    const md = [
      "Check this prose.",
      "$teh + x$",
      "$$",
      "recieve",
      "$$",
      "```ts",
      "const teh = 1;",
      "```",
      "#+begin lean4",
      "theorem teh : True := trivial",
      "#+end lean4",
      "",
    ].join("\n");
    const masked = maskAaronnoteProse(md);
    expect(masked.length).toBe(md.length);
    expect(visible(masked)).toContain("Check this prose.");
    expect(masked).not.toContain("$teh");
    expect(masked).not.toContain("recieve");
    expect(masked).not.toContain("const");
    expect(masked).not.toContain("theorem");
  });

  test("keeps prose org env bodies but masks delimiters", () => {
    const md = "#+begin theorem Spectral\nThe recieve typo is visible.\n#+end theorem\n";
    const masked = maskAaronnoteProse(md);
    expect(masked).toContain("The recieve typo is visible.");
    expect(masked).not.toContain("#+begin");
    expect(masked).not.toContain("Spectral");
    expect(masked).not.toContain("#+end");
  });

  test("browser word collection skips accepted technical words", () => {
    const masked = maskAaronnoteProse("Aaronnote uses CodeMirror. The recieve typo remains.");
    const words = collectBrowserSpellWords(masked).map((entry) => entry.word);
    expect(words).toContain("recieve");
    expect(words).not.toContain("Aaronnote");
    expect(words).not.toContain("CodeMirror");
  });
});
