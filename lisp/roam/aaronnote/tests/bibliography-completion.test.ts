import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import {
  citeKeyCompletionContext,
  citeKeyRenderPrefix,
  citeNamespaceCompletionPrefix,
  citeNamespaceRenderPrefix,
} from "../aaronnote/bibliography-completion.ts";

describe("bibliography completion context", () => {
  test("recognizes namespace completion from a bounded cursor tail", () => {
    const before = `${"long paragraph ".repeat(100)}@@cite(project/UNSW`;
    const bounded = before.slice(-320);
    expect(citeNamespaceCompletionPrefix(bounded)).toBe("project/UNSW");
  });

  test("recognizes the current key after semicolon-separated keys", () => {
    expect(citeKeyCompletionContext("text @@cite(iso) [Str87; Iv")).toEqual({
      namespace: "iso",
      prefix: "Iv",
    });
  });

  test("does not treat a completed citation as an active completion", () => {
    expect(citeNamespaceCompletionPrefix("@@cite(iso) [Str87]")).toBeNull();
    expect(citeKeyCompletionContext("@@cite(iso) [Str87]")).toBeNull();
  });

  test("uses non-empty popup identities for empty namespace and key prefixes", () => {
    expect(citeNamespaceRenderPrefix("")).toBe("@@cite(");
    expect(citeKeyRenderPrefix({ namespace: "iso", prefix: "" })).toBe("@@cite(iso) [");
  });
});
