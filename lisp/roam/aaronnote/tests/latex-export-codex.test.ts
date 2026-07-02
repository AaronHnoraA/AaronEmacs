import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdtemp, mkdir, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";

// @ts-ignore Node ESM module outside the TS app graph.
import { codexAvailable, loadAgentRules, proseFidelityWarnings } from "../server/lib/latex-export-codex.mjs";

const roots: string[] = [];
afterEach(async () => {
  await Promise.all(roots.splice(0).map((root) => rm(root, { recursive: true, force: true })));
});

describe("latex-export-codex helpers", () => {
  test("codexAvailable trusts bare names and existing files, rejects missing paths", () => {
    expect(codexAvailable("codex")).toBe(true);
    expect(codexAvailable("")).toBe(false);
    expect(codexAvailable("/definitely/not/here/codex")).toBe(false);
  });

  test("proseFidelityWarnings ignores formatting but flags dropped and added prose", () => {
    const same = proseFidelityWarnings(
      "The quick brown fox jumps over the lazy dog near the river",
      "\\textbf{The} quick \\emph{brown} fox jumps over the lazy dog near the river",
    );
    expect(same).toEqual([]);

    const drift = proseFidelityWarnings(
      "alpha beta gamma delta epsilon zeta eta theta",
      "\\section{alpha} plus many entirely unrelated inserted english words here now",
    );
    expect(drift.some((w: string) => /missing/.test(w))).toBe(true);
    expect(drift.some((w: string) => /not in the source/.test(w))).toBe(true);
  });

  test("proseFidelityWarnings ignores math and code content", () => {
    const warnings = proseFidelityWarnings(
      "See the bound \\(x \\le y\\) and the snippet `foo()` below here",
      "See the bound \\(x \\le y\\) and the snippet \\texttt{foo()} below here",
    );
    expect(warnings).toEqual([]);
  });

  test("loadAgentRules reads envMap/commentBlocks and returns null when empty", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-agent-"));
    roots.push(root);
    await mkdir(join(root, "mechanical"), { recursive: true });
    expect(await loadAgentRules(root)).toBe(null); // no file yet

    await writeFile(join(root, "mechanical", "rules.json"),
      JSON.stringify({ envMap: { claim: "theorem" }, commentBlocks: ["aside"] }), "utf8");
    const rules = await loadAgentRules(root) as { envMap: Record<string, string>; commentBlocks: string[] };
    expect(rules.envMap.claim).toBe("theorem");
    expect(rules.commentBlocks).toContain("aside");

    await writeFile(join(root, "mechanical", "rules.json"),
      JSON.stringify({ envMap: {}, commentBlocks: [] }), "utf8");
    expect(await loadAgentRules(root)).toBe(null); // empty rules -> null
  });
});
