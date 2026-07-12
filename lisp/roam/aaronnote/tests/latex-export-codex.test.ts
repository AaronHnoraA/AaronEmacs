import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { chmod, mkdtemp, mkdir, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";

// @ts-ignore Node ESM module outside the TS app graph.
import { buildPolishCandidates, codexAvailable, loadAgentRules, normalizeAgentTitle, polishBodyWithAgent, proseFidelityWarnings, strictFidelityIssues } from "../server/lib/latex-export-codex.mjs";

const roots: string[] = [];
afterEach(async () => {
  await Promise.all(roots.splice(0).map((root) => rm(root, { recursive: true, force: true })));
});

describe("latex-export-codex helpers", () => {
  test("normalizes generated titles to the title-area budget", () => {
    expect(normalizeAgentTitle('Title: "Linear Algebra Projectors"')).toBe("Linear Algebra Projectors");
    const title = normalizeAgentTitle("A Very Long and Needlessly Detailed Assignment Title About Idempotent Linear Transformations and Their Diagonal Matrices");
    expect([...title].length).toBeLessThanOrEqual(42);
    expect(title.endsWith(" ")).toBe(false);
  });
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

  test("strict fidelity gate rejects reordered prose and changed protected payloads", () => {
    expect(strictFidelityIssues("Alpha beta gamma.", "Gamma alpha beta.")).toContain("visible prose tokens changed or were reordered");
    expect(strictFidelityIssues("Value \\(x+1\\).", "Value \\(x+2\\)."))
      .toContain("math payloads changed or were reordered");
    expect(strictFidelityIssues("Answer A.", "Answer B!")).toContain("visible prose tokens changed or were reordered");
    expect(strictFidelityIssues("Value 1.", "Value 2.")).toContain("visible prose tokens changed or were reordered");
    expect(strictFidelityIssues("Code \\texttt{x+y}.", "Code \\texttt{x-y}."))
      .toContain("code payloads changed or were reordered");
    expect(strictFidelityIssues("\\includegraphics[width=2cm,alt={A}]{x.png}", "\\includegraphics[width=3cm,alt={B}]{x.png}"))
      .toContain("resources payloads changed or were reordered");
    expect(strictFidelityIssues("\\includegraphics[width=2cm,alt={A}]{x.png}", "\\includegraphics[width=3cm,alt={A}]{x.png}"))
      .toEqual([]);
    expect(strictFidelityIssues("Alpha.", "Alpha.\\copyright"))
      .toContain("visible prose tokens changed or were reordered");
    expect(strictFidelityIssues("\\section{Alpha}", "\\subsection{Alpha}"))
      .toContain("document structure changed or was reordered");
    expect(strictFidelityIssues("\\section{Title}", "\\section*{Title}"))
      .toContain("document structure changed or was reordered");
    expect(strictFidelityIssues("A \\footnote{note}", "A note"))
      .toContain("document structure changed or was reordered");
    expect(strictFidelityIssues(
      "\\begin{longtable}{ll}\nA & B \\\\\n\\end{longtable}",
      "\\begin{longtable}{ll}\nA B \\\\\n\\end{longtable}",
    )).toContain("document structure changed or was reordered");
    expect(strictFidelityIssues("\\begin{itemize}\n\\item Alpha\n\\end{itemize}", "\\begin{enumerate}\n\\item Alpha\n\\end{enumerate}"))
      .not.toEqual([]);
    expect(strictFidelityIssues(
      "\\begin{enumerate}\n\\def\\labelenumi{(\\alph{enumi})}\n\\item Alpha\n\\end{enumerate}",
      "\\begin{enumerate}\n\\def\\labelenumi{(\\roman{enumi})}\n\\item Alpha\n\\end{enumerate}",
    )).not.toEqual([]);
    expect(strictFidelityIssues(
      "(a) Alpha\n(b) Beta",
      "\\begin{enumerate}\n\\def\\labelenumi{(\\alph{enumi})}\n\\item Alpha\n\\item Beta\n\\end{enumerate}",
    )).toEqual([]);
    expect(strictFidelityIssues("\\textbf{Alpha beta}.", "\\emph{Alpha beta}.")).toEqual([]);
  });

  test("does not reuse a title from an Agent attempt rejected by the gates", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-rejected-title-"));
    roots.push(root);
    const workdir = join(root, "work");
    await mkdir(workdir, { recursive: true });
    const agent = join(root, "fake-agent.sh");
    await writeFile(agent, [
      "#!/bin/sh",
      "printf 'Answer B!\\n' > body.tex",
      "printf 'Untrusted Rejected Title\\n' > title.txt",
      "printf '%s\\n' '{\"decisions\":[{\"id\":\"whole-document-structure\",\"action\":\"kept\",\"reason\":\"checked\"},{\"id\":\"academic-layout\",\"action\":\"kept\",\"reason\":\"checked\"}]}' > review.json",
    ].join("\n"), "utf8");
    await chmod(agent, 0o755);
    const result = await polishBodyWithAgent({
      sourceMarkdown: "Answer A.",
      draftBody: "Answer A.\n",
      templateText: "{{body}}",
      assemble: (body: string) => body,
      latexBin: "/usr/bin/true",
      agentBin: agent,
      backend: "codex",
      needsTitle: true,
      makeWorkdir: async () => workdir,
      maxAttempts: 1,
    });
    expect(result.usedAgent).toBe(false);
    expect(result.body).toBe("Answer A.\n");
    expect(result.aiTitle).toBe("");
  });

  test("compiles an Agent title together with its candidate body before accepting it", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-title-compile-"));
    roots.push(root);
    const workdir = join(root, "work");
    await mkdir(workdir, { recursive: true });
    const agent = join(root, "fake-agent.sh");
    const compiler = join(root, "fake-latex.sh");
    await writeFile(agent, [
      "#!/bin/sh",
      "printf 'Answer A.\\n' > body.tex",
      "printf 'Candidate Title\\n' > title.txt",
      "printf '%s\\n' '{\"decisions\":[{\"id\":\"whole-document-structure\",\"action\":\"kept\",\"reason\":\"checked\"},{\"id\":\"academic-layout\",\"action\":\"kept\",\"reason\":\"checked\"}]}' > review.json",
    ].join("\n"), "utf8");
    await writeFile(compiler, [
      "#!/bin/sh",
      "for last do :; done",
      "if grep -q 'Candidate Title' \"$last\"; then exit 1; fi",
      "exit 0",
    ].join("\n"), "utf8");
    await chmod(agent, 0o755);
    await chmod(compiler, 0o755);
    const result = await polishBodyWithAgent({
      sourceMarkdown: "Answer A.",
      draftBody: "Answer A.\n",
      templateText: "{{body}}",
      assemble: (body: string, title = "") => `\\title{${title || "Original Title"}}\n${body}`,
      latexBin: compiler,
      agentBin: agent,
      backend: "codex",
      needsTitle: true,
      makeWorkdir: async () => workdir,
      maxAttempts: 1,
    });
    expect(result.usedAgent).toBe(false);
    expect(result.aiTitle).toBe("");
  });

  test("builds mandatory and context-sensitive polish candidates", () => {
    const candidates = buildPolishCandidates("(a) First\n(b) Second\n\nProof follows.", "A".repeat(150));
    expect(candidates.map((candidate: { id: string }) => candidate.id)).toEqual(expect.arrayContaining([
      "whole-document-structure", "academic-layout", "alpha-enumeration", "role-environments", "long-material",
    ]));
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
