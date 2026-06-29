import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdtemp, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";

// @ts-ignore The converter is a Node ESM module outside the TS app graph.
import { aaronnoteMarkdownToLatex, applyLatexTemplate, escapeLatexTitle, latexMacrosPreamble, writeLatexExport } from "../server/lib/latex-export.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { configure, exportLatex, latexExportDefaults } from "../server/lib/index.mjs";

const roots: string[] = [];

afterEach(async () => {
  await Promise.all(roots.splice(0).map((root) => rm(root, { recursive: true, force: true })));
});

async function setupRoot() {
  const root = await mkdtemp(join(tmpdir(), "aaronnote-latex-"));
  const notes = join(root, "roam");
  const templates = join(root, "templates");
  await mkdir(notes, { recursive: true });
  await mkdir(join(templates, "latex"), { recursive: true });
  await writeFile(join(templates, "latex", "aaronnote-article.tex"), [
    "\\documentclass{article}",
    "\\usepackage{amsmath,amsthm}",
    "\\newtheorem{theorem}{Theorem}",
    "\\title{ {{title}} }",
    "\\date{ {{date}} }",
    "\\begin{document}",
    "\\maketitle",
    "{{body}}",
    "\\end{document}",
    "",
  ].join("\n"), "utf8");
  roots.push(root);
  configure({
    root: notes,
    workspaceRoot: root,
    pluginRoot: join(root, "plugin"),
    latexTemplatesRoot: templates,
  });
  return { root, notes };
}

describe("LaTeX export", () => {
  test("converts Aaronnote theorem and proof blocks", () => {
    const result = aaronnoteMarkdownToLatex([
      "#+begin meta",
      "title: Graph Tensor",
      "#+end meta",
      "",
      "# Main",
      "",
      "#+begin theorem Edge",
      "\\[",
      "\\lambda_\\otimes(B_G)=\\lambda(G)",
      "\\]",
      "#+end theorem",
      "",
      "#+begin proof =>",
      "If \\(d < \\lambda(G)\\), contradiction.",
      "#+end proof",
      "",
    ].join("\n"));

    expect(result.meta.title).toBe("Graph Tensor");
    expect(result.body).toContain("\\section{Main}");
    expect(result.body).toContain("\\begin{theorem}[Edge]");
    expect(result.body).toContain("\\lambda_\\otimes(B_G)=\\lambda(G)");
    expect(result.body).toContain("\\begin{proof}[=>]");
    expect(result.body).toContain("If \\(d < \\lambda(G)\\), contradiction.");
  });

  test("converts inline Markdown to valid LaTeX commands", () => {
    const result = aaronnoteMarkdownToLatex(
      "Text **bold_x**, *emphasis*, `a_b`, [paper](https://example.com/a_b), and \\(x_1\\).",
    );

    expect(result.body).toContain("\\textbf{bold\\_x}");
    expect(result.body).toContain("\\emph{emphasis}");
    expect(result.body).toContain("\\texttt{a\\_b}");
    expect(result.body).toContain("\\href{https://example.com/a_b}{paper}");
    expect(result.body).toContain("\\(x_1\\)");
    expect(result.body).not.toContain("\\\\textbf");
    expect(result.body).not.toContain("[paper](");
  });

  test("preserves inline math in document titles, headings, and block labels", () => {
    const result = aaronnoteMarkdownToLatex([
      "# \\(\\lambda\\) and \\(\\kappa\\)",
      "",
      "#+begin theorem Case \\(d < \\lambda(G)\\)",
      "Body.",
      "#+end theorem",
      "",
    ].join("\n"));
    const latex = applyLatexTemplate("\\title{ {{title}} }\n{{body}}", {
      title: escapeLatexTitle("\\(\\lambda\\) and \\(\\kappa\\)"),
      body: result.body,
    });

    expect(result.body).toContain("\\section{\\(\\lambda\\) and \\(\\kappa\\)}");
    expect(result.body).toContain("\\begin{theorem}[Case \\(d < \\lambda(G)\\)]");
    expect(latex).toContain("\\title{ \\(\\lambda\\) and \\(\\kappa\\) }");
    expect(latex).not.toContain("\\textbackslash");
  });

  test("rejects structurally incomplete source instead of emitting broken LaTeX", () => {
    expect(() => aaronnoteMarkdownToLatex("\\[\nx + y\n")).toThrow(/Unclosed display math.*line 1/);
    expect(() => aaronnoteMarkdownToLatex("```ts\nconst x = 1;\n")).toThrow(/Unclosed Markdown code fence.*line 1/);
    expect(() => aaronnoteMarkdownToLatex("#+begin theorem\nBody\n#+end proof\n")).toThrow(/Mismatched Aaronnote block/);
  });

  test("writes atomically and never writes LaTeX into a non-tex extension", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-latex-write-"));
    roots.push(root);
    const file = await writeLatexExport(join(root, "paper.pdf"), "\\documentclass{article}\n");
    expect(file).toBe(join(root, "paper.pdf.tex"));
    expect(await readFile(file, "utf8")).toContain("\\documentclass");
    await expect(writeLatexExport("", "body")).rejects.toThrow(/Missing output path/);
  });

  test("injects global KaTeX macros into the LaTeX preamble", () => {
    const macros = latexMacrosPreamble({
      "\\rank": "\\operatorname{rank}",
      "\\ip": "\\left\\langle#1,#2\\right\\rangle",
    });
    const latex = applyLatexTemplate("\\documentclass{article}\n\\begin{document}\n{{body}}\n\\end{document}\n", {
      macros,
      body: "\\(\\rank \\Phi_C \\le \\dim N\\)",
    });

    expect(latex).toContain("\\providecommand{\\rank}{}");
    expect(latex).toContain("\\renewcommand{\\rank}{\\operatorname{rank}}");
    expect(latex).toContain("\\providecommand{\\ip}[2]{}");
    expect(latex.indexOf("\\renewcommand{\\rank}")).toBeLessThan(latex.indexOf("\\begin{document}"));
  });

  test("omits Aaronnote todos from exported body", () => {
    const result = aaronnoteMarkdownToLatex([
      "# Main",
      "",
      "@@todo(doing) [draft private reminder] {ddl: 2026-07-03}",
      "@@todo bare private reminder",
      "",
      "Visible text. @@todo(done) [hidden inline reminder] More text.",
      "",
    ].join("\n"));

    expect(result.body).toContain("Visible text.");
    expect(result.body).toContain("More text.");
    expect(result.body).not.toContain("TODO");
    expect(result.body).not.toContain("todo");
    expect(result.body).not.toContain("private reminder");
    expect(result.body).not.toContain("hidden inline reminder");
  });

  test("writes export and remembers the last path per note", async () => {
    const { notes } = await setupRoot();
    const note = join(notes, "a.md");
    const out = join(notes, "out", "a.tex");
    await writeFile(note, "#+begin meta\ntitle: A\n#+end meta\n\n# A\n\nBody with \\(x\\).\n", "utf8");

    const exported = await exportLatex({ file: note, outputPath: out }) as { ok?: boolean; file?: string };
    expect(exported.ok).toBe(true);
    expect(exported.file).toBe(out);
    expect(await readFile(out, "utf8")).toContain("\\title{ A }");

    const defaults = await latexExportDefaults({ file: note }) as { outputPath?: string };
    expect(defaults.outputPath).toBe(out);
  });
});
