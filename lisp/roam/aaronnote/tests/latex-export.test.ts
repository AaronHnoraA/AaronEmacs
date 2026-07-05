import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdtemp, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";

// @ts-ignore The converter is a Node ESM module outside the TS app graph.
import { aaronnoteMarkdownToLatex, applyLatexTemplate, escapeLatexTitle, latexMacrosPreamble, latexSideCommentPreamble, writeLatexExport } from "../server/lib/latex-export.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { configure, exportLatex, latexExportAgentStatus, latexExportDefaults, listLatexTemplates, setLatexExportAgent } from "../server/lib/index.mjs";

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
    // Keep unit tests deterministic and offline: never shell out to codex.
    latexExportEngine: "mechanical",
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
    expect(result.body).toContain("\\begin{proof}[Proof (\\(\\Rightarrow\\))]");
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

  test("preserves nested unordered and ordered list hierarchy", () => {
    const result = aaronnoteMarkdownToLatex([
      "- Parent",
      "    1. First child",
      "    2. Second child",
      "- Tail",
    ].join("\n"));
    expect(result.body).toContain([
      "\\begin{itemize}",
      "\\item Parent",
      "\\begin{enumerate}",
      "\\item First child",
      "\\item Second child",
      "\\end{enumerate}",
      "\\item Tail",
      "\\end{itemize}",
    ].join("\n"));
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

  test("keeps the Proof label visible when a proof has a direction or title", () => {
    const result = aaronnoteMarkdownToLatex([
      "#+begin proof <=",
      "Left direction.",
      "#+end proof",
      "",
      "#+begin proof Easy direction",
      "Right direction.",
      "#+end proof",
    ].join("\n"));
    expect(result.body).toContain("\\begin{proof}[Proof (\\(\\Leftarrow\\))]");
    expect(result.body).toContain("\\begin{proof}[Proof (Easy direction)]");
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

  test("omits inline @@comment annotations from exported body", () => {
    const result = aaronnoteMarkdownToLatex([
      "# Main",
      "",
      "@@comment [a private annotation line]",
      "",
      "Visible text. @@comment [hidden aside] More text.",
      "",
    ].join("\n"));

    expect(result.body).toContain("Visible text.");
    expect(result.body).toContain("More text.");
    expect(result.body).not.toContain("comment");
    expect(result.body).not.toContain("private annotation");
    expect(result.body).not.toContain("hidden aside");
  });

  test("converts @@scomment and reports the required LaTeX feature", () => {
    const result = aaronnoteMarkdownToLatex([
      "# Main",
      "",
      String.raw`Visible @@scomment [Check **non-degenerate** \(u,v,w\).] text.`,
      "",
    ].join("\n"));

    expect(result.body).toContain(String.raw`\sidecomment{Check \textbf{non-degenerate} \(u,v,w\).}`);
    expect(result.body).toContain("Visible");
    expect(result.body).toContain("text.");
    expect(result.features).toEqual({ usesSideComment: true });
  });

  test("injects the side-comment package and macro only when needed", () => {
    const enabled = latexSideCommentPreamble(true);
    expect(enabled).toContain("todonotes");
    expect(enabled).toContain("\\providecommand{\\sidecomment}");
    expect(enabled).toContain("fancyline");
    expect(latexSideCommentPreamble(false)).toBe("");

    const plain = aaronnoteMarkdownToLatex("Plain body.");
    expect(plain.features).toEqual({ usesSideComment: false });
    const latex = applyLatexTemplate("\\documentclass{article}\n\\begin{document}\n{{body}}\n\\end{document}", {
      macros: enabled,
      body: String.raw`\sidecomment{note}`,
    });
    expect(latex.indexOf("todonotes")).toBeLessThan(latex.indexOf("\\begin{document}"));
  });

  test("writes export and remembers the last path per note", async () => {
    const { notes } = await setupRoot();
    const note = join(notes, "a.md");
    const out = join(notes, "out", "a.tex");
    await writeFile(note, "#+begin meta\ntitle: A\n#+end meta\n\n# A\n\nBody with \\(x\\). @@scomment [Review \\(x\\).]\n", "utf8");

    const exported = await exportLatex({ file: note, outputPath: out }) as { ok?: boolean; file?: string };
    expect(exported.ok).toBe(true);
    expect(exported.file).toBe(out);
    // Title precedence: explicit meta title ("A") wins over the filename ("a").
    const tex = await readFile(out, "utf8");
    expect(tex).toContain("\\title{ A }");
    expect(tex).toContain("\\providecommand{\\sidecomment}");
    expect(tex).toContain("\\sidecomment{Review \\(x\\).}");

    const defaults = await latexExportDefaults({ file: note }) as { outputPath?: string };
    expect(defaults.outputPath).toBe(out);
  });

  test("lists templates and parses their headers", async () => {
    const { root } = await setupRoot();
    const latexDir = join(root, "templates", "latex");
    await writeFile(join(latexDir, "aaronnote-assignment.tex"), [
      '% aaronnote-template: {"name":"Assignment","engine":"xelatex","vars":[{"id":"coursecode","label":"Course code","default":"COMP"}]}',
      "\\documentclass{article}",
      "\\begin{document}{{body}}\\end{document}",
      "",
    ].join("\n"), "utf8");

    const result = await listLatexTemplates() as { templates?: Array<Record<string, unknown>> };
    const templates = result.templates || [];
    const article = templates.find((t) => t.key === "aaronnote-article");
    const assignment = templates.find((t) => t.key === "aaronnote-assignment");
    expect(templates[0]?.key).toBe("aaronnote-article"); // default sorts first
    expect(article?.engine).toBe("pdflatex"); // header-less falls back to pdflatex
    expect(assignment?.name).toBe("Assignment");
    expect(assignment?.engine).toBe("xelatex");
    expect((assignment?.vars as Array<Record<string, unknown>>)[0]?.id).toBe("coursecode");
  });

  test("selects a template by path and fills declared vars", async () => {
    const { root, notes } = await setupRoot();
    const templatePath = join(root, "templates", "latex", "aaronnote-assignment.tex");
    await writeFile(templatePath, [
      '% aaronnote-template: {"name":"Assignment","engine":"pdflatex","vars":[{"id":"coursecode","label":"Course code","default":"COMP"}]}',
      "\\documentclass{article}",
      "\\newcommand{\\course}{ {{coursecode}} }",
      "\\begin{document}",
      "{{body}}",
      "\\end{document}",
      "",
    ].join("\n"), "utf8");
    const note = join(notes, "assg.md");
    await writeFile(note, "# Q1\n\nBody text.\n", "utf8");
    const out = join(notes, "assg.tex");

    const exported = await exportLatex({
      file: note,
      outputPath: out,
      templatePath,
      vars: { coursecode: "COMP3453" },
    }) as { ok?: boolean; template?: string; engine?: string };
    expect(exported.ok).toBe(true);
    expect(exported.template).toBe(templatePath);
    expect(exported.engine).toBe("mechanical");
    const tex = await readFile(out, "utf8");
    expect(tex).toContain("\\newcommand{\\course}{ COMP3453 }");
    expect(tex).toContain("Body text.");

    // The chosen template + vars are remembered for the next export of this note.
    const defaults = await latexExportDefaults({ file: note }) as { template?: string; vars?: Record<string, string> };
    expect(defaults.template).toBe(templatePath);
    expect(defaults.vars?.coursecode).toBe("COMP3453");
  });

  test("does not force document title markup into templates without a title placeholder", async () => {
    const { root, notes } = await setupRoot();
    const templatePath = join(root, "templates", "latex", "body-only.tex");
    await writeFile(templatePath, [
      '% aaronnote-template: {"name":"Body only","engine":"pdflatex"}',
      "\\documentclass{article}",
      "\\begin{document}",
      "{{body}}",
      "\\end{document}",
      "",
    ].join("\n"), "utf8");
    const note = join(notes, "body-only.md");
    await writeFile(note, "# Generic\n\nActual body.\n", "utf8");
    const out = join(notes, "body-only.tex");

    await exportLatex({ file: note, outputPath: out, templatePath });
    const tex = await readFile(out, "utf8");
    expect(tex).toContain("Actual body.");
    expect(tex).not.toContain("\\title{");
    expect(tex).not.toContain("\\maketitle");
  });

  test("switches and persists the LaTeX export agent backend at runtime", async () => {
    const { root } = await setupRoot();
    let status = await setLatexExportAgent({ agent: "opencode" }) as { agent?: string; engine?: string };
    expect(status.agent).toBe("opencode");
    expect(status.engine).toBe("codex");

    configure({
      root: join(root, "roam"),
      workspaceRoot: root,
      latexTemplatesRoot: join(root, "templates"),
      latexExportEngine: "mechanical",
      latexExportAgent: "codex",
    });
    status = await latexExportAgentStatus() as { agent?: string; engine?: string };
    expect(status.agent).toBe("opencode");
    expect(status.engine).toBe("codex");
  });

  test("merges agent-maintained conversion rules into the mechanical draft", async () => {
    const { root, notes } = await setupRoot();
    const agentDir = join(root, "agents", "latex-export");
    await mkdir(join(agentDir, "mechanical"), { recursive: true });
    await writeFile(join(agentDir, "mechanical", "rules.json"),
      JSON.stringify({ envMap: { claim: "theorem" } }), "utf8");
    configure({
      root: notes,
      workspaceRoot: root,
      latexTemplatesRoot: join(root, "templates"),
      latexExportEngine: "mechanical",
      latexAgentDir: agentDir,
    });
    const note = join(notes, "claim.md");
    await writeFile(note, "#+begin claim Key\nBody.\n#+end claim\n", "utf8");
    const out = join(notes, "claim.tex");
    await exportLatex({ file: note, outputPath: out });
    const tex = await readFile(out, "utf8");
    expect(tex).toContain("\\begin{theorem}[Key]");
  });

  test("falls back to the mechanical engine when codex is unavailable", async () => {
    const { root, notes } = await setupRoot();
    configure({
      root: notes,
      workspaceRoot: root,
      latexTemplatesRoot: join(root, "templates"),
      latexExportEngine: "codex",
      latexCodexBin: "/nonexistent/codex-binary",
    });
    const note = join(notes, "c.md");
    await writeFile(note, "# C\n\nBody.\n", "utf8");
    const out = join(notes, "c.tex");
    const exported = await exportLatex({ file: note, outputPath: out }) as { ok?: boolean; engine?: string };
    expect(exported.ok).toBe(true);
    expect(exported.engine).toBe("mechanical"); // codex unavailable -> mechanical draft
    expect(await readFile(out, "utf8")).toContain("Body.");
  });

  test("falls back to the first H1 heading for the title when no meta title", async () => {
    const { notes } = await setupRoot();
    const note = join(notes, "cheat-sheet.md");
    await writeFile(note, "# Linear Algebra Notes\n\nBody.\n", "utf8");
    const out = join(notes, "h.tex");
    const exported = await exportLatex({ file: note, outputPath: out }) as { ok?: boolean; title?: string };
    expect(exported.title).toBe("Linear Algebra Notes"); // not the filename "cheat-sheet"
    expect(await readFile(out, "utf8")).toContain("\\title{ Linear Algebra Notes }");
  });
});
