// Codex-assisted LaTeX export polish.
//
// Pipeline: the Aaronnote preprocessor + Pandoc produces a draft body;
// this module lets codex adjust that draft (formatting only, never prose), the
// server compiles the assembled document, and on failure feeds the log back to
// codex for a bounded number of retries. If codex is unavailable or never
// produces a compiling body, the caller falls back to the verified Pandoc draft.
//
// This module owns no runtime state: paths, the template-assembly closure, and
// resolved executable paths are all passed in, so it stays pure enough to test.

import { execFile, spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { appendFile, copyFile, cp, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { basename, join } from "node:path";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

export function codexAvailable(codexBin) {
  const bin = String(codexBin || "").trim();
  if (!bin) return false;
  // `executablePath` (runtime) resolves to an absolute path when found, else the
  // bare command name. Treat an existing file as available; a bare name is only
  // trusted when it contains no path separator (assumed to be on PATH).
  if (existsSync(bin)) return true;
  return !bin.includes("/");
}

// Backend-neutral alias (codex/claude/opencode all resolve to a bin path).
export const agentAvailable = codexAvailable;

// ---- Agent-maintained conversion rules -------------------------------------

export async function loadAgentRules(agentDir) {
  const dir = String(agentDir || "").trim();
  if (!dir) return null;
  try {
    const raw = await readFile(join(dir, "mechanical", "rules.json"), "utf8");
    const parsed = JSON.parse(raw);
    if (!parsed || typeof parsed !== "object") return null;
    const envMap = parsed.envMap && typeof parsed.envMap === "object" ? parsed.envMap : {};
    const commentBlocks = Array.isArray(parsed.commentBlocks) ? parsed.commentBlocks : [];
    const hiddenBlocks = Array.isArray(parsed.hiddenBlocks) ? parsed.hiddenBlocks : [];
    const pandocExtensions = Array.isArray(parsed.pandocExtensions) ? parsed.pandocExtensions : [];
    if (Object.keys(envMap).length === 0 && commentBlocks.length === 0 && hiddenBlocks.length === 0 && pandocExtensions.length === 0) return null;
    return { envMap, commentBlocks, hiddenBlocks, pandocExtensions };
  } catch {
    return null;
  }
}

export async function recordPendingImprovement(pendingLogFile, entry) {
  const file = String(pendingLogFile || "").trim();
  if (!file) return;
  try {
    await mkdir(join(file, ".."), { recursive: true });
    await appendFile(file, `${JSON.stringify({ at: new Date().toISOString(), ...entry })}\n`, "utf8");
  } catch {}
}

// ---- Prose fidelity check (heuristic, non-blocking) ------------------------

function proseWords(text) {
  let s = String(text ?? "");
  // Drop math, code, comments, and Aaronnote todos before comparing words.
  s = s.replace(/\\\(.*?\\\)/gs, " ").replace(/\\\[.*?\\\]/gs, " ").replace(/\$\$.*?\$\$/gs, " ");
  s = s.replace(/```[\s\S]*?```/g, " ").replace(/\\begin\{verbatim\}[\s\S]*?\\end\{verbatim\}/g, " ");
  s = s.replace(/@@(?:todo|itodo)[^\n]*/gi, " ");
  s = s.replace(/%[^\n]*/g, " "); // LaTeX comments
  s = s.replace(/\\[A-Za-z@]+\s*(\[[^\]]*\])?/g, " "); // LaTeX command names + optional args
  s = s.replace(/[#+*_>`~^{}\\$&/=.,;:!?()\[\]|"'—–-]/g, " "); // markup + punctuation
  const words = s.toLowerCase().match(/[\p{L}\p{N}]+/gu) || [];
  const counts = new Map();
  for (const w of words) {
    if (w.length < 2) continue; // ignore single chars / stray letters from commands
    counts.set(w, (counts.get(w) || 0) + 1);
  }
  return counts;
}

export function proseFidelityWarnings(sourceMarkdown, latexBody, options = {}) {
  const threshold = Number.isFinite(options.threshold) ? options.threshold : 0.08;
  const src = proseWords(sourceMarkdown);
  const out = proseWords(latexBody);
  let total = 0;
  let missing = 0;
  for (const [w, n] of src) {
    total += n;
    const have = out.get(w) || 0;
    if (have < n) missing += n - have;
  }
  let extra = 0;
  for (const [w, n] of out) {
    const had = src.get(w) || 0;
    if (n > had) extra += n - had;
  }
  const warnings = [];
  if (total > 0 && missing / total > threshold) {
    warnings.push(`fidelity: ~${missing}/${total} source words are missing from the LaTeX body (possible dropped/reworded text)`);
  }
  if (total > 0 && extra / total > threshold) {
    warnings.push(`fidelity: ~${extra} words in the LaTeX body are not in the source (possible added text)`);
  }
  return warnings;
}

function protectedPayloads(text) {
  const source = String(text || "");
  return {
    math: [...source.matchAll(/\\\(([\s\S]*?)\\\)|\\\[([\s\S]*?)\\\]/g)].map((match) => (match[1] ?? match[2] ?? "").replace(/\s+/g, "")),
    code: [...source.matchAll(/\\begin\{verbatim\}([\s\S]*?)\\end\{verbatim\}|\\texttt\{((?:\\.|[^{}])*)\}|\\verb\*?(.)(.*?)\3/g)]
      .map((match) => (match[1] ?? match[2] ?? match[4] ?? "").replace(/^\n|\n$/g, "")),
    citations: [...source.matchAll(/\\cite(?:\[[^\]]*\])?\{([^}]+)\}/g)].map((match) => match[0]),
    resources: [...source.matchAll(/\\href\{((?:\\.|[^{}])*)\}|\\includegraphics(?:\[([^\]]*)\])?\{([^}]*)\}/g)]
      .map((match) => match[1] ?? `${match[3] || ""}\0${String(match[2] || "").match(/\balt\s*=\s*\{([^}]*)\}/)?.[1] || ""}`),
    anchors: [...source.matchAll(/\\(?:label|hypertarget)\{([^}]*)\}/g)].map((match) => match[1]),
  };
}

function romanNumeral(value) {
  const table = [[1000, "m"], [900, "cm"], [500, "d"], [400, "cd"], [100, "c"], [90, "xc"], [50, "l"], [40, "xl"], [10, "x"], [9, "ix"], [5, "v"], [4, "iv"], [1, "i"]];
  let number = Math.max(1, Number(value) || 1);
  let output = "";
  for (const [amount, symbol] of table) while (number >= amount) { output += symbol; number -= amount; }
  return output;
}

function listLabel(style, index) {
  const kind = /Alph/.test(style) ? "Alph" : /alph/.test(style) ? "alph" : /Roman/.test(style) ? "Roman" : /roman/.test(style) ? "roman" : "arabic";
  let value = kind === "alph" || kind === "Alph"
    ? String.fromCharCode((kind === "Alph" ? 65 : 97) + ((index - 1) % 26))
    : kind === "roman" || kind === "Roman"
      ? (kind === "Roman" ? romanNumeral(index).toUpperCase() : romanNumeral(index))
      : String(index);
  if (/\([^)]*(?:alph|roman|arabic)/i.test(style)) value = `(${value})`;
  else if (/(?:alph|roman|arabic)[^}]*\)/i.test(style)) value = `${value})`;
  else value = `${value}.`;
  return value;
}

function renderListLabels(text) {
  const stack = [];
  const output = [];
  for (const line of String(text || "").split("\n")) {
    const begin = line.match(/\\begin\{(enumerate|itemize|description)\}(?:\[([^\]]*)\])?/);
    if (begin) stack.push({ kind: begin[1], style: begin[2] || "arabic", count: 0 });
    const definition = line.match(/^\s*\\def\\labelenum\w+\{(.+)\}\s*$/);
    if (definition && stack.at(-1)?.kind === "enumerate") {
      stack.at(-1).style = definition[1];
      continue;
    }
    const item = line.match(/\\item(?:\[([^\]]*)\])?/);
    if (item && stack.length) {
      const current = stack.at(-1);
      current.count += 1;
      const label = item[1] || (current.kind === "enumerate" ? listLabel(current.style, current.count) : current.kind === "itemize" ? "•" : "");
      output.push(line.slice(0, item.index) + (label ? `${label} ` : "") + line.slice(item.index + item[0].length));
    } else output.push(line);
    const end = line.match(/\\end\{(enumerate|itemize|description)\}/);
    if (end && stack.at(-1)?.kind === end[1]) stack.pop();
  }
  return output.join("\n");
}

function structuralSignature(text) {
  const tokens = [];
  const pattern = /\\(begin|end)\{([^}]+)\}|\\(part|chapter|section|subsection|subsubsection|paragraph|subparagraph)(\*)?|\\item(?:\[([^\]]*)\])?|\\def\\labelenum\w+\{([^\n]+)\}|\\(footnote|caption)(\*)?/g;
  const structuralEnvironments = new Set(["enumerate", "itemize", "description", "theorem", "lemma", "proposition", "corollary", "definition", "remark", "example", "proof", "quote"]);
  for (const match of String(text || "").matchAll(pattern)) {
    if (match[1] && structuralEnvironments.has(match[2])) tokens.push(`${match[1]}:${match[2]}`);
    else if (match[3]) tokens.push(`heading:${match[3]}${match[4] || ""}`);
    else if (match[0].startsWith("\\item")) tokens.push(`item:${match[5] || ""}`);
    else if (match[6] != null) tokens.push(`enum-label:${match[6].replace(/\s+/g, "")}`);
    else if (match[7]) tokens.push(`${match[7]}${match[8] || ""}`);
  }
  let tableDepth = 0;
  for (const line of String(text || "").split(/\r?\n/)) {
    if (/\\begin\{(?:longtable\*?|tabular\*?|tabularx|array)\}/.test(line)) tableDepth += 1;
    if (tableDepth > 0) {
      const separators = [...line.matchAll(/(?<!\\)&/g)].length;
      const rowEnd = /(?<!\\)\\\\(?:\[[^\]]*\])?\s*$/.test(line);
      if (separators > 0 || rowEnd) tokens.push(`table-row:${separators + 1}`);
    }
    if (/\\end\{(?:longtable\*?|tabular\*?|tabularx|array)\}/.test(line)) tableDepth = Math.max(0, tableDepth - 1);
  }
  return tokens;
}

function nonListStructure(tokens) {
  return tokens.filter((token) => !/^(?:begin|end):(enumerate|itemize|description)$|^item:|^enum-label:/.test(token));
}

function visibleContentSignature(text) {
  let source = renderListLabels(text);
  let resourceIndex = 0;
  let anchorIndex = 0;
  let codeIndex = 0;
  let mathIndex = 0;
  let citeIndex = 0;
  source = source.replace(/\\href\{(?:\\.|[^{}])*\}\{|\\includegraphics(?:\[[^\]]*\])?\{[^}]*\}/g,
    (match) => ` AARONNOTERESOURCE${resourceIndex++} ${match.startsWith("\\href") ? "{" : ""}`);
  source = source.replace(/\\label\{[^}]*\}|\\hypertarget\{[^}]*\}\{/g,
    (match) => ` AARONNOTEANCHOR${anchorIndex++} ${match.startsWith("\\hypertarget") ? "{" : ""}`);
  source = source.replace(/\\begin\{verbatim\}[\s\S]*?\\end\{verbatim\}|\\texttt\{(?:\\.|[^{}])*\}|\\verb\*?(.).*?\1/g,
    () => ` AARONNOTECODE${codeIndex++} `);
  source = source.replace(/\\\([\s\S]*?\\\)|\\\[[\s\S]*?\\\]/g,
    () => ` AARONNOTEMATH${mathIndex++} `);
  source = source.replace(/\\cite(?:\[[^\]]*\])?\{[^}]+\}/g,
    () => ` AARONNOTECITE${citeIndex++} `);
  source = source
    .replace(/(?<!\\)%[^\n]*/g, " ")
    .replace(/^\s*\\def\\LTcaptype\{none\}\s*$/gm, " ")
    .replace(/\\begin\{(?:longtable\*?|tabular\*?|tabularx|array)\}(?:\[[^\]]*\])?\{(?:[^{}]|\{[^{}]*\})*\}/g, " ")
    .replace(/\\(?:begin|end)\{[^{}]+\}/g, " ")
    .replace(/\\multicolumn\{[^{}]*\}\{[^{}]*\}\{/g, "{")
    .replace(/\\(?:noalign|Needspace|vspace\*?|hspace\*?|addvspace|setlength|addtolength|enlargethispage)(?:\[[^\]]*\])?(?:\{(?:[^{}]|\{[^{}]*\})*\})*/g, "")
    .replace(/\\textbackslash\{\}/g, "\uE000")
    .replace(/\\textasciicircum\{\}/g, "\uE001")
    .replace(/\\textasciitilde\{\}/g, "\uE002")
    .replace(/\\([#$%&_{}])/g, (_match, value) => ({ "#": "\uE003", "$": "\uE004", "%": "\uE005", "&": "\uE006", "_": "\uE007", "{": "\uE008", "}": "\uE009" })[value])
    .replace(/\\\\|\\[ \t]/g, " ")
    .replace(/\\([A-Za-z@]+)\*?/g, (_match, name) => new Set([
      "textbf", "emph", "textit", "textnormal", "textrm", "textsf", "textsl", "textsc", "underline", "uline", "sout", "st",
      "textsuperscript", "textsubscript", "section", "subsection", "subsubsection", "paragraph", "subparagraph", "part", "chapter",
      "item", "tightlist", "toprule", "midrule", "bottomrule", "endhead", "endlastfoot", "tabularnewline", "noalign",
      "footnote", "caption", "noindent", "newpage", "clearpage", "pagebreak", "nopagebreak", "allowbreak", "linebreak", "newline",
      "Needspace", "raggedright", "centering", "small", "footnotesize", "scriptsize", "sloppy", "fussy", "qedhere", "hfill", "vfill",
      "smallskip", "medskip", "bigskip", "quad", "qquad", "par", "pandocbounded",
    ]).has(name) ? "" : ` AARONNOTECOMMAND:${name} `)
    .replace(/[{}]/g, "")
    .replace(/&/g, " ")
    .replace(/~/g, " ")
    .replace(/\uE000/g, "\\")
    .replace(/\uE001/g, "^")
    .replace(/\uE002/g, "~")
    .replace(/\uE003/g, "#")
    .replace(/\uE004/g, "$")
    .replace(/\uE005/g, "%")
    .replace(/\uE006/g, "&")
    .replace(/\uE007/g, "_")
    .replace(/\uE008/g, "{")
    .replace(/\uE009/g, "}")
    .replace(/\s+/g, " ")
    .trim();
  return source;
}

export function strictFidelityIssues(draftBody, polishedBody) {
  const issues = [];
  const draftVisible = visibleContentSignature(draftBody);
  const polishedVisible = visibleContentSignature(polishedBody);
  if (draftVisible !== polishedVisible) issues.push("visible prose tokens changed or were reordered");
  const draftStructure = structuralSignature(draftBody);
  const polishedStructure = structuralSignature(polishedBody);
  if (JSON.stringify(draftStructure) !== JSON.stringify(polishedStructure)) {
    const listOnlyEquivalent = draftVisible === polishedVisible
      && JSON.stringify(nonListStructure(draftStructure)) === JSON.stringify(nonListStructure(polishedStructure));
    if (!listOnlyEquivalent) issues.push("document structure changed or was reordered");
  }
  const draftProtected = protectedPayloads(draftBody);
  const polishedProtected = protectedPayloads(polishedBody);
  for (const key of ["math", "code", "citations", "resources", "anchors"]) {
    if (JSON.stringify(draftProtected[key]) !== JSON.stringify(polishedProtected[key])) issues.push(`${key} payloads changed or were reordered`);
  }
  return issues;
}

export function buildPolishCandidates(sourceMarkdown, draftBody) {
  const candidates = [
    { id: "whole-document-structure", kind: "structure", detail: "Audit heading, paragraph, list, theorem/proof, citation, math, and code structure end-to-end." },
    { id: "academic-layout", kind: "typesetting", detail: "Audit restrained academic spacing, page flow, tables, figures, long material, and template fit." },
  ];
  const source = String(sourceMarkdown || "");
  if (/(?:^|\n)\s*\([a-z]\)\s+.+\n\s*\([a-z]\)\s+/i.test(source)) {
    candidates.push({ id: "alpha-enumeration", kind: "list", detail: "Verify that consecutive (a)/(b) material is a true list and that Pandoc preserved the intended labels." });
  }
  if (/^(?:problem|solution|answer|proof)\b/im.test(source)) {
    candidates.push({ id: "role-environments", kind: "environment", detail: "Review explicit Problem/Solution/Answer/Proof roles against environments actually defined by the template." });
  }
  if (String(draftBody || "").split(/\r?\n/).some((line) => line.length > 140 && !/^\\(?:begin|end)\b/.test(line))) {
    candidates.push({ id: "long-material", kind: "line-break", detail: "Review long URLs, inline math, code-like text, or table cells for semantic break opportunities without rewriting content." });
  }
  return candidates;
}

function reviewGateIssue(review, candidates) {
  if (!review || !Array.isArray(review.decisions)) return "review.json missing or invalid";
  const decisions = new Map(review.decisions.map((decision) => [String(decision?.id || ""), decision]));
  for (const candidate of candidates) {
    const decision = decisions.get(candidate.id);
    if (!decision) return `review.json omitted candidate ${candidate.id}`;
    if (!["applied", "kept"].includes(String(decision.action || ""))) return `review.json has invalid action for ${candidate.id}`;
    if (!String(decision.reason || "").trim()) return `review.json has no reason for ${candidate.id}`;
  }
  return "";
}

// ---- Compile a candidate assembled document --------------------------------

// We only need to know whether the document compiles, never the PDF itself, so
// verify in draft mode: pdflatex/lualatex skip PDF output with `-draftmode`,
// xelatex with `-no-pdf`. This skips font embedding / PDF writing — the biggest
// per-attempt cost.
function draftModeFlag(engine) {
  return engine === "xelatex" ? "-no-pdf" : "-draftmode";
}

async function compileLatex({ tex, dir, latexBin, engine = "pdflatex", sourceDir, timeoutMs, signal }) {
  const texFile = join(dir, "out.tex");
  await writeFile(texFile, tex, "utf8");
  // Compile inside the staging dir (so filecontents-based classes stay there),
  // but let \includegraphics / \input resolve assets next to the source note.
  const env = { ...process.env };
  if (sourceDir) env.TEXINPUTS = `${sourceDir}//:${env.TEXINPUTS || ""}`;
  try {
    await execFileAsync(latexBin, [
      "-interaction=nonstopmode",
      "-halt-on-error",
      draftModeFlag(engine),
      `-output-directory=${dir}`,
      texFile,
    ], { cwd: dir, env, timeout: timeoutMs, maxBuffer: 16 * 1024 * 1024, signal });
    let log = "";
    try { log = await readFile(join(dir, "out.log"), "utf8"); } catch {}
    const layout = log.split(/\r?\n/)
      .filter((line) => /Overfull \\[hv]box|Float too large|Too many unprocessed floats/i.test(line))
      .slice(-20)
      .join("\n");
    return { ok: true, log: layout };
  } catch (err) {
    const logFile = join(dir, "out.log");
    let log = "";
    try { log = await readFile(logFile, "utf8"); } catch {}
    const tail = (log || `${err?.stdout || ""}\n${err?.stderr || ""}` || String(err?.message || ""))
      .split(/\r?\n/).filter((l) => /^!|error|undefined|runaway|missing|\.tex:\d+/i.test(l))
      .slice(-30).join("\n");
    return { ok: false, log: tail || String(err?.message || "LaTeX compile failed").slice(0, 2000) };
  }
}

// ---- Agent invocation (codex / claude / opencode) --------------------------

function buildPrompt({ retryLog, needsTitle = true, sourceTitle = "", documentRole = "" }) {
  const base = [
    "You are polishing a LaTeX export. All files are in your working directory.",
    "Read: style.md (style contract — obey strictly), AGENTS.md (your contract),",
    "skills/aaronnote-latex-polish/SKILL.md and skills/academic-typesetting/SKILL.md,",
    "then polish-candidates.json; review.json must answer every candidate id.",
    "syntax.md (Aaronnote/Markdown syntax), template.tex (the target template; note",
    "which theorem environments and macros it defines), source.md (the author's",
    "Markdown — the source of truth for text), and draft.tex (the Pandoc-based",
    "conversion). body.tex currently equals draft.tex.",
    sourceTitle ? `The original source-name title is: ${JSON.stringify(sourceTitle)}.` : "",
    documentRole ? `The selected template's document role is: ${JSON.stringify(documentRole)}.` : "",
    "",
    "ROLE: You are a format converter and validator, NOT an author or copy editor.",
    "GOAL: deliver a publication-ready LaTeX body that is faithful to source.md,",
    "fits template.tex, and compiles cleanly. Priorities, in order:",
    "1. Preserve every public statement and its logical order exactly.",
    "2. Preserve semantic structure: headings, paragraphs, lists, math, proofs,",
    "   theorem labels, citations, explicit line breaks, and code-like material.",
    "3. Use only environments/macros supported by template.tex and style.md.",
    "4. Apply restrained academic typesetting: coherent hierarchy and theorem/proof/",
    "   list/math presentation, balanced spacing, and sensible page flow.",
    "5. Improve layout only where source semantics justify it; do not add decorative",
    "   boxes, colours, rules, abstracts, numbering, captions, or invented structure.",
    "Fidelity is the hard gate: if a formatting improvement might change text or",
    "meaning, do not make it. It is correct to leave already-faithful markup alone.",
    "",
    "Edit body.tex so that it compiles when the host inserts it into template.tex",
    "and its formatting follows style.md. Do NOT add, remove, translate, or reword",
    "any prose from source.md — only change markup. Emit body content only: no",
    "\\documentclass, no preamble, no package or macro definitions.",
    "",
    "Before writing the final files, perform this mandatory review:",
    "- compare source.md against body.tex from beginning to end for omissions,",
    "  duplication, reordered text, leaked private commands, and broken math;",
    "- check every begin/end pair, moving argument, list nesting, and explicit break;",
    "- check likely overfull boxes, orphan headings, excessive whitespace, and title",
    "  overflow against template.tex; make only markup-level corrections;",
    "- leave body.tex unchanged when draft.tex is already the most faithful result.",
    "",
    needsTitle
      ? [
          "This template uses a document title. After reading the full source.md and",
          "final body.tex, write a concise document title to title.txt (one plain-text",
          "line, no markup, no quotes). A title is a short application-facing label,",
          "never a summary sentence. Synthesize exactly three signals: the semantic",
          "intent of the original source name, the document role implied by the template",
          "(Assignment, Report, Notes, etc.), and ONE dominant subject from the content.",
          "Do not blindly copy an internal slug or abbreviation such as assg/hw/q1;",
          "expand its intent using the role and subject. Conversely, preserve a source",
          "name that is already clear and suitable. Do not enumerate topics. Use at most",
          "42 characters and normally at most 6 words. Example: source 'assg' + an",
          "Assignment template + linear-algebra content -> 'Linear Algebra Assignment'.",
          "Ensure the result fits template.tex's title area comfortably.",
        ].join("\n")
      : [
          "The host title is authoritative or this template does not accept a generated",
          "title. Do not invent title markup or force a title into body.tex; focus only",
          "on adapting the exported body to this template.",
        ].join("\n"),
    "",
    needsTitle
      ? "Write body.tex, title.txt, and the required review.json, then stop. Run no other commands."
      : "Write body.tex and the required review.json, then stop. Run no other commands.",
  ];
  if (retryLog) {
    base.push(
      "",
      "The previous attempt did not pass the host gates. Diagnostic:",
      "----",
      retryLog,
      "----",
      "Fix only what the log indicates, without changing any prose.",
    );
  }
  return base.join("\n");
}

// Backend-specific argv. All run non-interactively with permission prompts
// disabled and read/write files within the working directory.
function agentArgs(backend, { workdir, model, prompt }) {
  switch (backend) {
    case "claude":
      return [
        "-p", prompt,
        "--dangerously-skip-permissions",
        "--add-dir", workdir,
        "--output-format", "stream-json",
        "--verbose",
        ...(model ? ["--model", model] : []),
      ];
    case "opencode":
      return [
        "run",
        "--dangerously-skip-permissions",
        "--format", "json",
        ...(model ? ["-m", model] : []),
        prompt,
      ];
    case "codex":
    default:
      return [
        "exec",
        "-C", workdir,
        "--sandbox", "workspace-write",
        "--skip-git-repo-check",
        "--ephemeral",
        "-c", "approval_policy=\"never\"",
        ...(model ? ["-m", model] : []),
        prompt,
      ];
  }
}

// Extract a short human-readable progress label from a backend stdout line.
// codex prints plain text; claude/opencode emit JSONL/JSON events.
function progressLabel(backend, line) {
  const raw = String(line || "").trim();
  if (!raw) return "";
  if (backend === "codex") return raw.slice(0, 160);
  try {
    const ev = JSON.parse(raw);
    const type = ev.type || ev.event || "";
    if (backend === "claude") {
      if (type === "assistant" && ev.message?.content) {
        const t = ev.message.content.find?.((c) => c.type === "tool_use") || ev.message.content.find?.((c) => c.type === "text");
        if (t?.type === "tool_use") return `claude: ${t.name || "tool"}`;
        if (t?.type === "text" && t.text) return `claude: ${String(t.text).slice(0, 120)}`;
      }
      if (type) return `claude: ${type}`;
    } else {
      const label = ev.tool || ev.name || type;
      if (label) return `opencode: ${String(label).slice(0, 120)}`;
    }
  } catch {
    return raw.slice(0, 160);
  }
  return "";
}

function runAgent({ backend, bin, workdir, model, retryLog, needsTitle, sourceTitle, documentRole, timeoutMs, signal, onProgress }) {
  return new Promise((resolve) => {
    const args = agentArgs(backend, { workdir, model, prompt: buildPrompt({ retryLog, needsTitle, sourceTitle, documentRole }) });
    let child;
    try {
      child = spawn(bin, args, { cwd: workdir, stdio: ["ignore", "pipe", "pipe"] });
    } catch (err) {
      resolve({ ok: false, message: String(err?.message || err) });
      return;
    }
    let stderr = "";
    let stdoutBuf = "";
    let settled = false;
    const finish = (result) => {
      if (settled) return;
      settled = true;
      if (timer) clearTimeout(timer);
      if (signal) signal.removeEventListener?.("abort", onAbort);
      resolve(result);
    };
    const onAbort = () => {
      if (!child.killed) child.kill("SIGKILL");
      finish({ ok: false, message: "aborted" });
    };
    const timer = setTimeout(() => {
      if (!child.killed) child.kill("SIGKILL");
      finish({ ok: false, message: `${backend} timed out` });
    }, timeoutMs);
    if (signal) {
      if (signal.aborted) { onAbort(); return; }
      signal.addEventListener?.("abort", onAbort, { once: true });
    }
    child.stdout?.on("data", (chunk) => {
      if (!onProgress) return;
      stdoutBuf += String(chunk);
      let nl;
      while ((nl = stdoutBuf.indexOf("\n")) >= 0) {
        const line = stdoutBuf.slice(0, nl);
        stdoutBuf = stdoutBuf.slice(nl + 1);
        const label = progressLabel(backend, line);
        if (label) { try { onProgress(label); } catch {} }
      }
      if (stdoutBuf.length > 65536) stdoutBuf = stdoutBuf.slice(-65536);
    });
    child.stderr?.on("data", (chunk) => { stderr += String(chunk); if (stderr.length > 8192) stderr = stderr.slice(-8192); });
    child.on("error", (err) => finish({ ok: false, message: String(err?.message || err) }));
    child.on("close", (code) => finish(code === 0 ? { ok: true } : { ok: false, message: stderr.trim() || `${backend} exited ${code}` }));
  });
}

// ---- Orchestrator ----------------------------------------------------------

export function normalizeAgentTitle(value, maxLength = 42) {
  const line = String(value || "").split(/\r?\n/).map((part) => part.trim()).find(Boolean) || "";
  const clean = line
    .replace(/^\s*(?:title\s*:\s*)/i, "")
    .replace(/^['\"“”‘’]+|['\"“”‘’]+$/g, "")
    .replace(/[*_`#]/g, "")
    .replace(/\s+/g, " ")
    .trim();
  if ([...clean].length <= maxLength) return clean;
  const clipped = [...clean].slice(0, maxLength + 1).join("");
  const boundary = clipped.slice(0, maxLength).replace(/[\s,:;\-–—]+\S*$/u, "").trim();
  return (boundary || [...clean].slice(0, maxLength).join("")).replace(/[\s,:;\-–—]+$/u, "").trim();
}

async function readAgentTitle(workdir) {
  try {
    const raw = await readFile(join(workdir, "title.txt"), "utf8");
    return normalizeAgentTitle(raw);
  } catch {
    return "";
  }
}

async function readAgentReview(workdir) {
  try {
    const parsed = JSON.parse(await readFile(join(workdir, "review.json"), "utf8"));
    if (!parsed || !Array.isArray(parsed.decisions)) return null;
    return parsed;
  } catch {
    return null;
  }
}

/**
 * Polish the Pandoc draft with the configured agent, gated on fidelity + compilation.
 * @returns {Promise<{body:string, aiTitle:string, usedAgent:boolean, backend:string, compiled:boolean, attempts:number, warnings:string[]}>}
 */
export async function polishBodyWithAgent(opts) {
  const {
    sourceMarkdown = "",
    draftBody = "",
    templateText = "",
    styleDoc = "",
    syntaxDoc = "",
    agentsDoc = "",
    assemble,
    engine = "pdflatex",
    latexBin = "",
    backend = "codex",
    agentBin = "",
    model = "",
    sourceDir = "",
    makeWorkdir,
    maxAttempts = 3,
    needsTitle = true,
    sourceTitle = "",
    documentRole = "",
    supportFiles = [],
    skillsDir = "",
    agentTimeoutMs = 180_000,
    compileTimeoutMs = 120_000,
    signal,
    onProgress,
  } = opts || {};

  const emit = (text) => { if (onProgress && text) { try { onProgress(text); } catch {} } };
  const warnings = [];
  const base = { body: draftBody, aiTitle: "", backend, attempts: 0 };
  if (!agentAvailable(agentBin)) {
    return { ...base, usedAgent: false, compiled: false, warnings: [`${backend} unavailable; used Pandoc draft`] };
  }
  const compileEnabled = typeof assemble === "function" && !!latexBin && existsSync(latexBin);

  const workdir = await makeWorkdir();
  try {
    await writeFile(join(workdir, "source.md"), sourceMarkdown, "utf8");
    await writeFile(join(workdir, "draft.tex"), draftBody, "utf8");
    await writeFile(join(workdir, "body.tex"), draftBody, "utf8");
    await writeFile(join(workdir, "template.tex"), templateText, "utf8");
    const polishCandidates = buildPolishCandidates(sourceMarkdown, draftBody);
    await writeFile(join(workdir, "polish-candidates.json"), `${JSON.stringify({ candidates: polishCandidates }, null, 2)}\n`, "utf8");
    if (sourceTitle) await writeFile(join(workdir, "source-title.txt"), sourceTitle, "utf8");
    for (const file of supportFiles) {
      if (file?.name && file?.content) await writeFile(join(workdir, basename(file.name)), file.content);
    }
    if (styleDoc && existsSync(styleDoc)) await copyFile(styleDoc, join(workdir, "style.md"));
    if (syntaxDoc && existsSync(syntaxDoc)) await copyFile(syntaxDoc, join(workdir, "syntax.md"));
    if (agentsDoc && existsSync(agentsDoc)) await copyFile(agentsDoc, join(workdir, "AGENTS.md"));
    if (skillsDir && existsSync(skillsDir)) await cp(skillsDir, join(workdir, "skills"), { recursive: true });

    let body = draftBody;
    let retryLog = "";
    let attempts = 0;
    for (let i = 0; i < Math.max(1, maxAttempts); i += 1) {
      attempts = i + 1;
      emit(retryLog ? `Polishing with ${backend} (retry ${attempts})…` : `Polishing with ${backend}…`);
      const run = await runAgent({ backend, bin: agentBin, workdir, model, retryLog, needsTitle, sourceTitle, documentRole, timeoutMs: agentTimeoutMs, signal, onProgress });
      if (!run.ok) {
        warnings.push(`${backend} adjust failed (${run.message || "unknown"})`);
        break;
      }
      const candidateAiTitle = needsTitle ? await readAgentTitle(workdir) : "";
      try {
        body = await readFile(join(workdir, "body.tex"), "utf8");
      } catch {
        warnings.push(`${backend} did not produce body.tex; used Pandoc draft`);
        return { ...base, usedAgent: false, compiled: false, attempts, warnings };
      }
      const review = await readAgentReview(workdir);
      const fidelityIssues = strictFidelityIssues(draftBody, body);
      const reviewIssue = reviewGateIssue(review, polishCandidates);
      if (reviewIssue || fidelityIssues.length > 0) {
        const gate = [reviewIssue, ...fidelityIssues].filter(Boolean).join("; ");
        warnings.push(`${backend} polish gate rejected attempt ${attempts}: ${gate}`);
        retryLog = `POLISH GATE FAILURE: ${gate}. Re-read both skills, restore source fidelity, and write review.json.`;
        continue;
      }
      if (!compileEnabled) {
        warnings.push("compile not verified (no LaTeX engine / assembler)");
        warnings.push(...proseFidelityWarnings(sourceMarkdown, body));
        return { body, aiTitle: candidateAiTitle, backend, usedAgent: true, compiled: false, attempts, warnings };
      }
      emit(`Compiling (attempt ${attempts})…`);
      const res = await compileLatex({ tex: assemble(body, candidateAiTitle), dir: workdir, latexBin, engine, sourceDir, timeoutMs: compileTimeoutMs, signal });
      if (res.ok) {
        if (res.log) {
          emit(`Layout warnings found; feeding log back to ${backend}…`);
          retryLog = `LATEX LAYOUT WARNINGS:\n${res.log}`;
          continue;
        }
        warnings.push(...proseFidelityWarnings(sourceMarkdown, body));
        return { body, aiTitle: candidateAiTitle, backend, usedAgent: true, compiled: true, attempts, warnings };
      }
      emit(`Compile failed; feeding log back to ${backend}…`);
      retryLog = res.log;
    }

    // Agent path did not pass the gates. Fall back to the verified Pandoc draft.
    if (compileEnabled) {
      emit("Falling back to Pandoc draft…");
      const draftRes = await compileLatex({ tex: assemble(draftBody), dir: workdir, latexBin, engine, sourceDir, timeoutMs: compileTimeoutMs, signal });
      if (draftRes.ok) {
        warnings.push(`${backend} polish did not pass after ${attempts} attempt(s); used Pandoc draft`);
        return { ...base, usedAgent: false, compiled: true, attempts, warnings };
      }
      warnings.push(`neither ${backend} polish nor Pandoc draft compiled; wrote best-effort Pandoc draft`);
      return { ...base, usedAgent: false, compiled: false, attempts, warnings };
    }
    return { ...base, usedAgent: false, compiled: false, attempts, warnings };
  } finally {
    await rm(workdir, { recursive: true, force: true }).catch(() => {});
  }
}

// Back-compat alias: earlier callers used the codex-only name/shape.
export async function polishBodyWithCodex(opts = {}) {
  const result = await polishBodyWithAgent({
    ...opts,
    backend: "codex",
    agentBin: opts.agentBin || opts.codexBin || "",
    agentTimeoutMs: opts.agentTimeoutMs || opts.codexTimeoutMs,
  });
  return { ...result, usedCodex: result.usedAgent };
}
