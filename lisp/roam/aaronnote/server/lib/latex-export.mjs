import { mkdir, readFile, rename, rm, writeFile } from "node:fs/promises";
import { basename, dirname, extname, join, resolve } from "node:path";

const DEFAULT_TEMPLATE = `\\documentclass[11pt]{article}
\\usepackage[a4paper,margin=1in]{geometry}
\\usepackage{amsmath,amssymb,amsthm,mathtools}
\\usepackage{CJKutf8}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\AtBeginDocument{\\begin{CJK*}{UTF8}{gbsn}}
\\AtEndDocument{\\end{CJK*}}

\\newtheorem{theorem}{Theorem}
\\newtheorem{lemma}{Lemma}
\\newtheorem{proposition}{Proposition}
\\newtheorem{corollary}{Corollary}
\\newtheorem{definition}{Definition}
\\theoremstyle{remark}
\\newtheorem{remark}{Remark}
\\newtheorem{example}{Example}

{{macros}}

\\title{ {{title}} }
\\date{ {{date}} }

\\begin{document}
\\maketitle

{{body}}

\\end{document}
`;

const ENV_MAP = new Map([
  ["definition", "definition"],
  ["define", "definition"],
  ["theorem", "theorem"],
  ["lemma", "lemma"],
  ["proposition", "proposition"],
  ["corollary", "corollary"],
  ["proof", "proof"],
  ["remark", "remark"],
  ["example", "example"],
]);

const COMMENT_BLOCKS = new Set(["comment", "summary", "note", "important", "warning", "attention"]);
const TODO_LINE_RE = /^\s*@@todo(?:\([^)\n]*\))?\s*(?:\[[^\]\n]*\](?:\s*\{[^}\n]*\})?|[^\n]*)\s*$/i;
const TODO_INLINE_RE = /@@todo(?:\([^)\n]*\))?\s*(?:\[[^\]\n]*\](?:\s*\{[^}\n]*\})?|[^\n]*)/gi;
const DISPLAY_MATH_OPEN_RE = /^\s*(?:\\\[|\$\$)\s*$/;
const DISPLAY_MATH_CLOSE_RE = /^\s*(?:\\\]|\$\$)\s*$/;

function escapeLatexText(value) {
  return String(value ?? "")
    .replace(/\\/g, "\\textbackslash{}")
    .replace(/([#$%&_{}])/g, "\\$1")
    .replace(/\^/g, "\\textasciicircum{}")
    .replace(/~/g, "\\textasciitilde{}");
}

function escapeLatexUrl(value) {
  return String(value ?? "").trim().replace(/\\/g, "/").replace(/([%#{}])/g, "\\$1");
}

function inlineTokenAt(source, pos) {
  const rest = source.slice(pos);
  let match = rest.match(/^\\\(([^\n]+?)\\\)/);
  if (match) return { length: match[0].length, latex: `\\(${match[1]}\\)` };

  match = rest.match(/^`([^`\n]+)`/);
  if (match) return { length: match[0].length, latex: `\\texttt{${escapeLatexText(match[1])}}` };

  match = rest.match(/^!\[([^\]\n]*)\]\(([^)\n]+)\)/);
  if (match) {
    const label = convertInline(match[1] || "image");
    return { length: match[0].length, latex: `\\href{${escapeLatexUrl(match[2])}}{${label}}` };
  }

  match = rest.match(/^\[([^\]\n]+)\]\(([^)\n]+)\)/);
  if (match) return {
    length: match[0].length,
    latex: `\\href{${escapeLatexUrl(match[2])}}{${convertInline(match[1])}}`,
  };

  match = rest.match(/^\*\*([^*\n]+)\*\*/);
  if (match) return { length: match[0].length, latex: `\\textbf{${convertInline(match[1])}}` };
  match = rest.match(/^__([^_\n]+)__/);
  if (match) return { length: match[0].length, latex: `\\textbf{${convertInline(match[1])}}` };
  match = rest.match(/^\*([^*\n]+)\*/);
  if (match) return { length: match[0].length, latex: `\\emph{${convertInline(match[1])}}` };
  match = rest.match(/^_([^_\n]+)_/);
  if (match) return { length: match[0].length, latex: `\\emph{${convertInline(match[1])}}` };
  return null;
}

function convertInline(text) {
  const source = String(text ?? "").replace(TODO_INLINE_RE, "").trim();
  let latex = "";
  let plain = "";
  const flushPlain = () => {
    latex += escapeLatexText(plain);
    plain = "";
  };
  for (let pos = 0; pos < source.length;) {
    const token = inlineTokenAt(source, pos);
    if (!token) {
      plain += source[pos];
      pos += 1;
      continue;
    }
    flushPlain();
    latex += token.latex;
    pos += token.length;
  }
  flushPlain();
  return latex;
}

// Titles, headings, and environment labels are LaTeX moving arguments, but
// inline math is still valid there. Escape prose while preserving Aaronnote's
// canonical \(...\) math spans instead of turning their backslashes into text.
export function escapeLatexTitle(value) {
  return convertInline(value).replace(/\s+/g, " ").trim();
}

function parseMeta(lines) {
  const meta = {};
  let inMeta = false;
  for (const line of lines) {
    if (/^#\+begin\s+meta\s*$/i.test(line)) {
      inMeta = true;
      continue;
    }
    if (inMeta && /^#\+end\s+meta\s*$/i.test(line)) break;
    if (!inMeta) continue;
    const match = line.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
    if (match) meta[match[1].toLowerCase()] = match[2].trim();
  }
  return meta;
}

function stripMeta(lines) {
  const out = [];
  let inMeta = false;
  for (const line of lines) {
    if (/^#\+begin\s+meta\s*$/i.test(line)) {
      inMeta = true;
      continue;
    }
    if (inMeta && /^#\+end\s+meta\s*$/i.test(line)) {
      inMeta = false;
      continue;
    }
    if (!inMeta) out.push(line);
  }
  return out;
}

function sectionCommand(level) {
  if (level <= 1) return "section";
  if (level === 2) return "subsection";
  if (level === 3) return "subsubsection";
  return "paragraph";
}

function beginEnv(kind, title, envMap = ENV_MAP, commentBlocks = COMMENT_BLOCKS) {
  const env = envMap.get(kind);
  if (env) {
    let label = title ? escapeLatexTitle(title) : "";
    if (kind === "proof" && label && !/^proof\b/i.test(title.trim())) {
      const direction = title.trim() === "=>"
        ? "\\(\\Rightarrow\\)"
        : title.trim() === "<="
          ? "\\(\\Leftarrow\\)"
          : label;
      label = `Proof (${direction})`;
    }
    label = label ? `[${label}]` : "";
    return `\\begin{${env}}${label}`;
  }
  if (commentBlocks.has(kind)) {
    const heading = title || kind;
    return `\\begin{remark}[${escapeLatexTitle(heading)}]`;
  }
  return `\\paragraph{${escapeLatexTitle(kind)}}${title ? ` ${convertInline(title)}` : ""}`;
}

function endEnv(kind, envMap = ENV_MAP, commentBlocks = COMMENT_BLOCKS) {
  const env = envMap.get(kind) || (commentBlocks.has(kind) ? "remark" : "");
  return env ? `\\end{${env}}` : "";
}

// Merge agent-maintained conversion rules over the built-in mapping. The base
// module stays pure; runtime.mjs reads the rules file and passes the parsed
// object in as `options.rules`. Shape: `{ envMap: {kind: env}, commentBlocks: [] }`.
function effectiveEnvMap(rules) {
  const extra = rules && typeof rules.envMap === "object" && rules.envMap ? rules.envMap : null;
  if (!extra) return ENV_MAP;
  const merged = new Map(ENV_MAP);
  for (const [rawKind, rawEnv] of Object.entries(extra)) {
    const kind = String(rawKind || "").trim().toLowerCase();
    const env = String(rawEnv || "").trim();
    if (kind && env) merged.set(kind, env);
  }
  return merged;
}

function effectiveCommentBlocks(rules) {
  const extra = rules && Array.isArray(rules.commentBlocks) ? rules.commentBlocks : null;
  if (!extra || extra.length === 0) return COMMENT_BLOCKS;
  const merged = new Set(COMMENT_BLOCKS);
  for (const raw of extra) {
    const kind = String(raw || "").trim().toLowerCase();
    if (kind) merged.add(kind);
  }
  return merged;
}

function flushParagraph(out, paragraph) {
  if (paragraph.length === 0) return;
  out.push(paragraph.map(convertInline).join(" "));
  out.push("");
  paragraph.length = 0;
}

export function aaronnoteMarkdownToLatex(markdown, options = {}) {
  const lines = String(markdown ?? "").replace(/\r\n?/g, "\n").split("\n");
  const meta = parseMeta(lines);
  const bodyLines = stripMeta(lines);
  const envMap = effectiveEnvMap(options.rules);
  const commentBlocks = effectiveCommentBlocks(options.rules);
  const out = [];
  const paragraph = [];
  const envStack = [];
  let inFence = false;
  let fenceLine = 0;
  let inDisplayMath = false;
  let displayMathLine = 0;
  const listStack = [];

  function closeList() {
    while (listStack.length) out.push(`\\end{${listStack.pop().kind}}`, "");
  }

  function openList(kind, indent) {
    while (listStack.length && listStack.at(-1).indent > indent) {
      out.push(`\\end{${listStack.pop().kind}}`);
    }
    const current = listStack.at(-1);
    if (current?.indent === indent && current.kind === kind) return;
    if (current?.indent === indent) out.push(`\\end{${listStack.pop().kind}}`);
    listStack.push({ kind, indent });
    out.push(`\\begin{${kind}}`);
  }

  for (let lineIndex = 0; lineIndex < bodyLines.length; lineIndex += 1) {
    const rawLine = bodyLines[lineIndex];
    const lineNumber = lineIndex + 1;
    const line = rawLine.replace(/\s+$/g, "");

    if (/^```/.test(line)) {
      flushParagraph(out, paragraph);
      closeList();
      out.push(inFence ? "\\end{verbatim}" : "\\begin{verbatim}");
      inFence = !inFence;
      fenceLine = inFence ? lineNumber : 0;
      continue;
    }
    if (inFence) {
      out.push(line);
      continue;
    }

    if (!inDisplayMath && DISPLAY_MATH_OPEN_RE.test(line)) {
      flushParagraph(out, paragraph);
      closeList();
      out.push("\\[");
      inDisplayMath = true;
      displayMathLine = lineNumber;
      continue;
    }
    if (inDisplayMath && DISPLAY_MATH_CLOSE_RE.test(line)) {
      out.push("\\]");
      inDisplayMath = false;
      displayMathLine = 0;
      continue;
    }
    if (inDisplayMath) {
      out.push(line);
      continue;
    }

    const begin = line.match(/^#\+begin\s+([A-Za-z0-9_-]+)\s*(.*)$/i);
    if (begin) {
      flushParagraph(out, paragraph);
      closeList();
      const kind = begin[1].toLowerCase();
      envStack.push(kind);
      out.push(beginEnv(kind, begin[2].trim(), envMap, commentBlocks), "");
      continue;
    }
    const end = line.match(/^#\+end\s+([A-Za-z0-9_-]+)\s*$/i);
    if (end) {
      flushParagraph(out, paragraph);
      closeList();
      const requestedKind = end[1].toLowerCase();
      const kind = envStack.pop();
      if (!kind) {
        throw new Error(`Unexpected #+end ${requestedKind} on line ${lineNumber}`);
      }
      if (kind !== requestedKind) {
        throw new Error(`Mismatched Aaronnote block on line ${lineNumber}: expected #+end ${kind}, found #+end ${requestedKind}`);
      }
      const close = endEnv(kind, envMap, commentBlocks);
      if (close) out.push(close, "");
      continue;
    }

    if (/^\s*$/.test(line)) {
      flushParagraph(out, paragraph);
      closeList();
      continue;
    }

    const heading = line.match(/^(#{1,6})\s+(.+)$/);
    if (heading) {
      flushParagraph(out, paragraph);
      closeList();
      const command = sectionCommand(heading[1].length);
      out.push(`\\${command}{${escapeLatexTitle(heading[2])}}`, "");
      continue;
    }

    const unordered = line.match(/^([ \t]*)[-*+]\s+(.+)$/);
    if (unordered) {
      flushParagraph(out, paragraph);
      const indent = [...unordered[1]].reduce((sum, char) => sum + (char === "\t" ? 4 : 1), 0);
      openList("itemize", indent);
      out.push(`\\item ${convertInline(unordered[2])}`);
      continue;
    }

    const ordered = line.match(/^([ \t]*)\d+[.)]\s+(.+)$/);
    if (ordered) {
      flushParagraph(out, paragraph);
      const indent = [...ordered[1]].reduce((sum, char) => sum + (char === "\t" ? 4 : 1), 0);
      openList("enumerate", indent);
      out.push(`\\item ${convertInline(ordered[2])}`);
      continue;
    }

    const quote = line.match(/^>\s*(.*)$/);
    if (quote) {
      flushParagraph(out, paragraph);
      closeList();
      out.push("\\begin{quote}", convertInline(quote[1]), "\\end{quote}", "");
      continue;
    }

    if (TODO_LINE_RE.test(line)) {
      flushParagraph(out, paragraph);
      closeList();
      continue;
    }

    paragraph.push(line);
  }

  flushParagraph(out, paragraph);
  closeList();
  if (inFence) throw new Error(`Unclosed Markdown code fence opened on line ${fenceLine}`);
  if (inDisplayMath) throw new Error(`Unclosed display math opened on line ${displayMathLine}`);
  if (envStack.length) throw new Error(`Unclosed Aaronnote block: #+begin ${envStack.at(-1)}`);

  return {
    meta,
    body: out.join("\n").replace(/\n{3,}/g, "\n\n").trim() + "\n",
  };
}

// Canonical name for the deterministic base conversion. `aaronnoteMarkdownToLatex`
// is kept as an alias for callers and tests that predate the mechanical/codex split.
export const mechanicalConvert = aaronnoteMarkdownToLatex;

export function applyLatexTemplate(template, vars) {
  const source = String(template || DEFAULT_TEMPLATE);
  const hasMacrosSlot = /\{\{\s*macros\s*\}\}/.test(source);
  let rendered = source.replace(/\{\{\s*([A-Za-z][\w-]*)\s*\}\}/g, (_m, key) => {
    return Object.prototype.hasOwnProperty.call(vars, key) ? String(vars[key] ?? "") : "";
  });
  const macros = String(vars?.macros || "").trim();
  if (macros && !hasMacrosSlot) {
    rendered = rendered.replace(/\\begin\{document\}/, `${macros}\n\n\\begin{document}`);
  }
  return rendered;
}

export function latexMacrosPreamble(macros) {
  const lines = ["% Aaronnote global math macros"];
  for (const [rawName, rawBody] of Object.entries(macros || {}).sort(([a], [b]) => a.localeCompare(b))) {
    if (!/^\\[A-Za-z@]+$/.test(rawName)) continue;
    const body = String(rawBody ?? "");
    let argc = 0;
    for (const match of body.matchAll(/#([1-9])/g)) argc = Math.max(argc, Number(match[1]));
    const args = argc ? `[${argc}]` : "";
    // `provide` then `renew` works for both new names and LaTeX built-ins such
    // as \C and \vec, while preserving the KaTeX macro set as authoritative.
    lines.push(`\\providecommand{${rawName}}${args}{}`);
    lines.push(`\\renewcommand{${rawName}}${args}{${body}}`);
  }
  return lines.length > 1 ? `${lines.join("\n")}\n` : "";
}

export async function readLatexTemplate(templatesRoot, templatePath = "") {
  const candidates = [];
  if (templatePath) candidates.push(resolve(templatePath));
  if (templatesRoot) {
    candidates.push(join(resolve(templatesRoot), "latex", "aaronnote-article.tex"));
    candidates.push(join(resolve(templatesRoot), "tex", "aaronnote-article.tex"));
  }
  for (const candidate of candidates) {
    try {
      return { file: candidate, text: await readFile(candidate, "utf8") };
    } catch {}
  }
  return { file: "", text: DEFAULT_TEMPLATE };
}

export function defaultLatexOutputPath(sourceFile, title = "") {
  const file = resolve(String(sourceFile || "aaronnote-export.md"));
  const ext = extname(file);
  if (ext) return file.slice(0, -ext.length) + ".tex";
  const clean = String(title || "aaronnote-export").trim().replace(/[^\p{L}\p{N}._-]+/gu, "-").replace(/^-+|-+$/g, "") || "aaronnote-export";
  return resolve(dirname(file), `${clean}.tex`);
}

export async function writeLatexExport(outputPath, latex) {
  const rawPath = String(outputPath || "").trim();
  if (!rawPath) throw new Error("Missing output path");
  const requested = resolve(rawPath);
  const file = requested.toLowerCase().endsWith(".tex") ? requested : `${requested}.tex`;
  await mkdir(dirname(file), { recursive: true });
  const temporary = join(dirname(file), `.${basename(file)}.${process.pid}.${Date.now()}.tmp`);
  try {
    await writeFile(temporary, latex, "utf8");
    await rename(temporary, file);
  } finally {
    await rm(temporary, { force: true }).catch(() => {});
  }
  return file;
}
