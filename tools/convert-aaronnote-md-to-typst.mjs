#!/usr/bin/env node
import { promises as fs } from "node:fs";
import path from "node:path";

const ROOT = "/Users/hc/Documents/AaronNote";
const LANGLE = `upright("${String.fromCodePoint(0x27e8)}")`;
const RANGLE = `upright("${String.fromCodePoint(0x27e9)}")`;
const VBAR = 'upright("|")';
const TIMES = String.fromCodePoint(0x00d7);
const OTIMES = String.fromCodePoint(0x2297);
const OPLUS = String.fromCodePoint(0x2295);
const CONG = String.fromCodePoint(0x2245);
const SIM = String.fromCodePoint(0x223c);
const SIMEQ = String.fromCodePoint(0x2243);
const IN = String.fromCodePoint(0x2208);
const INFTY = String.fromCodePoint(0x221e);
const NOTIN = String.fromCodePoint(0x2209);
const SUBSET = String.fromCodePoint(0x2282);
const SUBSETEQ = String.fromCodePoint(0x2286);
const SETMINUS = String.fromCodePoint(0x2216);

function parseArgs(argv) {
  const args = { root: ROOT, outRoot: null, write: false };
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--root") args.root = argv[++i];
    else if (arg === "--out-root") args.outRoot = argv[++i];
    else if (arg === "--write") args.write = true;
    else throw new Error(`Unknown argument: ${arg}`);
  }
  return args;
}

async function walk(dir, root, out = []) {
  for (const entry of await fs.readdir(dir, { withFileTypes: true })) {
    const file = path.join(dir, entry.name);
    const rel = path.relative(root, file);
    if (rel.startsWith(".lean/") || rel.startsWith("_typst/")) continue;
    if (entry.isDirectory()) await walk(file, root, out);
    else if (entry.isFile() && entry.name.endsWith(".md")) out.push(file);
  }
  return out;
}

function parseMeta(lines) {
  const meta = {};
  let i = 0;
  if (lines[0]?.trim() === "#+begin meta") {
    i = 1;
    for (; i < lines.length; i++) {
      const line = lines[i];
      if (line.trim() === "#+end meta") {
        i++;
        break;
      }
      const m = /^([^:]+):\s*(.*)$/.exec(line);
      if (m) meta[m[1].trim()] = m[2].trim();
    }
  }
  return { meta, body: lines.slice(i) };
}

function typstString(value) {
  return `"${String(value ?? "").replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
}

function typstTags(value) {
  const tags = String(value ?? "")
    .split(",")
    .map((s) => s.trim())
    .filter(Boolean);
  if (tags.length === 0) return "()";
  return `(${tags.map(typstString).join(", ")},)`;
}

function typstDate(value) {
  const m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(String(value ?? ""));
  if (!m) return "none";
  return `datetime(year: ${Number(m[1])}, month: ${Number(m[2])}, day: ${Number(m[3])})`;
}

function escapeText(text) {
  return String(text)
    .replace(/\\/g, "\\\\")
    .replace(/#/g, "\\#")
    .replace(/\$/g, "\\$")
    .replace(/%/g, "\\%")
    .replace(/\[/g, "\\[")
    .replace(/\]/g, "\\]");
}

function convertLinkText(text) {
  return convertInline(text).replace(/^\s+|\s+$/g, "");
}

function convertInline(line) {
  const tokens = [];
  const pushToken = (value) => {
    const marker = `\u0000${tokens.length}\u0000`;
    tokens.push(value);
    return marker;
  };

  let s = line
    .replace(/==([^=]+)==/g, "**$1**")
    .replace(/\*\*([^*]+)\*\*/g, "*$1*");

  s = s.replace(/(?<!!)\[([^\]\n]+)\]\(([^)\n]+)\)/g, (_m, label, url) => {
    return pushToken(`#link(${typstString(url)})[${convertLinkText(label)}]`);
  });

  s = s.replace(/(?<!\$)\$([^$\n]+)\$(?!\$)/g, (_m, expr) => {
    return pushToken(`$${convertMath(expr)}$`);
  });

  s = escapeText(s);
  tokens.forEach((value, idx) => {
    s = s.replaceAll(escapeText(`\u0000${idx}\u0000`), value);
  });
  return s;
}

function takeBrace(s, start) {
  if (s[start] !== "{") return null;
  let depth = 0;
  for (let i = start; i < s.length; i++) {
    if (s[i] === "{" && s[i - 1] !== "\\") depth++;
    if (s[i] === "}" && s[i - 1] !== "\\") {
      depth--;
      if (depth === 0) return { value: s.slice(start + 1, i), end: i + 1 };
    }
  }
  return null;
}

function replaceCommand2(s, command, fn) {
  let out = "";
  for (let i = 0; i < s.length; ) {
    if (s.startsWith(command, i)) {
      const a = takeBrace(s, i + command.length);
      const b = a && takeBrace(s, a.end);
      if (a && b) {
        out += fn(a.value, b.value);
        i = b.end;
        continue;
      }
    }
    out += s[i++];
  }
  return out;
}

function replaceCommand1(s, command, fn) {
  let out = "";
  for (let i = 0; i < s.length; ) {
    if (s.startsWith(command, i)) {
      const a = takeBrace(s, i + command.length);
      if (a) {
        out += fn(a.value);
        i = a.end;
        continue;
      }
    }
    out += s[i++];
  }
  return out;
}

function convertMatrix(body) {
  const rows = body
    .split(/\\\\/)
    .map((row) => row.trim())
    .filter(Boolean)
    .map((row) =>
      row
        .split("&")
        .map((cell) => convertMath(cell.trim()))
        .join(", "),
    );
  return `mat(${rows.join("; ")})`;
}

function convertCases(body) {
  const rows = body
    .split(/\\\\/)
    .map((row) => row.trim())
    .filter(Boolean)
    .map((row) => {
      const cells = row
        .split("&")
        .map((cell) => replaceCommasOutsideStrings(convertMath(cell.trim()).replace(/[,.]+$/g, "")))
        .filter((cell) => cell.length > 0);
      return cells.join(", ");
    });
  return `mat(delim: "{", ${rows.join("; ")})`;
}

function normalizeScript(body) {
  const value = body.trim();
  const symbolWords = new Set([
    "alpha",
    "beta",
    "gamma",
    "delta",
    "Delta",
    "epsilon",
    "theta",
    "lambda",
    "mu",
    "rho",
    "psi",
    "phi",
    "ell",
    "dagger",
  ]);
  if (symbolWords.has(value)) return value;
  return value.replace(/\b([A-Za-z]{2,})\b/g, (word) => {
    if (symbolWords.has(word)) return word;
    return word.split("").join(" ");
  });
}

function escapeAngleCommas(value) {
  let out = "";
  let depth = 0;
  for (let i = 0; i < value.length; ) {
    if (value.startsWith(LANGLE, i)) {
      depth++;
      out += LANGLE;
      i += LANGLE.length;
      continue;
    }
    if (value.startsWith(RANGLE, i)) {
      depth = Math.max(0, depth - 1);
      out += RANGLE;
      i += RANGLE.length;
      continue;
    }
    if (value[i] === '"') {
      const start = i;
      i++;
      while (i < value.length) {
        if (value[i] === '"' && value[i - 1] !== "\\") {
          i++;
          break;
        }
        i++;
      }
      out += value.slice(start, i);
      continue;
    }
    if (value[i] === "," && depth > 0) {
      out += ' upright(",") ';
      i++;
      continue;
    }
    out += value[i++];
  }
  return out;
}

function replacePipesOutsideStrings(value) {
  let out = "";
  for (let i = 0; i < value.length; ) {
    if (value[i] === '"') {
      const start = i;
      i++;
      while (i < value.length) {
        if (value[i] === '"' && value[i - 1] !== "\\") {
          i++;
          break;
        }
        i++;
      }
      out += value.slice(start, i);
      continue;
    }
    if (value[i] === "|") {
      out += ` ${VBAR} `;
      i++;
      continue;
    }
    out += value[i++];
  }
  return out;
}

function replaceCommasOutsideStrings(value) {
  let out = "";
  for (let i = 0; i < value.length; ) {
    if (value[i] === '"') {
      const start = i;
      i++;
      while (i < value.length) {
        if (value[i] === '"' && value[i - 1] !== "\\") {
          i++;
          break;
        }
        i++;
      }
      out += value.slice(start, i);
      continue;
    }
    if (value[i] === ",") {
      out += ' upright(",") ';
      i++;
      continue;
    }
    out += value[i++];
  }
  return out;
}

function convertMath(input) {
  let s = String(input ?? "").trim();

  s = s.replace(/\\begin\{(?:p|b|B|v|V)?matrix\*?\}(?:\[[^\]]+\])?([\s\S]*?)\\end\{(?:p|b|B|v|V)?matrix\*?\}/g, (_m, body) =>
    convertMatrix(body),
  );
  s = s.replace(/\\begin\{cases\}([\s\S]*?)\\end\{cases\}/g, (_m, body) => convertCases(body));
  s = s.replace(/\\\|([\s\S]*?)\\\|/g, (_m, body) => `norm(${convertMath(body)})`);

  s = replaceCommand2(s, "\\frac", (a, b) => `(${convertMath(a)})/(${convertMath(b)})`);
  s = s.replace(/\\sqrt\[([^\]]+)\]\{([^{}]+)\}/g, (_m, root, body) => `root(${convertMath(root)}, ${convertMath(body)})`);
  s = replaceCommand1(s, "\\sqrt", (a) => `sqrt(${convertMath(a)})`);

  const alphabet = {
    R: "RR",
    C: "CC",
    N: "NN",
    Z: "ZZ",
    Q: "QQ",
    F: "FF",
    P: "PP",
  };
  s = replaceCommand1(s, "\\mathbb", (a) => alphabet[a.trim()] || `bb(${a.trim()})`);
  s = replaceCommand1(s, "\\mathcal", (a) => `cal(${a.trim()})`);
  s = replaceCommand1(s, "\\operatorname", (a) => {
    const op = a.trim();
    return op === "tr" ? op : `upright(${JSON.stringify(op)})`;
  });
  s = replaceCommand1(s, "\\mathrm", (a) => `upright(${JSON.stringify(a)})`);
  s = replaceCommand1(s, "\\text", (a) => `text(${JSON.stringify(a.replace(/\\,/g, " "))})`);
  s = replaceCommand1(s, "\\hat", (a) => `hat(${convertMath(a)})`);
  s = replaceCommand1(s, "\\tilde", (a) => `tilde(${convertMath(a)})`);
  s = replaceCommand1(s, "\\overline", (a) => `overline(${convertMath(a)})`);
  s = replaceCommand1(s, "\\bar", (a) => `overline(${convertMath(a)})`);

  const commands = new Map([
    ["\\langle", ` ${LANGLE} `],
    ["\\rangle", ` ${RANGLE}`],
    ["\\mid", " | "],
    ["\\vert", " | "],
    ["\\lvert", " | "],
    ["\\rvert", " | "],
    ["\\|", "norm"],
    ["\\dagger", "dagger"],
    ["\\alpha", " alpha "],
    ["\\beta", " beta "],
    ["\\gamma", " gamma "],
    ["\\delta", " delta "],
    ["\\Delta", " Delta "],
    ["\\epsilon", " epsilon "],
    ["\\varepsilon", " epsilon "],
    ["\\theta", " theta "],
    ["\\lambda", " lambda "],
    ["\\mu", " mu "],
    ["\\rho", " rho "],
    ["\\psi", " psi "],
    ["\\phi", " phi "],
    ["\\ell", " ell "],
    ["\\times", ` ${TIMES} `],
    ["\\otimes", ` ${OTIMES} `],
    ["\\oplus", ` ${OPLUS} `],
    ["\\cong", ` ${CONG} `],
    ["\\sim", ` ${SIM} `],
    ["\\simeq", ` ${SIMEQ} `],
    ["\\infty", ` ${INFTY} `],
    ["\\in", ` ${IN} `],
    ["\\notin", ` ${NOTIN} `],
    ["\\subseteq", ` ${SUBSETEQ} `],
    ["\\subset", ` ${SUBSET} `],
    ["\\setminus", ` ${SETMINUS} `],
    ["\\geq", ">="],
    ["\\ge", ">="],
    ["\\leq", "<="],
    ["\\le", "<="],
    ["\\neq", "!="],
    ["\\ne", "!="],
    ["\\to", " -> "],
    ["\\rightarrow", " -> "],
    ["\\Longrightarrow", " ==> "],
    ["\\Longleftrightarrow", " <==> "],
    ["\\Rightarrow", " => "],
    ["\\Leftrightarrow", " <=> "],
    ["\\leftrightarrow", " <-> "],
    ["\\leadsto", " arrow.r.squiggly "],
    ["\\forall", "forall "],
    ["\\exists", "exists "],
    ["\\sum", "sum"],
    ["\\int", "integral"],
    ["\\qquad", "quad"],
    ["\\quad", " quad "],
    ["\\,", " "],
    ["\\ldots", "dots"],
    ["\\dots", "dots"],
    ["\\cdot", "dot"],
    ["\\cdots", "dots.c"],
    ["\\ddots", "dots.down"],
    ["\\Pr", "Pr"],
  ]);

  s = s.replace(/\\left|\\right|\\middle/g, "");
  s = s.replace(/\\\\/g, "; ");
  s = s.replace(/\\([{}])/g, "$1");
  s = s.replace(/†/g, "dagger");
  s = s.replace(/∈/g, " in ");
  s = s.replace(/≤/g, "<=");
  s = s.replace(/≥/g, ">=");
  s = s.replace(/≠/g, "!=");
  s = s.replace(/\\=/g, "=");

  for (const [from, to] of [...commands.entries()].sort((a, b) => b[0].length - a[0].length)) {
    s = s.replaceAll(from, to);
  }

  s = replacePipesOutsideStrings(s);
  s = escapeAngleCommas(s);
  s = s.replace(/_\{([^{}]+)\}/g, (_m, body) => `_(${normalizeScript(body)})`);
  s = s.replace(/\^\{([^{}]+)\}/g, (_m, body) => `^(${normalizeScript(body)})`);
  s = s.replace(/\baP_/g, "a P_");
  s = s.replace(/\bdE_/g, "d E_");
  s = s.replace(/\bab\b/g, "a b");
  s = s.replace(/\b([A-Z])([A-Z])\b/g, (word, a, b) => {
    if (["RR", "CC", "NN", "ZZ", "QQ", "FF", "PP"].includes(word)) return word;
    return `${a} ${b}`;
  });
  s = s.replace(/\b([A-Z])([a-z])\b/g, (word, a, b) => {
    if (word === "Pr") return word;
    return `${a} ${b}`;
  });
  s = s.replace(/\\([A-Za-z]+)/g, "$1");
  s = s.replace(/\s+/g, " ").trim();
  return s;
}

function blockStart(kind, arg) {
  const label = arg?.trim();
  const nameArg = label ? `(name: ${typstString(label)})` : "";
  switch (kind) {
    case "define":
    case "definition":
      return `#definition${nameArg}[`;
    case "theorem":
      return `#theorem${nameArg}[`;
    case "lemma":
      return `#lemma${nameArg}[`;
    case "corollary":
      return `#corollary${nameArg}[`;
    case "proposition":
      return `#proposition${nameArg}[`;
    case "proof":
      return label ? `#proof[*${escapeText(label)}*\n\n` : "#proof[";
    case "example":
      return label ? `#example[*${escapeText(label)}*\n\n` : "#example[";
    case "todo":
      return label ? `#todo[*${escapeText(label)}*\n\n` : "#todo[";
    case "summary":
    case "note":
    case "warning":
    case "important":
    case "attention": {
      const title = `${kind[0].toUpperCase()}${kind.slice(1)}${label ? `: ${label}` : ""}`;
      return `#remark[*${escapeText(title)}*\n\n`;
    }
    default:
      return `#remark[*${escapeText(kind + (label ? `: ${label}` : ""))}*\n\n`;
  }
}

function convertImage(line) {
  const m = /^!\[([^\]]*)\]\(([^)]+)\)\s*$/.exec(line.trim());
  if (!m) return null;
  const alt = m[1] || path.basename(m[2]);
  return `#figure(image(${typstString(m[2])}, width: 80%), caption: [${convertInline(alt)}])`;
}

function convertBody(lines) {
  const out = [];
  let inMath = false;
  let math = [];
  let inFence = false;
  let fence = [];

  const flushMath = () => {
    out.push("$");
    out.push(convertMath(math.join("\n")));
    out.push("$");
    math = [];
  };
  const flushFence = () => {
    out.push(...fence);
    fence = [];
  };

  for (let i = 0; i < lines.length; i++) {
    const raw = lines[i];
    const trimmed = raw.trim();

    if (inMath) {
      if (trimmed === "$$") {
        inMath = false;
        flushMath();
      } else {
        math.push(raw);
      }
      continue;
    }

    if (inFence) {
      fence.push(raw);
      if (trimmed.startsWith("```")) {
        inFence = false;
        flushFence();
      }
      continue;
    }

    if (trimmed === "$$") {
      inMath = true;
      math = [];
      continue;
    }

    if (trimmed.startsWith("```")) {
      inFence = true;
      fence = [raw];
      continue;
    }

    if (/^\|.*\|\s*$/.test(raw)) {
      const table = [];
      while (i < lines.length && /^\|.*\|\s*$/.test(lines[i])) table.push(lines[i++]);
      i--;
      out.push("```markdown");
      out.push(...table);
      out.push("```");
      continue;
    }

    if (/^\{[^}]+:\s*.*\}$/.test(trimmed)) {
      out.push(`// ${trimmed}`);
      continue;
    }

    const begin = /^#\+begin\s+([A-Za-z0-9_-]+)\s*(.*)$/.exec(trimmed);
    if (begin) {
      out.push(blockStart(begin[1].toLowerCase(), begin[2]));
      continue;
    }

    if (/^#\+end\s+/.test(trimmed)) {
      out.push("]");
      continue;
    }

    const image = convertImage(raw);
    if (image) {
      out.push(image);
      continue;
    }

    if (/^---+\s*$/.test(trimmed)) {
      out.push("#line(length: 100%)");
      continue;
    }

    const leanPlaceholder = /^@@lean4(?:\(([^)]*)\))?\s+\[([^\]]+)\]\s*$/.exec(trimmed);
    if (leanPlaceholder) {
      const selector = (leanPlaceholder[1] || "").trim();
      const tag = leanPlaceholder[2].trim();
      const pathArg = selector ? `, path: ${typstString(selector)}` : "";
      out.push(`#note-code(lang: lean${pathArg})[${tag}]`);
      continue;
    }

    const todo = /^@@todo(?:\(([^)]+)\))?\s*\[([^\]]*)\]/.exec(trimmed);
    if (todo) {
      const status = todo[1] ? `${todo[1]}: ` : "";
      out.push(`#todo[${convertInline(status + todo[2])}]`);
      continue;
    }

    if (/^>\s?/.test(raw)) {
      const quote = [];
      while (i < lines.length && /^>\s?/.test(lines[i])) quote.push(lines[i++].replace(/^>\s?/, ""));
      i--;
      out.push("#remark[*Quote.*");
      out.push("");
      out.push(...quote.map(convertInline));
      out.push("]");
      continue;
    }

    const heading = /^(#{1,6})\s+(.*)$/.exec(raw);
    if (heading) {
      out.push(`${"=".repeat(heading[1].length)} ${convertInline(heading[2])}`);
      continue;
    }

    const numbered = /^(\s*)\d+\.\s+(.*)$/.exec(raw);
    if (numbered) {
      out.push(`${numbered[1]}+ ${convertInline(numbered[2])}`);
      continue;
    }

    const task = /^(\s*)-\s*\[\s*\]\s*(.*)$/.exec(raw);
    if (task) {
      out.push(`${task[1]}- TODO ${convertInline(task[2])}`);
      continue;
    }

    const bullet = /^(\s*)-\s+(.*)$/.exec(raw);
    if (bullet) {
      out.push(`${bullet[1]}- ${convertInline(bullet[2])}`);
      continue;
    }

    out.push(convertInline(raw));
  }

  if (inMath) flushMath();
  if (inFence) flushFence();
  return out.join("\n").replace(/\n{4,}/g, "\n\n\n").trim() + "\n";
}

function convertFile(source, root, file) {
  const lines = source.split(/\r?\n/);
  const { meta, body } = parseMeta(lines);
  const title = meta.title || "Untitled";
  const id = meta.id || title;
  const relSource = path.relative(root, file);
  const sourceLabel = meta.source || relSource;
  const header = [
    '#import "/_typst/roam.typ": *',
    `#let note-code = note-code.with(note-path: ${typstString(relSource.replace(/\.md$/, ".typ"))})`,
    "#show: note.with(",
    `  id: ${typstString(id)},`,
    `  title: ${typstString(title)},`,
    `  tags: ${typstTags(meta.tags)},`,
    `  created: ${typstDate(meta.date)},`,
    ")",
    "",
    `// source: ${sourceLabel}`,
    "",
  ];
  const converted = convertBody(body);
  const noteStem = relSource.replace(/\.md$/, "");
  const leanDir = path.posix.dirname(`/.lean/${noteStem}.lean`);
  return header.join("\n") + converted.replace(
    /#note-code\(lang: lean, path: "([^"]+)"\)/g,
    (_match, selector) => {
      const newfile = /^newfile(?::(\d+))?$/i.exec(selector);
      const resolved = newfile
        ? `/.lean/${noteStem}.mirror-${Number(newfile[1] || 1)}.lean`
        : path.posix.normalize(path.posix.join(
          leanDir,
          path.posix.extname(selector) ? selector : `${selector}.lean`,
        ));
      return `#note-code(lang: lean, path: ${typstString(resolved)})`;
    },
  );
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const root = path.resolve(args.root);
  const outRoot = args.outRoot ? path.resolve(args.outRoot) : root;
  const files = await walk(root, root);
  const written = [];
  for (const file of files) {
    const rel = path.relative(root, file);
    const outRel = rel.replace(/\.md$/, ".typ");
    const outPath = path.join(outRoot, outRel);
    const source = await fs.readFile(file, "utf8");
    const converted = convertFile(source, root, file);
    await fs.mkdir(path.dirname(outPath), { recursive: true });
    if (args.write || args.outRoot) await fs.writeFile(outPath, converted, "utf8");
    written.push(outPath);
  }
  console.log(`converted ${written.length} file(s)`);
  for (const file of written) console.log(file);
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
