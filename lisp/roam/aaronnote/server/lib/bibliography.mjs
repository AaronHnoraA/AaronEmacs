import { readdir, readFile, stat } from "node:fs/promises";
import { realpathSync } from "node:fs";
import { basename, dirname, extname, isAbsolute, join, relative, resolve, sep } from "node:path";
import { createHash } from "node:crypto";
import { scanInlineCommands } from "../../shared/command-syntax.mjs";

const BIB_CACHE_LIMIT = 32;
const BIB_CACHE_BYTES = 16 * 1024 * 1024;
const SAFE_EXT = new Set([".md", ".markdown"]);
const INHERIT_FIELDS = new Set(["bib", "tags", "kind", "project", "source", "summary", "private", "css"]);
const META_BLOCK_RE = /^\s*#\+begin\s+meta\s*\r?\n([\s\S]*?)\r?\n\s*#\+end\s+meta\s*$/im;

let rootDir = "";
let version = 0;
const bibCache = new Map();
let bibCacheBytes = 0;

function canonicalPath(value) {
  const absolute = resolve(String(value || ""));
  try {
    return realpathSync.native(absolute);
  } catch {
    return absolute;
  }
}

export function configureBibliography(options = {}) {
  rootDir = canonicalPath(options.root);
  clearBibliographyCache();
}

export function clearBibliographyCache() {
  bibCache.clear();
  bibCacheBytes = 0;
  version += 1;
}

export function bibliographyVersion() {
  return version;
}

function inside(file, root) {
  const rel = relative(root, file);
  return rel === "" || (rel !== ".." && !rel.startsWith(`..${sep}`) && !isAbsolute(rel));
}

function rootRelative(file) {
  return relative(rootDir, file).replace(/\\/g, "/");
}

function parseMeta(content) {
  const match = String(content || "").match(META_BLOCK_RE);
  const out = {};
  if (!match) return out;
  for (const line of match[1].split(/\r?\n/)) {
    const m = line.match(/^\s*([A-Za-z0-9_-]+)\s*:\s*(.*?)\s*$/);
    if (m) out[m[1].toLowerCase()] = m[2].trim();
  }
  return out;
}

function splitList(value) {
  return String(value || "").split(",").map((item) => item.trim()).filter(Boolean);
}

async function readMetaFromFile(file) {
  try {
    const ext = extname(file).toLowerCase();
    if (!SAFE_EXT.has(ext) || !inside(file, rootDir)) return {};
    return parseMeta(await readFile(file, "utf8"));
  } catch {
    return {};
  }
}

async function effectiveMeta(file, content, seen = new Set()) {
  const current = parseMeta(content);
  const key = canonicalPath(file || rootDir);
  if (seen.has(key)) return { meta: current, diagnostics: [`extend cycle at ${rootRelative(key)}`] };
  seen.add(key);
  const diagnostics = [];
  const extend = String(current.extend || "").trim();
  let parent = {};
  if (extend && file) {
    const parentFile = canonicalPath(resolve(dirname(key), extend));
    if (!inside(parentFile, rootDir)) {
      diagnostics.push(`extend is outside Aaronnote root: ${extend}`);
    } else {
      parent = (await effectiveMeta(parentFile, await readFile(parentFile, "utf8").catch(() => ""), seen)).meta;
    }
  }
  const merged = {};
  for (const [k, v] of Object.entries(parent)) if (INHERIT_FIELDS.has(k)) merged[k] = v;
  for (const [k, v] of Object.entries(current)) {
    if (k === "bib" && merged.bib) merged.bib = `${v}, ${merged.bib}`;
    else merged[k] = v;
  }
  return { meta: merged, diagnostics };
}

async function visibleBibFiles(file, content) {
  const noteFile = canonicalPath(file || join(rootDir, "scratch.md"));
  const base = file ? dirname(noteFile) : rootDir;
  const { meta, diagnostics } = await effectiveMeta(noteFile, content);
  const dirs = splitList(meta.bib);
  const files = [];
  for (const raw of dirs) {
    const dir = canonicalPath(resolve(base, raw));
    if (!inside(dir, rootDir)) {
      diagnostics.push(`bib directory is outside Aaronnote root: ${raw}`);
      continue;
    }
    try {
      const entries = await readdir(dir, { withFileTypes: true });
      for (const entry of entries) {
        if (!entry.isFile() || extname(entry.name).toLowerCase() !== ".bib") continue;
        const abs = join(dir, entry.name);
        const full = rootRelative(abs).replace(/\.bib$/i, "");
        const localShort = basename(entry.name, ".bib");
        files.push({ file: abs, namespace: full, shortNamespace: localShort });
      }
    } catch {
      diagnostics.push(`bib directory not found: ${raw}`);
    }
  }
  return { files, diagnostics };
}

function skipSpaces(source, pos) {
  while (pos < source.length && /\s/.test(source[pos])) pos += 1;
  return pos;
}

function readBalanced(source, pos, open, close) {
  if (source[pos] !== open) return null;
  let depth = 0;
  let quote = "";
  for (let i = pos; i < source.length; i += 1) {
    const ch = source[i];
    if (quote) {
      if (ch === "\\" && i + 1 < source.length) i += 1;
      else if (ch === quote) quote = "";
      continue;
    }
    if (ch === '"') {
      quote = ch;
      continue;
    }
    if (ch === open) depth += 1;
    if (ch === close) {
      depth -= 1;
      if (depth === 0) return { text: source.slice(pos + 1, i), end: i + 1 };
    }
  }
  return null;
}

function readQuoted(source, pos) {
  const quote = source[pos];
  if (quote !== '"' && quote !== "'") return null;
  let out = "";
  for (let i = pos + 1; i < source.length; i += 1) {
    const ch = source[i];
    if (ch === "\\" && i + 1 < source.length) {
      out += ch + source[i + 1];
      i += 1;
      continue;
    }
    if (ch === quote) return { text: out, end: i + 1 };
    out += ch;
  }
  return null;
}

function cleanBibValue(value) {
  return String(value || "")
    .trim()
    .replace(/^["{]+|["}]+$/g, "")
    .replace(/[{}]/g, "")
    .replace(/\\&/g, "&")
    .replace(/\\_/g, "_")
    .replace(/\s+/g, " ")
    .trim();
}

function parseFields(body) {
  const comma = body.indexOf(",");
  if (comma < 0) return null;
  const key = body.slice(0, comma).trim();
  const fields = {};
  let pos = comma + 1;
  while (pos < body.length) {
    pos = skipSpaces(body, pos);
    if (body[pos] === ",") { pos += 1; continue; }
    const m = body.slice(pos).match(/^([A-Za-z][\w-]*)\s*=/);
    if (!m) break;
    const name = m[1].toLowerCase();
    pos += m[0].length;
    pos = skipSpaces(body, pos);
    let value = "";
    if (body[pos] === "{") {
      const parsed = readBalanced(body, pos, "{", "}");
      if (!parsed) break;
      value = parsed.text;
      pos = parsed.end;
    } else if (body[pos] === '"') {
      const parsed = readQuoted(body, pos);
      if (!parsed) break;
      value = parsed.text;
      pos = parsed.end;
    } else {
      const next = body.indexOf(",", pos);
      value = body.slice(pos, next < 0 ? body.length : next);
      pos = next < 0 ? body.length : next;
    }
    fields[name] = cleanBibValue(value);
  }
  return key ? { key, fields } : null;
}

function parseStringBody(body) {
  const m = String(body || "").match(/^\s*([A-Za-z][\w-]*)\s*=\s*([\s\S]+?)\s*$/);
  if (!m) return null;
  return { key: m[1].toLowerCase(), value: cleanBibValue(m[2]) };
}

export function parseBibTeX(source) {
  const entries = [];
  const diagnostics = [];
  const strings = new Map();
  const re = /@([A-Za-z]+)\s*[{(]/g;
  let match;
  while ((match = re.exec(source))) {
    const type = match[1].toLowerCase();
    const open = re.lastIndex - 1;
    const parsed = readBalanced(source, open, source[open], source[open] === "{" ? "}" : ")");
    if (!parsed) {
      diagnostics.push(`Unclosed BibTeX entry near offset ${match.index}`);
      continue;
    }
    re.lastIndex = parsed.end;
    if (type === "comment" || type === "preamble") continue;
    if (type === "string") {
      const string = parseStringBody(parsed.text);
      if (string) strings.set(string.key, string.value);
      continue;
    }
    const fields = parseFields(parsed.text);
    if (!fields) {
      diagnostics.push(`Invalid BibTeX entry near offset ${match.index}`);
      continue;
    }
    for (const [key, value] of Object.entries(fields.fields)) {
      const expanded = strings.get(String(value).toLowerCase());
      if (expanded) fields.fields[key] = expanded;
    }
    entries.push({ type, ...fields, raw: source.slice(match.index, parsed.end) });
  }
  return { entries, diagnostics };
}

async function readBibFile(file, namespace, shortNamespace) {
  const st = await stat(file);
  const cached = bibCache.get(file);
  if (cached && cached.mtimeMs === st.mtimeMs && cached.size === st.size) {
    cached.usedAt = Date.now();
    return cached.value;
  }
  const source = await readFile(file, "utf8");
  const parsed = parseBibTeX(source);
  const entries = parsed.entries.map((entry) => ({
    ...entry,
    namespace,
    shortNamespace,
    file,
    path: rootRelative(file),
    id: `${namespace}:${entry.key}`,
  }));
  const value = { file, path: rootRelative(file), namespace, shortNamespace, entries, diagnostics: parsed.diagnostics };
  const old = bibCache.get(file);
  if (old) bibCacheBytes -= old.size;
  bibCache.set(file, { mtimeMs: st.mtimeMs, size: st.size, usedAt: Date.now(), value });
  bibCacheBytes += st.size;
  while (bibCache.size > BIB_CACHE_LIMIT || bibCacheBytes > BIB_CACHE_BYTES) {
    const victim = [...bibCache.entries()].sort((a, b) => a[1].usedAt - b[1].usedAt)[0];
    if (!victim) break;
    bibCache.delete(victim[0]);
    bibCacheBytes -= victim[1].size;
  }
  return value;
}

function authors(value) {
  return cleanBibValue(value)
    .split(/\s+and\s+/i)
    .map((name) => name.includes(",")
      ? name.split(",").map((x) => x.trim()).filter(Boolean).reverse().join(" ")
      : name.trim())
    .filter(Boolean);
}

export function formatBibEntry(entry, index = 0) {
  const f = entry?.fields || {};
  const a = authors(f.author || f.editor || "");
  const names = a.length > 2 ? `${a[0]} et al.` : a.join(" and ");
  const title = f.title || entry.key || "";
  const venue = f.journaltitle || f.journal || f.booktitle || f.publisher || "";
  const year = f.year || f.date || "";
  const pages = f.pages ? `, pp. ${f.pages}` : "";
  const doi = f.doi ? ` DOI: ${f.doi}.` : "";
  const url = f.url ? ` ${f.url}` : "";
  const head = index > 0 ? `[${index}] ` : "";
  return `${head}${names ? `${names}. ` : ""}${title ? `"${title}." ` : ""}${venue ? `${venue}. ` : ""}${year}${pages}.${doi}${url}`.replace(/\s+/g, " ").trim();
}

function citeArgs(command) {
  const raw = String(command.argsRaw || "").trim().replace(/^\{|\}$/g, "");
  const args = {};
  for (const part of raw.split(";")) {
    const m = part.match(/^\s*([A-Za-z][\w-]*)\s*:\s*(.*?)\s*$/);
    if (m) args[m[1].toLowerCase()] = m[2];
  }
  return args;
}

function commandKeys(command) {
  return String(command.context || "").split(";").map((key) => key.trim()).filter(Boolean);
}

function namespaceMatches(files, namespace) {
  return files.filter((file) => file.namespace === namespace || file.shortNamespace === namespace);
}

export async function bibliographyForDocument({ file = "", content = "" } = {}) {
  if (!rootDir) return { ok: false, message: "Bibliography root is not configured", entries: [], references: [], citations: [] };
  const commands = scanInlineCommands(content, "cite");
  if (commands.length === 0) return { ok: true, version, entries: [], references: [], citations: [], namespaces: [] };
  const { files, diagnostics } = await visibleBibFiles(file, content);
  const parsed = await Promise.all(
    files.map((bib) => readBibFile(bib.file, bib.namespace, bib.shortNamespace)));
  const namespaceList = parsed.map((bib) => ({
    namespace: bib.namespace,
    shortNamespace: bib.shortNamespace,
    file: bib.path,
    entries: bib.entries.length,
  }));
  const numbered = new Map();
  const citations = [];
  for (const command of commands) {
    const ns = command.switchValue.trim();
    const keys = commandKeys(command);
    const args = citeArgs(command);
    const matches = namespaceMatches(parsed, ns);
    const cite = { from: command.fullFrom, to: command.fullTo, namespace: ns, keys, args, itemIds: [], numbers: [], diagnostics: [] };
    if (!ns) cite.diagnostics.push("citation namespace is required");
    if (matches.length === 0) cite.diagnostics.push(`unknown bibliography namespace: ${ns}`);
    if (matches.length > 1) cite.diagnostics.push(`ambiguous bibliography namespace: ${ns}`);
    for (const key of keys) {
      const found = matches.length === 1 ? matches[0].entries.filter((entry) => entry.key === key) : [];
      if (found.length !== 1) {
        cite.diagnostics.push(found.length > 1 ? `duplicate BibTeX key: ${key}` : `unknown BibTeX key: ${key}`);
        continue;
      }
      const entry = found[0];
      if (!numbered.has(entry.id)) numbered.set(entry.id, numbered.size + 1);
      cite.itemIds.push(entry.id);
      cite.numbers.push(numbered.get(entry.id));
    }
    citations.push(cite);
  }
  const byId = new Map(parsed.flatMap((bib) => bib.entries).map((entry) => [entry.id, entry]));
  const references = [...numbered.entries()].map(([id, number]) => {
    const entry = byId.get(id);
    return { id, number, entry, text: formatBibEntry(entry, number), links: bibLinks(entry) };
  });
  const hash = createHash("sha1").update(content).digest("hex").slice(0, 12);
  return { ok: true, version, hash, namespaces: namespaceList, entries: [], references, citations, diagnostics };
}

export async function bibliographyCompletions({ file = "", content = "", namespace = "", prefix = "", kind = "keys" } = {}) {
  const { files } = await visibleBibFiles(file, content);
  const parsed = await Promise.all(
    files.map((bib) => readBibFile(bib.file, bib.namespace, bib.shortNamespace)));
  const needle = String(prefix || "").toLowerCase();
  if (kind === "namespaces") {
    const seen = new Map();
    for (const bib of parsed) {
      seen.set(bib.shortNamespace, { key: bib.shortNamespace, name: bib.shortNamespace, body: bib.shortNamespace, detail: bib.path });
      seen.set(bib.namespace, { key: bib.namespace, name: bib.namespace, body: bib.namespace, detail: bib.path });
    }
    return { ok: true, items: [...seen.values()].filter((item) => !needle || item.key.toLowerCase().includes(needle)).slice(0, 24) };
  }
  const matches = namespaceMatches(parsed, namespace);
  const items = matches.flatMap((bib) => bib.entries).map((entry) => {
    const f = entry.fields || {};
    const detail = [authors(f.author || "").join(", "), f.year || f.date, f.title].filter(Boolean).join(" · ");
    return { key: entry.key, name: entry.key, body: entry.key, detail, source: entry.path };
  });
  return { ok: true, items: items.filter((item) => !needle || `${item.key} ${item.detail}`.toLowerCase().includes(needle)).slice(0, 24) };
}

function bibLinks(entry) {
  const f = entry?.fields || {};
  const links = [];
  if (f.doi) links.push({ label: "DOI", href: /^https?:\/\//i.test(f.doi) ? f.doi : `https://doi.org/${f.doi}` });
  if (f.url) links.push({ label: "URL", href: f.url });
  for (const key of ["zotero", "zoteroselect", "zotero_select", "zotero-link", "zotero_link"]) {
    if (f[key]) links.push({ label: "Zotero", href: f[key] });
  }
  if (f.file) links.push({ label: "file", href: f.file });
  return links;
}

export function bibliographyPathWatchRelevant(file) {
  return extname(String(file || "")).toLowerCase() === ".bib";
}
