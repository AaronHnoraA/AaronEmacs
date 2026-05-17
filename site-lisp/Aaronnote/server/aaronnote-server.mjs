#!/usr/bin/env node
import { createServer as createHttpServer } from "node:http";
import { mkdir, mkdtemp, readdir, readFile, rm, stat, writeFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { dirname, isAbsolute, join, relative, resolve, sep } from "node:path";
import { homedir, tmpdir } from "node:os";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";

import { createServer as createViteServer } from "vite";

const appDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const workspaceRoot = resolve(appDir, "..");
const publishJsDir = join(workspaceRoot, "js");
const execFileAsync = promisify(execFile);

function parseArgs(argv) {
  const out = {};
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (!arg.startsWith("--")) continue;
    const key = arg.slice(2);
    const next = argv[i + 1];
    if (!next || next.startsWith("--")) out[key] = true;
    else {
      out[key] = next;
      i++;
    }
  }
  return out;
}

const args = parseArgs(process.argv.slice(2));
const host = String(args.host || process.env.AARONNOTE_HOST || "127.0.0.1");
const port = Number(args.port || process.env.AARONNOTE_PORT || 5179);
const noteRoot = resolve(String(args.root || process.env.AARONNOTE_ROOT || join(appDir, "..", "roam")));
const noteScanRoot = noteRoot;
const excludedDirs = new Set(["_typst", "public", "var", ".git", ".direnv", ".venv", "node_modules"]);
const noteExts = new Set([".typ", ".md", ".markdown"]);

function sendJson(res, statusCode, value) {
  const data = JSON.stringify(value);
  res.writeHead(statusCode, {
    "content-type": "application/json; charset=utf-8",
    "content-length": Buffer.byteLength(data),
  });
  res.end(data);
}

async function sendTextFile(res, file, contentType) {
  const data = await readFile(file);
  res.writeHead(200, {
    "content-type": contentType,
    "content-length": data.length,
  });
  res.end(data);
}

function inside(child, parent) {
  const rel = relative(parent, child);
  return rel === "" || (!rel.startsWith("..") && !isAbsolute(rel));
}

function safeFile(input) {
  const file = resolve(String(input || ""));
  if (!inside(file, noteRoot)) {
    const err = new Error(`File is outside note root: ${file}`);
    err.statusCode = 403;
    throw err;
  }
  return file;
}

function modeForFile(file) {
  const lower = file.toLowerCase();
  return lower.endsWith(".md") || lower.endsWith(".markdown") ? "markdown" : "source";
}

function parseListValue(value) {
  const trimmed = String(value || "").trim();
  if (!trimmed) return [];
  if (trimmed.startsWith("(")) {
    return [...trimmed.matchAll(/"((?:[^"\\]|\\.)*)"/g)]
      .map((match) => match[1].replace(/\\"/g, '"').replace(/\\\\/g, "\\"))
      .filter(Boolean);
  }
  return trimmed.split(/[, ]+/).map((item) => item.trim()).filter(Boolean);
}

function parseMetaBlock(content) {
  const match = content.match(/^\s*#\+begin\s+meta\s*\r?\n([\s\S]*?)\r?\n\s*#\+end\s+meta\s*$/im);
  if (!match) return {};
  const meta = {};
  for (const rawLine of match[1].split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line || line.startsWith("#")) continue;
    const pair = line.match(/^([A-Za-z0-9_-]+)\s*:\s*(.*)$/);
    if (!pair) continue;
    const key = pair[1].toLowerCase();
    let value = pair[2].trim();
    if ((value.startsWith('"') && value.endsWith('"')) || (value.startsWith("'") && value.endsWith("'"))) {
      value = value.slice(1, -1);
    }
    if (key === "tags" || key === "refs" || key === "aliases") {
      meta[key] = parseListValue(value);
    } else {
      meta[key] = value;
    }
  }
  return meta;
}

function yamlishValue(content, key) {
  return content.match(new RegExp(`^\\s*${key}:\\s*"([^"]+)"`, "m"))?.[1]
    || content.match(new RegExp(`^\\s*${key}:\\s*([^\\n]+)`, "m"))?.[1]?.trim();
}

function typstUnescape(value) {
  return String(value || "")
    .replace(/\\n/g, "\n")
    .replace(/\\t/g, "\t")
    .replace(/\\"/g, '"')
    .replace(/\\\\/g, "\\");
}

function parseTypstMetadata(content) {
  const match = content.match(/#metadata\s*\(\(([\s\S]*?)\)\)\s*<note>/m);
  if (!match) return {};
  const body = match[1];
  const fields = {};
  const pairs = [...body.matchAll(/([A-Za-z0-9_-]+)\s*:\s*/g)];
  for (let i = 0; i < pairs.length; i++) {
    const key = pairs[i][1].toLowerCase();
    const start = pairs[i].index + pairs[i][0].length;
    const end = i + 1 < pairs.length ? pairs[i + 1].index : body.length;
    const raw = body.slice(start, end).trim().replace(/,\s*$/, "").trim();
    if (!raw) continue;
    if (raw.startsWith("(")) {
      fields[key] = [...raw.matchAll(/"((?:[^"\\]|\\.)*)"/g)].map((item) => typstUnescape(item[1]));
    } else if (raw === "true" || raw === "false") {
      fields[key] = raw === "true";
    } else {
      const string = raw.match(/"((?:[^"\\]|\\.)*)"/);
      fields[key] = string ? typstUnescape(string[1]) : raw;
    }
  }
  return fields;
}

function noteMetadata(content) {
  return {
    ...parseTypstMetadata(content),
    ...parseMetaBlock(content),
  };
}

function desktopExportPath(file) {
  const raw = file ? file.split(sep).pop() || "Aaronnote.md" : "Aaronnote.md";
  const safe = raw.replace(/[/:]/g, "-") || "Aaronnote.md";
  return join(homedir(), "Desktop", safe);
}

function pdfExportName(file) {
  const raw = file ? file.split(sep).pop() || "Aaronnote.pdf" : "Aaronnote.pdf";
  const stem = raw.replace(/\.[^.]+$/, "") || "Aaronnote";
  return `${stem}.pdf`.replace(/[/:]/g, "-");
}

function slugifyTitle(title) {
  const slug = String(title || "untitled")
    .normalize("NFKD")
    .replace(/[^\w\s-]/g, "")
    .trim()
    .replace(/\s+/g, "-")
    .replace(/-+/g, "-")
    .toLowerCase();
  return slug || "untitled";
}

function timestampId() {
  const now = new Date();
  const pad = (value) => String(value).padStart(2, "0");
  return [
    now.getFullYear(),
    pad(now.getMonth() + 1),
    pad(now.getDate()),
    "T",
    pad(now.getHours()),
    pad(now.getMinutes()),
    pad(now.getSeconds()),
  ].join("");
}

function markdownForPdf(content) {
  return String(content ?? "")
    .replace(/^\s*#\+begin\s+meta\s*\n[\s\S]*?\n\s*\\?#\+end\s+meta\s*\n*/i, "")
    .replace(/^#\+begin\s+([A-Za-z][\w-]*)(?:\s+([^\n]+))?\s*$/gmi, (_m, kind, title = "") => {
      const label = String(kind).toLowerCase() === "summary" ? "Summary" : String(kind);
      return `::: {.${String(kind).toLowerCase()}}\n**${label}${title ? `: ${title}` : ""}.**`;
    })
    .replace(/^\\?#\+end\s+[A-Za-z][\w-]*\s*$/gmi, ":::");
}

async function exportPdf(file, content) {
  const dir = await mkdtemp(join(tmpdir(), "aaronnote-pdf-"));
  const input = join(dir, "input.md");
  const out = join(dir, "output.pdf");
  await writeFile(input, markdownForPdf(content), "utf8");
  try {
    await execFileAsync("pandoc", [
      input,
      "--from=markdown+tex_math_dollars+fenced_divs",
      "--pdf-engine=xelatex",
      "-V", "mainfont=Times New Roman",
      "-V", "CJKmainfont=FZLiuGongQuanKaiShuJF",
      "-V", "mathfont=GFS Neohellenic Math",
      "-V", "geometry:margin=1in",
      "-o", out,
    ], {
      cwd: noteRoot,
      maxBuffer: 1024 * 1024 * 8,
    });
    return {
      name: pdfExportName(file),
      data: await readFile(out),
    };
  } catch (err) {
    const message = [err.message, err.stderr, err.stdout].filter(Boolean).join("\n");
    const next = new Error(message || "PDF export failed");
    next.statusCode = 500;
    throw next;
  }
  finally {
    await rm(dir, { recursive: true, force: true });
  }
}

function titleFromContent(file, content) {
  const meta = noteMetadata(content);
  if (meta.title) return String(meta.title);
  const typTitle = yamlishValue(content, "title");
  if (typTitle) return typTitle;
  const typHeading = content.match(/^=+\s+(.+)$/m)?.[1]?.trim();
  if (typHeading) return typHeading;
  const mdHeading = content.match(/^#+\s+(.+)$/m)?.[1]?.trim();
  if (mdHeading) return mdHeading;
  return file.split(sep).pop()?.replace(/\.[^.]+$/, "") || "Untitled";
}

function idFromContent(file, root, content) {
  const meta = noteMetadata(content);
  return meta.id || yamlishValue(content, "id") || relative(root, file);
}

function tagsFromContent(content) {
  const meta = noteMetadata(content);
  if (Array.isArray(meta.tags)) return meta.tags;
  const tags = [];
  const lines = content.split(/\r?\n/);
  const start = lines.findIndex((line) => /^\s*tags:\s*$/.test(line));
  if (start >= 0) {
    for (const line of lines.slice(start + 1)) {
      const item = line.match(/^\s*-\s*(.+)$/);
      if (!item) break;
      tags.push(item[1].trim());
    }
  }
  return tags;
}

function refsFromContent(content) {
  const meta = noteMetadata(content);
  const refs = new Set(Array.isArray(meta.refs) ? meta.refs : []);
  for (const match of content.matchAll(/#note\("([^"]+)"\)/g)) refs.add(match[1]);
  for (const match of content.matchAll(/\[\[([^\]\n]+)\]\]/g)) refs.add(match[1].trim());
  for (const match of content.matchAll(/\]\(([^)\n]+\.md)(?:#[^)]*)?\)/g)) {
    refs.add(decodeURIComponent(match[1]));
  }
  return [...refs].filter(Boolean);
}

function aliasesFromContent(content) {
  const meta = noteMetadata(content);
  return Array.isArray(meta.aliases) ? meta.aliases : [];
}

function dateFromContent(content) {
  const meta = noteMetadata(content);
  return String(meta.date || yamlishValue(content, "date") || "");
}

function sourceFromContent(content) {
  const meta = noteMetadata(content);
  return String(meta.source || "");
}

function kindFromContent(content) {
  const meta = noteMetadata(content);
  return String(meta.kind || "note");
}

function summaryFromContent(content) {
  const meta = noteMetadata(content);
  if (meta.summary) return String(meta.summary);
  const withoutMeta = content
    .replace(/^\s*#\+begin\s+meta\s*\r?\n[\s\S]*?\r?\n\s*#\+end\s+meta\s*\r?\n*/im, "")
    .replace(/#metadata\s*\(\([\s\S]*?\)\)\s*<note>/m, "")
    .replace(/^#(?:import|show|set)[^\n]*$/gm, "")
    .replace(/#note\("([^"]+)"\)\[([^\]]+)\]/g, "$2")
    .replace(/^=+\s+/gm, "")
    .replace(/^#+\s+/gm, "")
    .replace(/[#*_`$()[\]{}]/g, " ");
  return withoutMeta.split(/\s+/).filter(Boolean).join(" ").slice(0, 220);
}

function groupKeyFor(file) {
  const parent = dirname(relative(noteScanRoot, file));
  return parent === "." ? "Root" : parent;
}

function groupLabelFor(groupKey) {
  if (!groupKey || groupKey === "Root") return "Root";
  const leaf = groupKey.split(sep).filter(Boolean).at(-1) || groupKey;
  return leaf.toUpperCase() === leaf ? leaf : leaf.replace(/[-_]/g, " ").replace(/\b\w/g, (char) => char.toUpperCase());
}

function preferNote(candidate, current) {
  if (!current) return candidate;
  if (candidate.ext === "md" && current.ext !== "md") return candidate;
  if (candidate.path && candidate.path === current.source) return candidate;
  return current;
}

async function walkFiles(root, accept) {
  const files = [];
  async function walk(dir) {
    let entries = [];
    try {
      entries = await readdir(dir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const entry of entries) {
      if (entry.name.startsWith(".") && entry.name !== ".emacs.d") continue;
      const full = join(dir, entry.name);
      if (entry.isDirectory()) {
        if (!excludedDirs.has(entry.name)) await walk(full);
      } else if (entry.isFile() && accept(full, entry.name)) {
        files.push(full);
      }
    }
  }
  await walk(root);
  return files;
}

async function scanNotes() {
  const files = await walkFiles(noteScanRoot, (file) => {
    const dot = file.lastIndexOf(".");
    return dot >= 0 && noteExts.has(file.slice(dot).toLowerCase());
  });
  const notes = [];
  for (const file of files) {
    try {
      const content = await readFile(file, "utf8");
      const relPath = relative(noteScanRoot, file);
      const groupKey = groupKeyFor(file);
      const id = idFromContent(file, noteScanRoot, content);
      notes.push({
        key: id,
        id,
        title: titleFromContent(file, content),
        file,
        link: relPath,
        path: relPath,
        ext: file.slice(file.lastIndexOf(".") + 1).toLowerCase(),
        kind: kindFromContent(content),
        date: dateFromContent(content),
        groupKey,
        groupLabel: groupLabelFor(groupKey),
        section: groupKey.includes(sep) ? groupKey.split(sep)[0] : groupKey,
        source: sourceFromContent(content),
        aliases: aliasesFromContent(content),
        summary: summaryFromContent(content),
        tags: tagsFromContent(content),
        refs: refsFromContent(content),
        backlinks: [],
      });
    } catch {}
  }
  const uniqueNotes = [...notes.reduce((map, note) => {
    map.set(note.id, preferNote(note, map.get(note.id)));
    return map;
  }, new Map()).values()];
  const byId = new Map(uniqueNotes.map((note) => [note.id, note]));
  const byRel = new Map(uniqueNotes.map((note) => [relative(noteScanRoot, note.file), note]));
  const byBase = new Map(uniqueNotes.map((note) => [note.file.split(sep).pop(), note]));
  const bySource = new Map(uniqueNotes.filter((note) => note.source).map((note) => [note.source, note]));
  for (const note of uniqueNotes) {
    const resolved = [];
    for (const ref of note.refs || []) {
      const target = byId.get(ref) || byRel.get(ref) || byBase.get(ref) || bySource.get(ref) || byRel.get(ref.replace(/^\.\//, ""));
      if (!target || target.id === note.id) continue;
      resolved.push(target.id);
      target.backlinks.push(note.id);
    }
    note.refs = [...new Set(resolved)].sort();
  }
  for (const note of uniqueNotes) note.backlinks = [...new Set(note.backlinks)].sort();
  return uniqueNotes.sort((a, b) => a.title.localeCompare(b.title));
}

function snippetDirs() {
  const raw = process.env.AARONNOTE_SNIPPETS;
  const dirs = raw
    ? raw.split(":").filter(Boolean)
    : [
        join(appDir, "snippets"),
      ];

  return [...new Set(dirs.map((dir) => resolve(dir)).filter((dir) => existsSync(dir)))];
}

function parseSnippetBody(content) {
  const lines = content.split(/\r?\n/);
  const headers = new Map();
  let bodyStart = 0;
  for (let i = 0; i < lines.length; i++) {
    const marker = lines[i].match(/^# --\s*$/);
    if (marker) {
      bodyStart = i + 1;
      break;
    }
    const header = lines[i].match(/^#\s*([^:\n]+):\s*(.*)$/);
    if (header) headers.set(header[1].trim().toLowerCase(), header[2].trim());
  }
  return {
    headers,
    body: lines.slice(bodyStart).join("\n").replace(/\s+$/, ""),
  };
}

async function scanSnippets() {
  const snippets = [];
  for (const root of snippetDirs()) {
    const files = await walkFiles(root, (_file, name) => !name.startsWith(".") && !name.endsWith(".el"));
    for (const file of files) {
      try {
        const content = await readFile(file, "utf8");
        const { headers, body } = parseSnippetBody(content);
        if (!body.trim()) continue;
        const rel = relative(root, file);
        const parts = rel.split(sep);
        const mode = parts[0] || "";
        const key = headers.get("key") || parts.at(-1) || "snippet";
        snippets.push({
          key,
          name: headers.get("name") || key,
          mode,
          group: headers.get("group") || "",
          body,
          source: file,
        });
      } catch {}
    }
  }
  return snippets.sort((a, b) => `${a.mode}/${a.key}`.localeCompare(`${b.mode}/${b.key}`));
}

async function readNote(file) {
  const safe = safeFile(file);
  const info = await stat(safe);
  if (!info.isFile()) {
    const err = new Error(`Not a regular file: ${safe}`);
    err.statusCode = 400;
    throw err;
  }
  const content = await readFile(safe, "utf8");
  return {
    type: "open",
    file: safe,
    title: titleFromContent(safe, content),
    mode: modeForFile(safe),
    content,
    notes: await scanNotes(),
    snippets: await scanSnippets(),
  };
}

async function createNode(body) {
  const title = String(body.title || "Untitled").trim() || "Untitled";
  const id = String(body.id || `${timestampId()}-${slugifyTitle(title)}`).trim();
  const tags = Array.isArray(body.tags) ? body.tags.map(String).filter(Boolean) : [];
  const dir = resolve(noteRoot, String(body.directory || "."));
  if (!inside(dir, noteRoot)) {
    const err = new Error(`Directory is outside note root: ${dir}`);
    err.statusCode = 403;
    throw err;
  }
  await mkdir(dir, { recursive: true });
  await writeFile(join(dir, ".aaronnote-keep"), "", { flag: "a" }).catch(() => {});
  const file = join(dir, `${slugifyTitle(id)}.md`);
  if (existsSync(file)) {
    const err = new Error(`Node already exists: ${file}`);
    err.statusCode = 409;
    throw err;
  }
  const content = [
    "#+begin meta",
    `id: ${id}`,
    `title: ${title}`,
    `date: ${new Date().toISOString().slice(0, 10)}`,
    `tags: ${tags.join(", ")}`,
    "refs:",
    "#+end meta",
    "",
    `# ${title}`,
    "",
  ].join("\n");
  await writeFile(file, content, "utf8");
  return readNote(file);
}

async function readRequestJson(req) {
  const chunks = [];
  for await (const chunk of req) chunks.push(chunk);
  return JSON.parse(Buffer.concat(chunks).toString("utf8") || "{}");
}

async function routeApi(req, res) {
  const url = new URL(req.url || "/", `http://${host}:${port}`);
  try {
    if (req.method === "GET" && url.pathname === "/api/bootstrap") {
      const notes = await scanNotes();
      const snippets = await scanSnippets();
      const file = url.searchParams.get("file");
      if (file) {
        sendJson(res, 200, await readNote(file));
        return true;
      }
      sendJson(res, 200, {
        type: "open",
        file: "",
        title: "Aaronnote",
        mode: "markdown",
        content: "# Aaronnote\n\nSelect a note from the left, or keep this scratch buffer.",
        notes,
        snippets,
        root: noteRoot,
        noteDir: ".",
      });
      return true;
    }

    if (req.method === "GET" && url.pathname === "/api/file") {
      sendJson(res, 200, await readNote(url.searchParams.get("file")));
      return true;
    }

    if (req.method === "GET" && (url.pathname === "/api/notes" || url.pathname === "/api/roamdb/sync")) {
      sendJson(res, 200, { type: "notes", notes: await scanNotes(), root: noteRoot });
      return true;
    }

    if (req.method === "POST" && (url.pathname === "/api/node" || url.pathname === "/api/create-node")) {
      sendJson(res, 200, await createNode(await readRequestJson(req)));
      return true;
    }

    if (req.method === "POST" && url.pathname === "/api/save") {
      const body = await readRequestJson(req);
      const file = safeFile(body.file);
      await writeFile(file, String(body.content ?? ""));
      sendJson(res, 200, { type: "saved", ok: true, file, message: "Saved", notes: await scanNotes() });
      return true;
    }

    if (req.method === "POST" && url.pathname === "/api/export-desktop") {
      const body = await readRequestJson(req);
      const out = desktopExportPath(String(body.file || ""));
      await writeFile(out, String(body.content ?? ""));
      sendJson(res, 200, { type: "exported", ok: true, file: out, message: `Exported ${out}` });
      return true;
    }

    if (req.method === "POST" && url.pathname === "/api/export-pdf") {
      const body = await readRequestJson(req);
      const pdf = await exportPdf(String(body.file || ""), String(body.content ?? ""));
      res.writeHead(200, {
        "content-type": "application/pdf",
        "content-length": pdf.data.length,
        "content-disposition": `attachment; filename="${pdf.name.replace(/"/g, "")}"`,
      });
      res.end(pdf.data);
      return true;
    }
  } catch (err) {
    sendJson(res, err.statusCode || 500, {
      type: "error",
      ok: false,
      message: err.message || String(err),
    });
    return true;
  }
  return false;
}

async function routeRoamTools(req, res) {
  const url = new URL(req.url || "/", `http://${host}:${port}`);
  if (req.method !== "GET") return false;
  if (url.pathname === "/roam-tools/data.js") {
    const notes = await scanNotes();
    const tags = [...new Set(notes.flatMap((note) => note.tags || []))].sort();
    const data = {
      meta: {
        generatedAt: new Date().toISOString(),
        noteCount: notes.length,
        tagCount: tags.length,
      },
      notes,
    };
    const body = `window.SITE_DATA = ${JSON.stringify(data)};\n`;
    res.writeHead(200, {
      "content-type": "application/javascript; charset=utf-8",
      "content-length": Buffer.byteLength(body),
    });
    res.end(body);
    return true;
  }
  if (url.pathname === "/roam-tools/knowledge.js") {
    await sendTextFile(res, join(publishJsDir, "knowledge.js"), "application/javascript; charset=utf-8");
    return true;
  }
  if (url.pathname === "/roam-tools/graph.js") {
    await sendTextFile(res, join(publishJsDir, "graph.js"), "application/javascript; charset=utf-8");
    return true;
  }
  return false;
}

let vite;
const server = createHttpServer(async (req, res) => {
  if ((req.url || "").startsWith("/api/") && await routeApi(req, res)) return;
  if ((req.url || "").startsWith("/roam-tools/") && await routeRoamTools(req, res)) return;
  vite.middlewares(req, res, () => {
    res.statusCode = 404;
    res.end("Not found");
  });
});

vite = await createViteServer({
  configFile: join(appDir, "vite.aaronnote.config.ts"),
  server: {
    middlewareMode: true,
    host,
    hmr: { server },
  },
  appType: "spa",
});

server.listen(port, host, () => {
  console.log(`Aaronnote root: ${noteRoot}`);
  console.log(`Aaronnote notes: ${noteScanRoot}`);
  console.log(`Aaronnote snippets: ${snippetDirs().join(":") || "(none)"}`);
  console.log(`Aaronnote URL: http://${host}:${port}/`);
});
