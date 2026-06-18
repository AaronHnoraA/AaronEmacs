import { copyFile, mkdir, readdir, readFile, rename, rm, stat, writeFile } from "node:fs/promises";
import { existsSync, realpathSync } from "node:fs";
import { basename, delimiter, dirname, extname, isAbsolute, join, relative, resolve, sep } from "node:path";
import { homedir } from "node:os";
import { execFile, spawn } from "node:child_process";
import { promisify } from "node:util";
import { fileURLToPath, pathToFileURL } from "node:url";
import { createHash } from "node:crypto";
import { changedRoamFilesSince, commitRoam, fileHistory, restoreFileFromCommit, discardFileChanges, roamRepoStatus, roamRepoChanges, diffRoamFile, diffRoamCommit, pullRoam, pushRoam, repoHistory, headSha } from "./roam-git.mjs";
import { configureTmpRoot, aaronnoteTmpRoot, runtimeMkdtemp, runtimeTmpFile } from "./tmp.mjs";

const appDir = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..");
let workspaceRoot = resolve(process.env.AARONNOTE_WORKSPACE_ROOT || resolve(appDir, ".."));
let publishJsDir = resolve(process.env.AARONNOTE_PUBLISH_JS_DIR || join(workspaceRoot, "js"));
let stateRoot = resolve(process.env.AARONNOTE_STATE_DIR || join(workspaceRoot, "var", "aaronnote"));
let runtimeTmpRoot = configureTmpRoot(process.env.AARONNOTE_TMP_DIR || join(stateRoot, "tmp"));
let snippetsRoot = resolve(process.env.AARONNOTE_SNIPPETS_ROOT || join(workspaceRoot, "snippets"));
let templatesRoot = resolve(process.env.AARONNOTE_TEMPLATES_ROOT || join(workspaceRoot, "templates", "aaronnote"));
const execFileAsync = promisify(execFile);

let noteRoot = resolveUserPath(process.env.AARONNOTE_ROOT || join(appDir, "..", "roam"));
let noteScanRoot = noteRoot;
const excludedDirs = new Set([
  "_typst",
  "public",
  "var",
  ".git",
  ".direnv",
  ".venv",
  "node_modules",
  "__pycache__",
  ".ipynb_checkpoints",
  ".jupyter",
  ".pytest_cache",
  ".mypy_cache",
  ".ruff_cache",
  ".virtual_documents",
]);
const generatedAttachmentDirs = new Set(["asset", "assets", "attachment", "attachments", "file", "files", "img", "imgs", "image", "images", "media", "pdf", "pdfs"]);
const noteExts = new Set([".typ", ".md", ".markdown"]);
const projectRootMarkers = [
  ".git",
  ".project",
  ".projectile",
  ".root",
  "AGENT.md",
  "CLAUDE.md",
  "Makefile",
  "CMakeLists.txt",
  "Cargo.toml",
  "go.mod",
  "package.json",
  "pyproject.toml",
  "requirements.txt",
  "pom.xml",
  "build.gradle",
  "settings.gradle",
  "flake.nix",
  "dune-project",
  "mix.exs",
  "Gemfile",
];
const defaultNoteKind = "default";
const defaultNoteKindAliases = new Set(["", "default", "note"]);
const noteKindPattern = /^[a-z0-9_-]+$/;
const refTokenPattern = /#note\("([^"]+)"\)|\[\[([^\]\n]+)\]\]|\broam:\/\/[^\s<>)\]]+/gi;
let noteCacheRoot = "";
let noteCache = new Map();
let notesSnapshotRoot = "";
let notesSnapshot = null;
let notesRawSnapshot = null;
let notesRelationshipCache = null;
let notesSnapshotDirty = true;
let notesSnapshotFullDirty = true;
let dirtyNoteFiles = new Set();
let snippetCache = { key: "", scannedAt: 0, snippets: [] };
let templateCache = { key: "", scannedAt: 0, templates: [] };
let copilotClient = null;
let copilotLog = [];
let copilotLogRecording = false;
let roamSyncTimer = null;
let roamSyncInFlight = null;
let queuedRoamSyncNotes = null;
let queuedRoamSyncChangedFiles = [];
let atomicWriteCounter = 0;
const noteCodeFileCache = new Map();
const noteCodeFilePending = new Map();
let noteCodeFileCacheBytes = 0;
const pathSuggestionDirListingCache = new Map();
const contentRootCache = new Map();
const CURRENT_DB_SCHEMA = 1;
const BOOK_CACHE_SCHEMA = 1;
const ASSET_CLEANUP_SCHEMA = 2;
const ROAM_FULL_SYNC_INTERVAL_MS = 7 * 24 * 60 * 60 * 1000; // 7 days
const scanConcurrency = Math.max(1, Math.min(64, Number(process.env.AARONNOTE_SCAN_CONCURRENCY) || 16));
const saveRequestVersions = new Map();
const saveWriteQueues = new Map();
const NOTE_CODE_FILE_CACHE_LIMIT = 64;
const NOTE_CODE_FILE_CACHE_BYTES = 8_000_000;
const PATH_SUGGESTION_DIR_CACHE_LIMIT = 64;
const contentTypes = new Map([
  [".html", "text/html; charset=utf-8"],
  [".js", "application/javascript; charset=utf-8"],
  [".mjs", "application/javascript; charset=utf-8"],
  [".css", "text/css; charset=utf-8"],
  [".json", "application/json; charset=utf-8"],
  [".svg", "image/svg+xml"],
  [".png", "image/png"],
  [".jpg", "image/jpeg"],
  [".jpeg", "image/jpeg"],
  [".gif", "image/gif"],
  [".webp", "image/webp"],
  [".avif", "image/avif"],
  [".bmp", "image/bmp"],
  [".pdf", "application/pdf"],
  [".txt", "text/plain; charset=utf-8"],
  [".md", "text/markdown; charset=utf-8"],
  [".markdown", "text/markdown; charset=utf-8"],
  [".lean", "text/x-lean4; charset=utf-8"],
  [".drawio", "application/vnd.jgraph.mxfile"],
  [".dio", "application/vnd.jgraph.mxfile"],
  [".mp3", "audio/mpeg"],
  [".mp4", "video/mp4"],
  [".mov", "video/quicktime"],
  [".woff", "font/woff"],
  [".woff2", "font/woff2"],
  [".ttf", "font/ttf"],
  [".wasm", "application/wasm"],
]);
const pathSuggestionCodeExts = new Set([
  ".bash",
  ".c",
  ".cpp",
  ".csv",
  ".go",
  ".ipynb",
  ".jl",
  ".js",
  ".json",
  ".jsx",
  ".lua",
  ".m",
  ".py",
  ".qmd",
  ".r",
  ".rmd",
  ".rs",
  ".sh",
  ".ts",
  ".tsx",
  ".zsh",
]);
async function atomicWriteFile(file, data, options) {
  await mkdir(dirname(file), { recursive: true });
  const tmp = await runtimeTmpFile("save", file, `.tmp-${process.pid}-${Date.now()}-${++atomicWriteCounter}`);
  try {
    await writeFile(tmp, data, options);
    try {
      await rename(tmp, file);
    } catch (err) {
      if (err?.code !== "EXDEV") throw err;
      await copyFile(tmp, file);
      await rm(tmp, { force: true }).catch(() => {});
    }
    noteSelfWrite(file);
  } catch (err) {
    await rm(tmp, { force: true }).catch(() => {});
    throw err;
  }
}

function canonicalExistingPath(path) {
  const resolved = resolve(String(path || ""));
  let probe = resolved;
  const missingParts = [];
  while (!existsSync(probe)) {
    const parent = dirname(probe);
    if (parent === probe) return resolved;
    missingParts.unshift(basename(probe));
    probe = parent;
  }
  try {
    const real = realpathSync.native(probe);
    return missingParts.length ? join(real, ...missingParts) : real;
  } catch {
    return resolved;
  }
}

function relativeInsideP(rel) {
  return rel === "" || (!rel.startsWith("..") && !isAbsolute(rel));
}

function inside(child, parent) {
  const rel = relative(parent, child);
  if (relativeInsideP(rel)) return true;
  return relativeInsideP(relative(canonicalExistingPath(parent), canonicalExistingPath(child)));
}

function relativeCanonical(parent, child) {
  const rel = relative(parent, child);
  if (relativeInsideP(rel)) return rel;
  return relative(canonicalExistingPath(parent), canonicalExistingPath(child));
}

function expandUserPath(input) {
  const raw = String(input || "");
  if (raw === "~") return homedir();
  if (/^~[\\/]/.test(raw)) return join(homedir(), raw.slice(2));
  return raw;
}

function resolveUserPath(input) {
  return resolve(expandUserPath(input));
}

function slashPath(path) {
  return String(path || "").split(sep).join("/");
}

function displayPathForFile(file, root = noteScanRoot) {
  if (inside(file, noteRoot)) return slashPath(relativeCanonical(noteRoot, file));
  const home = homedir();
  if (inside(file, home)) {
    const rel = slashPath(relativeCanonical(home, file));
    return rel ? `~/${rel}` : "~";
  }
  if (root && inside(file, root)) return slashPath(relativeCanonical(root, file));
  return slashPath(file);
}

function displayPathForScanRoot(file, root = noteScanRoot) {
  if (root && inside(file, root)) {
    const rel = slashPath(relativeCanonical(root, file));
    return rel || "";
  }
  return displayPathForFile(file, root);
}

function scanRootForOpenFile(file) {
  return standaloneFile(file) ? dirname(file) : noteRoot;
}

function safeFile(input) {
  const file = resolveUserPath(input);
  if (!inside(file, noteRoot)) {
    const err = new Error(`File is outside note root: ${file}`);
    err.statusCode = 403;
    throw err;
  }
  return file;
}

function standaloneMarkdownFile(file) {
  return /\.(?:md|markdown)$/i.test(file);
}

function leanSourceFile(file) {
  return /\.lean$/i.test(String(file || ""));
}

function safeOpenFile(input) {
  const file = resolveUserPath(input);
  if (inside(file, noteRoot)) return file;
  if (standaloneMarkdownFile(file)) return file;
  const err = new Error(`File is outside note root: ${file}`);
  err.statusCode = 403;
  throw err;
}

function standaloneFile(file) {
  return !inside(file, noteRoot);
}

function markerProjectRoot(startDir) {
  const start = resolveUserPath(startDir || "");
  const cached = contentRootCache.get(start);
  if (cached) return cached;
  let dir = start;
  let root = "";
  for (let depth = 0; depth < 32; depth++) {
    if (projectRootMarkers.some((marker) => existsSync(join(dir, marker)))) {
      root = dir;
      break;
    }
    const parent = dirname(dir);
    if (parent === dir) break;
    dir = parent;
  }
  if (!root) {
    const parent = dirname(start);
    root = parent === start ? start : parent;
  }
  contentRootCache.set(start, root);
  if (contentRootCache.size > 512) contentRootCache.clear();
  return root;
}

function contentRootForFile(file) {
  if (!file || !standaloneFile(file)) return noteRoot;
  return markerProjectRoot(dirname(file));
}

function cleanContentPath(input) {
  let raw = String(input || "").trim();
  if (raw.startsWith("<") && raw.endsWith(">")) raw = raw.slice(1, -1).trim();
  return raw.split(/[?#]/, 1)[0].trim();
}

function roamPrefixedPath(raw) {
  const clean = String(raw || "").replace(/\\/g, "/").replace(/^\/+/, "");
  if (clean === "roam") return "";
  return clean.startsWith("roam/") ? clean.slice("roam/".length) : null;
}

export function resolveContentFile(input, base = "") {
  const raw = cleanContentPath(input);
  if (!raw) {
    const err = new Error("Missing content file");
    err.statusCode = 400;
    throw err;
  }
  const baseFile = base ? safeOpenFile(base) : "";
  const baseDir = baseFile ? dirname(baseFile) : noteRoot;
  const logicalRoot = contentRootForFile(baseFile);
  let file = "";
  const roamPath = roamPrefixedPath(raw);
  if (/^file:\/\//i.test(raw)) {
    try {
      file = fileURLToPath(raw);
    } catch {
      file = resolveUserPath(raw.replace(/^file:\/\//i, ""));
    }
  } else if (/^file:/i.test(raw)) {
    file = resolveUserPath(raw.replace(/^file:/i, ""));
  } else if (/^~(?:$|[\\/])/.test(raw)) {
    file = resolveUserPath(raw);
  } else if (roamPath != null) {
    file = resolve(noteRoot, roamPath);
  } else if (raw.startsWith("/")) {
    file = baseFile ? resolve(logicalRoot, raw.replace(/^\/+/, "")) : resolve(raw);
  } else {
    file = resolve(baseDir, raw);
  }
  if (!inside(file, noteRoot) && !inside(file, logicalRoot)) {
    const err = new Error(`Content file is outside the allowed root: ${file}`);
    err.statusCode = 403;
    throw err;
  }
  return file;
}

async function deleteManagedLeanMirror(_file, _info) {}

async function renameManagedLeanMirror(_file, _target, _info) {}

async function copyManagedLeanMirror(_file, _target, _info) {}

export function fileContentType(file) {
  if (/\.drawio\.xml$/i.test(String(file || ""))) return "application/vnd.jgraph.mxfile";
  return contentTypes.get(extname(file).toLowerCase()) || "application/octet-stream";
}

function sanitizeAssetName(input, fallback = "attachment") {
  const raw = basename(String(input || fallback)).normalize("NFKC");
  const safe = raw
    .replace(/[\u0000-\u001f<>:"/\\|?*]+/g, "-")
    .replace(/\s+/g, "-")
    .trim()
    .replace(/^\.+$/, "");
  return safe || fallback;
}

function imageAssetP(name, type = "") {
  if (String(type).toLowerCase().startsWith("image/")) return true;
  return new Set([".avif", ".bmp", ".gif", ".jpeg", ".jpg", ".png", ".svg", ".webp"])
    .has(extname(name).toLowerCase());
}

function visualAssetP(name, type = "") {
  const lowerType = String(type || "").toLowerCase();
  if (lowerType.includes("jgraph") || lowerType.includes("drawio") || lowerType === "text/html" || lowerType.startsWith("text/html;")) return true;
  const lowerName = String(name || "").toLowerCase();
  return /\.(?:drawio|dio)(?:\.xml)?$/i.test(lowerName) || /\.html?$/i.test(lowerName);
}

async function uniqueAssetPath(dir, name) {
  const ext = extname(name);
  const stem = basename(name, ext) || "attachment";
  let candidate = join(dir, name);
  for (let i = 2; existsSync(candidate); i++) {
    candidate = join(dir, `${stem}-${i}${ext}`);
  }
  return candidate;
}

function markdownRelativePath(fromFile, targetFile) {
  const fromDir = fromFile ? dirname(safeOpenFile(fromFile)) : noteRoot;
  let rel = relativeCanonical(fromDir, targetFile).split(sep).join("/");
  if (!rel.startsWith(".") && !rel.startsWith("/")) rel = `./${rel}`;
  return rel;
}

function resolveInputPath(input, root) {
  const raw = String(input || "");
  if (/^~(?:$|[\\/])/.test(raw) || isAbsolute(raw)) return resolveUserPath(raw);
  return resolve(root, raw);
}

function resolveInternalContentPath(input, baseDir, allowedRoot = noteRoot) {
  const raw = String(input || "").trim();
  if (!raw) return "";
  let file = "";
  const roamPath = roamPrefixedPath(raw);
  if (/^file:\/\//i.test(raw)) {
    try {
      file = fileURLToPath(raw);
    } catch {
      file = resolveUserPath(raw.replace(/^file:\/\//i, ""));
    }
  } else if (/^file:/i.test(raw)) {
    file = resolveUserPath(raw.replace(/^file:/i, ""));
  } else if (/^~(?:$|[\\/])/.test(raw)) {
    file = resolveUserPath(raw);
  } else if (roamPath != null) {
    file = resolve(noteRoot, roamPath);
  } else if (raw.startsWith("/")) {
    file = resolve(allowedRoot, raw.replace(/^\/+/, ""));
  } else {
    file = resolve(baseDir, raw);
  }
  if (!inside(file, noteRoot) && !inside(file, allowedRoot)) return "";
  return file;
}

function bareRelativeContentPath(input) {
  const raw = String(input || "").trim();
  if (!raw || raw.startsWith(".") || raw.startsWith("/") || /^file:/i.test(raw) || /^~(?:$|[\\/])/.test(raw)) return false;
  return roamPrefixedPath(raw) == null;
}

function resolveNoteCodePath(input, baseDir, allowedRoot = noteRoot) {
  const file = resolveInternalContentPath(input, baseDir, allowedRoot);
  if (!file || !bareRelativeContentPath(input) || existsSync(file)) return file;
  const rootFile = resolve(allowedRoot, String(input || "").trim());
  if (inside(rootFile, allowedRoot) && existsSync(rootFile)) return rootFile;
  return file;
}

function assetFolderName(current) {
  if (!current) return "scratch";
  const ext = extname(current);
  return sanitizeAssetName(basename(current, ext), "note");
}

function standaloneAssetRoot(file) {
  return contentRootForFile(file);
}

export function resolveMediaFile(file, base = "") {
  const raw = String(file || "");
  if (!raw) {
    const err = new Error("Missing media file");
    err.statusCode = 400;
    throw err;
  }
  let resolved;
  try {
    resolved = resolveContentFile(raw, base);
  } catch (err) {
    if (err?.statusCode === 400) throw err;
    resolved = "";
  }
  if (!resolved) {
    const err = new Error(`Media file is outside the current document folder: ${resolved}`);
    err.statusCode = 403;
    throw err;
  }
  return resolved;
}

export async function storeAsset(body) {
  const current = body.file ? safeOpenFile(body.file) : "";
  const originalName = sanitizeAssetName(body.name, imageAssetP("", body.type) ? "image.png" : "attachment");
  const isImage = imageAssetP(originalName, body.type);
  const baseDir = current ? dirname(current) : noteRoot;
  const allowedRoot = current && standaloneFile(current) ? contentRootForFile(current) : noteRoot;
  const targetDir = join(baseDir, isImage ? "images" : "attachments", assetFolderName(current));
  if (!inside(targetDir, noteRoot) && !inside(targetDir, allowedRoot)) {
    const err = new Error(`Asset directory is outside the current document folder: ${targetDir}`);
    err.statusCode = 403;
    throw err;
  }
  const rawData = String(body.data || "");
  if (!rawData) {
    const err = new Error("Missing asset data");
    err.statusCode = 400;
    throw err;
  }
  const target = await uniqueAssetPath(targetDir, originalName);
  await mkdir(targetDir, { recursive: true });
  await writeFile(target, Buffer.from(rawData, "base64"));
  return {
    ok: true,
    file: target,
    name: basename(target),
    type: fileContentType(target),
    isImage,
    markdownPath: markdownRelativePath(current, target),
  };
}

export async function storeAssetFromPath(body) {
  const current = body.file ? safeOpenFile(body.file) : "";
  const source = resolveUserPath(body.path || body.source || "");
  if (!source) {
    const err = new Error("Missing asset source path");
    err.statusCode = 400;
    throw err;
  }
  const info = await stat(source);
  if (!info.isFile()) {
    const err = new Error(`Asset source is not a regular file: ${source}`);
    err.statusCode = 400;
    throw err;
  }
  const originalName = sanitizeAssetName(body.name || basename(source), "attachment");
  const type = String(body.type || fileContentType(source));
  const isImage = imageAssetP(originalName, type);
  const baseDir = current ? dirname(current) : noteRoot;
  const allowedRoot = current && standaloneFile(current) ? contentRootForFile(current) : noteRoot;
  const targetDir = join(baseDir, isImage ? "images" : "attachments", assetFolderName(current));
  if (!inside(targetDir, noteRoot) && !inside(targetDir, allowedRoot)) {
    const err = new Error(`Asset directory is outside the current document folder: ${targetDir}`);
    err.statusCode = 403;
    throw err;
  }
  const target = await uniqueAssetPath(targetDir, originalName);
  await mkdir(targetDir, { recursive: true });
  await copyFile(source, target);
  return {
    ok: true,
    file: target,
    name: basename(target),
    type: fileContentType(target),
    isImage,
    markdownPath: markdownRelativePath(current, target),
  };
}

function tikzVersionMs(value) {
  const raw = String(value || "").trim();
  if (!raw) return 0;
  if (/^\d{13}$/.test(raw)) return Number(raw);
  if (/^\d{10}$/.test(raw)) return Number(raw) * 1000;
  const compact = raw.match(/^(\d{4})(\d{2})(\d{2})(?:[-_T]?(\d{2})(\d{2})(\d{2})?)?$/);
  if (compact) {
    const [, y, m, d, hh = "00", mm = "00", ss = "00"] = compact;
    return new Date(Number(y), Number(m) - 1, Number(d), Number(hh), Number(mm), Number(ss)).getTime();
  }
  const parsed = Date.parse(raw);
  return Number.isFinite(parsed) ? parsed : 0;
}

function normalizeTikzForLatex(source) {
  const cleaned = String(source || "")
    .split(/\r?\n/)
    .map((line) => {
      for (let i = 0; i < line.length; i++) {
        if (line[i] !== "%") continue;
        let slashCount = 0;
        for (let j = i - 1; j >= 0 && line[j] === "\\"; j--) slashCount++;
        if (slashCount % 2 === 0) return line.slice(0, i).trimEnd();
      }
      return line;
    })
    .join("\n")
    .trim();
  if (!cleaned) return "";
  if (/\\documentclass\b|\\begin\s*\{\s*document\s*\}/.test(cleaned)) return cleaned;
  if (/\\begin\s*\{\s*tikzpicture\s*\}/.test(cleaned)) {
    return [
      "\\documentclass[tikz,border=2pt]{standalone}",
      "\\begin{document}",
      cleaned,
      "\\end{document}",
    ].join("\n");
  }
  return [
    "\\documentclass[tikz,border=2pt]{standalone}",
    "\\begin{document}",
    "\\begin{tikzpicture}",
    cleaned,
    "\\end{tikzpicture}",
    "\\end{document}",
  ].join("\n");
}

function executablePath(command) {
  if (String(command || "").includes(sep) && existsSync(command)) return command;
  const paths = [
    ...(process.env.PATH || "").split(delimiter),
    join(homedir(), ".nix-profile", "bin"),
    "/run/current-system/sw/bin",
    "/opt/homebrew/bin",
    "/usr/local/bin",
    "/usr/bin",
    "/bin",
  ].filter(Boolean);
  for (const dir of paths) {
    const candidate = join(dir, command);
    if (existsSync(candidate)) return candidate;
  }
  return command;
}

function commandOutputTail(err) {
  const parts = [
    err?.message,
    err?.stderr,
    err?.stdout,
  ].filter(Boolean).map((part) => String(part).trim()).filter(Boolean);
  const text = parts.join("\n");
  if (!text) return "";
  return text.split(/\r?\n/).slice(-8).join("\n");
}

export async function renderTikzAsset(body) {
  const current = body.file ? safeOpenFile(body.file) : "";
  if (!current) {
    const err = new Error("Missing current note file");
    err.statusCode = 400;
    throw err;
  }
  const id = sanitizeAssetName(body.id || createHash("sha1").update(String(body.source || "")).digest("hex").slice(0, 12), "tikz");
  const timestamp = String(body.timestamp || body.version || "").trim();
  const baseDir = dirname(current);
  const allowedRoot = current && standaloneFile(current) ? contentRootForFile(current) : noteRoot;
  const targetDir = join(baseDir, "images", assetFolderName(current));
  if (!inside(targetDir, noteRoot) && !inside(targetDir, allowedRoot)) {
    const err = new Error(`Asset directory is outside the current document folder: ${targetDir}`);
    err.statusCode = 403;
    throw err;
  }
  const target = join(targetDir, `tikz-${id}.svg`);
  const wantedMs = tikzVersionMs(timestamp);
  const existing = existsSync(target) ? await stat(target) : null;
  if (existing && (!wantedMs || existing.mtimeMs >= wantedMs)) {
    return {
      ok: true,
      file: target,
      name: basename(target),
      type: "image/svg+xml",
      isImage: true,
      markdownPath: markdownRelativePath(current, target),
      rendered: false,
      mtimeMs: existing.mtimeMs,
    };
  }

  const tex = normalizeTikzForLatex(body.source || "");
  if (!tex) {
    const err = new Error("Missing TikZ source");
    err.statusCode = 400;
    throw err;
  }

  const tmp = await runtimeMkdtemp("tikz", current);
  let latexError = null;
  let dvisvgmError = null;
  let mutoolError = null;
  try {
    const texFile = join(tmp, "main.tex");
    const pdfFile = join(tmp, "main.pdf");
    const svgFile = join(tmp, "out.svg");
    await writeFile(texFile, tex, "utf8");
    try {
      await execFileAsync(executablePath("pdflatex"), [
        "-interaction=nonstopmode",
        "-halt-on-error",
        `-output-directory=${tmp}`,
        texFile,
      ], { timeout: 20_000, maxBuffer: 8 * 1024 * 1024 });
    } catch (err) {
      latexError = err;
      throw err;
    }

    try {
      await execFileAsync(executablePath("dvisvgm"), [
        "--pdf",
        "--no-fonts",
        "--exact",
        "--bbox=min",
        "-o",
        svgFile,
        pdfFile,
      ], { timeout: 20_000, maxBuffer: 8 * 1024 * 1024 });
    } catch (err) {
      dvisvgmError = err;
      try {
        await execFileAsync(executablePath("mutool"), [
          "convert",
          "-o",
          svgFile,
          pdfFile,
        ], { timeout: 20_000, maxBuffer: 8 * 1024 * 1024 });
      } catch (fallbackErr) {
        mutoolError = fallbackErr;
        throw fallbackErr;
      }
    }
    const renderedSvgFile = existsSync(svgFile)
      ? svgFile
      : existsSync(join(tmp, "out1.svg"))
        ? join(tmp, "out1.svg")
        : svgFile;
    if (!existsSync(renderedSvgFile)) {
      throw new Error("TikZ SVG conversion did not produce an SVG file");
    }
    await mkdir(targetDir, { recursive: true });
    await copyFile(renderedSvgFile, target);
    const info = await stat(target);
    return {
      ok: true,
      file: target,
      name: basename(target),
      type: "image/svg+xml",
      isImage: true,
      markdownPath: markdownRelativePath(current, target),
      rendered: true,
      mtimeMs: info.mtimeMs,
    };
  } catch (err) {
    const details = [
      latexError ? `pdflatex: ${commandOutputTail(latexError)}` : "",
      dvisvgmError ? `dvisvgm: ${commandOutputTail(dvisvgmError)}` : "",
      mutoolError ? `mutool: ${commandOutputTail(mutoolError)}` : "",
    ].filter(Boolean).join("\n\n");
    return {
      ok: false,
      file: target,
      name: basename(target),
      type: "image/svg+xml",
      isImage: true,
      markdownPath: markdownRelativePath(current, target),
      rendered: false,
      message: details || (err instanceof Error ? err.message : String(err)),
    };
  } finally {
    await rm(tmp, { recursive: true, force: true });
  }
}

function pathSuggestionDirectoryPrefix(value) {
  const raw = String(value || "./").replace(/\\/g, "/");
  const slash = raw.lastIndexOf("/");
  return slash >= 0 ? raw.slice(0, slash + 1) : "./";
}

function pathSuggestionDirectory(current, prefix) {
  const displayPrefix = pathSuggestionDirectoryPrefix(prefix);
  const rootBased = displayPrefix.startsWith("/");
  const allowedRoot = contentRootForFile(current);
  const baseDir = rootBased ? allowedRoot : dirname(current);
  const relativeDir = rootBased ? displayPrefix.replace(/^\/+/, "") : displayPrefix;
  const dir = resolve(baseDir, relativeDir || ".");
  if (!inside(dir, allowedRoot)) return null;
  const relParts = relativeCanonical(allowedRoot, dir).split(sep).filter(Boolean);
  if (relParts.some((part) => excludedDirs.has(part))) return null;
  return { dir, displayPrefix };
}

export async function pathSuggestionsForFile(file, prefix = "./") {
  const current = file ? safeOpenFile(file) : "";
  if (!current) return [];
  const target = pathSuggestionDirectory(current, prefix);
  if (!target) return [];
  const version = notesIndexVersion;
  const cached = pathSuggestionDirListingCache.get(target.dir);
  let entries = cached && cached.version === version ? cached.entries : null;
  if (!entries) {
    try {
      entries = await readdir(target.dir, { withFileTypes: true });
    } catch {
      return [];
    }
    pathSuggestionDirListingCache.set(target.dir, { entries, version });
    if (pathSuggestionDirListingCache.size > PATH_SUGGESTION_DIR_CACHE_LIMIT) {
      const oldest = pathSuggestionDirListingCache.keys().next();
      if (!oldest.done) pathSuggestionDirListingCache.delete(oldest.value);
    }
  }
  return entries
    .filter((entry) => !entry.name.startsWith("."))
    .filter((entry) => entry.isFile() || (entry.isDirectory() && !excludedDirs.has(entry.name)))
    .map((entry) => `${target.displayPrefix}${entry.name}${entry.isDirectory() ? "/" : ""}`)
    .sort((a, b) => {
      const aDir = a.endsWith("/");
      const bDir = b.endsWith("/");
      return aDir === bDir ? a.localeCompare(b) : aDir ? -1 : 1;
    })
    .slice(0, 500);
}

function normalizeLeanTag(value) {
  return String(value || "")
    .trim()
    .replace(/^\[|\]$/g, "")
    .replace(/[^A-Za-z0-9_.:-]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 80);
}

function scanCodeRegions(text) {
  const source = String(text || "");
  const regions = [];
  const matches = [];
  // Matches any line-comment prefix (#, //, --, ;) followed by @aaronnote or @note-code + tag.
  // Leading whitespace is allowed so indented markers work too.
  const tagRe = /^[ \t]*(?:--|#|\/\/|;)[ \t]*@(?:aaronnote|note-code)[ \t]+([A-Za-z0-9_.:-]+)[ \t]*$/gm;
  let match;
  while ((match = tagRe.exec(source)) !== null) {
    const markerFrom = match.index;
    const markerTo = tagRe.lastIndex;
    const bodyFrom = source.slice(markerTo, markerTo + 1) === "\n" ? markerTo + 1 : markerTo;
    matches.push({ tag: match[1], markerFrom, markerTo, bodyFrom });
  }
  for (let i = 0; i < matches.length; i++) {
    const current = matches[i];
    const next = matches[i + 1];
    const bodyTo = next ? next.markerFrom : source.length;
    regions.push({ ...current, bodyTo, body: source.slice(current.bodyFrom, bodyTo) });
  }
  return regions;
}

function languageForFile(file) {
  const ext = extname(file).toLowerCase();
  const map = {
    ".lean": "lean4", ".py": "python", ".r": "r", ".jl": "julia",
    ".js": "javascript", ".ts": "typescript", ".jsx": "javascript", ".tsx": "typescript",
    ".el": "elisp", ".lisp": "lisp", ".scm": "scheme", ".clj": "clojure",
    ".sh": "bash", ".bash": "bash", ".zsh": "zsh",
    ".c": "c", ".h": "c", ".cpp": "cpp", ".cc": "cpp", ".hpp": "cpp",
    ".java": "java", ".rs": "rust", ".go": "go", ".hs": "haskell",
    ".rb": "ruby", ".kt": "kotlin", ".swift": "swift", ".cs": "csharp",
    ".ml": "ocaml", ".lua": "lua", ".sql": "sql",
  };
  return map[ext] || (ext.length > 1 ? ext.slice(1) : "text");
}

function rememberNoteCodeFile(file, info, text, regions) {
  const bytes = Buffer.byteLength(text, "utf8");
  const existing = noteCodeFileCache.get(file);
  if (existing) noteCodeFileCacheBytes -= existing.bytes;
  noteCodeFileCache.delete(file);
  noteCodeFileCache.set(file, { mtimeMs: info.mtimeMs, size: info.size, bytes, regions });
  noteCodeFileCacheBytes += bytes;
  while (noteCodeFileCache.size > NOTE_CODE_FILE_CACHE_LIMIT || noteCodeFileCacheBytes > NOTE_CODE_FILE_CACHE_BYTES) {
    const oldest = noteCodeFileCache.keys().next().value;
    if (!oldest) break;
    const removed = noteCodeFileCache.get(oldest);
    noteCodeFileCache.delete(oldest);
    noteCodeFileCacheBytes -= removed?.bytes || 0;
  }
}

async function loadNoteCodeRegionsForFile(file) {
  const info = await stat(file);
  if (!info.isFile()) {
    const err = new Error(`Not a regular file: ${file}`);
    err.statusCode = 400;
    throw err;
  }
  const cached = noteCodeFileCache.get(file);
  if (cached && cached.mtimeMs === info.mtimeMs && cached.size === info.size) {
    noteCodeFileCache.delete(file);
    noteCodeFileCache.set(file, cached);
    return { info, regions: cached.regions };
  }
  const text = await readFile(file, "utf8");
  const regions = scanCodeRegions(text);
  rememberNoteCodeFile(file, info, text, regions);
  return { info, regions };
}

function noteCodeRegionsForFile(file) {
  const existing = noteCodeFilePending.get(file);
  if (existing) return existing;
  const pending = loadNoteCodeRegionsForFile(file)
    .finally(() => {
      if (noteCodeFilePending.get(file) === pending) noteCodeFilePending.delete(file);
    });
  noteCodeFilePending.set(file, pending);
  return pending;
}

export async function readNoteCodeRegion(body) {
  const notePath = safeOpenFile(body?.notePath || body?.file || "");
  const rawPath = String(body?.path || "").trim();
  const id = normalizeLeanTag(body?.id || "");
  if (!rawPath || !id) {
    const err = new Error("Missing note-code path or id");
    err.statusCode = 400;
    throw err;
  }
  const baseDir = dirname(notePath);
  const allowedRoot = contentRootForFile(notePath);
  const file = resolveNoteCodePath(rawPath, baseDir, allowedRoot);
  if (!file) {
    const err = new Error(`Code file is outside the allowed root: ${rawPath}`);
    err.statusCode = 403;
    throw err;
  }
  const { info, regions } = await noteCodeRegionsForFile(file);
  const region = regions.find((item) => item.tag === id);
  if (!region) {
    const err = new Error(`Region not found: ${id}`);
    err.statusCode = 404;
    throw err;
  }
  return { ok: true, file, path: rawPath, id, body: region.body, language: languageForFile(file), mtimeMs: info.mtimeMs, size: info.size };
}

function assetCandidateFile(file) {
  const relParts = relativeCanonical(noteRoot, file).split(sep).map((part) => part.toLowerCase());
  if (relParts.includes(".lean")) return false;
  if (!relParts.includes("images") && !relParts.includes("attachments")) return false;
  const ext = extname(file).toLowerCase();
  return !leanSourceFile(file) && !noteExts.has(ext) && basename(file) !== ".aaronnote-keep";
}

function assetReferenceSourceFile(file) {
  const relParts = relativeCanonical(noteRoot, file).split(sep).map((part) => part.toLowerCase());
  if (relParts.includes(".lean")) return false;
  return /\.(?:md|markdown|typ)$/i.test(file);
}

function resolveReferencedAsset(href, noteFile) {
  const protocol = hrefProtocol(href);
  if (protocol && protocol !== "file") return "";
  const rawPath = hrefPath(href);
  if (!rawPath || rawPath.startsWith("#")) return "";
  try {
    const file = resolveContentFile(rawPath, noteFile);
    return inside(file, noteRoot) || inside(file, contentRootForFile(noteFile)) ? file : "";
  } catch {
    return "";
  }
}

export function assetRefsFromContent(content, noteFile) {
  const refs = new Set();
  const addHref = (href) => {
    const file = resolveReferencedAsset(href, noteFile);
    if (file) refs.add(file);
  };
  for (const href of markdownLinkHrefs(content)) {
    addHref(href);
  }
  for (const match of content.matchAll(/\b(?:src|href|poster|data-src)\s*=\s*["']([^"']+)["']/gi)) {
    addHref(match[1]);
  }
  for (const match of content.matchAll(/\bsrcset\s*=\s*["']([^"']+)["']/gi)) {
    for (const item of String(match[1] || "").split(",")) {
      const href = item.trim().split(/\s+/, 1)[0] || "";
      addHref(href);
    }
  }
  for (const match of content.matchAll(/\burl\(\s*(['"]?)([^'")]+)\1\s*\)/gi)) {
    addHref(match[2]);
  }
  for (const match of content.matchAll(/\[\[(?:file:)?([^\]\n]+?)(?:\][^\]\n]*)?\]\]/gi)) {
    addHref(match[1]);
  }
  for (const match of content.matchAll(/^\s*#\+include:\s+["<]?([^">\n]+)[">]?/gim)) {
    addHref(match[1]);
  }
  return [...refs];
}

function assetCleanupStateFile() {
  return join(stateRoot, "asset-cleanup", "state.json");
}

async function readAssetCleanupState() {
  try {
    const raw = await readFile(assetCleanupStateFile(), "utf8");
    return JSON.parse(raw);
  } catch {
    return {};
  }
}

async function writeAssetCleanupState(next) {
  await atomicWriteFile(assetCleanupStateFile(), `${JSON.stringify(next, null, 2)}\n`, "utf8");
}

async function assetReferenceFiles() {
  return walkFiles(noteScanRoot, assetReferenceSourceFile);
}

async function assetSourceStats(files) {
  const stats = {};
  await mapLimit(files, scanConcurrency, async (file) => {
    try {
      const info = await stat(file);
      stats[file] = { mtimeMs: info.mtimeMs, size: info.size };
    } catch {}
  });
  return stats;
}

async function assetRefsForFiles(files) {
  const refsByFile = {};
  await mapLimit(files, scanConcurrency, async (file) => {
    try {
      const content = await readFile(file, "utf8");
      refsByFile[file] = assetRefsFromContent(content, file);
    } catch {
      refsByFile[file] = [];
    }
  });
  return refsByFile;
}

async function assetRefsByFileIncremental() {
  const state = await readAssetCleanupState();
  const schemaOk = state.schemaVersion === ASSET_CLEANUP_SCHEMA;
  const cachedRefs = state.refsByFile && typeof state.refsByFile === "object" ? state.refsByFile : {};
  const sourceFiles = await assetReferenceFiles();
  const sourceSet = new Set(sourceFiles);
  const sourceStats = await assetSourceStats(sourceFiles);
  const previousStats = state.sourceStats && typeof state.sourceStats === "object" ? state.sourceStats : {};
  const assetStale = state.lastFullAt
    ? (Date.now() - new Date(state.lastFullAt).getTime()) > ROAM_FULL_SYNC_INTERVAL_MS
    : false;
  const forceFull = !schemaOk || !state.lastScannedCommit || assetStale;
  let refsByFile = {};
  let full = forceFull;
  let changedFiles = null;
  if (!full) {
    changedFiles = await changedRoamFilesSince(noteRoot, state.lastScannedCommit);
    if (changedFiles === null) full = true;
  }
  if (full) {
    refsByFile = await assetRefsForFiles(sourceFiles);
  } else {
    refsByFile = { ...cachedRefs };
    for (const file of Object.keys(refsByFile)) {
      if (!sourceSet.has(file)) delete refsByFile[file];
    }
    const statChanged = sourceFiles.filter((file) => {
      const prev = previousStats[file];
      const next = sourceStats[file];
      return !prev || !next || Number(prev.mtimeMs) !== Number(next.mtimeMs) || Number(prev.size) !== Number(next.size);
    });
    const changedSources = [...new Set([
      ...(changedFiles || []).map((file) => resolveUserPath(file)).filter((file) => sourceSet.has(file)),
      ...statChanged,
    ])];
    Object.assign(refsByFile, await assetRefsForFiles(changedSources));
  }
  const sha = await headSha(noteRoot);
  await writeAssetCleanupState({
    schemaVersion: ASSET_CLEANUP_SCHEMA,
    lastScannedCommit: sha || state.lastScannedCommit || "",
    lastFullAt: full ? new Date().toISOString() : state.lastFullAt || "",
    lastScannedAt: new Date().toISOString(),
    sourceStats,
    refsByFile,
  }).catch(() => {});
  return refsByFile;
}

export async function scanUnusedAssets() {
  const refsByFile = await assetRefsByFileIncremental();
  const referenced = new Set();
  for (const refs of Object.values(refsByFile)) {
    if (!Array.isArray(refs)) continue;
    for (const ref of refs) referenced.add(resolve(String(ref)));
  }
  const files = await walkFiles(noteRoot, assetCandidateFile);
  const assets = await mapLimit(files, scanConcurrency, async (file) => {
    try {
      const info = await stat(file);
      if (!info.isFile() || referenced.has(file)) return null;
      const rel = relativeCanonical(noteRoot, file).split(sep).join("/");
      return {
        file,
        path: rel,
        name: basename(file),
        type: fileContentType(file),
        size: info.size,
        mtimeMs: info.mtimeMs,
        isImage: imageAssetP(file),
      };
    } catch {}
    return null;
  });
  return assets
    .filter(Boolean)
    .sort((a, b) => String(a.path).localeCompare(String(b.path)));
}

export async function trashUnusedAssets(body) {
  const requested = Array.isArray(body.files) ? body.files.map((file) => resolve(String(file || ""))) : [];
  if (requested.length === 0) return { type: "unused-assets-trash", ok: true, trashed: [], skipped: [], assets: await scanUnusedAssets() };
  const assets = await scanUnusedAssets();
  const byFile = new Map(assets.map((asset) => [asset.file, asset]));
  const trashed = [];
  const skipped = [];
  for (const file of requested) {
    const asset = byFile.get(file);
    if (!asset) {
      skipped.push(file);
      continue;
    }
    try {
      trashed.push({ ...asset, trashedTo: await moveToTrash(asset.file) });
    } catch {
      skipped.push(file);
    }
  }
  return { type: "unused-assets-trash", ok: true, trashed, skipped, assets: await scanUnusedAssets() };
}

function recentStoreFile() {
  return join(stateRoot, "recent.json");
}

function normalizeRecentNotes(entries) {
  if (!Array.isArray(entries)) return [];
  const byFile = new Map();
  for (const item of entries) {
    const file = item && typeof item.file === "string" ? item.file : "";
    const openedAt = item && typeof item.openedAt === "number" ? item.openedAt : NaN;
    if (!file || !Number.isFinite(openedAt)) continue;
    let safe;
    try {
      safe = safeOpenFile(file);
    } catch {
      continue;
    }
    const current = byFile.get(safe);
    if (!current || openedAt > current.openedAt) byFile.set(safe, { file: safe, openedAt });
  }
  return [...byFile.values()].sort((a, b) => b.openedAt - a.openedAt).slice(0, 24);
}

export async function readRecentNotes() {
  try {
    const raw = await readFile(recentStoreFile(), "utf8");
    return normalizeRecentNotes(JSON.parse(raw));
  } catch {
    return [];
  }
}

async function writeRecentNotes(entries) {
  const file = recentStoreFile();
  await atomicWriteFile(file, `${JSON.stringify(normalizeRecentNotes(entries), null, 2)}\n`, "utf8");
}

export async function touchRecentNote(file, openedAt = Date.now()) {
  const safe = safeOpenFile(file);
  const recent = await readRecentNotes();
  const next = normalizeRecentNotes([{ file: safe, openedAt }, ...recent]);
  await writeRecentNotes(next);
  return next;
}

function positionStoreFile() {
  return join(stateRoot, "positions.json");
}

function normalizeCursorPositions(entries) {
  if (!Array.isArray(entries)) return [];
  const byFile = new Map();
  for (const item of entries) {
    const file = item && typeof item.file === "string" ? item.file : "";
    if (!file) continue;
    let safe;
    try {
      safe = safeOpenFile(file);
    } catch {
      continue;
    }
    const from = item && typeof item.from === "number" && Number.isFinite(item.from) ? Math.max(0, item.from) : 0;
    const to = item && typeof item.to === "number" && Number.isFinite(item.to) ? Math.max(0, item.to) : from;
    const scrollY = item && typeof item.scrollY === "number" && Number.isFinite(item.scrollY) ? Math.max(0, item.scrollY) : 0;
    const updatedAt = item && typeof item.updatedAt === "number" && Number.isFinite(item.updatedAt) ? item.updatedAt : 0;
    const mode = item && item.mode === "source" ? "source" : "markdown";
    const current = byFile.get(safe);
    if (!current || updatedAt > current.updatedAt) {
      byFile.set(safe, { file: safe, mode, from, to, scrollY, updatedAt });
    }
  }
  return [...byFile.values()].sort((a, b) => b.updatedAt - a.updatedAt).slice(0, 240);
}

export async function readCursorPositions() {
  try {
    const raw = await readFile(positionStoreFile(), "utf8");
    return normalizeCursorPositions(JSON.parse(raw));
  } catch {
    return [];
  }
}

async function writeCursorPositions(entries) {
  const file = positionStoreFile();
  await atomicWriteFile(file, `${JSON.stringify(normalizeCursorPositions(entries), null, 2)}\n`, "utf8");
}

export async function touchCursorPosition(body) {
  const safe = safeOpenFile(body.file);
  const current = await readCursorPositions();
  const next = normalizeCursorPositions([{ ...body, file: safe, updatedAt: Number(body.updatedAt) || Date.now() }, ...current]);
  await writeCursorPositions(next);
  return next;
}

function modeForFile(file) {
  const lower = file.toLowerCase();
  if (lower.endsWith(".md") || lower.endsWith(".markdown")) return "markdown";
  return "source";
}

function parseListValue(value, options = {}) {
  const trimmed = String(value || "").trim();
  if (!trimmed) return [];
  if (trimmed.startsWith("(")) {
    return [...trimmed.matchAll(/"((?:[^"\\]|\\.)*)"/g)]
      .map((match) => match[1].replace(/\\"/g, '"').replace(/\\\\/g, "\\"))
      .filter(Boolean);
  }
  const separator = options.splitSpaces === false ? /[,\n]+/ : /[, ]+/;
  return trimmed.split(separator).map((item) => item.trim()).filter(Boolean);
}

function parseMetaScalar(value) {
  let trimmed = String(value || "").trim();
  if ((trimmed.startsWith('"') && trimmed.endsWith('"')) || (trimmed.startsWith("'") && trimmed.endsWith("'"))) {
    trimmed = trimmed.slice(1, -1);
  }
  if (trimmed === "true" || trimmed === "false") return trimmed === "true";
  return trimmed.replace(/\\_/g, "_");
}

function parseMetaLines(raw) {
  const meta = {};
  let currentList = "";
  for (const rawLine of raw.split(/\r?\n/)) {
    const item = rawLine.match(/^\s*-\s*(.+?)\s*$/);
    if (item && currentList) {
      if (!Array.isArray(meta[currentList])) meta[currentList] = [];
      meta[currentList].push(parseMetaScalar(item[1]));
      continue;
    }
    const line = rawLine.trim();
    if (!line || line.startsWith("#")) continue;
    const pair = line.match(/^([A-Za-z0-9_-]+)\s*:\s*(.*)$/);
    if (!pair) continue;
    const key = pair[1].toLowerCase();
    const value = pair[2].trim();
    if (!value) {
      meta[key] = [];
      currentList = key;
      continue;
    }
    if (key === "tags" || key === "refs" || key === "aliases") {
      meta[key] = parseListValue(value, { splitSpaces: key !== "aliases" });
    } else {
      meta[key] = parseMetaScalar(value);
    }
    currentList = "";
  }
  return meta;
}

function parseFrontMatter(content) {
  const match = String(content || "").match(/^\s*---\s*\r?\n([\s\S]*?)\r?\n---\s*(?:\r?\n|$)/);
  return match ? parseMetaLines(match[1]) : {};
}

function parseMetaBlock(content) {
  const match = content.match(/^\s*#\+begin\s+meta\s*\r?\n([\s\S]*?)\r?\n\s*#\+end\s+meta\s*$/im);
  return match ? parseMetaLines(match[1]) : {};
}

function metaBlockRange(content) {
  const match = content.match(/^\s*#\+begin\s+meta\s*\r?\n[\s\S]*?\r?\n\s*#\+end\s+meta\s*(?:\r?\n)*/im);
  if (!match || match.index == null) return null;
  return { from: match.index, to: match.index + match[0].length, text: match[0] };
}

function normalizeTags(tags) {
  const byKey = new Map();
  for (const tag of (Array.isArray(tags) ? tags : parseListValue(tags))) {
    const clean = String(tag).trim().replace(/^#/, "");
    if (!clean) continue;
    const key = clean.toLowerCase();
    const previous = byKey.get(key);
    if (!previous || clean === key) byKey.set(key, clean);
  }
  return [...byKey.values()].sort((a, b) => a.localeCompare(b));
}

function roamOffFromMeta(meta) {
  return String(meta.roam ?? "").trim().toLowerCase() === "off";
}

function hasRoamMeta(content) {
  const meta = noteMetadata(content);
  const id = String(meta.id ?? "").trim();
  return id.length > 0 && !roamOffFromMeta(meta);
}

function hasNoteMetadata(content) {
  return Object.keys(noteMetadata(content)).length > 0;
}

function ensureDate(value = "") {
  return String(value || new Date().toISOString().slice(0, 10));
}

function buildMetaBlock(fields) {
  const tags = normalizeTags(fields.tags || []);
  const refs = normalizeTags(fields.refs || []);
  const aliases = normalizeTags(fields.aliases || []);
  const lines = ["#+begin meta"];
  // Omit the roam id for standalone (non-roam) notes so they keep a meta block
  // (tags etc.) without being synced into the roam graph database.
  if (fields.id) lines.push(`id: ${fields.id}`);
  lines.push(
    `title: ${fields.title}`,
    `date: ${ensureDate(fields.date)}`,
    `kind: ${fields.kind || defaultNoteKind}`,
  );
  if (roamOffFromMeta(fields)) lines.push("roam: off");
  lines.push(
    `tags: ${tags.join(", ")}`,
    `refs: ${refs.join(", ")}`,
  );
  if (aliases.length > 0) lines.push(`aliases: ${aliases.join(", ")}`);
  if (fields.source) lines.push(`source: ${fields.source}`);
  if (fields.summary) lines.push(`summary: ${String(fields.summary).replace(/\r?\n/g, " ")}`);
  if (fields.private !== undefined) lines.push(`private: ${fields.private === true || fields.private === "true" ? "true" : "false"}`);
  lines.push("#+end meta", "");
  return lines.join("\n");
}

function metaFieldsForFile(file, content, patch = {}) {
  const current = noteMetadata(content);
  const title = String(patch.title || current.title || titleFromContent(file, content) || basename(file, extname(file)) || "Untitled").trim();
  const hasPatchId = Object.prototype.hasOwnProperty.call(patch, "id");
  const keepsNoRoamId = !current.id && roamOffFromMeta({ ...current, ...patch });
  const id = String(hasPatchId ? patch.id : (current.id || (keepsNoRoamId ? "" : `${timestampId()}-${slugifyTitle(title)}`))).trim();
  return {
    ...current,
    ...patch,
    id,
    title,
    date: ensureDate(patch.date || current.date),
    kind: normalizeNoteKind(patch.kind || current.kind || defaultNoteKind),
    tags: normalizeTags(patch.tags ?? current.tags ?? []),
    refs: normalizeTags(patch.refs ?? current.refs ?? []),
    aliases: normalizeTags(patch.aliases ?? current.aliases ?? []),
  };
}

function removeMetaBlock(content) {
  const range = metaBlockRange(content);
  if (!range) return content;
  return `${content.slice(0, range.from)}${content.slice(range.to)}`.replace(/^\s+/, "");
}

function upsertMetaBlock(file, content, patch = {}) {
  const nextMeta = buildMetaBlock(metaFieldsForFile(file, content, patch));
  const body = removeMetaBlock(content);
  return `${nextMeta}\n${body.replace(/^\s+/, "")}`;
}

// Insert a `roam: off` line into an existing meta block (used for regular notes
// created from a template that already supplies its own meta). Preserves the rest
// of the block and reports the byte offset inserted so callers can shift a cursor
// selection that sits after the meta block.
function withMetaRoamOff(content) {
  const range = metaBlockRange(content);
  if (!range || roamOffFromMeta(parseMetaBlock(content))) return { content, offset: 0 };
  const insertLine = "roam: off\n";
  const block = range.text;
  const kindMatch = block.match(/^[ \t]*kind:[^\n]*\r?\n/im);
  const beginMatch = block.match(/^\s*#\+begin\s+meta\s*\r?\n/i);
  const within = kindMatch?.index != null
    ? kindMatch.index + kindMatch[0].length
    : (beginMatch ? beginMatch[0].length : 0);
  const insertAt = range.from + within;
  return {
    content: `${content.slice(0, insertAt)}${insertLine}${content.slice(insertAt)}`,
    offset: insertLine.length,
  };
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
    ...parseFrontMatter(content),
    ...parseTypstMetadata(content),
    ...parseMetaBlock(content),
  };
}

function bookCacheDir() {
  return join(stateRoot, "book");
}

function safeBookCacheName(id) {
  return `${String(id || "book").trim().replace(/[\\/:\0]/g, "_") || "book"}.json`;
}

function sha256Text(text) {
  return createHash("sha256").update(String(text || "")).digest("hex");
}

function bookMetaFromContent(content) {
  const meta = noteMetadata(content);
  const value = meta.book;
  if (value === true) return { role: "cover", parentRef: "" };
  const raw = String(value || "").trim();
  if (!raw) return { role: "", parentRef: "" };
  if (/^(true|yes|book)$/i.test(raw)) return { role: "cover", parentRef: "" };
  const included = raw.match(/^included@(.+)$/i);
  if (included) return { role: "included", parentRef: included[1].trim() };
  return { role: "", parentRef: "" };
}

function includeRefsFromContent(content) {
  const refs = [];
  const re = /^[ \t]*@@include[ \t]+\[([^\]\n]+)\][ \t]*$/gmi;
  let match;
  while ((match = re.exec(String(content || ""))) !== null) {
    const ref = String(match[1] || "").trim();
    if (ref) refs.push(ref);
  }
  return refs;
}

function markdownHrefPathOnly(raw) {
  return decodeRef(String(raw || "").trim().split(/[?#]/, 1)[0] || "");
}

function bookResolvePathFrom(baseFile, rawRef) {
  const protocol = hrefProtocol(rawRef);
  if (protocol && protocol !== "file") return "";
  const rawPath = markdownHrefPathOnly(rawRef);
  if (!rawPath) return "";
  const file = resolveInputPath(rawPath, dirname(baseFile));
  return inside(file, noteRoot) ? file : "";
}

function slugBookAnchor(value) {
  const slug = String(value || "")
    .replace(/\s+/g, " ")
    .trim()
    .toLowerCase()
    .replace(/[\u0000-\u001f\u007f]/g, "")
    .replace(/[!"#$%&'()*+,./:;<=>?@[\\\]^`{|}~]/g, "")
    .replace(/\s+/g, "-")
    .replace(/-+/g, "-")
    .replace(/^-|-$/g, "");
  return slug || "section";
}

const SEMANTIC_MARKDOWN_OFFSET = 5;
const SEMANTIC_SECTION_LEVELS = {
  "": 2,
  sec: 2,
  section: 2,
  sub: 3,
  subsub: 4,
  subsubsub: 5,
};

function semanticOutlineFromInlineCommand(command) {
  const name = String(command?.name || "").toLowerCase();
  const title = String(command?.context || "").trim() || "Untitled";
  if (name === "part") {
    return {
      level: 1,
      text: title,
      slug: String(command.args?.id || "").trim() || slugBookAnchor(title),
      source: "semantic",
      kind: "part",
    };
  }
  if (name !== "section") return null;
  const level = SEMANTIC_SECTION_LEVELS[String(command.switchValue || "").trim().toLowerCase()];
  if (!level) return null;
  return {
    level,
    text: title,
    slug: String(command.args?.id || "").trim() || slugBookAnchor(title),
    source: "semantic",
    kind: "section",
  };
}

function semanticBookHeadingsFromLine(line) {
  const trimmed = String(line || "").trim();
  if (!trimmed.startsWith("@@part") && !trimmed.startsWith("@@section")) return [];
  const command = scanInlineCommands(trimmed)[0];
  if (!command || command.fullFrom !== 0 || command.fullTo !== trimmed.length) return [];
  const outline = semanticOutlineFromInlineCommand(command);
  return outline ? [outline] : [];
}

function contentHasSemanticBookHeadings(lines) {
  let inFence = false;
  for (const line of lines) {
    if (/^\s*(```|~~~)/.test(line)) {
      inFence = !inFence;
      continue;
    }
    if (!inFence && semanticBookHeadingsFromLine(line).length > 0) return true;
  }
  return false;
}

function bookHeadingsFromContent(content, note, used) {
  const withoutMeta = removeMetaBlock(String(content || ""));
  const lines = withoutMeta.split(/\r?\n/);
  const hasSemantic = contentHasSemanticBookHeadings(lines);
  const headings = [];
  let hasH1 = false;
  let inFence = false;
  for (const line of lines) {
    if (/^\s*(```|~~~)/.test(line)) {
      inFence = !inFence;
      continue;
    }
    if (inFence) continue;
    for (const semantic of semanticBookHeadingsFromLine(line)) {
      let slug = semantic.slug || slugBookAnchor(semantic.text);
      const base = slug;
      for (let i = 2; used.has(slug); i++) slug = `${base}-${i}`;
      used.add(slug);
      headings.push({ ...semantic, slug, path: note.path || "", id: note.id || "" });
    }
    const match = line.match(/^\s{0,3}(#{1,6})\s+(.+?)\s*#*\s*$/);
    if (!match) continue;
    const markdownLevel = match[1].length;
    const level = hasSemantic ? SEMANTIC_MARKDOWN_OFFSET + markdownLevel : markdownLevel;
    if (level === 1) hasH1 = true;
    const text = match[2].trim() || "Untitled";
    let slug = slugBookAnchor(text);
    const base = slug;
    for (let i = 2; used.has(slug); i++) slug = `${base}-${i}`;
    used.add(slug);
    headings.push({ level, text, slug, path: note.path || "", id: note.id || "", source: "markdown" });
  }
  if (!hasSemantic && !hasH1 && note.title) {
    let slug = slugBookAnchor(note.title);
    const base = slug;
    for (let i = 2; used.has(slug); i++) slug = `${base}-${i}`;
    used.add(slug);
    headings.unshift({ level: 1, text: note.title, slug, path: note.path || "", id: note.id || "", source: "title" });
  }
  return headings;
}

function domTargetsFromContent(content, note) {
  const stack = [];
  const labelStack = [];
  return bookHeadingsFromContent(content, note, new Set()).map((heading) => {
    const label = String(heading.text || heading.slug || "").trim();
    const slug = String(heading.slug || slugBookAnchor(label)).trim();
    const level = Math.max(1, Number(heading.level || 1));
    while (stack.length > 0 && stack[stack.length - 1].level >= level) {
      stack.pop();
      labelStack.pop();
    }
    const parentPath = stack[stack.length - 1]?.path || [];
    const parentLabels = labelStack[labelStack.length - 1]?.path || [];
    const path = [...parentPath, slug].filter(Boolean);
    const labelPath = [...parentLabels, label].filter(Boolean);
    stack.push({ level, path });
    labelStack.push({ level, path: labelPath });
    return {
      label,
      slug,
      path,
      labelPath,
      level,
      notePath: note.path || "",
    };
  }).filter((target) => target.label && target.slug && target.path.length > 0);
}

function noteBookRefValues(note) {
  return [
    note?.id,
    note?.key,
    note?.title,
    note?.path,
    note?.link,
    note?.source,
    note?.file,
    note?.file ? basename(note.file) : "",
    ...(note?.aliases || []),
  ].filter((value) => String(value || "").trim());
}

function resolveBookRef(notes, ref, fromNote = null) {
  const raw = String(ref || "").trim();
  if (!raw) return null;
  if (fromNote?.file && (raw.includes("/") || /\.(?:md|markdown|typ)$/i.test(markdownHrefPathOnly(raw)))) {
    const file = bookResolvePathFrom(fromNote.file, raw);
    if (file) {
      const byFile = notes.find((note) => note.file === file);
      if (byFile) return byFile;
    }
  }
  const key = canonicalServerNoteRef(raw);
  if (!key) return null;
  return notes.find((note) => noteBookRefValues(note).some((value) => canonicalServerNoteRef(value) === key)) || null;
}

async function readNoteTextSafe(file) {
  try {
    return await readFile(file, "utf8");
  } catch {
    return "";
  }
}

function contentMayAffectBook(text) {
  const bookMeta = bookMetaFromContent(text);
  return Boolean(bookMeta.role || includeRefsFromContent(text).length > 0);
}

async function applyBookMetadata(notes) {
  const covers = notes.filter((note) => note.bookRole === "cover" && note.id);
  const notesByFile = new Map(notes.map((note) => [note.file, note]));
  const coverById = new Map(covers.map((note) => [note.id, note]));
  const diagnosticsByCover = new Map(covers.map((note) => [note.id, []]));
  const includedByCover = new Map(covers.map((note) => [note.id, []]));
  const treeByCover = new Map(covers.map((note) => [note.id, null]));

  for (const note of notes) {
    note.bookCoverId = note.bookRole === "cover" ? note.id : "";
    note.bookIncludedPaths = [];
    note.bookToc = [];
    note.bookDomTargets = [];
    note.bookRawRefs = [];
  }

  function markIncluded(note, cover, parent = null) {
    if (!note || !cover || note.file === cover.file) return;
    note.bookRole = "included";
    note.bookCoverId = cover.id;
    note.bookCoverPath = cover.path || "";
    note.bookParentPath = parent?.path || "";
    note.roam = false;
    const list = includedByCover.get(cover.id) || [];
    if (!list.some((item) => item.file === note.file)) list.push(note);
    includedByCover.set(cover.id, list);
  }

  function resolveInclude(note, rawRef, cover) {
    const file = bookResolvePathFrom(note.file, rawRef);
    if (!file) {
      diagnosticsByCover.get(cover.id)?.push({ level: "error", message: `Include is outside note root or invalid: ${rawRef}`, path: note.path || "" });
      return null;
    }
    const child = notesByFile.get(file);
    if (!child) {
      diagnosticsByCover.get(cover.id)?.push({ level: "error", message: `Included note not found in index: ${rawRef}`, path: note.path || "" });
      return null;
    }
    if (child.bookRole === "cover" && child.id !== cover.id) {
      diagnosticsByCover.get(cover.id)?.push({ level: "error", message: `Cannot include another book cover: ${child.path || rawRef}`, path: note.path || "" });
      return null;
    }
    return child;
  }

  function visitIncludeTree(cover, note, parent, stack) {
    if (stack.includes(note.file)) {
      diagnosticsByCover.get(cover.id)?.push({ level: "error", message: `Book include cycle: ${[...stack, note.file].map((file) => notesByFile.get(file)?.path || file).join(" -> ")}`, path: note.path || "" });
      return null;
    }
    if (note !== cover) markIncluded(note, cover, parent);
    const node = {
      id: note.id || "",
      title: note.title || "",
      path: note.path || "",
      role: note === cover ? "cover" : "included",
      children: [],
    };
    const nextStack = [...stack, note.file];
    for (const rawRef of note.bookIncludeRefs || []) {
      const child = resolveInclude(note, rawRef, cover);
      if (!child) continue;
      const childNode = visitIncludeTree(cover, child, note, nextStack);
      if (childNode) node.children.push(childNode);
    }
    return node;
  }

  function coverFromParentChain(note, seen = new Set()) {
    if (!note || note.bookRole !== "included" || !note.bookParentRef) return null;
    if (seen.has(note.file)) return null;
    seen.add(note.file);
    const parent = resolveBookRef(notes, note.bookParentRef, note);
    if (!parent) return null;
    if (parent.bookRole === "cover") return parent;
    return coverFromParentChain(parent, seen);
  }

  for (const cover of covers) {
    treeByCover.set(cover.id, visitIncludeTree(cover, cover, null, []));
  }
  for (const note of notes.filter((item) => item.bookRole === "included")) {
    const cover = coverById.get(note.bookCoverId) || coverFromParentChain(note);
    if (cover) markIncluded(note, cover, resolveBookRef(notes, note.bookParentRef, note));
  }

  for (const [coverId, included] of includedByCover.entries()) {
    const cover = coverById.get(coverId);
    if (!cover) continue;
    const bookNotes = [cover, ...included];
    const rawRefs = new Set(cover.refs || []);
    const rawRoamRefs = new Set();
    const inlineTags = new Set(cover.inlineTags || []);
    const summaries = [cover.summary || ""];
    const usedAnchors = new Set();
    const toc = [];
    for (const note of bookNotes) {
      const text = await readNoteTextSafe(note.file);
      for (const ref of refsFromContent(text)) rawRefs.add(ref);
      for (const ref of roamDbRefsFromContent(text)) rawRoamRefs.add(ref);
      for (const tag of inlineTagsFromContent(text)) inlineTags.add(tag);
      if (note !== cover && note.summary) summaries.push(note.summary);
      toc.push(...bookHeadingsFromContent(text, note, usedAnchors));
    }
    cover.refs = [...rawRefs].filter(Boolean);
    cover.bookRawRefs = [...rawRoamRefs].filter(Boolean);
    cover.inlineTags = [...inlineTags].filter(Boolean).sort((a, b) => a.localeCompare(b));
    cover.summary = summaries.join(" ").replace(/\s+/g, " ").trim().slice(0, 220);
    cover.bookRole = "cover";
    cover.bookCoverId = cover.id;
    cover.bookIncludedPaths = included.map((note) => note.path || "").filter(Boolean);
    cover.bookToc = toc;
    cover.bookDomTargets = toc.map((item) => ({ label: item.text, slug: item.slug, path: item.path, level: item.level }));
    cover.bookIncludeTree = treeByCover.get(cover.id);
    cover.bookDiagnostics = diagnosticsByCover.get(cover.id) || [];
  }

  await writeBookCaches(notes, covers, includedByCover, treeByCover, diagnosticsByCover);
}

async function fileDigestEntry(note) {
  try {
    const info = await stat(note.file);
    const text = await readFile(note.file, "utf8");
    return {
      path: note.path || "",
      file: note.file,
      mtimeMs: info.mtimeMs,
      size: info.size,
      sha256: sha256Text(text),
    };
  } catch {
    return { path: note.path || "", file: note.file, missing: true };
  }
}

async function writeJsonIfChanged(file, value) {
  const text = `${JSON.stringify(value, null, 2)}\n`;
  try {
    if (await readFile(file, "utf8") === text) return;
  } catch {}
  await atomicWriteFile(file, text, "utf8");
}

async function writeBookCaches(notes, covers, includedByCover, treeByCover, diagnosticsByCover) {
  if (covers.length === 0 && !existsSync(bookCacheDir())) return;
  await mkdir(bookCacheDir(), { recursive: true });
  const now = new Date().toISOString();
  const index = { schema: BOOK_CACHE_SCHEMA, updatedAt: now, books: {} };
  for (const cover of covers) {
    const included = includedByCover.get(cover.id) || [];
    const bookNotes = [cover, ...included];
    const files = [];
    for (const note of bookNotes) files.push(await fileDigestEntry(note));
    const cacheFile = safeBookCacheName(cover.id);
    const cache = {
      schema: BOOK_CACHE_SCHEMA,
      id: cover.id,
      title: cover.title || "Untitled",
      coverPath: cover.path || "",
      coverFile: cover.file || "",
      updatedAt: now,
      files,
      includeTree: treeByCover.get(cover.id),
      toc: cover.bookToc || [],
      anchors: cover.bookDomTargets || [],
      diagnostics: diagnosticsByCover.get(cover.id) || [],
      hash: sha256Text(JSON.stringify(files.map((item) => [item.path, item.sha256 || "", item.mtimeMs || 0, item.size || 0]))),
    };
    await writeJsonIfChanged(join(bookCacheDir(), cacheFile), cache);
    index.books[cover.id] = {
      id: cover.id,
      title: cover.title || "Untitled",
      coverPath: cover.path || "",
      cacheFile,
      updatedAt: now,
    };
  }
  await writeJsonIfChanged(join(bookCacheDir(), "index.json"), index);
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

export async function exportPdf(file, content) {
  const dir = await runtimeMkdtemp("pdf", file || "Aaronnote.pdf");
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
  return meta.id || yamlishValue(content, "id") || relativeCanonical(root, file);
}

export function tagsFromContent(content) {
  const meta = noteMetadata(content);
  const tags = Array.isArray(meta.tags) ? [...meta.tags] : [];
  const lines = content.split(/\r?\n/);
  if (!Array.isArray(meta.tags)) {
    const start = lines.findIndex((line) => /^\s*tags:\s*$/.test(line));
    if (start >= 0) {
      for (const line of lines.slice(start + 1)) {
        const item = line.match(/^\s*-\s*(.+)$/);
        if (!item) break;
        tags.push(item[1].trim());
      }
    }
  }
  return normalizeTags(tags);
}

export function inlineTagsFromContent(content) {
  const tags = [];
  let inFence = false;
  for (const line of String(content || "").split(/\r?\n/)) {
    if (/^\s*(```|~~~)/.test(line)) {
      inFence = !inFence;
      continue;
    }
    if (inFence) continue;
    const stripped = line.replace(/`[^`\n]*`/g, "");
    for (const command of scanInlineCommands(stripped, "tag")) {
      const tag = String(command.context || "").trim();
      if (tag) tags.push(tag);
    }
  }
  return tags;
}

function decodeRef(ref) {
  let decoded = String(ref || "");
  try {
    decoded = decodeURIComponent(decoded);
  } catch {
    decoded = String(ref || "");
  }
  return decoded.replace(/\\([\\`*_[\](){}#+.!<>-])/g, "$1");
}

function refFromRoamHref(href) {
  const match = String(href || "").trim().match(/^roam:\/\/(.+)$/i);
  if (!match) return "";
  return refFromRoamLikeHref(String(href || "").trim());
}

function hrefProtocol(href) {
  return String(href || "").trim().match(/^([A-Za-z][\w+.-]*):/)?.[1]?.toLowerCase() || "";
}

function hrefPath(href) {
  const raw = String(href || "").trim();
  if (/^file:\/\//i.test(raw)) {
    try {
      return decodeRef(new URL(raw).pathname);
    } catch {
      return decodeRef(raw.replace(/^file:\/\//i, ""));
    }
  }
  if (/^file:/i.test(raw)) return decodeRef(raw.replace(/^file:/i, "").split(/[?#]/, 1)[0] || "");
  return decodeRef(raw.split(/[?#]/, 1)[0] || "");
}

function stripDomTargetFromPath(path) {
  const clean = String(path || "");
  const match = clean.match(/^(.+?\.(?:md|markdown|typ))@/i);
  if (match) return match[1];
  return clean;
}

function noteFileRefFromHref(href) {
  const protocol = hrefProtocol(href);
  if (protocol && protocol !== "file") return "";
  const path = stripDomTargetFromPath(hrefPath(href));
  return /\.(?:md|markdown|typ)$/i.test(path) ? path : "";
}

function refFromRoamLikeHref(href) {
  const raw = String(href || "").trim();
  const protocol = hrefProtocol(raw);
  if (protocol && protocol !== "roam") return "";
  if (protocol !== "roam" && !raw.includes("#") && !raw.includes("@")) return "";
  let body = raw.replace(/^roam:\/\//i, "");
  body = body.split(/[?&]/, 1)[0] || body;
  const hashIndex = body.indexOf("#");
  if (hashIndex >= 0) body = body.slice(0, hashIndex);
  const fileDomMatch = body.match(/^(.+?\.(?:md|markdown|typ))@/i);
  if (fileDomMatch) body = fileDomMatch[1];
  else {
    const atIndex = body.indexOf("@");
    if (atIndex >= 0) body = body.slice(0, atIndex);
  }
  const ref = decodeRef(body.replace(/^\/+/, "").replace(/[.,;:]+$/, "")).trim();
  if (!ref || ref === "." || ref === "./") return "";
  return ref;
}

function markdownEscapedAt(text, pos) {
  let slashes = 0;
  for (let i = pos - 1; i >= 0 && text[i] === "\\"; i--) slashes++;
  return slashes % 2 === 1;
}

function markdownLabelClose(text, open) {
  let depth = 0;
  for (let i = open + 1; i < text.length; i++) {
    const ch = text[i];
    if (ch === "\\" && i + 1 < text.length) {
      i++;
      continue;
    }
    if (ch === "[") {
      depth++;
      continue;
    }
    if (ch !== "]") continue;
    if (depth === 0) return i;
    depth--;
  }
  return -1;
}

function skipMarkdownSpaces(text, pos) {
  while (pos < text.length && /[ \t]/.test(text[pos])) pos++;
  return pos;
}

function parseMarkdownTitle(text, pos) {
  if (text[pos] !== '"') return null;
  let title = "";
  for (let i = pos + 1; i < text.length; i++) {
    const ch = text[i];
    if (ch === "\\" && i + 1 < text.length) {
      title += ch + text[i + 1];
      i++;
      continue;
    }
    if (ch === '"') return { title, end: i + 1 };
    if (ch === "\n" || ch === "\r") return null;
    title += ch;
  }
  return null;
}

function parseMarkdownDestination(text, pos) {
  let cursor = skipMarkdownSpaces(text, pos);
  let href = "";
  let hrefFrom = cursor;
  let hrefTo = cursor;
  if (text[cursor] === ")") return { href, end: cursor + 1 };
  if (text[cursor] === "<") {
    let end = -1;
    for (let i = cursor + 1; i < text.length; i++) {
      const ch = text[i];
      if (ch === "\n" || ch === "\r") return null;
      if (ch === ">" && !markdownEscapedAt(text, i)) {
        end = i;
        break;
      }
    }
    if (end < 0) return null;
    hrefFrom = cursor + 1;
    hrefTo = end;
    href = text.slice(cursor + 1, end);
    cursor = end + 1;
  } else {
    const start = cursor;
    let depth = 0;
    for (; cursor < text.length; cursor++) {
      const ch = text[cursor];
      if (ch === "\n" || ch === "\r") return null;
      if (ch === "\\" && cursor + 1 < text.length) {
        cursor++;
        continue;
      }
      if (ch === "(") {
        depth++;
        continue;
      }
      if (ch === ")") {
        if (depth === 0) break;
        depth--;
        continue;
      }
      if (depth === 0 && /[ \t]/.test(ch)) break;
    }
    hrefFrom = start;
    hrefTo = cursor;
    href = text.slice(start, cursor);
  }
  cursor = skipMarkdownSpaces(text, cursor);
  if (text[cursor] !== ")") {
    const title = parseMarkdownTitle(text, cursor);
    if (!title) return null;
    cursor = skipMarkdownSpaces(text, title.end);
  }
  if (text[cursor] !== ")") return null;
  return { href, hrefFrom, hrefTo, end: cursor + 1 };
}

function markdownLinkHrefs(text) {
  const hrefs = [];
  for (let i = 0; i < text.length; i++) {
    if (text[i] !== "[" || markdownEscapedAt(text, i)) continue;
    const labelClose = markdownLabelClose(text, i);
    if (labelClose < 0 || text[labelClose + 1] !== "(") continue;
    const dest = parseMarkdownDestination(text, labelClose + 2);
    if (!dest) continue;
    hrefs.push(dest.href);
    i = dest.end - 1;
  }
  return hrefs;
}

function markdownLinkDestinations(text) {
  const destinations = [];
  for (let i = 0; i < text.length; i++) {
    if (text[i] !== "[" || markdownEscapedAt(text, i)) continue;
    const labelClose = markdownLabelClose(text, i);
    if (labelClose < 0 || text[labelClose + 1] !== "(") continue;
    const dest = parseMarkdownDestination(text, labelClose + 2);
    if (!dest) continue;
    destinations.push(dest);
    i = dest.end - 1;
  }
  return destinations;
}

export function refsFromContent(content) {
  const meta = noteMetadata(content);
  const refs = new Set(Array.isArray(meta.refs) ? meta.refs : []);
  refTokenPattern.lastIndex = 0;
  let match;
  while ((match = refTokenPattern.exec(content)) !== null) {
    if (match[1]) refs.add(match[1]);
    else if (match[2]) refs.add(match[2].trim());
    else refs.add(refFromRoamHref(match[0]));
  }
  for (const href of markdownLinkHrefs(content)) {
    const noteRef = noteFileRefFromHref(href);
    if (noteRef) refs.add(noteRef);
    const roamRef = refFromRoamLikeHref(href);
    if (roamRef) refs.add(roamRef);
  }
  return [...refs].filter(Boolean);
}

export function roamDbRefsFromContent(content) {
  const meta = noteMetadata(content);
  const refs = new Set(Array.isArray(meta.refs) ? meta.refs : []);
  refTokenPattern.lastIndex = 0;
  let match;
  while ((match = refTokenPattern.exec(content)) !== null) {
    if (match[1]) refs.add(match[1]);
    else if (match[2]) refs.add(match[2].trim());
    else refs.add(refFromRoamHref(match[0]));
  }
  for (const href of markdownLinkHrefs(content)) {
    if (noteFileRefFromHref(href)) continue;
    const roamRef = refFromRoamLikeHref(href);
    if (roamRef) refs.add(roamRef);
  }
  return [...refs].filter(Boolean);
}

function aliasesFromContent(content) {
  const meta = noteMetadata(content);
  return Array.isArray(meta.aliases) ? meta.aliases : [];
}

function graphNoteKey(note) {
  return String(note.key || note.id || note.path || note.file || "").trim();
}

export function graphPayload(notes) {
  const graphNotes = notes.filter((note) => note.roam);
  const byId = new Map();
  for (const note of graphNotes) {
    const key = graphNoteKey(note);
    if (!key) continue;
    for (const ref of [key, note.id, note.path, note.link, note.source, note.file].filter(Boolean)) {
      byId.set(String(ref), key);
    }
  }
  const edges = [];
  for (const note of graphNotes) {
    const source = graphNoteKey(note);
    if (!source) continue;
    for (const ref of note.refs || []) {
      const target = byId.get(String(ref));
      if (target && target !== source) edges.push({ source, target });
    }
  }
  const tags = [...new Set(graphNotes.flatMap((note) => note.tags || []))].sort();
  return {
    type: "graph",
    meta: {
      generatedAt: new Date().toISOString(),
      noteCount: graphNotes.length,
      edgeCount: edges.length,
      tagCount: tags.length,
    },
    nodes: graphNotes.map((note) => ({
      key: graphNoteKey(note),
      id: note.id || "",
      title: note.title || "",
      path: note.path || "",
      link: note.link || note.path || "#",
      groupKey: note.groupKey || "",
      groupLabel: note.groupLabel || "",
      tags: note.tags || [],
      aliases: note.aliases || [],
    })),
    edges,
  };
}

export function wantedPages(notes) {
  const graphNotes = notes.filter((note) => note.roam);
  const byId = new Map();
  for (const note of graphNotes) {
    const key = graphNoteKey(note);
    if (!key) continue;
    for (const ref of [key, note.id, note.path, note.link, note.source, note.file].filter(Boolean)) {
      byId.set(String(ref), key);
    }
  }
  const wantedMap = new Map();
  for (const note of graphNotes) {
    const source = graphNoteKey(note);
    if (!source) continue;
    for (const ref of note.refs || []) {
      const strRef = String(ref);
      if (!byId.has(strRef)) {
        const entry = wantedMap.get(strRef) ?? { target: ref, by: new Set() };
        entry.by.add(source);
        wantedMap.set(strRef, entry);
      }
    }
  }
  return {
    type: "wanted-pages",
    items: [...wantedMap.values()].map((entry) => ({
      target: entry.target,
      by: [...entry.by],
    })),
  };
}

export function tagIndexPayload(notes) {
  const tags = new Map();
  const add = (tag, note, kind) => {
    const name = String(tag || "").trim();
    const key = graphNoteKey(note);
    if (!name || !key) return;
    const lower = name.toLowerCase();
    const entry = tags.get(lower) ?? { name, count: 0, notes: [], metaCount: 0, inlineCount: 0 };
    if (!entry.notes.some((item) => item.key === key)) {
      entry.notes.push({ key, id: note.id || "", title: note.title || "", path: note.path || "" });
      entry.count++;
    }
    if (kind === "inline") entry.inlineCount++;
    else entry.metaCount++;
    tags.set(lower, entry);
  };
  for (const note of notes.filter((item) => item.roam)) {
    for (const tag of note.tags || []) add(tag, note, "meta");
    for (const tag of note.inlineTags || []) add(tag, note, "inline");
  }
  const items = [...tags.values()]
    .map((entry) => ({
      ...entry,
      notes: entry.notes.sort((a, b) => a.title.localeCompare(b.title) || a.key.localeCompare(b.key)),
    }))
    .sort((a, b) => b.count - a.count || a.name.localeCompare(b.name));
  return {
    type: "tags",
    tags: items,
    meta: {
      tagCount: items.length,
      noteCount: new Set(items.flatMap((entry) => entry.notes.map((note) => note.key))).size,
    },
  };
}

function dateFromContent(content) {
  const meta = noteMetadata(content);
  return String(meta.date || yamlishValue(content, "date") || "");
}

function sourceFromContent(content) {
  const meta = noteMetadata(content);
  return String(meta.source || "");
}

function normalizeNoteKind(value) {
  const item = Array.isArray(value) ? value[0] : value;
  const kind = String(item || "").trim().replace(/\\_/g, "_").toLowerCase();
  if (defaultNoteKindAliases.has(kind)) return defaultNoteKind;
  return noteKindPattern.test(kind) ? kind : defaultNoteKind;
}

export function kindFromContent(content) {
  const meta = noteMetadata(content);
  return normalizeNoteKind(meta.kind ?? meta.kinds ?? defaultNoteKind);
}

export function activeKindFromContent(content) {
  const kind = kindFromContent(content);
  return kind === defaultNoteKind ? "" : kind;
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

function groupKeyFor(file, root = noteScanRoot) {
  const parent = dirname(displayPathForScanRoot(file, root));
  return parent === "." ? "Root" : parent;
}

function groupLabelFor(groupKey) {
  if (!groupKey || groupKey === "Root") return "Root";
  const leaf = groupKey.split(sep).filter(Boolean).at(-1) || groupKey;
  return leaf.toUpperCase() === leaf ? leaf : leaf.replace(/[-_]/g, " ").replace(/\b\w/g, (char) => char.toUpperCase());
}

function directoryPathParts(path) {
  return String(path || "")
    .replace(/^\.\/?/, "")
    .split(/[\\/]/)
    .filter(Boolean);
}

function directoryParentPath(path) {
  const parts = directoryPathParts(path);
  if (parts.length <= 1) return "Root";
  return parts.slice(0, -1).join("/");
}

function directoryAncestors(path) {
  if (!path || path === "Root") return ["Root"];
  const parts = directoryPathParts(path);
  const out = ["Root"];
  for (let i = 1; i <= parts.length; i++) out.push(parts.slice(0, i).join("/"));
  return out;
}

function generatedPathPart(path) {
  return directoryPathParts(path).some((part) => generatedAttachmentDirs.has(part.toLowerCase()));
}

function ensureDirectory(map, path, generated = false) {
  const key = path && path !== "." ? path : "Root";
  const existing = map.get(key);
  if (existing) {
    existing.generated = existing.generated || generated;
    return existing;
  }
  const entry = {
    path: key,
    label: groupLabelFor(key),
    parent: directoryParentPath(key),
    noteCount: 0,
    fileCount: 0,
    generated,
  };
  map.set(key, entry);
  return entry;
}

async function scanFilesystemEntries(notes = []) {
  const directories = new Map();
  const files = [];
  ensureDirectory(directories, "Root");

  async function walk(dir, generatedParent = false) {
    const rel = displayPathForScanRoot(dir, noteScanRoot);
    const dirPath = rel ? rel : "Root";
    const generated = generatedParent || generatedPathPart(dirPath);
    ensureDirectory(directories, dirPath, generated);

    let entries = [];
    try {
      entries = await readdir(dir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const entry of entries) {
      if (entry.name.startsWith(".") && entry.name !== ".emacs.d") continue;
      if (entry.isDirectory() && excludedDirs.has(entry.name)) continue;
      const full = join(dir, entry.name);
      if (!inside(full, noteScanRoot)) continue;
      const childGenerated = generated || generatedAttachmentDirs.has(entry.name.toLowerCase());
      if (entry.isDirectory()) {
        await walk(full, childGenerated);
      } else if (entry.isFile()) {
        const ext = extname(entry.name).toLowerCase();
        if (ext === ".lean" || noteExts.has(ext) || entry.name === ".aaronnote-keep") continue;
        try {
          const info = await stat(full);
          const path = displayPathForScanRoot(full, noteScanRoot);
          const dirPath = groupKeyFor(full, noteScanRoot);
          files.push({
            file: full,
            path,
            name: basename(full),
            ext: ext.replace(/^\./, ""),
            type: fileContentType(full),
            size: info.size,
            mtimeMs: info.mtimeMs,
            groupKey: dirPath,
            groupLabel: groupLabelFor(dirPath),
            generated: childGenerated || generatedPathPart(path),
          });
          for (const ancestor of directoryAncestors(dirPath)) {
            ensureDirectory(directories, ancestor, generatedPathPart(ancestor)).fileCount += 1;
          }
        } catch {}
      }
    }
  }

  await walk(noteScanRoot);

  for (const note of notes) {
    const group = note.groupKey || groupKeyFor(note.file || "", noteScanRoot);
    for (const ancestor of directoryAncestors(group)) {
      ensureDirectory(directories, ancestor, generatedPathPart(ancestor)).noteCount += 1;
    }
  }

  return {
    directories: [...directories.values()].sort((a, b) => {
      if (a.path === "Root") return -1;
      if (b.path === "Root") return 1;
      return a.path.localeCompare(b.path);
    }),
    files: files.sort((a, b) => a.path.localeCompare(b.path)),
  };
}

export async function notesIndexPayload(notes = null) {
  const scanned = notes ?? await scanNotes();
  const fs = await scanFilesystemEntries(scanned);
  return { notes: scanned, directories: fs.directories, files: fs.files, indexVersion: notesIndexVersionValue() };
}

function preferNote(candidate, current) {
  if (!current) return candidate;
  if (candidate.ext === "md" && current.ext !== "md") return candidate;
  if (candidate.path && candidate.path === current.source) return candidate;
  return current;
}

function normalizeNoteRefPath(value) {
  const raw = String(value || "").replace(/\\/g, "/");
  const absolute = raw.startsWith("/");
  const parts = [];
  for (const part of raw.split("/")) {
    if (!part || part === ".") continue;
    if (part === "..") {
      if (parts.length > 0 && parts[parts.length - 1] !== "..") parts.pop();
      else if (!absolute) parts.push(part);
      continue;
    }
    parts.push(part);
  }
  return `${absolute ? "/" : ""}${parts.join("/")}`;
}

function canonicalServerNoteRef(value) {
  const roamRef = refFromRoamLikeHref(String(value || ""));
  const ref = roamRef || String(value || "");
  return normalizeNoteRefPath(decodeRef(ref).trim().replace(/^\.\/+/, "")).toLowerCase();
}

function serverNoteRefValues(note) {
  const file = String(note?.file || "");
  const base = file.split(/[\\/]/).filter(Boolean).at(-1) || "";
  return [
    note?.id,
    note?.key,
    note?.title,
    note?.path,
    note?.link,
    note?.source,
    note?.file,
    base,
    ...(note?.aliases || []),
  ].filter((value) => String(value || "").trim());
}

function serverNoteReferenceIndex(notes) {
  const index = new Map();
  const byId = new Map(notes.map((note) => [String(note.id || ""), note]));
  for (const note of notes) {
    const target = note.bookRole === "included" && note.bookCoverId
      ? byId.get(String(note.bookCoverId)) || note
      : note;
    for (const value of serverNoteRefValues(note)) {
      const key = canonicalServerNoteRef(value);
      if (key && !index.has(key)) index.set(key, target);
    }
  }
  return index;
}

function cloneNote(note) {
  return {
    ...note,
    aliases: [...(note.aliases || [])],
    tags: [...(note.tags || [])],
    inlineTags: [...(note.inlineTags || [])],
    refs: [...(note.refs || [])],
    backlinks: [...(note.backlinks || [])],
    bookIncludeRefs: [...(note.bookIncludeRefs || [])],
    bookIncludedPaths: [...(note.bookIncludedPaths || [])],
    bookToc: [...(note.bookToc || [])],
    bookDomTargets: [...(note.bookDomTargets || [])],
    bookRawRefs: [...(note.bookRawRefs || [])],
    bookDiagnostics: [...(note.bookDiagnostics || [])],
    leanBlocks: [...(note.leanBlocks || [])],
  };
}

function cloneNotes(notes) {
  return notes.map(cloneNote);
}

// Monotonically-increasing version counter. Bumped on every markNotesDirty()
// so clients can detect external index changes via the indexVersion field in
// notesIndexPayload() responses and refresh without polling.
let notesIndexVersion = 1;
export function notesIndexVersionValue() { return notesIndexVersion; }

// Registry of files the server wrote itself (atomic renames). The watcher
// ignores self-writes within a 2-second window to avoid triggering redundant
// re-scans immediately after save. Capped at 256 entries to prevent unbounded growth.
const recentSelfWrites = new Map();
export function noteSelfWrite(file) {
  recentSelfWrites.set(file, Date.now());
  if (recentSelfWrites.size > 256) {
    // Delete the oldest entry
    const oldest = recentSelfWrites.keys().next().value;
    recentSelfWrites.delete(oldest);
  }
}
export function noteSelfWriteRecently(file, windowMs = 2000) {
  const ts = recentSelfWrites.get(file);
  if (!ts) return false;
  if (Date.now() - ts > windowMs) { recentSelfWrites.delete(file); return false; }
  return true;
}

// Whether a vault-relative path is eligible to affect the note index.
// Mirrors the filter in walkFiles so the watcher and the scanner agree.
export function notePathWatchRelevant(relPath) {
  if (!relPath) return false;
  const parts = String(relPath).replace(/\\/g, "/").split("/").filter(Boolean);
  if (parts.some((p) => excludedDirs.has(p) || p.startsWith("."))) return false;
  const name = parts[parts.length - 1] || "";
  const dot = name.lastIndexOf(".");
  return dot >= 0 && noteExts.has(name.slice(dot).toLowerCase());
}

export function markNotesDirty(file = "") {
  notesIndexVersion++;
  pathSuggestionDirListingCache.clear();
  notesSnapshotDirty = true;
  if (file && inside(file, noteScanRoot)) {
    dirtyNoteFiles.add(file);
    noteCache.delete(file);
  } else {
    notesSnapshotFullDirty = true;
    dirtyNoteFiles = new Set();
  }
}

function notePathMayAffectIndex(file) {
  if (!file) return true;
  const dot = file.lastIndexOf(".");
  return dot >= 0 && noteExts.has(file.slice(dot).toLowerCase());
}

async function noteFromFileForIndex(file) {
  try {
    const info = await stat(file);
    if (!info.isFile()) return null;
    const cached = noteCache.get(file);
    if (cached && cached.mtimeMs === info.mtimeMs && cached.size === info.size) {
      return { ...cached.note, backlinks: [] };
    }
    const content = await readFile(file, "utf8");
    const relPath = displayPathForScanRoot(file, noteScanRoot);
    const groupKey = groupKeyFor(file, noteScanRoot);
    const id = idFromContent(file, noteScanRoot, content);
    const bookMeta = bookMetaFromContent(content);
    const roam = hasRoamMeta(content);
    const inlineTags = inlineTagsFromContent(content);
    const leanBlocks = [];
    const note = {
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
      inlineTags,
      refs: refsFromContent(content),
      backlinks: [],
      roam,
      bookRole: bookMeta.role,
      bookParentRef: bookMeta.parentRef,
      bookCoverId: bookMeta.role === "cover" ? id : "",
      bookCoverPath: "",
      bookParentPath: "",
      bookIncludeRefs: includeRefsFromContent(content),
      bookIncludedPaths: [],
      bookToc: [],
      bookDomTargets: [],
      domTargets: [],
      bookRawRefs: [],
      bookDiagnostics: [],
      leanBlocks,
      standalone: standaloneFile(file),
    };
    note.domTargets = domTargetsFromContent(content, note);
    const todoContent = contentMayHaveTodos(content) ? content : "";
    noteCache.set(file, {
      mtimeMs: info.mtimeMs,
      size: info.size,
      note,
      todos: todoContent ? null : [],
      todoContent,
    });
    return { ...note };
  } catch {
    noteCache.delete(file);
    return null;
  }
}

function resolveNoteRelationships(notes) {
  const uniqueNotes = [...notes.reduce((map, note) => {
    map.set(note.id, preferNote({ ...note, backlinks: [] }, map.get(note.id)));
    return map;
  }, new Map()).values()];
  const refsByKey = serverNoteReferenceIndex(uniqueNotes);
  for (const note of uniqueNotes) {
    const resolved = [];
    for (const ref of note.refs || []) {
      const target = refsByKey.get(canonicalServerNoteRef(ref));
      if (!target || target.id === note.id) continue;
      resolved.push(target.id);
      target.backlinks.push(note.id);
    }
    note.refs = [...new Set(resolved)].sort();
  }
  for (const note of uniqueNotes) note.backlinks = [...new Set(note.backlinks)].sort();
  return uniqueNotes.sort((a, b) => a.title.localeCompare(b.title));
}

function sortedUniqueStrings(values) {
  return [...new Set(values.map((value) => String(value || "")).filter(Boolean))].sort();
}

function sameStringList(a = [], b = []) {
  if (a.length !== b.length) return false;
  return a.every((value, index) => value === b[index]);
}

function noteRefIdentityKeys(note) {
  return sortedUniqueStrings(serverNoteRefValues(note).map((value) => canonicalServerNoteRef(value)));
}

function buildRelationshipCache(resolvedNotes) {
  const targetByCanonical = new Map();
  const refKeysById = new Map();
  for (const note of resolvedNotes) {
    refKeysById.set(note.id, noteRefIdentityKeys(note));
    for (const value of serverNoteRefValues(note)) {
      const key = canonicalServerNoteRef(value);
      if (key && !targetByCanonical.has(key)) targetByCanonical.set(key, note.id);
    }
  }
  return { targetByCanonical, refKeysById };
}

function resolvedRefsForRawNote(note, targetByCanonical) {
  const resolved = [];
  for (const ref of note.refs || []) {
    const targetId = targetByCanonical.get(canonicalServerNoteRef(ref));
    if (targetId && targetId !== note.id) resolved.push(targetId);
  }
  return sortedUniqueStrings(resolved);
}

function sortResolvedNotes(notes) {
  for (const note of notes) {
    note.refs = sortedUniqueStrings(note.refs || []);
    note.backlinks = sortedUniqueStrings(note.backlinks || []);
  }
  return notes.sort((a, b) => a.title.localeCompare(b.title));
}

function patchResolvedRelationships(previousResolved, dirtyFiles, dirtyRawNotes) {
  if (!notesRelationshipCache) return null;
  const oldByFile = new Map(previousResolved.map((note) => [note.file, note]));
  const nextById = new Map(previousResolved.map((note) => [note.id, cloneNote(note)]));
  const rawByFile = new Map(dirtyRawNotes.filter(Boolean).map((note) => [note.file, note]));

  for (const file of dirtyFiles) {
    const oldNote = oldByFile.get(file);
    const rawNote = rawByFile.get(file);
    if (!oldNote || !rawNote || oldNote.id !== rawNote.id) return null;
    const oldKeys = notesRelationshipCache.refKeysById.get(oldNote.id) ?? noteRefIdentityKeys(oldNote);
    const newKeys = noteRefIdentityKeys(rawNote);
    if (!sameStringList(oldKeys, newKeys)) return null;
  }

  for (const file of dirtyFiles) {
    const oldNote = oldByFile.get(file);
    const rawNote = rawByFile.get(file);
    const previous = nextById.get(oldNote.id);
    if (!previous) return null;

    const oldRefs = new Set(previous.refs || []);
    const newRefs = new Set(resolvedRefsForRawNote(rawNote, notesRelationshipCache.targetByCanonical));
    const nextNote = { ...cloneNote(rawNote), refs: [...newRefs], backlinks: [...(previous.backlinks || [])] };

    for (const ref of oldRefs) {
      if (newRefs.has(ref)) continue;
      const target = nextById.get(ref);
      if (target) target.backlinks = (target.backlinks || []).filter((id) => id !== oldNote.id);
    }
    for (const ref of newRefs) {
      if (oldRefs.has(ref)) continue;
      const target = nextById.get(ref);
      if (target && !(target.backlinks || []).includes(oldNote.id)) target.backlinks.push(oldNote.id);
    }
    nextById.set(oldNote.id, nextNote);
  }

  return sortResolvedNotes([...nextById.values()]);
}

function rememberNoteSnapshots(rawNotes, resolvedNotes) {
  notesRawSnapshot = rawNotes;
  notesSnapshot = resolvedNotes;
  notesRelationshipCache = buildRelationshipCache(resolvedNotes);
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

async function mapLimit(items, limit, mapper) {
  const out = new Array(items.length);
  let next = 0;
  const workers = Array.from({ length: Math.min(limit, items.length) }, async () => {
    while (next < items.length) {
      const index = next++;
      out[index] = await mapper(items[index], index);
    }
  });
  await Promise.all(workers);
  return out;
}

export async function scanNotes() {
  if (noteCacheRoot !== noteScanRoot) {
    noteCacheRoot = noteScanRoot;
    noteCache = new Map();
    notesSnapshotRoot = noteScanRoot;
    notesSnapshot = null;
    notesRawSnapshot = null;
    notesRelationshipCache = null;
    notesSnapshotDirty = true;
    notesSnapshotFullDirty = true;
    dirtyNoteFiles = new Set();
  }
  if (notesSnapshotRoot === noteScanRoot && notesSnapshot && !notesSnapshotDirty) {
    return cloneNotes(notesSnapshot);
  }
  notesSnapshotRoot = noteScanRoot;

  if (notesSnapshot && !notesSnapshotFullDirty && dirtyNoteFiles.size > 0) {
    const dirty = [...dirtyNoteFiles].filter((file) => notePathMayAffectIndex(file));
    dirtyNoteFiles = new Set();
    const dirtySet = new Set(dirty.map(canonicalExistingPath));
    const rawNotes = cloneNotes(notesRawSnapshot || [])
      .filter((note) => !dirtySet.has(canonicalExistingPath(note.file)));
    const dirtyNotes = [];
    for (const file of dirty) {
      const note = await noteFromFileForIndex(file);
      if (note) {
        rawNotes.push(note);
        dirtyNotes.push(note);
      }
    }
    await applyBookMetadata(rawNotes);
    const sorted = resolveNoteRelationships(rawNotes);
    rememberNoteSnapshots(rawNotes, sorted);
    notesSnapshotDirty = false;
    return cloneNotes(sorted);
  }

  const files = await walkFiles(noteScanRoot, (file) => {
    const dot = file.lastIndexOf(".");
    return dot >= 0 && noteExts.has(file.slice(dot).toLowerCase());
  });
  const notes = [];
  const seen = new Set(files);
  const scanned = await mapLimit(files, scanConcurrency, async (file) => {
    return noteFromFileForIndex(file);
  });
  for (const note of scanned) if (note) notes.push(note);
  for (const file of noteCache.keys()) {
    if (!seen.has(file)) noteCache.delete(file);
  }
  await applyBookMetadata(notes);
  const sorted = resolveNoteRelationships(notes);
  rememberNoteSnapshots(notes, sorted);
  notesSnapshotDirty = false;
  notesSnapshotFullDirty = false;
  dirtyNoteFiles = new Set();
  return cloneNotes(sorted);
}

// Always scan from noteRoot regardless of the current standalone-file scan context.
// The graph view is a global roam view and must not inherit a standalone file's directory.
export async function scanRoamNotes() {
  const prev = noteScanRoot;
  if (prev !== noteRoot) {
    noteScanRoot = noteRoot;
    notesSnapshotDirty = true;
  }
  try {
    return await scanNotes();
  } finally {
    if (prev !== noteRoot) {
      noteScanRoot = prev;
      notesSnapshotDirty = true;
    }
  }
}

const todoStatuses = new Set(["todo", "doing", "done", "blocked"]);

const DATE_KEYS = new Set(["ddl", "due", "deadline", "scheduled", "start", "done", "date", "when"]);

function midnightMs(d) {
  return new Date(d.getFullYear(), d.getMonth(), d.getDate()).getTime();
}

function pad2(n) { return String(n).padStart(2, "0"); }

export function parseDateValue(raw) {
  const t = String(raw ?? "").trim();
  if (!t) return null;
  const lower = t.toLowerCase();
  if (lower === "today" || lower === "今天") return { time: midnightMs(new Date()), hasTime: false };
  if (lower === "tomorrow" || lower === "明天") return { time: midnightMs(new Date()) + 86_400_000, hasTime: false };
  if (lower === "yesterday" || lower === "昨天") return { time: midnightMs(new Date()) - 86_400_000, hasTime: false };
  if (lower === "now") return { time: Date.now(), hasTime: true };
  const rel = lower.match(/^([+-])(\d+)\s*(d|day|days|w|week|weeks|m|month|months|y|year|years)$/);
  if (rel) {
    const sign = rel[1] === "-" ? -1 : 1;
    const n = Number(rel[2]) * sign;
    const u = rel[3];
    const base = new Date();
    base.setHours(0, 0, 0, 0);
    if (u.startsWith("d")) base.setDate(base.getDate() + n);
    else if (u.startsWith("w")) base.setDate(base.getDate() + 7 * n);
    else if (u.startsWith("m")) base.setMonth(base.getMonth() + n);
    else if (u.startsWith("y")) base.setFullYear(base.getFullYear() + n);
    return { time: base.getTime(), hasTime: false };
  }
  const cjk = t.replace(/年|月/g, "-").replace(/日|号/g, "");
  const norm = cjk.replace(/[./]/g, "-").trim();
  let m = norm.match(/^(\d{4})-(\d{1,2})(?:-(\d{1,2}))?(?:[\sT](\d{1,2}):(\d{2}))?$/);
  if (m) {
    const y = Number(m[1]);
    const mo = Number(m[2]) - 1;
    const d = m[3] ? Number(m[3]) : 1;
    const hh = m[4] ? Number(m[4]) : 0;
    const mm = m[5] ? Number(m[5]) : 0;
    const date = new Date(y, mo, d, hh, mm);
    if (Number.isFinite(date.getTime())) return { time: date.getTime(), hasTime: Boolean(m[4]) };
  }
  m = norm.match(/^(\d{1,2})-(\d{1,2})(?:[\sT](\d{1,2}):(\d{2}))?$/);
  if (m) {
    const mo = Number(m[1]) - 1;
    const d = Number(m[2]);
    const hh = m[3] ? Number(m[3]) : 0;
    const mm = m[4] ? Number(m[4]) : 0;
    if (mo >= 0 && mo < 12 && d >= 1 && d <= 31) {
      const date = new Date(new Date().getFullYear(), mo, d, hh, mm);
      return { time: date.getTime(), hasTime: Boolean(m[3]) };
    }
  }
  const parsed = Date.parse(t);
  if (Number.isFinite(parsed)) return { time: parsed, hasTime: /\d{1,2}:\d{2}/.test(t) };
  return null;
}

export function formatDateValue(time, hasTime) {
  const d = new Date(time);
  const base = `${d.getFullYear()}-${pad2(d.getMonth() + 1)}-${pad2(d.getDate())}`;
  return hasTime ? `${base} ${pad2(d.getHours())}:${pad2(d.getMinutes())}` : base;
}

export function normalizeDateValue(raw) {
  const parsed = parseDateValue(raw);
  return parsed ? formatDateValue(parsed.time, parsed.hasTime) : null;
}

function normalizeArgDates(args) {
  if (!args || typeof args !== "object") return args;
  const out = { ...args };
  for (const key of Object.keys(out)) {
    if (DATE_KEYS.has(key) && typeof out[key] === "string") {
      const canon = normalizeDateValue(out[key]);
      if (canon) out[key] = canon;
    }
  }
  return out;
}

export function normalizeTodoStatus(raw = "") {
  const value = String(raw || "").trim().toLowerCase();
  if (!value || value === " " || value === "open" || value === "unchecked") return "todo";
  if (value === "~" || value === "-" || value === "wip" || value === "active") return "doing";
  if (value === "x" || value === "checked" || value === "complete") return "done";
  if (value === "!" || value === "block") return "blocked";
  return todoStatuses.has(value) ? value : "todo";
}

function cleanCommandArgValue(value = "") {
  return String(value).trim().replace(/^["']|["']$/g, "");
}

export function parseCommandArgs(raw = "") {
  const body = String(raw || "").trim().replace(/^\{/, "").replace(/\}$/, "").trim();
  const args = {};
  if (!body) return args;
  for (const part of body.split(/[;,]/)) {
    const split = part.trim().match(/^([A-Za-z][\w-]*)\s*[:=]\s*(.+)$/);
    if (!split) continue;
    const key = split[1].trim().toLowerCase();
    const value = cleanCommandArgValue(split[2]);
    if (!key || !value) continue;
    args[key] = value;
  }
  return args;
}

function findInlineCommandClose(text, open, closeChar) {
  for (let i = open + 1; i < text.length; i++) {
    const ch = text[i];
    if (ch === "\\" && i + 1 < text.length) {
      i++;
      continue;
    }
    if (ch === "\n" || ch === "\r") return -1;
    if (ch === closeChar) return i;
  }
  return -1;
}

function inlineCommandMetaRange(text, closeBracket) {
  let openBrace = closeBracket + 1;
  while (openBrace < text.length && (text[openBrace] === " " || text[openBrace] === "\t")) openBrace++;
  if (text[openBrace] !== "{") return { raw: "", fullTo: closeBracket + 1 };
  const closeBrace = findInlineCommandClose(text, openBrace, "}");
  if (closeBrace < 0) return { raw: "", fullTo: closeBracket + 1 };
  return {
    raw: text.slice(openBrace, closeBrace + 1),
    fullTo: closeBrace + 1,
  };
}

export function scanInlineCommands(text, name = "") {
  const commands = [];
  const wanted = String(name || "").toLowerCase();
  const pushCommand = (commandName, switchValue, openBracket, closeBracket, fullFrom, fullTo, argsRaw = "") => {
    if (wanted && commandName !== wanted) return;
    commands.push({
      name: commandName,
      switchValue,
      context: text.slice(openBracket + 1, closeBracket),
      argsRaw,
      args: parseCommandArgs(argsRaw),
      fullFrom,
      fullTo,
      contextFrom: openBracket + 1,
      contextTo: closeBracket,
    });
  };

  const tagRe = /@@tag\[/gi;
  let tagMatch;
  while ((tagMatch = tagRe.exec(text))) {
    const openBracket = tagRe.lastIndex - 1;
    const closeBracket = findInlineCommandClose(text, openBracket, "]");
    if (closeBracket < 0) continue;
    pushCommand("tag", "", openBracket, closeBracket, tagMatch.index, closeBracket + 1);
    tagRe.lastIndex = closeBracket + 1;
  }

  const re = /@@([A-Za-z][\w-]*)(?:\(([^)\n]*)\))?[ \t]+\[/g;
  let match;
  while ((match = re.exec(text))) {
    const commandName = match[1].toLowerCase();
    const openBracket = re.lastIndex - 1;
    const closeBracket = findInlineCommandClose(text, openBracket, "]");
    if (closeBracket < 0) continue;
    const meta = inlineCommandMetaRange(text, closeBracket);
    pushCommand(commandName, String(match[2] || "").trim(), openBracket, closeBracket, match.index, meta.fullTo, meta.raw);
    re.lastIndex = meta.fullTo;
  }
  return commands.sort((a, b) => a.fullFrom - b.fullFrom || a.fullTo - b.fullTo);
}

export function extractTodos(content, note, updatedAt) {
  const todos = [];
  const lineStarts = [0];
  for (let i = 0; i < content.length; i++) {
    if (content[i] === "\n") lineStarts.push(i + 1);
  }
  const lineFor = (index) => {
    let lo = 0;
    let hi = lineStarts.length - 1;
    while (lo <= hi) {
      const mid = Math.floor((lo + hi) / 2);
      if (lineStarts[mid] <= index) lo = mid + 1;
      else hi = mid - 1;
    }
    return Math.max(0, hi) + 1;
  };
  for (const command of scanInlineCommands(content, "todo")) {
    const source = content.slice(command.fullFrom, command.fullTo);
    const text = String(command.context || "").replace(/\\([\]\\])/g, "$1").trim();
    const status = normalizeTodoStatus(command.switchValue);
    const args = normalizeArgDates(command.args);
    const line = lineFor(command.fullFrom);
    const lineStart = lineStarts[line - 1] || 0;
    const lineEnd = content.indexOf("\n", lineStart);
    const rawLine = content.slice(lineStart, lineEnd < 0 ? content.length : lineEnd).trim();
    todos.push({
      id: `${note.file}:${command.fullFrom}`,
      status,
      text,
      args,
      meta: command.argsRaw,
      ddl: args.ddl || "",
      source,
      index: command.fullFrom,
      line,
      column: command.fullFrom - lineStart + 1,
      context: rawLine,
      file: note.file,
      path: note.path,
      noteKey: note.key,
      noteId: note.id,
      noteTitle: note.title,
      noteDate: note.date || "",
      groupKey: note.groupKey || "",
      groupLabel: note.groupLabel || "",
      updatedAt,
    });
  }
  return todos;
}

function contentMayHaveTodos(content) {
  return /@@todo(?:\s*\(|[ \t]+\[)/i.test(String(content || ""));
}

async function todosForNote(note) {
  const cached = note.file ? noteCache.get(note.file) : null;
  if (cached) {
    if (Array.isArray(cached.todos)) return cached.todos.map((todo) => ({ ...todo }));
    if (typeof cached.todoContent === "string" && cached.todoContent) {
      const todos = extractTodos(cached.todoContent, note, cached.mtimeMs);
      cached.todos = todos;
      cached.todoContent = "";
      return todos.map((todo) => ({ ...todo }));
    }
    cached.todos = [];
    return [];
  }

  try {
    const info = await stat(note.file);
    const content = await readFile(note.file, "utf8");
    return extractTodos(content, note, info.mtimeMs).map((todo) => ({ ...todo }));
  } catch {
    return [];
  }
}

async function scanTodos() {
  const scanned = await scanNotes();
  const todoGroups = await mapLimit(scanned, scanConcurrency, async (note) => {
    return todosForNote(note);
  });
  const todos = todoGroups.flat();
  return todos.sort((a, b) => {
    const statusRank = { blocked: 0, doing: 1, todo: 2, done: 3 };
    return (statusRank[a.status] ?? 9) - (statusRank[b.status] ?? 9)
      || b.updatedAt - a.updatedAt
      || String(a.noteTitle).localeCompare(String(b.noteTitle));
  });
}

function existingUniqueDirs(dirs) {
  const out = [];
  const seen = new Set();
  for (const dir of dirs) {
    const resolved = resolve(dir);
    if (seen.has(resolved) || !existsSync(resolved)) continue;
    seen.add(resolved);
    out.push(resolved);
  }
  return out;
}

function snippetDirs() {
  const raw = process.env.AARONNOTE_SNIPPETS;
  if (raw) return existingUniqueDirs(raw.split(delimiter).filter(Boolean));
  return existingUniqueDirs([
    process.env.AARONNOTE_EMACS_SNIPPETS_ROOT || join(homedir(), ".config", "emacs", "snippets"),
    snippetsRoot,
  ]);
}

async function snippetRoots() {
  const roots = snippetDirs().map((dir) => ({ dir, kind: "" }));
  const kindRoots = [
    resolve(workspaceRoot, "kinds"),
    resolve(appDir, "..", "kinds"),
    resolve(process.cwd(), "kinds"),
  ].filter((dir, index, dirs) => dirs.indexOf(dir) === index && existsSync(dir));
  for (const kindsRoot of kindRoots) {
    try {
      const entries = await readdir(kindsRoot, { withFileTypes: true });
      for (const entry of entries) {
        if (!entry.isDirectory()) continue;
        const kind = normalizeNoteKind(entry.name);
        if (kind === defaultNoteKind || kind !== entry.name.toLowerCase()) continue;
        const dir = resolve(kindsRoot, entry.name, "snippet");
        if (existsSync(dir) && !roots.some((root) => root.dir === dir)) roots.push({ dir, kind });
      }
    } catch {}
  }
  return roots;
}

function parseSnippetBody(content) {
  const lines = content.split(/\r?\n/);
  const headers = new Map();
  let bodyStart = 0;
  for (let i = 0; i < lines.length; i++) {
    const marker = lines[i].match(/^# --\s*$/);
    if (marker) {
      bodyStart = i + 1;
      while (bodyStart < lines.length && /^# --\s*$/.test(lines[bodyStart])) bodyStart++;
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

export async function scanSnippets(options = {}) {
  const roots = await snippetRoots();
  const key = roots.map((root) => `${root.kind}@${root.dir}`).join(":");
  const now = Date.now();
  if (!options.force && snippetCache.key === key && now - snippetCache.scannedAt < 10_000) {
    return snippetCache.snippets;
  }
  const snippets = [];
  const seenSnippets = new Set();
  for (const root of roots) {
    const files = (await walkFiles(root.dir, (_file, name) => !name.startsWith(".") && !name.endsWith(".el")))
      .sort((a, b) => relative(root.dir, a).localeCompare(relative(root.dir, b)));
    const parsed = await mapLimit(files, scanConcurrency, async (file) => {
      try {
        const content = await readFile(file, "utf8");
        const { headers, body } = parseSnippetBody(content);
        if (!body.trim()) return null;
        const rel = relative(root.dir, file);
        const parts = rel.split(sep);
        const mode = parts[0] || "";
        const key = headers.get("key") || parts.at(-1) || "snippet";
        return {
          key,
          name: headers.get("name") || key,
          mode,
          group: headers.get("group") || "",
          kind: root.kind,
          body,
          source: file,
        };
      } catch {
        return null;
      }
    });
    for (const snippet of parsed) {
      if (!snippet) continue;
      const id = `${snippet.kind}\0${snippet.mode}\0${snippet.key}`;
      if (seenSnippets.has(id)) continue;
      seenSnippets.add(id);
      snippets.push(snippet);
    }
  }
  snippetCache = {
    key,
    scannedAt: now,
    snippets: snippets.sort((a, b) => `${a.kind}/${a.mode}/${a.key}`.localeCompare(`${b.kind}/${b.mode}/${b.key}`)),
  };
  return snippetCache.snippets;
}

function templateDirs() {
  const raw = process.env.AARONNOTE_TEMPLATES;
  if (raw) return existingUniqueDirs(raw.split(delimiter).filter(Boolean));
  return existingUniqueDirs([templatesRoot]);
}

function templateIdentity(rootDir, file, headers) {
  const rel = relative(rootDir, file);
  const parts = rel.split(sep).filter(Boolean);
  const fileKey = headers.get("key") || parts.at(-1) || "template";
  let kind = headers.get("kind") ? normalizeNoteKind(headers.get("kind")) : "";
  let mode = headers.get("mode") || "markdown-mode";
  if (parts[0] === "markdown-mode") {
    mode = parts[0];
  } else if (parts[0]) {
    const folderKind = normalizeNoteKind(parts[0]);
    if (folderKind !== defaultNoteKind && folderKind === parts[0].toLowerCase()) kind = folderKind;
    if (parts[1] === "markdown-mode") mode = parts[1];
  }
  const key = kind ? `${kind}/${fileKey}` : fileKey;
  return { key, name: headers.get("name") || fileKey, mode, kind };
}

export async function scanTemplates(options = {}) {
  const roots = templateDirs();
  const key = roots.join(":");
  const now = Date.now();
  if (!options.force && templateCache.key === key && now - templateCache.scannedAt < 10_000) {
    return templateCache.templates;
  }
  const templates = [];
  for (const rootDir of roots) {
    const files = await walkFiles(rootDir, (_file, name) => !name.startsWith(".") && !name.endsWith(".el"));
    for (const file of files) {
      try {
        const content = await readFile(file, "utf8");
        const { headers, body } = parseSnippetBody(content);
        if (!body.trim()) continue;
        const identity = templateIdentity(rootDir, file, headers);
        templates.push({
          ...identity,
          group: headers.get("group") || "templates",
          body,
          source: file,
        });
      } catch {}
    }
  }
  templateCache = {
    key,
    scannedAt: now,
    templates: templates.sort((a, b) => `${a.kind}/${a.key}`.localeCompare(`${b.kind}/${b.key}`)),
  };
  return templateCache.templates;
}

function templateVarsForNode({ title, id, tags, kind, path }) {
  const now = new Date();
  const date = now.toISOString().slice(0, 10);
  const time = now.toTimeString().slice(0, 5);
  const folder = groupKeyFor(resolveInputPath(path, noteScanRoot), noteScanRoot);
  return {
    title,
    slug: slugifyTitle(title),
    date,
    time,
    id,
    path: slashPath(path),
    folder: folder === "Root" ? "" : folder,
    kind,
    tags: normalizeTags(tags).join(", "),
  };
}

function replaceTemplateVariables(body, vars) {
  return String(body || "").replace(/\{\{\s*([A-Za-z][\w-]*)\s*\}\}/g, (_m, key) => {
    return Object.prototype.hasOwnProperty.call(vars, key) ? String(vars[key] ?? "") : "";
  });
}

function expandTemplateBody(body, vars) {
  const source = replaceTemplateVariables(body, vars);
  const values = new Map();
  let text = "";
  let cursor = null;
  let i = 0;

  function valueFor(index, fallback) {
    if (!values.has(index)) values.set(index, fallback);
    return values.get(index) ?? "";
  }

  function pushTabstop(index, value) {
    const from = text.length;
    text += value;
    const to = text.length;
    if (cursor == null || index === 0) cursor = { from: index === 0 ? to : from, to };
  }

  while (i < source.length) {
    const rest = source.slice(i);
    const choice = rest.match(/^\$\{(\d+)\|([^}]*)\|\}/);
    if (choice) {
      const index = Number(choice[1]);
      const options = choice[2].split(",").map((x) => x.trim()).filter(Boolean);
      pushTabstop(index, valueFor(index, options[0] ?? ""));
      i += choice[0].length;
      continue;
    }
    const placeholder = rest.match(/^\$\{(\d+):([^}]*)\}/);
    if (placeholder) {
      const index = Number(placeholder[1]);
      pushTabstop(index, valueFor(index, placeholder[2]));
      i += placeholder[0].length;
      continue;
    }
    const braced = rest.match(/^\$\{(\d+)\}/);
    if (braced) {
      const index = Number(braced[1]);
      pushTabstop(index, valueFor(index, ""));
      i += braced[0].length;
      continue;
    }
    const plain = rest.match(/^\$(\d+)/);
    if (plain) {
      const index = Number(plain[1]);
      pushTabstop(index, index === 0 ? "" : valueFor(index, ""));
      i += plain[0].length;
      continue;
    }
    text += source[i];
    i++;
  }
  return { text, selection: cursor };
}

async function templateByKey(key) {
  const wanted = String(key || "").trim();
  if (!wanted) return null;
  const templates = await scanTemplates();
  return templates.find((template) => template.key === wanted)
    ?? templates.find((template) => template.key.split("/").at(-1) === wanted)
    ?? null;
}

export function offsetToPosition(text, offset) {
  const source = String(text || "");
  const target = Math.max(0, Math.min(Number(offset) || 0, source.length));
  let line = 0;
  let lineStart = 0;
  for (let i = 0; i < target; i++) {
    if (source.charCodeAt(i) !== 10) continue;
    line++;
    lineStart = i + 1;
  }
  return { line, character: target - lineStart };
}

export function positionToOffset(text, position) {
  const source = String(text || "");
  const targetLine = Math.max(0, Number(position?.line) || 0);
  const targetChar = Math.max(0, Number(position?.character) || 0);
  let line = 0;
  let lineStart = 0;
  for (let i = 0; i < source.length && line < targetLine; i++) {
    if (source.charCodeAt(i) !== 10) continue;
    line++;
    lineStart = i + 1;
  }
  let lineEnd = source.indexOf("\n", lineStart);
  if (lineEnd < 0) lineEnd = source.length;
  return Math.max(lineStart, Math.min(lineStart + targetChar, lineEnd));
}

function languageIdForFile(file) {
  const ext = extname(String(file || "")).toLowerCase();
  if (ext === ".md" || ext === ".markdown") return "markdown";
  if (ext === ".typ") return "typst";
  if (ext === ".ts") return "typescript";
  if (ext === ".js" || ext === ".mjs" || ext === ".cjs") return "javascript";
  if (ext === ".json") return "json";
  if (ext === ".tex") return "latex";
  if (ext === ".lean") return "lean";
  return "plaintext";
}

function copilotUriForFile(file) {
  if (typeof file === "string" && file.trim()) {
    try {
      return pathToFileURL(safeOpenFile(file)).href;
    } catch {}
  }
  return pathToFileURL(join(runtimeTmpRoot || aaronnoteTmpRoot(), "copilot", "aaronnote-copilot.md")).href;
}

function uniqueExistingCommands(commands) {
  const seen = new Set();
  const out = [];
  for (const cmd of commands) {
    const key = `${cmd.command}\0${cmd.args.join("\0")}`;
    if (seen.has(key)) continue;
    if (cmd.mustExist && !existsSync(cmd.mustExist)) continue;
    seen.add(key);
    out.push(cmd);
  }
  return out;
}

function unpackedAsarPath(file) {
  return String(file || "").replace(/\.asar(?=$|[\\/])/, ".asar.unpacked");
}

function nodeCommand() {
  if (process.env.AARONNOTE_NODE) return process.env.AARONNOTE_NODE;
  if (process.versions?.electron) return "node";
  return process.execPath;
}

function appendCopilotLog(event, detail = {}) {
  copilotLog.push({
    at: new Date().toISOString(),
    event,
    ...detail,
  });
  if (copilotLog.length > 200) copilotLog = copilotLog.slice(-200);
}

function pushCopilotLog(event, detail = {}) {
  if (!copilotLogRecording) return;
  appendCopilotLog(event, detail);
}

function setCopilotLogRecording(enabled, options = {}) {
  if (options.clear) copilotLog = [];
  copilotLogRecording = enabled;
  appendCopilotLog(enabled ? "recording-started" : "recording-stopped", {});
}

function rawCopilotServerCommands() {
  const configured = process.env.AARONNOTE_COPILOT_LANGUAGE_SERVER;
  if (configured) return [{ command: configured, args: ["--stdio"] }];
  const binFile = join(appDir, "node_modules", ".bin", "copilot-language-server");
  const serverFile = join(appDir, "node_modules", "@github", "copilot-language-server", "dist", "language-server.js");
  const unpackedBin = unpackedAsarPath(binFile);
  const unpackedServer = unpackedAsarPath(serverFile);
  const resourceServer = process.resourcesPath
    ? join(process.resourcesPath, "app.asar.unpacked", "node_modules", "@github", "copilot-language-server", "dist", "language-server.js")
    : "";
  const commands = [];
  if (!appDir.includes(".asar")) {
    commands.push(
      { command: binFile, args: ["--stdio"], mustExist: binFile },
      { command: nodeCommand(), args: [serverFile, "--stdio"], mustExist: serverFile },
    );
  }
  for (const file of [unpackedBin, unpackedServer, resourceServer]) {
    if (!file) continue;
    if (process.versions?.electron) {
      commands.push({
        command: process.execPath,
        args: [file, "--stdio"],
        env: { ELECTRON_RUN_AS_NODE: "1" },
        mustExist: file,
      });
    } else {
      commands.push({ command: file, args: ["--stdio"], mustExist: file });
      commands.push({ command: nodeCommand(), args: [file, "--stdio"], mustExist: file });
    }
  }
  return commands;
}

function copilotServerCommands() {
  return uniqueExistingCommands(rawCopilotServerCommands());
}

function copilotDiagnostics() {
  return {
    type: "copilot-log",
    now: new Date().toISOString(),
    platform: process.platform,
    arch: process.arch,
    pid: process.pid,
    execPath: process.execPath,
    nodeCommand: nodeCommand(),
    electron: process.versions?.electron || "",
    appDir,
    childProcessCwd: copilotProcessCwd(),
    workspaceRoot,
    noteRoot,
    resourcesPath: process.resourcesPath || "",
    logRecording: copilotLogRecording,
    env: {
      AARONNOTE_COPILOT_LANGUAGE_SERVER: process.env.AARONNOTE_COPILOT_LANGUAGE_SERVER || "",
      AARONNOTE_NODE: process.env.AARONNOTE_NODE || "",
      ELECTRON_RUN_AS_NODE: process.env.ELECTRON_RUN_AS_NODE || "",
      PATH: process.env.PATH || "",
    },
    rawCommands: rawCopilotServerCommands().map((cmd) => ({
      command: cmd.command,
      args: cmd.args,
      env: cmd.env || {},
      mustExist: cmd.mustExist || "",
      exists: cmd.mustExist ? existsSync(cmd.mustExist) : existsSync(cmd.command),
    })),
    runnableCommands: copilotServerCommands().map((cmd) => ({
      command: cmd.command,
      args: cmd.args,
      env: cmd.env || {},
      mustExist: cmd.mustExist || "",
    })),
    client: copilotClient
      ? {
          hasProcess: !!copilotClient.proc,
          pid: copilotClient.proc?.pid || 0,
          status: copilotClient.status,
          pending: copilotClient.pending?.size || 0,
          documents: copilotClient.documents?.size || 0,
        }
      : null,
    log: copilotLog,
  };
}

function openExternalUri(uri) {
  if (!/^https?:\/\//i.test(String(uri || ""))) return;
  pushCopilotLog("open-uri", { uri });
  if (process.platform === "darwin") {
    execFile("open", [uri], () => {});
  }
  return uri;
}

function findFirstExternalUri(value, depth = 0) {
  if (depth > 5 || value == null) return "";
  if (typeof value === "string") return /^https?:\/\//i.test(value) ? value : "";
  if (Array.isArray(value)) {
    for (const item of value) {
      const uri = findFirstExternalUri(item, depth + 1);
      if (uri) return uri;
    }
    return "";
  }
  if (typeof value === "object") {
    for (const item of Object.values(value)) {
      const uri = findFirstExternalUri(item, depth + 1);
      if (uri) return uri;
    }
  }
  return "";
}

function findStringByKey(value, pattern, depth = 0) {
  if (depth > 5 || value == null || typeof value !== "object") return "";
  if (Array.isArray(value)) {
    for (const item of value) {
      const found = findStringByKey(item, pattern, depth + 1);
      if (found) return found;
    }
    return "";
  }
  for (const [key, item] of Object.entries(value)) {
    if (pattern.test(key) && typeof item === "string" && item) return item;
    const found = findStringByKey(item, pattern, depth + 1);
    if (found) return found;
  }
  return "";
}

function deviceCodeFromText(text) {
  const value = String(text || "");
  const match = value.match(/\b([A-Z0-9]{4}-[A-Z0-9]{4})\b/i) || value.match(/\b([A-Z0-9]{8})\b/i);
  return match ? match[1].toUpperCase().replace(/^([A-Z0-9]{4})([A-Z0-9]{4})$/, "$1-$2") : "";
}

function copilotProcessCwd() {
  if (appDir.includes(".asar")) return dirname(appDir);
  return appDir;
}

class CopilotLspClient {
  constructor() {
    this.proc = null;
    this.buffer = Buffer.alloc(0);
    this.nextId = 1;
    this.pending = new Map();
    this.documents = new Map();
    this.status = { message: "Not started", kind: "Inactive", busy: false };
    this.ready = null;
    this.lastAuthCode = "";
    this.lastAuthMessage = "";
  }

  async ensureReady() {
    if (this.ready) return this.ready;
    this.ready = this.start();
    return this.ready;
  }

  async start() {
    const commands = copilotServerCommands();
    pushCopilotLog("start", { commands: commands.map((cmd) => ({ command: cmd.command, args: cmd.args, env: cmd.env || {} })) });
    if (commands.length === 0) {
      pushCopilotLog("missing-server", { rawCommands: copilotDiagnostics().rawCommands });
      throw new Error("Copilot language server is unavailable. Set AARONNOTE_COPILOT_LANGUAGE_SERVER to Emacs's copilot-server-executable.");
    }
    let lastError = null;
    for (const cmd of commands) {
      try {
        await this.startCommand(cmd);
        pushCopilotLog("started", { command: cmd.command, args: cmd.args, pid: this.proc?.pid || 0 });
        return;
      } catch (err) {
        lastError = err;
        pushCopilotLog("start-failed", {
          command: cmd.command,
          args: cmd.args,
          message: err instanceof Error ? err.message : String(err),
          code: err?.code || "",
        });
        this.stop();
      }
    }
    throw lastError ?? new Error("Copilot language server failed to start");
  }

  failPending(err) {
    for (const pending of this.pending.values()) pending.reject(err);
    this.pending.clear();
  }

  async startCommand(cmd) {
    const proc = spawn(cmd.command, cmd.args, {
      cwd: copilotProcessCwd(),
      env: cmd.env ? { ...process.env, ...cmd.env } : process.env,
      stdio: ["pipe", "pipe", "pipe"],
    });
    pushCopilotLog("spawn", { command: cmd.command, args: cmd.args, cwd: copilotProcessCwd(), env: cmd.env || {}, pid: proc.pid || 0 });
    this.proc = proc;
    proc.stdout.on("data", (chunk) => this.receive(chunk));
    proc.stderr.on("data", (chunk) => {
      const msg = String(chunk || "").trim();
      if (msg) {
        pushCopilotLog("stderr", { message: msg });
        console.warn(`Copilot LSP: ${msg}`);
      }
    });
    proc.once("error", (err) => {
      if (this.proc !== proc) return;
      pushCopilotLog("error", { message: err.message, code: err.code || "" });
      this.failPending(err);
      this.proc = null;
      this.ready = null;
      this.status = { message: err.message, kind: "Error", busy: false };
    });
    proc.once("exit", (code, signal) => {
      if (this.proc !== proc) return;
      const err = new Error(`Copilot language server exited (${signal || (code ?? "unknown")})`);
      pushCopilotLog("exit", { code, signal });
      this.failPending(err);
      this.proc = null;
      this.ready = null;
      this.documents.clear();
      this.status = { message: err.message, kind: "Error", busy: false };
    });

    await this.request("initialize", {
      processId: process.pid,
      rootUri: pathToFileURL(workspaceRoot).href,
      workspaceFolders: [{ uri: pathToFileURL(workspaceRoot).href, name: basename(workspaceRoot) || "workspace" }],
      capabilities: {
        workspace: { workspaceFolders: true, configuration: true },
        window: { showDocument: { support: true } },
        textDocument: {},
      },
      initializationOptions: {
        editorInfo: { name: "Aaronnote", version: "0.3.1" },
        editorPluginInfo: { name: "Aaronnote Copilot", version: "0.1.0" },
      },
    });
    this.notify("initialized", {});
    this.notify("workspace/didChangeConfiguration", {
      settings: {
        telemetry: { telemetryLevel: "all" },
      },
    });
    this.status = { message: "Ready", kind: "Normal", busy: false };
  }

  send(value) {
    if (!this.proc?.stdin?.writable) throw new Error("Copilot language server is not running");
    const body = Buffer.from(JSON.stringify(value), "utf8");
    const header = Buffer.from(`Content-Length: ${body.length}\r\n\r\n`, "utf8");
    this.proc.stdin.write(Buffer.concat([header, body]));
  }

  request(method, params) {
    const id = this.nextId++;
    this.send({ jsonrpc: "2.0", id, method, params });
    return new Promise((resolveRequest, reject) => {
      this.pending.set(id, { resolve: resolveRequest, reject });
      windowSetTimeout(() => {
        const pending = this.pending.get(id);
        if (!pending) return;
        this.pending.delete(id);
        pending.reject(new Error(`Copilot request timed out: ${method}`));
      }, 30_000);
    });
  }

  notify(method, params) {
    this.send({ jsonrpc: "2.0", method, params });
  }

  respond(id, result, error = null) {
    if (error) this.send({ jsonrpc: "2.0", id, error });
    else this.send({ jsonrpc: "2.0", id, result });
  }

  receive(chunk) {
    this.buffer = Buffer.concat([this.buffer, Buffer.from(chunk)]);
    while (true) {
      const headerEnd = this.buffer.indexOf("\r\n\r\n");
      if (headerEnd < 0) return;
      const header = this.buffer.slice(0, headerEnd).toString("utf8");
      const match = header.match(/content-length:\s*(\d+)/i);
      if (!match) {
        this.buffer = this.buffer.slice(headerEnd + 4);
        continue;
      }
      const length = Number(match[1]);
      const start = headerEnd + 4;
      const end = start + length;
      if (this.buffer.length < end) return;
      const raw = this.buffer.slice(start, end).toString("utf8");
      this.buffer = this.buffer.slice(end);
      try {
        this.handle(JSON.parse(raw));
      } catch (err) {
        console.warn("Copilot LSP parse failed", err);
      }
    }
  }

  handle(message) {
    if (Object.prototype.hasOwnProperty.call(message, "id") && (Object.prototype.hasOwnProperty.call(message, "result") || message.error)) {
      const pending = this.pending.get(message.id);
      if (!pending) return;
      this.pending.delete(message.id);
      if (message.error) pending.reject(new Error(message.error.message || "Copilot request failed"));
      else pending.resolve(message.result);
      return;
    }
    if (message.method === "didChangeStatus") {
      this.status = message.params || this.status;
      return;
    }
    if (message.method === "window/logMessage") {
      const msg = message.params?.message;
      if (msg) console.warn(`Copilot LSP: ${msg}`);
      return;
    }
    if (message.method === "window/showDocument") {
      openExternalUri(message.params?.uri);
      this.respond(message.id, { success: true });
      return;
    }
    if (message.method === "workspace/configuration") {
      const items = Array.isArray(message.params?.items) ? message.params.items : [];
      this.respond(message.id, items.map(() => ({})));
      return;
    }
    if (message.method === "window/showMessageRequest") {
      const text = String(message.params?.message || "");
      const code = deviceCodeFromText(text);
      if (code) {
        this.lastAuthCode = code;
        this.lastAuthMessage = text;
      }
      pushCopilotLog("show-message-request", {
        message: text,
        code,
        actions: Array.isArray(message.params?.actions) ? message.params.actions : [],
      });
      const actions = Array.isArray(message.params?.actions) ? message.params.actions : [];
      this.respond(message.id, actions[0] ?? null);
      return;
    }
    if (Object.prototype.hasOwnProperty.call(message, "id")) {
      this.respond(message.id, null);
    }
  }

  syncDocument(uri, file, content) {
    const languageId = languageIdForFile(file);
    const current = this.documents.get(uri);
    if (!current) {
      const version = 1;
      this.documents.set(uri, { version, content, languageId });
      this.notify("textDocument/didOpen", {
        textDocument: { uri, languageId, version, text: content },
      });
      return { version, languageId };
    }
    if (current.content !== content) {
      const version = current.version + 1;
      this.notify("textDocument/didChange", {
        textDocument: { uri, version },
        contentChanges: [{
          range: { start: { line: 0, character: 0 }, end: offsetToPosition(current.content, current.content.length) },
          rangeLength: current.content.length,
          text: content,
        }],
      });
      this.documents.set(uri, { version, content, languageId });
      return { version, languageId };
    }
    return { version: current.version, languageId: current.languageId };
  }

  async inline(body) {
    await this.ensureReady();
    const content = String(body.content || "");
    const file = String(body.file || "");
    const offset = Math.max(0, Math.min(Number(body.offset) || 0, content.length));
    const uri = copilotUriForFile(file);
    const { version } = this.syncDocument(uri, file, content);
    this.notify("textDocument/didFocus", { textDocument: { uri } });
    const result = await this.request("textDocument/inlineCompletion", {
      textDocument: { uri, version },
      position: offsetToPosition(content, offset),
      context: { triggerKind: 2 },
      formattingOptions: { tabSize: 2, insertSpaces: true },
    });
    const item = Array.isArray(result?.items) ? result.items.find((candidate) => typeof candidate?.insertText === "string") : null;
    if (!item) return { type: "copilot-inline", items: [], status: this.status };
    const range = item.range
      ? {
          from: positionToOffset(content, item.range.start),
          to: positionToOffset(content, item.range.end),
        }
      : { from: offset, to: offset };
    return {
      type: "copilot-inline",
      items: [{
        insertText: item.insertText,
        range,
        item,
      }],
      status: this.status,
    };
  }

  async shown(body) {
    await this.ensureReady();
    if (body?.item) this.notify("textDocument/didShowCompletion", { item: body.item });
    return { ok: true };
  }

  async accept(body) {
    await this.ensureReady();
    const item = body?.item;
    if (!item) return { ok: false };
    const acceptedLength = Number(body.acceptedLength);
    if (Number.isFinite(acceptedLength) && acceptedLength >= 0 && acceptedLength < String(item.insertText || "").length) {
      this.notify("textDocument/didPartiallyAcceptCompletion", { item, acceptedLength });
      return { ok: true, partial: true };
    }
    if (item.command?.command) {
      await this.request("workspace/executeCommand", {
        command: item.command.command,
        arguments: Array.isArray(item.command.arguments) ? item.command.arguments : [],
      });
    }
    return { ok: true };
  }

  async signIn() {
    await this.ensureReady();
    this.lastAuthCode = "";
    this.lastAuthMessage = "";
    const result = await this.request("signIn", {});
    pushCopilotLog("sign-in-result", { result });
    const resultUri = findStringByKey(result, /^(verificationUri|verification_uri|verificationUriComplete|verification_uri_complete|uri|url)$/i)
      || findFirstExternalUri(result);
    const userCode = findStringByKey(result, /^(userCode|user_code|code)$/i) || this.lastAuthCode || deviceCodeFromText(this.lastAuthMessage);
    const openedUri = result?.status === "AlreadySignedIn"
      ? openExternalUri("https://github.com/settings/copilot")
      : openExternalUri(resultUri);
    if (result?.command?.command) {
      void this.request("workspace/executeCommand", {
        command: result.command.command,
        arguments: Array.isArray(result.command.arguments) ? result.command.arguments : [],
      }).catch((err) => {
        console.warn("Copilot sign-in command failed", err);
      });
    }
    const message = result?.status === "AlreadySignedIn"
      ? `Already signed in${result?.user ? ` as ${result.user}` : ""}; opened GitHub Copilot settings`
      : openedUri
        ? userCode
          ? `Opened GitHub login; code ${userCode}`
          : "Opened GitHub login"
        : userCode
          ? `Copilot login code ${userCode}`
          : "Copilot login did not return a device code";
    return { type: "copilot-sign-in", ...result, openedUri, userCode, message, status: this.status };
  }

  async signOut() {
    await this.ensureReady();
    await this.request("signOut", {});
    return { ok: true, status: this.status };
  }

  async quota() {
    await this.ensureReady();
    const result = await this.request("checkQuota", {}).catch((err) => ({ error: err.message }));
    return { type: "copilot-quota", result };
  }

  stop() {
    const proc = this.proc;
    this.proc = null;
    this.ready = null;
    if (!proc) return;
    proc.kill(); // SIGTERM
    // Escalate to SIGKILL after 2 s if the language server ignores SIGTERM.
    const fallback = setTimeout(() => {
      try { proc.kill("SIGKILL"); } catch (_) {}
    }, 2000);
    fallback.unref?.();
    proc.once("exit", () => clearTimeout(fallback));
  }
}

function windowSetTimeout(fn, ms) {
  return setTimeout(fn, ms);
}

function getCopilotClient() {
  if (!copilotClient) copilotClient = new CopilotLspClient();
  return copilotClient;
}

export async function shutdownCopilot() {
  if (copilotClient) {
    copilotClient.stop();
    copilotClient = null;
  }
}

export async function handleCopilotRequest(action, body = {}) {
  if (action === "log") {
    if (body?.record === true) {
      setCopilotLogRecording(true, { clear: body?.clear !== false });
      return { ...copilotDiagnostics(), message: "Copilot log recording started" };
    }
    if (body?.record === false) {
      setCopilotLogRecording(false);
      return { ...copilotDiagnostics(), message: "Copilot logs recorded" };
    }
    return copilotDiagnostics();
  }
  const client = getCopilotClient();
  if (action === "inline") return client.inline(body);
  if (action === "shown") return client.shown(body);
  if (action === "accept") return client.accept(body);
  if (action === "sign-in") return client.signIn();
  if (action === "sign-out") return client.signOut();
  if (action === "quota") return client.quota();
  if (action === "status") {
    await client.ensureReady();
    return { type: "copilot-status", status: client.status };
  }
  return { ok: false, message: "Unknown Copilot action" };
}

export async function readNote(file, options = {}) {
  const safe = safeOpenFile(file);
  if (leanSourceFile(safe)) {
    const err = new Error("Lean files are edited manually");
    err.statusCode = 400;
    throw err;
  }
  noteScanRoot = scanRootForOpenFile(safe);
  const info = await stat(safe);
  if (!info.isFile()) {
    const err = new Error(`Not a regular file: ${safe}`);
    err.statusCode = 400;
    throw err;
  }
  const content = await readFile(safe, "utf8");
  const standalone = standaloneFile(safe);
  const payload = {
    type: "open",
    file: safe,
    title: titleFromContent(safe, content),
    mode: modeForFile(safe),
    content,
    kind: kindFromContent(content),
    mtimeMs: info.mtimeMs,
    size: info.size,
    standalone,
  };
  if (options.includeIndex === true) {
    Object.assign(payload, await notesIndexPayload());
    payload.snippets = await scanSnippets();
    payload.templates = await scanTemplates();
  }
  return payload;
}

async function noteSummaryForFile(file, content = null) {
  const safe = safeOpenFile(file);
  const info = await stat(safe);
  const text = content == null ? await readFile(safe, "utf8") : String(content);
  const relPath = displayPathForScanRoot(safe, noteScanRoot);
  const groupKey = groupKeyFor(safe, noteScanRoot);
  const id = idFromContent(safe, noteScanRoot, text);
  const bookMeta = bookMetaFromContent(text);
  const roam = hasRoamMeta(text);
  const note = {
    key: id,
    id,
    title: titleFromContent(safe, text),
    file: safe,
    link: relPath,
    path: relPath,
    ext: safe.slice(safe.lastIndexOf(".") + 1).toLowerCase(),
    kind: kindFromContent(text),
    date: dateFromContent(text),
    groupKey,
    groupLabel: groupLabelFor(groupKey),
    section: groupKey.includes(sep) ? groupKey.split(sep)[0] : groupKey,
    source: sourceFromContent(text),
    aliases: aliasesFromContent(text),
    summary: summaryFromContent(text),
    tags: tagsFromContent(text),
    inlineTags: inlineTagsFromContent(text),
    refs: refsFromContent(text),
    backlinks: [],
    roam,
    bookRole: bookMeta.role,
    bookParentRef: bookMeta.parentRef,
    bookCoverId: bookMeta.role === "cover" ? id : "",
    bookCoverPath: "",
    bookParentPath: "",
    bookIncludeRefs: includeRefsFromContent(text),
    bookIncludedPaths: [],
    bookToc: [],
    bookDomTargets: [],
    domTargets: [],
    bookRawRefs: [],
    bookDiagnostics: [],
    standalone: standaloneFile(safe),
    mtimeMs: info.mtimeMs,
    size: info.size,
  };
  note.domTargets = domTargetsFromContent(text, note);
  return note;
}

function roamDbFile() {
  return join(noteRoot, "roam.db");
}

function roamSyncStateFile() {
  return join(stateRoot, "sync", "state.json");
}

async function readSyncState() {
  try {
    const raw = await readFile(roamSyncStateFile(), "utf8");
    return JSON.parse(raw);
  } catch {
    return {};
  }
}

async function writeSyncState(patch) {
  let current = {};
  try {
    const raw = await readFile(roamSyncStateFile(), "utf8");
    current = JSON.parse(raw);
  } catch {}
  const next = { ...current, ...patch };
  await atomicWriteFile(roamSyncStateFile(), JSON.stringify(next, null, 2), "utf8");
}

function sqlString(value) {
  return `'${String(value ?? "").replace(/'/g, "''")}'`;
}

function sqlNumber(value) {
  return Number.isFinite(value) ? String(value) : "0";
}

function notePosition(content) {
  const range = metaBlockRange(content);
  if (!range) return 1;
  return range.to + 1;
}

function roamDbSchemaStatements() {
  return [
    `CREATE TABLE IF NOT EXISTS files (
      path text primary key,
      mtime real not null,
      title text,
      node_id text,
      size integer not null default 0
    );`,
    `CREATE TABLE IF NOT EXISTS nodes (
      id text primary key,
      file text not null,
      title text not null,
      date text,
      position integer not null,
      summary text not null default ''
    );`,
    "CREATE TABLE IF NOT EXISTS tags (node_id text not null, tag text not null);",
    "CREATE TABLE IF NOT EXISTS aliases (node_id text not null, alias text not null);",
    "CREATE TABLE IF NOT EXISTS links (source_id text not null, target_id text not null, file text not null, line integer not null, label text);",
    "CREATE INDEX IF NOT EXISTS note_nodes_file_idx on nodes(file);",
    "CREATE INDEX IF NOT EXISTS note_tags_node_idx on tags(node_id);",
    "CREATE INDEX IF NOT EXISTS note_aliases_node_idx on aliases(node_id);",
    "CREATE INDEX IF NOT EXISTS note_links_target_idx on links(target_id);",
    "CREATE INDEX IF NOT EXISTS note_links_source_idx on links(source_id);",
  ];
}

async function appendRoamNodeStatements(statements, note, roamIds, refIndex, options = {}) {
  let info = null;
  let content = "";
  try {
    info = await stat(note.file);
    content = await readFile(note.file, "utf8");
  } catch {
    return;
  }
  if (options.includeNode !== false) {
    statements.push(
      `INSERT OR REPLACE INTO files(path, mtime, title, node_id, size) VALUES (${[
        sqlString(note.file),
        sqlNumber(info.mtimeMs / 1000),
        sqlString(note.title || ""),
        sqlString(note.id || ""),
        sqlNumber(info.size),
      ].join(", ")});`,
      `INSERT OR REPLACE INTO nodes(id, file, title, date, position, summary) VALUES (${[
        sqlString(note.id || ""),
        sqlString(note.file),
        sqlString(note.title || "Untitled"),
        sqlString(note.date || ""),
        sqlNumber(notePosition(content)),
        sqlString(note.summary || ""),
      ].join(", ")});`,
    );
    for (const tag of note.tags || []) {
      statements.push(`INSERT INTO tags(node_id, tag) VALUES (${sqlString(note.id)}, ${sqlString(tag)});`);
    }
    for (const alias of note.aliases || []) {
      statements.push(`INSERT INTO aliases(node_id, alias) VALUES (${sqlString(note.id)}, ${sqlString(alias)});`);
    }
  }
  for (const ref of [...new Set([...roamDbRefsFromContent(content), ...(note.bookRawRefs || [])])]) {
    const target = refIndex.get(canonicalServerNoteRef(ref));
    const targetId = target?.id || "";
    if (!roamIds.has(targetId) || targetId === note.id) continue;
    statements.push(`INSERT INTO links(source_id, target_id, file, line, label) VALUES (${[
      sqlString(note.id),
      sqlString(targetId),
      sqlString(note.file),
      "1",
      sqlString(""),
    ].join(", ")});`);
  }
}

async function incrementalRoamDbStatements(scanned, changedFiles) {
  const files = [...new Set((changedFiles || []).map((file) => resolveUserPath(file)).filter((file) => inside(file, noteRoot)))];
  if (files.length === 0) return null;
  const fileKeySet = new Set(files.map(canonicalExistingPath));
  const noteFileChanged = (note) => Boolean(note.file && fileKeySet.has(canonicalExistingPath(note.file)));
  const changedNotes = scanned.filter(noteFileChanged);
  const changedIds = [...new Set(changedNotes.map((note) => note.id).filter(Boolean))];
  if (changedIds.length === 0) return null;
  const roamNotes = scanned.filter((note) => note.roam && note.file);
  const roamIds = new Set(roamNotes.map((note) => note.id));
  const refIndex = serverNoteReferenceIndex(roamNotes);
  const affectedSources = roamNotes.filter((note) =>
    noteFileChanged(note) || (note.refs || []).some((ref) => changedIds.includes(ref)));
  const linkRefreshFiles = [...new Set([...files, ...affectedSources.map((note) => note.file).filter(Boolean)])];
  const changedDbFiles = [...new Set([...files, ...changedNotes.map((note) => note.file).filter(Boolean)])];
  const statements = [
    "PRAGMA foreign_keys = OFF;",
    "BEGIN;",
    ...roamDbSchemaStatements(),
    `DELETE FROM links WHERE file IN (${linkRefreshFiles.map(sqlString).join(", ")}) OR source_id IN (${changedIds.map(sqlString).join(", ")}) OR target_id IN (${changedIds.map(sqlString).join(", ")});`,
    `DELETE FROM tags WHERE node_id IN (${changedIds.map(sqlString).join(", ")});`,
    `DELETE FROM aliases WHERE node_id IN (${changedIds.map(sqlString).join(", ")});`,
    `DELETE FROM nodes WHERE id IN (${changedIds.map(sqlString).join(", ")}) OR file IN (${changedDbFiles.map(sqlString).join(", ")});`,
    `DELETE FROM files WHERE path IN (${changedDbFiles.map(sqlString).join(", ")});`,
  ];
  for (const note of changedNotes.filter((note) => note.roam && note.file)) {
    await appendRoamNodeStatements(statements, note, roamIds, refIndex, { includeNode: true });
  }
  for (const note of affectedSources.filter((note) => !noteFileChanged(note))) {
    await appendRoamNodeStatements(statements, note, roamIds, refIndex, { includeNode: false });
  }
  statements.push("COMMIT;");
  return statements;
}

async function runFullRoamSync(scanned, dbFile) {
  const roamNotes = scanned.filter((note) => note.roam && note.file);
  const roamIds = new Set(roamNotes.map((note) => note.id));
  const refIndex = serverNoteReferenceIndex(roamNotes);
  const tmpDb = await runtimeTmpFile("db", dbFile, `.tmp-${process.pid}-${Date.now()}-${++atomicWriteCounter}`);
  const statements = [
    "PRAGMA foreign_keys = OFF;",
    "BEGIN;",
    ...roamDbSchemaStatements(),
    "DELETE FROM links;",
    "DELETE FROM tags;",
    "DELETE FROM aliases;",
    "DELETE FROM nodes;",
    "DELETE FROM files;",
  ];
  for (const note of roamNotes) {
    await appendRoamNodeStatements(statements, note, roamIds, refIndex, { includeNode: true });
  }
  statements.push("COMMIT;");
  await mkdir(dirname(dbFile), { recursive: true });
  try {
    await execFileAsync("sqlite3", [tmpDb, statements.join("\n")], {
      cwd: noteRoot,
      maxBuffer: 1024 * 1024 * 8,
    });
    try {
      await rename(tmpDb, dbFile);
    } catch (err) {
      if (err?.code !== "EXDEV") throw err;
      await copyFile(tmpDb, dbFile);
      await rm(tmpDb, { force: true }).catch(() => {});
    }
  } finally {
    await rm(tmpDb, { force: true }).catch(() => {});
  }
}

async function runIncrementalRoamSync(scanned, dbFile, changedFiles) {
  const statements = await incrementalRoamDbStatements(scanned, changedFiles);
  if (!statements) return false;
  await execFileAsync("sqlite3", [dbFile, statements.join("\n")], {
    cwd: noteRoot,
    maxBuffer: 1024 * 1024 * 8,
  });
  return true;
}

// options.mode: "auto" (default) | "full"
// options.changedFiles: string[] — caller-supplied explicit changed file list (skip git detection)
export async function syncRoamDb(notes = null, options = {}) {
  if (roamSyncTimer) {
    clearTimeout(roamSyncTimer);
    roamSyncTimer = null;
  }
  const queuedNotes = notes ? null : queuedRoamSyncNotes;
  const queuedFiles = queuedRoamSyncChangedFiles.splice(0);
  queuedRoamSyncNotes = null;
  const optionFiles = Array.isArray(options.changedFiles) ? options.changedFiles : [];
  const pendingFiles = [...new Set([...optionFiles, ...queuedFiles])];
  const scanned = notes ?? queuedNotes ?? await scanNotes();
  const previous = roamSyncInFlight ?? Promise.resolve();
  const current = previous.catch(() => {}).then(async () => {
    const dbFile = roamDbFile();
    const forceMode = options.mode === "full";
    const explicitFiles = pendingFiles.length > 0 ? pendingFiles : null;

    const state = await readSyncState();
    const schemaOk = state.dbSchemaVersion === CURRENT_DB_SCHEMA;
    const dbExists = existsSync(dbFile);
    const now = new Date().toISOString();

    // Determine whether we must do a full rebuild.
    // Use a deterministic time-based policy instead of random sampling so the
    // save path has predictable latency.  Weekly rebuild provides self-healing.
    const stale = state.lastFullAt
      ? (Date.now() - new Date(state.lastFullAt).getTime()) > ROAM_FULL_SYNC_INTERVAL_MS
      : false;
    const needFull = forceMode || !dbExists || !schemaOk || !state.lastSyncedCommit || stale;

    if (needFull) {
      const reason = forceMode ? "forced" : !dbExists ? "no-db" : !schemaOk ? "schema" : !state.lastSyncedCommit ? "no-state" : "stale";
      console.log(`[roam-sync] full rebuild (${reason})`);
      await runFullRoamSync(scanned, dbFile);
      const sha = await commitRoam(noteRoot, `roam sync: ${now}`);
      await writeSyncState({ lastSyncedCommit: sha, lastSyncedAt: now, lastFullAt: now, dbSchemaVersion: CURRENT_DB_SCHEMA });
      return;
    }

    // Resolve changed files: explicit > git detection
    let changedFiles = explicitFiles;
    if (!changedFiles) {
      changedFiles = await changedRoamFilesSince(noteRoot, state.lastSyncedCommit);
      if (changedFiles === null) {
        // commit no longer reachable (rebase/squash) — fallback to full
        console.log("[roam-sync] full rebuild (stale commit ref)");
        await runFullRoamSync(scanned, dbFile);
        const sha = await commitRoam(noteRoot, `roam sync: ${now}`);
        await writeSyncState({ lastSyncedCommit: sha, lastSyncedAt: now, lastFullAt: state.lastFullAt, dbSchemaVersion: CURRENT_DB_SCHEMA });
        return;
      }
    }

    if (changedFiles.length === 0) {
      console.log("[roam-sync] incremental: no changes detected");
      return;
    }

    console.log(`[roam-sync] incremental: ${changedFiles.length} file(s)`);
    const ok = await runIncrementalRoamSync(scanned, dbFile, changedFiles);
    if (!ok) {
      // incrementalRoamDbStatements returned null — changed IDs resolved to nothing roam-worthy
      return;
    }
    const sha = await commitRoam(noteRoot, `roam sync: ${now}`);
    await writeSyncState({ lastSyncedCommit: sha, lastSyncedAt: now, lastFullAt: state.lastFullAt, dbSchemaVersion: CURRENT_DB_SCHEMA });
  });
  roamSyncInFlight = current;
  try {
    await current;
  } finally {
    if (roamSyncInFlight === current) roamSyncInFlight = null;
  }
  return scanned;
}

// Exported for desktop/main.mjs weekly full-sync check
export async function maybeScheduleWeeklyFullSync() {
  const state = await readSyncState();
  if (!state.lastFullAt) return false; // no state yet — first full sync will happen on next manual sync
  const age = Date.now() - new Date(state.lastFullAt).getTime();
  if (age < 7 * 24 * 60 * 60 * 1000) return false;
  console.log("[roam-sync] weekly full rebuild triggered");
  void syncRoamDb(null, { mode: "full" }).catch((err) => {
    console.error("[roam-sync] weekly full rebuild failed:", err?.message || err);
  });
  return true;
}

// Exported for version control features
export { fileHistory, restoreFileFromCommit, discardFileChanges, roamRepoStatus, roamRepoChanges, diffRoamFile, diffRoamCommit, pullRoam, pushRoam, repoHistory, noteRoot as roamNoteRoot };

export async function createNode(body) {
  const title = String(body.title || "Untitled").trim() || "Untitled";
  const nodeType = String(body.nodeType || body.type || "roam").toLowerCase() === "regular" ? "regular" : "roam";
  const roam = nodeType === "roam";
  const id = String(body.id || `${timestampId()}-${slugifyTitle(title)}`).trim();
  const kind = normalizeNoteKind(body.kind || (roam ? "note" : defaultNoteKind));
  const tags = Array.isArray(body.tags) ? body.tags.map(String).filter(Boolean) : [];
  const rawPath = String(body.path || body.file || "").trim();
  const directory = String(body.directory || ".").trim() || ".";
  const defaultName = `${slugifyTitle(roam ? id : title)}.md`;
  let relativePath = rawPath
    ? rawPath
    : join(directory, defaultName);
  if (relativePath.endsWith("/") || relativePath.endsWith(sep)) {
    relativePath = join(relativePath, defaultName);
  } else if (!extname(relativePath)) {
    relativePath = `${relativePath}.md`;
  }
  const baseRoot = roam ? noteRoot : noteScanRoot;
  const file = resolveInputPath(relativePath, baseRoot);
  if (!inside(file, baseRoot) || (roam && !inside(file, noteRoot))) {
    const err = new Error(`File is outside note root: ${file}`);
    err.statusCode = 403;
    throw err;
  }
  if (!/\.(?:md|markdown)$/i.test(file)) {
    const err = new Error("New notes must use .md or .markdown");
    err.statusCode = 400;
    throw err;
  }
  const dir = dirname(file);
  await mkdir(dir, { recursive: true });
  if (existsSync(file)) {
    const err = new Error(`Node already exists: ${file}`);
    err.statusCode = 409;
    throw err;
  }
  let selection = null;
  let content = "";
  const template = await templateByKey(body.templateKey || body.template || "");
  if (template) {
    const relPath = displayPathForScanRoot(file, noteScanRoot);
    const expanded = expandTemplateBody(template.body, templateVarsForNode({
      title,
      id,
      tags,
      kind,
      path: relPath,
    }));
    content = expanded.text.replace(/\s+$/, "") + "\n";
    selection = expanded.selection;
    if (!hasNoteMetadata(content)) {
      const meta = buildMetaBlock({
        id: roam ? id : "",
        title,
        date: new Date().toISOString().slice(0, 10),
        kind,
        roam: roam ? "" : "off",
        tags,
        refs: [],
      });
      const prefix = `${meta}\n`;
      const offset = prefix.length;
      content = `${prefix}${content.replace(/^\s+/, "")}`;
      if (selection) selection = { from: selection.from + offset, to: selection.to + offset };
    } else if (!roam) {
      // Regular note from a template that already carries its own meta block:
      // ensure it is excluded from the roam graph.
      const { content: next, offset } = withMetaRoamOff(content);
      content = next;
      if (selection && offset) selection = { from: selection.from + offset, to: selection.to + offset };
    }
  } else {
    content = [
      buildMetaBlock({
        id: roam ? id : "",
        title,
        date: new Date().toISOString().slice(0, 10),
        kind,
        roam: roam ? "" : "off",
        tags,
        refs: [],
      }),
      `# ${title}`,
      "",
    ].join("\n");
  }
  await writeFile(file, content, "utf8");
  markNotesDirty(file);
  const opened = await readNote(file, { includeIndex: true });
  if (selection) opened.selection = selection;
  if (roam) queueRoamDbSync(opened.notes, [file]);
  return opened;
}

export async function createFolder(body) {
  const rawPath = String(body.path || body.dir || body.folder || "").trim();
  if (!rawPath) {
    const err = new Error("Missing folder path");
    err.statusCode = 400;
    throw err;
  }
  const dir = resolveInputPath(rawPath, noteScanRoot);
  if (!inside(dir, noteScanRoot)) {
    const err = new Error(`Folder is outside note root: ${dir}`);
    err.statusCode = 403;
    throw err;
  }
  await mkdir(dir, { recursive: true });
  markNotesDirty();
  const index = await notesIndexPayload();
  return {
    ok: true,
    path: displayPathForScanRoot(dir, noteScanRoot) || "Root",
    ...index,
  };
}

async function uniqueTrashPath(file) {
  const trashDir = join(homedir(), ".Trash");
  await mkdir(trashDir, { recursive: true });
  const ext = extname(file);
  const stem = basename(file, ext) || "note";
  let target = join(trashDir, basename(file));
  for (let i = 2; existsSync(target); i++) {
    target = join(trashDir, `${stem}-${i}${ext}`);
  }
  return target;
}

async function moveToTrash(file) {
  if (process.platform === "darwin") {
    try {
      await execFileAsync("osascript", [
        "-e",
        `tell application "Finder" to delete POSIX file ${JSON.stringify(file)}`,
      ]);
      return "system-trash";
    } catch {}
  }
  const target = await uniqueTrashPath(file);
  await rename(file, target);
  return target;
}

export function queueRoamDbSync(notes = null, changedFiles = []) {
  if (notes) queuedRoamSyncNotes = notes;
  const files = Array.isArray(changedFiles) ? changedFiles : [changedFiles];
  for (const file of files) {
    if (!file) continue;
    const normalized = resolveUserPath(file);
    if (!queuedRoamSyncChangedFiles.includes(normalized)) {
      queuedRoamSyncChangedFiles.push(normalized);
    }
  }
  if (roamSyncTimer) {
    clearTimeout(roamSyncTimer);
    roamSyncTimer = null;
  }
}

export function runtimeDebugSnapshot() {
  return {
    roamDbSync: {
      queued: Boolean(queuedRoamSyncNotes) || queuedRoamSyncChangedFiles.length > 0,
      changedFiles: queuedRoamSyncChangedFiles.length,
      inFlight: Boolean(roamSyncInFlight),
    },
    paths: {
      stateRoot,
      tmpRoot: runtimeTmpRoot || aaronnoteTmpRoot(),
    },
    saveWrites: {
      queuedFiles: saveWriteQueues.size,
    },
    copilot: {
      started: Boolean(copilotClient),
      busy: Boolean(copilotClient?.status?.busy),
      status: copilotClient?.status?.message || "Not started",
    },
  };
}

function scheduleRoamDbSync(notes, changedFile) {
  queueRoamDbSync(notes, changedFile ? [changedFile] : []);
}

export async function deleteNote(body) {
  const file = safeOpenFile(body.file);
  noteScanRoot = scanRootForOpenFile(file);
  let trashedTo = "";
  let info = null;
  try {
    info = await stat(file);
  } catch {}
  try {
    trashedTo = await moveToTrash(file);
  } catch (err) {
    if (err?.code !== "ENOENT") throw err;
  }
  markNotesDirty(file);
  const index = await notesIndexPayload();
  if (!standaloneFile(file)) queueRoamDbSync(index.notes, [file]);
  return { type: "deleted", ok: true, file, trashedTo, ...index };
}

function safeManagedPath(input) {
  const raw = String(input || "").trim();
  if (!raw) {
    const err = new Error("Missing path");
    err.statusCode = 400;
    throw err;
  }
  const file = resolveInputPath(raw, noteScanRoot);
  if (!inside(file, noteScanRoot)) {
    const err = new Error(`Path is outside note root: ${file}`);
    err.statusCode = 403;
    throw err;
  }
  return file;
}

async function managedPathInfo(input) {
  const file = safeManagedPath(input);
  const info = await stat(file);
  return { file, info };
}

function targetPathForRename(file, name) {
  const clean = sanitizeAssetName(name, basename(file));
  if (!clean) {
    const err = new Error("Missing new name");
    err.statusCode = 400;
    throw err;
  }
  return resolve(dirname(file), clean);
}

function targetPathForMove(file, body) {
  if (body.target || body.to || body.pathTo) return safeManagedPath(body.target || body.to || body.pathTo);
  const rawDir = String(body.directory || body.dir || "").trim();
  if (!rawDir) {
    const err = new Error("Missing target directory");
    err.statusCode = 400;
    throw err;
  }
  const dir = safeManagedPath(rawDir);
  return resolve(dir, basename(file));
}

async function assertMoveTargetParent(target) {
  let info = null;
  try {
    info = await stat(dirname(target));
  } catch {
    const err = new Error(`Target folder does not exist: ${dirname(target)}`);
    err.statusCode = 400;
    throw err;
  }
  if (!info.isDirectory()) {
    const err = new Error(`Target parent is not a folder: ${dirname(target)}`);
    err.statusCode = 400;
    throw err;
  }
}

function assertTargetWritable(source, target) {
  if (!inside(target, noteScanRoot)) {
    const err = new Error(`Target is outside note root: ${target}`);
    err.statusCode = 403;
    throw err;
  }
  if (target === source) {
    const err = new Error("Source and target are the same");
    err.statusCode = 400;
    throw err;
  }
  if (existsSync(target)) {
    const err = new Error(`Target already exists: ${target}`);
    err.statusCode = 409;
    throw err;
  }
}

async function fsPayload(extra = {}) {
  const index = await notesIndexPayload();
  return { ok: true, ...extra, ...index };
}

export async function renameManagedPath(body) {
  const { file, info } = await managedPathInfo(body.path || body.file);
  if (file === noteScanRoot) {
    const err = new Error("Cannot rename the root folder");
    err.statusCode = 400;
    throw err;
  }
  const target = targetPathForRename(file, body.name || body.targetName);
  assertTargetWritable(file, target);
  await rename(file, target);
  noteSelfWrite(file); noteSelfWrite(target);
  await renameManagedLeanMirror(file, target, info);
  markNotesDirty();
  return fsPayload({
    type: "fs-renamed",
    file: target,
    oldFile: file,
    path: displayPathForScanRoot(target, noteScanRoot) || "Root",
    oldPath: displayPathForScanRoot(file, noteScanRoot) || "Root",
  });
}

export async function moveManagedPath(body) {
  const { file, info } = await managedPathInfo(body.path || body.file);
  if (file === noteScanRoot) {
    const err = new Error("Cannot move the root folder");
    err.statusCode = 400;
    throw err;
  }
  const target = targetPathForMove(file, body);
  if (info.isDirectory() && inside(target, file)) {
    const err = new Error("Cannot move a folder into itself");
    err.statusCode = 400;
    throw err;
  }
  assertTargetWritable(file, target);
  await assertMoveTargetParent(target);
  await rename(file, target);
  noteSelfWrite(file); noteSelfWrite(target);
  await renameManagedLeanMirror(file, target, info);
  markNotesDirty();
  return fsPayload({
    type: "fs-moved",
    file: target,
    oldFile: file,
    path: displayPathForScanRoot(target, noteScanRoot) || "Root",
    oldPath: displayPathForScanRoot(file, noteScanRoot) || "Root",
  });
}

function duplicatePathFor(file, requested = "") {
  if (requested) return safeManagedPath(requested);
  const ext = extname(file);
  const stem = basename(file, ext);
  for (let i = 1; i < 10_000; i++) {
    const suffix = i === 1 ? " copy" : ` copy ${i}`;
    const target = resolve(dirname(file), `${stem}${suffix}${ext}`);
    if (!existsSync(target)) return target;
  }
  const err = new Error("Could not find a duplicate path");
  err.statusCode = 409;
  throw err;
}

export async function duplicateManagedFile(body) {
  const { file, info } = await managedPathInfo(body.path || body.file);
  if (!info.isFile()) {
    const err = new Error("Only files can be duplicated");
    err.statusCode = 400;
    throw err;
  }
  const target = duplicatePathFor(file, body.target || body.to || "");
  assertTargetWritable(file, target);
  await mkdir(dirname(target), { recursive: true });
  await copyFile(file, target);
  await copyManagedLeanMirror(file, target, info);
  markNotesDirty(target);
  return fsPayload({
    type: "fs-duplicated",
    file: target,
    oldFile: file,
    path: displayPathForScanRoot(target, noteScanRoot),
    oldPath: displayPathForScanRoot(file, noteScanRoot),
  });
}

async function directoryHasEntries(dir) {
  try {
    const entries = await readdir(dir);
    return entries.some((entry) => entry !== ".aaronnote-keep");
  } catch {
    return false;
  }
}

export async function trashManagedPath(body) {
  const file = safeManagedPath(body.path || body.file);
  let info;
  try {
    info = await stat(file);
  } catch (err) {
    if (err?.code === "ENOENT") {
      await deleteManagedLeanMirror(file, null);
      markNotesDirty();
      return fsPayload({
        type: "fs-missing",
        file,
        path: displayPathForScanRoot(file, noteScanRoot) || "Root",
      });
    }
    throw err;
  }
  if (file === noteScanRoot) {
    const err = new Error("Cannot trash the root folder");
    err.statusCode = 400;
    throw err;
  }
  if (info.isDirectory() && await directoryHasEntries(file) && body.confirm !== "TRASH") {
    const err = new Error("Type TRASH to move a non-empty folder to Trash");
    err.statusCode = 400;
    throw err;
  }
  const trashedTo = await moveToTrash(file);
  markNotesDirty();
  return fsPayload({
    type: "fs-trashed",
    file,
    trashedTo,
    path: displayPathForScanRoot(file, noteScanRoot) || "Root",
  });
}

export async function updateCurrentNoteMeta(body, action) {
  const file = safeFile(body.file);
  const content = typeof body.content === "string" ? body.content : await readFile(file, "utf8");
  let next = content;
  if (action === "remove") {
    next = removeMetaBlock(content);
  } else if (action === "tag") {
    const currentTags = tagsFromContent(content);
    const incoming = Array.isArray(body.tags) ? body.tags : parseListValue(body.tags || "");
    next = upsertMetaBlock(file, content, { tags: normalizeTags([...currentTags, ...incoming]) });
  } else if (action === "hide-roam") {
    next = upsertMetaBlock(file, content, { roam: "off" });
  } else if (action === "activate-roam") {
    next = upsertMetaBlock(file, content, { roam: "" });
  } else {
    next = upsertMetaBlock(file, content, {
      title: body.title,
      tags: body.tags || tagsFromContent(content),
      kind: body.kind || defaultNoteKind,
    });
  }
  if (next !== content) {
    await atomicWriteFile(file, next, "utf8");
    markNotesDirty(file);
  }
  const opened = await readNote(file, { includeIndex: true });
  if (next !== content) queueRoamDbSync(opened.notes, [file]);
  return opened;
}

async function rewriteRoamMetaTags(updateTags) {
  const scanned = await scanNotes();
  const changedFiles = [];
  const changed = [];
  for (const note of scanned.filter((item) => item.roam && item.file)) {
    let content = "";
    try {
      content = await readFile(note.file, "utf8");
    } catch {
      continue;
    }
    const before = tagsFromContent(content);
    const after = normalizeTags(updateTags(before));
    if (sameStringList(before, after)) continue;
    const next = upsertMetaBlock(note.file, content, { tags: after });
    if (next === content) continue;
    await atomicWriteFile(note.file, next, "utf8");
    markNotesDirty(note.file);
    changedFiles.push(note.file);
    changed.push({ file: note.file, path: note.path || "", title: note.title || "", tags: after });
  }
  const index = await notesIndexPayload();
  if (changedFiles.length > 0) queueRoamDbSync(index.notes, changedFiles);
  return { ok: true, changed, changedCount: changed.length, ...index };
}

export async function renameRoamTag(body) {
  const from = String(body.from || body.old || "").trim().replace(/^#/, "");
  const to = String(body.to || body.next || "").trim().replace(/^#/, "");
  if (!from || !to) {
    const err = new Error("Missing tag rename values");
    err.statusCode = 400;
    throw err;
  }
  return rewriteRoamMetaTags((tags) => tags.map((tag) => tag.toLowerCase() === from.toLowerCase() ? to : tag));
}

export async function deleteRoamTag(body) {
  const tag = String(body.tag || body.name || "").trim().replace(/^#/, "");
  if (!tag) {
    const err = new Error("Missing tag");
    err.statusCode = 400;
    throw err;
  }
  return rewriteRoamMetaTags((tags) => tags.filter((item) => item.toLowerCase() !== tag.toLowerCase()));
}

export async function roamTagOverlapReport() {
  const scanned = await scanNotes();
  const byTag = new Map();
  const variants = new Map();
  for (const note of scanned.filter((item) => item.roam)) {
    const key = graphNoteKey(note);
    if (!key) continue;
    for (const tag of note.tags || []) {
      const clean = String(tag || "").trim().replace(/^#/, "");
      if (!clean) continue;
      const lower = clean.toLowerCase();
      if (!byTag.has(lower)) byTag.set(lower, { name: clean, lower, notes: new Map() });
      byTag.get(lower).notes.set(key, { key, id: note.id || "", title: note.title || "", path: note.path || "" });
      if (!variants.has(lower)) variants.set(lower, new Set());
      variants.get(lower).add(clean);
    }
  }
  const duplicateCase = [...variants.entries()]
    .map(([lower, names]) => ({ lower, variants: [...names].sort((a, b) => a.localeCompare(b)) }))
    .filter((item) => item.variants.length > 1);
  const entries = [...byTag.values()].filter((entry) => entry.notes.size >= 2);
  const overlaps = [];
  for (let i = 0; i < entries.length; i++) {
    for (let j = i + 1; j < entries.length; j++) {
      const a = entries[i];
      const b = entries[j];
      const aKeys = new Set(a.notes.keys());
      const bKeys = new Set(b.notes.keys());
      const sharedKeys = [...aKeys].filter((key) => bKeys.has(key));
      if (sharedKeys.length < 2) continue;
      const containment = sharedKeys.length / Math.min(aKeys.size, bKeys.size);
      const jaccard = sharedKeys.length / new Set([...aKeys, ...bKeys]).size;
      if (containment < 0.8 && jaccard < 0.65) continue;
      overlaps.push({
        a: a.name,
        b: b.name,
        aCount: aKeys.size,
        bCount: bKeys.size,
        sharedCount: sharedKeys.length,
        containment,
        jaccard,
        notes: sharedKeys.slice(0, 8).map((key) => a.notes.get(key)),
      });
    }
  }
  overlaps.sort((a, b) =>
    b.containment - a.containment
    || b.sharedCount - a.sharedCount
    || `${a.a}/${a.b}`.localeCompare(`${b.a}/${b.b}`));
  return {
    ok: true,
    duplicateCase,
    overlaps: overlaps.slice(0, 80),
    tagCount: byTag.size,
  };
}

function notePathWithoutRoamPrefix(path) {
  return normalizeNoteRefPath(path).replace(/^roam\//i, "");
}

function notePathKeyVariants(path) {
  const clean = normalizeNoteRefPath(path);
  const noRoam = notePathWithoutRoamPrefix(clean);
  return new Set([
    canonicalServerNoteRef(clean),
    canonicalServerNoteRef(noRoam),
    canonicalServerNoteRef(`roam/${noRoam}`),
  ].filter(Boolean));
}

function relativeNotePath(fromDir, toPath) {
  const fromParts = fromDir && fromDir !== "Root" ? directoryPathParts(fromDir) : [];
  const toParts = directoryPathParts(toPath);
  let shared = 0;
  while (shared < fromParts.length && shared < toParts.length && fromParts[shared] === toParts[shared]) shared++;
  const parts = [
    ...Array.from({ length: fromParts.length - shared }, () => ".."),
    ...toParts.slice(shared),
  ];
  return parts.join("/") || toParts.at(-1) || "";
}

function hrefPathSuffixIndex(href) {
  const raw = String(href || "");
  const indexes = ["?", "#"]
    .map((token) => raw.indexOf(token))
    .filter((index) => index >= 0);
  const at = raw.lastIndexOf("@");
  if (at > raw.lastIndexOf("/") && /\.(?:md|markdown|typ)$/i.test(raw.slice(0, at))) indexes.push(at);
  return indexes.length > 0 ? Math.min(...indexes) : raw.length;
}

function markdownHrefMatch(href, note, oldKeys) {
  const protocol = hrefProtocol(href);
  if (protocol && protocol !== "file") return null;
  let path = noteFileRefFromHref(href);
  if (!path) return null;
  const sourceDir = directoryParentPath(note.path || "");
  const direct = notePathKeyVariants(path);
  if ([...direct].some((key) => oldKeys.has(key))) return { sourceDir, relative: false };
  if (path.startsWith("/")) {
    const resolved = resolveUserPath(path);
    if (inside(resolved, noteRoot)) {
      const rel = slashPath(relativeCanonical(noteRoot, resolved));
      if ([...notePathKeyVariants(rel)].some((key) => oldKeys.has(key))) return { sourceDir, relative: false };
    }
    return null;
  }
  if (!/^roam\//i.test(path)) {
    const fromSource = normalizeNoteRefPath(`${sourceDir === "Root" ? "" : sourceDir}/${path}`);
    if ([...notePathKeyVariants(fromSource)].some((key) => oldKeys.has(key))) return { sourceDir, relative: true };
  }
  return null;
}

function replacementHrefPath(href, match, newPath) {
  const raw = String(href || "");
  const pathEnd = hrefPathSuffixIndex(raw);
  const oldPath = raw.slice(0, pathEnd);
  const suffix = raw.slice(pathEnd);
  const nextRootPath = notePathWithoutRoamPrefix(newPath);
  let nextPath = nextRootPath;
  if (/^roam\//i.test(oldPath)) nextPath = `roam/${nextRootPath}`;
  else if (oldPath.startsWith("/")) nextPath = `/${nextRootPath}`;
  else if (oldPath.startsWith(".") || match.relative) nextPath = relativeNotePath(match.sourceDir, nextRootPath);
  return `${nextPath.replace(/ /g, "%20")}${suffix}`;
}

function rewriteMarkdownPathRefsInContent(content, note, oldPath, newPath) {
  const oldKeys = notePathKeyVariants(oldPath);
  const destinations = markdownLinkDestinations(content)
    .map((dest) => ({ dest, match: markdownHrefMatch(dest.href, note, oldKeys) }))
    .filter((item) => item.match);
  if (destinations.length === 0) return { content, count: 0 };
  let next = content;
  for (const { dest, match } of destinations.reverse()) {
    const href = replacementHrefPath(dest.href, match, newPath);
    next = `${next.slice(0, dest.hrefFrom)}${href}${next.slice(dest.hrefTo)}`;
  }
  return { content: next, count: destinations.length };
}

export async function rewriteMarkdownPathReferences(body) {
  const oldPath = String(body.oldPath || body.from || "").trim();
  const newPath = String(body.newPath || body.to || "").trim();
  if (!oldPath || !newPath) {
    const err = new Error("Missing path rewrite values");
    err.statusCode = 400;
    throw err;
  }
  const dryRun = body.dryRun === true;
  const scanned = await scanNotes();
  const changedFiles = [];
  const changed = [];
  for (const note of scanned.filter((item) => item.file)) {
    let content = "";
    try {
      content = await readFile(note.file, "utf8");
    } catch {
      continue;
    }
    const result = rewriteMarkdownPathRefsInContent(content, note, oldPath, newPath);
    if (result.count === 0 || result.content === content) continue;
    if (!dryRun) {
      await atomicWriteFile(note.file, result.content, "utf8");
      markNotesDirty(note.file);
      changedFiles.push(note.file);
    }
    changed.push({ file: note.file, path: note.path || "", title: note.title || "", count: result.count });
  }
  const index = dryRun ? await notesIndexPayload(scanned) : await notesIndexPayload();
  if (!dryRun && changedFiles.length > 0) queueRoamDbSync(index.notes, changedFiles);
  return { ok: true, dryRun, changed, changedCount: changed.length, referenceCount: changed.reduce((sum, item) => sum + item.count, 0), ...index };
}

function acceptSaveRequest(file, body) {
  const clientId = typeof body.clientId === "string" ? body.clientId : "";
  const seq = Number(body.seq);
  if (!clientId || !Number.isSafeInteger(seq) || seq <= 0) return true;
  const key = `${clientId}\0${file}`;
  const previous = saveRequestVersions.get(key) ?? 0;
  if (seq < previous) return false;
  saveRequestVersions.set(key, seq);
  if (saveRequestVersions.size > 2000) {
    for (const oldKey of saveRequestVersions.keys()) {
      saveRequestVersions.delete(oldKey);
      if (saveRequestVersions.size <= 1000) break;
    }
  }
  return true;
}

async function enqueueSaveWrite(file, task) {
  const previous = saveWriteQueues.get(file) ?? Promise.resolve();
  const current = previous.catch(() => {}).then(task);
  saveWriteQueues.set(file, current);
  try {
    return await current;
  } finally {
    if (saveWriteQueues.get(file) === current) saveWriteQueues.delete(file);
  }
}

export function configure(options = {}) {
  noteRoot = resolveUserPath(options.root || process.env.AARONNOTE_ROOT || join(appDir, "..", "roam"));
  noteScanRoot = noteRoot;
  workspaceRoot = resolve(String(options.workspaceRoot || process.env.AARONNOTE_WORKSPACE_ROOT || resolve(appDir, "..")));
  publishJsDir = resolve(String(options.publishJsDir || process.env.AARONNOTE_PUBLISH_JS_DIR || join(workspaceRoot, "js")));
  stateRoot = resolve(String(options.stateRoot || process.env.AARONNOTE_STATE_DIR || join(workspaceRoot, "var", "aaronnote")));
  runtimeTmpRoot = configureTmpRoot(options.tmpRoot || process.env.AARONNOTE_TMP_DIR || join(stateRoot, "tmp"));
  snippetsRoot = resolve(String(options.snippetsRoot || process.env.AARONNOTE_SNIPPETS_ROOT || join(workspaceRoot, "snippets")));
  templatesRoot = resolve(String(options.templatesRoot || process.env.AARONNOTE_TEMPLATES_ROOT || join(workspaceRoot, "templates", "aaronnote")));
  snippetCache = { key: "", scannedAt: 0, snippets: [] };
  templateCache = { key: "", scannedAt: 0, templates: [] };
  contentRootCache.clear();
  noteCodeFileCache.clear();
  noteCodeFilePending.clear();
  noteCodeFileCacheBytes = 0;
  if (roamSyncTimer) {
    clearTimeout(roamSyncTimer);
    roamSyncTimer = null;
  }
  roamSyncInFlight = null;
  queuedRoamSyncNotes = null;
  queuedRoamSyncChangedFiles = [];
  markNotesDirty();
}

export async function saveNote(body) {
  const file = safeOpenFile(body.file);
  const content = String(body.content ?? "");
  const previousContent = await readFile(file, "utf8").catch(() => "");
  if (body.force !== true && content.trim() === "" && previousContent.trim() !== "") {
    return {
      type: "saved", ok: false, file,
      message: "Refusing to save empty content over a non-empty file. Use force: true to override.",
    };
  }
  const bookSensitiveSave = contentMayAffectBook(previousContent) || contentMayAffectBook(content);
  const force = body.force === true;
  const baseMtimeMs = Number(body.baseMtimeMs);
  const wrote = await enqueueSaveWrite(file, async () => {
    if (!acceptSaveRequest(file, body)) return false;
    if (!force && Number.isFinite(baseMtimeMs) && baseMtimeMs > 0) {
      try {
        const current = await stat(file);
        if (Math.abs(current.mtimeMs - baseMtimeMs) > 1) {
          return { conflict: true, mtimeMs: current.mtimeMs, size: current.size };
        }
      } catch {}
    }
    await atomicWriteFile(file, content, "utf8");
    const info = await stat(file);
    markNotesDirty(file);
    return { wrote: true, mtimeMs: info.mtimeMs, size: info.size };
  });
  if (wrote && typeof wrote === "object" && wrote.conflict) {
    return { type: "saved", ok: false, file, conflict: true, message: "File changed on disk. Review before overwriting.", mtimeMs: wrote.mtimeMs, size: wrote.size };
  }
  if (!wrote) {
    return { type: "saved", ok: true, file, stale: true, message: "Skipped stale save" };
  }
  if (standaloneFile(file)) {
    noteScanRoot = scanRootForOpenFile(file);
    markNotesDirty(file);
    return { type: "saved", ok: true, file, kind: kindFromContent(content), message: "Saved", note: await noteSummaryForFile(file, content), notesRefresh: "deferred", standalone: true, mtimeMs: wrote.mtimeMs, size: wrote.size };
  }
  const refresh = body.refresh === "deferred" ? "deferred" : "full";
  if (refresh === "deferred") {
    markNotesDirty(file);
    if (bookSensitiveSave) {
      const notes = await scanNotes();
      scheduleRoamDbSync(notes, file);
      const note = notes.find((item) => item.file === file) || await noteSummaryForFile(file, content);
      return {
        type: "saved",
        ok: true,
        file,
        message: "Saved",
        note,
        notes,
        kind: kindFromContent(content),
        notesRefresh: "book",
        standalone: false,
        mtimeMs: wrote.mtimeMs,
        size: wrote.size,
      };
    }
    scheduleRoamDbSync(null, file);
    return { type: "saved", ok: true, file, message: "Saved", note: await noteSummaryForFile(file, content), kind: kindFromContent(content), notesRefresh: "deferred", standalone: false, mtimeMs: wrote.mtimeMs, size: wrote.size };
  }
  const notes = await scanNotes();
  scheduleRoamDbSync(notes, file);
  return { type: "saved", ok: true, file, message: "Saved", notes, notesRefresh: "full", standalone: false, mtimeMs: wrote.mtimeMs, size: wrote.size };
}

export async function bootstrapNote(file) {
  if (file) {
    return readNote(file, { includeIndex: true });
  }
  noteScanRoot = noteRoot;
  const index = await notesIndexPayload();
  const snippets = await scanSnippets();
  const templates = await scanTemplates();
  return { type: "open", file: "", title: "Aaronnote", mode: "markdown", content: "# Aaronnote\n\nSelect a note from the left, or keep this scratch buffer.", ...index, snippets, templates, root: noteRoot, noteDir: "." };
}

export async function getTodos(file) {
  if (file) {
    const safe = safeOpenFile(file);
    if (standaloneFile(safe)) noteScanRoot = scanRootForOpenFile(safe);
  }
  return { type: "todos", todos: await scanTodos(), root: noteScanRoot };
}
