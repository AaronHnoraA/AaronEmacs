/**
 * Aaronnote web host for Emacs/Appine.
 *
 * Aaronnote owns the editable CodeMirror document in the browser.  Emacs only
 * starts this host, opens the local URL in Appine/xwidget, and receives coarse
 * events such as "open this file in Emacs".  There is intentionally no
 * per-keystroke Emacs -> browser preview stream here.
 */

import { createServer } from "node:http";
import { existsSync, statSync } from "node:fs";
import { readFile, rm, stat } from "node:fs/promises";
import { execFile, spawn } from "node:child_process";
import { promisify } from "node:util";
import { dirname, extname, isAbsolute, join, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

import {
  bootstrapNote,
  readNote,
  notesIndexPayload,
  roamNotesIndexPayload,
  graphPayload,
  wantedPages,
  scanRoamNotes,
  scanNotes,
  tagIndexPayload,
  pathSuggestionsForFile,
  latexExportDefaults,
  latexExportAgentStatus,
  setLatexExportAgent,
  listLatexTemplates,
  chooseLatexOutputPath,
  exportLatex,
  readNoteCodeRegion,
  syncRoamDb,
  scanSnippets,
  scanTemplates,
  renameRoamTag,
  deleteRoamTag,
  roamTagOverlapReport,
  rewriteMarkdownPathReferences,
  getTodos,
  updateTodoStatus,
  runtimeDebugSnapshot,
} from "./server/lib/index.mjs";
import { configure, markNotesDirty, notesIndexVersionValue, noteSelfWriteRecently, notePathWatchRelevant } from "./server/lib/state.mjs";
import { startNoteWatcher } from "./server/lib/watch.mjs";
import { saveNote } from "./server/lib/save.mjs";
import {
  storeAsset,
  storeAssetFromPath,
  renderTikzAsset,
  scanUnusedAssets,
  trashUnusedAssets,
} from "./server/lib/assets.mjs";
import {
  createNode,
  createFolder,
  deleteNote,
  renameManagedPath,
  moveManagedPath,
  duplicateManagedFile,
  trashManagedPath,
} from "./server/lib/fs-ops.mjs";
import { updateCurrentNoteMeta } from "./server/lib/meta.mjs";
import { resolveContentFile, resolveMediaFile, fileContentType } from "./server/lib/media.mjs";
import {
  readRecentNotes,
  touchRecentNote,
  readCursorPositions,
  touchCursorPosition,
} from "./server/lib/session.mjs";
import { handleCopilotRequest, shutdownCopilot } from "./server/lib/copilot.mjs";
import { acceptProseWord, runExternalProseChecks } from "./server/lib/prose-check.mjs";
import { createImeSwitcher } from "./server/lib/ime.mjs";

const ime = createImeSwitcher();
import { runtimeMkdtemp, sweepRuntimeTmp } from "./server/lib/tmp.mjs";
import { loadKatexMacros } from "./server/lib/katex-macros.mjs";

const execFileAsync = promisify(execFile);

const scriptDir = dirname(fileURLToPath(import.meta.url));
const webDir = resolve(process.env.AARONNOTE_WEB_DIR || join(scriptDir, "dist", "aaronnote"));
const runtimeRoot = resolve(process.env.AARONNOTE_RUNTIME_ROOT || scriptDir);
const workspaceRoot = resolve(process.env.AARONNOTE_WORKSPACE_ROOT || resolve(scriptDir, "..", "..", ".."));
const noteRoot = resolve(process.env.AARONNOTE_ROOT || join(workspaceRoot, ".roam"));
const publishJsDir = resolve(process.env.AARONNOTE_PUBLISH_JS_DIR || join(runtimeRoot, "js"));
const stateRoot = resolve(process.env.AARONNOTE_STATE_DIR || join(workspaceRoot, "var", "aaronnote"));
const tmpRoot = resolve(process.env.AARONNOTE_TMP_DIR || join(stateRoot, "tmp"));
const snippetsRoot = resolve(process.env.AARONNOTE_SNIPPETS_ROOT || join(workspaceRoot, "snippets"));
const templatesRoot = resolve(process.env.AARONNOTE_TEMPLATES_ROOT || join(workspaceRoot, "templates", "aaronnote"));
const katexMacrosDir = resolve(process.env.AARONNOTE_KATEX_MACROS_DIR || join(workspaceRoot, "etc", "katex-macros"));
const bindHost = process.env.AARONNOTE_WEB_HOST || "127.0.0.1";
const bindPort = Number(process.env.AARONNOTE_WEB_PORT || 0);
const liuGongQuanFontCandidates = [
  process.env.AARONNOTE_LIUGONGQUAN_FONT,
  join(homedir(), "Library", "Fonts", "方正柳公权楷书 简繁.TTF"),
  join(homedir(), "Library", "Fonts", "FZLiuGongQuanKaiShuJF.ttf"),
].filter(Boolean);

configure({
  root: noteRoot,
  workspaceRoot,
  publishJsDir,
  stateRoot,
  tmpRoot,
  snippetsRoot,
  templatesRoot,
});

// One-shot orphan sweep: remove staging/clipboard/db temp files older than 24h.
void sweepRuntimeTmp().then(({ removed }) => {
  if (removed > 0) process.stderr.write(`[aaronnote-web] swept ${removed} orphaned tmp file(s)\n`);
}).catch(() => {});

// Vault file watcher: marks the note index dirty on external changes (Emacs
// saves, git pull, dired renames, etc.) and broadcasts a notes-index-changed
// SSE event so connected pages can refresh their notes array without polling.
// Self-writes (the server's own atomic saves/renames) are suppressed within a
// 2-second window to avoid redundant index re-reads.
// Set AARONNOTE_WATCH=0 to disable (useful in test environments).
const noteWatcher = process.env.AARONNOTE_WATCH !== "0"
  ? startNoteWatcher({
      root: noteRoot,
      isRelevant: notePathWatchRelevant,
      isSelfWrite: (file) => noteSelfWriteRecently(file),
      onBatch(files) {
        for (const file of files) markNotesDirty(file);
        broadcast("command", { command: "notes-index-changed", version: notesIndexVersionValue() });
      },
      onFullRescan() {
        markNotesDirty();
        broadcast("command", { command: "notes-index-changed", version: notesIndexVersionValue() });
      },
    })
  : { close() {} };

if (!existsSync(webDir)) {
  process.stderr.write(
    `[aaronnote-web] FATAL: web app directory not found: ${webDir}\n` +
    `[aaronnote-web] Run "npm run build" in ${runtimeRoot} to build first.\n`
  );
  process.exit(1);
}

const eventClients = new Set();

// SSE keepalive heartbeat — prevents hung-client memory leak and keeps
// connections alive through idle-timeout proxies.
const sseHeartbeatInterval = setInterval(() => {
  const dead = [];
  for (const res of eventClients) {
    try { res.write(": keepalive\n\n"); } catch { dead.push(res); }
  }
  dead.forEach((res) => eventClients.delete(res));
}, 25000);
sseHeartbeatInterval.unref();

// Keep the process alive on unexpected errors (forcing exit would drop any
// unsaved editor state), but do not let the failure be silent: surface a
// bounded diagnostic on the SSE stream so the editor / Emacs can react instead
// of the server wedging in a half-broken state unnoticed.
function reportServerError(kind, detail) {
  const text = detail?.stack || String(detail ?? "");
  process.stderr.write(`[aaronnote-web] ${kind}: ${text}\n`);
  try {
    broadcast("command", {
      command: "server-error",
      kind,
      message: (detail instanceof Error ? detail.message : String(detail ?? "")).slice(0, 500),
      at: Date.now(),
    });
  } catch {
    // Never let diagnostic broadcasting trigger another uncaughtException.
  }
}

process.on("uncaughtException", (err) => reportServerError("uncaughtException", err));
process.on("unhandledRejection", (reason) => reportServerError("unhandledRejection", reason));

async function shutdown() {
  clearInterval(sseHeartbeatInterval);
  for (const res of eventClients) {
    try { res.end(); } catch {}
  }
  eventClients.clear();
  try { noteWatcher.close(); } catch {}
  server.close();
  try { await shutdownCopilot(); } catch {}
  process.exit(0);
}
process.on("SIGTERM", shutdown);
process.on("SIGINT", shutdown);

const MIME = {
  ".css": "text/css; charset=utf-8",
  ".gif": "image/gif",
  ".html": "text/html; charset=utf-8",
  ".ico": "image/x-icon",
  ".jpeg": "image/jpeg",
  ".jpg": "image/jpeg",
  ".js": "application/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".mjs": "application/javascript; charset=utf-8",
  ".pdf": "application/pdf",
  ".png": "image/png",
  ".svg": "image/svg+xml",
  ".ttf": "font/ttf",
  ".wasm": "application/wasm",
  ".webp": "image/webp",
  ".woff": "font/woff",
  ".woff2": "font/woff2",
};

function mimeFor(file) {
  return MIME[extname(file).toLowerCase()] || fileContentType(file) || "application/octet-stream";
}

function isWithin(root, file) {
  const normalizedRoot = resolve(root);
  const normalizedFile = resolve(file);
  return normalizedFile === normalizedRoot
    || normalizedFile.startsWith(normalizedRoot + sep);
}

async function isFile(file) {
  try {
    return (await stat(file)).isFile();
  } catch {
    return false;
  }
}

function sendJson(res, status, value) {
  res.writeHead(status, { "Content-Type": "application/json; charset=utf-8" });
  res.end(JSON.stringify(value));
}

function sendText(res, status, value, type = "text/plain; charset=utf-8") {
  res.writeHead(status, { "Content-Type": type });
  res.end(value);
}

function sendHtmlNoStore(res, value) {
  res.writeHead(200, {
    "Content-Type": "text/html; charset=utf-8",
    "Cache-Control": "no-store",
  });
  res.end(value);
}

function sendSse(res, event, data) {
  res.write(`event: ${event}\ndata: ${JSON.stringify(data)}\n\n`);
}

function broadcast(event, data) {
  for (const res of eventClients) {
    try {
      sendSse(res, event, data);
    } catch {
      eventClients.delete(res);
    }
  }
}

function errorPayload(err) {
  return {
    type: "error",
    ok: false,
    message: err instanceof Error ? err.message : String(err),
  };
}

function assetProxyPath(raw) {
  return `/aaronnote-asset?url=${encodeURIComponent(String(raw || ""))}`;
}

function transformJavaScript(text) {
  if (!text.includes("aaronnote-asset://")) return text;
  return text
    .replaceAll("aaronnote-asset://roam-tools", assetProxyPath("aaronnote-asset://roam-tools"))
    .replaceAll("aaronnote-asset://kinds/", assetProxyPath("aaronnote-asset://kinds/"))
    .replaceAll(
      "aaronnote-asset://font/FZLiuGongQuanKaiShuJF.ttf",
      assetProxyPath("aaronnote-asset://font/FZLiuGongQuanKaiShuJF.ttf"),
    );
}

function cleanStatusCode(err, fallback = 500) {
  const code = Number(err?.statusCode || err?.status);
  return Number.isFinite(code) && code >= 400 && code < 600 ? code : fallback;
}

function readText(req, maxBytes = 4 * 1024 * 1024) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    let size = 0;
    req.on("data", (chunk) => {
      size += chunk.length;
      if (size > maxBytes) {
        reject(Object.assign(new Error("Request body too large"), { statusCode: 413 }));
      } else {
        chunks.push(chunk);
      }
    });
    req.on("end", () => resolve(Buffer.concat(chunks).toString("utf8")));
    req.on("error", reject);
  });
}

function readJson(req, maxBytes = 64 * 1024 * 1024) {
  return new Promise((resolveBody, reject) => {
    let body = "";
    req.on("data", (chunk) => {
      body += chunk;
      if (body.length > maxBytes) {
        reject(Object.assign(new Error("Request body too large"), { statusCode: 413 }));
      }
    });
    req.on("end", () => {
      try {
        resolveBody(body ? JSON.parse(body) : {});
      } catch (err) {
        reject(Object.assign(err, { statusCode: 400 }));
      }
    });
    req.on("error", reject);
  });
}

async function notesListPayload(force = false) {
  if (force) markNotesDirty();
  return { type: "notes", ...await notesIndexPayload(), root: noteRoot };
}

let cachedCompletionTags = null;
let cachedCompletionTagsVersion = -1;
async function getCachedCompletionTags() {
  const version = notesIndexVersionValue();
  if (cachedCompletionTags && cachedCompletionTagsVersion === version) return cachedCompletionTags;
  const payload = tagIndexPayload(await scanNotes());
  const names = payload.tags.map((tag) => tag.name);
  cachedCompletionTags = {
    names,
    lowerNames: names.map((name) => name.toLowerCase()),
  };
  cachedCompletionTagsVersion = version;
  return cachedCompletionTags;
}

let cachedCompletionRoamNotes = null;
let cachedCompletionRoamVersion = -1;
async function getCachedCompletionRoamNotes() {
  const version = notesIndexVersionValue();
  if (cachedCompletionRoamNotes && cachedCompletionRoamVersion === version) return cachedCompletionRoamNotes;
  cachedCompletionRoamNotes = (await scanRoamNotes())
    .filter((note) => note.roam && (note.id || note.key || note.title))
    .map((note) => ({
      id: note.id || note.key || "",
      key: note.key || note.id || "",
      title: note.title || "",
      path: note.path || note.file || "",
      search: [note.id, note.key, note.title, ...(note.aliases || [])]
        .map((value) => String(value || "").toLowerCase())
        .join(" "),
    }));
  cachedCompletionRoamVersion = version;
  return cachedCompletionRoamNotes;
}

function roamSyncStats(index) {
  const noteList = index.notes || [];
  return {
    noteCount: noteList.length,
    linkCount: noteList.reduce((sum, n) => sum + (n.refs?.length || 0), 0),
    tagCount: new Set(noteList.flatMap(n => n.tags || [])).size,
    dirCount: (index.directories || []).length,
  };
}

async function roamSyncPayload(reload = false) {
  if (reload) markNotesDirty();
  const notes = await syncRoamDb();
  const index = await notesIndexPayload(notes);
  return { type: "notes", ...index, stats: roamSyncStats(index), root: noteRoot, db: join(noteRoot, "roam.db") };
}

async function roamSyncFullPayload() {
  markNotesDirty();
  const notes = await syncRoamDb(null, { mode: "full" });
  const index = await notesIndexPayload(notes);
  return { type: "notes", ...index, stats: roamSyncStats(index), root: noteRoot, db: join(noteRoot, "roam.db") };
}

async function templatesPayload(force = false) {
  return { type: "templates", templates: await scanTemplates({ force }) };
}

async function snippetsPayload(force = false) {
  return { type: "snippets", snippets: await scanSnippets({ force }) };
}

function resolveShellPath(file) {
  const raw = String(file || "").trim();
  if (!raw || raw === "Root") return noteRoot;
  return resolve(isAbsolute(raw) ? raw : join(noteRoot, raw));
}

function openTargetProtocol(value) {
  return String(value || "").match(/^([a-z][a-z0-9+.-]*):/i)?.[1]?.toLowerCase() || "";
}

function resolveSystemOpenTarget(target, base = "") {
  const value = String(target || "").trim();
  const protocol = openTargetProtocol(value);
  if (protocol && protocol !== "file") return value;
  return resolveContentFile(value, base);
}

function resolveShellDirectoryPath(path, base = "") {
  const raw = String(path || "").trim();
  const baseFile = String(base || "").trim() ? resolveShellPath(base) : "";
  const baseDir = baseFile && !isWithin(noteRoot, baseFile) ? dirname(baseFile) : noteRoot;
  const target = !raw || raw === "Root" ? baseDir : resolve(isAbsolute(raw) ? raw : join(baseDir, raw));
  try {
    return existsSync(target) && statSync(target).isDirectory() ? target : dirname(target);
  } catch {
    return target;
  }
}

async function macOpen(args) {
  if (process.platform !== "darwin") return { ok: false, message: "Native open is only available on macOS in this host" };
  try {
    await execFileAsync("open", args);
    return { ok: true };
  } catch (err) {
    return { ok: false, message: err instanceof Error ? err.message : String(err) };
  }
}

async function showInFolder(file) {
  const target = resolveShellPath(file);
  const safeTarget = isWithin(noteRoot, target) ? target : noteRoot;
  const result = await macOpen(["-R", safeTarget]);
  return { ...result, file: safeTarget };
}

async function openPath(file) {
  const target = resolveShellPath(file);
  const safeTarget = isWithin(noteRoot, target) ? target : noteRoot;
  const result = await macOpen([safeTarget]);
  return { ...result, file: safeTarget };
}

async function openDirectory(body) {
  const target = resolveShellDirectoryPath(body?.path ?? body, body?.base ?? "");
  const result = await macOpen([target]);
  return { ...result, file: target };
}

async function apiOpenInEmacs(file, line = 1, col = 0, tag = "") {
  const target = resolveShellPath(file);
  const payload = { file: target, line, col };
  if (tag) payload.tag = String(tag);
  process.stdout.write(`aaronote-event:open:${JSON.stringify(payload)}\n`);
  return { ok: true, ...payload };
}

async function apiCurrentFile(body) {
  const raw = String((body && typeof body === "object" ? body.file : body) || "").trim();
  const client = String((body && typeof body === "object" ? body.client : "") || "").trim();
  const target = raw ? resolveShellPath(raw) : "";
  const payload = { file: target };
  if (client) payload.client = client;
  process.stdout.write(`aaronote-event:current-file:${JSON.stringify(payload)}\n`);
  return { ok: true, ...payload };
}

async function apiEmacsKey(key) {
  const k = String(key || "").trim();
  if (!k || k.length > 32) return { ok: false, message: "Invalid key" };
  process.stdout.write(`aaronote-event:key:${JSON.stringify({ key: k })}\n`);
  return { ok: true };
}

async function apiSystemOpen(body) {
  const value = String((body && typeof body === "object" ? body.target : body) || "").trim();
  const base = String((body && typeof body === "object" ? body.base : "") || "");
  if (!value) {
    const err = new Error("system-open: empty target");
    err.statusCode = 400;
    throw err;
  }
  const resolved = resolveSystemOpenTarget(value, base);
  process.stdout.write(`aaronote-event:system-open:${JSON.stringify({ target: resolved })}\n`);
  return { ok: true, target: resolved };
}

const apiHandlers = {
  "aaronnote:api:notes:bootstrap": (file) => bootstrapNote(file || undefined),
  "aaronnote:api:notes:open": (file) => readNote(file),
  "aaronnote:api:notes:list": (force) => notesListPayload(force === true),
  "aaronnote:api:notes:save": async (body) => {
    const result = await saveNote(body || {});
    if (result?.ok && !result?.conflict && result?.file) {
      process.stdout.write(`aaronote-event:saved:${JSON.stringify({ file: String(result.file) })}\n`);
    }
    return result;
  },
  "aaronnote:api:notes:create-node": (draft) => createNode(draft || {}),
  "aaronnote:api:notes:delete": (file) => deleteNote({ file }),
  "aaronnote:api:notes:delete-node": (file) => deleteNote({ file }),
  "aaronnote:api:notes:create-folder": (path) => createFolder({ path }),
  "aaronnote:api:notes:path-suggestions": async (body) => {
    const file = typeof body === "string" ? body : body?.file;
    const prefix = typeof body === "string" ? "./" : body?.prefix;
    return { type: "path-suggestions", paths: await pathSuggestionsForFile(file || "", prefix || "./") };
  },
  "aaronnote:api:completions:tags": async (body) => {
    const prefix = String(body?.prefix || "").toLowerCase();
    const { names, lowerNames } = await getCachedCompletionTags();
    const filtered = prefix ? names.filter((_, index) => lowerNames[index].includes(prefix)) : names;
    return { type: "completion-tags", tags: filtered.slice(0, 50) };
  },
  "aaronnote:api:completions:roam": async (body) => {
    const prefix = String(body?.prefix || "").toLowerCase();
    const roamNotes = await getCachedCompletionRoamNotes();
    const matches = prefix ? roamNotes.filter((note) => note.search.includes(prefix)) : roamNotes;
    return {
      type: "completion-roam",
      notes: matches.slice(0, 20).map((note) => ({
        id: note.id,
        key: note.key,
        title: note.title,
        path: note.path,
      })),
    };
  },
  "aaronnote:api:notes:todos": async (body) => {
    return await getTodos(typeof body === "string" ? body : body?.file || "");
  },
  "aaronnote:api:notes:update-todo": (body) => updateTodoStatus(body || {}),
  "aaronnote:api:notes:index": async () => {
    return { type: "notes", ...await notesIndexPayload(), root: noteRoot };
  },
  "aaronnote:api:notes:roam-index": async () => {
    return { type: "notes", ...await roamNotesIndexPayload(), root: noteRoot };
  },
  "aaronnote:api:runtime:debug": async () => ({ type: "runtime-debug", ...runtimeDebugSnapshot() }),
  "aaronnote:api:note-code:read-region": (body) => readNoteCodeRegion(body || {}),
  "aaronnote:api:notes:wanted": async () => {
    const notes = await scanRoamNotes();
    return wantedPages(notes);
  },
  "aaronnote:api:notes:roam-sync": (reload) => roamSyncPayload(reload === true),
  "aaronnote:api:notes:roam-sync-full": () => roamSyncFullPayload(),
  "aaronnote:api:notes:templates": (force) => templatesPayload(force === true),
  "aaronnote:api:notes:snippets": () => snippetsPayload(true),
  "aaronnote:api:latex:defaults": (body) => latexExportDefaults(body || {}),
  "aaronnote:api:latex:agent-status": () => latexExportAgentStatus(),
  "aaronnote:api:latex:set-agent": (body) => setLatexExportAgent(body || {}),
  "aaronnote:api:latex:templates": () => listLatexTemplates(),
  "aaronnote:api:latex:choose-output-path": (body) => chooseLatexOutputPath(body || {}),
  "aaronnote:api:latex:export": (body) => exportLatex({
    ...(body || {}),
    // Stream export phase/agent progress to connected pages via SSE.
    onProgress: (text) => broadcast("command", { command: "latex-export-progress", text: String(text || "") }),
  }),
  "aaronnote:api:notes:meta-add": (body) => updateCurrentNoteMeta(body || {}, "add"),

  "aaronnote:api:roam-tools:rename-tag": (body) => renameRoamTag(body || {}),
  "aaronnote:api:roam-tools:delete-tag": (body) => deleteRoamTag(body || {}),
  "aaronnote:api:roam-tools:tag-overlap": () => roamTagOverlapReport(),
  "aaronnote:api:roam-tools:rewrite-path-refs": (body) => rewriteMarkdownPathReferences(body || {}),

  "aaronnote:api:assets:upload": (body) => storeAsset(body || {}),
  "aaronnote:api:assets:store-from-path": (body) => storeAssetFromPath(body || {}),
  "aaronnote:api:assets:render-tikz": (body) => renderTikzAsset(body || {}),
  "aaronnote:api:assets:scan-orphans": async () => ({ type: "unused-assets", assets: await scanUnusedAssets(), root: noteRoot }),
  "aaronnote:api:assets:trash-orphans": (files) => trashUnusedAssets({ files }),
  "aaronnote:api:clipboard:read": (body) => readSystemClipboard(body || {}),

  "aaronnote:api:session:recent": async () => ({ type: "recent", recent: await readRecentNotes() }),
  "aaronnote:api:session:touch-recent": async (file, openedAt) => ({
    type: "recent",
    recent: await touchRecentNote(String(file || ""), Number(openedAt) || Date.now()),
  }),
  "aaronnote:api:session:positions": async () => ({ type: "positions", positions: await readCursorPositions() }),
  "aaronnote:api:session:save-position": async (position) => ({ type: "positions", positions: await touchCursorPosition(position || {}) }),

  "aaronnote:api:fs:rename": (body) => renameManagedPath(body || {}),
  "aaronnote:api:fs:move": (body) => moveManagedPath(body || {}),
  "aaronnote:api:fs:duplicate": (body) => duplicateManagedFile(body || {}),
  "aaronnote:api:fs:trash": (body) => trashManagedPath(body || {}),
  "aaronnote:api:meta:add": (body) => updateCurrentNoteMeta(body || {}, "add"),
  "aaronnote:api:meta:remove": (body) => updateCurrentNoteMeta(body || {}, "remove"),
  "aaronnote:api:meta:tag": (body) => updateCurrentNoteMeta(body || {}, "tag"),
  "aaronnote:api:meta:hide-roam": (body) => updateCurrentNoteMeta(body || {}, "hide-roam"),
  "aaronnote:api:meta:activate-roam": (body) => updateCurrentNoteMeta(body || {}, "activate-roam"),

  "aaronnote:api:copilot:request": (action, body) => handleCopilotRequest(String(action || ""), body || {}),

  "aaronnote:api:prose-check:run": (body) => runExternalProseChecks(body || {}),
  "aaronnote:api:prose-check:accept-word": (word) => acceptProseWord(word),
  "aaronnote:api:ime:vim-mode": (body) => ime.vimMode(String(body?.mode || "")),
  "aaronnote:api:shell:show-in-folder": (file) => showInFolder(file),
  "aaronnote:api:shell:open-path": (file) => openPath(file),
  "aaronnote:api:shell:open-directory": (body) => openDirectory(body),
  "aaronnote:api:shell:open-directory-in-kitty": () => ({ ok: false, message: "Kitty integration is not available in the Emacs web host yet" }),
  "aaronnote:api:shell:show-attachment-menu": (file) => openPath(file),
  "aaronnote:api:shell:show-editor-context-menu": () => ({ ok: true }),
  "aaronnote:api:emacs:open": (body) => apiOpenInEmacs(body?.file ?? body, body?.line, body?.col, body?.tag),
  "aaronnote:api:emacs:current-file": (file) => apiCurrentFile(file),
  "aaronnote:api:emacs:key": (key) => apiEmacsKey(key),
  "aaronnote:api:emacs:system-open": (target) => apiSystemOpen(target),
  "aaronnote:api:config:katex-macros": () => katexMacrosPayload(),
};

// Read + parse the global KaTeX macro folder on every request (few small files),
// so editing macros only needs a browser refresh to take effect.
function katexMacrosPayload() {
  const { macros, errors } = loadKatexMacros(katexMacrosDir);
  return { type: "katex-macros", dir: katexMacrosDir, macros, errors };
}

async function readSystemClipboard(body) {
  const file = String(body.file || "");
  let tempDir = "";
  try {
    tempDir = await runtimeMkdtemp("clipboard", file || "clipboard.png");
    const target = join(tempDir, "clipboard.png");
    await execFileAsync("pngpaste", [target]);
    if (await isFile(target)) {
      const asset = await storeAssetFromPath({
        file,
        path: target,
        name: "clipboard.png",
        type: "image/png",
      });
      return { kind: "asset", asset };
    }
  } catch (_) {
    // No image on the clipboard, pngpaste unavailable, or asset storage failed.
  } finally {
    if (tempDir) {
      try { await rm(tempDir, { recursive: true, force: true }); } catch {}
    }
  }

  try {
    const { stdout } = await execFileAsync("pbpaste");
    return stdout ? { kind: "text", text: stdout } : { kind: "empty" };
  } catch (_) {
    return { kind: "empty" };
  }
}

async function callApi(channel, args = []) {
  const handler = apiHandlers[channel];
  if (!handler) throw Object.assign(new Error(`Unknown API channel: ${channel}`), { statusCode: 404 });
  return await handler(...(Array.isArray(args) ? args : []));
}

function adapterScript(origin) {
  return `<script>
(function() {
  var BASE = ${JSON.stringify(origin)};
  window.__aaronnoteNotesRoot = ${JSON.stringify(noteRoot)};
  function call(channel, args) {
    return fetch(BASE + "/api", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({channel: channel, args: args || []})
    }).then(function(res) { return res.json(); });
  }
  function callKeepalive(channel, args) {
    try {
      fetch(BASE + "/api", {
        method: "POST",
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({channel: channel, args: args || []}),
        keepalive: true
      }).catch(function() {});
    } catch (_) {}
  }
  function assetProxy(raw) {
    return BASE + "/aaronnote-asset?url=" + encodeURIComponent(String(raw || ""));
  }
  function noteAssetProxy(raw) {
    var url = BASE + "/note-asset?src=" + encodeURIComponent(String(raw || ""));
    var base = currentFile();
    if (base) url += "&base=" + encodeURIComponent(base);
    return url;
  }
  function proxiedUrl(raw) {
    var value = String(raw || "");
    if (!value) return value;
    if (value.indexOf("aaronnote-asset:") === 0) return assetProxy(value);
    if (value.indexOf("file:") === 0) return noteAssetProxy(value);
    return value;
  }
  function installUrlPropertyProxy(proto, prop) {
    if (!proto) return;
    var desc = Object.getOwnPropertyDescriptor(proto, prop);
    if (!desc || typeof desc.set !== "function" || typeof desc.get !== "function") return;
    try {
      Object.defineProperty(proto, prop, {
        configurable: true,
        enumerable: desc.enumerable,
        get: function() { return desc.get.call(this); },
        set: function(value) { desc.set.call(this, proxiedUrl(value)); }
      });
    } catch (_) {}
  }
  function currentFile() {
    try { return window.AaronnoteCurrentFile && window.AaronnoteCurrentFile() || ""; }
    catch (_) { return ""; }
  }
  installUrlPropertyProxy(window.HTMLImageElement && HTMLImageElement.prototype, "src");
  installUrlPropertyProxy(window.HTMLIFrameElement && HTMLIFrameElement.prototype, "src");
  installUrlPropertyProxy(window.HTMLScriptElement && HTMLScriptElement.prototype, "src");
  installUrlPropertyProxy(window.HTMLMediaElement && HTMLMediaElement.prototype, "src");
  installUrlPropertyProxy(window.HTMLSourceElement && HTMLSourceElement.prototype, "src");
  installUrlPropertyProxy(window.HTMLLinkElement && HTMLLinkElement.prototype, "href");
  var originalSetAttribute = Element.prototype.setAttribute;
  Element.prototype.setAttribute = function(name, value) {
    var key = String(name || "").toLowerCase();
    return originalSetAttribute.call(this, name, key === "src" || key === "href" ? proxiedUrl(value) : value);
  };
  var eventSource = new EventSource(BASE + "/events");
  eventSource.addEventListener("command", function(event) {
    try {
      window.dispatchEvent(new CustomEvent("aaronnote:command", {detail: JSON.parse(event.data)}));
    } catch (err) {
      console.error("[aaronnote-host] command event failed", err);
    }
  });
  eventSource.addEventListener("open-file", function(event) {
    try {
      window.dispatchEvent(new CustomEvent("aaronnote:open-file", {detail: JSON.parse(event.data)}));
    } catch (err) {
      console.error("[aaronnote-host] open-file event failed", err);
    }
  });
  var assetResolver = function(source) {
    var raw = String(source || "").trim();
    if (!raw || /^(?:data:|https?:|blob:|#)/i.test(raw)) return raw;
    if (raw.indexOf("aaronnote-asset:") === 0 || raw.indexOf("file:") === 0) return proxiedUrl(raw);
    var url = new URL("aaronnote-asset://media");
    url.searchParams.set("file", raw);
    var base = currentFile();
    if (base) url.searchParams.set("base", base);
    return assetProxy(url.toString());
  };
  Object.defineProperty(window, "AaronnoteResolveAssetUrl", {
    configurable: true,
    get: function() { return assetResolver; },
    set: function(next) {
      if (typeof next !== "function") return;
      assetResolver = function(source) { return proxiedUrl(next(source)); };
    }
  });
  var originalFetch = window.fetch.bind(window);
  window.fetch = function(input, init) {
    if (typeof input === "string" && input.indexOf("aaronnote-asset:") === 0) {
      return originalFetch(proxiedUrl(input), init);
    }
    if (typeof input === "string" && input.indexOf("file:") === 0) {
      return originalFetch(proxiedUrl(input), init);
    }
    if (input instanceof Request && input.url.indexOf("aaronnote-asset:") === 0) {
      return originalFetch(proxiedUrl(input.url), init);
    }
    if (input instanceof Request && input.url.indexOf("file:") === 0) {
      return originalFetch(proxiedUrl(input.url), init);
    }
    return originalFetch(input, init);
  };
  window.AaronnoteDesktop = {
    chooseNotePath: function() { return Promise.resolve(""); },
    trashNote: function(file) { return call("aaronnote:api:notes:delete", [String(file || "")]); },
    exportPdf: function() { return Promise.resolve({ok: false, canceled: true, message: "PDF export is not available in the Emacs web host yet"}); },
    ready: function() {},
    onOpenFile: function(handler) {
      if (typeof handler !== "function") return function() {};
      var listener = function(event) { handler(String(event.detail && event.detail.file || "")); };
      window.addEventListener("aaronnote:open-file", listener);
      return function() { window.removeEventListener("aaronnote:open-file", listener); };
    }
  };
  window.aaronnoteApi = {
    notes: {
      bootstrap: function(file) { return call("aaronnote:api:notes:bootstrap", [String(file || "")]); },
      open: function(file) { return call("aaronnote:api:notes:open", [String(file || "")]); },
      list: function(force) { return call("aaronnote:api:notes:list", [force === true]); },
      save: function(body) { return call("aaronnote:api:notes:save", [body || {}]); },
      saveKeepalive: function(body) { callKeepalive("aaronnote:api:notes:save", [body || {}]); },
      createNode: function(draft) { return call("aaronnote:api:notes:create-node", [draft || {}]); },
      deleteNode: function(file) { return call("aaronnote:api:notes:delete-node", [String(file || "")]); },
      deleteNote: function(file) { return call("aaronnote:api:notes:delete", [String(file || "")]); },
      createFolder: function(path) { return call("aaronnote:api:notes:create-folder", [String(path || "")]); },
      pathSuggestions: function(file, prefix) {
        return call("aaronnote:api:notes:path-suggestions", [{ file: String(file || ""), prefix: String(prefix || "./") }]);
      },
      roamSync: function(reload) { return call("aaronnote:api:notes:roam-sync", [reload === true]); },
      roamSyncFull: function() { return call("aaronnote:api:notes:roam-sync-full", []); },
      templates: function(force) { return call("aaronnote:api:notes:templates", [force === true]); },
      snippets: function() { return call("aaronnote:api:notes:snippets", []); },
      metaAdd: function(body) { return call("aaronnote:api:notes:meta-add", [body || {}]); },
      notesIndex: function() { return call("aaronnote:api:notes:index", []); },
      todos: function(file) { return call("aaronnote:api:notes:todos", [{ file: String(file || "") }]); },
      updateTodo: function(body) { return call("aaronnote:api:notes:update-todo", [body || {}]); }
    },
    completions: {
      tags: function(prefix) { return call("aaronnote:api:completions:tags", [{ prefix: String(prefix || "") }]); },
      roam: function(prefix) { return call("aaronnote:api:completions:roam", [{ prefix: String(prefix || "") }]); },
    },
    noteCode: {
      readRegion: function(body) { return call("aaronnote:api:note-code:read-region", [body || {}]); }
    },
    latex: {
      defaults: function(body) { return call("aaronnote:api:latex:defaults", [body || {}]); },
      agentStatus: function() { return call("aaronnote:api:latex:agent-status", []); },
      setAgent: function(body) { return call("aaronnote:api:latex:set-agent", [body || {}]); },
      templates: function() { return call("aaronnote:api:latex:templates", []); },
      chooseOutputPath: function(body) { return call("aaronnote:api:latex:choose-output-path", [body || {}]); },
      export: function(body) { return call("aaronnote:api:latex:export", [body || {}]); }
    },
    roamTools: {
      renameTag: function(body) { return call("aaronnote:api:roam-tools:rename-tag", [body || {}]); },
      deleteTag: function(body) { return call("aaronnote:api:roam-tools:delete-tag", [body || {}]); },
      tagOverlap: function() { return call("aaronnote:api:roam-tools:tag-overlap", []); },
      rewritePathRefs: function(body) { return call("aaronnote:api:roam-tools:rewrite-path-refs", [body || {}]); }
    },
    assets: {
      upload: function(body) { return call("aaronnote:api:assets:upload", [body || {}]); },
      storeFromPath: function(body) { return call("aaronnote:api:assets:store-from-path", [body || {}]); },
      renderTikz: function(body) { return call("aaronnote:api:assets:render-tikz", [body || {}]); },
      scanOrphans: function() { return call("aaronnote:api:assets:scan-orphans", []); },
      trashOrphans: function(files) { return call("aaronnote:api:assets:trash-orphans", [files || []]); }
    },
    clipboard: {
      read: function(body) { return call("aaronnote:api:clipboard:read", [body || {}]); }
    },
    session: {
      getRecent: function() { return call("aaronnote:api:session:recent", []); },
      touchRecent: function(file, openedAt) { return call("aaronnote:api:session:touch-recent", [String(file || ""), Number(openedAt) || Date.now()]); },
      getPositions: function() { return call("aaronnote:api:session:positions", []); },
      savePosition: function(position) { return call("aaronnote:api:session:save-position", [position || {}]); }
    },
    fs: {
      rename: function(body) { return call("aaronnote:api:fs:rename", [body || {}]); },
      move: function(body) { return call("aaronnote:api:fs:move", [body || {}]); },
      duplicate: function(body) { return call("aaronnote:api:fs:duplicate", [body || {}]); },
      trash: function(body) { return call("aaronnote:api:fs:trash", [body || {}]); }
    },
    meta: {
      add: function(body) { return call("aaronnote:api:meta:add", [body || {}]); },
      remove: function(body) { return call("aaronnote:api:meta:remove", [body || {}]); },
      tag: function(body) { return call("aaronnote:api:meta:tag", [body || {}]); },
      hideRoam: function(body) { return call("aaronnote:api:meta:hide-roam", [body || {}]); },
      activateRoam: function(body) { return call("aaronnote:api:meta:activate-roam", [body || {}]); }
    },
    emacs: {
      open: function(body) { return call("aaronnote:api:emacs:open", [body || {}]); },
      currentFile: function(file) {
        return call("aaronnote:api:emacs:current-file", [
          file && typeof file === "object" ? file : String(file || "")
        ]);
      },
      key: function(k) { return call("aaronnote:api:emacs:key", [String(k || "")]); },
      systemOpen: function(target, base) {
        return call("aaronnote:api:emacs:system-open", [
          base ? {target: String(target || ""), base: String(base || "")} : String(target || "")
        ]);
      }
    },
    shell: {
      showInFolder: function(file) { return call("aaronnote:api:shell:show-in-folder", [String(file || "")]); },
      openPath: function(file) { return call("aaronnote:api:shell:open-path", [String(file || "")]); },
      openDirectory: function(path, base) { return call("aaronnote:api:shell:open-directory", [{path: String(path || ""), base: String(base || "")}]); },
      openDirectoryInKitty: function(path, base) { return call("aaronnote:api:shell:open-directory-in-kitty", [{path: String(path || ""), base: String(base || "")}]); },
      showAttachmentMenu: function(file, base, options) { return call("aaronnote:api:shell:show-attachment-menu", [String(file || ""), String(base || ""), options || {}]); },
      showEditorContextMenu: function(options) { return call("aaronnote:api:shell:show-editor-context-menu", [options || {}]); }
    },
    proseCheck: {
      run: function(body) { return call("aaronnote:api:prose-check:run", [body || {}]); },
      acceptWord: function(word) { return call("aaronnote:api:prose-check:accept-word", [String(word || "")]); },
      browserSpellcheck: function(words) {
        return Array.isArray(words) ? words.map(function(word) {
          return {word: String(word || ""), misspelled: false, suggestions: []};
        }) : [];
      }
    },
    copilot: {
      request: function(action, body) { return call("aaronnote:api:copilot:request", [String(action || ""), body || {}]); }
    },
    ime: {
      vimMode: function(mode) { return call("aaronnote:api:ime:vim-mode", [{ mode: String(mode || "") }]); }
    },
    config: {
      katexMacros: function() { return call("aaronnote:api:config:katex-macros", []); }
    }
  };
}());
</script>`;
}

function cleanAssetSource(source) {
  let value = String(source || "").trim();
  if (value.startsWith("<") && value.endsWith(">")) value = value.slice(1, -1);
  if (/^file:/i.test(value)) {
    try {
      return fileURLToPath(value);
    } catch {
      return "";
    }
  }
  return value.split(/[?#]/, 1)[0] || "";
}

function visualFrameBaseStyle() {
  return [
    "html,body{margin:0;width:100%;height:100%;overflow:hidden;background:#fff;color:#1f2937;",
    "font:13px/1.45 system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}",
    "body{position:relative}",
    "iframe{position:absolute;inset:0;width:100%;height:100%;border:0;background:#fff}",
    ".status{position:absolute;inset:0;z-index:2;box-sizing:border-box;display:grid;place-items:center;padding:18px;text-align:center;color:#6b7280;background:#fff}",
    ".status.error{color:#9f1239;background:#fff7f7}",
  ].join("");
}

function htmlEscape(value) {
  return String(value || "").replace(/[&<>"]/g, (ch) => ({
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    "\"": "&quot;",
  }[ch]));
}

function visualFrameErrorHTML(message) {
  return `<!doctype html>
<html>
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><style>${visualFrameBaseStyle()}</style></head>
<body><div class="status error">${htmlEscape(message || "Visual attachment failed")}</div></body>
</html>`;
}

function scriptString(value) {
  return JSON.stringify(String(value ?? ""))
    .replace(/</g, "\\u003c")
    .replace(/\u2028/g, "\\u2028")
    .replace(/\u2029/g, "\\u2029");
}

function drawioFrameHTML(xml) {
  return `<!doctype html>
<html>
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><style>${visualFrameBaseStyle()}</style></head>
<body>
<iframe id="drawio-frame" title="draw.io diagram" allow="fullscreen; clipboard-read; clipboard-write" src="https://embed.diagrams.net/?embed=1&proto=json&spin=1&ui=min&libraries=1&noSaveBtn=1&noExitBtn=1"></iframe>
<script>
(function () {
  var xml = ${scriptString(xml)};
  var frame = document.getElementById("drawio-frame");
  function sendLoad() {
    frame.contentWindow.postMessage(JSON.stringify({
      action: "load",
      autosave: 0,
      modified: 0,
      title: "draw.io diagram",
      xml: xml
    }), "*");
  }
  window.addEventListener("message", function (event) {
    var data = event.data;
    try {
      if (typeof data === "string" && data.charAt(0) === "{") data = JSON.parse(data);
    } catch (err) {}
    if (data === "ready" || data && data.event === "init") sendLoad();
  });
}());
</script>
</body>
</html>`;
}

function resolveAssetFile(rawUrl) {
  const parsed = new URL(String(rawUrl || ""));
  if (parsed.protocol !== "aaronnote-asset:") throw new Error(`Unsupported asset URL: ${rawUrl}`);
  const host = parsed.hostname;
  if (host === "media") {
    return resolveMediaFile(parsed.searchParams.get("file"), parsed.searchParams.get("base"));
  }
  if (host === "font") {
    const requested = decodeURIComponent(parsed.pathname.replace(/^\/+/, ""));
    if (requested && requested !== "FZLiuGongQuanKaiShuJF.ttf") throw new Error(`Unknown font: ${requested}`);
    const fontFile = liuGongQuanFontCandidates.map((file) => resolve(String(file))).find((file) => existsSync(file));
    if (!fontFile) throw new Error("FZLiuGongQuanKaiShuJF font not found");
    return fontFile;
  }
  if (host === "kinds") {
    const root = resolve(workspaceRoot, "kinds");
    const requested = decodeURIComponent(parsed.pathname.replace(/^\/+/, ""));
    const file = resolve(root, requested);
    if (!isWithin(root, file)) throw new Error(`Kind asset is outside kinds root: ${file}`);
    return file;
  }
  if (host === "roam-tools") {
    const name = decodeURIComponent(parsed.pathname.replace(/^\/+/, ""));
    if (name !== "knowledge.js" && name !== "graph.js") throw new Error(`Unknown roam tool: ${name}`);
    return resolve(publishJsDir, name);
  }
  throw new Error(`Unknown Aaronnote asset host: ${host}`);
}

function visualFrameSourceFile(src) {
  const raw = String(src || "");
  if (!raw) throw new Error("Missing visual attachment source");
  const parsed = new URL(raw);
  if (parsed.protocol !== "aaronnote-asset:" || parsed.hostname !== "media") {
    throw new Error(`Unsupported visual attachment source: ${raw}`);
  }
  return resolveAssetFile(raw);
}

async function serveVisualFrame(rawUrl, res) {
  try {
    const parsed = new URL(String(rawUrl || ""));
    const kind = decodeURIComponent(parsed.pathname.replace(/^\/+/, ""));
    const file = visualFrameSourceFile(parsed.searchParams.get("src"));
    if (kind === "drawio") {
      sendHtmlNoStore(res, drawioFrameHTML(await readFile(file, "utf8")));
      return;
    }
    throw new Error(`Unknown visual attachment kind: ${kind}`);
  } catch (err) {
    sendHtmlNoStore(res, visualFrameErrorHTML(err instanceof Error ? err.message : String(err)));
  }
}

async function serveAaronnoteAsset(url, res) {
  const raw = url.searchParams.get("url") || "";
  let parsedRaw = null;
  try {
    parsedRaw = new URL(raw);
  } catch {}
  if (parsedRaw?.hostname === "visual-frame") {
    await serveVisualFrame(raw, res);
    return;
  }
  const file = resolveAssetFile(raw);
  if (!file || !(await isFile(file))) {
    sendText(res, 404, "Asset not found");
    return;
  }
  const data = await readFile(file);
  res.writeHead(200, {
    "Content-Type": mimeFor(file),
    "Cache-Control": "no-cache",
  });
  res.end(data);
}

async function serveNoteAsset(url, res) {
  const source = cleanAssetSource(url.searchParams.get("src"));
  const base = url.searchParams.get("base") || "";
  const assetUrl = new URL("aaronnote-asset://media");
  assetUrl.searchParams.set("file", source);
  if (base) assetUrl.searchParams.set("base", base);
  const file = resolveAssetFile(assetUrl.toString());
  if (!file || !(await isFile(file))) {
    sendText(res, 404, "Asset not found");
    return;
  }
  const data = await readFile(file);
  res.writeHead(200, {
    "Content-Type": mimeFor(file),
    "Cache-Control": "no-cache",
  });
  res.end(data);
}

async function serveStatic(urlPath, res, origin) {
  const requested = decodeURIComponent(urlPath).replace(/^\/+/, "") || "index.html";
  const file = resolve(webDir, requested);
  if (!isWithin(webDir, file) || !(await isFile(file))) {
    sendText(res, 404, "Not found");
    return;
  }
  const data = await readFile(file);
  if (file.endsWith(".js")) {
    res.writeHead(200, {
      "Content-Type": "application/javascript; charset=utf-8",
      "Cache-Control": "public, max-age=86400",
    });
    res.end(transformJavaScript(data.toString("utf8")));
    return;
  }
  if (file.endsWith("index.html")) {
    const html = data.toString("utf8").replace("</head>", `${adapterScript(origin)}\n</head>`);
    res.writeHead(200, {
      "Content-Type": "text/html; charset=utf-8",
      "Cache-Control": "no-cache",
    });
    res.end(html);
    return;
  }
  res.writeHead(200, {
    "Content-Type": mimeFor(file),
    "Cache-Control": "public, max-age=86400",
  });
  res.end(data);
}

const server = createServer(async (req, res) => {
  try {
    const url = new URL(req.url, "http://localhost");
    const origin = `http://${bindHost}:${server.address()?.port}`;

    if (url.pathname === "/events") {
      res.writeHead(200, {
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        "Connection": "keep-alive",
      });
      res.write("retry: 2000\n\n");
      eventClients.add(res);
      req.on("close", () => eventClients.delete(res));
      return;
    }

    if (url.pathname === "/api/clipboard") {
      if (req.method === "GET") {
        try {
          const { stdout } = await execFileAsync("pbpaste");
          res.writeHead(200, { "Content-Type": "text/plain; charset=utf-8" });
          res.end(stdout);
        } catch (_) {
          res.writeHead(200, { "Content-Type": "text/plain; charset=utf-8" });
          res.end("");
        }
        return;
      }
      if (req.method === "POST") {
        try {
          const text = await readText(req);
          await new Promise((resolve) => {
            const proc = spawn("pbcopy");
            proc.stdin.write(text, "utf8");
            proc.stdin.end();
            proc.on("close", resolve);
            proc.on("error", resolve);
          });
        } catch (_) {}
        res.writeHead(204);
        res.end();
        return;
      }
    }

    if (url.pathname === "/api" && req.method === "POST") {
      const body = await readJson(req);
      const result = await callApi(String(body.channel || ""), body.args);
      sendJson(res, 200, result ?? { ok: true });
      return;
    }

    if (url.pathname === "/emacs/command" && req.method === "POST") {
      const body = await readJson(req, 1024 * 1024);
      if (body.type === "command" || body.command) {
        const detail = { ...(body.detail && typeof body.detail === "object" ? body.detail : {}), command: String(body.command || "") };
        if (body.client) detail.client = String(body.client);
        broadcast("command", detail);
        sendJson(res, 200, { ok: true });
        return;
      }
      if (body.type === "open" || body.file) {
        const file = resolveShellPath(body.file);
        broadcast("open-file", { file });
        sendJson(res, 200, { ok: true, file });
        return;
      }
      sendJson(res, 400, { ok: false, message: "Unknown command type" });
      return;
    }

    if (url.pathname === "/emacs/event" && req.method === "POST") {
      const body = await readJson(req, 1024 * 1024);
      if (body.type === "open" || body.type === "goto") {
        sendJson(res, 200, await apiOpenInEmacs(body.file, body.line, body.col, body.tag));
        return;
      }
      if (body.type === "current-file") {
        sendJson(res, 200, await apiCurrentFile(body.file));
        return;
      }
      sendJson(res, 400, { ok: false, message: "Unknown event type" });
      return;
    }

    if (url.pathname === "/aaronnote-asset") {
      await serveAaronnoteAsset(url, res);
      return;
    }

    if (url.pathname === "/note-asset") {
      await serveNoteAsset(url, res);
      return;
    }

    // Serve roam-pub JS files (D3, knowledge.js, graph.js) via plain HTTP
    if (url.pathname.startsWith("/roam-pub/")) {
      const name = url.pathname.slice("/roam-pub/".length);
      let filePath;
      if (name === "d3.min.js") filePath = resolve(runtimeRoot, "node_modules/d3/dist/d3.min.js");
      else if (name === "knowledge.js" || name === "graph.js") filePath = resolve(publishJsDir, name);
      else { sendText(res, 404, "Not found"); return; }
      if (!(await isFile(filePath))) { sendText(res, 404, "Not found"); return; }
      const data = await readFile(filePath);
      res.writeHead(200, {
        "Content-Type": "application/javascript; charset=utf-8",
        "Cache-Control": "no-cache",
      });
      res.end(data);
      return;
    }

    if (url.pathname === "/graph") {
      const notes = await scanRoamNotes();
      const raw = graphPayload(notes);
      // Build SITE_DATA in the format knowledge.js expects:
      // { notes: [{ key, title, link, path, tags, aliases, refs, backlinks, groupKey, groupLabel }] }
      const backlinksMap = {};
      for (const edge of raw.edges ?? []) {
        if (!backlinksMap[edge.target]) backlinksMap[edge.target] = [];
        backlinksMap[edge.target].push(edge.source);
      }
      const siteData = {
        notes: raw.nodes.map((n) => ({
          key: n.key,
          id: n.id || n.key,
          title: n.title,
          link: n.link || n.path,
          path: n.path,
          groupKey: n.groupKey || "Root",
          groupLabel: n.groupLabel || n.groupKey || "Root",
          tags: n.tags ?? [],
          aliases: n.aliases ?? [],
          refs: (raw.edges ?? []).filter((e) => e.source === n.key).map((e) => e.target),
          backlinks: backlinksMap[n.key] ?? [],
        })),
      };
      sendHtmlNoStore(res, `<!doctype html>
<html>
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
${adapterScript(origin)}
<style>
*{box-sizing:border-box}
html,body{margin:0;padding:0;width:100vw;height:100vh;overflow:hidden;display:flex}
#graph-container{flex:1;width:0;height:100vh;position:relative}
#graph-focus{width:280px;min-width:280px;height:100vh;overflow-y:auto;border-left:1px solid #d8d0c2;background:color-mix(in srgb,#fffaf0,white 12%)}
#graph-focus:empty,#graph-focus.empty{display:none}
</style>
<script>var SITE_DATA=${JSON.stringify(siteData).replace(/</g,"\\u003c")};</script>
<script src="${origin}/roam-pub/knowledge.js"></script>
<script>window.__GRAPH_NO_AUTO_INIT__=true;</script>
</head>
<body>
<div id="graph-container" data-graph-toolbar="true"></div>
<div id="graph-focus" class="graph-focus"></div>
<script src="${origin}/roam-pub/d3.min.js"></script>
<script src="${origin}/roam-pub/graph.js"></script>
<script>
document.addEventListener("DOMContentLoaded", function () {
  var root = window.__aaronnoteNotesRoot || "";
  window.initKnowledgeGraph({
    onNoteOpen: function (note) {
      var path = (note && (note.path || note.link)) || "";
      if (!path) return;
      var abs = path;
      if (root) {
        var r = root;
        while (r.length && r.charAt(r.length - 1) === "/") r = r.slice(0, -1);
        var p = path;
        while (p.length && p.charAt(0) === "/") p = p.slice(1);
        abs = r + "/" + p;
      }
      var api = window.aaronnoteApi;
      if (api && api.emacs && api.emacs.open) {
        api.emacs.open({ file: abs }).catch(function () {});
      }
    }
  });
});
</script>
</body>
</html>`);
      return;
    }

    if (url.pathname === "/health") {
      sendJson(res, 200, {
        ok: true,
        root: noteRoot,
        web: webDir,
        runtime: runtimeRoot,
        state: stateRoot,
        tmp: tmpRoot,
        snippets: snippetsRoot,
        templates: templatesRoot,
      });
      return;
    }

    await serveStatic(url.pathname, res, origin);
  } catch (err) {
    const status = cleanStatusCode(err);
    if (req.url?.startsWith("/api")) sendJson(res, status, errorPayload(err));
    else sendText(res, status, err instanceof Error ? err.message : String(err));
  }
});

server.on("error", (err) => {
  process.stderr.write(`[aaronnote-web] Failed to start server: ${err.message}\n`);
  process.exit(1);
});
server.listen(bindPort, bindHost, () => {
  const port = server.address().port;
  process.stdout.write(`aaronote-web-host:ready:${port}\n`);
  process.stderr.write(`[aaronnote-web] http://${bindHost}:${port}\n`);
});
