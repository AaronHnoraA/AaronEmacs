import { app, BrowserWindow, Menu, Notification, dialog, ipcMain, shell, protocol, net, globalShortcut, powerMonitor, clipboard } from "electron";
import { execFile, spawn } from "node:child_process";
import { existsSync, statSync } from "node:fs";
import { access, readFile, writeFile } from "node:fs/promises";
import { createServer } from "node:net";
import { basename, dirname, join, relative, resolve } from "node:path";
import { findExecutable, findKittyExecutable, kittyDirectoryCommand } from "./lean-external.mjs";
import { openExternalEditorTarget } from "./external-editor.mjs";
import { shouldOwnShortcut, historyShortcutCommand } from "./shortcuts.mjs";
import { jupyterLabUrl, jupyterLaunchArgs, jupyterSelectorPath, mergeJupyterEnv, parseNulEnv } from "./jupyter.mjs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { homedir } from "node:os";
import { promisify } from "node:util";

import { saveNote } from "../server/lib/save.mjs";
import { commitRoam } from "../server/lib/roam-git.mjs";
import {
  bootstrapNote,
  readNote,
  notesIndexPayload,
  scanNotes,
  graphPayload,
  tagIndexPayload,
  pathSuggestionsForFile,
  syncRoamDb,
  queueRoamDbSync,
  runtimeDebugSnapshot,
  maybeScheduleWeeklyFullSync,
  fileHistory,
  restoreFileFromCommit,
  discardFileChanges,
  roamRepoStatus,
  roamRepoChanges,
  diffRoamFile,
  diffRoamCommit,
  pullRoam,
  pushRoam,
  repoHistory,
  roamNoteRoot,
  renameRoamTag,
  deleteRoamTag,
  roamTagOverlapReport,
  rewriteMarkdownPathReferences,
  getTodos,
  scanSnippets,
  scanTemplates,
} from "../server/lib/index.mjs";
import { configure, markNotesDirty } from "../server/lib/state.mjs";
import { storeAsset, storeAssetFromPath, renderTikzAsset, scanUnusedAssets, trashUnusedAssets } from "../server/lib/assets.mjs";
import {
  createNode,
  createFolder,
  deleteNote,
  renameManagedPath,
  moveManagedPath,
  duplicateManagedFile,
  trashManagedPath,
} from "../server/lib/fs-ops.mjs";
import { updateCurrentNoteMeta } from "../server/lib/meta.mjs";
import { readRecentNotes, touchRecentNote, readCursorPositions, touchCursorPosition } from "../server/lib/session.mjs";
import { scanPlugins, readPluginOverrides, writePluginOverrides } from "../server/lib/plugins.mjs";
import { handleCopilotRequest } from "../server/lib/copilot.mjs";
import { handleRoamLookupRequest } from "../server/lib/roamlookup.mjs";
import { handleLeanRequest, registerLeanPushHandlers, setNotesRoot as setLeanNotesRoot } from "../server/lib/lean.mjs";
import { resolveMediaFile, fileContentType } from "../server/lib/media.mjs";
import { runExternalProseChecks } from "../server/lib/prose-check.mjs";
import { normalizePickedNotePath } from "./path-selection.mjs";
import { attachmentContextMenuTemplate, editorContextMenuTemplate } from "./context-menus.mjs";

const desktopDir = dirname(fileURLToPath(import.meta.url));
const projectDir = resolve(desktopDir, "..");
const noteRoot = process.env.AARONNOTE_ROOT || resolve(projectDir, "..", "..", "..", ".roam");
const workspaceRoot = process.env.AARONNOTE_WORKSPACE_ROOT
  ? resolve(process.env.AARONNOTE_WORKSPACE_ROOT)
  : resolve(noteRoot, "..");
const publishScript = join(workspaceRoot, "bin", "publish-site");
const pdfPublishRoot = join(workspaceRoot, "public", ".export");
const execFileAsync = promisify(execFile);
const pluginRoot = process.env.AARONNOTE_PLUGIN_ROOT
  ? resolve(process.env.AARONNOTE_PLUGIN_ROOT)
  : join(projectDir, "plugin");
const isPackaged = app.isPackaged;
const staticDir = isPackaged
  ? join(app.getAppPath(), "dist", "aaronnote")
  : join(projectDir, "dist", "aaronnote");
const devViteUrl = process.env.AARONNOTE_DEV_VITE_URL || "";
const publishJsDir = isPackaged
  ? join(process.resourcesPath, "js")
  : join(projectDir, "js");
const liuGongQuanFontCandidates = [
  process.env.AARONNOTE_LIUGONGQUAN_FONT,
  join(homedir(), "Library", "Fonts", "方正柳公权楷书 简繁.TTF"),
  join(homedir(), "Library", "Fonts", "FZLiuGongQuanKaiShuJF.ttf"),
].filter(Boolean);

let mainWindow = null;
let debugPanel = null;
let pendingOpenFile = process.argv.slice(1).find((arg) => /\.(?:md|markdown)$/i.test(arg)) || "";
let allowQuit = false;
let leanMenuStatus = { message: "Not started", kind: "Inactive", busy: false };
let leanMenuLog = [];
let leanMenuUpdateTimer = null;
const jupyterIdleMs = Math.max(10 * 60_000, Number(process.env.AARONNOTE_JUPYTER_IDLE_MS) || 45 * 60_000);
let jupyterSession = null;
let jupyterIdleTimer = null;
let jupyterShellEnvPromise = null;

protocol.registerSchemesAsPrivileged([{
  scheme: "aaronnote-asset",
  privileges: {
    secure: true,
    standard: true,
    supportFetchAPI: true,
    stream: true,
    bypassCSP: true,
    corsEnabled: true,
  },
}]);

const hasSingleInstanceLock = app.requestSingleInstanceLock();
if (!hasSingleInstanceLock) app.exit(0);


const ZOOM_STEP = 0.5;
const DEFAULT_ZOOM_LEVEL = 2;
const ZOOM_MIN = -3;
const ZOOM_MAX = 3;
let desiredZoomLevel = DEFAULT_ZOOM_LEVEL;

function clampZoomLevel(level) {
  return Math.max(ZOOM_MIN, Math.min(ZOOM_MAX, Number(level) || 0));
}

function appWindows() {
  return BrowserWindow.getAllWindows().filter((win) => win.aaronnoteAppWindow && !win.isDestroyed());
}

function applyZoom(win) {
  if (!win || win.isDestroyed()) return;
  if (win.webContents.isDestroyed()) return;
  win.webContents.setZoomLevel(desiredZoomLevel);
}

function scheduleApplyZoom(win) {
  applyZoom(win);
  setTimeout(() => applyZoom(win), 0);
  setTimeout(() => applyZoom(win), 80);
}

function enablePinchZoom(win) {
  if (!win || win.isDestroyed()) return;
  if (win.webContents.isDestroyed()) return;
  // Best-effort: the window may tear down between the guard above and this call.
  void win.webContents.setVisualZoomLevelLimits(1, 3).catch(() => {});
}

function applyZoomToAllWindows() {
  for (const win of appWindows()) scheduleApplyZoom(win);
}

function setDesiredZoomLevel(level) {
  desiredZoomLevel = clampZoomLevel(level);
  applyZoomToAllWindows();
}

function stepDesiredZoomLevel(delta) {
  setDesiredZoomLevel(desiredZoomLevel + delta);
}

function handleZoomShortcut(win, input) {
  if (!input.meta || input.alt || input.control) return false;
  if (input.type !== "keyDown") return false;
  const key = input.key;
  const isPlus = key === "=" || key === "+";
  const isMinus = key === "-" || key === "_";
  const isZero = key === "0";
  if (!isPlus && !isMinus && !isZero) return false;
  if (isZero) {
    setDesiredZoomLevel(DEFAULT_ZOOM_LEVEL);
  } else {
    stepDesiredZoomLevel(isPlus ? ZOOM_STEP : -ZOOM_STEP);
  }
  return true;
}

function handleFullscreenShortcut(win, input) {
  if (input.type !== "keyDown") return false;
  if (input.alt || input.control || input.meta || input.shift) return false;
  if (input.key !== "F11") return false;
  win.setFullScreen(!win.isFullScreen());
  return true;
}

function inside(child, parent) {
  const rel = relative(parent, child);
  return rel === "" || (!rel.startsWith("..") && !rel.startsWith("/"));
}

function internalRendererUrl(targetUrl) {
  if (targetUrl.startsWith("aaronnote-asset:")) return true;
  if (rendererAppUrl(targetUrl)) return true;
  return false;
}

function rendererAppUrl(targetUrl) {
  if (devViteUrl && targetUrl.startsWith(devViteUrl)) return true;
  try {
    const url = new URL(targetUrl);
    if (url.protocol !== "file:") return false;
    return inside(fileURLToPath(url), resolve(staticDir));
  } catch {
    return false;
  }
}

function targetAppWindow() {
  const win = BrowserWindow.getFocusedWindow();
  if (win?.aaronnoteAppWindow && !win.isDestroyed()) return win;
  if (mainWindow && !mainWindow.isDestroyed()) return mainWindow;
  return null;
}

function errorPayload(err) {
  return {
    type: "error",
    ok: false,
    message: err instanceof Error ? err.message : String(err),
  };
}

function registerApiHandler(channel, handler) {
  ipcMain.handle(channel, async (_event, ...args) => {
    try {
      if (!debugPanel) return await handler(...args);
      return await debugPanel.trackTask(channel, () => handler(...args));
    } catch (err) {
      return errorPayload(err);
    }
  });
}

async function openDebugPanel() {
  if (!debugPanel) {
    const { createDebugPanel } = await import("./debug-panel.mjs");
    debugPanel = createDebugPanel({
      app,
      BrowserWindow,
      desktopDir,
      ipcMain,
      powerMonitor,
      runtimeSnapshot: runtimeDebugSnapshot,
      appWindows,
      onClose: () => {
        debugPanel = null;
      },
    });
  }
  debugPanel.show();
}

ipcMain.on("aaronnote:renderer-ready", (event) => {
  const win = BrowserWindow.fromWebContents(event.sender);
  if (!win || win.isDestroyed()) return;
  win.aaronnoteRendererReady = true;
  applyZoom(win);
  flushPendingOpenFile(win);
});

function showTaskNotification(title, body) {
  try {
    if (!Notification.isSupported()) return;
    new Notification({ title, body }).show();
  } catch {
    // Desktop notifications are optional; ignore platform/permission failures.
  }
}

async function notesListPayload(force = false) {
  if (force) markNotesDirty();
  return { type: "notes", ...await notesIndexPayload(), root: noteRoot };
}

async function roamSyncPayload(reload = false) {
  if (reload) markNotesDirty();
  const notes = await syncRoamDb();
  const index = await notesIndexPayload(notes);
  return { type: "notes", ...index, root: noteRoot, db: join(noteRoot, "roam.db") };
}

async function roamSyncFullPayload() {
  markNotesDirty();
  const notes = await syncRoamDb(null, { mode: "full" });
  const index = await notesIndexPayload(notes);
  return { type: "notes", ...index, root: noteRoot, db: join(noteRoot, "roam.db") };
}

async function templatesPayload(force = false) {
  return { type: "templates", templates: await scanTemplates({ force }) };
}

async function snippetsPayload(force = false) {
  return { type: "snippets", snippets: await scanSnippets({ force }) };
}

async function pluginsPayload(force = false) {
  return { type: "plugins", plugins: await scanPlugins({ force }), root: pluginRoot };
}

async function fileProtocolResponse(file) {
  const response = await net.fetch(pathToFileURL(file).toString());
  const headers = new Headers(response.headers);
  if (!headers.has("content-type")) headers.set("content-type", fileContentType(file));
  return new Response(response.body, {
    status: response.status,
    statusText: response.statusText,
    headers,
  });
}

function visualFrameHTML(body) {
  return new Response(body, {
    status: 200,
    headers: {
      "content-type": "text/html; charset=utf-8",
      "cache-control": "no-store",
      "access-control-allow-origin": "*",
    },
  });
}

function visualFrameBaseStyle() {
  return [
    "html,body{margin:0;width:100%;height:100%;overflow:hidden;background:#fff;color:#1f2937;",
    "font:13px/1.45 system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}",
    "body{position:relative}",
    "iframe{position:absolute;inset:0;width:100%;height:100%;border:0;background:#fff}",
    ".status{position:absolute;inset:0;z-index:2;box-sizing:border-box;display:grid;place-items:center;padding:18px;text-align:center;color:#6b7280;background:#fff}",
    ".status.error{color:#9f1239;background:#fff7f7}",
    ".status a{color:#1d4ed8}",
  ].join("");
}

function visualFrameErrorHTML(message) {
  return `<!doctype html>
<html>
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><style>${visualFrameBaseStyle()}</style></head>
<body><div class="status error">${String(message || "Visual attachment failed").replace(/[&<>"]/g, (ch) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;" }[ch]))}</div></body>
</html>`;
}

function visualFrameScriptString(value) {
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
  var xml = ${visualFrameScriptString(xml)};
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

function visualFrameSourceFile(src) {
  const raw = String(src || "");
  if (!raw) throw new Error("Missing visual attachment source");
  const url = new URL(raw);
  if (url.protocol !== "aaronnote-asset:" || url.hostname !== "media") {
    throw new Error(`Unsupported visual attachment source: ${raw}`);
  }
  return resolveAssetProtocolFile(raw);
}

async function visualFrameProtocolResponse(requestUrl) {
  try {
    const url = new URL(requestUrl);
    const kind = decodeURIComponent(url.pathname.replace(/^\/+/, ""));
    const file = visualFrameSourceFile(url.searchParams.get("src"));
    if (kind === "drawio") {
      return visualFrameHTML(drawioFrameHTML(await readFile(file, "utf8")));
    }
    throw new Error(`Unknown visual attachment kind: ${kind}`);
  } catch (err) {
    return visualFrameHTML(visualFrameErrorHTML(err instanceof Error ? err.message : String(err)));
  }
}

function notFoundResponse(message = "Not found", status = 404) {
  return new Response(message, {
    status,
    headers: { "content-type": "text/plain; charset=utf-8" },
  });
}

function resolveAssetProtocolFile(requestUrl) {
  const url = new URL(requestUrl);
  const host = url.hostname;
  if (host === "media") {
    return resolveMediaFile(url.searchParams.get("file"), url.searchParams.get("base"));
  }
  if (host === "font") {
    const requested = decodeURIComponent(url.pathname.replace(/^\/+/, ""));
    if (requested && requested !== "FZLiuGongQuanKaiShuJF.ttf") throw new Error(`Unknown font: ${requested}`);
    const fontFile = liuGongQuanFontCandidates
      .map((file) => resolve(String(file)))
      .find((file) => existsSync(file));
    if (!fontFile) throw new Error("FZLiuGongQuanKaiShuJF font not found");
    return fontFile;
  }
  if (host === "kinds") {
    const root = resolve(workspaceRoot, "kinds");
    const requested = decodeURIComponent(url.pathname.replace(/^\/+/, ""));
    const file = resolve(root, requested);
    if (!inside(file, root)) throw new Error(`Kind asset is outside kinds root: ${file}`);
    return file;
  }
  if (host === "roam-tools") {
    const name = decodeURIComponent(url.pathname.replace(/^\/+/, ""));
    if (name !== "knowledge.js" && name !== "graph.js") throw new Error(`Unknown roam tool: ${name}`);
    return resolve(publishJsDir, name);
  }
  throw new Error(`Unknown Aaronnote asset host: ${host}`);
}

function registerAssetProtocol() {
  protocol.handle("aaronnote-asset", async (request) => {
    try {
      const host = new URL(request.url).hostname;
      if (host === "visual-frame") {
        return await visualFrameProtocolResponse(request.url);
      }
      const file = resolveAssetProtocolFile(request.url);
      return await fileProtocolResponse(file);
    } catch (err) {
      console.error("aaronnote-asset failed", request.url, err);
      return notFoundResponse(err instanceof Error ? err.message : String(err));
    }
  });
}

function registerApiIpc() {
  registerApiHandler("aaronnote:api:notes:bootstrap", (file) => bootstrapNote(file || undefined));
  registerApiHandler("aaronnote:api:notes:open", (file) => readNote(file));
  registerApiHandler("aaronnote:api:notes:list", (force) => notesListPayload(force === true));
  registerApiHandler("aaronnote:api:notes:save", (body) => saveNote(body || {}));
  registerApiHandler("aaronnote:api:notes:create-node", (draft) => createNode(draft || {}));
  registerApiHandler("aaronnote:api:notes:delete", (file) => deleteNote({ file }));
  registerApiHandler("aaronnote:api:notes:create-folder", (path) => createFolder({ path }));
  registerApiHandler("aaronnote:api:notes:path-suggestions", async (file) => ({ type: "path-suggestions", paths: await pathSuggestionsForFile(file || "") }));
  registerApiHandler("aaronnote:api:notes:roam-sync", (reload) => roamSyncPayload(reload === true));
  registerApiHandler("aaronnote:api:notes:roam-sync-full", () => roamSyncFullPayload());
  registerApiHandler("aaronnote:api:roam-tools:file-history", async (file) => {
    if (!file) throw Object.assign(new Error("Missing file"), { statusCode: 400 });
    const entries = await fileHistory(roamNoteRoot, file);
    return { type: "file-history", file, entries };
  });
  registerApiHandler("aaronnote:api:roam-tools:restore-file-version", async (body) => {
    const { file, sha } = body || {};
    if (!file || !sha) throw Object.assign(new Error("Missing file or sha"), { statusCode: 400 });
    await restoreFileFromCommit(roamNoteRoot, file, sha);
    markNotesDirty(file);
    queueRoamDbSync(null, [file]);
    const index = await notesIndexPayload();
    return { type: "notes", ...index, root: noteRoot, db: join(noteRoot, "roam.db"), restoredFile: file };
  });
  registerApiHandler("aaronnote:api:roam-tools:discard-file-changes", async (file) => {
    if (!file) throw Object.assign(new Error("Missing file"), { statusCode: 400 });
    const result = await discardFileChanges(roamNoteRoot, file);
    markNotesDirty(result.file || file);
    queueRoamDbSync(null, [result.file || file]);
    const index = await notesIndexPayload();
    return { type: "notes", ...index, root: noteRoot, db: join(noteRoot, "roam.db"), restoredFile: result.file || file, discarded: result.changed !== false };
  });
  registerApiHandler("aaronnote:api:roam-tools:repo-status", async () => {
    const status = await roamRepoStatus(roamNoteRoot);
    return { type: "roam-repo-status", ...status };
  });
  registerApiHandler("aaronnote:api:roam-tools:repo-history", async (limit) => {
    const entries = await repoHistory(roamNoteRoot, typeof limit === "number" ? limit : 30);
    return { type: "roam-repo-history", entries };
  });
  registerApiHandler("aaronnote:api:roam-tools:changes", async () => {
    const changes = await roamRepoChanges(roamNoteRoot);
    return { type: "roam-repo-changes", changes };
  });
  registerApiHandler("aaronnote:api:roam-tools:diff", async (body) => {
    const { file, path, scope, sha } = body || {};
    const target = file || path;
    if (!target) throw Object.assign(new Error("Missing file"), { statusCode: 400 });
    return { type: "roam-repo-diff", ...await diffRoamFile(roamNoteRoot, target, { scope, sha }) };
  });
  registerApiHandler("aaronnote:api:roam-tools:commit-diff", async (sha) => {
    const cleanSha = String(sha || "").trim();
    if (!cleanSha) throw Object.assign(new Error("Missing commit"), { statusCode: 400 });
    return { type: "roam-commit-diff", ...await diffRoamCommit(roamNoteRoot, cleanSha) };
  });
  registerApiHandler("aaronnote:api:roam-tools:pull", async () => {
    const output = await pullRoam(roamNoteRoot);
    markNotesDirty();
    return { type: "roam-pull-done", ok: true, output };
  });
  registerApiHandler("aaronnote:api:roam-tools:push", async () => {
    await pushRoam(roamNoteRoot);
    return { type: "roam-push-done", ok: true };
  });
  registerApiHandler("aaronnote:api:roam-tools:commit", async (message) => {
    const msg = typeof message === "string" && message.trim() ? message.trim() : `roam commit: ${new Date().toISOString()}`;
    const sha = await commitRoam(roamNoteRoot, msg);
    return { type: "roam-commit-done", ok: true, sha };
  });
  registerApiHandler("aaronnote:api:notes:templates", (force) => templatesPayload(force === true));
  registerApiHandler("aaronnote:api:notes:snippets", () => snippetsPayload(true));
  registerApiHandler("aaronnote:api:notes:todos", (file) => getTodos(file || undefined));
  registerApiHandler("aaronnote:api:notes:meta-add", (body) => updateCurrentNoteMeta(body || {}, "add"));
  registerApiHandler("aaronnote:api:roam-tools:rename-tag", (body) => renameRoamTag(body || {}));
  registerApiHandler("aaronnote:api:roam-tools:delete-tag", (body) => deleteRoamTag(body || {}));
  registerApiHandler("aaronnote:api:roam-tools:tag-overlap", () => roamTagOverlapReport());
  registerApiHandler("aaronnote:api:roam-tools:rewrite-path-refs", (body) => rewriteMarkdownPathReferences(body || {}));

  registerApiHandler("aaronnote:api:assets:upload", (body) => storeAsset(body || {}));
  registerApiHandler("aaronnote:api:assets:store-from-path", (body) => storeAssetFromPath(body || {}));
  registerApiHandler("aaronnote:api:assets:render-tikz", (body) => renderTikzAsset(body || {}));
  registerApiHandler("aaronnote:api:assets:scan-orphans", async () => ({ type: "unused-assets", assets: await scanUnusedAssets(), root: noteRoot }));
  registerApiHandler("aaronnote:api:assets:trash-orphans", async (files) => {
    try {
      return await trashUnusedAssets({ files });
    } catch (err) {
      showTaskNotification("AaronNote", err instanceof Error
        ? `Move unused assets to Trash failed: ${err.message}`
        : "Move unused assets to Trash failed.");
      throw err;
    }
  });

  registerApiHandler("aaronnote:api:session:recent", async () => ({ type: "recent", recent: await readRecentNotes() }));
  registerApiHandler("aaronnote:api:session:touch-recent", async (file, openedAt) => ({
    type: "recent",
    recent: await touchRecentNote(String(file || ""), Number(openedAt) || Date.now()),
  }));
  registerApiHandler("aaronnote:api:session:positions", async () => ({ type: "positions", positions: await readCursorPositions() }));
  registerApiHandler("aaronnote:api:session:save-position", async (position) => ({
    type: "positions",
    positions: await touchCursorPosition(position || {}),
  }));

  registerApiHandler("aaronnote:api:plugins:list", () => pluginsPayload(false));
  registerApiHandler("aaronnote:api:plugins:overrides", async () => ({ type: "plugin-overrides", overrides: await readPluginOverrides() }));
  registerApiHandler("aaronnote:api:plugins:save-overrides", async (overrides) => ({ type: "plugin-overrides", overrides: await writePluginOverrides(overrides) }));

  registerApiHandler("aaronnote:api:fs:rename", (body) => renameManagedPath(body || {}));
  registerApiHandler("aaronnote:api:fs:move", (body) => moveManagedPath(body || {}));
  registerApiHandler("aaronnote:api:fs:duplicate", (body) => duplicateManagedFile(body || {}));
  registerApiHandler("aaronnote:api:fs:trash", (body) => trashManagedPath(body || {}));
  registerApiHandler("aaronnote:api:meta:add", (body) => updateCurrentNoteMeta(body || {}, "add"));
  registerApiHandler("aaronnote:api:meta:remove", (body) => updateCurrentNoteMeta(body || {}, "remove"));
  registerApiHandler("aaronnote:api:meta:tag", (body) => updateCurrentNoteMeta(body || {}, "tag"));
  registerApiHandler("aaronnote:api:meta:hide-roam", (body) => updateCurrentNoteMeta(body || {}, "hide-roam"));
  registerApiHandler("aaronnote:api:meta:activate-roam", (body) => updateCurrentNoteMeta(body || {}, "activate-roam"));
  registerApiHandler("aaronnote:api:prose-check:run", (body) => runExternalProseChecks(body || {}));
  registerApiHandler("aaronnote:api:shell:show-in-folder", (file) => {
    const target = resolveShellPath(file);
    shell.showItemInFolder(target);
    return { ok: true, file: target };
  });
  registerApiHandler("aaronnote:api:shell:open-path", async (file) => {
    const target = resolveShellPath(file);
    const message = await shell.openPath(target);
    return message ? { ok: false, file: target, message } : { ok: true, file: target };
  });
  registerApiHandler("aaronnote:api:shell:open-directory", async (body) => {
    const target = resolveShellDirectoryPath(body?.path ?? body, body?.base ?? "");
    const message = await shell.openPath(target);
    return message ? { ok: false, file: target, message } : { ok: true, file: target };
  });
  registerApiHandler("aaronnote:api:shell:open-directory-in-kitty", (body) => openDirectoryInKitty(body || {}));
  registerApiHandler("aaronnote:api:external-editor:open", (body) => openExternalEditorTarget(body || {}, { resolveFile: resolveShellPath }));
  registerApiHandler("aaronnote:api:shell:show-attachment-menu", (file, base, options = {}) => {
    const target = resolveMediaFile(file, base);
    const href = String(options?.href || file || "");
    Menu.buildFromTemplate(attachmentContextMenuTemplate({
      file: target,
      href,
      jupyter: /\.ipynb$/i.test(target),
    }, {
      command: (command, detail) => runInWindow(dispatchCommandScript(command, detail)),
      open: () => void shell.openPath(target),
      reveal: () => shell.showItemInFolder(target),
      copy: (text) => clipboard.writeText(String(text || "")),
    })).popup();
    return { ok: true, file: target };
  });
  ipcMain.handle("aaronnote:api:shell:show-editor-context-menu", (event, options = {}) => {
    const win = BrowserWindow.fromWebContents(event.sender);
    Menu.buildFromTemplate([
      ...proseDiagnosticMenuItems(win, options),
      ...editorContextMenuTemplate(options, {
        command: (command, detail) => runInSpecificWindow(win, dispatchCommandScript(command, detail)),
      }),
      { type: "separator" },
      {
        label: "Toggle Lean Panel",
        click: () => runInSpecificWindow(win, dispatchCommandScript("toggle-lean-panel")),
      },
      {
        label: "Insert Lean Block...",
        click: () => runInSpecificWindow(win, dispatchCommandScript("open-lean-block-manager")),
      },
      leanCleanMenuItem(win, options),
    ]).popup({ window: win ?? undefined });
    return { ok: true };
  });
  ipcMain.handle("aaronnote:api:shell:show-lean-editor-menu", (event, options = {}) => {
    const win = BrowserWindow.fromWebContents(event.sender);
    const editorId = String(options?.editorId ?? "");
    const line = Number(options?.line ?? 0);
    const character = Number(options?.character ?? 0);
    const lsp = (action, label) => ({ label, click: () => runInSpecificWindow(win, leanMenuActionScript(editorId, "lsp", action, line, character)) });
    const edit = (action, label) => ({ label, click: () => runInSpecificWindow(win, leanMenuActionScript(editorId, "edit", action, line, character)) });
    const external = (action, label) => ({ label, click: () => runInSpecificWindow(win, leanMenuActionScript(editorId, "external", action, line, character)) });
    Menu.buildFromTemplate([
      external("openNeovide", "Open in Neovide"),
      { type: "separator" },
      {
        label: "Lean Symbol",
        submenu: [
          lsp("definition", "Go to Definition"),
          lsp("declaration", "Go to Declaration"),
          lsp("typeDefinition", "Go to Type Definition"),
          lsp("implementation", "Go to Implementation"),
          lsp("references", "Find References"),
          lsp("hover", "Show Hover"),
        ],
      },
      {
        label: "Lean Edit",
        submenu: [
          edit("toggleLineComment", "Toggle Line Comment"),
          edit("toggleBlockComment", "Toggle Block Comment"),
          { type: "separator" },
          edit("duplicateUp", "Duplicate Up"),
          edit("duplicateDown", "Duplicate Down"),
          edit("moveUp", "Move Lines Up"),
          edit("moveDown", "Move Lines Down"),
          { type: "separator" },
          edit("joinLines", "Join Lines"),
          edit("deleteTrailingWhitespace", "Delete Trailing Whitespace"),
          { type: "separator" },
          edit("indent", "Indent"),
          edit("outdent", "Outdent"),
        ],
      },
    ]).popup({ window: win ?? undefined });
    return { ok: true };
  });
  ipcMain.handle("aaronnote:api:shell:open-lean-location", (_event, target = {}) => openLeanLocation(target || {}));
  registerApiHandler("aaronnote:api:copilot:request", (action, body) => handleCopilotRequest(String(action || ""), body || {}));
  registerApiHandler("aaronnote:api:roamlookup:request", (action, body) => handleRoamLookupRequest(String(action || ""), body || {}));
  registerApiHandler("aaronnote:api:lean:request", (action, body) => handleLeanRequest(String(action || ""), body || {}));
  registerApiHandler("aaronnote:api:jupyter:request", (action, body) => handleJupyterRequest(String(action || ""), body || {}));
  ipcMain.handle("aaronnote:api:jupyter:scroll", async (event, body = {}) => {
    try {
      const run = () => handleJupyterScroll(event.sender, body || {});
      return debugPanel ? await debugPanel.trackTask("aaronnote:api:jupyter:scroll", run) : await run();
    } catch (err) {
      return errorPayload(err);
    }
  });
  ipcMain.handle("aaronnote:api:jupyter:kernel-status", async (event, body = {}) => {
    try {
      return await handleJupyterKernelStatus(event.sender, body || {});
    } catch (err) {
      return errorPayload(err);
    }
  });
  registerApiHandler("aaronnote:api:graph", async () => graphPayload(await scanNotes()));
  registerApiHandler("aaronnote:api:tags", async () => tagIndexPayload(await scanNotes()));
}

function proseDiagnosticMenuItems(win, options = {}) {
  const diagnostics = Array.isArray(options?.diagnostics) ? options.diagnostics.slice(0, 6) : [];
  const items = [];
  for (const diag of diagnostics) {
    const source = String(diag?.source || "prose");
    const message = String(diag?.message || "Prose issue").replace(/\s+/g, " ").slice(0, 140);
    const from = Number(diag?.from);
    const to = Number(diag?.to);
    items.push({ label: `${source}: ${message}`, enabled: false });
    const suggestions = [...new Set(Array.isArray(diag?.suggestions) ? diag.suggestions.map((item) => String(item)) : [])].slice(0, 8);
    if (Number.isFinite(from) && Number.isFinite(to) && from < to) {
      for (const suggestion of suggestions) {
        const label = suggestion ? `Replace with "${suggestion.slice(0, 72)}"` : "Remove";
        items.push({
          label,
          click: () => runInSpecificWindow(win, dispatchProseFixScript(from, to, suggestion)),
        });
      }
    }
  }
  return items.length > 0 ? [...items, { type: "separator" }] : [];
}

function leanCleanMenuItem(win, options = {}) {
  const block = options?.leanBlock && typeof options.leanBlock === "object" ? options.leanBlock : null;
  const tag = String(block?.tag ?? "").trim();
  const selector = String(block?.selector ?? "");
  if (!tag) {
    return {
      label: "Clean Current Lean Block",
      click: () => runInSpecificWindow(win, dispatchCommandScript("clean-lean-block")),
    };
  }
  const target = selector || "default";
  return {
    label: `Clean Lean Block (${target} #${tag})`,
    click: () => runInSpecificWindow(win, dispatchCommandScript("clean-lean-block", { tag, selector })),
  };
}

function resolveShellPath(file) {
  const raw = String(file || "").trim();
  if (!raw || raw === "Root") return noteRoot;
  return resolve(raw.startsWith("/") ? raw : join(noteRoot, raw));
}

function resolveShellDirectoryPath(file, base = "") {
  const raw = String(file || "").trim();
  const baseFile = String(base || "").trim() ? resolveShellPath(base) : "";
  const baseDir = baseFile && !inside(baseFile, noteRoot) ? dirname(baseFile) : noteRoot;
  const target = !raw || raw === "Root"
    ? baseDir
    : resolve(raw.startsWith("/") ? raw : join(baseDir, raw));
  try {
    return statSync(target).isDirectory() ? target : dirname(target);
  } catch {
    return target;
  }
}

async function flushRendererState(win) {
  if (!win || win.isDestroyed()) return;
  // Best-effort flush during window close; the renderer may already be gone.
  await win.webContents.executeJavaScript(
    "window.dispatchEvent(new CustomEvent('aaronnote:command', { detail: { command: 'flush-state' } })); true",
    true,
  ).catch(() => {});
}

function confirmWindowClose(win) {
  const openWindows = BrowserWindow.getAllWindows().filter((window) => !window.isDestroyed());
  if (openWindows.length <= 1) {
    dialog.showMessageBoxSync(win, {
      type: "info",
      buttons: ["Keep Open"],
      defaultId: 0,
      title: "Keep Last Window Open",
      message: "AaronNote keeps the last window open.",
      detail: "Use Cmd+Q to quit the app.",
      noLink: true,
    });
    return false;
  }
  const choice = dialog.showMessageBoxSync(win, {
    type: "question",
    buttons: ["Cancel", "Close"],
    defaultId: 0,
    cancelId: 0,
    title: "Close AaronNote?",
    message: "Close this AaronNote window?",
    detail: "Current cursor position and pending edits will be flushed before closing.",
    noLink: true,
  });
  return choice === 1;
}

function confirmQuit() {
  const choice = dialog.showMessageBoxSync(mainWindow ?? undefined, {
    type: "question",
    buttons: ["Cancel", "Quit"],
    defaultId: 0,
    cancelId: 0,
    title: "Quit AaronNote?",
    message: "Quit AaronNote?",
    detail: "Current cursor position and pending edits will be flushed before quitting.",
    noLink: true,
  });
  return choice === 1;
}

async function loadRenderer(win, file = "") {
  const resolvedFile = file ? resolve(file) : "";
  if (devViteUrl) {
    await win.loadURL(urlForFile(devViteUrl, resolvedFile));
    scheduleApplyZoom(win);
    return;
  }
  const query = resolvedFile ? { file: resolvedFile } : undefined;
  await win.loadFile(join(staticDir, "index.html"), query ? { query } : undefined);
  scheduleApplyZoom(win);
}

async function loadRendererUrl(win, targetUrl) {
  await win.loadURL(targetUrl);
  scheduleApplyZoom(win);
}

function createWindow(options = {}) {
  const win = new BrowserWindow({
    width: 1320,
    height: 920,
    minWidth: 920,
    minHeight: 640,
    title: "AaronNote",
    autoHideMenuBar: false,
    backgroundColor: "#eeeae1",
    fullscreen: true,
    webPreferences: {
      contextIsolation: true,
      nodeIntegration: false,
      preload: join(desktopDir, "preload.cjs"),
      sandbox: true,
    },
  });
  if (options.primary !== false) mainWindow = win;
  win.aaronnoteAppWindow = true;
  win.aaronnoteRendererReady = false;
  win.aaronnotePendingOpenFile = "";
  enablePinchZoom(win);
  debugPanel?.observeWindow(win);
  scheduleApplyZoom(win);
  win.on("focus", () => scheduleApplyZoom(win));
  win.on("show", () => scheduleApplyZoom(win));
  win.on("restore", () => scheduleApplyZoom(win));
  win.on("enter-full-screen", () => scheduleApplyZoom(win));
  win.on("leave-full-screen", () => scheduleApplyZoom(win));
  win.webContents.on("dom-ready", () => scheduleApplyZoom(win));
  win.webContents.on("did-finish-load", () => {
    scheduleApplyZoom(win);
    flushPendingOpenFile(win);
  });
  win.webContents.on("did-navigate", () => scheduleApplyZoom(win));
  win.webContents.on("zoom-changed", (event, zoomDirection) => {
    event.preventDefault();
    stepDesiredZoomLevel(zoomDirection === "in" ? ZOOM_STEP : -ZOOM_STEP);
  });

  win.webContents.setWindowOpenHandler(({ url: targetUrl }) => {
    if (rendererAppUrl(targetUrl)) {
      createWindow({ primary: false, url: targetUrl });
      return { action: "deny" };
    }
    if (targetUrl.startsWith("aaronnote-asset:")) {
      return { action: "deny" };
    }
    void shell.openExternal(targetUrl);
    return { action: "deny" };
  });
  win.webContents.on("will-navigate", (event, targetUrl) => {
    if (internalRendererUrl(targetUrl)) return;
    event.preventDefault();
    void shell.openExternal(targetUrl);
  });
  win.webContents.on("before-input-event", (event, input) => {
    if (handleFullscreenShortcut(win, input)) {
      event.preventDefault();
      return;
    }
    if (handleZoomShortcut(win, input)) {
      event.preventDefault();
      return;
    }
    const historyCommand = historyShortcutCommand(input);
    if (historyCommand) {
      event.preventDefault();
      if (input.type === "keyDown") {
        runInSpecificWindow(win, dispatchCommandScript(historyCommand));
      }
      return;
    }
    if (shouldOwnShortcut(input)) {
      event.preventDefault();
      if (input.type !== "keyDown") return;
      const key = input.key.toLowerCase();
      if (key === "j" && !input.shift) runInSpecificWindow(win, dispatchCommandScript("jump-stack"));
      else if (key === "l") runInSpecificWindow(win, dispatchCommandScript(input.shift ? "insert-lean-block" : "toggle-lean-panel"));
      else if (key === "r" && !input.shift) void reloadCurrentWindow();
      else if (key === "r" && input.shift) runInSpecificWindow(win, dispatchCommandScript("reload-snippets"));
      else if (key === "w" && !input.shift) closeCurrentWindow();
    }
  });
  win.on("close", async (event) => {
    if (allowQuit || win.aaronnoteAllowClose) return;
    event.preventDefault();
    if (!confirmWindowClose(win)) return;
    win.aaronnoteAllowClose = true;
    await flushRendererState(win);
    win.close();
  });

  const targetUrl = typeof options.url === "string" && rendererAppUrl(options.url) ? options.url : "";
  void (targetUrl ? loadRendererUrl(win, targetUrl) : loadRenderer(win, pendingOpenFile));
  if (options.primary !== false) pendingOpenFile = "";
  return win;
}

function urlForFile(baseUrl, file = "") {
  if (!file) return baseUrl;
  const url = new URL(baseUrl);
  url.searchParams.set("file", resolve(file));
  return url.toString();
}

function leanSourceFile(file) {
  return /\.lean$/i.test(String(file || ""));
}

function notifyLeanManualOpen() {
  showTaskNotification("AaronNote", "Lean files are edited manually.");
}

function sendOpenFileToWindow(win, file) {
  const resolved = resolve(file);
  if (leanSourceFile(resolved)) {
    notifyLeanManualOpen();
    return;
  }
  if (!win || win.isDestroyed()) return;
  if (!win.aaronnoteRendererReady) {
    win.aaronnotePendingOpenFile = resolved;
    return;
  }
  win.webContents.send("aaronnote:open-file", resolved);
}

function flushPendingOpenFile(win) {
  if (!win || win.isDestroyed() || !win.aaronnoteRendererReady) return;
  const file = win.aaronnotePendingOpenFile;
  if (!file) return;
  win.aaronnotePendingOpenFile = "";
  sendOpenFileToWindow(win, file);
}

function runInWindow(script) {
  const win = targetAppWindow();
  if (!win || win.isDestroyed()) return;
  void win.webContents.executeJavaScript(script, true);
}

function runInSpecificWindow(win, script) {
  if (!win || win.isDestroyed()) return;
  void win.webContents.executeJavaScript(script, true);
}

function focusAndRunCommand(command) {
  let win = mainWindow;
  const script = dispatchCommandScript(command);
  if (!win || win.isDestroyed()) {
    win = createWindow();
    win.webContents.once("did-finish-load", () => {
      win.show();
      win.focus();
      runInSpecificWindow(win, script);
    });
    return;
  }
  win.show();
  win.focus();
  runInSpecificWindow(win, script);
}

function registerGlobalShortcuts() {
  globalShortcut.register("CommandOrControl+Shift+N", () => {
    focusAndRunCommand("new-markdown-note");
  });
}

function closeCurrentWindow() {
  const win = targetAppWindow();
  if (!win || win.isDestroyed()) return;
  win.close();
}

async function reloadCurrentWindow() {
  const win = targetAppWindow();
  if (!win || win.isDestroyed()) return;
  await flushRendererState(win);
  const file = await win.webContents.executeJavaScript(
    "window.AaronnoteCurrentFile?.() || new URL(window.location.href).searchParams.get('file') || ''",
    true,
  ).catch(() => "");
  if (file) {
    sendOpenFileToWindow(win, String(file));
    return;
  }
  win.webContents.reload();
}

function createNewWindow() {
  createWindow({ primary: false });
}

async function openRoamDb() {
  await roamSyncPayload().catch((err) => console.warn("[roam] sync payload failed", err));
  await shell.openPath(join(noteRoot, "roam.db"));
}

function dispatchKeyScript(key) {
  return `document.dispatchEvent(new KeyboardEvent('keydown', { key: ${JSON.stringify(key)}, metaKey: true, bubbles: true }))`;
}

function dispatchCommandScript(command, detail = {}) {
  const payload = detail && typeof detail === "object" && !Array.isArray(detail)
    ? { ...detail, command }
    : { command };
  return `window.dispatchEvent(new CustomEvent('aaronnote:command', { detail: ${JSON.stringify(payload)} }))`;
}

function dispatchProseFixScript(from, to, replacement) {
  const detail = { command: "apply-prose-fix", from, to, replacement };
  return `window.dispatchEvent(new CustomEvent('aaronnote:command', { detail: ${JSON.stringify(detail)} }))`;
}

function leanMenuActionScript(editorId, kind, action, line, character) {
  return dispatchCommandScript("lean-editor-menu-action", { editorId, kind, action, line, character });
}

/**
 * Open a Lean source location (e.g. Mathlib/prelude) in a fresh Kitty window
 * running Neovim at the target line/character. Paths are passed as an argv
 * array — never shell-concatenated. Missing kitty/nvim/file is reported back so
 * the renderer can surface it in the status bar.
 */
function openLeanLocation(target) {
  const file = String(target?.file ?? "");
  if (!file || !existsSync(file)) return { ok: false, message: `Lean source not found: ${file}` };
  return openExternalEditorTarget({
    kind: "file",
    file,
    line: target?.line,
    character: target?.character,
  }, { resolveFile: (path) => resolve(String(path || "")) });
}

function openDirectoryInKitty(body) {
  const target = resolveShellDirectoryPath(body?.path ?? body, body?.base ?? "");
  if (!target || !existsSync(target)) return { ok: false, file: target, message: `Directory not found: ${target}` };
  try {
    if (!statSync(target).isDirectory()) return { ok: false, file: target, message: `Not a directory: ${target}` };
  } catch (err) {
    return { ok: false, file: target, message: err instanceof Error ? err.message : "Directory unavailable" };
  }
  const kitty = findKittyExecutable();
  if (!kitty) return { ok: false, file: target, message: "Kitty executable not found. Set AARONNOTE_KITTY or update PATH." };
  const { command, args } = kittyDirectoryCommand({ kitty, dir: target });
  try {
    const child = spawn(command, args, { detached: true, stdio: "ignore" });
    child.once("error", (err) => console.error("Kitty directory open failed", err));
    child.unref();
    return { ok: true, file: target };
  } catch (err) {
    return { ok: false, file: target, message: err instanceof Error ? err.message : "Failed to open Kitty" };
  }
}

function zshEnvironmentShell() {
  const shellPath = String(process.env.SHELL || "");
  if (basename(shellPath) === "zsh" && existsSync(shellPath)) return shellPath;
  return existsSync("/bin/zsh") ? "/bin/zsh" : "";
}

async function readJupyterShellEnv(cwd = noteRoot) {
  if (jupyterShellEnvPromise) return jupyterShellEnvPromise;
  const shellPath = zshEnvironmentShell();
  if (!shellPath) return {};
  jupyterShellEnvPromise = execFileAsync(shellPath, ["-lic", "command env -0"], {
    cwd,
    env: process.env,
    maxBuffer: 2 * 1024 * 1024,
    timeout: 5000,
  })
    .then(({ stdout }) => parseNulEnv(stdout))
    .catch((err) => {
      console.warn("Failed to load zsh environment for Jupyter", err);
      return {};
    });
  return jupyterShellEnvPromise;
}

async function jupyterProcessEnv(root) {
  const shellEnv = await readJupyterShellEnv(root);
  return { ...mergeJupyterEnv(process.env, shellEnv), BROWSER: "" };
}

function findJupyterExecutable(env = process.env) {
  const pathValue = String(env.PATH ?? "");
  const preferredDirs = ["/opt/homebrew/bin", "/usr/local/bin"];
  return findExecutable("jupyter-lab", {
    candidates: [env.AARONNOTE_JUPYTER, env.AARONNOTE_JUPYTER_LAB],
    preferredDirs,
    pathValue,
  }) || findExecutable("jupyter", {
    candidates: [env.AARONNOTE_JUPYTER],
    preferredDirs,
    pathValue,
  });
}

function freeLocalPort() {
  return new Promise((resolvePort, rejectPort) => {
    const server = createServer();
    server.once("error", rejectPort);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      const port = typeof address === "object" && address ? address.port : 0;
      server.close(() => port ? resolvePort(port) : rejectPort(new Error("Failed to allocate Jupyter port")));
    });
  });
}

function resolveJupyterNotebookPath(path, base = "") {
  const target = resolveMediaFile(path, base);
  if (!/\.ipynb$/i.test(target)) {
    const err = new Error(`Jupyter preview requires an .ipynb file: ${target}`);
    err.statusCode = 400;
    throw err;
  }
  if (!existsSync(target)) {
    const err = new Error(`Notebook not found: ${target}`);
    err.statusCode = 404;
    throw err;
  }
  return target;
}

function jupyterFileFromUrl(url) {
  const parsed = parsedUrl(url);
  if (!parsed || !jupyterSession?.root) return "";
  const prefix = "/lab/tree/";
  if (!parsed.pathname.startsWith(prefix)) return "";
  const parts = parsed.pathname
    .slice(prefix.length)
    .split("/")
    .filter(Boolean)
    .map((part) => {
      try { return decodeURIComponent(part); }
      catch { return part; }
    });
  return resolve(jupyterSession.root, ...parts);
}

function jupyterUrlWithSelector(url, selector, selectorKind) {
  const parsed = parsedUrl(url);
  if (!parsed) return String(url || "");
  const cleanSelector = jupyterSelectorPath(selector);
  void selectorKind;
  parsed.hash = cleanSelector ? encodeURIComponent(cleanSelector) : "";
  return parsed.toString();
}

async function resolveJupyterSelector(file, selector, selectorKind) {
  void file;
  void selectorKind;
  const cleanSelector = jupyterSelectorPath(selector);
  return { selector: cleanSelector, selectorKind: cleanSelector ? "toc" : "" };
}

function jupyterRootForFile(file) {
  const resolved = resolve(file);
  return inside(resolved, noteRoot) ? noteRoot : dirname(resolved);
}

function broadcastJupyterStatus(payload) {
  for (const win of appWindows()) {
    if (!win.isDestroyed()) win.webContents.send("aaronnote:jupyter:status", payload);
  }
}

function touchJupyterSession() {
  if (!jupyterSession) return;
  jupyterSession.lastUsedAt = Date.now();
  if (jupyterIdleTimer) clearTimeout(jupyterIdleTimer);
  jupyterIdleTimer = setTimeout(() => {
    if (!jupyterSession) return;
    if (Date.now() - jupyterSession.lastUsedAt >= jupyterIdleMs) void stopJupyterSession();
    else touchJupyterSession();
  }, jupyterIdleMs);
}

async function waitForJupyterReady(session) {
  const statusUrl = `${session.baseUrl}/api/status?token=${encodeURIComponent(session.token)}`;
  const started = Date.now();
  let lastMessage = "";
  while (Date.now() - started < 30_000) {
    if (session.child.exitCode != null) {
      throw new Error(lastMessage || `Jupyter exited with code ${session.child.exitCode}`);
    }
    try {
      const res = await fetch(statusUrl);
      if (res.ok) return;
      lastMessage = `Jupyter status ${res.status}`;
    } catch (err) {
      lastMessage = err instanceof Error ? err.message : String(err);
    }
    await new Promise((resolveTimer) => setTimeout(resolveTimer, 300));
  }
  throw new Error(lastMessage ? `Jupyter did not become ready: ${lastMessage}` : "Jupyter did not become ready");
}

function withTimeout(promise, ms, label) {
  let timer = null;
  const timeout = new Promise((_, reject) => {
    timer = setTimeout(() => reject(new Error(`${label} timed out after ${ms}ms`)), ms);
  });
  return Promise.race([promise, timeout]).finally(() => {
    if (timer) clearTimeout(timer);
  });
}

function ensureJupyterReady(session) {
  if (!session) return Promise.reject(new Error("Jupyter session is unavailable"));
  if (session.ready === true) return Promise.resolve(session);
  if (!session.readyPromise) {
    session.readyPromise = waitForJupyterReady(session)
      .then(() => {
        session.ready = true;
        session.readyError = "";
        return session;
      })
      .catch((err) => {
        session.readyError = err instanceof Error ? err.message : String(err);
        throw err;
      })
      .finally(() => {
        session.readyPromise = null;
      });
  }
  return session.readyPromise;
}

async function stopJupyterSession() {
  const session = jupyterSession;
  jupyterSession = null;
  if (jupyterIdleTimer) clearTimeout(jupyterIdleTimer);
  jupyterIdleTimer = null;
  if (!session?.child || session.child.exitCode != null) return { ok: true };
  session.child.kill("SIGTERM");
  await new Promise((resolveTimer) => setTimeout(resolveTimer, 700));
  if (session.child.exitCode == null) session.child.kill("SIGKILL");
  return { ok: true };
}

async function ensureJupyterSession(root, restart = false) {
  const resolvedRoot = resolve(root);
  if (jupyterSession && !restart && jupyterSession.root === resolvedRoot && jupyterSession.child.exitCode == null) {
    touchJupyterSession();
    return jupyterSession;
  }
  if (jupyterSession) await stopJupyterSession();
  const env = await jupyterProcessEnv(resolvedRoot);
  const command = findJupyterExecutable(env);
  if (!command) throw new Error("JupyterLab not found in the zsh environment. Install jupyterlab or set AARONNOTE_JUPYTER.");
  const port = await freeLocalPort();
  const token = ""; // Auth is disabled for the local 127.0.0.1 server (see jupyterLaunchArgs).
  const baseUrl = `http://127.0.0.1:${port}`;
  const child = spawn(command, jupyterLaunchArgs({ command, root: resolvedRoot, port, token }), {
    cwd: resolvedRoot,
    stdio: ["ignore", "pipe", "pipe"],
    env,
  });
  const session = {
    child,
    command,
    port,
    token,
    root: resolvedRoot,
    baseUrl,
    startedAt: Date.now(),
    lastUsedAt: Date.now(),
    output: "",
    ready: false,
    readyError: "",
    readyPromise: null,
  };
  const appendOutput = (chunk) => {
    session.output = `${session.output}${String(chunk || "")}`.slice(-8000);
  };
  child.stdout?.on("data", appendOutput);
  child.stderr?.on("data", appendOutput);
  child.once("error", (err) => {
    appendOutput(err instanceof Error ? err.message : String(err));
    if (jupyterSession === session) {
      jupyterSession = null;
      broadcastJupyterStatus({ running: false, crashed: true, output: session.output.slice(-2000) });
    }
  });
  child.once("exit", (code, signal) => {
    // stopJupyterSession() / restart nulls jupyterSession before killing, so an exit
    // while we still hold this session means the server died unexpectedly. Only
    // notify in that case — intentional stop/restart manage their own UI.
    if (jupyterSession !== session) return;
    jupyterSession = null;
    broadcastJupyterStatus({
      running: false,
      crashed: true,
      code: code ?? null,
      signal: signal ?? null,
      output: session.output.slice(-2000),
    });
  });
  jupyterSession = session;
  void ensureJupyterReady(session)
    .catch(() => {
      console.warn("Jupyter readiness check failed", session.readyError);
    });
  touchJupyterSession();
  return session;
}

function parsedUrl(value) {
  try {
    return new URL(String(value || ""));
  } catch {
    return null;
  }
}

function jupyterFrameForContents(contents, targetUrl = "") {
  const frames = contents?.mainFrame?.framesInSubtree || [];
  const target = parsedUrl(targetUrl);
  const sessionBase = parsedUrl(jupyterSession?.baseUrl || "");
  const candidates = frames.filter((frame) => {
    if (!frame || frame === contents.mainFrame || frame.isDestroyed?.() || !frame.parent) return false;
    const url = parsedUrl(frame.url);
    if (!url || !url.pathname.startsWith("/lab")) return false;
    if (target && url.origin === target.origin) return true;
    return Boolean(sessionBase && url.origin === sessionBase.origin);
  });
  if (target) {
    const exact = candidates.find((frame) => {
      const url = parsedUrl(frame.url);
      return url && url.origin === target.origin && url.pathname === target.pathname;
    });
    if (exact) return exact;
  }
  return candidates[0] || null;
}

function jupyterScrollScript(body = {}) {
  const payload = JSON.stringify({
    url: String(body.url || ""),
    selector: String(body.selector || ""),
    selectorKind: String(body.selectorKind || ""),
  });
  return `
(() => {
  const payload = ${payload};
  const selector = String(payload.selector || "").trim();
  const selectorKind = String(payload.selectorKind || "");
  const targetUrl = String(payload.url || "");
  void selectorKind;

  function decode(value) {
    try { return decodeURIComponent(String(value || "")); }
    catch { return String(value || ""); }
  }

  function attr(name, value) {
    try {
      return document.querySelector("[" + name + "=" + JSON.stringify(String(value || "")) + "]");
    } catch {
      return null;
    }
  }

  function firstExisting(values, finder) {
    for (const value of values) {
      if (!value) continue;
      const found = finder(value);
      if (found) return found;
    }
    return null;
  }

  function normalizedText(value) {
    return decode(value)
      .replace(/^#/, "")
      .replace(/\\s+/g, " ")
      .trim()
      .toLowerCase();
  }

  function slugText(value) {
    return normalizedText(value)
      .replace(/['"]/g, "")
      .replace(/[^\\w\\u00a0-\\uffff]+/g, "-")
      .replace(/^-+|-+$/g, "");
  }

  function withoutNumberPrefix(value) {
    const normalized = normalizedText(value);
    const stripped = normalized.replace(/^\\d+(?:\\.\\d+)*\\.?\\s+/, "").trim();
    return stripped || normalized;
  }

  function selectorTextVariants(value = selector) {
    const raw = String(value || "").replace(/^#/, "");
    const decoded = decode(raw).replace(/^#/, "");
    return Array.from(new Set([raw, decoded].map((value) => String(value || "").trim()).filter(Boolean)));
  }

  function revealTocPanel() {
    if (document.querySelector(".jp-TableOfContents-content[data-document-type=\\"notebook\\"] .jp-tocItem")) return true;
    const tab = document.querySelector('[data-id="table-of-contents"], .lm-TabBar-tab[data-id="table-of-contents"]');
    if (!tab) return false;
    tab.dispatchEvent(new MouseEvent("mousedown", { bubbles: true, cancelable: true, view: window, button: 0, buttons: 1 }));
    tab.dispatchEvent(new MouseEvent("mouseup", { bubbles: true, cancelable: true, view: window, button: 0, buttons: 0 }));
    tab.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, view: window, button: 0, buttons: 0 }));
    return true;
  }

  function tocItems() {
    const root = document.querySelector(".jp-TableOfContents-content[data-document-type=\\"notebook\\"]")
      || document.querySelector(".jp-TableOfContents-content");
    if (!root) return [];
    return Array.from(root.querySelectorAll(".jp-tocItem, .jp-TreeItem"));
  }

  function tocItemContent(item) {
    return item.querySelector(".jp-tocItem-content") || item.querySelector("[title]") || item;
  }

  function tocItemValues(item) {
    const content = tocItemContent(item);
    const dataset = content.dataset || {};
    return [
      content.getAttribute("title") || "",
      content.textContent || "",
      dataset.jupyterId || "",
      dataset.id || "",
      dataset.headingId || "",
    ];
  }

  function valuesMatchSelector(values, selectorValue) {
    const variants = selectorTextVariants(selectorValue);
    const normalizedVariants = variants.map(normalizedText).filter(Boolean);
    const slugVariants = variants.map(slugText).filter(Boolean);
    for (const value of values) {
      const normalized = normalizedText(value);
      if (normalized && normalizedVariants.includes(normalized)) return true;
      const stripped = withoutNumberPrefix(value);
      if (stripped && normalizedVariants.includes(stripped)) return true;
      const slug = slugText(value);
      if (slug && slugVariants.includes(slug)) return true;
    }
    return false;
  }

  function tocItemMatches(item) {
    return valuesMatchSelector(tocItemValues(item), selector);
  }

  function triggerTocScroll() {
    if (!selector) return false;
    revealTocPanel();
    const item = tocItems().find(tocItemMatches);
    if (!item) return false;
    const target = tocItemContent(item);
    target.dispatchEvent(new MouseEvent("mousedown", { bubbles: true, cancelable: true, view: window, button: 0, buttons: 1 }));
    target.dispatchEvent(new MouseEvent("mouseup", { bubbles: true, cancelable: true, view: window, button: 0, buttons: 0 }));
    target.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, view: window, button: 0, buttons: 0 }));
    return true;
  }

  function updateHash() {
    const hash = selector ? encodeURIComponent(selector) : "";
    const oldUrl = location.href;
    try {
      const next = targetUrl ? new URL(targetUrl) : null;
      const nextHash = next ? next.hash : (hash ? "#" + hash : "");
      history.replaceState(history.state, "", location.pathname + location.search + nextHash);
    } catch {
      history.replaceState(history.state, "", location.pathname + location.search + (hash ? "#" + hash : ""));
    }
    if (oldUrl !== location.href) {
      try {
        window.dispatchEvent(new HashChangeEvent("hashchange", { oldURL: oldUrl, newURL: location.href }));
      } catch {
        window.dispatchEvent(new Event("hashchange"));
      }
    }
  }

  function tocElement() {
    const decoded = decode(selector).replace(/^#/, "");
    const raw = selector.replace(/^#/, "");
    const variants = Array.from(new Set([raw, decoded, encodeURIComponent(decoded)].filter(Boolean)));
    const direct = firstExisting(variants, (value) =>
      document.getElementById(value)
      || attr("name", value)
      || attr("data-anchor", value));
    if (direct) return direct;
    const normalized = decoded.trim().toLowerCase();
    if (!normalized) return null;
    return Array.from(document.querySelectorAll("h1,h2,h3,h4,h5,h6")).find((heading) =>
      String(heading.textContent || "").trim().toLowerCase() === normalized) || null;
  }

  function targetElement() {
    if (!selector) return document.querySelector(".jp-NotebookPanel-notebook, .jp-Notebook, .jp-WindowedPanel-outer");
    const toc = tocElement();
    if (toc) return toc;
    return null;
  }

  function scrollableParent(element) {
    let node = element.parentElement;
    while (node && node !== document.body) {
      const style = window.getComputedStyle(node);
      if (/(auto|scroll|overlay)/.test(style.overflowY || "") && node.scrollHeight > node.clientHeight + 1) return node;
      node = node.parentElement;
    }
    return document.scrollingElement || document.documentElement;
  }

  function scrollElement(target) {
    const scrollTarget = target.closest?.(".jp-Cell, .jp-Notebook-cell") || target;
    const scroller = scrollableParent(scrollTarget);
    if (scroller && scroller !== document.body && scroller !== document.documentElement && scroller !== document.scrollingElement) {
      const scrollerRect = scroller.getBoundingClientRect();
      const targetRect = scrollTarget.getBoundingClientRect();
      const centeredTop = scroller.scrollTop
        + (targetRect.top - scrollerRect.top)
        - Math.max(12, (scrollerRect.height - Math.min(targetRect.height, scrollerRect.height)) / 2);
      scroller.scrollTo({ top: Math.max(0, centeredTop), left: 0, behavior: "auto" });
    } else {
      scrollTarget.scrollIntoView({ block: "center", inline: "nearest", behavior: "auto" });
    }
  }

  function scrollNow() {
    updateHash();
    if (triggerTocScroll()) return { ok: true, scrolled: true, via: "toc" };
    const target = targetElement();
    if (!target) return { ok: true, scrolled: false };
    if (!selector) {
      if (typeof target.scrollTo === "function") target.scrollTo({ top: 0, left: 0, behavior: "auto" });
      else window.scrollTo(0, 0);
      return { ok: true, scrolled: true };
    }
    scrollElement(target);
    return { ok: true, scrolled: true };
  }

  const result = scrollNow();
  if (!result.scrolled) {
    // Retry each animation frame until the anchor element appears in the DOM,
    // stopping as soon as it does or a ~1500ms deadline passes. Replaces the old
    // fire-and-hope triple-shot (100 / 350 / 900ms) which guessed at render timing.
    const deadline = Date.now() + 1500;
    const retry = () => {
      if (Date.now() > deadline) return;
      const r = scrollNow();
      if (!r.scrolled) requestAnimationFrame(retry);
    };
    requestAnimationFrame(retry);
  }
  return result;
})()
`;
}

async function handleJupyterScroll(contents, body = {}) {
  const file = body.path
    ? resolveJupyterNotebookPath(body.path || "", body.base || "")
    : jupyterFileFromUrl(body.url || "");
  const resolved = file
    ? await resolveJupyterSelector(file, body.selector || "", body.selectorKind || "")
    : { selector: String(body.selector || ""), selectorKind: String(body.selectorKind || "") };
  const scrollBody = {
    ...body,
    selector: resolved.selector,
    selectorKind: resolved.selectorKind,
    url: jupyterUrlWithSelector(body.url || "", resolved.selector, resolved.selectorKind),
  };
  const frame = jupyterFrameForContents(contents, scrollBody.url || body.url || "");
  if (!frame) return { ok: false, message: "Jupyter frame not found" };
  const result = await frame.executeJavaScript(jupyterScrollScript(scrollBody), true);
  touchJupyterSession();
  return { ok: true, url: scrollBody.url, ...(result && typeof result === "object" ? result : {}) };
}

// Reads JupyterLab's live kernel state from inside the (cross-origin) iframe. The
// renderer cannot reach contentWindow, but the Electron WebFrame can run this script.
const JUPYTER_KERNEL_STATUS_SCRIPT = `
(() => {
  try {
    const app = window.jupyterapp || window.jupyterlab || null;
    let connectionStatus = "";
    let status = "";
    const widget = app && app.shell && app.shell.currentWidget;
    const sessionContext = widget && (widget.sessionContext || (widget.context && widget.context.sessionContext));
    const kernel = sessionContext && sessionContext.session && sessionContext.session.kernel;
    if (kernel) {
      connectionStatus = String(kernel.connectionStatus || "");
      status = String(kernel.status || "");
    }
    if (!connectionStatus) {
      const exec = document.querySelector(".jp-Notebook-ExecutionIndicator");
      if (exec) status = exec.getAttribute("data-status") || status;
    }
    const dead = status === "dead";
    const live = connectionStatus === "connected" && (status === "idle" || status === "busy");
    return { hasKernel: Boolean(kernel), connectionStatus, status, connected: live, dead };
  } catch (err) {
    return { hasKernel: false, connectionStatus: "", status: "", connected: false, dead: false, error: String(err) };
  }
})()
`;

async function handleJupyterKernelStatus(contents, body = {}) {
  if (!jupyterSession) return { ok: true, running: false };
  const frame = jupyterFrameForContents(contents, body.url || "");
  if (!frame) return { ok: true, running: true, found: false };
  const result = await frame.executeJavaScript(JUPYTER_KERNEL_STATUS_SCRIPT, true);
  touchJupyterSession();
  return { ok: true, running: true, found: true, ...(result && typeof result === "object" ? result : {}) };
}

async function handleJupyterRequest(action, body = {}) {
  if (action === "open" || action === "restart") {
    const file = resolveJupyterNotebookPath(body.path || body.file || "", body.base || "");
    const root = jupyterRootForFile(file);
    const session = await withTimeout(
      ensureJupyterSession(root, action === "restart" || body.restart === true),
      8000,
      "Starting Jupyter",
    );
    const resolved = await resolveJupyterSelector(file, body.selector || "", body.selectorKind || "");
    touchJupyterSession();
    return {
      ok: true,
      file,
      root,
      url: jupyterLabUrl({
        baseUrl: session.baseUrl,
        root,
        file,
        token: session.token,
        selector: resolved.selector,
        selectorKind: resolved.selectorKind,
      }),
      selector: resolved.selector,
      selectorKind: resolved.selectorKind,
      baseUrl: session.baseUrl,
      startedAt: session.startedAt,
      ready: session.ready === true,
    };
  }
  if (action === "status") {
    // Re-arm the readiness check if it previously failed; ensureJupyterReady de-dupes
    // via readyPromise, so the renderer's poll can recover instead of getting stuck.
    if (jupyterSession && jupyterSession.ready !== true && jupyterSession.child?.exitCode == null) {
      void ensureJupyterReady(jupyterSession).catch((err) => console.warn("[jupyter] readiness re-arm failed", err));
    }
    return jupyterSession
      ? {
        ok: true,
        running: true,
        root: jupyterSession.root,
        baseUrl: jupyterSession.baseUrl,
        startedAt: jupyterSession.startedAt,
        ready: jupyterSession.ready === true,
        message: jupyterSession.readyError || "",
        output: jupyterSession.output.slice(-2000),
      }
      : { ok: true, running: false };
  }
  if (action === "stop") return await stopJupyterSession();
  return { ok: false, message: "Unknown Jupyter action" };
}

function pdfNameForFile(file, fallback = "Aaronnote.pdf") {
  const raw = String(file || fallback).split(/[\\/]/).pop() || fallback;
  const stem = raw.replace(/\.[^.]+$/, "") || "Aaronnote";
  return `${stem}.pdf`.replace(/[/:]/g, "-");
}

function publishedHtmlForFile(file) {
  const resolved = resolve(String(file || ""));
  if (!inside(resolved, workspaceRoot)) {
    throw new Error(`Cannot publish PDF for file outside workspace: ${resolved}`);
  }
  const rel = relative(workspaceRoot, resolved);
  if (!/\.(?:md|markdown)$/i.test(rel)) {
    throw new Error(`PDF export requires a Markdown note: ${resolved}`);
  }
  return join(pdfPublishRoot, rel.replace(/\.(?:md|markdown)$/i, ".html"));
}

async function publishNoteHtmlForPdf(file) {
  const resolved = resolve(String(file || ""));
  const htmlFile = publishedHtmlForFile(resolved);
  await execFileAsync(publishScript, [
    "--note",
    resolved,
    "--include-private",
    "--format",
    "pdf",
    "--output-root",
    pdfPublishRoot,
  ], {
    cwd: workspaceRoot,
    env: process.env,
    maxBuffer: 1024 * 1024 * 16,
  });
  await access(htmlFile);
  return htmlFile;
}

async function waitForPrintableAssets(win) {
  // Best-effort wait for fonts/images before printing; the inner 2500ms race caps it.
  await win.webContents.executeJavaScript(`
    Promise.race([
      (async () => {
        if (document.fonts?.ready) await document.fonts.ready.catch(() => {});
        const images = Array.from(document.images || []);
        await Promise.all(images.map((img) => img.complete ? true : new Promise((resolve) => {
          img.addEventListener("load", resolve, { once: true });
          img.addEventListener("error", resolve, { once: true });
        })));
        return true;
      })(),
      new Promise((resolve) => setTimeout(() => resolve(true), 2500)),
    ])
  `, true).catch(() => {});
}

async function chooseAndOpenMarkdown() {
  const win = targetAppWindow();
  if (!win) return;
  const result = await dialog.showOpenDialog(win, {
    title: "Open Markdown Note",
    defaultPath: noteRoot,
    properties: ["openFile"],
    filters: [
      { name: "Markdown", extensions: ["md", "markdown"] },
      { name: "All Files", extensions: ["*"] },
    ],
  });
  const file = result.filePaths[0];
  if (!file) return;
  sendOpenFileToWindow(win, file);
}

ipcMain.handle("aaronnote:choose-note-path", async (event, options = {}) => {
  const mode = options.mode === "directory" ? "directory" : options.mode === "openFile" ? "openFile" : "file";
  const suggestedPath = typeof options.suggestedPath === "string" && options.suggestedPath.trim()
    ? options.suggestedPath.trim()
    : mode === "directory" ? "." : mode === "openFile" ? ".lean" : "untitled.md";
  const defaultPath = resolve(noteRoot, suggestedPath);
  const owner = BrowserWindow.fromWebContents(event.sender) || targetAppWindow() || undefined;
  if (mode === "directory") {
    const dialogOptions = {
      title: typeof options.title === "string" ? options.title : "Choose Folder",
      defaultPath,
      properties: ["openDirectory", "createDirectory"],
    };
    const result = owner
      ? await dialog.showOpenDialog(owner, dialogOptions)
      : await dialog.showOpenDialog(dialogOptions);
    const picked = result.filePaths[0];
    if (result.canceled || !picked) return "";
    return normalizePickedNotePath(noteRoot, picked);
  }
  if (mode === "openFile") {
    const dialogOptions = {
      title: typeof options.title === "string" ? options.title : "Choose File",
      defaultPath,
      properties: ["openFile"],
      filters: [
        { name: "Lean", extensions: ["lean"] },
        { name: "All Files", extensions: ["*"] },
      ],
    };
    const result = owner
      ? await dialog.showOpenDialog(owner, dialogOptions)
      : await dialog.showOpenDialog(dialogOptions);
    const picked = result.filePaths[0];
    if (result.canceled || !picked) return "";
    return normalizePickedNotePath(noteRoot, picked);
  }
  const dialogOptions = {
    title: typeof options.title === "string" ? options.title : "Choose Note Path",
    defaultPath,
    properties: ["createDirectory", "showOverwriteConfirmation"],
    filters: [
      { name: "Markdown", extensions: ["md", "markdown"] },
      { name: "All Files", extensions: ["*"] },
    ],
  };
  const result = owner
    ? await dialog.showSaveDialog(owner, dialogOptions)
    : await dialog.showSaveDialog(dialogOptions);
  if (result.canceled || !result.filePath) return "";
  return normalizePickedNotePath(noteRoot, result.filePath);
});

ipcMain.handle("aaronnote:trash-note", async (_event, file = "") => {
  const resolved = resolve(String(file || ""));
  if (!inside(resolved, noteRoot)) {
    throw new Error(`File is outside note root: ${resolved}`);
  }
  return deleteNote({ file: resolved });
});

ipcMain.handle("aaronnote:export-pdf", async (event, options = {}) => {
  const owner = BrowserWindow.fromWebContents(event.sender) || mainWindow || undefined;
  const suggestedName = pdfNameForFile(options.name || options.file);
  const dialogOptions = {
    title: "Export PDF",
    defaultPath: join(homedir(), "Desktop", suggestedName),
    properties: ["createDirectory", "showOverwriteConfirmation"],
    filters: [
      { name: "PDF", extensions: ["pdf"] },
      { name: "All Files", extensions: ["*"] },
    ],
  };
  const result = owner
    ? await dialog.showSaveDialog(owner, dialogOptions)
    : await dialog.showSaveDialog(dialogOptions);
  if (result.canceled || !result.filePath) {
    return { ok: false, canceled: true, message: "Export canceled" };
  }

  const printWindow = new BrowserWindow({
    show: false,
    width: 960,
    height: 1280,
    backgroundColor: "#f7f4ed",
    webPreferences: {
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true,
    },
  });

  try {
    const htmlFile = await publishNoteHtmlForPdf(options.file || "");
    await printWindow.loadFile(htmlFile);
    await waitForPrintableAssets(printWindow);
    const pdf = await printWindow.webContents.printToPDF({
      pageSize: "A4",
      printBackground: true,
      preferCSSPageSize: true,
      scale: 0.7,
    });
    await writeFile(result.filePath, pdf);
    return { ok: true, file: result.filePath, message: `Exported ${result.filePath}` };
  } catch (err) {
    showTaskNotification("AaronNote", err instanceof Error
      ? `Export ${basename(result.filePath)} failed: ${err.message}`
      : `Export ${basename(result.filePath)} failed.`);
    throw err;
  } finally {
    printWindow.destroy();
  }
});

function openFileInWindow(file) {
  const resolved = resolve(file);
  if (leanSourceFile(resolved)) {
    notifyLeanManualOpen();
    return;
  }
  pendingOpenFile = resolved;
  if (!app.isReady()) return;
  if (!mainWindow || mainWindow.isDestroyed()) createWindow();
  else {
    mainWindow.show();
    mainWindow.focus();
    scheduleApplyZoom(mainWindow);
    sendOpenFileToWindow(mainWindow, resolved);
    pendingOpenFile = "";
  }
}

function leanLogMessage(entry) {
  const parts = [];
  if (entry.message) parts.push(String(entry.message));
  if (entry.package) parts.push(`package ${entry.package}`);
  if (entry.reason) parts.push(String(entry.reason));
  if (entry.command) parts.push(String(entry.command));
  if (entry.cwd) parts.push(String(entry.cwd));
  if (entry.notePath) parts.push(String(entry.notePath));
  if (entry.leanPath) parts.push(String(entry.leanPath));
  if (entry.path) parts.push(String(entry.path));
  if (entry.uri) parts.push(String(entry.uri));
  if (entry.count != null) parts.push(`${entry.count} diagnostics`);
  if (entry.bytes != null) parts.push(`${entry.bytes} bytes`);
  if (entry.tokenTypes != null) parts.push(`${entry.tokenTypes} token types`);
  if (entry.kind && entry.message == null) parts.push(String(entry.kind));
  return parts.join(" | ");
}

function leanLogMenuLabel(entry) {
  const time = new Date(entry.ts || Date.now()).toISOString().slice(11, 19);
  const message = leanLogMessage(entry);
  const raw = `${time} ${entry.type}${message ? `: ${message}` : ""}`;
  return raw.length > 96 ? `${raw.slice(0, 93)}...` : raw;
}

function leanFullLogText() {
  if (leanMenuLog.length === 0) return "No log entries yet.";
  return leanMenuLog.map((entry) => leanLogMenuLabel(entry)).join("\n");
}

function updateLeanMenuStatus(status = {}) {
  leanMenuStatus = {
    message: String(status.message || "Not started"),
    kind: String(status.kind || "Inactive"),
    busy: Boolean(status.busy),
  };
  scheduleLeanMenuUpdate();
}

function appendLeanMenuLog(entry) {
  if (!entry || typeof entry !== "object") return;
  leanMenuLog.push(entry);
  if (leanMenuLog.length > 12) leanMenuLog = leanMenuLog.slice(-12);
  scheduleLeanMenuUpdate();
}

function scheduleLeanMenuUpdate() {
  if (leanMenuUpdateTimer) return;
  leanMenuUpdateTimer = setTimeout(() => {
    leanMenuUpdateTimer = null;
    Menu.setApplicationMenu(buildMenu());
  }, 120);
}

async function refreshLeanMenuLog() {
  const result = await handleLeanRequest("log");
  leanMenuLog = Array.isArray(result?.entries) ? result.entries.slice(-12) : [];
  scheduleLeanMenuUpdate();
}

async function showLeanLogDialog() {
  await refreshLeanMenuLog();
  void dialog.showMessageBox({
    title: "Lean Server Log",
    message: "Lean Server Log",
    detail: leanFullLogText(),
    type: "info",
  });
}

function buildMenu() {
  return Menu.buildFromTemplate([
  {
    label: "AaronNote",
    submenu: [
      { role: "about" },
      { type: "separator" },
      { role: "hide" },
      { role: "hideOthers" },
      { role: "unhide" },
      { type: "separator" },
      { role: "quit" },
    ],
  },
  {
    label: "File",
    submenu: [
      {
        label: "Open Markdown...",
        accelerator: "CmdOrCtrl+O",
        click: () => void chooseAndOpenMarkdown(),
      },
      {
        label: "New Window",
        accelerator: "CmdOrCtrl+N",
        click: () => createNewWindow(),
      },
      {
        label: "New Markdown Note...",
        accelerator: "CmdOrCtrl+Shift+N",
        click: () => runInWindow(dispatchCommandScript("new-markdown-note")),
      },
      {
        label: "Delete Current Note",
        accelerator: "CmdOrCtrl+Backspace",
        click: () => runInWindow(dispatchCommandScript("delete-node")),
      },
      { type: "separator" },
      {
        label: "Save",
        accelerator: "CmdOrCtrl+S",
        click: () => runInWindow(dispatchKeyScript("s")),
      },
      {
        label: "Export PDF",
        accelerator: "CmdOrCtrl+P",
        click: () => runInWindow(dispatchKeyScript("p")),
      },
      { type: "separator" },
      {
        label: "Close Window",
        accelerator: "CmdOrCtrl+W",
        click: () => closeCurrentWindow(),
      },
    ],
  },
  {
    label: "Roam",
    submenu: [
      {
        label: "Open Roam Folder",
        click: () => void shell.openPath(noteRoot),
      },
      {
        label: "Open Roam Database",
        click: () => void openRoamDb(),
      },
      { type: "separator" },
      {
        label: "New Roam Note...",
        accelerator: "CmdOrCtrl+Alt+N",
        click: () => runInWindow(dispatchCommandScript("new-roam-node")),
      },
      {
        label: "Open Today's Daily Note",
        click: () => runInWindow(dispatchCommandScript("open-today-daily")),
      },
      {
        label: "Open Roam Node",
        click: () => runInWindow(dispatchCommandScript("open-roam-node")),
      },
      {
        label: "Open Roam Graph",
        click: () => runInWindow(dispatchCommandScript("open-roam-graph")),
      },
      {
        label: "Sync Roam DB",
        click: () => runInWindow(dispatchCommandScript("sync-roamdb")),
      },
      {
        label: "Force Full Refresh Roam DB",
        click: () => runInWindow(dispatchCommandScript("sync-roamdb-full")),
      },
      { type: "separator" },
      {
        label: "Generate or Copy Roam ID",
        click: () => runInWindow(dispatchCommandScript("ensure-roam-id")),
      },
      {
        label: "Insert Roam Idlink",
        click: () => runInWindow(dispatchCommandScript("insert-roam-idlink")),
      },
      {
        label: "Quick Add Meta",
        accelerator: "CmdOrCtrl+Shift+M",
        click: () => runInWindow(dispatchCommandScript("add-meta")),
      },
      {
        label: "Unregister Meta",
        accelerator: "CmdOrCtrl+Shift+U",
        click: () => runInWindow(dispatchCommandScript("remove-meta")),
      },
      {
        label: "Set roam: off for Current Note",
        click: () => runInWindow(dispatchCommandScript("hide-roam")),
      },
      {
        label: "Clear roam: off for Current Note",
        click: () => runInWindow(dispatchCommandScript("activate-roam")),
      },
      {
        label: "Restore Current File from Commit…",
        click: () => runInWindow(dispatchCommandScript("roam-restore-file-version")),
      },
      { type: "separator" },
      {
        label: "Roam Git Log",
        click: () => runInWindow(dispatchCommandScript("roam-git-log")),
      },
      {
        label: "Roam Git Status",
        click: () => runInWindow(dispatchCommandScript("roam-git-status")),
      },
      {
        label: "Commit Roam Now…",
        click: () => runInWindow(dispatchCommandScript("roam-commit-now")),
      },
      {
        label: "Push Roam to Remote",
        click: () => runInWindow(dispatchCommandScript("roam-push")),
      },
      { type: "separator" },
      {
        label: "Insert Inline Tag",
        click: () => runInWindow(dispatchCommandScript("insert-inline-tag")),
      },
      {
        label: "Manage Note Tags",
        click: () => runInWindow(dispatchCommandScript("manage-note-tags")),
      },
      {
        label: "Tag Context",
        accelerator: "CmdOrCtrl+T",
        click: () => runInWindow(dispatchCommandScript("tag-context")),
      },
    ],
  },
  {
    label: "Note",
    submenu: [
      {
        label: "Check Spelling and Prose",
        accelerator: "CmdOrCtrl+Shift+S",
        click: () => runInWindow(dispatchCommandScript("check-prose")),
      },
      {
        label: "Insert Block...",
        accelerator: "Cmd+Enter",
        click: () => runInWindow(dispatchCommandScript("open-block-menu")),
      },
    ],
  },
  {
    label: "Snippets",
    submenu: [
      {
        label: "Reload Snippets",
        accelerator: "CmdOrCtrl+Shift+R",
        click: () => runInWindow(dispatchCommandScript("reload-snippets")),
      },
      {
        label: "Enable Snippet Suggestions",
        click: () => runInWindow(dispatchCommandScript("enable-snippet-suggestions")),
      },
      {
        label: "Disable Snippet Suggestions",
        click: () => runInWindow(dispatchCommandScript("disable-snippet-suggestions")),
      },
      {
        label: "Reset Snippet Suggestions",
        click: () => runInWindow(dispatchCommandScript("reset-snippet-suggestions")),
      },
    ],
  },
  {
    label: "Tools",
    submenu: [
      {
        label: "Plugin Manager",
        click: () => runInWindow(dispatchCommandScript("open-plugin-manager")),
      },
    ],
  },
  {
    label: "Lean",
    submenu: [
      {
        label: `Status: ${leanMenuStatus.busy ? "..." : ""}${leanMenuStatus.message}`,
        enabled: false,
      },
      {
        label: `Kind: ${leanMenuStatus.kind}`,
        enabled: false,
      },
      { type: "separator" },
      {
        label: "Toggle Lean Panel",
        accelerator: "CmdOrCtrl+L",
        click: () => runInWindow(dispatchCommandScript("toggle-lean-panel")),
      },
      {
        label: "Insert Lean Block",
        accelerator: "CmdOrCtrl+Shift+L",
        click: () => runInWindow(dispatchCommandScript("insert-lean-block")),
      },
      {
        label: "Clean Current Lean Block",
        click: () => runInWindow(dispatchCommandScript("clean-lean-block")),
      },
      {
        label: "Restart Lean for Current Note",
        click: () => runInWindow(dispatchCommandScript("restart-lean-server")),
      },
      {
        label: "Stop Lean Server",
        click: async () => {
          await handleLeanRequest("stop");
          updateLeanMenuStatus({ message: "Stopped", kind: "Inactive", busy: false });
        },
      },
      {
        label: "Download Mathlib Cache",
        click: async () => {
          const result = await handleLeanRequest("cache-get");
          updateLeanMenuStatus({
            message: result?.ok === false ? (result.message || "Cache failed") : (result?.message || "Mathlib cache ready"),
            kind: result?.ok === false ? "Error" : "Normal",
            busy: false,
          });
        },
      },
      { type: "separator" },
      {
        label: "Refresh Log",
        click: () => void refreshLeanMenuLog(),
      },
      {
        label: "Show Log Snapshot...",
        click: () => void showLeanLogDialog(),
      },
      { type: "separator" },
      ...(leanMenuLog.length > 0
        ? leanMenuLog.slice(-8).reverse().map((entry) => ({
          label: leanLogMenuLabel(entry),
          enabled: false,
        }))
        : [{ label: "No log entries yet", enabled: false }]),
    ],
  },
  {
    label: "Debug",
    submenu: [
      {
        label: "Open Monitor",
        click: () => void openDebugPanel(),
      },
    ],
  },
  {
    label: "Edit",
    submenu: [
      {
        label: "Undo",
        accelerator: "CmdOrCtrl+Z",
        click: () => runInWindow(dispatchCommandScript("undo")),
      },
      {
        label: "Redo",
        accelerator: process.platform === "darwin" ? "Ctrl+Z" : "CmdOrCtrl+Shift+Z",
        click: () => runInWindow(dispatchCommandScript("redo")),
      },
      { type: "separator" },
      { role: "cut" },
      { role: "copy" },
      { role: "paste" },
      { role: "selectAll" },
    ],
  },
  {
    label: "View",
    submenu: [
      {
        label: "Toggle Source",
        accelerator: "CmdOrCtrl+/",
        // Keep the menu hint, but let the renderer route this by focus:
        // Markdown toggles source mode; an embedded Lean editor toggles comments.
        registerAccelerator: false,
        click: () => runInWindow(dispatchCommandScript("toggle-source")),
      },
      { type: "separator" },
      {
        label: "Reset Zoom",
        accelerator: "CmdOrCtrl+0",
        click: () => setDesiredZoomLevel(DEFAULT_ZOOM_LEVEL),
      },
      {
        label: "Zoom In",
        accelerator: "CmdOrCtrl+=",
        click: () => stepDesiredZoomLevel(ZOOM_STEP),
      },
      {
        label: "Zoom Out",
        accelerator: "CmdOrCtrl+-",
        click: () => stepDesiredZoomLevel(-ZOOM_STEP),
      },
      {
        label: "Toggle TOC",
        accelerator: "Alt+T",
        click: () => runInWindow("document.querySelector('.aaronnote-floating-toc > button')?.click()"),
      },
      { type: "separator" },
      {
        label: "Toggle Full Screen",
        accelerator: "F11",
        click: () => {
          const win = targetAppWindow();
          if (!win) return;
          win.setFullScreen(!win.isFullScreen());
        },
      },
      {
        label: "Reload Current Note",
        accelerator: "CmdOrCtrl+R",
        click: () => void reloadCurrentWindow(),
      },
      { role: "toggleDevTools" },
      { role: "togglefullscreen" },
    ],
  },
  {
    label: "Navigate",
    submenu: [
      {
        label: "Notes",
        click: () => runInWindow("document.querySelector('[data-action=notes]')?.click()"),
      },
      {
        label: "Filesystem",
        accelerator: "Ctrl+Enter",
        click: () => runInWindow(dispatchCommandScript("open-filesystem")),
      },
      {
        label: "Editor",
        click: () => runInWindow("document.querySelector('[data-action=editor],[data-action=editor-inline]')?.click()"),
      },
      {
        label: "Jump Stack",
        accelerator: "CmdOrCtrl+J",
        click: () => runInWindow(dispatchCommandScript("jump-stack")),
      },
      {
        label: "Snippet Next Field",
        accelerator: "CmdOrCtrl+]",
        click: () => runInWindow(dispatchKeyScript("]")),
      },
      {
        label: "Snippet Previous Field",
        accelerator: "CmdOrCtrl+[",
        click: () => runInWindow(dispatchKeyScript("[")),
      },
    ],
  },
]);
}

Menu.setApplicationMenu(buildMenu());

app.on("open-file", (event, file) => {
  event.preventDefault();
  openFileInWindow(file);
});

app.on("second-instance", (_event, argv) => {
  const file = argv.find((arg) => /\.(?:md|markdown)$/i.test(arg));
  if (file) {
    openFileInWindow(file);
    return;
  }
  if (mainWindow && !mainWindow.isDestroyed()) {
    mainWindow.show();
    mainWindow.focus();
    scheduleApplyZoom(mainWindow);
  }
});

app.whenReady().then(async () => {
  configure({
    root: noteRoot,
    workspaceRoot,
    publishJsDir,
    pluginRoot,
  });
  setLeanNotesRoot(noteRoot);
  registerAssetProtocol();
  registerApiIpc();
  registerGlobalShortcuts();
  const win = createWindow();
  registerLeanPushHandlers({
    onDiagnostics: (data) => { if (!win.isDestroyed()) win.webContents.send("aaronnote:lean:diagnostics", data); },
    onProgress: (data) => { if (!win.isDestroyed()) win.webContents.send("aaronnote:lean:progress", data); },
    onSemanticTokens: (data) => { if (!win.isDestroyed()) win.webContents.send("aaronnote:lean:semantic-tokens", data); },
    onNotification: (data) => { if (!win.isDestroyed()) win.webContents.send("aaronnote:lean:notification", data); },
    onClientNotification: (data) => { if (!win.isDestroyed()) win.webContents.send("aaronnote:lean:client-notification", data); },
    onStatus: (data) => {
      updateLeanMenuStatus(data);
      if (!win.isDestroyed()) win.webContents.send("aaronnote:lean:status", data);
    },
    onLog: (entry) => appendLeanMenuLog(entry),
  });
  setTimeout(() => {
    void maybeScheduleWeeklyFullSync().catch((err) => {
      console.error("[roam-sync] weekly full-sync check failed:", err?.message || err);
    });
  }, 30_000);
});

app.on("activate", () => {
  if (BrowserWindow.getAllWindows().length === 0) createWindow();
  else applyZoomToAllWindows();
});

app.on("before-quit", async (event) => {
  if (allowQuit) return;
  event.preventDefault();
  if (!confirmQuit()) return;
  allowQuit = true;
  await Promise.all(BrowserWindow.getAllWindows().map(flushRendererState));
  app.quit();
});

app.on("will-quit", () => {
  void stopJupyterSession();
  globalShortcut.unregisterAll();
});
