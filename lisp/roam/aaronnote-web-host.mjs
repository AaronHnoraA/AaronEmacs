/**
 * Aaronote headless web-host.
 *
 * Lives in the Emacs config (lisp/roam/); reaches the Aaronote app via the
 * machine-local var/publish symlink → ~/HC/Org, so Aaronnote is at
 * var/publish/Aaronnote/.
 *
 * Serves dist/aaronnote/ with an injected aaronnoteApi web adapter so the
 * full Aaronote app runs inside xwidget-webkit (no Electron required).
 *
 * Endpoints:
 *   POST /api/<ns>/<method>    – mirrors desktop/main.mjs handler table
 *   GET  /aaronnote-asset/...  – aaronnote-asset:// protocol over HTTP
 *   GET  /sse                  – SSE push channel (server → page)
 *   POST /emacs/command        – Emacs control commands forwarded to page
 *   GET  /health               – liveness + port announcement on stdout
 *
 * Environment variables (all optional):
 *   AARONNOTE_ROOT              – notes directory (default: var/publish/Aaronnote/../roam)
 *   AARONNOTE_WORKSPACE_ROOT    – workspace root
 *   AARONNOTE_WEB_PORT          – HTTP port (default 0 = OS-assigned)
 *   AARONNOTE_WEB_HOST          – bind host (default 127.0.0.1)
 */

import { createServer } from "node:http";
import { readFile, stat } from "node:fs/promises";
import { existsSync } from "node:fs";
import { join, resolve, dirname, extname } from "node:path";
import { fileURLToPath } from "node:url";

// Path to the Aaronote app directory, via the var/publish symlink.
const emacsCfgDir = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..");
const aaronoteAppDir = resolve(emacsCfgDir, "var", "publish", "Aaronnote");
const distDir   = join(aaronoteAppDir, "dist", "aaronnote");
const publicDir = join(aaronoteAppDir, "public");

const {
  roamNoteRoot: noteRootValue,
  markNotesDirty,
  bootstrapNote,
  readNote,
  notesIndexPayload,
  graphPayload,
  tagIndexPayload,
  scanNotes,
  saveNote,
  getTodos,
  scanSnippets,
  scanTemplates,
  createNode,
  createFolder,
  deleteNote,
  pathSuggestionsForFile,
  syncRoamDb,
  storeAsset,
  storeAssetFromPath,
  renderTikzAsset,
  scanUnusedAssets,
  trashUnusedAssets,
  readRecentNotes,
  touchRecentNote,
  readCursorPositions,
  touchCursorPosition,
  scanPlugins,
  readPluginOverrides,
  writePluginOverrides,
  renameManagedPath,
  moveManagedPath,
  duplicateManagedFile,
  trashManagedPath,
  updateCurrentNoteMeta,
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
  renameRoamTag,
  deleteRoamTag,
  roamTagOverlapReport,
  rewriteMarkdownPathReferences,
} = await import(join(aaronoteAppDir, "server", "lib", "runtime.mjs"));

const noteRoot = noteRootValue;
const workspaceRoot = resolve(
  process.env.AARONNOTE_WORKSPACE_ROOT || resolve(aaronoteAppDir, "..")
);
const publishJsDir = resolve(
  process.env.AARONNOTE_PUBLISH_JS_DIR || join(workspaceRoot, "js")
);
const pluginRoot = resolve(
  process.env.AARONNOTE_PLUGIN_ROOT || join(workspaceRoot, "plugin")
);

const bindHost = process.env.AARONNOTE_WEB_HOST || "127.0.0.1";
const bindPort = Number(process.env.AARONNOTE_WEB_PORT || 0);

// ── SSE push clients ────────────────────────────────────────────────────────
const sseClients = new Set();

function sendToPage(event, data) {
  const msg = `event: ${event}\ndata: ${JSON.stringify(data)}\n\n`;
  for (const res of sseClients) {
    try { res.write(msg); } catch { sseClients.delete(res); }
  }
}

// ── MIME types ─────────────────────────────────────────────────────────────
const MIME = {
  ".html": "text/html; charset=utf-8",
  ".js":   "application/javascript; charset=utf-8",
  ".mjs":  "application/javascript; charset=utf-8",
  ".css":  "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".svg":  "image/svg+xml",
  ".png":  "image/png",
  ".jpg":  "image/jpeg",
  ".jpeg": "image/jpeg",
  ".gif":  "image/gif",
  ".webp": "image/webp",
  ".woff": "font/woff",
  ".woff2":"font/woff2",
  ".ttf":  "font/ttf",
  ".ico":  "image/x-icon",
};
const mimeFor = p => MIME[extname(p).toLowerCase()] || "application/octet-stream";

// ── Web adapter (injected into index.html) ──────────────────────────────────
function makeAdapter(origin) {
  return `<script>
(function() {
  var BASE = ${JSON.stringify(origin)};

  function api(path) {
    var args = Array.prototype.slice.call(arguments, 1);
    return fetch(BASE + "/api/" + path, {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(args),
    }).then(function(r) { return r.json(); });
  }

  window.aaronnoteApi = {
    notes: {
      bootstrap:    function(f)     { return api("notes/bootstrap", f); },
      open:         function(f)     { return api("notes/open", f); },
      list:         function(force) { return api("notes/list", force); },
      save:         function(b)     { return api("notes/save", b); },
      createNode:   function(d)     { return api("notes/createNode", d); },
      deleteNote:   function(f)     { return api("notes/deleteNote", f); },
      createFolder: function(p)     { return api("notes/createFolder", p); },
      pathSuggestions: function(f)  { return api("notes/pathSuggestions", f); },
      roamSync:     function(r)     { return api("notes/roamSync", r); },
      roamSyncFull: function()      { return api("notes/roamSyncFull"); },
      templates:    function(force) { return api("notes/templates", force); },
      snippets:     function()      { return api("notes/snippets"); },
      todos:        function(f)     { return api("notes/todos", f); },
      metaAdd:      function(b)     { return api("notes/metaAdd", b); },
    },
    roamTools: {
      renameTag:          function(b) { return api("roamTools/renameTag", b); },
      deleteTag:          function(b) { return api("roamTools/deleteTag", b); },
      tagOverlap:         function()  { return api("roamTools/tagOverlap"); },
      rewritePathRefs:    function(b) { return api("roamTools/rewritePathRefs", b); },
      fileHistory:        function(f) { return api("roamTools/fileHistory", f); },
      restoreFileVersion: function(b) { return api("roamTools/restoreFileVersion", b); },
      discardFileChanges: function(f) { return api("roamTools/discardFileChanges", f); },
      repoStatus:         function()  { return api("roamTools/repoStatus"); },
      repoHistory:        function(l) { return api("roamTools/repoHistory", l); },
      changes:            function()  { return api("roamTools/changes"); },
      diff:               function(b) { return api("roamTools/diff", b); },
      commitDiff:         function(s) { return api("roamTools/commitDiff", s); },
      pull:               function()  { return api("roamTools/pull"); },
      push:               function()  { return api("roamTools/push"); },
      commit:             function(m) { return api("roamTools/commit", m); },
    },
    assets: {
      upload:        function(b) { return api("assets/upload", b); },
      storeFromPath: function(b) { return api("assets/storeFromPath", b); },
      renderTikz:    function(b) { return api("assets/renderTikz", b); },
      scanOrphans:   function()  { return api("assets/scanOrphans"); },
      trashOrphans:  function(f) { return api("assets/trashOrphans", f); },
    },
    session: {
      getRecent:    function()         { return api("session/getRecent"); },
      touchRecent:  function(f, at)    { return api("session/touchRecent", f, at); },
      getPositions: function()         { return api("session/getPositions"); },
      savePosition: function(pos)      { return api("session/savePosition", pos); },
    },
    plugins: {
      list:         function()  { return api("plugins/list"); },
      getOverrides: function()  { return api("plugins/getOverrides"); },
      saveOverrides:function(o) { return api("plugins/saveOverrides", o); },
    },
    fs: {
      rename:    function(b) { return api("fs/rename", b); },
      move:      function(b) { return api("fs/move", b); },
      duplicate: function(b) { return api("fs/duplicate", b); },
      trash:     function(b) { return api("fs/trash", b); },
    },
    meta: {
      add:          function(b) { return api("meta/add", b); },
      remove:       function(b) { return api("meta/remove", b); },
      tag:          function(b) { return api("meta/tag", b); },
      hideRoam:     function(b) { return api("meta/hideRoam", b); },
      activateRoam: function(b) { return api("meta/activateRoam", b); },
    },
    shell: {
      showInFolder:         function() { return Promise.resolve(); },
      openPath:             function() { return Promise.resolve(); },
      openDirectory:        function() { return Promise.resolve(); },
      openDirectoryInKitty: function() { return Promise.resolve(); },
      showAttachmentMenu:   function() { return Promise.resolve(); },
      showEditorContextMenu:function() { return Promise.resolve(); },
      showLeanEditorMenu:   function() { return Promise.resolve(); },
      openLeanLocation:     function() { return Promise.resolve({}); },
    },
    externalEditor: { open: function() { return Promise.resolve(); } },
    jupyter: {
      request:      function() { return Promise.resolve(); },
      scroll:       function() { return Promise.resolve(); },
      kernelStatus: function() { return Promise.resolve(null); },
      onStatus:     function() {},
    },
    proseCheck: {
      run:               function() { return Promise.resolve({ diagnostics: [] }); },
      browserSpellcheck: function() { return Promise.resolve({ diagnostics: [] }); },
    },
    copilot: {
      request:  function() { return Promise.resolve(); },
      status:   function() { return Promise.resolve(null); },
      inline:   function() { return Promise.resolve(null); },
      shown:    function() { return Promise.resolve(); },
      accept:   function() { return Promise.resolve(); },
      signIn:   function() { return Promise.resolve(); },
      signOut:  function() { return Promise.resolve(); },
      quota:    function() { return Promise.resolve(null); },
      log:      function() { return Promise.resolve(); },
    },
    lean: {
      request:              function() { return Promise.resolve(); },
      status:               function() { return Promise.resolve(null); },
      openNote:             function() { return Promise.resolve(); },
      changeNote:           function() { return Promise.resolve(); },
      closeNote:            function() { return Promise.resolve(); },
      saveNote:             function() { return Promise.resolve(); },
      deleteNote:           function() { return Promise.resolve(); },
      renameNote:           function() { return Promise.resolve(); },
      getGoals:             function() { return Promise.resolve(null); },
      getTermGoal:          function() { return Promise.resolve(null); },
      getHover:             function() { return Promise.resolve(null); },
      getCompletions:       function() { return Promise.resolve(null); },
      rpcCall:              function() { return Promise.resolve(null); },
      getDefinition:        function() { return Promise.resolve(null); },
      getDiagnostics:       function() { return Promise.resolve([]); },
      lspRequest:           function() { return Promise.resolve(null); },
      lspNotify:            function() { return Promise.resolve(); },
      createRpcSession:     function() { return Promise.resolve(null); },
      closeRpcSession:      function() { return Promise.resolve(); },
      rpcRelease:           function() { return Promise.resolve(); },
      cacheStatus:          function() { return Promise.resolve(null); },
      cacheGet:             function() { return Promise.resolve(null); },
      onDiagnostics:        function() {},
      onProgress:           function() {},
      onSemanticTokens:     function() {},
      onStatus:             function() {},
      onNotification:       function() {},
      onClientNotification: function() {},
    },
    roamlookup: {
      request: function() { return Promise.resolve(); },
      status:  function() { return Promise.resolve(null); },
      start:   function() { return Promise.resolve(); },
      query:   function() { return Promise.resolve(null); },
      close:   function() { return Promise.resolve(); },
    },
  };

  // Rewrite aaronnote-asset:// URLs to HTTP before the app fetches them.
  var _origFetch = window.fetch.bind(window);
  window.fetch = function(input, init) {
    if (typeof input === "string" && input.startsWith("aaronnote-asset://")) {
      var url = new URL(input);
      var rewritten = BASE + "/aaronnote-asset/" + url.hostname + url.pathname
        + (url.search || "");
      return _origFetch(rewritten, init);
    }
    return _origFetch(input, init);
  };

  // SSE connection: server → page push.
  // "command" events → dispatch aaronnote:command (view switches, etc.)
  // "preview" events → apply incremental CM6 diff to the active editor.
  var evtSrc = new EventSource(BASE + "/sse");
  evtSrc.addEventListener("command", function(e) {
    try {
      var detail = JSON.parse(e.data);
      window.dispatchEvent(new CustomEvent("aaronnote:command", { detail: detail }));
    } catch(err) { console.error("[adapter] command parse error", err); }
  });
  evtSrc.addEventListener("preview", function(e) {
    try {
      var payload = JSON.parse(e.data);
      var editor = window.__aaronoteEditor;
      if (!editor) return;
      var cur  = editor.view.state.doc.toString();
      var next = String(payload.content || "");
      if (cur === next) return;
      // Minimal prefix/suffix diff → minimal CM6 change → no flicker.
      var p = 0;
      var minLen = Math.min(cur.length, next.length);
      while (p < minLen && cur[p] === next[p]) p++;
      var s = 0;
      var maxS = Math.min(cur.length - p, next.length - p);
      while (s < maxS && cur[cur.length - 1 - s] === next[next.length - 1 - s]) s++;
      editor.view.dispatch({
        changes: { from: p, to: cur.length - s, insert: next.slice(p, next.length - s) },
      });
    } catch(err) { console.error("[adapter] preview error", err); }
  });
})();
</script>`;
}

// ── API handler table ───────────────────────────────────────────────────────
async function notesListPayload(force = false) {
  if (force) markNotesDirty();
  return { type: "notes", ...await notesIndexPayload(), root: noteRoot };
}

async function roamSyncPayload(reload = false) {
  if (reload) markNotesDirty();
  const notes = await syncRoamDb();
  return { type: "notes", ...await notesIndexPayload(notes), root: noteRoot, db: join(noteRoot, "roam.db") };
}

async function roamSyncFullPayload() {
  markNotesDirty();
  const notes = await syncRoamDb(null, { mode: "full" });
  return { type: "notes", ...await notesIndexPayload(notes), root: noteRoot, db: join(noteRoot, "roam.db") };
}

const apiHandlers = {
  "notes/bootstrap":     ([f])     => bootstrapNote(f || undefined),
  "notes/open":          ([f])     => readNote(f),
  "notes/list":          ([force]) => notesListPayload(force === true),
  "notes/save":          ([b])     => saveNote(b || {}),
  "notes/createNode":    ([d])     => createNode(d || {}),
  "notes/deleteNote":    ([f])     => deleteNote({ file: f }),
  "notes/createFolder":  ([p])     => createFolder({ path: p }),
  "notes/pathSuggestions":async ([f]) => ({
    type: "path-suggestions", paths: await pathSuggestionsForFile(f || ""),
  }),
  "notes/roamSync":      ([r])  => roamSyncPayload(r === true),
  "notes/roamSyncFull":  ([])   => roamSyncFullPayload(),
  "notes/templates":     ([force]) => scanTemplates({ force: force === true }).then(t => ({ type: "templates", templates: t })),
  "notes/snippets":      ([])   => scanSnippets().then(s => ({ type: "snippets", snippets: s })),
  "notes/todos":         ([f])  => getTodos(f || undefined),
  "notes/metaAdd":       ([b])  => updateCurrentNoteMeta(b, "add"),

  "roamTools/renameTag":         ([b]) => renameRoamTag(b),
  "roamTools/deleteTag":         ([b]) => deleteRoamTag(b),
  "roamTools/tagOverlap":        ([])  => roamTagOverlapReport(),
  "roamTools/rewritePathRefs":   ([b]) => rewriteMarkdownPathReferences(b),
  "roamTools/fileHistory":       ([f]) => fileHistory(noteRoot, f),
  "roamTools/restoreFileVersion":([b]) => restoreFileFromCommit(noteRoot, b.file, b.sha),
  "roamTools/discardFileChanges":([f]) => discardFileChanges(noteRoot, f),
  "roamTools/repoStatus":        ([])  => roamRepoStatus(noteRoot),
  "roamTools/repoHistory":       ([l]) => repoHistory(noteRoot, l),
  "roamTools/changes":           ([])  => roamRepoChanges(noteRoot),
  "roamTools/diff":              ([b]) => diffRoamFile(noteRoot, b?.file || b?.path, b),
  "roamTools/commitDiff":        ([s]) => diffRoamCommit(noteRoot, s),
  "roamTools/pull":              ([])  => pullRoam(noteRoot),
  "roamTools/push":              ([])  => pushRoam(noteRoot),
  "roamTools/commit":            ([m]) => import(join(aaronoteAppDir, "server", "lib", "roam-git.mjs")).then(mod => mod.commitRoam(noteRoot, m)),

  "assets/upload":        ([b]) => storeAsset(b),
  "assets/storeFromPath": ([b]) => storeAssetFromPath(b),
  "assets/renderTikz":    ([b]) => renderTikzAsset(b),
  "assets/scanOrphans":   ([])  => scanUnusedAssets(),
  "assets/trashOrphans":  ([f]) => trashUnusedAssets({ files: f }),

  "session/getRecent":    ([])       => readRecentNotes(),
  "session/touchRecent":  ([f, at])  => touchRecentNote(f, at),
  "session/getPositions": ([])       => readCursorPositions(),
  "session/savePosition": ([pos])    => touchCursorPosition(pos),

  "plugins/list":          ([]) => scanPlugins().then(p => ({ type: "plugins", plugins: p, root: pluginRoot })),
  "plugins/getOverrides":  ([]) => readPluginOverrides(),
  "plugins/saveOverrides": ([o]) => writePluginOverrides(o),

  "fs/rename":    ([b]) => renameManagedPath(b),
  "fs/move":      ([b]) => moveManagedPath(b),
  "fs/duplicate": ([b]) => duplicateManagedFile(b),
  "fs/trash":     ([b]) => trashManagedPath(b),

  "meta/add":          ([b]) => updateCurrentNoteMeta(b, "add"),
  "meta/remove":       ([b]) => updateCurrentNoteMeta(b, "remove"),
  "meta/tag":          ([b]) => updateCurrentNoteMeta(b, "tag"),
  "meta/hideRoam":     ([b]) => updateCurrentNoteMeta(b, "hide-roam"),
  "meta/activateRoam": ([b]) => updateCurrentNoteMeta(b, "activate-roam"),

  "graph": ([]) => scanNotes().then(notes => graphPayload(notes)),
  "tags":  ([]) => scanNotes().then(notes => tagIndexPayload(notes)),
};

// ── Aaronnote asset protocol over HTTP ─────────────────────────────────────
async function serveAaronnoteAsset(host, pathname, searchParams, res) {
  try {
    let file;
    if (host === "media") {
      const f    = searchParams.get("file");
      const base = searchParams.get("base");
      if (!f) { res.writeHead(400); res.end("Missing file param"); return; }
      file = resolve(base || noteRoot, decodeURIComponent(f));
    } else if (host === "font") {
      const name = decodeURIComponent(pathname.replace(/^\/+/, ""));
      if (name !== "FZLiuGongQuanKaiShuJF.ttf") {
        res.writeHead(404); res.end("Unknown font"); return;
      }
      const candidates = [
        "/Library/Fonts/FZLiuGongQuanKaiShuJF.ttf",
        join(workspaceRoot, "fonts", name),
      ];
      file = candidates.find(existsSync);
      if (!file) { res.writeHead(404); res.end("Font not found"); return; }
    } else if (host === "kinds") {
      const root      = resolve(workspaceRoot, "kinds");
      const requested = decodeURIComponent(pathname.replace(/^\/+/, ""));
      file = resolve(root, requested);
      if (!file.startsWith(root)) { res.writeHead(403); res.end("Forbidden"); return; }
    } else if (host === "roam-tools") {
      const name = decodeURIComponent(pathname.replace(/^\/+/, ""));
      if (name !== "knowledge.js" && name !== "graph.js") {
        res.writeHead(404); res.end("Unknown roam tool"); return;
      }
      file = resolve(publishJsDir, name);
    } else {
      res.writeHead(404); res.end(`Unknown asset host: ${host}`); return;
    }
    if (!existsSync(file)) { res.writeHead(404); res.end("Not found"); return; }
    const data = await readFile(file);
    res.writeHead(200, { "Content-Type": mimeFor(file), "Cache-Control": "no-cache" });
    res.end(data);
  } catch (err) {
    res.writeHead(500); res.end(String(err));
  }
}

// ── Static file server ──────────────────────────────────────────────────────
async function serveStatic(urlPath, res, origin) {
  try {
    const cleaned = urlPath.replace(/[?#].*$/, "").replace(/^\/+/, "");
    let file = join(distDir, cleaned || "index.html");
    const isFile = async p => {
      try { return (await stat(p)).isFile(); } catch { return false; }
    };
    if (!(await isFile(file))) {
      const pub = join(publicDir, cleaned);
      file = (await isFile(pub)) ? pub : join(distDir, "index.html");
    }
    let data = await readFile(file);
    if (file.endsWith("index.html")) {
      const html = data.toString("utf8").replace("</head>", makeAdapter(origin) + "\n</head>");
      res.writeHead(200, { "Content-Type": "text/html; charset=utf-8", "Cache-Control": "no-cache" });
      res.end(html);
    } else {
      res.writeHead(200, { "Content-Type": mimeFor(file), "Cache-Control": "public, max-age=86400" });
      res.end(data);
    }
  } catch (err) {
    res.writeHead(500); res.end(String(err));
  }
}

// ── HTTP server ─────────────────────────────────────────────────────────────
const server = createServer(async (req, res) => {
  res.setHeader("Access-Control-Allow-Origin", "*");
  const url  = new URL(req.url, "http://localhost");
  const path = url.pathname;

  // SSE push channel (server → page).
  if (path === "/sse") {
    res.writeHead(200, {
      "Content-Type":  "text/event-stream",
      "Cache-Control": "no-cache",
      "Connection":    "keep-alive",
    });
    res.write("retry: 2000\n\n");
    sseClients.add(res);
    req.on("close", () => sseClients.delete(res));
    return;
  }

  // Emacs control: POST /emacs/command  { type, ... }
  if (path === "/emacs/command" && req.method === "POST") {
    let body = "";
    req.on("data", d => { body += d; });
    req.on("end", () => {
      try {
        const msg = JSON.parse(body);
        if (msg.type === "command") {
          sendToPage("command", { command: msg.command, ...msg.detail });
        } else if (msg.type === "preview") {
          sendToPage("preview", { content: msg.content });
        }
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ ok: true }));
      } catch (err) {
        res.writeHead(400, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ error: String(err) }));
      }
    });
    return;
  }

  // Health / liveness.
  if (path === "/health") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ ok: true, root: noteRoot }));
    return;
  }

  // aaronnote-asset:// protocol rewritten to HTTP.
  if (path.startsWith("/aaronnote-asset/")) {
    const rest     = path.slice("/aaronnote-asset/".length);
    const slashIdx = rest.indexOf("/");
    const host     = slashIdx === -1 ? rest : rest.slice(0, slashIdx);
    const assetPth = slashIdx === -1 ? "/" : rest.slice(slashIdx);
    await serveAaronnoteAsset(host, assetPth, url.searchParams, res);
    return;
  }

  // API: POST /api/<ns>/<method>
  if (path.startsWith("/api/") && req.method === "POST") {
    const key     = path.slice("/api/".length);
    const handler = apiHandlers[key];
    if (!handler) {
      res.writeHead(404, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ error: `Unknown API: ${key}` }));
      return;
    }
    let body = "";
    req.on("data", d => { body += d; });
    req.on("end", async () => {
      try {
        const args   = body ? JSON.parse(body) : [];
        const result = await handler(Array.isArray(args) ? args : [args]);
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify(result ?? null));
      } catch (err) {
        res.writeHead(err.statusCode || 500, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ error: err.message || String(err) }));
      }
    });
    return;
  }

  // Static files (dist/aaronnote + public/).
  const origin = `http://${bindHost}:${server.address()?.port}`;
  await serveStatic(path, res, origin);
});

server.listen(bindPort, bindHost, () => {
  const port = server.address().port;
  // Announce ready on stdout so Emacs can capture the port.
  process.stdout.write(`aaronote-web-host:ready:${port}\n`);
  process.stderr.write(`[aaronote-web-host] http://${bindHost}:${port}  root=${noteRoot}\n`);
});

process.on("SIGTERM", () => { server.close(); process.exit(0); });
process.on("SIGINT",  () => { server.close(); process.exit(0); });
