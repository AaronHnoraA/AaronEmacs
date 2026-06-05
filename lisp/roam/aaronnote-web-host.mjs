/**
 * Self-contained web host for the Emacs Markdown preview.
 *
 * The browser runtime lives beside this file in aaronnote-preview/. Emacs owns
 * the document and sends preview/reveal messages over HTTP; the page sends
 * click locations back through stdout.
 *
 * Endpoints:
 *   GET  /                     – embedded preview runtime
 *   GET  /sse                  – SSE push channel (server to page)
 *   GET  /note-asset           – current note's local attachment
 *   POST /emacs/command        – preview/reveal messages from Emacs
 *   POST /emacs/event          – goto messages from the page
 *   GET  /health               – liveness
 */

import { createServer } from "node:http";
import { readFile, stat } from "node:fs/promises";
import { dirname, extname, isAbsolute, join, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const previewDir = join(scriptDir, "aaronnote-preview");
const noteRoot = resolve(process.env.AARONNOTE_ROOT || process.cwd());
const bindHost = process.env.AARONNOTE_WEB_HOST || "127.0.0.1";
const bindPort = Number(process.env.AARONNOTE_WEB_PORT || 0);

const sseClients = new Set();
let latestPreview = null;
let latestReveal = null;

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
  return MIME[extname(file).toLowerCase()] || "application/octet-stream";
}

function sendJson(res, status, value) {
  res.writeHead(status, { "Content-Type": "application/json; charset=utf-8" });
  res.end(JSON.stringify(value));
}

function sendSse(res, event, data) {
  const message = `event: ${event}\ndata: ${JSON.stringify(data)}\n\n`;
  res.write(message);
}

function sendToPage(event, data) {
  for (const res of sseClients) {
    try {
      sendSse(res, event, data);
    } catch {
      sseClients.delete(res);
    }
  }
}

function readJson(req, maxBytes = 64 * 1024 * 1024) {
  return new Promise((resolveBody, reject) => {
    let body = "";
    req.on("data", (chunk) => {
      body += chunk;
      if (body.length > maxBytes) reject(new Error("Request body too large"));
    });
    req.on("end", () => {
      try {
        resolveBody(body ? JSON.parse(body) : {});
      } catch (err) {
        reject(err);
      }
    });
    req.on("error", reject);
  });
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

function resolveNoteAsset(source, sourceFile, requestedBase) {
  const raw = cleanAssetSource(source);
  if (!raw || /^(?:data|https?|blob):/i.test(raw)) return null;

  const sourceDir = sourceFile && isAbsolute(sourceFile)
    ? dirname(resolve(sourceFile))
    : null;
  const allowedRoots = [noteRoot, ...(sourceDir ? [sourceDir] : [])];
  const base = requestedBase && isAbsolute(requestedBase)
    && allowedRoots.some((root) => isWithin(root, requestedBase))
    ? resolve(requestedBase)
    : sourceDir || noteRoot;
  const file = isAbsolute(raw) ? resolve(raw) : resolve(base, raw);
  return allowedRoots.some((root) => isWithin(root, file)) ? file : null;
}

function makeAdapter(origin) {
  return `<script>
(function() {
  var BASE = ${JSON.stringify(origin)};

  function noteAssetUrl(source, base) {
    return BASE + "/note-asset?src=" + encodeURIComponent(source || "")
      + (base ? "&base=" + encodeURIComponent(base) : "");
  }

  window.aaronnoteEmacsBridge = {
    sendEvent: function(payload) {
      return fetch(BASE + "/emacs/event", {
        method: "POST",
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify(payload),
      });
    },
  };

  window.AaronnoteResolveAssetUrl = function(source) {
    var raw = String(source || "").trim();
    if (!raw || /^(?:data:|https?:|blob:|#)/i.test(raw)) return raw;
    if (raw.indexOf("aaronnote-asset://media") === 0) {
      try {
        var media = new URL(raw);
        return noteAssetUrl(
          media.searchParams.get("file") || "",
          media.searchParams.get("base") || ""
        );
      } catch (_) {
        return raw;
      }
    }
    return noteAssetUrl(raw, "");
  };

  var originalFetch = window.fetch.bind(window);
  window.fetch = function(input, init) {
    if (typeof input === "string" && input.indexOf("aaronnote-asset://media") === 0) {
      return originalFetch(window.AaronnoteResolveAssetUrl(input), init);
    }
    return originalFetch(input, init);
  };

  var eventSource = new EventSource(BASE + "/sse");
  eventSource.addEventListener("preview", function(event) {
    try {
      var payload = JSON.parse(event.data);
      var editor = window.__aaronoteEditor;
      if (!editor) return;
      var current = editor.view.state.doc.toString();
      var next = String(payload.content || "");
      if (current === next) return;

      var prefix = 0;
      var minimum = Math.min(current.length, next.length);
      while (prefix < minimum && current[prefix] === next[prefix]) prefix++;

      var suffix = 0;
      var maximum = Math.min(current.length - prefix, next.length - prefix);
      while (suffix < maximum
             && current[current.length - 1 - suffix] === next[next.length - 1 - suffix]) {
        suffix++;
      }

      editor.view.dispatch({
        changes: {
          from: prefix,
          to: current.length - suffix,
          insert: next.slice(prefix, next.length - suffix),
        },
      });
    } catch (err) {
      console.error("[emacs-preview] preview update failed", err);
    }
  });

  eventSource.addEventListener("reveal", function(event) {
    try {
      window.dispatchEvent(new CustomEvent("aaronnote:reveal", {
        detail: JSON.parse(event.data),
      }));
    } catch (err) {
      console.error("[emacs-preview] reveal failed", err);
    }
  });

  document.addEventListener("mousedown", function(event) {
    var editor = window.__aaronoteEditor;
    if (!editor || event.button !== 0 || !editor.view.dom.contains(event.target)) return;
    var pos = editor.view.posAtCoords({x: event.clientX, y: event.clientY});
    if (pos == null) return;
    event.preventDefault();
    event.stopImmediatePropagation();
    var line = editor.view.state.doc.lineAt(pos);
    window.aaronnoteEmacsBridge.sendEvent({
      type: "goto",
      line: line.number,
      col: pos - line.from,
    }).catch(function(err) {
      console.error("[emacs-preview] goto failed", err);
    });
  }, true);

  ["beforeinput", "paste", "drop", "keydown", "click", "contextmenu"].forEach(function(name) {
    document.addEventListener(name, function(event) {
      var editor = window.__aaronoteEditor;
      if (!editor || !editor.view.dom.contains(event.target)) return;
      event.preventDefault();
      event.stopImmediatePropagation();
    }, true);
  });
})();
</script>`;
}

async function serveStatic(urlPath, res, origin) {
  const requested = decodeURIComponent(urlPath).replace(/^\/+/, "") || "index.html";
  const file = resolve(previewDir, requested);
  if (!isWithin(previewDir, file) || !(await isFile(file))) {
    res.writeHead(404);
    res.end("Not found");
    return;
  }

  const data = await readFile(file);
  if (file.endsWith("index.html")) {
    const html = data.toString("utf8")
      .replace("</head>", makeAdapter(origin) + "\n</head>");
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

async function serveNoteAsset(url, res) {
  const file = resolveNoteAsset(
    url.searchParams.get("src"),
    latestPreview?.file || "",
    url.searchParams.get("base"),
  );
  if (!file || !(await isFile(file))) {
    res.writeHead(404);
    res.end("Asset not found");
    return;
  }
  const data = await readFile(file);
  res.writeHead(200, {
    "Content-Type": mimeFor(file),
    "Cache-Control": "no-cache",
  });
  res.end(data);
}

const server = createServer(async (req, res) => {
  const url = new URL(req.url, "http://localhost");

  if (url.pathname === "/sse") {
    res.writeHead(200, {
      "Content-Type": "text/event-stream",
      "Cache-Control": "no-cache",
      "Connection": "keep-alive",
    });
    res.write("retry: 2000\n\n");
    sseClients.add(res);
    if (latestPreview) sendSse(res, "preview", latestPreview);
    if (latestReveal) sendSse(res, "reveal", latestReveal);
    req.on("close", () => sseClients.delete(res));
    return;
  }

  if (url.pathname === "/emacs/command" && req.method === "POST") {
    try {
      const message = await readJson(req);
      if (message.type === "preview") {
        latestPreview = {
          content: String(message.content || ""),
          file: String(message.file || ""),
        };
        sendToPage("preview", latestPreview);
      } else if (message.type === "reveal") {
        latestReveal = {
          line: Math.max(1, Math.trunc(Number(message.line) || 1)),
          col: Math.max(0, Math.trunc(Number(message.col) || 0)),
        };
        sendToPage("reveal", latestReveal);
      }
      sendJson(res, 200, { ok: true });
    } catch (err) {
      sendJson(res, 400, { error: String(err) });
    }
    return;
  }

  if (url.pathname === "/emacs/event" && req.method === "POST") {
    try {
      const message = await readJson(req, 1024 * 1024);
      if (message.type !== "goto") throw new Error("Unknown event type");
      const line = Math.max(1, Math.trunc(Number(message.line) || 1));
      const col = Math.max(0, Math.trunc(Number(message.col) || 0));
      process.stdout.write(`aaronote-event:goto:${line}:${col}\n`);
      sendJson(res, 200, { ok: true });
    } catch (err) {
      sendJson(res, 400, { error: String(err) });
    }
    return;
  }

  if (url.pathname === "/note-asset") {
    await serveNoteAsset(url, res);
    return;
  }

  if (url.pathname === "/health") {
    sendJson(res, 200, { ok: true, root: noteRoot, preview: previewDir });
    return;
  }

  const origin = `http://${bindHost}:${server.address()?.port}`;
  await serveStatic(url.pathname, res, origin);
});

server.listen(bindPort, bindHost, () => {
  const port = server.address().port;
  process.stdout.write(`aaronote-web-host:ready:${port}\n`);
  process.stderr.write(`[emacs-markdown-preview] http://${bindHost}:${port}\n`);
});

process.on("SIGTERM", () => { server.close(); process.exit(0); });
process.on("SIGINT", () => { server.close(); process.exit(0); });
