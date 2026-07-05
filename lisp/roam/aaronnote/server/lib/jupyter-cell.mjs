import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import { existsSync } from "node:fs";
import { mkdir, readFile, stat, writeFile } from "node:fs/promises";
import { basename, dirname, extname, join, resolve, sep } from "node:path";

function inside(root, file) {
  const normalizedRoot = resolve(root);
  const normalizedFile = resolve(file);
  return normalizedFile === normalizedRoot
    || normalizedFile.startsWith(normalizedRoot + sep);
}

function error(message, statusCode = 500) {
  const err = new Error(message);
  err.statusCode = statusCode;
  return err;
}

function delay(ms) {
  return new Promise((resolveDelay) => setTimeout(resolveDelay, ms));
}

function jsonHeaders() {
  return { Accept: "application/json", "Content-Type": "application/json" };
}

function splitSetCookieHeader(value) {
  if (!value) return [];
  return String(value).split(/,(?=\s*[^;,=\s]+=[^;,]+)/g).map((item) => item.trim()).filter(Boolean);
}

function cookiePair(setCookieValue) {
  return String(setCookieValue || "").split(";", 1)[0].trim();
}

function cookieName(pair) {
  const index = String(pair || "").indexOf("=");
  return index > 0 ? pair.slice(0, index) : "";
}

class JupyterHttpClient {
  constructor(baseUrl) {
    this.baseUrl = String(baseUrl || "").replace(/\/+$/, "");
    this.cookies = new Map();
    this.xsrfToken = "";
  }

  rememberCookies(response) {
    const getSetCookie = response.headers?.getSetCookie;
    const values = typeof getSetCookie === "function"
      ? getSetCookie.call(response.headers)
      : splitSetCookieHeader(response.headers?.get?.("set-cookie"));
    for (const value of values || []) {
      const pair = cookiePair(value);
      const name = cookieName(pair);
      if (!name) continue;
      this.cookies.set(name, pair);
      if (name === "_xsrf") {
        this.xsrfToken = decodeURIComponent(pair.slice(name.length + 1));
      }
    }
  }

  cookieHeader() {
    return Array.from(this.cookies.values()).join("; ");
  }

  async refreshXsrf() {
    const response = await fetch(`${this.baseUrl}/`, {
      headers: this.cookieHeader() ? { Cookie: this.cookieHeader() } : {},
    });
    this.rememberCookies(response);
    if (!this.xsrfToken) {
      const fallback = await fetch(`${this.baseUrl}/api/status`, {
        headers: this.cookieHeader() ? { Cookie: this.cookieHeader() } : {},
      });
      this.rememberCookies(fallback);
    }
  }

  async request(path, options = {}, retried = false) {
    const method = String(options.method || "GET").toUpperCase();
    const mutating = !["GET", "HEAD", "OPTIONS"].includes(method);
    if (mutating && !this.xsrfToken) await this.refreshXsrf();
    const headers = {
      ...jsonHeaders(),
      ...(this.cookieHeader() ? { Cookie: this.cookieHeader() } : {}),
      ...(mutating && this.xsrfToken ? { "X-XSRFToken": this.xsrfToken } : {}),
      ...(options.headers || {}),
    };
    const response = await fetch(`${this.baseUrl}${path}`, {
      ...options,
      headers,
      body: options.body && typeof options.body !== "string"
        ? JSON.stringify(options.body)
        : options.body,
    });
    this.rememberCookies(response);
    if (!response.ok) {
      const text = await response.text().catch(() => "");
      if (!retried && mutating && /_xsrf|xsrf/i.test(text)) {
        this.xsrfToken = "";
        await this.refreshXsrf();
        return this.request(path, options, true);
      }
      throw error(text || `Jupyter request failed: ${response.status}`, response.status);
    }
    return response;
  }

  async json(path, options = {}) {
    const response = await this.request(path, options);
    if (response.status === 204) return {};
    return await response.json().catch(() => ({}));
  }
}

function wsDataToString(data) {
  if (typeof data === "string") return data;
  if (data instanceof ArrayBuffer) return Buffer.from(data).toString("utf8");
  if (ArrayBuffer.isView(data)) return Buffer.from(data.buffer, data.byteOffset, data.byteLength).toString("utf8");
  return String(data || "");
}

function cleanToken(value, fallback) {
  const clean = String(value || "").trim();
  return clean || fallback;
}

function safeSlug(value, fallback = "cell") {
  const clean = String(value || "")
    .trim()
    .replace(/^\.+/, "")
    .replace(/[^\p{L}\p{N}._-]+/gu, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 90);
  return clean || fallback;
}

function markerId(value) {
  return String(value || "").trim().replace(/\s+/g, "-");
}

function languageForKernel(kernel, requested = "") {
  const explicit = String(requested || "").trim().toLowerCase();
  const value = String(kernel || "").toLowerCase();
  if (value.includes("lean") || explicit === "lean" || explicit === "lean4") return "lean4";
  if (["bash", "sh", "shell", "zsh"].includes(explicit)) return "bash";
  if (explicit) return explicit;
  if (value.includes("sage")) return "python";
  if (value.includes("python") || value === "py" || value === "python3") return "python";
  if (value.includes("julia")) return "julia";
  if (value === "r" || value.startsWith("ir")) return "r";
  if (value.includes("bash") || value.includes("zsh") || value.includes("shell")) return "bash";
  if (value.includes("javascript") || value === "js" || value.includes("node")) return "javascript";
  if (value.includes("typescript") || value === "ts") return "typescript";
  return "python";
}

function extensionForLanguage(language) {
  const map = {
    bash: "sh",
    c: "c",
    cpp: "cpp",
    csharp: "cs",
    elisp: "el",
    javascript: "js",
    julia: "jl",
    lean: "lean",
    lean4: "lean",
    lisp: "lisp",
    python: "py",
    r: "R",
    ruby: "rb",
    rust: "rs",
    sage: "py",
    scheme: "scm",
    shell: "sh",
    sql: "sql",
    typescript: "ts",
  };
  return map[String(language || "").toLowerCase()] || "txt";
}

function commentPrefix(language) {
  const value = String(language || "").toLowerCase();
  if (["javascript", "typescript", "c", "cpp", "java", "rust", "go", "swift", "kotlin", "csharp"].includes(value)) return "//";
  if (value === "sql") return "--";
  if (value === "lean" || value === "lean4") return "--";
  if (["elisp", "lisp", "scheme", "clojure"].includes(value)) return ";";
  return "#";
}

function cellStoreDir(noteFile) {
  return join(dirname(noteFile), ".cell");
}

function hiddenScriptPath(noteFile, session, language) {
  const noteExt = extname(noteFile);
  const noteBase = safeSlug(basename(noteFile, noteExt), "note");
  const safeLanguage = safeSlug(language, "python");
  const safeSession = safeSlug(session, "default");
  const ext = extensionForLanguage(language);
  return join(cellStoreDir(noteFile), `${noteBase}.${safeLanguage}.${safeSession}.${ext}`);
}

function outputMirrorPath(noteFile, session, language) {
  const noteExt = extname(noteFile);
  const noteBase = safeSlug(basename(noteFile, noteExt), "note");
  const safeLanguage = safeSlug(language, "python");
  const safeSession = safeSlug(session, "default");
  return join(cellStoreDir(noteFile), `${noteBase}.output.${safeLanguage}.${safeSession}.json`);
}

function normalizeCode(value) {
  return String(value ?? "").replace(/\r\n/g, "\n").replace(/\r/g, "\n");
}

function leanRuntimeP(language, kernel) {
  return /lean/i.test(String(language || "")) || /lean/i.test(String(kernel || ""));
}

function durationFromEnv(name, fallback) {
  const value = Number(process.env[name] || "");
  return Number.isFinite(value) && value >= 0 ? value : fallback;
}

function isoTime(value) {
  return value ? new Date(value).toISOString() : "";
}

function parseHiddenScriptCells(text) {
  const lines = normalizeCode(text).split("\n");
  const cells = new Map();
  let current = null;
  let body = [];
  const startRe = /^\s*(?:\/\/|--|#|;)\s*%%\s+aaronnote-cell\s+id=([^\s]+)\s*$/;
  const endRe = /^\s*(?:\/\/|--|#|;)\s*%%\s+end-aaronnote-cell\s+id=([^\s]+)\s*$/;
  for (const line of lines) {
    const start = startRe.exec(line);
    if (start) {
      current = markerId(start[1]);
      body = [];
      continue;
    }
    const end = endRe.exec(line);
    if (end && current && markerId(end[1]) === current) {
      cells.set(current, body.join("\n").replace(/\n$/, ""));
      current = null;
      body = [];
      continue;
    }
    if (current) body.push(line);
  }
  return cells;
}

async function readExistingHiddenCells(scriptFile) {
  try {
    return parseHiddenScriptCells(await readFile(scriptFile, "utf8"));
  } catch (err) {
    if (err?.code === "ENOENT") return new Map();
    throw err;
  }
}

async function readOutputMirror(file) {
  try {
    const parsed = JSON.parse(await readFile(file, "utf8"));
    return parsed && typeof parsed === "object" ? parsed : {};
  } catch (err) {
    if (err?.code === "ENOENT") return {};
    throw err;
  }
}

async function writeOutputMirror(file, value) {
  await mkdir(dirname(file), { recursive: true });
  await writeFile(file, `${JSON.stringify(value, null, 2)}\n`, "utf8");
}

function buildHiddenScript({ noteFile, kernel, session, language, cells, targetCellId, storage = "markdown", existingCells = new Map() }) {
  const prefix = commentPrefix(language);
  const leanRuntime = leanRuntimeP(language, kernel);
  const lines = [
    `${prefix} Aaronnote cell source: ${noteFile}`,
    `${prefix} Aaronnote cell kernel: ${kernel}`,
    `${prefix} Aaronnote cell session: ${session}`,
    `${prefix} Aaronnote cell storage: ${storage}`,
    leanRuntime
      ? `${prefix} Aaronnote Lean cell source; edit cell bodies between markers.`
      : `${prefix} Aaronnote Jupyter cell script; edit cell bodies between markers.`,
    "",
  ];
  let targetLine = 1;
  for (const cell of cells) {
    const id = markerId(cell.cellId || cell.id);
    if (!id) continue;
    lines.push(`${prefix} %% aaronnote-cell id=${id}`);
    if (id === targetCellId) targetLine = lines.length + 1;
    const incoming = normalizeCode(cell.code);
    const code = incoming.trim() ? incoming : (existingCells.get(id) ?? incoming);
    const codeLines = normalizeCode(code).split("\n");
    lines.push(...codeLines);
    lines.push(`${prefix} %% end-aaronnote-cell id=${id}`);
    lines.push("");
  }
  return { text: `${lines.join("\n").replace(/\s*$/, "")}\n`, line: targetLine };
}

export function createJupyterCellService({
  runtimeRoot,
  noteRoot,
  workspaceRoot,
  stdout = process.stdout,
  stderr = process.stderr,
} = {}) {
  const root = resolve(runtimeRoot || process.cwd());
  const notes = resolve(noteRoot || root);
  const workspace = resolve(workspaceRoot || notes);
  const jupyterRoot = join(root, "jupyter");
  const runScript = join(jupyterRoot, "scripts", "run-jupyter-server.sh");
  const host = process.env.AARONNOTE_JUPYTER_HOST || "127.0.0.1";
  const port = Number(process.env.AARONNOTE_JUPYTER_PORT || 8890);
  const baseUrl = process.env.AARONNOTE_JUPYTER_URL || `http://${host}:${port}`;
  const wsBaseUrl = baseUrl.replace(/^http/i, "ws");
  const http = new JupyterHttpClient(baseUrl);
  const kernelsByKey = new Map();
  const kernelIdleTtlMs = durationFromEnv("AARONNOTE_JUPYTER_KERNEL_IDLE_TTL_MS", 10 * 60 * 1000);
  const serverIdleTtlMs = durationFromEnv("AARONNOTE_JUPYTER_SERVER_IDLE_TTL_MS", 90 * 1000);
  const cleanupIntervalMs = durationFromEnv("AARONNOTE_JUPYTER_CLEANUP_INTERVAL_MS", 30 * 1000);
  const execTimeoutMs = durationFromEnv("AARONNOTE_JUPYTER_EXEC_TIMEOUT_MS", 0);
  let serverProcess = null;
  let startPromise = null;
  let serverStartedAt = 0;
  let lastServerUseAt = 0;
  let activeRequests = 0;
  let cleanupTimer = null;
  let cleanupRunning = false;

  function cleanupNeeded() {
    return Boolean(serverProcess) || kernelsByKey.size > 0;
  }

  function cancelCleanupTimer() {
    if (!cleanupTimer) return;
    clearTimeout(cleanupTimer);
    cleanupTimer = null;
  }

  function scheduleCleanup() {
    if (cleanupTimer || !cleanupNeeded()) return;
    cleanupTimer = setTimeout(() => {
      cleanupTimer = null;
      void cleanupIdle({ scheduled: true }).catch((err) => {
        stderr.write(`[aaronnote-jupyter] cleanup failed: ${err?.message || err}\n`);
      });
    }, Math.max(1000, cleanupIntervalMs));
    cleanupTimer.unref?.();
  }

  function touchServer() {
    lastServerUseAt = Date.now();
    scheduleCleanup();
  }

  async function withActiveRequest(run) {
    activeRequests += 1;
    touchServer();
    try {
      return await run();
    } finally {
      activeRequests = Math.max(0, activeRequests - 1);
      touchServer();
    }
  }

  function touchKernel(record) {
    if (!record) return;
    const now = Date.now();
    record.lastUsedAt = now;
    record.lastActivityAt = now;
    scheduleCleanup();
  }

  function kernelRecordForBody(body) {
    const explicitKey = String(body?.key || "").trim();
    if (explicitKey && kernelsByKey.has(explicitKey)) return { key: explicitKey, record: kernelsByKey.get(explicitKey) };
    const id = String(body?.id || body?.kernelId || "").trim();
    if (id) {
      for (const [key, record] of kernelsByKey.entries()) {
        if (record?.id === id) return { key, record };
      }
    }
    const fileValue = String(body?.file || "").trim();
    if (!fileValue) return { key: "", record: null };
    const file = safeNoteFile(fileValue);
    const kernel = cleanToken(body?.kernel, "python3");
    const session = cleanToken(body?.session, "default");
    const key = kernelKey({ file, kernel, session });
    return { key, record: kernelsByKey.get(key) || null };
  }

  function kernelTask(key, record) {
    const now = Date.now();
    const running = Math.max(0, Number(record?.running || 0));
    return {
      key,
      id: record?.id || "",
      file: record?.file || "",
      kernel: record?.kernel || "",
      session: record?.session || "",
      status: running > 0 ? "running" : (record?.lastStatus || "idle"),
      running,
      createdAt: record?.createdAt || 0,
      createdAtIso: isoTime(record?.createdAt),
      lastUsedAt: record?.lastUsedAt || 0,
      lastUsedAtIso: isoTime(record?.lastUsedAt),
      lastActivityAt: record?.lastActivityAt || 0,
      lastActivityAtIso: isoTime(record?.lastActivityAt),
      idleMs: Math.max(0, now - Number(record?.lastUsedAt || now)),
      runningMs: running > 0 ? Math.max(0, now - Number(record?.executionStartedAt || now)) : 0,
      totalRuns: Number(record?.totalRuns || 0),
      executionCount: record?.executionCount ?? null,
      lastCellId: record?.lastCellId || "",
      lastError: record?.lastError || "",
      protected: running > 0,
      ttlMs: kernelIdleTtlMs,
    };
  }

  function safeNoteFile(raw) {
    const value = String(raw || "").trim();
    if (!value) throw error("Missing note file", 400);
    const file = resolve(value);
    const ext = extname(file).toLowerCase();
    const markdownLike = ext === ".md" || ext === ".markdown" || ext === ".mdown" || ext === ".mkd";
    if (!inside(notes, file) && !inside(workspace, file) && !markdownLike) {
      throw error(`Note file is outside the allowed root: ${file}`, 403);
    }
    return file;
  }

  async function fetchJson(path, options = {}) {
    return http.json(path, options);
  }

  async function serverReady() {
    try {
      const response = await fetch(`${baseUrl}/api/status`);
      return response.ok;
    } catch {
      return false;
    }
  }

  async function waitForServer(timeoutMs = 20000) {
    const started = Date.now();
    while (Date.now() - started < timeoutMs) {
      if (await serverReady()) return true;
      await delay(250);
    }
    return false;
  }

  async function ensureServer() {
    if (await serverReady()) {
      touchServer();
      return;
    }
    kernelsByKey.clear();
    if (startPromise) return await startPromise;
    startPromise = (async () => {
      if (!existsSync(runScript)) {
        throw error(`Aaronnote Jupyter launcher not found: ${runScript}`, 500);
      }
      if (!serverProcess || serverProcess.exitCode != null) {
        serverProcess = spawn(runScript, [workspace], {
          cwd: root,
          env: {
            ...process.env,
            AARONNOTE_JUPYTER_HOST: host,
            AARONNOTE_JUPYTER_PORT: String(port),
            AARONNOTE_JUPYTER_USE_HOME_KERNELS: process.env.AARONNOTE_JUPYTER_USE_HOME_KERNELS || "1",
          },
          stdio: ["ignore", "pipe", "pipe"],
        });
        serverProcess.stdout?.on("data", (chunk) => stderr.write(`[aaronnote-jupyter] ${chunk}`));
        serverProcess.stderr?.on("data", (chunk) => stderr.write(`[aaronnote-jupyter] ${chunk}`));
        serverStartedAt = Date.now();
        serverProcess.on("exit", () => {
          serverProcess = null;
          serverStartedAt = 0;
          kernelsByKey.clear();
          scheduleCleanup();
        });
      }
      if (!(await waitForServer())) {
        throw error("Aaronnote Jupyter did not become ready. Run npm run jupyter:bootstrap if it is not installed.", 503);
      }
      touchServer();
    })();
    try {
      await startPromise;
    } finally {
      startPromise = null;
    }
  }

  function kernelKey({ file, kernel, session }) {
    return `${resolve(file)}\0${kernel}\0${session}`;
  }

  async function ensureKernel(body) {
    await ensureServer();
    const file = safeNoteFile(body?.file);
    const kernel = cleanToken(body?.kernel, "python3");
    const session = cleanToken(body?.session, "default");
    const key = kernelKey({ file, kernel, session });
    const existing = kernelsByKey.get(key);
    if (existing?.id) {
      touchKernel(existing);
      return { ...existing, file, kernel, session, key };
    }
    const created = await fetchJson("/api/kernels", {
      method: "POST",
      body: { name: kernel },
    });
    const id = String(created.id || "");
    if (!id) throw error("Jupyter did not return a kernel id", 502);
    const now = Date.now();
    const next = {
      id,
      file,
      kernel,
      session,
      createdAt: now,
      lastUsedAt: now,
      lastActivityAt: now,
      running: 0,
      totalRuns: 0,
      executionCount: null,
      executionStartedAt: 0,
      lastCellId: "",
      lastStatus: "idle",
      lastError: "",
    };
    kernelsByKey.set(key, next);
    scheduleCleanup();
    return { ...next, key };
  }

  async function kernels() {
    return await withActiveRequest(async () => {
      await ensureServer();
      const result = await fetchJson("/api/kernelspecs");
      const specs = result.kernelspecs && typeof result.kernelspecs === "object" ? result.kernelspecs : {};
      const kernels = Object.entries(specs).map(([name, spec]) => ({
        name,
        displayName: spec?.spec?.display_name || spec?.display_name || name,
        language: spec?.spec?.language || "",
      }));
      if (!kernels.some((item) => item.name === "lean4")) {
        kernels.push({ name: "lean4", displayName: "Lean 4", language: "lean4" });
      }
      return {
        ok: true,
        default: result.default || "python3",
        kernels: kernels.sort((a, b) => a.name.localeCompare(b.name)),
      };
    });
  }

  async function execute(body) {
    const code = normalizeCode(body?.code);
    const cellId = String(body?.cellId || body?.id || "");
    const requestedKernel = cleanToken(body?.kernel, "python3");
    const requestedLanguage = languageForKernel(requestedKernel, body?.language || body?.lang);
    if (leanRuntimeP(requestedLanguage, requestedKernel)) {
      return {
        ok: true,
        cellId,
        kernel: requestedKernel,
        session: cleanToken(body?.session, "default"),
        status: "ok",
        executionCount: null,
        outputs: [],
        runtime: "lean4",
      };
    }
    if (!code.trim()) return { ok: true, status: "ok", outputs: [], executionCount: null, cellId };
    return await withActiveRequest(async () => {
      const kernelInfo = await ensureKernel(body || {});
      const record = kernelsByKey.get(kernelInfo.key);
      if (record) {
        const now = Date.now();
        record.running = Math.max(0, Number(record.running || 0)) + 1;
        record.totalRuns = Number(record.totalRuns || 0) + 1;
        record.executionStartedAt = now;
        record.lastUsedAt = now;
        record.lastActivityAt = now;
        record.lastCellId = cellId;
        record.lastStatus = "running";
        record.lastError = "";
      }
      const ws = new WebSocket(`${wsBaseUrl}/api/kernels/${encodeURIComponent(kernelInfo.id)}/channels?session_id=${encodeURIComponent(randomUUID())}`);
      const msgId = randomUUID();
      const sessionId = randomUUID();
      const outputs = [];
      const displayIndexes = new Map();
      let executionCount = null;
      let shellReply = null;
      let idle = false;
      let clearOnNext = false;
      let settled = false;

      const finalizeRecord = (status, err = "") => {
        if (!record) return;
        record.running = Math.max(0, Number(record.running || 0) - 1);
        record.executionStartedAt = record.running > 0 ? record.executionStartedAt : 0;
        record.executionCount = executionCount ?? record.executionCount ?? null;
        record.lastStatus = status || (record.running > 0 ? "running" : "idle");
        record.lastError = err;
        touchKernel(record);
      };

      const finishIfDone = (resolveDone) => {
        if (!shellReply || !idle) return;
        try { ws.close(); } catch {}
        finalizeRecord(shellReply.status || "ok");
        resolveDone({
          ok: true,
          cellId,
          kernel: kernelInfo.kernel,
          session: kernelInfo.session,
          status: shellReply.status || "ok",
          executionCount,
          outputs,
        });
      };

      return await new Promise((resolveDone, rejectDone) => {
        const timeout = execTimeoutMs > 0 ? setTimeout(() => {
          fail(error("Jupyter execution timed out", 504));
        }, execTimeoutMs) : null;

        const done = (value) => {
          if (settled) return;
          settled = true;
          if (timeout) clearTimeout(timeout);
          resolveDone(value);
        };
        const fail = (err) => {
          if (settled) return;
          settled = true;
          if (timeout) clearTimeout(timeout);
          try { ws.close(); } catch {}
          finalizeRecord("error", err?.message || String(err || ""));
          rejectDone(err);
        };

        ws.addEventListener("open", () => {
          touchKernel(record);
          ws.send(JSON.stringify({
            header: {
              msg_id: msgId,
              username: "aaronnote",
              session: sessionId,
              msg_type: "execute_request",
              version: "5.3",
              date: new Date().toISOString(),
            },
            parent_header: {},
            metadata: {},
            content: {
              code,
              silent: Boolean(body?.silent),
              store_history: body?.storeHistory === false ? false : true,
              user_expressions: {},
              allow_stdin: false,
              stop_on_error: false,
            },
            channel: "shell",
            buffers: [],
          }));
        });
        ws.addEventListener("error", () => fail(error("Jupyter websocket failed", 502)));
        ws.addEventListener("close", () => {
          if (!settled && (!shellReply || !idle)) fail(error("Jupyter websocket closed", 502));
        });
        ws.addEventListener("message", (event) => {
          let message;
          try {
            message = JSON.parse(wsDataToString(event.data));
          } catch {
            return;
          }
          const parentId = message?.parent_header?.msg_id;
          if (parentId !== msgId) return;
          touchKernel(record);
          const channel = message.channel || "";
          const type = message.header?.msg_type || "";
          const content = message.content || {};
          if (channel === "shell" && type === "execute_reply") {
            shellReply = content;
            finishIfDone(done);
            return;
          }
          if (channel !== "iopub") return;
          if (type === "status" && content.execution_state === "idle") {
            idle = true;
            finishIfDone(done);
            return;
          }
          if (type === "execute_input") {
            executionCount = content.execution_count ?? executionCount;
            if (record) record.executionCount = executionCount;
            return;
          }
          const pushOutput = (output) => {
            if (clearOnNext) {
              outputs.length = 0;
              displayIndexes.clear();
              clearOnNext = false;
            }
            const displayId = output?.transient?.display_id;
            if (displayId && output.output_type === "update_display_data" && displayIndexes.has(displayId)) {
              outputs[displayIndexes.get(displayId)] = { ...output, output_type: "display_data" };
              return;
            }
            if (displayId) displayIndexes.set(displayId, outputs.length);
            outputs.push(output);
          };
          if (type === "stream") {
            pushOutput({ output_type: "stream", name: content.name || "stdout", text: content.text || "" });
          } else if (type === "execute_result") {
            executionCount = content.execution_count ?? executionCount;
            if (record) record.executionCount = executionCount;
            pushOutput({
              output_type: "execute_result",
              execution_count: content.execution_count ?? null,
              data: content.data || {},
              metadata: content.metadata || {},
            });
          } else if (type === "display_data" || type === "update_display_data") {
            pushOutput({
              output_type: type,
              data: content.data || {},
              metadata: content.metadata || {},
              transient: content.transient || {},
            });
          } else if (type === "error") {
            pushOutput({
              output_type: "error",
              ename: content.ename || "",
              evalue: content.evalue || "",
              traceback: Array.isArray(content.traceback) ? content.traceback : [],
            });
          } else if (type === "clear_output") {
            if (content.wait) clearOnNext = true;
            else {
              outputs.length = 0;
              displayIndexes.clear();
            }
          }
        });
      });
    });
  }

  async function openScript(body) {
    const noteFile = safeNoteFile(body?.file);
    const kernel = cleanToken(body?.kernel, "python3");
    const session = cleanToken(body?.session, "default");
    const language = languageForKernel(kernel, body?.language || body?.lang);
    const targetCellId = markerId(body?.cellId || body?.id);
    const storage = cleanToken(body?.storage, "markdown") === "script" ? "script" : "markdown";
    const cells = Array.isArray(body?.cells) ? body.cells : [];
    if (!targetCellId) throw error("Missing Jupyter cell id", 400);
    if (cells.length === 0) throw error("No Jupyter cells to write", 400);
    const scriptFile = hiddenScriptPath(noteFile, session, language);
    const existingCells = await readExistingHiddenCells(scriptFile);
    const rendered = buildHiddenScript({ noteFile, kernel, session, language, cells, targetCellId, storage, existingCells });
    await mkdir(dirname(scriptFile), { recursive: true });
    let changed = true;
    try {
      changed = await readFile(scriptFile, "utf8") !== rendered.text;
    } catch (err) {
      if (err?.code !== "ENOENT") throw err;
    }
    if (changed) await writeFile(scriptFile, rendered.text, "utf8");
    const info = await stat(scriptFile);
    const payload = { file: scriptFile, line: rendered.line, col: 0, nonce: randomUUID() };
    if (body?.open !== false) {
      stdout.write(`aaronote-event:open:${JSON.stringify(payload)}\n`);
    }
    return {
      ok: true,
      ...payload,
      kernel,
      session,
      language,
      changed,
      mtimeMs: info.mtimeMs,
      size: info.size,
    };
  }

  async function readScriptCell(body) {
    const noteFile = safeNoteFile(body?.file);
    const kernel = cleanToken(body?.kernel, "python3");
    const session = cleanToken(body?.session, "default");
    const language = languageForKernel(kernel, body?.language || body?.lang);
    const cellId = markerId(body?.cellId || body?.id);
    if (!cellId) throw error("Missing Jupyter cell id", 400);
    const scriptFile = hiddenScriptPath(noteFile, session, language);
    const outputFile = outputMirrorPath(noteFile, session, language);
    const cells = await readExistingHiddenCells(scriptFile);
    const outputs = await readOutputMirror(outputFile);
    let info = null;
    try { info = await stat(scriptFile); } catch {}
    return {
      ok: true,
      file: scriptFile,
      kernel,
      session,
      language,
      cellId,
      code: cells.get(cellId) ?? "",
      output: outputs?.cells?.[cellId] ?? null,
      exists: Boolean(info),
      mtimeMs: info?.mtimeMs ?? 0,
      size: info?.size ?? 0,
    };
  }

  async function executeScriptCell(body) {
    const read = await readScriptCell(body || {});
    const noteFile = safeNoteFile(body?.file);
    const result = await execute({
      ...(body || {}),
      file: body?.file,
      kernel: read.kernel,
      session: read.session,
      cellId: read.cellId,
      code: read.code,
    });
    if (leanRuntimeP(read.language, read.kernel)) return result;
    const outputFile = outputMirrorPath(noteFile, read.session, read.language);
    const mirror = await readOutputMirror(outputFile);
    const cells = mirror.cells && typeof mirror.cells === "object" ? mirror.cells : {};
    cells[read.cellId] = {
      ...result,
      savedAt: new Date().toISOString(),
      kernel: read.kernel,
      session: read.session,
      language: read.language,
    };
    await writeOutputMirror(outputFile, {
      version: 1,
      source: noteFile,
      kernel: read.kernel,
      session: read.session,
      cells,
    });
    return result;
  }

  async function clearScriptCellOutput(body) {
    const noteFile = safeNoteFile(body?.file);
    const kernel = cleanToken(body?.kernel, "python3");
    const session = cleanToken(body?.session, "default");
    const language = languageForKernel(kernel, body?.language || body?.lang);
    const cellId = markerId(body?.cellId || body?.id);
    if (!cellId) throw error("Missing Jupyter cell id", 400);
    const outputFile = outputMirrorPath(noteFile, session, language);
    const mirror = await readOutputMirror(outputFile);
    const cells = mirror.cells && typeof mirror.cells === "object" ? mirror.cells : {};
    delete cells[cellId];
    await writeOutputMirror(outputFile, {
      version: 1,
      source: noteFile,
      kernel,
      session,
      language,
      cells,
    });
    return { ok: true, file: outputFile, cellId, kernel, session };
  }

  async function clearAllOutputs(body) {
    const noteFile = safeNoteFile(body?.file);
    const kernel = cleanToken(body?.kernel, "python3");
    const session = cleanToken(body?.session, "default");
    const language = languageForKernel(kernel, body?.language || body?.lang);
    const outputFile = outputMirrorPath(noteFile, session, language);
    await writeOutputMirror(outputFile, {
      version: 1,
      source: noteFile,
      kernel,
      session,
      language,
      cells: {},
    });
    return { ok: true, file: outputFile, kernel, session };
  }

  async function variables(body) {
    const kernel = cleanToken(body?.kernel, "python3");
    if (!/python|sage/i.test(kernel)) {
      return { ok: true, supported: false, kernel, variables: [] };
    }
    const marker = `AARONNOTE_VARIABLES_${randomUUID().replace(/-/g, "")}`;
    const code = [
      "import json as _aaronnote_json",
      "def _aaronnote_repr(value):",
      "    try:",
      "        text = repr(value)",
      "    except Exception:",
      "        text = '<unrepresentable>'",
      "    return text if len(text) <= 160 else text[:157] + '...'",
      "def _aaronnote_shape(value):",
      "    shape = getattr(value, 'shape', None)",
      "    try:",
      "        return list(shape) if shape is not None else None",
      "    except Exception:",
      "        return None",
      "_aaronnote_vars = []",
      "for _aaronnote_name, _aaronnote_value in sorted(globals().items()):",
      "    if _aaronnote_name.startswith('_aaronnote_') or _aaronnote_name.startswith('__'):",
      "        continue",
      "    try:",
      "        _aaronnote_vars.append({'name': _aaronnote_name, 'type': type(_aaronnote_value).__name__, 'summary': _aaronnote_repr(_aaronnote_value), 'shape': _aaronnote_shape(_aaronnote_value)})",
      "    except Exception:",
      "        pass",
      `print('${marker}' + _aaronnote_json.dumps(_aaronnote_vars, default=str))`,
    ].join("\n");
    const result = await execute({ ...(body || {}), kernel, code, cellId: "__aaronnote_variables__", storeHistory: false });
    const text = (result.outputs || [])
      .filter((item) => item.output_type === "stream")
      .map((item) => String(item.text || ""))
      .join("");
    const line = text.split(/\r?\n/).find((item) => item.startsWith(marker));
    let values = [];
    if (line) {
      try { values = JSON.parse(line.slice(marker.length)); } catch {}
    }
    return { ok: true, supported: true, kernel, session: cleanToken(body?.session, "default"), variables: values };
  }

  function kernelStatus(body) {
    const file = safeNoteFile(body?.file);
    const kernel = cleanToken(body?.kernel, "python3");
    const session = cleanToken(body?.session, "default");
    const key = kernelKey({ file, kernel, session });
    const existing = kernelsByKey.get(key);
    return {
      ok: true,
      kernel,
      session,
      status: existing?.id ? (Number(existing.running || 0) > 0 ? "running" : (existing.lastStatus || "idle")) : "not-started",
      id: existing?.id || "",
      key: existing?.id ? key : "",
    };
  }

  async function restart(body) {
    return await withActiveRequest(async () => {
      const kernelInfo = await ensureKernel(body || {});
      await fetchJson(`/api/kernels/${encodeURIComponent(kernelInfo.id)}/restart`, { method: "POST", body: {} });
      const record = kernelsByKey.get(kernelInfo.key);
      if (record) {
        record.running = 0;
        record.executionStartedAt = 0;
        record.lastStatus = "restarted";
        record.lastError = "";
        touchKernel(record);
      }
      return { ok: true, kernel: kernelInfo.kernel, session: kernelInfo.session };
    });
  }

  async function interrupt(body) {
    return await withActiveRequest(async () => {
      const kernelInfo = await ensureKernel(body || {});
      await fetchJson(`/api/kernels/${encodeURIComponent(kernelInfo.id)}/interrupt`, { method: "POST", body: {} });
      const record = kernelsByKey.get(kernelInfo.key);
      if (record) {
        record.lastStatus = "interrupting";
        touchKernel(record);
      }
      return { ok: true, kernel: kernelInfo.kernel, session: kernelInfo.session };
    });
  }

  async function shutdownKernel(body) {
    const { key, record } = kernelRecordForBody(body || {});
    if (!record?.id) return { ok: true, status: "not-started" };
    if (await serverReady()) {
      await withActiveRequest(async () => {
        try { await fetchJson(`/api/kernels/${encodeURIComponent(record.id)}`, { method: "DELETE" }); } catch {}
      });
    }
    kernelsByKey.delete(key);
    scheduleCleanup();
    return { ok: true, status: "shutdown", kernel: record.kernel, session: record.session, key };
  }

  async function listTasks() {
    const ready = cleanupNeeded() ? await serverReady() : false;
    if (!ready) kernelsByKey.clear();
    return {
      ok: true,
      server: {
        status: ready ? "running" : "not-started",
        owned: Boolean(serverProcess),
        pid: serverProcess?.pid || null,
        activeRequests,
        startedAt: serverStartedAt,
        startedAtIso: isoTime(serverStartedAt),
        lastUsedAt: lastServerUseAt,
        lastUsedAtIso: isoTime(lastServerUseAt),
        idleMs: lastServerUseAt ? Math.max(0, Date.now() - lastServerUseAt) : 0,
        idleTtlMs: serverIdleTtlMs,
      },
      cleanup: {
        kernelIdleTtlMs,
        serverIdleTtlMs,
        cleanupIntervalMs,
        execTimeoutMs,
      },
      kernels: Array.from(kernelsByKey.entries()).map(([key, record]) => kernelTask(key, record)),
    };
  }

  async function cleanupIdle({ force = false, scheduled = false } = {}) {
    if (cleanupRunning) return await listTasks();
    cleanupRunning = true;
    const removed = [];
    try {
      const now = Date.now();
      const ready = cleanupNeeded() ? await serverReady() : false;
      if (!ready) {
        kernelsByKey.clear();
      } else {
        for (const [key, record] of Array.from(kernelsByKey.entries())) {
          const running = Number(record?.running || 0) > 0;
          const idleMs = now - Number(record?.lastUsedAt || now);
          if (!force && (running || idleMs < kernelIdleTtlMs)) continue;
          try { await fetchJson(`/api/kernels/${encodeURIComponent(record.id)}`, { method: "DELETE" }); } catch {}
          kernelsByKey.delete(key);
          removed.push({ key, kernel: record.kernel, session: record.session, reason: force ? "forced" : "idle" });
        }
      }
      const serverIdleMs = now - Number(lastServerUseAt || now);
      const mayStopServer = Boolean(serverProcess)
        && activeRequests === 0
        && kernelsByKey.size === 0
        && (force || serverIdleMs >= serverIdleTtlMs);
      if (mayStopServer) {
        try { serverProcess.kill("SIGTERM"); } catch {}
        serverProcess = null;
        serverStartedAt = 0;
      }
    } finally {
      cleanupRunning = false;
      if (cleanupNeeded()) scheduleCleanup();
    }
    const snapshot = await listTasks();
    return { ...snapshot, scheduled, removed };
  }

  async function shutdown() {
    cancelCleanupTimer();
    for (const item of kernelsByKey.values()) {
      if (!item?.id) continue;
      try { await fetchJson(`/api/kernels/${encodeURIComponent(item.id)}`, { method: "DELETE" }); } catch {}
    }
    kernelsByKey.clear();
    if (serverProcess) {
      try { serverProcess.kill("SIGTERM"); } catch {}
      serverProcess = null;
    }
  }

  return {
    execute,
    kernels,
    openScript,
    readScriptCell,
    executeScriptCell,
    clearScriptCellOutput,
    clearAllOutputs,
    variables,
    kernelStatus,
    restart,
    interrupt,
    shutdownKernel,
    listTasks,
    cleanup: cleanupIdle,
    shutdown,
  };
}
