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
    const response = await fetch(`${this.baseUrl}/lab`, {
      headers: this.cookieHeader() ? { Cookie: this.cookieHeader() } : {},
    });
    this.rememberCookies(response);
    if (!this.xsrfToken) {
      const fallback = await fetch(`${this.baseUrl}/`, {
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
  let serverProcess = null;
  let startPromise = null;

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
    if (await serverReady()) return;
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
        serverProcess.on("exit", () => { serverProcess = null; });
      }
      if (!(await waitForServer())) {
        throw error("Aaronnote Jupyter did not become ready. Run npm run jupyter:bootstrap if it is not installed.", 503);
      }
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
    if (existing?.id) return { ...existing, file, kernel, session, key };
    const created = await fetchJson("/api/kernels", {
      method: "POST",
      body: { name: kernel },
    });
    const id = String(created.id || "");
    if (!id) throw error("Jupyter did not return a kernel id", 502);
    const next = { id, file, kernel, session };
    kernelsByKey.set(key, next);
    return { ...next, key };
  }

  async function kernels() {
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
    const kernelInfo = await ensureKernel(body || {});
    const ws = new WebSocket(`${wsBaseUrl}/api/kernels/${encodeURIComponent(kernelInfo.id)}/channels?session_id=${encodeURIComponent(randomUUID())}`);
    const msgId = randomUUID();
    const sessionId = randomUUID();
    const outputs = [];
    const displayIndexes = new Map();
    let executionCount = null;
    let shellReply = null;
    let idle = false;
    let clearOnNext = false;

    const finishIfDone = (resolveDone) => {
      if (!shellReply || !idle) return;
      try { ws.close(); } catch {}
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
      const timeout = setTimeout(() => {
        try { ws.close(); } catch {}
        rejectDone(error("Jupyter execution timed out", 504));
      }, Number(process.env.AARONNOTE_JUPYTER_EXEC_TIMEOUT_MS || 120000));

      const done = (value) => {
        clearTimeout(timeout);
        resolveDone(value);
      };
      const fail = (err) => {
        clearTimeout(timeout);
        try { ws.close(); } catch {}
        rejectDone(err);
      };

      ws.addEventListener("open", () => {
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
      ws.addEventListener("message", (event) => {
        let message;
        try {
          message = JSON.parse(wsDataToString(event.data));
        } catch {
          return;
        }
        const parentId = message?.parent_header?.msg_id;
        if (parentId !== msgId) return;
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
    return { ok: true, kernel, session, status: existing?.id ? "idle" : "not-started", id: existing?.id || "" };
  }

  async function restart(body) {
    const kernelInfo = await ensureKernel(body || {});
    await fetchJson(`/api/kernels/${encodeURIComponent(kernelInfo.id)}/restart`, { method: "POST", body: {} });
    return { ok: true, kernel: kernelInfo.kernel, session: kernelInfo.session };
  }

  async function interrupt(body) {
    const kernelInfo = await ensureKernel(body || {});
    await fetchJson(`/api/kernels/${encodeURIComponent(kernelInfo.id)}/interrupt`, { method: "POST", body: {} });
    return { ok: true, kernel: kernelInfo.kernel, session: kernelInfo.session };
  }

  async function shutdownKernel(body) {
    const kernelInfo = await ensureKernel(body || {});
    await fetchJson(`/api/kernels/${encodeURIComponent(kernelInfo.id)}`, { method: "DELETE" });
    kernelsByKey.delete(kernelInfo.key);
    return { ok: true, kernel: kernelInfo.kernel, session: kernelInfo.session };
  }

  async function shutdown() {
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
    shutdown,
  };
}
