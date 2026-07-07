// Codex-assisted LaTeX export polish.
//
// Pipeline: the mechanical converter (latex-export.mjs) produces a draft body;
// this module lets codex adjust that draft (formatting only, never prose), the
// server compiles the assembled document, and on failure feeds the log back to
// codex for a bounded number of retries. If codex is unavailable or never
// produces a compiling body, the caller falls back to the raw mechanical draft.
//
// This module owns no runtime state: paths, the template-assembly closure, and
// resolved executable paths are all passed in, so it stays pure enough to test.

import { execFile, spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { appendFile, copyFile, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

export function codexAvailable(codexBin) {
  const bin = String(codexBin || "").trim();
  if (!bin) return false;
  // `executablePath` (runtime) resolves to an absolute path when found, else the
  // bare command name. Treat an existing file as available; a bare name is only
  // trusted when it contains no path separator (assumed to be on PATH).
  if (existsSync(bin)) return true;
  return !bin.includes("/");
}

// Backend-neutral alias (codex/claude/opencode all resolve to a bin path).
export const agentAvailable = codexAvailable;

// ---- Agent-maintained conversion rules -------------------------------------

export async function loadAgentRules(agentDir) {
  const dir = String(agentDir || "").trim();
  if (!dir) return null;
  try {
    const raw = await readFile(join(dir, "mechanical", "rules.json"), "utf8");
    const parsed = JSON.parse(raw);
    if (!parsed || typeof parsed !== "object") return null;
    const envMap = parsed.envMap && typeof parsed.envMap === "object" ? parsed.envMap : {};
    const commentBlocks = Array.isArray(parsed.commentBlocks) ? parsed.commentBlocks : [];
    if (Object.keys(envMap).length === 0 && commentBlocks.length === 0) return null;
    return { envMap, commentBlocks };
  } catch {
    return null;
  }
}

export async function recordPendingImprovement(pendingLogFile, entry) {
  const file = String(pendingLogFile || "").trim();
  if (!file) return;
  try {
    await mkdir(join(file, ".."), { recursive: true });
    await appendFile(file, `${JSON.stringify({ at: new Date().toISOString(), ...entry })}\n`, "utf8");
  } catch {}
}

// ---- Prose fidelity check (heuristic, non-blocking) ------------------------

function proseWords(text) {
  let s = String(text ?? "");
  // Drop math, code, comments, and Aaronnote todos before comparing words.
  s = s.replace(/\\\(.*?\\\)/gs, " ").replace(/\\\[.*?\\\]/gs, " ").replace(/\$\$.*?\$\$/gs, " ");
  s = s.replace(/```[\s\S]*?```/g, " ").replace(/\\begin\{verbatim\}[\s\S]*?\\end\{verbatim\}/g, " ");
  s = s.replace(/@@(?:todo|itodo)[^\n]*/gi, " ");
  s = s.replace(/%[^\n]*/g, " "); // LaTeX comments
  s = s.replace(/\\[A-Za-z@]+\s*(\[[^\]]*\])?/g, " "); // LaTeX command names + optional args
  s = s.replace(/[#+*_>`~^{}\\$&/=.,;:!?()\[\]|"'—–-]/g, " "); // markup + punctuation
  const words = s.toLowerCase().match(/[\p{L}\p{N}]+/gu) || [];
  const counts = new Map();
  for (const w of words) {
    if (w.length < 2) continue; // ignore single chars / stray letters from commands
    counts.set(w, (counts.get(w) || 0) + 1);
  }
  return counts;
}

export function proseFidelityWarnings(sourceMarkdown, latexBody, options = {}) {
  const threshold = Number.isFinite(options.threshold) ? options.threshold : 0.08;
  const src = proseWords(sourceMarkdown);
  const out = proseWords(latexBody);
  let total = 0;
  let missing = 0;
  for (const [w, n] of src) {
    total += n;
    const have = out.get(w) || 0;
    if (have < n) missing += n - have;
  }
  let extra = 0;
  for (const [w, n] of out) {
    const had = src.get(w) || 0;
    if (n > had) extra += n - had;
  }
  const warnings = [];
  if (total > 0 && missing / total > threshold) {
    warnings.push(`fidelity: ~${missing}/${total} source words are missing from the LaTeX body (possible dropped/reworded text)`);
  }
  if (total > 0 && extra / total > threshold) {
    warnings.push(`fidelity: ~${extra} words in the LaTeX body are not in the source (possible added text)`);
  }
  return warnings;
}

// ---- Compile a candidate assembled document --------------------------------

// We only need to know whether the document compiles, never the PDF itself, so
// verify in draft mode: pdflatex/lualatex skip PDF output with `-draftmode`,
// xelatex with `-no-pdf`. This skips font embedding / PDF writing — the biggest
// per-attempt cost.
function draftModeFlag(engine) {
  return engine === "xelatex" ? "-no-pdf" : "-draftmode";
}

async function compileLatex({ tex, dir, latexBin, engine = "pdflatex", sourceDir, timeoutMs, signal }) {
  const texFile = join(dir, "out.tex");
  await writeFile(texFile, tex, "utf8");
  // Compile inside the staging dir (so filecontents-based classes stay there),
  // but let \includegraphics / \input resolve assets next to the source note.
  const env = { ...process.env };
  if (sourceDir) env.TEXINPUTS = `${sourceDir}//:${env.TEXINPUTS || ""}`;
  try {
    await execFileAsync(latexBin, [
      "-interaction=nonstopmode",
      "-halt-on-error",
      draftModeFlag(engine),
      `-output-directory=${dir}`,
      texFile,
    ], { cwd: dir, env, timeout: timeoutMs, maxBuffer: 16 * 1024 * 1024, signal });
    return { ok: true, log: "" };
  } catch (err) {
    const logFile = join(dir, "out.log");
    let log = "";
    try { log = await readFile(logFile, "utf8"); } catch {}
    const tail = (log || `${err?.stdout || ""}\n${err?.stderr || ""}` || String(err?.message || ""))
      .split(/\r?\n/).filter((l) => /^!|error|undefined|runaway|missing|\.tex:\d+/i.test(l))
      .slice(-30).join("\n");
    return { ok: false, log: tail || String(err?.message || "LaTeX compile failed").slice(0, 2000) };
  }
}

// ---- Agent invocation (codex / claude / opencode) --------------------------

function buildPrompt({ retryLog, needsTitle = true }) {
  const base = [
    "You are polishing a LaTeX export. All files are in your working directory.",
    "Read: style.md (style contract — obey strictly), AGENTS.md (your contract),",
    "syntax.md (Aaronnote/Markdown syntax), template.tex (the target template; note",
    "which theorem environments and macros it defines), source.md (the author's",
    "Markdown — the source of truth for text), and draft.tex (the mechanical",
    "conversion). body.tex currently equals draft.tex.",
    "",
    "Edit body.tex so that (1) it compiles when the host inserts it into template.tex,",
    "and (2) its formatting follows style.md. Do NOT add, remove, translate, or reword",
    "any prose from source.md — only change markup. Emit body content only: no",
    "\\documentclass, no preamble, no package or macro definitions.",
    "",
    needsTitle
      ? [
          "This template uses a document title. After reading the full source.md and",
          "final body.tex, write a concise document title to title.txt (one plain-text",
          "line, no markup, no quotes). The title must summarize the whole exported",
          "document, not merely copy the filename or first heading when those are generic.",
        ].join("\n")
      : [
          "This template does not use a document title placeholder. Do not invent title",
          "markup or force a title into body.tex; focus only on adapting the exported",
          "body to this template.",
        ].join("\n"),
    "",
    needsTitle
      ? "Write the final body to body.tex, write title.txt, then stop. Run no other commands."
      : "Write the final body to body.tex, then stop. Run no other commands.",
  ];
  if (retryLog) {
    base.push(
      "",
      "The previous body.tex did not compile. Compiler log tail:",
      "----",
      retryLog,
      "----",
      "Fix only what the log indicates, without changing any prose.",
    );
  }
  return base.join("\n");
}

// Backend-specific argv. All run non-interactively with permission prompts
// disabled and read/write files within the working directory.
function agentArgs(backend, { workdir, model, prompt }) {
  switch (backend) {
    case "claude":
      return [
        "-p", prompt,
        "--dangerously-skip-permissions",
        "--add-dir", workdir,
        "--output-format", "stream-json",
        "--verbose",
        ...(model ? ["--model", model] : []),
      ];
    case "opencode":
      return [
        "run",
        "--dangerously-skip-permissions",
        "--format", "json",
        ...(model ? ["-m", model] : []),
        prompt,
      ];
    case "codex":
    default:
      return [
        "exec",
        "-C", workdir,
        "--sandbox", "workspace-write",
        "--skip-git-repo-check",
        "--ephemeral",
        "-c", "approval_policy=\"never\"",
        ...(model ? ["-m", model] : []),
        prompt,
      ];
  }
}

// Extract a short human-readable progress label from a backend stdout line.
// codex prints plain text; claude/opencode emit JSONL/JSON events.
function progressLabel(backend, line) {
  const raw = String(line || "").trim();
  if (!raw) return "";
  if (backend === "codex") return raw.slice(0, 160);
  try {
    const ev = JSON.parse(raw);
    const type = ev.type || ev.event || "";
    if (backend === "claude") {
      if (type === "assistant" && ev.message?.content) {
        const t = ev.message.content.find?.((c) => c.type === "tool_use") || ev.message.content.find?.((c) => c.type === "text");
        if (t?.type === "tool_use") return `claude: ${t.name || "tool"}`;
        if (t?.type === "text" && t.text) return `claude: ${String(t.text).slice(0, 120)}`;
      }
      if (type) return `claude: ${type}`;
    } else {
      const label = ev.tool || ev.name || type;
      if (label) return `opencode: ${String(label).slice(0, 120)}`;
    }
  } catch {
    return raw.slice(0, 160);
  }
  return "";
}

function runAgent({ backend, bin, workdir, model, retryLog, needsTitle, timeoutMs, signal, onProgress }) {
  return new Promise((resolve) => {
    const args = agentArgs(backend, { workdir, model, prompt: buildPrompt({ retryLog, needsTitle }) });
    let child;
    try {
      child = spawn(bin, args, { cwd: workdir, stdio: ["ignore", "pipe", "pipe"] });
    } catch (err) {
      resolve({ ok: false, message: String(err?.message || err) });
      return;
    }
    let stderr = "";
    let stdoutBuf = "";
    let settled = false;
    const finish = (result) => {
      if (settled) return;
      settled = true;
      if (timer) clearTimeout(timer);
      if (signal) signal.removeEventListener?.("abort", onAbort);
      resolve(result);
    };
    const onAbort = () => {
      if (!child.killed) child.kill("SIGKILL");
      finish({ ok: false, message: "aborted" });
    };
    const timer = setTimeout(() => {
      if (!child.killed) child.kill("SIGKILL");
      finish({ ok: false, message: `${backend} timed out` });
    }, timeoutMs);
    if (signal) {
      if (signal.aborted) { onAbort(); return; }
      signal.addEventListener?.("abort", onAbort, { once: true });
    }
    child.stdout?.on("data", (chunk) => {
      if (!onProgress) return;
      stdoutBuf += String(chunk);
      let nl;
      while ((nl = stdoutBuf.indexOf("\n")) >= 0) {
        const line = stdoutBuf.slice(0, nl);
        stdoutBuf = stdoutBuf.slice(nl + 1);
        const label = progressLabel(backend, line);
        if (label) { try { onProgress(label); } catch {} }
      }
      if (stdoutBuf.length > 65536) stdoutBuf = stdoutBuf.slice(-65536);
    });
    child.stderr?.on("data", (chunk) => { stderr += String(chunk); if (stderr.length > 8192) stderr = stderr.slice(-8192); });
    child.on("error", (err) => finish({ ok: false, message: String(err?.message || err) }));
    child.on("close", (code) => finish(code === 0 ? { ok: true } : { ok: false, message: stderr.trim() || `${backend} exited ${code}` }));
  });
}

// ---- Orchestrator ----------------------------------------------------------

async function readAgentTitle(workdir) {
  try {
    const raw = await readFile(join(workdir, "title.txt"), "utf8");
    const line = raw.split(/\r?\n/).map((l) => l.trim()).find(Boolean) || "";
    return line.replace(/^["'“”]+|["'“”]+$/g, "").slice(0, 200);
  } catch {
    return "";
  }
}

/**
 * Polish the mechanical draft with the configured agent, gated on compilation.
 * @returns {Promise<{body:string, aiTitle:string, usedAgent:boolean, backend:string, compiled:boolean, attempts:number, warnings:string[]}>}
 */
export async function polishBodyWithAgent(opts) {
  const {
    sourceMarkdown = "",
    draftBody = "",
    templateText = "",
    styleDoc = "",
    syntaxDoc = "",
    agentsDoc = "",
    assemble,
    engine = "pdflatex",
    latexBin = "",
    backend = "codex",
    agentBin = "",
    model = "",
    sourceDir = "",
    makeWorkdir,
    maxAttempts = 3,
    needsTitle = true,
    agentTimeoutMs = 180_000,
    compileTimeoutMs = 120_000,
    signal,
    onProgress,
  } = opts || {};

  const emit = (text) => { if (onProgress && text) { try { onProgress(text); } catch {} } };
  const warnings = [];
  const base = { body: draftBody, aiTitle: "", backend, attempts: 0 };
  if (!agentAvailable(agentBin)) {
    return { ...base, usedAgent: false, compiled: false, warnings: [`${backend} unavailable; used mechanical draft`] };
  }
  const compileEnabled = typeof assemble === "function" && !!latexBin && existsSync(latexBin);

  const workdir = await makeWorkdir();
  try {
    await writeFile(join(workdir, "source.md"), sourceMarkdown, "utf8");
    await writeFile(join(workdir, "draft.tex"), draftBody, "utf8");
    await writeFile(join(workdir, "body.tex"), draftBody, "utf8");
    await writeFile(join(workdir, "template.tex"), templateText, "utf8");
    if (styleDoc && existsSync(styleDoc)) await copyFile(styleDoc, join(workdir, "style.md"));
    if (syntaxDoc && existsSync(syntaxDoc)) await copyFile(syntaxDoc, join(workdir, "syntax.md"));
    if (agentsDoc && existsSync(agentsDoc)) await copyFile(agentsDoc, join(workdir, "AGENTS.md"));

    let body = draftBody;
    let bestAiTitle = "";
    let retryLog = "";
    let attempts = 0;
    for (let i = 0; i < Math.max(1, maxAttempts); i += 1) {
      attempts = i + 1;
      emit(retryLog ? `Polishing with ${backend} (retry ${attempts})…` : `Polishing with ${backend}…`);
      const run = await runAgent({ backend, bin: agentBin, workdir, model, retryLog, needsTitle, timeoutMs: agentTimeoutMs, signal, onProgress });
      if (!run.ok) {
        warnings.push(`${backend} adjust failed (${run.message || "unknown"})`);
        break;
      }
      const aiTitle = needsTitle ? await readAgentTitle(workdir) : "";
      if (aiTitle) bestAiTitle = aiTitle;
      try {
        body = await readFile(join(workdir, "body.tex"), "utf8");
      } catch {
        warnings.push(`${backend} did not produce body.tex; used mechanical draft`);
        return { ...base, aiTitle: bestAiTitle, usedAgent: false, compiled: false, attempts, warnings };
      }
      if (!compileEnabled) {
        warnings.push("compile not verified (no LaTeX engine / assembler)");
        warnings.push(...proseFidelityWarnings(sourceMarkdown, body));
        return { body, aiTitle: bestAiTitle, backend, usedAgent: true, compiled: false, attempts, warnings };
      }
      emit(`Compiling (attempt ${attempts})…`);
      const res = await compileLatex({ tex: assemble(body), dir: workdir, latexBin, engine, sourceDir, timeoutMs: compileTimeoutMs, signal });
      if (res.ok) {
        warnings.push(...proseFidelityWarnings(sourceMarkdown, body));
        return { body, aiTitle: bestAiTitle, backend, usedAgent: true, compiled: true, attempts, warnings };
      }
      emit(`Compile failed; feeding log back to ${backend}…`);
      retryLog = res.log;
    }

    // Agent path did not yield a compiling body. Fall back to the mechanical draft.
    if (compileEnabled) {
      emit("Falling back to mechanical draft…");
      const draftRes = await compileLatex({ tex: assemble(draftBody), dir: workdir, latexBin, engine, sourceDir, timeoutMs: compileTimeoutMs, signal });
      if (draftRes.ok) {
        warnings.push(`${backend} polish did not compile after ${attempts} attempt(s); used mechanical draft`);
        return { ...base, aiTitle: bestAiTitle, usedAgent: false, compiled: true, attempts, warnings };
      }
      warnings.push(`neither ${backend} polish nor mechanical draft compiled; wrote best-effort mechanical draft`);
      return { ...base, aiTitle: bestAiTitle, usedAgent: false, compiled: false, attempts, warnings };
    }
    return { ...base, aiTitle: bestAiTitle, usedAgent: false, compiled: false, attempts, warnings };
  } finally {
    await rm(workdir, { recursive: true, force: true }).catch(() => {});
  }
}

// Back-compat alias: earlier callers used the codex-only name/shape.
export async function polishBodyWithCodex(opts = {}) {
  const result = await polishBodyWithAgent({
    ...opts,
    backend: "codex",
    agentBin: opts.agentBin || opts.codexBin || "",
    agentTimeoutMs: opts.agentTimeoutMs || opts.codexTimeoutMs,
  });
  return { ...result, usedCodex: result.usedAgent };
}
