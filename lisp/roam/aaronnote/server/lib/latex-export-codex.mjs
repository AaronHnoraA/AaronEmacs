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
  s = s.replace(/@@todo[^\n]*/gi, " ");
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

async function compileLatex({ tex, dir, latexBin, sourceDir, timeoutMs, signal }) {
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

// ---- Codex invocation ------------------------------------------------------

function buildPrompt({ retryLog }) {
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
    "\\documentclass, no preamble, no package or macro definitions. Write the final",
    "result to body.tex and then stop. Run no other commands.",
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

function runCodex({ codexBin, workdir, model, retryLog, timeoutMs, signal }) {
  return new Promise((resolve) => {
    const args = [
      "exec",
      "-C", workdir,
      "--sandbox", "workspace-write",
      "--skip-git-repo-check",
      "--ephemeral",
      "-c", "approval_policy=\"never\"",
    ];
    if (model) args.push("-m", model);
    args.push(buildPrompt({ retryLog }));

    let child;
    try {
      child = spawn(codexBin, args, { cwd: workdir, stdio: ["ignore", "pipe", "pipe"] });
    } catch (err) {
      resolve({ ok: false, message: String(err?.message || err) });
      return;
    }
    let stderr = "";
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
      finish({ ok: false, message: "codex timed out" });
    }, timeoutMs);
    if (signal) {
      if (signal.aborted) { onAbort(); return; }
      signal.addEventListener?.("abort", onAbort, { once: true });
    }
    child.stdout?.on("data", () => {});
    child.stderr?.on("data", (chunk) => { stderr += String(chunk); if (stderr.length > 8192) stderr = stderr.slice(-8192); });
    child.on("error", (err) => finish({ ok: false, message: String(err?.message || err) }));
    child.on("close", (code) => finish(code === 0 ? { ok: true } : { ok: false, message: stderr.trim() || `codex exited ${code}` }));
  });
}

// ---- Orchestrator ----------------------------------------------------------

/**
 * Polish the mechanical draft with codex, gated on compilation.
 * @returns {Promise<{body:string, usedCodex:boolean, compiled:boolean, attempts:number, warnings:string[]}>}
 */
export async function polishBodyWithCodex(opts) {
  const {
    sourceMarkdown = "",
    draftBody = "",
    templateText = "",
    styleDoc = "",
    syntaxDoc = "",
    agentsDoc = "",
    assemble,
    latexBin = "",
    codexBin = "",
    model = "",
    sourceDir = "",
    makeWorkdir,
    maxAttempts = 3,
    codexTimeoutMs = 180_000,
    compileTimeoutMs = 120_000,
    signal,
  } = opts || {};

  const warnings = [];
  if (!codexAvailable(codexBin)) {
    return { body: draftBody, usedCodex: false, compiled: false, attempts: 0, warnings: ["codex unavailable; used mechanical draft"] };
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
    let retryLog = "";
    let attempts = 0;
    for (let i = 0; i < Math.max(1, maxAttempts); i += 1) {
      attempts = i + 1;
      const run = await runCodex({ codexBin, workdir, model, retryLog, timeoutMs: codexTimeoutMs, signal });
      if (!run.ok) {
        warnings.push(`codex adjust failed (${run.message || "unknown"})`);
        break;
      }
      try {
        body = await readFile(join(workdir, "body.tex"), "utf8");
      } catch {
        warnings.push("codex did not produce body.tex; used mechanical draft");
        return { body: draftBody, usedCodex: false, compiled: false, attempts, warnings };
      }
      if (!compileEnabled) {
        warnings.push("compile not verified (no LaTeX engine / assembler)");
        warnings.push(...proseFidelityWarnings(sourceMarkdown, body));
        return { body, usedCodex: true, compiled: false, attempts, warnings };
      }
      const res = await compileLatex({ tex: assemble(body), dir: workdir, latexBin, sourceDir, timeoutMs: compileTimeoutMs, signal });
      if (res.ok) {
        warnings.push(...proseFidelityWarnings(sourceMarkdown, body));
        return { body, usedCodex: true, compiled: true, attempts, warnings };
      }
      retryLog = res.log;
    }

    // Codex path did not yield a compiling body. Fall back to the mechanical draft.
    if (compileEnabled) {
      const draftRes = await compileLatex({ tex: assemble(draftBody), dir: workdir, latexBin, sourceDir, timeoutMs: compileTimeoutMs, signal });
      if (draftRes.ok) {
        warnings.push(`codex polish did not compile after ${attempts} attempt(s); used mechanical draft`);
        return { body: draftBody, usedCodex: false, compiled: true, attempts, warnings };
      }
      warnings.push(`neither codex polish nor mechanical draft compiled; wrote best-effort mechanical draft`);
      return { body: draftBody, usedCodex: false, compiled: false, attempts, warnings };
    }
    return { body: draftBody, usedCodex: false, compiled: false, attempts, warnings };
  } finally {
    await rm(workdir, { recursive: true, force: true }).catch(() => {});
  }
}
