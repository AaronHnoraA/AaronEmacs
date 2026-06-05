import { execFile } from "node:child_process";
import { constants, existsSync } from "node:fs";
import { access } from "node:fs/promises";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { homedir, tmpdir } from "node:os";
import { basename, join } from "node:path";
import { promisify } from "node:util";

import {
  AARONNOTE_ACCEPTED_WORDS,
  maskAaronnoteProse,
  offsetFromLineColumn,
  rangeHasCheckedText,
} from "../../shared/prose-mask.mjs";

const execFileAsync = promisify(execFile);
const TOOL_TIMEOUT_MS = 4000;
const TOOL_BUDGET_MS = 6500;
const MAX_BUFFER = 4 * 1024 * 1024;
const MAX_DIAGNOSTICS_PER_TOOL = 240;
const MAX_CHECK_CHARS = 180_000;
const CHUNK_TARGET_CHARS = 45_000;
const MAX_CHUNKS_PER_TOOL = 6;
const CSPELL_SEPARATOR = "\u001f";

const VALE_CONFIG = join(homedir(), ".config", "vale", ".vale.ini");
const LEGACY_VALE_CONFIG = join(homedir(), ".vale.ini");
const CSPELL_CONFIG = join(homedir(), ".config", "vale", "aaronnote-cspell.json");
const GUI_TOOL_PATHS = [
  join(homedir(), ".local", "bin"),
  join(homedir(), ".nix-profile", "bin"),
  "/opt/homebrew/bin",
  "/opt/homebrew/sbin",
  "/opt/homebrew/opt/node/bin",
  "/usr/local/bin",
  "/usr/local/sbin",
  "/run/current-system/sw/bin",
  "/nix/var/nix/profiles/default/bin",
  "/usr/bin",
  "/bin",
  "/usr/sbin",
  "/sbin",
];
const TOOL_ENV = {
  ...process.env,
  PATH: [...new Set([...GUI_TOOL_PATHS, ...String(process.env.PATH || "").split(":").filter(Boolean)])].join(":"),
};

async function executable(path) {
  try {
    await access(path, constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

async function resolveTool(name, envVar) {
  const configured = String(process.env[envVar] || "").trim();
  if (configured && await executable(configured)) return configured;
  for (const dir of GUI_TOOL_PATHS) {
    const candidate = join(dir, name);
    if (await executable(candidate)) return candidate;
  }
  return name;
}

function clampRange(masked, from, to) {
  const start = Math.max(0, Math.min(masked.length, Number(from) || 0));
  const end = Math.max(start, Math.min(masked.length, Number(to) || start));
  return { from: start, to: end };
}

function lineStartAt(text, pos) {
  const index = Math.max(0, Math.min(text.length, Number(pos) || 0));
  return text.lastIndexOf("\n", Math.max(0, index - 1)) + 1;
}

function lineEndAt(text, pos) {
  const index = Math.max(0, Math.min(text.length, Number(pos) || 0));
  const newline = text.indexOf("\n", index);
  return newline < 0 ? text.length : newline;
}

function normalizeCheckRanges(masked, ranges) {
  const sourceRanges = Array.isArray(ranges) && ranges.length > 0
    ? ranges
    : [{ from: 0, to: masked.length }];
  const normalized = [];
  for (const range of sourceRanges) {
    const rawFrom = Number(range?.from);
    const rawTo = Number(range?.to);
    if (!Number.isFinite(rawFrom) || !Number.isFinite(rawTo)) continue;
    const clamped = clampRange(masked, Math.min(rawFrom, rawTo), Math.max(rawFrom, rawTo));
    if (clamped.to <= clamped.from) continue;
    normalized.push({
      from: lineStartAt(masked, clamped.from),
      to: lineEndAt(masked, clamped.to),
    });
  }
  normalized.sort((a, b) => a.from - b.from || a.to - b.to);
  const merged = [];
  for (const range of normalized) {
    const previous = merged[merged.length - 1];
    if (previous && range.from <= previous.to + 1) {
      previous.to = Math.max(previous.to, range.to);
    } else {
      merged.push({ ...range });
    }
  }
  return merged.length > 0 ? merged : [{ from: 0, to: Math.min(masked.length, MAX_CHECK_CHARS) }];
}

function createCheckChunks(masked, ranges) {
  const normalized = normalizeCheckRanges(masked, ranges);
  const chunks = [];
  let checkedChars = 0;
  let partial = false;
  for (const range of normalized) {
    let from = range.from;
    while (from < range.to) {
      if (chunks.length >= MAX_CHUNKS_PER_TOOL || checkedChars >= MAX_CHECK_CHARS) {
        partial = true;
        break;
      }
      const remaining = MAX_CHECK_CHARS - checkedChars;
      const wantedTo = Math.min(range.to, from + CHUNK_TARGET_CHARS, from + remaining);
      let to = wantedTo >= range.to ? range.to : lineEndAt(masked, wantedTo);
      if (to <= from) to = Math.min(range.to, from + remaining);
      if (rangeHasCheckedText(masked, from, to)) {
        chunks.push({ index: chunks.length, from, to, text: masked.slice(from, to) });
        checkedChars += to - from;
      }
      from = Math.max(to, from + 1);
    }
    if (partial) break;
  }
  if (chunks.length === 0 && masked.length > 0) {
    const to = Math.min(masked.length, MAX_CHECK_CHARS);
    chunks.push({ index: 0, from: 0, to, text: masked.slice(0, to) });
    partial = to < masked.length;
    checkedChars = to;
  }
  return {
    chunks,
    checkedChars,
    totalChars: masked.length,
    partial: partial || normalized.some((range) => range.to - range.from > MAX_CHECK_CHARS),
  };
}

function normalizeCheckSegments(segments) {
  if (!Array.isArray(segments)) return [];
  const normalized = [];
  for (const segment of segments) {
    const from = Number(segment?.from);
    if (!Number.isFinite(from) || from < 0) continue;
    const text = String(segment?.text || "");
    if (!text) continue;
    normalized.push({
      from,
      to: from + text.length,
      text,
    });
  }
  return normalized.sort((a, b) => a.from - b.from || a.to - b.to);
}

function createCheckChunksFromSegments(segments, totalChars) {
  const normalized = normalizeCheckSegments(segments);
  const sourceLength = Number.isFinite(Number(totalChars)) && Number(totalChars) > 0
    ? Number(totalChars)
    : normalized.reduce((max, segment) => Math.max(max, segment.to), 0);
  const chunks = [];
  let checkedChars = 0;
  let partial = false;
  for (const segment of normalized) {
    if (chunks.length >= MAX_CHUNKS_PER_TOOL || checkedChars >= MAX_CHECK_CHARS) {
      partial = true;
      break;
    }
    const remaining = MAX_CHECK_CHARS - checkedChars;
    const masked = maskAaronnoteProse(segment.text);
    const info = createCheckChunks(masked, [{ from: 0, to: Math.min(masked.length, remaining) }]);
    for (const chunk of info.chunks) {
      if (chunks.length >= MAX_CHUNKS_PER_TOOL || checkedChars >= MAX_CHECK_CHARS) {
        partial = true;
        break;
      }
      chunks.push({
        index: chunks.length,
        from: segment.from + chunk.from,
        to: segment.from + chunk.to,
        text: chunk.text,
      });
      checkedChars += chunk.to - chunk.from;
    }
    if (info.partial || masked.length > remaining) partial = true;
    if (partial) break;
  }
  return {
    chunks,
    checkedChars,
    totalChars: sourceLength,
    partial: partial || normalized.some((segment) => segment.text.length > MAX_CHECK_CHARS),
  };
}

function mapChunkDiagnostics(diagnostics, chunk, sourceLength) {
  return diagnostics
    .map((diag) => ({
      ...diag,
      from: diag.from + chunk.from,
      to: diag.to + chunk.from,
    }))
    .filter((diag) => diag.from >= 0 && diag.to > diag.from && diag.to <= sourceLength);
}

function severity(value) {
  const raw = String(value || "").toLowerCase();
  if (raw === "error") return "error";
  if (raw === "warning") return "warning";
  return "info";
}

function valeReplacement(item) {
  const action = item?.Action;
  if (!action || typeof action !== "object") return undefined;
  const name = String(action.Name || "").toLowerCase();
  const params = Array.isArray(action.Params) ? action.Params : [];
  if (name === "remove") return "";
  if (name === "replace" && params[0] != null) return String(params[0]);
  if (name === "edit" && String(params[0] || "").toLowerCase() === "replace" && params[1] != null) {
    return String(params[1]);
  }
  return undefined;
}

export function parseValeDiagnostics(stdout, masked) {
  let parsed;
  try {
    parsed = JSON.parse(stdout || "{}");
  } catch {
    return [];
  }
  const items = Object.values(parsed).flat().filter((item) => item && typeof item === "object");
  const diagnostics = [];
  for (const item of items) {
    const span = Array.isArray(item.Span) ? item.Span : [];
    if (!Number.isFinite(Number(item.Line)) || !Number.isFinite(Number(span[0]))) continue;
    const from = offsetFromLineColumn(masked, Number(item.Line), Number(span[0]));
    const endColumn = Number.isFinite(Number(span[1])) ? Number(span[1]) + 1 : Number(span[0]) + String(item.Match || "").length;
    const to = offsetFromLineColumn(masked, Number(item.Line), endColumn);
    const range = clampRange(masked, from, Math.max(to, from + String(item.Match || "").length));
    if (!rangeHasCheckedText(masked, range.from, range.to)) continue;
    const replacement = valeReplacement(item);
    diagnostics.push({
      source: "vale",
      from: range.from,
      to: range.to,
      severity: severity(item.Severity),
      message: String(item.Message || item.Check || "Vale issue"),
      rule: String(item.Check || ""),
      suggestions: replacement == null ? [] : [replacement],
    });
    if (diagnostics.length >= MAX_DIAGNOSTICS_PER_TOOL) break;
  }
  return diagnostics;
}

function parseCspellSuggestions(raw) {
  return String(raw || "")
    .replace(/^Suggestions?:\s*/i, "")
    .replace(/^\[|\]$/g, "")
    .split(/[,;]/)
    .map((value) => value.trim().replace(/^["']|["']$/g, ""))
    .filter(Boolean)
    .slice(0, 8);
}

export function parseCspellDiagnostics(stdout, masked) {
  const diagnostics = [];
  for (const line of String(stdout || "").split(/\r?\n/)) {
    if (!line.trim()) continue;
    const parts = line.split(CSPELL_SEPARATOR);
    if (parts.length < 4) continue;
    const [row, col, word, message, suggestionsRaw = ""] = parts;
    const from = offsetFromLineColumn(masked, Number(row), Number(col));
    const range = clampRange(masked, from, from + String(word || "").length);
    if (!rangeHasCheckedText(masked, range.from, range.to)) continue;
    diagnostics.push({
      source: "cspell",
      from: range.from,
      to: range.to,
      severity: "warning",
      message: String(message || `Unknown word: ${word}`),
      word: String(word || ""),
      suggestions: parseCspellSuggestions(suggestionsRaw),
    });
    if (diagnostics.length >= MAX_DIAGNOSTICS_PER_TOOL) break;
  }
  return diagnostics;
}

async function runVale(chunks, file, sourceLength) {
  const bin = await resolveTool("vale", "AARONNOTE_VALE_BIN");
  const config = existsSync(VALE_CONFIG) ? VALE_CONFIG : existsSync(LEGACY_VALE_CONFIG) ? LEGACY_VALE_CONFIG : "";
  const diagnostics = [];
  const deadline = Date.now() + TOOL_BUDGET_MS;
  let partial = false;
  for (const chunk of chunks) {
    if (diagnostics.length >= MAX_DIAGNOSTICS_PER_TOOL || Date.now() >= deadline - 400) {
      partial = true;
      break;
    }
    const args = [
      "--output=JSON",
      "--no-exit",
      "--no-wrap",
      ...(config ? [`--config=${config}`] : []),
      chunk.file,
    ];
    try {
      const timeout = Math.max(750, Math.min(TOOL_TIMEOUT_MS, deadline - Date.now()));
      const { stdout } = await execFileAsync(bin, args, { timeout, maxBuffer: MAX_BUFFER, env: TOOL_ENV });
      diagnostics.push(...mapChunkDiagnostics(parseValeDiagnostics(stdout, chunk.text), chunk, sourceLength));
    } catch (err) {
      if (err?.code === "ENOENT") return { source: "vale", ok: false, diagnostics: [], message: "Vale is not installed or not on PATH" };
      const parsed = mapChunkDiagnostics(parseValeDiagnostics(err?.stdout || "", chunk.text), chunk, sourceLength);
      diagnostics.push(...parsed);
      if (err?.killed) {
        partial = true;
        break;
      }
      return {
        source: "vale",
        ok: parsed.length > 0,
        diagnostics: diagnostics.slice(0, MAX_DIAGNOSTICS_PER_TOOL),
        message: String(err?.stderr || err?.message || "Vale failed").trim(),
        partial,
      };
    }
  }
  if (diagnostics.length >= MAX_DIAGNOSTICS_PER_TOOL) partial = true;
  return {
    source: "vale",
    ok: true,
    diagnostics: diagnostics.slice(0, MAX_DIAGNOSTICS_PER_TOOL),
    message: partial ? "Vale checked a bounded scope to stay responsive" : "",
    partial,
  };
}

async function runCspell(chunks, sourceLength) {
  const bin = await resolveTool("cspell", "AARONNOTE_CSPELL_BIN");
  const configArgs = existsSync(CSPELL_CONFIG) ? ["--config", CSPELL_CONFIG] : [];
  const diagnostics = [];
  const deadline = Date.now() + TOOL_BUDGET_MS;
  let partial = false;
  for (const chunk of chunks) {
    if (diagnostics.length >= MAX_DIAGNOSTICS_PER_TOOL || Date.now() >= deadline - 400) {
      partial = true;
      break;
    }
    const args = [
      "lint",
      "--no-progress",
      "--no-summary",
      "--no-exit-code",
      "--no-color",
      "--show-suggestions",
      "--issue-template",
      `$row${CSPELL_SEPARATOR}$col${CSPELL_SEPARATOR}$text${CSPELL_SEPARATOR}$message${CSPELL_SEPARATOR}$suggestions`,
      ...configArgs,
      chunk.file,
    ];
    try {
      const timeout = Math.max(750, Math.min(TOOL_TIMEOUT_MS, deadline - Date.now()));
      const { stdout } = await execFileAsync(bin, args, { timeout, maxBuffer: MAX_BUFFER, env: TOOL_ENV });
      diagnostics.push(...mapChunkDiagnostics(parseCspellDiagnostics(stdout, chunk.text), chunk, sourceLength));
    } catch (err) {
      if (err?.code === "ENOENT") return { source: "cspell", ok: false, diagnostics: [], message: "CSpell is not installed or not on PATH", optional: true };
      const parsed = mapChunkDiagnostics(parseCspellDiagnostics(err?.stdout || "", chunk.text), chunk, sourceLength);
      diagnostics.push(...parsed);
      if (err?.killed) {
        partial = true;
        break;
      }
      return {
        source: "cspell",
        ok: parsed.length > 0,
        diagnostics: diagnostics.slice(0, MAX_DIAGNOSTICS_PER_TOOL),
        message: String(err?.stderr || err?.message || "CSpell failed").trim(),
        partial,
      };
    }
  }
  if (diagnostics.length >= MAX_DIAGNOSTICS_PER_TOOL) partial = true;
  return {
    source: "cspell",
    ok: true,
    diagnostics: diagnostics.slice(0, MAX_DIAGNOSTICS_PER_TOOL),
    message: partial ? "CSpell checked a bounded scope to stay responsive" : "",
    partial,
  };
}

function tempMarkdownName(file) {
  const raw = basename(String(file || "Aaronnote.md")).replace(/[^\w.-]+/g, "-") || "Aaronnote.md";
  return /\.(?:md|markdown)$/i.test(raw) ? raw : `${raw}.md`;
}

export async function runExternalProseChecks({ file = "", content = "", ranges = [], segments = [], totalChars = 0 } = {}) {
  const source = String(content || "");
  const tempDir = await mkdtemp(join(tmpdir(), "aaronnote-prose-"));
  try {
    const baseName = tempMarkdownName(file);
    const segmentList = normalizeCheckSegments(segments);
    const chunkInfo = segmentList.length > 0
      ? createCheckChunksFromSegments(segmentList, totalChars)
      : createCheckChunks(maskAaronnoteProse(source), ranges);
    const chunks = chunkInfo.chunks.map((chunk) => ({
      ...chunk,
      file: join(tempDir, `${String(chunk.index + 1).padStart(2, "0")}-${baseName}`),
    }));
    await Promise.all(chunks.map((chunk) => writeFile(chunk.file, chunk.text, "utf8")));
    const results = await Promise.all([
      runVale(chunks, file, chunkInfo.totalChars),
      runCspell(chunks, chunkInfo.totalChars),
    ]);
    return {
      ok: true,
      diagnostics: results.flatMap((result) => result.diagnostics ?? []),
      tools: results.map(({ source, ok, message, partial, optional }) => ({ source, ok, message: message || "", partial: !!partial, optional: !!optional })),
      scope: {
        checkedChars: chunkInfo.checkedChars,
        totalChars: chunkInfo.totalChars,
        partial: chunkInfo.partial || results.some((result) => result.partial),
      },
      acceptedWords: AARONNOTE_ACCEPTED_WORDS,
    };
  } finally {
    await rm(tempDir, { recursive: true, force: true }).catch(() => {});
  }
}
