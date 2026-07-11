import { execFile } from "node:child_process";
import { constants } from "node:fs";
import { access } from "node:fs/promises";
import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { dirname, join } from "node:path";

import {
  AARONNOTE_ACCEPTED_WORDS,
  maskAaronnoteProse,
  rangeHasCheckedText,
} from "../../shared/prose-mask.mjs";

const LANGUAGETOOL_TIMEOUT_MS = 15_000;
const LANGUAGETOOL_HTTP_TIMEOUT_MS = 5_000;
const MAX_BUFFER = 4 * 1024 * 1024;
const MAX_DIAGNOSTICS_PER_TOOL = 240;
const MAX_CHECK_CHARS = 180_000;
const CHUNK_TARGET_CHARS = 45_000;
const MAX_CHUNKS_PER_TOOL = 6;
const LANGUAGETOOL_LANGUAGE = process.env.AARONNOTE_LANGUAGETOOL_LANGUAGE || "en-US";
const LANGUAGETOOL_URL = process.env.AARONNOTE_LANGUAGETOOL_URL
  || "http://10.243.90.222:8765";

const WORKSPACE_ROOT = process.env.AARONNOTE_WORKSPACE_ROOT || join(homedir(), ".config", "emacs");
const USER_WORDS_FILE = process.env.AARONNOTE_PROSE_WORDS
  || join(WORKSPACE_ROOT, "etc", "prose-accepted-words.txt");
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

function execFileWithInput(file, args, input, options) {
  return new Promise((resolve, reject) => {
    const child = execFile(file, args, options, (error, stdout, stderr) => {
      if (error) {
        error.stdout = stdout;
        error.stderr = stderr;
        reject(error);
        return;
      }
      resolve({ stdout, stderr });
    });
    child.stdin.end(input);
  });
}

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

function languageToolSeverity(item) {
  const issueType = String(item?.rule?.issueType || "").toLowerCase();
  const category = String(item?.rule?.category?.id || "").toUpperCase();
  if (issueType === "grammar" || category === "GRAMMAR") return "error";
  if (issueType === "misspelling" || category === "TYPOS") return "warning";
  return "info";
}

export function parseLanguageToolDiagnostics(stdout, masked) {
  let parsed;
  try {
    parsed = JSON.parse(stdout || "{}");
  } catch {
    return [];
  }
  const items = Array.isArray(parsed?.matches) ? parsed.matches : [];
  const diagnostics = [];
  for (const item of items) {
    const offset = Number(item?.offset);
    const length = Number(item?.length);
    if (!Number.isFinite(offset) || !Number.isFinite(length) || length <= 0) continue;
    const range = clampRange(masked, offset, offset + length);
    if (!rangeHasCheckedText(masked, range.from, range.to)) continue;
    const suggestions = Array.isArray(item?.replacements)
      ? item.replacements.map((entry) => String(entry?.value ?? "")).slice(0, 8)
      : [];
    diagnostics.push({
      source: "languagetool",
      from: range.from,
      to: range.to,
      severity: languageToolSeverity(item),
      message: String(item?.message || item?.shortMessage || "LanguageTool issue"),
      rule: String(item?.rule?.id || ""),
      word: masked.slice(range.from, range.to),
      suggestions,
    });
    if (diagnostics.length >= MAX_DIAGNOSTICS_PER_TOOL) break;
  }
  return diagnostics;
}

function normalizedAcceptedWord(value) {
  const word = String(value || "").trim();
  if (!/^[A-Za-z][A-Za-z'’-]{1,63}$/.test(word)) return "";
  return word.toLowerCase();
}

async function readUserWords() {
  try {
    return new Set((await readFile(USER_WORDS_FILE, "utf8"))
      .split(/\r?\n/)
      .map(normalizedAcceptedWord)
      .filter(Boolean));
  } catch {
    return new Set();
  }
}

export async function acceptProseWord(value) {
  const word = normalizedAcceptedWord(value);
  if (!word) return { ok: false, message: "Word must contain 2-64 alphabetic characters" };
  let entries = [];
  try {
    entries = (await readFile(USER_WORDS_FILE, "utf8")).split(/\r?\n/).map((entry) => entry.trim()).filter(Boolean);
  } catch {
    // The vocabulary is created on first use.
  }
  if (!entries.some((entry) => normalizedAcceptedWord(entry) === word)) entries.push(String(value).trim());
  const sorted = entries.sort((a, b) => a.localeCompare(b, undefined, { sensitivity: "base" }));
  await mkdir(dirname(USER_WORDS_FILE), { recursive: true });
  const temporary = `${USER_WORDS_FILE}.${process.pid}.tmp`;
  await writeFile(temporary, `${sorted.join("\n")}\n`, "utf8");
  await rename(temporary, USER_WORDS_FILE);
  return { ok: true, word };
}

function combineLanguageToolChunks(chunks) {
  let text = "";
  const mappings = [];
  for (const chunk of chunks) {
    if (text) text += "\n\n";
    const combinedFrom = text.length;
    text += chunk.text;
    mappings.push({
      combinedFrom,
      combinedTo: text.length,
      sourceFrom: chunk.from,
    });
  }
  return { text, mappings };
}

function mapLanguageToolDiagnostics(diagnostics, mappings, sourceLength) {
  return diagnostics.flatMap((diagnostic) => {
    const mapping = mappings.find((entry) => (
      diagnostic.from >= entry.combinedFrom && diagnostic.to <= entry.combinedTo
    ));
    if (!mapping) return [];
    const from = mapping.sourceFrom + diagnostic.from - mapping.combinedFrom;
    const to = mapping.sourceFrom + diagnostic.to - mapping.combinedFrom;
    if (from < 0 || to <= from || to > sourceLength) return [];
    return [{ ...diagnostic, from, to }];
  });
}

function languageToolResult(stdout, combined, sourceLength) {
  const diagnostics = mapLanguageToolDiagnostics(
    parseLanguageToolDiagnostics(stdout, combined.text),
    combined.mappings,
    sourceLength,
  );
  const partial = diagnostics.length >= MAX_DIAGNOSTICS_PER_TOOL;
  return {
    source: "languagetool",
    ok: true,
    diagnostics: diagnostics.slice(0, MAX_DIAGNOSTICS_PER_TOOL),
    message: partial ? "LanguageTool diagnostics were capped to stay responsive" : "",
    partial,
  };
}

async function runLanguageToolRemote(combined, sourceLength) {
  const endpoint = `${LANGUAGETOOL_URL.replace(/\/+$/, "")}/v2/check`;
  const body = new URLSearchParams({
    language: LANGUAGETOOL_LANGUAGE,
    level: "picky",
    text: combined.text,
  });
  const response = await fetch(endpoint, {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body,
    signal: AbortSignal.timeout(LANGUAGETOOL_HTTP_TIMEOUT_MS),
  });
  if (!response.ok) throw new Error(`NAS LanguageTool returned HTTP ${response.status}`);
  return languageToolResult(await response.text(), combined, sourceLength);
}

async function runLanguageToolCli(combined, sourceLength) {
  const bin = await resolveTool("languagetool", "AARONNOTE_LANGUAGETOOL_BIN");
  const args = [
    "--encoding", "utf8",
    "--json",
    "--language", LANGUAGETOOL_LANGUAGE,
    "--level", "PICKY",
    "--clean-overlapping",
    "-",
  ];
  try {
    const { stdout } = await execFileWithInput(bin, args, combined.text, {
      timeout: LANGUAGETOOL_TIMEOUT_MS,
      maxBuffer: MAX_BUFFER,
      env: TOOL_ENV,
    });
    return languageToolResult(stdout, combined, sourceLength);
  } catch (err) {
    if (err?.code === "ENOENT") {
      return {
        source: "languagetool",
        ok: false,
        diagnostics: [],
        message: "LanguageTool is not installed or not on PATH",
      };
    }
    const diagnostics = mapLanguageToolDiagnostics(
      parseLanguageToolDiagnostics(err?.stdout || "", combined.text),
      combined.mappings,
      sourceLength,
    );
    return {
      source: "languagetool",
      ok: diagnostics.length > 0,
      diagnostics: diagnostics.slice(0, MAX_DIAGNOSTICS_PER_TOOL),
      message: String(err?.stderr || err?.message || "LanguageTool failed").trim(),
      partial: !!err?.killed,
    };
  }
}

async function runLanguageTool(chunks, sourceLength, allowLocalFallback) {
  const combined = combineLanguageToolChunks(chunks);
  try {
    return await runLanguageToolRemote(combined, sourceLength);
  } catch (remoteError) {
    const reason = remoteError instanceof Error ? remoteError.message : String(remoteError);
    if (!allowLocalFallback) {
      return {
        source: "languagetool",
        ok: false,
        diagnostics: [],
        message: `NAS LanguageTool unavailable (${reason})`,
        partial: false,
      };
    }
    const result = await runLanguageToolCli(combined, sourceLength);
    return {
      ...result,
      message: result.ok
        ? `NAS LanguageTool unavailable; used local CLI (${reason})`
        : `${reason}; ${result.message}`,
    };
  }
}

export async function runExternalProseChecks({ file = "", content = "", ranges = [], segments = [], totalChars = 0, allowLocalFallback = true } = {}) {
  void file;
  const source = String(content || "");
  const segmentList = normalizeCheckSegments(segments);
  const chunkInfo = segmentList.length > 0
    ? createCheckChunksFromSegments(segmentList, totalChars)
    : createCheckChunks(maskAaronnoteProse(source), ranges);
  const [result, userWords] = await Promise.all([
    runLanguageTool(chunkInfo.chunks, chunkInfo.totalChars, allowLocalFallback !== false),
    readUserWords(),
  ]);
  const acceptedWords = new Set([
    ...AARONNOTE_ACCEPTED_WORDS.map(normalizedAcceptedWord),
    ...userWords,
  ]);
  const diagnostics = (result.diagnostics ?? [])
    .filter((diagnostic) => !acceptedWords.has(normalizedAcceptedWord(diagnostic.word)));
  const { source: toolSource, ok, message, partial } = result;
  return {
    ok: true,
    diagnostics,
    tools: [{ source: toolSource, ok, message: message || "", partial: !!partial }],
    scope: {
      checkedChars: chunkInfo.checkedChars,
      totalChars: chunkInfo.totalChars,
      partial: chunkInfo.partial || !!result.partial,
    },
    acceptedWords: [...acceptedWords],
  };
}
