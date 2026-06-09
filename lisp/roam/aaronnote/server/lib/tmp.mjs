import { mkdir, mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, join, resolve, sep } from "node:path";
import { createHash } from "node:crypto";

let tmpRoot = resolve(process.env.AARONNOTE_TMP_DIR || join(tmpdir(), "aaronnote"));

function safeKind(kind) {
  return String(kind || "runtime").replace(/[^A-Za-z0-9._-]+/g, "-") || "runtime";
}

function safeReadablePath(filePath) {
  const raw = resolve(String(filePath || "scratch"));
  const readable = raw
    .split(sep)
    .filter(Boolean)
    .join("_")
    .replace(/[^A-Za-z0-9._-]+/g, "-")
    .replace(/^-+|-+$/g, "");
  const fallback = basename(raw).replace(/[^A-Za-z0-9._-]+/g, "-") || "scratch";
  return (readable || fallback).slice(-96);
}

export function configureTmpRoot(root) {
  const next = String(root || process.env.AARONNOTE_TMP_DIR || "").trim();
  tmpRoot = resolve(next || join(tmpdir(), "aaronnote"));
  return tmpRoot;
}

export function aaronnoteTmpRoot() {
  return tmpRoot;
}

export function encodeOriginalPathToTmpName(filePath) {
  const resolved = resolve(String(filePath || "scratch"));
  const hash = createHash("sha256").update(resolved).digest("hex").slice(0, 12);
  return `${safeReadablePath(resolved)}--${hash}`;
}

export async function runtimeTmpDir(kind = "runtime") {
  const dir = join(tmpRoot, safeKind(kind));
  await mkdir(dir, { recursive: true });
  return dir;
}

export async function runtimeTmpFile(kind, originalPath, suffix = ".tmp") {
  const dir = await runtimeTmpDir(kind);
  const cleanSuffix = String(suffix || ".tmp").startsWith(".") ? String(suffix || ".tmp") : `.${suffix}`;
  return join(dir, `${safeKind(kind)}--${encodeOriginalPathToTmpName(originalPath)}${cleanSuffix}`);
}

export async function runtimeMkdtemp(kind, originalPath = "") {
  const dir = await runtimeTmpDir(kind);
  const prefix = `${safeKind(kind)}--${encodeOriginalPathToTmpName(originalPath || kind)}--`;
  return mkdtemp(join(dir, prefix));
}
