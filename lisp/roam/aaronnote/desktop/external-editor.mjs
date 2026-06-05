import { accessSync, constants, existsSync, statSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { spawn } from "node:child_process";
import { findExecutable } from "./lean-external.mjs";

const defaultExternalEditorDirs = ["/opt/homebrew/bin", "/usr/local/bin"];
const macNeovideBinary = "/Applications/Neovide.app/Contents/MacOS/neovide";

function executable(file) {
  try {
    accessSync(file, constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

export function findNeovideExecutable({ env = process.env, preferredDirs = defaultExternalEditorDirs } = {}) {
  const explicit = String(env.AARONNOTE_NEOVIDE || "");
  if (explicit && executable(explicit)) return explicit;
  const found = findExecutable("neovide", { preferredDirs, pathValue: String(env.PATH ?? "") });
  if (found) return found;
  return executable(macNeovideBinary) ? macNeovideBinary : "";
}

export function findExternalEditorExecutables({ env = process.env, preferredDirs = defaultExternalEditorDirs } = {}) {
  const pathValue = String(env.PATH ?? "");
  return {
    neovide: findNeovideExecutable({ env, preferredDirs }),
    nvim: findExecutable("nvim", { candidates: [env.AARONNOTE_NVIM], preferredDirs, pathValue }),
  };
}

export function neovideOpenFileCommand({ neovide, nvim, file, line = 0, character = 0, cwd = "" }) {
  const safeLine = Math.max(0, Math.floor(Number(line) || 0));
  const safeChar = Math.max(0, Math.floor(Number(character) || 0));
  const dir = cwd ? resolve(cwd) : dirname(file);
  return {
    command: neovide,
    args: [
      "--reuse-instance",
      "--neovim-bin", nvim,
      "--chdir", dir,
      "--",
      `+call cursor(${safeLine + 1}, ${safeChar + 1})`,
      "--",
      file,
    ],
  };
}

export function openFileInNeovide(target, { resolveFile = (file) => resolve(String(file || "")) } = {}) {
  const file = resolveFile(target?.file ?? target?.path ?? "");
  if (!file || !existsSync(file)) return { ok: false, file, message: `File not found: ${file}` };
  try {
    if (!statSync(file).isFile()) return { ok: false, file, message: `Not a file: ${file}` };
  } catch (err) {
    return { ok: false, file, message: err instanceof Error ? err.message : "File unavailable" };
  }
  const { neovide, nvim } = findExternalEditorExecutables();
  if (!neovide) return { ok: false, file, message: "Neovide executable not found. Set AARONNOTE_NEOVIDE or update PATH." };
  if (!nvim) return { ok: false, file, message: "Neovim executable not found. Set AARONNOTE_NVIM or update PATH." };
  const cwd = target?.cwd ? resolveFile(target.cwd) : dirname(file);
  const { command, args } = neovideOpenFileCommand({
    neovide,
    nvim,
    file,
    cwd,
    line: target?.line,
    character: target?.character,
  });
  try {
    const child = spawn(command, args, { detached: true, stdio: "ignore" });
    child.once("error", (err) => console.error("Neovide external editor failed", err));
    child.unref();
    return { ok: true, editor: "neovide", file, cwd };
  } catch (err) {
    return { ok: false, file, message: err instanceof Error ? err.message : "Failed to open Neovide" };
  }
}

export function openExternalEditorTarget(target, options = {}) {
  const kind = String(target?.kind || "file");
  if (kind !== "file") {
    return { ok: false, message: `Unsupported external editor target: ${kind}` };
  }
  return openFileInNeovide(target, options);
}
