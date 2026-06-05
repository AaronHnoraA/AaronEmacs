/**
 * Build the argv for opening a Lean source location in a fresh Kitty window
 * running Neovim at the target position. Kept electron-free and pure so it is
 * unit-testable. LSP line/character are 0-based; Neovim `cursor()` is 1-based.
 * Paths are returned as an argv array — callers must never shell-concatenate.
 */
import { accessSync, constants } from "node:fs";
import { delimiter, dirname, join } from "node:path";

const defaultLeanExternalDirs = ["/opt/homebrew/bin", "/usr/local/bin"];

/** Resolve an executable from explicit candidates, common GUI-app paths, then PATH. */
export function findExecutable(name, { candidates = [], preferredDirs = [], pathValue = process.env.PATH ?? "" } = {}) {
  const pathDirs = String(pathValue).split(delimiter).filter(Boolean);
  const files = [
    ...candidates.map((file) => String(file || "")).filter(Boolean),
    ...[...preferredDirs, ...pathDirs].map((dir) => join(dir, name)),
  ];
  for (const file of [...new Set(files)]) {
    try {
      accessSync(file, constants.X_OK);
      return file;
    } catch {
      // Keep looking. Packaged GUI apps often have a minimal PATH.
    }
  }
  return "";
}

export function findLeanExternalExecutables({ env = process.env, preferredDirs = defaultLeanExternalDirs } = {}) {
  const pathValue = String(env.PATH ?? "");
  return {
    kitty: findKittyExecutable({ env, preferredDirs }),
    nvim: findExecutable("nvim", { candidates: [env.AARONNOTE_NVIM], preferredDirs, pathValue }),
  };
}

export function findKittyExecutable({ env = process.env, preferredDirs = defaultLeanExternalDirs } = {}) {
  const pathValue = String(env.PATH ?? "");
  return findExecutable("kitty", { candidates: [env.AARONNOTE_KITTY], preferredDirs, pathValue });
}

export function leanExternalNvimCommand({ kitty, nvim, file, line = 0, character = 0 }) {
  const safeLine = Math.max(0, Math.floor(Number(line) || 0));
  const safeChar = Math.max(0, Math.floor(Number(character) || 0));
  return {
    command: kitty,
    args: [
      "--directory", dirname(file),
      nvim,
      `+call cursor(${safeLine + 1}, ${safeChar + 1})`,
      "--",
      file,
    ],
  };
}

export function kittyDirectoryCommand({ kitty, dir }) {
  return {
    command: kitty,
    args: ["--directory", dir],
  };
}
