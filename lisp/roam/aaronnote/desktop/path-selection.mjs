import { realpath } from "node:fs/promises";
import { isAbsolute, relative, resolve, sep } from "node:path";

function relativePathInsideRoot(root, picked) {
  const rel = relative(resolve(root), resolve(picked));
  if (rel === "") return ".";
  if (rel === ".." || rel.startsWith(`..${sep}`) || isAbsolute(rel)) return "";
  return rel.replace(/\\/g, "/");
}

export async function normalizePickedNotePath(noteRoot, picked) {
  const logical = relativePathInsideRoot(noteRoot, picked);
  if (logical) return logical;

  try {
    const physicalRoot = await realpath(noteRoot);
    const physical = relativePathInsideRoot(physicalRoot, picked);
    if (physical) return physical;

    const physicalPicked = await realpath(picked);
    const canonical = relativePathInsideRoot(physicalRoot, physicalPicked);
    if (canonical) return canonical;
  } catch {
    // realpath throws for non-existent paths; fall back to the resolved path below.
  }

  return resolve(picked);
}
