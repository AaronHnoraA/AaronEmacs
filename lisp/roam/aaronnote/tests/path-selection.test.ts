import { mkdir, mkdtemp, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
// @ts-ignore Desktop helpers are plain Node ESM modules outside the TS app graph.
import { normalizePickedNotePath } from "../desktop/path-selection.mjs";

const roots: string[] = [];

afterEach(async () => {
  await Promise.all(roots.splice(0).map((root) => rm(root, { recursive: true, force: true })));
});

describe("desktop note path selection", () => {
  test("normalizes system picker paths returned through a symlinked notes root", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-path-selection-"));
    roots.push(root);
    const physicalRoot = join(root, "notes");
    const linkedRoot = join(root, "roam");
    const leanFile = join(physicalRoot, ".lean", "math", "shared.lean");
    await mkdir(join(physicalRoot, ".lean", "math"), { recursive: true });
    await writeFile(leanFile, "#check Nat\n", "utf8");
    await symlink(physicalRoot, linkedRoot);

    await expect(normalizePickedNotePath(linkedRoot, leanFile))
      .resolves.toBe(".lean/math/shared.lean");
  });

  test("keeps files outside the notes root absolute", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-path-selection-outside-"));
    roots.push(root);
    const notesRoot = join(root, "notes");
    const outside = join(root, "outside.lean");
    await mkdir(notesRoot, { recursive: true });
    await writeFile(outside, "#check Nat\n", "utf8");

    await expect(normalizePickedNotePath(notesRoot, outside))
      .resolves.toBe(resolve(outside));
  });
});
