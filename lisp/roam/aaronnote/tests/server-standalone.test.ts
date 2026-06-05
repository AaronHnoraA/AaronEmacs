import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdtemp, mkdir, readFile, rm, stat, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";

// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { configure } from "../server/lib/state.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { createNode, deleteNote, duplicateManagedFile, moveManagedPath, renameManagedPath, trashManagedPath } from "../server/lib/fs-ops.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { getTodos, notesIndexPayload, pathSuggestionsForFile, readNote, scanTemplates } from "../server/lib/index.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { scanUnusedAssets } from "../server/lib/assets.mjs";

const roots: string[] = [];

afterEach(async () => {
  await Promise.all(roots.splice(0).map((root) => rm(root, { recursive: true, force: true })));
});

async function setupRoot() {
  const root = await mkdtemp(join(tmpdir(), "aaronnote-standalone-"));
  const notes = join(root, "roam");
  const loose = join(root, "loose");
  await mkdir(notes, { recursive: true });
  await mkdir(loose, { recursive: true });
  roots.push(root);
  configure({
    root: notes,
    workspaceRoot: root,
    pluginRoot: join(root, "plugin"),
  });
  return { root, notes, loose };
}

describe("server standalone notes", () => {
  test("opening standalone Markdown keeps the content payload light and notes refresh scans siblings", async () => {
    const { loose } = await setupRoot();
    const file = join(loose, "a.md");
    const sibling = join(loose, "sibling.md");
    await writeFile(file, "# A\n", "utf8");
    await writeFile(sibling, "# Sibling\n", "utf8");

    const msg = await readNote(file) as {
      standalone?: boolean;
      notes?: Array<{ file?: string; path?: string; standalone?: boolean }>;
    };

    expect(msg.standalone).toBe(true);
    expect(msg.notes).toBeUndefined();

    const notesMsg = await notesIndexPayload() as {
      notes?: Array<{ file?: string; path?: string; standalone?: boolean }>;
    };
    expect(notesMsg.notes?.map((note) => note.file).sort()).toEqual([file, sibling].sort());
    expect(notesMsg.notes?.every((note) => note.standalone === true)).toBe(true);
  });

  test("Lean source files are not opened as notes", async () => {
    const { notes } = await setupRoot();
    const file = join(notes, "proof.lean");
    await writeFile(file, "#check Nat\n", "utf8");

    await expect(readNote(file)).rejects.toThrow("Lean files are edited manually");
  });

  test("regular notes created while browsing standalone files stay in that folder", async () => {
    const { loose, notes } = await setupRoot();
    const file = join(loose, "a.md");
    await writeFile(file, "# A\n", "utf8");
    await readNote(file);

    const msg = await createNode({
      nodeType: "regular",
      title: "Child",
      path: "child.md",
    }) as { file?: string; standalone?: boolean; notes?: Array<{ file?: string }> };
    const child = join(loose, "child.md");

    expect(msg.file).toBe(child);
    expect(msg.standalone).toBe(true);
    const childContent = await readFile(child, "utf8");
    expect(childContent).toContain("roam: off");
    expect(childContent).not.toMatch(/^id:/m);
    expect(childContent).toContain("# Child");
    expect(msg.notes?.some((note) => note.file === child)).toBe(true);
    await expect(readFile(join(notes, "child.md"), "utf8")).rejects.toThrow();
  });

  test("standalone agenda scans from the current file directory", async () => {
    const { root, loose, notes } = await setupRoot();
    const file = join(loose, "a.md");
    const sibling = join(loose, "sibling.md");
    const outsideDir = join(root, "outside");
    await mkdir(outsideDir, { recursive: true });
    await writeFile(file, "# A\n", "utf8");
    await writeFile(sibling, "# Sibling\n\n@@todo [loose todo]\n", "utf8");
    await writeFile(join(outsideDir, "other.md"), "# Other\n\n@@todo [outside todo]\n", "utf8");
    await writeFile(join(notes, "roam.md"), "# Roam\n\n@@todo [roam todo]\n", "utf8");

    const msg = await getTodos(file) as { todos?: Array<{ text?: string; file?: string }> };

    expect(msg.todos?.map((todo) => todo.text)).toEqual(["loose todo"]);
    expect(msg.todos?.[0]?.file).toBe(sibling);
  });

  test("notes payload includes real directories and hides Lean project internals", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, "empty", "child"), { recursive: true });
    await mkdir(join(notes, "images", "note-assets"), { recursive: true });
    await mkdir(join(notes, ".lean"), { recursive: true });
    await writeFile(join(notes, "a.md"), "# A\n", "utf8");
    await writeFile(join(notes, "images", "note-assets", "pic.png"), "png", "utf8");
    await writeFile(join(notes, ".lean", "a.lean"), "-- @aaronnote proof\n#check Nat\n", "utf8");

    const msg = await notesIndexPayload() as {
      directories?: Array<{ path?: string; generated?: boolean; noteCount?: number; fileCount?: number }>;
      files?: Array<{ path?: string; generated?: boolean }>;
    };

    expect(msg.directories?.some((dir) => dir.path === "empty")).toBe(true);
    expect(msg.directories?.some((dir) => dir.path === "empty/child")).toBe(true);
    expect(msg.directories?.some((dir) => dir.path === ".lean")).toBe(false);
    expect(msg.directories?.find((dir) => dir.path === "images")?.generated).toBe(true);
    expect(msg.directories?.find((dir) => dir.path === "Root")?.noteCount).toBe(1);
    expect(msg.files?.some((file) => file.path === ".lean/a.lean")).toBe(false);
    expect(msg.files).toContainEqual(expect.objectContaining({
      path: "images/note-assets/pic.png",
      generated: true,
    }));
  });

  test("path suggestions include notebooks and common programming files", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, "experiments"), { recursive: true });
    await writeFile(join(notes, "a.md"), "# A\n", "utf8");
    await writeFile(join(notes, "experiments", "analysis.ipynb"), "{}", "utf8");
    await writeFile(join(notes, "experiments", "analysis.ipynb.py"), "# %%\n", "utf8");
    await writeFile(join(notes, "experiments", "script.sh"), "echo ok\n", "utf8");
    await writeFile(join(notes, "experiments", "notes.qmd"), "---\n---\n", "utf8");

    const paths = await pathSuggestionsForFile(join(notes, "a.md")) as string[];

    expect(paths).toContain("./experiments/analysis.ipynb");
    expect(paths).toContain("./experiments/analysis.ipynb.py");
    expect(paths).toContain("./experiments/script.sh");
    expect(paths).toContain("./experiments/notes.qmd");
  });

  test("creates a note from an independent template with variables", async () => {
    const { root, notes } = await setupRoot();
    const templateDir = join(root, "templates", "markdown-mode");
    await mkdir(templateDir, { recursive: true });
    await writeFile(join(templateDir, "meeting"), [
      "# name: Meeting",
      "# key: meeting",
      "# --",
      "# {{title}}",
      "",
      "Date: {{date}}",
      "Tags: {{tags}}",
      "",
      "${1:agenda}",
      "$0",
    ].join("\n"), "utf8");

    const templates = await scanTemplates({ force: true });
    expect(templates).toContainEqual(expect.objectContaining({ key: "meeting" }));

    const created = await createNode({
      nodeType: "regular",
      title: "Weekly Sync",
      path: "weekly.md",
      tags: ["work"],
      templateKey: "meeting",
    }) as { selection?: { from?: number; to?: number } };
    const content = await readFile(join(notes, "weekly.md"), "utf8");
    expect(content).toContain("roam: off");
    expect(content).not.toMatch(/^id:/m);
    expect(content).toContain("# Weekly Sync");
    expect(content).toContain("Tags: work");
    expect(created.selection?.from).toBeDefined();
    expect(created.selection?.to).toBeDefined();
  });

  test("filesystem APIs rename, move, duplicate, and trash managed notes", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, "a"), { recursive: true });
    await mkdir(join(notes, "b"), { recursive: true });
    await writeFile(join(notes, "a", "one.md"), "# One\n", "utf8");

    const renamed = await renameManagedPath({ path: "a/one.md", name: "two.md" }) as { ok?: boolean };
    expect(renamed.ok).toBe(true);
    expect(await readFile(join(notes, "a", "two.md"), "utf8")).toBe("# One\n");

    const moved = await moveManagedPath({ path: "a/two.md", directory: "b" }) as { ok?: boolean };
    expect(moved.ok).toBe(true);
    expect(await readFile(join(notes, "b", "two.md"), "utf8")).toBe("# One\n");

    const duplicated = await duplicateManagedFile({ path: "b/two.md", target: "b/two-copy.md" }) as { ok?: boolean };
    expect(duplicated.ok).toBe(true);
    expect(await readFile(join(notes, "b", "two-copy.md"), "utf8")).toBe("# One\n");

    const trashed = await trashManagedPath({ path: "b/two-copy.md" }) as { ok?: boolean };
    expect(trashed.ok).toBe(true);
    await expect(readFile(join(notes, "b", "two-copy.md"), "utf8")).rejects.toThrow();
  });

  test("filesystem APIs manage non-Markdown files when visible through ranger", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, "assets"), { recursive: true });
    await mkdir(join(notes, "archive"), { recursive: true });
    await writeFile(join(notes, "assets", "pic.png"), "png", "utf8");

    const renamed = await renameManagedPath({ path: "assets/pic.png", name: "hero.png" }) as { ok?: boolean };
    expect(renamed.ok).toBe(true);
    expect(await readFile(join(notes, "assets", "hero.png"), "utf8")).toBe("png");

    const moved = await moveManagedPath({ path: "assets/hero.png", directory: "archive" }) as { ok?: boolean };
    expect(moved.ok).toBe(true);
    expect(await readFile(join(notes, "archive", "hero.png"), "utf8")).toBe("png");

    const duplicated = await duplicateManagedFile({ path: "archive/hero.png", target: "archive/hero-copy.png" }) as { ok?: boolean };
    expect(duplicated.ok).toBe(true);
    expect(await readFile(join(notes, "archive", "hero-copy.png"), "utf8")).toBe("png");

    const trashed = await trashManagedPath({ path: "archive/hero-copy.png" }) as { ok?: boolean };
    expect(trashed.ok).toBe(true);
    await expect(readFile(join(notes, "archive", "hero-copy.png"), "utf8")).rejects.toThrow();
  });

  test("unused asset scan recognizes modern references and ignores Lean sources", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, "images", "note-assets"), { recursive: true });
    await mkdir(join(notes, ".lean"), { recursive: true });
    await writeFile(join(notes, "images", "note-assets", "linked.png"), "png", "utf8");
    await writeFile(join(notes, "images", "note-assets", "css.png"), "png", "utf8");
    await writeFile(join(notes, "images", "note-assets", "html.png"), "png", "utf8");
    await writeFile(join(notes, "images", "note-assets", "only-lean.png"), "png", "utf8");
    await writeFile(join(notes, "images", "note-assets", "unused.png"), "png", "utf8");
    await writeFile(join(notes, "a.md"), [
      "# A",
      "![linked](images/note-assets/linked.png)",
      "<img src=\"images/note-assets/html.png\">",
      "<div style=\"background-image: url(images/note-assets/css.png)\"></div>",
      "",
    ].join("\n"), "utf8");
    await writeFile(join(notes, ".lean", "a.lean"), "-- images/note-assets/only-lean.png\n", "utf8");

    const assets = await scanUnusedAssets() as Array<{ path?: string }>;
    const paths = assets.map((asset) => asset.path).sort();

    expect(paths).toContain("images/note-assets/only-lean.png");
    expect(paths).toContain("images/note-assets/unused.png");
    expect(paths).not.toContain("images/note-assets/linked.png");
    expect(paths).not.toContain("images/note-assets/css.png");
    expect(paths).not.toContain("images/note-assets/html.png");
  });

  test("filesystem APIs keep existing Lean mirror files with managed notes", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, "a"), { recursive: true });
    await mkdir(join(notes, "b"), { recursive: true });
    await mkdir(join(notes, ".lean", "a"), { recursive: true });
    await writeFile(join(notes, "a", "one.md"), "# One\n@@lean4 [proof]\n", "utf8");
    await writeFile(join(notes, ".lean", "a", "one.lean"), "-- @aaronnote proof\n#check Nat\n", "utf8");

    const renamed = await renameManagedPath({ path: "a/one.md", name: "two.md" }) as { ok?: boolean };
    expect(renamed.ok).toBe(true);
    await expect(readFile(join(notes, ".lean", "a", "one.lean"), "utf8")).rejects.toThrow();
    expect(await readFile(join(notes, ".lean", "a", "two.lean"), "utf8")).toContain("#check Nat");

    const moved = await moveManagedPath({ path: "a/two.md", directory: "b" }) as { ok?: boolean };
    expect(moved.ok).toBe(true);
    await expect(readFile(join(notes, ".lean", "a", "two.lean"), "utf8")).rejects.toThrow();
    expect(await readFile(join(notes, ".lean", "b", "two.lean"), "utf8")).toContain("#check Nat");

    const duplicated = await duplicateManagedFile({ path: "b/two.md", target: "b/two-copy.md" }) as { ok?: boolean };
    expect(duplicated.ok).toBe(true);
    expect(await readFile(join(notes, ".lean", "b", "two-copy.lean"), "utf8")).toContain("#check Nat");

    const trashed = await trashManagedPath({ path: "b/two-copy.md" }) as { ok?: boolean };
    expect(trashed.ok).toBe(true);
    await expect(readFile(join(notes, ".lean", "b", "two-copy.lean"), "utf8")).rejects.toThrow();
  });

  test("filesystem APIs sync managed Lean mirrors without moving linked Lean files", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, "a", "lib"), { recursive: true });
    await mkdir(join(notes, ".lean", "a", "lib"), { recursive: true });
    await writeFile(join(notes, "a", "one.md"), [
      "# One",
      "@@lean4 [default]",
      "@@lean4(newfile:1) [mirror]",
      "@@lean4(lib/shared) [linked]",
      "",
    ].join("\n"), "utf8");
    await writeFile(join(notes, ".lean", "a", "one.lean"), "-- @aaronnote default\n#check Nat\n", "utf8");
    await writeFile(join(notes, ".lean", "a", "one.mirror-1.lean"), "-- @aaronnote mirror\n#check Int\n", "utf8");
    await writeFile(join(notes, ".lean", "a", "lib", "shared.lean"), "-- @aaronnote linked\n#check String\n", "utf8");

    const renamed = await renameManagedPath({ path: "a/one.md", name: "two.md" }) as { ok?: boolean };
    expect(renamed.ok).toBe(true);
    await expect(readFile(join(notes, ".lean", "a", "one.lean"), "utf8")).rejects.toThrow();
    await expect(readFile(join(notes, ".lean", "a", "one.mirror-1.lean"), "utf8")).rejects.toThrow();
    expect(await readFile(join(notes, ".lean", "a", "two.lean"), "utf8")).toContain("#check Nat");
    expect(await readFile(join(notes, ".lean", "a", "two.mirror-1.lean"), "utf8")).toContain("#check Int");
    expect(await readFile(join(notes, ".lean", "a", "lib", "shared.lean"), "utf8")).toContain("#check String");

    const duplicated = await duplicateManagedFile({ path: "a/two.md", target: "a/two-copy.md" }) as { ok?: boolean };
    expect(duplicated.ok).toBe(true);
    expect(await readFile(join(notes, ".lean", "a", "two-copy.lean"), "utf8")).toContain("#check Nat");
    expect(await readFile(join(notes, ".lean", "a", "two-copy.mirror-1.lean"), "utf8")).toContain("#check Int");
    await expect(readFile(join(notes, ".lean", "a", "lib", "shared copy.lean"), "utf8")).rejects.toThrow();

    const trashed = await trashManagedPath({ path: "a/two-copy.md" }) as { ok?: boolean };
    expect(trashed.ok).toBe(true);
    await expect(readFile(join(notes, ".lean", "a", "two-copy.lean"), "utf8")).rejects.toThrow();
    await expect(readFile(join(notes, ".lean", "a", "two-copy.mirror-1.lean"), "utf8")).rejects.toThrow();
    expect(await readFile(join(notes, ".lean", "a", "lib", "shared.lean"), "utf8")).toContain("#check String");
  });

  test("filesystem APIs move and trash Lean mirror directories with managed folders", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, "a", "nested"), { recursive: true });
    await mkdir(join(notes, "b"), { recursive: true });
    await mkdir(join(notes, ".lean", "a", "nested"), { recursive: true });
    await writeFile(join(notes, "a", "nested", "one.md"), "# One\n@@lean4 [proof]\n", "utf8");
    await writeFile(join(notes, ".lean", "a", "nested", "one.lean"), "-- @aaronnote proof\n#check Nat\n", "utf8");

    const moved = await moveManagedPath({ path: "a", directory: "b" }) as { ok?: boolean };
    expect(moved.ok).toBe(true);
    await expect(readFile(join(notes, ".lean", "a", "nested", "one.lean"), "utf8")).rejects.toThrow();
    expect(await readFile(join(notes, ".lean", "b", "a", "nested", "one.lean"), "utf8")).toContain("#check Nat");

    const renamed = await renameManagedPath({ path: "b/a", name: "c" }) as { ok?: boolean };
    expect(renamed.ok).toBe(true);
    await expect(readFile(join(notes, ".lean", "b", "a", "nested", "one.lean"), "utf8")).rejects.toThrow();
    expect(await readFile(join(notes, ".lean", "b", "c", "nested", "one.lean"), "utf8")).toContain("#check Nat");

    const trashed = await trashManagedPath({ path: "b/c", confirm: "TRASH" }) as { ok?: boolean };
    expect(trashed.ok).toBe(true);
    await expect(readFile(join(notes, ".lean", "b", "c", "nested", "one.lean"), "utf8")).rejects.toThrow();
  });

  test("delete note removes its existing Lean mirror file", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, ".lean"), { recursive: true });
    await writeFile(join(notes, "one.md"), "# One\n@@lean4 [proof]\n", "utf8");
    await writeFile(join(notes, ".lean", "one.lean"), "-- @aaronnote proof\n#check Nat\n", "utf8");

    const deleted = await deleteNote({ file: join(notes, "one.md") }) as { ok?: boolean };

    expect(deleted.ok).toBe(true);
    await expect(readFile(join(notes, ".lean", "one.lean"), "utf8")).rejects.toThrow();
  });

  test("filesystem move does not create implicit target folders", async () => {
    const { notes } = await setupRoot();
    await mkdir(join(notes, "a"), { recursive: true });
    await writeFile(join(notes, "a", "one.md"), "# One\n", "utf8");

    await expect(moveManagedPath({ path: "a/one.md", directory: "missing/Untitled.md" }))
      .rejects.toThrow(/Target folder does not exist/);
    await expect(stat(join(notes, "missing"))).rejects.toThrow();
    expect(await readFile(join(notes, "a", "one.md"), "utf8")).toBe("# One\n");
  });
});
