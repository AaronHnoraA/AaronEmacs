import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { execFile } from "node:child_process";
import { mkdtemp, mkdir, readFile, rm, stat, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { promisify } from "node:util";

// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { configure } from "../server/lib/state.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { getTodos, notesIndexPayload } from "../server/lib/index.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { saveNote } from "../server/lib/save.mjs";
// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { createNode } from "../server/lib/fs-ops.mjs";

const roots: string[] = [];
const execFileAsync = promisify(execFile);

async function git(root: string, args: string[]): Promise<string> {
  const { stdout } = await execFileAsync("git", ["-C", root, ...args]);
  return stdout.trim();
}

afterEach(async () => {
  await Promise.all(roots.splice(0).map((root) => rm(root, { recursive: true, force: true })));
});

async function setupRoot() {
  const root = await mkdtemp(join(tmpdir(), "aaronnote-save-"));
  const notes = join(root, "roam");
  await mkdir(notes, { recursive: true });
  roots.push(root);
  configure({
    root: notes,
    workspaceRoot: root,
    pluginRoot: join(root, "plugin"),
  });
  return { root, notes };
}

describe("server save API", () => {
  test("deferred save returns the current note summary without a full notes list", async () => {
    const { notes } = await setupRoot();
    const file = join(notes, "a.md");
    await writeFile(file, "# A\n", "utf8");
    const base = await stat(file);

    const msg = await saveNote({
      file,
      content: "# A\n\nBody\n",
      clientId: "test",
      seq: 1,
      baseMtimeMs: base.mtimeMs,
      refresh: "deferred",
    }) as { ok?: boolean; notes?: unknown; note?: { title?: string }; notesRefresh?: string };

    expect(msg.ok).toBe(true);
    expect(msg.notes).toBeUndefined();
    expect(msg.note?.title).toBe("A");
    expect(msg.notesRefresh).toBe("deferred");
    expect(await readFile(file, "utf8")).toBe("# A\n\nBody\n");
  });

  test("mtime mismatch reports a conflict and preserves disk content", async () => {
    const { notes } = await setupRoot();
    const file = join(notes, "a.md");
    await writeFile(file, "# A\n", "utf8");
    const base = await stat(file);
    await writeFile(file, "# External\n", "utf8");

    const msg = await saveNote({
      file,
      content: "# Local\n",
      clientId: "test",
      seq: 1,
      baseMtimeMs: base.mtimeMs - 10_000,
      refresh: "deferred",
    }) as { conflict?: boolean };

    expect(msg.conflict).toBe(true);
    expect(await readFile(file, "utf8")).toBe("# External\n");
  });

  test("deferred save invalidates the lazy todo cache", async () => {
    const { notes } = await setupRoot();
    const file = join(notes, "a.md");
    await writeFile(file, "# A\n\n@@todo(todo) [first]\n", "utf8");

    expect((await notesIndexPayload()).notes).toHaveLength(1);
    const first = await getTodos();
    expect((first.todos as Array<{ text?: string; status?: string }>).map((todo) => [todo.status, todo.text]))
      .toEqual([["todo", "first"]]);

    const saved = await saveNote({
      file,
      content: "# A\n\n@@todo(done) [second]\n",
      clientId: "test",
      seq: 1,
      force: true,
      refresh: "deferred",
    }) as { ok?: boolean };
    expect(saved.ok).toBe(true);

    expect((await notesIndexPayload()).notes).toHaveLength(1);
    const second = await getTodos();
    expect((second.todos as Array<{ text?: string; status?: string }>).map((todo) => [todo.status, todo.text]))
      .toEqual([["done", "second"]]);
  });

  test("deferred save does not auto-sync or auto-commit roam db", async () => {
    const { root, notes } = await setupRoot();
    await git(root, ["init"]);
    await git(root, ["config", "user.email", "test@example.com"]);
    await git(root, ["config", "user.name", "Aaronnote Test"]);
    const file = join(notes, "a.md");
    await writeFile(file, "# A\n", "utf8");
    await git(root, ["add", "."]);
    await git(root, ["commit", "-m", "initial"]);
    const base = await stat(file);

    const saved = await saveNote({
      file,
      content: "# A\n\nNo auto commit\n",
      clientId: "test",
      seq: 1,
      baseMtimeMs: base.mtimeMs,
      refresh: "deferred",
    }) as { ok?: boolean };
    expect(saved.ok).toBe(true);

    await new Promise((resolve) => setTimeout(resolve, 2100));
    expect(await git(root, ["rev-list", "--count", "HEAD"])).toBe("1");
    expect(await git(root, ["status", "--porcelain", "--", "."])).toContain("roam/a.md");
  });

  test("creating a roam node queues db sync without committing immediately", async () => {
    const { root, notes } = await setupRoot();
    await git(root, ["init"]);
    await git(root, ["config", "user.email", "test@example.com"]);
    await git(root, ["config", "user.name", "Aaronnote Test"]);
    await writeFile(join(notes, "a.md"), "# A\n", "utf8");
    await git(root, ["add", "."]);
    await git(root, ["commit", "-m", "initial"]);

    await createNode({
      nodeType: "roam",
      id: "queued-node",
      title: "Queued Node",
      path: "queued-node.md",
    });

    expect(await git(root, ["rev-list", "--count", "HEAD"])).toBe("1");
    expect(await git(root, ["status", "--porcelain", "--", "."])).toContain("roam/queued-node.md");
  });

});
