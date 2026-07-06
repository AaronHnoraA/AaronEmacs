import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { existsSync } from "node:fs";
import { mkdtemp, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

// @ts-ignore The server is a Node ESM module outside the TS app graph.
import * as serverIndex from "../server/lib/index.mjs";

const {
  canonicalTodoArgs,
  configure,
  extractTodos,
  getTodos,
  inlineTagsFromContent,
  normalizeTodoStatus,
  parseCommandArgs,
  scanInlineCommands,
  syncRoamDb,
  tagsFromContent,
  todoArgKeyForCanonical,
  updateTodoStatus,
} = serverIndex as any;

const note = {
  file: "/notes/a.md",
  path: "a.md",
  key: "a",
  id: "a",
  title: "A",
  tags: ["math", "topology"],
  inlineTags: ["local-anchor"],
  groupKey: "demo",
  groupLabel: "Demo",
};

describe("server todo scan", () => {
  test("normalizes explicit todo statuses", () => {
    expect(normalizeTodoStatus("doing")).toBe("doing");
    expect(normalizeTodoStatus("done")).toBe("done");
    expect(normalizeTodoStatus("cancel")).toBe("cancelled");
    expect(normalizeTodoStatus("canceled")).toBe("cancelled");
    expect(normalizeTodoStatus("cancelled")).toBe("cancelled");
    expect(normalizeTodoStatus("")).toBe("todo");
  });

  test("extracts explicit statuses and keeps bare todo whitespace-sensitive", () => {
    const todos = extractTodos(
      [
        "@@todo [plain]",
        "@@todo(done) [closed]",
        "@@todo(doing) [active]{ddl: 2026-05-20}",
        "@@todo(cancel) [dropped]",
        "@@todo bare task",
        "@@todo(blocked) bare blocked {ddl: 2026-06-01}",
        "@@todo(doing)[not parsed]",
        "@@todo[not parsed]",
      ].join("\n"),
      note,
      1,
    );

    expect(todos.map((todo: { status: string; text: string; ddl?: string }) => [todo.status, todo.text, todo.ddl || ""])).toEqual([
      ["todo", "plain", ""],
      ["done", "closed", ""],
      ["doing", "active", "2026-05-20"],
      ["cancelled", "dropped", ""],
      ["todo", "bare task", ""],
      ["blocked", "bare blocked", "2026-06-01"],
    ]);
    expect(todos[0]).toMatchObject({
      tags: ["math", "topology"],
      inlineTags: ["local-anchor"],
      roamId: "a",
      groupKey: "demo",
      groupLabel: "Demo",
      parentFile: "a.md",
      parentTitle: "A",
    });
  });

  test("exposes reusable inline command scanning", () => {
    expect(scanInlineCommands("@@cmd(switch) [context]{arg: value}", "cmd")).toMatchObject([
      {
        name: "cmd",
        switchValue: "switch",
        context: "context",
        args: { arg: "value" },
      },
    ]);
    expect(scanInlineCommands("@@todo(doing) [text]{ddl=2026-05-20}", "todo")[0]?.args)
      .toEqual({ ddl: "2026-05-20" });
    expect(scanInlineCommands(String.raw`@@todo [prove $\alpha_{[i]}$ and $\sqrt[3]{x}$]{ddl=2026-06-01}`, "todo"))
      .toMatchObject([
        {
          name: "todo",
          switchValue: "",
          context: String.raw`prove $\alpha_{[i]}$ and $\sqrt[3]{x}$`,
          args: { ddl: "2026-06-01" },
        },
      ]);
    expect(scanInlineCommands("@@todo 裸文本 {ddl=2026-06-01}", "todo")).toMatchObject([
      {
        name: "todo",
        switchValue: "",
        context: "裸文本",
        args: { ddl: "2026-06-01" },
      },
    ]);
    expect(scanInlineCommands("@@tag[qc]", "tag")).toMatchObject([
      {
        name: "tag",
        switchValue: "",
        context: "qc",
      },
    ]);
    expect(scanInlineCommands("@@todo[not parsed]", "todo")).toEqual([]);
    expect(scanInlineCommands("@@lean4 [group-cancel]", "lean4")).toMatchObject([
      {
        name: "lean4",
        switchValue: "",
        context: "group-cancel",
      },
    ]);
    expect(scanInlineCommands("@@section(sub) [Outline]{id: custom}", "section")).toMatchObject([
      {
        name: "section",
        switchValue: "sub",
        context: "Outline",
        args: { id: "custom" },
      },
    ]);
    expect(scanInlineCommands("@@section[not parsed]", "section")).toEqual([]);
  });

  test("parseCommandArgs keeps a comma inside a quoted value intact", () => {
    expect(parseCommandArgs('{after: "fix parser, then ship", ddl: 2026-07-10}')).toEqual({
      after: "fix parser, then ship",
      ddl: "2026-07-10",
    });
  });

  test("canonicalTodoArgs normalizes aliases and uppercases priority", () => {
    expect(canonicalTodoArgs({ due: "2026-07-07", priority: "b", repeat: "+1w" })).toEqual({
      ddl: "2026-07-07",
      prio: "B",
      repeat: "+1w",
    });
    expect(canonicalTodoArgs({ scheduled: "2026-07-06" })).toEqual({ sche: "2026-07-06" });
    expect(canonicalTodoArgs({})).toEqual({});
  });

  test("todoArgKeyForCanonical reuses the alias already present on the line", () => {
    expect(todoArgKeyForCanonical("ddl", { due: "2026-07-07" })).toBe("due");
    expect(todoArgKeyForCanonical("ddl", {})).toBe("ddl");
    expect(todoArgKeyForCanonical("prio", { priority: "A" })).toBe("priority");
  });

  test("extractTodos attaches canon alongside raw args", () => {
    const todos = extractTodos(
      "@@todo(doing) [ship it] {due: 2026-07-07, priority: b}",
      note,
      1,
    );
    expect(todos[0].canon).toEqual({ ddl: "2026-07-07", prio: "B" });
    expect(todos[0].args).toEqual({ due: "2026-07-07", priority: "b" });
  });

  test("keeps inline anchors separate from file tags", () => {
    const content = [
      "---",
      "tags:",
      "  - paper",
      "  - quantum",
      "  - Quantum",
      "---",
      "",
      "Body @@tag[local-anchor]",
    ].join("\n");
    expect(tagsFromContent(content)).toEqual(["paper", "quantum"]);
    expect(inlineTagsFromContent(content)).toEqual(["local-anchor"]);
  });
});

describe("server todo agenda", () => {
  test("getTodos scans markdown without a separate todo db", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-todos-"));
    try {
      await mkdir(join(root, "state"), { recursive: true });
      configure({ root, workspaceRoot: root, stateRoot: join(root, "state"), tmpRoot: join(root, "tmp") });
      await writeFile(
        join(root, "a.md"),
        [
          "---",
          "id: 20260706T120000-a",
          "---",
          "# A",
          "",
          "@@todo(doing) [write proof]{due=2026-07-07, priority=A, scheduled=2026-07-06, repeat=+1w}",
          "",
        ].join("\n"),
        "utf8",
      );
      await writeFile(join(root, "b.md"), "---\nid: b-note\n---\n# B\n\n@@todo [other]\n", "utf8");

      await syncRoamDb(null, { mode: "full" });
      const payload = await getTodos("");
      expect(payload).toMatchObject({ source: "scan" });
      expect(payload).not.toHaveProperty("db");
      expect(existsSync(join(root, "todo.db"))).toBe(false);
      expect(payload.todos).toHaveLength(2);
      expect(payload.todos[0]).toMatchObject({
        status: "doing",
        text: "write proof",
        noteId: "20260706T120000-a",
        args: {
          due: "2026-07-07",
          priority: "A",
          scheduled: "2026-07-06",
          repeat: "+1w",
        },
      });
      const filePayload = await getTodos(join(root, "a.md"));
      expect(filePayload).toMatchObject({ source: "scan" });
      expect(filePayload.todos.map((todo: { text?: string }) => todo.text)).toEqual(["write proof"]);
    } finally {
      await rm(root, { recursive: true, force: true });
    }
  });

  test("todo status update rewrites markdown without refreshing a todo db", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-todo-update-"));
    try {
      await mkdir(join(root, "state"), { recursive: true });
      configure({ root, workspaceRoot: root, stateRoot: join(root, "state"), tmpRoot: join(root, "tmp") });
      const file = join(root, "a.md");
      await writeFile(file, "---\nid: task-note\n---\n# A\n\n@@todo [ship it]{due=2026-07-07}\n", "utf8");

      await syncRoamDb(null, { mode: "full" });
      const before = await getTodos("");
      await updateTodoStatus({
        file,
        id: before.todos[0].id,
        index: before.todos[0].index,
        source: before.todos[0].source,
        text: before.todos[0].text,
        status: "done",
      });

      expect(await readFile(file, "utf8")).toContain("@@todo(done) [ship it]");
      const after = await getTodos("");
      expect(after).toMatchObject({ source: "scan" });
      expect(existsSync(join(root, "todo.db"))).toBe(false);
      expect(after.todos[0]).toMatchObject({ status: "done", text: "ship it" });
    } finally {
      await rm(root, { recursive: true, force: true });
    }
  });

  test("todo metadata update rewrites args without changing status", async () => {
    const root = await mkdtemp(join(tmpdir(), "aaronnote-todo-meta-"));
    try {
      await mkdir(join(root, "state"), { recursive: true });
      configure({ root, workspaceRoot: root, stateRoot: join(root, "state"), tmpRoot: join(root, "tmp") });
      const file = join(root, "a.md");
      await writeFile(file, "---\nid: task-meta\n---\n# A\n\n@@todo(doing) [ship it]{due=2026-07-07}\n", "utf8");

      await syncRoamDb(null, { mode: "full" });
      const before = await getTodos("");
      await updateTodoStatus({
        file,
        id: before.todos[0].id,
        index: before.todos[0].index,
        source: before.todos[0].source,
        text: before.todos[0].text,
        priority: "b",
        due: "tomorrow",
        scheduled: "2026-07-06",
        repeat: "+1w",
      });

      const content = await readFile(file, "utf8");
      expect(content).toContain("@@todo(doing) [ship it]");
      expect(content).toContain("priority=B");
      expect(content).toContain("scheduled=2026-07-06");
      expect(content).toContain("repeat=+1w");
      const after = await getTodos("");
      expect(after.todos[0]).toMatchObject({
        status: "doing",
        text: "ship it",
        args: {
          priority: "B",
          scheduled: "2026-07-06",
          repeat: "+1w",
        },
      });
    } finally {
      await rm(root, { recursive: true, force: true });
    }
  });
});
