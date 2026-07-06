import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

// @ts-ignore The server is a Node ESM module outside the TS app graph.
import * as serverIndex from "../server/lib/index.mjs";

const {
  applyRepeater,
  buildAgenda,
  completeTodo,
  configure,
  depRefForTodo,
  extractTodos,
  getTodos,
  parseDepRefs,
  parseLeadTime,
  parseRepeater,
  patchTodo,
  resolveTodoDeps,
  syncRoamDb,
  todoUrgency,
} = serverIndex as any;

const note = (file: string, title: string) => ({
  file,
  path: file,
  key: title,
  id: title,
  title,
  tags: [],
  inlineTags: [],
  groupKey: "",
  groupLabel: "",
});

async function withVault(fn: (root: string) => Promise<void>) {
  const root = await mkdtemp(join(tmpdir(), "aaronnote-agenda-"));
  try {
    await mkdir(join(root, "state"), { recursive: true });
    configure({ root, workspaceRoot: root, stateRoot: join(root, "state"), tmpRoot: join(root, "tmp") });
    await fn(root);
  } finally {
    await rm(root, { recursive: true, force: true });
  }
}

describe("repeater math", () => {
  test("parseRepeater grammar incl. bare Nd", () => {
    expect(parseRepeater("+1w")).toEqual({ mode: "+", n: 1, unit: "w" });
    expect(parseRepeater("++2d")).toEqual({ mode: "++", n: 2, unit: "d" });
    expect(parseRepeater(".+3d")).toEqual({ mode: ".+", n: 3, unit: "d" });
    expect(parseRepeater("5d")).toEqual({ mode: "+", n: 5, unit: "d" });
    expect(parseRepeater("nonsense")).toBeNull();
    expect(parseRepeater("")).toBeNull();
  });

  test("+ shifts once from the old date, even if still in the past", () => {
    const today = new Date(2026, 6, 6).getTime(); // 2026-07-06
    expect(applyRepeater("2026-06-01", parseRepeater("+1w"), today)).toBe("2026-06-08");
  });

  test("++ shifts repeatedly until the result is after today", () => {
    const today = new Date(2026, 6, 6).getTime(); // 2026-07-06
    // 2026-06-01 + 1w repeatedly: 06-08, 06-15, 06-22, 06-29, 07-06(not after), 07-13
    expect(applyRepeater("2026-06-01", parseRepeater("++1w"), today)).toBe("2026-07-13");
  });

  test(".+ shifts from the completion date (today), not the old date", () => {
    const today = new Date(2026, 6, 6).getTime(); // 2026-07-06
    expect(applyRepeater("2026-01-01", parseRepeater(".+3d"), today)).toBe("2026-07-09");
  });

  test("month/year boundaries via Date arithmetic", () => {
    const today = new Date(2026, 0, 15).getTime();
    expect(applyRepeater("2026-01-31", parseRepeater("+1m"), today)).toBe("2026-03-03");
    expect(applyRepeater("2026-01-01", parseRepeater("+1y"), today)).toBe("2027-01-01");
  });

  test("parseLeadTime supports days/weeks/months, defaults to 14", () => {
    expect(parseLeadTime("3d")).toBe(3);
    expect(parseLeadTime("2w")).toBe(14);
    expect(parseLeadTime("1m")).toBe(30);
    expect(parseLeadTime("")).toBe(14);
    expect(parseLeadTime("garbage")).toBe(14);
  });
});

describe("dependency resolution (no ids)", () => {
  test("parseDepRefs splits on & and recognizes [[Title]]:: cross-file refs", () => {
    expect(parseDepRefs("write proof & [[Other]]::fix bug")).toEqual([
      { noteTitle: null, text: "write proof", raw: "write proof" },
      { noteTitle: "Other", text: "fix bug", raw: "[[Other]]::fix bug" },
    ]);
    expect(parseDepRefs("")).toEqual([]);
  });

  test("resolves same-file deps by exact/prefix/substring tiers and computes blocked", () => {
    const todos = extractTodos(
      [
        "@@todo(doing) [write proof of lemma]",
        "@@todo [write up final draft] {after: write proof}",
        "@@todo(done) [background reading]",
      ].join("\n"),
      note("/notes/a.md", "A"),
      1,
    );
    const { lints } = resolveTodoDeps(todos);
    expect(lints).toEqual([]);
    expect(todos[1].deps).toEqual([todos[0].id]);
    expect(todos[1].effectiveStatus).toBe("blocked");
    expect(todos[1].blockedBy).toEqual([todos[0].id]);
    // completing the dependency should unblock purely by recomputation
    todos[0].status = "done";
    resolveTodoDeps(todos);
    expect(todos[1].effectiveStatus).toBe("todo");
    expect(todos[1].blockedBy).toEqual([]);
  });

  test("manual (blocked) status is distinct from computed blocking", () => {
    const todos = extractTodos("@@todo(blocked) [waiting on external reviewer]", note("/notes/a.md", "A"), 1);
    resolveTodoDeps(todos);
    expect(todos[0].status).toBe("blocked");
    expect(todos[0].effectiveStatus).toBe("blocked");
    expect(todos[0].blockedBy).toEqual([]);
  });

  test("ambiguous and broken refs lint but never block", () => {
    const todos = extractTodos(
      [
        '@@todo [draft section] {after: "nonexistent task"}',
        "@@todo [draft intro]",
        "@@todo [draft conclusion]",
        "@@todo [depends on draft] {after: draft}",
      ].join("\n"),
      note("/notes/a.md", "A"),
      1,
    );
    const { lints } = resolveTodoDeps(todos);
    expect(lints).toMatchObject([
      { kind: "broken-ref", ref: "nonexistent task" },
      { kind: "ambiguous-ref", ref: "draft" },
    ]);
    expect(todos[0].effectiveStatus).toBe("todo");
    expect(todos[3].effectiveStatus).toBe("todo");
  });

  test("cross-file refs resolve against the target note's todos", async () => {
    await withVault(async (root) => {
      await writeFile(join(root, "a.md"), "---\nid: a\n---\n# A\n\n@@todo [depends] {after: \"[[B]]::write proof\"}\n", "utf8");
      await writeFile(join(root, "b.md"), "---\nid: b\n---\n# B\n\n@@todo(doing) [write proof]\n", "utf8");
      await syncRoamDb(null, { mode: "full" });
      const agenda = await buildAgenda({});
      expect(agenda.lints).toEqual([]);
      const dependent = agenda.todos.find((t: any) => t.text === "depends");
      const dep = agenda.todos.find((t: any) => t.text === "write proof");
      expect(dependent.deps).toEqual([dep.id]);
      expect(dependent.effectiveStatus).toBe("blocked");
    });
  });
});

describe("urgency ordering", () => {
  test("priority and deadline proximity dominate; computed-blocked sorts last", () => {
    const todayMs = new Date(2026, 6, 6).getTime();
    const todos = extractTodos(
      [
        "@@todo(doing) [urgent] {priority: A, due: 2026-07-01}",
        "@@todo [later] {due: 2026-08-01}",
        "@@todo [blocked one] {after: something else}",
        "@@todo [something else]",
      ].join("\n"),
      note("/n/a.md", "A"),
      1,
    );
    const [overdueA, lowPrioFuture, blocked] = todos;
    resolveTodoDeps(todos);
    for (const t of todos) t.urgency = todoUrgency(t, todayMs);
    expect(overdueA.urgency).toBeGreaterThan(lowPrioFuture.urgency);
    expect(lowPrioFuture.urgency).toBeGreaterThan(blocked.urgency);
  });
});

describe("agenda bucketing", () => {
  test("deadline/warning/overdue/scheduled/sched-carry/log entries land in the right buckets", async () => {
    await withVault(async (root) => {
      const today = new Date();
      const iso = (d: Date) => `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
      const in3 = new Date(today); in3.setDate(in3.getDate() + 3);
      const ago2 = new Date(today); ago2.setDate(ago2.getDate() - 2);
      const schedAgo1 = new Date(today); schedAgo1.setDate(schedAgo1.getDate() - 1);

      await writeFile(
        join(root, "a.md"),
        [
          "---\nid: a\n---\n# A\n",
          `@@todo [near deadline] {ddl: ${iso(in3)}, warn: 5d}`,
          `@@todo [overdue task] {ddl: ${iso(ago2)}}`,
          `@@todo [carried sched] {sche: ${iso(schedAgo1)}}`,
          `@@todo(done) [closed one] {done: ${iso(today)}}`,
          "",
        ].join("\n"),
        "utf8",
      );
      await syncRoamDb(null, { mode: "full" });
      const agenda = await buildAgenda({ days: 7 });
      const todayBucket = agenda.days.find((d: any) => d.date === agenda.range.today);
      const kinds = todayBucket.entries.map((e: any) => e.kind);
      expect(kinds).toContain("warning");
      expect(kinds).toContain("overdue");
      expect(kinds).toContain("sched-carry");
      expect(kinds).toContain("log");
      const deadlineBucket = agenda.days.find((d: any) => d.date === iso(in3));
      expect(deadlineBucket.entries.map((e: any) => e.kind)).toContain("deadline");
      expect(agenda.logByDay[agenda.range.today]).toBe(1);
      expect(agenda.stats.overdue).toBe(1);
    });
  });
});

describe("completeTodo repeater roll", () => {
  test("completing a repeating todo rolls ddl forward, resets status, records done+log", async () => {
    await withVault(async (root) => {
      const file = join(root, "a.md");
      await writeFile(file, "---\nid: a\n---\n# A\n\n@@todo(doing) [water plants] {due: 2026-07-01, repeat: +1w}\n", "utf8");
      await syncRoamDb(null, { mode: "full" });
      const before = (await getTodos("")).todos[0];
      const result = await completeTodo({ file, id: before.id, index: before.index, source: before.source, text: before.text });
      expect(result.changed).toBe(true);
      const content = await readFile(file, "utf8");
      expect(content).toMatch(/@@todo \[water plants\]/);
      expect(content).toContain("due=2026-07-08");
      expect(content).toMatch(/done=\d{4}-\d{2}-\d{2}/);
      expect(content).toMatch(/log=\d{4}-\d{2}-\d{2}/);
      const after = (await getTodos("")).todos[0];
      expect(after.status).toBe("todo");
    });
  });

  test("completing a non-repeating todo just marks it done with a done date", async () => {
    await withVault(async (root) => {
      const file = join(root, "a.md");
      await writeFile(file, "---\nid: a\n---\n# A\n\n@@todo [one-off] {due: 2026-07-01}\n", "utf8");
      await syncRoamDb(null, { mode: "full" });
      const before = (await getTodos("")).todos[0];
      await completeTodo({ file, id: before.id, index: before.index, source: before.source, text: before.text });
      const after = (await getTodos("")).todos[0];
      expect(after.status).toBe("done");
      expect(after.canon.done).toMatch(/\d{4}-\d{2}-\d{2}/);
    });
  });
});

describe("patchTodo alias-preserving writes", () => {
  test("reuses the existing alias and only introduces canonical keys for new args", async () => {
    await withVault(async (root) => {
      const file = join(root, "a.md");
      await writeFile(file, "---\nid: a\n---\n# A\n\n@@todo [ship it] {due: 2026-07-07}\n", "utf8");
      await syncRoamDb(null, { mode: "full" });
      const before = (await getTodos("")).todos[0];
      await patchTodo({ file, id: before.id, index: before.index, source: before.source, text: before.text, ddl: "2026-07-09", prio: "A" });
      const content = await readFile(file, "utf8");
      expect(content).toContain("due=2026-07-09");
      expect(content).toContain("prio=A");
      expect(content).not.toContain("ddl=");
    });
  });

  test("afterAdd appends a dep ref with &", async () => {
    await withVault(async (root) => {
      const file = join(root, "a.md");
      await writeFile(file, "---\nid: a\n---\n# A\n\n@@todo [second task] {after: first}\n", "utf8");
      await syncRoamDb(null, { mode: "full" });
      const before = (await getTodos("")).todos[0];
      await patchTodo({ file, id: before.id, index: before.index, source: before.source, text: before.text, afterAdd: "third" });
      const content = await readFile(file, "utf8");
      expect(content).toMatch(/after=.*first.*third|after=.*third.*first/);
    });
  });
});

describe("depRefForTodo", () => {
  test("generates the shortest unique word-boundary prefix", () => {
    const [a, b] = extractTodos(
      ["@@todo [write introduction section]", "@@todo [write conclusion section]"].join("\n"),
      note("/n/a.md", "A"),
      1,
    );
    const ref = depRefForTodo(a, [a, b], b);
    expect(ref).toBe("write introduction");
  });

  test("prefixes cross-file refs with [[Title]]::", () => {
    const a = extractTodos("@@todo [unique target text]", note("/n/a.md", "A"), 1)[0];
    const b = extractTodos("@@todo [source task]", note("/n/b.md", "B"), 1)[0];
    const ref = depRefForTodo(a, [a], b);
    expect(ref).toBe("[[A]]::unique");
  });
});
