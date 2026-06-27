import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { extractTodos, inlineTagsFromContent, normalizeTodoStatus, scanInlineCommands, tagsFromContent } from "../server/lib/index.mjs";

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
