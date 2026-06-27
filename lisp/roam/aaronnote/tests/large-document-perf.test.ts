import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { createEditor } from "../src/editor-api.ts";

describe("large-document bounded editing", () => {
  test("plain edits in the 5 MB fixture do not trigger long synchronous scans", () => {
    const content = readFileSync(join(process.cwd(), "tests", "synthetic_qc_note_5mb.md"), "utf8");
    const host = document.createElement("div");
    document.body.append(host);
    const editor = createEditor(host, { kernel: "cm6", initialContent: content });
    const position = Math.floor(editor.getMarkdownLength() / 2);
    const latencies: number[] = [];

    for (let index = 0; index < 7; index++) {
      const start = performance.now();
      editor.view.dispatch({ changes: { from: position, insert: "x" } });
      editor.view.dispatch({ changes: { from: position, to: position + 1 } });
      latencies.push(performance.now() - start);
    }

    latencies.sort((a, b) => a - b);
    const median = latencies[Math.floor(latencies.length / 2)] ?? Infinity;
    // Happy DOM is slower and noisier than WebKit. This ceiling catches the
    // accidental O(document) edit path while the browser POC keeps the 16 ms goal.
    expect(median).toBeLessThan(240);
    editor.destroy();
    host.remove();
  }, 20_000);
});
