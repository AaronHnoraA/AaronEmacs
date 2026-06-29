import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { createEditor } from "../src/editor-api.ts";

// Happy DOM is slower and noisier than WebKit, and the 5 MB syntax tree is
// already fully parsed at editor creation, so absolute numbers are inflated and
// jittery. These ceilings are not the 16 ms browser goal — they exist to catch
// an *accidental* O(document) edit path slipping into a previously bounded key.
//
// BOUNDED: single-character typing whose decoration work is window-bounded by
// design (line decos, table window, heading index). Must not blow up.
const BOUNDED_CEILING_MS = 480;
// KNOWN-SCAN: opening a math/diagram fence currently triggers a full-document
// rescan in blockMathRangesField / mermaid fenced-code collection (a pre-existing
// design trade-off, see docs/audit-2026-06.md). We only guard against a runaway
// (e.g. an accidental second full pass), not against the scan itself.
const KNOWN_SCAN_CEILING_MS = 2000;

function medianEditLatency(content: string, insert: string): number {
  const host = document.createElement("div");
  document.body.append(host);
  const editor = createEditor(host, { kernel: "cm6", initialContent: content });
  const position = Math.floor(editor.getMarkdownLength() / 2);
  const latencies: number[] = [];

  for (let index = 0; index < 7; index++) {
    const start = performance.now();
    editor.view.dispatch({ changes: { from: position, insert } });
    editor.view.dispatch({ changes: { from: position, to: position + insert.length } });
    latencies.push(performance.now() - start);
  }

  latencies.sort((a, b) => a - b);
  const median = latencies[Math.floor(latencies.length / 2)] ?? Infinity;
  editor.destroy();
  host.remove();
  return median;
}

describe("large-document bounded editing", () => {
  const content = readFileSync(join(process.cwd(), "tests", "synthetic_qc_note_5mb.md"), "utf8");

  // Newline is the important regression guard: an Enter press must stay on the
  // near-change line-decoration patch path (lineDecoField), never fall back to a
  // whole-document buildLineDecos rebuild.
  const boundedCases: Array<[name: string, insert: string]> = [
    ["plain text", "x"],
    ["newline (Enter)", "\n"],
    ["table pipe", "|"],
    ["heading marker", "#"],
  ];
  for (const [name, insert] of boundedCases) {
    test(`bounded latency for ${name} edits in the 5 MB fixture`, () => {
      expect(medianEditLatency(content, insert)).toBeLessThan(BOUNDED_CEILING_MS);
    }, 20_000);
  }

  const knownScanCases: Array<[name: string, insert: string]> = [
    ["block math fence", "\\["],
    ["code fence", "```"],
  ];
  for (const [name, insert] of knownScanCases) {
    test(`no runaway latency for ${name} edits in the 5 MB fixture`, () => {
      expect(medianEditLatency(content, insert)).toBeLessThan(KNOWN_SCAN_CEILING_MS);
    }, 20_000);
  }
});
