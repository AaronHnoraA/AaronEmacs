import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

// @ts-ignore The server is a Node ESM module outside the TS app graph.
import { offsetToPosition, positionToOffset } from "../server/lib/copilot.mjs";

describe("copilot server helpers", () => {
  test("maps markdown offsets to LSP positions and back", () => {
    const text = "alpha\nbeta\nc";
    expect(offsetToPosition(text, 0)).toEqual({ line: 0, character: 0 });
    expect(offsetToPosition(text, 8)).toEqual({ line: 1, character: 2 });
    expect(positionToOffset(text, { line: 1, character: 2 })).toBe(8);
    expect(positionToOffset(text, { line: 9, character: 2 })).toBe(text.length);
  });

});
