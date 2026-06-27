import { describe, it, expect } from "@voidzero-dev/vite-plus-test";
import { countDocStats } from "../aaronnote/ui/top-bar.tsx";

describe("countDocStats", () => {
  it("counts zero words for empty / whitespace-only input", () => {
    expect(countDocStats("")).toEqual({ words: 0, chars: 0 });
    expect(countDocStats("   \n\t ")).toEqual({ words: 0, chars: 6 });
  });

  it("counts whitespace-delimited words and raw character length", () => {
    expect(countDocStats("hello world")).toEqual({ words: 2, chars: 11 });
    expect(countDocStats("  one   two  three  ")).toEqual({ words: 3, chars: 20 });
  });

  it("treats every whitespace-delimited token as a word (markup included)", () => {
    // tokens: "#", "Heading", "-", "**bold**", "item"
    expect(countDocStats("# Heading\n\n- **bold** item").words).toBe(5);
  });

  it("uses full string length (including markup) for chars", () => {
    const md = "a *b* c";
    expect(countDocStats(md).chars).toBe(md.length);
  });
});
