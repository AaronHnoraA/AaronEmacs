import { markConsumed, type InlineSpan } from "../inline-parse.ts";
import type { FeatureSpec, InlineFeatureSpec } from "./_types.ts";

function countBackslashesBefore(src: string, pos: number): number {
  let count = 0;
  for (let i = pos - 1; i >= 0 && src.charCodeAt(i) === 0x5c; i--) count++;
  return count;
}

function escaped(src: string, pos: number): boolean {
  return countBackslashesBefore(src, pos) % 2 === 1;
}

function validContent(delimiter: "$" | "$$", content: string): boolean {
  if (delimiter === "$$") return content.trim().length > 0;
  return content.length > 0 && !content.includes("\n") && !/^\s|\s$/.test(content);
}

function scanMathRuns(text: string, consumed: Uint8Array): InlineSpan[] {
  const out: InlineSpan[] = [];
  for (let i = 0; i < text.length; i++) {
    if (consumed[i] || text[i] !== "$" || escaped(text, i)) continue;

    const delimiter: "$" | "$$" = text[i + 1] === "$" && !consumed[i + 1] ? "$$" : "$";
    const delimLen = delimiter.length;
    const openFrom = i;
    const openTo = openFrom + delimLen;
    let closeFrom = -1;
    let closeTo = -1;

    for (let j = openTo; j < text.length; j++) {
      if (delimiter === "$" && text[j] === "\n") break;
      if (consumed[j]) continue;
      if (text.slice(j, j + delimLen) !== delimiter || escaped(text, j)) continue;
      closeFrom = j;
      closeTo = j + delimLen;
      break;
    }
    if (closeFrom < 0) continue;

    let blocked = false;
    for (let j = openFrom; j < closeTo; j++) {
      if (consumed[j]) {
        blocked = true;
        break;
      }
    }
    if (blocked) continue;

    const rawTex = text.slice(openTo, closeFrom);
    if (!validContent(delimiter, rawTex)) continue;

    const display = delimiter === "$$";
    const tex = display ? rawTex.trim() : rawTex;
    markConsumed(consumed, openFrom, closeTo);
    out.push({
      type: "math",
      from: openTo,
      to: closeFrom,
      openFrom,
      openTo,
      closeFrom,
      closeTo,
      attrs: { tex, delimiter, display },
      delimRanges: [{ from: openFrom, to: closeTo, softInside: true }],
      widgetDecorations: [
        {
          pos: openFrom,
          when: "outside",
          kind: "math-render",
          attrs: { tex, display: display ? "1" : "0" },
          side: -1,
        },
      ],
    });
    i = closeTo - 1;
  }
  return out;
}

function mathExtRanges(parent: Parameters<InlineFeatureSpec["extRanges"]>[0]): Array<[number, number]> {
  const consumed = new Uint8Array(parent.textContent.length);
  return scanMathRuns(parent.textContent, consumed).map((span) => [
    span.openFrom,
    span.closeTo,
  ]);
}

export const math: FeatureSpec = {
  name: "math",

  marks: {
    math: {
      attrs: {
        tex: { default: "" },
        delimiter: { default: "$" },
        display: { default: false },
      },
      inclusive: false,
      parseDOM: [
        {
          tag: "span[data-aaronnote-math-mark]",
          getAttrs: (el: HTMLElement) => ({
            tex: el.getAttribute("data-tex") ?? "",
            delimiter: el.getAttribute("data-delimiter") ?? "$",
            display: el.getAttribute("data-display") === "1",
          }),
        },
      ],
      toDOM: (mark) => [
        "span",
        {
          "data-aaronnote-math-mark": "",
          "data-tex": mark.attrs.tex,
          "data-delimiter": mark.attrs.delimiter,
          "data-display": mark.attrs.display ? "1" : "0",
        },
        0,
      ],
    },
  },

  markDelims: {
    math: { open: "", close: "" },
  },

  inline: {
    priority: 0.75,
    scan: scanMathRuns,
    markNames: ["math"],
    extRanges: mathExtRanges,
  },
};
