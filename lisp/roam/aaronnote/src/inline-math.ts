// Inline math detection shared by CM6 widgets and floating preview.
// Inline math uses explicit LaTeX bracket delimiters `\( … \)`. Unlike the old
// single-dollar syntax these are unambiguous and never occur in ordinary prose,
// so no prose heuristics are needed: a `\( … \)` span on a single line is math.

export const INLINE_MATH_RE = /\\\(([^\n]+?)\\\)/g;

export interface InlineMathRange {
  from: number;
  to: number;
  tex: string;
}

export function scanInlineMathRanges(text: string, baseOffset = 0): InlineMathRange[] {
  const ranges: InlineMathRange[] = [];
  INLINE_MATH_RE.lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = INLINE_MATH_RE.exec(text)) !== null) {
    const tex = match[1]!;
    ranges.push({
      from: baseOffset + match.index,
      to: baseOffset + match.index + match[0].length,
      tex,
    });
  }
  return ranges;
}
