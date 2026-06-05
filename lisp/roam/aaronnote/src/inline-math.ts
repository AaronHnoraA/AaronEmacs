// Conservative inline math detection shared by CM6 widgets and floating preview.
// Single dollar signs often appear in prose and prices, so these rules prefer
// false negatives over wrapping ordinary text in a math box. Closing dollars
// may have a small amount of padding before them because users often type
// formulas as "$x $"; cap it tightly so long prose spans do not get swallowed.

const INLINE_MATH_TRAILING_SPACE_LIMIT = 5;

export const INLINE_MATH_RE = /(?<![A-Za-z0-9_$])\$(?![\s$])([^$\n]{0,119}\S) {0,5}\$(?![A-Za-z0-9_$])/g;

const INLINE_TEXT_WORD_RE = /(?:^|[^\\A-Za-z])([A-Za-z]+)(?=$|[^A-Za-z])/g;
const INLINE_CJK_RE = /[\u3400-\u9fff]/;
const INLINE_MATH_SIGNAL_RE = /[\\^_=+\-*/<>|()[\]{}0-9]/;
const INLINE_COMMON_PROSE_WORDS = new Set([
  "a",
  "an",
  "and",
  "are",
  "as",
  "at",
  "be",
  "but",
  "for",
  "from",
  "graph",
  "has",
  "have",
  "i",
  "in",
  "is",
  "it",
  "like",
  "math",
  "not",
  "of",
  "on",
  "or",
  "plain",
  "prose",
  "some",
  "text",
  "the",
  "this",
  "to",
  "words",
]);

export interface InlineMathRange {
  from: number;
  to: number;
  tex: string;
}

export function isEscapedSource(src: string, pos: number): boolean {
  let count = 0;
  for (let i = pos - 1; i >= 0 && src[i] === "\\"; i--) count++;
  return count % 2 === 1;
}

export function isInlineDollar(src: string, pos: number): boolean {
  return (
    src[pos] === "$" &&
    src[pos - 1] !== "$" &&
    src[pos + 1] !== "$" &&
    !isEscapedSource(src, pos)
  );
}

export function isInlineMathOpen(src: string, pos: number): boolean {
  return (
    isInlineDollar(src, pos) &&
    !/[A-Za-z0-9_]/.test(src[pos - 1] ?? "") &&
    !/\s/.test(src[pos + 1] ?? "")
  );
}

export function isInlineMathClose(src: string, pos: number): boolean {
  if (!isInlineDollar(src, pos) || /[A-Za-z0-9_]/.test(src[pos + 1] ?? "")) return false;
  let spaces = 0;
  while (
    spaces <= INLINE_MATH_TRAILING_SPACE_LIMIT &&
    src[pos - spaces - 1] === " "
  ) {
    spaces++;
  }
  const before = src[pos - spaces - 1] ?? "";
  return (
    spaces <= INLINE_MATH_TRAILING_SPACE_LIMIT &&
    before !== "" &&
    !/\s/.test(before)
  );
}

export function isLikelyInlineMath(tex: string): boolean {
  const trimmed = tex.trim();
  if (!trimmed || trimmed.length !== tex.length || trimmed.length > 120) return false;
  if (/[#$]/.test(trimmed)) return false;
  if (INLINE_CJK_RE.test(trimmed) && !trimmed.includes("\\")) return false;
  if (INLINE_MATH_SIGNAL_RE.test(trimmed)) return true;

  let words = 0;
  let commonProseWords = 0;
  INLINE_TEXT_WORD_RE.lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = INLINE_TEXT_WORD_RE.exec(trimmed)) !== null) {
    const word = match[1]!;
    const wordStart = match.index + match[0].length - word.length;
    if (wordStart > 0 && trimmed[wordStart - 1] === "\\") continue;
    words++;
    if (INLINE_COMMON_PROSE_WORDS.has(word.toLowerCase())) commonProseWords++;
  }
  if (words >= 3 && commonProseWords >= 3) return false;
  return true;
}

export function scanInlineMathRanges(text: string, baseOffset = 0): InlineMathRange[] {
  const ranges: InlineMathRange[] = [];
  INLINE_MATH_RE.lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = INLINE_MATH_RE.exec(text)) !== null) {
    const tex = match[1]!;
    if (!isLikelyInlineMath(tex)) continue;
    ranges.push({
      from: baseOffset + match.index,
      to: baseOffset + match.index + match[0].length,
      tex,
    });
  }
  return ranges;
}
