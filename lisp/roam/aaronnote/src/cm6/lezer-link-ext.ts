/**
 * Replaces `@lezer/markdown`'s built-in `LinkEnd` inline parser (the `]`
 * handler) so a nested, unescaped `[inner]` inside link/image text doesn't
 * eat the enclosing `[...](url)` structure.
 *
 * Background: stock Lezer resolves ANY non-empty `[label]` into a Link node
 * the moment it sees the closing `]` — regardless of whether a matching
 * `[label]: url` reference definition exists anywhere in the document. For
 * `[link [inner] rest](url)` this means the *inner* bracket pair "wins" (it's
 * the nearest unmatched `[`), gets consumed as a bare shortcut-reference
 * Link, and — per CommonMark's "no links inside links" rule — deactivates
 * the outer `[`, so `[link ` and `](url)` end up as plain, disconnected text.
 * markdown-it (used for HTML/publish export) gets this right because it can
 * check reference-definition existence; `@lezer/markdown`'s single-pass,
 * per-leaf inline parser has no cheap way to know about definitions that may
 * appear anywhere else in the document, so it never tries.
 *
 * Full reference-aware resolution would need a whole-document, two-pass scan
 * (collect every `[label]: url` first, then resolve inline ambiguity), which
 * doesn't fit Lezer's incremental per-leaf parsing model. Instead this parser
 * narrows the gap with a local, stack-based rule: a bracket pair with no
 * immediate `(` or `[` following (i.e. it can only be a bare "shortcut
 * reference") is deferred — treated as ordinary text, not a Link — whenever
 * an earlier, still-open `[`/`![` is pending on the delimiter stack. That
 * lets the *outer* bracket claim the closing `]` that actually has `(url)`
 * after it. A bracket immediately followed by `(` or `[` (an unambiguous
 * inline link, full reference, or collapsed reference) always proceeds
 * exactly as stock does, nested or not.
 *
 * Known limitation: this is a local heuristic, not full reference-definition
 * lookahead. A standalone bare shortcut reference (`[my-ref]` with a real
 * `[my-ref]: url` definition elsewhere) still resolves as a link exactly as
 * before *unless* it happens to be nested inside another still-open, never
 * later closed `[`/`![` in the same paragraph — an intentionally accepted,
 * narrow edge case in exchange for not needing whole-document lookahead.
 *
 * `parseURL`/`parseLinkTitle`/`parseLinkLabel`/`finishLink` below are
 * rewritten (not copy-pasted 1:1) from `@lezer/markdown`'s private
 * equivalents, adapted to the public `InlineContext` API (document-relative
 * positions via `cx.char`/`cx.slice` instead of leaf-relative text+offset).
 */

import { InlineContext, type DelimiterType, type Element, type MarkdownConfig } from "@lezer/markdown";

// `InlineContext.getDelimiterAt` is publicly typed as `{from,to,type}`, but
// the real object is the library's internal `InlineDelimiter`, which also
// carries a mutable `.side` bitflag (1 = open, 2 = close). Reading/writing
// `.side` here mirrors exactly what the library's own built-in `LinkEnd`
// does internally — there is no public API for it.
type InternalDelimiter = { from: number; to: number; type: DelimiterType; side: number };

function asInternal(part: { from: number; to: number; type: DelimiterType } | null): InternalDelimiter | null {
  return part as InternalDelimiter | null;
}

function isSpaceCode(ch: number): boolean {
  return ch === 32 || ch === 9 || ch === 10 || ch === 13;
}

function parseURL(cx: InlineContext, start: number): { from: number; to: number } | null | false {
  const next = cx.char(start);
  if (next === 60 /* '<' */) {
    for (let pos = start + 1; pos < cx.end; pos++) {
      const ch = cx.char(pos);
      if (ch === 62 /* '>' */) return { from: start, to: pos + 1 };
      if (ch === 60 || ch === 10) return false;
    }
    return null;
  }
  let depth = 0;
  let escaped = false;
  let pos = start;
  for (; pos < cx.end; pos++) {
    const ch = cx.char(pos);
    if (isSpaceCode(ch)) break;
    else if (escaped) escaped = false;
    else if (ch === 40 /* '(' */) depth++;
    else if (ch === 41 /* ')' */) {
      if (!depth) break;
      depth--;
    } else if (ch === 92 /* '\\' */) escaped = true;
  }
  return pos > start ? { from: start, to: pos } : pos === cx.end ? null : false;
}

function parseLinkTitle(cx: InlineContext, start: number): { from: number; to: number } | null | false {
  const next = cx.char(start);
  if (next !== 39 && next !== 34 && next !== 40 /* ' " ( */) return false;
  const end = next === 40 ? 41 : next;
  let escaped = false;
  for (let pos = start + 1; pos < cx.end; pos++) {
    const ch = cx.char(pos);
    if (escaped) escaped = false;
    else if (ch === end) return { from: start, to: pos + 1 };
    else if (ch === 92) escaped = true;
  }
  return null;
}

function parseLinkLabel(cx: InlineContext, start: number, requireNonWS: boolean): { from: number; to: number } | null | false {
  const limit = Math.min(cx.end, start + 1 + 999);
  let escaped = false;
  let needNonWS = requireNonWS;
  for (let pos = start + 1; pos < limit; pos++) {
    const ch = cx.char(pos);
    if (escaped) escaped = false;
    else if (ch === 93 /* ']' */) return needNonWS ? false : { from: start, to: pos + 1 };
    else {
      if (needNonWS && !isSpaceCode(ch)) needNonWS = false;
      if (ch === 91 /* '[' */) return false;
      else if (ch === 92) escaped = true;
    }
  }
  return null;
}

function finishLink(cx: InlineContext, content: Element[], isImage: boolean, start: number, startPos: number): Element {
  const nodeType = isImage ? "Image" : "Link";
  const out: Element[] = [
    cx.elt("LinkMark", start, start + (isImage ? 2 : 1)),
    ...content,
    cx.elt("LinkMark", startPos - 1, startPos),
  ];
  let endPos = startPos;
  const next = cx.char(startPos);
  if (next === 40 /* '(' */) {
    let pos = cx.skipSpace(startPos + 1);
    const dest = parseURL(cx, pos);
    let title: { from: number; to: number } | null | false = null;
    if (dest) {
      pos = cx.skipSpace(dest.to);
      if (pos !== dest.to) {
        title = parseLinkTitle(cx, pos);
        if (title) pos = cx.skipSpace(title.to);
      }
    }
    if (cx.char(pos) === 41 /* ')' */) {
      out.push(cx.elt("LinkMark", startPos, startPos + 1));
      endPos = pos + 1;
      if (dest) out.push(cx.elt("URL", dest.from, dest.to));
      if (title) out.push(cx.elt("LinkTitle", title.from, title.to));
      out.push(cx.elt("LinkMark", pos, endPos));
    }
  } else if (next === 91 /* '[' */) {
    const label = parseLinkLabel(cx, startPos, false);
    if (label) {
      out.push(cx.elt("LinkLabel", label.from, label.to));
      endPos = label.to;
    }
  }
  return cx.elt(nodeType, start, endPos, out);
}

function hasEarlierOpenBracket(cx: InlineContext, beforeIndex: number): boolean {
  for (let j = 0; j < beforeIndex; j++) {
    const d = asInternal(cx.getDelimiterAt(j));
    if (d && (d.type === InlineContext.linkStart || d.type === InlineContext.imageStart) && (d.side & 1)) return true;
  }
  return false;
}

function nestingAwareLinkEnd(cx: InlineContext, next: number, start: number): number {
  if (next !== 93 /* ']' */) return -1;

  const linkIdx = cx.findOpeningDelimiter(InlineContext.linkStart);
  const imageIdx = cx.findOpeningDelimiter(InlineContext.imageStart);
  const idx = linkIdx == null ? imageIdx : imageIdx == null ? linkIdx : Math.max(linkIdx, imageIdx);
  if (idx == null) return -1;

  const part = asInternal(cx.getDelimiterAt(idx));
  if (!part) return -1;

  const hasImmediateDest = /[(\[]/.test(cx.slice(start + 1, start + 2));
  const isEmptyText = cx.skipSpace(part.to) === start;

  if (!part.side || (isEmptyText && !hasImmediateDest)) {
    part.side = 0;
    return -1;
  }

  if (!hasImmediateDest && hasEarlierOpenBracket(cx, idx)) {
    // Bare shortcut-reference shape, nested inside another pending bracket —
    // defer so the enclosing `[...]`/`![...]` can still claim this text as
    // literal content if it later completes with `(url)` or `[id]`.
    part.side = 0;
    return -1;
  }

  const isImage = part.type === InlineContext.imageStart;
  const content = cx.takeContent(idx);
  const link = finishLink(cx, content, isImage, part.from, start + 1);

  if (!isImage) {
    for (let j = 0; j < idx; j++) {
      const p = asInternal(cx.getDelimiterAt(j));
      if (p && p.type === InlineContext.linkStart) p.side = 0;
    }
  }

  cx.addElement(link);
  return link.to;
}

export const nestingAwareLinkExtension: MarkdownConfig = {
  remove: ["LinkEnd"],
  parseInline: [{ name: "LinkEnd", parse: nestingAwareLinkEnd }],
};
