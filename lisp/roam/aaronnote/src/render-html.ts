import MarkdownIt from "markdown-it";
import { full as emoji } from "markdown-it-emoji";
import type Token from "markdown-it/lib/token.mjs";
import type StateBlock from "markdown-it/lib/rules_block/state_block.mjs";
import type StateCore from "markdown-it/lib/rules_core/state_core.mjs";
import type StateInline from "markdown-it/lib/rules_inline/state_inline.mjs";

import { cleanEditorHTML } from "./export-html.ts";
import { supportedDiagramLang } from "./diagram-langs.ts";
import { imageLayoutClasses, imageLayoutFromAttrs, imageLayoutStyle, readImageTrailingAttrs } from "./image-attrs.ts";
import { layoutClasses, layoutFromAttrs, layoutStyle, readLayoutAttrsLine, type LayoutAttrs } from "./layout-attrs.ts";
import { renderMathHTML } from "./math-render.ts";
import { markdownLinkDestination } from "./markdown-link.ts";
import { safeHref } from "./url-safety.ts";
import { scanInlineCommands } from "./command-syntax.ts";
import { semanticOutlineFromCommand } from "./semantic-outline.ts";
import { renderTikzIframe } from "./tikz-render.ts";
import {
  VISUAL_ATTACHMENT_IFRAME_ALLOW,
  visualAttachmentEmbeddableP,
  visualAttachmentFrame,
  visualAttachmentKind,
  visualAttachmentSandbox,
  visualAttachmentTitle,
  type VisualAttachmentKind,
} from "./visual-attachments.ts";

declare global {
  interface Window {
    AaronnoteResolveAssetUrl?: (src: string) => string;
  }
}

export type RenderMarkdownHTMLOptions = {
  assetResolver?: (src: string) => string;
  /** Allow authored HTML, then pass it through the normal Aaronnote sanitizer. */
  allowHtml?: boolean;
};

export type RenderPublishedNoteOptions = {
  title: string;
  group?: string;
  date?: string;
  root?: string;
  kind?: string;
  format?: "html" | "pdf";
  noteThemeVersion?: string;
  kindAssetsHtml?: string;
  private?: boolean;
  includePrivateContent?: boolean;
};

type OrgEnvTokenMeta = {
  kind: string;
  title: string;
  body: string;
};

type SemanticHeadingTokenMeta = {
  kind: string;
  label: string;
  level: number;
  text: string;
  slug: string;
  attrs: Record<string, string>;
};

type CiteTokenMeta = {
  namespace: string;
  keys: string[];
  args: Record<string, string>;
};

const ORG_ENV_OPEN_RE = /^\s*#\+\s*begin\s+(\S+)(?:[ \t]+([^\n]*?))?[ \t]*$/i;
const TABLE_ROW_LINE_RE = /^\s*\|.*\|\s*$/;
const FENCE_CLOSE_LINE_RE = /^[ \t]{0,3}(`{3,}|~{3,})\s*$/;
const CEIL_COMMAND_LINE_RE = /^\s*@@cell(?:[ \t]*\([^)\n]*\))?(?:[ \t]+\[[^\]\n]*\])?[ \t]*$/i;

function escapeHtml(value: string): string {
  return value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function escapeAttr(value: string): string {
  return escapeHtml(value);
}

/** Join human-readable citation affixes without inserting spaces before closing punctuation. */
export function formatCitationLabel(label: string, prefix = "", suffix = ""): string {
  const cleanPrefix = String(prefix || "").trim();
  const cleanSuffix = String(suffix || "").trim();
  const prefixGlue = cleanPrefix && !/[\s([{‘“«]$/u.test(cleanPrefix) ? " " : "";
  const suffixGlue = cleanSuffix && !/^[\s,.;:!?%\])}’”»]/u.test(cleanSuffix) ? " " : "";
  return `${cleanPrefix}${prefixGlue}${label}${suffixGlue}${cleanSuffix}`;
}

function safeNoteKind(value: string | undefined): string {
  const kind = String(value || "default").toLowerCase();
  return /^[a-z0-9_-]+$/.test(kind) ? kind : "default";
}

function classList(...parts: Array<string | false | undefined>): string {
  return parts.filter(Boolean).join(" ");
}

export function parseMetaEntries(body: string): Array<{ key: string; value: string }> {
  return body
    .split(/\r?\n/)
    .map((line) => line.match(/^\s*([A-Za-z0-9_-]+)\s*:\s*(.*?)\s*$/))
    .filter((match): match is RegExpMatchArray => Boolean(match))
    .map((match) => ({ key: match[1]!, value: match[2] ?? "" }));
}

export function metaEntryMap(entries: Array<{ key: string; value: string }>): Map<string, string> {
  return new Map(entries.map((entry) => [entry.key.toLowerCase(), entry.value]));
}

export function metaTags(value: string): string[] {
  return String(value || "")
    .split(",")
    .map((tag) => tag.trim().replace(/^#/, ""))
    .filter(Boolean);
}

export function showMetaTag(tag: string): boolean {
  return !/[\\/_]/.test(tag);
}

function unquoteMetaScalar(value: string): string {
  const trimmed = String(value || "").trim();
  if ((trimmed.startsWith('"') && trimmed.endsWith('"')) || (trimmed.startsWith("'") && trimmed.endsWith("'"))) {
    return trimmed.slice(1, -1);
  }
  return trimmed;
}

export function metaRoamIndexed(entries: Array<{ key: string; value: string }>): boolean {
  const byKey = metaEntryMap(entries);
  const id = unquoteMetaScalar(byKey.get("id") || "").trim();
  const roam = unquoteMetaScalar(byKey.get("roam") || "").trim().toLowerCase();
  return id.length > 0 && roam !== "off";
}

export function cssHrefFromMetaPath(value: string): string {
  const raw = unquoteMetaScalar(value).replace(/\\_/g, "_");
  if (!raw) return "";
  if (/^https?:\/\//i.test(raw) || /^file:\/\//i.test(raw)) return raw;
  if (/^[A-Za-z]:[\\/]/.test(raw)) return encodeURI(`file:///${raw.replace(/\\/g, "/")}`);
  if (raw.startsWith("/") && !raw.startsWith("//")) return encodeURI(`file://${raw}`);
  return "";
}

export function noteCssHrefFromMarkdown(markdown: string): string {
  const text = String(markdown || "");
  const org = text.match(/^\s*#\+begin\s+meta\s*\r?\n([\s\S]*?)\r?\n\s*#\+end\s+meta\s*$/im);
  const yaml = text.match(/^\s*---\s*\r?\n([\s\S]*?)\r?\n---\s*(?:\r?\n|$)/);
  const entries = metaEntryMap(parseMetaEntries(org?.[1] ?? yaml?.[1] ?? ""));
  return cssHrefFromMetaPath(entries.get("css") || "");
}

function renderMetaCover(body: string): string {
  const entries = parseMetaEntries(body);
  const roamBadge = metaRoamIndexed(entries)
    ? ""
    : '<span class="aaronnote-meta-roam-badge" title="Not in roam database" aria-label="Not in roam database">🔕</span>';
  if (entries.length === 0) {
    return [
      '<div class="cm-org-env-block org-env-block" data-kind="meta" data-label="Meta">',
      '<div class="org-env-meta aaronnote-meta-cover">',
      roamBadge,
      '<span class="org-env-meta-empty">No metadata</span>',
      "</div>",
      "</div>",
    ].join("");
  }

  const byKey = metaEntryMap(entries);
  const title = byKey.get("title") || "Untitled";
  const date = byKey.get("date") || "";
  const tags = metaTags(byKey.get("tags") || "")
    .filter(showMetaTag)
    .map((tag) => `<button class="aaronnote-meta-tag">#${escapeHtml(tag)}</button>`)
    .join("");

  return [
    '<div class="cm-org-env-block org-env-block" data-kind="meta" data-label="Meta">',
    '<div class="org-env-meta aaronnote-meta-cover">',
    roamBadge,
    `<h1 class="aaronnote-meta-title">${escapeHtml(title)}</h1>`,
    date ? `<p class="aaronnote-meta-date">${escapeHtml(date)}</p>` : "",
    tags ? `<nav class="aaronnote-meta-tags" aria-label="Tags">${tags}</nav>` : "",
    "</div>",
    "</div>",
  ].join("");
}

function resolveAssetSrc(src: string, resolver?: (src: string) => string): string {
  const raw = String(src || "").trim();
  if (!raw) return raw;
  return resolver?.(raw) ?? window.AaronnoteResolveAssetUrl?.(raw) ?? raw;
}

function envLabel(kind: string): string {
  const labels: Record<string, string> = {
    meta: "Meta",
    theorem: "Theorem",
    thm: "Theorem",
    definition: "Definition",
    defn: "Definition",
    lemma: "Lemma",
    corollary: "Corollary",
    cor: "Corollary",
    proposition: "Proposition",
    prop: "Proposition",
    property: "Property",
    proof: "Proof",
    example: "Example",
    attention: "Attention",
    warning: "Warning",
    note: "Note",
    info: "Info",
    comment: "Comment",
    summary: "Summary",
    fold: "Fold",
    tikz: "TikZ",
    convention: "Convention",
    axiom: "Axiom",
    assumption: "Assumption",
    conjecture: "Conjecture",
    claim: "Claim",
    remark: "Remark",
    notation: "Notation",
    observation: "Observation",
    exercise: "Exercise",
    solution: "Solution",
    algorithm: "Algorithm",
    question: "Question",
  };
  return labels[kind] ?? kind;
}

function lineText(state: StateBlock, line: number): string {
  return state.src.slice(state.bMarks[line]! + state.tShift[line]!, state.eMarks[line]);
}

function orgEnvBlockRule(state: StateBlock, startLine: number, endLine: number, silent: boolean): boolean {
  const openLine = lineText(state, startLine);
  const open = openLine.match(ORG_ENV_OPEN_RE);
  if (!open) return false;
  const kind = open[1]!;
  if (kind.toLowerCase() === "lean4") return false;
  const closeRe = new RegExp(`^\\s*#\\+\\s*end\\s+${kind.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\s*$`, "i");

  let closeLine = -1;
  for (let line = startLine + 1; line < endLine; line++) {
    if (closeRe.test(lineText(state, line))) {
      closeLine = line;
      break;
    }
  }
  if (closeLine < 0) return false;
  if (silent) return true;

  const bodyStart = state.bMarks[startLine + 1] ?? state.eMarks[startLine];
  const bodyEnd = state.bMarks[closeLine] ?? state.eMarks[closeLine];
  const token = state.push("org_env_block", "org-env-block", 0);
  token.block = true;
  token.map = [startLine, closeLine + 1];
  token.meta = {
    kind,
    title: open[2]?.trim() ?? "",
    body: state.src.slice(bodyStart, bodyEnd).replace(/\n$/, ""),
  } satisfies OrgEnvTokenMeta;
  state.line = closeLine + 1;
  return true;
}

function mathBlockRule(state: StateBlock, startLine: number, endLine: number, silent: boolean): boolean {
  const open = lineText(state, startLine);
  if (!/^\s*\\\[\s*$/.test(open)) return false;
  let closeLine = -1;
  for (let line = startLine + 1; line < endLine; line++) {
    if (/^\s*\\\]\s*$/.test(lineText(state, line))) {
      closeLine = line;
      break;
    }
  }
  if (closeLine < 0) return false;
  if (silent) return true;

  const bodyStart = state.bMarks[startLine + 1] ?? state.eMarks[startLine];
  const bodyEnd = state.bMarks[closeLine] ?? state.eMarks[closeLine];
  const token = state.push("math_block", "math-block", 0);
  token.block = true;
  token.map = [startLine, closeLine + 1];
  token.content = state.src.slice(bodyStart, bodyEnd).trim();
  state.line = closeLine + 1;
  return true;
}

function semanticHeadingBlockRule(state: StateBlock, startLine: number, _endLine: number, silent: boolean): boolean {
  const raw = lineText(state, startLine);
  const trimmed = raw.trim();
  if (!trimmed.startsWith("@@part") && !trimmed.startsWith("@@section")) return false;
  const command = scanInlineCommands(trimmed)[0];
  if (!command || command.fullFrom !== 0 || command.fullTo !== trimmed.length) return false;
  const outline = semanticOutlineFromCommand(command);
  if (!outline) return false;
  if (silent) return true;
  const token = state.push("semantic_heading_block", "div", 0);
  token.block = true;
  token.map = [startLine, startLine + 1];
  token.meta = {
    kind: outline.kind,
    label: outline.label,
    level: outline.level,
    text: outline.text,
    slug: outline.slug,
    attrs: outline.attrs,
  } satisfies SemanticHeadingTokenMeta;
  state.line = startLine + 1;
  return true;
}

function frontMatterRule(state: StateBlock, startLine: number, endLine: number, silent: boolean): boolean {
  if (startLine !== 0 || !/^---\s*$/.test(lineText(state, startLine))) return false;
  let closeLine = -1;
  for (let line = startLine + 1; line < endLine; line++) {
    if (/^---\s*$/.test(lineText(state, line))) {
      closeLine = line;
      break;
    }
  }
  if (closeLine < 0) return false;
  if (silent) return true;
  const bodyStart = state.bMarks[startLine + 1] ?? state.eMarks[startLine];
  const bodyEnd = state.bMarks[closeLine] ?? state.eMarks[closeLine];
  const token = state.push("front_matter", "yaml-block", 0);
  token.block = true;
  token.map = [startLine, closeLine + 1];
  token.content = state.src.slice(bodyStart, bodyEnd).trim();
  state.line = closeLine + 1;
  return true;
}

function tocRule(state: StateBlock, startLine: number, _endLine: number, silent: boolean): boolean {
  if (!/^\s*\[toc\]\s*$/i.test(lineText(state, startLine))) return false;
  if (silent) return true;
  const token = state.push("toc_block", "div", 0);
  token.block = true;
  token.map = [startLine, startLine + 1];
  state.line = startLine + 1;
  return true;
}

function privateCommandLineRule(state: StateBlock, startLine: number, _endLine: number, silent: boolean): boolean {
  const raw = lineText(state, startLine);
  const trimmed = raw.trim();
  if (!trimmed) return false;
  if (CEIL_COMMAND_LINE_RE.test(raw)) {
    if (silent) return true;
    const token = state.push("private_command_line", "", 0);
    token.block = true;
    token.hidden = true;
    token.map = [startLine, startLine + 1];
    state.line = startLine + 1;
    return true;
  }
  const command = scanInlineCommands(trimmed)[0];
  if (!command || command.fullFrom !== 0 || command.fullTo !== trimmed.length) return false;
  if (!["todo", "itodo", "comment"].includes(command.name)) return false;
  if (silent) return true;
  const token = state.push("private_command_line", "", 0);
  token.block = true;
  token.hidden = true;
  token.map = [startLine, startLine + 1];
  state.line = startLine + 1;
  return true;
}

function mathInlineRule(state: StateInline, silent: boolean): boolean {
  const start = state.pos;
  // Inline math opens with the literal LaTeX delimiter `\(` and closes with `\)`.
  if (state.src[start] !== "\\" || state.src[start + 1] !== "(") return false;
  const end = state.src.indexOf("\\)", start + 2);
  if (end < 0 || end === start + 2) return false;
  const tex = state.src.slice(start + 2, end);
  if (tex.includes("\n")) return false;
  if (silent) return true;
  const token = state.push("math_inline", "span", 0);
  token.content = tex;
  state.pos = end + 2;
  return true;
}

function markdownEscapedAt(source: string, index: number): boolean {
  let slashes = 0;
  for (let pos = index - 1; pos >= 0 && source[pos] === "\\"; pos--) slashes++;
  return slashes % 2 === 1;
}

function insideHtmlComment(source: string, index: number): boolean {
  return source.lastIndexOf("<!--", index) > source.lastIndexOf("-->", index);
}

/** Render @@cite source as stable, hydratable HTML even without a bibliography model. */
function citeInlineRule(state: StateInline, silent: boolean): boolean {
  const start = state.pos;
  if (state.src.slice(start, start + 6).toLowerCase() !== "@@cite") return false;
  if (markdownEscapedAt(state.src, start) || insideHtmlComment(state.src, start)) return false;
  const lineEnd = state.src.indexOf("\n", start);
  const slice = state.src.slice(start, lineEnd < 0 ? state.src.length : lineEnd);
  const command = scanInlineCommands(slice, "cite")[0];
  if (!command || command.fullFrom !== 0) return false;
  if (silent) return true;
  const token = state.push("cite_inline", "span", 0);
  token.meta = {
    namespace: command.switchValue.trim(),
    keys: command.context.split(";").map((key) => key.trim()).filter(Boolean),
    args: command.args,
  } satisfies CiteTokenMeta;
  state.pos = start + command.fullTo;
  return true;
}

/**
 * `@@comment [text]{args}` — a private annotation. Exported HTML uses the
 * same classes/structure as the editor chip (button + `.org-env-content`)
 * but without a `hidden` attribute or toggle script, matching how the
 * sibling `#+begin comment` block already exports its content unhidden
 * (org-env-comment collapse is `.cm-editor`-scoped CSS, not present outside
 * the live editor).
 */
function commentInlineRule(state: StateInline, silent: boolean): boolean {
  const start = state.pos;
  if (!state.src.startsWith("@@comment", start)) return false;
  const lineEnd = state.src.indexOf("\n", start);
  const slice = state.src.slice(start, lineEnd < 0 ? state.src.length : lineEnd);
  const cmd = scanInlineCommands(slice, "comment")[0];
  if (!cmd || cmd.fullFrom !== 0) return false;
  if (silent) return true;
  const token = state.push("comment_inline", "span", 0);
  token.content = cmd.context.trim();
  state.pos = start + cmd.fullTo;
  return true;
}

function sideCommentInlineRule(state: StateInline, silent: boolean): boolean {
  const start = state.pos;
  if (!state.src.startsWith("@@scomment", start)) return false;
  const lineEnd = state.src.indexOf("\n", start);
  const slice = state.src.slice(start, lineEnd < 0 ? state.src.length : lineEnd);
  const cmd = scanInlineCommands(slice, "scomment")[0];
  if (!cmd || cmd.fullFrom !== 0) return false;
  if (silent) return true;
  const token = state.push("side_comment_inline", "span", 0);
  token.content = cmd.context.trim();
  state.pos = start + cmd.fullTo;
  return true;
}

function privateInlineRule(state: StateInline, silent: boolean): boolean {
  const start = state.pos;
  if (!state.src.startsWith("@@todo", start) && !state.src.startsWith("@@itodo", start)) return false;
  const lineEnd = state.src.indexOf("\n", start);
  const slice = state.src.slice(start, lineEnd < 0 ? state.src.length : lineEnd);
  const cmd = scanInlineCommands(slice)[0];
  if (!cmd || cmd.fullFrom !== 0 || !["todo", "itodo"].includes(cmd.name)) return false;
  if (silent) return true;
  const token = state.push("private_inline", "", 0);
  token.hidden = true;
  state.pos = start + cmd.fullTo;
  return true;
}

function renderCommentInline(tokens: Token[], idx: number, _options: unknown, _env: unknown, _renderer: MarkdownIt["renderer"]): string {
  const context = tokens[idx]!.content;
  const body = context ? renderMarkdownInlineHTML(context) : "";
  return [
    '<span class="inline-comment-widget inline-command-token" data-comment-open="false">',
    '<span class="org-env-comment-button" aria-expanded="false">',
    '<span class="org-env-comment-label">comment</span>',
    '<span class="org-env-comment-state">show</span>',
    "</span>",
    `<span class="org-env-content">${body}</span>`,
    "</span>",
  ].join("");
}

function renderSideCommentInline(tokens: Token[], idx: number): string {
  const context = tokens[idx]!.content;
  const body = context ? renderMarkdownInlineHTML(context) : "";
  return [
    '<span class="inline-side-comment-widget inline-command-token" role="note" aria-label="Side comment">',
    '<span class="inline-side-comment-anchor" aria-hidden="true">',
    '<span class="inline-side-comment-connector"></span>',
    "</span>",
    `<span class="inline-side-comment-card">${body}</span>`,
    "</span>",
  ].join("");
}

function renderCiteInline(tokens: Token[], idx: number): string {
  const meta = (tokens[idx]!.meta || {}) as Partial<CiteTokenMeta>;
  const namespace = String(meta.namespace || "").trim();
  const keys = Array.isArray(meta.keys) ? meta.keys.map(String).map((key) => key.trim()).filter(Boolean) : [];
  const args = meta.args || {};
  const locator = String(args.locator || args.page || args.pages || "").trim();
  const prefix = String(args.prefix || "").trim();
  const suffix = String(args.suffix || "").trim();
  const keyText = keys.join("; ") || "?";
  const label = formatCitationLabel(`[${keyText}${locator ? `, ${locator}` : ""}]`, prefix, suffix);
  const description = `Citation ${namespace ? `${namespace}:` : ""}${keyText}`;
  const attrs = [
    'class="inline-cite-widget inline-command-token is-unresolved"',
    'data-cite-state="unresolved"',
    `data-cite-namespace="${escapeAttr(namespace)}"`,
    `data-cite-keys="${escapeAttr(keys.join(";"))}"`,
    `aria-label="${escapeAttr(description)}"`,
    'role="doc-biblioref"',
  ];
  if (locator) attrs.push(`data-cite-locator="${escapeAttr(locator)}"`);
  if (prefix) attrs.push(`data-cite-prefix="${escapeAttr(prefix)}"`);
  if (suffix) attrs.push(`data-cite-suffix="${escapeAttr(suffix)}"`);
  return `<span ${attrs.join(" ")}>${escapeHtml(label)}</span>`;
}

function renderMath(tex: string, displayMode: boolean): { html: string; error?: string } {
  return renderMathHTML(tex, {
    displayMode,
    throwOnError: false,
    strict: false,
    trust: false,
    output: "html",
  });
}

function renderMathBlock(tokens: Token[], idx: number, _options: unknown, _env: unknown, renderer: MarkdownIt["renderer"]): string {
  const tex = tokens[idx]!.content;
  const escaped = escapeAttr(tex);
  const rendered = renderMath(tex, true);
  const cls = rendered.error ? "aaronnote-math-block math-block-render aaronnote-math-error" : "aaronnote-math-block math-block-render";
  const html = rendered.error
    ? escapeHtml(rendered.error)
    : rendered.html || renderer.rules.text?.(tokens, idx, {}, {}, renderer) || renderer.renderToken(tokens, idx, {});
  return `<math-block data-aaronnote-math-block="" class="math-block-rendered" data-tex="${escaped}"><div class="${cls}" data-tex="${escaped}" data-math-render-key="display\n${escaped}">${html}</div></math-block>`;
}

function renderMathInline(tokens: Token[], idx: number, _options: unknown, _env: unknown, _renderer: MarkdownIt["renderer"]): string {
  const tex = tokens[idx]!.content;
  const escaped = escapeAttr(tex);
  const rendered = renderMath(tex, false);
  const cls = rendered.error ? "aaronnote-math-inline aaronnote-math-error" : "aaronnote-math-inline";
  const html = rendered.error ? escapeHtml(rendered.error) : rendered.html || escapeHtml(`\\(${tex}\\)`);
  return `<span class="${cls}" data-tex="${escaped}" data-math-render-key="inline\n${escaped}">${html}</span>`;
}

function isRoamCoreHref(href: string): boolean {
  const raw = String(href || "").trim();
  if (!raw) return false;
  if (/^roam:\/\//i.test(raw)) return true;
  if (/^[A-Za-z][\w+.-]*:/i.test(raw)) return false;
  if (raw.startsWith("#") || raw.startsWith("@")) return false;
  if (/\.ipynb/i.test(raw)) return false;
  return raw.includes("#") || raw.includes("@");
}

function isJupyterHref(href: string): boolean {
  const raw = String(href || "").trim();
  if (!raw) return false;
  if (/^[A-Za-z][\w+.-]*:/i.test(raw) && !/^file:/i.test(raw)) return false;
  return /\.ipynb(?:[?@#]|$)/i.test(raw);
}

function isZoteroHref(href: string): boolean {
  return /^zotero:\/\//i.test(String(href || "").trim());
}

function joinTokenStyle(token: Token, style: string): void {
  if (!style) return;
  const current = token.attrGet("style");
  token.attrSet("style", current ? `${current.trim().replace(/;?$/, ";")} ${style}` : style);
}

function applyImageAttrs(tokens: Token[], idx: number): void {
  const token = tokens[idx]!;
  const next = tokens[idx + 1];
  if (!next || next.type !== "text") return;
  const trailing = readImageTrailingAttrs(next.content, 0);
  if (!trailing) return;
  const layout = imageLayoutFromAttrs(trailing.attrs);
  next.content = next.content.slice(trailing.to);
  token.attrJoin("class", imageLayoutClasses(layout));
  token.attrSet("data-aaronnote-image-align", layout.align);
  token.attrSet("data-aaronnote-image-wrap", layout.wrap ? "true" : "false");
  joinTokenStyle(token, imageLayoutStyle(layout));
}

function renderVisualAttachmentImage(token: Token, kind: VisualAttachmentKind, resolvedSrc: string): string {
  const alt = token.content || token.attrGet("alt") || "";
  const classes = [
    "cm-image-widget",
    "aaronnote-visual-attachment",
    `aaronnote-visual-attachment-${kind}`,
    token.attrGet("class") || "",
  ].join(" ").trim().replace(/\s+/g, " ");
  const attrs = [
    `class="${escapeAttr(classes)}"`,
    `data-aaronnote-visual-kind="${escapeAttr(kind)}"`,
  ];
  const style = token.attrGet("style");
  if (style) attrs.push(`style="${escapeAttr(style)}"`);
  for (const name of ["data-aaronnote-image-align", "data-aaronnote-image-wrap"]) {
    const value = token.attrGet(name);
    if (value) attrs.push(`${name}="${escapeAttr(value)}"`);
  }

  const body = visualAttachmentEmbeddableP(kind, resolvedSrc)
    ? (() => {
      const frame = visualAttachmentFrame(kind, resolvedSrc);
      const frameAttrs = [
        `class="cm-image-render aaronnote-visual-embed aaronnote-visual-embed-${escapeAttr(kind)}"`,
        `title="${escapeAttr(visualAttachmentTitle(kind, alt))}"`,
        'loading="lazy"',
        `allow="${escapeAttr(VISUAL_ATTACHMENT_IFRAME_ALLOW)}"`,
        'referrerpolicy="no-referrer-when-downgrade"',
      ];
      frameAttrs.push(`sandbox="${escapeAttr(visualAttachmentSandbox(kind))}"`);
      if (frame.mode === "src") {
        frameAttrs.push(`src="${escapeAttr(frame.src)}"`);
      } else {
        frameAttrs.push(`srcdoc="${escapeAttr(frame.srcdoc)}"`);
      }
      return `<iframe ${frameAttrs.join(" ")}></iframe>`;
    })()
    : `<div class="cm-image-render cm-visual-file-card cm-visual-file-card-${escapeAttr(kind)}" title="${escapeAttr(`System Open: ${resolvedSrc}`)}">${escapeHtml(visualAttachmentTitle(kind, alt))}</div>`;

  const caption = alt.trim()
    ? `<figcaption class="cm-image-caption">${escapeHtml(alt.trim())}</figcaption>`
    : "";
  return `<figure ${attrs.join(" ")}>${body}${caption}</figure>`;
}

function happyDomTestEnvironmentP(): boolean {
  return typeof navigator !== "undefined" && /\bHappyDOM\//.test(navigator.userAgent);
}

function protectIframeNavigationAttrsForDom(html: string): { html: string; attrs: string[] } {
  const attrs: string[] = [];
  if (!happyDomTestEnvironmentP()) return { html, attrs };
  return {
    attrs,
    html: html.replace(/<iframe\b[^>]*>/g, (tag) =>
      tag.replace(/\s(srcdoc|src)="([^"]*)"/g, (_match, name: string, value: string) => {
        const index = attrs.push(`${name}="${value}"`) - 1;
        return ` data-aaronnote-dom-${name}-index="${index}"`;
      })),
  };
}

function restoreIframeNavigationAttrsFromDom(html: string, attrs: readonly string[]): string {
  return html
    .replace(/\sdata-aaronnote-dom-srcdoc-index="(\d+)"/g, (_match, rawIndex: string) => {
      const attr = attrs[Number(rawIndex)];
      return attr ? ` ${attr}` : "";
    })
    .replace(/\sdata-aaronnote-dom-src-index="(\d+)"/g, (_match, rawIndex: string) => {
      const attr = attrs[Number(rawIndex)];
      return attr ? ` ${attr}` : "";
    });
}

function emptyHtmlLinkEmbedRule(state: StateInline, silent: boolean): boolean {
  if (state.src.charCodeAt(state.pos) !== 0x5b || state.src.charCodeAt(state.pos + 1) !== 0x5d) return false;
  const match = state.src.slice(state.pos).match(/^\[\]\(([^)\n]+)\)/);
  if (!match) return false;
  const alt = "";
  const src = markdownLinkDestination(match[1] ?? "");
  if (!safeHref(src) || visualAttachmentKind(src) !== "html") return false;
  if (!silent) {
    const token = state.push("image", "img", 0);
    token.attrs = [["src", src], ["alt", alt]];
    token.children = [];
    token.content = alt;
  }
  state.pos += match[0]!.length;
  return true;
}

function jupyterLinkRule(state: StateInline, silent: boolean): boolean {
  const start = state.pos;
  if (state.src.charCodeAt(start) !== 0x5b /* [ */) return false;
  const closeLabel = state.src.indexOf("]", start + 1);
  if (closeLabel < 0 || state.src.charCodeAt(closeLabel + 1) !== 0x28 /* ( */) return false;
  const closeHref = state.src.indexOf(")", closeLabel + 2);
  if (closeHref < 0) return false;
  const label = state.src.slice(start + 1, closeLabel);
  const href = markdownLinkDestination(state.src.slice(closeLabel + 2, closeHref));
  if (!label || label.includes("\n") || href.includes("\n") || !isJupyterHref(href) || !safeHref(href)) return false;
  if (silent) return true;
  const open = state.push("link_open", "a", 1);
  open.attrs = [["href", href]];
  const text = state.push("text", "", 0);
  text.content = label;
  state.push("link_close", "a", -1);
  state.pos = closeHref + 1;
  return true;
}

function applyLayoutToToken(token: Token, kind: string, layout: LayoutAttrs): void {
  token.attrJoin("class", layoutClasses(kind, layout));
  token.attrSet("data-aaronnote-layout", kind);
  token.attrSet("data-aaronnote-layout-align", layout.align);
  token.attrSet("data-aaronnote-layout-wrap", layout.wrap ? "true" : "false");
  joinTokenStyle(token, layoutStyle(kind, layout));
}

function consumeLayoutAttrsParagraph(tokens: Token[], idx: number): LayoutAttrs | null {
  const open = tokens[idx];
  const inline = tokens[idx + 1];
  const close = tokens[idx + 2];
  if (!open || !inline || !close) return null;
  if (open.type !== "paragraph_open" || inline.type !== "inline" || close.type !== "paragraph_close") return null;
  const trailing = readLayoutAttrsLine(inline.content);
  if (!trailing) return null;
  open.hidden = true;
  inline.hidden = true;
  inline.content = "";
  inline.children = [];
  close.hidden = true;
  return layoutFromAttrs(trailing.attrs);
}

function findMatchingToken(tokens: Token[], idx: number, closeType: string): number {
  let depth = 0;
  for (let i = idx; i < tokens.length; i++) {
    depth += tokens[i]!.nesting;
    if (depth === 0 && tokens[i]!.type === closeType) return i;
  }
  return -1;
}

function applyTableAttrs(tokens: Token[], idx: number): void {
  const closeIdx = findMatchingToken(tokens, idx, "table_close");
  if (closeIdx < 0) return;
  const layout = consumeLayoutAttrsParagraph(tokens, closeIdx + 1);
  if (!layout) return;
  applyLayoutToToken(tokens[idx]!, "table", layout);
}

function diagramLangFromInfo(info: string): string {
  return String(info || "").trim().split(/\s+/, 1)[0] ?? "";
}

function renderDiagramFence(token: Token, layout: LayoutAttrs): string {
  const lang = diagramLangFromInfo(token.info);
  const cls = classList("aaronnote-diagram-code", layoutClasses("diagram", layout));
  const style = layoutStyle("diagram", layout);
  const styleAttr = style ? ` style="${escapeAttr(style)}"` : "";
  const codeClass = lang ? ` class="language-${escapeAttr(lang)}"` : "";
  return `<pre class="${escapeAttr(cls)}"${styleAttr}><code${codeClass}>${escapeHtml(token.content)}</code></pre>\n`;
}

function renderMarkdownSummary(md: MarkdownIt, title: string): string {
  return md.renderInline(title.trim() || "Details");
}

function renderSemanticHeading(tokens: Token[], idx: number): string {
  const meta = tokens[idx]!.meta as SemanticHeadingTokenMeta;
  const attrs = [
    `id="${escapeAttr(meta.slug)}"`,
    'class="aaronnote-section-heading"',
    `data-section-kind="${escapeAttr(meta.kind)}"`,
    `data-section-label="${escapeAttr(meta.label)}"`,
    `data-outline-level="${escapeAttr(String(meta.level))}"`,
  ];
  for (const [key, value] of Object.entries(meta.attrs || {})) {
    if (!/^[A-Za-z][\w-]*$/.test(key) || key.toLowerCase() === "id") continue;
    attrs.push(`data-section-${escapeAttr(key.toLowerCase())}="${escapeAttr(value)}"`);
  }
  return [
    `<div ${attrs.join(" ")}>`,
    '<div class="aaronnote-section-heading-inner">',
    `<span class="aaronnote-section-label">${escapeHtml(meta.label)}</span>`,
    `<span class="aaronnote-section-title">${escapeHtml(meta.text)}</span>`,
    "</div>",
    "</div>",
  ].join("");
}

function tikzTitleLayout(title: string): LayoutAttrs {
  const raw = String(title || "").trim();
  const open = raw.indexOf("{");
  if (open < 0) return layoutFromAttrs({});
  const trailing = readImageTrailingAttrs(raw, open);
  if (!trailing || raw.slice(trailing.to).trim()) return layoutFromAttrs({});
  return imageLayoutFromAttrs(trailing.attrs);
}

function renderOrgEnv(md: MarkdownIt, tokens: Token[], idx: number): string {
  const meta = tokens[idx]!.meta as OrgEnvTokenMeta;
  const kind = meta.kind;
  if (kind.toLowerCase() === "meta") return renderMetaCover(meta.body);
  if (kind.toLowerCase() === "html") {
    return meta.body.trim() ? `<div class="aaronnote-html">${meta.body}</div>` : "";
  }
  if (kind.toLowerCase() === "tikz") {
    const layout = tikzTitleLayout(meta.title);
    const classes = classList("aaronnote-tikz", imageLayoutClasses(layout));
    const style = imageLayoutStyle(layout);
    const styleAttr = style ? ` style="${escapeAttr(style)}"` : "";
    return meta.body.trim()
      ? `<div class="${escapeAttr(classes)}" data-aaronnote-image-align="${escapeAttr(layout.align)}" data-aaronnote-image-wrap="${layout.wrap ? "true" : "false"}"${styleAttr}>${renderTikzIframe(meta.body)}</div>`
      : "";
  }
  const title = meta.title;
  const label = envLabel(kind);
  const body = meta.body.trim() ? md.render(meta.body) : "";
  if (kind.toLowerCase() === "fold") {
    return [
      `<details class="org-env-fold" data-kind="fold" data-title="${escapeAttr(title)}" data-label="${escapeAttr(label)}">`,
      `<summary class="org-env-fold-summary">${renderMarkdownSummary(md, title)}</summary>`,
      `<div class="org-env-content">${body}</div>`,
      "</details>",
    ].join("");
  }
  return [
    `<org-env-block data-kind="${escapeAttr(kind)}" data-title="${escapeAttr(title)}" data-label="${escapeAttr(label)}" data-comment-open="false">`,
    `<span class="org-env-heading"><span class="org-env-heading-label">${escapeHtml(label)}</span><span class="org-env-heading-title" data-empty="${title ? "false" : "true"}">${md.renderInline(title)}</span></span>`,
    `<div class="org-env-content">${body}</div>`,
    "</org-env-block>",
  ].join("");
}

function applyTaskCheckboxes(root: HTMLElement): void {
  root.querySelectorAll<HTMLLIElement>("li").forEach((li) => {
    const first = li.firstChild;
    if (!(first instanceof Text)) return;
    const match = first.data.match(/^\[([ xX])\]\s+/);
    if (!match) return;
    first.data = first.data.slice(match[0].length);
    const box = document.createElement("span");
    box.className = "checkbox";
    box.dataset.checked = match[1]!.toLowerCase() === "x" ? "1" : "0";
    li.prepend(box);
  });
}

function isolateBlockLayoutAttrLines(markdown: string): string {
  const lines = String(markdown || "").split(/\r?\n/);
  const out: string[] = [];
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;
    const prev = out[out.length - 1] ?? "";
    if (
      readLayoutAttrsLine(line) &&
      prev.trim() &&
      (TABLE_ROW_LINE_RE.test(prev) || FENCE_CLOSE_LINE_RE.test(prev))
    ) {
      out.push("");
    }
    out.push(line);
  }
  return out.join("\n");
}

// Obsidian-style callout blocks: `> [!type] Optional title`
// Transforms blockquotes whose first paragraph starts with [!type] into
// `<blockquote class="callout callout-<type>" data-callout="<type>">` with a
// `<div class="callout-title">` header. Body content is unchanged.
function aaronnoteCalloutsRule(state: StateCore): void {
  const { tokens } = state;
  for (let i = 0; i < tokens.length; i++) {
    const bqOpen = tokens[i];
    if (!bqOpen || bqOpen.type !== "blockquote_open") continue;

    let paraOpen = -1, inlineIdx = -1, paraClose = -1;
    let depth = 1;
    for (let j = i + 1; j < tokens.length; j++) {
      const t = tokens[j]!;
      if (t.type === "blockquote_open") { depth++; continue; }
      if (t.type === "blockquote_close") { if (--depth === 0) break; continue; }
      if (depth !== 1) continue;
      if (t.type === "paragraph_open" && paraOpen < 0) paraOpen = j;
      if (t.type === "inline" && inlineIdx < 0 && paraOpen >= 0) inlineIdx = j;
      if (t.type === "paragraph_close" && paraClose < 0 && inlineIdx >= 0) { paraClose = j; break; }
    }

    if (inlineIdx < 0) continue;

    const inline = tokens[inlineIdx]!;
    const firstLine = inline.content.split("\n")[0] ?? "";
    const m = /^\[!(\w+)\](?:[ \t]+(.*))?$/.exec(firstLine);
    if (!m) continue;

    const calloutType = m[1]!.toLowerCase();
    const titleText = m[2] ?? "";

    bqOpen.attrJoin("class", `callout callout-${calloutType}`);
    bqOpen.attrSet("data-callout", calloutType);

    if (paraOpen >= 0 && paraClose >= 0) {
      tokens[paraOpen]!.tag = "div";
      tokens[paraOpen]!.attrJoin("class", "callout-title");
      tokens[paraClose]!.tag = "div";
    }

    // Strip [!type] from inline content; re-tokenize so inline formatting in title survives
    const restLines = inline.content.includes("\n") ? inline.content.split("\n").slice(1) : [];
    const newContent = restLines.length > 0 ? titleText + "\n" + restLines.join("\n") : titleText;
    inline.content = newContent;
    const parsed = state.md.parseInline(newContent, state.env as Record<string, unknown>);
    inline.children = parsed[0]?.children ?? [];
  }
}

function createMarkdownIt(options: RenderMarkdownHTMLOptions): MarkdownIt {
  const md = new MarkdownIt({
    html: options.allowHtml === true,
    linkify: true,
    typographer: false,
  }).use(emoji);

  md.validateLink = () => true;
  md.block.ruler.before("fence", "front_matter", frontMatterRule, { alt: [] });
  md.block.ruler.before("fence", "org_env_block", orgEnvBlockRule, { alt: ["paragraph", "reference", "blockquote"] });
  md.block.ruler.before("fence", "math_block", mathBlockRule, { alt: ["paragraph", "reference", "blockquote"] });
  md.block.ruler.before("paragraph", "semantic_heading_block", semanticHeadingBlockRule, { alt: ["paragraph"] });
  md.block.ruler.before("paragraph", "toc_block", tocRule, { alt: ["paragraph"] });
  md.block.ruler.before("paragraph", "private_command_line", privateCommandLineRule, { alt: ["paragraph"] });
  md.core.ruler.push("aaronnote_callouts", aaronnoteCalloutsRule);
  md.inline.ruler.before("link", "empty_html_link_embed", emptyHtmlLinkEmbedRule);
  md.inline.ruler.before("link", "jupyter_link", jupyterLinkRule);
  // Must run before `escape`: otherwise the backslash escape rule consumes the
  // `\(` opener as a literal `(` and inline math is never recognized.
  md.inline.ruler.before("escape", "math_inline", mathInlineRule);
  md.inline.ruler.before("escape", "cite_inline", citeInlineRule);
  md.inline.ruler.before("escape", "comment_inline", commentInlineRule);
  md.inline.ruler.before("escape", "side_comment_inline", sideCommentInlineRule);
  md.inline.ruler.before("escape", "private_inline", privateInlineRule);

  md.renderer.rules.math_block = renderMathBlock;
  md.renderer.rules.math_inline = renderMathInline;
  md.renderer.rules.cite_inline = renderCiteInline;
  md.renderer.rules.comment_inline = renderCommentInline;
  md.renderer.rules.side_comment_inline = renderSideCommentInline;
  md.renderer.rules.private_inline = () => "";
  md.renderer.rules.private_command_line = () => "";
  md.renderer.rules.org_env_block = (tokens, idx) => renderOrgEnv(md, tokens, idx);
  md.renderer.rules.semantic_heading_block = renderSemanticHeading;
  md.renderer.rules.front_matter = (tokens, idx, _opts, _env, _renderer) =>
    `<yaml-block><pre>${escapeHtml(tokens[idx]!.content)}</pre></yaml-block>`;
  md.renderer.rules.toc_block = () => `<div class="toc"><div class="toc-empty">(no headings yet)</div></div>`;

  const originalLinkOpen = md.renderer.rules.link_open ?? ((tokens, idx, opts, _env, self) => self.renderToken(tokens, idx, opts));
  md.renderer.rules.link_open = (tokens, idx, opts, env, self) => {
    const token = tokens[idx]!;
    const href = token.attrGet("href");
    if (href && !safeHref(href)) {
      const attrIndex = token.attrIndex("href");
      if (attrIndex >= 0) token.attrs?.splice(attrIndex, 1);
    } else if (href && isRoamCoreHref(href)) {
      token.attrJoin("class", "aaronnote-roam-link");
      token.attrSet("data-roam-link", "true");
    } else if (href && isJupyterHref(href)) {
      token.attrJoin("class", "aaronnote-jupyter-link");
      token.attrSet("data-jupyter-link", "true");
    } else if (href && isZoteroHref(href)) {
      token.attrJoin("class", "aaronnote-zotero-link");
      token.attrSet("data-zotero-link", "true");
    }
    return originalLinkOpen(tokens, idx, opts, env, self);
  };

  const originalTableOpen = md.renderer.rules.table_open ?? ((tokens, idx, opts, _env, self) => self.renderToken(tokens, idx, opts));
  md.renderer.rules.table_open = (tokens, idx, opts, env, self) => {
    applyTableAttrs(tokens, idx);
    return originalTableOpen(tokens, idx, opts, env, self);
  };

  const originalFence = md.renderer.rules.fence ?? ((tokens, idx, opts, _env, self) => self.renderToken(tokens, idx, opts));
  md.renderer.rules.fence = (tokens, idx, opts, env, self) => {
    const token = tokens[idx]!;
    if (supportedDiagramLang(token.info)) {
      const layout = consumeLayoutAttrsParagraph(tokens, idx + 1);
      if (layout) return renderDiagramFence(token, layout);
    }
    return originalFence(tokens, idx, opts, env, self);
  };

  const originalImage = md.renderer.rules.image ?? ((tokens, idx, opts, _env, self) => self.renderToken(tokens, idx, opts));
  md.renderer.rules.image = (tokens, idx, opts, env, self) => {
    const token = tokens[idx]!;
    applyImageAttrs(tokens, idx);
    const src = token.attrGet("src");
    if (src && !safeHref(src)) {
      const attrIndex = token.attrIndex("src");
      if (attrIndex >= 0) token.attrs?.splice(attrIndex, 1);
    } else if (src) {
      const kind = visualAttachmentKind(src);
      const resolvedSrc = resolveAssetSrc(src, options.assetResolver);
      if (kind) return renderVisualAttachmentImage(token, kind, resolvedSrc);
      token.attrSet("src", resolvedSrc);
    }
    return originalImage(tokens, idx, opts, env, self);
  };

  return md;
}

export function renderMarkdownHTML(
  markdown: string,
  options: RenderMarkdownHTMLOptions = {},
): string {
  const md = createMarkdownIt(options);
  const root = document.createElement("div");
  const protectedHtml = protectIframeNavigationAttrsForDom(md.render(isolateBlockLayoutAttrLines(markdown)));
  root.innerHTML = protectedHtml.html;
  applyTaskCheckboxes(root);
  return restoreIframeNavigationAttrsFromDom(cleanEditorHTML(root), protectedHtml.attrs);
}

export function renderMarkdownInlineHTML(
  markdown: string,
  options: RenderMarkdownHTMLOptions = {},
): string {
  const md = createMarkdownIt(options);
  const root = document.createElement("span");
  root.innerHTML = md.renderInline(markdown);
  return cleanEditorHTML(root);
}

export function renderPublishedNoteHTML(
  markdown: string,
  options: RenderPublishedNoteOptions,
): string {
  const root = options.root || "./";
  const assetRoot = escapeAttr(root);
  const version = options.noteThemeVersion || "dev";
  const title = options.title || "Untitled";
  const group = options.group || "Root";
  const date = options.date || "Undated";
  const kind = safeNoteKind(options.kind);
  const format = options.format === "pdf" ? "pdf" : "html";
  const pdf = format === "pdf";
  const hidden = Boolean(options.private && !options.includePrivateContent);
  const contentHtml = hidden
    ? '<p class="sealed-note-message">This note has been sealed by the administrator.</p>'
    : renderMarkdownHTML(markdown);
  const shellClass = classList(
    "aaronnote-shell",
    "published-note-page",
    pdf && "published-note-pdf",
    hidden && "hidden-note-page",
    kind !== "default" && `note-kind-${kind}`,
  );
  const kindAssetsHtml = options.kindAssetsHtml ? `${options.kindAssetsHtml}\n` : "";
  const noteCssHref = noteCssHrefFromMarkdown(markdown);
  const noteCssHtml = noteCssHref
    ? `  <link rel="stylesheet" data-aaronnote-note-css href="${escapeAttr(noteCssHref)}" />\n`
    : "";
  const toolbarHtml = pdf ? "" : `    <header class="aaronnote-toolbar">
      <div class="aaronnote-title">
        <strong>Aaronnote</strong>
        <span data-file-label>${escapeHtml(group)} / ${escapeHtml(date)}</span>
      </div>
      <nav class="aaronnote-actions" aria-label="Published note navigation">
        <a href="${assetRoot}index.html">Home</a>
        <a href="${assetRoot}notes.html">Archive</a>
      </nav>
      <span class="aaronnote-vim-mode">READ</span>
      <span class="aaronnote-status">Published HTML</span>
    </header>
`;
  const tocHtml = pdf ? "" : `      <aside class="aaronnote-floating-toc is-collapsed" data-floating-toc data-published-toc>
        <button type="button" data-toc-toggle aria-expanded="false" title="Toggle page outline">Page</button>
        <nav data-toc-list aria-label="Page outline"></nav>
      </aside>
`;
  const localGraphHtml = pdf ? "" : `      <div class="aaronnote-local-graph-trigger-wrap" data-published-local-graph>
        <button type="button" class="macwin-graph-trigger" data-local-graph-open aria-label="Open local graph">
          ⬡ Graph
        </button>
      </div>
`;
  const scriptHtml = pdf ? "" : `  <script src="${assetRoot}js/note-page.js?v=${escapeAttr(version)}"></script>
  <script src="${assetRoot}Aaronnote/aaronnote/published-toc.js?v=${escapeAttr(version)}"></script>
  <script src="${assetRoot}js/mac-window.js?v=${escapeAttr(version)}"></script>
  <script type="module" src="${assetRoot}Aaronnote/aaronnote/published-local-graph.js?v=${escapeAttr(version)}"></script>
`;
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>${escapeHtml(title)} | Aaron He</title>
  <link rel="stylesheet" href="${assetRoot}Aaronnote/aaronnote/style.css?v=${escapeAttr(version)}" />
  <link rel="stylesheet" href="${assetRoot}Aaronnote/src/styles/widgets.css?v=${escapeAttr(version)}" />
  <link rel="stylesheet" href="${assetRoot}Aaronnote/src/styles/theme-typora.css?v=${escapeAttr(version)}" />
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.47/dist/katex.min.css" crossorigin="anonymous" />
  <link rel="stylesheet" href="${assetRoot}css/aaronnote-published.css?v=${escapeAttr(version)}" />
  <link rel="stylesheet" href="${assetRoot}css/mac-window.css?v=${escapeAttr(version)}" />
${kindAssetsHtml}${noteCssHtml}</head>
<body class="${pdf ? "aaronnote-published-document aaronnote-pdf-document" : "aaronnote-published-document"}" data-note-kind="${escapeAttr(kind)}">
  <main class="${escapeAttr(shellClass)}" data-note-kind="${escapeAttr(kind)}">
${toolbarHtml}
    <section class="aaronnote-body">
      <section class="aaronnote-editor" id="editor">
        <div class="typora-web-wrap">
          <div class="typora-web-editor-host">
            <article id="content" class="cm-editor" data-note-title="${escapeAttr(title)}" data-note-kind="${escapeAttr(kind)}">
              ${contentHtml}
            </article>
          </div>
        </div>
      </section>
${tocHtml}
${localGraphHtml}
    </section>
  </main>
${scriptHtml}
</body>
</html>
`;
}
