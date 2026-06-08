import DOMPurify from "dompurify";

// Shared policy for user-authored HTML rendered in the live editor.
// Mirrors the uri allowlist used by paste-html.ts; forbids active/embedding tags.
export function sanitizeEmbeddedHtml(source: string): string {
  return String(DOMPurify.sanitize(source, {
    ALLOWED_URI_REGEXP: /^(?:(?:https?|mailto|tel|file|zotero|roam):|[#/]|\.{0,2}\/|[^a-z])/i,
    FORBID_TAGS: ["script", "style", "iframe", "object", "embed"],
  }));
}
