import { htmlToMarkdown } from "./paste-html.ts";

export function normalizePastedSourceText(text: string): string {
  return text
    .replace(/\r\n?/g, "\n")
    .replace(/\u0008/g, String.raw`\b`)
    .replace(/\u000c/g, String.raw`\f`)
    .replace(/\u000b/g, String.raw`\v`);
}

export function plainTextLooksLikeMarkdownSource(text: string): boolean {
  const md = text.trim();
  if (!md) return false;
  return [
    /^\s{0,3}#{1,6}\s+\S/m,
    /^\s{0,3}\$\$\s*$/m,
    /^\s{0,3}```/m,
    /^\s{0,3}#\+begin\b/im,
    /^\s{0,3}>\s+\S/m,
    /^\s{0,3}(?:[-*+]\s+|\d+[.)]\s+)/m,
    /\[[^\]\n]+\]\([^)]+\)/,
    /\$[^$\n]+\$/,
  ].some((re) => re.test(md));
}

export function markdownFromClipboard(data: DataTransfer): string {
  const plain = normalizePastedSourceText(data.getData("text/plain"));
  const html = data.getData("text/html");
  if (plainTextLooksLikeMarkdownSource(plain)) return plain;
  if (html && /<[A-Za-z][\s\S]*>/.test(html)) {
    const md = htmlToMarkdown(html);
    if (md) return normalizePastedSourceText(md);
  }
  return plain;
}
