export function isMarkdownMimeType(mimeType: string): boolean {
  const normalized = mimeType.toLowerCase();
  return normalized.includes("markdown") || normalized.includes("ipythongfm");
}
