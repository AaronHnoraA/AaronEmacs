export function citeNamespaceCompletionPrefix(before: string): string | null {
  const match = before.match(/@@cite\(([^)\n]*)$/);
  return match ? match[1] ?? "" : null;
}

export function citeNamespaceRenderPrefix(prefix: string): string {
  return `@@cite(${prefix}`;
}

export type CiteKeyCompletionContext = { namespace: string; prefix: string; separator?: string };

export function citeKeyCompletionContext(before: string): CiteKeyCompletionContext | null {
  const match = before.match(/@@cite\(([^)\n]+)\)(\s*)\[([^\]\n]*)$/);
  if (!match) return null;
  const keys = match[3] ?? "";
  return {
    namespace: (match[1] ?? "").trim(),
    separator: match[2] ?? "",
    prefix: keys.split(";").at(-1)?.trimStart() ?? "",
  };
}

export function citeKeyRenderPrefix(context: CiteKeyCompletionContext): string {
  return `@@cite(${context.namespace})${context.separator ?? " "}[${context.prefix}`;
}
