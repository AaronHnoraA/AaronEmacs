export function citeNamespaceCompletionPrefix(before: string): string | null {
  const match = before.match(/@@cite\(([^)\n]*)$/);
  return match ? match[1] ?? "" : null;
}

export function citeNamespaceRenderPrefix(prefix: string): string {
  return `@@cite(${prefix}`;
}

export function citeKeyCompletionContext(before: string): { namespace: string; prefix: string } | null {
  const match = before.match(/@@cite\(([^)\n]+)\)\s+\[([^\]\n]*)$/);
  if (!match) return null;
  const keys = match[2] ?? "";
  return {
    namespace: (match[1] ?? "").trim(),
    prefix: keys.split(";").at(-1)?.trimStart() ?? "",
  };
}

export function citeKeyRenderPrefix(context: { namespace: string; prefix: string }): string {
  return `@@cite(${context.namespace}) [${context.prefix}`;
}
