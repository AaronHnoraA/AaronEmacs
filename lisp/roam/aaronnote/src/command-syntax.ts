import { findSingleLineClose, parseAttrArgs, readTrailingAttrs } from "./attrs-syntax.ts";

export type InlineCommand = {
  name: string;
  switchValue: string;
  context: string;
  argsRaw: string;
  args: Record<string, string>;
  fullFrom: number;
  fullTo: number;
  contextFrom: number;
  contextTo: number;
};

export type BlockCommand = {
  name: string;
  title: string;
  content: string;
};

export function parseCommandArgs(raw = ""): Record<string, string> {
  return parseAttrArgs(raw);
}

function findClose(text: string, open: number, closeChar: "]" | "}"): number {
  return findSingleLineClose(text, open, closeChar);
}

function metaRange(text: string, closeBracket: number): { raw: string; fullTo: number } {
  const trailing = readTrailingAttrs(text, closeBracket + 1, { allowWhitespace: true });
  return trailing ? { raw: trailing.raw, fullTo: trailing.to } : { raw: "", fullTo: closeBracket + 1 };
}

function trailingMetaBeforeLineEnd(text: string, bodyFrom: number, lineEnd: number): {
  raw: string;
  bodyTo: number;
  fullTo: number;
} {
  const line = text.slice(bodyFrom, lineEnd);
  const match = line.match(/[ \t]+(\{[^{}\n]*\})[ \t]*$/);
  if (!match || match.index === undefined) return { raw: "", bodyTo: lineEnd, fullTo: lineEnd };
  const raw = match[1]!;
  return {
    raw,
    bodyTo: bodyFrom + match.index,
    fullTo: bodyFrom + match.index + match[0].length,
  };
}

export function scanInlineCommands(text: string, name?: string): InlineCommand[] {
  const commands: InlineCommand[] = [];
  const wanted = name?.toLowerCase();
  const pushCommand = (
    commandName: string,
    switchValue: string,
    contextFrom: number,
    contextTo: number,
    fullFrom: number,
    fullTo: number,
    argsRaw = "",
  ) => {
    if (wanted && commandName !== wanted) return;
    commands.push({
      name: commandName,
      switchValue,
      context: text.slice(contextFrom, contextTo),
      argsRaw,
      args: parseCommandArgs(argsRaw),
      fullFrom,
      fullTo,
      contextFrom,
      contextTo,
    });
  };

  const tagRe = /@@tag\[/gi;
  let tagMatch: RegExpExecArray | null;
  while ((tagMatch = tagRe.exec(text))) {
    const openBracket = tagRe.lastIndex - 1;
    const closeBracket = findClose(text, openBracket, "]");
    if (closeBracket < 0) continue;
    pushCommand("tag", "", openBracket + 1, closeBracket, tagMatch.index, closeBracket + 1);
    tagRe.lastIndex = closeBracket + 1;
  }

  const re = /@@([A-Za-z][\w-]*)(?:\(([^)\n]*)\))?[ \t]+\[/g;
  let match: RegExpExecArray | null;
  while ((match = re.exec(text))) {
    const commandName = match[1].toLowerCase();
    const openBracket = re.lastIndex - 1;
    const closeBracket = findClose(text, openBracket, "]");
    if (closeBracket < 0) continue;
    const meta = metaRange(text, closeBracket);
    pushCommand(commandName, match[2]?.trim() ?? "", openBracket + 1, closeBracket, match.index, meta.fullTo, meta.raw);
    re.lastIndex = meta.fullTo;
  }

  const bareTodoRe = /@@todo(?:\(([^)\n]*)\))?[ \t]+(?!\[)([^\n]+)/gi;
  let bareTodoMatch: RegExpExecArray | null;
  while ((bareTodoMatch = bareTodoRe.exec(text))) {
    const bodyFrom = bareTodoMatch.index + bareTodoMatch[0].length - bareTodoMatch[2]!.length;
    const lineEnd = bareTodoMatch.index + bareTodoMatch[0].length;
    const meta = trailingMetaBeforeLineEnd(text, bodyFrom, lineEnd);
    if (text.slice(bodyFrom, meta.bodyTo).trim()) {
      pushCommand("todo", bareTodoMatch[1]?.trim() ?? "", bodyFrom, meta.bodyTo, bareTodoMatch.index, meta.fullTo, meta.raw);
    }
    bareTodoRe.lastIndex = lineEnd;
  }
  return commands.sort((a, b) => a.fullFrom - b.fullFrom || a.fullTo - b.fullTo);
}

export function parseBlockCommandOpenLine(line: string): { name: string; title: string } | null {
  const match = line.match(/^\s*#\+begin(?:_|\s+)([A-Za-z][\w-]*)(?:\s+([^\n]+?))?\s*$/i);
  if (!match) return null;
  return {
    name: match[1].toLowerCase(),
    title: match[2]?.trim() ?? "",
  };
}

export function isBlockCommandCloseLine(line: string, name: string): boolean {
  const escaped = line.replace(/^(\s*)\\(?=#\+end)/i, "$1");
  const pattern = new RegExp(`^\\s*#\\+end(?:_|\\s+)${name}\\s*$`, "i");
  return pattern.test(escaped);
}

export function parseBlockCommandText(text: string): BlockCommand | null {
  const open = text.match(/^\s*#\+begin(?:_|\s+)([A-Za-z][\w-]*)(?:\s+([^\n]+?))?\s*\n/i);
  if (!open) return null;
  const name = open[1].toLowerCase();
  const lines = text.slice(open[0].length).replace(/\n$/, "").split(/\n/);
  const closeLine = lines.at(-1) ?? "";
  if (!isBlockCommandCloseLine(closeLine, name)) return null;
  return {
    name,
    title: open[2]?.trim() ?? "",
    content: lines.slice(0, -1).join("\n").replace(/\n$/, ""),
  };
}
