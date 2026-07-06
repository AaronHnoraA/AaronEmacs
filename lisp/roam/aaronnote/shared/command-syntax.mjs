function cleanAttrValue(value) {
  return String(value || "").trim().replace(/^["']|["']$/g, "");
}

function splitArgChunks(body) {
  const chunks = [];
  let current = "";
  let quote = null;
  for (let i = 0; i < body.length; i++) {
    const ch = body[i];
    if (quote) {
      current += ch;
      if (ch === quote) quote = null;
      continue;
    }
    if (ch === '"' || ch === "'") {
      quote = ch;
      current += ch;
      continue;
    }
    if (ch === ";" || ch === ",") {
      chunks.push(current);
      current = "";
      continue;
    }
    current += ch;
  }
  chunks.push(current);
  return chunks;
}

export function parseCommandArgs(raw = "") {
  const body = String(raw || "").trim().replace(/^\{/, "").replace(/\}$/, "").trim();
  if (!body) return {};
  const out = {};
  for (const chunk of splitArgChunks(body)) {
    const item = chunk.trim();
    if (!item) continue;
    const attrPattern = /([A-Za-z][\w-]*)(?:\s*[:=]\s*("[^"]*"|'[^']*'|.*?))?(?=\s+[A-Za-z][\w-]*(?:\s*[:=]|\s*$)|$)/g;
    let matched = false;
    for (const match of item.matchAll(attrPattern)) {
      matched = true;
      const key = match[1].toLowerCase();
      const value = cleanAttrValue(match[2] ?? key);
      if (key && value) out[key] = value;
    }
    if (matched) continue;
    const pair = item.match(/^([A-Za-z][\w-]*)\s*[:=]\s*(.+)$/);
    if (pair) {
      const key = pair[1].toLowerCase();
      const value = cleanAttrValue(pair[2]);
      if (key && value) out[key] = value;
      continue;
    }
    const bare = item.match(/^([A-Za-z][\w-]*)$/);
    if (bare) out[bare[1].toLowerCase()] = bare[1].toLowerCase();
  }
  return out;
}

export function findInlineCommandClose(text, open, closeChar) {
  let bracketDepth = 0;
  for (let i = open + 1; i < text.length; i++) {
    const ch = text[i];
    if (closeChar === "]" && ch === "\\" && (text[i + 1] === "(" || text[i + 1] === "[")) {
      const close = text[i + 1] === "[" ? "\\]" : "\\)";
      const start = i + 2;
      const found = text.indexOf(close, start);
      if (found >= 0 && !/[\n\r]/.test(text.slice(start, found))) {
        i = found + close.length - 1;
        continue;
      }
    }
    if (ch === "\\" && i + 1 < text.length) {
      i++;
      continue;
    }
    if (ch === "\n" || ch === "\r") return -1;
    if (closeChar === "]" && ch === "[") {
      bracketDepth++;
      continue;
    }
    if (ch === closeChar) {
      if (closeChar === "]" && bracketDepth > 0) {
        bracketDepth--;
        continue;
      }
      return i;
    }
  }
  return -1;
}

function metaRange(text, closeBracket) {
  let openBrace = closeBracket + 1;
  while (openBrace < text.length && (text[openBrace] === " " || text[openBrace] === "\t")) openBrace++;
  if (text[openBrace] !== "{") return { raw: "", fullTo: closeBracket + 1 };
  const closeBrace = findInlineCommandClose(text, openBrace, "}");
  return closeBrace < 0
    ? { raw: "", fullTo: closeBracket + 1 }
    : { raw: text.slice(openBrace, closeBrace + 1), fullTo: closeBrace + 1 };
}

function trailingMetaBeforeLineEnd(text, bodyFrom, lineEnd) {
  const line = text.slice(bodyFrom, lineEnd);
  const match = line.match(/[ \t]+(\{[^{}\n]*\})[ \t]*$/);
  if (!match || match.index === undefined) return { raw: "", bodyTo: lineEnd, fullTo: lineEnd };
  return {
    raw: match[1] || "",
    bodyTo: bodyFrom + match.index,
    fullTo: bodyFrom + match.index + match[0].length,
  };
}

export function scanInlineCommands(input, name = "") {
  const text = String(input || "");
  const commands = [];
  const wanted = String(name || "").toLowerCase();
  const push = (commandName, switchValue, contextFrom, contextTo, fullFrom, fullTo, argsRaw = "") => {
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
  let tagMatch;
  while ((tagMatch = tagRe.exec(text))) {
    const open = tagRe.lastIndex - 1;
    const close = findInlineCommandClose(text, open, "]");
    if (close < 0) continue;
    push("tag", "", open + 1, close, tagMatch.index, close + 1);
    tagRe.lastIndex = close + 1;
  }

  const re = /@@([A-Za-z][\w-]*)(?:\(([^)\n]*)\))?[ \t]+\[/g;
  let match;
  while ((match = re.exec(text))) {
    const commandName = match[1].toLowerCase();
    const open = re.lastIndex - 1;
    const close = findInlineCommandClose(text, open, "]");
    if (close < 0) continue;
    const meta = metaRange(text, close);
    push(commandName, match[2]?.trim() ?? "", open + 1, close, match.index, meta.fullTo, meta.raw);
    re.lastIndex = meta.fullTo;
  }

  const bareTodoRe = /@@todo(?:\(([^)\n]*)\))?[ \t]+(?!\[)([^\n]+)/gi;
  let bare;
  while ((bare = bareTodoRe.exec(text))) {
    const bodyFrom = bare.index + bare[0].length - bare[2].length;
    const lineEnd = bare.index + bare[0].length;
    const meta = trailingMetaBeforeLineEnd(text, bodyFrom, lineEnd);
    if (text.slice(bodyFrom, meta.bodyTo).trim()) {
      push("todo", bare[1]?.trim() ?? "", bodyFrom, meta.bodyTo, bare.index, meta.fullTo, meta.raw);
    }
    bareTodoRe.lastIndex = lineEnd;
  }

  return commands.sort((a, b) => a.fullFrom - b.fullFrom || a.fullTo - b.fullTo);
}

export function parseBlockCommandOpenLine(line) {
  const match = String(line || "").match(/^\s*#\+begin(?:_|\s+)([A-Za-z][\w-]*)(?:\s+([^\n]+?))?\s*$/i);
  return match ? { name: match[1].toLowerCase(), title: match[2]?.trim() ?? "" } : null;
}

export function isBlockCommandCloseLine(line, name) {
  const escaped = String(line || "").replace(/^(\s*)\\(?=#\+end)/i, "$1");
  return new RegExp(`^\\s*#\\+end(?:_|\\s+)${name}\\s*$`, "i").test(escaped);
}

export function parseBlockCommandText(text) {
  const source = String(text || "");
  const open = source.match(/^\s*#\+begin(?:_|\s+)([A-Za-z][\w-]*)(?:\s+([^\n]+?))?\s*\n/i);
  if (!open) return null;
  const name = open[1].toLowerCase();
  const lines = source.slice(open[0].length).replace(/\n$/, "").split(/\n/);
  if (!isBlockCommandCloseLine(lines.at(-1) ?? "", name)) return null;
  return {
    name,
    title: open[2]?.trim() ?? "",
    content: lines.slice(0, -1).join("\n").replace(/\n$/, ""),
  };
}
