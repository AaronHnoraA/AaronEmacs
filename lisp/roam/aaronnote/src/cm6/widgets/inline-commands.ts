/**
 * Inline @@command widgets — currently handles @@todo(status) [text]{args}
 *
 * Uses a ViewPlugin (viewport-scoped) since these are inline decorations
 * (no block:true needed).  When the cursor is inside a command span the
 * raw source is shown; otherwise it is replaced with a rendered chip.
 */

import {
  Decoration,
  EditorView,
  ViewPlugin,
  type DecorationSet,
  type ViewUpdate,
} from "@codemirror/view";
import { MeasuredWidget } from "./measured-widget.ts";
import { scanInlineCommands, type InlineCommand } from "../../command-syntax.ts";
import type { Range } from "@codemirror/state";
import { blockMathRangesOverlapping, mergeOverlappingRanges, rangeOverlapsAny } from "../math-ranges.ts";
import { scanCodeRanges } from "../code-ranges.ts";
import { scanInlineMathRanges } from "../../inline-math.ts";
import {
  DATE_KEYS,
  DATE_KEY_LABELS,
  formatDateValue,
  parseDateValue,
  relativeDateClass,
  relativeDateLabel,
} from "../../date-syntax.ts";
import { hasViewportDecorationRefresh } from "../viewport-refresh.ts";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const STATUS_LABELS: Record<string, string> = {
  todo: "TODO",
  doing: "DOING",
  done: "DONE",
  blocked: "BLOCKED",
  "": "TODO",
};

const STATUS_ICONS: Record<string, string> = {
  todo: "□",
  doing: "▶",
  done: "✓",
  blocked: "✕",
};

function statusLabel(sw: string): string {
  return STATUS_LABELS[sw.toLowerCase()] ?? sw.toUpperCase();
}

function statusIcon(sw: string): string {
  return STATUS_ICONS[sw.toLowerCase()] ?? "•";
}

function cleanTag(value: string): string {
  return value.trim().replace(/^#/, "");
}

// ---------------------------------------------------------------------------
// Widget
// ---------------------------------------------------------------------------

class TodoWidget extends MeasuredWidget {
  cmd: InlineCommand;

  constructor(cmd: InlineCommand) {
    super();
    this.cmd = cmd;
  }

  protected measureKey(): string { return ""; }
  protected get measuredBlock(): boolean { return false; }

  eq(other: TodoWidget): boolean {
    return (
      this.cmd.switchValue === other.cmd.switchValue &&
      this.cmd.context === other.cmd.context &&
      this.cmd.argsRaw === other.cmd.argsRaw &&
      this.cmd.fullFrom === other.cmd.fullFrom &&
      this.cmd.fullTo === other.cmd.fullTo
    );
  }

  toDOM(): HTMLElement {
    const { cmd } = this;
    const status = cmd.switchValue.toLowerCase() || "todo";

    const wrap = document.createElement("span");
    wrap.className = "inline-todo-widget inline-command-token";
    wrap.dataset.status = status;
    wrap.dataset.cmSourceFrom = String(cmd.fullFrom);
    wrap.dataset.cmSourceTo = String(cmd.fullTo);
    wrap.dataset.cmOpenSource = "true";

    const card = document.createElement("span");
    card.className = "inline-todo-card";

    const chip = document.createElement("span");
    chip.className = "inline-todo-chip";
    chip.dataset.status = status;

    const icon = document.createElement("span");
    icon.className = "inline-todo-chip-icon";
    icon.setAttribute("aria-hidden", "true");
    icon.textContent = statusIcon(status);
    chip.append(icon);

    const label = document.createElement("span");
    label.className = "inline-todo-chip-label";
    label.textContent = statusLabel(status);
    chip.append(label);

    card.append(chip);

    if (cmd.context.trim()) {
      const text = document.createElement("span");
      text.className = "inline-todo-text";
      const lBracket = document.createElement("span");
      lBracket.className = "inline-todo-bracket";
      lBracket.setAttribute("aria-hidden", "true");
      lBracket.textContent = "[";
      const body = document.createElement("span");
      body.className = "inline-todo-text-body";
      body.textContent = cmd.context.trim();
      const rBracket = document.createElement("span");
      rBracket.className = "inline-todo-bracket";
      rBracket.setAttribute("aria-hidden", "true");
      rBracket.textContent = "]";
      text.append(lBracket, body, rBracket);
      card.append(text);
    }

    const metaEntries = Object.entries(cmd.args)
      .filter((entry): entry is [string, string] => Boolean(entry[0] && entry[1]));
    if (metaEntries.length > 0) {
      const meta = document.createElement("span");
      meta.className = "inline-todo-meta";
      for (const [key, value] of metaEntries) {
        const lowKey = key.toLowerCase();
        const isDateKey = DATE_KEYS.has(lowKey);
        const parsed = isDateKey ? parseDateValue(value) : null;
        if (isDateKey && parsed) {
          const canonical = formatDateValue(parsed.time, parsed.hasTime);
          const pill = document.createElement("span");
          pill.className = "inline-todo-date";
          pill.dataset.when = relativeDateClass(parsed.time);
          pill.dataset.key = lowKey;
          const k = document.createElement("span");
          k.className = "inline-todo-date-key";
          k.textContent = DATE_KEY_LABELS[lowKey] ?? lowKey;
          const v = document.createElement("span");
          v.className = "inline-todo-date-value";
          v.textContent = canonical;
          v.title = canonical === value.trim() ? canonical : `${value.trim()} → ${canonical}`;
          const rel = document.createElement("span");
          rel.className = "inline-todo-date-rel";
          rel.textContent = relativeDateLabel(parsed.time);
          pill.append(k, v, rel);
          meta.append(pill);
        } else {
          const pill = document.createElement("span");
          pill.className = "inline-todo-arg";
          const k = document.createElement("span");
          k.className = "inline-todo-arg-key";
          k.textContent = key;
          const v = document.createElement("span");
          v.className = "inline-todo-arg-value";
          v.textContent = value;
          pill.append(k, v);
          meta.append(pill);
        }
      }
      card.append(meta);
    }

    wrap.append(card);

    const rail = document.createElement("span");
    rail.className = "inline-todo-rail";
    rail.setAttribute("aria-hidden", "true");
    wrap.append(rail);

    return wrap;
  }

  ignoreEvent(): boolean { return false; }
}

// ---------------------------------------------------------------------------
// ViewPlugin
// ---------------------------------------------------------------------------

function excludedCommandRanges(view: EditorView): Array<{ from: number; to: number }> {
  const math = blockMathRangesOverlapping(view.state, view.visibleRanges).map(({ from, to }) => ({ from, to }));
  const inlineMath = view.visibleRanges.flatMap(({ from, to }) =>
    scanInlineMathRanges(view.state.doc.sliceString(from, to), from));
  const code = scanCodeRanges(view.state, view.visibleRanges);
  return mergeOverlappingRanges([...math, ...inlineMath, ...code]);
}

function buildInlineCommandDecos(
  view: EditorView,
  excludedRanges: Array<{ from: number; to: number }>,
): DecorationSet {
  const decos: Range<Decoration>[] = [];
  const sel = view.state.selection.main;
  const doc = view.state.doc;
  const lineTags = new Map<number, string[]>();

  for (const { from: vFrom, to: vTo } of view.visibleRanges) {
    const text = doc.sliceString(vFrom, vTo);
    for (const cmd of scanInlineCommands(text)) {
      const from = vFrom + cmd.fullFrom;
      const to = vFrom + cmd.fullTo;
      if (rangeOverlapsAny(from, to, excludedRanges)) continue;
      const cursorInside = sel.from <= to && sel.to >= from;
      if (cmd.name === "todo" && !cursorInside) {
        decos.push(
          Decoration.replace({
            widget: new TodoWidget({ ...cmd, fullFrom: from, fullTo: to }),
          }).range(from, to),
        );
      }
      if (cmd.name === "tag") {
        const tag = cleanTag(cmd.context);
        if (!tag) continue;
        const line = doc.lineAt(from);
        const current = lineTags.get(line.from) ?? [];
        if (!current.includes(tag)) current.push(tag);
        lineTags.set(line.from, current);
        if (!cursorInside) {
          decos.push(Decoration.replace({}).range(from, to));
        }
      }
    }
  }

  for (const [lineFrom, tags] of lineTags) {
    decos.push(Decoration.line({
      attributes: {
        class: "cm-line-has-aaronnote-tags",
        "data-aaronnote-tags": tags.map((tag) => `#${tag}`).join("\n"),
      },
    }).range(lineFrom));
  }

  decos.sort((a, b) => a.from - b.from || a.to - b.to);
  return Decoration.set(decos, true);
}

function activeInlineCommandKey(view: EditorView): string {
  const sel = view.state.selection.main;
  const firstLine = view.state.doc.lineAt(sel.from).number;
  const lastLine = view.state.doc.lineAt(Math.min(sel.to, view.state.doc.length)).number;
  if (lastLine - firstLine > 50) return `wide:${sel.from}:${sel.to}`;
  const keys: string[] = [];

  for (let lineNum = firstLine; lineNum <= lastLine; lineNum++) {
    const line = view.state.doc.line(lineNum);
    const inlineMathRanges = scanInlineMathRanges(line.text, line.from);
    for (const cmd of scanInlineCommands(line.text)) {
      const from = line.from + cmd.fullFrom;
      const to = line.from + cmd.fullTo;
      if (rangeOverlapsAny(from, to, inlineMathRanges)) continue;
      if (sel.from <= to && sel.to >= from) keys.push(`${from}:${to}`);
    }
  }
  return keys.join("|");
}

class TodoPlugin {
  decorations: DecorationSet;
  excludedRanges: Array<{ from: number; to: number }>;
  private activeCommandKey: string;

  constructor(view: EditorView) {
    this.excludedRanges = excludedCommandRanges(view);
    this.activeCommandKey = activeInlineCommandKey(view);
    this.decorations = buildInlineCommandDecos(view, this.excludedRanges);
  }

  update(update: ViewUpdate): void {
    if (update.view.compositionStarted && update.selectionSet && !update.docChanged && !update.viewportChanged) return;
    if (update.docChanged || update.viewportChanged || hasViewportDecorationRefresh(update)) {
      this.excludedRanges = excludedCommandRanges(update.view);
      this.activeCommandKey = activeInlineCommandKey(update.view);
      this.decorations = buildInlineCommandDecos(update.view, this.excludedRanges);
    } else if (update.selectionSet) {
      const nextCommandKey = activeInlineCommandKey(update.view);
      if (nextCommandKey === this.activeCommandKey) return;
      this.activeCommandKey = nextCommandKey;
      this.decorations = buildInlineCommandDecos(update.view, this.excludedRanges);
    }
  }
}

export const inlineCommandsExtension = ViewPlugin.fromClass(TodoPlugin, {
  decorations: (v) => v.decorations,
});
