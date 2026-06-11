import type { Editor } from "../src/lib.ts";
import {
  inlineTagAnchorsFromText,
  markdownHeadingsFromText,
  tocIndexFromState,
  type InlineTagAnchor,
  type MarkdownHeading,
} from "../src/cm6/toc-index.ts";
import type { NoteSummary } from "./types.ts";

type OpenNoteOptions = { newWindow?: boolean; equationTag?: string; inlineTag?: string };

export { inlineTagAnchorsFromText, markdownHeadingsFromText };
export type { InlineTagAnchor };

export type FloatingTocPanel = {
  update: () => void;
  toggle: () => void;
};

function floatingTocFoldKeys(headings: readonly MarkdownHeading[]): string[] {
  const counts = new Map<string, number>();
  const stack: Array<{ level: number; ordinal: number }> = [];
  return headings.map((heading) => {
    while (stack.length > 0 && heading.level <= stack[stack.length - 1]!.level) {
      stack.pop();
    }
    const parentPath = stack.map((part) => part.ordinal).join(".");
    const siblingGroup = `${parentPath}|${heading.level}`;
    const ordinal = (counts.get(siblingGroup) ?? 0) + 1;
    counts.set(siblingGroup, ordinal);
    const path = parentPath ? `${parentPath}.${ordinal}` : String(ordinal);
    stack.push({ level: heading.level, ordinal });
    return `${path}:${heading.level}:${heading.text}`;
  });
}

function floatingTocSignature(headings: readonly MarkdownHeading[]): string {
  const keys = floatingTocFoldKeys(headings);
  return headings
    .map((heading, index) => `${keys[index]}\t${heading.level}\t${heading.text}\t${heading.source || "markdown"}\t${heading.kind || ""}\t${heading.omit ? 1 : 0}`)
    .join("\n");
}

function floatingTocVisible(
  headings: readonly MarkdownHeading[],
  foldState: ReadonlySet<string>,
  visit: (heading: MarkdownHeading, index: number, key: string) => void,
): void {
  const foldedDepths: number[] = [];
  const keys = floatingTocFoldKeys(headings);
  for (let index = 0; index < headings.length; index += 1) {
    const heading = headings[index]!;
    while (foldedDepths.length > 0 && heading.level <= foldedDepths[foldedDepths.length - 1]!) {
      foldedDepths.pop();
    }
    const visible = foldedDepths.length === 0;
    const key = keys[index]!;
    if (visible) visit(heading, index, key);
    if (visible && foldState.has(key)) foldedDepths.push(heading.level);
  }
}

export function createFloatingTocPanel(options: {
  toc: HTMLElement;
  toggleButton: HTMLButtonElement;
  list: HTMLElement;
  editor: Editor;
  getNotes: () => NoteSummary[];
  getCurrentFile: () => string;
  resolveNoteRef: (ref: string) => NoteSummary | undefined;
  openNote: (note: NoteSummary, options?: OpenNoteOptions) => void;
  openTag?: (tag: string) => void;
}): FloatingTocPanel {
  let renderKey = "";
  const floatingFoldState = new Set<string>();
  let headingDoc: unknown = null;
  let headingCache: {
    items: MarkdownHeading[];
    signature: string;
  } = { items: [], signature: "" };
  let anchorDoc: unknown = null;
  let anchorCache: {
    items: InlineTagAnchor[];
    signature: string;
  } = { items: [], signature: "" };

  function editorHeadings(): {
    items: MarkdownHeading[];
    signature: string;
  } {
    const state = options.editor.view.state;
    if (state.doc === headingDoc) return headingCache;
    const index = tocIndexFromState(state);
    headingDoc = state.doc;
    headingCache = {
      items: index.headings,
      signature: index.headingSignature,
    };
    return headingCache;
  }

  function editorInlineAnchors(): {
    items: InlineTagAnchor[];
    signature: string;
  } {
    const state = options.editor.view.state;
    if (state.doc === anchorDoc) return anchorCache;
    const index = tocIndexFromState(state);
    anchorDoc = state.doc;
    anchorCache = {
      items: index.anchors,
      signature: index.anchorSignature,
    };
    return anchorCache;
  }

  function renderRelatedNotes(parent: DocumentFragment | HTMLElement, currentNote: NoteSummary | undefined): void {
    if (!currentNote) return;
    const notes = options.getNotes();
    const byId = new Map(notes.map((note) => [note.id, note]));
    const sections: Array<[string, string[]]> = [
      ["Links", currentNote.refs ?? []],
      ["Backlinks", currentNote.backlinks ?? []],
    ];
    for (const [label, ids] of sections) {
      const resolved = ids
        .map((id) => byId.get(id) || options.resolveNoteRef(id))
        .filter((note): note is NoteSummary => Boolean(note?.file));
      if (resolved.length === 0) continue;
      const head = document.createElement("div");
      head.className = "aaronnote-toc-section";
      head.textContent = label;
      parent.appendChild(head);
      for (const note of resolved) {
        const button = document.createElement("button");
        button.type = "button";
        button.className = "aaronnote-toc-item aaronnote-toc-related";
        button.style.setProperty("--toc-depth", "0");
        button.textContent = note.title || note.id || note.file || "Untitled";
        button.title = note.file || note.title || "";
        button.addEventListener("click", (event) => options.openNote(note, { newWindow: event.altKey || event.metaKey }));
        button.addEventListener("auxclick", (event) => {
          if (event.button !== 1) return;
          event.preventDefault();
          options.openNote(note, { newWindow: true });
        });
        parent.appendChild(button);
      }
    }
  }

  function renderCurrentTags(parent: DocumentFragment | HTMLElement, currentNote: NoteSummary | undefined): void {
    const tags = [...new Set((currentNote?.tags ?? []).map((tag) => String(tag).trim()).filter(Boolean))]
      .sort((a, b) => a.localeCompare(b));
    if (tags.length === 0) return;
    const head = document.createElement("div");
    head.className = "aaronnote-toc-section";
    head.textContent = "Tags";
    parent.appendChild(head);
    for (const tag of tags) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "aaronnote-toc-item aaronnote-toc-tag";
      button.style.setProperty("--toc-depth", "0");
      button.textContent = `#${tag.replace(/^#/, "")}`;
      button.title = `tag:${tag.replace(/^#/, "")}`;
      button.addEventListener("click", () => options.openTag?.(tag.replace(/^#/, "")));
      parent.appendChild(button);
    }
  }

  function renderInlineAnchors(parent: DocumentFragment | HTMLElement, anchors: InlineTagAnchor[]): void {
    if (anchors.length === 0) return;
    const head = document.createElement("div");
    head.className = "aaronnote-toc-section";
    head.textContent = "Inline anchors";
    parent.appendChild(head);
    for (const anchor of anchors) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "aaronnote-toc-item aaronnote-toc-anchor";
      button.style.setProperty("--toc-depth", "0");
      button.textContent = `#${anchor.tag}`;
      button.title = `@@tag[${anchor.tag}]`;
      button.addEventListener("click", () => {
        options.editor.setSelection(anchor.pos, anchor.to);
        options.editor.focus();
      });
      parent.appendChild(button);
    }
  }

  function update(): void {
    const notes = options.getNotes();
    const headingState = editorHeadings();
    const anchorState = editorInlineAnchors();
    const headings = headingState.items.filter((h) => !h.omit);
    const anchors = anchorState.items;
    const selectionPos = options.editor.view.state.selection.main.from;
    const activeIndex = headings.reduce((active, heading, index) => heading.pos <= selectionPos ? index : active, -1);
    const currentNote = notes.find((note) => note.file === options.getCurrentFile());
    const relatedIds = [...(currentNote?.refs ?? []), ...(currentNote?.backlinks ?? [])];
    const tags = currentNote?.tags ?? [];
    const foldRevision = [...floatingFoldState].sort().join(",");
    const headingRenderSignature = floatingTocSignature(headings);
    const key = `${activeIndex}\n${currentNote?.id ?? ""}\n${relatedIds.join(",")}\n${tags.join(",")}\n${headingRenderSignature}\n${anchorState.signature}\n${foldRevision}`;
    if (key === renderKey) return;
    renderKey = key;
    const frag = document.createDocumentFragment();
    const relatedCount = relatedIds.length;
    const tagCount = tags.length;
    const anchorCount = anchors.length;
    options.toggleButton.textContent = headings.length > 0 ? `Page ${headings.length}` : "Page";
    if (headings.length === 0 && relatedIds.length === 0 && tagCount === 0 && anchorCount === 0) {
      const empty = document.createElement("div");
      empty.className = "aaronnote-toc-empty";
      empty.textContent = "No roam context";
      frag.appendChild(empty);
      options.list.replaceChildren(frag);
      return;
    }
    const status = document.createElement("div");
    status.className = "aaronnote-toc-status";
    status.textContent = [
      `${headings.length} headings`,
      anchorCount > 0 ? `${anchorCount} anchors` : "",
      tagCount > 0 ? `${tagCount} tags` : "",
      relatedCount > 0 ? `${relatedCount} links` : "",
    ].filter(Boolean).join(" · ");
    frag.appendChild(status);

    // Determine which headings have children (for chevron)
    const headingHasChildren = headings.map((_, i) =>
      i < headings.length - 1 && headings[i + 1]!.level > headings[i]!.level,
    );

    floatingTocVisible(headings, floatingFoldState, (heading, index, foldKey) => {
      const row = document.createElement("div");
      row.className = "aaronnote-toc-row";
      row.style.setProperty("--toc-depth", String(Math.max(0, heading.level - 1)));

      if (headingHasChildren[index]) {
        const isFolded = floatingFoldState.has(foldKey);
        const chevron = document.createElement("button");
        chevron.type = "button";
        chevron.className = "aaronnote-toc-chevron";
        chevron.textContent = isFolded ? "▶" : "▼";
        chevron.title = isFolded ? "Expand" : "Collapse";
        chevron.addEventListener("click", (event) => {
          event.stopPropagation();
          if (floatingFoldState.has(foldKey)) floatingFoldState.delete(foldKey);
          else floatingFoldState.add(foldKey);
          renderKey = "";
          update();
        });
        row.appendChild(chevron);
      }

      const button = document.createElement("button");
      button.type = "button";
      button.className = index === activeIndex ? "aaronnote-toc-item is-active" : "aaronnote-toc-item";
      button.dataset.level = String(heading.level);
      button.title = heading.text;
      if (index === activeIndex) button.setAttribute("aria-current", "location");
      button.textContent = heading.text;
      button.addEventListener("click", () => {
        const currentHeadings = editorHeadings().items.filter((item) => !item.omit);
        const currentKeys = floatingTocFoldKeys(currentHeadings);
        const currentHeading = currentHeadings[currentKeys.indexOf(foldKey)] ?? heading;
        options.editor.setSelection(currentHeading.pos);
        options.editor.focus();
      });
      row.appendChild(button);
      frag.appendChild(row);
    });
    renderInlineAnchors(frag, anchors);
    renderCurrentTags(frag, currentNote);
    renderRelatedNotes(frag, currentNote);
    options.list.replaceChildren(frag);
  }

  function toggle(): void {
    options.toc.classList.toggle("is-collapsed");
    options.toggleButton.setAttribute("aria-expanded", options.toc.classList.contains("is-collapsed") ? "false" : "true");
  }

  return { update, toggle };
}
