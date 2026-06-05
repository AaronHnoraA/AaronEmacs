import type { Editor } from "../src/lib.ts";
import {
  inlineTagAnchorsFromText,
  markdownHeadingsFromText,
  tocIndexFromState,
  type InlineTagAnchor,
} from "../src/cm6/toc-index.ts";
import type { NoteSummary } from "./types.ts";

type OpenNoteOptions = { newWindow?: boolean; equationTag?: string; inlineTag?: string };

export { inlineTagAnchorsFromText, markdownHeadingsFromText };
export type { InlineTagAnchor };

export type FloatingTocPanel = {
  update: () => void;
  toggle: () => void;
};

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
  let headingDoc: unknown = null;
  let headingCache: {
    items: Array<{ level: number; text: string; pos: number }>;
    signature: string;
  } = { items: [], signature: "" };
  let anchorDoc: unknown = null;
  let anchorCache: {
    items: InlineTagAnchor[];
    signature: string;
  } = { items: [], signature: "" };

  function editorHeadings(): {
    items: Array<{ level: number; text: string; pos: number }>;
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
    const headings = headingState.items;
    const anchors = anchorState.items;
    const selectionPos = options.editor.view.state.selection.main.from;
    const activeIndex = headings.reduce((active, heading, index) => heading.pos <= selectionPos ? index : active, -1);
    const currentNote = notes.find((note) => note.file === options.getCurrentFile());
    const relatedIds = [...(currentNote?.refs ?? []), ...(currentNote?.backlinks ?? [])];
    const tags = currentNote?.tags ?? [];
    const key = `${activeIndex}\n${currentNote?.id ?? ""}\n${relatedIds.join(",")}\n${tags.join(",")}\n${headingState.signature}\n${anchorState.signature}`;
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
    headings.forEach((heading, index) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = index === activeIndex ? "aaronnote-toc-item is-active" : "aaronnote-toc-item";
      button.style.setProperty("--toc-depth", String(Math.max(0, heading.level - 1)));
      button.dataset.level = String(heading.level);
      button.title = heading.text;
      if (index === activeIndex) button.setAttribute("aria-current", "location");
      button.textContent = heading.text;
      button.addEventListener("click", () => {
        options.editor.setSelection(heading.pos);
        options.editor.focus();
      });
      frag.appendChild(button);
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
