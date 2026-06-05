import type { NoteSummary } from "./types.ts";
import { CoalescedTimer } from "../src/coalesced-timer.ts";

type GraphApi = {
  destroy?: () => void;
  setVisibleKeys?: (keys: string[]) => void;
  setSearchQuery?: (query: string, options?: { focus?: boolean; refit?: boolean }) => void;
};

declare global {
  interface Window {
    AaronnoteResolveAssetUrl?: (src: string) => string;
  }
}

export type GraphPanel = {
  dispose: () => void;
  render: () => void;
  scheduleRender: (delay?: number) => void;
};

function noteKey(note: NoteSummary): string {
  return note.key || note.id || note.path || note.file || "";
}

function roamNotes(notes: NoteSummary[]): NoteSummary[] {
  return notes.filter((note) => note.roam);
}

function loadScriptOnce(src: string): Promise<void> {
  const existing = document.querySelector<HTMLScriptElement>(`script[src="${src}"]`);
  if (existing?.dataset.loaded === "true") return Promise.resolve();
  return new Promise((resolve, reject) => {
    const script = existing || document.createElement("script");
    script.src = src;
    script.async = false;
    script.addEventListener("load", () => {
      script.dataset.loaded = "true";
      resolve();
    }, { once: true });
    script.addEventListener("error", () => reject(new Error(`Failed to load ${src}`)), { once: true });
    if (!existing) document.head.appendChild(script);
  });
}

function renderError(host: HTMLElement, message: string): void {
  const empty = document.createElement("div");
  empty.className = "aaronnote-empty";
  empty.textContent = message;
  host.replaceChildren(empty);
}

export function createGraphPanel(options: {
  page: HTMLElement;
  filter: HTMLInputElement;
  stats: HTMLElement;
  canvas: HTMLElement;
  focusPanel: HTMLElement;
  getNotes: () => NoteSummary[];
  openNote: (note: NoteSummary) => void;
}): GraphPanel {
  let api: GraphApi | null = null;
  let scriptsReady: Promise<void> | null = null;
  const renderTimer = new CoalescedTimer(120);
  let dataKey = "";

  async function ensureScripts(): Promise<void> {
    if (scriptsReady) return scriptsReady;
    scriptsReady = (async () => {
      await loadScriptOnce("https://d3js.org/d3.v7.min.js");
      const toolsRoot = window.AaronnoteResolveAssetUrl?.("aaronnote-asset://roam-tools")
        ?? "aaronnote-asset://roam-tools";
      await loadScriptOnce(`${toolsRoot}/knowledge.js`);
      await loadScriptOnce(`${toolsRoot}/graph.js`);
    })();
    return scriptsReady;
  }

  function updatePublishData(): void {
    const graphNotes = roamNotes(options.getNotes());
    window.SITE_DATA = {
      meta: {
        generatedAt: new Date().toISOString(),
        noteCount: graphNotes.length,
        tagCount: new Set(graphNotes.flatMap((note) => note.tags ?? [])).size,
      },
      notes: graphNotes.map((note) => ({
        ...note,
        key: noteKey(note),
        link: note.link || note.path || "#",
        refs: note.refs ?? [],
        backlinks: note.backlinks ?? [],
        tags: note.tags ?? [],
        aliases: note.aliases ?? [],
      })),
    };
  }

  function currentDataKey(): string {
    return roamNotes(options.getNotes())
      .map((note) => [
        noteKey(note),
        note.title ?? "",
        note.path ?? "",
        (note.refs ?? []).join(","),
        (note.backlinks ?? []).join(","),
        (note.tags ?? []).join(","),
        (note.aliases ?? []).join(","),
      ].join("\t"))
      .join("\n");
  }

  function visibleKeysFromSharedSearch(): string[] {
    const query = options.filter.value.trim();
    const knowledge = window.KNOWLEDGE_DATA;
    if (!knowledge?.notes) return roamNotes(options.getNotes()).map(noteKey);
    const matched = query && typeof (knowledge as any).filterNotes === "function"
      ? (knowledge as any).filterNotes({ text: query, includeHidden: true }) as Array<NoteSummary & { key: string }>
      : knowledge.notes;
    return matched.map((note) => note.key || noteKey(note));
  }

  function render(): void {
    renderTimer.cancel();
    if (options.page.hidden) return;
    const nextDataKey = currentDataKey();
    updatePublishData();
    if (api?.setSearchQuery && dataKey === nextDataKey) {
      const visibleKeys = visibleKeysFromSharedSearch();
      options.stats.textContent = `${visibleKeys.length} nodes`;
      api.setSearchQuery(options.filter.value, { focus: false, refit: true });
      return;
    }
    void ensureScripts()
      .then(() => {
        if (options.page.hidden) return;
        if (!window.initKnowledgeGraph) throw new Error("Publish graph is unavailable");
        window.buildKnowledgeData?.();
        const visibleKeys = visibleKeysFromSharedSearch();
        options.stats.textContent = `${visibleKeys.length} nodes`;
        api?.destroy?.();
        dataKey = nextDataKey;
        api = window.initKnowledgeGraph({
          knowledge: window.KNOWLEDGE_DATA,
          container: options.canvas,
          focusPanel: options.focusPanel,
          toolbar: true,
          emptyMessage: "Select a node.",
          listenForGlobalFilters: false,
          dispatchTagEvents: true,
          initialSearchText: options.filter.value,
          onNoteOpen(note: NoteSummary) {
            const target = options.getNotes().find((item) => noteKey(item) === noteKey(note) || item.id === note.id);
            if (target) options.openNote(target);
          },
          initialVisibleKeys: visibleKeys,
        });
      })
      .catch((err) => {
        if (options.page.hidden) return;
        renderError(options.canvas, err instanceof Error ? err.message : "Graph failed");
      });
  }

  function scheduleRender(delay = 120): void {
    if (options.page.hidden) return;
    renderTimer.schedule(render, undefined, delay);
  }

  function dispose(): void {
    renderTimer.cancel();
    api?.destroy?.();
    api = null;
    dataKey = "";
    options.canvas.replaceChildren();
    options.focusPanel.replaceChildren();
  }

  return { dispose, render, scheduleRender };
}
