/**
 * React app-chrome: the editor top bar (Marker merge, phase 1 thin slice).
 *
 * This is the first React-owned surface in the otherwise vanilla shell. It renders
 * the header markup ONCE, preserving every `data-*` hook and button that
 * `aaronnote/main.ts` already queries and drives imperatively (file name, vim mode,
 * status text, TOC/Graph/Tools/Source/Save). Those nodes are static JSX, so React
 * never re-asserts them on re-render and the existing imperative wiring keeps working.
 *
 * The genuinely React-state-owned element is the live word count, fed through an
 * external store so updates re-render only that node — the CM6 editor surface stays
 * a plain-DOM island outside React entirely.
 */
import { useSyncExternalStore, type ReactElement } from "react";
import { createRoot, type Root } from "react-dom/client";
import { flushSync } from "react-dom";
import { cn } from "./lib/utils.ts";

export type DocStats = { words: number; chars: number };

export type WordCountStore = {
  subscribe(cb: () => void): () => void;
  getSnapshot(): DocStats;
  set(stats: DocStats): void;
};

export type TopBarHandle = {
  /** Update the live word/char count shown in the bar. */
  setStats(stats: DocStats): void;
  /** Tear down the React root (e.g. on hot reload / teardown). */
  unmount(): void;
};

const EMPTY_STATS: DocStats = { words: 0, chars: 0 };

/**
 * Matches the JavaScript regex `\s` whitespace set (including non-breaking and
 * CJK full-width spaces), so word counts agree with a `/\s+/` split.
 */
function isWhitespaceCode(code: number): boolean {
  return (
    code === 0x20 || code === 0x09 || code === 0x0a || code === 0x0d ||
    code === 0x0c || code === 0x0b || code === 0xa0 || code === 0x1680 ||
    (code >= 0x2000 && code <= 0x200a) || code === 0x2028 || code === 0x2029 ||
    code === 0x202f || code === 0x205f || code === 0x3000 || code === 0xfeff
  );
}

/**
 * Count whitespace-delimited words and characters of a markdown string.
 *
 * Single pass, O(1) extra space: unlike `trimmed.split(/\s+/).length`, this never
 * allocates a words array, which matters for large (multi-MB) documents recomputed
 * on the trailing edge of edits.
 */
export function countDocStats(markdown: string): DocStats {
  let words = 0;
  let inWord = false;
  for (let i = 0; i < markdown.length; i++) {
    if (isWhitespaceCode(markdown.charCodeAt(i))) {
      inWord = false;
    } else if (!inWord) {
      inWord = true;
      words += 1;
    }
  }
  return { words, chars: markdown.length };
}

function createWordCountStore(): WordCountStore {
  let state: DocStats = EMPTY_STATS;
  const listeners = new Set<() => void>();
  return {
    subscribe(cb) {
      listeners.add(cb);
      return () => listeners.delete(cb);
    },
    getSnapshot() {
      return state;
    },
    set(stats) {
      if (state.words === stats.words && state.chars === stats.chars) return;
      state = stats;
      listeners.forEach((cb) => cb());
    },
  };
}

function WordCount({ store }: { store: WordCountStore }): ReactElement {
  const stats = useSyncExternalStore(store.subscribe, store.getSnapshot, store.getSnapshot);
  return (
    <span
      data-wordcount
      title={`${stats.chars} characters`}
      className={cn(
        "aaronnote-wordcount-badge",
        "inline-flex items-center rounded-md bg-muted px-2 py-0.5",
        "text-xs font-medium text-muted-foreground tabular-nums select-none",
      )}
    >
      {stats.words} {stats.words === 1 ? "word" : "words"}
    </span>
  );
}

function TopBar({ store }: { store: WordCountStore }): ReactElement {
  return (
    <header className="aaronnote-focused-bar">
      <strong data-file>AaronNote</strong>
      <span data-vim-mode>INSERT</span>
      <span data-status>Opening...</span>
      <WordCount store={store} />
      <button type="button" data-toc-toggle aria-expanded="false">TOC</button>
      <button type="button" data-graph-toggle aria-expanded="false">Graph</button>
      <button type="button" data-tools-toggle aria-expanded="false">Tools</button>
      <button type="button" data-source>Source</button>
      <button type="button" data-save>Save</button>
    </header>
  );
}

/**
 * Render the React top bar into `container` and return an imperative handle.
 * Rendering is flushed synchronously so `main.ts` can query the `data-*` nodes
 * immediately after this returns.
 */
export function mountTopBar(container: HTMLElement): TopBarHandle {
  const store = createWordCountStore();
  let root: Root | null = createRoot(container);
  flushSync(() => root!.render(<TopBar store={store} />));
  return {
    setStats(stats) {
      store.set(stats);
    },
    unmount() {
      root?.unmount();
      root = null;
    },
  };
}
