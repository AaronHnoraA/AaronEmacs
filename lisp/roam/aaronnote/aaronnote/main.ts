import "../src/styles/tailwind.css";
import "../src/styles/widgets.css";
import "../src/styles/theme-typora.css";
import "./style.css";

import {
  createEditor,
  type EditorClipboardPayload,
  type EditorCommand,
  type StoredPasteAsset,
} from "../src/lib.ts";
import { setupCopilot } from "../src/copilot/index.ts";
import { continueMarkdownBlock, exitEmptyMarkdownBlock, indentMarkdownBlock, tableNavigateCell, tableEnterSameColumn } from "../src/cm6/commands.ts";
import { getBlockMathRanges, rangeAtPosition, rangeOverlapsAny } from "../src/cm6/math-ranges.ts";
import { equationTagsFromText, getEquationTagHits } from "../src/equation-tags.ts";
import { INLINE_MATH_RE, isLikelyInlineMath } from "../src/inline-math.ts";
import { formatMathRenderError, renderMathLazy } from "../src/math-render.ts";
import { hrefProtocol, safeHref } from "../src/url-safety.ts";
import { api } from "./api-client.ts";
import { Epoch } from "../src/async-epoch.ts";
import { CoalescedTimer } from "../src/coalesced-timer.ts";
import { blobToBase64 } from "../src/paste.ts";
import { AssistScheduler, type AssistUpdateFlags, type AssistUpdateOptions } from "./assist-scheduler.ts";
import { createFloatingTocPanel, inlineTagAnchorsFromText, markdownHeadingsFromText } from "./floating-toc.ts";
import { resolveAnchorHeading } from "../src/heading-slug.ts";
import { createLocalGraphPanel } from "./local-graph.ts";
import {
  canonicalRoamNoteId,
  escapeMarkdownLinkText,
  markdownRoamIdLink,
  resolveRoamNoteSearch,
  roamHrefForNote,
  roamNoteSearchValue,
} from "./roam-idlink.ts";
import { matchingSnippetsForPrefix, SnippetSession, snippetDetail, snippetLabel, snippetPopupKeyAction } from "./snippets.ts";
import type { CursorPosition, NoteSummary, SnippetSummary } from "./types.ts";
import { createVimCursor, updateVimCursor } from "./vim-cursor.ts";
import { createVimLite, type VimLiteKey, type VimLiteMode } from "./vim-lite.ts";
import {
  handleXwidgetControlBeforeInput,
  handleXwidgetControlKeydown,
  handleXwidgetEmacsKeydown,
  handleXwidgetSpecialBeforeInput,
  handleXwidgetSpecialKeydown,
  handleXwidgetVimBeforeInput,
  handleXwidgetVimKeydown,
} from "./xwidget-key-guard.ts";
import { mountTopBar, countDocStats } from "./ui/top-bar.tsx";

const root = document.querySelector<HTMLElement>("#app");
if (!root) throw new Error("Missing #app");

// The header is owned by React (aaronnote/ui/top-bar.tsx); the editor section stays a
// plain-DOM island that CodeMirror 6 fully controls. `display:contents` keeps the
// React mount transparent to the shell's flex layout.
root.innerHTML = `
  <main class="aaronnote-focused-shell">
    <div data-topbar-root style="display: contents"></div>
    <section class="aaronnote-focused-editor" data-editor></section>
  </main>
`;

const topbar = mountTopBar(root.querySelector<HTMLElement>("[data-topbar-root]")!);

const host = root.querySelector<HTMLElement>("[data-editor]")!;
const fileLabel = root.querySelector<HTMLElement>("[data-file]")!;
const modeLabel = root.querySelector<HTMLElement>("[data-vim-mode]")!;
const statusLabel = root.querySelector<HTMLElement>("[data-status]")!;
const tocButton = root.querySelector<HTMLButtonElement>("[data-toc-toggle]")!;
const graphButton = root.querySelector<HTMLButtonElement>("[data-graph-toggle]")!;
const toolsButton = root.querySelector<HTMLButtonElement>("[data-tools-toggle]")!;
const sourceButton = root.querySelector<HTMLButtonElement>("[data-source]")!;
const saveButton = root.querySelector<HTMLButtonElement>("[data-save]")!;

const graphPanelRoot = document.createElement("aside");
graphPanelRoot.className = "aaronnote-local-graph-panel is-collapsed";
graphPanelRoot.innerHTML = `
  <header>
    <strong>Local graph</strong>
    <button type="button" data-graph-close>Close</button>
  </header>
  <div class="aaronnote-local-graph-controls">
    <label>Depth <input type="range" data-graph-depth min="1" max="2" value="1" /></label>
    <span data-graph-depth-label>1</span>
    <label><input type="checkbox" data-graph-refs checked /> Refs</label>
    <label><input type="checkbox" data-graph-backlinks checked /> Back</label>
    <label><input type="checkbox" data-graph-tags checked /> Tags</label>
  </div>
  <div class="aaronnote-local-graph-canvas" data-graph-canvas></div>
  <div class="aaronnote-local-graph-status" data-graph-status></div>
`;
document.body.appendChild(graphPanelRoot);
const graphDepthInput = graphPanelRoot.querySelector<HTMLInputElement>("[data-graph-depth]")!;
const graphDepthLabel = graphPanelRoot.querySelector<HTMLElement>("[data-graph-depth-label]")!;
const graphRefsInput = graphPanelRoot.querySelector<HTMLInputElement>("[data-graph-refs]")!;
const graphBacklinksInput = graphPanelRoot.querySelector<HTMLInputElement>("[data-graph-backlinks]")!;
const graphTagsInput = graphPanelRoot.querySelector<HTMLInputElement>("[data-graph-tags]")!;
const graphCanvas = graphPanelRoot.querySelector<HTMLElement>("[data-graph-canvas]")!;
const graphStatus = graphPanelRoot.querySelector<HTMLElement>("[data-graph-status]")!;
const graphClose = graphPanelRoot.querySelector<HTMLButtonElement>("[data-graph-close]")!;

const toc = document.createElement("aside");
toc.className = "aaronnote-floating-toc is-collapsed";
toc.innerHTML = `<nav data-toc-list aria-label="Page outline"></nav>`;
document.body.appendChild(toc);
const tocList = toc.querySelector<HTMLElement>("[data-toc-list]")!;

const toolsPanel = document.createElement("div");
toolsPanel.className = "aaronnote-tools-panel";
toolsPanel.hidden = true;
toolsPanel.innerHTML = `
  <div class="aaronnote-tools-head">
    <strong>Tools</strong>
    <button type="button" data-tools-close>Close</button>
  </div>
  <div class="aaronnote-tools-list" data-tools-list></div>
`;
document.body.appendChild(toolsPanel);
const toolsList = toolsPanel.querySelector<HTMLElement>("[data-tools-list]")!;
const toolsClose = toolsPanel.querySelector<HTMLButtonElement>("[data-tools-close]")!;

const roamToolsPanel = document.createElement("section");
roamToolsPanel.className = "aaronnote-roam-tools";
roamToolsPanel.hidden = true;
roamToolsPanel.innerHTML = `
  <header>
    <strong data-roam-tools-title>Roam tools</strong>
    <button type="button" data-roam-tools-close>Close</button>
  </header>
  <div class="aaronnote-roam-tools-list" data-roam-tools-list></div>
`;
document.body.appendChild(roamToolsPanel);
const roamToolsTitle = roamToolsPanel.querySelector<HTMLElement>("[data-roam-tools-title]")!;
const roamToolsList = roamToolsPanel.querySelector<HTMLElement>("[data-roam-tools-list]")!;
const roamToolsClose = roamToolsPanel.querySelector<HTMLButtonElement>("[data-roam-tools-close]")!;

const modal = document.createElement("div");
modal.className = "aaronnote-modal";
modal.hidden = true;
document.body.appendChild(modal);
const snippetPopup = document.createElement("div");
snippetPopup.className = "aaronnote-snippet-popup";
snippetPopup.hidden = true;
snippetPopup.setAttribute("role", "listbox");
document.body.appendChild(snippetPopup);

const mathPreview = document.createElement("div");
mathPreview.className = "aaronnote-math-preview";
mathPreview.hidden = true;
document.body.appendChild(mathPreview);

const selectionTool = document.createElement("div");
selectionTool.className = "aaronnote-selection-tool";
selectionTool.innerHTML = `
  <button type="button" data-selection-command="bold" title="Bold">B</button>
  <button type="button" data-selection-command="italic" title="Italic">I</button>
  <button type="button" data-selection-command="highlight" title="Highlight">==</button>
  <button type="button" data-selection-command="strike" title="Strikethrough">~~</button>
  <button type="button" data-selection-command="code" title="Inline code">&lt;&gt;</button>
  <button type="button" data-selection-command="link" title="Link">@</button>
  <span aria-hidden="true"></span>
  <button type="button" data-selection-command="copy" title="Copy">Copy</button>
  <button type="button" data-selection-command="more" title="More actions">...</button>
  <div class="aaronnote-selection-more" data-selection-more hidden>
    <button type="button" data-selection-command="insert-roam-idlink">Insert roam idlink...</button>
  </div>
`;
selectionTool.hidden = true;
document.body.appendChild(selectionTool);
const selectionMore = selectionTool.querySelector<HTMLElement>("[data-selection-more]")!;
const selectionRoamIdlink = selectionTool.querySelector<HTMLButtonElement>("[data-selection-command='insert-roam-idlink']")!;

const vimCursor = createVimCursor();

let currentFile = "";
let currentClient = "";
let currentKind = "";
let currentStandalone = false;
let currentMtimeMs = 0;
let revision = 0;
let savedRevision = 0;
let applyingContent = false;
let saveTimer = 0;
let cursorPositionsLoaded = false;
let cursorPositions: CursorPosition[] = [];
let lastSavedCursorPositionKey = "";
let lastTrackedCursorPositionKey = "";
let cursorPositionFlushInFlight = false;
let cursorPositionFlushQueued = false;
let navigationBackStack: CursorPosition[] = [];
let restoringNavigationBack = false;
let snippets: SnippetSummary[] = [];
let notes: NoteSummary[] = [];
let pathSuggestions: string[] = [];
// Tracks the index version from the last notesIndexPayload response so we can
// detect when the server's watcher has bumped the index due to external changes.
let lastNotesIndexVersion = 0;
// True when a notes-index-changed event arrived while the page was hidden;
// triggers reloadNotes on the next visibility-restore.
let pendingNotesRefresh = false;
const notesRefreshTimer = new CoalescedTimer(500);
// Ephemeral request-level cache for completions — NOT a roam business cache.
// Holds results only for the duration of the current completion session (same
// context key). Discarded as soon as the context key changes.
const completionEpoch = new Epoch();
const completionTimer = new CoalescedTimer(60);
let completionContextKey = "";
let completionPendingItems: SnippetSummary[] | null = null;
let pendingOpenHash = "";
let pendingOpenDomTarget = "";
let snippetPopupItems: SnippetSummary[] = [];
let snippetPopupIndex = 0;
let snippetDeleteBefore = 0;
let snippetSuppressedPrefix = "";
let snippetRenderKey = "";
let snippetPopupMatchKey = "";
let paused = false;
const pauseReasons = new Set<string>();
let mathPreviewKey = "";
let mathPreviewPendingErrorKey = "";
let mathPreviewErrorTimer = 0;
let mathPreviewWidth = 0;
const clientId = globalThis.crypto?.randomUUID?.() ?? `${Date.now()}-${Math.random().toString(16).slice(2)}`;
const changeHandlers = new Set<() => void>();
const MATH_PREVIEW_ERROR_IDLE_MS = 650;
const MATH_PREVIEW_ERROR_MAX_LENGTH = 180;
const NAVIGATION_BACK_STACK_MAX = 80;
const editorCommands = new Set<EditorCommand>([
  "bold",
  "italic",
  "highlight",
  "strike",
  "code",
  "link",
  "blockquote",
  "bullet-list",
  "ordered-list",
  "task-list",
  "code-block",
  "paragraph-menu",
  "insert-table",
  "insert-math-block",
  "insert-toc",
  "insert-org-env",
  "image-edit",
  "table-insert-row",
  "table-insert-column",
  "table-delete-row",
  "table-delete-column",
  "heading-1",
  "heading-2",
  "heading-3",
  "heading-4",
  "heading-5",
  "heading-6",
  "fold-heading",
  "unfold-heading",
  "toggle-fold",
  "fold-all-headings",
  "unfold-all-headings",
  "copy-code",
]);

window.AaronnoteCurrentFile = () => currentFile;

async function uploadPasteBlobAsset(
  blob: Blob,
  meta: { file?: string; name?: string; type?: string },
): Promise<StoredPasteAsset> {
  return api.assets.upload({
    file: meta.file || currentFile,
    name: meta.name,
    type: meta.type || blob.type,
    data: await blobToBase64(blob),
  });
}

async function storePasteAssetFromPath(
  path: string,
  meta: { file?: string; name?: string; type?: string },
): Promise<StoredPasteAsset> {
  return api.assets.storeFromPath({
    file: meta.file || currentFile,
    path,
    name: meta.name,
    type: meta.type,
  });
}

async function readSystemClipboardForPaste(): Promise<EditorClipboardPayload | null> {
  try {
    const payload = await api.clipboard.read({ file: currentFile }) as EditorClipboardPayload;
    return payload && typeof payload === "object" ? payload : null;
  } catch {
    return null;
  }
}

function roamFeaturesEnabled(): boolean {
  return !currentStandalone;
}

function setStatus(message: string): void {
  statusLabel.textContent = message;
}

function updateTitle(): void {
  const name = currentFile.split(/[\\/]/).at(-1) || "AaronNote";
  fileLabel.textContent = name;
  document.title = revision === savedRevision ? name : `* ${name}`;
}

function updateModeLabel(mode: VimLiteMode): void {
  modeLabel.textContent = mode.toUpperCase();
  modeLabel.dataset.mode = mode;
  root.dataset.vimMode = mode;
  host.dataset.vimMode = mode;
  document.body.dataset.vimMode = mode;
  if (mode === "normal") noteCursorPositionEvent();
  scheduleAssistUpdate({ cursor: true });
}

function subscribe<K extends keyof DocumentEventMap>(
  type: K,
  handler: (event: DocumentEventMap[K]) => void,
  options?: AddEventListenerOptions,
): () => void {
  document.addEventListener(type, handler, options);
  return () => document.removeEventListener(type, handler, options);
}

// Forward ref patched after vim is created (avoids TDZ while keeping reset near vim).
let onBlurVimReset: (() => void) | undefined;

// Debounced live word count for the React top bar; recomputed off the trailing edge
// of edits so large documents don't pay a full scan per keystroke.
const wordCountTimer = new CoalescedTimer(300);
function scheduleWordCount(): void {
  wordCountTimer.schedule(() => topbar.setStats(countDocStats(editor.getMarkdown())));
}

const editor = createEditor(host, {
  initialContent: "",
  getCurrentFile: () => currentFile,
  pasteAssets: {
    uploadBlobAsset: uploadPasteBlobAsset,
    storeAssetFromPath: storePasteAssetFromPath,
  },
  readSystemClipboardFallback: readSystemClipboardForPaste,
  onChange: () => {
    if (!applyingContent) revision += 1;
    updateTitle();
    changeHandlers.forEach((handler) => handler());
    scheduleWordCount();
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true });
    scheduleSave();
  },
  onBlur: () => {
    onBlurVimReset?.();
    void flushCursorPosition();
  },
});

function activateEditorFromPointer(event: PointerEvent | MouseEvent): void {
  const target = event.target;
  if (!(target instanceof Node) || !host.contains(target)) return;
  const element = target instanceof Element ? target : target.parentElement;
  if (element?.closest("input, textarea, select, button, a")) return;
  // Two focus calls: the first is immediate, the second is deferred one tick.
  // xwidget may not deliver the first call if Emacs still holds focus at event
  // time; the deferred call lands after the event loop yields to WebKit.
  editor.focus();
  window.setTimeout(() => editor.focus(), 0);
}

host.addEventListener("pointerdown", activateEditorFromPointer, { capture: true });
host.addEventListener("mousedown", activateEditorFromPointer, { capture: true });

const snippetSession = new SnippetSession(editor);
host.addEventListener("aaronnote-assist-update", () => scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true }));

// IME switching for Vim mode (macOS) — fire-and-forget, never blocks keystrokes.
// Requires macism or im-select installed; feature silently disables when absent.
const imeCoalesceTimer = new CoalescedTimer(80);
let imeEnabled = true;
let imeLastSentMode: "" | "normal" | "insert" = "";
function syncImeForVimMode(mode: import("./vim-lite.ts").VimLiteMode): void {
  if (!imeEnabled) return;
  const effective: "normal" | "insert" = mode === "insert" ? "insert" : "normal";
  imeCoalesceTimer.schedule(() => {
    if (effective === "insert" && !document.hasFocus()) return;
    if (effective === imeLastSentMode) return;
    imeLastSentMode = effective;
    void api.ime.vimMode(effective)
      .then((r) => { if (r?.enabled === false) imeEnabled = false; })
      .catch(() => {});
  });
}

const vim = createVimLite(editor, host, {
  onModeChange: (mode) => { updateModeLabel(mode); syncImeForVimMode(mode); },
  onUndo: () => editor.undo(),
  onRedo: () => editor.redo(),
  onIndent: (dir) => indentMarkdownBlock(editor.view, dir),
  onFold: (action) => {
    if (action === "close") return editor.runCommand("fold-heading");
    if (action === "open") return editor.runCommand("unfold-heading");
    if (action === "toggle") return editor.runCommand("toggle-fold");
    if (action === "close-all") return editor.runCommand("fold-all-headings");
    return editor.runCommand("unfold-all-headings");
  },
});
const assistScheduler = new AssistScheduler(window, editorSurfaceVisible, runAssistUpdate);
updateModeLabel(vim.mode());
// Reset to normal mode when the editor loses focus (xwidget buffer switch).
// Prevents silent insert/visual mode on return from another Emacs buffer.
onBlurVimReset = () => {
  if (vim.mode() !== "normal") vim.setMode("normal");
  imeLastSentMode = "";
  syncImeForVimMode("normal");
};
// Re-assert IME state when the window regains focus.
window.addEventListener("focus", () => {
  imeLastSentMode = "";
  syncImeForVimMode(vim.mode());
});

const floatingTocPanel = createFloatingTocPanel({
  toc,
  toggleButton: tocButton,
  list: tocList,
  editor,
  getNotes: () => notes,
  getCurrentFile: () => currentFile,
  resolveNoteRef,
  openNote,
  openTag: openTagFilter,
});

const localGraphPanel = createLocalGraphPanel({
  root: graphPanelRoot,
  toggleButton: graphButton,
  depthInput: graphDepthInput,
  depthLabel: graphDepthLabel,
  refsInput: graphRefsInput,
  backlinksInput: graphBacklinksInput,
  tagsInput: graphTagsInput,
  canvas: graphCanvas,
  status: graphStatus,
  getNotes: () => notes.filter(note => note.roam !== false),
  getCurrentNote: currentNote,
  getMarkdown: () => editor.getMarkdown(),
  resolveNoteRef,
  openNote,
  openTag: openTagFilter,
});

graphClose.addEventListener("click", () => localGraphPanel.collapse());

function saveBody() {
  return {
    file: currentFile,
    content: editor.getMarkdown(),
    mode: editor.isSourceMode() ? "source" : "markdown",
    clientId,
    seq: revision,
    baseMtimeMs: currentMtimeMs,
    refresh: "deferred",
  };
}

async function save(): Promise<void> {
  window.clearTimeout(saveTimer);
  if (!currentFile || revision === savedRevision) return;
  const savingRevision = revision;
  const savingFile = currentFile;
  setStatus("Saving...");
  try {
    const result = await api.notes.save(saveBody());
    // If the user switched notes while this save was in flight, discard the
    // result — applying metadata to the new note would corrupt its dirty tracking.
    if (savingFile !== currentFile) return;
    if (result.conflict) {
      setStatus(result.message || "Save conflict; reopen from Emacs");
      return;
    }
    currentMtimeMs = Number(result.mtimeMs) || currentMtimeMs;
    applyIndexPayload(result);
    savedRevision = Math.max(savedRevision, savingRevision);
    updateTitle();
    setStatus(revision === savedRevision ? "Saved" : "Edited");
  } catch (error) {
    setStatus(error instanceof Error ? error.message : "Save failed");
  }
}

function scheduleSave(): void {
  window.clearTimeout(saveTimer);
  if (!currentFile || applyingContent || revision === savedRevision) return;
  setStatus("Edited");
  saveTimer = window.setTimeout(() => void save(), 650);
}

function cursorPositionKey(position: Pick<CursorPosition, "file" | "mode" | "from" | "to" | "scrollY">): string {
  return [
    position.file,
    position.mode,
    Math.max(0, Math.floor(position.from)),
    Math.max(0, Math.floor(position.to)),
    Math.max(0, Math.floor(position.scrollY)),
  ].join("|");
}

function currentCursorPosition(): CursorPosition | null {
  if (!currentFile) return null;
  const { from, to } = editor.getMarkdownSelection();
  return {
    file: currentFile,
    mode: editor.isSourceMode() ? "source" : "markdown",
    from: Math.max(0, from),
    to: Math.max(0, to),
    scrollY: Math.max(0, Math.floor(window.scrollY || 0)),
    updatedAt: Date.now(),
  };
}

function rememberCursorPosition(position: CursorPosition, positions?: CursorPosition[]): void {
  if (Array.isArray(positions)) {
    cursorPositions = positions;
    return;
  }
  const index = cursorPositions.findIndex((entry) => entry.file === position.file);
  if (index >= 0) cursorPositions[index] = position;
  else cursorPositions.unshift(position);
}

async function loadCursorPositions(): Promise<CursorPosition[]> {
  if (cursorPositionsLoaded) return cursorPositions;
  cursorPositionsLoaded = true;
  try {
    const result = await api.session.getPositions();
    cursorPositions = Array.isArray(result.positions) ? result.positions : [];
  } catch {
    cursorPositions = [];
  }
  return cursorPositions;
}

function rememberedCursorPosition(file: string, positions = cursorPositions): CursorPosition | undefined {
  return positions.find((position) => position.file === file);
}

function trackCursorPosition(): CursorPosition | null {
  const position = currentCursorPosition();
  if (!position) return null;
  const key = cursorPositionKey(position);
  if (key !== lastTrackedCursorPositionKey) {
    lastTrackedCursorPositionKey = key;
    rememberCursorPosition(position);
  }
  return position;
}

async function persistCursorPosition(position: CursorPosition): Promise<void> {
  const key = cursorPositionKey(position);
  if (key === lastSavedCursorPositionKey) return;
  if (cursorPositionFlushInFlight) {
    cursorPositionFlushQueued = true;
    return;
  }
  cursorPositionFlushInFlight = true;
  try {
    const result = await api.session.savePosition(position);
    rememberCursorPosition(position, result.positions);
    lastSavedCursorPositionKey = key;
  } catch {
    // Cursor position memory is best-effort and should never block editing.
  } finally {
    cursorPositionFlushInFlight = false;
    if (cursorPositionFlushQueued) {
      cursorPositionFlushQueued = false;
      const latest = trackCursorPosition();
      if (latest && cursorPositionKey(latest) !== lastSavedCursorPositionKey) {
        void persistCursorPosition(latest);
      }
    }
  }
}

function noteCursorPositionEvent(): void {
  trackCursorPosition();
}

function pushNavigationBackLocation(location = trackCursorPosition()): void {
  if (!location || restoringNavigationBack) return;
  const key = cursorPositionKey(location);
  const top = navigationBackStack[navigationBackStack.length - 1];
  if (top && cursorPositionKey(top) === key) return;
  navigationBackStack.push({ ...location, updatedAt: Date.now() });
  if (navigationBackStack.length > NAVIGATION_BACK_STACK_MAX) {
    navigationBackStack = navigationBackStack.slice(-NAVIGATION_BACK_STACK_MAX);
  }
  try {
    window.history.pushState({ aaronnoteNavigation: true }, "", window.location.href);
  } catch {
    // Browser history is an optional convenience; the in-memory stack remains valid.
  }
}

function restoreCursorPosition(location: CursorPosition): void {
  const length = editor.getMarkdown().length;
  const from = Math.min(Math.max(0, location.from), length);
  const to = Math.min(Math.max(0, location.to), length);
  if ((location.mode === "source") !== editor.isSourceMode()) editor.toggleSource();
  sourceButton.classList.toggle("is-active", editor.isSourceMode());
  editor.setMarkdownSelection(from, to);
  editor.revealCursor();
  editor.focus();
  trackCursorPosition();
  scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true, selectionTool: true });
}

async function restoreNavigationBack(): Promise<boolean> {
  const location = navigationBackStack.pop();
  if (!location) return false;
  restoringNavigationBack = true;
  try {
    if (location.file !== currentFile) await openFile(location.file);
    restoreCursorPosition(location);
    return true;
  } finally {
    restoringNavigationBack = false;
  }
}

async function flushCursorPosition(): Promise<void> {
  const position = trackCursorPosition();
  if (position) await persistCursorPosition(position);
}

function applyOpenedNote(
  opened: Awaited<ReturnType<typeof api.notes.bootstrap>>,
  fallbackFile?: string,
  rememberedPositions: CursorPosition[] = cursorPositions,
): void {
  currentFile = String(opened.file || fallbackFile || "");
  currentKind = String(opened.kind || "");
  currentStandalone = Boolean(opened.standalone);
  applyIndexPayload(opened);
  if (Array.isArray(opened.snippets)) snippets = opened.snippets;
  currentMtimeMs = Number(opened.mtimeMs) || 0;
  const remembered = !opened.selection && !pendingOpenHash && !pendingOpenDomTarget
    ? rememberedCursorPosition(currentFile, rememberedPositions)
    : undefined;
  applyingContent = true;
  editor.setMarkdown(String(opened.content || ""), { history: "reset" });
  revision = 0;
  savedRevision = 0;
  const mode = remembered?.mode || opened.mode;
  if ((mode === "source") !== editor.isSourceMode()) editor.toggleSource();
  sourceButton.classList.toggle("is-active", editor.isSourceMode());
  const from = Number(opened.selection?.from ?? remembered?.from);
  const to = Number(opened.selection?.to ?? remembered?.to ?? from);
  if (Number.isFinite(from)) {
    const length = editor.getMarkdown().length;
    const safeFrom = Math.min(Math.max(0, from), length);
    const safeTo = Math.min(Math.max(0, Number.isFinite(to) ? to : from), length);
    editor.setMarkdownSelection(safeFrom, safeTo);
    editor.revealCursor();
  }
  applyingContent = false;
  const restored = currentCursorPosition();
  lastSavedCursorPositionKey = restored ? cursorPositionKey(restored) : "";
  lastTrackedCursorPositionKey = lastSavedCursorPositionKey;
  if (restored) rememberCursorPosition(restored);
  snippetSession.clear();
  hideSnippetPopup();
  hideMathPreview();
  selectionTool.hidden = true;
  selectionMore.hidden = true;
  vim.setMode("insert");
  updateTitle();
  void api.emacs.currentFile(currentFile, currentClient);
  setStatus(currentFile ? "Ready" : "Scratch");
  editor.focus();
  scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true });
  const targetHash = pendingOpenHash;
  const targetDom = pendingOpenDomTarget;
  pendingOpenHash = "";
  pendingOpenDomTarget = "";
  if (targetHash || targetDom) {
    window.requestAnimationFrame(() => {
      if (targetDom && jumpToDomTarget(targetDom)) return;
      if (targetHash && jumpToHash(targetHash)) return;
      setStatus(targetDom ? `DOM target not found: ${targetDom}` : `Anchor not found: ${targetHash}`);
    });
  }
  void reloadNotes(false);
  if (!Array.isArray(opened.snippets) && snippets.length === 0) void reloadSnippets();
}

async function openFile(file?: string, bootstrap = false): Promise<void> {
  const target = file || undefined;
  try {
    if (currentFile) await flushCursorPosition();
    if (currentFile && revision !== savedRevision) {
      await save();
      if (revision !== savedRevision) return;
    }
    const openPromise = target && !bootstrap
      ? api.notes.open(target)
      : api.notes.bootstrap(target);
    const [opened, positions] = await Promise.all([openPromise, loadCursorPositions()]);
    applyOpenedNote(opened, target, positions);
  } catch (error) {
    applyingContent = false;
    setStatus(error instanceof Error ? error.message : "Open failed");
  }
}

async function openInitialFile(): Promise<void> {
  const params = new URLSearchParams(window.location.search);
  const file = params.get("file") || undefined;
  currentClient = params.get("client") || "";
  pendingOpenHash = params.get("hash") || "";
  pendingOpenDomTarget = params.get("dom") || "";
  await openFile(file, true);
}

setupCopilot({
  editor,
  host,
  currentFile: () => currentFile,
  vimMode: () => vim.mode(),
  setStatus,
  onChange: (handler) => {
    changeHandlers.add(handler);
    return () => changeHandlers.delete(handler);
  },
  onKeyDown: (handler) => {
    const listener = (event: KeyboardEvent) => {
      if (handler(event)) event.stopPropagation();
    };
    document.addEventListener("keydown", listener, true);
    return () => document.removeEventListener("keydown", listener, true);
  },
  onAction: () => () => {},
  onSettingsChange: () => () => {},
  getSettings: () => ({ idleDelayMs: 850, largeBufferThresholdKb: 512 }),
  isActive: () => !paused && editorSurfaceVisible(),
  onDocumentEvent: subscribe,
  jumpSnippetNext: jumpSnippetTabstop,
  jumpSnippetPrevious: jumpSnippetTabstopBack,
  forwardDelimiter: () => false,
  backwardDelimiter: () => false,
});

function toggleSourceMode(): void {
  editor.toggleSource();
  sourceButton.classList.toggle("is-active", editor.isSourceMode());
  editor.focus();
  scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
}

function isEditorCommand(command: string): command is EditorCommand {
  return editorCommands.has(command as EditorCommand);
}

function primaryMod(event: KeyboardEvent): boolean {
  return /Mac/.test(navigator.platform)
    ? event.metaKey && !event.ctrlKey
    : event.ctrlKey && !event.metaKey;
}

function plainEscapeKey(event: KeyboardEvent): boolean {
  return event.key === "Escape" && !event.metaKey && !event.ctrlKey && !event.altKey && !event.isComposing;
}

function runFormattingShortcut(event: KeyboardEvent): boolean {
  if (!primaryMod(event) || event.altKey || event.isComposing) return false;
  const key = event.key.toLowerCase();
  const command: EditorCommand | "" = event.shiftKey && key === "x" ? "strike"
    : !event.shiftKey && key === "b" ? "bold"
    : !event.shiftKey && key === "i" ? "italic"
    : !event.shiftKey && key === "k" ? "link"
    : "";
  if (!command) return false;
  event.preventDefault();
  editor.runCommand(command);
  editor.focus();
  return true;
}

function runSourceToggleShortcut(event: KeyboardEvent): boolean {
  if (!primaryMod(event) || event.shiftKey || event.altKey || event.isComposing) return false;
  if (event.key !== "/" && event.code !== "Slash") return false;
  event.preventDefault();
  toggleSourceMode();
  return true;
}

function fileNameFromPath(path: string): string {
  return String(path || "").split(/[\\/]/).filter(Boolean).at(-1) || path || "";
}

function decodeNoteRef(ref: string): string {
  try {
    return decodeURIComponent(ref);
  } catch {
    return ref;
  }
}

function encodeMarkdownHrefPath(path: string): string {
  return String(path || "")
    .replace(/\\/g, "/")
    .split("/")
    .map((part) => encodeURIComponent(decodeNoteRef(part)))
    .join("/");
}

function applyIndexPayload(payload: { notes?: NoteSummary[]; note?: NoteSummary; kind?: string; standalone?: boolean; indexVersion?: number }): void {
  if (typeof payload.indexVersion === "number" && payload.indexVersion > lastNotesIndexVersion) {
    lastNotesIndexVersion = payload.indexVersion;
  }
  if (Array.isArray(payload.notes)) notes = payload.notes;
  else if (payload.note?.file) {
    const index = notes.findIndex((note) => note.file === payload.note?.file);
    if (index >= 0) notes = notes.map((note, i) => i === index ? payload.note! : note);
    else notes = [...notes, payload.note];
  }
  if (typeof payload.kind === "string") currentKind = payload.kind;
  if (typeof payload.standalone === "boolean") currentStandalone = payload.standalone;
  pathSuggestions = [...new Set(notes
    .flatMap((note) => [note.path, note.file, note.link])
    .map((value) => String(value || "").trim())
    .filter(Boolean))]
    .sort((a, b) => a.localeCompare(b));
  scheduleAssistUpdate({ toc: true });
  localGraphPanel.invalidate();
}

async function reloadNotes(force = false): Promise<void> {
  try {
    const msg = await api.notes.list(force);
    applyIndexPayload(msg);
    void loadPathSuggestions();
  } catch (error) {
    if (force) setStatus(error instanceof Error ? error.message : "Note index failed");
  }
}

async function loadPathSuggestions(): Promise<void> {
  if (!currentFile) return;
  try {
    const msg = await api.notes.pathSuggestions(currentFile);
    if (Array.isArray(msg.paths)) pathSuggestions = msg.paths;
  } catch {
    // Keep the coarse note-index suggestions from applyIndexPayload.
  }
}

function currentNote(): NoteSummary | undefined {
  return notes.find((note) => note.file === currentFile)
    ?? notes.find((note) => note.path === currentFile || note.link === currentFile);
}

function noteSearchValues(note: NoteSummary): string[] {
  return [
    note.id,
    note.key,
    note.title,
    note.path,
    note.link,
    note.source,
    note.file,
    ...(note.aliases ?? []),
    ...(note.tags ?? []),
  ].map((value) => String(value || "").trim()).filter(Boolean);
}

function resolveNoteRef(ref: string): NoteSummary | undefined {
  const raw = decodeNoteRef(String(ref || "").replace(/^roam:\/\//i, "").split(/[?#@]/, 1)[0] || "").trim();
  if (!raw) return undefined;
  const key = raw.toLowerCase();
  return notes.find((note) => noteSearchValues(note).some((value) => value.toLowerCase() === key))
    ?? notes.find((note) => noteSearchValues(note).some((value) => value.toLowerCase().includes(key)));
}

function openNote(
  note: NoteSummary,
  options: { newWindow?: boolean; hash?: string; domTarget?: string; equationTag?: string; inlineTag?: string } = {},
): void {
  if (!note.file) return;
  const before = trackCursorPosition();
  noteCursorPositionEvent();
  if (options.newWindow) {
    const url = new URL(window.location.href);
    url.searchParams.set("file", note.file);
    if (options.hash) url.searchParams.set("hash", options.hash);
    if (options.domTarget) url.searchParams.set("dom", options.domTarget);
    window.open(url.toString(), "_blank", "noopener,noreferrer");
    return;
  }
  if (note.file === currentFile) {
    let jumped = false;
    if (options.domTarget) {
      jumped = jumpToDomTarget(options.domTarget);
      if (!jumped) setStatus(`DOM target not found: ${options.domTarget}`);
    } else if (options.hash) {
      jumped = jumpToHash(options.hash);
      if (!jumped) setStatus(`Anchor not found: ${options.hash}`);
    }
    if (jumped) pushNavigationBackLocation(before);
    if (options.domTarget || options.hash) return;
  }
  pushNavigationBackLocation(before);
  pendingOpenHash = options.hash || "";
  pendingOpenDomTarget = options.domTarget || "";
  void openFile(note.file);
}

function cleanHref(href: string): string {
  return String(href || "").trim();
}

function hrefPath(href: string): string {
  const raw = cleanHref(href);
  if (!raw) return "";
  if (/^file:\/\//i.test(raw)) {
    try {
      return decodeNoteRef(new URL(raw).pathname);
    } catch {
      return decodeNoteRef(raw.replace(/^file:\/\//i, ""));
    }
  }
  const path = raw
    .replace(/^file:/i, "")
    .split(/[?#]/, 1)[0]
    .trim();
  const fileDomMatch = path.match(/^(.+?\.(?:md|markdown|typ))@/i);
  return decodeNoteRef(fileDomMatch?.[1] || path);
}

function hrefHash(href: string): string {
  const raw = cleanHref(href);
  const hashIndex = raw.indexOf("#");
  if (hashIndex < 0) return "";
  return decodeNoteRef(raw.slice(hashIndex + 1).split(/[?&]/, 1)[0] || "").trim();
}

function normalizeNotePath(path: string): string {
  const normalized = String(path || "").replace(/\\/g, "/");
  const absolute = normalized.startsWith("/");
  const parts: string[] = [];
  for (const part of normalized.split("/")) {
    if (!part || part === ".") continue;
    if (part === "..") {
      if (parts.length > 0 && parts[parts.length - 1] !== "..") parts.pop();
      else if (!absolute) parts.push(part);
      continue;
    }
    parts.push(part);
  }
  return `${absolute ? "/" : ""}${parts.join("/")}`;
}

function dirnamePath(path: string): string {
  const normalized = normalizeNotePath(path);
  const index = normalized.lastIndexOf("/");
  if (index < 0) return "";
  if (index === 0) return "/";
  return normalized.slice(0, index);
}

function joinNotePath(baseDir: string, path: string): string {
  if (!baseDir || path.startsWith("/")) return normalizeNotePath(path);
  return normalizeNotePath(`${baseDir}/${path}`);
}

function notePathKey(value: unknown): string {
  return normalizeNotePath(String(value || "")
    .replace(/^file:(?:\/\/)?/i, "")
    .replace(/^\.\/+/, ""));
}

function noteMatchesPath(note: NoteSummary, path: string): boolean {
  const key = notePathKey(path);
  if (!key) return false;
  return [note.path, note.link, note.file, note.source]
    .map(notePathKey)
    .some((value) => value === key || value.endsWith(`/${key}`));
}

function noteHrefCandidates(href: string): string[] {
  const path = hrefPath(href);
  const candidates = new Set<string>();
  const add = (value: string) => {
    const normalized = normalizeNotePath(value);
    if (normalized) candidates.add(normalized);
  };
  add(path);
  add(path.replace(/^\.\/+/, ""));
  if (!path.startsWith("/") && currentFile) add(joinNotePath(dirnamePath(currentFile), path));
  const note = currentNote();
  if (!path.startsWith("/") && note?.path) add(joinNotePath(dirnamePath(note.path), path));
  return [...candidates];
}

function markdownNoteHref(href: string): boolean {
  const protocol = hrefProtocol(href);
  if (protocol && protocol !== "file") return false;
  return /\.(?:md|markdown|typ)$/i.test(hrefPath(href));
}

function splitRoamLikeHref(href: string): { ref: string; hash: string; dom: string } | null {
  const raw = cleanHref(href);
  if (!raw || (hrefProtocol(raw) && !/^roam:\/\//i.test(raw))) return null;
  let body = raw.replace(/^roam:\/\//i, "").split(/[?&]/, 1)[0] || "";
  let hash = "";
  const hashIndex = body.indexOf("#");
  if (hashIndex >= 0) {
    hash = decodeNoteRef(body.slice(hashIndex + 1));
    body = body.slice(0, hashIndex);
  }
  let dom = "";
  const fileDomMatch = body.match(/^(.+?\.(?:md|markdown|typ))@(.+)$/i);
  if (fileDomMatch) {
    body = fileDomMatch[1] || "";
    dom = normalizeDomTargetPath(fileDomMatch[2] || "");
  } else {
    const atIndex = body.indexOf("@");
    if (atIndex >= 0) {
      dom = normalizeDomTargetPath(body.slice(atIndex + 1));
      body = body.slice(0, atIndex);
    }
  }
  const ref = decodeNoteRef(body.replace(/^\/+/, "").replace(/[.,;:]+$/, "")).trim();
  if (!ref && !hash && !dom) return null;
  return { ref, hash: hash.trim(), dom };
}

function resolveHrefNote(href: string): NoteSummary | undefined {
  const raw = cleanHref(href);
  if (!raw) return undefined;
  const roamLike = splitRoamLikeHref(raw);
  if (roamLike?.ref && /^roam:\/\//i.test(raw)) return resolveNoteRef(roamLike.ref);
  const path = hrefPath(raw);
  for (const candidate of noteHrefCandidates(raw)) {
    const exactPath = notes.find((note) => noteMatchesPath(note, candidate));
    if (exactPath?.file) return exactPath;
    const byRef = resolveNoteRef(candidate);
    if (byRef?.file) return byRef;
  }
  if (!hrefProtocol(raw) && path && !markdownNoteHref(raw)) return resolveNoteRef(path);
  return undefined;
}

function resolveHrefTarget(href: string): { note?: NoteSummary; hash: string; domTarget: string } {
  const raw = cleanHref(href);
  const roamLike = splitRoamLikeHref(raw);
  if (roamLike) {
    const note = roamLike.ref ? resolveNoteRef(roamLike.ref) : undefined;
    if (note?.file || /^roam:\/\//i.test(raw)) return { note, hash: roamLike.hash, domTarget: roamLike.dom };
  }
  return {
    note: resolveHrefNote(raw),
    hash: hrefHash(raw),
    domTarget: roamLike?.dom || "",
  };
}

function slugifyAnchor(value: string): string {
  return String(value || "")
    .normalize("NFKC")
    .trim()
    .toLowerCase()
    .replace(/[^\p{L}\p{N}]+/gu, "-")
    .replace(/^-+|-+$/g, "");
}

function jumpToHash(hash: string): boolean {
  const clean = normalizeInlineTag(hash.replace(/^#/, ""));
  if (!clean) return false;
  const equationTag = clean.replace(/^eq-/i, "");
  const equation = getEquationTagHits(editor.view.state)
    .find((hit) => hit.tag.toLowerCase() === equationTag.toLowerCase());
  if (equation) {
    editor.setMarkdownSelection(equation.from, equation.to);
    editor.revealCursor();
    editor.focus();
    noteCursorPositionEvent();
    return true;
  }
  const inline = inlineTagAnchorsFromText(editor.getMarkdown())
    .find((anchor) => anchor.tag.toLowerCase() === clean.toLowerCase()
      || `tag-${anchor.tag}`.toLowerCase() === clean.toLowerCase());
  if (inline) {
    editor.setMarkdownSelection(inline.pos, inline.to);
    editor.revealCursor();
    editor.focus();
    noteCursorPositionEvent();
    return true;
  }
  const allHeadings = markdownHeadingsFromText(editor.view.state.doc);
  const heading = resolveAnchorHeading(allHeadings, clean)
    ?? allHeadings.find((item) => item.text.toLowerCase() === clean.toLowerCase()
      || item.slug === clean
      || slugifyAnchor(item.text) === clean);
  if (heading) {
    editor.setMarkdownSelection(heading.pos);
    editor.revealCursor();
    editor.focus();
    noteCursorPositionEvent();
    return true;
  }
  return false;
}

function openExternalUrl(href: string, options: { newWindow?: boolean } = {}): void {
  const raw = cleanHref(href);
  if (!raw) return;
  if (!safeHref(raw)) {
    setStatus("Blocked unsafe link");
    return;
  }
  const before = trackCursorPosition();
  noteCursorPositionEvent();
  const hash = hrefHash(raw);
  const target = resolveHrefTarget(raw);
  const note = target.note;
  const targetHash = target.hash || hash;
  const targetDom = target.domTarget;
  if (note?.file) {
    if (note.file === currentFile && targetDom) {
      const jumped = jumpToDomTarget(targetDom);
      if (jumped) pushNavigationBackLocation(before);
      else setStatus(`DOM target not found: ${targetDom}`);
      return;
    }
    if (note.file === currentFile && targetHash) {
      const jumped = jumpToHash(targetHash);
      if (jumped) pushNavigationBackLocation(before);
      else setStatus(`Anchor not found: ${targetHash}`);
      return;
    }
    openNote(note, { newWindow: options.newWindow, hash: targetHash, domTarget: targetDom });
    return;
  }
  if (/^roam:\/\//i.test(raw)) {
    setStatus(`Roam note not found: ${splitRoamLikeHref(raw)?.ref || raw}`);
    return;
  }
  if (raw.startsWith("#")) {
    const jumped = jumpToHash(hash || raw.slice(1));
    if (jumped) pushNavigationBackLocation(before);
    else setStatus(`Anchor not found: ${hash || raw.slice(1)}`);
    return;
  }
  const protocol = hrefProtocol(raw);
  if (!protocol) {
    const targetPath = hrefPath(raw) || raw;
    void api.emacs.systemOpen(targetPath, currentFile)
      .catch((err) => setStatus(err instanceof Error ? err.message : `Cannot open: ${targetPath}`));
    return;
  }
  if (protocol === "zotero") {
    void api.emacs.systemOpen(raw)
      .then(() => setStatus("Opened Zotero link"))
      .catch((err) => setStatus(err instanceof Error ? err.message : "Failed to open Zotero link"));
    return;
  }
  if (options.newWindow) {
    window.open(raw, "_blank", "noopener,noreferrer");
    return;
  }
  window.location.href = raw;
}

function relationTags(note: NoteSummary | undefined): string[] {
  return [...new Set([...(note?.tags ?? []), ...(note?.inlineTags ?? [])]
    .map((tag) => String(tag || "").trim().replace(/^#/, ""))
    .filter(Boolean))]
    .sort((a, b) => a.localeCompare(b));
}

function openTagFilter(tag: string): void {
  const clean = String(tag || "").trim().replace(/^#/, "");
  if (!clean) return;
  const rows = notes
    .filter((note) => relationTags(note).some((item) => item.toLowerCase() === clean.toLowerCase()))
    .map((note) => ({
      title: note.title || note.path || note.file || canonicalRoamNoteId(note) || "Untitled",
      detail: [note.path || note.file || "", relationTags(note).join(", ")].filter(Boolean).join(" - "),
      kind: "TAG",
    }));
  showRoamToolRows(`#${clean}`, rows);
}

function normalizeInlineTag(value: string): string {
  return String(value || "")
    .replace(/[\r\n\[\]]/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

type DomTargetEntry = {
  label: string;
  slug: string;
  path: string[];
  labelPath: string[];
  level?: number;
  pos?: number;
  to?: number;
  notePath?: string;
};

function normalizeDomTarget(value: string): string {
  return decodeNoteRef(String(value || ""))
    .replace(/^@/, "")
    .replace(/[\r\n\[\]]/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function domTargetPathSegments(value: string): string[] {
  return String(value || "")
    .trim()
    .replace(/^@+/, "")
    .split("@")
    .map((segment) => normalizeDomTarget(segment))
    .filter(Boolean);
}

function slugDomTarget(value: string): string {
  return normalizeDomTarget(value)
    .toLowerCase()
    .replace(/[`*_~()[\]{}#+.!<>:;,'"@]/g, " ")
    .trim()
    .replace(/\s+/g, "-");
}

function normalizeDomTargetPath(value: string): string {
  return domTargetPathSegments(value)
    .map(slugDomTarget)
    .filter(Boolean)
    .join("@");
}

function domTargetPathLabel(path: readonly string[]): string {
  return path.filter(Boolean).join(" / ");
}

function targetSegmentMatches(actual: string, wanted: string): boolean {
  const actualNorm = normalizeDomTarget(actual).toLowerCase();
  const wantedNorm = normalizeDomTarget(wanted).toLowerCase();
  if (actualNorm && actualNorm === wantedNorm) return true;
  const actualSlug = slugDomTarget(actual);
  const wantedSlug = slugDomTarget(wanted);
  return Boolean(actualSlug && wantedSlug && actualSlug === wantedSlug);
}

function targetPathMatches(actualPath: readonly string[], wantedPath: readonly string[], allowSuffix = true): boolean {
  if (wantedPath.length === 0 || actualPath.length === 0) return false;
  const pathMatchesAt = (offset: number) => wantedPath.every((segment, index) =>
    targetSegmentMatches(actualPath[offset + index] || "", segment));
  if (actualPath.length === wantedPath.length && pathMatchesAt(0)) return true;
  if (!allowSuffix || actualPath.length < wantedPath.length) return false;
  return pathMatchesAt(actualPath.length - wantedPath.length);
}

function findDomTargetEntry(entries: readonly DomTargetEntry[], rawTarget: string): DomTargetEntry | undefined {
  const targetPath = domTargetPathSegments(rawTarget);
  if (targetPath.length === 0) return undefined;
  if (targetPath.length > 1) {
    return entries.find((entry) => targetPathMatches(entry.path, targetPath, false))
      ?? entries.find((entry) => targetPathMatches(entry.path, targetPath, true));
  }
  const target = targetPath[0] || "";
  const targetSlug = slugDomTarget(target);
  const targetNorm = normalizeDomTarget(target).toLowerCase();
  return entries.find((entry) => {
    const label = normalizeDomTarget(entry.label).toLowerCase();
    return label === targetNorm || entry.slug === targetSlug || entry.slug === targetNorm;
  });
}

function currentDomTargets(): DomTargetEntry[] {
  const stack: string[] = [];
  const labelStack: string[] = [];
  return markdownHeadingsFromText(editor.view.state.doc).map((heading) => {
    const level = Math.max(1, Number(heading.level || 1));
    const label = normalizeDomTarget(heading.text);
    const slug = heading.slug || slugDomTarget(label);
    stack.length = Math.min(stack.length, level - 1);
    labelStack.length = Math.min(labelStack.length, level - 1);
    stack.push(slug);
    labelStack.push(label);
    return {
      label,
      slug,
      path: [...stack],
      labelPath: [...labelStack],
      level,
      pos: heading.pos,
      to: heading.to ?? heading.pos + heading.text.length,
    };
  });
}

function jumpToDomTarget(rawTarget: string): boolean {
  const target = normalizeDomTargetPath(rawTarget);
  if (!target) return false;
  const hit = findDomTargetEntry(currentDomTargets(), target);
  if (!hit) return false;
  editor.setMarkdownSelection(hit.pos ?? 0, hit.to ?? hit.pos ?? 0);
  editor.revealCursor();
  editor.focus();
  setStatus(`DOM target ${target}`);
  scheduleAssistUpdate({ toc: true });
  noteCursorPositionEvent();
  return true;
}

function tagSlugSegment(value: string): string {
  return String(value || "")
    .normalize("NFKC")
    .trim()
    .replace(/[^\p{L}\p{N}]+/gu, "-")
    .replace(/^-+|-+$/g, "")
    .replace(/-+/g, "-")
    .toLowerCase();
}

function activeHeadingPath(): string[] {
  const pos = editor.getMarkdownSelection().from;
  const stack: string[] = [];
  for (const heading of markdownHeadingsFromText(editor.view.state.doc)) {
    if (heading.pos > pos) break;
    stack[heading.level - 1] = heading.text;
    stack.length = heading.level;
  }
  return stack;
}

function anchorTagOccurrences(content = editor.getMarkdown()): string[] {
  return [
    ...equationTagsFromText(content),
    ...inlineTagAnchorsFromText(content).map((anchor) => anchor.tag),
  ].map(normalizeInlineTag).filter(Boolean);
}

function allAnchorTagSuggestions(content = editor.getMarkdown()): string[] {
  return [...new Set(anchorTagOccurrences(content))].sort((a, b) => a.localeCompare(b));
}

function nextAnchorTagSuggestion(kind: "equation" | "inline"): string {
  const headingParts = activeHeadingPath().map(tagSlugSegment).filter(Boolean);
  const fallback = tagSlugSegment(currentNote()?.title || fileNameFromPath(currentFile || "note")) || "anchor";
  const core = headingParts.slice(-3).join(".") || fallback;
  const base = kind === "equation" ? `eq:${core}` : core;
  const used = new Set(allAnchorTagSuggestions().map((tag) => tag.toLowerCase()));
  if (!used.has(base.toLowerCase())) return base;
  for (let i = 2; i < 1000; i++) {
    const candidate = `${base}.${i}`;
    if (!used.has(candidate.toLowerCase())) return candidate;
  }
  return `${base}.${Date.now()}`;
}

function noteAnchorHref(note: NoteSummary | undefined, hash: string): string {
  const cleanHash = String(hash || "").replace(/^#/, "");
  if (roamFeaturesEnabled() && note?.roam) return roamHrefForNote(note, cleanHash);
  const target = note?.path || note?.link || currentFile || note?.file || fileNameFromPath(currentFile || "note.md");
  return `${encodeMarkdownHrefPath(target)}${cleanHash ? `#${cleanHash}` : ""}`;
}

function inlineTagReferenceMarkdown(tag: string): string {
  const clean = normalizeInlineTag(tag);
  return `[${escapeMarkdownLinkText(`#${clean}`)}](${noteAnchorHref(currentNote(), encodeURIComponent(clean))})`;
}

function equationReferenceMarkdown(tag: string): string {
  const clean = normalizeInlineTag(tag);
  return `[${escapeMarkdownLinkText(clean)}](${noteAnchorHref(currentNote(), `eq-${encodeURIComponent(clean)}`)})`;
}

function inlineTagMarkdown(tag: string): string {
  return `@@tag[${normalizeInlineTag(tag)}]`;
}

function inlineTagAtCursor(): string {
  const selection = editor.getMarkdownSelection();
  const from = Math.min(selection.from, selection.to);
  const to = Math.max(selection.from, selection.to);
  return inlineTagAnchorsFromText(editor.getMarkdown())
    .find((anchor) => from === to ? from >= anchor.pos && from <= anchor.to : from < anchor.to && to > anchor.pos)
    ?.tag ?? "";
}

async function copyText(text: string): Promise<void> {
  try {
    await navigator.clipboard.writeText(text);
  } catch {
    const fallback = document.createElement("textarea");
    fallback.value = text;
    fallback.style.position = "fixed";
    fallback.style.left = "-9999px";
    document.body.appendChild(fallback);
    fallback.select();
    document.execCommand("copy");
    fallback.remove();
  }
}

function parseTagPrompt(value: string | null): string[] {
  const byKey = new Map<string, string>();
  for (const tag of String(value || "").split(/[, ]+/)) {
    const clean = tag.trim().replace(/^#/, "");
    if (!clean) continue;
    const key = clean.toLowerCase();
    const previous = byKey.get(key);
    if (!previous || clean === key) byKey.set(key, clean);
  }
  return [...byKey.values()];
}

function tagSuggestions(): string[] {
  const tags = new Map<string, string>();
  for (const note of notes) {
    if (!note.roam) continue;
    for (const tag of relationTags(note)) {
      const key = tag.toLowerCase();
      const previous = tags.get(key);
      if (!previous || tag === key) tags.set(key, tag);
    }
  }
  return [...tags.values()].sort((a, b) => a.localeCompare(b));
}

type ModalField = {
  id: string;
  label: string;
  value?: string;
  type?: "text" | "tags";
  suggestions?: string[];
};

function openFormModal(title: string, fields: ModalField[], submitLabel = "OK"): Promise<Record<string, string> | null> {
  return new Promise((resolve) => {
    modal.innerHTML = "";
    const panel = document.createElement("form");
    panel.className = fields.some((field) => field.type === "tags") ? "aaronnote-modal-panel has-tags" : "aaronnote-modal-panel";
    const heading = document.createElement("h2");
    heading.textContent = title;
    panel.appendChild(heading);
    const inputs = new Map<string, HTMLInputElement>();

    fields.forEach((field, index) => {
      const label = document.createElement("label");
      label.textContent = field.label;
      const input = document.createElement("input");
      input.name = field.id;
      input.value = field.value || "";
      input.autocomplete = "off";
      input.spellcheck = false;
      if (field.suggestions?.length) {
        const listId = `aaronnote-modal-list-${index}`;
        const list = document.createElement("datalist");
        list.id = listId;
        for (const suggestion of field.suggestions) {
          const option = document.createElement("option");
          option.value = suggestion;
          list.appendChild(option);
        }
        input.setAttribute("list", listId);
        label.append(input, list);
      } else {
        label.appendChild(input);
      }
      inputs.set(field.id, input);
      panel.appendChild(label);

      if (field.type === "tags" && field.suggestions?.length) {
        const picker = document.createElement("div");
        picker.className = "aaronnote-modal-tag-picker";
        for (const tag of field.suggestions.slice(0, 40)) {
          const button = document.createElement("button");
          button.type = "button";
          button.textContent = `#${tag}`;
          button.addEventListener("click", () => {
            const existing = parseTagPrompt(input.value);
            const lower = tag.toLowerCase();
            input.value = existing.some((item) => item.toLowerCase() === lower)
              ? existing.filter((item) => item.toLowerCase() !== lower).join(", ")
              : [...existing, tag].join(", ");
          });
          picker.appendChild(button);
        }
        panel.appendChild(picker);
      }
    });

    const actions = document.createElement("div");
    actions.className = "aaronnote-modal-actions";
    const cancel = document.createElement("button");
    cancel.type = "button";
    cancel.textContent = "Cancel";
    const submit = document.createElement("button");
    submit.type = "submit";
    submit.textContent = submitLabel;
    actions.append(cancel, submit);
    panel.appendChild(actions);

    const close = (value: Record<string, string> | null): void => {
      modal.hidden = true;
      modal.innerHTML = "";
      editor.focus();
      resolve(value);
    };
    cancel.addEventListener("click", () => close(null));
    modal.addEventListener("mousedown", (event) => {
      if (event.target === modal) close(null);
    }, { once: true });
    panel.addEventListener("keydown", (event) => {
      if (event.key !== "Escape" || event.metaKey || event.ctrlKey || event.altKey || event.isComposing) return;
      event.preventDefault();
      event.stopPropagation();
      close(null);
    });
    panel.addEventListener("submit", (event) => {
      event.preventDefault();
      const value: Record<string, string> = {};
      for (const [id, input] of inputs) value[id] = input.value;
      close(value);
    });
    modal.appendChild(panel);
    modal.hidden = false;
    window.setTimeout(() => fields[0] && inputs.get(fields[0].id)?.focus(), 0);
  });
}

async function updateNoteMeta(
  action: (body: Record<string, unknown>) => Promise<Awaited<ReturnType<typeof api.notes.bootstrap>>>,
  body: Record<string, unknown>,
  success: string,
): Promise<void> {
  if (!currentFile) {
    setStatus("No current note");
    return;
  }
  setStatus("Updating note");
  try {
    const msg = await action({
      file: currentFile,
      content: editor.getMarkdown(),
      ...body,
    });
    applyOpenedNote(msg, currentFile);
    setStatus(success);
  } catch (error) {
    setStatus(error instanceof Error ? error.message : "Update failed");
  }
}

async function quickAddMeta(): Promise<void> {
  const result = await openFormModal("Quick add meta", [
    { id: "title", label: "Title", value: currentNote()?.title || fileLabel.textContent || "Untitled" },
    { id: "tags", label: "Tags", type: "tags", value: relationTags(currentNote()).join(", "), suggestions: tagSuggestions() },
  ], "Register");
  if (!result) return;
  await updateNoteMeta(api.meta.add, { title: result.title, tags: parseTagPrompt(result.tags), kind: currentKind || "default" }, "Meta registered");
}

async function unregisterMeta(): Promise<void> {
  const result = await openFormModal("Unregister meta", [
    { id: "confirm", label: "Type REMOVE to delete roam meta", value: "" },
  ], "Remove");
  if (result?.confirm !== "REMOVE") return;
  await updateNoteMeta(api.meta.remove, {}, "Meta unregistered");
}

async function addTag(): Promise<void> {
  const result = await openFormModal("Add tag", [
    { id: "tags", label: "Tags", type: "tags", value: "", suggestions: tagSuggestions() },
  ], "Add");
  if (!result) return;
  const tags = parseTagPrompt(result.tags);
  if (tags.length === 0) return;
  await updateNoteMeta(api.meta.tag, { tags }, "Tag added");
}

async function manageNoteTags(): Promise<void> {
  const note = currentNote();
  const result = await openFormModal("Note tags", [
    { id: "tags", label: "Tags", type: "tags", value: relationTags(note).join(", "), suggestions: tagSuggestions() },
  ], "Update");
  if (!result) return;
  await updateNoteMeta(api.meta.add, {
    title: note?.title || fileLabel.textContent || "Untitled",
    tags: parseTagPrompt(result.tags),
    kind: note?.kind || currentKind || "default",
  }, "Tags updated");
}

async function insertRoamIdLink(): Promise<void> {
  const selection = editor.getMarkdownSelection();
  const selected = selection.from === selection.to ? "" : editor.textBetween(selection.from, selection.to).trim();
  const result = await openFormModal("Insert roam idlink", [
    { id: "note", label: "Roam note", value: "", suggestions: notes.filter((note) => note.roam).map(roamNoteSearchValue).sort() },
    { id: "label", label: "Link text", value: selected },
  ], "Insert");
  if (!result) return;
  const target = resolveRoamNoteSearch(notes, result.note);
  if (!target) {
    setStatus("Roam note not found");
    return;
  }
  const markdown = markdownRoamIdLink(target, result.label || selected || target.title || canonicalRoamNoteId(target));
  if (!markdown) {
    setStatus("Roam note has no id");
    return;
  }
  editor.replaceMarkdownRange(selection.from, selection.to, markdown, "end");
  setStatus("Roam idlink inserted");
  scheduleAssistUpdate({ snippets: true, toc: true });
}

function activeDisplayMathTarget(): { tex: string; replace: (nextTex: string) => void } | null {
  const state = editor.view.state;
  const cursor = state.selection.main.from;
  const range = rangeAtPosition(cursor, getBlockMathRanges(state));
  if (!range || cursor <= range.from || cursor >= range.to) return null;
  return {
    tex: range.tex,
    replace: (nextTex: string) => editor.replaceMarkdownRange(range.contentFrom, range.contentTo, nextTex, "end"),
  };
}

function existingLatexTag(tex: string): string {
  return tex.match(/\\tag\s*\{([^{}\n]+)\}/)?.[1]?.trim() || "";
}

function upsertLatexTag(tex: string, tag: string): string {
  const clean = tex.replace(/\s*\\tag\s*\{[^{}\n]*\}/g, "").replace(/\s+$/g, "");
  const separator = clean.includes("\n") ? "\n" : " ";
  return `${clean}${separator}\\tag{${tag}}`;
}

async function tagOrCopyRef(): Promise<void> {
  const math = activeDisplayMathTarget();
  if (math) {
    const existing = existingLatexTag(math.tex);
    if (existing) {
      await copyText(equationReferenceMarkdown(existing));
      setStatus(`Equation ref copied: ${existing}`);
      return;
    }
    const result = await openFormModal("Equation tag", [
      { id: "tag", label: "LaTeX tag", value: nextAnchorTagSuggestion("equation"), suggestions: allAnchorTagSuggestions() },
    ], "Tag & Copy Ref");
    if (!result?.tag) return;
    const tag = normalizeInlineTag(result.tag);
    math.replace(upsertLatexTag(math.tex, tag));
    await copyText(equationReferenceMarkdown(tag));
    setStatus(`Equation tag ${tag}; ref copied`);
    scheduleAssistUpdate({ mathPreview: true, toc: true });
    return;
  }

  const inline = inlineTagAtCursor();
  if (inline) {
    await copyText(inlineTagReferenceMarkdown(inline));
    setStatus(`Inline anchor ref copied: ${inline}`);
    return;
  }

  const result = await openFormModal("Inline anchor", [
    { id: "tag", label: "Anchor tag", value: nextAnchorTagSuggestion("inline"), suggestions: allAnchorTagSuggestions() },
  ], "Tag & Copy Ref");
  if (!result?.tag) return;
  const tag = normalizeInlineTag(result.tag);
  const selection = editor.getMarkdownSelection();
  editor.replaceMarkdownRange(selection.to, selection.to, inlineTagMarkdown(tag), "end");
  await copyText(inlineTagReferenceMarkdown(tag));
  setStatus(`Inline anchor ${tag}; ref copied`);
  scheduleAssistUpdate({ snippets: true, toc: true });
}

function changedRows(changed: unknown): Array<{ title: string; detail?: string; kind?: string }> {
  return (Array.isArray(changed) ? changed : []).slice(0, 80).map((item) => {
    const value = item as { title?: string; path?: string; file?: string; count?: number; tags?: string[] };
    return {
      title: value.title || value.path || value.file || "Untitled",
      detail: [
        value.path || value.file || "",
        typeof value.count === "number" ? `${value.count} refs` : "",
        Array.isArray(value.tags) ? value.tags.join(", ") : "",
      ].filter(Boolean).join(" - "),
      kind: typeof value.count === "number" ? "REF" : "TAG",
    };
  });
}

function showRoamToolRows(title: string, rows: Array<{ title: string; detail?: string; kind?: string }>): void {
  roamToolsTitle.textContent = title;
  const frag = document.createDocumentFragment();
  if (rows.length === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-empty";
    empty.textContent = "No issues";
    frag.appendChild(empty);
  }
  for (const row of rows) {
    const item = document.createElement("div");
    item.className = "aaronnote-roam-tool-item";
    const kind = document.createElement("span");
    kind.className = "aaronnote-roam-tool-kind";
    kind.textContent = row.kind || "ROAM";
    const body = document.createElement("div");
    body.className = "aaronnote-roam-tool-body";
    const titleEl = document.createElement("strong");
    titleEl.textContent = row.title;
    body.appendChild(titleEl);
    if (row.detail) {
      const detail = document.createElement("span");
      detail.textContent = row.detail;
      body.appendChild(detail);
    }
    item.append(kind, body);
    frag.appendChild(item);
  }
  roamToolsList.replaceChildren(frag);
  roamToolsPanel.hidden = false;
}

async function renameRoamTagTool(): Promise<void> {
  const result = await openFormModal("Rename roam tag", [
    { id: "from", label: "Current tag", type: "tags", value: "", suggestions: tagSuggestions() },
    { id: "to", label: "New tag", value: "" },
    { id: "confirm", label: "Type RENAME to update all roam notes", value: "" },
  ], "Rename");
  if (!result || result.confirm !== "RENAME") return;
  setStatus("Renaming roam tag");
  try {
    const msg = await api.roamTools.renameTag({ from: parseTagPrompt(result.from)[0] || result.from, to: result.to });
    applyIndexPayload(msg as { notes?: NoteSummary[] });
    showRoamToolRows(`Renamed ${msg.changedCount ?? 0} notes`, changedRows(msg.changed));
    setStatus(`Renamed tag in ${msg.changedCount ?? 0} notes`);
  } catch (error) {
    setStatus(error instanceof Error ? error.message : "Roam tag rename failed");
  }
}

async function deleteRoamTagTool(): Promise<void> {
  const result = await openFormModal("Delete roam tag", [
    { id: "tag", label: "Tag", type: "tags", value: "", suggestions: tagSuggestions() },
    { id: "confirm", label: "Type DELETE to remove it from all roam notes", value: "" },
  ], "Delete");
  if (!result || result.confirm !== "DELETE") return;
  setStatus("Deleting roam tag");
  try {
    const msg = await api.roamTools.deleteTag({ tag: parseTagPrompt(result.tag)[0] || result.tag });
    applyIndexPayload(msg as { notes?: NoteSummary[] });
    showRoamToolRows(`Deleted tag from ${msg.changedCount ?? 0} notes`, changedRows(msg.changed));
    setStatus(`Deleted tag from ${msg.changedCount ?? 0} notes`);
  } catch (error) {
    setStatus(error instanceof Error ? error.message : "Roam tag delete failed");
  }
}

async function tagOverlapReportTool(): Promise<void> {
  setStatus("Scanning tag overlap");
  try {
    const report = await api.roamTools.tagOverlap();
    const duplicateRows = (Array.isArray(report.duplicateCase) ? report.duplicateCase : []).map((item) => {
      const value = item as { variants?: string[] };
      return { title: `Case variants: ${(value.variants || []).join(" / ")}`, detail: "Use Rename tag to normalize these", kind: "CASE" };
    });
    const overlapRows = (Array.isArray(report.overlaps) ? report.overlaps : []).map((item) => {
      const value = item as { a?: string; b?: string; aCount?: number; bCount?: number; sharedCount?: number; containment?: number };
      return {
        title: `${value.a || ""} overlaps ${value.b || ""}`,
        detail: `${value.sharedCount ?? 0} shared - ${value.aCount ?? 0}/${value.bCount ?? 0} notes - ${Math.round((value.containment ?? 0) * 100)}% containment`,
        kind: "TAG",
      };
    });
    showRoamToolRows(`Tag overlap (${report.tagCount ?? 0} tags)`, [...duplicateRows, ...overlapRows]);
    setStatus("Tag overlap scanned");
  } catch (error) {
    setStatus(error instanceof Error ? error.message : "Tag overlap scan failed");
  }
}

async function rewritePathRefsTool(): Promise<void> {
  const result = await openFormModal("Rewrite path references", [
    { id: "oldPath", label: "Old target path", value: "", suggestions: pathSuggestions },
    { id: "newPath", label: "New target path", value: "", suggestions: pathSuggestions },
    { id: "confirm", label: "Type UPDATE to rewrite Markdown path links", value: "" },
  ], "Update");
  if (!result || result.confirm !== "UPDATE") return;
  setStatus("Rewriting path references");
  try {
    const msg = await api.roamTools.rewritePathRefs({ oldPath: result.oldPath, newPath: result.newPath });
    applyIndexPayload(msg as { notes?: NoteSummary[] });
    showRoamToolRows(`Rewrote ${msg.referenceCount ?? 0} references`, changedRows(msg.changed));
    setStatus(`Rewrote ${msg.referenceCount ?? 0} references`);
  } catch (error) {
    setStatus(error instanceof Error ? error.message : "Path reference rewrite failed");
  }
}

type ToolAction = {
  id: string;
  title: string;
  detail: string;
  run: () => void;
};

function toolActions(): ToolAction[] {
  const common: ToolAction[] = [
    { id: "toc", title: "Toggle TOC", detail: "Page headings, anchors, tags, backlinks", run: () => { floatingTocPanel.toggle(); updateFloatingToc(); } },
    { id: "tag-ref", title: "Tag / copy ref", detail: "Equation tag, inline anchor, reference copy", run: () => void tagOrCopyRef() },
    { id: "reload-snippets", title: "Reload snippets", detail: "Refresh Emacs md/tex snippets", run: () => void reloadSnippets() },
  ];
  return [
    ...common,
    { id: "reload-index", title: "Reload roam index", detail: "Refresh notes, tags, links", run: () => void reloadNotes(true) },
    { id: "add-meta", title: "Add meta", detail: "Register title/kind/tags", run: () => void quickAddMeta() },
    { id: "remove-meta", title: "Remove meta", detail: "Delete current note meta block", run: () => void unregisterMeta() },
    { id: "hide-roam", title: "Set roam off", detail: "Keep meta but hide from roam graph", run: () => void updateNoteMeta(api.meta.hideRoam, {}, "roam: off set") },
    { id: "activate-roam", title: "Clear roam off", detail: "Activate current note in roam graph", run: () => void updateNoteMeta(api.meta.activateRoam, {}, "roam: off cleared") },
    { id: "add-tag", title: "Add tag", detail: "Append tags to current note", run: () => void addTag() },
    { id: "manage-tags", title: "Manage note tags", detail: "Replace current note tag list", run: () => void manageNoteTags() },
    { id: "insert-roam-idlink", title: "Insert roam idlink", detail: "Search roam note and insert id link", run: () => void insertRoamIdLink() },
    { id: "rename-tag", title: "Rename roam tag", detail: "Bulk rename tag in roam notes", run: () => void renameRoamTagTool() },
    { id: "delete-tag", title: "Delete roam tag", detail: "Bulk remove tag in roam notes", run: () => void deleteRoamTagTool() },
    { id: "tag-overlap", title: "Tag overlap report", detail: "Find duplicate/overlapping tags", run: () => void tagOverlapReportTool() },
    { id: "rewrite-paths", title: "Rewrite path refs", detail: "Bulk rewrite Markdown path links", run: () => void rewritePathRefsTool() },
  ];
}

function renderToolsPanel(): void {
  toolsList.replaceChildren();
  for (const action of toolActions()) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "aaronnote-tool-action";
    button.dataset.action = action.id;
    const title = document.createElement("strong");
    title.textContent = action.title;
    const detail = document.createElement("span");
    detail.textContent = action.detail;
    button.append(title, detail);
    button.addEventListener("click", () => {
      toolsPanel.hidden = true;
      toolsButton.setAttribute("aria-expanded", "false");
      action.run();
    });
    toolsList.appendChild(button);
  }
}

function toggleToolsPanel(): void {
  if (toolsPanel.hidden) renderToolsPanel();
  toolsPanel.hidden = !toolsPanel.hidden;
  toolsButton.setAttribute("aria-expanded", toolsPanel.hidden ? "false" : "true");
}

function closeToolsPanel(): void {
  toolsPanel.hidden = true;
  toolsButton.setAttribute("aria-expanded", "false");
}

function closeRoamToolsPanel(): void {
  roamToolsPanel.hidden = true;
}

function editorSurfaceVisible(): boolean {
  return !host.hidden && document.body.contains(host);
}

function editorOwnsActiveSurface(): boolean {
  const active = document.activeElement;
  if (!active || !host.contains(active)) return false;
  const editable = active.closest<HTMLElement>("input, textarea, select, [contenteditable='true']");
  return !editable || editable.classList.contains("cm-content");
}

function clearMathPreviewErrorTimer(): void {
  window.clearTimeout(mathPreviewErrorTimer);
  mathPreviewErrorTimer = 0;
}

function hideMathPreview(): void {
  clearMathPreviewErrorTimer();
  mathPreview.hidden = true;
  mathPreview.innerHTML = "";
  mathPreview.classList.remove("is-display", "is-error", "is-overflowing");
  mathPreview.style.left = "";
  mathPreview.style.top = "";
  mathPreview.style.width = "";
  mathPreviewKey = "";
  mathPreviewPendingErrorKey = "";
  mathPreviewWidth = 0;
}

function hideSnippetPopup(): void {
  snippetPopup.hidden = true;
  snippetPopupItems = [];
  snippetPopupIndex = 0;
  snippetDeleteBefore = 0;
  snippetRenderKey = "";
  snippetPopupMatchKey = "";
}

function placeFloating(el: HTMLElement, rect: { left: number; top: number; bottom: number } | null, width = 340): void {
  if (!rect) {
    el.hidden = true;
    return;
  }
  const margin = 8;
  const resolvedWidth = Math.min(width, Math.max(220, window.innerWidth - margin * 2));
  const left = Math.min(
    Math.max(margin, rect.left),
    Math.max(margin, window.innerWidth - resolvedWidth - margin),
  );
  const height = Math.min(el.offsetHeight || 180, Math.max(160, window.innerHeight - margin * 2));
  let top = rect.bottom + 8;
  if (top + height > window.innerHeight - margin) top = rect.top - height - 8;
  if (top < margin) top = Math.max(margin, window.innerHeight - height - margin);
  el.style.left = `${left}px`;
  el.style.top = `${top}px`;
  el.style.width = `${resolvedWidth}px`;
}

function placeFloatingAbove(
  el: HTMLElement,
  rect: { left: number; top: number; bottom: number } | null,
  width = 320,
  bottomRect?: { bottom: number } | null,
): void {
  if (!rect) {
    el.hidden = true;
    return;
  }
  const margin = 8;
  const resolvedWidth = Math.min(width, Math.max(220, window.innerWidth - margin * 2));
  const left = Math.min(
    Math.max(margin, rect.left),
    Math.max(margin, window.innerWidth - resolvedWidth - margin),
  );
  const height = Math.min(el.offsetHeight || 180, Math.max(160, window.innerHeight - margin * 2));
  let top = rect.top - height - 8;
  if (top < margin) top = (bottomRect ?? rect).bottom + 8;
  if (top + height > window.innerHeight - margin) top = Math.max(margin, window.innerHeight - height - margin);
  el.style.left = `${left}px`;
  el.style.top = `${top}px`;
  el.style.width = `${resolvedWidth}px`;
}

function currentSnippetKind(): string {
  return currentKind.trim().toLowerCase();
}

function matchingSnippets(prefix: string, mode: string): SnippetSummary[] {
  return matchingSnippetsForPrefix(snippets, prefix, { kind: currentSnippetKind(), mode, limit: 10 });
}

function insertSnippet(snippet: SnippetSummary, deleteBefore = 0): boolean {
  if (!snippetSession.insert(snippet, deleteBefore)) return false;
  setStatus(`Inserted ${snippet.key || snippet.name || "snippet"}`);
  scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
  return true;
}

function jumpSnippetTabstop(): boolean {
  const moved = snippetSession.next();
  if (moved) {
    setStatus("Snippet field");
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
  }
  return moved;
}

function jumpSnippetTabstopBack(): boolean {
  const moved = snippetSession.previous();
  if (moved) {
    setStatus("Snippet field");
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
  }
  return moved;
}

function snippetPrefix(before: string): string {
  return before.match(/([A-Za-z0-9_:/;.+\\-]{1,40})$/)?.[1] ?? "";
}

function markdownEscapedAt(text: string, index: number): boolean {
  let slashCount = 0;
  for (let pos = index - 1; pos >= 0 && text[pos] === "\\"; pos--) slashCount++;
  return slashCount % 2 === 1;
}

function markdownLinkLabelEnd(line: string, openBracket: number): number {
  for (let pos = openBracket + 1; pos < line.length; pos++) {
    if (line[pos] === "]" && !markdownEscapedAt(line, pos)) return pos;
  }
  return -1;
}

function markdownLinkTargetEnd(line: string, openParen: number): number {
  let depth = 0;
  let quote = "";
  for (let pos = openParen + 1; pos < line.length; pos++) {
    const ch = line[pos] || "";
    if (markdownEscapedAt(line, pos)) continue;
    if (quote) {
      if (ch === quote) quote = "";
      continue;
    }
    if ((ch === "\"" || ch === "'") && /\s/.test(line[pos - 1] ?? "")) {
      quote = ch;
      continue;
    }
    if (ch === "(") {
      depth++;
      continue;
    }
    if (ch === ")") {
      if (depth === 0) return pos;
      depth--;
    }
  }
  return -1;
}

function markdownLinkTargetBounds(rawTarget: string): { href: string; start: number; end: number } | null {
  const leading = rawTarget.match(/^\s*/)?.[0].length ?? 0;
  if (rawTarget[leading] === "<") {
    for (let index = leading + 1; index < rawTarget.length; index++) {
      if (rawTarget[index] === ">" && !markdownEscapedAt(rawTarget, index)) {
        const href = rawTarget.slice(leading + 1, index).trim();
        return href ? { href, start: leading + 1, end: index } : null;
      }
    }
  }
  const titleMatch = rawTarget.match(/\s+(?:"[^"]*"|'[^']*')\s*$/);
  const beforeTitleEnd = titleMatch?.index ?? rawTarget.length;
  const start = leading;
  const end = rawTarget.slice(0, beforeTitleEnd).replace(/\s+$/, "").length;
  if (start >= end) return null;
  return { href: rawTarget.slice(start, end), start, end };
}

function markdownInlineLinkTargetAtCursor(): {
  href: string;
  prefix: string;
  deleteBefore: number;
} | null {
  const selection = editor.getMarkdownSelection();
  const pos = Math.max(0, Math.min(selection.from, editor.view.state.doc.length));
  const line = editor.view.state.doc.lineAt(pos);
  const localPos = pos - line.from;
  for (let start = 0; start < line.text.length; start++) {
    if (line.text[start] !== "[" || markdownEscapedAt(line.text, start)) continue;
    if (start > 0 && line.text[start - 1] === "!" && !markdownEscapedAt(line.text, start - 1)) continue;
    const labelEnd = markdownLinkLabelEnd(line.text, start);
    if (labelEnd < 0 || line.text[labelEnd + 1] !== "(") continue;
    const targetOpen = labelEnd + 1;
    const targetEnd = markdownLinkTargetEnd(line.text, targetOpen);
    if (targetEnd < 0) continue;
    if (localPos < targetOpen + 1 || localPos > targetEnd + 1) {
      start = targetEnd;
      continue;
    }
    const rawTarget = line.text.slice(targetOpen + 1, targetEnd);
    const bounds = markdownLinkTargetBounds(rawTarget);
    if (!bounds) return null;
    const targetLocal = Math.max(bounds.start, Math.min(localPos - (targetOpen + 1), bounds.end));
    return {
      href: bounds.href,
      prefix: rawTarget.slice(bounds.start, targetLocal),
      deleteBefore: targetLocal - bounds.start,
    };
  }
  return null;
}

function completionDetail(snippet: SnippetSummary): string {
  const group = String(snippet.group || "");
  if (group === "path") return snippet.source || "";
  if (group === "wikilink") return snippet.source ? `[[${snippet.source}]]` : "wikilink";
  if (group === "roam") return snippet.body ? `roam -> ${snippet.body}` : "roam";
  if (group === "tag") return snippet.source ? `inline tag in ${snippet.source}` : "inline tag";
  if (group === "dom") return snippet.source ? `DOM target in ${snippet.source}` : "DOM target";
  return snippetDetail(snippet);
}

function completionPreviewText(snippet: SnippetSummary): string {
  const group = String(snippet.group || "");
  if (group === "path") return "";
  if (group === "wikilink" || group === "roam" || group === "tag" || group === "dom") {
    return String(snippet.source || snippet.body || "").replace(/\s+/g, " ").trim().slice(0, 96);
  }
  return String(snippet.body || "").replace(/\s+/g, " ").trim().slice(0, 96);
}

function pathCompletionPrefix(before: string): string {
  const match = before.match(/(?:^|[\s([{"'=])([^\s\])}"'`<>#@]*\/[^\s\])}"'`<>#@]*)$/);
  const prefix = match?.[1] ?? "";
  if (!prefix || prefix.startsWith("//") || hrefProtocol(prefix)) return "";
  return prefix;
}

function roamCompletionPrefix(before: string): string | null {
  const match = before.match(/(?:^|[\s([{"'=])roam:\/\/([^\s\])}"'`<>]*)$/i);
  return match ? match[1] ?? "" : null;
}

function wikilinkCompletionPrefix(before: string): string | null {
  const match = before.match(/(?:^|[\s([{"'=])\[\[([^\]\n]*)$/);
  return match ? match[1] ?? "" : null;
}

function inlineTagCompletionPrefix(before: string): string | null {
  const match = before.match(/@@tag\[([^\]\n]*)$/);
  return match ? match[1] ?? "" : null;
}

function displayPathCompletion(path: string, prefix: string): string {
  if (prefix.startsWith("./") && !path.startsWith("./") && !path.startsWith("../") && !path.startsWith("/")) return `./${path}`;
  return path;
}

function isPureTraversalPath(path: string): boolean {
  const parts = path.replace(/\\/g, "/").split("/").map((part) => part.trim()).filter(Boolean);
  return parts.length > 0 && parts.every((part) => part === "." || part === "..");
}

function pathCompletionRank(path: string, prefix: string): number {
  const display = displayPathCompletion(path, prefix);
  const sameDir = display.startsWith("./") ? 0 : 100;
  const parentPenalty = (display.match(/\.\.\//g) ?? []).length * 25;
  const dirPenalty = display.split("/").length;
  const directoryBoost = display.endsWith("/") ? -2 : 0;
  const exactPrefixBoost = display.toLowerCase().startsWith(prefix.toLowerCase()) ? -4 : 0;
  return sameDir + parentPenalty + dirPenalty + directoryBoost + exactPrefixBoost;
}

function noteFromCompletionRef(ref: string): NoteSummary | undefined {
  return resolveHrefNote(ref) || resolveNoteRef(ref);
}

function tagCompletionContext(before: string): { note: NoteSummary; tagPrefix: string } | null {
  const roamMatch = before.match(/(?:^|[\s([{"'=])roam:\/\/([^\s\])}"'`<>#]*)#([^\s\])}"'`<>]*)$/i);
  if (roamMatch) {
    const note = noteFromCompletionRef(roamMatch[1] ?? "");
    if (note) return { note, tagPrefix: roamMatch[2] ?? "" };
  }
  const pathMatch = before.match(/(?:^|[\s([{"'=])((?:\.{1,2}\/|\.|[^\s\])}"'`<>#@]+)[^\s\])}"'`<>#@]*)#([^\s\])}"'`<>]*)$/);
  if (pathMatch) {
    const note = noteFromCompletionRef(pathMatch[1] ?? "");
    if (note) return { note, tagPrefix: pathMatch[2] ?? "" };
  }
  return null;
}

function domCompletionParts(rawHref: string): { ref: string; parentSegments: string[]; domPrefix: string } | null {
  const clean = cleanHref(rawHref);
  if (!clean || clean.includes("#")) return null;
  const roamTarget = splitRoamLikeHref(clean);
  if (roamTarget?.dom) {
    const endsAtSeparator = /@$/.test(clean);
    const segments = domTargetPathSegments(roamTarget.dom);
    return {
      ref: roamTarget.ref,
      parentSegments: endsAtSeparator ? segments : segments.slice(0, -1),
      domPrefix: endsAtSeparator ? "" : segments[segments.length - 1] || "",
    };
  }
  const fileDomMatch = clean.match(/^(.+?\.(?:md|markdown|typ))@(.+)$/i);
  const plainDomMatch = fileDomMatch ? null : clean.match(/^(.+?)@([^@]*)$/);
  const match = fileDomMatch || plainDomMatch;
  if (!match) return null;
  const endsAtSeparator = /@$/.test(clean);
  const segments = domTargetPathSegments(match[2] || "");
  return {
    ref: match[1] || "",
    parentSegments: endsAtSeparator ? segments : segments.slice(0, -1),
    domPrefix: endsAtSeparator ? "" : segments[segments.length - 1] || "",
  };
}

function domCompletionContext(before: string): { note: NoteSummary; domPrefix: string; parentSegments: string[] } | null {
  const match = before.match(/(?:^|[\s([{"'=])((?:roam:\/\/|\.{1,2}\/|\.|[^\s()[\]{}"'`<>#]+)[^\s()[\]{}"'`<>#]*)$/i);
  const parts = match ? domCompletionParts(match[1] ?? "") : null;
  if (!parts) return null;
  const note = noteFromCompletionRef(parts.ref);
  if (!note) return null;
  return { note, domPrefix: parts.domPrefix, parentSegments: parts.parentSegments };
}

async function matchingTagCompletions(note: NoteSummary, prefix: string): Promise<SnippetSummary[]> {
  const query = prefix.toLowerCase().replace(/^tag-/, "");
  let tags: string[];
  if (note.file === currentFile) {
    tags = [...new Set(allAnchorTagSuggestions().map((tag) => normalizeInlineTag(tag).replace(/^#/, "")).filter(Boolean))].sort();
  } else {
    // For roam://noteid# and ./path.md# anchor completion: show only the inline
    // tags defined in the target note (not global roam tags from all notes).
    tags = [...(note.inlineTags ?? [])].map((t) => normalizeInlineTag(t).replace(/^#/, "")).filter(Boolean);
  }
  return [...new Set(tags)]
    .filter((tag) => !query || tag.toLowerCase().includes(query))
    .slice(0, 12)
    .map((tag) => ({
      key: tag,
      name: `#${tag}`,
      mode: "markdown-mode",
      group: "tag",
      body: encodeURIComponent(tag),
      source: note.path || note.file || canonicalRoamNoteId(note),
    }));
}

function indexedDomTargets(note: NoteSummary): DomTargetEntry[] {
  const indexed = (note.domTargets ?? []).map((target) => {
    const label = normalizeDomTarget(target.label || target.slug || "");
    const slug = slugDomTarget(target.slug || label);
    const path = (Array.isArray(target.path) && target.path.length > 0 ? target.path : [slug])
      .map((segment) => slugDomTarget(segment))
      .filter(Boolean);
    const labelPath = (Array.isArray(target.labelPath) && target.labelPath.length > 0 ? target.labelPath : [label])
      .map(normalizeDomTarget)
      .filter(Boolean);
    return { label, slug, path, labelPath, level: Math.max(1, Number(target.level || 1)), notePath: target.notePath || note.path || "" };
  }).filter((target) => target.label && target.slug && target.path.length > 0);
  return indexed;
}

function domTargetsForCompletion(note: NoteSummary): DomTargetEntry[] {
  if (note.file === currentFile) return currentDomTargets();
  return indexedDomTargets(note);
}

function immediateDomCompletionTargets(entries: readonly DomTargetEntry[], parentSegments: readonly string[]): DomTargetEntry[] {
  const parentPath = parentSegments.map(slugDomTarget).filter(Boolean);
  const parentLength = parentPath.length;
  return entries.filter((entry) => {
    if (entry.path.length !== parentLength + 1) return false;
    if (parentLength === 0) return true;
    return targetPathMatches(entry.path.slice(0, parentLength), parentPath, false);
  });
}

function descendantDomCompletionTargets(entries: readonly DomTargetEntry[], parentSegments: readonly string[]): DomTargetEntry[] {
  const parentPath = parentSegments.map(slugDomTarget).filter(Boolean);
  const parentLength = parentPath.length;
  return entries.filter((entry) => {
    if (entry.path.length <= parentLength) return false;
    if (parentLength === 0) return true;
    return targetPathMatches(entry.path.slice(0, parentLength), parentPath, false);
  });
}

function matchingDomCompletions(note: NoteSummary, prefix: string, parentSegments: readonly string[] = []): SnippetSummary[] {
  const query = normalizeDomTarget(prefix).toLowerCase();
  const entries = domTargetsForCompletion(note);
  const candidates = query
    ? descendantDomCompletionTargets(entries, parentSegments)
      .filter((target) => target.slug.includes(query) || target.label.toLowerCase().includes(query))
    : immediateDomCompletionTargets(entries, parentSegments);
  return candidates.slice(0, 12).map((target) => ({
    key: target.slug,
    name: `@${target.slug}`,
    mode: "markdown-mode",
    group: "dom",
    body: encodeURIComponent(target.slug),
    source: domTargetPathLabel(target.labelPath) || note.path || note.file || canonicalRoamNoteId(note) || target.label,
  }));
}

async function matchingRoamCompletions(prefix: string): Promise<SnippetSummary[]> {
  if (!roamFeaturesEnabled()) return [];
  const needle = prefix.trim().toLowerCase();
  try {
    const result = await api.completions.roam(needle);
    return (result.notes ?? []).map((note) => ({
      key: note.title || note.id,
      name: note.title || note.id,
      body: `${encodeURIComponent(note.id || note.key)}`,
      mode: "markdown-mode",
      group: "roam",
      source: note.path || note.id,
    }));
  } catch {
    return [];
  }
}

async function matchingWikilinkCompletions(prefix: string): Promise<SnippetSummary[]> {
  const needle = prefix.trim().toLowerCase();
  try {
    const result = await api.completions.roam(needle);
    return (result.notes ?? []).map((note) => {
      const label = String(note.title || note.path || note.id || "Untitled")
        .replace(/[\r\n\]]+/g, " ")
        .replace(/\s+/g, " ")
        .trim() || "Untitled";
      return {
        key: label,
        name: label,
        body: `${label}]]`,
        mode: "markdown-mode",
        group: "wikilink",
        source: note.path || "",
      };
    });
  } catch {
    return [];
  }
}

async function matchingInlineTagCompletions(prefix: string): Promise<SnippetSummary[]> {
  const needle = normalizeInlineTag(prefix).toLowerCase();
  const localTags = allAnchorTagSuggestions().map(normalizeInlineTag).filter(Boolean);
  let backendTags: string[] = [];
  try {
    const result = await api.completions.tags(needle);
    backendTags = result.tags ?? [];
  } catch {
    // fall back to local tags only
  }
  const tags = new Map<string, string>();
  for (const tag of [...localTags, ...backendTags]) {
    const clean = normalizeInlineTag(tag).replace(/^#/, "");
    if (!clean) continue;
    const key = clean.toLowerCase();
    if (!tags.has(key)) tags.set(key, clean);
  }
  return [...tags.values()]
    .filter((tag) => !needle || tag.toLowerCase().includes(needle))
    .sort((a, b) => a.localeCompare(b))
    .slice(0, 12)
    .map((tag) => ({
      key: tag,
      name: tag,
      body: `${tag}]`,
      mode: "markdown-mode",
      group: "tag",
      source: tag,
    }));
}

async function matchingPathCompletions(prefix: string): Promise<SnippetSummary[]> {
  if (!prefix || !currentFile) return [];
  try {
    const result = await api.notes.pathSuggestions(currentFile, prefix);
    const paths = result.paths ?? [];
    return paths
      .filter((path) => !isPureTraversalPath(displayPathCompletion(path, prefix)))
      .sort((a, b) => {
        const rank = pathCompletionRank(a, prefix) - pathCompletionRank(b, prefix);
        return rank || displayPathCompletion(a, prefix).localeCompare(displayPathCompletion(b, prefix));
      })
      .slice(0, 8)
      .map((path) => {
        const displayPath = displayPathCompletion(path, prefix);
        const note = resolveHrefNote(displayPath);
        const roamId = roamFeaturesEnabled() && note?.roam ? canonicalRoamNoteId(note) : "";
        return {
          key: displayPath,
          name: displayPath,
          mode: "markdown-mode",
          group: "path",
          body: roamId ? roamHrefForNote(note) : displayPath,
          source: note?.title && note.title !== displayPath ? note.title : "",
        };
      });
  } catch {
    return [];
  }
}


function renderSnippetPopup(prefix: string, rect: { left: number; top: number; bottom: number } | null): void {
  const nextKey = `${prefix}\n${snippetPopupIndex}\n${snippetPopupItems.map((snippet) => `${snippet.mode}:${snippet.key}:${snippet.name}`).join("\n")}`;
  if (!snippetPopup.hidden && snippetRenderKey === nextKey) {
    placeFloating(snippetPopup, rect);
    snippetPopup.querySelector(".aaronnote-snippet-option.is-active")?.scrollIntoView({ block: "nearest" });
    return;
  }
  snippetRenderKey = nextKey;
  snippetPopup.innerHTML = "";
  snippetPopupItems.forEach((snippet, index) => {
    const button = document.createElement("button");
    button.type = "button";
    button.id = `aaronnote-snippet-option-${index}`;
    button.className = index === snippetPopupIndex
      ? "aaronnote-snippet-option is-active"
      : "aaronnote-snippet-option";
    button.setAttribute("role", "option");
    button.setAttribute("aria-selected", index === snippetPopupIndex ? "true" : "false");

    const number = document.createElement("span");
    number.className = "aaronnote-snippet-option-number";
    number.textContent = index < 9 ? String(index + 1) : index === 9 ? "0" : "";

    const key = document.createElement("span");
    key.className = "aaronnote-snippet-option-key";
    key.textContent = snippetLabel(snippet);

    const detail = document.createElement("span");
    detail.className = "aaronnote-snippet-option-detail";
    detail.textContent = completionDetail(snippet);

    button.append(number, key, detail);
    const previewText = completionPreviewText(snippet);
    if (previewText) {
      const preview = document.createElement("span");
      preview.className = "aaronnote-snippet-option-preview";
      preview.textContent = previewText;
      button.appendChild(preview);
    }
    button.addEventListener("mousedown", (event) => {
      event.preventDefault();
      snippetPopupIndex = index;
      chooseSnippetPopupItem();
    });
    button.addEventListener("mouseenter", () => {
      if (snippetPopupIndex === index) return;
      snippetPopupIndex = index;
      updateSnippetPopupActiveOption();
    });
    snippetPopup.appendChild(button);
  });
  snippetPopup.dataset.prefix = prefix;
  snippetPopup.setAttribute("aria-activedescendant", `aaronnote-snippet-option-${snippetPopupIndex}`);
  snippetPopup.hidden = false;
  placeFloating(snippetPopup, rect);
  snippetPopup.querySelector(".aaronnote-snippet-option.is-active")?.scrollIntoView({ block: "nearest" });
}

function updateSnippetPopupActiveOption(): void {
  snippetPopup.querySelectorAll<HTMLButtonElement>(".aaronnote-snippet-option").forEach((button, index) => {
    const active = index === snippetPopupIndex;
    button.classList.toggle("is-active", active);
    button.setAttribute("aria-selected", active ? "true" : "false");
  });
  snippetPopup.setAttribute("aria-activedescendant", `aaronnote-snippet-option-${snippetPopupIndex}`);
  snippetPopup.querySelector(".aaronnote-snippet-option.is-active")?.scrollIntoView({ block: "nearest" });
}

function showSnippetPopup(prefix: string, items: SnippetSummary[], deleteBefore: number, rect: { left: number; top: number; bottom: number } | null): void {
  const matchKey = `${prefix}\n${items.map((snippet) => `${snippet.kind}:${snippet.mode}:${snippet.group}:${snippet.key}:${snippet.name}`).join("\n")}`;
  snippetDeleteBefore = deleteBefore;
  if (matchKey !== snippetPopupMatchKey) {
    snippetPopupIndex = 0;
    snippetRenderKey = "";
  } else {
    snippetPopupIndex = Math.min(snippetPopupIndex, items.length - 1);
  }
  snippetPopupMatchKey = matchKey;
  snippetPopupItems = items;
  renderSnippetPopup(prefix, rect);
}

function mathAtCursor(ctx: ReturnType<typeof editor.cursorContext>): {
  tex: string;
  display: boolean;
  rect: { left: number; top: number; bottom: number } | null;
  rectEnd?: { left: number; top: number; bottom: number } | null;
} | null {
  const state = editor.view.state;
  const cursor = state.selection.main.from;
  const contextStart = Math.max(0, cursor - ctx.before.length);
  const rectAtSourceOffset = (offset: number) => ctx.rectAtOffset(offset - contextStart);
  const blockRanges = getBlockMathRanges(state);
  const displayMath = rangeAtPosition(cursor, blockRanges);
  if (displayMath && cursor > displayMath.from && cursor < displayMath.to) {
    return {
      tex: displayMath.tex,
      display: true,
      rect: rectAtSourceOffset(displayMath.from),
      rectEnd: rectAtSourceOffset(displayMath.to),
    };
  }

  const line = state.doc.lineAt(cursor);
  INLINE_MATH_RE.lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = INLINE_MATH_RE.exec(line.text)) !== null) {
    const from = line.from + match.index;
    const to = from + match[0].length;
    const tex = match[1] || "";
    if (cursor <= from || cursor >= to) continue;
    if (rangeOverlapsAny(from, to, blockRanges)) continue;
    if (!isLikelyInlineMath(tex)) continue;
    return { tex, display: false, rect: rectAtSourceOffset(from) };
  }
  return null;
}

function snippetContextMode(ctx: ReturnType<typeof editor.cursorContext>): string {
  return mathAtCursor(ctx) ? "tex-mode" : "markdown-mode";
}

function clearCompletionCache(): void {
  completionEpoch.cancel();
  completionTimer.cancel();
  completionContextKey = "";
  completionPendingItems = null;
}

function scheduleAsyncCompletion(
  contextKey: string,
  renderPrefix: string,
  deleteBefore: number,
  rect: { left: number; top: number; bottom: number } | null,
  fetchFn: () => Promise<SnippetSummary[]>,
): void {
  if (renderPrefix === snippetSuppressedPrefix) {
    hideSnippetPopup();
    clearCompletionCache();
    return;
  }
  // Same context: show cached result immediately, no new request needed.
  if (contextKey === completionContextKey && completionPendingItems !== null) {
    if (completionPendingItems.length > 0) {
      showSnippetPopup(renderPrefix, completionPendingItems, deleteBefore, rect);
    } else {
      hideSnippetPopup();
    }
    return;
  }
  // New context: start a fresh epoch; keep old popup visible while request is in flight.
  completionContextKey = contextKey;
  completionPendingItems = null;
  const run = completionEpoch.begin();
  completionTimer.schedule(() => {
    void fetchFn().then((items) => {
      if (!run.current) return;
      completionPendingItems = items;
      if (items.length > 0) {
        showSnippetPopup(renderPrefix, items, deleteBefore, rect);
      } else {
        hideSnippetPopup();
      }
    }).catch(() => {
      if (!run.current) return;
      hideSnippetPopup();
    });
  });
}

function updateSnippetPopup(ctx: ReturnType<typeof editor.cursorContext>): void {
  if (!editorOwnsActiveSurface()) {
    hideSnippetPopup();
    clearCompletionCache();
    return;
  }

  // Link target completion ([...](here) or inline href position)
  const linkTarget = markdownInlineLinkTargetAtCursor();
  if (linkTarget) {
    const targetPrefix = cleanHref(linkTarget.prefix);
    const target = cleanHref(linkTarget.href);

    const hashIndex = targetPrefix.lastIndexOf("#");
    if (hashIndex >= 0) {
      const ref = targetPrefix.slice(0, hashIndex);
      const note = noteFromCompletionRef(ref || target);
      if (!note) { hideSnippetPopup(); clearCompletionCache(); return; }
      const tagPrefix = targetPrefix.slice(hashIndex + 1);
      const renderPrefix = `#${tagPrefix}`;
      scheduleAsyncCompletion(
        `link-tag:${note.file}:${tagPrefix}`,
        renderPrefix,
        tagPrefix.length,
        ctx.rect,
        () => matchingTagCompletions(note, tagPrefix),
      );
      return;
    }

    const domParts = domCompletionParts(targetPrefix);
    if (domParts) {
      const note = noteFromCompletionRef(domParts.ref);
      if (!note) { hideSnippetPopup(); clearCompletionCache(); return; }
      const renderPrefix = `@${domParts.domPrefix}`;
      if (renderPrefix === snippetSuppressedPrefix) { hideSnippetPopup(); clearCompletionCache(); return; }
      const matches = matchingDomCompletions(note, domParts.domPrefix, domParts.parentSegments);
      if (matches.length === 0) { hideSnippetPopup(); clearCompletionCache(); return; }
      clearCompletionCache();
      showSnippetPopup(renderPrefix, matches, domParts.domPrefix.length, ctx.rect);
      return;
    }

    const roamLinkPrefix = targetPrefix.match(/^roam:\/\/(.*)$/i)?.[1];
    if (roamLinkPrefix != null) {
      if (!roamFeaturesEnabled()) { hideSnippetPopup(); clearCompletionCache(); return; }
      const renderPrefix = `roam://${roamLinkPrefix}`;
      scheduleAsyncCompletion(
        `link-roam:${roamLinkPrefix}`,
        renderPrefix,
        roamLinkPrefix.length,
        ctx.rect,
        () => matchingRoamCompletions(roamLinkPrefix),
      );
      return;
    }

    if (pathCompletionPrefix(` ${targetPrefix}`) === targetPrefix) {
      scheduleAsyncCompletion(
        `link-path:${currentFile}:${targetPrefix}`,
        targetPrefix,
        targetPrefix.length,
        ctx.rect,
        () => matchingPathCompletions(targetPrefix),
      );
      return;
    }

    hideSnippetPopup();
    clearCompletionCache();
    return;
  }

  const domContext = domCompletionContext(ctx.before);
  if (domContext) {
    const renderPrefix = `@${domContext.domPrefix}`;
    if (renderPrefix === snippetSuppressedPrefix) { hideSnippetPopup(); clearCompletionCache(); return; }
    const matches = matchingDomCompletions(domContext.note, domContext.domPrefix, domContext.parentSegments);
    if (matches.length === 0) { hideSnippetPopup(); clearCompletionCache(); return; }
    clearCompletionCache();
    showSnippetPopup(renderPrefix, matches, domContext.domPrefix.length, ctx.rect);
    return;
  }

  const tagContext = tagCompletionContext(ctx.before);
  if (tagContext) {
    const renderPrefix = `#${tagContext.tagPrefix}`;
    scheduleAsyncCompletion(
      `tag:${tagContext.note.file}:${tagContext.tagPrefix}`,
      renderPrefix,
      tagContext.tagPrefix.length,
      ctx.rect,
      () => matchingTagCompletions(tagContext.note, tagContext.tagPrefix),
    );
    return;
  }

  const inlineTagPrefix = inlineTagCompletionPrefix(ctx.before);
  if (inlineTagPrefix !== null) {
    const renderPrefix = `@@tag[${inlineTagPrefix}`;
    scheduleAsyncCompletion(
      `inline-tag:${inlineTagPrefix}`,
      renderPrefix,
      inlineTagPrefix.length,
      ctx.rect,
      () => matchingInlineTagCompletions(inlineTagPrefix),
    );
    return;
  }

  const wikilinkPrefix = wikilinkCompletionPrefix(ctx.before);
  if (wikilinkPrefix !== null) {
    const renderPrefix = `[[${wikilinkPrefix}`;
    scheduleAsyncCompletion(
      `wikilink:${wikilinkPrefix}`,
      renderPrefix,
      wikilinkPrefix.length,
      ctx.rect,
      () => matchingWikilinkCompletions(wikilinkPrefix),
    );
    return;
  }

  const roamPrefix = roamCompletionPrefix(ctx.before);
  if (roamPrefix !== null) {
    if (!roamFeaturesEnabled()) { hideSnippetPopup(); clearCompletionCache(); return; }
    const renderPrefix = `roam://${roamPrefix}`;
    scheduleAsyncCompletion(
      `roam:${roamPrefix}`,
      renderPrefix,
      roamPrefix.length,
      ctx.rect,
      () => matchingRoamCompletions(roamPrefix),
    );
    return;
  }

  const pathPrefix = pathCompletionPrefix(ctx.before);
  if (pathPrefix) {
    scheduleAsyncCompletion(
      `path:${currentFile}:${pathPrefix}`,
      pathPrefix,
      pathPrefix.length,
      ctx.rect,
      () => matchingPathCompletions(pathPrefix),
    );
    return;
  }

  // Plain snippet completion — synchronous, no backend needed.
  clearCompletionCache();
  const prefix = snippetPrefix(ctx.before);
  if (!prefix || prefix === snippetSuppressedPrefix) {
    hideSnippetPopup();
    return;
  }
  const mode = snippetContextMode(ctx);
  const matches = matchingSnippets(prefix, mode);
  if (matches.length === 0) {
    hideSnippetPopup();
    return;
  }
  showSnippetPopup(prefix, matches, prefix.length, ctx.rect);
}

function chooseSnippetPopupItem(): void {
  const snippet = snippetPopupItems[snippetPopupIndex];
  if (!snippet) return;
  const deleteBefore = snippetDeleteBefore;
  hideSnippetPopup();
  snippetSuppressedPrefix = "";
  insertSnippet(snippet, deleteBefore);
}

function acceptSnippetPopupItem(): boolean {
  if (snippetPopup.hidden || snippetPopupItems.length === 0) return false;
  chooseSnippetPopupItem();
  return true;
}

function applySnippetPopupKeyAction(action: ReturnType<typeof snippetPopupKeyAction>): boolean {
  if (snippetPopup.hidden) return false;
  if (snippetPopupItems.length === 0) {
    hideSnippetPopup();
    return false;
  }
  switch (action.type) {
    case "move":
      snippetPopupIndex = (snippetPopupIndex + action.delta + snippetPopupItems.length) % snippetPopupItems.length;
      updateSnippetPopupActiveOption();
      return true;
    case "page":
      snippetPopupIndex = ((snippetPopupIndex + action.delta) % snippetPopupItems.length + snippetPopupItems.length) % snippetPopupItems.length;
      updateSnippetPopupActiveOption();
      return true;
    case "edge":
      snippetPopupIndex = action.edge === "first" ? 0 : snippetPopupItems.length - 1;
      updateSnippetPopupActiveOption();
      return true;
    case "accept":
      return acceptSnippetPopupItem();
    case "consume":
      return true;
    case "select":
      if (action.index < 0 || action.index >= snippetPopupItems.length) return false;
      snippetPopupIndex = action.index;
      chooseSnippetPopupItem();
      return true;
    case "dismiss":
      snippetSuppressedPrefix = snippetPopup.dataset.prefix ?? "";
      hideSnippetPopup();
      return true;
    case "none":
      return false;
  }
}

function handleSnippetPopupKey(event: KeyboardEvent): boolean {
  const handled = applySnippetPopupKeyAction(snippetPopupKeyAction({
    key: event.key === "\t" ? "Tab" : event.key, // xwidget may send "\t" instead of "Tab"
    shiftKey: event.shiftKey,
    commandKey: event.metaKey && !event.ctrlKey,
    ctrlKey: event.ctrlKey,
    altKey: event.altKey,
    isComposing: event.isComposing,
  }));
  if (handled) {
    event.preventDefault();
  }
  return handled;
}

function handleSnippetPopupHostKey(key: VimLiteKey): boolean {
  return applySnippetPopupKeyAction(snippetPopupKeyAction({
    key: key.key,
    shiftKey: key.shiftKey,
    commandKey: key.metaKey && !key.ctrlKey,
    ctrlKey: key.ctrlKey,
    altKey: key.altKey,
    isComposing: key.isComposing,
  }));
}

function expandSnippetAtCursor(): boolean {
  const ctx = editor.cursorContext(320);
  const prefix = snippetPrefix(ctx.before);
  if (!prefix) return false;
  const mode = snippetContextMode(ctx);
  const matches = matchingSnippets(prefix, mode);
  const exact = matches.find((snippet) => String(snippet.key || "") === prefix)
    ?? (matches.length === 1 ? matches[0] : undefined);
  if (!exact) return false;
  hideSnippetPopup();
  snippetSuppressedPrefix = "";
  return insertSnippet(exact, prefix.length);
}

function mathPreviewKeyFor(math: { tex: string; display: boolean }): string {
  return `${math.display ? "display" : "inline"}\n${math.tex.trim()}`;
}

function resetMathPreviewFitState(): void {
  const child = mathPreview.querySelector<HTMLElement>(".katex-display, .katex, math, mjx-container");
  if (!child) return;
  child.style.transform = "";
  child.style.transformOrigin = "";
  child.style.display = "";
  child.style.maxWidth = "";
  mathPreview.style.minHeight = "";
  mathPreview.classList.remove("is-math-scaled");
}

function mathPreviewPreferredWidth(display: boolean): number {
  const margin = 8;
  const maxWidth = Math.max(220, window.innerWidth - margin * 2);
  const minimum = display ? 220 : 120;
  const fallback = display ? 420 : 180;
  const previousWidth = mathPreview.style.width;
  resetMathPreviewFitState();
  mathPreview.style.width = "max-content";
  const natural = Math.ceil(mathPreview.scrollWidth || mathPreview.offsetWidth || fallback);
  mathPreview.style.width = previousWidth;
  return Math.min(maxWidth, Math.max(minimum, natural));
}

function updateMathPreviewOverflow(): void {
  if (mathPreview.hidden || mathPreview.classList.contains("is-error")) return;
  const overflowX = mathPreview.scrollWidth > mathPreview.clientWidth + 2;
  const overflowY = mathPreview.scrollHeight > mathPreview.clientHeight + 2;
  mathPreview.classList.toggle("is-overflowing", overflowX || overflowY);
}

function placeMathPreview(
  anchorRect: { left: number; top: number; bottom: number } | null,
  display: boolean,
  bottomRect?: { bottom: number } | null,
): void {
  mathPreview.classList.remove("is-overflowing");
  if (!mathPreviewWidth || mathPreviewWidth > window.innerWidth - 16) {
    mathPreviewWidth = mathPreviewPreferredWidth(display);
  }
  placeFloatingAbove(mathPreview, anchorRect, mathPreviewWidth, bottomRect);
  updateMathPreviewOverflow();
}

function scheduleMathPreviewError(nextKey: string, error: string, display: boolean): void {
  clearMathPreviewErrorTimer();
  mathPreviewPendingErrorKey = nextKey;
  const message = `Math error: ${formatMathRenderError(error, MATH_PREVIEW_ERROR_MAX_LENGTH)}`;
  mathPreviewErrorTimer = window.setTimeout(() => {
    if (mathPreviewPendingErrorKey !== nextKey || mathPreviewKey !== nextKey) return;
    if (paused || vim.mode() !== "insert" || !editorSurfaceVisible()) return;
    const ctx = editor.cursorContext(display ? 640 : 320);
    const math = mathAtCursor(ctx);
    if (!math || mathPreviewKeyFor(math) !== nextKey) return;
    const anchorRect = math.rect ?? ctx.rect;
    const bottomRect = math.display ? (math.rectEnd ?? anchorRect) : undefined;
    mathPreview.innerHTML = "";
    mathPreview.textContent = message;
    mathPreview.classList.add("is-error");
    mathPreview.classList.toggle("is-display", math.display);
    mathPreview.hidden = false;
    mathPreviewWidth = 0;
    placeFloatingAbove(mathPreview, anchorRect, math.display ? 640 : 320, bottomRect);
  }, MATH_PREVIEW_ERROR_IDLE_MS);
}

function updateMathPreview(ctx: ReturnType<typeof editor.cursorContext>, allowNewPreview: boolean): void {
  const math = mathAtCursor(ctx);
  if (!math || math.tex.trim().length === 0) {
    if (!mathPreview.hidden || mathPreviewKey) hideMathPreview();
    return;
  }
  const nextKey = mathPreviewKeyFor(math);
  const anchorRect = math.rect ?? ctx.rect;
  const bottomRect = math.display ? (math.rectEnd ?? anchorRect) : undefined;
  if (mathPreview.hidden && !allowNewPreview) return;
  if (mathPreviewKey === nextKey && !mathPreview.hidden) {
    placeMathPreview(anchorRect, math.display, bottomRect);
    return;
  }
  if (mathPreviewKey !== nextKey && !allowNewPreview) return;
  if (mathPreviewKey !== nextKey) {
    clearMathPreviewErrorTimer();
    mathPreviewKey = nextKey;
    mathPreviewWidth = 0;
    mathPreview.innerHTML = "";
    mathPreview.classList.remove("is-error");
    mathPreview.classList.toggle("is-display", math.display);
    let renderFailed = false;
    renderMathLazy(math.tex.trim(), mathPreview, {
      displayMode: math.display,
      strict: "ignore",
    }, (error) => {
      renderFailed = true;
      scheduleMathPreviewError(nextKey, error, math.display);
    });
    if (renderFailed) {
      mathPreview.hidden = true;
      return;
    }
  }
  if (mathPreviewPendingErrorKey === nextKey && mathPreview.hidden) return;
  clearMathPreviewErrorTimer();
  mathPreview.classList.remove("is-error");
  mathPreview.hidden = false;
  placeMathPreview(anchorRect, math.display, bottomRect);
  window.requestAnimationFrame(() => {
    if (!paused && editorSurfaceVisible() && mathPreviewKey === nextKey && !mathPreview.hidden) {
      placeMathPreview(anchorRect, math.display, bottomRect);
    }
  });
}

function activeEditorSelection(): { text: string; rect: DOMRect } | null {
  if (editor.isSourceMode()) return null;
  const selection = window.getSelection();
  if (!selection || selection.isCollapsed || selection.rangeCount === 0) return null;
  const anchor = selection.anchorNode;
  const focus = selection.focusNode;
  if (!anchor || !focus || !host.contains(anchor) || !host.contains(focus)) return null;
  const logical = editor.getSelection();
  const from = Math.min(logical.from, logical.to);
  const to = Math.max(logical.from, logical.to);
  const text = from < to ? editor.textBetween(from, to) : selection.toString();
  if (!text.trim()) return null;
  const rect = selection.getRangeAt(0).getBoundingClientRect();
  if (rect.width === 0 && rect.height === 0) return null;
  return { text, rect };
}

function selectionTouchesEditor(): boolean {
  const selection = window.getSelection();
  if (!selection || selection.rangeCount === 0) return false;
  const anchor = selection.anchorNode;
  const focus = selection.focusNode;
  return Boolean(anchor && focus && host.contains(anchor) && host.contains(focus));
}

function updateSelectionTool(active = activeEditorSelection()): void {
  if (!active || !modal.hidden) {
    selectionTool.hidden = true;
    selectionMore.hidden = true;
    return;
  }
  selectionRoamIdlink.hidden = currentStandalone;
  const margin = 8;
  const width = Math.min(520, Math.max(360, selectionTool.offsetWidth || 440));
  const left = Math.min(
    Math.max(margin, active.rect.left + active.rect.width / 2 - width / 2),
    Math.max(margin, window.innerWidth - width - margin),
  );
  const top = Math.max(margin, active.rect.top - 46);
  selectionTool.style.left = `${left}px`;
  selectionTool.style.top = `${top}px`;
  selectionTool.hidden = false;
}

async function copyActiveSelection(): Promise<void> {
  const active = activeEditorSelection();
  if (!active) return;
  let copied = false;
  try {
    await navigator.clipboard.writeText(active.text);
    copied = true;
  } catch {
    const fallback = document.createElement("textarea");
    fallback.value = active.text;
    fallback.style.position = "fixed";
    fallback.style.left = "-9999px";
    document.body.appendChild(fallback);
    fallback.select();
    copied = document.execCommand("copy");
    fallback.remove();
  }
  setStatus(copied ? "Selection copied" : "Copy failed");
  selectionTool.hidden = true;
}

function runSelectionCommand(command: string): void {
  if (command === "copy") {
    void copyActiveSelection();
    return;
  }
  if (command === "more") {
    selectionMore.hidden = !selectionMore.hidden;
    return;
  }
  if (command === "insert-roam-idlink") {
    selectionMore.hidden = true;
    selectionTool.hidden = true;
    void insertRoamIdLink();
    return;
  }
  if (!["bold", "italic", "highlight", "strike", "code", "link"].includes(command)) return;
  editor.runCommand(command as EditorCommand);
  selectionTool.hidden = true;
  selectionMore.hidden = true;
}

function runAssistUpdate(flags: AssistUpdateFlags): void {
  const needsCursorContext = vim.mode() === "insert" && (
    flags.snippets
    || flags.mathPreview
    || !snippetPopup.hidden
    || !mathPreview.hidden
  );
  const ctx = needsCursorContext ? editor.cursorContext(!snippetPopup.hidden ? 640 : 320) : null;
  if (flags.cursor || ctx) updateVimCursor(vimCursor, editor, vim.mode(), ctx?.rect);
  if (flags.toc) updateFloatingToc();
  if (flags.selectionTool) {
    const activeSelection = snippetPopup.hidden && modal.hidden ? activeEditorSelection() : null;
    updateSelectionTool(activeSelection);
  }
  if (vim.mode() !== "insert") {
    hideSnippetPopup();
    hideMathPreview();
    return;
  }
  if (ctx) {
    if (flags.snippets || !snippetPopup.hidden) updateSnippetPopup(ctx);
    updateMathPreview(ctx, flags.mathPreview);
  }
}

function cancelAssistWork(): void {
  assistScheduler.cancel();
  clearCompletionCache();
  clearMathPreviewErrorTimer();
  hideSnippetPopup();
  hideMathPreview();
  selectionTool.hidden = true;
  selectionMore.hidden = true;
}

function applyPaused(next: boolean): void {
  if (paused === next) return;
  paused = next;
  assistScheduler.setPaused(next);
  document.documentElement.classList.toggle("aaronnote-paused", next);
  if (next) {
    cancelAssistWork();
  } else {
    scheduleAssistUpdate({ cursor: true, mathPreview: true, selectionTool: true, toc: true });
  }
}

function setPausedReason(reason: string, active: boolean): void {
  if (active) pauseReasons.add(reason);
  else pauseReasons.delete(reason);
  applyPaused(pauseReasons.size > 0);
}

function scheduleAssistUpdate(options: AssistUpdateOptions = {}): void {
  assistScheduler.schedule(options);
}

function updateFloatingToc(): void {
  floatingTocPanel.update();
}

async function reloadSnippets(): Promise<void> {
  setStatus("Reloading snippets");
  try {
    const msg = await api.notes.snippets();
    if (!Array.isArray(msg.snippets)) {
      const message = (msg as { message?: string }).message || "Snippet reload failed";
      throw new Error(message);
    }
    snippets = msg.snippets;
    hideSnippetPopup();
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true });
    setStatus(`Reloaded ${snippets.length} snippets`);
  } catch (error) {
    setStatus(error instanceof Error ? error.message : "Snippet reload failed");
  }
}

function insertHostKeyText(key: string, text?: string): boolean {
  const literal = typeof text === "string" ? text
    : key === "Enter" ? "\n"
      : key === "Tab" ? "\t"
        : key.length === 1 ? key
          : "";
  if (!literal) return false;
  editor.insertText(literal);
  return true;
}

function deleteHostKeyText(key: string): boolean {
  const { from, to } = editor.getMarkdownSelection();
  if (from !== to) {
    editor.replaceMarkdownRange(from, to, "", "start");
    return true;
  }
  if (key === "Backspace" && from > 0) {
    editor.replaceMarkdownRange(from - 1, from, "", "start");
    return true;
  }
  if (key === "Delete") {
    editor.replaceMarkdownRange(from, Math.min(from + 1, editor.getMarkdown().length), "", "start");
    return true;
  }
  return false;
}

function runHostKey(body: Record<string, unknown>): boolean {
  const rawKey = String(body.key || "");
  const shiftTabAlias = rawKey === "Backtab" || rawKey === "ISO_Left_Tab" || rawKey === "Shift-Tab";
  const key = shiftTabAlias ? "Tab" : rawKey;
  if (!key) return false;
  const hostKey: VimLiteKey = {
    key,
    ctrlKey: Boolean(body.ctrlKey),
    metaKey: Boolean(body.metaKey),
    altKey: Boolean(body.altKey),
    shiftKey: Boolean(body.shiftKey) || shiftTabAlias,
  };
  editor.focus();
  if (handleSnippetPopupHostKey(hostKey)) {
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
    return true;
  }
  if (vim.handleKey(hostKey)) {
    scheduleAssistUpdate({ cursor: true });
    return true;
  }
  if (key === "Tab") {
    if (vim.mode() !== "insert") return false;
    editor.focus();
    if (hostKey.shiftKey) {
      const tableHandled = tableNavigateCell(editor.view, -1);
      if (tableHandled) { scheduleAssistUpdate({ cursor: true }); return true; }
      const handled = jumpSnippetTabstopBack();
      if (handled) scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
      return handled;
    }
    const tableHandled = tableNavigateCell(editor.view, 1);
    if (tableHandled) { scheduleAssistUpdate({ cursor: true }); return true; }
    const snippetHandled = jumpSnippetTabstop() || expandSnippetAtCursor();
    if (snippetHandled) {
      scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
      return true;
    }
    // no snippet — fall through to insertHostKeyText("\t")
  }
  if (vim.mode() !== "insert" || hostKey.ctrlKey || hostKey.metaKey || hostKey.altKey) return false;
  if (key === "Enter") {
    const handled = tableEnterSameColumn(editor.view) || exitEmptyMarkdownBlock(editor.view) || continueMarkdownBlock(editor.view);
    if (!handled) editor.insertText("\n");
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
    return true;
  }
  if (key === "Backspace" || key === "Delete") {
    const handled = deleteHostKeyText(key);
    if (handled) scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
    return handled;
  }
  const inserted = insertHostKeyText(key, typeof body.text === "string" ? body.text : undefined);
  if (inserted) scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
  return inserted;
}

function runHostCommand(detail: unknown): boolean {
  const body = (detail && typeof detail === "object" ? detail : {}) as {
    command?: string;
    key?: string;
    value?: string;
    mode?: VimLiteMode;
    version?: number;
  };
  const command = String(body.command || "").trim().toLowerCase();
  if (!command) return false;

  switch (command) {
    case "notes-index-changed": {
      const version = typeof body.version === "number" ? body.version : 0;
      // Ignore stale broadcasts (e.g. replayed on reconnect).
      if (version && version <= lastNotesIndexVersion) return true;
      if (version) lastNotesIndexVersion = version;
      if (pauseReasons.has("visibility")) {
        pendingNotesRefresh = true;
      } else {
        notesRefreshTimer.schedule(() => void reloadNotes(false));
      }
      return true;
    }
    case "key":
      return runHostKey(body as Record<string, unknown>);
    case "pause":
      setPausedReason("host", true);
      return true;
    case "resume":
      setPausedReason("host", false);
      return true;
    case "toggle-pause":
      setPausedReason("host", !pauseReasons.has("host"));
      return true;
    case "save":
      void save();
      return true;
    case "back":
    case "nav-back":
    case "navigation-back":
      void restoreNavigationBack();
      return true;
    case "focus":
      editor.focus();
      return true;
    case "paste":
      editor.focus();
      void editor.pasteFromClipboard();
      return true;
    case "escape":
    case "normal":
    case "vim-normal":
      vim.setMode("normal");
      editor.focus();
      return true;
    case "insert":
    case "vim-insert":
      vim.setMode("insert");
      editor.focus();
      return true;
    case "toggle-source":
    case "source":
      toggleSourceMode();
      return true;
    case "undo":
      editor.focus();
      return editor.undo();
    case "redo":
      editor.focus();
      return editor.redo();
    default:
      if (isEditorCommand(command)) {
        editor.focus();
        return editor.runCommand(command, body.value || "");
      }
      return false;
  }
}

tocButton.addEventListener("click", () => {
  floatingTocPanel.toggle();
  updateFloatingToc();
});
toolsButton.addEventListener("click", toggleToolsPanel);
toolsClose.addEventListener("click", closeToolsPanel);
roamToolsClose.addEventListener("click", closeRoamToolsPanel);
sourceButton.addEventListener("click", toggleSourceMode);
saveButton.addEventListener("click", () => void save());
document.addEventListener("keydown", (event) => {
  if (handleXwidgetEmacsKeydown(event)) return;
  snippetSuppressedPrefix = event.key === "Escape" ? snippetSuppressedPrefix : "";
  if (handleSnippetPopupKey(event)) {
    event.stopPropagation();
    return;
  }
  if (plainEscapeKey(event)) {
    if (!modal.hidden) return;
    if (!toolsPanel.hidden) {
      event.preventDefault();
      closeToolsPanel();
      editor.focus();
      return;
    }
    if (!roamToolsPanel.hidden) {
      event.preventDefault();
      closeRoamToolsPanel();
      editor.focus();
      return;
    }
  }
  if (handleXwidgetControlKeydown(event, {
    editor,
    editorHost: host,
    vim,
    enabled: modal.hidden && toolsPanel.hidden && roamToolsPanel.hidden,
  })) {
    if (plainEscapeKey(event)) noteCursorPositionEvent();
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
    return;
  }
  if (handleXwidgetVimKeydown(event, {
    editor,
    editorHost: host,
    vim,
    enabled: modal.hidden && toolsPanel.hidden && roamToolsPanel.hidden,
  })) {
    if (plainEscapeKey(event)) noteCursorPositionEvent();
    scheduleAssistUpdate({ cursor: true });
    return;
  }
  if (vim.mode() === "insert" && (event.key === "Tab" || event.key === "\t") && !event.metaKey && !event.ctrlKey && !event.altKey) {
    const handled = event.shiftKey
      ? jumpSnippetTabstopBack()
      : jumpSnippetTabstop() || expandSnippetAtCursor();
    if (handled) {
      event.preventDefault();
      event.stopPropagation();
      return;
    }
  }
  if (handleXwidgetSpecialKeydown(event, {
    editor,
    editorHost: host,
    vim,
    enabled: modal.hidden && toolsPanel.hidden && roamToolsPanel.hidden,
  })) {
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
    return;
  }
  if (vim.handleKeyDown(event)) {
    if (plainEscapeKey(event)) noteCursorPositionEvent();
    scheduleAssistUpdate({ cursor: true });
    event.stopPropagation();
    return;
  }
  if (runSourceToggleShortcut(event)) {
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
    event.stopPropagation();
    return;
  }
  if (primaryMod(event) && !event.shiftKey && !event.altKey && event.key.toLowerCase() === "s") {
    event.preventDefault();
    void save();
    event.stopPropagation();
    return;
  }
  if (runFormattingShortcut(event)) {
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
    event.stopPropagation();
    return;
  }
}, true);
document.addEventListener("beforeinput", (event) => {
  const ie = event as InputEvent;
  // xwidget Tab: may arrive only as beforeinput(insertText, "\t") with no keydown.
  // Try snippet popup acceptance and snippet expansion before letting CM6 insert \t.
  if (ie.inputType === "insertText" && ie.data === "\t"
      && vim.mode() === "insert"
      && modal.hidden && toolsPanel.hidden && roamToolsPanel.hidden) {
    const accepted = applySnippetPopupKeyAction(snippetPopupKeyAction({
      key: "Tab", shiftKey: false, commandKey: false, ctrlKey: false, altKey: false, isComposing: false,
    }));
    if (accepted) {
      event.preventDefault();
      scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
      return;
    }
    if (jumpSnippetTabstop() || expandSnippetAtCursor()) {
      event.preventDefault();
      scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
      return;
    }
    if (tableNavigateCell(editor.view, 1)) {
      event.preventDefault();
      scheduleAssistUpdate({ cursor: true });
      return;
    }
    // No snippet match: fall through so CM6 inserts \t naturally
    return;
  }
  if (handleXwidgetControlBeforeInput(event as InputEvent, {
    editor,
    editorHost: host,
    vim,
    enabled: modal.hidden && toolsPanel.hidden && roamToolsPanel.hidden,
  })) {
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
    return;
  }
  if (handleXwidgetSpecialBeforeInput(event as InputEvent, {
    editor,
    editorHost: host,
    vim,
    enabled: modal.hidden && toolsPanel.hidden && roamToolsPanel.hidden,
  })) {
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true });
    return;
  }
  if (handleXwidgetVimBeforeInput(event as InputEvent, {
    editor,
    editorHost: host,
    vim,
    enabled: modal.hidden && toolsPanel.hidden && roamToolsPanel.hidden,
  })) {
    scheduleAssistUpdate({ cursor: true });
  }
}, true);
document.addEventListener("selectionchange", () => {
  if (!editorSurfaceVisible()) return;
  if (selectionTouchesEditor() || !selectionTool.hidden) {
    scheduleAssistUpdate({ selectionTool: true });
  }
});
document.addEventListener("mouseup", (event) => {
  if (!editorSurfaceVisible()) return;
  if (event.target instanceof Node && host.contains(event.target)) noteCursorPositionEvent();
  scheduleAssistUpdate({ mathPreview: true, cursor: true, selectionTool: true });
});
window.addEventListener("resize", () => {
  scheduleAssistUpdate({ mathPreview: true, cursor: true, selectionTool: !selectionTool.hidden });
});
window.addEventListener("scroll", () => scheduleAssistUpdate({ mathPreview: true, cursor: true, selectionTool: !selectionTool.hidden }), { capture: true, passive: true });
selectionTool.addEventListener("mousedown", (event) => event.preventDefault());
selectionTool.addEventListener("click", (event) => {
  const button = (event.target as Element | null)?.closest<HTMLButtonElement>("[data-selection-command]");
  if (!button) return;
  event.preventDefault();
  event.stopPropagation();
  runSelectionCommand(button.dataset.selectionCommand || "");
});
document.addEventListener("aaronnote:open-url", (event) => {
  const custom = event as CustomEvent<{ href?: string; newWindow?: boolean }>;
  const href = custom.detail?.href;
  if (!href) return;
  event.preventDefault();
  openExternalUrl(href, { newWindow: custom.detail?.newWindow === true });
});
document.addEventListener("aaronnote:open-attachment", (event) => {
  const custom = event as CustomEvent<{ href?: string }>;
  const href = custom.detail?.href;
  if (!href) return;
  event.preventDefault();
  const rawPath = hrefPath(href) || href;
  void api.emacs.systemOpen(rawPath, currentFile).catch((err) => setStatus(`Open failed: ${String(err)}`));
});
window.addEventListener("aaronnote:open-file", (event) => {
  const detail = (event as CustomEvent<{ file?: string }>).detail;
  if (detail?.file && detail.file !== currentFile) pushNavigationBackLocation();
  void openFile(detail?.file);
});
window.addEventListener("aaronnote:command", (event) => {
  runHostCommand((event as CustomEvent<unknown>).detail);
});
document.addEventListener("visibilitychange", () => {
  if (document.hidden) {
    setPausedReason("visibility", true);
    void flushCursorPosition();
  } else {
    setPausedReason("visibility", false);
    if (pendingNotesRefresh) {
      pendingNotesRefresh = false;
      notesRefreshTimer.schedule(() => void reloadNotes(false));
    }
  }
});
window.addEventListener("pagehide", () => {
  void flushCursorPosition();
  if (currentFile && revision !== savedRevision) api.notes.saveKeepalive(saveBody());
});
window.addEventListener("beforeunload", () => {
  void flushCursorPosition();
});
window.addEventListener("popstate", () => {
  void restoreNavigationBack();
});

void openInitialFile();
