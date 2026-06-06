import "../src/styles/widgets.css";
import "../src/styles/theme-typora.css";
import "./style.css";

import { createEditor, type EditorCommand } from "../src/lib.ts";
import { setupCopilot } from "../src/copilot/index.ts";
import { getBlockMathRanges, rangeAtPosition, rangeOverlapsAny } from "../src/cm6/math-ranges.ts";
import { equationTagsFromText, getEquationTagHits } from "../src/equation-tags.ts";
import { INLINE_MATH_RE, isLikelyInlineMath } from "../src/inline-math.ts";
import { formatMathRenderError, renderMathLazy } from "../src/math-render.ts";
import { hrefProtocol, safeHref } from "../src/url-safety.ts";
import { api } from "./api-client.ts";
import { createFloatingTocPanel, inlineTagAnchorsFromText, markdownHeadingsFromText } from "./floating-toc.ts";
import {
  canonicalRoamNoteId,
  escapeMarkdownLinkText,
  markdownRoamIdLink,
  resolveRoamNoteSearch,
  roamHrefForNote,
  roamNoteSearchValue,
} from "./roam-idlink.ts";
import { matchingSnippetsForPrefix, SnippetSession, snippetDetail, snippetLabel } from "./snippets.ts";
import type { NoteSummary, SnippetSummary } from "./types.ts";
import { createVimCursor, updateVimCursor } from "./vim-cursor.ts";
import { createVimLite, type VimLiteKey, type VimLiteMode } from "./vim-lite.ts";

const root = document.querySelector<HTMLElement>("#app");
if (!root) throw new Error("Missing #app");

root.innerHTML = `
  <main class="aaronnote-focused-shell">
    <header class="aaronnote-focused-bar">
      <strong data-file>AaronNote</strong>
      <span data-vim-mode>INSERT</span>
      <span data-status>Opening...</span>
      <button type="button" data-toc-toggle aria-expanded="false">TOC</button>
      <button type="button" data-tools-toggle aria-expanded="false">Tools</button>
      <button type="button" data-source>Source</button>
      <button type="button" data-save>Save</button>
    </header>
    <section class="aaronnote-focused-editor" data-editor></section>
  </main>
`;

const host = root.querySelector<HTMLElement>("[data-editor]")!;
const fileLabel = root.querySelector<HTMLElement>("[data-file]")!;
const modeLabel = root.querySelector<HTMLElement>("[data-vim-mode]")!;
const statusLabel = root.querySelector<HTMLElement>("[data-status]")!;
const tocButton = root.querySelector<HTMLButtonElement>("[data-toc-toggle]")!;
const toolsButton = root.querySelector<HTMLButtonElement>("[data-tools-toggle]")!;
const sourceButton = root.querySelector<HTMLButtonElement>("[data-source]")!;
const saveButton = root.querySelector<HTMLButtonElement>("[data-save]")!;
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

const vimCursor = createVimCursor();

let currentFile = "";
let currentKind = "";
let currentStandalone = false;
let currentMtimeMs = 0;
let revision = 0;
let savedRevision = 0;
let applyingContent = false;
let saveTimer = 0;
let snippets: SnippetSummary[] = [];
let notes: NoteSummary[] = [];
let pathSuggestions: string[] = [];
let pendingOpenHash = "";
let pendingOpenDomTarget = "";
let snippetPopupItems: SnippetSummary[] = [];
let snippetPopupIndex = 0;
let snippetDeleteBefore = 0;
let snippetSuppressedPrefix = "";
let snippetRenderKey = "";
let assistFrame = 0;
let snippetScanRequested = false;
let mathPreviewUpdateRequested = false;
let vimCursorUpdateRequested = false;
let tocUpdateRequested = false;
let mathPreviewKey = "";
let mathPreviewPendingErrorKey = "";
let mathPreviewErrorTimer = 0;
const clientId = globalThis.crypto?.randomUUID?.() ?? `${Date.now()}-${Math.random().toString(16).slice(2)}`;
const changeHandlers = new Set<() => void>();
const MATH_PREVIEW_ERROR_IDLE_MS = 650;
const MATH_PREVIEW_ERROR_MAX_LENGTH = 180;
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
  "copy-code",
]);

window.AaronnoteCurrentFile = () => currentFile;

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

const editor = createEditor(host, {
  initialContent: "",
  onChange: () => {
    if (!applyingContent) revision += 1;
    updateTitle();
    changeHandlers.forEach((handler) => handler());
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true });
    scheduleSave();
  },
});
const snippetSession = new SnippetSession(editor);
host.addEventListener("aaronnote-assist-update", () => scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true }));

const vim = createVimLite(editor, host, {
  onModeChange: updateModeLabel,
  onUndo: () => editor.undo(),
  onRedo: () => editor.redo(),
});
updateModeLabel(vim.mode());

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
  setStatus("Saving...");
  try {
    const result = await api.notes.save(saveBody());
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

function applyOpenedNote(opened: Awaited<ReturnType<typeof api.notes.bootstrap>>, fallbackFile?: string): void {
  currentFile = String(opened.file || fallbackFile || "");
  currentKind = String(opened.kind || "");
  currentStandalone = Boolean(opened.standalone);
  applyIndexPayload(opened);
  if (Array.isArray(opened.snippets)) snippets = opened.snippets;
  currentMtimeMs = Number(opened.mtimeMs) || 0;
  applyingContent = true;
  editor.setMarkdown(String(opened.content || ""), { history: "reset" });
  applyingContent = false;
  revision = 0;
  savedRevision = 0;
  if ((opened.mode === "source") !== editor.isSourceMode()) editor.toggleSource();
  sourceButton.classList.toggle("is-active", editor.isSourceMode());
  const from = Number(opened.selection?.from);
  const to = Number(opened.selection?.to ?? from);
  if (Number.isFinite(from)) {
    editor.setMarkdownSelection(from, Number.isFinite(to) ? to : from);
    editor.revealCursor();
  }
  snippetSession.clear();
  hideSnippetPopup();
  hideMathPreview();
  vim.setMode("insert");
  updateTitle();
  void api.emacs.currentFile(currentFile);
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
    if (currentFile && revision !== savedRevision) {
      await save();
      if (revision !== savedRevision) return;
    }
    const opened = target && !bootstrap
      ? await api.notes.open(target)
      : await api.notes.bootstrap(target);
    applyOpenedNote(opened, target);
  } catch (error) {
    applyingContent = false;
    setStatus(error instanceof Error ? error.message : "Open failed");
  }
}

async function openInitialFile(): Promise<void> {
  const params = new URLSearchParams(window.location.search);
  const file = params.get("file") || undefined;
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
  scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true });
}

function isEditorCommand(command: string): command is EditorCommand {
  return editorCommands.has(command as EditorCommand);
}

function primaryMod(event: KeyboardEvent): boolean {
  return /Mac/.test(navigator.platform)
    ? event.metaKey && !event.ctrlKey
    : event.ctrlKey && !event.metaKey;
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

function applyIndexPayload(payload: { notes?: NoteSummary[]; note?: NoteSummary; kind?: string; standalone?: boolean }): void {
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
  if (options.newWindow) {
    const url = new URL(window.location.href);
    url.searchParams.set("file", note.file);
    if (options.hash) url.searchParams.set("hash", options.hash);
    if (options.domTarget) url.searchParams.set("dom", options.domTarget);
    window.open(url.toString(), "_blank", "noopener,noreferrer");
    return;
  }
  if (note.file === currentFile) {
    if (options.domTarget && !jumpToDomTarget(options.domTarget)) setStatus(`DOM target not found: ${options.domTarget}`);
    else if (options.hash && !jumpToHash(options.hash)) setStatus(`Anchor not found: ${options.hash}`);
    if (options.domTarget || options.hash) return;
  }
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
    return true;
  }
  const inline = inlineTagAnchorsFromText(editor.getMarkdown())
    .find((anchor) => anchor.tag.toLowerCase() === clean.toLowerCase()
      || `tag-${anchor.tag}`.toLowerCase() === clean.toLowerCase());
  if (inline) {
    editor.setMarkdownSelection(inline.pos, inline.to);
    editor.revealCursor();
    editor.focus();
    return true;
  }
  const heading = markdownHeadingsFromText(editor.view.state.doc)
    .find((item) => item.text.toLowerCase() === clean.toLowerCase()
      || item.slug === clean
      || slugifyAnchor(item.text) === clean);
  if (heading) {
    editor.setMarkdownSelection(heading.pos);
    editor.revealCursor();
    editor.focus();
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
  const hash = hrefHash(raw);
  const target = resolveHrefTarget(raw);
  const note = target.note;
  const targetHash = target.hash || hash;
  const targetDom = target.domTarget;
  if (note?.file) {
    if (note.file === currentFile && targetDom) {
      if (!jumpToDomTarget(targetDom)) setStatus(`DOM target not found: ${targetDom}`);
      return;
    }
    if (note.file === currentFile && targetHash) {
      if (!jumpToHash(targetHash)) setStatus(`Anchor not found: ${targetHash}`);
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
    if (!jumpToHash(hash || raw.slice(1))) setStatus(`Anchor not found: ${hash || raw.slice(1)}`);
    return;
  }
  const protocol = hrefProtocol(raw);
  if (!protocol) {
    setStatus(`Note not found: ${hrefPath(raw) || raw}`);
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
  if (!roamFeaturesEnabled()) {
    setStatus("Roam links are disabled for this standalone note");
    return;
  }
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
  if (!roamFeaturesEnabled()) {
    setStatus("Roam tools are disabled for this standalone note");
    return;
  }
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
  if (!roamFeaturesEnabled()) {
    setStatus("Roam tools are disabled for this standalone note");
    return;
  }
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
  if (!roamFeaturesEnabled()) {
    setStatus("Roam tools are disabled for this standalone note");
    return;
  }
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
  if (!roamFeaturesEnabled()) {
    setStatus("Roam tools are disabled for this standalone note");
    return;
  }
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
  if (!roamFeaturesEnabled()) {
    setStatus("Roam tools are disabled for this standalone note");
    return;
  }
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
  if (!roamFeaturesEnabled()) return common;
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
  mathPreviewKey = "";
  mathPreviewPendingErrorKey = "";
}

function hideSnippetPopup(): void {
  snippetPopup.hidden = true;
  snippetPopupItems = [];
  snippetPopupIndex = 0;
  snippetDeleteBefore = 0;
  snippetRenderKey = "";
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
  const match = before.match(/(?:^|[\s([{"'=])((?:\.{1,2}\/)[^\s\])}"'`<>]*)$/);
  return match?.[1] ?? "";
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

function noteCompletionSearch(note: NoteSummary): string {
  return noteSearchValues(note).join(" ").toLowerCase();
}

function displayPathCompletion(path: string, prefix: string): string {
  if (prefix.startsWith("./") && !path.startsWith("./") && !path.startsWith("../") && !path.startsWith("/")) return `./${path}`;
  return path;
}

function pathCompletionMatches(path: string, prefix: string): boolean {
  const query = prefix.toLowerCase();
  const display = displayPathCompletion(path, prefix).toLowerCase();
  if (display.startsWith(query)) return true;
  if (query.startsWith("./")) return path.toLowerCase().startsWith(query.slice(2));
  return false;
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

function relativeNotePath(fromDir: string, toPath: string): string {
  const from = normalizeNotePath(fromDir);
  const target = normalizeNotePath(toPath);
  if (!target) return "";
  if (!from || from.startsWith("/") !== target.startsWith("/")) return target;
  const fromParts = from.split("/").filter(Boolean);
  const targetParts = target.split("/").filter(Boolean);
  let shared = 0;
  while (shared < fromParts.length && shared < targetParts.length && fromParts[shared] === targetParts[shared]) shared++;
  const up = Array.from({ length: fromParts.length - shared }, () => "..");
  const down = targetParts.slice(shared);
  return [...up, ...down].join("/") || fileNameFromPath(target);
}

function indexedPathSuggestions(): string[] {
  const values = new Set(pathSuggestions);
  const current = currentNote();
  const currentPath = String(current?.path || current?.link || "").trim();
  const baseDir = dirnamePath(currentPath);
  for (const note of notes) {
    const rawPaths = [note.path, note.link, note.file]
      .map((value) => String(value || "").trim())
      .filter(Boolean);
    rawPaths.forEach((path) => values.add(path));
    const notePath = String(note.path || note.link || "").trim();
    if (!currentPath || !notePath) continue;
    const relativePath = relativeNotePath(baseDir, notePath);
    if (!relativePath) continue;
    values.add(relativePath);
    if (!relativePath.startsWith(".") && !relativePath.startsWith("/")) values.add(`./${relativePath}`);
  }
  return [...values].sort((a, b) => a.localeCompare(b));
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

function noteInlineTagsForCompletion(note: NoteSummary): string[] {
  const tags = note.file === currentFile
    ? allAnchorTagSuggestions()
    : [...(note.inlineTags ?? [])];
  return [...new Set(tags.map((tag) => normalizeInlineTag(tag).replace(/^#/, "")).filter(Boolean))]
    .sort((a, b) => a.localeCompare(b));
}

function matchingTagCompletions(note: NoteSummary, prefix: string): SnippetSummary[] {
  const query = prefix.toLowerCase().replace(/^tag-/, "");
  return noteInlineTagsForCompletion(note)
    .filter((tag) => tag.toLowerCase().includes(query))
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
  if (indexed.length > 0) return indexed;

  const tocStack: Array<{ level: number; path: string[]; labelPath: string[] }> = [];
  const tocTargets = (note.bookToc ?? []).map((item) => {
    const level = Math.max(1, Number(item.level || 1));
    const label = normalizeDomTarget(item.text || item.slug || "");
    const slug = slugDomTarget(item.slug || label);
    while (tocStack.length > 0 && tocStack[tocStack.length - 1]!.level >= level) tocStack.pop();
    const parent = tocStack[tocStack.length - 1];
    const path = [...(parent?.path ?? []), slug];
    const labelPath = [...(parent?.labelPath ?? []), label];
    tocStack.push({ level, path, labelPath });
    return { label, slug, path, labelPath, level, notePath: item.path || "" };
  }).filter((target) => target.label && target.slug);
  if (tocTargets.length > 0) return tocTargets;

  return (note.bookDomTargets ?? []).map((target) => {
    const label = normalizeDomTarget(target.label || target.slug || "");
    const slug = slugDomTarget(target.slug || label);
    return { label, slug, path: [slug], labelPath: [label], level: target.level || 1, notePath: target.path || "" };
  }).filter((target) => target.label && target.slug);
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

function matchingRoamCompletions(prefix: string): SnippetSummary[] {
  if (!roamFeaturesEnabled()) return [];
  const needle = prefix.trim().toLowerCase();
  return notes
    .filter((note) => note.roam && canonicalRoamNoteId(note))
    .filter((note) => !needle || noteCompletionSearch(note).includes(needle))
    .slice(0, 12)
    .map((note) => {
      const id = canonicalRoamNoteId(note);
      return {
        key: note.title || id,
        name: note.title || id,
        body: `${encodeURIComponent(id)}`,
        mode: "markdown-mode",
        group: "roam",
        source: note.path || note.file || id,
      };
    });
}

function matchingWikilinkCompletions(prefix: string): SnippetSummary[] {
  const needle = prefix.trim().toLowerCase();
  return notes
    .filter((note) => !needle || noteCompletionSearch(note).includes(needle))
    .slice(0, 12)
    .map((note) => {
      const label = String(note.title || note.path || note.link || note.file || canonicalRoamNoteId(note) || "Untitled")
        .replace(/[\r\n\]]+/g, " ")
        .replace(/\s+/g, " ")
        .trim() || "Untitled";
      return {
        key: label,
        name: label,
        body: `${label}]]`,
        mode: "markdown-mode",
        group: "wikilink",
        source: note.path || note.file || "",
      };
    });
}

function matchingInlineTagCompletions(prefix: string): SnippetSummary[] {
  const needle = normalizeInlineTag(prefix).toLowerCase();
  const tags = new Map<string, string>();
  for (const tag of [...allAnchorTagSuggestions(), ...notes.flatMap((note) => note.inlineTags ?? [])]) {
    const clean = normalizeInlineTag(tag);
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

function matchingPathCompletions(prefix: string): SnippetSummary[] {
  if (!prefix) return [];
  return indexedPathSuggestions()
    .filter((path) => pathCompletionMatches(path, prefix))
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
}

function linkTargetCompletionMatches(href: string, prefix: string): {
  renderPrefix: string;
  deleteBefore: number;
  matches: SnippetSummary[];
} | null {
  const targetPrefix = cleanHref(prefix);
  const target = cleanHref(href);
  const hashIndex = targetPrefix.lastIndexOf("#");
  if (hashIndex >= 0) {
    const ref = targetPrefix.slice(0, hashIndex);
    const note = noteFromCompletionRef(ref || target);
    if (!note) return null;
    const tagPrefix = targetPrefix.slice(hashIndex + 1);
    const matches = matchingTagCompletions(note, tagPrefix);
    return { renderPrefix: `#${tagPrefix}`, deleteBefore: tagPrefix.length, matches };
  }

  const domParts = domCompletionParts(targetPrefix);
  if (domParts) {
    const note = noteFromCompletionRef(domParts.ref);
    if (!note) return null;
    const matches = matchingDomCompletions(note, domParts.domPrefix, domParts.parentSegments);
    return { renderPrefix: `@${domParts.domPrefix}`, deleteBefore: domParts.domPrefix.length, matches };
  }

  const roamPrefix = targetPrefix.match(/^roam:\/\/(.*)$/i)?.[1];
  if (roamPrefix != null) {
    if (!roamFeaturesEnabled()) return null;
    const matches = matchingRoamCompletions(roamPrefix);
    return { renderPrefix: `roam://${roamPrefix}`, deleteBefore: roamPrefix.length, matches };
  }

  if (/^\.{1,2}\//.test(targetPrefix)) {
    const matches = matchingPathCompletions(targetPrefix);
    return { renderPrefix: targetPrefix, deleteBefore: targetPrefix.length, matches };
  }

  return null;
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
      snippetRenderKey = "";
      renderSnippetPopup(snippetPopup.dataset.prefix ?? prefix, editor.cursorRect());
    });
    snippetPopup.appendChild(button);
  });
  snippetPopup.dataset.prefix = prefix;
  snippetPopup.setAttribute("aria-activedescendant", `aaronnote-snippet-option-${snippetPopupIndex}`);
  snippetPopup.hidden = false;
  placeFloating(snippetPopup, rect);
  snippetPopup.querySelector(".aaronnote-snippet-option.is-active")?.scrollIntoView({ block: "nearest" });
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

function updateSnippetPopup(ctx: ReturnType<typeof editor.cursorContext>): void {
  if (!editorOwnsActiveSurface()) {
    hideSnippetPopup();
    return;
  }
  const linkTarget = markdownInlineLinkTargetAtCursor();
  const linkMatches = linkTarget ? linkTargetCompletionMatches(linkTarget.href, linkTarget.prefix) : null;
  if (linkMatches) {
    if (linkMatches.renderPrefix === snippetSuppressedPrefix || linkMatches.matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = linkMatches.deleteBefore;
    snippetPopupIndex = Math.min(snippetPopupIndex, linkMatches.matches.length - 1);
    snippetPopupItems = linkMatches.matches;
    renderSnippetPopup(linkMatches.renderPrefix, ctx.rect);
    return;
  }
  const domContext = domCompletionContext(ctx.before);
  if (domContext) {
    const renderPrefix = `@${domContext.domPrefix}`;
    if (renderPrefix === snippetSuppressedPrefix) {
      hideSnippetPopup();
      return;
    }
    const matches = matchingDomCompletions(domContext.note, domContext.domPrefix, domContext.parentSegments);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = domContext.domPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(renderPrefix, ctx.rect);
    return;
  }
  const tagContext = tagCompletionContext(ctx.before);
  if (tagContext) {
    const renderPrefix = `#${tagContext.tagPrefix}`;
    if (renderPrefix === snippetSuppressedPrefix) {
      hideSnippetPopup();
      return;
    }
    const matches = matchingTagCompletions(tagContext.note, tagContext.tagPrefix);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = tagContext.tagPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(renderPrefix, ctx.rect);
    return;
  }
  const inlineTagPrefix = inlineTagCompletionPrefix(ctx.before);
  if (inlineTagPrefix !== null) {
    const renderPrefix = `@@tag[${inlineTagPrefix}`;
    if (renderPrefix === snippetSuppressedPrefix) {
      hideSnippetPopup();
      return;
    }
    const matches = matchingInlineTagCompletions(inlineTagPrefix);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = inlineTagPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(renderPrefix, ctx.rect);
    return;
  }
  const wikilinkPrefix = wikilinkCompletionPrefix(ctx.before);
  if (wikilinkPrefix !== null) {
    const renderPrefix = `[[${wikilinkPrefix}`;
    if (renderPrefix === snippetSuppressedPrefix) {
      hideSnippetPopup();
      return;
    }
    const matches = matchingWikilinkCompletions(wikilinkPrefix);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = wikilinkPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(renderPrefix, ctx.rect);
    return;
  }
  const roamPrefix = roamCompletionPrefix(ctx.before);
  if (roamPrefix !== null) {
    const renderPrefix = `roam://${roamPrefix}`;
    if (renderPrefix === snippetSuppressedPrefix) {
      hideSnippetPopup();
      return;
    }
    const matches = matchingRoamCompletions(roamPrefix);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = roamPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(renderPrefix, ctx.rect);
    return;
  }
  const pathPrefix = pathCompletionPrefix(ctx.before);
  if (pathPrefix) {
    if (pathPrefix === snippetSuppressedPrefix) {
      hideSnippetPopup();
      return;
    }
    const matches = matchingPathCompletions(pathPrefix);
    if (matches.length === 0) {
      hideSnippetPopup();
      return;
    }
    snippetDeleteBefore = pathPrefix.length;
    snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
    snippetPopupItems = matches;
    renderSnippetPopup(pathPrefix, ctx.rect);
    return;
  }
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
  snippetDeleteBefore = prefix.length;
  snippetPopupIndex = Math.min(snippetPopupIndex, matches.length - 1);
  snippetPopupItems = matches;
  renderSnippetPopup(prefix, ctx.rect);
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

function snippetPopupKeyName(key: string): string {
  const normalized = String(key || "");
  if (/^(?:Enter|Return|RET|CR|NumpadEnter)$/i.test(normalized)) return "Enter";
  if (/^(?:Esc|Escape)$/i.test(normalized)) return "Escape";
  if (/^(?:Backtab|Shift-Tab)$/i.test(normalized)) return "Shift-Tab";
  return normalized;
}

function handleSnippetPopupKey(event: KeyboardEvent): boolean {
  if (snippetPopup.hidden || event.isComposing) return false;
  if (event.metaKey || event.ctrlKey || event.altKey) return false;
  const key = snippetPopupKeyName(event.key);
  if (snippetPopupItems.length === 0) {
    hideSnippetPopup();
    return false;
  }
  if (key === "ArrowDown") {
    event.preventDefault();
    snippetPopupIndex = (snippetPopupIndex + 1) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (key === "ArrowUp") {
    event.preventDefault();
    snippetPopupIndex = (snippetPopupIndex + snippetPopupItems.length - 1) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (key === "PageDown" || key === "PageUp") {
    event.preventDefault();
    const delta = key === "PageDown" ? 6 : -6;
    snippetPopupIndex = ((snippetPopupIndex + delta) % snippetPopupItems.length + snippetPopupItems.length) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (key === "Home" || key === "End") {
    event.preventDefault();
    snippetPopupIndex = key === "Home" ? 0 : snippetPopupItems.length - 1;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (key === "Enter" || (key === "Tab" && !event.shiftKey)) {
    event.preventDefault();
    acceptSnippetPopupItem();
    return true;
  }
  if (key === "Escape") {
    event.preventDefault();
    snippetSuppressedPrefix = snippetPopup.dataset.prefix ?? "";
    hideSnippetPopup();
    return true;
  }
  return false;
}

function handleSnippetPopupHostKey(key: VimLiteKey): boolean {
  if (snippetPopup.hidden || key.metaKey || key.ctrlKey || key.altKey) return false;
  const name = snippetPopupKeyName(key.key);
  if (snippetPopupItems.length === 0) {
    hideSnippetPopup();
    return false;
  }
  if (name === "ArrowDown") {
    snippetPopupIndex = (snippetPopupIndex + 1) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (name === "ArrowUp") {
    snippetPopupIndex = (snippetPopupIndex + snippetPopupItems.length - 1) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (name === "PageDown" || name === "PageUp") {
    const delta = name === "PageDown" ? 6 : -6;
    snippetPopupIndex = ((snippetPopupIndex + delta) % snippetPopupItems.length + snippetPopupItems.length) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (name === "Home" || name === "End") {
    snippetPopupIndex = name === "Home" ? 0 : snippetPopupItems.length - 1;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorRect());
    return true;
  }
  if (name === "Enter" || (name === "Tab" && !key.shiftKey)) {
    return acceptSnippetPopupItem();
  }
  if (name === "Escape") {
    snippetSuppressedPrefix = snippetPopup.dataset.prefix ?? "";
    hideSnippetPopup();
    return true;
  }
  return false;
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

function mathPreviewPreferredWidth(display: boolean): number {
  const margin = 8;
  const maxWidth = Math.max(220, window.innerWidth - margin * 2);
  const fallback = display ? 640 : 320;
  const natural = Math.max(mathPreview.scrollWidth, fallback);
  const padding = display ? 40 : 28;
  const minimum = display ? 420 : 280;
  return Math.min(maxWidth, Math.max(Math.min(fallback, maxWidth), minimum, Math.ceil(natural + padding)));
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
  placeFloatingAbove(mathPreview, anchorRect, mathPreviewPreferredWidth(display), bottomRect);
  updateMathPreviewOverflow();
}

function scheduleMathPreviewError(nextKey: string, error: string, display: boolean): void {
  clearMathPreviewErrorTimer();
  mathPreviewPendingErrorKey = nextKey;
  const message = `Math error: ${formatMathRenderError(error, MATH_PREVIEW_ERROR_MAX_LENGTH)}`;
  mathPreviewErrorTimer = window.setTimeout(() => {
    if (mathPreviewPendingErrorKey !== nextKey || mathPreviewKey !== nextKey) return;
    if (vim.mode() !== "insert" || !editorSurfaceVisible()) return;
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
    if (mathPreviewKey === nextKey && !mathPreview.hidden) {
      placeMathPreview(anchorRect, math.display, bottomRect);
    }
  });
}

type AssistUpdateOptions = {
  snippets?: boolean;
  mathPreview?: boolean;
  cursor?: boolean;
  toc?: boolean;
};

function scheduleAssistUpdate(options: AssistUpdateOptions = {}): void {
  if (!editorSurfaceVisible()) {
    snippetScanRequested = false;
    mathPreviewUpdateRequested = false;
    vimCursorUpdateRequested = false;
    tocUpdateRequested = false;
    window.cancelAnimationFrame(assistFrame);
    return;
  }
  const explicit = Object.keys(options).length > 0;
  snippetScanRequested = snippetScanRequested || options.snippets === true;
  mathPreviewUpdateRequested = mathPreviewUpdateRequested || options.mathPreview === true;
  vimCursorUpdateRequested = vimCursorUpdateRequested || (explicit ? options.cursor === true : true);
  tocUpdateRequested = tocUpdateRequested || options.toc === true;
  window.cancelAnimationFrame(assistFrame);
  assistFrame = window.requestAnimationFrame(() => {
    const shouldScanSnippets = snippetScanRequested;
    const shouldUpdateMathPreview = mathPreviewUpdateRequested;
    const shouldUpdateVimCursor = vimCursorUpdateRequested;
    const shouldUpdateToc = tocUpdateRequested;
    snippetScanRequested = false;
    mathPreviewUpdateRequested = false;
    vimCursorUpdateRequested = false;
    tocUpdateRequested = false;

    const needsCursorContext = vim.mode() === "insert" && (
      shouldScanSnippets
      || shouldUpdateMathPreview
      || !snippetPopup.hidden
      || !mathPreview.hidden
    );
    const ctx = needsCursorContext ? editor.cursorContext(!snippetPopup.hidden ? 640 : 320) : null;
    if (shouldUpdateVimCursor || ctx) updateVimCursor(vimCursor, editor, vim.mode(), ctx?.rect);
    if (shouldUpdateToc) updateFloatingToc();
    if (vim.mode() !== "insert") {
      hideSnippetPopup();
      hideMathPreview();
      return;
    }
    if (ctx) {
      if (shouldScanSnippets || !snippetPopup.hidden) updateSnippetPopup(ctx);
      updateMathPreview(ctx, shouldUpdateMathPreview);
    }
  });
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
  const key = String(body.key || "");
  if (!key) return false;
  const hostKey: VimLiteKey = {
    key,
    ctrlKey: Boolean(body.ctrlKey),
    metaKey: Boolean(body.metaKey),
    altKey: Boolean(body.altKey),
    shiftKey: Boolean(body.shiftKey),
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
  if (vim.mode() !== "insert" || hostKey.ctrlKey || hostKey.metaKey || hostKey.altKey) return false;
  if (key === "Tab") {
    if (hostKey.shiftKey) return jumpSnippetTabstopBack();
    return jumpSnippetTabstop() || expandSnippetAtCursor();
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
  };
  const command = String(body.command || "").trim().toLowerCase();
  if (!command) return false;

  switch (command) {
    case "key":
      return runHostKey(body as Record<string, unknown>);
    case "pause":
      document.documentElement.classList.add("aaronnote-paused");
      return true;
    case "resume":
      document.documentElement.classList.remove("aaronnote-paused");
      return true;
    case "save":
      void save();
      return true;
    case "focus":
      editor.focus();
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
  snippetSuppressedPrefix = event.key === "Escape" ? snippetSuppressedPrefix : "";
  if (handleSnippetPopupKey(event)) {
    event.stopPropagation();
    return;
  }
  if (event.key === "Escape" && !event.metaKey && !event.ctrlKey && !event.altKey) {
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
  if (vim.mode() === "insert" && event.key === "Tab" && !event.metaKey && !event.ctrlKey && !event.altKey) {
    const handled = event.shiftKey
      ? jumpSnippetTabstopBack()
      : jumpSnippetTabstop() || expandSnippetAtCursor();
    if (handled) {
      event.preventDefault();
      event.stopPropagation();
      return;
    }
  }
  if (vim.handleKeyDown(event)) {
    scheduleAssistUpdate({ cursor: true, toc: true });
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
    scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true });
    event.stopPropagation();
  }
}, true);
document.addEventListener("selectionchange", () => scheduleAssistUpdate({ snippets: true, mathPreview: true, cursor: true, toc: true }));
window.addEventListener("resize", () => scheduleAssistUpdate({ mathPreview: true, cursor: true, toc: true }));
window.addEventListener("scroll", () => scheduleAssistUpdate({ mathPreview: true, cursor: true, toc: true }), true);
document.addEventListener("aaronnote:open-url", (event) => {
  const custom = event as CustomEvent<{ href?: string; newWindow?: boolean }>;
  const href = custom.detail?.href;
  if (!href) return;
  event.preventDefault();
  openExternalUrl(href, { newWindow: custom.detail?.newWindow === true });
});
window.addEventListener("aaronnote:open-file", (event) => {
  const detail = (event as CustomEvent<{ file?: string }>).detail;
  void openFile(detail?.file);
});
window.addEventListener("aaronnote:command", (event) => {
  runHostCommand((event as CustomEvent<unknown>).detail);
});
document.addEventListener("visibilitychange", () => {
  if (document.hidden) {
    document.documentElement.classList.add("aaronnote-paused");
  } else {
    document.documentElement.classList.remove("aaronnote-paused");
  }
});
window.addEventListener("pagehide", () => {
  if (currentFile && revision !== savedRevision) api.notes.saveKeepalive(saveBody());
});

void openInitialFile();
