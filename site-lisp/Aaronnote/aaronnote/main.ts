import "prosemirror-view/style/prosemirror.css";
import "katex/dist/katex.min.css";
import "../src/styles/widgets.css";
import "../src/styles/theme-typora.css";
import "./style.css";

import katex from "katex";
import { createEditor } from "../src/lib.ts";
import { createVimLite, type VimLiteMode } from "./vim-lite.ts";

type NoteSummary = {
  id?: string;
  title?: string;
  file?: string;
  tags?: string[];
};

type SnippetSummary = {
  key?: string;
  name?: string;
  mode?: string;
  group?: string;
  body?: string;
  source?: string;
};

type Inbound =
  | { type: "open"; file?: string; title?: string; content?: string; mode?: "markdown" | "source"; notes?: NoteSummary[]; snippets?: SnippetSummary[] }
  | { type: "saved"; ok?: boolean; message?: string; file?: string }
  | { type: "notes"; notes?: NoteSummary[] }
  | { type: "snippets"; snippets?: SnippetSummary[] };

const params = new URLSearchParams(window.location.search);
const emacsPort = params.get("emacsPort") || "";
const token = params.get("token") || "";

const root = document.querySelector<HTMLDivElement>("#app")!;
root.innerHTML = `
  <main class="aaronnote-shell">
    <header class="aaronnote-toolbar">
      <div class="aaronnote-title">
        <strong>Aaronnote</strong>
        <span data-file-label>No file</span>
      </div>
      <div class="aaronnote-actions">
        <button type="button" data-action="toggle-source">Source</button>
        <button type="button" data-action="open-emacs">Open in Emacs</button>
        <button type="button" data-action="save">Save</button>
      </div>
      <span class="aaronnote-vim-mode" data-vim-mode>INSERT</span>
      <span class="aaronnote-status" data-status>Connecting</span>
    </header>
    <section class="aaronnote-body">
      <aside class="aaronnote-sidebar">
        <div class="aaronnote-sidebar-section">
          <input data-note-filter type="search" placeholder="Filter notes" />
          <div data-note-list class="aaronnote-note-list"></div>
        </div>
        <div class="aaronnote-sidebar-section">
          <input data-snippet-filter type="search" placeholder="Filter snippets" />
          <div data-snippet-list class="aaronnote-note-list"></div>
        </div>
      </aside>
      <section class="aaronnote-editor" id="editor"></section>
    </section>
  </main>
`;

const host = document.querySelector<HTMLElement>("#editor")!;
const statusEl = document.querySelector<HTMLElement>("[data-status]")!;
const vimModeEl = document.querySelector<HTMLElement>("[data-vim-mode]")!;
const fileLabel = document.querySelector<HTMLElement>("[data-file-label]")!;
const noteList = document.querySelector<HTMLElement>("[data-note-list]")!;
const noteFilter = document.querySelector<HTMLInputElement>("[data-note-filter]")!;
const snippetList = document.querySelector<HTMLElement>("[data-snippet-list]")!;
const snippetFilter = document.querySelector<HTMLInputElement>("[data-snippet-filter]")!;
const snippetPopup = document.createElement("div");
snippetPopup.className = "aaronnote-snippet-popup";
snippetPopup.hidden = true;
document.body.appendChild(snippetPopup);

const mathPreview = document.createElement("div");
mathPreview.className = "aaronnote-math-preview";
mathPreview.hidden = true;
document.body.appendChild(mathPreview);

let currentFile = "";
let currentMode: "markdown" | "source" = "markdown";
let saveTimer = 0;
let ws: WebSocket | null = null;
let notes: NoteSummary[] = [];
let snippets: SnippetSummary[] = [];
let receivedOpen = false;
let assistFrame = 0;
let vimMode: VimLiteMode = "insert";
let snippetPopupItems: SnippetSummary[] = [];
let snippetPopupIndex = 0;
let snippetDeleteBefore = 0;
let snippetSuppressedPrefix = "";

const demoSnippets: SnippetSummary[] = [
  {
    key: "eq",
    name: "Display equation",
    mode: "markdown",
    group: "Aaronnote demo",
    body: "$$\n${1:E = mc^2}\n$$",
  },
  {
    key: "im",
    name: "Inline math",
    mode: "markdown",
    group: "Aaronnote demo",
    body: "$${1:x^2 + y^2}$",
  },
  {
    key: "note",
    name: "Note block",
    mode: "markdown",
    group: "Aaronnote demo",
    body: "## ${1:Title}\n\n${2:Body}",
  },
];

function scratchStatus(): string {
  return emacsPort && token ? "Scratch" : "Demo only";
}

const editor = createEditor(host, {
  initialContent: "",
  onChange: () => {
    scheduleAssistUpdate();
    if (!currentFile) {
      setStatus(scratchStatus());
      return;
    }
    setStatus("Dirty");
    window.clearTimeout(saveTimer);
    saveTimer = window.setTimeout(() => save(), 900);
  },
});

const vim = createVimLite(editor, host, {
  onModeChange(mode) {
    vimMode = mode;
    root.dataset.vimMode = mode;
    vimModeEl.textContent = mode.toUpperCase();
    if (mode === "normal") {
      hideSnippetPopup();
      mathPreview.hidden = true;
      setStatus("NORMAL");
    } else {
      setStatus(currentFile ? "INSERT" : scratchStatus());
      scheduleAssistUpdate();
    }
  },
});

function setStatus(text: string): void {
  statusEl.textContent = text;
}

function send(payload: Record<string, unknown>): void {
  const msg = JSON.stringify({ token, ...payload });
  if (ws?.readyState === WebSocket.OPEN) ws.send(msg);
}

function save(): void {
  if (!currentFile) {
    setStatus(scratchStatus());
    return;
  }
  send({
    type: "save",
    file: currentFile,
    content: editor.getMarkdown(),
    mode: editor.isSourceMode() ? "source" : "markdown",
  });
  setStatus("Saving");
}

function expandSnippetBody(snippet: SnippetSummary): string {
  const body = snippet.body ?? "";
  const values = new Map<string, string>();
  let expanded = body.replace(/\$\{(\d+)\|([^}]*)\|\}/g, (_m, index: string, choices: string) => {
    if (!values.has(index)) {
      const options = choices.split(",").map((x) => x.trim()).filter(Boolean);
      values.set(index, options[0] ?? "");
    }
    return values.get(index) ?? "";
  });
  expanded = expanded.replace(/\$\{(\d+):([^}]*)\}/g, (_m, index: string, fallback: string) => {
    if (!values.has(index)) {
      values.set(index, fallback);
    }
    return values.get(index) ?? "";
  });
  expanded = expanded.replace(/\$\{(\d+)\}/g, (_m, index: string) => {
    if (!values.has(index)) {
      values.set(index, "");
    }
    return values.get(index) ?? "";
  });
  expanded = expanded.replace(/\$(\d+)/g, (_m, index: string) => {
    if (index === "0") return "";
    if (!values.has(index)) {
      values.set(index, "");
    }
    return values.get(index) ?? "";
  });
  return expanded.replace(/\$0/g, "");
}

function insertSnippet(snippet: SnippetSummary, deleteBefore = 0): void {
  const text = expandSnippetBody(snippet);
  if (!text) return;
  editor.insertText(text, deleteBefore);
  setStatus(`Inserted ${snippet.key || snippet.name || "snippet"}`);
  scheduleAssistUpdate();
}

function snippetLabel(snippet: SnippetSummary): string {
  return snippet.key || snippet.name || "snippet";
}

function snippetDetail(snippet: SnippetSummary): string {
  return [snippet.name, snippet.mode, snippet.group].filter(Boolean).join(" / ");
}

function snippetScore(snippet: SnippetSummary, query: string): number {
  const key = (snippet.key ?? "").toLowerCase();
  const name = (snippet.name ?? "").toLowerCase();
  const mode = (snippet.mode ?? "").toLowerCase();
  const group = (snippet.group ?? "").toLowerCase();
  if (key === query) return 0;
  if (key.startsWith(query)) return 1;
  if (name.startsWith(query)) return 2;
  if (key.includes(query)) return 3;
  if (name.includes(query)) return 4;
  if (mode.includes(query) || group.includes(query)) return 5;
  return Number.POSITIVE_INFINITY;
}

function matchingSnippets(prefix: string): SnippetSummary[] {
  const query = prefix.toLowerCase();
  return snippets
    .map((snippet) => ({ snippet, score: snippetScore(snippet, query) }))
    .filter((item) => Number.isFinite(item.score))
    .sort((a, b) => {
      if (a.score !== b.score) return a.score - b.score;
      return snippetLabel(a.snippet).localeCompare(snippetLabel(b.snippet));
    })
    .slice(0, 10)
    .map((item) => item.snippet);
}

function hideSnippetPopup(): void {
  snippetPopup.hidden = true;
  snippetPopupItems = [];
}

function placeFloating(el: HTMLElement, rect: { left: number; top: number; bottom: number } | null, width = 320): void {
  if (!rect) {
    el.hidden = true;
    return;
  }
  const margin = 8;
  const left = Math.min(Math.max(margin, rect.left), Math.max(margin, window.innerWidth - width - margin));
  let top = rect.bottom + 8;
  if (top + 240 > window.innerHeight) top = Math.max(margin, rect.top - 220);
  el.style.left = `${left}px`;
  el.style.top = `${top}px`;
  el.style.width = `${Math.min(width, window.innerWidth - margin * 2)}px`;
}

function renderSnippetPopup(prefix: string, rect: { left: number; top: number; bottom: number } | null): void {
  snippetPopup.innerHTML = "";
  snippetPopupItems.forEach((snippet, index) => {
    const button = document.createElement("button");
    button.type = "button";
    button.className = index === snippetPopupIndex
      ? "aaronnote-snippet-option is-active"
      : "aaronnote-snippet-option";
    const key = document.createElement("span");
    key.className = "aaronnote-snippet-option-key";
    key.textContent = snippetLabel(snippet);
    const detail = document.createElement("span");
    detail.className = "aaronnote-snippet-option-detail";
    detail.textContent = snippetDetail(snippet);
    button.append(key, detail);
    button.addEventListener("mousedown", (event) => {
      event.preventDefault();
      snippetPopupIndex = index;
      chooseSnippetPopupItem();
    });
    snippetPopup.appendChild(button);
  });
  snippetPopup.dataset.prefix = prefix;
  snippetPopup.hidden = false;
  placeFloating(snippetPopup, rect);
}

function updateSnippetPopup(ctx: ReturnType<typeof editor.cursorContext>): void {
  const active = document.activeElement;
  if (!active || !host.contains(active)) {
    hideSnippetPopup();
    return;
  }
  const match = ctx.before.match(/([A-Za-z0-9_:/;.+\\-]{1,40})$/);
  const prefix = match?.[1] ?? "";
  if (!prefix || prefix === snippetSuppressedPrefix) {
    hideSnippetPopup();
    return;
  }
  const matches = matchingSnippets(prefix);
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

function handleSnippetPopupKey(event: KeyboardEvent): boolean {
  if (snippetPopup.hidden) return false;
  if (event.key === "ArrowDown") {
    event.preventDefault();
    snippetPopupIndex = (snippetPopupIndex + 1) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorContext().rect);
    return true;
  }
  if (event.key === "ArrowUp") {
    event.preventDefault();
    snippetPopupIndex = (snippetPopupIndex + snippetPopupItems.length - 1) % snippetPopupItems.length;
    renderSnippetPopup(snippetPopup.dataset.prefix ?? "", editor.cursorContext().rect);
    return true;
  }
  if (event.key === "Enter" || event.key === "Tab") {
    event.preventDefault();
    chooseSnippetPopupItem();
    return true;
  }
  if (event.key === "Escape") {
    event.preventDefault();
    snippetSuppressedPrefix = snippetPopup.dataset.prefix ?? "";
    hideSnippetPopup();
    return true;
  }
  return false;
}

function isEscaped(src: string, pos: number): boolean {
  let count = 0;
  for (let i = pos - 1; i >= 0 && src[i] === "\\"; i--) count++;
  return count % 2 === 1;
}

function findMathClose(src: string, delimiter: "$" | "$$", from: number): number {
  for (let i = from; i < src.length; i++) {
    if (delimiter === "$" && src[i] === "\n") return -1;
    if (src.slice(i, i + delimiter.length) === delimiter && !isEscaped(src, i)) return i;
  }
  return -1;
}

function mathAtCursor(ctx: ReturnType<typeof editor.cursorContext>): { tex: string; display: boolean } | null {
  const src = ctx.before + ctx.after;
  const cursor = ctx.before.length;
  for (let i = 0; i < src.length; i++) {
    if (src[i] !== "$" || isEscaped(src, i)) continue;
    const delimiter: "$" | "$$" = src[i + 1] === "$" ? "$$" : "$";
    const openFrom = i;
    const openTo = i + delimiter.length;
    if (openTo > cursor) break;
    const closeFrom = findMathClose(src, delimiter, openTo);
    if (closeFrom >= 0) {
      const closeTo = closeFrom + delimiter.length;
      if (cursor > openFrom && cursor < closeTo) {
        return {
          tex: src.slice(openTo, closeFrom),
          display: delimiter === "$$",
        };
      }
      i = closeTo - 1;
      continue;
    }
    if (cursor >= openTo && openFrom < cursor) {
      const tex = src.slice(openTo, cursor);
      if (delimiter === "$" && tex.includes("\n")) return null;
      return { tex, display: delimiter === "$$" };
    }
  }
  return null;
}

function updateMathPreview(ctx: ReturnType<typeof editor.cursorContext>): void {
  const math = mathAtCursor(ctx);
  if (!math || math.tex.trim().length === 0) {
    mathPreview.hidden = true;
    return;
  }
  mathPreview.innerHTML = "";
  mathPreview.classList.toggle("is-display", math.display);
  try {
    katex.render(math.tex.trim(), mathPreview, {
      displayMode: math.display,
      throwOnError: false,
      strict: "ignore",
    });
  } catch {
    mathPreview.textContent = math.tex;
  }
  mathPreview.hidden = false;
  placeFloating(mathPreview, ctx.rect, math.display ? 420 : 300);
}

function scheduleAssistUpdate(): void {
  window.cancelAnimationFrame(assistFrame);
  assistFrame = window.requestAnimationFrame(() => {
    if (vimMode === "normal") {
      hideSnippetPopup();
      mathPreview.hidden = true;
      return;
    }
    const ctx = editor.cursorContext(1600);
    updateSnippetPopup(ctx);
    updateMathPreview(ctx);
  });
}

function renderSnippets(): void {
  const query = snippetFilter.value.trim().toLowerCase();
  const shown = snippets
    .filter((snippet) => {
      const haystack = `${snippet.key ?? ""} ${snippet.name ?? ""} ${snippet.mode ?? ""} ${snippet.group ?? ""}`.toLowerCase();
      return !query || haystack.includes(query);
    })
    .slice(0, 120);

  snippetList.innerHTML = "";
  if (shown.length === 0) {
    snippetList.innerHTML = `<div class="aaronnote-empty">No snippets</div>`;
    return;
  }
  for (const snippet of shown) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "aaronnote-note aaronnote-snippet";
    button.textContent = snippetLabel(snippet);
    button.title = [snippet.name, snippet.mode, snippet.source].filter(Boolean).join("\n");
    button.addEventListener("click", () => insertSnippet(snippet));
    snippetList.appendChild(button);
  }
}

function renderNotes(): void {
  const query = noteFilter.value.trim().toLowerCase();
  const shown = notes
    .filter((note) => {
      const haystack = `${note.title ?? ""} ${note.id ?? ""} ${note.file ?? ""} ${(note.tags ?? []).join(" ")}`.toLowerCase();
      return !query || haystack.includes(query);
    })
    .slice(0, 80);

  noteList.innerHTML = "";
  if (shown.length === 0) {
    noteList.innerHTML = `<div class="aaronnote-empty">No notes</div>`;
    return;
  }
  for (const note of shown) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "aaronnote-note";
    button.textContent = note.title || note.id || note.file || "Untitled";
    button.title = note.file || "";
    button.addEventListener("click", () => {
      if (note.file) send({ type: "open-file", file: note.file });
    });
    noteList.appendChild(button);
  }
}

function applyOpen(msg: Extract<Inbound, { type: "open" }>): void {
  receivedOpen = true;
  currentFile = msg.file || "";
  currentMode = msg.mode === "source" ? "source" : "markdown";
  fileLabel.textContent = currentFile || "Scratch";

  if (Array.isArray(msg.notes)) {
    notes = msg.notes;
    renderNotes();
  }
  if (Array.isArray(msg.snippets)) {
    snippets = msg.snippets.length > 0 ? msg.snippets : demoSnippets;
    renderSnippets();
  }

  if (currentMode === "source" && !editor.isSourceMode()) editor.toggleSource();
  if (currentMode === "markdown" && editor.isSourceMode()) editor.toggleSource();
  editor.setMarkdown(msg.content ?? "");
  editor.focus();
  vim.setMode("insert");
  setStatus(currentMode === "source" ? "Source mode" : "Ready");
  scheduleAssistUpdate();
}

function handleInbound(raw: string): void {
  let msg: Inbound;
  try {
    msg = JSON.parse(raw) as Inbound;
  } catch {
    return;
  }
  if (msg.type === "open") applyOpen(msg);
  if (msg.type === "saved") setStatus(msg.ok ? "Saved" : msg.message || "Save failed");
  if (msg.type === "notes" && Array.isArray(msg.notes)) {
    notes = msg.notes;
    renderNotes();
  }
  if (msg.type === "snippets" && Array.isArray(msg.snippets)) {
    snippets = msg.snippets.length > 0 ? msg.snippets : demoSnippets;
    renderSnippets();
    scheduleAssistUpdate();
  }
}

function applyDemoOpen(): void {
  if (receivedOpen) return;
  currentFile = "";
  currentMode = "markdown";
  fileLabel.textContent = emacsPort && token ? "Scratch" : "Demo (no Emacs)";
  notes = [];
  snippets = demoSnippets;
  renderNotes();
  renderSnippets();
  if (editor.isSourceMode()) editor.toggleSource();
  editor.setMarkdown([
    "# Aaronnote Demo",
    "",
    "Inline math: $x^2 + y^2$.",
    "",
    "$$",
    "E = mc^2",
    "$$",
    "",
  ].join("\n"));
  editor.focus();
  vim.setMode("insert");
  setStatus(emacsPort && token ? "Demo" : "Demo only");
  scheduleAssistUpdate();
}

function connect(): void {
  if (!emacsPort || !token) {
    setStatus("Missing Emacs bridge");
    return;
  }
  ws = new WebSocket(`ws://127.0.0.1:${emacsPort}/`);
  ws.addEventListener("open", () => {
    setStatus("Connected");
    send({ type: "hello" });
  });
  ws.addEventListener("message", (event) => handleInbound(String(event.data)));
  ws.addEventListener("close", () => {
    setStatus("Disconnected");
    window.setTimeout(connect, 1500);
  });
  ws.addEventListener("error", () => {
    try {
      ws?.close();
    } catch {}
  });
}

document.addEventListener("keydown", (event) => {
  if (handleSnippetPopupKey(event)) {
    event.stopPropagation();
    return;
  }
  if (vim.handleKeyDown(event)) {
    event.stopPropagation();
    return;
  }
  const isMac = /Mac/.test(navigator.platform);
  if (event.key.toLowerCase() === "s" && (isMac ? event.metaKey : event.ctrlKey)) {
    event.preventDefault();
    event.stopPropagation();
    save();
  }
}, true);

document.querySelector("[data-action='toggle-source']")?.addEventListener("click", () => {
  editor.toggleSource();
  vim.setMode("insert");
  currentMode = editor.isSourceMode() ? "source" : "markdown";
  setStatus(currentMode === "source" ? "Source mode" : "Ready");
});

document.querySelector("[data-action='open-emacs']")?.addEventListener("click", () => {
  if (currentFile) send({ type: "open-file", file: currentFile });
});

document.querySelector("[data-action='save']")?.addEventListener("click", save);
noteFilter.addEventListener("input", renderNotes);
snippetFilter.addEventListener("input", renderSnippets);
document.addEventListener("keyup", (event) => {
  if (event.key !== "Escape") snippetSuppressedPrefix = "";
  scheduleAssistUpdate();
});
document.addEventListener("selectionchange", scheduleAssistUpdate);
document.addEventListener("mouseup", scheduleAssistUpdate);
window.addEventListener("resize", scheduleAssistUpdate);
window.addEventListener("scroll", scheduleAssistUpdate, true);

connect();
window.setTimeout(applyDemoOpen, 1200);
