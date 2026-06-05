import "../src/styles/widgets.css";
import "../src/styles/theme-typora.css";
import "./style.css";

import { createEditor } from "../src/lib.ts";
import { setupCopilot } from "../src/copilot/index.ts";
import { api } from "./api-client.ts";

const root = document.querySelector<HTMLElement>("#app");
if (!root) throw new Error("Missing #app");

root.innerHTML = `
  <main class="aaronnote-focused-shell">
    <header class="aaronnote-focused-bar">
      <strong data-file>AaronNote</strong>
      <span data-status>Opening...</span>
      <button type="button" data-source>Source</button>
      <button type="button" data-save>Save</button>
    </header>
    <section class="aaronnote-focused-editor" data-editor></section>
  </main>
`;

const host = root.querySelector<HTMLElement>("[data-editor]")!;
const fileLabel = root.querySelector<HTMLElement>("[data-file]")!;
const statusLabel = root.querySelector<HTMLElement>("[data-status]")!;
const sourceButton = root.querySelector<HTMLButtonElement>("[data-source]")!;
const saveButton = root.querySelector<HTMLButtonElement>("[data-save]")!;

let currentFile = "";
let currentMtimeMs = 0;
let revision = 0;
let savedRevision = 0;
let applyingContent = false;
let saveTimer = 0;
const clientId = globalThis.crypto?.randomUUID?.() ?? `${Date.now()}-${Math.random().toString(16).slice(2)}`;
const changeHandlers = new Set<() => void>();

window.AaronnoteCurrentFile = () => currentFile;

function setStatus(message: string): void {
  statusLabel.textContent = message;
}

function updateTitle(): void {
  const name = currentFile.split(/[\\/]/).at(-1) || "AaronNote";
  fileLabel.textContent = name;
  document.title = revision === savedRevision ? name : `* ${name}`;
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
    scheduleSave();
  },
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

async function openInitialFile(): Promise<void> {
  const file = new URLSearchParams(window.location.search).get("file") || undefined;
  try {
    const opened = await api.notes.bootstrap(file);
    currentFile = String(opened.file || file || "");
    currentMtimeMs = Number(opened.mtimeMs) || 0;
    applyingContent = true;
    editor.setMarkdown(String(opened.content || ""));
    applyingContent = false;
    revision = 0;
    savedRevision = 0;
    if (opened.mode === "source" && !editor.isSourceMode()) editor.toggleSource();
    updateTitle();
    setStatus(currentFile ? "Ready" : "Scratch");
    editor.focus();
  } catch (error) {
    applyingContent = false;
    setStatus(error instanceof Error ? error.message : "Open failed");
  }
}

setupCopilot({
  editor,
  host,
  currentFile: () => currentFile,
  vimMode: () => "insert",
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
  jumpSnippetNext: () => false,
  jumpSnippetPrevious: () => false,
  forwardDelimiter: () => false,
  backwardDelimiter: () => false,
});

sourceButton.addEventListener("click", () => {
  editor.toggleSource();
  sourceButton.classList.toggle("is-active", editor.isSourceMode());
  editor.focus();
});
saveButton.addEventListener("click", () => void save());
document.addEventListener("keydown", (event) => {
  if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === "s") {
    event.preventDefault();
    void save();
  }
});
window.addEventListener("pagehide", () => {
  if (currentFile && revision !== savedRevision) api.notes.saveKeepalive(saveBody());
});

void openInitialFile();
