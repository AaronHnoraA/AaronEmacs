import "../src/styles/widgets.css";
import "../src/styles/theme-typora.css";
import "./style.css";

import { createEditor, type EditorCommand } from "../src/lib.ts";
import { setupCopilot } from "../src/copilot/index.ts";
import { api } from "./api-client.ts";
import { createVimLite, type VimLiteKey, type VimLiteMode } from "./vim-lite.ts";

const root = document.querySelector<HTMLElement>("#app");
if (!root) throw new Error("Missing #app");

root.innerHTML = `
  <main class="aaronnote-focused-shell">
    <header class="aaronnote-focused-bar">
      <strong data-file>AaronNote</strong>
      <span data-vim-mode>INSERT</span>
      <span data-status>Opening...</span>
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

const vim = createVimLite(editor, host, {
  onModeChange: updateModeLabel,
  onUndo: () => editor.undo(),
  onRedo: () => editor.redo(),
});
updateModeLabel(vim.mode());

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

function applyOpenedNote(opened: Awaited<ReturnType<typeof api.notes.bootstrap>>, fallbackFile?: string): void {
  currentFile = String(opened.file || fallbackFile || "");
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
  vim.setMode("insert");
  updateTitle();
  setStatus(currentFile ? "Ready" : "Scratch");
  editor.focus();
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
  const file = new URLSearchParams(window.location.search).get("file") || undefined;
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
  jumpSnippetNext: () => false,
  jumpSnippetPrevious: () => false,
  forwardDelimiter: () => false,
  backwardDelimiter: () => false,
});

function toggleSourceMode(): void {
  editor.toggleSource();
  sourceButton.classList.toggle("is-active", editor.isSourceMode());
  editor.focus();
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
  if (vim.handleKey(hostKey)) return true;
  if (vim.mode() !== "insert" || hostKey.ctrlKey || hostKey.metaKey || hostKey.altKey) return false;
  if (key === "Backspace" || key === "Delete") return deleteHostKeyText(key);
  return insertHostKeyText(key, typeof body.text === "string" ? body.text : undefined);
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

sourceButton.addEventListener("click", toggleSourceMode);
saveButton.addEventListener("click", () => void save());
document.addEventListener("keydown", (event) => {
  if (vim.handleKeyDown(event)) {
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
    event.stopPropagation();
  }
}, true);
window.addEventListener("aaronnote:open-file", (event) => {
  const detail = (event as CustomEvent<{ file?: string }>).detail;
  void openFile(detail?.file);
});
window.addEventListener("aaronnote:command", (event) => {
  runHostCommand((event as CustomEvent<unknown>).detail);
});
window.addEventListener("pagehide", () => {
  if (currentFile && revision !== savedRevision) api.notes.saveKeepalive(saveBody());
});

void openInitialFile();
