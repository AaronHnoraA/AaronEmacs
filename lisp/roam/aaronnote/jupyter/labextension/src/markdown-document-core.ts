import { EditorView } from "@codemirror/view";

import type {
  AaronnoteMarkdownCellEditor,
  AaronnoteMarkdownCellEditorFactory,
} from "./markdown-cell-core";

export type AaronnoteMarkdownDocumentBridgeOptions = {
  document?: Document;
  createEditor: AaronnoteMarkdownCellEditorFactory;
  scanIntervalMs?: number;
  onSizeChange?: (size: number) => void;
};

export type AaronnoteMarkdownDocumentBridge = {
  scan(): number;
  dispose(): void;
  size(): number;
};

const FILE_EDITOR_SELECTOR = ".jp-FileEditor";
const HOST_CLASS = "aaronnote-jupyter-document-host";
const ACTIVE_CLASS = "aaronnote-jupyter-document-active";
const HIDDEN_SOURCE_CLASS = "aaronnote-jupyter-source-hidden";

function isMarkdownName(name: string): boolean {
  return /(?:\.md|\.markdown)$/i.test(name.trim()) || /^README$/i.test(name.trim());
}

function documentName(editor: HTMLElement): string | null {
  const documentWidget = editor.closest<HTMLElement>(".jp-MainAreaWidget.jp-Document");
  const tabId = documentWidget?.getAttribute("aria-labelledby");
  if (!tabId) return null;
  const tab = editor.ownerDocument.getElementById(tabId);
  return tab?.textContent?.trim() || null;
}

function markdownFileEditors(doc: Document): HTMLElement[] {
  return Array.from(doc.querySelectorAll<HTMLElement>(FILE_EDITOR_SELECTOR))
    .filter((editor) => editor.isConnected && isMarkdownName(documentName(editor) ?? ""));
}

function sourceView(editor: HTMLElement): { element: HTMLElement; view: EditorView } | null {
  const element = Array.from(editor.querySelectorAll<HTMLElement>(".cm-editor"))
    .find((candidate) => !candidate.closest(`.${HOST_CLASS}`));
  if (!element) return null;
  try {
    const view = EditorView.findFromDOM(element);
    return view ? { element, view } : null;
  } catch {
    return null;
  }
}

class MarkdownDocumentAdapter {
  private readonly container: HTMLElement;
  private readonly sourceElement: HTMLElement;
  private readonly sourceView: EditorView;
  private readonly host: HTMLElement;
  private readonly editor: AaronnoteMarkdownCellEditor;
  private readonly previousDisplay: string;
  private disposed = false;
  private applyingFromAaronnote = false;
  private applyingFromJupyter = false;
  private lastSourceMarkdown: string;

  constructor(
    container: HTMLElement,
    sourceElement: HTMLElement,
    view: EditorView,
    createEditor: AaronnoteMarkdownCellEditorFactory,
  ) {
    this.container = container;
    this.sourceElement = sourceElement;
    this.sourceView = view;
    this.previousDisplay = sourceElement.style.display;
    this.lastSourceMarkdown = view.state.doc.toString();
    this.host = container.ownerDocument.createElement("section");
    this.host.className = HOST_CLASS;
    this.host.setAttribute("aria-label", "Aaronnote Markdown document editor");

    sourceElement.classList.add(HIDDEN_SOURCE_CLASS);
    sourceElement.style.display = "none";
    container.classList.add(ACTIVE_CLASS);
    container.append(this.host);

    this.editor = createEditor(this.host, {
      initialContent: this.lastSourceMarkdown,
      onChange: (markdown) => this.applyAaronnoteChange(markdown),
    });
  }

  isCurrent(): boolean {
    return !this.disposed
      && this.container.isConnected
      && this.sourceElement.isConnected
      && this.sourceView.dom.isConnected
      && isMarkdownName(documentName(this.container) ?? "");
  }

  refreshFromSource(): void {
    if (this.disposed || this.applyingFromAaronnote) return;
    const markdown = this.sourceView.state.doc.toString();
    if (markdown === this.lastSourceMarkdown || markdown === this.editor.getMarkdown()) {
      this.lastSourceMarkdown = markdown;
      return;
    }

    this.applyingFromJupyter = true;
    try {
      this.editor.setMarkdown(markdown, { history: "reset" });
      this.lastSourceMarkdown = markdown;
    } finally {
      this.applyingFromJupyter = false;
    }
  }

  dispose(): void {
    if (this.disposed) return;
    this.disposed = true;
    this.container.classList.remove(ACTIVE_CLASS);
    this.sourceElement.classList.remove(HIDDEN_SOURCE_CLASS);
    this.sourceElement.style.display = this.previousDisplay;
    this.editor.destroy();
    this.host.remove();
  }

  private applyAaronnoteChange(markdown: string): void {
    if (this.disposed || this.applyingFromJupyter) return;
    this.applyingFromAaronnote = true;
    try {
      const current = this.sourceView.state.doc.toString();
      if (current !== markdown) {
        this.sourceView.dispatch({
          changes: { from: 0, to: this.sourceView.state.doc.length, insert: markdown },
        });
      }
      this.lastSourceMarkdown = markdown;
    } finally {
      this.applyingFromAaronnote = false;
    }
  }
}

export function installAaronnoteMarkdownDocumentBridge(
  options: AaronnoteMarkdownDocumentBridgeOptions,
): AaronnoteMarkdownDocumentBridge {
  const doc = options.document ?? document;
  const win = doc.defaultView ?? window;
  const adapters = new Map<HTMLElement, MarkdownDocumentAdapter>();
  let disposed = false;
  let scanQueued = false;

  const scanNow = (): number => {
    if (disposed) return adapters.size;
    const editors = new Set(markdownFileEditors(doc));

    for (const [container, adapter] of adapters) {
      if (!editors.has(container) || !adapter.isCurrent()) {
        adapter.dispose();
        adapters.delete(container);
      } else {
        adapter.refreshFromSource();
      }
    }

    for (const container of editors) {
      if (adapters.has(container)) continue;
      const source = sourceView(container);
      if (!source) continue;
      adapters.set(container, new MarkdownDocumentAdapter(
        container,
        source.element,
        source.view,
        options.createEditor,
      ));
    }

    options.onSizeChange?.(adapters.size);
    return adapters.size;
  };

  const scheduleScan = (): void => {
    if (disposed || scanQueued) return;
    scanQueued = true;
    win.requestAnimationFrame(() => {
      scanQueued = false;
      scanNow();
    });
  };

  const observer = new win.MutationObserver(scheduleScan);
  observer.observe(doc.body ?? doc.documentElement, {
    subtree: true,
    childList: true,
    attributes: true,
    attributeFilter: ["aria-labelledby", "class"],
  });

  const intervalMs = options.scanIntervalMs ?? 750;
  const interval = intervalMs > 0
    ? win.setInterval(scanNow, intervalMs)
    : undefined;

  scanNow();
  Object.assign(globalThis, {
    __aaronnoteJupyterMarkdownDocuments: {
      installed: true,
      version: "0.2.0",
      size: () => adapters.size,
    },
  });

  return {
    scan: scanNow,
    size: () => adapters.size,
    dispose: () => {
      if (disposed) return;
      disposed = true;
      observer.disconnect();
      if (interval !== undefined) win.clearInterval(interval);
      for (const adapter of adapters.values()) adapter.dispose();
      adapters.clear();
      options.onSizeChange?.(0);
    },
  };
}
