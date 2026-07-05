import { EditorView } from "@codemirror/view";

export type AaronnoteMarkdownCellEditor = {
  getMarkdown(): string;
  setMarkdown(markdown: string, options?: { history?: "record" | "skip" | "reset" }): void;
  focus(): void;
  destroy(): void;
};

export type AaronnoteMarkdownCellEditorFactory = (
  host: HTMLElement,
  options: {
    initialContent?: string;
    onChange?: (markdown: string) => void;
    onFocus?: () => void;
    onBlur?: () => void;
    readOnly?: boolean;
  },
) => AaronnoteMarkdownCellEditor;

export type AaronnoteMarkdownCellBridgeOptions = {
  document?: Document;
  createEditor: AaronnoteMarkdownCellEditorFactory;
  scanIntervalMs?: number;
  onSizeChange?: (size: number) => void;
};

export type AaronnoteMarkdownCellBridge = {
  scan(): number;
  dispose(): void;
  size(): number;
};

const MARKDOWN_CELL_SELECTOR = [
  ".jp-MarkdownCell",
  ".jp-Cell[data-cell-type='markdown']",
  ".jp-Cell.jp-mod-markdown",
].join(",");

const HOST_CLASS = "aaronnote-jupyter-mdcell-host";
const ACTIVE_CELL_CLASS = "aaronnote-jupyter-mdcell-active";
const HIDDEN_SOURCE_CLASS = "aaronnote-jupyter-source-hidden";

function markdownFromSourceView(view: EditorView): string {
  return view.state.doc.toString();
}

function replaceSourceMarkdown(view: EditorView, markdown: string): boolean {
  const current = markdownFromSourceView(view);
  if (current === markdown) return false;
  view.dispatch({
    changes: {
      from: 0,
      to: view.state.doc.length,
      insert: markdown,
    },
  });
  return true;
}

function isMarkdownCell(element: Element): element is HTMLElement {
  return element instanceof HTMLElement && element.matches(MARKDOWN_CELL_SELECTOR);
}

function markdownCells(doc: Document): HTMLElement[] {
  return Array.from(doc.querySelectorAll<HTMLElement>(MARKDOWN_CELL_SELECTOR))
    .filter((cell) => cell.isConnected);
}

function sourceEditorElement(cell: HTMLElement): HTMLElement | null {
  for (const editor of Array.from(cell.querySelectorAll<HTMLElement>(".cm-editor"))) {
    if (editor.closest(`.${HOST_CLASS}`)) continue;
    return editor;
  }
  return null;
}

function sourceViewFromCell(cell: HTMLElement): { editor: HTMLElement; view: EditorView } | null {
  const editor = sourceEditorElement(cell);
  if (!editor) return null;
  try {
    const view = EditorView.findFromDOM(editor);
    return view ? { editor, view } : null;
  } catch {
    return null;
  }
}

class MarkdownCellAdapter {
  private readonly cell: HTMLElement;
  private readonly sourceEditor: HTMLElement;
  private readonly sourceView: EditorView;
  private readonly host: HTMLElement;
  private readonly editor: AaronnoteMarkdownCellEditor;
  private readonly previousDisplay: string;
  private disposed = false;
  private applyingFromAaronnote = false;
  private applyingFromJupyter = false;
  private lastSourceMarkdown: string;

  constructor(
    cell: HTMLElement,
    sourceEditor: HTMLElement,
    sourceView: EditorView,
    createEditor: AaronnoteMarkdownCellEditorFactory,
  ) {
    this.cell = cell;
    this.sourceEditor = sourceEditor;
    this.sourceView = sourceView;
    this.lastSourceMarkdown = markdownFromSourceView(sourceView);
    this.previousDisplay = sourceEditor.style.display;
    this.host = cell.ownerDocument.createElement("section");
    this.host.className = HOST_CLASS;
    this.host.setAttribute("aria-label", "Aaronnote Markdown cell editor");
    this.host.addEventListener("mousedown", () => {
      this.cell.classList.add("jp-mod-active", "jp-mod-selected");
    });

    sourceEditor.classList.add(HIDDEN_SOURCE_CLASS);
    sourceEditor.style.display = "none";
    cell.classList.add(ACTIVE_CELL_CLASS);
    sourceEditor.insertAdjacentElement("afterend", this.host);

    this.editor = createEditor(this.host, {
      initialContent: this.lastSourceMarkdown,
      onChange: (markdown) => this.applyAaronnoteChange(markdown),
    });
  }

  isCurrent(): boolean {
    return !this.disposed
      && this.cell.isConnected
      && this.sourceEditor.isConnected
      && this.sourceView.dom.isConnected
      && isMarkdownCell(this.cell);
  }

  refreshFromSource(): void {
    if (this.disposed || this.applyingFromAaronnote) return;
    const markdown = markdownFromSourceView(this.sourceView);
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
    this.cell.classList.remove(ACTIVE_CELL_CLASS);
    this.sourceEditor.classList.remove(HIDDEN_SOURCE_CLASS);
    this.sourceEditor.style.display = this.previousDisplay;
    this.editor.destroy();
    this.host.remove();
  }

  private applyAaronnoteChange(markdown: string): void {
    if (this.disposed || this.applyingFromJupyter) return;
    this.applyingFromAaronnote = true;
    try {
      replaceSourceMarkdown(this.sourceView, markdown);
      this.lastSourceMarkdown = markdown;
    } finally {
      this.applyingFromAaronnote = false;
    }
  }
}

export function installAaronnoteMarkdownCellBridge(
  options: AaronnoteMarkdownCellBridgeOptions,
): AaronnoteMarkdownCellBridge {
  const doc = options.document ?? document;
  const win = doc.defaultView ?? window;
  const adapters = new Map<HTMLElement, MarkdownCellAdapter>();
  let disposed = false;
  let scanQueued = false;

  const scanNow = (): number => {
    if (disposed) return adapters.size;

    const cells = new Set(markdownCells(doc));
    for (const [cell, adapter] of adapters) {
      if (!cells.has(cell) || !adapter.isCurrent()) {
        adapter.dispose();
        adapters.delete(cell);
        continue;
      }
      adapter.refreshFromSource();
    }

    for (const cell of cells) {
      if (adapters.has(cell)) continue;
      const source = sourceViewFromCell(cell);
      if (!source) continue;
      adapters.set(cell, new MarkdownCellAdapter(
        cell,
        source.editor,
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
    const run = (): void => {
      scanQueued = false;
      scanNow();
    };
    if (typeof win.requestAnimationFrame === "function") {
      win.requestAnimationFrame(run);
    } else {
      win.setTimeout(run, 0);
    }
  };

  const Observer = win.MutationObserver;
  const observer = new Observer(scheduleScan);
  observer.observe(doc.body ?? doc.documentElement, {
    subtree: true,
    childList: true,
    attributes: true,
    attributeFilter: ["class", "data-cell-type"],
  });

  const intervalMs = options.scanIntervalMs ?? 750;
  const interval = intervalMs > 0
    ? win.setInterval(() => {
        for (const adapter of adapters.values()) adapter.refreshFromSource();
        scheduleScan();
      }, intervalMs)
    : undefined;

  scanNow();
  Object.assign(globalThis, {
    __aaronnoteJupyterMarkdownCells: {
      installed: true,
      version: "0.1.0",
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
      Object.assign(globalThis, {
        __aaronnoteJupyterMarkdownCells: {
          installed: false,
          version: "0.1.0",
          size: () => 0,
        },
      });
    },
  };
}
