import { Compartment, type Extension, type StateEffect } from "@codemirror/state";
import { EditorView, ViewPlugin } from "@codemirror/view";
import type { CodeEditor } from "@jupyterlab/codeeditor";
import {
  IEditorExtensionRegistry,
  type IConfigurableExtension,
} from "@jupyterlab/codemirror";

import { isMarkdownMimeType } from "./embedded-markdown-core";

export type AaronnoteEmbeddedRuntime = {
  createAaronnoteMarkdownExtensions(options?: Record<string, unknown>): Extension;
  toggleAaronnoteMarkdownSource(view: EditorView): boolean;
  isAaronnoteMarkdownSource(view: EditorView): boolean;
};

export type AaronnoteEmbeddedSurface = "cell" | "document";

export type AaronnoteEmbeddedStatus = {
  surface: AaronnoteEmbeddedSurface;
  active: boolean;
};

export type AaronnoteEmbeddedOptions = {
  registry: IEditorExtensionRegistry;
  runtime: AaronnoteEmbeddedRuntime;
  onStatusChange?: (status: AaronnoteEmbeddedStatus) => void;
};

function createMimeAwareExtension(
  model: CodeEditor.IModel,
  surface: AaronnoteEmbeddedSurface,
  runtime: AaronnoteEmbeddedRuntime,
  onStatusChange: (status: AaronnoteEmbeddedStatus) => void,
): Extension {
  const compartment = new Compartment();
  const aaronnoteExtension = runtime.createAaronnoteMarkdownExtensions();
  let active = isMarkdownMimeType(model.mimeType);

  const lifecycle = ViewPlugin.fromClass(class {
    private readonly view: EditorView;

    constructor(view: EditorView) {
      this.view = view;
      model.mimeTypeChanged.connect(this.onMimeTypeChanged);
      this.setSurfaceState(active);
    }

    destroy(): void {
      model.mimeTypeChanged.disconnect(this.onMimeTypeChanged);
      if (active) {
        this.view.dom.classList.remove("aaronnote-embedded-markdown");
        onStatusChange({ surface, active: false });
      }
    }

    private readonly onMimeTypeChanged = (): void => {
      const nextActive = isMarkdownMimeType(model.mimeType);
      if (nextActive === active) return;
      active = nextActive;
      this.view.dispatch({
        effects: compartment.reconfigure(active ? aaronnoteExtension : []),
      });
      this.setSurfaceState(active);
    };

    private setSurfaceState(enabled: boolean): void {
      this.view.dom.classList.toggle("aaronnote-embedded-markdown", enabled);
      if (enabled) onStatusChange({ surface, active: true });
      else onStatusChange({ surface, active: false });
    }
  });

  return [
    compartment.of(active ? aaronnoteExtension : []),
    lifecycle,
  ];
}

export function installAaronnoteEmbeddedMarkdown(options: AaronnoteEmbeddedOptions): void {
  const onStatusChange = options.onStatusChange ?? (() => undefined);
  options.registry.addExtension<undefined>({
    name: "aaronnoteMarkdown",
    factory: ({ inline, model }): IConfigurableExtension<undefined> | null => inline ? null : ({
      instance: () => createMimeAwareExtension(
        model,
        "document",
        options.runtime,
        onStatusChange,
      ),
      reconfigure: (): StateEffect<undefined> | null => null,
    }),
  });
}

export function findActiveAaronnoteEditorView(doc: Document = document): EditorView | null {
  const editor = doc.querySelector<HTMLElement>([
    ".jp-AaronnoteMarkdownCell.jp-mod-active .cm-editor",
    ".jp-AaronnoteMarkdownCell.jp-mod-selected .cm-editor",
    ".jp-FileEditor .cm-editor.cm-focused",
    ".jp-FileEditor .cm-editor",
  ].join(","));
  if (!editor) return null;
  try {
    return EditorView.findFromDOM(editor);
  } catch {
    return null;
  }
}

export { IEditorExtensionRegistry };
export { isMarkdownMimeType } from "./embedded-markdown-core";
