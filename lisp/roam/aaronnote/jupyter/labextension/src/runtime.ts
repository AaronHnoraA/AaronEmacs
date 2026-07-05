import type { Extension } from "@codemirror/state";
import type { EditorView } from "@codemirror/view";

import type { AaronnoteMarkdownCellEditorFactory } from "./markdown-cell-core";

export type AaronnoteRuntime = {
  createEditor: AaronnoteMarkdownCellEditorFactory;
  createAaronnoteMarkdownExtensions(options?: Record<string, unknown>): Extension;
  toggleAaronnoteMarkdownSource(view: EditorView): boolean;
  isAaronnoteMarkdownSource(view: EditorView): boolean;
};

export async function loadAaronnoteRuntime(): Promise<AaronnoteRuntime> {
  // Built by jupyter/scripts/build-labextension.sh before this extension is bundled.
  // @ts-expect-error The generated library is intentionally outside labextension/src.
  return await import("../../../dist/lib/typora-web.js") as AaronnoteRuntime;
}
