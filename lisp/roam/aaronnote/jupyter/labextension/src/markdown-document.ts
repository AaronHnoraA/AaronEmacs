import type { AaronnoteMarkdownCellEditorFactory } from "./markdown-cell-core";
import {
  installAaronnoteMarkdownDocumentBridge,
  type AaronnoteMarkdownDocumentBridge,
} from "./markdown-document-core";

export type AaronnoteJupyterMarkdownDocumentOptions = {
  document?: Document;
  scanIntervalMs?: number;
  createEditor: AaronnoteMarkdownCellEditorFactory;
  onSizeChange?: (size: number) => void;
};

export function installAaronnoteJupyterMarkdownDocuments(
  options: AaronnoteJupyterMarkdownDocumentOptions,
): AaronnoteMarkdownDocumentBridge {
  return installAaronnoteMarkdownDocumentBridge(options);
}
