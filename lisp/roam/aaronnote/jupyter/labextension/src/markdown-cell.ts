import "../style/markdown-cell.css";
import {
  installAaronnoteMarkdownCellBridge,
  type AaronnoteMarkdownCellEditorFactory,
  type AaronnoteMarkdownCellBridge,
} from "./markdown-cell-core";
import { loadAaronnoteRuntime } from "./runtime";

export type AaronnoteJupyterMarkdownCellOptions = {
  document?: Document;
  scanIntervalMs?: number;
  createEditor?: AaronnoteMarkdownCellEditorFactory;
  onSizeChange?: (size: number) => void;
};

export async function installAaronnoteJupyterMarkdownCells(
  options: AaronnoteJupyterMarkdownCellOptions = {},
): Promise<AaronnoteMarkdownCellBridge> {
  const createEditor = options.createEditor ?? (await loadAaronnoteRuntime()).createEditor;
  return installAaronnoteMarkdownCellBridge({
    ...options,
    createEditor,
  });
}
