import type { Extension } from "@codemirror/state";
import { ViewPlugin } from "@codemirror/view";
import { MarkdownCell, type MarkdownCell as MarkdownCellNamespace } from "@jupyterlab/cells";
import { NotebookPanel } from "@jupyterlab/notebook";

import { replaceMarkdownCellFactory } from "./native-cell-factory-core";
import "../style/aaronnote-embedded.css";
import "../style/native-markdown-cell.css";

type CellCountReporter = (count: number) => void;
type CellViewReporter = (active: boolean) => void;
type AaronnoteExtensionFactory = (options?: Record<string, unknown>) => Extension;

let nativeCellCount = 0;
let reportCellCount: CellCountReporter = () => undefined;

function attachmentName(model: MarkdownCellNamespace.IOptions["model"], requestedName: string): string {
  const clean = requestedName.replace(/[^A-Za-z0-9._-]+/g, "-") || "attachment";
  if (!model.attachments.has(clean)) return clean;
  const dot = clean.lastIndexOf(".");
  const stem = dot > 0 ? clean.slice(0, dot) : clean;
  const extension = dot > 0 ? clean.slice(dot) : "";
  let index = 2;
  while (model.attachments.has(`${stem}-${index}${extension}`)) index += 1;
  return `${stem}-${index}${extension}`;
}

async function blobBase64(blob: Blob): Promise<string> {
  const bytes = new Uint8Array(await blob.arrayBuffer());
  let binary = "";
  for (let index = 0; index < bytes.length; index += 0x8000) {
    binary += String.fromCharCode(...bytes.slice(index, index + 0x8000));
  }
  return btoa(binary);
}

function createCellExtensions(
  options: MarkdownCellNamespace.IOptions,
  createExtension: AaronnoteExtensionFactory,
  reportView: CellViewReporter,
): Extension[] {
  const pasteAssets = {
    uploadBlobAsset: async (
      blob: Blob,
      metadata: { name?: string; type?: string },
    ) => {
      const name = attachmentName(options.model, metadata.name ?? "attachment");
      const type = metadata.type || blob.type || "application/octet-stream";
      options.model.attachments.set(name, { [type]: await blobBase64(blob) });
      return {
        ok: true,
        name,
        type,
        isImage: type.startsWith("image/"),
        markdownPath: `attachment:${name}`,
      };
    },
  };
  const lifecycle = ViewPlugin.fromClass(class {
    constructor() {
      reportView(true);
    }

    destroy(): void {
      reportView(false);
    }
  });
  return [
    createExtension({ pasteAssets }),
    lifecycle,
  ];
}

export class AaronnoteMarkdownCell extends MarkdownCell {
  private counted = true;

  constructor(options: MarkdownCellNamespace.IOptions) {
    super(options);
    this.addClass("jp-AaronnoteMarkdownCell");
    nativeCellCount += 1;
    reportCellCount(nativeCellCount);
    this.rendered = false;
  }

  override get rendered(): boolean {
    return false;
  }

  override set rendered(_value: boolean) {
    if (super.rendered) super.rendered = false;
  }

  override dispose(): void {
    if (this.counted) {
      this.counted = false;
      nativeCellCount = Math.max(0, nativeCellCount - 1);
      reportCellCount(nativeCellCount);
    }
    super.dispose();
  }
}

export function installAaronnoteMarkdownCellFactory(
  contentFactory: NotebookPanel.IContentFactory,
  createExtension: AaronnoteExtensionFactory,
  reporter: CellCountReporter = () => undefined,
  viewReporter: CellViewReporter = () => undefined,
): void {
  reportCellCount = reporter;
  reportCellCount(nativeCellCount);
  replaceMarkdownCellFactory(contentFactory, (options: MarkdownCellNamespace.IOptions) => {
    const editorExtensions = [
      ...(options.editorExtensions ?? []),
      ...createCellExtensions(options, createExtension, viewReporter),
    ];
    return new AaronnoteMarkdownCell({ ...options, editorExtensions }).initializeState();
  });
}

export function getAaronnoteMarkdownCellCount(): number {
  return nativeCellCount;
}
