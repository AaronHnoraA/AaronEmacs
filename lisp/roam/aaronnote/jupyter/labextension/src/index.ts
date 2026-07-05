import type { JupyterFrontEnd, JupyterFrontEndPlugin } from "@jupyterlab/application";
import { NotebookPanel } from "@jupyterlab/notebook";

import { aaronnoteDiagnostics } from "./diagnostics";
import { installAaronnoteDiagnosticsPanel } from "./diagnostics-panel";
import {
  findActiveAaronnoteEditorView,
  IEditorExtensionRegistry,
  installAaronnoteEmbeddedMarkdown,
  type AaronnoteEmbeddedSurface,
} from "./embedded-markdown";
import { installAaronnoteJupyterKeyboard } from "./keyboard";
import { installAaronnoteMarkdownCellFactory } from "./native-markdown-cell";
import { loadAaronnoteRuntime } from "./runtime";

const diagnosticsPlugin: JupyterFrontEndPlugin<void> = {
  id: "@aaronnote/jupyter-extension:diagnostics",
  description: "Aaronnote component status and runtime log panel.",
  autoStart: true,
  activate: (app: JupyterFrontEnd) => {
    aaronnoteDiagnostics.setComponent({
      id: "extension",
      label: "Aaronnote Extension",
      level: "ok",
      detail: "v0.3.0 loaded",
    });
    installAaronnoteDiagnosticsPanel(app);
    app.commands.addCommand("aaronnote:show-diagnostics", {
      label: "Show Aaronnote Diagnostics",
      execute: () => app.shell.activateById("aaronnote-jupyter-diagnostics"),
    });
  },
};

const keyboardPlugin: JupyterFrontEndPlugin<void> = {
  id: "@aaronnote/jupyter-extension:keyboard",
  description: "Aaronnote keyboard normalization for JupyterLab inside xwidget.",
  autoStart: true,
  activate: (app: JupyterFrontEnd) => {
    installAaronnoteJupyterKeyboard({ commands: app.commands });
    aaronnoteDiagnostics.setComponent({
      id: "keyboard",
      label: "Keyboard Bridge",
      level: "ok",
      detail: "Delete / Backspace / Escape ready",
    });
    aaronnoteDiagnostics.log("Keyboard bridge installed");
  },
};

const nativeMarkdownPlugin: JupyterFrontEndPlugin<void> = {
  id: "@aaronnote/jupyter-extension:native-markdown",
  description: "Aaronnote-native Jupyter Markdown cells and document extensions.",
  autoStart: true,
  requires: [NotebookPanel.IContentFactory, IEditorExtensionRegistry],
  activate: async (
    app: JupyterFrontEnd,
    contentFactory: NotebookPanel.IContentFactory,
    extensionRegistry: IEditorExtensionRegistry,
  ) => {
    aaronnoteDiagnostics.setComponent({
      id: "markdown-runtime",
      label: "Markdown Runtime",
      level: "waiting",
      detail: "loading Aaronnote editor",
    });
    try {
      const runtime = await loadAaronnoteRuntime();
      const surfaces: Record<AaronnoteEmbeddedSurface, number> = {
        cell: 0,
        document: 0,
      };
      const reportSurface = (surface: AaronnoteEmbeddedSurface): void => {
        const id = surface === "cell" ? "markdown-views" : "markdown-documents";
        const label = surface === "cell" ? "Aaronnote Cell Views" : "Markdown Documents";
        aaronnoteDiagnostics.setComponent({
          id,
          label,
          level: "ok",
          detail: `${surfaces[surface]} native EditorView`,
        });
      };

      installAaronnoteEmbeddedMarkdown({
        registry: extensionRegistry,
        runtime,
        onStatusChange: ({ surface, active }) => {
          surfaces[surface] = Math.max(0, surfaces[surface] + (active ? 1 : -1));
          reportSurface(surface);
        },
      });

      installAaronnoteMarkdownCellFactory(contentFactory, runtime.createAaronnoteMarkdownExtensions, (count) => {
        aaronnoteDiagnostics.setComponent({
          id: "markdown-cells",
          label: "Aaronnote Markdown Cells",
          level: "ok",
          detail: `${count} native cell widget`,
        });
      }, (active) => {
        surfaces.cell = Math.max(0, surfaces.cell + (active ? 1 : -1));
        reportSurface("cell");
      });

      app.commands.addCommand("aaronnote:toggle-markdown-source", {
        label: "Toggle Aaronnote Markdown Source",
        isEnabled: () => findActiveAaronnoteEditorView() !== null,
        execute: () => {
          const view = findActiveAaronnoteEditorView();
          return view ? runtime.toggleAaronnoteMarkdownSource(view) : false;
        },
      });

      aaronnoteDiagnostics.setComponent({
        id: "markdown-runtime",
        label: "Markdown Runtime",
        level: "ok",
        detail: "native CM6 extensions ready",
      });
      aaronnoteDiagnostics.log("Native Markdown cell factory and CM6 extensions installed");
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      aaronnoteDiagnostics.setComponent({
        id: "markdown-runtime",
        label: "Markdown Runtime",
        level: "error",
        detail: message,
      });
      aaronnoteDiagnostics.log(`Markdown runtime failed: ${message}`, "error");
      throw error;
    }
  },
};

export default [diagnosticsPlugin, keyboardPlugin, nativeMarkdownPlugin];
