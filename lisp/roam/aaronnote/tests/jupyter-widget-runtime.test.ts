import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { readFileSync } from "node:fs";
import { join } from "node:path";

describe("Jupyter widget runtime", () => {
  test("uses the JupyterLab widget manager output implementation for live interact output", () => {
    const source = readFileSync(join(process.cwd(), "src/jupyter-widget-runtime.ts"), "utf8");

    expect(source).toContain('import * as widgetOutput from "@jupyter-widgets/jupyterlab-manager/lib/output";');
    expect(source).toContain('import { KernelWidgetManager, WIDGET_VIEW_MIMETYPE } from "@jupyter-widgets/jupyterlab-manager/lib/manager";');
    expect(source).toContain('import { WidgetRenderer } from "@jupyter-widgets/jupyterlab-manager/lib/renderer";');
    expect(source).toContain("extends KernelWidgetManager");
    expect(source).toContain("restoreWidgets()");
    expect(source).toContain("/jupyter/widget-runtimes/");
    expect(source).toContain("runtimeWebSocketCtor");
    expect(source).toContain("new RenderMimeRegistry");
    expect(source).toContain('"jupyter-js-widgets": "@jupyter-widgets/base"');
    expect(source).toContain('document.body.dataset.baseUrl ??= new URL("/jupyter/", window.location.origin).toString();');
    expect(source).not.toContain('import * as widgetOutput from "@jupyter-widgets/html-manager/lib/output";');
    expect(source).not.toContain('import * as widgetOutput from "@jupyter-widgets/output";');
    expect(source).not.toContain('@jupyterlab/outputarea/style/index.js');
  });
});
