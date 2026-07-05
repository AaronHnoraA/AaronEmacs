import * as widgetBase from "@jupyter-widgets/base";
import * as widgetControls from "@jupyter-widgets/controls";
import * as widgetOutput from "@jupyter-widgets/output";
import { HTMLManager } from "@jupyter-widgets/html-manager";
import { KernelConnection, ServerConnection, type Kernel } from "@jupyterlab/services";
import requireJsSource from "requirejs/require.js?raw";
import { evaluateAmdLoaderSource, validWidgetModuleName, validWidgetModuleVersion, widgetModuleCdnUrl } from "./jupyter-widget-loader.ts";

import "@jupyter-widgets/base/css/index.css";
import "@jupyter-widgets/controls/css/widgets.css";
import "@fortawesome/fontawesome-free/css/all.min.css";

export type JupyterWidgetRuntime = {
  id: string;
  name: string;
  generation?: number;
};

type RequireJs = {
  (modules: string[], onLoad: (module: unknown) => void, onError: (error: unknown) => void): void;
  config(options: { paths: Record<string, string> }): RequireJs;
  defined(name: string): boolean;
  undef(name: string): void;
};

type WidgetViewLike = {
  remove(): unknown;
};

declare global {
  interface Window {
    requirejs?: RequireJs;
    define?: {
      (name: string, dependencies: string[], factory: () => unknown): void;
      amd?: unknown;
    };
  }
}

let requireJsReady: Promise<RequireJs> | null = null;

function installBundledRequireJs(): RequireJs {
  // RequireJS declares its globals with top-level `var`.  In xwidget-webkit,
  // an indirect eval can execute that source successfully without reflecting
  // those bindings onto `window`.  Execute it in a function scope, return the
  // bindings explicitly, and install them ourselves.
  const bindings = evaluateAmdLoaderSource(requireJsSource);
  if (typeof bindings.requirejs !== "function" || typeof bindings.define !== "function") {
    throw new Error("Bundled RequireJS did not expose its AMD bindings");
  }
  window.requirejs = bindings.requirejs as RequireJs;
  window.define = bindings.define as Window["define"];
  // Some third-party widget bundles inspect the conventional `require`
  // global even though AaronNote itself uses `requirejs`.
  (window as unknown as { require?: RequireJs }).require = (typeof bindings.require === "function" ? bindings.require : bindings.requirejs) as RequireJs;
  return bindings.requirejs as RequireJs;
}

function ensureRequireJs(): Promise<RequireJs> {
  if (window.requirejs) return Promise.resolve(window.requirejs);
  if (requireJsReady) return requireJsReady;
  requireJsReady = new Promise((resolve, reject) => {
    try {
      resolve(installBundledRequireJs());
    } catch (error) {
      reject(new Error(`Failed to initialize the bundled RequireJS runtime: ${error instanceof Error ? error.message : String(error)}`));
    }
  });
  return requireJsReady;
}

function requireModule(requireJs: RequireJs, name: string): Promise<unknown> {
  return new Promise((resolve, reject) => requireJs([name], resolve, reject));
}

function defineCoreAmdModules(): void {
  const define = window.define;
  const requireJs = window.requirejs;
  if (!define || !requireJs) throw new Error("RequireJS globals are unavailable");
  const modules: Array<[string, unknown]> = [
    ["@jupyter-widgets/base", widgetBase],
    ["@jupyter-widgets/controls", widgetControls],
    ["@jupyter-widgets/output", widgetOutput],
  ];
  for (const [name, value] of modules) {
    if (!requireJs.defined(name)) define(name, [], () => value);
  }
}

const coreWidgetModules = new Map<string, unknown>([
  ["@jupyter-widgets/base", widgetBase],
  ["@jupyter-widgets/controls", widgetControls],
  ["@jupyter-widgets/output", widgetOutput],
]);

async function loadCustomWidgetModule(moduleName: string, moduleVersion: string): Promise<unknown> {
  if (!validWidgetModuleName(moduleName)) throw new Error(`Invalid widget module name: ${moduleName}`);
  if (!validWidgetModuleVersion(moduleVersion)) throw new Error(`Invalid widget module version: ${moduleVersion}`);
  // Standard ipywidgets (including @interact sliders) are already bundled.
  // Returning them directly avoids making core controls depend on an AMD
  // compatibility layer that is only needed by third-party widget packages.
  const coreModule = coreWidgetModules.get(moduleName);
  if (coreModule) return coreModule;
  const requireJs = await ensureRequireJs();
  defineCoreAmdModules();
  const localPath = `${window.location.origin}/jupyter/nbextensions/${moduleName}/index`;
  requireJs.config({ paths: { [moduleName]: localPath } });
  try {
    return await requireModule(requireJs, moduleName);
  } catch {
    requireJs.undef(moduleName);
  }
  const cdnPath = widgetModuleCdnUrl(moduleName, moduleVersion);
  console.info(`[aaronnote-jupyter] loading widget module ${moduleName}@${moduleVersion} from jsDelivr`);
  requireJs.config({ paths: { [moduleName]: cdnPath } });
  return await requireModule(requireJs, moduleName);
}

class AaronnoteWidgetManager extends HTMLManager {
  readonly kernel: Kernel.IKernelConnection;
  private restorePromise: Promise<void> | null = null;

  constructor(kernel: Kernel.IKernelConnection) {
    super({ loader: loadCustomWidgetModule });
    this.kernel = kernel;
    kernel.registerCommTarget(this.comm_target_name, async (comm, msg) => {
      await this.handle_comm_open(new widgetBase.shims.services.Comm(comm), msg);
    });
  }

  override async _create_comm(targetName: string, modelId: string, data?: unknown, metadata?: unknown, buffers?: ArrayBuffer[]): Promise<widgetBase.shims.services.Comm> {
    const comm = this.kernel.createComm(targetName, modelId);
    if (data || metadata || buffers?.length) comm.open(data as never, metadata as never, buffers);
    return new widgetBase.shims.services.Comm(comm);
  }

  override _get_comm_info(): Promise<Record<string, unknown>> {
    return this.kernel.requestCommInfo({ target_name: this.comm_target_name })
      .then((reply) => (reply.content as { comms?: Record<string, unknown> }).comms ?? {});
  }

  restoreFromKernel(): Promise<void> {
    if (!this.restorePromise) this.restorePromise = this._loadFromKernel();
    return this.restorePromise;
  }

  async mount(modelId: string, host: HTMLElement): Promise<() => void> {
    await this.restoreFromKernel();
    const model = await this.get_model(modelId);
    const view = await this.create_view(model);
    host.replaceChildren();
    await this.display_view(view, host);
    return () => {
      try { (view as unknown as WidgetViewLike).remove(); } catch {}
    };
  }
}

type RuntimeEntry = {
  kernel: KernelConnection;
  manager: AaronnoteWidgetManager;
};

let runtimeEntries: Map<string, Promise<RuntimeEntry>> | undefined;

function runtimeEntryMap(): Map<string, Promise<RuntimeEntry>> {
  runtimeEntries ??= new Map<string, Promise<RuntimeEntry>>();
  return runtimeEntries;
}

function runtimeKey(runtime: JupyterWidgetRuntime): string {
  return `${runtime.id}:${Number(runtime.generation || 1)}`;
}

function disposeOlderGenerations(runtime: JupyterWidgetRuntime): void {
  const keep = runtimeKey(runtime);
  const entries = runtimeEntryMap();
  for (const [key, pending] of Array.from(entries.entries())) {
    if (!key.startsWith(`${runtime.id}:`) || key === keep) continue;
    entries.delete(key);
    void pending.then(({ kernel }) => kernel.dispose()).catch(() => {});
  }
}

async function createRuntimeEntry(runtime: JupyterWidgetRuntime): Promise<RuntimeEntry> {
  const baseUrl = new URL("./jupyter/", window.location.origin + window.location.pathname).toString();
  const wsUrl = baseUrl.replace(/^http/i, "ws");
  const serverSettings = ServerConnection.makeSettings({ baseUrl, wsUrl, token: "" });
  const kernel = new KernelConnection({
    model: { id: runtime.id, name: runtime.name },
    serverSettings,
    username: "aaronnote-widget",
    handleComms: true,
  });
  await kernel.info;
  return { kernel, manager: new AaronnoteWidgetManager(kernel) };
}

function getRuntimeEntry(runtime: JupyterWidgetRuntime): Promise<RuntimeEntry> {
  disposeOlderGenerations(runtime);
  const key = runtimeKey(runtime);
  const entries = runtimeEntryMap();
  const existing = entries.get(key);
  if (existing) return existing;
  const pending = createRuntimeEntry(runtime).catch((error) => {
    entries.delete(key);
    throw error;
  });
  entries.set(key, pending);
  return pending;
}

export async function mountJupyterWidget(host: HTMLElement, modelId: string, runtime: JupyterWidgetRuntime): Promise<() => void> {
  if (!runtime.id || !runtime.name) throw new Error("Missing live Jupyter widget runtime");
  const { manager } = await getRuntimeEntry(runtime);
  return await manager.mount(modelId, host);
}
