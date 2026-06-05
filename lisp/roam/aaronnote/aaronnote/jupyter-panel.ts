import { api } from "./api-client.ts";
import { Epoch, type EpochRun } from "../src/async-epoch.ts";

export type JupyterTarget = {
  href: string;
  path: string;
  base: string;
  selector?: string;
  selectorKind?: "toc" | "";
};

export type JupyterPanel = {
  show: () => void;
  open: (target: JupyterTarget, options?: { restart?: boolean }) => Promise<void>;
  hide: () => void;
  stop: () => Promise<void>;
  readonly visible: boolean;
};

type JupyterPanelOptions = {
  root: HTMLElement;
  setStatus: (text: string) => void;
  onVisibilityChange?: () => void;
};

function targetKey(target: JupyterTarget): string {
  return [
    target.path,
    target.selectorKind || "",
    target.selector || "",
  ].join("\u0000");
}

function notebookKey(target: JupyterTarget): string {
  return String(target.path || "");
}

function selectorLabel(target: JupyterTarget): string {
  const selector = String(target.selector || "").trim();
  if (!selector) return "";
  return `@${selector}`;
}

function targetHash(target: JupyterTarget): string {
  const selector = String(target.selector || "").trim();
  if (!selector) return "";
  return encodeURIComponent(selector);
}

function withRequestTimeout<T>(promise: Promise<T>, ms: number, label: string): Promise<T> {
  let timer = 0;
  const timeout = new Promise<never>((_, reject) => {
    timer = window.setTimeout(() => reject(new Error(`${label} timed out after ${Math.round(ms / 1000)}s`)), ms);
  });
  return Promise.race([
    promise.finally(() => window.clearTimeout(timer)),
    timeout,
  ]);
}

export function createJupyterPanel(options: JupyterPanelOptions): JupyterPanel {
  const { root, setStatus, onVisibilityChange } = options;
  root.innerHTML = `
<div class="jupyter-panel-header">
  <span class="jupyter-panel-title">Jupyter</span>
  <span class="jupyter-panel-target" data-jupyter-target>No notebook</span>
  <button class="lean-panel-btn lean-panel-btn--icon" data-jupyter-external title="Open in browser">↗</button>
  <button class="lean-panel-btn lean-panel-btn--icon" data-jupyter-restart title="Restart Jupyter server">↺</button>
  <button class="lean-panel-btn lean-panel-btn--text" data-jupyter-stop title="Stop Jupyter server">Stop</button>
  <button class="lean-panel-btn lean-panel-btn--icon" data-jupyter-close title="Close panel">✕</button>
</div>
<div class="jupyter-panel-body">
  <div class="jupyter-panel-empty" data-jupyter-empty>Open an .ipynb link to start Jupyter.</div>
  <iframe data-jupyter-frame title="JupyterLab preview" allow="clipboard-read; clipboard-write; fullscreen" hidden></iframe>
</div>
`;

  const targetEl = root.querySelector<HTMLElement>("[data-jupyter-target]")!;
  const externalBtn = root.querySelector<HTMLButtonElement>("[data-jupyter-external]")!;
  const restartBtn = root.querySelector<HTMLButtonElement>("[data-jupyter-restart]")!;
  const stopBtn = root.querySelector<HTMLButtonElement>("[data-jupyter-stop]")!;
  const closeBtn = root.querySelector<HTMLButtonElement>("[data-jupyter-close]")!;
  const emptyEl = root.querySelector<HTMLElement>("[data-jupyter-empty]")!;
  const frame = root.querySelector<HTMLIFrameElement>("[data-jupyter-frame]")!;

  let _visible = false;
  let currentTarget: JupyterTarget | null = null;
  let currentUrl = "";
  let _currentKey = "";
  const openEpoch = new Epoch();
  let activeRun: EpochRun | null = null;
  let readyTimer = 0;
  let kernelTimer = 0;

  function show(): void {
    if (_visible) return;
    _visible = true;
    root.removeAttribute("hidden");
    root.classList.remove("jupyter-panel--hidden");
    document.body.classList.add("lean-panel-open");
    if (activeRun && currentUrl && !frame.getAttribute("src")) waitForJupyterReady(activeRun, currentUrl);
    onVisibilityChange?.();
  }

  function hide(): void {
    if (!_visible) return;
    _visible = false;
    window.clearTimeout(readyTimer);
    window.clearTimeout(kernelTimer);
    root.classList.add("jupyter-panel--hidden");
    root.removeAttribute("hidden");
    document.body.classList.remove("lean-panel-open");
    onVisibilityChange?.();
  }

  function setBusy(text: string): void {
    window.clearTimeout(readyTimer);
    window.clearTimeout(kernelTimer);
    targetEl.textContent = text;
    emptyEl.textContent = "Starting JupyterLab...";
    emptyEl.hidden = false;
    frame.hidden = true;
  }

  function setTargetLabel(target: JupyterTarget): void {
    targetEl.textContent = [target.path.split(/[\\/]/).pop() || target.path, selectorLabel(target)]
      .filter(Boolean)
      .join(" ");
  }

  async function scrollFrameTarget(run: EpochRun, url: string, retries = 4): Promise<boolean> {
    const target = currentTarget;
    if (!target || !run.current || currentUrl !== url || frame.hidden || !frame.getAttribute("src")) return false;
    try {
      const response = await api.jupyter.scroll({
        url,
        path: target.path,
        base: target.base,
        selector: target.selector || "",
        selectorKind: target.selectorKind || "",
      });
      if (typeof response.url === "string" && response.url) currentUrl = response.url;
      setStatus(target.selector ? "Jupyter preview scrolled" : "Jupyter preview ready");
      return response.scrolled !== false;
    } catch {
      if (retries <= 0 || !run.current || currentUrl !== url) return false;
      window.setTimeout(() => void scrollFrameTarget(run, url, retries - 1), 250);
      return false;
    }
  }

  frame.addEventListener("load", () => {
    const run = activeRun;
    const url = currentUrl;
    if (!run) return;
    window.setTimeout(() => void scrollFrameTarget(run, url), 100);
    window.setTimeout(() => void scrollFrameTarget(run, url), 600);
    if (url) {
      setStatus("Kernel connecting…");
      pollKernelStatus(run, url);
    }
  });

  async function scrollCurrentNotebook(target: JupyterTarget): Promise<void> {
    if (!currentUrl) return;
    const next = new URL(currentUrl);
    next.hash = targetHash(target);
    const nextUrl = next.toString();
    currentTarget = target;
    currentUrl = nextUrl;
    _currentKey = targetKey(target);
    const run = openEpoch.begin();
    activeRun = run;
    setTargetLabel(target);
    if (frame.hidden || !frame.getAttribute("src")) {
      emptyEl.textContent = "Starting JupyterLab...";
      emptyEl.hidden = false;
      waitForJupyterReady(run, nextUrl);
      setStatus("Jupyter preview loading");
      return;
    }
    emptyEl.hidden = true;
    frame.hidden = false;
    let updated = await scrollFrameTarget(run, nextUrl);
    if (!updated) {
      try {
        const frameWindow = frame.contentWindow;
        if (frameWindow) {
          frameWindow.location.hash = next.hash;
          updated = true;
        }
      } catch {
        updated = false;
      }
    }
    setStatus(target.selector ? "Jupyter preview scrolled" : "Jupyter preview ready");
  }

  function showFrame(run: EpochRun, url: string): void {
    if (!run.current || currentUrl !== url) return;
    window.clearTimeout(readyTimer);
    emptyEl.hidden = true;
    frame.hidden = false;
    if (frame.src !== url) frame.src = url;
    else void scrollFrameTarget(run, url);
  }

  function waitForJupyterReady(run: EpochRun, url: string, attempts = 60): void {
    window.clearTimeout(readyTimer);
    if (!run.current || currentUrl !== url || !_visible) return;
    readyTimer = window.setTimeout(async () => {
      if (!run.current || currentUrl !== url || !_visible) return;
      let detail = "";
      try {
        const status = await api.jupyter.request("status");
        if (status.ready === true) {
          showFrame(run, url);
          setStatus("Jupyter preview ready");
          return;
        }
        const message = typeof status.message === "string" ? status.message : "";
        const output = typeof status.output === "string" ? status.output.trim() : "";
        detail = [message, output ? output.slice(-600) : ""].filter(Boolean).join("\n\n");
      } catch {
        // Keep the lightweight readiness poll local to active previews.
      }
      if (attempts <= 1) {
        emptyEl.textContent = detail
          ? `Jupyter preview is still starting.\n\n${detail}`
          : "Jupyter preview is still starting.";
        emptyEl.hidden = false;
        frame.hidden = true;
        setStatus("Jupyter preview still starting");
        return;
      }
      waitForJupyterReady(run, url, attempts - 1);
    }, 600);
  }

  // Bounded probe of JupyterLab's real kernel state inside the iframe. Stops as soon
  // as the kernel connects (or dies) and never runs while the panel is hidden, so it
  // adds no perpetual background polling.
  function pollKernelStatus(run: EpochRun, url: string, attempts = 40): void {
    window.clearTimeout(kernelTimer);
    if (!run.current || currentUrl !== url || !_visible || frame.hidden) return;
    kernelTimer = window.setTimeout(async () => {
      if (!run.current || currentUrl !== url || !_visible || frame.hidden) return;
      try {
        const status = await api.jupyter.kernelStatus({ url });
        if (!run.current || currentUrl !== url) return;
        if (status.connected === true) {
          setStatus("Kernel ready");
          return;
        }
        if (status.dead === true) {
          setStatus("Kernel error");
          return;
        }
        setStatus("Kernel connecting…");
      } catch {
        // Frame may not have booted JupyterLab yet; keep probing until attempts run out.
      }
      if (attempts <= 1) {
        setStatus("Kernel not responding");
        return;
      }
      pollKernelStatus(run, url, attempts - 1);
    }, 1000);
  }

  async function open(target: JupyterTarget, panelOptions: { restart?: boolean } = {}): Promise<void> {
    const key = targetKey(target);
    show();
    if (!panelOptions.restart && key === _currentKey && currentUrl) {
      await scrollCurrentNotebook(target);
      return;
    }
    if (!panelOptions.restart && currentTarget && notebookKey(target) === notebookKey(currentTarget) && currentUrl) {
      await scrollCurrentNotebook(target);
      return;
    }
    currentTarget = target;
    _currentKey = key;
    const run = openEpoch.begin();
    activeRun = run;
    setBusy(panelOptions.restart ? "Restarting..." : "Starting...");
    setStatus(panelOptions.restart ? "Restarting Jupyter" : "Opening Jupyter preview");
    try {
      const response = await withRequestTimeout(
        api.jupyter.request(panelOptions.restart ? "restart" : "open", {
          path: target.path,
          base: target.base,
          selector: target.selector || "",
          selectorKind: target.selectorKind || "",
        }),
        10_000,
        panelOptions.restart ? "Restarting Jupyter" : "Starting Jupyter",
      );
      if (!run.current) return;
      currentUrl = String(response.url || "");
      if (!currentUrl) throw new Error("Jupyter did not return a preview URL");
      setTargetLabel(target);
      if (response.ready === true) {
        showFrame(run, currentUrl);
        setStatus("Jupyter preview ready");
      } else {
        emptyEl.textContent = "Starting JupyterLab...";
        emptyEl.hidden = false;
        frame.hidden = true;
        waitForJupyterReady(run, currentUrl);
        setStatus("Jupyter preview loading");
      }
    } catch (err) {
      if (!run.current) return;
      window.clearTimeout(readyTimer);
      currentUrl = "";
      emptyEl.textContent = err instanceof Error ? err.message : "Jupyter preview failed";
      emptyEl.hidden = false;
      frame.hidden = true;
      setStatus(err instanceof Error ? err.message : "Jupyter preview failed");
    }
  }

  async function stop(): Promise<void> {
    setBusy("Stopping...");
    try {
      await api.jupyter.request("stop");
      frame.removeAttribute("src");
      window.clearTimeout(readyTimer);
      openEpoch.cancel();
      activeRun = null;
      currentUrl = "";
      _currentKey = "";
      currentTarget = null;
      targetEl.textContent = "Stopped";
      emptyEl.textContent = "Jupyter stopped.";
      setStatus("Jupyter stopped");
    } catch (err) {
      setStatus(err instanceof Error ? err.message : "Jupyter stop failed");
    }
  }

  externalBtn.addEventListener("click", () => {
    if (currentUrl) window.open(currentUrl, "_blank", "noopener,noreferrer");
  });
  restartBtn.addEventListener("click", () => {
    if (currentTarget) void open(currentTarget, { restart: true });
  });
  stopBtn.addEventListener("click", () => void stop());
  closeBtn.addEventListener("click", () => hide());

  api.jupyter.onStatus((data) => {
    if (!data || data.running !== false) return;
    window.clearTimeout(readyTimer);
    window.clearTimeout(kernelTimer);
    openEpoch.cancel();
    activeRun = null;
    frame.removeAttribute("src");
    frame.hidden = true;
    currentUrl = "";
    _currentKey = "";
    const tail = typeof data.output === "string" && data.output.trim() ? `\n\n${data.output.trim().slice(-600)}` : "";
    emptyEl.textContent = `${data.crashed ? "Jupyter server stopped unexpectedly." : "Jupyter server stopped."}${tail}`;
    emptyEl.hidden = false;
    setStatus(data.crashed ? "Jupyter server crashed" : "Jupyter stopped");
  });

  return {
    show,
    open,
    hide,
    stop,
    get visible() { return _visible; },
  };
}
