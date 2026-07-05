import type { JupyterFrontEnd } from "@jupyterlab/application";
import { clearIcon, markdownIcon, refreshIcon } from "@jupyterlab/ui-components";
import { Widget } from "@lumino/widgets";

import { aaronnoteDiagnostics, type AaronnoteComponentStatus } from "./diagnostics";
import "../style/diagnostics.css";

function iconButton(label: string, icon: typeof refreshIcon): HTMLButtonElement {
  const button = document.createElement("button");
  button.className = "aaronnote-diagnostics-icon-button";
  button.type = "button";
  button.title = label;
  button.setAttribute("aria-label", label);
  button.append(icon.element({ tag: "span" }));
  return button;
}

function statusRow(status: AaronnoteComponentStatus): HTMLElement {
  const row = document.createElement("div");
  row.className = "aaronnote-diagnostics-status";
  row.dataset.level = status.level;

  const indicator = document.createElement("span");
  indicator.className = "aaronnote-diagnostics-indicator";
  const text = document.createElement("div");
  const label = document.createElement("strong");
  label.textContent = status.label;
  const detail = document.createElement("span");
  detail.textContent = status.detail;
  text.append(label, detail);
  row.append(indicator, text);
  return row;
}

export class AaronnoteDiagnosticsPanel extends Widget {
  private readonly statusList: HTMLElement;
  private readonly logList: HTMLElement;
  private unsubscribe: (() => void) | null = null;

  constructor() {
    super({ node: document.createElement("section") });
    this.id = "aaronnote-jupyter-diagnostics";
    this.addClass("aaronnote-diagnostics-panel");
    this.title.label = "Aaronnote";
    this.title.caption = "Aaronnote 组件状态与日志";
    this.title.icon = markdownIcon;
    this.title.closable = false;

    const header = document.createElement("header");
    const heading = document.createElement("h2");
    heading.textContent = "Aaronnote";
    const actions = document.createElement("div");
    const refresh = iconButton("刷新状态", refreshIcon);
    refresh.addEventListener("click", () => void this.refreshServer());
    const clear = iconButton("清空日志", clearIcon);
    clear.addEventListener("click", () => aaronnoteDiagnostics.clearLogs());
    actions.append(refresh, clear);
    header.append(heading, actions);

    const statusHeading = document.createElement("h3");
    statusHeading.textContent = "组件状态";
    this.statusList = document.createElement("div");
    this.statusList.className = "aaronnote-diagnostics-status-list";

    const logHeading = document.createElement("h3");
    logHeading.textContent = "运行日志";
    this.logList = document.createElement("div");
    this.logList.className = "aaronnote-diagnostics-log";
    this.node.append(header, statusHeading, this.statusList, logHeading, this.logList);
    this.render();
  }

  protected onAfterAttach(): void {
    this.unsubscribe = aaronnoteDiagnostics.subscribe(() => this.render());
    void this.refreshServer();
  }

  protected onBeforeDetach(): void {
    this.unsubscribe?.();
    this.unsubscribe = null;
  }

  private async refreshServer(): Promise<void> {
    try {
      const response = await fetch("/api/status", { credentials: "same-origin" });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      aaronnoteDiagnostics.setComponent({
        id: "server",
        label: "Jupyter Server",
        level: "ok",
        detail: "online",
      });
    } catch (error) {
      aaronnoteDiagnostics.setComponent({
        id: "server",
        label: "Jupyter Server",
        level: "error",
        detail: error instanceof Error ? error.message : String(error),
      });
    }
  }

  private render(): void {
    const snapshot = aaronnoteDiagnostics.snapshot();
    this.statusList.replaceChildren(...snapshot.components.map(statusRow));

    const entries = snapshot.logs.slice(-80).map((entry) => {
      const line = document.createElement("div");
      line.className = "aaronnote-diagnostics-log-entry";
      line.dataset.level = entry.level;
      const time = document.createElement("time");
      time.textContent = entry.time;
      const message = document.createElement("span");
      message.textContent = entry.message;
      line.append(time, message);
      return line;
    });
    this.logList.replaceChildren(...entries);
    this.logList.scrollTop = this.logList.scrollHeight;
  }
}

export function installAaronnoteDiagnosticsPanel(app: JupyterFrontEnd): AaronnoteDiagnosticsPanel {
  const panel = new AaronnoteDiagnosticsPanel();
  app.shell.add(panel, "right", { rank: 850 });
  aaronnoteDiagnostics.log("Diagnostics panel ready");
  Object.assign(globalThis, {
    __aaronnoteJupyterDiagnostics: {
      installed: true,
      version: "0.2.0",
      snapshot: () => aaronnoteDiagnostics.snapshot(),
    },
  });
  return panel;
}
