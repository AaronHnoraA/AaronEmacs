import { join } from "node:path";
import { inspect } from "node:util";

const MAX_LOGS = 300;
const MAX_TASKS = 80;
const POWER_EVENTS = [
  "suspend",
  "resume",
  "on-ac",
  "on-battery",
  "shutdown",
  "lock-screen",
  "unlock-screen",
  "user-did-become-active",
  "user-did-resign-active",
  "thermal-state-change",
  "speed-limit-change",
];

function boundedPush(items, item, limit) {
  items.push(item);
  if (items.length > limit) items.splice(0, items.length - limit);
}

function debugText(value) {
  if (value instanceof Error) return value.stack || value.message;
  if (typeof value === "string") return value;
  return inspect(value, { breakLength: 180, depth: 3, maxArrayLength: 20 });
}

function thermalState(powerMonitor) {
  try {
    return powerMonitor.getCurrentThermalState?.() || "";
  } catch {
    return "";
  }
}

function speedLimit(powerMonitor) {
  try {
    return powerMonitor.getCurrentSpeedLimit?.() ?? null;
  } catch {
    return null;
  }
}

function idleState(powerMonitor) {
  try {
    return {
      seconds: powerMonitor.getSystemIdleTime(),
      state: powerMonitor.getSystemIdleState(60),
    };
  } catch {
    return null;
  }
}

class DebugPanel {
  constructor(options) {
    this.options = options;
    this.tasks = [];
    this.logs = [];
    this.powerEvents = [];
    this.sequence = 0;
    this.interval = null;
    this.windowListeners = new Map();
    this.powerListeners = [];
    this.restoreConsole = null;
    this.stopped = false;
    this.onCloseRequest = (event) => {
      if (event.sender === this.win.webContents) this.win.close();
    };
    this.win = this.createWindow();
    this.start();
  }

  createWindow() {
    const win = new this.options.BrowserWindow({
      width: 1080,
      height: 760,
      minWidth: 760,
      minHeight: 520,
      title: "AaronNote Debug",
      backgroundColor: "#f5f1e8",
      webPreferences: {
        contextIsolation: true,
        nodeIntegration: false,
        preload: join(this.options.desktopDir, "debug-preload.cjs"),
        sandbox: true,
      },
    });
    win.on("closed", () => this.stop());
    void win.loadFile(join(this.options.desktopDir, "debug-panel.html"));
    return win;
  }

  show() {
    if (this.win.isMinimized()) this.win.restore();
    this.win.show();
    this.win.focus();
    this.send();
  }

  async trackTask(name, run) {
    const task = {
      id: ++this.sequence,
      name,
      state: "running",
      startedAt: Date.now(),
      endedAt: 0,
      message: "",
    };
    boundedPush(this.tasks, task, MAX_TASKS);
    this.send();
    try {
      const result = await run();
      task.state = result?.ok === false ? "returned error" : "done";
      task.message = result?.ok === false ? String(result.message || "") : "";
      return result;
    } catch (err) {
      task.state = "failed";
      task.message = err instanceof Error ? err.message : String(err);
      throw err;
    } finally {
      task.endedAt = Date.now();
      this.send();
    }
  }

  start() {
    this.options.ipcMain.on("aaronnote:debug:close", this.onCloseRequest);
    this.patchConsole();
    for (const win of this.options.appWindows()) this.observeWindow(win);
    this.observePower();
    this.interval = setInterval(() => this.send(), 1000);
  }

  stop() {
    if (this.stopped) return;
    this.stopped = true;
    if (this.interval) clearInterval(this.interval);
    this.interval = null;
    this.options.ipcMain.off("aaronnote:debug:close", this.onCloseRequest);
    for (const [webContents, listener] of this.windowListeners) {
      if (!webContents.isDestroyed()) webContents.off("console-message", listener);
    }
    this.windowListeners.clear();
    for (const [name, listener] of this.powerListeners) this.options.powerMonitor.off(name, listener);
    this.powerListeners = [];
    this.restoreConsole?.();
    this.restoreConsole = null;
    this.tasks.length = 0;
    this.logs.length = 0;
    this.powerEvents.length = 0;
    this.options.onClose();
  }

  patchConsole() {
    const warn = console.warn;
    const error = console.error;
    console.warn = (...args) => {
      this.log("main", "warning", args.map(debugText).join(" "));
      warn.apply(console, args);
    };
    console.error = (...args) => {
      this.log("main", "error", args.map(debugText).join(" "));
      error.apply(console, args);
    };
    this.restoreConsole = () => {
      console.warn = warn;
      console.error = error;
    };
  }

  observeWindow(win) {
    if (!win?.aaronnoteAppWindow || win.isDestroyed()) return;
    const { webContents } = win;
    if (this.windowListeners.has(webContents)) return;
    const listener = (details, deprecatedLevel, deprecatedMessage, deprecatedLine, deprecatedSourceId) => {
      const level = typeof details?.level === "string"
        ? details.level
        : deprecatedLevel >= 3
          ? "error"
          : deprecatedLevel === 2
            ? "warning"
            : "info";
      if (level !== "warning" && level !== "error") return;
      const message = details?.message || deprecatedMessage || "";
      const line = details?.lineNumber || deprecatedLine || 0;
      const source = details?.sourceId || deprecatedSourceId || "";
      this.log("renderer", level, String(message), source ? `${source}:${line}` : "");
    };
    webContents.on("console-message", listener);
    this.windowListeners.set(webContents, listener);
    webContents.once("destroyed", () => this.windowListeners.delete(webContents));
  }

  observePower() {
    for (const name of POWER_EVENTS) {
      const listener = (details = {}) => {
        boundedPush(this.powerEvents, {
          at: Date.now(),
          name,
          state: details.state || "",
          limit: details.limit ?? "",
        }, 40);
        this.send();
      };
      this.options.powerMonitor.on(name, listener);
      this.powerListeners.push([name, listener]);
    }
  }

  log(processName, level, message, source = "") {
    boundedPush(this.logs, { at: Date.now(), process: processName, level, message, source }, MAX_LOGS);
    this.send();
  }

  snapshot() {
    return {
      sampledAt: Date.now(),
      tasks: this.tasks.slice().reverse(),
      logs: this.logs.slice().reverse(),
      power: {
        thermalState: thermalState(this.options.powerMonitor),
        speedLimit: speedLimit(this.options.powerMonitor),
        idle: idleState(this.options.powerMonitor),
        events: this.powerEvents.slice().reverse(),
        metrics: this.options.app.getAppMetrics().map((metric) => ({
          pid: metric.pid,
          type: metric.type,
          cpu: metric.cpu?.percentCPUUsage ?? 0,
          memoryKb: metric.memory?.workingSetSize ?? 0,
        })),
      },
      runtime: this.options.runtimeSnapshot(),
    };
  }

  send() {
    if (!this.win || this.win.isDestroyed() || this.win.webContents.isDestroyed()) return;
    this.win.webContents.send("aaronnote:debug:snapshot", this.snapshot());
  }
}

export function createDebugPanel(options) {
  return new DebugPanel(options);
}
