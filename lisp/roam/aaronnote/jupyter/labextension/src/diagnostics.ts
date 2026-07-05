export type AaronnoteComponentLevel = "ok" | "waiting" | "error";

export type AaronnoteComponentStatus = {
  id: string;
  label: string;
  level: AaronnoteComponentLevel;
  detail: string;
};

export type AaronnoteLogEntry = {
  time: string;
  level: "info" | "error";
  message: string;
};

type Listener = () => void;

export class AaronnoteDiagnostics {
  private readonly components = new Map<string, AaronnoteComponentStatus>();
  private readonly logs: AaronnoteLogEntry[] = [];
  private readonly listeners = new Set<Listener>();

  setComponent(status: AaronnoteComponentStatus): void {
    this.components.set(status.id, status);
    this.emit();
  }

  log(message: string, level: "info" | "error" = "info"): void {
    this.logs.push({
      time: new Date().toLocaleTimeString([], { hour12: false }),
      level,
      message,
    });
    if (this.logs.length > 200) this.logs.splice(0, this.logs.length - 200);
    this.emit();
  }

  snapshot(): { components: AaronnoteComponentStatus[]; logs: AaronnoteLogEntry[] } {
    return {
      components: Array.from(this.components.values()),
      logs: [...this.logs],
    };
  }

  clearLogs(): void {
    this.logs.length = 0;
    this.emit();
  }

  subscribe(listener: Listener): () => void {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  }

  private emit(): void {
    for (const listener of this.listeners) listener();
  }
}

export const aaronnoteDiagnostics = new AaronnoteDiagnostics();
