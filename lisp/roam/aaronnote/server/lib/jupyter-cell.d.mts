// Minimal ambient types for the Node cell service so TypeScript callers (tests,
// tooling) can import it. The implementation lives in jupyter-cell.mjs.

export interface JupyterCellServiceOptions {
  runtimeRoot?: string;
  noteRoot?: string;
  workspaceRoot?: string;
  stdout?: NodeJS.WritableStream;
  stderr?: NodeJS.WritableStream;
}

export interface JupyterCellService {
  execute(body?: Record<string, unknown>): Promise<Record<string, any>>;
  kernels(): Promise<Record<string, any>>;
  openScript(body?: Record<string, unknown>): Promise<Record<string, any>>;
  readScriptCell(body?: Record<string, unknown>): Promise<Record<string, any>>;
  executeScriptCell(body?: Record<string, unknown>): Promise<Record<string, any>>;
  clearScriptCellOutput(body?: Record<string, unknown>): Promise<Record<string, any>>;
  clearAllOutputs(body?: Record<string, unknown>): Promise<Record<string, any>>;
  variables(body?: Record<string, unknown>): Promise<Record<string, any>>;
  kernelStatus(body?: Record<string, unknown>): Record<string, any>;
  restart(body?: Record<string, unknown>): Promise<Record<string, any>>;
  interrupt(body?: Record<string, unknown>): Promise<Record<string, any>>;
  shutdownKernel(body?: Record<string, unknown>): Promise<Record<string, any>>;
  widgetProxyTarget(pathname: string, search?: string, websocket?: boolean): string | null;
  touchKernelById(id: string): boolean;
  listTasks(): Promise<Record<string, any>>;
  cleanup(body?: Record<string, unknown>): Promise<Record<string, any>>;
  shutdown(): Promise<void>;
}

export function createJupyterCellService(options?: JupyterCellServiceOptions): JupyterCellService;
