import type { Inbound } from "./types.ts";

type OpenMsg = Extract<Inbound, { type: "open" }>;
type SavedMsg = Extract<Inbound, { type: "saved" }>;
type SaveBody = {
  file: string;
  content: string;
  mode: string;
  clientId: string;
  seq: number;
  baseMtimeMs?: number;
  refresh?: string;
};
type NativeApi = {
  notes?: {
    bootstrap?: (file?: string) => Promise<unknown>;
    open?: (file: string) => Promise<unknown>;
    save?: (body: SaveBody) => Promise<unknown>;
  };
  assets?: {
    renderTikz?: (body: { file: string; id: string; timestamp: string; source: string }) => Promise<unknown>;
  };
};

declare global {
  interface Window {
    aaronnoteApi?: NativeApi;
  }
}

function requireMethod<T extends (...args: any[]) => unknown>(method: T | undefined, feature: string): T {
  if (!method) throw new Error(`${feature} is unavailable`);
  return method;
}

function nativeApi(): NativeApi {
  if (!window.aaronnoteApi) throw new Error("AaronNote host bridge is unavailable");
  return window.aaronnoteApi;
}

function ensureOk<T>(value: T, fallback: string, allowConflict = false): T {
  const result = value as T & { ok?: boolean; conflict?: boolean; message?: string };
  if (result?.ok === false && !(allowConflict && result.conflict)) {
    throw new Error(result.message || fallback);
  }
  return value;
}

export const api = {
  notes: {
    async bootstrap(file?: string): Promise<OpenMsg> {
      const call = requireMethod(nativeApi().notes?.bootstrap, "Open");
      return ensureOk(await call(file) as OpenMsg, "Open failed");
    },
    async open(file: string): Promise<OpenMsg> {
      const call = requireMethod(nativeApi().notes?.open, "Open");
      return ensureOk(await call(file) as OpenMsg, "Open failed");
    },
    async save(body: SaveBody): Promise<SavedMsg> {
      const call = requireMethod(nativeApi().notes?.save, "Save");
      return ensureOk(await call(body) as SavedMsg, "Save failed", true);
    },
    saveKeepalive(body: SaveBody): void {
      const call = requireMethod(nativeApi().notes?.save, "Save");
      void call(body).catch(() => {});
    },
  },
  assets: {
    async renderTikz(body: { file: string; id: string; timestamp: string; source: string }) {
      const call = requireMethod(nativeApi().assets?.renderTikz, "TikZ render");
      return ensureOk(await call(body) as { ok?: boolean; file?: string; markdownPath?: string; message?: string }, "TikZ render failed");
    },
  },
};
