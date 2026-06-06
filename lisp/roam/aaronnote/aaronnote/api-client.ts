import type { Inbound, SnippetSummary } from "./types.ts";

type OpenMsg = Extract<Inbound, { type: "open" }>;
type SavedMsg = Extract<Inbound, { type: "saved" }>;
type NotesMsg = Extract<Inbound, { type: "notes" }>;
type SnippetsMsg = Extract<Inbound, { type: "snippets" }>;
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
    list?: (force?: boolean) => Promise<unknown>;
    pathSuggestions?: (file: string, prefix?: string) => Promise<unknown>;
    save?: (body: SaveBody) => Promise<unknown>;
    saveKeepalive?: (body: SaveBody) => void;
    snippets?: () => Promise<unknown>;
    metaAdd?: (body: Record<string, unknown>) => Promise<unknown>;
  };
  noteCode?: {
    readRegion?: (body?: unknown) => Promise<unknown>;
  };
  meta?: {
    add?: (body: Record<string, unknown>) => Promise<unknown>;
    remove?: (body: Record<string, unknown>) => Promise<unknown>;
    tag?: (body: Record<string, unknown>) => Promise<unknown>;
    hideRoam?: (body: Record<string, unknown>) => Promise<unknown>;
    activateRoam?: (body: Record<string, unknown>) => Promise<unknown>;
  };
  emacs?: {
    open?: (body: { file: string; tag?: string; line?: number; col?: number }) => Promise<unknown>;
    currentFile?: (file: string) => Promise<unknown>;
  };
  roamTools?: {
    renameTag?: (body: Record<string, unknown>) => Promise<unknown>;
    deleteTag?: (body: Record<string, unknown>) => Promise<unknown>;
    tagOverlap?: () => Promise<unknown>;
    rewritePathRefs?: (body: Record<string, unknown>) => Promise<unknown>;
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
    async list(force = false): Promise<NotesMsg> {
      const call = requireMethod(nativeApi().notes?.list, "Note index");
      return ensureOk(await call(force) as NotesMsg, "Note index failed");
    },
    async pathSuggestions(file: string, prefix = "./"): Promise<{ paths?: string[] }> {
      const call = requireMethod(nativeApi().notes?.pathSuggestions, "Path suggestions");
      return ensureOk(await call(file, prefix) as { paths?: string[] }, "Path suggestions failed");
    },
    async save(body: SaveBody): Promise<SavedMsg> {
      const call = requireMethod(nativeApi().notes?.save, "Save");
      return ensureOk(await call(body) as SavedMsg, "Save failed", true);
    },
    async snippets(): Promise<SnippetsMsg & { snippets?: SnippetSummary[] }> {
      const call = requireMethod(nativeApi().notes?.snippets, "Snippet reload");
      return ensureOk(await call() as SnippetsMsg & { snippets?: SnippetSummary[] }, "Snippet reload failed");
    },
    saveKeepalive(body: SaveBody): void {
      const api = window.aaronnoteApi?.notes;
      if (!api) return;
      if (api.saveKeepalive) {
        api.saveKeepalive(body);
        return;
      }
      if (api.save) void api.save(body).catch(() => {});
    },
  },
  noteCode: {
    async readRegion(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().noteCode?.readRegion, "Note code");
      return ensureOk(await call(body) as Record<string, unknown>, "Note code failed");
    },
  },
  meta: {
    async add(body: Record<string, unknown>): Promise<OpenMsg> {
      const call = requireMethod(nativeApi().meta?.add ?? nativeApi().notes?.metaAdd, "Meta add");
      return ensureOk(await call(body) as OpenMsg, "Meta add failed");
    },
    async remove(body: Record<string, unknown>): Promise<OpenMsg> {
      const call = requireMethod(nativeApi().meta?.remove, "Meta remove");
      return ensureOk(await call(body) as OpenMsg, "Meta remove failed");
    },
    async tag(body: Record<string, unknown>): Promise<OpenMsg> {
      const call = requireMethod(nativeApi().meta?.tag, "Tag add");
      return ensureOk(await call(body) as OpenMsg, "Tag add failed");
    },
    async hideRoam(body: Record<string, unknown>): Promise<OpenMsg> {
      const call = requireMethod(nativeApi().meta?.hideRoam, "Roam hide");
      return ensureOk(await call(body) as OpenMsg, "Roam hide failed");
    },
    async activateRoam(body: Record<string, unknown>): Promise<OpenMsg> {
      const call = requireMethod(nativeApi().meta?.activateRoam, "Roam activate");
      return ensureOk(await call(body) as OpenMsg, "Roam activate failed");
    },
  },
  emacs: {
    async open(body: { file: string; tag?: string; line?: number; col?: number }): Promise<void> {
      const call = window.aaronnoteApi?.emacs?.open;
      if (!call) return;
      await call(body).catch(() => {});
    },
    async currentFile(file: string): Promise<void> {
      const call = window.aaronnoteApi?.emacs?.currentFile;
      if (!call) return;
      await call(file).catch(() => {});
    },
  },
  roamTools: {
    async renameTag(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().roamTools?.renameTag, "Rename tag");
      return ensureOk(await call(body) as Record<string, unknown>, "Rename tag failed");
    },
    async deleteTag(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().roamTools?.deleteTag, "Delete tag");
      return ensureOk(await call(body) as Record<string, unknown>, "Delete tag failed");
    },
    async tagOverlap(): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().roamTools?.tagOverlap, "Tag overlap");
      return ensureOk(await call() as Record<string, unknown>, "Tag overlap failed");
    },
    async rewritePathRefs(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().roamTools?.rewritePathRefs, "Rewrite path refs");
      return ensureOk(await call(body) as Record<string, unknown>, "Rewrite path refs failed");
    },
  },
  assets: {
    async renderTikz(body: { file: string; id: string; timestamp: string; source: string }) {
      const call = requireMethod(nativeApi().assets?.renderTikz, "TikZ render");
      return ensureOk(await call(body) as { ok?: boolean; file?: string; markdownPath?: string; message?: string }, "TikZ render failed");
    },
  },
};
