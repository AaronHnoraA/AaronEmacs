import type { CursorPosition, Inbound, SnippetSummary, UnusedAsset } from "./types.ts";

type OpenMsg = Extract<Inbound, { type: "open" }>;
type SavedMsg = Extract<Inbound, { type: "saved" }>;
type NotesMsg = Extract<Inbound, { type: "notes" }>;
type PositionsMsg = Extract<Inbound, { type: "positions" }>;
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
type AssetStoreMsg = {
  ok?: boolean;
  file?: string;
  name?: string;
  type?: string;
  isImage?: boolean;
  markdownPath?: string;
  message?: string;
};
type ProseCheckBody = {
  file?: string;
  content?: string;
  ranges?: Array<{ from: number; to: number }>;
  segments?: Array<{ from: number; text: string }>;
  totalChars?: number;
};
type ProseCheckMsg = {
  ok?: boolean;
  diagnostics?: Array<{
    source: "vale" | "cspell" | "browser";
    from: number;
    to: number;
    severity?: "info" | "warning" | "error";
    message: string;
    rule?: string;
    word?: string;
    suggestions?: string[];
  }>;
  tools?: Array<{ source?: string; ok?: boolean; message?: string; partial?: boolean; optional?: boolean }>;
  scope?: { checkedChars?: number; totalChars?: number; partial?: boolean };
};
export type TodoItem = Record<string, unknown> & {
  id?: string;
  file?: string;
  path?: string;
  note?: string;
  noteId?: string;
  noteTitle?: string;
  title?: string;
  text?: string;
  source?: string;
  status?: string;
  ddl?: string;
  deadline?: string;
  due?: string;
  line?: number;
  index?: number;
  tags?: string[];
  inlineTags?: string[];
};
export type TodosMsg = {
  type?: string;
  todos?: TodoItem[];
  root?: string;
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
    notesIndex?: () => Promise<unknown>;
    todos?: (file: string) => Promise<unknown>;
    updateTodo?: (body: Record<string, unknown>) => Promise<unknown>;
  };
  completions?: {
    tags?: (prefix: string) => Promise<unknown>;
    roam?: (prefix: string) => Promise<unknown>;
  };
  clipboard?: {
    read?: (body?: { file?: string }) => Promise<unknown>;
  };
  noteCode?: {
    readRegion?: (body?: unknown) => Promise<unknown>;
  };
  latex?: {
    defaults?: (body?: Record<string, unknown>) => Promise<unknown>;
    agentStatus?: () => Promise<unknown>;
    setAgent?: (body?: Record<string, unknown>) => Promise<unknown>;
    templates?: () => Promise<unknown>;
    chooseOutputPath?: (body?: Record<string, unknown>) => Promise<unknown>;
    export?: (body?: Record<string, unknown>) => Promise<unknown>;
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
    currentFile?: (body: string | { file: string; client?: string }) => Promise<unknown>;
    key?: (keyString: string) => Promise<unknown>;
    systemOpen?: (target: string, base?: string) => Promise<unknown>;
  };
  roamTools?: {
    renameTag?: (body: Record<string, unknown>) => Promise<unknown>;
    deleteTag?: (body: Record<string, unknown>) => Promise<unknown>;
    tagOverlap?: () => Promise<unknown>;
    rewritePathRefs?: (body: Record<string, unknown>) => Promise<unknown>;
  };
  session?: {
    getPositions?: () => Promise<unknown>;
    savePosition?: (position: Partial<CursorPosition> & { file: string }) => Promise<unknown>;
  };
  assets?: {
    upload?: (body: { file?: string; name?: string; type?: string; data?: string }) => Promise<unknown>;
    storeFromPath?: (body: { file?: string; path?: string; source?: string; name?: string; type?: string }) => Promise<unknown>;
    renderTikz?: (body: { file: string; id: string; timestamp: string; source: string }) => Promise<unknown>;
    scanOrphans?: () => Promise<unknown>;
    trashOrphans?: (files: string[]) => Promise<unknown>;
  };
  ime?: {
    vimMode?: (mode: string) => Promise<unknown>;
  };
  proseCheck?: {
    run?: (body: ProseCheckBody) => Promise<unknown>;
    acceptWord?: (word: string) => Promise<unknown>;
  };
  config?: {
    katexMacros?: () => Promise<unknown>;
  };
};

export type LatexTemplateVar = { id: string; label: string; default: string };
export type LatexTemplate = { key: string; file: string; name: string; engine: string; vars: LatexTemplateVar[] };
export type LatexTemplatesResult = { type?: string; ok?: boolean; templates?: LatexTemplate[]; root?: string };
export type LatexExportAgentStatus = {
  type?: string;
  ok?: boolean;
  agent?: string;
  engine?: string;
  agents?: Array<{ id: string; label?: string; current?: boolean; available?: boolean }>;
};

export type KatexMacrosResult = {
  type?: string;
  dir?: string;
  macros?: Record<string, string>;
  errors?: { file: string; message: string }[];
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
    async todos(file = ""): Promise<TodosMsg> {
      const call = requireMethod(nativeApi().notes?.todos, "Todo agenda");
      return ensureOk(await call(file) as TodosMsg, "Todo agenda failed");
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
    async updateTodo(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().notes?.updateTodo, "Todo update");
      return ensureOk(await call(body) as Record<string, unknown>, "Todo update failed");
    },
  },
  noteCode: {
    async readRegion(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().noteCode?.readRegion, "Note code");
      return ensureOk(await call(body) as Record<string, unknown>, "Note code failed");
    },
  },
  latex: {
    async defaults(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().latex?.defaults, "LaTeX export defaults");
      return ensureOk(await call(body) as Record<string, unknown>, "LaTeX export defaults failed");
    },
    async agentStatus(): Promise<LatexExportAgentStatus> {
      const call = requireMethod(nativeApi().latex?.agentStatus, "LaTeX export agent status");
      return ensureOk(await call() as LatexExportAgentStatus, "LaTeX export agent status failed");
    },
    async setAgent(body: Record<string, unknown>): Promise<LatexExportAgentStatus> {
      const call = requireMethod(nativeApi().latex?.setAgent, "LaTeX export agent switch");
      return ensureOk(await call(body) as LatexExportAgentStatus, "LaTeX export agent switch failed");
    },
    async templates(): Promise<LatexTemplatesResult> {
      const call = requireMethod(nativeApi().latex?.templates, "LaTeX templates");
      return ensureOk(await call() as LatexTemplatesResult, "LaTeX templates failed");
    },
    async chooseOutputPath(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().latex?.chooseOutputPath, "LaTeX output path chooser");
      return await call(body) as Record<string, unknown>;
    },
    async export(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().latex?.export, "LaTeX export");
      return ensureOk(await call(body) as Record<string, unknown>, "LaTeX export failed");
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
    async currentFile(file: string, client = ""): Promise<void> {
      const call = window.aaronnoteApi?.emacs?.currentFile;
      if (!call) return;
      const body = client ? { file, client } : file;
      await call(body).catch(() => {});
    },
    async key(keyString: string): Promise<void> {
      const call = window.aaronnoteApi?.emacs?.key;
      if (!call) return;
      await call(keyString).catch(() => {});
    },
    async systemOpen(target: string, base?: string): Promise<void> {
      const call = window.aaronnoteApi?.emacs?.systemOpen;
      if (!call) {
        window.location.href = target;
        return;
      }
      await call(target, base);
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
  session: {
    async getPositions(): Promise<PositionsMsg> {
      const call = window.aaronnoteApi?.session?.getPositions;
      if (!call) return { type: "positions", positions: [] };
      return ensureOk(await call() as PositionsMsg, "Cursor positions failed");
    },
    async savePosition(position: Partial<CursorPosition> & { file: string }): Promise<PositionsMsg> {
      const call = window.aaronnoteApi?.session?.savePosition;
      if (!call) return { type: "positions", positions: [] };
      return ensureOk(await call(position) as PositionsMsg, "Cursor position save failed");
    },
  },
  completions: {
    async tags(prefix = ""): Promise<{ tags?: string[] }> {
      const call = window.aaronnoteApi?.completions?.tags;
      if (!call) return { tags: [] };
      return await call(prefix) as { tags?: string[] };
    },
    async roam(prefix = ""): Promise<{ notes?: Array<{ id: string; key: string; title: string; path: string }> }> {
      const call = window.aaronnoteApi?.completions?.roam;
      if (!call) return { notes: [] };
      return await call(prefix) as { notes?: Array<{ id: string; key: string; title: string; path: string }> };
    },
  },
  clipboard: {
    async read(body: { file?: string } = {}): Promise<unknown> {
      const call = requireMethod(nativeApi().clipboard?.read, "Clipboard read");
      return await call(body);
    },
  },
  assets: {
    async upload(body: { file?: string; name?: string; type?: string; data?: string }): Promise<AssetStoreMsg> {
      const call = requireMethod(nativeApi().assets?.upload, "Asset upload");
      return ensureOk(await call(body) as AssetStoreMsg, "Asset upload failed");
    },
    async storeFromPath(body: { file?: string; path?: string; source?: string; name?: string; type?: string }): Promise<AssetStoreMsg> {
      const call = requireMethod(nativeApi().assets?.storeFromPath, "Asset import");
      return ensureOk(await call(body) as AssetStoreMsg, "Asset import failed");
    },
    async renderTikz(body: { file: string; id: string; timestamp: string; source: string }) {
      const call = requireMethod(nativeApi().assets?.renderTikz, "TikZ render");
      return ensureOk(await call(body) as { ok?: boolean; file?: string; markdownPath?: string; message?: string }, "TikZ render failed");
    },
    async scanOrphans(): Promise<Record<string, unknown> & { assets?: UnusedAsset[]; message?: string }> {
      const call = requireMethod(nativeApi().assets?.scanOrphans, "Asset scan");
      return ensureOk(await call() as Record<string, unknown> & { assets?: UnusedAsset[]; message?: string }, "Asset scan failed");
    },
    async trashOrphans(files: string[]): Promise<Record<string, unknown> & { assets?: UnusedAsset[]; trashed?: unknown[]; message?: string }> {
      const call = requireMethod(nativeApi().assets?.trashOrphans, "Asset trash");
      return ensureOk(await call(files) as Record<string, unknown> & { assets?: UnusedAsset[]; trashed?: unknown[]; message?: string }, "Asset trash failed");
    },
  },
  ime: {
    async vimMode(mode: "normal" | "insert"): Promise<{ enabled?: boolean }> {
      const call = window.aaronnoteApi?.ime?.vimMode;
      if (!call) return { enabled: false };
      try {
        return (await call(mode)) as { enabled?: boolean } ?? { enabled: false };
      } catch (_) {
        return {};
      }
    },
  },
  proseCheck: {
    async run(body: ProseCheckBody): Promise<ProseCheckMsg> {
      const call = requireMethod(nativeApi().proseCheck?.run, "Prose check");
      return ensureOk(await call(body) as ProseCheckMsg, "Prose check failed");
    },
    async acceptWord(word: string): Promise<{ ok?: boolean; word?: string }> {
      const call = requireMethod(nativeApi().proseCheck?.acceptWord, "Prose dictionary");
      return ensureOk(await call(word) as { ok?: boolean; word?: string }, "Adding word failed");
    },
  },
  config: {
    async katexMacros(): Promise<KatexMacrosResult> {
      const call = nativeApi().config?.katexMacros;
      if (!call) return {};
      return (await call()) as KatexMacrosResult;
    },
  },
};
