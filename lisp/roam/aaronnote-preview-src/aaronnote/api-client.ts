// Optional native API client retained by rendering widgets. Runtime calls fail
// loudly when the preview host does not provide the bridge.

import type {
  NoteSummary, DirectorySummary, FileSummary,
  SnippetSummary, Inbound,
  UnusedAsset, CursorPosition, RecentNote,
  UploadedAsset, PluginSummary,
  GitChange, GitCommitEntry, GitRepoStatus,
} from "./types.ts";
import type {
  LeanGoalsResponse, LeanTermGoalResponse, LeanHoverResponse, LeanCompletionResponse,
  LeanOpenRegionResponse, LeanUpdateRegionResponse, LeanRegionRead,
  LeanDiagnosticsPush, LeanProgressPush, LeanSemanticTokensPush,
} from "../src/types/lean-ipc.ts";

type IndexPayload = { notes?: NoteSummary[]; directories?: DirectorySummary[]; files?: FileSummary[] };
type OpenMsg = Extract<Inbound, { type: "open" }>;
type SavedMsg = Extract<Inbound, { type: "saved" }>;
type NotesMsg = Extract<Inbound, { type: "notes" }>;
type TemplatesMsg = Extract<Inbound, { type: "templates" }>;

type SaveBody = {
  file: string;
  content: string;
  mode: string;
  clientId: string;
  seq: number;
  baseMtimeMs?: number;
  refresh?: string;
  force?: boolean;
};

export type ProseDiagnostic = {
  source: "vale" | "cspell" | "browser";
  from: number;
  to: number;
  severity?: "info" | "warning" | "error";
  message: string;
  rule?: string;
  word?: string;
  suggestions?: string[];
};

type NativeApi = {
  notes?: {
    bootstrap?: (file?: string) => Promise<unknown>;
    open?: (file: string) => Promise<unknown>;
    list?: (force?: boolean) => Promise<unknown>;
    save?: (body: SaveBody) => Promise<unknown>;
    createNode?: (draft: Record<string, unknown>) => Promise<unknown>;
    deleteNote?: (file: string) => Promise<unknown>;
    createFolder?: (path: string) => Promise<unknown>;
    pathSuggestions?: (file: string) => Promise<unknown>;
    roamSync?: (reload?: boolean) => Promise<unknown>;
    roamSyncFull?: () => Promise<unknown>;
    templates?: (force?: boolean) => Promise<unknown>;
    snippets?: () => Promise<unknown>;
    todos?: (file?: string) => Promise<unknown>;
    metaAdd?: (body: { file: string; content: string; title: string; tags: string[] }) => Promise<unknown>;
  };
  roamTools?: {
    renameTag?: (body: Record<string, unknown>) => Promise<unknown>;
    deleteTag?: (body: Record<string, unknown>) => Promise<unknown>;
    tagOverlap?: () => Promise<unknown>;
    rewritePathRefs?: (body: Record<string, unknown>) => Promise<unknown>;
    fileHistory?: (file: string) => Promise<unknown>;
    restoreFileVersion?: (body: { file: string; sha: string }) => Promise<unknown>;
    discardFileChanges?: (file: string) => Promise<unknown>;
    repoStatus?: () => Promise<unknown>;
    repoHistory?: (limit?: number) => Promise<unknown>;
    changes?: () => Promise<unknown>;
    diff?: (body: { file?: string; path?: string; scope?: string; sha?: string }) => Promise<unknown>;
    commitDiff?: (sha: string) => Promise<unknown>;
    pull?: () => Promise<unknown>;
    push?: () => Promise<unknown>;
    commit?: (message: string) => Promise<unknown>;
  };
  assets?: {
    upload?: (body: { file: string; name: string; type: string; data: string }) => Promise<unknown>;
    storeFromPath?: (body: { file: string; path: string; name?: string; type?: string }) => Promise<unknown>;
    renderTikz?: (body: { file: string; id: string; timestamp: string; source: string }) => Promise<unknown>;
    scanOrphans?: () => Promise<unknown>;
    trashOrphans?: (files: string[]) => Promise<unknown>;
  };
  session?: {
    getRecent?: () => Promise<unknown>;
    touchRecent?: (file: string, openedAt: number) => Promise<unknown>;
    getPositions?: () => Promise<unknown>;
    savePosition?: (position: CursorPosition) => Promise<unknown>;
  };
  plugins?: {
    list?: () => Promise<unknown>;
    getOverrides?: () => Promise<unknown>;
    saveOverrides?: (overrides: Record<string, unknown>) => Promise<unknown>;
  };
  fs?: {
    rename?: (body: Record<string, unknown>) => Promise<unknown>;
    move?: (body: Record<string, unknown>) => Promise<unknown>;
    duplicate?: (body: Record<string, unknown>) => Promise<unknown>;
    trash?: (body: Record<string, unknown>) => Promise<unknown>;
  };
  meta?: {
    add?: (body: Record<string, unknown>) => Promise<unknown>;
    remove?: (body: Record<string, unknown>) => Promise<unknown>;
    tag?: (body: Record<string, unknown>) => Promise<unknown>;
    hideRoam?: (body: Record<string, unknown>) => Promise<unknown>;
    activateRoam?: (body: Record<string, unknown>) => Promise<unknown>;
  };
  shell?: {
    showInFolder?: (file: string) => Promise<unknown>;
    openPath?: (file: string) => Promise<unknown>;
    openDirectory?: (path: string, base?: string) => Promise<unknown>;
    openDirectoryInKitty?: (path: string, base?: string) => Promise<unknown>;
    showAttachmentMenu?: (file: string, base?: string, options?: unknown) => Promise<unknown>;
    showEditorContextMenu?: (options?: unknown) => Promise<unknown>;
    showLeanEditorMenu?: (options?: unknown) => Promise<unknown>;
    openLeanLocation?: (target: { file: string; line: number; character: number }) => Promise<{ ok?: boolean; message?: string }>;
  };
  externalEditor?: {
    open?: (target?: unknown) => Promise<unknown>;
  };
  jupyter?: {
    request?: (action: string, body?: unknown) => Promise<unknown>;
    scroll?: (body?: unknown) => Promise<unknown>;
    kernelStatus?: (body?: unknown) => Promise<unknown>;
    onStatus?: (handler: (data: unknown) => void) => () => void;
  };
  proseCheck?: {
    run?: (body: { file?: string; content?: string; ranges?: Array<{ from: number; to: number }>; segments?: Array<{ from: number; to: number; text: string }>; totalChars?: number }) => Promise<unknown>;
    browserSpellcheck?: (words: string[]) => Array<{ word?: string; misspelled?: boolean; suggestions?: string[] }>;
  };
  copilot?: {
    request?: (action: string, body?: unknown) => Promise<unknown>;
    status?: () => Promise<unknown>;
    inline?: (body?: unknown) => Promise<unknown>;
    shown?: (body?: unknown) => Promise<unknown>;
    accept?: (body?: unknown) => Promise<unknown>;
    signIn?: (body?: unknown) => Promise<unknown>;
    signOut?: (body?: unknown) => Promise<unknown>;
    quota?: (body?: unknown) => Promise<unknown>;
    log?: (body?: unknown) => Promise<unknown>;
  };
  roamlookup?: {
    request?: (action: string, body?: unknown) => Promise<unknown>;
    status?: () => Promise<unknown>;
    start?: (body?: unknown) => Promise<unknown>;
    query?: (body?: unknown) => Promise<unknown>;
    close?: (body?: unknown) => Promise<unknown>;
  };
  lean?: {
    request?: (action: string, body?: unknown) => Promise<unknown>;
    status?: () => Promise<unknown>;
    openNote?: (body?: unknown) => Promise<unknown>;
    changeNote?: (body?: unknown) => Promise<unknown>;
    closeNote?: (body?: unknown) => Promise<unknown>;
    saveNote?: (body?: unknown) => Promise<unknown>;
    deleteNote?: (body?: unknown) => Promise<unknown>;
    renameNote?: (body?: unknown) => Promise<unknown>;
    getGoals?: (body?: unknown) => Promise<unknown>;
    getTermGoal?: (body?: unknown) => Promise<unknown>;
    getHover?: (body?: unknown) => Promise<unknown>;
    getCompletions?: (body?: unknown) => Promise<unknown>;
    rpcCall?: (body?: unknown) => Promise<unknown>;
    getDefinition?: (body?: unknown) => Promise<unknown>;
    getDiagnostics?: (body?: unknown) => Promise<unknown>;
    lspRequest?: (body?: unknown) => Promise<unknown>;
    lspNotify?: (body?: unknown) => Promise<unknown>;
    createRpcSession?: (body?: unknown) => Promise<unknown>;
    closeRpcSession?: (body?: unknown) => Promise<unknown>;
    rpcRelease?: (body?: unknown) => Promise<unknown>;
    cacheStatus?: () => Promise<unknown>;
    cacheGet?: (body?: unknown) => Promise<unknown>;
    ensureRegion?: (body?: unknown) => Promise<unknown>;
    readRegion?: (body?: unknown) => Promise<unknown>;
    updateRegion?: (body?: unknown) => Promise<unknown>;
    deleteRegion?: (body?: unknown) => Promise<unknown>;
    openRegionFile?: (body?: unknown) => Promise<unknown>;
    getRegionMeta?: (body?: unknown) => Promise<unknown>;
    onDiagnostics?: (handler: (data: unknown) => void) => () => void;
    onProgress?: (handler: (data: unknown) => void) => () => void;
    onSemanticTokens?: (handler: (data: unknown) => void) => () => void;
    onStatus?: (handler: (data: unknown) => void) => () => void;
    onNotification?: (handler: (data: unknown) => void) => () => void;
    onClientNotification?: (handler: (data: unknown) => void) => () => void;
  };
};

declare global {
  interface Window {
    aaronnoteApi?: NativeApi;
  }
}

function nativeApi(): NativeApi | undefined {
  return globalThis.window?.aaronnoteApi;
}

function requireNative(): NativeApi {
  const native = nativeApi();
  if (!native) throw new Error("Native IPC bridge is unavailable");
  return native;
}

function requireMethod<T extends (...args: any[]) => unknown>(method: T | undefined, feature: string): T {
  if (!method) throw new Error(`${feature} is unavailable`);
  return method;
}

function ensureOk<T>(msg: T, fallback: string, allowConflict = false): T {
  const value = msg as T & { ok?: boolean; conflict?: boolean; message?: string };
  if (value?.ok === false && !(allowConflict && value.conflict)) {
    throw new Error(value.message ?? fallback);
  }
  return msg;
}

export const api = {
  notes: {
    async bootstrap(file?: string): Promise<OpenMsg> {
      const native = requireMethod(requireNative().notes?.bootstrap, "Bootstrap");
      return ensureOk(await native(file) as OpenMsg, "Bootstrap failed");
    },

    async open(file: string): Promise<OpenMsg> {
      const native = requireMethod(requireNative().notes?.open, "Open");
      return ensureOk(await native(file) as OpenMsg, "Open failed");
    },

    async list(force = false): Promise<NotesMsg> {
      const native = requireMethod(requireNative().notes?.list, "Notes load");
      return ensureOk(await native(force) as NotesMsg, "Notes load failed");
    },

    async save(body: SaveBody): Promise<SavedMsg> {
      const native = requireMethod(requireNative().notes?.save, "Save");
      return ensureOk(await native(body) as SavedMsg, "Save failed", true);
    },

    saveKeepalive(body: SaveBody): void {
      const native = requireMethod(requireNative().notes?.save, "Save");
      // Best-effort save fired during page unload; the renderer is tearing down,
      // so there is nowhere useful to surface a rejection.
      void native(body).catch(() => {});
    },

    async createNode(draft: Record<string, unknown>): Promise<OpenMsg & { message?: string }> {
      const native = requireMethod(requireNative().notes?.createNode, "Create node");
      return ensureOk(await native(draft) as OpenMsg & { message?: string }, "Create node failed");
    },

    async deleteNote(file: string): Promise<IndexPayload & { ok?: boolean; message?: string }> {
      const native = requireMethod(requireNative().notes?.deleteNote, "Move to Trash");
      return ensureOk(await native(file) as IndexPayload & { ok?: boolean; message?: string }, "Move to Trash failed");
    },

    async createFolder(path: string): Promise<IndexPayload & { ok?: boolean; path?: string; message?: string }> {
      const native = requireMethod(requireNative().notes?.createFolder, "Create folder");
      return ensureOk(await native(path) as IndexPayload & { ok?: boolean; path?: string; message?: string }, "Create folder failed");
    },

    async pathSuggestions(file: string): Promise<{ paths?: string[] }> {
      const native = requireMethod(requireNative().notes?.pathSuggestions, "Path suggestion load");
      return ensureOk(await native(file) as { paths?: string[] }, "Path suggestion load failed");
    },

    async roamSync(reload = false): Promise<IndexPayload & { message?: string; db?: string }> {
      const native = requireMethod(requireNative().notes?.roamSync, "Sync");
      return ensureOk(await native(reload) as IndexPayload & { message?: string; db?: string }, "Sync failed");
    },

    async roamSyncFull(): Promise<IndexPayload & { message?: string; db?: string }> {
      const native = requireMethod(requireNative().notes?.roamSyncFull, "Full Sync");
      return ensureOk(await native() as IndexPayload & { message?: string; db?: string }, "Full sync failed");
    },

    async templates(force = false): Promise<TemplatesMsg> {
      const native = requireMethod(requireNative().notes?.templates, "Template load");
      return ensureOk(await native(force) as TemplatesMsg, "Template load failed");
    },

    async snippets(): Promise<{ snippets?: SnippetSummary[]; message?: string }> {
      const native = requireMethod(requireNative().notes?.snippets, "Snippet reload");
      return ensureOk(await native() as { snippets?: SnippetSummary[]; message?: string }, "Snippet reload failed");
    },

    async todos(file?: string): Promise<{ todos?: unknown[]; message?: string }> {
      const native = requireMethod(requireNative().notes?.todos, "Todo scan");
      return ensureOk(await native(file) as { todos?: unknown[]; message?: string }, "Todo scan failed");
    },

    async exportPdf(_body: { file: string; content: string }): Promise<Response> {
      throw new Error("PDF export requires the desktop bridge");
    },

    async metaAdd(body: { file: string; content: string; title: string; tags: string[] }): Promise<OpenMsg & { message?: string }> {
      const native = requireMethod(requireNative().notes?.metaAdd, "Generate Roam ID");
      return ensureOk(await native(body) as OpenMsg & { message?: string }, "Generate Roam ID failed");
    },
  },

  roamTools: {
    async renameTag(body: Record<string, unknown>): Promise<IndexPayload & { ok?: boolean; changedCount?: number; changed?: unknown[]; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.renameTag, "Roam tag rename");
      return ensureOk(await native(body) as IndexPayload & { ok?: boolean; changedCount?: number; changed?: unknown[]; message?: string }, "Roam tag rename failed");
    },

    async deleteTag(body: Record<string, unknown>): Promise<IndexPayload & { ok?: boolean; changedCount?: number; changed?: unknown[]; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.deleteTag, "Roam tag delete");
      return ensureOk(await native(body) as IndexPayload & { ok?: boolean; changedCount?: number; changed?: unknown[]; message?: string }, "Roam tag delete failed");
    },

    async tagOverlap(): Promise<{ ok?: boolean; duplicateCase?: unknown[]; overlaps?: unknown[]; tagCount?: number; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.tagOverlap, "Roam tag overlap report");
      return ensureOk(await native() as { ok?: boolean; duplicateCase?: unknown[]; overlaps?: unknown[]; tagCount?: number; message?: string }, "Roam tag overlap report failed");
    },

    async rewritePathRefs(body: Record<string, unknown>): Promise<IndexPayload & { ok?: boolean; changedCount?: number; referenceCount?: number; changed?: unknown[]; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.rewritePathRefs, "Roam path reference rewrite");
      return ensureOk(await native(body) as IndexPayload & { ok?: boolean; changedCount?: number; referenceCount?: number; changed?: unknown[]; message?: string }, "Roam path reference rewrite failed");
    },

    async fileHistory(file: string): Promise<{ entries?: GitCommitEntry[]; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.fileHistory, "File history");
      return ensureOk(await native(file) as { entries?: GitCommitEntry[]; message?: string }, "File history failed");
    },

    async restoreFileVersion(body: { file: string; sha: string }): Promise<IndexPayload & { restoredFile?: string; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.restoreFileVersion, "File version restore");
      return ensureOk(await native(body) as IndexPayload & { restoredFile?: string; message?: string }, "File version restore failed");
    },

    async discardFileChanges(file: string): Promise<IndexPayload & { restoredFile?: string; discarded?: boolean; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.discardFileChanges, "File restore");
      return ensureOk(await native(file) as IndexPayload & { restoredFile?: string; discarded?: boolean; message?: string }, "File restore failed");
    },

    async repoStatus(): Promise<GitRepoStatus> {
      const native = requireMethod(requireNative().roamTools?.repoStatus, "Repo status");
      return ensureOk(await native() as GitRepoStatus, "Repo status failed");
    },

    async repoHistory(limit = 30): Promise<{ entries?: GitCommitEntry[]; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.repoHistory, "Repo history");
      return ensureOk(await native(limit) as { entries?: GitCommitEntry[]; message?: string }, "Repo history failed");
    },

    async changes(): Promise<{ changes?: GitChange[]; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.changes, "Repo changes");
      return ensureOk(await native() as { changes?: GitChange[]; message?: string }, "Repo changes failed");
    },

    async diff(body: { file?: string; path?: string; scope?: string; sha?: string }): Promise<{ file?: string; path?: string; diff?: string; scope?: string; sha?: string; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.diff, "Repo diff");
      return ensureOk(await native(body) as { file?: string; path?: string; diff?: string; scope?: string; sha?: string; message?: string }, "Repo diff failed");
    },

    async commitDiff(sha: string): Promise<{ sha?: string; diff?: string; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.commitDiff, "Commit diff");
      return ensureOk(await native(sha) as { sha?: string; diff?: string; message?: string }, "Commit diff failed");
    },

    async pull(): Promise<{ ok?: boolean; output?: string; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.pull, "Roam pull");
      return ensureOk(await native() as { ok?: boolean; output?: string; message?: string }, "Roam pull failed");
    },

    async push(): Promise<{ ok?: boolean; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.push, "Roam push");
      return ensureOk(await native() as { ok?: boolean; message?: string }, "Roam push failed");
    },

    async commit(message: string): Promise<{ ok?: boolean; sha?: string; message?: string }> {
      const native = requireMethod(requireNative().roamTools?.commit, "Roam commit");
      return ensureOk(await native(message) as { ok?: boolean; sha?: string; message?: string }, "Roam commit failed");
    },
  },

  assets: {
    async upload(body: { file: string; name: string; type: string; data: string }): Promise<UploadedAsset> {
      const native = requireMethod(requireNative().assets?.upload, "Asset upload");
      return ensureOk(await native(body) as UploadedAsset, "Asset upload failed");
    },

    async storeFromPath(body: { file: string; path: string; name?: string; type?: string }): Promise<UploadedAsset> {
      const native = requireMethod(requireNative().assets?.storeFromPath, "Native asset import");
      return ensureOk(await native(body) as UploadedAsset, "Asset upload failed");
    },

    async renderTikz(body: { file: string; id: string; timestamp: string; source: string }): Promise<UploadedAsset & { rendered?: boolean; mtimeMs?: number }> {
      const native = requireMethod(requireNative().assets?.renderTikz, "TikZ render");
      return ensureOk(await native(body) as UploadedAsset & { rendered?: boolean; mtimeMs?: number }, "TikZ render failed");
    },

    async scanOrphans(): Promise<{ assets?: UnusedAsset[]; message?: string }> {
      const native = requireMethod(requireNative().assets?.scanOrphans, "Asset scan");
      return ensureOk(await native() as { assets?: UnusedAsset[]; message?: string }, "Asset scan failed");
    },

    async trashOrphans(files: string[]): Promise<{ assets?: UnusedAsset[]; trashed?: unknown[]; message?: string }> {
      const native = requireMethod(requireNative().assets?.trashOrphans, "Move to Trash");
      return ensureOk(await native(files) as { assets?: UnusedAsset[]; trashed?: unknown[]; message?: string }, "Move to Trash failed");
    },
  },

  session: {
    async getRecent(): Promise<{ recent?: RecentNote[] }> {
      const native = requireMethod(requireNative().session?.getRecent, "Recent notes load");
      return ensureOk(await native() as { recent?: RecentNote[] }, "Recent notes load failed");
    },

    async touchRecent(file: string, openedAt: number): Promise<void> {
      const native = requireMethod(requireNative().session?.touchRecent, "Recent note save");
      ensureOk(await native(file, openedAt), "Recent note save failed");
    },

    async getPositions(): Promise<{ positions?: CursorPosition[] }> {
      const native = requireMethod(requireNative().session?.getPositions, "Cursor positions load");
      return ensureOk(await native() as { positions?: CursorPosition[] }, "Cursor positions load failed");
    },

    savePosition(position: CursorPosition, _keepalive = false): void {
      const native = requireMethod(requireNative().session?.savePosition, "Cursor position save");
      void native(position).catch((err) => console.warn("[session] cursor position save failed", err));
    },
  },

  plugins: {
    async list(): Promise<{ plugins?: PluginSummary[]; message?: string }> {
      const native = requireMethod(requireNative().plugins?.list, "Plugin scan");
      return ensureOk(await native() as { plugins?: PluginSummary[]; message?: string }, "Plugin scan failed");
    },

    async getOverrides(): Promise<{ overrides?: Record<string, unknown> }> {
      const native = requireMethod(requireNative().plugins?.getOverrides, "Plugin override load");
      return ensureOk(await native() as { overrides?: Record<string, unknown> }, "Plugin override load failed");
    },

    async saveOverrides(overrides: Record<string, unknown>): Promise<void> {
      const native = requireMethod(requireNative().plugins?.saveOverrides, "Plugin override save");
      ensureOk(await native(overrides), "Plugin override save failed");
    },
  },

  fs: {
    async rename(body: Record<string, unknown>): Promise<Record<string, unknown> & { message?: string }> {
      const native = requireMethod(requireNative().fs?.rename, "Rename");
      return ensureOk(await native(body) as Record<string, unknown> & { message?: string }, "Rename failed");
    },

    async move(body: Record<string, unknown>): Promise<Record<string, unknown> & { message?: string }> {
      const native = requireMethod(requireNative().fs?.move, "Move");
      return ensureOk(await native(body) as Record<string, unknown> & { message?: string }, "Move failed");
    },

    async duplicate(body: Record<string, unknown>): Promise<Record<string, unknown> & { message?: string }> {
      const native = requireMethod(requireNative().fs?.duplicate, "Duplicate");
      return ensureOk(await native(body) as Record<string, unknown> & { message?: string }, "Duplicate failed");
    },

    async trash(body: Record<string, unknown>): Promise<Record<string, unknown> & { message?: string }> {
      const native = requireMethod(requireNative().fs?.trash, "Move to Trash");
      return ensureOk(await native(body) as Record<string, unknown> & { message?: string }, "Move to Trash failed");
    },
  },

  meta: {
    async add(body: Record<string, unknown>): Promise<Record<string, unknown> & { message?: string }> {
      const native = requireMethod(requireNative().meta?.add, "Metadata registration");
      return ensureOk(await native(body) as Record<string, unknown> & { message?: string }, "Metadata registration failed");
    },

    async remove(body: Record<string, unknown>): Promise<Record<string, unknown> & { message?: string }> {
      const native = requireMethod(requireNative().meta?.remove, "Metadata removal");
      return ensureOk(await native(body) as Record<string, unknown> & { message?: string }, "Metadata removal failed");
    },

    async tag(body: Record<string, unknown>): Promise<Record<string, unknown> & { message?: string }> {
      const native = requireMethod(requireNative().meta?.tag, "Tag update");
      return ensureOk(await native(body) as Record<string, unknown> & { message?: string }, "Tag update failed");
    },

    async hideRoam(body: Record<string, unknown>): Promise<Record<string, unknown> & { message?: string }> {
      const native = requireMethod(requireNative().meta?.hideRoam, "Set roam off");
      return ensureOk(await native(body) as Record<string, unknown> & { message?: string }, "Set roam off failed");
    },

    async activateRoam(body: Record<string, unknown>): Promise<Record<string, unknown> & { message?: string }> {
      const native = requireMethod(requireNative().meta?.activateRoam, "Clear roam off");
      return ensureOk(await native(body) as Record<string, unknown> & { message?: string }, "Clear roam off failed");
    },
  },

  shell: {
    available(): boolean {
      return Boolean(nativeApi()?.shell?.showInFolder || nativeApi()?.shell?.openPath || nativeApi()?.shell?.openDirectory);
    },

    async showInFolder(file: string): Promise<void> {
      const native = requireMethod(requireNative().shell?.showInFolder, "Native shell integration");
      ensureOk(await native(file), "Reveal failed");
    },

    async openPath(file: string): Promise<void> {
      const native = requireMethod(requireNative().shell?.openPath, "Native shell integration");
      ensureOk(await native(file), "Open failed");
    },

    async openDirectory(path: string, base = ""): Promise<void> {
      const native = requireMethod(requireNative().shell?.openDirectory, "Native shell integration");
      ensureOk(await native(path, base), "Open directory failed");
    },

    async openDirectoryInKitty(path: string, base = ""): Promise<void> {
      const native = requireMethod(requireNative().shell?.openDirectoryInKitty, "Native shell integration");
      ensureOk(await native(path, base), "Open Kitty failed");
    },

    async showAttachmentMenu(file: string, base = "", options: unknown = {}): Promise<void> {
      const native = requireMethod(requireNative().shell?.showAttachmentMenu, "Native shell integration");
      ensureOk(await native(file, base, options), "Attachment menu failed");
    },

    async showEditorContextMenu(options: unknown = {}): Promise<void> {
      const native = requireMethod(requireNative().shell?.showEditorContextMenu, "Native shell integration");
      ensureOk(await native(options), "Context menu failed");
    },

    async showLeanEditorMenu(options: unknown = {}): Promise<void> {
      const native = requireMethod(requireNative().shell?.showLeanEditorMenu, "Native shell integration");
      ensureOk(await native(options), "Lean menu failed");
    },

    async openLeanLocation(target: { file: string; line: number; character: number }): Promise<{ ok: boolean; message?: string }> {
      const native = nativeApi()?.shell?.openLeanLocation;
      if (!native) return { ok: false, message: "External Lean navigation unavailable" };
      const res = await native(target) as { ok?: boolean; message?: string };
      return { ok: Boolean(res?.ok), message: res?.message };
    },
  },

  externalEditor: {
    available(): boolean {
      return Boolean(nativeApi()?.externalEditor?.open);
    },

    async open(target: unknown): Promise<{ ok: boolean; editor?: string; file?: string; cwd?: string; message?: string }> {
      const native = nativeApi()?.externalEditor?.open;
      if (!native) return { ok: false, message: "External editor integration unavailable" };
      const res = await native(target) as { ok?: boolean; editor?: string; file?: string; cwd?: string; message?: string };
      return {
        ok: Boolean(res?.ok),
        editor: res?.editor,
        file: res?.file,
        cwd: res?.cwd,
        message: res?.message,
      };
    },
  },

  jupyter: {
    available(): boolean {
      return Boolean(nativeApi()?.jupyter?.request);
    },

    async request(action: string, body: Record<string, unknown> = {}): Promise<Record<string, unknown> & { ok?: boolean; message?: string }> {
      const native = requireMethod(requireNative().jupyter?.request, "Jupyter integration");
      return ensureOk(await native(action, body) as Record<string, unknown> & { ok?: boolean; message?: string }, "Jupyter failed");
    },

    async scroll(body: Record<string, unknown> = {}): Promise<Record<string, unknown> & { ok?: boolean; message?: string }> {
      const native = requireMethod(requireNative().jupyter?.scroll, "Jupyter frame navigation");
      return ensureOk(await native(body) as Record<string, unknown> & { ok?: boolean; message?: string }, "Jupyter scroll failed");
    },

    async kernelStatus(body: Record<string, unknown> = {}): Promise<Record<string, unknown> & { ok?: boolean; connected?: boolean; dead?: boolean; status?: string; connectionStatus?: string }> {
      const native = nativeApi()?.jupyter?.kernelStatus;
      if (!native) return { ok: false };
      return await native(body) as Record<string, unknown> & { ok?: boolean };
    },

    onStatus(handler: (data: { running?: boolean; crashed?: boolean; output?: string }) => void): () => void {
      const native = nativeApi()?.jupyter?.onStatus;
      if (!native) return () => {};
      return native(handler as (data: unknown) => void);
    },
  },

  proseCheck: {
    async run(body: { file?: string; content?: string; ranges?: Array<{ from: number; to: number }>; segments?: Array<{ from: number; to: number; text: string }>; totalChars?: number }): Promise<{ diagnostics?: ProseDiagnostic[]; tools?: Array<{ source?: string; ok?: boolean; message?: string; optional?: boolean }>; message?: string }> {
      const native = requireMethod(requireNative().proseCheck?.run, "Prose check");
      return ensureOk(await native(body) as { diagnostics?: ProseDiagnostic[]; tools?: Array<{ source?: string; ok?: boolean; message?: string; optional?: boolean }>; message?: string }, "Prose check failed");
    },

    browserSpellcheck(words: string[]): Array<{ word?: string; misspelled?: boolean; suggestions?: string[] }> {
      const native = nativeApi()?.proseCheck?.browserSpellcheck;
      if (!native) return [];
      return native(words);
    },
  },

  lean: {
    available(): boolean {
      return Boolean(nativeApi()?.lean);
    },

    async request(action: string, body?: unknown): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.request) return { ok: false };
      return lean.request(action, body);
    },

    async status(): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.status) return { kind: "Inactive", message: "Not available", running: false };
      return lean.status();
    },

    async openNote(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.openNote) return { ok: false };
      return lean.openNote(body);
    },

    async changeNote(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.changeNote) return { ok: false };
      return lean.changeNote(body);
    },

    async closeNote(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.closeNote) return { ok: false };
      return lean.closeNote(body);
    },

    async saveNote(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.saveNote) return { ok: false };
      return lean.saveNote(body);
    },

    async cacheStatus(): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.cacheStatus) return api.lean.request("cache-status");
      return lean.cacheStatus();
    },

    async cacheGet(body: Record<string, unknown> = {}): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.cacheGet) return api.lean.request("cache-get", body);
      return lean.cacheGet(body);
    },

    async ensureRegion(body: Record<string, unknown>): Promise<unknown> {
      return api.lean.request("ensure-region", body);
    },

    async readRegion(body: Record<string, unknown>): Promise<LeanRegionRead> {
      return api.lean.request("read-region", body) as Promise<LeanRegionRead>;
    },

    async updateRegion(body: Record<string, unknown>): Promise<LeanUpdateRegionResponse> {
      return api.lean.request("update-region", body) as Promise<LeanUpdateRegionResponse>;
    },

    async deleteRegion(body: Record<string, unknown>): Promise<unknown> {
      return api.lean.request("delete-region", body);
    },

    async openRegionFile(body: Record<string, unknown>): Promise<LeanOpenRegionResponse> {
      return api.lean.request("open-region-file", body) as Promise<LeanOpenRegionResponse>;
    },

    async getRegionMeta(body: Record<string, unknown>): Promise<unknown> {
      return api.lean.request("get-region-meta", body);
    },

    async getGoals(body: Record<string, unknown>): Promise<LeanGoalsResponse> {
      const lean = nativeApi()?.lean;
      if (!lean?.getGoals) return { ok: false };
      return lean.getGoals(body) as Promise<LeanGoalsResponse>;
    },

    async getTermGoal(body: Record<string, unknown>): Promise<LeanTermGoalResponse> {
      const lean = nativeApi()?.lean;
      if (!lean?.getTermGoal) return { ok: false };
      return lean.getTermGoal(body) as Promise<LeanTermGoalResponse>;
    },

    async getHover(body: Record<string, unknown>): Promise<LeanHoverResponse> {
      const lean = nativeApi()?.lean;
      if (!lean?.getHover) return { ok: false };
      return lean.getHover(body) as Promise<LeanHoverResponse>;
    },

    async getCompletions(body: Record<string, unknown>): Promise<LeanCompletionResponse> {
      const lean = nativeApi()?.lean;
      if (!lean?.getCompletions) return { ok: false };
      return lean.getCompletions(body) as Promise<LeanCompletionResponse>;
    },

    async rpcCall(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.rpcCall) return api.lean.request("rpc-call", body);
      return lean.rpcCall(body);
    },

    async getDefinition(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.getDefinition) return { ok: false };
      return lean.getDefinition(body);
    },

    async getDiagnostics(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.getDiagnostics) return api.lean.request("get-diagnostics", body);
      return lean.getDiagnostics(body);
    },

    async lspRequest(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.lspRequest) return api.lean.request("lsp-request", body);
      return lean.lspRequest(body);
    },

    async lspNotify(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.lspNotify) return api.lean.request("lsp-notify", body);
      return lean.lspNotify(body);
    },

    async createRpcSession(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.createRpcSession) return api.lean.request("create-rpc-session", body);
      return lean.createRpcSession(body);
    },

    async closeRpcSession(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.closeRpcSession) return api.lean.request("close-rpc-session", body);
      return lean.closeRpcSession(body);
    },

    async rpcRelease(body: Record<string, unknown>): Promise<unknown> {
      const lean = nativeApi()?.lean;
      if (!lean?.rpcRelease) return api.lean.request("rpc-release", body);
      return lean.rpcRelease(body);
    },

    async getLog(): Promise<Array<{ type: string; ts: number; message?: string }>> {
      const lean = nativeApi()?.lean;
      if (!lean?.request) return [];
      const res = await lean.request("log");
      const r = res as { entries?: unknown[] } | null;
      return (r?.entries ?? []) as Array<{ type: string; ts: number; message?: string }>;
    },

    onDiagnostics(handler: (data: LeanDiagnosticsPush) => void): () => void {
      return nativeApi()?.lean?.onDiagnostics?.(handler as (data: unknown) => void) ?? (() => {});
    },

    onProgress(handler: (data: LeanProgressPush) => void): () => void {
      return nativeApi()?.lean?.onProgress?.(handler as (data: unknown) => void) ?? (() => {});
    },

    onSemanticTokens(handler: (data: LeanSemanticTokensPush) => void): () => void {
      return nativeApi()?.lean?.onSemanticTokens?.(handler as (data: unknown) => void) ?? (() => {});
    },

    onStatus(handler: (data: unknown) => void): () => void {
      return nativeApi()?.lean?.onStatus?.(handler) ?? (() => {});
    },

    onNotification(handler: (data: unknown) => void): () => void {
      return nativeApi()?.lean?.onNotification?.(handler) ?? (() => {});
    },

    onClientNotification(handler: (data: unknown) => void): () => void {
      return nativeApi()?.lean?.onClientNotification?.(handler) ?? (() => {});
    },
  },
};
