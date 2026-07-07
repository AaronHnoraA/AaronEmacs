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
export type JupyterCellOutput = {
  output_type?: string;
  name?: string;
  text?: string;
  execution_count?: number | null;
  data?: Record<string, unknown>;
  metadata?: Record<string, unknown>;
  transient?: Record<string, unknown>;
  ename?: string;
  evalue?: string;
  traceback?: string[];
};
export type JupyterCellExecuteResult = {
  ok?: boolean;
  cellId?: string;
  kernel?: string;
  session?: string;
  status?: string;
  executionCount?: number | null;
  outputs?: JupyterCellOutput[];
  message?: string;
  stoppedAt?: string;
  autoRan?: boolean;
  results?: JupyterCellExecuteResult[];
  plan?: Array<{ cellId?: string; mode?: string; selected?: boolean }>;
  widgetMessages?: Array<Record<string, unknown>>;
  widgetMessagesTruncated?: boolean;
  live?: boolean;
  savedAt?: string;
  kernelRuntime?: {
    id?: string;
    name?: string;
    generation?: number;
  };
  widgetRuntime?: {
    id: string;
    name: string;
    generation?: number;
  };
};
export type JupyterKernelSpec = { name: string; displayName?: string; language?: string };
export type JupyterKernelListResult = {
  ok?: boolean;
  default?: string;
  kernels?: JupyterKernelSpec[];
  attachable?: JupyterKernelSpec[];
};
export type JupyterVariable = {
  name?: string;
  type?: string;
  summary?: string;
  shape?: unknown;
};
export type JupyterVariablesResult = {
  ok?: boolean;
  supported?: boolean;
  kernel?: string;
  session?: string;
  variables?: JupyterVariable[];
};
export type JupyterKernelTask = {
  key?: string;
  id?: string;
  file?: string;
  sourceFile?: string;
  kernel?: string;
  session?: string;
  status?: string;
  running?: number;
  createdAt?: number;
  createdAtIso?: string;
  lastUsedAt?: number;
  lastUsedAtIso?: string;
  lastActivityAt?: number;
  lastActivityAtIso?: string;
  idleMs?: number;
  runningMs?: number;
  totalRuns?: number;
  executionCount?: number | null;
  lastCellId?: string;
  lastError?: string;
  executedCells?: number;
  protected?: boolean;
  ttlMs?: number;
};
export type JupyterTasksResult = {
  ok?: boolean;
  server?: {
    status?: string;
    owned?: boolean;
    pid?: number | null;
    activeRequests?: number;
    startedAt?: number;
    startedAtIso?: string;
    lastUsedAt?: number;
    lastUsedAtIso?: string;
    idleMs?: number;
    idleTtlMs?: number;
  };
  cleanup?: {
    kernelIdleTtlMs?: number;
    serverIdleTtlMs?: number;
    cleanupIntervalMs?: number;
    execTimeoutMs?: number;
  };
  kernels?: JupyterKernelTask[];
  removed?: Array<{ key?: string; kernel?: string; session?: string; reason?: string }>;
  scheduled?: boolean;
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
  command?: "todo" | "itodo" | string;
  source?: string;
  status?: string;
  ddl?: string;
  deadline?: string;
  due?: string;
  line?: number;
  index?: number;
  tags?: string[];
  inlineTags?: string[];
  /** Canonical arg keys (ddl/sche/prio/repeat/warn/after/done/log) after alias normalization. */
  canon?: Record<string, string>;
  /** Ids of other todos this one depends on (resolved by text reference, no ids in source). */
  deps?: string[];
  /** Status as computed by dependency resolution: "blocked" when an unresolved dep is open. */
  effectiveStatus?: string;
  /** Ids of open dependencies causing a computed-blocked effectiveStatus. */
  blockedBy?: string[];
  /** Sort key from the urgency formula (priority + deadline proximity + doing/blocked adjustments). */
  urgency?: number;
};
export type TodosMsg = {
  type?: string;
  todos?: TodoItem[];
  root?: string;
};
export type TodoLint = {
  todoId?: string;
  file?: string;
  line?: number;
  kind?:
    | "broken-ref"
    | "ambiguous-ref"
    | "ambiguous-note"
    | "missing-gantt-date"
    | "missing-milestone-date"
    | "cycle"
    | "broken-clock-ref"
    | "ambiguous-clock-ref"
    | "duplicate-id";
  via?: "after" | "blocks";
  ref?: string;
  message?: string;
  candidates?: Array<{ id: string; text: string }>;
};
export type TodoRefCompletion = { label?: string; ref?: string; hasId?: boolean; file?: string; status?: string };
export type PlanningItem = Record<string, unknown> & {
  id?: string;
  kind?: "project" | "milestone" | "clock" | string;
  status?: string;
  title?: string;
  text?: string;
  args?: Record<string, string>;
  canon?: Record<string, string>;
  file?: string;
  path?: string;
  noteTitle?: string;
  line?: number;
  index?: number;
  source?: string;
};
export type GanttTask = {
  id?: string;
  name?: string;
  project?: string;
  status?: string;
  start?: string;
  end?: string;
  dependencies?: string[];
  progress?: number;
  source?: { file?: string; index?: number; line?: number; source?: string; text?: string };
};
export type GanttMilestone = {
  id?: string;
  name?: string;
  project?: string;
  date?: string;
  source?: { file?: string; index?: number; line?: number; source?: string; text?: string };
};
export type GanttLane = { id?: string; key?: string; name?: string; start?: string; end?: string; childTaskIds?: string[] };
export type GanttMsg = {
  tasks?: GanttTask[];
  backlog?: GanttTask[];
  milestones?: GanttMilestone[];
  lanes?: GanttLane[];
  lints?: TodoLint[];
};
export type ProjectRollup = {
  id?: string;
  key?: string;
  title?: string;
  status?: string;
  area?: string;
  phase?: string;
  file?: string;
  open?: number;
  doing?: number;
  done?: number;
  cancelled?: number;
  blocked?: number;
  total?: number;
  progress?: number;
  effortMinutes?: number;
  clockedMinutes?: number;
  childTodoIds?: string[];
};
export type AgendaEntry = {
  kind?: "deadline" | "warning" | "overdue" | "scheduled" | "sched-carry" | "log" | "repeat";
  label?: string;
  todoId?: string;
  date?: string;
  dateKey?: string;
  time?: string | null;
  urgency?: number;
  virtual?: boolean;
};
export type AgendaDay = { date?: string; entries?: AgendaEntry[] };
export type ClockTask = { todoId?: string; text?: string; file?: string; minutes?: number; effortMinutes?: number };
export type ClockModel = {
  tasks?: ClockTask[];
  byDay?: Record<string, number>;
  byProject?: Record<string, number>;
  running?: { todoId?: string; text?: string; file?: string; from?: string; minutesSoFar?: number } | null;
};
export type AgendaMsg = {
  type?: string;
  range?: { from?: string; to?: string; today?: string };
  days?: AgendaDay[];
  todos?: TodoItem[];
  projects?: PlanningItem[];
  milestones?: PlanningItem[];
  clocks?: PlanningItem[];
  clocktable?: ClockModel;
  projectModel?: ProjectRollup[];
  gantt?: GanttMsg;
  lints?: TodoLint[];
  logByDay?: Record<string, number>;
  stats?: { open?: number; doing?: number; done?: number; cancelled?: number; blocked?: number; overdue?: number };
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
    agenda?: (body: Record<string, unknown>) => Promise<unknown>;
    createTodo?: (body: Record<string, unknown>) => Promise<unknown>;
    patchTodo?: (body: Record<string, unknown>) => Promise<unknown>;
    clockIn?: (body: Record<string, unknown>) => Promise<unknown>;
    clockOut?: (body: Record<string, unknown>) => Promise<unknown>;
    todoDepRef?: (body: Record<string, unknown>) => Promise<unknown>;
  };
  completions?: {
    tags?: (prefix: string) => Promise<unknown>;
    roam?: (prefix: string) => Promise<unknown>;
    todoRefs?: (body: Record<string, unknown>) => Promise<unknown>;
  };
  clipboard?: {
    read?: (body?: { file?: string }) => Promise<unknown>;
  };
  noteCode?: {
    readRegion?: (body?: unknown) => Promise<unknown>;
  };
  jupyterCell?: {
    kernels?: () => Promise<unknown>;
    execute?: (body?: unknown) => Promise<unknown>;
    openScript?: (body?: unknown) => Promise<unknown>;
    readScriptCell?: (body?: unknown) => Promise<unknown>;
    executeScriptCell?: (body?: unknown) => Promise<unknown>;
    clearScriptCellOutput?: (body?: unknown) => Promise<unknown>;
    deleteScriptCell?: (body?: unknown) => Promise<unknown>;
    saveScriptCellOutputUi?: (body?: unknown) => Promise<unknown>;
    clearAllOutputs?: (body?: unknown) => Promise<unknown>;
    variables?: (body?: unknown) => Promise<unknown>;
    kernelStatus?: (body?: unknown) => Promise<unknown>;
    restart?: (body?: unknown) => Promise<unknown>;
    interrupt?: (body?: unknown) => Promise<unknown>;
    shutdown?: (body?: unknown) => Promise<unknown>;
    tasks?: () => Promise<unknown>;
    cleanup?: (body?: unknown) => Promise<unknown>;
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
    key?: (body: string | { key: string; client?: string }) => Promise<unknown>;
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
    async agenda(body: Record<string, unknown> = {}): Promise<AgendaMsg> {
      const call = requireMethod(nativeApi().notes?.agenda, "Agenda");
      return ensureOk(await call(body) as AgendaMsg, "Agenda failed");
    },
    async createTodo(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().notes?.createTodo, "Todo create");
      return ensureOk(await call(body) as Record<string, unknown>, "Todo create failed");
    },
    async patchTodo(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().notes?.patchTodo, "Todo patch");
      return ensureOk(await call(body) as Record<string, unknown>, "Todo patch failed");
    },
    async clockIn(body: Record<string, unknown>): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().notes?.clockIn, "Clock in");
      return ensureOk(await call(body) as Record<string, unknown>, "Clock in failed");
    },
    async clockOut(body: Record<string, unknown> = {}): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().notes?.clockOut, "Clock out");
      return ensureOk(await call(body) as Record<string, unknown>, "Clock out failed");
    },
    async todoDepRef(body: Record<string, unknown>): Promise<{ type?: string; ref?: string }> {
      const call = requireMethod(nativeApi().notes?.todoDepRef, "Todo dependency reference");
      return ensureOk(await call(body) as { type?: string; ref?: string }, "Todo dependency reference failed");
    },
  },
  noteCode: {
    async readRegion(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().noteCode?.readRegion, "Note code");
      return ensureOk(await call(body) as Record<string, unknown>, "Note code failed");
    },
  },
  jupyterCell: {
    async kernels(): Promise<JupyterKernelListResult> {
      const call = requireMethod(nativeApi().jupyterCell?.kernels, "Jupyter kernels");
      return ensureOk(await call() as JupyterKernelListResult, "Jupyter kernels failed");
    },
    async execute(body: unknown): Promise<JupyterCellExecuteResult> {
      const call = requireMethod(nativeApi().jupyterCell?.execute, "Jupyter cell");
      return ensureOk(await call(body) as JupyterCellExecuteResult, "Jupyter cell failed");
    },
    async openScript(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.openScript, "Jupyter cell script");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter cell script failed");
    },
    async readScriptCell(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.readScriptCell, "Jupyter cell script");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter cell script failed");
    },
    async executeScriptCell(body: unknown): Promise<JupyterCellExecuteResult> {
      const call = requireMethod(nativeApi().jupyterCell?.executeScriptCell, "Jupyter cell");
      return ensureOk(await call(body) as JupyterCellExecuteResult, "Jupyter cell failed");
    },
    async clearScriptCellOutput(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.clearScriptCellOutput, "Jupyter cell output");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter cell output failed");
    },
    async deleteScriptCell(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.deleteScriptCell, "Jupyter cell delete");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter cell delete failed");
    },
    async saveScriptCellOutputUi(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.saveScriptCellOutputUi, "Jupyter cell output UI");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter cell output UI save failed");
    },
    async clearAllOutputs(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.clearAllOutputs, "Jupyter outputs");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter outputs failed");
    },
    async variables(body: unknown): Promise<JupyterVariablesResult> {
      const call = requireMethod(nativeApi().jupyterCell?.variables, "Jupyter variables");
      return ensureOk(await call(body) as JupyterVariablesResult, "Jupyter variables failed");
    },
    async kernelStatus(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.kernelStatus, "Jupyter kernel status");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter kernel status failed");
    },
    async restart(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.restart, "Jupyter kernel restart");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter kernel restart failed");
    },
    async interrupt(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.interrupt, "Jupyter kernel interrupt");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter kernel interrupt failed");
    },
    async shutdown(body: unknown): Promise<Record<string, unknown>> {
      const call = requireMethod(nativeApi().jupyterCell?.shutdown, "Jupyter kernel shutdown");
      return ensureOk(await call(body) as Record<string, unknown>, "Jupyter kernel shutdown failed");
    },
    async tasks(): Promise<JupyterTasksResult> {
      const call = requireMethod(nativeApi().jupyterCell?.tasks, "Jupyter tasks");
      return ensureOk(await call() as JupyterTasksResult, "Jupyter tasks failed");
    },
    async cleanup(body: unknown = {}): Promise<JupyterTasksResult> {
      const call = requireMethod(nativeApi().jupyterCell?.cleanup, "Jupyter cleanup");
      return ensureOk(await call(body) as JupyterTasksResult, "Jupyter cleanup failed");
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
    async key(keyString: string, client = ""): Promise<void> {
      const call = window.aaronnoteApi?.emacs?.key;
      if (!call) return;
      const body = client ? { key: keyString, client } : keyString;
      await call(body).catch(() => {});
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
    async todoRefs(body: { prefix?: string; file?: string; excludeId?: string; limit?: number } = {}): Promise<{ items?: TodoRefCompletion[] }> {
      const call = window.aaronnoteApi?.completions?.todoRefs;
      if (!call) return { items: [] };
      return await call(body) as { items?: TodoRefCompletion[] };
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
