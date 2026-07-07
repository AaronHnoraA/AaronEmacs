// Full-screen, vault-wide agenda view (org-agenda-class): week/list/month/
// log/gantt/projects/clocktable/lints views over the server-computed agenda
// view-model (`api.notes.agenda`). This is the first-class surface for
// priority/scheduled/deadline/repeat/dependency/project/clock work across
// the whole vault — served as its own page (see `agenda.html`/
// `agenda-main.ts`) as well as embeddable via `openAgendaView`. All edits
// round-trip through `api.notes.patchTodo`/`clockIn`/`clockOut`, which write
// straight back into markdown — this view holds no state that isn't
// re-derivable from it. See `docs/agenda.md` for the view-model shapes.
import type { AgendaMsg, GanttTask, TodoItem, TodoLint } from "./api-client.ts";

export type AgendaViewDeps = {
  api: {
    notes: {
      agenda: (body: Record<string, unknown>) => Promise<AgendaMsg>;
      createTodo: (body: Record<string, unknown>) => Promise<Record<string, unknown>>;
      patchTodo: (body: Record<string, unknown>) => Promise<Record<string, unknown>>;
      todoDepRef: (body: Record<string, unknown>) => Promise<{ ref?: string }>;
      clockIn: (body: Record<string, unknown>) => Promise<Record<string, unknown>>;
      clockOut: (body: Record<string, unknown>) => Promise<Record<string, unknown>>;
    };
  };
  jumpToTodo: (todo: TodoItem) => void | Promise<void>;
  setStatus: (message: string) => void;
  /** True when mounted as the standalone `/agenda` page: hides the "Close"
   * button (there is nothing to return to) and syncs `view`/`q` to the URL. */
  pageMode?: boolean;
};

type ViewKind = "week" | "list" | "month" | "log" | "gantt" | "projects" | "clocktable" | "lints";

// Legacy/external view names (e.g. `main.ts`'s Agenda+ link, or a bookmark)
// map onto the real ones above.
const VIEW_ALIASES: Record<string, ViewKind> = { agenda: "week", calendar: "month" };

function normalizeView(raw: string | null | undefined): ViewKind {
  const v = String(raw || "").trim().toLowerCase();
  if (VIEW_ALIASES[v]) return VIEW_ALIASES[v];
  const known: ViewKind[] = ["week", "list", "month", "log", "gantt", "projects", "clocktable", "lints"];
  return (known as string[]).includes(v) ? (v as ViewKind) : "week";
}

const DAY_MS = 86_400_000;
const STATUS_CYCLE = ["todo", "doing", "done"];

let deps: AgendaViewDeps | null = null;
let overlay: HTMLElement | null = null;
let listEl: HTMLElement | null = null;
let headerEl: HTMLElement | null = null;
let data: AgendaMsg | null = null;
let view: ViewKind = "week";
let anchorMs = midnight(Date.now());
let query = "";
let cursorId = "";
let selection = new Set<string>();
let loading = false;
let ganttDragging: { id: string; x: number } | null = null;

function midnight(ms: number): number {
  const d = new Date(ms);
  d.setHours(0, 0, 0, 0);
  return d.getTime();
}

function fmtDate(ms: number): string {
  const d = new Date(ms);
  return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
}

function parseYmd(s: string): number {
  const m = String(s || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return midnight(Date.now());
  return new Date(Number(m[1]), Number(m[2]) - 1, Number(m[3])).getTime();
}

function startOfMonth(ms: number): number {
  const d = new Date(ms);
  return new Date(d.getFullYear(), d.getMonth(), 1).getTime();
}

function daysInMonth(ms: number): number {
  const d = new Date(ms);
  return new Date(d.getFullYear(), d.getMonth() + 1, 0).getDate();
}

function todoField(todo: TodoItem, ...keys: string[]): string {
  for (const key of keys) {
    const value = (todo as Record<string, unknown>)[key];
    if (typeof value === "string" && value.trim()) return value.trim();
    if (typeof value === "number") return String(value);
  }
  return "";
}

function todoNote(todo: TodoItem): string {
  return todoField(todo, "noteTitle", "title", "path") || "Untitled";
}

function todoText(todo: TodoItem): string {
  return todoField(todo, "text") || "(empty todo)";
}

function todoStatus(todo: TodoItem): string {
  return (todo.effectiveStatus as string) || (todo.status as string) || "todo";
}

function todoPrio(todo: TodoItem): string {
  return String((todo.canon as Record<string, string> | undefined)?.prio || "");
}

function todoHaystack(todo: TodoItem): string {
  return [
    todoStatus(todo),
    todoPrio(todo),
    todoNote(todo),
    todoText(todo),
    todoField(todo, "id", "noteId", "path", "file"),
    ...(Array.isArray(todo.tags) ? todo.tags : []),
  ].join(" ").toLowerCase();
}

function matchesQuery(todo: TodoItem): boolean {
  const q = query.trim().toLowerCase();
  if (!q) return true;
  return q.split(/\s+/).every((term) => todoHaystack(todo).includes(term));
}

function todoById(id: string): TodoItem | undefined {
  return data?.todos?.find((t) => t.id === id);
}

function lintsFor(todoId: string): TodoLint[] {
  return (data?.lints || []).filter((lint) => lint.todoId === todoId);
}

const WIDE_VIEWS = new Set<ViewKind>(["gantt", "projects", "clocktable", "lints"]);

function syncPageUrl(): void {
  if (!deps?.pageMode || typeof history === "undefined") return;
  const params = new URLSearchParams();
  params.set("view", view);
  if (query) params.set("q", query);
  history.replaceState(null, "", `/agenda?${params.toString()}`);
}

async function fetchAgenda(): Promise<void> {
  if (!deps) return;
  loading = true;
  render();
  try {
    const wide = WIDE_VIEWS.has(view);
    const from = wide ? fmtDate(midnight(Date.now())) : view === "month" ? fmtDate(startOfMonth(anchorMs)) : fmtDate(anchorMs);
    const days = wide ? 60 : view === "month" ? daysInMonth(anchorMs) : view === "week" ? 7 : view === "list" ? 30 : 60;
    data = await deps.api.notes.agenda({ from, days, includePlanning: true, includeGantt: true });
  } catch (error) {
    deps.setStatus(error instanceof Error ? error.message : "Agenda failed");
    data = null;
  } finally {
    loading = false;
    render();
  }
}

// --- editing actions (all round-trip through patchTodo / write straight to disk) ---

function todoPatchBase(todo: TodoItem): Record<string, unknown> {
  return {
    file: todoField(todo, "file"),
    id: todoField(todo, "id"),
    index: todo.index,
    source: todoField(todo, "source"),
    text: todoText(todo),
  };
}

async function applyPatch(todo: TodoItem, patch: Record<string, unknown>): Promise<void> {
  if (!deps) return;
  try {
    await deps.api.notes.patchTodo({ ...todoPatchBase(todo), ...patch });
    await fetchAgenda();
  } catch (error) {
    deps.setStatus(error instanceof Error ? error.message : "Todo update failed");
  }
}

function cycleStatus(todo: TodoItem): void {
  const current = (todo.status as string) || "todo";
  const idx = STATUS_CYCLE.indexOf(current);
  const next = STATUS_CYCLE[(idx + 1) % STATUS_CYCLE.length];
  if (next === "done") {
    void applyPatch(todo, { op: "complete" });
  } else {
    void applyPatch(todo, { status: next });
  }
}

function promptEdit(label: string, current: string, apply: (value: string) => void): void {
  const value = window.prompt(label, current);
  if (value === null) return;
  apply(value.trim());
}

function parseQuickTodo(raw: string): Record<string, unknown> {
  const chunks = raw.split("|").map((part) => part.trim()).filter(Boolean);
  const text = chunks.shift() || "";
  const body: Record<string, unknown> = { text };
  for (const chunk of chunks) {
    const match = chunk.match(/^([A-Za-z][\w-]*)\s*[:=]\s*(.*)$/);
    if (!match) continue;
    const key = match[1].toLowerCase();
    const value = match[2].trim();
    if (key && value) body[key] = value;
  }
  return body;
}

async function createTodoFromPrompt(): Promise<void> {
  if (!deps) return;
  const current = cursorId ? todoById(cursorId) : undefined;
  const project = String((current?.canon as Record<string, string> | undefined)?.project || "");
  const raw = window.prompt("New todo: task | project=paper | ddl=today | sche=+1d | prio=A | file=inbox.md", "");
  if (raw === null) return;
  const body = parseQuickTodo(raw);
  if (!String(body.text || "").trim()) return;
  if (!body.file && current?.file) body.file = current.file;
  if (!body.project && project) body.project = project;
  try {
    const result = await deps.api.notes.createTodo(body);
    const todo = result.todo as TodoItem | undefined;
    if (todo?.id) cursorId = String(todo.id);
    await fetchAgenda();
  } catch (error) {
    deps.setStatus(error instanceof Error ? error.message : "Todo create failed");
  }
}

function showHelp(): void {
  window.alert([
    "Agenda keys",
    "",
    "n new todo",
    "j/k move, Enter jump",
    "t status, p priority, d deadline, s scheduled, r repeat",
    "a dependency, c clock in/out",
    "m mark, B bulk status",
    "f/b next/previous range, . today, v next view, g refresh",
    "q/Esc close",
  ].join("\n"));
}

function editPriority(todo: TodoItem): void {
  promptEdit("Priority (A-F, blank to clear):", todoPrio(todo), (value) => {
    void applyPatch(todo, { prio: value });
  });
}

function editDeadline(todo: TodoItem): void {
  const current = (todo.canon as Record<string, string> | undefined)?.ddl || "";
  promptEdit("Deadline (e.g. 2026-07-10, +1w, today; blank to clear):", current, (value) => {
    void applyPatch(todo, { ddl: value });
  });
}

function editScheduled(todo: TodoItem): void {
  const current = (todo.canon as Record<string, string> | undefined)?.sche || "";
  promptEdit("Scheduled (e.g. 2026-07-10, +1w, today; blank to clear):", current, (value) => {
    void applyPatch(todo, { sche: value });
  });
}

function editRepeat(todo: TodoItem): void {
  const current = (todo.canon as Record<string, string> | undefined)?.repeat || "";
  promptEdit("Repeat (+1w / ++1w / .+3d; blank to clear):", current, (value) => {
    void applyPatch(todo, { repeat: value });
  });
}

async function addDependency(todo: TodoItem): Promise<void> {
  if (!deps || !data) return;
  const candidates = (data.todos || []).filter((t) => t.id !== todo.id);
  const label = candidates
    .slice(0, 200)
    .map((t, i) => `${i + 1}. [${todoNote(t)}] ${todoText(t)}`)
    .join("\n");
  const raw = window.prompt(`Depends on which # (from below)?\n\n${label}`, "");
  if (!raw) return;
  const n = Number(raw.trim());
  if (!Number.isInteger(n) || n < 1 || n > candidates.length) {
    deps.setStatus("No matching todo number");
    return;
  }
  const target = candidates[n - 1];
  try {
    const { ref } = await deps.api.notes.todoDepRef({ targetId: target.id, sourceId: todo.id });
    if (!ref) throw new Error("Could not build a dependency reference");
    await applyPatch(todo, { afterAdd: ref });
  } catch (error) {
    deps.setStatus(error instanceof Error ? error.message : "Dependency link failed");
  }
}

async function clockInTodo(todo: TodoItem): Promise<void> {
  if (!deps) return;
  try {
    await deps.api.notes.clockIn(todoPatchBase(todo));
    await fetchAgenda();
  } catch (error) {
    deps.setStatus(error instanceof Error ? error.message : "Clock in failed");
  }
}

async function clockOutRunning(): Promise<void> {
  if (!deps) return;
  try {
    await deps.api.notes.clockOut({});
    await fetchAgenda();
  } catch (error) {
    deps.setStatus(error instanceof Error ? error.message : "Clock out failed");
  }
}

function toggleMark(id: string): void {
  if (selection.has(id)) selection.delete(id);
  else selection.add(id);
  render();
}

async function bulkStatus(): Promise<void> {
  if (!deps || selection.size === 0) return;
  const value = window.prompt("Bulk set status (todo/doing/done/blocked/cancelled):", "done");
  if (!value) return;
  const status = value.trim().toLowerCase();
  const ids = [...selection];
  selection.clear();
  for (const id of ids) {
    const todo = todoById(id);
    if (!todo) continue;
    if (status === "done") await applyPatch(todo, { op: "complete" });
    else await applyPatch(todo, { status });
  }
}

// --- rendering ---

function prioClass(prio: string): string {
  if (prio === "A") return "prio-a";
  if (prio === "B") return "prio-b";
  return "prio-other";
}

function buildRow(todo: TodoItem, opts: { badge?: string } = {}): HTMLElement {
  const row = document.createElement("div");
  row.className = "aaronnote-agenda-full-row is-task-row";
  row.dataset.status = todoStatus(todo);
  row.dataset.todoId = String(todo.id || "");
  row.tabIndex = 0;
  row.setAttribute("role", "button");
  if (todo.id === cursorId) row.classList.add("is-cursor");
  if (todo.id && selection.has(String(todo.id))) row.classList.add("is-selected");

  const mark = document.createElement("span");
  mark.className = "aaronnote-agenda-full-mark";
  mark.textContent = todo.id && selection.has(String(todo.id)) ? "■" : "□";
  mark.addEventListener("click", (event) => {
    event.stopPropagation();
    toggleMark(String(todo.id || ""));
  });

  const status = document.createElement("span");
  status.className = "aaronnote-agenda-full-status";
  status.textContent = todoStatus(todo).toUpperCase();
  status.addEventListener("click", (event) => {
    event.stopPropagation();
    cycleStatus(todo);
  });

  const prio = document.createElement("span");
  const prioValue = todoPrio(todo);
  prio.className = `aaronnote-agenda-full-prio ${prioValue ? prioClass(prioValue) : ""}`;
  prio.textContent = prioValue ? `#${prioValue}` : "";
  prio.addEventListener("click", (event) => {
    event.stopPropagation();
    editPriority(todo);
  });

  const badge = document.createElement("span");
  badge.className = "aaronnote-agenda-full-badge";
  badge.textContent = opts.badge || "";

  const clock = document.createElement("span");
  clock.className = "aaronnote-agenda-full-clock";
  const runningTodoId = data?.clocktable?.running?.todoId || "";
  const status0 = todoStatus(todo);
  if (todo.id && runningTodoId === String(todo.id)) {
    clock.classList.add("is-running");
    clock.textContent = `⏱ ${data?.clocktable?.running?.minutesSoFar ?? 0}m`;
    clock.title = "Clock out";
    clock.addEventListener("click", (event) => {
      event.stopPropagation();
      void clockOutRunning();
    });
  } else if (status0 !== "done" && status0 !== "cancelled") {
    clock.textContent = "⏱";
    clock.title = "Clock in";
    clock.addEventListener("click", (event) => {
      event.stopPropagation();
      void clockInTodo(todo);
    });
  }

  const body = document.createElement("span");
  body.className = "aaronnote-agenda-full-body";
  const text = document.createElement("span");
  text.className = "aaronnote-agenda-full-text";
  text.textContent = todoText(todo);
  const note = document.createElement("span");
  note.className = "aaronnote-agenda-full-note";
  note.textContent = todoNote(todo);
  body.append(text, note);

  const canon = (todo.canon as Record<string, string> | undefined) || {};
  if (canon.after) {
    const dep = document.createElement("span");
    dep.className = "aaronnote-agenda-full-dep";
    dep.textContent = `after: ${canon.after}`;
    body.appendChild(dep);
  }
  if (canon.repeat) {
    const rep = document.createElement("span");
    rep.className = "aaronnote-agenda-full-repeat";
    rep.textContent = `↻ ${canon.repeat}`;
    body.appendChild(rep);
  }
  const rowLints = lintsFor(String(todo.id || ""));
  if (rowLints.length > 0) {
    const lint = document.createElement("span");
    lint.className = "aaronnote-agenda-full-lint";
    lint.title = rowLints.map((l) => l.message || l.kind || "").join("; ");
    lint.textContent = "⚠";
    body.appendChild(lint);
  }

  row.append(mark, status, prio, clock, badge, body);
  row.addEventListener("click", () => {
    cursorId = String(todo.id || "");
    if (deps) void deps.jumpToTodo(todo);
  });
  return row;
}

function lintDetail(lint: TodoLint): string {
  const ref = typeof lint.ref === "string" && lint.ref.trim() && lint.ref.trim() !== "undefined" ? lint.ref.trim() : "";
  const todo = lint.todoId ? todoById(lint.todoId) : undefined;
  const subject = ref || (todo ? todoText(todo) : "") || lint.message || lint.kind || "issue";
  const label = ref || todo ? `"${subject}"` : subject;
  return `${label} (${lint.kind || "lint"})`;
}

function renderLints(): HTMLElement | null {
  if (!data?.lints?.length) return null;
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-lints";
  wrap.textContent = `${data.lints.length} issue${data.lints.length === 1 ? "" : "s"}: `;
  wrap.textContent += data.lints.slice(0, 6).map(lintDetail).join(", ");
  return wrap;
}

function renderWeek(): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-week";
  for (const day of data?.days || []) {
    const col = document.createElement("div");
    col.className = "aaronnote-agenda-full-day";
    if (day.date === data?.range?.today) col.classList.add("is-today");
    const head = document.createElement("div");
    head.className = "aaronnote-agenda-full-day-head";
    head.textContent = day.date || "";
    col.appendChild(head);
    const entries = (day.entries || []).filter((e) => {
      const todo = e.todoId ? todoById(e.todoId) : undefined;
      return todo ? matchesQuery(todo) : true;
    });
    if (entries.length === 0) {
      const empty = document.createElement("div");
      empty.className = "aaronnote-empty";
      empty.textContent = "—";
      col.appendChild(empty);
    }
    for (const entry of entries) {
      const todo = entry.todoId ? todoById(entry.todoId) : undefined;
      if (!todo) continue;
      col.appendChild(buildRow(todo, { badge: entry.label || "" }));
    }
    wrap.appendChild(col);
  }
  return wrap;
}

function renderList(): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-list";
  const todos = (data?.todos || [])
    .filter((t) => t.status !== "done" && t.status !== "cancelled")
    .filter(matchesQuery);
  const byNote = new Map<string, TodoItem[]>();
  for (const todo of todos) {
    const key = todoNote(todo);
    if (!byNote.has(key)) byNote.set(key, []);
    byNote.get(key)!.push(todo);
  }
  for (const [noteTitle, group] of [...byNote.entries()].sort(([a], [b]) => a.localeCompare(b))) {
    const head = document.createElement("div");
    head.className = "aaronnote-agenda-full-group-head";
    head.textContent = noteTitle;
    wrap.appendChild(head);
    for (const todo of group) wrap.appendChild(buildRow(todo));
  }
  if (todos.length === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-empty";
    empty.textContent = "No matching tasks";
    wrap.appendChild(empty);
  }
  return wrap;
}

function renderMonth(): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-month";
  const first = startOfMonth(anchorMs);
  const firstWeekday = new Date(first).getDay();
  for (let i = 0; i < firstWeekday; i++) {
    const filler = document.createElement("div");
    filler.className = "aaronnote-agenda-full-month-cell is-filler";
    wrap.appendChild(filler);
  }
  for (const day of data?.days || []) {
    const cell = document.createElement("div");
    cell.className = "aaronnote-agenda-full-month-cell";
    if (day.date === data?.range?.today) cell.classList.add("is-today");
    const dateNum = document.createElement("div");
    dateNum.className = "aaronnote-agenda-full-month-date";
    dateNum.textContent = String(Number(day.date?.slice(-2) || 0));
    cell.appendChild(dateNum);
    const count = (day.entries || []).length;
    if (count > 0) {
      const badge = document.createElement("div");
      badge.className = "aaronnote-agenda-full-month-badge";
      badge.textContent = String(count);
      cell.appendChild(badge);
    }
    cell.addEventListener("click", () => {
      anchorMs = parseYmd(day.date || fmtDate(anchorMs));
      view = "week";
      void fetchAgenda();
    });
    wrap.appendChild(cell);
  }
  return wrap;
}

function renderLog(): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-list";
  const days = [...(data?.days || [])].reverse();
  for (const day of days) {
    const closed = (day.entries || []).filter((e) => e.kind === "log");
    if (closed.length === 0) continue;
    const head = document.createElement("div");
    head.className = "aaronnote-agenda-full-group-head";
    head.textContent = `${day.date} — ${closed.length} closed`;
    wrap.appendChild(head);
    for (const entry of closed) {
      const todo = entry.todoId ? todoById(entry.todoId) : undefined;
      if (todo) wrap.appendChild(buildRow(todo));
    }
  }
  return wrap;
}

// --- Gantt ---

function ganttRange(tasks: GanttTask[]): { min: number; days: number } {
  const vals: number[] = [];
  for (const t of tasks) {
    if (t.start) vals.push(parseYmd(t.start));
    if (t.end) vals.push(parseYmd(t.end));
  }
  const finite = vals.filter((v) => Number.isFinite(v));
  const rawMin = finite.length ? Math.min(...finite) : Date.now();
  const rawMax = finite.length ? Math.max(...finite) : rawMin + 14 * DAY_MS;
  const min = rawMin - 2 * DAY_MS;
  const max = rawMax + 3 * DAY_MS;
  return { min, days: Math.max(7, Math.round((max - min) / DAY_MS) + 1) };
}

function ganttPatchBase(task: GanttTask, patch: Record<string, unknown>): Record<string, unknown> {
  const source = (task.source || {}) as Record<string, unknown>;
  return { file: source.file, index: source.index, source: source.source, text: source.text, ...patch };
}

async function bumpGanttProgress(task: GanttTask): Promise<void> {
  if (!deps) return;
  const next = ((Number(task.progress) || 0) + 25) % 125;
  try {
    await deps.api.notes.patchTodo(ganttPatchBase(task, { progress: next > 100 ? 0 : next }));
    await fetchAgenda();
  } catch (error) {
    deps.setStatus(error instanceof Error ? error.message : "Progress update failed");
  }
}

async function rescheduleGanttTask(task: GanttTask, deltaDays: number): Promise<void> {
  if (!deps || !deltaDays) return;
  const startMs = task.start ? parseYmd(task.start) : Date.now();
  const endMs = task.end ? parseYmd(task.end) : startMs;
  const nextStart = fmtDate(startMs + deltaDays * DAY_MS);
  const nextEnd = fmtDate(endMs + deltaDays * DAY_MS);
  try {
    await deps.api.notes.patchTodo(ganttPatchBase(task, { sche: nextStart, end: nextEnd, ddl: nextEnd }));
    await fetchAgenda();
  } catch (error) {
    deps.setStatus(error instanceof Error ? error.message : "Reschedule failed");
  }
}

function renderGanttBar(task: GanttTask, range: { min: number; days: number }): HTMLElement {
  const line = document.createElement("div");
  line.className = "aaronnote-gantt-line";

  const name = document.createElement("div");
  name.className = "aaronnote-gantt-name";
  const progressBtn = document.createElement("button");
  progressBtn.type = "button";
  progressBtn.textContent = `${task.progress || 0}%`;
  progressBtn.addEventListener("click", () => void bumpGanttProgress(task));
  const label = document.createElement("span");
  label.textContent = task.name || "";
  const sub = document.createElement("small");
  sub.textContent = task.project || "";
  name.append(progressBtn, label, document.createElement("br"), sub);

  const timeline = document.createElement("div");
  timeline.className = "aaronnote-gantt-timeline";
  const bar = document.createElement("div");
  bar.className = `aaronnote-gantt-bar ${task.status || ""}`;
  bar.draggable = true;
  const startMs = task.start ? parseYmd(task.start) : range.min;
  const endMs = task.end ? parseYmd(task.end) : startMs;
  const totalMs = range.days * DAY_MS;
  bar.style.left = `${((startMs - range.min) / totalMs) * 100}%`;
  bar.style.width = `${Math.max(1, ((endMs - startMs + DAY_MS) / totalMs) * 100)}%`;
  bar.textContent = task.name || "";
  bar.addEventListener("dragstart", (ev) => {
    ganttDragging = { id: String(task.id || ""), x: ev.clientX };
  });
  bar.addEventListener("dragend", (ev) => {
    if (!ganttDragging) return;
    const dragStart = ganttDragging;
    ganttDragging = null;
    const rect = timeline.getBoundingClientRect();
    const delta = Math.round(((ev.clientX - dragStart.x) / rect.width) * range.days);
    void rescheduleGanttTask(task, delta);
  });
  timeline.appendChild(bar);

  line.append(name, timeline);
  return line;
}

function renderGanttHead(range: { min: number; days: number }): HTMLElement {
  const head = document.createElement("div");
  head.className = "aaronnote-gantt-head";
  const name = document.createElement("div");
  name.className = "aaronnote-gantt-name";
  name.textContent = "Task";
  const ticks = document.createElement("div");
  ticks.className = "aaronnote-gantt-ticks";
  for (let i = 0; i < range.days; i++) {
    const tick = document.createElement("div");
    tick.className = "aaronnote-gantt-tick";
    tick.textContent = fmtDate(range.min + i * DAY_MS).slice(5);
    ticks.appendChild(tick);
  }
  head.append(name, ticks);
  return head;
}

function renderGantt(): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-gantt";
  const gantt = data?.gantt;
  const tasks = gantt?.tasks || [];
  const range = ganttRange(tasks);

  const chart = document.createElement("div");
  chart.className = "aaronnote-gantt-chart";
  chart.style.setProperty("--gantt-days", String(range.days));
  chart.appendChild(renderGanttHead(range));

  const laned = new Set<string>();
  for (const lane of gantt?.lanes || []) {
    const laneHead = document.createElement("div");
    laneHead.className = "aaronnote-gantt-lane-head";
    laneHead.textContent = lane.name || lane.key || "";
    chart.appendChild(laneHead);
    for (const childId of lane.childTaskIds || []) {
      laned.add(childId);
      const task = tasks.find((t) => t.id === childId);
      if (task) chart.appendChild(renderGanttBar(task, range));
    }
  }
  const rest = tasks.filter((t) => !laned.has(String(t.id || "")));
  if (rest.length > 0 && (gantt?.lanes?.length || 0) > 0) {
    const otherHead = document.createElement("div");
    otherHead.className = "aaronnote-gantt-lane-head";
    otherHead.textContent = "Other";
    chart.appendChild(otherHead);
  }
  for (const task of rest) chart.appendChild(renderGanttBar(task, range));
  wrap.appendChild(chart);

  if ((gantt?.milestones || []).length > 0) {
    const milestoneHead = document.createElement("div");
    milestoneHead.className = "aaronnote-agenda-full-group-head";
    milestoneHead.textContent = "Milestones";
    wrap.appendChild(milestoneHead);
    for (const m of gantt?.milestones || []) {
      const row = document.createElement("div");
      row.className = "aaronnote-agenda-full-row is-marker-row";
      row.tabIndex = 0;
      row.setAttribute("role", "button");
      const diamond = document.createElement("span");
      diamond.className = "aaronnote-agenda-full-mark";
      diamond.textContent = "◆";
      const body = document.createElement("span");
      body.className = "aaronnote-agenda-full-body";
      const text = document.createElement("span");
      text.className = "aaronnote-agenda-full-text";
      text.textContent = m.name || "Milestone";
      const note = document.createElement("span");
      note.className = "aaronnote-agenda-full-note";
      note.textContent = `${m.project || ""} · ${m.date || ""}`;
      body.append(text, note);
      row.append(diamond, body);
      const source = (m.source || {}) as Record<string, unknown>;
      row.addEventListener("click", () => {
        if (deps) void deps.jumpToTodo({ file: source.file, line: source.line } as unknown as TodoItem);
      });
      wrap.appendChild(row);
    }
  }

  if ((gantt?.backlog || []).length > 0) {
    const backlogHead = document.createElement("div");
    backlogHead.className = "aaronnote-agenda-full-group-head";
    backlogHead.textContent = "Backlog (unscheduled)";
    wrap.appendChild(backlogHead);
    for (const t of gantt?.backlog || []) {
      const todo = t.id ? todoById(t.id) : undefined;
      if (todo) wrap.appendChild(buildRow(todo, { badge: "unscheduled" }));
    }
  }

  return wrap;
}

// --- Projects ---

function renderProjects(): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-list";
  const projects = data?.projectModel || [];
  if (projects.length === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-empty";
    empty.textContent = "No @@project entries yet";
    wrap.appendChild(empty);
    return wrap;
  }
  for (const project of projects) {
    const card = document.createElement("div");
    card.className = "aaronnote-agenda-full-project";
    const title = document.createElement("h3");
    title.textContent = project.title || project.key || "";
    const meta = document.createElement("div");
    meta.className = "aaronnote-agenda-full-note";
    const parts = [
      `${project.progress ?? 0}% done`,
      `${project.open ?? 0} open`,
      `${project.doing ?? 0} doing`,
      `${project.blocked ?? 0} blocked`,
    ];
    if (project.effortMinutes) parts.push(`${Math.round((project.effortMinutes || 0) / 60)}h effort`);
    if (project.clockedMinutes) parts.push(`${Math.round((project.clockedMinutes || 0) / 60)}h clocked`);
    meta.textContent = parts.join(" · ");
    card.append(title, meta);
    wrap.appendChild(card);
  }
  return wrap;
}

// --- Clocktable ---

function renderClocktable(): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-list";
  const model = data?.clocktable;
  if (!model) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-empty";
    empty.textContent = "No clock data";
    wrap.appendChild(empty);
    return wrap;
  }
  if (model.running) {
    const running = document.createElement("div");
    running.className = "aaronnote-agenda-full-clocktable-running";
    running.textContent = `● Running: ${model.running.text || ""} (${model.running.minutesSoFar ?? 0}m)`;
    const stop = document.createElement("button");
    stop.type = "button";
    stop.textContent = "Clock out";
    stop.addEventListener("click", () => void clockOutRunning());
    running.appendChild(stop);
    wrap.appendChild(running);
  }
  const tasksHead = document.createElement("div");
  tasksHead.className = "aaronnote-agenda-full-group-head";
  tasksHead.textContent = "By task";
  wrap.appendChild(tasksHead);
  for (const task of model.tasks || []) {
    const row = document.createElement("div");
    row.className = "aaronnote-agenda-full-row is-simple-row";
    const body = document.createElement("span");
    body.className = "aaronnote-agenda-full-body";
    const text = document.createElement("span");
    text.className = "aaronnote-agenda-full-text";
    text.textContent = task.text || "(untitled)";
    const note = document.createElement("span");
    note.className = "aaronnote-agenda-full-note";
    const hours = ((task.minutes || 0) / 60).toFixed(1);
    const effort = task.effortMinutes ? ` / ${(task.effortMinutes / 60).toFixed(1)}h effort` : "";
    note.textContent = `${hours}h${effort}`;
    body.append(text, note);
    row.appendChild(body);
    wrap.appendChild(row);
  }
  const dayHead = document.createElement("div");
  dayHead.className = "aaronnote-agenda-full-group-head";
  dayHead.textContent = "By day";
  wrap.appendChild(dayHead);
  for (const [day, minutes] of Object.entries(model.byDay || {})) {
    const row = document.createElement("div");
    row.className = "aaronnote-agenda-full-row is-simple-row";
    const body = document.createElement("span");
    body.className = "aaronnote-agenda-full-body";
    body.textContent = `${day} — ${(minutes / 60).toFixed(1)}h`;
    row.appendChild(body);
    wrap.appendChild(row);
  }
  return wrap;
}

// --- Lints ---

function renderLintsView(): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-list";
  const lints = data?.lints || [];
  if (lints.length === 0) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-empty";
    empty.textContent = "No lints";
    wrap.appendChild(empty);
    return wrap;
  }
  for (const lint of lints) {
    const row = document.createElement("div");
    row.className = "aaronnote-agenda-full-lint";
    const kind = document.createElement("b");
    kind.textContent = lint.kind || "";
    row.appendChild(kind);
    row.appendChild(document.createTextNode(` ${lint.message || lint.ref || ""} `));
    const where = document.createElement("small");
    where.textContent = `${lint.file || ""}${lint.line ? `:${lint.line}` : ""}`;
    row.appendChild(where);
    wrap.appendChild(row);
  }
  return wrap;
}

function flatTodoIds(): string[] {
  if (!listEl) return [];
  return [...listEl.querySelectorAll<HTMLElement>("[data-todo-id]")].map((el) => el.dataset.todoId || "");
}

function moveCursor(delta: number): void {
  const ids = flatTodoIds();
  if (ids.length === 0) return;
  const idx = Math.max(0, ids.indexOf(cursorId));
  const next = Math.min(ids.length - 1, Math.max(0, idx + delta));
  cursorId = ids[next];
  render();
  listEl?.querySelector<HTMLElement>(`[data-todo-id="${CSS.escape(cursorId)}"]`)?.scrollIntoView({ block: "nearest" });
}

function renderHeader(): void {
  if (!headerEl) return;
  headerEl.replaceChildren();

  const views: Array<[ViewKind, string]> = [
    ["week", "Week"],
    ["list", "List"],
    ["month", "Month"],
    ["log", "Log"],
    ["gantt", "Gantt"],
    ["projects", "Projects"],
    ["clocktable", "Clock"],
    ["lints", "Lints"],
  ];
  const tabs = document.createElement("div");
  tabs.className = "aaronnote-agenda-full-tabs";
  for (const [kind, label] of views) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = kind === view ? "is-active" : "";
    button.textContent = label;
    button.addEventListener("click", () => {
      view = kind;
      syncPageUrl();
      void fetchAgenda();
    });
    tabs.appendChild(button);
  }
  headerEl.appendChild(tabs);

  if (!WIDE_VIEWS.has(view)) {
    const nav = document.createElement("div");
    nav.className = "aaronnote-agenda-full-nav";
    const prev = document.createElement("button");
    prev.type = "button";
    prev.textContent = "←";
    prev.addEventListener("click", () => {
      anchorMs -= view === "month" ? daysInMonth(anchorMs) * DAY_MS : 7 * DAY_MS;
      void fetchAgenda();
    });
    const today = document.createElement("button");
    today.type = "button";
    today.textContent = "Today";
    today.addEventListener("click", () => {
      anchorMs = midnight(Date.now());
      void fetchAgenda();
    });
    const next = document.createElement("button");
    next.type = "button";
    next.textContent = "→";
    next.addEventListener("click", () => {
      anchorMs += view === "month" ? daysInMonth(anchorMs) * DAY_MS : 7 * DAY_MS;
      void fetchAgenda();
    });
    nav.append(prev, today, next);
    headerEl.appendChild(nav);
  }

  const search = document.createElement("input");
  search.type = "search";
  search.value = query;
  search.placeholder = "Search status, priority, note, text, tag...";
  search.addEventListener("input", () => {
    query = search.value;
    syncPageUrl();
    render();
  });
  headerEl.appendChild(search);

  const create = document.createElement("button");
  create.type = "button";
  create.className = "aaronnote-agenda-full-primary";
  create.textContent = "New";
  create.title = "Create todo (n)";
  create.addEventListener("click", () => void createTodoFromPrompt());
  headerEl.appendChild(create);

  const help = document.createElement("button");
  help.type = "button";
  help.textContent = "?";
  help.title = "Keyboard help";
  help.addEventListener("click", showHelp);
  headerEl.appendChild(help);

  if (selection.size > 0) {
    const bulk = document.createElement("button");
    bulk.type = "button";
    bulk.className = "aaronnote-agenda-full-bulk";
    bulk.textContent = `Bulk (${selection.size})`;
    bulk.addEventListener("click", () => void bulkStatus());
    headerEl.appendChild(bulk);
  }

  if (data?.clocktable?.running) {
    const running = data.clocktable.running;
    const clockBadge = document.createElement("button");
    clockBadge.type = "button";
    clockBadge.className = "aaronnote-agenda-full-clock-badge";
    clockBadge.textContent = `⏱ ${running.text || ""} (${running.minutesSoFar ?? 0}m)`;
    clockBadge.title = "Clock out";
    clockBadge.addEventListener("click", () => void clockOutRunning());
    headerEl.appendChild(clockBadge);
  }

  if (!deps?.pageMode) {
    const close = document.createElement("button");
    close.type = "button";
    close.className = "aaronnote-agenda-full-close";
    close.textContent = "Close";
    close.addEventListener("click", closeAgendaView);
    headerEl.appendChild(close);
  }

  const stats = document.createElement("div");
  stats.className = "aaronnote-agenda-full-stats";
  if (data?.stats) {
    const s = data.stats;
    stats.textContent = `${s.open || 0} open · ${s.doing || 0} doing · ${s.blocked || 0} blocked · ${s.overdue || 0} overdue`;
  }
  headerEl.appendChild(stats);
}

function render(): void {
  if (!overlay || !listEl) return;
  renderHeader();
  listEl.replaceChildren();
  if (loading) {
    const spinner = document.createElement("div");
    spinner.className = "aaronnote-empty";
    spinner.textContent = "Loading agenda...";
    listEl.appendChild(spinner);
    return;
  }
  if (!data) {
    const empty = document.createElement("div");
    empty.className = "aaronnote-empty";
    empty.textContent = "Agenda unavailable";
    listEl.appendChild(empty);
    return;
  }
  if (view !== "lints") {
    const lints = renderLints();
    if (lints) listEl.appendChild(lints);
  }
  if (view === "week") listEl.appendChild(renderWeek());
  else if (view === "list") listEl.appendChild(renderList());
  else if (view === "month") listEl.appendChild(renderMonth());
  else if (view === "log") listEl.appendChild(renderLog());
  else if (view === "gantt") listEl.appendChild(renderGantt());
  else if (view === "projects") listEl.appendChild(renderProjects());
  else if (view === "clocktable") listEl.appendChild(renderClocktable());
  else listEl.appendChild(renderLintsView());

  if (!cursorId) {
    const ids = flatTodoIds();
    if (ids.length > 0) cursorId = ids[0];
  }
}

function hasCommandModifier(event: KeyboardEvent): boolean {
  return event.isComposing || event.metaKey || event.ctrlKey || event.altKey;
}

function handleKeydown(event: KeyboardEvent): void {
  if (!overlay || overlay.hidden) return;
  const target = event.target as HTMLElement | null;
  if (target && (target.tagName === "INPUT" || target.tagName === "TEXTAREA")) {
    if (event.key === "Escape" && !hasCommandModifier(event)) { event.preventDefault(); closeAgendaView(); }
    return;
  }
  if (hasCommandModifier(event)) return;
  const todo = cursorId ? todoById(cursorId) : undefined;
  switch (event.key) {
    case "Escape":
    case "q":
      event.preventDefault();
      closeAgendaView();
      break;
    case "j":
    case "ArrowDown":
      event.preventDefault();
      moveCursor(1);
      break;
    case "k":
    case "ArrowUp":
      event.preventDefault();
      moveCursor(-1);
      break;
    case "Enter":
    case "Tab":
      event.preventDefault();
      if (todo && deps) void deps.jumpToTodo(todo);
      break;
    case "t":
      event.preventDefault();
      if (todo) cycleStatus(todo);
      break;
    case "n":
      event.preventDefault();
      void createTodoFromPrompt();
      break;
    case "p":
    case ",":
      event.preventDefault();
      if (todo) editPriority(todo);
      break;
    case "d":
      event.preventDefault();
      if (todo) editDeadline(todo);
      break;
    case "s":
      event.preventDefault();
      if (todo) editScheduled(todo);
      break;
    case "r":
      event.preventDefault();
      if (todo) editRepeat(todo);
      break;
    case "a":
      event.preventDefault();
      if (todo) void addDependency(todo);
      break;
    case "m":
    case "u":
      event.preventDefault();
      if (todo?.id) toggleMark(String(todo.id));
      break;
    case "B":
      event.preventDefault();
      void bulkStatus();
      break;
    case "f":
      event.preventDefault();
      anchorMs += view === "month" ? daysInMonth(anchorMs) * DAY_MS : 7 * DAY_MS;
      void fetchAgenda();
      break;
    case "b":
      event.preventDefault();
      anchorMs -= view === "month" ? daysInMonth(anchorMs) * DAY_MS : 7 * DAY_MS;
      void fetchAgenda();
      break;
    case ".":
      event.preventDefault();
      anchorMs = midnight(Date.now());
      void fetchAgenda();
      break;
    case "v": {
      event.preventDefault();
      const order: ViewKind[] = ["week", "list", "month", "log", "gantt", "projects", "clocktable", "lints"];
      view = order[(order.indexOf(view) + 1) % order.length];
      syncPageUrl();
      void fetchAgenda();
      break;
    }
    case "c":
      event.preventDefault();
      if (data?.clocktable?.running) void clockOutRunning();
      else if (todo) void clockInTodo(todo);
      break;
    case "g":
      event.preventDefault();
      void fetchAgenda();
      break;
    case "?":
      event.preventDefault();
      showHelp();
      break;
    default:
      break;
  }
}

function ensureOverlay(): void {
  if (overlay) return;
  overlay = document.createElement("section");
  overlay.className = "aaronnote-agenda-full";
  overlay.hidden = true;
  overlay.innerHTML = `
    <div class="aaronnote-agenda-full-header" data-agenda-full-header></div>
    <div class="aaronnote-agenda-full-body" data-agenda-full-list></div>
  `;
  document.body.appendChild(overlay);
  headerEl = overlay.querySelector<HTMLElement>("[data-agenda-full-header]");
  listEl = overlay.querySelector<HTMLElement>("[data-agenda-full-list]");
  document.addEventListener("keydown", handleKeydown, true);
}

export function closeAgendaView(): void {
  if (overlay) overlay.hidden = true;
  selection.clear();
}

// Re-fetches without resetting view/anchor/cursor — for SSE-driven refresh
// (`agenda-changed`/`notes-index-changed`) so a background edit doesn't
// yank the user back to today's view.
export async function refreshAgendaView(): Promise<void> {
  if (!overlay || overlay.hidden) return;
  await fetchAgenda();
}

export async function openAgendaView(nextDeps: AgendaViewDeps): Promise<void> {
  deps = nextDeps;
  ensureOverlay();
  if (!overlay) return;
  overlay.hidden = false;
  anchorMs = midnight(Date.now());
  cursorId = "";
  if (nextDeps.pageMode && typeof location !== "undefined") {
    const params = new URLSearchParams(location.search);
    view = normalizeView(params.get("view"));
    query = params.get("q") || "";
  } else {
    view = "week";
    query = "";
  }
  await fetchAgenda();
}
