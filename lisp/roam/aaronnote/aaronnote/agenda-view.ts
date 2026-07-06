// Full-screen, vault-wide agenda view (org-agenda-class): week/list/month/log
// views over the server-computed agenda view-model (`api.notes.agenda`).
// The small `.is-agenda` modal in main.ts stays as a per-file quick panel;
// this is the first-class surface for priority/scheduled/deadline/repeat/
// dependency work across the whole vault. All edits round-trip through
// `api.notes.patchTodo`, which writes straight back into the `@@todo` line —
// this view holds no state that isn't re-derivable from the markdown.
import type { AgendaMsg, TodoItem, TodoLint } from "./api-client.ts";

export type AgendaViewDeps = {
  api: {
    notes: {
      agenda: (body: Record<string, unknown>) => Promise<AgendaMsg>;
      patchTodo: (body: Record<string, unknown>) => Promise<Record<string, unknown>>;
      todoDepRef: (body: Record<string, unknown>) => Promise<{ ref?: string }>;
    };
  };
  jumpToTodo: (todo: TodoItem) => void | Promise<void>;
  setStatus: (message: string) => void;
};

type ViewKind = "week" | "list" | "month" | "log";

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

async function fetchAgenda(): Promise<void> {
  if (!deps) return;
  loading = true;
  render();
  try {
    const from = view === "month" ? fmtDate(startOfMonth(anchorMs)) : fmtDate(anchorMs);
    const days = view === "month" ? daysInMonth(anchorMs) : view === "week" ? 7 : view === "list" ? 30 : 60;
    data = await deps.api.notes.agenda({ from, days });
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
  row.className = "aaronnote-agenda-full-row";
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

  row.append(mark, status, prio, badge, body);
  row.addEventListener("click", () => {
    cursorId = String(todo.id || "");
    if (deps) void deps.jumpToTodo(todo);
  });
  return row;
}

function renderLints(): HTMLElement | null {
  if (!data?.lints?.length) return null;
  const wrap = document.createElement("div");
  wrap.className = "aaronnote-agenda-full-lints";
  wrap.textContent = `${data.lints.length} dependency issue${data.lints.length === 1 ? "" : "s"}: `;
  const details = data.lints.slice(0, 6).map((l) => `"${l.ref}" (${l.kind})`).join(", ");
  wrap.textContent += details;
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

  const views: Array<[ViewKind, string]> = [["week", "Week"], ["list", "List"], ["month", "Month"], ["log", "Log"]];
  const tabs = document.createElement("div");
  tabs.className = "aaronnote-agenda-full-tabs";
  for (const [kind, label] of views) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = kind === view ? "is-active" : "";
    button.textContent = label;
    button.addEventListener("click", () => {
      view = kind;
      void fetchAgenda();
    });
    tabs.appendChild(button);
  }
  headerEl.appendChild(tabs);

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

  const search = document.createElement("input");
  search.type = "search";
  search.value = query;
  search.placeholder = "Search status, priority, note, text, tag...";
  search.addEventListener("input", () => {
    query = search.value;
    render();
  });
  headerEl.appendChild(search);

  if (selection.size > 0) {
    const bulk = document.createElement("button");
    bulk.type = "button";
    bulk.className = "aaronnote-agenda-full-bulk";
    bulk.textContent = `Bulk (${selection.size})`;
    bulk.addEventListener("click", () => void bulkStatus());
    headerEl.appendChild(bulk);
  }

  const close = document.createElement("button");
  close.type = "button";
  close.className = "aaronnote-agenda-full-close";
  close.textContent = "Close";
  close.addEventListener("click", closeAgendaView);
  headerEl.appendChild(close);

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
  const lints = renderLints();
  if (lints) listEl.appendChild(lints);
  if (view === "week") listEl.appendChild(renderWeek());
  else if (view === "list") listEl.appendChild(renderList());
  else if (view === "month") listEl.appendChild(renderMonth());
  else listEl.appendChild(renderLog());

  if (!cursorId) {
    const ids = flatTodoIds();
    if (ids.length > 0) cursorId = ids[0];
  }
}

function handleKeydown(event: KeyboardEvent): void {
  if (!overlay || overlay.hidden) return;
  const target = event.target as HTMLElement | null;
  if (target && (target.tagName === "INPUT" || target.tagName === "TEXTAREA")) {
    if (event.key === "Escape") { event.preventDefault(); closeAgendaView(); }
    return;
  }
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
      const order: ViewKind[] = ["week", "list", "month", "log"];
      view = order[(order.indexOf(view) + 1) % order.length];
      void fetchAgenda();
      break;
    }
    case "g":
      event.preventDefault();
      void fetchAgenda();
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

export async function openAgendaView(nextDeps: AgendaViewDeps): Promise<void> {
  deps = nextDeps;
  ensureOverlay();
  if (!overlay) return;
  overlay.hidden = false;
  view = "week";
  anchorMs = midnight(Date.now());
  cursorId = "";
  await fetchAgenda();
}
