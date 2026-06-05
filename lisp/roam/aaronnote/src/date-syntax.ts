/**
 * Date parsing / formatting for inline-todo args and agenda.
 *
 * Accepts many user formats — ISO, slash, dot, CJK, m-d shorthand,
 * relative ("today"/"+3d"). Always normalizes to canonical
 * `YYYY-MM-DD` or `YYYY-MM-DD HH:MM`.
 */

export type ParsedDate = { time: number; hasTime: boolean };

function midnight(d: Date): number {
  return new Date(d.getFullYear(), d.getMonth(), d.getDate()).getTime();
}

function pad2(n: number): string { return String(n).padStart(2, "0"); }

export function parseDateValue(raw: string): ParsedDate | null {
  const t = String(raw ?? "").trim();
  if (!t) return null;
  const lower = t.toLowerCase();

  if (lower === "today" || lower === "今天") return { time: midnight(new Date()), hasTime: false };
  if (lower === "tomorrow" || lower === "明天") return { time: midnight(new Date()) + 86_400_000, hasTime: false };
  if (lower === "yesterday" || lower === "昨天") return { time: midnight(new Date()) - 86_400_000, hasTime: false };
  if (lower === "now") return { time: Date.now(), hasTime: true };

  const rel = lower.match(/^([+-])(\d+)\s*(d|day|days|w|week|weeks|m|month|months|y|year|years)$/);
  if (rel) {
    const sign = rel[1] === "-" ? -1 : 1;
    const n = Number(rel[2]) * sign;
    const u = rel[3]!;
    const base = new Date();
    base.setHours(0, 0, 0, 0);
    if (u.startsWith("d")) base.setDate(base.getDate() + n);
    else if (u.startsWith("w")) base.setDate(base.getDate() + 7 * n);
    else if (u.startsWith("m")) base.setMonth(base.getMonth() + n);
    else if (u.startsWith("y")) base.setFullYear(base.getFullYear() + n);
    return { time: base.getTime(), hasTime: false };
  }

  const now = new Date();
  const cjk = t.replace(/年|月/g, "-").replace(/日|号/g, "");
  const norm = cjk.replace(/[./]/g, "-").trim();

  let m = norm.match(/^(\d{4})-(\d{1,2})(?:-(\d{1,2}))?(?:[\sT](\d{1,2}):(\d{2}))?$/);
  if (m) {
    const y = Number(m[1]);
    const mo = Number(m[2]) - 1;
    const d = m[3] ? Number(m[3]) : 1;
    const hh = m[4] ? Number(m[4]) : 0;
    const mm = m[5] ? Number(m[5]) : 0;
    const date = new Date(y, mo, d, hh, mm);
    if (Number.isFinite(date.getTime())) {
      return { time: date.getTime(), hasTime: Boolean(m[4]) };
    }
  }

  m = norm.match(/^(\d{1,2})-(\d{1,2})(?:[\sT](\d{1,2}):(\d{2}))?$/);
  if (m) {
    const mo = Number(m[1]) - 1;
    const d = Number(m[2]);
    const hh = m[3] ? Number(m[3]) : 0;
    const mm = m[4] ? Number(m[4]) : 0;
    if (mo >= 0 && mo < 12 && d >= 1 && d <= 31) {
      const date = new Date(now.getFullYear(), mo, d, hh, mm);
      return { time: date.getTime(), hasTime: Boolean(m[3]) };
    }
  }

  const parsed = Date.parse(t);
  if (Number.isFinite(parsed)) {
    return { time: parsed, hasTime: /\d{1,2}:\d{2}/.test(t) };
  }
  return null;
}

export function formatDateValue(time: number, hasTime: boolean): string {
  const d = new Date(time);
  const base = `${d.getFullYear()}-${pad2(d.getMonth() + 1)}-${pad2(d.getDate())}`;
  if (!hasTime) return base;
  return `${base} ${pad2(d.getHours())}:${pad2(d.getMinutes())}`;
}

export function normalizeDateValue(raw: string): string | null {
  const parsed = parseDateValue(raw);
  if (!parsed) return null;
  return formatDateValue(parsed.time, parsed.hasTime);
}

export function relativeDateClass(time: number): "overdue" | "today" | "soon" | "future" {
  if (!Number.isFinite(time)) return "future";
  const today = midnight(new Date());
  const dayDiff = Math.floor((time - today) / 86_400_000);
  if (dayDiff < 0) return "overdue";
  if (dayDiff === 0) return "today";
  if (dayDiff <= 7) return "soon";
  return "future";
}

export function relativeDateLabel(time: number): string {
  if (!Number.isFinite(time)) return "";
  const today = midnight(new Date());
  const dayDiff = Math.floor((time - today) / 86_400_000);
  if (dayDiff < 0) return `${-dayDiff}d ago`;
  if (dayDiff === 0) return "today";
  if (dayDiff === 1) return "tomorrow";
  if (dayDiff < 7) return `in ${dayDiff}d`;
  if (dayDiff < 30) return `in ${Math.ceil(dayDiff / 7)}w`;
  return "later";
}

export const DATE_KEYS = new Set([
  "ddl", "due", "deadline", "scheduled", "start", "done", "date", "when",
]);

export const DATE_KEY_LABELS: Record<string, string> = {
  ddl: "DDL",
  due: "due",
  deadline: "DDL",
  scheduled: "scheduled",
  start: "start",
  done: "done",
  date: "on",
  when: "when",
};
