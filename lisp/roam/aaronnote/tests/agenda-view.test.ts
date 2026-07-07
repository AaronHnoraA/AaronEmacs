import { afterEach, describe, expect, test, vi } from "@voidzero-dev/vite-plus-test";

import { closeAgendaView, openAgendaView } from "../aaronnote/agenda-view.ts";
import type { AgendaViewDeps } from "../aaronnote/agenda-view.ts";
import type { AgendaMsg } from "../aaronnote/api-client.ts";

const emptyAgenda: AgendaMsg = {
  type: "agenda",
  range: { from: "2026-07-07", to: "2026-07-07", today: "2026-07-07" },
  days: [{ date: "2026-07-07", entries: [] }],
  todos: [],
  lints: [],
  stats: { open: 0, doing: 0, done: 0, cancelled: 0, blocked: 0, overdue: 0 },
};

function deps(): AgendaViewDeps {
  return {
    api: {
      notes: {
        agenda: async () => emptyAgenda,
        createTodo: async () => ({}),
        patchTodo: async () => ({}),
        todoDepRef: async () => ({}),
        clockIn: async () => ({}),
        clockOut: async () => ({}),
      },
    },
    jumpToTodo: () => {},
    setStatus: () => {},
  };
}

afterEach(() => {
  vi.restoreAllMocks();
  closeAgendaView();
});

describe("agenda keyboard handling", () => {
  test("does not treat Meta-q as the agenda q shortcut", async () => {
    await openAgendaView(deps());
    const overlay = document.querySelector<HTMLElement>(".aaronnote-agenda-full")!;

    const metaQ = new KeyboardEvent("keydown", {
      key: "q",
      code: "KeyQ",
      metaKey: true,
      bubbles: true,
      cancelable: true,
    });
    document.dispatchEvent(metaQ);
    expect(metaQ.defaultPrevented).toBe(false);
    expect(overlay.hidden).toBe(false);

    const plainQ = new KeyboardEvent("keydown", {
      key: "q",
      code: "KeyQ",
      bubbles: true,
      cancelable: true,
    });
    document.dispatchEvent(plainQ);
    expect(plainQ.defaultPrevented).toBe(true);
    expect(overlay.hidden).toBe(true);
  });

  test("n creates a quick todo from the agenda", async () => {
    const d = deps();
    const createTodo = vi.fn(async () => ({ todo: { id: "new-todo" } }));
    d.api.notes.createTodo = createTodo;
    Object.defineProperty(window, "prompt", {
      value: vi.fn(() => "Write intro | ddl=today | prio=A"),
      configurable: true,
    });

    await openAgendaView(d);
    const key = new KeyboardEvent("keydown", {
      key: "n",
      code: "KeyN",
      bubbles: true,
      cancelable: true,
    });
    document.dispatchEvent(key);
    await Promise.resolve();
    await Promise.resolve();

    expect(key.defaultPrevented).toBe(true);
    expect(createTodo).toHaveBeenCalledWith({ text: "Write intro", ddl: "today", prio: "A" });
  });
});
