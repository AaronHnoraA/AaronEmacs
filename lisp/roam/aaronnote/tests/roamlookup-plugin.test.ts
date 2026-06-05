import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

// @ts-ignore The plugin is bundled from the local Aaronnote plugin directory.
import { setup } from "../plugin/roamlookup/index.ts";

async function flushPromises(count = 20): Promise<void> {
  for (let i = 0; i < count; i++) await Promise.resolve();
}

function mountLookup(requestImpl: (action: string, body: unknown) => Promise<unknown>) {
  document.body.innerHTML = `
    <div id="app"></div>
    <section data-notes-page>
      <div class="aaronnote-notes-tabs">
        <button type="button" data-notes-tab="filesystem">Filesystem</button>
      </div>
      <div class="aaronnote-notes-inner">
        <div data-notes-panel="filesystem"></div>
      </div>
    </section>
  `;
  const target = window as Window & { aaronnoteApi?: { roamlookup?: { request?: (action: string, body?: unknown) => Promise<unknown> } } };
  const oldApi = target.aaronnoteApi;
  const handlers: { action?: (action: string) => void } = {};
  target.aaronnoteApi = {
    ...(oldApi ?? {}),
    roamlookup: {
      ...(oldApi?.roamlookup ?? {}),
      request: requestImpl,
    },
  };
  const cleanup = setup({
    id: "roamlookup",
    root: document.querySelector<HTMLElement>("#app")!,
    host: document.createElement("div"),
    currentFile: () => "/tmp/current.md",
    setStatus: () => {},
    onAction: (handler: (action: string) => void) => {
      handlers.action = handler;
      return () => {
        delete handlers.action;
      };
    },
    onSettingsChange: () => () => {},
    getSettings: () => ({ idleMs: 10_000 }),
  });
  return {
    handlers,
    cleanup: () => {
      cleanup();
      if (oldApi) target.aaronnoteApi = oldApi;
      else delete target.aaronnoteApi;
      document.body.innerHTML = "";
    },
  };
}

describe("roamlookup plugin", () => {
  test("renders assistant note links and opens them in a new window through Aaronnote", async () => {
    const opened: Array<{ href?: string; newWindow?: boolean }> = [];
    const requests: string[] = [];
    const requestImpl = async (action: string) => {
      requests.push(action);
      if (action === "start") {
        return { ok: true, sessionId: "s1", status: "Ready", idleMs: 10_000 };
      }
      if (action === "query") {
        return { ok: true, sessionId: "s1", answer: "See [Ethics](roam/Philosophy/ethics.md:12)." };
      }
      return { ok: true };
    };
    const { handlers, cleanup } = mountLookup(requestImpl);
    const listener = (event: Event) => {
      opened.push((event as CustomEvent<{ href?: string; newWindow?: boolean }>).detail || {});
    };
    document.addEventListener("aaronnote:open-url", listener);

    try {
      handlers.action?.("open");
      await flushPromises();
      const input = document.querySelector<HTMLTextAreaElement>(".aaronnote-roamlookup-form textarea")!;
      input.value = "lookup ethics";
      input.closest("form")?.dispatchEvent(new Event("submit", { bubbles: true, cancelable: true }));
      await flushPromises();

      const link = document.querySelector<HTMLAnchorElement>(".aaronnote-roamlookup-message a[data-roamlookup-note='true']")!;
      expect(link.textContent).toBe("Ethics");
      link.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true }));

      expect(opened).toEqual([{ href: "roam/Philosophy/ethics.md", newWindow: true }]);
      expect(requests).toContain("query");
    } finally {
      document.removeEventListener("aaronnote:open-url", listener);
      cleanup();
    }
  });

  test("does not close on typing and starts close timer only after leaving lookup", async () => {
    const oldSetTimeout = window.setTimeout;
    const oldClearTimeout = window.clearTimeout;
    const scheduled: Array<() => void> = [];
    window.setTimeout = ((handler: TimerHandler) => {
      if (typeof handler === "function") scheduled.push(handler as () => void);
      return scheduled.length as unknown as number;
    }) as typeof window.setTimeout;
    window.clearTimeout = (() => {}) as typeof window.clearTimeout;

    const requests: string[] = [];
    const requestImpl = async (action: string) => {
      requests.push(action);
      if (action === "start") {
        return { ok: true, sessionId: "s1", status: "Ready", idleMs: 10_000 };
      }
      return { ok: true };
    };
    const { handlers, cleanup } = mountLookup(requestImpl);

    try {
      handlers.action?.("open");
      await flushPromises();
      const input = document.querySelector<HTMLTextAreaElement>(".aaronnote-roamlookup-form textarea")!;
      input.dispatchEvent(new Event("input", { bubbles: true }));
      expect(scheduled).toHaveLength(0);

      document.querySelector<HTMLButtonElement>("[data-notes-tab='filesystem']")!
        .dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true }));
      expect(scheduled).toHaveLength(1);
      scheduled[0]!();
      await Promise.resolve();
      expect(requests).toContain("close");
    } finally {
      cleanup();
      window.setTimeout = oldSetTimeout;
      window.clearTimeout = oldClearTimeout;
    }
  });
});
