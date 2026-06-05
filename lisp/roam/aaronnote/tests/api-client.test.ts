import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import { api } from "../aaronnote/api-client.ts";

type NativeBridge = NonNullable<Window["aaronnoteApi"]>;

async function withNativeBridge<T>(bridge: NativeBridge, run: () => Promise<T>): Promise<T> {
  const target = window as Window & { aaronnoteApi?: NativeBridge };
  const previousBridge = target.aaronnoteApi;
  const previousFetch = globalThis.fetch;
  target.aaronnoteApi = bridge;
  globalThis.fetch = (async () => {
    throw new Error("fetch should not be called when aaronnoteApi is available");
  }) as typeof fetch;
  try {
    return await run();
  } finally {
    if (previousBridge) target.aaronnoteApi = previousBridge;
    else delete target.aaronnoteApi;
    globalThis.fetch = previousFetch;
  }
}

describe("api client native bridge", () => {
  test("uses IPC save and preserves conflict payloads", async () => {
    const calls: unknown[] = [];
    await withNativeBridge({
      notes: {
        save: async (body) => {
          calls.push(body);
          return { type: "saved", ok: false, conflict: true, file: body.file, message: "conflict" };
        },
      },
    }, async () => {
      const msg = await api.notes.save({
        file: "/tmp/a.md",
        content: "# A\n",
        mode: "markdown",
        clientId: "test",
        seq: 1,
      });
      expect(msg.conflict).toBe(true);
      expect(calls).toHaveLength(1);
    });
  });

  test("throws native ok:false errors for normal requests", async () => {
    await withNativeBridge({
      fs: {
        rename: async () => ({ ok: false, message: "rename failed" }),
      },
    }, async () => {
      await expect(api.fs.rename({ path: "a.md", name: "b.md" })).rejects.toThrow("rename failed");
    });
  });

  test("uses native asset import from file path", async () => {
    const calls: unknown[] = [];
    await withNativeBridge({
      assets: {
        storeFromPath: async (body) => {
          calls.push(body);
          return { ok: true, file: "/tmp/images/a/pic.png", name: "pic.png", isImage: true, markdownPath: "./images/a/pic.png" };
        },
      },
    }, async () => {
      const msg = await api.assets.storeFromPath({
        file: "/tmp/a.md",
        path: "/tmp/pic.png",
        name: "pic.png",
        type: "image/png",
      });
      expect(msg.markdownPath).toBe("./images/a/pic.png");
      expect(calls).toHaveLength(1);
    });
  });

  test("native asset import requires the bridge", async () => {
    const target = window as Window & { aaronnoteApi?: NativeBridge };
    const previousBridge = target.aaronnoteApi;
    delete target.aaronnoteApi;
    try {
      await expect(api.assets.storeFromPath({ file: "/tmp/a.md", path: "/tmp/pic.png" }))
        .rejects.toThrow("Native IPC bridge is unavailable");
    } finally {
      if (previousBridge) target.aaronnoteApi = previousBridge;
    }
  });

  test("uses native shell reveal without fetch", async () => {
    const calls: string[] = [];
    await withNativeBridge({
      shell: {
        showInFolder: async (file) => {
          calls.push(file);
          return { ok: true, file };
        },
      },
    }, async () => {
      expect(api.shell.available()).toBe(true);
      await api.shell.showInFolder("notes/a.md");
      expect(calls).toEqual(["notes/a.md"]);
    });
  });

  test("uses native attachment context menu with its note base", async () => {
    const calls: string[][] = [];
    await withNativeBridge({
      shell: {
        showAttachmentMenu: async (file, base) => {
          calls.push([file, base || ""]);
          return { ok: true, file };
        },
      },
    }, async () => {
      await api.shell.showAttachmentMenu("./images/a.png", "notes/a.md");
      expect(calls).toEqual([["./images/a.png", "notes/a.md"]]);
    });
  });

  test("passes editor context menu options through the native bridge", async () => {
    const calls: unknown[] = [];
    await withNativeBridge({
      shell: {
        showEditorContextMenu: async (options) => {
          calls.push(options);
          return { ok: true };
        },
      },
    }, async () => {
      const options = { linkHref: "target.md", hasSelection: true, blockType: "paragraph" };
      await api.shell.showEditorContextMenu(options);
      expect(calls).toEqual([options]);
    });
  });

  test("fire-and-forget persistence uses IPC instead of beacon or fetch", async () => {
    const saves: unknown[] = [];
    const positions: unknown[] = [];
    await withNativeBridge({
      notes: {
        save: async (body) => {
          saves.push(body);
          return { type: "saved", ok: true, file: body.file };
        },
      },
      session: {
        savePosition: async (position) => {
          positions.push(position);
          return { type: "positions", positions: [position] };
        },
      },
    }, async () => {
      api.notes.saveKeepalive({
        file: "/tmp/a.md",
        content: "# A\n",
        mode: "markdown",
        clientId: "test",
        seq: 2,
      });
      api.session.savePosition({
        file: "/tmp/a.md",
        mode: "markdown",
        from: 0,
        to: 0,
        scrollY: 0,
        updatedAt: 1,
      }, true);
      await Promise.resolve();
      expect(saves).toHaveLength(1);
      expect(positions).toHaveLength(1);
    });
  });
});
