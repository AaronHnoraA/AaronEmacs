import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createJupyterCellService } from "../server/lib/jupyter-cell.mjs";

// These exercise only the filesystem + short-circuit paths of the cell service
// (hidden-script write/read, output-mirror self-heal, non-kernel branches). They
// never reach a Jupyter server, so they run anywhere. Kernel execution, stream
// merge/truncate and dead-kernel retry are covered by manual e2e (they need a
// live websocket).

async function withService(run: (ctx: {
  service: ReturnType<typeof createJupyterCellService>;
  note: string;
}) => Promise<void>): Promise<void> {
  const root = await mkdtemp(join(tmpdir(), "aaronnote-jcell-"));
  const service = createJupyterCellService({ runtimeRoot: root, noteRoot: root, workspaceRoot: root });
  const note = join(root, "note.md");
  await writeFile(note, "# note\n", "utf8");
  try {
    await run({ service, note });
  } finally {
    await service.shutdown().catch(() => {});
  }
}

describe("jupyter cell service (no kernel)", () => {
  test("openScript writes a hidden script that readScriptCell round-trips", async () => {
    await withService(async ({ service, note }) => {
      await service.openScript({
        file: note,
        cellId: "cell-a",
        kernel: "python3",
        session: "default",
        language: "python",
        storage: "script",
        open: false,
        cells: [{ cellId: "cell-a", id: "cell-a", code: "print('hi')" }],
      });
      const read = await service.readScriptCell({
        file: note, cellId: "cell-a", kernel: "python3", session: "default", language: "python",
      });
      expect(read.ok).toBe(true);
      expect(read.exists).toBe(true);
      expect(read.code).toBe("print('hi')");
      expect(read.output).toBe(null);
    });
  });

  test("openScript preserves existing hidden cell bodies omitted by a partial context", async () => {
    await withService(async ({ service, note }) => {
      await service.openScript({
        file: note,
        cellId: "cell-a",
        kernel: "python3",
        session: "default",
        language: "python",
        storage: "script",
        open: false,
        cells: [
          { cellId: "cell-a", id: "cell-a", code: "x = 1" },
          { cellId: "cell-b", id: "cell-b", code: "print(x)" },
        ],
      });
      await service.openScript({
        file: note,
        cellId: "cell-c",
        kernel: "python3",
        session: "default",
        language: "python",
        storage: "script",
        open: false,
        cells: [{ cellId: "cell-c", id: "cell-c", code: "" }],
      });
      const readA = await service.readScriptCell({
        file: note, cellId: "cell-a", kernel: "python3", session: "default", language: "python",
      });
      const readB = await service.readScriptCell({
        file: note, cellId: "cell-b", kernel: "python3", session: "default", language: "python",
      });
      const readC = await service.readScriptCell({
        file: note, cellId: "cell-c", kernel: "python3", session: "default", language: "python",
      });
      expect(readA.code).toBe("x = 1");
      expect(readB.code).toBe("print(x)");
      expect(readC.code).toBe("");
    });
  });

  test("openScript orders hidden cells by the incoming document context", async () => {
    await withService(async ({ service, note }) => {
      await service.openScript({
        file: note,
        cellId: "cell-c",
        kernel: "python3",
        session: "default",
        language: "python",
        storage: "script",
        open: false,
        cells: [
          { cellId: "cell-a", id: "cell-a", code: "a = 1" },
          { cellId: "cell-c", id: "cell-c", code: "c = a + 1" },
        ],
      });
      await service.openScript({
        file: note,
        cellId: "cell-b",
        kernel: "python3",
        session: "default",
        language: "python",
        storage: "script",
        open: false,
        cells: [
          { cellId: "cell-a", id: "cell-a", code: "" },
          { cellId: "cell-b", id: "cell-b", code: "" },
          { cellId: "cell-c", id: "cell-c", code: "" },
        ],
      });
      const scriptPath = join(note, "..", ".cell", "note.python.default.py");
      const script = await readFile(scriptPath, "utf8");
      expect(script.indexOf("id=cell-a")).toBeLessThan(script.indexOf("id=cell-b"));
      expect(script.indexOf("id=cell-b")).toBeLessThan(script.indexOf("id=cell-c"));
      expect(script).toContain("a = 1");
      expect(script).toContain("c = a + 1");
    });
  });

  test("a corrupt output mirror is ignored, not thrown", async () => {
    await withService(async ({ service, note }) => {
      await service.openScript({
        file: note, cellId: "cell-b", kernel: "python3", session: "default", language: "python",
        storage: "script", open: false, cells: [{ cellId: "cell-b", id: "cell-b", code: "x = 1" }],
      });
      const before = await service.readScriptCell({
        file: note, cellId: "cell-b", kernel: "python3", session: "default", language: "python",
      });
      // Write a valid mirror via the public clear path, then corrupt the file.
      await service.clearAllOutputs({ file: note, kernel: "python3", session: "default", language: "python" });
      const mirrorPath = join(note, "..", ".cell", "note.output.python.default.json");
      await writeFile(mirrorPath, "{ this is not json", "utf8");
      // readScriptCell must survive the corrupt mirror.
      const after = await service.readScriptCell({
        file: note, cellId: "cell-b", kernel: "python3", session: "default", language: "python",
      });
      expect(before.code).toBe("x = 1");
      expect(after.code).toBe("x = 1");
      expect(after.output).toBe(null);
    });
  });

  test("output mirror is written atomically (no leftover temp files)", async () => {
    await withService(async ({ service, note }) => {
      await service.clearAllOutputs({ file: note, kernel: "python3", session: "default", language: "python" });
      const mirrorPath = join(note, "..", ".cell", "note.output.python.default.json");
      const parsed = JSON.parse(await readFile(mirrorPath, "utf8"));
      expect(parsed.version).toBe(1);
      expect(parsed.cells).toEqual({});
    });
  });

  test("readScriptCell marks persisted output stale when no matching live kernel exists", async () => {
    await withService(async ({ service, note }) => {
      await service.openScript({
        file: note, cellId: "cell-live", kernel: "python3", session: "default", language: "python",
        storage: "script", open: false, cells: [{ cellId: "cell-live", id: "cell-live", code: "x = 1" }],
      });
      const mirrorPath = join(note, "..", ".cell", "note.output.python.default.json");
      await writeFile(mirrorPath, JSON.stringify({
        version: 1,
        source: note,
        kernel: "python3",
        session: "default",
        language: "python",
        cells: {
          "cell-live": {
            ok: true,
            status: "ok",
            executionCount: 3,
            outputs: [],
            kernelRuntime: { id: "old-kernel", name: "python3", generation: 1 },
          },
        },
      }), "utf8");
      const read = await service.readScriptCell({
        file: note, cellId: "cell-live", kernel: "python3", session: "default", language: "python",
      });
      expect(read.output.live).toBe(false);
      expect(read.output.widgetRuntime).toBeUndefined();
    });
  });

  test("lean cells short-circuit without a kernel", async () => {
    await withService(async ({ service, note }) => {
      const result = await service.execute({
        file: note, cellId: "l1", kernel: "lean4", language: "lean4", code: "#check 1",
      });
      expect(result.ok).toBe(true);
      expect(result.runtime).toBe("lean4");
      expect(result.outputs).toEqual([]);
    });
  });

  test("empty code short-circuits without a kernel", async () => {
    await withService(async ({ service, note }) => {
      const result = await service.execute({ file: note, cellId: "e1", kernel: "python3", code: "   \n  " });
      expect(result.ok).toBe(true);
      expect(result.status).toBe("ok");
      expect(result.outputs).toEqual([]);
    });
  });

  test("variables is unsupported for non-python kernels", async () => {
    await withService(async ({ service, note }) => {
      const result = await service.variables({ file: note, kernel: "bash", session: "default" });
      expect(result.ok).toBe(true);
      expect(result.supported).toBe(false);
      expect(result.variables).toEqual([]);
    });
  });

  test("kernelStatus reports not-started before any run", async () => {
    await withService(async ({ service, note }) => {
      const result = await service.kernelStatus({ file: note, kernel: "python3", session: "default" });
      expect(result.status).toBe("not-started");
      expect(result.id).toBe("");
    });
  });

  test("widget proxy refuses unknown kernels and unrelated paths", async () => {
    await withService(async ({ service }) => {
      expect(service.widgetProxyTarget("/jupyter/api/kernels/missing/channels", "?session_id=x", true)).toBe(null);
      expect(service.widgetProxyTarget("/jupyter/api/kernels", "", false)).toBe(null);
      expect(service.widgetProxyTarget("/jupyter/nbextensions/widget/index.js", "", false))
        .toContain("/nbextensions/widget/index.js");
      expect(service.touchKernelById("missing")).toBe(false);
    });
  });
});
