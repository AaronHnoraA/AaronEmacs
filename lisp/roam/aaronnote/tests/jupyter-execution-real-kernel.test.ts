import { describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import * as zmq from "zeromq";
import { createKernelRegistry } from "../server/jupyter/kernel-registry.mjs";
import { defaultKernelSearchDirs, findKernelSpecs } from "../server/jupyter/kernel-finder.mjs";
import { executeOnKernel } from "../server/jupyter/execution-message-handler.mjs";

// End-to-end parity check for executeOnKernel against a real ipykernel + real
// ipywidgets, proving the ported message handler correctly captures the
// initial synchronous Output-widget output a `with out: print(...)` /
// `interact()` produces — the exact state Phase 4's seedOutputWidgets needs
// to show a widget's initial content before the browser's live connection
// exists. Gated: spawns a real kernel process.
//
// Note: this test relies on ipykernel's own comm_open/stdout-flush ordering
// under real (threaded) kernel execution, so it can be sensitive to heavy CPU
// contention from many *other* concurrently-launched kernels (e.g. running the
// full gated suite at once, which spawns a dozen+ kernel processes across
// files). It is deterministic and passes reliably alone or under moderate
// concurrency; the routing logic itself is proven independent of kernel
// timing by the scripted-message tests in jupyter-execution-message-handler.test.ts.
const RUN = process.env.AARONNOTE_TEST_KERNEL === "1";
const describeIfKernel = RUN ? describe : describe.skip;

const aaronnoteRoot = join(import.meta.dirname, "..");
const venvDir = join(aaronnoteRoot, "jupyter", ".venv");

describeIfKernel("executeOnKernel (real ipykernel + ipywidgets)", () => {
  test(
    "captures print(), execute_result, and an Output-widget's initial synchronous output",
    async () => {
      const runtimeDir = await mkdtemp(join(tmpdir(), "aaronnote-exec-real-"));
      const registry = createKernelRegistry({ runtimeDir, venvBinDir: join(venvDir, "bin"), zmq, launchTimeoutMs: 15_000 });
      const searchDirs = defaultKernelSearchDirs({ venvPrefix: venvDir, useHomeKernels: false });
      const specs = await findKernelSpecs({ searchDirs });
      const python3 = specs.find((s) => s.name === "python3")!;
      const key = "exec-real-test";

      try {
        const record = await registry.ensure(key, python3);

        const basic = await executeOnKernel(record.kernel, "print('hello')\n6 * 7");
        expect(basic.status).toBe("ok");
        expect(basic.outputs.some((o: any) => o.output_type === "stream" && o.text.includes("hello"))).toBe(true);
        expect(basic.outputs.some((o: any) => o.output_type === "execute_result" && o.data["text/plain"] === "42")).toBe(true);

        const widgetCode = [
          "import ipywidgets as widgets",
          "out = widgets.Output()",
          "with out:",
          "    print('initial widget output')",
          "out",
        ].join("\n");
        const widgetResult = await executeOnKernel(record.kernel, widgetCode);
        expect(widgetResult.status).toBe("ok");
        // The Output widget's own display (via bare `out` as the last expression)
        // is a normal execute_result/display_data at the top level...
        const hasWidgetView = widgetResult.outputs.some(
          (o: any) => o.output_type === "execute_result" && o.data["application/vnd.jupyter.widget-view+json"],
        );
        expect(hasWidgetView).toBe(true);
        // ...while the print() *inside* `with out:` must be scoped into
        // widgetOutputs under the Output widget's comm id, not top-level.
        expect(widgetResult.outputs.some((o: any) => o.output_type === "stream")).toBe(false);
        expect(widgetResult.widgetOutputs).toBeDefined();
        const captured = Object.values(widgetResult.widgetOutputs || {}).flat();
        expect(captured.some((o: any) => o.output_type === "stream" && o.text.includes("initial widget output"))).toBe(true);
      } finally {
        await registry.shutdownAll();
        await rm(runtimeDir, { recursive: true, force: true });
      }
    },
    30_000,
  );
});
