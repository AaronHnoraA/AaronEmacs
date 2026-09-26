// Test-only stdio bridge to the real Noema API; no mocks of Contents or kernels.
import readline from 'node:readline';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createJupyterCellService } from '../site-lisp/noema/server/lib/jupyter-cell.mjs';
import { createJupyterApiHandlers } from '../site-lisp/noema/server/Features/Jupyter/api.mjs';
let service, handlers, root;
try {
  for await (const line of readline.createInterface({ input: process.stdin })) {
    let request;
    try {
      request = JSON.parse(line);
      let result;
      if (request.channel === 'init') {
        root = await mkdtemp(join(tmpdir(), 'noema-contents-live-'));
        const server = request.body;
        service = createJupyterCellService({ runtimeRoot: root, noteRoot: root, workspaceRoot: root,
          publish(channel, payload) {
            if (['jupyter-session', 'jupyter-debug-ended'].includes(channel)) {
              process.stdout.write(`@@noema-test@@${JSON.stringify({ event: channel, payload })}\n`);
            }
          },
          serverHost: { async listServers() { return [{ id: server.id, displayName: 'Live Contents', url: server.url }]; },
            async resolveServer(id) { if (id !== server.id) throw new Error('Wrong server'); return server; } },
        });
        handlers = createJupyterApiHandlers(service);
        result = { ok: true };
      } else if (request.channel === 'close') {
        await service?.shutdown(); result = { ok: true };
      } else {
        if (!handlers?.[request.channel]) throw new Error(`Unknown channel: ${request.channel}`);
        result = await handlers[request.channel](request.body);
      }
      process.stdout.write(`@@noema-test@@${JSON.stringify({ id: request.id, result })}\n`);
      if (request.channel === 'close') break;
    } catch (err) {
      process.stdout.write(`@@noema-test@@${JSON.stringify({ id: request?.id, error: String(err.message || err) })}\n`);
    }
  }
} finally {
  await service?.shutdown();
  if (root) await rm(root, { recursive: true, force: true });
}
