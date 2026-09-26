// Companion to jupyter-remote-live-smoke.el. Connection secrets arrive on stdin.
import assert from 'node:assert/strict';
import { createRequire } from 'node:module';
import crypto from 'node:crypto';
import { createRawKernelConnection, waitForConnected, warmupKernelInfo } from '../site-lisp/noema/server/jupyter/raw-kernel.mjs';
import { executeOnKernel } from '../site-lisp/noema/server/jupyter/execution-message-handler.mjs';
import { createServerRegistry } from '../site-lisp/noema/server/jupyter/server-registry.mjs';
import { createKernelRegistry } from '../site-lisp/noema/server/jupyter/kernel-registry.mjs';
const require = createRequire(new URL('../site-lisp/noema/package.json', import.meta.url));
const zmq = require('zeromq');
let input = '';
for await (const chunk of process.stdin) input += chunk;
const request = JSON.parse(input);
const deadline = setTimeout(() => { console.error('Live Jupyter probe timed out'); process.exit(2); }, 60000);
async function execute(kernel, code, expected) {
  const result = await executeOnKernel(kernel, code, { execTimeoutMs: 25000 });
  const text = result.outputs.filter(o => o.output_type === 'stream').map(o => o.text).join('');
  assert(!result.outputs.some(o => o.output_type === 'error'), JSON.stringify(result.outputs));
  assert(text.includes(expected), `Expected ${expected}; received ${text}`);
  return result;
}
try {
  if (request.mode === 'raw') {
    const { kernel, socket } = createRawKernelConnection({ connectionInfo: request.connectionInfo,
      clientId: crypto.randomUUID(), username: 'noema-audit', model: { id: crypto.randomUUID(), name: 'python3' }, zmq });
    try {
      assert(await waitForConnected(kernel, 15000));
      await warmupKernelInfo(kernel, 15000);
      await execute(kernel, request.code, request.expected);
      const completion = await kernel.requestComplete({ code: 'noema_audit_', cursor_pos: 12 });
      assert(completion.content.matches.includes('noema_audit_state'));
    } finally { kernel.dispose(); socket.dispose(); }
  } else if (request.mode === 'server') {
    const servers = createServerRegistry({ resolveServer: async () => request.server });
    const registry = createKernelRegistry({ runtimeDir: '/tmp', zmq, serverRegistry: servers });
    let owned;
    try {
      const specs = await servers.listKernelSpecs('audit');
      assert(specs.some(s => s.name === 'python3'));
      const contents = await servers.contents('audit');
      await contents.save('audit.txt', { type: 'file', format: 'text', content: 'remote contents roundtrip' });
      assert.equal((await contents.get('audit.txt', { content: true, format: 'text' })).content, 'remote contents roundtrip');
      await contents.delete('audit.txt');
      owned = await registry.ensureServer('owned', 'python3', { serverId: 'audit', kernelSpecName: 'python3', path: 'audit.ipynb' });
      await execute(owned.kernel, 'noema_audit_state = 41; print(noema_audit_state + 1)', '42');
      const adopted = await registry.ensureServer('adopted', 'python3', { serverId: 'audit', kernelId: owned.serverKernelId });
      await execute(adopted.kernel, 'print(noema_audit_state + 1)', '42');
      await registry.shutdown('adopted');
      assert((await servers.listRunning('audit')).some(k => k.id === owned.serverKernelId), 'Detaching adopted kernel killed owner');
      await execute(owned.kernel, 'print(noema_audit_state + 1)', '42');
      const restarted = await registry.restart('owned');
      await execute(restarted.kernel, 'print("noema_audit_state" not in globals())', 'True');
      await registry.shutdown('owned');
      assert(!(await servers.listRunning('audit')).some(k => k.id === owned.serverKernelId));
    } finally { await registry.shutdownAll(); await servers.forgetAll(); }
  } else throw new Error('Unknown probe mode');
  console.log(`PASS ${request.label || request.mode}`);
} finally { clearTimeout(deadline); }
