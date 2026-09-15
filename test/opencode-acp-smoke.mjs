// Real native ACP -> local deterministic provider. No credentials/model calls.
// Usage: node test/opencode-acp-smoke.mjs /absolute/path/to/opencode
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { mkdtemp, mkdir, realpath, writeFile } from "node:fs/promises";
import { createServer } from "node:http";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { createInterface } from "node:readline";

const binary = resolve(process.argv[2] || "/opt/homebrew/bin/opencode");
const root = await realpath(await mkdtemp(join(tmpdir(), "opencode-acp-smoke-")));
const cwd = join(root, "project");
await mkdir(cwd);
const fixture = join(cwd, "fixture.txt");
await writeFile(fixture, "LOCAL_FIXTURE_OK\n");
let requests = 0;
const mock = createServer(async (req, res) => {
  try {
    assert.equal(req.url, "/v1/chat/completions");
    let body = "";
    for await (const chunk of req) body += chunk;
    const messages = JSON.parse(body).messages;
    await writeFile(join(root, `provider-${requests + 1}.json`), body);
    const results = messages.filter((message) => message.role === "tool");
    const turn = ++requests;
    assert.ok(turn <= 3, "Unexpected model loop");
    const delta = turn === 1
      ? { role: "assistant", tool_calls: [{ index: 0, id: "call_fixture", type: "function",
          function: { name: "read", arguments: JSON.stringify({ filePath: fixture }) } }] }
      : { role: "assistant", content: turn === 2 ? "FIRST_TURN_OK" : "SECOND_TURN_OK" };
    if (turn > 1) assert.match(JSON.stringify(results), /LOCAL_FIXTURE_OK/);
    const chunk = { id: `chatcmpl_${turn}`, object: "chat.completion.chunk", created: 1, model: "mock",
      choices: [{ index: 0, delta, finish_reason: null }] };
    const end = { ...chunk, choices: [{ index: 0, delta: {}, finish_reason: turn === 1 ? "tool_calls" : "stop" }],
      usage: { prompt_tokens: 10, completion_tokens: 5, total_tokens: 15 } };
    res.writeHead(200, { "Content-Type": "text/event-stream" });
    res.end([chunk, end].map((event) => `data: ${JSON.stringify(event)}\n\n`).join("") + "data: [DONE]\n\n");
  } catch (error) {
    console.error(`Local provider assertion: ${error.message}`);
    res.writeHead(400);
    res.end(String(error));
  }
});
mock.listen(0, "127.0.0.1");
await once(mock, "listening");
const config = {
  autoupdate: false, plugin: [], mcp: {}, lsp: false,
  model: "mock/mock", small_model: "mock/mock", enabled_providers: ["mock"],
  permission: { "*": "deny", read: "allow", external_directory: { [`${cwd}/*`]: "allow" } },
  agent: { title: { disable: true }, summary: { disable: true } },
  provider: { mock: { npm: "@ai-sdk/openai-compatible", name: "Local smoke test",
    options: { baseURL: `http://127.0.0.1:${mock.address().port}/v1`, apiKey: "local-test" },
    models: { mock: { name: "mock", limit: { context: 32000, output: 2000 } } } } },
};
const env = {
  PATH: process.env.PATH, SHELL: "/bin/sh", TMPDIR: tmpdir(),
  OPENCODE_CONFIG_CONTENT: JSON.stringify(config), OPENCODE_DISABLE_PROJECT_CONFIG: "1",
  OPENCODE_DISABLE_DEFAULT_PLUGINS: "1", OPENCODE_DISABLE_EXTERNAL_SKILLS: "1",
  OPENCODE_DISABLE_MODELS_FETCH: "1", OPENCODE_DISABLE_AUTOUPDATE: "1",
};
for (const key of ["DATA", "CONFIG", "CACHE", "STATE"]) env[`XDG_${key}_HOME`] = join(root, key.toLowerCase());
const child = spawn(binary, ["acp", "--cwd", cwd], { cwd, env, stdio: ["pipe", "pipe", "pipe"] });
const exited = once(child, "exit");
let seq = 0;
let stderr = "";
const pending = new Map();
const updates = [];
const lines = createInterface({ input: child.stdout });
child.stderr.on("data", (data) => { stderr = (stderr + data).slice(-16_000); });
lines.on("line", (line) => {
  let msg;
  try { msg = JSON.parse(line); } catch { return; }
  if (msg.method === "session/update") updates.push(msg.params.update);
  if (msg.method === "session/request_permission") {
    const option = msg.params.options?.find((item) => item.kind === "allow_once");
    child.stdin.write(JSON.stringify({jsonrpc: "2.0", id: msg.id, result: {outcome: option
      ? {outcome: "selected", optionId: option.optionId} : {outcome: "cancelled"}}}) + "\n");
  }
  const task = pending.get(msg.id);
  if (!task) return;
  pending.delete(msg.id);
  msg.error ? task.reject(new Error(JSON.stringify(msg.error))) : task.resolve(msg.result);
});
child.on("exit", (code) => {
  for (const task of pending.values()) task.reject(new Error(`ACP exited: ${code}`));
  pending.clear();
});
const request = (method, params) => new Promise((resolve, reject) => {
  const id = ++seq;
  pending.set(id, { resolve, reject });
  child.stdin.write(JSON.stringify({ jsonrpc: "2.0", id, method, params }) + "\n");
});
const timeout = setTimeout(() => child.kill("SIGTERM"), 40_000);
try {
  await request("initialize", { protocolVersion: 1,
    clientInfo: { name: "local-acp-regression", version: "1" },
    clientCapabilities: { fs: { readTextFile: false, writeTextFile: false }, terminal: false } });
  const session = await request("session/new", { cwd, mcpServers: [] });
  assert.ok(session.sessionId);
  for (const [index, prompt] of ["Read fixture.txt and report the result.", "Confirm you retained the previous tool result."].entries()) {
    if (index === 1) await request("session/load", { sessionId: session.sessionId, cwd, mcpServers: [] });
    const result = await request("session/prompt", { sessionId: session.sessionId, prompt: [{ type: "text", text: prompt }] });
    assert.equal(result.stopReason, "end_turn");
  }
  assert.equal(requests, 3);
  assert.match(JSON.stringify(updates), /FIRST_TURN_OK/);
  assert.match(JSON.stringify(updates), /SECOND_TURN_OK/);
  console.log(JSON.stringify({ passed: true, binary, sessionReloaded: true, localProviderRequests: requests, paidModelRequests: 0, artifacts: root }));
} catch (error) {
  console.error(JSON.stringify({ passed: false, binary, localProviderRequests: requests, error: String(error), artifacts: root }));
  await writeFile(join(root, "stderr.log"), stderr);
  await writeFile(join(root, "updates.json"), JSON.stringify(updates));
  process.exitCode = 1;
} finally {
  clearTimeout(timeout);
  lines.close();
  child.kill("SIGTERM");
  const force = setTimeout(() => child.kill("SIGKILL"), 2000);
  await exited;
  clearTimeout(force);
  mock.closeAllConnections();
  await new Promise((done) => mock.close(done));
}
