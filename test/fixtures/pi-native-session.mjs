// A native SDK history fixture. No model or network request is made.
import { existsSync, readFileSync, realpathSync } from "node:fs";
import { createRequire } from "node:module";
import { join, resolve } from "node:path";
import { pathToFileURL } from "node:url";

const [adapter, cwd, agentDirectory] = process.argv.slice(2);
if (!adapter || !cwd || !agentDirectory) throw new Error("Expected pi-acp, cwd and isolated agent directory");
process.env.PI_CODING_AGENT_DIR = agentDirectory;
const require = createRequire(realpathSync(adapter));
const packageName = "@earendil-works/pi-coding-agent";
const directory = require.resolve.paths(packageName).map((root) => join(root, packageName))
  .find((root) => existsSync(join(root, "package.json")));
if (!directory) throw new Error("The installed pi-acp SDK is unavailable");
const metadata = JSON.parse(readFileSync(join(directory, "package.json"), "utf8"));
const { SessionManager } = await import(pathToFileURL(resolve(directory, metadata.exports["."].import)).href);
const manager = SessionManager.create(cwd);
manager.appendMessage({ role: "user", content: "Native session fixture", timestamp: Date.now() });
manager.appendMessage({ role: "assistant", content: [{ type: "text", text: "Fixture response, not a model result." }],
  api: "openai-codex-responses", provider: "openai-codex", model: "gpt-5.6-luna", stopReason: "stop",
  usage: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, totalTokens: 0,
    cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 } }, timestamp: Date.now() });
process.stdout.write(manager.getSessionId());
