import { basename, relative, sep } from "node:path";

const frameAncestors = "frame-ancestors 'self' file: http://127.0.0.1:* http://localhost:* aaronnote-asset:";

function slashPath(path) {
  return String(path || "").split(sep).join("/");
}

function encodePathSegments(path) {
  return slashPath(path)
    .split("/")
    .filter(Boolean)
    .map((part) => encodeURIComponent(part))
    .join("/");
}

export function jupyterSelectorPath(selector = "") {
  const segments = String(selector || "")
    .trim()
    .replace(/^#/, "")
    .split("@")
    .map((part) => part.trim())
    .filter(Boolean);
  return segments[0] || "";
}

function envAssignment(entry) {
  const index = String(entry || "").indexOf("=");
  if (index <= 0) return null;
  const key = entry.slice(0, index);
  if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(key)) return null;
  return [key, entry.slice(index + 1)];
}

export function parseNulEnv(stdout = "") {
  const text = Buffer.isBuffer(stdout) ? stdout.toString("utf8") : String(stdout || "");
  const env = {};
  for (const rawEntry of text.split("\0")) {
    const direct = envAssignment(rawEntry);
    const assignment = direct || rawEntry.split(/\r?\n/).map(envAssignment).filter(Boolean).pop();
    if (!assignment) continue;
    env[assignment[0]] = assignment[1];
  }
  return env;
}

export function mergeJupyterEnv(base = {}, shell = {}) {
  const merged = { ...base, ...shell };
  for (const [key, value] of Object.entries(base || {})) {
    if (key.startsWith("AARONNOTE_")) merged[key] = value;
  }
  return merged;
}

function jupyterUsesSubcommand(command) {
  return /^jupyter(?:\.exe)?$/i.test(basename(String(command || "")));
}

export function jupyterLaunchArgs({ command = "", root = "", port = 0, token = "" } = {}) {
  void token; // Auth is disabled below; the token is no longer used to start the server.
  const args = jupyterUsesSubcommand(command) ? ["lab"] : [];
  return [
    ...args,
    "--no-browser",
    "--ServerApp.ip=127.0.0.1",
    `--ServerApp.port=${Math.max(1, Number(port) || 0)}`,
    // Disable all authentication. The notebook is embedded in a cross-origin
    // Electron iframe (the app loads from file:// / the Vite origin, the server
    // from http://127.0.0.1), so jupyter_server's auth cookie never flows to the
    // kernel WebSocket and the kernel hangs forever at "Connecting". With an empty
    // token + password jupyter_server serves an anonymous identity and the WS
    // handshake needs no cookie. The server is bound to 127.0.0.1 only.
    "--ServerApp.token=",
    "--IdentityProvider.token=",
    "--ServerApp.password=",
    "--ServerApp.allow_origin=*",
    "--ServerApp.disable_check_xsrf=True",
    `--ServerApp.root_dir=${String(root || "")}`,
    `--ServerApp.tornado_settings=${JSON.stringify({
      headers: {
        "Content-Security-Policy": frameAncestors,
      },
    })}`,
  ];
}

export function jupyterLabUrl({ baseUrl = "", root = "", file = "", token = "", selector = "", selectorKind = "" } = {}) {
  const rel = slashPath(relative(root, file));
  const url = new URL(`/lab/tree/${encodePathSegments(rel)}`, baseUrl);
  if (token) url.searchParams.set("token", token);
  const cleanSelector = jupyterSelectorPath(selector);
  void selectorKind;
  if (cleanSelector) url.hash = encodeURIComponent(cleanSelector);
  return url.toString();
}
