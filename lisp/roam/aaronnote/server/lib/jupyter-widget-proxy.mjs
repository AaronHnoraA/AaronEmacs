import { WebSocket, WebSocketServer } from "ws";

export function jupyterProxyKernelId(pathname) {
  const match = /^\/jupyter\/(?:api\/kernels|widget-runtimes)\/([^/]+)\/channels$/.exec(String(pathname || ""));
  if (!match) return "";
  try { return decodeURIComponent(match[1]); } catch { return ""; }
}

function offeredProtocols(req) {
  return String(req.headers["sec-websocket-protocol"] || "")
    .split(",")
    .map((value) => value.trim())
    .filter(Boolean);
}

function closeSocket(socket, code = 1011, reason = "Jupyter widget proxy failed") {
  try {
    if (socket.readyState === WebSocket.OPEN || socket.readyState === WebSocket.CONNECTING) {
      socket.close(code, reason.slice(0, 120));
    }
  } catch {}
}

function forwardedCloseCode(code) {
  const value = Number(code || 0);
  return value >= 1000 && value <= 4999 && ![1004, 1005, 1006, 1015].includes(value) ? value : 1000;
}

export function installJupyterWidgetProxy({ server, resolveTarget, touchKernel, stderr = process.stderr }) {
  const webSocketServer = new WebSocketServer({ noServer: true });

  server.on("upgrade", (req, socket, head) => {
    let url;
    try { url = new URL(req.url || "/", "http://localhost"); }
    catch { socket.destroy(); return; }
    const target = resolveTarget(url.pathname, url.search, true);
    if (!target) {
      socket.destroy();
      return;
    }
    const kernelId = jupyterProxyKernelId(url.pathname);
    webSocketServer.handleUpgrade(req, socket, head, (downstream) => {
      const protocols = offeredProtocols(req);
      const options = { headers: { Origin: new URL(target).origin } };
      const upstream = protocols.length > 0
        ? new WebSocket(target, protocols, options)
        : new WebSocket(target, options);
      const pending = [];
      let pendingBytes = 0;
      const maxPendingBytes = 16 * 1024 * 1024;

      const fail = (detail) => {
        if (detail) stderr.write(`[aaronnote-jupyter] widget proxy: ${detail}\n`);
        closeSocket(downstream);
        closeSocket(upstream);
      };

      downstream.on("message", (data, isBinary) => {
        if (kernelId) touchKernel(kernelId);
        if (upstream.readyState === WebSocket.OPEN) {
          upstream.send(data, { binary: isBinary });
          return;
        }
        const size = Number(data?.byteLength ?? data?.length ?? 0);
        pendingBytes += size;
        if (pendingBytes > maxPendingBytes) {
          fail("pending widget messages exceeded 16 MiB");
          return;
        }
        pending.push({ data, isBinary });
      });
      upstream.on("open", () => {
        for (const item of pending.splice(0)) upstream.send(item.data, { binary: item.isBinary });
        pendingBytes = 0;
      });
      upstream.on("message", (data, isBinary) => {
        if (kernelId) touchKernel(kernelId);
        if (downstream.readyState === WebSocket.OPEN) downstream.send(data, { binary: isBinary });
      });
      downstream.on("close", (code, reason) => {
        if (upstream.readyState === WebSocket.OPEN) upstream.close(forwardedCloseCode(code), reason.toString());
        else closeSocket(upstream, 1000, "Client closed");
      });
      upstream.on("close", (code, reason) => {
        if (downstream.readyState === WebSocket.OPEN) downstream.close(forwardedCloseCode(code), reason.toString());
      });
      downstream.on("error", (err) => fail(err?.message || "downstream websocket error"));
      upstream.on("error", (err) => fail(err?.message || "upstream websocket error"));
    });
  });

  return {
    async proxyHttp(req, res, url) {
      if (req.method !== "GET" && req.method !== "HEAD") return false;
      const target = resolveTarget(url.pathname, url.search, false);
      if (!target) return false;
      const response = await fetch(target, {
        method: req.method,
        headers: { Accept: String(req.headers.accept || "*/*") },
        signal: AbortSignal.timeout(15_000),
      });
      const headers = {
        "Content-Type": response.headers.get("content-type") || "application/javascript; charset=utf-8",
        "Cache-Control": response.ok ? "public, max-age=3600" : "no-store",
      };
      res.writeHead(response.status, headers);
      if (req.method === "HEAD") res.end();
      else res.end(Buffer.from(await response.arrayBuffer()));
      return true;
    },
    close() {
      for (const client of webSocketServer.clients) {
        try { client.terminate(); } catch {}
      }
      try { webSocketServer.close(); } catch {}
    },
  };
}
